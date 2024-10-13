(in-package #:com.bhester.reader)

;; helpers

(utils:my-defconstant +digits+ (str->seq "0123456789"))
(utils:my-defconstant +signs+ (str->seq "+-"))
(utils:my-defconstant +whitespace+
                      (mapcar
                       #'char-code
                       (list #\space ;; TODO: fill out rest of WS characters
                             #\tab
                             #\linefeed
                             #\return)))
(utils:my-defconstant +lf-code+ (char-code #\linefeed))
(utils:my-defconstant +cr-code+ (char-code #\return))

(defun digit-p (d)
  (member d +digits+))
(defun digit-value (d)
  (- d (char-code #\0)))

(defun sign-p (d)
  (member d +signs+))

(defun whitespace-p (c)
  (member c +whitespace+))

(defun set-pos (ind file)
  (file-position file ind))

(defun char-at (ind file)
  (file-position file ind)
  (read-byte file))

(defun valid-seq-p (seq file &optional (start 0))
  (if (null seq)
    (values t start)
    (if (eq (car seq)
            (char-at start file))
      (valid-seq-p (rest seq) file (1+ start))
      (values nil nil))))

(defun object-builder (init add-char success fail)
  (lambda (start-ind file)
    (set-pos start-ind file)
    (let ((init-context (funcall init)))
      (labels ((next-step (context)
                 (let ((next-char (read-byte file)))
                   (funcall add-char
                            next-char
                            context
                            success
                            fail
                            (lambda (context1) (next-step context1))))))
        (next-step init-context)))))

(defun init-number-context (force-integer)
  (let ((num 0)
        (found-digit nil)
        (found-decimal nil)
        (pow 0)
        (sign nil))
    (labels ((add-char! (c success fail cont)
               (declare (ignore fail))
               (cond
                 ((digit-p c)
                  (setf num (+ (* 10 num) (digit-value c)))
                  (setf found-digit t)
                  (when found-decimal
                    (decf pow))
                  (funcall cont #'this-context))
                 ((and (eq (char-code #\.) c)
                       (not (or force-integer found-decimal)))
                  (setf found-decimal t)
                  (funcall cont #'this-context))
                 ((and (sign-p c)
                       (not found-digit))
                  (setf sign c)
                  (funcall cont #'this-context))
                 (t
                  (funcall success #'this-context))))
             (this-context (message &rest args)
               (ecase message
                 ((add-char!) (apply #'add-char! args))
                 ((cur-val) num)
                 ((found-digit-p) found-digit)
                 ((sign-pow-corrected)
                  (let ((sign-corrected
                         (if (eq (char-code #\-) sign)
                           (- num)
                           num)))
                    (if found-decimal
                      (* sign-corrected (expt 10 pow))
                      sign-corrected))))))
      #'this-context)))

(defun number-builder (force-integer force-real success fail)
  (object-builder
   ;; init
   (lambda () (init-number-context force-integer))
   ;; add-char
   (lambda (next-char context success fail cont)
     (funcall context 'add-char! next-char success fail cont))
   ;; success
   (lambda (context)
     (if (funcall context 'found-digit-p)
       (let ((sign-pow-corrected (funcall context 'sign-pow-corrected)))
         (funcall success
                  (if force-real
                    (float sign-pow-corrected)
                    sign-pow-corrected)))
       (funcall fail "No digit found in number")))
   fail))

(defun read-number (start-ind
                    file
                    &key
                    (force-integer nil)
                    (force-real nil)
                    (skip-whitespace nil)
                    (err-p t))
  (let ((build-number (number-builder
                       force-integer
                       force-real
                       (lambda (num)
                         (values num (1- (file-position file))))
                       (lambda (reason)
                         (if err-p
                           (error reason)
                           (values 'error nil))))))
    (funcall build-number
             (if skip-whitespace
               (skip-whitespace start-ind file)
               start-ind)
             file)))

(defun read-pdf-number (start-ind file)
  (let ((build-number (number-builder
                       nil
                       nil
                       (lambda (num)
                         (values (objs::make-object 'number num)
                                 (1- (file-position file))))
                       #'error)))
    (funcall build-number
             (skip-whitespace start-ind file)
             file)))

(defun skip-whitespace (cur-pos file)
  (let ((c (char-at cur-pos file)))
    (if (whitespace-p c)
      (skip-whitespace (1+ cur-pos) file)
      cur-pos)))

(defun line-start (from-ind file)
  (labels ((check-crlf ()
             (let ((c (char-at (- from-ind 2) file)))
               (values from-ind
                       (if (eq c +cr-code+)
                         'crlf
                         'lf)))))
    (let ((c (char-at (1- from-ind) file)))
      (utils:condcase c
        (+lf-code+ (check-crlf))
        (+cr-code+ (values from-ind 'cr))
        (t (line-start (1- from-ind) file))))))

(defun read-to (target file from-ind &optional (end-ind nil end-ind-p))
  (let ((test-end (if end-ind-p end-ind (file-length file))))
    (if (= from-ind test-end)
      (values from-ind nil)
      (let ((cur (char-at from-ind file)))
        (if (eq cur target)
          (values from-ind t)
          (read-to target file (1+ from-ind) test-end))))))

(defun back-by-ending (line-start line-ending)
  (- line-start
     (if (eq line-ending 'crlf) 2 1)))

(defun prev-line-start (line-start line-ending file)
  (line-start (back-by-ending line-start line-ending)
              file))

;; reader

(defun read-header (file)
  (multiple-value-bind (valid version-start)
      (valid-seq-p (str->seq "%PDF-") file 0)
    (unless valid
      (error "Invalid pdf header"))
    (read-number version-start file :force-real t)))

(defun find-xref (file)
  (utils:letmv* ((len (file-length file))
                 (eof-marker (last-eof-marker len file))
                 ((xref-off xref-ending)
                  (prev-line-start (car eof-marker) (cdr eof-marker) file))
                 (xref-entries
                  (utils:-> (prev-line-start xref-off xref-ending file)
                            (skip-whitespace file)
                            (get-xref-byte-off file)
                            (read-xref-entries file))))
    'then-maybe-the-trailer-after-that?
    xref-entries))

(defun last-eof-marker (last-stop file)
  (utils:letmv* (((line-start line-ending) (line-start last-stop file))
                 ((ind found-p) (read-to (char-code #\%)
                                         file
                                         line-start
                                         last-stop)))
    (if (and found-p
             (valid-seq-p (str->seq "%%EOF") file ind))
      (cons line-start line-ending)
      (last-eof-marker (back-by-ending line-start line-ending)
                       file))))

(defun get-xref-byte-off (startxref-start file)
  (if (valid-seq-p (str->seq "startxref")
                   file
                   startxref-start)
    (let ((byte-off (read-number (+ 9 startxref-start)
                                 file
                                 :force-integer t
                                 :skip-whitespace t)))
      (unless (valid-seq-p (str->seq "xref") file byte-off)
        (error "Invalid xref byte offset. Could not find \"xref\""))
      byte-off)
    (error "Invalid trailer. Could not find \"startxref\"")))

(defun read-xref-entries (xref-byte-off file)
  (let ((subsection-start (skip-whitespace (+ 4 xref-byte-off)
                                           file)))
    (apply #'append (try-xref-subsections subsection-start file))))

(defun try-xref-subsections (subsection-start file)
  (multiple-value-bind (maybe-start-obj-num ind)
      (read-number subsection-start file :force-integer t :err-p nil)
    (if (eq maybe-start-obj-num 'error)
      '()
      (multiple-value-bind (entry-count after-ec)
          (read-number ind file :force-integer t :skip-whitespace t)
        (let* ((entry-start (skip-whitespace after-ec file))
               (next-start (+ entry-start
                              (* 20 entry-count))))
          (cons (xref-subsection-entries entry-start
                                         maybe-start-obj-num
                                         entry-count
                                         file)
                (try-xref-subsections next-start file)))))))

(defun xref-subsection-entries (entry-start object-num entry-count file)
  (if (zerop entry-count)
    '()
    (cons (xref-subsection-entry object-num entry-start file)
          (xref-subsection-entries (+ 20 entry-start)
                                   (1+ object-num)
                                   (1- entry-count)
                                   file))))

(defun xref-subsection-entry (object-num entry-start file)
  (let ((n1 (read-number entry-start file :force-integer t))
        (n2 (read-number (+ 11 entry-start) file :force-integer t))
        (c (code-char (char-at (+ 17 entry-start) file))))
    (utils:condcase c
      (#\n (make-n-xref-entry object-num n2 n1))
      (#\f (make-f-xref-entry object-num n2))
      (t (error "Invalid xref entry type")))))

(defun make-n-xref-entry (object-num generation-num offset)
  (utils:tag-value 'n-xref-entry (list object-num generation-num offset)))

(defun make-f-xref-entry (object-num generation-num)
  (utils:tag-value 'f-xref-entry (list object-num generation-num)))
