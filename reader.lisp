(in-package #:com.bhester.reader)

;; helpers

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

(utils:my-defconstant +digits+ (str->seq "0123456789"))
(defun digit-p (d)
  (member d +digits+))
(defun digit-value (d)
  (- d (char-code #\0)))

(utils:my-defconstant +signs+ (str->seq "+-"))
(defun sign-p (d)
  (member d +signs+))

(utils:my-defconstant +whitespace+
                      (mapcar
                       #'char-code
                       (list #\space ;; TODO: fill out rest of WS characters
                             #\tab
                             #\linefeed
                             #\return)))
(defun whitespace-p (c)
  (member c +whitespace+))

(defun read-number (file
                    start-ind
                    &key
                    (force-integer nil)
                    (force-real nil)
                    (skip-whitespace nil)
                    (err-p t))
  (when (and force-integer force-real)
    (error "Cannot provide both :force-integer and :force-real"))
  (%read-number file
                (if skip-whitespace
                  (skip-whitespace start-ind file)
                  start-ind)
                force-integer
                force-real
                err-p))

(defun %read-number (file start-ind force-integer force-real err-p)
  (funcall (add-char file force-integer force-real)
           start-ind
           0
           nil
           nil
           nil
           0
           (lambda (&rest args)
             (if err-p
               (apply #'error args)
               (values 'error nil)))
           (lambda (num ind)
             (values num ind))))

(defun add-char (file force-integer force-real)
  (lambda (ind num sign found-decimal found-digit pow fail success)
    (labels ((inner (ind num sign found-decimal found-digit pow)
               (let ((c (char-at ind file)))
                 (cond
                   ((digit-p c)
                    (inner (1+ ind)
                           (+ (* 10 num)
                              (digit-value c))
                           sign
                           found-decimal
                           t
                           (if found-decimal
                             (1- pow)
                             pow)))
                   ((eq (char-code #\.) c)
                    (if found-decimal
                      (funcall fail "Multiple decimal points found in number")
                      (inner (1+ ind)
                             num
                             sign
                             t
                             found-digit
                             pow)))
                   ((sign-p c)
                    (if found-digit
                      (funcall fail "Sign in the middle of a number")
                      (inner (1+ ind)
                             num
                             c
                             found-decimal
                             found-digit
                             pow)))
                   (t
                    (funcall (check-num-type force-integer force-real)
                             ind
                             num
                             sign
                             found-decimal
                             found-digit
                             pow
                             fail
                             success))))))
      (inner ind num sign found-decimal found-digit pow))))

(defun check-num-type (force-integer force-real)
  (lambda (ind num sign found-decimal found-digit pow fail success)
    (cond
      ((not found-digit)
       (funcall fail "No digit found in number"))
      ((and force-integer found-decimal)
       (funcall fail "Found decimal in integer"))
      ((and force-real (not found-decimal))
       (funcall fail "No decimal in real number"))
      (t
       (funcall (correct-pow force-real)
                ind
                num
                sign
                found-decimal
                pow
                success)))))

(defun correct-pow (force-real)
  (lambda (ind num sign found-decimal pow success)
    (correct-sign ind
                  (if force-real
                    (float (if found-decimal
                             (* num (expt 10 pow))
                             num))
                    num)
                  sign
                  success)))

(defun correct-sign (ind num sign success)
  (funcall success
           (if (eq (char-code #\-) sign)
             (- num)
             num)
           ind))

(defun skip-whitespace (cur-pos file)
  (let ((c (char-at cur-pos file)))
    (if (whitespace-p c)
      (skip-whitespace (1+ cur-pos) file)
      cur-pos)))

(utils:my-defconstant +lf-code+ (char-code #\linefeed))
(utils:my-defconstant +cr-code+ (char-code #\return))

(defun line-start (file from-ind)
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
        (t (line-start file (1- from-ind)))))))

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

(defun prev-line-start (file line-start line-ending)
  (line-start file
              (back-by-ending line-start line-ending)))

;; reader

(defun read-header (file)
  (multiple-value-bind (valid version-start)
      (valid-seq-p (str->seq "%PDF-") file 0)
    (unless valid
      (error "Invalid pdf header"))
    (read-number file version-start :force-real t)))

(defun find-xref (file)
  (let ((len (file-length file)))
    (let ((eof-marker (last-eof-marker len file)))
      (multiple-value-bind (xref-off xref-ending)
          (prev-line-start file (car eof-marker) (cdr eof-marker))
        (let* ((line-before (prev-line-start file xref-off xref-ending))
               (startxref-start (skip-whitespace line-before file))
               (xref-byte-off (get-xref-byte-off startxref-start file))
               (xref-entries (read-xref-entries xref-byte-off file)))
          (format t "The byte offset is... ~A~%" xref-byte-off)
          'find-the-xref-section
          'then-maybe-the-trailer-after-that?
          xref-entries)))))

(defun last-eof-marker (last-stop file)
  (multiple-value-bind (line-start line-ending)
      (line-start file last-stop)
    (multiple-value-bind (ind found-p)
        (read-to (char-code #\%) file line-start last-stop)
      (if (and found-p
               (valid-seq-p (str->seq "%%EOF") file ind))
        (cons line-start line-ending)
        (last-eof-marker (back-by-ending line-start line-ending)
                         file)))))

(defun get-xref-byte-off (startxref-start file)
  (if (valid-seq-p (str->seq "startxref")
                   file
                   startxref-start)
    (let ((byte-off (read-number file
                                 (+ 9 startxref-start)
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
      (read-number file subsection-start :force-integer t :err-p nil)
    (format t "~A~%" maybe-start-obj-num)
    (if (eq maybe-start-obj-num 'error)
      '()
      (multiple-value-bind (entry-count after-ec)
          (read-number file ind :force-integer t :skip-whitespace t)
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
  (let ((n1 (read-number file entry-start :force-integer t))
        (n2 (read-number file (+ 11 entry-start) :force-integer t))
        (c (code-char (char-at (+ 17 entry-start) file))))
    (utils:condcase c
      (#\n (make-n-xref-entry object-num n2 n1))
      (#\f (make-f-xref-entry object-num n2))
      (t (error "Invalid xref entry type")))))

(defun make-n-xref-entry (object-num generation-num offset)
  (utils:tag-value 'n-xref-entry (list object-num generation-num offset)))

(defun make-f-xref-entry (object-num generation-num)
  (utils:tag-value 'f-xref-entry (list object-num generation-num)))
