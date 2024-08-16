(in-package #:com.bhester.reader)

;; helpers

(defun char-at (ind file)
  (file-position file ind)
  (read-byte file))

(defun validate-seq (seq file &optional (start 0))
  (if (null seq)
    (values t start)
    (if (eq (car seq)
            (char-at start file))
      (validate-seq (rest seq) file (1+ start))
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

(defun read-number (file start-ind &key (force-integer nil)
                                        (force-real nil))
  (when (and force-integer force-real)
    (error "Cannot provide both :force-integer and :force-real"))
  (labels ((correct-sign (sign num)
             (if (eq (char-code #\-) sign)
               (- num)
               num))
           (correct-pow (found-decimal pow num)
             (if force-real
               (float (if found-decimal
                        (* num (expt 10 pow))
                        num))
               num))
           (final-check (num sign found-decimal found-digit pow)
             (cond
               ((not found-digit)
                (error "No digit found in number"))
               ((and force-integer found-decimal)
                (error "Found decimal in integer"))
               ((and force-real (not found-decimal))
                (error "No decimal in real number")))
             (correct-sign sign
                           (correct-pow found-decimal pow num)))
           (add-char (ind cur-num sign found-decimal found-digit pow)
             (let ((c (char-at ind file)))
               (cond
                 ((digit-p c)
                  (add-char (1+ ind)
                            (+ (* 10 cur-num)
                               (digit-value c))
                            sign
                            found-decimal
                            t
                            (if found-decimal
                              (1- pow)
                              pow)))
                 ((eq (char-code #\.) c)
                  (if found-decimal
                    (error "Multiple decimal points found in number")
                    (add-char (1+ ind)
                              cur-num
                              sign
                              t
                              found-digit
                              pow)))
                 ((sign-p c)
                  (if found-digit
                    (error "Sign in the middle of a number")
                    (add-char (1+ ind)
                              cur-num
                              c
                              found-decimal
                              found-digit
                              pow)))
                 (t (values (final-check cur-num
                                         sign
                                         found-decimal
                                         found-digit
                                         pow)
                            ind))))))
    (add-char start-ind 0 nil nil nil 0)))

(defun skip-whitespace (cur-pos file)
  (let ((c (char-at cur-pos file)))
    (if (whitespace-p c)
      (skip-whitespace (1+ cur-pos) file)
      cur-pos)))

(utils:my-defconstant +lf-code+ (char-code #\linefeed))
(utils:my-defconstant +cr-code+ (char-code #\return))

(defun find-line-start (file from-ind)
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
        (t (find-line-start file (1- from-ind)))))))

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

(defun find-prev-line-start (file line-start line-ending)
  (find-line-start file
                   (back-by-ending line-start line-ending)))

;; reader

(defun read-header (file)
  (multiple-value-bind (valid version-start)
      (validate-seq (str->seq "%PDF-") file 0)
    (unless valid
      (error "Invalid pdf header"))
    (read-number file version-start :force-real t)))

(defun find-xref (file)
  (let ((len (file-length file)))
    (labels ((find-eof-marker (last-stop)
               (multiple-value-bind (line-start line-ending)
                   (find-line-start file last-stop)
                 (multiple-value-bind (ind found-p)
                     (read-to (char-code #\%) file line-start last-stop)
                   (if (and found-p
                            (validate-seq (str->seq "%%EOF") file ind))
                     (cons line-start line-ending)
                     (find-eof-marker (back-by-ending line-start
                                                      line-ending))))))
             (get-xref-byte-off (startxref-start)
               (if (validate-seq (str->seq "startxref") file startxref-start)
                 (let ((num-start (skip-whitespace (+ 9 ; length of startxref
                                                      startxref-start)
                                                   file)))
                   (read-number file
                                num-start
                                :force-integer t))
                 (error "Invalid trailer. Could not find \"startxref\""))))
      (let ((eof-marker (find-eof-marker len)))
        (multiple-value-bind (xref-off xref-ending)
            (find-prev-line-start file (car eof-marker) (cdr eof-marker))
          (let* ((startxref-start (skip-whitespace
                                   (find-prev-line-start file
                                                         xref-off
                                                         xref-ending)
                                   file))
                 (xref-byte-off (get-xref-byte-off startxref-start)))
            (format t "The byte offset is... ~A~%" xref-byte-off)
            'find-the-xref-section
            'then-maybe-the-trailer-after-that?))))))
