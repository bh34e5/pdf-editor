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

;; reader

(defun read-header (file)
  (multiple-value-bind (valid version-start)
      (validate-seq (str->seq "%PDF-") file 0)
    (unless valid
      (error "Invalid pdf header"))
    (read-number file version-start :force-real t)))
