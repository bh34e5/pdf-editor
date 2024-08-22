(in-package #:com.bhester.utils)

(defmacro my-defconstant (name value)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (boundp (quote ,name))
       (defconstant ,name ,value))))

(defun quoted-p (l)
  (and (consp l)
       (eq (car l) 'quote)
       (consp (cdr l))
       (null (cddr l))))

(defmacro condcase (test &rest cases)
  (let* ((g-test (gensym "test"))
         (case-tests (mapcar (lambda (cur-case)
                               (let ((item (car cur-case)))
                                 (cond
                                   ((eq t item) t)
                                   ((and (consp item)
                                         (not (quoted-p item)))
                                    `(member ,g-test (list ,@item)))
                                   (t `(eq ,g-test ,item)))))
                             cases)))
    `(let ((,g-test ,test))
       (cond ,@(mapcar (lambda (test case)
                         `(,test ,(cadr case)))
                       case-tests
                       cases)))))

(defun str->seq (str)
  (labels ((inner (str-len &optional (ind 0))
             (if (<= str-len ind)
               nil
               (cons (char-code (elt str ind))
                     (inner str-len (1+ ind))))))
    (inner (length str))))

(defun tag-value (tag value)
  (cons tag value))

(defun get-tag (value)
  (car value))

(defun get-tagged (value)
  (cdr value))

(defun tagged-p (value)
  (consp value))

(defun has-tag (tag value)
  (and (tagged-p value)
       (eq (get-tag value) tag)))
