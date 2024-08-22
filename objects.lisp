(in-package #:com.bhester.objects)

;; TODO: where do I want to do validations? Like the fact that names cannot
;; contain certain characters, or that the stream dictionary must contain at
;; least the Key "Length", etc.

;; objects

(utils:my-defconstant +pdf-object-types+ (list 'bool
                                               'number
                                               'string
                                               'name
                                               'array
                                               'dictionary
                                               'stream
                                               'null
                                               'indirect
                                               'reference))
(utils:my-defconstant +pdf-number-types+ (list 'integer 'real))

(defun make-pdf-object (obj-type &rest args)
  (unless (member obj-type +pdf-object-types+)
    (error "Invalid object type"))
  (utils:tag-value 'pdf-obj
                    (make-inner-obj obj-type args)))

(defun make-inner-obj (obj-type args-list)
  (utils:condcase obj-type
    ('bool (apply #'make-pdf-bool args-list))
    ('number (apply #'make-pdf-number args-list))
    ('string (apply #'make-pdf-string args-list))
    ('name (apply #'make-pdf-name args-list))
    ('array (apply #'make-pdf-array args-list))
    ('dictionary (apply #'make-pdf-dictionary args-list))
    ('stream (apply #'make-pdf-stream args-list))
    ('null (make-pdf-null))
    ('indirect (apply #'make-pdf-indirect args-list))
    ('reference (apply #'make-pdf-reference args-list))))

(defun object-type (obj)
  (unless (object-p obj)
    (error "Valid is not a pdf object"))
  (cond
    ((bool-p obj) 'bool)
    ((number-p obj) 'number)
    ((string-p obj) 'string)
    ((name-p obj) 'name)
    ((array-p obj) 'array)
    ((dictionary-p obj) 'dictionary)
    ((stream-p obj) 'stream)
    ((null-p obj) 'null)
    ((indirect-p obj) 'indirect)
    ((reference-p obj) 'reference)))

(defun object-p (value)
  (utils:has-tag 'pdf-obj value))

(defun object-value (value)
  (utils:get-tagged value))

;; booleans

(let ((pdf-t (utils:tag-value 'bool t))
      (pdf-f (utils:tag-value 'bool nil)))
  (defun make-pdf-bool (truthy)
    (if truthy pdf-t pdf-f))

  (defun bool-p (value)
    (or (true-p value)
        (false-p value)))

  (defun true-p (value)
    (eq value pdf-t))

  (defun false-p (value)
    (eq value pdf-f)))

;; numbers

(defun make-pdf-number (number-type value)
  (unless (member number-type +pdf-number-types+)
    (error "Invalid number type"))
  (utils:tag-value 'number
                    (utils:condcase number-type
                      ('integer (make-int value))
                      ('real (make-real value)))))

(defun number-p (value)
  (and (object-p value)
       (utils:has-tag 'number (object-value value))))

(defun make-int (value)
  (unless (integerp value)
    (error "Value is not an integer"))
  (utils:tag-value 'integer value))

(defun make-real (value)
  (unless (realp value)
    (error "Value is not a real number"))
  (utils:tag-value 'real value))

;; strings

(defun make-pdf-string (contents)
  (utils:tag-value 'string contents))

(defun string-p (value)
  (and (object-p value)
       (utils:has-tag 'string (object-value value))))

;; names

(defun make-pdf-name (contents)
  ;; TODO: should I be passed a symbol here? do I want to check this is a
  ;; symbol? I think names are unique/singletons like symbols...
  (utils:tag-value 'name contents))

(defun name-p (value)
  (and (object-p value)
       (utils:has-tag 'name (object-value value))))

;; arrays

(defun make-pdf-array (contents)
  (labels ((valid-p (contents)
             (or (null contents)
                 (and (object-p (car contents))
                      (valid-p (rest contents))))))
    (if (valid-p contents)
      (utils:tag-value 'array contents)
      (error "Not all values are pdf objects"))))

(defun array-p (value)
  (and (object-p value)
       (utils:has-tag 'array (object-value value))))

;; dictionaries

(defun make-pdf-dictionary (pairs)
  (labels ((pair-p (p)
             (and (consp p)
                  (consp (cdr p))
                  (null (cddr p))))
           (valid-p (pairs)
             (or (null pairs)
                 (let ((f (car pairs)))
                   (and (pair-p f)
                        (name-p (car f))
                        (object-p (cadr f))
                        (valid-p (rest pairs)))))))
    (if (valid-p pairs)
      (utils:tag-value 'dictionary pairs)
      (error "Not all pairs are valid"))))

(defun dictionary-p (value)
  (and (object-p value)
       (utils:has-tag 'dictionary (object-value value))))

;; streams

(defun make-pdf-stream (dictionary contents)
  (unless (dictionary-p dictionary)
    (error "Dictionary is not valid"))
  (utils:tag-value 'stream (list dictionary contents)))

(defun stream-p (value)
  (and (object-p value)
       (utils:has-tag 'stream (object-value value))))

;; null object

(let ((pdf-null (utils:tag-value 'null nil)))
  (defun make-pdf-null ()
    pdf-null)

  (defun null-p (value)
    (eq value pdf-null)))

;; indirect objects

(defun make-pdf-indirect (object-num generation-num object)
  (unless (object-p object)
    (error "Indirect object value is not a valid object"))
  (utils:tag-value 'indirect (list object-num generation-num object)))

(defun indirect-p (value)
  (and (object-p value)
       (utils:has-tag 'indirect (object-value value))))

;; references

(defun make-pdf-reference (object-num generation-num)
  (utils:tag-value 'reference (list object-num generation-num)))

(defun reference-p (value)
  (and (object-p value)
       (utils:has-tag 'reference (object-value value))))
