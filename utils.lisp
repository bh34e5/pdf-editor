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

(defun length-1 (l)
  (and (consp l)
       (null (cdr l))))

(defmacro -> (&rest forms)
  (let ((rforms (reverse forms)))
    (%-> rforms)))

(defun %-> (rforms)
  (if (length-1 rforms)
    (car rforms)
    (list* (caar rforms) (%-> (cdr rforms)) (cdar rforms))))

(defmacro letmv* ((&rest bindings) &body forms)
  (%letmv* bindings forms))

(defun %letmv* (bindings forms)
  (cond
    ((null bindings) `(progn ,@forms))
    ((consp (caar bindings))
     `(multiple-value-bind ,(caar bindings)
          ,(cadar bindings)
        ,(%letmv* (cdr bindings) forms)))
    (t
     `(let (,(car bindings))
        ,(%letmv* (cdr bindings) forms)))))

(defmacro letmv ((&rest bindings) &body forms)
  (%letmv bindings forms))

(defun %letmv (bindings forms)
  (let* ((sym-swapped (mapcar (lambda (binding)
                                (if (consp (car binding))
                                  (cons  (mapcar (lambda (b)
                                                   (declare (ignore b))
                                                   (gensym))
                                                 (car binding))
                                         (cdr binding))
                                  (cons (gensym) (cdr binding))))
                              bindings))
         (rebindings (apply #'append
                            (mapcar (lambda (binding ss-binding)
                                      (if (consp (car binding))
                                        (mapcar (lambda (b-sym ssb-sym)
                                                  (list b-sym ssb-sym))
                                                (car binding)
                                                (car ss-binding))
                                        (list (list (car binding)
                                                    (car ss-binding)))))
                                    bindings
                                    sym-swapped))))
    `(letmv* ,sym-swapped
       (let ,rebindings
         ,@forms))))
