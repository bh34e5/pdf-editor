(in-package #:com.bhester.reader)

;; TODO: figure out if these two can become functions...?

(defmacro str->bytes (str)
  `(map '(vector (unsigned-byte 8)) #'char-code ,str))

(defmacro sym-name-from (pattern sub)
  `(format nil ,pattern (string-upcase ,sub)))

(defmacro define-bytes-const (name const-string)
  `(alexandria:define-constant
    ,name
    ,(str->bytes const-string)
    :test #'equalp))

(defmacro define-keyword-constant (name bytes)
  `(defvar
    ,name
    (make-instance 'pdf-keyword
                   :keyword-bytes ,bytes)))

(defmacro define-keyword (const-string)
  (let ((bytes-name (sym-name-from "+~a+" const-string))
        (kwd-name (sym-name-from "+KWD-~a+" const-string)))
    `(progn (define-bytes-const ,(intern bytes-name)
                                ,const-string)
            (define-keyword-constant ,(intern kwd-name)
                                     ,(intern bytes-name)))))

(define-bytes-const +header-beginning+ "%PDF-")
(define-bytes-const +eof-sym+ "%%EOF")

(defmacro define-constants (name &rest constant-strings)
  (let* ((bytes-names (mapcar (lambda (const-string)
                                (intern (sym-name-from "+~a+" const-string)))
                              constant-strings))
         (kwd-names (mapcar (lambda (const-string)
                              (intern (sym-name-from "+KWD-~a+" const-string)))
                            constant-strings)))
    `(progn
       ,@(mapcar (lambda (const-string bytes-name kwd-name)
                   `(progn
                      (define-bytes-const ,bytes-name ,const-string)
                      (define-keyword-constant ,kwd-name ,bytes-name)))
                 constant-strings
                 bytes-names
                 kwd-names)
       (alexandria:define-constant
        ,name
        (list ,@bytes-names)
        :test #'equalp))))

(define-constants +all-keyword-bytes+
  "startxref"
  "trailer"
  "xref"
  "obj"
  "endobj"
  "stream"
  "endstream"
  "R"
  "f"
  "n")

;;; TODO: make these into actual null/boolean objects...
;;; NOTE: they are also not being checked in the `read-keyword` function, so I
;;; will need to add something to read these as objects as well.
(define-keyword "null")
(define-keyword "true")
(define-keyword "false")

(defun singleton-p (l)
  (and (consp l)
       (null (cdr l))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun names-equal (&rest names)
    (or (null names)
        (singleton-p names)
        (and (equalp (name-bytes (first names))
                     (name-bytes (second names)))
             (apply #'names-equal (rest names))))))

(defmacro define-name (name-str)
  (let ((bytes-name (sym-name-from "+NAME-BYTES-~a+" name-str))
        (name-name (sym-name-from "+NAME-~a+" name-str)))
    `(progn
       (define-bytes-const ,(intern bytes-name) ,name-str)
       (alexandria:define-constant
         ,(intern name-name)
         (make-instance 'pdf-name
                        :bytes ,(intern bytes-name))
         :test #'names-equal))))

;;; Frequently used names

(define-name "Count")
(define-name "Length")
(define-name "Pages")
(define-name "Prev")
(define-name "Root")
