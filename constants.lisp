(in-package #:com.bhester.reader)

(defmacro define-keyword-bytes (name const-string)
  `(alexandria:define-constant
    ,name
    (map '(vector (unsigned-byte 8))
         #'char-code
         ,const-string)
    :test #'equalp))

(defmacro define-keyword-constant (name bytes)
  `(defvar
    ,name
    (make-instance 'pdf-keyword
                   :keyword-bytes ,bytes)))

(defmacro define-keyword (const-string)
  (let ((bytes-name (format nil "+~a+" (string-upcase const-string)))
        (kwd-name (format nil "+KWD-~a+" (string-upcase const-string))))
    `(progn (define-keyword-bytes ,(intern bytes-name)
                                  ,const-string)
            (define-keyword-constant ,(intern kwd-name)
                                     ,(intern bytes-name)))))

(define-keyword-bytes +header-beginning+ "%PDF-")
(define-keyword-bytes +eof-sym+ "%%EOF")

(define-keyword "startxref")
(define-keyword "trailer")
(define-keyword "xref")
(define-keyword "obj")
(define-keyword "endobj")
(define-keyword "R")
