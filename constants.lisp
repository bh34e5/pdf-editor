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

;; TODO: make this into a list of keyword byte arrays so that I can test for
;; memebr equality instead of individual checks. And so I can only add in one
;; place instead of two.

(define-keyword "startxref")
(define-keyword "trailer")
(define-keyword "xref")
(define-keyword "obj")
(define-keyword "endobj")
(define-keyword "R")
(define-keyword "f")
(define-keyword "n")
