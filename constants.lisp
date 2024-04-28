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

;;; TODO: make these into actual null/boolean objects...
;;; NOTE: they are also not being checked in the `read-keyword` function, so I
;;; will need to add something to read these as objects as well.
(define-keyword "null")
(define-keyword "true")
(define-keyword "false")

(defun singleton-p (l)
  (and (consp l)
       (null (cdr l))))

(defun names-equal (&rest names)
  (or (null names)
      (singleton-p names)
      (and (equalp (name-bytes (first names))
                   (name-bytes (second names)))
           (apply #'names-equal (rest names)))))

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
(define-name "Pages")
(define-name "Prev")
(define-name "Root")
