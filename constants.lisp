(in-package #:com.bhester.reader)

(defmacro define-keyword-constant (name const-string)
  `(alexandria:define-constant ,name
                               (map '(vector (unsigned-byte 8))
                                    #'char-code
                                    ,const-string)
                               :test #'equalp))

(alexandria:define-constant +return-char+ (char-code #\Return))
(alexandria:define-constant +feed-char+ (char-code #\LineFeed))

(define-keyword-constant +header-beginning+ "%PDF-")
(define-keyword-constant +eof-sym+ "%%EOF")

(define-keyword-constant +startxref+ "startxref")

(alexandria:define-constant +line-endings+ `(,+return-char+ ,+feed-char+)
                            :test #'equalp)
