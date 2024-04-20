(defpackage #:com.bhester.pdf-editor
  (:use :cl))

(defpackage #:com.bhester.file-utils
  (:use :cl)
  (:export #:get-file-line-ending
           #:read-to-line-ending
           #:+return-char+
           #:+feed-char+))

(defpackage #:com.bhester.reader
  (:use :cl)
  (:local-nicknames (#:futils #:com.bhester.file-utils)))

(defpackage #:com.bhester.testing
  (:use :cl))
