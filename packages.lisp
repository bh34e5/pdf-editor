(defpackage #:com.bhester.utils
  (:use :cl)
  (:export #:my-defconstant
           #:condcase
           #:str->seq
           #:tag-value
           #:get-tag
           #:get-tagged
           #:tagged-p
           #:has-tag))

(defpackage #:com.bhester.objects
  (:use :cl)
  (:local-nicknames (#:utils #:com.bhester.utils))
  (:export #:make-object))

(defpackage #:com.bhester.reader
  (:use :cl)
  (:local-nicknames (#:objs #:com.bhester.objects)
                    (#:utils #:com.bhester.utils))
  (:import-from #:com.bhester.utils #:str->seq))

(defpackage #:com.bhester.pdf-editor
  (:use :cl)
  (:local-nicknames (#:objs #:com.bhester.objects)
                    (#:reader #:com.bhester.reader)))
