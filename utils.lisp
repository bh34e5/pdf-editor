(in-package #:com.bhester.utils)

(defmacro my-defconstant (name value)
  `(unless (boundp (quote ,name))
     (defconstant ,name ,value)))
