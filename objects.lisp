(in-package #:com.bhester.reader)

(defclass pdf-wrapper ()
  ((%handle
    :reader pdf-handle
    :initarg :handle
    :initform (error "PDF Handle Required"))
   (%line-ending
    :reader pdf-line-ending
    :initarg :line-ending
    :type (member :lf :cr :crlf)
    :initform (error "Line Ending Required"))
   (%header-version
    :reader pdf-header-version
    :initarg :header-version)
   (%cross-ref-start
    :reader pdf-cross-ref-start)
   (%trailer-start
    :reader pdf-trailer-start)))

(defclass pdf-object ()
  ((%obj-type :reader obj-type)))

(defclass pdf-number (pdf-object)
  ((%obj-type :initform :pdf-number)
   (%num-type
    :reader num-type
    :type (member :real :integer)
    :initarg :num-type
    :initform (error "Numeric type required for number"))
   (%numeric-val
    :reader numeric-value
    :initarg :numeric-value
    :initform (error "Numeric value required for number"))))

(defclass pdf-keyword (pdf-object)
  ((%obj-type :initform :pdf-keyword)
   (%bytes
    :reader keyword-bytes
    :initarg :keyword-bytes
    :initform (error "Character bytes required for keyword"))))
