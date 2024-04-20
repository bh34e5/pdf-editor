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
    :reader pdf-header-version)
   (%cross-ref-start
    :reader pdf-cross-ref-start)
   (%trailer
    :reader pdf-trailer)))

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

(defclass indirect-obj-ref (pdf-object)
  ((%obj-type :initform :pdf-indirect-obj-ref)
   (%allocation
    :reader ref-allocation
    :type (member :free :allocated)
    :initarg :allocation
    :initform (error "Reference allocation type required"))
   (%byte-offset
    :reader object-byte-offset
    :reader next-free-obj
    :initarg :byte-offset
    :initarg :next-free-obj
    :initform (error "Byte offset or next free object number required"))
   (%object-number
    :reader object-number
    :initarg :object-number
    :initform (error "Object number required"))
   (%generation-number
    :reader object-generation-number
    :initarg :generation-number
    :initform (error "Generation number required"))))

(defclass cross-ref-section ()
  ((%free-objects-list
    :reader free-objects-list
    :initarg :free-objects
    :initform (error "Free objects list required"))
   (%live-objects-list
    :reader live-objects-list
    :initarg :live-objects
    :initform (error "Live objects list required"))))

(defclass trailer ()
  ((%prev
    :reader previous-trailer
    :initarg :prev
    :initform nil)
   (%cross-ref-section
    :reader cross-ref-section
    :initarg :cross-ref-section
    ;; TODO: figure out how to type this...
    ;; :type 'cross-ref-section
    :initform (error "Cross reference section required"))))
