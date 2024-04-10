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

(defun load-pdf (filename)
  (let* ((file-handle (open filename
                            :direction :input
                            :element-type '(unsigned-byte 8))))
    (make-instance 'pdf-wrapper
                   :handle file-handle
                   :line-ending (get-file-line-ending file-handle))))

(defmethod initialize-instance :after ((pdf-wrapper pdf-wrapper) &key)
  ;; TODO: decide whether these should be calculated before creating the object
  ;; or here in the after initialize-instance...
  (setf (slot-value pdf-wrapper '%header-version)
        (read-version-specifier pdf-wrapper line-ending))
  (setf (slot-value pdf-wrapper '%cross-ref-start)
        (find-cross-ref-start pdf-wrapper))
  (setf (slot-value pdf-wrapper '%trailer-start)
        (find-trailer-start pdf-wrapper)))

(defmethod read-bytes ((pdf-wrapper pdf-wrapper)
                       &key
                       (num 1)
                       (direction :forward)
                       (to-line-end-p nil))
  (if (to-line-end-p)
    (if (not (eq direction :forward))
      ;; TODO: better handling :D
      (error "Invalid argument combination")
      (let ((container (make-array num
                                   :fill-pointer 0
                                   :element-type '(unsigned-byte 8)
                                   :adjustable t)))
        (read-to-line-ending (pdf-handle pdf-wrapper)
                             (pdf-line-ending pdf-wrapper)
                             :action (lambda (cur-char)
                                       (vector-push-extend cur-char
                                                           container)))
        container))
    (let ((file-handle (pdf-handle pdf-wrapper)))
      (if (= num 1)
        (read-single-byte file-handle direction)
        (let ((container (make-array num :element-type '(unsigned-byte 8))))
          (if (eq direction :backward)
            (file-position file-handle (- (file-position file-handle) num)))
          (read-sequence container file-handle)
          (if (eq direction :backward)
            (file-position file-handle (- (file-position file-handle) num)))
          container)))))

(defun read-single-byte (file-handle direction)
  (let ((res))
    (if (eq direction :backward)
      (file-position file-handle (- (file-position file-handle) 1)))
    (setf res (read-byte file-handle))
    (if (eq direction :backward)
      (file-position file-handle (- (file-position file-handle) 1)))
    res))

(defmethod scan-forward-line ((pdf-wrapper pdf-wrapper))
  (let ((file-handle (pdf-handle pdf-wrapper))
        (line-ending (pdf-line-ending pdf-wrapper)))
    (read-to-line-ending file-handle line-ending)
    (file-position file-handle
                   (+ (file-position file-handle)
                      (if (eq line-ending :crlf) 2 1)))))

(defmethod scan-back-line ((pdf-wrapper pdf-wrapper))
  (let ((file-handle (pdf-handle pdf-wrapper))
        (line-ending (pdf-line-ending pdf-wrapper)))
    (file-position file-handle
                   (- (file-position file-handle)
                      ;; make sure if we are already at the start of the line,
                      ;; we move back to the previous one
                      (if (eq line-ending :crlf) 3 2)))
    (do ((cur-char (read-byte file-handle) (read-byte file-handle))
         (prev-char nil cur-char))
        ((or (and (eq line-ending :crlf)
                  (eq cur-char +return-char+)
                  (eq prev-char +feed-char+))
             (and (eq line-ending :cr)
                  (eq cur-char +return-char+))
             (and (eq line-ending :lf)
                  (eq cur-char +feed-char+))))
      (file-position file-handle (- (file-position file-handle) 2)))
    (if (eq line-ending :crlf)
      (file-position file-handle (1+ (file-position file-handle))))))

(defmethod read-version-specifier ((pdf-wrapper pdf-wrapper) line-ending)
  (let ((file-handle (pdf-handle pdf-wrapper)))
    (file-position file-handle 0)
    (if (< (file-length file-handle) 5)
      (error "Invalid PDF"))
    (let ((arr (read-bytes pdf-wrapper :num 5))
      (if (not (equalp arr +header-beginning+))
        (error "Invalid PDF")
        (let ((version-spec (read-bytes pdf-wrapper :num 3 :to-line-end-p t)))
          (map 'string #'code-char version-spec)))))))

(defmethod find-eof-sym-start ((pdf-wrapper pdf-wrapper))
  (let ((file-handle (pdf-handle pdf-wrapper))
        (f-len (file-length file-handle))
        (test-arr (make-array 5 :element-type '(unsigned-byte 8))))
    (do ((start (- f-len 4) (1- start)))
        ((< start 0) (error "EOF keyword could not be found"))
      (file-position file-handle start)
      (read-sequence test-arr file-handle)
      (if (equalp test-arr +eof-sym+)
        (return start)))))

(defmethod find-cross-ref-start ((pdf-wrapper pdf-wrapper))
  (let ((file-handle (pdf-handle pdf-wrapper))
        (eof-sym-start (find-eof-sym-start file-handle)))
    (file-position file-handle eof-sym-start)
    (scan-back-line pdf-wrapper) ; move to start of line with offset
    (scan-back-line pdf-wrapper) ; move to start of line with startxref keyword
    (format t "~A~%" (file-position file-handle))
    (let ((arr (read-bytes pdf-wrapper :num (length +startxref+))))
      (if (not (equalp arr +startxref+))
        (error "Error Reading PDF. Could not find `startxref` keyword")
        (progn
          (scan-forward-line pdf-wrapper)
          (let ((offset (read-number file-handle)))
            (if (<= (file-length file-handle) offset)
              (error "Invalid offset for cross-reference"))
            offset))))))

(defmethod find-trailer-start ((pdf-wrapper pdf-wrapper))
  (let ((file-handle (pdf-handle pdf-wrapper))
        (cross-ref-start (pdf-cross-ref-start pdf-wrapper)))
    (let ((kwd (read-keyword file-handle)))
      (if (not (eq kwd +xref+))
        (error "Invalid cross-reference section"))
      (do ((section (read-cross-reference-subsection file-handle)
                    (read-cross-reference-subsection file-handle)))
          ((eq (section +trailer+)))
        nil))
    ;; TODO: read the xref keyword, and then skip the appropriate number of
    ;; lines to get past all the cross reference subsections, until hitting the
    ;; trailer and then return the byte offset
    ))

(defun read-cross-reference-subsection (file-handle)
  ;; read and object. if it's an integer, it's a subsection
  ;; otherwise it should be the keyword `trailer`
  )

(defun get-file-line-ending (file-handle)
  (file-position file-handle 0)
  (do ((cur-char (read-byte file-handle) (read-byte file-handle)))
      ((member cur-char +line-endings+)
       (cond ((and (eq cur-char +return-char+)
                   (eq (read-byte file-handle) +feed-char+))
              :crlf)
             ((eq cur-char +return-char+) :cr)
             ((eq cur-char +feed-char+) :lf)))
    nil))

(defun read-to-line-ending (file-handle line-ending &key (action nil))
  (do ((cur-char (read-byte file-handle)
                 (or test-char (read-byte file-handle)))
       (test-char nil nil))
      ((or (and (eq line-ending :lf)
                (eq cur-char +feed-char+))
           (and (eq line-ending :crlf)
                (eq cur-char +return-char+)
                (eq (setf test-char (read-byte file-handle)) +feed-char+))
           (and (eq line-ending :cr)
                (eq cur-char +return-char+))))
    (when action
      (funcall action cur-char)))
  (file-position file-handle
                 (- (file-position file-handle)
                    (if (eq line-ending :crlf) 2 1))))

(defun read-object (file-handle)
  (let ((c (read-byte file-handle)))
    ;; FIXME: can this be a `.` to start off a real number?
    (cond ((<= (char-code #\0) c (char-code #\9))
           (read-number file-handle c))
          (t (error "Unimplemented object")))))

(defun read-number (file-handle first-char)
  (do ((n (- first-char (char-code #\0)))
       (decimal-places 0 ))
      (())
    ))

(mutil:comment
  (asdf:clear-system "pdf-editor")
  (asdf:load-system "pdf-editor")
  (defvar *test-pdf* (load-pdf #P"~/Downloads/tess_test.pdf"))
  (pdf-header-version *test-pdf*)
  (pdf-trailer-start *test-pdf*)
  (eq (char-code #\Space) (char-code #\Space))
  (with-open-file (f #P"~/Downloads/sobel.pdf" :element-type '(unsigned-byte 8))
    (read-version-specifier f))
  (equalp #(1 2 3) #(1 2 3))
  )
