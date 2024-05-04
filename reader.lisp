(in-package #:com.bhester.reader)

(defmacro with-pdf-wrapper (((file-handle line-ending) pdf-wrapper) &body body)
  `(let ((,file-handle (pdf-handle ,pdf-wrapper))
         (,line-ending (pdf-line-ending ,pdf-wrapper)))
     . ,body))

(defun load-pdf (filename)
  (let* ((file-handle (open filename
                            :direction :input
                            :element-type '(unsigned-byte 8))))
    (make-instance 'pdf-wrapper
                   :handle file-handle
                   :line-ending (futils:get-file-line-ending file-handle))))

(defmethod initialize-instance :after ((pdf-wrapper pdf-wrapper) &key)
  ;; TODO: decide whether these should be calculated before creating the object
  ;; or here in the after initialize-instance...
  (let ((line-ending (pdf-line-ending pdf-wrapper)))
    (setf (slot-value pdf-wrapper '%header-version)
          (read-version-specifier pdf-wrapper line-ending))
    (setf (slot-value pdf-wrapper '%cross-ref-start)
          (find-cross-ref-start pdf-wrapper))
    (setf (slot-value pdf-wrapper '%trailer)
          (read-trailer pdf-wrapper))))

(defmethod close-pdf ((pdf-wrapper pdf-wrapper))
  (let ((file-handle (pdf-handle pdf-wrapper)))
    (close file-handle)))

(defmethod read-bytes ((pdf-wrapper pdf-wrapper)
                       &key
                       (num 1)
                       (direction :forward)
                       (to-line-end-p nil))
  (if to-line-end-p
    (if (not (eq direction :forward))
      ;; TODO: better handling :D
      (error "Invalid argument combination")
      (let ((container (make-array num
                                   :fill-pointer 0
                                   :element-type '(unsigned-byte 8)
                                   :adjustable t)))
        (futils:read-to-line-ending (pdf-handle pdf-wrapper)
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
      (file-position file-handle (1- (file-position file-handle))))
    (setf res (read-byte file-handle))
    (if (eq direction :backward)
      (file-position file-handle (1- (file-position file-handle))))
    res))

(defmethod get-object-reference ((pdf-wrapper pdf-wrapper) obj-num gen-num)
  (labels ((find-in-trailer (trailer)
             (when (null trailer)
               (error "Object reference not found"))
             (let* ((xref-section (cross-ref-section trailer))
                    (prev (previous-trailer trailer))
                    (find-res (find-object-ref-info
                               xref-section
                               obj-num
                               gen-num)))
               (if (null find-res)
                 (find-in-trailer prev)
                 find-res))))
    (let ((trailer (pdf-trailer pdf-wrapper)))
      (find-in-trailer trailer))))

(defmethod get-key ((dictionary pdf-dictionary) (key pdf-name))
  (let ((res (find-if (lambda (el)
                        (let ((entry-name (car el)))
                          (equalp (name-bytes entry-name) (name-bytes key))))
                      (key-value-pairs dictionary))))
    (when res
      (cdr res))))

(defun find-object-ref-info (xref-section obj-num gen-num)
  ;; TODO: for now this xref-section is just a list
  (find-if (lambda (el)
             (and (typep el 'indirect-obj-ref-info)
                  (eq (ref-info-allocation el)
                      :allocated)
                  (= (object-number el) obj-num)
                  (= (object-generation-number el) gen-num)))
           xref-section))

(defun ensure-direct-obj (pdf-wrapper possible-ref expected-type)
  (if (typep possible-ref expected-type)
    possible-ref
    (progn
      (assert (typep possible-ref 'indirect-obj-ref))
      (let ((from-ref (read-object-from-ref pdf-wrapper possible-ref)))
        (assert (typep from-ref expected-type))
        from-ref))))

(defmethod read-object-from-ref ((pdf-wrapper pdf-wrapper)
                                 (obj-ref indirect-obj-ref))
  (let ((obj-num (object-number obj-ref))
        (gen-num (object-generation-number obj-ref)))
    ;; TODO assert value exists
    (indirect-obj-value
     (read-object-number pdf-wrapper obj-num gen-num))))

(defmethod read-object-number ((pdf-wrapper pdf-wrapper) obj-num gen-num)
  (let ((ref-obj (get-object-reference pdf-wrapper obj-num gen-num)))
    (let ((byte-off (object-byte-offset ref-obj))
          (file-handle (pdf-handle pdf-wrapper)))
      (file-position file-handle byte-off)
      (read-object pdf-wrapper))))

(defmethod page-count ((pdf-wrapper pdf-wrapper))
  (let* ((trailer (pdf-trailer pdf-wrapper))
         (trailer-dict (trailer-dictionary trailer))
         (root-obj-ref (get-key trailer-dict +name-root+))
         (root-obj (read-object-from-ref pdf-wrapper root-obj-ref))
         (pages-obj-ref (get-key root-obj +name-pages+))
         (pages-obj (read-object-from-ref pdf-wrapper pages-obj-ref))
         (count-obj (get-key pages-obj +name-count+)))
    (cond ((typep count-obj 'pdf-number) (numeric-value count-obj))
          ((typep count-obj 'indirect-obj-ref)
           (let ((count-ref-obj (read-object-from-ref pdf-wrapper count-obj)))
             (numeric-value count-ref-obj)))
          (t (error "Invalid count type")))))

(defun scan-forward-line (file-handle line-ending)
  (futils:read-to-line-ending file-handle line-ending)
  (file-position file-handle
                 (+ (file-position file-handle)
                    (if (eq line-ending :crlf) 2 1))))

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
                  (eq cur-char futils:+return-char+)
                  (eq prev-char futils:+feed-char+))
             (and (eq line-ending :cr)
                  (eq cur-char futils:+return-char+))
             (and (eq line-ending :lf)
                  (eq cur-char futils:+feed-char+))))
      (file-position file-handle (- (file-position file-handle) 2)))
    (if (eq line-ending :crlf)
      (file-position file-handle (1+ (file-position file-handle))))))

(defmethod read-version-specifier ((pdf-wrapper pdf-wrapper) line-ending)
  (let ((file-handle (pdf-handle pdf-wrapper)))
    (file-position file-handle 0)
    (if (< (file-length file-handle) 5)
      (error "Invalid PDF"))
    (let ((arr (read-bytes pdf-wrapper :num 5)))
      (if (not (equalp arr +header-beginning+))
        (error "Invalid PDF")
        (let ((version-spec (read-bytes pdf-wrapper
                                        :num 3
                                        :to-line-end-p t)))
          (map 'string #'code-char version-spec))))))

(defmethod find-eof-sym-start ((pdf-wrapper pdf-wrapper))
  (let* ((file-handle (pdf-handle pdf-wrapper))
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
        (line-ending (pdf-line-ending pdf-wrapper))
        (eof-sym-start (find-eof-sym-start pdf-wrapper)))
    (file-position file-handle eof-sym-start)
    (scan-back-line pdf-wrapper) ; move to start of line with offset
    (scan-back-line pdf-wrapper) ; move to start of line with startxref keyword
    (let ((arr (read-bytes pdf-wrapper :num (length +startxref+))))
      (if (not (equalp arr +startxref+))
        (error "Error Reading PDF. Could not find `startxref` keyword")
        (progn
          (scan-forward-line file-handle line-ending)
          (let ((offset (read-object pdf-wrapper)))
            (assert (eq (type-of offset) 'pdf-number))
            (if (<= (file-length file-handle) (numeric-value offset))
              (error "Invalid offset for cross-reference"))
            (numeric-value offset)))))))

(defmethod read-trailer ((pdf-wrapper pdf-wrapper))
  (let ((file-handle (pdf-handle pdf-wrapper))
        (cross-ref-start (pdf-cross-ref-start pdf-wrapper)))
    (file-position file-handle cross-ref-start)
    (read-trailer-from-kwd pdf-wrapper)))

(defun read-trailer-from-kwd (pdf-wrapper)
  (let ((file-handle (pdf-handle pdf-wrapper)))
    (let ((kwd (read-object pdf-wrapper)))
      (if (not (eq kwd +kwd-xref+))
        (error "Invalid cross-reference section"))
      (let* ((xref-section (read-cross-reference-section pdf-wrapper))
             (trailer-kwd (read-object pdf-wrapper)))
        (assert (eq trailer-kwd +kwd-trailer+))
        (let ((trailer-dict (read-object pdf-wrapper)))
          (assert (typep trailer-dict 'pdf-dictionary))
          (let ((prev-offset-num (get-key trailer-dict +name-prev+)))
            (if (not (null prev-offset-num))
              (progn
                (assert (typep prev-offset-num 'pdf-number))
                (file-position file-handle
                               (numeric-value prev-offset-num))
                (make-instance 'trailer
                               :prev (read-trailer-from-kwd pdf-wrapper)
                               :cross-ref-section xref-section
                               :dictionary trailer-dict))
              (make-instance 'trailer
                             :cross-ref-section xref-section
                             :dictionary trailer-dict))))))))

(defmethod read-cross-reference-section ((pdf-wrapper pdf-wrapper))
  (let ((file-handle (pdf-handle pdf-wrapper)))
    (labels ((read-next ()
               (let ((subsection (read-cross-reference-subsection pdf-wrapper)))
                 (if (eq subsection +kwd-trailer+)
                   (progn
                     (file-position file-handle
                                    (- (file-position file-handle)
                                       (length +trailer+)))
                     '())
                   (append subsection (read-next))))))
      (read-next))))

(defun read-cross-reference-subsection (pdf-wrapper)
  (with-pdf-wrapper ((file-handle line-ending) pdf-wrapper)
    (let ((start-obj (read-object pdf-wrapper)))
      (if (eq start-obj +kwd-trailer+)
        start-obj
        (let ((count-obj (read-object pdf-wrapper)))
          (assert (and (typep start-obj 'pdf-number)
                       (typep count-obj 'pdf-number)))
          (let ((start-obj-num (numeric-value start-obj))
                (count-obj-num (numeric-value count-obj)))
            (labels ((read-entries (obj-num left &optional (entries nil))
                       (if (zerop left)
                         (nreverse entries)
                         (let ((new (read-cross-reference-entry
                                     file-handle
                                     obj-num)))
                           (read-entries (1+ obj-num)
                                         (1- left)
                                         (cons new entries))))))
              (scan-forward-line file-handle line-ending)
              (read-entries start-obj-num count-obj-num))))))))

(defun read-cross-reference-entry (file-handle obj-num)
  (let ((buf (make-array 20 :element-type '(unsigned-byte 8))))
    (read-sequence buf file-handle)
    (labels ((read-num (digits idx &optional (cur 0))
               (if (zerop digits)
                 cur
                 (let* ((ch (elt buf idx))
                        (dig (code->int ch)))
                   (read-num (1- digits)
                             (1+ idx)
                             (+ (* 10 cur)
                                dig))))))
      (let ((byte-off (read-num 10 0))
            (gen-num (read-num 5 11))
            (type-char (elt buf 17)))
        (assert (member type-char (list (char-code #\f)
                                        (char-code #\n))))
        (let ((free-p (eq (char-code #\f) type-char)))
          (if free-p
            (make-instance 'indirect-obj-ref-info
                           :allocation :free
                           :next-free-obj byte-off
                           :object-number obj-num
                           :generation-number gen-num)
            (make-instance 'indirect-obj-ref-info
                           :allocation :allocated
                           :byte-offset byte-off
                           :object-number obj-num
                           :generation-number gen-num)))))))

(defun alpha-p (ch)
  (or (<= (char-code #\a) ch (char-code #\z))
      (<= (char-code #\A) ch (char-code #\Z))))

(defun digit-p (ch)
  (<= (char-code #\0) ch (char-code #\9)))

(defun octal-p (ch)
  (<= (char-code #\0) ch (char-code #\7)))

(defun whitespace-p (ch)
  (member ch '(#x00  ;; NULL
               #x09  ;; Tab
               #x0A  ;; Line Feed
               #x0C  ;; Form Feed
               #x0D  ;; Carriage Return
               #x20  ;; Space
               )))

(defun delimiter-p (ch)
  (member (code-char ch) '(#\( #\) #\< #\> #\[ #\] #\{ #\} #\/ #\%)))

(defun hex-p (ch)
  (or (digit-p ch)
      (<= (char-code #\a) ch (char-code #\f))
      (<= (char-code #\A) ch (char-code #\F))))

(defun int-from-hex-char (ch)
  (assert (hex-p ch))
  (cond ((digit-p ch)            (- ch (char-code #\0)))
        ((>= ch (char-code #\a)) (- ch (char-code #\a)))
        ((>= ch (char-code #\A)) (- ch (char-code #\A)))))

(defun char-from-hex (&rest hex-digs)
  (labels ((rec (digs &optional (cur 0))
             (cond ((null digs) cur)
                   (t (rec (rest digs)
                           (+ (* 16 cur)
                              (int-from-hex-char (first digs))))))))
    (rec hex-digs)))

(define-condition invalid-object (error)
  ((offending-char :initarg :offending-char
                   :reader offending-char))
  (:report (lambda (condition stream)
             (format stream
                     "Could not read object because we encountered ~a.~&"
                     (offending-char condition)))))

(defun read-object (pdf-wrapper)
  (with-pdf-wrapper ((file-handle line-ending) pdf-wrapper)
    (let ((ch (read-byte file-handle)))
      (cond ((eq (char-code #\%) ch)
             (futils:read-to-line-ending file-handle line-ending)
             (read-object pdf-wrapper))
            ((eq (char-code #\/) ch)
             (read-name file-handle))
            ((eq (char-code #\[) ch)
             (read-array pdf-wrapper))
            ((eq (char-code #\() ch)
             (read-ascii-string file-handle line-ending))
            ((eq (char-code #\<) ch)
             (read-possible-dictionary pdf-wrapper))
            ((or (eq ch (char-code #\.))
                 (eq ch (char-code #\+))
                 (eq ch (char-code #\-)))
             (read-number file-handle ch))
            ((whitespace-p ch)
             ;; already incremented with the read, so just try again
             (read-object pdf-wrapper))
            ((digit-p ch)
             (read-possible-object pdf-wrapper ch))
            ((alpha-p ch) (read-keyword file-handle ch))
            (t (error 'invalid-object :offending-char ch))))))

(defun read-name (file-handle)
  (let ((bytes (make-array 1
                           :element-type '(unsigned-byte 8)
                           :fill-pointer 0
                           :adjustable t)))
    (labels ((unread-and-return ()
               (file-position file-handle
                              (1- (file-position file-handle)))
               bytes)
             (read-escaped-char ()
               (let* ((first-char (read-byte file-handle))
                      (second-char (read-byte file-handle)))
                 (vector-push-extend (char-from-hex first-char second-char)
                                     bytes)))
             (read-next ()
               (let ((ch (read-byte file-handle)))
                 (cond ((or (whitespace-p ch)
                            (delimiter-p ch))
                        (unread-and-return))
                       ((eq (char-code #\#) ch)
                        (read-escaped-char)
                        (read-next))
                       (t
                        (vector-push-extend ch bytes)
                        (read-next))))))
      (make-instance 'pdf-name
                     :bytes (read-next)))))

(defun read-array (pdf-wrapper)
  (let ((file-handle (pdf-handle pdf-wrapper)))
    (labels ((reset-and-read ()
               (file-position file-handle
                              (1- (file-position file-handle)))
               (read-object pdf-wrapper))
             (read-next (&optional (objs nil))
               (let ((ch (read-byte file-handle)))
                 ;; TODO: need to check all the places that I'm expecting an
                 ;; ending delimeter and make sure that I take into account the
                 ;; possibility of whitespace
                 (cond ((whitespace-p ch) (read-next objs))
                       ((eq (char-code #\]) ch) (nreverse objs))
                       (t (read-next (cons (reset-and-read) objs)))))))
      (make-instance 'pdf-array
                     :objects (read-next)))))

(defun read-ascii-string (file-handle line-ending)
  (let ((bytes (make-array 8
                           :element-type '(unsigned-byte 8)
                           :fill-pointer 0
                           :adjustable t)))
    (labels ((push-and-continue (ch depth)
               (when ch
                 (vector-push-extend ch bytes))
               (read-next depth))
             (read-line-end (depth)
               (cond ((eq :crlf line-ending)
                      (let ((next-char (read-byte file-handle)))
                        (if (eq futils:+feed-char+ next-char)
                          (push-and-continue futils:+feed-char+ depth)
                          (progn
                            ;; unread the character
                            (file-position file-handle
                                           (1- (file-position file-handle)))
                            (push-and-continue futils:+return-char+ depth)))))
                     ((eq :cr line-ending)
                      (push-and-continue futils:+feed-char+ depth))
                     ((eq :lf line-ending)
                      (push-and-continue futils:+return-char+ depth))))
             (read-escape () (read-escape-sequence file-handle))
             (read-next (&optional (depth 0))
               (let ((ch (read-byte file-handle)))
                 (cond ((eq (char-code #\() ch)
                        (push-and-continue ch (1+ depth)))
                       ((eq (char-code #\)) ch)
                        (if (zerop depth)
                          bytes
                          (push-and-continue ch (1- depth))))
                       ((eq (char-code #\\) ch)
                        (push-and-continue (read-escape) depth))
                       ((eq futils:+return-char+ ch)
                        (read-line-end depth))
                       (t (push-and-continue ch depth))))))
      (read-next))))

(defun read-escape-sequence (file-handle)
  (labels ((read-trailing-newline ()
             (let ((ch (read-byte file-handle)))
               (unless (eq futils:+feed-char+ ch)
                 (file-position file-handle
                                (1- (file-position file-handle))))
               nil)))
    (let ((ch (read-byte file-handle)))
      (cond ((octal-p ch) (read-octal-escape file-handle ch))
            ((eq (char-code #\n) futils:+feed-char+))
            ((eq (char-code #\r) futils:+return-char+))
            ((eq (char-code #\t) #\tab))
            ((eq (char-code #\b) #\backspace))
            ((eq (char-code #\f) #\page))
            ((eq (char-code #\() #\())
            ((eq (char-code #\)) #\)))
            ((eq (char-code #\\) #\\))
            ((eq ch futils:+return-char+)
             (read-trailing-newline))
            (t nil)))))

(defun read-octal-escape (file-handle first-char)
  (labels ((unread-and-return (res)
             (file-position file-handle
                            (1- (file-position file-handle)))
             res)
           (read-next (accum &optional (len 1))
             (if (= len 3)
               accum
               (let ((ch (read-byte file-handle)))
                 (if (not (octal-p ch))
                   (unread-and-return accum)
                   (read-next (+ (* 8 accum)
                                 (code->int ch))
                              (1+ len)))))))
    (read-next (code->int first-char))))

(defun read-hex-string (file-handle first-char)
  (labels ((read-first-char (&optional (bytes nil))
             (let ((ch (read-byte file-handle)))
               (cond ((eq (char-code #\>) ch)
                      (nreverse bytes))
                     ((whitespace-p ch) (read-first-char bytes))
                     (t (read-second-char ch bytes)))))
           (read-second-char (first-char &optional (bytes nil))
             (let ((ch (read-byte file-handle)))
               (cond ((eq (char-code #\>) ch)
                      (nreverse (cons (char-from-hex first-char
                                                     (char-code #\0))
                                      bytes)))
                     ((whitespace-p ch) (read-second-char first-char bytes))
                     (t (read-first-char (cons (char-from-hex first-char ch)
                                               bytes)))))))
    (make-instance 'pdf-string
                   :bytes (if (whitespace-p first-char)
                            (read-first-char)
                            (read-second-char first-char)))))

(defun read-possible-dictionary (pdf-wrapper)
  (let* ((file-handle (pdf-handle pdf-wrapper))
         (first-char (read-byte file-handle)))
    (if (eq (char-code #\<) first-char)
      (read-possible-stream pdf-wrapper)
      (read-hex-string file-handle first-char))))

(defun read-possible-stream (pdf-wrapper)
  (let* ((file-handle (pdf-handle pdf-wrapper))
         (dict (read-dictionary pdf-wrapper))
         (reset-position (file-position file-handle)))
    (assert (typep dict 'pdf-dictionary))
    (labels ((reset-and-return ()
               (file-position file-handle reset-position)
               dict))
      (handler-case
          (let ((next-obj (read-object pdf-wrapper)))
            (if (not (eq next-obj +kwd-stream+))
              (reset-and-return)
              (make-instance 'pdf-stream
                             :dictionary dict
                             :encoded-bytes (read-stream-bytes pdf-wrapper
                                                               dict))))
        (invalid-object ()
          (reset-and-return))
        (end-of-file ()
          (reset-and-return))))))

(defun read-stream-bytes (pdf-wrapper stream-dictionary)
  (with-pdf-wrapper ((file-handle line-ending) pdf-wrapper)
    (let* ((stm-length-obj (get-key stream-dictionary +name-length+))
           (stm-length (ensure-direct-obj pdf-wrapper
                                          stm-length-obj
                                          'pdf-number)))
      (let ((res-array (make-array (numeric-value stm-length)
                                    :element-type '(unsigned-byte 8))))
        ;; read the next line... the line ending must either be a single line
        ;; feed or a carriage return followed by a line feed... no single
        ;; carriage return is allowed
        (cond ((eq line-ending :lf)
               (file-position file-handle
                              (1+ (file-position file-handle))))
              (t
               (file-position file-handle
                              (+ 2 (file-position file-handle)))))
        (read-sequence res-array file-handle)
        res-array))))

(defun read-dictionary (pdf-wrapper)
  (let ((file-handle (pdf-handle pdf-wrapper)))
    (labels ((read-entry ()
               (let ((name (read-name file-handle))
                     (value (read-object pdf-wrapper)))
                 ;; TODO: return actual objects (maybe). Just until it works,
                 ;; using a-list (ordered in the same way the objects are read)
                 (cons name value)))
             (finish-dict (entries)
               (read-byte file-handle) ;; read the second #\>
               (nreverse entries))
             (read-entries (&optional (entries nil))
               (let ((ch (read-byte file-handle)))
                 (cond ((eq (char-code #\>) ch)
                        (finish-dict entries))
                       ((whitespace-p ch)
                        (read-entries entries))
                       ((eq (char-code #\/) ch)
                        (read-entries (cons (read-entry)
                                            entries)))))))
      (make-instance 'pdf-dictionary
                     :pairs (read-entries)))))

(defun read-possible-object (pdf-wrapper first-char)
  (let* ((file-handle (pdf-handle pdf-wrapper))
         (num (read-number file-handle first-char))
         (reset-position (file-position file-handle)))
    (if (or (eq :real (num-type num))
            (< (numeric-value num) 0))
      ;; number with a decimal point or a negative number, must be a number
      num
      ;; integer result, could still be an object or reference
      (labels ((reset-and-return ()
                 (file-position file-handle reset-position)
                 num)
               (read-indirect-object (obj-num gen-num)
                 (let* ((obj-val (read-object pdf-wrapper))
                        (end-kwd (read-object pdf-wrapper)))
                   (assert (eq end-kwd +kwd-endobj+))
                   (make-instance 'indirect-obj
                                  :object-number obj-num
                                  :generation-number gen-num
                                  :object obj-val)))
               (try-third (next-obj)
                 (let ((third-obj (read-object pdf-wrapper)))
                   (if (and (typep third-obj 'pdf-keyword)
                            (or (eq third-obj +kwd-obj+)
                                (eq third-obj +kwd-r+)))
                     (let ((obj-num (numeric-value num))
                           (gen-num (numeric-value next-obj)))
                       (if (eq third-obj +kwd-r+)
                         (make-instance 'indirect-obj-ref
                                        :object-number obj-num
                                        :generation-number gen-num)
                         (read-indirect-object obj-num gen-num)))
                     ;; this wasn't an object / reference after all.
                     ;; return the number
                     (reset-and-return))))
               (try-second-and-third ()
                 (let ((next-obj (read-object pdf-wrapper)))
                   (if (and (typep next-obj 'pdf-number)
                            (eq :integer (num-type next-obj))
                            (>= (numeric-value next-obj) 0))
                     (try-third next-obj)
                     ;; next-obj was not a positive integer, so this can't be a
                     ;; reference. return the original number
                     (reset-and-return)))))
        (handler-case
            (try-second-and-third)
          (invalid-object ()
            (reset-and-return))
          (end-of-file ()
            (reset-and-return)))))))

(defun code->int (ch)
  (assert (digit-p ch))
  (- ch (char-code #\0)))

(defun read-number (file-handle first-char)
  ;;; FIXME: this doesn't handle plus/minus sign
  (labels ((read-next ()
             (read-byte file-handle))
           (read-num-rec (ch &optional (n 0) (real-p nil) (decimal-places 0))
             (if (not (or (digit-p ch) (eq (char-code #\.) ch)))
               (progn
                 ;; move the handle back a byte, then return the number
                 (file-position file-handle
                                (1- (file-position file-handle)))
                 (values
                  (if real-p
                    (/ n (expt 10 decimal-places))
                    n)
                  real-p))
               (if (eq (char-code #\.) ch)
                 (if real-p
                   (error "Invalid number, two decimal places...")
                   (read-num-rec (read-next) n t decimal-places))
                 (progn
                   (read-num-rec
                    (read-next)
                    (+ (* 10 n)
                       (code->int ch))
                    real-p
                    (if real-p (1+ decimal-places) decimal-places)))))))
    (multiple-value-bind (num real-p)
        (read-num-rec first-char)
      (make-instance 'pdf-number
                     :num-type (if real-p :real :integer)
                     :numeric-value num))))

(defun read-keyword (file-handle first-char)
  (labels ((read-next ()
             (read-byte file-handle))
           (kwd-from (bytes)
             (cond
               ((equalp bytes +startxref+) +kwd-startxref+)
               ((equalp bytes +trailer+) +kwd-trailer+)
               ((equalp bytes +xref+) +kwd-xref+)
               ((equalp bytes +obj+) +kwd-obj+)
               ((equalp bytes +endobj+) +kwd-endobj+)
               ((equalp bytes +stream+) +kwd-stream+)
               ((equalp bytes +endstream+) +kwd-endstream+)
               ((equalp bytes +r+) +kwd-r+)
               ((equalp bytes +f+) +kwd-f+)
               ((equalp bytes +n+) +kwd-n+)
               (t (error "Unrecognized keyword"))))
           (read-kwd-rec (ch
                          &optional
                          (bytes
                           (make-array 1
                                       :fill-pointer 0
                                       :element-type '(unsigned-byte 8)
                                       :adjustable t)))
             (if (not (alpha-p ch))
               (progn
                 (file-position file-handle
                                (1- (file-position file-handle)))
                 (kwd-from bytes))
               (progn
                 (vector-push-extend ch bytes)
                 (read-kwd-rec (read-next) bytes)))))
    (read-kwd-rec first-char)))
