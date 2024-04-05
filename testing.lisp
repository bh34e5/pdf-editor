(in-package #:com.bhester.testing)

;; one thing I found while doing this: the linefeed immediately after stream was
;; necessary so that zlib recognized this as a valid stream.

(defun deflate-stream (out-stm in-stm)
  (chipz:decompress out-stm 'chipz:zlib  in-stm))
