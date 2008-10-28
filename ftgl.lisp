;;;; cl-ftgl -- FTGL binding for CL.  See README for licensing information.

(in-package :cl-ftgl)

(defmacro with-font ((var type file) &body body)
  `(let ((,var (,(intern (format nil "CREATE-~A-FONT" type) (find-package :cl-ftgl)) ,file)))
     (unwind-protect (progn ,@body) (destroy-font ,var))))
