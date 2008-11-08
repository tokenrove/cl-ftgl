;;;; cl-ftgl -- FTGL binding for CL.  See README for licensing information.

(in-package :cl-ftgl)

(defmacro with-font ((var type file &key (size 12 size-p) (res 72) (char-map :unicode char-map-p)) &body body)
  "Wraps BODY in CREATE-type-FONT and DESTROY-FONT, optionally setting
some of the font parameters per keywords."
  `(let ((,var (,(intern (format nil "CREATE-~A-FONT" type) (find-package :cl-ftgl)) ,file)))
     (unwind-protect
          (progn
            ,(when size-p `(ftgl:set-font-face-size ,var ,size ,res))
            ,(when char-map-p `(ftgl:set-font-char-map ,var ,char-map))
            ,@body)
       (destroy-font ,var))))

(defun get-font-bbox (font string)
  (with-foreign-object (bbox :float 6)
    (%get-font-bbox font string -1 bbox)
    (loop for i below 6 collect (mem-aref bbox :float i))))