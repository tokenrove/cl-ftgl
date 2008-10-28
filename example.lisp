;;;; cl-ftgl -- FTGL binding for CL.  See README for licensing information.
;;;;
;;;; A fairly literal translation of FTGL's c-demo.c, which was
;;;; copyright 2008 Sam Hocevar <sam@zoy.org>.

(asdf:oos 'asdf:load-op '#:cl-opengl)
(asdf:oos 'asdf:load-op '#:cl-glu)
(asdf:oos 'asdf:load-op '#:cl-glut)
(asdf:oos 'asdf:load-op '#:cl-ftgl)
(asdf:oos 'asdf:load-op '#:anaphora)

(defpackage #:cl-ftgl-example
  (:use #:cl #:cl-ftgl #:anaphora))

(in-package :cl-ftgl-example)

(defparameter *width* 640)
(defparameter *height* 480)

(defclass demo-window (glut:window)
  ((file :reader file-of :initarg :file)
   (fonts :accessor fonts-of)
   (frames :accessor frames-of :initform 0)
   (last-fps :accessor last-fps-of :initform 0)
   (font-index :accessor font-index-of :initarg :font-index))
  (:default-initargs :width *width* :height *height*
                     :title "simple FTGL CL demo"
                     :mode '(:depth :double :rgba)
                     :font-index 0))

(defmethod glut:display-window :before ((it demo-window))
  (gl:matrix-mode :projection)
  (%gl:load-identity)
  (glu:perspective 90 (/ *width* *height*) 1 1000)
  (%gl:matrix-mode :modelview)
  (%gl:load-identity)
  (glu:look-at 0 0 (/ *width* 2) 0 0 0 0 1 0)

  (setf (fonts-of it)
        (list (aprog1 (ftgl:create-extrude-font (file-of it))
                (ftgl:set-font-face-size it 80 72)
                (ftgl:set-font-depth it 10)
                (ftgl:set-font-outset it 0 3)
                (ftgl:set-font-char-map it :unicode))
              (aprog1 (ftgl:create-buffer-font (file-of it))
                (ftgl:set-font-face-size it 80 72)
                (ftgl:set-font-char-map it :unicode)))))
  ;; (ftgl:create-custom-font file nil (cffi:callback make-halo-glyph))

(defmethod glut:idle ((it demo-window)) (glut:post-redisplay))

(defmethod glut:display ((it demo-window))
  (let* ((now (glut:get :elapsed-time))
         (n (/ now 20))
         (t1 (sin (/ n 80)))
         (t2 (sin (1+ (/ n 50))))
         (t3 (sin (+ 2 (/ n 30))))
         (ambient (list (/ (+ t1 2) 3) (/ (+ t2 2) 3) (/ (+ t3 2) 3) 0.3))
         (diffuse #(1 0.9 0.9 1))
         (specular #(1 0.7 0.7 1))
         (position #(100 100 0 1))
         (front-ambient #(0.7 0.7 0.7 0)))
    (gl:clear :color-buffer :depth-buffer)
    (gl:enable :lighting)
    (gl:enable :depth-test)
    (gl:with-pushed-matrix
      (gl:translate -0.9 -0.2 -10)
      (gl:light :light1 :ambient ambient)
      (gl:light :light1 :diffuse diffuse)
      (gl:light :light1 :specular specular)
      (gl:light :light1 :position position)
      (gl:enable :light1))

    (gl:with-pushed-matrix
      (gl:material :front :ambient front-ambient)
      (%gl:color-material :front :diffuse)
      (gl:translate 0 0 20)
      (gl:rotate (/ n 1.11) 0 1 0)
      (gl:rotate (/ n 2.23) 1 0 0)
      (gl:rotate (/ n 3.17) 0 0 1)
      (gl:translate -260 -0.2 0)
      (gl:color 0 0 0)
      (ftgl:render-font (elt (fonts-of it) (font-index-of it)) "Hello FTGL!" :all))

    (glut:swap-buffers)
    (incf (frames-of it))
    (when (> #1=(- now (last-fps-of it)) 5000)
      (format t "~&~A frames in 5 seconds = ~A FPS~%" (frames-of it) (/ (* (frames-of it) 1000) (float #1#)))
      (incf (last-fps-of it) 5000)
      (setf (frames-of it) 0))))

(defmethod glut:keyboard ((it demo-window) key x y)
  (declare (ignore x y))
  (cond ((eql key #\Esc)
         (dolist (font (fonts-of it)) (ftgl:destroy-font font))
         (glut:destroy-current-window))
        ((eql key #\Tab)
         (setf (font-index-of it) (mod (1+ (font-index-of it)) (length (fonts-of it)))))))

(defun main (file)
  (glut:init)
  (glut:display-window (make-instance 'demo-window :file file)))

(main "example.otf")
