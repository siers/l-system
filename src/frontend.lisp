;; Curve's drawer.

(in-package #:l-system)

(defparameter *n* 16) ;; iterations
(defparameter *dimm-x* 640)
(defparameter *dimm-y* 480)
(defparameter *colour*
  (sdl:color :r 255 :g #xc0 :b 0))
(defparameter *bk-colour*
  (sdl:color :r 255 :b 0 :g 0))

(defun pseudo-cos (angle)
  (case angle
    (0 1)
    (180 -1)
    (otherwise 0)))

(defun pseudo-sin (angle)
  (case angle
    (90 1)
    (270 -1)
    (otherwise 0)))

(defun change-angle (angle fun)
  (mod (funcall fun angle 90) 360))

(defun draw-dragon-curve (n)
  (let ((directions (dragon-curve n)) (angle 0)
        (x 0) (y 0) (halfx (/ *dimm-x* 2)) (halfy (/ *dimm-y* 2)))
    (loop for command in directions do
         (case command
           (f
            (incf x (pseudo-cos angle))
            (incf y (pseudo-sin angle))
            (sdl:draw-pixel-* (+ x halfx) (+ y halfy) :color *colour*))
           (- (setf angle (change-angle angle #'+)))
           (+ (setf angle (change-angle angle #'-)))))))

(defun draw ()
  (sdl:clear-display *bk-colour*)
  (draw-dragon-curve *n*))

(defun run ()
  (sdl:with-init ()
    (sdl:window *dimm-x* *dimm-y*)
    (sdl:clear-display *bk-colour*)
    (setf (sdl:frame-rate) 5)
    (draw)
    
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
        (cond
          ((sdl:key= key :SDL-KEY-ESCAPE)
           (sdl:push-quit-event))
          ((sdl:key= key :SDL-KEY-R) ;; redraw after compiled
           (draw))
          ((sdl:key= key :SDL-KEY-N)
           (incf *n*)
           (draw))
          ((sdl:key= key :SDL-KEY-P)
           (decf *n*)
           (draw))))
      (:idle () (sdl:update-display)))))