;;; Dragon curve generator.

(defpackage #:l-system
  (:use :cl))

(in-package :l-system)

;; Heighway dragon:
;;  * angle 90°
;;  * initial string FX
;;  * string rewriting rules
;;     o X ↦ X+YF+
;;     o Y ↦ −FX−Y.

(defun dragon-curve (n &optional (list '(f x)))
  (if (= n 0)
      (return-from dragon-curve list))
  (dragon-curve
    (decf n)
    (loop for item in list append
         (case item
           (x '(x + y f +))
           (y '(- f x - y))
           (otherwise (list item))))))