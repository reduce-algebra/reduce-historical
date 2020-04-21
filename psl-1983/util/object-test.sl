(BothTimes (load objects mathlib))
(defflavor ship ((x-position 0.0)
		 (y-position 0.0)
		 (x-velocity 0.0)
		 (y-velocity 0.0)
		 )
  ()
  settable-instance-variables
  )  

(setq s (make-instance 'ship))
(=> s x-position)
(=> s y-position)
(=> s x-velocity)
(=> s y-velocity)
(=> s describe)

(=> s set-x-position 1.0)
(=> s set-y-position 2.0)
(=> s set-x-velocity 3.0)
(=> s set-y-velocity 4.0)
(=> s x-position)
(=> s y-position)
(=> s x-velocity)
(=> s y-velocity)
(=> s describe)

(defmethod (ship speed) ()
  (sqrt (+ (* x-velocity x-velocity)
	   (* y-velocity y-velocity)))
  )

(=> s speed)

(defmethod (ship speed) ()
  (let ((x (=> self x-velocity))
	(y (=> self y-velocity)))
    (sqrt (+ (* x x) (* y y)))
    ))

(=> s speed)

(defmethod (ship direction) ()
  (if (= x-velocity 0.0)
      (if (< y-velocity 0.0) 270.0 90.0)
      (atanD (/ y-velocity x-velocity))
      ))

(=> s direction)

(setq s1 (make-instance 'ship 'x-position 3.0 'y-position 3.5))
(=> s1 describe)

(setq s2 (make-instance 'ship 'x-position 6.0 'y-position -6.0
			      'x-velocity 10.0 'y-velocity -10.0))
(=> s2 describe)
