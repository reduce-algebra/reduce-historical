
(de topleveltak (x y z) (tak x y z))

(de tak (x y z)
  (cond ((null (ilessp y x))  z)
	(t (tak (tak (isub1 x) y z)
		(tak (isub1 y) z x)
		(tak (isub1 z) x y)))))

