% Some interpreted tests of PROG for MAIN9

(Dashed "Expect 1 printed")
(shouldbe "Prog Value" (PROG NIL (print 1)) NIL)

(Dashed "Expect 1 and 2 printed")
(shouldbe "Prog value" (PROG NIL (print 1) (print 2) (return 3)) 3)

(Dashed "Test 1 var PROG binding")
(ShouldBe  "Before PROG, x=" (setq x 2) 2)
(Shouldbe  "Prog value"
	(PROG (X) 
		(ShouldBe "Inside prog, x=" x NIL)
		(setq x 3) 
		(ShouldBe  "After setq, x=" x 3)
	)
	NIL)
(ShouldBe "after exit, x=" x 2)

(Dashed "Test 2 var PROG binding")
(ShouldBe  "Before PROG, x=" (setq x 2) 2)
(ShouldBe  "Before PROG, y=" (setq y 20) 20)
(Shouldbe  "Prog value"
	(PROG (X Y) 
		(ShouldBe "Inside prog, x=" x NIL)
		(ShouldBe "Inside prog, y=" y NIL)
		(setq x 3) 
		(setq y 30) 
		(ShouldBe  "After setq, x=" x 3)
		(ShouldBe  "After setq, y=" y 30)
	)
	NIL)
(ShouldBe "after exit, x=" x 2)
(ShouldBe "after exit, y=" y 20)

(dashed "Test simple loop in prog")
(shouldbe "Return 0 after 5 loops"
(prog (x)
	(setq x 6)
	(prin2t "Expect x to decrease from 5 to 1")
 L	(setq x (sub1 x))
	(prin2 "  In loop x=")(prin2T x)
	(cond ((greaterp x 1) (go L)))
	(return 0))
	0)
(shouldbe "Return 1 after 5 loops"
(prog (x)
	(setq x 5)
	(prin2T "Expect x to decrease from 5  to 1")
 L	(cond ((lessp x 1) (return 1)))
	(prin2 "  In loop, x=") (Prin2t x)
	(setq x (sub1 x))
	(go L))
	1)
