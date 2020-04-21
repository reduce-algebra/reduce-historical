% Some interpreted tests of CATCH and THROW for MAIN 9

(Dashed "Expect an Error, that FOO uncaught")
(THROW 'FOO 1)

(shouldbe "Catch should return argument "
	(CATCH 'FOO 1)
	1)

(Dashed "Expect 1 to be printed, and 2 returned, no 3")
(Shouldbe "Catch the Thrown value"
	(CATCH 'FOO (PROGN (print 1) (throw 'foo 2) (print 3)))
	2)

