% PSL-TIMER.SL  Source of PSL "spectral" tests

% Compile this file to produce psl-timer.b
% then LAPIN the file "time-psl.sl"
'(
(sstatus translink t)
(declare (localf tak gtak))
(def de (macro (x) (cons 'defun (cdr x))))
(def igreaterp (macro (x) (cons '> (cdr x))))
(def ilessp (macro (x) (cons '< (cdr x))))
(def iadd1 (macro (x) (cons '1+ (cdr x))))
(def isub1 (macro (x) (cons '1- (cdr x))))
(def itimes2 (macro (x) (cons '* (cdr x))))
(allocate 'fixnum 2000)
(allocate 'list 500)
(setq $gcprint t)
(defun time () (* (car (ptime)) 17))
(defun reclaim () (gc))
)
(de TestSetup ()
(progn
    (setq TestList (PrepareTest 1000))
    (setq TestList2 (PrepareTest 2000))
    (MakeLongList)
    (setq EvalForm '(setq Foo (cadr '(1 2 3))))))

(de MakeLongList ()
(prog (I)
    (setq LongList '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
    (setq I 0)
loop
    (cond ((igreaterp I 5) (return nil)))
    (setq LongList (append LongList LongList))
    (setq I (iadd1 I))
    (go loop)))

(de PrepareTest (n)
   (prog (l i)
      (setq i -1 l nil)
      top
      (cond ((ilessp n i) (return l)))
      (setq i (iadd1 i)
	    l (cons nil l))
      (go top)))

(de Cdr1Test (N)
(prog (I L)
    (setq I -1)
loop
    (setq I (iadd1 I))
    (setq L LongList)
    (cond ((igreaterp I N) (return nil)))
loop1
    (cond ((atom (setq L (cdr L))) (go loop)))
    (go loop1)))

(de Cdr2Test (N)
(prog (I L)
    (setq I -1)
loop
    (setq I (iadd1 I))
    (setq L LongList)
    (cond ((igreaterp I N) (return nil)))
loop1
    (cond ((null (setq L (cdr L))) (go loop)))
    (go loop1)))

(de CddrTest (N)
(prog (I L)
    (setq I -1)
loop
    (setq I (iadd1 I))
    (setq L LongList)
    (cond ((igreaterp I N) (return nil)))
loop1
    (cond ((null (setq L (cddr L))) (go loop)))
    (go loop1)))

(de ListOnlyCdrTest1 ()
   (prog (l1 l2)
      (setq l1 TestList)
      top
      (setq l2 TestList)
      again
      (cond ((null (setq l2 (cdr l2)))
	     (cond ((null (setq l1 (cdr l1)))
		    (return nil))
		   (t (go top))))
	    (t (go again)))))

(de ListOnlyCddrTest1 ()
   (prog (l1 l2)
      (setq l1 TestList2)
      top
      (setq l2 TestList2)
      again
      (cond ((null (setq l2 (cddr l2)))
	     (cond ((null (setq l1 (cddr l1)))
		    (return nil))
		   (t (go top))))
	    (t (go again)))))

(de ListOnlyCdrTest2 ()
   (prog (l1 l2)
      (setq l1 TestList)
      top
      (setq l2 TestList)
      again
      (cond ((atom (setq l2 (cdr l2)))
	     (cond ((atom (setq l1 (cdr l1)))
		    (return nil))
		   (t (go top))))
	    (t (go again)))))

(de ListOnlyCddrTest2 ()
   (prog (l1 l2)
      (setq l1 TestList2)
      top
      (setq l2 TestList2)
      again
      (cond ((atom (setq l2 (cddr l2)))
	     (cond ((atom (setq l1 (cddr l1)))
		    (return nil))
		   (t (go top))))
	    (t (go again)))))

(de EmptyTest (N)
(prog (I)
    (setq I 0)
loop
    (cond ((igreaterp I N) (return nil)))
    (setq I (iadd1 I))
    (go loop)))

(de SlowEmptyTest (N)
(prog (I)
    (setq I 0)
loop
    (cond ((greaterp I N) (return nil)))
    (setq I (add1 I))
    (go loop)))

(de ReverseTest (N)
(prog (I)
    (setq I 0)
loop
    (cond ((igreaterp I N) (return nil)))
    (reverse LongList)
    (setq I (iadd1 I))
    (go loop)))

(de MyReverse1Test (N)
(prog (I)
    (setq I 0)
loop
    (cond ((igreaterp I N) (return nil)))
    (myreverse1 LongList)
    (setq I (iadd1 I))
    (go loop)))

(de myreverse1 (L)
(prog (M)
loop
    (cond ((atom L) (return M)))
    (setq M (cons (car L) M))
    (setq L (cdr L))
    (go loop)))

(de MyReverse2Test (N)
(prog (I)
    (setq I 0)
loop
    (cond ((igreaterp I N) (return nil)))
    (myreverse2 LongList)
    (setq I (iadd1 I))
    (go loop)))

(de myreverse2 (L)
(prog (M)
loop
    (cond ((null L) (return M)))
    (setq M (cons (car L) M))
    (setq L (cdr L))
    (go loop)))

(de LengthTest (N)
(prog (I)
    (setq I 0)
loop
    (cond ((igreaterp I N) (return nil)))
    (length LongList)
    (setq I (iadd1 I))
    (go loop)))

(de Fact (N)
    (cond ((ilessp N 2) 1) (t (itimes2 N (Fact (isub1 N))))))

(de ArithmeticTest (N)
(prog (I)
    (setq I 0)
loop
    (cond ((igreaterp I N) (return nil)))
    (Fact 9)
    (setq I (iadd1 I))
    (go loop)))

(de EvalTest (N)
(prog (I)
    (setq I 0)
loop
    (cond ((igreaterp I N) (return nil)))
    (eval EvalForm)
    (setq I (iadd1 I))
    (go loop)))

(de TimeEval (Form)
(prog (I)
    (setq I (time))
    (eval Form)
    (return (difference (time) I))))

(de topleveltak (x y z) (tak x y z))

(de tak (x y z)
  (cond ((null (ilessp y x))  z)
	(t (tak (tak (isub1 x) y z)
		(tak (isub1 y) z x)
		(tak (isub1 z) x y)))))

(de toplevelgtak (x y z) (gtak x y z))

(de gtak (x y z)
  (cond ((null (lessp y x))  z)
	(t (gtak (gtak (sub1 x) y z)
		(gtak (sub1 y) z x)
		(gtak (sub1 z) x y)))))

(de gtsta (F)
  (prog (I)
    (setq I 1)
Loop
    (cond ((igreaterp I 100000) (return nil)))
    (apply F (list I))
    (setq I (iadd1 I))
    (go Loop)))

(de gtstb (F)
  (prog (I)
    (setq I 1)
Loop
    (cond ((igreaterp I 100000) (return nil)))
    (funcall F I)
    (setq I (iadd1 I))
    (go Loop)))

(de g0 (X) X) 
(de g1 (X) (iadd1 X))

(de nreverse (x)
  (nreconc x nil))

(de nreconc (x y)
 (prog (z)
   L (cond ((atom x) (return y)))
      (setq z x)
      (setq x (cdr x))
      (setq y (rplacd z y))
      (go L)))

(de nnils (N)
  (prog (LST i)
    (setq i 0)
loop
    (cond ((igreaterp i N) (return LST)))
    (setq LST (cons nil LST))
    (setq i (iadd1 i))
    (go loop)))

(global '(TestGlobalVar))

(de nils (N)
  (setq TESTGLOBALVAR (nnils N))
  N)

(de nr ()
  (setq TESTGLOBALVAR (nreverse TESTGLOBALVAR))
  nil)

