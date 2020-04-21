%(!* YPP -- THE PRETTYPRINTER
%
% <BENSON>YPP.SL.19, 17-Sep-82 09:52:42, Edit by BENSON
% Courtesy of IMSSS, with modifications for PSL
%
% PP( LST:list )                        FEXPR
% PRETTYPRINT( X:any )                  EXPR
%
%       Revision History:
%
%	Feb. 23, 1983 Douglas
%		Seperated the testing of specially treated test functions
%		and the printing of these special test functions to 
%		eliminate a recursion problem with special forms in
%		the cdr slot.
%
%	Feb. 10, 1983 Douglas Lanam
%	  Fixed a bug where special list structures in the cdr position
%	  were not handled correctly.
%	  Also removed calls to the function "add" since this is not
%	  a basic psl function.  Replaced them with "plus".
%
%	Feb. 8, 1983 Douglas Lanam
%	  Fix of many numerous small bugs and some clean up of code.
%
%	Feb. 5, 1983 MLG
%	  Changed the nflatsize1 definition line to correct parens.
%
%       Dec. 14, 1982 Douglas Lanam
%         Fixed bug with sprint-prog and sprint-lamdba, so that it
%         gets the correct left-margin for sub-expression.
%
%       Dec. 13, 1982 Douglas Lanam
%         Removal of old code that put properties on 'de','df','dm',
%         than messed up prettyprint on expressions with that atom
%         in the car of the expression.  Also handles prinlevel, and
%         prinlength.
%         Fix bug with '(quote x y).  Taught system about labels in
%         progs and dos.  Taught system about special forms: do,let,
%         de, df, dm, defmacro, and cond.
%
%       November 1982 Douglas Lanam
%         Rewritten to be more compact, more modular,
%         and handle vectors.
%")

(COMPILETIME
     (FLAG '(WARNING
             PP-VAL
             PP-DEF
             PP-DEF-1
             BROKEN
             GET-GOOD-DEF
             S2PRINT
             sprint-dtpr
             sprint-vector
             sprint-read-macro
             read-macro-internal-sprint
             is-read-macrop
             handle-read-macros
             handle-special-list-structures
             check-if-room-for-and-back-indent
             nflatsize1
             CHRCT
             SPACES-LEFT
             SAFE-PPOS
             POSN1
             POSN2
             PPOS) 'INTERNALFUNCTION))

(compiletime
  (fluid '(prinlength prinlevel sprint-level)))

(setq sprint-level 0)

(DE WARNING (X) (ERRORPRINTF "*** %L" X))

%(!* "Change the system prettyprint function to use this one.")

(DE PRETTYPRINT (X) (PROGN (SPRINT X (posn)) (TERPRI)))

(DM PP (L)
  (LIST 'EVPP (LIST 'QUOTE (CDR L))))

(DE EVPP (L)
  (PROGN (MAPC L (FUNCTION PP1)) (TERPRI) T))

(DE PP1 (EXP)
 (PROG NIL
   (COND ((IDP EXP)
          (PROGN (PP-VAL EXP)
                 (PP-DEF EXP)))
         (T (PROGN (SPRINT EXP 1) (TERPRI))))))

(DE PP-VAL (ID)
 (PROG (VAL)
       (COND ((ATOM (SETQ VAL (ERRORSET ID NIL NIL))) (RETURN NIL)))
       (TERPRI)
       (sprint `(setq ,id ',(car val)) (posn))
       (TERPRI)))

(DE PP-DEF (ID)
  (PROG (DEF TYPE ORIG-DEF)
        (SETQ DEF (GETD ID))
   TEST (COND ((NULL DEF)
               (RETURN (AND ORIG-DEF
                            (WARNING (LIST "Gack. "
                                           ID
                                           " has no unbroken definition.")))))
              ((CODEP (CDR DEF))
               (RETURN (WARNING (LIST "Can't PP compiled definition for"
                                      ID))))
              ((AND (NOT ORIG-DEF) (BROKEN ID))
               (PROGN (WARNING (LIST "Note:"
                                     ID
                                     "is broken or traced."))
                      (SETQ ORIG-DEF DEF)
                      (SETQ DEF
                            (CONS (CAR DEF) (GET-GOOD-DEF ID)))
                      (GO TEST))))
        (SETQ TYPE (CAR DEF))
        (TERPRI)
        (SETQ ORIG-DEF
              (ASSOC TYPE
                     '((EXPR . DE)
                       (MACRO . DM)
                       (FEXPR . DF)
                       (NEXPR . DN))))
        (RETURN (PP-DEF-1 (CDR ORIG-DEF) ID (CDDR DEF)))))

(DE PP-DEF-1 (FN NAME TAIL)
  (sprint (cons fn (cons name tail)) (posn)))

(DE BROKEN (X) (GET X 'TRACE))

(DE GET-GOOD-DEF (X)
 (PROG (XX)
       (COND ((AND (SETQ XX (GET X 'TRACE))
                   (SETQ XX (ASSOC 'ORIGINALFN XX)))
              (RETURN (CDR XX))))))

%(!* "S2PRINT: prin2 a string and then sprint an expression.")

(DE S2PRINT (S EXP)
 (PROGN
  (OR (GREATERP (SPACES-LEFT) (PLUS (FLATSIZE2 S) (nFLATSIZE EXP)))
      (TERPRI))
  (PRIN2 S)
  (SPRINT EXP (ADD1 (POSN)))))

(de make-room-for (left-margin size flag)
  (cond ((or %flag
             (greaterp (add1 size) (difference 75 (posn)))
             (lessp (add1 (posn)) left-margin))
         (tab left-margin))))

(de is-read-macrop (exp)
  (and (pairp exp) (atom (car exp)) (pairp (cdr exp)) (null (cddr exp))
       (get (car exp) 'printmacro)))

(de read-macro-internal-sprint (read-macro-c a lm1)
  (make-room-for lm1 (plus2 (flatsize2 read-macro-c) (nflatsize a))
                 (or (pairp a) (vectorp a)))
  (princ read-macro-c)
  (internal-sprint a (plus2 (flatsize2 read-macro-c) lm1)))

(de sprint-read-macro (exp left-margin)
  (let ((c (get (car exp) 'printmacro)))
       (read-macro-internal-sprint c (cadr exp) left-margin)))

(de handle-read-macros (exp left-margin)
  (prog (c)
        (cond ((and (pairp exp)
                    (atom (car exp))
                    (pairp (cdr exp))
                    (null (cddr exp))
                    (setq c (get (car exp) 'printmacro)))
               (read-macro-internal-sprint c (cadr exp) left-margin)
               (return t)))))

(dm define-special-sprint-list-structure (x)
  ((lambda (tag test-if-special sprint-function)
	   `(progn (put ',tag 'sprint-test ',test-if-special)
		   (put ',tag 'sprint-function ',sprint-function)))
   (cadr x)
   (caddr x)
   (cadddr x)))

(de handle-special-list-structures (exp left-margin)
  (prog (c test)
        (cond ((and (pairp exp)
                    (atom (car exp)))
	       (setq test (get (car exp) 'sprint-test))
	       (setq c (get (car exp) 'sprint-function))
	       (cond ((and (or (null test)
			       (apply test (list exp)))
			   c)
		      (apply c (list exp left-margin))
		      (return t)))))))

(de handle-special-list-structures-in-cdr-slot (exp left-margin)
  (prog (c test)
        (cond ((and (pairp exp)
                    (atom (car exp)))
	       (setq test (get (car exp) 'sprint-test))
	       (setq c (get (car exp) 'sprint-function))
	       (cond ((and (or (null test)
			       (apply test (list exp)))
			   c)
		      (princ ". ")
		      (apply c (list exp left-margin))
		      (return t)))))))

(define-special-sprint-list-structure lambda sprint-lambda-test sprint-lambda)
(define-special-sprint-list-structure cond sprint-lambda-test sprint-lambda)
(define-special-sprint-list-structure progn sprint-lambda-test sprint-lambda)
(define-special-sprint-list-structure prog1 sprint-lambda-test sprint-lambda)
(define-special-sprint-list-structure let sprint-let-test sprint-lambda)
(define-special-sprint-list-structure defun sprint-defun-test sprint-defun)
(define-special-sprint-list-structure do sprint-do-test sprint-prog)
(define-special-sprint-list-structure prog sprint-prog-test sprint-prog)
(define-special-sprint-list-structure de sprint-defun-test sprint-defun)
(define-special-sprint-list-structure df sprint-defun-test sprint-defun)
(define-special-sprint-list-structure dm sprint-defun-test sprint-defun)
(define-special-sprint-list-structure defmacro sprint-defun-test sprint-defun)

(de sprint-let-test (exp)
  (and (cdr exp)
       (pairp (cdr exp))
       (pairp (cadr exp))))

(de sprint-do-test (exp)
  (and (cdr exp)
       (pairp (cdr exp))
       (pairp (cadr exp))
       (cddr exp)
       (pairp (cddr exp))
       (pairp (caddr exp))))

(de sprint-defun-test (exp)
  (and (cdr exp)
       (pairp (cdr exp))
       (cddr exp)
       (pairp (cddr exp))))

(de sprint-defun (exp left-margin)
  (make-room-for left-margin (nflatsize exp) nil)
  (princ "(") %)
  (let ((a (plus2 1 (posn))))
       (princ (car exp)) (princ " ")
       (princ (cadr exp)) (princ " ")
       (internal-sprint (caddr exp) a)
       (do ((i (cdddr exp) (cdr i)))
	   ((null i)  %(
			(princ ")"))
	   (tab a)
	   (cond ((atom i)
		  (princ ". ") (internal-sprint i (plus2 2 a) )
		  %(
		    (princ ")")
		  (return nil))
		 ((is-read-macrop i)
		  (make-room-for a (plus2 2 (nflatsize i)) nil)
		  (princ ". ")
		  (sprint-read-macro i a)
		  %(
		    (princ ")")
		  (return nil))
		 (t (internal-sprint (car i) a))))))

(de sprint-prog-test (exp)
  (and (cdr exp)
       (pairp (cdr exp))
       (cddr exp)))

(de sprint-prog (exp left-margin)
  (make-room-for left-margin (nflatsize exp) nil)
  (princ "(") %)
  (let ((b (posn))
	(a (plus2 1 (plus2 (posn) (flatsize (car exp))))))
       (princ (car exp)) (princ " ")
       (internal-sprint (cadr exp) a)
       (do ((i (cddr exp) (cdr i)))
	   ((null i)  %(
			(princ ")"))
	   (tab b)
	   (cond ((atom i)
		  (princ ". ") (internal-sprint i (plus2 2 a) )
		  %(
		    (princ ")")
		  (return nil))
		 ((is-read-macrop i)
		  (make-room-for a (plus2 2 (nflatsize i)) nil)
		  (princ ". ")
		  (sprint-read-macro i a)
		  %(
		    (princ ")")
		  (return nil))
		 ((atom (car i))
		  (internal-sprint (car i) b))
		 (t (internal-sprint (car i) a))))))

(de sprint-lambda-test (exp)
  (and (cdr exp)
       (pairp (cdr exp))))

(de sprint-lambda (exp left-margin)
  (make-room-for left-margin (nflatsize exp) nil)
  (princ "(") %)
  (princ (car exp)) (princ " ")
  (let ((a (posn)))
       (internal-sprint (cadr exp) a)
       (do ((i (cddr exp) (cdr i)))
	   ((null i)  %(
			(princ ")"))
	   (tab a)
	   (cond ((atom i)
		  (princ ". ") (internal-sprint i (plus2 2 a) )
		  %(
		    (princ ")")
		  (return nil))
		 ((is-read-macrop i)
		  (make-room-for a (plus2 2 (nflatsize i)) nil)
		  (princ ". ")
		  (sprint-read-macro i a)
		  %(
		    (princ ")")
		  (return nil))
		 (t (internal-sprint (car i) a))))))

(de depth-greater-than-n (l n)
  (cond ((weq n 0) t)
	((pairp l)
	 (do ((i l (cdr i)))
	     ((null i))
	     (cond ((atom i) (return nil))
		   ((and (pairp i)
			 (depth-greater-than-n (car i) (sub1 n)))
		    (return t)))))))

(de sprint-dtpr2 (exp left-margin)
  (make-room-for left-margin (nflatsize exp) nil)
  (prog (lm)
        (princ "(") %)
        (setq lm (plus2 1 (cond ((and (atom (car exp))
                                      (null (vectorp (car exp)))
                                      (lessp (plus2 (posn)
                                                    (nflatsize
                                                     (car exp)))
                                             40)
				      (null (depth-greater-than-n exp 13)))
                                 (plus2 1 (plus2 left-margin
                                                 (nflatsize
                                                  (car exp)))))
                                (t left-margin))))
        (do ((a exp (cdr a))
             (i 1 (add1 i))
             (l (add1 left-margin) lm))
            ((null a)   % (
                           (princ ")"))
            (cond ((and (numberp prinlength)
                        (greaterp i prinlength))
                   % (
                      (princ "...)")
                   (return nil)))
            (cond ((atom a) 
                   (make-room-for l (plus2 2 (nflatsize a)) nil)
                   (princ ". ") (internal-sprint a l) 
                   %(
                     (princ ")")
                   (return nil))
                  ((is-read-macrop a)
                   (princ ". ")
                   (sprint-read-macro a (plus2 l 2))
                   %(
                     (princ ")")
                   (return nil))
		  ((handle-special-list-structures-in-cdr-slot a left-margin)
		   %(
		     (princ ")")
		   (return nil))
                  (t (internal-sprint (car a) l)))
            (cond ((cdr a) 
                   (cond ((greaterp (nflatsize (car a))
                                    (difference 75 l))
                          (tab l))
                         (t (princ " ")))
                   )))))

(de sprint-dtpr (exp left-margin)
  ((lambda
    (sprint-level)
    (cond ((and (numberp prinlevel)
                (greaterp sprint-level prinlevel))
           (princ "#"))
          ((handle-read-macros exp left-margin))
          ((handle-special-list-structures exp left-margin))
          (t (sprint-dtpr2 exp left-margin))))
   (add1 sprint-level)))

(de sprint-vector (vector left-margin)
  ((lambda
    (sprint-level)
    (cond ((and (Numberp prinlevel)
                (greaterp sprint-level prinlevel))
           (princ "#"))
          (t
           (prog (c)
                 (princ "[")
                 (let ((lm (add1 left-margin)))
                      (do ((i 0 (1+ i))
                           (size (size vector)))
                          ((greaterp i size) (princ "]"))
                          (cond ((and (numberp prinlength)
                                      (greaterp i prinlength))
                                 (princ "...]")
                                 (return nil)))
                          (internal-sprint (getv vector i) lm)
                          (cond ((lessp i size)
                                 (cond ((greaterp (nflatsize (getv vector 
								   (plus2 i 1)))
                                                  (difference 75 lm))
                                        (tab lm))
				       ((lessp (posn) lm)
					(tab lm))
                                       (t (princ " ")))))))))))
   (add1 sprint-level)))

(de check-if-room-for-and-back-indent (a lm)
  (cond ((and (atom a)
              (null (vectorp a))
              (greaterp (add1 (nflatsize a)) (difference (linelength nil) lm))
              (null (lessp (posn) 2)))
         (terpri)
         (cond ((eq (getv lispscantable* (id2int '!%)) 12)
                (princ "%"))
               ((eq (getv lispscantable* (id2int '!;)) 12)
                (princ ";"))
               (t (princ "%")))
         (princ "**** <<<<<<  Reindenting.")
         (terpri)
         lm)))

(de internal-sprint (a lm)
  (let ((indent (check-if-room-for-and-back-indent a lm)))
       (cond ((lessp (posn) lm)
	      (tab lm)))
       (cond ((handle-read-macros a lm))
             ((handle-special-list-structures a lm))
             (t (make-room-for lm (nflatsize a) 
                               (or (pairp a) (vectorp a)))
                (cond ((pairp a) (sprint-dtpr a (posn)))
                      ((vectorp a) (sprint-vector a (posn)))
		      (t (and (lessp (posn) lm)
			      (tab lm))
			 (prin1 a)))))
       (cond (indent
              (terpri)
              (cond ((eq (getv lispscantable* (id2int '!%)) 12)
                     (princ "%"))
                    ((eq (getv lispscantable* (id2int '!;)) 12)
                     (princ ";"))
                    (t (princ "%")))
              (princ "**** >>>>> Reindenting.")
              (terpri)))))

(de sprint (exp left-margin)
  (let ((a (posn))
        (sprint-level 0)
        (b (linelength nil)))
       (linelength 600)
       (cond ((eq a left-margin))
             (t (tab left-margin)))
       (internal-sprint exp left-margin)
       (linelength b)
       nil))

(PUT 'QUOTE 'PRINTMACRO "'")
(PUT 'BACKQUOTE 'PRINTMACRO "`")
(PUT 'UNQUOTE 'PRINTMACRO ",")
(PUT 'UNQUOTEL 'PRINTMACRO ",@")
(PUT 'UNQUOTED 'PRINTMACRO ",.")

(DE PM-DEF (FORM)
  (PP-DEF-1 (CAR FORM) (CADR FORM) (CDDR FORM)))

(DE CHRCT NIL (DIFFERENCE (MIN 80 (LINELENGTH NIL)) (POSN)))

(DE SPACES-LEFT NIL (SUB1 (CHRCT)))

(DE SAFE-PPOS (N SIZE)
 (PROG (MIN-N)
       (SETQ MIN-N (SUB1 (DIFFERENCE (LINELENGTH NIL) SIZE)))
       (COND ((LESSP MIN-N N)
              (PROGN (OR (GREATERP MIN-N (POSN1)) (TERPRI)) (PPOS MIN-N)))
             (T (PPOS N)))))

(DE POSN1 NIL (ADD1 (POSN)))

(DE POSN2 NIL (PLUS 2 (POSN)))

(DE PPOS (N)
 (PROG NIL
       (OR (GREATERP N (POSN)) (TERPRI))
       (SETQ N (SUB1 N))
  LOOP (COND ((LESSP (POSN) N) (PROGN (PRIN2 " ") (GO LOOP))))))

(de nflatsize (n) (nflatsize1 n sprint-level))

(de nflatsize1 (n currentlevel)
  (cond ((and (numberp prinlevel)
              (wgreaterp currentlevel prinlevel)) 1)
        ((vectorp n)
         (do ((i (size n) (sub1 i))
              (s (iplus2 1 (size n))
                 (iplus2 1 (iplus2 s 
                                   (nflatsize1 (getv n i)
                                               (iplus2 1 currentlevel))))))
             ((wlessp i 0) s)))
        ((atom n) (flatsize n))
        ((is-read-macrop n)
         (let ((c (get (car n) 'printmacro)))
              (iplus2 (flatsize2 c) 
                      (nflatsize1 (cadr n) (iplus2 1 currentlevel)))))
        ((do ((i n (cdr i))
              (s 1 (iplus2 (nflatsize1 (car i) (iplus2 1 currentlevel))
                           (iplus2 1 s))))
             ((null i) s)
             (cond ((atom i)
                    (return (iplus2 3 (iplus2 s (nflatsize1
                                                 i (iplus2 1 currentlevel))))))
                   ((is-read-macrop i)
                    (return (iplus2 3 (iplus2 s (nflatsize1
                                                 i (iplus2 1 currentlevel))))))
                   )))))
        
