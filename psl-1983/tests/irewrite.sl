
% {DSK}IREWRITE.PSL;2  6-JAN-83 10:08:06 
(FLUID '(unify-subst))
(FLAG '(
ADD-LEMMA
ADD-LEMMA-LST
Apply-subst
Apply-subst-lst
false
one-way-unify
one-way-unify1
one-way-unify1-lst
ptime
rewrite
rewrite-with-lemmas
tautologyP
tautp
trans-of-implies
trans-of-implies1
truep

) 'InternalFunction)


(DE ADD-LEMMA (TERM)
(COND ((AND (NOT (ATOM TERM))
	    (EQ (CAR TERM)
		'EQUAL)
	    (NOT (ATOM (CADR TERM))))
       (PUT (CAR (CADR TERM))
	    'LEMMAS
	    (CONS TERM (GET (CAR (CADR TERM))
			    'LEMMAS))))
      (T (ERROR 0 (LIST 'ADD-LEMMA-DID-NOT-LIKE-TERM
			TERM)))))


(DE ADD-LEMMA-LST (LST)
(COND ((NULL LST)
       T)
      (T (ADD-LEMMA (CAR LST))
	 (ADD-LEMMA-LST (CDR LST)))))


% lmm  7-JUN-81 10:07 
(DE APPLY-SUBST (ALIST TERM)
(COND ((NOT (PAIRP TERM))
       ((LAMBDA (TEM)
	  (COND
	    (TEM (CDR TEM))
	    (T TERM)))
	(ASSOC TERM ALIST)))
      (T (CONS (CAR TERM)
	       (MAPCAR (CDR TERM)
		       (FUNCTION (LAMBDA (X)
				   (APPLY-SUBST ALIST X))))))))


(DE APPLY-SUBST-LST (ALIST LST)
(COND ((NULL LST)
       NIL)
      (T (CONS (APPLY-SUBST ALIST (CAR LST))
	       (APPLY-SUBST-LST ALIST (CDR LST))))))


(DE FALSEP (X LST)
(OR (EQUAL X '(F))
    (MEMBER X LST)))


(DE ONE-WAY-UNIFY (TERM1 TERM2)
(PROGN (SETQ UNIFY-SUBST NIL)
       (ONE-WAY-UNIFY1 TERM1 TERM2)))


% lmm  7-JUN-81 09:47 
(DE ONE-WAY-UNIFY1 (TERM1 TERM2)
(COND ((NOT (PAIRP TERM2))
       ((LAMBDA (TEM)
	  (COND
	    (TEM (EQUAL TERM1 (CDR TEM)))
	    (T (SETQ UNIFY-SUBST (CONS (CONS TERM2 TERM1)
				       UNIFY-SUBST))
	       T)))
	(ASSOC TERM2 UNIFY-SUBST)))
      ((NOT (PAIRP TERM1))
       NIL)
      ((EQ (CAR TERM1)
	   (CAR TERM2))
       (ONE-WAY-UNIFY1-LST (CDR TERM1)
			   (CDR TERM2)))
      (T NIL)))


(DE ONE-WAY-UNIFY1-LST (LST1 LST2)
(COND ((NULL LST1)
       T)
      ((ONE-WAY-UNIFY1 (CAR LST1)
		       (CAR LST2))
       (ONE-WAY-UNIFY1-LST (CDR LST1)
			   (CDR LST2)))
      (T NIL)))


(DE PTIME NIL
(PROG (GCTM)
      (SETQ GCTM 0)
      (RETURN (CONS (time)
		    GCTM))))


% lmm  7-JUN-81 10:04 
(DE REWRITE (TERM)
(COND ((NOT (PAIRP TERM))
       TERM)
      (T (REWRITE-WITH-LEMMAS (CONS (CAR TERM)
				    (MAPCAR (CDR TERM)
					    (FUNCTION REWRITE)))
			      (GET (CAR TERM)
				   'LEMMAS)))))


(DE REWRITE-WITH-LEMMAS (TERM LST)
(COND ((NULL LST)
       TERM)
      ((ONE-WAY-UNIFY TERM (CADR (CAR LST)))
       (REWRITE (APPLY-SUBST UNIFY-SUBST (CADDR (CAR LST)))))
      (T (REWRITE-WITH-LEMMAS TERM (CDR LST)))))


(DE SETUP NIL
(ADD-LEMMA-LST
  '((EQUAL (COMPILE FORM)
	   (REVERSE (CODEGEN (OPTIMIZE FORM)
			     (NIL))))
    (EQUAL (EQP X Y)
	   (EQUAL (FIX X)
		  (FIX Y)))
    (EQUAL (GREATERP X Y)
	   (LESSP Y X))
    (EQUAL (LESSEQP X Y)
	   (NOT (LESSP Y X)))
    (EQUAL (GREATEREQP X Y)
	   (NOT (LESSP X Y)))
    (EQUAL (BOOLEAN X)
	   (OR (EQUAL X (T))
	       (EQUAL X (F))))
    (EQUAL (IFF X Y)
	   (AND (IMPLIES X Y)
		(IMPLIES Y X)))
    (EQUAL (EVEN1 X)
	   (IF (ZEROP X)
	       (T)
	       (ODD (SUB1 X))))
    (EQUAL (COUNTPS- L PRED)
	   (COUNTPS-LOOP L PRED (ZERO)))
    (EQUAL (FACT- I)
	   (FACT-LOOP I 1))
    (EQUAL (REVERSE- X)
	   (REVERSE-LOOP X (NIL)))
    (EQUAL (DIVIDES X Y)
	   (ZEROP (REMAINDER Y X)))
    (EQUAL (ASSUME-TRUE VAR ALIST)
	   (CONS (CONS VAR (T))
		 ALIST))
    (EQUAL (ASSUME-FALSE VAR ALIST)
	   (CONS (CONS VAR (F))
		 ALIST))
    (EQUAL (TAUTOLOGY-CHECKER X)
	   (TAUTOLOGYP (NORMALIZE X)
		       (NIL)))
    (EQUAL (FALSIFY X)
	   (FALSIFY1 (NORMALIZE X)
		     (NIL)))
    (EQUAL (PRIME X)
	   (AND (NOT (ZEROP X))
		(NOT (EQUAL X (ADD1 (ZERO))))
		(PRIME1 X (SUB1 X))))
    (EQUAL (AND P Q)
	   (IF P (IF Q (T)
		     (F))
	       (F)))
    (EQUAL (OR P Q)
	   (IF P (T)
	       (IF Q (T)
		   (F))
	       (F)))
    (EQUAL (NOT P)
	   (IF P (F)
	       (T)))
    (EQUAL (IMPLIES P Q)
	   (IF P (IF Q (T)
		     (F))
	       (T)))
    (EQUAL (FIX X)
	   (IF (NUMBERP X)
	       X
	       (ZERO)))
    (EQUAL (IF (IF A B C)
	       D E)
	   (IF A (IF B D E)
	       (IF C D E)))
    (EQUAL (ZEROP X)
	   (OR (EQUAL X (ZERO))
	       (NOT (NUMBERP X))))
    (EQUAL (PLUS (PLUS X Y)
		 Z)
	   (PLUS X (PLUS Y Z)))
    (EQUAL (EQUAL (PLUS A B)
		  (ZERO))
	   (AND (ZEROP A)
		(ZEROP B)))
    (EQUAL (DIFFERENCE X X)
	   (ZERO))
    (EQUAL (EQUAL (PLUS A B)
		  (PLUS A C))
	   (EQUAL (FIX B)
		  (FIX C)))
    (EQUAL (EQUAL (ZERO)
		  (DIFFERENCE X Y))
	   (NOT (LESSP Y X)))
    (EQUAL (EQUAL X (DIFFERENCE X Y))
	   (AND (NUMBERP X)
		(OR (EQUAL X (ZERO))
		    (ZEROP Y))))
    (EQUAL (MEANING (PLUS-TREE (APPEND X Y))
		    A)
	   (PLUS (MEANING (PLUS-TREE X)
			  A)
		 (MEANING (PLUS-TREE Y)
			  A)))
    (EQUAL (MEANING (PLUS-TREE (PLUS-FRINGE X))
		    A)
	   (FIX (MEANING X A)))
    (EQUAL (APPEND (APPEND X Y)
		   Z)
	   (APPEND X (APPEND Y Z)))
    (EQUAL (REVERSE (APPEND A B))
	   (APPEND (REVERSE B)
		   (REVERSE A)))
    (EQUAL (TIMES X (PLUS Y Z))
	   (PLUS (TIMES X Y)
		 (TIMES X Z)))
    (EQUAL (TIMES (TIMES X Y)
		  Z)
	   (TIMES X (TIMES Y Z)))
    (EQUAL (EQUAL (TIMES X Y)
		  (ZERO))
	   (OR (ZEROP X)
	       (ZEROP Y)))
    (EQUAL (EXEC (APPEND X Y)
		 PDS ENVRN)
	   (EXEC Y (EXEC X PDS ENVRN)
		 ENVRN))
    (EQUAL (MC-FLATTEN X Y)
	   (APPEND (FLATTEN X)
		   Y))
    (EQUAL (MEMBER X (APPEND A B))
	   (OR (MEMBER X A)
	       (MEMBER X B)))
    (EQUAL (MEMBER X (REVERSE Y))
	   (MEMBER X Y))
    (EQUAL (LENGTH (REVERSE X))
	   (LENGTH X))
    (EQUAL (MEMBER A (INTERSECT B C))
	   (AND (MEMBER A B)
		(MEMBER A C)))
    (EQUAL (NTH (ZERO)
		I)
	   (ZERO))
    (EQUAL (EXP I (PLUS J K))
	   (TIMES (EXP I J)
		  (EXP I K)))
    (EQUAL (EXP I (TIMES J K))
	   (EXP (EXP I J)
		K))
    (EQUAL (REVERSE-LOOP X Y)
	   (APPEND (REVERSE X)
		   Y))
    (EQUAL (REVERSE-LOOP X (NIL))
	   (REVERSE X))
    (EQUAL (COUNT-LIST Z (SORT-LP X Y))
	   (PLUS (COUNT-LIST Z X)
		 (COUNT-LIST Z Y)))
    (EQUAL (EQUAL (APPEND A B)
		  (APPEND A C))
	   (EQUAL B C))
    (EQUAL (PLUS (REMAINDER X Y)
		 (TIMES Y (QUOTIENT X Y)))
	   (FIX X))
    (EQUAL (POWER-EVAL (BIG-PLUS1 L I BASE)
		       BASE)
	   (PLUS (POWER-EVAL L BASE)
		 I))
    (EQUAL (POWER-EVAL (BIG-PLUS X Y I BASE)
		       BASE)
	   (PLUS I (PLUS (POWER-EVAL X BASE)
			 (POWER-EVAL Y BASE))))
    (EQUAL (REMAINDER Y 1)
	   (ZERO))
    (EQUAL (LESSP (REMAINDER X Y)
		  Y)
	   (NOT (ZEROP Y)))
    (EQUAL (REMAINDER X X)
	   (ZERO))
    (EQUAL (LESSP (QUOTIENT I J)
		  I)
	   (AND (NOT (ZEROP I))
		(OR (ZEROP J)
		    (NOT (EQUAL J 1)))))
    (EQUAL (LESSP (REMAINDER X Y)
		  X)
	   (AND (NOT (ZEROP Y))
		(NOT (ZEROP X))
		(NOT (LESSP X Y))))
    (EQUAL (POWER-EVAL (POWER-REP I BASE)
		       BASE)
	   (FIX I))
    (EQUAL (POWER-EVAL (BIG-PLUS (POWER-REP I BASE)
				 (POWER-REP J BASE)
				 (ZERO)
				 BASE)
		       BASE)
	   (PLUS I J))
    (EQUAL (GCD X Y)
	   (GCD Y X))
    (EQUAL (NTH (APPEND A B)
		I)
	   (APPEND (NTH A I)
		   (NTH B (DIFFERENCE I (LENGTH A)))))
    (EQUAL (DIFFERENCE (PLUS X Y)
		       X)
	   (FIX Y))
    (EQUAL (DIFFERENCE (PLUS Y X)
		       X)
	   (FIX Y))
    (EQUAL (DIFFERENCE (PLUS X Y)
		       (PLUS X Z))
	   (DIFFERENCE Y Z))
    (EQUAL (TIMES X (DIFFERENCE C W))
	   (DIFFERENCE (TIMES C X)
		       (TIMES W X)))
    (EQUAL (REMAINDER (TIMES X Z)
		      Z)
	   (ZERO))
    (EQUAL (DIFFERENCE (PLUS B (PLUS A C))
		       A)
	   (PLUS B C))
    (EQUAL (DIFFERENCE (ADD1 (PLUS Y Z))
		       Z)
	   (ADD1 Y))
    (EQUAL (LESSP (PLUS X Y)
		  (PLUS X Z))
	   (LESSP Y Z))
    (EQUAL (LESSP (TIMES X Z)
		  (TIMES Y Z))
	   (AND (NOT (ZEROP Z))
		(LESSP X Y)))
    (EQUAL (LESSP Y (PLUS X Y))
	   (NOT (ZEROP X)))
    (EQUAL (GCD (TIMES X Z)
		(TIMES Y Z))
	   (TIMES Z (GCD X Y)))
    (EQUAL (VALUE (NORMALIZE X)
		  A)
	   (VALUE X A))
    (EQUAL (EQUAL (FLATTEN X)
		  (CONS Y (NIL)))
	   (AND (NLISTP X)
		(EQUAL X Y)))
    (EQUAL (LISTP (GOPHER X))
	   (LISTP X))
    (EQUAL (SAMEFRINGE X Y)
	   (EQUAL (FLATTEN X)
		  (FLATTEN Y)))
    (EQUAL (EQUAL (GREATEST-FACTOR X Y)
		  (ZERO))
	   (AND (OR (ZEROP Y)
		    (EQUAL Y 1))
		(EQUAL X (ZERO))))
    (EQUAL (EQUAL (GREATEST-FACTOR X Y)
		  1)
	   (EQUAL X 1))
    (EQUAL (NUMBERP (GREATEST-FACTOR X Y))
	   (NOT (AND (OR (ZEROP Y)
			 (EQUAL Y 1))
		     (NOT (NUMBERP X)))))
    (EQUAL (TIMES-LIST (APPEND X Y))
	   (TIMES (TIMES-LIST X)
		  (TIMES-LIST Y)))
    (EQUAL (PRIME-LIST (APPEND X Y))
	   (AND (PRIME-LIST X)
		(PRIME-LIST Y)))
    (EQUAL (EQUAL Z (TIMES W Z))
	   (AND (NUMBERP Z)
		(OR (EQUAL Z (ZERO))
		    (EQUAL W 1))))
    (EQUAL (GREATEREQPR X Y)
	   (NOT (LESSP X Y)))
    (EQUAL (EQUAL X (TIMES X Y))
	   (OR (EQUAL X (ZERO))
	       (AND (NUMBERP X)
		    (EQUAL Y 1))))
    (EQUAL (REMAINDER (TIMES Y X)
		      Y)
	   (ZERO))
    (EQUAL (EQUAL (TIMES A B)
		  1)
	   (AND (NOT (EQUAL A (ZERO)))
		(NOT (EQUAL B (ZERO)))
		(NUMBERP A)
		(NUMBERP B)
		(EQUAL (SUB1 A)
		       (ZERO))
		(EQUAL (SUB1 B)
		       (ZERO))))
    (EQUAL (LESSP (LENGTH (DELETE X L))
		  (LENGTH L))
	   (MEMBER X L))
    (EQUAL (SORT2 (DELETE X L))
	   (DELETE X (SORT2 L)))
    (EQUAL (DSORT X)
	   (SORT2 X))
    (EQUAL (LENGTH (CONS X1 (CONS X2 (CONS X3 (CONS X4
						    (CONS X5 (CONS X6 X7)))))))
	   (PLUS 6 (LENGTH X7)))
    (EQUAL (DIFFERENCE (ADD1 (ADD1 X))
		       2)
	   (FIX X))
    (EQUAL (QUOTIENT (PLUS X (PLUS X Y))
		     2)
	   (PLUS X (QUOTIENT Y 2)))
    (EQUAL (SIGMA (ZERO)
		  I)
	   (QUOTIENT (TIMES I (ADD1 I))
		     2))
    (EQUAL (PLUS X (ADD1 Y))
	   (IF (NUMBERP Y)
	       (ADD1 (PLUS X Y))
	       (ADD1 X)))
    (EQUAL (EQUAL (DIFFERENCE X Y)
		  (DIFFERENCE Z Y))
	   (IF (LESSP X Y)
	       (NOT (LESSP Y Z))
	       (IF (LESSP Z Y)
		   (NOT (LESSP Y X))
		   (EQUAL (FIX X)
			  (FIX Z)))))
    (EQUAL (MEANING (PLUS-TREE (DELETE X Y))
		    A)
	   (IF (MEMBER X Y)
	       (DIFFERENCE (MEANING (PLUS-TREE Y)
				    A)
			   (MEANING X A))
	       (MEANING (PLUS-TREE Y)
			A)))
    (EQUAL (TIMES X (ADD1 Y))
	   (IF (NUMBERP Y)
	       (PLUS X (TIMES X Y))
	       (FIX X)))
    (EQUAL (NTH (NIL)
		I)
	   (IF (ZEROP I)
	       (NIL)
	       (ZERO)))
    (EQUAL (LAST (APPEND A B))
	   (IF (LISTP B)
	       (LAST B)
	       (IF (LISTP A)
		   (CONS (CAR (LAST A))
			 B)
		   B)))
    (EQUAL (EQUAL (LESSP X Y)
		  Z)
	   (IF (LESSP X Y)
	       (EQUAL T Z)
	       (EQUAL F Z)))
    (EQUAL (ASSIGNMENT X (APPEND A B))
	   (IF (ASSIGNEDP X A)
	       (ASSIGNMENT X A)
	       (ASSIGNMENT X B)))
    (EQUAL (CAR (GOPHER X))
	   (IF (LISTP X)
	       (CAR (FLATTEN X))
	       (ZERO)))
    (EQUAL (FLATTEN (CDR (GOPHER X)))
	   (IF (LISTP X)
	       (CDR (FLATTEN X))
	       (CONS (ZERO)
		     (NIL))))
    (EQUAL (QUOTIENT (TIMES Y X)
		     Y)
	   (IF (ZEROP Y)
	       (ZERO)
	       (FIX X)))
    (EQUAL (GET J (SET I VAL MEM))
	   (IF (EQP J I)
	       VAL
	       (GET J MEM))))))


% lmm  7-JUN-81 09:44 
(DE TAUTOLOGYP (X TRUE-LST FALSE-LST)
(COND ((TRUEP X TRUE-LST)
       T)
      ((FALSEP X FALSE-LST)
       NIL)
      ((NOT (PAIRP X))
       NIL)
      ((EQ (CAR X)
	   'IF)
       (COND ((TRUEP (CADR X)
		     TRUE-LST)
	      (TAUTOLOGYP (CADDR X)
			  TRUE-LST FALSE-LST))
	     ((FALSEP (CADR X)
		      FALSE-LST)
	      (TAUTOLOGYP (CADDDR X)
			  TRUE-LST FALSE-LST))
	     (T (AND (TAUTOLOGYP (CADDR X)
				 (CONS (CADR X)
				       TRUE-LST)
				 FALSE-LST)
		     (TAUTOLOGYP (CADDDR X)
				 TRUE-LST
				 (CONS (CADR X)
				       FALSE-LST))))))
      (T NIL)))


(DE TAUTP (X)
(TAUTOLOGYP (REWRITE X)
	    NIL NIL))


(DE TEST NIL
(PROG (TM1 TM2 ANS TERM)
      (SETQ TM1 (PTIME))
      (SETQ TERM (APPLY-SUBST '((X F (PLUS (PLUS A B)
					   (PLUS C (ZERO))))
				(Y F (TIMES (TIMES A B)
					    (PLUS C D)))
				(Z F (REVERSE (APPEND (APPEND A B)
						      (NIL))))
				(U EQUAL (PLUS A B)
				   (DIFFERENCE X Y))
				(W LESSP (REMAINDER A B)
				   (MEMBER A (LENGTH B))))
			      '(IMPLIES (AND (IMPLIES X Y)
					     (AND (IMPLIES Y Z)
						  (AND (IMPLIES Z U)
						       (IMPLIES U W))))
					(IMPLIES X W))))
      (SETQ ANS (TAUTP TERM))
      (SETQ TM2 (PTIME))
      (RETURN (LIST ANS (DIFFERENCE (CAR TM2)
				    (CAR TM1))
		    (DIFFERENCE (CDR TM2)
				(CDR TM1))))))


(DE TRANS-OF-IMPLIES (N)
(LIST 'IMPLIES
      (TRANS-OF-IMPLIES1 N)
      (LIST 'IMPLIES
	    0 N)))


(DE TRANS-OF-IMPLIES1 (N)
(COND ((EQUAL N 1)
       (LIST 'IMPLIES
	     0 1))
      (T (LIST 'AND
	       (LIST 'IMPLIES
		     (SUB1 N)
		     N)
	       (TRANS-OF-IMPLIES1 (SUB1 N))))))


(DE TRUEP (X LST)
(OR (EQUAL X '(T))
    (MEMBER X LST)))
