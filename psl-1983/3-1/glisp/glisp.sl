%
%  GLHEAD.PSL.13               16 FEB. 1983
%
%  HEADER FOR GLISP FOR COMPATIBILITY WITH PORTABLE STANDARD LISP (PSL)
%  G. NOVAK     20 OCTOBER 1982
%


(GLOBAL '(GLQUIETFLG GLSEPBITTBL GLUNITPKGS GLSEPMINUS GLOBJECTNAMES
          GLTYPENAMES GLBREAKONERROR GLUSERSTRNAMES GLLASTFNCOMPILED
          GLLASTSTREDITED GLCAUTIOUSFLG GLLISPDIALECT GLBASICTYPES
          GLOBJECTTYPES GLTYPESUSED))

(FLUID '(TTLIST SPECS SOURCE GLGLOBALVARS DOMAINNAME ARGTYPES NOTFLG
            GLAMBDAFN ADDISATYPE PAIRLIST PROGG BITTBL KEY Y TYPES
            CONTEXT EXPR VALBUSY FAULTFN GLSEPATOM GLSEPPTR *GL* *GLVAL*
            GLTOPCTX RESULTTYPE RESULT GLNATOM FIRST OPNDS OPERS
            GLEXPR DESLIST EXPRSTACK GLTYPESUBS GLPROGLST
            TYPE GLNRECURSIONS GLFNSUBS GLEVALSUBS))

%  CASEQ MACRO FOR PSL
(DM CASEQ (L)
  (PROG (CVAR CODE)
    (SETQ CVAR (COND ((ATOM (CADR L))(CADR L))
                     (T 'CASEQSELECTORVAR)))
    (SETQ CODE (CONS 'COND (MAPCAR (CDDR L) 
		       (FUNCTION (LAMBDA (X)
        (COND ((EQ (CAR X) T) X)
              ((ATOM (CAR X))
	       (CONS (LIST 'EQ CVAR
                           (LIST 'QUOTE (CAR X)))
                     (CDR X)))
	      (T (CONS (LIST 'MEMQ CVAR
			     (LIST 'QUOTE (CAR X)))
		       (CDR X)))))))))
    (RETURN (COND ((ATOM (CADR L)) CODE)
		  (T (LIST 'PROG (LIST CVAR)
			   (LIST 'SETQ CVAR (CADR L))
			   (LIST 'RETURN CODE)))))))


%
%  GLTAIL.PSL.4               18 Feb. 1983
%
%  FILE OF FUNCTIONS FOR COMPATIBILITY WITH PORTABLE STANDARD LISP (PSL)
%  G. NOVAK     20 OCTOBER 1982
%


(DE GETDDD (X)
  (COND ((PAIRP (GETD X)) (CDR (GETD X)))
        (T NIL)))

(DE PUTDDD (FN DEF) (REMD FN) (PUTD FN 'EXPR DEF))


(DE LISTGET (L PROP)
  (COND ((NOT (PAIRP L)) NIL)
        ((EQ (CAR L) PROP) (CADR L))
        (T (LISTGET (CDDR L) PROP) )) )



%  NOTE -- THIS VERSION OF NLEFT ONLY WORKS FOR N=2.
(DE NLEFT (L N)
  (COND ((NOT (EQN N 2)) (ERROR 0 N))
        ((NULL L) NIL)
        ((NULL (CDDR L)) L)
        (T (NLEFT (CDR L) N) )) )


(DE NLISTP (X) (NOT (PAIRP X)))
(DF COMMENT (X) NIL)


%  ASSUME EVERYTHING UPPER-CASE FOR PSL.
(DE U-CASEP (X) T)
(de glucase (x) x)


%  PARTIAL IMPLEMENTATION OF SUBATOM FOR POSITIVE NUMBERS.
(DE SUBATOM (ATM N M)
 (PROG (LST SZ)
  (setq sz (flatsize2 atm))
  (cond ((minusp n) (setq n (add1 (plus sz n)))))
  (cond ((minusp m) (setq m (add1 (plus sz m)))))
  (COND ((GREATERP M sz)(RETURN NIL)))
A (COND ((GREATERP N M)(RETURN (AND LST (IMPLODE (REVERSIP LST))))))
  (SETQ LST (CONS (GLNTHCHAR ATM N) LST))
  (COND ((MEMQ (CAR LST) '(!' !, !!))
          (RPLACD LST (CONS (QUOTE !!) (CDR LST))) ))
  (SETQ N (ADD1 N))
  (GO A) ))


%  FIND THE STRING POSITION IN ATOM ATM WHERE A CHARACTER IN THE
%  BIT TABLE BITTBL OCCURS, STARTING WITH CHARACTER N.
(DE STRPOSL (BITTBL ATM N)
 (PROG (NC)
  (COND ((NULL N)(SETQ N 1)))
  (SETQ NC (FLATSIZE2 ATM))
A (COND ((GREATERP N NC)(RETURN NIL))
        ((INDX GLSEPBITTBL (id2int (GLNTHCHAR ATM N)))(RETURN N)))
  (SETQ N (ADD1 N))
  (GO A) ))

%  MAKE A BIT TABLE FROM A LIST OF CHARACTERS.
(DE MAKEBITTABLE (L)
 (PROG ()
  (SETQ GLSEPBITTBL (MkVect 255))
  (MAPC L (FUNCTION (LAMBDA (X)
     (PutV GLSEPBITTBL (id2int X) T) )))
  (RETURN GLSEPBITTBL) ))


%  Fexpr for defining GLISP functions.
(df dg (x)
   (put (car x) 'gloriginalexpr (cons 'lambda (cdr x)))
   (glputhook (car x)) )

%  Put the hook macro onto a function to cause auto compilation.
(de glputhook (x)
   (put x 'glcompiled nil)
   (putd x 'macro '(lambda (gldgform)(glhook gldgform))) )

%  Hook for compiling a GLISP function on its first call.
(de glhook (gldgform) (glcc (car gldgform)) gldgform)

%  Interlisp-style NTHCHAR.
(de glnthchar (x n)
  (prog (s l)
    (setq s (id2string x))
    (setq l (size s))
    (cond ((minusp n)(setq n (add1 (plus l n))))
          (t (setq n (sub1 n))))
    (cond ((or (minusp n)(greaterp n l))(return nil)))
    (return (int2id (indx s n)))))


%  FIND FIRST ELEMENT OF A LIST FOR WHICH FN IS TRUE
(DE SOME (L FN)
  (COND ((NULL L) NIL)
        ((APPLY FN (LIST (CAR L))) L)
        (T (SOME (CDR L) FN))))

%  TEST IF FN IS TRUE FOR EVERY ELEMENT OF A LIST
%  SOME and EVERY switched FN and L
(DE EVERY (L FN)
  (COND ((NULL L) T)
        ((APPLY FN (LIST (CAR L))) (EVERY (CDR L) FN))
        (T NIL)))

%  SUBSET OF A LIST FOR WHICH FN IS TRUE
(DE SUBSET (L FN)
  (PROG (RESULT)
  A (COND ((NULL L)(RETURN (REVERSIP RESULT)))
          ((APPLY FN (LIST (CAR L)))
              (SETQ RESULT (CONS (CAR L) RESULT))))
    (SETQ L (CDR L))
    (GO A)))

(DE REMOVE (X L) (DELETE X L))

%  LIST DIFFERENCE   X - Y
(DE LDIFFERENCE (X Y)
  (MAPCAN X (FUNCTION (LAMBDA (Z)
               (COND ((MEMQ Z Y) NIL)
                     (T (CONS Z NIL)))))))

%  FIRST A FEW FUNCTION DEFINITIONS.

%  GET FUNCTION DEFINITION FOR THE GLISP COMPILER.
(DE GLGETD (FN)
  (OR (and (or (null (get fn 'glcompiled))
               (eq (getddd fn) (get fn 'glcompiled)))
           (GET FN 'GLORIGINALEXPR))
      (GETDDD FN)))

(DE GLGETDB (FN) (GLGETD FN))

(DE GLAMBDATRAN (GLEXPR)
 (PROG (NEWEXPR)
  (SETQ GLLASTFNCOMPILED FAULTFN)
  (PUT FAULTFN 'GLORIGINALEXPR GLEXPR)
  (COND ((SETQ NEWEXPR (GLCOMP FAULTFN GLEXPR NIL NIL NIL))
           (putddd FAULTFN NEWEXPR)
           (put faultfn 'glcompiled newexpr) ))
  (RETURN NEWEXPR) ))

(DE GLERROR (FN MSGLST)
 (PROG ()
  (TERPRI)
  (PRIN2 "GLISP error detected by ")
  (PRIN1 FN)
  (PRIN2 " in function ")
  (PRINT FAULTFN)
  (MAPC MSGLST (FUNCTION (LAMBDA (X) (PRIN1 X)(SPACES 1))))
  (TERPRI)
  (PRIN2 "in expression: ")
  (PRINT (CAR EXPRSTACK))
  (TERPRI)
  (PRIN2 "within expression: ")
  (PRINT (CADR EXPRSTACK))
  (COND (GLBREAKONERROR (ERROR 0 (CAR EXPRSTACK))))
  (RETURN (LIST (LIST 'GLERR (LIST 'QUOTE (CAR EXPRSTACK))))) ))

%  PRINT THE RESULT OF GLISP COMPILATION.
(DE GLP (FN)
 (PROG ()
  (SETQ FN (OR FN GLLASTFNCOMPILED))
  (TERPRI)
  (PRIN2 "GLRESULTTYPE: ")
  (PRINT (GET FN 'GLRESULTTYPE))
  (PRETTYPRINT (GETDDD FN))
  (RETURN FN)))


%  GLISP STRUCTURE EDITOR 
(DE GLEDS (STRNAME)
  (EDITV (GET STRNAME 'GLSTRUCTURE))
  STRNAME)

%  GLISP PROPERTY-LIST EDITOR
(DE GLED (ATM) (EDITV (PROP ATM)))

%  GLISP FUNCTION EDITOR
(DE GLEDF (FNNAME)
  (EDITV (GLGETD FNNAME))
  FNNAME)

(DE KWOTE (X)
  (COND ((NUMBERP X) X)
        (T (LIST (QUOTE QUOTE) X))) )




% {DSK}GLISP.PSL;1 16-MAR-83 12:28:51 





% GSN  7-MAR-83 16:41 
% Transform an expression X for Portable Standard Lisp dialect. 
(DE GLPSLTRANSFM (X)
(PROG (TMP NOTFLG)
      
% First do argument reversals. 

      (COND ((NOT (PAIRP X))
	     (RETURN X))
	    ((MEMQ (CAR X)
		   '(push PUSH))
	     (SETQ X (LIST (CAR X)
			   (CADDR X)
			   (CADR X))))
	    ((MEMQ (CAR X)
		   NIL)
	     (SETQ X (LIST (CAR X)
			   (CADR X)
			   (CADDDR X)
			   (CADDR X))))
	    ((EQ (CAR X)
		 'APPLY*)
	     (SETQ X (LIST 'APPLY
			   (CADR X)
			   (CONS 'LIST
				 (CDDR X))))))
      
% Now see if the result will be negated. 

      (SETQ NOTFLG (MEMQ (CAR X)
			 '(NLISTP BOUNDP GEQ LEQ IGEQ ILEQ)))
      (COND ((SETQ TMP (ASSOC (CAR X)
			      '((MEMB MEMQ)
				(FMEMB MEMQ)
				(FASSOC ASSOC)
				(LITATOM IDP)
				(GETPROP GET)
				(GETPROPLIST PROP)
				(PUTPROP PUT)
				(LISTP PAIRP)
				(NLISTP PAIRP)
				(NEQ NE)
				(IGREATERP GREATERP)
				(IGEQ LESSP)
				(GEQ LESSP)
				(ILESSP LESSP)
				(ILEQ GREATERP)
				(LEQ GREATERP)
				(IPLUS PLUS)
				(IDIFFERENCE DIFFERENCE)
				(ITIMES TIMES)
				(IQUOTIENT QUOTIENT)
                                               (* CommentOutCode)
				(MAPCONC MAPCAN)
				(DECLARE CommentOutCode)
				(NCHARS FlatSize2)
				(NTHCHAR GLNTHCHAR)
				(DREVERSE REVERSIP)
				(STREQUAL String!=)
				(ALPHORDER String!<!=)
				(GLSTRGREATERP String!>)
				(GLSTRGEP String!>!=)
				(GLSTRLESSP String!<)
				(EQP EQN)
				(LAST LASTPAIR)
				(NTH PNth)
				(NCONC1 ACONC)
				(U-CASE GLUCASE)
				(DSUBST SUBSTIP)
				(BOUNDP UNBOUNDP)
				(UNPACK EXPLODE)
				(PACK IMPLODE)
				(DREMOVE DELETIP)
				(GETD GETDDD)
				(PUTD PUTDDD))))
	     (SETQ X (CONS (CADR TMP)
			   (CDR X))))
	    ((AND (EQ (CAR X)
		      'RETURN)
		  (NULL (CDR X)))
	     (SETQ X (LIST (CAR X)
			   NIL)))
	    ((AND (EQ (CAR X)
		      'APPEND)
		  (NULL (CDDR X)))
	     (SETQ X (LIST (CAR X)
			   (CADR X)
			   NIL)))
	    ((EQ (CAR X)
		 'ERROR)
	     (SETQ X (LIST (CAR X)
			   0
			   (COND ((NULL (CDR X))
				  NIL)
				 ((NULL (CDDR X))
				  (CADR X))
				 (T (CONS 'LIST
					  (CDR X)))))))
	    ((EQ (CAR X)
		 'SELECTQ)
	     (RPLACA X 'CASEQ)
	     (SETQ TMP (NLEFT X 2))
	     (COND ((NULL (CADR TMP))
		    (RPLACD TMP NIL))
		   (T (RPLACD TMP (LIST (LIST T (CADR TMP))))))))
      (RETURN (COND (NOTFLG (LIST 'NOT
				  X))
		    (T X)))))


% edited: 18-NOV-82 11:47 
(DF A (L)
(GLAINTERPRETER L))


% edited: 18-NOV-82 11:47 
(DF AN (L)
(GLAINTERPRETER L))


% edited: 29-OCT-81 14:25 
(DE GL-A-AN? (X)
(MEMQ X '(A AN a an An)))


% GSN 17-FEB-83 11:31 
% Test whether FNNAME is an abstract function. 
(DE GLABSTRACTFN? (FNNAME)
(PROG (DEFN)
      (RETURN (AND (SETQ DEFN (GLGETD FNNAME))
		   (PAIRP DEFN)
		   (EQ (CAR DEFN)
		       'MLAMBDA)))))


% GSN 16-FEB-83 12:39 
% Add a PROPerty entry of type PROPTYPE to structure STRNAME. 
(DE GLADDPROP (STRNAME PROPTYPE LST)
(PROG (PL SUBPL)
      (COND ((NOT (AND (ATOM STRNAME)
		       (SETQ PL (GET STRNAME 'GLSTRUCTURE))))
	     (ERROR 0 (LIST STRNAME " has no structure definition.")))
	    ((SETQ SUBPL (LISTGET (CDR PL)
				  PROPTYPE))
	     (NCONC SUBPL (LIST LST)))
	    (T (NCONC PL (LIST PROPTYPE (LIST LST)))))))


% edited: 25-Jan-81 18:17 
% Add the type SDES to RESULTTYPE in GLCOMP 
(DE GLADDRESULTTYPE (SDES)
(COND ((NULL RESULTTYPE)
       (SETQ RESULTTYPE SDES))
      ((AND (PAIRP RESULTTYPE)
	    (EQ (CAR RESULTTYPE)
		'OR))
       (COND ((NOT (MEMBER SDES (CDR RESULTTYPE)))
	      (ACONC RESULTTYPE SDES))))
      ((NOT (EQUAL SDES RESULTTYPE))
       (SETQ RESULTTYPE (LIST 'OR
			      RESULTTYPE SDES)))))


% edited:  2-Jan-81 13:37 
% Add an entry to the current context for a variable ATM, whose NAME 
%   in context is given, and which has structure STR. The entry is 
%   pushed onto the front of the list at the head of the context. 
(DE GLADDSTR (ATM NAME STR CONTEXT)
(RPLACA CONTEXT (CONS (LIST ATM NAME STR)
		      (CAR CONTEXT))))


% GSN 10-FEB-83 12:56 
% edited: 17-Sep-81 13:58 
% Compile code to test if SOURCE is PROPERTY. 
(DE GLADJ (SOURCE PROPERTY ADJWD)
(PROG (ADJL TRANS TMP FETCHCODE)
      (COND ((EQ ADJWD 'ISASELF)
	     (COND ((SETQ ADJL (GLSTRPROP PROPERTY 'ISA
					  'self
					  NIL))
		    (GO A))
		   (T (RETURN NIL))))
	    ((SETQ ADJL (GLSTRPROP (CADR SOURCE)
				   ADJWD PROPERTY NIL))
	     (GO A)))
      
% See if the adjective can be found in a TRANSPARENT substructure. 

      (SETQ TRANS (GLTRANSPARENTTYPES (CADR SOURCE)))
      B
      (COND ((NULL TRANS)
	     (RETURN NIL))
	    ((SETQ TMP (GLADJ (LIST '*GL*
				    (GLXTRTYPE (CAR TRANS)))
			      PROPERTY ADJWD))
	     (SETQ FETCHCODE (GLSTRFN (CAR TRANS)
				      (CADR SOURCE)
				      NIL))
	     (GLSTRVAL TMP (CAR FETCHCODE))
	     (GLSTRVAL TMP (CAR SOURCE))
	     (RETURN TMP))
	    (T (SETQ TRANS (CDR TRANS))
	       (GO B)))
      A
      (COND ((AND (PAIRP (CADR ADJL))
		  (MEMQ (CAADR ADJL)
			'(NOT Not not))
		  (ATOM (CADADR ADJL))
		  (NULL (CDDADR ADJL))
		  (SETQ TMP (GLSTRPROP (CADR SOURCE)
				       ADJWD
				       (CADADR ADJL)
				       NIL)))
	     (SETQ ADJL TMP)
	     (SETQ NOTFLG (NOT NOTFLG))
	     (GO A)))
      (RETURN (GLCOMPMSGL SOURCE ADJWD ADJL NIL CONTEXT))))


% GSN 10-FEB-83 15:08 
(DE GLAINTERPRETER (L)
(PROG (CODE GLNATOM FAULTFN CONTEXT VALBUSY GLSEPATOM GLSEPPTR EXPRSTACK 
	    GLTOPCTX GLGLOBALVARS GLNRECURSIONS)
      (SETQ GLNATOM 0)
      (SETQ GLNRECURSIONS 0)
      (SETQ FAULTFN 'GLAINTERPRETER)
      (SETQ VALBUSY T)
      (SETQ GLSEPPTR 0)
      (SETQ CONTEXT (SETQ GLTOPCTX (LIST NIL)))
      (SETQ CODE (GLDOA (CONS 'A
			      L)))
      (RETURN (EVAL (CAR CODE)))))


% edited: 26-DEC-82 15:40 
% AND operator 
(DE GLANDFN (LHS RHS)
(COND ((NULL LHS)
       RHS)
      ((NULL RHS)
       LHS)
      ((AND (PAIRP (CAR LHS))
	    (EQ (CAAR LHS)
		'AND)
	    (PAIRP (CAR RHS))
	    (EQ (CAAR RHS)
		'AND))
       (LIST (APPEND (CAR LHS)
		     (CDAR RHS))
	     (CADR LHS)))
      ((AND (PAIRP (CAR LHS))
	    (EQ (CAAR LHS)
		'AND))
       (LIST (APPEND (CAR LHS)
		     (LIST (CAR RHS)))
	     (CADR LHS)))
      ((AND (PAIRP (CAR RHS))
	    (EQ (CAAR RHS)
		'AND))
       (LIST (CONS 'AND
		   (CONS (CAR LHS)
			 (CDAR RHS)))
	     (CADR LHS)))
      ((AND (PAIRP (CADR RHS))
	    (EQ (CAADR RHS)
		'LISTOF)
	    (EQUAL (CADR LHS)
		   (CADR RHS)))
       (LIST (LIST 'INTERSECTION
		   (CAR LHS)
		   (CAR RHS))
	     (CADR RHS)))
      ((GLDOMSG LHS 'AND
		(LIST RHS)))
      ((GLUSERSTROP LHS 'AND
		    RHS))
      (T (LIST (LIST 'AND
		     (CAR LHS)
		     (CAR RHS))
	       (CADR RHS)))))


% edited: 19-MAY-82 13:54 
% Test if ATM is the name of any CAR/CDR combination. If so, the value 
%   is a list of the intervening letters in reverse order. 
(DE GLANYCARCDR? (ATM)
(PROG (RES N NMAX TMP)
      (OR (AND (EQ (GLNTHCHAR ATM 1)
		   'C)
	       (EQ (GLNTHCHAR ATM -1)
		   'R))
	  (RETURN NIL))
      (SETQ NMAX (SUB1 (FlatSize2 ATM)))
      (SETQ N 2)
      A
      (COND ((GREATERP N NMAX)
	     (RETURN RES))
	    ((OR (EQ (SETQ TMP (GLNTHCHAR ATM N))
		     'D)
		 (EQ TMP 'A))
	     (SETQ RES (CONS TMP RES))
	     (SETQ N (ADD1 N))
	     (GO A))
	    (T (RETURN NIL)))))


% edited: 26-OCT-82 15:26 
% Try to get indicator IND from an ATOM structure. 
(DE GLATOMSTRFN (IND DES DESLIST)
(PROG (TMP)
      (RETURN (OR (AND (SETQ TMP (ASSOC 'PROPLIST
					(CDR DES)))
		       (GLPROPSTRFN IND TMP DESLIST T))
		  (AND (SETQ TMP (ASSOC 'BINDING
					(CDR DES)))
		       (GLSTRVALB IND (CADR TMP)
				  '(EVAL *GL*)))))))


% GSN  1-FEB-83 16:35 
% edited: 14-Sep-81 12:45 
% Test whether STR is a legal ATOM structure. 
(DE GLATMSTR? (STR)
(PROG (TMP)
      (COND ((OR (AND (CDR STR)
		      (OR (NOT (PAIRP (CADR STR)))
			  (AND (CDDR STR)
			       (OR (NOT (PAIRP (CADDR STR)))
				   (CDDDR STR))))))
	     (RETURN NIL)))
      (COND ((SETQ TMP (ASSOC 'BINDING
			      (CDR STR)))
	     (COND ((OR (CDDR TMP)
			(NULL (GLOKSTR? (CADR TMP))))
		    (RETURN NIL)))))
      (COND ((SETQ TMP (ASSOC 'PROPLIST
			      (CDR STR)))
	     (RETURN (EVERY (CDR TMP)
			    (FUNCTION (LAMBDA (X)
					(AND (ATOM (CAR X))
					     (GLOKSTR? (CADR X)))))))))
      (RETURN T)))


% edited: 23-DEC-82 10:43 
% Test whether TYPE is implemented as an ATOM structure. 
(DE GLATOMTYPEP (TYPE)
(PROG (TYPEB)
      (RETURN (OR (EQ TYPE 'ATOM)
		  (AND (PAIRP TYPE)
		       (MEMQ (CAR TYPE)
			     '(ATOM ATOMOBJECT)))
		  (AND (NE (SETQ TYPEB (GLXTRTYPEB TYPE))
			   TYPE)
		       (GLATOMTYPEP TYPEB))))))


% edited: 24-AUG-82 17:21 
(DE GLBUILDALIST (ALIST PREVLST)
(PROG (LIS TMP1 TMP2)
      A
      (COND ((NULL ALIST)
	     (RETURN (AND LIS (GLBUILDLIST LIS NIL)))))
      (SETQ TMP1 (pop ALIST))
      (COND ((SETQ TMP2 (GLBUILDSTR TMP1 PAIRLIST PREVLST))
	     (SETQ LIS (ACONC LIS (GLBUILDCONS (KWOTE (CAR TMP1))
					       TMP2 T)))))
      (GO A)))


% edited:  9-DEC-82 17:14 
% Generate code to build a CONS structure. OPTFLG is true iff the 
%   structure does not need to be a newly created one. 
(DE GLBUILDCONS (X Y OPTFLG)
(COND ((NULL Y)
       (GLBUILDLIST (LIST X)
		    OPTFLG))
      ((AND (PAIRP Y)
	    (EQ (CAR Y)
		'LIST))
       (GLBUILDLIST (CONS X (CDR Y))
		    OPTFLG))
      ((AND OPTFLG (GLCONST? X)
	    (GLCONST? Y))
       (LIST 'QUOTE
	     (CONS (GLCONSTVAL X)
		   (GLCONSTVAL Y))))
      ((AND (GLCONSTSTR? X)
	    (GLCONSTSTR? Y))
       (LIST 'COPY
	     (LIST 'QUOTE
		   (CONS (GLCONSTVAL X)
			 (GLCONSTVAL Y)))))
      (T (LIST 'CONS
	       X Y))))


% edited:  9-DEC-82 17:13 
% Build a LIST structure, possibly doing compile-time constant 
%   folding. OPTFLG is true iff the structure does not need to be a 
%   newly created copy. 
(DE GLBUILDLIST (LST OPTFLG)
(COND ((EVERY LST (FUNCTION GLCONST?))
       (COND (OPTFLG (LIST 'QUOTE
			   (MAPCAR LST (FUNCTION GLCONSTVAL))))
	     (T (GLGENCODE (LIST 'APPEND
				 (LIST 'QUOTE
				       (MAPCAR LST (FUNCTION GLCONSTVAL))))))))
      ((EVERY LST (FUNCTION GLCONSTSTR?))
       (GLGENCODE (LIST 'COPY
			(LIST 'QUOTE
			      (MAPCAR LST (FUNCTION GLCONSTVAL))))))
      (T (CONS 'LIST
	       LST))))


% edited: 19-OCT-82 15:05 
% Build code to do (NOT CODE) , doing compile-time folding if 
%   possible. 
(DE GLBUILDNOT (CODE)
(PROG (TMP)
      (COND ((GLCONST? CODE)
	     (RETURN (NOT (GLCONSTVAL CODE))))
	    ((NOT (PAIRP CODE))
	     (RETURN (LIST 'NOT
			   CODE)))
	    ((EQ (CAR CODE)
		 'NOT)
	     (RETURN (CADR CODE)))
	    ((NOT (ATOM (CAR CODE)))
	     (RETURN NIL))
	    ((SETQ TMP (ASSOC (CAR CODE)
			      '((EQ NE)
				(NE EQ)
				(LEQ GREATERP)
				(GEQ LESSP))))
	     (RETURN (CONS (CADR TMP)
			   (CDR CODE))))
	    (T (RETURN (LIST 'NOT
			     CODE))))))


% edited: 26-OCT-82 16:02 
(DE GLBUILDPROPLIST (PLIST PREVLST)
(PROG (LIS TMP1 TMP2)
      A
      (COND ((NULL PLIST)
	     (RETURN (AND LIS (GLBUILDLIST LIS NIL)))))
      (SETQ TMP1 (pop PLIST))
      (COND ((SETQ TMP2 (GLBUILDSTR TMP1 PAIRLIST PREVLST))
	     (SETQ LIS (NCONC LIS (LIST (KWOTE (CAR TMP1))
					TMP2)))))
      (GO A)))


% edited: 12-NOV-82 11:26 
% Build a RECORD structure. 
(DE GLBUILDRECORD (STR PAIRLIST PREVLST)
(PROG (TEMP ITEMS RECORDNAME)
      (COND ((ATOM (CADR STR))
	     (SETQ RECORDNAME (CADR STR))
	     (SETQ ITEMS (CDDR STR)))
	    (T (SETQ ITEMS (CDR STR))))
      (COND ((EQ (CAR STR)
		 'OBJECT)
	     (SETQ ITEMS (CONS '(CLASS ATOM)
			       ITEMS))))
      (RETURN (CONS 'Vector
		    (MAPCAR ITEMS (FUNCTION (LAMBDA (X)
					      (GLBUILDSTR X PAIRLIST PREVLST)))
			    )))))


% GSN  7-MAR-83 17:01 
% edited: 13-Aug-81 14:06 
% Generate code to build a structure according to the structure 
%   description STR. PAIRLIST is a list of elements of the form 
%   (SLOTNAME CODE TYPE) for each named slot to be filled in in the 
%   structure. 
(DE GLBUILDSTR (STR PAIRLIST PREVLST)
(PROG (PROPLIS TEMP PROGG TMPCODE ATMSTR)
      (SETQ ATMSTR '((ATOM)
		     (INTEGER . 0)
		     (REAL . 0.0)
		     (NUMBER . 0)
		     (BOOLEAN)
		     (NIL)
		     (ANYTHING)))
      (COND ((NULL STR)
	     (RETURN NIL))
	    ((ATOM STR)
	     (COND ((SETQ TEMP (ASSOC STR ATMSTR))
		    (RETURN (CDR TEMP)))
		   ((MEMQ STR PREVLST)
		    (RETURN NIL))
		   ((SETQ TEMP (GLGETSTR STR))
		    (RETURN (GLBUILDSTR TEMP NIL (CONS STR PREVLST))))
		   (T (RETURN NIL))))
	    ((NOT (PAIRP STR))
	     (GLERROR 'GLBUILDSTR
		      (LIST "Illegal structure type encountered:" STR))
	     (RETURN NIL)))
      (RETURN (CASEQ (CAR STR)
		     (CONS (GLBUILDCONS (GLBUILDSTR (CADR STR)
						    PAIRLIST PREVLST)
					(GLBUILDSTR (CADDR STR)
						    PAIRLIST PREVLST)
					NIL))
		     (LIST (GLBUILDLIST (MAPCAR (CDR STR)
						(FUNCTION (LAMBDA (X)
							    (GLBUILDSTR X 
								  PAIRLIST 
								   PREVLST))))
					NIL))
		     (LISTOBJECT (GLBUILDLIST
				   (CONS (KWOTE (CAR PREVLST))
					 (MAPCAR (CDR STR)
						 (FUNCTION (LAMBDA (X)
							     (GLBUILDSTR
							       X PAIRLIST 
							       PREVLST)))))
				   NIL))
		     (ALIST (GLBUILDALIST (CDR STR)
					  PREVLST))
		     (PROPLIST (GLBUILDPROPLIST (CDR STR)
						PREVLST))
		     (ATOM (SETQ PROGG
				 (LIST 'PROG
				       (LIST 'ATOMNAME)
				       (LIST 'SETQ
					     'ATOMNAME
					     (COND
					       ((AND PREVLST
						     (ATOM (CAR PREVLST)))
						(LIST 'GLMKATOM
						      (KWOTE (CAR PREVLST))))
					       (T (LIST 'GENSYM))))))
			   (COND ((SETQ TEMP (ASSOC 'BINDING
						    (CDR STR)))
				  (SETQ TMPCODE (GLBUILDSTR (CADR TEMP)
							    PAIRLIST PREVLST))
				  (ACONC PROGG (LIST 'SET
						     'ATOMNAME
						     TMPCODE))))
			   (COND ((SETQ TEMP (ASSOC 'PROPLIST
						    (CDR STR)))
				  (SETQ PROPLIS (CDR TEMP))
				  (GLPUTPROPS PROPLIS PREVLST)))
			   (ACONC PROGG (COPY '(RETURN ATOMNAME)))
			   PROGG)
		     (ATOMOBJECT
		       (SETQ PROGG
			     (LIST 'PROG
				   (LIST 'ATOMNAME)
				   (LIST 'SETQ
					 'ATOMNAME
					 (COND ((AND PREVLST
						     (ATOM (CAR PREVLST)))
						(LIST 'GLMKATOM
						      (KWOTE (CAR PREVLST))))
					       (T (LIST 'GENSYM))))))
		       (ACONC PROGG (GLGENCODE (LIST 'PUTPROP
						     'ATOMNAME
						     (LIST 'QUOTE
							   'CLASS)
						     (KWOTE (CAR PREVLST)))))
		       (GLPUTPROPS (CDR STR)
				   PREVLST)
		       (ACONC PROGG (COPY '(RETURN ATOMNAME))))
		     (TRANSPARENT (AND (NOT (MEMQ (CADR STR)
						  PREVLST))
				       (SETQ TEMP (GLGETSTR (CADR STR)))
				       (GLBUILDSTR TEMP PAIRLIST
						   (CONS (CADR STR)
							 PREVLST))))
		     (LISTOF NIL)
		     (RECORD (GLBUILDRECORD STR PAIRLIST PREVLST))
		     (OBJECT (GLBUILDRECORD STR
					    (CONS (LIST 'CLASS
							(KWOTE (CAR PREVLST))
							'ATOM)
						  PAIRLIST)
					    PREVLST))
		     (T (COND ((ATOM (CAR STR))
			       (COND ((SETQ TEMP (ASSOC (CAR STR)
							PAIRLIST))
				      (CADR TEMP))
				     ((AND (ATOM (CADR STR))
					   (NOT (ASSOC (CADR STR)
						       ATMSTR)))
				      (GLBUILDSTR (CADR STR)
						  NIL PREVLST))
				     (T (GLBUILDSTR (CADR STR)
						    PAIRLIST PREVLST))))
			      (T NIL)))))))


% edited: 14-MAR-83 16:59 
% Find the result type for a CAR/CDR function applied to a structure 
%   whose description is STR. LST is a list of A and D in application 
%   order. 
(DE GLCARCDRRESULTTYPE (LST STR)
(COND ((NULL LST)
       STR)
      ((NULL STR)
       NIL)
      ((MEMQ STR GLBASICTYPES)
       NIL)
      ((ATOM STR)
       (GLCARCDRRESULTTYPE LST (GLGETSTR STR)))
      ((NOT (PAIRP STR))
       (ERROR 0 NIL))
      (T (GLCARCDRRESULTTYPEB LST (GLXTRTYPE STR)))))


% edited: 19-MAY-82 14:41 
% Find the result type for a CAR/CDR function applied to a structure 
%   whose description is STR. LST is a list of A and D in application 
%   order. 
(DE GLCARCDRRESULTTYPEB (LST STR)
(COND ((NULL STR)
       NIL)
      ((ATOM STR)
       (GLCARCDRRESULTTYPE LST STR))
      ((NOT (PAIRP STR))
       (ERROR 0 NIL))
      ((AND (ATOM (CAR STR))
	    (NOT (MEMQ (CAR STR)
		       GLTYPENAMES))
	    (CDR STR)
	    (NULL (CDDR STR)))
       (GLCARCDRRESULTTYPE LST (CADR STR)))
      ((EQ (CAR LST)
	   'A)
       (COND ((OR (EQ (CAR STR)
		      'LISTOF)
		  (EQ (CAR STR)
		      'CONS)
		  (EQ (CAR STR)
		      'LIST))
	      (GLCARCDRRESULTTYPE (CDR LST)
				  (CADR STR)))
	     (T NIL)))
      ((EQ (CAR LST)
	   'D)
       (COND ((EQ (CAR STR)
		  'CONS)
	      (GLCARCDRRESULTTYPE (CDR LST)
				  (CADDR STR)))
	     ((EQ (CAR STR)
		  'LIST)
	      (COND ((CDDR STR)
		     (GLCARCDRRESULTTYPE (CDR LST)
					 (CONS 'LIST
					       (CDDR STR))))
		    (T NIL)))
	     ((EQ (CAR STR)
		  'LISTOF)
	      (GLCARCDRRESULTTYPE (CDR LST)
				  STR))))
      (T (ERROR 0 NIL))))


% edited: 13-JAN-82 13:45 
% Test if X is a CAR or CDR combination up to 3 long. 
(DE GLCARCDR? (X)
(MEMQ X
      '(CAR CDR CAAR CADR CDAR CDDR CAAAR CAADR CADAR CDAAR CADDR CDADR CDDAR 
	    CDDDR)))


% edited:  5-OCT-82 15:24 
(DE GLCC (FN)
(SETQ FN (OR FN GLLASTFNCOMPILED))(COND ((NOT (GLGETD FN))
					 (PRIN1 FN)
					 (PRIN1 " ?")
					 (TERPRI))
					(T (GLCOMPILE FN))))


% GSN 18-JAN-83 15:04 
% Get the Class of object OBJ. 
(DE GLCLASS (OBJ)
(PROG (CLASS)
      (RETURN (AND (SETQ CLASS (COND ((VectorP OBJ)
				      (GetV OBJ 0))
				     ((ATOM OBJ)
				      (GET OBJ 'CLASS))
				     ((PAIRP OBJ)
				      (CAR OBJ))
				     (T NIL)))
		   (GLCLASSP CLASS)
		   CLASS))))


% edited: 11-NOV-82 11:23 
% Test whether the object OBJ is a member of class CLASS. 
(DE GLCLASSMEMP (OBJ CLASS)
(GLDESCENDANTP (GLCLASS OBJ)
	       CLASS))


% edited: 11-NOV-82 11:45 
% See if CLASS is a Class name. 
(DE GLCLASSP (CLASS)
(PROG (TMP)
      (RETURN (AND (ATOM CLASS)
		   (SETQ TMP (GET CLASS 'GLSTRUCTURE))
		   (MEMQ (CAR (GLXTRTYPE (CAR TMP)))
			 '(OBJECT ATOMOBJECT LISTOBJECT))))))


% GSN  9-FEB-83 16:58 
% Execute a message to CLASS with selector SELECTOR and arguments 
%   ARGS. PROPNAME is one of MSG, ADJ, ISA, PROP. 
(DE GLCLASSSEND (CLASS SELECTOR ARGS PROPNAME)
(PROG (FNCODE)
      (COND ((SETQ FNCODE (GLCOMPPROP CLASS SELECTOR PROPNAME))
	     (RETURN (COND ((ATOM FNCODE)
			    (EVAL (CONS FNCODE (MAPCAR ARGS
						       (FUNCTION KWOTE)))))
			   (T (APPLY FNCODE ARGS))))))
      (RETURN 'GLSENDFAILURE)))


% GSN 10-FEB-83 15:09 
% GLISP compiler function. GLAMBDAFN is the atom whose function 
%   definition is being compiled; GLEXPR is the GLAMBDA expression to 
%   be compiled. The compiled function is saved on the property list 
%   of GLAMBDAFN under the indicator GLCOMPILED. The property 
%   GLRESULTTYPE is the RESULT declaration, if specified; GLGLOBALS is 
%   a list of global variables referenced and their types. 
(DE GLCOMP (GLAMBDAFN GLEXPR GLTYPESUBS GLFNSUBS ARGTYPES)
(PROG (NEWARGS NEWEXPR GLNATOM GLTOPCTX RESULTTYPE GLGLOBALVARS RESULT 
	       GLSEPATOM GLSEPPTR VALBUSY EXPRSTACK GLTU GLNRECURSIONS)
      (SETQ GLSEPPTR 0)
      (SETQ GLNRECURSIONS 0)
      (COND ((NOT GLQUIETFLG)
	     (PRINT (LIST 'GLCOMP
			  GLAMBDAFN))))
      (SETQ EXPRSTACK (LIST GLEXPR))
      (SETQ GLNATOM 0)
      (SETQ GLTOPCTX (LIST NIL))
      (SETQ GLTU GLTYPESUSED)
      (SETQ GLTYPESUSED NIL)
      
% Process the argument list of the GLAMBDA. 

      (SETQ NEWARGS (GLDECL (CADR GLEXPR)
			    '(T NIL)
			    GLTOPCTX GLAMBDAFN ARGTYPES))
      
% See if there is a RESULT declaration. 

      (SETQ GLEXPR (CDDR GLEXPR))
      (GLSKIPCOMMENTS)
      (GLRESGLOBAL)
      (GLSKIPCOMMENTS)
      (GLRESGLOBAL)
      (SETQ VALBUSY (NULL (CDR GLEXPR)))
      (SETQ NEWEXPR (GLPROGN GLEXPR (CONS NIL GLTOPCTX)))
      (PUT GLAMBDAFN 'GLRESULTTYPE
	   (OR RESULTTYPE (CADR NEWEXPR)))
      (PUT GLAMBDAFN 'GLTYPESUSED
	   GLTYPESUSED)
      (GLSAVEFNTYPES GLAMBDAFN GLTYPESUSED)
      (SETQ RESULT (GLUNWRAP (CONS 'LAMBDA
				   (CONS NEWARGS (CAR NEWEXPR)))
			     T))
      (SETQ GLTYPESUSED GLTU)
      (RETURN RESULT)))


% GSN  2-FEB-83 14:52 
% Compile an abstract function into an instance function given the 
%   specified set of type substitutions and function substitutions. 
(DE GLCOMPABSTRACT (FN INSTFN TYPESUBS FNSUBS ARGTYPES)
(PROG (TMP)
      (COND (INSTFN)
	    ((SETQ TMP (ASSOC FN FNSUBS))
	     (SETQ INSTFN (CDR TMP)))
	    (T (SETQ INSTFN (GLINSTANCEFNNAME FN))))
      (SETQ FNSUBS (CONS (CONS FN INSTFN)
			 FNSUBS))
      
% Now compile the abstract function with the specified type 
%   substitutions. 

      (PUTDDD INSTFN (GLCOMP INSTFN (GLGETD FN)
			     TYPESUBS FNSUBS ARGTYPES))
      (RETURN INSTFN)))


% GSN 10-FEB-83 15:09 
% Compile a GLISP expression. CODE is a GLISP expression. VARLST is a 
%   list of lists (VAR TYPE) . The result is a list (OBJCODE TYPE) 
%   where OBJCODE is the Lisp code corresponding to CODE and TYPE is 
%   the type returned by OBJCODE. 
(DE GLCOMPEXPR (CODE VARLST)
(PROG (OBJCODE GLNATOM CONTEXT VALBUSY GLSEPATOM GLSEPPTR EXPRSTACK GLTOPCTX 
	       GLGLOBALVARS GLTYPESUBS FAULTFN GLNRECURSIONS)
      (SETQ FAULTFN 'GLCOMPEXPR)
      (SETQ GLNRECURSIONS 0)
      (SETQ GLNATOM 0)
      (SETQ VALBUSY T)
      (SETQ GLSEPPTR 0)
      (SETQ CONTEXT (SETQ GLTOPCTX (LIST NIL)))
      (MAPC VARLST (FUNCTION (LAMBDA (X)
			       (GLADDSTR (CAR X)
					 NIL
					 (CADR X)
					 CONTEXT))))
      (COND ((SETQ OBJCODE (GLPUSHEXPR CODE T CONTEXT T))
	     (RETURN (LIST (GLUNWRAP (CAR OBJCODE)
				     T)
			   (CADR OBJCODE)))))))


% edited: 27-MAY-82 12:58 
% Compile the function definition stored for the atom FAULTFN using 
%   the GLISP compiler. 
(DE GLCOMPILE (FAULTFN)
(GLAMBDATRAN (GLGETD FAULTFN))FAULTFN)


% edited:  4-MAY-82 11:13 
% Compile FN if not already compiled. 
(DE GLCOMPILE? (FN)
(OR (GET FN 'GLCOMPILED)
    (GLCOMPILE FN)))


% GSN 10-FEB-83 15:33 
% Compile a Message. MSGLST is the Message list, consisting of message 
%   selector, code, and properties defined with the message. 
(DE GLCOMPMSG (OBJECT MSGLST ARGLIST CONTEXT)
(PROG (RESULT)
      (COND ((GREATERP (SETQ GLNRECURSIONS (ADD1 GLNRECURSIONS))
		       9)
	     (RETURN (GLERROR 'GLCOMPMSG
			      (LIST "Infinite loop detected in compiling"
				    (CAR MSGLST)
				    "for object of type"
				    (CADR OBJECT))))))
      (SETQ RESULT (GLCOMPMSGB OBJECT MSGLST ARGLIST CONTEXT))
      (SETQ GLNRECURSIONS (SUB1 GLNRECURSIONS))
      (RETURN RESULT)))


% GSN 10-FEB-83 15:13 
% Compile a Message. MSGLST is the Message list, consisting of message 
%   selector, code, and properties defined with the message. 
(DE GLCOMPMSGB (OBJECT MSGLST ARGLIST CONTEXT)
(PROG
  (GLPROGLST RESULTTYPE METHOD RESULT VTYPE)
  (SETQ RESULTTYPE (LISTGET (CDDR MSGLST)
			    'RESULT))
  (SETQ METHOD (CADR MSGLST))
  (COND
    ((ATOM METHOD)
     
% Function name is specified. 

     (COND
       ((LISTGET (CDDR MSGLST)
		 'OPEN)
	(RETURN (GLCOMPOPEN METHOD (CONS OBJECT ARGLIST)
			    (CONS (CADR OBJECT)
				  (LISTGET (CDDR MSGLST)
					   'ARGTYPES))
			    RESULTTYPE
			    (LISTGET (CDDR MSGLST)
				     'SPECVARS))))
       (T (RETURN (LIST (CONS METHOD (CONS (CAR OBJECT)
					   (MAPCAR ARGLIST
						   (FUNCTION CAR))))
			(OR (GLRESULTTYPE
			      METHOD
			      (CONS (CADR OBJECT)
				    (MAPCAR ARGLIST (FUNCTION CADR))))
			    (LISTGET (CDDR MSGLST)
				     'RESULT)))))))
    ((NOT (PAIRP METHOD))
     (RETURN (GLERROR 'GLCOMPMSG
		      (LIST "The form of Response is illegal for message"
			    (CAR MSGLST)))))
    ((AND (PAIRP (CAR METHOD))
	  (MEMQ (CAAR METHOD)
		'(virtual Virtual VIRTUAL)))
     (OR (SETQ VTYPE (LISTGET (CDDR MSGLST)
			      'VTYPE))
	 (PROGN (SETQ VTYPE (GLMAKEVTYPE (CADR OBJECT)
					 (CAR METHOD)))
		(NCONC MSGLST (LIST 'VTYPE
				    VTYPE))))
     (RETURN (LIST (CAR OBJECT)
		   VTYPE))))
  
% The Method is a list of stuff to be compiled open. 

  (SETQ CONTEXT (LIST NIL))
  (COND ((ATOM (CAR OBJECT))
	 (GLADDSTR (LIST 'PROG1
			 (CAR OBJECT))
		   'self
		   (CADR OBJECT)
		   CONTEXT))
	((AND (PAIRP (CAR OBJECT))
	      (EQ (CAAR OBJECT)
		  'PROG1)
	      (ATOM (CADAR OBJECT))
	      (NULL (CDDAR OBJECT)))
	 (GLADDSTR (CAR OBJECT)
		   'self
		   (CADR OBJECT)
		   CONTEXT))
	(T (SETQ GLPROGLST (CONS (LIST 'self
				       (CAR OBJECT))
				 GLPROGLST))
	   (GLADDSTR 'self
		     NIL
		     (CADR OBJECT)
		     CONTEXT)))
  (SETQ RESULT (GLPROGN METHOD CONTEXT))
  
% If more than one expression resulted, embed in a PROGN. 

  (RPLACA RESULT (COND ((CDAR RESULT)
			(CONS 'PROGN
			      (CAR RESULT)))
		       (T (CAAR RESULT))))
  (RETURN (LIST (COND (GLPROGLST (GLGENCODE (LIST 'PROG
						  GLPROGLST
						  (LIST 'RETURN
							(CAR RESULT)))))
		      (T (CAR RESULT)))
		(OR RESULTTYPE (CADR RESULT))))))


% GSN 16-FEB-83 17:37 
% Attempt to compile code for a message list for an object. OBJECT is 
%   the destination, in the form (<code> <type>) , PROPTYPE is the 
%   property type (ADJ etc.) , MSGLST is the message list, and ARGS is 
%   a list of arguments of the form (<code> <type>) . The result is of 
%   the form (<code> <type>) , or NIL if failure. 
(DE GLCOMPMSGL (OBJECT PROPTYPE MSGLST ARGS CONTEXT)
(PROG
  (TYPE SELECTOR NEWFN NEWMSGLST)
  (SETQ TYPE (GLXTRTYPE (CADR OBJECT)))
  (SETQ SELECTOR (CAR MSGLST))
  (RETURN
    (COND
      ((LISTGET (CDDR MSGLST)
		'MESSAGE)
       (SETQ CONTEXT (LIST NIL))
       (GLADDSTR (CAR OBJECT)
		 'self
		 TYPE CONTEXT)
       (LIST
	 (COND
	   ((EQ PROPTYPE 'MSG)
	    (CONS 'SEND
		  (CONS (CAR OBJECT)
			(CONS SELECTOR (MAPCAR ARGS (FUNCTION CAR))))))
	   (T (CONS 'SENDPROP
		    (CONS (CAR OBJECT)
			  (CONS SELECTOR (CONS PROPTYPE
					       (MAPCAR ARGS
						       (FUNCTION CAR))))))))
	 (GLEVALSTR (LISTGET (CDDR MSGLST)
			     'RESULT)
		    CONTEXT)))
      ((LISTGET (CDDR MSGLST)
		'SPECIALIZE)
       (SETQ NEWFN (GLINSTANCEFNNAME (CADR MSGLST)))
       (SETQ NEWMSGLST (LIST (CAR MSGLST)
			     NEWFN
			     'SPECIALIZATION
			     T))
       (GLADDPROP (CADR OBJECT)
		  PROPTYPE NEWMSGLST)
       (GLCOMPABSTRACT (CADR MSGLST)
		       NEWFN NIL NIL (CONS (CADR OBJECT)
					   (MAPCAR ARGS
						   (FUNCTION CADR))))
       (PUT NEWFN 'GLSPECIALIZATION
	    (CONS (LIST (CADR MSGLST)
			(CADR OBJECT)
			PROPTYPE SELECTOR)
		  (GET NEWFN 'GLSPECIALIZATION)))
       (NCONC NEWMSGLST (LIST 'RESULT
			      (GET NEWFN 'GLRESULTTYPE)))
       (GLCOMPMSG OBJECT NEWMSGLST ARGS CONTEXT))
      (T (GLCOMPMSG OBJECT MSGLST ARGS CONTEXT))))))


% GSN  4-MAR-83 14:17 
% Compile the function FN Open, given as arguments ARGS with argument 
%   types ARGTYPES. Types may be defined in the definition of function 
%   FN (which may be either a GLAMBDA or LAMBDA function) or by 
%   ARGTYPES; ARGTYPES takes precedence. 
(DE GLCOMPOPEN (FN ARGS ARGTYPES RESULTTYPE SPCVARS)
(PROG (PTR FNDEF GLPROGLST NEWEXPR CONTEXT NEWARGS)
      
% Put a new level on top of CONTEXT. 

      (SETQ CONTEXT (LIST NIL))
      (SETQ FNDEF (GLGETD FN))
      
% Get the parameter declarations and add to CONTEXT. 

      (GLDECL (CADR FNDEF)
	      '(T NIL)
	      CONTEXT NIL NIL)
      
% Make the function parameters into names and put in the values, 
%   hiding any which are simple variables. 

      (SETQ PTR (REVERSIP (CAR CONTEXT)))
      (RPLACA CONTEXT NIL)
      LP
      (COND ((NULL PTR)
	     (GO B)))
      (COND ((EQ ARGS T)
	     (GLADDSTR (CAAR PTR)
		       NIL
		       (OR (CAR ARGTYPES)
			   (CADDAR PTR))
		       CONTEXT)
	     (SETQ NEWARGS (CONS (CAAR PTR)
				 NEWARGS)))
	    ((AND (ATOM (CAAR ARGS))
		  (NE SPCVARS T)
		  (NOT (MEMQ (CAAR PTR)
			     SPCVARS)))
	     
% Wrap the atom in a PROG1 so it won't match as a name; the PROG1 will 
%   generally be stripped later. 

	     (GLADDSTR (LIST 'PROG1
			     (CAAR ARGS))
		       (CAAR PTR)
		       (OR (CADAR ARGS)
			   (CAR ARGTYPES)
			   (CADDAR PTR))
		       CONTEXT))
	    ((AND (NE SPCVARS T)
		  (NOT (MEMQ (CAAR PTR)
			     SPCVARS))
		  (PAIRP (CAAR ARGS))
		  (EQ (CAAAR ARGS)
		      'PROG1)
		  (ATOM (CADAAR ARGS))
		  (NULL (CDDAAR ARGS)))
	     (GLADDSTR (CAAR ARGS)
		       (CAAR PTR)
		       (OR (CADAR ARGS)
			   (CAR ARGTYPES)
			   (CADDAR PTR))
		       CONTEXT))
	    (T 
% Since the actual argument is not atomic, make a PROG variable for 
%   it. 

	       (SETQ GLPROGLST (CONS (LIST (CAAR PTR)
					   (CAAR ARGS))
				     GLPROGLST))
	       (GLADDSTR (CAAR PTR)
			 (CADAR PTR)
			 (OR (CADAR ARGS)
			     (CAR ARGTYPES)
			     (CADDAR PTR))
			 CONTEXT)))
      (SETQ PTR (CDR PTR))
      (COND ((PAIRP ARGS)
	     (SETQ ARGS (CDR ARGS))))
      (SETQ ARGTYPES (CDR ARGTYPES))
      (GO LP)
      B
      (SETQ FNDEF (CDDR FNDEF))
      
% Get rid of comments at start of function. 

      C
      (COND ((AND FNDEF (PAIRP (CAR FNDEF))
		  (MEMQ (CAAR FNDEF)
			'(RESULT * GLOBAL)))
	     (SETQ FNDEF (CDR FNDEF))
	     (GO C)))
      (SETQ NEWEXPR (GLPROGN FNDEF CONTEXT))
      
% Get rid of atomic result if it isnt busy outside. 

      (COND ((AND (NOT VALBUSY)
		  (CDAR EXPR)
		  (OR (ATOM (CADR (SETQ PTR (NLEFT (CAR NEWEXPR)
						   2))))
		      (AND (PAIRP (CADR PTR))
			   (EQ (CAADR PTR)
			       'PROG1)
			   (ATOM (CADADR PTR))
			   (NULL (CDDADR PTR)))))
	     (RPLACD PTR NIL)))
      (SETQ RESULT (LIST (COND (GLPROGLST (SETQ PTR (LASTPAIR (CAR NEWEXPR)))
					  (RPLACA PTR (LIST 'RETURN
							    (CAR PTR)))
					  (GLGENCODE
					    (CONS 'PROG
						  (CONS (REVERSIP GLPROGLST)
							(CAR NEWEXPR)))))
			       ((CDAR NEWEXPR)
				(CONS 'PROGN
				      (CAR NEWEXPR)))
			       (T (CAAR NEWEXPR)))
			 (OR RESULTTYPE (GLRESULTTYPE FN NIL)
			     (CADR NEWEXPR))))
      (COND ((EQ ARGS T)
	     (RPLACA RESULT (LIST 'LAMBDA
				  (REVERSIP NEWARGS)
				  (CAR RESULT)))))
      (RETURN RESULT)))


% GSN  1-FEB-83 16:18 
% Compile a LAMBDA expression to compute the property PROPNAME of type 
%   PROPTYPE for structure STR. The property type STR is allowed for 
%   structure access. 
(DE GLCOMPPROP (STR PROPNAME PROPTYPE)
(PROG (CODE PL SUBPL PROPENT)
      
% See if the property has already been compiled. 

      (COND ((AND (SETQ PL (GET STR 'GLPROPFNS))
		  (SETQ SUBPL (ASSOC PROPTYPE PL))
		  (SETQ PROPENT (ASSOC PROPNAME (CDR SUBPL))))
	     (RETURN (CADR PROPENT))))
      
% Compile code for this property and save it. 

      (COND ((NOT (MEMQ PROPTYPE '(STR ADJ ISA PROP MSG)))
	     (ERROR 0 NIL)))
      (OR (SETQ CODE (GLCOMPPROPL STR PROPNAME PROPTYPE))
	  (RETURN NIL))
      (COND ((NOT PL)
	     (PUT STR 'GLPROPFNS
		  (SETQ PL (COPY '((STR)
				   (PROP)
				   (ADJ)
				   (ISA)
				   (MSG)))))
	     (SETQ SUBPL (ASSOC PROPTYPE PL))))
      (RPLACD SUBPL (CONS (CONS PROPNAME CODE)
			  (CDR SUBPL)))
      (RETURN (CAR CODE))))


% GSN 16-FEB-83 11:25 
% Compile a message as a closed form, i.e., function name or LAMBDA 
%   form. 
(DE GLCOMPPROPL (STR PROPNAME PROPTYPE)
(PROG (CODE MSGL TRANS TMP FETCHCODE NEWVAR GLNATOM CONTEXT VALBUSY GLSEPATOM 
	    GLSEPPTR EXPRSTACK GLTOPCTX GLGLOBALVARS GLTYPESUBS FAULTFN 
	    GLNRECURSIONS)
      (SETQ FAULTFN 'GLCOMPPROPL)
      (SETQ GLNRECURSIONS 0)
      (SETQ GLNATOM 0)
      (SETQ VALBUSY T)
      (SETQ GLSEPPTR 0)
      (SETQ CONTEXT (SETQ GLTOPCTX (LIST NIL)))
      (COND ((EQ PROPTYPE 'STR)
	     (COND ((SETQ CODE (GLSTRFN PROPNAME STR NIL))
		    (RETURN (LIST (LIST 'LAMBDA
					(LIST 'self)
					(GLUNWRAP (SUBSTIP 'self
							   '*GL*
							   (CAR CODE))
						  T))
				  (CADR CODE))))
		   (T (RETURN NIL))))
	    ((SETQ MSGL (GLSTRPROP STR PROPTYPE PROPNAME NIL))
	     (COND ((ATOM (CADR MSGL))
		    (COND ((LISTGET (CDDR MSGL)
				    'OPEN)
			   (SETQ CODE (GLCOMPOPEN (CADR MSGL)
						  T
						  (LIST STR)
						  NIL NIL)))
			  (T (SETQ CODE (LIST (CADR MSGL)
					      (GLRESULTTYPE (CADR MSGL)
							    NIL))))))
		   ((SETQ CODE (GLADJ (LIST 'self
					    STR)
				      PROPNAME PROPTYPE))
		    (SETQ CODE (LIST (LIST 'LAMBDA
					   (LIST 'self)
					   (GLUNWRAP (CAR CODE)
						     T))
				     (CADR CODE))))))
	    ((SETQ TRANS (GLTRANSPARENTTYPES STR))
	     (GO B))
	    (T (RETURN NIL)))
      (RETURN (LIST (GLUNWRAP (CAR CODE)
			      T)
		    (OR (CADR CODE)
			(LISTGET (CDDR MSGL)
				 'RESULT))))
      
% Look for the message in a contained TRANSPARENT type. 

      B
      (COND ((NULL TRANS)
	     (RETURN NIL))
	    ((SETQ TMP (GLCOMPPROPL (GLXTRTYPE (CAR TRANS))
				    PROPNAME PROPTYPE))
	     (COND ((ATOM (CAR TMP))
		    (GLERROR 'GLCOMPPROPL
			     (LIST "GLISP cannot currently" 
				   "handle inheritance of the property"
				   PROPNAME 
				   "which is specified as a function name"
				   "in a TRANSPARENT subtype.  Sorry."))
		    (RETURN NIL)))
	     (SETQ FETCHCODE (GLSTRFN (CAR TRANS)
				      STR NIL))
	     (SETQ NEWVAR (GLMKVAR))
	     (GLSTRVAL FETCHCODE NEWVAR)
	     (RETURN (LIST (GLUNWRAP (LIST 'LAMBDA
					   (CONS NEWVAR (CDADAR TMP))
					   (LIST 'PROG
						 (LIST (LIST (CAADAR TMP)
							     (CAR FETCHCODE)))
						 (LIST 'RETURN
						       (CADDAR TMP))))
				     T)
			   (CADR TMP))))
	    (T (SETQ TRANS (CDR TRANS))
	       (GO B)))))


% edited: 14-MAR-83 17:07 
% Attempt to infer the type of a constant expression. 
(DE GLCONSTANTTYPE (EXPR)
(PROG (TMP TYPES)
      (COND ((SETQ TMP (COND ((FIXP EXPR)
			      'INTEGER)
			     ((NUMBERP EXPR)
			      'NUMBER)
			     ((ATOM EXPR)
			      'ATOM)
			     ((STRINGP EXPR)
			      'STRING)
			     ((NOT (PAIRP EXPR))
			      'ANYTHING)
			     ((NOT (OR (NULL (CDR EXPR))
				       (PAIRP (CDR EXPR))))
			      'ANYTHING)
			     ((EVERY EXPR (FUNCTION FIXP))
			      '(LISTOF INTEGER))
			     ((EVERY EXPR (FUNCTION NUMBERP))
			      '(LISTOF NUMBER))
			     ((EVERY EXPR (FUNCTION ATOM))
			      '(LISTOF ATOM))
			     ((EVERY EXPR (FUNCTION STRINGP))
			      '(LISTOF STRING))))
	     (RETURN TMP)))
      (SETQ TYPES (MAPCAR EXPR (FUNCTION GLCONSTANTTYPE)))
      (COND ((EVERY (CDR TYPES)
		    (FUNCTION (LAMBDA (Y)
				(EQUAL Y (CAR TYPES)))))
	     (RETURN (LIST 'LISTOF
			   (CAR TYPES))))
	    (T (RETURN (CONS 'LIST
			     TYPES))))))


% edited: 31-AUG-82 15:38 
% Test X to see if it represents a compile-time constant value. 
(DE GLCONST? (X)
(OR (NULL X)
    (EQ X T)
    (NUMBERP X)
    (AND (PAIRP X)
	 (EQ (CAR X)
	     'QUOTE)
	 (ATOM (CADR X)))
    (AND (ATOM X)
	 (GET X 'GLISPCONSTANTFLG))))


% edited:  9-DEC-82 17:02 
% Test to see if X is a constant structure. 
(DE GLCONSTSTR? (X)
(OR (GLCONST? X)
    (AND (PAIRP X)
	 (OR (EQ (CAR X)
		 'QUOTE)
	     (AND (MEMQ (CAR X)
			'(COPY APPEND))
		  (PAIRP (CADR X))
		  (EQ (CAADR X)
		      'QUOTE)
		  (OR (NE (CAR X)
			  'APPEND)
		      (NULL (CDDR X))
		      (NULL (CADDR X))))
	     (AND (EQ (CAR X)
		      'LIST)
		  (EVERY (CDR X)
			 (FUNCTION GLCONSTSTR?)))
	     (AND (EQ (CAR X)
		      'CONS)
		  (GLCONSTSTR? (CADR X))
		  (GLCONSTSTR? (CADDR X)))))))


% edited:  9-DEC-82 17:07 
% Get the value of a compile-time constant 
(DE GLCONSTVAL (X)
(COND ((OR (NULL X)
	   (EQ X T)
	   (NUMBERP X))
       X)
      ((AND (PAIRP X)
	    (EQ (CAR X)
		'QUOTE))
       (CADR X))
      ((PAIRP X)
       (COND ((AND (MEMQ (CAR X)
			 '(COPY APPEND))
		   (PAIRP (CADR X))
		   (EQ (CAADR X)
		       'QUOTE)
		   (OR (NULL (CDDR X))
		       (NULL (CADDR X))))
	      (CADADR X))
	     ((EQ (CAR X)
		  'LIST)
	      (MAPCAR (CDR X)
		      (FUNCTION GLCONSTVAL)))
	     ((EQ (CAR X)
		  'CONS)
	      (CONS (GLCONSTVAL (CADR X))
		    (GLCONSTVAL (CADDR X))))
	     (T (ERROR 0 NIL))))
      ((AND (ATOM X)
	    (GET X 'GLISPCONSTANTFLG))
       (GET X 'GLISPCONSTANTVAL))
      (T (ERROR 0 NIL))))


% edited:  5-OCT-82 15:23 
(DE GLCP (FN)
(SETQ FN (OR FN GLLASTFNCOMPILED))(COND ((NOT (GLGETD FN))
					 (PRIN1 FN)
					 (PRIN1 " ?")
					 (TERPRI))
					(T (GLCOMPILE FN)
					   (GLP FN))))


% GSN 28-JAN-83 09:29 
% edited:  1-Jun-81 16:02 
% Process a declaration list from a GLAMBDA expression. Each element 
%   of the list is of the form <var>, <var>:<str-descr>, :<str-descr>, 
%   or <var>: (A <str-descr>) or (A <str-descr>) . Forms without a 
%   variable are accepted only if NOVAROK is true. If VALOK is true, a 
%   PROG form (variable value) is allowed. The result is a list of 
%   variable names. 
(DE GLDECL (LST FLGS GLTOPCTX FN ARGTYPES)
(PROG (RESULT FIRST SECOND THIRD TOP TMP EXPR VARS STR NOVAROK VALOK)
      (SETQ NOVAROK (CAR FLGS))
      (SETQ VALOK (CADR FLGS))
      (COND ((NULL GLTOPCTX)
	     (ERROR 0 NIL)))
      A
      
% Get the next variable/description from LST 

      (COND ((NULL LST)
	     (SETQ ARGTYPES NIL)
	     (SETQ CONTEXT GLTOPCTX)
	     (MAPC (CAR GLTOPCTX)
		   (FUNCTION (LAMBDA (S)
			       (SETQ ARGTYPES (CONS (GLEVALSTR (CADDR S)
							       GLTOPCTX)
						    ARGTYPES))
			       (RPLACA (CDDR S)
				       (CAR ARGTYPES)))))
	     (SETQ RESULT (REVERSIP RESULT))
	     (COND (FN (PUT FN 'GLARGUMENTTYPES
			    ARGTYPES)))
	     (RETURN RESULT)))
      (SETQ TOP (pop LST))
      (COND ((NOT (ATOM TOP))
	     (GO B)))
      (SETQ VARS NIL)
      (SETQ STR NIL)
      (GLSEPINIT TOP)
      (SETQ FIRST (GLSEPNXT))
      (SETQ SECOND (GLSEPNXT))
      (COND ((EQ FIRST ':)
	     (COND ((NULL SECOND)
		    (COND ((AND NOVAROK LST (GLOKSTR? (CAR LST)))
			   (GLDECLDS (GLMKVAR)
				     (pop LST))
			   (GO A))
			  (T (GO E))))
		   ((AND NOVAROK (GLOKSTR? SECOND)
			 (NULL (GLSEPNXT)))
		    (GLDECLDS (GLMKVAR)
			      SECOND)
		    (GO A))
		   (T (GO E)))))
      D
      
% At least one variable name has been found. Collect other variable 
%   names until a <type> is found. 

      (SETQ VARS (ACONC VARS FIRST))
      (COND ((NULL SECOND)
	     (GO C))
	    ((EQ SECOND ':)
	     (COND ((AND (SETQ THIRD (GLSEPNXT))
			 (GLOKSTR? THIRD)
			 (NULL (GLSEPNXT)))
		    (SETQ STR THIRD)
		    (GO C))
		   ((AND (NULL THIRD)
			 (GLOKSTR? (CAR LST)))
		    (SETQ STR (pop LST))
		    (GO C))
		   (T (GO E))))
	    ((EQ SECOND '!,)
	     (COND ((SETQ FIRST (GLSEPNXT))
		    (SETQ SECOND (GLSEPNXT))
		    (GO D))
		   ((ATOM (CAR LST))
		    (GLSEPINIT (pop LST))
		    (SETQ FIRST (GLSEPNXT))
		    (SETQ SECOND (GLSEPNXT))
		    (GO D))))
	    (T (GO E)))
      C
      
% Define the <type> for each variable on VARS. 

      (MAPC VARS (FUNCTION (LAMBDA (X)
			     (GLDECLDS X STR))))
      (GO A)
      B
      
% The top of LST is non-atomic. Must be either (A <type>) or 
%   (<var> <value>) . 

      (COND ((AND (GL-A-AN? (CAR TOP))
		  NOVAROK
		  (GLOKSTR? TOP))
	     (GLDECLDS (GLMKVAR)
		       TOP))
	    ((AND VALOK (NOT (GL-A-AN? (CAR TOP)))
		  (ATOM (CAR TOP))
		  (CDR TOP))
	     (SETQ EXPR (CDR TOP))
	     (SETQ TMP (GLDOEXPR NIL GLTOPCTX T))
	     (COND (EXPR (GO E)))
	     (GLADDSTR (CAR TOP)
		       NIL
		       (CADR TMP)
		       GLTOPCTX)
	     (SETQ RESULT (CONS (LIST (CAR TOP)
				      (CAR TMP))
				RESULT)))
	    ((AND NOVAROK (GLOKSTR? TOP))
	     (GLDECLDS (GLMKVAR)
		       TOP))
	    (T (GO E)))
      (GO A)
      E
      (GLERROR 'GLDECL
	       (LIST "Bad argument structure" LST))
      (RETURN NIL)))


% GSN 26-JAN-83 13:17 
% edited:  2-Jan-81 13:39 
% Add ATM to the RESULT list of GLDECL, and declare its structure. 
(DE GLDECLDS (ATM STR)
(PROG NIL 
% If a substitution exists for this type, use it. 

      (COND (ARGTYPES (SETQ STR (pop ARGTYPES)))
	    (GLTYPESUBS (SETQ STR (GLSUBSTTYPE STR GLTYPESUBS))))
      (SETQ RESULT (CONS ATM RESULT))
      (GLADDSTR ATM NIL STR GLTOPCTX)))


% GSN 26-JAN-83 10:28 
% Declare variables and types in top of CONTEXT. 
(DE GLDECLS (VARS TYPES CONTEXT)
(PROG NIL A (COND ((NULL VARS)
		   (RETURN NIL)))
      (GLADDSTR (CAR VARS)
		NIL
		(CAR TYPES)
		CONTEXT)
      (SETQ VARS (CDR VARS))
      (SETQ TYPES (CDR TYPES))
      (GO A)))


% edited: 19-MAY-82 13:33 
% Define the result types for a list of functions. The format of the 
%   argument is a list of dotted pairs, (FN . TYPE) 
(DE GLDEFFNRESULTTYPES (LST)
(MAPC LST (FUNCTION (LAMBDA (X)
		      (MAPC (CADR X)
			    (FUNCTION (LAMBDA (Y)
					(PUT Y 'GLRESULTTYPE
					     (CAR X)))))))))


% edited: 19-MAY-82 13:05 
% Define the result type functions for a list of functions. The format 
%   of the argument is a list of dotted pairs, (FN . TYPEFN) 
(DE GLDEFFNRESULTTYPEFNS (LST)
(MAPC LST (FUNCTION (LAMBDA (X)
		      (PUT (CAR X)
			   'GLRESULTTYPEFN
			   (CDR X))))))


% GSN  2-MAR-83 10:14 
% Define properties for an object type. Each property is of the form 
%   (<propname> (<definition>) <properties>) 
(DE GLDEFPROP (OBJECT PROP LST)
(PROG (LSTP)
      (MAPC LST (FUNCTION (LAMBDA (X)
			    (COND
			      ((NOT (OR (EQ PROP 'DOC)
					(AND (EQ PROP 'SUPERS)
					     (ATOM X))
					(AND (PAIRP X)
					     (ATOM (CAR X))
					     (CDR X))))
				(PRIN1 "GLDEFPROP: For object ")
				(PRIN1 OBJECT)
				(PRIN1 " the ")
				(PRIN1 PROP)
				(PRIN1 " property ")
				(PRIN1 X)
				(PRIN1 " has bad form.")
				(TERPRI)
				(PRIN1 "This property was ignored.")
				(TERPRI))
			      (T (SETQ LSTP (CONS X LSTP)))))))
      (NCONC (GET OBJECT 'GLSTRUCTURE)
	     (LIST PROP (REVERSIP LSTP)))))


% GSN 10-FEB-83 12:31 
% edited: 17-Sep-81 12:21 
% Process a Structure Description. The format of the argument is the 
%   name of the structure followed by its structure description, 
%   followed by other optional arguments. 
(DE GLDEFSTR (LST SYSTEMFLG)
(PROG (STRNAME STR OLDSTR)
      (SETQ STRNAME (pop LST))
      (COND ((AND (NOT SYSTEMFLG)
		  (MEMQ STRNAME GLBASICTYPES))
	     (PRIN1 "The GLISP type ")
	     (PRIN1 STRNAME)
	     (PRIN1 " may not be redefined by the user.")
	     (TERPRI)
	     (RETURN NIL))
	    ((SETQ OLDSTR (GET STRNAME 'GLSTRUCTURE))
	     (COND ((EQUAL OLDSTR LST)
		    (RETURN NIL))
		   ((NOT GLQUIETFLG)
		    (PRIN1 STRNAME)
		    (PRIN1 " structure redefined.")
		    (TERPRI)))
	     (GLSTRCHANGED STRNAME))
	    ((NOT SYSTEMFLG)
	     NIL))
      (SETQ STR (pop LST))
      (PUT STRNAME 'GLSTRUCTURE
	   (LIST STR))
      (COND ((NOT (GLOKSTR? STR))
	     (PRIN1 STRNAME)
	     (PRIN1 " has faulty structure specification.")
	     (TERPRI)))
      (COND ((NOT (MEMQ STRNAME GLOBJECTNAMES))
	     (SETQ GLOBJECTNAMES (CONS STRNAME GLOBJECTNAMES))))
      
% Process the remaining specifications, if any. Each additional 
%   specification is a list beginning with a keyword. 

      LP
      (COND ((NULL LST)
	     (RETURN NIL)))
      (CASEQ (CAR LST)
	     ((ADJ Adj adj)
	      (GLDEFPROP STRNAME 'ADJ
			 (CADR LST)))
	     ((PROP Prop prop)
	      (GLDEFPROP STRNAME 'PROP
			 (CADR LST)))
	     ((ISA Isa IsA isA isa)
	      (GLDEFPROP STRNAME 'ISA
			 (CADR LST)))
	     ((MSG Msg msg)
	      (GLDEFPROP STRNAME 'MSG
			 (CADR LST)))
	     (T (GLDEFPROP STRNAME (CAR LST)
			   (CADR LST))))
      (SETQ LST (CDDR LST))
      (GO LP)))


% edited: 27-APR-82 11:01 
(DF GLDEFSTRNAMES (LST)
(MAPC LST (FUNCTION (LAMBDA (X)
		      (PROG (TMP)
			    (COND
			      ((SETQ TMP (ASSOC (CAR X)
						GLUSERSTRNAMES))
				(RPLACD TMP (CDR X)))
			      (T (SETQ GLUSERSTRNAMES (ACONC GLUSERSTRNAMES X))
				 )))))))


% GSN 10-FEB-83 11:50 
% Define named structure descriptions. The descriptions are of the 
%   form (<name> <description>) . Each description is put on the 
%   property list of <name> as GLSTRUCTURE 
(DF GLDEFSTRQ (ARGS)
(MAPC ARGS (FUNCTION (LAMBDA (ARG)
		       (GLDEFSTR ARG NIL)))))


% GSN 10-FEB-83 12:13 
% Define named structure descriptions. The descriptions are of the 
%   form (<name> <description>) . Each description is put on the 
%   property list of <name> as GLSTRUCTURE 
(DF GLDEFSYSSTRQ (ARGS)
(MAPC ARGS (FUNCTION (LAMBDA (ARG)
		       (GLDEFSTR ARG T)))))


% edited: 27-MAY-82 13:00 
% This function is called by the user to define a unit package to the 
%   GLISP system. The argument, a unit record, is a list consisting of 
%   the name of a function to test an entity to see if it is a unit of 
%   the units package, the name of the unit package's runtime GET 
%   function, and an ALIST of operations on units and the functions to 
%   perform those operations. Operations include GET, PUT, ISA, ISADJ, 
%   NCONC, REMOVE, PUSH, and POP. 
(DE GLDEFUNITPKG (UNITREC)
(PROG (LST)
      (SETQ LST GLUNITPKGS)
      A
      (COND ((NULL LST)
	     (SETQ GLUNITPKGS (ACONC GLUNITPKGS UNITREC))
	     (RETURN NIL))
	    ((EQ (CAAR LST)
		 (CAR UNITREC))
	     (RPLACA LST UNITREC)))
      (SETQ LST (CDR LST))
      (GO A)))


% GSN 23-JAN-83 15:39 
% Remove the GLISP structure definition for NAME. 
(DE GLDELDEF (NAME TYPE)
(PUT NAME 'GLSTRUCTURE
     NIL))


% edited: 28-NOV-82 15:18 
(DE GLDESCENDANTP (SUBCLASS CLASS)
(PROG (SUPERS)
      (COND ((EQ SUBCLASS CLASS)
	     (RETURN T)))
      (SETQ SUPERS (GLGETSUPERS SUBCLASS))
      LP
      (COND ((NULL SUPERS)
	     (RETURN NIL))
	    ((GLDESCENDANTP (CAR SUPERS)
			    CLASS)
	     (RETURN T)))
      (SETQ SUPERS (CDR SUPERS))
      (GO LP)))


% GSN 25-FEB-83 16:41 
% edited: 25-Jun-81 15:26 
% Function to compile an expression of the form (A <type> ...) 
(DE GLDOA (EXPR)
(PROG (TYPE UNITREC TMP)
      (SETQ TYPE (CADR EXPR))
      (COND ((AND (PAIRP TYPE)
		  (EQ (CAR TYPE)
		      'TYPEOF))
	     (SETQ TYPE (GLGETTYPEOF TYPE))
	     (GLNOTICETYPE TYPE)
	     (RETURN (GLMAKESTR TYPE (CDDR EXPR))))
	    ((GLGETSTR TYPE)
	     (GLNOTICETYPE TYPE)
	     (RETURN (GLMAKESTR TYPE (CDDR EXPR))))
	    ((AND (SETQ UNITREC (GLUNIT? TYPE))
		  (SETQ TMP (ASSOC 'A
				   (CADDR UNITREC))))
	     (RETURN (APPLY (CDR TMP)
			    (LIST EXPR))))
	    (T (GLERROR 'GLDOA
			(LIST "The type" TYPE "is not defined."))))))


% GSN  7-MAR-83 16:54 
% Compile code for Case statement. 
(DE GLDOCASE (EXPR)
(PROG
  (SELECTOR SELECTORTYPE RESULT TMP RESULTTYPE TYPEOK ELSECLAUSE TMPB)
  (SETQ TYPEOK T)
  (SETQ TMP (GLPUSHEXPR (LIST (CADR EXPR))
			NIL CONTEXT T))
  (SETQ SELECTOR (CAR TMP))
  (SETQ SELECTORTYPE (CADR TMP))
  (SETQ EXPR (CDDR EXPR))
  
% Get rid of of if present 

  (COND ((MEMQ (CAR EXPR)
	       '(OF Of of))
	 (SETQ EXPR (CDR EXPR))))
  A
  (COND
    ((NULL EXPR)
     (RETURN (LIST (GLGENCODE (CONS 'SELECTQ
				    (CONS SELECTOR (ACONC RESULT ELSECLAUSE))))
		   RESULTTYPE)))
    ((MEMQ (CAR EXPR)
	   '(ELSE Else
	      else))
     (SETQ TMP (GLPROGN (CDR EXPR)
			CONTEXT))
     (SETQ ELSECLAUSE (COND ((CDAR TMP)
			     (CONS 'PROGN
				   (CAR TMP)))
			    (T (CAAR TMP))))
     (SETQ EXPR NIL))
    (T
      (SETQ TMP (GLPROGN (CDAR EXPR)
			 CONTEXT))
      (SETQ
	RESULT
	(ACONC RESULT
	       (CONS (COND
		       ((ATOM (CAAR EXPR))
			(OR (AND (SETQ TMPB (GLSTRPROP SELECTORTYPE
						       'VALUES
						       (CAAR EXPR)
						       NIL))
				 (CADR TMPB))
			    (CAAR EXPR)))
		       (T (MAPCAR (CAAR EXPR)
				  (FUNCTION
				    (LAMBDA (X)
				      (OR (AND (SETQ TMPB (GLSTRPROP
						   SELECTORTYPE
						   'VALUES
						   X NIL))
					       (CADR TMPB))
					  X))))))
		     (CAR TMP))))))
  
% If all the result types are the same, then we know the result of the 
%   Case statement. 

  (COND (TYPEOK (COND ((NULL RESULTTYPE)
		       (SETQ RESULTTYPE (CADR TMP)))
		      ((EQUAL RESULTTYPE (CADR TMP)))
		      (T (SETQ TYPEOK NIL)
			 (SETQ RESULTTYPE NIL)))))
  (COND (EXPR (SETQ EXPR (CDR EXPR))))
  (GO A)))


% edited: 23-APR-82 14:38 
% Compile a COND expression. 
(DE GLDOCOND (CONDEXPR)
(PROG (RESULT TMP TYPEOK RESULTTYPE)
      (SETQ TYPEOK T)
      A
      (COND ((NULL (SETQ CONDEXPR (CDR CONDEXPR)))
	     (GO B)))
      (SETQ TMP (GLPROGN (CAR CONDEXPR)
			 CONTEXT))
      (COND ((NE (CAAR TMP)
		 NIL)
	     (SETQ RESULT (ACONC RESULT (CAR TMP)))
	     (COND (TYPEOK (COND ((NULL RESULTTYPE)
				  (SETQ RESULTTYPE (CADR TMP)))
				 ((EQUAL RESULTTYPE (CADR TMP)))
				 (T (SETQ RESULTTYPE NIL)
				    (SETQ TYPEOK NIL)))))))
      (COND ((NE (CAAR TMP)
		 T)
	     (GO A)))
      B
      (RETURN (LIST (COND ((AND (NULL (CDR RESULT))
				(EQ (CAAR RESULT)
				    T))
			   (CONS 'PROGN
				 (CDAR RESULT)))
			  (T (CONS 'COND
				   RESULT)))
		    (AND TYPEOK RESULTTYPE)))))


% GSN  4-MAR-83 14:06 
% edited: 23-Sep-81 17:08 
% Compile a single expression. START is set if EXPR is the start of a 
%   new expression, i.e., if EXPR might be a function call. The global 
%   variable EXPR is the expression, CONTEXT the context in which it 
%   is compiled. VALBUSY is T if the value of the expression is needed 
%   outside the expression. The value is a list of the new expression 
%   and its value-description. 
(DE GLDOEXPR (START CONTEXT VALBUSY)
(PROG (FIRST TMP RESULT)
      (SETQ EXPRSTACK (CONS EXPR EXPRSTACK))
      (COND ((NOT (PAIRP EXPR))
	     (GLERROR 'GLDOEXPR
		      (LIST "Expression is not a list."))
	     (GO OUT))
	    ((AND (NOT START)
		  (STRINGP (CAR EXPR)))
	     (GO A))
	    ((OR (NOT (IDP (CAR EXPR)))
		 (NOT START))
	     (GO A)))
      
% Test the initial atom to see if it is a function name. It is assumed 
%   to be a function name if it doesnt contain any GLISP operators and 
%   the following atom doesnt start with a GLISP binary operator. 

      (COND ((AND (EQ GLLISPDIALECT 'INTERLISP)
		  (EQ (CAR EXPR)
		      '*))
	     (SETQ RESULT (LIST EXPR NIL))
	     (GO OUT))
	    ((MEMQ (CAR EXPR)
		   ''Quote)
	     (SETQ FIRST (CAR EXPR))
	     (GO B)))
      (GLSEPINIT (CAR EXPR))
      
% See if the initial atom contains an expression operator. 

      (COND ((NE (SETQ FIRST (GLSEPNXT))
		 (CAR EXPR))
	     (COND ((OR (MEMQ (CAR EXPR)
			      '(APPLY* BLKAPPLY* PACK* PP*))
			(GETDDD (CAR EXPR))
			(GET (CAR EXPR)
			     'MACRO)
			(AND (NE FIRST '~)
			     (GLOPERATOR? FIRST)))
		    (GLSEPCLR)
		    (SETQ FIRST (CAR EXPR))
		    (GO B))
		   (T (GLSEPCLR)
		      (GO A))))
	    ((OR (EQ FIRST '~)
		 (EQ FIRST '-))
	     (GLSEPCLR)
	     (GO A))
	    ((OR (NOT (PAIRP (CDR EXPR)))
		 (NOT (IDP (CADR EXPR))))
	     (GO B)))
      
% See if the initial atom is followed by an expression operator. 

      (GLSEPINIT (CADR EXPR))
      (SETQ TMP (GLSEPNXT))
      (GLSEPCLR)
      (COND ((GLOPERATOR? TMP)
	     (GO A)))
      
% The EXPR is a function reference. Test for system functions. 

      B
      (SETQ RESULT (CASEQ FIRST ('Quote
			   (LIST EXPR (GLCONSTANTTYPE (CADR EXPR))))
			  ((GO Go go)
			   (LIST EXPR NIL))
			  ((PROG Prog prog)
			   (GLDOPROG EXPR CONTEXT))
			  ((FUNCTION Function function)
			   (GLDOFUNCTION EXPR NIL CONTEXT T))
			  ((SETQ Setq setq)
			   (GLDOSETQ EXPR))
			  ((COND Cond cond)
			   (GLDOCOND EXPR))
			  ((RETURN Return return)
			   (GLDORETURN EXPR))
			  ((FOR For for)
			   (GLDOFOR EXPR))
			  ((THE The the)
			   (GLDOTHE EXPR))
			  ((THOSE Those those)
			   (GLDOTHOSE EXPR))
			  ((IF If if)
			   (GLDOIF EXPR CONTEXT))
			  ((A a AN An an)
			   (GLDOA EXPR))
			  ((_ SEND Send send)
			   (GLDOSEND EXPR))
			  ((PROGN PROG2)
			   (GLDOPROGN EXPR))
			  (PROG1 (GLDOPROG1 EXPR CONTEXT))
			  ((SELECTQ CASEQ)
			   (GLDOSELECTQ EXPR CONTEXT))
			  ((WHILE While while)
			   (GLDOWHILE EXPR CONTEXT))
			  ((REPEAT Repeat repeat)
			   (GLDOREPEAT EXPR))
			  ((CASE Case case)
			   (GLDOCASE EXPR))
			  ((MAP MAPLIST MAPCON MAPC MAPCAR MAPCONC MAPCAN)
			   (GLDOMAP EXPR))
			  (T (GLUSERFN EXPR))))
      (GO OUT)
      A
      
% The current EXPR is possibly a GLISP expression. Parse the next 
%   subexpression using GLPARSEXPR. 

      (SETQ RESULT (GLPARSEXPR))
      OUT
      (SETQ EXPRSTACK (CDR EXPRSTACK))
      (RETURN RESULT)))


% GSN  2-MAR-83 17:03 
% edited: 21-Apr-81 11:25 
% Compile code for a FOR loop. 
(DE GLDOFOR (EXPR)
(PROG (DOMAIN DOMAINNAME DTYPE ORIGEXPR LOOPVAR NEWCONTEXT LOOPCONTENTS 
	      SINGFLAG LOOPCOND COLLECTCODE)
      (SETQ ORIGEXPR EXPR)
      (pop EXPR)
      
% Parse the forms (FOR EACH <set> ...) and (FOR <var> IN <set> ...) 

      (COND ((MEMQ (CAR EXPR)
		   '(EACH Each each))
	     (SETQ SINGFLAG T)
	     (pop EXPR))
	    ((AND (ATOM (CAR EXPR))
		  (MEMQ (CADR EXPR)
			'(IN In in)))
	     (SETQ LOOPVAR (pop EXPR))
	     (pop EXPR))
	    (T (GO X)))
      
% Now get the <set> 

      (COND ((NULL (SETQ DOMAIN (GLDOMAIN SINGFLAG)))
	     (GO X)))
      (SETQ DTYPE (GLXTRTYPE (CADR DOMAIN)))
      (COND ((OR (NULL DTYPE)
		 (EQ DTYPE 'ANYTHING))
	     (SETQ DTYPE '(LISTOF ANYTHING)))
	    ((OR (NOT (PAIRP DTYPE))
		 (NE (CAR DTYPE)
		     'LISTOF))
	     (COND ((OR (AND (PAIRP (SETQ DTYPE (GLXTRTYPE (GLGETSTR DTYPE))))
			     (EQ (CAR DTYPE)
				 'LISTOF))
			(NULL DTYPE)))
		   (T (GLERROR 'GLDOFOR
			       (LIST 
			    "Warning: The domain of a FOR loop is of type"
				     DTYPE "which is not a LISTOF type."))
		      (SETQ DTYPE '(LISTOF ANYTHING))))))
      
% Add a level onto the context for the inside of the loop. 

      (SETQ NEWCONTEXT (CONS NIL CONTEXT))
      
% If a loop variable wasnt specified, make one. 

      (OR LOOPVAR (SETQ LOOPVAR (GLMKVAR)))
      (GLADDSTR LOOPVAR (AND SINGFLAG DOMAINNAME)
		(CADR DTYPE)
		NEWCONTEXT)
      
% See if a condition is specified. If so, add it to LOOPCOND. 

      (COND ((MEMQ (CAR EXPR)
		   '(WITH With with))
	     (pop EXPR)
	     (SETQ LOOPCOND (GLPREDICATE (LIST LOOPVAR (CADR DTYPE))
					 NEWCONTEXT NIL NIL)))
	    ((MEMQ (CAR EXPR)
		   '(WHICH Which which WHO Who who THAT That that))
	     (pop EXPR)
	     (SETQ LOOPCOND (GLPREDICATE (LIST LOOPVAR (CADR DTYPE))
					 NEWCONTEXT T T))))
      (COND ((AND EXPR (MEMQ (CAR EXPR)
			     '(when When WHEN)))
	     (pop EXPR)
	     (SETQ LOOPCOND (GLANDFN LOOPCOND (GLDOEXPR NIL NEWCONTEXT T)))))
      (COND ((MEMQ (CAR EXPR)
		   '(collect Collect COLLECT))
	     (pop EXPR)
	     (SETQ COLLECTCODE (GLDOEXPR NIL NEWCONTEXT T)))
	    (T (COND ((MEMQ (CAR EXPR)
			    '(DO Do do))
		      (pop EXPR)))
	       (SETQ LOOPCONTENTS (CAR (GLPROGN EXPR NEWCONTEXT)))))
      (RETURN (GLMAKEFORLOOP LOOPVAR DOMAIN LOOPCONTENTS LOOPCOND COLLECTCODE))
      X
      (RETURN (GLUSERFN ORIGEXPR))))


% GSN 26-JAN-83 10:14 
% Compile a functional expression. TYPES is a list of argument types 
%   which is sent in from outside, e.g. when a mapping function is 
%   compiled. 
(DE GLDOFUNCTION (EXPR ARGTYPES CONTEXT VALBUSY)
(PROG (NEWCODE RESULTTYPE PTR ARGS)
      (COND ((NOT (AND (PAIRP EXPR)
		       (MEMQ (CAR EXPR)
			     ''FUNCTION)))
	     (RETURN (GLPUSHEXPR EXPR T CONTEXT T)))
	    ((ATOM (CADR EXPR))
	     (RETURN (LIST EXPR (GLRESULTTYPE (CADR EXPR)
					      ARGTYPES))))
	    ((NOT (MEMQ (CAADR EXPR)
			'(GLAMBDA LAMBDA)))
	     (GLERROR 'GLDOFUNCTION
		      (LIST "Bad functional form."))))
      (SETQ CONTEXT (CONS NIL CONTEXT))
      (SETQ ARGS (GLDECL (CADADR EXPR)
			 '(T NIL)
			 CONTEXT NIL NIL))
      (SETQ PTR (REVERSIP (CAR CONTEXT)))
      (RPLACA CONTEXT NIL)
      LP
      (COND ((NULL PTR)
	     (GO B)))
      (GLADDSTR (CAAR PTR)
		NIL
		(OR (CADDAR PTR)
		    (CAR ARGTYPES))
		CONTEXT)
      (SETQ PTR (CDR PTR))
      (SETQ ARGTYPES (CDR ARGTYPES))
      (GO LP)
      B
      (SETQ NEWCODE (GLPROGN (CDDADR EXPR)
			     CONTEXT))
      (RETURN (LIST (LIST 'FUNCTION
			  (CONS 'LAMBDA
				(CONS ARGS (CAR NEWCODE))))
		    (CADR NEWCODE)))))


% edited:  4-MAY-82 10:46 
% Process an IF ... THEN expression. 
(DE GLDOIF (EXPR CONTEXT)
(PROG (PRED ACTIONS CONDLIST TYPE TMP OLDCONTEXT)
      (SETQ OLDCONTEXT CONTEXT)
      (pop EXPR)
      A
      (COND ((NULL EXPR)
	     (RETURN (LIST (CONS 'COND
				 CONDLIST)
			   TYPE))))
      (SETQ CONTEXT (CONS NIL OLDCONTEXT))
      (SETQ PRED (GLPREDICATE NIL CONTEXT NIL T))
      (COND ((MEMQ (CAR EXPR)
		   '(THEN Then
			then))
	     (pop EXPR)))
      (SETQ ACTIONS (CONS (CAR PRED)
			  NIL))
      (SETQ TYPE (CADR PRED))
      C
      (SETQ CONDLIST (ACONC CONDLIST ACTIONS))
      B
      (COND ((NULL EXPR)
	     (GO A))
	    ((MEMQ (CAR EXPR)
		   '(ELSEIF ElseIf Elseif elseIf
		      elseif))
	     (pop EXPR)
	     (GO A))
	    ((MEMQ (CAR EXPR)
		   '(ELSE Else
		      else))
	     (pop EXPR)
	     (SETQ ACTIONS (CONS T NIL))
	     (SETQ TYPE 'BOOLEAN)
	     (GO C))
	    ((SETQ TMP (GLDOEXPR NIL CONTEXT T))
	     (ACONC ACTIONS (CAR TMP))
	     (SETQ TYPE (CADR TMP))
	     (GO B))
	    (T (GLERROR 'GLDOIF
			(LIST "IF statement contains bad code."))))))


% edited: 16-DEC-81 15:47 
% Compile a LAMBDA expression for which the ARGTYPES are given. 
(DE GLDOLAMBDA (EXPR ARGTYPES CONTEXT)
(PROG (ARGS NEWEXPR VALBUSY)
      (SETQ ARGS (CADR EXPR))
      (SETQ CONTEXT (CONS NIL CONTEXT))
      LP
      (COND (ARGS (GLADDSTR (CAR ARGS)
			    NIL
			    (CAR ARGTYPES)
			    CONTEXT)
		  (SETQ ARGS (CDR ARGS))
		  (SETQ ARGTYPES (CDR ARGTYPES))
		  (GO LP)))
      (SETQ VALBUSY T)
      (SETQ NEWEXPR (GLPROGN (CDDR EXPR)
			     CONTEXT))
      (RETURN (LIST (CONS 'LAMBDA
			  (CONS (CADR EXPR)
				(CAR NEWEXPR)))
		    (CADR NEWEXPR)))))


% edited: 30-MAY-82 16:12 
% Get a domain specification from the EXPR. If SINGFLAG is set and the 
%   top of EXPR is a simple atom, the atom is made plural and used as 
%   a variable or field name. 
(DE GLDOMAIN (SINGFLAG)
(PROG (NAME FIRST)
      (COND ((MEMQ (CAR EXPR)
		   '(THE The the))
	     (SETQ FIRST (CAR EXPR))
	     (RETURN (GLPARSFLD NIL)))
	    ((ATOM (CAR EXPR))
	     (GLSEPINIT (CAR EXPR))
	     (COND ((EQ (SETQ NAME (GLSEPNXT))
			(CAR EXPR))
		    (pop EXPR)
		    (SETQ DOMAINNAME NAME)
		    (RETURN (COND (SINGFLAG (COND ((MEMQ (CAR EXPR)
							 '(OF Of of))
						   (SETQ FIRST 'THE)
						   (SETQ EXPR
							 (CONS (GLPLURAL
								 NAME)
							       EXPR))
						   (GLPARSFLD NIL))
						  (T (GLIDNAME (GLPLURAL
								 NAME)
							       NIL))))
				  (T (GLIDNAME NAME NIL)))))
		   (T (GLSEPCLR)
		      (RETURN (GLDOEXPR NIL CONTEXT T)))))
	    (T (RETURN (GLDOEXPR NIL CONTEXT T))))))


% edited: 29-DEC-82 14:50 
% Compile code for MAP functions. MAPs are treated specially so that 
%   types can be propagated. 
(DE GLDOMAP (EXPR)
(PROG (MAPFN MAPSET SETTYPE MAPCODE NEWCODE RESULTTYPE ITEMTYPE)
      (SETQ MAPFN (CAR EXPR))
      (SETQ EXPR (CDR EXPR))
      (PROGN (SETQ MAPSET (GLDOEXPR NIL CONTEXT T))
	     (COND ((OR (NULL EXPR)
			(CDR EXPR))
		    (GLERROR 'GLDOMAP
			     (LIST "Bad form of mapping function.")))
		   (T (SETQ MAPCODE (CAR EXPR)))))
      (SETQ SETTYPE (GLXTRTYPEB (CADR MAPSET)))
      (COND ((AND (PAIRP SETTYPE)
		  (EQ (CAR SETTYPE)
		      'LISTOF))
	     (SETQ ITEMTYPE (CASEQ MAPFN ((MAP MAPLIST MAPCON)
				    SETTYPE)
				   ((MAPC MAPCAR MAPCONC MAPCAN)
				    (CADR SETTYPE))
				   (T (ERROR 0 NIL))))))
      (SETQ NEWCODE (GLDOFUNCTION MAPCODE (LIST ITEMTYPE)
				  CONTEXT
				  (MEMQ MAPFN
					'(MAPLIST MAPCON MAPCAR MAPCONC MAPCAN)
					)))
      (SETQ RESULTTYPE (CASEQ MAPFN ((MAP MAPC)
			       NIL)
			      ((MAPLIST MAPCON MAPCAR MAPCONC MAPCAN)
			       (LIST 'LISTOF
				     (CADR NEWCODE)))
			      (T (ERROR 0 NIL))))
      (RETURN (LIST (GLGENCODE (LIST MAPFN (CAR MAPSET)
				     (CAR NEWCODE)))
		    RESULTTYPE))))


% GSN 10-FEB-83 12:56 
% Attempt to compile code for the sending of a message to an object. 
%   OBJECT is the destination, in the form (<code> <type>) , SELECTOR 
%   is the message selector, and ARGS is a list of arguments of the 
%   form (<code> <type>) . The result is of this form, or NIL if 
%   failure. 
(DE GLDOMSG (OBJECT SELECTOR ARGS)
(PROG (UNITREC TYPE TMP METHOD TRANS FETCHCODE)
      (SETQ TYPE (GLXTRTYPE (CADR OBJECT)))
      (COND ((SETQ METHOD (GLSTRPROP TYPE 'MSG
				     SELECTOR ARGS))
	     (RETURN (GLCOMPMSGL OBJECT 'MSG
				 METHOD ARGS CONTEXT)))
	    ((AND (SETQ UNITREC (GLUNIT? TYPE))
		  (SETQ TMP (ASSOC 'MSG
				   (CADDR UNITREC))))
	     (RETURN (APPLY (CDR TMP)
			    (LIST OBJECT SELECTOR ARGS))))
	    ((SETQ TRANS (GLTRANSPARENTTYPES (CADR OBJECT))))
	    ((AND (MEMQ TYPE '(NUMBER REAL INTEGER))
		  (MEMQ SELECTOR
			'(+ - * / ^ > < >= <=))
		  ARGS
		  (NULL (CDR ARGS))
		  (MEMQ (GLXTRTYPE (CADAR ARGS))
			'(NUMBER REAL INTEGER)))
	     (RETURN (GLREDUCEARITH SELECTOR OBJECT (CAR ARGS))))
	    (T (RETURN NIL)))
      
% See if the message can be handled by a TRANSPARENT subobject. 

      B
      (COND ((NULL TRANS)
	     (RETURN NIL))
	    ((SETQ TMP (GLDOMSG (LIST '*GL*
				      (GLXTRTYPE (CAR TRANS)))
				SELECTOR ARGS))
	     (SETQ FETCHCODE (GLSTRFN (CAR TRANS)
				      (CADR OBJECT)
				      NIL))
	     (GLSTRVAL TMP (CAR FETCHCODE))
	     (GLSTRVAL TMP (CAR OBJECT))
	     (RETURN TMP))
	    ((SETQ TMP (CDR TMP))
	     (GO B)))))


% GSN 26-JAN-83 10:14 
% edited: 17-Sep-81 14:01 
% Compile a PROG expression. 
(DE GLDOPROG (EXPR CONTEXT)
(PROG (PROGLST NEWEXPR RESULT NEXTEXPR TMP RESULTTYPE)
      (pop EXPR)
      (SETQ CONTEXT (CONS NIL CONTEXT))
      (SETQ PROGLST (GLDECL (pop EXPR)
			    '(NIL T)
			    CONTEXT NIL NIL))
      (SETQ CONTEXT (CONS NIL CONTEXT))
      
% Compile the contents of the PROG onto NEWEXPR 

      
% Compile the next expression in a PROG. 

      L
      (COND ((NULL EXPR)
	     (GO X)))
      (SETQ NEXTEXPR (pop EXPR))
      (COND ((ATOM NEXTEXPR)
	     (SETQ NEWEXPR (CONS NEXTEXPR NEWEXPR))
	     
% ***** 

	     
% Set up the context for the label we just found. 

	     (GO L))
	    ((NOT (PAIRP NEXTEXPR))
	     (GLERROR 'GLDOPROG
		      (LIST "PROG contains bad stuff:" NEXTEXPR))
	     (GO L))
	    ((EQ (CAR NEXTEXPR)
		 '*)
	     (SETQ NEWEXPR (CONS NEXTEXPR NEWEXPR))
	     (GO L)))
      (COND ((SETQ TMP (GLPUSHEXPR NEXTEXPR T CONTEXT NIL))
	     (SETQ NEWEXPR (CONS (CAR TMP)
				 NEWEXPR))))
      (GO L)
      X
      (SETQ RESULT (CONS 'PROG
			 (CONS PROGLST (REVERSIP NEWEXPR))))
      (RETURN (LIST RESULT RESULTTYPE))))


% edited:  5-NOV-81 14:31 
% Compile a PROGN in the source program. 
(DE GLDOPROGN (EXPR)
(PROG (RES)
      (SETQ RES (GLPROGN (CDR EXPR)
			 CONTEXT))
      (RETURN (LIST (CONS (CAR EXPR)
			  (CAR RES))
		    (CADR RES)))))


% edited: 25-JAN-82 17:34 
% Compile a PROG1, whose result is the value of its first argument. 
(DE GLDOPROG1 (EXPR CONTEXT)
(PROG (RESULT TMP TYPE TYPEFLG)
      (SETQ EXPR (CDR EXPR))
      A
      (COND ((NULL EXPR)
	     (RETURN (LIST (CONS 'PROG1
				 (REVERSIP RESULT))
			   TYPE)))
	    ((SETQ TMP (GLDOEXPR NIL CONTEXT (NOT TYPEFLG)))
	     (SETQ RESULT (CONS (CAR TMP)
				RESULT))
	     
% Get the result type from the first item of the PROG1. 

	     (COND ((NOT TYPEFLG)
		    (SETQ TYPE (CADR TMP))
		    (SETQ TYPEFLG T)))
	     (GO A))
	    (T (GLERROR 'GLDOPROG1
			(LIST "PROG1 contains bad subexpression."))
	       (pop EXPR)
	       (GO A)))))


% edited: 26-MAY-82 15:12 
(DE GLDOREPEAT (EXPR)
(PROG
  (ACTIONS TMP LABEL)
  (pop EXPR)
  A
  (COND ((MEMQ (CAR EXPR)
	       '(UNTIL Until until))
	 (pop EXPR))
	((AND EXPR (SETQ TMP (GLDOEXPR NIL CONTEXT T)))
	 (SETQ ACTIONS (ACONC ACTIONS (CAR TMP)))
	 (GO A))
	(EXPR (RETURN (GLERROR 'GLDOREPEAT
			       (LIST "REPEAT contains bad subexpression.")))))
  (COND ((OR (NULL EXPR)
	     (NULL (SETQ TMP (GLPREDICATE NIL CONTEXT NIL NIL)))
	     EXPR)
	 (GLERROR 'GLDOREPEAT
		  (LIST "REPEAT contains no UNTIL or bad UNTIL clause"))
	 (SETQ TMP (LIST T 'BOOLEAN))))
  (SETQ LABEL (GLMKLABEL))
  (RETURN
    (LIST (CONS 'PROG
		(CONS NIL (CONS LABEL
				(ACONC ACTIONS
				       (LIST 'COND
					     (LIST (GLBUILDNOT (CAR TMP))
						   (LIST 'GO
							 LABEL)))))))
	  NIL))))


% edited:  7-Apr-81 11:49 
% Compile a RETURN, capturing the type of the result as a type of the 
%   function result. 
(DE GLDORETURN (EXPR)
(PROG (TMP)
      (pop EXPR)
      (COND ((NULL EXPR)
	     (GLADDRESULTTYPE NIL)
	     (RETURN '((RETURN)
		       NIL)))
	    (T (SETQ TMP (GLDOEXPR NIL CONTEXT T))
	       (GLADDRESULTTYPE (CADR TMP))
	       (RETURN (LIST (LIST 'RETURN
				   (CAR TMP))
			     (CADR TMP)))))))


% edited: 26-AUG-82 09:30 
% Compile a SELECTQ. Special treatment is necessary in order to quote 
%   the selectors implicitly. 
(DE GLDOSELECTQ (EXPR CONTEXT)
(PROG (RESULT RESULTTYPE TYPEOK KEY TMP TMPB FN)
      (SETQ FN (CAR EXPR))
      (SETQ RESULT (LIST (CAR (GLPUSHEXPR (LIST (CADR EXPR))
					  NIL CONTEXT T))))
      (SETQ TYPEOK T)
      (SETQ EXPR (CDDR EXPR))
      
% If the selection criterion is constant, do it directly. 

      (COND ((OR (SETQ KEY (NUMBERP (CAR RESULT)))
		 (AND (PAIRP (CAR RESULT))
		      (EQ (CAAR RESULT)
			  'QUOTE)
		      (SETQ KEY (CADAR RESULT))))
	     (SETQ TMP (SOME EXPR (FUNCTION (LAMBDA (X)
					      (COND
						((ATOM (CAR X))
						  (EQUAL KEY (CAR X)))
						((PAIRP (CAR X))
						  (MEMBER KEY (CAR X)))
						(T NIL))))))
	     (COND ((OR (NULL TMP)
			(NULL (CDR TMP)))
		    (SETQ TMPB (GLPROGN (LASTPAIR EXPR)
					CONTEXT)))
		   (T (SETQ TMPB (GLPROGN (CDAR TMP)
					  CONTEXT))))
	     (RETURN (LIST (CONS 'PROGN
				 (CAR TMPB))
			   (CADR TMPB)))))
      A
      (COND ((NULL EXPR)
	     (RETURN (LIST (GLGENCODE (CONS FN RESULT))
			   RESULTTYPE))))
      (SETQ RESULT (ACONC RESULT (COND ((OR (CDR EXPR)
					    (EQ FN 'CASEQ))
					(SETQ TMP (GLPROGN (CDAR EXPR)
							   CONTEXT))
					(CONS (CAAR EXPR)
					      (CAR TMP)))
				       (T (SETQ TMP (GLDOEXPR NIL CONTEXT T))
					  (CAR TMP)))))
      (COND (TYPEOK (COND ((NULL RESULTTYPE)
			   (SETQ RESULTTYPE (CADR TMP)))
			  ((EQUAL RESULTTYPE (CADR TMP)))
			  (T (SETQ TYPEOK NIL)
			     (SETQ RESULTTYPE NIL)))))
      (SETQ EXPR (CDR EXPR))
      (GO A)))


% edited:  4-JUN-82 15:35 
% Compile code for the sending of a message to an object. The syntax 
%   of the message expression is 
%   (_ <object> <selector> <arg1>...<argn>) , where the _ may 
%   optionally be SEND, Send, or send. 
(DE GLDOSEND (EXPRR)
(PROG
  (EXPR OBJECT SELECTOR ARGS TMP FNNAME)
  (SETQ FNNAME (CAR EXPRR))
  (SETQ EXPR (CDR EXPRR))
  (SETQ OBJECT (GLPUSHEXPR (LIST (pop EXPR))
			   NIL CONTEXT T))
  (SETQ SELECTOR (pop EXPR))
  (COND ((OR (NULL SELECTOR)
	     (NOT (IDP SELECTOR)))
	 (RETURN (GLERROR 'GLDOSEND
			  (LIST SELECTOR "is an illegal message Selector.")))))
  
% Collect arguments of the message, if any. 

  A
  (COND
    ((NULL EXPR)
     (COND
       ((SETQ TMP (GLDOMSG OBJECT SELECTOR ARGS))
	(RETURN TMP))
       (T
	 
% No message was defined, so just pass it through and hope one will be 
%   defined by runtime. 

	 (RETURN
	   (LIST (GLGENCODE
		   (CONS FNNAME (CONS (CAR OBJECT)
				      (CONS SELECTOR
					    (MAPCAR ARGS
						    (FUNCTION CAR))))))
		 (CADR OBJECT))))))
    ((SETQ TMP (GLDOEXPR NIL CONTEXT T))
     (SETQ ARGS (ACONC ARGS TMP))
     (GO A))
    (T (GLERROR 'GLDOSEND
		(LIST "A message argument is bad."))))))


% edited:  7-Apr-81 11:52 
% Compile a SETQ expression 
(DE GLDOSETQ (EXPR)
(PROG (VAR)
      (pop EXPR)
      (SETQ VAR (pop EXPR))
      (RETURN (GLDOVARSETQ VAR (GLDOEXPR NIL CONTEXT T)))))


% edited: 20-MAY-82 15:13 
% Process a THE expression in a list. 
(DE GLDOTHE (EXPR)
(PROG (RESULT)
      (SETQ RESULT (GLTHE NIL))
      (COND (EXPR (GLERROR 'GLDOTHE
			   (LIST "Stuff left over at end of The expression." 
				 EXPR))))
      (RETURN RESULT)))


% edited: 20-MAY-82 15:16 
% Process a THE expression in a list. 
(DE GLDOTHOSE (EXPR)
(PROG (RESULT)
      (SETQ EXPR (CDR EXPR))
      (SETQ RESULT (GLTHE T))
      (COND (EXPR (GLERROR 'GLDOTHOSE
			   (LIST "Stuff left over at end of The expression." 
				 EXPR))))
      (RETURN RESULT)))


% edited:  5-MAY-82 15:51 
% Compile code to do a SETQ of VAR to the RHS. If the type of VAR is 
%   unknown, it is set to the type of RHS. 
(DE GLDOVARSETQ (VAR RHS)
(PROG NIL (GLUPDATEVARTYPE VAR (CADR RHS))
      (RETURN (LIST (LIST 'SETQ
			  VAR
			  (CAR RHS))
		    (CADR RHS)))))


% edited:  4-MAY-82 10:46 
(DE GLDOWHILE (EXPR CONTEXT)
(PROG (ACTIONS TMP LABEL)
      (SETQ CONTEXT (CONS NIL CONTEXT))
      (pop EXPR)
      (SETQ ACTIONS (LIST (CAR (GLPREDICATE NIL CONTEXT NIL T))))
      (COND ((MEMQ (CAR EXPR)
		   '(DO Do do))
	     (pop EXPR)))
      A
      (COND ((AND EXPR (SETQ TMP (GLDOEXPR NIL CONTEXT T)))
	     (SETQ ACTIONS (ACONC ACTIONS (CAR TMP)))
	     (GO A))
	    (EXPR (GLERROR 'GLDOWHILE
			   (LIST "Bad stuff in While statement:" EXPR))
		  (pop EXPR)
		  (GO A)))
      (SETQ LABEL (GLMKLABEL))
      (RETURN (LIST (LIST 'PROG
			  NIL LABEL (LIST 'COND
					  (ACONC ACTIONS (LIST 'GO
							       LABEL))))
		    NIL))))


% edited: 23-DEC-82 10:47 
% Produce code to test the two sides for equality. 
(DE GLEQUALFN (LHS RHS)
(PROG
  (TMP LHSTP RHSTP)
  (RETURN
    (COND ((SETQ TMP (GLDOMSG LHS '=
			      (LIST RHS)))
	   TMP)
	  ((SETQ TMP (GLUSERSTROP LHS '=
				  RHS))
	   TMP)
	  (T (SETQ LHSTP (CADR LHS))
	     (SETQ RHSTP (CADR RHS))
	     (LIST (COND ((NULL (CAR RHS))
			  (LIST 'NULL
				(CAR LHS)))
			 ((NULL (CAR LHS))
			  (LIST 'NULL
				(CAR RHS)))
			 (T (GLGENCODE (LIST (COND
					       ((OR (EQ LHSTP 'INTEGER)
						    (EQ RHSTP 'INTEGER))
						'EQP)
					       ((OR (GLATOMTYPEP LHSTP)
						    (GLATOMTYPEP RHSTP))
						'EQ)
					       ((AND (EQ LHSTP 'STRING)
						     (EQ RHSTP 'STRING))
						'STREQUAL)
					       (T 'EQUAL))
					     (CAR LHS)
					     (CAR RHS)))))
		   'BOOLEAN))))))


% edited: 23-SEP-82 11:52 
(DF GLERR (ERREXP)
(PRIN1 "Execution of GLISP error expression: ")(PRINT ERREXP)(ERROR 0 NIL))


% GSN 26-JAN-83 13:42 
% Look through a structure to see if it involves evaluating other 
%   structures to produce a concrete type. 
(DE GLEVALSTR (STR CONTEXT)
(PROG (GLEVALSUBS)
      (GLEVALSTRB STR)
      (RETURN (COND (GLEVALSUBS (GLSUBLIS GLEVALSUBS STR))
		    (T STR)))))


% GSN 30-JAN-83 15:34 
% Find places where substructures need to be evaluated and collect 
%   substitutions for them. 
(DE GLEVALSTRB (STR)
(PROG (TMP EXPR)
      (COND ((ATOM STR)
	     (RETURN NIL))
	    ((NOT (PAIRP STR))
	     (ERROR 0 NIL))
	    ((EQ (CAR STR)
		 'TYPEOF)
	     (SETQ EXPR (CDR STR))
	     (SETQ TMP (GLDOEXPR NIL CONTEXT T))
	     (COND ((CADR TMP)
		    (SETQ GLEVALSUBS (CONS (CONS STR (CADR TMP))
					   GLEVALSUBS)))
		   (T (GLERROR 'GLEVALSTRB
			       (LIST "The evaluated type" STR "was not found.")
			       )))
	     (RETURN NIL))
	    (T (MAPC (CDR STR)
		     (FUNCTION GLEVALSTRB))))))


% GSN 27-JAN-83 13:56 
% If a PROGN occurs within a PROGN, expand it by splicing its contents 
%   into the top-level list. 
(DE GLEXPANDPROGN (LST BUSY PROGFLG)
(PROG (X Y)
      (SETQ Y LST)
      LP
      (SETQ X (CDR Y))
      (COND ((NULL X)
	     (RETURN LST))
	    ((NOT (PAIRP (CAR X)))
	     
% Eliminate non-busy atomic items. 

	     (COND ((AND (NOT PROGFLG)
			 (OR (CDR X)
			     (NOT BUSY)))
		    (RPLACD Y (CDR X))
		    (GO LP))))
	    ((MEMQ (CAAR X)
		   '(PROGN PROG2))
	     
% Expand contained PROGNs in-line. 

	     (COND ((CDDAR X)
		    (RPLACD (LASTPAIR (CAR X))
			    (CDR X))
		    (RPLACD X (CDDAR X))))
	     (RPLACA X (CADAR X)))
	    ((AND (EQ (CAAR X)
		      'PROG)
		  (NULL (CADAR X))
		  (EVERY (CDDAR X)
			 (FUNCTION (LAMBDA (Y)
				     (NOT (ATOM Y)))))
		  (NOT (GLOCCURS 'RETURN
				 (CDDAR X))))
	     
% Expand contained simple PROGs. 

	     (COND ((CDDDAR X)
		    (RPLACD (LASTPAIR (CAR X))
			    (CDR X))
		    (RPLACD X (CDDDAR X))))
	     (RPLACA X (CADDAR X))))
      (SETQ Y (CDR Y))
      (GO LP)))


% edited:  9-JUN-82 12:55 
% Test if EXPR is expensive to compute. 
(DE GLEXPENSIVE? (EXPR)
(COND ((ATOM EXPR)
       NIL)
      ((NOT (PAIRP EXPR))
       (ERROR 0 NIL))
      ((MEMQ (CAR EXPR)
	     '(CDR CDDR CDDDR CDDDDR CAR CAAR CADR CAADR CADDR CADDDR))
       (GLEXPENSIVE? (CADR EXPR)))
      ((AND (EQ (CAR EXPR)
		'PROG1)
	    (NULL (CDDR EXPR)))
       (GLEXPENSIVE? (CADR EXPR)))
      (T T)))


% edited:  2-Jan-81 14:26 
% Find the first entry for variable VAR in the CONTEXT structure. 
(DE GLFINDVARINCTX (VAR CONTEXT)
(AND CONTEXT (OR (ASSOC VAR (CAR CONTEXT))
		 (GLFINDVARINCTX VAR (CDR CONTEXT)))))


% edited: 19-OCT-82 15:19 
% Generate code of the form X. The code generated by the compiler is 
%   transformed, if necessary, for the output dialect. 
(DE GLGENCODE (X)
(GLPSLTRANSFM X))


% edited: 20-Mar-81 15:52 
% Get the value for the entry KEY from the a-list ALST. GETASSOC is 
%   used so that the corresponding PUTASSOC can be generated by 
%   GLPUTFN. 
(DE GLGETASSOC (KEY ALST)
(PROG (TMP)
      (RETURN (AND (SETQ TMP (ASSOC KEY ALST))
		   (CDR TMP)))))


% edited: 30-AUG-82 10:25 
(DE GLGETCONSTDEF (ATM)
(COND ((GET ATM 'GLISPCONSTANTFLG)
       (LIST (KWOTE (GET ATM 'GLISPCONSTANTVAL))
	     (GET ATM 'GLISPCONSTANTTYPE)))
      (T NIL)))


% edited: 30-OCT-81 12:20 
% Get the GLISP object description for NAME for the file package. 
(DE GLGETDEF (NAME TYPE)
(LIST 'GLDEFSTRQ
      (CONS NAME (GET NAME 'GLSTRUCTURE))))


% edited:  5-OCT-82 15:06 
% Find a way to retrieve the FIELD from the structure pointed to by 
%   SOURCE (which may be a variable name, NIL, or a list (CODE DESCR)) 
%   relative to CONTEXT. The result is a list of code to get the field 
%   and the structure description of the resulting field. 
(DE GLGETFIELD (SOURCE FIELD CONTEXT)
(PROG (TMP CTXENTRY CTXLIST)
      (COND ((NULL SOURCE)
	     (GO B))
	    ((ATOM SOURCE)
	     (COND ((SETQ CTXENTRY (GLFINDVARINCTX SOURCE CONTEXT))
		    (COND ((SETQ TMP (GLVALUE SOURCE FIELD (CADDR CTXENTRY)
					      NIL))
			   (RETURN TMP))
			  (T (GLERROR 'GLGETFIELD
				      (LIST "The property" FIELD 
					    "cannot be found for"
					    SOURCE "whose type is"
					    (CADDR CTXENTRY))))))
		   ((SETQ TMP (GLGETFIELD NIL SOURCE CONTEXT))
		    (SETQ SOURCE TMP))
		   ((SETQ TMP (GLGETGLOBALDEF SOURCE))
		    (RETURN (GLGETFIELD TMP FIELD NIL)))
		   ((SETQ TMP (GLGETCONSTDEF SOURCE))
		    (RETURN (GLGETFIELD TMP FIELD NIL)))
		   (T (RETURN (GLERROR 'GLGETFIELD
				       (LIST "The name" SOURCE 
					     "cannot be found.")))))))
      (COND ((PAIRP SOURCE)
	     (COND ((SETQ TMP (GLVALUE (CAR SOURCE)
				       FIELD
				       (CADR SOURCE)
				       NIL))
		    (RETURN TMP))
		   (T (RETURN (GLERROR 'GLGETFIELD
				       (LIST "The property" FIELD 
					     "cannot be found for type"
					     (CADR SOURCE)
					     "in"
					     (CAR SOURCE))))))))
      B
      
% No source is specified. Look for a source in the context. 

      (COND ((NULL CONTEXT)
	     (RETURN NIL)))
      (SETQ CTXLIST (pop CONTEXT))
      C
      (COND ((NULL CTXLIST)
	     (GO B)))
      (SETQ CTXENTRY (pop CTXLIST))
      (COND ((EQ FIELD (CADR CTXENTRY))
	     (RETURN (LIST (CAR CTXENTRY)
			   (CADDR CTXENTRY))))
	    ((NULL (SETQ TMP (GLVALUE (CAR CTXENTRY)
				      FIELD
				      (CADDR CTXENTRY)
				      NIL)))
	     (GO C)))
      (RETURN TMP)))


% edited: 27-MAY-82 13:01 
% Call the appropriate function to compile code to get the indicator 
%   (QUOTE IND') from the item whose description is DES, where DES 
%   describes a unit in a unit package whose record is UNITREC. 
(DE GLGETFROMUNIT (UNITREC IND DES)
(PROG (TMP)
      (COND ((SETQ TMP (ASSOC 'GET
			      (CADDR UNITREC)))
	     (RETURN (APPLY (CDR TMP)
			    (LIST IND DES))))
	    (T (RETURN NIL)))))


% edited: 23-APR-82 16:58 
(DE GLGETGLOBALDEF (ATM)
(COND ((GET ATM 'GLISPGLOBALVAR)
       (LIST ATM (GET ATM 'GLISPGLOBALVARTYPE)))
      (T NIL)))


% edited:  4-JUN-82 15:36 
% Get pairs of <field> = <value>, where the = and , are optional. 
(DE GLGETPAIRS (EXPR)
(PROG (PROP VAL PAIRLIST)
      A
      (COND ((NULL EXPR)
	     (RETURN PAIRLIST))
	    ((NOT (ATOM (SETQ PROP (pop EXPR))))
	     (GLERROR 'GLGETPAIRS
		      (LIST PROP "is not a legal property name.")))
	    ((EQ PROP '!,)
	     (GO A)))
      (COND ((MEMQ (CAR EXPR)
		   '(= _ :=))
	     (pop EXPR)))
      (SETQ VAL (GLDOEXPR NIL CONTEXT T))
      (SETQ PAIRLIST (ACONC PAIRLIST (CONS PROP VAL)))
      (GO A)))


% edited: 23-DEC-81 12:52 
(DE GLGETSTR (DES)
(PROG (TYPE TMP)
      (RETURN (AND (SETQ TYPE (GLXTRTYPE DES))
		   (ATOM TYPE)
		   (SETQ TMP (GET TYPE 'GLSTRUCTURE))
		   (CAR TMP)))))


% edited: 28-NOV-82 15:10 
% Get the superclasses of CLASS. 
(DE GLGETSUPERS (CLASS)
(LISTGET (CDR (GET CLASS 'GLSTRUCTURE))
	 'SUPERS))


% GSN  9-FEB-83 15:28 
% Get the type of an expression. 
(DE GLGETTYPEOF (TYPE)
(PROG (TMP)
      (COND ((SETQ TMP (GLPUSHEXPR (CDR TYPE)
				   NIL CONTEXT T))
	     (RETURN (CADR TMP))))))


% edited: 21-MAY-82 17:01 
% Identify a given name as either a known variable name of as an 
%   implicit field reference. 
(DE GLIDNAME (NAME DEFAULTFLG)
(PROG (TMP)
      (RETURN (COND ((ATOM NAME)
		     (COND ((NULL NAME)
			    (LIST NIL NIL))
			   ((IDP NAME)
			    (COND ((EQ NAME T)
				   (LIST NAME 'BOOLEAN))
				  ((SETQ TMP (GLVARTYPE NAME CONTEXT))
				   (LIST NAME (COND ((EQ TMP '*NIL*)
						     NIL)
						    (T TMP))))
				  ((GLGETFIELD NIL NAME CONTEXT))
				  ((SETQ TMP (GLIDTYPE NAME CONTEXT))
				   (LIST (CAR TMP)
					 (CADDR TMP)))
				  ((GLGETCONSTDEF NAME))
				  ((GLGETGLOBALDEF NAME))
				  (T (COND ((OR (NOT DEFAULTFLG)
						GLCAUTIOUSFLG)
					    (GLERROR 'GLIDNAME
						     (LIST "The name" NAME 
					"cannot be found in this context."))))
				     (LIST NAME NIL))))
			   ((FIXP NAME)
			    (LIST NAME 'INTEGER))
			   ((FLOATP NAME)
			    (LIST NAME 'REAL))
			   (T (GLERROR 'GLIDNAME
				       (LIST NAME "is an illegal name.")))))
		    (T NAME)))))


% edited: 27-MAY-82 13:02 
% Try to identify a name by either its referenced name or its type. 
(DE GLIDTYPE (NAME CONTEXT)
(PROG (CTXLEVELS CTXLEVEL CTXENTRY)
      (SETQ CTXLEVELS CONTEXT)
      LPA
      (COND ((NULL CTXLEVELS)
	     (RETURN NIL)))
      (SETQ CTXLEVEL (pop CTXLEVELS))
      LPB
      (COND ((NULL CTXLEVEL)
	     (GO LPA)))
      (SETQ CTXENTRY (CAR CTXLEVEL))
      (SETQ CTXLEVEL (CDR CTXLEVEL))
      (COND ((OR (EQ (CADR CTXENTRY)
		     NAME)
		 (EQ (CADDR CTXENTRY)
		     NAME)
		 (AND (PAIRP (CADDR CTXENTRY))
		      (GL-A-AN? (CAADDR CTXENTRY))
		      (EQ NAME (CADR (CADDR CTXENTRY)))))
	     (RETURN CTXENTRY)))
      (GO LPB)))


% GSN  4-MAR-83 11:57 
% Initialize things for GLISP 
(DE GLINIT NIL
(PROG NIL
      (SETQ GLSEPBITTBL
	    (MAKEBITTABLE '(: _ + - !' = ~ < > * / !, ^)))
      (SETQ GLUNITPKGS NIL)
      (SETQ GLSEPMINUS NIL)
      (SETQ GLQUIETFLG NIL)
      (SETQ GLSEPATOM NIL)
      (SETQ GLSEPPTR 0)
      (SETQ GLBREAKONERROR NIL)
      (SETQ GLUSERSTRNAMES NIL)
      (SETQ GLTYPESUSED NIL)
      (SETQ GLLASTFNCOMPILED NIL)
      (SETQ GLLASTSTREDITED NIL)
      (SETQ GLCAUTIOUSFLG NIL)
      (MAPC '(EQ NE EQUAL AND
		   OR NOT MEMQ ADD1 SUB1 EQN ASSOC PLUS MINUS TIMES SQRT EXPT 
		      DIFFERENCE QUOTIENT GREATERP GEQ LESSP LEQ CAR CDR CAAR 
		      CADR)
	    (FUNCTION (LAMBDA (X)
			(PUT X 'GLEVALWHENCONST
			     T))))
      (MAPC '(ADD1 SUB1 EQN PLUS MINUS TIMES SQRT EXPT DIFFERENCE QUOTIENT 
		   GREATERP GEQ LESSP LEQ)
	    (FUNCTION (LAMBDA (X)
			(PUT X 'GLARGSNUMBERP
			     T))))
      (GLDEFFNRESULTTYPES '((NUMBER (PLUS MINUS DIFFERENCE TIMES EXPT QUOTIENT 
					  REMAINDER MIN MAX ABS))
			    (INTEGER (LENGTH FIX ADD1 SUB1))
			    (REAL (SQRT LOG EXP SIN COS ATAN ARCSIN ARCCOS 
					ARCTAN ARCTAN2 FLOAT))
			    (BOOLEAN (ATOM NULL EQUAL MINUSP ZEROP GREATERP 
					   LESSP NUMBERP FIXP FLOATP STRINGP 
					   ARRAYP EQ NOT NULL BOUNDP))))
      (GLDEFFNRESULTTYPES '((INTEGER (FLATSIZE FLATSIZE2))
			    (BOOLEAN (EQN NE PAIRP IDP UNBOUNDP))
			    (STRING (SUBSTRING CONCAT))))
      (GLDEFFNRESULTTYPEFNS (APPEND '((CONS . GLLISTRESULTTYPEFN)
				      (LIST . GLLISTRESULTTYPEFN)
				      (NCONC . GLLISTRESULTTYPEFN))
				    '((PNTH . GLNTHRESULTTYPEFN))))
      (GLDEFSYSSTRQ (STRING STRING PROP ((LENGTH ((ADD1 (SIZE self)))
						 RESULT INTEGER))
			    MSG
			    ((+ CONCAT RESULT STRING)))
		    (INTEGER INTEGER SUPERS (NUMBER))
		    (ATOM ATOM PROP ((PNAME ID2STRING RESULT STRING)))
		    (REAL REAL SUPERS (NUMBER)))))


% edited: 26-JUL-82 17:07 
% Look up an instance function of an abstract function name which 
%   takes arguments of the specified types. 
(DE GLINSTANCEFN (FNNAME ARGTYPES)
(PROG (INSTANCES IARGS TMP)
      (OR (SETQ INSTANCES (GET FNNAME 'GLINSTANCEFNS))
	  (RETURN NIL))
      
% Get ultimate data types for arguments. 

      LP
      (COND ((NULL INSTANCES)
	     (RETURN NIL)))
      (SETQ IARGS (GET (CAAR INSTANCES)
		       'GLARGUMENTTYPES))
      (SETQ TMP ARGTYPES)
      
% Match the ultimate types of each argument. 

      LPB
      (COND ((NULL IARGS)
	     (RETURN (CAR INSTANCES)))
	    ((EQUAL (GLXTRTYPEB (CAR IARGS))
		    (GLXTRTYPEB (CAR TMP)))
	     (SETQ IARGS (CDR IARGS))
	     (SETQ TMP (CDR TMP))
	     (GO LPB)))
      (SETQ INSTANCES (CDR INSTANCES))
      (GO LP)))


% GSN  3-FEB-83 14:13 
% Make a new name for an instance of a generic function. 
(DE GLINSTANCEFNNAME (FN)
(PROG (INSTFN N)
      (SETQ N (ADD1 (OR (GET FN 'GLINSTANCEFNNO)
			0)))
      (PUT FN 'GLINSTANCEFNNO
	   N)
      (SETQ INSTFN (IMPLODE (NCONC (EXPLODE FN)
				   (CONS '-
					 (EXPLODE N)))))
      (PUT FN 'GLINSTANCEFNS
	   (CONS INSTFN (GET FN 'GLINSTANCEFNS)))
      (RETURN INSTFN)))


% edited: 30-AUG-82 10:28 
% Define compile-time constants. 
(DF GLISPCONSTANTS (ARGS)
(PROG (TMP EXPR EXPRSTACK FAULTFN)
      (MAPC ARGS (FUNCTION (LAMBDA (ARG)
			     (PUT (CAR ARG)
				  'GLISPCONSTANTFLG
				  T)
			     (PUT (CAR ARG)
				  'GLISPORIGCONSTVAL
				  (CADR ARG))
			     (PUT (CAR ARG)
				  'GLISPCONSTANTVAL
				  (PROGN (SETQ EXPR (LIST (CADR ARG)))
					 (SETQ TMP (GLDOEXPR NIL NIL T))
					 (SET (CAR ARG)
					      (EVAL (CAR TMP)))))
			     (PUT (CAR ARG)
				  'GLISPCONSTANTTYPE
				  (OR (CADDR ARG)
				      (CADR TMP))))))))


% edited: 26-MAY-82 15:30 
% Define compile-time constants. 
(DF GLISPGLOBALS (ARGS)
(MAPC ARGS (FUNCTION (LAMBDA (ARG)
		       (PUT (CAR ARG)
			    'GLISPGLOBALVAR
			    T)
		       (PUT (CAR ARG)
			    'GLISPGLOBALVARTYPE
			    (CADR ARG))))))


% GSN 10-FEB-83 11:51 
% edited:  7-Jan-81 10:48 
% Define named structure descriptions. The descriptions are of the 
%   form (<name> <description>) . Each description is put on the 
%   property list of <name> as GLSTRUCTURE 
(DF GLISPOBJECTS (ARGS)
(MAPC ARGS (FUNCTION (LAMBDA (ARG)
		       (GLDEFSTR ARG NIL)))))


% GSN  4-MAR-83 13:53 
% Test the word ADJ to see if it is a LISP adjective. If so, return 
%   the CONS of the name of the function to test it and the type of 
%   the result. 
(DE GLLISPADJ (ADJ)
(PROG (TMP)
      (RETURN (AND (SETQ TMP (ASSOC (GLUCASE ADJ)
				    '((ATOMIC ATOM ATOM)
				      (NULL NULL NIL)
				      (NIL NULL NIL)
				      (INTEGER FIXP INTEGER)
				      (REAL FLOATP REAL)
				      (BOUND BOUNDP ATOM)
				      (ZERO ZEROP NUMBER)
				      (NUMERIC NUMBERP NUMBER)
				      (NEGATIVE MINUSP NUMBER)
				      (MINUS MINUSP NUMBER))))
		   (CDR TMP)))))


% GSN  4-MAR-83 13:54 
% Test to see if ISAWORD is a LISP ISA word. If so, return the CONS of 
%   the name of the function to test for it and the type of the result 
%   if true. 
(DE GLLISPISA (ISAWORD)
(PROG (TMP)
      (COND ((SETQ TMP (ASSOC (GLUCASE ISAWORD)
			      '((ATOM ATOM ATOM)
				(LIST LISTP (LISTOF ANYTHING))
				(NUMBER NUMBERP NUMBER)
				(INTEGER FIXP INTEGER)
				(SYMBOL LITATOM ATOM)
				(ARRAY ARRAYP ARRAY)
				(STRING STRINGP STRING)
				(BIGNUM BIGP BIGNUM)
				(LITATOM LITATOM ATOM))))
	     (RETURN (CDR TMP))))))


% edited: 12-NOV-82 10:53 
% Compute result types for Lisp functions. 
(DE GLLISTRESULTTYPEFN (FN ARGTYPES)
(PROG (ARG1 ARG2)
      (SETQ ARG1 (GLXTRTYPE (CAR ARGTYPES)))
      (COND ((CDR ARGTYPES)
	     (SETQ ARG2 (GLXTRTYPE (CADR ARGTYPES)))))
      (RETURN (CASEQ FN (CONS (OR (AND (PAIRP ARG2)
				       (COND ((EQ (CAR ARG2)
						  'LIST)
					      (CONS 'LIST
						    (CONS ARG1 (CDR ARG2))))
					     ((AND (EQ (CAR ARG2)
						       'LISTOF)
						   (EQUAL ARG1 (CADR ARG2)))
					      ARG2)))
				  (LIST FN ARGTYPES)))
		     (NCONC (COND ((EQUAL ARG1 ARG2)
				   ARG1)
				  ((AND (PAIRP ARG1)
					(PAIRP ARG2)
					(EQ (CAR ARG1)
					    'LISTOF)
					(EQ (CAR ARG2)
					    'LIST)
					(NULL (CDDR ARG2))
					(EQUAL (CADR ARG1)
					       (CADR ARG2)))
				   ARG1)
				  (T (OR ARG1 ARG2))))
		     (LIST (CONS FN (MAPCAR ARGTYPES (FUNCTION GLXTRTYPE))))
		     (T (ERROR 0 NIL))))))


% GSN 11-JAN-83 14:05 
% Create a function call to retrieve the field IND from a LIST 
%   structure. 
(DE GLLISTSTRFN (IND DES DESLIST)
(PROG (TMP N FNLST)
      (SETQ N 1)
      (SETQ FNLST '((CAR *GL*)
		    (CADR *GL*)
		    (CADDR *GL*)
		    (CADDDR *GL*)))
      (COND ((EQ (CAR DES)
		 'LISTOBJECT)
	     (SETQ N (ADD1 N))
	     (SETQ FNLST (CDR FNLST))))
      C
      (pop DES)
      (COND ((NULL DES)
	     (RETURN NIL))
	    ((NOT (PAIRP (CAR DES))))
	    ((SETQ TMP (GLSTRFN IND (CAR DES)
				DESLIST))
	     (RETURN (GLSTRVAL TMP (COND
				 (FNLST (COPY (CAR FNLST)))
				 (T (LIST 'CAR
					  (GLGENCODE (LIST 'NTH
							   '*GL*
							   N)))))))))
      (SETQ N (ADD1 N))
      (AND FNLST (SETQ FNLST (CDR FNLST)))
      (GO C)))


% edited: 24-AUG-82 17:36 
% Compile code for a FOR loop. 
(DE GLMAKEFORLOOP (LOOPVAR DOMAIN LOOPCONTENTS LOOPCOND COLLECTCODE)
(COND
  ((NULL COLLECTCODE)
   (LIST (GLGENCODE (LIST 'MAPC
			  (CAR DOMAIN)
			  (LIST 'FUNCTION
				(LIST 'LAMBDA
				      (LIST LOOPVAR)
				      (COND (LOOPCOND
					      (LIST 'COND
						    (CONS (CAR LOOPCOND)
							  LOOPCONTENTS)))
					    ((NULL (CDR LOOPCONTENTS))
					     (CAR LOOPCONTENTS))
					    (T (CONS 'PROGN
						     LOOPCONTENTS)))))))
	 NIL))
  (T (LIST (COND
	     (LOOPCOND (GLGENCODE
			 (LIST 'MAPCONC
			       (CAR DOMAIN)
			       (LIST 'FUNCTION
				     (LIST 'LAMBDA
					   (LIST LOOPVAR)
					   (LIST 'AND
						 (CAR LOOPCOND)
						 (LIST 'CONS
						       (CAR COLLECTCODE)
						       NIL)))))))
	     ((AND (PAIRP (CAR COLLECTCODE))
		   (ATOM (CAAR COLLECTCODE))
		   (CDAR COLLECTCODE)
		   (EQ (CADAR COLLECTCODE)
		       LOOPVAR)
		   (NULL (CDDAR COLLECTCODE)))
	      (GLGENCODE (LIST 'MAPCAR
			       (CAR DOMAIN)
			       (LIST 'FUNCTION
				     (CAAR COLLECTCODE)))))
	     (T (GLGENCODE (LIST 'MAPCAR
				 (CAR DOMAIN)
				 (LIST 'FUNCTION
				       (LIST 'LAMBDA
					     (LIST LOOPVAR)
					     (CAR COLLECTCODE)))))))
	   (LIST 'LISTOF
		 (CADR COLLECTCODE))))))


% GSN  1-MAR-83 11:36 
% Compile code to create a structure in response to a statement 
%   (A <structure> WITH <field> = <value> ...) 
(DE GLMAKESTR (TYPE EXPR)
(PROG (PAIRLIST STRDES)
      (COND ((MEMQ (CAR EXPR)
		   '(WITH With with))
	     (pop EXPR)))
      (COND ((NULL (SETQ STRDES (GLGETSTR TYPE)))
	     (GLERROR 'GLMAKESTR
		      (LIST "The type name" TYPE "is not defined."))))
      (COND ((EQ (CAR STRDES)
		 'LISTOF)
	     (RETURN (LIST (CONS 'LIST
				 (MAPCAR EXPR (FUNCTION (LAMBDA (EXPR)
							  (GLDOEXPR NIL 
								   CONTEXT T)))
					 ))
			   TYPE))))
      (SETQ PAIRLIST (GLGETPAIRS EXPR))
      (RETURN (LIST (GLBUILDSTR STRDES PAIRLIST (LIST TYPE))
		    TYPE))))


% GSN  3-FEB-83 12:12 
% Make a virtual type for a view of the original type. 
(DE GLMAKEVTYPE (ORIGTYPE VLIST)
(PROG (SUPER PL PNAME TMP VTYPE)
      (SETQ SUPER (CADR VLIST))
      (SETQ VLIST (CDDR VLIST))
      (COND ((MEMQ (CAR VLIST)
		   '(with With WITH))
	     (SETQ VLIST (CDR VLIST))))
      LP
      (COND ((NULL VLIST)
	     (GO OUT)))
      (SETQ PNAME (CAR VLIST))
      (SETQ VLIST (CDR VLIST))
      (COND ((EQ (CAR VLIST)
		 '=)
	     (SETQ VLIST (CDR VLIST))))
      (SETQ TMP NIL)
      LPB
      (COND ((OR (NULL VLIST)
		 (EQ (CAR VLIST)
		     '!,)
		 (AND (ATOM (CAR VLIST))
		      (CDR VLIST)
		      (EQ (CADR VLIST)
			  '=)))
	     (SETQ PL (CONS (LIST PNAME (REVERSIP TMP))
			    PL))
	     (COND ((AND VLIST (EQ (CAR VLIST)
				   '!,))
		    (SETQ VLIST (CDR VLIST))))
	     (GO LP)))
      (SETQ TMP (CONS (CAR VLIST)
		      TMP))
      (SETQ VLIST (CDR VLIST))
      (GO LPB)
      OUT
      (SETQ VTYPE (GLMKVTYPE))
      (PUT VTYPE 'GLSTRUCTURE
	   (LIST (LIST 'TRANSPARENT
		       ORIGTYPE)
		 'PROP
		 PL
		 'SUPERS
		 (LIST SUPER)))
      (RETURN VTYPE)))


% GSN 25-FEB-83 16:08 
% Test whether an item of type TNEW could be stored into a slot of 
%   type TINTO. 
(DE GLMATCH (TNEW TINTO)
(PROG (TMP RES)
      (RETURN (COND ((OR (EQ TNEW TINTO)
			 (NULL TINTO)
			 (EQ TINTO 'ANYTHING)
			 (AND (MEMQ TNEW '(INTEGER REAL NUMBER))
			      (MEMQ TINTO '(NUMBER ATOM)))
			 (AND (EQ TNEW 'ATOM)
			      (PAIRP TINTO)
			      (EQ (CAR TINTO)
				  'ATOM)))
		     TNEW)
		    ((AND (SETQ TMP (GLXTRTYPEC TNEW))
			  (SETQ RES (GLMATCH TMP TINTO)))
		     RES)
		    ((AND (SETQ TMP (GLXTRTYPEC TINTO))
			  (SETQ RES (GLMATCH TNEW TMP)))
		     RES)
		    (T NIL)))))


% GSN 25-FEB-83 16:03 
% Test whether two types match as an element type and a list type. The 
%   result is the resulting element type. 
(DE GLMATCHL (TELEM TLIST)
(PROG (TMP RES)
      (RETURN (COND ((AND (PAIRP TLIST)
			  (EQ (CAR TLIST)
			      'LISTOF)
			  (GLMATCH TELEM (CADR TLIST)))
		     TELEM)
		    ((AND (SETQ TMP (GLXTRTYPEC TLIST))
			  (SETQ RES (GLMATCHL TELEM TMP))))
		    (T NIL)))))


% edited: 26-MAY-82 15:33 
% Construct the NOT of the argument LHS. 
(DE GLMINUSFN (LHS)
(OR (GLDOMSG LHS 'MINUS
	     NIL)
    (GLUSERSTROP LHS 'MINUS
		 NIL)
    (LIST (GLGENCODE (COND ((NUMBERP (CAR LHS))
			    (MINUS (CAR LHS)))
			   ((EQ (GLXTRTYPE (CADR LHS))
				'INTEGER)
			    (LIST 'IMINUS
				  (CAR LHS)))
			   (T (LIST 'MINUS
				    (CAR LHS)))))
	  (CADR LHS))))


% edited: 11-NOV-82 11:54 
% Make a variable name for GLCOMP functions. 
(DE GLMKATOM (NAME)
(PROG (N NEWATOM)
      LP
      (PUT NAME 'GLISPATOMNUMBER
	   (SETQ N (ADD1 (OR (GET NAME 'GLISPATOMNUMBER)
			     0))))
      (SETQ NEWATOM (IMPLODE (APPEND (EXPLODE NAME)
				     (EXPLODE N))))
      
% If an atom with this name has something on its proplist, try again. 

      (COND ((PROP NEWATOM)
	     (GO LP))
	    (T (RETURN NEWATOM)))))


% edited: 27-MAY-82 11:02 
% Make a variable name for GLCOMP functions. 
(DE GLMKLABEL NIL
(PROG NIL (SETQ GLNATOM (ADD1 GLNATOM))
      (RETURN (IMPLODE (APPEND '(G L L A B E L)
			       (EXPLODE GLNATOM))))))


% edited: 27-MAY-82 11:04 
% Make a variable name for GLCOMP functions. 
(DE GLMKVAR NIL
(PROG NIL (SETQ GLNATOM (ADD1 GLNATOM))
      (RETURN (IMPLODE (APPEND '(G L V A R)
			       (EXPLODE GLNATOM))))))


% edited: 18-NOV-82 11:58 
% Make a virtual type name for GLCOMP functions. 
(DE GLMKVTYPE NIL
(GLMKATOM 'GLVIRTUALTYPE))


% GSN 25-JAN-83 16:47 
% edited:  2-Jun-81 14:18 
% Produce a function to implement the _+ operator. Code is produced to 
%   append the right-hand side to the left-hand side. Note: parts of 
%   the structure provided are used multiple times. 
(DE GLNCONCFN (LHS RHS)
(PROG (LHSCODE LHSDES NCCODE TMP STR)
      (SETQ LHSCODE (CAR LHS))
      (SETQ LHSDES (GLXTRTYPE (CADR LHS)))
      (COND ((EQ LHSDES 'INTEGER)
	     (COND ((EQN (CAR RHS)
			 1)
		    (SETQ NCCODE (LIST 'ADD1
				       LHSCODE)))
		   ((OR (FIXP (CAR RHS))
			(EQ (CADR RHS)
			    'INTEGER))
		    (SETQ NCCODE (LIST 'IPLUS
				       LHSCODE
				       (CAR RHS))))
		   (T (SETQ NCCODE (LIST 'PLUS
					 LHSCODE
					 (CAR RHS))))))
	    ((OR (EQ LHSDES 'NUMBER)
		 (EQ LHSDES 'REAL))
	     (SETQ NCCODE (LIST 'PLUS
				LHSCODE
				(CAR RHS))))
	    ((EQ LHSDES 'BOOLEAN)
	     (SETQ NCCODE (LIST 'OR
				LHSCODE
				(CAR RHS))))
	    ((NULL LHSDES)
	     (SETQ NCCODE (LIST 'NCONC1
				LHSCODE
				(CAR RHS)))
	     (COND ((AND (ATOM LHSCODE)
			 (CADR RHS))
		    (GLUPDATEVARTYPE LHSCODE (LIST 'LISTOF
						   (CADR RHS))))))
	    ((AND (PAIRP LHSDES)
		  (EQ (CAR LHSDES)
		      'LISTOF)
		  (NOT (EQUAL LHSDES (CADR RHS))))
	     (SETQ NCCODE (LIST 'NCONC1
				LHSCODE
				(CAR RHS))))
	    ((SETQ TMP (GLUNITOP LHS RHS 'NCONC))
	     (RETURN TMP))
	    ((SETQ TMP (GLDOMSG LHS '_+
				(LIST RHS)))
	     (RETURN TMP))
	    ((SETQ TMP (GLDOMSG LHS '+
				(LIST RHS)))
	     (SETQ NCCODE (CAR TMP)))
	    ((AND (SETQ STR (GLGETSTR LHSDES))
		  (SETQ TMP (GLNCONCFN (LIST (CAR LHS)
					     STR)
				       RHS)))
	     (RETURN (LIST (CAR TMP)
			   (CADR LHS))))
	    ((SETQ TMP (GLUSERSTROP LHS '_+
				    RHS))
	     (RETURN TMP))
	    ((SETQ TMP (GLREDUCEARITH '+
				      LHS RHS))
	     (SETQ NCCODE (CAR TMP)))
	    (T (RETURN NIL)))
      (RETURN (GLPUTFN LHS (LIST (GLGENCODE NCCODE)
				 LHSDES)
		       T))))


% edited: 23-DEC-82 10:49 
% Produce code to test the two sides for inequality. 
(DE GLNEQUALFN (LHS RHS)
(PROG (TMP)
      (COND ((SETQ TMP (GLDOMSG LHS '~=
				(LIST RHS)))
	     (RETURN TMP))
	    ((SETQ TMP (GLUSERSTROP LHS '~=
				    RHS))
	     (RETURN TMP))
	    ((OR (GLATOMTYPEP (CADR LHS))
		 (GLATOMTYPEP (CADR RHS)))
	     (RETURN (LIST (GLGENCODE (LIST 'NEQ
					    (CAR LHS)
					    (CAR RHS)))
			   'BOOLEAN)))
	    (T (RETURN (LIST (GLGENCODE (LIST 'NOT
					      (CAR (GLEQUALFN LHS RHS))))
			     'BOOLEAN))))))


% GSN  7-MAR-83 16:55 
% If SOURCE represents a variable name, add the TYPE of SOURCE to the 
%   CONTEXT. 
(DE GLNOTESOURCETYPE (SOURCE TYPE ADDISATYPE)
(PROG (TMP)
      (RETURN (COND (ADDISATYPE (COND ((ATOM (CAR SOURCE))
				       (GLADDSTR (CAR SOURCE)
						 NIL TYPE CONTEXT))
				      ((AND (PAIRP (CAR SOURCE))
					    (MEMQ (CAAR SOURCE)
						  '(SETQ PROG1))
					    (ATOM (CADAR SOURCE)))
				       (GLADDSTR (CADAR SOURCE)
						 (COND ((SETQ
							  TMP
							  (GLFINDVARINCTX
							    (CAR SOURCE)
							    CONTEXT))
							(CADR TMP)))
						 TYPE CONTEXT))))))))


% edited:  3-MAY-82 14:35 
% Construct the NOT of the argument LHS. 
(DE GLNOTFN (LHS)
(OR (GLDOMSG LHS '~
	     NIL)
    (GLUSERSTROP LHS '~
		 NIL)
    (LIST (GLBUILDNOT (CAR LHS))
	  'BOOLEAN)))


% GSN 28-JAN-83 09:39 
% Add TYPE to the global variable GLTYPESUSED if not already there. 
(DE GLNOTICETYPE (TYPE)
(COND ((NOT (MEMQ TYPE GLTYPESUSED))
       (SETQ GLTYPESUSED (CONS TYPE GLTYPESUSED)))))


% edited: 23-JUN-82 14:31 
% Compute the result type for the function NTH. 
(DE GLNTHRESULTTYPEFN (FN ARGTYPES)
(PROG (TMP)
      (RETURN (COND ((AND (PAIRP (SETQ TMP (GLXTRTYPE (CAR ARGTYPES))))
			  (EQ (CAR TMP)
			      'LISTOF))
		     (CAR ARGTYPES))
		    (T NIL)))))


% edited:  3-JUN-82 11:02 
% See if X occurs in STR, using EQ. 
(DE GLOCCURS (X STR)
(COND ((EQ X STR)
       T)
      ((NOT (PAIRP STR))
       NIL)
      (T (OR (GLOCCURS X (CAR STR))
	     (GLOCCURS X (CDR STR))))))


% GSN 30-JAN-83 15:35 
% Check a structure description for legality. 
(DE GLOKSTR? (STR)
(COND ((NULL STR)
       NIL)
      ((ATOM STR)
       T)
      ((AND (PAIRP STR)
	    (ATOM (CAR STR)))
       (CASEQ (CAR STR)
	      ((A AN a an An)
	       (COND ((CDDR STR)
		      NIL)
		     ((OR (GLGETSTR (CADR STR))
			  (GLUNIT? (CADR STR))
			  (COND (GLCAUTIOUSFLG (PRIN1 "The structure ")
					       (PRIN1 (CADR STR))
					       (PRIN1 
				   " is not currently defined.  Accepted.")
					       (TERPRI)
					       T)
				(T T))))))
	      (CONS (AND (CDR STR)
			 (CDDR STR)
			 (NULL (CDDDR STR))
			 (GLOKSTR? (CADR STR))
			 (GLOKSTR? (CADDR STR))))
	      ((LIST OBJECT ATOMOBJECT LISTOBJECT)
	       (AND (CDR STR)
		    (EVERY (CDR STR)
			   (FUNCTION GLOKSTR?))))
	      (RECORD (COND ((AND (CDR STR)
				  (ATOM (CADR STR)))
			     (pop STR)))
		      (AND (CDR STR)
			   (EVERY (CDR STR)
				  (FUNCTION (LAMBDA (X)
					      (AND (ATOM (CAR X))
						   (GLOKSTR? (CADR X))))))))
	      (LISTOF (AND (CDR STR)
			   (NULL (CDDR STR))
			   (GLOKSTR? (CADR STR))))
	      ((ALIST PROPLIST)
	       (AND (CDR STR)
		    (EVERY (CDR STR)
			   (FUNCTION (LAMBDA (X)
				       (AND (ATOM (CAR X))
					    (GLOKSTR? (CADR X))))))))
	      (ATOM (GLATMSTR? STR))
	      (TYPEOF T)
	      (T (COND ((AND (CDR STR)
			     (NULL (CDDR STR)))
			(GLOKSTR? (CADR STR)))
		       ((ASSOC (CAR STR)
			       GLUSERSTRNAMES))
		       (T NIL)))))
      (T NIL)))


% edited: 30-DEC-81 16:41 
% Get the next operand from the input list, EXPR (global) . The 
%   operand may be an atom (possibly containing operators) or a list. 
(DE GLOPERAND NIL
(PROG NIL (COND ((SETQ FIRST (GLSEPNXT))
		 (RETURN (GLPARSNFLD)))
		((NULL EXPR)
		 (RETURN NIL))
		((STRINGP (CAR EXPR))
		 (RETURN (LIST (pop EXPR)
			       'STRING)))
		((ATOM (CAR EXPR))
		 (GLSEPINIT (pop EXPR))
		 (SETQ FIRST (GLSEPNXT))
		 (RETURN (GLPARSNFLD)))
		(T (RETURN (GLPUSHEXPR (pop EXPR)
				       T CONTEXT T))))))


% GSN  4-MAR-83 14:26 
% Test if an atom is a GLISP operator 
(DE GLOPERATOR? (ATM)
(MEMQ ATM
      '(_ := __ + - * / > < >=
	  <= ^ _+
	    +_ _-
	    -_ = ~= <> AND And and OR Or or __+
					    __-
					    _+_)))


% edited: 26-DEC-82 15:48 
% OR operator 
(DE GLORFN (LHS RHS)
(COND ((AND (PAIRP (CADR LHS))
	    (EQ (CAADR LHS)
		'LISTOF)
	    (EQUAL (CADR LHS)
		   (CADR RHS)))
       (LIST (LIST 'UNION
		   (CAR LHS)
		   (CAR RHS))
	     (CADR LHS)))
      ((GLDOMSG LHS 'OR
		(LIST RHS)))
      ((GLUSERSTROP LHS 'OR
		    RHS))
      (T (LIST (LIST 'OR
		     (CAR LHS)
		     (CAR RHS))
	       (COND ((EQUAL (GLXTRTYPE (CADR LHS))
			     (GLXTRTYPE (CADR RHS)))
		      (CADR LHS))
		     (T NIL))))))


% GSN 10-FEB-83 16:13 
% Remove unwanted system properties from LST for making an output 
%   file. 
(DE GLOUTPUTFILTER (PROPTYPE LST)
(COND
  ((MEMQ PROPTYPE '(PROP ADJ ISA MSG))
   (MAPCAN
     LST
     (FUNCTION
       (LAMBDA (L)
	 (COND
	   ((LISTGET (CDDR L)
		     'SPECIALIZATION)
	     NIL)
	   (T (LIST (CONS (CAR L)
			  (CONS (CADR L)
				(MAPCON (CDDR L)
					(FUNCTION (LAMBDA (PAIR)
						    (COND
						      ((MEMQ (CAR PAIR)
							     '(VTYPE))
							NIL)
						      (T (LIST (CAR PAIR)
							       (CADR PAIR))))))
					(FUNCTION CDDR)))))))))))
  (T LST)))


% edited: 22-SEP-82 17:16 
% Subroutine of GLDOEXPR to parse a GLISP expression containing field 
%   specifications and/or operators. The global variable EXPR is used, 
%   and is modified to reflect the amount of the expression which has 
%   been parsed. 
(DE GLPARSEXPR NIL
(PROG (OPNDS OPERS FIRST LHSP RHSP)
      
% Get the initial part of the expression, i.e., variable or field 
%   specification. 

      L
      (SETQ OPNDS (CONS (GLOPERAND)
			OPNDS))
      M
      (COND ((NULL FIRST)
	     (COND ((OR (NULL EXPR)
			(NOT (ATOM (CAR EXPR))))
		    (GO B)))
	     (GLSEPINIT (CAR EXPR))
	     (COND
	       ((GLOPERATOR? (SETQ FIRST (GLSEPNXT)))
		(pop EXPR)
		(GO A))
	       ((MEMQ FIRST '(IS Is is HAS Has has))
		(COND
		  ((AND OPERS (GREATERP (GLPREC (CAR OPERS))
					5))
		   (GLREDUCE)
		   (SETQ FIRST NIL)
		   (GO M))
		  (T (SETQ OPNDS
			   (CONS (GLPREDICATE
				   (pop OPNDS)
				   CONTEXT T
				   (AND (NOT (UNBOUNDP 'ADDISATYPE))
					ADDISATYPE))
				 OPNDS))
		     (SETQ FIRST NIL)
		     (GO M))))
	       (T (GLSEPCLR)
		  (GO B))))
	    ((GLOPERATOR? FIRST)
	     (GO A))
	    (T (GLERROR 'GLPARSEXPR
			(LIST FIRST 
			     "appears illegally or cannot be interpreted."))))
      
% FIRST now contains an operator 

      A
      
% While top operator < top of stack in precedence, reduce. 

      (COND ((NOT (OR (NULL OPERS)
		      (LESSP (SETQ LHSP (GLPREC (CAR OPERS)))
			     (SETQ RHSP (GLPREC FIRST)))
		      (AND (EQN LHSP RHSP)
			   (MEMQ FIRST '(_ ^ :=)))))
	     (GLREDUCE)
	     (GO A)))
      
% Push new operator onto the operator stack. 

      (SETQ OPERS (CONS FIRST OPERS))
      (GO L)
      B
      (COND (OPERS (GLREDUCE)
		   (GO B)))
      (RETURN (CAR OPNDS))))


% edited: 30-DEC-82 10:55 
% Parse a field specification of the form var:field:field... Var may 
%   be missing, and there may be zero or more fields. The variable 
%   FIRST is used globally; it contains the first atom of the group on 
%   entry, and the next atom on exit. 
(DE GLPARSFLD (PREV)
(PROG (FIELD TMP)
      (COND ((NULL PREV)
	     (COND ((EQ FIRST '!')
		    (COND ((SETQ TMP (GLSEPNXT))
			   (SETQ FIRST (GLSEPNXT))
			   (RETURN (LIST (KWOTE TMP)
					 'ATOM)))
			  (EXPR (SETQ FIRST NIL)
				(SETQ TMP (pop EXPR))
				(RETURN (LIST (KWOTE TMP)
					      (GLCONSTANTTYPE TMP))))
			  (T (RETURN NIL))))
		   ((MEMQ FIRST '(THE The the))
		    (SETQ TMP (GLTHE NIL))
		    (SETQ FIRST NIL)
		    (RETURN TMP))
		   ((NE FIRST ':)
		    (SETQ PREV FIRST)
		    (SETQ FIRST (GLSEPNXT))))))
      A
      (COND ((EQ FIRST ':)
	     (COND ((SETQ FIELD (GLSEPNXT))
		    (SETQ PREV (GLGETFIELD PREV FIELD CONTEXT))
		    (SETQ FIRST (GLSEPNXT))
		    (GO A))))
	    (T (RETURN (COND ((EQ PREV '*NIL*)
			      (LIST NIL NIL))
			     (T (GLIDNAME PREV T))))))))


% edited: 20-MAY-82 11:30 
% Parse a field specification which may be preceded by a ~. 
(DE GLPARSNFLD NIL
(PROG (TMP UOP)
      (COND ((OR (EQ FIRST '~)
		 (EQ FIRST '-))
	     (SETQ UOP FIRST)
	     (COND ((SETQ FIRST (GLSEPNXT))
		    (SETQ TMP (GLPARSFLD NIL)))
		   ((AND EXPR (ATOM (CAR EXPR)))
		    (GLSEPINIT (pop EXPR))
		    (SETQ FIRST (GLSEPNXT))
		    (SETQ TMP (GLPARSFLD NIL)))
		   ((AND EXPR (PAIRP (CAR EXPR)))
		    (SETQ TMP (GLPUSHEXPR (pop EXPR)
					  T CONTEXT T)))
		   (T (RETURN (LIST UOP NIL))))
	     (RETURN (COND ((EQ UOP '~)
			    (GLNOTFN TMP))
			   (T (GLMINUSFN TMP)))))
	    (T (RETURN (GLPARSFLD NIL))))))


% edited: 27-MAY-82 10:42 
% Form the plural of a given word. 
(DE GLPLURAL (WORD)
(PROG (TMP LST UCASE ENDING)
      (COND ((SETQ TMP (GET WORD 'PLURAL))
	     (RETURN TMP)))
      (SETQ LST (REVERSIP (EXPLODE WORD)))
      (SETQ UCASE (U-CASEP (CAR LST)))
      (COND ((AND (MEMQ (CAR LST)
			'(Y y))
		  (NOT (MEMQ (CADR LST)
			     '(A a E e O o U u))))
	     (SETQ LST (CDR LST))
	     (SETQ ENDING (OR (AND UCASE '(S E I))
			      '(s e i))))
	    ((MEMQ (CAR LST)
		   '(S s X x))
	     (SETQ ENDING (OR (AND UCASE '(S E))
			      '(s e))))
	    (T (SETQ ENDING (OR (AND UCASE '(S))
				'(s)))))
      (RETURN (IMPLODE (REVERSIP (APPEND ENDING LST))))))


% edited: 29-DEC-82 12:40 
% Produce a function to implement the -_ (pop) operator. Code is 
%   produced to remove one element from the right-hand side and assign 
%   it to the left-hand side. 
(DE GLPOPFN (LHS RHS)
(PROG (RHSCODE RHSDES POPCODE GETCODE TMP STR)
      (SETQ RHSCODE (CAR RHS))
      (SETQ RHSDES (GLXTRTYPE (CADR RHS)))
      (COND ((AND (PAIRP RHSDES)
		  (EQ (CAR RHSDES)
		      'LISTOF))
	     (SETQ POPCODE (GLPUTFN RHS (LIST (LIST 'CDR
						    RHSCODE)
					      RHSDES)
				    T))
	     (SETQ GETCODE (GLPUTFN LHS (LIST (LIST 'CAR
						    (CAR RHS))
					      (CADR RHSDES))
				    NIL)))
	    ((EQ RHSDES 'BOOLEAN)
	     (SETQ POPCODE (GLPUTFN RHS '(NIL NIL)
				    NIL))
	     (SETQ GETCODE (GLPUTFN LHS RHS NIL)))
	    ((SETQ TMP (GLDOMSG RHS '-_
				(LIST LHS)))
	     (RETURN TMP))
	    ((AND (SETQ STR (GLGETSTR RHSDES))
		  (SETQ TMP (GLPOPFN LHS (LIST (CAR RHS)
					       STR))))
	     (RETURN TMP))
	    ((SETQ TMP (GLUSERSTROP RHS '-_
				    LHS))
	     (RETURN TMP))
	    ((OR (GLATOMTYPEP RHSDES)
		 (AND (NE RHSDES 'ANYTHING)
		      (MEMQ (GLXTRTYPEB RHSDES)
			    GLBASICTYPES)))
	     (RETURN NIL))
	    (T 
% If all else fails, assume a list. 

	       (SETQ POPCODE (GLPUTFN RHS (LIST (LIST 'CDR
						      RHSCODE)
						RHSDES)
				      T))
	       (SETQ GETCODE (GLPUTFN LHS (LIST (LIST 'CAR
						      (CAR RHS))
						(CADR RHSDES))
				      NIL))))
      (RETURN (LIST (LIST 'PROG1
			  (CAR GETCODE)
			  (CAR POPCODE))
		    (CADR GETCODE)))))


% edited: 30-OCT-82 14:36 
% Precedence numbers for operators 
(DE GLPREC (OP)
(PROG (TMP)
      (COND ((SETQ TMP (ASSOC OP '((_ . 1)
				   (:= . 1)
				   (__ . 1)
				   (_+ . 2)
				   (__+ . 2)
				   (+_ . 2)
				   (_+_ . 2)
				   (_- . 2)
				   (__- . 2)
				   (-_ . 2)
				   (= . 5)
				   (~= . 5)
				   (<> . 5)
				   (AND . 4)
				   (And . 4)
				   (and . 4)
				   (OR . 3)
				   (Or . 3)
				   (or . 3)
				   (/ . 7)
				   (+ . 6)
				   (- . 6)
				   (> . 5)
				   (< . 5)
				   (>= . 5)
				   (<= . 5)
				   (^ . 8))))
	     (RETURN (CDR TMP)))
	    ((EQ OP '*)
	     (RETURN 7))
	    (T (RETURN 10)))))


% GSN  7-MAR-83 17:13 
% Get a predicate specification from the EXPR (referenced globally) 
%   and return code to test the SOURCE for that predicate. VERBFLG is 
%   true if a verb is expected as the top of EXPR. 
(DE GLPREDICATE (SOURCE CONTEXT VERBFLG ADDISATYPE)
(PROG (NEWPRED SETNAME PROPERTY TMP NOTFLG)
      (COND ((NULL VERBFLG)
	     (SETQ NEWPRED (GLDOEXPR NIL CONTEXT T)))
	    ((NULL SOURCE)
	     (GLERROR 'GLPREDICATE
		      (LIST "The object to be tested was not found.  EXPR =" 
			    EXPR)))
	    ((MEMQ (CAR EXPR)
		   '(HAS Has has))
	     (pop EXPR)
	     (COND ((MEMQ (CAR EXPR)
			  '(NO No no))
		    (SETQ NOTFLG T)
		    (pop EXPR)))
	     (SETQ NEWPRED (GLDOEXPR NIL CONTEXT T)))
	    ((MEMQ (CAR EXPR)
		   '(IS Is is ARE Are are))
	     (pop EXPR)
	     (COND ((MEMQ (CAR EXPR)
			  '(NOT Not not))
		    (SETQ NOTFLG T)
		    (pop EXPR)))
	     (COND ((GL-A-AN? (CAR EXPR))
		    (pop EXPR)
		    (SETQ SETNAME (pop EXPR))
		    
% The condition is to test whether SOURCE IS A SETNAME. 

		    (COND ((SETQ NEWPRED (GLADJ SOURCE SETNAME 'ISA)))
			  ((SETQ NEWPRED (GLADJ (LIST (CAR SOURCE)
						      SETNAME)
						SETNAME
						'ISASELF))
			   (GLNOTESOURCETYPE SOURCE SETNAME ADDISATYPE))
			  ((GLCLASSP SETNAME)
			   (SETQ NEWPRED (LIST (LIST 'GLCLASSMEMP
						     (CAR SOURCE)
						     (KWOTE SETNAME))
					       'BOOLEAN)))
			  ((SETQ TMP (GLLISPISA SETNAME))
			   (SETQ NEWPRED (LIST (GLGENCODE (LIST (CAR TMP)
								(CAR SOURCE)))
					       'BOOLEAN))
			   (GLNOTESOURCETYPE SOURCE (CADR TMP)
					     ADDISATYPE))
			  (T (GLERROR 'GLPREDICATE
				      (LIST "IS A adjective" SETNAME 
					    "could not be found for"
					    (CAR SOURCE)
					    "whose type is"
					    (CADR SOURCE)))
			     (SETQ NEWPRED (LIST (LIST 'GLERR
						       (CAR SOURCE)
						       'IS
						       'A
						       SETNAME)
						 'BOOLEAN)))))
		   (T (SETQ PROPERTY (CAR EXPR))
		      
% The condition to test is whether SOURCE is PROPERTY. 

		      (COND ((SETQ NEWPRED (GLADJ SOURCE PROPERTY
						  'ADJ))
			     (pop EXPR))
			    ((SETQ TMP (GLLISPADJ PROPERTY))
			     (pop EXPR)
			     (SETQ NEWPRED (LIST (GLGENCODE
						   (LIST (CAR TMP)
							 (CAR SOURCE)))
						 'BOOLEAN))
			     (GLNOTESOURCETYPE SOURCE (CADR TMP)
					       ADDISATYPE))
			    (T (GLERROR 'GLPREDICATE
					(LIST "The adjective" PROPERTY 
					      "could not be found for"
					      (CAR SOURCE)
					      "whose type is"
					      (CADR SOURCE)))
			       (pop EXPR)
			       (SETQ NEWPRED (LIST (LIST 'GLERR
							 (CAR SOURCE)
							 'IS
							 PROPERTY)
						   'BOOLEAN))))))))
      (RETURN (COND (NOTFLG (LIST (GLBUILDNOT (CAR NEWPRED))
				  'BOOLEAN))
		    (T NEWPRED)))))


% edited: 25-MAY-82 16:09 
% Compile an implicit PROGN, that is, a list of items. 
(DE GLPROGN (EXPR CONTEXT)
(PROG (RESULT TMP TYPE GLSEPATOM GLSEPPTR)
      (SETQ GLSEPPTR 0)
      A
      (COND ((NULL EXPR)
	     (RETURN (LIST (REVERSIP RESULT)
			   TYPE)))
	    ((SETQ TMP (GLDOEXPR NIL CONTEXT VALBUSY))
	     (SETQ RESULT (CONS (CAR TMP)
				RESULT))
	     (SETQ TYPE (CADR TMP))
	     (GO A))
	    (T (GLERROR 'GLPROGN
			(LIST 
			 "Illegal item appears in implicit PROGN.  EXPR ="
			      EXPR))))))


% edited: 14-MAR-83 17:12 
% Create a function call to retrieve the field IND from a 
%   property-list type structure. FLG is true if a PROPLIST is inside 
%   an ATOM structure. 
(DE GLPROPSTRFN (IND DES DESLIST FLG)
(PROG (DESIND TMP RECNAME N)
      
% Handle a PROPLIST by looking inside each property for IND. 

      (COND ((AND (EQ (SETQ DESIND (pop DES))
		      'RECORD)
		  (ATOM (CAR DES)))
	     (SETQ RECNAME (pop DES))))
      (SETQ N 0)
      P
      (COND ((NULL DES)
	     (RETURN NIL))
	    ((AND (PAIRP (CAR DES))
		  (ATOM (CAAR DES))
		  (CDAR DES)
		  (SETQ TMP (GLSTRFN IND (CAR DES)
				     DESLIST)))
	     (SETQ
	       TMP
	       (GLSTRVAL TMP
			 (CASEQ DESIND (ALIST (LIST 'GLGETASSOC
						    (KWOTE (CAAR DES))
						    '*GL*))
				((RECORD OBJECT)
				 (COND ((EQ DESIND 'OBJECT)
					(SETQ N (ADD1 N))))
				 (LIST 'GetV
				       '*GL*
				       N))
				((PROPLIST ATOMOBJECT)
				 (GLGENCODE
				   (LIST (COND ((OR FLG (EQ DESIND
							    'ATOMOBJECT))
						'GETPROP)
					       (T 'LISTGET))
					 '*GL*
					 (KWOTE (CAAR DES))))))))
	     (RETURN TMP))
	    (T (pop DES)
	       (SETQ N (ADD1 N))
	       (GO P)))))


% edited:  4-JUN-82 13:37 
% Test if the function X is a pure computation, i.e., can be 
%   eliminated if the result is not used. 
(DE GLPURE (X)
(MEMQ X '(CAR CDR CXR CAAR CADR CDAR CDDR ADD1 SUB1 CADDR CADDDR)))


% edited: 25-MAY-82 16:10 
% This function serves to call GLDOEXPR with a new expression, 
%   rebinding the global variable EXPR. 
(DE GLPUSHEXPR (EXPR START CONTEXT VALBUSY)
(PROG (GLSEPATOM GLSEPPTR)
      (SETQ GLSEPPTR 0)
      (RETURN (GLDOEXPR START CONTEXT VALBUSY))))


% GSN 25-JAN-83 16:48 
% edited:  2-Jun-81 14:19 
% Produce a function to implement the +_ operator. Code is produced to 
%   push the right-hand side onto the left-hand side. Note: parts of 
%   the structure provided are used multiple times. 
(DE GLPUSHFN (LHS RHS)
(PROG (LHSCODE LHSDES NCCODE TMP STR)
      (SETQ LHSCODE (CAR LHS))
      (SETQ LHSDES (GLXTRTYPE (CADR LHS)))
      (COND ((EQ LHSDES 'INTEGER)
	     (COND ((EQN (CAR RHS)
			 1)
		    (SETQ NCCODE (LIST 'ADD1
				       LHSCODE)))
		   ((OR (FIXP (CAR RHS))
			(EQ (CADR RHS)
			    'INTEGER))
		    (SETQ NCCODE (LIST 'IPLUS
				       LHSCODE
				       (CAR RHS))))
		   (T (SETQ NCCODE (LIST 'PLUS
					 LHSCODE
					 (CAR RHS))))))
	    ((OR (EQ LHSDES 'NUMBER)
		 (EQ LHSDES 'REAL))
	     (SETQ NCCODE (LIST 'PLUS
				LHSCODE
				(CAR RHS))))
	    ((EQ LHSDES 'BOOLEAN)
	     (SETQ NCCODE (LIST 'OR
				LHSCODE
				(CAR RHS))))
	    ((NULL LHSDES)
	     (SETQ NCCODE (LIST 'CONS
				(CAR RHS)
				LHSCODE))
	     (COND ((AND (ATOM LHSCODE)
			 (CADR RHS))
		    (GLUPDATEVARTYPE LHSCODE (LIST 'LISTOF
						   (CADR RHS))))))
	    ((AND (PAIRP LHSDES)
		  (MEMQ (CAR LHSDES)
			'(LIST CONS LISTOF)))
	     (SETQ NCCODE (LIST 'CONS
				(CAR RHS)
				LHSCODE)))
	    ((SETQ TMP (GLUNITOP LHS RHS 'PUSH))
	     (RETURN TMP))
	    ((SETQ TMP (GLDOMSG LHS '+_
				(LIST RHS)))
	     (RETURN TMP))
	    ((SETQ TMP (GLDOMSG LHS '+
				(LIST RHS)))
	     (SETQ NCCODE (CAR TMP)))
	    ((AND (SETQ STR (GLGETSTR LHSDES))
		  (SETQ TMP (GLPUSHFN (LIST (CAR LHS)
					    STR)
				      RHS)))
	     (RETURN (LIST (CAR TMP)
			   (CADR LHS))))
	    ((SETQ TMP (GLUSERSTROP LHS '+_
				    RHS))
	     (RETURN TMP))
	    ((SETQ TMP (GLREDUCEARITH '+
				      RHS LHS))
	     (SETQ NCCODE (CAR TMP)))
	    (T (RETURN NIL)))
      (RETURN (GLPUTFN LHS (LIST (GLGENCODE NCCODE)
				 LHSDES)
		       T))))


% GSN 22-JAN-83 14:44 
% Process a store into a value which is computed by an arithmetic 
%   expression. 
(DE GLPUTARITH (LHS RHS)
(PROG (LHSC OP TMP NEWLHS NEWRHS)
      (SETQ LHSC (CAR LHS))
      (SETQ OP (CAR LHSC))
      (COND ((NOT (SETQ TMP (ASSOC OP '((PLUS DIFFERENCE)
					(MINUS MINUS)
					(DIFFERENCE PLUS)
					(TIMES QUOTIENT)
					(QUOTIENT TIMES)
					(IPLUS IDIFFERENCE)
					(IMINUS IMINUS)
					(IDIFFERENCE IPLUS)
					(ITIMES IQUOTIENT)
					(IQUOTIENT ITIMES)
					(ADD1 SUB1)
					(SUB1 ADD1)
					(EXPT SQRT)
					(SQRT EXPT)))))
	     (RETURN NIL)))
      (SETQ NEWLHS (CADR LHSC))
      (CASEQ OP ((ADD1 SUB1 MINUS IMINUS)
	      (SETQ NEWRHS (LIST (CADR TMP)
				 (CAR RHS))))
	     ((PLUS DIFFERENCE TIMES QUOTIENT IPLUS IDIFFERENCE ITIMES 
		    IQUOTIENT)
	      (COND ((NUMBERP (CADDR LHSC))
		     (SETQ NEWRHS (LIST (CADR TMP)
					(CAR RHS)
					(CADDR LHSC))))
		    ((NUMBERP (CADR LHSC))
		     (SETQ NEWLHS (CADDR LHSC))
		     (CASEQ OP ((DIFFERENCE IDIFFERENCE QUOTIENT IQUOTIENT)
			     (SETQ NEWRHS (LIST OP (CADR LHSC)
						(CAR RHS))))
			    (T (PROGN (SETQ NEWRHS (LIST (CADR TMP)
							 (CAR RHS)
							 (CADR LHSC)))))))))
	     (EXPT (COND ((EQUAL (CADDR LHSC)
				 2)
			  (SETQ NEWRHS (LIST (CADR TMP)
					     (CAR RHS))))))
	     (SQRT (SETQ NEWRHS (LIST (CADR TMP)
				      (CAR RHS)
				      2))))
      (RETURN (AND NEWRHS (GLPUTFN (LIST NEWLHS (CADR LHS))
				   (LIST NEWRHS (CADR RHS))
				   NIL)))))


% GSN 22-JAN-83 14:37 
% edited:  2-Jun-81 14:16 
% Create code to put the right-hand side datum RHS into the left-hand 
%   side, whose access function and type are given by LHS. 
(DE GLPUTFN (LHS RHS OPTFLG)
(PROG (LHSD LNAME TMP RESULT TMPVAR)
      (SETQ LHSD (CAR LHS))
      (COND ((ATOM LHSD)
	     (RETURN (OR (GLDOMSG LHS '_
				  (LIST RHS))
			 (GLUSERSTROP LHS '_
				      RHS)
			 (AND (NULL (CADR LHS))
			      (CADR RHS)
			      (GLUSERSTROP (LIST (CAR LHS)
						 (CADR RHS))
					   '_
					   RHS))
			 (GLDOVARSETQ LHSD RHS)))))
      (SETQ LNAME (CAR LHSD))
      (COND ((EQ LNAME 'CAR)
	     (SETQ RESULT (COND
		     ((AND OPTFLG (GLEXPENSIVE? (CADR LHSD)))
		      (LIST 'PROG
			    (LIST (LIST (SETQ TMPVAR (GLMKVAR))
					(CADR LHSD)))
			    (LIST 'RETURN
				  (LIST 'CAR
					(LIST 'RPLACA
					      TMPVAR
					      (SUBST TMPVAR (CADR LHSD)
						     (CAR RHS)))))))
		     (T (LIST 'CAR
			      (LIST 'RPLACA
				    (CADR LHSD)
				    (CAR RHS)))))))
	    ((EQ LNAME 'CDR)
	     (SETQ RESULT (COND
		     ((AND OPTFLG (GLEXPENSIVE? (CADR LHSD)))
		      (LIST 'PROG
			    (LIST (LIST (SETQ TMPVAR (GLMKVAR))
					(CADR LHSD)))
			    (LIST 'RETURN
				  (LIST 'CDR
					(LIST 'RPLACD
					      TMPVAR
					      (SUBST TMPVAR (CADR LHSD)
						     (CAR RHS)))))))
		     (T (LIST 'CDR
			      (LIST 'RPLACD
				    (CADR LHSD)
				    (CAR RHS)))))))
	    ((SETQ TMP (ASSOC LNAME '((CADR . CDR)
				      (CADDR . CDDR)
				      (CADDDR . CDDDR))))
	     (SETQ RESULT
		   (COND
		     ((AND OPTFLG (GLEXPENSIVE? (CADR LHSD)))
		      (LIST 'PROG
			    (LIST (LIST (SETQ TMPVAR (GLMKVAR))
					(LIST (CDR TMP)
					      (CADR LHSD))))
			    (LIST 'RETURN
				  (LIST 'CAR
					(LIST 'RPLACA
					      TMPVAR
					      (SUBST (LIST 'CAR
							   TMPVAR)
						     LHSD
						     (CAR RHS)))))))
		     (T (LIST 'CAR
			      (LIST 'RPLACA
				    (LIST (CDR TMP)
					  (CADR LHSD))
				    (CAR RHS)))))))
	    ((SETQ TMP (ASSOC LNAME '((GetV . PutV)
				      (IGetV . IPutV)
				      (GET . PUTPROP)
				      (GETPROP . PUTPROP)
				      (LISTGET . LISTPUT))))
	     (SETQ RESULT (LIST (CDR TMP)
				(CADR LHSD)
				(CADDR LHSD)
				(CAR RHS))))
	    ((EQ LNAME 'CXR)
	     (SETQ RESULT (LIST 'CXR
				(CADR LHSD)
				(LIST 'RPLACX
				      (CADR LHSD)
				      (CADDR LHSD)
				      (CAR RHS)))))
	    ((EQ LNAME 'GLGETASSOC)
	     (SETQ RESULT (LIST 'PUTASSOC
				(CADR LHSD)
				(CAR RHS)
				(CADDR LHSD))))
	    ((EQ LNAME 'EVAL)
	     (SETQ RESULT (LIST 'SET
				(CADR LHSD)
				(CAR RHS))))
	    ((EQ LNAME 'fetch)
	     (SETQ RESULT (LIST 'replace
				(CADR LHSD)
				'of
				(CADDDR LHSD)
				'with
				(CAR RHS))))
	    ((SETQ TMP (GLUNITOP LHS RHS 'PUT))
	     (RETURN TMP))
	    ((SETQ TMP (GLDOMSG LHS '_
				(LIST RHS)))
	     (RETURN TMP))
	    ((SETQ TMP (GLUSERSTROP LHS '_
				    RHS))
	     (RETURN TMP))
	    ((SETQ TMP (GLPUTARITH LHS RHS))
	     (RETURN TMP))
	    (T (RETURN (GLERROR 'GLPUTFN
				(LIST "Illegal assignment.  LHS =" LHS "RHS =" 
				      RHS)))))
      X
      (RETURN (LIST (GLGENCODE RESULT)
		    (OR (CADR LHS)
			(CADR RHS))))))


% edited: 27-MAY-82 13:07 
% This function appends PUTPROP calls to the list PROGG (global) so 
%   that ATOMNAME has its property list built. 
(DE GLPUTPROPS (PROPLIS PREVLST)
(PROG (TMP TMPCODE)
      A
      (COND ((NULL PROPLIS)
	     (RETURN NIL)))
      (SETQ TMP (pop PROPLIS))
      (COND ((SETQ TMPCODE (GLBUILDSTR TMP PAIRLIST PREVLST))
	     (ACONC PROGG (GLGENCODE (LIST 'PUTPROP
					   'ATOMNAME
					   (KWOTE (CAR TMP))
					   TMPCODE)))))
      (GO A)))


% edited: 26-JAN-82 10:29 
% This function implements the __ operator, which is interpreted as 
%   assignment to the source of a variable (usually self) outside an 
%   open-compiled function. Any other use of __ is illegal. 
(DE GLPUTUPFN (OP LHS RHS)
(PROG (TMP TMPOP)
      (OR (SETQ TMPOP (ASSOC OP '((__ . _)
				  (__+ . _+)
				  (__- . _-)
				  (_+_ . +_))))
	  (ERROR 0 (LIST (LIST 'GLPUTUPFN
			       OP)
			 " Illegal operator.")))
      (COND ((AND (ATOM (CAR LHS))
		  (NOT (UNBOUNDP 'GLPROGLST))
		  (SETQ TMP (ASSOC (CAR LHS)
				   GLPROGLST)))
	     (RETURN (GLREDUCEOP (CDR TMPOP)
				 (LIST (CADR TMP)
				       (CADR LHS))
				 RHS)))
	    ((AND (PAIRP (CAR LHS))
		  (EQ (CAAR LHS)
		      'PROG1)
		  (ATOM (CADAR LHS)))
	     (RETURN (GLREDUCEOP (CDR TMPOP)
				 (LIST (CADAR LHS)
				       (CADR LHS))
				 RHS)))
	    (T (RETURN (GLERROR 'GLPUTUPFN
				(LIST 
		"A self-assignment __ operator is used improperly.  LHS ="
				      LHS)))))))


% edited: 30-OCT-82 14:38 
% Reduce the operator on OPERS and the operands on OPNDS 
%   (in GLPARSEXPR) and put the result back on OPNDS 
(DE GLREDUCE NIL
(PROG (RHS OPER)
      (SETQ RHS (pop OPNDS))
      (SETQ OPNDS
	    (CONS (COND ((MEMQ (SETQ OPER (pop OPERS))
			       '(_ := _+
				   +_ _-
				   -_ = ~= <> AND And and OR Or
				     or __+
					__ _+_ __-))
			 (GLREDUCEOP OPER (pop OPNDS)
				     RHS))
			((MEMQ OPER
			       '(+ - * / > < >= <= ^))
			 (GLREDUCEARITH OPER (pop OPNDS)
					RHS))
			((EQ OPER 'MINUS)
			 (GLMINUSFN RHS))
			((EQ OPER '~)
			 (GLNOTFN RHS))
			(T (LIST (GLGENCODE (LIST OPER (CAR (pop OPNDS))
						  (CAR RHS)))
				 NIL)))
		  OPNDS))))


% GSN 25-FEB-83 16:32 
% edited: 14-Aug-81 12:38 
% Reduce an arithmetic operator in an expression. 
(DE GLREDUCEARITH (OP LHS RHS)
(PROG (TMP OPLIST IOPLIST PREDLIST NUMBERTYPES LHSTP RHSTP)
      (SETQ OPLIST '((+ . PLUS)
		     (- . DIFFERENCE)          (* . TIMES)
		     (/ . QUOTIENT)
		     (> . GREATERP)
		     (< . LESSP)
		     (>= . GEQ)
		     (<= . LEQ)
		     (^ . EXPT)))
      (SETQ IOPLIST '((+ . IPLUS)
		      (- . IDIFFERENCE)        (* . ITIMES)
		      (/ . IQUOTIENT)
		      (> . IGREATERP)
		      (< . ILESSP)
		      (>= . IGEQ)
		      (<= . ILEQ)))
      (SETQ PREDLIST '(GREATERP LESSP GEQ LEQ IGREATERP ILESSP IGEQ ILEQ))
      (SETQ NUMBERTYPES '(INTEGER REAL NUMBER))
      (SETQ LHSTP (GLXTRTYPE (CADR LHS)))
      (SETQ RHSTP (GLXTRTYPE (CADR RHS)))
      (COND ((OR (AND (EQ LHSTP 'INTEGER)
		      (EQ RHSTP 'INTEGER)
		      (SETQ TMP (ASSOC OP IOPLIST)))
		 (AND (MEMQ LHSTP NUMBERTYPES)
		      (MEMQ RHSTP NUMBERTYPES)
		      (SETQ TMP (ASSOC OP OPLIST))))
	     (RETURN (LIST (COND ((AND (NUMBERP (CAR LHS))
				       (NUMBERP (CAR RHS)))
				  (EVAL (GLGENCODE (LIST (CDR TMP)
							 (CAR LHS)
							 (CAR RHS)))))
				 (T (GLGENCODE (COND
						 ((AND (EQ (CDR TMP)
							   'IPLUS)
						       (EQN (CAR RHS)
							    1))
						  (LIST 'ADD1
							(CAR LHS)))
						 ((AND (EQ (CDR TMP)
							   'IDIFFERENCE)
						       (EQN (CAR RHS)
							    1))
						  (LIST 'SUB1
							(CAR LHS)))
						 (T (LIST (CDR TMP)
							  (CAR LHS)
							  (CAR RHS)))))))
			   (COND ((MEMQ (CDR TMP)
					PREDLIST)
				  'BOOLEAN)
				 (T LHSTP))))))
      (COND
	((EQ LHSTP 'STRING)
	 (COND ((NE RHSTP 'STRING)
		(RETURN (GLERROR 'GLREDUCEARITH
				 (LIST "operation on string and non-string"))))
	       ((SETQ TMP (ASSOC OP '((+ CONCAT STRING)
				      (> GLSTRGREATERP BOOLEAN)
				      (>= GLSTRGEP BOOLEAN)
				      (< GLSTRLESSP BOOLEAN)
				      (<= ALPHORDER BOOLEAN))))
		(RETURN (LIST (GLGENCODE (LIST (CADR TMP)
					       (CAR LHS)
					       (CAR RHS)))
			      (CADDR TMP))))
	       (T (RETURN (GLERROR 'GLREDUCEARITH
				   (LIST OP 
				    "is an illegal operation for strings.")))))
	 )
	((EQ LHSTP 'BOOLEAN)
	 (COND
	   ((NE RHSTP 'BOOLEAN)
	    (RETURN (GLERROR 'GLREDUCEARITH
			     (LIST "Operation on Boolean and non-Boolean"))))
	   ((MEMQ OP '(+ * -))
	    (RETURN (LIST (GLGENCODE (CASEQ OP (+ (LIST 'OR
							(CAR LHS)
							(CAR RHS)))
					    (* (LIST 'AND
						     (CAR LHS)
						     (CAR RHS)))
					    (- (LIST 'AND
						     (CAR LHS)
						     (LIST 'NOT
							   (CAR RHS))))))
			  'BOOLEAN)))
	   (T (RETURN (GLERROR 'GLREDUCEARITH
			       (LIST OP 
				   "is an illegal operation for Booleans.")))))
	 )
	((AND (PAIRP LHSTP)
	      (EQ (CAR LHSTP)
		  'LISTOF))
	 (COND ((AND (PAIRP RHSTP)
		     (EQ (CAR RHSTP)
			 'LISTOF))
		(COND ((NOT (EQUAL (CADR LHSTP)
				   (CADR RHSTP)))
		       (RETURN (GLERROR 'GLREDUCEARITH
					(LIST 
				  "Operations on lists of different types"
					      (CADR LHSTP)
					      (CADR RHSTP))))))
		(COND ((SETQ TMP (ASSOC OP '((+ UNION)
					     (- LDIFFERENCE)
                                               (* INTERSECTION)
					     )))
		       (RETURN (LIST (GLGENCODE (LIST (CADR TMP)
						      (CAR LHS)
						      (CAR RHS)))
				     (CADR LHS))))
		      (T (RETURN (GLERROR 'GLREDUCEARITH
					  (LIST "Illegal operation" OP 
						"on lists."))))))
	       ((AND (GLMATCH RHSTP (CADR LHSTP))
		     (MEMQ OP '(+ - >=)))
		(RETURN (LIST (GLGENCODE (LIST (COND ((EQ OP '+)
						      'CONS)
						     ((EQ OP '-)
						      'REMOVE)
						     ((EQ OP '>=)
						      (COND
							((GLATOMTYPEP RHSTP)
							 'MEMB)
							(T 'MEMBER))))
					       (CAR RHS)
					       (CAR LHS)))
			      (CADR LHS))))
	       (T (RETURN (GLERROR 'GLREDUCEARITH
				   (LIST "Illegal operation on list."))))))
	((AND (MEMQ OP '(+ <=))
	      (GLMATCHL LHSTP RHSTP))
	 (RETURN (COND ((EQ OP '+)
			(LIST (GLGENCODE (LIST 'CONS
					       (CAR LHS)
					       (CAR RHS)))
			      (CADR RHS)))
		       ((EQ OP '<=)
			(LIST (GLGENCODE (LIST (COND ((GLATOMTYPEP LHSTP)
						      'MEMB)
						     (T 'MEMBER))
					       (CAR LHS)
					       (CAR RHS)))
			      'BOOLEAN)))))
	((AND (MEMQ OP '(+ - >=))
	      (SETQ TMP (GLMATCHL LHSTP RHSTP)))
	 (RETURN (GLREDUCEARITH (LIST (CAR LHS)
				      (LIST 'LISTOF
					    TMP))
				OP
				(LIST (CAR RHS)
				      TMP))))
	((SETQ TMP (GLDOMSG LHS OP (LIST RHS)))
	 (RETURN TMP))
	((SETQ TMP (GLUSERSTROP LHS OP RHS))
	 (RETURN TMP))
	((SETQ TMP (GLXTRTYPEC LHSTP))
	 (SETQ TMP (GLREDUCEARITH OP (LIST (CAR LHS)
					   TMP)
				  (LIST (CAR RHS)
					(OR (GLXTRTYPEC RHSTP)
					    RHSTP))))
	 (RETURN (LIST (CAR TMP)
		       LHSTP)))
	((SETQ TMP (ASSOC OP OPLIST))
	 (AND LHSTP RHSTP (GLERROR 'GLREDUCEARITH
				   (LIST 
	"Warning: Arithmetic operation on non-numeric arguments of types:"
					 LHSTP RHSTP)))
	 (RETURN (LIST (GLGENCODE (LIST (CDR TMP)
					(CAR LHS)
					(CAR RHS)))
		       (COND ((MEMQ (CDR TMP)
				    PREDLIST)
			      'BOOLEAN)
			     (T 'NUMBER)))))
	(T (ERROR 0 (LIST 'GLREDUCEARITH
			  OP LHS RHS))))))


% edited: 29-DEC-82 12:20 
% Reduce the operator OP with operands LHS and RHS. 
(DE GLREDUCEOP (OP LHS RHS)
(PROG (TMP RESULT)
      (COND ((MEMQ OP '(_ :=))
	     (RETURN (GLPUTFN LHS RHS NIL)))
	    ((SETQ TMP (ASSOC OP '((_+ . GLNCONCFN)
				   (+_ . GLPUSHFN)
				   (_- . GLREMOVEFN)
				   (-_ . GLPOPFN)
				   (= . GLEQUALFN)
				   (~= . GLNEQUALFN)
				   (<> . GLNEQUALFN)
				   (AND . GLANDFN)
				   (And . GLANDFN)
				   (and . GLANDFN)
				   (OR . GLORFN)
				   (Or . GLORFN)
				   (or . GLORFN))))
	     (COND ((SETQ RESULT (APPLY (CDR TMP)
					(LIST LHS RHS)))
		    (RETURN RESULT))
		   (T (GLERROR 'GLREDUCEOP
			       (LIST "The operator" OP 
				  "could not be interpreted for arguments"
				     LHS "and" RHS)))))
	    ((MEMQ OP '(__ __+
			   __-
			   _+_))
	     (RETURN (GLPUTUPFN OP LHS RHS)))
	    (T (ERROR 0 (LIST 'GLREDUCEOP
			      OP LHS RHS))))))


% GSN 25-JAN-83 16:50 
% edited:  2-Jun-81 14:20 
% Produce a function to implement the _- operator. Code is produced to 
%   remove the right-hand side from the left-hand side. Note: parts of 
%   the structure provided are used multiple times. 
(DE GLREMOVEFN (LHS RHS)
(PROG (LHSCODE LHSDES NCCODE TMP STR)
      (SETQ LHSCODE (CAR LHS))
      (SETQ LHSDES (GLXTRTYPE (CADR LHS)))
      (COND ((EQ LHSDES 'INTEGER)
	     (COND ((EQN (CAR RHS)
			 1)
		    (SETQ NCCODE (LIST 'SUB1
				       LHSCODE)))
		   (T (SETQ NCCODE (LIST 'IDIFFERENCE
					 LHSCODE
					 (CAR RHS))))))
	    ((OR (EQ LHSDES 'NUMBER)
		 (EQ LHSDES 'REAL))
	     (SETQ NCCODE (LIST 'DIFFERENCE
				LHSCODE
				(CAR RHS))))
	    ((EQ LHSDES 'BOOLEAN)
	     (SETQ NCCODE (LIST 'AND
				LHSCODE
				(LIST 'NOT
				      (CAR RHS)))))
	    ((OR (NULL LHSDES)
		 (AND (PAIRP LHSDES)
		      (EQ (CAR LHSDES)
			  'LISTOF)))
	     (SETQ NCCODE (LIST 'REMOVE
				(CAR RHS)
				LHSCODE)))
	    ((SETQ TMP (GLUNITOP LHS RHS 'REMOVE))
	     (RETURN TMP))
	    ((SETQ TMP (GLDOMSG LHS '_-
				(LIST RHS)))
	     (RETURN TMP))
	    ((SETQ TMP (GLDOMSG LHS '-
				(LIST RHS)))
	     (SETQ NCCODE (CAR TMP)))
	    ((AND (SETQ STR (GLGETSTR LHSDES))
		  (SETQ TMP (GLREMOVEFN (LIST (CAR LHS)
					      STR)
					RHS)))
	     (RETURN (LIST (CAR TMP)
			   (CADR LHS))))
	    ((SETQ TMP (GLUSERSTROP LHS '_-
				    RHS))
	     (RETURN TMP))
	    (T (RETURN NIL)))
      (RETURN (GLPUTFN LHS (LIST (GLGENCODE NCCODE)
				 LHSDES)
		       T))))


% GSN 26-JAN-83 13:41 
% Get GLOBAL and RESULT declarations for the GLISP compiler. The 
%   property GLRESULTTYPE is the RESULT declaration, if specified; 
%   GLGLOBALS is a list of global variables referenced and their 
%   types. 
(DE GLRESGLOBAL NIL
(COND ((PAIRP (CAR GLEXPR))
       (COND ((MEMQ (CAAR GLEXPR)
		    '(RESULT Result result))
	      (COND ((AND (GLOKSTR? (CADAR GLEXPR))
			  (NULL (CDDAR GLEXPR)))
		     (PUT GLAMBDAFN 'GLRESULTTYPE
			  (SETQ RESULTTYPE (GLSUBSTTYPE (GLEVALSTR
							  (CADAR GLEXPR)
							  GLTOPCTX)
							GLTYPESUBS)))
		     (pop GLEXPR))
		    (T (GLERROR 'GLCOMP
				(LIST "Bad RESULT structure declaration:"
				      (CAR GLEXPR)))
		       (pop GLEXPR))))
	     ((MEMQ (CAAR GLEXPR)
		    '(GLOBAL Global global))
	      (SETQ GLGLOBALVARS (GLDECL (CDAR GLEXPR)
					 '(NIL NIL)
					 GLTOPCTX NIL NIL))
	      (PUT GLAMBDAFN 'GLGLOBALS
		   GLGLOBALVARS)
	      (pop GLEXPR))))))


% edited: 26-MAY-82 16:14 
% Get the result type for a function which has a GLAMBDA definition. 
%   ATM is the function name. 
(DE GLRESULTTYPE (ATM ARGTYPES)
(PROG (TYPE FNDEF STR TMP)
      
% See if this function has a known result type. 

      (COND ((SETQ TYPE (GET ATM 'GLRESULTTYPE))
	     (RETURN TYPE)))
      
% If there exists a function to compute the result type, let it do so. 

      (COND ((SETQ TMP (GET ATM 'GLRESULTTYPEFN))
	     (RETURN (APPLY TMP (LIST ATM ARGTYPES))))
	    ((SETQ TMP (GLANYCARCDR? ATM))
	     (RETURN (GLCARCDRRESULTTYPE TMP (CAR ARGTYPES)))))
      (SETQ FNDEF (GLGETDB ATM))
      (COND ((OR (NOT (PAIRP FNDEF))
		 (NOT (MEMQ (CAR FNDEF)
			    '(LAMBDA GLAMBDA))))
	     (RETURN NIL)))
      (SETQ FNDEF (CDDR FNDEF))
      A
      (COND ((OR (NULL FNDEF)
		 (NOT (PAIRP (CAR FNDEF))))
	     (RETURN NIL))
	    ((OR (AND (EQ GLLISPDIALECT 'INTERLISP)
		      (EQ (CAAR FNDEF)
			  '*))
		 (MEMQ (CAAR FNDEF)
		       '(GLOBAL Global global)))
	     (pop FNDEF)
	     (GO A))
	    ((AND (MEMQ (CAAR FNDEF)
			'(RESULT Result result))
		  (GLOKSTR? (SETQ STR (CADAR FNDEF))))
	     (RETURN STR))
	    (T (RETURN NIL)))))


% GSN 28-JAN-83 09:55 
(DE GLSAVEFNTYPES (GLAMBDAFN TYPELST)
(PROG (Y)
      (MAPC TYPELST (FUNCTION (LAMBDA (X)
				(COND
				  ((NOT (MEMQ GLAMBDAFN (SETQ Y
						(GET X 'GLFNSUSEDIN))))
				    (PUT X 'GLFNSUSEDIN
					 (CONS GLAMBDAFN Y)))))))))


% GSN 16-FEB-83 11:30 
% Send a runtime message to OBJ. 
(DE GLSENDB (OBJ CLASS SELECTOR PROPTYPE ARGS)
(PROG (RESULT ARGLIST FNCODE PUTCODE *GL* *GLVAL* SEL)
      (COND (CLASS)
	    ((SETQ CLASS (GLCLASS OBJ)))
	    (T (ERROR 0 (LIST "Object" OBJ "has no Class."))))
      (SETQ ARGLIST (CONS OBJ ARGS))
      (COND ((NE (SETQ RESULT (GLCLASSSEND CLASS SELECTOR ARGLIST PROPTYPE))
		 'GLSENDFAILURE)
	     (RETURN RESULT))
	    ((AND (EQ SELECTOR 'CLASS)
		  (MEMQ PROPTYPE '(PROP MSG)))
	     (RETURN CLASS))
	    ((NE PROPTYPE 'MSG)
	     (GO ERR))
	    ((AND ARGS (NULL (CDR ARGS))
		  (EQ (GLNTHCHAR SELECTOR -1)
		      ':)
		  (SETQ SEL (SUBATOM SELECTOR 1 -2))
		  (SETQ FNCODE (OR (GLCOMPPROP CLASS SEL 'STR)
				   (GLCOMPPROP CLASS SEL 'PROP)))
		  (SETQ PUTCODE (GLPUTFN (LIST (SUBST '*GL*
						      (CAADR FNCODE)
						      (CADDR FNCODE))
					       NIL)
					 (LIST '*GLVAL*
					       NIL)
					 NIL)))
	     (SETQ *GLVAL* (CAR ARGS))
	     (SETQ *GL* OBJ)
	     (RETURN (EVAL (CAR PUTCODE))))
	    (ARGS (GO ERR))
	    ((NE (SETQ RESULT (GLCLASSSEND CLASS SELECTOR ARGLIST
					   'STR))
		 'GLSENDFAILURE)
	     (RETURN RESULT))
	    ((NE (SETQ RESULT (GLCLASSSEND CLASS SELECTOR ARGLIST
					   'PROP))
		 'GLSENDFAILURE)
	     (RETURN RESULT))
	    ((NE (SETQ RESULT (GLCLASSSEND CLASS SELECTOR ARGLIST
					   'ADJ))
		 'GLSENDFAILURE)
	     (RETURN RESULT))
	    ((NE (SETQ RESULT (GLCLASSSEND CLASS SELECTOR ARGLIST
					   'ISA))
		 'GLSENDFAILURE)
	     (RETURN RESULT)))
      ERR
      (ERROR 0 (LIST "Message" SELECTOR "to object" OBJ "of class" CLASS 
		     "not understood."))))


% edited: 30-DEC-81 16:34 
(DE GLSEPCLR NIL
(SETQ GLSEPPTR 0))


% GSN  9-FEB-83 17:24 
% edited: 30-Dec-80 10:05 
% Initialize the scanning function which breaks apart atoms containing 
%   embedded operators. 
(DE GLSEPINIT (ATM)
(COND ((AND (ATOM ATM)
	    (NOT (STRINGP ATM)))
       (SETQ GLSEPATOM ATM)
       (SETQ GLSEPPTR 1))
      (T (SETQ GLSEPATOM NIL)
	 (SETQ GLSEPPTR 0))))


% edited: 30-OCT-82 14:40 
% Get the next sub-atom from the atom which was previously given to 
%   GLSEPINIT. Sub-atoms are defined by splitting the given atom at 
%   the occurrence of operators. Operators which are defined are : _ 
%   _+ __ +_ _- -_ ' = ~= <> > < 
(DE GLSEPNXT NIL
(PROG (END TMP)
      (COND ((ZEROP GLSEPPTR)
	     (RETURN NIL))
	    ((NULL GLSEPATOM)
	     (SETQ GLSEPPTR 0)
	     (RETURN '*NIL*))
	    ((NUMBERP GLSEPATOM)
	     (SETQ TMP GLSEPATOM)
	     (SETQ GLSEPPTR 0)
	     (RETURN TMP)))
      (SETQ END (STRPOSL GLSEPBITTBL GLSEPATOM GLSEPPTR))
      A
      (COND ((NULL END)
	     (RETURN (PROG1 (COND ((EQN GLSEPPTR 1)
				   GLSEPATOM)
				  ((GREATERP GLSEPPTR (FlatSize2 GLSEPATOM))
				   NIL)
				  (T (GLSUBATOM GLSEPATOM GLSEPPTR
						(FlatSize2 GLSEPATOM))))
			    (SETQ GLSEPPTR 0))))
	    ((MEMQ (SETQ TMP (GLSUBATOM GLSEPATOM GLSEPPTR (PLUS GLSEPPTR 2)))
		   '(__+
		      __-
		      _+_))
	     (SETQ GLSEPPTR (PLUS GLSEPPTR 3))
	     (RETURN TMP))
	    ((MEMQ (SETQ TMP (GLSUBATOM GLSEPATOM GLSEPPTR (ADD1 GLSEPPTR)))
		   '(:= __ _+
			+_ _-
			-_ ~= <> >= <=))
	     (SETQ GLSEPPTR (PLUS GLSEPPTR 2))
	     (RETURN TMP))
	    ((AND (NOT GLSEPMINUS)
		  (EQ (GLNTHCHAR GLSEPATOM END)
		      '-)
		  (NOT (EQ (GLNTHCHAR GLSEPATOM (ADD1 END))
			   '_)))
	     (SETQ END (STRPOSL GLSEPBITTBL GLSEPATOM (ADD1 END)))
	     (GO A))
	    ((GREATERP END GLSEPPTR)
	     (RETURN (PROG1 (GLSUBATOM GLSEPATOM GLSEPPTR (SUB1 END))
			    (SETQ GLSEPPTR END))))
	    (T (RETURN (PROG1 (GLSUBATOM GLSEPATOM GLSEPPTR GLSEPPTR)
			      (SETQ GLSEPPTR (ADD1 GLSEPPTR))))))))


% edited: 26-MAY-82 16:17 
% Skip comments in GLEXPR. 
(DE GLSKIPCOMMENTS NIL
(PROG NIL A (COND ((AND (PAIRP GLEXPR)
			(PAIRP (CAR GLEXPR))
			(OR (AND (EQ GLLISPDIALECT 'INTERLISP)
				 (EQ (CAAR GLEXPR)
				     '*))
			    (EQ (CAAR GLEXPR)
				'COMMENT)))
		   (pop GLEXPR)
		   (GO A)))))


% GSN 17-FEB-83 12:36 
% This function is called when the structure STR has been changed. It 
%   uncompiles code which depends on STR. 
(DE GLSTRCHANGED (STR)
(PROG (FNS)
      (COND ((NOT (GET STR 'GLSTRUCTURE))
	     (RETURN NIL))
	    ((GET STR 'GLPROPFNS)
	     (PUT STR 'GLPROPFNS
		  NIL)))
      (SETQ FNS (GET STR 'GLFNSUSEDIN))
      (PUT STR 'GLFNSUSEDIN
	   NIL)
      (MAPC FNS (FUNCTION GLUNCOMPILE))))


% GSN 28-JAN-83 10:19 
% Create a function call to retrieve the field IND from a structure 
%   described by the structure description DES. The value is NIL if 
%   failure, (NIL DESCR) if DES equals IND, or (FNSTR DESCR) if IND 
%   can be gotten from within DES. In the latter case, FNSTR is a 
%   function to get the IND from the atom *GL*. GLSTRFN only does 
%   retrieval from a structure, and does not get properties of an 
%   object unless they are part of a TRANSPARENT substructure. DESLIST 
%   is a list of structure descriptions which have been tried already; 
%   this prevents a compiler loop in case the user specifies circular 
%   TRANSPARENT structures. 
(DE GLSTRFN (IND DES DESLIST)
(PROG (DESIND TMP STR UNITREC)
      
% If this structure has already been tried, quit to avoid a loop. 

      (COND ((MEMQ DES DESLIST)
	     (RETURN NIL)))
      (SETQ DESLIST (CONS DES DESLIST))
      (COND ((OR (NULL DES)
		 (NULL IND))
	     (RETURN NIL))
	    ((OR (ATOM DES)
		 (AND (PAIRP DES)
		      (ATOM (CADR DES))
		      (GL-A-AN? (CAR DES))
		      (SETQ DES (CADR DES))))
	     (RETURN (COND ((SETQ STR (GLGETSTR DES))
			    (GLNOTICETYPE DES)
			    (GLSTRFN IND STR DESLIST))
			   ((SETQ UNITREC (GLUNIT? DES))
			    (GLGETFROMUNIT UNITREC IND DES))
			   ((EQ IND DES)
			    (LIST NIL (CADR DES)))
			   (T NIL))))
	    ((NOT (PAIRP DES))
	     (GLERROR 'GLSTRFN
		      (LIST "Bad structure specification" DES))))
      (SETQ DESIND (CAR DES))
      (COND ((OR (EQ IND DES)
		 (EQ DESIND IND))
	     (RETURN (LIST NIL (CADR DES)))))
      (RETURN (CASEQ DESIND (CONS (OR (GLSTRVALB IND (CADR DES)
						 '(CAR *GL*))
				      (GLSTRVALB IND (CADDR DES)
						 '(CDR *GL*))))
		     ((LIST LISTOBJECT)
		      (GLLISTSTRFN IND DES DESLIST))
		     ((PROPLIST ALIST RECORD ATOMOBJECT OBJECT)
		      (GLPROPSTRFN IND DES DESLIST NIL))
		     (ATOM (GLATOMSTRFN IND DES DESLIST))
		     (TRANSPARENT (GLSTRFN IND (CADR DES)
					   DESLIST))
		     (T (COND ((AND (SETQ TMP (ASSOC DESIND GLUSERSTRNAMES))
				    (CADR TMP))
			       (APPLY (CADR TMP)
				      (LIST IND DES DESLIST)))
			      ((OR (NULL (CDR DES))
				   (ATOM (CADR DES))
				   (AND (PAIRP (CADR DES))
					(GL-A-AN? (CAADR DES))))
			       NIL)
			      (T (GLSTRFN IND (CADR DES)
					  DESLIST))))))))


% GSN 16-MAR-83 10:49 
% If STR is a structured object, i.e., either a declared GLISP 
%   structure or a Class of Units, get the property PROP from the 
%   GLISP class of properties GLPROP. 
(DE GLSTRPROP (STR GLPROP PROP ARGS)
(PROG (STRB UNITREC GLPROPS PROPL TMP SUPERS)
      (OR (ATOM (SETQ STRB (GLXTRTYPE STR)))
	  (RETURN NIL))
      (COND ((SETQ GLPROPS (GET STRB 'GLSTRUCTURE))
	     (GLNOTICETYPE STRB)
	     (COND ((AND (SETQ PROPL (LISTGET (CDR GLPROPS)
					      GLPROP))
			 (SETQ TMP (GLSTRPROPB PROP PROPL ARGS)))
		    (RETURN TMP)))))
      (SETQ SUPERS (AND GLPROPS (LISTGET (CDR GLPROPS)
					 'SUPERS)))
      LP
      (COND (SUPERS (COND ((SETQ TMP (GLSTRPROP (CAR SUPERS)
						GLPROP PROP ARGS))
			   (RETURN TMP))
			  (T (SETQ SUPERS (CDR SUPERS))
			     (GO LP))))
	    ((AND (SETQ UNITREC (GLUNIT? STRB))
		  (SETQ TMP (APPLY (CADDDR UNITREC)
				   (LIST STRB GLPROP PROP))))
	     (RETURN TMP)))))


% GSN 10-FEB-83 13:14 
% See if the property PROP can be found within the list of properties 
%   PROPL. If ARGS is specified and ARGTYPES are specified for a 
%   property entry, ARGS are required to match ARGTYPES. 
(DE GLSTRPROPB (PROP PROPL ARGS)
(PROG (PROPENT ARGTYPES LARGS)
      LP
      (COND ((NULL PROPL)
	     (RETURN NIL)))
      (SETQ PROPENT (CAR PROPL))
      (SETQ PROPL (CDR PROPL))
      (COND ((NE (CAR PROPENT)
		 PROP)
	     (GO LP)))
      (OR (AND ARGS (SETQ ARGTYPES (LISTGET (CDDR PROPENT)
					    'ARGTYPES)))
	  (RETURN PROPENT))
      (SETQ LARGS ARGS)
      LPB
      (COND ((AND (NULL LARGS)
		  (NULL ARGTYPES))
	     (RETURN PROPENT))
	    ((OR (NULL LARGS)
		 (NULL ARGTYPES))
	     (GO LP))
	    ((GLTYPEMATCH (CADAR LARGS)
			  (CAR ARGTYPES))
	     (SETQ LARGS (CDR LARGS))
	     (SETQ ARGTYPES (CDR ARGTYPES))
	     (GO LPB))
	    (T (GO LP)))))


% edited: 11-JAN-82 14:58 
% GLSTRVAL is a subroutine of GLSTRFN. Given an old partial retrieval 
%   function, in which the item from which the retrieval is made is 
%   specified by *GL*, and a new function to compute *GL*, a composite 
%   function is made. 
(DE GLSTRVAL (OLDFN NEW)
(PROG NIL (COND ((CAR OLDFN)
		 (RPLACA OLDFN (SUBST NEW '*GL*
				      (CAR OLDFN))))
		(T (RPLACA OLDFN NEW)))
      (RETURN OLDFN)))


% edited: 13-Aug-81 16:13 
% If the indicator IND can be found within the description DES, make a 
%   composite retrieval function using a copy of the function pattern 
%   NEW. 
(DE GLSTRVALB (IND DES NEW)
(PROG (TMP)
      (COND ((SETQ TMP (GLSTRFN IND DES DESLIST))
	     (RETURN (GLSTRVAL TMP (COPY NEW))))
	    (T (RETURN NIL)))))


% edited: 30-DEC-81 16:35 
(DE GLSUBATOM (X Y Z)
(OR (SUBATOM X Y Z)
    '*NIL*))


% GSN 22-JAN-83 16:27 
% Same as SUBLIS, but allows first elements in PAIRS to be non-atomic. 
(DE GLSUBLIS (PAIRS EXPR)
(PROG (TMP)
      (RETURN (COND ((SETQ TMP (ASSOC EXPR PAIRS))
		     (CDR TMP))
		    ((NOT (PAIRP EXPR))
		     EXPR)
		    (T (CONS (GLSUBLIS PAIRS (CAR EXPR))
			     (GLSUBLIS PAIRS (CDR EXPR))))))))


% edited: 30-AUG-82 10:29 
% Make subtype substitutions within TYPE according to GLTYPESUBS. 
(DE GLSUBSTTYPE (TYPE SUBS)
(SUBLIS SUBS TYPE))


% edited: 11-NOV-82 14:02 
% Get the list of superclasses for CLASS. 
(DE GLSUPERS (CLASS)
(PROG (TMP)
      (RETURN (AND (SETQ TMP (GET CLASS 'GLSTRUCTURE))
		   (LISTGET (CDR TMP)
			    'SUPERS)))))


% GSN 16-FEB-83 11:56 
% edited: 17-Apr-81 14:23 
% EXPR begins with THE. Parse the expression and return code. 
(DE GLTHE (PLURALFLG)
(PROG (SOURCE SPECS NAME QUALFLG DTYPE NEWCONTEXT LOOPVAR LOOPCOND TMP)
      
% Now trace the path specification. 

      (GLTHESPECS)
      (SETQ QUALFLG
	    (AND EXPR
		 (MEMQ (CAR EXPR)
		       '(with With
			   WITH who Who WHO which Which WHICH that That THAT)))
	    )
      B
      (COND ((NULL SPECS)
	     (COND ((MEMQ (CAR EXPR)
			  '(IS Is is HAS Has has ARE Are are))
		    (RETURN (GLPREDICATE SOURCE CONTEXT T NIL)))
		   (QUALFLG (GO C))
		   (T (RETURN SOURCE))))
	    ((AND QUALFLG (NOT PLURALFLG)
		  (NULL (CDR SPECS)))
	     
% If this is a definite reference to a qualified entity, make the name 
%   of the entity plural. 

	     (SETQ NAME (CAR SPECS))
	     (RPLACA SPECS (GLPLURAL (CAR SPECS)))))
      
% Try to find the next name on the list of SPECS from SOURCE. 

      (COND ((NULL SOURCE)
	     (OR (SETQ SOURCE (GLIDNAME (SETQ NAME (pop SPECS))
					NIL))
		 (RETURN (GLERROR 'GLTHE
				  (LIST "The definite reference to" NAME 
					"could not be found.")))))
	    (SPECS (SETQ SOURCE (GLGETFIELD SOURCE (pop SPECS)
					    CONTEXT))))
      (GO B)
      C
      (COND ((ATOM (SETQ DTYPE (GLXTRTYPE (CADR SOURCE))))
	     (SETQ DTYPE (GLXTRTYPE (GLGETSTR DTYPE)))))
      (COND ((OR (NOT (PAIRP DTYPE))
		 (NE (CAR DTYPE)
		     'LISTOF))
	     (GLERROR 'GLTHE
		      (LIST "The group name" NAME "has type" DTYPE 
			    "which is not a legal group type."))))
      (SETQ NEWCONTEXT (CONS NIL CONTEXT))
      (GLADDSTR (SETQ LOOPVAR (GLMKVAR))
		NAME
		(CADR DTYPE)
		NEWCONTEXT)
      (SETQ LOOPCOND
	    (GLPREDICATE (LIST LOOPVAR (CADR DTYPE))
			 NEWCONTEXT
			 (MEMQ (pop EXPR)
			       '(who Who WHO which Which WHICH that That THAT))
			 NIL))
      (SETQ TMP (GLGENCODE (LIST (COND (PLURALFLG 'SUBSET)
				       (T 'SOME))
				 (CAR SOURCE)
				 (LIST 'FUNCTION
				       (LIST 'LAMBDA
					     (LIST LOOPVAR)
					     (CAR LOOPCOND))))))
      (RETURN (COND (PLURALFLG (LIST TMP (CADR SOURCE)))
		    (T (LIST (LIST 'CAR
				   TMP)
			     (CADR DTYPE)))))))


% edited: 20-MAY-82 17:19 
% EXPR begins with THE. Parse the expression and return code in SOURCE 
%   and path names in SPECS. 
(DE GLTHESPECS NIL
(PROG NIL A (COND ((NULL EXPR)
		   (RETURN NIL))
		  ((MEMQ (CAR EXPR)
			 '(THE The the))
		   (pop EXPR)
		   (COND ((NULL EXPR)
			  (RETURN (GLERROR 'GLTHE
					   (LIST "Nothing following THE")))))))
      (COND ((ATOM (CAR EXPR))
	     (GLSEPINIT (CAR EXPR))
	     (COND ((EQ (GLSEPNXT)
			(CAR EXPR))
		    (SETQ SPECS (CONS (pop EXPR)
				      SPECS)))
		   (T (GLSEPCLR)
		      (SETQ SOURCE (GLDOEXPR NIL CONTEXT T))
		      (RETURN NIL))))
	    (T (SETQ SOURCE (GLDOEXPR NIL CONTEXT T))
	       (RETURN NIL)))
      
% SPECS contains a path specification. See if there is any more. 

      (COND ((MEMQ (CAR EXPR)
		   '(OF Of of))
	     (pop EXPR)
	     (GO A)))))


% edited: 14-DEC-81 10:51 
% Return a list of all transparent types defined for STR 
(DE GLTRANSPARENTTYPES (STR)
(PROG (TTLIST)
      (COND ((ATOM STR)
	     (SETQ STR (GLGETSTR STR))))
      (GLTRANSPB STR)
      (RETURN (REVERSIP TTLIST))))


% edited: 13-NOV-81 15:37 
% Look for TRANSPARENT substructures for GLTRANSPARENTTYPES. 
(DE GLTRANSPB (STR)
(COND ((NOT (PAIRP STR)))
      ((EQ (CAR STR)
	   'TRANSPARENT)
       (SETQ TTLIST (CONS STR TTLIST)))
      ((MEMQ (CAR STR)
	     '(LISTOF ALIST PROPLIST)))
      (T (MAPC (CDR STR)
	       (FUNCTION GLTRANSPB)))))


% edited:  4-JUN-82 11:18 
% Translate places where a PROG variable is initialized to a value as 
%   allowed by Interlisp. This is done by adding a SETQ to set the 
%   value of each PROG variable which is initialized. In some cases, a 
%   change of variable name is required to preserve the same 
%   semantics. 
(DE GLTRANSPROG (X)
(PROG (TMP ARGVALS SETVARS)
      (MAP (CADR X)
	   (FUNCTION (LAMBDA (Y)
		       (COND
			 ((PAIRP (CAR Y))
			   
% If possible, use the same variable; otherwise, make a new one. 

			   (SETQ TMP
			     (COND
			       ((OR (SOME (CADR X)
					  (FUNCTION (LAMBDA (Z)
						      (AND
							(PAIRP Z)
							(GLOCCURS
							  (CAR Z)
							  (CADAR Y))))))
				    (SOME ARGVALS (FUNCTION (LAMBDA (Z)
							      (GLOCCURS
								(CAAR Y)
								Z)))))
				 (GLMKVAR))
			       (T (CAAR Y))))
			   (SETQ SETVARS (ACONC SETVARS (LIST 'SETQ
							      TMP
							      (CADAR Y))))
			   (SUBSTIP TMP (CAAR Y)
				    (CDDR X))
			   (SETQ ARGVALS (CONS (CADAR Y)
					       ARGVALS))
			   (RPLACA Y TMP))))))
      (COND (SETVARS (RPLACD (CDR X)
			     (NCONC SETVARS (CDDR X)))))
      (RETURN X)))


% GSN 10-FEB-83 13:31 
% See if the type SUBTYPE matches the type TYPE, either directly or 
%   because TYPE is a SUPER of SUBTYPE. 
(DE GLTYPEMATCH (SUBTYPE TYPE)
(PROG NIL (SETQ SUBTYPE (GLXTRTYPE SUBTYPE))
      (RETURN (OR (NULL SUBTYPE)
		  (NULL TYPE)
		  (EQ TYPE 'ANYTHING)
		  (EQUAL SUBTYPE TYPE)
		  (SOME (GLSUPERS SUBTYPE)
			(FUNCTION (LAMBDA (Y)
				    (GLTYPEMATCH Y TYPE))))))))


% GSN  3-FEB-83 14:41 
% Remove the GLISP-compiled definition and properties of GLAMBDAFN 
(DE GLUNCOMPILE (GLAMBDAFN)
(PROG (SPECS SPECLST STR LST TMP)
      (OR (GET GLAMBDAFN 'GLCOMPILED)
	  (SETQ SPECS (GET GLAMBDAFN 'GLSPECIALIZATION))
	  (RETURN NIL))
      (COND ((NOT GLQUIETFLG)
	     (PRIN1 "uncompiling ")
	     (PRIN1 GLAMBDAFN)
	     (TERPRI)))
      (PUT GLAMBDAFN 'GLCOMPILED
	   NIL)
      (PUT GLAMBDAFN 'GLRESULTTYPE
	   NIL)
      (GLUNSAVEDEF GLAMBDAFN)
      (MAPC (GET GLAMBDAFN 'GLTYPESUSED)
	    (FUNCTION (LAMBDA (Y)
			(PUT Y 'GLFNSUSEDIN
			     (DELETIP GLAMBDAFN (GET Y 'GLFNSUSEDIN))))))
      (PUT GLAMBDAFN 'GLTYPESUSED
	   NIL)
      (OR SPECS (RETURN NIL))
      
% Uncompile a specialization of a generic function. 

      
% Remove the function definition so it will be garbage collected. 

      (PUTDDD GLAMBDAFN NIL)
      A
      (COND ((NULL SPECS)
	     (RETURN NIL)))
      (SETQ SPECLST (pop SPECS))
      (PUT (CAR SPECLST)
	   'GLINSTANCEFNS
	   (DELETIP GLAMBDAFN (GET (CAR SPECLST)
				   'GLINSTANCEFNS)))
      
% Remove the specialization entry in the datatype where it was 
%   created. 

      (OR (SETQ STR (GET (CADR SPECLST)
			 'GLSTRUCTURE))
	  (GO A))
      (SETQ LST (CDR STR))
      LP
      (COND ((NULL LST)
	     (GO A))
	    ((EQ (CAR LST)
		 (CADDR SPECLST))
	     (COND ((AND (SETQ TMP (ASSOC (CADDDR SPECLST)
					  (CADR LST)))
			 (EQ (CADR TMP)
			     GLAMBDAFN))
		    (RPLACA (CDR LST)
			    (DELETIP TMP (CADR LST)))))
	     (GO A))
	    (T (SETQ LST (CDDR LST))
	       (GO LP)))))


% edited: 27-MAY-82 13:08 
% GLUNITOP calls a function to generate code for an operation on a 
%   unit in a units package. UNITREC is the unit record for the units 
%   package, LHS and RHS the code for the left-hand side and 
%   right-hand side of the operation 
%   (in general, the (QUOTE GET') code for each side) , and OP is the 
%   operation to be performed. 
(DE GLUNITOP (LHS RHS OP)
(PROG (TMP LST UNITREC)
      
% 

      (SETQ LST GLUNITPKGS)
      A
      (COND ((NULL LST)
	     (RETURN NIL))
	    ((NOT (MEMQ (CAAR LHS)
			(CADAR LST)))
	     (SETQ LST (CDR LST))
	     (GO A)))
      (SETQ UNITREC (CAR LST))
      (COND ((SETQ TMP (ASSOC OP (CADDR UNITREC)))
	     (RETURN (APPLY (CDR TMP)
			    (LIST LHS RHS)))))
      (RETURN NIL)))


% edited: 27-MAY-82 13:08 
% GLUNIT? tests a given structure to see if it is a unit of one of the 
%   unit packages on GLUNITPKGS. If so, the value is the unit package 
%   record for the unit package which matched. 
(DE GLUNIT? (STR)
(PROG (UPS)
      (SETQ UPS GLUNITPKGS)
      LP
      (COND ((NULL UPS)
	     (RETURN NIL))
	    ((APPLY (CAAR UPS)
		    (LIST STR))
	     (RETURN (CAR UPS))))
      (SETQ UPS (CDR UPS))
      (GO LP)))


% GSN 28-JAN-83 11:15 
% Remove the GLISP-compiled definition of GLAMBDAFN 
(DE GLUNSAVEDEF (GLAMBDAFN)
(GLPUTHOOK GLAMBDAFN))


% GSN 27-JAN-83 13:58 
% Unwrap an expression X by removing extra stuff inserted during 
%   compilation. 
(DE GLUNWRAP (X BUSY)
(COND
  ((NOT (PAIRP X))
   X)
  ((NOT (ATOM (CAR X)))
   (ERROR 0 (LIST 'GLUNWRAP
		  X)))
  ((CASEQ
     (CAR X)
     ('GO
      X)
     ((PROG2 PROGN)
      (COND ((NULL (CDDR X))
	     (GLUNWRAP (CADR X)
		       BUSY))
	    (T (MAP (CDR X)
		    (FUNCTION (LAMBDA (Y)
				(RPLACA Y (GLUNWRAP
					  (CAR Y)
					  (AND BUSY (NULL (CDR Y))))))))
	       (GLEXPANDPROGN X BUSY NIL)
	       (COND ((NULL (CDDR X))
		      (CADR X))
		     (T X)))))
     (PROG1 (COND ((NULL (CDDR X))
		   (GLUNWRAP (CADR X)
			     BUSY))
		  (T (MAP (CDR X)
			  (FUNCTION
			    (LAMBDA (Y)
			      (RPLACA Y (GLUNWRAP (CAR Y)
						  (AND BUSY
						       (EQ Y (CDR X))))))))
		     (COND (BUSY (GLEXPANDPROGN (CDR X)
						BUSY NIL))
			   (T (RPLACA X 'PROGN)
			      (GLEXPANDPROGN X BUSY NIL)))
		     (COND ((NULL (CDDR X))
			    (CADR X))
			   (T X)))))
     (FUNCTION (RPLACA (CDR X)
		       (GLUNWRAP (CADR X)
				 BUSY))
	       (MAP (CDDR X)
		    (FUNCTION (LAMBDA (Y)
				(RPLACA Y (GLUNWRAP (CAR Y)
						    T)))))
	       X)
     ((MAP MAPC MAPCAR MAPCONC SUBSET SOME EVERY)
      (GLUNWRAPMAP X BUSY))
     (LAMBDA (MAP (CDDR X)
		  (FUNCTION (LAMBDA (Y)
			      (RPLACA Y (GLUNWRAP (CAR Y)
						  (AND BUSY
						       (NULL (CDR Y))))))))
       (GLEXPANDPROGN (CDR X)
		      BUSY NIL)
       X)
     (PROG (GLUNWRAPPROG X BUSY))
     (COND (GLUNWRAPCOND X BUSY))
     ((SELECTQ CASEQ)
      (GLUNWRAPSELECTQ X BUSY))
     ((UNION INTERSECTION LDIFFERENCE)
      (GLUNWRAPINTERSECT X))
     (T
       (COND
	 ((AND (EQ (CAR X)
		   '*)
	       (EQ GLLISPDIALECT 'INTERLISP))
	  X)
	 ((AND (NOT BUSY)
	       (CDR X)
	       (NULL (CDDR X))
	       (GLPURE (CAR X)))
	  (GLUNWRAP (CADR X)
		    NIL))
	 (T (MAP (CDR X)
		 (FUNCTION (LAMBDA (Y)
			     (RPLACA Y (GLUNWRAP (CAR Y)
						 T)))))
	    (COND
	      ((AND (CDR X)
		    (NULL (CDDR X))
		    (PAIRP (CADR X))
		    (GLCARCDR? (CAR X))
		    (GLCARCDR? (CAADR X))
		    (LESSP (PLUS (FlatSize2 (CAR X))
				 (FlatSize2 (CAADR X)))
			   9))
	       (RPLACA X
		       (IMPLODE
			 (CONS 'C
			       (REVERSIP (CONS 'R
					       (NCONC (GLANYCARCDR?
							(CAADR X))
						      (GLANYCARCDR?
							(CAR X))))))))
	       (RPLACA (CDR X)
		       (CADADR X))
	       (GLUNWRAP X BUSY))
	      ((AND (GET (CAR X)
			 'GLEVALWHENCONST)
		    (EVERY (CDR X)
			   (FUNCTION GLCONST?))
		    (OR (NOT (GET (CAR X)
				  'GLARGSNUMBERP))
			(EVERY (CDR X)
			       (FUNCTION NUMBERP))))
	       (EVAL X))
	      ((MEMQ (CAR X)
		     '(AND OR))
	       (GLUNWRAPLOG X))
	      (T X)))))))))


% GSN 27-JAN-83 13:57 
% Unwrap a COND expression. 
(DE GLUNWRAPCOND (X BUSY)
(PROG (RESULT)
      (SETQ RESULT X)
      A
      (COND ((NULL (CDR RESULT))
	     (GO B)))
      (RPLACA (CADR RESULT)
	      (GLUNWRAP (CAADR RESULT)
			T))
      (COND ((EQ (CAADR RESULT)
		 NIL)
	     (RPLACD RESULT (CDDR RESULT))
	     (GO A))
	    (T (MAP (CDADR RESULT)
		    (FUNCTION (LAMBDA (Y)
				(RPLACA Y (GLUNWRAP
					  (CAR Y)
					  (AND BUSY (NULL (CDR Y))))))))
	       (GLEXPANDPROGN (CADR RESULT)
			      BUSY NIL)))
      (COND ((EQ (CAADR RESULT)
		 T)
	     (RPLACD (CDR RESULT)
		     NIL)))
      (SETQ RESULT (CDR RESULT))
      (GO A)
      B
      (COND ((AND (NULL (CDDR X))
		  (EQ (CAADR X)
		      T))
	     (RETURN (CONS 'PROGN
			   (CDADR X))))
	    (T (RETURN X)))))


% GSN 17-FEB-83 13:40 
% Optimize intersections and unions of subsets of the same set: 
%   (INTERSECT (SUBSET S P) (SUBSET S Q)) -> (SUBSET S (AND P Q)) 
(DE GLUNWRAPINTERSECT (CODE)
(PROG
  (LHS RHS P Q QQ SA SB)
  (SETQ LHS (GLUNWRAP (CADR CODE)
		      T))
  (SETQ RHS (GLUNWRAP (CADDR CODE)
		      T))
  (OR (AND (PAIRP LHS)
	   (PAIRP RHS)
	   (EQ (CAR LHS)
	       'SUBSET)
	   (EQ (CAR RHS)
	       'SUBSET))
      (GO OUT))
  (PROGN (SETQ SA (GLUNWRAP (CADR LHS)
			    T))
	 (SETQ SB (GLUNWRAP (CADR RHS)
			    T)))
  
% Make sure the sets are the same. 

  (OR (EQUAL SA SB)
      (GO OUT))
  (PROGN (SETQ P (GLXTRFN (CADDR LHS)))
	 (SETQ Q (GLXTRFN (CADDR RHS))))
  (SETQ QQ (SUBST (CAR P)
		  (CAR Q)
		  (CADR Q)))
  (RETURN
    (GLGENCODE
      (LIST 'SUBSET
	    SA
	    (LIST 'FUNCTION
		  (LIST 'LAMBDA
			(LIST (CAR P))
			(GLUNWRAP (CASEQ (CAR CODE)
					 (INTERSECTION (LIST 'AND
							     (CADR P)
							     QQ))
					 (UNION (LIST 'OR
						      (CADR P)
						      QQ))
					 (LDIFFERENCE
					   (LIST 'AND
						 (CADR P)
						 (LIST 'NOT
						       QQ)))
					 (T (ERROR 0 NIL)))
				  T))))))
  OUT
  (MAP (CDR CODE)
       (FUNCTION (LAMBDA (Y)
		   (RPLACA Y (GLUNWRAP (CAR Y)
				       T)))))
  (RETURN CODE)))


% GSN 16-MAR-83 10:50 
% Unwrap a logical expression by performing constant transformations 
%   and splicing in sublists of the same type, e.g., (AND X (AND Y Z)) 
%   -> (AND X Y Z) . 
(DE GLUNWRAPLOG (X)
(PROG (Y LAST)
      (SETQ Y (CDR X))
      (SETQ LAST X)
      LP
      (COND ((NULL Y)
	     (GO OUT))
	    ((OR (AND (NULL (CAR Y))
		      (EQ (CAR X)
			  'AND))
		 (AND (EQ (CAR Y)
			  T)
		      (EQ (CAR X)
			  'OR)))
	     (RPLACD Y NIL))
	    ((OR (AND (NULL (CAR Y))
		      (EQ (CAR X)
			  'OR))
		 (AND (EQ (CAR Y)
			  T)
		      (EQ (CAR X)
			  'AND)))
	     (SETQ Y (CDR Y))
	     (RPLACD LAST Y)
	     (GO LP))
	    ((AND (PAIRP (CAR Y))
		  (EQ (CAAR Y)
		      (CAR X)))
	     (RPLACD (LASTPAIR (CAR Y))
		     (CDR Y))
	     (RPLACD Y (CDDAR Y))
	     (RPLACA Y (CADAR Y))))
      (SETQ Y (CDR Y))
      (SETQ LAST (CDR LAST))
      (GO LP)
      OUT
      (COND ((NULL (CDR X))
	     (RETURN (EQ (CAR X)
			 'AND)))
	    ((NULL (CDDR X))
	     (RETURN (CADR X))))
      (RETURN X)))


% edited: 19-OCT-82 16:03 
% Unwrap and optimize mapping-type functions. 
(DE GLUNWRAPMAP (X BUSY)
(PROG (LST FN OUTSIDE INSIDE OUTFN INFN NEWFN NEWMAP TMPVAR NEWLST)
      (PROGN (SETQ LST (GLUNWRAP (CADR X)
				 T))
	     (SETQ FN (GLUNWRAP (CADDR X)
				(NOT (MEMQ (CAR X)
					   '(MAPC MAP))))))
      (COND ((OR (NOT (MEMQ (SETQ OUTFN (CAR X))
			    '(SUBSET MAPCAR MAPC MAPCONC)))
		 (NOT (AND (PAIRP LST)
			   (MEMQ (SETQ INFN (CAR LST))
				 '(SUBSET MAPCAR)))))
	     (GO OUT)))
      
% Optimize compositions of mapping functions to avoid construction of 
%   lists of intermediate results. 

      
% These optimizations are not correct if the mapping functions have 
%   interdependent side-effects. However, these are likely to be very 
%   rare, so we do it anyway. 

      (SETQ OUTSIDE (GLXTRFN FN))
      (SETQ INSIDE (GLXTRFN (PROGN (SETQ NEWLST (CADR LST))
				   (CADDR LST))))
      (CASEQ INFN (SUBSET (CASEQ OUTFN ((SUBSET MAPCONC)
				  (SETQ NEWMAP OUTFN)
				  (SETQ NEWFN (LIST 'AND
						    (CADR INSIDE)
						    (SUBST (CAR INSIDE)
							   (CAR OUTSIDE)
							   (CADR OUTSIDE)))))
				 (MAPCAR (SETQ NEWMAP 'MAPCONC)
					 (SETQ
					   NEWFN
					   (LIST 'AND
						 (CADR INSIDE)
						 (LIST 'CONS
						       (SUBST (CAR INSIDE)
							      (CAR OUTSIDE)
							      (CADR OUTSIDE))
						       NIL))))
				 (MAPC (SETQ NEWMAP 'MAPC)
				       (SETQ NEWFN (LIST 'AND
							 (CADR INSIDE)
							 (SUBST (CAR INSIDE)
								(CAR OUTSIDE)
								(CADR OUTSIDE))
							 )))
				 (T (ERROR 0 NIL))))
	     (MAPCAR (SETQ NEWFN (LIST 'PROG
				       (LIST (SETQ TMPVAR (GLMKVAR)))
				       (LIST 'SETQ
					     TMPVAR
					     (CADR INSIDE))
				       (LIST 'RETURN
					     '*GLCODE*)))
		     (CASEQ OUTFN (SUBSET (SETQ NEWMAP 'MAPCONC)
					  (SETQ
					    NEWFN
					    (SUBST (LIST 'AND
							 (SUBST TMPVAR
								(CAR OUTSIDE)
								(CADR OUTSIDE))
							 (LIST 'CONS
							       TMPVAR NIL))
						   '*GLCODE*
						   NEWFN)))
			    (MAPCAR (SETQ NEWMAP 'MAPCAR)
				    (SETQ NEWFN (SUBST (SUBST TMPVAR
							      (CAR OUTSIDE)
							      (CADR OUTSIDE))
						       '*GLCODE*
						       NEWFN)))
			    (MAPC (SETQ NEWMAP 'MAPC)
				  (SETQ NEWFN (SUBST (SUBST TMPVAR
							    (CAR OUTSIDE)
							    (CADR OUTSIDE))
						     '*GLCODE*
						     NEWFN)))
			    (T (ERROR 0 NIL))))
	     (T (ERROR 0 NIL)))
      (RETURN (GLUNWRAP (GLGENCODE (LIST NEWMAP NEWLST
					 (LIST 'FUNCTION
					       (LIST 'LAMBDA
						     (LIST (CAR INSIDE))
						     NEWFN))))
			BUSY))
      OUT
      (RETURN (GLGENCODE (LIST OUTFN LST FN)))))


% GSN 27-JAN-83 13:57 
% Unwrap a PROG expression. 
(DE GLUNWRAPPROG (X BUSY)
(PROG (LAST)
      (COND ((NE GLLISPDIALECT 'INTERLISP)
	     (GLTRANSPROG X)))
      
% First see if the PROG is not busy and ends with a RETURN. 

      (COND ((AND (NOT BUSY)
		  (SETQ LAST (LASTPAIR X))
		  (PAIRP (CAR LAST))
		  (EQ (CAAR LAST)
		      'RETURN))
	     
% Remove the RETURN. If atomic, remove the atom also. 

	     (COND ((ATOM (CADAR LAST))
		    (RPLACD (NLEFT X 2)
			    NIL))
		   (T (RPLACA LAST (CADAR LAST))))))
      
% Do any initializations of PROG variables. 

      (MAPC (CADR X)
	    (FUNCTION (LAMBDA (Y)
			(COND
			  ((PAIRP Y)
			    (RPLACA (CDR Y)
				    (GLUNWRAP (CADR Y)
					      T)))))))
      (MAP (CDDR X)
	   (FUNCTION (LAMBDA (Y)
		       (RPLACA Y (GLUNWRAP (CAR Y)
					   NIL)))))
      (GLEXPANDPROGN (CDR X)
		     BUSY T)
      (RETURN X)))


% GSN 27-JAN-83 13:57 
% Unwrap a SELECTQ or CASEQ expression. 
(DE GLUNWRAPSELECTQ (X BUSY)
(PROG (L SELECTOR)
      
% First unwrap the component expressions. 

      (RPLACA (CDR X)
	      (GLUNWRAP (CADR X)
			T))
      (MAP (CDDR X)
	   (FUNCTION
	     (LAMBDA (Y)
	       (COND
		 ((OR (CDR Y)
		      (EQ (CAR X)
			  'CASEQ))
		   (MAP (CDAR Y)
			(FUNCTION (LAMBDA (Z)
				    (RPLACA Z
					    (GLUNWRAP
					      (CAR Z)
					      (AND BUSY (NULL (CDR Z))))))))
		   (GLEXPANDPROGN (CAR Y)
				  BUSY NIL))
		 (T (RPLACA Y (GLUNWRAP (CAR Y)
					BUSY)))))))
      
% Test if the selector is a compile-time constant. 

      (COND ((NOT (GLCONST? (CADR X)))
	     (RETURN X)))
      
% Evaluate the selection at compile time. 

      (SETQ SELECTOR (GLCONSTVAL (CADR X)))
      (SETQ L (CDDR X))
      LP
      (COND ((NULL L)
	     (RETURN NIL))
	    ((AND (NULL (CDR L))
		  (EQ (CAR X)
		      'SELECTQ))
	     (RETURN (CAR L)))
	    ((AND (EQ (CAR X)
		      'CASEQ)
		  (EQ (CAAR L)
		      T))
	     (RETURN (GLUNWRAP (CONS 'PROGN
				     (CDAR L))
			       BUSY)))
	    ((OR (EQ SELECTOR (CAAR L))
		 (AND (PAIRP (CAAR L))
		      (MEMQ SELECTOR (CAAR L))))
	     (RETURN (GLUNWRAP (CONS 'PROGN
				     (CDAR L))
			       BUSY))))
      (SETQ L (CDR L))
      (GO LP)))


% edited:  5-MAY-82 15:49 
% Update the type of VAR to be TYPE. 
(DE GLUPDATEVARTYPE (VAR TYPE)
(PROG (CTXENT)
      (COND ((NULL TYPE))
	    ((SETQ CTXENT (GLFINDVARINCTX VAR CONTEXT))
	     (COND ((NULL (CADDR CTXENT))
		    (RPLACA (CDDR CTXENT)
			    TYPE))))
	    (T (GLADDSTR VAR NIL TYPE CONTEXT)))))


% GSN 23-JAN-83 15:31 
% edited:  7-Apr-81 10:44 
% Process a user-function, i.e., any function which is not specially 
%   compiled by GLISP. The function is tested to see if it is one 
%   which a unit package wants to compile specially; if not, the 
%   function is compiled by GLUSERFNB. 
(DE GLUSERFN (EXPR)
(PROG (FNNAME TMP UPS)
      (SETQ FNNAME (CAR EXPR))
      
% First see if a user structure-name package wants to intercept this 
%   function call. 

      (SETQ UPS GLUSERSTRNAMES)
      LPA
      (COND ((NULL UPS)
	     (GO B))
	    ((SETQ TMP (ASSOC FNNAME (CAR (CDDDDR (CAR UPS)))))
	     (RETURN (APPLY (CDR TMP)
			    (LIST EXPR CONTEXT)))))
      (SETQ UPS (CDR UPS))
      (GO LPA)
      B
      
% Test the function name to see if it is a function which some unit 
%   package would like to intercept and compile specially. 

      (SETQ UPS GLUNITPKGS)
      LP
      (COND ((NULL UPS)
	     (GO C))
	    ((AND (MEMQ FNNAME (CAR (CDDDDR (CAR UPS))))
		  (SETQ TMP (ASSOC 'UNITFN
				   (CADDR (CAR UPS)))))
	     (RETURN (APPLY (CDR TMP)
			    (LIST EXPR CONTEXT)))))
      (SETQ UPS (CDR UPS))
      (GO LP)
      C
      (COND ((AND (NOT (UNBOUNDP 'GLFNSUBS))
		  (SETQ TMP (ASSOC FNNAME GLFNSUBS)))
	     (RETURN (GLUSERFNB (CONS (CDR TMP)
				      (CDR EXPR)))))
	    (T (RETURN (GLUSERFNB EXPR))))))


% GSN 23-JAN-83 15:54 
% edited:  7-Apr-81 10:44 
% Parse an arbitrary function by getting the function name and then 
%   calling GLDOEXPR to get the arguments. 
(DE GLUSERFNB (EXPR)
(PROG (ARGS ARGTYPES FNNAME TMP)
      (SETQ FNNAME (pop EXPR))
      A
      (COND ((NULL EXPR)
	     (SETQ ARGS (REVERSIP ARGS))
	     (SETQ ARGTYPES (REVERSIP ARGTYPES))
	     (RETURN (COND ((AND (GET FNNAME 'GLEVALWHENCONST)
				 (EVERY ARGS (FUNCTION GLCONST?)))
			    (LIST (EVAL (CONS FNNAME ARGS))
				  (GLRESULTTYPE FNNAME ARGTYPES)))
			   (T (LIST (CONS FNNAME ARGS)
				    (GLRESULTTYPE FNNAME ARGTYPES))))))
	    ((SETQ TMP (OR (GLDOEXPR NIL CONTEXT T)
			   (PROG1 (GLERROR 'GLUSERFNB
					   (LIST 
			    "Function call contains illegal item.  EXPR ="
						 EXPR))
				  (SETQ EXPR NIL))))
	     (SETQ ARGS (CONS (CAR TMP)
			      ARGS))
	     (SETQ ARGTYPES (CONS (CADR TMP)
				  ARGTYPES))
	     (GO A)))))


% edited: 24-AUG-82 17:40 
% Get the arguments to an function call for use by a user compilation 
%   function. 
(DE GLUSERGETARGS (EXPR CONTEXT)
(PROG (ARGS TMP)
      (pop EXPR)
      A
      (COND ((NULL EXPR)
	     (RETURN (REVERSIP ARGS)))
	    ((SETQ TMP (OR (GLDOEXPR NIL CONTEXT T)
			   (PROG1 (GLERROR 'GLUSERFNB
					   (LIST 
			    "Function call contains illegal item.  EXPR ="
						 EXPR))
				  (SETQ EXPR NIL))))
	     (SETQ ARGS (CONS TMP ARGS))
	     (GO A)))))


% GSN 10-FEB-83 16:01 
% Try to perform an operation on a user-defined structure, which is 
%   LHS. The type of LHS is looked up on GLUSERSTRNAMES, and if found, 
%   the appropriate user function is called. 
(DE GLUSERSTROP (LHS OP RHS)
(PROG (TMP DES TMPB)
      (SETQ DES (CADR LHS))
      (COND ((NULL DES)
	     (RETURN NIL))
	    ((ATOM DES)
	     (COND ((NE (SETQ TMP (GLGETSTR DES))
			DES)
		    (RETURN (GLUSERSTROP (LIST (CAR LHS)
					       TMP)
					 OP RHS)))
		   (T (RETURN NIL))))
	    ((NOT (PAIRP DES))
	     (RETURN NIL))
	    ((AND (SETQ TMP (ASSOC (CAR DES)
				   GLUSERSTRNAMES))
		  (SETQ TMPB (ASSOC OP (CADDDR TMP))))
	     (RETURN (APPLY (CDR TMPB)
			    (LIST LHS RHS))))
	    (T (RETURN NIL)))))


% GSN 10-FEB-83 12:57 
% Get the value of the property PROP from SOURCE, whose type is given 
%   by TYPE. The property may be a field in the structure, or may be a 
%   PROP virtual field. 
% DESLIST is a list of object types which have previously been tried, 
%   so that a compiler loop can be prevented. 
(DE GLVALUE (SOURCE PROP TYPE DESLIST)
(PROG (TMP PROPL TRANS FETCHCODE)
      (COND ((MEMQ TYPE DESLIST)
	     (RETURN NIL))
	    ((SETQ TMP (GLSTRFN PROP TYPE DESLIST))
	     (RETURN (GLSTRVAL TMP SOURCE)))
	    ((SETQ PROPL (GLSTRPROP TYPE 'PROP
				    PROP NIL))
	     (SETQ TMP (GLCOMPMSGL (LIST SOURCE TYPE)
				   'PROP
				   PROPL NIL CONTEXT))
	     (RETURN TMP)))
      
% See if the value can be found in a TRANSPARENT subobject. 

      (SETQ TRANS (GLTRANSPARENTTYPES TYPE))
      B
      (COND ((NULL TRANS)
	     (RETURN NIL))
	    ((SETQ TMP (GLVALUE '*GL*
				PROP
				(GLXTRTYPE (CAR TRANS))
				(CONS (CAR TRANS)
				      DESLIST)))
	     (SETQ FETCHCODE (GLSTRFN (CAR TRANS)
				      TYPE NIL))
	     (GLSTRVAL TMP (CAR FETCHCODE))
	     (GLSTRVAL TMP SOURCE)
	     (RETURN TMP))
	    ((SETQ TMP (CDR TMP))
	     (GO B)))))


% edited: 16-DEC-81 12:00 
% Get the structure-description for a variable in the specified 
%   context. 
(DE GLVARTYPE (VAR CONTEXT)
(PROG (TMP)
      (RETURN (COND ((SETQ TMP (GLFINDVARINCTX VAR CONTEXT))
		     (OR (CADDR TMP)
			 '*NIL*))
		    (T NIL)))))


% edited:  3-DEC-82 10:24 
% Extract the code and variable from a FUNCTION list. If there is no 
%   variable, a new one is created. The result is a list of the 
%   variable and code. 
(DE GLXTRFN (FNLST)
(PROG (TMP)
      
% If only the function name is specified, make a LAMBDA form. 

      (COND ((ATOM (CADR FNLST))
	     (RPLACA (CDR FNLST)
		     (LIST 'LAMBDA
			   (LIST (SETQ TMP (GLMKVAR)))
			   (LIST (CADR FNLST)
				 TMP)))))
      (COND ((CDDDR (CADR FNLST))
	     (RPLACD (CDADR FNLST)
		     (LIST (CONS 'PROGN
				 (CDDADR FNLST))))))
      (RETURN (LIST (CAADR (CADR FNLST))
		    (CADDR (CADR FNLST))))))


% edited: 26-JUL-82 14:03 
% Extract an atomic type name from a type spec which may be either 
%   <type> or (A <type>) . 
(DE GLXTRTYPE (TYPE)
(COND ((ATOM TYPE)
       TYPE)
      ((NOT (PAIRP TYPE))
       NIL)
      ((AND (OR (GL-A-AN? (CAR TYPE))
		(EQ (CAR TYPE)
		    'TRANSPARENT))
	    (CDR TYPE)
	    (ATOM (CADR TYPE)))
       (CADR TYPE))
      ((MEMQ (CAR TYPE)
	     GLTYPENAMES)
       TYPE)
      ((ASSOC (CAR TYPE)
	      GLUSERSTRNAMES)
       TYPE)
      ((AND (ATOM (CAR TYPE))
	    (CDR TYPE))
       (GLXTRTYPE (CADR TYPE)))
      (T (GLERROR 'GLXTRTYPE
		  (LIST TYPE "is an illegal type specification."))
	 NIL)))


% edited: 26-JUL-82 14:02 
% Extract a -real- type from a type spec. 
(DE GLXTRTYPEB (TYPE)
(COND ((NULL TYPE)
       NIL)
      ((ATOM TYPE)
       (COND ((MEMQ TYPE GLBASICTYPES)
	      TYPE)
	     (T (GLXTRTYPEB (GLGETSTR TYPE)))))
      ((NOT (PAIRP TYPE))
       NIL)
      ((MEMQ (CAR TYPE)
	     GLTYPENAMES)
       TYPE)
      ((ASSOC (CAR TYPE)
	      GLUSERSTRNAMES)
       TYPE)
      ((AND (ATOM (CAR TYPE))
	    (CDR TYPE))
       (GLXTRTYPEB (CADR TYPE)))
      (T (GLERROR 'GLXTRTYPE
		  (LIST TYPE "is an illegal type specification."))
	 NIL)))


% edited:  1-NOV-82 16:38 
% Extract a -real- type from a type spec. 
(DE GLXTRTYPEC (TYPE)
(AND (ATOM TYPE)
     (NOT (MEMQ TYPE GLBASICTYPES))
     (GLXTRTYPE (GLGETSTR TYPE))))


% GSN  9-FEB-83 16:46 
(DF SEND (GLISPSENDARGS)
(GLSENDB (EVAL (CAR GLISPSENDARGS))
	 NIL
	 (CADR GLISPSENDARGS)
	 'MSG
	 (MAPCAR (CDDR GLISPSENDARGS)
		 (FUNCTION EVAL))))


% GSN  9-FEB-83 16:48 
(DF SENDC (GLISPSENDARGS)
(GLSENDB (EVAL (CAR GLISPSENDARGS))
	 (CADR GLISPSENDARGS)
	 (CADDR GLISPSENDARGS)
	 'MSG
	 (MAPCAR (CDDDR GLISPSENDARGS)
		 (FUNCTION EVAL))))


% GSN  9-FEB-83 16:46 
(DF SENDPROP (GLISPSENDPROPARGS)
(GLSENDB (EVAL (CAR GLISPSENDPROPARGS))
	 NIL
	 (CADR GLISPSENDPROPARGS)
	 (CADDR GLISPSENDPROPARGS)
	 (MAPCAR (CDDDR GLISPSENDPROPARGS)
		 (FUNCTION EVAL))))


% GSN  9-FEB-83 16:48 
(DF SENDPROPC (GLISPSENDPROPARGS)
(GLSENDB (EVAL (CAR GLISPSENDPROPARGS))
	 (CADR GLISPSENDPROPARGS)
	 (CADDR GLISPSENDPROPARGS)
	 (CADDDR GLISPSENDPROPARGS)
	 (MAPCAR (CDDDDR GLISPSENDPROPARGS)
		 (FUNCTION EVAL))))

(SETQ GLBASICTYPES '(ATOM INTEGER REAL NUMBER STRING BOOLEAN ANYTHING))

(SETQ GLTYPENAMES '(CONS LIST RECORD LISTOF ALIST ATOM OBJECT LISTOBJECT 
			 ATOMOBJECT))

(SETQ GLOBJECTNAMES NIL)


(GLISPOBJECTS


(GLTYPE (ATOM (PROPLIST (GLSTRUCTURE (CONS (STRDES ANYTHING)
					   (PROPLIST (PROP (LISTOF GLPROPENTRY)
							   )
						     (ADJ (LISTOF GLPROPENTRY))
						     (ISA (LISTOF GLPROPENTRY))
						     (MSG (LISTOF GLPROPENTRY))
						     (DOC ANYTHING)
						     (SUPERS (LISTOF GLTYPE))))
				     )
			(GLISPATOMNUMBER INTEGER)
			(GLPROPFNS (ALIST (STR (LISTOF GLPROPFNENTRY))
					  (PROP (LISTOF GLPROPFNENTRY))
					  (ADJ (LISTOF GLPROPFNENTRY))
					  (ISA (LISTOF GLPROPFNENTRY))
					  (MSG (LISTOF GLPROPFNENTRY))))
			(GLFNSUSEDIN (LISTOF GLFUNCTION))))
PROP    ((PROPS (PROP))
	 (ADJS (ADJ))
	 (ISAS (ISA))
	 (MSGS (MSG))))


(GLPROPENTRY (CONS (NAME ATOM)
		   (CONS (CODE ANYTHING)
			 (PROPLIST (RESULT GLTYPE)
				   (OPEN BOOLEAN))))
PROP    ((SHORTVALUE (NAME))))


(GLPROPFNENTRY (LIST (NAME ATOM)
		     (CODE ANYTHING)
		     (RESULT GLTYPE)))


(GLFUNCTION (ATOM (PROPLIST (GLORIGINALEXPR ANYTHING)
			    (GLCOMPILED ANYTHING)
			    (GLRESULTTYPE ANYTHING)
			    (GLARGUMENTTYPES (LISTOF ANYTHING))
			    (GLTYPESUSED (LISTOF GLTYPE)))))

)


(SETQ GLLISPDIALECT 'PSL)

(GLINIT)
