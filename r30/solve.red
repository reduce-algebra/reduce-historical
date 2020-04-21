COMMENT SOLVE MODULE;

%******************* Global Declarations ***************************;

SYMBOLIC;

FLAG('(!*SOLVEWRITE), 'SHARE);

ARRAY !!CF(12), !!INTERVAL(10,2), !!EXACT(10);

GLOBAL '(!!HIPOW !!GCD !*SOLVESINGULAR SM!* MP!* !*ALLBRANCH
         !*SOLVEWRITE !!ARBINT !*SOLVEINTERVAL !!INTERVALARRAY);

!*SOLVESINGULAR := T;  % Solves consistent, singular eqns (0=0) if T;
!*ALLBRANCH     := T;  % Returns all branches of solutions if T;
!*SOLVEWRITE    := T;  % Prints solutions if T;
%!*SOLVEINTERVAL = NIL;% Attempts to isolate insoluble, real roots if T;

!!INTERVALARRAY := '!!INTERVAL;  % Value is the name of an array used to
				 %   pass args to RealRoot routines;
!!ARBINT    := 0;                % Index for arbitrary constants;

%  !!HIPOW : SOLVECOEFF returns highest power of its arg in this
%  !!GCD   : SOLVECOEFF returns GCD of powers of its arg in this
%  !!CF    : Array of coeffs from SOLVECOEFF
%
%  SM!*      : List of solutions
%  MP!*      : List of multiplicities;

ALGEBRAIC MATRIX SOLN, MULTIPLICITY;

%******************* Utility Functions *****************************;

SYMBOLIC PROCEDURE RPLACX U;
BEGIN SCALAR CARU;
  CARU := CAR U;
  RETURN RPLACD(RPLACA(U,CDR U),CARU)
END;

SYMBOLIC PROCEDURE UNIVARIATEP F;
  % F is a standard form.  Non-nil iff F is univariate or a constant;
DOMAINP F OR
(DOMAINP LC F AND (DOMAINP RED F OR
                   ((MVAR F = MVAR RED F) AND UNIVARIATEP RED F) ));

SYMBOLIC SMACRO PROCEDURE SUBTRSQ(U,V);
   ADDSQ(U, NEGSQ V);

SYMBOLIC SMACRO PROCEDURE VARLIS U;
   %U is an r-polynomial.
   %value is an ordered list of variables in U;
   VARLIS1(U,NIL);

SYMBOLIC SMACRO PROCEDURE LFCTR U;
   COMMENT RETURNS LEFTFACTOR OF A PAIR.  USED BY
      SUMFACTORS IN IEQN.RED;
   CAAR U;

SYMBOLIC OPERATOR LCMD;

SYMBOLIC PROCEDURE LCMD(C,D);
   COMMENT C and D are prefix rational numbers.  Returns
      integer least-common-multiple of their denominators;
   LCM(DENR SIMP!* C, DENR SIMP!* D);

SYMBOLIC PROCEDURE VARLIS1(U,V);
   IF DOMAINP U THEN V
    ELSE VARLIS1(CDR U,VARLIS1(CDAR U,ORDAS(CAAAR U,V)));

SYMBOLIC PROCEDURE ORDAS(A,L);
   IF NULL L THEN LIST A
    ELSE IF A=CAR L THEN L
    ELSE IF ORDP(A,CAR L) THEN A . L
    ELSE CAR L . ORDAS(A,CDR L);

SYMBOLIC PROCEDURE RATNUMP X;
   COMMENT Returns T iff any prefix expression X is a rational
      number;
   ATOM NUMR(X:=SIMP!* X) AND ATOM DENR X;

FLAG ('(RATNUMP), 'DIRECT);

SYMBOLIC PROCEDURE KARGLIS(KNAME, KLIS);
   COMMENT KNAME evaluates to an atom and KLIS to a list of
      kernels.  Returns the list of kernels named KNAME in KLIS;
   IF NULL KLIS THEN NIL
   ELSE UNION(KARG1(KNAME, CAR KLIS), KARGLIS(KNAME,CDR KLIS));

SYMBOLIC PROCEDURE KARG1(KNAME, KRN);
   COMMENT KNAME evaluates to an atom and KRN to a kernel.
      Returns a list of kernels named KNAME in KRN;
   IF ATOM KRN THEN NIL
   ELSE IF CAR KRN=KNAME THEN UNION(KARGLIS(KNAME,CDR KRN),
      LIST(KRN))
   ELSE KARGLIS(KNAME, CDR KRN);

SYMBOLIC PROCEDURE ALLKERN ELST;
   COMMENT Returns list of all top-level kernels in the list of
      standard forms ELST;
   IF NULL ELST THEN NIL
   ELSE UNION(VARLIS CAR ELST, ALLKERN CDR ELST);

SYMBOLIC OPERATOR FREEOFKERN;

SYMBOLIC PROCEDURE FREEOFKERN(X,U);
   COMMENT Returns T iff any expression U is free of kernel X;
   IF ATOM X THEN FREEOF(U,X)
   ELSE FREEOF(SUBST('!!DUM,X,U),'!!DUM);

FLAG('(FREEOFKERN),'DIRECT);

SYMBOLIC PROCEDURE TOPKERN(EX, X);
   BEGIN COMMENT Returns list of toplevel kernels in the
     standard form EX that contain the kernel X;
   SCALAR ALLK, WITHX;
   ALLK := VARLIS EX;
   WHILE  ALLK DO<<
      IF NOT FREEOFKERN(X,CAR ALLK) THEN WITHX:=CAR ALLK.WITHX;
      ALLK:=CDR ALLK>>;
   RETURN WITHX
   END;

SYMBOLIC PROCEDURE COEFLIS(EX);
% EX is a standard form.
% Returns a list of the coefficients of the main variable
%   in ex in the form ((expon.coeff) (expon.coeff) ... ),
%   where the expon's occur in increasing order, and entries
%   do not occur of zero coefficients;
   BEGIN
      SCALAR X, ANS, OLDKORD, VAR;
      X := EX;
      IF DOMAINP(X) THEN
         RETURN (0 . X);
      VAR := MVAR(EX);
      WHILE (NOT DOMAINP(X)) AND MVAR(X)=VAR DO <<
         ANS := (LDEG(X) . LC(X)) . ANS;
         X := RED(X) >>;
      IF X THEN
         ANS := (0 . X) . ANS;
      RETURN ANS
   END;

%******************* Temporary Factoring Routine *******************;

% The following square free factoring routine, based on the Reduce
%   function SQFRF, will eventually be replaced by the Norman-Moore
%   complete factorization technique.;

FLUID '(!*GCD);

SYMBOLIC PROCEDURE FACTLIS(EX, KLIST);
% EX is a standard form.
% KLIST is a list of kernels.
% Returns a list of square free factors containing the elements of
% KLIST in the form ((integer exponent . standard form factor) ...).;
% Factors constant with respect to KLIST are discarded;
BEGIN
   SCALAR  FIRST, ANS, OLDGCD, OLDKORD; INTEGER EXPON;
   OLDGCD := !*GCD;                     
   !*GCD  := T;               % Must be on for SQFRF;
   OLDKORD := SETKORDER(KLIST);         
   EX := REORDER(EX);                   
   WHILE (NOT DOMAINP(EX)) AND (MVAR(EX) MEMBER KLIST) DO <<
      FIRST := PP(EX);
      IF NOT DOMAINP(FIRST) THEN <<
         % Non-zero roots;
         EX  := QUOTF(EX, FIRST);
         FIRST := SQFRF(FIRST);
         FOR EACH X IN FIRST DO
            IF NOT DOMAINP X THEN
               ANS :=  RPLACX X . ANS >>
      ELSE <<
         % Zero root (possibly multiple);
         ANS := (LDEG(EX) . !*K2F(MVAR(EX))) . ANS;
         EX  := QUOTF(EX, !*P2F(LPOW(EX))) >> >>;
   % Restore the state of the world;
   SETKORDER(OLDKORD);
   !*GCD  := OLDGCD;
   RETURN ANS
END;

%******************* SOLVE Statement ******************************;

SYMBOLIC PROCEDURE SIMPSOLVE ARGLIST;
    BEGIN
        INTEGER NARGS;
        NARGS := LENGTH(ARGLIST);       
        RETURN !*F2Q IF NARGS=1 THEN SOLVE0(CAR ARGLIST,NIL)
		      ELSE IF NARGS=2
		       THEN SOLVE0(CAR ARGLIST, CADR ARGLIST)
		      ELSE SOLVE0(CAR ARGLIST,'LST . CDR ARGLIST)
    END;

PUT ('SOLVE,'SIMPFN,'SIMPSOLVE);

%******************* Fundamental SOLVE Procedures ******************;

SYMBOLIC PROCEDURE SOLVE0(ELST, XLST);

   BEGIN COMMENT ELST is any prefix expression, including the
      kernel named LST with any number of arguments.  XLST is
      a kernel, perhaps named LST with any number of arguments.
      Solves eqns in ELST for vars in XLST, putting solutions
      and multiplicities in SOLN and MULTIPLICITIES.
      Prints SOLN if !*SOLVEWRITE is non-nil.
      Returns number of rows in global matrix SOLN;
   SCALAR FLST, VARS, NONLIN;  INTEGER NEQN, I;
   %/ MAYBELOADMATR();
   ALGEBRAIC CLEAR SOLN, MULTIPLICITY;
   SM!* := MP!* := NIL;
   IF NOT ATOM ELST  AND CAR ELST='LST THEN ELST:=CDR ELST
   ELSE ELST:=LIST ELST;
   NEQN:=0;
   WHILE  ELST DO <<FLST:= NUMR SIMP!* CAR ELST . FLST;
      NEQN:=NEQN+1;  ELST:= CDR ELST >>;
% Note that ELST and XLST are reversed from the order entered;
   IF NULL XLST THEN <<VARS := ALLKERN FLST;
	 WRITE "UNKNOWNS:";
	 MAPCAR(REVERSE VARS, FUNCTION MATHPRINT);
	 TERPRI()>>
   ELSE<<IF ATOM XLST OR NOT(CAR XLST='LST)THEN XLST:=LIST(XLST)
         ELSE XLST:=CDR XLST;
         WHILE  XLST DO<<
            VARS:=MVAR !*A2F CAR XLST.VARS;
	    XLST:= CDR XLST>>>>;
   IF NOT(NEQN=LENGTH VARS) THEN REDERR
    "SOLVE CALLED WITH UNEQUAL NUMBER OF EXPRESSIONS AND UNKNOWNS";
   IF NEQN=1 THEN
      IF NULL (FLST:=CAR FLST) THEN
        IF !*SOLVESINGULAR THEN <<!!ARBINT:=!!ARBINT+1;
	   CONSSMMP(SIMP!* LIST('ARBCOMPLEX,!!ARBINT), 1) >>
        ELSE RETURN 0
      ELSE <<VARS:=CAR VARS;
         SOLVE1(FLST./1, VARS, 1) >>
   COMMENT More than one equation;
   ELSE <<
      SM!* := TP1(SOLVESYS(FLST, VARS));
      MP!* := LIST(LIST(MK!*SQ(!*F2Q(1)))) >>;
   SM!* := MAPC2(SM!*, FUNCTION MK!*SQ);
   PUT('MULTIPLICITY, 'MATRIX, 'MAT . MP!*);
   PUT('SOLN, 'MATRIX, 'MAT . SM!*);
   IF !*SOLVEWRITE THEN
      MATPRI(SM!*, 'SOLN);
   RETURN LENGTH SM!*
   END;

SYMBOLIC PROCEDURE CONSSMMP(S, M);
   BEGIN COMMENT S is a standard quotient and M is an integer.
      Conses (S) to global variable SM!* and (M) to global
      variable MP!*;
   SM!* := LIST(S) . SM!*;
   MP!* := LIST(MK!*SQ(M./1)) . MP!*
   END;

SYMBOLIC PROCEDURE SOLVEF(F, V);
% F is a standard form, V is a kernel.  Returns a list of
% pairs, each of which car is a standard quotient and cdr an
% integer.  If the integer is positive, the SQ is a zero of
% F with multiplicity equal to the integer.  Otherwise it is
% an insoluble factor, with multiplicity the absolute value of
% the integer;
BEGIN SCALAR OLDSOLVEWRITE, ANS;
   OLDSOLVEWRITE := !*SOLVEWRITE;
   !*SOLVEWRITE := NIL;
   SOLVE0(MK!*SQ(!*F2Q(F)), V);
   ANS := PAIR(MAPCAR(SM!*, FUNCTION LAMBDA(X); SIMP!*(CAR(X))),
	       MAPCAR(MP!*, FUNCTION CAR) );
   !*SOLVEWRITE := OLDSOLVEWRITE;
   RETURN ANS
END;

%******************* Procedures for solving a single eqn ***********;

SYMBOLIC PROCEDURE SOLVE1 (EX, X, MUL);
   BEGIN COMMENT Factors standard quotient EX with respect to
      toplevel occurrences of X and kernels containing variable
      X.  Factors containing more than one such kernel
      are appended to SM!*, with a negative multiplicity
      indicating unsolvability, and SOLVE2 is applied
      to the other factors.  Integer MUL is the multiplicity
      passed from any previous factorizations.  Returns NIL;
   SCALAR E1, X1, TKLIST;  INTEGER MU;
   EX := NUMR EX;
   TKLIST := TOPKERN(EX,X);
   IF NULL TKLIST THEN RETURN NIL;
   EX := FACTLIS(EX, TKLIST);
   WHILE EX DO <<
      E1 := CDAR(EX);
      X1 := TOPKERN(E1, X);
      MU := MUL*CAAR EX;
      IF  X1 THEN
         IF NULL CDR X1 THEN
           SOLVE2(E1,CAR X1,X,MU)
	 ELSE IF SMEMQ('SOL,
            (X1:=SIMP!* LIST('SOL,MK!*SQ(E1./1), X))) THEN
	       CONSSMMP(E1./1, -MU)
         ELSE
           SOLVE1(X1,X,MU);
      EX := CDR(EX) >>
   END;

SYMBOLIC PROCEDURE SOLVE2(E1, X1, X, MU);
  BEGIN COMMENT E1 is a standard form, MU is an
      integer, X1 and X are kernels. Uses roots of unity, known
      inverses, together with quadratic, cubic and quartic
      formulas, treating other cases as unsolvable. Returns NIL;
  SCALAR B, C, D, F;  INTEGER N;
  F:= ERRORSET(SOLVECOEFF(E1, X1),NIL,NIL);
  N:= !!GCD;

  COMMENT Test for single power of X1;
  IF ATOM(F) THEN CONSSMMP(E1./1, -MU)
  ELSE IF (F:=CAR F)=-1 THEN <<
    B:= LIST('EXPT, MK!*SQ QUOTSQ(NEGSQ SIMP!* GETELV(LIST('!!CF,0)),
      SIMP!* GETELV(LIST('!!CF,1))), MK!*SQ(1 ./!!GCD));
    FOR K := 0:N-1 DO <<
      SETELV(LIST('!!CF,1), SIMP!* LIST('TIMES,B,
        MKEXP LIST('QUOTIENT,LIST('TIMES,K,2,'PI),N)));

      COMMENT  x = b;
      IF X1=X THEN CONSSMMP(GETELV(LIST('!!CF, 1)), MU)

      COMMENT  LOG(x) = b;
      ELSE IF CAR X1 = 'LOG THEN SOLVE1           
         (SUBTRSQ(SIMP!* CADR X1,SIMP!* LIST('EXPT,'E,MK!*SQ
         GETELV(LIST('!!CF, 1)))),X,MU)

      ELSE IF CAR X1 = 'EXPT THEN

        COMMENT c**(...) = b;
	IF FREEOF(CADR X1,X) THEN <<
          IF !*ALLBRANCH THEN <<!!ARBINT:=!!ARBINT+1;
            C:=LIST('TIMES,2,'I,'PI,LIST('ARBINT,!!ARBINT)) >>
          ELSE C:=0;
          SOLVE1(SUBTRSQ(SIMP!* CADDR X1,QUOTSQ(ADDSQ(
	    SIMP!* LIST('LOG,MK!*SQ GETELV(LIST('!!CF, 1))),SIMP!* C),
	    SIMP!* LIST('LOG,CADR X1))),X,MU) >>

	ELSE IF FREEOF(CADDR X1,X) THEN

          COMMENT  (...)**(m/n) = b;
          IF RATNUMP CADDR X1 THEN SOLVE1(SUBTRSQ(
	    EXPTSQ(SIMP!* CADR X1,NUMR SIMP!* CADDR X1),
            SIMP!* LIST('EXPT,MK!*SQ GETELV(LIST('!!CF, 1)),MK!*SQ(DENR
            SIMP!* CADDR X1./1))),X,MU)

          COMMENT (...)**c = b;
          ELSE <<
            IF !*ALLBRANCH THEN <<!!ARBINT:=!!ARBINT+1;
              C:=MKEXP LIST('TIMES,LIST
                ('ARBREAL,!!ARBINT)) >>
            ELSE C:=1;
            SOLVE1(SUBTRSQ(SIMP!* CADR X1,MULTSQ(SIMP!*
	      LIST('EXPT,MK!*SQ GETELV(LIST('!!CF, 1)), MK!*SQ INVSQ
	      SIMP!* CADDR X1),SIMP!* C)), X, MU) >>

        COMMENT (...)**(...) = b : transcendental;
	ELSE CONSSMMP(SUBTRSQ(SIMP!* X1,GETELV(LIST('!!CF, 1))), -MU)

      COMMENT SIN(...) = b;
      ELSE IF CAR X1='SIN THEN<<
        IF !*ALLBRANCH THEN <<
          !!ARBINT:=!!ARBINT+1;
          F:=LIST('TIMES,2,'PI,LIST('ARBINT,!!ARBINT)) >>
        ELSE
          F:=0;
        C:=SIMP!* CADR X1;
        D:=LIST('ASIN,MK!*SQ GETELV(LIST('!!CF, 1)));
        SOLVE1(SUBTRSQ(C,SIMP!* LIST('PLUS,D,F)),X,MU);
        IF !*ALLBRANCH THEN SOLVE1(SUBTRSQ(C,SIMP!* LIST
          ('PLUS,'PI,MK!*SQ
          SUBTRSQ(SIMP!* F,SIMP!* D))), X, MU) >>

      COMMENT COS(...) = b;
      ELSE IF CAR X1='COS THEN<<
        IF !*ALLBRANCH THEN<<!!ARBINT:=!!ARBINT+1;
              C:=LIST('TIMES,2,'PI,LIST('ARBINT,!!ARBINT))>>
        ELSE C:=0;
        C:=SUBTRSQ(SIMP!* CADR X1,SIMP!* C);
        D:=SIMP!* LIST('ACOS,MK!*SQ GETELV(LIST('!!CF,1)));
        SOLVE1(SUBTRSQ(C,D), X, MU);
        IF !*ALLBRANCH THEN SOLVE1(ADDSQ(C,D), X, MU) >>   

      COMMENT Unknown inverse;
      ELSE IF NULL(F:=GET(CAR X1,'INVERSE))THEN
	CONSSMMP(SUBTRSQ(SIMP!* X1,GETELV(LIST('!!CF,1))), -MU)

      COMMENT Other, known inverse;
      ELSE SOLVE1(SUBTRSQ(SIMP!* CADR X1,SIMP!*
        LIST(F,MK!*SQ GETELV(LIST('!!CF,1)))), X, MU)>> >>      

  COMMENT Test for 2 powers of X1;
  ELSE IF F>=0 THEN <<
      D:= SIMP!* GETELV(LIST('!!CF,2));
      C := QUOTSQ(SIMP!* GETELV(LIST('!!CF,0)),D);
      D := QUOTSQ(SIMP!* GETELV(LIST('!!CF,1)),MULTSQ((2 ./1),D));
      C:=SIMP!* LIST('EXPT, MK!*SQ SUBTRSQ(EXPTSQ(D,2),C),
        MK!*SQ(1 ./2));
      D := ADDSQ(D, EXPTSQ(SIMP!* X1, N));
      SOLVE1(SUBTRSQ(D,C), X, MU);
      SOLVE1(ADDSQ(D,C), X, MU) >>
  ELSE SOLVE22(E1,X1,X,MU)
 END;

SYMBOLIC PROCEDURE SOLVE22(E1,X1,X,MU);
   BEGIN SCALAR B,C,D,F; INTEGER N;
    COMMENT Test for reciprocal equation, cubic, or quartic;
      F:=(!!HIPOW+1)/2;  D:=EXPTSQ(SIMP!* X1,N);
      IF (FOR J:=0:F DO IF NOT(GETELV(LIST('!!CF,J))
                               =GETELV(LIST('!!CF,!!HIPOW-J)) )
                        THEN RETURN T)
        THEN IF (FOR J:=0:F DO IF  NUMR ADDSQ(SIMP!*
          GETELV(LIST('!!CF,J)), SIMP!* GETELV(LIST('!!CF,!!HIPOW-J)))
             THEN RETURN T)
          THEN IF !!HIPOW=3 THEN SOLVECUBIC(D,X,MU,T)
            ELSE IF !!HIPOW=4 THEN SOLVEQUARTIC(D,X,MU)
              ELSE IF !*SOLVEINTERVAL AND UNIVARIATEP E1 THEN
                     SOLVEINTERVAL(E1,MU)
		ELSE CONSSMMP(E1./1, -MU)

        COMMENT Antisymmetric reciprocal equation;
        ELSE <<  C:=ADDSQ(D,(-1 ./1));
          SOLVE1(C, X, MU);
          E1:= QUOTSQ(E1./1, C);
          IF F+F = !!HIPOW THEN <<C:=ADDSQ(D,(1 ./1));
            SOLVE1(C, X, MU);
            E1:= QUOTSQ(E1, C) >>;
          SOLVE1(E1, X, MU) >>

      COMMENT Symmetric reciprocal equation;
      ELSE IF F+F=!!HIPOW+1 THEN <<
          C:=ADDSQ(D, 1 ./1);
          SOLVE1(C,X,MU);
          SOLVE1(QUOTSQ(E1./1, C), X, MU) >>
        ELSE <<
	  B:=SM!*;
          SETELV(LIST('!!CF, 0), SIMP!* 2);
          SETELV(LIST('!!CF, 1), SIMP!* '!!X);
          C:=ADDSQ(MULTSQ(SIMP!* GETELV(LIST('!!CF,F+1)),
			  GETELV(LIST('!!CF,1))),
			  SIMP!* GETELV(LIST('!!CF,F)));
          FOR J:=2:F DO <<
	    SETELV(LIST('!!CF, J),
		   SUBTRSQ(MULTSQ(GETELV(LIST('!!CF,1)),
				  GETELV(LIST('!!CF,J-1))),
			   GETELV(LIST('!!CF,J-2))));
            C:=ADDSQ(C,MULTSQ(GETELV(LIST('!!CF,J)),
                              SIMP!* GETELV(LIST('!!CF,F+J)) )) >>;
          SOLVE1(C,'!!X,MU);  C:=F:=NIL;
	  WHILE NOT(SM!*=B) DO <<
	    C:=CAR SM!* . C;
	    F:=CAR MP!* . F;
	    SM!*:=CDR SM!*;
	    MP!*:=CDR MP!* >>;
          WHILE  C DO <<
            SOLVE1(ADDSQ(1 ./1,MULTSQ(D,SUBTRSQ(D,CAAR C))),
               X, !*A2F CAAR F*MU);
	    C:=CDR C >>  >>
  END;

SYMBOLIC PROCEDURE MKEXP U;
   (LAMBDA X;
      LIST('PLUS,LIST('COS,X),LIST('TIMES,'I,LIST('SIN,X))))
   REVAL U;

SYMBOLIC PROCEDURE SOLVECOEFF(EX, VAR);
% EX is a standard form.
% VAR is a kernel.
% Puts the coefficients (as prefix standard quotients) of
%    VAR in EX into the elements of !!CF, with index equal
%    to the exponent divided by the GCD of all the
%    exponents.  This GCD is put into !!GCD, and the
%    highest power divided by the GCD is put into
%    !!HIPOW.
% Returns the lowest power if the highest is equal to 2;
%    -1 if the highest power is less than 2, or -1 if
%    the highest power is greater than 2.
% This bizarre behaviour stems from the rewriting of the
%    Reduce COEFF function originally used by SOLVE.
%    Hopefully this will be rewritten someday without
%    the kludginess.
% Note that !!CF (an array), !!GCD, and !!HIPOW are globals.;
BEGIN
   SCALAR CLIST, X, OLDKORD;
   OLDKORD := SETKORDER(LIST(VAR));
   CLIST := REORDER (EX);
   SETKORDER(OLDKORD);
   !!HIPOW := LDEG(CLIST);
   CLIST := COEFLIS(CLIST);
   !!GCD := 0;
   X := CLIST;
   WHILE X DO <<
      !!GCD := GCDN(CAAR(X), !!GCD);
      X := CDR(X) >>;
   X := CLIST;
   FOR I := 0:(CAR(DIMENSION('!!CF))-1) DO
      SETELV(LIST('!!CF, I), NIL);
   WHILE X DO <<
      SETELV(LIST('!!CF, CAAR(X)/!!GCD), MK!*SQ(CDAR(X) ./ 1));
      X := CDR(X) >>;
   !!HIPOW := !!HIPOW/!!GCD;
   IF !!HIPOW=2 THEN
      RETURN CAAR(CLIST)/!!GCD
   ELSE IF !!HIPOW<2 THEN
      RETURN -1
   ELSE
      RETURN -2
END;

SYMBOLIC PROCEDURE SOLVEINTERVAL(EX, MUL);
% EX is a standard form,  MUL is an integer.   Isolates
% insoluble, real roots  of EX  in rational  intervals,
% stuffing result in the form  INTERVL(Lowlim,Highlim)
% into SM!* with multiplicity MUL put into MP!*.;
BEGIN  INTEGER I;
  REALROOT(PREPF EX,PREPSQ !*K2Q MVAR EX,!!INTERVALARRAY,'!!EXACT);
  FOR I := 1:GETELV LIST('!!EXACT,0) DO
    CONSSMMP(SIMP!* GETELV LIST('!!EXACT,I), MUL);
  FOR I := 1:GETELV LIST(!!INTERVALARRAY,0,0) DO
    CONSSMMP(SIMP!* LIST('INTERVL,
                         GETELV LIST(!!INTERVALARRAY,I,1),
                         GETELV LIST(!!INTERVALARRAY,I,2) ),
             MUL)
END;

SYMBOLIC PROCEDURE REALROOT(U,V,W,X);
   REDERR("Real root finding not yet implemented");


%***************** Procedures for solving Cubic and Quartic eqns ***;

SYMBOLIC PROCEDURE SOLVECUBIC(X1, X, MU, CUBE3) ;
   BEGIN COMMENT Solves !!CF(3)*X1**3 + !!CF(2)*X1**2 ...
      X1 and X are
      kernels, M and MU are integers, CUBE3 is T or NIL.
      Returns NIL;
   SCALAR A,B,C,D;
   D:=SIMP!* GETELV(LIST('!!CF,3));
   C:=QUOTSQ(SIMP!* GETELV(LIST('!!CF,2)),D);
   B:=QUOTSQ(SIMP!* GETELV(LIST('!!CF,1)),D);
   A:=QUOTSQ(SIMP!* GETELV(LIST('!!CF,0)),D);
   A:=MULTSQ(ADDSQ(MULTSQ((9 ./1),MULTSQ(C,B)), ADDSQ(MULTSQ
      ((-27 ./1),A),MULTSQ((-2 ./1),EXPTSQ(C,3)))),(1 ./54));
   B := MULTSQ((-1 ./9),ADDSQ(EXPTSQ(C,2),MULTSQ((-3 ./1),B)));
   D := SIMP!* LIST('EXPT, MK!*SQ ADDSQ(EXPTSQ(B,3),
      EXPTSQ(A,2)), MK!*SQ(1 ./2));
   D := SIMP!* LIST('EXPT, MK!*SQ ADDSQ(A,D),MK!*SQ(1 ./3));
   A := NEGSQ QUOTSQ(B,D);
   B := ADDSQ(D, A);
   C := ADDSQ(X1, MULTSQ(C,(1 ./3)));
   SOLVE1(SUBTRSQ(C,B), X, MU);
   IF CUBE3 THEN <<C := ADDSQ(MULTSQ(B,(1 ./2)), C);
      D := MULTSQ(SIMP!* LIST('EXPT,MK!*SQ(-3 ./4),MK!*SQ
         (1 ./2)), SUBTRSQ(D,A));
      SOLVE1(ADDSQ(C,D), X, MU);
      SOLVE1(SUBTRSQ(C,D), X, MU)>>
   END;

SYMBOLIC PROCEDURE SOLVEQUARTIC(X1,X,MU) ;
   BEGIN COMMENT Solves !!CF(4)*X1**4 + !!CF(3)*X1**3 + ....
      X1 is a standard quotient, X is a kernel, MU is an integer,
      CUBE3 is T or NIL.  Returns NIL;
   SCALAR A,B,C,D,F;
   F:=SIMP!* GETELV(LIST('!!CF,4));
   A:=QUOTSQ(SIMP!* GETELV(LIST('!!CF,0)),F);
   B:=QUOTSQ(SIMP!* GETELV(LIST('!!CF,1)),F);
   C:=QUOTSQ(SIMP!* GETELV(LIST('!!CF,2)),F);
   D:=QUOTSQ(SIMP!* GETELV(LIST('!!CF,3)),F);
   F := ADDSQ(EXPTSQ(D,2), MULTSQ((-4 ./1),C));
   SETELV(LIST('!!CF, 0), MK!*SQ NEGSQ ADDSQ(EXPTSQ(B,2),MULTSQ(A,F)));
   SETELV(LIST('!!CF, 1), MK!*SQ ADDSQ(MULTSQ(B,D),MULTSQ((-4 ./1),A)));
   SETELV(LIST('!!CF, 2), MK!*SQ NEGSQ C);
   SETELV(LIST('!!CF, 3), 1);
   SOLVECUBIC(SIMP!* X, X, MU, NIL);
   B := CAAR SM!*;
   SM!* := CDR SM!*;
   MP!*:= CDR MP!*;
   A := SIMP!* LIST('EXPT, MK!*SQ ADDSQ(EXPTSQ(B,2),MULTSQ(A,
      (-4 ./1))), MK!*SQ(1 ./2));
   F := SIMP!* LIST('EXPT, MK!*SQ ADDSQ(F,MULTSQ(B,(4 ./1))),
      MK!*SQ(1 ./2));
   SOLVE1(ADDSQ(EXPTSQ(X1,2),MULTSQ((1 ./2),ADDSQ(MULTSQ(X1,ADDSQ
      (D,F)), ADDSQ(B,A)))), X, MU);
   SOLVE1(ADDSQ(EXPTSQ(X1,2),MULTSQ((1 ./2),ADDSQ(MULTSQ(X1,
      SUBTRSQ(D,F)), SUBTRSQ(B,A)))), X, MU);
   END;

%******************* Procedures for solving a system of eqns *******;

SYMBOLIC PROCEDURE SOLVESYS(EXLIST,VARLIST);
% EXLIST is a list of standard forms.
% VARLIST is a list of kernels.
% If EXLIST and VARLIST are of the same length and the
%   elements of VARLIST are linear in the elements of
%   exlist, and further the system of linear eqns so
%   defined is non-singular, then SOLVESYS returns a
%   list of standard quotients which are solutions of
%   the system, ordered as in VARLIST.
% Otherwise an error results.;
BEGIN
   SCALAR MTRX, RHS;    % Coeffs and right side of system;
   SCALAR ROW, OLDKORD;
   IF LENGTH(EXLIST) NEQ LENGTH(VARLIST) THEN
      REDERR "SOLVESYS given unequal number of eqns & unknowns";
   OLDKORD := SETKORDER(VARLIST);
   EXLIST := MAPCAR(EXLIST, 'REORDER);
   FOR EACH EX IN EXLIST DO <<
      ROW := NIL;
      FOR EACH VAR IN VARLIST DO<<
         IF NOT DOMAINP(EX) AND
            (MVAR(EX)=VAR AND LDEG(EX)>1
             OR (NOT FREEOFKERN(VAR, LC(EX)))
             OR (NOT FREEOFKERN(VAR, RED(EX))) ) THEN
               REDERR
       "SOLVE given system of non linear-fractional equations";
         IF NOT DOMAINP(EX) AND MVAR(EX)=VAR THEN <<
            ROW := !*F2Q(LC(EX)) . ROW;
            EX := RED(EX) >>
         ELSE
            ROW := !*F2Q(NIL) . ROW >>;
      RHS := LIST(!*F2Q(NEGF(EX))) . RHS;
      MTRX := ROW . MTRX >>;
   SETKORDER(OLDKORD);
   RETURN SOLVELNRSOLVE(MTRX, RHS)
END;

SYMBOLIC PROCEDURE SOLVELNRSOLVE(U,V);
% U is a matrix canonical form, V a compatible matrix form.
% Result is the solution,y, to the matrix equation U*y=V.
% If !*SOLVESINGULAR is non-nil, introduces arbitrary constants
% if necessary.  Returns an error if the system represented is
% inconsistent or if !*SOLVESINGULAR is nil and U is singular.;
   BEGIN INTEGER N, K; SCALAR X,!*S!*, PERM;
        X := !*EXP; !*EXP := T; N := LENGTH U; PERM := INDEXLIS(1, N);
        U := CAR NORMMAT AUGMENT(U,V);
        IF NOT !*SOLVESINGULAR THEN
           U := BAREISS U
        ELSE <<
           U := SOLVEBAREISS(U, PERM);
           IF U THEN
              U := INSERTARBCONSTS(CDR(U),
                                   CAR(U)+1,
                                   FUNCTION MAKEARBCOMPLEX) >>;
        !*S!* := BACKSUB(U,N);
        U := MAPC2(RHSIDE(CAR !*S!*,N),
                   FUNCTION (LAMBDA J; CANCEL(J . CDR !*S!*)));
        !*EXP := X;
        RETURN PERMUTE(U, PERM);
   END;

SYMBOLIC PROCEDURE SOLVEBAREISS(U, PERM);
  %The 2-step integer preserving elimination method of Bareiss
  %based on the implementation of Lipson;
  %This is based on the Bareiss function in the Reduce matrix package,
  %modified to reduce singular matrices.  If PERM is nil, behaves
  %as BAREISS, except a pair is returned for non-singular U, whose
  %cdr is the triangularized U.  The car is the rank of U, which in
  %this case is always LENGTH(U).
  %Otherwise PERM is a list of the integers 1,2...length(U).
  %As columns are interchanged, then so are the elements of PERM.
  %In this case a pair is returned whose car is the rank of U and
  %whose cdr is the triangularized U. Note that, just as in BAREISS, the
  %lower triangular portion of the returned matrix standard form is only
  %implicitly all nils--the requisite RPLACAs are not performed.  Also,
  %if PERM is non-nil and the rank,r,  of U is less than the order of U,
  %only the first r rows of the upper triangular portion are explicitly
  %set.  The all nil rows are only implicitly all nils.
  %U is a list of lists of standard forms (a matrix standard form)
  %corresponding to the appropriate augmented matrix.
  %If the value of procedure is NIL then U is singular, otherwise the
  %value is the triangularized form of U (in the same form);
  BEGIN SCALAR AA,C0,CI1,CI2,IK1,IJ,KK1,KJ,K1J,K1K1,
	       UI,U1,X,K1COL,KIJ,FLG;
        INTEGER K,K1,COL,MAXCOL;
        %U1 points to K-1th row of U
        %UI points to Ith row of U
        %IJ points to U(I,J)
        %K1J points to U(K-1,J)
        %KJ points to U(K,J)
        %IK1 points to U(I,K-1)
        %KK1 points to U(K,K-1)
        %K1K1 points to U(K-1,K-1)
        %M in comments is number of rows in U
        %N in comments is number of columns in U;
        MAXCOL := LENGTH(U);
        AA:= 1;
        K:= 2;
        K1:=1;
        U1:=U;
        GO TO PIVOT;
   AGN: U1 := CDR U1;
        IF NULL CDR U1 OR NULL CDDR U1 THEN
           IF PERM AND CDR(U1) AND
              NULL(CAR(IJ := PNTH(NTH(U, MAXCOL), MAXCOL))) THEN <<
                 MAPC(CDR(IJ), FUNCTION LAMBDA(X);
                               IF X THEN RETURN NIL);
                 RETURN (MAXCOL-1).U >>
           ELSE
              RETURN MAXCOL.U;
        AA:=NTH(CAR U1,K);              %AA := U(K,K);
        K:=K+2;
        K1:=K-1;
        U1:=CDR U1;
   PIVOT:  %pivot algorithm;
        COL := K1;
        K1J:= K1K1 := PNTH(CAR U1,K1);
  PIV1: K1COL := PNTH(CAR(U1), COL);
        IF CAR K1COL THEN GO TO L2;
        UI:= CDR U1;                    %I := K;
   L:   IF NULL UI THEN
           IF PERM THEN
              IF COL>=MAXCOL THEN
                 RETURN (K1-1).U
              ELSE <<
                 COL := COL+1;
                 GO TO PIV1 >>
           ELSE
              RETURN NIL
        ELSE IF NULL CAR(IJ := PNTH(CAR UI,COL))
          THEN GO TO L1;
   L0:  IF NULL IJ THEN GO TO L2;
        X := CAR IJ;
        RPLACA(IJ,NEGF CAR K1COL);
        RPLACA(K1COL,X);
        IJ:= CDR IJ;
        K1COL:= CDR K1COL;
        GO TO L0;
   L1:  UI:= CDR UI;
        GO TO L;
   L2:  SWAPCOLUMNS(U, K1, COL, PERM);
        COL := K;
  PIV2: UI:= CDR U1;                    %I:= K;
   L21: IF NULL UI THEN
           IF PERM THEN
              IF COL>=MAXCOL THEN <<
                 FLG := T;
                 WHILE FLG AND U1 DO <<
                    IK1 := PNTH(CAR(U1), K1);
                    IJ := PNTH(IK1, MAXCOL-K1+2);
                    KIJ := PNTH(K1K1, MAXCOL-K1+2);
                    WHILE FLG AND IJ DO
                       IF ADDF(MULTF(CAR(K1K1), CAR(IJ)),
                               MULTF(CAR(IK1), NEGF(CAR(KIJ))) )
                       THEN FLG := NIL
                       ELSE IJ := CDR(IJ);
                    U1 := CDR(U1) >>;
                 IF FLG THEN
                    RETURN (K-1).U
                 ELSE
                    RETURN NIL >>
              ELSE <<
                 COL := COL+1;
                 GO TO PIV2 >>
           ELSE
              RETURN NIL;
        IJ:= PNTH(CAR UI,K1);
        C0:= ADDF(MULTF(CAR K1K1,NTH(IJ, COL-K+2)),
                  MULTF(NTH(K1K1, COL-K+2),NEGF CAR IJ));
        IF C0 THEN GO TO L3;
        UI:= CDR UI;                    %I:= I+1;
        GO TO L21;
   L3:  SWAPCOLUMNS(U, K, COL, PERM);
        C0:= QUOTF!*(C0,AA);
        KK1 := KJ := PNTH(CADR U1,K1);  %KK1 := U(K,K-1);
        IF CDR U1 AND NULL CDDR U1 THEN GO TO EV0
         ELSE IF UI EQ CDR U1 THEN GO TO COMP;
   L31: IF NULL IJ THEN GO TO COMP;     %IF I>N THEN GO TO COMP;
        X:= CAR IJ;
        RPLACA(IJ,NEGF CAR KJ);
        RPLACA(KJ,X);
        IJ:= CDR IJ;
        KJ:= CDR KJ;
        GO TO L31;
        %pivoting complete;
    COMP:
        IF NULL CDR U1 THEN GO TO EV;
        UI:= CDDR U1;                   %I:= K+1;
    COMP1:
        IF NULL UI THEN GO TO EV;       %IF I>M THEN GO TO EV;
        IK1:= PNTH(CAR UI,K1);
        CI1:= QUOTF!*(ADDF(MULTF(CADR K1K1,CAR IK1),
                           MULTF(CAR K1K1,NEGF CADR IK1)),
                     AA);
        CI2:= QUOTF!*(ADDF(MULTF(CAR KK1,CADR IK1),
                           MULTF(CADR KK1,NEGF CAR IK1)),
                     AA);
        IF NULL CDDR K1K1 THEN GO TO COMP3;%IF J>N THEN GO TO COMP3;
        IJ:= CDDR IK1;                  %J:= K+1;
        KJ:= CDDR KK1;
        K1J:= CDDR K1K1;
    COMP2:
        IF NULL IJ THEN GO TO COMP3;
        RPLACA(IJ,QUOTF!*(ADDF(MULTF(CAR IJ,C0),
                               ADDF(MULTF(CAR KJ,CI1),
                                  MULTF(CAR K1J,CI2))),
                     AA));
        IJ:= CDR IJ;
        KJ:= CDR KJ;
        K1J:= CDR K1J;
        GO TO COMP2;
    COMP3:
        UI:= CDR UI;
        GO TO COMP1;
    EV0:IF NULL C0 THEN RETURN;
    EV: KJ := CDR KK1;
        X := CDDR K1K1;                 %X := U(K-1,K+1);
        RPLACA(KJ,C0);
    EV1:KJ:= CDR KJ;
        IF NULL KJ THEN GO TO AGN;
        RPLACA(KJ,QUOTF!*(ADDF(MULTF(CAR K1K1,CAR KJ),
                               MULTF(CAR KK1,NEGF CAR X)),
                     AA));
        X := CDR X;
        GO TO EV1
   END;

SYMBOLIC PROCEDURE SWAPCOLUMNS(MATRX, COL1, COL2, PERM);
IF COL1=COL2 THEN
   MATRX
ELSE <<
   SWAPELEMENTS(PERM, COL1, COL2);
   FOR EACH U IN MATRX DO
      SWAPELEMENTS(U, COL1, COL2);
   MATRX >>;

SYMBOLIC PROCEDURE SWAPELEMENTS(LST, I, J);
% Swaps the  Ith and Jth elements of the list LST al la
%  RPLACA and returns nil.;
BEGIN SCALAR TEMP;
   IF I>J THEN <<
      TEMP := I;
      I := J;
      J := TEMP >>;
   LST := PNTH(LST, I);
   I := J-I+1;
   TEMP := NTH(LST, I);
   RPLACA(PNTH(LST, I), CAR(LST));
   RPLACA(LST, TEMP)
END;

SYMBOLIC PROCEDURE INDEXLIS(M, N);
% M,N are integers.  Returns the list (M M+1 M+2 ... N-1 N);
IF M<=N THEN M . INDEXLIS(M+1,N);

SYMBOLIC PROCEDURE INSERTARBCONSTS(M, ZEROROW, ARBFN);
% M is a matrix standard form, representing a
% matrix which has been row reduced.  All elements below
% the principal diagonal are implicitly nil, as are all
% elements in row ZEROROW and below.  It is such a form
% as is returned by SOLVEBAREISS with a non-nil second
% argument.  It inserts approriate arbitrary constants in
% the inhomogeneous portion, and 1's on the main diagonal
% except for the last row, which gets the new determinant
% of the square submatrix.  Calls ARBFN to generate arbitrary
% constants.;
BEGIN SCALAR U, V, NEWDET; INTEGER N;
   N := LENGTH(M);
   IF ZEROROW<=N THEN <<
      NEWDET := 1;
      U := M;
      FOR I := 1:(ZEROROW-1) DO <<
         NEWDET := MULTF(NEWDET, NTH(CAR(U), I));
         U := CDR(U) >>;
      FOR I := ZEROROW:(N-1) DO <<
         V := PNTH(CAR(U), I);
         RPLACA(V, 1);
         V := CDR(V);
         FOR J := I+1:N DO <<
            RPLACA(V, NIL);
            V := CDR(V) >>;
         WHILE V DO <<
	    RPLACA(V, !*K2F EVAL LIST ARBFN);
            V := CDR(V) >>;
         U := CDR(U) >>;
      V := PNTH(CAR(U), N);
      RPLACA(V, NEWDET);
      V := CDR(V);
      WHILE V DO <<
	 RPLACA(V, MULTF(NEWDET, !*K2F EVAL LIST ARBFN));
         V := CDR(V) >> >>;
   RETURN M
END;

SYMBOLIC PROCEDURE PERMUTE(U, V);
% U is a list.  V is a list of the numbers 1,2,...LENGTH(U), permuted;
% Returns a constructed list of the elements of U permuted by V.;
IF V THEN NCONC(LIST(NTH(U,CAR(V))), PERMUTE(U, CDR(V)));
   
SYMBOLIC PROCEDURE MAKEARBCOMPLEX();
BEGIN SCALAR ANS;
   ANS := NUMR(SIMP!*(LIST('ARBCOMPLEX, !!ARBINT)));
   !!ARBINT := !!ARBINT+1;
   RETURN ANS
END;

%******** Algebraic Let Statements and related declarations ********;

PUT('ASIN, 'INVERSE, 'SIN);
PUT('ACOS, 'INVERSE, 'COS);

ALGEBRAIC <<

OPERATOR SOL, INTERVL, ARBCOMPLEX, ARBREAL, ARBINT, LST;

COMMENT Supply missing argument and simplify 1/4 roots of unity;
LET   E**(I*PI/2) = I,
      E**(I*PI) = -1,
      E**(3*I*PI/2)=-I;

FOR ALL N SUCH THAT FIXP N
   LET COS((N*PI)/2)= 0;

LET COS(PI/2)=0;

FOR ALL N SUCH THAT FIXP N
   LET SIN((N*PI)/2)=
	IF REMAINDER(ABS N,4)<2 THEN 1 ELSE -1;

LET SIN(PI/2)=1;

FOR ALL N SUCH THAT FIXP N
   LET COS((N*PI)/3)=
	(IF N=4 OR REMAINDER(ABS N+2,6)>3 THEN -1 ELSE 1)/2;

LET COS(PI/3)=1/2;

FOR ALL N SUCH THAT FIXP N
   LET SIN((N*PI)/3)=
	(IF REMAINDER(ABS N,6)<3 THEN 1 ELSE -1)*SQRT(3)/2;

LET SIN(PI/3)=SQRT(3)/2;

FOR ALL N SUCH THAT FIXP N
   LET COS((N*PI)/4)=
       (IF REMAINDER(ABS N+2,8)<4 THEN 1 ELSE -1)*SQRT(2)/2;

LET COS(PI/4)=SQRT 2/2;

FOR ALL N SUCH THAT FIXP N
   LET SIN((N*PI)/4)=
	(IF REMAINDER(ABS N,8)<4 THEN 1 ELSE -1)*SQRT(2)/2;

LET SIN(PI/4)=SQRT(2)/2;

FOR ALL N SUCH THAT FIXP N
   LET COS((N*PI)/6)=

      (IF REMAINDER(ABS N+2,12)<6 THEN 1 ELSE -1)*SQRT(3)/2;

LET COS(PI/6)=SQRT 3/2;

FOR ALL N SUCH THAT FIXP N
   LET SIN((N*PI)/6)=
	(IF REMAINDER(ABS N,12)<6 THEN 1 ELSE -1)/2;

LET SIN(PI/6)=1/2;

COMMENT Rules for reducing the number of distinct kernels in an
   equation;

FOR ALL A,B,X SUCH THAT RATNUMP C AND RATNUMP D LET
   SOL(A**C-B**D, X) = A**(C*LCMD(C,D)) - B**(D*LCMD(C,D));

FOR ALL A,B,C,D,X SUCH THAT FREEOFKERN(X,A) AND FREEOFKERN(X,C) LET
   SOL(A**B-C**D, X) = E**(B*LOG A - D*LOG C);

FOR ALL A,B,C,D,X SUCH THAT FREEOFKERN(X,A) AND FREEOFKERN(X,C) LET
   SOL(A*LOG B + C*LOG D, X) = B**A*D**C - 1,
   SOL(A*LOG B - C*LOG D, X) = B**A - D**C;

FOR ALL A,B,C,D,F,X SUCH THAT FREEOFKERN(X,A) AND FREEOFKERN(X,C) LET
   SOL(A*LOG B + C*LOG D + F, X) = SOL(LOG(B**A*D**C) + F, X),
   SOL(A*LOG B + C*LOG D - F, X) = SOL(LOG(B**A*D**C) - F, X),
   SOL(A*LOG B - C*LOG D + F, X) = SOL(LOG(B**A/D**C) + F, X),
   SOL(A*LOG B - C*LOG D - F, X) = SOL(LOG(B**A/D**C) - F, X);

FOR ALL A,B,D,F,X SUCH THAT FREEOFKERN(X,A) LET
   SOL(A*LOG B + LOG D + F, X) = SOL(LOG(B**A*D) + F, X),
   SOL(A*LOG B + LOG D - F, X) = SOL(LOG(B**A*D) - F, X),
   SOL(A*LOG B - LOG D + F, X) = SOL(LOG(B**A/D) + F, X),
   SOL(A*LOG B - LOG D - F, X) = SOL(LOG(B**A/D) - F, X),
   SOL(LOG D - A*LOG B + F, X) = SOL(LOG(D/B**A) + F, X),
   SOL(LOG D - A*LOG B - F, X) = SOL(LOG(D/B**A) - F, X);

FOR ALL A,B,D,X SUCH THAT FREEOFKERN(X,A) LET
   SOL(A*LOG B + LOG D, X) = B**A*D - 1,
   SOL(A*LOG B - LOG D, X) = B**A - D,
   SOL(LOG D - A*LOG B, X) = D - B**A;

FOR ALL A,B,C,X LET
   SOL(LOG A + LOG B + C, X) = SOL(LOG(A*B) + C, X),
   SOL(LOG A - LOG B + C, X) = SOL(LOG(A/B) + C, X),
   SOL(LOG A + LOG B - C, X) = SOL(LOG(A*B) - C, X),
   SOL(LOG A - LOG B - C, X) = SOL(LOG(A/B) - C, X);

FOR ALL A,C,X SUCH THAT FREEOFKERN(X,C) LET
   SOL(LOG A + C, X) = A - E**C,
   SOL(LOG A - C, X) = A - E**(-C);

FOR ALL A,B,X LET
   SOL(LOG A + LOG B, X) = A*B - 1,
   SOL(LOG A - LOG B, X) = A - B,
   SOL(COS A - SIN B, X) = SOL(COS A - COS(PI/2-B), X),
   SOL(SIN A + COS B, X) = SOL(SIN A - SIN(B-PI/2), X),
   SOL(SIN A - COS B, X) = SOL(SIN A - SIN(PI/2-B), X),
   SOL(SIN A + SIN B, X) = SOL(SIN A - SIN(-B), X),
   SOL(SIN A - SIN B, X) = IF !*ALLBRANCH THEN SIN((A-B)/2)*
       COS((A+B)/2)  ELSE A-B,
   SOL(COS A + COS B, X) = IF !*ALLBRANCH THEN COS((A+B)/2)*
       COS((A-B)/2)  ELSE A+B,
   SOL(COS A - COS B, X) = IF !*ALLBRANCH THEN SIN((A+B)/2)*
       SIN((A-B)/2)  ELSE A-B,
   SOL(ASIN A - ASIN B, X) = A-B,
   SOL(ASIN A + ASIN B, X) = A+B,
   SOL(ACOS A - ACOS B, X) = A-B,
   SOL(ACOS A + ACOS B, X) = A+B;

LET COS(PI/2)=0>>;


END;
