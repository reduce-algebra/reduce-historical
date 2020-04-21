%*********************************************************************
%*********************************************************************
%            REDUCE BASIC ALGEBRAIC PROCESSOR (PART 2)
%*********************************************************************
%********************************************************************;

%Copyright (c) 1983 The Rand Corporation;

SYMBOLIC;

COMMENT The following free variables are referenced in this module;

FLUID '(!*MCD);

GLOBAL '(ASYMPLIS!* FRLIS!* KORD!* MCHFG!* MCOND!* POWLIS!* POWLIS1!*
	 SPLIS!* SUBFG!* TYPL!* VARNAM!* WTL!* !*FLOAT !*FORT !*MATCH
	 !*NAT !*PRI !*RESUBS !*SUB2);


%*********************************************************************
%*********************************************************************
%      FUNCTIONS WHICH APPLY MORE GENERAL PATTERN MATCHING RULES
%*********************************************************************
%********************************************************************;

%*********************************************************************
%		     FUNCTIONS FOR MATCHING POWERS
%********************************************************************;

COMMENT Fluid variable used in this section;

FLUID '(!*STRUCTURE);

!*STRUCTURE := NIL;

COMMENT If STRUCTURE is ON, then expressions like (a**(b/2))**2 are not
simplified, to allow some attempt at a structure theorem use, especially
in the integrator;

SYMBOLIC PROCEDURE SUBS2Q U; QUOTSQ(SUBS2F NUMR U,SUBS2F DENR U);

SYMBOLIC PROCEDURE SUBS2F U;
   BEGIN SCALAR X;
	!*SUB2 := NIL;
	X := SUBS2F1 U;
	IF (!*SUB2 OR POWLIS1!*) AND !*RESUBS
	   THEN IF NUMR X=U AND DENR X=1 THEN !*SUB2 := NIL
		ELSE X := SUBS2Q X; RETURN X;
   END;

SYMBOLIC PROCEDURE SUBS2F1 U;
   IF DOMAINP U THEN !*D2Q U
    ELSE BEGIN SCALAR KERN,V,W,X,Y,Z;
	KERN := MVAR U;
	Z := NIL ./ 1;
    A:	IF NULL U OR DEGR(U,KERN)=0 THEN GO TO A1;
	Y := LT U .+ Y;
	U := RED U;
	GO TO A;
    A1: X := POWLIS!*;
    A2: IF NULL X THEN GO TO B
	 ELSE IF CAAAR Y = CAAR X
	  THEN <<W := SUBS2P(CAAR Y,CADAR X,CADDDR CAR X); GO TO E1>>
%	 ELSE IF EQCAR(KERN,'SQRT) AND CADR KERN = CAAR X
%	  THEN <<W := RADDSQ(SUBS2P(CADR KERN . CDAAR Y,
%			     CADAR X,CADDDR CAR X),2);% GO TO E1>>;
	 ELSE IF EQCAR(KERN,'EXPT)
		AND CADR KERN = CAAR X
		AND EQCAR(CADDR KERN,'QUOTIENT)
		AND CADR CADDR KERN = 1
		AND NUMBERP CADDR CADDR KERN
	  THEN <<V := DIVIDE(CDAAR Y,CADDR CADDR KERN);
		 IF CAR V NEQ 0 THEN W := MKSQ(CADR KERN,CAR V)
		  ELSE W := 1 ./ 1;
		 IF CDR V NEQ 0
		   THEN <<V := CANCEL(CDR V.CADDR CADDR KERN);
			 W := MULTSQ(RADDSQ(SUBS2P(CADR KERN . CAR V,
				     	CADAR X,CADDDR CAR X),
			      	CDR V),W)>>;
		 GO TO E1>>;
	X := CDR X;
	GO TO A2;
    B:	X := POWLIS1!*;
    L2: IF NULL X THEN GO TO L3
	 ELSE IF W:= MTCHP(CAAR Y,CAAR X,CADDAR X,CAADAR X,CDADAR X)
	  THEN GO TO E1;
	X := CDR X;
	GO TO L2;
    L3: IF EQCAR(KERN,'EXPT) AND NOT !*STRUCTURE THEN GO TO L1;
	Z := ADDSQ(MULTPQ(CAAR Y,SUBS2F1 CDAR Y),Z);
    C:	Y := CDR Y;
	IF Y THEN GO TO A1;
    D:	RETURN ADDSQ(Z,SUBS2F1 U);
    E1: Z := ADDSQ(MULTSQ(W,SUBS2F1 CDAR Y),Z);
	GO TO C;
    L1: IF ONEP CDAAR Y THEN W := MKSQ(KERN,1)
	 ELSE W := SIMPEXPT LIST(CADR KERN,
				 LIST('TIMES,CADDR KERN,CDAAR Y));
	Z := ADDSQ(MULTSQ(W,SUBS2F1 CDAR Y),Z);
	Y := CDR Y;
	IF Y THEN GO TO L1 ELSE GO TO D;
    END;

SYMBOLIC PROCEDURE SUBS2P(U,V,W);
   %U is a power, V an integer, and W an algebraic expression, such
   %that CAR U**V=W. Value is standard quotient for U with this
   %substitution;
   BEGIN 
      V := DIVIDE(CDR U,V);
      IF CAR V=0 THEN RETURN !*P2Q U;
      W := EXPTSQ(SIMP W,CAR V);
      RETURN IF CDR V=0 THEN W ELSE MULTPQ(CAR U TO CDR V,W)
   END;

SYMBOLIC PROCEDURE RADDSQ(U,N);
   %U is a standard quotient, N and integer. Value is sq for U**(1/N);
   SIMPEXPT LIST(MK!*SQ U,LIST('QUOTIENT,1,N));

SYMBOLIC PROCEDURE MTCHP(U,V,W,FLG,BOOL);
   %U is a standard power, V a power to be matched against.
   %W is the replacement expression.
   %FLG is a flag which is T if an exact power match required.
   %BOOL is a boolean expression to be satisfied for substitution.
   %Value is the substitution standard quotient if a match found,
   %NIL otherwise;
   BEGIN SCALAR X;
	X := MTCHP1(U,V,FLG,BOOL);
    A:	IF NULL X THEN RETURN NIL
	 ELSE IF EVAL SUBLA(CAR X,BOOL) THEN GO TO B;
	X := CDR X;
	GO TO A;
    B:	V := DIVIDE(CDR U,SUBLA(CAR X,CDR V));
	W := EXPTSQ(SIMP SUBLA(CAR X,W),CAR V);
	IF CDR V NEQ 0 THEN W := MULTPQ(CAR U TO CDR V,W);
	RETURN W
   END;

SYMBOLIC PROCEDURE MTCHP1(U,V,FLG,BOOL);
   %U is a standard power, V a power to be matched against.
   %FLG is a flag which is T if an exact power match required.
   %BOOL is a boolean expression to be satisfied for substitution.
   %Value is a list of possible free variable pairings which
   %match conditions;
   BEGIN SCALAR X;
	IF U=V THEN RETURN LIST NIL
	 ELSE IF NOT (X:= MCHK(CAR U,CAR V)) THEN RETURN NIL
	 ELSE IF CDR V MEMQ FRLIS!*
	  THEN RETURN MAPCONS(X,CDR V . CDR U)
	 ELSE IF (FLG AND NOT CDR U=CDR V)
		OR (IF !*MCD THEN CDR U<CDR V
		     ELSE (CDR U*CDR V)<0 OR
			%implements explicit sign matching;
			    ABS CDR U<ABS CDR V)
	  THEN RETURN NIL
	 ELSE RETURN X
   END;


%*********************************************************************
%		    FUNCTIONS FOR MATCHING PRODUCTS
%********************************************************************;

SYMBOLIC PROCEDURE SUBS3Q U;
   %U is a standard quotient.
   %Value is a standard quotient with all product substitutions made;
   BEGIN SCALAR X;
	X := MCHFG!*;	%save value in case we are in inner loop;
	MCHFG!* := NIL;
	U := QUOTSQ(SUBS3F NUMR U,SUBS3F DENR U);
	MCHFG!* := X;
	RETURN U
   END;

SYMBOLIC PROCEDURE SUBS3F U;
   %U is a standard form.
   %Value is a standard quotient with all product substitutions made;
   SUBS3F1(U,!*MATCH,T);

SYMBOLIC PROCEDURE SUBS3F1(U,L,BOOL);
   %U is a standard form.
   %L is a list of possible matches.
   %BOOL is a boolean variable which is true if we are at top level.
   %Value is a standard quotient with all product substitutions made;
   BEGIN SCALAR X,Z;
	Z := NIL ./ 1;
    A:	IF NULL U THEN RETURN Z
	 ELSE IF DOMAINP U THEN RETURN ADDSQ(Z,U ./ 1)
	 ELSE IF BOOL AND DOMAINP LC U THEN GO TO C;
	X := SUBS3T(LT U,L);
	IF NOT BOOL				%not top level;
	 OR NOT MCHFG!* THEN GO TO B;		%no replacement made;
	MCHFG!* := NIL;
	IF NULL !*RESUBS THEN GO TO B
	 ELSE IF !*SUB2 OR POWLIS1!* THEN X := SUBS2Q X;
	   %make another pass;
	X := SUBS3Q X;
    B:	Z := ADDSQ(Z,X);
	U := CDR U;
	GO TO A;
    C:	X := LIST LT U ./ 1;
	GO TO B
   END;

SYMBOLIC PROCEDURE SUBS3T(U,V);
   %U is a standard term, V a list of matching templates.
   %Value is a standard quotient for the substituted term;
   BEGIN SCALAR X,Y,Z;
	X := MTCHK(CAR U,IF DOMAINP CDR U THEN SIZCHK(V,1) ELSE V);
	IF NULL X THEN GO TO A			%lpow doesn't match;
	 ELSE IF NULL CAAR X THEN GO TO B;	%complete match found;
	Y := SUBS3F1(CDR U,X,NIL);		%check tc for match;
	IF MCHFG!* THEN RETURN MULTPQ(CAR U,Y);
    A:	RETURN LIST U . 1;			%no match;
    B:	X := CDDAR X;		%list(<subst value>,<denoms>);
	Z := CAADR X;		%leading denom;
	MCHFG!* := NIL; 	%initialize for tc check;
	Y := SUBS3F1(CDR U,!*MATCH,NIL);
	MCHFG!* := T;
	IF CAR Z NEQ CAAR U THEN GO TO E
	 ELSE IF Z NEQ CAR U	%powers don't match;
	  THEN Y := MULTPQ(CAAR U TO (CDAR U-CDR Z),Y);
    B1: Y := MULTSQ(SIMPCAR X,Y);
	X := CDADR X;
	IF NULL X THEN RETURN Y;
	Z := 1; 		%unwind remaining denoms;
    C:	IF NULL X THEN GO TO D;
	Z:=LIST(MKSP(CAAR X,
      %was IF ATOM CAAR X OR SFP CAAR X THEN CAAR X ELSE REVOP1 CAAR X;
			IF !*MCD THEN CDAR X ELSE -CDAR X) . Z);
	%kernel CAAR X is not unique here;
	X := CDR X;
	GO TO C;
    D:	RETURN IF !*MCD THEN CAR Y . MULTF(Z,CDR Y)
		ELSE MULTF(Z,CAR Y) . CDR Y;
    E:	IF SIMP CAR Z NEQ SIMP CAAR U THEN ERRACH LIST('SUBS3T,U,X,Z);
	%maybe arguments were in different order, otherwise it's fatal;
	IF CDR Z NEQ CDAR U
	  THEN Y:= MULTPQ(CAAR U TO (CDAR U-CDR Z),Y);
	GO TO B1
   END;

SYMBOLIC PROCEDURE SIZCHK(U,N);
   IF NULL U THEN NIL
    ELSE IF LENGTH CAAR U>N THEN SIZCHK(CDR U,N)
    ELSE CAR U . SIZCHK(CDR U,N);

SYMBOLIC PROCEDURE MTCHK(U,V);
   %U is a standard power, V a list of matching templates.
   %If a match is made, value is of the form:
   %list list(NIL,<boolean form>,<subst value>,<denoms>),
   %otherwise value is an updated list of templates;
   BEGIN SCALAR FLG,V1,W,X,Y,Z;
	FLG := NONCOMP CAR U;
    A0: IF NULL V THEN RETURN Z;
	V1 := CAR V;
	W := CAR V1;
    A:	IF NULL W THEN GO TO D;
	X := MTCHP1(U,CAR W,CAADR V1,CDADR V1);
    B:	IF NULL X THEN GO TO C
	 ELSE IF CAR (Y := SUBLA(CAR X,DELETE(CAR W,CAR V1))
				. LIST(SUBLA(CAR X,CADR V1),
				      SUBLA(CAR X,CADDR V1),
				      SUBLA(CAR X,CAR W)
					  . CADDDR V1))
	  THEN Z := Y . Z
	 ELSE IF EVAL SUBLA(CAR X,CDADR V1) THEN RETURN LIST Y;
	X := CDR X;
	GO TO B;
    C:	IF FLG THEN GO TO C1;
	W := CDR W;
	GO TO A;
    C1: IF CADDDR V1 AND NOT NOCP CADDDR V1 THEN GO TO E;
    D:	Z := APPEND(Z,LIST V1);
    E:	V := CDR V;
	GO TO A0
   END;

SYMBOLIC PROCEDURE NOCP U;
   NULL U OR (NONCOMP CAAR U AND NOCP CDR U);


%*********************************************************************
%		      FUNCTIONS FOR MATCHING SUMS
%********************************************************************;

SYMBOLIC PROCEDURE SUBS4Q U;
   QUOTSQ(SUBS4F NUMR U,SUBS4F DENR U);

SYMBOLIC PROCEDURE SUBS4F U;
   BEGIN SCALAR W,X,Y,Z;
      X := SPLIS!*;
    A:	IF NULL X THEN RETURN U ./ 1;
	W := LQREMF!*(U,CAAR X);
	IF NULL CDR W THEN <<X := CDR X; GO TO A>>;
	X := SIMP CADDAR X;
	Y := 1 ./ 1;
	Z := NIL ./ 1;
	WHILE W DO
	 <<IF CAR W THEN Z := ADDSQ(MULTSQ(CAR W ./ 1,Y),Z);
	   Y := MULTSQ(X,Y);
	   W := CDR W>>;
	RETURN IF DENR Z=1 AND NUMR Z=U THEN U ./ 1 ELSE SUBS4Q Z;
	%one could test on size here and only change if smaller;
   END;

SYMBOLIC PROCEDURE LQREMF!*(U,V);
   IF DOMAINP U THEN LIST U ELSE LQREMF(U,REORDER V);


%*********************************************************************
%*********************************************************************
%		EXTENDED OUTPUT PACKAGE FOR EXPRESSIONS
%*********************************************************************
%********************************************************************;

%Global variables used in this Section;

GLOBAL '(DNL!* FACTORS!* ORDL!* UPL!* !*ALLFAC !*DIV !*RAT);

DNL!* := NIL;		%output control flag: puts powers in denom;
FACTORS!* := NIL;	%list of output factors;
ORDL!* := NIL;		%list of kernels introduced by ORDER statement;
UPL!* := NIL;		%output control flag: puts denom powers in
			%numerator;
!*ALLFAC := T;		%factoring option for this package;
!*DIV := NIL;		%division option in this package;
!*RAT := NIL;		%flag indicating rational mode for output;

!*PRI := T;		%to activate this package;

SYMBOLIC PROCEDURE FACTOR U;
   FACTOR1(U,T,'FACTORS!*);

SYMBOLIC PROCEDURE FACTOR1(U,V,W);
   BEGIN SCALAR X,Y;
	Y := EVAL W;
	FOR EACH J IN U DO
	 <<X := !*A2K J;
	   IF V THEN Y := ACONC(DELETE(X,Y),X)
	    ELSE IF NOT X MEMBER Y
	     THEN MSGPRI(NIL,J,"not found",NIL,NIL)
	    ELSE Y := DELETE(X,Y)>>;
	SET(W,Y)
   END;

SYMBOLIC PROCEDURE REMFAC U;
   FACTOR1(U,NIL,'FACTORS!*);

RLISTAT '(FACTOR REMFAC);

SYMBOLIC PROCEDURE ORDER U;
   IF U AND NULL CAR U AND NULL CDR U THEN (ORDL!* := NIL)
    ELSE FOR EACH X IN U DO
      <<IF (X := !*A2K X) MEMBER ORDL!* THEN ORDL!* := DELETE(X,ORDL!*);
	ORDL!* := ACONC(ORDL!*,X)>>;

RLISTAT '(ORDER);

SYMBOLIC PROCEDURE UP U;
   FACTOR1(U,T,'UPL!*);

SYMBOLIC PROCEDURE DOWN U;
   FACTOR1(U,T,'DNL!*);

RLISTAT '(UP DOWN);

SYMBOLIC PROCEDURE FORMOP U;
   IF DOMAINP U THEN U
    ELSE RADDF(MULTOP(LPOW U,FORMOP LC U),FORMOP RED U);

SYMBOLIC PROCEDURE MULTOP(U,V);
   IF NULL KORD!* THEN MULTPF(U,V)
    ELSE IF CAR U EQ 'K!* THEN V
    ELSE RMULTPF(U,V);

SYMBOLIC SMACRO PROCEDURE LCX U;
   %returns leading coefficient of a form with zero reductum, or an
   %error otherwise;
   CDR CARX U;

SYMBOLIC PROCEDURE QUOTOF(P,Q);
   %P is a standard form, Q a standard form which is either a domain
   %element or has zero reductum.
   %returns the quotient of P and Q for output purposes;
   IF NULL P THEN NIL
    ELSE IF P=Q THEN 1
    ELSE IF Q=1 THEN P
    ELSE IF DOMAINP Q THEN QUOTOFD(P,Q)
    ELSE IF DOMAINP P
     THEN MKSP(MVAR Q,-LDEG Q) .* QUOTOF(P,LCX Q) .+ NIL
    ELSE (LAMBDA (X,Y);
	  IF CAR X EQ CAR Y
	      THEN (LAMBDA (N,W,Z);
		 IF N=0 THEN RADDF(W,Z)
		  ELSE ((CAR Y TO N) .* W) .+ Z)
	      (CDR X-CDR Y,QUOTOF(LC P,LCX Q),QUOTOF(RED P,Q))
	   ELSE IF ORDOP(CAR X,CAR Y)
	      THEN (X .* QUOTOF(LC P,Q)) .+ QUOTOF(RED P,Q)
	   ELSE MKSP(CAR Y,- CDR Y) .* QUOTOF(P,LCX Q) .+ NIL)
       (LPOW P,LPOW Q);

SYMBOLIC PROCEDURE QUOTOFD(P,Q);
   %P is a form, Q a domain element. Value is quotient of P and Q
   %for output purposes;
   IF NULL P THEN NIL
    ELSE IF DOMAINP P THEN QUOTODD(P,Q)
    ELSE (LPOW P .* QUOTOFD(LC P,Q)) .+ QUOTOFD(RED P,Q);

SYMBOLIC PROCEDURE QUOTODD(P,Q);
   %P and Q are domain elements. Value is domain element for P/Q;
   IF ATOM P AND ATOM Q THEN MKRN(P,Q) ELSE LOWEST!-TERMS(P,Q);

SYMBOLIC PROCEDURE LOWEST!-TERMS(U,V);
   %reduces compatible domain elements U and V to a ratio in lowest
   %terms.  Value as a rational may contain domain arguments rather than
   %just integers;
   IF FLAGP(CAR V,'FIELD) OR FLAGP(CAR U,'FIELD)
     THEN MULTDM(U,!:EXPT(V,-1))
     ELSE BEGIN SCALAR X;
      X := DCOMBINE(U,V,'GCD);
      U := DCOMBINE(U,X,'QUOTIENT);
      V := DCOMBINE(V,X,'QUOTIENT);
      RETURN IF !:ONEP V THEN U ELSE '!:RN!: . (U . V)
   END;

SYMBOLIC PROCEDURE CKRN U;
   BEGIN SCALAR X;
	IF DOMAINP U THEN RETURN U;
    A:	X := GCK2(CKRN CDAR U,X);
	IF NULL CDR U
	  THEN RETURN IF NONCOMP MVAR U THEN X ELSE LIST(CAAR U . X)
	 ELSE IF DOMAINP CDR U OR NOT CAAAR U EQ CAAADR U
	  THEN RETURN GCK2(CKRN CDR U,X);
	U := CDR U;
	GO TO A
   END;

SYMBOLIC PROCEDURE GCK2(U,V);
   %U and V are domain elements or forms with a zero reductum.
   %Value is the gcd of U and V;
   IF NULL V THEN U
    ELSE IF U=V THEN U
    ELSE IF DOMAINP U THEN IF DOMAINP V THEN GCDDD(U,V)
	ELSE GCK2(U,CDARX V)
    ELSE IF DOMAINP V THEN GCK2(CDARX U,V)
    ELSE (LAMBDA (X,Y);
	IF CAR X EQ CAR Y
	  THEN LIST((IF CDR X>CDR Y THEN Y ELSE X) .
		    GCK2(CDARX U,CDARX V))
	 ELSE IF ORDOP(CAR X,CAR Y) THEN GCK2(CDARX U,V)
	 ELSE GCK2(U,CDARX V))
    (CAAR U,CAAR V);

SYMBOLIC PROCEDURE CDARX U;
   CDR CARX U;

SYMBOLIC PROCEDURE PREPSQ!* U;
   BEGIN SCALAR X;
	IF NULL NUMR U THEN RETURN 0;
	X := KORD!*;
	KORD!* := APPEND((FOR EACH J IN FACTORS!*
		     CONC IF NOT IDP J THEN NIL
			   ELSE FOR EACH K IN GET(J,'KLIST)
				     COLLECT CAR K),
		   APPEND(FACTORS!*,ORDL!*));
	IF KORD!* NEQ X OR WTL!*
	  THEN U := FORMOP NUMR U . FORMOP DENR U;
	U := IF !*RAT OR (NOT !*FLOAT AND !*DIV) OR UPL!* OR DNL!*
	       THEN REPLUS PREPSQ!*1(NUMR U,DENR U,NIL)
	      ELSE SQFORM(U,FUNCTION(LAMBDA J;
			    REPLUS PREPSQ!*1(J,1,NIL)));
	KORD!* := X;
	RETURN U
   END;

SYMBOLIC PROCEDURE PREPSQ!*0(U,V);
   %U is a standard quotient, but not necessarily in lowest terms.
   %V a list of factored powers;
   %Value is equivalent list of prefix expressions (an implicit sum);
   BEGIN SCALAR X;
      RETURN IF NULL NUMR U THEN NIL
 	      ELSE IF (X := GCDF(NUMR U,DENR U)) NEQ 1
        THEN PREPSQ!*1(QUOTF(NUMR U,X),QUOTF(DENR U,X),V)
       ELSE PREPSQ!*1(NUMR U,DENR U,V)
   END;

SYMBOLIC PROCEDURE PREPSQ!*1(U,V,W);
   %U and V are the numerator and denominator expression resp,
   %in lowest terms.
   %W is a list of powers to be factored from U;
   BEGIN SCALAR X,Y,Z;
	%look for "factors" in the numerator;
	IF NOT DOMAINP U AND (MVAR U MEMBER FACTORS!* OR (NOT
		ATOM MVAR U AND CAR MVAR U MEMBER FACTORS!*))
	  THEN RETURN NCONC(IF V=1 THEN PREPSQ!*0(LC U ./ V,LPOW U . W)
		ELSE (BEGIN SCALAR N,V1,Z1;
		%see if the same "factor" appears in denominator;
		N := LDEG U;
		V1 := V;
		Z1 := !*K2F MVAR U;
		WHILE (Z := QUOTF(V1,Z1))
		   DO <<V1 := Z; N := N-1>>;
		RETURN
		  PREPSQ!*0(LC U ./ V1,
			    IF N>0 THEN (MVAR U .** N) . W
			     ELSE IF N<0
			      THEN MKSP(LIST('EXPT,MVAR U,N),1) . W
			     ELSE W)
		   END),
			PREPSQ!*0(RED U ./ V,W));
	%now see if there are any remaining "factors" in denominator
	%(KORD!* contains all potential kernel factors);
	IF NOT DOMAINP V
	 THEN FOR EACH J IN KORD!* DO
	   BEGIN INTEGER N; SCALAR Z1;
		N := 0;
		Z1 := !*K2F J;
		WHILE Z := QUOTF(V,Z1) DO <<N := N-1; V := Z>>;
		IF N<0 THEN W := MKSP(LIST('EXPT,J,N),1) . W
           END;
	%now all "factors" have been removed;
	IF KERNLP U THEN <<U := MKKL(W,U); W := NIL>>;
	IF DNL!*
	  THEN <<X := IF NULL !*ALLFAC THEN 1 ELSE CKRN U;
		 Z := CKRN!*(X,DNL!*);
		 X := QUOTOF(X,Z);
		 U := QUOTOF(U,Z);
		 V := QUOTOF(V,Z)>>;
	Y := CKRN V;
	IF UPL!*
	  THEN <<Z := CKRN!*(Y,UPL!*);
		 Y := QUOTOF(Y,Z);
		 U := QUOTOF(U,Z);
		 V := QUOTOF(V,Z)>>;
	IF NULL !*DIV AND NULL !*FLOAT THEN Y := 1;
	U := CANONSQ (U . QUOTOF(V,Y));
%	IF !*GCD THEN U := CANCEL U;
	U := QUOTOF(NUMR U,Y) ./ DENR U;
	IF NULL !*ALLFAC THEN X := 1 ELSE X := CKRN NUMR U;
	IF !*ALLFAC AND X NEQ CAR U THEN GO TO B
	 ELSE IF W THEN <<W := EXCHK(W,NIL,NIL); GO TO C>>;
    D:	U := PREPSQ U;
	RETURN IF EQCAR(U,'PLUS) THEN CDR U ELSE LIST U;
    B:	IF ONEP X AND NULL W THEN GO TO D
	 ELSE IF !*FLOAT THEN X := QUOTOF(X,KERNLP X);
	U := QUOTOF(NUMR U,X) . DENR U;
	W := PREPF MKKL(W,X);
	IF U = (1 ./ 1) THEN RETURN W
	 ELSE IF EQCAR(W,'TIMES) THEN W := CDR W
	 ELSE W := LIST W;
    C:	RETURN LIST RETIMES ACONC(W,PREPSQ U)
   END;

SYMBOLIC PROCEDURE MKKL(U,V);
   IF NULL U THEN V ELSE MKKL(CDR U,LIST (CAR U . V));

SYMBOLIC PROCEDURE CKRN!*(U,V);
   IF NULL U THEN ERRACH 'CKRN!*
    ELSE IF DOMAINP U THEN 1
    ELSE IF CAAAR U MEMBER V
       THEN LIST (CAAR U . CKRN!*(CDR CARX U,V))
    ELSE CKRN!*(CDR CARX U,V);


COMMENT Procedures for printing the structure of expressions;

FLUID '(COUNTR VAR VARLIS);

SYMBOLIC PROCEDURE STRUCTR U;
   BEGIN SCALAR COUNTR,FVAR,VAR,VARLIS;
      %VARLIS is a list of elements of form:
      %(<unreplaced expression> . <newvar> . <replaced exp>);
      COUNTR :=0;
      FVAR := VAR := VARNAM!*;
      IF CDR U THEN FVAR := CADR U;
      U := SIMPCAR U;
      U := STRUCTF NUMR U./ STRUCTF DENR U;
      IF NULL !*FORT THEN MATHPRINT MK!*SQ U;
	IF COUNTR=0 AND NULL !*FORT THEN RETURN NIL;
      IF NULL !*FORT THEN <<IF NULL !*NAT THEN TERPRI();
			    PRIN2T "   WHERE">>
       ELSE VARLIS := REVERSIP VARLIS;
      FOR EACH X IN VARLIS DO
	 <<TERPRI!* T;
	   IF NULL !*FORT THEN PRIN2!* "      ";
	     VARPRI(CDDR X,LIST MKQUOTE CADR X,T)>>;
      IF !*FORT THEN VARPRI(MK!*SQ U,LIST MKQUOTE FVAR,T)
   END;

RLISTAT '(STRUCTR);

SYMBOLIC PROCEDURE STRUCTF U;
   IF NULL U THEN NIL
    ELSE IF DOMAINP U THEN U
    ELSE BEGIN SCALAR X,Y;
	X := MVAR U;
	IF SFP X THEN IF Y := ASSOC(X,VARLIS) THEN X := CADR Y
		ELSE X := STRUCTK(PREPSQ!*(STRUCTF X ./ 1),GENVAR(),X)
	 ELSE IF NOT ATOM X AND NOT ATOMLIS CDR X
	  THEN IF Y := ASSOC(X,VARLIS) THEN X := CADR Y
		ELSE X := STRUCTK(X,GENVAR(),X);
	RETURN X .** LDEG U .* STRUCTF LC U .+ STRUCTF RED U
     END;

SYMBOLIC PROCEDURE STRUCTK(U,ID,V);
   BEGIN SCALAR X;
      IF X := SUBCHK1(U,VARLIS,ID)
	THEN RPLACD(X,(V . ID . U) . CDR X)
       ELSE IF X := SUBCHK2(U,VARLIS)
	THEN VARLIS := (V . ID . X) . VARLIS
       ELSE VARLIS := (V . ID . U) . VARLIS;
      RETURN ID
   END;

SYMBOLIC PROCEDURE SUBCHK1(U,V,ID);
   BEGIN SCALAR W;
      WHILE V DO
       <<SMEMBER(U,CDDAR V)
	    AND <<W := V; RPLACD(CDAR V,SUBST(ID,U,CDDAR V))>>;
	 V := CDR V>>;
      RETURN W
   END;

SYMBOLIC PROCEDURE SUBCHK2(U,V);
   BEGIN SCALAR BOOL;
      FOR EACH X IN V DO
       SMEMBER(CDDR X,U)
	  AND <<BOOL := T; U := SUBST(CADR X,CDDR X,U)>>;
      IF BOOL THEN RETURN U ELSE RETURN NIL
   END;

UNFLUID '(COUNTR VAR VARLIS);


%*********************************************************************
%*********************************************************************
%                       COEFF OPERATOR PACKAGE
%*********************************************************************
%********************************************************************;

%*********************************************************************
%		   REQUIRES EXTENDED OUTPUT PACKAGE
%********************************************************************;

FLAG ('(HIPOW!* LOWPOW!*),'SHARE);

GLOBAL '(HIPOW!* LOWPOW!*);

SYMBOLIC PROCEDURE COEFF(U,V,W);
   BEGIN SCALAR X,Y,Z;
	V := !*A2K V;
	IF ATOM W THEN (IF NOT ARRAYP W
	   THEN (IF NUMBERP(W := REVAL W) THEN TYPERR(W,'ID)))
	 ELSE IF NOT ARRAYP CAR W THEN TYPERR(CAR W,'array)
	 ELSE W := CAR W . FOR EACH X IN CDR W
			    COLLECT IF X EQ 'TIMES THEN X ELSE REVAL X;
	U := !*Q2F SIMP!* U;
	X := SETKORDER LIST V;
	Y := REORDER U;
	SETKORDER X;
	IF NULL Y THEN GO TO B0;
	WHILE NOT DOMAINP Y AND MVAR Y=V
	   DO <<Z := (LDEG Y . MK!*SQ1 CANCEL (LC Y ./ 1)) . Z;
		Y := RED Y>>;
    B:	IF NULL Y THEN GO TO B1;
    B0: Z := (0 . MK!*SQ1 CANCEL (Y ./ 1)) . Z;
    B1: LOWPOW!* := CAAR Z;
	IF (NOT ATOM W AND ATOM CAR W
			 AND (Y := DIMENSION CAR W))
	     OR ((Y := DIMENSION W) AND NULL CDR Y)
	 THEN GO TO G;
	Y := EXPLODE W;
	W := NIL;
    C:	W := INTERN COMPRESS APPEND(Y,EXPLODE CAAR Z) . W;
	SETK1(CAR W,CDAR Z,T);
	IF NULL CDR Z THEN GO TO D;
	Z := CDR Z;
	GO TO C;
    D:	HIPOW!* := CAAR Z;
	LPRIM ACONC(W,"are non zero");
    E:	RETURN HIPOW!*;
    G:	Z := REVERSE Z;
	IF ATOM W
	  THEN <<IF CAAR Z NEQ (CAR Y-1)
		   THEN <<Y := LIST(CAAR Z+1);
			  PUT(W,'ARRAY,MKARRAY Y);
			  PUT(W,'DIMENSION,Y)>>;
		 W := LIST(W,'TIMES)>>;
	HIPOW!* := CAAR Z;
	Y := PAIR(CDR W,Y);
    G0: WHILE NOT SMEMQ('TIMES,CAAR Y) DO Y := CDR Y;
	Y := CDAR Y-REVAL SUBST(0,'TIMES,CAAR Y)-1;
	   %-1 needed since DIMENSION gives length, not highest index;
	IF CAAR Z>Y
	  THEN REDERR LIST("Index",CAAR Z,"out of range");
    H:	IF NULL Z OR Y NEQ CAAR Z
	  THEN SETELV(SUBST(Y,'TIMES,W),0)
	 ELSE <<SETELV(SUBST(Y,'TIMES,W),CDAR Z); Z := CDR Z>>;
	IF Y=0 THEN GO TO E;
	Y := Y-1;
	GO TO H
   END;

SYMBOLIC PROCEDURE MK!*SQ1 U;
   IF WTL!* THEN PREPSQ U ELSE MK!*SQ U;

FLAG ('(COEFF),'OPFN);

FLAG ('(COEFF),'NOVAL);


%*********************************************************************
%*********************************************************************
%                     ASYMPTOTIC COMMAND PACKAGE
%********************************************************************;
%********************************************************************;

SYMBOLIC PROCEDURE WEIGHT U;
   BEGIN SCALAR Y,Z;
	RMSUBS();
	FOR EACH X IN U DO
	   IF NOT EQEXPR X THEN ERRPRI2(X,'HOLD)
	    ELSE <<Y := !*A2K CADR X;
		   Z := REVAL CADDR X;
		   IF NOT (NUMBERP Z AND FIXP Z AND Z>0)
		     THEN TYPERR(Z,"weight");
		   WTL!* :=  (Y . Z) . DELASC(Y,WTL!*)>>
   END;

SYMBOLIC PROCEDURE WTLEVEL U;
   BEGIN INTEGER N; SCALAR X;
	N := REVAL CAR U;
	IF NOT(NUMBERP N AND FIXP N AND NOT N<0)
	  THEN ERRPRI2(N,'HOLD);
	N := N+1;
	X := ATSOC('K!*,ASYMPLIS!*);
	IF N=CDR X THEN RETURN NIL ELSE IF N<=CDR X THEN RMSUBS2();
	RMSUBS1();
	RPLACD(X,N)
   END;

RLISTAT '(WEIGHT WTLEVEL);

ALGEBRAIC LET K!***2=0;


%*********************************************************************
%*********************************************************************
%			LINEAR OPERATOR PACKAGE
%*********************************************************************
%********************************************************************;

%Global variables referenced in this Section;

GLOBAL '(DEPL!*);   %list of dependencies among kernels;

%*********************************************************************
%      FUNCTIONS FOR DEFINING AND CHECKING EXPRESSION DEPENDENCY
%********************************************************************;

SYMBOLIC PROCEDURE DEPEND U;
   FOR EACH X IN CDR U DO DEPEND1(CAR U,X,T);

SYMBOLIC PROCEDURE NODEPEND U;
   <<RMSUBS(); FOR EACH X IN CDR U DO DEPEND1(CAR U,X,NIL)>>;

RLISTAT '(DEPEND NODEPEND);

SYMBOLIC PROCEDURE DEPEND1(U,V,BOOL);
   BEGIN SCALAR Y,Z;
      U := !*A2K U;
      V := !*A2K V;
      IF U EQ V THEN RETURN NIL;
      Y := ASSOC(U,DEPL!*);
      IF Y THEN IF BOOL THEN RPLACD(Y,UNION(LIST V,CDR Y))
		 ELSE IF (Z := DELETE(V,CDR Y)) THEN RPLACD(Y,Z)
		 ELSE DEPL!* := DELETE(Y,DEPL!*)
       ELSE IF NULL BOOL
	 THEN LPRIM LIST(U,"has no prior dependence on",V)
       ELSE DEPL!* := LIST(U,V) . DEPL!*
   END;

SYMBOLIC PROCEDURE DEPENDS(U,V);
   IF NULL U OR NUMBERP U OR NUMBERP V THEN NIL
    ELSE IF U=V THEN U
    ELSE IF ATOM U AND U MEMQ FRLIS!* THEN T
      %to allow the most general pattern matching to occur;
    ELSE IF (LAMBDA X; X AND LDEPENDS(CDR X,V)) ASSOC(U,DEPL!*)
     THEN T
    ELSE IF NOT ATOM U
      AND (LDEPENDS(CDR U,V) OR DEPENDS(CAR U,V)) THEN T
    ELSE IF ATOM V THEN NIL
    ELSE DEPENDSL(U,CDR V);

SYMBOLIC PROCEDURE LDEPENDS(U,V);
   U AND (DEPENDS(CAR U,V) OR LDEPENDS(CDR U,V));

SYMBOLIC PROCEDURE DEPENDSL(U,V);
   V AND (DEPENDS(U,CAR V) OR DEPENDSL(U,CDR V));

SYMBOLIC PROCEDURE FREEOF(U,V);
   NOT(SMEMBER(V,U) OR V MEMBER ASSOC(U,DEPL!*));

FLAG('(FREEOF),'BOOLEAN);

INFIX FREEOF;

PRECEDENCE FREEOF,LESSP;   %put it above all boolean operators;


%*********************************************************************
%	      FUNCTIONS FOR SIMPLIFYING LINEAR OPERATORS
%********************************************************************;

SYMBOLIC PROCEDURE LINEAR U;
   FOR EACH X IN U DO
    <<IF NOT IDP X THEN TYPERR(X,'operator); FLAG(LIST X,'LINEAR);
      MKOP X>>;

RLISTAT '(LINEAR);

PUT('LINEAR,'SIMPFG,'((RMSUBS)));

SYMBOLIC PROCEDURE FORMLNR U;
  (LAMBDA (X,Y,Z);
   IF Y = 1 THEN U
    ELSE IF NOT DEPENDS(Y,CAR Z)
     THEN LIST('TIMES,Y,X . 1 . Z)
    ELSE IF ATOM Y THEN U
    ELSE IF CAR Y EQ 'PLUS
     THEN 'PLUS . FOR EACH J IN CDR Y COLLECT FORMLNR(X . J. Z)
    ELSE IF CAR Y EQ 'MINUS
     THEN LIST('MINUS,FORMLNR(X . CADR Y . Z))
    ELSE IF CAR Y EQ 'DIFFERENCE
     THEN LIST('DIFFERENCE,FORMLNR(X . CADR Y . Z),
			   FORMLNR(X . CADDR Y . Z))
    ELSE IF CAR Y EQ 'TIMES THEN FORMLNTMS(X,CDR Y,Z,U)
    ELSE IF CAR Y EQ 'QUOTIENT THEN FORMLNQUOT(X,CDR Y,Z,U)
    ELSE IF CAR Y EQ 'RECIP AND NOT DEPENDS(CADR Y,CAR Z)
     THEN LIST('QUOTIENT,X . 1 . Z,CADR Y)
    ELSE (LAMBDA V; IF V THEN LIST('TIMES,CAR V,X . CDR V . Z) ELSE U)
	  EXPT!-SEPARATE(Y,CAR Z))
   (CAR U,CADR U,!*A2K CADDR U . CDDDR U);

SYMBOLIC PROCEDURE FORMSEPARATE(U,V);
   %separates U into two parts, and returns a dotted pair of them: those
   %which are not commutative and do not depend on V, and the remainder;
   BEGIN SCALAR W,X,Y;
      FOR EACH Z IN U DO
	IF NOT NONCOMP Z AND NOT DEPENDS(Z,V) THEN X := Z . X
	 ELSE IF (W := EXPT!-SEPARATE(Z,V))
	THEN <<X := CAR W . X; Y := CDR W . Y>>
	 ELSE Y := Z . Y;
      RETURN REVERSIP X . REVERSIP Y
   END;

SYMBOLIC PROCEDURE EXPT!-SEPARATE(U,V);
   %determines if U is an expression in EXPT that can be separated into
   %two parts, one that does not depend on V and one that does,
   %except if there is no non-dependent part, NIL is returned;
   IF NOT EQCAR(U,'EXPT) OR DEPENDS(CADR U,V)
	   OR NOT EQCAR(CADDR U,'PLUS)
     THEN NIL
    ELSE EXPT!-SEPARATE1(CDADDR U,CADR U,V);

SYMBOLIC PROCEDURE EXPT!-SEPARATE1(U,V,W);
   BEGIN SCALAR X;
      X := FORMSEPARATE(U,W);
      RETURN IF NULL CAR X THEN NIL
	      ELSE LIST('EXPT,V,REPLUS CAR X) .
		   IF NULL CDR X THEN 1 ELSE LIST('EXPT,V,REPLUS CDR X)
   END;

SYMBOLIC PROCEDURE FORMLNTMS(U,V,W,X);
   %U is a linear operator, V its first argument with TIMES removed,
   %W the rest of the arguments and X the whole expression.
   %Value is the transformed expression;
   BEGIN SCALAR Y;
      Y := FORMSEPARATE(V,CAR W);
      RETURN IF NULL CAR Y THEN X
	      ELSE 'TIMES . ACONC(CAR Y,
		IF NULL CDDR Y THEN FORMLNR(U . CADR Y . W)
		      ELSE U . ('TIMES . CDR Y) . W)
   END;

SYMBOLIC PROCEDURE FORMLNQUOT(FN,QUOTARGS,REST,WHOLE);
   %FN is a linear operator, QUOTARGS its first argument with QUOTIENT
   %removed, REST the remaining arguments, WHOLE the whole expression.
   %Value is the transformed expression;
   BEGIN SCALAR X;
      RETURN IF NOT DEPENDS(CADR QUOTARGS,CAR REST)
	 THEN LIST('QUOTIENT,FORMLNR(FN . CAR QUOTARGS . REST),
		   CADR QUOTARGS)
	ELSE IF NOT DEPENDS(CAR QUOTARGS,CAR REST)
	       AND CAR QUOTARGS NEQ 1
	 THEN LIST('TIMES,CAR QUOTARGS,
		   FORMLNR(FN . LIST('RECIP,CADR QUOTARGS) . REST))
	ELSE IF EQCAR(CAR QUOTARGS,'PLUS)
	 THEN 'PLUS . FOR EACH J IN CDAR QUOTARGS
		COLLECT FORMLNR(FN . ('QUOTIENT . J . CDR QUOTARGS)
				 . REST)
	ELSE IF EQCAR(CAR QUOTARGS,'MINUS)
	 THEN LIST('MINUS,FORMLNR(FN .
			('QUOTIENT . CADAR QUOTARGS . CDR QUOTARGS)
			    . REST))
	ELSE IF EQCAR(CAR QUOTARGS,'TIMES)
		AND CAR(X := FORMSEPARATE(CDAR QUOTARGS,CAR REST))
	 THEN 'TIMES . ACONC(CAR X,
		FORMLNR(FN . LIST('QUOTIENT,MKTIMES CDR X,
			     CADR QUOTARGS) . REST))
	ELSE IF EQCAR(CADR QUOTARGS,'TIMES)
		AND CAR(X := FORMSEPARATE(CDADR QUOTARGS,CAR REST))
	 THEN LIST('TIMES,LIST('RECIP,MKTIMES CAR X),
		FORMLNR(FN . LIST('QUOTIENT,CAR QUOTARGS,MKTIMES CDR X)
			 . REST))
	ELSE IF X := EXPT!-SEPARATE(CAR QUOTARGS,CAR REST)
	 THEN LIST('TIMES,CAR X,FORMLNR(FN . LIST('QUOTIENT,CDR X,CADR
						     QUOTARGS) . REST))
	ELSE IF X := EXPT!-SEPARATE(CADR QUOTARGS,CAR REST)
	 THEN LIST('TIMES,LIST('RECIP,CAR X),
		   FORMLNR(FN . LIST('QUOTIENT,CAR QUOTARGS,CDR X)
			      . REST))
	ELSE IF (X := REVAL!* CADR QUOTARGS) NEQ CADR QUOTARGS
	 THEN FORMLNQUOT(FN,LIST(CAR QUOTARGS,X),REST,WHOLE)
	ELSE WHOLE
   END;

SYMBOLIC PROCEDURE MKTIMES U;
   IF NULL CDR U THEN CAR U ELSE 'TIMES . U;

SYMBOLIC PROCEDURE REVAL!* U;
   %like REVAL, except INTSTR is always ON;
   BEGIN SCALAR !*INTSTR;
      !*INTSTR := T;
      RETURN REVAL U
   END;


%*********************************************************************
%       FUNCTIONS FOR ALGEBRAIC MODE OPERATIONS ON POLYNOMIALS
%********************************************************************;

SYMBOLIC PROCEDURE POLPART(EXPRN,KERN,FN);
   BEGIN SCALAR X,Y;
      EXPRN := !*A2F EXPRN;
      KERN := !*A2K KERN;
      IF DOMAINP EXPRN THEN RETURN NIL
       ELSE IF MVAR EXPRN EQ KERN
	THEN RETURN !*F2A APPLY(FN,LIST EXPRN);
      X := SETKORDER LIST KERN;
      EXPRN := REORDER EXPRN;
      IF NOT(MVAR EXPRN EQ KERN) THEN EXPRN := NIL
       ELSE EXPRN := APPLY(FN,LIST EXPRN);
      SETKORDER X;
      RETURN !*F2A EXPRN
   END;

SYMBOLIC PROCEDURE DEG(U,KERN); POLPART(U,KERN,'CDAAR);

SYMBOLIC PROCEDURE LCOF(U,KERN); POLPART(U,KERN,'CDAR);

SYMBOLIC PROCEDURE LTERM(U,KERN); POLPART(U,KERN,'!*LTERM);

SYMBOLIC PROCEDURE !*LTERM U; LT U .+ NIL;

SYMBOLIC PROCEDURE MAINVAR U;
   IF DOMAINP(U := !*A2F U) THEN NIL
    ELSE IF SFP(U := MVAR U) THEN PREPF U
    ELSE U;

SYMBOLIC PROCEDURE REDUCT(EXPRN,KERN);
   BEGIN SCALAR X,Y;
      EXPRN := !*A2F EXPRN;
      KERN := !*A2K KERN;
      IF DOMAINP EXPRN THEN RETURN EXPRN
       ELSE IF MVAR EXPRN EQ KERN THEN RETURN !*F2A CDR EXPRN;
      X := SETKORDER LIST KERN;
      EXPRN := REORDER EXPRN;
      IF MVAR EXPRN EQ KERN THEN EXPRN := CDR EXPRN;
      SETKORDER X;
      RETURN !*F2A EXPRN
   END;

SYMBOLIC OPERATOR DEG,LCOF,LTERM,MAINVAR,REDUCT;


%*********************************************************************
%	    SIMPLIFICATION RULES FOR ELEMENTARY FUNCTIONS
%********************************************************************;

ALGEBRAIC;

COMMENT RULE FOR I**2;

REMFLAG('(I),'RESERVED);

LET I**2= -1;

FLAG('(E I NIL PI T),'RESERVED);

COMMENT LOGARITHMS;

OPERATOR LOG;

LET LOG(E)= 1,
    LOG(1)= 0;

FOR ALL X LET LOG(E**X)=X;

FOR ALL X LET DF(LOG(X),X) = 1/X;

COMMENT TRIGONOMETRICAL FUNCTIONS;

SYMBOLIC PROCEDURE SIMPTRIG U;
   %This is a basic simplification function for trigonometrical
   %functions. The prefix expression U is of the form (<trig-function>
   % <argument>). It is assumed that the trig-function is either even
   %or odd, with even the default (and the odd case a flag "odd"). 
   %The value is a standard quotient for the simplified expression;
   BEGIN SCALAR BOOL,FN,X,Y,Z;
      FN := CAR U;
      U := CDR U;
      IF NULL U OR CDR U
	THEN REDERR LIST("Wrong number of arguments to",FN);
      U := SIMP!* CAR U;
      IF NULL NUMR U AND FLAGP(FN,'ODD) THEN RETURN NIL ./ 1;
      X := LIST(FN,PREPSQ!* U);
      IF SUBFG!* AND (Z := OPMTCH X) THEN RETURN SIMP Z
       ELSE IF Z := NUMVALCHK X THEN RETURN Z
       ELSE IF MINUSF NUMR U
	THEN <<IF FLAGP(FN,'ODD) THEN BOOL := T;
	       X := LIST(FN,PREPSQ!*(NEGF NUMR U ./ DENR U));
	       IF SUBFG!* AND (Z := OPMTCH X) THEN RETURN SIMP Z>>;
      X := MKSQ(X,1);
      RETURN IF BOOL THEN NEGSQ X ELSE X
   END;

DEFLIST('((ACOS SIMPTRIG) (ASIN SIMPTRIG) (ATAN SIMPTRIG)
	  (ACOSH SIMPTRIG) (ASINH SIMPTRIG) (ATANH SIMPTRIG)
	  (COS SIMPTRIG) (SIN SIMPTRIG) (TAN SIMPTRIG)
	  (COT SIMPTRIG)(ACOT SIMPTRIG)(COTH SIMPTRIG)(ACOTH SIMPTRIG)
	  (COSH SIMPTRIG) (SINH SIMPTRIG) (TANH SIMPTRIG)
   ),'SIMPFN);

%The following declaration causes the simplifier to pass the full
%expression (including the function) to SIMPTRIG;

FLAG ('(ACOS ASIN ATAN ACOSH ASINH ATANH COS SIN TAN COSH SINH TANH
	COT ACOT COTH ACOTH),
      'FULL);

FLAG('(ASIN ATAN ASINH ATANH SIN TAN SINH TANH COT ACOT COTH ACOTH),
      'ODD);

%In the following rules, it is not necessary to let f(0)=0, when f
%is odd, since SIMPTRIG already does this;

LET COS(0)= 1,
    COS(PI/2)= 0,
    SIN(PI/2)= 1,
    SIN(PI)= 0,
    COS(PI)=-1,
    COSH 0=1;

FOR ALL X LET COS ACOS X=X, SIN ASIN X=X, TAN ATAN X=X,
	   COSH ACOSH X=X, SINH ASINH X=X, TANH ATANH X=X,
	   COT ACOT X=X, COTH ACOTH X=X;


FOR ALL N SUCH THAT NUMBERP N AND FIXP N
	  LET SIN(N*PI)=0, COS(N*PI) = (-1)**N;

FOR ALL X LET DF(ACOS(X),X)= -SQRT(1-X**2)/(1-X**2),
	      DF(ASIN(X),X)= SQRT(1-X**2)/(1-X**2),
	      DF(ATAN(X),X)= 1/(1+X**2),
	      DF(ACOSH(X),X)= SQRT(X**2-1)/(X**2-1),
	      DF(ASINH(X),X)= SQRT(X**2+1)/(X**2+1),
	      DF(ATANH(X),X)= 1/(1-X**2),
	      DF(COS X,X)= -SIN(X),
	      DF(SIN(X),X)= COS(X),
              DF(TAN X,X)=1+TAN X**2,
              DF(SINH X,X)=COSH X,
              DF(COSH X,X)=SINH X,
              DF(TANH X,X)=1-TANH X**2,
	      DF(COT X,X)=-1-COT X**2,
	      DF(COTH X,X)=1-COTH X**2;

LET   E**(I*PI/2) = I,
      E**(I*PI) = -1,
      E**(3*I*PI/2)=-I;

%FOR ALL X LET E**LOG X=X;   %requires every power to be checked;

FOR ALL X,Y LET DF(X**Y,X)= Y*X**(Y-1),
                DF(X**Y,Y)= LOG X*X**Y;

COMMENT SQUARE ROOTS;

DEFLIST('((SQRT SIMPSQRT)),'SIMPFN);

%FOR ALL X LET SQRT X**2=X;

FLUID '(!*!*SQRT);   %Used to indicate that SQRTs have been used;

SYMBOLIC PROCEDURE MKSQRT U;
   <<IF NULL !*!*SQRT THEN <<!*!*SQRT := T;
			     ALGEBRAIC FOR ALL X LET SQRT X**2=X>>;
     LIST('SQRT,U)>>;

FOR ALL X LET DF(SQRT X,X)=SQRT X/(2*X);


COMMENT ERF,EXP, EXPINT AND DILOG;

OPERATOR ERF,EXP,EXPINT,DILOG;

LET ERF 0=0;

LET DILOG(0)=PI**2/6;

FOR ALL X LET ERF(-X)=-ERF X;

FOR ALL X LET DF(ERF X,X)=2*SQRT(PI)*E**(-X**2/2)/PI;

FOR ALL X LET EXP(X)=E**X;

FOR ALL X LET DF(EXPINT(X),X)=E**X/X;

FOR ALL X LET DF(DILOG X,X)=-LOG X/(X-1);


SYMBOLIC;


%*********************************************************************
%*********************************************************************
%	  SIMPLIFICATION FUNCTIONS FOR NON-SCALAR QUANTITIES
%*********************************************************************
%********************************************************************;

SYMBOLIC PROCEDURE NSSIMP(U,V);
   %U is a prefix expression involving non-commuting
   %quantities. Result is an expression of the form
   % SUM R(I)*PRODUCT M(I,J) where the R(I) are standard
   %quotients and the M(I,J) non-commuting expressions;
   %N. B: the products in M(I,J) are returned in reverse order
   %(to facilitate, e.g., matrix augmentation);
   BEGIN SCALAR W,X,Y,Z;
	U := DSIMP(U,V);
    A:	IF NULL U THEN RETURN Z;
	W := CAR U;
    C:	IF NULL W THEN GO TO D
	 ELSE IF NUMBERP CAR W
		OR NOT(EQCAR(CAR W,'!*DIV) OR APPLY(V,LIST CAR W))
	  THEN X := ACONC(X,CAR W)
	 ELSE Y := ACONC(Y,CAR W);
	W := CDR W;
	GO TO C;
    D:	IF NULL Y THEN GO TO ER;
    E:	Z := ADDNS(((IF NULL X THEN 1 ./ 1 ELSE SIMPTIMES X) . Y),Z);
	U := CDR U;
	X := Y:= NIL;
	GO TO A;
    ER: Y := GET(V,'NAME);
	IF IDP CAR X
	  THEN IF NOT FLAGP(CAR X,GET(Y,'FN)) THEN REDMSG(CAR X,Y)
	    ELSE REDERR LIST(Y,X,"not set")
	 ELSE IF Y EQ 'MATRIX THEN <<Y:= '((MAT (1))); GO TO E>>
	 %to allow a scalar to be a 1 by 1 matrix;
	 ELSE REDERR LIST("Missing",Y,X);
	PUT(CAR X,Y,Y);
	Y := LIST CAR X;
	X := CDR X;
	GO TO E
   END;

SYMBOLIC PROCEDURE DSIMP(U,V);
   %result is a list of lists representing a sum of products;
   %N. B: symbols are in reverse order in product list;
   IF NUMBERP U THEN LIST LIST U
    ELSE IF ATOM U THEN (LAMBDA W; (LAMBDA X;
	IF X AND NOT X EQ W AND SUBFG!* THEN DSIMP(X,V)
	 ELSE IF FLAGP(U,'SHARE) THEN DSIMP(EVAL U,V)
	 ELSE <<FLAG(LIST U,'USED!*); LIST LIST U>>)
     GET(U,W))
    GET(V,'NAME)
    ELSE IF CAR U EQ 'PLUS
     THEN FOR EACH J IN CDR U CONC DSIMP(J,V)
    ELSE IF CAR U EQ 'DIFFERENCE
     THEN NCONC(DSIMP(CADR U,V),
		DSIMP('MINUS . CDDR U,V))
    ELSE IF CAR U EQ 'MINUS
     THEN DSIMPTIMES(LIST(-1,CARX CDR U),V)
    ELSE IF CAR U EQ 'TIMES
     THEN DSIMPTIMES(CDR U,V)
    ELSE IF CAR U EQ 'QUOTIENT
     THEN DSIMPTIMES(LIST(CADR U, LIST('RECIP,CARX CDDR U)),V)
    ELSE IF NOT APPLY(V,LIST U) THEN LIST LIST U
    ELSE IF CAR U EQ 'RECIP THEN LIST LIST LIST('!*DIV,CARX CDR U)
    ELSE IF CAR U EQ 'EXPT THEN (LAMBDA Z;
       IF NOT NUMBERP Z OR NOT FIXP Z THEN ERRPRI2(U,T)
	ELSE IF Z<0
	 THEN LIST LIST LIST('!*DIV,'TIMES . NLIST(CADR U,-Z))
	 ELSE IF Z=0 THEN LIST LIST LIST('!*DIV,CADR U,1)
	ELSE DSIMPTIMES(NLIST(CADR U,Z),V))
      REVAL CADDR U
    ELSE IF CAR U EQ 'MAT THEN LIST LIST U
    ELSE IF ARRAYP CAR U
       THEN DSIMP(GETELV U,V)
    ELSE (LAMBDA X; IF X THEN DSIMP(X,V)
		     ELSE (LAMBDA Y; IF Y THEN DSIMP(Y,V)
					  ELSE LIST LIST U)
				OPMTCH REVOP1 U)
	OPMTCH U;

SYMBOLIC PROCEDURE DSIMPTIMES(U,V);
   IF NULL U THEN ERRACH 'DSIMPTIMES
    ELSE IF NULL CDR U THEN DSIMP(CAR U,V)
    ELSE (LAMBDA J;
	  FOR EACH K IN DSIMPTIMES(CDR U,V) CONC MAPPEND(J,K))
       DSIMP(CAR U,V);

SYMBOLIC PROCEDURE ADDNS(U,V);
   IF NULL V THEN LIST U
    ELSE IF CDR U=CDAR V
       THEN (LAMBDA X; IF NULL CAR X THEN CDR V
			 ELSE (X . CDR U) . CDR V)
       ADDSQ(CAR U,CAAR V)
    ELSE IF ORDP(CDR U,CDAR V) THEN U . V
    ELSE CAR V . ADDNS(U,CDR V);

SYMBOLIC PROCEDURE NSLET(U,V,W,B,FLG);
   BEGIN
	IF FLG THEN GO TO A
	 ELSE IF NOT ATOM U
	  THEN IF ARRAYP CAR U THEN GO TO A ELSE TYPERR(U,"array");
	REDMSG(U,W);
	PUT(U,W,W);
    A:	IF NULL B THEN GO TO C
	 ELSE IF NOT ATOM U OR FLAGP(U,'USED!*) THEN RMSUBS();
    C:	IF NOT ATOM U
	  THEN IF ARRAYP CAR U
		 THEN SETELV(U,IF B THEN V ELSE NIL)
		ELSE PUT(CAR U,'OPMTCH,XADD!*(CDR U .
		    LIST(NIL . (IF MCOND!* THEN MCOND!* ELSE T),V,NIL),
			GET(CAR U,'OPMTCH),U,B))
	 ELSE IF NULL B THEN REMPROP(U,W)
	 ELSE IF W EQ 'MATRIX AND NOT EQCAR(V,'MAT)
	  THEN PUT(U,W,IF MATP V THEN GET(V,'MATRIX)
			ELSE LIST('MAT,LIST V))   %1 by 1 matrix case;
	 ELSE PUT(U,W,V)
   END;

SYMBOLIC PROCEDURE NSP(U,V);
   IF NUMBERP U THEN NIL
    ELSE IF ATOM U THEN GET(U,V)
			  OR (FLAGP(U,'SHARE) AND NSP(EVAL U,V))
    ELSE IF CAR U MEMQ '(TIMES QUOTIENT) THEN NSOR(CDR U,V)
    ELSE IF CAR U MEMQ '(PLUS DIFFERENCE MINUS EXPT RECIP)
     THEN NSP(CADR U,V)
    ELSE IF ARRAYP CAR U THEN NSP(GETELX U,V)
    ELSE FLAGP(CAR U,GET(V,'FN));

SYMBOLIC PROCEDURE GETELX U;
   %to take care of free variables in LET statements;
   IF SMEMQLP(FRLIS!*,CDR U) THEN NIL
    ELSE IF NULL(U := GETELV U) THEN 0
    ELSE REVAL U;

SYMBOLIC PROCEDURE NSOR(U,V);
   U AND (NSP(CAR U,V) OR NSOR(CDR U,V));


%*********************************************************************
%*********************************************************************
%			    MATRIX PACKAGE
%*********************************************************************
%********************************************************************;

%*********************************************************************
%     REQUIRES SIMPLIFICATION FUNCTIONS FOR NON-SCALAR QUANTITIES
%********************************************************************;

SYMBOLIC PROCEDURE MATRIX U;
   %declares list U as matrices;
   BEGIN SCALAR V,W; INTEGER N;
	TYPL!* := UNION('(MATP),TYPL!*);
    A:	IF NULL U THEN RETURN NIL
	 ELSE IF ATOM CAR U AND NOT TYPECHK(CAR U,'MATRIX)
	  THEN PUT(CAR U,'MATRIX,'MATRIX)
	 ELSE IF NOT IDP CAAR U
		OR LENGTH (V := REVLIS CDAR U) NEQ 2 OR NOT NUMLIS V
	  THEN GO TO ER
	 ELSE IF NOT TYPECHK(CAAR U,'MATRIX) THEN GO TO C;
    B:	U := CDR U;
	GO TO A;
    C:	N := CAR V;
    D:	IF N=0 THEN GO TO E;
	W := NZERO CADR V . W;
	N := N-1;
	GO TO D;
    E:	PUT(CAAR U,'MATRIX,'MAT . W);
	W := NIL;
	GO TO B;
    ER: ERRPRI2(CAR U,'HOLD);
	GO TO B
   END;

RLISTAT '(MATRIX);

SYMBOLIC PROCEDURE NZERO N;
   %returns a list of N zeros;
   IF N=0 THEN NIL ELSE 0 . NZERO(N-1);

SYMBOLIC PROCEDURE FORMMAT(U,VARS,MODE);
   'LIST . MKQUOTE 'MAT
     . FOR EACH X IN U COLLECT('LIST . FORMLIS(X,VARS,MODE));

PUT('MAT,'FORMFN,'FORMMAT);

SYMBOLIC PROCEDURE MATP U;
   %predicate which tests for matrix expressions;
   NSP(U,'MATRIX);

FLAG('(MAT TP),'MATFLG);

PUT('TP,'MSIMPFN,'TP);

PUT('MATP,'LETFN,'NSLET);

PUT('MATP,'NAME,'MATRIX);

PUT('MATRIX,'FN,'MATFLG);

PUT('MATP,'EVFN,'MATSM!*);

PUT('MATP,'PRIFN,'MATPRI!*);


END;
