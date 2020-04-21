%*********************************************************************
%*********************************************************************
%		      HIGH ENERGY PHYSICS PACKAGE
%*********************************************************************
%********************************************************************;

%Copyright (c) 1983 The Rand Corporation;

SYMBOLIC;

%*********************************************************************
%     REQUIRES SIMPLIFICATION FUNCTIONS FOR NON-SCALAR QUANTITIES
%********************************************************************;


%*********************************************************************
%            NON LOCAL VARIABLES REFERENCED IN THIS PACKAGE
%********************************************************************;

FLUID '(!*S!*);

GLOBAL '(DEFINDICES!* INDICES!* MUL!* NCMP!* NDIM!* TYPL!* !*SUB2);

DEFINDICES!* := NIL; %deferred indices in N dim calculations;

INDICES!* := NIL; %list of indices in High Energy Physics
		  %tensor expressions;
NDIM!* := 4;      %number of dimensions in gamma algebra;

COMMENT The generalizations in this package for n dimensional vector
	and gamma algebra are due to Gastmans, Van Proeyen and
	Verbaeten, University of Leuven, Belgium;


%*********************************************************************
%			  SOME DECLARATIONS
%********************************************************************;

DEFLIST ('((CONS SIMPDOT)),'SIMPFN);

SYMBOLIC PROCEDURE VECTOR U;
   VECTOR1 U;

SYMBOLIC PROCEDURE VECTOR1 U;
   <<TYPL!* := UNION('(HVECTORP),TYPL!*);
     FOR EACH X IN U DO PUT(X,'VECTOR,'VECTOR)>>;

SYMBOLIC PROCEDURE HVECTORP U;
   NSP(U,'VECTOR);

PUT('VECTOR,'FN,'VECFN);

PUT('HVECTORP,'LETFN,'NSLET);

PUT('HVECTORP,'NAME,'VECTOR);

PUT('HVECTORP,'EVFN,'VEVAL);

PUT('G,'SIMPFN,'SIMPGAMMA);

FLAGOP NONCOM,NOSPUR;

FLAG ('(G),'NONCOM);

SYMBOLIC PROCEDURE INDEX U;
   BEGIN VECTOR1 U; RMSUBS(); INDICES!* := UNION(INDICES!*,U) END;

SYMBOLIC PROCEDURE REMIND U;
   BEGIN INDICES!* := SETDIFF(INDICES!*,U) END;

SYMBOLIC PROCEDURE MASS U;
   <<TYPL!* := UNION('(HVECTORP),TYPL!*);
     FOR EACH X IN U DO
     <<PUT(CADR X,'MASS,CADDR X); PUT(CADR X,'VECTOR,'VECTOR)>>>>;

SYMBOLIC PROCEDURE GETMAS U;
   (LAMBDA X; IF X THEN X ELSE REDERR LIST(U,"has no mass"))
      GET!*(U,'MASS);

SYMBOLIC PROCEDURE VECDIM U;
   BEGIN
      TYPL!* := UNION('(HVECTORP),TYPL!*);
      NDIM!* := CAR U
   END;

SYMBOLIC PROCEDURE MSHELL U;
   BEGIN SCALAR X,Z;
	TYPL!* := UNION('(HVECTORP),TYPL!*);
    A:	IF NULL U THEN RETURN LET0(Z,NIL);
	X := GETMAS CAR U;
	Z := LIST('EQUAL,LIST('CONS,CAR U,CAR U),LIST('EXPT,X,2)) . Z;
	U := CDR U;
	GO TO A
   END;

RLISTAT '(VECDIM INDEX MASS MSHELL REMIND VECTOR);


%*********************************************************************
%	   FUNCTIONS FOR SIMPLIFYING HIGH ENERGY EXPRESSIONS
%********************************************************************;

SYMBOLIC PROCEDURE VEVAL U;
   BEGIN SCALAR Z;
	U := NSSIMP(U,'HVECTORP);
    A:	IF NULL U THEN RETURN REPLUS Z
	 ELSE IF NULL CDAR U THEN REDERR "Missing vector"
	 ELSE IF CDDAR U THEN REDERR LIST("Redundant vector",CDAR U);
	Z := ACONC(Z,RETIMES(PREPSQ CAAR U . CDAR U));
	U := CDR U;
	GO TO A
   END;

SYMBOLIC PROCEDURE VMULT U;
   BEGIN SCALAR Z;
	Z := LIST LIST(1 . 1);
    A:	IF NULL U THEN RETURN Z;
	Z := VMULT1(NSSIMP(CAR U,'HVECTORP),Z);
	IF NULL Z THEN RETURN;
	U := CDR U;
	GO TO A
   END;

SYMBOLIC PROCEDURE VMULT1(U,V);
   BEGIN SCALAR Z;
	IF NULL V THEN RETURN;
    A:	IF NULL U THEN RETURN Z
	 ELSE IF CDDAR U
	  THEN REDERR("Redundant vector" . CDAR U);
	Z := NCONC(Z,MAPCAR(V,FUNCTION (LAMBDA J;
	      MULTSQ(CAR J,CAAR U) . APPEND(CDR J,CDAR U))));
	U := CDR U;
	GO TO A
   END;

SYMBOLIC PROCEDURE SIMPDOT U;
   MKVARG(U,FUNCTION DOTORD);

SYMBOLIC PROCEDURE DOTORD U;
   <<IF XNP(U,INDICES!*) AND NOT MEMQ('ISIMPQ,MUL!*)
	   THEN MUL!* := ACONC(MUL!*,'ISIMPQ) ELSE NIL;
	IF 'A MEMQ U
	  THEN REDERR "A represents only gamma5 in vector expressions"
	 ELSE MKSQ('CONS . ORD2(CAR U,CARX(CDR U,'DOT)),1)>>;

SYMBOLIC PROCEDURE MKVARG(U,V);
   BEGIN SCALAR Z;
	U := VMULT U;
	Z := NIL ./ 1;
    A:	IF NULL U THEN RETURN Z;
	Z := ADDSQ(MULTSQ(APPLY(V,LIST CDAR U),CAAR U),Z);
	U := CDR U;
	GO TO A
   END;

SYMBOLIC PROCEDURE SPUR U;
   <<RMSUBS();
	 MAP(U,FUNCTION (LAMBDA J;
		   <<REMFLAG(LIST CAR J,'NOSPUR);
			 REMFLAG(LIST CAR J,'REDUCE)>>))>>;

RLISTAT '(SPUR);

SYMBOLIC PROCEDURE SIMPGAMMA !*S!*;
   IF NULL !*S!* OR NULL CDR !*S!*
       THEN REDERR "Missing arguments for G operator"
    ELSE BEGIN
	IF NOT MEMQ('ISIMPQ,MUL!*) THEN MUL!*:= ACONC(MUL!*,'ISIMPQ);
	NCMP!* := T;
	RETURN MKVARG(CDR !*S!*,FUNCTION (LAMBDA J;
			 LIST ((('G . CAR !*S!* . J) . 1) . 1) . 1))
    END;

SYMBOLIC PROCEDURE SIMPEPS U;
   MKVARG(U,FUNCTION EPSORD);

SYMBOLIC PROCEDURE EPSORD U;
   IF REPEATS U THEN NIL ./ 1 ELSE MKEPSQ U;

SYMBOLIC PROCEDURE MKEPSK U;
   %U is of the form (v1 v2 v3 v4).
   %Value is <sign flag> . <kernel for EPS(v1,v2,v3,v4)>;
   BEGIN SCALAR X;
	IF XNP(U,INDICES!*) AND NOT 'ISIMPQ MEMQ MUL!*
	  THEN MUL!* := ACONC(MUL!*,'ISIMPQ);
	X := ORDN U;
	U := PERMP(X,U);
	RETURN U . ('EPS . X)
   END;

SYMBOLIC PROCEDURE MKEPSQ U;
   (LAMBDA X; (LAMBDA Y; IF NULL CAR X THEN NEGSQ Y ELSE Y)
		 MKSQ(CDR X,1))
	MKEPSK U;


%*********************************************************************
%    FUNCTIONS FOR SIMPLIFYING VECTOR AND GAMMA MATRIX EXPRESSIONS
%********************************************************************;

SYMBOLIC SMACRO PROCEDURE MKG(U,L);
   %Value is the standard form for G(L,U);
   !*P2F('G . L . U TO 1);

SYMBOLIC SMACRO PROCEDURE MKA L;
   %Value is the standard form for G(L,A);
   !*P2F(LIST('G,L,'A) TO 1);

SYMBOLIC SMACRO PROCEDURE MKGF(U,L);
   MKSF('G . (L . U));

SYMBOLIC PROCEDURE MKG1(U,L);
   IF NOT FLAGP(L,'NOSPUR) THEN MKG(U,L) ELSE MKGF(U,L);

SYMBOLIC SMACRO PROCEDURE MKPF(U,V);
   MULTPF(U,V);

SYMBOLIC PROCEDURE MKF(U,V);
   MULTF(U,V);

SYMBOLIC PROCEDURE MULTD!*(U,V);
   IF ONEP U THEN V ELSE MULTD(U,V);

SYMBOLIC SMACRO PROCEDURE ADDFS(U,V);
   ADDF(U,V);

SYMBOLIC SMACRO PROCEDURE MULTFS(U,V);
   %U and V are pseudo standard forms
   %Value is pseudo standard form for U*V;
   MULTF(U,V);

FLUID '(NDIMS!*);

SYMBOLIC PROCEDURE ISIMPQ U;
   BEGIN SCALAR NDIMS!*;
      NDIMS!* := SIMP NDIM!*;
      IF DENR NDIMS!* NEQ 1
	THEN <<!*SUB2 := T;
	       NDIMS!* := MULTPF(MKSP(LIST('RECIP,DENR NDIMS!*),1),
				 NUMR NDIMS!*)>>
       ELSE NDIMS!* := NUMR NDIMS!*;
   A: U := ISIMP1(NUMR U,INDICES!*,NIL,NIL,NIL) ./ DENR U;
      IF DEFINDICES!*
	THEN <<INDICES!* := UNION(DEFINDICES!*,INDICES!*);
	       DEFINDICES!* := NIL;
	       GO TO A>>
       ELSE IF NULL !*SUB2 THEN RETURN U
       ELSE RETURN RESIMP U
   END;

SYMBOLIC PROCEDURE ISIMP1(U,I,V,W,X);
   IF NULL U THEN NIL
    ELSE IF DOMAINP U
       THEN IF X THEN MULTD(U,SPUR0(CAR X,I,V,W,CDR X))
	     ELSE IF V THEN REDERR("Unmatched index" . I)
	     ELSE IF W THEN MULTFS(EMULT W,ISIMP1(U,I,V,NIL,X))
	     ELSE U
    ELSE ADDFS(ISIMP2(CAR U,I,V,W,X),ISIMP1(CDR U,I,V,W,X));

SYMBOLIC PROCEDURE ISIMP2(U,I,V,W,X);
   BEGIN SCALAR Z;
	IF ATOM (Z := CAAR U) THEN GO TO A
	 ELSE IF CAR Z EQ 'CONS AND XNP(CDR Z,I)
	    THEN RETURN DOTSUM(U,I,V,W,X)
	 ELSE IF CAR Z EQ 'G
	  THEN GO TO B
	 ELSE IF CAR Z EQ 'EPS THEN RETURN ESUM(U,I,V,W,X);
    A:	RETURN MKPF(CAR U,ISIMP1(CDR U,I,V,W,X));
    B:	Z := GADD(APPN(CDDR Z,CDAR U),X,CADR Z);
	RETURN ISIMP1(MULTD!*(NB CAR Z,CDR U),I,V,W,CDR Z)
   END;

SYMBOLIC PROCEDURE NB U;
   IF U THEN 1 ELSE -1;

SYMBOLIC SMACRO PROCEDURE MKDOT(U,V);
   %Returns a standard form for U.V;
   MKSF('CONS . ORD2(U,V));

SYMBOLIC PROCEDURE DOTSUM(U,I,V,W,X);
   BEGIN SCALAR I1,N,U1,U2,V1,Y,Z;
	N := CDAR U;
	IF NOT (CAR (U1 := CDAAR U) MEMBER I) THEN U1 := REVERSE U1;
	U2 := CADR U1;
	U1 := CAR U1;
	V1 := CDR U;
	IF N=2 THEN GO TO H ELSE IF N NEQ 1 THEN REDERR U;
    A:	IF U1 MEMBER I THEN GO TO A1
	 ELSE IF NULL (Z := MKDOT(U1,U2)) THEN RETURN NIL
	 ELSE RETURN MKF(Z,ISIMP1(V1,I1,V,W,X));
    A1: I1 := DELETE(U1,I);
	IF U1 EQ U2 THEN RETURN MULTF(NDIMS!*,ISIMP1(V1,I1,V,W,X))
	 ELSE IF NOT (Z := ATSOC(U1,V)) THEN GO TO C
	 ELSE IF U2 MEMBER I THEN GO TO D;
	U1 := CDR Z;
	GO TO E;
    C:	IF Z := MEMLIS(U1,X)
	    THEN RETURN ISIMP1(V1,
			      I1,
			      V,
			      W,
			      SUBST(U2,U1,Z) . DELETE(Z,X))
	 ELSE IF Z := MEMLIS(U1,W)
	    THEN RETURN ESUM((('EPS . SUBST(U2,U1,Z)) . 1) . V1,
			     I1,
			     V,
			     DELETE(Z,W),
			     X)
	 ELSE IF U2 MEMBER I AND NULL Y THEN GO TO G;
	RETURN ISIMP1(V1,I,(U1 . U2) . V,W,X);
    D:	U1 := U2;
	U2 := CDR Z;
    E:	I := I1;
	V := DELETE(Z,V);
	GO TO A;
    G:	Y := T;
	Z := U1;
	U1 := U2;
	U2 := Z;
	GO TO A1;
    H:	IF U1 EQ U2 THEN REDERR U;
	I := I1 := DELETE(U1,I);
	U1 := U2;
	GO TO A
   END;

SYMBOLIC PROCEDURE MKSF U;
   %U is a kernel.
   %Value is a (possibly substituted) standard form for U;
   BEGIN SCALAR X;
	X := MKSQ(U,1);
	IF CDR X=1 THEN RETURN CAR X;
	!*SUB2 := T;
	RETURN !*P2F(U TO 1)
   END;


%*********************************************************************
%	    FUNCTIONS FOR SIMPLIFYING DIRAC GAMMA MATRICES
%********************************************************************;

SYMBOLIC PROCEDURE GADD(U,V,L);
   BEGIN SCALAR W,X; INTEGER N;
	N := 0; 		%number of gamma5 interchanges;
	IF NOT (X := ATSOC(L,V)) THEN GO TO A;
	V := DELETE(X,V);
	W := CDDR X;		%list being built;
	X := CADR X;		%true if gamma5 remains;
    A:	IF NULL U THEN RETURN ((REMAINDER(N,2)=0) . (L . X . W) . V)
	 ELSE IF CAR U EQ 'A THEN GO TO C
	 ELSE W := CAR U . W;
    B:	U := CDR U;
	GO TO A;
    C: IF NDIMS!* NEQ 4
	 THEN REDERR "Gamma5 not allowed unless vecdim is 4";
       X := NOT X;
	N := LENGTH W + N;
	GO TO B
   END;


%*********************************************************************
%	FUNCTIONS FOR COMPUTING TRACES OF DIRAC GAMMA MATRICES
%********************************************************************;

SYMBOLIC PROCEDURE SPUR0(U,I,V1,V2,V3); 
   BEGIN SCALAR L,W,I1,KAHP,N,Z; 
      L := CAR U; 
      N := 1; 
      Z := CADR U; 
      U := REVERSE CDDR U; 
      IF Z THEN U := 'A . U; %GAMMA5 REMAINS;
      IF NULL U THEN GO TO END1
       ELSE IF NULL FLAGP(L,'NOSPUR)
        THEN IF CAR U EQ 'A AND (LENGTH U<5 OR HEVENP U)
                  OR NOT CAR U EQ 'A AND NOT HEVENP U
               THEN RETURN NIL
              ELSE IF NULL I THEN <<W := REVERSE U; GO TO END1>>; 
    A: 
      IF NULL U THEN GO TO END1
       ELSE IF CAR U MEMBER I
        THEN IF CAR U MEMBER CDR U
               THEN <<IF CAR U EQ CADR U
                        THEN <<I := DELETE(CAR U,I); 
                               U := CDDR U; 
                               N := MULTF(N,NDIMS!*); 
                               GO TO A>>; 
                      KAHP := T; 
                      I1 := CAR U . I1; 
                      GO TO A1>>
              ELSE IF CAR U MEMBER I1 THEN GO TO A1
              ELSE IF Z := BASSOC(CAR U,V1)
               THEN <<V1 := DELETE(Z,V1); 
                      I := DELETE(CAR W,I); 
                      U := OTHER(CAR U,Z) . CDR U; 
                      GO TO A>>
              ELSE IF Z := MEMLIS(CAR U,V2)
               THEN RETURN IF FLAGP(L,'NOSPUR)
                                AND NULL V1
                                AND NULL V3
                                AND NULL CDR V2
                             THEN MKF(MKGF(APPEND(REVERSE W,U),L),
                                      MULTFS(N,MKEPSF Z))
                            ELSE MULTD!*(N,
                                         ISIMP1(SPUR0(
           L . (NIL . APPEND(REVERSE U,W)),NIL,V1,DELETE(Z,V2),V3),
						I,NIL,LIST Z,NIL))
              ELSE IF Z := MEMLIS(CAR U,V3)
               THEN IF NDIMS!*=4
		      THEN RETURN SPUR0I(U,DELETE(CAR U,I),V1,V2,
					 DELETE(Z,V3),L,N,W,Z)
                     ELSE <<INDICES!* := DELETE(CAR U,INDICES!*); 
                            I := DELETE(CAR U,I); 
                            IF NOT CAR U MEMQ DEFINDICES!*
                              THEN DEFINDICES!* := 
                                    CAR U . DEFINDICES!*; 
                            GO TO A1>>
	      ELSE REDERR LIST("Unmatched index",CAR U);
    A1: 
      W := CAR U . W; 
      U := CDR U; 
      GO TO A; 
    END1: 
      IF KAHP
        THEN IF NDIMS!*=4
               THEN <<Z := MULTFS(N,KAHANE(REVERSE W,I1,L)); 
                      RETURN ISIMP1(Z,SETDIFF(I,I1),V1,V2,V3)>>
              ELSE Z := SPURDIM(W,I,L,NIL,1)
       ELSE Z := SPURR(W,L,NIL,1); 
      RETURN IF NULL Z THEN NIL
              ELSE IF GET('EPS,'KLIST) AND NOT FLAGP(L,'NOSPUR)
               THEN ISIMP1(MULTFS(N,Z),I,V1,V2,V3)
              ELSE MULTFS(Z,ISIMP1(N,I,V1,V2,V3))
   END;

SYMBOLIC PROCEDURE SPUR0I(U,I,V1,V2,V3,L,N,W,Z); 
   BEGIN SCALAR KAHP,I1; 
      IF FLAGP(L,'NOSPUR) AND FLAGP(CAR Z,'NOSPUR)
	THEN ERRACH "This NOSPUR option not implemented"
       ELSE IF FLAGP(CAR Z,'NOSPUR) THEN KAHP := CAR Z; 
      Z := CDR Z; 
      I1 := CAR Z; 
      Z := REVERSE CDR Z; 
      IF I1 THEN Z := 'A . Z; 
      I1 := NIL; 
      <<WHILE NULL (CAR U EQ CAR Z) DO 
           <<I1 := CAR Z . I1; Z := CDR Z>>; 
        Z := CDR Z; 
        U := CDR U; 
        IF FLAGP(L,'NOSPUR)
          THEN <<W := W . (U . (I1 . Z)); 
                 I1 := CAR W; 
                 Z := CADR W; 
                 U := CADDR W; 
                 W := CDDDR W>>; 
        W := REVERSE W; 
        IF NULL ((NULL U OR NOT EQCAR(W,'A)) AND (U := APPEND(U,W)))
          THEN <<IF NOT HEVENP U THEN N :=  - N; 
                 U := 'A . APPEND(U,CDR W)>>; 
        IF KAHP THEN L := KAHP; 
        Z := 
         MKF(MKG(REVERSE I1,L),
             MULTF(BRACE(U,L,I),MULTFS(N,MKG1(Z,L)))); 
        Z := ISIMP1(Z,I,V1,V2,V3); 
        IF NULL Z OR (Z := QUOTF(Z,2)) THEN RETURN Z
         ELSE ERRACH LIST('SPUR0,N,I,V1,V2,V3)>>
   END;

SYMBOLIC PROCEDURE SPURDIM(U,I,L,V,N);
   BEGIN SCALAR W,X,Y,Z,Z1; INTEGER M;
    A:	IF NULL U
	  THEN RETURN IF NULL V THEN N
		ELSE IF FLAGP(L,'NOSPUR) THEN MULTFS(N,MKGF(V,L))
		ELSE MULTFS(N,SPRGEN V)
	 ELSE IF NOT(CAR U MEMQ CDR U)
	  THEN <<V := CAR U . V; U := CDR U; GO TO A>>;
	X := CAR U;
	Y := CDR U;
	W := Y;
	M := 1;
    B:	IF X MEMQ I THEN GO TO D
	 ELSE IF NOT X EQ CAR W THEN GO TO C
	 ELSE IF NULL(W := MKDOT(X,X)) THEN RETURN Z;
	IF X MEMQ I THEN W := NDIMS!*;
	RETURN ADDFS(MKF(W,SPURDIM(DELETE(X,Y),I,L,V,N)),Z);
    C:	Z1 := MKDOT(X,CAR W);
	IF CAR W MEMQ I
	  THEN Z := ADDFS(SPURDIM(SUBST(X,CAR W,REMOVE(Y,M)),
				  I,L,V,2*N),Z)
	 ELSE IF Z1
	  THEN Z := ADDFS(MKF(Z1,SPURDIM(REMOVE(Y,M),I,L,V,2*N)),Z);
	W := CDR W;
	N := -N;
	M := M+1;
	GO TO B;
   D:	WHILE NOT(X EQ CAR W) DO
	 <<Z:= ADDFS(SPURDIM(SUBST(CAR W,X,REMOVE(Y,M)),I,L,V,2*N),Z);
	   W := CDR W;
	   N := -N;
	   M := M+1>>;
	RETURN ADDFS(MKF(NDIMS!*,SPURDIM(DELETE(X,Y),I,L,V,N)),Z)
   END;

SYMBOLIC PROCEDURE APPN(U,N);
   IF N=1 THEN U ELSE APPEND(U,APPN(U,N-1));

SYMBOLIC PROCEDURE OTHER(U,V);
   IF U EQ CAR V THEN CDR V ELSE CAR V;

SYMBOLIC PROCEDURE KAHANE(U,I,L);
   %The Kahane algorithm for Dirac matrix string reduction
   %Ref: Kahane, J., Journ. Math. Phys. 9 (1968) 1732-1738;
   BEGIN SCALAR P,R,V,W,X,Y,Z; INTEGER K,M;
	K := 0;
    MARK:
	IF EQCAR(U,'A) THEN GO TO A1;
    A:	P := NOT P;		%vector parity;
	IF NULL U THEN GO TO D ELSE IF CAR U MEMBER I THEN GO TO C;
    A1: W := ACONC(W,CAR U);
    B:	U := CDR U;
	GO TO A;
    C:	Y := CAR U . P;
	Z := (X . (Y . W)) . Z;
	X := Y;
	W := NIL;
	K := K+1;
	GO TO B;
    D:	Z := (NIL . (X . W)) . Z;
	%BEWARE ... END OF STRING HAS OPPOSITE CONVENTION;
    PASS2:
	M := 1;
    L1: IF NULL Z THEN GO TO L9;
	U := CAAR Z;
	X := CADAR Z;
	W := CDDAR Z;
	Z := CDR Z;
	M := M+1;
	IF NULL U THEN GO TO L2
	 ELSE IF (CAR U EQ CAR X) AND EXC(X,CDR U) THEN GO TO L7;
	W := REVERSE W;
	R := T;
    L2: P := NOT EXC(X,R);
	X := CAR X;
	Y := NIL;
    L3: IF NULL Z
	  THEN REDERR("Unmatched index" .
	         IF Y THEN IF NOT ATOM CADAR Y THEN CADAR Y
			    ELSE IF NOT ATOM CAAR Y THEN CAAR Y
		  ELSE NIL
		ELSE NIL)
	  ELSE IF (X EQ CAR (I := CADAR Z)) AND NOT EXC(I,P)
	   THEN GO TO L5
	  ELSE IF (X EQ CAR (I := CAAR Z)) AND EXC(I,P) THEN GO TO L4;
	Y := CAR Z . Y;
	Z := CDR Z;
	GO TO L3;
    L4: X := CADAR Z;
	W := APPR(CDDAR Z,W);
	R := T;
	GO TO L6;
    L5: X := CAAR Z;
	W := APPEND(CDDAR Z,W);
	R := NIL;
    L6: Z := APPR(Y,CDR Z);
	IF NULL X THEN GO TO L8
	 ELSE IF NOT EQCAR(U,CAR X) THEN GO TO L2;
    L7: IF W AND CDR U THEN W := ACONC(CDR W,CAR W);
	V := MULTFS(BRACE(W,L,NIL),V);	%V := ('BRACE . L . W) . V;
	GO TO L1;
    L8: V := MKG(W,L);			%V := LIST('G . L . W);
	Z := REVERSE Z;
	K := K/2;
	GO TO L1;
    L9: U := 2**K;
	IF NOT (REMAINDER(K-M,2) = 0) THEN U :=  - U;
	RETURN MULTD!*(U,V)		%RETURN 'TIMES . U . V;
   END;

SYMBOLIC PROCEDURE APPR(U,V);
   IF NULL U THEN V ELSE APPR(CDR U,CAR U . V);

SYMBOLIC PROCEDURE EXC(U,V);
   IF NULL CDR U THEN V ELSE NOT V;

SYMBOLIC PROCEDURE BRACE(U,L,I);
   IF NULL U THEN 2
    ELSE IF XNP(I,U) OR FLAGP(L,'NOSPUR)
     THEN ADDF(MKG1(U,L),MKG1(REVERSE U,L))
    ELSE IF CAR U EQ 'A
       THEN IF HEVENP U THEN ADDFS(MKG(U,L),
				 NEGF MKG('A . REVERSE CDR U,L))
	     ELSE MKF(MKA L,SPR2(CDR U,L,2,NIL))
    ELSE IF HEVENP U THEN SPR2(U,L,2,NIL)
    ELSE SPR1(U,L,2,NIL);

SYMBOLIC PROCEDURE SPR1(U,L,N,B);
   IF NULL U THEN NIL
    ELSE IF NULL CDR U THEN MULTD!*(N,MKG1(U,L))
    ELSE BEGIN SCALAR M,X,Z;
	       X := U;
	       M := 1;
	  A:   IF NULL X THEN RETURN Z;
	       Z:= ADDFS(MKF(MKG1(LIST CAR X,L),
			      IF NULL B THEN SPURR(REMOVE(U,M),L,NIL,N)
			       ELSE SPR1(REMOVE(U,M),L,N,NIL)),
			 Z);
	       X := CDR X;
	       N :=  - N;
	       M := M+1;
	       GO TO A
    END;

SYMBOLIC PROCEDURE SPR2(U,L,N,B);
   IF NULL CDDR U AND NULL B THEN MULTD!*(N,MKDOT(CAR U,CADR U))
    ELSE (LAMBDA X; IF B THEN ADDFS(SPR1(U,L,N,B),X) ELSE X)
       ADDFS(SPURR(U,L,NIL,N),
	     MKF(MKA L,SPURR(APPEND(U,LIST 'A),L,NIL,N)));

SYMBOLIC PROCEDURE HEVENP U;
   NULL U OR NOT HEVENP CDR U;

SYMBOLIC PROCEDURE BASSOC(U,V);
   IF NULL V THEN NIL
    ELSE IF U EQ CAAR V OR U EQ CDAR V THEN CAR V
    ELSE BASSOC(U,CDR V);

SYMBOLIC PROCEDURE MEMLIS(U,V);
   IF NULL V THEN NIL
    ELSE IF U MEMBER CAR V THEN CAR V
    ELSE MEMLIS(U,CDR V);

SYMBOLIC PROCEDURE SPURR(U,L,V,N);
   BEGIN SCALAR W,X,Y,Z,Z1; INTEGER M;
    A:	IF NULL U THEN GO TO B
	 ELSE IF CAR U MEMBER CDR U THEN GO TO G;
	V := CAR U . V;
	U := CDR U;
	GO TO A;
    B:	RETURN IF NULL V THEN N
	 ELSE IF FLAGP(L,'NOSPUR) THEN MULTD!*(N,MKGF(V,L))
	 ELSE MULTD!*(N,SPRGEN V);
    G:	X := CAR U;
	Y := CDR U;
	W := Y;
	M := 1;
    H:	IF NOT X EQ CAR W THEN GO TO H1
	 ELSE IF NULL(W:= MKDOT(X,X)) THEN RETURN Z
	 ELSE RETURN ADDFS(MKF(W,SPURR(DELETE(X,Y),L,V,N)),Z);
    H1: Z1 := MKDOT(X,CAR W);
	IF Z1 THEN Z:= ADDFS(MKF(Z1,SPURR(REMOVE(Y,M),L,V,2*N)),Z);
	W := CDR W;
	N :=  - N;
	M := M+1;
	GO TO H
   END;

SYMBOLIC PROCEDURE SPRGEN V;
   BEGIN SCALAR X,Y,Z;
	IF NOT (CAR V EQ 'A) THEN RETURN SPRGEN1(V,T)
	 ELSE IF NULL (X := COMB(V := CDR V,4)) THEN RETURN NIL
	 ELSE IF NULL CDR X THEN GO TO E;
    C:	IF NULL X THEN RETURN MULTPF('I TO 1,Z);
	Y := MKEPSF CAR X;
	IF ASIGN(CAR X,V,1)=-1 THEN Y := NEGF Y;
	Z := ADDF(MULTF(Y,SPRGEN1(SETDIFF(V,CAR X),T)),Z);
    D:	X := CDR X;
	GO TO C;
    E:	Z := MKEPSF CAR X;
	GO TO D
   END;

SYMBOLIC PROCEDURE ASIGN(U,V,N);
   IF NULL U THEN N ELSE ASIGN(CDR U,V,ASIGN1(CAR U,V,-1)*N);

SYMBOLIC PROCEDURE ASIGN1(U,V,N);
   IF U EQ CAR V THEN N ELSE ASIGN1(U,CDR V,-N);

SYMBOLIC PROCEDURE SPRGEN1(U,B);
   IF NULL U THEN NIL
    ELSE IF NULL CDDR U THEN (LAMBDA X; IF B THEN X ELSE NEGF X)
				MKDOT(CAR U,CADR U)
    ELSE BEGIN SCALAR W,X,Y,Z;
	       X := CAR U;
	       U := CDR U;
	       Y := U;
	  A:   IF NULL U THEN RETURN Z
		ELSE IF NULL(W:= MKDOT(X,CAR U)) THEN GO TO C;
	       Z := ADDF(MULTF(W,SPRGEN1(DELETE(CAR U,Y),B)),Z);
	  C:   B := NOT B;
	       U := CDR U;
	       GO TO A
    END;

%*********************************************************************
%		     FUNCTIONS FOR EPSILON ALGEBRA
%********************************************************************;


PUT('EPS,'SIMPFN,'SIMPEPS);

SYMBOLIC PROCEDURE MKEPSF U;
   (LAMBDA X; (LAMBDA Y; IF NULL CAR X THEN NEGF Y ELSE Y) MKSF CDR X)
	MKEPSK U;

SYMBOLIC PROCEDURE ESUM(U,I,V,W,X);
   BEGIN SCALAR Y,Z,Z1;
	Z := CAR U;
	U := CDR U;
	IF CDR Z NEQ 1
	 THEN U := MULTF(EXPTF(MKEPSF CDAR Z,CDR Z-1),U);
	Z := CDAR Z;
    A:	IF REPEATS Z THEN RETURN;
    B:	IF NULL Z THEN RETURN ISIMP1(U,I,V,REVERSE Y . W,X)
	 ELSE IF NOT (CAR Z MEMBER I) THEN GO TO D
	 ELSE IF NOT (Z1 := BASSOC(CAR Z,V)) THEN GO TO C;
	V := DELETE(Z1,V);
	I := DELETE(CAR Z,I);
	Z := APPEND(REVERSE Y,OTHER(CAR Z,Z1) . CDR Z);
	Y := NIL;
	GO TO A;
    C:	IF Z1 := MEMLIS(CAR Z,W) THEN GO TO C1
	 ELSE RETURN ISIMP1(U,I,V,APPEND(REVERSE Y,Z) . W,X);
    C1: Z := APPEND(REVERSE Y,Z);
	Y := XN(I,XN(Z,Z1));
	RETURN ISIMP1(MULTFS(EMULT1(Z1,Z,Y),U),
		      SETDIFF(I,Y),
		      V,
		      DELETE(Z1,W),
		      X);
    D:	Y := CAR Z . Y;
	Z := CDR Z;
	GO TO B
   END;

SYMBOLIC PROCEDURE EMULT U;
   IF NULL CDR U THEN MKEPSF CAR U
    ELSE IF NULL CDDR U THEN EMULT1(CAR U,CADR U,NIL)
    ELSE MULTFS(EMULT1(CAR U,CADR U,NIL),EMULT CDDR U);

SYMBOLIC PROCEDURE EMULT1(U,V,I);
   (LAMBDA (X,Y);
	 (LAMBDA (M,N);
	       IF M=4 THEN 24*N
		ELSE IF M=3 THEN MULTD(6*N,MKDOT(CAR X,CAR Y))
		ELSE MULTD!*(N*(IF M = 0 THEN 1 ELSE M),
			   CAR DETQ MAPLIST(X,
			     FUNCTION (LAMBDA K;
			       MAPLIST(Y,
				 FUNCTION (LAMBDA J;
				   MKDOT(CAR K,CAR J) . 1))))))
	    (LENGTH I,
	     (LAMBDA J; NB IF PERMP(U,APPEND(I,X)) THEN NOT J ELSE J)
		PERMP(V,APPEND(I,Y))))
      (SETDIFF(U,I),SETDIFF(V,I));


END;
