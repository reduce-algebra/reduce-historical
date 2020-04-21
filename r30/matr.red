%*********************************************************************
%*********************************************************************
%                           MATRIX PACKAGE
%*********************************************************************
%********************************************************************;

%Copyright (c) 1983 The Rand Corporation;

SYMBOLIC;

%*********************************************************************
%     REQUIRES SIMPLIFICATION FUNCTIONS FOR NON-SCALAR QUANTITIES
%********************************************************************;

FLUID '(!*EXP !*S!*);   %Used in this module;

GLOBAL '(SUBFG!* !*SUB2 !*NAT);

SYMBOLIC PROCEDURE MATSM!* U;
   %matrix expression simplification function;
   BEGIN
	U := MATSM U;
	U := IF NULL CDR U AND NULL CDAR U THEN MK!*SQ2 CAAR U
		ELSE 'MAT . MAPC2(U,FUNCTION MK!*SQ2);
	!*SUB2 := NIL;	 %since all substitutions done;
	RETURN U
   END;

SYMBOLIC PROCEDURE MAPC2(U,V);
   %this very conservative definition is to allow for systems with
   %poor handling of functional arguments, and because of bootstrap-
   %ping difficulties, which are no longer really relevant;
   BEGIN SCALAR X,Y,Z;
   A: IF NULL U THEN RETURN REVERSIP Z;
      X := CAR U;
      Y := NIL;
   B: IF NULL X THEN GO TO C;
      Y := APPLY(V,LIST CAR X) . Y;
      X := CDR X;
      GO TO B;
   C: U := CDR U;
      Z := REVERSIP Y . Z:
      GO TO A
   END;

SYMBOLIC PROCEDURE MK!*SQ2 U;
   BEGIN SCALAR X;
	X := !*SUB2;   %since we need value for each element;
	U := SUBS2 U;
	!*SUB2 := X;
	RETURN MK!*SQ U
   END;

SYMBOLIC PROCEDURE MATSM U;
   BEGIN SCALAR X,Y;
	U := NSSIMP(U,'MATP);
    A:	IF NULL U THEN RETURN X;
	Y := MULTSM(CAAR U,MATRIXTIMES CDAR U);
	X := IF NULL X THEN Y ELSE ADDM(X,Y);
	U := CDR U;
	GO TO A
   END;

SYMBOLIC PROCEDURE MATRIXTIMES U;
   %returns matrix canonical form for matrix symbol product U;
   BEGIN SCALAR X,Y,Z; INTEGER N;
    A:	IF NULL U THEN RETURN Z
	 ELSE IF EQCAR(CAR U,'!*DIV) THEN GO TO D
	 ELSE IF ATOM CAR U THEN GO TO ER
	 ELSE IF CAAR U EQ 'MAT THEN GO TO C1
	 ELSE IF (X := GET(CAAR U,'MSIMPFN))
	  THEN X := APPLY(X,CDAR U)
	 ELSE GO TO ER;
    B:	Z := IF NULL Z THEN X
	      ELSE IF NULL CDR Z AND NULL CDAR Z THEN MULTSM(CAAR Z,X)
	      ELSE MULTM(X,Z);
    C:	U := CDR U;
	GO TO A;
    C1: IF NOT LCHK CDAR U THEN REDERR "MATRIX MISMATCH";
	X := MAPC2(CDAR U,FUNCTION XSIMP);
	GO TO B;
    D:	Y := MATSM CADAR U;
	IF (N := LENGTH CAR Y) NEQ LENGTH Y
	  THEN REDERR "NON SQUARE MATRIX"
	 ELSE IF (Z AND N NEQ LENGTH Z) THEN REDERR "MATRIX MISMATCH"
	 ELSE IF CDDAR U THEN GO TO H
	 ELSE IF NULL CDR Y AND NULL CDAR Y THEN GO TO E;
	X := SUBFG!*;
	SUBFG!* := NIL;
	IF NULL Z THEN Z:= GENERATEIDENT N;
	Z := LNRSOLVE(Y,Z);
	SUBFG!* := X;
	GO TO C;
    E:	IF NULL CAAAR Y THEN REDERR "ZERO DENOMINATOR";
	Y := REVPR CAAR Y;
	Z := IF NULL Z THEN LIST LIST Y ELSE MULTSM(Y,Z);
	GO TO C;
     H: IF NULL Z THEN Z := GENERATEIDENT N;
	GO  TO C;
    ER: REDERR LIST('MATRIX,CAR U,"NOT SET")
   END;

SYMBOLIC PROCEDURE LCHK U;
   BEGIN INTEGER N;
	IF NULL U OR ATOM CAR U THEN RETURN NIL;
	N := LENGTH CAR U;
	REPEAT U := CDR U
	   UNTIL NULL U OR ATOM CAR U OR LENGTH CAR U NEQ N;
	RETURN NULL U
   END;

SYMBOLIC PROCEDURE ADDM(U,V);
   %returns sum of two matrix canonical forms U and V;
   FOR EACH J IN ADDM1(U,V,FUNCTION CONS)
      COLLECT ADDM1(CAR J,CDR J,FUNCTION ADDSQ);

SYMBOLIC PROCEDURE ADDM1(U,V,W);
   IF NULL U AND NULL V THEN NIL
    ELSE IF NULL U OR NULL V THEN REDERR "MATRIX MISMATCH"
    ELSE APPLY(W,LIST(CAR U,CAR V)) . ADDM1(CDR U,CDR V,W);

SYMBOLIC PROCEDURE TP U; TP1 MATSM U;

SYMBOLIC PROCEDURE TP1 U;
   %returns transpose of the matrix canonical form U;
   %U is destroyed in the process;
   BEGIN SCALAR V,W,X,Y,Z;
	V := W := LIST NIL;
	WHILE CAR U DO
	 <<X := U;
	   Y := Z := LIST NIL;
	   WHILE X DO
	     <<Z := CDR RPLACD(Z,LIST CAAR X);
	       X := CDR RPLACA(X,CDAR X)>>;
	   W := CDR RPLACD(W,LIST CDR Y)>>;
	RETURN CDR V
   END;

SYMBOLIC PROCEDURE SCALPROD(U,V);
   %returns scalar product of two lists (vectors) U and V;
   IF NULL U AND NULL V THEN NIL ./ 1
    ELSE IF NULL U OR NULL V THEN REDERR "MATRIX MISMATCH"
    ELSE ADDSQ(MULTSQ(CAR U,CAR V),SCALPROD(CDR U,CDR V));

SYMBOLIC PROCEDURE MULTM(U,V);
   %returns matrix product of two matrix canonical forms U and V;
    (LAMBDA X;
	FOR EACH Y IN U COLLECT FOR EACH K IN X COLLECT SCALPROD(Y,K))
     TP1 V;

SYMBOLIC PROCEDURE MULTSM(!*S!*,U);
   %returns product of standard quotient !*S!* and matrix standard
   %form U;
   IF !*S!* = (1 ./ 1) THEN U
    ELSE MAPC2(U,FUNCTION (LAMBDA J; MULTSQ(!*S!*,J)));

SYMBOLIC PROCEDURE LETMTR(U,V,Y);
   %substitution for matrix elements;
   BEGIN SCALAR Z;
	IF NOT EQCAR(Y,'MAT) THEN REDERR LIST('MATRIX,CAR U,"NOT SET")
	 ELSE IF NOT NUMLIS (Z := REVLIS CDR U) OR LENGTH Z NEQ 2
	  THEN RETURN ERRPRI2(U,'HOLD);
	RPLACA(PNTH(NTH(CDR Y,CAR Z),CADR Z),V);
   END;

SYMBOLIC PROCEDURE MATPRI!*(U,V,W);
   %symbolic interface to VARPRI;
   MATPRI(CDR U,IF V THEN EVAL CAR V ELSE NIL);

SYMBOLIC PROCEDURE MATPRI(U,X);
   %prints a matrix canonical form U with name X;
   BEGIN SCALAR M,N;
	M := 1;
	IF NULL X THEN X := 'MAT;
	FOR EACH Y IN U DO
	 <<N := 1;
	   FOR EACH Z IN Y DO
	    <<VARPRI(Z,LIST MKQUOTE LIST(X,M,N),T);
	      IF !*NAT THEN TERPRI!* T;
	      N := N+1>>;
	M := M+1>>
   END;


%*********************************************************************
%		       MATRIX INVERSION ROUTINES
%********************************************************************;

SYMBOLIC PROCEDURE LNRSOLVE(U,V);
   %U is a matrix standard form, V a compatible matrix form;
   %Value is U**(-1)*V;
   BEGIN INTEGER N; SCALAR X,!*S!*;
	X := !*EXP; !*EXP := T; N := LENGTH U;
	!*S!* := BACKSUB(BAREISS CAR NORMMAT AUGMENT(U,V),N);
	U := MAPC2(RHSIDE(CAR !*S!*,N),
		FUNCTION (LAMBDA J; CANCEL(J . CDR !*S!*)));
	!*EXP := X;
	RETURN U
   END;

SYMBOLIC PROCEDURE AUGMENT(U,V);
   IF NULL U THEN NIL ELSE APPEND(CAR U,CAR V) . AUGMENT(CDR U,CDR V);

SYMBOLIC PROCEDURE GENERATEIDENT N;
  %returns matrix canonical form of identity matrix of order N;
   BEGIN SCALAR U,V;
	FOR I := 1:N DO
	 <<U := NIL;
	   FOR J := 1:N DO U := ((IF I=J THEN 1 ELSE NIL) . 1) . U;
	   V := U . V>>;
	RETURN V
   END;

SYMBOLIC PROCEDURE RHSIDE(U,M);
   IF NULL U THEN NIL ELSE PNTH(CAR U,M+1) . RHSIDE(CDR U,M);

SYMBOLIC PROCEDURE BAREISS U;
  %The 2-step integer preserving elimination method of Bareiss
  %based on the implementation of Lipson;
  %If the value of procedure is NIL then U is singular, otherwise the
  %value is the triangularized form of U (in matrix polynomial form);
  BEGIN SCALAR AA,C0,CI1,CI2,IK1,IJ,KK1,KJ,K1J,K1K1,UI,U1,X;
	INTEGER K,K1;
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
	AA:= 1;
	K:= 2;
	K1:=1;
	U1:=U;
	GO TO PIVOT;
   AGN: U1 := CDR U1;
	IF NULL CDR U1 OR NULL CDDR U1 THEN RETURN U;
	AA:=NTH(CAR U1,K);		%AA := U(K,K);
	K:=K+2;
	K1:=K-1;
	U1:=CDR U1;
   PIVOT:  %pivot algorithm;
	K1J:= K1K1 := PNTH(CAR U1,K1);
	IF CAR K1K1 THEN GO TO L2;
	UI:= CDR U1;			%I := K;
   L:	IF NULL UI THEN RETURN NIL
	 ELSE IF NULL CAR(IJ := PNTH(CAR UI,K1))
	  THEN GO TO L1;
   L0:	IF NULL IJ THEN GO TO L2;
	X:= CAR IJ;
	RPLACA(IJ,NEGF CAR K1J);
	RPLACA(K1J,X);
	IJ:= CDR IJ;
	K1J:= CDR K1J;
	GO TO L0;
   L1:	UI:= CDR UI;
	GO TO L;
   L2:	UI:= CDR U1;			%I:= K;
   L21: IF NULL UI THEN RETURN; %IF I>M THEN RETURN;
	IJ:= PNTH(CAR UI,K1);
	C0:= ADDF(MULTF(CAR K1K1,CADR IJ),
		    MULTF(CADR K1K1,NEGF CAR IJ));
	IF C0 THEN GO TO L3;
	UI:= CDR UI;			%I:= I+1;
	GO TO L21;
   L3:	C0:= QUOTF!*(C0,AA);
	KK1 := KJ := PNTH(CADR U1,K1);	%KK1 := U(K,K-1);
	IF CDR U1 AND NULL CDDR U1 THEN GO TO EV0
	 ELSE IF UI EQ CDR U1 THEN GO TO COMP;
   L31: IF NULL IJ THEN GO TO COMP;	%IF I>N THEN GO TO COMP;
	X:= CAR IJ;
	RPLACA(IJ,NEGF CAR KJ);
	RPLACA(KJ,X);
	IJ:= CDR IJ;
	KJ:= CDR KJ;
	GO TO L31;
	%pivoting complete;
    COMP:
	IF NULL CDR U1 THEN GO TO EV;
	UI:= CDDR U1;			%I:= K+1;
    COMP1:
	IF NULL UI THEN GO TO EV;	%IF I>M THEN GO TO EV;
	IK1:= PNTH(CAR UI,K1);
	CI1:= QUOTF!*(ADDF(MULTF(CADR K1K1,CAR IK1),
			   MULTF(CAR K1K1,NEGF CADR IK1)),
		     AA);
	CI2:= QUOTF!*(ADDF(MULTF(CAR KK1,CADR IK1),
			   MULTF(CADR KK1,NEGF CAR IK1)),
		     AA);
	IF NULL CDDR K1K1 THEN GO TO COMP3;%IF J>N THEN GO TO COMP3;
	IJ:= CDDR IK1;			%J:= K+1;
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
	X := CDDR K1K1; 		%X := U(K-1,K+1);
	RPLACA(KJ,C0);
    EV1:KJ:= CDR KJ;
	IF NULL KJ THEN GO TO AGN;
	RPLACA(KJ,QUOTF!*(ADDF(MULTF(CAR K1K1,CAR KJ),
			       MULTF(CAR KK1,NEGF CAR X)),
		     AA));
	X := CDR X;
	GO TO EV1
   END;

SYMBOLIC PROCEDURE BACKSUB(U,M);
   BEGIN SCALAR DETM,DET1,IJ,IJJ,RI,SUMM,UJ,UR; INTEGER I,JJ;
   %N in comments is number of columns in U;
	IF NULL U THEN REDERR "SINGULAR MATRIX";
	UR := REVERSE U;
	DETM := CAR PNTH(CAR UR,M);		%DETM := U(I,J);
	IF NULL DETM THEN REDERR "SINGULAR MATRIX";
	I := M;
    ROWS:
	I := I-1;
	UR := CDR UR;
	IF NULL UR THEN RETURN U . DETM;
		%IF I=0 THEN RETURN U . DETM;
	RI := CAR UR;
	JJ := M+1;
	IJJ:=PNTH(RI,JJ);
    R2: IF NULL IJJ THEN GO TO ROWS;	%IF JJ>N THEN GO TO ROWS;
	IJ := PNTH(RI,I);		%J := I;
	DET1 := CAR IJ; 		%DET1 := U(I,I);
	UJ := PNTH(U,I);
	SUMM := NIL;			%SUMM := 0;
    R3: UJ := CDR UJ;			%J := J+1;
	IF NULL UJ THEN GO TO R4;	%IF J>M THEN GO TO R4;
	IJ := CDR IJ;
	SUMM := ADDF(SUMM,MULTF(CAR IJ,NTH(CAR UJ,JJ)));
		%SUMM:=SUMM+U(I,J)*U(J,JJ);
	GO TO R3;
    R4: RPLACA(IJJ,QUOTF!*(ADDF(MULTF(DETM,CAR IJJ),NEGF SUMM),DET1));
		%U(I,J):=(DETM*U(I,J)-SUMM)/DET1;
	JJ := JJ+1;
	IJJ := CDR IJJ;
	GO TO R2
   END;

SYMBOLIC PROCEDURE NORMMAT U; 
   %U is a matrix standard form.
   %Value is dotted pair of matrix polynomial form and factor;
   BEGIN SCALAR X,Y,Z; 
      X := 1; 
      FOR EACH V IN U DO
         <<Y := 1; 
           FOR EACH W IN V DO Y := LCM(Y,DENR W);
           Z := (FOR EACH W IN V
		    COLLECT MULTF(NUMR W,QUOTF(Y,DENR W)))
              . Z; 
           X := MULTF(Y,X)>>; 
      RETURN REVERSE Z . X
   END;


%*********************************************************************
%		    DETERMINANT AND TRACE ROUTINES
%********************************************************************;

SYMBOLIC PROCEDURE SIMPDET U;
   DETQ MATSM CARX(U,'DET);

COMMENT The hashing and determinant routines below
	are due to M. L. Griss;

COMMENT Some general purpose hashing functions;

FLAG('(ARRAY),'EVAL);      %declared again for bootstrapping purposes;

ARRAY !$HASH 64;  %general array for hashing;

SYMBOLIC PROCEDURE GETHASH KEY;
   %access previously saved element;
   ASSOC(KEY,!$HASH(REMAINDER(KEY,64)));

SYMBOLIC PROCEDURE PUTHASH(KEY,VALU);
   BEGIN INTEGER K; SCALAR BUK;
      K := REMAINDER(KEY,64);
      BUK := (KEY . VALU) . !$HASH K;
      !$HASH K := BUK;
      RETURN CAR BUK
   END;

SYMBOLIC PROCEDURE CLRHASH;
   FOR I := 0:64 DO !$HASH I := NIL;

COMMENT Determinant Routines;

SYMBOLIC PROCEDURE DETQ U;
   %top level determinant function;
   BEGIN INTEGER LEN;
      LEN := LENGTH U;	 %number of rows;
      FOR EACH X IN U DO
	IF LENGTH X NEQ LEN THEN REDERR "NON SQUARE MATRIX";
      IF LEN=1 THEN RETURN CAAR U;
      CLRHASH();
      U := DETQ1(U,LEN,0);
      CLRHASH();
      RETURN U
   END;

SYMBOLIC PROCEDURE DETQ1(U,LEN,IGNNUM);
   %U is a square matrix of order LEN. Value is the determinant of U;
   %Algorithm is expansion by minors of first row;
   %IGNNUM is packed set of column indices to avoid;
   BEGIN INTEGER N2; SCALAR ROW,SIGN,Z;
      ROW := CAR U;   %current row;
      N2 := 1;
      IF LEN=1
	THEN RETURN <<WHILE TWOMEM(N2,IGNNUM)
			 DO <<N2 := 2*N2; ROW := CDR ROW>>;
		      CAR ROW>>;   %last row, single element;
      IF Z := GETHASH IGNNUM THEN RETURN CDR Z;
      LEN := LEN-1;
      U := CDR U;
      Z := NIL ./ 1;
      FOR EACH X IN ROW DO
	<<IF NOT TWOMEM(N2,IGNNUM)
	    THEN <<IF NUMR X
		     THEN <<IF SIGN THEN X := NEGSQ X;
			    Z:= ADDSQ(MULTSQ(X,DETQ1(U,LEN,N2+IGNNUM)),
					Z)>>;
		   SIGN := NOT SIGN>>;
	  N2 := 2*N2>>;
      PUTHASH(IGNNUM,Z);
      RETURN Z
   END;

SYMBOLIC PROCEDURE TWOMEM(N1,N2);
   %for efficiency reasons, this procedure should be coded in assembly
   %language;
   REMAINDER(N2/N1,2)=1;

PUT('DET,'SIMPFN,'SIMPDET);

SYMBOLIC PROCEDURE SIMPTRACE U;
   BEGIN INTEGER N; SCALAR Z;
	U := MATSM CARX(U,'TRACE);
	IF LENGTH U NEQ LENGTH CAR U THEN REDERR "NON SQUARE MATRIX";
	Z := NIL ./ 1;
	N := 1;
    A:	IF NULL U THEN RETURN Z;
	Z := ADDSQ(NTH(CAR U,N),Z);
	U := CDR U;
	N := N+1;
	GO TO A
   END;

PUT('TRACE,'SIMPFN,'SIMPTRACE);


END;
