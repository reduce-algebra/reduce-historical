%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simple POLY, RAT AND ALG system, based on POLY by Fitch and Marti. 

% Edit by Cris Perdue, 28 Jan 1983 2045-PST
% "Dipthong" -> "Diphthong", order of revision history reversed
% Modified by GRISS, JUly 1982 for PSL
% MORRISON again, March 1981.
% Parses INFIX expressions to PREFIX, then SIMPlifies and PRINTs
% Handles also PREFIX expressions
% Parser modified by OTTENHEIMER
% February 1981, to be left associative March 1981.
% Further modified by MORRISON
% October 1980.
% Modifed by GRISS and GALWAY
% September 1980. 

% RUNNING: After loading POLY.RED, run function ALGG();
%   This accepts a sequence of expressions:
%	 <exp> ;	 (Semicolon terminator)
%	 <exp> ::= <term> [+ <exp>  | - <exp>]
%	 <term> ::= <primary> [* <term> | / <term>]
%	 <primary> ::= <primary0> [^ <primary0> | ' <primary0> ]
%		 ^ is exponentiation, ' is derivative
%	 <primary0> ::= <number> | <variable> | ( <exp> )

% PREFIX Format:	<number> | <id> | (op arg1 arg2)
%		+ -> PLUS2
%		- -> DIFFERENCE (or MINUS)
%		* -> TIMES2
%		/ -> QUOTIENT
%		^ -> EXPT
%		' -> DIFF

% Canonical Formats: Polynomial: integer | (term . polynomial)
%                    term      : (power . polynomial)
%                    power     : (variable . integer)
%                    Rational  : (polynomial .  polynomial)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%******************** Selectors and Constructors **********************

smacro procedure RATNUM X; % parts of Rational
 CAR X;

smacro procedure RATDEN X;
 CDR X;

smacro procedure MKRAT(X,Y);
  CONS(X,Y);

smacro procedure POLTRM X;	% parts of Poly
 CAR X;

smacro procedure POLRED X;
 CDR X;

smacro procedure MKPOLY(X,Y);
 CONS(X,Y);

smacro procedure TRMPWR X;	% parts of TERM
 CAR X;

smacro procedure TRMCOEF X;
 CDR X;

smacro procedure MKTERM(X,Y);
 CONS(X,Y);

smacro procedure PWRVAR X;	% parts of Poly
 CAR X;

smacro procedure PWREXPT X;
 CDR X;

smacro procedure MKPWR(X,Y);
 CONS(X,Y);

smacro procedure POLVAR X;
 PWRVAR TRMPWR POLTRM X;

smacro procedure POLEXPT X;
 PWREXPT TRMPWR POLTRM X;

smacro procedure POLCOEF X;
  TRMCOEF POLTRM X;

%*********************** Utility Routines *****************************

procedure VARP X;
 IDP X OR (PAIRP X AND IDP CAR X);


%*********************** Entry Point **********************************

FLUID '(!*RBACKTRACE 
        !*RECHO 
        REXPRESSION!* 
        !*RMESSAGE
        PromptString!*
        TOK!*
	CurrentScantable!*
);

!*RECHO := NIL; % No echo of parse
!*RMESSAGE := T; % Do Print messages

procedure RAT();	%. Main LOOP, end with QUIT OR Q
BEGIN SCALAR VVV,PromptString!*;
      Prin2T "Canonical Rational Evaluator";
      PromptString!*:="poly> ";
      ALGINIT();
      CLEARTOKEN();		% Initialize scanner
LOOP: VVV := ERRORSET('(RPARSE),T,!*RBACKTRACE);
      IF ATOM VVV THEN		% What about resetting the Scanner?
	<<PRINT LIST('RATT, 'error, VVV); CLEARTOKEN();GO TO LOOP>>;
      REXPRESSION!* := CAR VVV;
      IF !*RECHO THEN PRINT LIST('parse,REXPRESSION!*);
      IF REXPRESSION!* EQ 'QUIT THEN <<
	PRINT 'QUITTING;
	RETURN >>;
      ERRORSET('(RATPRINT (RSIMP REXPRESSION!*)),T,!*RBACKTRACE);
 GOTO LOOP
END RAT;

procedure ALGG();	%. Main LOOP, end with QUIT OR Q
BEGIN SCALAR VVV,PromptString!*;
      prin2t "non-canonical rational evaluator";
      alginit();
      promptstring!* := "poly> ";
      cleartoken();		% initialize scanner
loop: vvv := errorset('(rparse),t,!*rbacktrace);
      if atom vvv then		% what about resetting the scanner?
	<<print list('algg, 'error, vvv); cleartoken();go to loop>>;
      rexpression!* := car vvv;
      if !*recho then print rexpression!*;
      if rexpression!* eq 'quit then <<
	print 'quitting;
	return >>;
      errorset('(preprint (presimp rexpression!*)),t,!*rbacktrace);
  go to loop
end algg;

procedure alginit();   %. called to init tables
 begin  
	inittoken();
        prin2t "quit; to exit";
	put('times2,'rsimp,'r!*);	%. simplifier tables
	put('plus2,'rsimp,'r!+);
	put('difference,'rsimp,'r!-);
	put('quotient,'rsimp,'r!/);
	put('expt,'rsimp,'r!^);
	put('diff,'rsimp,'r!');
	put('minus,'rsimp,'r!.neg);
	put('!+,'rexp,'plus2);	 % use corresponding 'r!xx in eval mode
	put('!-,'rexp,'difference);
	put('!*,'rterm,'times2);;
	put('!/,'rterm,'quotient);
	put('!^,'rprimary,'expt);
	put('!','rprimary,'diff);
	put('plus2,'prinop,'plusprin);	%. output funs
	put('difference,'prinop,'differenceprin);
	put('times2,'prinop,'timesprin);
	put('quotient,'prinop,'quotprin);
	put('expt,'prinop,'expprin);
 end;

procedure cleartoken;
 nil;

procedure inittoken;
<< AlgScantable!* := 
 '[17 11 11 11 11 11 11 11 11 17 17 11 17 17 11 11 11 11 11 11 11 11 11 11 
   11 11 11 11 11 11 11 11 17 14 15 11 11 12 11 11 11 11 13 19 11 18 20 11 
    0 1 2 3 4 5 6 7 8 9 13 11 13 11 13 11 11 10 10 10 10 10 10 10 10 10 10 
   10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 11 16 11 11 10 11 10 10 
   10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 
   11 11 11 11 11 Algdiphthong];
   AlgScanTable!*[char '!+]:=11;
   AlgScanTable!*[char '!-]:=11;
>>;


procedure NTOKEN;
 Begin Scalar CurrentScantable!*;
  CurrentScanTable!* := AlgScanTable!*;
  TOK!* := RATOM();
  Return Tok!*;
 End;

procedure RSIMP X;	 %. Simplify Prefix Form to Canonical
 IF ATOM X THEN RCREATE X
  ELSE BEGIN SCALAR Y,OP;
   OP:=CAR X; 
   IF (Y:=GET(OP,'RSIMP)) THEN RETURN APPLY(Y,RSIMPL CDR X);
  Y:=PRESIMP X;      % As "variable" ? 
  IF ATOM Y OR NOT(X=Y) THEN RETURN RSIMP Y;
  RETURN RCREATE Y;
 END;

procedure RSIMPL X;	%. Simplify argument list
 IF NULL X THEN NIL  ELSE RSIMP(CAR X) . RSIMPL CDR X;

procedure PRESIMP X;	 %. Simplify Prefix Form to PREFIX
 IF ATOM X THEN X
  ELSE BEGIN SCALAR Y,OP;
   OP:=CAR X; 
   IF (Y:=GET(OP,'RSIMP)) THEN RETURN RAT2PRE APPLY(Y,RSIMPL CDR X);
   X:=PRESIMPL CDR X;
   IF (Y:=GET(OP,'PRESIMP)) THEN RETURN APPLY(Y,X);
   RETURN (OP . X);
 END;

procedure PRESIMPL X;	%. Simplify argument list
 IF NULL X THEN NIL  ELSE PRESIMP(CAR X) . PRESIMPL CDR X;

%**************** Simplification Routines for Rationals ***************

procedure R!+(A,B);	%. RAT addition
    IF RATDEN A = RATDEN B THEN          %/ Risa
	MAKERAT(P!+(RATNUM A,RATNUM B),RATDEN A)
     ELSE
	MAKERAT(P!+(P!*(RATNUM A,RATDEN B),
		     P!*(RATDEN A,RATNUM B)),
		P!*(RATDEN A,RATDEN B));

procedure R!-(A,B);	%. RAT subtraction
    R!+(A,R!.NEG B);

procedure R!.NEG A;	%. RAT negation
    MKRAT(P!.NEG RATNUM A,RATDEN A);

procedure R!*(A,B);	%. RAT multiplication
    BEGIN SCALAR X,Y;
	X:=MAKERAT(RATNUM A,RATDEN B);
	Y:=MAKERAT(RATNUM B,RATDEN A);
	IF RATNUM X=0 OR RATNUM Y=0 THEN RETURN 0 . 1;
	RETURN MKRAT(P!*(RATNUM X,RATNUM Y),
		    P!*(RATDEN X,RATDEN Y))
END;

procedure R!.RECIP A;	%. RAT inverse
    IF RATNUM A=0 THEN ERROR(777,'(ZERO DIVISOR))
    ELSE MKRAT(RATDEN A,RATNUM A);

procedure R!/(A,B); 	%. RAT division
   R!*(A,R!.RECIP B);

procedure R!.LVAR A;	%. Leading VARIABLE of RATIONAL
 BEGIN SCALAR P;
	P:=RATNUM A;
	IF NUMBERP P THEN RETURN ERROR(99,'(non structured polynomial));
	P:=POLVAR P;
	RETURN P;
 END;

procedure R!'(A,X);	%. RAT derivative
 <<X:=R!.LVAR X;
   IF RATDEN A=1 THEN MKRAT(PDIFF(RATNUM A,X),1)
    ELSE R!-(MAKERAT(PDIFF(RATNUM A,X),RATDEN A),
	     MAKERAT(P!*(RATNUM A,PDIFF(RATDEN A,X)),
		     P!*(RATDEN A,RATDEN A) ) ) >>;

procedure RCREATE X;		%. RAT create
    IF NUMBERP X THEN X . 1
     ELSE IF VARP X THEN (PCREATE X) . 1
     ELSE ERROR(100,LIST(X, '(non kernel)));

procedure MAKERAT(A,B);
IF A=B THEN MKRAT(1,1)
 ELSE IF A=0 THEN 0 . 1
 ELSE IF B=0 THEN ERROR(777,'(ZERO DIVISOR))
 ELSE IF NUMBERP A AND NUMBERP B THEN 
	BEGIN SCALAR GG;
	    GG:=NUMGCD(A,B);
            IF B<0 THEN <<B:=-B; A := -A>>;
    	    RETURN MKRAT(A/GG,B/GG)
	END
 ELSE BEGIN SCALAR GG,NN;
	GG:=PGCD(A,B);
	IF GG=1 THEN RETURN MKRAT(A,B);
	NN:=GG;
LL:	IF NUMBERP NN THEN NN:=GCDPT(GG,NN)
	 ELSE << NN:=POLCOEF GG; GOTO LL >>;
	GG:=CAR PDIVIDE(GG,NN);
	RETURN MKRAT(DIVIDEOUT(A,GG),DIVIDEOUT(B,GG))
END;

procedure R!^(A,N);		%. RAT Expt
 BEGIN  SCALAR AA;
   N:=RATNUM N;
   IF NOT NUMBERP N THEN RETURN ERROR(777,'(Non numeric exponent))
      ELSE IF N=0 THEN RETURN RCREATE 1;
     IF N<0 THEN <<A:=R!.RECIP A; N:=-N>>;
	AA:=1 . 1;
	FOR I:=1:N DO AA:=R!*(AA,A);
	RETURN AA
  END;

%**************** Simplification Routines for Polynomials *************

procedure P!+(A,B);	%. POL addition
    IF A=0 THEN B  ELSE IF B=0 THEN A  ELSE
    IF NUMBERP A AND NUMBERP B THEN PLUS2(A,B)
     ELSE IF NUMBERP A THEN MKPOLY(POLTRM B,P!+(A,POLRED B))
     ELSE IF NUMBERP B THEN MKPOLY(POLTRM A,P!+(B,POLRED A))
     ELSE BEGIN SCALAR ORD;
	ORD:=PORDERP(POLVAR A,POLVAR B);
	IF ORD=1 THEN RETURN MKPOLY(POLTRM A,P!+(POLRED A,B));
	IF ORD=-1 THEN RETURN MKPOLY(POLTRM B,P!+(POLRED B,A));
	IF POLEXPT A=POLEXPT B THEN RETURN
	    BEGIN SCALAR AA,BB;
		AA:=P!+(POLCOEF A,POLCOEF B);
		IF AA=0 THEN RETURN P!+(POLRED A,POLRED B);
		AA:=MKPOLY(TRMPWR POLTRM A,AA);
		AA:=ZCONS AA; BB:=P!+(POLRED A,POLRED B);
		RETURN P!+(AA,BB) END;
	IF POLEXPT A>POLEXPT B THEN RETURN
		MKPOLY(POLTRM A,P!+(POLRED A,B));
	RETURN MKPOLY(POLTRM B,P!+(POLRED B,A))
    END;

procedure PORDERP(A,B);	%. POL variable ordering
  IF A EQ B THEN 0
	 ELSE IF ORDERP(A,B) THEN 1  ELSE -1;

procedure P!*(A,B);		%. POL multiply
    IF NUMBERP A THEN
        IF A=0 THEN 0
	 ELSE IF NUMBERP B THEN TIMES2(A,B)
	 ELSE CONS(CONS(CAAR B,PNTIMES(CDAR B,A)),
		  PNTIMES(CDR B,A))
     ELSE IF NUMBERP B THEN  PNTIMES(A,B)
     ELSE P!+(PTTIMES(CAR A,B),P!*(CDR A,B));

procedure PTTIMES(TT,A);	%. POL term mult
    IF NUMBERP A THEN
	IF A=0 THEN 0  ELSE
	ZCONS CONS(CAR TT,PNTIMES(CDR TT,A))
     ELSE P!+(TTTIMES(TT,CAR A),PTTIMES(TT,CDR A));

procedure PNTIMES(A,N);	%. POL numeric coef mult
    IF N=0 THEN 0
     ELSE IF NUMBERP A THEN TIMES2(A,N)
     ELSE CONS(CONS(CAAR A,PNTIMES(CDAR A,N)),PNTIMES(CDR A,N));

procedure TTTIMES(TA,TB);	%. TERM Mult
    BEGIN SCALAR ORD;
	ORD:=PORDERP(CAAR TA,CAAR TB);
	RETURN IF ORD=0 THEN
		ZCONS(CONS(CONS(CAAR TA,PLUS2(CDAR TA,CDAR TB)),
			P!*(CDR TA,CDR TB)))
	 ELSE IF ORD=1 THEN
		ZCONS(CONS(CAR TA,P!*(ZCONS TB,CDR TA)))
	 ELSE    ZCONS(CONS(CAR TB,P!*(ZCONS TA,CDR TB)))
END;

procedure ZCONS A; 		%. Make single term POL
  CONS(A,0);

procedure PCREATE1(X);          %. Create POLY from Variable/KERNEL
	ZCONS(CONS(CONS(X,1),1));

procedure PCREATE X;
 IF IDP X THEN PCREATE1 X
  ELSE IF PAIRP X AND IDP CAR X THEN PCREATE1 MKKERNEL X
  ELSE ERROR(1000,LIST(X, '(bad kernel)));

procedure PGCD(A,B);		%. POL Gcd
% A and B must be primitive.
IF A=1 OR B=1 THEN 1  ELSE
IF NUMBERP A AND NUMBERP B THEN NUMGCD(A,B)
 ELSE IF NUMBERP A THEN GCDPT(B,A)
 ELSE IF NUMBERP B THEN GCDPT(A,B)
 ELSE BEGIN SCALAR ORD;
	ORD:=PORDERP(CAAAR A,CAAAR B);
	IF ORD=0 THEN RETURN GCDPP(A,B);
	IF ORD>0 THEN RETURN GCDPT(A,B);
	RETURN GCDPT(B,A)
END;

procedure NUMGCD(A,B);		%. Numeric GCD
	IF A=0 THEN ABS B
	 ELSE NUMGCD(REMAINDER(B,A),A);

procedure GCDPT(A,B);		%. POL GCD, non-equal vars
IF NUMBERP A THEN IF NUMBERP B THEN NUMGCD(A,B)  ELSE
	GCDPT(B,A)  ELSE
BEGIN SCALAR ANS,ANS1;
	ANS:=PGCD(CDAR A,B);
	A:=CDR A;
	WHILE NOT NUMBERP A DO <<
	    ANS1:=PGCD(CDAR A,B);
	    ANS:=PGCD(ANS,ANS1);
	    A:=CDR A;
	    IF ANS=1 THEN RETURN ANS >>;
	RETURN IF A=0 THEN ANS  ELSE GCDPT(ANS,A)
END;

procedure GCDPP(A,B);		%. POL GCD, equal vars
BEGIN SCALAR TT,PA,ALPHA,PREVALPHA;
	IF POLEXPT B>POLEXPT A THEN <<
	  TT := A;
	  A := B;
	  B := TT >>;
	ALPHA := 1;
LOOP:	PREVALPHA := ALPHA;
	ALPHA := POLCOEF B;
	PA := POLEXPT A - POLEXPT B;
	IF PA<0 THEN <<
          PRINT A;
	  PRINT B;
	  PRINT PA;
	  ERROR(999,'(WRONG)) >>;
	WHILE NOT (PA=0) DO <<
	  PA := PA-1;
	  ALPHA := P!*(POLCOEF B,ALPHA) >>;
	A := P!*(A,ALPHA);	% to ensure no fractions;
	TT := CDR PDIVIDE(A,B);	% quotient and remainder of polynomials;
	IF TT=0 THEN
	  RETURN B;	% which is the GCD;
	A := B;
	B := PDIVIDE(TT,PREVALPHA);
	IF NOT(CDR B=0) THEN
	  ERROR(12,'(REDUCED PRS FAILS));
	B := CAR B;
	IF NUMBERP B OR NOT (POLVAR A EQ POLVAR B) THEN RETURN 1;
                % Lost leading VAR we started with. /MLG
	GO TO LOOP
END;

procedure DIVIDEOUT(A,B);	%. POL exact division
	CAR PDIVIDE(A,B);
	    
procedure PDIVIDE(A,B);	%. POL (quotient.remainder)
    IF NUMBERP A THEN
	IF NUMBERP B THEN DIVIDE(A,B)
	 ELSE CONS(0,A)
     ELSE IF NUMBERP B THEN BEGIN SCALAR SS,TT;
	SS:=PDIVIDE(CDR A,B);
	TT:=PDIVIDE(CDAR A,B);
	RETURN CONS(
		P!+(P!*(ZCONS CONS(CAAR A,1),CAR TT),CAR SS),
		P!+(P!*(ZCONS CONS(CAAR A,1),CDR TT),CDR SS))
    END
     ELSE BEGIN SCALAR QQ,BB,CC,TT;
            IF NOT(POLVAR A EQ POLVAR B) OR POLEXPT A < POLEXPT B THEN
	      RETURN CONS(0,A);		% Not same var/MLG, degree check/DFM
	    QQ:=PDIVIDE(POLCOEF A,POLCOEF B);	% Look for leading term;
	    IF NOT(CDR QQ=0) THEN RETURN CONS(0,A);
	    QQ:=CAR QQ;			%Get the quotient;
	    BB:=P!*(B,QQ);
	    IF CDAAR A>CDAAR B THEN <<
		TT:=ZCONS CONS(CONS(CAAAR A,CDAAR A-CDAAR B),1);
		BB:=P!*(BB,TT);
		QQ:=P!*(QQ,TT)
	     >>;
	    CC:=P!-(A,BB);			%Take it off;
	    BB:=PDIVIDE(CC,B);
	    RETURN CONS(P!+(QQ,CAR BB),CDR BB)
    END;

procedure P!-(A,B);		%. POL subtract
    P!+(A,P!.NEG B);

procedure P!.NEG(A);		%. POL Negate
  IF NUMBERP A THEN -A
     ELSE CONS(CONS(CAAR A,P!.NEG CDAR A),P!.NEG CDR A);

procedure PDIFF(A,X);		%. POL derivative (to variable)
    IF NUMBERP A THEN 0
     ELSE BEGIN SCALAR ORD;
	ORD:=PORDERP(POLVAR A,X);
	RETURN
	IF ORD=-1 THEN 0
	 ELSE IF ORD=0 THEN 
	    IF CDAAR A=1 THEN
		CDAR A
	     ELSE P!+(ZCONS CONS(CONS(X,CDAAR A-1),P!*(CDAAR A,CDAR A)),
		     PDIFF(CDR A,X))
	 ELSE P!+(P!*(ZCONS CONS(CAAR A,1),PDIFF(CDAR A,X)),PDIFF(CDR A,X))
END;

procedure MKKERNEL X;
 BEGIN SCALAR KERNELS,K,OP;
       K:=KERNELS:=GET(OP:=CAR X,'KERNELS);
 L:    IF NULL K THEN RETURN<<PUT(OP,'KERNELS,X.KERNELS);X>>;
       IF X=CAR K THEN RETURN CAR K;
	K:=CDR K;
	GOTO L
  END;

%***************************** Parser *********************************

% Simple parser creates expressions to be evaluated by the
% rational polynomial routines.
% J.  Marti, August 1980. 
% Modified and Extended by GRISS and GALWAY
% Rewritten to be left associative by OTTENHEIMER, March 1981


procedure RPARSE();	%. PARSE Infix to Prefix
BEGIN SCALAR X;
  NTOKEN();
  IF TOK!* EQ '!; THEN RETURN NIL;	% Fix for null exp RBO 9 Feb 81
  IF NULL(X := REXP()) THEN RETURN ERROR(105, '(Unparsable Expression));
  IF TOK!* NEQ '!; THEN RETURN ERROR(106, '(Missing !; at end of expression));
  RETURN X
END;

procedure REXP();	 %. Parse an EXP and rename OP
BEGIN SCALAR LEFT, RIGHT,OP;
  IF NOT (LEFT := RTERM()) THEN RETURN NIL;
  WHILE (OP := GET(TOK!*,'REXP)) DO
    << NTOKEN();
       IF NOT(RIGHT := RTERM()) THEN RETURN ERROR(100, '(Missing Term in Exp));
       LEFT := LIST(OP, LEFT, RIGHT)
    >>;
  RETURN LEFT
END;

procedure RTERM();	%. PARSE a TERM
BEGIN SCALAR LEFT, RIGHT, OP;
  IF NOT (LEFT := RPRIMARY()) THEN RETURN NIL;
  WHILE (OP := GET(TOK!*,'RTERM)) DO
    << NTOKEN();
       IF NOT (RIGHT := RPRIMARY()) THEN
	  RETURN ERROR (101, '(Missing Primary in Term));
       LEFT := LIST(OP, LEFT, RIGHT)
    >>;
  RETURN LEFT
END;

procedure RPRIMARY();	%. RPRIMARY, allows "^" and "'"
BEGIN SCALAR LEFT, RIGHT, OP;
  IF TOK!* EQ '!+ THEN RETURN <<NTOKEN(); RPRIMARY0()>>;
  IF TOK!* EQ '!- 
      THEN RETURN << NTOKEN();
		     IF (LEFT := RPRIMARY0()) THEN LIST('MINUS, LEFT) 
                     ELSE RETURN ERROR(200,'(Missing Primary0 after MINUS))
		  >>;

  IF NOT (LEFT := RPRIMARY0()) THEN RETURN NIL;
  WHILE (OP := GET(TOK!*,'RPRIMARY)) DO
    << NTOKEN();
       IF NOT (RIGHT := RPRIMARY0()) THEN 
		RETURN ERROR(200, '(Missing Primary0 in Primary));
       LEFT := LIST(OP, LEFT, RIGHT) 
    >>;
  RETURN LEFT;
END;

procedure RPRIMARY0();		%. Variables, etc
BEGIN SCALAR EXP, ARGS;
  IF TOK!* EQ '!( THEN
    << NTOKEN();
       IF NOT (EXP := REXP()) THEN RETURN ERROR(102, '(Missing Expression));
       IF TOK!* NEQ '!) THEN RETURN ERROR(103, '(Missing Right Parenthesis));
       NTOKEN();
       RETURN EXP
    >>;

    IF NUMBERP(EXP := TOK!*) 
      THEN RETURN <<NTOKEN(); EXP>>;

    IF NOT IDP EXP THEN  RETURN NIL;
    NTOKEN();
    IF ARGS := RARGS(EXP) THEN RETURN ARGS;
    RETURN EXP;
END;

procedure RARGS(X);
  BEGIN SCALAR ARGS,ARG;
	IF TOK!* NEQ '!( THEN RETURN NIL;
	NTOKEN();
	IF TOK!* EQ '!) THEN RETURN <<NTOKEN();X . NIL>>;
  L:	IF NOT (ARG :=REXP()) THEN ERROR(104,'(Not expression in ARGLST));
	ARGS := ARG . ARGS;
	IF TOK!* EQ '!, THEN <<NTOKEN(); GOTO L>>;
	IF TOK!* EQ '!) THEN RETURN <<NTOKEN();X . REVERSE ARGS>>;
        ERROR(105,'(Missing !) or !, in ARGLST));
  END;

procedure MKATOM X;
%  Use LIST('RCREATE, LIST('QUOTE,x)); if doing EVAL mode
 X;

%******************* Printing Routines ********************************

procedure PPRINT A;
% Print internal canonical form in Infix notation.
    IF NUMBERP A THEN PRIN2 A  ELSE
BEGIN
	IF NUMBERP CDAR A THEN
	  IF CDAR A = 0 THEN
	    << PRIN2 '0; RETURN NIL >>
	   ELSE IF CDAR A NEQ 1 THEN 
	    << PRIN2 CDAR A; PRIN2 '!* >>
	   ELSE NIL
	 ELSE IF RPREC!* CDAR A THEN << PPRINT CDAR A; PRIN2 '!* >> 
	   ELSE <<PRIN2 '!(; PPRINT CDAR A; PRIN2 '!)!* >>;
	IF CDAAR A = 0 THEN PRIN2 1
	   ELSE IF CDAAR A = 1 THEN PRIN2 CAAAR A
	   ELSE << PRIN2 CAAAR A; PRIN2 '!^;
		  IF RPREC!^ CDAAR A THEN PPRINT CDAAR A
		    ELSE <<PRIN2 '!(; PPRINT CAAAR A; PRIN2 '!) >> >>;
	IF NUMBERP CDR A THEN
	  IF CDR A> 0 THEN <<PRIN2 '!+ ; PRIN2 CDR A; RETURN NIL>>
	   ELSE IF CDR A < 0 THEN <<PRIN2 '!-! ; PRIN2 (-CDR A);
                                        RETURN NIL>>
           ELSE RETURN NIL;
	IF ATOM CDR A THEN <<PRIN2  '!+ ; PRIN2 CDR A; RETURN NIL>>;
	PRIN2 '!+ ; PPRINT CDR A;
END;

procedure RPREC!* X;	%. T if there is no significant addition in X.
  ATOM X OR (NUMBERP POLRED X AND POLRED X = 0);

procedure RPREC!^ X;	%. T if there is not significant 
                        %. addition or multiplication in X.
RPREC!* X AND (ATOM X OR
  (ATOM CDAR X AND NUMBERP CDAR X));

procedure SIMPLE X;	%. POL that doest need ()
 ATOM X OR ((POLRED X=0) AND (POLEXPT X=1) AND (POLCOEF X =1));

procedure RATPRINT A;	%. Print a RAT
BEGIN
        IF CDR A = 1 THEN PPRINT CAR A
         ELSE <<NPRINT CAR A;
		PRIN2 '!/; 
	        NPRINT CDR A>>;
	TERPRI()
END;

procedure NPRINT A; 	%. Add parens, if needed
 IF NOT SIMPLE A THEN <<PRIN2 '!( ; PPRINT A; PRIN2 '!) >>
  ELSE PPRINT A;

%. Convert RCAN back to PREFIX form

procedure RAT2PRE X;           %. RATIONAL to Prefix
 IF RATDEN X = 1 THEN POL2PRE RATNUM X
  ELSE LIST('QUOTIENT,POL2PRE RATNUM X, POL2PRE RATDEN X);

procedure POL2PRE X;		%. Polynomial to Prefix
BEGIN SCALAR TT,RR;
 IF NOT PAIRP X THEN RETURN X;
  TT:=TRM2PRE POLTRM X;
  RR:=POL2PRE POLRED X;
  IF RR = 0 THEN RETURN TT;
  IF NUMBERP RR AND RR <0 THEN RETURN LIST('DIFFERENCE,TT,-RR);
  RETURN  LIST('PLUS2,TT,RR);
END;

procedure TRM2PRE X;		%. Term to Prefix
 IF TRMCOEF X = 1 THEN PWR2PRE TRMPWR X
  ELSE IF TRMCOEF X = (-1) THEN LIST('MINUS,PWR2PRE TRMPWR X)
  ELSE LIST('TIMES2,POL2PRE TRMCOEF X,PWR2PRE TRMPWR X);

procedure PWR2PRE X;		%. Power to Prefix
 IF PWREXPT X = 1 THEN PWRVAR X
  ELSE LIST('EXPT,PWRVAR X,PWREXPT X);

%. prefix Pretty print

procedure PREPRIN(A,PARENS);	%. Print PREFIX form in Infix notation.
 BEGIN SCALAR PRINOP;
	IF ATOM A THEN RETURN PRIN2 A;
        IF (PRINOP:=GET(CAR A,'PRINOP)) 
	 THEN RETURN APPLY(PRINOP,LIST(A,PARENS));
	PRIN2(CAR A); PRINARGS CDR A;
	RETURN A;
 END;

procedure PRINARGS A;	%. Print ArgLIST
 IF NOT PAIRP A THEN PRIN2 '!(!)
  ELSE <<PRIN2 '!(; WHILE PAIRP A DO
		    <<PREPRIN(CAR A,NIL); 
		      IF PAIRP (A:=CDR A) THEN PRIN2 '!,>>;
	PRIN2 '!)>>;

procedure PREPRINT A;
 <<PREPRIN(A,NIL); TERPRI(); A>>;

procedure NARYPRIN(OP,ARGS,PARENS);
  IF NOT PAIRP ARGS THEN NIL
   ELSE IF NOT PAIRP CDR ARGS THEN PREPRIN(CAR ARGS,PARENS)
   ELSE <<IF PARENS THEN PRIN2 '!(; 
	  WHILE PAIRP ARGS DO
		  <<PREPRIN(CAR ARGS,T); % Need precedence here
		    IF PAIRP(ARGS:=CDR ARGS) THEN PRIN2 OP>>;
          IF PARENS THEN PRIN2 '!)>>;
	
         
procedure PLUSPRIN(A,PARENS);
  NARYPRIN('! !+! ,CDR A,PARENS);

procedure DIFFERENCEPRIN(A,PARENS);
  NARYPRIN('! !-! ,CDR A,PARENS);

procedure TIMESPRIN(A,PARENS);
  NARYPRIN('!*,CDR A,PARENS);

procedure QUOTPRIN(A,PARENS);
   NARYPRIN('!/,CDR A,PARENS);

procedure EXPPRIN(A,PARENS);
  NARYPRIN('!^,CDR A,PARENS);


procedure OrderP(x,y);
% ordering of ID's as VARS
 Id2int(x) <= Id2Int (y);


End;

