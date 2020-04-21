%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                     
% 		PASCAL BASED MINI-LISP
%
% File: 	PAS3.RED - Basic LISP Functions
% ChangeDate: 	10:48pm  Wednesday, 15 July 1981
% By: 		M. L. Griss
%       	Change to add Features for Schlumberger Demo
% 
% 	    All RIGHTS RESERVED
%           COPYRIGHT (C) - 1981 - M. L. GRISS
%           Computer Science Department
%           University of Utah
%
%           Do Not distribute with out written consent of M. L. Griss
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%. Tagged TCATCH and TTHROW In terms of CATCH and THROW

SYMBOLIC PROCEDURE TCATCH(TG,FORM);
 BEGIN SCALAR VAL;
     THROWING!* := NIL;
     VAL:=CATCH(FORM);
     IF NULL TG OR NULL THROWING!* THEN RETURN VAL;	% CatchALL
     IF THROWTAG!* EQ TG THEN RETURN VAL;
     THROW VAL;
 END;

SYMBOLIC PROCEDURE TTHROW(TG,VAL);
 <<THROWING!* := 'T;
   THROWTAG!* := TG;
   THROW VAL>>;

SYMBOLIC PROCEDURE GETD NAM;		%. Return (type.code) if func
  BEGIN SCALAR TY,V;
	IF NOT IDP NAM THEN RETURN NIL;
	TY:=GET(NAM,'TYPE);
	V:=FUNCELL NAM;
	IF NULL TY AND V THEN TY:='EXPR;
        IF V THEN RETURN (TY . V) ELSE RETURN NIL;
  END;

SYMBOLIC PROCEDURE PUTD(NAM,TY,BOD);	%. Make function entry
 IF FLAGP(NAM, 'LOSE) THEN
 <<  ERRPRT LIST(NAM,'not,'flagged,'LOSE); NAM >>
 ELSE BEGIN
	IF GETD(NAM) THEN MSGPRT LIST('Function,NAM,'redefined);
	IF (CODEP BOD OR EQCAR(BOD,'LAMBDA)
          AND TY MEMQ '(EXPR FEXPR NEXPR MACRO) )
 	 THEN <<IF TY EQ 'EXPR THEN TY:=NIL;
                PUT(NAM,'TYPE,TY);
	        SETFUNCELL(NAM,BOD)>>
          ELSE RETURN ERROR(99,LIST(NAM,'Cant,'be,'defined));
	RETURN NAM;
 END;

SYMBOLIC PROCEDURE REMD NAM;		%. Remove function defn
 BEGIN SCALAR PR;
	IF (PR:=GETD NAM) THEN <<SETFUNCELL(NAM,NIL);
				 REMPROP(NAM,'TYPE)>>;
	RETURN PR;
 END;


%. Convenient definitions

SYMBOLIC PROCEDURE PUTL(L,IND,VAL);
 IF NOT PAIRP L THEN NIL
  ELSE <<PUT(CAR L,IND,VAL);
         PUTL(CDR L,IND,VAL)>>;

SYMBOLIC FEXPR PROCEDURE DE L;
   PUTD(CAR L,'EXPR,'LAMBDA . CDR L);

SYMBOLIC FEXPR PROCEDURE DF L;
   PUTD(CAR L,'FEXPR,'LAMBDA . CDR L);

SYMBOLIC FEXPR PROCEDURE DN L;
   PUTD(CAR L,'NEXPR,'LAMBDA . CDR L);

SYMBOLIC FEXPR PROCEDURE DM L;
   PUTD(CAR L,'MACRO,'LAMBDA . CDR L);

%. d) Improved EVAL, with LAMBDA, FEXPR, etc

SYMBOLIC PROCEDURE EVAL(X);
 BEGIN SCALAR FN,A,TY;
 L:IF IDP(X) THEN RETURN VALUE(X)
    ELSE IF NOT PAIRP(X) OR (FN := CAR X) EQ 'LAMBDA THEN
	RETURN X;
    A := CDR X;                         % Arguments
    IF FN EQ 'QUOTE THEN		%Important special Fexprs
	RETURN CAR(A);
    IF FN EQ 'SETQ THEN RETURN SET(CAR A,EVAL CADR A);
    IF IDP FN AND (TY := GET(FN, 'TYPE)) THEN 
     <<IF TY EQ 'FEXPR THEN
           RETURN APPLY1(FN,A);   % No Spread, No EVAL
       IF TY EQ 'NEXPR THEN
        RETURN APPLY1(FN,EVLIS A); % No Spread, EVAL
       IF TY EQ 'MACRO               % Reval full form
          THEN  <<X := APPLY1(FN,X);  GOTO L >> >>;
       A := EVLIS A;
       IF FN EQ 'LIST THEN RETURN A;
       RETURN APPLY(FN,A);
END;

SYMBOLIC PROCEDURE APPLY1(FN,A);
 APPLY(FN, A . NIL);

SYMBOLIC PROCEDURE APPLY(FN,A);
 BEGIN SCALAR EFN;
    EFN := FUNCELL FN;
    IF CODEP EFN THEN RETURN XAPPLY(EFN,A); % Spread args and EXECUTE
    RETURN EVLAM(EFN,A);
END;

SYMBOLIC PROCEDURE EVLIS(L);
IF NOT PAIRP L THEN EVAL L
 ELSE EVAL(CAR L) . EVLIS(CDR L);

%. Some standard FEXPRS and MACROS

SYMBOLIC FEXPR PROCEDURE PROGN ARGS;	%. Evaluate a LIST
  P!.N ARGS;

SYMBOLIC PROCEDURE PROG2(A,B); B;

SYMBOLIC PROCEDURE P!.N ARGS;		%. EVALS elems of list and returns last
BEGIN SCALAR ANS;
   WHILE PAIRP ARGS DO <<ANS := EVAL CAR ARGS; ARGS:=CDR ARGS>>;
  RETURN ANS
END;

%.===== Section 3.7 =====	Program Feature functions

% All this stuff should be rewritten to use the same binding mechanism as
% compiled code, and obey the same constraints on placement of GO/RETURN
% as compiled code.

SYMBOLIC FEXPR PROCEDURE RETURN E;	%. Return From Current PROG
<< P!.P := NIL;
   TTHROW('!$PROG!$,P!.N E) >>;

SYMBOLIC FEXPR PROCEDURE GO E;		%. Go to label in Current PROG
BEGIN SCALAR L;
  E := CAR E;
  REPEAT <<
    WHILE NOT IDP E DO
      ERROR(1100,LIST(E,'Not,'Label));
    L := ATSOC(E,P!.G);
    IF ATOM L THEN
      ERROR(1101,LIST(E,'Not,'a,'label))>>
  UNTIL PAIRP L;
  P!.P := CDR L;
  TTHROW('!$PROG!$,NIL)
END;

SYMBOLIC FEXPR PROCEDURE PROG E;	%. Program feature interpreter
%  P!.P is Next SEXPR to EVAL
BEGIN SCALAR TG,X,V,NVALS,SAVEP,SAVEG;
  SAVEP:=P!.P;
  SAVEG:=P!.G;	% Note FLUIDS not yet working compiled
  NVALS :=LENGTH CAR E;
  PBINDN CAR E;	% Bind each to NIL, putting old value on BSTACK
  P!.P := CDR E; 
% The code body
  X := P!.P;
  P!.G := NIL;
  FOR EACH U ON P!.P DO
    IF IDP CAR U THEN
  P!.G := U . P!.G;
  THROWING!* := NIL;
  TG := '!$PROG!$;
  WHILE P!.P AND TG EQ '!$PROG!$ DO <<
    X := CAR P!.P;
    P!.P := CDR P!.P;
    IF NOT IDP X THEN <<
      X := TCATCH(NIL,X);
      IF THROWING!* THEN
	<<TG := THROWTAG!*; V:=X>>  >> >>;
% UNBIND Even if thrown through
  UNBINDN NVALS;
  P!.P := SAVEP;
  P!.G := SAVEG;
  IF NOT(TG EQ '!$PROG!$) THEN
    TTHROW(TG,V)
  ELSE
    RETURN V
END;


SYMBOLIC FEXPR PROCEDURE WHILE ARGS;	%. Simple WHILE LOOP
% Will do (WHILE bool s1 .. sn)
  BEGIN SCALAR BOOL;
	IF NOT PAIRP ARGS THEN RETURN NIL;
	BOOL:=CAR ARGS;
 L1:	IF NULL EVAL BOOL THEN RETURN NIL;
	P!.N CDR ARGS;
	GOTO L1
 END;


SYMBOLIC FEXPR PROCEDURE AND(X);	%. Xis list of actions
   BEGIN 
     IF NOT PAIRP X THEN RETURN(T);
 L:  IF NULL CDR(X) THEN RETURN(EVAL(CAR X))
      ELSE IF NULL EVAL(CAR X) THEN RETURN(NIL)
      ELSE << X:=CDR X; GOTO L >>
 END;

%/// Add also IF ?

SYMBOLIC FEXPR PROCEDURE COND(E);		%. Conditional eval
   BEGIN SCALAR PR,Y;
 L:  IF NOT PAIRP E THEN RETURN NIL;
     PR:=CAR E; E:=CDR E;
     IF PAIRP PR THEN Y:=CAR PR ELSE Y:=PR;
     IF NULL (Y:=EVAL(Y)) THEN GOTO L;
     IF NULL PAIRP PR OR NULL CDR PR THEN RETURN(Y);
     RETURN P!.N(CDR PR)
   END;

SYMBOLIC FEXPR PROCEDURE  OR(X);	%. Or of action list
   BEGIN SCALAR Y;
 L: IF NOT PAIRP X THEN RETURN(NIL)
     ELSE IF(Y:=EVAL(CAR X)) THEN RETURN(Y)
     ELSE << X:=CDR X;GOTO L >>
 END;

%.===== Section 3.12 =====	MAP composite functions

SYMBOLIC PROCEDURE MAP(X,FN); 		%. Apply FN to each cdr x
   WHILE X DO <<APPLY1(FN,X); X := CDR X>>;

SYMBOLIC PROCEDURE MAPC(X,FN); 		%. Apply FN to each car x
   WHILE X DO <<APPLY1(FN,CAR X); X := CDR X>>;

SYMBOLIC PROCEDURE MAPCAN(X,FN); 	%. Append FN car x
   IF ATOM X THEN NIL ELSE NCONC(APPLY1(FN,CAR X),MAPCAN(CDR X,FN));

SYMBOLIC PROCEDURE MAPCAR(X,FN); 	%. Collect FN car x
   IF ATOM X THEN NIL ELSE APPLY1(FN,CAR X) . MAPCAR(CDR X,FN);

SYMBOLIC PROCEDURE MAPCON(X,FN); 	%. Append FN cdr x
   IF ATOM X THEN NIL ELSE NCONC(APPLY1(FN,X),MAPCON(CDR X,FN));

SYMBOLIC PROCEDURE MAPLIST(X,FN); 	%. Collect FN cdr x
   IF ATOM X THEN NIL ELSE APPLY1(FN,X) . MAPLIST(CDR X,FN);

SYMBOLIC PROCEDURE NCONC(U,V); 		%. Tack V onto end U
   BEGIN SCALAR W; 
      IF ATOM U THEN RETURN V; 
      W := U; 
      WHILE PAIRP CDR W DO W := CDR W; 
      RPLACD(W,V); 
      RETURN U
   END;

%... This procedure drives a simple read/eval/print top loop.

SYMBOLIC PROCEDURE PUTC(X,Y,Z);
  PUT(X,Y,Z);

SYMBOLIC PROCEDURE FLUID L;
  L;

SYMBOLIC PROCEDURE PRIN2TL L;
 IF NOT PAIRP L THEN TERPRI()
  ELSE <<PRIN2 CAR L; PRIN2 '! ; PRIN2TL CDR L>>;
% ... Missing functions to complete Standard LISP set
% ... some dummies developed for PERQ, modified to better use PASLSP


SYMBOLIC PROCEDURE FLOATP X; NIL;

SYMBOLIC PROCEDURE STRINGP X; IDP X;

SYMBOLIC PROCEDURE VECTORP X; NIL;

SYMBOLIC PROCEDURE FLUIDP X; NIL;

SYMBOLIC PROCEDURE INTERN X; X;

SYMBOLIC PROCEDURE REMOB X; NIL;

SYMBOLIC PROCEDURE GLOBAL X; 
   WHILE X DO <<FLAG(X,'GLOBAL); X := CDR X>>;

SYMBOLIC PROCEDURE GLOBALP X; 
   FLAGP(X,'GLOBAL);

SYMBOLIC PROCEDURE UNFLUID X; 
   NIL;


% No vectors yet

SYMBOLIC PROCEDURE GETV(A,B); NIL;

SYMBOLIC PROCEDURE MKVECT X; NIL;

SYMBOLIC PROCEDURE PUTV(A,B,C); NIL;

SYMBOLIC PROCEDURE UPBV X; NIL;

SYMBOLIC PROCEDURE DIGIT X; NIL;

SYMBOLIC PROCEDURE LITER X; NIL;
 
SYMBOLIC PROCEDURE READCH X; NIL;  %/ Needs Interp Mod
 
SYMBOLIC PROCEDURE RDEVPR;
 WHILE T DO PRINT EVAL READ();

SYMBOLIC PROCEDURE DSKIN(FILE);
 BEGIN SCALAR TMP;
   TMP := RDS OPEN(FILE, 'INPUT);
   WHILE NULL EOFP PRINT EVAL READ() DO NIL; %Use RDEVPR ?
   CLOSE RDS TMP;
 END;

SYMBOLIC PROCEDURE !*FIRST!-PROCEDURE;
BEGIN SCALAR X, EOFFLG, OUT;
    PRIN2TL '(Pascal  LISP  V2 !- 15 Feb 1982);
    PRIN2TL '(Copyright (c) 1981 U UTAH);
    PRIN2TL '(All  Rights  Reserved);
    NEXPRS:='(LIST);
    PUTL(NEXPRS,'TYPE,'NEXPR);
    PROCS:='(EXPR FEXPR NEXPR MACRO);
    EOFFLG := NIL;
    % Continue reading Init-File on channel 1;
    WHILE NOT EOFFLG DO
    <<  X := READ();
        EOFFLG := EOFP(X);
	IF NOT EOFFLG THEN
	    EVAL X
    >>;
    RDS(2); % Switch to USER input, THE TTY
    EOFFLG := NIL;
    WHILE NOT EOFFLG DO
      <<OUT := WRS 3; PRIN2 '!>; WRS OUT; % Prompt, OUT holds channel #
        X := READ();
        IF EQCAR(X,'QUIT) THEN EOFFLG := 'T ELSE EOFFLG := EOFP(X);
	IF NOT EOFFLG THEN
	  PRIN2T(CATCH X)
      >>;
    PRIN2T LIST('EXITING,'Top,'Loop);
END;

END;
