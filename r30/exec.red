COMMENT This file provides support for calling the EXEC and the system
	editor under TOPS-20 or TENEX;

SYMBOLIC;

GLOBAL '(PROGEXT!* PSYSDEV!* CRLFST!* EXECFORK!* EXECFILE!* SYSTEM!*
	 !$EOL!$);

PROGEXT!* := IF SYSTEM!*>0 THEN '(V A S !.) ELSE '(E X E !.);

PSYSDEV!* := IF SYSTEM!*>0 THEN '(!< S U B S Y S !>) ELSE '(S Y S !:);

CRLFST!* := IF SYSTEM!*<0 THEN LIST(INTERN ASCII 13,INTERN ASCII 10,'!")
	     ELSE LIST(!$EOL!$,'!");

EXECFORK!* := EXECFILE!* := IF SYSTEM!*<0 THEN "<SYSTEM>EXEC.EXE"
			     ELSE "<SYSTEM>EXEC.SAV";

SYMBOLIC PROCEDURE PINSTAT;
 BEGIN SCALAR X,Y,Z;
  Z := CURSYM!*;
  IF DELCP(X := NXTSYM!*) THEN GO TO DUN;
  Y := REVERSIP EXPLODEC NXTSYM!*;
  IF DELCP(X := CRCHAR!*) THEN GO TO DUN;
  Y :=  CRCHAR!* . Y;
  CRCHAR!* := '! ;
  WHILE NOT DELCP(X := READCHQ()) DO Y := X . Y;
DUN:
  NXTSYM!* := X;
  TTYPE!* := 3;
  SCAN();
  RETURN LIST(Z,IF Y THEN MKQUOTE REVERSIP Y ELSE NIL)
 END;

SYMBOLIC PROCEDURE READCHQ;
 IF !*INT AND NULL IFL!* THEN READCH1() ELSE READCH();

REMPROP('EXEC,'STAT);

REMPROP('PUSH,'STAT);

REMFLAG('(EXEC PUSH),'GO);

SYMBOLIC PROCEDURE PUSH U; EXEC U;   %we might as well support both;

SYMBOLIC PROCEDURE EXEC U;
 BEGIN SCALAR V,X,Y,Z;
   IF NULL U THEN RETURN XEQKEEP('EXECFORK!*,EXECFILE!*,NIL);
   V := U;
A: IF CAR U EQ '!: OR CAR U EQ '!< THEN Y := T
    ELSE IF CAR U EQ '!. THEN Z := T
    ELSE IF SEPRP CAR U THEN GO TO B;
   X := CAR U . X;
   IF (U := CDR U) THEN GO TO A;
B: X := REVERSIP('!" . IF Z THEN X ELSE APPEND(PROGEXT!*,X));
   X := COMPRESS('!" . IF Y THEN X ELSE APPEND(PSYSDEV!*,X));
   RETURN XEQKILL(X,LIST COMPRESS('!" . APPEND(V,CRLFST!*)))
 END;

PUT('EXEC,'STAT,'PINSTAT);

PUT('PUSH,'STAT,'PINSTAT);

%FLAG('(EXEC PUSH),'GO);

SYMBOLIC PROCEDURE XEQKILL(FILENAME,ARG);
   %handles infrequent calls by creating and killing each fork;
   <<!%XEQ(FILENAME,T,T,NIL,ARG); TERPRI();
     PRIN2T "Returned to REDUCE ..."; NIL>>;

SYMBOLIC EXPR PROCEDURE XEQKEEP(FORKN,FILE,ARG);
   %This retains the lower fork for speedy subsequent calls to the same
   %program (e.g., PUSH or EDIT), and the ---FILE check will set up the
   %fork again after a SAVE;
 BEGIN SCALAR A;
  A:=ERRORSET(LIST('!%XEQ,FORKN,T,NIL,NIL,MKQUOTE ARG),NIL,NIL);
  SET(FORKN,IF ATOM A THEN !%XEQ(FILE,T,NIL,NIL,ARG) ELSE CAR A);
  TERPRI();
  PRIN2T "Returned to REDUCE ..."
 END;

%SYMBOLIC PROCEDURE KFORK U;
% PAIRP ERRORSET(LIST('JSYS,153,MKQUOTE U,0,0,1),NIL,NIL);

%DATE!*:=JSYS(144,'(BUF),-1,604241920,1);

%The following function is called by BEGIN. It checks that terminal 
% linelength in REDUCE is shorter than the width of the controlling
% terminal.
% Commented out as it is to sensitive to operating system differences.
%SYMBOLIC PROCEDURE CHKLEN;
% BEGIN SCALAR A,B;
%  A := ERRORSET('(JSYS 63 65 24 0 3),NIL,NIL);	%Try MTOPR first, 
%  A := IF PAIRP A THEN CAR A
%        ELSE BOOLE(1,LSH(JSYS(71,65,0,0,2),-18),127); % else use RFMOD
%  IF A<10 THEN RETURN;
%  B := LINELENGTH NIL;
%  IF A LEQ B THEN LINELENGTH(A-1);
%  RETURN B
% END;


END;
