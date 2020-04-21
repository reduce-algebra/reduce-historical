%
% RLISP-PARSER.RED - RLISP parser based on Nordstrom and Pratt model
% 
% Author:      Martin Griss and Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        May 1981
% Copyright (c) 1981 University of Utah
%
% Known Bugs and Problems:
%	Procedure TEMPLATES parsed at wrong precendence, so
%	procedure x/y; is ok
%	procedure (x/Y) fails!
%
%	IF a Then B;  ELSE c;  parses badly, doesnt catch ELSE
%	QUOTIENT(A,B) parses as RECIP(A)
%
% Edit by Nancy Kendzierski, 07 Apr 1983 1337-PST
% Changed SEMIC!* to fluid (also in rlisp-support) to match kernel decls.
% Edit by Cris Perdue, 28 Jan 1983 2038-PST
% Occurrences of "dipthong" changed to "diphthong"
% <PSL.UTIL.NEWVERSIONS>RLISP-PARSER.RED.4, 16-Dec-82 12:11:15, Edit by KESSLER
%  Make SEMIC!* a Global (as in rlisp-support), so it won't be made fluid in 
%  compilation of Scan.
%  <PSL.UTIL>RLISP-PARSER.RED.3,  13-Dec-82 13:14:36, Edit by OTHMER
%  Flagged EMB as 'FTYPE so debug functions will work
%  <PSL.UTIL>RLISP-PARSER.RED.42, 17-Mar-82 02:36:14, Edit by BENSON
%  Finally infix as prefix works!!!
%  <PSL.UTIL>RLISP-PARSER.RED.25, 14-Jan-82 13:16:34, Edit by BENSON
%  Added JOIN to for each
%  <PSL.UTIL>RLISP-PARSER.RED.24, 30-Dec-81 01:01:30, Edit by BENSON
%  Unfixed infix as prefix.  Have to check to make sure the thing is an arglist
%  <PSL.UTIL>RLISP-PARSER.RED.21, 28-Dec-81 15:22:37, Edit by BENSON
%  fixed LAMBDA();...
%  <PSL.UTIL>RLISP-PARSER.RED.21, 28-Dec-81 15:21:43, Edit by BENSON
%  Infix operators used as prefix are parsed correctly
%  <PSL.UTIL>RLISP-PARSER.RED.19, 28-Dec-81 14:44:47, Edit by BENSON
%  Removed assign-op in favor of SetF
%  <PSL.UTIL>RLISP-PARSER.RED.36,  5-Feb-82 07:17:34, Edit by GRISS
%  Add NE as infix

CompileTime flag('(DefineBOpX DefineROpX DoInfixAsPrefix IsOpOp
		   DoPrefix DoInfix MakeLocals MkQuotList
		   PrecSet InfixOp PrefixOp RlispRead RemSemicol
		   SymErr RAtomHook
		   CommentPart), 'InternalFunction);

FLUID '(CURSYM!* !*InsideStructureRead SEMIC!*);
CURSYM!*:='! ;
global '(TokType!*);

lisp procedure SymErr(X, Y);
    StdError BldMsg("Syntax error %r", X);

SYMBOLIC PROCEDURE SCAN;
  BEGIN SCALAR X;
A:	CURSYM!* := RATOMHOOK();
	IF TOKTYPE!* EQ 3 THEN		 %/ Also a R,
          (IF CURSYM!* EQ '!' THEN CURSYM!* := LIST('QUOTE, RLISPREAD())
	    ELSE IF (X:=GET(CURSYM!*,'NeWNAM!-OP))THEN
	       <<IF X EQ '!*SEMICOL!* THEN SEMIC!* := CURSYM!*;
	         CURSYM!*:=X >> );
        IF (X:=(GET(CURSYM!*,'NEWNAM))) THEN CURSYM!*:=X;
	IF CURSYM!* EQ 'COMMENT THEN
	<<  WHILE NOT (READCH() MEMQ '(!; !$)) DO ; GOTO A >>;
	RETURN CURSYM!*;
   END;

SYMBOLIC PROCEDURE RESETPARSER;
  CURSYM!*:= '! ;

%-----------------------------------------------------------------
%--- Boot strap functions, move to build file-----;

FLUID '(	%. Name of Grammer being defined
	 DEFPREFIX
	 DEFINFIX
	 GRAMPREFIX
	 GRAMINFIX
);	%. Name of grammer running


DEFPREFIX := 'RLISPPREFIX;	%. Key for Grammer being defined
DEFINFIX := 'RLISPINFIX;	%. Key for Grammer being defined
GRAMPREFIX := 'RLISPPREFIX;	%. Key for Grammer being defined
GRAMINFIX := 'RLISPINFIX;	%. Key for Grammer being defined


SYMBOLIC FEXPR PROCEDURE DEFINEBOP U;
 DEFINEBOPX U;

SYMBOLIC PROCEDURE DEFINEBOPX U; 
% u=(opname, lprec, rprec,function)
   BEGIN SCALAR W,Y; 
      W := EVAL CAR U; % Opname; Remove ' which used to suppress OP props
      Y := 
       EVAL CADR U	% Lprec
         . EVAL CADDR U	% Rprec
             . IF NULL CDDDR U THEN NIL	% Default function is NIL
                ELSE IF ATOM CADDDR U THEN CADDDR U
                ELSE LIST('LAMBDA,'(X Y),CADDDR U); 
      PUT(W,DEFINFIX,Y)	% Binop in CAR
   END;

SYMBOLIC PROCEDURE INFIXOP U;	% Used also in REDUCE
  GET(U,GRAMINFIX);

SYMBOLIC PROCEDURE INFIXPREC U;	% Used in REDUCE MathPrint
  BEGIN SCALAR V;
	IF NULL(V:=INFIXOP U) THEN RETURN NIL;
	IF PAIRP V AND NUMBERP CAR V THEN RETURN CAR V;
	RETURN NIL;
  END;

SYMBOLIC FEXPR PROCEDURE DEFINEROP U; 
  DEFINEROPX U;

SYMBOLIC PROCEDURE DEFINEROPX U;
% u=(opname,lprec,function)
   BEGIN SCALAR W,Y; 
      W := EVAL CAR U; 			% Name, remove ' mark
      Y := 
       EVAL CADR U	 		% Lprec
         . IF NULL CDDR U THEN NIL	% Default is NIL
            ELSE IF ATOM CADDR U THEN CADDR U	% function name
            ELSE LIST('LAMBDA,'(X),CADDR U); % 
      PUT(W,DEFPREFIX,Y)
   END;

SYMBOLIC PROCEDURE PREFIXOP U;
 GET(U,GRAMPREFIX);

FLUID '(OP);			%. Current TOKEN being studied

% ***** General Parser Functions *****; 

SYMBOLIC PROCEDURE PARSE0(RP,PRESCAN);  %. Collect Phrase to LP<RP
   BEGIN SCALAR CURSYM,U;
%/      IF COMPR!* AND CURSYM!* EQ CAAR COMPR!*
%/        THEN <<CURSYM := CAR COMPR!*; COMPR!* := CDR COMPR!*>>; 
      OP := IF PRESCAN THEN SCAN() ELSE CURSYM!*; 
%/      IF PRESCAN AND COMPR!* AND CURSYM!* EQ CAAR COMPR!*
%/        THEN <<CURSYM := CAR COMPR!*; COMPR!* := CDR COMPR!*>>; 
      U := RDRIGHT(RP,OP); 
%/      IF CURSYM THEN RPLACA(CURSYM,U); 
      RETURN U
   END;

SYMBOLIC PROCEDURE RDRIGHT(RP,Y); 	%. Collect phrase until OP with LP<RP
% Y is starting TOKEN.
% RP=NIL - Caller applies Function to Y, without collecting RHS subphrase
   BEGIN SCALAR TEMP,OP1,TEMPSCAN, TEMPOP, !*InsideStructureRead;
	!*InsideStructureRead := T;
      IF NULL RP THEN RETURN Y
 %/       ELSE IF IDFLAG THEN OP := SCAN()	% Set IDFLAG if not Operator
       ELSE IF RP=0 AND Y EQ '!*SEMICOL!* THEN RETURN NIL %/ Toplevel ; or $?
       ELSE IF  (TEMP:=PREFIXOP Y)
        THEN
	<<  TEMPSCAN := SCAN();
	    IF STRONGERINFIXOP(TEMPSCAN, Y, CAR TEMP) THEN
		OP := TEMPSCAN
	    ELSE
		Y := DOPREFIX(CDR TEMP,Y,RDRIGHT(CAR TEMP,TEMPSCAN)) >>
       ELSE IF NOT INFIXOP Y THEN OP := SCAN()
	%/ Binary OP in Prefix Position
       ELSE IF ISOPOP(OP,RP,Y) THEN <<OP := Y; Y := NIL>>
       ELSE OP := SCAN();% Y:=DoINFIXasPREFIX(Y,OP:=SCAN());
    RDLEFT: 
      IF 	%/IDFLAG OR
         NOT (TEMP := INFIXOP OP)
        THEN IF NULL OP 
	       THEN <<Y := LIST(Y,NIL); OP := SCAN()>>
              ELSE Y := REPCOM(Y,RDRIGHT(99,OP))  %. Do as PREFIX
       ELSE IF RP>CAR TEMP THEN RETURN Y
       ELSE <<OP1:=OP;  %/ !*ORD PROBLEM?
	      TEMPSCAN := SCAN();
	      IF TEMPSCAN = '!*LPAR!* AND NOT FUNBOUNDP OP1 THEN
	      <<  OP := TEMPSCAN;	%/ kludge to allow infix/prefix
		  TEMPSCAN := RDRIGHT(CADR TEMP, OP);
		  IF EQCAR(TEMPSCAN, '!*COMMA!*) THEN
		    Y := LIST(Y, REPCOM(OP1, TEMPSCAN))
		  ELSE Y := DOINFIX(CDDR TEMP,Y,OP1,TEMPSCAN) >>
	      ELSE IF STRONGERINFIXOP(TEMPSCAN, OP1, CADR TEMP) THEN
	      <<  Y := LIST(Y, OP1);
		  OP := TEMPSCAN >>
	      ELSE
	         Y := DOINFIX(CDDR TEMP,Y,OP1,RDRIGHT(CADR TEMP,TEMPSCAN))>>;
      GO TO RDLEFT
   END;

SYMBOLIC PROCEDURE STRONGERINFIXOP(NEXTOP, LASTOP, LASTPREC);
BEGIN SCALAR TEMPOP, MATCHER;
   RETURN NOT PREFIXOP NEXTOP
		    AND (TEMPOP := INFIXOP NEXTOP)
		    AND NUMBERP LASTPREC AND NUMBERP CAR TEMPOP
		    AND CAR TEMPOP <= 6
		    AND CAR TEMPOP <= LASTPREC
		    AND NOT ((MATCHER := GET(LASTOP, 'CLOSER))
				AND MATCHER EQ NEXTOP)
		    AND NOT ISOPOP(NEXTOP, LASTPREC, LASTOP);
END;

DefList('((BEGIN END)
	  (!*LPAR!* !*RPAR!*)
	  (!*LSQB!* !*RSQB!*)
	  (!*LVEC!* !*RVEC!*)), 'CLOSER);

SYMBOLIC PROCEDURE DoINFIXasPREFIX(LHS,BOP);
  REPCOM(LHS,RDRIGHT(99,BOP));

%. Note that PREFIX functions have next token SCANed, and get an argument,
%. "X", that is either this TOKEN, or a complete parsed Phrase

SYMBOLIC PROCEDURE DOPREFIX(ACT,ROP,RHS);
  IF NULL ACT THEN LIST(ROP,RHS)
   ELSE APPLY(ACT,LIST RHS);

%. Note that INFIX functions have next token SCANed, and get two arguments,
%. "X" and "Y"; "X" is LHS phrase,
%.  "Y"  is either the scanned TOKEN, or a complete parsed Phrase

SYMBOLIC PROCEDURE DOINFIX(ACT,LHS,BOP,RHS);
 IF NULL ACT THEN LIST(BOP,LHS,RHS)
   ELSE APPLY(ACT,LIST(LHS,RHS));

SYMBOLIC PROCEDURE ISOPOP(XOP,RP,Y); 	%. Test for legal OP-> <-OP
   IF RP=2 THEN Y EQ '!*RPAR!*		% LPAR scans for LP 2
    ELSE IF RP=0 AND XOP EQ 'END
		AND Y MEMBER '(!*SEMICOL!* !*COLON!* !*RSQB!* END) THEN T
    ELSE IF Y MEMQ '(!*SEMICOL!* END !*RSQB!*)	% Special cases in BEGIN-END
     THEN RP= -2 OR XOP MEMQ '(!*SEMICOL!* !*COLON!* !*RSQB!*)
    ELSE NIL;

SYMBOLIC PROCEDURE PARERR(X,Y); 
    StdError X;

SYMBOLIC PROCEDURE REMCOM X; 		%. (, x y z) -> (x y z)
   IF EQCAR(X,'!*COMMA!*) THEN CDR X ELSE LIST X;

SYMBOLIC PROCEDURE REMSEMICOL X; 	%. (; x y z) -> (x y z)
   IF EQCAR(X,'!*SEMICOL!*) THEN CDR X ELSE LIST X;

SYMBOLIC PROCEDURE REPCOM(TYPE,X); 	%.  Create ARGLIST
   IF EQCAR(X,'!*COMMA!*) THEN  (TYPE . CDR X)
    ELSE IF X EQ '!*EMPTY!* THEN LIST(TYPE)
    ELSE LIST(TYPE,X);

%SYMBOLIC PROCEDURE SELF RHS;		%. Prefix Operator returns RHS
%  RHS;

SYMBOLIC PROCEDURE ParseNOOP X;
  <<OP:=SCAN();X>>;

DEFINEROP('NOOP,NIL,ParseNOOP);	%. Prevent TOKEN from being an OP

SYMBOLIC PROCEDURE MKQUOTLIST U; 
   %this could be replaced by MKQUOTE in most cases;
   'LIST
     . FOR EACH X IN U COLLECT IF CONSTANTP X THEN X ELSE MKQUOTE X;

SYMBOLIC PROCEDURE NARY(XOP,LHS,RHS); 	%. Remove repeated NARY ops
   IF EQCAR(LHS,XOP) THEN ACONC(LHS,RHS) ELSE LIST(XOP,LHS,RHS);

% ***** Tables for Various Infix Operators *****; 

SYMBOLIC PROCEDURE ParseCOMMA(X,Y);
   NARY('!*COMMA!*,X,Y);

DEFINEBOP('!*COMMA!*,5,6,ParseCOMMA );

SYMBOLIC PROCEDURE ParseSEMICOL(X,Y);
   NARY('!*SEMICOL!*,X,Y);

DEFINEBOP('!*SEMICOL!*, - 1,0,ParseSEMICOL );

SYMBOLIC PROCEDURE ParseSETQ(LHS,RHS); %. Extended SETQ
  LIST(IF ATOM LHS THEN 'SETQ ELSE 'SETF, LHS, RHS);

DEFINEBOP('SETQ,7,6,ParseSETQ);

DEFINEBOP('CONS,23,21);

SYMBOLIC PROCEDURE ParsePLUS2(X,Y);
 NARY('PLUS,X,Y);

DEFINEBOP('PLUS,17,18,ParsePLUS2);

%SYMBOLIC PROCEDURE ParsePLUS1(X);
%  IF EQCAR(X,'!*COMMA!*) THEN REPCOM('PLUS,X) ELSE X;
%
%DEFINEROP('PLUS,26,ParsePLUS1);	%/ **** Prefix + sign...

DEFINEROP('MINUS,26);

SYMBOLIC PROCEDURE ParseDIFFERENCE(X);
  IF NUMBERP X THEN (0 - X )
   ELSE IF EQCAR(X,'!*COMMA!*)
	 THEN REPCOM('DIFFERENCE,X)
   ELSE  LIST('MINUS,X);

DEFINEROP('DIFFERENCE,26,ParseDIFFERENCE );

DEFINEBOP('DIFFERENCE,17,18);

DEFINEBOP('TIMES,19,20);

SYMBOLIC PROCEDURE ParseQUOTIENT(X);
 IF NOT EQCAR(X,'!*COMMA!*) THEN LIST('RECIP,X)
  ELSE REPCOM('QUOTIENT,X);

DEFINEROP('QUOTIENT,26,ParseQUOTIENT);

DEFINEBOP('QUOTIENT,19,20);

DEFINEROP('RECIP,26);

DEFINEBOP('EXPT,23,24);

SYMBOLIC PROCEDURE ParseOR(X,Y);
  NARY('OR,X,Y);

DEFINEBOP('OR,9,10,ParseOR);

%/DEFINEROP('OR,26,REPCOM('OR,X));

SYMBOLIC PROCEDURE ParseAND(X,Y);
  NARY('AND,X,Y);

DEFINEBOP('AND,11,12,ParseAND);

%/DEFINEROP('AND,26,REPCOM('AND,X));

DEFINEROP('NOT,14);

DEFINEBOP('MEMBER,15,16);

%/DEFINEROP('MEMBER,26,REPCOM('MEMBER,X));

DEFINEBOP('MEMQ,15,16);

%/DEFINEROP('MEMQ,26,REPCOM('MEMQ,X));

DEFINEBOP('EQ,15,16);

%/DEFINEROP('EQ,26,REPCOM('EQ,X));

DEFINEBOP('EQUAL,15,16);

DEFINEBOP('GEQ,15,16);

DEFINEBOP('GREATERP,15,16);

DEFINEBOP('LEQ,15,16);

DEFINEBOP('LESSP,15,16);

DEFINEBOP('NEQ,15,16);
DEFINEBOP('NE,15,16);

% ***** Tables and Definitions for Particular Parsing Constructs *****; 

% ***** IF Expression *****; 

DEFINEROP('IF,4,ParseIF);

DEFINEBOP('THEN,3,6);

DEFINEBOP('ELSE,3,6);

SYMBOLIC PROCEDURE ParseIF X; 
   BEGIN SCALAR Y,Z; 
      IF OP EQ 'THEN THEN Y := PARSE0(6,T) ELSE PARERR("IF missing THEN",T); 
      IF OP EQ 'ELSE THEN Z := LIST PARSE0(6,T); 
      RETURN 'COND
               . LIST(X,Y)
                   . IF Z
                       THEN IF EQCAR(CAR Z,'COND) THEN CDAR Z
                             ELSE LIST (T . Z)
                      ELSE NIL
   END;

SYMBOLIC PROCEDURE ParseCASE(X);		%. Parser function
 BEGIN
  IF NOT (OP EQ 'OF) THEN PARERR("CASE Missing OF",T);
  RETURN 'CASE . X . CASELIST()
 END;

DEFINEBOP('OF,3,6);
DEFINEBOP('TO,8,9);
DEFINEROP('CASE,4,ParseCASE);

SYMBOLIC PROCEDURE CASELIST;
 BEGIN SCALAR TG,BOD,TAGLIST,BODLIST;
   L1:  OP := SCAN();		% Drop OF, : , etc
	IF OP EQ 'END THEN GOTO L2;	% For optional ; before END
	TG := PARSETAGS();	% The TAG expressions
        BOD:= PARSE0(6,T);	% The expression
        BODLIST:=LIST(TG,BOD) . BODLIST;
        IF OP EQ '!*SEMICOL!* THEN GOTO L1;
        IF OP NEQ 'END THEN PARERR("Expect END after CASE list",T);
   L2:  OP:=SCAN(); % Skip 'END
        RETURN  REVERSE BODLIST;
 END;

SYMBOLIC PROCEDURE PARSETAGS();
% Collects a single CASE-tag form; OP prescanned
 BEGIN SCALAR TG,TGLST;
	TG:=PARSE0(6,NIL);	% , and : below 6
        IF EQCAR(TG,'TO) THEN TG:='RANGE . CDR TG; % TO is infix OP
	IF TG MEMQ '(OTHERWISE DEFAULT)
	  THEN RETURN <<IF OP NEQ '!*COLON!* 
			  THEN PARERR("OTHERWISE in CASE must be SINGLE tag",T);
			NIL>>;
	IF OP EQ '!*COLON!* THEN RETURN LIST(TG);
	IF OP EQ '!*COMMA!* 
	   THEN RETURN 
		<<OP:=SCAN();
		  TGLST:=PARSETAGS();
	          IF NULL TGLST 
			THEN PARERR("OTHERWISE in CASE must be SINGLE tag",T);
	          TG . TGLST>>;
	PARERR("Expect one or more tags before : in CASE",T);
 END;

% ***** Block Expression *****; 

fluid '(BlockEnders!*);
BlockEnders!* :='(END !*RPAR!* !*SEMICOL!* ELSE UNTIL !*RSQB!*);

SYMBOLIC PROCEDURE ParseBEGIN(X);
           ParseBEGIN1(REMSEMICOL X,
                COMMENTPART(SCAN(),BlockEnders!*));

DEFINEROP('BEGIN,-2,ParseBEGIN);

DEFINEBOP('END,-3,-2);

SYMBOLIC PROCEDURE ParseGO X;
  IF X EQ 'TO THEN LIST('GO,PARSE0(6,T)) % Why not Just SCAN?
           ELSE <<OP := SCAN(); LIST('GO,X)>>;

DEFINEROP('GO,NIL,ParseGO );

SYMBOLIC PROCEDURE ParseGOTO X;
  <<OP := SCAN(); LIST('GO,X)>>;

DEFINEROP('GOTO,NIL,ParseGOTO );

SYMBOLIC PROCEDURE ParseRETURN X;
Begin Scalar XOP;
           RETURN LIST('RETURN,
               IF (XOP := INFIXOP X) AND NUMBERP CAR XOP AND CAR XOP <= 1
	       THEN <<OP := X; NIL>> ELSE RDRIGHT(6,X));
END;

DEFINEROP('RETURN,NIL,ParseRETURN);

SYMBOLIC PROCEDURE ParseEXIT X;
Begin Scalar XOP;
           RETURN LIST('EXIT,
               IF (XOP := INFIXOP X) AND NUMBERP CAR XOP AND CAR XOP <= 1
	       THEN <<OP := X; NIL>> ELSE RDRIGHT(6,X));
END;

DEFINEROP('EXIT,NIL,ParseEXIT);

DEFINEBOP('!*COLON!*,1,0 );

SYMBOLIC PROCEDURE COMMENTPART(A,L); 
   IF A MEMQ L THEN <<OP := A; NIL>>
    ELSE A . COMMENTPART(SCAN(),L);

SYMBOLIC PROCEDURE ParseBEGIN1(L,COMPART); 
   BEGIN SCALAR DECLS,S; 
    % Look for Sequence of Decls after Block Header
  A:  IF NULL L THEN GO TO ND
%/      SCAN();
%/      IF CURSYM!* MEMQ '(INTEGER REAL SCALAR)
%/	THEN <<Z1:=REPCOM(CURSYM!*,PARSE0(0,NIL))>>; % Arg Decl;
       ELSE IF NULL CAR L THEN <<L := CDR L; GO TO A>>
       ELSE IF EQCAR(CAR L,'DECLARE)
        THEN <<DECLS :=APPEND(CDAR L, DECLS); % Reverse order collection
               L := CDR L>>
       ELSE <<S:=L; GO TO B>>;	% Hold Body for Rescan
      GO TO A; 
  B:  IF NULL L THEN GO TO ND
       ELSE IF EQCAR(CAR L,'DECLARE)
        THEN PARERR("DECLARATION invalid in BEGIN body",NIL)
       ELSE IF EQCAR(CAR L,'!*COLON!*)
        THEN <<RPLACD(CDDAR L,CDR L); 
               RPLACD(L,CDDAR L); 
               RPLACA(L,CADAR L)>>
       ELSE IF CDR L AND NULL CADR L
        THEN <<RPLACD(L,CDDR L); L := NIL . L>>; 
      L := CDR L; 
      GO TO B;
 ND:  RETURN ('PROG . MAKELOCALS(DECLS) . S);
   END;

SYMBOLIC PROCEDURE MAKELOCALS(U);	%. Remove Types from Reversed DECLARE
 IF NULL U THEN NIL
  ELSE APPEND(CDAR U,MAKELOCALS CDR U);

% ***** Procedure Expression *****; 

GLOBAL '(!*MODE);

!*MODE := 'SYMBOLIC;

SYMBOLIC PROCEDURE NMODESTAT VV;	% Parses TOP-LEVEL mode ....;
   BEGIN SCALAR TMODE,X;
	X:= CURSYM!*;
	% SCAN();
	IF CURSYM!* EQ '!*SEMICOL!* 
	  THEN RETURN <<NEWMODE VV;
                        OP:='!*SEMICOL!*;NIL>>;
        IF FLAGP(CURSYM!*,'DELIM) 
	  THEN RETURN <<NEWMODE VV;
                        OP:='!*SEMICOL!*;NIL>>;
	TMODE := !*MODE;
	!*MODE := VV;  % Local MODE change for MKPROC
	X := ERRORSET('(PARSE0 0 NIL),T,!*BACKTRACE);
	!*MODE := TMODE;
	RETURN IF ATOM X OR CDR X THEN NIL ELSE CAR X
   END;

SYMBOLIC PROCEDURE NEWMODE VV;
 <<PRINT LIST('NEWMODE,LIST('QUOTE,VV)); 
   IF NULL VV THEN VV:='SYMBOLIC;
   !*MODE := VV>>;

CommentOutCode <<
fluid '(FTypes!*);
FTYPES!* := '(EXPR FEXPR MACRO);

SYMBOLIC PROCEDURE OLDPROCSTAT;
   BEGIN SCALAR BOOL,U,TYPE,X,Y,Z;
	IF FNAME!* THEN GO TO B
	 ELSE IF CURSYM!* EQ 'PROCEDURE THEN TYPE := 'EXPR
	 ELSE PROGN(TYPE := CURSYM!*,SCAN());
	IF NOT CURSYM!* EQ 'PROCEDURE THEN GO TO C;
	X := ERRORSET('(PARSE0 0 T),T,!*BACKTRACE);
	IF ATOM X OR CDR X THEN GO TO A
	 ELSE IF ATOM (X := CAR X) THEN X := LIST X;   %no arguments;
	FNAME!* := CAR X;   %function name;
	IF IDP FNAME!* %AND NOT(TYPE MEMQ FTYPES!*);
	  THEN IF NULL FNAME!* OR (Z := GETTYPE FNAME!*)
			AND NOT Z MEMQ '(PROCEDURE OPERATOR)
		THEN GO TO D
	      ELSE IF NOT GETD FNAME!* THEN FLAG(LIST FNAME!*,'FNC);
	   %to prevent invalid use of function name in body;
	U := CDR X;
	Y := ERRORSET(LIST('FLAGTYPE,MKQUOTE U,MKQUOTE 'SCALAR),
		      T,!*BACKTRACE);
	IF ATOM Y OR CDR Y THEN Y := NIL ELSE Y := CAR Y;
	X := CAR X . Y;
    A:	Z := ERRORSET('(PARSE0 0 T),T,!*BACKTRACE);
	IF NOT ATOM Z AND NULL CDR Z THEN Z := CAR Z;
	IF NULL ERFG!* THEN Z:=PROCSTAT1(X,Z,TYPE);
	REMTYPE Y;
	REMFLAG(LIST FNAME!*,'FNC);
	FNAME!*:=NIL;
	IF NOT BOOL AND ERFG!* THEN REDERR "ERROR TERMINATION";
	RETURN Z;
    B:	BOOL := T;
    C:	ERRORSET('(SYMERR (QUOTE PROCEDURE) T),T,!*BACKTRACE);
	GO TO A;
    D:	LPRIE LIST(Z,FNAME!*,"INVALID AS PROCEDURE");
	GO TO A
   END;
>>;
% Some OLD Crap looks at 'STAT values!!!

DEFLIST ('((PROCEDURE PROCSTAT) 
	   (EXPR PROCSTAT) 
	   (FEXPR PROCSTAT)
	   (EMB PROCSTAT)
	   (MACRO PROCSTAT) (NMACRO PROCSTAT) (SMACRO PROCSTAT)),
	'STAT);

DEFLIST ('((ALGEBRAIC MODESTAT) 
           (SYMBOLIC MODESTAT)
	   (SYSLSP MODESTAT)
	),
	 'STAT);	 %/ STAT used for OLD style BEGIN KEY search

DEFLIST('((LISP SYMBOLIC)),'NEWNAM);

DEFINEROP('SYMBOLIC,NIL,NMODESTAT('SYMBOLIC));	% Make it a Prefix OP
DEFINEROP('ALGEBRAIC,NIL,NMODESTAT('ALGEBRAIC));	% Make it a Prefix OP
DEFINEROP('SYSLSP,NIL,NMODESTAT('SYMBOLIC));	% Make it a Prefix OP
DEFINEBOP('PROCEDURE,1,NIL,ParsePROCEDURE);	% Pick up MODE -- will go

DEFINEROP('PROCEDURE,NIL,ParsePROCEDURE('EXPR,X));	%/ Unary, use DEFAULT mode?

SYMBOLIC PROCEDURE ParsePROCEDURE2(NAME,VARLIS,BODY,TYPE);
   BEGIN SCALAR Y;
%	IF FLAGP(NAME,'LOSE) AND (!*LOSE OR NULL !*DEFN)
%	  THEN RETURN PROGN(LPRIM LIST(NAME,
%			    "Not defined (LOSE Flag)"),
%			NIL);
	if (Y := get(Type, 'FunctionDefiningFunction)) then
	    Body := list(Y, Name, VarLis, Body)
	else if (Y := get(Type, 'ImmediateDefiningFunction)) then return
	    Apply(Y, list(Name, VarLis, Body))
	 ELSE BODY := LIST('PUTC,
			   MKQUOTE NAME,
			   MKQUOTE TYPE,
			   MKQUOTE LIST('LAMBDA,VARLIS, REFORM BODY));
	RETURN IF !*MODE NEQ 'ALGEBRAIC THEN BODY
%/		ELSE LIST('PROGN,
%/			 LIST('FLAG,MKQUOTE LIST NAME,MKQUOTE 'OPFN),
%/			  BODY)
   END;


DefList('((Expr DE)
	  (FExpr DF)
	  (Macro DM)
	  (NExpr DN)
	  (SMacro DS)), 'FunctionDefiningFunction);

put('Emb, 'ImmediateDefiningFunction, 'EmbFn);

SYMBOLIC PROCEDURE ParsePROCEDURE1(NAM,ARGS,BODY,ARGTYPE,TYPES);
%/ Crude conversion of PROC to PUTD. Need make Etypes and Ftypes
%/  Keywords also.
  BEGIN SCALAR ETYPE,FTYPE;
	ETYPE:=!*MODE; FTYPE:='EXPR;
	IF NOT PAIRP TYPES THEN TYPES:=TYPES . NIL;
	FOR EACH Z IN TYPES DO
	 IF FLAGP(Z,'ETYPE) THEN ETYPE:=Z
	  ELSE IF FLAGP(Z,'FTYPE) THEN FTYPE:=Z;
    	RETURN ParsePROCEDURE2(NAM,ARGS,BODY,FTYPE);
   END;

FLAG('(EXPR FEXPR NEXPR NFEXPR MACRO SMACRO NMACRO EMB),'FTYPE);
FLAG('(SYMBOLIC ALGEBRAIC LISP SYSLISP SYSLSP),'ETYPE);

SYMBOLIC PROCEDURE ParsePROCEDURE(EFTYPES,Y); 
   BEGIN SCALAR OP1,Z,Z1; 
      OP := OP1 := SCAN(); 
      IF OP1 EQ '!*SEMICOL!* THEN Y := LIST Y
       ELSE IF INFIXOP OP1 THEN Y := LIST(OP1,Y,PARSE0(8,T))	
		% Binary as Prefix
       ELSE Y := REPCOM(Y,PARSE0(8,NIL)); %/ Why 8
      IF OP NEQ '!*SEMICOL!* 
	THEN PARERR("PROCEDURE missing terminator after template",T); 
%/      SCAN();
%/      IF CURSYM!* MEMQ '(INTEGER REAL SCALAR)
%/	THEN <<Z1:=REPCOM(CURSYM!*,PARSE0(0,NIL))>>; % Arg Decl;
      Z := PARSE0(0,T); 
      IF EQCAR(Z,'DECLARE) THEN <<Z1 := Z; Z := PARSE0(0,T)>>; % repeated DECL?
      RETURN ParsePROCEDURE1(CAR Y,CDR Y,Z,Z1,EFTYPES);
			% Nam, args, body, arg decl, E/Fmode
   END;

% ***** Left and Right Parentheses Handling *****; 

DEFINEROP('!*LPAR!*,NIL,ParseLPAR);

DEFINEBOP('!*RPAR!*,1,0);

SYMBOLIC PROCEDURE ParseLPAR X; 
   BEGIN SCALAR RES; 
       IF X EQ '!*RPAR!* THEN <<OP := X; RES := '!*EMPTY!*>>
        ELSE RES:= RDRIGHT(2,X);
      IF OP EQ '!*RPAR!* THEN OP := SCAN()
       ELSE PARERR("Missing ) after argument list",NIL); 
      RETURN RES
   END;

% ***** Left and Right << and >> Handling *****; 

DEFINEROP('!*LSQB!*,-2,ParseRSQB);
SYMBOLIC PROCEDURE ParseRSQB(X);
          IF OP EQ '!*RSQB!*
            THEN <<OP := SCAN(); 'PROGN . REMSEMICOL X>>
           ELSE PARERR("Missing right >> after Group",NIL);

DEFINEBOP('!*RSQB!*,-3,0);

%COMMENT ***** [] vector syntax;

REMPROP('![,'NEWNAM);
REMPROP('!],'NEWNAM);

% ***** [] vector syntax;

DEFINEBOP('!*LVEC!*,121,6,ParseLVEC);

SYMBOLIC PROCEDURE ParseLVEC(X,Y);
 IF OP EQ '!*RVEC!* THEN <<OP :=SCAN(); LIST('INDX,X,Y)>>
  ELSE  PARERR("Missing ] in index expression ",NIL);

% INDX is used for both Vectors and Strings in PSL.  You will need to
% have INDX map to GETV in vanilla Standard Lisp

DEFINEBOP('!*RVEC!*,5,7);

% ***** Lambda Expression *****; 

DEFINEROP('LAMBDA,0,ParseLAMBDA);
SYMBOLIC PROCEDURE ParseLAMBDA X;
          LIST('LAMBDA,IF X AND X NEQ '!*EMPTY!* THEN REMCOM X ELSE NIL,
	       PARSE0(6,T));

% ***** Repeat Expression *****; 

DEFINEROP('REPEAT,4,ParseREPEAT);
SYMBOLIC PROCEDURE ParseREPEAT X;
          LIST('REPEAT,X,
               IF OP EQ 'UNTIL THEN PARSE0(6,T)
                ELSE PARERR("REPEAT missing UNTIL clause",T)) ;

DEFINEBOP('UNTIL,3,6);

% ***** While Expression *****; 

DEFINEROP('WHILE,4, ParseWHILE);

SYMBOLIC PROCEDURE ParseWHILE X;
          LIST('WHILE,X,
               IF OP EQ 'DO THEN PARSE0(6,T) 
	        ELSE PARERR("WHILE missing DO clause",T)) ;

DEFINEBOP('DO,3,6);

% ***** Declare Expression *****; 

DEFINEROP('DECLARE,2,ParseDECL);

DEFINEROP('DCL,2,ParseDECL);

SYMBOLIC PROCEDURE ParseDECL X; 
   BEGIN SCALAR Y,Z; 
    A: 
      IF OP NEQ '!*COLON!* THEN PARERR("DECLARE needs : before mode",T); 
      IF (Z := SCAN()) MEMQ '(INTEGER REAL SCALAR) THEN OP := SCAN()
       ELSE Z := PARSE0(6,NIL); 
      Y := ACONC(Y,Z . REMCOM X); 
      IF OP EQ '!*SEMICOL!* THEN RETURN 'DECLARE . Y
       ELSE IF OP NEQ '!*COMMA!* 
	THEN PARERR("DECLAREd variables separated by ,",T); 
      X := PARSE0(2,T); 
      GO TO A
   END;

SYMBOLIC FEXPR PROCEDURE DECLARE U; 
   %to take care of top level declarations;
   <<LPRIM "Declarations are not permitted at the top level";
     NMODESTAT U>>;

% ***** For Expression *****; 

DEFINEROP('FOR,NIL,ParseFOR);

DEFINEBOP('STEP,3,6);

DEFINEBOP('SUM,3,6);

DEFINEBOP('PRODUCT,3,6);

SYMBOLIC PROCEDURE ParseFOR X; 
   BEGIN SCALAR INIT,STP,UNTL,ACTION,ACTEXPR; 
      IF X EQ 'EACH THEN RETURN ParseFOREACH SCAN()
       ELSE IF X EQ 'ALL THEN RETURN ParseFORALL PARSE0(4,T)
       ELSE IF (OP := SCAN()) EQ 'SETQ THEN INIT := PARSE0(6,T)
       ELSE PARERR("FOR missing loop VAR assignment",T); 
      IF OP EQ '!*COLON!* THEN <<STP := 1; OP := 'UNTIL>>
       ELSE IF OP EQ 'STEP THEN STP := PARSE0(6,T)
       ELSE PARERR("FOR missing : or STEP clause",T); 
      IF OP EQ 'UNTIL THEN UNTL := PARSE0(6,T) 
	ELSE PARERR("FOR missing UNTIL clause",T); 
      ACTION := OP; 
      IF ACTION MEMQ '(DO SUM PRODUCT) THEN ACTEXPR := PARSE0(6,T)
       ELSE PARERR("FOR missing action keyword",T); 
      RETURN LIST('FOR,
                  LIST('FROM,X,INIT,UNTL,STP),
		  LIST(ACTION,ACTEXPR))
   END;

% ***** Foreach Expression *****; 

DEFINEROP('FOREACH,NIL,ParseFOREACH);

DEFINEBOP('COLLECT,3,6);
DEFINEBOP('CONC,3,6);
DEFINEBOP('JOIN,3,6);

SYMBOLIC PROCEDURE ParseFOREACH X; 
   BEGIN SCALAR L,INON,ACTION; 
      IF NOT ((INON := SCAN()) EQ 'IN OR INON EQ 'ON)
        THEN PARERR("FOR EACH missing iterator clause",T); 
      L := PARSE0(6,T); 
      IF NOT ((ACTION := OP) MEMBER '(DO COLLECT CONC JOIN))
        THEN PARERR("FOR EACH missing action clause",T); 
      RETURN LIST('FOREACH,X,INON,L,ACTION,PARSE0(6,T))
   END;

% ***** Let Expression *****; 

DEFINEBOP('LET,1,0,ParseLET);

DEFINEROP('LET,0,ParseLET(NIL . NIL,X) );

DEFINEBOP('CLEAR,0,1,ParseCLEAR);

DEFINEROP('CLEAR,0,ParseCLEAR(NIL . NIL,X));

DEFINEBOP('SUCH,3,6);

SYMBOLIC PROCEDURE ParseLET(X,Y); ParseLET1(X,Y,NIL);

SYMBOLIC PROCEDURE ParseCLEAR(X,Y); ParseLET1(X,Y,T);

SYMBOLIC PROCEDURE ParseLET1(X,Y,Z); 
   LIST('LET!*,CAR X,REMCOM Y,CDR X,NIL,Z);

SYMBOLIC PROCEDURE ParseFORALL X; 
   BEGIN SCALAR BOOL; 
      IF OP EQ 'SUCH
        THEN IF SCAN() EQ 'THAT THEN BOOL := PARSE0(6,T)
              ELSE PARERR("FOR ALL missing SUCH THAT clause",T); 
      IF NOT OP MEMQ '(LET CLEAR) THEN PARERR("FOR ALL missing ACTION",T); 
      RETURN REMCOM X . BOOL
   END;

% ******** Standard Qoted LIST collectors

SYMBOLIC PROCEDURE RLISF(U,V,W); 	%. Used to Collect a list of IDs to
					%. FLAG with Something
   BEGIN 
      V := RDRIGHT(0,V); 
      V := 
       IF EQCAR(V,'!*COMMA!*) THEN CDR V
        ELSE IF V THEN LIST V
        ELSE V; 
      RETURN FLAG(V,U)
   END;

SYMBOLIC PROCEDURE FLAGOP U; 		%. Declare U as Flagger
   RLISTAT(U,'FLAGOP);

SYMBOLIC PROCEDURE RLISTAT(OPLIST,B); 	%. Declare els of OPLIST to be RLIS
   FOR EACH U IN OPLIST DO 
      DEFINEROPX LIST(MKQUOTE U,NIL,
                        LIST(IF B EQ 'FLAGOP THEN 'RLISF ELSE 'RLIS1,
                             MKQUOTE U,'X,MKQUOTE B));
      
SYMBOLIC PROCEDURE RLIS1(U,V,W); 	%. parse LIST of args, maybe quoted
 % U=funcname, V=following Phrase, W=arg treatment
   BEGIN 
      IF V EQ '!*SEMICOL!* THEN RETURN
      <<OP := V;
        IF W = 'NOQUOTE THEN LIST U ELSE LIST(U, NIL) >>
       ELSE V := RDRIGHT(0,V); 
      V := 
       IF EQCAR(V,'!*COMMA!*) THEN CDR V
        ELSE IF V THEN LIST V
        ELSE V; 
      IF W EQ 'IO
        THEN V := MAPCAR(V,FUNCTION (LAMBDA J; NEWMKFIL J)); 
      RETURN IF W EQ 'NOQUOTE THEN U . V ELSE LIST(U,MKQUOTLIST V)
   END;

% ***** Parsing Rules For Various IO Expressions *****; 

RLISTAT('(IN OUT SHUT),'NOQUOTE);
RLISTAT('(TR UNTR BR UNBR),'NOQUOTE);	% for mini-trace in PSL

RLISTAT('(LOAD HELP), 'NOQUOTE);

FLAG('(IN OUT SHUT ON OFF
      TR UNTR UNTRST TRST),'NOCHANGE); % No REVAL of args
DEFINEROP('FSLEND,NIL,ESTAT('FasLEND));
DEFINEROP('FaslEND,NIL,ESTAT('FaslEND));

RLISTAT('(WRITE),'NOQUOTE);

RLISTAT('(ARRAY),1);

%		       2.11.3 ON/OFF STATEMENTS

RLISTAT('(ON OFF), 'NOQUOTE);

% ***** Parsing Rules for INTEGER/SCALAR/REAL *****; 

% These will eventually be removed in favor of DECLARE; 

DEFINEROP('INTEGER,0,ParseINTEGER);

SYMBOLIC PROCEDURE ParseINTEGER X;
  LIST('DECLARE,REPCOM('INTEGER,X));

DEFINEROP('REAL,0,ParseREAL);

SYMBOLIC PROCEDURE ParseREAL X;
 LIST('DECLARE,REPCOM('REAL,X));

DEFINEROP('SCALAR,0,ParseSCALAR);

SYMBOLIC PROCEDURE ParseSCALAR X;
LIST('DECLARE,REPCOM('SCALAR,X));

%/ Cuase problems in INTEGER procedure foo;...

SYMBOLIC PROCEDURE COMM1 U; 	%. general Comment Parser
   BEGIN 
      IF U EQ 'END THEN SCAN();
    A: 
      IF CURSYM!* EQ '!*SEMICOL!*
           OR U EQ 'END
                AND CURSYM!*
                      MEMQ '(END ELSE UNTIL !*RPAR!* !*RSQB!*)
        THEN RETURN NIL; 
	SCAN();
        GOTO A;
   END;

SYMBOLIC PROCEDURE ESTAT(FN);	%. returns (FN), dropping till semicol ;
 BEGIN
     	WHILE CURSYM!* NEQ '!*SEMICOL!* DO SCAN();
	OP := '!*SEMICOL!*;
     	RETURN LIST(FN);
 END;

SYMBOLIC PROCEDURE ENDSTAT;
  %This procedure can also be used for any key-words  which  take  no
  %arguments;
   BEGIN SCALAR X;
	X := OP;
	COMM1 'END;
        OP := '!*SEMICOL!*;
	RETURN LIST X
   END;

% Some useful ESTATs:

DEFINEROP('QUIT,NIL,ESTAT('QUIT));
DEFINEROP('PAUSE,NIL,ESTAT('PAUSE));
DEFINEROP('CONT,NIL,ESTAT('CONT));
DEFINEROP('RECLAIM,NIL,ESTAT('RECLAIM));
DEFINEROP('RETRY,NIL,ESTAT('RETRY));
DEFINEROP('SHOWTIME,NIL,ESTAT('SHOWTIME));

FLAG('(FSLEND CONT RECLAIM RETRY SHOWTIME QUIT PAUSE),'OPFN);
% Symbolic OPS, or could use NOCHANGE
RLISTAT('(FLAGOP),1);

CommentOutCode <<
SYMBOLIC PROCEDURE INFIX X;  % Makes Left ASSOC, not like CONS
  FOR EACH Y IN X DO
	DEFINEBOPX LIST(MKQUOTE Y,8,9,NIL);
>>;

FLAG('(NEWTOK),'EVAL);

SYMBOLIC PROCEDURE PRECEDENCE U; 
  PRECSET(CAR U,CADR U);

SYMBOLIC PROCEDURE PRECSET(U,V); 
   BEGIN SCALAR Z; 
      IF NULL (Z := INFIXOP V) OR NULL (Z := CDR Z)
        THEN REDERR LIST(V,"NOT INFIX")
       ELSE DEFINEBOPX LIST(MKQUOTE U,CAR Z,CADR Z,NIL)
   END;

RLISTAT('(INFIX PRECEDENCE),3);

REMPROP('SHOWTIME,'STAT);
%*********************************************************************
%			   DEFINE STATEMENT
%********************************************************************;

SYMBOLIC PROCEDURE ParseDEFINE(X);	% X is following Token
   BEGIN SCALAR Y,Z;
     B:	IF X EQ '!*SEMICOL!* THEN RETURN <<OP:='!*SEMICOL!*;
					     MKPROG(NIL,Z)>>
	 ELSE IF X EQ '!*COMMA!* THEN <<X:=SCAN();	%/ Should use SCAN0
					GO TO B>>
	 ELSE IF NOT IDP X THEN GO TO ER;
	Y := SCAN();
	IF NOT (Y EQ 'EQUAL) THEN GO TO ER;
	Z := ACONC(Z,LIST('PUT,MKQUOTE X,MKQUOTE 'NEWNAM,
				MKQUOTE PARSE0(6,T))); % So doesnt include ,
	X := CURSYM!*;
	GO TO B;
    ER: SYMERR('DEFINE,T)
   END;

DEFINEROP('DEFINE,NIL,ParseDEFINE);

FLAG('(DEFINE),'EVAL);


%*********************************************************************
%			 3.2.4 WRITE STATEMENT
%********************************************************************;

SYMBOLIC PROCEDURE ParseWRITE(X);
   BEGIN SCALAR Y,Z;
	X := REMCOM XREAD1 'LAMBDA;
    A:	IF NULL X
	  THEN RETURN MKPROG(NIL,'(TERPRI) . Y);
	Z := LIST('PRIN2,CAR X);
	IF NULL CDR X THEN Z := LIST('RETURN,Z);
    B:	Y := ACONC(Y,Z);
	X := CDR X;
	GO TO A;
   END;

DEFINEROP('WRITE,NIL,ParseWRITE);

%*********************************************************************
%			 VARIOUS DECLARATIONS
%********************************************************************;

SYMBOLIC PROCEDURE ParseOPERATOR(X);
   BEGIN SCALAR Y;
	Y := REMCOM PARSE0(0,NIL);
	RETURN
	 IF !*MODE EQ 'SYMBOLIC
	   THEN MKPROG(NIL,LIST LIST('FLAG,MKQUOTE Y,MKQUOTE 'OPFN))
	  ELSE IF X NEQ 'OPERATOR
	   THEN IF EQCAR(CAR Y,'PROG) THEN CAR Y
		 ELSE X . MAPCAR(LIST Y,FUNCTION MKARG)
	  ELSE IF KEY!* NEQ 'OPERATOR AND GET(KEY!*,'FN)
	   THEN (LAMBDA K; MKPROG(NIL,MAPCAR(Y,FUNCTION (LAMBDA J;
			   LIST('FLAG,LIST('LIST,MKQUOTE J),
					K,K)))))
		MKQUOTE GET(KEY!*,'FN)
	  ELSE MKPROG(NIL,
		      LIST LIST('OPERATOR,MKQUOTE Y))
   END;

SYMBOLIC PROCEDURE OPERATOR U; MAPCAR(U,FUNCTION MKOP);

DEFINEROP('OPERATOR,NIL,ParseOPERATOR);

	%. Diphthongs and READtable Changes

Symbolic Procedure ChangeCharType(TBL,Ch,Ty);	%. Set Character type
begin scalar IDNum;
 If IDP Ch  and (IDNum := ID2Int Ch) < 128 and 
		Numberp Ty and Ty >=0 and Ty <=19 then
  PutV(TBL,IDNum,Ty)
 Else Error(99,"Cant Set ReadTable");
end;

Symbolic Procedure MakeDiphthong(TBL,DipIndicator,StartCh, FollowCh, Diphthong);
 If IDP Startch and IDP FollowCh and IDP Diphthong
  then <<ChangeCharType(TBL,StartCh,13);
         PUT(StartCh,DipIndicator,
             (FollowCh . Diphthong) . GET(StartCh,DipIndicator))>>
 else Error(99, "Cant Declare Diphthong");


SYMBOLIC PROCEDURE MYNEWTOK(X,REPLACE,PRTCHARS);
 BEGIN SCALAR Y;
	PUT(X,'NEWNAM!-OP,REPLACE);
        IF NULL PRTCHARS THEN Y:=LIST(X,X)
	 ELSE IF IDP PRTCHARS THEN Y:=LIST(PRTCHARS,X)
	 ELSE Y:=PRTCHARS;
        PUT(REPLACE,'PRTCH,Y);
 END;

MYNEWTOK('!;,'!*SEMICOL!*,NIL)$
MYNEWTOK('!$,'!*SEMICOL!*,NIL)$
MYNEWTOK('!,,'!*COMMA!*,NIL)$
MYNEWTOK('!.,'CONS,NIL)$
MYNEWTOK('!:!=,'SETQ,'! !:!=! )$
MYNEWTOK('!+,'PLUS,'! !+! )$
MYNEWTOK('!-,'DIFFERENCE,'! !-! )$
MYNEWTOK('!*,'TIMES,NIL)$
MYNEWTOK('!/,'QUOTIENT,NIL)$
MYNEWTOK('!*!*,'EXPT,NIL)$
MYNEWTOK('!^,'EXPT,NIL)$
MYNEWTOK('!=,'EQUAL,NIL)$
MYNEWTOK('!:,'!*COLON!*,NIL)$
MYNEWTOK('!(,'!*LPAR!*,NIL)$
MYNEWTOK('!),'!*RPAR!*,NIL)$
MYNEWTOK('!{,'!*LSQB!*,NIL)$
MYNEWTOK('!},'!*RSQB!*,NIL)$
MYNEWTOK('!<!<,'!*LSQB!*,NIL)$
MYNEWTOK('!>!>,'!*RSQB!*,NIL)$
MYNEWTOK('![,'!*LVEC!*,NIL)$
MYNEWTOK('!],'!*RVEC!*,NIL)$
MYNEWTOK('!<,'LESSP,NIL)$
MYNEWTOK('!<!=,'LEQ,NIL)$
MYNEWTOK('!>!=,'GEQ,NIL)$
MYNEWTOK('!>,'GREATERP,NIL)$

fluid '(RLispScanTable!* RLispReadScanTable!*);
RLispReadScanTable!* := '
[17 11 11 11 11 11 11 11 11 17 17 11 17 17 11 11 11 11 11 11 11 11 11 11 
11 11 11 11 11 11 11 11 17 14 15 11 11 12 11 11 11 11 13 19 11 18 20 11 
0 1 2 3 4 5 6 7 8 9 13 11 13 11 13 11 11 10 10 10 10 10 10 10 10 10 10 
10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 11 16 11 11 10 11 10 10 
10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 
11 11 11 11 11 LispDiphthong];

RLispScanTable!* := TotalCopy RLispReadScanTable!*;
PutV(RLispScanTable!*, 128, 'RLISPDIPHTHONG);

ChangeCharType(RLispScanTable!*, '!-, 11);
ChangeCharType(RLispScanTable!*, '!+, 11);
MAKEDIPHTHONG(RLISPSCANTABLE!*,'RLISPDIPHTHONG,'!:,'!=,'!:!= );
MAKEDIPHTHONG(RLISPSCANTABLE!*,'RLISPDIPHTHONG,'!<,'!=,'!<!= );
MAKEDIPHTHONG(RLISPSCANTABLE!*,'RLISPDIPHTHONG,'!>,'!=,'!>!= );
MAKEDIPHTHONG(RLISPSCANTABLE!*,'RLISPDIPHTHONG,'!<,'!<,'!<!< );
MAKEDIPHTHONG(RLISPSCANTABLE!*,'RLISPDIPHTHONG,'!>,'!>,'!>!> );
MAKEDIPHTHONG(RLISPSCANTABLE!*,'RLISPDIPHTHONG,'!*,'!*,'!*!* );

Symbolic Procedure XReadEof(Channel,Ef);
    if !*InsideStructureRead then
	StdError BldMsg("Unexpected EOF while parsing on channel %r", Channel)
    else Throw('!$ERROR!$, list !$EOF!$);	% embarrasingly gross kludge

Put(Int2ID char EOF, 'RlispReadMacro, 'XReadEOF);

Symbolic Procedure RatomHOOK();	%. To get READ MACRO', EG EOF
  ChannelReadTokenWithHooks IN!*;

lisp procedure RlispChannelRead Channel;  %. Parse S-expression from channel
begin scalar CurrentScanTable!*, CurrentReadMacroIndicator!*,
	CurrentDiphthongIndicator!*;
    CurrentScanTable!* := RLispReadScanTable!*;
    CurrentReadMacroIndicator!* := 'LispReadMacro;
    CurrentDiphthongIndicator!* := 'LispDiphthong;
    return ChannelReadTokenWithHooks Channel;
end;

lisp procedure RlispRead();		%. Parse S-expr from current input
    RlispChannelRead IN!*;

END;
