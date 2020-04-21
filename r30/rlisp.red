%*********************************************************************
%*********************************************************************
%                        THE REDUCE TRANSLATOR
%*********************************************************************
%********************************************************************;


%Copyright (c) 1983 The Rand Corporation;


SYMBOLIC;  %Most of REDUCE is defined in symbolic mode;


%*********************************************************************
%		NON-LOCAL VARIABLES USED IN TRANSLATOR
%********************************************************************;

%The following are used as non-local variables in this section;

FLUID '(DFPRINT!* LREADFN!* SEMIC!* TSLIN!* !*BACKTRACE !*DEFN !*ECHO
	 !*MODE !*OUTPUT !*RAISE !*SLIN !*TIME);

GLOBAL '(BLOCKP!* CMSG!* CRBUFLIS!* CRBUF!* CRBUF1!* EOF!* ERFG!*
	 FNAME!* FTYPES!* INITL!* INPUTBUFLIS!* LETL!* MOD!* OTIME!*
         OUTL!* PRECLIS!* PROMPTEXP RESULTBUFLIS!* TTYPE!* TYPL!*
         STATCOUNTER !*NAT NAT!*!* CRCHAR!* CURSYM!* IFL!* IPL!* KEY!*
         !*FORCE NXTSYM!* OFL!* OPL!* PROGRAM!* PROGRAML!* WS !*FORT
         TECHO!* !*BLANKNOTOK!* !*COMPOSITES !*CREF !*DEMO !*EXTRAECHO
         !*INT !*LOSE !*MSG !*PRET !*!*ESC);

%	These non-local variables divide into two classes. The first
%class are those which must be initialized at the top level of the
%program. These are as follows;

%BLOCKP!* := NIL;       %keeps track of which block is active;
%CRBUFLIS!* := NIL;     %terminal input buffer;
%CMSG!* := NIL;         %shows that continuation msg has been printed;
%DFPRINT!* := NIL;      %used to define special output process;
%EOF!* := NIL;          %flag indicating an end-of-file;
%ERFG!* := NIL;         %indicates that an input error has occurred;
INITL!* := '(BLOCKP!* OUTL!*);
			%list of variables initialized in BEGIN1;
%INPUTBUFLIS!* := NIL;  %association list for storing input commands;
KEY!* := 'SYMBOLIC;	%stores first word read in command;
%LETL!* := NIL;         %used in algebraic mode for special delimiters;
%LREADFN!* := NIL;      %used to define special reading function;
%MOD!* := NIL;          %modular base, NIL for integer arithmetic;
%OUTL!* := NIL;         %storage for output of input line;
PRECLIS!*:= '(OR AND NOT MEMBER MEMQ EQUAL NEQ EQ GEQ GREATERP LEQ
	      LESSP PLUS DIFFERENCE TIMES QUOTIENT EXPT CONS);
			%precedence list of infix operators;
%RESULTBUFLIS!* := NIL;  %association list for storing command outputs;
STATCOUNTER := 0;       %terminal statement counter;
%TECHO!* := NIL;        %terminal echo status;
%TSLIN!* := NIL;        %stack of input reading functions;
%!*BACKTRACE := NIL;    %if ON, prints a LISP backtrace;
%!*BLANKNOTOK!* := NIL; %if ON, disables blank as CEDIT character;
%!*COMPOSITES := NIL;   %used to indicate the use of composite numbers;
%!*CREF := NIL;         %used by cross-reference program;
%!*DEFN := NIL;         %indicates that LISP code should be output;
%!*ECHO := NIL;         %indicates echoing of input;
%!*FORCE := NIL;        %causes all macros to expand;
!*LOSE := T;		%determines whether a function flagged LOSE
			%is defined;
%!*MSG:=NIL;            %flag to indicate whether messages should be
			%printed;
%!*NAT := NIL;          %used in algebraic mode to denote 'natural'
			%output. Must be on in symbolic mode to
			%ensure input echoing;
%NAT!*!* := NIL;        %temporary variable used in algebraic mode;
!*OUTPUT := T;		%used to suppress output;
!*RAISE := T;		%causes lower to be converted to upper case;
%!*SLIN := NIL;         %indicates that LISP code should be read;
%!*TIME := NIL;         %used to indicate timing should be printed;

%	 The second class are those non-local variables which are
%initialized within some function, although they do not appear in that
%function's variable list. These are;

% CRCHAR!*		next character in input line
% CURSYM!*		current symbol (i. e. identifier, parenthesis,
%			delimiter, e.t.c,) in input line
% FNAME!*		name of a procedure being read
% FTYPES!*		list of regular procedure types
% IFL!* 		input file/channel pair - set in BEGIN to NIL
% IPL!* 		input file list- set in BEGIN to NIL
% NXTSYM!*		next symbol read in TOKEN
% OFL!* 		output file/channel pair - set in BEGIN to NIL
% OPL!* 		output file list- set in BEGIN to NIL
% PROGRAM!*		current input program
% PROGRAML!*		stores input program when error occurs for a
%			later restart
% PROMPTEXP		expression used for command prompt
% SEMIC!*		current delimiter character (used to decide
%			whether to print result of calculation)
% TTYPE!*               current token type
% WS 			used in algebraic mode to store top level value
% !*FORT		used in algebraic mode to denote FORTRAN output
% !*INT 		indicates interactive system use
% !*MODE		current mode of calculation
% !*PRET		indicates REDUCE prettyprinting of input;


COMMENT THE FOLLOWING IS USED AS A FLUID VARIABLE;

FLUID '(!*S!*);


%*********************************************************************
%                          GO TO STATEMENT
%********************************************************************;

%	 It is necessary to introduce the GO TO statement at this
%point as part of the boot-strapping process. A general description
%of the method of statement implementation is given later;

SYMBOLIC PROCEDURE GOSTAT;
   BEGIN SCALAR VAR;
	VAR := IF EQ(SCAN(),'TO) THEN SCAN() ELSE CURSYM!*;
	SCAN();
	RETURN LIST('GO,VAR)
   END;

PUT('GO,'STAT,'GOSTAT);

PUT('GOTO,'NEWNAM,'GO);


%*********************************************************************
%                 INITIALIZATION OF INFIX OPERATORS
%********************************************************************;

%	 Several operators in REDUCE are used in an infix form	(e.g.,
%+,- ). The internal alphanumeric names associated with these
%operators are introduced by the function NEWTOK defined below.
%This association, and the precedence of each infix operator, is
%initialized in this section. We also associate printing characters
%with each internal alphanumeric name as well;

DEFLIST ('(
   (NOT NOT)
   (PLUS PLUS)
   (DIFFERENCE MINUS)
   (MINUS MINUS)
   (TIMES TIMES)
   (QUOTIENT RECIP)
   (RECIP RECIP)
 ), 'UNARY);

FLAG ('(AND OR !*COMMA!* PLUS TIMES),'NARY);

FLAG ('(CONS SETQ PLUS TIMES),'RIGHT);

DEFLIST ('((MINUS PLUS) (RECIP TIMES)),'ALT);

SYMBOLIC PROCEDURE MKPREC;
   BEGIN SCALAR X,Y,Z;
	X := '!*COMMA!* . ('SETQ . PRECLIS!*);
	Y := 1;
    A:	IF NULL X THEN RETURN NIL;
	PUT(CAR X,'INFIX,Y);
	PUT(CAR X,'OP,LIST LIST(Y,Y));	 %for RPRINT;
	IF Z := GET(CAR X,'UNARY) THEN PUT(Z,'INFIX,Y);
	IF AND(Z,NULL FLAGP(Z,'NARY)) THEN PUT(Z,'OP,LIST(NIL,Y));
	X := CDR X;
	Y := ADD1 Y;
	GO TO A
   END;

MKPREC();

SYMBOLIC PROCEDURE ATSOC(U,V);
   IF NULL V THEN NIL
    ELSE IF U EQ CAAR V THEN CAR V
    ELSE ATSOC(U,CDR V);

SYMBOLIC PROCEDURE CONSESCC U;
   IF NULL U THEN NIL ELSE '!! . CAR U . CONSESCC CDR U;

SYMBOLIC PROCEDURE LSTCHR(U,V);
   IF NULL CDR U THEN CAR U . (NIL . V)
    ELSE LIST(CAR U,LIST LSTCHR(CDR U,V));

SYMBOLIC PROCEDURE NEWTOK U;
   BEGIN SCALAR V,X,Y,Z;
	V := CDR U;
	U := CAR U;
	Y := U;
	IF NULL(X:= GET(CAR Y,'SWITCH!*)) THEN GO TO D;
	Y := CDR Y;
    A:	IF NULL Y THEN GO TO E
	 ELSE IF NULL CAR X
	  THEN PROGN(RPLACA(X,LIST LSTCHR(Y,V)),GO TO C)
	 ELSE IF NULL(Z := ATSOC(CAR Y,CAR X)) THEN GO TO B1;
    B:	Y := CDR Y;
	X := CDR Z;
	GO TO A;
    B1: RPLACA(X,APPEND(CAR X,LIST LSTCHR(Y,V)));
    C:	X := INTERN COMPRESS CONSESCC U;
	IF CDR V THEN IF CDDR V THEN Y:= LIST(CADR V,CADDR V)
			ELSE Y:= LIST(CADR V,X)
	 ELSE Y:= LIST(X,X);   %the print list;
	PUT(CAR V,'PRTCH,Y);
	IF X := GET(CAR V,'UNARY) THEN PUT(X,'PRTCH,Y);
	RETURN NIL;
    D:	PUT(CAR Y,'SWITCH!*,CDR LSTCHR(Y,V));
	GO TO C;
    E:  IF !*MSG THEN LPRIM LIST(COMPRESS CONSESCC U,"redefined");
	   %test on MSG is for bootstrapping purposes;
	RPLACD(X,V);
	GO TO C
   END;

NEWTOK '((!$) !*SEMICOL!*);
NEWTOK '((!;) !*SEMICOL!*);
NEWTOK '((!+) PLUS ! !+! );
NEWTOK '((!-) DIFFERENCE ! !-! );
NEWTOK '((!*) TIMES);
NEWTOK '((!* !*) EXPT);
NEWTOK '((!/) QUOTIENT);
NEWTOK '((!=) EQUAL);
NEWTOK '((!,) !*COMMA!*);
NEWTOK '((!() !*LPAR!*);
NEWTOK '((!)) !*RPAR!*);
NEWTOK '((!:) !*COLON!*);
NEWTOK '((!: !=) SETQ ! !:!=! );
NEWTOK '((!.) CONS);
NEWTOK '((!<) LESSP);
NEWTOK '((!< !=) LEQ);
NEWTOK '((!< !<) !*LSQB!*);
NEWTOK '((!>) GREATERP);
NEWTOK '((!> !=) GEQ);
NEWTOK '((!> !>) !*RSQB!*);

FLAG('(NEWTOK),'EVAL);


%*********************************************************************
%			   REDUCE SUPERVISOR
%********************************************************************;

% The true REDUCE supervisory function is BEGIN, again defined in
%the system dependent part of this program. However, most of the work
%is done by BEGIN1, which is called by BEGIN for every file
%encountered on input;

SYMBOLIC PROCEDURE ERRORP U;
   %returns true if U is an ERRORSET error format;
   ATOM U OR CDR U;

SYMBOLIC PROCEDURE FLAGP!*!*(U,V);
  IDP U AND FLAGP(U,V);

SYMBOLIC PROCEDURE PRINTPROMPT U;
   %Prints the prompt expression for input;
   PROGN(IF OFL!* THEN WRS NIL, PRIN2 U, IF OFL!* THEN WRS CDR OFL!*);

SYMBOLIC PROCEDURE BEGIN1;
   BEGIN SCALAR MODE,PARSERR,RESULT;
    A0: CURSYM!* := '!*SEMICOL!*;
	OTIME!* := TIME();
    A:  IF NULL TERMINALP() THEN GO TO A2
	 ELSE IF STATCOUNTER>0 THEN ADD2BUFLIS();
	STATCOUNTER := STATCOUNTER + 1;
	PROMPTEXP 
         := COMPRESS('!! . APPEND(EXPLODE STATCOUNTER,EXPLODE '!:! ));
	SETPCHAR PROMPTEXP;
    A2: PARSERR := NIL;
	IF !*TIME THEN EVAL '(SHOWTIME);   %Since a STAT;
	IF !*OUTPUT AND NULL OFL!* AND TERMINALP() AND NULL !*DEFN
	  THEN TERPRI();
	IF TSLIN!*
	  THEN PROGN(!*SLIN := CAR TSLIN!*,
		     LREADFN!* := CDR TSLIN!*,
		     TSLIN!* := NIL);
	MAPCAR(INITL!*,FUNCTION SINITL);
	IF !*INT THEN ERFG!* := NIL;	%to make editing work properly;
	IF CURSYM!* EQ 'END THEN GO TO ND0;
	IF TERMINALP() AND NULL(KEY!* EQ 'ED)
	  THEN PRINTPROMPT PROMPTEXP;
	PROGRAM!* := ERRORSET('(COMMAND),T,!*BACKTRACE);
	CONDTERPRI();
	IF ERRORP PROGRAM!* THEN GO TO ERR1;
	PROGRAM!* := CAR PROGRAM!*;
	IF PROGRAM!* EQ !$EOF!$ AND TTYPE!*=3 THEN GO TO ND1
	 ELSE IF CURSYM!* EQ 'END THEN GO TO ND0
	 ELSE IF EQCAR(PROGRAM!*,'RETRY) THEN PROGRAM!* := PROGRAML!*
	 ELSE IF PROGRAM!* EQ 'ED AND GETD 'CEDIT
	   THEN PROGN(CEDIT NIL,GO TO A2)
	 ELSE IF EQCAR(PROGRAM!*,'ED) AND GETD 'CEDIT
	   THEN PROGN(CEDIT CDR PROGRAM!*,GO TO A2);
	%The following section decides what the target mode should be.
	%That mode is also assumed to be the printing mode;
	IF IDP KEY!* AND GET(KEY!*,'STAT) EQ 'MODESTAT
	  THEN MODE := KEY!*
	 ELSE IF NULL ATOM PROGRAM!* AND NULL(CAR PROGRAM!* EQ 'QUOTE)
	   AND (NULL(IDP CAR PROGRAM!* 
		   AND (FLAGP(CAR PROGRAM!*,'NOCHANGE)
			 OR FLAGP(CAR PROGRAM!*,'INTFN)
			 OR CAR PROGRAM!* EQ 'LIST))
	     OR CAR PROGRAM!* MEMQ '(SETQ SETEL)
		     AND EQCAR(CADDR PROGRAM!*,'QUOTE))
	  THEN MODE := 'SYMBOLIC
	 ELSE MODE := !*MODE;
	PROGRAM!* := CONVERTMODE1(PROGRAM!*,NIL,'SYMBOLIC,MODE);
	ADD2INPUTBUF PROGRAM!*;
	IF !*DEFN THEN GO TO D;
    B:	IF !*OUTPUT AND IFL!* AND !*ECHO THEN TERPRI();
	RESULT := ERRORSET(PROGRAM!*,T,!*BACKTRACE);
	IF ERRORP RESULT OR ERFG!*
	  THEN PROG2(PROGRAML!* := PROGRAM!*,GO TO ERR2)
	 ELSE IF !*DEFN THEN GO TO A;
	RESULT := CAR RESULT;
	IF NULL(MODE EQ 'SYMBOLIC) AND RESULT THEN ADD2RESULTBUF RESULT;
    C:  IF NULL !*OUTPUT THEN GO TO A
	 ELSE IF SEMIC!* EQ '!;
	  THEN IF MODE EQ 'SYMBOLIC
	        THEN IF NULL RESULT AND NULL(!*MODE EQ 'SYMBOLIC)
		       THEN NIL
	 	 ELSE BEGIN TERPRI(); PRINT RESULT END
	 ELSE IF RESULT THEN VARPRI(RESULT,SETVARS PROGRAM!*,'ONLY);
	GO TO A;
    D:	IF ERFG!* THEN GO TO A
	 ELSE IF FLAGP!*!*(KEY!*,'IGNORE) OR EQCAR(PROGRAM!*,'QUOTE)
	  THEN GO TO B;
	IF PROGRAM!* THEN DFPRINT PROGRAM!*;
	IF FLAGP!*!*(KEY!*,'EVAL) THEN GO TO B ELSE GO TO A;
    ND0:COMM1 'END;
    ND1: EOF!* := NIL;
	IF NULL IPL!*   %terminal END;
	  THEN BEGIN
		IF OFL!* THEN PROGN(WRS NIL,OFL!* := NIL);
	    AA: IF NULL OPL!* THEN RETURN NIL;
		CLOSE CDAR OPL!*;
		OPL!* := CDR OPL!*;
		GO TO AA
	      END;
	RETURN NIL;
    ERR1:
	IF EOF!* OR PROGRAM!* EQ !$EOF!$ AND TTYPE!*=3 THEN GO TO ND1
	 ELSE IF PROGRAM!* EQ "BEGIN invalid" THEN GO TO A
	 ELSE IF PROGRAM!* EQ !*!*ESC AND TTYPE!*=3 THEN GO TO A0;
	PARSERR := T;
    ERR2:
	RESETPARSER();  %in case parser needs to be modified;
	ERFG!* := T;
	IF NULL !*INT THEN GO TO E;
	RESULT := PAUSE1 PARSERR;
	IF RESULT THEN RETURN NULL EVAL RESULT;
	ERFG!* := NIL;
	GO TO A;
    E:	!*DEFN := T;	%continue syntax analyzing but not evaluation;
	!*ECHO := T;
	IF NULL CMSG!* THEN LPRIE "Continuing with parsing only ...";
	CMSG!* := T;
	GO TO A
   END;

SYMBOLIC PROCEDURE ADD2BUFLIS;
   BEGIN
      CRBUF!* := REVERSIP CRBUF!*;   %put in right order;
   A: IF CAR CRBUF!* EQ !$EOL!$
	    OR (!*BLANKNOTOK!* AND CAR CRBUF!* EQ '! )
	THEN PROG2(CRBUF!* := CDR CRBUF!*, GO TO A);
      CRBUFLIS!* := (STATCOUNTER . CRBUF!*) . CRBUFLIS!*;
      CRBUF!* := NIL
   END;

SYMBOLIC PROCEDURE ADD2INPUTBUF U;
   BEGIN
      IF TERMINALP()
	THEN INPUTBUFLIS!* := (STATCOUNTER . U) . INPUTBUFLIS!*
   END;

SYMBOLIC PROCEDURE ADD2RESULTBUF U;
   BEGIN
      WS := U;
      IF TERMINALP()
	THEN RESULTBUFLIS!* := (STATCOUNTER . U) . RESULTBUFLIS!*
   END;

SYMBOLIC PROCEDURE CONDTERPRI;
   !*OUTPUT AND !*ECHO AND !*EXTRAECHO AND (NULL !*INT OR IFL!*)
	AND NULL !*DEFN AND TERPRI();

SYMBOLIC PROCEDURE RESETPARSER;
   %resets the parser after an error;
   IF NULL !*SLIN THEN COMM1 T;

SYMBOLIC PROCEDURE SETVARS U;
   IF ATOM U THEN NIL
    ELSE IF CAR U MEMQ '(SETEL SETK)
     THEN CADR U . SETVARS CADDR U
    ELSE IF CAR U EQ 'SETQ THEN MKQUOTE CADR U . SETVARS CADDR U
    ELSE NIL;

SYMBOLIC PROCEDURE TERMINALP;
   %true if input is coming from an interactive terminal;
   !*INT AND NULL IFL!*;

SYMBOLIC PROCEDURE DFPRINT U;
   %Looks for special action on a form, otherwise prettyprints it;
   IF DFPRINT!* THEN APPLY(DFPRINT!*,LIST U)
    ELSE IF CMSG!* THEN NIL
    ELSE IF NULL EQCAR(U,'PROGN) THEN PRETTYPRINT U
    ELSE BEGIN
	    A:	U := CDR U;
		IF NULL U THEN RETURN NIL;
		DFPRINT CAR U;
		GO TO A
	 END;

SYMBOLIC PROCEDURE SHOWTIME;
   BEGIN SCALAR X;
      X := OTIME!*;
      OTIME!* := TIME();
      X := OTIME!*-X;
%     IF NULL TERMINALP() THEN TERPRI();
      TERPRI();
      PRIN2 "TIME: "; PRIN2 X; PRIN2T " MS";
%     IF TERMINALP() THEN TERPRI();
   END;

SYMBOLIC PROCEDURE SINITL U;
   SET(U,GET(U,'INITL));

FLAG ('(IN OUT ON OFF SHUT),'IGNORE);


%*********************************************************************
%	       IDENTIFIER AND RESERVED CHARACTER READING
%********************************************************************;

%	 The function TOKEN defined below is used for reading
%identifiers and reserved characters (such as parentheses and infix
%operators). It is called by the function SCAN, which translates
%reserved characters into their internal name, and sets up the output
%of the input line. The following definitions of TOKEN and SCAN are
%quite general, but also inefficient. THE READING PROCESS CAN OFTEN
%BE SPEEDED UP BY A FACTOR OF AS MUCH AS FIVE IF THESE FUNCTIONS
%(ESPECIALLY TOKEN) ARE CODED IN ASSEMBLY LANGUAGE;

SYMBOLIC PROCEDURE PRIN2X U;
  OUTL!*:=U . OUTL!*;

SYMBOLIC PROCEDURE MKQUOTE U; LIST('QUOTE,U);

SYMBOLIC PROCEDURE REVERSIP U;
   BEGIN SCALAR X,Y;
    A:	IF NULL U THEN RETURN Y;
	X := CDR U; Y := RPLACD(U,Y); U := X;
	GO TO A
   END;

SYMBOLIC PROCEDURE MKSTRNG U;
   %converts the uninterned id U into a string;
   %if strings are not constants, this should be replaced by
   %LIST('STRING,U);
   U;

CRCHAR!* := '! ;

SYMBOLIC PROCEDURE READCH1;
   BEGIN SCALAR X;
      IF NULL TERMINALP() THEN RETURN READCH()
       ELSE IF CRBUF1!*
	THEN BEGIN X := CAR CRBUF1!*; CRBUF1!* := CDR CRBUF1!* END
       ELSE X := READCH();
      CRBUF!* := X . CRBUF!*;
      RETURN X
   END;

SYMBOLIC PROCEDURE TOKEN1;
   BEGIN SCALAR X,Y,Z;
	X := CRCHAR!*;
    A:	IF SEPRP X THEN GO TO SEPR
	 ELSE IF DIGIT X THEN GO TO NUMBER
	 ELSE IF LITER X THEN GO TO LETTER
	 ELSE IF X EQ '!% THEN GO TO COMENT
	 ELSE IF X EQ '!! THEN GO TO ESCAPE
	 ELSE IF X EQ '!' THEN GO TO QUOTE
	 ELSE IF X EQ '!" THEN GO TO STRING;
	TTYPE!* := 3;
	IF X EQ !$EOF!$ THEN GO TO EOF;
	NXTSYM!* := X;
	IF DELCP X THEN GO TO D;
    A1: CRCHAR!* := READCH1();
	GO TO C;
    ESCAPE: 
	Z := !*RAISE;
	!*RAISE := NIL;
	Y := X . Y;
	X := READCH1();
	!*RAISE := Z;
    LETTER:
	TTYPE!* := 0;
    LET1:
	Y := X . Y;
	IF DIGIT (X := READCH1()) OR LITER X THEN GO TO LET1
	 ELSE IF X EQ '!! THEN GO TO ESCAPE;
	NXTSYM!* := INTERN COMPRESS REVERSIP Y;
    B:	CRCHAR!* := X;
    C:	RETURN NXTSYM!*;
    NUMBER:	
	TTYPE!* := 2;
    NUM1:
	Y := X . Y;
	Z := X;
	IF DIGIT (X := READCH1()) 
	   OR X EQ '!.
	   OR X EQ 'E
	   OR Z EQ 'E
	  THEN GO TO NUM1;
	NXTSYM!* := COMPRESS REVERSIP Y;
	GO TO B;
    QUOTE:
	CRCHAR!* := READCH1();
	NXTSYM!* := MKQUOTE RREAD();
	TTYPE!* := 4;
	GO TO C;
    STRING:
	Z := !*RAISE;
	!*RAISE := NIL;
    STRINX:
	Y := X . Y;
	IF NULL((X := READCH1()) EQ '!") THEN GO TO STRINX;
	Y := X . Y;
	NXTSYM!* := MKSTRNG COMPRESS REVERSIP Y;
	!*RAISE := Z;
	TTYPE!* := 1;
	GO TO A1;
    COMENT:
	IF NULL(READCH1() EQ !$EOL!$) THEN GO TO COMENT;
    SEPR:
	X := READCH1();
	GO TO A;
    D:  CRCHAR!* := '! ;
	GO TO C;
    EOF:CRCHAR!* := '! ;
	FILENDERR()
   END;

SYMBOLIC PROCEDURE TOKEN;
   %This provides a hook for a faster TOKEN;
   TOKEN1();

SYMBOLIC PROCEDURE FILENDERR;
   BEGIN 
      EOF!* := T;
      ERROR(99,IF IFL!* THEN LIST("EOF read in file",CAR IFL!*)
		ELSE LIST "EOF read")
   END;

SYMBOLIC PROCEDURE PTOKEN;
   BEGIN SCALAR X;
	X := TOKEN();
	IF X EQ '!) AND EQCAR(OUTL!*,'! ) THEN OUTL!*:= CDR OUTL!*;
	   %an explicit reference to OUTL!* used here;
	PRIN2X X;
	IF NULL ((X EQ '!() OR (X EQ '!))) THEN PRIN2X '! ;
	RETURN X
   END;

SYMBOLIC PROCEDURE RREAD1;
   BEGIN SCALAR X,Y;
	X := PTOKEN();
	IF NULL (TTYPE!*=3) THEN RETURN X
	 ELSE IF X EQ '!( THEN RETURN RRDLS()
	 ELSE IF NULL (X EQ '!+ OR X EQ '!-) THEN RETURN X;
	Y := PTOKEN();
	IF NULL NUMBERP Y
	  THEN PROGN(NXTSYM!* := " ",
		     SYMERR("Syntax error: improper number",NIL))
	 ELSE IF X EQ '!- THEN Y := APPLY('MINUS,LIST Y);
	   %we need this construct for bootstrapping purposes;
	RETURN Y
   END;

SYMBOLIC PROCEDURE RRDLS;
   BEGIN SCALAR X,Y;
	X := RREAD1();
	IF NULL (TTYPE!*=3) THEN GO TO A
	 ELSE IF X EQ '!) THEN RETURN NIL
	 ELSE IF NULL (X EQ '!.) THEN GO TO A;
	X := RREAD1();
	Y := PTOKEN();
	IF NULL (TTYPE!*=3) OR NULL (Y EQ '!))
	  THEN PROGN(NXTSYM!* := " ",SYMERR("Invalid S-expression",NIL))
	 ELSE RETURN X;
    A:	RETURN (X . RRDLS())
   END;

SYMBOLIC PROCEDURE RREAD;
   PROGN(PRIN2X " '",RREAD1());

SYMBOLIC PROCEDURE SCAN;
   BEGIN SCALAR X,Y;
	IF NULL (CURSYM!* EQ '!*SEMICOL!*) THEN GO TO B;
    A:	NXTSYM!* := TOKEN();
    B:	IF NULL ATOM NXTSYM!* THEN GO TO Q1
	 ELSE IF NXTSYM!* EQ 'ELSE OR CURSYM!* EQ '!*SEMICOL!*
	 THEN OUTL!* := NIL;
	PRIN2X NXTSYM!*;
    C:	IF NULL IDP NXTSYM!* THEN GO TO L
	 ELSE IF (X:=GET(NXTSYM!*,'NEWNAM)) AND
			(NULL (X=NXTSYM!*)) THEN GO TO NEW
	 ELSE IF NXTSYM!* EQ 'COMMENT OR NXTSYM!* EQ '!% AND TTYPE!*=3
	  THEN GO TO COMM
	 ELSE IF NULL(TTYPE!* = 3) THEN GO TO L
	 ELSE IF NXTSYM!* EQ !*!*ESC THEN ERROR(9999,!*!*ESC)
	 ELSE IF NXTSYM!* EQ !$EOF!$ THEN RETURN FILENDERR()
	 ELSE IF NXTSYM!* EQ '!' THEN GO TO QUOTE
	 ELSE IF NULL (X:= GET(NXTSYM!*,'SWITCH!*)) THEN GO TO L
	 ELSE IF CADR X EQ '!*SEMICOL!* THEN GO TO DELIM;
   SW1: NXTSYM!* := TOKEN();
	IF NULL(TTYPE!* = 3) THEN GO TO SW2
	 ELSE IF NXTSYM!* EQ !$EOF!$ THEN RETURN FILENDERR()
	 ELSE IF CAR X THEN GO TO SW3;
   SW2: CURSYM!*:=CADR X;
	IF CURSYM!* EQ '!*RPAR!* THEN GO TO L2
	 ELSE RETURN CURSYM!*;
   SW3: IF NULL (Y:= ATSOC(NXTSYM!*,CAR X)) THEN GO TO SW2;
	PRIN2X NXTSYM!*;
	X := CDR Y;
	GO TO SW1;
  COMM: IF DELCP CRCHAR!* THEN GO TO COM1;
	CRCHAR!* := READCH();
	GO TO COMM;
  COM1: CRCHAR!* := '! ;
	CONDTERPRI();
	GO TO A;
  DELIM:
	SEMIC!*:=NXTSYM!*;
	RETURN (CURSYM!*:='!*SEMICOL!*);
  NEW:	NXTSYM!* := X;
	IF STRINGP X THEN GO TO L
	ELSE IF ATOM X THEN GO TO C
	ELSE GO TO L;
  QUOTE:
	NXTSYM!* := MKQUOTE RREAD1();
	GO TO L;
  Q1:	IF NULL (CAR NXTSYM!* EQ 'STRING) THEN GO TO L;
	PRIN2X " ";
	PRIN2X CADR(NXTSYM!* := MKQUOTE CADR NXTSYM!*);
  L:	CURSYM!*:=NXTSYM!*;
  L1:	NXTSYM!* := TOKEN();
	IF NXTSYM!* EQ !$EOF!$ AND TTYPE!* = 3 THEN RETURN FILENDERR();
  L2:	IF NUMBERP NXTSYM!*
	   OR (ATOM NXTSYM!* AND NULL GET(NXTSYM!*,'SWITCH!*))
	  THEN PRIN2X " ";
	RETURN CURSYM!*;
  EOF:  FILENDERR()
   END;


%*********************************************************************
%			  EXPRESSION READING
%********************************************************************;

%	 The conversion of a REDUCE expression to LISP prefix form is
%carried out by the function XREAD. This function initiates the
%scanning process, and then calls the auxiliary function XREAD1 to
%perform the actual parsing. Both XREAD and XREAD1 are used by many
%functions whenever an expression must be read;

FLAG ('(END !*COLON!* !*SEMICOL!*),'DELIM);

SYMBOLIC PROCEDURE EQCAR(U,V);
   NULL ATOM U AND CAR U EQ V;

SYMBOLIC PROCEDURE MKSETQ(U,V);
   LIST('SETQ,U,V);

SYMBOLIC PROCEDURE MKVAR(U,V); U;

SYMBOLIC PROCEDURE REMCOMMA U;
   IF EQCAR(U,'!*COMMA!*) THEN CDR U ELSE LIST U;

SYMBOLIC PROCEDURE ARRAYP U;
   GET(U,'ARRAY);

SYMBOLIC PROCEDURE GETTYPE U;
   %it might be better to use a table here for more generality;
   IF NULL ATOM U THEN 'FORM
    ELSE IF NUMBERP U THEN 'NUMBER
    ELSE IF ARRAYP U THEN 'ARRAY
    ELSE IF GET(U,'SIMPFN) OR GET(U,'MSIMPFN) THEN 'OPERATOR
    ELSE IF GET(U,'AVALUE) THEN 'VARIABLE
    ELSE IF GETD U THEN 'PROCEDURE
    ELSE IF GLOBALP U THEN 'GLOBAL
    ELSE IF FLUIDP U THEN 'FLUID
    ELSE IF GET(U,'MATRIX) THEN 'MATRIX
    ELSE IF FLAGP(U,'PARM) THEN 'PARAMETER
    ELSE NIL;

SYMBOLIC PROCEDURE XREAD1 U;
   BEGIN SCALAR V,W,X,Y,Z,Z1,Z2;
	% V: EXPRESSION BEING BUILT
	% W: PREFIX OPERATOR STACK
	% X: INFIX OPERATOR STACK
	% Y: INFIX VALUE OR STAT PROPERTY
	% Z: CURRENT SYMBOL
	% Z1: NEXT SYMBOL
	% Z2: TEMPORARY STORAGE;
  A:    Z := CURSYM!*;
  A1:   IF NULL IDP Z THEN NIL
	 ELSE IF Z EQ '!*LPAR!* THEN GO TO LPAREN
	 ELSE IF Z EQ '!*RPAR!* THEN GO TO RPAREN
	 ELSE IF Y := GET(Z,'INFIX) THEN GO TO INFX
	 ELSE IF NXTSYM!* EQ '!: THEN NIL
	 ELSE IF FLAGP(Z,'DELIM) THEN GO TO DELIMIT
	 ELSE IF Y := GET(Z,'STAT) THEN GO TO STAT;
  A2:   Y := NIL;
  A3:   W := Z . W;
  NEXT: Z := SCAN();
	GO TO A1;
  LPAREN:
	Y := NIL;
	IF SCAN() EQ '!*RPAR!* THEN GO TO LP1;
	   %function of no args;
	Z := XREAD1 IF EQCAR(W,'MAT)
		    THEN PROGN(TYPL!* := UNION('(MATP),TYPL!*),'MAT)
		   ELSE 'PAREN;
	IF U EQ 'MAT THEN GO TO LP2
	 ELSE IF NULL EQCAR(Z,'!*COMMA!*) THEN GO TO A3
	 ELSE IF NULL W
	   THEN (IF U EQ 'LAMBDA THEN GO TO A3
		 ELSE SYMERR("Improper delimiter",NIL))
	 ELSE W := (CAR W . CDR Z) . CDR W;
	GO TO NEXT;
  LP1:  IF W THEN W := LIST CAR W . CDR W;  %function of no args;
	GO TO NEXT;
  LP2:  Z := REMCOMMA Z;
	GO TO A3;
  RPAREN:
	IF NULL U OR U EQ 'GROUP OR U EQ 'PROC
	  THEN SYMERR("Too many right parentheses",NIL)
	 ELSE GO TO END1;
  INFX: IF Z EQ '!*COMMA!* OR NULL ATOM (Z1 := SCAN())
		OR NUMBERP Z1 THEN GO TO IN1
	 ELSE IF Z1 EQ '!*RPAR!*%infix operator used as variable;
		OR Z1 EQ '!*COMMA!*
		OR FLAGP(Z1,'DELIM)
	  THEN GO TO IN2
	 ELSE IF Z1 EQ '!*LPAR!*%infix operator in prefix position;
		    AND NULL ATOM(Z1 := XREAD 'PAREN)
		    AND CAR Z1 EQ '!*COMMA!*
		    AND (Z := Z . CDR Z1)
	  THEN GO TO A1;
  IN1:	IF W THEN GO TO UNWIND
	 ELSE IF NULL(Z := GET(Z,'UNARY))
	  THEN SYMERR("Redundant operator",NIL);
	V := '!*!*UN!*!* . V;
	GO TO PR1;
  IN2:  Y := NIL;
	W := Z . W;
  IN3:  Z := Z1;
	GO TO A1;
  UNWIND:
	Z2 := MKVAR(CAR W,Z);
  UN1:	W:= CDR W;
	IF NULL W THEN GO TO UN2
	 ELSE IF NUMBERP CAR W THEN SYMERR("Missing Operator",NIL);
	Z2 := LIST(CAR W,Z2);
	GO TO UN1;
  UN2:	V:= Z2 . V;
  PRECED:
	IF NULL X THEN IF Y=0 THEN GO TO END2 ELSE NIL
	 ELSE IF Y<CAAR X
	   OR (Y=CAAR X
	       AND ((Z EQ CDAR X AND NULL FLAGP(Z,'NARY)
				 AND NULL FLAGP(Z,'RIGHT))
			     OR GET(CDAR X,'ALT)))
	  THEN GO TO PR2;
  PR1:	X:= (Y . Z) . X;
	IF NULL(Z EQ '!*COMMA!*) THEN GO TO IN3
	 ELSE IF CDR X OR NULL U OR U MEMQ '(LAMBDA MAT PAREN)
	  THEN GO TO NEXT
	 ELSE GO TO END2;
  PR2:	%IF CDAR X EQ 'SETQ THEN GO TO ASSIGN ELSE;
	IF CADR V EQ '!*!*UN!*!*
	  THEN (IF CAR V EQ '!*!*UN!*!* THEN GO TO PR1
		ELSE Z2 := LIST(CDAR X,CAR V))
	 ELSE Z2 := CDAR X .
		     IF EQCAR(CAR V,CDAR X) AND FLAGP(CDAR X,'NARY)
		       THEN (CADR V . CDAR V)
		      ELSE LIST(CADR V,CAR V);
	X:= CDR X;
	V := Z2 . CDDR V;
	GO TO PRECED;
  STAT: IF NULL(FLAGP(Z,'GO)
	   OR NULL(U EQ 'PROC) AND (FLAGP(Y,'ENDSTAT)
		OR (NULL DELCP NXTSYM!* AND NULL (NXTSYM!* EQ '!,))))
	  THEN GO TO A2;
	W := APPLY(Y,NIL) . W;
	Y := NIL;
	GO TO A;
  DELIMIT:
	IF Z EQ '!*COLON!* AND NULL(U EQ 'FOR)
	      AND (NULL BLOCKP!* OR NULL W OR NULL ATOM CAR W OR CDR W)
	   OR FLAGP(Z,'NODEL)
	      AND (NULL U OR U EQ 'GROUP AND NULL Z EQ '!*RSQB!*)
	  THEN SYMERR("Improper delimiter",NIL)
	 ELSE IF U MEMQ '(MAT PAREN)
	  THEN SYMERR("Too few right parentheses",NIL);
  END1: IF Y THEN SYMERR("Improper delimiter",NIL)
	 ELSE IF NULL V AND NULL W AND NULL X THEN RETURN NIL;
	Y := 0;
	GO TO UNWIND;
  END2: IF NULL CDR V THEN RETURN CAR V
	 ELSE SYMERR("Improper delimiter",NIL)
   END;

%SYMBOLIC PROCEDURE GETELS U;
%   GETEL(CAR U . !*EVLIS CDR U);

%SYMBOLIC PROCEDURE !*EVLIS U;
%   MAPCAR(U,FUNCTION EVAL);

FLAG ('(ENDSTAT MODESTAT RETSTAT),'ENDSTAT);

FLAG ('(ELSE UNTIL),'NODEL);

FLAG ('(BEGIN),'GO);

SYMBOLIC PROCEDURE XREAD U;
   PROGN(SCAN(),XREAD1 U);

FLAG('(XREAD),'OPFN);   %to make it an operator;

SYMBOLIC PROCEDURE COMMAND;
   BEGIN SCALAR X;
        IF !*DEMO AND (X := IFL!*)
          THEN PROGN(TERPRI(),RDS NIL,READCH(),RDS CDR X);
	IF NULL !*SLIN 
	  THEN PROGN(SCAN(),KEY!* := CURSYM!*,X := XREAD1 NIL)
	 ELSE PROGN(KEY!* := (SEMIC!* := '!;),
		    X := IF LREADFN!* THEN APPLY(LREADFN!*,NIL)
			  ELSE READ(),
		    IF KEY!* EQ '!;
		      THEN KEY!* := IF ATOM X THEN X ELSE CAR X);
	IF !*PRET THEN PROGN(TERPRI(),RPRINT X);
	IF NULL !*SLIN THEN X := FORM X;
	RETURN X
   END;

FLAG ('(DEFLIST FLAG FLUID GLOBAL REMFLAG REMPROP UNFLUID),'EVAL);


%*********************************************************************
%			   GENERAL FUNCTIONS
%********************************************************************;


SYMBOLIC PROCEDURE ACONC(U,V);
   %adds element V to the tail of U. U is destroyed in process;
   NCONC(U,LIST V);

SYMBOLIC PROCEDURE PRIN2T U; PROGN(PRIN2 U, TERPRI(), U);

SYMBOLIC PROCEDURE UNION(X,Y);
   IF NULL X THEN Y
    ELSE UNION(CDR X,IF CAR X MEMBER Y THEN Y ELSE CAR X . Y);

SYMBOLIC PROCEDURE XN(U,V);
   IF NULL U THEN NIL
    ELSE IF CAR U MEMBER V THEN CAR U . XN(CDR U,DELETE(CAR U,V))
    ELSE XN(CDR U,V);

SYMBOLIC PROCEDURE U>=V; NOT(U<V);

SYMBOLIC PROCEDURE U<=V; NOT(U>V);

SYMBOLIC PROCEDURE U NEQ V; NOT(U=V);


%*********************************************************************
%	 FUNCTIONS FOR PRINTING DIAGNOSTIC AND ERROR MESSAGES
%********************************************************************;

SYMBOLIC PROCEDURE LPRI U;
   BEGIN
    A:	IF NULL U THEN RETURN NIL;
	PRIN2 CAR U;
	PRIN2 " ";
	U := CDR U;
	GO TO A
   END;

SYMBOLIC PROCEDURE LPRIW (U,V);
   BEGIN SCALAR X;
	U := U . IF V AND ATOM V THEN LIST V ELSE V;
	IF OFL!* AND (!*FORT OR NOT !*NAT OR !*DEFN) THEN GO TO C;
	TERPRI();
    A:	LPRI U;
	TERPRI();
	IF NULL X THEN GO TO B;
	WRS CDR X;
	RETURN NIL;
    B:	IF NULL OFL!* THEN RETURN NIL;
    C:	X := OFL!*;
	WRS NIL;
	GO TO A
   END;

SYMBOLIC PROCEDURE LPRIM U;
   !*MSG AND LPRIW("***",U);

SYMBOLIC PROCEDURE LPRIE U;
   BEGIN SCALAR X;
	IF !*INT THEN GO TO A;
	X:= !*DEFN;
	!*DEFN := NIL;
    A:	ERFG!* := T;
	LPRIW ("*****",U);
	IF NULL !*INT THEN !*DEFN := X
   END;

SYMBOLIC PROCEDURE PRINTTY U;
   BEGIN SCALAR OFL;
	IF NULL !*FORT AND !*NAT THEN PRINT U;
	IF NULL OFL!* THEN RETURN NIL;
	OFL := OFL!*;
	WRS NIL;
	PRINT U;
	WRS CDR OFL
   END;

SYMBOLIC PROCEDURE REDERR U;
   BEGIN LPRIE U; ERROR1() END;

FLAG('(REDERR),'OPFN);

SYMBOLIC PROCEDURE SYMERR(U,V);
   BEGIN SCALAR X;
	ERFG!* := T;
	IF NUMBERP CURSYM!* OR NOT(X := GET(CURSYM!*,'PRTCH))
	  THEN X := CURSYM!*
	 ELSE X := CAR X;
	TERPRI();
	IF !*ECHO THEN TERPRI();
	OUTL!*:=CAR OUTL!* . '!$!$!$ . CDR OUTL!*;
	COMM1 T;
	MAPCAR(REVERSIP OUTL!*,FUNCTION PRIN2);
	TERPRI();
	OUTL!* := NIL;
	IF NULL V THEN REDERR U
	 ELSE REDERR(X . ("invalid" .
		     (IF U THEN LIST("in",U,"statement") ELSE NIL)))
   END;

SYMBOLIC PROCEDURE TYPERR(U,V); REDERR LIST(U,"invalid as",V);


%*********************************************************************
%                             STATEMENTS
%********************************************************************;

%	 With the exception of assignment statements, which are
%handled by XREAD, statements in REDUCE are introduced by a key-word,
%which	initiates a reading process peculiar to that statement. The
%key-word is recognized (in XREAD1) by the indicator STAT on its
%property list. The corresponding property is the name of the
%function (of no arguments) which carries out the reading sequence. We
%begin	by introducing several statements which are necessary in a
%basic system. Later on, we introduce statements which are part of the
%complete system, but may be omitted if the corresponding
%constructions are not required.

%	 System users may add new statements to REDUCE by putting the
%name of the statement reading function on the property list of the
%new key-word with the indicator STAT. The reading function could be
%defined as a new function or be a function already in the system.
%Several applications only require that the arguments be grouped
%together and quoted (such as IN, OUT, etc). To help with this, the
%following two general statement reading functions are available. They
%are used in this translator by ARRAY defined later. The function RLIS
%reads a list of arguments and returns it as one argument;

SYMBOLIC PROCEDURE RLIS;
   BEGIN SCALAR X;
	X := CURSYM!*;
	RETURN IF FLAGP!*!*(SCAN(),'DELIM) THEN LIST(X,NIL)
 	ELSE X . REMCOMMA XREAD1 'LAMBDA
   END;

SYMBOLIC PROCEDURE FLAGOP U; BEGIN FLAG(U,'FLAGOP); RLISTAT U END;

SYMBOLIC PROCEDURE RLISTAT U;
   BEGIN
    A:	IF NULL U THEN RETURN NIL;
	PUT(CAR U,'STAT,'RLIS);
	U := CDR U;
	GO TO A
   END;

RLISTAT '(FLAGOP);


%*********************************************************************
%                               COMMENTS
%********************************************************************;

SYMBOLIC PROCEDURE COMM1 U;
   BEGIN SCALAR BOOL;
	IF U EQ 'END THEN GO TO B;
  A:	IF CURSYM!* EQ '!*SEMICOL!*
	   OR U EQ 'END
		AND CURSYM!* MEMQ
	 	   '(END ELSE THEN UNTIL !*RPAR!* !*RSQB!*)
	  THEN RETURN NIL
	 ELSE IF U EQ 'END AND NULL BOOL
	  THEN PROGN(LPRIM LIST("END-COMMENT NO LONGER SUPPORTED"),
		     BOOL := T);
  B:	SCAN();
	GO TO A
   END;


%*********************************************************************
%                        CONDITIONAL STATEMENT
%********************************************************************;

SYMBOLIC PROCEDURE FORMCOND(U,VARS,MODE);
   'COND . FORMCOND1(U,VARS,MODE);

SYMBOLIC PROCEDURE FORMCOND1(U,VARS,MODE);
   IF NULL U THEN NIL
    ELSE LIST(FORMBOOL(CAAR U,VARS,MODE),FORMC(CADAR U,VARS,MODE))
	      . FORMCOND1(CDR U,VARS,MODE);

PUT('COND,'FORMFN,'FORMCOND);

SYMBOLIC PROCEDURE IFSTAT;
   BEGIN SCALAR CONDX,CONDIT;
	FLAG(LETL!*,'DELIM);
    A:	CONDX := XREAD T;
	REMFLAG(LETL!*,'DELIM);
	IF NOT CURSYM!* EQ 'THEN THEN GO TO C;
	CONDIT := ACONC(CONDIT,LIST(CONDX,XREAD T));
	IF NOT CURSYM!* EQ 'ELSE THEN GO TO B
	 ELSE IF SCAN() EQ 'IF THEN GO TO A
	 ELSE CONDIT := ACONC(CONDIT,LIST(T,XREAD1 T));
    B:	RETURN ('COND . CONDIT);
    C:	IF NOT CURSYM!* MEMQ LETL!* THEN SYMERR('IF,T);
	RETURN IFLET CONDX
   END;

PUT('IF,'STAT,'IFSTAT);

FLAG ('(THEN ELSE),'DELIM);


%*********************************************************************
%                          COMPOUND STATEMENT
%********************************************************************;

SYMBOLIC PROCEDURE DECL U;
   BEGIN SCALAR VARLIS,W;
    A:	IF CURSYM!* EQ '!*SEMICOL!* THEN GO TO C
	 ELSE IF NOT FLAGP!*!*(CURSYM!*,'TYPE) THEN RETURN VARLIS
	 ELSE IF CURSYM!* EQ 'DCL THEN GO TO DCL;
	W := CURSYM!*;
	IF SCAN() EQ 'PROCEDURE THEN RETURN PROCSTAT1 W;
	VARLIS := APPEND(VARLIS,PAIRVARS(REMCOMMA XREAD1 NIL,NIL,W));
    B: 	IF NOT CURSYM!* EQ '!*SEMICOL!* THEN SYMERR(NIL,T)
	 ELSE IF NULL U THEN RETURN LIST('DCL,MKQUOTE VARLIS);
		%top level declaration;
    C:	SCAN();
	GO TO A;
    DCL: VARLIS := APPEND(VARLIS,DCLSTAT1());
	GO TO B
   END;

FLAG ('(DCL REAL INTEGER SCALAR),'TYPE);

SYMBOLIC PROCEDURE DCLSTAT; LIST('DCL,MKQUOTE DCLSTAT1());

SYMBOLIC PROCEDURE DCLSTAT1;
   BEGIN SCALAR X,Y;
    A:	X := XREAD NIL;
	IF NOT CURSYM!* EQ '!*COLON!* THEN SYMERR('DCL,T);
	Y := APPEND(Y,PAIRVARS(REMCOMMA X,NIL,SCAN()));
	IF SCAN() EQ '!*SEMICOL!* THEN RETURN Y
	 ELSE IF NOT CURSYM!* EQ '!*COMMA!* THEN SYMERR('DCL,T)
	 ELSE GO TO A
   END;

GLOBAL '(!*VARS!*);

SYMBOLIC PROCEDURE DCL U;
   %U is a list of (id, mode) pairs, which are declared as global vars;
   BEGIN SCALAR X;
      !*VARS!* := APPEND(U,!*VARS!*);
      X := MAPCAR(U,FUNCTION CAR);
      GLOBAL X;
      FLAG(X,'SHARE);
   A: IF NULL U THEN RETURN NIL;
      SET(CAAR U,GET(CDAR U,'INITVALUE));
      U := CDR U;
      GO TO A
   END;

PUT('INTEGER,'INITVALUE,0);

PUT('DCL,'STAT,'DCLSTAT);

SYMBOLIC PROCEDURE MKPROG(U,V);
   'PROG . (U . V);

SYMBOLIC PROCEDURE SETDIFF(U,V);
   IF NULL V THEN U ELSE SETDIFF(DELETE(CAR V,U),CDR V);

SYMBOLIC PROCEDURE PAIRVARS(U,VARS,MODE);
   BEGIN SCALAR X;
   A: IF NULL U THEN RETURN APPEND(REVERSIP X,VARS);
      X := (CAR U . MODE) . X;
      U := CDR U;
      GO TO A
   END;

SYMBOLIC PROCEDURE FORMBLOCK(U,VARS,MODE);
   'PROG . APPEND(INITPROGVARS CAR U,
	      FORMPROG1(CDR U,APPEND(CAR U,VARS),MODE));

SYMBOLIC PROCEDURE INITPROGVARS U;
   BEGIN SCALAR X,Y,Z;
    A: IF NULL U THEN RETURN(REVERSIP X . REVERSIP Y)
       ELSE IF Z := GET(CDAR U,'INITVALUE)
	THEN Y := MKSETQ(CAAR U,Z) . Y;
      X := CAAR U . X;
      U := CDR U;
      GO TO A
   END;

SYMBOLIC PROCEDURE FORMPROG(U,VARS,MODE);
   'PROG . CAR U . FORMPROG1(CDR U,PAIRVARS(CAR U,VARS,MODE),MODE);

SYMBOLIC PROCEDURE FORMPROG1(U,VARS,MODE);
   IF NULL U THEN NIL
    ELSE IF ATOM CAR U THEN CAR U . FORMPROG1(CDR U,VARS,MODE)
    ELSE IF IDP CAAR U AND GET(CAAR U,'STAT) EQ 'MODESTAT
     THEN FORMC(CADAR U,VARS,CAAR U) . FORMPROG1(CDR U,VARS,MODE)
    ELSE FORMC(CAR U,VARS,MODE) . FORMPROG1(CDR U,VARS,MODE);

PUT('BLOCK,'FORMFN,'FORMBLOCK);

PUT('PROG,'FORMFN,'FORMPROG);

SYMBOLIC PROCEDURE BLOCKSTAT;
   BEGIN SCALAR X,HOLD,VARLIS;
	BLOCKP!* := NIL . BLOCKP!*;
	SCAN();
	IF CURSYM!* MEMQ '(NIL !*RPAR!*) THEN REDERR "BEGIN invalid";
	VARLIS := DECL T;
    A:	IF CURSYM!* EQ 'END AND NOT NXTSYM!* EQ '!: THEN GO TO B;
	X := XREAD1 NIL;
	IF EQCAR(X,'END) THEN GO TO C;
	NOT CURSYM!* EQ 'END AND SCAN();
	IF X THEN HOLD := ACONC(HOLD,X);
	GO TO A;
    B:	COMM1 'END;
    C:	BLOCKP!* := CDR BLOCKP!*;
	RETURN MKBLOCK(VARLIS,HOLD)
   END;

SYMBOLIC PROCEDURE MKBLOCK(U,V); 'BLOCK . (U . V);

PUTD('BLOCK,'MACRO,
 '(LAMBDA (U) (CONS 'PROG
		 (CONS (MAPCAR (CADR U) (FUNCTION CAR)) (CDDR U)))));

SYMBOLIC PROCEDURE DECSTAT;
   %only called if a declaration occurs at the top level or not first
   %in a block;
   BEGIN SCALAR X,Y,Z;
      IF BLOCKP!* THEN SYMERR('BLOCK,T);
      X := CURSYM!*;
      Y := NXTSYM!*;
      Z := DECL NIL;
      IF Y NEQ 'PROCEDURE THEN REDERR LIST(X,"invalid outside block");
      RETURN Z
   END;

PUT('INTEGER,'STAT,'DECSTAT);

PUT('REAL,'STAT,'DECSTAT);

PUT('SCALAR,'STAT,'DECSTAT);

PUT('BEGIN,'STAT,'BLOCKSTAT);


%*********************************************************************
%                           RETURN STATEMENT
%********************************************************************;

SYMBOLIC PROCEDURE RETSTAT;
   IF NOT BLOCKP!* THEN SYMERR(NIL,T)
    ELSE LIST('RETURN,
	      IF FLAGP!*!*(SCAN(),'DELIM) THEN NIL ELSE XREAD1 T);

PUT('RETURN,'STAT,'RETSTAT);


%*********************************************************************
%                      EVALUATION MODE STATEMENT
%********************************************************************;

SYMBOLIC PROCEDURE MODESTAT;
   BEGIN SCALAR X;
      X:= CURSYM!*;
      RETURN IF FLAGP!*!*(SCAN(),'DELIM) THEN PROGN(!*MODE := X, NIL)
	      ELSE LIST(X,XREAD1 T)
   END;


%*********************************************************************
%                           LAMBDA STATEMENT
%********************************************************************;

SYMBOLIC PROCEDURE FORMLAMB(U,VARS,MODE);
   LIST('LAMBDA,CAR U,FORM1(CADR U,PAIRVARS(CAR U,VARS,MODE),MODE));

PUT('LAMBDA,'FORMFN,'FORMLAMB);

SYMBOLIC PROCEDURE LAMSTAT;
   BEGIN SCALAR X,Y;
	X:= XREAD 'LAMBDA;
%	X := FLAGTYPE(IF NULL X THEN NIL ELSE REMCOMMA X,'SCALAR);
	IF X THEN X := REMCOMMA X;
	Y := LIST('LAMBDA,X,XREAD T);
%	REMTYPE X;
	RETURN Y
   END;

PUT ('LAMBDA,'STAT,'LAMSTAT);


%*********************************************************************
%			    GROUP STATEMENT
%********************************************************************;

SYMBOLIC PROCEDURE FORMPROGN(U,VARS,MODE);
   'PROGN . FORMCLIS(U,VARS,MODE);

PUT('PROGN,'FORMFN,'FORMPROGN);

SYMBOLIC PROCEDURE MKPROGN;
   %Expects a list of statements terminated by a >>;
   BEGIN SCALAR LST;
    A:	LST := ACONC(LST,XREAD 'GROUP);
	IF NULL(CURSYM!* EQ '!*RSQB!*) THEN GO TO A;
	SCAN();
	RETURN 'PROGN . LST
   END;

PUT('!*LSQB!*,'STAT,'MKPROGN);

FLAG('(!*RSQB!*),'DELIM);

FLAG('(!*RSQB!*),'NODEL);


%*********************************************************************
%                      EXPRESSION MODE ANALYSIS
%********************************************************************;

COMMENT This module is required at this point for bootstrapping
	purposes;

SYMBOLIC PROCEDURE EXPDRMACRO U;
   %returns the macro form for U if expansion is permitted;
   BEGIN SCALAR X;
      IF NULL(X := GETRMACRO U) THEN RETURN NIL
       ELSE IF NULL !*CREF AND (NULL !*DEFN OR CAR X EQ 'SMACRO)
          OR FLAGP(U,'EXPAND) OR !*FORCE AND NULL FLAGP(U,'NOEXPAND)
        THEN RETURN X
       ELSE RETURN NIL
   END;

SYMBOLIC PROCEDURE GETRMACRO U;
   %returns a Reduce macro definition for U, if one exists,
   %in GETD format;
   BEGIN SCALAR X;
      RETURN IF NOT IDP U THEN NIL
       ELSE IF (X := GETD U) AND CAR X EQ 'MACRO THEN X
       ELSE IF (X := GET(U,'SMACRO)) THEN 'SMACRO . X
%       ELSE IF (X := GET(U,'NMACRO)) THEN 'NMACRO . X;
       ELSE NIL
   END;

SYMBOLIC PROCEDURE APPLMACRO(U,V,W);
   APPLY(U,LIST(W . V));

%SYMBOLIC PROCEDURE APPLNMACRO(U,V,W);
%   APPLY(U,IF FLAGP(W,'NOSPREAD) THEN LIST V ELSE V);

SYMBOLIC PROCEDURE APPLSMACRO(U,V,W);
   %We could use an atom sublis here, eg SUBLA;
   SUBLIS(PAIR(CADR U,V),CADDR U);

PUT('MACRO,'MACROFN,'APPLMACRO);

%PUT('NMACRO,'MACROFN,'APPLNMACRO);

PUT('SMACRO,'MACROFN,'APPLSMACRO);

FLAG('(ED GO QUOTE),'NOFORM);

SYMBOLIC PROCEDURE FORM1(U,VARS,MODE);
   BEGIN SCALAR X,Y;
      IF ATOM U
	THEN RETURN IF U EQ 'ED THEN LIST U
		     ELSE IF NOT(IDP U AND (X:= GET(MODE,'IDFN))) THEN U
		     ELSE APPLY(X,LIST(U,VARS))
       ELSE IF NOT ATOM CAR U THEN RETURN FORMLIS(U,VARS,MODE)
       ELSE IF NOT IDP CAR U
	THEN TYPERR(CAR U,"operator")
       ELSE IF FLAGP(CAR U,'NOFORM) THEN RETURN U
       ELSE IF ARRAYP CAR U
	 AND (MODE EQ 'SYMBOLIC OR INTEXPRLISP(CDR U,VARS))
	THEN RETURN LIST('GETEL,INTARGFN(U,VARS))
       ELSE IF GET(CAR U,'STAT) EQ 'MODESTAT
	THEN RETURN CONVERTMODE(CADR U,VARS,MODE,CAR U)
       ELSE IF (X := GET(CAR U,'FORMFN))
	THEN RETURN MACROCHK(APPLY(X,LIST(CDR U,VARS,MODE)),MODE)
       ELSE IF GET(CAR U,'STAT) EQ 'RLIS
	THEN RETURN MACROCHK(FORMRLIS(U,VARS,MODE),MODE);
      X := FORMLIS(CDR U,VARS,MODE);
      Y := IF X=CDR U THEN U ELSE CAR U . X;
      RETURN IF MODE EQ 'SYMBOLIC
	      OR GET(CAR U,'STAT) OR CDR U AND EQCAR(CADR U,'QUOTE)
	      OR INTEXPRNP(Y,VARS) AND NULL !*COMPOSITES AND NULL MOD!*
	       THEN MACROCHK(Y,MODE)
	      ELSE IF NOT(MODE EQ 'ALGEBRAIC)
	       THEN CONVERTMODE(Y,VARS,MODE,'ALGEBRAIC)
	      ELSE ('LIST . MKQUOTE CAR U . X)
   END;

SYMBOLIC PROCEDURE FORMLIS(U,VARS,MODE);
   MAPCAR(U,FUNCTION (LAMBDA X; FORM1(X,VARS,MODE)));

SYMBOLIC PROCEDURE FORMCLIS(U,VARS,MODE);
   MAPCAR(U,FUNCTION (LAMBDA X; FORMC(X,VARS,MODE)));

SYMBOLIC PROCEDURE FORM U; FORM1(U,!*VARS!*,!*MODE);

SYMBOLIC PROCEDURE MACROCHK(U,MODE);
   BEGIN SCALAR Y;
   %expands U if CAR U is a macro and expansion allowed;
      IF ATOM U THEN RETURN U
       ELSE IF (Y := EXPDRMACRO CAR U)
	AND (MODE EQ 'SYMBOLIC OR IDP CAR U AND FLAGP(CAR U,'OPFN))
	THEN RETURN APPLY(GET(CAR Y,'MACROFN),LIST(CDR Y,CDR U,CAR U))
       ELSE RETURN U
   END;

PUT('SYMBOLIC,'IDFN,'SYMBID);

SYMBOLIC PROCEDURE SYMBID(U,VARS); U;
%   IF ATSOC(U,VARS) OR FLUIDP U OR GLOBALP U OR U MEMQ '(NIL T) 
%	OR FLAGP(U,'SHARE) THEN U
%    ELSE <<LPRIM LIST(U,"Non-Local Identifier");% U>>;

PUT('ALGEBRAIC,'IDFN,'ALGID);

SYMBOLIC PROCEDURE ALGID(U,VARS);
   IF ATSOC(U,VARS) OR FLAGP(U,'SHARE) THEN U ELSE MKQUOTE U;

PUT('INTEGER,'IDFN,'INTID);

SYMBOLIC PROCEDURE INTID(U,VARS);
   BEGIN SCALAR X,Y;
      RETURN IF (X := ATSOC(U,VARS))
	THEN IF CDR X EQ 'INTEGER THEN U
	       ELSE IF Y := GET(CDR X,'INTEGER)
		THEN APPLY(Y,LIST(U,VARS))
	       ELSE IF CDR X EQ 'SCALAR THEN !*!*A2I(U,VARS)
	       ELSE REDERR LIST(CDR X,"not convertable to INTEGER")
      ELSE !*!*A2I(MKQUOTE U,VARS)
   END;

SYMBOLIC PROCEDURE CONVERTMODE(EXPRN,VARS,TARGET,SOURCE);
   CONVERTMODE1(FORM1(EXPRN,VARS,SOURCE),VARS,TARGET,SOURCE);

SYMBOLIC PROCEDURE CONVERTMODE1(EXPRN,VARS,TARGET,SOURCE);
   BEGIN SCALAR X;
%      EXPRN := FORM1(EXPRN,VARS,SOURCE);
      IF TARGET EQ SOURCE THEN RETURN EXPRN
       ELSE IF IDP EXPRN AND (X := ATSOC(EXPRN,VARS))
	  AND NOT(CDR X EQ 'SCALAR) AND NOT(CDR X EQ SOURCE)
	THEN RETURN CONVERTMODE(EXPRN,VARS,TARGET,CDR X)
       ELSE IF NOT (X := GET(SOURCE,TARGET))
	THEN TYPERR(SOURCE,TARGET)
       ELSE RETURN APPLY(X,LIST(EXPRN,VARS))
   END;

PUT('ALGEBRAIC,'SYMBOLIC,'!*!*A2S);

PUT('SYMBOLIC,'ALGEBRAIC,'!*!*S2A);

FLUID '(!*!*A2SFN);

!*!*A2SFN := 'AEVAL;

SYMBOLIC PROCEDURE !*!*A2S(U,VARS);
   IF NULL U OR CONSTANTP U AND NULL FIXP U
      OR INTEXPRNP(U,VARS) AND NULL !*COMPOSITES AND NULL MOD!*
      OR NOT ATOM U AND IDP CAR U
	 AND FLAGP(CAR U,'NOCHANGE) AND NOT(CAR U EQ 'GETEL)
     THEN U
    ELSE IF U = '(QUOTE NIL) THEN NIL
    ELSE LIST(!*!*A2SFN,U);

SYMBOLIC PROCEDURE !*!*S2A(U,VARS); U;

SYMBOLIC PROCEDURE FORMC(U,VARS,MODE);
   %this needs to be generalized;
   IF MODE EQ 'ALGEBRAIC AND INTEXPRNP(U,VARS) THEN U
    ELSE CONVERTMODE(U,VARS,'SYMBOLIC,MODE);

SYMBOLIC PROCEDURE INTARGFN(U,VARS);
   %transforms U into a function with integer arguments.
   %We assume that the analysis is done in algebraic mode;
   'LIST . FORM1(CAR U,VARS,'ALGEBRAIC) . 
       MAPCAR(CDR U,
	      FUNCTION (LAMBDA X;
			CONVERTMODE(X,VARS,'INTEGER,'ALGEBRAIC)));

PUT('ALGEBRAIC,'INTEGER,'!*!*A2I);

SYMBOLIC PROCEDURE !*!*A2I(U,VARS);
   IF INTEXPRNP(U,VARS) THEN U ELSE LIST('!*S2I,LIST('REVAL,U));

PUT('SYMBOLIC,'INTEGER,'!*!*S2I);

SYMBOLIC PROCEDURE !*!*S2I(U,VARS);
   IF NUMBERP U AND FIXP U THEN U ELSE LIST('!*S2I,U);

SYMBOLIC PROCEDURE !*S2I U;
   IF NUMBERP U AND FIXP U THEN U ELSE TYPERR(U,"integer");

PUT('INTEGER,'SYMBOLIC,'IDENTITY);

SYMBOLIC PROCEDURE IDENTITY(U,VARS); U;

SYMBOLIC PROCEDURE FORMBOOL(U,VARS,MODE);
   IF MODE EQ 'SYMBOLIC THEN FORM1(U,VARS,MODE)
    ELSE IF ATOM U THEN IF NOT IDP U OR ATSOC(U,VARS) OR U EQ 'T
	   THEN U
	  ELSE FORMC!*(U,VARS,MODE)
    ELSE IF INTEXPRLISP(CDR U,VARS) AND GET(CAR U,'BOOLFN) THEN U
    ELSE IF IDP CAR U AND GET(CAR U,'BOOLFN)
     THEN GET(CAR U,'BOOLFN) . FORMCLIS(CDR U,VARS,MODE)
    ELSE IF IDP CAR U AND FLAGP(CAR U,'BOOLEAN)
	THEN CAR U .
	  MAPCAR(CDR U,FUNCTION (LAMBDA X;
	    IF FLAGP(CAR U,'BOOLARGS)
		      THEN FORMBOOL(X,VARS,MODE)
		     ELSE FORMC!*(X,VARS,MODE)))
    ELSE FORMC!*(U,VARS,MODE);

SYMBOLIC PROCEDURE FORMC!*(U,VARS,MODE);
   BEGIN SCALAR !*!*A2SFN;
      !*!*A2SFN := 'REVAL;
      RETURN FORMC(U,VARS,MODE)
   END;

SYMBOLIC PROCEDURE FORMSETQ(U,VARS,MODE);
   BEGIN SCALAR TARGET,X,Y;
     IF EQCAR(CADR U,'QUOTE) THEN MODE := 'SYMBOLIC;
      IF IDP CAR U
	   AND (Y := ATSOC(CAR U,VARS)) AND NOT(CDR Y EQ 'SCALAR)
	THEN TARGET := CDR Y
      ELSE TARGET := 'SYMBOLIC;
      X := CONVERTMODE(CADR U,VARS,TARGET,MODE);
      RETURN IF NOT ATOM CAR U
	THEN IF NOT IDP CAAR U THEN TYPERR(CAR U,"assignment")
	  ELSE IF ARRAYP CAAR U
	    AND (MODE EQ 'SYMBOLIC OR INTEXPRLISP(CDAR U,VARS))
	   THEN LIST('SETEL,INTARGFN(CAR U,VARS),X)
	  ELSE IF Y := GET(CAAR U,'SETQFN) 
	   THEN FORM1((Y . APPEND(CDAR U,CDR U)),VARS,MODE)
	  ELSE LIST('SETK,FORM1(CAR U,VARS,MODE),X)
    ELSE IF NOT IDP CAR U THEN TYPERR(CAR U,"assignment")
    ELSE IF MODE EQ 'SYMBOLIC OR Y OR FLAGP(CAR U,'SHARE)
	 OR EQCAR(X,'QUOTE)
     THEN MKSETQ(CAR U,X)
    ELSE LIST('SETK,MKQUOTE CAR U,X)
   END;

PUT('CAR,'SETQFN,'RPLACA);

PUT('CDR,'SETQFN,'RPLACD);

PUT('SETQ,'FORMFN,'FORMSETQ);

SYMBOLIC PROCEDURE FORMFUNC(U,VARS,MODE);
   IF IDP CAR U THEN IF GETRMACRO CAR U
     THEN REDERR LIST("Macro",CAR U,"Used as Function")
	ELSE LIST('FUNCTION,CAR U)
    ELSE LIST('FUNCTION,FORM1(CAR U,VARS,MODE));

PUT('FUNCTION,'FORMFN,'FORMFUNC);

SYMBOLIC PROCEDURE FORMRLIS(U,VARS,MODE);
   IF NOT FLAGP(CAR U,'FLAGOP)
	THEN LIST(CAR U,'LIST . FORMLIS(CDR U,VARS,'ALGEBRAIC))
    ELSE MKPROG(NIL,LIST('FLAG,MKQUOTE CDR U,MKQUOTE CAR U)
			     . GET(CAR U,'SIMPFG));

SYMBOLIC PROCEDURE MKARG(U,VARS);
   %returns the "unevaled" form of U;
   IF NULL U OR CONSTANTP U THEN U
    ELSE IF ATOM U THEN IF ATSOC(U,VARS) THEN U ELSE MKQUOTE U
    ELSE IF CAR U EQ 'QUOTE THEN MKQUOTE U
    ELSE 'LIST . MAPCAR(U,FUNCTION (LAMBDA X; MKARG(X,VARS)));


%*********************************************************************
%                         PROCEDURE STATEMENT
%********************************************************************;

FTYPES!* := '(EXPR FEXPR MACRO);

FLUID '(!*COMP);

SYMBOLIC PROCEDURE PUTC(NAME,TYPE,BODY);
   %defines a non-standard function, such as an smacro. Returns NAME;
   BEGIN
      IF !*COMP AND FLAGP(TYPE,'COMPILE) THEN COMPD(NAME,TYPE,BODY)
       ELSE PUT(NAME,TYPE,BODY);
      RETURN NAME
   END;

SYMBOLIC PROCEDURE PAIRXVARS(U,V,VARS,MODE);
   %Pairs procedure variables and their modes, taking into account
   %the convention which allows a top level prog to change the mode
   %of such a variable;
   BEGIN SCALAR X,Y;
   A: IF NULL U THEN RETURN APPEND(REVERSIP X,VARS) . V
       ELSE IF (Y := ATSOC(CAR U,V))
	THEN <<V := DELETE(Y,V);
	       IF NOT(CDR Y EQ 'SCALAR) THEN X := (CAR U . CDR Y) . X
		ELSE X := (CAR U . MODE) . X>>
       ELSE X := (CAR U . MODE) . X;
      U := CDR U;
      GO TO A
   END;

SYMBOLIC PROCEDURE FORMPROC(U,VARS,MODE);
   BEGIN SCALAR BODY,NAME,TYPE,VARLIS,X,Y;
	NAME := CAR U;
	IF CADR U THEN MODE := CADR U;   %overwrite previous mode;
	U := CDDR U;
	TYPE := CAR U;
	IF FLAGP(NAME,'LOSE) AND (!*LOSE OR NULL !*DEFN)
	  THEN RETURN PROGN(LPRIM LIST(NAME,
			    "not defined (LOSE flag)"),
			NIL);
	VARLIS := CADR U;
	U := CADDR U;
	X := IF EQCAR(U,'BLOCK) THEN CADR U ELSE NIL;
	Y := PAIRXVARS(VARLIS,X,VARS,MODE);
	IF X THEN RPLACA(CDR U,CDR Y);
	BODY:= FORM1(U,CAR Y,MODE);
	IF TYPE EQ 'EXPR THEN BODY := LIST('DE,NAME,VARLIS,BODY)
	 ELSE IF TYPE EQ 'FEXPR THEN BODY := LIST('DF,NAME,VARLIS,BODY)
         ELSE IF TYPE EQ 'MACRO THEN BODY := LIST('DM,NAME,VARLIS,BODY)
	 ELSE IF TYPE EQ 'EMB THEN RETURN EMBFN(NAME,VARLIS,BODY)
	 ELSE BODY := LIST('PUTC,
			   MKQUOTE NAME,
			   MKQUOTE TYPE,
			   MKQUOTE LIST('LAMBDA,VARLIS,BODY));
	IF NOT(MODE EQ 'SYMBOLIC)
	  THEN BODY := LIST('PROGN,
			 LIST('FLAG,MKQUOTE LIST NAME,MKQUOTE 'OPFN),
			  BODY);
	IF !*DEFN AND TYPE MEMQ '(MACRO SMACRO)
	  THEN EVAL BODY;
	RETURN BODY
   END;

PUT('PROCEDURE,'FORMFN,'FORMPROC);

SYMBOLIC PROCEDURE PROCSTAT1 MODE;
   BEGIN SCALAR BOOL,U,TYPE,X,Y,Z;
	BOOL := ERFG!*;
	IF FNAME!* THEN GO TO B
	 ELSE IF CURSYM!* EQ 'PROCEDURE THEN TYPE := 'EXPR
	 ELSE PROGN(TYPE := CURSYM!*,SCAN());
	IF NOT CURSYM!* EQ 'PROCEDURE THEN GO TO C;
	X := ERRORSET('(XREAD (QUOTE PROC)),NIL,!*BACKTRACE);
	IF ERRORP X THEN GO TO A
	 ELSE IF ATOM (X := CAR X) THEN X := LIST X;   %no arguments;
	FNAME!* := CAR X;   %function name;
	IF IDP FNAME!* %AND NOT(TYPE MEMQ FTYPES!*);
	  THEN IF NULL FNAME!* OR (Z := GETTYPE FNAME!*)
			AND NOT Z MEMQ '(PROCEDURE OPERATOR)
		THEN GO TO D
	      ELSE IF NOT GETD FNAME!* THEN FLAG(LIST FNAME!*,'FNC);
	   %to prevent invalid use of function name in body;
	U := CDR X;
	Y := U;
	X := CAR X . Y;
    A:	Z := ERRORSET('(XREAD T),NIL,!*BACKTRACE);
	IF NOT ERRORP Z THEN Z := CAR Z;
	IF NULL ERFG!* THEN Z:=LIST('PROCEDURE,CAR X,MODE,TYPE,Y,Z);
	REMFLAG(LIST FNAME!*,'FNC);
	FNAME!*:=NIL;
	IF ERFG!* THEN PROGN(Z := NIL,IF NOT BOOL THEN ERROR1());
	RETURN Z;
    B:	BOOL := T;
    C:	ERRORSET('(SYMERR (QUOTE PROCEDURE) T),NIL,!*BACKTRACE);
	GO TO A;
    D:  TYPERR(LIST(Z,FNAME!*),"procedure");
	GO TO A
   END;

SYMBOLIC PROCEDURE PROCSTAT; PROCSTAT1 NIL;

DEFLIST ('((PROCEDURE PROCSTAT) (EXPR PROCSTAT) (FEXPR PROCSTAT)
	   (EMB PROCSTAT)
	   (MACRO PROCSTAT) (SMACRO PROCSTAT)),
	'STAT);

DEFLIST ('((ALGEBRAIC MODESTAT) (SYMBOLIC MODESTAT)),
	 'STAT);

DEFLIST('((LISP SYMBOLIC)),'NEWNAM);

COMMENT Defining GEQ, LEQ and NEQ as SMACROS;

SMACRO PROCEDURE U>=V; NOT(U<V);

SMACRO PROCEDURE U<=V; NOT(U>V);

SMACRO PROCEDURE U NEQ V; NOT(U=V);


%*********************************************************************
%                            END STATEMENT
%********************************************************************;

SYMBOLIC PROCEDURE ENDSTAT;
  %This procedure can also be used for any key-words  which  take  no
  %arguments;
   BEGIN SCALAR X;
	X := CURSYM!*;
	COMM1 'END;
	RETURN LIST X
   END;

PUT('END,'STAT,'ENDSTAT);

PUT('BYE,'STAT,'ENDSTAT);

PUT('QUIT,'STAT,'ENDSTAT);

FLAG('(BYE QUIT),'EVAL);

PUT('SHOWTIME,'STAT,'ENDSTAT);


%*********************************************************************
%*********************************************************************
%			  MODULAR STATEMENTS
%*********************************************************************
%********************************************************************;

%	 The remaining statements defined in this section are truly
%modular, and any may be omitted if desired.


%*********************************************************************
%            FUNCTIONS FOR INTRODUCING NEW INFIX OPERATORS
%********************************************************************;

SYMBOLIC PROCEDURE INFIX X;
   BEGIN SCALAR Y;
	 IF !*MODE EQ 'ALGEBRAIC THEN MAPCAR(X,FUNCTION MKOP);
	IF Y := XN(X,PRECLIS!*) THEN LPRIM APPEND(Y,'(REDEFINED));
	 PRECLIS!* := APPEND(REVERSE X,SETDIFF(PRECLIS!*,X));
	 MKPREC()
   END;

SYMBOLIC PROCEDURE PRECEDENCE U;
   BEGIN SCALAR X,Y,Z;
	 PRECLIS!* := DELETE(CAR U,PRECLIS!*);
	 Y := CADR U;
	 X := PRECLIS!*;
    A:   IF NULL X THEN REDERR LIST (Y,"not found")
	  ELSE IF Y EQ CAR X THEN GO TO B;
	 Z := CAR X . Z;
	 X := CDR X;
	 GO TO A;
    B:	 PRECLIS!* := NCONC(REVERSIP Z,CAR X . (CAR U . CDR X));
	 MKPREC()
   END;

RLISTAT '(INFIX PRECEDENCE);

FLAG('(INFIX PRECEDENCE),'EVAL);


%*********************************************************************
%                            FOR STATEMENT
%********************************************************************;

%REMPROP('FOR,'STAT); %in case rebuilding system on top of itself;

SYMBOLIC PROCEDURE FORLOOP;
   BEGIN SCALAR ACTION,BODY,INCR,VAR,X;
      X := XREAD1 'FOR;
      IF ATOM X OR NOT CAR X MEMQ '(EQUAL SETQ) THEN SYMERR('FOR,T);
      VAR := CADR X;
      X := CADDR X;
      IF NOT IDP VAR THEN SYMERR('FOR,T);
%      VAR := CAR FLAGTYPE(LIST VAR,'INTEGER);
      IF CURSYM!* EQ 'STEP
	THEN <<INCR := XREAD T;
		IF NOT CURSYM!* EQ 'UNTIL THEN SYMERR('FOR,T)>>
       ELSE IF CURSYM!* EQ '!*COLON!* THEN INCR := 1
       ELSE SYMERR('FOR,T);
      INCR := LIST(X,INCR,XREAD T);
      IF NOT GET(ACTION := CURSYM!*,'BIN) AND NOT ACTION EQ 'DO
	THEN SYMERR('FOR,T);
      BODY := XREAD T;
%      REMTYPE LIST VAR;
      RETURN LIST('FOR,VAR,INCR,ACTION,BODY)
   END;

SYMBOLIC PROCEDURE FORMFOR(U,VARS,MODE);
   LIST('FOR,CAR U,
	 MAPCAR(CADR U,FUNCTION (LAMBDA X; FORMC(X,VARS,MODE))),
	 CADDR U,
	 FORMC(CADDDR U,
	       (CAR U . IF INTEXPRLISP(CADR U,VARS)
			  THEN 'INTEGER ELSE MODE) . VARS,MODE));

PUT('FOR,'FORMFN,'FORMFOR);

SYMBOLIC PROCEDURE INTEXPRNP(U,VARS);
   %determines if U is an integer expression;
    IF ATOM U THEN IF NUMBERP U THEN FIXP U
	           ELSE IF (U := ATSOC(U,VARS)) THEN CDR U EQ 'INTEGER
		   ELSE NIL
     ELSE IDP CAR U AND FLAGP(CAR U,'INTFN) AND INTEXPRLISP(CDR U,VARS);

SYMBOLIC PROCEDURE INTEXPRLISP(U,VARS);
   NULL U OR INTEXPRNP(CAR U,VARS) AND INTEXPRLISP(CDR U,VARS);

FLAG('(DIFFERENCE EXPT MINUS PLUS TIMES),'INTFN);

SYMBOLIC MACRO PROCEDURE FOR U;
   BEGIN SCALAR ACTION,ALGP,BODY,EXP,INCR,LAB1,LAB2,RESULT,TAIL,VAR,X;
	%ALGP is used to determine if the loop calculation must be
	%done algebraically or not;
      VAR := CADR U;
      INCR := CADDR U;
      ACTION := CADDDR U;
      BODY := CAR CDDDDR U;
      IF ALGMODEP CAR INCR OR ALGMODEP CADR INCR
	OR ALGMODEP CADDR INCR THEN ALGP := T;
      RESULT := LIST LIST('SETQ,VAR,CAR INCR);
      INCR := CDR INCR;
      X := IF ALGP THEN LIST('LIST,MKQUOTE 'DIFFERENCE,CADR INCR,VAR)
	    ELSE LIST('DIFFERENCE,CADR INCR,VAR);
      IF CAR INCR NEQ 1
	THEN X := IF ALGP THEN LIST('LIST,MKQUOTE 'TIMES,CAR INCR,X)
	           ELSE LIST('TIMES,CAR INCR,X);
      IF NOT ACTION EQ 'DO
	THEN <<ACTION := GET(ACTION,'BIN);
		EXP := GENSYM();
		BODY := LIST('SETQ,EXP,
			      LIST(CAR ACTION,LIST('SIMP,BODY),EXP));
		RESULT := LIST('SETQ,EXP,MKQUOTE CDR ACTION) . RESULT;
		TAIL := LIST LIST('RETURN,LIST('MK!*SQ,EXP));
		EXP := LIST EXP>>;
      LAB1 := GENSYM();
      LAB2 := GENSYM();
      X := IF ALGP THEN LIST('AMINUSP!:,X) ELSE LIST('MINUSP,X);
      RESULT := NCONC(RESULT,
		 LAB1 .
		LIST('COND,LIST(X,LIST('GO,LAB2))) .
		BODY .
		LIST('SETQ,VAR,
		     IF ALGP
		       THEN LIST('AEVAL,
				LIST('LIST,MKQUOTE 'PLUS,VAR,CAR INCR))
		      ELSE LIST('PLUS2,VAR,CAR INCR)) .
		LIST('GO,LAB1) .
		LAB2 .
		TAIL);
      RETURN MKPROG(VAR . EXP,RESULT)
   END;

SYMBOLIC PROCEDURE ALGMODEP U; EQCAR(U,'AEVAL);

SYMBOLIC PROCEDURE AMINUSP!: U;
   BEGIN SCALAR X;
      U := AEVAL U;
      X := U;
      IF FIXP X THEN RETURN MINUSP X
       ELSE IF NOT EQCAR(X,'!*SQ)
	THEN MSGPRI(NIL,REVAL U,"invalid in FOR statement",NIL,T);
      X := CADR X;
      IF FIXP CAR X AND FIXP CDR X THEN RETURN MINUSP CAR X
       ELSE IF NOT CDR X = 1
	     OR NOT DOMAINP (X := CAR X) 
	THEN MSGPRI(NIL,REVAL U,"invalid in FOR statement",NIL,T)
       ELSE RETURN APPLY('!:MINUSP,LIST X)
   END;

FLAG('(FOR),'NOCHANGE);

SYMBOLIC PROCEDURE FORSTAT;
   IF SCAN() EQ 'ALL THEN FORALLSTAT()
    ELSE IF CURSYM!* EQ 'EACH THEN FOREACHSTAT()
    ELSE FORLOOP();

PUT('FOR,'STAT,'FORSTAT);

FLAG ('(STEP DO UNTIL),'DELIM);


%*********************************************************************
%			  FOR EACH STATEMENT
%********************************************************************;

SYMBOLIC PROCEDURE FORMFOREACH(U,VARS,MODE);
   LIST('FOREACH,CAR U,CADR U,FORMC(CADDR U,VARS,MODE),CADDDR U,
         FORMC(CAR CDDDDR U,(CAR U . MODE) . VARS,MODE));

PUT('FOREACH,'FORMFN,'FORMFOREACH);

SYMBOLIC PROCEDURE FOREACHSTAT;
   BEGIN SCALAR W,X,Y,Z;
	X := SCAN();
	Y := SCAN();
	IF NOT Y MEMQ '(IN ON) THEN SYMERR("FOR EACH",T);
	IF FLAGP('CONC,'DELIM) THEN W := T
	 ELSE FLAG('(COLLECT CONC),'DELIM);
	Z := XREAD T;
	IF NULL W THEN REMFLAG('(COLLECT CONC),'DELIM);
	W := CURSYM!*;
	IF NOT W MEMQ '(DO COLLECT CONC)
	  THEN SYMERR("FOR EACH",T);
	RETURN LIST('FOREACH,X,Y,Z,W,XREAD T)
   END;

PUT('FOREACH,'STAT,'FOREACHSTAT);

SYMBOLIC MACRO PROCEDURE FOREACH U;
   BEGIN SCALAR ACTION,BODY,FN,LST,MOD,VAR;
	VAR := CADR U; U := CDDR U;
	MOD := CAR U; U := CDR U;
	LST := CAR U; U := CDR U;
	ACTION := CAR U; U := CDR U;
	BODY := CAR U;
	FN := IF ACTION EQ 'DO THEN IF MOD EQ 'IN THEN 'MAPC ELSE 'MAP
		ELSE IF ACTION EQ 'CONC
		 THEN IF MOD EQ 'IN THEN 'MAPCAN ELSE 'MAPCON
		ELSE IF ACTION EQ 'COLLECT
		 THEN IF MOD EQ 'IN THEN 'MAPCAR ELSE 'MAPLIST
		ELSE REDERR LIST(ACTION,"invalid in FOREACH statement");
	RETURN LIST(FN,LST,LIST('FUNCTION,LIST('LAMBDA,LIST VAR,BODY)))
   END;

%*********************************************************************
%			   REPEAT STATEMENT
%********************************************************************;

SYMBOLIC PROCEDURE FORMREPEAT(U,VARS,MODE);
   LIST('REPEAT,FORMC(CAR U,VARS,MODE),FORMBOOL(CADR U,VARS,MODE));

PUT('REPEAT,'FORMFN,'FORMREPEAT);

SYMBOLIC PROCEDURE REPEATSTAT;
  BEGIN SCALAR BODY;
	BODY:= XREAD T;
	IF NOT CURSYM!* EQ 'UNTIL THEN SYMERR('REPEAT,T);
	RETURN LIST('REPEAT,BODY,XREAD T);
   END;

PUT('REPEAT,'STAT,'REPEATSTAT);

MACRO PROCEDURE REPEAT U;
   BEGIN SCALAR BODY,BOOL,LAB;
	BODY := CADR U; BOOL := CADDR U;
	LAB := GENSYM();
	RETURN MKPROG(NIL,LIST(LAB,BODY,
		LIST('COND,LIST(LIST('NOT,BOOL),LIST('GO,LAB)))))
   END;

FLAG('(REPEAT),'NOCHANGE);

%*********************************************************************
%			    WHILE STATEMENT
%********************************************************************;

SYMBOLIC PROCEDURE FORMWHILE(U,VARS,MODE);
   LIST('WHILE,FORMBOOL(CAR U,VARS,MODE),FORMC(CADR U,VARS,MODE));

PUT('WHILE,'FORMFN,'FORMWHILE);

SYMBOLIC PROCEDURE WHILSTAT;
   BEGIN SCALAR BOOL;
	BOOL := XREAD T;
	IF NOT CURSYM!* EQ 'DO THEN SYMERR('WHILE,T);
	RETURN LIST('WHILE,BOOL,XREAD T)
   END;

PUT('WHILE,'STAT,'WHILSTAT);

MACRO PROCEDURE WHILE U;
   BEGIN SCALAR BODY,BOOL,LAB;
	BOOL := CADR U; BODY := CADDR U;
	LAB := GENSYM();
	RETURN MKPROG(NIL,LIST(LAB,LIST('COND,LIST(LIST('NOT,BOOL),
		LIST('RETURN,NIL))),BODY,LIST('GO,LAB)))
   END;

FLAG('(WHILE),'NOCHANGE);


%*********************************************************************
%                           ARRAY STATEMENT
%********************************************************************;

SYMBOLIC PROCEDURE GETEL U;
   %returns the value of the array element U;
   GETEL1(GET(CAR U,'ARRAY),CDR U);

SYMBOLIC PROCEDURE GETEL1(U,V);
   IF NULL V THEN U ELSE GETEL1(GETV(U,CAR V),CDR V);

SYMBOLIC PROCEDURE SETEL(U,V);
   %Sets array element U to V and returns V;
   SETEL1(GET(CAR U,'ARRAY),CDR U,V);

SYMBOLIC PROCEDURE SETEL1(U,V,W);
   IF NULL CDR V THEN PUTV(U,CAR V,W)
    ELSE SETEL1(GETV(U,CAR V),CDR V,W);

SYMBOLIC PROCEDURE DIMENSION U;
 GET(U,'DIMENSION);


COMMENT further support for REDUCE arrays;

SYMBOLIC PROCEDURE TYPECHK(U,V);
   BEGIN SCALAR X;
      IF (X := GETTYPE U) EQ V OR X EQ 'PARAMETER
	THEN LPRIM LIST(V,U,"REDEFINED")
       ELSE IF X THEN TYPERR(LIST(X,U),V)
   END;

SYMBOLIC PROCEDURE ARRAYFN(U,V);
   %U is the defining mode, V a list of lists, assumed syntactically
   %correct.
   %ARRAYFN declares each element as an array unless a semantic
   %mismatch occurs;
   BEGIN SCALAR Y;
      FOR EACH X IN V DO
         <<TYPECHK(CAR X,'ARRAY);
           Y := ADD1LIS FOR EACH Z IN CDR X COLLECT EVAL Z;
           IF ERFG!* THEN RETURN NIL;
           PUT(CAR X,'ARRAY,MKARRAY Y);
           PUT(CAR X,'DIMENSION,Y)>>
   END;

SYMBOLIC PROCEDURE ADD1LIS U;
   IF NULL U THEN NIL ELSE (CAR U+1) . ADD1LIS CDR U;

SYMBOLIC PROCEDURE MKARRAY U;
   %U is a list of positive integers representing array bounds.
   %Value is an array structure;
   IF NULL U THEN NIL
    ELSE BEGIN INTEGER N; SCALAR X;
      N := CAR U-1;
      X := MKVECT N;
      FOR I:=0:N DO PUTV(X,I,MKARRAY CDR U);
      RETURN X
   END;

RLISTAT '(ARRAY);

FLAG ('(ARRAY),'EVAL);

SYMBOLIC PROCEDURE FORMARRAY(U,VARS,MODE);
   BEGIN SCALAR X;
      X := U;
      WHILE X DO <<IF ATOM X THEN TYPERR(X,"Array List")
		  ELSE IF ATOM CAR X OR NOT IDP CAAR X
			 OR NOT LISTP CDAR X
		  THEN TYPERR(CAR X,"Array");
		   X := CDR X>>;
      U := FOR EACH Z IN U COLLECT INTARGFN(Z,VARS);
      %ARRAY arguments must be returned as quoted structures;
      RETURN LIST('ARRAYFN,MKQUOTE MODE,'LIST . U)
   END;

SYMBOLIC PROCEDURE LISTP U;
   %returns T if U is a top level list;
   NULL U OR NOT ATOM U AND LISTP CDR U;

PUT('ARRAY,'FORMFN,'FORMARRAY);


%*********************************************************************
%                          ON/OFF STATEMENTS
%********************************************************************;

SYMBOLIC PROCEDURE ON U; ONOFF(U,T);

SYMBOLIC PROCEDURE OFF U; ONOFF(U,NIL);

SYMBOLIC PROCEDURE ONOFF(U,BOOL);
   BEGIN SCALAR X;
      FOR EACH J IN U DO
	IF NOT IDP J THEN TYPERR(J,"ON/OFF argument")
	 ELSE <<SET(INTERN COMPRESS APPEND(EXPLODE '!*,EXPLODE J),BOOL);
		IF X := ATSOC(BOOL,GET(J,'SIMPFG))
		  THEN EVAL MKPROG(NIL,CDR X)>>
   END;

RLISTAT '(OFF ON);


%*********************************************************************
%			   DEFINE STATEMENT
%********************************************************************;

SYMBOLIC PROCEDURE DEFSTAT;
   BEGIN SCALAR X,Y,Z;
    A:	X := SCAN();
    B:	IF FLAGP!*!*(X,'DELIM) THEN RETURN MKPROG(NIL,Z)
	 ELSE IF X EQ '!*COMMA!* THEN GO TO A
	 ELSE IF NOT IDP X THEN GO TO ER;
	Y := SCAN();
	IF NOT Y EQ 'EQUAL THEN GO TO ER;
	Z := ACONC(Z,LIST('PUT,MKQUOTE X,MKQUOTE 'NEWNAM,
				MKQUOTE XREAD T));
	X := CURSYM!*;
	GO TO B;
    ER: SYMERR('DEFINE,T)
   END;

PUT('DEFINE,'STAT,'DEFSTAT);

FLAG('(DEFINE),'EVAL);


%*********************************************************************
%                           WRITE STATEMENT
%********************************************************************;

RLISTAT '(WRITE);

SYMBOLIC PROCEDURE FORMWRITE(U,VARS,MODE);
   BEGIN SCALAR BOOL1,BOOL2,X,Y,Z;
      BOOL1 := MODE EQ 'SYMBOLIC;
      WHILE U DO 
	<<X := FORMC(CAR U,VARS,MODE);
	  Z := (IF BOOL1 THEN LIST('PRIN2,X) 
		      ELSE LIST('VARPRI,X,MKARG(SETVARS X,VARS),
	  IF NOT CDR U THEN IF NOT BOOL2 THEN MKQUOTE 'ONLY ELSE T
	   ELSE IF NOT BOOL2 THEN MKQUOTE 'FIRST ELSE NIL)) .
			     Z;
	  BOOL2 := T;
	  U := CDR U>>;
	RETURN MKPROG(NIL,REVERSIP Z)
   END;

PUT('WRITE,'FORMFN,'FORMWRITE);


%*********************************************************************
%*********************************************************************
%	REDUCE FUNCTIONS FOR HANDLING INPUT AND OUTPUT OF FILES
%*********************************************************************
%********************************************************************;

GLOBAL '(CONTL!*);

SYMBOLIC PROCEDURE IN U;
   BEGIN SCALAR CHAN,ECHO,ECHOP,TYPE;
    ECHOP := SEMIC!* EQ '!;;   %record echo character from input;
    ECHO := !*ECHO;   %save current echo status;
    IF NULL IFL!* THEN TECHO!* := !*ECHO;   %terminal echo status;
    FOR EACH FL IN U DO
      <<IF FL EQ 'T THEN FL := NIL;
	IF NULL FL THEN <<!*ECHO := TECHO!*; IFL!* := NIL>>
	 ELSE <<CHAN := OPEN(FL := MKFIL FL,'INPUT);
		IFL!* := FL . CHAN>>;
	IPL!* := IFL!* . IPL!*;  %add to input file stack;
	RDS (IF IFL!* THEN CDR IFL!* ELSE NIL);
	!*ECHO := ECHOP;
	TYPE := FILETYPE FL;
	IF TYPE AND (TYPE := GET(TYPE,'ACTION)) THEN EVAL LIST TYPE
	 ELSE BEGIN1();
	IF CHAN THEN CLOSE CHAN;
	IF FL EQ CAAR IPL!* THEN IPL!* := CDR IPL!*
	 ELSE ERRACH LIST("FILE STACK CONFUSION",FL,IPL!*)>>;
    !*ECHO := ECHO;   %restore echo status;
    IF IPL!* AND NULL CONTL!* THEN IFL!* := CAR IPL!*
     ELSE IFL!* := NIL;
    RDS(IF IFL!* THEN CDR IFL!* ELSE NIL)
   END;

SYMBOLIC PROCEDURE OUT U;
   %U is a list of one file;
   BEGIN INTEGER N; SCALAR CHAN,FL,X;
	N := LINELENGTH NIL;
	IF NULL U THEN RETURN NIL
	 ELSE IF CAR U EQ 'T THEN RETURN <<WRS(OFL!* := NIL); NIL>>;
	FL := MKFIL CAR U;
	IF NOT (X := ASSOC(FL,OPL!*))
	  THEN <<CHAN := OPEN(FL,'OUTPUT);
		 OFL!* := FL . CHAN;
		 OPL!* := OFL!* . OPL!*>>
	 ELSE OFL!* := X;
	WRS CDR OFL!*;
	LINELENGTH N
   END;

SYMBOLIC PROCEDURE SHUT U;
   %U is a list of names of files to be shut;
   BEGIN SCALAR FL1;
      FOR EACH FL IN U DO
       <<IF FL1 := ASSOC((FL := MKFIL FL),OPL!*) 
	   THEN <<OPL!* := DELETE(FL1,OPL!*);
		  IF FL1=OFL!* THEN <<OFL!* := NIL; WRS NIL>>;
	 	  CLOSE CDR FL1>>
	 ELSE IF NOT (FL1 := ASSOC(FL,IPL!*))
	  THEN REDERR LIST(FL,"not open")
	 ELSE IF FL1 NEQ IFL!*
	  THEN <<CLOSE CDR FL1; IPL!* := DELETE(FL1,IPL!*)>>
	 ELSE REDERR LIST("Cannot shut current input file",CAR FL1)>>
   END;

DEFLIST ('((IN RLIS) (OUT RLIS) (SHUT RLIS)),'STAT);


%*********************************************************************
%		FUNCTIONS HANDLING INTERACTIVE FEATURES
%********************************************************************;

%GLOBAL Variables referenced in this Section;

GLOBAL '(FLG!* CLOC!* EDIT!*);

CONTL!* := NIL;

SYMBOLIC PROCEDURE PAUSE;
   %Must appear at the top-most level;
   IF KEY!* EQ 'PAUSE THEN PAUSE1 NIL
    ELSE %TYPERR('PAUSE,"lower level command");
	 PAUSE1 NIL;   %Allow at lower level for now;

SYMBOLIC PROCEDURE PAUSE1 BOOL;
   BEGIN
      IF BOOL THEN
%	IF NULL IFL!*
%	  THEN RETURN NIL ELSE;
	IF GETD 'EDIT1 AND ERFG!* AND CLOC!* AND YESP "Edit?"
	  THEN RETURN <<CONTL!* := NIL;
	   IF OFL!* THEN <<LPRIM LIST(CAR OFL!*,'SHUT);
			   CLOSE CDR OFL!*;
			   OPL!* := DELETE(OFL!*,OPL!*);
			   OFL!* := NIL>>;
	   EDIT1(CLOC!*,NIL)>>
	 ELSE IF FLG!* THEN RETURN (EDIT!* := NIL);
      IF NULL IFL!* OR YESP "Cont?" THEN RETURN NIL;
      CONTL!* := IFL!* . !*ECHO . CONTL!*;
      RDS (IFL!* := NIL);
      !*ECHO := TECHO!*
   END;

SYMBOLIC PROCEDURE YESP U;
   BEGIN SCALAR BOOL,IFL,OFL,X,Y,Z;
	IF IFL!* THEN <<IFL:= IFL!*; RDS NIL>>;
	IF OFL!* THEN <<OFL:= OFL!*; WRS NIL>>;
	TERPRI();
	IF ATOM U THEN PRIN2 U ELSE LPRI U;
	PRIN2T " (Y or N)";
	TERPRI();
	Z := SETPCHAR '!?;
    A:	X := READ();
	IF (Y := (X EQ 'Y)) OR X EQ 'N THEN GO TO B;
	IF NULL BOOL THEN PRIN2T "TYPE Y OR N";
	BOOL := T;
	GO TO A;
    B:	SETPCHAR Z;
	IF OFL THEN WRS CDR OFL;
	IF IFL THEN RDS CDR IFL;
	CURSYM!* := '!*SEMICOL!*;
	RETURN Y
   END;

SYMBOLIC PROCEDURE CONT;
   BEGIN SCALAR FL,TECHO;
	IF IFL!* THEN RETURN NIL   %CONT only active from terminal;
	 ELSE IF NULL CONTL!* THEN REDERR "No file open";
	FL := CAR CONTL!*;
	TECHO := CADR CONTL!*;
	CONTL!* := CDDR CONTL!*;
	IF FL=CAR IPL!* THEN <<IFL!* := FL;
			       RDS IF FL THEN CDR FL ELSE NIL;
			       !*ECHO := TECHO>>
	 ELSE <<EOF!* :=T; LPRIM LIST(FL,"not open"); ERROR1()>>
   END;

DEFLIST ('((PAUSE ENDSTAT) (CONT ENDSTAT) (RETRY ENDSTAT)),'STAT);

PUT('RETRY,'STAT,'ENDSTAT);

FLAG ('(CONT),'IGNORE);


END;
