%  <PSL.UTIL>RLISP-SUPPORT.RED.14, 07-Apr-83 13:34:02, Edit by KENDZIERSKI
%  Changed !*OUTPUT and SEMIC!* to fluid from global to agree w/kernel decls.
%  <PSL.UTIL>RLISP-SUPPORT.RED.8, 13-Oct-82 10:21:02, Edit by BENSON
%  !*INT is globally T
%  <PSL.UTIL>RLISP-SUPPORT.RED.5,  5-Oct-82 11:05:30, Edit by BENSON
%  Changed SaveSystem to 3 arguments
%  <PSL.UTIL>RLISP-SUPPORT.RED.3, 20-Sep-82 11:57:21, Edit by BENSON
%  Added Begin1 and BeginRlisp to IgnoredInBacktrace!*

CompileTime REMPROP('SHOWTIME,'STAT);
                  
%*********************************************************************
%	RLISP and REDUCE Support Code for NEW-RLISP / On PSL
%********************************************************************;


GLOBAL '(FLG!*);

GLOBAL '(BLOCKP!* CMSG!* ERFG!* INITL!* LETL!*
	PRECLIS!* VARS!* !*FORCE
	CLOC!*
        !*DEMO
	!*QUIET
        OTIME!* !*SLIN LREADFN!* TSLIN!*
	!*NAT NAT!*!* CRCHAR!* IFL!* IPL!* KEY!* KEY1!*
	OFL!* OPL!* PROGRAM!* PROGRAML!*
	EOF!* TECHO!* !*INT !*MODE
	!*CREF !*MSG !*PRET !*EXTRAECHO);

FLUID '(!*DEFN !*ECHO DFPRINT!* !*TIME !*BACKTRACE CURSYM!* SEMIC!* !*OUTPUT);

%	These global variables divide into two classes. The first
%class are those which must be initialized at the top level of the
%program. These are as follows;

BLOCKP!* := NIL;	%keeps track of which block is active;
CMSG!* := NIL;		%shows that continuation msg has been printed;
EOF!* := NIL;		%flag indicating an end-of-file;
ERFG!* := NIL;		%indicates that an input error has occurred;
INITL!* := '(BLOCKP!* VARS!*);
			%list of variables initialized in BEGIN1;
KEY!* := 'SYMBOLIC;	%stores first word read in command;
LETL!* := NIL;		%used in algebraic mode for special delimiters;
LREADFN!* := NIL;	%used to define special reading function;
%OUTL!* := NIL;		%storage for output of input line;
PRECLIS!*:= '(OR AND NOT MEMBER MEMQ EQUAL NEQ EQ GEQ GREATERP LEQ
	      LESSP PLUS DIFFERENCE TIMES QUOTIENT EXPT CONS);
			%precedence list of infix operators;
TECHO!* := NIL; 	%terminal echo status;
VARS!* := NIL;		%list of current bound variables during parse;
!*BACKTRACE := NIL;	%if ON, prints a LISP backtrace;
!*CREF := NIL;		%used by cross-reference program;
!*DEMO := NIL;		% causes a PAUSE (READCH) in COMMAND loop
!*ECHO := NIL;		%indicates echoing of input;
!*FORCE := NIL; 	%causes all macros to expand;
!*INT := T;		% system is interactive
%!*LOSE := T;		%determines whether a function flagged LOSE
			%is defined;
%!*MSG:=NIL;		%flag to indicate whether messages should be
			%printed;
!*NAT := NIL;		%used in algebraic mode to denote 'natural'
			%output. Must be on in symbolic mode to
			%ensure input echoing;
NAT!*!* := NIL; 	%temporary variable used in algebraic mode;
!*OUTPUT := T;		%used to suppress output;
!*SLIN := NIL;		%indicates that LISP code should be read;
!*TIME := NIL;		%used to indicate timing should be printed;

%	 The second class are those global variables which are
%initialized within some function, although they do not appear in that
%function's variable list. These are;

% CRCHAR!*		next character in input line
% CURSYM!*		current symbol (i. e. identifier, parenthesis,
%			delimiter, e.t.c,) in input line
% FNAME!*		name of a procedure being read
% FTYPES!*		list of regular procedure types
% IFL!* 		input file/channel pair - set in BEGIN to NIL
% IPL!* 		input file list- set in BEGIN to NIL
% KEY1!*		current key-word being analyzed - set in RLIS1;
% NXTSYM!*		next symbol read in TOKEN
% OFL!* 		output file/channel pair - set in BEGIN to NIL
% OPL!* 		output file list- set in BEGIN to NIL
% PROGRAM!*		current input program
% PROGRAML!*		stores input program when error occurs for a
%			later restart
% SEMIC!*		current delimiter character (used to decide
%			whether to print result of calculation)
% TTYPE!*		current token type;
% WS 			used in algebraic mode to store top level value
% !*FORT		used in algebraic mode to denote FORTRAN output
% !*INT 		indicates interactive system use
% !*MODE		current mode of calculation
% !*PRET		indicates REDUCE prettyprinting of input;


fluid '(IgnoredInBacktrace!*);
IgnoredInBacktrace!* := Append(IgnoredInBacktrace!*, '(Begin1 BeginRlisp));

CompileTime flag('(FlagP!*!* CondTerPri
		   LispFileNameP MkFil SetLispScanTable SetRlispScanTable
		   ProgVr),
		'InternalFunction);

CompileTime <<
macro procedure PgLine U;		% needed for LOCN
    ''(1 . 1);
>>;

%*********************************************************************
%			   REDUCE SUPERVISOR
%********************************************************************;

% The true REDUCE supervisory function is BEGIN, again defined in
%the system dependent part of this program. However, most of the work
%is done by BEGIN1, which is called by BEGIN for every file
%encountered on input;

SYMBOLIC PROCEDURE FLAGP!*!*(U,V);
  IDP U AND FLAGP(U,V);

FLUID '(PROMPTSTRING!*);

fluid '(STATCOUNTER!*);
STATCOUNTER!* := 0;

lisp procedure RlispPrompt();
    BldMsg("[%w] ", StatCounter!*);

put('Symbolic, 'PromptFn, 'RlispPrompt);

SYMBOLIC PROCEDURE BEGIN1;
   BEGIN SCALAR MODE,PARSERR,RESULT,PROMPT,WRKSP,MODEPRINT,PROMPTFN,RESULTL,
	PROMPTSTRING!*;
    A0: CURSYM!* := '!*SEMICOL!*;
	OTIME!* := TIME();
	GO TO A1;
    A:	%IF NULL IFL!* AND !*INT
	 % THEN <<%/CRBUFLIS!* := (STATCOUNTER!* . CRBUF!*) . CRBUFLIS!*;
		% CRBUF!* := NIL>>;
    A1: IF NULL IFL!* AND !*INT THEN STATCOUNTER!* := STATCOUNTER!* + 1;
	IF PROMPTFN := GET(!*MODE,'PROMPTFN) THEN
	  PROMPTSTRING!* := APPLY(PROMPTFN,NIL);
    A2: PARSERR := NIL;
%	IF !*OUTPUT AND !*INT AND NULL IFL!* AND NULL OFL!*
%	    AND NULL !*DEFN
%	  THEN TERPRI();
	IF !*TIME THEN SHOWTIME();
	IF TSLIN!*
	  THEN PROGN(!*SLIN := CAR TSLIN!*,
		     LREADFN!* := CDR TSLIN!*,
		     TSLIN!* := NIL);
	MAPC(INITL!*,FUNCTION SINITL);
	IF !*INT THEN ERFG!* := NIL;	%to make editing work properly;
	IF CURSYM!* EQ 'END THEN GO TO ND0;
	PROGRAM!* := ERRORSET('(COMMAND),T,!*BACKTRACE);
	CONDTERPRI();
	IF ATOM PROGRAM!* OR CDR PROGRAM!* THEN GO TO ERR1;
	PROGRAM!* := CAR PROGRAM!*;
	IF PROGRAM!* EQ !$EOF!$ THEN GO TO ND1
	 ELSE IF EQCAR(PROGRAM!*,'!*COMMA!*) THEN GO TO ER
	 ELSE IF CURSYM!* EQ 'END THEN GO TO ND0
	 ELSE IF EQCAR(PROGRAM!*,'RETRY) THEN PROGRAM!* := PROGRAML!*
;%	 ELSE IF PROGRAM!* EQ 'ED 
%	   THEN PROGN(CEDIT NIL,GO TO A2)
%	 ELSE IF EQCAR(PROGRAM!*,'ED)
%	   THEN PROGN(CEDIT CDR PROGRAM!*,GO TO A2);
	IF !*DEFN THEN GO TO D;
    B:	%IF !*OUTPUT AND IFL!* AND !*ECHO THEN TERPRI();
	RESULTL := ERRORSET(PROGRAM!*,T,!*BACKTRACE);
	IF ATOM RESULTL OR CDR RESULTL OR ERFG!* THEN GO TO ERR2
	 ELSE IF !*DEFN THEN GO TO A;
	RESULT := CAR RESULTL;
	IF IDP KEY!* AND GET(KEY!*,'STAT) EQ 'MODESTAT
	  THEN MODE := KEY!*
	 ELSE MODE := !*MODE;
	IF NULL !*OUTPUT OR IFL!* AND !*QUIET THEN GO TO C;
	IF SEMIC!* EQ '!; THEN <<
	  MODEPRINT := GET(MODE,'MODEPRINFN) OR 'PrintWithFreshLine;
%	  IF NOT FLAGP(MODE,'NOTERPRI) THEN
%	    TERPRI();
	    APPLY(MODEPRINT,RESULTL) >>;
    C:	IF WRKSP := GET(MODE,'WORKSPACE) THEN
	  SET(WRKSP,RESULT);
	GO TO A;
    D:	IF ERFG!* THEN GO TO A
	 ELSE IF FLAGP!*!*(KEY!*,'IGNORE) OR EQCAR(PROGRAM!*,'QUOTE)
	  THEN GO TO B;
	IF PROGRAM!* THEN DFPRINT PROGRAM!*;
	IF FLAGP!*!*(KEY!*,'EVAL) THEN GO TO B ELSE GO TO A;
    ND0:COMM1 'END;
    ND1: EOF!* := NIL;
	IF NULL IPL!*	%terminal END;
	  THEN BEGIN
		IF OFL!* THEN WRS NIL;
	    AA: IF NULL OPL!* THEN RETURN(OFL!* := NIL);
		CLOSE CDAR OPL!*;
		OPL!* := CDR OPL!*;
		GO TO AA
	      END;
	RETURN NIL;
    ERR1:
	IF EOF!* OR PROGRAM!* EQ !$EOF!$ THEN GO TO ND1
	 ELSE IF PROGRAM!* EQ 'EXTRA! BEGIN THEN GO TO A
%	 ELSE IF PROGRAM!* EQ !*!*ESC THEN GO TO A0
	 ELSE GO TO ER1;
    ER: LPRIE IF NULL ATOM CADR PROGRAM!*
		  THEN LIST(CAADR PROGRAM!*,"UNDEFINED")
		 ELSE "SYNTAX ERROR";
    ER1:
	PARSERR := T;
	GO TO ERR3;
    ERR2:
	PROGRAML!* := PROGRAM!*;
    ERR3:
	RESETPARSER();
%	IF NULL ERFG!* OR ERFG!* EQ 'HOLD
%	 THEN LPRIE "ERROR TERMINATION *****";
	ERFG!* := T;
	IF NULL !*INT THEN GO TO E;
	RESULT := PAUSE1 PARSERR;
	IF RESULT THEN RETURN NULL EVAL RESULT;
	ERFG!* := NIL;
	GO TO A;
    E:	!*DEFN := T;	%continue syntax analyzing but not evaluation;
	!*ECHO := T;
	IF NULL CMSG!* THEN LPRIE "CONTINUING WITH PARSING ONLY ...";
	CMSG!* := T;
	GO TO A
   END;

SYMBOLIC PROCEDURE CONDTERPRI;
   !*OUTPUT AND !*ECHO AND !*EXTRAECHO AND (NULL !*INT OR IFL!*)
	AND NULL !*DEFN AND POSN() > 0 AND TERPRI();

CommentOutCode <<
SYMBOLIC PROCEDURE ASSGNL U;
   IF ATOM U OR NULL (CAR U MEMQ '(SETK SETQ SETEL))
     THEN NIL
    ELSE IF ATOM CADR U THEN MKQUOTE CADR U . ASSGNL CADDR U
    ELSE CADR U . ASSGNL CADDR U;
>>;

SYMBOLIC PROCEDURE DFPRINT U;
   %Looks for special action on a form, otherwise prettyprints it;
   IF DFPRINT!* THEN APPLY(DFPRINT!*,LIST U)
%    ELSE IF CMSG!* THEN NIL
    ELSE IF NULL EQCAR(U,'PROGN) THEN
    <<  PRINTF "%f";
	PRETTYPRINT U >>
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
%      TERPRI();
      PRIN2 "TIME: "; PRIN2 X; PRIN2T " MS";
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

CommentOutCode <<
SYMBOLIC PROCEDURE PRIN2X U;
  OUTL!*:=U . OUTL!*;

SYMBOLIC PROCEDURE PTOKEN;
   BEGIN SCALAR X;
	X := TOKEN();
	IF X EQ '!) AND EQCAR(OUTL!*,'! ) THEN OUTL!*:= CDR OUTL!*;
	   %an explicit reference to OUTL!* used here;
	PRIN2X X;
	IF NULL ((X EQ '!() OR (X EQ '!))) THEN PRIN2X '! ;
	RETURN X
   END;
>>;

SYMBOLIC PROCEDURE MKEX U;
   IF NOT(!*MODE EQ 'ALGEBRAIC) OR EQCAR(U,'AEVAL) THEN U
    ELSE NIL;%APROC(U,'AEVAL);

SYMBOLIC PROCEDURE MKSETQ(U,V);
   LIST('SETQ,U,V);

SYMBOLIC PROCEDURE MKVAR(U,V); U;

SYMBOLIC PROCEDURE RPLCDX(U,V); IF CDR U=V THEN U ELSE RPLACD(U,V);

SYMBOLIC PROCEDURE REFORM U;
   IF ATOM U OR CAR U EQ 'QUOTE THEN U
   ELSE IF CAR U EQ 'COND THEN 'COND . REFORM CDR U
   ELSE IF CAR U EQ 'PROG
    THEN PROGN(RPLCDX(CDR U,MAPCAR(CDDR U,FUNCTION REFORM)),U)
    ELSE IF CAR U EQ 'LAMBDA
     THEN PROGN(RPLACA(CDDR U,REFORM CADDR U),U)
    ELSE IF CAR U EQ 'FUNCTION AND ATOM CADR U
     THEN BEGIN SCALAR X;
	IF NULL !*CREF AND (X:= GET(CADR U,'SMACRO))
	  THEN RETURN LIST('FUNCTION,X)
	 ELSE IF GET(CADR U,'NMACRO) OR MACROP CADR U
	  THEN REDERR "MACRO USED AS FUNCTION"
	 ELSE RETURN U END
%    ELSE IF CAR U EQ 'MAT THEN RPLCDX(U,MAPC2(CDR U,FUNCTION REFORM))
    ELSE IF ATOM CAR U
     THEN BEGIN SCALAR X,Y;
	 IF (Y := GETD CAR U) AND CAR Y EQ 'MACRO
		AND EXPANDQ CAR U
	  THEN RETURN REFORM APPLY(CDR Y,LIST U);
	X := REFORMLIS CDR U;
	IF NULL IDP CAR U THEN RETURN(CAR U . X)
	 ELSE IF (NULL !*CREF OR EXPANDQ CAR U)
		 AND (Y:= GET(CAR U,'NMACRO))
	  THEN RETURN
		APPLY(Y,IF FLAGP(CAR U,'NOSPREAD) THEN LIST X ELSE X)
	 ELSE IF (NULL !*CREF OR EXPANDQ CAR U)
		   AND (Y:= GET(CAR U,'SMACRO))
	  THEN RETURN SUBLIS(PAIR(CADR Y,X),CADDR Y)
	   %we could use an atom SUBLIS here (eg, SUBLA);
	 ELSE RETURN PROGN(RPLCDX(U,X),U)
      END
    ELSE REFORM CAR U . REFORMLIS CDR U;

SYMBOLIC PROCEDURE REFORMLIS U;
    IF ATOM U THEN U ELSE REFORM CAR U . REFORMLIS CDR U;

SYMBOLIC PROCEDURE EXPANDQ U;
   %determines if macro U should be expanded in REFORM;
   FLAGP(U,'EXPAND) OR !*FORCE AND NULL FLAGP(U,'NOEXPAND);

CommentOutCode <<
SYMBOLIC PROCEDURE ARRAYP U;
   GET(U,'ARRAY);

SYMBOLIC PROCEDURE GETTYPE U;
   %it might be better to use a table here for more generality;
   IF NULL ATOM U THEN 'FORM
    ELSE IF NUMBERP U THEN 'NUMBER
    ELSE IF ARRAYP U THEN 'ARRAY
    ELSE IF GETD U THEN 'PROCEDURE
    ELSE IF GLOBALP U THEN 'GLOBAL
    ELSE IF FLUIDP U THEN 'FLUID
    ELSE IF GET(U,'MATRIX) THEN 'MATRIX
    ELSE IF GET(U,'SIMPFN) OR GET(U,'MSIMPFN) THEN 'OPERATOR
    ELSE IF FLAGP(U,'PARM) THEN 'PARAMETER
    ELSE NIL;

SYMBOLIC PROCEDURE GETELS U;
   GETEL(CAR U . EVLIS(CDR U));

SYMBOLIC PROCEDURE SETELS(U,V);
   SETEL(CAR U . EVLIS(CDR U),V);
>>;

%. Top Level Entry Function
%. --- Special Flags -----
% !*DEMO -

SYMBOLIC PROCEDURE COMMAND;
   BEGIN SCALAR X,Y;
	IF !*DEMO AND (X := IFL!*)
	  THEN PROGN(TERPRI(),RDS NIL,READCH(),RDS CDR X);
%	IF EDIT!* THEN EDITLINE() ELSE IF FLG!* THEN GO TO A;
	IF !*SLIN THEN
	  <<KEY!* := SEMIC!* := '!;;
	    CLOC!* := IF IFL!* THEN CAR IFL!* . PGLINE() ELSE NIL;
	    X := IF LREADFN!* THEN APPLY(LREADFN!*,NIL) ELSE READ();
	    IF KEY!* EQ '!; THEN KEY!* := IF ATOM X THEN X ELSE CAR X>>
	 ELSE <<SetRlispScanTable(); MakeInputAvailable(); SCAN();
		CLOC!* := IF IFL!* THEN CAR IFL!* . PGLINE() ELSE NIL;
		KEY!* := CURSYM!*; X := XREAD1 NIL>>;
	IF !*PRET THEN PROGN(TERPRI(),RPRINT X);
	X := REFORM X;
	IF CLOC!* AND NOT ATOM X AND CAR X MEMQ '(DE DF DM)
	  THEN PUT(CADR X,'LOCN,CLOC!*)
	ELSE IF CLOC!* AND EQCAR(X,'PROGN)
	      AND CDDR X AND NOT ATOM CADDR X
	      AND CAADDR X MEMQ '(DE DF DM)
	  THEN PUT(CADR CADDR X,'LOCN,CLOC!*);
%	IF IFL!*='(DSK!: (INPUT . TMP)) AND 
%	   (Y:= PGLINE()) NEQ '(1 . 0)
%	  THEN LPL!*:= Y;	%use of IN(noargs);
	IF NULL IDP KEY!* OR NULL(GET(KEY!*,'STAT) EQ 'MODESTAT)
		AND NULL(KEY!* EQ 'ED)
	  THEN X := MKEX X;
    A:	IF FLG!* AND IFL!* THEN BEGIN
		CLOSE CDR IFL!*;
		IPL!* := DELETE(IFL!*,IPL!*);
		IF IPL!* THEN RDS CDAR IPL!* ELSE RDS NIL;
		IFL!* := NIL END;
	FLG!* := NIL;
	RETURN X 
   END;

OFF R2I;

SYMBOLIC PROCEDURE RPRINT U;		% Autoloading stub
<<  LOAD RPRINT;
    RPRINT U >>;

ON R2I;

%*********************************************************************
%			   GENERAL FUNCTIONS
%********************************************************************;


%SYMBOLIC PROCEDURE MAPC2(U,V);
%   %this very conservative definition is to allow for systems with
%   %poor handling of functional arguments, and because of bootstrap-
%   %ping difficulties;
%   BEGIN SCALAR X,Y,Z;
%   A: IF NULL U THEN RETURN REVERSIP Z;
%      X := CAR U;
%      Y := NIL;
%   B: IF NULL X THEN GO TO C;
%      Y := APPLY(V,LIST CAR X) . Y;
%      X := CDR X;
%      GO TO B;
%   C: U := CDR U;
%      Z := REVERSIP Y . Z:
%      GO TO A
%   END;



%*********************************************************************
%	 FUNCTIONS FOR PRINTING DIAGNOSTIC AND ERROR MESSAGES
%********************************************************************;

SYMBOLIC PROCEDURE LPRIE U;
<<  ERRORPRINTF("***** %L", U);
    ERFG!* := T >>;

SYMBOLIC PROCEDURE LPRIM U; 
    !*MSG AND ERRORPRINTF("*** %L", U);

SYMBOLIC PROCEDURE REDERR U;
   BEGIN %TERPRI(); 
     LPRIE U; ERROR(99,NIL) END;


SYMBOLIC PROCEDURE PROGVR VAR;
   IF NOT ATOM VAR THEN NIL
    ELSE IF NUMBERP VAR OR FLAGP(VAR,'SHARE)
	OR NOT(!*MODE EQ 'ALGEBRAIC) AND FLUIDP VAR THEN T
    ELSE BEGIN SCALAR X;
	IF X := GET(VAR,'DATATYPE) THEN RETURN CAR X END;

SYMBOLIC PROCEDURE MKARG U;
   IF NULL U THEN NIL
    ELSE IF ATOM U THEN IF PROGVR U THEN U ELSE MKQUOTE U
    ELSE IF CAR U EQ 'QUOTE THEN MKQUOTE U
    ELSE IF FLAGP!*!*(CAR U,'NOCHANGE) AND NOT FLAGP(KEY1!*,'QUOTE)
     THEN U
    ELSE 'LIST . MAPCAR(U,FUNCTION MKARG);


SYMBOLIC PROCEDURE MKPROG(U,V);
   'PROG . (U . V);

CommentOutCode <<
SYMBOLIC PROCEDURE SETDIFF(U,V);
   IF NULL V THEN U ELSE SETDIFF(DELETE(CAR V,U),CDR V);

SYMBOLIC PROCEDURE REMTYPE VARLIS;
   BEGIN SCALAR X,Y;
	VARS!* := SETDIFF(VARS!*,VARLIS);
    A:	IF NULL VARLIS THEN RETURN NIL;
	X := CAR VARLIS;
	Y := CDR GET(X,'DATATYPE);
	IF Y THEN PUT(X,'DATATYPE,Y)
	 ELSE PROGN(REMPROP(X,'DATATYPE),REMFLAG(LIST X,'PARM));
	VARLIS := CDR VARLIS;
	GO TO A
   END;
>>;

DEFLIST('((LISP SYMBOLIC)),'NEWNAM);

FLAG('(FOR),'NOCHANGE);

FLAG('(REPEAT),'NOCHANGE);

FLAG('(WHILE),'NOCHANGE);

CommentOutCode <<
COMMENT LISP arrays built with computed index into a vector;
% FLUID '(U V X Y N); %/ Fix for MAPC closed compile

SYMBOLIC PROCEDURE ARRAY U;
   FOR EACH X IN U DO
      BEGIN INTEGER Y;
	IF NULL CDR X OR NOT IDP CAR X
	  THEN REDERR LIST(X,"CANNOT BECOME AN ARRAY");
	Y:=1;
	FOR EACH V IN CDR X DO Y:=Y*(V+1);
	PUT(CAR X,'ARRAY,MKVECT(Y-1));
	PUT(CAR X,'DIMENSION,ADD1LIS CDR X);
   END;

SYMBOLIC PROCEDURE CINDX!* U;
   BEGIN SCALAR V; INTEGER N;
	N:=0;
	IF NULL(V:=DIMENSION CAR U)
	  THEN REDERR LIST(CAR U,"NOT AN ARRAY");
	FOR EACH Y IN CDR U DO
	 <<IF NULL V THEN REDERR LIST(U,"TOO MANY INDICES");
	   IF Y<0 OR Y>CAR V-1
	     THEN REDERR LIST(U,"INDEX OUT OF RANGE");
	   N:=Y+N*CAR V;
	   V:=CDR V>>;
	IF V THEN REDERR LIST(U,"TOO FEW INDICES");
	RETURN N
   END;
%UNFLUID '(U V X Y N); %/ Fix for MAPC closed compile

SYMBOLIC PROCEDURE GETEL U;
 GETV(ARRAYP CAR U,CINDX!* U);

SYMBOLIC PROCEDURE SETEL(U,V);
 PUTV(ARRAYP CAR U,CINDX!* U,V);

SYMBOLIC PROCEDURE DIMENSION U;
 GET(U,'DIMENSION);


COMMENT further support for REDUCE arrays;

SYMBOLIC PROCEDURE TYPECHK(U,V);
   BEGIN SCALAR X;
      IF (X := GETTYPE U) EQ V OR X EQ 'PARAMETER
	THEN LPRIM LIST(U,"ALREADY DEFINED AS",V)
       ELSE IF X THEN REDERR LIST(X,U,"INVALID AS",V)
   END;

SYMBOLIC PROCEDURE NUMLIS U;
   NULL U OR (NUMBERP CAR U AND NUMLIS CDR U);

CompileTime REMPROP('ARRAY,'STAT);	 %for bootstrapping purposes;

SYMBOLIC PROCEDURE ARRAYFN U;
   BEGIN SCALAR X,Y;
    A:	IF NULL U THEN RETURN;
	X := CAR U;
	IF ATOM X THEN REDERR "SYNTAX ERROR"
	 ELSE IF TYPECHK(CAR X,'ARRAY) THEN GO TO B;
	Y := IF NOT(!*MODE EQ 'ALGEBRAIC) THEN !*EVLIS CDR X
		ELSE REVLIS CDR X;
	IF NOT NUMLIS Y
	  THEN LPRIE LIST("INCORRECT ARRAY ARGUMENTS FOR",CAR X);
	ARRAY LIST (CAR X . Y);
    B:	U := CDR U;
	GO TO A
   END;

SYMBOLIC PROCEDURE ADD1LIS U;
   IF NULL U THEN NIL ELSE (CAR U+1) . ADD1LIS CDR U;

>>;
%*********************************************************************
%*********************************************************************
%	REDUCE FUNCTIONS FOR HANDLING INPUT AND OUTPUT OF FILES
%*********************************************************************
%********************************************************************;

GLOBAL '(CONTL!*);

MACRO PROCEDURE IN U;
    LIST('EVIN, MKQUOTE CDR U);

SYMBOLIC PROCEDURE EVIN U;
   BEGIN SCALAR CHAN,ECHO,ECHOP,EXTN,OSLIN,OLRDFN,OTSLIN;
    ECHOP := SEMIC!* EQ '!;;
    ECHO := !*ECHO;
    IF NULL IFL!* THEN TECHO!* := !*ECHO;	%terminal echo status;
    OSLIN := !*SLIN;
    OLRDFN := LREADFN!*;
    OTSLIN := TSLIN!*;
    TSLIN!* := NIL;
    FOR EACH FL IN U DO
      <<CHAN := OPEN(FL,'INPUT); IFL!* := FL . CHAN;
	IPL!* := IFL!* . IPL!*;
	RDS (IF IFL!* THEN CDR IFL!* ELSE NIL);
	!*ECHO := ECHOP;
	!*SLIN := T;
	 IF LISPFILENAMEP FL THEN LREADFN!* := NIL
	 ELSE !*SLIN := OSLIN;
	BEGIN1();
	IF !*SLIN THEN RESETPARSER();
	IF CHAN THEN CLOSE CHAN;
	LREADFN!* := OLRDFN;
	!*SLIN := OSLIN;
	IF FL EQ CAAR IPL!* THEN IPL!* := CDR IPL!*
	 ELSE REDERR LIST("FILE STACK CONFUSION",FL,IPL!*)>>;
    !*ECHO := ECHO;   %restore echo status;
    TSLIN!* := OTSLIN;
    IF IPL!* AND NULL CONTL!* THEN IFL!* := CAR IPL!*
     ELSE IFL!* := NIL;
    RDS(IF IFL!* THEN CDR IFL!* ELSE NIL);
    RETURN NIL
   END;

CommentOutCode <<
lisp procedure RedIN F;
begin scalar !*Echo, !*Output, !*SLIN, Chan;
   IPL!* := (IFL!* := (F . (Chan := Open(F, 'Input)))) . IPL!*;
   RDS Chan;
   Begin1();
   IPL!* := cdr IPL!*;
   RDS(if not null IPL!* then cdr first IPL!* else NIL);
end;
>>;

SYMBOLIC PROCEDURE LISPFILENAMEP S;	%. Look for ".SL" or ".LSP"
BEGIN SCALAR C, I, SS;
    SS := SIZE S;
    IF SS < 3 THEN RETURN NIL;
    I := SS;
LOOP:
    IF I < 0 THEN RETURN NIL;
    IF INDX(S, I) = CHAR '!. THEN GOTO LOOPEND;
    I := I - 1;
    GOTO LOOP;
LOOPEND:
    I := I + 1;
    C := SS - I;
    IF NOT (C MEMBER '(1 2)) THEN RETURN NIL;
    C := SUBSEQ(S, I, SS + 1);
    RETURN IF C MEMBER '("SL" "sl" "LSP" "lsp" "Sl" "Lsp") THEN T ELSE NIL;
END;

MACRO PROCEDURE OUT U;
    LIST('EVOUT, MKQUOTE CDR U);

SYMBOLIC PROCEDURE EVOUT U;
   %U is a list of one file;
   BEGIN SCALAR CHAN,FL,X;
	IF NULL U THEN RETURN NIL
	 ELSE IF CAR U EQ 'T THEN RETURN <<WRS(OFL!* := NIL); NIL>>;
	FL := MKFIL CAR U;
	IF NOT (X := ASSOC(FL,OPL!*))
	  THEN <<CHAN := OPEN(FL,'OUTPUT);
		 OFL!* := FL . CHAN;
		 OPL!* := OFL!* . OPL!*>>
	 ELSE OFL!* := X;
	WRS CDR OFL!*
   END;

MACRO PROCEDURE SHUT U;
    LIST('EVSHUT, MKQUOTE CDR U);

SYMBOLIC PROCEDURE EVSHUT U;
   %U is a list of names of files to be shut;
   BEGIN SCALAR FL,FL1;
    A:	IF NULL U THEN RETURN NIL
	 ELSE IF FL1 := ASSOC((FL := MKFIL CAR U),OPL!*) THEN GO TO B
	 ELSE IF NOT (FL1 := ASSOC(FL,IPL!*))
	  THEN REDERR LIST(FL,"NOT OPEN");
	IF FL1 NEQ IFL!*
	  THEN <<CLOSE CDR FL1; IPL!* := DELETE(FL1,IPL!*)>>
	  ELSE REDERR LIST("CANNOT CLOSE CURRENT INPUT FILE",CAR FL);
	GO TO C;
    B:	OPL!* := DELETE(FL1,OPL!*);
	IF FL1=OFL!* THEN <<OFL!* := NIL; WRS NIL>>;
	CLOSE CDR FL1;
    C:	U := CDR U;
	GO TO A
   END;

%/ removed STAT property

%*********************************************************************
%		FUNCTIONS HANDLING INTERACTIVE FEATURES
%********************************************************************;

%GLOBAL Variables referenced in this Section;

CONTL!* := NIL;

SYMBOLIC PROCEDURE PAUSE;
   PAUSE1 NIL;

SYMBOLIC PROCEDURE PAUSE1 BOOL;
   BEGIN
%      IF BOOL THEN
%	IF NULL IFL!*
%	 THEN RETURN IF !*INT AND GETD 'CEDIT AND YESP 'EDIT!?
%		       THEN CEDIT() ELSE
%		       NIL
%	 ELSE IF GETD 'EDIT1 AND ERFG!* AND CLOC!* AND YESP 'EDIT!?
%	  THEN RETURN <<CONTL!* := NIL;
%	   IF OFL!* THEN <<LPRIM LIST(CAR OFL!*,'SHUT);
%			   CLOSE CDR OFL!*;
%			   OPL!* := DELETE(OFL!*,OPL!*);
%			   OFL!* := NIL>>;
%	   EDIT1(CLOC!*,NIL)>>
%	 ELSE IF FLG!* THEN RETURN (EDIT!* := NIL);
      IF NULL IFL!* OR YESP 'CONT!? THEN RETURN NIL;
      CONTL!* := IFL!* . !*ECHO . CONTL!*;
      RDS (IFL!* := NIL);
      !*ECHO := TECHO!*
   END;

SYMBOLIC PROCEDURE CONT;
   BEGIN SCALAR FL,TECHO;
	IF IFL!* THEN RETURN NIL   %CONT only active from terminal;
	 ELSE IF NULL CONTL!* THEN REDERR "NO FILE OPEN";
	FL := CAR CONTL!*;
	TECHO := CADR CONTL!*;
	CONTL!* := CDDR CONTL!*;
	IF FL=CAR IPL!* THEN <<IFL!* := FL;
			       RDS IF FL THEN CDR FL ELSE NIL;
			       !*ECHO := TECHO>>
	 ELSE <<EOF!* :=T; LPRIM LIST(FL,"NOT OPEN"); ERROR(99,NIL)>>
   END;

%/DEFLIST ('((PAUSE ENDSTAT) (CONT ENDSTAT) (RETRY ENDSTAT)),'STAT);

%/PUT('RETRY,'STAT,'ENDSTAT);

FLAG ('(CONT),'IGNORE);


%******** "rend" fixups

GLOBAL '(!*INT CONTL!* DATE!* !*MODE
	 IMODE!* CRCHAR!* !*SLIN LREADFN!*);

REMFLAG('(BEGINRLISP),'GO);

%---- Merge into XREAD1 in command ----
% Shouldnt USE Scan in COMMAND, since need change Parser first

FLUID '(!*PECHO);

Symbolic Procedure XREAD1 x;           %. With Catches
 Begin scalar Form!*;
     Form!*:=PARSE0(0, NIL);
     If !*PECHO then PRIN2T LIST("parse>",Form!*);
     Return Form!*   
 end;

lisp procedure Xread X;
 Begin scalar Form!*;
     MakeInputAvailable();
     Form!*:=PARSE0(0, T);
     If !*PECHO then PRIN2T LIST("parse>",Form!*);
     Return Form!*   
 end;

!*PECHO:=NIL;

SYMBOLIC PROCEDURE BEGINRLISP;
   BEGIN SCALAR A,B,PROMPTSTRING!*;
%/	!*BAKGAG := NIL;
	!*INT := T;
	!*ECHO := NIL;
	A := !*SLIN;
	!*SLIN := LREADFN!* := NIL;
	CONTL!* := IFL!* := IPL!* := OFL!* := OPL!* := NIL;
	!*MODE := IMODE!*;
	CRCHAR!* := '! ;
%/	RDSLSH NIL;
%/	SETPCHAR '!*;
	SetRlispScanTable();
%	IF SYSTEM!* NEQ 0 THEN CHKLEN();
	IF DATE!* EQ NIL
	  THEN IF A THEN <<PRIN2 "Entering RLISP..."; GO TO B>>
		ELSE GO TO A;
%/	IF FILEP '((REDUCE . INI)) THEN <<IN REDUCE.INI; TERPRI()>>;
%/	ERRORSET(QUOTE LAPIN "PSL.INI", NIL, NIL);	% no error if not there
	PRIN2 DATE!*;
	DATE!* := NIL;
%	IF SYSTEM!* NEQ 1 THEN GO TO A;
%	IF !*HELP THEN PRIN2 "For help, type HELP()";
  B:    TERPRI();
  A:    BEGIN1();
%	TERPRI();
	!*SLIN := T;
%/        RDSLSH NIL;
        SetLispScanTable();
	PRIN2T "Entering LISP..."
   END;

FLAG('(BEGINRLISP),'GO);

PUTD('BEGIN,'EXPR, CDR GETD 'BEGINRLISP);

SYMBOLIC PROCEDURE MKFIL U;
   %converts file descriptor U into valid system filename;
   U;

SYMBOLIC PROCEDURE NEWMKFIL U;
   %converts file descriptor U into valid system filename;
   U;

lisp procedure SetPChar C;		%. Set prompt, return old one
begin scalar OldPrompt;
    OldPrompt := PromptString!*;
    PromptString!* := if StringP C then C
		      else if IDP C then CopyString ID2String C
		      else BldMsg("%w", C);
    return OldPrompt;
end;

COMMENT Some Global Variables required by REDUCE;

%GLOBAL '(!*!*ESC);
%
%!*!*ESC := 'ESC!.NOT!.NEEDED!.NOW;   %to make it user settable (used to be a NEWNAM);


COMMENT The remaining material in this file introduces extensions
	or redefinitions of code in the REDUCE source files, and
	is not really necessary to run a basic system;


lisp procedure SetRlispScanTable();
<<  CurrentReadMacroIndicator!* :='RLispReadMacro;
    CurrentScanTable!* := RLispScanTable!* >>;

lisp procedure SetLispScanTable();
<<  CurrentReadMacroIndicator!* :='LispReadMacro;
    CurrentScanTable!* := LispScanTable!* >>;

PutD('LispSaveSystem, 'EXPR, cdr GetD 'SaveSystem);

lisp procedure SaveSystem(S, F, I);		%. Set up for saving EXE file
<<  StatCounter!* := 0;
    RemD 'Main;
    Copyd('Main, 'RlispMain);
    Date!* := BldMsg("%w, %w", S, Date());
    LispSaveSystem("PSL", F, I) >>;

lisp procedure RlispMain();
<<  BeginRlisp();
    StandardLisp() >>;

lisp procedure Rlisp();			% Uses new top loop
<<  SetRlispScanTable();
    TopLoop('ReformXRead, 'PrintWithFreshLine, 'Eval, "rlisp", "PSL Rlisp") >>;

lisp procedure ReformXRead();
    Reform XRead T;

!*RAISE := T;

%IF GETD 'ADDSQ THEN IMODE!* := 'ALGEBRAIC ELSE IMODE!* := 'SYMBOLIC;
IMODE!* := 'SYMBOLIC;

TSLIN!* := NIL;
!*MSG := T;

END;
