COMMENT The following is needed to get string case correct;

FLAG('(OFF),'EVAL);

OFF RAISE;

COMMENT The following functions, which are referenced in the basic
REDUCE source (RLISP, ALG1, ALG2, MATR and PHYS) should be defined to
complete the definition of REDUCE:

	BYE
        DELCP
	ERROR1
	FILETYPE
        MKFIL
	ORDERP
	QUIT
	SEPRP
	SETPCHAR.

Prototypical descriptions of these functions are as follows;

SYMBOLIC PROCEDURE BYE;
   %Returns control to the computer's operating system command level.
   %The current REDUCE job cannot be restarted;
   EVAL '(QUIT);

SYMBOLIC PROCEDURE DELCP U;
   %Returns true if U is a semicolon, dollar sign, or other delimiter.
   %This definition replaces the one in the BOOT file;
   U EQ '!; OR U EQ '!$ OR U EQ INTERN ASCII 125;

SYMBOLIC PROCEDURE ERROR1;
   %This is the only call to an error function in the REDUCE source.  It
   %should cause an error return, but NOT print anything, as preceding
   %statements have already done that.  In terms of the LISP error
   %function it can be defined as follows;
   ERROR(99,NIL);

SYMBOLIC PROCEDURE FILETYPE U;
   %determines the extension of a file U;
   IF ATOM U THEN NIL
    ELSE IF NOT ATOM CAR U AND NULL CDR U THEN FILETYPE CAR U
    ELSE IF DEVP CAR U
     THEN IF CAR U EQ 'DIR!: THEN FILETYPE CADDR U ELSE FILETYPE CADR U
    ELSE IF NOT IDP CDR U THEN NIL ELSE CDR U;

SYMBOLIC PROCEDURE DEVP U;
   %determines if U is a file device type.
   NOT ATOM U OR IDP U AND CAR REVERSIP EXPLODE U EQ '!:;

%SYMBOLIC PROCEDURE MKFIL U;
   %converts file descriptor U into valid system filename;
   %U;   %this is the simplest one can do;

%SYMBOLIC PROCEDURE ORDERP(U,V);
   %Returns true if U has same or higher order than id V by some
   %consistent convention (eg unique position in memory);
   %It must usually be defined in LAP, as in following DEC 10 version;
   %It must also be loaded BEFORE ALG2.RED;
   LAP '((ORDERP EXPR 2)
    	(104960 1 2)
    	(112640 1 (C 0))
    	(MOVEI 1 (QUOTE T))
    	(POPJ P));

%SYMBOLIC PROCEDURE QUIT;
   %Returns control to the computer's operating system command level.
   %The current REDUCE job can however be restarted;

GLOBAL '(!$EOL!$);

SYMBOLIC PROCEDURE SEPRP U;
   %returns true if U is a blank or other separator (eg, tab or ff).
   %This definition replaces one in the BOOT file;
   U EQ '!  OR U EQ '!	 OR U EQ !$EOL!$ OR U EQ INTERN ASCII 12;

%SYMBOLIC PROCEDURE SETPCHAR U;
   %This function sets the terminal prompt character to U and returns
   %the previous value;
   %U;


COMMENT The following functions are only referenced if various flags are
set, or the functions are actually defined. They are defined in another
module, which is not needed to build the basic system. The name of the
flag follows the function name, enclosed in parentheses:

        BFQUOTIENT!: (BIGFLOAT)
	CEDIT (?)
	COMPD (COMP)
	EDIT1	This function provides a link to an editor. However, a
		definition is not necessary, since REDUCE checks to see
		if it has a function value.
	EMBFN (?)
	EZGCDF (EZGCD)
	FACTORF (FACTOR)
	LOAD!-MODULE (property list attribute MODULE-NAME)
		This function is used to load an external module into
		the system. It is only called if an attribute DOMAIN-MODE
		is given to a domain mode tag
	PRETTYPRINT (DEFN --- also called by DFPRINT)
		This function is used in particular for output of RLISP
		expressions in LISP syntax. If that feature is needed,
		and the prettyprint module is not available, then it
		should be defined as PRINT
        RPRINT (PRET)
	TEXPT!: (BIGFLOAT)
        TEXPT!:ANY (BIGFLOAT)
	TIME (TIME) returns elapsed time from some arbitrary initial
		    point in milliseconds;

COMMENT The FACTOR module also requires a definition for GCTIME, the 
time taken for garbage collection. If this is not defined in the given
system, the following definition may be used;

SYMBOLIC PROCEDURE GCTIME; 0;


COMMENT The following definition overrides the standard source version;

REMFLAG('(PRINTPROMPT),'LOSE);

SYMBOLIC PROCEDURE PRINTPROMPT U; NIL;

FLAG('(PRINTPROMPT),'LOSE);

COMMENT There is also one global variable in the system which must be
set independent of the sources, namely **ESC. This variable is used to
"escape" from an input sequence to the top level of REDUCE.
For complete flexibility, it should be defined as a global. Otherwise,
a NEWNAM statement can be used. However, it MUST be defined in LISP
before RLISP is loaded, and cannot be left until this file is defined.
At the moment, this feature is not supported, as it interferes with the
editing facilities;

GLOBAL '(!*!*ESC);

!*!*ESC := '!*ESC!*;

COMMENT In addition, the global variable ESC* is used by the interactive
string editor (defined in CEDIT) as a terminator for input strings. On
ASCII terminals, <escape> is a good candidate;

GLOBAL '(ESC!*);

ESC!* := INTERN ASCII 125;   %escape character;


COMMENT We also need to define a function BEGIN, which acts as the
top-level call to REDUCE, and sets the appropriate variables. The
following is a minimum definition;

REMFLAG('(BEGIN),'GO);

FLUID '(LREADFN!* !*ECHO !*MODE !*SLIN);

GLOBAL '(CRCHAR!* DATE!* ORIG!* !*EXTRAECHO !*HELP !*INT);

GLOBAL '(CONTL!* IFL!* IPL!* OFL!* OPL!*);

COMMENT The following two variables are DEC 10 specific;

GLOBAL '(SYSTEM!* !*BAKGAG);

SYMBOLIC PROCEDURE BEGIN;
   BEGIN SCALAR A1;
      ORIG!* := 0;
      !*ECHO := NOT !*INT;
%     !*EXTRAECHO := T;   %this is needed in systems which do not
			  %have the "standard" eol convention;
      CONTL!* := IFL!* := IPL!* := OFL!* := OPL!* := NIL;
      A1 := !*SLIN; !*SLIN := NIL;   %shows we have entered this BEGIN;
      %The next eight lines are DEC 10 specific;
      !*BAKGAG := NIL;    %turn off backtrace;
      LREADFN!* := NIL;   %define a special reading function;
      RDSLSH NIL;         %modify reader for Rlisp token handling;
      SCANSET T;	  %use table driven scanner;
%     IF SYSTEM!* NEQ 0 THEN CHKLEN();
%     IF SYSTEM!*=1 THEN BEGIN SCALAR A2;
%	SETSYS
%	   IF PAIRP(A2:=ERRORSET('(JSYS 32 0 "<REDUCE>" 0 1),NIL,NIL))
%	     THEN BOOLE(1,CAR A2,262143) ELSE 0 END;
      %end of DEC 10 specific code;
      IF NULL DATE!*
	THEN <<IF A1 THEN PRIN2T "Reduce Parsing ..."; GO TO A>>;
      IF FILEP '((REDUCE . INI)) THEN <<IN "REDUCE.INI"; TERPRI()>>;
	   %allows for the automatic load of an initialization file;
      LINELENGTH IF !*INT THEN 72 ELSE 115;
      PRIN2 "REDUCE 3.0, ";
      PRIN2 DATE!*;
      PRIN2T " ...";
      !*MODE := IF GETD 'ADDSQ THEN 'ALGEBRAIC ELSE 'SYMBOLIC;
      DATE!* := NIL;
      IF !*HELP THEN PRIN2 "For help, type HELP<escape>";
      TERPRI();
   A: CRCHAR!* := '! ;    %necessary initialization of CRCHAR!*;
      BEGIN1();
      !*SLIN := T;
      RESETPARSER();   %in case *SLIN affects this;
      PRIN2T "Entering LISP ...";
      SETPCHAR '!*
   END;

FLAG('(BEGIN),'GO);


COMMENT And now to set some system dependent variables;

DATE!* := "15-Apr-83";

%!*INT := T;		%sets the appropriate interactive mode.
			%Needs to be suppressed during bootstrapping
			%to avoid CRBUF!* being used;

COMMENT on the DEC 10, the end-of-file condition is not handled 
in quite the way described in the Standard LISP Report. The following
statement is necessary to solve this problem;

%!$EOF!$ := '!$EOF!$;


COMMENT And finally ...;

%REMD 'BEGIN2;  %used in full bootstrap and needed later;


COMMENT Definitions needed to support Norman-Moore factorizer on
	the PDP-10;

FLUID '(LARGEST!-SMALL!-MODULUS);

LARGEST!-SMALL!-MODULUS := 2**32;

SYMBOLIC PROCEDURE LOGAND2(M,N); BOOLE(1,M,N);

SYMBOLIC PROCEDURE LOGOR2(M,N); BOOLE(7,M,N);

SYMBOLIC PROCEDURE LOGXOR2(M,N); BOOLE(6,M,N);

REMFLAG('(IRIGHTSHIFT), 'LOSE);

SYMBOLIC SMACRO PROCEDURE IRIGHTSHIFT(U,N); LSH(U,-N);

FLAG('(IRIGHTSHIFT), 'LOSE);

SYMBOLIC SMACRO PROCEDURE LEFTSHIFT(U,N); LSH(U,N);


COMMENT Definition of MKFIL to handle string file names properly;

SYMBOLIC PROCEDURE MKFIL U;
   %U is an ID or string. Result is a permissible LISP 1.6 filename.
   BEGIN SCALAR FILE,V,Y,Y1,Z;
      IF NULL U THEN FILERR U
       ELSE IF NOT STRINGP U
	THEN RETURN IF IDP U THEN U ELSE FILERR U;
      V := EXPLODEC U;
   A: Z := NEXTELM V; V := CDR Z; Z := CAR Z;
      IF NULL V THEN NIL
       ELSE IF CAR V EQ '!:
	THEN <<FILE := MKFRAG('!: . '!! . Z) . FILE; V := CDR V>>
       ELSE IF CAR V EQ '!.
	THEN IF NULL Z THEN FILERR U
	  ELSE <<Y := NEXTELM CDR V; V := CDR Y;
		 FILE := (MKFRAG Z . MKFRAG CAR Y) . FILE;
		 Z := NIL>>
       ELSE IF CAR V EQ '!< 
	 THEN <<Y := NEXTELM CDR V; V := CDR Y;
		IF NOT EQCAR(V,'!>) THEN FILERR U;
		FILE := MKFRAG CAR Y . 'DIR!: . FILE;
		V := CDR V>>
       ELSE IF CAR V EQ '!> THEN FILERR U
       ELSE IF CAR V EQ '![
	THEN <<Y := NEXTELM CDR V; V := CDR Y;
	       IF NOT EQCAR(V,'!,) THEN FILERR U;
	       Y1 := MKFRAG CAR Y; Y := NEXTELM CDR V;
	       V := CDR Y; IF NOT EQCAR(V,'!]) THEN FILERR U;
	       FILE := LIST(Y1,MKFRAG CAR Y) . FILE;
	       V := CDR V>>
       ELSE IF CAR V EQ '!, OR CAR V EQ '!] THEN FILERR U;
      IF V THEN GO TO A
       ELSE IF Z
	THEN FILE := MKFRAG Z . IF NULL FILE THEN '(DSK!:) ELSE FILE;
      RETURN REVERSE FILE
   END;

GLOBAL '(LITERS!*);

SYMBOLIC PROCEDURE NEXTELM U;
   BEGIN SCALAR X,Y;
      WHILE U AND NOT(CAR U MEMQ '(!. !: !< !> ![ !, !]))
	DO <<IF LITER CAR U THEN IF Y := ATSOC(CAR U,LITERS!*)
			THEN X := CDR Y . X ELSE X := CAR U . X
	      ELSE IF DIGIT CAR U THEN X := CAR U . X
	      ELSE X := CAR U . '!! . X;
	     U := CDR U>>;
      RETURN X . U
   END;

LITERS!* := '((!a . A) (!b . B) (!c . C) (!d . D) (!e . E) (!f . F)
	      (!g . G) (!h . H) (!i . I) (!j . J) (!k . K) (!l . L)
	      (!m . M) (!n . N) (!o . O) (!p . P) (!q . Q) (!r . R)
	      (!s . S) (!t . T) (!u . U) (!v . V) (!w . W) (!x . X)
	      (!y . Y) (!z . Z));

SYMBOLIC PROCEDURE FILERR U; TYPERR(U,"file name");

SYMBOLIC PROCEDURE MKFRAG U;
   (LAMBDA X; IF NUMBERP X THEN X ELSE INTERN X) COMPRESS REVERSIP U;


END;
