% DEBUG.RED - General tracing capabilities
%             Norman and Morisson
%---------
% Revision History:
%  <PSL.UTIL>DEBUG.RED.21,  4-Feb-83 13:01:05, Edit by OTHMER
%  Added Br - UnBr from Mini-Trace.Red
%  Added functions UnBrAll, UnTrAll
%  Added globals TracedFns!*, BrokenFns!*
%  Changed Restr to be a macro that can take a list of file names
%  as argument
%  Removed many lines of code that had been commented out
%  <PSL.UTIL>DEBUG.RED.20,  3-Feb-83 11:00:06, Edit by KESSLER
%  Remove fluid defintion of !*mode
%  Edit by Griss, 25 January 1983, fix !*MODE and DEFINEROP
%  for REDUCE
%  <PSL.NEW>DEBUG.RED.2, 29-Dec-82 15:28:13, Edit by PERDUE
%  In the fix of 12-december, changed > to !-greaterp
%  Also added a << >> pair to !-findentries
%  <PSL.UTIL>DEBUG.RED.16, 28-Dec-82 13:50:19, Edit by PERDUE
%  Added !-TRSTCOND to handle COND correctly
%  <PSL.UTIL>DEBUG.RED,  12-Dec-82 15:59:45, Edit by GRISS
%    Fixed printx to handle 0 SIZE (i.e. one-element) vectors

CompileTime flag('(!-LPRIE !-LPRIM
		   !-PAD !-IDLISTP !-CIRLIST !-FIRSTN !-LISTOFATOMS !-!-PUTD
		   !-LABELNAME !-FINDENTRIES !-PRINTPASS !-PRINS
		   !-TRGET !-TRGETX !-TRFLAGP !-TRPUT !-TRPUTX !-TRPUTX1
		   !-TRFLAG !-TRFLAG1 !-TRREMPROP !-TRREMPROPX
		   !-TRREMFLAG !-TRREMFLAG1
		   !-TRINSTALL !-ARGNAMES
		   !-TRRESTORE !-OUTRACE1 !-DUMPTRACEBUFF
		   !-ERRAPPLY
		   !-ENTERPRI !-EXITPRI !-TRINDENT !-TRACEPRI1
		   !-TRACENTRYPRI1 !-TRACEXPANDPRI
		   !-MKTRST !-MKTRST1
		   !-BTRPUSH !-BTRPOP !-BTRDUMP
		   !-EMBSUBST
		   !-TR1 !-MKSTUB
		   !-PLIST1 !-PPF1 !-GETC),
		 'InternalFunction);

%********************* Implementation dependent procedures ***********

fluid '(IgnoredInBacktrace!*);

IgnoredInBacktrace!* := Append('(!-TRACEDCALL !-APPLY !-GET),
			       IgnoredInBacktrace!*);

%ON NOUUO; % Slow links 

PUTD('!-!%PROP,'EXPR,CDR GETD 'PROP);

SYMBOLIC PROCEDURE !-GETPROPERTYLIST U;
% U is an  id.  Returns  a list  of all  the flags  (id's) and  property-values
% (dotted pairs) of U.
 !-!%PROP U;

%DEFINE !-GETPROPERTYLIST=!-!%CDR;
%
%PUTD('!-ATOM,'EXPR,CDR GETD 'ATOM);
%
% SYMBOLIC PROCEDURE !-ATOM U;
% A safe version of ATOM.
% !-!%PATOM U;
%
%DEFINE !-ATOM=!-!%PATOM;
%
%GLOBAL '(!*NOUUO);
%
CompileTime <<
SYMBOLIC SMACRO PROCEDURE !-SLOWLINKS;
% Suppresses creation of fast-links
% No-op in PSL
 NIL;
>>;
%******************************************************************

% Needs REDIO for sorting routine.  If compiled without it only
% the printing under the influence of COUNT will be affected.

% I systematically use names starting with a '-' within this
% package for internal routines that must not interfere with the
% user. This means that the debug package may behave incorrectly
% if user functions or variables have names starting with a '-';

%******************** Globals declarations ************************

GLOBAL '(
% Boolean valued flags
  !*BTR			 % T -> stack traced function calls for backtrace
  !*BTRSAVE		 % T -> bactrace things which fail in errorsets
  !*INSTALL		 % T -> "install" trace info on all PUTD'd functions
  !*SAVENAMES		 % controlls saving of substructure names in PRINTX
  !*TRACE		 % T -> print trace information at run time
  !*TRACEALL		 % T -> trace all functions defined with PUTD
  !*TRSTEXPANDMACROS	 % T -> expand macros before embedding SETQs to print
  !*TRUNKNOWN		 % T -> never ask for the number of args
  !*TRCOUNT		 % T -> count # of invocations of traced functions
% Other globals intended to be accessed outside of DEBUG
  !*MSG			 % 
  BROKENFNS!*            % List of functions that have been broken
  TRACEDFNS!*            % List of functions that have been traced
  EMSG!*		 %
  ERFG!*		 % Reduce flag
  MSGCHNL!*		 % Channel to output trace information
  PPFPRINTER!*		 % Used by PPF to print function bodies 
  PROPERTYPRINTER!*	 % Used by PLIST to print property values
  PUTDHOOK!*		 % User hook run after a successful PUTD
  STUBPRINTER!*		 % For printing arguments in calls on stubs
  STUBREADER!*		 % For reading the return value in calls on stubs
  TRACEMINLEVEL!*	 % Minimum recursive depth at which to trace
  TRACEMAXLEVEL!*	 % Maximum     "       "   "	"   "	 "
  TRACENTRYHOOK!*	 % User hook into traced functions
  TRACEXITHOOK!*	 %  "	 "    "     "	     "
  TRACEXPANDHOOK!*	 %  "	 "    "     "	     "
  TREXPRINTER!*		 % Function used to print args/values in traced fns
  TRINSTALLHOOK!*	 % User hook called when a function is first traced
  TRPRINTER!*		 % Function used to print macro expansions
% Globals principally for internal use
  !-ARBARGNAMES!*	 % List of ids to be used for unspecified names
  !-ARGINDENT!*		 % Number of spaces to indent when printing args
  !-BTRSAVEDINTERVALS!*	 % Saved BTR frames from within errorsets
  !-BTRSTK!*		 % Stack for bactrace info
%  !-COLONERRNUM!*	 % Error number used by failing :CAR,:CDR, etc.
  !-FUNCTIONFLAGS!*	 % Flags which PPF considers printing
  !-GLOBALNAMES!*	 % Used by PRINTX to store common substructure names
  !-INDENTCUTOFF!*	 % Furthest right to indent trace output
  !-INDENTDEPTH!*	 % Number of spaces to indent each level trace output
  !-INVISIBLEPROPS!*	 % Properties which PLIST should ignore
  !-INVISIBLEFLAGS!*	 % Flags which PLIST should ignore
  !-INSTALLEDFNS!*	 % Functions which have had information installed
  !-NONSTANDARDFNS!*	 % Properties under which special MACRO's are stored
%  !-SAFEFNSINSTALLED!*	 % T -> :CAR, etc have replaced CAR, etc
  !-TRACEBUFF!*		 % Ringbuffer to save recent trace output
  !-TRACECOUNT!*	 % Decremented -- if >0 it may suppresses tracing
  !-TRACEFLAG!*		 % Enables tracing
	);

FLUID '(
  !*COMP		 % Standard Lisp flag
  !*BACKTRACE		 % Reduce flag
  !*DEFN		 % Reduce flag
  !-ENTRYPOINTS!*	 % for PRINTX
  !-ORIGINALFN!*	 % fluid argument in EMBed function calls
  !-PRINTXCOUNT!*	 % Used by PRINTX for making up names for EQ structures
  !-TRINDENT!*		 % Current level of indentation of trace output
  !-VISITED!*		 % for PRINTX
	);

!*BTR		  := T;
!*BTRSAVE	  := T;
!*TRACE           := T;
!*TRCOUNT	  := T;
!*TRSTEXPANDMACROS := T;
!-ARBARGNAMES!*   := '(A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11 A12 A13 A14 A15);
!-ARGINDENT!*     := 3;
%!-COLONERRNUM!*   := 993; % Any ideas of anything particularly appropriate?
!-FUNCTIONFLAGS!* := '(EVAL IGNORE LOSE NOCHANGE EXPAND NOEXPAND OPFN DIRECT);
!-INDENTCUTOFF!*  := 12;
!-INDENTDEPTH!*	  := 2;
!-INVISIBLEPROPS!*:= '(TYPE !*LAMBDALINK);
!-NONSTANDARDFNS!*:= '(SMACRO NMACRO CMACRO);
!-TRACECOUNT!*	  := 0;
!-TRINDENT!*	  := -1;	 % It's always incremented BEFORE use
!-TRACEFLAG!*	  := T;
!*MSG := T;
PPFPRINTER!*      := IF GETD 'RPRINT THEN 'RPRINT ELSE 'PRETTYPRINT;
PROPERTYPRINTER!* := IF GETD 'PRETTYPRINT THEN 'PRETTYPRINT ELSE 'PRINT;
STUBPRINTER!*     := 'PRINTX;
STUBREADER!*      := IF GETD 'XREAD THEN '!-REDREADER ELSE '!-READ;
TRACEMAXLEVEL!*   := 10000;	 % Essentially no limit
TRACEMINLEVEL!*	  := 0;
TREXPRINTER!*	  := IF GETD 'RPRINT THEN 'RPRINT ELSE 'PRETTYPRINT;
TRPRINTER!*	  := 'PRINTX;
BrokenFns!*       := Nil;
TracedFns!*       := Nil;

% Let TRST know about the behaviour of some common FEXPRs

FLAG('(	% common FEXPRs which never pass back an unEVALed argument
  AND
  LIST
  MAX
  MIN
  OR
  PLUS
  PROGN
  REPEAT
  TIMES
  WHILE
     ),'TRSTINSIDE);

DEFLIST ('( % special sorts of FEXPRs
  (LAMBDA !-TRSTPROG)	% Not really a function, but handled by TRST as such
  (PROG !-TRSTPROG)
  (SETQ !-TRSTSETQ)
  (COND !-TRSTCOND)
	 ),'TRSTINSIDEFN);

%****************** Utility functions ********************************

% Copy the entrypoints of various key functions so that
% nobody gets muddled by trying to trace or redefine them;

PUTD('!-APPEND,'EXPR,CDR GETD 'APPEND);
PUTD('!-APPLY,'EXPR,CDR GETD 'APPLY);
PUTD('!-ATSOC,'EXPR,CDR GETD 'ATSOC);
%PUTD('!-CAR,'EXPR,CDR GETD 'CAR);
%PUTD('!-CDR,'EXPR,CDR GETD 'CDR);
%PUTD('!-CODEP,'EXPR,CDR GETD 'CODEP);
PUTD('!-COMPRESS,'EXPR,CDR GETD 'COMPRESS);
%PUTD('!-CONS,'EXPR,CDR GETD 'CONS);
PUTD('!-EQUAL,'EXPR,CDR GETD 'EQUAL);
PUTD('!-ERRORSET,'EXPR,CDR GETD 'ERRORSET);
PUTD('!-EVAL,'EXPR,CDR GETD 'EVAL);
%PUTD('!-EVLIS,'EXPR,CDR GETD 'EVLIS);
PUTD('!-EXPLODE,'EXPR,CDR GETD 'EXPLODE);
PUTD('!-FLAG,'EXPR,CDR GETD 'FLAG);
PUTD('!-FLAGP,'EXPR,CDR GETD 'FLAGP);
PUTD('!-FLUID,'EXPR,CDR GETD 'FLUID);
PUTD('!-GET,'EXPR,CDR GETD 'GET);
PUTD('!-GETD,'EXPR,CDR GETD 'GETD);
%PUTD('!-IDP,'EXPR,CDR GETD 'IDP);
PUTD('!-INTERN,'EXPR,CDR GETD 'INTERN);
PUTD('!-LENGTH,'EXPR,CDR GETD 'LENGTH);
PUTD('!-MAX2,'EXPR,CDR GETD 'MAX2);
PUTD('!-MEMQ,'EXPR,CDR GETD 'MEMQ);
PUTD('!-MIN2,'EXPR,CDR GETD 'MIN2);
PUTD('!-OPEN,'EXPR,CDR GETD 'OPEN);
%PUTD('!-PATOM,'EXPR,CDR GETD 'PATOM);
PUTD('!-PLUS2,'EXPR,CDR GETD 'PLUS2);
PUTD('!-POSN,'EXPR,CDR GETD 'POSN);
PUTD('!-PRIN1,'EXPR,CDR GETD 'PRIN1);
PUTD('!-PRIN2,'EXPR,CDR GETD 'PRIN2);
PUTD('!-PRINC,'EXPR,CDR GETD 'PRINC);
PUTD('!-PRINT,'EXPR,CDR GETD 'PRINT);
%PUTD('!-PROG,'FEXPR,CDR GETD 'PROG);
PUTD('!-PUT,'EXPR,CDR GETD 'PUT);
PUTD('!-PUTD,'EXPR,CDR GETD 'PUTD);
PUTD('!-READ,'EXPR,CDR GETD 'READ);
PUTD('!-REMD,'EXPR,CDR GETD 'REMD);
PUTD('!-REMPROP,'EXPR,CDR GETD 'REMPROP);
%PUTD('!-RETURN,'EXPR,CDR GETD 'RETURN);
PUTD('!-REVERSE,'EXPR,CDR GETD 'REVERSE);
%PUTD('!-RPLACA,'EXPR,CDR GETD 'RPLACA);
%PUTD('!-RPLACD,'EXPR,CDR GETD 'RPLACD);
PUTD('!-SET,'EXPR,CDR GETD 'SET);
PUTD('!-TERPRI,'EXPR,CDR GETD 'TERPRI);
PUTD('!-WRS,'EXPR,CDR GETD 'WRS);
%PUTD('!-ZEROP,'EXPR,CDR GETD 'ZEROP);



CompileTime <<

smacro procedure alias(x, y);
    macro procedure x u; 'y . cdr u;

alias(!-DIFFERENCE, IDifference);
alias(!-GREATERP, IGreaterP);
alias(!-LESSP, ILessP);
alias(!-SUB1, ISub1);
alias(!-TIMES2, ITimes2);

load Fast!-Vector;
alias(!-GETV, IGetV);
alias(!-UPBV, ISizeV);

%alias(!-ADD1, IAdd1);
put('!-add1, 'cmacro , '(lambda (x) (iadd1 x)));
>>;

lisp procedure !-ADD1 X;		% because it gets called from EVAL
    IAdd1 X;

SYMBOLIC PROCEDURE !-LPRIE U;
<<  ERRORPRINTF("***** %L", U);
    ERFG!* := T >>;

SYMBOLIC PROCEDURE !-LPRIM U; 
    !*MSG AND ERRORPRINTF("*** %L", U);


PUTD('!-REVERSIP, 'EXPR, CDR GETD 'REVERSIP);
PUTD('!-MKQUOTE, 'EXPR, CDR GETD 'MKQUOTE);
PUTD('!-EQCAR, 'EXPR, CDR GETD 'EQCAR);
PUTD('!-SPACES, 'EXPR, CDR GETD 'SPACES);
PUTD('!-SPACES2, 'EXPR, CDR GETD 'SPACES2);
PUTD('!-PRIN2T, 'EXPR, CDR GETD 'PRIN2T);

SYMBOLIC PROCEDURE !-PAD(L, N);
IF FIXP N THEN
   IF N < !-LENGTH L THEN
      !-PAD(!-REVERSIP CDR !-REVERSE L, N)
   ELSE IF N > !-LENGTH L THEN
      !-PAD(!-APPEND(L, LIST NIL), N)
   ELSE
      L
ELSE
   REDERR "!-PAD given nonintegral second arg";

SYMBOLIC PROCEDURE !-IDLISTP L;
NULL L OR IDP CAR L  AND !-IDLISTP CDR L;

SYMBOLIC PROCEDURE !-CIRLIST(U,N);
% Returns a circular list consisting of N U's.
BEGIN SCALAR A,B;
  IF NOT !-GREATERP(N,0) THEN
    RETURN NIL;
  B := A := U . NIL;
  FOR I := 2:N DO
    B := U . B;
  RETURN RPLACD(A,B)
END !-CIRCLIST;

SYMBOLIC PROCEDURE !-FIRSTN(N,L);
    IF N=0 THEN NIL
    ELSE IF NULL L THEN !-FIRSTN(N,LIST GENSYM())
    ELSE CAR L . !-FIRSTN(!-DIFFERENCE(N,1),CDR L);

SYMBOLIC PROCEDURE !-LISTOFATOMS L;
    IF NULL L THEN T
    ELSE IF IDP CAR L THEN !-LISTOFATOMS CDR L
    ELSE NIL;

SYMBOLIC PROCEDURE !-!-PUTD(NAME,TYPE,BODY);
% as PUTD but never compiles, and preserves TRACE property;
  BEGIN
    SCALAR COMP,SAVER,BOL;
    COMP:=!*COMP; % REMEMBER STATE OF !*COMP FLAG;
    !*COMP:=NIL; % TURN OFF COMPILATION;
    SAVER:=!-GET(NAME,'TRACE);
    BOL:=FLAGP(NAME,'LOSE);
    REMFLAG(LIST NAME,'LOSE);	% IGNORE LOSE FLAG;
    !-REMD NAME; % TO MAKE THE NEXT PUTD QUIET EVEN IF I AM REDEFINING;
    BODY:=!-PUTD(NAME,TYPE,BODY);
    IF NOT NULL SAVER THEN !-PUT(NAME,'TRACE,SAVER);
    !*COMP:=COMP; % RESTORE COMPILATION FLAG;
    IF BOL THEN FLAG(LIST NAME,'LOSE);
    RETURN BODY
  END;


%******* Routines for printing looped and shared structures ******
%
% MAIN ENTRYPOINT:
%
%    PRINTX (A)
%
% !-PRINTS THE LIST A. IF !*SAVENAMES IS TRUE CYCLES ARE PRESERVED
% BETWEEN CALLS TO !-PRINTS;
% PRINTX RETURNS NIL;

%VARIABLES USED -
%
% !-ENTRYPOINTS!*   ASSOCIATION LIST OF POINTS WHERE THE LIST
%		RE-ENTERS ITSELF. VALUE PART OF A-LIST ENTRY
%		IS NIL IF NODE HAS NOT YET BEEN GIVEN A NAME,
%		OTHERWISE IT IS THE NAME USED.
%
% !-VISITED!*	    LIST OF NODES THAT HAVE BEEN ENCOUNTERED DURING
%		CURRENT SCAN OF LIST
%
% !-GLOBALNAMES!*   LIKE !-ENTRYPOINTS!*, BUT STAYS ACTIVE BETWEEN CALLS
%		TO PRINTX
%
% !-PRINTXCOUNT!* USED TO DECIDE ON A NAME FOR THE NEXT NODE;


SYMBOLIC PROCEDURE !-LABELNAME();
    BldMsg("%%L%W", !-PRINTXCOUNT!* := !-PLUS2(!-PRINTXCOUNT!*,1));

SYMBOLIC PROCEDURE !-FINDENTRIES A;
    IF NOT (PAIRP A OR VECTORP A) THEN NIL
    ELSE IF !-ATSOC(A,!-ENTRYPOINTS!*) THEN NIL
    ELSE IF !-MEMQ(A,!-VISITED!*) THEN
	!-ENTRYPOINTS!*:=(A . NIL) . !-ENTRYPOINTS!*
    ELSE
    <<	!-VISITED!*:=A . !-VISITED!*;
	IF VECTORP A THEN
	BEGIN SCALAR N, I;
	    I := 0;
	    N := !-UPBV A;
	    WHILE NOT !-GREATERP(I, N) DO
	    <<  !-FINDENTRIES !-GETV(A,I);
		I := !-ADD1 I >>;
	END ELSE
	<< !-FINDENTRIES CAR A;
	!-FINDENTRIES CDR A >> >>;

SYMBOLIC PROCEDURE !-PRINTPASS A;
    IF NOT (PAIRP A OR VECTORP A) THEN !-PRIN1 A
    ELSE BEGIN SCALAR W, N, I;
	IF !-GREATERP(!-POSN(),50) THEN !-TERPRI();
	W:=!-ATSOC(A,!-ENTRYPOINTS!*);
	IF NULL W THEN GO TO ORDINARY;
	IF CDR W THEN RETURN !-PRIN2 CDR W;
	RPLACD(W,!-PRIN2 !-LABELNAME());
	!-PRIN2 ": ";
ORDINARY:
	IF VECTORP A THEN RETURN
	<<  N := !-UPBV A;
	    !-PRINC '![;
              IF !-GREATERP(N,-1) THEN  % perdue fix
	    <<  !-PRINTPASS !-GETV(A, 0);
		I := 1;
		WHILE NOT !-GREATERP(I, N) DO
		<<  !-PRINC '! ;
		    !-PRINTPASS !-GETV(A, I);
		    I := !-ADD1 I >> >>;
	    !-PRINC '!] >>;
	!-PRINC '!(;
LOOP:
	!-PRINTPASS CAR A;
	A:=CDR A;
	IF NULL A THEN GOTO NILEND
	ELSE IF ATOM A THEN GO TO ATOMEND
	ELSE IF (W:=!-ATSOC(A,!-ENTRYPOINTS!*)) THEN GOTO LABELED;
BLANKIT:
	!-PRINC '! ;
	GO TO LOOP;
LABELED:
	IF CDR W THEN GOTO REFER;
	!-PRINC '! ;
	RPLACD(W,!-PRIN2 !-LABELNAME());
	!-PRIN2 ", ";
	GO TO LOOP;
REFER:
	!-PRIN2 " . ";
	!-PRIN2 CDR W;
	GO TO NILEND;
ATOMEND:
	!-PRIN2 " . ";
	!-PRIN1 A;
NILEND:
	!-PRINC '!);
	RETURN NIL
  END;

SYMBOLIC PROCEDURE !-PRINS(A,L);
  BEGIN
    SCALAR !-VISITED!*,!-ENTRYPOINTS!*,!-PRINTXCOUNT!*;
    IF ATOM L THEN !-PRINTXCOUNT!*:=0
    ELSE << !-PRINTXCOUNT!*:=CAR L; !-ENTRYPOINTS!*:=CDR L >>;
    !-FINDENTRIES A;
    !-PRINTPASS A;
    RETURN (!-PRINTXCOUNT!* . !-ENTRYPOINTS!*)
  END;

SYMBOLIC PROCEDURE PRINTX A;
    <<IF !*SAVENAMES THEN !-GLOBALNAMES!*:=!-PRINS(A,!-GLOBALNAMES!*)
       ELSE !-PRINS(A,NIL);
      !-TERPRI();
      NIL >>;


%****************** Trace sub-property-list functions ******************

% The property TRACE is removed from any function that is subject
% to definition or redefinition by PUTD, and so it represents
% a good place to hide information about the function. The following
% set of functions run a sub-property-list stored under this
% indicator;

SYMBOLIC PROCEDURE !-TRGET(ID,IND);
    !-TRGETX(!-GET(ID,'TRACE),IND);

SYMBOLIC PROCEDURE !-TRGETX(L,IND);
% L IS A 'PROPERTY LIST' AND IND IS AN INDICATOR;
    IF NULL L THEN NIL
    ELSE IF !-EQCAR(CAR L,IND) THEN CDAR L
    ELSE !-TRGETX(CDR L,IND);

SYMBOLIC PROCEDURE !-TRFLAGP(ID,IND);
    !-MEMQ(IND,!-GET(ID,'TRACE));

SYMBOLIC PROCEDURE !-TRPUT(ID,IND,VAL);
    !-PUT(ID,'TRACE,!-TRPUTX(!-GET(ID,'TRACE),IND,VAL));

SYMBOLIC PROCEDURE !-TRPUTX(L,IND,VAL);
IF !-TRPUTX1(L,IND,VAL) THEN L
ELSE (IND . VAL) . L;

SYMBOLIC PROCEDURE !-TRPUTX1(L,IND,VAL);
BEGIN
 L: IF NULL L THEN
      RETURN NIL;
    IF !-EQCAR(CAR L,IND) THEN <<
      RPLACD(CAR L,VAL);
      RETURN T >>;
    L := CDR L;
    GO TO L
END;

SYMBOLIC PROCEDURE !-TRFLAG(L,IND);
FOR EACH ID IN L DO
  !-TRFLAG1(ID,IND);

SYMBOLIC PROCEDURE !-TRFLAG1(ID,IND);
BEGIN SCALAR A;
 A:=!-GET(ID,'TRACE);
 IF NOT !-MEMQ(IND,A) THEN
   !-PUT(ID,'TRACE,IND . A)
END;

SYMBOLIC PROCEDURE !-TRREMPROP(ID,IND);
 << IND:=!-TRREMPROPX(!-GET(ID,'TRACE),IND);
    IF NULL IND THEN !-REMPROP(ID,'TRACE)
    ELSE !-PUT(ID,'TRACE,IND) >>;

SYMBOLIC PROCEDURE !-TRREMPROPX(L,IND);
    IF NULL L THEN NIL
    ELSE IF !-EQCAR(CAR L,IND) THEN CDR L
    ELSE CAR L . !-TRREMPROPX(CDR L,IND);

SYMBOLIC PROCEDURE !-TRREMFLAG(L,IND);
    FOR EACH ID IN L DO !-TRREMFLAG1(ID,IND);

SYMBOLIC PROCEDURE !-TRREMFLAG1(ID,IND);
 << IND:=DELETE(IND,!-GET(ID,'TRACE));
    IF NULL IND THEN !-REMPROP(ID,'TRACE)
    ELSE !-PUT(ID,'TRACE,IND) >>;


%******************* Basic functions for TRACE and friends ***********

SYMBOLIC PROCEDURE !-TRINSTALL(NAM,ARGNUM);
% Sets up TRACE properties for function NAM.  This is common to all  TRACE-like
% actions.  Function NAM  is redefined to  dispatch through !-TRACEDCALL  which
% takes various actions  (which may simply  be to run  the original  function).
% Important items stored under the TRACE property include ORIGINALFN, which  is
% the original definition,  FNTYPE, the original  function "type" (e.g.   EXPR,
% MACRO ...),  and ARGNAMES,  a list  of the  names of	the arguments  to  NAM.
% arguments to the function.  Runs TRINSTALLHOOK!* if non-nil.	Returns non-nil
% if it succeeds, nil if for some reason it fails.
BEGIN SCALAR DEFN,CNTR,ARGS,TYP;
  if Memq (Nam,BrokenFns!*) then
     << EvUnBr List Nam;
        BrokenFns!* := DelQ(Nam,BrokenFns!*) >>;
  DEFN := !-GETD NAM;
  IF NULL DEFN THEN <<
    !-LPRIM LIST("Function",NAM,"is not defined.");
    RETURN NIL >>;
  TYP  := CAR DEFN;
  DEFN := CDR DEFN;
  IF !-GET(NAM,'TRACE) THEN
    IF NUMBERP ARGNUM AND TYP EQ 'FEXPR AND
       !-TRGET(NAM,'FNTYPE) EQ 'EXPR THEN <<
	 TYP := 'EXPR;
	 !-TRREMFLAG(LIST NAM,'UNKNOWNARGS);
	 DEFN := !-TRGET(NAM,'ORIGINALFN) >>
    ELSE
      RETURN T
  ELSE IF TRINSTALLHOOK!* AND
	  NOT !-ERRAPPLY(TRINSTALLHOOK!*,LIST NAM,'TRINSTALLHOOK) THEN
	    RETURN NIL;
  !-TRPUT(NAM,'ORIGINALFN,DEFN);
  !-TRPUT(NAM,'FNTYPE,TYP);
  ARGS := !-ARGNAMES(NAM,DEFN,TYP,ARGNUM);
  IF ARGS EQ 'UNKNOWN THEN <<
    !-TRPUT(NAM,'ARGNAMES,!-ARBARGNAMES!*);
    !-TRFLAG(LIST NAM,'UNKNOWNARGS) >>
  ELSE
    !-TRPUT(NAM,'ARGNAMES,ARGS);
  CNTR := GENSYM();
  !-FLUID LIST CNTR;
  !-TRPUT(NAM,'LEVELVAR,CNTR);
  !-SET(CNTR,0);
  !-TRPUT(NAM,'COUNTER,0);
  IF ARGS EQ 'UNKNOWN THEN
    !-!-PUTD(NAM,
	     'FEXPR,
	     LIST('LAMBDA,
		    '(!-L),
		    LIST(LIST('LAMBDA,
				  LIST(CNTR,'!-TRINDENT!*),
				  LIST('!-TRACEDCALL,
					 !-MKQUOTE NAM,
					 '(!-EVLIS !-L) ) ),
 			   LIST('!-ADD1,CNTR),
			   '!-TRINDENT!*) ) )
  ELSE
    !-!-PUTD(NAM,
	     TYP,
	     LIST('LAMBDA,
		    ARGS,
		    LIST(LIST('LAMBDA,
				  LIST(CNTR,'!-TRINDENT!*),
				  LIST('!-TRACEDCALL,
					 !-MKQUOTE NAM,
					 'LIST . ARGS) ),
			   LIST('!-ADD1,CNTR),
			   '!-TRINDENT!*) ) );
  IF NOT !-MEMQ(NAM,!-INSTALLEDFNS!*) THEN
    !-INSTALLEDFNS!* := NAM . !-INSTALLEDFNS!*;
  RETURN T
END !-TRINSTALL;

SYMBOLIC PROCEDURE !-TRINSTALLIST U;
FOR EACH V IN U DO !-TRINSTALL(V,NIL);

SYMBOLIC PROCEDURE !-ARGNAMES(FN,DEFN,TYPE,NM);
% Tries to discover the names of the arguments	of FN.	NM is a good guess,  as
% for instance based on the arguments to an EMB procedure.  Returns UNKNOWN  if
% it can't find out.  ON TRUNKNOWN will cause it to return UNKNOWN rather  than
% asking the user.
IF !-EQCAR(DEFN,'LAMBDA) THEN		% otherwise it must be a code pointer
  CADR DEFN
ELSE IF NOT TYPE EQ 'EXPR THEN
  LIST CAR !-ARBARGNAMES!*
ELSE IF (TYPE:=!-GET(FN,'ARGUMENTS!*))
	or (TYPE := code!-number!-of!-arguments DEFN) THEN
  IF NUMBERP TYPE THEN
    !-FIRSTN(TYPE,!-ARBARGNAMES!*)
  ELSE
    CAR TYPE
ELSE IF NUMBERP NM THEN
  !-FIRSTN(NM,!-ARBARGNAMES!*)
ELSE IF !*TRUNKNOWN THEN
  'UNKNOWN
ELSE !-ARGNAMES1 FN;
%  BEGIN SCALAR RESULT;
%    RESULT := ERRORSET(LIST('!-ARGNAMES1,!-MKQUOTE FN),NIL,NIL);
%    IF PAIRP RESULT THEN
%      RETURN CAR RESULT
%    ELSE
%      ERROR(RESULT,EMSG!*)
%  END;

FLUID '(PROMPTSTRING!*);

SYMBOLIC PROCEDURE !-ARGNAMES1 FN;
BEGIN SCALAR N, PROMPTSTRING!*;
  PROMPTSTRING!* := BLDMSG("How many arguments does %r take? ", FN);
AGAIN:
  N:=READ();
  IF N='!? THEN <<
    !-TERPRI(); %EXPLAIN OPTIONS;
    !-PRIN2 "Give a number, a list of atoms (for the names of";
    !-TERPRI();
    !-PRIN2 "the arguments) or the word 'UNKNOWN'. System security";
    !-TERPRI();
    !-PRIN2 "will not be good if you say UNKNOWN, but LISP will";
    !-TERPRI();
    !-PRIN2 "at least try to help you";
    !-TERPRI();
%   !-PRIN2 "Number of arguments";
    GO TO AGAIN >>
  ELSE IF N='UNKNOWN THEN
    RETURN N
  ELSE IF FIXP N AND NOT !-LESSP(N,0) THEN
    RETURN !-FIRSTN(N,!-ARBARGNAMES!*)
  ELSE IF !-LISTOFATOMS N THEN
    RETURN N;
  !-TERPRI();
  !-PRIN2 "*** Please try again, ? will explain options ";
  GO TO AGAIN
END !-ARGNAMES1;

SYMBOLIC PROCEDURE !-TRRESTORE U;
BEGIN SCALAR BOD,TYP;
  IF NOT !-GET(U,'TRACE) THEN
    RETURN;
  BOD := !-TRGET(U,'ORIGINALFN);
  TYP := !-TRGET(U,'FNTYPE);
  IF NULL BOD OR NULL TYP THEN <<
    !-LPRIM LIST("Can't restore",U);
    RETURN >>;
  !-REMD U;
  !-PUTD(U,TYP,BOD);
  !-REMPROP(U,'TRACE)
END !-TRRESTORE;

SYMBOLIC PROCEDURE REDEFINED!-PUTD(NAM,TYP,BOD);
BEGIN SCALAR ANSWER;
  REMPROP(NAM,'TRACE);
  ANSWER := !-PUTD(NAM,TYP,BOD);
  IF NULL ANSWER THEN
    RETURN NIL;
  IF !*TRACEALL OR !*INSTALL THEN
    !-TRINSTALL(NAM,NIL);
  IF !*TRACEALL THEN
     << !-TRFLAG(LIST NAM,'TRPRINT);
      If Not Memq (NAM, TracedFns!*) then
         TracedFns!* := NAM . TracedFns!*>>;
  IF PUTDHOOK!* THEN
    APPLY(PUTDHOOK!*,LIST NAM);
  RETURN ANSWER
END;

PUTD('PUTD, 'EXPR, CDR GETD 'REDEFINED!-PUTD);

%FEXPR PROCEDURE DE U;
%PUTD(CAR U,'EXPR,'LAMBDA . CADR U . CDDR U);
%
%FEXPR PROCEDURE DF U;
%PUTD(CAR U,'FEXPR,'LAMBDA . CADR U . CDDR U);
%
%FEXPR PROCEDURE DM U;
%PUTD(CAR U,'MACRO,'LAMBDA . CADR U . CDDR U);

PUT('TRACEALL,'SIMPFG,'((T (SETQ !*INSTALL T))(NIL (SETQ !*INSTALL NIL))));
PUT('INSTALL,'SIMPFG,'((NIL (SETQ !*TRACEALL NIL))));

%*********************************************************************

SYMBOLIC PROCEDURE TROUT U;
% U is a filename.  Redirects trace output there. 
<< IF MSGCHNL!* THEN
    CLOSE MSGCHNL!*;
   MSGCHNL!* := !-OPEN(U,'OUTPUT) >>;

SYMBOLIC PROCEDURE STDTRACE;
<< IF MSGCHNL!* THEN
    CLOSE MSGCHNL!*;
   MSGCHNL!* := NIL >>;

CompileTime <<
SYMBOLIC MACRO PROCEDURE !-OUTRACE U;
% Main trace output handler.  !-OUTRACE(fn,arg1,...argn) calls fn(arg1,...argn)
% as appropriate to print trace information.
LIST('!-OUTRACE1,
     'LIST . MKQUOTE CADR U . FOR EACH V IN CDDR U COLLECT
				                         LIST('!-MKQUOTE,V) );
>>;

SYMBOLIC PROCEDURE !-OUTRACE1 !-U;
BEGIN SCALAR !-STATE;
  IF !-TRACEBUFF!* THEN <<
    RPLACA(!-TRACEBUFF!*,!-U);
    !-TRACEBUFF!* := CDR !-TRACEBUFF!* >>;
  IF !*TRACE THEN <<
    !-STATE := !-ENTERPRI();
    !-EVAL !-U;
    !-EXITPRI !-STATE >>
END !-OUTRACE;

SYMBOLIC PROCEDURE !-DUMPTRACEBUFF DELFLG;
% Prints the ring buffer of saved trace output stored by OUTRACE.
% DELFLG non-nil wipes it clean as well.
BEGIN SCALAR PTR;
  IF NOT !-EQUAL(!-POSN(),0) THEN
    !-TERPRI();
  IF NULL !-TRACEBUFF!* THEN <<
    !-PRIN2T "*** No trace information has been saved ***";
    RETURN >>;
  !-PRIN2T "*** Start of saved trace information ***";
  PTR := !-TRACEBUFF!*;
  REPEAT <<
    !-EVAL CAR PTR;
    IF DELFLG THEN
      RPLACA(PTR,NIL);
    PTR := CDR PTR >>
  UNTIL PTR EQ !-TRACEBUFF!*;
  !-PRIN2T "*** End of saved trace information ***";
END !-DUMPTRACEBUFF;

SYMBOLIC PROCEDURE NEWTRBUFF N;
% Makes a new ring buffer for trace output with N entries.
<< !-TRACEBUFF!* := !-CIRLIST(NIL,N);
   NIL >>;

!-FLAG('(NEWTRBUFF),'OPFN);

NEWTRBUFF 5;

SYMBOLIC PROCEDURE !-TRACEDCALL(!-NAM,!-ARGS);
% Main routine for handling  traced functions.	Currently  saves the number  of
% invocations of the function,	prints trace information,  causes EMB and  TRST
% functions to	be  handled correctly,	calls  several hooks,  and  stacks  and
% unstacks  information in  the BTR  stack, if	appropriate.  Examines	several
% state variables and  a number of  function specific flags  to determine  what
% must be done.
BEGIN SCALAR !-A,!-BOD,!-VAL,!-FLG,!-LOCAL,!-STATE,!-BTRTOP,!-TYP,!-LEV,!-EMB;
  IF !*TRCOUNT THEN
    IF !-A := !-TRGET(!-NAM,'COUNTER) THEN
      !-TRPUT(!-NAM,'COUNTER,!-ADD1 !-A);
  !-TRACECOUNT!* := !-SUB1 !-TRACECOUNT!*;
  IF !-LESSP(!-TRACECOUNT!*,1) THEN <<
    !-TRACEFLAG!* := T;
    IF !-EQUAL(!-TRACECOUNT!*,0) THEN <<
      !-STATE := !-ENTERPRI();
      !-PRIN2 "*** TRACECOUNT reached ***";
      !-EXITPRI !-STATE >> >>;
  IF NOT !-TRACEFLAG!* AND !-TRFLAGP(!-NAM,'TRACEWITHIN) THEN <<
    !-TRACEFLAG!* := !-LOCAL := T;
    !-STATE := !-ENTERPRI();
    !-LPRIM LIST("TRACECOUNT =",!-TRACECOUNT!*);
    !-EXITPRI !-STATE >>;
  IF TRACENTRYHOOK!* THEN
    !-FLG := !-ERRAPPLY(TRACENTRYHOOK!*,
			LIST(!-NAM,!-ARGS),
			'TRACENTRYHOOK)
  ELSE
    !-FLG := T;
  !-LEV := !-EVAL !-TRGET(!-NAM,'LEVELVAR);
  !-FLG := !-FLG AND !-TRACEFLAG!* AND !-TRFLAGP(!-NAM,'TRPRINT) AND
	   NOT(!-LESSP(!-LEV,TRACEMINLEVEL!*) OR
	       !-GREATERP(!-LEV,TRACEMAXLEVEL!*) );
  IF !-FLG AND !-TRFLAGP(!-NAM,'TRST) THEN
    !-BOD := !-TRGET(!-NAM,'TRSTFN) OR !-TRGET(!-NAM,'ORIGINALFN)
  ELSE
    !-BOD := !-TRGET(!-NAM,'ORIGINALFN);
  IF !-FLG THEN <<
    !-TRINDENT!* := !-ADD1 !-TRINDENT!*;
    !-OUTRACE(!-TRACENTRYPRI,!-NAM,!-ARGS,!-LEV,!-TRINDENT!*) >>;
  IF !*BTR THEN
    !-BTRTOP := !-BTRPUSH(!-NAM,!-ARGS);
  !-TYP := !-TRGET(!-NAM,'FNTYPE);
  IF NOT(!-TYP EQ 'EXPR) THEN
    !-ARGS := LIST CAR !-ARGS;
  IF !-TRFLAGP(!-NAM,'EMB) AND (!-EMB := !-TRGET(!-NAM,'EMBFN)) THEN
    !-VAL := !-APPLY(!-EMB,!-BOD . !-ARGS)
  ELSE
    !-VAL := !-APPLY(!-BOD,!-ARGS);
  IF !-TYP EQ 'MACRO THEN <<
    IF TRACEXPANDHOOK!* THEN
      !-ERRAPPLY(TRACEXPANDHOOK!*,
		 LIST(!-NAM,!-VAL),
		 'TRACEXPANDHOOK);
%    IF !-FLG THEN
%      !-OUTRACE(!-TRACEXPANDPRI,!-NAM,!-VAL,!-LEV,!-TRINDENT!*);
%    !-VAL := !-EVAL !-VAL
    >>;
  IF !*BTR THEN
    !-BTRPOP !-BTRTOP;
  IF !-FLG THEN
    !-OUTRACE(!-TRACEXITPRI,!-NAM,!-VAL,!-LEV,!-TRINDENT!*);
  IF !-LOCAL AND !-GREATERP(!-TRACECOUNT!*,0) THEN
    !-TRACEFLAG!* := NIL;
  IF TRACEXITHOOK!* THEN
    !-ERRAPPLY(TRACEXITHOOK!*,LIST(!-NAM,!-VAL),'TRACEXITHOOK);
  RETURN !-VAL
END !-TRACEDCALL;

SYMBOLIC PROCEDURE !-ERRAPPLY(!-FN,!-ARGS,!-NAM);
BEGIN SCALAR !-ANS,!-CHN;
  !-ANS := !-ERRORSET(LIST('!-APPLY,!-FN,!-ARGS),T,!*BACKTRACE);
  IF ATOM !-ANS THEN <<
    !-CHN := !-WRS MSGCHNL!*;
    !-PRIN2 "***** Error occured evaluating ";
    !-PRIN2 !-NAM;
    !-PRIN2 " *****";
    !-TERPRI();
    !-WRS !-CHN;
    RETURN !-ANS >>
  ELSE
    RETURN CAR !-ANS
END !-ERRAPPLY;

%************ Routines for printing trace information ***************

SYMBOLIC PROCEDURE TRACECOUNT N;
% Suppresses TRACE output until N traced function invocations have passed.
BEGIN
  SCALAR OLD;
  OLD:=!-TRACECOUNT!*;
  IF NUMBERP N THEN <<
    !-TRACECOUNT!*:=N;
    IF !-GREATERP(N,0) THEN
      !-TRACEFLAG!*:=NIL
    ELSE
      !-TRACEFLAG!*:=T >>;
  RETURN OLD
END;

!-FLAG('(TRACECOUNT),'OPFN);

SYMBOLIC PROCEDURE TRACEWITHIN L;
% L is a list of function names.  Forces tracing to be enabled within them.
<< !-TRFLAG(L,'TRACEWITHIN);
   IF NOT !-GREATERP(!-TRACECOUNT!*,0) THEN <<
     !-TRACECOUNT!*:=100000;
     !-TRACEFLAG!*:=NIL;
     !-LPRIM "TRACECOUNT set to 100000" >>;
   FOR EACH U IN L CONC
     IF !-TRINSTALL(U,NIL) THEN
       LIST U >>;

SYMBOLIC PROCEDURE TRACE L;
% Enables tracing on each function in the list L.
FOR EACH FN IN L CONC
  IF !-TRINSTALL(FN,NIL) THEN <<
    !-TRFLAG(LIST FN,'TRPRINT);
    If Not Memq (FN, TracedFns!*) then
       TracedFns!* := FN . TracedFns!*;
    LIST FN >>;

SYMBOLIC PROCEDURE UNTRACE L;
% Disables tracing for each function in the list L.
FOR EACH FN IN L CONC <<
  !-TRREMFLAG(LIST FN,'TRACEWITHIN);
  !-TRREMFLAG(LIST FN,'TRST);
  IF !-TRFLAGP(FN,'TRPRINT) THEN <<
    !-TRREMFLAG(LIST FN,'TRPRINT);
    FN >>
  ELSE <<
    !-LPRIM LIST("Function",FN,"was not traced.");
    NIL >> >>;

SYMBOLIC PROCEDURE !-ENTERPRI;
BEGIN SCALAR !-CHN,!-PSN;
  !-CHN := !-WRS MSGCHNL!*;
  !-PSN := !-POSN();
  IF !-GREATERP(!-PSN,0) THEN <<
    !-PRIN2 '!< ;
    !-TERPRI() >>;
  RETURN !-CHN . !-PSN
END !-ENTERPRI;

SYMBOLIC PROCEDURE !-EXITPRI !-STATE;
BEGIN SCALAR !-PSN;
  !-PSN := CDR !-STATE;
  IF !-GREATERP(!-PSN,0) THEN <<
    IF NOT !-LESSP(!-POSN(),!-PSN) THEN
      !-TERPRI();
    !-SPACES2 !-SUB1 !-PSN;
    !-PRIN2 '!> >>
  ELSE IF !-GREATERP(!-POSN(),0) THEN
    !-TERPRI();
  !-WRS CAR !-STATE
END;

SYMBOLIC PROCEDURE !-TRINDENT !-INDNT;
BEGIN SCALAR !-N;
  !-N := !-TIMES2(!-INDNT,!-INDENTDEPTH!*);
  IF NOT !-GREATERP(!-N,!-INDENTCUTOFF!*) THEN
    !-SPACES2 !-N
  ELSE <<
    !-SPACES2 !-INDENTCUTOFF!*;
    !-PRIN2 '!* >>
END !-TRINDENT;

SYMBOLIC PROCEDURE !-TRACEPRI1(!-NAM,!-LEV,!-INDNT);
<< !-TRINDENT !-INDNT;
   !-PRIN1 !-NAM;
   IF !-GREATERP(!-LEV,1) THEN <<
     !-PRIN2 " (level ";
     !-PRIN2 !-LEV;
     !-PRIN2 '!) >> >>;

SYMBOLIC PROCEDURE !-TRACENTRYPRI(!-NAM,!-ARGS,!-LEV,!-INDNT);
% Handles printing trace information at entry to a function.
!-TRACENTRYPRI1(!-NAM,!-ARGS,!-LEV,!-INDNT," being entered");

SYMBOLIC PROCEDURE !-TRACENTRYPRI1(!-NAM,!-ARGS,!-LEV,!-INDNT,!-S);
BEGIN SCALAR !-ARGNAMS;
  !-TRACEPRI1(!-NAM,!-LEV,!-INDNT);
  !-PRIN2 !-S;
  !-TERPRI();
  !-ARGNAMS := !-TRGET(!-NAM,'ARGNAMES);
  WHILE !-ARGS DO <<
    !-TRINDENT !-INDNT;
    !-SPACES !-ARGINDENT!*;
    IF !-ARGNAMS THEN <<
      !-PRIN2 CAR !-ARGNAMS;
      !-ARGNAMS := CDR !-ARGNAMS >>
    ELSE
      !-PRIN2 '!?!?!?!? ;
    !-PRIN2 ":	";
    APPLY(TRPRINTER!*,LIST CAR !-ARGS);
    !-ARGS := CDR !-ARGS;
    IF !-ARGS AND NOT !-POSN() = 0 THEN
      !-TERPRI() >>;
END !-TRACENTRYPRI;

SYMBOLIC PROCEDURE !-TRACEXPANDPRI(!-NAM,!-EXP,!-LEV,!-INDNT);
% Prints macro expansions.
<< !-TRACEPRI1(!-NAM,!-LEV,!-INDNT);
   !-PRIN2 " MACRO expansion = ";
   APPLY(TREXPRINTER!*,LIST !-EXP) >>;

SYMBOLIC PROCEDURE !-TRACEXITPRI(!-NAM,!-VAL,!-LEV,!-INDNT);
% Prints information upon exiting a function.
<< !-TRACEPRI1(!-NAM,!-LEV,!-INDNT);
   !-PRIN2 " = ";
   APPLY(TRPRINTER!*,LIST !-VAL) >>;

%*************** TRST functions ***********************************

SYMBOLIC PROCEDURE TRACESET L;
BEGIN SCALAR DFN;
  RETURN FOR EACH FN IN L CONC
    IF !-TRINSTALL(FN,NIL) THEN <<
      !-TRFLAG(LIST FN,'TRPRINT);
      If Not Memq (FN, TracedFns!*) then
         TracedFns!* := FN . TracedFns!*;
      DFN := !-TRGET(FN,'ORIGINALFN);
      IF CODEP DFN THEN <<
	!-LPRIM LIST("Function",FN,"is compiled.  It cannot be traceset.");
	NIL >>
      ELSE <<
	!-TRFLAG(LIST FN,'TRST);
        IF NOT !-TRGET(FN,'TRSTFN) THEN
	  !-TRPUT(FN,'TRSTFN,!-MKTRST DFN);
	LIST FN >> >>
END TRACESET;

SYMBOLIC PROCEDURE UNTRACESET L;
FOR EACH FN IN L CONC
  IF !-TRFLAGP(FN,'TRST) THEN <<
    !-TRREMFLAG(LIST FN,'TRST);
    LIST FN >>
  ELSE <<
    !-LPRIM LIST("Function",FN,"was not traceset.");
    NIL >>;

SYMBOLIC PROCEDURE !-TRSTPRI(!-NAM,!-VAL);
<< !-OUTRACE(!-TRSTPRI1,!-NAM,!-VAL,!-TRINDENT!*);
   !-VAL >>;

SYMBOLIC PROCEDURE !-TRSTPRI1(!-NAM,!-VAL,!-INDNT);
BEGIN SCALAR !-STATE;
  !-STATE := !-ENTERPRI();
  !-TRINDENT !-INDNT;
  !-PRIN2 !-NAM;
  !-PRIN2 " := ";
  APPLY(TRPRINTER!*,LIST !-VAL);
  !-EXITPRI !-STATE;
END !-TRSTPRI;

SYMBOLIC PROCEDURE !-MKTRST U;
BEGIN SCALAR V;
  IF ATOM U THEN
    RETURN U;
  IF !-FLAGP(CAR U,'TRSTINSIDE) THEN
    RETURN !-MKTRST1 U;
  IF V := !-GET(CAR U,'TRSTINSIDEFN) THEN
    RETURN APPLY(V,LIST U);
  IF IDP CAR U AND (V := !-GETD CAR U) THEN <<
    V := CAR V;
    IF V EQ 'FEXPR THEN
      RETURN U;
    IF V EQ 'MACRO THEN
      IF !*TRSTEXPANDMACROS THEN
	RETURN !-MKTRST APPLY(CAR U,LIST U)
      ELSE
	RETURN U >>;
  RETURN !-MKTRST1 U
END;

SYMBOLIC PROCEDURE !-MKTRST1 U;
FOR EACH V IN U COLLECT !-MKTRST V;

% Functions for TRSTing certain special functions

SYMBOLIC PROCEDURE !-TRSTSETQ U;
IF ATOM CDR U OR ATOM CDDR U THEN
  !-LPRIE LIST("Malformed expression",U)
ELSE
  LIST(CAR U,CADR U,LIST('!-TRSTPRI,!-MKQUOTE CADR U,!-MKTRST CADDR U));

symbolic procedure !-TrstCond u;
cons(car u,
    for each v in cdr u collect !-MkTrST1 v);

SYMBOLIC PROCEDURE !-TRSTPROG U;
IF ATOM CDR U THEN
  !-LPRIE LIST("Malformed expression",U)
ELSE
  CAR U . CADR U . !-MKTRST1 CDDR U;

%****************** Heavy handed backtrace routines *******************

SYMBOLIC PROCEDURE !-BTRPUSH(!-NAM,!-ARGS);
BEGIN SCALAR !-OSTK;
  !-OSTK := !-BTRSTK!*;
  !-BTRSTK!* := (!-NAM . !-ARGS) . !-OSTK;
  RETURN !-OSTK
END !-BTRPUSH;

SYMBOLIC PROCEDURE !-BTRPOP !-PTR;
BEGIN SCALAR !-A;
  IF !*BTRSAVE AND NOT(!-PTR EQ CDR !-BTRSTK!*) THEN <<
    WHILE !-BTRSTK!* AND NOT(!-PTR EQ !-BTRSTK!*) DO <<
      !-A := CAR !-BTRSTK!* . !-A;
      !-BTRSTK!* := CDR !-BTRSTK!* >>;
    IF NOT(!-PTR EQ !-BTRSTK!*) THEN <<
      !-TERPRI();
      !-PRIN2 "***** Internal error in DEBUG: BTR stack underflow *****";
      !-TERPRI() >>;
    !-BTRSAVEDINTERVALS!* := !-A . !-BTRSAVEDINTERVALS!* >>
  ELSE
    !-BTRSTK!* := !-PTR
END !-BTRPOP;

SYMBOLIC PROCEDURE !-BTRDUMP;
BEGIN SCALAR STK;
  STK := !-BTRSTK!*;
  IF NOT (!-POSN() = 0) THEN
    !-TERPRI();
  IF NULL STK AND NOT(!*BTRSAVE AND !-BTRSAVEDINTERVALS!*) THEN <<
    !-PRIN2T "*** No traced functions were left abnormally ***";
    RETURN >>;
  !-PRIN2T "*** Backtrace: ***";
  IF STK THEN <<
    !-PRIN2T "These functions were left abnormally:";
    REPEAT <<
      !-TRACENTRYPRI1(CAAR STK,CDAR STK,1,1,"");
      STK := CDR STK >>
    UNTIL NULL STK >>;
  IF !*BTRSAVE THEN
    FOR EACH U IN !-BTRSAVEDINTERVALS!* DO <<
      !-PRIN2T "These functions were left abnormally, but without";
      !-PRIN2T "returning to top level:";
      FOR EACH V IN U DO
	!-TRACENTRYPRI1(CAR V,CDR V,1,1,"") >>;
  !-PRIN2T "*** End of backtrace ***"
END !-BTRDUMP;

SYMBOLIC PROCEDURE BTRACE L;
<< !*BTR := T;
   !-BTRNEWSTK();
   FOR EACH U IN L CONC
     IF !-TRINSTALL(U,NIL) THEN LIST U >>;

SYMBOLIC PROCEDURE !-BTRNEWSTK;
!-BTRSTK!* := !-BTRSAVEDINTERVALS!* := NIL;

!-BTRNEWSTK();

PUT('BTR,'SIMPFG,'((NIL (!-BTRNEWSTK))(T (!-BTRNEWSTK))));

%********************* Embed functions ****************************

SYMBOLIC PROCEDURE !-EMBSUBST(NAM,FN,NEW);
IF ATOM FN OR CAR FN EQ 'QUOTE THEN
  FN
ELSE IF CAR FN EQ NAM THEN
  NEW . '!-ORIGINALFN!* . CDR FN
ELSE
  FOR EACH U IN FN COLLECT !-EMBSUBST(NAM,U,NEW);

SYMBOLIC MACRO PROCEDURE !-EMBCALL !-U;
LIST('!-APPLY,CADR !-U,'LIST . CDDR !-U);

SYMBOLIC PROCEDURE EMBFN(NAM,VARS,BOD);
BEGIN SCALAR EMBF;
  IF !*DEFN THEN << % For REDUCE;
    OUTDEF LIST('EMBFN,!-MKQUOTE NAM,!-MKQUOTE VARS,!-MKQUOTE BOD);
    RETURN >>;
  IF !-TRINSTALL(NAM,!-LENGTH VARS) THEN <<
    EMBF := !-TRGET(NAM,'EMBFN);
    EMBF := LIST('LAMBDA,
		   '!-ORIGINALFN!* . VARS,
		   !-EMBSUBST(NAM,BOD,IF EMBF THEN EMBF ELSE '!-EMBCALL) );
    !-TRPUT(NAM,'EMBFN,EMBF);
    !-TRFLAG(LIST NAM,'EMB);
    RETURN !-MKQUOTE NAM >>
END;

SYMBOLIC PROCEDURE EMBEDFNS U;
FOR EACH X IN U CONC
  IF !-TRGET(X,'EMBFN) THEN <<
    X := LIST X;
    !-TRFLAG(X,'EMB);
    X >>
  ELSE <<
    !-LPRIM LIST("Procedure",X,"has no EMB definition");
    NIL >>;

SYMBOLIC PROCEDURE UNEMBEDFNS U;
FOR EACH X IN U CONC
  IF !-TRFLAGP(X,'EMB) THEN <<
    X := LIST X;
    !-TRREMFLAG(X,'EMB);
    X >>;

%***************** Function call histogram routines *************

SYMBOLIC PROCEDURE !-HISTOGRAM;
% Simplistic histogram routine for number of function calls.
BEGIN INTEGER M,N,NM; SCALAR NAM,NMS,NEW;
  IF !-GETD 'TREESORT THEN % If REDIO is available
    !-INSTALLEDFNS!* := MSORT !-INSTALLEDFNS!*;
  !-TERPRI();
  !-TERPRI();
  N := 0;
  FOR EACH U IN !-INSTALLEDFNS!* DO
    IF !-GET(U,'TRACE) THEN <<
      N := !-MAX2(!-TRGET(U,'COUNTER),N);
      NEW := U . NEW >>;
  !-INSTALLEDFNS!* := NEW;
  N := FLOAT(LINELENGTH NIL - 21) / FLOAT N;
  FOR EACH U IN !-INSTALLEDFNS!* DO <<
    NAM :=  !-EXPLODE U;
    NM := !-TRGET(U,'COUNTER);
    NMS := !-EXPLODE NM;
    M := !-MIN2(LENGTH NAM,17-LENGTH NMS);
    FOR I := 1:M DO <<
      !-PRINC CAR NAM;
      NAM := CDR NAM >>;
    !-PRINC '!( ;
    WHILE NMS DO <<
      !-PRINC CAR NMS;
      NMS := CDR NMS >>;
    !-PRINC '!) ;
    !-SPACES2 20;
    FOR I := FIX(NM*N) STEP -1 UNTIL 1 DO
      !-PRINC '!* ;
    !-TERPRI() >>;
  !-TERPRI();
  !-TERPRI()
END !-HISTOGRAM;

SYMBOLIC PROCEDURE !-CLEARCOUNT;
BEGIN SCALAR NEWVAL;
  FOR EACH U IN !-INSTALLEDFNS!* DO
    IF !-GET(U,'TRACE) THEN <<
      !-TRPUT(U,'COUNTER,0);
      NEWVAL := U . NEWVAL >>;
  !-INSTALLEDFNS!* := NEWVAL
END !-CLEARCOUNT;

% SIMPFG so ON/OFF TRCOUNT will do a histogram

PUT('TRCOUNT,'SIMPFG,'((T (!-CLEARCOUNT)) (NIL (!-HISTOGRAM))));


%************************ TRACE related statements *********************

%SYMBOLIC PROCEDURE TRSTAT;
%% Nearly the same as RLIS2, but allows zero or more args rather than one or 
%% more.
%BEGIN SCALAR NAM,ARGS;
%  NAM := CURSYM!*;
%  IF FLAGP!*!*(SCAN(),'DELIM) THEN
%    RETURN LIST(NAM,NIL);
%  RETURN LOOP <<
%    ARGS := MKQUOTE CURSYM!* . ARGS;
%    IF FLAGP!*!*(SCAN(),'DELIM) THEN
%      EXIT LIST(NAM,'LIST . REVERSIP ARGS)
%    ELSE IF CURSYM!* NEQ '!*COMMA!* THEN
%      SYMERR("Syntax Error",NIL);
%    SCAN() >>
%END TRSTAT;

SYMBOLIC PROCEDURE !-TR1(L,FN);
BEGIN SCALAR X;
  !-SLOWLINKS();
  X := APPLY(FN,LIST L);
  IF !*MODE EQ 'ALGEBRAIC THEN << % For REDUCE;
    !-TERPRI();
    !-PRINT X >>
  ELSE
    RETURN X
END;

MACRO PROCEDURE TR U;
    LIST('EVTR, MKQUOTE CDR U);

SYMBOLIC PROCEDURE EVTR U;
IF U THEN
  !-TR1(U,'TRACE)
ELSE
  !-DUMPTRACEBUFF NIL;

MACRO PROCEDURE UNTR U;
    LIST('EVUNTR, MKQUOTE CDR U);

procedure UnTrAll();
    <<EvUnTr TracedFns!*;
      TracedFns!* := Nil>>;

SYMBOLIC PROCEDURE EVUNTR U;
BEGIN SCALAR L;
IF U THEN
  <<!-TR1(U,'UNTRACE);
    Foreach L in U do
       TracedFns!*:=DelQ(L,TracedFns!*)>>
ELSE <<
  !-TRACEFLAG!* := NIL;
  !-LPRIM "TRACECOUNT set to 10000";
  !-TRACECOUNT!* := 10000 >>;
END;

MACRO PROCEDURE RESTR U;
  LIST ('EVRESTR, MKQUOTE CDR U);

SYMBOLIC PROCEDURE EVRESTR U;
BEGIN SCALAR L;
   IF U THEN
      <<FOR EACH L IN U DO
          !-TRRESTORE L;
        !-INSTALLEDFNS!* := DELQ (L,!-INSTALLEDFNS!*);
        TRACEDFNS!* := DELQ (L,TRACEDFNS!*)>>
   ELSE
      << FOR EACH U IN !-INSTALLEDFNS!* DO
           !-TRRESTORE U;
         !-INSTALLEDFNS!* := NIL;
         TRACEDFNS!* := NIL>>;
END;

MACRO PROCEDURE TRIN U;
    LIST('EVTRIN, MKQUOTE CDR U);

SYMBOLIC PROCEDURE EVTRIN U; !-TR1(U,'TRACEWITHIN);

MACRO PROCEDURE TRST U;
    LIST('EVTRST, MKQUOTE CDR U);

SYMBOLIC PROCEDURE EVTRST U; !-TR1(U,'TRACESET);

MACRO PROCEDURE UNTRST U;
    LIST('EVUNTRST, MKQUOTE CDR U);

SYMBOLIC PROCEDURE EVUNTRST U; !-TR1(U,'UNTRACESET);

MACRO PROCEDURE BTR U;
    LIST('EVBTR, MKQUOTE CDR U);

SYMBOLIC PROCEDURE EVBTR U;
IF U THEN
  !-TR1(U,'BTRACE)
ELSE
  !-BTRDUMP();

SYMBOLIC PROCEDURE RESBTR; !-BTRNEWSTK();

MACRO PROCEDURE EMBED U;
    LIST('EVEMBED, MKQUOTE CDR U);

SYMBOLIC PROCEDURE EVEMBED U; !-TR1(U,'EMBEDFNS);

MACRO PROCEDURE UNEMBED U;
    LIST('EVUNEMBED, MKQUOTE CDR U);

SYMBOLIC PROCEDURE EVUNEMBED U; !-TR1(U,'UNEMBEDFNS);

MACRO PROCEDURE TRCNT U;
    LIST('EVTRCNT, MKQUOTE CDR U);

SYMBOLIC PROCEDURE EVTRCNT U; !-TR1(U,'!-TRINSTALLIST);

IF NOT FUNBOUNDP 'DEFINEROP THEN <<
RLISTAT('(TR UNTR TRIN TRST UNTRST BTR
	EMBED UNEMBED TRCNT RESTR FSTUB STUB PLIST PPF), 'NOQUOTE);
RLISTAT('(TROUT), 'NOQUOTE);
DEFINEROP('RESBTR,NIL,ESTAT('RESBTR));
DEFINEROP('STDTRACE,NIL,ESTAT('STDTRACE));
>>;

%DEFLIST('(
%  (TR TRSTAT)
%  (UNTR RLIS2)
%  (TRIN RLIS2)
%  (TRST RLIS2)
%  (UNTRST RLIS2)
%  (BTR TRSTAT)
%  (EMBED RLIS2)
%  (UNEMBED RLIS2)
%  (TRCNT RLIS2)
%  (RESBTR ENDSTAT)
%  (RESTR RLIS2)
%  (STDTRACE ENDSTAT)
%  (TROUT IOSTAT)
%         ), 'STAT);

FLAG('(TR UNTR BTR),'GO);

FLAG('(TR TRIN UNTR TRST UNTRST BTR EMBED UNEMBED RESBTR RESTR TRCNT 
       TROUT STDTRACE),
     'IGNORE);

%******************Break Functions***********************************

fluid '(ArgLst!*			% Default names for args in traced code
	TrSpace!*			% Number spaces to indent
	!*NoTrArgs			% Control arg-trace
);

CompileTime flag('(TrMakeArgList), 'InternalFunction);

lisp procedure TrMakeArgList N;		% Get Arglist for N args
    cdr Assoc(N, ArgLst!*);
LoadTime
<<  ArgLst!* := '((0 . ())
		  (1 . (X1))
		  (2 . (X1 X2))
		  (3 . (X1 X2 X3))
		  (4 . (X1 X2 X3 X4))
		  (5 . (X1 X2 X3 X4 X5))
		  (6 . (X1 X2 X3 X4 X5 X6))
		  (7 . (X1 X2 X3 X4 X5 X6 X7))
		  (8 . (X1 X2 X3 X4 X5 X6 X7 X8))
		  (9 . (X1 X2 X3 X4 X5 X6 X7 X8 X9))
		  (10 . (X1 X2 X3 X4 X5 X6 X7 X8 X9 X10))
		  (11 . (X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11))
		  (12 . (X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12))
		  (13 . (X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12 X13))
		  (14 . (X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12 X13 X14))
		  (15 . (X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12 X13 X14 X15)));
    TrSpace!* := 0;
    !*NoTrArgs := NIL >>;

Fluid '(ErrorForm!* !*ContinuableError);

lisp procedure Br!.Prc(PN, B, A); 	% Called in place of "Broken" code
%
% Called by BREAKFN for proc nam PN, body B, args A;
%
begin scalar K, SvArgs, VV, Numb, Result;
    TrSpace!* := TrSpace!* + 1;
    Numb := Min(TrSpace!*, 15);
    Tab Numb;
    PrintF("%p %w:", PN, TrSpace!*);
    if not !*NoTrArgs then
    <<  SvArgs := A;
	K := 1;
	while SvArgs do
	<<  PrintF(" Arg%w:=%p, ", K, car SvArgs);
	    SvArgs := cdr SvArgs;
	    K := K + 1 >> >>;
    TerPri();
    ErrorForm!* := NIL;
    PrintF(" BREAK before entering %r%n",PN);
    !*ContinuableError:=T;
    Break();
    VV := Apply(B, A);
    PrintF(" BREAK after call %r, value %r%n",PN,VV);
    ErrorForm!* := MkQuote VV;
    !*ContinuableError:=T;
    Result:=Break();
    Tab Numb;
    PrintF("%p %w:=%p%n", PN, TrSpace!*, Result);
    TrSpace!* := TrSpace!* - 1;
    return Result
end;

fluid '(!*Comp PromptString!*);

lisp procedure Br!.1 Nam; 		% Called To Break a single function
begin scalar PN, X, Y, Bod, Args, N, OldIn, OldPrompt, !*Comp;
    if not (Y:=GetD Nam) then
    <<  ErrorPrintF("*** %r is not a defined function and cannot be BROKEN",
			Nam);
	return >>;
    if Memq (Nam,TracedFns!*) or Memq (Nam,!-InstalledFns!*) then
        <<!-TrRestore Nam;
          Y:=GetD Nam;
          !-InstalledFns!*:=DelQ(Nam,!-InstalledFns!*);
          TracedFns!*:=DelQ(Nam,TracedFns!*)>>;
    if Not Memq (Nam,BrokenFns!*) then
        BrokenFns!*:=Cons(Nam, BrokenFns!*);
    PN := GenSym();
    !-!-PutD(PN, car Y, cdr Y);
    put(Nam, 'OldCod, Y . get(Nam, 'OldCod));
    if EqCar(cdr Y, 'LAMBDA) then
       Args := cadr cdr Y
    else if (N:=Code!-Number!-Of!-Arguments Cdr Y) then
       Args := TrMakeArgList N
    else
    <<  OldPrompt := PromptString!*;
	PromptString!* := BldMsg("How many arguments for %r?", Nam);
	OldIn := RDS NIL;
	while not NumberP(N := Read()) or N < 0 or N > 15 do ;
	PromptString!* := OldPrompt;
	RDS OldIn;
	Args := TrMakeArgList N >>;
    Bod:= list('LAMBDA, Args,
			list('Br!.prc, MkQuote Nam,
				       MkQuote PN, 'LIST . Args));
    !-!-PutD(Nam, car Y, Bod);
    put(Nam, 'BreakCode, cdr GetD Nam);
end;

lisp procedure UnBr!.1 Nam;
begin scalar X, Y, !*Comp;
   if not IDP Nam or not PairP(X := get(Nam, 'OldCod))
	    or not PairP(Y := GetD Nam)
	    or not (cdr Y eq get(Nam, 'BreakCode)) then
    <<  ErrorPrintF("*** %r cannot be unbroken", Nam);
	return >>;
    !-!-PutD(Nam, caar X, cdar X);
    RemProp(Nam, 'OldCod);
    RemProp(Nam, 'Breakcode);
    BrokenFns!*:=DelQ(Nam,BrokenFns!*);
end;

macro procedure Br L;			%. Break functions in L
    list('EvBr, MkQuote cdr L);

expr procedure EvBr L;
    Begin;
      for each X in L do Br!.1 X;
      Return L
    end;

macro procedure UnBr L;			%. Unbreak functions in L
    list('EvUnBr, MkQuote cdr L);

expr procedure EvUnBr L;
    for each X in L do UnBr!.1 X;

expr procedure UnBrAll();
    <<EvUnBr BrokenFns!*;
      BrokenFns!* := Nil>>;

%************************ Stubs *************************************

% These procedures implement  stubs for Rlisp/Reduce.   Usage is  "STUB
% <model   function   invocation>   [,<model   function   invocation>]*
% <semicol>".  For example,  to declare function  FOO, BAR, and  BLETCH
% with formal parameters X,Y,Z for FOO, U for BAR, and none for  BLETCH
% do "STUB FOO(X,Y,Z),BAR U,  BLETCH();".  When a  stub is executed  it
% announces its invocation,  prettyprints its arguments,  and asks  for
% the value to return.  Fexpr stubs may be declared with the  analogous
% statement FSTUB.

MACRO PROCEDURE STUB U;
    LIST('EVSTUB, MKQUOTE CDR U);

SYMBOLIC PROCEDURE EVSTUB FNLIS;
FOR EACH Y IN FNLIS DO
  IF NOT PAIRP Y THEN
    IF NOT IDP Y THEN
      !-LPRIE "Function name must be an ID"
    ELSE <<
      !-LPRIM LIST("Stub",Y,"declared as a function of zero arguments");
      !-MKSTUB(Y,NIL,'EXPR) >>
  ELSE IF NOT IDP CAR Y THEN
    !-LPRIE "Function name must be an ID"
  ELSE IF NOT !-IDLISTP CDR Y THEN
    !-LPRIE "Formal parameter must be an ID"
  ELSE
    !-MKSTUB(CAR Y,CDR Y,'EXPR);

MACRO PROCEDURE FSTUB U;
    LIST('EVFSTUB, MKQUOTE CDR U);

SYMBOLIC PROCEDURE EVFSTUB FNLIS;
FOR EACH Y IN FNLIS DO
   IF NOT PAIRP Y THEN
      !-LPRIE "Arguments to FSTUB must be model function calls"
   ELSE IF NOT IDP CAR Y THEN
      !-LPRIE "Function name must be an ID"
   ELSE IF NOT !-IDLISTP CDR Y THEN
      !-LPRIE "Formal parameter must be an ID"
   ELSE IF !-LENGTH CDR Y NEQ 1 THEN
      !-LPRIE "An FEXPR must have exactly one formal parameter"
   ELSE
      !-MKSTUB(CAR Y, CDR Y, 'FEXPR);


SYMBOLIC PROCEDURE !-MKSTUB(NAME, VARLIS, TYPE);
PUTD(NAME,
     TYPE,
     LIST('LAMBDA,
	  VARLIS,
	  LIST('!-STUB1,
	       !-MKQUOTE NAME,
	       !-MKQUOTE VARLIS,
	       'LIST . VARLIS,
	       !-MKQUOTE TYPE) ) );

SYMBOLIC PROCEDURE !-STUB1(!-PNAME, !-ANAMES, !-AVALS, !-TYPE);
% Weird variable names because of call to EVAL.
BEGIN INTEGER !-I;
   IF !-TYPE NEQ 'EXPR THEN
      !-PRIN2 !-TYPE;
   !-PRIN2 " Stub ";
   !-PRIN2 !-PNAME;
   !-PRIN2 " called";
   !-TERPRI();
   !-TERPRI();
   !-I := 1;
   FOR EACH !-U IN PAIR(!-PAD(!-ANAMES,!-LENGTH !-AVALS),!-AVALS) DO <<
      IF CAR !-U THEN
	 !-PRIN2 CAR !-U
      ELSE <<
	 !-SET(!-INTERN !-COMPRESS !-APPEND('(A R G),!-EXPLODE !-I),
	     CDR !-U);
	 !-PRIN2 "Arg #";
	 !-PRIN2 !-I >>;
      !-PRIN2 ": ";
      APPLY(STUBPRINTER!*, LIST CDR !-U);
      !-I := !-I + 1 >>;
   !-PRIN2T "Return? :";
   RETURN !-EVAL APPLY(STUBREADER!*,NIL)
END;

SYMBOLIC PROCEDURE !-REDREADER;
XREAD NIL;

%*************** Functions for printing useful information *************

MACRO PROCEDURE PLIST U;
    LIST('EVPLIST, MKQUOTE CDR U);

SYMBOLIC PROCEDURE EVPLIST U;
% Prints the  property	list and  flags  of  U in  a  descent  format,
% prettyprinting nasty	things.   Does	not print  properties  in  the
% global list !-INVISIBLEPROPS!* or flags in !-INVISIBLEFLAGS!*.  Usage is
% "PLIST <id> [,<id>]* <semicol>".
<< !-TERPRI();
   FOR EACH V IN U CONC
     IF V := !-PLIST1 V THEN
       LIST V >>;


SYMBOLIC PROCEDURE !-PLIST1 U;
BEGIN SCALAR PLST,FLGS,HASPROPS;
  !-TERPRI();
  IF NOT IDP U THEN <<
    !-LPRIE LIST(U,"is not an ID");
    RETURN NIL >>;
  PLST := !-GETPROPERTYLIST U; % System dependent kludge
  FOR EACH V IN PLST DO
    IF ATOM V AND NOT !-MEMQ(V,!-INVISIBLEFLAGS!*) THEN
      FLGS := V . FLGS
    ELSE IF NOT !-MEMQ(CAR V,!-INVISIBLEPROPS!*) THEN <<
      IF NOT HASPROPS THEN <<
	HASPROPS := T;
	!-PRIN2 "Properties for ";
	!-PRIN1 U;
	!-PRIN2T ":";
	!-TERPRI() >>;
      !-SPACES 4;
      !-PRIN1 CAR V;
      !-PRIN2 ":";
      !-SPACES 2;
      !-SPACES2 15;
      APPLY(PROPERTYPRINTER!*,LIST CDR V) >>;
  IF FLGS THEN <<
    IF HASPROPS THEN
      !-PRIN2 "Flags:  "
    ELSE <<
      !-PRIN2 "Flags for ";
      !-PRIN1 U;
      !-PRIN2 ":	" >>;
    FOR EACH V IN FLGS DO <<
      !-PRIN1 V;
      !-SPACES 1 >>;
    !-TERPRI();
    !-TERPRI() >>
  ELSE IF NOT HASPROPS THEN <<
    !-PRIN2 "No Flags or Properties for ";
    !-PRINT U;
    !-TERPRI() >>;
  IF HASPROPS OR FLGS THEN
    RETURN U
END !-PLIST1;

MACRO PROCEDURE PPF U;
    LIST('EVPPF, MKQUOTE CDR U);

SYMBOLIC PROCEDURE EVPPF FLIS; 
% Pretty prints one or more function definitions, from their
% names.  Usage is "PPF <name> [,<name>]* <semicol>".
<< !-TERPRI();
   FOR EACH FN IN FLIS CONC
     IF FN := !-PPF1 FN THEN
       LIST FN >>;

SYMBOLIC PROCEDURE !-PPF1 FN;
BEGIN SCALAR BOD,TYP,ARGS,TRC,FLGS;
  IF !-GET(FN,'TRACE) THEN <<
    BOD := !-TRGET(FN,'ORIGINALFN);
    IF NOT CODEP BOD THEN
      BOD := CADDR BOD;
    TYP := !-TRGET(FN,'FNTYPE);
    IF NOT !-TRFLAGP(FN,'UNKNOWNARGS) THEN 
      ARGS := !-TRGET(FN,'ARGNAMES);
    IF !-TRFLAGP(FN,'TRST) THEN
      TRC := 'TraceSet . TRC
    ELSE IF !-TRFLAGP(FN,'TRPRINT) THEN
      TRC := 'Traced . TRC;
    IF !-TRFLAGP(FN,'TRACEWITHIN) THEN
      TRC := 'TracedWithin . TRC;
    IF !-TRFLAGP(FN,'EMB) THEN
      TRC := 'Embeded . TRC;
    IF NULL TRC THEN
      TRC := '(Installed) >>
  ELSE IF BOD := !-GETC FN THEN <<
    TYP := CAR BOD;
    BOD := CDR BOD;
    IF NOT CODEP BOD THEN <<
      ARGS := CADR BOD;
      BOD := CDDR BOD >> >>
  ELSE <<
    !-LPRIE LIST("Procedure",FN,"is not defined.");
    RETURN NIL >>;
  FOR EACH U IN !-FUNCTIONFLAGS!* DO
    IF !-FLAGP(FN,U) THEN
      FLGS := U . FLGS;
  IF NOT (!-POSN() = 0) THEN
    !-TERPRI();
  !-TERPRI();
  !-PRIN2 TYP;
  !-PRIN2 " procedure ";
  !-PRIN1 FN;
  IF ARGS THEN <<
    !-PRIN2 '!( ;
    FOR EACH U ON ARGS DO <<
      !-PRIN1 CAR U;
      IF CDR U THEN
	!-PRIN2 '!, >>;
    !-PRIN2 '!) >>;
  IF TRC OR FLGS THEN <<
    !-PRIN2 " [";
    FOR EACH U IN !-REVERSIP TRC DO <<
      !-PRIN2 U;
      !-PRIN2 '!; >>;
    IF TRC THEN <<
      !-PRIN2 "Invoked ";
      !-PRIN2 !-TRGET(FN,'COUNTER);
      !-PRIN2 " times";
      IF FLGS THEN
	!-PRIN2 '!; >>;
    IF FLGS THEN <<
      !-PRIN2 "Flagged: ";
      FOR EACH U ON FLGS DO <<
	!-PRIN1 CAR U;
	IF CDR U THEN
	  !-PRIN2 '!, >> >>;
    !-PRIN2 '!] >>;
  IF CODEP BOD THEN <<
    !-PRIN2 " is compiled (";
    !-PRIN2 BOD;
    !-PRIN2T ")." >>
  ELSE <<
    !-PRIN2T '!: ;
    FOR EACH FORM IN BOD DO APPLY(PPFPRINTER!*,LIST FORM);
    !-TERPRI() >>;
  RETURN FN  
END !-PPF1;


SYMBOLIC PROCEDURE !-GETC U;
% Like GETD,  but  also  looks for  non-standard  functions,  such  as
% SMACROs.  The only non-standard functions looked for are those whose
% tags appear in the list NONSTANDARDFNS!*.
BEGIN SCALAR X,Y;
  X := !-NONSTANDARDFNS!*;
  Y := !-GETD U;
  WHILE X AND NOT Y DO <<
    Y := !-GET(U,CAR X);
    IF Y THEN
      Y := CAR X . Y;
    X := CDR X >>;
  RETURN Y
END !-GETC;

FLAG('(PPF PLIST), 'IGNORE);

END;
