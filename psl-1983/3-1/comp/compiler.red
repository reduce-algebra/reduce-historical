% MLG: 15 Dec
%   added additional arguments to 
%    Compiler BUG message in &LOCATE to get more info
%  <PSL.COMP>COMPILER.RED.19,  3-Dec-82 18:21:21, Edit by PERDUE
%  Removed REFORMNE, which was over-optimizing sometimes
%  <PSL.COMP>COMPILER.RED.18,  1-Dec-82 15:59:45, Edit by BENSON
%  Fixed car of atom bug in &PaApply
%  New extended compiler for PSL
%    John Peterson    4-5-81

%  <PSL.COMP>COMPILER.RED.4, 20-Sep-82 11:40:31, Edit by BENSON
%  Slight improvement to "FOO not compiled" messages
%  <PSL.COMP>COMPILER.RED.2, 20-Sep-82 10:32:51, Edit by BENSON
%  (DE FOO (LIST) (LIST LIST)) does the right thing
%  <PSL.COMP>COMPILER.RED.10, 10-Sep-82 12:43:27, Edit by BENSON
%  NONLOCALSYS calls NONLOCALLISP if not WVAR or WARRAY
%  <PSL.COMP>COMPILER.RED.9, 10-Sep-82 09:53:08, Edit by BENSON
%  Changed error and warning messages

CompileTime flag(
'(!&COMPERROR !&COMPWARN !&IREG
!&ADDRVALS !&ALLARGS1 !&ALLCONST !&ANYREG !&ANYREGL !&ANYREGP 
!&ARGLOC !&ASSOCOP1 !&ASSOCOP2 !&ATTACH !&ATTJMP !&ATTLBL !&CALL 
!&CALL1 !&CALLOPEN !&CFNTYPE !&CLASSMEMBER !&CLRSTR !&COMLIS !&COMLIS1 
!&COMOPENTST !&COMPLY !&COMTST !&COMVAL !&COMVAL1 !&CONSTTAG
!&DEFEQLBL !&DEFEQLBL1 !&DELARG !&DELCLASS !&DELETEMAC !&DELMAC 
!&EMITMAC !&EQP !&EQPL !&EQVP !&EXTERNALVARP !&FIXCHAINS !&FIXFRM 
!&FIXLABS !&FIXLINKS !&FIXREGTEST1
!&FRAME !&FREERSTR !&GENLBL !&GENSYM !&GETFRAMES 
!&GETFRAMES1 !&GETFRAMES2 !&GETFRM !&GETFVAR !&GETGROUPARGS !&GETGROUPARGS1 
!&GETGROUPARGS2 !&GETLBL !&GETNUM !&HIGHEST !&HIGHEST1 !&HIGHEST2 
!&INALL !&INSERTMAC !&INSOP !&INSOP1 !&INSTALLDESTROY !&INSTBL !&JUMPNIL 
!&JUMPT !&LABCLASS !&LBLEQ !&LOADARGS !&LOADOPENEXP !&LOADTEMP1 !&LOADTEMP2 
!&LOADTEMPREG !&LOCATE !&LOCATEL !&LREG !&LREG1 !&MACROSUBST !&MACROSUBST1 
!&MACROSUBST2 !&MAKEADDRESS !&MAKEXP !&MATCHES !&MEMADDRESS !&MKFRAME 
!&MKFUNC !&MKNAM !&MKPROGN !&MKREG !&MOVEJUMP &NOANYREG1 
!&NOSIDEEFFECTP !&NOSIDEEFFECTPL !&OPENFNP !&OPENP !&OPENPL
!&PA1V !&PALISV
!&PA1X !&PAASSOC1 !&PAEQUAL1 !&PALIS !&PAMAPCOLLECT !&PAMAPCONC !&PAMAPDO 
!&PAMEMBER1 !&PANONLOCAL !&PAPROGBOD !&PASS1 !&PASS2 !&PASS3 !&PEEPHOLEOPT 
!&PROTECT !&RASSOC !&REFERENCES !&REFERENCESL !&REFEXTERNAL !&REFEXTERNALL 
!&REFMEMORY !&REFMEMORYL !&REFORMMACROS !&REGP !&REGVAL !&REMCODE 
!&REMMREFS !&REMMREFS1 !&REMOPEN !&REMREFS !&REMREFS1 !&REMREGS !&REMREGSL 
!&REMTAGS !&REMTAGS1 !&REMTAGS2 !&REMTAGS3 !&REMTAGS4 !&REMUNUSEDMAC 
!&REMVARL !&REMVREFS !&REMVREFS1 !&REPASC !&RMERGE !&RSTVAR !&RSTVARL !&RVAL 
!&SAVER1 !&STORELOCAL !&STOREVAR !&SUBARG !&SUBARGS !&TEMPREG !&TRANSFERP 
!&UNPROTECT !&UNUSEDLBLS !&USESDESTL !&VARBIND !&VARP !&WCONSTP
!&CONSTP ISAWCONST MKNONLOCAL MKWCONST NONLOCAL NONLOCALLISP 
NONLOCALSYS PA1ERR WARRAYP WCONSTP WVARP),
'InternalFunction);

GLOBAL '(ERFG!*
        !*NOLINKE !*ORD !*R2I !*UNSAFEBINDER
        MAXNARGS!&
        !*NOFRAMEFLUID !*USEREGFLUID
        !*INSTALLDESTROY
	!*USINGDESTROY
        !*SHOWDEST
	GLOBALGENSYM!&);	% list of symbols to be re-used by the compiler

FLUID '(ALSTS!& FLAGG!& NAME!& GOLIST!& CODELIST!& CONDTAIL!&
        LLNGTH!& NARG!& REGS!& EXITT!& LBLIST!& JMPLIST!& SLST!& STOMAP!&
	LASTACTUALREG!& DFPRINT!* !*PLAP
	!*SYSLISP
	SWITCH!&
        TOPLAB!&
        FREEBOUND!&
        STATUS!&
        REGS1!&
	PREGS!& DESTREG!&
        EXITREGS!&
        DEST!& ENVIRONMENT!&
        HOLEMAP!&
	LOCALGENSYM!&);	 % traveling pointer into GLOBALGENSYM!&

%COMMENT **************************************************************
%**********************************************************************
%                      THE STANDARD LISP COMPILER
%**********************************************************************
%                        Augmented for SYSLISP
%*********************************************************************; 
%
%COMMENT machine dependent parts are in a separate file; 
%
%COMMENT these include the macros described below and, in addition,
%	an auxiliary function !&MKFUNC which is required to pass
%	functional arguments (input as FUNCTION <func>) to the
%	loader. In most cases, !&MKFUNC may be defined as MKQUOTE; 
%
%COMMENT Registers used:
%1-MAXNARGS!&	used for args of link. result returned in reg 1; 
%
%COMMENT Macros used in this compiler; 
%
%COMMENT The following macros must NOT change REGS!& 1-MAXNARGS!&:
%!*ALLOC nw      	allocate new stack frame of nw words
%!*DEALLOC nw		deallocate above frame
%!*ENTRY	name type noargs   entry point to function name of type type
%			   with noargs args
%!*EXIT			EXIT to previously saved return address
%!*JUMP adr  		unconditional jump
%!*LBL adr		define label
%!*LAMBIND regs alst	bind free lambda vars in alst currently in regs
%!*PROGBIND alst		bind free prog vars in alst
%!*FREERSTR alst		unbind free variables in alst
%!*STORE reg floc	store contents of reg (or NIL) in floc
%
%COMMENT the following macro must only change specific register being
%	loaded:
%
%!*LOAD reg exp		load exp into reg; 
%
%COMMENT the following macros do not protect regs 1-MAXNARGS!&:
%
%!*LINK fn type nargs	  link to fn of type type with nargs args
%!*LINKE fn type nargs nw  link to fn of type type with nargs args
%			     and EXITT!& removing frame of nw words; 
%
%
%COMMENT variable types are: 
%
%  LOCAL		allocated on stack and known only locally
%  GLOBAL	accessed via cell (GLOBAL name) known to
%	        loader at load time
%  WGLOBAL	accessed via cell (WGLOBAL name) known to
%	        loader at load time, SYSLISP
%  FLUID		accessed via cell (FLUID name)
%		known to loader. This cell is rebound by LAMBIND/
%		PROGBIND if variable used in lambda/prog list
%		and restored by FREERSTR; 
%
%COMMENT global flags used in this compiler:
%!*UNSAFEBINDER	for Don's BAKER problem...GC may be called in
%		Binder, so regs cant be preserved
%!*MODULE	indicates block compilation (a future extension of
%		this compiler)
%!*NOLINKE 	if ON inhibits use of !*LINKE macro
%!*ORD		if ON forces left-to-right argument evaluation
%!*PLAP		if ON causes LAP output to be printed
%!*R2I		if ON causes recursion removal where possible;
%
%
%COMMENT global variables used:
%
%DFPRINT!*	name of special definition process (or NIL)
%ERFG!*		used by REDUCE to control error recovery
%MAXNARGS!&	maximum number of arguments permitted in implementation;
%
%
%
%%Standard LISP limit;
%
%COMMENT fluid variables used:
%
%ALSTS	alist of fluid parameters
%FLAGG	used in COMTST, and in FIXREST
%FREEBOUND indicates that some variables were FLUID
%GOLIST	storage map for jump labels
%PREGS   A list of protected registers
%CODELIST  code being built
%CONDTAIL simulated stack of position in the tail of a COND
%LLNGTH	cell whose CAR is length of frame
%NAME	NAME!& of function being currently compiled
%FNAME!&	name of function being currently compiled, set by COMPILE
%NARG	number of arguments in function
%REGS	known current contents of registers as an alist with elements 
%	of form (<reg> . <contents>)
%EXITT	label for *EXIT jump
%EXITREGS List or register statuses at return point
%LBLIST	list of label words
%JMPLIST	list of locations in CODELIST!& of transfers
%SLST	association list for stores which have not yet been used
%STOMAP	storage map for variables
%SWITCH	boolean expression value flag - keeps track of NULLs; 
%

SYMBOLIC PROCEDURE !&MKFUNC FN; MKQUOTE FN;

SYMBOLIC PROCEDURE WARRAYP X;
 GET(X,'WARRAY) OR GET(X, 'WSTRING);

SYMBOLIC PROCEDURE WVARP X;
  GET(X,'WVAR);

SYMBOLIC PROCEDURE WCONSTP X;
  NUMBERP X OR (IDP X AND GET(X,'WCONST));

SYMBOLIC PROCEDURE !&ANYREGP X;
  FLAGP(X, 'ANYREG);

macro procedure LocalF U;	% declare functions internal, ala Franz
    list('flag, Mkquote cdr U, ''InternalFunction);

%************************************************************
%        The compiler
%************************************************************

% Top level compile entry - X is list of functions to compile

SYMBOLIC PROCEDURE COMPILE X; 
   BEGIN SCALAR EXP; 
       FOR EACH FNAME!& IN X DO
         <<EXP := GETD FNAME!&; 
           IF NULL EXP THEN !&COMPWARN LIST("No definition for", FNAME!&)
	   ELSE IF CODEP CDR EXP THEN
	       !&COMPWARN LIST(FNAME!&, "already compiled")
            ELSE COMPD(FNAME!&,CAR EXP,CDR EXP)>>
   END;

% COMPD - Single function compiler
% Makes sure function type is compilable; sends original definition to
% DFPRINT!*, then compiles the function.  Shows LAP code when PLAP is on.
% Runs LAP and adds COMPFN property if LAP indeed redefines the function.

SYMBOLIC PROCEDURE COMPD(NAME!&,TY,EXP); 
   BEGIN 
      IF NOT FLAGP(TY,'COMPILE)
        THEN <<!&COMPERROR LIST("Uncompilable function type", TY); 
               RETURN NIL>>; 
      IF NOT EQCAR(EXP, 'LAMBDA)
	THEN
	<<  !&COMPERROR LIST("Attempt to compile non-lambda expression", EXP);
	    RETURN NIL >>
%/        ELSE IF !*MODULE THEN MODCMP(NAME!&,TY,EXP)
%              ELSE IF DFPRINT!*
%               THEN APPLY(DFPRINT!*,LIST IF TY EQ 'EXPR
%                                  THEN 'DE . (NAME!& . CDR EXP)
%                                 ELSE IF TY EQ 'FEXPR
%                                  THEN 'DF . (NAME!& . CDR EXP)
%                                 ELSE IF TY EQ 'MACRO
%%                                  THEN 'DM . (NAME!& . CDR EXP)
%                                 ELSE IF TY EQ 'NEXPR
%                                  THEN 'DN . (NAME!& . CDR EXP)
%                                 ELSE LIST('PUTD,MKQUOTE NAME!&,
%                                           MKQUOTE TY,
%                                           MKQUOTE EXP))
              ELSE BEGIN SCALAR X; 
                      IF TY MEMQ '(EXPR FEXPR)
                        THEN PUT(NAME!&,'CFNTYPE,LIST TY); 
                      X := 
                       LIST('!*ENTRY,NAME!&,TY,LENGTH CADR EXP)
                         . !&COMPROC(EXP,
                                     IF TY MEMQ '(EXPR FEXPR)
                                       THEN NAME!&); 
                      IF !*PLAP THEN FOR EACH Y IN X DO PRINT Y; 
		      % ***Code**Pointer** is a magic token that tells
		      % COMPD to return a code pointer instead of an ID
		      IF NAME!& = '!*!*!*Code!*!*Pointer!*!*!* then
		          NAME!& := LAP X
		      ELSE
		      <<  LAP X;
		          %this is the hook to the assembler. LAP must
		          %remove old function definition if it exists;
		          IF (X := GET(NAME!&,'CFNTYPE))
			      AND EQCAR(GETD NAME!&,CAR X)
			  THEN REMPROP(NAME!&,'CFNTYPE) >>
                   END; 
      RETURN NAME!&
   END;

%************************************************************
%   Pass 1 routines
%************************************************************


SYMBOLIC PROCEDURE !&PASS1 EXP; %. Pass1- reform body of expression for
  !&PA1(EXP,NIL);		% Compilation

SYMBOLIC PROCEDURE PA1ERR(X);	%. Error messages from PASS1
 STDERROR LIST("-- PA1 --", X);
   
lisp procedure !&Pa1(U, Vbls);
    !&Pa1V(U, Vbls, NIL);

% Do the real pass1 and an extra reform

SYMBOLIC PROCEDURE !&PA1V(U,VBLS, VAR);
 BEGIN
  SCALAR Z,FN; % Z is the pass1 result.  Reform if necessary
  Z:=!&PA1X(U,VBLS, VAR);
  IF IDP CAR Z AND (FN:=GET(CAR Z,'PA1REFORMFN)) THEN
      Z := APPLY(FN,LIST Z);
  RETURN Z;
 END;

SYMBOLIC PROCEDURE !&PA1X(U,VBLS,VAR); 	%. VBLS are current local vars
   BEGIN SCALAR X; 
      RETURN IF ATOM U % tag variables and constants
               THEN IF ISAWCONST U THEN MKWCONST U
                     ELSE IF CONSTANTP U OR U MEMQ '(NIL T) THEN MKQUOTE U
                     ELSE IF NONLOCAL U THEN !&PANONLOCAL(U, VBLS)
                     ELSE IF U MEMQ VBLS THEN LIST('!$LOCAL,U)
                     ELSE <<MKNONLOCAL U; !&PANONLOCAL(U, VBLS) >>
              ELSE IF NOT IDP CAR U
               THEN IF EQCAR(CAR U,'LAMBDA) THEN
			!&PA1V(CAR U,VBLS,VAR) . !&PALISV(CDR U,VBLS,VAR)
		      ELSE		% Change to APPLY
		      <<  !&COMPERROR
		            list("Ill-formed function expression", U);
			 '(QUOTE NIL) >>
% Changed semantics of EVAL to conform to Common Lisp.
% CAR of a form is NEVER evaluated.
%              ELSE IF CAR U MEMQ VBLS OR FLUIDP CAR U
%			OR (GLOBALP CAR U
%				AND NOT GETD CAR U) THEN % Change to APPLY
%		      <<  !&COMPWARN list("Functional form converted to APPLY", U);
%			!&PA1(LIST('APPLY, CAR U, 'LIST . CDR U), VBLS) >>
              ELSE IF X := GET(CAR U,'PA1ALGFN) % Do const folding, etc.
	       THEN APPLY(X,LIST(U,VBLS,VAR))
              ELSE IF X := GET(CAR U,'PA1FN) % Do PA1FN's
	       THEN APPLY(X,LIST(U,VBLS))
              ELSE IF X := GET(CAR U,'CMACRO) % CMACRO substitution
               THEN !&PA1V(SUBLIS(PAIR(CADR X,CDR U),CADDR X),VBLS,VAR)
              ELSE IF (X := GETD CAR U) % Expand macros
                        AND CAR X EQ 'MACRO
                        AND NOT (GET(CAR U,'COMPFN) OR GET(CAR U,'OPENFN))
               THEN !&PA1V(APPLY(CDR X,LIST U),VBLS,VAR)
              ELSE IF !&CFNTYPE CAR U EQ 'FEXPR % Transform FEXPR calls to
                        AND NOT (GET(CAR U,'COMPFN) OR GET(CAR U,'OPENFN))
                THEN LIST(CAR U,MKQUOTE CDR U) % EXPR calls
              ELSE IF !&CFNTYPE CAR U EQ 'NEXPR % Transform NEXPR calls to
                        AND NOT (GET(CAR U,'COMPFN) OR GET(CAR U,'OPENFN))
                THEN LIST(CAR U,!&PA1V('LIST . CDR U,VBLS,VAR)) % EXPR calls
              ELSE CAR U . !&PALISV(CDR U,VBLS,VAR);
   END;

SYMBOLIC PROCEDURE !&PALIS(U,VBLS);
    !&PALISV(U,VBLS,NIL);

SYMBOLIC PROCEDURE !&PALISV(U,VBLS, VAR);
   FOR EACH X IN U COLLECT !&PA1V(X,VBLS,VAR);

SYMBOLIC PROCEDURE ISAWCONST X;		%. Check to see if WCONST, 
					%. in SYSLISP only
  !*SYSLISP AND WCONSTP X;

SYMBOLIC PROCEDURE !&CONSTTAG();
    IF !*SYSLISP THEN 'WCONST ELSE 'QUOTE;

SYMBOLIC PROCEDURE MKWCONST X;		%. Made into WCONST
BEGIN SCALAR Y;
  RETURN LIST('WCONST, IF (Y := GET(X, 'WCONST)) AND NOT GET(X, 'WARRAY)
						 AND NOT GET(X, 'WSTRING) THEN
			Y
		ELSE X);
END;

SYMBOLIC PROCEDURE !&PAWCONST(U, VBLS);
    MKWCONST CADR U;

SYMBOLIC PROCEDURE NONLOCAL X; 		%. Default NON-LOCAL types
 IF !*SYSLISP THEN NONLOCALSYS X
  ELSE NONLOCALLISP X;

SYMBOLIC PROCEDURE NONLOCALLISP X;
   IF FLUIDP X THEN '!$FLUID 
    ELSE IF GLOBALP X THEN '!$GLOBAL 
    ELSE IF WVARP X OR WARRAYP X THEN
	<<!&COMPWARN LIST(X,"already SYSLISP non-local");NIL>>
    ELSE NIL;

SYMBOLIC PROCEDURE NONLOCALSYS X;
   IF WARRAYP X THEN 'WARRAY
    ELSE IF WVARP X THEN 'WVAR
    ELSE NONLOCALLISP X;

SYMBOLIC PROCEDURE !&PANONLOCAL(X, VBLS);	%. Reform Non-locals
 % X will be a declared NONLOCAL
 BEGIN SCALAR Z;
  RETURN
  IF NOT IDP X OR NOT NONLOCAL X THEN PA1ERR LIST("non-local error",X)
  ELSE IF FLUIDP X THEN LIST('!$FLUID,X)
  ELSE IF GLOBALP X THEN LIST('!$GLOBAL,X)
  ELSE IF GET(X,'WVAR) THEN 
	IF X MEMBER VBLS THEN <<!&COMPWARN(LIST('WVAR,X,"used as local"));
				LIST('!$LOCAL,X)>>
	ELSE LIST('WVAR,X)
  ELSE IF WARRAYP X THEN 
	LIST('WCONST, X)
  ELSE PA1ERR LIST("Unknown in PANONLOCAL",X);
 END;

% Make unknown symbols into FLUID for LISP, WVAR for SYSLISP, with warning
% Changed to just declare it fluid, EB, 9:36am  Friday, 10 September 1982
SYMBOLIC PROCEDURE MKNONLOCAL U; 
%   IF !*SYSLISP THEN
%   <<  !&COMPERROR LIST("Undefined symbol", U,
%			"in Syslisp, treated as WVAR");
%	WDECLARE1(U, 'INTERNAL, 'WVAR, NIL, 0);
%	LIST('WVAR, U) >>
%   ELSE
 <<!&COMPWARN LIST(U,"declared fluid"); FLUID LIST U; LIST('!$FLUID,U)>>;


% Utility stuff for the PA1 functions

SYMBOLIC PROCEDURE !&MKNAM U; 
   %generates unique name for auxiliary function in U;
   IMPLODE NCONC(EXPLODE U,EXPLODE !&GENSYM());

% For making implied PROGN's into explicit ones (as in COND)
SYMBOLIC PROCEDURE !&MKPROGN U;
   IF NULL U OR CDR U THEN 'PROGN . U ELSE CAR U;


SYMBOLIC PROCEDURE !&EQP U; 
   %!&EQP is true if U is an object for which EQ can replace EQUAL;
   INUMP U OR IDP U;

SYMBOLIC PROCEDURE !&EQVP U; 
   %!&EQVP is true if EVAL U is an object for which EQ can
   %replace EQUAL;
   INUMP U OR NULL U OR U EQ 'T OR EQCAR(U,'QUOTE) AND !&EQP CADR U;

% !&EQPL U is true if !&EQP of all elements of U
SYMBOLIC PROCEDURE !&EQPL U;
NULL U OR !&EQP(CAR U) AND !&EQPL(CDR U);

SYMBOLIC PROCEDURE !&MAKEADDRESS U;
% convert an expression into an addressing expression, (MEMORY var const),
% where var is the variable part & const is the constant part (tagged, of
% course).  It is assumed that U has been through pass 1, which does constant
% folding & puts any constant term at the top level.
  IF EQCAR(U,'LOC) THEN CADR U ELSE	 % GETMEM LOC x == x
'MEMORY .
  (IF EQCAR(U,'WPLUS2) AND !&CONSTP CADDR U THEN CDR U
  ELSE IF EQCAR(U,'WDIFFERENCE) AND !&CONSTP CADR U THEN
	LIST(LIST('WMINUS,CADDR U),CADR U)
  ELSE LIST(U,'(WCONST 0)));

SYMBOLIC PROCEDURE !&DOOP U;
% simplification for random operators - op is doable only when all operands
% are constant
   IF !&ALLCONST CDR U THEN 
     LIST(CAR CADR U,
	  APPLY(GET(CAR U,'DOFN) or car U, FOR EACH X IN CDR U COLLECT CADR X))
    ELSE U;

SYMBOLIC PROCEDURE !&ALLCONST L;
    NULL L OR (car L = 'QUOTE or !&WCONSTP CAR L AND NUMBERP CADR CAR L)
	AND !&ALLCONST CDR L;

lisp procedure !&PaReformWTimes2 U;
begin scalar X;
    U := !&Doop U;
    return if first U = 'WTimes2 then
	if !&WConstP second U and (X := PowerOf2P second second U) then
	    list('WShift, third U, list(!&ConstTag(), X))
	else if !&WConstP third U and (X := PowerOf2P second third U) then
	    list('WShift, second U, list(!&ConstTag(), X))
	else U
    else U;
end;

SYMBOLIC PROCEDURE !&ASSOCOP(U,VBLS); % For abelian semi-groups & monoids
% given an associative, communitive operation (TIMES2, AND, ...) collect all
% arguments, seperate constant args, evaluate true constants, check for zero's
% and ones (0*X = 0, 1*X = X)
!&ASSOCOPV(U,VBLS,NIL);

SYMBOLIC PROCEDURE !&ASSOCOPV(U,VBLS,VAR);
  BEGIN SCALAR ARGS,NUM,CONSTS,VARS;
    ARGS := !&ASSOCOP1(CAR U,!&PALIS(CDR U,VBLS));
    CONSTS := VARS := NUM := NIL;
    FOR EACH ARG IN ARGS DO
     IF !&WCONSTP ARG THEN
	IF NUMBERP CADR ARG THEN
	    IF NUM THEN NUM := APPLY(GET(CAR U,'DOFN),LIST(NUM,CADR ARG))
	    ELSE NUM := CADR ARG
	ELSE CONSTS := NCONC(CONSTS,LIST ARG)
     ELSE VARS := NCONC(VARS,LIST ARG);
    IF NUM THEN
	<<IF NUM = GET(CAR U,'ZERO) THEN RETURN LIST(!&CONSTTAG(),NUM);
	  IF NUM NEQ GET(CAR U,'ONE) THEN CONSTS := NUM . CONSTS
	  ELSE IF NULL VARS AND NULL CONSTS THEN RETURN
		LIST(!&CONSTTAG(), NUM) >>;
    IF CONSTS THEN
	 VARS := NCONC(VARS,LIST LIST('WCONST,!&INSOP(CAR U,CONSTS)));
    IF VAR MEMBER VARS THEN
      <<VARS := DELETIP(VAR,VARS);
        RETURN !&INSOP(CAR U,REVERSIP(VAR . REVERSIP VARS))>>;
    RETURN !&INSOP(CAR U,VARS);
   END;

SYMBOLIC PROCEDURE !&ASSOCOP1(OP,ARGS);
  IF NULL ARGS THEN NIL 
     ELSE NCONC(!&ASSOCOP2(OP,CAR ARGS),!&ASSOCOP1(OP,CDR ARGS));

SYMBOLIC PROCEDURE !&ASSOCOP2(OP,ARG);
  IF EQCAR(ARG,OP) THEN !&ASSOCOP1(OP,CDR ARG)
   ELSE LIST ARG;

SYMBOLIC PROCEDURE !&INSOP(OP,L);
% Insert OP into a list of operands as follows: INSOP(~,'(A B C D)) =
% (~ (~ (~ A B) C) D)
 IF NULL L THEN NIL ELSE if null cdr L then car L else
    !&INSOP1(list(OP, first L, second L), rest rest L, OP);

SYMBOLIC PROCEDURE !&INSOP1(NEW, RL, OP);
 if null RL then NEW else !&INSOP1(list(OP, NEW, first RL), rest RL, OP);

SYMBOLIC PROCEDURE !&GROUP(U,VBLS);
% Like ASSOP, except inverses exist.  All operands are partitioned into two
% lists, non-inverted and inverted.  Cancellation is done between these two
% lists.  The group is defined by three operations, the group operation (+),
% inversion (unary -), and subtraction (dyadic -).  The GROUPOPS property on
% all three of there operators must contain the names of these operators in
% the order (add subtract minus)
!&GROUPV(U,VBLS,NIL);

SYMBOLIC PROCEDURE !&GROUPV(U,VBLS,VAR);
 BEGIN SCALAR X,ARGS,INVARGS,FNS,CONSTS,INVCONSTS,CON,RES,VFLG,INVFLG,ONE;
  FNS := GET(CAR U,'GROUPOPS);
  ONE := LIST(!&CONSTTAG(),GET(CAR FNS,'ONE));
  X := !&GETGROUPARGS(FNS,CAR U . !&PALIS(CDR U, VBLS),NIL,'(NIL NIL));
  ARGS := CAR X;
  INVARGS := CADR X;
  FOR EACH ARG IN ARGS DO
    IF ARG MEMBER INVARGS THEN 
      <<ARGS := !&DELARG(ARG,ARGS);
	INVARGS := !&DELARG(ARG,INVARGS)>>;
  CONSTS := INVCONSTS := CON := NIL;
  FOR EACH ARG IN ARGS DO
   IF !&WCONSTP ARG THEN
     <<ARGS := !&DELARG(ARG,ARGS);
       IF NUMBERP CADR ARG THEN
 	  IF CON THEN CON := APPLY(GET(CAR FNS,'DOFN),LIST(CON,CADR ARG))
	         ELSE CON := CADR ARG
       ELSE  CONSTS := NCONC(CONSTS,LIST ARG)>>;
  FOR EACH ARG IN INVARGS DO
   IF !&WCONSTP ARG THEN
     <<INVARGS := !&DELARG(ARG,INVARGS);
       IF NUMBERP CADR ARG THEN
 	  IF CON THEN CON := APPLY(GET(CADR FNS,'DOFN),LIST(CON,CADR ARG))
	         ELSE CON := APPLY(GET(CADDR FNS,'DOFN),LIST CADR ARG)
       ELSE  INVCONSTS := NCONC(INVCONSTS,LIST ARG)>>;
  IF CON AND CON = GET(CAR FNS,'ZERO) THEN RETURN LIST(!&CONSTTAG(),CON);
  IF CON AND CON = CADR ONE THEN CON := NIL;
  IF CON THEN CONSTS := CON . CONSTS;
  CONSTS := !&MAKEXP(CONSTS,INVCONSTS,FNS);
  IF CONSTS AND NOT !&WCONSTP CONSTS THEN CONSTS := LIST('WCONST,CONSTS);
  IF VAR MEMBER ARGS THEN
    <<ARGS := DELETE(VAR,ARGS);
      VFLG := T;
      INVFLG := NIL>>;
  IF VAR MEMBER INVARGS THEN
    <<INVARGS := DELETE(VAR,INVARGS);
      VFLG := T;
      INVFLG := T>>;
  ARGS := !&MAKEXP(ARGS,INVARGS,FNS);
  RES := IF NULL ARGS THEN
	    IF NULL CONSTS THEN
		ONE
	    ELSE CONSTS
	  ELSE
	    IF NULL CONSTS THEN ARGS
	    ELSE IF EQCAR(ARGS,CADDR FNS) THEN
	     LIST(CADR FNS,CONSTS,CADR ARGS)
	  ELSE 
	     LIST(CAR FNS,ARGS,CONSTS);
  IF VFLG THEN
    IF RES = ONE THEN
      IF INVFLG THEN RES := LIST(CADDR FNS,VAR)
 		ELSE RES := VAR
    ELSE
      RES := LIST(IF INVFLG THEN CADR FNS ELSE CAR FNS,RES,VAR);
  RETURN RES;
 END;

SYMBOLIC PROCEDURE !&MAKEXP(ARGS,INVARGS,FNS);
 IF NULL ARGS THEN
   IF NULL INVARGS THEN NIL
   ELSE LIST(CADDR FNS,!&INSOP(CAR FNS,INVARGS))
 ELSE
   IF NULL INVARGS THEN !&INSOP(CAR FNS,ARGS)
   ELSE !&INSOP(CADR FNS,!&INSOP(CAR FNS,ARGS) . INVARGS);

SYMBOLIC PROCEDURE !&GETGROUPARGS(FNS,EXP,INVFLG,RES);
 IF ATOM EXP OR NOT(CAR EXP MEMBER FNS) THEN
    !&GETGROUPARGS1(EXP,INVFLG,RES)
 ELSE IF CAR EXP EQ CAR FNS THEN !&GETGROUPARGS2(FNS,CDR EXP,INVFLG,RES)
 ELSE IF CAR EXP EQ CADR FNS THEN
   !&GETGROUPARGS(FNS,CADR EXP,INVFLG,
		  !&GETGROUPARGS(FNS,CADDR EXP,NOT INVFLG,RES))
 ELSE IF CAR EXP EQ CADDR FNS THEN
    !&GETGROUPARGS(FNS,CADR EXP,NOT INVFLG,RES)
 ELSE !&COMPERROR(LIST("Compiler bug in constant folding",FNS,EXP));

SYMBOLIC PROCEDURE !&GETGROUPARGS1(THING,INVFLG,RES);
 IF INVFLG THEN LIST(CAR RES,THING . CADR RES)
 ELSE (THING . CAR RES) . CDR RES;

SYMBOLIC PROCEDURE !&GETGROUPARGS2(FNS,ARGS,INVFLG,RES);
 IF NULL ARGS THEN RES 
 ELSE !&GETGROUPARGS2(FNS,CDR ARGS,INVFLG,
		      !&GETGROUPARGS(FNS,CAR ARGS,INVFLG,RES));

SYMBOLIC PROCEDURE !&DELARG(ARG,ARGS);
  IF ARG = CAR ARGS THEN CDR ARGS ELSE CAR ARGS . !&DELARG(ARG,CDR ARGS);

%************************************************************
%         Pass 1 functions
%************************************************************

lisp procedure !&PaApply(U, Vars);
    if EqCar(third U, 'LIST) then	% set up for !&COMAPPLY
	if EqCar(second U, 'function)
		and !&CfnType second second U = 'EXPR then
	    !&Pa1(second second U . rest third U, Vars)
	else list('APPLY,
		  !&Pa1(second U, Vars),
		  'LIST . !&PaLis(rest third U, Vars))
    else 'APPLY . !&PaLis(rest U, Vars);

% Try to turn ASSOC into ATSOC
SYMBOLIC PROCEDURE !&PAASSOC(U,VARS); 
  !&PAASSOC1(CADR U,CADDR U) . !&PALIS(CDR U,VARS);

SYMBOLIC PROCEDURE !&PAASSOC1(ASSOCVAR,ASSOCLIST);
       IF !&EQVP ASSOCVAR 
	  OR EQCAR(ASSOCLIST,'QUOTE) AND 
            !&EQPL(FOR EACH U IN CADR ASSOCLIST COLLECT CAR U)
       THEN 'ATSOC ELSE 'ASSOC;

SYMBOLIC PROCEDURE !&PACOND(U,VBLS);
begin scalar RevU, Result, Temp;
    if null cdr U then return '(QUOTE NIL);	% (COND) == NIL
    RevU := reverse cdr U;
    if first first RevU neq T then RevU := '(T NIL) . RevU;
    for each CondForm in RevU do
	if null rest CondForm then
	<<  if not Temp then
	    <<  Temp := !&Gensym();
		VBLS := Temp . VBLS >>;
	    Result := list(!&PA1(list('SETQ, Temp, first CondForm), VBLS),
			   !&PA1(Temp, VBLS)) . Result >>
	else
	    Result := list(!&PA1(first CondForm, VBLS),
			   !&PA1(!&MkProgN rest CondForm, VBLS)) . Result;
    return if Temp then list(list('LAMBDA,
				  list !&PA1(Temp, VBLS),
				  'COND . Result),
			     '(QUOTE NIL))
    else 'COND . Result;
end;

lisp procedure !&PaCatch(U, Vbls);
(lambda(Tag, Forms);
<<  if null cdr Forms and
	(atom car Forms
	     or car car Forms = 'QUOTE
	     or car car Forms = 'LIST) then
	!&CompWarn list("Probable obsolete use of CATCH:", U);
    !&Pa1(list(list('lambda, '(!&!&HiddenVar!&!&),
			list('cond, list('(null ThrowSignal!*),
					  list('(lambda (xxx)
					         (!%UnCatch !&!&HiddenVar!&!&)
						      xxx),
					       'progn . Forms)),
				    '(t !&!&HiddenVar!&!&))),
		    list('CatchSetup, Tag)),
	  Vbls)>>)(cadr U, cddr U);

% X-1 -> SUB1 X
SYMBOLIC PROCEDURE !&PADIFF(U,VARS); 
   IF CADDR U=1 THEN LIST('SUB1,!&PA1(CADR U,VARS))
    ELSE 'DIFFERENCE . !&PALIS(CDR U,VARS);


SYMBOLIC PROCEDURE !&PAEQUAL(U,VARS); 
  !&PAEQUAL1(CADR U,CADDR U) . !&PALIS(CDR U,VARS);

SYMBOLIC PROCEDURE !&PAEQUAL1(LEFT,RIGHT);
    IF !&EQVP LEFT OR !&EQVP RIGHT THEN 'EQ
        ELSE IF NUMBERP LEFT OR NUMBERP RIGHT THEN 'EQN
        ELSE 'EQUAL;

% FUNCTION will compile a non-atomic arg into a GENSYMed name.
% Currently, MKFUNC = MKQUOTE

SYMBOLIC PROCEDURE !&PAFUNCTION(U,VBLS);
  IF ATOM CADR U THEN !&MKFUNC CADR U	% COMPD returns a code pointer here
                     ELSE !&MKFUNC COMPD('!*!*!*Code!*!*Pointer!*!*!*,
					'EXPR,CADR U);

SYMBOLIC PROCEDURE !&PAGETMEM(U,VBLS);
 !&MAKEADDRESS !&PA1(CADR U,VBLS);

SYMBOLIC PROCEDURE !&PAIDENT(U,VBLS);	%. return form
  U;

% LAMBDA - pick up new vars, check implicit PROGN

SYMBOLIC PROCEDURE !&PACASE(U,VBLS);
  'CASE . !&PA1(CADR U,VBLS) . FOR EACH EXP IN CDDR U COLLECT
   LIST(!&PALIS(CAR EXP,VBLS),!&PA1(CADR EXP,VBLS));

SYMBOLIC PROCEDURE !&PALAMBDA(U,VBLS);
   <<VBLS := APPEND(CADR U,VBLS);
     'LAMBDA   . LIST(!&PALIS(CADR U,VBLS),!&PA1(!&MKPROGN CDDR U,VBLS)) >>;

% X<0 -> MINUSP(X)

SYMBOLIC PROCEDURE !&PALESSP(U,VARS); 
   IF CADDR U=0 THEN LIST('MINUSP,!&PA1(CADR U,VARS))
    ELSE 'LESSP . !&PALIS(CDR U,VARS);

SYMBOLIC PROCEDURE !&PALIST(U, VBLS);
 BEGIN SCALAR L,FN;
  L := LENGTH CDR U;
  RETURN
    IF L = 0 THEN '(QUOTE NIL)
    ELSE IF FN := ASSOC(L,'((1 . NCONS)
			    (2 . LIST2)
			    (3 . LIST3)
			    (4 . LIST4)
			    (5 . LIST5)))
	 THEN !&PA1(CDR FN . CDR U, VBLS)
     ELSE !&PA1(LIST('CONS,CADR U, 'LIST . CDDR U), VBLS);
 END;

lisp procedure !&PaNth(U, Vbls);
    !&PaNths(U, Vbls, '((1 . CAR) (2 . CADR) (3 . CADDR) (4 . CADDDR)));

lisp procedure !&PaPNth(U, Vbls);
    !&PaNths(U, Vbls, '((1 . CR)
			(2 . CDR)
			(3 . CDDR)
			(4 . CDDDR)
			(5 . CDDDDR)));

lisp procedure !&PaNths(U, Vbls, FnTable);
begin scalar N, X, Fn;
    N := !&Pa1(third U, Vbls);
    X := second U;
    return if first N memq '(QUOTE WCONST) and FixP second N
	and (Fn := Assoc(second N, FnTable)) then
	    if cdr Fn = 'CR then
		!&Pa1(X, Vbls)
	    else !&Pa1(list(cdr Fn, X), Vbls)
    else list(car U, !&Pa1(X, Vbls), N);
end;

SYMBOLIC PROCEDURE !&PAMAP(U, VBLS);
  !&PAMAPDO(U, VBLS, NIL);

SYMBOLIC PROCEDURE !&PAMAPC(U, VBLS);
  !&PAMAPDO(U, VBLS, T);

SYMBOLIC PROCEDURE !&PAMAPDO(U, VBLS, CARFLAG);
  IF NOT EQCAR(CADDR U,'FUNCTION) THEN CAR U . !&PALIS(CDR U,VBLS)
  ELSE BEGIN SCALAR TMP;
	TMP := !&GENSYM();
	RETURN !&PA1(SUBLA(LIST('TMP . TMP,
				'STARTINGLIST . CADR U,
				'FNCALL . LIST(CADR CADDR U,
					       IF CARFLAG THEN
					       LIST('CAR, TMP)
					      ELSE TMP)),
			   '(PROG (TMP)
			      (SETQ TMP STARTINGLIST)
			    LOOPLABEL
			      (COND ((ATOM TMP) (RETURN NIL)))
			      FNCALL
			      (SETQ TMP (CDR TMP))
			      (GO LOOPLABEL))), VBLS);
  END;

SYMBOLIC PROCEDURE !&PAMAPLIST(U, VBLS);
  !&PAMAPCOLLECT(U, VBLS, NIL);

SYMBOLIC PROCEDURE !&PAMAPCAR(U, VBLS);
  !&PAMAPCOLLECT(U, VBLS, T);

SYMBOLIC PROCEDURE !&PAMAPCOLLECT(U, VBLS, CARFLAG);
  IF NOT EQCAR(CADDR U,'FUNCTION) THEN CAR U . !&PALIS(CDR U,VBLS)
  ELSE BEGIN SCALAR TMP, RESULT, ENDPTR;
    TMP := !&GENSYM();
    RESULT := !&GENSYM();
    ENDPTR := !&GENSYM();
    RETURN !&PA1(SUBLA(LIST('TMP . TMP,
			    'RESULT . RESULT,
			    'ENDPTR . ENDPTR,
			    'STARTINGLIST . CADR U,
			    'FNCALL . LIST(CADR CADDR U,
					   IF CARFLAG THEN
						LIST('CAR, TMP)
					   ELSE TMP)),
		      '(PROG (TMP RESULT ENDPTR)
			 (SETQ TMP STARTINGLIST)
			 (COND ((ATOM TMP) (RETURN NIL)))
			 (SETQ RESULT (SETQ ENDPTR (NCONS FNCALL)))
		       LOOPLABEL
			 (SETQ TMP (CDR TMP))
			 (COND ((ATOM TMP) (RETURN RESULT)))
			 (RPLACD ENDPTR (NCONS FNCALL))
			 (SETQ ENDPTR (CDR ENDPTR))
			 (GO LOOPLABEL))), VBLS);
  END;

SYMBOLIC PROCEDURE !&PAMAPCON(U, VBLS);
  !&PAMAPCONC(U, VBLS, NIL);

SYMBOLIC PROCEDURE !&PAMAPCAN(U, VBLS);
  !&PAMAPCONC(U, VBLS, T);

SYMBOLIC PROCEDURE !&PAMAPCONC(U, VBLS, CARFLAG);
  IF NOT EQCAR(CADDR U,'FUNCTION) THEN CAR U . !&PALIS(CDR U,VBLS)
  ELSE BEGIN SCALAR TMP, RESULT, ENDPTR;
    TMP := !&GENSYM();
    RESULT := !&GENSYM();
    ENDPTR := !&GENSYM();
    RETURN !&PA1(SUBLA(LIST('TMP . TMP,
			    'RESULT . RESULT,
			    'ENDPTR . ENDPTR,
			    'STARTINGLIST . CADR U,
			    'FNCALL . LIST(CADR CADDR U,
					   IF CARFLAG THEN
						LIST('CAR, TMP)
					   ELSE TMP)),
		      '(PROG (TMP RESULT ENDPTR)
			 (SETQ TMP STARTINGLIST)
		      STARTOVER
			 (COND ((ATOM TMP) (RETURN NIL)))
			 (SETQ RESULT FNCALL)
			 (SETQ ENDPTR (LASTPAIR RESULT))
			 (SETQ TMP (CDR TMP))
			 (COND ((ATOM ENDPTR) (GO STARTOVER)))
		       LOOPLABEL
			 (COND ((ATOM TMP) (RETURN RESULT)))
			 (RPLACD ENDPTR FNCALL)
			 (SETQ ENDPTR (LASTPAIR ENDPTR))
			 (SETQ TMP (CDR TMP))
			 (GO LOOPLABEL))), VBLS);
  END;

% Attempt to change MEMBER to MEMQ

SYMBOLIC PROCEDURE !&PAMEMBER(U,VARS); 
   !&PAMEMBER1(CADR U,CADDR U) . !&PALIS(CDR U,VARS);

SYMBOLIC PROCEDURE !&PAMEMBER1(THING,LST);
  IF !&EQVP THING OR EQCAR(LST,'QUOTE) AND !&EQPL CADR LST
   THEN 'MEMQ ELSE 'MEMBER;

% (Intern (Compress X)) == (Implode X)
% (Intern (Gensym)) == (InternGensym)

SYMBOLIC PROCEDURE !&PAINTERN(U, VBLS);
<<  U := !&PA1(CADR U, VBLS);
    IF EQCAR(U, 'COMPRESS) THEN 'IMPLODE . CDR U
    ELSE IF EQCAR(U, 'GENSYM) THEN 'INTERNGENSYM . CDR U
    ELSE LIST('INTERN, U) >>;

% Do MINUS on constants.

SYMBOLIC PROCEDURE !&PAMINUS(U,VBLS); 
   IF EQCAR(U := !&PA1(CADR U,VBLS),'QUOTE) AND NUMBERP CADR U
     THEN MKQUOTE ( - CADR U)
   ELSE IF EQCAR(U ,'WCONST) AND NUMBERP CADR U
     THEN MKWCONST ( - CADR U)
    ELSE LIST('MINUS,U);

SYMBOLIC PROCEDURE !&REFORMLOC U;
    IF EQCAR(CADR U, 'MEMORY) THEN
	LIST('WPLUS2, CADDR CADR U, CADR CADR U)
    ELSE U;

SYMBOLIC PROCEDURE !&REFORMNULL U;
 BEGIN SCALAR FLIP;
  RETURN
	  IF PAIRP CADR U AND (FLIP := GET(CAADR U,'FLIPTST)) THEN
	    FLIP . CDADR U
	  ELSE LIST('EQ, CADR U, '(QUOTE NIL));
 END;

% Perdue 12/3/82
% This optimization causes compiled code to behave differently
% from interpreted code.  The FLIPTST property on NE and PASS2
% handling of negation in tests (&COMTST) are enough to cause good code
% to be generated when NE is used as a test.

% SYMBOLIC PROCEDURE !&REFORMNE U;
%     IF CADR U = '(QUOTE NIL) THEN CADDR U
%     ELSE IF CADDR U = '(QUOTE NIL) THEN CADR U
%     ELSE U;

% PLUS2(X,1) -> ADD1(X)

SYMBOLIC PROCEDURE !&PAPLUS2(U,VARS); 
   IF CADDR U=1 THEN !&PA1(LIST('ADD1, CADR U),VARS)
    ELSE IF CADR U=1 THEN !&PA1('ADD1 . CDDR U,VARS)
    ELSE 'PLUS2 . !&PALIS(CDR U,VARS);

% Pick up PROG vars, ignore labels.

SYMBOLIC PROCEDURE !&PAPROG(U,VBLS);
   <<VBLS := APPEND(CADR U,VBLS);
     'PROG . (!&PALIS(CADR U,VBLS) . !&PAPROGBOD(CDDR U,VBLS)) >>;

SYMBOLIC PROCEDURE !&PAPROGBOD(U,VBLS); 
   FOR EACH X IN U COLLECT IF ATOM X THEN X ELSE !&PA1(X,VBLS);

SYMBOLIC PROCEDURE !&PAPUTMEM(U,VBLS);
  !&PA1('SETQ . LIST('GETMEM, CADR U) . CDDR U, VBLS);

SYMBOLIC PROCEDURE !&PAPUTLISPVAR(U, VBLS);
  !&PA1('SETQ . LIST('LISPVAR, CADR U) . CDDR U, VBLS);

SYMBOLIC PROCEDURE !&PALISPVAR(U, VBLS);
  LIST('!$FLUID, CADR U);

SYMBOLIC PROCEDURE !&PASETQ(U,VBLS);
 BEGIN SCALAR VAR,FN,EXP, LN;
 LN := LENGTH CDR U;
 IF LN NEQ 2 THEN RETURN
 <<  LN := DIVIDE(LN, 2);
     IF CDR LN NEQ 0 THEN
     <<  !&COMPERROR LIST("Odd number of arguments to SETQ", U);
	 U := APPEND(U, LIST NIL);
	 LN := CAR LN + 1 >>
    ELSE LN := CAR LN;
    U := CDR U;
    FOR I := 1 STEP 1 UNTIL LN DO
    <<  EXP := LIST('SETQ, CAR U, CADR U) . EXP;
	U := CDDR U >>;
    !&PA1('PROGN . REVERSIP EXP, VBLS) >>;
 VAR := !&PA1(CADR U,VBLS);
 EXP := !&PA1V(CADDR U, VBLS, VAR);
 U := IF FLAGP(CAR VAR,'VAR) THEN LIST('!$NAME,VAR) ELSE VAR;
 IF (NOT (FN := GET(CAR EXP,'MEMMODFN))) OR not (LastCar EXP = VAR) THEN
 	RETURN LIST('SETQ,U,EXP)
 ELSE RETURN FN . U . REVERSIP CDR REVERSIP CDR EXP;
END;

SYMBOLIC PROCEDURE !&INSTALLDESTROY(NAME!&);
% determine which (if any) registers are unaltered by the function.
% Print this information out if !*SHOWDEST, install it on the
% property list of the function if !*INSTALLDESTOY
  BEGIN SCALAR DESTL,R,HRU;
   HRU := !&HIGHEST(CODELIST!&,NIL,NARG!&,T);
% Find the highest register used in the code. Registers above this are
% unchanged.  Incoming registers have a distinguished value, IREG n, placed
% in register n.  If this value remains, it has not been destroyed.
   IF HRU = 'ALL THEN RETURN NIL;
   DESTL := NIL;
   FOR I := 1:NARG!& DO 
    <<R := !&MKREG I;
      IF NOT (!&IREG I MEMBER !&REGVAL R) THEN DESTL := R . DESTL>>;
   FOR I := NARG!&+1 : HRU DO
      DESTL := !&MKREG I . DESTL;
   IF NULL DESTL THEN DESTL := '((REG 1));
   IF !*INSTALLDESTROY THEN PUT(NAME!&,'DESTROYS,DESTL);
       IF !*SHOWDEST THEN <<PRIN2 NAME!&;PRIN2 " DESTROYS ";PRIN2T DESTL>>;
  END;


% COMPROC does the dirty work - initializes variables and gets the 
% three passes going.
SYMBOLIC PROCEDURE !&COMPROC(EXP,NAME!&); 
   %compiles a function body, returning the generated LAP;
   BEGIN SCALAR CODELIST!&,FLAGG!&,JMPLIST!&,LBLIST!&,
		LOCALGENSYM!&,
                LLNGTH!&,REGS!&,REGS1!&,ALSTS!&,
		EXITT!&,TOPLAB!&,SLST!&,STOMAP!&,
                CONDTAIL!&,FREEBOUND!&,HOLEMAP!&,PREGS!&,
                SWITCH!&,EXITREGS!&,RN; INTEGER NARG!&; 
      LOCALGENSYM!& := GLOBALGENSYM!&;
      PREGS!& := NIL;
      REGS!& := NIL;
      LLNGTH!& := 0; 
      IF NOT EQCAR(EXP, 'LAMBDA) THEN
      <<  !&COMPERROR LIST("Attempt to compile a non-lambda expression", EXP);
	  RETURN NIL >>;
      NARG!& := LENGTH CADR EXP; 
      EXITREGS!& := NIL;
      EXITT!& := !&GENLBL(); 
      TOPLAB!& := !&GENLBL();
      STOMAP!& := NIL;
      CODELIST!& := LIST '(!*ALLOC (!*FRAMESIZE));
      !&ATTLBL TOPLAB!&;
      EXP := !&PASS1 EXP; 
      IF NARG!& > MAXNARGS!&
	THEN !&COMPERROR LIST("Too many arguments",NARG!&);
      ALSTS!& := !&VARBIND(CADR EXP,T); % Generate LAMBIND
      RN := 1;
      FOR I := 1:LENGTH CADR EXP DO
 	REGS!& := !&ADDRVALS(!&MKREG I,REGS!&,LIST( !&IREG I));
      !&PASS2 CADDR EXP; 
      !&FREERSTR(ALSTS!&,0); %Restores old fluid bindings
      !&PASS3(); 
      IF !*INSTALLDESTROY OR !*SHOWDEST THEN !&INSTALLDESTROY(NAME!&);
      !&REFORMMACROS(); % Plugs compile time constants into macros. FIXFRM?
      !&REMTAGS(); % Kludge
      RETURN CODELIST!&
   END;

lisp procedure !&IReg N;
    if N > 0 and N <= 15 then
	GetV('[() (IREG 1) (IREG 2) (IREG 3) (IREG 4) (IREG 5)
	       (IREG 6) (IREG 7) (IREG 8) (IREG 9) (IREG 10)
	       (IREG 11) (IREG 12) (IREG 13) (IREG 14) (IREG 15)], n)
    else list('IREG, N);

SYMBOLIC PROCEDURE !&WCONSTP X;
    PairP X and (first X = 'WConst or first X = 'Quote and FixP second X);

%************************************************************
%       Pass 2						    *
%************************************************************

% Initialize STATUS!&=0  (Top level)

SYMBOLIC PROCEDURE !&PASS2 EXP; !&COMVAL(EXP,0);

SYMBOLIC PROCEDURE !&COMVAL(EXP,STATUS!&); 
% Compile EXP.  Special cases: if STATUS!&>1 (compiling for side effects),
% anyreg functions are ignored since they have no side effects.
% Otherwise, top level ANYREG stuff is factored out and done via a LOAD
% instead of a LINK.
   IF !&ANYREG(EXP)
     THEN IF STATUS!&>1 THEN
	<<IF NOT (CAR EXP MEMBER '(QUOTE !$LOCAL !$FLUID)) THEN
	      !&COMPWARN(LIST("Value of",
			      EXP,
			      "not used, therefore not compiled"));
	  NIL >>
      ELSE !&LREG1(EXP) % Just a LOAD
   ELSE  % When not all ANYREG
     IF !&ANYREGFNP EXP % Is the top level an ANYREG fn?
        THEN IF STATUS!&>1 THEN
	  <<!&COMVAL(CADR EXP,STATUS!&);
	    !&COMPWARN LIST("Top level", CAR EXP,
			    "in", EXP, "not used, therefore not compiled");
	    NIL>>
	ELSE
          !&LREG1(CAR EXP . !&COMLIS CDR EXP) % Preserve the anyreg fn
     ELSE !&COMVAL1(EXP,STOMAP!&,STATUS!&); % no anyregs in sight

% Generate code which loads the value of EXP into register 1

% Patch to COMVAL1 for better register allocation

SYMBOLIC PROCEDURE !&COMVAL1(EXP,STOMAP!&,STATUS!&); 
   BEGIN SCALAR X; 
      IF !&ANYREG EXP OR !&OPENFNP EXP OR !&ANYREGFNP EXP THEN
        IF STATUS!&<2 AND !&NOSIDEEFFECTP EXP 
            THEN !&COMPWARN(LIST(EXP," not compiled"))
            ELSE <<!&LOADOPENEXP(IF STATUS!& > 1 THEN !&AllocTemp(Exp)
						 ELSE '(REG 1),
			         CAR EXP . !&COMLIS CDR EXP,STATUS!&,PREGS!&)>>
       ELSE IF NOT ATOM CAR EXP % Non atomic function?
        THEN IF CAAR EXP EQ 'LAMBDA
               THEN !&COMPLY(CAR EXP,CDR EXP,STATUS!&) % LAMBDA compilation
              ELSE !&COMPERROR LIST(CAR EXP, "Invalid as function")
					%  Should be noticed in pass 1
       ELSE IF X := GET(CAR EXP,'COMPFN) THEN APPLY(X,LIST(EXP,STATUS!&))
		% Dispatch built in compiler functions
       ELSE IF CAR EXP EQ 'LAMBDA
	THEN !&COMPERROR LIST("Invalid use of LAMBDA in COMVAL1",EXP)
       ELSE !&CALL(CAR EXP,CDR EXP,STATUS!&); % Call a function
      RETURN NIL
   END;

% Procedure to allocate temps for OPEN exprs.  Used only when STATUS!&<1 to
% set up destination.  Only special case is SETQ.  SETQ tries to put the
% value of X:=... into a register containing X (keeps variables in the same
% register if possible.

Symbolic Procedure !&Alloctemp(Exp);
 if car Exp = 'Setq then
  if car caddr exp = 'Setq then     % Nested setq - move to actual RHS
    !&Alloctemp(caddr Exp)
  else
    begin
      Scalar Reg;
      If (Reg := !&RAssoc(Cadr Cadr Exp,Regs!&)) % LHS variable already in reg?
	 and not (Car Reg member PRegs!&) then % and reg must be available
         Return Car Reg % Return the reg previously used for the var
      else
         Return !&Tempreg() % Just get a temp
    end
 else !&TempReg(); % not SETQ - any old temp will do


SYMBOLIC PROCEDURE !&CALL(FN,ARGS,STATUS!&); 
   !&CALL1(FN,!&COMLIS1 ARGS,STATUS!&);

%Args have been compiled

SYMBOLIC PROCEDURE !&CALL1(FN,ARGS,STATUS!&); 
   %ARGS is reversed list of compiled arguments of FN;
   BEGIN INTEGER ARGNO; 
      SCALAR DEST!&;
      ARGNO := LENGTH ARGS; 
      IF !&ANYREGP FN THEN !&LREG1(FN . ARGS)
      ELSE <<!&LOADARGS(ARGS,1,PREGS!&); %Emits loads to registers
             !&ATTACH LIST('!*LINK,FN,!&CFNTYPE FN,ARGNO); 
             !&REMMREFS();
	     !&REMVREFS();
% Default - all registers destroyed
             IF !*USINGDESTROY THEN DEST!& := GET(FN,'DESTROYS);
             IF NULL DEST!& THEN REGS!& := NIL
              ELSE
                 BEGIN SCALAR TEMP;
                  TEMP := NIL;
                  FOR EACH R IN REGS!& DO
                    IF NOT(CAR R MEMBER DEST!&) THEN TEMP := R . TEMP;
                  REGS!& := TEMP
                 END >>
   END;

% Comlis altered to return unreversed list

SYMBOLIC PROCEDURE !&COMLIS EXP; REVERSIP !&COMLIS1 EXP;
 
% COMLIS1 returns reversed list of compiled arguments;

SYMBOLIC PROCEDURE !&COMLIS1 EXP; 
   BEGIN SCALAR ACUSED,Y; % Y gathers a set of ANYREG expressions denoting
% the params.  Code for non ANYREG stuff is emitted by ATTACH.  ACUSED is
% name of psuedo variable holding results of non anyreg stuff.
      Y := NIL;
      WHILE EXP DO
         <<IF !&CONSTP CAR EXP OR
              !&OPENP CAR EXP
                AND (NOT !*ORD OR !&NOSIDEEFFECTPL CDR EXP)
	    THEN Y := CAR EXP . Y
% Anyreg stuff is handled later.  Anyreg args are not loaded until after
% all others.
% If !*ORD is true, order is still switched unless no side effects
            ELSE <<
			%/  Special coding for top level ANYREG
		    IF ACUSED THEN !&SAVER1();
                    IF (!&ANYREGFNP CAR EXP OR !&OPENFNP CAR EXP)
                      AND (NOT !*ORD OR !&NOSIDEEFFECTPL CDR EXP) THEN
                       <<Y := (CAAR EXP . !&COMLIS CDAR EXP) . Y;
                         ACUSED := T>>
% Emit code to place arg in R1, generate a name for the result to put in R1
                       ELSE <<!&COMVAL1(CAR EXP,STOMAP!&,1); 	
		   ACUSED := LIST('!$LOCAL,!&GENSYM()); 
                   REGS!& := !&ADDRVALS('(REG 1),REGS!&,LIST ACUSED);
% REGS!& the new variable name goes on the code list (rest already emitted)
                   Y := ACUSED . Y>>>>;
% place arg in memory while doing others
           EXP := CDR EXP>>; 
      RETURN Y
   END;

% SAVE R1 IF NECESSARY

SYMBOLIC PROCEDURE !&SAVER1; %MARKS CONTENTS OF REGISTER 1 FOR STORAGE;
   BEGIN SCALAR X; 
      X := !&REGVAL '(REG 1); % Contents of R1 
      IF NULL X OR NOT !&VARP CAR X
	THEN RETURN NIL % Dont save constants
       ELSE IF NOT ASSOC(CAR X,STOMAP!&) THEN !&FRAME CAR X; % For temporaries
				% as generated in COMLIS
      !&STORELOCAL(CAR X,'(REG 1)) % Emit a store
   END;

% Compiler for LAMBDA

SYMBOLIC PROCEDURE !&COMPLY(FN,ARGS,STATUS!&); 
   BEGIN SCALAR ALSTS!&,VARS, N, I;
         %SCALAR OLDSTOMAP,OLDCODE;
%      OLDSTOMAP := STOMAP!&;
%      OLDCODE := CODELIST!&;
      VARS := CADR FN; 
% Compile args to the lambda
      ARGS := !&COMLIS1 ARGS; 
      N := LENGTH ARGS; 
      IF N>MAXNARGS!& THEN 
	!&COMPERROR LIST("Too many arguments in LAMBDA form",FN);
% Put the args into registers
      !&LOADARGS(ARGS,1,PREGS!&); 
% Enter new ENVIRONMENT!&
      ARGS := !&REMVARL VARS; % The stores that were protected;
      I := 1; 
% Put this junk on the frame
      ALSTS!& := !&VARBIND(VARS,T); %Old fluid values saved;
% compile the body
      !&COMVAL(CADDR FN,STATUS!&); 
% Restore old fluids
      !&FREERSTR(ALSTS!&,STATUS!&); 
% Go back to the old ENVIRONMENT!&
      !&RSTVARL(VARS,ARGS);
%/      !&FIXFRM(OLDSTOMAP,OLDCODE,0)
   END;

% Load a sequence of expressions into the registers

SYMBOLIC PROCEDURE !&LOADARGS(ARGS,STATUS!&,PREGS!&); 
   BEGIN INTEGER N; SCALAR FN,DESTREG!&;
      N := LENGTH ARGS; 
      IF N>MAXNARGS!& THEN
	 !&COMPERROR LIST("Too many arguments",ARGS);
      WHILE ARGS DO 
% Generate a load for each arg
         <<DESTREG!& := !&MKREG N;
           !&LOADOPENEXP(DESTREG!&,CAR ARGS,STATUS!&,PREGS!&);
	   PREGS!& := DESTREG!& . PREGS!&;
           N := N - 1; 
           ARGS := CDR ARGS>>
   END;
	
SYMBOLIC PROCEDURE !&LOADOPENEXP(DESTREG!&,ARG,STATUS!&,PREGS!&);
  BEGIN SCALAR R;
  IF !&ANYREG ARG OR !&RASSOC(ARG,REGS!&) THEN !&LREG(DESTREG!&,!&LOCATE ARG)
    ELSE IF !&ANYREGFNP ARG THEN
     <<!&LOADOPENEXP(DESTREG!&,CADR ARG,1,PREGS!&);
       !&LREG(DESTREG!&,!&LOCATE (CAR ARG . DESTREG!& . CDDR ARG)) >>
    ELSE   %  Must be an open function
	IF FLAGP(CAR ARG,'MEMMOD) AND STATUS!& < 2 THEN
          <<!&LOADOPENEXP(DESTREG!&,ARG,2,PREGS!&);
	    !&LREG(DESTREG!&,IF EQCAR(CADR ARG,'!$NAME) THEN 
			        !&LOCATE CADR CADR ARG
			   ELSE !&LOCATE CADR ARG)>>
	ELSE
	     BEGIN
	      SCALAR OPFN,ADJFN,ANYREGARGS;
		ANYREGARGS := !&REMOPEN(DESTREG!&,CDR ARG);
		OPFN := GET(CAR ARG,'OPENFN);
                IF IDP OPFN THEN
                   APPLY(OPFN,LIST(DESTREG!&,ANYREGARGS,ARG))
	         ELSE
		   !&CALLOPEN(OPFN,DESTREG!&,ANYREGARGS,CAR ARG)
              END;
     END;  

SYMBOLIC PROCEDURE !&REMOPEN(DESTREG!&,ARGS);
   FOR EACH ARG IN ARGS COLLECT !&ARGLOC ARG;

SYMBOLIC PROCEDURE !&ARGLOC ARG;
  BEGIN SCALAR LOC;
    IF EQCAR(ARG,'!$NAME) THEN RETURN ARG;
    IF !&CONSTP ARG THEN RETURN ARG;
    IF EQCAR(ARG,'MEMORY) THEN RETURN !&MEMADDRESS ARG;
    IF LOC := !&RASSOC(ARG,REGS!&) THEN
        <<PREGS!& := CAR LOC . PREGS!&; RETURN CAR LOC>>;
    IF !&ANYREG ARG THEN RETURN ARG;
    IF !&ANYREGFNP ARG THEN RETURN (CAR ARG . !&ARGLOC CADR ARG . CDDR ARG);
    IF NULL DESTREG!& OR DESTREG!& MEMBER PREGS!& THEN DESTREG!& := !&TEMPREG();
    IF FLAGP(CAR ARG,'MEMMOD) THEN 
       <<!&LOADOPENEXP(DESTREG!&,ARG,2,PREGS!&);
         RETURN CADR CADR ARG>>
    ELSE !&LOADOPENEXP(DESTREG!&,ARG,1,PREGS!&);
    PREGS!& := DESTREG!& . PREGS!&;
    RETURN DESTREG!&
  END;

SYMBOLIC PROCEDURE !&MEMADDRESS ARG;
 BEGIN SCALAR TEMPDEST;
  PREGS!& := DESTREG!& . PREGS!&;
  TEMPDEST := !&TEMPREG();
  PREGS!& := CDR PREGS!&;
  ARG := CAR ARG . !&REMOPEN(TEMPDEST,CDR ARG);
  IF NOT(CADDR ARG = '(WCONST 0) AND NOT !&ANYREGFNP CADR ARG
     OR !&REGFP CADR ARG) THEN 
	<<!&LREG(TEMPDEST,!&LOCATE CADR ARG);
          ARG := CAR ARG . TEMPDEST . CDDR ARG>>;
  IF CADR ARG = TEMPDEST THEN PREGS!& := TEMPDEST . PREGS!&;
  RETURN ARG;
 END;

SYMBOLIC PROCEDURE !&CALLOPEN(OPFN,DEST!&,ARGS,OP);
 BEGIN
  SCALAR PATS,PARAMS,ADJFN,REGFN,ENVIRONMENT!&;
  PATS := CAR OPFN;
  IF IDP PATS THEN PATS := GET(PATS,'PATTERN);
  PARAMS := OP . CDR OPFN;
  ADJFN := CAR PATS;
  REGFN := CADR PATS;
  IF ADJFN THEN ARGS := APPLY(ADJFN,LIST ARGS);
  PATS := CDDR PATS;
  WHILE NOT NULL PATS AND NOT !&MATCHES(CAAR PATS,ARGS) DO
	 PATS := CDR PATS;
  IF NULL PATS THEN
    <<!&COMPERROR(LIST("Compiler bug - no pattern for",OP . ARGS));
      RETURN NIL>>;
  FOR EACH MAC IN CDAR PATS DO
    !&EMITMAC(!&SUBARGS(MAC,ARGS,PARAMS));
  IF REGFN THEN IF IDP REGFN THEN APPLY(REGFN,LIST(OP, ARGS))
		ELSE !&EMITMAC(!&SUBARGS(REGFN,ARGS,PARAMS));
  RETURN NIL;
 END;

SYMBOLIC PROCEDURE !&MATCHES(PAT,SUBJ);
 IF EQCAR(PAT,'QUOTE) THEN CADR PAT = SUBJ
  ELSE IF NULL PAT THEN NULL SUBJ
  ELSE IF EQCAR(PAT,'NOVAL) THEN STATUS!& > 1 AND !&MATCHES(CDR PAT,SUBJ)
  ELSE IF ATOM PAT THEN APPLY(GET(PAT,'MATCHFN),LIST SUBJ)
  ELSE PAIRP SUBJ AND !&MATCHES(CAR PAT,CAR SUBJ)
        AND !&MATCHES(CDR PAT,CDR SUBJ);

SYMBOLIC PROCEDURE !&ANY U;T;

SYMBOLIC PROCEDURE !&DEST U;U = DEST!&;

% An anyreg which uses DEST!& at any level
SYMBOLIC PROCEDURE !&USESDEST U;
  !&DEST U OR PAIRP U AND !&USESDESTL CDR U;

SYMBOLIC PROCEDURE !&USESDESTL U;
  PAIRP U AND (!&DEST CAR U OR !&USESDEST CAR U OR !&USESDESTL CDR U);

SYMBOLIC PROCEDURE !&REGFP U;!&REGP U OR EQCAR(U,'!$LOCAL);

SYMBOLIC PROCEDURE !&REGN U; !&REGP U OR EQCAR(U,'!$LOCAL) OR U = '(QUOTE NIL);

SYMBOLIC PROCEDURE !&MEM U;
 NOT(U = '(QUOTE NIL) OR EQCAR(U,'!$LOCAL))
	AND (!&CONSTP U OR !&VARP U OR CAR U = 'MEMORY);

SYMBOLIC PROCEDURE !&NOTANYREG U;!&MEM U OR !&REGFP U;



SYMBOLIC PROCEDURE !&SUBARGS(MAC,ARGS,PARAMS);
    FOR EACH ARG IN MAC COLLECT !&SUBARG(ARG,ARGS,PARAMS);

SYMBOLIC PROCEDURE !&SUBARG(ARG,ARGS,PARAMS);
 BEGIN SCALAR ARGFN;
  RETURN
    IF EQCAR(ARG,'QUOTE) THEN CADR ARG
    ELSE IF PAIRP ARG THEN !&SUBARGS(ARG,ARGS,PARAMS)
    ELSE IF ARG = 'DEST THEN DEST!&
    ELSE IF ARGFN := GET(ARG,'SUBSTFN) THEN
	APPLY(ARGFN,LIST(ARG,ARGS,PARAMS))
    ELSE !&COMPERROR(LIST("Compiler bug", ARG,"invalid in macro"))
 END;

SYMBOLIC PROCEDURE !&ARG1(ARG,ARGS,PARAMS);
 !&LOCATE CAR ARGS;

SYMBOLIC PROCEDURE !&ARG2(ARG,ARGS,PARAMS);
 !&LOCATE CADR ARGS;

SYMBOLIC PROCEDURE !&ARG3(ARG,ARGS,PARAMS);
 !&LOCATE CADDR ARGS;

SYMBOLIC PROCEDURE !&ARG4(ARG,ARGS,PARAMS);
 !&LOCATE CADDDR ARGS;

SYMBOLIC PROCEDURE !&PARAM1(ARG,ARGS,PARAMS);
 CAR PARAMS;

SYMBOLIC PROCEDURE !&PARAM2(ARG,ARGS,PARAMS);
 CADR PARAMS;

SYMBOLIC PROCEDURE !&PARAM3(ARG,ARGS,PARAMS);
 CADDR PARAMS;

SYMBOLIC PROCEDURE !&PARAM4(ARG,ARGS,PARAMS);
 CADDDR PARAMS;

SYMBOLIC PROCEDURE !&GETTEMP(TNAME,ARGS,PARAMS);
 BEGIN SCALAR TN;
  RETURN IF TN := ASSOC(TNAME,ENVIRONMENT!&) THEN CDR TN
	  ELSE <<TN := !&TEMPREG();
		 ENVIRONMENT!& := (TNAME . TN) . ENVIRONMENT!&;
		 PREGS!& := TN . PREGS!&;
		 TN>>;
  END;

SYMBOLIC PROCEDURE !&GETTEMPLBL(LNAME,ARGS,PARAMS);
 BEGIN SCALAR LAB;
   RETURN IF LAB := ASSOC(LNAME,ENVIRONMENT!&) THEN CDR LAB
           ELSE <<LAB := !&GENLBL();
		  ENVIRONMENT!& := (LNAME . LAB) . ENVIRONMENT!&;
		  LAB>>
  END;

SYMBOLIC PROCEDURE !&GENSYM();	 % gensym local to compiler, reuses symbols
BEGIN SCALAR SYMB;
    IF NULL CDR LOCALGENSYM!& THEN
	RPLACD(LOCALGENSYM!&, LIST GENSYM());
    SYMB := CAR LOCALGENSYM!&;
    LOCALGENSYM!& := CDR LOCALGENSYM!&;
    RETURN SYMB;
END;

SYMBOLIC PROCEDURE !&COMPERROR U;
<<  ERRORPRINTF("***** in %P: %L", NAME!&, U);
    ERFG!* := T >>;

SYMBOLIC PROCEDURE !&COMPWARN U; 
    !*MSG AND ERRORPRINTF("*** in %P: %L", NAME!&, U);

SYMBOLIC PROCEDURE !&EMITMAC MAC;
 BEGIN SCALAR EMITFN;
  IF CAR MAC = '!*DO THEN APPLY(CADR MAC,CDDR MAC)
  ELSE IF CAR MAC = '!*DESTROY THEN
    FOR EACH REG IN CDR MAC DO REGS!& := DELASC(REG,REGS!&)
  ELSE IF CAR MAC = '!*SET THEN
    REGS!& := !&REPASC(CADR MAC,!&REMREGSL CADDR MAC,REGS!&)
  ELSE 
     IF EMITFN := GET(CAR MAC,'EMITFN) THEN
       APPLY(EMITFN,LIST MAC)
     ELSE !&ATTACH MAC
 END;

SYMBOLIC PROCEDURE !&EMITLOAD M;
 !&LREG(CADR M,CADDR M);

SYMBOLIC PROCEDURE !&EMITSTORE M;
 !&STOREVAR(CADDR M,CADR M);

SYMBOLIC PROCEDURE !&EMITJUMP M;
 !&ATTJMP CADR M;

SYMBOLIC PROCEDURE !&EMITLBL M;
 !&ATTLBL CADR M;

SYMBOLIC PROCEDURE !&EMITMEMMOD M;
 BEGIN SCALAR Y, X;
  X := CADR M;
  !&REMREFS X;
  IF EQCAR(X,'!$LOCAL) THEN
      WHILE Y := ASSOC(X,SLST!&) DO SLST!& := DELETIP(Y,SLST!&); 
  IF EQCAR(X,'!$LOCAL) THEN M := CAR M . !&GETFRM X . CDDR M;
  !&ATTACH(GET(CAR M, 'UNMEMMOD) . CDR M);
 END;
 
% Support to patterns - register adjustment functions

SYMBOLIC PROCEDURE !&NOANYREG ARGS;
% remove all ANYREG stuff except top level MEMORY
IF NULL ARGS THEN NIL
ELSE 
    !&NOANYREG1 CAR ARGS . !&NOANYREG CDR ARGS;

SYMBOLIC PROCEDURE !&NOANYREG1 ARG;
    IF !&ANYREGFNP ARG AND NOT EQCAR(ARG,'MEMORY) THEN
	!&LOADTEMPREG ARG ELSE ARG;

SYMBOLIC PROCEDURE !&INREG ARGS;
  IF NOT !&REGFP CAR ARGS THEN LIST !&LOADTEMPREG CAR ARGS ELSE ARGS;

SYMBOLIC PROCEDURE !&REGMEM ARGS;
 <<ARGS := !&NOANYREG ARGS;
   IF !&MEM CAR ARGS AND !&MEM CADR ARGS THEN 
	!&LOADTEMPREG CAR ARGS . CDR ARGS
   ELSE ARGS>>;

SYMBOLIC PROCEDURE !&DESTMEM ARGS;
% A1 in DEST!&, A2 in MEM, rest (if any) not anyreg
<<ARGS := CAR ARGS . !&NOANYREG CDR ARGS;
  IF STATUS!& > 1 THEN
    IF !&REGFP CAR ARGS THEN ARGS
    ELSE !&LOADTEMPREG CAR ARGS . CDR ARGS
  ELSE IF !&DEST CADR ARGS OR !&USESDEST CADR ARGS THEN
	!&DESTMEM(CAR ARGS . !&LOADTEMPREG CADR ARGS . CDDR ARGS)
  ELSE IF CAR ARGS NEQ DEST!& THEN 
	<<!&LREG(DEST!&,!&LOCATE CAR ARGS);
	  DEST!& . CDR ARGS>>
  ELSE ARGS>>;

SYMBOLIC PROCEDURE !&DESTMEMA ARGS;
% put either a1or A2 into DEST!&, the other to MEM.
IF CAR ARGS = DEST!& THEN % A1 = DEST!&, make A1 mem or reg
  IF !&NOTANYREG CADR ARGS AND NOT !&USESDEST CADR ARGS THEN ARGS
	ELSE !&LOADTEMP2 ARGS
ELSE IF CADR ARGS = DEST!& THEN % A2 = DEST!&, make A2 mem or reg
  IF !&NOTANYREG CAR ARGS AND NOT !&USESDEST CAR ARGS THEN ARGS
	ELSE !&LOADTEMP1 ARGS
ELSE IF !&NOTANYREG CADR ARGS OR NOT !&NOTANYREG CAR ARGS
THEN  % A2 is MEM or A1 is anyreg: make A1 the destination
  <<IF NOT !&NOTANYREG CADR ARGS OR !&USESDEST CADR ARGS THEN
	ARGS := !&LOADTEMP2 ARGS;
    !&LREG(DEST!&,!&LOCATE CAR ARGS);
    DEST!& . CDR ARGS>>
ELSE  % Make A2 the DEST!& - only when A2 is anyreg and a1 is mem
  <<IF NOT !&NOTANYREG CAR ARGS OR !&USESDEST CAR ARGS THEN
	ARGS := !&LOADTEMP1 ARGS;
    !&LREG(DEST!&,!&LOCATE CADR ARGS);
    LIST(CAR ARGS,DEST!&)>>;

SYMBOLIC PROCEDURE !&LOADTEMP1 U;
% Bring first arg into a temp
!&LOADTEMPREG CAR U . CDR U;

SYMBOLIC PROCEDURE !&LOADTEMP2 U;
% put second arg in a temp
CAR U . !&LOADTEMPREG CADR U . CDDR U;

SYMBOLIC PROCEDURE !&CONSARGS ARGS;
 IF 
    NOT !&ANYREGFNP CADR ARGS AND CADR ARGS NEQ DEST!&
   OR
    NOT !&ANYREGFNP CAR ARGS AND CAR ARGS NEQ DEST!&
 THEN ARGS
 ELSE LIST(CAR ARGS,!&LOADTEMPREG CADR ARGS);

SYMBOLIC PROCEDURE !&LOADTEMPREG ARG;
% Load ARG into a temporary register.  Return the register.
 BEGIN
    SCALAR TEMP;
    TEMP := !&TEMPREG();
    PREGS!& := TEMP . PREGS!&;
    !&LREG(TEMP,!&LOCATE ARG);
    RETURN TEMP
   END;

SYMBOLIC PROCEDURE !&FIXREGTEST(OP,ARGS);
    !&FIXREGTEST1(OP, first ARGS, second ARGS);

SYMBOLIC PROCEDURE !&FIXREGTEST1(OP, A1, A2);
% Fixes up the registers after a conditional jump has been emitted.
% For JUMPEQ and JUMPNE, equalities can be assumed in REGS!& or REGS1!&
% For other jumps, REGS!& copied onto REGS1!&.
  <<REGS1!& := REGS!&;
    IF OP = 'EQ OR OP = 'NE THEN
     IF NOT !&REGP A1 THEN
     <<  IF !&REGP A2 THEN !&FIXREGTEST1(OP,A2,A1) >>
     ELSE 
      <<IF OP = 'EQ THEN REGS1!& := !&ADDRVALS(A1,REGS1!&,!&REMREGS A2)
		    ELSE REGS!&  := !&ADDRVALS(A1,REGS!& ,!&REMREGS A2)>>>>;


SYMBOLIC PROCEDURE !&SETREGS1(OP, ARGS); REGS1!& := REGS!&;


% Find the location of a variable


SYMBOLIC PROCEDURE !&LOCATE X; 
   BEGIN SCALAR Y,VTYPE; 
% Constants are their own location
     IF ATOM X OR EQCAR(X,'LABEL) OR !&CONSTP X THEN RETURN X;
     IF EQCAR(X,'!$NAME) THEN RETURN CADR X;
     IF CAR X = 'MEMORY THEN
	RETURN(CAR X . !&LOCATE CADR X . CDDR X);
     IF Y := !&RASSOC(X,REGS!&) THEN RETURN CAR Y;
% If in a register, return the register number
% Registers are their own location
% For ANYREG stuff, locate each constant 
      IF !&ANYREGFNP X THEN
	RETURN CAR X . !&LOCATEL CDR X;
      IF NOT EQCAR(X,'!$LOCAL) THEN RETURN X;
% Since the value of the variable has been referenced, a previous store was
% justified, so it can be removed from SLST!&
% Must be in the frame, otherwise make nonlocal (really ought to be an error)
% Frame location (<=0) is returned
        WHILE Y := ASSOC(X,SLST!&) DO SLST!& := DELETIP(Y,SLST!&); 
        IF Y := ASSOC(X,STOMAP!&) THEN RETURN CADR Y;
% Nasty compiler bug.  Until we fix it, tell the user to simplify expressions
	!&COMPERROR LIST
	 ("Compiler bug: expression too complicated, please simplify",X);
	RETURN '(QUOTE 0);		% just so it doesn't blow up
   END;

SYMBOLIC PROCEDURE !&LOCATEL U;
   FOR EACH X IN U COLLECT !&LOCATE X;

% Load register REG with value U. V (always NIL except when called from
% LOADARGS) is a list of other loads to be done

SYMBOLIC PROCEDURE !&LREG(REG,VAL);
 BEGIN SCALAR ACTUALVAL;
  ACTUALVAL := !&REMREGS VAL;
  IF REG = VAL OR ACTUALVAL MEMBER !&REGVAL REG THEN RETURN NIL;
  !&ATTACH LIST('!*MOVE,VAL,REG);
  REGS!& := !&REPASC(REG,ACTUALVAL,REGS!&);
 END;

% Load register 1 with X

SYMBOLIC PROCEDURE !&LREG1(X); !&LOADOPENEXP('(REG 1),X,1,PREGS!&);

SYMBOLIC PROCEDURE !&JUMPT LAB;
!&ATTACH LIST('!*JUMPNOTEQ,LAB,'(REG 1),'(QUOTE NIL));

SYMBOLIC PROCEDURE !&JUMPNIL LAB;
!&ATTACH LIST('!*JUMPEQ,LAB,'(REG 1),'(QUOTE NIL));


COMMENT Functions for Handling Non-local Variables; 

SYMBOLIC PROCEDURE !&VARBIND(VARS,LAMBP); 
   %bind FLUID variables in lambda or prog lists;
   %LAMBP is true for LAMBDA, false for PROG;
   BEGIN SCALAR VLOCS,VNAMES,FREGS,Y,REG,TAIL; INTEGER I; 
      I := 1; 
      FOR EACH X IN VARS DO
  	       <<
		REG := !&MKREG I;
                IF EQCAR(X,'!$GLOBAL) THEN	 % whoops
                <<  !&COMPWARN LIST("Illegal to bind global",
				     CADR X, "but binding anyway");
		    RPLACA(X,'!$FLUID) >>;	 % cheat a little
		IF EQCAR(X,'!$FLUID)
                  THEN <<FREEBOUND!& := T;
			 VNAMES := X . VNAMES; 
                         IF NOT !*NOFRAMEFLUID THEN VLOCS := !&FRAME X . VLOCS;
			 FREGS := REG . FREGS>>
                ELSE IF EQCAR(X,'!$LOCAL)
                        THEN <<!&FRAME X;
			       !&STORELOCAL(X,IF LAMBP THEN REG ELSE NIL)>>
		   ELSE !&COMPERROR LIST("Cannot bind non-local variable",X);
		IF LAMBP THEN
		  IF EQCAR(X,'!$LOCAL) THEN
			 REGS!& := !&REPASC(REG,LIST X,REGS!&)
			ELSE REGS!& := !&REPASC(REG,NIL,REGS!&);
		I := I + 1>>; 
      IF NULL VNAMES THEN RETURN NIL;
      VNAMES := 'NONLOCALVARS . VNAMES;
      FREGS := 'REGISTERS . FREGS;
      VLOCS := 'FRAMES . VLOCS;
      TAIL := IF !*NOFRAMEFLUID THEN LIST VNAMES
	      ELSE LIST(VNAMES,VLOCS);
      IF LAMBP THEN !&ATTACH('!*LAMBIND . FREGS . TAIL)
	       ELSE !&ATTACH('!*PROGBIND . TAIL);
      IF !*UNSAFEBINDER THEN REGS!& := NIL;
      RETURN TAIL;
   END;

SYMBOLIC PROCEDURE !&FREERSTR(ALSTS!&,STATUS!&); %restores FLUID variables;
    IF ALSTS!& THEN
    <<  !&ATTACH('!*FREERSTR . ALSTS!&);
	IF !*UNSAFEBINDER THEN REGS!& := NIL >>;

% ATTACH is used to emit code

SYMBOLIC PROCEDURE !&ATTACH U; CODELIST!& := U . CODELIST!&;

SYMBOLIC PROCEDURE !&STORELOCAL(U,REG); 
   %marks expression U in register REG for storage;
   BEGIN SCALAR X; 
      IF NULL REG THEN REG := '(QUOTE NIL);
      X := LIST('!*MOVE,REG,!&GETFRM U);
% Update list of stores done so far
      !&ATTACH X; 
% Zap out earlier stores if there were never picked up
% ie, if you store to X, then a ref to X will remove this store from
% SLST!&.  Otherwise, the previous store will be removed by CLRSTR
% SLST!& is for variables only (anything else?)
      !&CLRSTR U;
       SLST!& := (U . CODELIST!&) . SLST!&;
   END;

SYMBOLIC PROCEDURE !&CLRSTR VAR; %removes unneeded stores;
   BEGIN SCALAR X; 
% Inside conditionals, you cant tell if store was on the same path
      IF CONDTAIL!& THEN RETURN NIL; 
      X := ASSOC(VAR,SLST!&); 
      IF NULL X THEN RETURN NIL; 
      SLST!& := DelQIP(X,SLST!&); 
      !&DELMAC CDR X;
   END;

COMMENT Functions for general tests; 

SYMBOLIC PROCEDURE !&COMTST(EXP,LABL); 
   %compiles boolean expression EXP.
   %If EXP has the same value as SWITCH!& then branch to LABL,
   %otherwise fall through;
   %REGS are active registers for fall through,
   %REGS1 for branch;
   BEGIN SCALAR X,FN,REG; 
% First factor out NOT's to set up the SWITCH!&
      WHILE EQCAR(EXP,'EQ) AND CADDR EXP = '(QUOTE NIL) DO 
         <<SWITCH!& := NOT SWITCH!&; EXP := CADR EXP>>; 
% Dispatch a built in compiling function
      IF NOT SWITCH!& AND (FN := GET(CAR EXP,'FLIPTST)) THEN
	EXP := FN . CDR EXP;  % SWITCH!& is assumed to be true by fn's with
			      % a flip test
      IF FN := GET(CAR EXP,'OPENTST)
         THEN <<IF ATOM FN THEN APPLY(FN,LIST(EXP,LABL))
		 ELSE !&COMOPENTST(FN,EXP,LABL,PREGS!&)>>
% Trivial case of condition is T.  FLAGG!& indicates jump cannot take place
       ELSE <<IF EQCAR(EXP,'QUOTE) THEN
                IF SWITCH!& AND CADR EXP 
		    OR (NOT SWITCH!&) AND (NOT CADR EXP) THEN 
		   <<REGS1!& := REGS!&;
		    !&ATTJMP LABL>>
		 ELSE FLAGG!& := T
              ELSE <<!&COMTST(LIST('NE,EXP,'(QUOTE NIL)),LABL)>>>>

   END;

SYMBOLIC PROCEDURE !&COMOPENTST(PAT,EXP,DESTLAB,PREGS!&);
 BEGIN
  SCALAR ANYREGARGS,ADJFN;
  ANYREGARGS := !&REMOPEN(!&TEMPREG(),!&COMLIS CDR EXP);
  !&CALLOPEN(PAT,DESTLAB,ANYREGARGS,CAR EXP)
 END;


% Remove variables to avoid name conflicts:  Hide variable names which match
% new names when entering an inner function.  Other names will be available
% as global info.  VARS is the list of new variable names, the result is a
% list of protected stores.

SYMBOLIC PROCEDURE !&REMVARL VARS; 
   FOR EACH X IN VARS COLLECT !&PROTECT X;


% Delete all references to U from SLST!&
% return the protected store
SYMBOLIC PROCEDURE !&PROTECT U; 
   BEGIN SCALAR X; 
      IF X := ASSOC(U,SLST!&) THEN SLST!& := DelQIP(X,SLST!&); 
      RETURN X
   END;

% Restore a previous ENVIRONMENT!&.  VARS is the list of variables taken out
% of the ENVIRONMENT!&; LST is the list of protected stores.  One or zero
% stores for each variable.

SYMBOLIC PROCEDURE !&RSTVARL(VARS,LST); 
   WHILE VARS DO 
      <<!&RSTVAR(CAR VARS,CAR LST); VARS := CDR VARS; LST := CDR LST>>;

% Restore a particular variable and STORE

SYMBOLIC PROCEDURE !&RSTVAR(VAR,VAL); 
   BEGIN 
      !&REMREFS VAR;
      !&CLRSTR VAR; 
% Put back on store list if not NIL
      !&UNPROTECT VAL
   END;

SYMBOLIC PROCEDURE !&UNPROTECT VAL; %restores VAL to SLST!&;
   IF VAL THEN SLST!& := VAL . SLST!&;


SYMBOLIC PROCEDURE !&STOREVAR(U,V); 
% The store generated by a SETQ
   BEGIN SCALAR VTYPE,X;
      !&REMREFS U;
      IF CAR U = '!$LOCAL THEN
         !&STORELOCAL(U,V)
      ELSE
         !&ATTACH LIST('!*MOVE,V,U);
      IF !&REGP V THEN
	 REGS!& := !&ADDRVALS(V,REGS!&,LIST U)
   END;


COMMENT Support Functions; 

SYMBOLIC PROCEDURE !&REFERENCES(EXP,VAR);
% True if expression EXP (probably ANYREG) references VAR.
EXP = VAR OR 
  IF ATOM EXP OR FLAGP(CAR EXP,'TERMINAL) THEN NIL
    ELSE !&REFERENCESL(CDR EXP,VAR);

SYMBOLIC PROCEDURE !&REFERENCESL(EXP,VAR);
IF NULL EXP THEN NIL ELSE !&REFERENCES(CAR EXP,VAR)
			  OR !&REFERENCESL(CDR EXP,VAR);

SYMBOLIC PROCEDURE !&CFNTYPE FN; 
   BEGIN SCALAR X; 
      RETURN IF X := GET(FN,'CFNTYPE) THEN CAR X
              ELSE IF X := GETD FN THEN CAR X
              ELSE  'EXPR
   END;

SYMBOLIC PROCEDURE !&GENLBL; 
   BEGIN SCALAR L; 
      L := LIST('LABEL,!&GENSYM());
      LBLIST!& := LIST L . LBLIST!&; 
      RETURN L
   END;

SYMBOLIC PROCEDURE !&GETLBL LABL; 
   BEGIN SCALAR X; 
      X := ASSOC(LABL,GOLIST!&); 
      IF NULL X THEN !&COMPERROR LIST("Compiler bug: missing label", LABL);
      RETURN CDR X
   END;


SYMBOLIC PROCEDURE !&ATTLBL LBL; 
   IF CAAR CODELIST!& EQ '!*LBL THEN !&DEFEQLBL(LBL,CADR CAR CODELIST!&)
   ELSE !&ATTACH LIST('!*LBL,LBL);

SYMBOLIC PROCEDURE !&ATTJMP LBL; 
   BEGIN 
      IF CAAR CODELIST!& EQ '!*LBL
        THEN <<!&DEFEQLBL(LBL,CADR CAR CODELIST!&);
               !&DELMAC CODELIST!&>>; 
      IF !&TRANSFERP CODELIST!& THEN RETURN NIL; 
      !&ATTACH LIST('!*JUMP,LBL); 
   END;

SYMBOLIC PROCEDURE !&TRANSFERP X; 
   IF CAAR X = '!*NOOP THEN !&TRANSFERP CDR X ELSE
        FLAGP(IF CAAR X EQ '!*LINK THEN CADAR X ELSE CAAR X,'TRANSFER);

SYMBOLIC PROCEDURE !&DEFEQLBL(LAB1,LAB2);
 LBLIST!& := !&DEFEQLBL1(LBLIST!&,LAB1,LAB2);

SYMBOLIC PROCEDURE !&DEFEQLBL1(LABS,LAB1,LAB2);
 IF LAB1 MEMBER CAR LABS THEN
	IF LAB2 MEMBER CAR LABS THEN LABS
	 ELSE APPEND(!&LABCLASS LAB2,CAR LABS) . !&DELCLASS(LAB2,CDR LABS)
   ELSE IF LAB2 MEMBER CAR LABS THEN
              APPEND(!&LABCLASS LAB1,CAR LABS) . !&DELCLASS(LAB1,CDR LABS)
   ELSE CAR LABS . !&DEFEQLBL1(CDR LABS,LAB1,LAB2);

SYMBOLIC PROCEDURE !&LABCLASS(LAB);
 BEGIN SCALAR TEMP;
  TEMP := LBLIST!&;
   WHILE TEMP AND NOT (LAB MEMBER CAR TEMP) DO TEMP := CDR TEMP;
   RETURN IF TEMP THEN CAR TEMP ELSE NIL;
  END;

SYMBOLIC PROCEDURE !&DELCLASS(LAB,LABS);
 IF LAB MEMBER CAR LABS THEN CDR LABS ELSE CAR LABS . !&DELCLASS(LAB,CDR LABS);

SYMBOLIC PROCEDURE !&LBLEQ(LAB1,LAB2);
 LAB1 MEMBER !&LABCLASS LAB2;

SYMBOLIC PROCEDURE !&FRAME U; %allocates space for U in frame;
   BEGIN SCALAR Z,RES; 
      Z := IF NULL STOMAP!& THEN 1 ELSE 1 + CADR CADAR STOMAP!&;
      RES := !&MKFRAME Z;
      STOMAP!& := LIST(U,RES) . STOMAP!&; 
      LLNGTH!& := MAX(Z,LLNGTH!&);
      RETURN RES
   END;

% GETFRM returns the frame location on a variable
SYMBOLIC PROCEDURE !&GETFRM U; 
   BEGIN SCALAR X;
     IF X:=ASSOC(U,STOMAP!&) THEN RETURN CADR X;
     !&COMPERROR LIST("Compiler bug: lost variable",U)
   END;

%*************************************************************************
% The following functions determine classes or properties of expressions *
%*************************************************************************


SYMBOLIC PROCEDURE !&ANYREG U; 
% !&ANYREG determines if U is an ANYREG expression
%
% ANYREG expressions are those expressions which may be loaded into any
% register without the use of (visable) temporary registers.  It is assumed
% that ANYREG expressions have no side effects.
%
% ANYREG expressions are defined as constants, variables, and ANYREG functions
% whose arguments are ANYREG expressions.  Note that ANYREG functions are
% not necessarily a part of ANYREG expressions; their arguments may not be
% ANYREG expressions.
!&CONSTP U OR !&VARP U OR !&ANYREGFNP U AND !&ANYREGL CDR U;

SYMBOLIC PROCEDURE !&ANYREGL U; 
   NULL U OR !&ANYREG(CAR U) AND !&ANYREGL CDR U;

SYMBOLIC PROCEDURE !&ANYREGFNP U;
% !&ANYREGFNP is true when U is an ANYREG function.  The arguments are not
% checked
   !&ANYREGP CAR U;

SYMBOLIC PROCEDURE !&OPENP U;
!&CONSTP U OR !&VARP U OR (!&ANYREGFNP U OR !&OPENFNP U) AND !&OPENPL CDR U;

SYMBOLIC PROCEDURE !&OPENPL U;
NULL U OR !&OPENP CAR U AND !&OPENPL CDR U;

SYMBOLIC PROCEDURE !&OPENFNP U;
   GET(CAR U,'OPENFN);

SYMBOLIC PROCEDURE !&CONSTP U;
% True if U is a constant expression
   IDP CAR U AND FLAGP(CAR U,'CONST);

SYMBOLIC PROCEDURE !&VARP U;
% True if U is a variable: (LOCAL x),(FLUID x), ...
   PAIRP U AND FLAGP(CAR U,'VAR);

SYMBOLIC PROCEDURE !&REGP U;
   PAIRP U AND FLAGP(CAR U,'REG);

SYMBOLIC PROCEDURE !&NOSIDEEFFECTP U;
% True if the expression U has no side effects.  ANYREG expressions and
% functions are assumed to have no side effects; other functions must be
% flagged NOSIDEEFFECT.  All arguments to a function must also be NOSIDEEFFECT.
!&ANYREG U OR  
   (!&ANYREGFNP U OR FLAGP(CAR U,'NOSIDEEFFECT)) AND !&NOSIDEEFFECTPL CDR U;


SYMBOLIC PROCEDURE !&NOSIDEEFFECTPL U;
NULL U OR !&NOSIDEEFFECTP CAR U AND !&NOSIDEEFFECTPL CDR U;

%**********************************************************************
%  Basic register manipulation utilities
%**********************************************************************


SYMBOLIC PROCEDURE !&RVAL(R,RGS); 
% Return the set of values in register R as determined by register list RGS
   IF NULL RGS THEN NIL
      ELSE IF CAAR RGS = R THEN CDAR RGS
       ELSE !&RVAL(R,CDR RGS);

SYMBOLIC PROCEDURE !&REGVAL R;
% Normally, register contents are found in register list REGS!&.
   !&RVAL(R,REGS!&);


SYMBOLIC PROCEDURE !&ADDRVALS(REG,RGS,VALS);
% Add the values VALS to the contents of REG in register list RGS
  IF NULL RGS THEN LIST (REG . VALS)
  ELSE IF CAAR RGS = REG THEN (CAAR RGS . APPEND(VALS,CDAR RGS)) . CDR RGS
  ELSE CAR RGS . !&ADDRVALS(REG,CDR RGS,VALS);

SYMBOLIC PROCEDURE !&MKREG NUM;
% Used to generate a tagged register from a register number
BEGIN SCALAR AENTRY;
  RETURN
  IF AENTRY := ASSOC(NUM, '((1 . (REG 1)) (2 . (REG 2)) (3 . (REG 3))
			    (4 . (REG 4)) (5 . (REG 5)) (6 . (REG 6))
			    (7 . (REG 7)) (8 . (REG 8)) (9 . (REG 9)))) THEN
	CDR AENTRY
  ELSE LIST('REG,NUM);
END;

SYMBOLIC PROCEDURE !&MKFRAME NUM;
% Used to generate a tagged register from a register number
BEGIN SCALAR AENTRY;
  RETURN
  IF AENTRY := ASSOC(NUM, '((1 . (FRAME 1)) (2 . (FRAME 2)) (3 . (FRAME 3))
			    (4 . (FRAME 4)) (5 . (FRAME 5)) (6 . (FRAME 6))
			    (7 . (FRAME 7)) (8 . (FRAME 8)) (9 . (FRAME 9))))
	THEN CDR AENTRY
  ELSE LIST('FRAME,NUM);
END;

SYMBOLIC PROCEDURE !&RASSOC(VAL,RGS); 
% Find a register in register list RGS which contains VAL.  NIL is returned if
% VAL is not present in RGS
   IF NULL RGS THEN NIL
    ELSE IF VAL MEMBER CDAR RGS THEN CAR RGS
    ELSE !&RASSOC(VAL,CDR RGS);

SYMBOLIC PROCEDURE !&REPASC(REG,VAL,REGL); 
% Replace the contants of REG in list REGL by the value VAL
   IF NULL REGL THEN LIST (REG . VAL)
    ELSE IF REG=CAAR REGL THEN (REG . VAL) . CDR REGL
    ELSE CAR REGL . !&REPASC(REG,VAL,CDR REGL);

SYMBOLIC PROCEDURE !&RMERGE U;
% RMERGE takes a list of register contents representing the information
% present in the registers from a number of different ways to reach the same
% place.  RMERGE returns whatever information is known to be in the registers
% regardless of which path was taken.

IF NULL U THEN NIL ELSE
  BEGIN
   SCALAR RES,CONTENTS;
   RES := NIL;
   FOR EACH RG IN CAR U DO
     <<CONTENTS := NIL;
       FOR EACH THING IN CDR RG DO
         IF !&INALL(THING,CAR RG,CDR U) THEN
            CONTENTS := THING . CONTENTS;
       IF CONTENTS THEN RES := (CAR RG . CONTENTS) . RES>>;
   RETURN RES;
  END;

SYMBOLIC PROCEDURE !&INALL(THING,RG,LST);
NULL LST OR (THING MEMBER !&RVAL(RG,CAR LST)) AND !&INALL(THING,RG,CDR LST);


SYMBOLIC PROCEDURE !&TEMPREG();
 BEGIN SCALAR I,R,EMPTY,UNPROT;
  EMPTY := UNPROT := NIL;
  I := 1;
   WHILE I <= MAXNARGS!& AND NOT EMPTY DO
    <<R := !&MKREG I;
      IF NOT(R MEMBER PREGS!&) THEN
        IF I <= LASTACTUALREG!& AND NULL !&REGVAL R THEN EMPTY := R
          ELSE IF NOT UNPROT THEN UNPROT := R;
      I := I + 1
      >>;
   IF EMPTY THEN RETURN EMPTY;
   IF UNPROT THEN RETURN UNPROT;
   !&COMPERROR("Compiler bug: Not enough registers");
   RETURN '(REG ERROR);
 END;

SYMBOLIC PROCEDURE !&REMREGS U;
 IF !&REGP U THEN !&REGVAL U
  ELSE IF EQCAR(U,'FRAME) THEN LIST !&GETFVAR (U,STOMAP!&)
   ELSE IF !&CONSTP U OR !&VARP U THEN LIST U
    ELSE !&REMREGSL U;

SYMBOLIC PROCEDURE !&GETFVAR (V,SMAP);
 IF NULL SMAP THEN !&COMPERROR(LIST("Compiler bug:", V,"evaporated?"))
  ELSE IF CADAR SMAP = V THEN CAAR SMAP
   ELSE !&GETFVAR (V,CDR SMAP);

SYMBOLIC PROCEDURE !&REMREGSL U;
FOR EACH ARG IN !&ALLARGS CDR U COLLECT (CAR U . ARG);

SYMBOLIC PROCEDURE !&ALLARGS ARGLST;
   if null Arglst then NIL
   else IF NULL CDR ARGLST THEN 
	FOR EACH VAL IN !&REMREGS CAR ARGLST COLLECT LIST VAL
  ELSE !&ALLARGS1(!&REMREGS CAR ARGLST,!&ALLARGS CDR ARGLST);

SYMBOLIC PROCEDURE !&ALLARGS1(FIRSTARGS,RESTARGS);
 BEGIN SCALAR RES;
  RES := NIL;
  FOR EACH A1 IN FIRSTARGS DO
   FOR EACH A2 IN RESTARGS DO
    RES := (A1 . A2) . RES;
  RETURN RES;
 END;

SYMBOLIC PROCEDURE !&REMMREFS();
REGS!& := FOR EACH R IN REGS!& COLLECT (CAR R . !&REMMREFS1 CDR R);

SYMBOLIC PROCEDURE !&REMMREFS1 L;
IF NULL L THEN L ELSE
 IF !&REFMEMORY CAR L THEN !&REMMREFS1 CDR L
 ELSE CAR L . !&REMMREFS1 CDR L;

SYMBOLIC PROCEDURE !&REFMEMORY EXP;
 IF ATOM EXP OR FLAGP(CAR EXP,'TERMINAL) THEN NIL
 ELSE CAR EXP MEMBER '(MEMORY CAR CDR) OR !&REFMEMORYL CDR EXP;

SYMBOLIC PROCEDURE !&REFMEMORYL L;
 IF NULL L THEN NIL ELSE !&REFMEMORY CAR L OR !&REFMEMORYL CDR L;

SYMBOLIC PROCEDURE !&REMVREFS;
BEGIN SCALAR S;
    REGS!& := FOR EACH R IN REGS!& COLLECT (CAR R . !&REMVREFS1 CDR R);
% Slow version:
%   SLST!& := FOR EACH S IN SLST!& CONC 
%     IF !&EXTERNALVARP CAR S THEN NIL ELSE LIST S;
% Faster version:
   while not null Slst!& and !&ExternalVarP car car Slst!& do
	Slst!& := cdr Slst!&;
   S := Slst!&;
   while not null S and not null cdr S do
   <<  if !&ExternalVarP car car cdr S then Rplacd(S, cddr S);
	S := cdr S >>;
END;

SYMBOLIC PROCEDURE !&REMVREFS1 L;
  FOR EACH THING IN L CONC 
   IF !&REFEXTERNAL THING THEN NIL ELSE LIST THING;

SYMBOLIC PROCEDURE !&REFEXTERNAL EXP;
  IF ATOM EXP THEN NIL
   ELSE IF !&EXTERNALVARP EXP THEN T
   ELSE IF FLAGP(CAR EXP,'TERMINAL) THEN NIL 
    ELSE !&REFEXTERNALL CDR EXP;

SYMBOLIC PROCEDURE !&REFEXTERNALL EXPS;
  IF NULL EXPS THEN NIL
   ELSE !&EXTERNALVARP CAR EXPS OR !&REFEXTERNALL CDR EXPS;

SYMBOLIC PROCEDURE !&EXTERNALVARP U;
  PAIRP U AND FLAGP(CAR U,'EXTVAR);

SYMBOLIC PROCEDURE !&REMREFS V;
% Remove all references to V from REGS!&
 IF CAR V MEMBER '(MEMORY CAR CDR) THEN
   !&REMMREFS()
 ELSE
   REGS!& := FOR EACH R IN REGS!& COLLECT
            CAR R . !&REMREFS1(V,CDR R);


SYMBOLIC PROCEDURE !&REMREFS1(X,LST);
% Remove all expressions from LST which reference X
IF NULL LST THEN NIL 
 ELSE IF !&REFERENCES(CAR LST,X) THEN !&REMREFS1(X,CDR LST)
 ELSE CAR LST . !&REMREFS1(X,CDR LST);


%************************************************************
%   Test functions
%************************************************************

SYMBOLIC PROCEDURE !&TSTANDOR(EXP,LABL); 
   BEGIN SCALAR FLG,FLG1,FN,LAB2,REGSL,REGS1L,
                TAILP; 
      %FLG is initial SWITCH!& condition;
      %FN is appropriate AND/OR case;
      %FLG1 determines appropriate switching state;
      FLG := SWITCH!&; 
      SWITCH!& := NIL; 
      FN := CAR EXP EQ 'AND; 
      FLG1 := FLG EQ FN; 
      EXP := CDR EXP; 
      LAB2 := !&GENLBL(); 
      WHILE EXP DO 
         <<SWITCH!& := NIL; 
           IF NULL CDR EXP AND FLG1
             THEN <<IF FN THEN SWITCH!& := T; 
                    !&COMTST(CAR EXP,LABL); 
                    REGSL := REGS!& . REGSL; 
                    REGS1L := REGS1!& . REGS1L>>
            ELSE <<IF NOT FN THEN SWITCH!& := T; 
                   IF FLG1
                     THEN <<!&COMTST(CAR EXP,LAB2); 
                            REGSL := REGS1!& . REGSL; 
                            REGS1L := REGS!& . REGS1L>>
                    ELSE <<!&COMTST(CAR EXP,LABL); 
                           REGSL := REGS!& . REGSL; 
                           REGS1L := REGS1!& . REGS1L>>>>; 
           IF NULL TAILP
             THEN <<CONDTAIL!& := NIL . CONDTAIL!&; TAILP := T>>; 
           EXP := CDR EXP>>; 
      !&ATTLBL LAB2; 
      REGS!& := IF NOT FLG1 THEN CAR REGSL ELSE !&RMERGE REGSL; 
      REGS1!& := IF FLG1 THEN CAR REGS1L ELSE !&RMERGE REGS1L; 
      IF TAILP THEN CONDTAIL!& := CDR CONDTAIL!&; 
      SWITCH!& := FLG
   END;



%************************************************************
%  Pass2 compile functions
%************************************************************

SYMBOLIC PROCEDURE !&COMANDOR(EXP,STATUS!&); 
   BEGIN SCALAR FN,LABL,REGSL; 
      FN := CAR EXP EQ 'AND; 
      LABL := !&GENLBL(); 
      EXP := CDR EXP; 
      WHILE EXP DO 
      <<!&COMVAL(CAR EXP,IF CDR EXP THEN 1 ELSE STATUS!&); 
        %to allow for recursion on last entry;
        REGSL := REGS!& . REGSL; 
	IF CDR EXP THEN IF FN THEN !&JUMPNIL LABL ELSE !&JUMPT LABL;
	EXP := CDR EXP>>; 
      REGS!& := !&RMERGE REGSL;
      !&ATTLBL LABL
   END;

SYMBOLIC PROCEDURE !&COMAPPLY(EXP,STATUS); % Look for LIST;
   BEGIN SCALAR FN,ARGS, N,NN;
      EXP := CDR EXP; 
      FN := CAR EXP; 
      ARGS := CDR EXP; 
      IF NULL ARGS
           OR CDR ARGS
           OR NOT (PAIRP CAR ARGS 
		     AND CAAR ARGS MEMBER
			'(LIST QUOTE NCONS LIST1 LIST2 LIST3 LIST4 LIST5))
           OR LENGTH CDAR ARGS>MAXNARGS!&
        THEN RETURN !&CALL('APPLY,EXP,STATUS); 
      ARGS := IF EQCAR(CAR ARGS,'QUOTE) THEN 
		FOR EACH THING IN CADAR ARGS COLLECT LIST('QUOTE,THING)
              ELSE CDAR ARGS;
      NN := LENGTH ARGS;
      ARGS := REVERSIP (FN . REVERSE ARGS); 
      !&LOADARGS(REVERSIP !&COMLIS ARGS,1,PREGS!&); 
      !&ATTACH LIST('!*MOVE, !&MKREG(NN + 1), '(REG T1));
      !&ATTACH LIST('!*LINK,'FASTAPPLY,'EXPR, NN);
      REGS!& := NIL;
      !&REMVREFS();
   END;

%Bug fix to COMCOND - tail has (QUOTE T) not T. Test for tail screwed up anyway

SYMBOLIC PROCEDURE !&COMCOND(EXP,STATUS!&); 
   %compiles conditional expressions;
   %registers REGS!& are set for dropping through,
   %REGS1  are set for a branch;
   BEGIN SCALAR REGS1!&,FLAGG!&,SWITCH!&,LAB1,LAB2,REGSL,
                TAILP; 
      EXP := CDR EXP; 
      LAB1 := !&GENLBL(); 
      FOR EACH X ON EXP DO  % Changed IN -> ON
		 <<LAB2 := !&GENLBL(); 
                   SWITCH!& := NIL; 
                   IF CDR X THEN !&COMTST(CAAR X,LAB2) % CAR -> CAAR
			 %update CONDTAIL!&;
                   ELSE IF CAAR X = '(QUOTE T) THEN % CAR -> CAAR, T->(QUOTE T)
                        FLAGG!& := T
		   ELSE <<!&COMVAL(CAAR X,1); % CAR -> CAAR
			  !&JUMPNIL LAB2;
			  REGS1!& := !&ADDRVALS('(REG 1),
						REGS!&,
						list '(QUOTE NIL)) >>;
                   IF NULL TAILP
                      THEN <<CONDTAIL!& := NIL . CONDTAIL!&; 
                             TAILP := T>>; 
                   !&COMVAL(CADR CAR X,STATUS!&); %X -> CAR X
                          % Branch code;
	                          %test if need jump to LAB1;
                   IF NOT FLAGG!& THEN   % New line
		     <<IF NOT !&TRANSFERP CODELIST!&
                       THEN <<!&ATTJMP LAB1; 
                             REGSL := REGS!& . REGSL>>; 
                       REGS!& := REGS1!&;>>;
            %restore register status for next iteration;
            %we do not need to set REGS1!& to NIL since all COMTSTs
            %are required to set it;
                   !&ATTLBL LAB2>>; 
      IF NULL FLAGG!& AND STATUS!&<2
        THEN <<!&LREG1('(QUOTE NIL)); 
               REGS!& := !&RMERGE(REGS!& . REGSL)>>
       ELSE IF REGSL
        THEN REGS!& := !&RMERGE(REGS!& . REGSL); 
      !&ATTLBL LAB1;
      IF TAILP THEN CONDTAIL!& := CDR CONDTAIL!&
   END;

SYMBOLIC PROCEDURE !&COMCONS(EXP,STATUS!&); 
   IF NULL (EXP := CDR EXP) OR NULL CDR EXP OR CDDR EXP
     THEN !&COMPERROR LIST("Wrong number of arguments to CONS",EXP)
    ELSE IF CADR EXP='(QUOTE NIL)
     THEN !&CALL('NCONS,LIST CAR EXP,STATUS!&)
    ELSE IF CADR EXP MEMBER !&REGVAL '(REG 1)
	AND !&OPENP CAR EXP
     THEN !&CALL1('XCONS,!&COMLIS EXP,STATUS!&)
    ELSE IF !&OPENP CADR EXP THEN !&CALL('CONS,EXP,STATUS!&)
    ELSE !&CALL1('XCONS,!&COMLIS EXP,STATUS!&);

SYMBOLIC PROCEDURE !&COMGO(EXP,STATUS!&); 
   << IF STATUS!&>1 THEN <<!&ATTJMP !&GETLBL CADR EXP; SLST!& := NIL>>
      ELSE !&COMPERROR LIST(EXP,"invalid go")>>;

SYMBOLIC PROCEDURE !&COMCASE(EXP,STATUS!&);
 BEGIN SCALAR BOTTOMLAB,REGS1!&,JUMPS,EXPS,ELSELAB,HIGH,LOW,SAVEREGS,
	      JMPS,JLIST,RANGES,TABLE,TAILP;
  BOTTOMLAB := !&GENLBL();
  REGS1!& := NIL;
  !&COMVAL(CADR EXP,1);
  JUMPS := EXPS := NIL;
  CONDTAIL!& := NIL . CONDTAIL!&; 
  TAILP := T;
  FOR EACH THING ON CDDR EXP DO
   BEGIN SCALAR LAB;
     LAB := !&GENLBL();
     JUMPS := NCONC(JUMPS,LIST LIST(CAAR THING,LAB));
     EXPS := NCONC(EXPS,LIST LIST(LAB,CADAR THING));
     IF NULL CDR THING THEN
	IF NOT NULL CAAR THING THEN
	   IF STATUS!& > 1 THEN <<REGS1!& := REGS!& . REGS1!&;
			        ELSELAB := BOTTOMLAB>>
	   ELSE EXPS := NCONC(EXPS,LIST LIST(ELSELAB := !&GENLBL(),
					     '(QUOTE NIL)))
 	ELSE ELSELAB := LAB;
   END;
  RANGES := NIL;
  TABLE := NIL;
  FOR EACH JMP IN JUMPS DO
   FOR EACH NUM IN CAR JMP DO
    IF EQCAR(NUM,'RANGE) THEN
      BEGIN
  	SCALAR HIGH,LOW;
	LOW := !&GETNUM CADR NUM;
	HIGH := !&GETNUM CADDR NUM;
	IF HIGH >= LOW THEN
	  IF HIGH - LOW < 6 THEN
	     FOR I := LOW:HIGH DO
		TABLE := !&INSTBL(TABLE,I,CADR JMP)
	  ELSE RANGES := NCONC(RANGES,LIST LIST(LOW,HIGH,CADR JMP));
      END
    ELSE TABLE := !&INSTBL(TABLE,!&GETNUM NUM,CADR JMP);
  FOR EACH R IN RANGES DO
   !&ATTACH LIST('!*JUMPWITHIN,CADDR R,CAR R,CADR R);
  WHILE TABLE DO
   <<JMPS := LIST CAR TABLE;
     LOW := HIGH := CAAR TABLE;
     JLIST := LIST CADAR TABLE;
     WHILE CDR TABLE AND CAR CADR TABLE < HIGH + 5 DO
       <<TABLE := CDR TABLE;
	 WHILE HIGH < (CAAR TABLE) - 1 DO
	  <<HIGH := HIGH + 1;
	    JLIST := NCONC(JLIST,LIST ELSELAB)>>;
	 HIGH := HIGH + 1;
         JLIST := NCONC(JLIST,LIST CADAR TABLE);
	 JMPS := NCONC(JMPS,LIST CAR TABLE)>>;
     IF LENGTH JMPS < 4 THEN
	FOR EACH J IN JMPS DO
	   !&ATTACH LIST('!*JUMPEQ,CADR J,'(REG 1),LIST('WCONST,CAR J))
     ELSE
	!&ATTACH('!*JUMPON . '(REG 1) . LOW . HIGH . JLIST);
     TABLE := CDR TABLE>>;
  !&ATTJMP ELSELAB;
  SAVEREGS := REGS!&;
  FOR EACH THING IN EXPS DO
   <<!&ATTLBL CAR THING;
     REGS!& := SAVEREGS;
     IF CADR THING THEN !&COMVAL(CADR THING,STATUS!&);
     IF NOT !&TRANSFERP CODELIST!& THEN
	<<!&ATTJMP BOTTOMLAB;
	  REGS1!& := REGS!& . REGS1!&>> >>;
  !&ATTLBL BOTTOMLAB;
  REGS!& := !&RMERGE REGS1!&;
  CONDTAIL!& := CDR CONDTAIL!&
 END;

SYMBOLIC PROCEDURE !&INSTBL(TBL,I,L);
 IF NULL TBL THEN LIST LIST(I,L)
 ELSE IF I < CAAR TBL THEN LIST(I,L) . TBL
 ELSE IF I = CAAR TBL THEN
	!&COMPERROR LIST("Ambiguous case",TBL)
 ELSE CAR TBL . !&INSTBL(CDR TBL,I,L);

SYMBOLIC PROCEDURE !&GETNUM X;
 IF !&WCONSTP X AND NUMBERP CADR X THEN CADR X
 ELSE !&COMPERROR(LIST("Number expected for CASE label",X));

SYMBOLIC PROCEDURE !&COMPROG(EXP,STATUS!&); %compiles program blocks;
   BEGIN SCALAR ALSTS!&,GOLIST!&,PG,PROGLIS,EXITT!&,EXITREGS!&;
	 INTEGER I; 
	 %SCALAR OLDSTOMAP,OLDCODE;
%      OLDCODE := CODELIST!&;
%      OLDSTOMAP := STOMAP!&;
      EXITREGS!& := NIL;
      PROGLIS := CADR EXP; 
      EXP := CDDR EXP; 
      EXITT!& := !&GENLBL(); 
      PG := !&REMVARL PROGLIS; %protect prog variables;
      ALSTS!& := !&VARBIND(PROGLIS,NIL); 
      FOR EACH X IN EXP DO IF ATOM X
                             THEN GOLIST!& := (X . !&GENLBL()) . GOLIST!&; 
      WHILE EXP DO 
         <<IF ATOM CAR EXP
             THEN <<!&ATTLBL !&GETLBL CAR EXP; 
                    REGS!& := NIL>>
	   ELSE !&COMVAL(CAR EXP,IF STATUS!&>2 THEN 4 ELSE 3); 
           EXP := CDR EXP>>; 
      IF NOT !&TRANSFERP CODELIST!& AND STATUS!& < 2 THEN
	        !&LREG1('(QUOTE NIL));
      !&ATTLBL EXITT!&; 
      REGS!& := !&RMERGE (REGS!& . EXITREGS!&);
      !&FREERSTR(ALSTS!&,STATUS!&); 
      !&RSTVARL(PROGLIS,PG);
%/      !&FIXFRM(OLDSTOMAP,OLDCODE,0);
   END;

SYMBOLIC PROCEDURE !&COMPROGN(EXP,STATUS!&); 
   BEGIN 
      EXP := CDR EXP; 
      IF NULL EXP THEN RETURN !&COMVAL('(QUOTE NIL), STATUS!&);
      WHILE CDR EXP DO 
         <<!&COMVAL(CAR EXP,IF STATUS!&<2 THEN 2 ELSE STATUS!&); 
           EXP := CDR EXP>>; 
      !&COMVAL(CAR EXP,STATUS!&)
   END;

SYMBOLIC PROCEDURE !&COMRETURN(EXP,STATUS!&); 
<< EXP := CDR EXP;
   IF NULL EXP OR NOT NULL CDR EXP THEN
   <<  !&COMPERROR LIST("RETURN must have exactly one argument",EXP);
       EXP := '((QUOTE NIL)) >>;
   IF STATUS!&<4 OR NOT !&NOSIDEEFFECTP(CAR EXP)
       THEN !&LREG1(CAR !&COMLIS1 EXP); 
   SLST!& := NIL;
   EXITREGS!& := REGS!& . EXITREGS!&;
   !&ATTJMP EXITT!& >>;


SYMBOLIC PROCEDURE !&DELMAC X;
% Delete macro CAR X from CODELIST!&
  RPLACA(X,'(!*NOOP));

%*************************************************************
%              Pass 3
%*************************************************************


COMMENT Post Code Generation Fixups; 

SYMBOLIC PROCEDURE !&PASS3; 
% Pass 3 - optimization.
%    The optimizations currently performed are:
% 1. Deletion of stores not yet picked up from SLST!&.
% 2. Removal of unreachable macros.
% 3. A peep hole optimizer, currently only optmizing LBL macros.
% 4. Removal of common code chains
% 5. Changing LINK to LINKE where possible
% 6. Squeezing out unused frame locations and mapping the stack onto
%    the registers.
% Other functions of PASS3 are to tack exit code on the end and reverse
% the code list.

  <<
      FOR EACH J IN SLST!& DO !&DELMAC CDR J;
      !&ATTLBL EXITT!&; 
      !&ATTACH '(!*EXIT (!*FRAMESIZE));
      !&REMCODE(T);
      !&FIXLABS();
      !&FIXCHAINS(); 
      !&FIXLINKS(); 
      !&REMCODE(NIL);
      !&FIXFRM(NIL,NIL,NARG!&); 
      !&PEEPHOLEOPT(); 
      !&REMCODE(NIL);
      CODELIST!& := REVERSIP CODELIST!&;
  >>;

SYMBOLIC PROCEDURE !&INSERTMAC(PLACE,MAC);
 RPLACW(PLACE,MAC . (CAR PLACE . CDR PLACE));

SYMBOLIC PROCEDURE !&DELETEMAC(PLACE);
 RPLACW(PLACE,CDR PLACE);

SYMBOLIC PROCEDURE !&REMCODE(KEEPTOP);
 BEGIN SCALAR UNUSEDLBLS;
  UNUSEDLBLS := !&UNUSEDLBLS(KEEPTOP);
  !&REMUNUSEDMAC(UNUSEDLBLS);
  WHILE (UNUSEDLBLS := !&UNUSEDLBLS(KEEPTOP)) DO !&REMUNUSEDMAC(UNUSEDLBLS);
 END;

SYMBOLIC PROCEDURE !&UNUSEDLBLS(KEEPTOP);
 BEGIN SCALAR USED,UNUSED;
 USED := NIL;
 UNUSED := LBLIST!&;
 IF KEEPTOP THEN
   <<USED := !&LABCLASS(TOPLAB!&) . USED;
     UNUSED := !&DELCLASS(TOPLAB!&,UNUSED)>>;
  FOR EACH MAC IN CODELIST!& DO
   IF CAR MAC NEQ '!*LBL THEN
    FOR EACH FLD IN CDR MAC DO
     IF EQCAR(FLD,'LABEL) AND !&CLASSMEMBER(FLD,UNUSED) THEN
      <<USED := !&LABCLASS(FLD) . USED;
        UNUSED := !&DELCLASS(FLD,UNUSED)>>;
 LBLIST!& := USED;
 RETURN UNUSED;
 END;

SYMBOLIC PROCEDURE !&CLASSMEMBER(LAB,CLASSES);
 IF NULL CLASSES THEN NIL
   ELSE LAB MEMBER CAR CLASSES OR !&CLASSMEMBER(LAB,CDR CLASSES);


SYMBOLIC PROCEDURE !&REMUNUSEDMAC(UNUSEDLABS);
 BEGIN SCALAR P,Q,R;
  CODELIST!& := P := REVERSIP CODELIST!&;
  WHILE CDR P DO
   <<Q := CDR P;
     IF CAAR Q = '!*NOOP OR
        !&TRANSFERP P AND CAAR Q NEQ '!*LBL 
	OR CAAR Q = '!*LBL AND !&CLASSMEMBER(CADAR Q,UNUSEDLABS) THEN
        RPLACD(P,CDR Q)
     ELSE P := CDR P >>;
  CODELIST!& := REVERSIP CODELIST!&;
 END;

lisp procedure !&FixLinks(); 
%
% replace LINK by LINKE where appropriate
%
if not !*NoLinkE and not FreeBound!& then
begin scalar Switched;
    for each Inst on CodeList!& do
    begin scalar SaveRest;
	if ExitT!& and first first Inst = '!*JUMP
		   and second first Inst = ExitT!&
		or first first Inst = '!*EXIT then
	<<  if first second Inst = '!*LBL then
	    <<  if first third Inst = '!*LINK then
		<<  Inst := cdr Inst;
		    SaveRest := T >> >>;
	    if first second Inst = '!*LINK then
	    <<  if second second Inst eq NAME!& and !*R2I then
		    Rplaca(rest Inst, list('!*JUMP, TopLab!&))
		else
		    Rplaca(rest Inst, '!*LINKE . '(!*FRAMESIZE)
						. rest second Inst);
	        if not SaveRest then !&DeleteMac Inst >> >>;
    end;
end;

SYMBOLIC PROCEDURE !&PEEPHOLEOPT; 
   %'peep-hole' optimization for various cases;
   BEGIN SCALAR X,Z; 
      Z := CODELIST!&; 
      WHILE Z DO 
 	 IF CAAR Z = '!*NOOP THEN !&DELETEMAC Z
          ELSE IF NOT (X := GET(CAAR Z,'OPTFN)) OR NOT APPLY(X,LIST Z)
           THEN Z := CDR Z
   END;

COMMENT Peep-hole optimization tables; 
SYMBOLIC PROCEDURE !&STOPT U; 
 IF CAADR U = '!*ALLOC AND LLNGTH!& = 1 
    AND CDDAR U = '((FRAME 1)) THEN
  <<RPLACW(U,LIST('!*PUSH,CADAR U) . CDDR U)>>
 ELSE IF CAADR U = '!*MOVE AND CAADDR U = '!*ALLOC AND LLNGTH!& = 2
    AND CDDAR U = '((FRAME 2)) AND CDDADR U = '((FRAME 1)) THEN
  <<RPLACW(U,LIST('!*PUSH,CADADR U) . LIST('!*PUSH,CADAR U) . CDDDR U)>>;

SYMBOLIC PROCEDURE !&LBLOPT U; 
   BEGIN SCALAR Z; 
      IF CADR U = '!*LBL THEN 
	<<!&DEFEQLBL(CADR U,CADR CDR U);
	  RPLACD(U,CDDR U);
          RETURN T>>;
      IF CDADR U AND EQCAR(CADADR U,'LABEL) AND !&LBLEQ(CADAR U,CADADR U) 
		THEN RETURN RPLACW(CDR U,CDDR U)
       ELSE IF CAADR U = '!*JUMP
                 AND (Z := GET(CAADDR U,'NEGJMP))
                 AND !&LBLEQ(CADAR U,CADR CADDR U)
        THEN RETURN <<Z := Z . (CADADR U . CDDR CADDR U); 
                      RPLACD(U,(Z . CDDDR U)); 
                      T>>
       ELSE RETURN NIL
   END;

SYMBOLIC PROCEDURE !&JUMPOPT U;
 IF CADAR U = EXITT!& AND LLNGTH!& = 0 THEN
   RPLACA(U,'(!*EXIT (!*FRAMESIZE)));

SYMBOLIC PROCEDURE !&FIXCHAINS();
 BEGIN SCALAR LAB;
  FOR EACH LABCODE ON CODELIST!& DO
   IF CAAR LABCODE = '!*LBL % OR CAAR LABCODE = '!*JUMP	% croaks on this one
    THEN
    <<LAB := CADAR LABCODE;
      FOR EACH JUMPCODE ON CDR LABCODE DO
         IF CAAR JUMPCODE = '!*JUMP AND CADAR JUMPCODE = LAB THEN
	     !&MOVEJUMP(LABCODE,JUMPCODE)>>
   END;

SYMBOLIC PROCEDURE !&MOVEJUMP(LABCODE,JUMPCODE);
 IF CADR LABCODE = CADR JUMPCODE THEN
  BEGIN SCALAR LAB;
   REPEAT
    <<IF CADR LABCODE = CADR JUMPCODE THEN
 	  <<JUMPCODE := CDR JUMPCODE;
	    LABCODE := CDR LABCODE>>;
      WHILE CAADR LABCODE = '!*LBL DO LABCODE := CDR LABCODE;
      WHILE CAADR JUMPCODE = '!*LBL DO JUMPCODE := CDR JUMPCODE;>>
   UNTIL NOT(CADR JUMPCODE = CADR LABCODE);
   IF CAAR LABCODE = '!*LBL THEN
	RPLACD(JUMPCODE,LIST('!*JUMP,CADR CAR LABCODE) . CDR JUMPCODE)
   ELSE
      <<LAB := !&GENLBL();
        RPLACD(JUMPCODE,LIST('!*JUMP,LAB) . CDR JUMPCODE);
        RPLACD(LABCODE,LIST('!*LBL,LAB) . CDR LABCODE)>>;
   END;


SYMBOLIC PROCEDURE !&FIXFRM(OLDSTOMAP,OLDCODE,HIGHREG); 
% Should change FIXFRM to do sliding squeeze, not reorder;
   BEGIN SCALAR LST,GAZINTA,N,NF,TOP,FRAMESUSED,R,USED,FR,P,HMAP;
      HOLEMAP!& := NIL;
% No stores were generated - frame size = 0
      N := 1; 
      GAZINTA := 1;
% Now, loop through every allocated slot in the frame
      FRAMESUSED := !&GETFRAMES(CODELIST!&,OLDCODE,NIL);
      WHILE N <= LLNGTH!& DO 
        <<USED := NIL;
          FR := !&MKFRAME N;
          FOR EACH VAR IN OLDSTOMAP DO IF CADR VAR = FR THEN USED := T;
          IF FR MEMBER FRAMESUSED THEN USED := T;
% Find out if a frame location was used.  N and GAZINTA used for squeeze
% HOLEMAP!& is an association list between old and new frame locations.
          IF USED THEN <<HOLEMAP!& := LIST(FR,!&MKFRAME GAZINTA) . HOLEMAP!&;
			 GAZINTA := GAZINTA + 1 >>;
          N := N + 1>>; 
      LLNGTH!& := GAZINTA - 1;
      %now see if we can map stack to registers;
      TOP := !&HIGHEST(CODELIST!&,OLDCODE,HIGHREG,NIL);
      IF NOT(TOP = 'ALL OR 
             FREEBOUND!& AND NOT !*USEREGFLUID) THEN
         <<HMAP := NIL;
	   NF := 0;
	   FOR EACH HOLE IN HOLEMAP!& DO
			IF TOP < LASTACTUALREG!& THEN
			<<  TOP := TOP + 1;
                            LLNGTH!& := LLNGTH!& - 1;
			    R := !&MKREG TOP;
			    REGS!& := DELASC(R,REGS!&);
			    HMAP := LIST(CAR HOLE,R) . HMAP>>
			ELSE
			<<  NF := NF + 1;
			    HMAP := LIST(CAR HOLE, !&MKFRAME NF) . HMAP >>;
	       IF NF NEQ 0 THEN LLNGTH!& := NF;
               HOLEMAP!& := HMAP;
           >>
       ELSE IF N = GAZINTA THEN RETURN NIL;
       P := CODELIST!&;
       WHILE NOT (P EQ OLDCODE) DO
        <<RPLACA(P,!&MACROSUBST(CAR P,HOLEMAP!&));
          P := CDR P>>;
END;

SYMBOLIC PROCEDURE !&GETFRAMES(CODE,OLDCODE,RES);
IF CODE EQ OLDCODE THEN RES
     ELSE !&GETFRAMES(CDR CODE,OLDCODE,!&GETFRAMES1(CDAR CODE,RES));

SYMBOLIC PROCEDURE !&GETFRAMES1(MACARGS,RES);
IF NULL MACARGS THEN RES ELSE !&GETFRAMES1(CDR MACARGS,
  !&GETFRAMES2(CAR MACARGS,RES));

SYMBOLIC PROCEDURE !&GETFRAMES2(MACARG,RES);
IF ATOM MACARG OR !&VARP MACARG OR !&CONSTP MACARG OR !&REGP MACARG THEN RES
 ELSE IF EQCAR(MACARG,'FRAME) THEN 
	IF MACARG MEMBER RES THEN RES ELSE MACARG . RES
  ELSE !&GETFRAMES1(CDR MACARG,RES);



SYMBOLIC PROCEDURE !&HIGHEST(START,STOP,HIGHREG,EXITFLAG); 
% Find the highest register used.  'ALL is returned if all are used.
  IF START EQ STOP THEN HIGHREG ELSE
    BEGIN SCALAR FN,MAC;
      MAC := CAR START;
      RETURN
        IF CAR MAC = '!*LINK OR CAR MAC = '!*LINKE AND EXITFLAG THEN
          <<FN := CADR MAC;
            IF FN = NAME!& THEN
		IF EXITFLAG THEN 
		   !&HIGHEST(CDR START,STOP,HIGHREG,EXITFLAG)
	         ELSE 'ALL
            ELSE IF (DEST!& := GET(FN,'DESTROYS)) AND !*USINGDESTROY THEN
              <<FOR EACH R IN DEST!& DO HIGHREG := MAX(HIGHREG,CADR R);
		!&HIGHEST(CDR START,STOP,HIGHREG,EXITFLAG)>>
             ELSE 'ALL>>
        ELSE IF CAR MAC = '!*LINKF OR CAR MAC = '!*LINKEF AND EXITFLAG THEN
	  'ALL
        ELSE
          !&HIGHEST(CDR START,STOP,!&HIGHEST1(HIGHREG,CDR MAC),EXITFLAG);
END;

SYMBOLIC PROCEDURE !&HIGHEST1(H,ARGS);
 BEGIN
   FOR EACH A IN ARGS DO
     H := MAX(H,!&HIGHEST2(H,A));
   RETURN H;
 END;

SYMBOLIC PROCEDURE !&HIGHEST2(H,ARG);
  IF ATOM ARG THEN H
    ELSE IF NOT ATOM CAR ARG THEN !&HIGHEST1(H,ARG)
    ELSE IF !&CONSTP ARG THEN H
    ELSE IF CAR ARG = 'REG AND NUMBERP CADR ARG THEN MAX(H,CADR ARG)
    ELSE !&HIGHEST1(H,CDR ARG);

SYMBOLIC PROCEDURE !&REFORMMACROS;
 BEGIN SCALAR FINALTRANSFORM;
  FINALTRANSFORM := LIST(LIST('(!*FRAMESIZE),LLNGTH!&));
  FOR EACH MAC ON CODELIST!& DO
   RPLACA(MAC,!&MACROSUBST(CAR MAC,FINALTRANSFORM));
  END;

SYMBOLIC PROCEDURE !&FIXLABS();
 BEGIN SCALAR TRANSFORM,U;
  TRANSFORM := NIL;
  FOR EACH LAB IN LBLIST!& DO
    FOR EACH EQLAB IN CDR LAB DO
       TRANSFORM := LIST(EQLAB,CAR LAB) . TRANSFORM;
  FOR EACH MAC ON CODELIST!& DO
    RPLACA(MAC,!&MACROSUBST(CAR MAC,TRANSFORM));
  IF U := ASSOC(EXITT!&,TRANSFORM) THEN EXITT!& := CADR U;
  IF U := ASSOC(TOPLAB!&,TRANSFORM) THEN TOPLAB!& := CADR U;
  LBLIST!& := FOR EACH LAB IN LBLIST!& COLLECT LIST CAR LAB;
  END;

SYMBOLIC PROCEDURE !&MACROSUBST(MAC,ALIST);
  CAR MAC . !&MACROSUBST1(CDR MAC,ALIST);

SYMBOLIC PROCEDURE !&MACROSUBST1(ARGS,ALIST);
  FOR EACH ARG IN ARGS COLLECT !&MACROSUBST2(ARG,ALIST);

SYMBOLIC PROCEDURE !&MACROSUBST2(ARG,ALIST);
 BEGIN SCALAR U;
  U:=ASSOC(ARG,ALIST);
  RETURN IF U THEN CADR U
          ELSE IF ATOM ARG OR FLAGP(CAR ARG,'TERMINAL) THEN ARG
	  ELSE (CAR ARG . !&MACROSUBST1(CDR ARG,ALIST));
 END;

SYMBOLIC PROCEDURE !&REMTAGS();
  FOR EACH MAC IN CODELIST!& DO !&REMTAGS1 MAC;

SYMBOLIC PROCEDURE !&REMTAGS1 MAC;
<<  IF CAR MAC = '!*JUMPON THEN RPLACD(CDDDR MAC, LIST CDDDDR MAC);
   FOR EACH MACFIELD IN CDR MAC DO !&REMTAGS2 MACFIELD >>;

SYMBOLIC PROCEDURE !&REMTAGS2 U;
   IF EQCAR(U, 'WCONST) THEN !&REMTAGS3 CADR U;

SYMBOLIC PROCEDURE !&REMTAGS3 U;
BEGIN SCALAR DOFN;
    IF ATOM U THEN RETURN NIL;
    IF DOFN := GET(CAR U, 'DOFN) THEN
       RPLACA(U, DOFN);
    !&REMTAGS4 CDR U;
END;

SYMBOLIC PROCEDURE !&REMTAGS4 U;
    FOR EACH X IN U DO !&REMTAGS3 X;

% Entry points used in setting up the system

SYMBOLIC PROCEDURE !&ONEREG U;
 FOR EACH X IN U DO PUT(X,'DESTROYS,'((REG 1)));

SYMBOLIC PROCEDURE !&TWOREG U;
 FOR EACH X IN U DO PUT(X,'DESTROYS,'((REG 1) (REG 2)));

SYMBOLIC PROCEDURE !&THREEREG U;
 FOR EACH X IN U DO PUT(X,'DESTROYS,'((REG 1) (REG 2) (REG 3)));

END;
