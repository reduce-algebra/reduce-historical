
% ===============================================================
% CREF for PSL, requires GSORT and PSL-CREFIO.RED
% Adapted from older RCREF
% MLG, 6:28am  Tuesday, 15 December 1981
% ===============================================================

% MLG 20 Dec 1982:
%  Add FOR WHILE REPEAT FOREACH to EXPAND!* list
%  Ensures that not treated as undefined functions in processing
%  May need to add some other (CATCH?)

% MLG 20 Dec 1982
%  Add DS and DN as new ANLFN types, similar to DE, DF, DM etc

%FLAG('(ANLFN CRFLAPO),'FTYPE);  % To force PUTC
%FLAG('(ANLFN CRFLAPO),'COMPILE);

CompileTime <<
macro procedure DefANLFN U;
    list('put, MkQuote cadr U, ''ANLFN, list('function, 'lambda . cddr U));

flag('(ANLFN), 'FType);
put('ANLFN, 'FunctionDefiningFunction, 'DefANLFN);
>>;

GLOBAL '(UNDEFG!* GSEEN!* BTIME!*
	EXPAND!* HAVEARGS!* NOTUSE!*
	NOLIST!* DCLGLB!*
	ENTPTS!* UNDEFNS!* SEEN!* TSEEN!*
	OP!*!*
	CLOC!* PFILES!*
	CURLIN!* PRETITL!* !*CREFTIME
	!*SAVEPROPS MAXARG!* !*CREFSUMMARY
	!*RLISP  !*CREF   !*DEFN !*MODE 
	!*GLOBALS !*ALGEBRAICS
  );

FLUID '(GLOBS!* CALLS!* LOCLS!* TOPLV!* CURFUN!* DFPRINT!*
  );

!*ALGEBRAICS:='T; % Default is normal parse of algebraic;
!*GLOBALS:='T;	% Do analyse globals;
!*RLISP:=NIL; 	% REDUCE as default;
!*SAVEPROPS:=NIL;
MAXARG!*:=15;	% Maximum args in Standard Lisp;

COMMENT  EXPAND flag on these forces expansion of MACROS;

EXPAND!*:='(
WHILE FOREACH FOR REPEAT
);

SYMBOLIC PROCEDURE STANDARDFUNCTIONS L;
  NOLIST!* := NCONC(DEFLIST(L,'ARGCOUNT),NOLIST!*);

STANDARDFUNCTIONS '(
(ABS 1) (ADD1 1) (APPEND 2) (APPLY 2) (ASSOC 2) (ATOM 1)
(CAR 1) (CDR 1) (CAAR 1) (CADR 1) (CDAR 1) (CDDR 1)
(CAAAR 1) (CAADR 1) (CADAR 1) (CADDR 1) (CDAAR 1) (CDADR 1)
(CDDAR 1) (CDDDR 1)
(CAAAAR 1) (CAAADR 1) (CAADAR 1) (CAADDR 1)
(CADAAR 1) (CADADR 1) (CADDAR 1) (CADDDR 1)
(CDAAAR 1) (CDAADR 1) (CDADAR 1) (CDADDR 1)
(CDDAAR 1) (CDDADR 1) (CDDDAR 1) (CDDDDR 1)
(CLOSE 1) (CODEP 1) (COMPRESS 1) (CONS 2) (CONSTANTP 1)
(DE 3) (DEFLIST 2) (DELETE 2) (DF 3) (DIFFERENCE 2) (DIGIT 1)
(DIVIDE 2) (DM 3) (DS 3) (DN 3)
(EJECT 0) (EQ 2) (EQN 2) (EQUAL 2) (ERROR 2) (ERRORSET 3)
(EVAL 1) (EVLIS 1) (EXPAND 2) (EXPLODE 1) (EXPT 2)

(FIX 1) (FIXP 1) (FLAG 2) (FLAGP 2) (FLOAT 1) (FLOATP 1)
(FLUID 1) (FLUIDP 1) (FUNCTION 1)
(GENSYM 0) (GET 2) (GETD 1) (GETV 2) (GLOBAL 1)
(GLOBALP 1) (GO 1) (GREATERP 2)

(IDP 1) (INTERN 1) (LENGTH 1) (LESSP 2) (LINELENGTH 1)
(LITER 1) (LPOSN 0)
(MAP 2) (MAPC 2) (MAPCAN 2) (MAPCAR 2) (MAPCON 2)
(MAPLIST 2) (MAX2 2) (MEMBER 2) (MEMQ 2)
(MINUS 1) (MINUSP 1) (MIN2 2) (MKVECT 1) (NCONC 2) (NOT 1) (NULL 1)
(NUMBERP 1) (ONEP 1) (OPEN 2)
(PAGELENGTH 1) (PAIR 2) (PAIRP 1) (PLUS2 2) (POSN 0)
(PRINC 1) (PRINT 1) (PRIN1 1) (PRIN2 1) (PROG2 2)
(PUT 3) (PUTD 3) (PUTV 3) (QUOTE 1) (QUOTIENT 2)
(RDS 1) (READ 0) (READCH 0) (REMAINDER 2) (REMD 1)
(REMFLAG 2) (REMOB 1) (REMPROP 2) (RETURN 1)
(REVERSE 1) (RPLACA 2) (RPLACD 2) (SASSOC 3) (SET 2) (SETQ 2)
(STRINGP 1) (SUBLIS 2) (SUBST 3) (SUB1 1)
(TERPRI 0) (TIMES2 2) (UNFLUID 1) (UPBV 1) (VECTORP 1) (WRS 1)
(ZEROP 1)
);

NOLIST!*:=APPEND('(AND COND LIST MAX MIN OR PLUS PROG PROG2 LAMBDA
   PROGN TIMES),NOLIST!*);

FLAG ('(PLUS TIMES AND OR PROGN MAX MIN COND PROG LAMBDA
        CASE LIST),
       'NARYARGS);

DCLGLB!*:='(!*COMP EMSG!* !*RAISE);

FLAG('(RDS DEFLIST FLAG FLUID GLOBAL REMPROP REMFLAG UNFLUID
	   SETQ CREFOFF),'EVAL);


SYMBOLIC PROCEDURE CREFON;
  BEGIN SCALAR A,OCRFIL,CRFIL;
	BTIME!*:=TIME();
	DFPRINT!* := 'REFPRINT;
	!*DEFN := T;
	IF NOT !*ALGEBRAICS THEN PUT('ALGEBRAIC,'NEWNAM,'SYMBOLIC);
	FLAG(NOLIST!*,'NOLIST);
	FLAG(EXPAND!*,'EXPAND);
	FLAG(DCLGLB!*,'DCLGLB);
%  Global lists;
	ENTPTS!*:=NIL; 	% Entry points to package;
	UNDEFNS!*:=NIL; % Functions undefined in package;
	SEEN!*:=NIL; 	% List of all encountered functions;
	TSEEN!*:=NIL;	% List of all encountered types not flagged FUNCTION;
	GSEEN!*:=NIL;	% All encountered globals;
        PFILES!*:=NIL;	% Processed files;
	UNDEFG!*:=NIL;	% Undeclared globals encountered;
	CURLIN!*:=NIL;	% Position in file(s) of current command ;
	PRETITL!*:=NIL;	% T if error or questionables found ;
% Usages in specific function under analysis;
	GLOBS!*:=NIL;	% Globals refered to in this ;
	CALLS!*:=NIL;	% Functions called by this;
	LOCLS!*:=NIL;	% Defined local variables in this ;
	TOPLV!*:=T;	% NIL if inside function body ;
	CURFUN!*:=NIL;	% Current function beeing analysed;
	OP!*!*:=NIL;	% Current op. in LAP code;
	SETPAGE("  Errors or questionables",NIL);
 END;

SYMBOLIC PROCEDURE UNDEFDCHK FN;
 IF NOT FLAGP(FN,'DEFD) THEN UNDEFNS!* := FN . UNDEFNS!*;

SYMBOLIC PROCEDURE PRINCNG U;
 PRINCN GETES U;

SYMBOLIC PROCEDURE CREFOFF;
% main call, sets up, alphabetizes and prints;
   BEGIN  SCALAR TIM,X;
	DFPRINT!* := NIL;
	!*DEFN:=NIL;
	IF NOT !*ALGEBRAICS
          THEN REMPROP('ALGEBRAIC,'NEWNAM);	%back to normal;
	TIM:=TIME()-BTIME!*;
        FOR EACH FN IN SEEN!* DO
         <<IF NULL GET(FN,'CALLEDBY) THEN ENTPTS!*:=FN . ENTPTS!*;
           UNDEFDCHK FN>>;
	TSEEN!*:=FOR EACH Z IN IDSORT TSEEN!* COLLECT
         <<REMPROP(Z,'TSEEN);
	   FOR EACH FN IN (X:=GET(Z,'FUNS)) DO
	    <<UNDEFDCHK FN; REMPROP(FN,'RCCNAM)>>;
	   Z.X>>;
        FOR EACH Z IN GSEEN!* DO
         IF GET(Z,'USEDUNBY) THEN UNDEFG!*:=Z . UNDEFG!*;
	SETPAGE("  Summary",NIL);
	NEWPAGE();
	PFILES!*:=PUNUSED("Crossreference listing for files:",
	                  FOR EACH Z IN PFILES!* COLLECT CDR Z);
	ENTPTS!*:=PUNUSED("Entry Points:",ENTPTS!*);
	UNDEFNS!*:=PUNUSED("Undefined Functions:",UNDEFNS!*);
	UNDEFG!*:=PUNUSED("Undeclared Global Variables:",UNDEFG!*);
	GSEEN!*:=PUNUSED("Global variables:",GSEEN!*);
	SEEN!*:=PUNUSED("Functions:",SEEN!*);
	FOR EACH Z IN TSEEN!* DO
	  <<RPLACD(Z,PUNUSED(LIST(CAR Z," procedures:"),CDR Z));
	    X:='!( . NCONC(EXPLODE CAR Z,LIST '!));
	    FOR EACH FN IN CDR Z DO
	     <<FN:=GETES FN; RPLACD(FN,APPEND(X,CDR FN));
	       RPLACA(FN,LENGTH CDR FN)>> >>;
	IF !*CREFSUMMARY THEN GOTO XY;
	IF !*GLOBALS AND GSEEN!* THEN
	      <<SETPAGE("  Global Variable Usage",1);
		NEWPAGE();
		FOR EACH Z IN GSEEN!* DO CREF6 Z>>;
	IF SEEN!* THEN CREF52("  Function Usage",SEEN!*);
        FOR EACH Z IN TSEEN!* DO
	   CREF52(LIST("  ",CAR Z," procedures"),CDR Z);
	SETPAGE("  Toplevel calls:",NIL);
	X:=T;
	FOR EACH Z IN PFILES!* DO
	 IF GET(Z,'CALLS) OR GET(Z,'GLOBS) THEN
	   <<IF X THEN <<NEWPAGE(); X:=NIL>>;
	     NEWLINE 0; NEWLINE 0; PRINCNG Z;
	     SPACES2 15; UNDERLINE2 (LINELENGTH(NIL)-10);
	     CREF51(Z,'CALLS,"Calls:");
	     IF !*GLOBALS THEN CREF51(Z,'GLOBS,"Globals:")>>;
  XY:	IF !*SAVEPROPS THEN GOTO XX;
	REMPROPSS(SEEN!*,'(GALL CALLS GLOBS CALLEDBY ALSOIS SAMEAS));
	REMFLAGSS(SEEN!*,'(SEEN CINTHIS DEFD));
	REMPROPSS(GSEEN!*,'(USEDBY USEDUNBY BOUNDBY SETBY));
	REMFLAGSS(GSEEN!*,'(DCLGLB GSEEN GLB2RF GLB2BD GLB2ST));
	FOR EACH Z IN TSEEN!* DO REMPROP(CAR Z,'FUNS);
        FOR EACH Z IN HAVEARGS!* DO REMPROP(Z,'ARGCOUNT);
        HAVEARGS!* := NIL;
  XX:	NEWLINE 2;
	IF NOT !*CREFTIME THEN RETURN;
	BTIME!*:=TIME()-BTIME!*;
	SETPAGE(" Timing Information",NIL);
	NEWPAGE(); NEWLINE 0;
	PRTATM " Total Time="; PRTNUM BTIME!*;
	PRTATM " (ms)";
	NEWLINE 0;
	PRTATM " Analysis Time="; PRTNUM TIM;
	NEWLINE 0;
	PRTATM " Sorting Time="; PRTNUM (BTIME!*-TIM);
	NEWLINE 0; NEWLINE 0
  END;

SYMBOLIC PROCEDURE PUNUSED(X,Y);
 IF Y THEN
  <<NEWLINE 2; PRTLST X; NEWLINE 0;
    LPRINT(Y := IDSORT Y,8); NEWLINE 0; Y>>;

SYMBOLIC PROCEDURE CREF52(X,Y);
 <<SETPAGE(X,1); NEWPAGE(); FOR EACH Z IN Y DO CREF5 Z>>;

SYMBOLIC PROCEDURE CREF5 FN;
% Print single entry;
   BEGIN SCALAR X,Y;
	NEWLINE 0; NEWLINE 0;
	PRIN1 FN; SPACES2 15; 
	Y:=GET(FN,'GALL);
	IF Y THEN <<PRIN1 CDR Y; X:=CAR Y>>
         ELSE PRIN2 "Undefined";
        SPACES2 25;
        IF FLAGP(FN,'NARYARGS) THEN PRIN2 "  Nary Args  "
         ELSE IF (Y:=GET(FN,'ARGCOUNT)) THEN
          <<PRIN2 "  "; PRIN2 Y; PRIN2 " Args  ">>;
        UNDERLINE2 (LINELENGTH(NIL)-10);
        IF X THEN
	  <<NEWLINE 15; PRTATM '!Line!:; SPACES2 27;
	    PRTNUM CDDR X; PRTATM '!/; PRTNUM CADR X;
	    PRTATM " in "; PRTATM CAR X>>;
        CREF51(FN,'CALLEDBY,"Called by:");
	CREF51(FN,'CALLS,"Calls:");
	CREF51(FN,'ALSOIS,"Is also:");
	CREF51(FN,'SAMEAS,"Same as:");
	IF !*GLOBALS THEN CREF51(FN,'GLOBS,"Globals:")
   END;

SYMBOLIC PROCEDURE CREF51(X,Y,Z);
 IF (X:=GET(X,Y)) THEN <<NEWLINE 15; PRTATM Z; LPRINT(IDSORT X,27)>>;

SYMBOLIC PROCEDURE CREF6 GLB;
% print single global usage entry;
      <<NEWLINE 0; PRIN1 GLB; SPACES2 15;
	NOTUSE!*:=T;
	CREF61(GLB,'USEDBY,"Global in:");
	CREF61(GLB,'USEDUNBY,"Undeclared:");
	CREF61(GLB,'BOUNDBY,"Bound in:");
	CREF61(GLB,'SETBY,"Set by:");
	IF NOTUSE!* THEN PRTATM "*** Not Used ***">>;

SYMBOLIC PROCEDURE CREF61(X,Y,Z);
   IF (X:=GET(X,Y)) THEN
     <<IF NOT NOTUSE!* THEN NEWLINE 15 ELSE NOTUSE!*:=NIL;
       PRTATM Z; LPRINT(IDSORT X,27)>>;

%  Analyse bodies of LISP functions for
%  functions called, and globals used, undefined
%;

SMACRO PROCEDURE ISGLOB U;
 FLAGP(U,'DCLGLB);

SMACRO PROCEDURE CHKSEEN S;
% Has this name been encountered already?;
	IF NOT FLAGP(S,'SEEN) THEN
	  <<FLAG1(S,'SEEN); SEEN!*:=S . SEEN!*>>;

SMACRO PROCEDURE GLOBREF U;
  IF NOT FLAGP(U,'GLB2RF)
   THEN <<FLAG1(U,'GLB2RF); GLOBS!*:=U . GLOBS!*>>;

SMACRO PROCEDURE ANATOM U;
% Global seen before local..ie detect extended from this;
   IF !*GLOBALS AND U AND NOT(U EQ 'T)
      AND IDP U AND NOT ASSOC(U,LOCLS!*)
     THEN GLOBREF U;

SMACRO PROCEDURE CHKGSEEN G;
 IF NOT FLAGP(G,'GSEEN) THEN <<GSEEN!*:=G . GSEEN!*;
			    FLAG1(G,'GSEEN)>>;

SYMBOLIC PROCEDURE DO!-GLOBAL L;
% Catch global defns;
% Distinguish FLUID from GLOBAL later;
   IF PAIRP(L:=QCRF CAR L) AND !*GLOBALS AND TOPLV!* THEN
     <<FOR EACH V IN L DO CHKGSEEN V; FLAG(L,'DCLGLB)>>;

PUT('GLOBAL,'ANLFN,'DO!-GLOBAL);

PUT('FLUID,'ANLFN,'DO!-GLOBAL);

SYMBOLIC ANLFN PROCEDURE UNFLUID L;
   IF PAIRP(L:=QCRF CAR L) AND !*GLOBALS AND TOPLV!* THEN
     <<FOR EACH V IN L DO CHKGSEEN V; REMFLAG(L,'DCLGLB)>>;

SYMBOLIC PROCEDURE ADD2LOCS LL;
  BEGIN SCALAR OLDLOC;
   IF !*GLOBALS THEN FOR EACH GG IN LL DO
      <<OLDLOC:=ASSOC(GG,LOCLS!*);
        IF NOT NULL OLDLOC THEN <<
           QERLINE 0;
           PRIN2 "*** Variable ";
           PRIN1 GG;
           PRIN2 " nested declaration in ";
           PRINCNG CURFUN!*;
           NEWLINE 0;
	   RPLACD(OLDLOC,NIL.OLDLOC)>>
	 ELSE LOCLS!*:=(GG . LIST NIL) . LOCLS!*;
	IF ISGLOB(GG) OR FLAGP(GG,'GLB2RF) THEN GLOBIND GG;
	IF FLAGP(GG,'SEEN) THEN
	  <<QERLINE 0;
	    PRIN2 "*** Function ";
	    PRINCNG GG;
	    PRIN2 " used as variable in ";
	    PRINCNG CURFUN!*;
	    NEWLINE 0>> >>
  END;

SYMBOLIC PROCEDURE GLOBIND GG;
  <<FLAG1(GG,'GLB2BD); GLOBREF GG>>;

SYMBOLIC PROCEDURE REMLOCS LLN;
   BEGIN SCALAR OLDLOC;
    IF !*GLOBALS THEN FOR EACH LL IN LLN DO
      <<OLDLOC:=ASSOC(LL,LOCLS!*);
	IF NULL OLDLOC THEN
	  IF GETD 'BEGIN THEN REDERR LIST(" Lvar confused",LL)
	   ELSE ERROR(0,LIST(" Lvar confused",LL));
	IF CDDR OLDLOC THEN RPLACD(OLDLOC,CDDR OLDLOC)
	 ELSE LOCLS!*:=EFFACE1(OLDLOC,LOCLS!*)>>
   END;

SYMBOLIC PROCEDURE ADD2CALLS FN;
% Update local CALLS!*;
   IF NOT(FLAGP(FN,'NOLIST) OR FLAGP(FN,'CINTHIS))
    THEN <<CALLS!*:=FN . CALLS!*; FLAG1(FN,'CINTHIS)>>;

SYMBOLIC PROCEDURE ANFORM U;
	IF ATOM U THEN ANATOM U
	 ELSE ANFORM1 U;

SYMBOLIC PROCEDURE ANFORML L;
   BEGIN
	WHILE NOT ATOM L DO <<ANFORM CAR L; L:=CDR L>>;
	IF L THEN ANATOM L
   END;

SYMBOLIC PROCEDURE ANFORM1 U;
   BEGIN SCALAR FN,X;
	FN:=CAR U; U:=CDR U;
	IF NOT ATOM FN THEN RETURN <<ANFORM1 FN; ANFORML U>>;
	IF NOT IDP FN THEN RETURN NIL
	 ELSE IF ISGLOB FN THEN <<GLOBREF FN; RETURN ANFORML U>>
         ELSE IF ASSOC(FN,LOCLS!*) THEN RETURN ANFORML U;
	ADD2CALLS FN;
	CHECKARGCOUNT(FN,LENGTH U);
	IF FLAGP(FN,'NOANL) THEN NIL
	 ELSE IF X:=GET(FN,'ANLFN) THEN APPLY(X,LIST U)
	 ELSE ANFORML U
   END;

SYMBOLIC ANLFN PROCEDURE LAMBDA U;
 <<ADD2LOCS CAR U; ANFORML CDR U; REMLOCS CAR U>>;

SYMBOLIC PROCEDURE ANLSETQ U;
 <<ANFORML U;
   IF !*GLOBALS AND FLAGP(U:=CAR U,'GLB2RF) THEN FLAG1(U,'GLB2ST)>>;

PUT('SETQ,'ANLFN,'ANLSETQ);

SYMBOLIC ANLFN PROCEDURE COND U;
 FOR EACH X IN U DO ANFORML X;

SYMBOLIC ANLFN PROCEDURE PROG U;
 <<ADD2LOCS CAR U;
   FOR EACH X IN CDR U DO
    IF NOT ATOM X THEN ANFORM1 X;
   REMLOCS CAR U>>;

SYMBOLIC ANLFN PROCEDURE FUNCTION U;
 IF PAIRP(U:=CAR U) THEN ANFORM1 U
  ELSE IF ISGLOB U THEN GLOBREF U
  ELSE IF NULL ASSOC(U,LOCLS!*) THEN ADD2CALLS U;

FLAG('(QUOTE GO),'NOANL);

SYMBOLIC ANLFN PROCEDURE ERRORSET U;
 BEGIN SCALAR FN,X;
  ANFORML CDR U;
  IF EQCAR(U:=CAR U,'QUOTE) THEN RETURN ERSANFORM CADR U
   ELSE IF NOT((EQCAR(U,'CONS) OR (X:=EQCAR(U,'LIST)))
               AND QUOTP(FN:=CADR U))
    THEN RETURN ANFORM U;
  ANFORML CDDR U;
  IF PAIRP(FN:=CADR FN) THEN ANFORM1 FN
   ELSE IF FLAGP(FN,'GLB2RF) THEN NIL
   ELSE IF ISGLOB FN THEN GLOBREF FN
   ELSE <<ADD2CALLS FN; IF X THEN CHECKARGCOUNT(FN,LENGTH CDDR U)>>
 END;

SYMBOLIC PROCEDURE ERSANFORM U;
 BEGIN SCALAR LOCLS!*;
  RETURN ANFORM U
 END;

SYMBOLIC PROCEDURE ANLMAP U;
 <<ANFORML CDR U;
   IF QUOTP(U:=CADDR U) AND IDP(U:=CADR U)
      AND NOT ISGLOBL U AND NOT ASSOC(U,LOCLS!*)
     THEN CHECKARGCOUNT(U,1)>>;

FOR EACH X IN '(MAP MAPC MAPLIST MAPCAR MAPCON MAPCAN) DO
 PUT(X,'ANLFN,'ANLMAP);

SYMBOLIC ANLFN PROCEDURE APPLY U;
 BEGIN SCALAR FN;
  ANFORML CDR U;
  IF QUOTP(FN:=CADR U) AND IDP(FN:=CADR FN) AND EQCAR(U:=CADDR U,'LIST)
    THEN CHECKARGCOUNT(FN,LENGTH CDR U)
 END;

SYMBOLIC PROCEDURE QUOTP U; EQCAR(U,'QUOTE) OR EQCAR(U,'FUNCTION);

PUT('CREF ,'SIMPFG ,'((T (CREFON)) (NIL (CREFOFF))));

SYMBOLIC PROCEDURE OUTREF(S,VARLIS,BODY,TYPE);
 BEGIN SCALAR CURFUN!*,CALLS!*,GLOBS!*,LOCLS!*,TOPLV!*,A;
  A:=IF VARLIS MEMQ '(ANP!!ATOM ANP!!IDB ANP!!EQ ANP!!UNKNOWN)
       THEN NIL
      ELSE LENGTH VARLIS;
  S := OUTRDEFUN(S,TYPE,IF A THEN A ELSE GET(BODY,'ARGCOUNT));
  IF A THEN <<ADD2LOCS VARLIS; ANFORM(BODY); REMLOCS VARLIS>>
   ELSE IF NULL BODY OR NOT IDP BODY THEN NIL
   ELSE IF VARLIS EQ 'ANP!!EQ
    THEN <<PUT(S,'SAMEAS,LIST BODY); TRAPUT(BODY,'ALSOIS,S)>>
   ELSE ADD2CALLS BODY;
  OUTREFEND S
 END;

SYMBOLIC PROCEDURE TRAPUT(U,V,W);
 BEGIN SCALAR A;
  IF A:=GET(U,V) THEN
    (IF NOT(TOPLV!* OR W MEMQ A) THEN RPLACD(A,W . CDR A))
   ELSE PUT(U,V,LIST W)
 END;

SMACRO PROCEDURE TOPUT(U,V,W);
 IF W THEN PUT(U,V,IF TOPLV!* THEN UNION(W,GET(U,V)) ELSE W);

SYMBOLIC PROCEDURE OUTREFEND S;
  <<TOPUT(S,'CALLS,CALLS!*);
    FOR EACH X IN CALLS!* DO
     <<REMFLAG1(X,'CINTHIS);
        IF NOT X EQ S THEN <<CHKSEEN X; TRAPUT(X,'CALLEDBY,S)>> >>;
    TOPUT(S,'GLOBS,GLOBS!*);
    FOR EACH X IN GLOBS!* DO
        <<TRAPUT(X,IF ISGLOB X THEN 'USEDBY
		    ELSE <<CHKGSEEN X; 'USEDUNBY>>,S);
          REMFLAG1(X,'GLB2RF);
          IF FLAGP(X,'GLB2BD)
	    THEN <<REMFLAG1(X,'GLB2BD); TRAPUT(X,'BOUNDBY,S)>>;
          IF FLAGP(X,'GLB2ST)
	    THEN <<REMFLAG1(X,'GLB2ST); TRAPUT(X,'SETBY,S)>> >> >>;

SYMBOLIC PROCEDURE RECREF(S,TYPE);
	  <<QERLINE 2;
	    PRTATM "*** Redefinition to ";
	    PRIN1 TYPE;
	    PRTATM " procedure, of:";
	    CREF5 S;
	    REMPROPSS(S,'(CALLS GLOBS SAMEAS));
	    NEWLINE 2>>;

SYMBOLIC PROCEDURE OUTRDEFUN(S,TYPE,V);
  BEGIN
    S:=QTYPNM(S,TYPE);
    IF FLAGP(S,'DEFD) THEN RECREF(S,TYPE)
     ELSE FLAG1(S,'DEFD);
    IF FLAGP(TYPE,'FUNCTION) AND (ISGLOB S OR ASSOC(S,LOCLS!*)) THEN
      <<QERLINE 0;
	PRIN2 "**** Variable ";
	PRINCNG S;
	PRIN2 " defined as function";
        NEWLINE 0>>;
    IF V AND NOT FLAGP(TYPE,'NARYARG) THEN DEFINEARGS(S,V);
    PUT(S,'GALL,CURLIN!* . TYPE);
    GLOBS!*:=NIL;
    CALLS!*:=NIL;
    RETURN CURFUN!*:=S
  END;

FLAG('(MACRO FEXPR),'NARYARG);

SYMBOLIC PROCEDURE QTYPNM(S,TYPE);
 IF FLAGP(TYPE,'FUNCTION) THEN <<CHKSEEN S; S>>
  ELSE BEGIN SCALAR X,Y,Z;
	IF (Y:=GET(TYPE,'TSEEN)) AND (X:=ATSOC(S,CDR Y))
	  THEN RETURN CDR X;
	IF NULL Y THEN
	  <<Y:=LIST ('!( . NCONC(EXPLODE TYPE,LIST '!)));
	    PUT(TYPE,'TSEEN,Y); TSEEN!* := TYPE . TSEEN!*>>;
	X := COMPRESS (Z := EXPLODE S);
	CDR Y := (S . X) . CDR Y;
	Y := APPEND(CAR Y,Z);
	PUT(X,'RCCNAM,LENGTH Y . Y);
	TRAPUT(TYPE,'FUNS,X);
	RETURN X
       END;

SYMBOLIC PROCEDURE DEFINEARGS(NAME,N);
  BEGIN SCALAR CALLEDWITH,X;
    CALLEDWITH:=GET(NAME,'ARGCOUNT);
    IF NULL CALLEDWITH THEN RETURN HASARG(NAME,N);
    IF N=CALLEDWITH THEN RETURN NIL;
    IF X := GET(NAME,'CALLEDBY) THEN INSTDOF(NAME,N,CALLEDWITH,X);
    HASARG(NAME,N)
  END;

SYMBOLIC PROCEDURE INSTDOF(NAME,N,M,FNLST);
  <<QERLINE 0;
    PRIN2 "***** ";
    PRIN1 NAME;
    PRIN2 " called with ";
    PRIN2 M;
    PRIN2 " instead of ";
    PRIN2 N;
    PRIN2 " arguments in:";
    LPRINT(IDSORT FNLST,POSN()+1);
    NEWLINE 0>>;

SYMBOLIC PROCEDURE HASARG(NAME,N);
  <<HAVEARGS!*:=NAME . HAVEARGS!*;
    IF N>MAXARG!* THEN
           <<QERLINE 0;
             PRIN2 "**** "; PRIN1 NAME;
             PRIN2 " has "; PRIN2 N;
             PRIN2 " arguments";
             NEWLINE 0 >>;
    PUT(NAME,'ARGCOUNT,N)>>;

SYMBOLIC PROCEDURE CHECKARGCOUNT(NAME,N);
  BEGIN SCALAR CORRECTN;
    IF FLAGP(NAME,'NARYARGS) THEN RETURN NIL;
    CORRECTN:=GET(NAME,'ARGCOUNT);
    IF NULL CORRECTN THEN RETURN HASARG(NAME,N);
    IF NOT CORRECTN=N THEN INSTDOF(NAME,CORRECTN,N,LIST CURFUN!*)
  END;

SYMBOLIC PROCEDURE REFPRINT U;
 BEGIN SCALAR X,Y;
  X:=IF CLOC!* THEN CAR CLOC!* ELSE "*TTYINPUT*";
  IF (CURFUN!*:=ASSOC(X,PFILES!*)) THEN
    <<X:=CAR CURFUN!*; CURFUN!*:=CDR CURFUN!*>>
   ELSE <<PFILES!*:=(X.(CURFUN!*:=GENSYM())).PFILES!*;
	  Y:=REVERSIP CDR REVERSIP CDR EXPLODE X;
	  PUT(CURFUN!*,'RCCNAM,LENGTH Y . Y)>>;
  CURLIN!*:=IF CLOC!* THEN X.CDR CLOC!* ELSE NIL;
  CALLS!*:=GLOBS!*:=LOCLS!*:=NIL;
  ANFORM U;
  OUTREFEND CURFUN!*
 END;

FLAG('(SMACRO NMACRO),'CREF);

SYMBOLIC ANLFN PROCEDURE PUT U;
 IF TOPLV!* AND QCPUTX CADR U THEN ANPUTX U
  ELSE ANFORML U;

PUT('PUTC,'ANLFN,GET('PUT,'ANLFN));

SYMBOLIC PROCEDURE QCPUTX U;
 EQCAR(U,'QUOTE) AND (FLAGP(CADR U,'CREF) OR FLAGP(CADR U,'COMPILE));

SYMBOLIC PROCEDURE ANPUTX U;
 BEGIN SCALAR NAM,TYP,BODY;
  NAM:=QCRF CAR U;
  TYP:=QCRF CADR U;
  U:=CADDR U;
  IF ATOM U THEN <<BODY:=QCRF U; U:='ANP!!ATOM>>
   ELSE IF CAR U MEMQ '(QUOTE FUNCTION) THEN
    IF EQCAR(U:=CADR U,'LAMBDA) THEN <<BODY:=CADDR U; U:=CADR U>>
     ELSE IF IDP U THEN <<BODY:=U; U:='ANP!!IDB>>
     ELSE RETURN NIL
   ELSE IF CAR U EQ 'CDR AND EQCAR(CADR U,'GETD) THEN
    <<BODY:=QCRF CADADR U; U:='ANP!!EQ>>
   ELSE IF CAR U EQ 'GET AND QCPUTX CADDR U THEN
    <<BODY:=QTYPNM(QCRF CADR U,CADR CADDR U); U:='ANP!!EQ>>
   ELSE IF CAR U EQ 'MKCODE THEN
    <<ANFORM CADR U; U:=QCRF CADDR U; BODY:=NIL>>
   ELSE <<BODY:=QCRF U; U:='ANP!!UNKNOWN>>;
  OUTREF(NAM,U,BODY,TYP)
 END;

SYMBOLIC ANLFN PROCEDURE PUTD U;
 IF TOPLV!* THEN ANPUTX U ELSE ANFORML U;

SYMBOLIC ANLFN PROCEDURE DE U;
 OUTDEFR(U,'EXPR);

SYMBOLIC ANLFN PROCEDURE DN U;
 OUTDEFR(U,'NEXPR);

SYMBOLIC ANLFN PROCEDURE DF U;
 OUTDEFR(U,'FEXPR);

SYMBOLIC ANLFN PROCEDURE DM U;
 OUTDEFR(U,'MACRO);

SYMBOLIC ANLFN PROCEDURE DS U;
 OUTDEFR(U,'SMACRO);

SYMBOLIC PROCEDURE OUTDEFR(U,TYPE);
 OUTREF(CAR U,CADR U,CADDR U,TYPE);

SYMBOLIC PROCEDURE QCRF U;
 IF NULL U OR U EQ T THEN U
  ELSE IF EQCAR(U,'QUOTE) THEN CADR U
  ELSE <<ANFORM U; COMPRESS EXPLODE '!?VALUE!?!?>>;

FLAG('(EXPR FEXPR MACRO SMACRO NMACRO),'FUNCTION);

CommentOutCode <<			% Lisp 1.6 LAP only
SYMBOLIC ANLFN PROCEDURE LAP U;
   IF PAIRP(U:=QCRF CAR U) THEN
    BEGIN SCALAR GLOBS!*,LOCLS!*,CALLS!*,CURFUN!*,TOPLV!*,X;
     WHILE U DO
      <<IF PAIRP CAR U THEN
	  IF X:=GET(OP!*!*:=CAAR U,'CRFLAPO) THEN APPLY(X,LIST U)
	   ELSE IF !*GLOBALS THEN FOR EACH Y IN CDAR U DO ANLAPEV Y;
	U:=CDR U>>;
     QOUTREFE()
    END;

SYMBOLIC CRFLAPO PROCEDURE !*ENTRY U;
 <<QOUTREFE(); U:=CDAR U; OUTRDEFUN(CAR U,CADR U,CADDR U)>>;

SYMBOLIC PROCEDURE QOUTREFE;
 BEGIN
  IF NULL CURFUN!* THEN
    IF GLOBS!* OR CALLS!* THEN
      <<CURFUN!*:=COMPRESS EXPLODE '!?LAP!?!?; CHKSEEN CURFUN!*>>
     ELSE RETURN;
  OUTREFEND CURFUN!*
 END;

SYMBOLIC CRFLAPO PROCEDURE !*LAMBIND U;
 FOR EACH X IN CADDAR U DO GLOBIND CAR X;

SYMBOLIC CRFLAPO PROCEDURE !*PROGBIND U;
 FOR EACH X IN CADAR U DO GLOBIND CAR X;

SYMBOLIC PROCEDURE LINCALL U;
 <<ADD2CALLS CAR (U:=CDAR U); CHECKARGCOUNT(CAR U,CADDR U)>>;

PUT('!*LINK,'CRFLAPO,'LINCALL);

PUT('!*LINKE,'CRFLAPO,'LINCALL);

SYMBOLIC PROCEDURE ANLAPEV U;
 IF PAIRP U THEN
   IF CAR U MEMQ '(GLOBAL FLUID) THEN
     <<U:=CADR U; GLOBREF U;
       IF FLAGP(OP!*!*,'STORE) THEN PUT(U,'GLB2ST,'T)>>
    ELSE <<ANLAPEV CAR U; ANLAPEV CDR U>>;

FLAG('(!*STORE),'STORE);

FLAG('(POP MOVEM SETZM HRRZM),'STORE);

SYMBOLIC PROCEDURE LAPCALLF U;
 BEGIN SCALAR FN;
  RETURN
   IF EQCAR(CADR (U:=CDAR U),'E) THEN
     <<ADD2CALLS(FN:=CADADR U); CHECKARGCOUNT(FN,CAR U)>>
    ELSE IF !*GLOBALS THEN ANLAPEV CADR U
 END;

PUT('JCALL,'CRFLAPO,'LAPCALLF);

PUT('CALLF,'CRFLAPO,'LAPCALLF);

PUT('JCALLF,'CRFLAPO,'LAPCALLF);

SYMBOLIC CRFLAPO PROCEDURE CALL U;
 IF NOT(CADDAR U = '(E !*LAMBIND!*)) THEN LAPCALLF U
  ELSE WHILE ((U:=CDR U) AND PAIRP CAR U AND CAAR U = 0) DO
	GLOBIND CADR CADDAR U;

>>;

SYMBOLIC PROCEDURE QERLINE U;
 IF PRETITL!* THEN NEWLINE U
  ELSE <<PRETITL!*:=T; NEWPAGE()>>;

% These functions defined to be able to run in bare LISP
% EQCAR MKQUOTE

SYMBOLIC PROCEDURE EFFACE1(U,V);
 IF NULL V THEN NIL
  ELSE IF U EQ CAR V THEN CDR V
  ELSE RPLACD(V,EFFACE1(U,CDR V));


MAXARG!*:=15;

END;
