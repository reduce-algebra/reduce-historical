COMMENT **************************************************************
**********************************************************************
                      THE STANDARD LISP COMPILER
**********************************************************************
*********************************************************************; 

COMMENT machine dependent parts are in a separate file; 

COMMENT these include the macros described below and, in addition,
	an auxiliary function !&MKFUNC which is required to pass
	functional arguments (input as FUNCTION <func>) to the
	loader. In most cases, !&MKFUNC may be defined as MKQUOTE; 

COMMENT global flags used in this compiler:

!*MODULE	indicates block compilation (a future extension of
		this compiler)
!*MSG		indicates whether certain messages should be printed
!*NOLINKE 	if ON inhibits use of !*LINKE c-macro
!*ORD		if ON forces left-to-right argument evaluation
!*PLAP		if ON causes LAP output to be printed
!*R2I		if ON causes recursion removal where possible;

GLOBAL '(!*MODULE !*MSG !*NOLINKE !*ORD !*PLAP !*R2I);

COMMENT global variables used:

ERFG!*		used by REDUCE to control error recovery
MAXNARGS	maximum number of arguments permitted;

GLOBAL '(ERFG!* MAXNARGS);

MAXNARGS := 15; 	%Standard LISP limit;

COMMENT fluid variables used:

ALSTS		alist of fluid parameters
CODELIST  	code being built
CONDTAIL 	simulated stack of position in the tail of a COND
DFPRINT!*	name of special definition process (or NIL)
EXIT		label for !*EXIT jump
FLAGG		used in !&COMTST, and in !&FIXREST
FREELST 	list of free variables with bindings
GOLIST		storage map for jump labels
IREGS		initial register contents
IREGS1  	temporary placeholder for IREGS for branch compilation
JMPLIST		list of locations in CODELIST of transfers
LBLIST		list of label words
LLNGTH		cell whose CAR is length of frame
NAME		name of function being currently compiled
NARG		number of arguments in function
REGS		known current contents of registers as an alist with 
                 elements  of form (<reg> . <contents>)
REGS1   	temporary placeholder for REGS during branch compilation
SLST		association list for stores which have not yet been used
STLST		list of active stores in function
STOMAP		storage map for variables
SWITCH		boolean expression value flag - keeps track of NULLs; 

FLUID '(ALSTS CODELIST CONDTAIL DFPRINT!* EXIT FLAGG FREELST GOLIST
	IREGS IREGS1 JMPLIST LBLIST LLNGTH NAME NARG REGS REGS1 SLST
	STLST STOMAP SWITCH);

COMMENT c-macros used in this compiler; 

COMMENT The following c-macros must NOT change regs 1-MAXNARGS:

!*ALLOC n                allocate new stack frame of n words
!*DEALLOC n              deallocate above frame
!*ENTRY name type nargs  entry point to function name of type type
                           with nargs args
!*EXIT                   exit to previously saved return address
!*STORE reg floc         store contents of reg (or NIL) in floc
!*JUMP adr               unconditional jump
!*JUMPC  adr exp type    jump to adr if exp is of type type
!*JUMPNC adr exp type    jump to adr if exp is not of type type
!*JUMPNIL adr            jump on register 1 eq to NIL
!*JUMPT adr              jump on register 1 not eq to NIL
!*JUMPE adr exp          jump on register 1 eq to exp
!*JUMPN adr exp 	 jump on register 1 not eq to exp
!*LBL adr                define label
!*LAMBIND regs alst      bind free lambda vars in alst currently in regs
!*PROGBIND alst          bind free prog vars in alst
!*FREERSTR alst          unbind free variables in alst

COMMENT the following c-macro must only change specific register
        being loaded:

!*LOAD reg exp           load exp into reg; 

COMMENT the following c-macros do not protect regs 1-MAXNARGS:

!*LINK fn type nargs     link to fn of type type with nargs args
!*LINKE fn type nargs n  link to fn of type type with nargs args
                           and exit removing frame of n words 
!*CODE list	         this macro allows for the inclusion of a list
			   of c-macro expressions (or even explicit
			   assembly language) in a function definition;

FLAG('(!*ALLOC !*DEALLOC !*ENTRY !*EXIT !*STORE !*JUMP !*JUMPC !*JUMPNC
       !*JUMPNIL !*JUMPT !*JUMPE !*JUMPN !*LBL !*LAMBIND !*PROGBIND
       !*FREERSTR !*LOAD !*LINK !*LINKE !*CODE),
'MC);

COMMENT general functions used in this compiler; 

SYMBOLIC PROCEDURE ATSOC(U,V); 
   IF NULL V THEN NIL
    ELSE IF U EQ CAAR V THEN CAR V
    ELSE ATSOC(U,CDR V);

SYMBOLIC PROCEDURE EQCAR(U,V); NOT ATOM U AND CAR U EQ V;

SYMBOLIC PROCEDURE LPRI U; 
   IF ATOM U THEN LPRI LIST U
    ELSE FOR EACH X IN U DO <<PRIN2 X; PRIN2 " ">>;

SYMBOLIC PROCEDURE LPRIE U; 
   <<LPRI ("*****" . IF ATOM U THEN LIST U ELSE U); 
     ERFG!* := T; 
     TERPRI()>>;

SYMBOLIC PROCEDURE LPRIM U; 
   IF !*MSG
     THEN <<TERPRI();
	    LPRI ("***" . IF ATOM U THEN LIST U ELSE U);
	    TERPRI()>>;

SYMBOLIC PROCEDURE MKQUOTE U; LIST('QUOTE,U);

SYMBOLIC PROCEDURE REVERSIP U; 
   BEGIN SCALAR X,Y; 
      WHILE U DO <<X := CDR U; Y := RPLACD(U,Y); U := X>>; 
      RETURN Y
   END;

SYMBOLIC PROCEDURE RPLACW(A,B); RPLACA(RPLACD(A,CDR B),CAR B);

COMMENT the following two functions are used by the CONS open
	coding. They should be defined in the interpreter if
	possible. They should only be compiled without a COMPFN
	for CONS; 

SYMBOLIC PROCEDURE NCONS U; U . NIL;

SYMBOLIC PROCEDURE XCONS(U,V); V . U;

COMMENT Top level compiling functions;

SYMBOLIC PROCEDURE COMPILE X; 
   BEGIN SCALAR EXP; 
      FOR EACH Y IN X DO
           IF NULL (EXP := GETD Y) THEN LPRIM LIST(Y,'UNDEFINED)
            ELSE COMPD(Y,CAR EXP,CDR EXP);
      RETURN X
   END;

SYMBOLIC PROCEDURE COMPD(NAME,TYPE,EXP); 
   BEGIN 
      IF NOT FLAGP(TYPE,'COMPILE)
        THEN <<LPRIM LIST("UNCOMPILABLE FUNCTION",NAME,"OF TYPE",
                          TYPE); 
               RETURN NIL>>; 
      IF NOT ATOM EXP
        THEN IF !*MODULE THEN MODCMP(NAME,TYPE,EXP)
              ELSE IF DFPRINT!*
               THEN APPLY(DFPRINT!*,
                          LIST IF TYPE EQ 'EXPR
                                 THEN 'DE . (NAME . CDR EXP)
                                ELSE IF TYPE EQ 'FEXPR
                                 THEN 'DF . (NAME . CDR EXP)
				ELSE IF TYPE EQ 'MACRO
				 THEN 'DM . (NAME . CDR EXP)
                                ELSE LIST('PUTD,MKQUOTE NAME,
                                           MKQUOTE TYPE,
                                           MKQUOTE EXP))
              ELSE BEGIN SCALAR X; 
                      IF FLAGP(TYPE,'COMPILE)
                        THEN PUT(NAME,'CFNTYPE,LIST TYPE); 
                      X := 
                       LIST('!*ENTRY,NAME,TYPE,LENGTH CADR EXP)
                         . !&COMPROC(EXP,
                                     IF FLAGP(TYPE,'COMPILE)
                                       THEN NAME); 
                      IF !*PLAP THEN FOR EACH Y IN X DO PRINT Y; 
                      LAP X; 
		      %this is the entry point to the assembler.  LAP
		      %must remove any preexisting function definition;
                      IF (X := GET(NAME,'CFNTYPE))
                           AND EQCAR(GETD NAME,CAR X)
                        THEN REMPROP(NAME,'CFNTYPE)
                   END; 
      RETURN NAME
   END;

FLAG('(EXPR FEXPR MACRO),'COMPILE);

SYMBOLIC PROCEDURE !&COMPROC(EXP,NAME); 
   %compiles a function body, returning the generated LAP;
   BEGIN SCALAR CODELIST,FLAGG,IREGS,IREGS1,JMPLIST,LBLIST,
                LLNGTH,REGS,REGS1,ALSTS,EXIT,SLST,STLST,STOMAP,
                CONDTAIL,FREELST,
                SWITCH; INTEGER NARG; 
      LLNGTH := LIST 1; 
      NARG := 0; 
      EXIT := !&GENLBL(); 
      STOMAP := '((NIL 1)); 
      CODELIST := LIST ('!*ALLOC . LLNGTH); 
      EXP := !&PASS1 EXP; 
      IF LENGTH CADR EXP>MAXNARGS
	THEN LPRIE LIST("TOO MANY ARGS FOR COMPILER IN",NAME);
      FOR EACH Z IN CADR EXP DO <<!&FRAME Z; 
                                  NARG := NARG + 1; 
                                  IF NOT NONLOCAL Z
                                    THEN IREGS := 
                                          NCONC(IREGS,
                                                LIST LIST(NARG,Z)); 
                                  REGS := 
                                   NCONC(REGS,LIST LIST(NARG,Z))>>; 
      IF NULL REGS THEN REGS := LIST (1 . NIL); 
      ALSTS := !&FREEBIND(CADR EXP,T); 
      !&PASS2 CADDR EXP; 
      !&FREERST(ALSTS,0); 
      !&PASS3(); 
      RPLACA(LLNGTH,1 - CAR LLNGTH); 
      RETURN CODELIST
   END;

SYMBOLIC PROCEDURE NONLOCAL X; 
   IF FLUIDP X THEN 'FLUID ELSE IF GLOBALP X THEN 'GLOBAL ELSE NIL;

COMMENT Pass 1 of the compiler;

SYMBOLIC PROCEDURE !&PASS1 EXP; !&PA1(EXP,NIL);

SYMBOLIC PROCEDURE !&PA1(U,VBLS); 
   BEGIN SCALAR X; 
      RETURN IF ATOM U
               THEN IF CONSTANTP U OR U MEMQ '(NIL T) THEN MKQUOTE U
                     ELSE IF U MEMQ VBLS THEN U
                     ELSE IF NONLOCAL U THEN U
                     ELSE <<MKNONLOCAL U; U>>
              ELSE IF NOT ATOM CAR U
               THEN !&PA1(CAR U,VBLS) . !&PALIS(CDR U,VBLS)
              ELSE IF X := GET(CAR U,'PA1FN) THEN APPLY(X,LIST(U,VBLS))
              ELSE IF (X := GETD CAR U)
                        AND CAR X EQ 'MACRO
                        AND NOT GET(CAR U,'COMPFN)
               THEN !&PA1(APPLY(CDR X,LIST U),VBLS)
              ELSE IF X := GET(CAR U,'CMACRO)
               THEN !&PA1(SUBLIS(PAIR(CADR X,CDR U),CADDR X),VBLS)
              ELSE IF !&CFNTYPE CAR U EQ 'FEXPR
                        AND NOT GET(CAR U,'COMPFN)
               THEN LIST(CAR U,MKQUOTE CDR U)
              ELSE IF CAR U MEMQ VBLS OR FLUIDP CAR U
               THEN LIST('APPLY,CAR U,!&PALIST(CDR U,VBLS))
              ELSE CAR U . !&PALIS(CDR U,VBLS)
   END;

SYMBOLIC PROCEDURE !&PAIDEN(U,VBLS); U;

PUT('GO,'PA1FN,'!&PAIDEN);

PUT('QUOTE,'PA1FN,'!&PAIDEN);

PUT('CODE,'PA1FN,'!&PAIDEN);

SYMBOLIC PROCEDURE !&PACOND(U,VBLS);
   'COND . FOR EACH Z IN CDR U 
               COLLECT LIST(!&PA1(CAR Z,VBLS),
                            !&PA1(!&MKPROGN CDR Z,VBLS));

PUT('COND,'PA1FN,'!&PACOND);

SYMBOLIC PROCEDURE !&PAFUNC(U,VBLS);
   IF ATOM CADR U THEN !&MKFUNC CADR U
    ELSE !&MKFUNC COMPD(!&MKNAM NAME,'EXPR,CADR U);

PUT('FUNCTION,'PA1FN,'!&PAFUNC);

SYMBOLIC PROCEDURE !&PALAMB(U,VBLS);
   'LAMBDA . LIST(CADR U,!&PA1(!&MKPROGN CDDR U,APPEND(CADR U,VBLS)));

PUT('LAMBDA,'PA1FN,'!&PALAMB);

SYMBOLIC PROCEDURE !&PALIST(U,VBLS); 'LIST . !&PALIS(U,VBLS);

SYMBOLIC PROCEDURE !&PAPROG(U,VBLS);
   'PROG . (CADR U . !&PAPROG1(CDDR U,APPEND(CADR U,VBLS)));

SYMBOLIC PROCEDURE !&PAPROG1(U,VBLS); 
   FOR EACH X IN U COLLECT IF ATOM X THEN X ELSE !&PA1(X,VBLS);

PUT('PROG,'PA1FN,'!&PAPROG);

SYMBOLIC PROCEDURE !&PALIS(U,VBLS); 
   FOR EACH X IN U COLLECT !&PA1(X,VBLS);

SYMBOLIC PROCEDURE MKNONLOCAL U; 
   <<LPRIM LIST(U,"declared fluid"); FLUID LIST U; LIST('FLUID,U)>>;

SYMBOLIC PROCEDURE !&MKNAM U; 
   %generates unique name for auxiliary function in U;
   INTERN COMPRESS APPEND(EXPLODE U,EXPLODE GENSYM());

SYMBOLIC PROCEDURE !&MKPROGN U;
   IF NULL U OR CDR U THEN 'PROGN . U ELSE CAR U;

COMMENT CMACRO definitions for some functions;

COMMENT We do not expand CAAAAR and similar functions, since fewer 
        instructions are generated without open coding; 

DEFLIST('((CAAR (LAMBDA (U) (CAR (CAR U))))
          (CADR (LAMBDA (U) (CAR (CDR U))))
          (CDAR (LAMBDA (U) (CDR (CAR U))))
          (CDDR (LAMBDA (U) (CDR (CDR U))))
          (CAAAR (LAMBDA (U) (CAR (CAR (CAR U)))))
          (CAADR (LAMBDA (U) (CAR (CAR (CDR U)))))
          (CADAR (LAMBDA (U) (CAR (CDR (CAR U)))))
          (CADDR (LAMBDA (U) (CAR (CDR (CDR U)))))
          (CDAAR (LAMBDA (U) (CDR (CAR (CAR U)))))
          (CDADR (LAMBDA (U) (CDR (CAR (CDR U)))))
          (CDDAR (LAMBDA (U) (CDR (CDR (CAR U)))))
          (CDDDR (LAMBDA (U) (CDR (CDR (CDR U)))))
          (NOT (LAMBDA (U) (NULL U)))),'CMACRO);

COMMENT Pass 2 of the compiler;

SYMBOLIC PROCEDURE !&PASS2 EXP; !&COMVAL(EXP,0);

SYMBOLIC PROCEDURE !&COMVAL(EXP,STATUS); 
   %computes code for value of EXP;
   IF !&ANYREG(EXP,NIL)
     THEN IF STATUS>1 THEN NIL ELSE !&LREG1(EXP,STATUS)
    ELSE !&COMVAL1(EXP,STOMAP,STATUS);

SYMBOLIC PROCEDURE !&COMVAL1(EXP,STOMAP,STATUS); 
   BEGIN SCALAR X; 
      IF ATOM EXP THEN IF STATUS<2 THEN !&LREG1(EXP,STATUS) ELSE NIL
       ELSE IF NOT ATOM CAR EXP
        THEN IF CAAR EXP EQ 'LAMBDA
               THEN !&COMPLY(CAR EXP,CDR EXP,STATUS)
              ELSE LPRIE LIST("INVALID FUNCTION",CAR EXP)
       ELSE IF X := GET(CAR EXP,'COMPFN) THEN APPLY(X,LIST(EXP,STATUS))
       ELSE IF !*R2I AND CAR EXP EQ NAME AND STATUS=0 AND NULL FREELST
        THEN !&COMREC(EXP,STATUS)
       ELSE IF CAR EXP EQ 'LAMBDA
	THEN LPRIE LIST("INVALID USE OF LAMBDA IN FUNCTION",NAME)
       ELSE IF CAR EXP EQ '!*CODE THEN !&ATTACH EXP
       ELSE !&CALL(CAR EXP,CDR EXP,STATUS); 
      RETURN NIL
   END;

SYMBOLIC PROCEDURE !&ANYREG(U,V); 
   %determines if U can be loaded in any register;
   %!*ORD = T means force correct order, unless safe;
   IF EQCAR(U,'QUOTE) THEN T
    ELSE (ATOM U 
	  OR IDP CAR U AND GET(CAR U,'ANYREG) AND !&ANYREG(CADR U,NIL))
           AND (NULL !*ORD OR !&ANYREGL V);

SYMBOLIC PROCEDURE !&ANYREGL U; 
   NULL U OR !&ANYREG(CAR U,NIL) AND !&ANYREGL CDR U;

SYMBOLIC PROCEDURE !&CALL(FN,ARGS,STATUS); 
   !&CALL1(FN,!&COMLIS ARGS,STATUS);

SYMBOLIC PROCEDURE !&CALL1(FN,ARGS,STATUS); 
   %ARGS is reversed list of compiled arguments of FN;
   BEGIN INTEGER ARGNO; 
      ARGNO := LENGTH ARGS; 
      !&LOADARGS(ARGS,STATUS); 
      !&ATTACH LIST('!*LINK,FN,!&CFNTYPE FN,ARGNO); 
      IF FLAGP(FN,'ONEREG) THEN REGS := (1 . NIL) . CDR REGS
       ELSE IF FLAGP(FN,'TWOREG)
        THEN REGS := (1 . NIL) . DELASC(2,CDR REGS)
       ELSE REGS := LIST (1 . NIL)
   END;

SYMBOLIC PROCEDURE DELASC(U,V); 
   IF NULL V THEN NIL
    ELSE IF U=CAAR V THEN CDR V
    ELSE CAR V . DELASC(U,CDR V);

SYMBOLIC PROCEDURE !&COMLIS EXP; 
   %returns reversed list of compiled arguments;
   BEGIN SCALAR ACUSED,Y; 
      WHILE EXP DO 
         <<IF !&ANYREG(CAR EXP,CDR EXP) THEN Y := CAR EXP . Y
            ELSE <<IF ACUSED THEN !&STORE1(); 
                   !&COMVAL1(CAR EXP,STOMAP,1); 
                   ACUSED := GENSYM(); 
                   REGS := (1 . (ACUSED . CDAR REGS)) . CDR REGS; 
                   Y := ACUSED . Y>>; 
           EXP := CDR EXP>>; 
      RETURN Y
   END;

SYMBOLIC PROCEDURE !&STORE1; %Marks contents of register 1 for storage;
   BEGIN SCALAR X; 
      X := CADAR REGS; 
      IF NULL X OR EQCAR(X,'QUOTE) THEN RETURN NIL
       ELSE IF NOT ATSOC(X,STOMAP) THEN !&FRAME X; 
      !&STORE0(X,1)
   END;

SYMBOLIC PROCEDURE !&COMPLY(FN,ARGS,STATUS); 
   BEGIN SCALAR ALSTS,VARS; INTEGER I; 
      VARS := CADR FN; 
      !&LOADARGS(!&COMLIS ARGS,1); 
      ARGS := !&REMVARL VARS; % The stores that were protected;
      I := 1; 
      FOR EACH V IN VARS DO <<!&FRAME V; 
                              REGS := !&REPASC(I,V,REGS); 
                              I := I + 1>>; 
      ALSTS := !&FREEBIND(VARS,T); %Old fluid values saved;
      I := 1; 
      FOR EACH V IN VARS DO <<IF NOT NONLOCAL V THEN !&STORE0(V,I); 
                              I := I + 1>>; 
      !&COMVAL(CADDR FN,STATUS); 
      !&FREERST(ALSTS,STATUS); 
      !&RSTVARL(VARS,ARGS)
   END;

SYMBOLIC PROCEDURE !&COMREC(EXP,STATUS); 
   BEGIN SCALAR X,Z; 
      !&LOADARGS(!&COMLIS CDR EXP,STATUS); 
      Z := CODELIST; 
      IF NULL CDR Z
        THEN LPRIE LIST("CIRCULAR DEFINITION FOR",CAR EXP); 
      WHILE CDDR Z DO Z := CDR Z; 
      IF CAAR Z EQ '!*LBL THEN X := CDAR Z
       ELSE <<X := !&GENLBL(); RPLACD(Z,LIST('!*LBL . X,CADR Z))>>; 
      !&ATTJMP X
   END;

SYMBOLIC PROCEDURE !&LOADARGS(ARGS,STATUS); 
   BEGIN INTEGER N; 
      N := LENGTH ARGS; 
      IF N>MAXNARGS THEN LPRIE LIST("TOO MANY ARGUMENTS IN",NAME); 
      IF STATUS>0 THEN !&CLRREGS(); 
      WHILE ARGS DO 
         <<!&LREG(N,CAR ARGS,CDR ARGS,STATUS); 
           N := N - 1; 
           ARGS := CDR ARGS>>
   END;

SYMBOLIC PROCEDURE !&LOCATE X; 
   BEGIN SCALAR Y,VTYPE; 
      IF EQCAR(X,'QUOTE) THEN RETURN LIST X
       ELSE IF Y := !&RASSOC(X,REGS) THEN RETURN LIST CAR Y
       ELSE IF NOT ATOM X THEN RETURN LIST (CAR X . !&LOCATE CADR X)
       ELSE IF VTYPE := NONLOCAL X THEN RETURN LIST LIST(VTYPE,X); 
      WHILE Y := ATSOC(X,SLST) DO SLST := DELETE(Y,SLST); 
      RETURN IF Y := ATSOC(X,STOMAP) THEN CDR Y ELSE LIST MKNONLOCAL X
   END;

SYMBOLIC PROCEDURE !&LREG(REG,U,V,STATUS); 
   BEGIN SCALAR X,Y; 
      IF (X := ASSOC(REG,REGS)) AND U MEMBER CDR X THEN RETURN NIL
       ELSE IF (Y := ASSOC(REG,IREGS))
                 AND (STATUS>0 OR !&MEMLIS(CADR Y,V))
        THEN <<!&STORE0(CADR Y,REG); IREGS := DELETE(Y,IREGS)>>; 
      !&ATTACH ('!*LOAD . (REG . !&LOCATE U)); 
      REGS := !&REPASC(REG,U,REGS)
   END;

SYMBOLIC PROCEDURE !&LREG1(X,STATUS); !&LREG(1,X,NIL,STATUS);

COMMENT Functions for handling non-local variables; 

SYMBOLIC PROCEDURE !&FREEBIND(VARS,LAMBP); 
   %bind FLUID variables in lambda or prog lists;
   %LAMBP is true for LAMBDA, false for PROG;
   BEGIN SCALAR FALST,FREGS,X,Y; INTEGER I; 
      I := 1; 
      FOR EACH X IN VARS DO <<IF FLUIDP X
                                THEN <<FALST := 
                                        (X . !&GETFFRM X) . FALST; 
                                       FREGS := I . FREGS>>
                               ELSE IF GLOBALP X
                                THEN LPRIE LIST("CANNOT BIND GLOBAL ",
                                                X); 
                              I := I + 1>>; 
      IF NULL FALST THEN RETURN NIL; 
      IF LAMBP THEN !&ATTACH LIST('!*LAMBIND,FREGS,FALST)
       ELSE !&ATTACH LIST('!*PROGBIND,FALST); 
      RETURN FALST
   END;

SYMBOLIC PROCEDURE !&FREERST(ALSTS,STATUS); %restores FLUID variables;
   IF ALSTS THEN !&ATTACH LIST('!*FREERSTR,ALSTS);

SYMBOLIC PROCEDURE !&ATTACH U; CODELIST := U . CODELIST;

SYMBOLIC PROCEDURE !&STORE0(U,REG); 
   %marks expression U in register REG for storage;
   BEGIN SCALAR X; 
      X := '!*STORE . (REG . !&GETFRM U); 
      STLST := X . STLST; 
      !&ATTACH X; 
      IF ATOM U
        THEN <<!&CLRSTR U; SLST := (U . CODELIST) . SLST>>
   END;

SYMBOLIC PROCEDURE !&CLRSTR VAR; %removes unneeded stores;
   BEGIN SCALAR X; 
      IF CONDTAIL THEN RETURN NIL; 
      X := ATSOC(VAR,SLST); 
      IF NULL X THEN RETURN NIL; 
      STLST := !&DELEQ(CADR X,STLST); 
      SLST := !&DELEQ(X,SLST); 
      RPLACA(CADR X,'!*NOOP)
   END;

COMMENT Functions for general tests; 

SYMBOLIC PROCEDURE !&COMTST(EXP,LABL); 
   %compiles boolean expression EXP.
   %If EXP has the same value as SWITCH then branch to LABL,
   %otherwise fall through;
   %REGS/IREGS are active registers for fall through,
   %REGS1/IREGS1 for branch;
   BEGIN SCALAR X; 
      WHILE EQCAR(EXP,'NULL) DO 
         <<SWITCH := NOT SWITCH; EXP := CADR EXP>>; 
      IF NOT ATOM EXP AND ATOM CAR EXP AND (X := GET(CAR EXP,'COMTST))
        THEN APPLY(X,LIST(EXP,LABL))
       ELSE <<IF EXP='(QUOTE T)
                THEN IF SWITCH THEN !&ATTJMP LABL ELSE FLAGG := T
               ELSE <<!&COMVAL(EXP,1); 
                      !&ATTACH LIST(IF SWITCH THEN '!*JUMPT
                                     ELSE '!*JUMPNIL,CAR LABL); 
                      !&ADDJMP CODELIST>>; 
              REGS1 := REGS; 
              IREGS1 := IREGS>>; 
      IF EQCAR(CAR CODELIST,'!*JUMPT)
        THEN REGS := (1 . ('(QUOTE NIL) . CDAR REGS)) . CDR REGS
       ELSE IF EQCAR(CAR CODELIST,'!*JUMPNIL)
        THEN REGS1 := (1 . ('(QUOTE NIL) . CDAR REGS1)) . CDR REGS1
   END;

COMMENT Specific function open coding; 

SYMBOLIC PROCEDURE !&COMANDOR(EXP,STATUS); 
   BEGIN SCALAR FN,LABL,IREGSL,REGSL; 
      FN := CAR EXP EQ 'AND; 
      LABL := !&GENLBL(); 
      IF STATUS>1
        THEN BEGIN SCALAR REGS1; 
                !&TSTANDOR(EXP,LABL); 
                REGS := !&RMERGE2(REGS,REGS1)
             END
       ELSE BEGIN 
               IF STATUS>0 THEN !&CLRREGS(); 
               EXP := CDR EXP; 
               WHILE EXP DO 
                  <<!&COMVAL(CAR EXP,IF CDR EXP THEN 1 ELSE STATUS); 
                       %to allow for recursion on last entry;
                    IREGSL := IREGS . IREGSL; 
                    REGSL := REGS . REGSL; 
                    IF CDR EXP
                      THEN <<!&ATTACH LIST(IF FN THEN '!*JUMPNIL
                                            ELSE '!*JUMPT,CAR LABL); 
                             !&ADDJMP CODELIST>>; 
                    EXP := CDR EXP>>; 
               IREGS := !&RMERGE IREGSL; 
               REGS := !&RMERGE REGSL
            END; 
      !&ATTLBL LABL
   END;

SYMBOLIC PROCEDURE !&TSTANDOR(EXP,LABL); 
   BEGIN SCALAR FLG,FLG1,FN,LAB2,REGSL,REGS1L,TAILP; 
      %FLG is initial switch condition;
      %FN is appropriate AND/OR case;
      %FLG1 determines appropriate switching state;
      FLG := SWITCH; 
      SWITCH := NIL; 
      FN := CAR EXP EQ 'AND; 
      FLG1 := FLG EQ FN; 
      EXP := CDR EXP; 
      LAB2 := !&GENLBL(); 
      !&CLRREGS(); 
      WHILE EXP DO 
         <<SWITCH := NIL; 
           IF NULL CDR EXP AND FLG1
             THEN <<IF FN THEN SWITCH := T; 
                    !&COMTST(CAR EXP,LABL); 
                    REGSL := REGS . REGSL; 
                    REGS1L := REGS1 . REGS1L>>
            ELSE <<IF NOT FN THEN SWITCH := T; 
                   IF FLG1
                     THEN <<!&COMTST(CAR EXP,LAB2); 
                            REGSL := REGS1 . REGSL; 
                            REGS1L := REGS . REGS1L>>
                    ELSE <<!&COMTST(CAR EXP,LABL); 
                           REGSL := REGS . REGSL; 
                           REGS1L := REGS1 . REGS1L>>>>; 
           IF NULL TAILP
             THEN <<CONDTAIL := NIL . CONDTAIL; TAILP := T>>; 
           EXP := CDR EXP>>; 
      !&ATTLBL LAB2; 
      REGS := IF NOT FLG1 THEN CAR REGSL ELSE !&RMERGE REGSL; 
      REGS1 := IF FLG1 THEN CAR REGS1L ELSE !&RMERGE REGS1L; 
      IF TAILP THEN CONDTAIL := CDR CONDTAIL; 
      SWITCH := FLG
   END;

PUT('AND,'COMPFN,'!&COMANDOR);

PUT('OR,'COMPFN,'!&COMANDOR);

PUT('AND,'COMTST,'!&TSTANDOR);

PUT('OR,'COMTST,'!&TSTANDOR);

SYMBOLIC PROCEDURE !&COMCOND(EXP,STATUS); 
   %compiles conditional expressions;
   %registers REGS and IREGS are set for dropping through,
   %REGS1 and IREGS1 are set for a branch;
   BEGIN SCALAR IREGS1,REGS1,FLAGG,SWITCH,LAB1,LAB2,REGSL,IREGSL,TAILP; 
      EXP := CDR EXP; 
      LAB1 := !&GENLBL(); 
      IF STATUS>0 THEN !&CLRREGS(); 
      FOR EACH X IN EXP DO <<LAB2 := !&GENLBL(); 
                             SWITCH := NIL; 
                             IF CDR X THEN !&COMTST(CAR X,LAB2)
					 %update CONDTAIL;
			      ELSE <<!&COMVAL(CAR X,1);
				     !&ATTACH LIST('!*JUMPNIL,CAR LAB2);
				     !&ADDJMP CODELIST;
				     IREGS1 := IREGS;
				     REGS1 := (1 . '(QUOTE NIL) .
						CDAR REGS) . CDR REGS>>;
                             IF NULL TAILP
                               THEN <<CONDTAIL := NIL . CONDTAIL; 
                                      TAILP := T>>; 
                             !&COMVAL(CADR X,STATUS); 
                                % Branch code;
                                %test if need jump to LAB1;
                             IF NOT !&TRANSFERP CAR CODELIST
                               THEN <<!&ATTJMP LAB1; 
                                      IREGSL := IREGS . IREGSL; 
                                      REGSL := REGS . REGSL>>; 
                             REGS := REGS1; 
            %restore register status for next iteration;
         IREGS := IREGS1; 
         IREGS1 := NIL; 
            %we do not need to set REGS1 to NIL since all !&COMTSTs
            %are required to set it;
         !&ATTLBL LAB2>>; 
      IF NULL FLAGG AND STATUS<2
        THEN <<!&LREG1('(QUOTE NIL),STATUS); 
               IREGS := !&RMERGE1(IREGS,IREGSL); 
               REGS := !&RMERGE1(REGS,REGSL)>>
       ELSE IF REGSL
        THEN <<IREGS := !&RMERGE1(IREGS,IREGSL); 
               REGS := !&RMERGE1(REGS,REGSL)>>; 
      !&ATTLBL LAB1; 
      IF TAILP THEN CONDTAIL := CDR CONDTAIL
   END;

SYMBOLIC PROCEDURE !&RMERGE U; 
   IF NULL U THEN NIL ELSE !&RMERGE1(CAR U,CDR U);

SYMBOLIC PROCEDURE !&RMERGE1(U,V); 
   IF NULL V THEN U ELSE !&RMERGE1(!&RMERGE2(U,CAR V),CDR V);

SYMBOLIC PROCEDURE !&RMERGE2(U,V); 
   IF NULL U OR NULL V THEN NIL
    ELSE (LAMBDA X; 
             IF X
               THEN (CAAR U . XN(CDAR U,CDR X))
                      . !&RMERGE2(CDR U,DELETE(X,V))
              ELSE !&RMERGE2(CDR U,V))
       ASSOC(CAAR U,V);

FLAG('(!*JUMP !*LINKE ERROR),'TRANSFER);

PUT('COND,'COMPFN,'!&COMCOND);

SYMBOLIC PROCEDURE !&COMCONS(EXP,STATUS); 
   IF NULL (EXP := CDR EXP) OR NULL CDR EXP OR CDDR EXP
     THEN LPRIE "MISMATCH OF ARGUMENTS"
    ELSE IF CADR EXP='(QUOTE NIL)
     THEN !&CALL('NCONS,LIST CAR EXP,STATUS)
    ELSE IF EQCAR(!&RASSOC(CADR EXP,REGS),1)
	AND !&ANYREG(CAR EXP,NIL)
     THEN !&CALL1('XCONS,!&COMLIS REVERSE EXP,STATUS)
    ELSE IF !&ANYREG(CADR EXP,NIL) THEN !&CALL('CONS,EXP,STATUS)
    ELSE !&CALL1('XCONS,REVERSIP !&COMLIS EXP,STATUS);

PUT('CONS,'COMPFN,'!&COMCONS);

SYMBOLIC PROCEDURE !&COMGO(EXP,STATUS); 
   <<!&CLRREGS(); 
     IF STATUS>2 THEN <<!&ATTJMP !&GETLBL CADR EXP; SLST := NIL>>
      ELSE LPRIE LIST(EXP,"INVALID")>>;

PUT('GO,'COMPFN,'!&COMGO);

SYMBOLIC PROCEDURE !&COMLIST(EXP,STATUS); 
   %we only support explicit functions up to 5 arguments here;
   BEGIN SCALAR M,N,FN; 
      EXP := CDR EXP; 
      M := MIN(MAXNARGS,5); 
      N := LENGTH EXP; 
      IF N=0 THEN !&LREG1('(QUOTE NIL),STATUS)
       ELSE IF N>M THEN !&COMVAL(!&COMLIST1 EXP,STATUS)
       ELSE !&CALL(IF N=1 THEN 'NCONS
                    ELSE IF N=2 THEN 'LIST2
                    ELSE IF N=3 THEN 'LIST3
                    ELSE IF N=4 THEN 'LIST4
                    ELSE 'LIST5,EXP,STATUS)
   END;

SYMBOLIC PROCEDURE LIST2(U,V); U . (V . NIL);

SYMBOLIC PROCEDURE LIST3(U,V,W); U . (V . (W . NIL));

SYMBOLIC PROCEDURE LIST4(U,V,W,X); U . (V . (W . (X . NIL)));

SYMBOLIC PROCEDURE LIST5(U,V,W,X,Y); U . (V . (W . (X . (Y . NIL))));

SYMBOLIC PROCEDURE !&COMLIST1 EXP; 
   IF NULL EXP THEN '(QUOTE NIL)
    ELSE LIST('CONS,CAR EXP,'LIST . CDR EXP);

PUT('LIST,'COMPFN,'!&COMLIST);

SYMBOLIC PROCEDURE !&PAMAP(U,VARS); 
   IF EQCAR(CADDR U,'FUNCTION)
     THEN (LAMBDA X; 
              LIST(CAR U,
                   !&PA1(CADR U,VARS),
                   MKQUOTE (IF ATOM X THEN X ELSE !&PA1(X,VARS))))
       CADR CADDR U
    ELSE CAR U . !&PALIS(CDR U,VARS);

PUT('MAP,'PA1FN,'!&PAMAP);

PUT('MAPC,'PA1FN,'!&PAMAP);

PUT('MAPCAN,'PA1FN,'!&PAMAP);

PUT('MAPCAR,'PA1FN,'!&PAMAP);

PUT('MAPCON,'PA1FN,'!&PAMAP);

PUT('MAPLIST,'PA1FN,'!&PAMAP);

SYMBOLIC PROCEDURE !&COMMAP(EXP,STATUS); 
   BEGIN SCALAR BODY,FN,LAB1,LAB2,LAB3,TMP,MTYPE,RESULT,SLST1,VAR,X; 
      BODY := CADR EXP; 
      FN := CADDR EXP; 
      LAB1 := !&GENLBL(); 
      LAB2 := !&GENLBL(); 
      MTYPE := 
       IF CAR EXP MEMQ '(MAPCAR MAPLIST) THEN 'CONS
        ELSE IF CAR EXP MEMQ '(MAPCAN MAPCON)
	       THEN <<LAB3 := !&GENLBL(); 'NCONC>>
        ELSE NIL; 
      !&CLRREGS(); 
      IF MTYPE THEN <<!&FRAME (RESULT := GENSYM());
		      IF NULL LAB3 THEN !&STORE0(RESULT,NIL)>>;
      !&FRAME (VAR := GENSYM()); 
      !&COMVAL(BODY,1); 
      REGS := LIST LIST(1,VAR); 
      IF LAB3 THEN <<!&STORE0(VAR,1); !&FRAME (TMP := GENSYM());
                     !&COMVAL('(NCONS 'NIL),1);
                     !&STORE0(RESULT,1); !&STORE0(TMP,1);
                     !&LREG1(VAR,1)>>;
      !&ATTJMP LAB2;
      !&ATTLBL LAB1; 
      !&STORE0(VAR,1); 
      X := IF CAR EXP MEMQ '(MAP MAPCON MAPLIST) THEN VAR
            ELSE LIST('CAR,VAR);
      IF EQCAR(FN,'QUOTE) THEN FN := CADR FN; 
      SLST1 := SLST; %to allow for store in function body;
      !&COMVAL(LIST(FN,X),IF MTYPE THEN 1 ELSE 3); 
      IF MTYPE
	THEN <<IF LAB3 THEN <<!&ATTACH LIST('!*JUMPNIL,CAR LAB3);
			      !&ADDJMP CODELIST;
			      !&ATTACH '(!*LOAD 2 1);
			      !&LREG1(TMP,1);
			      !&STORE0(TMP,2);
			      !&ATTACH '(!*LINK NCONC EXPR 2);
			      !&ATTLBL LAB3>>
                ELSE <<!&LREG(2,RESULT,NIL,1); 
                       !&ATTACH '(!*LINK CONS EXPR 2); 
                       !&STORE0(RESULT,1)>>; 
               REGS := LIST (1 . NIL)>>; 
      SLST := XN(SLST,SLST1); 
      !&COMVAL(LIST('CDR,VAR),1); 
      !&ATTLBL LAB2; 
      !&ATTACH LIST('!*JUMPT,CAR LAB1); 
      !&ADDJMP CODELIST; 
      IF MTYPE
        THEN !&COMVAL(LIST(IF LAB3 THEN 'CDR ELSE 'REVERSIP,RESULT),1)
       ELSE REGS := LIST LIST(1,MKQUOTE NIL)
   END;

SYMBOLIC PROCEDURE XN(U,V); 
   IF NULL U THEN NIL
    ELSE IF CAR U MEMBER V THEN CAR U . XN(CDR U,DELETE(CAR U,V))
    ELSE XN(CDR U,V);

PUT('MAP,'COMPFN,'!&COMMAP);

PUT('MAPC,'COMPFN,'!&COMMAP);

PUT('MAPCAN,'COMPFN,'!&COMMAP);

PUT('MAPCAR,'COMPFN,'!&COMMAP);

PUT('MAPCON,'COMPFN,'!&COMMAP);

PUT('MAPLIST,'COMPFN,'!&COMMAP);

SYMBOLIC PROCEDURE !&COMPROG(EXP,STATUS); %compiles program blocks;
   BEGIN SCALAR ALSTS,GOLIST,PG,PROGLIS,EXIT; INTEGER I; 
      PROGLIS := CADR EXP; 
      EXP := CDDR EXP; 
      EXIT := !&GENLBL(); 
      PG := !&REMVARL PROGLIS; %protect prog variables;
      FOR EACH X IN PROGLIS DO !&FRAME X; 
      ALSTS := !&FREEBIND(PROGLIS,NIL); 
      FOR EACH X IN PROGLIS DO IF NOT NONLOCAL X THEN !&STORE0(X,NIL); 
      FOR EACH X IN EXP DO IF ATOM X
                             THEN GOLIST := (X . !&GENLBL()) . GOLIST; 
      WHILE EXP DO 
         <<IF ATOM CAR EXP
             THEN <<!&CLRREGS(); 
                    !&ATTLBL !&GETLBL CAR EXP; 
                    REGS := LIST (1 . NIL)>>
            ELSE !&COMVAL(CAR EXP,IF STATUS>2 THEN 4 ELSE 3); 
           IF NULL CDR EXP
                AND STATUS<2
                AND (ATOM CAR EXP OR NOT CAAR EXP MEMQ '(GO RETURN))
             THEN EXP := LIST '(RETURN (QUOTE NIL))
            ELSE EXP := CDR EXP>>; 
      !&ATTLBL EXIT; 
      IF CDR !&FINDLBL EXIT THEN REGS := LIST (1 . NIL); 
      !&FREERST(ALSTS,STATUS); 
      !&RSTVARL(PROGLIS,PG)
   END;

PUT('PROG,'COMPFN,'!&COMPROG);

SYMBOLIC PROCEDURE !&REMVARL VARS; 
   FOR EACH X IN VARS COLLECT !&REMVAR X;

SYMBOLIC PROCEDURE !&REMVAR X; 
   %removes references to variable X from IREGS and REGS
   %and protects SLST;
   <<!&REMSTORES X; !&PROTECT X>>;

SYMBOLIC PROCEDURE !&REMSTORES X;
   BEGIN 
      FOR EACH Y IN IREGS DO IF X EQ CADR Y
                               THEN <<!&STORE0(CADR Y,CAR Y); 
                                      IREGS := DELETE(Y,IREGS)>>; 
      FOR EACH Y IN REGS DO WHILE X MEMBER CDR Y DO 
                               RPLACD(Y,!&DELEQ(X,CDR Y)) 
   END;

SYMBOLIC PROCEDURE !&PROTECT U; 
   BEGIN SCALAR X; 
      IF X := ATSOC(U,SLST) THEN SLST := !&DELEQ(X,SLST); 
      RETURN X
   END;

SYMBOLIC PROCEDURE !&RSTVARL(VARS,LST); 
   FOR EACH X IN VARS DO
     <<!&REMSTORES X; !&CLRSTR X; !&UNPROTECT CAR LST; LST := CDR LST>>;

SYMBOLIC PROCEDURE !&UNPROTECT VAL; %restores VAL to SLST;
   IF VAL THEN SLST := VAL . SLST;

SYMBOLIC PROCEDURE !&COMPROGN(EXP,STATUS); 
   BEGIN 
      EXP := CDR EXP; 
      IF NULL EXP THEN RETURN NIL;
      WHILE CDR EXP DO 
         <<!&COMVAL(CAR EXP,IF STATUS<2 THEN 2 ELSE STATUS); 
           EXP := CDR EXP>>; 
      !&COMVAL(CAR EXP,STATUS)
   END;

PUT('PROG2,'COMPFN,'!&COMPROGN);

PUT('PROGN,'COMPFN,'!&COMPROGN);

SYMBOLIC PROCEDURE !&COMRETURN(EXP,STATUS); 
   <<IF STATUS<4 OR NOT !&ANYREG(CADR EXP,NIL)
       THEN !&LREG1(CAR !&COMLIS LIST CADR EXP,STATUS); 
     !&ATTJMP EXIT>>;

PUT('RETURN,'COMPFN,'!&COMRETURN);

SYMBOLIC PROCEDURE !&COMSETQ(EXP,STATUS); 
   BEGIN SCALAR X; 
      EXP := CDR EXP; 
      IF STATUS>1 AND (NULL CADR EXP OR CADR EXP='(QUOTE NIL))
        THEN !&STORE2(CAR EXP,NIL)
       ELSE <<!&COMVAL(CADR EXP,1); 
              !&STORE2(CAR EXP,1); 
              IF X := !&RASSOC(CAR EXP,IREGS)
                THEN IREGS := DELETE(X,IREGS); 
              REGS := (1 . (CAR EXP . CDAR REGS)) . CDR REGS>>
   END;

SYMBOLIC PROCEDURE !&REMSETVAR(U,V); 
   %removes references to SETQ variable U from regs list V;
   IF NULL V THEN NIL
    ELSE (CAAR V . !&REMS1(U,CDAR V)) . !&REMSETVAR(U,CDR V);

SYMBOLIC PROCEDURE !&REMS1(U,V); 
   %removes references to SETQ variable U from list V;
   IF NULL V THEN NIL
    ELSE IF SMEMQ(U,CAR V) THEN !&REMS1(U,CDR V)
    ELSE CAR V . !&REMS1(U,CDR V);

SYMBOLIC PROCEDURE SMEMQ(U,V); 
   %true if atom U is a member of V at any level (excluding
   %quoted expressions);
   IF ATOM V THEN U EQ V
    ELSE IF CAR V EQ 'QUOTE THEN NIL
    ELSE SMEMQ(U,CAR V) OR SMEMQ(U,CDR V);

SYMBOLIC PROCEDURE !&STORE2(U,V); 
   BEGIN SCALAR VTYPE; 
      REGS := !&REMSETVAR(U,REGS); 
      IF VTYPE := NONLOCAL U
        THEN !&ATTACH LIST('!*STORE,V,LIST(VTYPE,U))
       ELSE IF NOT ATSOC(U,STOMAP)
        THEN !&ATTACH LIST('!*STORE,V,MKNONLOCAL U)
       ELSE !&STORE0(U,V)
   END;

PUT('SETQ,'COMPFN,'!&COMSETQ);

COMMENT Specific test open coding; 

SYMBOLIC PROCEDURE !&COMEQ(EXP,LABL); 
   BEGIN SCALAR U,V,W; 
      U := CADR EXP; 
      V := CADDR EXP; 
      IF U MEMBER CDAR REGS THEN W := !&COMEQ1(V,U)
       ELSE IF V MEMBER CDAR REGS THEN W := !&COMEQ1(U,V)
       ELSE IF !&ANYREG(V,NIL) THEN <<!&COMVAL(U,1); W := !&LOCATE V>>
       ELSE IF !&ANYREG(U,LIST V)
        THEN <<!&COMVAL(V,1); W := !&LOCATE U>>
       ELSE <<U := !&COMLIS CDR EXP; W := !&LOCATE CADR U>>; 
      !&ATTACH ((IF SWITCH THEN '!*JUMPE ELSE '!*JUMPN)
                  . (CAR LABL . W)); 
      IREGS1 := IREGS; 
      REGS1 := REGS; 
      !&ADDJMP CODELIST
   END;

SYMBOLIC PROCEDURE !&COMEQ1(U,V); 
   IF !&ANYREG(U,LIST V) THEN !&LOCATE U
    ELSE <<!&COMVAL(U,1); !&LOCATE V>>;

PUT('EQ,'COMTST,'!&COMEQ);

SYMBOLIC PROCEDURE !&TESTFN(EXP,LABL);
   %generates c-macros !*JUMPC and !*JUMPNC;
   BEGIN SCALAR X; 
      IF NOT (X := !&RASSOC(CADR EXP,REGS)) THEN !&COMVAL(CADR EXP,1); 
      !&CLRREGS(); 
      !&ATTACH LIST(IF SWITCH THEN '!*JUMPC ELSE '!*JUMPNC,
                    CAR LABL,
                    IF X THEN CAR X ELSE 1,CAR EXP); 
      REGS1 := REGS; 
      !&ADDJMP CODELIST
   END;

COMMENT Support functions; 

SYMBOLIC PROCEDURE !&MEMLIS(U,V); 
   V AND (!&MEMB(U,CAR V) OR !&MEMLIS(U,CDR V));

SYMBOLIC PROCEDURE !&MEMB(U,V); 
   IF ATOM V THEN U EQ V ELSE !&MEMB(U,CADR V);

SYMBOLIC PROCEDURE !&RASSOC(U,V); 
   IF NULL V THEN NIL
    ELSE IF U MEMBER CDAR V THEN CAR V
    ELSE !&RASSOC(U,CDR V);

SYMBOLIC PROCEDURE !&REPASC(REG,U,V); 
   IF NULL V THEN LIST LIST(REG,U)
    ELSE IF REG=CAAR V THEN LIST(REG,U) . CDR V
    ELSE CAR V . !&REPASC(REG,U,CDR V);

SYMBOLIC PROCEDURE !&CLRREGS; %store deferred values in IREGS;
   WHILE IREGS DO 
      <<!&STORE0(CADAR IREGS,CAAR IREGS); IREGS := CDR IREGS>>;

SYMBOLIC PROCEDURE !&CFNTYPE FN; 
   BEGIN SCALAR X; 
      RETURN IF NOT ATOM FN THEN 'EXPR
	      ELSE IF X := GET(FN,'CFNTYPE) THEN CAR X
              ELSE IF X := GETD FN THEN CAR X
              ELSE 'EXPR
   END;

SYMBOLIC PROCEDURE !&GENLBL; 
   BEGIN SCALAR L; 
      L := GENSYM(); 
      LBLIST := LIST L . LBLIST; 
      RETURN LIST L
   END;

SYMBOLIC PROCEDURE !&GETLBL LABL; 
   BEGIN SCALAR X; 
      X := ATSOC(LABL,GOLIST); 
      IF NULL X THEN LPRIE LIST(LABL," - MISSING LABEL -"); 
      RETURN CDR X
   END;

SYMBOLIC PROCEDURE !&FINDLBL LBLST; ASSOC(CAR LBLST,LBLIST);

SYMBOLIC PROCEDURE !&RECHAIN(OLBL,NLBL); 
   % Fix OLBL to now point at NLBL;
   BEGIN SCALAR X,Y,USES; 
      X := !&FINDLBL OLBL; 
      Y := !&FINDLBL NLBL; 
      RPLACA(OLBL,CAR NLBL); % FIX L VAR;
      USES := CDR X; % OLD USES;
      RPLACD(X,NIL); 
      RPLACD(Y,APPEND(USES,CDR Y)); 
      FOR EACH X IN USES DO RPLACA(CDR X,CAR NLBL)
   END;

SYMBOLIC PROCEDURE !&MOVEUP U; 
   IF CAADR U EQ '!*JUMP
     THEN <<JMPLIST := !&DELEQ(CDR U,JMPLIST); 
            RPLACW(U,CDR U); 
            JMPLIST := U . JMPLIST>>
    ELSE RPLACW(U,CDR U);

SYMBOLIC PROCEDURE !&ATTLBL LBL; 
   IF CAAR CODELIST EQ '!*LBL THEN !&RECHAIN(LBL,CDAR CODELIST)
    ELSE !&ATTACH ('!*LBL . LBL);

SYMBOLIC PROCEDURE !&ATTJMP LBL; 
   BEGIN 
      IF CAAR CODELIST EQ '!*LBL
        THEN <<!&RECHAIN(CDAR CODELIST,LBL); 
               CODELIST := CDR CODELIST>>; 
      IF !&TRANSFERP CAR CODELIST THEN RETURN NIL; 
      !&ATTACH ('!*JUMP . LBL); 
      !&ADDJMP CODELIST
   END;

SYMBOLIC PROCEDURE !&TRANSFERP X; 
   FLAGP(IF CAR X EQ '!*LINK THEN CADR X ELSE CAR X,'TRANSFER);

SYMBOLIC PROCEDURE !&ADDJMP CLIST; 
   BEGIN SCALAR X; 
      X := !&FINDLBL CDAR CLIST; 
      RPLACD(X,CAR CLIST . CDR X); 
      JMPLIST := CLIST . JMPLIST
   END;

SYMBOLIC PROCEDURE !&REMJMP CLIST; 
   BEGIN SCALAR X; 
      X := !&FINDLBL CDAR CLIST; 
      RPLACD(X,!&DELEQ(CAR CLIST,CDR X)); 
      JMPLIST := !&DELEQ(CLIST,JMPLIST); 
      !&MOVEUP CLIST
   END;

SYMBOLIC PROCEDURE !&DELEQ(U,V); 
   IF NULL V THEN NIL
    ELSE IF U EQ CAR V THEN CDR V
    ELSE CAR V . !&DELEQ(U,CDR V);

SYMBOLIC PROCEDURE !&FRAME U; %allocates space for U in frame;
   BEGIN SCALAR Z; 
      STOMAP := LIST(U,Z := CADAR STOMAP - 1) . STOMAP; 
      IF Z<CAR LLNGTH THEN RPLACA(LLNGTH,Z)
   END;

SYMBOLIC PROCEDURE !&GETFRM U; 
   (LAMBDA X; 
       IF X THEN CDR X ELSE LPRIE LIST("COMPILER ERROR: LOST VAR",U))
    ATSOC(U,STOMAP);

SYMBOLIC PROCEDURE !&GETFFRM U; 
   BEGIN SCALAR X; X := !&GETFRM U; FREELST := X . FREELST; RETURN X
   END;

COMMENT Pass 3 of the compiler (post code generation fixups); 

SYMBOLIC PROCEDURE !&PASS3; 
   BEGIN SCALAR FLAGG; %remove spurious stores;
      FOR EACH J IN SLST DO <<STLST := !&DELEQ(CADR J,STLST); 
                              RPLACA(CADR J,'!*NOOP)>>; 
      !&FIXCHAINS(); 
      !&FIXLINKS(); 
      !&FIXFRM(); 
      !&ATTLBL EXIT; 
      IF FLAGG
        THEN <<IF NOT !*NOLINKE
                    AND CAAR CODELIST EQ '!*LBL
                    AND CAADR CODELIST EQ '!*LINKE
                 THEN RPLACA(CDR CODELIST,
                             LIST('!*LINK,CADADR CODELIST,
                                  CADR CDADR CODELIST,
                                  CADDR CDADR CODELIST)); 
                  %removes unnecessary !*LINKE;
               !&ATTACH ('!*DEALLOC . LLNGTH); 
               !&ATTACH LIST '!*EXIT>>; 
      !&PEEPHOLEOPT(); 
      !&FIXREST()
   END;

SYMBOLIC PROCEDURE !&FIXCHAINS; 
   BEGIN SCALAR EJMPS,EJMPS1,P,Q; %find any common chains of code;
      IF NOT CAR CODELIST='!*LBL . EXIT THEN !&ATTLBL EXIT; 
      CODELIST := CDR CODELIST; 
      IF NOT CAR CODELIST='!*JUMP . EXIT THEN !&ATTJMP EXIT; 
      EJMPS := REVERSE JMPLIST; 
      WHILE EJMPS DO 
         BEGIN 
            P := CAR EJMPS; 
            EJMPS := CDR EJMPS; 
            IF CAAR P EQ '!*JUMP
              THEN <<EJMPS1 := EJMPS; 
                     WHILE EJMPS1 DO 
                        IF CAR P=CAAR EJMPS1 AND CADR P=CADAR EJMPS1
                          THEN <<!&REMJMP P; 
                                 !&FIXCHN(P,CDAR EJMPS1); 
                                 EJMPS1 := NIL>>
                         ELSE EJMPS1 := CDR EJMPS1>>
         END
   END;

SYMBOLIC PROCEDURE !&FIXLINKS; 
   %replace !*LINK by !*LINKE where appropriate;
   BEGIN SCALAR EJMPS,P,Q; 
      EJMPS := JMPLIST; 
      IF NOT !*NOLINKE
        THEN WHILE EJMPS DO 
                BEGIN 
                   P := CAR EJMPS; 
                   Q := CDR P; 
                   EJMPS := CDR EJMPS; 
                   IF NOT CADAR P EQ CAR EXIT THEN RETURN NIL
                    ELSE IF NOT CAAR P EQ '!*JUMP
                              OR NOT CAAR Q EQ '!*LINK
                     THEN RETURN FLAGG := T; 
                   RPLACW(CAR Q,
                          '!*LINKE
                            . (CADAR Q
                                 . (CADDAR Q
                                      . (CADR CDDAR Q . LLNGTH)))); 
                   !&REMJMP P
                END
       ELSE FLAGG := T
   END;

SYMBOLIC PROCEDURE !&FINDBLK(U,LBL); 
   IF NULL CDR U THEN NIL
    ELSE IF CAADR U EQ '!*LBL AND !&TRANSFERP CADDR U THEN U
    ELSE IF GET(CAADR U,'NEGJMP) AND CADADR U EQ LBL THEN U
    ELSE !&FINDBLK(CDR U,LBL);

PUT('!*NOOP,'OPTFN,'!&MOVEUP);

PUT('!*LBL,'OPTFN,'!&LBLOPT);

SYMBOLIC PROCEDURE !&LBLOPT U; 
   BEGIN SCALAR Z; 
      IF CADAR U EQ CADADR U THEN RETURN !&REMJMP CDR U
       ELSE IF CAADR U EQ '!*JUMP
                 AND (Z := GET(CAADDR U,'NEGJMP))
                 AND CADAR U EQ CADR CADDR U
        THEN RETURN <<Z := Z . (CADADR U . CDDR CADDR U); 
                      !&REMJMP CDR U; 
                      !&REMJMP CDR U; 
                      RPLACD(U,Z . (CADR U . CDDR U)); 
                      !&ADDJMP CDR U; 
                      T>>
       ELSE RETURN NIL
   END;

SYMBOLIC PROCEDURE !&PEEPHOLEOPT; 
   %'peep-hole' optimization for various cases;
   BEGIN SCALAR X,Z; 
      Z := CODELIST; 
      WHILE Z DO 
         IF NOT (X := GET(CAAR Z,'OPTFN)) OR NOT APPLY(X,LIST Z)
           THEN Z := CDR Z
   END;

SYMBOLIC PROCEDURE !&FIXREST; 
   %checks for various cases involving unique (and unused) labels
   %and sequences like (JUMPx lab) M1 ... Mn ... (LAB lab) M1 ... Mn
   %where Mi do not affect reg 1;
   BEGIN SCALAR LABS,TLABS,X,Y,Z; 
      WHILE CODELIST DO 
         <<IF CAAR CODELIST EQ '!*LBL
             THEN <<!&LBLOPT CODELIST; 
                    IF CDR (Z := !&FINDLBL CDAR CODELIST)
                      THEN <<Y := CAR CODELIST . Y; 
                             IF NULL CDDR Z
                                  AND !&TRANSFERP CADR Z
                                  AND CAADR Y EQ '!*LOAD
                                  AND !&NOLOADP(CDADR Y,
                                                CDR ATSOC(CADR Z,
                                                          JMPLIST))
                               THEN <<IF 
                                          NOT !&NOLOADP(CDADR Y,
                                                        CDR CODELIST)
                                        THEN RPLACW(CDR CODELIST,
                                                    CADR Y
                                                    . CADR CODELIST
						      . CDDR CODELIST);
                                      RPLACW(CDR Y,CDDR Y)>>
                              ELSE <<IF NULL CDDR Z
                                          AND CAADR CODELIST EQ '!*JUMP
                                          AND GET(CAADR Z,'NEGJMP)
                                       THEN LABS := 
                                             (CADR Z . Y) . LABS; 
                                     IF !&TRANSFERP CADR CODELIST
                                       THEN TLABS := 
                                             (CADAR Y . Y)
                                               . TLABS>>>>>>
            ELSE IF GET(CAAR CODELIST,'NEGJMP)
                      AND (Z := ATSOC(CAR CODELIST,LABS))
             THEN <<X := CAR CODELIST; 
                    CODELIST := CDR CODELIST; 
                    Z := CDDR Z; 
                    WHILE CAR Y=CAR Z
                            AND (CAAR Y EQ '!*STORE
                                   OR CAAR Y EQ '!*LOAD
                                        AND NOT CADAR Y=1) DO 
                       <<CODELIST := CAR Y . CODELIST; 
                         RPLACW(Z,CADR Z . CDDR Z); 
                         Y := CDR Y>>; 
                    CODELIST := X . CODELIST; 
                    Y := X . Y>>
            ELSE IF CAAR CODELIST EQ '!*JUMP
                      AND (Z := ATSOC(CADAR CODELIST,TLABS))
                      AND (X := 
                            !&FINDBLK(CDR CODELIST,
                                      IF CAAR Y EQ '!*LBL THEN CADAR Y
                                       ELSE NIL))
             THEN BEGIN SCALAR W; 
                     IF NOT CAADR X EQ '!*LBL
                       THEN <<IF NOT CAAR X EQ '!*LBL
                                THEN X := 
                                      CDR RPLACD(X,
                                                 ('!*LBL . !&GENLBL())
                                                   . CDR X); 
                              W := 
                               GET(CAADR X,'NEGJMP)
                                 . (CADAR X . CDDADR X); 
                              !&REMJMP CDR X; 
                              RPLACD(X,W . (CADR X . CDDR X)); 
                              !&ADDJMP CDR X>>
                      ELSE X := CDR X; 
                     W := NIL; 
                     REPEAT <<W := CAR Y . W; Y := CDR Y>>
                        UNTIL Y EQ CDR Z; 
                     RPLACD(X,NCONC(W,CDR X)); 
                     !&REMJMP CODELIST; 
                     TLABS := NIL; %since code chains have changed;
                     CODELIST := NIL . (CAR Y . CODELIST); 
                     Y := CDR Y
                  END
            ELSE Y := CAR CODELIST . Y; 
           CODELIST := CDR CODELIST>>; 
      CODELIST := Y
   END;

SYMBOLIC PROCEDURE !&NOLOADP(ARGS,INSTRS); 
   %determines if a LOAD is not necessary in instruction stream;
   ATOM CADR ARGS
     AND (CAAR INSTRS EQ '!*LOAD AND CDAR INSTRS=ARGS
            OR CAAR INSTRS EQ '!*STORE
                 AND (CDAR INSTRS=ARGS
                        OR NOT CADDAR INSTRS=CADR ARGS
                             AND !&NOLOADP(ARGS,CDR INSTRS)));

SYMBOLIC PROCEDURE !&FIXCHN(U,V); 
   BEGIN SCALAR X; 
      WHILE CAR U=CAR V DO <<!&MOVEUP U; V := CDR V>>; 
      X := !&GENLBL(); 
      IF CAAR V EQ '!*LBL THEN !&RECHAIN(X,CDAR V)
       ELSE RPLACW(V,('!*LBL . X) . (CAR V . CDR V)); 
      IF CAAR U EQ '!*LBL THEN <<!&RECHAIN(CDAR U,X); !&MOVEUP U>>; 
      IF CAAR U EQ '!*JUMP THEN RETURN NIL; 
      RPLACW(U,('!*JUMP . X) . (CAR U . CDR U)); 
      !&ADDJMP U
   END;

SYMBOLIC PROCEDURE !&FIXFRM; 
   BEGIN SCALAR HOLES,LST,X,Y,Z; INTEGER N; 
      IF NULL STLST AND NULL FREELST THEN RETURN RPLACA(LLNGTH,1); 
      N := 0; 
      WHILE NOT N<CAR LLNGTH DO 
         <<Y := NIL; 
           FOR EACH LST IN STLST DO IF N=CADDR LST
                                      THEN Y := CDDR LST . Y; 
           FOR EACH LST IN FREELST DO IF N=CAR LST THEN Y := LST . Y; 
           IF NULL Y THEN HOLES := N . HOLES ELSE Z := (N . Y) . Z; 
           N := N - 1>>; 
      Y := Z; 
      IF CAAR Z>CAR LLNGTH THEN RPLACA(LLNGTH,CAAR Z); 
      WHILE HOLES DO 
         <<WHILE HOLES AND CAR HOLES<CAR LLNGTH DO HOLES := CDR HOLES; 
           IF HOLES
             THEN <<HOLES := REVERSIP HOLES; 
                    FOR EACH X IN CDAR Z DO RPLACA(X,CAR HOLES); 
                    RPLACA(LLNGTH,
                           IF NULL CDR Z OR CAR HOLES<CAADR Z
                             THEN CAR HOLES
                            ELSE CAADR Z); 
                    HOLES := REVERSIP CDR HOLES; 
                    Z := CDR Z>>>>; 
      %now see if we can map frame to registers;
      N := IF NARG<3 THEN 3 ELSE NARG + 1; 
      IF FREELST OR NULL !&REGP CODELIST OR CAR LLNGTH<N - MAXNARGS
        THEN RETURN NIL; 
      FOR EACH X IN STLST DO RPLACW(X,
                                    LIST('!*LOAD,
                                         N - CADDR X,
                                         IF NULL CADR X
                                           THEN '(QUOTE NIL)
                                          ELSE CADR X)); 
      WHILE Y DO 
         <<FOR EACH X IN CDAR Y DO NOT CAR X>0
                                     AND RPLACA(X,N - CAR X); 
              %first test makes sure replacement only occurs once;
           Y := CDR Y>>; 
      RPLACA(LLNGTH,1)
   END;

SYMBOLIC PROCEDURE !&REGP U; 
   %there is no test for !*LAMBIND/!*PROGBIND
   %since FREELST tested explicitly in !&FIXFRM;
   IF NULL CDR U THEN T
    ELSE IF CAAR U MEMQ '(!*LOAD !*STORE)
	  AND NUMBERP CADAR U AND CADAR U>2
     THEN NIL
    ELSE IF FLAGP(CAADR U,'UNKNOWNUSE)
              AND 
                   NOT (IDP CADADR U
                          AND (FLAGP(CADADR U,'ONEREG)
                                 OR FLAGP(CADADR U,'TWOREG))
                          OR CAR U='!*JUMP . EXIT)
     THEN NIL
    ELSE !&REGP CDR U;

FLAG('(!*CODE !*LINK !*LINKE),'UNKNOWNUSE);

SYMBOLIC PROCEDURE !*CODE U;  EVAL U;

PUT('!*JUMPN,'NEGJMP,'!*JUMPE);

PUT('!*JUMPE,'NEGJMP,'!*JUMPN);

PUT('!*JUMPNIL,'NEGJMP,'!*JUMPT);

PUT('!*JUMPT,'NEGJMP,'!*JUMPNIL);

PUT('!*JUMPC,'NEGJMP,'!*JUMPNC);

PUT('!*JUMPNC,'NEGJMP,'!*JUMPC);

COMMENT Some arithmetic optimizations to reduce the amount of code 
        generated;

SYMBOLIC PROCEDURE !&PAPLUS2(U,VARS); 
   IF CADDR U=1 THEN LIST('ADD1,!&PA1(CADR U,VARS))
    ELSE IF CADR U=1 THEN LIST('ADD1,!&PA1(CADDR U,VARS))
    ELSE 'PLUS2 . !&PALIS(CDR U,VARS);

PUT('PLUS2,'PA1FN,'!&PAPLUS2);

SYMBOLIC PROCEDURE !&PADIFF(U,VARS); 
   IF CADDR U=1 THEN LIST('SUB1,!&PA1(CADR U,VARS))
    ELSE 'DIFFERENCE . !&PALIS(CDR U,VARS);

PUT('DIFFERENCE,'PA1FN,'!&PADIFF);

SYMBOLIC PROCEDURE !&PALESSP(U,VARS); 
   IF CADDR U=0 THEN LIST('MINUSP,!&PA1(CADR U,VARS))
    ELSE 'LESSP . !&PALIS(CDR U,VARS);

PUT('LESSP,'PA1FN,'!&PALESSP);

COMMENT removing unnecessary calls to MINUS; 

SYMBOLIC PROCEDURE !&PAMINUS(U,VARS); 
   IF EQCAR(U := !&PA1(CADR U,VARS),'QUOTE) AND NUMBERP CADR U
     THEN MKQUOTE ( - CADR U)
    ELSE LIST('MINUS,U);

PUT('MINUS,'PA1FN,'!&PAMINUS);


END;
