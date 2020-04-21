COMMENT DECSYSTEM 10 AND 20 COMPILER MACRO MODULE;

PUT('COMPLR,'IMPORTS,'(LAP));

COMMENT fixups for PDP-10 assembly; 

FLAG('(NCONS XCONS),'LOSE);

FLAG('(LIST2 LIST3 LIST4 LIST5),'LOSE);

REMFLAG('(XN),'LOSE);


COMMENT Global variable and flag values for PDP-10 version;

GLOBAL '(MAXNARGS !*NOLINKE !*ORD !*PLAP !*R2I);

MAXNARGS := 14;

!*NOLINKE := NIL;

!*ORD := NIL;

!*PLAP := NIL;

!*R2I := T;

%We also need;

FLUID '(REGS);


COMMENT general functions; 

SYMBOLIC PROCEDURE !&MKFUNC FN; MKQUOTE FN;

COMMENT c-macros for PDP-10 Implementation; 

SYMBOLIC PROCEDURE !*ALLOC N; 
   IF N=0 THEN NIL
    ELSE IF N=1 THEN LIST '(PUSH P 1)
    ELSE LIST(LIST('ADD,'P,LIST('C,0,0,N,N)),'(213 P 85 16));

SYMBOLIC PROCEDURE !*DEALLOC N; 
   IF N>0 THEN LIST LIST('SUB,'P,LIST('C,0,0,N,N)) ELSE NIL;

COMMENT !*ENTRY is handled by the loader;

SYMBOLIC PROCEDURE !*EXIT; LIST '(POPJ P);

SYMBOLIC PROCEDURE !*STORE(REG,FLOC); % Uses R as extra reg;
   BEGIN SCALAR OP,PQ; 
      IF NUMBERP FLOC
        THEN (IF FLOC>5 THEN FLOC := 'EXARG . (FLOC - 6)
               ELSE IF FLOC<1 THEN PQ := '(P))
       ELSE IF EQCAR(FLOC,'GLOBAL) THEN FLOC := 'FLUID . CDR FLOC; 
      IF NUMBERP REG AND REG>5
        THEN RETURN IF IDP FLOC OR NUMBERP FLOC AND FLOC>0
                      THEN !*LOAD(FLOC,REG)
                     ELSE NCONC(!*LOAD('R,REG),
                                LIST ('MOVEM . ('R . (FLOC . PQ)))); 
      OP := IF REG THEN 'MOVEM ELSE <<REG := 0; 'SETZM>>; 
      RETURN LIST (OP . (REG . (FLOC . PQ)))
   END;

SYMBOLIC PROCEDURE !*JUMP ADR; LIST LIST('JRST,0,ADR);

SYMBOLIC PROCEDURE !*JUMPNIL ADR; LIST LIST('JUMPE,1,ADR);

SYMBOLIC PROCEDURE !*JUMPT ADR; LIST LIST('JUMPN,1,ADR);

SYMBOLIC PROCEDURE !*JUMPE(ADR,EXP); 
   NCONC(!*LOADEXP(1,EXP,'(CAMN . CAIN)),LIST LIST('JRST,0,ADR));

SYMBOLIC PROCEDURE !*JUMPN(ADR,EXP); 
   NCONC(!*LOADEXP(1,EXP,'(CAME . CAIE)),LIST LIST('JRST,0,ADR));

SYMBOLIC PROCEDURE !*LBL ADR; LIST ADR;

SYMBOLIC PROCEDURE !*LAMBIND(REGS,ALST); 
   %produces the parameter list for binding;
   BEGIN SCALAR X,Y; 
      ALST := REVERSE ALST; 
      REGS := REVERSE REGS; 
      WHILE ALST DO 
         <<IF NULL REGS THEN X := 0
            ELSE <<X := CAR REGS; REGS := CDR REGS>>; 
           Y := LIST(0,X,LIST('FLUID,CAAR ALST)) . Y; 
           ALST := CDR ALST>>; 
      RETURN '(CALL 0 (E !*LAMBIND!*)) . Y
   END;

SYMBOLIC PROCEDURE !*PROGBIND ALST; !*LAMBIND(NIL,ALST);

SYMBOLIC PROCEDURE !*FREERSTR ALST; '((CALL 0 (E !*SPECRSTR!*)));

SYMBOLIC PROCEDURE !*LOAD(REG,EXP); % Uses R as extra reg;
   IF REG=EXP THEN NIL
    ELSE IF NUMBERP REG AND REG>5
     THEN IF IDP EXP OR NUMBERP EXP AND EXP>0 THEN !*STORE(EXP,REG)
           ELSE IF EXP='(QUOTE NIL) THEN !*STORE(NIL,REG)
           ELSE NCONC(!*LOAD('R,EXP),!*STORE('R,REG))
    ELSE !*LOADEXP(REG,EXP,'(MOVE . MOVEI));

SYMBOLIC PROCEDURE !*LINK(FN,TYPE,NARGS);
   !*MKLINK(FN,TYPE,NARGS,-1,'CALL);

SYMBOLIC PROCEDURE !*LINKE(FN,TYPE,NARGS,N);
   !*MKLINK(FN,TYPE,NARGS,N,'JCALL);

COMMENT Auxiliary functions used by the c-macros;

SYMBOLIC PROCEDURE !*OPEN U; 
   IF CAR U EQ 'LAMBDA THEN SUBPLIS(U,'(1 1)) ELSE U;

SYMBOLIC PROCEDURE SUBPLIS(X,Y); SUBLIS(PAIR(CADR X,Y),CADDR X);

SYMBOLIC PROCEDURE !*LOADEXP(REG,U,OPS); 
   %OPS=(direct . immediate). When not MOVE, uses D as extra reg;
   %REG is always an actual machine register;
   IF ATOM U
     THEN IF IDP U OR U>0 AND U<6 THEN LIST LIST(CAR OPS,REG,U)
           ELSE IF U>5 THEN LIST LIST(CAR OPS,REG,'EXARG . (U - 6))
           ELSE LIST LIST(CAR OPS,REG,U,'P)
    ELSE IF CAR U EQ 'QUOTE THEN LIST LIST(CDR OPS,REG,U)
    ELSE IF CAR U EQ 'GLOBAL THEN LIST LIST(CAR OPS,REG,'FLUID . CDR U)
    ELSE IF CAR U EQ 'FLUID THEN LIST LIST(CAR OPS,REG,U)
    ELSE IF NOT CAR OPS EQ 'MOVE
     THEN NCONC(!*LOAD('D,U),LIST LIST(CAR OPS,REG,'D))
    ELSE BEGIN SCALAR X,Y,Z; 
            X := 'ANYREG; 
            IF ATOM (Y := CADR U)
              THEN IF IDP Y THEN X := 'OPEN
                    ELSE IF Y<1 THEN Y := Y . '(P)
                    ELSE IF Y>5 THEN Y := LIST ('EXARG . (Y - 6))
                    ELSE X := 'OPEN
             ELSE IF CAR Y EQ 'GLOBAL THEN Y := LIST ('FLUID . CDR Y)
             ELSE IF CAR Y EQ 'FLUID THEN Y := LIST Y
             ELSE <<X := 'OPEN; Z := !*LOAD(REG,Y); Y := REG>>; 
            IF NOT (X := GET(CAR U,X))
              THEN LPRIE LIST("Incomplete macro definition for",
			      CAR U); 
            RETURN NCONC(Z,SUBPLIS(X,LIST(REG,Y)))
         END;

SYMBOLIC PROCEDURE !*MKLINK(FN,TYPE,NARGS,N,CALL); 
   BEGIN SCALAR B,Y; 
      B := N<0; 
      IF (Y := GET(FN,'OPEN)) AND (B OR NOT FLAGP(FN,'NOPENR))
        THEN <<Y := !*OPEN Y; 
               IF NOT B
                 THEN Y := 
                       APPEND(Y,LIST(LIST('!*DEALLOC,N),'(!*EXIT)))>>
       ELSE <<Y := 
               LIST LIST(CALL,
                         IF TYPE EQ 'FEXPR THEN 15 ELSE NARGS,
                         LIST('E,FN)); 
              IF N>0 THEN Y := LIST('!*DEALLOC,N) . Y>>; 
      RETURN Y
   END;

COMMENT Peep-hole optimization tables; 

SYMBOLIC PROCEDURE !&STOPT U; 
   %this has to use fact that LLNGTH is offset during code generation;
   IF CDAR U='(1 0) AND CADR U='(!*ALLOC 0)
     THEN <<RPLACA(U,'(PUSH P 1)); RPLACD(U,NIL)>>
    ELSE IF CDAR U='(2 -1)
              AND CADR U='(!*STORE 1 0)
              AND CADDR U='(!*ALLOC -1)
     THEN <<RPLACA(U,'(PUSH P 1)); 
            RPLACA(CDR U,'(PUSH P 2)); 
            RPLACD(CDR U,NIL)>>;

PUT('!*STORE,'OPTFN,'!&STOPT);

COMMENT Some PDP-10 dependent optimizations; 

SYMBOLIC PROCEDURE !&PAEQUAL(U,VARS); 
   (LAMBDA(X,Y); 
       IF !&EQVP X OR !&EQVP Y THEN 'EQ
        ELSE IF NUMBERP X OR NUMBERP Y THEN 'EQN
        ELSE 'EQUAL)
      (CADR U,CADDR U)
     . !&PALIS(CDR U,VARS);

PUT('EQUAL,'PA1FN,'!&PAEQUAL);

SYMBOLIC PROCEDURE !&EQP U; 
   %!&EQP is true if U is an object for which EQ can replace EQUAL;
   INUMP U OR IDP U;

SYMBOLIC PROCEDURE !&EQVP U; 
   %!&EQVP is true if EVAL U is an object for which EQ can
   %replace EQUAL;
   INUMP U OR EQCAR(U,'QUOTE) AND !&EQP CADR U;

SYMBOLIC PROCEDURE !&PAMEMBER(U,VARS); 
   (LAMBDA(X,Y); 
       IF !&EQVP X THEN 'MEMQ
        ELSE IF NOT EQCAR(Y,'QUOTE) THEN 'MEMBER
        ELSE BEGIN SCALAR A; 
                A := (Y := CADR Y); 
                WHILE Y AND A DO <<A := !&EQP CAR Y; Y := CDR Y>>; 
                RETURN IF A THEN 'MEMQ ELSE 'MEMBER
             END)
      (CADR U,CADDR U)
     . !&PALIS(CDR U,VARS);

PUT('MEMBER,'PA1FN,'!&PAMEMBER);

SYMBOLIC PROCEDURE !&PAASSOC(U,VARS); 
   (LAMBDA(X,Y); 
       IF !&EQVP X THEN 'ATSOC
        ELSE IF NOT EQCAR(Y,'QUOTE) THEN 'ASSOC
        ELSE BEGIN SCALAR A; 
                A := T; 
                Y := CADR Y; 
                WHILE Y AND A DO <<A := !&EQP CAAR Y; Y := CDR Y>>; 
                RETURN IF A THEN 'ATSOC ELSE 'ASSOC
             END)
      (CADR U,CADDR U)
     . !&PALIS(CDR U,VARS);

PUT('ASSOC,'PA1FN,'!&PAASSOC);

SYMBOLIC PROCEDURE !&COMAPPLY(EXP,STATUS); % Look for LIST;
   BEGIN INTEGER N,NN; SCALAR FN,ARGS; 
      EXP := CDR EXP; 
      FN := CAR EXP; 
      ARGS := CDR EXP; 
      IF !&CFNTYPE FN EQ 'FEXPR
        THEN LPRIE LIST(FN,"IS NOT AN EXPR FOR APPLY"); 
      IF NULL ARGS
           OR CDR ARGS
           OR NOT EQCAR(CAR ARGS,'LIST)
           OR (NN := (N := LENGTH CDAR ARGS))>MAXNARGS
        THEN RETURN !&CALL('APPLY,EXP,STATUS); 
      ARGS := REVERSE (FN . REVERSE CDAR ARGS); 
      ARGS := !&COMLIS ARGS; 
      !&STORE1(); 
      FN := CAR ARGS; 
      ARGS := CDR ARGS; 
      IF STATUS>0 THEN !&CLRREGS(); 
      WHILE N>0 DO 
         <<!&LREG(N,CAR ARGS,CDR ARGS,STATUS); 
           ARGS := CDR ARGS; 
           N := N - 1>>; 
      !&ATTACH ('!*LINKF . (NN . !&LOCATE FN)); 
      REGS := LIST (1 . NIL)
   END;

%PUT('APPLY,'COMPFN,'!&COMAPPLY);  %Only works for compiled functions;

SYMBOLIC PROCEDURE !&COMRPLAC(EXP,STATUS); 
   BEGIN SCALAR FN,X,Y; 
      FN := IF CAR EXP EQ 'RPLACA THEN '!*RPLACA ELSE '!*RPLACD; 
      EXP := !&COMLIS CDR EXP; 
      Y := IF CAR EXP = '(QUOTE NIL) THEN NIL
            ELSE IF Y := !&RASSOC(CAR EXP,REGS) THEN CAR Y
            ELSE <<!&LREG('TT,CAR EXP,CDR EXP,STATUS); 'TT>>; 
      IF STATUS<2
        THEN <<IF Y=1 THEN !&LREG(Y := 'TT,CAR EXP,CDR EXP,STATUS);
               !&LREG1(CADR EXP,STATUS)>>;
      !&ATTACH (FN . (Y . !&LOCATE CADR EXP)) 
   END;

PUT('RPLACA,'COMPFN,'!&COMRPLAC);

PUT('RPLACD,'COMPFN,'!&COMRPLAC);

COMMENT Additional c-macros defined in PDP-10 implementation; 

SYMBOLIC PROCEDURE !*LINKF(NARGS,FNEXP); 
   !*LOADEXP(NARGS,FNEXP,'(CALLF!@ . CALLF));

SYMBOLIC PROCEDURE !*RPLACA(REG,EXP); 
   !*LOADEXP!*(REG,EXP,'((RPLCA!@ . RPLCA) . (HRRZS!@ . HRRZS)));

SYMBOLIC PROCEDURE !*RPLACD(REG,EXP); 
   !*LOADEXP!*(REG,EXP,'((RPLCD!@ . RPLCD) . (HLLZS!@ . HLLZS)));

SYMBOLIC PROCEDURE !*LOADEXP!*(REG,EXP,OPS);
 IF REG
   THEN IF NUMBERP REG AND REG>5
          THEN NCONC(!*LOAD('R,REG),!*LOADEXP('R,EXP,CAR OPS))
         ELSE !*LOADEXP(REG,EXP,CAR OPS)
  ELSE !*LOADEXP(0,EXP,CDR OPS);

FLAG('(!*LINKF !*RPLACA !*RPLACD),'MC);

FLAG('(LINKF),'UNKNOWNUSE);

COMMENT Open coded functions in this version;

PUT('CAR,'OPEN,'(LAMBDA (X Y) ((HLRZ X 0 Y))));

PUT('CDR,'OPEN,'(LAMBDA (X Y) ((HRRZ X 0 Y))));

FLAG('(RPLACA RPLACD),'NOPENR);

PUT('CAR,'ANYREG,'(LAMBDA (X Y) ((HLRZ!@ X . Y))));

PUT('CDR,'ANYREG,'(LAMBDA (X Y) ((HRRZ!@ X . Y))));


COMMENT PDP-10 interpreter function register use;

FLAG( '(
CAR CDR RPLACA RPLACD
ATOM CLOSE CODEP CONSTANTP EJECT EQ FIXP FLOATP GET IDP LINELENGTH
LPOSN NCONS NOT NUMBERP NULL PAGELENGTH PAIRP POSN REMPROP REVERSE
STRINGP TERPRI VECTORP XCONS UPBV
!*LAMBIND!* !*PROGBIND!* !*SPECRSTR!* BIGP INUMP RECLAIM TYO UNTYI
),'ONEREG);

FLAG('(
ABS ATSOC CONS FIX FLOAT GETD GETV LENGTH PRINC PUTV PUT REMD
!*BOX ASCII BINI BINO DELIMITER EXAMINE EXCISE FILEP GCTIME IGNORE
LETTER MKCODE NUMVAL RDSLSH SCANSET SETPCHAR
SPEAK TIME 
),'TWOREG);


COMMENT Code for counting macro execution use; 

FLUID '(MCPROCS !*COUNTMC);

SYMBOLIC PROCEDURE RESETMC U; 
   BEGIN SCALAR L; 
      !*COUNTMC := U; 
      FOR EACH L IN MCPROCS DO <<SET(L,CDR (131072 + 1)); 
                                    % FWD of a fresh FIXNUM;
                                 DEPOSIT(!*BOX EVAL L,0); 
                                    % FWD = numeric 0 now;
                                 PUT(L,'MCCOUNT,0)>>
   END;

SYMBOLIC PROCEDURE COUNTMC L; LIST LIST(118800,0,LIST('FLUID,L));

SYMBOLIC PROCEDURE PRINTMC; 
   BEGIN SCALAR SM; 
      SM := 0; 
      PRIN2 "DYNAMIC COUNT:"; 
      TERPRI(); 
      FOR EACH L IN MCPROCS DO <<PRIN2 L; 
                                 PRIN2 "	"; 
                                 SM := 
                                  PRINT (CAR 131072 . EVAL L) + SM>>; 
      PRIN2 "DYNAMIC TOTAL: "; 
      PRINT SM; 
      TERPRI(); 
      PRIN2 "STATIC COUNT:"; 
      TERPRI(); 
      SM := 0; 
      FOR EACH L IN MCPROCS DO <<PRIN2 L; 
                                 PRIN2 "	"; 
                                 SM := PRINT GET(L,'MCCOUNT) + SM>>; 
      PRIN2 "STATIC TOTAL: "; 
      PRINT SM
   END;

MCPROCS := 
 '(!*ALLOC
   !*DEALLOC
   !*ENTRY
   !*EXIT
   !*LOAD
   !*STORE
   !*JUMP
   !*JUMPE
   !*JUMPN
   !*JUMPT
   !*JUMPNIL
   !*LBL
   !*LAMBIND
   !*PROGBIND
   !*FREERSTR
   !*LINK
   !*LINKF
   !*LINKE
   !*RPLACA
   !*RPLACD);

RESETMC NIL;


SYMBOLIC PROCEDURE LAPPRI U;
   BEGIN
    A: IF NULL U THEN RETURN NIL;
      PRIN1 CAR U;
      U := CDR U;
      IF NULL U THEN RETURN NIL;
      SPACES2 24;
      PRIN1 CAR U;
      U := CDR U;
      IF NULL U THEN RETURN NIL;
      SPACES2 48;
      PRIN1 CAR U;
      TERPRI();
      U := CDR U;
      GO TO A
   END;

SYMBOLIC PROCEDURE SPACES2 N;
      <<IF POSN()>N THEN TERPRI(); SPACES(N-POSN())>>;


END;
