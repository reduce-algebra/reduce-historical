%*********************************************************************
%*********************************************************************
%            REDUCE BASIC ALGEBRAIC PROCESSOR (PART 1)
%*********************************************************************
%********************************************************************;

%Copyright (c) 1983 The Rand Corporation;

SYMBOLIC;

%*********************************************************************
%	     NON-LOCAL VARIABLES REFERENCED IN THIS SECTION
%********************************************************************;

FLUID '(ALGLIST!* ARBL!* !*EXP !*GCD !*INTSTR !*LCM !*MCD !*MODE);

GLOBAL '(ASYMPLIS!* CURSYM!* DMODE!* DOMAINLIST!* EXLIST!* EXPTL!*
         EXPTP!* FRASC!* FRLIS!* INITL!* KORD!* KPROPS!* LETL!* MCHFG!*
	 MCOND!* MOD!* MUL!* NAT!*!* NCMP!* OFL!* POSN!* POWLIS!*
	 POWLIS1!* SPLIS!* SUBFG!* TSTACK!* TYPL!* WS WTL!* !*EZGCD
	 !*FLOAT !*FORT !*GROUP !*INT !*MATCH !*MSG !*NAT !*NERO
	 !*NOSUBS !*NUMVAL !*OUTP !*PERIOD !*PRI !*RESUBS !*SQVAR!*
         !*SUB2 !*VAL !*XDN);

GLOBAL '(DSUBL!* SUBL!*);   %not used at moment;

ALGLIST!* := NIL;	%association list for previously simplified
			%expressions;
ARBL!* := NIL;          %used for storage of arbitrary vars in LET
			%statements;
ASYMPLIS!* := NIL;	%association list of asymptotic replacements;
% CURSYM!*		current symbol (i. e. identifier, parenthesis,
%                       delimiter, e.t.c,) in input line;
DMODE!* := NIL;		%name of current polynomial domain mode if not
			%integer;
DOMAINLIST!* := NIL;	%list of currently supported poly domain modes;
%DSUBL!* := NIL;        %list of previously calculated derivatives of
			% expressions;
EXLIST!* := '((!*));	%property list for standard forms used as
			% kernels;
EXPTL!* := NIL; 	%list of exprs with non-integer exponents;
EXPTP!* := NIL; 	%flag telling EXPTs appear in LET statements;
FRASC!* := NIL; 	%association list for free variables in
			%substitution rules;
FRLIS!* := NIL; 	%list of renamed free variables to be found in
			%substitutions;
INITL!* := APPEND('(FRASC!* MCOND!* SUBFG!* !*SUB2 TSTACK!*),INITL!*);
KORD!* := NIL;		%kernel order in standard forms;
KPROPS!* := NIL;	%list of active non-atomic kernel plists;
LETL!* := '(LET MATCH CLEAR SAVEAS SUCH);   %special delimiters;
MCHFG!* := NIL; 	%indicates that a pattern match occurred during
			%a cycle of the matching routines;
MCOND!* := NIL; 	%used for temporary storage of a conditional
			%expression in a substitution;
MOD!* := NIL;		%modular base, NIL for integer arithmetic;
MUL!* := NIL;		%list of additional evaluations needed in a
			%given multiplication;
NAT!*!* := NIL; 	%temporary variable used in algebraic mode;
NCMP!* := NIL;		%flag indicating non-commutative multiplication
			%mode;
OFL!* := NIL;		%current output file name;
POSN!* := NIL;		%used to store output character position in 
			%printing functions;
POWLIS!* := NIL;	%association list of replacements for powers;
POWLIS1!* := NIL;	%association list of conditional replacements
			%for powers;
SPLIS!* := NIL; 	%substitution list for sums and products;
SUBFG!* := T;		%flag to indicate whether substitution
			%is required during evaluation;
%SUBL!* := NIL;         %list of previously evaluated expressions;
TSTACK!* := 0;		%stack counter in SIMPTIMES;
% TYPL!*;
WTL!* := NIL;		%tells that a WEIGHT assignment has been made;
!*EXP := T;		%expansion control flag;
!*EZGCD := NIL;         %ezgcd calculation flag;
!*FLOAT := NIL; 	%floating arithmetic mode flag;
!*FORT := NIL;          %specifies FORTRAN output;
!*GCD := NIL;		%greatest common divisor mode flag;
!*GROUP := NIL; 	%causes expressions to be grouped when EXP off;
!*INTSTR := NIL;   	%makes expression arguments structured;
%!*INT                  indicates interactive system use;
!*LCM := T;             %least common multiple computation flag;
!*MATCH := NIL;         %list of pattern matching rules;
!*MCD := T;		%common denominator control flag;
!*MODE := 'SYMBOLIC;	%current evaluation mode;
!*MSG := T;		%flag controlling message printing;
!*NAT := T;             %specifies natural printing mode;
!*NERO := NIL;		%flag to suppress printing of zeros;
!*NOSUBS := NIL;	%internal flag controlling substitution;
!*NUMVAL := NIL;	%used to indicate that numerical expressions
			%should be converted to a real value;
!*OUTP := NIL;		%holds prefix output form for extended output
			%package;
!*PERIOD := T;		%prints a period after a fixed coefficient
			%when FORT is on;
!*PRI := NIL;		%indicates that fancy output is required;
!*RESUBS := T;		%external flag controlling resubstitution;
!*SQVAR!*:='(T);	%variable used by *SQ expressions to control
			%resimplification;
!*SUB2 := NIL;		%indicates need for call of RESIMP;
!*VAL := T;		%controls operator argument evaluation;
!*XDN := T;		%flag indicating that denominators should be
			%expanded;

%initial values of some global variables in BEGIN1 loops;

PUT('TSTACK!*,'INITL,0);

PUT('SUBFG!*,'INITL,T);

%Old name for the expression workspace;

%PUT('!*ANS,'NEWNAM,'WS);


%*********************************************************************
%			   GENERAL FUNCTIONS
%********************************************************************;

SYMBOLIC PROCEDURE ATOMLIS U;
   NULL U OR (ATOM CAR U AND ATOMLIS CDR U);

SYMBOLIC PROCEDURE CARX(U,V);
   IF NULL CDR U THEN CAR U
    ELSE REDERR LIST("Wrong number of arguments to",V);

SYMBOLIC PROCEDURE DELASC(U,V);
   IF NULL V THEN NIL
    ELSE IF ATOM CAR V OR U NEQ CAAR V THEN CAR V . DELASC(U,CDR V)
    ELSE CDR V;

SYMBOLIC PROCEDURE LENGTHC U;
   %gives character length of U excluding string and escape chars;
   BEGIN INTEGER N; SCALAR X;
      N := 0;
      X := EXPLODE U;
      IF CAR X EQ '!" THEN RETURN LENGTH X-2;
      WHILE X DO
	<<IF CAR X EQ '!! THEN X := CDR X;
	  N := N+1;
	  X := CDR X>>;
      RETURN N
   END;

SYMBOLIC PROCEDURE GET!*(U,V);
   IF NUMBERP U THEN NIL ELSE GET(U,V);

SYMBOLIC PROCEDURE MAPCONS(U,V);
   FOR EACH J IN U COLLECT V . J;

SYMBOLIC PROCEDURE MAPPEND(U,V);
   FOR EACH J IN U COLLECT APPEND(V,J);

SYMBOLIC PROCEDURE NLIST(U,N);
   IF N=0 THEN NIL ELSE U . NLIST(U,N-1);

SYMBOLIC PROCEDURE NTH(U,N);
   CAR PNTH(U,N);

SYMBOLIC PROCEDURE PNTH(U,N);
   IF NULL U THEN REDERR "Index out of range"
    ELSE IF N=1 THEN U
    ELSE PNTH(CDR U,N-1);

SYMBOLIC PROCEDURE PERMP(U,V);
   IF NULL U THEN T
    ELSE IF CAR U EQ CAR V THEN PERMP(CDR U,CDR V)
    ELSE NOT PERMP(CDR U,SUBST(CAR V,CAR U,CDR V));

SYMBOLIC PROCEDURE REMOVE(X,N);
   %Returns X with Nth element removed;
   IF NULL X THEN NIL
    ELSE IF N=1 THEN CDR X
    ELSE CAR X . REMOVE(CDR X,N-1);

SYMBOLIC PROCEDURE REVPR U;
   CDR U . CAR U;

SYMBOLIC PROCEDURE REPEATS X;
   IF NULL X THEN NIL
    ELSE IF CAR X MEMBER CDR X THEN CAR X . REPEATS CDR X
    ELSE REPEATS CDR X;

SYMBOLIC PROCEDURE SMEMBER(U,V);
   %determines if S-expression U is a member of V at any level;
   IF U=V THEN T
    ELSE IF ATOM V THEN NIL
    ELSE SMEMBER(U,CAR V) OR SMEMBER(U,CDR V);

SYMBOLIC PROCEDURE SMEMQ(U,V);
   %true if id U is a member of V at any level (excluding
   %quoted expressions);
   IF ATOM V THEN U EQ V
    ELSE IF CAR V EQ 'QUOTE THEN NIL
    ELSE SMEMQ(U,CAR V) OR SMEMQ(U,CDR V);

SYMBOLIC PROCEDURE SMEMQL(U,V);
   %Returns those members of id list U contained in V at any
   %level (excluding quoted expressions);
   IF NULL U THEN NIL
    ELSE IF SMEMQ(CAR U,V) THEN CAR U . SMEMQL(CDR U,V)
    ELSE SMEMQL(CDR U,V);

SYMBOLIC PROCEDURE SMEMQLP(U,V);
   %True if any member of id list U is contained at any level
   %in V (exclusive of quoted expressions);
   IF NULL V THEN NIL
    ELSE IF ATOM V THEN V MEMQ U
    ELSE IF CAR V EQ 'QUOTE THEN NIL
    ELSE SMEMQLP(U,CAR V) OR SMEMQLP(U,CDR V);

SYMBOLIC PROCEDURE SPACES N; FOR I:= 1:N DO PRIN2 " ";

SYMBOLIC PROCEDURE SUBLA(U,V);
   BEGIN SCALAR X;
	IF NULL U OR NULL V THEN RETURN V
	 ELSE IF ATOM V
		 THEN RETURN IF X:= ATSOC(V,U) THEN CDR X ELSE V
	 ELSE RETURN(SUBLA(U,CAR V) . SUBLA(U,CDR V))
   END;

SYMBOLIC PROCEDURE XNP(U,V);
   %returns true if the atom lists U and V have at least one common
   %element;
   U AND (CAR U MEMQ V OR XNP(CDR U,V));


%*********************************************************************
%	 FUNCTIONS FOR PRINTING DIAGNOSTIC AND ERROR MESSAGES
%********************************************************************;

SYMBOLIC PROCEDURE MSGPRI(U,V,W,X,Y);
   BEGIN SCALAR NAT1,Z;
	IF NULL Y AND NULL !*MSG THEN RETURN;
	NAT1 := !*NAT;
	!*NAT := NIL;
	IF OFL!* AND (!*FORT OR NOT NAT1) THEN GO TO C;
    A:	TERPRI();
	LPRI ((IF NULL Y THEN "***" ELSE "*****")
		 . IF U AND ATOM U THEN LIST U ELSE U);
	POSN!* := POSN();
	MAPRIN V;
	PRIN2 " ";
	LPRI IF W AND ATOM W THEN LIST W ELSE W;
	POSN!* := POSN();
	MAPRIN X;
	IF NOT Y OR Y EQ 'HOLD THEN TERPRI();
	IF NULL Z THEN GO TO B;
	WRS CDR Z;
	GO TO D;
    B:	IF NULL OFL!* THEN GO TO D;
    C:	Z := OFL!*;
	WRS NIL;
	GO TO A;
    D:	!*NAT := NAT1;
	IF Y THEN IF Y EQ 'HOLD THEN ERFG!* := Y ELSE ERROR1()
   END;

SYMBOLIC PROCEDURE ERRACH U;
   BEGIN
	TERPRI!* T;
	LPRIE "CATASTROPHIC ERROR *****";
	PRINTTY U;
	LPRIW(" ",NIL);
	REDERR "Please send output and input listing to A. C. Hearn"
   END;

SYMBOLIC PROCEDURE ERRPRI1 U;
   MSGPRI("Substitution for",U,"not allowed",NIL,'HOLD);

SYMBOLIC PROCEDURE ERRPRI2(U,V);
   MSGPRI("Syntax error:",U,"invalid",NIL,V);

SYMBOLIC PROCEDURE REDMSG(U,V);
   IF NULL !*MSG THEN NIL
    ELSE IF TERMINALP() THEN YESP LIST("Declare",U,V,"?") OR ERROR1()
    ELSE LPRIM LIST(U,"declared",V);

SYMBOLIC PROCEDURE TYPERR(U,V);
   <<TERPRI!* T;
     PRIN2!* "***** ";
     IF NOT ATOM U AND ATOM CAR U AND ATOM CADR U AND NULL CDDR U
       THEN <<PRIN2!* CAR U; PRIN2!* " "; PRIN2!* CADR U>>
      ELSE MAPRIN U;
     PRIN2!* " invalid as "; PRIN2!* V;
     TERPRI!* NIL; ERFG!* := T; ERROR1()>>;


%*********************************************************************
%  ALGEBRAIC MODE FUNCTIONS AND DECLARATIONS REFERENCED IN SECTION 1
%********************************************************************;

%SYMBOLIC PROCEDURE APROC(U,V);
%   IF NULL U THEN NIL
%    ELSE IF ATOM U
%     THEN IF NUMBERP U AND FIXP U THEN U ELSE LIST(V,MKARG U)
%    ELSE IF FLAGP(CAR U,'NOCHANGE) OR GET(CAR U,'STAT) THEN U
%    ELSE IF FLAGP(CAR U,'BOOLEAN)
%     THEN CAR U . FOR EACH J IN CDR U COLLECT APROC(J,'REVAL)
%    ELSE IF CDR U AND EQCAR(CADR U,'QUOTE) THEN U
%    ELSE LIST(V,MKARG U);

SYMBOLIC PROCEDURE FORMINPUT(U,VARS,MODE);
   BEGIN SCALAR X;
      IF X := ASSOC(CAR U,INPUTBUFLIS!*) THEN RETURN CDR X
       ELSE REDERR LIST("Entry",CAR U,"not found")
   END;

PUT('INPUT,'FORMFN,'FORMINPUT);

SYMBOLIC PROCEDURE FORMWS(U,VARS,MODE);
   BEGIN SCALAR X;
      IF X := ASSOC(CAR U,RESULTBUFLIS!*) THEN RETURN MKQUOTE CDR X
       ELSE REDERR LIST("Entry",CAR U,"not found")
   END;

PUT('WS,'FORMFN,'FORMWS);

FLAG ('(AEVAL ARRAYFN COND FLAG GETEL GO PROG PROGN PROG2 RETURN
	SETQ SETK SETEL VARPRI),'NOCHANGE);
   %NB: FLAG IS NEEDED IN ALGEBRAIC PROC/OPERATOR DEFINITION;

FLAG ('(OR AND NOT MEMBER MEMQ EQUAL NEQ EQ GEQ GREATERP LEQ
	FIXP LESSP NUMBERP ORDP),'BOOLEAN);

FLAG ('(OR AND NOT),'BOOLARGS);

DEFLIST ('((SUM (ADDSQ . (NIL . 1))) (PRODUCT (MULTSQ . (1 . 1)))),
	 'BIN);

FLAG ('(SUM PRODUCT),'DELIM);

FLAG ('(SUM PRODUCT),'NODEL);

DEFLIST ('((EXP ((NIL (RMSUBS1)) (T (RMSUBS))))
	(FACTOR ((NIL (SETQ !*EXP T))
		 (T (SETQ !*EXP NIL) (RMSUBS))))
	(FORT ((NIL (SETQ !*NAT NAT!*!*)) (T (SETQ !*NAT NIL))))
	(GCD ((T (RMSUBS))))
	(MCD ((NIL (RMSUBS)) (T (RMSUBS))))
	(NAT ((NIL (SETQ NAT!*!* NIL)) (T (SETQ NAT!*!* T))))
	(NUMVAL ((T (RMSUBS)) (NIL (SETDMODE NIL))))
	(VAL ((T (RMSUBS))))
	(FLOAT ((T (RMSUBS))))),'SIMPFG);


%*********************************************************************
%      SELECTORS AND CONSTRUCTORS USED IN ALGEBRAIC CALCULATIONS
%********************************************************************;

NEWTOK '((!. !+) ADD);
NEWTOK '((!. !*) MULT);
NEWTOK '((!. !* !*) TO);
NEWTOK '((!. !/) OVER);

INFIX TO,.*,.+,./;

SMACRO PROCEDURE U.+V; %standard (polynomial) addition constructor;
   U . V;

SMACRO PROCEDURE LC U;	 %leading coefficient of standard form;
   CDAR U;

SMACRO PROCEDURE LDEG U; %leading degree of standard form;
   CDAAR U;

SMACRO PROCEDURE LT U;	 %leading term of standard form;
   CAR U;

SMACRO PROCEDURE U.*V;	%standard form multiplication constructor;
   U . V;

SMACRO PROCEDURE MVAR U; %main variable of standard form;
   CAAAR U;

SMACRO PROCEDURE LPOW U; %leading power of standard form;
   CAAR U;

SMACRO PROCEDURE PDEG U;
   %returns the degree of the power U;
   CDR U;

SMACRO PROCEDURE RED U; %reductum of standard form;
   CDR U;

SMACRO PROCEDURE TC U;	 %coefficient of standard term;
   CDR U;

SMACRO PROCEDURE TDEG U; %degree of standard term;
   CDAR U;

SMACRO PROCEDURE TPOW U; %power of standard term;
   CAR U;

SMACRO PROCEDURE TVAR U; %main variable of a standard term;
   CAAR U;

SMACRO PROCEDURE NUMR U; %numerator of standard quotient;
   CAR U;

SMACRO PROCEDURE DENR U; %denominator of standard quotient;
   CDR U;

SMACRO PROCEDURE U ./ V; %constructor for standard quotient;
   U . V;


%*********************************************************************
%     MACROS AND PROCEDURES FOR CONVERTING BETWEEN VARIOUS FORMS
%********************************************************************;

SYMBOLIC PROCEDURE !*A2F U;
   %U is an algebraic expression. Value is the equivalent form
   %or an error if conversion is not possible;
   !*Q2F SIMP!* U;

SYMBOLIC PROCEDURE !*A2K U;
   %U is an algebraic expression. Value is the equivalent kernel
   %or an error if conversion is not possible.
   %earlier versions used SIMP0;
   BEGIN SCALAR X;
      IF KERNP(X := SIMP!* U) THEN RETURN MVAR NUMR X
       ELSE TYPERR(U,'kernel)
   END;

SMACRO PROCEDURE !*F2A U; PREPF U;

SMACRO PROCEDURE !*F2Q U;
   %U is a standard form, value is a standard quotient;
   U . 1;

SMACRO PROCEDURE !*K2F U;
   %U is a kernel, value is a standard form;
   LIST (TO(U,1) . 1);

SMACRO PROCEDURE !*K2Q U;
   %U is a kernel, value is a standard quotient;
   LIST(TO(U,1) . 1) . 1;

SYMBOLIC PROCEDURE !*N2F U;
   %U is a number. Value is a standard form;
   IF ZEROP U THEN NIL ELSE U;

SMACRO PROCEDURE !*P2F U;
   %U is a standard power, value is a standard form;
   LIST (U . 1);

SMACRO PROCEDURE !*P2Q U;
   %U is a standard power, value is a standard quotient;
   LIST(U . 1) . 1;

SYMBOLIC PROCEDURE !*Q2F U;
   %U is a standard quotient, value is a standard form;
   IF DENR U=1 THEN NUMR U ELSE TYPERR(PREPSQ U,'polynomial);

SYMBOLIC PROCEDURE !*Q2K U;
   %U is a standard quotient, value is a kernel or an error if
   %conversion not possible;
   IF KERNP U THEN MVAR NUMR U
    ELSE TYPERR(PREPSQ U,'kernel);

SMACRO PROCEDURE !*T2F U;
   %U is a standard term, value is a standard form;
   LIST U;

SMACRO PROCEDURE !*T2Q U;
   %U is a standard term, value is a standard quotient;
   LIST U . 1;


%*********************************************************************
%	  FUNCTIONS FOR ALGEBRAIC EVALUATION OF PREFIX FORMS
%********************************************************************;

SYMBOLIC PROCEDURE REVAL U;
   REVAL1(U,T);

SYMBOLIC PROCEDURE AEVAL U;
   REVAL1(U,NIL);

SYMBOLIC PROCEDURE REVAL1(U,V);
   BEGIN SCALAR ALGLIST!*,X,Y;
    LOOP:
	IF STRINGP U THEN RETURN U
	 ELSE IF NUMBERP U AND FIXP U
	  THEN IF MOD!* THEN GO TO B ELSE RETURN U
	 ELSE IF ATOM U THEN NIL
	 ELSE IF CAR U EQ '!*COMMA!* THEN ERRPRI2(U,T)
	 ELSE IF CAR U EQ '!*SQ THEN GO TO B
	 ELSE IF ARRAYP CAR U
	  THEN <<U := GETELV U; GO TO LOOP>>;
	X := LIST U;
	Y := TYPL!*;
    A:	IF NULL Y THEN GO TO B
	 ELSE IF APPLY(CAR Y,X)
	  THEN RETURN APPLY(GET(CAR Y,'EVFN),X);
	Y := CDR Y;
	GO TO A;
    B:	U := SIMP!* U;
	IF NULL V THEN RETURN MK!*SQ U;
	U := PREPSQX U;
	RETURN IF EQCAR(U,'MINUS) AND NUMBERP CADR U THEN -CADR U
		ELSE U
   END;

SYMBOLIC PROCEDURE PREPSQX U;
   IF !*INTSTR THEN PREPSQ!* U ELSE PREPSQ U;

SYMBOLIC PROCEDURE IEVAL U;
   %returns algebraic value of U if U is an integer or an error;
   BEGIN
      IF NUMBERP U
	THEN IF FIXP U THEN RETURN U ELSE TYPERR(U,"integer")
       ELSE IF NOT ATOM U AND ARRAYP CAR U THEN U := GETELV U;
      U := SIMP!* U;
      IF DENR U NEQ 1 OR NOT ATOM NUMR U
	THEN TYPERR(PREPSQ U,"integer");
      U := NUMR U;
      IF NULL U THEN U := 0;
      RETURN U
   END;

SYMBOLIC PROCEDURE GETELV U;
   %returns the value of the array element U;
   GETEL(CAR U . FOR EACH X IN CDR U COLLECT IEVAL X);

SYMBOLIC PROCEDURE SETELV(U,V);
   SETEL(CAR U . FOR EACH X IN CDR U COLLECT IEVAL X,V);

SYMBOLIC PROCEDURE REVLIS U; FOR EACH J IN U COLLECT REVAL J;

SYMBOLIC PROCEDURE REVOP1 U;
   IF !*VAL THEN CAR U . REVLIS CDR U ELSE U;

SYMBOLIC PROCEDURE MK!*SQ U;
   IF NULL NUMR U THEN 0
    ELSE IF ATOM NUMR U AND DENR U=1 THEN NUMR U
    ELSE '!*SQ . EXPCHK U . IF !*RESUBS THEN !*SQVAR!* ELSE LIST NIL;

SYMBOLIC PROCEDURE EXPCHK U;
   IF !*EXP THEN U ELSE CANPROD(MKPROD!* NUMR U,MKPROD!* DENR U);


%*********************************************************************
%             EVALUATION FUNCTIONS FOR BOOLEAN OPERATORS
%********************************************************************;

SYMBOLIC PROCEDURE EVALEQUAL(U,V);
   (LAMBDA X; NUMBERP X AND ZEROP X) REVAL LIST('DIFFERENCE,U,V);

PUT('EQUAL,'BOOLFN,'EVALEQUAL);

SYMBOLIC PROCEDURE EVALGREATERP(U,V);
   (LAMBDA X;
    ATOM DENR X AND DOMAINP NUMR X AND NUMR X AND !:MINUSP NUMR X) 
	SIMP!* LIST('DIFFERENCE,V,U);

PUT('GREATERP,'BOOLFN,'EVALGREATERP);

SYMBOLIC PROCEDURE EVALGEQ(U,V); NOT EVALLESSP(U,V);

PUT('GEQ,'BOOLFN,'EVALGEQ);

SYMBOLIC PROCEDURE EVALLESSP(U,V);
   (LAMBDA X;
    ATOM DENR X AND DOMAINP NUMR X AND NUMR X AND !:MINUSP NUMR X) 
	SIMP!* LIST('DIFFERENCE,U,V);

PUT('LESSP,'BOOLFN,'EVALLESSP);

SYMBOLIC PROCEDURE EVALLEQ(U,V); NOT EVALGREATERP(U,V);

PUT('LEQ,'BOOLFN,'EVALLEQ);

SYMBOLIC PROCEDURE EVALNEQ(U,V); NOT EVALEQUAL(U,V);

PUT('NEQ,'BOOLFN,'EVALNEQ);

SYMBOLIC PROCEDURE EVALNUMBERP U; 
   (LAMBDA X; ATOM DENR X AND DOMAINP NUMR X) SIMP!* U;

PUT('NUMBERP,'BOOLFN,'EVALNUMBERP);


%*********************************************************************
%      FUNCTIONS FOR CONVERTING PREFIX FORMS INTO CANONICAL FORM
%********************************************************************;

SYMBOLIC PROCEDURE SIMP!* U;
   BEGIN SCALAR X;
	IF EQCAR(U,'!*SQ) AND CADDR U THEN RETURN CADR U;
	X := MUL!* . !*SUB2;	%save current environment;
	MUL!* := NIL;
	U:= SIMP U;
    A:	IF NULL MUL!* THEN GO TO B;
	U:= APPLY(CAR MUL!*,LIST U);
	MUL!*:= CDR MUL!*;
	GO TO A;
    B:	MUL!* := CAR X;
	U := SUBS2 U;
	!*SUB2 := CDR X;
	RETURN U
   END;

SYMBOLIC PROCEDURE SUBS2 U;
   BEGIN SCALAR XEXP;
	IF NULL SUBFG!* THEN RETURN U
	 ELSE IF !*SUB2 OR POWLIS1!* THEN U := SUBS2Q U;
	IF NULL !*MATCH AND NULL SPLIS!* THEN RETURN U
	 ELSE IF NULL !*EXP
	  THEN <<XEXP:= T; !*EXP := T; U := RESIMP U>>;
	IF !*MATCH THEN U := SUBS3Q U;
	IF SPLIS!* THEN U := SUBS4Q U;
	IF XEXP THEN !*EXP := NIL;
	RETURN U
   END;

SYMBOLIC PROCEDURE SIMP U;
   BEGIN SCALAR X;
	IF ATOM U THEN RETURN SIMPATOM U
	 ELSE IF CAR U EQ '!*SQ AND CADDR U THEN RETURN CADR U
	 ELSE IF X := ASSOC(U,ALGLIST!*) THEN RETURN CDR X
	 ELSE IF NOT IDP CAR U THEN GO TO E
	 ELSE IF FLAGP(CAR U,'OPFN)
	  THEN RETURN !*SSAVE(SIMP EVAL(CAR U . FOR EACH J IN
			     (IF FLAGP(CAR U,'NOVAL) THEN CDR U
			       ELSE REVLIS CDR U) COLLECT MKQUOTE J),U)
	 ELSE IF X := GET(CAR U,'POLYFN)
	  THEN RETURN !*SSAVE(!*F2Q APPLY(X,
			FOR EACH J IN CDR U COLLECT !*Q2F SIMP!* J),
			U)
	 ELSE IF GET(CAR U,'OPMTCH)
		AND NOT(GET(CAR U,'SIMPFN) EQ 'SIMPIDEN)
		AND (X := OPMTCH REVOP1 U)
	  THEN RETURN SIMP X
	 ELSE IF X := GET(CAR U,'SIMPFN)
	  THEN RETURN !*SSAVE(IF FLAGP(CAR U,'FULL) OR X EQ 'SIMPIDEN
			THEN APPLY(X,LIST U)
		       ELSE APPLY(X,LIST CDR U),U)
	 ELSE IF ARRAYP CAR U
	  THEN RETURN !*SSAVE(SIMP GETELV U,U)
	 ELSE IF (X := GET(CAR U,'MATRIX)) THEN GO TO M
	 ELSE IF FLAGP(CAR U,'BOOLEAN)
	  THEN TYPERR(GETINFIX CAR U,"algebraic operator")
	 ELSE IF GET(CAR U,'INFIX) THEN GO TO E
	 ELSE IF FLAGP(CAR U,'NOCHANGE)
	  THEN RETURN !*SSAVE(SIMP EVAL U,U)
	 ELSE <<REDMSG(CAR U,"operator"); MKOP CAR U; RETURN SIMP U>>;
    M:  IF NOT EQCAR(X,'MAT) THEN REDERR LIST("Matrix",CAR U,"not set")
	 ELSE IF NOT NUMLIS (U := REVLIS CDR U) OR LENGTH U NEQ 2
	 THEN GO TO E;
	RETURN !*SSAVE(SIMP NTH(NTH(CDR X,CAR U),CADR U),U);
    E:	IF EQCAR(CAR U,'MAT) THEN <<X := CAR U; GO TO M>>
	 ELSE ERRPRI2(GETINFIX U,T)
   END;

SYMBOLIC PROCEDURE GETINFIX U;
   %finds infix symbol for U if it exists;
   BEGIN SCALAR X; 
      RETURN IF X := GET(U,'PRTCH) THEN CAR X ELSE U
   END;

SYMBOLIC PROCEDURE !*SSAVE(U,V);
   BEGIN
      ALGLIST!* := (V . U) . ALGLIST!*;
      RETURN U
   END;

SYMBOLIC PROCEDURE NUMLIS U;
   NULL U OR (NUMBERP CAR U AND NUMLIS CDR U);

SYMBOLIC PROCEDURE SIMPATOM U;
   IF NULL U THEN NIL ./ 1
    ELSE IF NUMBERP U 
     THEN IF ZEROP U THEN NIL ./ 1
	   ELSE IF NOT FIXP U
	    THEN !*D2Q IF NULL DMODE!* THEN !*FT2RN MKFLOAT U
			ELSE IF DMODE!* EQ '!:FT!: THEN MKFLOAT U
			ELSE APPLY(GET('!:FT!:,DMODE!*),LIST MKFLOAT U)
           ELSE IF DMODE!* AND FLAGP(DMODE!*,'CONVERT)
            THEN !*D2Q APPLY(GET(DMODE!*,'I2D),LIST U)
           ELSE U ./ 1
    ELSE IF FLAGP(U,'SHARE) THEN SIMP EVAL U
    ELSE BEGIN SCALAR Z;
      IF !*NUMVAL AND (Z := GET(U,'DOMAINFN))
	THEN <<SETDMODE GET(U,'TARGETMODE);
	       RETURN !*D2Q APPLY(Z,NIL)>>;
      FOR EACH X IN TYPL!* DO IF APPLY(X,LIST U) THEN TYPERR(U,'scalar);
      RETURN MKSQ(U,1)
   END;

SYMBOLIC PROCEDURE MKOP U;
   BEGIN SCALAR X;
	IF NULL U THEN TYPERR("Local variable","operator")
	 ELSE IF (X := GETTYPE U) EQ 'OPERATOR
	  THEN LPRIM LIST(U,"already defined as operator")
	 ELSE IF X AND NOT X EQ 'PROCEDURE THEN TYPERR(U,'operator)
	 ELSE IF U MEMQ FRLIS!* THEN TYPERR(U,"free variable")
	 ELSE PUT(U,'SIMPFN,'SIMPIDEN)
   END;

SYMBOLIC PROCEDURE SIMPCAR U;
   SIMP CAR U;

PUT('QUOTE,'SIMPFN,'SIMPCAR);

FLAGOP SHARE;

FLAG('(WS !*MODE),'SHARE);


%*********************************************************************
%	    SIMPLIFICATION FUNCTIONS FOR EXPLICIT OPERATORS
%********************************************************************;

SYMBOLIC PROCEDURE SIMPABS U;
   (LAMBDA X; ABSF NUMR X ./ DENR X) SIMPCAR U;

PUT('ABS,'SIMPFN,'SIMPABS);

SYMBOLIC PROCEDURE SIMPEXPT U;
   BEGIN SCALAR FLG,M,N,X;
	IF DMODE!* EQ '!:MOD!: THEN <<X := T; DMODE!* := NIL>>;
	 %exponents must not use modular arithmetic;
	N := SIMP!* CARX(CDR U,'EXPT);
	IF X THEN DMODE!* := '!:MOD!:;
	U := CAR U;
    A:	M := NUMR N;
	IF NOT ATOM M OR DENR N NEQ 1 THEN GO TO NONUMEXP
	 ELSE IF NULL M
	  THEN RETURN IF NUMBERP U AND ZEROP U
			THEN REDERR " 0**0 formed"
		       ELSE 1 ./ 1
 	 ELSE IF ONEP U THEN RETURN 1 ./ 1;
	X := SIMP U;
	   %we could use simp!* here, except that it messes up the
	   %handling of gamma matrix expressions;
	IF !*NUMVAL AND DOMAINP NUMR X AND DOMAINP DENR X
	    AND NOT (ATOM NUMR X AND ATOM DENR X)
	  THEN RETURN NUMEXPT(MK!*SQ X,M,1)
	 ELSE IF NOT M<0 THEN RETURN EXPTSQ(X,M)
	 ELSE IF !*MCD THEN RETURN INVSQ EXPTSQ(X,-M)
	 ELSE RETURN EXPSQ(X,M);   %using OFF EXP code here;
		%there may be a pattern matching problem though;
    NONUMEXP:
	IF ONEP U THEN RETURN 1 ./ 1
	 ELSE IF ATOM U THEN GO TO A2
	 ELSE IF CAR U EQ 'TIMES
	  THEN <<N := PREPSQ N;
		 X := 1 ./ 1;
		 FOR EACH Z IN CDR U DO
		   X := MULTSQ(SIMPEXPT LIST(Z,N),X);
		 RETURN X>>
	 ELSE IF CAR U EQ 'QUOTIENT
	  THEN <<IF NOT FLG AND !*MCD THEN GO TO A2;
		 N := PREPSQ N;
		 RETURN MULTSQ(SIMPEXPT LIST(CADR U,N),
		          SIMPEXPT LIST(CADDR U,LIST('MINUS,N)))>>
	 ELSE IF CAR U EQ 'EXPT
	  THEN <<N := MULTSQ(SIMP CADDR U,N);
		 U := CADR U;
		 X := NIL;
		 GO TO A>>
	 ELSE IF CAR U EQ 'MINUS AND NUMBERP M AND DENR N=1
	  THEN RETURN MULTSQ(SIMPEXPT LIST(-1,M),
			     SIMPEXPT LIST(CADR U,M));
    A2: IF NULL FLG
	  THEN <<FLG := T;
	         U := PREPSQ IF NULL X THEN (X := SIMP!* U) ELSE X;
	         GO TO NONUMEXP>>
	 ELSE IF NUMBERP U AND ZEROP U THEN RETURN NIL ./ 1
	 ELSE IF NOT NUMBERP M THEN M := PREPF M;
	IF M MEMQ FRLIS!* THEN RETURN LIST ((U . M) . 1) . 1;
	   %"power" is not unique here;
	N := PREPF CDR N;
	IF !*MCD OR CDR X NEQ 1 OR NOT NUMBERP M OR N NEQ 1
	  OR ATOM U THEN GO TO C
   %	 ELSE IF MINUSF CAR X THEN RETURN MULTSQ(SIMPEXPT LIST(-1,M),
   %				SIMPEXPT LIST(PREPF NEGF CAR X,M));
	 ELSE IF CAR U EQ 'PLUS OR NOT !*MCD AND N=1
	  THEN RETURN MKSQ(U,M); %to make pattern matching work;
    C:	IF !*NUMVAL AND NUMTYPEP U AND NUMTYPEP M AND NUMTYPEP N
	  THEN RETURN NUMEXPT(U,M,N)
         ELSE RETURN SIMPX1(U,M,N)
   END;

SYMBOLIC PROCEDURE NUMEXPT(U,M,N);
   %U,M and N are all numbers. Result is standard quotient for U**(M/N);
   BEGIN SCALAR X;
      RETURN IF X := TARGETCONV(LIST(U,M,N),'BIGFLOAT)
	THEN !*D2Q IF N=1 AND ATOM M AND FIXP M THEN TEXPT!:(CAR X,M)
		    ELSE TEXPT!:ANY(CAR X,
			  IF N=1 THEN CADR X 
			   ELSE BFQUOTIENT!:(CADR X,CADDR X))
       ELSE SIMPX1(U,M,N)
   END;

SYMBOLIC PROCEDURE IEXPT(U,N);
   IF NULL MOD!* THEN U**N
    ELSE IF N<0 THEN CEXPT(CRECIP U,-N)
    ELSE CEXPT(U,N);

PUT('EXPT,'SIMPFN,'SIMPEXPT);

SYMBOLIC PROCEDURE SIMPX1(U,M,N);
   %U,M and N are prefix expressions;
   %Value is the standard quotient expression for U**(M/N);
	BEGIN SCALAR FLG,X,Z;
	IF NUMBERP M AND NUMBERP N
	   OR NULL SMEMQLP(FRLIS!*,M) OR NULL SMEMQLP(FRLIS!*,N)
	  THEN GO TO A;
	EXPTP!* := T;
	RETURN !*K2Q LIST('EXPT,U,IF N=1 THEN M
				   ELSE LIST('QUOTIENT,M,N));
    A:  IF NUMBERP M THEN IF MINUSP M THEN <<M := -M; GO TO MNS>>
			   ELSE IF FIXP M THEN GO TO E
			   ELSE GO TO B
	 ELSE IF ATOM M THEN GO TO B
	 ELSE IF CAR M EQ 'MINUS THEN <<M := CADR M; GO TO MNS>>
	 ELSE IF CAR M EQ 'PLUS THEN GO TO PLS
	 ELSE IF CAR M EQ 'TIMES AND NUMBERP CADR M AND FIXP CADR M
		AND NUMBERP N
	  THEN GO TO TMS;
    B:	Z := 1;
    C:	IF IDP U AND NOT FLAGP(U,'USED!*) THEN FLAG(LIST U,'USED!*);
	U := LIST('EXPT,U,IF N=1 THEN M ELSE LIST('QUOTIENT,M,N));
	IF NOT U MEMBER EXPTL!* THEN EXPTL!* := U . EXPTL!*;
    D:	RETURN MKSQ(U,IF FLG THEN -Z ELSE Z); %U is already in lowest
	%terms;
    E:	IF NUMBERP N AND FIXP N THEN GO TO INT;
	Z := M;
	M := 1;
	GO TO C;
    MNS: IF !*MCD THEN RETURN INVSQ SIMPX1(U,M,N);
	FLG := NOT FLG;
	GO TO A;
    PLS: Z := 1 ./ 1;
    PL1: M := CDR M;
	IF NULL M THEN RETURN Z;
	Z := MULTSQ(SIMPEXPT LIST(U,
			LIST('QUOTIENT,IF FLG THEN LIST('MINUS,CAR M)
					ELSE CAR M,N)),
		    Z);
	GO TO PL1;
    TMS: Z := GCDN(N,CADR M);
	N := N/Z;
	Z := CADR M/Z;
	M := RETIMES CDDR M;
	GO TO C;
    INT:Z := DIVIDE(M,N);
	IF CDR Z<0 THEN Z:= (CAR Z - 1) . (CDR Z+N);
	X := SIMPEXPT LIST(U,CAR Z);
	IF CDR Z=0 THEN RETURN X
	 ELSE IF N=2 THEN RETURN MULTSQ(X,SIMPSQRT LIST U)
	 ELSE RETURN MULTSQ(X,EXPTSQ(SIMPRAD(SIMP!* U,N),CDR Z))
   END;

SYMBOLIC PROCEDURE EXPSQ(U,N);
   %RAISES STANDARD QUOTIENT U TO NEGATIVE POWER N WITH EXP OFF;
   MULTF(EXPF(NUMR U,N),MKSFPF(DENR U,-N)) ./ 1;

SYMBOLIC PROCEDURE EXPF(U,N);
   %U is a standard form. Value is standard form of U raised to
   %negative integer power N. MCD is assumed off;
   %what if U is invertable?;
   IF NULL U THEN NIL
    ELSE IF ATOM U THEN MKRN(1,U**(-N))
    ELSE IF DOMAINP U THEN !:EXPT(U,N)
    ELSE IF RED U THEN MKSP!*(U,N)
    ELSE (LAMBDA X; IF X>0 AND SFP MVAR U
		     THEN MULTF(EXPTF(MVAR U,X),EXPF(LC U,N))
		    ELSE MVAR U TO X .* EXPF(LC U,N) .+ NIL)
	 (LDEG U*N);

SYMBOLIC PROCEDURE SIMPRAD(U,N);
   %simplifies radical expressions;
   BEGIN SCALAR X,Y,Z;
      X := RADF(NUMR U,N);
      Y := RADF(DENR U,N);
      Z := MULTSQ(CAR X ./ 1,1 ./ CAR Y);
      Z := MULTSQ(MULTSQ(MKROOTLF(CDR X,N) ./ 1,
			 1 ./ MKROOTLF(CDR Y,N)),
		  Z);
      RETURN Z
   END;

SYMBOLIC PROCEDURE MKROOTLF(U,N);
   %U is a list of prefix expressions, N an integer.
   %Value is standard form for U**(1/N);
   IF NULL U THEN 1 ELSE MULTF(MKROOTF(CAR U,N),MKROOTLF(CDR U,N));

SYMBOLIC PROCEDURE MKROOTF(U,N);
   %U is a prefix expression, N an integer.
   %Value is a standard form for U**(1/N);
   !*P2F IF EQCAR(U,'EXPT) AND FIXP CADDR U
	THEN MKSP(IF N=2 THEN MKSQRT CADR U
		   ELSE LIST('EXPT,CADR U,LIST('QUOTIENT,1,N)),CADDR U)
       ELSE MKSP(IF N=2 THEN MKSQRT U
		  ELSE LIST('EXPT,U,LIST('QUOTIENT,1,N)),1);

COMMENT The following three procedures return a partitioned root
	expression, which is a dotted pair of integral part (a standard
	form) and radical part (a list of prefix expressions). The whole
	structure represents U**(1/N);

SYMBOLIC PROCEDURE RADF(U,N);
   %U is a standard form, N a positive integer. Value is a partitioned
   %root expression for U**(1/N);
   BEGIN SCALAR IPART,RPART,X,Y,!*GCD;
      IF NULL U THEN RETURN LIST U;
      !*GCD := T;
      IPART := 1;
      WHILE NOT DOMAINP U DO
	 <<Y := COMFAC U;
	   IF CAR Y
	     THEN <<X := DIVIDE(PDEG CAR Y,N);
		    IF CAR X NEQ 0
		      THEN IPART:=MULTF(!*P2F(MVAR U TO CAR X),IPART);
		    IF CDR X NEQ 0
		      THEN RPART :=
			   MKEXPT(IF SFP MVAR U THEN PREPF MVAR U
				   ELSE MVAR U,CDR X) . RPART>>;
	   X := QUOTF1(U,COMFAC!-TO!-POLY Y);
	   U := CDR Y;
	   IF MINUSF X THEN <<X := NEGF X; U := NEGF U>>;
	   IF X NEQ 1
	     THEN <<X := RADF1(SQFRF X,N);
	   IPART := MULTF(CAR X,IPART);
	   RPART := APPEND(RPART,CDR X)>>>>;
      IF U NEQ 1
	THEN <<X := RADD(U,N);
	       IPART := MULTF(CAR X,IPART);
	       RPART := APPEND(CDR X,RPART)>>;
      RETURN IPART . RPART
   END;

SYMBOLIC PROCEDURE RADF1(U,N);
   %U is a form_power list, N a positive integer. Value is a
   %partitioned root expression for U**(1/N);
   BEGIN SCALAR IPART,RPART,X;
      IPART := 1;
      FOR EACH Z IN U DO
	 <<X := DIVIDE(CDR Z,N);
	   IF NOT(CAR X=0)
		    THEN IPART := MULTF(EXPTF(CAR Z,CAR X),IPART);
		  IF NOT(CDR X=0)
		    THEN RPART := MKEXPT(PREPSQ!*(CAR Z ./ 1),CDR X)
				   . RPART>>;
      RETURN IPART . RPART
   END;

SYMBOLIC PROCEDURE RADD(U,N);
   %U is a domain element, N an integer.
   %Value is a partitioned root expression for U**(1/N);
   BEGIN SCALAR IPART,X;
      IPART := 1;
      IF NOT ATOM U THEN RETURN LIST(1,U)
       ELSE IF U<0
	THEN IF N=2 THEN <<IPART := !*K2F 'I; U := -U>>
	 ELSE IF REMAINDER(N,2)=1 THEN <<IPART := -1; U := -U>>
	 ELSE RETURN LIST(1,U);
      X := NROOTN(U,N);
      RETURN IF CDR X=1 THEN LIST MULTD(CAR X,IPART)
	      ELSE LIST(MULTD(CAR X,IPART),CDR X)
   END;

SYMBOLIC PROCEDURE IROOT(M,N);
   %M and N are positive integers.
   %If M**(1/N) is an integer, this value is returned, otherwise NIL;
   BEGIN SCALAR X,X1,BK;
      IF M=0 THEN RETURN M;
      X := 10**CEILING(LENGTHC M,N);   %first guess;
   A: X1 := X**(N-1);
      BK := X-M/X1;
      IF BK<0 THEN RETURN NIL
       ELSE IF BK=0 THEN RETURN IF X1*X=M THEN X ELSE NIL;
      X := X-CEILING(BK,N);
      GO TO A
   END;

SYMBOLIC PROCEDURE CEILING(M,N);
   %M and N are positive integers. Value is ceiling of (M/N) (i.e.,
   %least integer greater or equal to M/N);
   (LAMBDA X; IF CDR X=0 THEN CAR X ELSE CAR X+1) DIVIDE(M,N);

SYMBOLIC PROCEDURE MKEXPT(U,N);
   IF N=1 THEN U ELSE LIST('EXPT,U,N);

SYMBOLIC PROCEDURE NROOTN(N,X); 
   %N is an integer, X a positive integer. Value is a pair
   %of integers I,J such that I*J**(1/X)=N**(1/X);
   BEGIN SCALAR I,J,R,SIGNN; 
      R := 1; 
      IF N<0
        THEN <<N := -N; 
               IF REMAINDER(X,2)=0 THEN SIGNN := T ELSE R := -1>>; 
      J := 2**X; 
      WHILE REMAINDER(N,J)=0 DO <<N := N/J; R := R*2>>; 
      I := 3; 
      J := 3**X; 
      WHILE J<=N DO 
         <<WHILE REMAINDER(N,J)=0 DO <<N := N/J; R := R*I>>; 
           IF REMAINDER(I,3)=1 THEN I := I+4 ELSE I := I+2; 
           J := I**X>>; 
      IF SIGNN THEN N := -N; 
      RETURN R . N
   END;

SYMBOLIC PROCEDURE SIMPIDEN U;
   BEGIN SCALAR Y,Z;
	U:= REVOP1 U;
	IF FLAGP(CAR U,'NONCOM) THEN NCMP!* := T;
	IF NULL SUBFG!* THEN GO TO C
	 ELSE IF FLAGP(CAR U,'LINEAR) AND (Z := FORMLNR U) NEQ U
	  THEN RETURN SIMP Z
	 ELSE IF Z := OPMTCH U THEN RETURN SIMP Z
	 ELSE IF Z := NUMVALCHK U THEN RETURN Z;
    C:	IF FLAGP(CAR U,'SYMMETRIC) THEN U := CAR U . ORDN CDR U
	 ELSE IF FLAGP(CAR U,'ANTISYMMETRIC)
	  THEN <<IF REPEATS CDR U THEN RETURN (NIL ./ 1)
		  ELSE IF NOT PERMP(Z:= ORDN CDR U,CDR U) THEN Y := T;
		 U := CAR U . Z>>;
	U := MKSQ(U,1);
	RETURN IF Y THEN NEGSQ U ELSE U
   END;

SYMBOLIC PROCEDURE NUMVALCHK U;
   BEGIN SCALAR Y,Z;
      IF NULL !*NUMVAL THEN RETURN NIL
       ELSE IF ATOM U THEN RETURN NIL
       ELSE IF (Z := GET(CAR U,'DOMAINFN))
		 AND DOMAINLISP CDR U
		AND (Y := TARGETCONV(CDR U,GET(CAR U,'TARGETMODE)))
	  THEN <<SETDMODE GET(CAR U,'TARGETMODE);
		 RETURN !*D2Q APPLY(Z,Y)>>
       ELSE RETURN NIL
   END;

SYMBOLIC PROCEDURE NUMTYPEP U;
   %returns true if U is a possible number, NIL otherwise;
   IF ATOM U THEN NUMBERP U
    ELSE IF GET(CAR U,'DNAME) THEN U
    ELSE IF CAR U EQ 'MINUS THEN NUMTYPEP CADR U
    ELSE IF CAR U EQ 'QUOTIENT THEN NUMTYPEP CADR U AND NUMTYPEP CADDR U
    ELSE NIL;

SYMBOLIC PROCEDURE DOMAINLISP U;
   %true if U is a list of domain element numbers, NIL otherwise;
   IF NULL U THEN T ELSE NUMTYPEP CAR U AND DOMAINLISP CDR U;

SYMBOLIC PROCEDURE TARGETCONV(U,V);
   %U is a list of domain elements, V a domain mode;
   %if all elements of U can be converted to mode V, a list of the
   %converted elements is returned, otherwise NIL is returned;
   BEGIN SCALAR X,Y,Z;
      V := GET(V,'TAG);
    A: IF NULL U THEN RETURN REVERSIP X
        ELSE IF ATOM (Z := NUMR SIMPCAR U)
	THEN X := APPLY(GET(V,'I2D),LIST IF NULL Z THEN 0 ELSE Z) . X
       ELSE IF CAR Z EQ V THEN X := Z . X
       ELSE IF Y := GET(CAR Z,V)
	THEN X := APPLY(Y,LIST Z) . X
       ELSE RETURN NIL;
      U := CDR U;
      GO TO A
   END;

SYMBOLIC PROCEDURE SIMPDIFF U;
   ADDSQ(SIMPCAR U,SIMPMINUS CDR U);

PUT('DIFFERENCE,'SIMPFN,'SIMPDIFF);

SYMBOLIC PROCEDURE SIMPMINUS U;
   NEGSQ SIMP CARX(U,'MINUS);

PUT('MINUS,'SIMPFN,'SIMPMINUS);

SYMBOLIC PROCEDURE SIMPPLUS U;
   BEGIN SCALAR Z;
	Z := NIL ./ 1;
    A:	IF NULL U THEN RETURN Z;
	Z := ADDSQ(SIMPCAR U,Z);
	U := CDR U;
	GO TO A
   END;

PUT('PLUS,'SIMPFN,'SIMPPLUS);

SYMBOLIC PROCEDURE SIMPQUOT U;
   MULTSQ(SIMPCAR U,SIMPRECIP CDR U);

PUT('QUOTIENT,'SIMPFN,'SIMPQUOT);

SYMBOLIC PROCEDURE SIMPRECIP U;
   IF NULL !*MCD THEN SIMPEXPT LIST(CARX(U,'RECIP),-1)
    ELSE INVSQ SIMP CARX( U,'RECIP);

PUT('RECIP,'SIMPFN,'SIMPRECIP);

SYMBOLIC PROCEDURE SIMPSQRT U;
   BEGIN SCALAR X,Y;
      X := XSIMP CAR U;
      RETURN IF !*NUMVAL AND (Y := NUMVALCHK MKSQRT PREPSQ!* X)
	       THEN Y
        ELSE SIMPRAD(X,2)
   END;

SYMBOLIC PROCEDURE XSIMP U; EXPCHK SIMP!* U;

SYMBOLIC PROCEDURE SIMPTIMES U;
   BEGIN SCALAR X,Y;
	IF TSTACK!* NEQ 0 OR NULL MUL!* THEN GO TO A0;
	Y := MUL!*;
	MUL!* := NIL;
    A0: TSTACK!* := TSTACK!*+1;
	X := SIMPCAR U;
    A:	U := CDR U;
	IF NULL NUMR X THEN GO TO C
	 ELSE IF NULL U THEN GO TO B;
	X := MULTSQ(X,SIMPCAR U);
	GO TO A;
    B:	IF NULL MUL!* OR TSTACK!*>1 THEN GO TO C;
	X:= APPLY(CAR MUL!*,LIST X);
	MUL!*:= CDR MUL!*;
	GO TO B;
    C:	TSTACK!* := TSTACK!*-1;
	IF TSTACK!* = 0 THEN MUL!* := Y;
	RETURN X;
   END;

PUT('TIMES,'SIMPFN,'SIMPTIMES);

SYMBOLIC PROCEDURE SIMPSUB U;
   BEGIN SCALAR X,Z,Z1;
    A:	IF NULL CDR U THEN GO TO D
	 ELSE IF NOT EQEXPR CAR U THEN ERRPRI2(CAR U,T);
	X := CADAR U;
	Z1 := TYPL!*;
    B:	IF NULL Z1 THEN GO TO B1
	 ELSE IF APPLY(CAR Z1,LIST X) THEN GO TO C;
	Z1 := CDR Z1;
	GO TO B;
    B1: X := !*A2K X;
    C:	Z := (X . CADDAR U) . Z;
	U := CDR U;
	GO TO A;
    D:	U := SIMP!* CAR U;
	RETURN QUOTSQ(SUBF(NUMR U,Z),SUBF(DENR U,Z))
  END;

SYMBOLIC PROCEDURE RESIMP U;
   %U is a standard quotient.
   %Value is the resimplified standard quotient;
   QUOTSQ(SUBF1(NUMR U,NIL),SUBF1(DENR U,NIL));

PUT('SUB,'SIMPFN,'SIMPSUB);

SYMBOLIC PROCEDURE EQEXPR U;
   NOT ATOM U
      AND CAR U MEMQ '(EQ EQUAL) AND CDDR U AND NULL CDDDR U;

SYMBOLIC PROCEDURE SIMP!*SQ U;
   IF NULL CADR U THEN RESIMP CAR U ELSE CAR U;

PUT('!*SQ,'SIMPFN,'SIMP!*SQ);


%*********************************************************************
%  FUNCTIONS FOR DEFINING AND MANIPULATING POLYNOMIAL DOMAIN MODES
%********************************************************************;

GLOBAL '(DMODE!* DOMAINLIST!*);

SYMBOLIC PROCEDURE INITDMODE U;
   %checks that U is a valid domain mode, and sets up appropriate 
   %interfaces to the system;
   BEGIN
      DMODECHK U;
      PUT(U,'SIMPFG,LIST(LIST(T,LIST('SETDMODE,MKQUOTE U)),
			 '(NIL (SETDMODE NIL))))
   END;

SYMBOLIC PROCEDURE SETDMODE U;
   %Sets polynomial domain mode to U. If U is NIL, integers are used;
   BEGIN SCALAR X;
      IF NULL U THEN RETURN <<RMSUBS(); DMODE!* := NIL>>
       ELSE IF NULL(X := GET(U,'TAG))
	THEN REDERR LIST("Domain mode error:",U,"is not a domain mode")
       ELSE IF DMODE!* EQ X THEN RETURN NIL;
      RMSUBS();
      IF DMODE!*
	THEN LPRIM LIST("Domain mode",
			GET(DMODE!*,'DNAME),"changed to",U);
      IF U := GET(U,'MODULE!-NAME) THEN LOAD!-MODULE U;
      DMODE!* := X
   END;

SYMBOLIC PROCEDURE DMODECHK U;
   %checks to see if U has complete specification for a domain mode;
   BEGIN SCALAR Z;
      IF NOT(Z := GET(U,'TAG))
	THEN REDERR LIST("Domain mode error:","No tag for",Z)
       ELSE IF NOT(GET(Z,'DNAME) EQ U)
	THEN REDERR LIST("Domain mode error:",
			 "Inconsistent or missing DNAME for",Z)
       ELSE IF NOT Z MEMQ DOMAINLIST!*
	THEN REDERR LIST("Domain mode error:",
			 Z,"not on domain list");
      U := Z;
      FOR EACH X IN DOMAINLIST!*
	DO IF U=X THEN NIL
	    ELSE IF NOT(GET(U,X) OR GET(X,U))
	     THEN REDERR LIST("Domain mode error:",
			   "No conversion defined between",U,"and",X);
      Z := '(DIFFERENCE I2D MINUSP PLUS PREPFN QUOTIENT SPECPRN TIMES
	     ZEROP);
      IF NOT FLAGP(U,'FIELD) THEN Z := 'DIVIDE . 'GCD . Z;
      FOR EACH X IN Z DO IF NOT GET(U,X)
	     THEN REDERR LIST("Domain mode error:",
			      X,"is not defined for",U)
   END;


COMMENT *** General Support Functions ***;

SYMBOLIC PROCEDURE !*D2Q U;
   %converts domain element U into a standard quotient;
   IF EQCAR(U,'!:RN!:) AND !*MCD THEN CDR U ELSE U ./ 1;

SYMBOLIC PROCEDURE FIELDP U;
   %U is a domain element. Value is T if U is invertable, NIL
   %otherwise;
   NOT ATOM U AND FLAGP(CAR U,'FIELD);

SYMBOLIC PROCEDURE !:EXPT(U,N);
   %raises domain element U to power N.  Value is a domain element;
   IF NULL U THEN IF N=0 THEN REDERR "0/0 formed" ELSE NIL
    ELSE IF N=0 THEN 1
    ELSE IF N<0
     THEN !:RECIP !:EXPT(IF NOT FIELDP U THEN MKRATNUM U ELSE U,-N)
    ELSE IF ATOM U THEN U**N
    ELSE BEGIN SCALAR V,W,X;
      V := APPLY(GET(CAR U,'I2D),LIST 1);   %unit element;
      X := GET(CAR U,'TIMES);
   A: W := DIVIDE(N,2);
      IF CDR W=1 THEN V := APPLY(X,LIST(U,V));
      IF CAR W=0 THEN RETURN V;
      U := APPLY(X,LIST(U,U));
      N := CAR W;
      GO TO A
   END;

SYMBOLIC PROCEDURE !:MINUS U;
   %U is a domain element. Value is -U;
   IF ATOM U THEN -U ELSE DCOMBINE(U,-1,'TIMES);

SYMBOLIC PROCEDURE !:MINUSP U;
   IF ATOM U THEN MINUSP U ELSE APPLY(GET(CAR U,'MINUSP),LIST U);

GLOBAL '(!:PREC!:);

SYMBOLIC PROCEDURE !:ONEP U;
   %Allow for round-up of two in the last place in bigfloats;
   IF ATOM U THEN U=1
    ELSE IF !:ZEROP DCOMBINE(U,1,'DIFFERENCE) THEN T
    ELSE CAR U EQ '!:BF!:
       AND !:ZEROP DCOMBINE(BFPLUS!:(U,'!:BF!: . 2 . -!:PREC!:),
			    1,'DIFFERENCE);

SYMBOLIC PROCEDURE !:RECIP U;
   %U is an invertable domain element. Value is 1/U;
   IF NUMBERP U AND ABS U=1 THEN U ELSE DCOMBINE(1,U,'QUOTIENT);

SYMBOLIC PROCEDURE !:ZEROP U;
   %returns T if domain element U is 0, NIL otherwise;
   IF ATOM U THEN U=0 ELSE APPLY(GET(CAR U,'ZEROP),LIST U);

SYMBOLIC PROCEDURE DCOMBINE(U,V,FN);
   %U and V are domain elements, but not both atoms (integers).
   %FN is a binary function on domain elements;
   %Value is the domain element representing FN(U,V);
   IF ATOM U
     THEN APPLY(GET(CAR V,FN),LIST(APPLY(GET(CAR V,'I2D),LIST U),V))
    ELSE IF ATOM V
     THEN APPLY(GET(CAR U,FN),LIST(U,APPLY(GET(CAR U,'I2D),LIST V)))
    ELSE IF CAR U EQ CAR V THEN APPLY(GET(CAR U,FN),LIST(U,V))
    ELSE BEGIN SCALAR X;
     IF NOT(X := GET(CAR U,CAR V))
	THEN <<V := APPLY(GET(CAR V,CAR U),LIST V);
	       X := GET(CAR U,FN)>>
       ELSE <<U := APPLY(X,LIST U); X := GET(CAR V,FN)>>;
      RETURN APPLY(X,LIST(U,V))
   END;


COMMENT *** Tables for Various domain arithmetics ***:

Syntactically, such elements have the following form:

<domain element> := integer|(<domain identifier> . <domain structure>).

To introduce a new domain, we need to define:

1) A conversion function from integer to the given mode.

2) A conversion function from new mode to or from every other mode.

3) Particular instance of the binary operations +,- and * for this mode.

4) Particular instance of ZEROP, MINUSP for this mode.

5) If domain is a field, a quotient must be defined.
   If domain is a ring, a gcd and divide must be defined, and
   also a quotient function which returns NIL if the division fails.

6) A printing function for this mode.

7) A function to convert structure to an appropriate prefix form.

8) A reading function for this mode.

9) A DNAME property for the tag, and a TAG property for the DNAME

To facilitate this, all such modes should be listed in the global
variable DOMAINLIST!*;


COMMENT *** Tables for rational numbers ***;

FLUID '(!*RATIONAL);

DOMAINLIST!* := UNION('(!:RN!:),DOMAINLIST!*);
PUT('RATIONAL,'TAG,'!:RN!:);
PUT('!:RN!:,'DNAME,'RATIONAL);
FLAG('(!:RN!:),'FIELD);
PUT('!:RN!:,'I2D,'!*I2RN);
PUT('!:RN!:,'MINUSP,'RNMINUSP!:);
PUT('!:RN!:,'PLUS,'RNPLUS!:);
PUT('!:RN!:,'TIMES,'RNTIMES!:);
PUT('!:RN!:,'DIFFERENCE,'RNDIFFERENCE!:);
PUT('!:RN!:,'QUOTIENT,'RNQUOTIENT!:);
PUT('!:RN!:,'ZEROP,'RNZEROP!:);
PUT('!:RN!:,'PREPFN,'RNPREP!:);
PUT('!:RN!:,'SPECPRN,'RNPRIN);

SYMBOLIC PROCEDURE MKRATNUM U;
   %U is a domain element. Value is equivalent rational number;
   IF ATOM U THEN !*I2RN U ELSE APPLY(GET(CAR U,'!:RN!:),LIST U);

SYMBOLIC PROCEDURE MKRN(U,V);
   %converts two integers U and V into a rational number, an integer
   %or NIL;
   IF U=0 THEN NIL
    ELSE IF V<0 THEN MKRN(-U,-V)
    ELSE (LAMBDA M;
     	  (LAMBDA (N1,N2); IF N2=1 THEN N1 ELSE '!:RN!: . (N1 . N2))
     	    (U/M,V/M))
       GCDN(U,V);

SYMBOLIC PROCEDURE !*I2RN U;
   %converts integer U to rational number;
   '!:RN!: . (U . 1);

SYMBOLIC PROCEDURE RNMINUSP!: U; CADR U<0;

SYMBOLIC PROCEDURE RNPLUS!:(U,V);
   MKRN(CADR U*CDDR V+CDDR U*CADR V,CDDR U*CDDR V);

SYMBOLIC PROCEDURE RNTIMES!:(U,V);
   MKRN(CADR U*CADR V,CDDR U*CDDR V);

SYMBOLIC PROCEDURE RNDIFFERENCE!:(U,V);
   MKRN(CADR U*CDDR V-CDDR U*CADR V,CDDR U*CDDR V);

SYMBOLIC PROCEDURE RNQUOTIENT!:(U,V);
   MKRN(CADR U*CDDR V,CDDR U*CADR V);

SYMBOLIC PROCEDURE RNZEROP!: U; CADR U=0;

SYMBOLIC PROCEDURE RNPREP!: U;
   IF CDDR U=1 THEN CADR U ELSE LIST('QUOTIENT,CADR U,CDDR U);

SYMBOLIC PROCEDURE RNPRIN U; MAPRIN RNPREP!: U;

INITDMODE 'RATIONAL;


COMMENT *** Tables for floats ***;

DOMAINLIST!* := UNION('(!:FT!:),DOMAINLIST!*);
PUT('FLOAT,'TAG,'!:FT!:);
PUT('!:FT!:,'DNAME,'FLOAT);
FLAG('(!:FT!:),'FIELD);
PUT('!:FT!:,'I2D,'!*I2FT);
PUT('!:FT!:,'!:RN!:,'!*FT2RN);
PUT('!:FT!:,'MINUSP,'FTMINUSP!:);
PUT('!:FT!:,'PLUS,'FTPLUS!:);
PUT('!:FT!:,'TIMES,'FTTIMES!:);
PUT('!:FT!:,'DIFFERENCE,'FTDIFFERENCE!:);
PUT('!:FT!:,'QUOTIENT,'FTQUOTIENT!:);
PUT('!:FT!:,'ZEROP,'FTZEROP!:);
PUT('!:FT!:,'PREPFN,'FTPREP!:);
PUT('!:FT!:,'SPECPRN,'PRIN2!*);

SYMBOLIC PROCEDURE MKFLOAT U;
   '!:FT!: . U;

SYMBOLIC PROCEDURE !*I2FT U;
   %converts integer U to floating point form or NIL;
   IF U=0 THEN NIL ELSE '!:FT!: . FLOAT U;

SYMBOLIC PROCEDURE !*FT2RN U;
   BEGIN INTEGER M; SCALAR X;
      U := CDR U;   %pick up actual number;
      M := FIX(1000000*U);
      X := GCDN(1000000,M);
      X := (M/X) . (1000000/X);
      MSGPRI(NIL,U,"represented by",LIST('QUOTIENT,CAR X,CDR X),NIL);
      RETURN '!:RN!: . X
   END;

SYMBOLIC PROCEDURE FTMINUSP!: U; CDR U<0;

SYMBOLIC PROCEDURE FTPLUS!:(U,V);
   (LAMBDA X; IF ABS(X/CDR U)<0.000001 AND ABS(X/CDR V)<0.000001 THEN 0
		 ELSE '!:FT!: . X)
   (CDR U+CDR V);

SYMBOLIC PROCEDURE FTTIMES!:(U,V); CAR U . (CDR U*CDR V);

SYMBOLIC PROCEDURE FTDIFFERENCE!:(U,V); CAR U .(CDR U-CDR V);

SYMBOLIC PROCEDURE FTQUOTIENT!:(U,V); CAR U . (CDR U/CDR V);

SYMBOLIC PROCEDURE FTZEROP!: U; CDR U=0.0;

SYMBOLIC PROCEDURE FTPREP!: U; CDR U;

INITDMODE 'FLOAT;


COMMENT *** Entry points for the bigfloat package ***;

FLUID '(!*BIGFLOAT);

PUT('BIGFLOAT,'SIMPFG,'((T (RMSUBS) (SETDMODE (QUOTE BIGFLOAT)))
			(NIL (SETDMODE NIL))));

PUT('NUMVAL,'SIMPFG,'((T (RMSUBS) (SETDMODE (QUOTE BIGFLOAT)))));

PUT('BIGFLOAT,'TAG,'!:BF!:);


COMMENT *** Tables for modular integers ***;

FLUID '(!*MODULAR);

DOMAINLIST!* := UNION('(!:MOD!:),DOMAINLIST!*);
PUT('MODULAR,'TAG,'!:MOD!:);
PUT('!:MOD!:,'DNAME,'MODULAR);
FLAG('(!:MOD!:),'FIELD);
FLAG('(!:MOD!:),'CONVERT);
PUT('!:MOD!:,'I2D,'!*I2MOD);
PUT('!:MOD!:,'!:BF!:,'MODCNV);
PUT('!:MOD!:,'!:FT!:,'MODCNV);
PUT('!:MOD!:,'!:RN!:,'MODCNV);
PUT('!:MOD!:,'MINUSP,'MODMINUSP!:);
PUT('!:MOD!:,'PLUS,'MODPLUS!:);
PUT('!:MOD!:,'TIMES,'MODTIMES!:);
PUT('!:MOD!:,'DIFFERENCE,'MODDIFFERENCE!:);
PUT('!:MOD!:,'QUOTIENT,'MODQUOTIENT!:);
PUT('!:MOD!:,'ZEROP,'MODZEROP!:);
PUT('!:MOD!:,'PREPFN,'MODPREP!:);
PUT('!:MOD!:,'SPECPRN,'MODPRIN);

SYMBOLIC PROCEDURE !*I2MOD U;
   %converts integer U to modular form;
   IF (U := CMOD U)=0 THEN NIL ELSE '!:MOD!: . U;

SYMBOLIC PROCEDURE MODCNV U;
   REDERR LIST("Conversion between modular integers and",
		GET(CAR U,'DNAME),"not defined");

SYMBOLIC PROCEDURE MODMINUSP!: U; NIL;   %what else can one do?;

SYMBOLIC PROCEDURE MODPLUS!:(U,V);
   (LAMBDA X; IF X=0 THEN NIL ELSE IF X=1 THEN 1 ELSE CAR U . X)
   CPLUS(CDR U,CDR V);

SYMBOLIC PROCEDURE MODTIMES!:(U,V);
   (LAMBDA X; IF X=1 THEN 1 ELSE CAR U . X) CTIMES(CDR U,CDR V);

SYMBOLIC PROCEDURE MODDIFFERENCE!:(U,V);
   CAR U . CPLUS(CDR U,MOD!*-CDR V);

SYMBOLIC PROCEDURE MODQUOTIENT!:(U,V);
   CAR U . CTIMES(CDR U,CRECIP CDR V);

SYMBOLIC PROCEDURE MODZEROP!: U; CDR U=0;

SYMBOLIC PROCEDURE MODPREP!: U; CDR U;

SYMBOLIC PROCEDURE MODPRIN U; PRIN2!* CDR U;

INITDMODE 'MODULAR;


%*********************************************************************
%                  FUNCTIONS FOR MODULAR ARITHMETIC
%********************************************************************;

COMMENT This section defines routines for modular integer arithmetic.
	It assumes that such numbers are normalized in the range 0<=n<p,
	where p is the modular base;

COMMENT The actual modulus is stored in MOD!*;

SYMBOLIC PROCEDURE CEXPT(M,N);
   %returns the normalized value of M**N;
   BEGIN INTEGER P;
      P := 1;
      WHILE N>0 DO
      <<IF REMAINDER(N,2)=1 THEN P := CTIMES(P,M);
	N := N/2;
	IF N>0 THEN M := CTIMES(M,M)>>;
      RETURN P
   END;

SYMBOLIC PROCEDURE CPLUS(M,N);
   %returns the normalized sum of U and V;
   (LAMBDA L; IF L>=MOD!* THEN L-MOD!* ELSE L) (M+N);

SYMBOLIC PROCEDURE CMINUS(M);
   %returns the negative of M;
   IF M=0 THEN M ELSE MOD!*-M;

SYMBOLIC PROCEDURE CDIF(M,N);
   %returns the normalized difference of M and N;
   (LAMBDA L; IF L<0 THEN L+MOD!* ELSE L) (M-N);

SYMBOLIC PROCEDURE CRECIP M;
   %returns the normalized reciprocal of M modulo MOD!*
   %provided M is non-zero mod MOD!*, and M and MOD!* are co-prime.
   %If not, an error results;
   CRECIP1(MOD!*,M,0,1);

SYMBOLIC PROCEDURE CRECIP1(A,B,X,Y);
   %This is essentially the same as RECIPROCAL-BY-GCD in the Norman/
   %Moore factorizer;
   IF B=0 THEN REDERR "Invalid modular division"
    ELSE IF B=1 THEN IF Y<0 THEN Y+MOD!* ELSE Y
    ELSE BEGIN SCALAR W;
      W := A/B;   %truncated integer division;
      RETURN CRECIP1(B,A-B*W,Y,X-Y*W)
   END;

SYMBOLIC PROCEDURE CTIMES(M,N);
   %returns the normalized product of M and N;
   REMAINDER(M*N,MOD!*);

SYMBOLIC PROCEDURE SETMOD U;
   %always returns value of MOD!* on entry.
   %if U=0, no other action, otherwise MOD!* is set to U;
   IF U=0 THEN MOD!* ELSE (LAMBDA N; <<MOD!* := U; N>>) MOD!*;

FLAG('(SETMOD),'OPFN);   %to make it a symbolic operator;

SYMBOLIC PROCEDURE CMOD M;
   %returns normalized M;
   (LAMBDA N; IF N<0 THEN N+MOD!* ELSE N) REMAINDER(M,MOD!*);

%A more general definition;

%SYMBOLIC PROCEDURE CMOD M;
   %returns normalized M;
%   (LAMBDA N; %IF N<0 THEN N+MOD!* ELSE N)
%   IF ATOM M THEN REMAINDER(M,MOD!*)
%    ELSE BEGIN SCALAR X;
%	X := DCOMBINE(M,MOD!*,'DIVIDE);
%        RETURN CDR X
%     END;


%*********************************************************************
%	FUNCTIONS FOR ADDING AND MULTIPLYING STANDARD QUOTIENTS
%********************************************************************;

SYMBOLIC PROCEDURE ADDSQ(U,V);
   %U and V are standard quotients.
   %Value is canonical sum of U and V;
   IF NULL NUMR U THEN V
    ELSE IF NULL NUMR V THEN U
    ELSE IF DENR U=1 AND DENR V=1 THEN ADDF(NUMR U,NUMR V) ./ 1
    ELSE BEGIN SCALAR X,Y,Z;
	IF NULL !*EXP THEN <<U := NUMR U ./ MKPROD!* DENR U;
			     V := NUMR V ./ MKPROD!* DENR V>>;
	IF !*LCM THEN X := GCDF!*(DENR U,DENR V)
	 ELSE X := GCDF(DENR U,DENR V);
	Z := CANSQ1(QUOTF(DENR U,X) ./ QUOTF(DENR V,X));
	Y := ADDF(MULTF(NUMR U,DENR Z),MULTF(NUMR V,NUMR Z));
	IF NULL Y THEN RETURN NIL ./ 1;
	Z := MULTF(DENR U,DENR Z);
	IF ONEP X THEN RETURN Y ./ Z;
	X := GCDF(Y,X);
	RETURN IF X=1 THEN Y ./ Z
		ELSE CANSQ1(QUOTF(Y,X) ./ QUOTF(Z,X))
    END;

SYMBOLIC PROCEDURE MULTSQ(U,V);
   %U and V are standard quotients.
   %Value is canonical product of U and V;
   IF NULL NUMR U OR NULL NUMR V THEN NIL ./ 1
    ELSE IF DENR U=1 AND DENR V=1 THEN MULTF(NUMR U,NUMR V) ./ 1
    ELSE BEGIN SCALAR X,Y;
	X := GCDF(NUMR U,DENR V);
	Y := GCDF(NUMR V,DENR U);
	RETURN CANSQ1(MULTF(QUOTF(NUMR U,X),QUOTF(NUMR V,Y))
		./ MULTF(QUOTF(DENR U,Y),QUOTF(DENR V,X)))
    END;

SYMBOLIC PROCEDURE NEGSQ U;
   NEGF NUMR U ./ DENR U;

SMACRO PROCEDURE MULTPQ(U,V);
   MULTSQ(!*P2Q U,V);

SYMBOLIC PROCEDURE CANCEL U;
   %returns canonical form of non-canonical standard form U;
   IF !*MCD OR DENR U=1 THEN CANONSQ MULTSQ(NUMR U ./ 1,1 ./ DENR U)
    ELSE MULTSQ(NUMR U ./ 1,SIMPEXPT LIST(MK!*SQ(DENR U ./ 1),-1));


%*********************************************************************
%	  FUNCTIONS FOR ADDING AND MULTIPLYING STANDARD FORMS
%********************************************************************;

SYMBOLIC SMACRO PROCEDURE PEQ(U,V);
   %tests for equality of powers U and V;
   U = V;

SYMBOLIC PROCEDURE ADDF(U,V);
   %U and V are standard forms. Value is standard form for U+V;
   IF NULL U THEN V
    ELSE IF NULL V THEN U
    ELSE IF DOMAINP U THEN ADDD(U,V)
    ELSE IF DOMAINP V THEN ADDD(V,U)
    ELSE IF PEQ(LPOW U,LPOW V)
       THEN (LAMBDA (X,Y); IF NULL X THEN Y ELSE LPOW U .* X .+ Y)
		(ADDF(LC U,LC V),ADDF(RED U,RED V))
    ELSE IF ORDPP(LPOW U,LPOW V) THEN LT U .+ ADDF(RED U,V)
    ELSE LT V .+ ADDF(U,RED V);

SYMBOLIC PROCEDURE ADDD(U,V);
   %U is a domain element, V a standard form.
   %Value is a standard form for U+V;
   IF NULL V THEN U
    ELSE IF DOMAINP V THEN ADDDM(U,V)
    ELSE LT V .+ ADDD(U,RED V);

SYMBOLIC PROCEDURE ADDDM(U,V);
   %U and V are both domain elements.
   %Value is standard form for U+V;
   IF ATOM U AND ATOM V THEN !*N2F PLUS2(U,V)
    ELSE BEGIN SCALAR X;
      RETURN IF !:ZEROP(X := DCOMBINE(U,V,'PLUS)) THEN NIL ELSE X
     END;

SYMBOLIC PROCEDURE DOMAINP U;
   ATOM U OR ATOM CAR U;

SYMBOLIC PROCEDURE NONCOMP U;
   NOT ATOM U AND FLAGP!*!*(CAR U,'NONCOM);

SYMBOLIC PROCEDURE MULTF(U,V);
   %U and V are standard forms.
   %Value is standard form for U*V;
   BEGIN SCALAR X,Y;
    A:	IF NULL U OR NULL V THEN RETURN NIL
	 ELSE IF ONEP U THEN RETURN V
	 ELSE IF ONEP V THEN RETURN U
	 ELSE IF DOMAINP U THEN RETURN MULTD(U,V)
	 ELSE IF DOMAINP V THEN RETURN MULTD(V,U)
	 ELSE IF NOT(!*EXP OR NCMP!* OR WTL!* OR X)
	  THEN <<U := MKPROD U; V := MKPROD V; X := T; GO TO A>>;
	X := MVAR U;
	Y := MVAR V;
	IF NONCOMP X AND NONCOMP Y THEN RETURN MULTFNC(U,V)
	 ELSE IF X EQ Y
	  THEN <<X := MKSPM(X,LDEG U+LDEG V);
		 Y := ADDF(MULTF(!*T2F LT U,RED V),MULTF(RED U,V));
		 RETURN IF NULL X OR NULL(U := MULTF(LC U,LC V)) THEN Y
		   ELSE IF NULL !*MCD
		    THEN ADDF(IF X=1 THEN U ELSE !*T2F(X .* U),Y)
		   ELSE X .* U .+ Y>>
	 ELSE IF ORDOP(X,Y)
	  THEN <<X := MULTF(LC U,V);
		 Y := MULTF(RED U,V);
		 RETURN IF NULL X THEN Y ELSE LPOW U .* X .+ Y>>;
	X := MULTF(U,LC V);
	Y := MULTF(U,RED V);
	RETURN IF NULL X THEN Y ELSE LPOW V .* X .+ Y
   END;

SYMBOLIC PROCEDURE MULTFNC(U,V);
   %returns canonical product of U and V, with both main vars non-
   %commutative;
   BEGIN SCALAR X,Y;
      X := MULTF(LC U,!*T2F LT V);
      RETURN ADDF((IF NOT DOMAINP X AND MVAR X EQ MVAR U
		     THEN ADDF(!*T2F(MKSPM(MVAR U,LDEG U+LDEG V)
				.* LC X),
			    MULTF(!*P2F LPOW U,RED X))
		    ELSE !*T2F(LPOW U .* X)),
		  ADDF(MULTF(RED U,V),MULTF(!*T2F LT U,RED V)))
   END;

SYMBOLIC PROCEDURE MULTD(U,V);
   %U is a domain element, V a standard form.
   %Value is standard form for U*V;
   IF NULL V THEN NIL
    ELSE IF DOMAINP V THEN MULTDM(U,V)
    ELSE LPOW V .* MULTD(U,LC V) .+ MULTD(U,RED V);

SYMBOLIC PROCEDURE MULTDM(U,V);
   %U and V are both domain elements. Value is standard form for U*V;
   IF ATOM U AND ATOM V THEN TIMES2(U,V)
    ELSE BEGIN SCALAR X;
      RETURN IF !:ONEP(X := DCOMBINE(U,V,'TIMES)) THEN 1 ELSE X
     END;

SMACRO PROCEDURE MULTPF(U,V);
   MULTF(!*P2F U,V);

GLOBAL '(!*FACTOR);  %used to call a factorizing routine if it exists;

SYMBOLIC PROCEDURE MKPROD U;
   BEGIN SCALAR W,X,Y,Z,!*EXP;
	IF NULL U OR KERNLP U THEN RETURN U;
	%first make sure there are no further simplifications;
	IF DENR(X := SUBS2(U ./ 1)) = 1 AND NUMR X NEQ U
	  THEN <<U := NUMR X; IF NULL U OR KERNLP U THEN RETURN U>>;
	!*EXP := T;
	W := CKRN U;
	U := QUOTF(U,W);
	X := EXPND U;
	IF NULL X OR KERNLP X THEN RETURN MULTF(W,X);
	%after this point, U is not KERNLP;
	IF !*FACTOR OR !*GCD THEN Y := FCTRF X
	  ELSE <<Y := CKRN X;
		 X := QUOTF(X,Y);
		 Y := LIST(Y,X . 1)>>;
	  IF CDADR Y>1 OR CDDR Y
	    THEN <<Z := CAR Y;
	           FOR EACH J IN CDR Y DO
		      Z := MULTF(MKSP!*(CAR J,CDR J),Z)>>
	 ELSE IF NOT !*GROUP AND TMSF U>TMSF CAADR Y
	  THEN Z := MULTF(MKSP!*(CAADR Y,CDADR Y),CAR Y)
	 ELSE Z := MKSP!*(U,1);
	RETURN MULTF(W,Z)
   END;

SYMBOLIC PROCEDURE MKSP!*(U,N);
   %Returns a standard form for U**N, in which U is first made 
   %positive and then converted into a kernel;
   BEGIN SCALAR B;
      IF MINUSF U THEN <<B := T; U := NEGF U>>;
      U := !*P2F MKSP(U,N);
      RETURN IF B AND NOT ZEROP REMAINDER(N,2) THEN NEGF U ELSE U
   END;

SYMBOLIC PROCEDURE TMSF U;
   %U is a standard form.
   %Value is number of terms in U (including kernel structure);
   BEGIN INTEGER N; SCALAR X;
	N := 0;
    A:	IF NULL U THEN RETURN N ELSE IF DOMAINP U THEN RETURN N+1;
	N := N+(IF SFP(X := MVAR U) THEN TMSF X ELSE 1)+TMSF!* LC U;
	IF LDEG U NEQ 1 THEN N := N+2;
	U := RED U;
	IF U THEN N := N+1;
	GO TO A
   END;

SYMBOLIC PROCEDURE TMSF!* U;
   IF NUMBERP U AND ABS FIX U=1 THEN 0 ELSE TMSF U+1;

SYMBOLIC PROCEDURE TMS U;
   TMSF NUMR SIMP!* U;

FLAG('(TMS),'OPFN);

FLAG('(TMS),'NOVAL);

SYMBOLIC PROCEDURE EXPND U;
   IF DOMAINP U THEN U
    ELSE ADDF(IF NOT SFP MVAR U OR LDEG U<0
		THEN MULTPF(LPOW U,EXPND LC U)
	ELSE MULTF(EXPTF(EXPND MVAR U,LDEG U),EXPND LC U),
			EXPND RED U);

SYMBOLIC PROCEDURE MKPROD!* U;
   IF DOMAINP U THEN U ELSE MKPROD U;

SYMBOLIC PROCEDURE CANPROD(P,Q);
   %P and Q are kernel product standard forms, value is P/Q;
   BEGIN SCALAR V,W,X,Y,Z;
	IF DOMAINP Q THEN RETURN CANCEL(P ./ Q);
      WHILE NOT DOMAINP P OR NOT DOMAINP Q DO
	IF SFPF P THEN
		<<Z := CPROD1(MVAR P,LDEG P,V,W);
			V := CAR Z; W := CDR Z; P := LC P>>
	 ELSE IF SFPF Q THEN <<Z := CPROD1(MVAR Q,LDEG Q,W,V);
			W := CAR Z; V := CDR Z; Q := LC Q>>
	 ELSE IF DOMAINP P THEN <<Y := LPOW Q . Y; Q := LC Q>>
	 ELSE IF DOMAINP Q THEN <<X := LPOW P . X; P := LC P>>
	 ELSE <<X := LPOW P . X; Y := LPOW Q . Y;
		P := LC P; Q := LC Q>>;
      V := REPROD(V,REPROD(X,P));
      W := REPROD(W,REPROD(Y,Q));
      IF MINUSF W THEN <<V := NEGF V; W := NEGF W>>;
      W := CANCEL(V ./ W);
      V := NUMR W;
	IF NOT DOMAINP V AND NULL RED V AND ONEP LC V
	 AND LDEG V=1 AND SFP(X := MVAR V)
	THEN V := X;
      RETURN CANSQ1(V ./ DENR W)
   END;

SYMBOLIC PROCEDURE SFPF U;
   NOT DOMAINP U AND SFP MVAR U;

SYMBOLIC PROCEDURE SFP U;
   %determines if mvar U is a standard form;
   NOT ATOM U AND NOT ATOM CAR U;

SYMBOLIC PROCEDURE REPROD(U,V);
   %U is a list of powers,V a standard form;
   %value is product of terms in U with V;
   <<WHILE U DO <<V := MULTPF(CAR U,V); U := CDR U>>; V>>;

SYMBOLIC PROCEDURE CPROD1(P,M,V,W);
   %U is a standard form, which occurs in a kernel raised to power M.
   %V is a list of powers multiplying P**M, W a list dividing it.
   %Value is a dotted pair of lists of powers after all possible kernels
   %have been cancelled;
   BEGIN SCALAR Z;
      Z := CPROD2(P,M,W,NIL);
      W := CADR Z;
      V := APPEND(CDDR Z,V);
      Z := CPROD2(CAR Z,M,V,T);
      V := CADR Z;
      W := APPEND(CDDR Z,W);
      IF CAR Z NEQ 1 THEN V := MKSP(CAR Z,M) . V;
      RETURN V . W
   END;

SYMBOLIC PROCEDURE CPROD2(P,M,U,B);
   %P and M are as in CPROD1. U is a list of powers. B is true if P**M
   %multiplies U, false if it divides.
   %Value has three parts: the first is the part of P which does not
   %have any common factors with U, the second a list of powers (plus
   %U) which multiply U, and the third a list of powers which divide U;
   %it is implicit here that the kernel standard forms are positive;
   BEGIN SCALAR N,V,W,Y,Z;
      WHILE U AND P NEQ 1 DO
	<<IF (Z := GCDF(P,CAAR U)) NEQ 1
	    THEN
	   <<P := QUOTF(P,Z);
	     Y := QUOTF(CAAR U,Z);
	     IF Y NEQ 1 THEN V := MKSP(Y,CDAR U) . V;
	     IF B THEN V := MKSP(Z,M+CDAR U) . V
	      ELSE IF (N := M-CDAR U)>0 THEN W := MKSP(Z,N) . W
	      ELSE IF N<0 THEN V := MKSP(Z,-N) . V>>
	    ELSE V := CAR U . V;
	   U := CDR U>>;
      RETURN (P . NCONC(U,V) . W)
   END;

SYMBOLIC PROCEDURE MKSPM(U,P);
   %U is a unique kernel, P an integer;
   %value is 1 if P=0 and not the weight variable K!*,
   %NIL if U**P is 0 or standard power of U**P otherwise;
   IF P=0 AND NOT(U EQ 'K!*) THEN 1
    ELSE BEGIN SCALAR X;
	IF SUBFG!* AND (X:= ATSOC(U,ASYMPLIS!*)) AND CDR X<=P
	  THEN RETURN NIL;
	SUB2CHK U;
	RETURN U TO P
   END;

SYMBOLIC PROCEDURE SUB2CHK U;
   %determines if kernel U is such that a power substitution i
   %necessary;
   IF SUBFG!* AND(ATSOC(U,POWLIS!*)
     OR NOT ATOM U AND CAR U MEMQ '(EXPT SQRT)
	AND ASSOC(CADR U,POWLIS!*))
    THEN !*SUB2 := T;

SYMBOLIC PROCEDURE NEGF U;
   MULTD(-1,U);


%*********************************************************************
%		 FUNCTIONS FOR DIVIDING STANDARD FORMS
%********************************************************************;

SYMBOLIC PROCEDURE QUOTSQ(U,V);
   MULTSQ(U,INVSQ V);

SYMBOLIC PROCEDURE QUOTF!*(U,V);
   IF NULL U THEN NIL
    ELSE (LAMBDA X; IF NULL X THEN ERRACH LIST("DIVISION FAILED",U,V)
			 ELSE X)
	  QUOTF(U,V);

SYMBOLIC PROCEDURE QUOTF(U,V);
   BEGIN SCALAR XEXP;
	XEXP := !*EXP;
	!*EXP := T;
	U := QUOTF1(U,V);
	!*EXP := XEXP;
	RETURN U
   END;

SYMBOLIC PROCEDURE QUOTF1(P,Q);
   %P and Q are standard forms
   %Value is the quotient of P and Q if it exists or NIL;
   IF NULL P THEN NIL
    ELSE IF P=Q THEN 1
    ELSE IF Q=1 THEN P
    ELSE IF DOMAINP Q THEN QUOTFD(P,Q)
    ELSE IF DOMAINP P THEN NIL
    ELSE IF MVAR P EQ MVAR Q
     THEN BEGIN SCALAR U,V,W,X,Y,Z,Z1; INTEGER N;
    A:IF IDP(U := RANK P) OR IDP(V := RANK Q) OR U<V THEN RETURN NIL;
	%the above IDP test is because of the possibility of a free
	%variable in the degree position from LET statements;
	U := LT!* P;
	V := LT!* Q;
	W := MVAR Q;
	X := QUOTF1(TC U,TC V);
	IF NULL X THEN RETURN NIL;
	N := TDEG U-TDEG V;
	IF N NEQ 0 THEN Y := W TO N;
	P := ADDF(P,MULTF(IF N=0 THEN Q
			       ELSE MULTPF(Y,Q),NEGF X));
	%leading terms of P and Q do not cancel if MCD is off;
	%however, there may be a problem with off exp;
	IF P AND (DOMAINP P OR MVAR P NEQ W) THEN RETURN NIL
	 ELSE IF N=0 THEN GO TO B;
	Z := ACONC(Z,Y .* X);
	%provided we have a non-zero power of X, terms
	%come out in right order;
	IF NULL P THEN RETURN IF Z1 THEN NCONC(Z,Z1) ELSE Z;
	GO TO A;
    B:	IF NULL P THEN RETURN NCONC(Z,X)
	 ELSE IF !*MCD THEN RETURN NIL
	 ELSE Z1 := X;
	GO TO A
   END
    ELSE IF ORDOP(MVAR P,MVAR Q) THEN QUOTK(P,Q)
    ELSE NIL;

SYMBOLIC PROCEDURE QUOTFD(P,Q);
   %P is a standard form, Q a domain element;
   %Value is P/Q if division is exact or NIL otherwise;
   IF FIELDP Q THEN MULTD(!:RECIP Q,P)
    ELSE IF DOMAINP P THEN QUOTDD(P,Q)
    ELSE QUOTK(P,Q);

SYMBOLIC PROCEDURE QUOTDD(U,V);
   %U and V are domain elements, value is U/V if division is exact,
   %NIL otherwise;
   IF ATOM U THEN IF ATOM V
		    THEN IF REMAINDER(U,V)=0 THEN U/V ELSE NIL
		   ELSE QUOTDD(APPLY(GET(CAR V,'I2D),LIST U),V)
    ELSE IF ATOM V THEN QUOTDD(U,APPLY(GET(CAR U,'I2D),LIST V))
    ELSE DCOMBINE(U,V,'QUOTIENT);

SYMBOLIC PROCEDURE QUOTK(P,Q);
   (LAMBDA W;
      IF W THEN IF NULL RED P THEN LIST (LPOW P .* W)
		 ELSE (LAMBDA Y;IF Y THEN LPOW P .* W .+ Y ELSE NIL)
			  QUOTF1(RED P,Q)
	 ELSE NIL)
      QUOTF1(LC P,Q);

SYMBOLIC PROCEDURE RANK P;
   %P is a standard form
   %Value is the rank of P;
   IF !*MCD THEN LDEG P
    ELSE BEGIN INTEGER M,N; SCALAR Y;
	N := LDEG P;
	Y := MVAR P;
    A:	M := LDEG P;
	IF NULL RED P THEN RETURN N-M;
	P := RED P;
	IF DEGR(P,Y)=0 THEN RETURN IF M<0 THEN IF N<0 THEN -M
		ELSE N-M ELSE N;
	GO TO A
    END;

SYMBOLIC PROCEDURE LT!* P;
   %Returns true leading term of polynomial P;
   IF !*MCD OR LDEG P>0 THEN CAR P
    ELSE BEGIN SCALAR X,Y;
	X := LT P;
	Y := MVAR P;
    A:	P := RED P;
	IF NULL P THEN RETURN X
	 ELSE IF DEGR(P,Y)=0 THEN RETURN (Y . 0) .* P;
	GO TO A
   END;

SYMBOLIC PROCEDURE REMF(U,V);
   %returns the remainder of U divided by V;
   CDR QREMF(U,V);

PUT('REMAINDER,'POLYFN,'REMF);

SYMBOLIC PROCEDURE QREMF(U,V);
   %returns the quotient and remainder of U divided by V;
   BEGIN INTEGER N; SCALAR X,Y,Z;
	IF DOMAINP V THEN RETURN QREMD(U,V);
	Z := LIST NIL;	 %final value;
    A:	IF DOMAINP U THEN RETURN PRADDF(Z,NIL . U)
	 ELSE IF MVAR U EQ MVAR V
	  THEN IF (N := LDEG U-LDEG V)<0 THEN RETURN PRADDF(Z,NIL . U)
		ELSE <<X := QREMF(LC U,LC V);
		Y := MULTPF(LPOW U,CDR X);
		Z := PRADDF(Z,(IF N=0 THEN CAR X
				ELSE MULTPF(MVAR U TO N,CAR X))
				. Y);
		U := IF NULL CAR X THEN RED U
			ELSE ADDF(ADDF(U,MULTF(IF N=0 THEN V
					ELSE MULTPF(MVAR U TO N,V),
					NEGF CAR X)), NEGF Y);
		GO TO A>>
	 ELSE IF NOT ORDOP(MVAR U,MVAR V)
	  THEN RETURN PRADDF(Z,NIL . U);
	X := QREMF(LC U,V);
	Z := PRADDF(Z,MULTPF(LPOW U,CAR X) . MULTPF(LPOW U,CDR X));
	U := RED U;
	GO TO A
   END;

SYMBOLIC PROCEDURE PRADDF(U,V);
   %U and V are dotted pairs of standard forms;
   ADDF(CAR U,CAR V) . ADDF(CDR U,CDR V);

SYMBOLIC PROCEDURE QREMD(U,V);
   %Returns a dotted pair of quotient and remainder of form U
   %divided by domain element V;
   IF NULL U THEN U . U
    ELSE IF V=1 THEN LIST U
    ELSE IF NOT ATOM V AND FLAGP(CAR V,'FIELD)
     THEN LIST MULTDM(!:RECIP V,U)
    ELSE IF DOMAINP U THEN QREMDD(U,V)
    ELSE BEGIN SCALAR X;
	X := QREMF(LC U,V);
	RETURN PRADDF(MULTPF(LPOW U,CAR X) . MULTPF(LPOW U,CDR X),
			QREMD(RED U,V))
   END;

SYMBOLIC PROCEDURE QREMDD(U,V);
   %returns a dotted pair of quotient and remainder of non-invertable
   %domain element U divided by non-invertable domain element V;
   IF ATOM U AND ATOM V THEN DIVIDEF(U,V) ELSE DCOMBINE(U,V,'DIVIDE);

SYMBOLIC PROCEDURE DIVIDEF(M,N);
   (LAMBDA X; (IF CAR X=0 THEN NIL ELSE CAR X).
			IF CDR X=0 THEN NIL ELSE CDR X)
   DIVIDE(M,N);

SYMBOLIC PROCEDURE LQREMF(U,V);
   %returns a list of coeffs of powers of V in U, constant term first;
   BEGIN SCALAR X,Y;
      Y := LIST U;
      WHILE CAR(X := QREMF(CAR Y,V)) DO Y := CAR X . CDR X . CDR Y;
      RETURN REVERSIP Y
   END;


%*********************************************************************
%		   GREATEST COMMON DIVISOR ROUTINES
%********************************************************************;

SYMBOLIC PROCEDURE GCDN(P,Q);
   %P and Q are integers. Value is absolute value of gcd of P and Q;
   IF Q = 0 THEN ABS P ELSE GCDN(Q,REMAINDER(P,Q));

SYMBOLIC PROCEDURE COMFAC P;
  %P is a non-atomic standard form
  %CAR of result is lowest common power of leading kernel in
  %every term in P (or NIL). CDR is gcd of all coefficients of
  %powers of leading kernel;
   BEGIN SCALAR X,Y;
	IF NULL RED P THEN RETURN LT P;
	X := LC P;
	Y := MVAR P;  %leading kernel;
    A:	P := RED P;
	IF DEGR(P,Y)=0 THEN RETURN NIL . GCDF1(X,P)
	 ELSE IF NULL RED P THEN RETURN LPOW P . GCDF1(X,LC P)
	 ELSE X := GCDF1(LC P,X);
	GO TO A
   END;

SYMBOLIC PROCEDURE DEGR(U,VAR);
   IF DOMAINP U OR NOT MVAR U EQ VAR THEN 0 ELSE LDEG U;

PUT('GCD,'POLYFN,'GCDF!*);

SYMBOLIC PROCEDURE GCDF!*(U,V);
   BEGIN SCALAR !*GCD; !*GCD := T; RETURN GCDF(U,V) END;

SYMBOLIC PROCEDURE GCDF(U,V);
   %U and V are standard forms.
   %Value is the gcd of U and V, complete only if *GCD is true;
   BEGIN SCALAR !*EXP,Y,Z;
	!*EXP := T;
	IF NULL U THEN RETURN ABSF V
	 ELSE IF NULL V THEN RETURN ABSF U
	 ELSE IF U=1 OR V=1 THEN RETURN 1
	 ELSE IF !*GCD AND !*EZGCD THEN RETURN EZGCDF(U,V);
	IF QUOTF1(U,V) THEN Z := V
	 ELSE IF QUOTF1(V,U) THEN Z := U
	 ELSE <<IF !*GCD THEN <<Y := SETKORDER KERNORD(U,V);
				U := REORDER U; V := REORDER V>>;
		Z := GCDF1(U,V);
		IF !*GCD
		THEN <<IF U AND V
			  AND (NULL QUOTF1(U,Z) OR NULL QUOTF1(V,Z))
		      THEN ERRACH LIST("GCDF FAILED",PREPSQ U,PREPSQ V);
		 %this probably implies that integer overflow occurred;
			SETKORDER Y;
			Z := REORDER Z>>>>;
	RETURN ABSF Z
   END;

SYMBOLIC PROCEDURE GCDF1(U,V);
   IF NULL U THEN V
    ELSE IF NULL V THEN U
    ELSE IF ONEP U OR ONEP V THEN 1
    ELSE IF DOMAINP U THEN GCDFD(U,V)
    ELSE IF DOMAINP V THEN GCDFD(V,U)
    ELSE IF QUOTF1(U,V) THEN V
    ELSE IF QUOTF1(V,U) THEN U
    ELSE IF MVAR U EQ MVAR V
     THEN BEGIN SCALAR X,Y,Z;
	X := COMFAC U;
	Y := COMFAC V;
	Z := GCDF1(CDR X,CDR Y);
	IF !*GCD
	  THEN Z := MULTF(GCDK(QUOTF1(U,COMFAC!-TO!-POLY X),
			       QUOTF1(V,COMFAC!-TO!-POLY Y)),
			  Z);
	IF CAR X AND CAR Y
	 THEN IF PDEG CAR X>PDEG CAR Y
		THEN Z := MULTPF(CAR Y,Z)
	       ELSE Z := MULTPF(CAR X,Z);
	RETURN Z
     END
    ELSE IF ORDOP(MVAR U,MVAR V) THEN GCDF1(CDR COMFAC U,V)
    ELSE GCDF1(CDR COMFAC V,U);

SYMBOLIC PROCEDURE GCDFD(U,V);
   %U is a domain element, V a form;
   %Value is gcd of U and V;
   IF NOT ATOM U AND FLAGP(CAR U,'FIELD) THEN 1 ELSE GCDFD1(U,V);

SYMBOLIC PROCEDURE GCDFD1(U,V);
   IF NULL V THEN U
    ELSE IF DOMAINP V THEN GCDDD(U,V)
    ELSE GCDFD1(GCDFD1(U,LC V),RED V);

SYMBOLIC PROCEDURE GCDDD(U,V);
   %U and V are domain elements.  If they are invertable, value is 1
   %otherwise the gcd of U and V as a domain element;
   IF U=1 OR V=1 THEN 1
    ELSE IF ATOM U THEN IF NOT FIELDP V THEN GCDDD1(U,V) ELSE 1
    ELSE IF ATOM V
     THEN IF NOT FLAGP(CAR U,'FIELD) THEN GCDDD1(U,V) ELSE 1
    ELSE IF FLAGP(CAR U,'FIELD) OR FLAGP(CAR V,'FIELD) THEN 1
    ELSE GCDDD1(U,V);

SYMBOLIC PROCEDURE GCDDD1(U,V);
   %U and V are non-invertable domain elements. Value is gcd of U and V;
   IF ATOM U AND ATOM V THEN GCDN(U,V) ELSE DCOMBINE(U,V,'GCD);

SYMBOLIC PROCEDURE GCDK(U,V);
   %U and V are primitive polynomials in the main variable VAR;
   %result is gcd of U and V;
   BEGIN SCALAR LCLST,VAR,W,X;
	IF U=V THEN RETURN U
	 ELSE IF DOMAINP U OR DEGR(V,(VAR := MVAR U))=0 THEN RETURN 1
	 ELSE IF LDEG U<LDEG V THEN <<W := U; U := V; V := W>>;
	IF QUOTF1(U,V) THEN RETURN V ELSE IF LDEG V=1 THEN RETURN 1;
    A:	W := REMK(U,V);
	IF NULL W THEN RETURN V
	 ELSE IF DEGR(W,VAR)=0 THEN RETURN 1;
	LCLST := ADDLC(V,LCLST);
	IF X := QUOTF1(W,LC W) THEN W := X
	 ELSE FOR EACH Y IN LCLST DO WHILE (X := QUOTF1(W,Y)) DO W := X;
	U := V; V := PP W;
	IF DEGR(V,VAR)=0 THEN RETURN 1 ELSE GO TO A
   END;

SYMBOLIC PROCEDURE ADDLC(U,V);
   IF U=1 THEN V
    ELSE (LAMBDA X;
      IF X=1 OR X=-1 OR NOT ATOM X AND FLAGP(CAR X,'FIELD) THEN V
       ELSE X . V)
     LC U;

SYMBOLIC PROCEDURE DELALL(U,V);
   IF NULL V THEN NIL
    ELSE IF U EQ CAAR V THEN DELALL(U,CDR V)
    ELSE CAR V . DELALL(U,CDR V);

SYMBOLIC PROCEDURE KERNORD(U,V);
   BEGIN SCALAR X,Y,Z;
      X := APPEND(POWERS(U,NIL),POWERS(V,NIL));
	WHILE X DO
      <<Y := MAXDEG(CDR X,CAR X);
        X := DELALL(CAR Y,X);
	Z := CAR Y . Z>>;
   RETURN Z
   END;

SYMBOLIC PROCEDURE MAXDEG(U,V);
   IF NULL U THEN V
    ELSE IF CDAR U>CDR V THEN MAXDEG(CDR U,CAR U)
    ELSE MAXDEG(CDR U,V);

SYMBOLIC PROCEDURE POWERS(FORM,POWLST);
   IF NULL FORM OR DOMAINP FORM THEN POWLST
    ELSE BEGIN SCALAR X;
	IF (X := ATSOC(MVAR FORM,POWLST))
	  THEN LDEG FORM>CDR X AND RPLACD(X,LDEG FORM)
	 ELSE POWLST := (MVAR FORM . LDEG FORM) . POWLST;
	RETURN POWERS(RED FORM,POWERS(LC FORM,POWLST))
     END;

SYMBOLIC PROCEDURE LCM(U,V);
   %U and V are standard forms. Value is lcm of U and V;
   IF NULL U OR NULL V THEN NIL
    ELSE IF ONEP U THEN V
    ELSE IF ONEP V THEN U
    ELSE MULTF(U,QUOTF(V,GCDF(U,V)));

SYMBOLIC PROCEDURE REMK(U,V);
   %modified pseudo-remainder algorithm
   %U and V are polynomials, value is modified prem of U and V;
   BEGIN SCALAR F1,VAR,X; INTEGER K,N;
	F1 := LC V;
	VAR := MVAR V;
	N := LDEG V;
	WHILE (K := DEGR(U,VAR)-N)>=0 DO
	 <<X := NEGF MULTF(LC U,RED V);
	   IF K>0 THEN X := MULTPF(VAR TO K,X);
	   U := ADDF(MULTF(F1,RED U),X)>>;
	RETURN U
   END;

SYMBOLIC PROCEDURE PP U;
   %returns the primitive part of the polynomial U wrt leading var;
   QUOTF1(U,COMFAC!-TO!-POLY COMFAC U);

SYMBOLIC PROCEDURE COMFAC!-TO!-POLY U;
   IF NULL CAR U THEN CDR U ELSE LIST U;

SYMBOLIC PROCEDURE LNC U;
   %U is a standard form.
   %Value is the leading numerical coefficient;
   IF NULL U THEN 0
    ELSE IF DOMAINP U THEN U
    ELSE LNC LC U;

COMMENT In this sub-section, we consider the manipulation of factored
	forms.  These have the structure
	
	   <monomial> . <form-power-list>

	where the monomial is itself a standard form (satisfying the
	KERNLP test) and a form-power is a dotted pair whose car is a 
	standard form and cdr an integer>0. We have thus represented the
	form as a product of a monomial and powers of non-monomial
        factors;

SYMBOLIC PROCEDURE FCTRF U;
   %U is a standard form. Value is a standard factored form;
   %The function FACTORF is an assumed entry point to a factorization
   %module which itself returns a form power list;
   BEGIN SCALAR X,Y,!*GCD;
      !*GCD := T;
      IF DOMAINP U THEN RETURN LIST U
       ELSE IF !*FACTOR THEN RETURN FACTORF U;
      X := COMFAC U;
      U := QUOTF(U,COMFAC!-TO!-POLY X);
      Y := FCTRF CDR X;
      IF CAR X THEN Y := MULTPF(CAR X,CAR Y) . CDR Y;
      IF DOMAINP U THEN RETURN MULTF(U,CAR Y) . CDR Y
       ELSE IF MINUSF U
	THEN <<U := NEGF U; Y := NEGF CAR Y . CDR Y>>;
      RETURN CAR Y . FACMERGE(SQFRF U,CDR Y)
   END;

SYMBOLIC PROCEDURE FACMERGE(U,V);
   %Returns the merge of the form_power_lists U and V;
   APPEND(U,V);

SYMBOLIC PROCEDURE SQFRF U;
   %U is a non-trivial form which is primitive in its main variable
   %and has a positive leading numerical coefficient.
   %SQFRF performs square free factorization on U and returns a 
   %form power list;
   BEGIN INTEGER K,N; SCALAR V,W,X,Z,!*GCD;
      N := 1;
      X := MVAR U;
      !*GCD := T;
   A: V := GCDF(U,DIFF(U,X));
      K := DEGR(V,X);
      IF K>0 THEN U := QUOTF(U,V);
      IF W
	THEN <<IF U NEQ W
		 THEN Z := FACMERGE(LIST(QUOTF(W,U) . N),Z);
	       N := N+1>>;
      IF K=0 THEN RETURN FACMERGE(LIST(U . N),Z);
      W := U;
      U := V;
      GO TO A
   END;

SYMBOLIC PROCEDURE DIFF(U,V);
   %a polynomial differentation routine which does not check
   %indeterminate dependences;
   IF DOMAINP U THEN NIL
    ELSE ADDF(ADDF(MULTPF(LPOW U,DIFF(LC U,V)),
		MULTF(LC U,DIFFP1(LPOW U,V))),
	      DIFF(RED U,V));

SYMBOLIC PROCEDURE DIFFP1(U,V);
   IF NOT CAR U EQ V THEN NIL
    ELSE IF CDR U=1 THEN 1
    ELSE MULTD(CDR U,!*P2F(CAR U TO (CDR U-1)));

SYMBOLIC PROCEDURE MINUSF U;
   %U is a non-zero standard form.
   %Value is T if U has a negative leading numerical coeff,
   %NIL otherwise;
   IF NULL U THEN NIL
    ELSE IF DOMAINP U
	   THEN IF ATOM U THEN U<0 ELSE APPLY(GET(CAR U,'MINUSP),LIST U)
    ELSE MINUSF LC U;

SYMBOLIC PROCEDURE ABSF U;
   %U is a standard form
   %value is a standard form in which the leading power has a
   %positive coefficient;
   IF MINUSF U THEN NEGF U ELSE U;

SYMBOLIC PROCEDURE CANONSQ U;
   %U is a standard quotient
   %value is a standard quotient in which the leading power
   %of the denominator has a positive numerical coefficient.
   %If FLOAT is true, then denom is given LNC of 1;
   BEGIN
	IF NULL NUMR U THEN RETURN NIL ./ 1
	 ELSE IF MINUSF DENR U THEN U:= NEGF NUMR U ./ NEGF DENR U;
	RETURN CANSQ1 U
   END;

SYMBOLIC PROCEDURE CANSQ1 U;
   %Normalizes denominator of standard quotient U where possible
   %returning normalized quotient;
   IF DENR U=1 THEN U
    ELSE IF DOMAINP DENR U AND !:ONEP DENR U THEN NUMR U ./ 1
    ELSE IF NULL DMODE!* OR NULL FLAGP(DMODE!*,'FIELD) THEN U
    ELSE BEGIN SCALAR X;
	X := LNC DENR U;
	IF !:ONEP X THEN RETURN U;
	IF ATOM X THEN X := APPLY(GET(DMODE!*,'I2D),LIST X);
	X := DCOMBINE(1,X,'QUOTIENT);
	U := MULTD(X,NUMR U) ./ MULTD(X,DENR U);
	RETURN IF DOMAINP DENR U AND !:ONEP DENR U THEN NUMR U ./ 1
		ELSE U
   END;

SYMBOLIC PROCEDURE INVSQ U;
   IF NULL NUMR U THEN REDERR "Zero denominator" ELSE CANONSQ REVPR U;


%*********************************************************************
%	     FUNCTIONS FOR SUBSTITUTING IN STANDARD FORMS
%********************************************************************;

SYMBOLIC PROCEDURE SUBF(U,L);
   BEGIN SCALAR X;
   %domain may have changed, so next line uses simpatom;
      IF DOMAINP U THEN RETURN !*D2Q U
       ELSE IF NCMP!* AND NONCOMEXPF U THEN RETURN SUBF1(U,L);
      X := REVERSE XN(FOR EACH Y IN L COLLECT CAR Y,
		      KERNORD(U,NIL));
      X := SETKORDER X;
      U := SUBF1(REORDER U,L);
      SETKORDER X;
      RETURN REORDER NUMR U ./ REORDER DENR U
   END;

SYMBOLIC PROCEDURE NONCOMEXPF U;
   NOT DOMAINP U
      AND (NONCOMP MVAR U OR NONCOMEXPF LC U OR NONCOMEXPF RED U);

SYMBOLIC PROCEDURE SUBF1(U,L);
   %U is a standard form,
   %L an association list of substitutions of the form
   %(<kernel> . <substitution>).
   %Value is the standard quotient for substituted expression.
   %Algorithm used is essentially the straight method.
   %Procedure depends on explicit data structure for standard form;
   IF DOMAINP U
     THEN IF ATOM U THEN IF NULL DMODE!* THEN U ./ 1 ELSE SIMPATOM U
	  ELSE IF DMODE!* EQ CAR U THEN !*D2Q U
	  ELSE SIMP PREPF U
    ELSE BEGIN INTEGER N; SCALAR KERN,M,W,X,XEXP,Y,Y1,Z;
	Z := NIL ./ 1;
    A0: KERN := MVAR U;
	IF M := ASSOC(KERN,ASYMPLIS!*) THEN M := CDR M;
    A:	IF NULL U OR (N := DEGR(U,KERN))=0 THEN GO TO B
	 ELSE IF NULL M OR N<M THEN Y := LT U . Y;
	U := RED U;
	GO TO A;
    B:	IF NOT ATOM KERN AND NOT ATOM CAR KERN THEN KERN := PREPF KERN;
	IF NULL L THEN XEXP := IF KERN EQ 'K!* THEN 1 ELSE KERN
	 ELSE IF (XEXP := SUBSUBLIS(L,KERN)) = KERN
		   AND NOT ASSOC(KERN,ASYMPLIS!*)
	  THEN GO TO F;
    C:	W := 1 ./ 1;
	N := 0;
	IF Y AND CDAAR Y<0 THEN GO TO H;
	X := SIMP!* XEXP;
	IF NULL L AND KERNP X AND MVAR NUMR X EQ KERN THEN GO TO F
	 ELSE IF NULL NUMR X THEN GO TO E;   %Substitution of 0;
	FOR EACH J IN Y DO
	 <<M := CDAR J;
	   W := MULTSQ(EXPTSQ(X,M-N),W);
	   N := M;
	   Z := ADDSQ(MULTSQ(W,SUBF1(CDR J,L)),Z)>>;
    E:	Y := NIL;
	IF NULL U THEN RETURN Z
	 ELSE IF DOMAINP U THEN RETURN ADDSQ(!*D2Q U,Z);
	GO TO A0;
    F:  SUB2CHK KERN;
	FOR EACH J IN Y DO Z := ADDSQ(MULTPQ(CAR J,SUBF1(CDR J,L)),Z);
	GO TO E;
    H:	%Substitution for negative powers;
	X := SIMPRECIP LIST XEXP;
    J:	Y1 := CAR Y . Y1;
	Y := CDR Y;
	IF Y AND CDAAR Y<0 THEN GO TO J;
    K:	M := -CDAAR Y1;
	W := MULTSQ(EXPTSQ(X,M-N),W);
	N := M;
	Z := ADDSQ(MULTSQ(W,SUBF1(CDAR Y1,L)),Z);
	Y1 := CDR Y1;
	IF Y1 THEN GO TO K ELSE IF Y THEN GO TO C ELSE GO TO E
     END;

SYMBOLIC PROCEDURE SUBSUBLIS(U,V);
   BEGIN SCALAR X;
      RETURN IF X := ASSOC(V,U) THEN CDR X
	      ELSE IF ATOM V THEN V
	      ELSE IF NOT IDP CAR V
	       THEN FOR EACH J IN V COLLECT SUBSUBLIS(U,J)
	      ELSE IF FLAGP(CAR V,'SUBFN) THEN SUBSUBF(U,V)
	      ELSE IF GET(CAR V,'DNAME) THEN V
	      ELSE FOR EACH J IN V COLLECT SUBSUBLIS(U,J)
   END;

SYMBOLIC PROCEDURE SUBSUBF(L,EXPN);
   %Sets up a formal SUB expression when necessary;
   BEGIN SCALAR X,Y;
      FOR EACH J IN CDDR EXPN DO
	 IF (X := ASSOC(J,L)) THEN <<Y := X . Y; L := DELETE(X,L)>>;
      EXPN := SUBLIS(L,CAR EXPN)
		 . FOR EACH J IN CDR EXPN COLLECT SUBSUBLIS(L,J);
	%to ensure only opr and individual args are transformed;
      IF NULL Y THEN RETURN EXPN;
      EXPN := ACONC(FOR EACH J IN REVERSIP Y
		     COLLECT LIST('EQUAL,CAR J,CDR J),EXPN);
      RETURN MK!*SQ IF L THEN SIMPSUB EXPN
		     ELSE !*P2Q MKSP('SUB . EXPN,1)
   END;

FLAG('(INT DF),'SUBFN);

SYMBOLIC PROCEDURE KERNP U;
   DENR U=1 AND NOT DOMAINP(U := NUMR U)
	AND NULL RED U AND ONEP LC U AND LDEG U=1;


%*********************************************************************
%	   FUNCTIONS FOR RAISING CANONICAL FORMS TO A POWER
%********************************************************************;

SYMBOLIC PROCEDURE EXPTSQ(U,N);
   BEGIN SCALAR X;
	IF N=1 THEN RETURN U
	 ELSE IF N=0
	   THEN RETURN IF NULL NUMR U THEN REDERR " 0**0 formed"
			ELSE 1 ./ 1
	 ELSE IF NULL NUMR U THEN RETURN U
	 ELSE IF N<0 THEN RETURN SIMPEXPT LIST(MK!*SQ U,N)
	 ELSE IF NULL !*EXP
	  THEN RETURN MKSFPF(NUMR U,N) ./ MKSFPF(DENR U,N)
	 ELSE IF KERNP U THEN RETURN MKSQ(MVAR NUMR U,N)
	 ELSE IF DOMAINP NUMR U
	  THEN RETURN MULTSQ(!:EXPT(NUMR U,N) ./ 1,
		             1 ./ EXPTF(DENR U,N))
	 ELSE IF DENR U=1 THEN RETURN EXPTF(NUMR U,N) ./ 1;
	X := U;
	WHILE (N := N-1)>0 DO X := MULTSQ(U,X);
	RETURN X
   END;

SYMBOLIC PROCEDURE EXPTF(U,N);
   IF DOMAINP U THEN !:EXPT(U,N)
    ELSE IF !*EXP OR KERNLP U THEN EXPTF1(U,N)
    ELSE MKSFPF(U,N);

SYMBOLIC PROCEDURE EXPTF1(U,N);
   %iterative multiplication seems to be faster than a binary sub-
   %division algorithm, probably because multiplying a small polynomial
   %by a large one is cheaper than multiplying two medium sized ones;
   BEGIN SCALAR X;
      X: = U;
      WHILE (N := N-1)>0 DO X := MULTF(U,X);
      RETURN X
   END;


%*********************************************************************
%		 FUNCTIONS FOR MAKING STANDARD POWERS
%********************************************************************;

SYMBOLIC SMACRO PROCEDURE GETPOWER(U,N);
   %U is a list (<kernel> . <properties>), N a positive integer.
   %Value is the standard power of U**N;
   CAR U . N;
%   BEGIN SCALAR V;
%	V := CADR U;
%	IF NULL V THEN RETURN CAAR RPLACA(CDR U,LIST (CAR U . N));
%    A:	IF N=CDAR V THEN RETURN CAR V
%	 ELSE IF N<CDAR V
%	    THEN RETURN CAR RPLACW(V,(CAAR V . N) . (CAR V . CDR V))
%	 ELSE IF NULL CDR V
%	    THEN RETURN CADR RPLACD(V,LIST (CAAR V . N));
%	V := CDR V;
%	GO TO A
%   END;

SYMBOLIC PROCEDURE MKSP(U,P);
   %U is a (non-unique) kernel and P a non-zero integer
   %Value is the standard power for U**P;
   GETPOWER(FKERN U,P);

SYMBOLIC PROCEDURE U TO P;
   %U is a (unique) kernel and P a non-zero integer;
   %Value is the standard power of U**P;
   U . P;
%   GETPOWER(FKERN U,P);

SYMBOLIC PROCEDURE FKERN U;
   %finds the unique "p-list" reference to the kernel U. The choice of
   %the search and merge used here has a strong influence on some
   %timings. The ordered list used here is also used by Prepsq* to
   %order factors in printed output, so cannot be unilaterally changed;
   BEGIN SCALAR X,Y;
	IF ATOM U THEN RETURN LIST(U,NIL);
	Y := IF ATOM CAR U THEN GET(CAR U,'KLIST) ELSE EXLIST!*;
	IF NOT (X := ASSOC(U,Y))
	  THEN <<X := LIST(U,NIL);
		 Y := ORDAD(X,Y);
		 IF ATOM CAR U
		   THEN <<KPROPS!* := UNION(LIST CAR U,KPROPS!*);
			  PUT(CAR U,'KLIST,Y)>>
		  ELSE EXLIST!* := Y>>;
	RETURN X
   END;

SYMBOLIC PROCEDURE MKSFPF(U,N);
   %raises form U to power N with EXP off. Returns a form;
%   IF DOMAINP U THEN !:EXPT(U,N)
%    ELSE IF N>=0 AND KERNLP U
%     THEN IF NULL RED U AND ONEP LC U THEN !*P2F MKSP(MVAR U,LDEG U*N)
%	   ELSE EXPTF1(U,N)
%    ELSE IF N=1 OR NULL SUBFG!* THEN MKSP!*(U,N)
%    ELSE (LAMBDA X; %IF X AND CDR X<=N THEN NIL ELSE MKSP!*(U,N))
%	  ASSOC(U,ASYMPLIS!*);
   EXPTF(MKPROD!* U,N);

SYMBOLIC PROCEDURE MKSQ(U,N);
    %U is a kernel, N a non-zero integer;
    %Value is a standard quotient of U**N, after making any
    %possible substitutions for U;
   BEGIN SCALAR X,Y,Z;
	IF NULL SUBFG!* THEN GO TO A1
	 ELSE IF (Y := ASSOC(U,WTL!*))
		AND NULL CAR(Y := MKSQ('K!*,N*CDR Y)) THEN RETURN Y
	 ELSE IF NOT ATOM U THEN GO TO B
	 ELSE IF NULL !*NOSUBS AND (Z:= GET(U,'AVALUE)) THEN GO TO D;
	FLAG(LIST U,'USED!*);  %tell system U used as algebraic var;
    A:	IF !*NOSUBS OR N=1 THEN GO TO A1
	 ELSE IF (Z:= ASSOC(U,ASYMPLIS!*)) AND CDR Z<=N
	  THEN RETURN NIL ./ 1
	 ELSE IF ((Z:= ASSOC(U,POWLIS!*))
		OR NOT ATOM U AND CAR U MEMQ '(EXPT SQRT)
		AND (Z := ASSOC(CADR U,POWLIS!*)))
	     AND NOT(N*CADR Z)<0
	   %implements explicit sign matching;
	  THEN !*SUB2 := T;
    A1: IF NULL X THEN X := FKERN U;
	X := !*P2F GETPOWER(X,N) ./ 1;
	RETURN IF Y THEN MULTSQ(Y,X) ELSE X;
    B:	IF NULL !*NOSUBS AND ATOM CAR U
	   AND (Z:= ASSOC(U,GET(CAR U,'KVALUE)))
	  THEN GO TO C
	 ELSE IF NOT('USED!* MEMQ CDDR (X := FKERN U))
	  THEN ACONC(X,'USED!*);
	GO TO A;
    C:	Z := CDR Z;
    D:	%optimization is possible as shown if all expression
	%dependency is known;
	%IF CDR Z THEN RETURN EXPTSQ(CDR Z,N); %value already computed;
	IF NULL !*RESUBS THEN !*NOSUBS := T;
	X := SIMPCAR Z;
	!*NOSUBS := NIL;
	%RPLACD(Z,X);		%save simplified value;
	%SUBL!* := Z . SUBL!*;
	RETURN EXPTSQ(X,N)
   END;


%*********************************************************************
%	    FUNCTIONS FOR INTERNAL ORDERING OF EXPRESSIONS
%********************************************************************;

SYMBOLIC PROCEDURE ORDAD(A,U);
   IF NULL U THEN LIST A
    ELSE IF ORDP(A,CAR U) THEN A . U
    ELSE CAR U . ORDAD(A,CDR U);

SYMBOLIC PROCEDURE ORDN U;
   IF NULL U THEN NIL
    ELSE IF NULL CDR U THEN U
    ELSE IF NULL CDDR U THEN ORD2(CAR U,CADR U)
    ELSE ORDAD(CAR U,ORDN CDR U);

SYMBOLIC PROCEDURE ORD2(U,V);
   IF ORDP(U,V) THEN LIST(U,V) ELSE LIST(V,U);

SYMBOLIC PROCEDURE ORDP(U,V);
   %returns TRUE if U ordered ahead or equal to V, NIL otherwise.
   %an expression with more structure at a given level is ordered 
   %ahead of one with less;
   IF NULL U THEN NULL V
    ELSE IF NULL V THEN T
    ELSE IF ATOM U
       THEN IF ATOM V
		THEN IF NUMBERP U THEN NUMBERP V AND NOT U<V
		      ELSE IF NUMBERP V THEN T ELSE ORDERP(U,V)
	     ELSE NIL
    ELSE IF ATOM V THEN T
    ELSE IF CAR U=CAR V THEN ORDP(CDR U,CDR V)
    ELSE ORDP(CAR U,CAR V);

SYMBOLIC PROCEDURE ORDPP(U,V);
   IF CAR U EQ CAR V THEN CDR U>CDR V
    ELSE IF NCMP!* THEN NCMORDP(CAR U,CAR V)
    ELSE ORDOP(CAR U,CAR V);

SYMBOLIC PROCEDURE ORDOP(U,V);
   BEGIN SCALAR X;
	X := KORD!*;
    A:	IF NULL X THEN RETURN ORDP(U,V)
	 ELSE IF U EQ CAR X THEN RETURN T
	 ELSE IF V EQ CAR X THEN RETURN;
	X := CDR X;
	GO TO A
   END;

SYMBOLIC PROCEDURE NCMORDP(U,V);
   IF NONCOMP U THEN IF NONCOMP V THEN ORDOP(U,V) ELSE T
    ELSE IF NONCOMP V THEN NIL
    ELSE ORDOP(U,V);


%*********************************************************************
%	       FUNCTIONS FOR REORDERING STANDARD FORMS
%*********************************************************************;

SYMBOLIC PROCEDURE REORDER U;
   %reorders a standard form so that current kernel order is used;
   IF DOMAINP U THEN U
    ELSE RADDF(RMULTPF(LPOW U,REORDER LC U),REORDER RED U);

SYMBOLIC PROCEDURE RADDF(U,V);
   %adds reordered forms U and V;
   IF NULL U THEN V
    ELSE IF NULL V THEN U
    ELSE IF DOMAINP U THEN ADDD(U,V)
    ELSE IF DOMAINP V THEN ADDD(V,U)
    ELSE IF PEQ(LPOW U,LPOW V)
     THEN (LPOW U .* RADDF(LC U,LC V)) .+ RADDF(RED U,RED V)
    ELSE IF ORDPP(LPOW U,LPOW V) THEN LT U . RADDF(RED U,V)
    ELSE LT V . RADDF(U,RED V);

SYMBOLIC PROCEDURE RMULTPF(U,V);
  %multiplies power U by reordered form V;
   IF NULL V THEN NIL
    ELSE IF DOMAINP V OR ORDOP(CAR U,MVAR V) THEN !*T2F(U .* V)
    ELSE (LPOW V .* RMULTPF(U,LC V)) .+ RMULTPF(U,RED V);

SYMBOLIC PROCEDURE KORDER U;
   <<KORD!* := IF U = '(NIL) THEN NIL
	        ELSE FOR EACH X IN U COLLECT !*A2K X;
     RMSUBS()>>;

RLISTAT '(KORDER);

SYMBOLIC PROCEDURE SETKORDER U;
   BEGIN SCALAR V; V := KORD!*; KORD!* := U; RETURN V END;


%*********************************************************************
%	  FUNCTIONS WHICH APPLY BASIC PATTERN MATCHING RULES
%********************************************************************;

SYMBOLIC PROCEDURE EMTCH U;
   IF ATOM U THEN U ELSE (LAMBDA X; IF X THEN X ELSE U) OPMTCH U;

SYMBOLIC PROCEDURE OPMTCH U;
   BEGIN SCALAR X,Y,Z;
	X := GET(CAR U,'OPMTCH);
	IF NULL X THEN RETURN NIL
	 ELSE IF NULL SUBFG!* THEN RETURN NIL;  %NULL(!*SUB2 := T);
	Z := FOR EACH J IN CDR U COLLECT EMTCH J;
    A:	IF NULL X THEN RETURN;
	Y := MCHARG(Z,CAAR X,CAR U);
    B:	IF NULL Y THEN GO TO C
	 ELSE IF EVAL SUBLA(CAR Y,CDADAR X)
	  THEN RETURN SUBLA(CAR Y,CADDAR X);
	Y := CDR Y;
	GO TO B;
    C:	X := CDR X;
	GO TO A
   END;

SYMBOLIC PROCEDURE MCHARG(U,V,W);
   %procedure to determine if an argument list matches given template;
   %U is argument list of operator W;
   %V is argument list template being matched against;
   %if there is no match, value is NIL,
   %otherwise a list of lists of free variable pairings;
   IF NULL U AND NULL V THEN LIST NIL
    ELSE BEGIN INTEGER M,N;
	M := LENGTH U;
	N := LENGTH V;
	IF FLAGP(W,'NARY) AND M>2
	  THEN IF M<6 AND FLAGP(W,'SYMMETRIC)
			     THEN RETURN MCHCOMB(U,V,W)
		ELSE IF N=2 THEN <<U := CDR MKBIN(W,U); M := 2>>
		ELSE RETURN NIL;   %we cannot handle this case;
	RETURN IF M NEQ N THEN NIL
		ELSE IF FLAGP(W,'SYMMETRIC) THEN MCHSARG(U,V)
		ELSE IF MTP V THEN LIST PAIR(V,U)
		ELSE MCHARG2(U,V,LIST NIL)
   END;

SYMBOLIC PROCEDURE MCHCOMB(U,V,OP);
   BEGIN INTEGER N;
      N := LENGTH U - LENGTH V +1;
      IF N<1 THEN RETURN NIL
       ELSE IF N=1 THEN RETURN MCHSARG(U,V)
       ELSE IF NOT SMEMQLP(FRLIS!*,V) THEN RETURN NIL;
      RETURN FOR EACH X IN COMB(U,N) CONC
	MCHSARG((OP . X) . SETDIFF(U,X),V)
   END;

SYMBOLIC PROCEDURE COMB(U,N);
   %value is list of all combinations of N elements from the list U;
   BEGIN SCALAR V; INTEGER M;
	IF N=0 THEN RETURN LIST NIL
	 ELSE IF (M:=LENGTH U-N)<0 THEN RETURN;
    A:	IF M=0 THEN RETURN U . V;
	V := NCONC(V,MAPCONS(COMB(CDR U,N-1),CAR U));
	U := CDR U;
	M := M-1;
	GO TO A
   END;

SYMBOLIC PROCEDURE MCHARG2(U,V,W);
   %matches compatible list U against template V;
   BEGIN SCALAR Y;
	IF NULL U THEN RETURN W;
	Y := MCHK(CAR U,CAR V);
	U := CDR U;
	V := CDR V;
	RETURN FOR EACH J IN Y
	   CONC MCHARG2(U,UPDTEMPLATE(J,V),MAPPEND(W,J))
   END;

SYMBOLIC PROCEDURE UPDTEMPLATE(U,V);
   BEGIN SCALAR X,Y;
      RETURN FOR EACH J IN V COLLECT
	IF (X := SUBLA(U,J)) = J THEN J
	 ELSE IF (Y := REVAL X) NEQ X THEN Y
	 ELSE X
   END;

SYMBOLIC PROCEDURE MCHK(U,V);
   IF U=V THEN LIST NIL
    ELSE IF ATOM V
	   THEN IF V MEMQ FRLIS!* THEN LIST LIST (V . U) ELSE NIL
    ELSE IF ATOM U	%special check for negative number match;
     THEN IF NUMBERP U AND U<0 THEN MCHK(LIST('MINUS,-U),V)
	   ELSE NIL
    ELSE IF CAR U EQ CAR V THEN MCHARG(CDR U,CDR V,CAR U)
    ELSE NIL;

SYMBOLIC PROCEDURE MKBIN(U,V);
   IF NULL CDDR V THEN U . V ELSE LIST(U,CAR V,MKBIN(U,CDR V));

SYMBOLIC PROCEDURE MTP V;
   NULL V OR (CAR V MEMQ FRLIS!* AND NOT CAR V MEMBER CDR V
       AND MTP CDR V);

SYMBOLIC PROCEDURE MCHSARG(U,V);
   REVERSIP IF MTP V
     THEN FOR EACH J IN PERMUTATIONS V COLLECT PAIR(J,U)
    ELSE FOR EACH J IN PERMUTATIONS U CONC MCHARG2(J,V,LIST NIL);

SYMBOLIC PROCEDURE PERMUTATIONS U;
   IF NULL U THEN LIST U
    ELSE FOR EACH J IN U CONC MAPCONS(PERMUTATIONS DELETE(J,U),J);

FLAGOP ANTISYMMETRIC,SYMMETRIC;

FLAG ('(PLUS TIMES CONS),'SYMMETRIC);


%*********************************************************************
%     FUNCTIONS FOR CONVERTING CANONICAL FORMS INTO PREFIX FORMS
%********************************************************************;

SYMBOLIC PROCEDURE PREPSQ U;
   IF NULL NUMR U THEN 0 ELSE SQFORM(U,FUNCTION PREPF);

SYMBOLIC PROCEDURE SQFORM(U,V);
   (LAMBDA (X,Y); IF Y=1 THEN X ELSE LIST('QUOTIENT,X,Y))
      (APPLY(V,LIST NUMR U),APPLY(V,LIST DENR U));

SYMBOLIC PROCEDURE PREPF U;
   REPLUS PREPF1(U,NIL);

SYMBOLIC PROCEDURE PREPF1(U,V);
   IF NULL U THEN NIL
    ELSE IF DOMAINP U
     THEN LIST RETIMES((IF ATOM U
			 THEN IF U<0 THEN LIST('MINUS,-U) ELSE U
			ELSE IF APPLY(GET(CAR U,'MINUSP),LIST U)
			 THEN LIST('MINUS,PREPD !:MINUS U)
					 ELSE PREPD U)
				. EXCHK(V,NIL,NIL))
    ELSE NCONC(PREPF1(LC U,IF MVAR U EQ 'K!* THEN V ELSE LPOW U .* V)
	       ,PREPF1(RED U,V));

SYMBOLIC PROCEDURE PREPD U; APPLY(GET(CAR U,'PREPFN),LIST U);

SYMBOLIC PROCEDURE EXCHK(U,V,W);
   IF NULL U
     THEN IF NULL W THEN V
	   ELSE EXCHK(U,LIST('EXPT,CAAR W,PREPSQX CDAR W) . V,CDR W)
    ELSE IF EQCAR(CAAR U,'EXPT)
     THEN EXCHK(CDR U,V,
       BEGIN SCALAR X,Y;
	X := ASSOC(CADAAR U,W);
	Y := SIMP LIST('TIMES,CDAR U,CADDAR CAR U);
	IF X THEN RPLACD(X,ADDSQ(Y,CDR X))
	 ELSE W := (CADAAR U . Y) . W;
	RETURN W
       END)
    ELSE IF CDAR U=1 THEN EXCHK(CDR U, SQCHK CAAR U . V,W)
    ELSE EXCHK(CDR U,LIST('EXPT,SQCHK CAAR U,CDAR U) . V,W);

SYMBOLIC PROCEDURE REPLUS U;
   IF ATOM U THEN U ELSE IF NULL CDR U THEN CAR U ELSE 'PLUS . U;

SYMBOLIC PROCEDURE RETIMES U;
   BEGIN SCALAR X,Y;
    A:	IF NULL U THEN GO TO D
	 ELSE IF ONEP CAR U THEN GO TO C
	 ELSE IF NOT EQCAR(CAR U,'MINUS) THEN GO TO B;
	X := NOT X;
	 IF ONEP CADAR U THEN GO TO C
	 ELSE U := CADAR U . CDR U;
    B:	Y := CAR U . Y;
    C:	U := CDR U;
	GO TO A;
    D:	Y := IF NULL Y THEN 1
		ELSE IF CDR Y THEN 'TIMES . REVERSE Y ELSE CAR Y;
	RETURN IF X THEN LIST('MINUS,Y) ELSE Y
   END;

SYMBOLIC PROCEDURE SQCHK U;
   IF ATOM U THEN U
    ELSE IF CAR U EQ '!*SQ THEN PREPSQ CADR U
    ELSE IF CAR U EQ 'EXPT AND CADDR U=1 THEN CADR U
    ELSE IF ATOM CAR U THEN U ELSE PREPF U;


%*********************************************************************
%	       BASIC OUTPUT PACKAGE FOR CANONICAL FORMS
%********************************************************************;

%Global variables referenced in this section;

GLOBAL '(VARNAM!* ORIG!* YCOORD!* YMIN!* SPARE!*);

SPARE!* := 5; %RIGHT MARGIN, TO AVOID TROUBLE WITH PREMATURE
	      %LINE-BREAKS INSERTED BY LISP;
VARNAM!* := 'ANS;
ORIG!*:=0;
POSN!* := 0;
YCOORD!* := 0;
YMIN!* := 0;

DEFLIST ('((!*SQ !*SQPRINT)),'SPECPRN);

SYMBOLIC PROCEDURE !*SQPRINT U; SQPRINT CAR U;

SYMBOLIC PROCEDURE SQPRINT U;
   %mathprints the standard quotient U;
   BEGIN SCALAR Z;
	Z := ORIG!*;
	IF !*NAT AND POSN!*<20 THEN ORIG!* := POSN!*;
	IF !*PRI OR WTL!* THEN GO TO C
	 ELSE IF CDR U NEQ 1 THEN GO TO B
	 ELSE XPRINF(CAR U,NIL,NIL);
    A:	RETURN (ORIG!* := Z);
    B:	PRIN2!* "(";
	XPRINF(CAR U,NIL,NIL);
	PRIN2!* ") / (";;
	XPRINF(CDR U,NIL,NIL);
	PRIN2!* ")";
	GO TO A;
    C:	MAPRIN(!*OUTP := U := PREPSQ!* U);
	GO TO A
   END;

SYMBOLIC PROCEDURE VARPRI(U,V,W);
   BEGIN SCALAR X,Y;
   %U is expression being printed
   %V is a list of expressions assigned to U
   %W is a flag which is true if expr is last in current set;
	IF NULL U THEN U := 0;	 %allow for unset array elements;
	IF !*NERO AND U=0 THEN RETURN;
	IF W MEMQ '(FIRST ONLY) THEN TERPRI!* T;
	X := TYPL!*;
    A:	IF NULL X THEN GO TO B
	 ELSE IF APPLY(CAR X,LIST U) AND (Y:= GET(CAR X,'PRIFN))
	  THEN RETURN APPLY(Y,LIST(U,V,W));
	X := CDR X;
	GO TO A;
    B:	IF !*FORT THEN RETURN FVARPRI(U,V,W)
	 ELSE IF NULL V THEN GO TO C;
	INPRINT('SETQ,GET('SETQ,'INFIX),MAPCAR(V,FUNCTION EVAL));
	OPRIN 'SETQ;
    C:	MAPRIN U;
	IF NULL W OR W EQ 'FIRST THEN RETURN NIL
	 ELSE IF NOT !*NAT THEN PRIN2!* "$";
	TERPRI!*(NOT !*NAT);
	RETURN
   END;

SYMBOLIC PROCEDURE XPRINF(U,V,W);
   %U is a standard form.
   %V is a flag which is true if a term has preceded current form.
   %W is a flag which is true if form is part of a standard term;
   %Procedure prints the form and returns NIL;
   BEGIN
    A:	IF NULL U THEN RETURN NIL
	 ELSE IF DOMAINP U THEN RETURN XPRID(U,V,W);
	XPRINT(LT U,V);
	U := RED U;
	V := T;
	GO TO A
   END;

SYMBOLIC PROCEDURE XPRID(U,V,W);
   %U is a domain element.
   %V is a flag which is true if a term has preceded element.
   %W is a flag which is true if U is part of a standard term.
   %Procedure prints element and returns NIL;
   BEGIN
	IF MINUSF U THEN <<OPRIN 'MINUS; U := !:MINUS U>>
	 ELSE IF V THEN OPRIN 'PLUS;
	IF NOT W OR U NEQ 1
	  THEN IF ATOM U THEN PRIN2!* U ELSE MAPRIN U
   END;

SYMBOLIC PROCEDURE XPRINT(U,V);
   %U is a standard term.
   %V is a flag which is true if a term has preceded this term.
   %Procedure prints the term and returns NIL;
   BEGIN SCALAR FLG,W;
	FLG := NOT ATOM TC U AND RED TC U;
	IF NOT FLG THEN GO TO A ELSE IF V THEN OPRIN 'PLUS;
	PRIN2!* "(";
    A:	XPRINF(TC U,IF FLG THEN NIL ELSE V,NOT FLG);
	IF FLG THEN PRIN2!* ")";
	IF NOT ATOM TC U OR NOT ABS FIX TC U=1 THEN OPRIN 'TIMES;
	W := TPOW U;
	IF ATOM CAR W THEN PRIN2!* CAR W
	 ELSE IF NOT ATOM CAAR W OR CAAR W EQ '!*SQ THEN GO TO C
	 ELSE IF CAAR W EQ 'PLUS THEN MAPRINT(CAR W,100)
	 ELSE MAPRIN CAR W;
    B:	IF CDR W=1 THEN RETURN;
	OPRIN 'EXPT;
	PRIN2!* CDR W;
	IF NOT !*NAT THEN RETURN;
	YCOORD!* := YCOORD!*-1;
	IF YMIN!*>YCOORD!* THEN YMIN!* := YCOORD!*;
	RETURN;
    C:	PRIN2!* "(";
	IF NOT ATOM CAAR W THEN XPRINF(CAR W,NIL,NIL)
	 ELSE SQPRINT CADAR W;
	PRIN2!* ")";
	GO TO B
   END;


%*********************************************************************
%	       FUNCTIONS FOR PRINTING PREFIX EXPRESSIONS
%********************************************************************;

%Global variables referenced in this sub-section;

GLOBAL '(OBRKP!* PLINE!* !*FORT !*LIST !*NAT YMAX!*);

OBRKP!* := T;
PLINE!* := NIL;
!*FORT:=NIL;
!*LIST := NIL;
!*NAT := NAT!*!* := T;
YMAX!* := 0;

INITL!* := APPEND('(ORIG!* PLINE!*),INITL!*);

PUT('ORIG!*,'INITL,0);

FLAG('(LINELENGTH),'OPFN);  %to make it a symbolic operator;


SYMBOLIC PROCEDURE MATHPRINT L;
   BEGIN TERPRI!* T; MAPRIN L; TERPRI!* T END;

SYMBOLIC PROCEDURE MAPRIN U;
   MAPRINT(U,0);

SYMBOLIC PROCEDURE MAPRINT(L,P);
   BEGIN SCALAR X,Y;
	IF NULL L THEN RETURN NIL
	 ELSE IF ATOM L THEN GO TO B
	 ELSE IF STRINGP L THEN RETURN PRIN2!* L
	 ELSE IF NOT ATOM CAR L THEN MAPRINT(CAR L,P)
	 ELSE IF X := GET(CAR L,'SPECPRN)
	  THEN RETURN APPLY(X,LIST CDR L)
	 ELSE IF X := GET(CAR L,'INFIX) THEN GO TO A
	 ELSE PRIN2!* CAR L;
	PRIN2!* "(";
	OBRKP!* := NIL;
	IF CDR L THEN INPRINT('!*COMMA!*,0,CDR L);
	OBRKP!* := T;
    E:	RETURN PRIN2!* ")";
    B:	IF NUMBERP L THEN GO TO D;
    C:	RETURN PRIN2!* L;
    D:	IF NOT L<0 THEN GO TO C;
	PRIN2!* "(";
	PRIN2!* L;
	GO TO E;
    A:	P := NOT X>P;
	IF NOT P THEN GO TO G;
	Y := ORIG!*;
	PRIN2!* "(";
	ORIG!* := IF POSN!*<18 THEN POSN!* ELSE ORIG!*+3;
    G:	INPRINT(CAR L,X,CDR L);
	IF NOT P THEN RETURN;
	PRIN2!* ")";
	ORIG!* := Y
   END;

SYMBOLIC PROCEDURE INPRINT(OP,P,L);
   BEGIN
	IF GET(OP,'ALT) THEN GO TO A
	 ELSE IF OP EQ 'EXPT AND !*NAT
	   AND FLATSIZEC CAR L+FLATSIZEC CADR L>
		    (LINELENGTH NIL-SPARE!*)-POSN!*
	  THEN TERPRI!* T;   %to avoid breaking exponent over line;
	MAPRINT(CAR L,P);
    A0: L := CDR L;
    A:	IF NULL L THEN RETURN NIL
	 ELSE IF NOT ATOM CAR L AND OP EQ GET!*(CAAR L,'ALT)
	  THEN GO TO B;
	OPRIN OP;
    B:	MAPRINT(CAR L,P);
	IF NOT !*NAT OR NOT OP EQ 'EXPT THEN GO TO A0;
	YCOORD!* := YCOORD!*-1;
	IF YMIN!*>YCOORD!* THEN YMIN!* := YCOORD!*;
	GO TO A0
   END;

SYMBOLIC PROCEDURE FLATSIZEC U;
   IF NULL U THEN 0
    ELSE IF ATOM U THEN LENGTHC U
    ELSE FLATSIZEC CAR U + FLATSIZEC CDR U;

SYMBOLIC PROCEDURE OPRIN OP;
   (LAMBDA X;
	 IF NULL X THEN PRIN2!* OP
	  ELSE IF !*FORT THEN PRIN2!* CADR X
	  ELSE IF !*LIST AND OBRKP!* AND OP MEMQ '(PLUS MINUS)
	   THEN BEGIN TERPRI!* T; PRIN2!* CAR X END
	  ELSE IF !*NAT AND OP EQ 'EXPT
	  THEN BEGIN
		YCOORD!* := YCOORD!*+1;
		IF YCOORD!*>YMAX!* THEN YMAX!* := YCOORD!*
	       END
	 ELSE PRIN2!* CAR X)
      GET(OP,'PRTCH);


SYMBOLIC PROCEDURE PRIN2!* U;
   BEGIN INTEGER M,N;
	IF !*FORT THEN RETURN FPRIN2 U;
	N := LENGTHC U;
	IF N>(LINELENGTH NIL-SPARE!*) THEN GO TO D;
	M := POSN!*+N;
    A:	IF M>(LINELENGTH NIL-SPARE!*) THEN GO TO C
	 ELSE IF NOT !*NAT THEN PRIN2 U
	 ELSE PLINE!* := (((POSN!* . M) . YCOORD!*) . U) . PLINE!*;
    B:	RETURN (POSN!* := M);
    C:	TERPRI!* T;
	IF (M := POSN!*+N)<=(LINELENGTH NIL-SPARE!*) THEN GO TO A;
    D:	%identifier longer than one line;
	IF !*FORT THEN REDERR LIST(U,"too long for FORTRAN");
	%let LISP print the atom;
	TERPRI!* NIL;
	PRIN2T U;
	M := REMAINDER(N,(LINELENGTH NIL-SPARE!*));
	GO TO B
   END;

SYMBOLIC PROCEDURE TERPRI!* U;
   BEGIN INTEGER N;
	IF !*FORT THEN RETURN FTERPRI(U)
	 ELSE IF NOT PLINE!* OR NOT !*NAT THEN GO TO B;
	N := YMAX!*;
	PLINE!* := REVERSE PLINE!*;
    A:	SCPRINT(PLINE!*,N);
	TERPRI();
	IF N= YMIN!* THEN GO TO B;
	N := N-1;
	GO TO A;
    B:	IF U THEN TERPRI();
    C:	PLINE!* := NIL;
	POSN!* := ORIG!*;
	YCOORD!* := YMAX!* := YMIN!* := 0
   END;

SYMBOLIC PROCEDURE SCPRINT(U,N);
   BEGIN SCALAR M;
	POSN!* := 0;
    A:	IF NULL U THEN RETURN NIL
	 ELSE IF NOT CDAAR U=N THEN GO TO B
	 ELSE IF NOT (M:= CAAAAR U-POSN!*)<0 THEN SPACES M;
	PRIN2 CDAR U;
	POSN!* := CDAAAR U;
    B:	U := CDR U;
	GO TO A
   END;


COMMENT ***** FORTRAN OUTPUT PACKAGE *****;

GLOBAL '(CARDNO!* FORTWIDTH!*);

FLAG ('(CARDNO!* FORTWIDTH!*),'SHARE);

CARDNO!*:=20;

FORTWIDTH!* := 70;

FLUID '(FBRKT);   %bracket level counter;

SYMBOLIC PROCEDURE VARNAME U;
   %sets the default variable assignment name;
   VARNAM!* := CAR U;

RLISTAT '(VARNAME);

SYMBOLIC PROCEDURE FLENGTH(U,CHARS);
   IF CHARS<0 THEN CHARS
    ELSE IF ATOM U
     THEN CHARS-IF NUMBERP U THEN IF FIXP U THEN FLATSIZEC U+1
				   ELSE FLATSIZEC U
		 ELSE FLATSIZEC((LAMBDA X; IF X THEN CADR X ELSE U)
				   GET(U,'PRTCH))
    ELSE FLENGTH(CAR U,FLENLIS(CDR U,CHARS)-2);

SYMBOLIC PROCEDURE FLENLIS(U,CHARS);
   IF NULL U THEN CHARS
    ELSE IF CHARS<0 THEN CHARS
    ELSE IF ATOM U THEN FLENGTH(U,CHARS)
    ELSE FLENLIS(CDR U,FLENGTH(CAR U,CHARS));

SYMBOLIC PROCEDURE FMPRINT(L,P);
   BEGIN SCALAR X;
	IF NULL L THEN RETURN NIL
	 ELSE IF ATOM L THEN GO TO B
	 ELSE IF STRINGP L THEN RETURN FPRIN2 L
	 ELSE IF NOT ATOM CAR L THEN FMPRINT(CAR L,P)
	 ELSE IF X := GET(CAR L,'INFIX) THEN GO TO A
	 ELSE IF X := GET(CAR L,'SPECPRN)
	  THEN RETURN APPLY(X,LIST CDR L) ELSE FPRIN2 CAR L;
	FPRIN2 "(";
	FBRKT := NIL . FBRKT;
	X := !*PERIOD; !*PERIOD := NIL; %turn off . inside an op exp;
	IF CDR L THEN FNPRINT('!*COMMA!*,0,CDR L);
	!*PERIOD := X;
    E:	FPRIN2 ")";
	RETURN FBRKT := CDR FBRKT;
    B:	IF NUMBERP L THEN GO TO D;
    C:	RETURN FPRIN2 L;
    D:	IF NOT L<0 THEN GO TO C;
	FPRIN2 "(";
	FBRKT := NIL . FBRKT;
	FPRIN2 L;
	GO TO E;
    A:	P := NOT X>P;
	IF P THEN <<FPRIN2 "("; FBRKT := NIL . FBRKT>>;
	FNPRINT(CAR L,X,CDR L);
	IF P THEN <<FPRIN2 ")"; FBRKT := CDR FBRKT>>
   END;

SYMBOLIC PROCEDURE FNPRINT(OP,P,L);
   BEGIN
	IF OP EQ 'EXPT THEN RETURN FEXPPRI(P,L)
	 ELSE IF GET(OP,'ALT) THEN GO TO A;
	FMPRINT(CAR L,P);
    A0: L := CDR L;
    A:	IF NULL L THEN RETURN NIL
	 ELSE IF NOT ATOM CAR L AND OP EQ GET!*(CAAR L,'ALT)
	  THEN GO TO B;
	FOPRIN OP;
    B:	FMPRINT(CAR L,P);
	GO TO A0
   END;

SYMBOLIC PROCEDURE FEXPPRI(P,L);
   BEGIN SCALAR PPERIOD;
      FMPRINT(CAR L,P);
      FOPRIN 'EXPT;
      PPERIOD := !*PERIOD;
      IF NUMBERP CADR L THEN !*PERIOD := NIL ELSE !*PERIOD := T;
      FMPRINT(CADR L,P);
      !*PERIOD := PPERIOD
   END;

SYMBOLIC PROCEDURE FOPRIN OP;
   (LAMBDA X; IF NULL X THEN FPRIN2 OP ELSE FPRIN2 CADR X)
      GET(OP,'PRTCH);

FLUID '(COUNTR EXPLIS FVAR NCHARS VAR);

SYMBOLIC PROCEDURE FVARPRI(U,V,W);
   %prints an assignment in FORTRAN notation;
   BEGIN INTEGER COUNTR,LLENGTH,NCHARS; SCALAR EXPLIS,FVAR,VAR;
	 LLENGTH := LINELENGTH NIL;
	 LINELENGTH FORTWIDTH!*;
	IF STRINGP U
	  THEN RETURN <<FPRIN2 U; IF W EQ 'ONLY THEN FTERPRI(T)>>;
	IF EQCAR(U,'!*SQ) THEN U := PREPSQ!* CADR U;
	COUNTR := 0;
	NCHARS := ((LINELENGTH NIL-SPARE!*)-12)*CARDNO!*;
	   %12 is to allow for indentation and end of line effects;
	VAR := VARNAM!*;
	FVAR := IF NULL V THEN VAR ELSE EVAL CAR V;
	IF POSN!*=0 AND W THEN FORTPRI(FVAR,U)
	 ELSE <<FMPRINT(U,0); IF W THEN FTERPRI W>>;
		%means that expression preceded by a string;
	LINELENGTH LLENGTH;
   END;

SYMBOLIC PROCEDURE FORTPRI(FVAR,XEXP);
   BEGIN SCALAR FBRKT;
	IF FLENGTH(XEXP,NCHARS)<0
	  THEN XEXP := CAR XEXP . FOUT(CDR XEXP,CAR XEXP);
	POSN!* := 0;
	FPRIN2 "      ";
	FMPRINT(FVAR,0);
	FPRIN2 "=";
	FMPRINT(XEXP,0);
	FTERPRI(T)
   END;

SYMBOLIC PROCEDURE FOUT(ARGS,OP);
   BEGIN INTEGER NCHARSL; SCALAR DISTOP,X,Z;
	NCHARSL := NCHARS;
	IF OP MEMQ '(PLUS TIMES) THEN DISTOP := OP;
	WHILE ARGS DO
	 <<X := CAR ARGS;
	   IF ATOM X AND (NCHARSL := FLENGTH(X,NCHARSL))
	      OR (NULL CDR ARGS OR DISTOP)
		AND (NCHARSL := FLENGTH(X,NCHARSL))>0
	     THEN Z := X . Z
	    ELSE IF DISTOP AND FLENGTH(X,NCHARS)>0
	     THEN <<Z := FOUT1(DISTOP . ARGS) . Z;
		    ARGS := LIST NIL>>
	    ELSE <<Z := FOUT1 X . Z;
		   NCHARSL := FLENGTH(OP,NCHARSL)>>;
	   NCHARSL := FLENGTH(OP,NCHARSL);
	   ARGS := CDR ARGS>>;
	RETURN REVERSIP Z
   END;

SYMBOLIC PROCEDURE FOUT1 XEXP;
   BEGIN SCALAR FVAR;
      FVAR := GENVAR();
      EXPLIS := (XEXP . FVAR) . EXPLIS;
      FORTPRI(FVAR,XEXP);
      RETURN FVAR
   END;

SYMBOLIC PROCEDURE FPRIN2 U;
   % FORTRAN output of U;
   BEGIN INTEGER M,N;
	N := FLATSIZEC U;
	M := POSN!*+N;
	IF NUMBERP U AND FIXP U AND !*PERIOD THEN M := M+1;
	IF M<(LINELENGTH NIL-SPARE!*) THEN POSN!* := M
	 ELSE <<TERPRI(); SPACES 5; PRIN2 ". "; POSN!* := N+7>>;
	PRIN2 U;
	IF NUMBERP U AND FIXP U AND !*PERIOD THEN PRIN2 "."
   END;

SYMBOLIC PROCEDURE FTERPRI(U);
   <<IF NOT POSN!*=0 AND U THEN TERPRI();
     POSN!* := 0>>;

SYMBOLIC PROCEDURE GENVAR;
   INTERN COMPRESS APPEND(EXPLODE VAR,EXPLODE(COUNTR := COUNTR + 1));

UNFLUID '(EXPLIS FBRKT FVAR NCHARS);


%*********************************************************************
%                           FOR ALL COMMAND
%********************************************************************;

SYMBOLIC PROCEDURE FORALLSTAT;
   BEGIN SCALAR ARBL,CONDS;
	IF CURSYM!* MEMQ LETL!* THEN SYMERR('forall,T);
	FLAG(LETL!*,'DELIM);
	ARBL := REMCOMMA XREAD NIL;
	IF CURSYM!* EQ 'SUCH THEN 
	  <<IF NOT SCAN() EQ 'THAT THEN SYMERR('let,T);
	    CONDS := XREAD NIL>>;
	REMFLAG(LETL!*,'DELIM);
	RETURN IFLET1(ARBL,CONDS)
   END;

SYMBOLIC PROCEDURE IFLET U; IFLET1(NIL,U);

SYMBOLIC PROCEDURE IFLET1(ARBL,CONDS);
   IF NOT CURSYM!* MEMQ LETL!* THEN SYMERR('let,T)
    ELSE LIST('FORALL,ARBL,CONDS,XREAD1 T);

SYMBOLIC PROCEDURE FORMARB(U,VARS,MODE);
   <<ARBL!* := CAR U . ARBL!*; MKQUOTE CAR U>>;

PUT('ARB,'FORMFN,'FORMARB);

PUT('FORALL,'STAT,'FORALLSTAT);

SYMBOLIC FEXPR PROCEDURE FORALL U;
   BEGIN SCALAR X,Y;
      X := FOR EACH J IN CAR U COLLECT NEWVAR J;
      Y := PAIR(CAR U,X);
      MCOND!* := SUBLA(Y,CADR U);
      FRASC!* := Y;
      FRLIS!* := UNION(X,FRLIS!*);
      RETURN EVAL CADDR U
   END;

SYMBOLIC PROCEDURE FORMFORALL(U,VARS,MODE);
   BEGIN SCALAR ARBL!*,X;
%      VARS := APPEND(CAR U,VARS);   %semantics are different;
      IF NULL CADR U THEN X := T ELSE X := FORMBOOL(CADR U,VARS,MODE);
      RETURN LIST('FORALL,UNION(ARBL!*,CAR U),
		  X,FORM1(CADDR U,VARS,MODE))
   END;

PUT('FORALL,'FORMFN,'FORMFORALL);

SYMBOLIC PROCEDURE NEWVAR U;
   IF NOT IDP U THEN TYPERR(U,"free variable")
    ELSE INTERN COMPRESS APPEND(EXPLODE '!=,EXPLODE U);


%*********************************************************************
%		      2.19 SUBSTITUTION COMMANDS
%********************************************************************;

SYMBOLIC PROCEDURE FORMLET1(U,VARS,MODE);
   'LIST . FOR EACH X IN U COLLECT
      IF EQEXPR X
	THEN LIST('LIST,MKQUOTE 'EQUAL,FORM1(CADR X,VARS,MODE),
				!*S2ARG(FORM1(CADDR X,VARS,MODE),VARS))
       ELSE ERRPRI2(X,T);

SYMBOLIC PROCEDURE !*S2ARG(U,VARS);
   %makes all NOCHANGE operators into their listed form;
   IF ATOM U THEN U
    ELSE IF NOT IDP CAR U OR NOT FLAGP(CAR U,'NOCHANGE)
     THEN FOR EACH J IN U COLLECT !*S2ARG(J,VARS)
    ELSE MKARG(U,VARS);

PUT('LET,'FORMFN,'FORMLET);

PUT('CLEAR,'FORMFN,'FORMCLEAR);

PUT('MATCH,'FORMFN,'FORMMATCH);

SYMBOLIC PROCEDURE FORMCLEAR(U,VARS,MODE);
   LIST('CLEAR,FORMCLEAR1(U,VARS,MODE));

SYMBOLIC PROCEDURE FORMCLEAR1(U,VARS,MODE);
   'LIST . FOR EACH X IN U COLLECT FORM1(X,VARS,MODE);

SYMBOLIC PROCEDURE FORMLET(U,VARS,MODE);
   LIST('LET,FORMLET1(U,VARS,MODE));

SYMBOLIC PROCEDURE FORMMATCH(U,VARS,MODE);
   LIST('MATCH,FORMLET1(U,VARS,MODE));

SYMBOLIC PROCEDURE LET U;
   LET0(U,NIL);

SYMBOLIC PROCEDURE LET0(U,V);
   BEGIN
      FOR EACH X IN U DO LET2(CADR X,CADDR X,V,T);
      MCOND!* := FRASC!* := NIL
   END;

SYMBOLIC PROCEDURE LET2(U,V,W,B);
   BEGIN SCALAR FLG,X,Y,Z;
	%FLG is set true if free variables are found in following;
	X := SUBLA(FRASC!*,U);
	IF X NEQ U
	  THEN IF ATOM X THEN GO TO LER1   %an atom cannot be free;
	 	  ELSE <<FLG := T; U := X>>;
        X := SUBLA(FRASC!*,V);
	IF X NEQ V
	  THEN <<V := X;
		 IF EQCAR(V,'!*SQ!*) THEN V := PREPSQ!* CADR V>>;
		 %to ensure no kernels or powers are copied during 
		 %pattern matching process;
	%check for unmatched free variables;
	X := SMEMQL(FRLIS!*,MCOND!*);
	Y := SMEMQL(FRLIS!*,U);
	IF (Z := SETDIFF(X,Y))
	   OR (Z := SETDIFF(SETDIFF(SMEMQL(FRLIS!*,V),X),
		    SETDIFF(Y,X)))
	  THEN <<LPRIE ("Unmatched free variable(s)" . Z);
	         ERFG!* := 'HOLD;
		 RETURN NIL>>
	 ELSE IF EQCAR(U,'GETEL) THEN U := EVAL CADR U;
    A:	X := U;
	IF NUMBERP X THEN GO TO LER1
	 ELSE IF IDP X AND FLAGP(X,'RESERVED)
	  THEN REDERR LIST(X,"is a reserved identifier");
	Y := TYPL!*;
    B:	IF NULL Y THEN GO TO C
	 ELSE IF (Z := APPLY(CAR Y,LIST X)) OR APPLY(CAR Y,LIST V)
	  THEN RETURN APPLY(GET(CAR Y,'LETFN),
				LIST(X,V,GET(CAR Y,'NAME),B,Z));
	Y := CDR Y;
	GO TO B;
    C:	IF NOT ATOM X THEN GO TO NONATOM;
	IF B OR W THEN GO TO D;
	%We remove all conceivable properties when an atom is cleared;
	REMPROP(X,'AVALUE);
	REMPROP(X,'OPMTCH);
%	REMPROP(X,'KLIST);   %since the relevant objects may still
			     %exist;
	REMPROP(X,'MATRIX);
	IF ARRAYP X
	  THEN <<REMPROP(X,'ARRAY); REMPROP(X,'DIMENSION)>>;
	WTL!* := DELASC(X,WTL!*);
	RMSUBS(); %since all kernel lists are gone;
	RETURN;
    D:	X := SIMP0 X;
	IF NOT DENR X=1 OR DOMAINP (X := NUMR X) THEN GO TO LER1;
    D1: IF W OR FLG OR DOMAINP X OR RED X OR LC X NEQ 1 OR LDEG X NEQ 1
		OR EXPTP!*
	 THEN GO TO PRODCT;
	Y := MVAR X;
	IF ATOM Y THEN IF FLAGP(Y,'USED!*) THEN RMSUBS() ELSE NIL
	 ELSE IF 'USED!* MEMQ CDDR FKERN Y THEN RMSUBS();
	SETK1(Y,V,B);
	RETURN;
    NONATOM:	%replacement for non-atomic expression;
	IF NOT IDP CAR X THEN GO TO LER2
	 ELSE IF ARRAYP CAR X THEN GO TO ARR
	 ELSE IF CAR X EQ 'DF THEN GO TO DIFF
	 ELSE IF (Y := GET(CAR X,'MATRIX)) THEN RETURN LETMTR(U,V,Y)
	 ELSE IF NOT GET(CAR X,'SIMPFN) THEN GO TO LER3
	 ELSE GO TO D;
    PRODCT:	%replacement of powers and products;
	IF EXPTP!* THEN W:= T;
		%to allow for normal form for exponent expressions;
	EXPTP!* := NIL;
	RMSUBS();
	IF NULL FLG AND RED X
	  THEN RETURN SPLIS!* := XADD(LIST(X,W . T,V,NIL),
					SPLIS!*,U,B);
	Y := KERNLP X;
	IF Y=-1
	  THEN BEGIN X:= NEGF X; V:= LIST('MINUS,V) END
	 ELSE IF Y NEQ 1 THEN GO TO LER1;
	X := KLISTT X;
	Y := LIST(W . (IF MCOND!* THEN MCOND!* ELSE T),V,NIL);
	IF CDR X
	  THEN RETURN (!*MATCH := XADD!*(X . Y,!*MATCH,U,B))
	 ELSE IF NULL W AND ONEP CDAR X THEN GO TO P1;
	IF V=0 AND NULL W AND NOT FLG
	  THEN <<ASYMPLIS!* := XADD(CAR X,ASYMPLIS!*,U,B);
		 POWLIS!* := XADD(CAAR X . CDAR X . Y,POWLIS!*,U,NIL)>>
	 ELSE IF W OR NOT CDAR Y EQ T OR FRASC!*
	  THEN POWLIS1!* := XADD(CAR X . Y,POWLIS1!*,U,B)
	 ELSE IF NULL B AND (Z := ASSOC(CAAR X,ASYMPLIS!*)) AND Z=CAR X
	  THEN ASYMPLIS!* := DELASC(CAAR X,ASYMPLIS!*)
	 ELSE <<POWLIS!* := XADD(CAAR X . CDAR X . Y,POWLIS!*,U,B);
		ASYMPLIS!* := DELASC(CAAR X,ASYMPLIS!*)>>;
	RETURN;
    P1: X := CAAR X;
	IF ATOM X THEN GO TO LER1;
	RETURN PUT(CAR X,
		   'OPMTCH,
		   XADD!*(CDR X . Y,GET(CAR X,'OPMTCH),U,B));
    DIFF:	%rules for differentiation;
	IF NULL LETDF(U,V,W,X,B) THEN GO TO D ELSE RETURN;
    ARR:	%array replacements;
	SETELV(X,V);
	RETURN;
    LER1:EXPTP!* := NIL;
	RETURN ERRPRI1 U;
    LER2:RETURN ERRPRI2(U,'HOLD);
    LER3:REDMSG(CAR X,"operator");
	MKOP CAR X;
	GO TO A
   END;

SYMBOLIC PROCEDURE SIMP0 U;
   BEGIN SCALAR X;
	IF EQCAR(U,'!*SQ) THEN RETURN SIMP0 PREPSQ!* CADR U;
	X := SUBFG!* . !*SUB2;
	SUBFG!* := NIL;
	IF ATOM U OR CAR U MEMQ '(EXPT MINUS PLUS TIMES QUOTIENT)
	  THEN U := SIMP U
	 ELSE U := SIMPIDEN U;
	SUBFG!* := CAR X;
	!*SUB2 := CDR X;
	RETURN U
   END;

SYMBOLIC PROCEDURE MATCH U;
   LET0(U,T);

SYMBOLIC PROCEDURE CLEAR U;
   BEGIN
      RMSUBS();
      FOR EACH X IN U DO <<LET2(X,NIL,NIL,NIL); LET2(X,NIL,T,NIL)>>;
      MCOND!* := FRASC!* := NIL
   END;

SYMBOLIC PROCEDURE SETK(U,V);
   <<LET2(U,V,NIL,T); V>>;

   %U is a literal atom or a pseudo-kernel, V an expression
   %SETK associates value V with U and returns V;
%   IF ATOM U THEN SETK1(U,V,T)
%    ELSE IF ARRAYP CAR U
%     THEN <<SETELV(U,V); %V>>
%    ELSE !*A2K REVOP1 U;

SYMBOLIC PROCEDURE SETK1(U,V,B);
   BEGIN SCALAR X,Y;
	IF NOT ATOM U THEN GO TO C
	 ELSE IF NULL B THEN GO TO B1
	 ELSE IF (X := GET(U,'AVALUE)) THEN GO TO A;
	X := NIL . NIL;
	PUT(U,'AVALUE,X);
    A:	RPLACD(RPLACA(X,V),NIL);
	RETURN V;
    B1: IF NOT GET(U,'AVALUE) THEN MSGPRI(NIL,U,"not found",NIL,NIL)
	 ELSE REMPROP(U,'AVALUE);
	RETURN;
    C:  IF NOT ATOM CAR U
	  THEN REDERR "Invalid syntax: improper assignment"
	 ELSE IF NULL B THEN GO TO B2
	 ELSE IF NOT (Y := GET(CAR U,'KVALUE)) THEN GO TO E
	 ELSE IF X := ASSOC(U,Y) THEN GO TO D;
	X := NIL . NIL;
	ACONC(Y,U . X);
	GO TO A;
    D:	X := CDR X;
	GO TO A;
    E:	X := NIL . NIL;
	PUT(CAR U,'KVALUE,LIST(U . X));
	GO TO A;
    B2: IF NOT(Y := GET(CAR U,'KVALUE)) OR NOT (X := ASSOC(U,Y))
	  THEN MSGPRI(NIL,U,"not found",NIL,NIL)
	 ELSE PUT(CAR U,'KVALUE,DELETE(X,Y));
	RETURN;
   END;

SYMBOLIC PROCEDURE KLISTT U;
   IF ATOM U THEN NIL ELSE CAAR U . KLISTT CDR CARX(U,'LIST);

SYMBOLIC PROCEDURE KERNLP U;
   IF DOMAINP U THEN U ELSE IF NULL CDR U THEN KERNLP CDAR U ELSE NIL;

SYMBOLIC PROCEDURE RMSUBS;
   <<RMSUBS1(); RMSUBS2()>>;

SYMBOLIC PROCEDURE RMSUBS2;
   BEGIN
	RPLACA(!*SQVAR!*,NIL); !*SQVAR!* := LIST T;
%	WHILE KPROPS!* DO
%          <<REMPROP(CAR KPROPS!*,'KLIST); %KPROPS!* := CDR KPROPS!*>>;
%	EXLIST!* := LIST '(!*);
	%This is too dangerous: someone else may have constructed a
	%standard form;
	ALGLIST!* := NIL
   END;

SYMBOLIC PROCEDURE RMSUBS1;
   NIL;
%   BEGIN
%    A:	IF NULL SUBL!* THEN GO TO B;
%	RPLACD(CAR SUBL!*,NIL);
%	SUBL!* := CDR SUBL!*;
%	GO TO A;
%    B:	IF NULL DSUBL!* THEN RETURN;
%	RPLACA(CAR DSUBL!*,NIL);
%	DSUBL!* := CDR DSUBL!*;
%	GO TO B
%   END;

SYMBOLIC PROCEDURE XADD(U,V,W,B);
   %adds replacement U to table V, with new rule at head;
   BEGIN SCALAR X;
	X := ASSOC(CAR U,V);
	IF NULL X THEN GO TO C;
	V := DELETE(X,V);
	IF B THEN BEGIN RMSUBS1(); V := U . V END;
    A:	RETURN V;
    C:	IF B THEN V := U . V;
	GO TO A
   END;

SYMBOLIC PROCEDURE XADD!*(U,V,W,B);
   %adds replacement U to table V, with new rule at head;
   %also checks boolean part for equality;
   BEGIN SCALAR X;
      X := V;
      WHILE X AND NOT(CAR U=CAAR X AND CADR U=CADAR X) DO X := CDR X;
      IF X THEN <<V := DELETE(CAR X,V); IF B THEN RMSUBS1()>>;
      IF B THEN V := U . V;
      RETURN V
   END;

RLISTAT '(CLEAR LET MATCH);

FLAG ('(CLEAR LET MATCH),'QUOTE);


%*********************************************************************
%			 VARIOUS DECLARATIONS
%********************************************************************;

PUT('OPERATOR,'FORMFN,'FORMOPR);

SYMBOLIC PROCEDURE FORMOPR(U,VARS,MODE);
   IF MODE EQ 'SYMBOLIC
     THEN MKPROG(NIL,LIST LIST('FLAG,MKQUOTE U,MKQUOTE 'OPFN))
    ELSE LIST('OPERATOR,MKARG(U,VARS));

SYMBOLIC PROCEDURE OPERATOR U; FOR EACH J IN U DO MKOP J;

RLISTAT '(OPERATOR);

SYMBOLIC PROCEDURE DEN U;
   MK!*SQ (DENR SIMP!* U ./ 1);

SYMBOLIC PROCEDURE NUM U;
   MK!*SQ (NUMR SIMP!* U ./ 1);

FLAG ('(DEN NUM ABS MAX MIN),'OPFN);

FLAG('(DEN NUM),'NOVAL);

PUT('SAVEAS,'FORMFN,'FORMSAVEAS);

SYMBOLIC PROCEDURE FORMSAVEAS(U,VARS,MODE);
   LIST('SAVEAS,FORMCLEAR1(U,VARS,MODE));

SYMBOLIC PROCEDURE SAVEAS U;
   LET0(LIST LIST('EQUAL,CAR U,
	   IF FRASC!* AND EQCAR(WS,'!*SQ) THEN PREPSQ CADR WS ELSE WS),
	NIL);

RLISTAT '(SAVEAS);

SYMBOLIC PROCEDURE TERMS U; TERMSF NUMR SIMP!* U;

FLAG ('(TERMS),'OPFN);

FLAG('(TERMS),'NOVAL);

SYMBOLIC PROCEDURE TERMSF U;
   %U is a standard form.
   %Value is number of terms in U (excluding kernel structure);
   BEGIN INTEGER N;
	N := 0;
    A:	IF NULL U THEN RETURN N ELSE IF DOMAINP U THEN RETURN N+1;
	N := N + TERMSF LC U;
	U := RED U;
	GO TO A
   END;


%*********************************************************************
%*********************************************************************
%*********************************************************************

%			       SECTION 3

%		      SPECIFIC ALGEBRAIC PACKAGES

%*********************************************************************
%*********************************************************************
%********************************************************************;


%*********************************************************************
%All these packages except where noted are self-contained and any or
%all may be omitted as required;
%********************************************************************;


%*********************************************************************
%*********************************************************************
%			DIFFERENTIATION PACKAGE
%*********************************************************************
%********************************************************************;

% REQUIRES EXPRESSION DEPENDENCY MODULE;

SYMBOLIC PROCEDURE SIMPDF U;
   %U is a list of forms, the first an expression and the remainder
   %kernels and numbers.
   %Value is derivative of first form wrt rest of list;
   BEGIN SCALAR V,X,Y;
	IF NULL SUBFG!* THEN RETURN MKSQ('DF . U,1);
	V := CDR U;
	U := SIMP!* CAR U;
    A:	IF NULL V OR NULL NUMR U THEN RETURN U;
	X := IF NULL Y OR Y=0 THEN SIMP!* CAR V ELSE Y;
	IF NULL KERNP X THEN TYPERR(PREPSQ X,"kernel");
	X := CAAAAR X;
	V := CDR V;
	IF NULL V THEN GO TO C;
	Y := SIMP!* CAR V;
	IF NULL NUMR Y THEN <<V := CDR V; Y := NIL; GO TO A>>
	 ELSE IF NOT DENR Y=1 OR NOT NUMBERP NUMR Y THEN GO TO C;
	V := CDR V;
    B:  FOR I:=1:CAR Y DO U := DIFFSQ(U,X);
	Y := NIL;
	GO TO A;
    C:	U := DIFFSQ(U,X);
	GO TO A
   END;

PUT('DF,'SIMPFN,'SIMPDF);

SYMBOLIC PROCEDURE DIFFSQ(U,V);
   %U is a standard quotient, V a kernel.
   %Value is the standard quotient derivative of U wrt V.
   %Algorithm: df(x/y,z)= (x'-(x/y)*y')/y;
   MULTSQ(ADDSQ(DIFFF(NUMR U,V),NEGSQ MULTSQ(U,DIFFF(DENR U,V))),
	  1 ./ DENR U);

SYMBOLIC PROCEDURE DIFFF(U,V);
   %U is a standard form, V a kernel.
   %Value is the standard quotient derivative of U wrt V;
   IF DOMAINP U THEN NIL ./ 1
    ELSE ADDSQ(ADDSQ(MULTPQ(LPOW U,DIFFF(LC U,V)),
			MULTSQ(LC U ./ 1,DIFFP(LPOW U,V))),
	       DIFFF(RED U,V));

SYMBOLIC PROCEDURE DIFFP(U,V);
   %U is a standard power, V a kernel.
   %Value is the standard quotient derivative of U wrt V;
   BEGIN SCALAR W,X,Y,Z; INTEGER N;
	N := CDR U;	%integer power;
	U := CAR U;	%main variable;
	IF U EQ V AND (W := 1 ./ 1) THEN GO TO E
	 ELSE IF ATOM U THEN GO TO F
	 %ELSE IF (X := ASSOC(U,DSUBL!*)) AND (X := ATSOC(V,CDR X))
%		AND (W := CDR X) THEN GO TO E	%deriv known;
	     %DSUBL!* not used for now;
	 ELSE IF (NOT ATOM CAR U AND (W:= DIFFF(U,V)))
		  OR (CAR U EQ '!*SQ AND (W:= DIFFSQ(CADR U,V)))
	  THEN GO TO C	%extended kernel found;
	 ELSE IF (X:= GET!*(CAR U,'DFN)) THEN NIL
	 ELSE IF CAR U EQ 'PLUS AND (W:=DIFFSQ(SIMP U,V))
	  THEN GO TO C
	 ELSE GO TO H;	%unknown derivative;
	Y := X;
	Z := CDR U;
    A:	W := DIFFSQ(SIMP CAR Z,V) . W;
	IF CAAR W AND NULL CAR Y THEN GO TO H;	%unknown deriv;
	Y := CDR Y;
	Z := CDR Z;
	IF Z AND Y THEN GO TO A
	 ELSE IF Z OR Y THEN GO TO H;  %arguments do not match;
	Y := REVERSE W;
	Z := CDR U;
	W := NIL ./ 1;
    B:	%computation of kernel derivative;
	IF CAAR Y
	  THEN W := ADDSQ(MULTSQ(CAR Y,SIMP SUBLA(PAIR(CAAR X,Z),
						   CDAR X)),
			  W);
	X := CDR X;
	Y := CDR Y;
	IF Y THEN GO TO B;
    C:	%save calculated deriv in case it is used again;
	%IF X := ATSOC(U,DSUBL!*) THEN GO TO D
	%ELSE X := U . NIL;
	%DSUBL!* := X . DSUBL!*;
    D:	%RPLACD(X,XADD(V . W,CDR X,NIL,T));
    E:	%allowance for power;
	%first check to see if kernel has weight;
	IF (X := ATSOC(U,WTL!*))
	  THEN W := MULTPQ('K!* TO (-CDR X),W);
	RETURN IF N=1 THEN W ELSE MULTSQ(!*T2Q((U TO (N-1)) .* N),W);
    F:	%check for possible unused substitution rule;
	IF NOT DEPENDS(U,V)
	   AND (NOT (X:= ATSOC(U,POWLIS!*))
		 OR NOT CAR DIFFSQ(SIMP CADDDR X,V))
	  THEN RETURN NIL ./ 1;
	W := MKSQ(LIST('DF,U,V),1);
	GO TO E;
    H:	%final check for possible kernel deriv;
	IF CAR U EQ 'DF
	  THEN IF DEPENDS(CADR U,V)
		 THEN W := 'DF . CADR U . DERAD(V,CDDR U)
		ELSE RETURN NIL ./ 1
	 ELSE IF DEPENDS(U,V) THEN W := LIST('DF,U,V)
	 ELSE RETURN NIL ./ 1;
	W := IF X := OPMTCH W THEN SIMP X ELSE MKSQ(W,1);
	GO TO E
   END;

SYMBOLIC PROCEDURE DERAD(U,V);
   IF NULL V THEN LIST U
    ELSE IF NUMBERP CAR V THEN CAR V . DERAD(U,CDR V)
    ELSE  IF U=CAR V THEN IF CDR V AND NUMBERP CADR V
			   THEN U . (CADR V + 1) . CDDR V
			  ELSE U . 2 . CDR V
    ELSE IF ORDP(U,CAR V) THEN U . V
    ELSE CAR V . DERAD(U,CDR V);

SYMBOLIC PROCEDURE LETDF(U,V,W,X,B);
   BEGIN SCALAR Z;
	IF ATOM CADR X THEN GO TO E
	 ELSE IF NOT GETTYPE CAADR X EQ 'OPERATOR THEN GO TO LER3;
    A:	RMSUBS();
	IF NOT FRLP CDADR X
		OR NULL CDDR X
		OR CDDDR X
		OR NOT FRLP CDDR X
		OR NOT CADDR X MEMBER CDADR X
	 THEN GO TO E;
	Z := LPOS(CADDR X,CDADR X);
	IF NOT GET(CAADR X,'DFN)
	    THEN PUT(CAADR X,
		     'DFN,
		     NLIST(NIL,LENGTH CDADR X));
	W := GET(CAADR X,'DFN);
    B1: IF NULL W OR Z=0 THEN RETURN ERRPRI1 U
	 ELSE IF Z NEQ 1 THEN GO TO C
	 ELSE IF NULL B THEN GO TO D;
%        ELSE IF CAR W
%         THEN MSGPRI("Assignment for",X,"redefined",NIL,NIL);
	RETURN RPLACA(W,CDADR X . V);
    C:	W := CDR W;
	Z := Z-1;
	GO TO B1;
    D:  %IF NULL CAR W THEN MSGPRI(NIL,X,"not found",NIL,NIL);
	RETURN RPLACA(W,NIL);
    LER3:REDMSG(CAADR X,"operator");
	MKOP CAADR X;
	GO TO A;
   E:   %check for dependency;
	IF CADDR X MEMQ FRLIS!* THEN RETURN NIL
	 ELSE IF IDP CADR X AND NOT(CADR X MEMQ FRLIS!*) 
	   THEN DEPEND1(CADR X,CADDR X,T)
	 ELSE IF NOT ATOM CADR X AND IDP CAADR X AND FRLP CDADR X
	  THEN DEPEND1(CAADR X,CADDR X,T);
	RETURN NIL
   END;

SYMBOLIC PROCEDURE FRLP U;
   NULL U OR (CAR U MEMQ FRLIS!* AND FRLP CDR U);

SYMBOLIC PROCEDURE LPOS(U,V);
   IF U EQ CAR V THEN 1 ELSE LPOS(U,CDR V)+1;


END;
