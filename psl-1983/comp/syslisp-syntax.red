%
% SYSLISP-SYNTAX.RED - SMacros and redefinition of arithmetic operators
%                      and other syslisp syntax
%  
% Author:      Eric Benson and M. L. griss
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        11 July 1981
% Copyright (c) 1981 University of Utah
%

fluid '(!*SYSLISP);

%  <PSL.COMP>SYSLISP-SYNTAX.RED.3,  5-May-82 11:33:48, Edit by BENSON
%  Wrapped if GetD 'BEGIN1 around parser calls

% New WDECLARE constructs

% Modify ***** [] vector syntax for PREFIX and INFIX forms
% At lower prec

SYMBOLIC PROCEDURE ParseLVEC(VNAME,VEXPR);
 IF OP EQ '!*RVEC!* THEN <<OP :=SCAN(); LIST('INDX,VNAME,VEXPR)>>
  ELSE  PARERR("Missing ] in index expression ");

% Use normal parsing, then CLEAN

SYMBOLIC PROCEDURE ParseWDEC0(FN,DMODES,DLIST);
 BEGIN SCALAR PLIST;
	IF EQCAR(DLIST,'!*COMMA!*) THEN DLIST:=REVERSE CDR DLIST
         ELSE DLIST:=LIST DLIST;
	PLIST:=FOR EACH DEC IN DLIST COLLECT ParseWDEC1(FN,DEC);
	RETURN ('WDECLARE . DMODES . FN . REVERSE PLIST);
 END;

SYMBOLIC PROCEDURE ParseWDEC1(FN,DEC);
% Process each WDEC to check legal modes
    if EqCar(DEC,'EQUAL) THEN
	AConc(ParseWDEC2(FN,CADR DEC), ParseWDEC3(FN,CADDR DEC))
    ELSE AConc(ParseWDEC2(FN,DEC), NIL);
	
SYMBOLIC PROCEDURE ParseWDEC2(FN,X);
% Remove INDXs from LHS of =
  IF IDP X THEN list(X, NIL)
   ELSE IF EQCAR(X,'INDX) THEN  LIST(CADR X,CADDR X)
   ELSE PARERR "Only [] allowed on LHS of WDECLARATION";

SYMBOLIC PROCEDURE ParseWDEC3(FN,X);
% Remove INDX's from RHS of =
  IF IDP X THEN X
   ELSE IF EQCAR(X,'INDX) 
     THEN (IF CADR X EQ '!*PREFIXVECT!*
		 THEN REMCOM(CADDR X)
            ELSE PARERR("Only [...] is legal INIT in WDECLARE"))
   ELSE X;

if not FUnBoundP 'BEGIN1 then <<	% kludge #+Rlisp
DEFINEBOP('!*LVEC!*,121,5,ParseLVEC);
DEFINEROP('!*LVEC!*,5,ParseLVEC('!*PREFIXVECT!*,X));

DEFINEBOP('!*RVEC!*,4,5);

DEFINEROP('WCONST,1,ParseWDEC0('WCONST,'DEFAULT,X));
DEFINEROP('WVAR,1,ParseWDEC0('WVAR,'DEFAULT,X));
DEFINEROP('WARRAY,1,ParseWDEC0('WARRAY,'DEFAULT,X));
DEFINEROP('WSTRING,1,ParseWDEC0('WSTRING,'DEFAULT,X));

DEFINEBOP('WCONST,1,1,ParseWDEC0('WCONST,X,Y));
DEFINEBOP('WVAR,1,1,ParseWDEC0('WVAR,X,Y));
DEFINEBOP('WARRAY,1,1,ParseWDEC0('WARRAY,X,Y));
DEFINEBOP('WSTRING,1,1,ParseWDEC0('WSTRING,X,Y));

% Operators @ for GetMem, & for Loc

put('!@, 'NewNam, 'GetMem);
put('!&, 'NewNam, 'Loc);

>>;

% SysName hooks for REFORM

REMFLAG('(REFORM),'LOSE);

SYMBOLIC PROCEDURE REFORM U;
  IF ATOM U OR CAR U MEMQ '(QUOTE WCONST)
	 THEN U
   ELSE IF CAR U EQ 'COND THEN 'COND . REFORM CDR U
   ELSE IF CAR U EQ 'PROG
    THEN PROGN(RPLCDX(CDR U,REFORMLIS CDDR U),U)
    ELSE IF CAR U EQ 'LAMBDA
     THEN PROGN(RPLACA(CDDR U,REFORM CADDR U),U)
    ELSE IF CAR U EQ 'FUNCTION AND ATOM CADR U
     THEN BEGIN SCALAR X;
	IF NULL !*CREF AND (X:= GET(CADR U,'SMACRO))
	  THEN RETURN LIST('FUNCTION,X)
	 ELSE IF  GET(CADR U,'NMACRO) OR MACROP CADR U
	  THEN REDERR "MACRO USED AS FUNCTION"
	 ELSE RETURN U END
%    ELSE IF CAR U EQ 'MAT THEN RPLCDX(U,MAPC2(CDR U,FUNCTION REFORM))
    ELSE IF ATOM CAR U
     THEN BEGIN SCALAR X,Y,FN;
	FN := CAR U;
	 IF (Y := GETD FN) AND CAR Y EQ 'MACRO
		AND EXPANDQ FN
	  THEN RETURN REFORM APPLY(CDR Y,LIST U);
	X := REFORMLIS CDR U;
	IF NULL IDP FN THEN RETURN(FN . X);
        IF !*SYSLISP AND (Y:=GET(FN,'SYSNAME)) THEN <<FN:=Y;U:=FN.CDR U>>;
	IF (NULL !*CREF OR EXPANDQ FN)
		 AND (Y:= GET(FN,'NMACRO))
	  THEN RETURN
		APPLY(Y,IF FLAGP(FN,'NOSPREAD) THEN LIST X ELSE X)
	 ELSE IF (NULL !*CREF OR EXPANDQ FN)
		   AND (Y:= GET(FN,'SMACRO))
	  THEN RETURN SUBLIS(PAIR(CADR Y,X),CADDR Y)
	   %we could use an atom SUBLIS here (eg, SUBLA);
	 ELSE RETURN PROGN(RPLCDX(U,X),U)
      END
    ELSE REFORM CAR U . REFORMLIS CDR U;

RemFlag('(Plus Times), 'NARY)$

DefList('((Plus WPlus2)
	  (Plus2 WPlus2)
	  (Minus WMinus)
	  (Difference WDifference)
	  (Times WTimes2)
	  (Times2 WTimes2)
	  (Quotient WQuotient)
	  (Remainder WRemainder)
	  (Mod WRemainder)
	  (Land WAnd)
	  (Lor WOr)
	  (Lxor WXor)
	  (Lnot WNot)
	  (LShift WShift)
	  (LSH WShift)), 'SysName);

DefList('((Neq WNeq)
	  (Equal WEq)	 
	  (Eqn WEq)
	  (Eq WEq)
	  (Greaterp WGreaterp)
	  (Lessp WLessp)
	  (Geq WGeq)
	  (Leq WLeq)
	  (Getv WGetv)
	  (Indx WGetv)
	  (Putv WPutv)
	  (SetIndx WPutv)), 'SysName);


% modification to arithmetic FOR loop for SysLisp

LISP PROCEDURE MKSYSFOR U;
   BEGIN SCALAR ACTION,BODY,EXP,INCR,LAB1,LAB2,RESULT,TAIL,VAR,X;
      VAR := second second U;
      INCR := cddr second U;
      if FixP third Incr or WConstEvaluable third Incr then return
	ConstantIncrementFor U;
      ACTION := first third U;
      BODY := second third U;
      RESULT := LIST LIST('SETQ,VAR,CAR INCR);
      INCR := CDR INCR;
      X := LIST('WDIFFERENCE,first INCR,VAR);
      IF second INCR NEQ 1 THEN X := LIST('WTIMES2,second INCR,X);
      IF NOT ACTION EQ 'DO THEN
	REDERR "Only do expected in SysLisp FOR";
      LAB1 := GENSYM();
      LAB2 := GENSYM();
      RESULT := NCONC(RESULT,
		 LAB1 .
		LIST('COND,LIST(LIST('WLESSP,X,0),LIST('GO,LAB2))) .
		BODY .
		LIST('SETQ,VAR,LIST('WPLUS2,VAR,second INCR)) .
		LIST('GO,LAB1) .
		LAB2 .
		TAIL);
      RETURN MKPROG(VAR . EXP,RESULT)
   END;

LISP PROCEDURE ConstantIncrementFor U;
   BEGIN SCALAR ACTION,BODY,EXP,INCR,LAB1,RESULT,VAR,X,
	StepValue, Limit;
      VAR := second second U;
      INCR := cddr second U;
      ACTION := first third U;
      BODY := second third U;
      RESULT := LIST LIST('SETQ,VAR,CAR INCR);
      INCR := CDR INCR;
      StepValue := if FixP second Incr then second Incr
		   else WConstEvaluable second Incr;
      Limit := first Incr;
      IF NOT ACTION EQ 'DO THEN
	REDERR "Only do expected in SysLisp FOR";
      LAB1 := GENSYM();
      RESULT := NCONC(RESULT,
		 LAB1 .
		LIST('COND,LIST(LIST(if MinusP StepValue then 'WLessP
							 else 'WGreaterP,
				     Var,
				     Limit),'(return 0))) .
		BODY .
		LIST('SETQ,VAR,LIST('WPLUS2,VAR,StepValue)) .
		LIST('GO,LAB1) .
		NIL);
      RETURN MKPROG(VAR . EXP,RESULT)
   END;

LISP PROCEDURE MKFOR1 U;
 IF !*SYSLISP THEN MKSYSFOR U ELSE MKLISPFOR U;

PUTD('MKLISPFOR,'EXPR,CDR GETD 'FOR);	% grab old FOR definition

macro procedure For U; MkFor1 U;	% redefine FOR

END;
