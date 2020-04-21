%
% LISP-MACROS.RED - Various macros to make pure Lisp more tolerable
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        5 October 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.INTERP>LISP-MACROS.RED.4, 22-Jul-82 10:51:11, Edit by BENSON
%  Added CASE, removed IF
% still to come: Do, Let
%  <PSL.INTERP>LISP-MACROS.RED.5, 28-Dec-81 14:43:39, Edit by BENSON
%  Added SetF

CompileTime flag('(InThisCase), 'InternalFunction);

% Not a macro, but it belongs with these

SYMBOLIC FEXPR PROCEDURE CASE U;
%U is of form (CASE <integer exp> (<case-1> <exp-1>) . . .(<case-n> <exp-n>)).
% If <case-i> is NIL it is default,
%   else is list of INT or (RANGE int int)
 BEGIN SCALAR CaseExpr,DEF,CaseLst,BOD;
	CaseExpr:=EVAL CAR U;
  L:	IF NOT PAIRP(U:=CDR U) THEN RETURN EVAL DEF;
	CaseLst:=CAAR U; BOD:=CADAR U;
	IF NOT PAIRP CaseLst
	    OR CAR CaseLst MEMQ '(OTHERWISE DEFAULT) THEN
	  <<DEF:=BOD; GOTO L>>;
	IF InThisCase(CaseExpr,CaseLst) THEN RETURN EVAL BOD;
	GOTO L
  END;

SYMBOLIC PROCEDURE InThisCase(CaseExpr,Cases);
 IF NOT PAIRP Cases Then NIL
  ELSE IF PAIRP Car Cases and Caar Cases EQ 'RANGE
   and CaseExpr>=Cadar Cases and CaseExpr<=Caddar Cases then T
  ELSE IF CaseExpr = Car Cases then T
  ELSE InThisCase(CaseExpr,Cdr Cases);


macro procedure SetF U;			%. General assignment macro
    ExpandSetF(cadr U, caddr U);

lisp procedure ExpandSetF(LHS, RHS);
begin scalar LHSOp;
    return if atom LHS then list('setq, LHS, RHS)
    else if (LHSOp := get(car LHS, 'Assign!-Op)) then
	LHSOp . Append(cdr LHS, list RHS)	% simple substitution case
    else if (LHSOp := get(car LHS, 'SetF!-Expand)) then
	Apply(LHSOp, list(LHS, RHS))		% more complex transformation
    else if (LHSOp := GetD car LHS) and car LHSOp = 'MACRO then
	ExpandSetF(Apply(cdr LHSOp, list LHS), RHS)
    else StdError BldMsg("%r is not a known form for assignment",
			 list('SetF, LHS, RHS));
end;

LoadTime DefList('((GetV PutV)
		   (car RplacA)
		   (cdr RplacD)
		   (Indx SetIndx)
		   (Sub SetSub)
		   (Nth (lambda (L I X) (rplaca (PNTH L I) X) X))
		   (Eval Set)
		   (Value Set)), 'Assign!-Op);

END;
