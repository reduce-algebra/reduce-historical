%
% PUTD-GETD.RED - Standard Lisp function defining functions
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        18 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL>PUTD-GETD.RED.3, 13-Jan-83 19:09:47, Edit by PERDUE
%  Removed obsolete code from PUTD in response to Bobbie Othmer's bug report
%  <PSL.KERNEL>PUTD-GETD.RED.2, 24-Sep-82 15:01:38, Edit by BENSON
%  Added CODE-NUMBER-OF-ARGUMENTS
%  <PSL.INTERP>PUTD-GETD.RED.3, 19-Apr-82 13:10:57, Edit by BENSON
%  Function in PutD may be an ID
%  <PSL.INTERP>PUTD-GETD.RED.4,  6-Jan-82 19:18:47, Edit by GRISS
% Add NEXPR
% DE, DF and DM are defined in EASY-SL.RED

% If the function is interpreted, the lambda form will be found by
%	GET(ID, '!*LambdaLink).

% If the type of a function is other than EXPR (i.e. FEXPR or MACRO or NEXPR),
% this will be indicated by GET(ID, 'TYPE) = 'FEXPR or 'MACRO or 'NEXPR


% PutD makes use of the fact that FLUID and GLOBAL declarations use the
% property list indicator TYPE

% Non-Standard Lisp functions used:
% function cell primitives FUnBoundP, etc. found in FUNCTION-PRIMITVES.RED
% CompD --	in COMPILER.RED
% ErrorPrintF, VerboseTypeError, BldMsg

% Error numbers:
% 1100 - ill-formed function expression
% 1300 - unknown function type
% +5 in GetD

lisp procedure GetD U;			%. Lookup function definition of U
    IDP U and not FUnBoundP U and ((get(U, 'TYPE) or 'EXPR) .
	(if FLambdaLinkP U then get(U, '!*LambdaLink) else GetFCodePointer U));

lisp procedure RemD U;			%. Remove function definition of U
begin scalar OldGetD;
    if (OldGetD := GetD U) then
    <<  MakeFUnBound U;
	RemProp(U, 'TYPE);
	RemProp(U, '!*LambdaLink) >>;
    return OldGetD;
end;

fluid '(!*RedefMSG			% controls printing of redefined
	!*UserMode);			% controls query for redefinition
LoadTime
<<  !*UserMode := NIL;			% start in system mode
    !*RedefMSG := T >>;			% message in PutD

fluid '(!*Comp				% controls automatic compilation
	PromptString!*);

lisp procedure PutD(FnName, FnType, FnExp);	%. Install function definition
%
% this differs from the SL Report in 2 ways:
% - function names flagged LOSE are not defined.
% - 	"      "   which are already fluid or global are defined anyway,
% with a warning.
%
    if not IDP FnName then
	NonIDError(FnName, 'PutD)
    else if not (FnType memq '(EXPR FEXPR MACRO NEXPR)) then
	ContError(1305,
		  "%r is not a legal function type",
		  FnType,
		  PutD(FnName, FnType, FnExp))
    else if FlagP(FnName, 'LOSE) then
    <<  ErrorPrintF("*** %r has not been defined, because it is flagged LOSE",
		    FnName);
	NIL >>
    else begin scalar VarType, PrintRedefinedMessage, OldIN, PromptString!*,
			QueryResponse;
	if not FUnBoundP FnName then
	<<  if !*RedefMSG then PrintRedefinedMessage := T;
	    if !*UserMode and not FlagP(FnName, 'USER) then
		if not YesP BldMsg(
		"Do you really want to redefine the system function %r?",
								   FnName)
		then return NIL
		else Flag1(FnName, 'USER) >>;
	if CodeP FnExp then
	<<  MakeFCode(FnName, FnExp);
	    RemProp(FnName, '!*LambdaLink) >>
	else if IDP FnExp and not FUnBoundP FnExp then return
	    PutD(FnName, FnType, cdr GetD FnExp)
	else if !*Comp then
	    return CompD(FnName, FnType, FnExp)
	else if EqCar(FnExp, 'LAMBDA) then
	<<  put(FnName, '!*LambdaLink, FnExp);
	    MakeFLambdaLink FnName >>
	else return ContError(1105,
			      "Ill-formed function expression in PutD",
			      PutD(FnName, FnType, FnExp));
	if FnType neq 'EXPR then put(FnName, 'TYPE, FnType)
	    else RemProp(FnName, 'TYPE);
	if !*UserMode then Flag1(FnName, 'USER) else RemFlag1(FnName, 'USER);
	if PrintRedefinedMessage then
	    ErrorPrintF("*** Function %r has been redefined", FnName);
	return FnName;
    end;

on Syslisp;

syslsp procedure code!-number!-of!-arguments cp;
begin scalar n;
    return if codep cp then 
    <<  n := !%code!-number!-of!-arguments CodeInf cp;
	if n >= 0 and n <= MaxArgs then n >>;
end;

END;
