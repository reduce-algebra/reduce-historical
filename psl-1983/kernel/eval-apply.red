%
% EVAL-APPLY.RED - Function calling mechanism
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        20 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL>EVAL-APPLY.RED.2, 20-Sep-82 10:36:28, Edit by BENSON
%  CAR of a form is never evaluated
%  <PSL.INTERP>EVAL-APPLY.RED.5,  6-Jan-82 19:22:46, Edit by GRISS
%  Add NEXPR

% FUnBoundP and other function cell primitives found in FUNCTION-PRIMITIVES
% Eval and Apply could have been defined using only GetD rather than these
% primitves.  They are used instead to avoid the CONS in GETD.

% ValueCell is found in SYMBOL-VALUES.RED

% IDApply, CodeApply, IDEvalApply and CodeEvalApply are written in LAP
% due to register usage and to make them faster.  They are found in
% APPLY-LAP.RED.  IDApply1 is handled by the compiler

% uses EvProgN, found in EASY-SL.RED, expr for PROGN

% Error numbers:
% 1000 - undefined function
% 1100 - ill-formed function expression
% 1200 - argument number mismatch
% 1300 - unknown function type
% +3 in LambdaEvalApply
% +4 in LambdaApply
% +2 in Apply
% +1 in Eval

CompileTime flag('(LambdaEvalApply LambdaApply), 'InternalFunction);

on SysLisp;

% the only reason these 2 are in Syslisp is to speed up arithmetic (N := N + 1)

syslsp procedure LambdaEvalApply(Fn, Args); %. Fn is Lambda, Args to be Evaled
    if not (PairP Fn and car Fn = 'LAMBDA) then
	ContinuableError('1103,
			 '"Ill-formed function expression",
			 Fn . Args)
    else begin scalar N, Result;
	N := BindEval(cadr Fn, Args); % hand-coded, bind formals to evlis args
	if N = -1 then return
	    ContinuableError('1203,
			     '"Argument number mismatch",
			     Fn . Args);
	Result := EvProgN cddr Fn;
	if N neq 0 then UnBindN N;
	return Result;
    end;

syslsp procedure LambdaApply(Fn, Args);	%. Fn is Lambda, unevaled Args
    if not (PairP Fn and car Fn = 'LAMBDA) then
	ContinuableError('1104,
			 '"Ill-formed function expression",
			 Fn . for each X in Args collect MkQuote X)
    else begin scalar Formals, N, Result;
	Formals := cadr Fn;
	N := 0;
	while PairP Formals and PairP Args do
	<<  LBind1(car Formals, car Args);
	    Formals := cdr Formals;
	    Args := cdr Args;
	    N := N + 1 >>;
	if PairP Formals or PairP Args then return
	    ContinuableError('1204,
			     '"Argument number mismatch",
			     Fn . for each X in Args collect MkQuote X);
	Result := EvProgN cddr Fn;
	if N neq 0 then UnBindN N;
	return Result;
    end;

off SysLisp;

% Apply differs from the Standard Lisp Report in that functions other
% than EXPRs are allowed to be applied, the effect being the same as
% Apply(cdr GetD Fn, Args)

lisp procedure Apply(Fn, Args);		%. Indirect function call
    if IDP Fn then begin scalar StackMarkForBacktrace, Result;
	if FUnBoundP Fn then return
	    ContinuableError(1002,
			     BldMsg("%r is an undefined function", Fn),
			     Fn . for each X in Args collect MkQuote X);
	StackMarkForBacktrace := MkBTR Inf Fn;
	Result := if FCodeP Fn then CodeApply(GetFCodePointer Fn, Args)
		else LambdaApply(get(Fn, '!*LambdaLink), Args);
	return Result;
    end
    else if CodeP Fn then CodeApply(Fn, Args)
    else if PairP Fn and car Fn = 'LAMBDA then
	LambdaApply(Fn, Args)
    else
	ContinuableError(1102,
			 "Ill-formed function expression",
			 Fn . for each X in Args collect MkQuote X);

lisp procedure Eval U;			%. Interpret S-Expression as program
    if not PairP U then
	if not IDP U then U else ValueCell U
    else begin scalar Fn;
	Fn := car U;
	return if IDP Fn then
	    if FUnBoundP Fn then
		ContinuableError(1300,
				 BldMsg("%r is an undefined function", Fn),
				 U)
	    else begin scalar FnType, StackMarkForBacktrace, Result;
		FnType := GetFnType Fn;
		StackMarkForBacktrace := MkBTR Inf Fn;
		Result := if null FnType then	 % must be an EXPR
			      if FCodeP Fn then
				  CodeEvalApply(GetFCodePointer Fn, cdr U)
			      else LambdaEvalApply(get(Fn, '!*LambdaLink),
						   cdr U)
			   else if FnType = 'FEXPR then
			       IDApply1(cdr U, Fn)
			   else if FnType = 'NEXPR then
			       IDApply1(EvLis cdr U, Fn)
			   else if FnType = 'MACRO then
			       Eval IDApply1(U, Fn)
			   else
			       ContinuableError(1301,
			                    BldMsg("Unknown function type %r",
								      FnType),
						U);
	    return Result;
	end
	else if CodeP Fn then CodeEvalApply(Fn, cdr U)
	else if PairP Fn and car Fn = 'LAMBDA then
	    LambdaEvalApply(Fn, cdr U)
	else ContinuableError(1302,
			      BldMsg("Ill-formed expression in Eval %r", U),
			      U);
    end;

END;
