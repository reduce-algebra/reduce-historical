%
% P-APPLY-LAP.RED - Inefficient, portable version of APPLY-LAP
% 
% Author:      Eric Benson and M. L. Griss
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        29 July 1982
% Copyright (c) 1982 University of Utah
%
% Modifications by M.L. Griss 25 October, 1982.
% Added J. MacDonalds Mods of 29 January (for IBM, non neg stack index)
% 	In CODEEVALAPLY
% Functions which must be written non-portably, 
%   "portable" versions defined in PT:TEST-FUNCTION-PRIMITIVES.RED

% CodePrimitive
%	Takes the code pointer stored in the fluid variable CodePtr!*
%	and jumps to its address, without distubing any of the argument
%	registers.  This can be flagged 'InternalFunction for compilation
%	before this file is compiled or done as an 'OpenCode and 'ExitOpenCode
%	property for the compiler.
% CompiledCallingInterpreted
%	Called by some convention from the function cell of an ID which
%	has an interpreted function definition.  It should store the ID
%	in the fluid variable CodeForm!* without disturbing the argument
%	registers, then finish with
%	(!*JCALL CompiledCallingInterpretedAux)
%	(CompiledCallingInterpretedAux may be flagged 'InternalFunction).
% FastApply
%	Called with a functional form in (reg t1) and argument registers
%	loaded.  If it is a code pointer or an ID, the function address
%	associated with either should be jumped to.  If it is anything else
%	except a lambda form, an error should be signaled.  If it is a lambda
%	form, store (reg t1) in the fluid variable CodeForm!* and
%	(!*JCALL FastLambdaApply)
%	(FastLambdaApply may be flagged 'InternalFunction).
% UndefinedFunction
%	Called by some convention from the function cell of an ID (probably
%	the same as CompiledCallingInterpreted) for an undefined function.
%	Should call Error with the ID as part of the error message.

Compiletime <<

fluid '(CodePtr!*		% gets code pointer used by CodePrimitive
	CodeForm!*		% gets fn to be called from code
);
>>;

on Syslisp;

external WArray CodeArgs;

syslsp procedure CodeApply(CodePtr, ArgList);
begin scalar I;
    I := 0;
    LispVar CodePtr!* := CodePtr;
    while PairP ArgList and ILessP(I, 15) do
    <<  WPutV(CodeArgs , I, first ArgList);
	I := IAdd1 I;
	ArgList := rest ArgList >>;
    if IGEQ(I, 15) 
	then return StdError List("Too many arguments to function",I,CodePtr);
    return case I of
    0:	CodePrimitive();
    1:	CodePrimitive WGetV(CodeArgs, 0);
    2:	CodePrimitive(WGetV(CodeArgs, 0), 	WGetV(CodeArgs, 1));
    3:	CodePrimitive(WGetV(CodeArgs, 0), 	WgetV(CodeArgs, 1),
      	        WGetV(CodeArgs, 2));
    4:	CodePrimitive(WGetV(CodeArgs, 0), 	WgetV(CodeArgs, 1),
	        WGetV(CodeArgs, 2), 		WgetV(CodeArgs, 3));
    5:	CodePrimitive(WGetV(CodeArgs, 0), 	WgetV(CodeArgs, 1),
	        WGetV(CodeArgs, 2),	 	WgetV(CodeArgs, 3),
	        WGetV(CodeArgs, 4));
    6:	CodePrimitive(WGetV(CodeArgs, 0), 	WgetV(CodeArgs, 1),
	        WGetV(CodeArgs, 2), 		WgetV(CodeArgs, 3),
	        WGetV(CodeArgs, 4), 		WgetV(CodeArgs, 5));
    7:	CodePrimitive(WGetV(CodeArgs, 0), 	WgetV(CodeArgs, 1),
		WgetV(CodeArgs, 2),		WgetV(CodeArgs, 3),
		WgetV(CodeArgs, 4),		WgetV(CodeArgs, 5),
		WgetV(CodeArgs, 6));
    8:	CodePrimitive(WgetV(CodeArgs, 0),	WgetV(CodeArgs, 1),
		WgetV(CodeArgs, 2),		WgetV(CodeArgs, 3),
		WgetV(CodeArgs, 4),		WgetV(CodeArgs, 5),
		WgetV(CodeArgs, 6),		WgetV(CodeArgs, 7));
    9:	CodePrimitive(WgetV(CodeArgs, 0),	WgetV(CodeArgs, 1),
		WgetV(CodeArgs, 2),		WgetV(CodeArgs, 3),
		WgetV(CodeArgs, 4),		WgetV(CodeArgs, 5),
		WgetV(CodeArgs, 6),		WgetV(CodeArgs, 7),
		WgetV(CodeArgs, 8));
    10:	CodePrimitive(WgetV(CodeArgs, 0),	WgetV(CodeArgs, 1),
		WgetV(CodeArgs, 2),		WgetV(CodeArgs, 3),
		WgetV(CodeArgs, 4),		WgetV(CodeArgs, 5),
		WgetV(CodeArgs, 6),		WgetV(CodeArgs, 7),
		WgetV(CodeArgs, 8),		WgetV(CodeArgs, 9));
    11:	CodePrimitive(WgetV(CodeArgs, 0),	WgetV(CodeArgs, 1),
		WgetV(CodeArgs, 2),		WgetV(CodeArgs, 3),
		WgetV(CodeArgs, 4),		WgetV(CodeArgs, 5),
		WgetV(CodeArgs, 6),		WgetV(CodeArgs, 7),
		WgetV(CodeArgs, 8),		WgetV(CodeArgs, 9),
		WgetV(CodeArgs, 10));
    12:	CodePrimitive(WgetV(CodeArgs, 0),	WgetV(CodeArgs, 1),
		WgetV(CodeArgs, 2),		WgetV(CodeArgs, 3),
		WgetV(CodeArgs, 4),		WgetV(CodeArgs, 5),
		WgetV(CodeArgs, 6),		WgetV(CodeArgs, 7),
		WgetV(CodeArgs, 8),		WgetV(CodeArgs, 9),
		WgetV(CodeArgs, 10),		WgetV(CodeArgs, 11));
    13:	CodePrimitive(WgetV(CodeArgs, 0),	WgetV(CodeArgs, 1),
		WgetV(CodeArgs, 2),		WgetV(CodeArgs, 3),
		WgetV(CodeArgs, 4),		WgetV(CodeArgs, 5),
		WgetV(CodeArgs, 6),		WgetV(CodeArgs, 7),
		WgetV(CodeArgs, 8),		WgetV(CodeArgs, 9),
		WgetV(CodeArgs, 10),		WgetV(CodeArgs, 11),
		WgetV(CodeArgs, 12));
    14:	CodePrimitive(WgetV(CodeArgs, 0),	WgetV(CodeArgs, 1),
		WgetV(CodeArgs, 2),		WgetV(CodeArgs, 3),
		WgetV(CodeArgs, 4),		WgetV(CodeArgs, 5),
		WgetV(CodeArgs, 6),		WgetV(CodeArgs, 7),
		WgetV(CodeArgs, 8),		WgetV(CodeArgs, 9),
		WgetV(CodeArgs, 10),		WgetV(CodeArgs, 11),
		WgetV(CodeArgs, 12),		WgetV(CodeArgs, 13));
    15:	CodePrimitive(WgetV(CodeArgs, 0),	WgetV(CodeArgs, 1),
		WgetV(CodeArgs, 2),		WgetV(CodeArgs, 3),
		WgetV(CodeArgs, 4),		WgetV(CodeArgs, 5),
		WgetV(CodeArgs, 6),		WgetV(CodeArgs, 7),
		WgetV(CodeArgs, 8),		WgetV(CodeArgs, 9),
		WgetV(CodeArgs, 10),		WgetV(CodeArgs, 11),
		WgetV(CodeArgs, 12),		WgetV(CodeArgs, 13),
		WgetV(CodeArgs, 14));
    end;
end;

%lisp procedure CodeEvalApply(CodePtr, ArgList);
%    CodeApply(CodePtr, EvLis ArgList);

lap '((!*entry CodeEvalApply expr 2)
	(!*ALLOC 15)
	(!*LOC (reg 3) (frame 15))  %/jim really wrong/
% 	(!*LOC (reg 3) (frame 1))   %/jim: for non-neg stack indices on IBM/
		                    % But must be base of a block of ascending
				    % addresses, check cmacros
	(!*CALL CodeEvalApplyAux)
	(!*EXIT 15)
);

syslsp procedure CodeEvalApplyAux(CodePtr, ArgList, P);
begin scalar N;
    N := 0;
    while PairP ArgList and ILessP(N, 15) do
 %/    <<  WPutV(P, ITimes2(StackDirection, N), Eval first ArgList); %/jim/
     <<  WPutV(P, N, Eval first ArgList);                            %/jim/
	ArgList := rest ArgList;
	N := IAdd1 N >>;
    if IGEQ(N, 15) 
	then return StdError list("Too many arguments to function",N,CodePtr);
    LispVar CodePtr!* := CodePtr;
    return case N of
    0:	CodePrimitive();
    1:	CodePrimitive(WgetV(P, 0));
    2:	CodePrimitive(WgetV(P, 0),	WgetV(P, 1));
    3:	CodePrimitive(WgetV(P, 0),	WgetV(P, 1),	WgetV(P, 2));
    4:	CodePrimitive(WgetV(P, 0),	WgetV(P, 1),	WgetV(P, 2),
		WgetV(P, 3));
    5:	CodePrimitive(WgetV(P, 0),	WgetV(P, 1),	WgetV(P, 2),
		WgetV(P, 3),		WgetV(P, 4));
    6:	CodePrimitive(WgetV(P, 0),	WgetV(P, 1),	WgetV(P, 2),
		WgetV(P, 3),		WgetV(P, 4),	WgetV(P, 5));
    7:	CodePrimitive(WgetV(P, 0),	WgetV(P, 1),	WgetV(P, 2),
		WgetV(P, 3),		WgetV(P, 4),	WgetV(P, 5),
		WgetV(P, 6));
    8:	CodePrimitive(WgetV(P, 0),	WgetV(P, 1),	WgetV(P, 2),
		WgetV(P, 3),		WgetV(P, 4),	WgetV(P, 5),
		WgetV(P, 6),		WgetV(P, 7));
    9:	CodePrimitive(WgetV(P, 0),	WgetV(P, 1),	WgetV(P, 2),
		WgetV(P, 3),		WgetV(P, 4),	WgetV(P, 5),
		WgetV(P, 6),		WgetV(P, 7),	WgetV(P, 8));
    10:	CodePrimitive(WgetV(P, 0),	WgetV(P, 1),	WgetV(P, 2),
		WgetV(P, 3),		WgetV(P, 4),	WgetV(P, 5),
		WgetV(P, 6),		WgetV(P, 7),	WgetV(P, 8),
		WgetV(P, 9));
    11:	CodePrimitive(WgetV(P, 0),	WgetV(P, 1),	WgetV(P, 2),
		WgetV(P, 3),		WgetV(P, 4),	WgetV(P, 5),
		WgetV(P, 6),		WgetV(P, 7),	WgetV(P, 8),
		WgetV(P, 9),		WgetV(P, 10));
    12:	CodePrimitive(WgetV(P, 0),	WgetV(P, 1),	WgetV(P, 2),
		WgetV(P, 3),		WgetV(P, 4),	WgetV(P, 5),
		WgetV(P, 6),		WgetV(P, 7),	WgetV(P, 8),
		WgetV(P, 9),		WgetV(P, 10),	WgetV(P, 11));
    13:	CodePrimitive(WgetV(P, 0),	WgetV(P, 1),	WgetV(P, 2),
		WgetV(P, 3),		WgetV(P, 4),	WgetV(P, 5),
		WgetV(P, 6),		WgetV(P, 7),	WgetV(P, 8),
		WgetV(P, 9),		WgetV(P, 10),	WgetV(P, 11),
		WgetV(P, 12));
    14:	CodePrimitive(WgetV(P, 0),	WgetV(P, 1),	WgetV(P, 2),
		WgetV(P, 3),		WgetV(P, 4),	WgetV(P, 5),
		WgetV(P, 6),		WgetV(P, 7),	WgetV(P, 8),
		WgetV(P, 9),		WgetV(P, 10),	WgetV(P, 11),
		WgetV(P, 12),		WgetV(P, 13));
    15:	CodePrimitive(WgetV(P, 0),	WgetV(P, 1),	WgetV(P, 2),
		WgetV(P, 3),		WgetV(P, 4),	WgetV(P, 5),
		WgetV(P, 6),		WgetV(P, 7),	WgetV(P, 8),
		WgetV(P, 9),		WgetV(P, 10),	WgetV(P, 11),
		WgetV(P, 12),		WgetV(P, 13),	WgetV(P, 14));
    end;
end;

syslsp procedure BindEval(Formals, Args);
    BindEvalAux(Formals, Args, 0);

syslsp procedure BindEvalAux(Formals, Args, N);
begin scalar F, A;
    return if PairP Formals then
	if PairP Args then
	<<  F := first Formals;
	    A := Eval first Args;
	    N := BindEvalAux(rest Formals, rest Args, IAdd1 N);
	    if N = -1 then -1 else
	    <<  LBind1(F, A);
		N >> >>
	else -1
    else if PairP Args then -1
    else N;
end;

syslsp procedure CompiledCallingInterpretedAux();
<< %Later Use NARGS also
   % Recall that ID# in CODEFORM
    CompiledCallingInterpretedAuxAux 
	get(MkID(LispVar CodeForm!*), '!*LambdaLink)>>;

syslsp procedure FastLambdaApply();
<<  SaveRegisters();
    CompiledCallingInterpretedAuxAux LispVar CodeForm!* >>;

syslsp procedure CompiledCallingInterpretedAuxAux Fn;
    if not (PairP Fn and car Fn = 'LAMBDA) then
	StdError BldMsg("Ill-formed functional expression %r for %r",
						  Fn,  LispVar CodeForm!*)
    else begin scalar Formals, N, Result;
	Formals := cadr Fn;
	N := 0;
	while PairP Formals do
	<<  LBind1(car Formals,WgetV(CodeArgs, N));
	    Formals := cdr Formals;
	    N := IAdd1 N >>;
	Result := EvProgN cddr Fn;
	if N neq 0 then UnBindN N;
	return Result;
    end;

off Syslisp;

END;
