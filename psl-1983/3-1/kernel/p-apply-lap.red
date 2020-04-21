%
% P-APPLY-LAP.RED - Inefficient, portable version of APPLY-LAP
% 
% Author:      Eric Benson
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        29 July 1982
% Copyright (c) 1982 University of Utah
%

% Functions which must be written non-portably:

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

CompileTime <<

flag('(CompiledCallingInterpretedAuxAux BindEvalAux SaveRegisters),
     'InternalFunction);

fluid '(CodePtr!*		% gets code pointer used by CodePrimitive
	CodeForm!*		% gets fn to be called from code
);
>>;

on Syslisp;

internal WArray CodeArgs[15];

syslsp procedure CodeApply(CodePtr, ArgList);
begin scalar I;
    I := 0;
    LispVar CodePtr!* := CodePtr;
    while PairP ArgList and ILessP(I, 15) do
    <<  WPutV(CodeArgs , I, first ArgList);
	I := IAdd1 I;
	ArgList := rest ArgList >>;
    if IGEQ(I, 15) then return StdError "Too many arguments to function";
    return case I of
    0:
	CodePrimitive();
    1:
	CodePrimitive WGetV(CodeArgs, 0);
    2:
	CodePrimitive(WGetV(CodeArgs, 0),
		      WGetV(CodeArgs, 1));
    3:
	CodePrimitive(WGetV(CodeArgs, 0),
		      WGetV(CodeArgs, 1),
		      WGetV(CodeArgs, 2));
    4:
	CodePrimitive(WGetV(CodeArgs, 0),
		      WGetV(CodeArgs, 1),
		      WGetV(CodeArgs, 2),
		      WGetV(CodeArgs, 3));
    5:
	CodePrimitive(WGetV(CodeArgs, 0),
		      WGetV(CodeArgs, 1),
		      WGetV(CodeArgs, 2),
		      WGetV(CodeArgs, 3),
		      WGetV(CodeArgs, 4));
    6:
	CodePrimitive(WGetV(CodeArgs, 0),
		      WGetV(CodeArgs, 1),
		      WGetV(CodeArgs, 2),
		      WGetV(CodeArgs, 3),
		      WGetV(CodeArgs, 4),
		      WGetV(CodeArgs, 5));
    7:
	CodePrimitive(WGetV(CodeArgs, 0),
		      WGetV(CodeArgs, 1),
		      WGetV(CodeArgs, 2),
		      WGetV(CodeArgs, 3),
		      WGetV(CodeArgs, 4),
		      WGetV(CodeArgs, 5),
		      WGetV(CodeArgs, 6));
    8:
	CodePrimitive(WGetV(CodeArgs, 0),
		      WGetV(CodeArgs, 1),
		      WGetV(CodeArgs, 2),
		      WGetV(CodeArgs, 3),
		      WGetV(CodeArgs, 4),
		      WGetV(CodeArgs, 5),
		      WGetV(CodeArgs, 6),
		      WGetV(CodeArgs, 7));
    9:
	CodePrimitive(WGetV(CodeArgs, 0),
		      WGetV(CodeArgs, 1),
		      WGetV(CodeArgs, 2),
		      WGetV(CodeArgs, 3),
		      WGetV(CodeArgs, 4),
		      WGetV(CodeArgs, 5),
		      WGetV(CodeArgs, 6),
		      WGetV(CodeArgs, 7),
		      WGetV(CodeArgs, 8));
    10:
	CodePrimitive(WGetV(CodeArgs, 0),
		      WGetV(CodeArgs, 1),
		      WGetV(CodeArgs, 2),
		      WGetV(CodeArgs, 3),
		      WGetV(CodeArgs, 4),
		      WGetV(CodeArgs, 5),
		      WGetV(CodeArgs, 6),
		      WGetV(CodeArgs, 7),
		      WGetV(CodeArgs, 8),
		      WGetV(CodeArgs, 9));
    11:
	CodePrimitive(WGetV(CodeArgs, 0),
		      WGetV(CodeArgs, 1),
		      WGetV(CodeArgs, 2),
		      WGetV(CodeArgs, 3),
		      WGetV(CodeArgs, 4),
		      WGetV(CodeArgs, 5),
		      WGetV(CodeArgs, 6),
		      WGetV(CodeArgs, 7),
		      WGetV(CodeArgs, 8),
		      WGetV(CodeArgs, 9),
		      WGetV(CodeArgs, 10));
    12:
	CodePrimitive(WGetV(CodeArgs, 0),
		      WGetV(CodeArgs, 1),
		      WGetV(CodeArgs, 2),
		      WGetV(CodeArgs, 3),
		      WGetV(CodeArgs, 4),
		      WGetV(CodeArgs, 5),
		      WGetV(CodeArgs, 6),
		      WGetV(CodeArgs, 7),
		      WGetV(CodeArgs, 8),
		      WGetV(CodeArgs, 9),
		      WGetV(CodeArgs, 10),
		      WGetV(CodeArgs, 11));
    13:
	CodePrimitive(WGetV(CodeArgs, 0),
		      WGetV(CodeArgs, 1),
		      WGetV(CodeArgs, 2),
		      WGetV(CodeArgs, 3),
		      WGetV(CodeArgs, 4),
		      WGetV(CodeArgs, 5),
		      WGetV(CodeArgs, 6),
		      WGetV(CodeArgs, 7),
		      WGetV(CodeArgs, 8),
		      WGetV(CodeArgs, 9),
		      WGetV(CodeArgs, 10),
		      WGetV(CodeArgs, 11),
		      WGetV(CodeArgs, 12));
    14:
	CodePrimitive(WGetV(CodeArgs, 0),
		      WGetV(CodeArgs, 1),
		      WGetV(CodeArgs, 2),
		      WGetV(CodeArgs, 3),
		      WGetV(CodeArgs, 4),
		      WGetV(CodeArgs, 5),
		      WGetV(CodeArgs, 6),
		      WGetV(CodeArgs, 7),
		      WGetV(CodeArgs, 8),
		      WGetV(CodeArgs, 9),
		      WGetV(CodeArgs, 10),
		      WGetV(CodeArgs, 11),
		      WGetV(CodeArgs, 12),
		      WGetV(CodeArgs, 13));
    15:
	CodePrimitive(WGetV(CodeArgs, 0),
		      WGetV(CodeArgs, 1),
		      WGetV(CodeArgs, 2),
		      WGetV(CodeArgs, 3),
		      WGetV(CodeArgs, 4),
		      WGetV(CodeArgs, 5),
		      WGetV(CodeArgs, 6),
		      WGetV(CodeArgs, 7),
		      WGetV(CodeArgs, 8),
		      WGetV(CodeArgs, 9),
		      WGetV(CodeArgs, 10),
		      WGetV(CodeArgs, 11),
		      WGetV(CodeArgs, 12),
		      WGetV(CodeArgs, 13),
		      WGetV(CodeArgs, 14));
    end;
end;

%lisp procedure CodeEvalApply(CodePtr, ArgList);
%    CodeApply(CodePtr, EvLis ArgList);

lap '((!*entry CodeEvalApply expr 2)
	(!*ALLOC 15)
	(!*LOC (reg 3) (frame 15))
	(!*CALL CodeEvalApplyAux)
	(!*EXIT 15)
);

syslsp procedure CodeEvalApplyAux(CodePtr, ArgList, P);
begin scalar N;
    N := 0;
    while PairP ArgList and ILessP(N, 15) do
    <<  WPutV(P, ITimes2(StackDirection, N), Eval first ArgList);
	ArgList := rest ArgList;
	N := IAdd1 N >>;
    if IGEQ(N, 15) then return StdError "Too many arguments to function";
    LispVar CodePtr!* := CodePtr;
    return case N of
    0:
	CodePrimitive();
    1:
	CodePrimitive WGetV(P, ITimes2(StackDirection, 0));
    2:
	CodePrimitive(WGetV(P, ITimes2(StackDirection, 0)),
		      WGetV(P, ITimes2(StackDirection, 1)));
    3:
	CodePrimitive(WGetV(P, ITimes2(StackDirection, 0)),
		      WGetV(P, ITimes2(StackDirection, 1)),
		      WGetV(P, ITimes2(StackDirection, 2)));
    4:
	CodePrimitive(WGetV(P, ITimes2(StackDirection, 0)),
		      WGetV(P, ITimes2(StackDirection, 1)),
		      WGetV(P, ITimes2(StackDirection, 2)),
		      WGetV(P, ITimes2(StackDirection, 3)));
    5:
	CodePrimitive(WGetV(P, ITimes2(StackDirection, 0)),
		      WGetV(P, ITimes2(StackDirection, 1)),
		      WGetV(P, ITimes2(StackDirection, 2)),
		      WGetV(P, ITimes2(StackDirection, 3)),
		      WGetV(P, ITimes2(StackDirection, 4)));
    6:
	CodePrimitive(WGetV(P, ITimes2(StackDirection, 0)),
		      WGetV(P, ITimes2(StackDirection, 1)),
		      WGetV(P, ITimes2(StackDirection, 2)),
		      WGetV(P, ITimes2(StackDirection, 3)),
		      WGetV(P, ITimes2(StackDirection, 4)),
		      WGetV(P, ITimes2(StackDirection, 5)));
    7:
	CodePrimitive(WGetV(P, ITimes2(StackDirection, 0)),
		      WGetV(P, ITimes2(StackDirection, 1)),
		      WGetV(P, ITimes2(StackDirection, 2)),
		      WGetV(P, ITimes2(StackDirection, 3)),
		      WGetV(P, ITimes2(StackDirection, 4)),
		      WGetV(P, ITimes2(StackDirection, 5)),
		      WGetV(P, ITimes2(StackDirection, 6)));
    8:
	CodePrimitive(WGetV(P, ITimes2(StackDirection, 0)),
		      WGetV(P, ITimes2(StackDirection, 1)),
		      WGetV(P, ITimes2(StackDirection, 2)),
		      WGetV(P, ITimes2(StackDirection, 3)),
		      WGetV(P, ITimes2(StackDirection, 4)),
		      WGetV(P, ITimes2(StackDirection, 5)),
		      WGetV(P, ITimes2(StackDirection, 6)),
		      WGetV(P, ITimes2(StackDirection, 7)));
    9:
	CodePrimitive(WGetV(P, ITimes2(StackDirection, 0)),
		      WGetV(P, ITimes2(StackDirection, 1)),
		      WGetV(P, ITimes2(StackDirection, 2)),
		      WGetV(P, ITimes2(StackDirection, 3)),
		      WGetV(P, ITimes2(StackDirection, 4)),
		      WGetV(P, ITimes2(StackDirection, 5)),
		      WGetV(P, ITimes2(StackDirection, 6)),
		      WGetV(P, ITimes2(StackDirection, 7)),
		      WGetV(P, ITimes2(StackDirection, 8)));
    10:
	CodePrimitive(WGetV(P, ITimes2(StackDirection, 0)),
		      WGetV(P, ITimes2(StackDirection, 1)),
		      WGetV(P, ITimes2(StackDirection, 2)),
		      WGetV(P, ITimes2(StackDirection, 3)),
		      WGetV(P, ITimes2(StackDirection, 4)),
		      WGetV(P, ITimes2(StackDirection, 5)),
		      WGetV(P, ITimes2(StackDirection, 6)),
		      WGetV(P, ITimes2(StackDirection, 7)),
		      WGetV(P, ITimes2(StackDirection, 8)),
		      WGetV(P, ITimes2(StackDirection, 9)));
    11:
	CodePrimitive(WGetV(P, ITimes2(StackDirection, 0)),
		      WGetV(P, ITimes2(StackDirection, 1)),
		      WGetV(P, ITimes2(StackDirection, 2)),
		      WGetV(P, ITimes2(StackDirection, 3)),
		      WGetV(P, ITimes2(StackDirection, 4)),
		      WGetV(P, ITimes2(StackDirection, 5)),
		      WGetV(P, ITimes2(StackDirection, 6)),
		      WGetV(P, ITimes2(StackDirection, 7)),
		      WGetV(P, ITimes2(StackDirection, 8)),
		      WGetV(P, ITimes2(StackDirection, 9)),
		      WGetV(P, ITimes2(StackDirection, 10)));
    12:
	CodePrimitive(WGetV(P, ITimes2(StackDirection, 0)),
		      WGetV(P, ITimes2(StackDirection, 1)),
		      WGetV(P, ITimes2(StackDirection, 2)),
		      WGetV(P, ITimes2(StackDirection, 3)),
		      WGetV(P, ITimes2(StackDirection, 4)),
		      WGetV(P, ITimes2(StackDirection, 5)),
		      WGetV(P, ITimes2(StackDirection, 6)),
		      WGetV(P, ITimes2(StackDirection, 7)),
		      WGetV(P, ITimes2(StackDirection, 8)),
		      WGetV(P, ITimes2(StackDirection, 9)),
		      WGetV(P, ITimes2(StackDirection, 10)),
		      WGetV(P, ITimes2(StackDirection, 11)));
    13:
	CodePrimitive(WGetV(P, ITimes2(StackDirection, 0)),
		      WGetV(P, ITimes2(StackDirection, 1)),
		      WGetV(P, ITimes2(StackDirection, 2)),
		      WGetV(P, ITimes2(StackDirection, 3)),
		      WGetV(P, ITimes2(StackDirection, 4)),
		      WGetV(P, ITimes2(StackDirection, 5)),
		      WGetV(P, ITimes2(StackDirection, 6)),
		      WGetV(P, ITimes2(StackDirection, 7)),
		      WGetV(P, ITimes2(StackDirection, 8)),
		      WGetV(P, ITimes2(StackDirection, 9)),
		      WGetV(P, ITimes2(StackDirection, 10)),
		      WGetV(P, ITimes2(StackDirection, 11)),
		      WGetV(P, ITimes2(StackDirection, 12)));
    14:
	CodePrimitive(WGetV(P, ITimes2(StackDirection, 0)),
		      WGetV(P, ITimes2(StackDirection, 1)),
		      WGetV(P, ITimes2(StackDirection, 2)),
		      WGetV(P, ITimes2(StackDirection, 3)),
		      WGetV(P, ITimes2(StackDirection, 4)),
		      WGetV(P, ITimes2(StackDirection, 5)),
		      WGetV(P, ITimes2(StackDirection, 6)),
		      WGetV(P, ITimes2(StackDirection, 7)),
		      WGetV(P, ITimes2(StackDirection, 8)),
		      WGetV(P, ITimes2(StackDirection, 9)),
		      WGetV(P, ITimes2(StackDirection, 10)),
		      WGetV(P, ITimes2(StackDirection, 11)),
		      WGetV(P, ITimes2(StackDirection, 12)),
		      WGetV(P, ITimes2(StackDirection, 13)));
    15:
	CodePrimitive(WGetV(P, ITimes2(StackDirection, 0)),
		      WGetV(P, ITimes2(StackDirection, 1)),
		      WGetV(P, ITimes2(StackDirection, 2)),
		      WGetV(P, ITimes2(StackDirection, 3)),
		      WGetV(P, ITimes2(StackDirection, 4)),
		      WGetV(P, ITimes2(StackDirection, 5)),
		      WGetV(P, ITimes2(StackDirection, 6)),
		      WGetV(P, ITimes2(StackDirection, 7)),
		      WGetV(P, ITimes2(StackDirection, 8)),
		      WGetV(P, ITimes2(StackDirection, 9)),
		      WGetV(P, ITimes2(StackDirection, 10)),
		      WGetV(P, ITimes2(StackDirection, 11)),
		      WGetV(P, ITimes2(StackDirection, 12)),
		      WGetV(P, ITimes2(StackDirection, 13)),
		      WGetV(P, ITimes2(StackDirection, 14)));
    end;
end;

off Syslisp;

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

syslsp procedure SaveRegisters(A1, A2, A3, A4, A5,
			       A6, A7, A8, A9, A10,
			       A11, A12, A13, A14, A15);
<<  CodeArgs[14] := A15;
    CodeArgs[13] := A14;
    CodeArgs[12] := A13;
    CodeArgs[11] := A12;
    CodeArgs[10] := A11;
    CodeArgs[9]  := A10;
    CodeArgs[8]  := A9;
    CodeArgs[7]  := A8;
    CodeArgs[6]  := A7;
    CodeArgs[5]  := A6;
    CodeArgs[4]  := A5;
    CodeArgs[3]  := A4;
    CodeArgs[2]  := A3;
    CodeArgs[1]  := A2;
    CodeArgs[0]  := A1 >>;

syslsp procedure CompiledCallingInterpretedAux();
<<  SaveRegisters();
    CompiledCallingInterpretedAuxAux get(LispVar CodeForm!*, '!*LambdaLink) >>;

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
	<<  LBind1(car Formals, WGetV(CodeArgs, N));
	    Formals := cdr Formals;
	    N := IAdd1 N >>;
	Result := EvProgN cddr Fn;
	if N neq 0 then UnBindN N;
	return Result;
    end;

off Syslisp;

END;
