%
% CATCH-THROW.RED - Common Lisp dynamic non-local exits
% 
% Author:      Eric Benson
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        12 October 1982
% Copyright (c) 1982 University of Utah
%

% 03-Mar-83  Nancy Kendzierski
%  Changed declaration of EMSG!* from fluid to global.
% Edit by Cris Perdue, 23 Feb 1983 1624-PST
% Modified the stack overflow warning message
% Edit by Cris Perdue, 16 Feb 1983 1032-PST
% Changed catch stack overflow checking to give a continuable error
%  when stack gets low, Reset when all out.
% Edit by Cris Perdue,  4 Feb 1983 1209-PST
% Moved ERRSET to ERROR-ERRORSET from here.
% Edit by Cris Perdue,  3 Feb 1983 1520-PST
% Changed catch stack overflow to talk about the CATCH stack. (!)
% Deleted definition of "errset".
%  <PSL.KERNEL>CATCH-THROW.RED.13, 21-Dec-82 15:55:26, Edit by BENSON
%  Added %clear-catch-stack
%  <PSL.KERNEL>CATCH-THROW.RED.13, 16-Dec-82 09:58:59, Edit by BENSON
%  Error not within ErrorSet now causes fatal error, not infinite loop


fluid '(ThrowSignal!*
	ThrowTag!*);

global '(EMSG!*);

macro procedure catch!-all u;
(lambda(fn, forms);
    list(list('lambda, '(!&!&Value!&!&),
		   list('cond, list('ThrowSignal!*,
				    list('Apply,
					 fn,
					 '(list ThrowTag!* !&!&Value!&!&))),
			       '(t !&!&Value!&!&))),
	 'catch . nil . forms))(cadr U, cddr U);

macro procedure unwind!-all u;
(lambda(fn, forms);
    list(list('lambda, '(!&!&Value!&!&),
		   list('Apply,
			fn,
			'(list (and ThrowSignal!* ThrowTag!*)
			       !&!&Value!&!&))),
	 'catch . nil . forms))(cadr U, cddr U);

macro procedure unwind!-protect u;
(lambda(protected_form, cleanup_forms);
    list(list('lambda, '(!&!&Value!&!&),
		   list('lambda, '(!&!&Thrown!&!& !&!&Tag!&!&),
				  'progn . cleanup_forms,
				  '(cond (!&!&Thrown!&!&
					  (!%Throw !&!&Tag!&!& !&!&Value!&!&))
					 (t !&!&Value!&!&)))
		   . '(ThrowSignal!* ThrowTag!*)),
	 list('catch, ''!$unwind!-protect!$, protected_form)))(cadr U,cddr U);

off R2I;

% This funny definition is due to a PA1FN for CATCH

fexpr procedure Catch U;
(lambda(Tag, Forms);
    Catch(Eval Tag, EvProgN Forms))(car U, cdr U);

on R2I;

% Temporary compatibility package.

macro procedure !*Catch U;
    'Catch . cdr U;

expr procedure !*Throw(x,y);
    throw(x,y);

on Syslisp;

% Size is in terms of number of frames
internal WConst CatchStackSize = 400;

internal WArray CatchStack[CatchStackSize*4];

internal WVar CatchStackPtr = &CatchStack[0];

CompileTime <<

smacro procedure CatchPop();
    CatchStackPtr := &CatchStackPtr[-4];

smacro procedure CatchStackDecrement X;
    &X[-4];

% Rather large for a smacro, used only from CatchSetupAux /csp
% Tests structured for fast usual execution /csp
% Random constant 5 for "reserve" catch stack frames /csp
smacro procedure CatchPush(Tag, PC, SP, Env);
<<  CatchStackPtr := &CatchStackPtr[4];
    if CatchStackPtr >= &CatchStack[(CatchStackSize-5)*4] then
    <<  if CatchStackPtr = &CatchStack[(CatchStackSize-5)*4] then
	    ContinuableError(99,"Catch-throw stack overflow (warning)", NIL);
	if CatchStackPtr >= &CatchStack[CatchStackSize*4] then
	<<  (LispVar EMSG!*) := "Catch stack overflow";
	    reset() >> >>;
    CatchStackPtr[0] := Tag;
    CatchStackPtr[1] := PC;
    CatchStackPtr[2] := SP;
    CatchStackPtr[3] := Env >>;

smacro procedure CatchTopTag();
    CatchStackPtr[0];

smacro procedure CatchTagAt X;
    X[0];

smacro procedure CatchTopPC();
    CatchStackPtr[1];

smacro procedure CatchTopSP();
    CatchStackPtr[2];

smacro procedure CatchTopEnv();
    CatchStackPtr[3];

flag('(CatchSetupAux ThrowAux FindCatchMarkAndThrow), 'InternalFunction);

>>;

% CatchSetup puts the return address in reg 2, the stack pointer in reg 3
% and calls CatchSetupAux

lap '((!*entry CatchSetup expr 1)	%. CatchSetup(Tag)
      (!*MOVE (MEMORY (reg st) (WConst 0)) (reg 2))
      (!*MOVE (reg st) (reg 3))
      (!*JCALL CatchSetupAux)
);

syslsp procedure CatchSetupAux(Tag, PC, SP);
begin scalar Previous;
    Previous := CatchStackPtr;
    CatchPush(Tag, PC, SP, CaptureEnvironment());
    LispVar ThrowSignal!* := NIL;
    return Previous;
end;

syslsp procedure !%UnCatch Previous;
<<  CatchStackPtr := Previous;
    LispVar ThrowSignal!* := NIL >>;

syslsp procedure !%clear!-catch!-stack();
    CatchStackPtr := &CatchStack[0];

syslsp procedure !%Throw(Tag, Value);
begin scalar TopTag;
    TopTag := CatchTopTag();
    return if not (null TopTag
		       or TopTag eq '!$unwind!-protect!$
		       or Tag eq TopTag) then
    <<  CatchPop();
	!%Throw(Tag, Value) >>
    else begin scalar PC, SP;
	PC := CatchTopPC();
	SP := CatchTopSP();
	RestoreEnvironment CatchTopEnv();
	CatchPop();
	LispVar ThrowSignal!* := T;
	LispVar ThrowTag!* := Tag;
	return ThrowAux(Value, PC, SP);
    end;
end;

lap '((!*entry ThrowAux expr 3)
      (!*MOVE (reg 3) (reg st))
      (!*MOVE (reg 2) (MEMORY (reg st) (WConst 0)))
      (!*EXIT 0)
);

syslsp procedure Throw(Tag, Value);
    FindCatchMarkAndThrow(Tag, Value, CatchStackPtr);

% Throw to $Error$ that doesn't have a catch can't cause a normal error
% else an infinite loop will result.  Changed to use FatalError instead.

syslsp procedure FindCatchMarkAndThrow(Tag, Value, P);
    if P = &CatchStack[0] then
	if not (Tag eq '!$Error!$) then
	ContError(99,
		  "Catch tag %r not found in Throw",
		  Tag,
		  Throw(Tag, Value))
	else FatalError "Error not within ErrorSet"
    else if null CatchTagAt P or Tag eq CatchTagAt P then
	!%Throw(Tag, Value)
    else FindCatchMarkAndThrow(Tag, Value, CatchStackDecrement P);

off Syslisp;

END;
