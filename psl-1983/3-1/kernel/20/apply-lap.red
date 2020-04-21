%
% APPLY-LAP.RED - LAP support for EVAL and APPLY
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        20 August 1981
% Copyright (c) 1981 University of Utah
%

%  25-May-1983 Mark R. Swanson
%  Changes to support extended addressing; mostly clearing instruction field
%  of entries from SYMFNC table
%  <PSL.NEW>APPLY-LAP.RED.2,  9-Dec-82 18:13:02, Edit by PERDUE
%  Modified UndefinedFunction to make it continuable

CompileTime flag('(FastLambdaApply), 'InternalFunction);

on SysLisp;

external WVar BndStkPtr, BndStkUpperBound;

% TAG( CodeApply )

% if this could be written in Syslisp, it would look something like this:

% syslsp procedure CodeApply(CodePtr, ArgList);
% begin scalar N;
%     N := 0;
%     while PairP ArgList do
%     <<  N := N + 1;
%	  ArgumentRegister[N] := car ArgList;
%	  ArgList := cdr ArgList >>;
%     (jump to address of code pointer)
% end;

lap '((!*entry CodeApply expr 2)	%. CodeApply(CodePointer, ArgList)
%
% r1 is code pointer, r2 is list of arguments
%
        (!*field (reg t1) (reg 1) 12 24) % make it a local address
	(!*MOVE (reg 2) (reg t2))
	(!*MOVE (WConst 1) (reg t3))
Loop
	(!*JUMPNOTTYPE
	       (MEMORY (REG T1) (WConst 0))
	       (reg t2) PAIR)
					% jump to code if list is exhauseted
	(!*MOVE (CAR (reg t2)) (reg t4))
	(!*MOVE (reg t4) (MEMORY (reg t3) 0))	% load argument register
	(!*MOVE (CDR (reg t2)) (reg t2))
	(!*WPLUS2 (reg t3) (WConst 1))	% increment register pointer
	(cain (reg t3) (plus2 (WConst MaxRealRegs) 1)) % skip if neq MaxRegs+1
	(!*MOVE (WConst ArgumentBlock) (reg t3)) % else switch to extra args
	(!*JUMPWLEQ (Label Loop)
		    (reg t3)
		    (WConst (plus2 9 (WConst ArgumentBlock))))
	(!*MOVE (QUOTE "Too many arguments to function") (reg 1))
	(!*JCALL StdError)
);

% TAG( CodeEvalApply )

% if this could be written in Syslisp, it would look something like this:

% syslsp procedure CodeEvalApply(CodePtr, ArgList);
% begin scalar N;
%     N := 0;
%     while PairP ArgList do
%     <<  N := N + 1;
%	  ArgumentRegister[N] := Eval car ArgList;
%	  ArgList := cdr ArgList >>;
%     (jump to address of code pointer)
% end;

lap '((!*entry CodeEvalApply expr 2)	%. CodeApply(CodePointer, EvLis Args)
%
% r1 is code pointer, r2 is list of arguments to be evaled
%
	(!*PUSH (reg 1))		% code pointer goes on the bottom
	(!*PUSH (WConst 0))		% then arg count
Loop					% if it's not a pair, then we're done
	(!*JUMPNOTTYPE (Label Done) (reg 2) PAIR)
	(!*JUMPWLESSP (Label ArgOverflow) (frame 1) (WConst -15))
	(!*MOVE (CAR (reg 2)) (reg 1))
	(!*MOVE (CDR (reg 2)) (reg 2))
	(!*PUSH (reg 2))		% save the cdr
	(!*CALL Eval)			% eval the car
	(!*POP (reg 2))			% grab the list in r2 again
	(!*POP (reg 3))			% get count in r3
	(!*WDIFFERENCE (reg 3) (WConst 1))	% decrement count
	(!*PUSH (reg 1))		% push the evaled arg
	(!*PUSH (reg 3))		% and the decremented count
	(!*JUMP (Label Loop))
Done
	(!*POP (reg 3))			% count in r3, == -no. of args to pop
	(!*JUMP (MEMORY (reg 3) (Label ZeroArgs)))	% indexed jump
	(!*POP (MEMORY (WConst (plus2 (WArray ArgumentBlock) 9)) (WConst 0)))
	(!*POP (MEMORY (WConst (plus2 (WArray ArgumentBlock) 8)) (WConst 0)))
	(!*POP (MEMORY (WConst (plus2 (WArray ArgumentBlock) 7)) (WConst 0)))
	(!*POP (MEMORY (WConst (plus2 (WArray ArgumentBlock) 6)) (WConst 0)))
	(!*POP (MEMORY (WConst (plus2 (WArray ArgumentBlock) 5)) (WConst 0)))
	(!*POP (MEMORY (WConst (plus2 (WArray ArgumentBlock) 4)) (WConst 0)))
	(!*POP (MEMORY (WConst (plus2 (WArray ArgumentBlock) 3)) (WConst 0)))
	(!*POP (MEMORY (WConst (plus2 (WArray ArgumentBlock) 2)) (WConst 0)))
	(!*POP (MEMORY (WConst (plus2 (WArray ArgumentBlock) 1)) (WConst 0)))
	(!*POP (MEMORY (WConst (plus2 (WArray ArgumentBlock) 0)) (WConst 0)))
	(!*POP (reg 5))
	(!*POP (reg 4))
	(!*POP (reg 3))
	(!*POP (reg 2))
	(!*POP (reg 1))
ZeroArgs
	(!*POP (reg t1))		% code pointer in (reg t1)
	(!*field (reg t1) (reg t1) 12 24) % isolate just local addr bits
	(!*JUMP (MEMORY (reg t1) (WConst 0))) % jump to address
ArgOverflow
	(!*MOVE (QUOTE "Too many arguments to function") (reg 1))
	(!*JCALL StdError)
);

% TAG( BindEval )

% if this could be written in Syslisp, it would look something like this:

% syslsp procedure BindEval(Formals, Args);
% begin scalar N;
%     N := 0;
%     while PairP Args and PairP Formals do
%     <<  N := N + 1;
%	  Push Eval car ArgList;
%	  Push car Formals;
%	  ArgList := cdr ArgList >>;
%     if PairP Args or PairP Formals then return -1;
%     for I := 1 step 1 until N do
%	  LBind1(Pop(), Pop());
%     return N;
% end;

lap '((!*entry BindEval expr 2)	 %. BindEval(FormalsList, ArgsToBeEvaledList);
%
% r1 is list of formals, r2 is list of arguments to be evaled
%
	(!*PUSH (WConst 0))		% count on the bottom
	(!*MOVE (WConst 0) (reg 4))
	(!*MOVE (reg 1) (reg 3))	% shift arg1 to r3
EvalLoop				% if it's not a pair, then we're done
	(!*JUMPNOTTYPE (Label DoneEval) (reg 2) PAIR)
	(!*MOVE (CAR (reg 2)) (reg 1))
	(!*MOVE (CDR (reg 2)) (reg 2))
	(!*PUSH (reg 3))		% save the formals
	(!*PUSH (reg 2))		% save the rest of args
	(!*CALL Eval)			% eval the car
	(!*POP (reg 2))			% save then rest of arglist
	(!*POP (reg 3))			% and the rest of formals
	(!*POP (reg 4))			% and the count
	(!*JUMPNOTTYPE (Label ReturnError) (reg 3) PAIR)
					% if it's not a pair, then error
	(!*WPLUS2 (reg 4) (WConst 1))	% increment the count
	(!*MOVE (CAR (reg 3)) (reg 5))
	(!*MOVE (CDR (reg 3)) (reg 3))
	(!*PUSH (reg 1))		% push the evaluated argument
	(!*PUSH (reg 5))		% and next formal
	(!*PUSH (reg 4))		% and new count
	(!*JUMP (Label EvalLoop))
ReturnError
	(!*WSHIFT (reg 4) (WConst 1))	% multiply count by 2
	(hrl (reg 4) (reg 4))		% in both halves
	(sub (reg st) (reg 4))		% move the stack ptr back
	(!*MOVE (WConst -1) (reg 1))	% return -1 as error indicator
	(!*EXIT 0)
DoneEval
	(!*DEALLOC 1)			% removed saved values at top of stack
	(!*JUMPTYPE (Label ReturnError) (reg 3) PAIR) % if more formals, error
	(!*MOVE (reg 4) (reg 3))   % r3 gets decremented, r4 saved for return
BindLoop
	(!*JUMPEQ (Label NormalReturn) (reg 3) (WConst 0))
					% if count is zero, then return
	(!*POP (reg 1))			% pop ID to bind
	(!*POP (reg 2))			% and value
	(!*PUSH (reg 3))
	(!*PUSH (reg 4))
	(!*CALL LBind1)
	(!*POP (reg 4))
	(!*POP (reg 3))
	(soja (reg 3) BindLoop)
NormalReturn
	(!*MOVE (reg 4) (reg 1))	% return count
	(!*EXIT 0)
);

% TAG( CompiledCallingInterpreted )

% This is pretty gross, but it is essentially the same as LambdaApply, taking
% values from the argument registers instead of a list.

% if this could be written in Syslisp, it would look something like this:

% syslsp procedure CompiledCallingInterpreted IDOfFunction;
% begin scalar LForm, LArgs, N, Result;
%     LForm := get(IDOfFunction, '!*LambdaLink);
%     LArgs := cadr LForm;
%     LForm := cddr LForm;
%     N := 1;
%     while PairP LArgs do
%     <<  LBind1(car LArgs, ArgumentRegister[N];
%         LArgs := cdr LArgs;
%         N := N + 1 >>;
%     Result := EvProgN LForm;
%     UnBindN(N - 1);
%     return Result;
% end;

lap '((!*entry CompiledCallingInterpreted expr 0)	%. link for lambda
%
% called by JSP T5, from function cell
%
	(!*MOVE (reg t5) (reg t1))
	(!*WDIFFERENCE (reg t1) (WConst (plus2 (WConst SymFnc) 1)))
	(!*MKITEM (reg t1) (WConst BtrTag))
	(!*PUSH (reg t1))		% make stack mark for btrace
	(hrrz (reg t1)(reg t1))         % discard extraneous left half
	(!*MOVE (MEMORY (reg t1) (WConst SymPrp)) (reg t1)) % load prop list
LoopFindProp
	(!*JUMPNOTTYPE (Label PropNotFound) (reg t1) PAIR)
	(!*MOVE (CAR (reg t1)) (reg t2))		% get car of prop list
	(!*MOVE (CDR (reg t1)) (reg t1))		% cdr down
	(!*JUMPNOTTYPE (Label LoopFindProp) (reg t2) PAIR)
	(!*MOVE (CAR (reg t2)) (reg t3))	% its a pair, look at car
	(!*JUMPNOTEQ (Label LoopFindProp) (reg t3) '!*LambdaLink)
	(!*MOVE (CDR (reg t2)) (reg t2))	% yes, get lambda form
	(!*entry FastLambdaApply expr 0)	% called from FastApply
	(!*MOVE (CDR (reg t2)) (reg t2))	% get cdr of lambda form
	(!*MOVE (CDR (reg t2)) (reg t1))	% save cddr in (reg t1)
	(!*MOVE (CAR (reg t2)) (reg t2))	% cadr of lambda == arg list
	(!*MOVE (WConst 1) (reg t3))	% pointer to arg register in t3
	(!*MOVE (WVar BndStkPtr) (reg t4))	% binding stack pointer in t4
	(!*PUSH (reg t4))		% save it on the stack
LoopBindingFormals
	(!*JUMPNOTTYPE (Label DoneBindingFormals) (reg t2) PAIR)
	(!*WPLUS2 (reg t4) (WConst 2))	% adjust binding stack pointer up 2
	(caml (reg t4) (WVar BndStkUpperBound))	% if overflow occured
	(!*JCALL BStackOverflow)	% then error
	(!*MOVE (CAR (reg t2)) (reg t5))	% get formal in t5
	(hrrzm (reg t5) (Indexed (reg t4) -1))	% store ID number in BndStk
	(!*MOVE (MEMORY (reg t5) (WArray SymVal)) (reg t6))	% get old value
	(!*MOVE (reg t6) (MEMORY (reg t4) (WConst 0)))	% store value in BndStk
	(!*MOVE (MEMORY (reg t3) (WConst 0)) (reg t6))	% get reg value in t6
	(!*MOVE (reg t6) (MEMORY (reg t5) (WConst SymVal))) % put in value cell
	(!*MOVE (CDR (reg t2)) (reg t2))	% cdr down argument list
	(!*WPLUS2 (reg t3) (WConst 1))	% increment register pointer
	(cain (reg t3) (plus2 (WConst MaxRealRegs) 1)) % Go to extra args?
	(movei (reg t3) (WArray ArgumentBlock))	% Yes
	(!*JUMP (Label LoopBindingFormals))	% No
DoneBindingFormals
	(!*MOVE (reg t4) (WVar BndStkPtr))	% store binding stack
	(!*MOVE (reg t1) (reg 1))	% get cddr of lambda form to eval
	(!*CALL EvProgN)		% implicit progn
	(exch (reg 1) (Indexed (reg st) 0)) % save result, get old bind stk ptr
	(!*CALL RestoreEnvironment)
	(!*POP (reg 1))			% restore old bindings and pickup value
	(!*EXIT 1)			% throw away backtrace mark and return
PropNotFound
	(!*MOVE (QUOTE
"Internal error in function calling mechanism; consult a wizard") (reg 1))
	(!*JCALL StdError)
);


% TAG( FastApply )

lap '((!*entry FastApply expr 0)	%. Apply with arguments loaded
%
% Called with arguments in the registers and functional form in (reg t1)
%
	(!*FIELD (reg t2) (reg t1)
		 (WConst TagStartingBit)
		 (WConst TagBitLength))
	(!*FIELD (reg t1) (reg t1) 12 24) % make it a local address
	(!*JUMPEQ (MEMORY (reg t1) (WConst SymFnc)) (reg t2) (WConst ID))
	(!*JUMPEQ (MEMORY (reg t1) (WConst 0)) (reg t2) (WConst CODE))
	(!*JUMPNOTEQ (Label IllegalFunctionalForm) (reg t2) (WConst PAIR))
	(!*MOVE (CAR (reg t1)) (reg t2))
	(!*JUMPNOTEQ IllegalFunctionalForm (reg t2) (QUOTE LAMBDA))
	(!*MOVE (reg t1) (reg t2))	% put lambda form in (reg t2)
	(!*PUSH '())			% align stack
	(!*JCALL FastLambdaApply)
IllegalFunctionalForm
	(!*MOVE (QUOTE "Illegal functional form %r in Apply") (reg 1))
	(!*MOVE (reg t1) (reg 2))
	(!*CALL BldMsg)
	(!*JCALL StdError)
);

% TAG( UndefinedFunction )

lap '((!*entry UndefinedFunction expr 0)	%. Error Handler for non code
%
% also called by JSP T5,
%
	(!*WDIFFERENCE (reg t5) (wconst 1))
	% T5 now points to the function entry slot of the atom that
	% is undefined as a function.
	% We will push the entry address onto the stack and transfer
	% to it by a POPJ at the end of this routine.
	(!*PUSH (reg t5))
	(!*PUSH (reg 1))	% Save all the regs (including fakes) (args)
	(!*PUSH (reg 2))
	(!*PUSH (reg 3))
	(!*PUSH (reg 4))
	(!*PUSH (reg 5))
	(!*PUSH (reg 6))
	(!*PUSH (reg 7))
	(!*PUSH (reg 8))
	(!*PUSH (reg 9))
	(!*PUSH (reg 10))
	(!*PUSH (reg 11))
	(!*PUSH (reg 12))
	(!*PUSH (reg 13))
	(!*PUSH (reg 14))
	(!*PUSH (reg 15))

	(!*WDIFFERENCE (reg t5) (WConst SymFnc))
	(!*MKITEM (reg t5) (WConst ID))
	(!*MOVE (reg t5) (reg 2))
	(!*MOVE (QUOTE "Undefined function %r called from compiled code")
		(reg 1))
	(!*CALL BldMsg)
	(!*MOVE (reg 1) (reg 2))
	(!*MOVE (WConst 0) (reg 1))
	(!*MOVE (reg NIL) (reg 3))
	(!*CALL ContinuableError)

	(!*POP (reg 15))	% Restore all those possible arguments
	(!*POP (reg 14))
	(!*POP (reg 13))
	(!*POP (reg 12))
	(!*POP (reg 11))
	(!*POP (reg 10))
	(!*POP (reg 9))
	(!*POP (reg 8))
	(!*POP (reg 7))
	(!*POP (reg 6))
	(!*POP (reg 5))
	(!*POP (reg 4))
	(!*POP (reg 3))
	(!*POP (reg 2))
	(!*POP (reg 1))
	(!*EXIT 0)
);

off SysLisp;

END;
