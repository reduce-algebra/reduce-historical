%  <PSL.KERNEL>BACKTRACE.RED.3, 20-Sep-82 10:21:41, Edit by BENSON
%  Attempt to make output easier to read

CompileTime flag('(Backtrace1 BacktraceRange), 'InternalFunction);

fluid '(IgnoredInBacktrace!* Options!* InterpreterFunctions!*);

IgnoredInBacktrace!* := '(Eval Apply FastApply CodeApply CodeEvalApply
    			  Catch ErrorSet EvProgN TopLoop BreakEval
			  BindEval
			  Break Main);

InterpreterFunctions!* := '(Cond Prog And Or ProgN SetQ);

on SysLisp;

external WVar StackLowerBound, HeapUpperBound;

syslsp procedure InterpBacktrace();
begin scalar Here;
    Here := &Here;
    PrintF "Backtrace, including interpreter functions, from top of stack:%n";
    return BacktraceRange(Here, StackLowerBound, 1);
end;

syslsp procedure Backtrace();
begin scalar Here, X;
    Here := &Here;
    PrintF "Backtrace from top of stack:%n";
    return BacktraceRange(Here, StackLowerBound, 0);
end;

syslsp procedure BacktraceRange(Starting, Ending, InterpFlag);
begin scalar X;
    for I := Starting step -(AddressingUnitsPerItem*StackDirection)
		until Ending do
	if Tag @I eq BtrTag then
	    Backtrace1(MkID Inf @I, InterpFlag)
	else if (X := ReturnAddressP @I) then
	    Backtrace1(X, InterpFlag);
    return TerPri();
end;

syslsp procedure VerboseBacktrace();
begin scalar Here, X;
    if not 'addr2id member options!* then load addr2id;
    Here := &Here;			% start a little before here
    for I := Here step -(AddressingUnitsPerItem*StackDirection)
		until StackLowerBound do
	if CodeP @I and Inf @I > HeapUpperBound then
	<<  WriteChar char TAB;
	    ChannelWriteUnknownItem(LispVar OUT!*, @I);
	    TerPri() >>
	else if Tag @I eq BtrTag then
	    PrintF("	%r%n", MkID Inf @I)
	else if (X := ReturnAddressP @I) then
	    PrintF("%p -> %p:%n", code!-address!-to!-symbol Inf @I, X)
	else PrintF("	%p%n", @I);
    return TerPri();
end;

off SysLisp;

lisp procedure Backtrace1(Item, Code);
%
% Code is 1 if Interpreter functions should be printed, 0 if not.
%
    if not (Item memq IgnoredInBacktrace!*) then
	if not (Code = 0 and Item memq InterpreterFunctions!*) then
	<<  Prin1 Item;
	    WriteChar char BLANK >>;

END;
