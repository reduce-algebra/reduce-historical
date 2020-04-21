%
% BREAK.RED - Break using new top loop
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        23 October 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL>BREAK.RED.2, 11-Oct-82 17:52:13, Edit by BENSON
%  Changed CATCH/THROW to new definition
%  <PSL.INTERP>BREAK.RED.6, 28-Jul-82 14:29:59, Edit by BENSON
%  Added A for abort-to-top-level
%  <PSL.INTERP>BREAK.RED.3, 30-Apr-82 14:34:33, Edit by BENSON
%  Added binding of !*DEFN to NIL

fluid '(!*Break !*QuitBreak BreakEval!* BreakName!* BreakValue!*
	ErrorForm!*
	BreakLevel!* MaxBreakLevel!*
	TopLoopName!* TopLoopEval!* TopLoopRead!* TopLoopPrint!*
	!*DEFN				% break binds !*DEFN to NIL
	BreakIn!* BreakOut!*);

LoadTime <<
BreakLevel!* := 0;
MaxBreakLevel!* := 5;
>>;

lisp procedure Break();			%. Enter top loop within evaluation
(lambda(BreakLevel!*);
begin scalar OldIn, OldOut, !*QuitBreak,BreakValue!*, !*Defn;
    OldIn := RDS BreakIn!*;
    OldOut := WRS BreakOut!*;
    !*QuitBreak := T;
    if TopLoopName!* then
    <<  if TopLoopEval!* neq 'BreakEval then
	<<  BreakEval!* := TopLoopEval!*;
	    BreakName!* := ConCat(TopLoopName!*, " break") >>;
        Catch('!$Break!$, TopLoop(TopLoopRead!*,
					TopLoopPrint!*,
					'BreakEval,
					BreakName!*,
					"Break loop")) >>
    else
    <<  BreakEval!* := 'Eval;
	BreakName!* := "lisp break";
	Catch('!$Break!$, TopLoop('Read,
					'Print,
					'BreakEval,
					BreakName!*,
					"Break loop")) >>;
    RDS OldIn;
    WRS OldOut;
    return if !*QuitBreak then begin scalar !*Break, !*EmsgP;
	return StdError "Exit to ErrorSet";
    end else
	Eval ErrorForm!*;
end)(BreakLevel!* + 1);

lisp procedure BreakEval U;
begin scalar F;
    return if IDP U and (F := get(U, 'BreakFunction)) then
	Apply(F, NIL)
    else BreakValue!*:=Apply(BreakEval!*, list U);
end;

lisp procedure BreakQuit();
<<  !*QuitBreak := T;
    Throw('!$Break!$, NIL) >>;

lisp procedure BreakContinue();
<<  ErrorForm!* := MkQuote BreakValue!*;
    BreakRetry() >>;

lisp procedure BreakRetry();
    if !*ContinuableError then
    <<  !*QuitBreak := NIL;
	Throw('!$Break!$, NIL) >>
    else
    <<  Prin2T
"Can only continue from a continuable error; use Q (BreakQuit) to quit";
	TerPri() >>;

lisp procedure HelpBreak();
<<  EvLoad '(HELP);
    DisplayHelpFile 'Break >>;

lisp procedure BreakErrMsg();
    PrintF("ErrorForm!* : %r %n", ErrorForm!*);

lisp procedure BreakEdit();
    if GetD 'Edit then ErrorForm!* := Edit ErrorForm!*
    else ErrorPrintF("*** Editor not loaded");

LoadTime DefList('((Q BreakQuit)
		   (!? HelpBreak)
		   (A Reset)		% Abort to top level
		   (M BreakErrMsg)
		   (E BreakEdit)
		   (C BreakContinue)
		   (R BreakRetry)
		   (I InterpBackTrace)
		   (V VerboseBackTrace)
		   (T BackTrace)),
		 'BreakFunction);

END;
