%
% TOP-LOOP.RED - Generalized top loop construct
% 
% Author:      Eric Benson and M. L. Griss
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        19 October 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL>TOP-LOOP.RED.6,  5-Oct-82 11:02:29, Edit by BENSON
%  Added EvalInitForms, changed SaveSystem to 3 args
%  <PSL.KERNEL>TOP-LOOP.RED.5,  4-Oct-82 18:09:33, Edit by BENSON
%  Added GCTime!*
%  $pi/top-loop.red, Mon Jun 28 10:54:19 1982, Edit by Fish
%  Conditional output: !*Output, Semic!*, !*NoNil.
%  <PSL.INTERP>TOP-LOOP.RED.13, 30-Apr-82 14:32:20, Edit by BENSON
%  Minor change to !*DEFN processing
%  <PSL.INTERP>TOP-LOOP.RED.5, 29-Apr-82 03:56:06, Edit by GRISS
%  Initial attempt to add !*DEFN processing
%<PSL.INTERP>TOP-LOOP.RED.18 24-Nov-81 15:22:25, Edit by BENSON
% Changed Standard!-Lisp to StandardLisp

CompileTime flag('(NthEntry DefnPrint DefnPrint1 HistPrint),
		 'InternalFunction);

fluid '(TopLoopRead!*			% reading function
	TopLoopPrint!*			% printing function
	TopLoopEval!*			% evaluation function
	TopLoopName!*			% short name to put in prompt
	TopLoopLevel!*			% depth of top loop invocations
	HistoryCount!*			% number of entries read so far
	HistoryList!*			% list of entries read and evaluated
	PromptString!*			% input prompt
	LispBanner!*		% Welcome banner printed in StandardLisp
	!*EMsgP				% whether to print error messages
	!*BackTrace			% whether to print backtrace
	!*Time				% whether to print timing of evaluation
	GCTime!*			% Time spent in garbage collection
        !*Defn                          % To "output" rather than process
        DFPRINT!*                       % Alternate DEFN print function
	!*Output			% Whether to print output.
	Semic!*				% Input terminator when in Rlisps.
	!*NoNil				% Whether to supress NIL value print.
	InitForms!*			% Forms to be evaluated at startup
);

LoadTime <<
TopLoopLevel!* := -1;
HistoryCount!* := 0;
LispBanner!* := "Portable Standard LISP";
!*Output := T;		% Output ON by default.
>>;

lisp procedure TopLoop(TopLoopRead!*,	%. Generalized top-loop mechanism
		       TopLoopPrint!*,	%.
		       TopLoopEval!*,	%.
		       TopLoopName!*,	%.
		       WelcomeBanner);	%.
begin scalar PromptString!*, Semic!*, LevelPrompt, ThisGCTime,
	     InputValue, OutputValue, TimeCheck;
Semic!* := '!; ;	% Output when semicolon terminator for rlisps.
(lambda TopLoopLevel!*;
begin
    TimeCheck := 0;
    ThisGCTime := GCTime!*;
    LevelPrompt := MkString(TopLoopLevel!*, char '!> );
    Prin2T WelcomeBanner;
LoopStart:
    HistoryCount!* := IAdd1 HistoryCount!*;
    HistoryList!* := (NIL . NIL) . HistoryList!*;
    PromptString!* := BldMsg("%w %w%w ",
			     HistoryCount!*,
			     TopLoopName!*,
			     LevelPrompt);
    InputValue := ErrorSet(quote Apply(TopLoopRead!*, NIL), T, !*Backtrace);
    if InputValue eq '!$ExitTopLoop!$ then goto LoopExit;
    if not PairP InputValue then
	goto LoopStart;
    InputValue := car InputValue;
    if InputValue eq '!$ExitTopLoop!$ then goto LoopExit;
    if InputValue eq !$EOF!$ then goto LoopExit;
    Rplaca(car HistoryList!*, InputValue);
    if !*Time then
    <<  TimeCheck := Time();
	ThisGCTime := GCTime!* >>;
    if !*Defn then
	OutputValue := DefnPrint InputValue
    else   
	OutputValue := ErrorSet(list('Apply, MkQuote TopLoopEval!*,
					     MkQuote list InputValue),
				T,
				!*Backtrace);
    if not PairP OutputValue then
	goto LoopStart;
    OutputValue := car OutputValue;
    if !*Time then
    <<  TimeCheck := Time() - TimeCheck;
	ThisGCTime := GCTime!* - ThisGCTime >>;
    Rplacd(car HistoryList!*, OutputValue);
    if  !*Output  and  Semic!* eq '!;
	and  not (!*NoNil and OutputValue eq NIL)  then
	    ErrorSet(list('Apply,
			  MkQuote TopLoopPrint!*,
			  MkQuote list OutputValue), T, !*Backtrace);
    if !*Time then
	if ThisGCTime = 0 then
	    PrintF("Cpu time: %w ms%n", TimeCheck)
	else
	    PrintF("Cpu time: %w ms, GC time: %w ms%n",
		    TimeCheck - ThisGCTime, ThisGCTime);
    goto LoopStart;
LoopExit:
    PrintF("Exiting %w%n", TopLoopName!*);
end)(IAdd1 TopLoopLevel!*);
end;

lisp procedure DefnPrint U; % handle case of !*Defn:=T
%
% Looks for special action on a form, otherwise prettyprints it;
% Adapted from DFPRINT
%
    if PairP U and FlagP(car U, 'Ignore) then DefnPrint1 U
    else				% So 'IGNORE is EVALED, not output
    <<  if DfPrint!* then Apply(DfPrint!*, list U)
	else PrettyPrint U;		% So 'EVAL gets EVALED and Output
	if PairP U and FlagP(car U, 'Eval) then DefnPrint1 U >>;

lisp procedure DefnPrint1 U;
    ErrorSet(list('Apply, MkQuote TopLoopEval!*,
			  MkQuote list U),
	     T,
	     !*Backtrace);

fluid '(!*Break);

lisp procedure NthEntry N;
begin scalar !*Break;
    return if IGEQ(N, HistoryCount!*) then
	StdError BldMsg("No history entry %r", N)
    else car PNth(cdr HistoryList!*, IDifference(HistoryCount!*, N));
end;

lisp procedure Inp N;			%. Return Nth input
    car NthEntry N;

expr procedure ReDo N;			%. Re-evaluate Nth input
    Apply(TopLoopEval!*, list car NthEntry N);

lisp procedure Ans N;			%. return Nth output
    cdr NthEntry N;

nexpr procedure Hist AL;		%. Print history entries
begin scalar I1, I2, L;
    if ILessP(HistoryCount!*, 2) then return NIL;
    I1 := 1;
    I2 := ISub1 HistoryCount!*;
    if PairP AL then
    <<  if car AL = 'CLEAR then
	<<  HistoryCount!* := 1;
	    HistoryList!* := NIL . NIL;
	    return NIL >>;
	if IMinusP car AL then return
	    HistPrint(cdr HistoryList!*,
		      ISub1 HistoryCount!*,
		      IMinus car AL);
	I1 := Max(I1, car AL);
	AL := cdr AL >>;
    if PairP AL then I2 := Min(I2, car AL);
    return HistPrint(PNTH(cdr HistoryList!*,
			  IDifference(HistoryCount!*, I2)),
		     I2,
		     IAdd1 IDifference(I2, I1));
end;

lisp procedure HistPrint(L, N, M);
    if IZeroP M then NIL else
    <<  HistPrint(cdr L, ISub1 N, ISub1 M);
	PrintF("%w	Inp: %p%n	Ans: %p%n",
		N,	  car first L,   cdr first L) >>;

lisp procedure Time();			%. Get run-time in milliseconds
    Sys2Int TimC();			% TimC is primitive runtime function

lisp procedure StandardLisp();		%. Lisp top loop
(lambda (CurrentReadMacroIndicator!*, CurrentScanTable!*);
    TopLoop('READ, 'PrintWithFreshLine, 'EVAL, "lisp", LispBanner!*)
    )('LispReadMacro, LispScanTable!*);

lisp procedure PrintWithFreshLine X;
    PrintF("%f%p%n", X);

lisp procedure SaveSystem(Banner, File, InitForms);
begin scalar SavedHistoryList, SavedHistoryCount;
    SavedHistoryCount := HistoryCount!*;
    SavedHistoryList := HistoryList!*;
    HistoryList!* := NIL;
    HistoryCount!* := 0;
    LispBanner!* := BldMsg("%w, %w", Banner, Date());
    !*UserMode := T;
    InitForms!* := InitForms;
    DumpLisp File;
    InitForms!* := NIL;
    HistoryCount!* := SavedHistoryCount;
    HistoryList!* := SavedHistoryList;
end;

lisp procedure EvalInitForms();		%. Evaluate and clear InitForms!*
<<  for each X in InitForms!* do Eval X;
    InitForms!* := NIL >>;

END;
