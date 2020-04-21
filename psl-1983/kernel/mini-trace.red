%
% MINI-TRACE.RED - Simple trace and BreakFn package
%
% Author:      Martin Griss and Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        18 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.INTERP>MINI-TRACE.RED.4,  3-May-82 11:26:12, Edit by BENSON
%  Bug fix in BR.PRC, changed VV to MkQuote VV
% Non-Standard Lisp functions used:
% PrintF, ErrorPrintF, BldMsg, EqCar, Atsoc, MkQuote, SubSeq

% -------- Simple TRACE package -----------

fluid '(ArgLst!*			% Default names for args in traced code
	TrSpace!*			% Number spaces to indent
	!*NoTrArgs			% Control arg-trace
);

CompileTime flag('(TrMakeArgList), 'InternalFunction);

lisp procedure Tr!.Prc(PN, B, A); 	% Called in place of Traced code
%
% Called by TRACE for proc nam PN, body B, args A;
%
begin scalar K, SvArgs, VV, Numb;
    TrSpace!* := TrSpace!* + 1;
    Numb := Min(TrSpace!*, 15);
    Tab Numb;
    PrintF("%p %w:", PN, TrSpace!*);
    if not !*NoTrArgs then
    <<  SvArgs := A;
	K := 1;
	while SvArgs do
	<<  PrintF(" Arg%w:=%p, ", K, car SvArgs);
	    SvArgs := cdr SvArgs;
	    K := K + 1 >> >>;
    TerPri();
    VV := Apply(B, A);
    Tab Numb;
    PrintF("%p %w:=%p%n", PN, TrSpace!*, VV);
    TrSpace!* := TrSpace!* - 1;
    return VV
end;

fluid '(!*Comp !*RedefMSG PromptString!*);

lisp procedure Tr!.1 Nam; 		% Called To Trace a single function
begin scalar PN, X, Y, Bod, Args, N, OldIn, OldPrompt, !*Comp, !*RedefMSG;
    if not (Y:=GetD Nam) then
    <<  ErrorPrintF("*** %r is not a defined function and cannot be traced",
			Nam);
	return >>;
    PN := GenSym();
    PutD(PN, car Y, cdr Y);
    put(Nam, 'OldCod, Y . get(Nam, 'OldCod));
    if EqCar(cdr Y, 'LAMBDA) then Args := cadr cdr Y else
    <<  OldPrompt := PromptString!*;
	PromptString!* := BldMsg("How many arguments for %r?", Nam);
	OldIn := RDS NIL;
	while not NumberP(N := Read()) or N < 0 or N > 15 do ;
	PromptString!* := OldPrompt;
	RDS OldIn;
	Args := TrMakeArgList N >>;
    Bod:= list('LAMBDA, Args,
			list('Tr!.prc, MkQuote Nam,
				       MkQuote PN, 'LIST . Args));
    PutD(Nam, car Y, Bod);
    put(Nam, 'TraceCode, cdr GetD Nam);
end;

lisp procedure UnTr!.1 Nam;
begin scalar X, Y, !*Comp;
    if not IDP Nam or not PairP(X := get(Nam, 'OldCod))
	    or not PairP(Y := GetD Nam)
	    or not (cdr Y eq get(Nam, 'TraceCode)) then
    <<  ErrorPrintF("*** %r cannot be untraced", Nam);
	return >>;
    PutD(Nam, caar X, cdar X);
    put(Nam, 'OldCod, cdr X)
end;

macro procedure TR L;			%. Trace functions in L
    list('EvTR, MkQuote cdr L);

expr procedure EvTR L;
    for each X in L do Tr!.1 X;

macro procedure UnTr L;			%. Untrace Function in L
    list('EvUnTr, MkQuote cdr L);

expr procedure EvUnTr L;
    for each X in L do UnTr!.1 X;

lisp procedure TrMakeArgList N;		% Get Arglist for N args
    cdr Assoc(N, ArgLst!*);

lisp procedure TrClr();			%. Called to setup or fix trace
<<  TrSpace!* := 0;
    !*NoTrArgs := NIL >>;

LoadTime
<<  ArgLst!* := '((0 . ())
		  (1 . (X1))
		  (2 . (X1 X2))
		  (3 . (X1 X2 X3))
		  (4 . (X1 X2 X3 X4))
		  (5 . (X1 X2 X3 X4 X5))
		  (6 . (X1 X2 X3 X4 X5 X6))
		  (7 . (X1 X2 X3 X4 X5 X6 X7))
		  (8 . (X1 X2 X3 X4 X5 X6 X7 X8))
		  (9 . (X1 X2 X3 X4 X5 X6 X7 X8 X9))
		  (10 . (X1 X2 X3 X4 X5 X6 X7 X8 X9 X10))
		  (11 . (X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11))
		  (12 . (X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12))
		  (13 . (X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12 X13))
		  (14 . (X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12 X13 X14))
		  (15 . (X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12 X13 X14 X15)));
    TrSpace!* := 0;
    !*NoTrArgs := NIL >>;

Fluid '(ErrorForm!* !*ContinuableError);

lisp procedure Br!.Prc(PN, B, A); 	% Called in place of "Broken" code
%
% Called by BREAKFN for proc nam PN, body B, args A;
%
begin scalar K, SvArgs, VV, Numb;
    TrSpace!* := TrSpace!* + 1;
    Numb := Min(TrSpace!*, 15);
    Tab Numb;
    PrintF("%p %w:", PN, TrSpace!*);
    if not !*NoTrArgs then
    <<  SvArgs := A;
	K := 1;
	while SvArgs do
	<<  PrintF(" Arg%w:=%p, ", K, car SvArgs);
	    SvArgs := cdr SvArgs;
	    K := K + 1 >> >>;
    TerPri();
    ErrorForm!* := NIL;
    PrintF(" BREAK before entering %r%n",PN);
    !*ContinuableError:=T;
    Break();
    VV := Apply(B, A);
    PrintF(" BREAK after call %r, value %r%n",PN,VV);
    ErrorForm!* := MkQuote VV;
    !*ContinuableError:=T;
    Break();
    Tab Numb;
    PrintF("%p %w:=%p%n", PN, TrSpace!*, ErrorForm!*);
    TrSpace!* := TrSpace!* - 1;
    return ErrorForm!*
end;

fluid '(!*Comp PromptString!*);

lisp procedure Br!.1 Nam; 		% Called To Trace a single function
begin scalar PN, X, Y, Bod, Args, N, OldIn, OldPrompt, !*Comp;
    if not (Y:=GetD Nam) then
    <<  ErrorPrintF("*** %r is not a defined function and cannot be BROKEN",
			Nam);
	return >>;
    PN := GenSym();
    PutD(PN, car Y, cdr Y);
    put(Nam, 'OldCod, Y . get(Nam, 'OldCod));
    if EqCar(cdr Y, 'LAMBDA) then Args := cadr cdr Y else
    <<  OldPrompt := PromptString!*;
	PromptString!* := BldMsg("How many arguments for %r?", Nam);
	OldIn := RDS NIL;
	while not NumberP(N := Read()) or N < 0 or N > 15 do ;
	PromptString!* := OldPrompt;
	RDS OldIn;
	Args := TrMakeArgList N >>;
    Bod:= list('LAMBDA, Args,
			list('Br!.prc, MkQuote Nam,
				       MkQuote PN, 'LIST . Args));
    PutD(Nam, car Y, Bod);
    put(Nam, 'BreakCode, cdr GetD Nam);
end;

lisp procedure UnBr!.1 Nam;
begin scalar X, Y, !*Comp;
   if not IDP Nam or not PairP(X := get(Nam, 'OldCod))
	    or not PairP(Y := GetD Nam)
	    or not (cdr Y eq get(Nam, 'BreakCode)) then
    <<  ErrorPrintF("*** %r cannot be unbroken", Nam);
	return >>;
    PutD(Nam, caar X, cdar X);
    put(Nam, 'OldCod, cdr X)
end;

macro procedure Br L;			%. Break functions in L
    list('EvBr, MkQuote cdr L);

expr procedure EvBr L;
    for each X in L do Br!.1 X;

macro procedure UnBr L;			%. Unbreak functions in L
    list('EvUnBr, MkQuote cdr L);

expr procedure EvUnBr L;
    for each X in L do UnBr!.1 X;

END;
