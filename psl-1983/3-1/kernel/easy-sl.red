%
% EASY-SL.RED - Standard Lisp functions with easy Standard Lisp definitions
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        17 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL>EASY-SL.RED.3, 17-Sep-82 16:16:58, Edit by BENSON
%  Added ChannelPrint
%  <PSL.INTERP>EASY-SL.RED.4, 13-Aug-82 14:14:49, Edit by BENSON
%  Changed nice recursive Append to ugly iterative definition
%  <PSL.INTERP>EASY-SL.RED.13,  8-Feb-82 17:43:07, Edit by BENSON
%  Made SetQ take multiple arguments
%  <PSL.INTERP>EASY-SL.RED.7, 18-Jan-82 17:30:14, Edit by BENSON
%  Added Max2 and Min2
%  <PSL.INTERP>EASY-SL.RED.6, 15-Jan-82 14:54:36, Edit by BENSON
%  Changed DE, DF, DM, DN from Fexprs to Macros

% This file contains only functions found in the Standard Lisp report which
% can be easily and efficiently defined in terms of other Standard Lisp
% functions.  It does not include primitive functions which are handled
% specially by the compiler, such as EQ.

% Many NULL tests in these functions have been replaced with not PairP tests,
% so that they will be safer.

CompileTime flag('(EvAnd1), 'InternalFunction);

% Section 3.1 -- Elementary predicates

lisp procedure Atom U;			%. is U a non pair?
    not PairP U;

lisp procedure ConstantP U;		%. is Eval U eq U by definition?
    not PairP U and not IDP U;

lisp procedure Null U;			%. is U eq NIL?
    U eq NIL;

lisp procedure NumberP U;		%. is U a number of any kind?
    FixP U or FloatP U;

lisp procedure Expt(X, N);
begin scalar Result;
    if not IntP N or not NumberP X then return
	ContError(99, "Illegal arguments to Expt", X ** N);
    Result := 1;
    if N > 0 then
	for I := 1 step 1 until N do Result := Result * X
    else if N < 0 then
	for I := -1 step -1 until N do Result := Result / X;
    return Result;
end;

% MinusP, OneP and ZeroP are in ARITHMETIC.RED
% FixP is defined in OTHERS-SL.RED

% Section 3.2 -- Functions on Dotted-Pairs

% composites of CAR and CDR are found in CARCDR.RED

fexpr procedure List U;			%. construct list of arguments
    EvLis U;


% section 3.5 -- Function definition

macro procedure DE U;			%. Terse syntax for PutD call for EXPR
    list('PutD, MkQuote cadr U,
		'(QUOTE EXPR),
		list('FUNCTION, ('LAMBDA . cddr U)));

macro procedure DF U;			%. Terse syntax for PutD call for FEXPR
    list('PutD, MkQuote cadr U,
		'(QUOTE FEXPR),
		list('FUNCTION, ('LAMBDA . cddr U)));

macro procedure DM U;			%. Terse syntax for PutD call for MACRO
    list('PutD, MkQuote cadr U,
		'(QUOTE MACRO),
		list('FUNCTION, ('LAMBDA . cddr U)));

macro procedure DN U;			%. Terse syntax for PutD call for NEXPR
    list('PutD, MkQuote cadr U,
		'(QUOTE NEXPR),
		list('FUNCTION, ('LAMBDA . cddr U)));


% Section 3.6 -- Variables and bindings

fexpr procedure SetQ U;			%. Standard named variable assignment
%
% Extended from SL Report to be Common Lisp compatible
% (setq foo 1 bar 2 ...) is permitted
%
begin scalar V, W;
    while U do
    <<  W := cdr U;
	Set(car U, V := Eval car W);
	U := cdr W >>;
    return V;
end;

% Section 3.7 -- Program feature functions

lisp procedure Prog2(U, V);		%. Return second argument
    V;

fexpr procedure ProgN U;		%. Sequential evaluation, return last
    EvProgN U;

StartupTime put('PROGN, 'TYPE, 'FEXPR);

lisp procedure EvProgN U;		%. EXPR support for ProgN, Eval, Cond
    if PairP U then
    <<  while PairP cdr U do
	<<  Eval car U;
	    U := cdr U >>;
	Eval car U >>
    else NIL;

% Section 3.10 -- Boolean functions and conditionals

fexpr procedure And U;			%. Sequentially evaluate until NIL
    EvAnd U;

lisp procedure EvAnd U;			%. EXPR support for And
    if not PairP U then T else EvAnd1 U;

lisp procedure EvAnd1 U;		% Auxiliary function for EvAnd
    if not PairP cdr U then Eval car U
    else if not Eval car U then NIL
    else EvAnd1 cdr U;

fexpr procedure OR U;			%. sequentially evaluate until non-NIL
    EvOr U;

lisp procedure EvOr U;			%. EXPR support for Or
    PairP U and (Eval car U or EvOr cdr U);

fexpr procedure Cond U;			%. Conditional evaluation construct
    EvCond U;

lisp procedure EvCond U;		%. EXPR support for Cond
%
% Extended from Standard Lisp definition to allow no consequent (antecedent is
% returned), or multiple consequent (implicit progn).
%
begin scalar CondForm, Antecedent, Result;
    return if not PairP U then NIL
    else
    <<  CondForm := car U;
	U := cdr U;
	Antecedent := if PairP CondForm then car CondForm else CondForm;
	if not (Result := Eval Antecedent) then
	    EvCond U
	else if not PairP CondForm or not PairP cdr CondForm then
	    Result
	else
	    EvProgN cdr CondForm >>;
end;

lisp procedure Not U;			%. Equivalent to NULL
    null U;


% Section 3.11 -- Arithmetic functions

lisp procedure Abs U;			%. Absolute value of number
    if MinusP U then -U else U;

lisp procedure Divide(U, V);		%. dotted pair remainder and quotient
    if ZeroP V then
	ContError(99, "Attempt to divide by 0 in DIVIDE", Divide(U, V))
    else
	Quotient(U, V) . Remainder(U, V);

macro procedure Max U;			%. numeric maximum of several arguments
    RobustExpand(cdr U, 'Max2, 0);	% should probably be -infinity

lisp procedure Max2(U, V);		%. maximum of 2 arguments
    if U < V then V else U;

macro procedure Min U;			%. numeric minimum of several arguments
    RobustExpand(cdr U, 'Min2, 0);	% should probably be +infinity

lisp procedure Min2(U, V);		%. minimum of 2 arguments
    if U > V then V else U;

macro procedure Plus U;			%. addition of several arguments
    RobustExpand(cdr U, 'Plus2, 0);

macro procedure Times U;		%. multiplication of several arguments
    RobustExpand(cdr U, 'Times2, 1);


% Section 3.12 -- MAP Composite functions

lisp procedure Map(L, Fn);		%. for each X on L do Fn(X);
    while PairP L do
    <<  Apply(Fn, list L);
	L := cdr L >>;

lisp procedure MapC(L, Fn);		%. for each X in L do Fn(X);
    while PairP L do
    <<  Apply(Fn, list car L);
	L := cdr L >>;

lisp procedure MapCan(L, Fn);		%. for each X in L conc Fn(X);
    if not PairP L then NIL
    else NConc(Apply(Fn, list car L), MapCan(cdr L, Fn));

lisp procedure MapCon(L, Fn);		%. for each X on L conc Fn(X);
    if not PairP L then NIL
    else NConc(Apply(Fn, list L), MapCon(cdr L, Fn));

lisp procedure MapCar(L, Fn);		%. for each X in L collect Fn(X);
    if not PairP L then NIL
    else Apply(Fn, list car L) . MapCar(cdr L, Fn);

lisp procedure MapList(L, Fn);		%. for each X on L collect Fn(X);
    if not PairP L then NIL
    else Apply(Fn, list L) . MapList(cdr L, Fn);


% Section 3.13 -- Composite functions

lisp procedure Append(U, V);		%. Combine 2 lists
    if not PairP U then V else begin scalar U1, U2;
	U1 := U2 := car U . NIL;
	U := cdr U;
	while PairP U do
	<<  RplacD(U2, car U . NIL);
	    U := cdr U;
	    U2 := cdr U2 >>;
	RplacD(U2, V);
	return U1;
    end;

%
% These A-list functions differ from the Standard Lisp Report in that
% poorly formed A-lists (non-pair entries) are not signalled as an error,
% rather the entries are ignored.  This is because some data structures
% (such as property lists) use atom entries for other purposes.
%

lisp procedure Assoc(U, V);		%. Return first (U . xxx) in V, or NIL
    if not PairP V then NIL
    else if PairP car V and U = caar V then car V
    else Assoc(U, cdr V);

lisp procedure Sassoc(U, V, Fn);	%. Return first (U . xxx) in V, or Fn()
    if not PairP V then Apply(Fn, NIL)
    else if PairP car V and U = caar V then car V
    else Sassoc(U, cdr V, Fn);

lisp procedure Pair(U, V);		%. For each X,Y in U,V collect (X . Y)
    if PairP U and PairP V then (car U . car V) . Pair(cdr U, cdr V)
    else if PairP U or PairP V then
	StdError "Different length lists in PAIR"
    else NIL;

lisp procedure SubLis(X, Y);		%. Substitution in Y by A-list X
    if not PairP X then Y
    else begin scalar U;
	U := Assoc(Y, X);
	return if PairP U then cdr U
	else if not PairP Y then Y
	else SubLis(X, car Y) . SubLis(X, cdr Y);
    end;


lisp procedure DefList(DList, Indicator);	%. PUT many IDs, same indicator
    if not PairP DList then NIL else
    <<  put(caar DList, Indicator, cadar DList);
	caar DList >> . DefList(cdr DList, Indicator);

lisp procedure Delete(U, V);		%. Remove first top-level U in V
    if not PairP V then V
    else if car V = U then cdr V
    else car V . Delete(U, cdr V);

%  DIGIT, LENGTH and LITER are optimized, don't use SL Report version

lisp procedure Member(U, V);		%. Find U in V
    if not PairP V then NIL
    else if U = car V then V
    else U Member cdr V;

lisp procedure MemQ(U, V);		% EQ version of Member
    if not PairP V then NIL
    else if U eq car V then V
    else U MemQ cdr V;

lisp procedure NConc(U, V);		%. Destructive version of Append
begin scalar W;
    if not PairP U then return V;
    W := U;
    while PairP cdr W do W := cdr W;
    RplacD(W, V);
    return U;
end;

lisp procedure Reverse U;		%. Top-level reverse of list
begin scalar V;
    while PairP U do
    <<  V := car U . V;
	U := cdr U >>;
    return V;
end;

lisp procedure Subst(A, X, L);		%. Replace every X in L with A
    if null L then NIL
    else if X = L then A
    else if null PairP L then L
    else Subst(A, X, car L) . Subst(A, X, cdr L);

lisp procedure EvLis U;			%. For each X in U collect Eval X
    if not PairP U then NIL
    else Eval car U . EvLis cdr U;

lisp procedure RobustExpand(L, Fn, EmptyCase); %. Expand + arg for empty list
    if null L then EmptyCase else Expand(L, Fn);

lisp procedure Expand(L, Fn);		%. L = (a b c) --> (Fn a (Fn b c))
    if not PairP L then L
    else if not PairP cdr L then car L
    else list(Fn, car L, Expand(cdr L, Fn));

fexpr procedure Quote U;		%. Return unevaluated argument
    car U;

StartupTime put('QUOTE, 'TYPE, 'FEXPR);	% needed to run from scratch

fexpr procedure Function U;		%. Same as Quote in this version
    car U;


% Section 3.15 -- Input and Output

lisp procedure ChannelPrint(C, U);	%. Display U and terminate line
<<  ChannelPrin1(C, U);
    ChannelTerPri C;
    U >>;

lisp procedure Print U;			%. Display U and terminate line
    ChannelPrint(OUT!*, U);

End;
