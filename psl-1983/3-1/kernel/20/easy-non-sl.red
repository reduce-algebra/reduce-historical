%
% EASY-NON-SL.RED - Commonly used Non-Standard Lisp functions
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        18 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL>EASY-NON-SL.RED.2, 17-Sep-82 16:10:18, Edit by BENSON
%  Added ChannelPrin2T, ChannelSpaces, ChannelTab, ChannelSpaces2
%  <PSL.INTERP>EASY-NON-SL.RED.7,  9-Jul-82 12:46:43, Edit by BENSON
%  Changed NTH to improve error reporting, using DoPNTH
%  <PSL.INTERP>EASY-NON-SL.RED.2, 19-Apr-82 23:05:35, Edit by BENSON
%  Changed order of tests in PNTH
%  <PSL.INTERP>EASY-NON-SL.RED.20, 23-Feb-82 21:36:36, Edit by BENSON
%  Added NE (not eq)
%  <PSL.INTERP>EASY-NON-SL.RED.19, 16-Feb-82 22:30:33, Edit by BENSON
%  made NEQ GEQ and LEQ back into EXPRs
%  <PSL.INTERP>EASY-NON-SL.RED.16, 15-Feb-82 18:01:14, Edit by BENSON
%  Made NEQ GEQ and LEQ into macros
%  <PSL.INTERP>EASY-NON-SL.RED.12, 18-Jan-82 12:28:13, Edit by BENSON
%  Added NexprP

CompileTime flag('(DelqIP1 DeletIP1 SubstIP1 DelAscIP1 DelAtQIP1 DoPNTH),
		 'InternalFunction);

% predicates

expr procedure NEQ(U, V);	%. not EQUAL (should be changed to not EQ)
    not(U = V);

expr procedure NE(U, V);		%. not EQ
    not(U eq V);

expr procedure GEQ(U, V);		%. greater than or equal to
    not(U < V);

expr procedure LEQ(U, V);		%. less than or equal to
    not(U > V);

lisp procedure EqCar(U, V);		%. car U eq V
    PairP U and car U eq V;

lisp procedure ExprP U;			%. Is U an EXPR?
    EqCar(U, 'LAMBDA) or CodeP U or EqCar(GetD U, 'EXPR);

lisp procedure MacroP U;		%. Is U a MACRO?
    EqCar(GetD U, 'MACRO);

lisp procedure FexprP U;		%. Is U an FEXPR?
    EqCar(GetD U, 'FEXPR);

lisp procedure NexprP U;		%. Is U an NEXPR?
    EqCar(GetD U, 'NEXPR);

% Function definition

lisp procedure CopyD(New, Old);		%. FunDef New := FunDef Old;
%
% CopyD(New:id, Old:id):id
% -----------------------
% Type: EVAL, SPREAD
% The function body and type for New become the same as Old. If no
% definition exists for Old, the error
%
% ***** `Old' has no definition in CopyD
%
% occurs.  New is returned.
%
begin scalar OldDef;
    OldDef := GetD Old;
    if PairP OldDef then
	PutD(New, car OldDef, cdr OldDef)
    else
        StdError BldMsg("%r has no definition in CopyD", Old);
    return New;
end;

% Numerical functions

lisp procedure Recip N;			%. Floating point reciprocal
    1.0 / N;

% Commonly used constructors

lisp procedure MkQuote U;		%. Eval MkQuote U eq U
    list('QUOTE, U);


% Nicer names to access parts of a list

macro procedure First U;		%. First element of a list
    'CAR . cdr U;

macro procedure Second U;		%. Second element of a list
    'CADR . cdr U;

macro procedure Third U;		%. Third element of a list
    'CADDR . cdr U;

macro procedure Fourth U;		%. Fourth element of a list
    'CADDDR . cdr U;

macro procedure Rest U;			%. Tail of a list
    'CDR . cdr U;


% Destructive and EQ versions of Standard Lisp functions

lisp procedure ReversIP U;	%. Destructive REVERSE (REVERSe In Place)
begin scalar X,Y; 
    while PairP U do
    <<  X := cdr U;
	Y := RplacD(U, Y);
	U := X >>; 
    return Y
end;

lisp procedure SubstIP1(A, X, L);	% Auxiliary function for SubstIP
<<  if X = car L then RplacA(L, A)
    else if PairP car L then SubstIP(A, X, car L);
    if PairP cdr L then SubstIP(A, X, cdr L) >>;

lisp procedure SubstIP(A, X, L);	%. Destructive version of Subst
    if null L then NIL
    else if X = L then A
    else if not PairP L then L
    else
    <<  SubstIP1(A, X, L);
	L >>;

lisp procedure DeletIP1(U, V);		% Auxiliary function for DeletIP
    if PairP cdr V then
	if U = cadr V then RplacD(V, cddr V)
	else DeletIP1(U, cdr V);

lisp procedure DeletIP(U, V);		%. Destructive DELETE
    if not PairP V then V
    else if U = car V then cdr V
    else
    <<  DeletIP1(U, V);
	V >>;

lisp procedure DelQ(U, V);		%. EQ version of DELETE
    if not PairP V then V
    else if car V eq U then cdr V
    else car V . DelQ(U, cdr V);

lisp procedure Del(F, U, V); %. Generalized Delete, F is comparison function
    if not PairP V then V
    else if Apply(F, list(car V, U)) then cdr V
    else car V . Del(F, U, cdr V);

lisp procedure DelqIP1(U, V);		% Auxiliary function for DelqIP
    if PairP cdr V then
	if U eq cadr V then RplacD(V, cddr V)
	else DelqIP1(U, cdr V);

lisp procedure DelqIP(U, V);		%. Destructive DELQ
    if not PairP V then V
    else if U eq car V then cdr V
    else
    <<  DelqIP1(U, V);
	V >>;

lisp procedure Atsoc(U, V);		%. EQ version of ASSOC
    if not PairP V then NIL
    else if PairP car V and U eq caar V then car V
    else Atsoc(U, cdr V);

lisp procedure Ass(F, U, V); %. Generalized Assoc, F is comparison function
%
% Not to be confused with Elbow
%
    if not PairP V then NIL
    else if PairP car V and Apply(F, list(U, caar V)) then car V
    else Ass(F, U, cdr V);

lisp procedure Mem(F, U, V); %. Generalized Member, F is comparison function
    if not PairP V then NIL
    else if Apply(F, list(U, car V)) then V
    else Mem(F, U, cdr V);

lisp procedure RAssoc(U, V);	%. Reverse Assoc, compare with cdr of entry
    if not PairP V then NIL
    else if PairP car V and U = cdar V then car V
    else RAssoc(U, cdr V);

lisp procedure DelAsc(U, V);		%. Remove first (U . xxx) from V
    if not PairP V then NIL
    else if PairP car V and U = caar V then cdr V
    else car V . DelAsc(U, cdr V);

lisp procedure DelAscIP1(U, V);		% Auxiliary function for DelAscIP
    if PairP cdr V then
	if PairP cadr V and U = caadr V then
	    RplacD(V, cddr V)
	else DelAscIP1(U, cdr V);

lisp procedure DelAscIP(U, V);		%. Destructive DelAsc
    if not PairP V then NIL
    else if PairP car V and U = caar V then cdr V
    else
    <<  DelAscIP1(U, V);
	V >>;

lisp procedure DelAtQ(U, V);		%. EQ version of DELASC
   if not PairP V then NIL
   else if EqCar(car V, U) then cdr V
   else car V . DelAtQ(U, cdr V);

lisp procedure DelAtQIP1(U, V);		% Auxiliary function for DelAtQIP
    if PairP cdr V then
	if PairP cadr V and U eq caadr V then
	    RplacD(V, cddr V)
	else DelAtQIP1(U, cdr V);

lisp procedure DelAtQIP(U, V);		%. Destructive DelAtQ
    if not PairP V then NIL
    else if PairP car V and U eq caar V then cdr V
    else
    <<  DelAtQIP1(U, V);
	V >>;

lisp procedure SublA(U,V);	%. EQ version of SubLis, replaces atoms only
begin scalar X;
    return if not PairP U or null V then V
    else if atom V then
	if (X := Atsoc(V, U)) then cdr X else V
    else SublA(U, car V) . SublA(U, cdr V)
end;


lisp procedure RplacW(A, B);		%. RePLACe Whole pair
    if PairP A then
	if PairP B then
	    RplacA(RplacD(A,
			  cdr B),
		   car B)
	else
	    NonPairError(B, 'RplacW)
    else
	NonPairError(A, 'RPlacW);

lisp procedure LastCar X;		%. last element of list
    if atom X then X else car LastPair X;

lisp procedure LastPair X;		%. last pair of list
    if atom X or atom cdr X then X else LastPair cdr X;

lisp procedure Copy U;			%. copy all pairs in S-Expr
%
% See also TotalCopy in COPIERS.RED
%
    if PairP U then Copy car U . Copy cdr U else U;	% blows up if circular


lisp procedure NTH(U, N);		%. N-th element of list
(lambda(X);
    if PairP X then car X else RangeError(U, N, 'NTH))(DoPNTH(U, N));

lisp procedure DoPNTH(U, N);
    if N = 1 or not PairP U then U
    else DoPNTH(cdr U, N - 1);

lisp procedure PNTH(U, N);		%. Pointer to N-th element of list
    if N = 1 then U
    else if not PairP U then
	RangeError(U, N, 'PNTH)
    else PNTH(cdr U, N - 1);

lisp procedure AConc(U, V);	%. destructively add element V to the tail of U
    NConc(U, list V);

lisp procedure TConc(Ptr, Elem);	%. AConc maintaining pointer to end
%
% ACONC with pointer to end of list
% Ptr is (list . last CDR of list)
% returns updated Ptr
% Ptr should be initialized to (NIL . NIL) before calling the first time
%
<<  Elem := list Elem;
    if not PairP Ptr then	 % if PTR not initialized, return starting ptr
	Elem . Elem
    else if null cdr Ptr then	 % Nothing in the list yet
	RplacA(RplacD(Ptr, Elem), Elem)
    else
    <<  RplacD(cdr Ptr, Elem);
	RplacD(Ptr, Elem) >> >>;

lisp procedure LConc(Ptr, Lst);		%. NConc maintaining pointer to end
%
% NCONC with pointer to end of list
% Ptr is (list . last CDR of list)
% returns updated Ptr
% Ptr should be initialized to NIL . NIL before calling the first time
%
    if null Lst then Ptr
    else if atom Ptr then	 % if PTR not initialized, return starting ptr
	Lst . LastPair Lst
    else if null cdr Ptr then	 % Nothing in the list yet
	RplacA(RplacD(Ptr, LastPair Lst), Lst)
    else
    <<  RplacD(cdr Ptr, Lst);
	RplacD(Ptr, LastPair Lst) >>;


% MAP functions of 2 arguments

lisp procedure Map2(L, M, Fn);		%. for each X, Y on L, M do Fn(X, Y);
<<  while PairP L and PairP M do
    <<  Apply(Fn, list(L, M));
	L := cdr L;
	M := cdr M >>;
    if PairP L or PairP M then
	StdError "Different length lists in MAP2"
    else NIL >>;

lisp procedure MapC2(L, M, Fn);		%. for each X, Y in L, M do Fn(X, Y);
<<  while PairP L and PairP M do
    <<  Apply(Fn, list(car L, car M));
	L := cdr L;
	M := cdr M >>;
    if PairP L or PairP M then
	StdError "Different length lists in MAPC2"
    else NIL >>;

% Printing functions

lisp procedure ChannelPrin2T(C, U);		%. Prin2 and TerPri
<<  ChannelPrin2(C, U);
    ChannelTerPri C;
    U >>;

lisp procedure Prin2T U;		%. Prin2 and TerPri
    ChannelPrin2T(OUT!*, U);

lisp procedure ChannelSpaces(C, N);		%. Prin2 N spaces
   for I := 1 step 1 until N do ChannelWriteChar(C, char BLANK);

lisp procedure Spaces N;		%. Prin2 N spaces
    ChannelSpaces(OUT!*, N);

lisp procedure ChannelTAB(Chn, N);	%. Spaces to column N
begin scalar M;
    M := ChannelPosn Chn;
    if N < M then
    <<  ChannelTerPri Chn;
	M := 0 >>;
    ChannelSpaces(Chn, N - M);
end;

lisp procedure TAB N;			%. Spaces to column N
    ChannelTAB(OUT!*, N);

if_system(Dec20, <<
lap '((!*entry FileP expr 1)
	(!*MOVE (REG 1) (REG 2))
	(!*MkItem (reg 2) 8#66)         % make a byte pointer
	(hrlzi 1 2#001000000000000001)	% gj%old + gj%sht
	(gtjfn)
	 (jrst NotFile)
	(rljfn)				% release it
	(jfcl)
	(!*MOVE (QUOTE T) (REG 1))
	(!*EXIT 0)
NotFile
	(!*MOVE (QUOTE NIL) (REG 1))
	(!*EXIT 0)
); >>, <<
lisp procedure FileP F;			%. is F an existing file?
%
% This could be done more efficiently in a much more system-dependent way,
% but efficiency probably doesn't matter too much here.
%
    if PairP(F := ErrorSet(list('OPEN, MkQuote F, '(QUOTE INPUT)), NIL, NIL))
    then
    <<  Close car F;
	T >>
    else NIL; >>);

% This doesn't belong anywhere and will be eliminated soon

lisp procedure PutC(Name, Ind, Exp);	%. Used by RLISP to define SMACROs
<<  put(Name, Ind, Exp);
    Name >>;

LoadTime <<
    PutD('Spaces2, 'EXPR, cdr GetD 'TAB);	% For compatibility
    PutD('ChannelSpaces2, 'EXPR, cdr GetD 'ChannelTAB);
>>;

END;
