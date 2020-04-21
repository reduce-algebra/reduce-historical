%
% PROPERTY-LIST.RED - Functions dealing with property lists
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        17 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.INTERP>PROPERTY-LIST.RED.11,  1-Mar-82 14:09:20, Edit by BENSON
%  Changed "move-to-front" to "exchange-with-previous"
%  <PSL.INTERP>PROPERTY-LIST.RED.7, 27-Feb-82 12:43:27, Edit by BENSON
%  Optimized GET and FLAGP, rearranges property list

% Every ID in the system has a property list.  It is obtained by the function
% PROP(ID) and updated with the function SETPROP(ID, PLIST).  These functions
% are not in the Standard Lisp report, and are not intended for use in user
% programs.  A property list (whose format should also not be known to
% user programs) is a list of IDs and dotted pairs (A-List entries).  The
% pairs are used by PUT and GET, and the IDs are used by FLAG and FLAGP.

% Non-Standard Lisp functions used:
% DELQIP -- EQ, destructive version of Delete	(in EASY-NON-SL.RED)
% ATSOC -- EQ version of ASSOC	(in EASY-NON-SL.RED)
% DELATQIP -- EQ, destructive version of DELASC (in EASY-NON-SL.RED)
% EQCAR(A,B) -- PairP A and car A eq B (in EASY-NON-SL.RED)
% NonIDError -- in TYPE-ERRORS.RED

on SysLisp;

syslsp procedure Prop U;		%. Access property list of U
    if IDP U then SymPrp IDInf U
    else NonIDError(U, 'Prop);

syslsp procedure SetProp(U, L);		%. Store L as property list of U
    if IDP U then
	SymPrp IDInf U := L
    else
	NonIDError(U, 'SetProp);

syslsp procedure FlagP(U, Indicator); 	%. Is U marked with Indicator?
    if not IDP U or not IDP Indicator then NIL
    else begin scalar PL, PreviousPointer;
	PL := SymPrp IDInf U;
	if null PL then return NIL;
	if car PL eq Indicator then return T;
	PreviousPointer := PL;
	PL := cdr PL;
Loop:
	if null PL then return NIL;
	if car PL eq Indicator then return
	<<  Rplaca(PL, car PreviousPointer);
	    Rplaca(PreviousPointer, Indicator);
	    T >>;
	PreviousPointer := PL;
	PL := cdr PL;
	goto Loop;
    end;

on FastLinks;

syslsp procedure GetFnType U;
    get(U, 'TYPE);

off FastLinks;

syslsp procedure Get(U, Indicator); %. Retrieve value stored for U with Ind
    if not IDP U or not IDP Indicator then NIL
    else begin scalar PL, X, PreviousPointer;
	PL := SymPrp IDInf U;
	if null PL then return NIL;
	X := car PL;
	if PairP X and car X eq Indicator then return cdr X;
	PreviousPointer := PL;
	PL := cdr PL;
Loop:
	if null PL then return NIL;
	X := car PL;
	if PairP X and car X eq Indicator then return
	<<  Rplaca(PL, car PreviousPointer);
	    Rplaca(PreviousPointer, X);
	    cdr X >>;
	PreviousPointer := PL;
	PL := cdr PL;
	goto Loop;
    end;

off SysLisp;

lisp procedure Flag(IDList, Indicator);	%. Mark all in IDList with Indicator
    if not IDP Indicator then
	NonIDError(Indicator, 'Flag)
    else
	for each U in IDList do Flag1(U, Indicator);

lisp procedure Flag1(U, Indicator);
    if not IDP U then
	NonIDError(U, 'Flag)
    else begin scalar PL;
	PL := Prop U;
	if not (Indicator memq PL) then SetProp(U, Indicator . PL);
    end;

lisp procedure RemFlag(IDList, Indicator); %. Remove marking of all in IDList
    if not IDP Indicator then
	NonIDError(Indicator, 'RemFlag)
    else
	for each U in IDList do RemFlag1(U, Indicator);

lisp procedure RemFlag1(U, Indicator);
    if not IDP U then
	NonIDError(U, 'RemFlag)
    else SetProp(U, DelQIP(Indicator, Prop U));


lisp procedure Put(U, Indicator, Val);	%. Store Val in U with Indicator
    if not IDP U then
	NonIDError(U, 'Put)
    else if not IDP Indicator then
	NonIDError(Indicator, 'Put)
    else begin scalar PL, V;
	PL := Prop U;
	if not (V := Atsoc(Indicator, PL)) then
	    SetProp(U, (Indicator . Val) . PL)
	else
	    RPlacD(V, Val);
	return Val;
    end;

lisp procedure RemProp(U, Indicator);	%. Remove value of U with Indicator
    if not IDP U or not IDP Indicator then NIL
    else begin scalar V;
	if (V := get(U, Indicator)) then
	    SetProp(U, DelAtQIP(Indicator, Prop U));
	return V;
    end;


lisp procedure RemPropL(L, Indicator);	%. RemProp for all IDs in L
    for each X in L do RemProp(X, Indicator);

END;
