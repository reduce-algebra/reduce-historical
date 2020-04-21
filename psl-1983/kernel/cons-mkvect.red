%
% CONS-MKVECT.RED - Standard Lisp constructor functions
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        20 August 1981
% Copyright (c) 1981 University of Utah
%

% Edit by Cris Perdue, 23 Feb 1983 1045-PST
% Changed occurrences of HeapUpperbound to HeapTrapBound in optimized
% allocators to supported pre-GC traps.
%  <PSL.KERNEL>CONS-MKVECT.RED.2, 10-Jan-83 15:50:08, Edit by PERDUE
%  Added MkEVect
% Edit by GRISS: (?)
% Optimized CONS, XCONS and NCONS
%  <PSL.INTERP>CONS-MKVECT.RED.5,  9-Feb-82 06:25:51, Edit by GRISS
%  Added HardCons

CompileTime flag('(HardCons), 'InternalFunction);

on SysLisp;

external WVar HeapLast, HeapTrapBound;

syslsp procedure HardCons(U, V);	% Basic CONS with car U and cdr V
begin scalar P;
    HeapLast := HeapLast - AddressingUnitsPerItem*PairPack();
    P := GtHeap PairPack();
    P[0] := U;
    P[1] := V;
    return MkPAIR P;
end;

syslsp procedure Cons(U, V);		%. Construct pair with car U and cdr V
begin scalar HP;
return
<<  HP := HeapLast;
    if (HeapLast := HeapLast + AddressingUnitsPerItem*PairPack())
		> HeapTrapBound then
	HardCons(U, V)
    else
    <<  HP[0] := U;
	HP[1] := V;
	MkPAIR HP >> >>;
end;

syslsp procedure XCons(U, V);		%. eXchanged Cons
begin scalar HP;
return
<<  HP := HeapLast;
    if (HeapLast := HeapLast + AddressingUnitsPerItem*PairPack())
		> HeapTrapBound then
	HardCons(V, U)
    else
    <<  HP[0] := V;
	HP[1] := U;
	MkPAIR HP >> >>;
end;

syslsp procedure NCons U;		%. U . NIL
begin scalar HP;
return
<<  HP := HeapLast;
    if (HeapLast := HeapLast + AddressingUnitsPerItem*PairPack())
		> HeapTrapBound then
	HardCons(U, NIL)
    else
    <<  HP[0] := U;
	HP[1] := NIL;
	MkPAIR HP >> >>;
end;

syslsp procedure MkVect N;		%. Allocate vector, init all to NIL
    if IntP N then
    <<  N := IntInf N;
	if N < (-1) then
	    StdError
		'"A vector with fewer than zero elements cannot be allocated"
	else begin scalar V;
	    V := GtVect N;
	    for I := 0 step 1 until N do VecItm(V, I) := NIL;
	    return MkVEC V;		% Tag it
	end >>
    else NonIntegerError(N, 'MkVect);

syslsp procedure MkEVECTOR(N,ETAG);      %. Allocate Evect, init all to NIL
    if IntP N then
    <<  N := IntInf N;
        if N < (-1) then
            StdError
                '"An  Evect with fewer than zero elements cannot be allocated"
        else begin scalar V;
            V := GtEVect N;
            EVecItm(V,0):=ETAG;
            for I := 1 step 1 until N do VecItm(V, I) := NIL;
            return MkEVECT V;            % Tag it
        end >>
    else NonIntegerError(N, 'MkEVECT);

off SysLisp;

END;
