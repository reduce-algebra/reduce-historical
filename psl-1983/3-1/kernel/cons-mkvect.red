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

%  <PSL.KERNEL>CONS-MKVECT.RED.4, 28-Feb-83 11:41:46, Edit by PERDUE
%  Moved Make-Words, Make-Halfwords, etc. here from SEQUENCE.RED
%  Also moved STRING and VECTOR here from there.
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

syslsp procedure MkString(L, C); %. Make str with upb L, all chars C
begin scalar L1, S;
    if IntP L then L1 := IntInf L else return NonIntegerError(L, 'MkString);
    if L1 < -1 then return NonPositiveIntegerError(L, 'MkString);
    S := GtStr L1;
    for I := 0 step 1 until L1 do
	StrByt(S, I) := C;
    return MkSTR S;
end;

syslsp procedure Make!-Bytes(L, C); %. Make byte vector with upb L, all items C
begin scalar L1, S;
    if IntP L then L1 := IntInf L else return NonIntegerError(L, 'Make!-Bytes);
    if L1 < -1 then return NonPositiveIntegerError(L, 'Make!-Bytes);
    S := GtStr L1;
    for I := 0 step 1 until L1 do
	StrByt(S, I) := C;
    return MkBytes S;
end;

syslsp procedure Make!-HalfWords(L, C); %. Make h vect with upb L, all items C
begin scalar L1, S;
    if IntP L then L1 := IntInf L else
	return NonIntegerError(L, 'Make!-HalfWords);
    if L1 < -1 then return NonPositiveIntegerError(L, 'Make!-HalfWords);
    S := GtHalfWords L1;
    for I := 0 step 1 until L1 do
	HalfWordItm(S, I) := C;
    return MkHalfWords S;
end;

syslsp procedure Make!-Words(L, C); %. Make w vect with upb L, all items C
begin scalar L1, S;
    if IntP L then L1 := IntInf L else return NonIntegerError(L, 'Make!-Words);
    if L1 < -1 then return NonPositiveIntegerError(L, 'Make!-Words);
    S := GtWrds L1;
    for I := 0 step 1 until L1 do
	WrdItm(S, I) := C;
    return MkWrds S;
end;

syslsp procedure Make!-Vector(L, C); %. Make vect with upb L, all items C
begin scalar L1, S;
    if IntP L then L1 := IntInf L else return
	NonIntegerError(L, 'Make!-Vector);
    if L1 < -1 then return NonPositiveIntegerError(L, 'Make!-Vector);
    S := GtVECT L1;
    for I := 0 step 1 until L1 do
	VecItm(S, I) := C;
    return MkVEC S;
end;

% Maybe we want to support efficient compilation of these, as with LIST,
% by functions String2, String3, Vector2, Vector3, etc.

nexpr procedure String U;	%. Analogous to LIST, string constructor
    List2String U;

nexpr procedure Vector U;	%. Analogous to LIST, vector constructor
    List2Vector U;

off SysLisp;

END;
