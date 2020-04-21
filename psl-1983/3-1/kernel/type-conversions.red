%
% TYPE-CONVERSIONS.RED - Functions for converting between various data types
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        28 August 1981
% Copyright (c) 1981 University of Utah

%  <PSL.VAX-INTERP>TYPE-CONVERSIONS.RED.2, 20-Jan-82 02:10:24, Edit by GRISS
%  Fix list2vector for NIL case

% The functions in this file are named `argument-type'2`result-type'.
% The number 2 is used rather than `To' only for compatibility with old
% versions.  Any other suggestions for a consistent naming scheme are welcomed.
% Perhaps they should also be `result-type'From`argument-type'.

% Float and Fix are in ARITH.RED

CompileTime flag('(Sys2FIXN), 'InternalFunction);

on SysLisp;

syslsp procedure ID2Int U;		%. Return ID index as Lisp number
    if IDP U then MkINT IDInf U
    else NonIDError(U, 'ID2Int);

syslsp procedure Int2ID U;		%. Return ID corresponding to index
begin scalar StripU;
    return if IntP U then
    <<  StripU := IntInf U;
	if StripU >= 0 then MkID StripU
	else TypeError(U, 'Int2ID, '"positive integer") >>
    else NonIntegerError(U, 'Int2ID);
end;

syslsp procedure Int2Sys N;		%. Convert Lisp integer to untagged
    if IntP N then IntInf N
    else if FixNP N then FixVal FixInf N
    else NonIntegerError(N, 'Int2Sys);

syslsp procedure Lisp2Char U;		%. Convert Lisp item to syslsp char
begin scalar C;				% integers, IDs and strings are legal
    return if IntP U and (C := IntInf U) >= 0 and C <= 127 then C
    else if IDP U then			% take first char of ID print name
	StrByt(StrInf SymNam IDInf U, 0)
    else if StringP U then
	StrByt(StrInf U, 0)	% take first character of Lisp string
    else NonCharacterError(U, 'Lisp2Char);
end;

syslsp procedure Int2Code N;		%. Convert Lisp integer to code pointer
    MkCODE N;

syslsp procedure Sys2Int N;		%. Convert word to Lisp number
    if SignedField(N, InfStartingBit - 1, InfBitLength + 1) eq N then N
    else Sys2FIXN N;

syslsp procedure Sys2FIXN N;
begin scalar FX;
    FX := GtFIXN();
    FixVal FX := N;
    return MkFIXN FX;
end;

syslsp procedure ID2String U;		%. Return print name of U (not copy)
    if IDP U then SymNam IDInf U
    else NonIDError(U, 'ID2String);

% The functions for converting strings to IDs are Intern and NewID.  Intern
% returns an interned ID, NewID returns an uninterned ID. They are both found
% in OBLIST.RED

syslsp procedure String2Vector U;	%. Make vector of ASCII values in U
    if StringP U then begin scalar StripU, V, N;
	N := StrLen StrInf U;
	V := GtVECT N;
	StripU := StrInf U;			% in case GC occurred
	for I := 0 step 1 until N do
	    VecItm(V, I) := MkINT StrByt(StripU, I);
	return MkVEC V;
    end else NonStringError(U, 'String2Vector);

syslsp procedure Vector2String V;	%. Make string with ASCII values in V
    if VectorP V then begin scalar StripV, S, N, Ch;
	N := VecLen VecInf V;
	S := GtSTR N;
	StripV := VecInf V;			% in case GC occurred
	for I := 0 step 1 until N do
	    StrByt(S, I) := Lisp2Char VecItm(StripV, I);
	return MkSTR S;
    end else NonVectorError(V, 'Vector2String);

syslsp procedure List2String P;		%. Make string with ASCII values in P
    if null P then '""
    else if PairP P then begin scalar S, N;
	N := IntInf Length P - 1;
	S := GtSTR N;
	for I := 0 step 1 until N do
	<<  StrByt(S, I) := Lisp2Char car P;
	    P := cdr P >>;
	return MkSTR S;
    end else NonPairError(P, 'List2String);

syslsp procedure String2List S;		%. Make list with ASCII values in S
    if StringP S then begin scalar L, N;
	L := NIL;
	N := StrLen StrInf S;
	for I := N step -1 until 0 do
	    L := MkINT StrByt(StrInf S, I) . L;	% strip S each time in case GC
	return L;
    end else NonStringError(S, 'String2List);

syslsp procedure List2Vector L;			%. convert list to vector
    if PairP L or NULL L then begin scalar V, N;% this function is used by READ
	N := IntInf Length L - 1;
	V := GtVECT N;
	for I := 0 step 1 until N do
	<<  VecItm(V, I) := car L;
	    L := cdr L >>;
	return MkVEC V;
    end else NonPairError(L, 'List2Vector);

syslsp procedure Vector2List V;		%. Convert vector to list
    if VectorP V then begin scalar L, N;
	L := NIL;
	N := VecLen VecInf V;
	for I := N step -1 until 0 do
	    L := VecItm(VecInf V, I) . L;	% strip V each time in case GC
	return L;
    end else NonVectorError(V, 'Vector2List);

off SysLisp;

END;
