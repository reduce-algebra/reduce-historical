%
% SEQUENCE.RED - Useful functions on strings, vectors and lists
% 
% Author:      Martin Griss and Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        10 September 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL>SEQUENCE.RED.2, 25-Jan-83 16:11:28, Edit by PERDUE
%  Removed Make-String, etc., moved to cons-mkvect.red
%  STRINGS pkg defines Make-String (differently and Common LISP compatibly)
%  <PSL.INTERP>SEQUENCE.RED.2, 27-Feb-82 00:46:03, Edit by BENSON
%  Started adding more vector types
%  <PSL.INTERP>STRING-OPS.RED.11,  6-Jan-82 20:41:16, Edit by BENSON
%  Changed String and Vector into Nexprs

on SysLisp;

% Indexing operations

syslsp procedure Indx(R1, R2);		%. Element of sequence
begin scalar Tmp1, Tmp2;
    if not PosIntP R2 then return IndexError(R2, 'Indx);   % Subscript
    Tmp1 := Inf R1;
    Tmp2 := Tag R1;
    return case Tmp2 of
	Str, Bytes:
	    if R2 > StrLen Tmp1 then
		RangeError(R1, R2, 'Indx)
	    else StrByt(Tmp1, R2);
	Vect:
	    if R2 > VecLen Tmp1 then
		RangeError(R1, R2, 'Indx)
	    else VecItm(Tmp1, R2);
	Wrds:
	    if R2 > WrdLen Tmp1 then
		RangeError(R1, R2, 'Indx)
	    else WrdItm(Tmp1, R2);
	HalfWords:
	    if R2 > HalfWordLen Tmp1 then
		RangeError(R1, R2, 'Indx)
	    else HalfWordItm(Tmp1, R2);
	Pair:
	<<  Tmp2 := R2;
	    while Tmp2 > 0 do
	    <<  R1 := cdr R1;
		if atom R1 then RangeError(R1, R2, 'Indx);
		Tmp2 := Tmp2 - 1 >>;
	    car R1 >>;
	default:
	    NonSequenceError(R1, 'Indx);
    end;
end;

syslsp procedure SetIndx(R1, R2, R3);	%. Store at index of sequence
begin scalar Tmp1, Tmp2;
    if not PosIntP R2 then return IndexError(R2, 'SetIndx);   % Subscript
    Tmp1 := Inf R1;
    Tmp2 := Tag R1;
    return case Tmp2 of
	Str, Bytes:
	    if R2 > StrLen Tmp1 then
		RangeError(R1, R2, 'SetIndx)
	    else
	    <<  StrByt(Tmp1, R2) := R3;
		R3 >>;
	Vect:
	    if R2 > VecLen Tmp1 then
		RangeError(R1, R2, 'SetIndx)
	    else
	    <<  VecItm(Tmp1, R2) := R3;
		R3 >>;
	Wrds:
	    if R2 > WrdLen Tmp1 then
		RangeError(R1, R2, 'SetIndx)
	    else
	    <<  WrdItm(Tmp1, R2) := R3;
		R3 >>;
	HalfWords:
	    if R2 > HalfWordLen Tmp1 then
		RangeError(R1, R2, 'SetIndx)
	    else
	    <<  HalfWordItm(Tmp1, R2) := R3;
		R3 >>;
	Pair:
	<<  Tmp2 := R2;
	    while Tmp2 > 0 do
	    <<  R1 := cdr R1;
		if atom R1 then RangeError(R1, R2, 'SetIndx);
		Tmp2 := Tmp2 - 1 >>;
	    Rplaca(R1, R3);
	    R3 >>;
	default:
	    NonSequenceError(R1, 'SetIndx);
    end;
end;

% String and vector sub-part operations.

syslsp procedure Sub(R1, R2, R3);	%. Obsolete subsequence function
    SubSeq(R1, R2, R2 + R3 + 1);

syslsp procedure SubSeq(R1, R2, R3);	% R2 is lower bound, R3 upper
begin scalar NewSize, OldSize, NewItem;
    if not PosIntP R2 then return IndexError(R2, 'SubSeq);
    if not PosIntP R3 then return IndexError(R3, 'SubSeq);
    NewSize := R3 - R2 - 1;
    if NewSize < -1 then return RangeError(R1, R3, 'SubSeq);
    return case Tag R1 of
	Str, Bytes:
	<<  OldSize := StrLen StrInf R1;
	    if R3 - 1 > OldSize then RangeError(R1, R3, 'SubSeq)
	    else
	    <<  NewItem := GtSTR NewSize;
		R3 := StrInf R1;
		for I := 0 step 1 until NewSize do
		    StrByt(NewItem, I) := StrByt(R3, R2 + I);
		case Tag R1 of
		    Str:
			MkSTR NewItem;
		    Bytes:
			MkBYTES NewItem;
		end >> >>;
	Vect:
	<<  OldSize := VecLen VecInf R1;
	    if R3 - 1 > OldSize then RangeError(R1, R3, 'SubSeq)
	    else
	    <<  NewItem := GtVECT NewSize;
		R3 := VecInf R1;
		for I := 0 step 1 until NewSize do
		    VecItm(NewItem, I) := VecItm(R3, R2 + I);
		MkVEC NewItem >> >>;
	Wrds:
	<<  OldSize := WrdLen WrdInf R1;
	    if R3 - 1 > OldSize then RangeError(R1, R3, 'SubSeq)
	    else
	    <<  NewItem := GtWRDS NewSize;
		R3 := WrdInf R1;
		for I := 0 step 1 until NewSize do
		    WrdItm(NewItem, I) := WrdItm(R3, R2 + I);
		MkWRDS NewItem >> >>;
	HalfWords:
	<<  OldSize := HalfWordLen HalfWordInf R1;
	    if R3 - 1 > OldSize then RangeError(R1, R3, 'SubSeq)
	    else
	    <<  NewItem := GtHalfWords NewSize;
		R3 := HalfWordInf R1;
		for I := 0 step 1 until NewSize do
		    HalfWordItm(NewItem, I) := HalfWordItm(R3, R2 + I);
		MkHalfWords NewItem >> >>;
	Pair:
	<<  for I := 1 step 1 until R2 do
		if PairP R1 then R1 := rest R1
		else RangeError(R1, R2, 'SubSeq);
	    NewItem := NIL . NIL;
	    for I := 0 step 1 until NewSize do
		if PairP R1 then
		<<  TConc(NewItem, first R1);
		    R1 := rest R1 >>
		else RangeError(R1, R3, 'SubSeq);
	    car NewItem >>;
	default:
	    NonSequenceError(R1, 'SubSeq);
    end;
end;

syslsp procedure SetSub(R1, R2, R3, R4); %. Obsolete subsequence function
    SetSubSeq(R1, R2, R2 + R3 + 1, R4);

syslsp procedure SetSubSeq(R1, R2, R3, R4);	% R2 is lower bound, R3 upper
begin scalar NewSize, OldSize, SubSize, NewItem;
    if not PosIntP R2 then return IndexError(R2, 'SetSubSeq);
    if not PosIntP R3 then return IndexError(R3, 'SetSubSeq);
    NewSize := R3 - R2 - 1;
    if NewSize < -1 then return RangeError(R1, R3, 'SetSubSeq);
    case Tag R1 of
	Str, Bytes:
	<<  if not StringP R4 and not BytesP R4 then return
		NonStringError(R4, 'SetSubSeq);
	    OldSize := StrLen StrInf R1;
	    NewItem := StrInf R4;
	    SubSize := StrLen NewItem;
	    if R3 - 1 > OldSize then RangeError(R1, R3, 'SetSubSeq)
	    else if not (NewSize eq SubSize) then
		RangeError(R4, NewSize, 'SetSubSeq)
	    else
	    <<  R3 := StrInf R1;
		for I := 0 step 1 until NewSize do
		    StrByt(R3, R2 + I) := StrByt(NewItem, I) >> >>;
	Vect:
	<<  if not VectorP R4 then return
		NonVectorError(R4, 'SetSubSeq);
	    OldSize := VecLen VecInf R1;
	    NewItem := VecInf R4;
	    SubSize := VecLen NewItem;
	    if R3 - 1 > OldSize then RangeError(R1, R3, 'SetSubSeq)
	    else if not (NewSize eq SubSize) then
		RangeError(R4, NewSize, 'SetSubSeq)
	    else
	    <<  R3 := VecInf R1;
		for I := 0 step 1 until NewSize do
		    VecItm(R3, R2 + I) := VecItm(NewItem, I) >> >>;
	Wrds:
	<<  if not WrdsP R4 then return
		NonVectorError(R4, 'SetSubSeq);
	    OldSize := WrdLen WrdInf R1;
	    NewItem := WrdInf R4;
	    SubSize := WrdLen NewItem;
	    if R3 - 1 > OldSize then RangeError(R1, R3, 'SetSubSeq)
	    else if not (NewSize eq SubSize) then
		RangeError(R4, NewSize, 'SetSubSeq)
	    else
	    <<  R3 := WrdInf R1;
		for I := 0 step 1 until NewSize do
		    WrdItm(R3, R2 + I) := WrdItm(NewItem, I) >> >>;
	HalfWords:
	<<  if not HalfWordsP R4 then return
		NonVectorError(R4, 'SetSubSeq);
	    OldSize := HalfWordLen HalfWordInf R1;
	    NewItem := HalfWordInf R4;
	    SubSize := HalfWordLen NewItem;
	    if R3 - 1 > OldSize then RangeError(R1, R3, 'SetSubSeq)
	    else if not (NewSize eq SubSize) then
		RangeError(R4, NewSize, 'SetSubSeq)
	    else
	    <<  R3 := HalfWordInf R1;
		for I := 0 step 1 until NewSize do
		    HalfWordItm(R3, R2 + I) := HalfWordItm(NewItem, I) >> >>;
	Pair:
	<<  if not PairP R4 and not null R4 then return
		NonPairError(R4, 'SetSubSeq);
	    for I := 1 step 1 until R2 do
		if PairP R1 then R1 := rest R1
		else RangeError(R1, R2, 'SetSubSeq);
	    NewItem := R4;
	    for I := 0 step 1 until NewSize do
		if PairP R1 and PairP NewItem then
		<<  RPlaca(R1, first NewItem);
		    R1 := rest R1;
		    NewItem := rest NewItem >>
		else RangeError(R1, R3, 'SetSubSeq) >>;
	default:
	    NonSequenceError(R1, 'SetSubSeq);
    end;
    return R4;
end;

syslsp procedure Concat(R1, R2);	%. Concatenate 2 sequences
begin scalar I1, I2, Tmp1, Tmp2, Tmp3;
return case Tag R1 of
    STR, BYTES:
    <<  if not (StringP R2 or BytesP R2) then return
	    NonStringError(R2, 'Concat);
	Tmp1 := StrInf R1;
	Tmp2 := StrInf R2;
	I1 := StrLen Tmp1;
	I2 := StrLen Tmp2;
	Tmp3 := GtSTR(I1 + I2 + 1);		% R1 and R2 can move
	Tmp1 := StrInf R1;
	Tmp2 := StrInf R2;
	for I := 0 step 1 until I1 do
	    StrByt(Tmp3, I) := StrByt(Tmp1, I);
	for I := 0 step 1 until I2 do
	    StrByt(Tmp3, I1 + I + 1) := StrByt(Tmp2, I);
	if StringP R1 then MkSTR Tmp3 else MkBYTES Tmp3 >>;
    VECT:
    <<  if not VectorP R2 then return
	    NonVectorError(R2, 'Concat);
	Tmp1 := VecInf R1;
	Tmp2 := VecInf R2;
	I1 := VecLen Tmp1;
	I2 := VecLen Tmp2;
	Tmp3 := GtVECT(I1 + I2 + 1);		% R1 and R2 can move
	Tmp1 := VecInf R1;
	Tmp2 := VecInf R2;
	for I := 0 step 1 until I1 do
	    VecItm(Tmp3, I) := VecItm(Tmp1, I);
	for I := 0 step 1 until I2 do
	    VecItm(Tmp3, I1 + I + 1) := VecItm(Tmp2, I);
	MkVEC Tmp3 >>;
    WRDS:
    <<  if not WrdsP R2 then return
	    NonVectorError(R2, 'Concat);
	Tmp1 := WrdInf R1;
	Tmp2 := WrdInf R2;
	I1 := WrdLen Tmp1;
	I2 := WrdLen Tmp2;
	Tmp3 := GtWrds(I1 + I2 + 1);		% R1 and R2 can move
	Tmp1 := WrdInf R1;
	Tmp2 := WrdInf R2;
	for I := 0 step 1 until I1 do
	    WrdItm(Tmp3, I) := WrdItm(Tmp1, I);
	for I := 0 step 1 until I2 do
	    WrdItm(Tmp3, I1 + I + 1) := WrdItm(Tmp2, I);
	MkWRDS Tmp3 >>;
    HALFWORDS:
    <<  if not HalfWordsP R2 then return
	    NonVectorError(R2, 'Concat);
	Tmp1 := HalfWordInf R1;
	Tmp2 := HalfWordInf R2;
	I1 := HalfWordLen Tmp1;
	I2 := HalfWordLen Tmp2;
	Tmp3 := GtHalfWords(I1 + I2 + 1);		% R1 and R2 can move
	Tmp1 := HalfWordInf R1;
	Tmp2 := HalfWordInf R2;
	for I := 0 step 1 until I1 do
	    HalfWordItm(Tmp3, I) := HalfWordItm(Tmp1, I);
	for I := 0 step 1 until I2 do
	    HalfWordItm(Tmp3, I1 + I + 1) := HalfWordItm(Tmp2, I);
	MkHalfWords Tmp3 >>;
    PAIR, ID:
	if null R1 or PairP R1 then Append(R1, R2);
    default:
	NonSequenceError(R1, 'Concat);
    end;
end;

syslsp procedure Size S;		%. Upper bound of sequence
    case Tag S of
	STR, BYTES, WRDS, VECT, HALFWORDS:
	    GetLen Inf S;
	ID:
	    -1;
	PAIR:
	begin scalar I;
	    I := -1;
	    while PairP S do
	    <<  I := I + 1;
	        S := cdr S >>;
	    return I;
	end;
	default:
	    NonSequenceError(S, 'Size);
    end;

off SysLisp;

END;
