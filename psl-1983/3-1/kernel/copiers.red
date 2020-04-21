% COPIERS.RED - Functions for copying various data types
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        27 August 1981
% Copyright (c) 1981 University of Utah
%

% <PSL.KERNEL>COPIERS.RED.2, 28-Sep-82 10:21:15, Edit by PERDUE
% Made CopyStringToFrom safe and to not bother clearing the
% terminating byte.

on SysLisp;

syslsp procedure CopyStringToFrom(New, Old);  %. Copy all chars in Old to New
begin scalar SLen, StripNew, StripOld;
    StripNew := StrInf New;
    StripOld := StrInf Old;
    SLen := StrLen StripOld;
    if StrLen StripNew < SLen then SLen := StrLen StripNew;
    for I := 0 step 1 until SLen do
	StrByt(StripNew, I) := StrByt(StripOld, I);
    return New;
end;

syslsp procedure CopyString S;		%. copy to new heap string
begin scalar S1;
    S1 := GtSTR StrLen StrInf S;
    CopyStringToFrom(S1, StrInf S);
    return MkSTR S1;
end;

syslsp procedure CopyWArray(New, Old, UpLim);	%. copy UpLim + 1 words
<<  for I := 0 step 1 until UpLim do
	New[I] := Old[I];
    New >>;

syslsp procedure CopyVectorToFrom(New, Old);	%. Move elements, don't recurse
begin scalar SLen, StripNew, StripOld;
    StripNew := VecInf New;
    StripOld := VecInf Old;
    SLen := VecLen StripOld;		% assumes VecLen New has been set
    for I := 0 step 1 until SLen do
	VecItm(StripNew, I) := VecItm(StripOld, I);
    return New;
end;

syslsp procedure CopyVector S;		%. Copy to new vector in heap
begin scalar S1;
    S1 := GtVECT VecLen VecInf S;
    CopyVectorToFrom(S1, VecInf S);
    return MkVEC S1;
end;

syslsp procedure CopyWRDSToFrom(New, Old);	%. Like CopyWArray in heap
begin scalar SLen, StripNew, StripOld;
    StripNew := WrdInf New;
    StripOld := WrdInf Old;
    SLen := WrdLen StripOld;		% assumes WrdLen New has been set
    for I := 0 step 1 until SLen do
	WrdItm(StripNew, I) := WrdItm(StripOld, I);
    return New;
end;

syslsp procedure CopyWRDS S;		%. Allocate new WRDS array in heap
begin scalar S1;
    S1 := GtWRDS WrdLen WrdInf S;
    CopyWRDSToFrom(S1, WrdInf S);
    return MkWRDS S1;
end;

% CopyPairToFrom is RplacW, found in EASY-NON-SL.RED
% CopyPair is: car S . cdr S;

% Usual Lisp definition of Copy only copies pairs, is found in EASY-NON-SL.RED

syslsp procedure TotalCopy S;		%. Unique copy of entire structure
begin scalar Len, Ptr, StripS;		% blows up on circular structures
    return case Tag S of
      PAIR:
	TotalCopy car S . TotalCopy cdr S;
      STR:
	CopyString S;
      VECT:
	<<  StripS := VecInf S;
	    Len := VecLen StripS;
	    Ptr := MkVEC GtVECT Len;
	    for I := 0 step 1 until Len do
		VecItm(VecInf Ptr, I) := TotalCopy VecItm(VecInf S, I);
	    Ptr >>;
      WRDS:
	CopyWRDS S;
      FIXN:
	MkFIXN Inf CopyWRDS S;
      FLTN:
	MkFLTN Inf CopyWRDS S;
      default:
	S
    end;
end;

off SysLisp;

END;
