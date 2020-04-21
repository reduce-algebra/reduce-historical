%
% DATA-MACHINE.RED - Macros for fast access to data structures
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        5 April 1982
% Copyright (c) 1982 University of Utah
%
% Edit by GRISS, 3Nov: Added missing EVEC operations
% Primitives handled by the compiler are BYTE, PUTBYTE, GETMEM, PUTMEM,
% MKITEM, FIELD, SIGNEDFIELD, PUTFIELD, HALFWORD, PUYTHALFWORD

on Syslisp;

off R2I;

% These definitions are for interpretive testing of Syslisp code.
% They may be dangerous in some cases.

CommentOutCode <<
syslsp procedure Byte(WAddr, ByteOffset);
    Byte(WAddr, ByteOffset);

syslsp procedure PutByte(WAddr, ByteOffset, Val);
    PutByte(WAddr, ByteOffset, Val);

syslsp procedure Halfword(WAddr, HalfwordOffset);
    Halfword(WAddr, HalfwordOffset);

syslsp procedure PutHalfword(WAddr, HalfwordOffset, Val);
    PutHalfword(WAddr, HalfwordOffset, Val);

syslsp procedure GetMem Addr;
    GetMem Addr;

syslsp procedure PutMem(Addr, Val);
    PutMem(Addr, Val);

syslsp procedure MkItem(TagPart, InfPart);
    MkItem(TagPart, InfPart);

CommentOutCode <<			% can't do FIELD w/ non constants
syslsp procedure Field(Cell, StartingBit, BitLength);
    Field(Cell, StartingBit, BitLength);

syslsp procedure SignedField(Cell, StartingBit, BitLength);
    SignedField(Cell, StartingBit, BitLength);

syslsp procedure PutField(Cell, StartingBit, BitLength, Val);
    PutField(Cell, StartingBit, BitLength, Val);
>>;

syslsp procedure WPlus2(R1, R2);
    WPlus2(R1, R2);

syslsp procedure WDifference(R1, R2);
    WDifference(R1, R2);

syslsp procedure WTimes2(R1, R2);
    WTimes2(R1, R2);

syslsp procedure WQuotient(R1, R2);
    WQuotient(R1, R2);

syslsp procedure WRemainder(R1, R2);
    WRemainder(R1, R2);

syslsp procedure WMinus R1;
    WMinus R1;

syslsp procedure WShift(R1, R2);
    WShift(R1, R2);

syslsp procedure WAnd(R1, R2);
    WAnd(R1, R2);

syslsp procedure WOr(R1, R2);
    WOr(R1, R2);

syslsp procedure WXor(R1, R2);
    WXor(R1, R2);

syslsp procedure WNot R1;
    WNot R1;

syslsp procedure WLessP(R1, R2);
    WLessP(R1, R2);

syslsp procedure WGreaterP(R1, R2);
    WGreaterP(R1, R2);

syslsp procedure WLEQ(R1, R2);
    WLEQ(R1, R2);

syslsp procedure WGEQ(R1, R2);
    WGEQ(R1, R2);
>>;

on R2I;

off Syslisp;

% SysLisp array accessing primitives

syslsp macro procedure WGetV U;
    list('GetMem, list('WPlus2, cadr U, list('WTimes2, caddr U,
					   '(WConst AddressingUnitsPerItem))));

syslsp macro procedure WPutV U;
    list('PutMem, list('WPlus2, cadr U, list('WTimes2, caddr U,
					    '(WConst AddressingUnitsPerItem))),
		  cadddr U);

% tags

CompileTime <<
lisp procedure DeclareTagRange(NameList, StartingValue, Increment);
begin scalar Result;
    Result := list 'progn;
    while NameList do
    <<  Result := list('put, MkQuote car NameList,
			     '(quote WConst),
			     StartingValue)
		  . Result;
	StartingValue := StartingValue + Increment;
	NameList := cdr NameList >>;
    return ReversIP Result;
end;

macro procedure LowTags U;
    DeclareTagRange(cdr U, 0, 1);

macro procedure HighTags U;
    DeclareTagRange(cdr U, LSH(1, get('TagBitLength, 'WConst)) - 1, -1);
>>;

LowTags(PosInt, FixN, BigN, FltN, Str, Bytes, HalfWords, Wrds, Vect, Pair,
        Evect);

put('Code, 'WConst, 15);

HighTags(NegInt, ID, Unbound, BtrTag, Forward,
	 HVect, HWrds, HHalfWords, HBytes);

% Item constructor macros

lisp procedure MakeItemConstructor(TagPart, InfPart);
    list('MkItem, TagPart, InfPart);

syslsp macro procedure MkBTR U;
    MakeItemConstructor('(wconst BtrTag), cadr U);

syslsp macro procedure MkID U;
    MakeItemConstructor('(wconst ID), cadr U);

syslsp macro procedure MkFIXN U;
    MakeItemConstructor('(wconst FIXN), cadr U);

syslsp macro procedure MkFLTN U;
    MakeItemConstructor('(wconst FLTN), cadr U);

syslsp macro procedure MkBIGN U;
    MakeItemConstructor('(wconst BIGN), cadr U);

syslsp macro procedure MkPAIR U;
    MakeItemConstructor('(wconst PAIR), cadr U);

syslsp macro procedure MkVEC U;
    MakeItemConstructor('(wconst VECT), cadr U);

syslsp macro procedure MkEVECT U;
    MakeItemConstructor('(wconst EVECT), cadr U);

syslsp macro procedure MkWRDS U;
    MakeItemConstructor('(wconst WRDS), cadr U);

syslsp macro procedure MkSTR U;
    MakeItemConstructor('(wconst STR), cadr U);

syslsp macro procedure MkBYTES U;
    MakeItemConstructor('(wconst BYTES), cadr U);

syslsp macro procedure MkHalfWords U;
    MakeItemConstructor('(wconst HalfWords), cadr U);

syslsp macro procedure MkCODE U;
    MakeItemConstructor('(wconst CODE), cadr U);

% Access to tag (type indicator) of Lisp item in ordinary code

syslsp macro procedure Tag U;
    list('Field, cadr U, '(wconst TagStartingBit), '(wconst TagBitLength));


% Access to info field of item (pointer or immediate operand)

syslsp macro procedure Inf U;
    list('Field, cadr U, '(wconst InfStartingBit), '(wconst InfBitLength));

syslsp macro procedure PutInf U;
    list('PutField, cadr U, '(wconst InfStartingBit),
			    '(wconst InfBitLength), caddr U);

for each X in '(IDInf StrInf VecInf EvecInf PairInf WrdInf HalfWordInf CodeInf
		FixInf FltInf BigInf) do
    PutD(X, 'Macro, cdr getd 'Inf);

for each X in '(PutIDInf PutStrInf PutVecInf PutPairInf PutWrdInf
		PutHalfWordInf PutEvecInf
		PutFixInf PutFltInf PutBigInf) do
    PutD(X, 'Macro, cdr getd 'PutInf);

% IntInf is no longer needed, will be a macro no-op
% for the time being

RemProp('IntInf, 'OpenFn);

macro procedure IntInf U;
    cadr U;

% Similarly for MkINT

macro procedure MkINT U;
    cadr U;

% # of words in a pair

syslsp macro procedure PairPack U;
    2;

% length (in characters, words, etc.) of a string, vector, or whatever,
% stored in the first word pointed to

syslsp macro procedure GetLen U;
    list('SignedField, list('GetMem, cadr U), '(WConst InfStartingBit),
					      '(WConst InfBitLength));

syslsp macro procedure StrBase U;	% point to chars of string
    list('WPlus2, cadr U, '(WConst AddressingUnitsPerItem));

% chars string length --> words string length

% Note that StrPack and HalfWordPack do not include the header word,
% VectPack and WrdPack do.

syslsp macro procedure StrPack U;
    list('WQuotient, list('WPlus2, cadr U,
				   list('WPlus2, '(WConst CharactersPerWord),
						 1)),
		     '(WConst CharactersPerWord));

% access to bytes of string; skip first word

syslsp macro procedure StrByt U;
    list('Byte, list('WPlus2, cadr U, '(WConst AddressingUnitsPerItem)),
		caddr U);

syslsp macro procedure PutStrByt U;
    list('PutByte, list('WPlus2, cadr U, '(WConst AddressingUnitsPerItem)),
		   caddr U,
		   cadddr U);

% access to halfword entries; skip first word

syslsp macro procedure HalfWordItm U;
    list('HalfWord, list('WPlus2, cadr U,
				  '(WConst AddressingUnitsPerItem)),
		    caddr U);

syslsp macro procedure PutHalfWordItm U;
    list('PutHalfWord, list('WPlus2, cadr U,
				     '(WConst AddressingUnitsPerItem)),
		       caddr U,
		       cadddr U);

% halfword length --> words  length

syslsp macro procedure HalfWordPack U;
    list('WPlus2, list('WShift, cadr U, -1), 1);


% length (in Item size quantities) of Lisp vectors

% size of Lisp vector in words

syslsp macro procedure VectPack U;
    list('WPlus2, cadr U, 1);

% size of Lisp Evector in words

syslsp macro procedure EVectPack U;
    list('WPlus2, cadr U, 1);

% access to elements of Lisp vector

syslsp macro procedure VecItm U;
    list('WGetV, cadr U,
		 list('WPlus2, caddr U, 1));

syslsp macro procedure PutVecItm U;
    list('WPutV, cadr U,
		 list('WPlus2, caddr U, 1),
		 cadddr U);

% access to elements of Lisp Evector

syslsp macro procedure EVecItm U;
    list('WGetV, cadr U,
		 list('WPlus2, caddr U, 1));

syslsp macro procedure PutEVecItm U;
    list('WPutV, cadr U,
		 list('WPlus2, caddr U, 1),
		 cadddr U);


% Wrd is like Vect, but not traced by the garbage collector

syslsp macro procedure WrdPack U;
    list('WPlus2, cadr U, 1);

for each X in '(StrLen ByteLen VecLen EVecLen WrdLen HalfWordLen) do
    PutD(X, 'Macro, cdr getd 'GetLen);

PutD('WrdItm, 'Macro, cdr GetD 'VecItm);

PutD('PutWrdItm, 'Macro, cdr GetD 'PutVecItm);

syslsp macro procedure FixVal U;
    list('WGetV, cadr U, 1);

syslsp macro procedure PutFixVal U;
    list('WPutV, cadr U, 1, caddr U);


syslsp macro procedure FloatBase U;
    list('WPlus2, cadr U, '(WConst AddressingUnitsPerItem));

syslsp macro procedure FloatHighOrder U;
    list('WGetV, cadr U, 1);

syslsp macro procedure FloatLowOrder U;
    list('WGetV, cadr U, 2);


% New addition: A code pointer can have the number of arguments it expects
% stored in the word just before the entry 
syslsp macro procedure !%code!-number!-of!-arguments U;
    list('WGetV, cadr U, -1);

% The four basic cells for each symbol: Val, Nam, Fnc, Prp, corresponding to
% variable value, symbol name (as string), function cell (jump to compiled
% code or lambda linker) and property list (pairs for PUT, GET, atoms for FLAG,
% FLAGP).  These are currently 4 separate arrays, but this representation may
% be changed to a contiguous 4 element record for each symbol or something else
% and therefore should not be accessed as arrays.

syslsp macro procedure SymVal U;
    list('WGetV, '(WConst SymVal), cadr U);

syslsp macro procedure PutSymVal U;
    list('WPutV, '(WConst SymVal), cadr U, caddr U);

syslsp macro procedure LispVar U;	 % Access value cell by name
    list('(WConst SymVal), list('IDLoc, cadr U));

syslsp macro procedure PutLispVar U;
    list('PutSymVal, list('IDLoc, cadr U), caddr U);

syslsp macro procedure SymNam U;
    list('WGetV, '(WConst SymNam), cadr U);

syslsp macro procedure PutSymNam U;
    list('WPutV, '(WConst SymNam), cadr U, caddr U);

% Retrieve the address stored in the function cell

% SymFnc and PutSymFnc are not defined portably

syslsp macro procedure SymPrp U;
    list('WGetV, '(WConst SymPrp), cadr U);

syslsp macro procedure PutSymPrp U;
    list('WPutV, '(WConst SymPrp), cadr U, caddr U);



% Binding stack primitives

syslsp macro procedure BndStkID U;
    list('WGetV, cadr U, -1);

syslsp macro procedure PutBndStkID U;
    list('WPutV, cadr U, -1, caddr U);

syslsp macro procedure BndStkVal U;
    list('GetMem, cadr U);

syslsp macro procedure PutBndStkVal U;
    list('PutMem, cadr U, caddr U);

syslsp macro procedure AdjustBndStkPtr U;
    list('WPlus2, cadr U,
		  list('WTimes2, caddr U,
				 list('WTimes2,
					'(WConst AddressingUnitsPerItem),
				         2)));

% ObArray is a linearly allocated hash table containing ID numbers of entries
% maintained as a circular buffer.  It is referenced only via these macros
% because we may decide to change to some other representation.

syslsp smacro procedure ObArray I;
    HalfWord(HashTable, I);

syslsp smacro procedure PutObArray(I, X);
    HalfWord(HashTable, I) := X;

put('ObArray, 'Assign!-Op, 'PutObArray);

syslsp smacro procedure OccupiedSlot U;
    ObArray U > 0;

DefList('((GetMem PutMem)
	  (Field PutField)
	  (Byte PutByte)
	  (HalfWord PutHalfWord)
	  (Tag PutTag)
	  (Inf PutInf)
	  (IDInf PutIDInf)
	  (StrInf PutStrInf)
	  (VecInf PutVecInf)
	  (EVecInf PutEVecInf)
	  (WrdInf PutWrdInf)
	  (PairInf PutPairInf)
	  (FixInf PutFixInf)
	  (FixVal PutFixVal)
	  (FltInf PutFltInf)
	  (BigInf PutBigInf)
	  (StrLen PutStrLen)
	  (StrByt PutStrByt)
	  (VecLen PutVecLen)
	  (VecInf PutVecInf)
	  (VecItm PutVecItm)
	  (EVecItm PutEVecItm)
	  (WrdLen PutWrdLen)
	  (WrdItm PutWrdItm)
	  (SymVal PutSymVal)
	  (LispVar PutLispVar)
	  (SymNam PutSymNam)
	  (SymFnc PutSymFnc)
	  (SymPrp PutSymPrp)
	  (BndStkID PutBndStkID)
	  (BndStkVal PutBndStkVal)), 'Assign!-Op);

% This is redefined for the HP 9836 to cure the high-order FF problem

macro procedure !%chipmunk!-kludge x;
    cadr x;

END;
