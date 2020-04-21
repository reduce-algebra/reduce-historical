%  <PSL.COMP.20.EXT>TAGS.RED.7,  1-Jun-83 08:10:26, Edit by KESSLER
%  Change BothTimes Declarations of wconsts to compiletime.
on syslisp;

% tags

CompileTime <<
exported WConst TagStartingBit = 0,
		TagBitLength = 6,
		InfStartingBit = 6,
		InfBitLength = 30,
		GCStartingBit = 0,
		GCBitLength = 0,
		AddressingUnitsPerItem = 1,
		CharactersPerWord = 5,
		BitsPerWord = 36,
		AddressingUnitsPerFunctionCell = 1,
		StackDirection = 1;

>>;

off syslisp;

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

macro procedure MidTags U;
    DeclareTagRange(cdr U, LSH(1, get('TagBitLength, 'WConst) - 1) - 2, -1);

macro procedure HighTags U;
    DeclareTagRange(cdr U, LSH(1, get('TagBitLength, 'WConst)) - 1, -1);
>>;

% JumpInType and friends depend on the ordering and contiguity of
% the numeric type tags.  Fast arithmetic depends on PosInt = 0,
% NegInt = -1.  Garbage collectors depend on pointer tags being
% between PosInt and Code, non-inclusive. /csp

LowTags(PosInt, FixN, BigN, FltN, Str, Bytes, HalfWords, Wrds, Vect, Pair,
        Evect);

put('Code, 'WConst, 15);

% Extended addressing treats negative word (one with aits high-order bit
% on) as a local address--hence pointer types must have (positive) MidTags

MidTags( ID, Unbound, BtrTag, Forward,
	 HVect, HWrds, HHalfWords, HBytes);

HighTags(NegInt);


