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
    DeclareTagRange(cdr U, if_system(MC68000, 16#FF, 31), -1);
>>;

LowTags(PosInt, FixN, BigN, FltN, Str, Bytes, HalfWords, Wrds, Vect, Pair);

put('Code, 'WConst, 15);

HighTags(NegInt, ID, Unbound, BtrTag, Forward,
	 HVect, HWrds, HHalfWords, HBytes);


