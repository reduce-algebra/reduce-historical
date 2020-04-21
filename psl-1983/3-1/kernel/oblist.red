%
% OBLIST.RED - Intern, RemOb and friends
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        27 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.INTERP>OBLIST.RED.9, 15-Sep-82 09:35:25, Edit by BENSON
%  InternP accepts a string as well as a symbol

% CopyString and CopyStringToFrom are found in COPIERS.RED

CompileTime flag('(AddToObList LookupOrAddToObList InObList
		   InitNewID GenSym1),
		 'InternalFunction);

on SysLisp;

internal WConst DeletedSlotValue = -1,
		EmptySlotValue = 0;

CompileTime <<

syslsp smacro procedure DeletedSlot U;
    ObArray U eq DeletedSlotValue;

syslsp smacro procedure EmptySlot U;
    ObArray U eq EmptySlotValue;

syslsp smacro procedure NextSlot H;
    if H eq MaxObArray then 0 else H + 1;

% StringEqual found in EQUAL.RED

syslsp smacro procedure EqualObArrayEntry(ObArrayIndex, S);
    StringEqual(SymNam ObArray ObArrayIndex, S);
>>;

syslsp procedure AddToObList U;
%
% U is an ID, which is added to the oblist if an ID with the same
% print name is not already there.  The interned ID is returned.
%
begin scalar V, W, X, Y;
    W := IDInf U;
    U := StrInf SymNam W;
    Y := StrLen U;
    if Y < 0 then return StdError '"The null string cannot be interned";
    if Y eq 0 then return MkID StrByt(U, 0);
    return if OccupiedSlot(V := InObList U) then MkID ObArray V
    else
    <<  ObArray V := W;
	X := GtConstSTR Y;
	CopyStringToFrom(X, U);
	SymNam W := MkSTR X;
	MkID W >>;
end;

syslsp procedure LookupOrAddToObList U;
%
% U is a String, which IS copied if it is not found on the ObList
% The interned ID with U as print name is returned
%
begin scalar V, W, X, Y;
    U := StrInf U;
    Y := StrLen U;
    if Y < 0 then return StdError '"The null string cannot be interned";
    if Y eq 0 then return MkID StrByt(U, 0);
    return if OccupiedSlot(V := InObList U) then MkID ObArray V
    else
    <<  W := GtID();			% allocate a new ID
	ObArray V := W;			% plant it in the Oblist
	X := GtConstSTR Y;		% allocate a string from uncollected
	CopyStringToFrom(X, StrInf U);	% space
	InitNewID(W, MkSTR X) >>;
end;

syslsp procedure NewID S;	 %. Allocate un-interned ID with print name S
    InitNewID(GtID(), S);		% Doesn't copy S

syslsp procedure InitNewID(U, V);	% Initialize cells of an ID to defaults
<<  SymNam U := V;
    U := MkID U;
    MakeUnBound U;
    SetProp(U, NIL);
    MakeFUnBound U;
    U >>;

syslsp procedure HashFunction S;	% Compute hash function of string
begin scalar Len, HashVal;		% Fold together a bunch of bits
    S := StrInf S;
    HashVal := 0;			% from the first BitsPerWord - 8
    Len := StrLen S;			% chars of the string
    if Len > BitsPerWord - 8 then Len := BitsPerWord - 8;
    for I := 0 step 1 until Len do
	HashVal := LXOR(HashVal, LSH(StrByt(S, I), (BitsPerWord - 8) - I));
    return MOD(HashVal, MaxObArray);
end;

syslsp procedure InObList U;	% U is a string.  Returns an ObArray pointer
begin scalar H, DSlot, WalkObArray;
    H := HashFunction U;
    WalkObArray := H;
    DSlot := -1;
Loop:
    if EmptySlot WalkObArray then return
	if DSlot neq -1 then
	    DSlot
	else
	    WalkObArray
    else if DeletedSlot WalkObArray and DSlot eq -1 then
	DSlot := WalkObArray
    else if EqualObArrayEntry(WalkObArray, U) then return
	WalkObArray;
    WalkObArray := NextSlot WalkObArray;
    if WalkObArray eq H then FatalError "Oblist overflow";
    goto Loop;
end;

syslsp procedure Intern U;	 %. Add U to ObList
%
% U is a string or uninterned ID
%
    if IDP U then
	AddToObList U
    else if StringP U then
	LookupOrAddToObList U
    else
	TypeError(U, 'Intern, '"ID or string");

syslsp procedure RemOb U;		%. REMove id from OBlist
begin scalar V;
    if not IDP U then return
	NonIDError(U, 'RemOb);
    V := IDInf U;
    if V < 128 then return
	TypeError(U, 'RemOb, '"non-char");
    V := SymNam V;
    return
    <<  if OccupiedSlot(V := InObList V) then
	    ObArray V := DeletedSlotValue;
	U >>
end;

% Changed to allow a string as well as a symbol, EB, 15 September 1982
syslsp procedure InternP U;		%. Is U an interned ID?
    if IDP U then
    <<  U := IDInf U;
	U < 128 or U eq ObArray InObList SymNam U >>
    else if StringP U then
	StrLen StrInf U eq 0 or OccupiedSlot InObList U
    else NIL;

internal WString GenSymPName = "G0000";

syslsp procedure GenSym();		%. GENerate unique, uninterned SYMbol
<<  GenSym1 4;
    NewID CopyString GenSymPName >>;

syslsp procedure GenSym1 N;		% Auxiliary function for GenSym
begin scalar Ch;
    return if N > 0 then
	if (Ch := StrByt(GenSymPName, N)) < char !9 then
	    StrByt(GenSymPName, N) := Ch + 1
	else
	<<  StrByt(GenSymPName, N) := char !0;
	    GenSym1(N - 1) >>
    else				% start over
    <<  StrByt(GenSymPName, 0) := StrByt(GenSymPName, 0) + 1;
	GenSym1 4 >>;
end;

syslsp procedure InternGenSym();	%. GENerate unique, interned SYMbol
<<  GenSym1 4;
    Intern MkSTR GenSymPName >>;

syslsp procedure MapObl F;		%. Apply F to every interned ID
<<  for I := 0 step 1 until 127 do Apply(F, list MkID I);
    for I := 0 step 1 until MaxObArray do
	if OccupiedSlot I then Apply(F, list MkID ObArray I) >>;

% These functions provide support for multiple oblists
% Cf PACKAGE.RED for their use

internal WVar LastObArrayPtr;

syslsp procedure GlobalLookup S;	% Lookup string S in global oblist
    if not StringP S then NonStringError(S, 'GlobalLookup)
    else if OccupiedSlot(LastObArrayPtr := InObList S) then
	MkID ObArray LastObArrayPtr
    else '0;

syslsp procedure GlobalInstall S;	% Add new ID with PName S to oblist
begin scalar Ind, PN;
    Ind := GlobalLookup S;
    return if Ind neq '0 then Ind
    else
    <<  Ind := GtID();
	ObArray LastObArrayPtr := Ind;
	PN := GtConstSTR StrLen StrInf S; % allocate a string from uncollected
	CopyStringToFrom(PN, StrInf S);	% space
	InitNewID(Ind, MkSTR PN) >>;
end;

syslsp procedure GlobalRemove S;	% Remove ID with PName S from oblist
begin scalar Ind;
    Ind := GlobalLookup S;
    return if Ind eq '0 then '0
    else
    <<  Ind := ObArray LastObArrayPtr;
	ObArray LastObArrayPtr := DeletedSlotValue;
	MkID Ind >>;
end;

syslsp procedure InitObList();
begin scalar Tmp;
    if_system(MC68000, <<	% 68000 systems don't clear memory statically
	for I := 0 step 1 until MaxObArray do
	    ObArray I := EmptySlotValue >>);
    Tmp := NextSymbol - 1;
    for I := 128 step 1 until Tmp do
	ObArray InObList SymNam I := I;
end;

off SysLisp;

StartupTime InitObList();

END;
