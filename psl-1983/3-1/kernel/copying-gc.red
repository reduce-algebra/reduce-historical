%
% GC.RED - Copying 2-space garbage collector for PSL
% 
% Author:      Eric Benson
%              Computer Science Dept.
%              University of Utah
% Date:        30 November 1981
% Copyright (c) 1981 Eric Benson
%

%  <PSL.KERNEL>COPYING-GC.RED.2, 23-Mar-83 11:35:37, Edit by KESSLER
%  Add HeadTrapBound Guys, so we can update the heap trap bound upon switch
% Edit by Cris Perdue, 15 Mar 1983 0937-PST
% Added missing comma as noted by Kessler.
% Edit by Cris Perdue, 16 Feb 1983 1409-PST
% Removed external declaration of HeapPreviousLast (the only occurrence)
% Now using "known-free-space" function and heap-warn-level
% Sets HeapTrapped to NIL now.
% Added check of Heap!-Warn!-Level after %Reclaim.
%  <PSL.KERNEL>COPYING-GC.RED.6,  4-Oct-82 17:56:49, Edit by BENSON
%  Added GCTime!*

fluid '(!*GC GCKnt!* GCTime!* Heap!-Warn!-Level);

LoadTime
<<  GCKnt!* := 0;
    GCTime!* := 0;
    !*GC := T;
    LispVar Heap!-Warn!-Level := 1000
>>;

on SysLisp;

CompileTime <<
syslsp smacro procedure PointerTagP X;
    X > PosInt and X < Code;

syslsp smacro procedure WithinOldHeapPointer X;
    X >= !%chipmunk!-kludge OldHeapLowerBound
	and X <= !%chipmunk!-kludge OldHeapLast;

syslsp smacro procedure Mark X;
    MkItem(Forward, X);

syslsp smacro procedure Marked X;
    Tag X eq Forward;

syslsp smacro procedure MarkID X;
    Field(SymNam X, TagStartingBit, TagBitLength) := Forward;

syslsp smacro procedure MarkedID X;
    Tag SymNam X eq Forward;

syslsp smacro procedure ClearIDMark X;
    Field(SymNam X, TagStartingBit, TagBitLength) := STR;

flag('(CopyFromAllBases CopyFromRange CopyFromBase CopyItem CopyItem1
       MarkAndCopyFromID MakeIDFreeList GCStats),
     'InternalFunction);
>>;

external WVar ST, StackLowerBound,
	      BndStkLowerBound, BndStkPtr,
	      HeapLast, HeapLowerBound, HeapUpperBound,
	      OldHeapLast, OldHeapLowerBound, OldHeapUpperBound,
	      HeapTrapBound, OldHeapTrapBound, HeapTrapped;

internal WVar StackLast, OldTime, OldSize;

syslsp procedure Reclaim();
    !%Reclaim();

syslsp procedure !%Reclaim();
begin scalar Tmp1, Tmp2;
    if LispVar !*GC then ErrorPrintF "*** Garbage collection starting";
    BeforeGCSystemHook();
    StackLast := MakeAddressFromStackPointer AdjustStackPointer(ST,
								-FrameSize());
    OldTime := TimC();
    OldSize := HeapLast - HeapLowerBound;
    LispVar GCKnt!* := LispVar GCKnt!* + 1;
    OldHeapLast := HeapLast;
    HeapLast := OldHeapLowerBound;
    Tmp1 := HeapLowerBound;
    Tmp2 := HeapUpperBound;
    HeapLowerBound := OldHeapLowerBound;
    HeapUpperBound := OldHeapUpperBound;
    OldHeapLowerBound := Tmp1;
    OldHeapUpperBound := Tmp2;
    Tmp1 := HeapTrapBound;
    HeapTrapBound := OldHeapTrapBound;
    OldHeapTrapBound := Tmp1;
    CopyFromAllBases();
    MakeIDFreeList();
    AfterGCSystemHook();
    OldTime := TimC() - OldTime;
    LispVar GCTime!* := Plus2(LispVar GCTime!*, OldTime);
    if LispVar !*GC then GCStats();
    HeapTrapped := NIL;
    if IntInf Known!-Free!-Space() < IntInf (LispVar Heap!-Warning!-Level) then
	ContinuableError(99, "Heap space low", NIL)
>>;

syslsp procedure MarkAndCopyFromID X;
% SymNam has to be copied before marking, since the mark destroys the tag
% No problem since it's only a string, can't reference itself.
<<  CopyFromBase &SymNam X;
    MarkID X;
    CopyFromBase &SymPrp X;
    CopyFromBase &SymVal X >>;

syslsp procedure CopyFromAllBases();
begin scalar LastSymbol, B;
    MarkAndCopyFromID 128;		% Mark NIL first
    for I := 0 step 1 until 127 do
	if not MarkedID I then MarkAndCopyFromID I;
    for I := 0 step 1 until MaxObArray do
    <<  B := ObArray I;
	if B > 0 and not MarkedID B then MarkAndCopyFromID B >>;
    B := BndStkLowerBound;
    while << B := AdjustBndStkPtr(B, 1);
	     B <= BndStkPtr >> do
	CopyFromBase B;
    for I := StackLowerBound step StackDirection*AddressingUnitsPerItem
			     until StackLast do
	CopyFromBase I;
end;

syslsp procedure CopyFromRange(Lo, Hi);
begin scalar X, I;
    X := Lo;
    I := 0;
    while X <= Hi do
    <<  CopyFromBase X;
	I := I + 1;
	X := &Lo[I] >>;
end;

syslsp procedure CopyFromBase P;
    @P := CopyItem @P;

syslsp procedure CopyItem X;
begin scalar Typ, Info, Hdr;
    Typ := Tag X;
    if not PointerTagP Typ then return
    <<  if Typ = ID and not null X then	% don't follow NIL, for speed
	<<  Info := IDInf X;
	    if not MarkedID Info then MarkAndCopyFromID Info >>;
	X >>;
    Info := Inf X;
    if not WithinOldHeapPointer Info then return X;
    Hdr := @Info;
    if Marked Hdr then return MkItem(Typ, Inf Hdr);
    return CopyItem1 X;
end;

syslsp procedure CopyItem1 S;		% Copier for GC
begin scalar NewS, Len, Ptr, StripS;
    return case Tag S of
      PAIR:
	<<  Ptr := car S;
	    Rplaca(S, Mark(NewS := GtHeap PairPack()));
	    NewS[1] := CopyItem cdr S;
	    NewS[0] := CopyItem Ptr;
	    MkPAIR NewS >>;
      STR:
	<<  @StrInf S := Mark(NewS := CopyString S);
	    NewS >>;
      VECT:
	<<  StripS := VecInf S;
	    Len := VecLen StripS;
	    @StripS := Mark(Ptr := GtVECT Len);
	    for I := 0 step 1 until Len do
		VecItm(Ptr, I) := CopyItem VecItm(StripS, I);
	    MkVEC Ptr >>;
      EVECT:
	<<  StripS := VecInf S;
	    Len := VecLen StripS;
	    @StripS := Mark(Ptr := GtVECT Len);
	    for I := 0 step 1 until Len do
		VecItm(Ptr, I) := CopyItem VecItm(StripS, I);
	    MkItem(EVECT, Ptr) >>;
      WRDS, FIXN, FLTN, BIGN:
	<<  Ptr := Tag S;
	    @Inf S := Mark(NewS := CopyWRDS S);
	    MkItem(Ptr, NewS) >>;
      default:
	FatalError "Unexpected tag found during garbage collection";
    end;
end;

syslsp procedure MakeIDFreeList();
begin scalar Previous;
    for I := 0 step 1 until 128 do
	ClearIDMark I;
    Previous := 129;
    while MarkedID Previous and Previous <= MaxSymbols do
    <<  ClearIDMark Previous;
	Previous := Previous + 1 >>;
    if Previous >= MaxSymbols then
	NextSymbol := 0
    else
	NextSymbol := Previous;		% free list starts here
    for I := Previous + 1 step 1 until MaxSymbols do
	if MarkedID I then ClearIDMark I
	else
	<<  SymNam Previous := I;
	    Previous := I >>;
    SymNam Previous := 0;		% end of free list
end;

syslsp procedure GCStats();
<<  ErrorPrintF("*** GC %w: time %d ms, %d recovered, %d free",
	LispVar GCKnt!*,   OldTime,
		(OldSize - (HeapLast - HeapLowerBound))/AddressingUnitsPerItem,
			Known!-Free!-Space() ) >>;

off SysLisp;

END;
