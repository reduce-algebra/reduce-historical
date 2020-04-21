%
% GC.RED - Compacting garbage collector for PSL
% 
% Author:      Martin Griss and Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        28 August 1981
% Copyright (c) 1981 University of Utah
%

% All data types have either explicit header tag in first item,
% or are assumed to be 1st element of pair.

% Revision History:
% Edit by Cris Perdue, 16 Feb 1983 1407-PST
% Fixed GtHeap and collector(s) to use only HeapLast, not HeapPreviousLast
% Sets HeapTrapped to NIL now.
% Using known-free-space function
%  Added check of Heap-Warn-Level after %Reclaim
%  Defined and used known-free-space function
%  <PSL.KERNEL>COMPACTING-GC.RED.9,  4-Oct-82 17:59:55, Edit by BENSON
%  Added GCTime!*
%  <PSL.KERNEL>COMPACTING-GC.RED.3, 21-Sep-82 10:43:21, Edit by BENSON
%  Flagged most functions internal
% (M.L. Griss, March, 1977).
% (Update to speed up, July 1978)
% Converted to Syslisp July 1980
% En-STRUCT-ed, Eric Benson April 1981
% Added EVECT tag, M. Griss, 3 July 1982
fluid '(!*GC				% Controls printing of statistics
	GCTime!*			% Total amount of time spent in GC
	GCKnt!*				% count of # of GC's since system build
	heap!-warn!-level);		% Continuable error if this much not
					% free after %Reclaim.

LoadTime <<
    !*GC := T;				% Do print GC messages (SL Rep says no)
    GCTime!* := 0;
    GCKnt!* := 0;			% Initialize to zero
    Heap!-Warn!-Level := 1000;
>>;

on Syslisp;


% Predicates for whether to follow pointers

external WVar HeapLowerBound,		% Bottom of heap
	      HeapUpperBound,		% Top of heap
	      HeapLast,			% Last item allocated
	      HeapTrapped;		% Boolean: has trap occurred since GC?

CompileTime <<

flag('(MarkFromAllBases BuildRelocationFields UpdateAllBases CompactHeap
       MarkFromOneSymbol MakeIDFreeList
       GCMessage MarkFromSymbols MarkFromRange MarkFromBase MarkFromVector
       GCError UpdateSymbols UpdateRegion UpdateItem UpdateHeap),
     'InternalFunction);

syslsp smacro procedure PointerTagP X;
    X > PosInt and X < Code;

syslsp smacro procedure WithinHeapPointer X;
    X >= HeapLowerBound and X <= HeapLast;

>>;

% Marking primitives

internal WConst GCMarkValue = 8#777,
		HSkip = Forward;

CompileTime <<
syslsp smacro procedure Mark X;		% Get GC mark bits in item X points to
    GCField @X;

syslsp smacro procedure SetMark X;	% Set GC mark bits in item X points to
    GCField @X := GCMarkValue;

syslsp smacro procedure ClearMark X;  % Clear GC mark bits in item X points to
    GCField @X := if NegIntP @X then -1 else 0;

syslsp smacro procedure Marked X;	% Is item pointed to by X marked?
    Mark X eq GCMarkValue;


syslsp smacro procedure MarkID X;
    Field(SymNam X, TagStartingBit, TagBitLength) := Forward;

syslsp smacro procedure MarkedID X;
    Tag SymNam X eq Forward;

syslsp smacro procedure ClearIDMark X;
    Field(SymNam X, TagStartingBit, TagBitLength) := STR;


% Relocation primitives

syslsp smacro procedure SkipLength X;	% Stored in heap header
    Inf @X;

syslsp smacro procedure PutSkipLength(X, L);	% Store in heap header
    Inf @X := L;

put('SkipLength, 'Assign!-Op, 'PutSkipLength);
>>;

internal WConst BitsInSegment = 13,
		SegmentLength = LShift(1, BitsInSegment),
		SegmentMask = SegmentLength - 1;

internal WConst GCArraySize = LShift(HeapSize, -BitsInSegment) + 1;

internal WArray GCArray[GCArraySize];


CompileTime <<
syslsp smacro procedure SegmentNumber X;	% Get segment part of pointer
    LShift(X - HeapLowerBound, -BitsInSegment);

syslsp smacro procedure OffsetInSegment X;	% Get offset part of pointer
    LAnd(X - HeapLowerBound, SegmentMask);

syslsp smacro procedure MovementWithinSegment X;	% Reloc field in item
    GCField @X;

syslsp smacro procedure PutMovementWithinSegment(X, M);	% Store reloc field
    GCField @X := M;

syslsp smacro procedure ClearMovementWithinSegment X;	% Clear reloc field
    GCField @X := if NegIntP @X then -1 else 0;

put('MovementWithinSegment, 'Assign!-Op, 'PutMovementWithinSegment);

syslsp smacro procedure SegmentMovement X;	% Segment table
    GCArray[X];

syslsp smacro procedure PutSegmentMovement(X, M);	% Store in seg table
    GCArray[X] := M;

put('SegmentMovement, 'Assign!-Op, 'PutSegmentMovement);

syslsp smacro procedure Reloc X;	% Compute pointer adjustment
    X - (SegmentMovement SegmentNumber X + MovementWithinSegment X);
>>;

external WVar ST,			% stack pointer
	      StackLowerBound;		% bottom of stack

% Base registers marked from by collector

% SymNam, SymPrp and SymVal are declared for all

external WVar NextSymbol;		% next ID number to be allocated

external WVar BndStkLowerBound,		% Bottom of binding stack
	      BndStkPtr;		% Binding stack pointer

internal WVar StackEnd,			% Holds address of bottom of stack
	      StackStart,		% Holds address of top of stack
	      MarkTag,			% Used by MarkFromBase only
	      Hole,			% First location moved in heap
	      HeapShrink,		% Total amount reclaimed
	      StartingRealTime;

syslsp procedure Reclaim();		%. User call to garbage collector
<<  !%Reclaim();
    NIL >>;

syslsp procedure !%Reclaim();		% Garbage collector
<<  StackEnd := MakeAddressFromStackPointer ST - FrameSize();
    StackStart := StackLowerBound;
    if LispVar !*GC then ErrorPrintF "*** Garbage collection starting";
    StartingRealTime := TimC();
    LispVar GCKnt!* := LispVar GCKnt!* + 1; % must be INUM > 0, so needn't chk
    MarkFromAllBases();
    MakeIDFreeList();
    BuildRelocationFields();
    UpdateAllBases();
    CompactHeap();
    HeapLast := HeapLast - HeapShrink;
    StartingRealTime := TimC() - StartingRealTime;
    LispVar GCTime!* := Plus2(LispVar GCTime!*, StartingRealTime);
    if LispVar !*GC then GCMessage();
    HeapTrapped := NIL;
    if IntInf known!-free!-space() < IntInf (LispVar Heap!-Warn!-Level) then
	ContinuableError(99, "Heap space low", NIL);
>>;

syslsp procedure MarkFromAllBases();
begin scalar B;
    MarkFromSymbols();
    MarkFromRange(StackStart, StackEnd);
    B := BndStkLowerBound;
    while << B := AdjustBndStkPtr(B, 1);
	     B <= BndStkPtr >> do
	MarkFromBase @B;
end;

syslsp procedure MarkFromSymbols();
begin scalar B;
    MarkFromOneSymbol 128;		% mark NIL first
    for I := 0 step 1 until 127 do
	if not MarkedID I then MarkFromOneSymbol I;
    for I := 0 step 1 until MaxObArray do
    <<  B := ObArray I;
	if B > 0 and not MarkedID B then MarkFromOneSymbol B >>;
end;

syslsp procedure MarkFromOneSymbol X;
% SymNam has to be marked from before marking ID, since the mark uses its tag
% No problem since it's only a string, can't reference itself.
<<  MarkFromBase SymNam X;
    MarkID X;
    MarkFromBase SymPrp X;
    MarkFromBase SymVal X >>;

syslsp procedure MarkFromRange(Low, High);
    for Ptr := Low step 1 until High do MarkFromBase @Ptr;

syslsp procedure MarkFromBase Base;
begin scalar MarkInfo;
    MarkTag := Tag Base;
    if not PointerTagP MarkTag then return
    <<  if MarkTag = ID and not null Base then
	<<  MarkInfo := IDInf Base;
	    if not MarkedID MarkInfo then MarkFromOneSymbol MarkInfo >> >>;
    MarkInfo := Inf Base;
    if not WithinHeapPointer MarkInfo
	or Marked MarkInfo then return;
    SetMark MarkInfo;
CommentOutCode    CheckAndSetMark MarkInfo;
    return if MarkTag eq VECT or MarkTag eq EVECT then
	MarkFromVector MarkInfo
    else if MarkTag eq PAIR then
	<<  MarkFromBase car Base;
	    MarkFromBase cdr Base >>;
end;

CommentOutCode <<
syslsp procedure CheckAndSetMark P;
begin scalar HeadAtP;
    HeadAtP := Tag @P;
    case MarkTag of
    STR:
	if HeadAtP eq HBYTES then SetMark P;
    FIXN, FLTN, BIGN, WRDS:
	if HeadAtP eq HWRDS then SetMark P;
    VECT, EVECT:
	if HeadAtP eq HVECT then SetMark P;
    PAIR:
	SetMark P;
    default:
	GCError("Internal error in marking phase, at %o", P)
    end;
end;
>>;

syslsp procedure MarkFromVector Info;
begin scalar Uplim;
CommentOutCode    if Tag @Info neq HVECT then return;
    Uplim := &VecItm(Info, VecLen Info);
    for Ptr := &VecItm(Info, 0) step 1 until Uplim do
	MarkFromBase @Ptr;
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

syslsp procedure BuildRelocationFields();
%
%        Pass 2 - Turn off GC marks and Build SEGKNTs
%
begin scalar CurrentItem, SGCurrent, IGCurrent, TmpIG, DCount, SegLen;
    SGCurrent := IGCurrent := 0;
    SegmentMovement SGCurrent := 0;	% Dummy
    Hole := HeapLowerBound - 1;		% will be first hole
    DCount := HeapShrink := 0;		% holes in current segment, total holes
    CurrentItem := HeapLowerBound;
    while CurrentItem < HeapLast do
    begin scalar Incr;
	SegLen := case Tag @CurrentItem of
	BTRTAG, CODE, NEGINT, POSINT, ID, UNBOUND,
	STR, BYTES, FIXN, FLTN, BIGN, WRDS, Halfwords, PAIR, VECT, EVECT:
	    2;	 % must be first of pair
	HBYTES:
	    1 + StrPack StrLen CurrentItem;
	HHalfwords:
	    1 + HalfWordPack StrLen CurrentItem;
	HWRDS:
	    1 + WrdPack WrdLen CurrentItem;
	HVECT:
	    1 + VectPack VecLen CurrentItem;
	HSKIP:
	    SkipLength CurrentItem;
	default:
	    GCError("Illegal item in heap at %o", CurrentItem)
	end;	 % case
	if Marked CurrentItem then	 % a hole
	    if HeapShrink = 0 then
		ClearMark CurrentItem
	else				% segment also clears mark
	<<  MovementWithinSegment CurrentItem := DCount; % incremental shift
	    Incr := 0 >>			 % no shift
	else
	<<  @CurrentItem := MkItem(HSKIP, SegLen);	 % a skip mark
	    Incr := 1;					 % more shift
	    if Hole < HeapLowerBound then Hole := CurrentItem >>;
	TmpIG := IGCurrent + SegLen;	% set SEG size
	CurrentItem := CurrentItem + SegLen;
	while TmpIG >= SegmentLength do
	  begin scalar Tmp;
	    Tmp := SegmentLength - IGCurrent;	% Expand to next SEGMENT
	    SegLen := SegLen - Tmp;
	    if Incr eq 1 then HeapShrink := HeapShrink + Tmp;
	    DCount := IGCurrent := 0;
	    SGCurrent := SGCurrent + 1;
	    SegmentMovement SGCurrent := HeapShrink;	% Store Next Base
	    TmpIG := TmpIG - SegmentLength;
	  end;
	IGCurrent := TmpIG;
	if Incr eq 1 then
	<<  HeapShrink := HeapShrink + SegLen;
	    DCount := DCount + SegLen >>;	% Add in Hole Size
      end;
    SegmentMovement(SGCurrent + 1) := HeapShrink;
end;

syslsp procedure UpdateAllBases();
begin scalar B;
    UpdateSymbols();
    UpdateRegion(StackStart, StackEnd);
    B := BndStkLowerBound;
    while << B := AdjustBndStkPtr(B, 1);
	     B <= BndStkPtr >> do
	UpdateItem B;
    UpdateHeap() >>;

syslsp procedure UpdateSymbols();
    for I := 0 step 1 until MaxSymbols do
    begin scalar NameLoc;
	NameLoc := &SymNam I;
	if StringP @NameLoc then
	<<  UpdateItem NameLoc;
	    UpdateItem &SymVal I;
	    UpdateItem &SymPrp I >>;
    end;

syslsp procedure UpdateRegion(Low, High);
    for Ptr := Low step 1 until High do UpdateItem Ptr;

syslsp procedure UpdateHeap();
begin scalar CurrentItem;
    CurrentItem := HeapLowerBound;
    while CurrentItem < HeapLast do
    begin
	case Tag @CurrentItem of
	BTRTAG, CODE, NEGINT, POSINT, ID, UNBOUND:
	    CurrentItem := CurrentItem + 1;
	STR, BYTES, FIXN, FLTN, BIGN, WRDS, Halfwords, PAIR, VECT, EVECT:
	<<  if Inf @CurrentItem >= Hole and Inf @CurrentItem <= HeapLast then
		Inf @CurrentItem := Reloc Inf @CurrentItem;
	    CurrentItem := CurrentItem + 1 >>;
	HBYTES:
	    CurrentItem := CurrentItem + 1 + StrPack StrLen CurrentItem;
	HHalfwords:
	    CurrentItem := CurrentItem + 1 + HalfwordPack StrLen CurrentItem;
	HWRDS:
	    CurrentItem := CurrentItem + 1 + WrdPack WrdLen CurrentItem;
	HVECT:
	begin scalar Tmp;
	    Tmp := VecLen CurrentItem;
	    CurrentItem := CurrentItem + 1;	% Move over header
	    for I := 0 step 1 until Tmp do	% VecLen + 1 items
	    begin scalar Tmp2, Tmp3;
		Tmp2 := @CurrentItem;
		Tmp3 := Tag Tmp2;
		if PointerTagP Tmp3
			and Inf Tmp2 >= Hole and Inf Tmp2 <= HeapLast then
		    Inf @CurrentItem := Reloc Inf Tmp2;
		CurrentItem := CurrentItem + 1;
	    end;
	  end;
	HSKIP:
	    CurrentItem := CurrentItem + SkipLength CurrentItem;
	default:
	    GCError("Internal error in updating phase at %o", CurrentItem)
	end;	 % case
    end
end;

syslsp procedure UpdateItem Ptr;
begin scalar Tg, Info;
    Tg := Tag @Ptr;
    if not PointerTagP Tg then return;
    Info := INF @Ptr;
    if Info < Hole or Info > HeapLast then return;
    Inf @Ptr := Reloc Info;
end;

syslsp procedure CompactHeap();
begin scalar OldItemPtr, NewItemPtr, SegLen;
    if Hole < HeapLowerBound then return;
    NewItemPtr := OldItemPtr := Hole;
    while OldItemPtr < HeapLast do
      begin;
	case Tag @OldItemPtr of
	BTRTAG, CODE, NEGINT, POSINT, ID, UNBOUND,
	STR, BYTES, FIXN, FLTN, BIGN, WRDS, Halfwords, PAIR, VECT, EVECT:
	    SegLen := PairPack OldItemPtr;
	HBYTES:
	    SegLen := 1 + StrPack StrLen OldItemPtr;
	HHalfwords:
	    SegLen := 1 + HalfWordPack HalfwordLen OldItemPtr;
	HWRDS:
	    SegLen := 1 + WrdPack WrdLen OldItemPtr;
	HVECT:
	    SegLen := 1 + VectPack VecLen OldItemPtr;
	HSKIP:
	<<  OldItemPtr := OldItemPtr + SkipLength OldItemPtr;
	    goto WhileNext >>;
	default:
	    GCError("Internal error in compaction at %o", OldItemPtr)
	end;	 % case
	ClearMovementWithinSegment OldItemPtr;
	for I := 1 step 1 until SegLen do
	<<  @NewItemPtr := @OldItemPtr;
	    NewItemPtr := NewItemPtr + 1;
	    OldItemPtr := OldItemPtr + 1 >>;
    WhileNext:
      end;
end;

syslsp procedure GCError(Message, P);
<<  ErrorPrintF("***** Fatal error during garbage collection");
    ErrorPrintF(Message, P);
    while T do Quit; >>;

syslsp procedure GCMessage();
<<  ErrorPrintF("*** GC %w: time %d ms",
	LispVar GCKnt!*,  StartingRealTime);
    ErrorPrintF("*** %d recovered, %d stable, %d active, %d free",
		HeapShrink, Hole - HeapLowerBound,
					HeapLast - Hole,
					  intinf known!-free!-space() ) >>;

off SysLisp;

END;
