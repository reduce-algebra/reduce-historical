%
% ALLOCATORS.RED - Low level storage management
% 
% Author:      Eric Benson
%              Computer Science Dept.
%              University of Utah
% Date:        27 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL>ALLOCATORS.RED.7, 23-Mar-83 11:35:37, Edit by KESSLER
%  Added OldHeapTrapBound to exported WVars, so we can update the heap trap
%  bound upon switch.
% Edit by Cris Perdue, 16 Feb 1983 1834-PST
% Pre-GC trap, known-free-space fns
%  <PSL.KERNEL>ALLOCATORS.RED.4, 10-Jan-83 15:50:50, Edit by PERDUE
%  Added GtEVect

on SysLisp;

external WArray BPS, Heap;

if_system(PDP10, <<			% For the compacting GC
exported WVar HeapLast = &Heap[0],	% pointer to next free slot in heap	
	      HeapLowerBound = &Heap[0],	% bottom of heap
	      HeapUpperBound = &Heap[HeapSize],
	      HeapTrapBound = &Heap[HeapSize]; % Value of HeapLast for trap
>>, <<
exported WVar HeapLast = &Heap[0],	% pointer to next free slot in heap	
	      HeapLowerBound = &Heap[0],	% bottom of heap
	      HeapUpperBound = &Heap[HeapSize/2], % end of active heap
	      OldHeapLast,
	      OldHeapLowerBound = &Heap[HeapSize/2 + 1],
	      OldHeapUpperBound = &Heap[HeapSize],
	      HeapTrapBound = &Heap[HeapSize/2], % Value of HeapLast for trap
	      OldHeapTrapBound = &Heap[HeapSize];
>>);
exported WVar HeapTrapped = NIL;	% Boolean: trap since last GC?


compiletime flag('(GtHeap1), 'InternalFunction);

syslsp procedure Known!-Free!-Space;
MkInt((HeapUpperBound - HeapLast)/AddressingUnitsPerItem);

syslsp procedure GtHEAP N;		%. get heap block of N words
if null N then known!-free!-space() else
    GtHeap1(N, NIL);

syslsp procedure GtHeap1(N, LastTryP);
begin scalar PrevLast;
    PrevLast := HeapLast;
    HeapLast := HeapLast + N*AddressingUnitsPerItem;
    if HeapLast > HeapTrapBound then
	if HeapLast > HeapUpperBound then
	<<  HeapLast := PrevLast;
	    if LastTryP then FatalError "Heap space exhausted"
	    else
	    <<  !%Reclaim();
		return GtHeap1(N, T) >> >>
	else
	%% From one GC to the next there can be at most 1 GC trap,
	%%  done the first time space gets "low".  %Reclaim resets
	%%  HeapTrapped to NIL.
	if HeapTrapped = NIL then
	    <<  HeapTrapped := T;
	        GC!-Trap() >>;
    return PrevLast
end;

syslsp procedure GC!-Trap!-Level;
MkInt (HeapUpperBound - HeapTrapBound)/AddressingUnitsPerItem;

syslsp procedure Set!-GC!-Trap!-Level N;
<<  if not IntP(N) then NonIntegerError(N, 'Set!-GC!-Trap!-Level);
    N := IntInf N;
    HeapTrapBound := HeapUpperBound - N*AddressingUnitsPerItem;
    T >>;

syslsp procedure DelHeap(LowPointer, HighPointer);
    if HighPointer eq HeapLast then HeapLast := LowPointer;

syslsp procedure GtSTR N;		%. Allocate space for a string N chars
begin scalar S, NW;
    S := GtHEAP((NW := STRPack N) + 1);
    @S := MkItem(HBytes, N);
    S[NW] := 0;				% clear last word, including last byte
    return S;
end;

syslsp procedure GtConstSTR N;	 %. allocate un-collected string for print name
begin scalar S, NW;			% same as GtSTR, but uses BPS, not heap
    S := GtBPS((NW := STRPack N) + 1);
    @S := N;
    S[NW] := 0;				% clear last word, including last byte
    return S;
end;

syslsp procedure GtHalfWords N;		%. Allocate space for N halfwords
begin scalar S, NW;
    S := GtHEAP((NW := HalfWordPack N) + 1);
    @S := MkItem(HHalfWords, N);
    return S;
end;

syslsp procedure GtVECT N;		%. Allocate space for a vector N items
begin scalar V;
    V := GtHEAP(VECTPack N + 1);
    @V := MkItem(HVECT, N);
    return V;
end;

Putd('GtEvect,'expr,cdr getd 'GtVect);

syslsp procedure GtWRDS N;		%. Allocate space for N untraced words
begin scalar W;
    W := GtHEAP(WRDPack N + 1);
    @W := MkItem(HWRDS, N);
    return W;
end;


syslsp procedure GtFIXN();		%. allocate space for a fixnum
begin scalar W;
    W := GtHEAP(WRDPack 0 + 1);
    @W := MkItem(HWRDS, 0);
    return W;
end;

syslsp procedure GtFLTN();		%. allocate space for a float
begin scalar W;
    W := GtHEAP(WRDPack 1 + 1);
    @W := MkItem(HWRDS, 1);
    return W;
end;

% NextSymbol and SymbolTableSize are globally declared

syslsp procedure GtID();		%. Allocate a new ID
%
% IDs are allocated as a linked free list through the SymNam cell,
% with a 0 to indicate the end of the list.
%
begin scalar U;
    if NextSymbol = 0 then 
    <<  Reclaim();
	if NextSymbol = 0 then
	    return FatalError "Ran out of ID space" >>;
    U := NextSymbol;
    NextSymbol := SymNam U;
    return U;
end;

exported WVar NextBPS = &BPS[0],
	      LastBPS = &BPS[BPSSize];

syslsp procedure GtBPS N;		%. Allocate N words for binary code
begin scalar B;
    if null N then return((LastBPS - NextBPS)/AddressingUnitsPerItem);
					% GTBPS NIL returns # left
    B := NextBPS;
    NextBPS := NextBPS + N*AddressingUnitsPerItem;
    return if NextBPS > LastBPS then
	StdError '"Ran out of binary program space"
    else B;
end;

syslsp procedure DelBPS(Bottom, Top);	%. Return space to BPS
    if NextBPS eq Top then NextBPS := Bottom;

syslsp procedure GtWArray N;	%. Allocate N words for WVar/WArray/WString
begin scalar B;
    if null N then return((LastBPS - NextBPS)/AddressingUnitsPerItem);
					% GtWArray NIL returns # left
    B := LastBPS - N*AddressingUnitsPerItem;
    return if NextBPS > B then
	StdError '"Ran out of WArray space"
    else
	LastBPS := B;
end;

syslsp procedure DelWArray(Bottom, Top);	%. Return space for WArray
    if LastBPS eq Bottom then LastBPS := Top;

off SysLisp;

END;
