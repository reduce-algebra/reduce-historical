% MINI-ALLOC.RED : Crude Mini Allocator and support
%            See PT:P-ALLOCATORS.RED
% Revisions: MLG, 18 Feb,1983
%	     Moved HEAP declaration to XXX-HEADER 
%            Had to provide an InitHeap routine
%            (or will be LoadTime :=)
on syslisp;

external Wvar HeapLowerBound, HeapUpperBound;

external WVar HeapLast,			        % next free slot in heap	
	      HeapPreviousLast;			% save start of new block

syslsp procedure GtHEAP N;		        
%  get heap block of N words
if null N then (HeapUpperBound - HeapLast) / AddressingUnitsPerItem else
<<  HeapPreviousLast := HeapLast;
    HeapLast := HeapLast + N*AddressingUnitsPerItem;
    if HeapLast > HeapUpperBound then
    <<  !%Reclaim();
	HeapPreviousLast := HeapLast;
	HeapLast := HeapLast + N*AddressingUnitsPerItem;
	if HeapLast > HeapUpperBound then
	    FatalError "Heap space exhausted" >>;
    HeapPreviousLast >>;

syslsp procedure GtSTR N;		
%  Allocate space for a string N chars
begin scalar S, NW;
    S := GtHEAP((NW := STRPack N) + 1);
    @S := MkItem(HBytes, N);
    S[NW] := 0;				% clear last word, including last byte
    return S;
end;

syslsp procedure GtVECT N;		
%  Allocate space for a vector N items
begin scalar V;
    V := GtHEAP(VECTPack N + 1);
    @V := MkItem(HVECT, N);
    return V;
end;

Procedure GtWarray N;  
% Dummy for Now, since no GC
 GtVect N;

Procedure GtID();
% Simple ID Allocator
 Begin scalar D;
  D:=NextSymbol;
  NextSymbol:=NextSymbol+1;
  return D;
 End;

Off syslisp;

End;
