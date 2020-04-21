% MINI-RECLAIM.RED - RECLAIM stubs for TEST series

on syslisp;

External Wvar HeapLowerBound,
	      HeapUpperBound,
	      HeapLast;

Procedure !%Reclaim();
 <<Prin2 '" *** Dummy !%RECLAIM: ";
   HeapInfo()>>;

Procedure Reclaim();
 <<Prin2 '"*** Dummy RECLAIM: ";
   HeapInfo()>>;

Procedure HeapInfo();
<< Prin1 ((HeapLast-HeapLowerBound)/AddressingUnitsPerItem);
   Prin2 '" Items used, ";
   Prin1 ((HeapUpperBound -HeapLast)/AddressingUnitsPerItem);
   Prin2t '" Items left.";
  0>>;

off syslisp;

End;
