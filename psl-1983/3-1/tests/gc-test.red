% GC-TEST.RED - Test of P-COMP-GC Marking primitives
% M. L. Griss, 17 June 1983
% MAcros extracted for file, P-COMP-GC.RED

On Syslisp;

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

%/ External WArray GCArray;


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

syslsp procedure testmarking;
 begin
	Prin2T "---- Test GC MARK of various HEAP structures ----";
	Prin2T "     Examine each case carefully, see MARK go on and back off";
	Test1Mark cons(1 , 2); % Build a fresh one
	Test1Mark cons(- 1 , -2); % testing sign extend
	Test1Mark cons('A, 'B);
	Test1Mark '[0 1 2 3];
	Test1Mark "01234";
	TestIdmark 'A;
	TestIdmark 'JUNK;
	TestIdmark 'NIL;
	Prin2T "---- Mark tests all done ---     ";
 End;

syslsp procedure Test1Mark X;
 Begin scalar P;
	Prin2 ".... Object to mark:           "; Print X;
	P:=Inf X;
	Prin2 "     MARK field:               "; Print Mark P;
	Prin2 "     MARKED should be NIL:     "; Print Marked P;
	PrintBits @P;
	Prin2 "  .. SETMARK :                 "; Print SetMark P;
	Prin2 "     MARK field now:           "; Print Mark P;
	Prin2 "     MARKED should be T:       "; Print Marked P;
	PrintBits @P;
	Prin2 "  .. CLEARMARK:                "; Print ClearMark P;
	Prin2 "     MARK field finally:       "; Print Mark P;
	Prin2 "     MARKED should be NIL:     "; Print Marked P;
	PrintBits @P;
	Prin2 "  .. Object again legal:       "; Print X;
 End;

syslsp procedure TestIDMark X;
 Begin scalar P;
	Prin2 ".... ID to mark:               "; Print X;
	P:=IDInf X;
	Prin2 "     MARKEDID should be NIL:     "; Print MARKEDID P;
	PrintBits SYMNAM P;
	Prin2 "  .. MARKID :                    "; Print MarkId P;
	Prin2 "     MARKEDID should be T:       "; Print MARKEDID P;
	PrintBits SYMNAM P;
	Prin2 "  .. CLEARIDMARK:                "; Print Clearidmark P;
	Prin2 "     MARKEDID should be NIL:     "; Print MARKEDID P;
	PrintBits SYMNAM P;
	Prin2 "  .. ID again legal:             "; Print X;
 End;

syslsp procedure PrintBits x;
      <<Prin2 "     BitPattern:               ";
        Prin2 Tag x; 
        Prin2 ":     ";
        Prin2 Inf x;
        Terpri();
      >>;
off syslisp;

procedure GCTEST;
Begin scalar X,N,M;
	Prin2T "---- GTEST series -----";
	Prin2T ".... Try individual Types first ...";
	Prin2  "     Reclaim called:     "; Reclaim();
	Prin2  " ..  Allocate a PAIR:    "; Print (x:=cons(1,2));
	Prin2  "     Reclaim called:     "; Reclaim();
	Prin2  " ..  Release the PAIR:   "; Print (X:=NIL);

	Prin2  "     Reclaim called:     "; Reclaim();

	Prin2  " ..  Allocate a  VECTOR: "; Print (x:=Mkvect(4));
	Prin2  "     Reclaim called:     "; Reclaim();
	Prin2  " ..  Release the VECTOR: "; Print (X:=NIL);

	Prin2  "     Reclaim called:     "; Reclaim();

	Prin2  " ..  Allocate a STRING:  "; Print (x:=Mkstring(5,65));
	Prin2  "     Reclaim called:     "; Reclaim();
	Prin2  " ..  Release the STRING: "; Print (X:=NIL);

	Prin2  "     Reclaim called:     "; Reclaim();
	M:=2;
	Prin2 ".... Loop until RECLAIM automatically called :";
         Prin2 M; Prin2t " times";
        N:=GCknt!*+M;
	Prin2T  " ..  Loop on PAIRs:      ";
	   While GCKnt!* <=N do list(1,2,3,4,5,6,7,8,9,10);
        N:=GCknt!*+M;
	Prin2T  " ..  Loop on VECTORs:    ";
	   While GCknt!* <=N do MkVect 5;
        N:=GCknt!*+M;
	Prin2T  " ..  Loop on STRINGs:    ";
	   While GCKnt!* <=N do Mkstring(20,65);
End;

off syslisp;

End;
