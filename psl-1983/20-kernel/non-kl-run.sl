%
% NON-KL-RUN.SL - Extra runtime support for KI processors
% 
% Author:      Eric Benson
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        11 May 1982
% Copyright (c) 1982 University of Utah
%

% Basic problem is lack of ADJBP instruction

(lap '((!*entry Byte expr 2)
       (idivi 2 5)			% divide word offset by 5
       (add 2 1)			% add word address to word offset
       (ldb 1 (indexed 3 BytePointerTable))	% fetch byte using remainder
       (!*EXIT 0)
       (!*entry PutByte expr 3)
       (move 4 3)			% save byte in 4
       (idivi 2 5)
       (add 2 1)
       (dpb 4 (indexed 3 BytePointerTable))
       (!*EXIT 0)
BytePointerTable
       (fullword (FieldPointer (indexed 2 0) 0 7))
       (fullword (FieldPointer (indexed 2 0) 7 7))
       (fullword (FieldPointer (indexed 2 0) 14 7))
       (fullword (FieldPointer (indexed 2 0) 21 7))
       (fullword (FieldPointer (indexed 2 0) 28 7))
))

(lap '((!*entry BitTable expr 2)
       (idivi 2 18)			% divide word offset by 18
       (add 2 1)			% add word address to word offset
       (ldb 1 (indexed 3 BytePointerTable))	% fetch byte using remainder
       (!*EXIT 0)
       (!*entry PutBitTable expr 3)
       (move 4 3)			% save byte in 4
       (idivi 2 18)
       (add 2 1)
       (dpb 4 (indexed 3 BytePointerTable))
       (!*EXIT 0)
BytePointerTable
       (fullword (FieldPointer (indexed 2 0) 0 2))
       (fullword (FieldPointer (indexed 2 0) 2 2))
       (fullword (FieldPointer (indexed 2 0) 4 2))
       (fullword (FieldPointer (indexed 2 0) 6 2))
       (fullword (FieldPointer (indexed 2 0) 8 2))
       (fullword (FieldPointer (indexed 2 0) 10 2))
       (fullword (FieldPointer (indexed 2 0) 12 2))
       (fullword (FieldPointer (indexed 2 0) 14 2))
       (fullword (FieldPointer (indexed 2 0) 16 2))
       (fullword (FieldPointer (indexed 2 0) 18 2))
       (fullword (FieldPointer (indexed 2 0) 20 2))
       (fullword (FieldPointer (indexed 2 0) 22 2))
       (fullword (FieldPointer (indexed 2 0) 24 2))
       (fullword (FieldPointer (indexed 2 0) 26 2))
       (fullword (FieldPointer (indexed 2 0) 28 2))
       (fullword (FieldPointer (indexed 2 0) 30 2))
       (fullword (FieldPointer (indexed 2 0) 32 2))
       (fullword (FieldPointer (indexed 2 0) 34 2))
))

(lap '((!*entry HalfWord expr 2)
       (rot 2 -1)			% make halfword offset into word offset
       (add 1 2)			% add word base to word offset
       (jumpl 1 (lit (hrrz 1 (indexed 1 0))	% test sign bit (from rot)
		     (!*EXIT 0)))
       (hlrz 1 (indexed 1 0))
       (!*EXIT 0)
))

(lap '((!*entry PutHalfWord expr 3)
       (rot 2 -1)
       (add 1 2)
       (jumpl 1 (lit (hrrm 3 (indexed 1 0))
		     (!*EXIT 0)))
       (hrlm 3 (indexed 1 0))
       (!*EXIT 0)
))
