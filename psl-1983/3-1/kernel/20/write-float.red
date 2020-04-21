%
% WRITE-FLOAT.RED - format a floating point number into a string
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        26 November 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL-20>WRITE-FLOAT.RED.3, 28-Sep-82 15:44:53, Edit by BENSON
%  Changed DMOVE to 2 moves, so this will run on a KI10 Tenex

lap '((!*entry WriteFloat expr 2)		% convert float to string
%
% r1 is string pointer, r2 is pointer to 2 word float
% puts characters in string buffer with terminating null char and count
%
	(!*MOVE (reg 1) (reg t1))	% save pointer to string count
	(!*WPLUS2 (reg 1) (WConst 1))	% move to chars
	(hrli (reg 1) 8#440700)		% make r1 a byte pointer
	(!*MOVE (reg 1) (reg t2))	% save starting byte pointer
	(move (reg 3) (Indexed (reg 2) 1))  % load r2 and r3 with the number
	(move (reg 2) (Indexed (reg 2) 0))
	(move (reg 4) (lit (fullword 2#000010100000001000000000010000000000)))
					% fl%one + fl%pnt + 16 fl%rnd
	(dfout)
	(!*JUMP (Label Error))
	(!*MOVE (WConst -1) (reg 4))			% count := -1
Count
	(!*JUMPEQ (Label DoneCounting) (reg 1) (reg t2)) % byte pointers equal?
	(ibp (reg t2))
	(aoja (reg 4) Count)		% Count := Count + 1
DoneCounting
	(!*MOVE (reg 4) (MEMORY (reg t1) (WConst 0)))	% deposit count
	(!*MOVE (WConst 0) (reg 2))
	(idpb (reg 4) (reg 1))		% deposit null byte
	(!*EXIT 0)
Error
	(!*MOVE (QUOTE "Couldn't print float") (reg 1))
	(!*JCALL IOError)
);

END;
