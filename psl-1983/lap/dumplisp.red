%
% DUMPLISP.RED - Dump running Lisp into a file
% 
% Author:      Eric Benson
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        25 April 1982
% Copyright (c) 1982 University of Utah
%

%  <PSL.KERNEL-20>DUMPLISP.RED.2,  5-Oct-82 10:57:34, Edit by BENSON
%  Removed DumpFileName!* added filename arg to Dumplisp
%  <PSL.20-INTERP>DUMPLISP.RED.7,  3-Sep-82 10:22:46, Edit by BENSON
%  Fixed page boundary bug when unmapping stack

CompileTime <<

flag('(unmap!-space unmap!-pages save!-into!-file), 'InternalFunction);

>>;

on Syslisp;

external WVar HeapLast, HeapUpperBound, NextBPS, LastBPS, StackUpperBound;

syslsp procedure DumpLisp Filename;
<<  if not StringP Filename then
	StdError "Dumplisp requires a filename argument";
    Reclaim;
    unmap!-space(HeapLast, HeapUpperBound);
    unmap!-space(NextBPS, LastBPS);
    %% Add some slack to the end of the stack fo the call to unmap-space!
    unmap!-space(MakeAddressFromStackPointer ST + 10, StackUpperBound);
    save!-into!-file Filename >>;

syslsp procedure unmap!-space(Lo, Hi);
begin scalar LoPage, HiPage;
    LoPage := LSH(Lo + 8#777, -9);
    HiPage := LSH(Hi - 8#1000, -9);
    return if not (LoPage >= HiPage) then
	unmap!-pages(LoPage, HiPage - LoPage);
end;

lap '((!*entry unmap!-pages expr 2)
	(hrlzi 3 2#100000000000000000)	% pm%cnt in AC3
	(hrr 3 2)			% page count in rh AC3
	(hrlzi 2 8#400000)		% .fhslf in lh AC2
	(hrr 2 1)			% starting page in rh AC2
	(!*MOVE (WConst -1) (REG 1))	% -1 in AC1
	(pmap)				% do it
	(!*EXIT 0)
);

lap '((!*entry save!-into!-file expr 1)
	(!*MOVE (reg 1) (reg 5))	% save in 5
	(move 2 1)			% file name in 2
	(hrli 2 8#10700)		% make a byte pointer
	(hrlzi 1 2#100000000000000001)	% gj%fou + gj%sht
	(gtjfn)
	 (jrst CouldntOpen)
	(hrli 1 8#400000)		% .fhslf
	(hrrzi 2 2#101010000000000000)	% ss%cpy, ss%rd, ss%exe, all pages
	(hrli 2 -8#1000)		% for Release 4 and before, 1000 pages
%/ Change previous line to following line for extended addressing
%	(tlo 2 8#400000)		% large negative number
	(!*MOVE (WConst 0) (REG 3))
	(ssave)
	(!*MOVE (WConst 0) (REG 1))
	(!*EXIT 0)
CouldntOpen
	(!*MOVE '"Couldn't GTJFN `%w' for Dumplisp" (reg 1))
	(!*MOVE (reg 5) (reg 2))
	(!*CALL BldMsg)
	(!*JCALL StdError)
);

off Syslisp;

END;
