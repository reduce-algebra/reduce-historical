%
% MAIN-START.RED - First routine called on startup
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        15 September 1981
% Copyright (c) 1981 University of Utah
%

%  26-May-1983 Mark R. Swanson
%  Cahnges to support extended addressing
%  <PSL.KERNEL-20>MAIN-START.RED.4,  5-Oct-82 10:42:14, Edit by BENSON
%  Added call to EvalInitForms in MAIN!.

on SysLisp;

internal WConst StackSize = 4000;

internal WArray Stack[StackSize];

exported WVar StackLowerBound = &Stack[0] + 8#1000000,
	      StackUpperBound = &Stack[StackSize] + 8#1000000;

external WVar ST;

internal WConst MaxArgBlock = (MaxArgs - MaxRealRegs) - 1;

% 0..MaxArgBlock are arguments (MaxRealRegs + 1)..MaxArgs

exported WArray ArgumentBlock[MaxArgBlock];

exported WArray HashTable[MaxObArray/2];

lap '((!*entry Main!. expr 0)
Forever
	(move (reg st) (lit (halfword (minus (WConst StackSize))
				      (difference (WConst Stack) 1))))
	(move (reg nil) (fluid nil))
	(!*CALL pre!-main)
	(jrst Forever)
);

syslsp procedure Reset();
    Throw('Reset, 'Reset);

syslsp procedure pre!-main();
<<  ClearBindings();
    ClearIO();
    EvalInitForms();
    if Catch('Reset, Main()) = 'Reset then pre!-main() >>;

syslsp procedure Main();		%. initialization function
%
% A new system can be created by redefining this function to call whatever
% top loop is desired.
%
<<  InitCode();				% special code accumulated in compiler
    SymFnc[IDLoc Main] := SymFnc[IDLoc StandardLisp];	% don't do it again
    StandardLisp() >>;

off SysLisp;

END;
