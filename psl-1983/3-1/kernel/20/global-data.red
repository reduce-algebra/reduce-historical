%
% GLOBAL-DATA.RED - Data used by everyone
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        1 September 1981
% Copyright (c) 1981 University of Utah
%

on SysLisp;

exported WConst MaxSymbols = 8000,
		HeapSize = 262000,
		MaxObArray = 8209,      % first prime above 8192
		StackSize = 10000,
		BPSSize = 170000;

exported WConst CompressedBinaryRadix = 8;

external WArray SymNam, SymVal, SymFnc, SymPrp;

external WVar NextSymbol;

exported WConst MaxRealRegs = 5,
		MaxArgs = 15;

external WArray ArgumentBlock;

external WArray HashTable;

off SysLisp;

END;
