% 20-TEST-GLOBAL-DATA - Data used by everyone, test version
% 
% Author:      Eric Benson, M Griss, S Lowder
%              Computer Science Dept.
%              University of Utah
% Date:        1 September 1981
% Copyright (c) 1981 University of Utah

on SysLisp;

% For testing with MAINn, see P20T:XXX-HEADER.RED
% Want a small SYMTAB and HEAP

exported WConst MaxSymbols = 800,	% Use 500 upto MAIN7
 		MaxChannels = 31,
		MaxObArray = 800,	% Use 500 upto MAIN7
                MaxRealRegs = 5,
		MaxArgs = 15;

% BitPositions for testing, etc:

exported Wconst BitsPerWord=36;

% The STACK stuff
external WVAR ST, StackLowerBound, StackUpperBound;

% "standard" Symbol table Data structures, handled
% specially in Compiler

external Warray Symnam,SymVal,SymFnc,SymPrp;
external WVar NextSymbol;

% For extra arguments not in Real registers
external WArray ArgumentBlock;

% For the Foreign Function Calling Protocol

external Wvar Arg1,Arg2,Arg3,Arg4,Arg5,Arg6,Arg7,Arg8,Arg9,
              Arg10,Arg11,Arg12,Arg13,Arg14,Arg15;

external Warray HashTable;

off SysLisp;

END;
