%
% FASL-INCLUDE.RED - data declarations for FASL at compile time
% 
% Author:      Eric Benson
%              Computer Science Dept.
%              University of Utah
% Date:        20 February 1982
% Copyright (c) 1982 Eric Benson
%

on SysLisp;

CompileTime <<

DefConst(FASL_MAGIC_NUMBER, 99);
		     
DefConst(RELOC_ID_NUMBER, 1,
	 RELOC_VALUE_CELL, 2,
	 RELOC_FUNCTION_CELL, 3);

DefConst(RELOC_WORD, 1,
	 RELOC_HALFWORD, 2,
	 RELOC_INF, 3);

smacro procedure RelocRightHalfTag X;
    Field(X, BitsPerWord/2, 2);

smacro procedure RelocRightHalfInf X;
    Field(X, BitsPerWord/2+2, BitsPerWord/2-2);

smacro procedure RelocInfTag X;
    Field(X, InfStartingBit, 2);

smacro procedure RelocInfInf X;
    Field(X, InfStartingBit+2, InfBitLength-2);

smacro procedure RelocWordTag X;
    Field(X, 0, 2);

smacro procedure RelocWordInf X;
    Field(X, 2, BitsPerWord-2);

>>;

off Syslisp;

END;
