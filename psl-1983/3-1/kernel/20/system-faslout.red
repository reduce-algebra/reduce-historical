%
% 20-FASLOUT.RED - 20-specific stuff for FASL
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        20 February 1982
% Copyright (c) 1982 University of Utah
%
%  21-May-1983  Mark R. Swanson
%   Changed FASL_MAGIC_NUMBER to 2099 to differentiate Extended-20 fasl
%   files from one-section fasl files.

CompileTime DefConst(AddressingUnitsPerItem, 1,
		     BitTableEntriesPerWord, 18,
		     FASL_MAGIC_NUMBER, 2099,
		     RELOC_ID_NUMBER, 1,
		     RELOC_VALUE_CELL, 2,
		     RELOC_FUNCTION_CELL, 3,
		     RELOC_WORD, 1,
		     RELOC_HALFWORD, 2,
		     RELOC_INF, 3);

on SysLisp;

CompileTime <<
smacro procedure RelocRightHalfTag X;
    Field(X, 18, 2);

smacro procedure RelocRightHalfInf X;
    Field(X, 20, 16);

smacro procedure RelocInfTag X;
    Field(X, 18, 2);

smacro procedure RelocInfInf X;
    Field(X, 20, 16);

smacro procedure RelocWordTag X;
    Field(X, 0, 2);

smacro procedure RelocWordInf X;
    Field(X, 2, 34);

smacro procedure PutRightHalf(Where, What);
    PutField(Where, 18, 18, What);

put('RightHalf, 'Assign!-Op, 'PutRightHalf);
>>;

CompileTime DefList('((BinaryWrite ((bout)))
		      (BinaryRead ((bin) (move (reg 1) (reg 2))))
		      (BinaryClose ((closf) (jfcl)))
		      (BinaryWriteBlock
%		                   ((hrli (reg 2) 8#740000)	% point 18,
%		                    (movns (reg 3))
%		                    (lsh 3,1)          	        % times 2
% for extended addressing, the following code should only work if it and
% the input buffer are in the same section, otherwise, something like the
% above must be implemented, i.e., a global byte pointer would be needed.
%
				   ((hrli (reg 2) 8#444400)	% point 36,
				    (movns (reg 3))
				    (sout)))
		      (BinaryReadBlock
				   ((hrli (reg 2) 8#444400)	% point 36,
				    (movns (reg 3))
				    (sin)))), 'OpenCode);

off Syslisp;

END;
