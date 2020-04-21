%
% 20-FASLIN.RED - Functions needed by faslin
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        21 April 1982
% Copyright (c) 1982 University of Utah
%

%  21-May-83  Mark R. Swanson
%    Changed reference to &SYMFNC in FunctionCellLocation to be an explicit
%    array reference.
%  <PSL.KERNEL-20>SYSTEM-FASLIN.RED.4,  7-Oct-82 13:37:56, Edit by BENSON
%  Changed 0 byte size to 36 byte size, for Tenex compatibility

on Syslisp;

syslsp procedure BinaryOpenRead FileName;
begin scalar F;
    F := Dec20Open(FileName,
		     %  gj%old	    gj%sht
		     2#001000000000000001000000000000000000,
		     % 36*of%bsz	of%rd
		     2#100100000000000000010000000000000000);
    return if F eq 0 then
	ContError(99, "Couldn't open binary file for input",
			BinaryOpenRead FileName)
    else F;
end;

syslsp procedure BinaryOpenWrite FileName;
begin scalar F;
    F := Dec20Open(FileName,
		    % gj%fou gj%new gj%sht
		    2#110000000000000001000000000000000000,
		    % 36*of%bsz		of%wr
		    2#100100000000000000001000000000000000);
    return if F eq 0 then
	ContError(99, "Couldn't open binary file for output",
			BinaryOpenWrite FileName)
    else F;
end;

syslsp procedure ValueCellLocation X;
    if not LispVar !*WritingFaslFile then
	&SymVal IDInf X
    else
    <<  LispVar NewBitTableEntry!* := const RELOC_HALFWORD;
	MakeRelocHalfWord(const RELOC_VALUE_CELL, FindIDNumber X) >>;

syslsp procedure ExtraRegLocation X;
<<  X := second X;
    if not LispVar !*WritingFaslFile then
	&ArgumentBlock[X - (MaxRealRegs + 1)]
    else
    <<  LispVar NewBitTableEntry!* := const RELOC_HALFWORD;
	MakeRelocHalfWord(const RELOC_VALUE_CELL, X + 8150) >> >>;

syslsp procedure FunctionCellLocation X;
    if not LispVar !*WritingFaslFile then
	&SymFnc[IDInf X]    % different from VALUECELLLOCATION because of
			    % strange interaction with SymFnc as a function?
    else
    <<  LispVar NewBitTableEntry!* := const RELOC_HALFWORD;
	MakeRelocHalfWord(const RELOC_FUNCTION_CELL, FindIDNumber X) >>;

off SysLisp;

END;
