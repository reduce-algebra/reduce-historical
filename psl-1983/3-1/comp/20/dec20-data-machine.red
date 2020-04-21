%
% 20-DATA-MACHINE.RED - Lisp item constructors & selectors for Dec-20 Syslisp
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        10 July 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.20-COMP>20-DATA-MACHINE.RED.1, 25-Feb-82 17:24:56, Edit by BENSON
%  Converted from VAX version (which was previously converted from 20 version!)

% Primitives handled by the compiler are BYTE, PUTBYTE, GETMEM, PUTMEM,
% MKITEM, FIELD, SIGNEDFIELD, PUTFIELD

fluid '(system_list!*);

system_list!* := '(ExtDec20 Tops20);

BothTimes <<
exported WConst TagStartingBit = 0,
		TagBitLength = 6,
		InfStartingBit = 6,
		InfBitLength = 30,
		GCStartingBit = 0,
		GCBitLength = 0,
		AddressingUnitsPerItem = 1,
		CharactersPerWord = 5,
		BitsPerWord = 36,
		AddressingUnitsPerFunctionCell = 1,
		StackDirection = 1;

>>;

syslsp macro procedure GCField U;
    list('Field, cadr U, '(WConst GCStartingBit), '(WConst GCBitLength));

syslsp macro procedure PutGCField U;
    list('PutField, cadr U, '(WConst GCStartingBit), '(WConst GCBitLength),
		    caddr U);

% Retrieve the address stored in the function cell and strip off 'JRST' part

syslsp macro procedure SymFnc U;
%    list ('Wshift, 
 %          list ('WShift, list('WGetV, '(WConst SymFnc), cadr U), 9),
  %         -9);
     list('Field, list('WGetV, '(WConst SymFnc), cadr U), 12, 24);

syslsp macro procedure PutSymFnc U;
% put JRST instr. part in table.
%   list('WPutV, '(WConst SymFnc), cadr U, '(Wor 8#254000000000, caddr U);
    list('WPutV, '(WConst SymFnc), cadr U, MkCode caddr U);
%   list('PutField, caddr U,'(Plus2 '(WConst SymFnc), cadr u), 9, 27);

% Macros for building stack pointers

syslsp macro procedure MakeStackPointerFromAddress U;
% when code resides in more than one section, the following will need to be
% changed to put the section number rather than a count in the left half
    list('WOr, list('WShift, list('WDifference, 0, caddr U), 18),
	       list('WDifference, cadr U, 1));

syslsp macro procedure MakeAddressFromStackPointer U;
%the next line will be the definition needed when code resides in more than
% one section.
%    list('Field, cadr U, InfStartingBit, InfBitLength);
%    list('Field, cadr U, 18, 18);	       
     list('Wor, list('Field, cadr U, 18, 18), 8#1000000);

put('AdjustStackPointer,'OpenFn,'(NonAssocPat !*ADJSP));

lisp procedure !*ADJSP(Arg1, Arg2);
    Expand2OperandCMacro(Arg1, Arg2, '!*ADJSP);

put('EOF, 'CharConst, char cntrl Z);

END;
