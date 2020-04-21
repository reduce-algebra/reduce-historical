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

system_list!* := '(Dec20 PDP10 Tops20 KL10);

BothTimes <<
exported WConst TagStartingBit = 0,
		TagBitLength = 5,
		InfStartingBit = 18,
		InfBitLength = 18,
		GCStartingBit = 5,
		GCBitLength = 13,
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

% Retrieve the address stored in the function cell

syslsp macro procedure SymFnc U;
    list('WGetV, '(WConst SymFnc), cadr U);


syslsp macro procedure PutSymFnc U;
    list('WPutV, '(WConst SymFnc), cadr U, caddr U);

% Macros for building stack pointers

syslsp macro procedure MakeStackPointerFromAddress U;
    list('WOr, list('WShift, list('WDifference, 0, caddr U), 18),
	       list('WDifference, cadr U, 1));

syslsp macro procedure MakeAddressFromStackPointer U;
    list('Field, cadr U, 18, 18);

put('AdjustStackPointer,'OpenFn,'(NonAssocPat !*ADJSP));

lisp procedure !*ADJSP(Arg1, Arg2);
    Expand2OperandCMacro(Arg1, Arg2, '!*ADJSP);

put('EOF, 'CharConst, char cntrl Z);

END;
