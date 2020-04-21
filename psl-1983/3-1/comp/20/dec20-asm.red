
% 20-ASM.RED - Dec-20 specific information for LAP-TO-ASM
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        5 January 1982
% Copyright (c) 1982 University of Utah
%

%  21-May-83 Mark R. Swanson
%    Added changes to support extended addressing.
%  <PSL.20-COMP>20-ASM.RED.1, 25-Feb-82 16:46:44, Edit by BENSON
%  Converted from VAX version

fluid '(CodeFileNameFormat!*
	DataFileNameFormat!*
	InputSymFile!*
	OutputSymFile!*
	CommentFormat!*
	LabelFormat!*
	ExternalDeclarationFormat!*
	ExportedDeclarationFormat!*
	FullWordFormat!*
	DoubleFloatFormat!*
	ReserveZeroBlockFormat!*
	ReserveDataBlockFormat!*
	DefinedFunctionCellFormat!*
	UndefinedFunctionCellInstructions!*
	MainEntryPointName!*
	!*MainFound
	CodeOut!*
	DataOut!*
	!*Lower
	ASMOpenParen!*
	ASMCloseParen!*
	NumericRegisterNames!*);

CodeFileNameFormat!* := "%w.mac";
DataFileNameFormat!* := "d%w.mac";
InputSymFile!* := "20.sym";
OutputSymFile!* := "20.sym";
GlobalDataFileName!* := "global-data.red"$
MainEntryPointName!* := 'MAIN!.;
NumericRegisterNames!* := '[0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15];
CommentFormat!* := "; %p%n";
LabelFormat!* := "%w:";
ExternalDeclarationFormat!* := "	extern %w%n";
ExportedDeclarationFormat!* := "	intern %w%n";
FullWordFormat!* := "	%e%n";	% FullWord expects %e for parameter
IndWordFormat!*:= "   IFIW %e%n"; % For extended addressing.
DoubleFloatFormat!* := "	%w%n	0%n";
ReserveZeroBlockFormat!* := "%w:	block %e%n";
ReserveDataBlockFormat!* := "	block %e%n";
DefinedFunctionCellFormat!* := "	jrst %w##%n";
UndefinedFunctionCellInstructions!* :=
	       '((jsp (reg t5) (Entry UndefinedFunction)));
ASMOpenParen!* := '!<;
ASMCloseParen!* := '!>;

DefList('((LAnd !&)
	  (LOr !!)
	  (LXor !^!!)
	  (LSH !_)), 'BinaryASMOp);

put('LNot, 'UnaryASMOp, '!^!-);

DefList('((t1 6)
	  (t2 7)
	  (t3 8)
	  (t4 9)
	  (t5 10)
	  (t6 11)
	  (nil 0)
	  (st 15)), 'RegisterName);

put('MkItem2, 'ASMExpressionFormat, "<%e_30>+<%e_18>+%e");
put('MkItem1, 'ASMExpressionFormat, "<%e_30>+%e");
put('MkItem, 'ASMExpressionFunction, 'ASMPseudoMkItem);

lisp procedure ASMPseudoMkItem U;
%
% (MkItem Tag Inf)
%
    if (second U) > 0 and (second U) < 15 % PointerTagP
    then % use a format that generates a global address 
      PrintExpression List('MkItem2, second U, 1, third U) % force section
							   % # to 1
    else
      PrintExpression List('MkItem1, second U, third U);

lisp procedure CodeFileHeader();
    CodePrintF "	search monsym,macsym%n	radix 10%n";

lisp procedure DataFileHeader();
    DataPrintF "	radix 10%n";

lisp procedure CodeFileTrailer();
    CodePrintF(if !*MainFound then "	end MAIN.%n" else "	end%n");

lisp procedure DataFileTrailer();
    DataPrintF "	end%n";

lisp procedure CodeBlockHeader();
    NIL;

lisp procedure CodeBlockTrailer();
    NIL;

lisp procedure DataAlignFullWord();
    NIL;

lisp procedure PrintString S;
begin scalar N;
    N := Size S;
    PrintF "	byte(7)";
    for I := 0 step 1 until N do
    <<  PrintExpression Indx(S, I);
	Prin2 '!, >>;
    PrintExpression 0;
    TerPri();
end;

lisp procedure PrintByteList L;
    if null L then NIL else
    <<  PrintF "	byte(7)";
	while cdr L do
	<<  PrintExpression car L;
	    Prin2 '!,;
	    L := cdr L >>;
	PrintExpression car L;
	TerPri() >>;

lisp procedure PrintByte X;
<<  PrintF "	byte(7)";
    PrintExpression X;
    TerPri() >>;

lisp procedure PrintHalfWordList L;
    if null L then NIL else
    <<  PrintF "	byte(18)";
	while cdr L do
	<<  PrintExpression car L;
	    Prin2 '!,;
	    L := cdr L >>;
	PrintExpression car L;
	TerPri() >>;

lisp procedure PrintOpcode X;
    Prin2 X;

lisp procedure SpecialActionForMainEntryPoint();
%
% "Hardwire" HEAPs into sections 2 & 4; code modifies self to avoid
% recreating sections on re-entry.

  <<DataPrintF("        intern HEAP%n        HEAP=2,,0%n");
    DataPrintF("        intern HEAP2%n        HEAP2=4,,0%n");
    CodePrintF "	intern MAIN.%nMAIN.:";
    CodePrintF "	reset%% %n";
    CodePrintF "	setzm 1%n";          % initially create sections 2,3,4
    CodePrintF "	move 2,[.fhslf,,2]%n";
    CodePrintF "	move 3,[140000,,3]%n";
    CodePrintF "smap.:  smap%%%n";
    CodePrintF "        move 1,[jfcl]%n";    % make sure it only happens once
    CodePrintF "        movem 1,smap.%n";>>; % by stuffing a NOOP instruction
    
lisp procedure ASMSymbolP X;
    Radix50SymbolP(if IDP X then ID2String X else X);

lisp procedure Radix50SymbolP X;
begin scalar N, C, I;
    N := Size X;
    if N > 5 then return NIL;
    C := Indx(X, 0);
    if not (C >= char A and C <= char Z
		or C = char !% or C = char !. or C = char !$) then return NIL;
    I := 1;
Loop:
    if I > N then return T;
    C := Indx(X, I);
    if not (C >= char A and C <= char Z
		or C >= char !0 and C <= char !9
		or C = char !% or C = char !. or C = char !$) then return NIL;
    I := I + 1;
    goto Loop;
end;

lisp procedure PrintNumericOperand X;
    if ImmediateP X then Prin2 X else PrintF("[%w]", X);

lisp procedure OperandPrintIndirect X;
<<  Prin2 '!@;
    PrintOperand cadr X >>;

put('Indirect, 'OperandPrintFunction, 'OperandPrintIndirect);

lisp procedure OperandPrintIndexed X;
<<  X := cdr X;
    PrintExpression cadr X;
    Prin2 '!(;
    PrintOperand car X;
    Prin2 '!) >>;

put('Indexed, 'OperandPrintFunction, 'OperandPrintIndexed);

macro procedure Immediate X;		% immediate does nothing on the 20
    cadr X;

lisp procedure ASMPseudoFieldPointer U;
%
% (FieldPointer Operand StartingBit Length)
%
<<  U := cdr U;
    Prin2 "point ";
    PrintExpression third U;
    Prin2 '!, ;
    PrintOperand first U;
    Prin2 '!, ;
    PrintExpression list('difference, list('plus2, second U, third U), 1) >>;

put('FieldPointer, 'ASMExpressionFunction, 'ASMPseudoFieldPointer);

procedure MCPrint(x); % Echo of MC's
 CodePrintF(";     %p%n",x);

procedure InstructionPrint(x);
 CodePrintF( ";          %p%n",x);

procedure !*cerror x;
 begin scalar i;
    i:=wrs Nil;
    printf( "%n *** CERROR: %r %n ",x);
    wrs i;
    return list list('cerror,x);
 end;

put('cerror,'asmpseudoop,'printcomment);

DefCmacro !*cerror;

END;
