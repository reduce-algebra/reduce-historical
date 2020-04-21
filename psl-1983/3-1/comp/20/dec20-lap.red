%
% 20-LAP.RED - Dec-20 PSL assembler
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        1 February 1982
% Copyright (c) 1982 University of Utah
%

% 27-May-1983 Mark R. Swanson
%  Added IndWord support for Extended adressing on -20

fluid '(LabelOffsets!* CurrentOffset!* CodeSize!* CodeBase!* Entries!*
	ForwardInternalReferences!*
	NewBitTableEntry!* LapReturnValue!*
	!*WritingFaslFile InitOffset!* !*PGWD !*PWrds);

CompileTime <<

flag('(SaveEntry DefineEntries DepositInstruction
       OpcodeValue OperandValue DepositWord DepositWordExpression
       DepositHalfWords LabelValue DepositItem DepositHalfWordIDNumber
       FindLabels OneLapLength MakeRelocInf MakeRelocWord),
     'InternalFunction);

smacro procedure LabelP X;
    atom X;

>>;

LoadTime <<

!*PWrds := T;

>>;

lisp procedure Lap U;
begin scalar LapReturnValue!*, LabelOffsets!*, Entries!*;
    if not !*WritingFaslFile then
	CurrentOffset!* := 0;
    U := Pass1Lap U;
    FindLabels U;
    if !*PGWD then for each X in U do
	if atom X then Prin2 X else PrintF("		%p%n", X);
    if not !*WritingFaslFile then
	CodeBase!* := GTBPS CodeSize!*;
    for each X in U do
	if not LabelP X then
	    if first X = '!*entry then SaveEntry X
	    else DepositInstruction X;
    DefineEntries();
    if not !*WritingFaslFile and !*PWrds then
	ErrorPrintF("*** %p: base %o, length %d words",
		for each X in Entries!* collect first car X,
				CodeBase!*, CodeSize!*);
    return MkCODE LapReturnValue!*;
end;

lisp procedure SaveEntry X;
    if second X = '!*!*!*Code!*!*Pointer!*!*!* then
	LapReturnValue!* :=		% Magic token that tells LAP to return
	    (if !*WritingFaslFile then CurrentOffset!*	%  a code pointer
		else IPlus2(CodeBase!*, CurrentOffset!*))
    else if not !*WritingFaslFile then
    <<  Entries!* := (rest X . CurrentOffset!*) . Entries!*;
	if not LapReturnValue!* then LapReturnValue!* :=
	    IPlus2(CodeBase!*, CurrentOffset!*) >>
    else if second X = '!*!*Fasl!*!*InitCode!*!* then
	InitOffset!* := CurrentOffset!*
    else if FlagP(second X, 'InternalFunction) then
	put(second X, 'InternalEntryOffset, CurrentOffset!*)
    else
    <<  FindIDNumber second X;
	DFPrintFasl list('PutEntry, MkQuote second X,
				    MkQuote third X,
				    CurrentOffset!*) >>;

lisp procedure DefineEntries();
    for each X in Entries!* do
	PutD(first car X, second car X, MkCODE IPlus2(CodeBase!*, cdr X));

lisp procedure DepositInstruction X;
%
% Legal forms are:
%  (special_form . any)
%  (opcode)
%  (opcode address)
%  (opcode ac address)
%
begin scalar Op, Y, A, E;
    return if (Y := get(first X, 'InstructionDepositFunction)) then
	Apply(Y, list X)
    else
    <<  NewBitTableEntry!* := 0;
	Op := OpcodeValue first X;
	if null(Y := rest X) then
	    A := E := 0
	else
	<<  E := OperandValue first Y;
	    if null(Y := rest Y) then
		A := 0
	    else
	    <<  A := E;
		E := OperandValue first Y >> >>;
	UpdateBitTable(1, NewBitTableEntry!*);
	DepositAllFields(Op, A, E) >>;
end;

lisp procedure DepositAllFields(Op, A, E);
<<  @IPlus2(CodeBase!*, CurrentOffset!*) :=
	ILOR(ILSH(Op, 27), ILOR(ILSH(A, 23), E));
    CurrentOffset!* := IAdd1 CurrentOffset!* >>;

lisp procedure OpcodeValue U;
    if PosIntP U then U
    else get(U, 'OpcodeValue) or StdError BldMsg("Unknown opcode %r", U);

lisp procedure OperandValue U;
%
% Legal forms are:
% number
% other atom (label)
% (special . any)	fluid, global, etc.
% (indexed register address)
% (indirect other_op)
%
begin scalar X;
    return if PosIntP U then U
    else if NegIntP U then ILAND(U, 8#777777)
    else if LabelP U then ILAND(LabelValue U, 8#777777)
    else if (X := get(first U, 'OperandValueFunction)) then
	Apply(X, list U)
    else if (X := WConstEvaluable U) then OperandValue X
    else StdError BldMsg("Unknown operand %r", U);
end;

lisp procedure BinaryOperand U;
%
% (op x x) can occur in expressions
%
begin scalar X;
    return if (X := WConstEvaluable U) then X
    else
    <<  X := if GetD first U then first U else get(first U, 'DOFN);
	U := rest U;
	if NumberP first U then
	    Apply(X, list(first U, LabelValue second U))
	else if NumberP second U then
	    Apply(X, list(LabelValue first U, second U))
	else StdError BldMsg("Expression too complicated in LAP %r", U) >>;
end;

% Add others to this list if they arise

put('difference, 'OperandValueFunction, 'BinaryOperand);
put('WPlus2, 'OperandValueFunction, 'BinaryOperand);

lisp procedure RegisterOperand U;
begin scalar V;
    U := second U;
    return if PosIntP U then U
    else if (V := get(U, 'RegisterNumber)) then V
    else StdError BldMsg("Unknown register %r", U);
end;

put('REG, 'OperandValueFunction, 'RegisterOperand);

DefList('((nil 0)
	  (t1 6)
	  (t2 7)
	  (t3 8)
	  (t4 9)
	  (t5 10)
	  (t6 11)
	  (st 8#17)), 'RegisterNumber);

lisp procedure ImmediateOperand U;
    OperandValue second U;		% immediate does nothing on the PDP10

put('immediate, 'OperandValueFunction, 'ImmediateOperand);

lisp procedure IndexedOperand U;
begin scalar V;
    V := OperandValue second U;
    U := OperandValue third U;
    return ILOR(ILSH(V, 18), U);
end;

put('indexed, 'OperandValueFunction, 'IndexedOperand);

lisp procedure LapValueCell U;
    ValueCellLocation second U;

DefList('((fluid LapValueCell)
	  (!$fluid LapValueCell)
	  (global LapValueCell)
	  (!$global LapValueCell)), 'OperandValueFunction);

lisp procedure LapEntry U;
    FunctionCellLocation second U;

put('entry, 'OperandValueFunction, 'LapEntry);

lisp procedure LapInternalEntry U;
begin scalar X;
    U := second U;
    NewBitTableEntry!* := const RELOC_HALFWORD;
    return if (X := Atsoc(U, LabelOffsets!*)) then
    <<  X := cdr X;
	if !*WritingFaslFile then X else IPlus2(CodeBase!*, X) >>
    else
    <<  if not !*WritingFaslFile then FunctionCellLocation U
	else if (X := get(U, 'InternalEntryOffset)) then X
	else
	<<  ForwardInternalReferences!* :=
		   (CurrentOffset!* . U) . ForwardInternalReferences!*;
	    0 >> >>;			% will be modified later
end;

put('InternalEntry, 'OperandValueFunction, 'LapInternalEntry);

lisp procedure DepositWordBlock X;
    for each Y in cdr X do DepositWordExpression Y;

put('fullword, 'InstructionDepositFunction, 'DepositWordBlock);
put('indword, 'InstructionDepositFunction, 'DepositIndWord);

lisp procedure DepositIndWord X;
begin scalar Infpart;
    InfPart := cadr X;
    if not !*WritingFaslFile then
    DepositWord MkItem(8#40,ILAND(8#777777, LabelValue InfPart))

    else
    <<  if LabelP InfPart then
	    @IPlus2(CodeBase!*, CurrentOffset!*) := % RELOC_CODE_OFFSET = 0
				MkItem(8#40, LabelValue InfPart);
	CurrentOffset!* := IAdd1 CurrentOffset!*;
	UpdateBitTable(1, const RELOC_HALFWORD) >>;
end;

lisp procedure DepositHalfWordBlock X;
begin scalar L, R;
    X := rest X;
    while not null X do
    <<  L := first X;
	X := rest X;
	if null X then
	   R := 0
	else
	<<  R := first X;
	    X := rest X >>;
	DepositHalfWords(L, R) >>;
end;

put('halfword, 'InstructionDepositFunction, 'DepositHalfWordBlock);

CommentOutCode <<
lisp procedure DepositByteBlock X;
    case length X of
    0: DepositWord 0;
    1: DepositBytes(first X, 0, 0, 0, 0);
    2: DepositBytes(first X, second X, 0, 0, 0);
    3: DepositBytes(first X, second X, third X, 0, 0);
    4: DepositBytes(first X, second X, third X, fourth X, 0);
    default:
    <<  DepositBytes(first X, second X, third X, fourth X, fourth rest X);
	DepositByteBlock rest rest rest rest rest X >>;
    end;

put('byte, 'InstructionDepositFunction, 'DepositByteBlock);
>>;

lisp procedure DepositString X;
begin scalar Y;
    X := StrInf second X;
    Y := StrPack StrLen X;
    for I := 1 step 1 until Y do DepositWord @IPlus2(X, I);
end;

put('string, 'InstructionDepositFunction, 'DepositString);

lisp procedure DepositFloat X;		% this will not work in cross-assembly
<<  X := second X;			% don't need to strip tag on PDP10
    DepositWord FloatHighOrder X;
    DepositWord FloatLowOrder X >>;

put('float, 'InstructionDepositFunction, 'DepositFloat);

lisp procedure DepositWord X;
<<  @IPlus2(CodeBase!*, CurrentOffset!*) := X;
    UpdateBitTable(1, 0);
    CurrentOffset!* := IAdd1 CurrentOffset!* >>;

lisp procedure DepositWordExpression X;	% Only limited expressions now handled
begin scalar Y;
    return if FixP X then DepositWord Int2Sys X
    else if LabelP X then
    <<  @IPlus2(CodeBase!*, CurrentOffset!*) := LabelValue X;
	UpdateBitTable(1, const RELOC_HALFWORD);
	CurrentOffset!* := IAdd1 CurrentOffset!* >>
    else if first X = 'MkItem then DepositItem(second X, third X)
    else if first X = 'FieldPointer then
	DepositFieldPointer(second X, third X, fourth X)
    else if (Y := WConstEvaluable X) then DepositWord Int2Sys Y
    else StdError BldMsg("Expression too complicated %r", X);
end;

lisp procedure DepositHalfWords(L, R);
begin scalar Y;
    if not (FixP L or (L := WConstEvaluable L))
	then StdError "Left half too complex";
    if PairP R and first R = 'IDLoc then
	DepositHalfWordIDNumber(L, second R)
    else if (Y := WConstEvaluable R) then DepositWord ILOR(ILSH(L, 18), Y)
    else StdError BldMsg("Halfword expression too complicated %r", R);
end;

lisp procedure LabelValue U;
begin scalar V;
    return if CodeP U then Inf U
    else if (V := Atsoc(U, LabelOffsets!*)) then
    <<  V := cdr V;
	if !*WritingFaslFile then
	<<  NewBitTableEntry!* := const RELOC_HALFWORD;
	    V >>
	else IPlus2(CodeBase!*, V) >>
    else StdError BldMsg("Unknown label %r in LAP", U);
end;

lisp procedure DepositItem(TagPart, InfPart);
    if not !*WritingFaslFile then
    DepositWord MkItem(TagPart, if LabelP InfPart then
				    LabelValue InfPart
				else if first InfPart = 'IDLoc then
				    IDInf second InfPart
				else
				    StdError BldMsg("Unknown inf in MkItem %r",
								     InfPart))
    else
    <<  if LabelP InfPart then
	    @IPlus2(CodeBase!*, CurrentOffset!*) :=	% RELOC_CODE_OFFSET = 0
				MkItem(TagPart, LabelValue InfPart)
	else if first InfPart = 'IDLoc then
	    @IPlus2(CodeBase!*, CurrentOffset!*) :=
			MkItem(TagPart,
			       MakeRelocInf(const RELOC_ID_NUMBER,
					    FindIDNumber second InfPart))
	else StdError BldMsg("Unknown inf in MkItem %r", InfPart);
	CurrentOffset!* := IAdd1 CurrentOffset!*;
	UpdateBitTable(1, const RELOC_INF) >>;

lisp procedure DepositHalfWordIDNumber(LHS, X);
    if not !*WritingFaslFile or ILEQ(IDInf X, 128) then
	DepositWord ILOR(ILSH(LHS, 18), IDInf X)
    else
    <<  @IPlus2(CodeBase!*, CurrentOffset!*) := ILOR(ILSH(LHS, 18),
		MakeRelocHalfWord(const RELOC_ID_NUMBER, FindIDNumber X));
	CurrentOffset!* := IAdd1 CurrentOffset!*;
	UpdateBitTable(1, const RELOC_HALFWORD) >>;

lisp procedure SystemFaslFixup();
<<  while not null ForwardInternalReferences!* do
    <<  Field(@IPlus2(CodeBase!*,
		      car first ForwardInternalReferences!*),
	      18, 18) :=
	    get(cdr first ForwardInternalReferences!*, 'InternalEntryOffset)
		or <<  ErrorPrintF(
"***** %r not defined in this module; normal function call being used",
	cdr first ForwardInternalReferences!*);
		MakeRelocHalfWord(const RELOC_FUNCTION_CELL,
				  FindIDNumber cdr first
					ForwardInternalReferences!*) >>;
	ForwardInternalReferences!* := cdr ForwardInternalReferences!* >>;
    MapObl function lambda(X);
	RemProp(X, 'InternalEntryOffset) >>;
			

fluid '(LapCodeList!*);

lisp procedure FindLabels LapCodeList!*;
<<  CodeSize!* := 0;
    for each X in LapCodeList!* do
	CodeSize!* := IPlus2(CodeSize!*, OneLapLength X) >>;

lisp procedure OneLapLength U;
begin scalar X;
    return if atom U then
    <<  LabelOffsets!* := (U . IPlus2(CurrentOffset!*, CodeSize!*))
				. LabelOffsets!*;
	0 >>
    else if (X := get(car U, 'LapLength)) then
	if PosIntP X then X
	else Apply(X, list U)
    else				% minor klugde for long constants
    <<  if length U = 3 and FixP(X := third U) and not ImmediateP X then
	begin scalar Y;
	    RPlaca(rest rest U, Y := StringGensym());
	    NConc(LapCodeList!*, list(Y, list('fullword, X)));
	end;
    1 >>;
end;

DefList('((!*entry LapEntryLength)
	  (float 2)
	  (string LapStringLength)
	  (fullword LapWordLength)
	  (halfword LapHalfwordLength)
	  (byte LapByteLength)), 'LapLength);

lisp procedure LapEntryLength U;
<<  LabelOffsets!* := (second U . IPlus2(CurrentOffset!*, CodeSize!*))
			. LabelOffsets!*;
    0 >>;

lisp procedure LapStringLength U;
    StrPack StrLen StrInf second U;

lisp procedure LapWordLength U;
    length rest U;

lisp procedure LapHalfwordLength U;
    ILSH(IAdd1 length rest U, -1);

lisp procedure LapByteLength U;
    StrPack length rest U;

on SysLisp;

syslsp procedure DepositFieldPointer(Opr, Start, Len);
<<  LispVar NewBitTableEntry!* := 0;
    Opr := OperandValue Opr;
    @IPlus2(LispVar CodeBase!*, LispVar CurrentOffset!*) :=
	ILOR(ILSH(36 - (Start + Len), 30), ILOR(ILSH(Len, 24), Opr));
    UpdateBitTable(1, LispVar NewBitTableEntry!*);
    LispVar CurrentOffset!* := IAdd1 LispVar CurrentOffset!* >>;

syslsp procedure IndirectOperand U;
    ILOR(ILSH(1, 22), OperandValue second U);

put('Indirect, 'OperandValueFunction, 'IndirectOperand);

% ExtraRegLocation is in 20-FASL

put('ExtraReg, 'OperandValueFunction, 'ExtraRegLocation);

syslsp procedure MakeRelocWord(RelocTag, RelocInf);
    LSH(RelocTag, 34) + Field(RelocInf, 2, 34);

syslsp procedure MakeRelocInf(RelocTag, RelocInf);
    LSH(RelocTag, 16) + Field(RelocInf, 20, 16);

syslsp procedure MakeRelocHalfWord(RelocTag, RelocInf);
    LSH(RelocTag, 16) + Field(RelocInf, 20, 16);

off SysLisp;

END;
