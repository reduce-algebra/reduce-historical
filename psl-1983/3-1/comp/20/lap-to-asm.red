%
% LAP-TO-ASM.RED - LAP to assembler translator
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        13 August 1981
% Copyright (c) 1981 University of Utah
%

%  21-May-83 Mark R. Swanson
%    Added IndWord functions to support extended-20
%  01-Mar-83  Nancy Kendzierski
%   Changed EVIN to PathIn in ASMOUT to enable search paths to be
%    used when doing system builds connected to a directory other
%    than pxx:, where xx=machine (hp, 20, vax, etc.)
%   Only set InputSymFile!*, OutputSymFile!*, GlobalDataFileName!*,
%    and InitFileNameFormat!* if they aren't already initialized.
%   Changed SEMIC!* declaration from global to fluid.
%  <PSL.COMP>LAP-TO-ASM.RED.5, 30-Apr-82 14:47:52, Edit by BENSON
%  Removed EVAL and IGNORE processing

Imports '(PathIn);

fluid '(Semic!*
        !*Comp
	!*PLap
	DfPrint!*
	CharactersPerWord
	AddressingUnitsPerItem
	AddressingUnitsPerFunctionCell
	InputSymFile!*
	OutputSymFile!*
	CodeOut!*
	DataOut!*
	InitOut!*;
	CodeFileNameFormat!*
	DataFileNameFormat!*
	InitFileNameFormat!*
	ModuleName!*
	UncompiledExpressions!*
	NextIDNumber!*
	OrderedIDList!*
	NilNumber!*
	!*MainFound
        !*MAIN
	!*DeclareBeforeUse
	MainEntryPointName!*
	EntryPoints!*
	LocalLabels!*
	CodeExternals!*
	CodeExporteds!*
	DataExternals!*
	DataExporteds!*
	ExternalDeclarationFormat!*
	ExportedDeclarationFormat!*
	LabelFormat!*
	FullWordFormat!*
	DoubleFloatFormat!*
	ReserveDataBlockFormat!*
	ReserveZeroBlockFormat!*
	UndefinedFunctionCellInstructions!*
	DefinedFunctionCellFormat!*
	PrintExpressionForm!*
	PrintExpressionFormPointer!*
	CommentFormat!*
	NumericRegisterNames!*
	ExpressionCount!*
	ASMOpenParen!*
	ASMCloseParen!*
	ToBeCompiledExpressions!*
	GlobalDataFileName!*
);


% Default values; set up if not already initialized.
if null InputSymFile!* then InputSymFile!* := "psl.sym";
if null OutputSymFile!* then OutputSymFile!* := "psl.sym";
if null GlobalDataFileName!* then GlobalDataFileName!* := "global-data.red";
if null InitFileNameFormat!* then InitFileNameFormat!* := "%w.init";

lisp procedure DfPrintASM U;		%. Called by TOP-loop, DFPRINT!*
begin scalar Nam, Ty, Fn;
	if atom U then return NIL;
	Fn := car U;
	IF FN = 'PUTD THEN GOTO DB2;
	IF NOT (FN MEMQ '(DE DF DM DN)) THEN GOTO DB1;
	NAM:=CADR U;
	U:='LAMBDA . CDDR U;
	TY:=CDR ASSOC(FN, '((DE . EXPR)
			    (DF . FEXPR)
			    (DM . MACRO)
			    (DN . NEXPR)));
DB3:	if Ty = 'MACRO then begin scalar !*Comp;
	    PutD(Nam, Ty, U);		% Macros get defined now
	end;
	if FlagP(Nam, 'Lose) then <<
	ErrorPrintF("*** %r has not been defined, because it is flagged LOSE",
			Nam);
	return NIL >>;
	IF FLAGP(TY,'COMPILE) THEN
	<<  PUT(NAM,'CFNTYPE,LIST TY); 
            U := LIST('!*ENTRY,NAM,TY,LENGTH CADR U)
                         . !&COMPROC(U, NAM);
	    if !*PLAP then for each X in U do Print X;
	    if TY neq 'EXPR then
		DfPrintASM list('put, MkQuote Nam, '(quote TYPE), MkQuote TY);
	    ASMOUTLAP U >>
	ELSE				% should never happen
	     SaveUncompiledExpression LIST('PUTD, MKQUOTE NAM,
						  MKQUOTE TY,
						  MKQUOTE U);
	RETURN NIL;
DB1:	% Simple S-EXPRESSION, maybe EVAL it;
        IF NOT PAIRP U THEN RETURN NIL;
	if (Fn := get(car U, 'ASMPreEval)) then return Apply(Fn, list U)
	else if (Fn := GetD car U) and car Fn = 'MACRO then
	    return DFPRINTASM Apply(cdr Fn, list U);
	SaveUncompiledExpression U;
	RETURN NIL;
DB2:	NAM:=CADR U;
	TY:=CADDR U;
	FN:=CADDDR U;
	IF EQCAR(NAM,'QUOTE) THEN <<  NAM:=CADR NAM;
	IF EQCAR(TY,'QUOTE) THEN << TY:=CADR TY;
	IF PAIRP FN AND CAR FN MEMBER '(FUNCTION QUOTE) THEN <<  FN:=CADR FN;
	IF TY MEMQ '(EXPR FEXPR MACRO NEXPR) THEN
	<<  U:=FN; GOTO DB3 >> >> >> >>;
	GOTO DB1;
   END;

lisp procedure ASMPreEvalLoadTime U;
    DFPrintASM cadr U;		% remove LOADTIME

put('LoadTime, 'ASMPreEval, 'ASMPreEvalLoadTime);

lisp procedure ASMPreEvalStartupTime U;
    SaveForCompilation cadr U;

put('StartupTime, 'ASMPreEval, 'ASMPreEvalStartupTime);

lisp procedure ASMPreEvalProgN U;
    for each X in cdr U do
	DFPrintASM X;

put('ProgN, 'ASMPreEval, 'ASMPreEvalProgN);

put('WDeclare, 'ASMPreEval, 'Eval);	% do it now

lisp procedure ASMPreEvalSetQ U;
begin scalar X, Val;
    X := cadr U;
    Val := caddr U;
    return if ConstantP Val or Val = T then
    <<  FindIDNumber X;
	put(X, 'InitialValue, Val);
	NIL >>
    else if null Val then
    <<  FindIDNumber X;
	RemProp(X, 'InitialValue);
	Flag(list X, 'NilInitialValue);
	NIL >>
    else if EqCar(Val, 'QUOTE) then
    <<  FindIDNumber X;
	Val := cadr Val;
	if null Val then
	<<  RemProp(X, 'InitialValue);
	    Flag(list X, 'NilInitialValue) >>
	else
	    put(X, 'InitialValue, Val);
	NIL >>
    else if IDP Val and get(Val, 'InitialValue)
		or FlagP(Val, 'NilInitialValue) then
    <<  if (Val := get(Val, 'InitialValue)) then
	    put(X, 'InitialValue, Val)
	else Flag(list X, 'NilInitialValue) >>
    else SaveUncompiledExpression U;	% just check simple cases, else return
end;

put('SetQ, 'ASMPreEval, 'ASMPreEvalSetQ);

lisp procedure ASMPreEvalPutD U;
    SaveUncompiledExpression CheckForEasySharedEntryPoints U;

lisp procedure CheckForEasySharedEntryPoints U;
%
% looking for (PUTD (QUOTE name1) xxxx (CDR (GETD (QUOTE name2))))
%
begin scalar NU, Nam, Exp;
    NU := cdr U;
    Nam := car NU;
    if car Nam = 'QUOTE then Nam := cadr Nam else return U;
    NU := cdr NU;
    Exp := cadr NU;
    if not (car Exp = 'CDR) then return U;
    Exp := cadr Exp;
    if not (car Exp = 'GETD) then return U;
    Exp := cadr Exp;
    if not (car Exp = 'QUOTE) then return U;
    Exp := cadr Exp;
    FindIDNumber Nam;
    put(Nam, 'EntryPoint, FindEntryPoint Exp);
    if not (car NU = '(QUOTE EXPR)) then return list('Put, '(Quote Type),
							   car NU);
    return NIL;
end;

put('PutD, 'ASMPreEval, 'ASMPreEvalPutD);

lisp procedure ASMPreEvalFluidAndGlobal U;
<<  if EqCar(cadr U, 'QUOTE) then Flag(cadr cadr U, 'NilInitialValue);
    SaveUncompiledExpression U >>;

put('Fluid, 'ASMPreEval, 'ASMPreEvalFluidAndGlobal);
put('Global, 'ASMPreEval, 'ASMPreEvalFluidAndGlobal);

CommentOutCode <<
fluid '(NewFluids!* NewGlobals!*);

lisp procedure ASMPreEvalFluidAndGlobal U;
begin scalar L;
    L := cadr U;
    return if car L = 'QUOTE then
    <<  L := cadr L;
	if car U = 'FLUID then
	    NewFluids!* := UnionQ(NewFluids!*, L)	% take union
	else NewGlobals!* := UnionQ(NewGlobals!*, L);
	Flag(L, 'NilInitialValue);
	NIL >>
    else SaveUncompiledExpression U;
end;

put('Fluid, 'ASMPreEval, 'ASMPreEvalFluidAndGlobal);
put('Global, 'ASMPreEval, 'ASMPreEvalFluidAndGlobal);
>>;

lisp procedure ASMPreEvalLAP U;
    if EqCar(cadr U, 'QUOTE) then ASMOutLap cadr cadr U
    else SaveUncompiledExpression U;

put('LAP, 'ASMPreEval, 'ASMPreEvalLAP);

CommentOutCode <<
lisp procedure InitialPut(Nam, Ind, Val);
begin scalar L, P;
    FindIDNumber Nam;
    if (P := Atsoc(Ind, L := get(Nam, 'InitialPropertyList))) then
	Rplacd(P, Val)
    else put(Nam, 'InitialPropertyList, (Ind . Val) . L);
end;

lisp procedure InitialRemprop(Nam, Ind);
begin scalar L;
    if (L := get(Nam, 'InitialPropertyList)) then
	put(Nam, 'InitialPropertyList, DelAtQIP(Ind, L));
end;

lisp procedure InitialFlag1(Nam, Ind);
begin scalar L, P;
    FindIDNumber Nam;
    if not Ind memq (L := get(Nam, 'InitialPropertyList)) then
	put(Nam, 'InitialPropertyList, Ind . L);
end;

lisp procedure InitialRemFlag1(Nam, Ind);
begin scalar L;
    if (L := get(Nam, 'InitialPropertyList)) then
	put(Nam, 'InitialPropertyList, DelQIP(Ind, L));
end;

lisp procedure ASMPreEvalPut U;
begin scalar Nam, Ind, Val;
    Nam := second U;
    Ind := third U;
    Val := fourth U;
    if EqCar(Nam, 'QUOTE) and EqCar(Ind, 'QUOTE) and
		(ConstantP Val or Val = T or EqCar(Val, 'QUOTE)) then
	InitialPut(second Nam, second Ind, if EqCar(Val, 'QUOTE) then
						second Val else Val)
    else SaveUncompiledExpression U;
end;

put('put, 'ASMPreEval, 'ASMPreEvalPut);

lisp procedure ASMPreEvalRemProp U;
begin scalar Nam, Ind;
    Nam := second U;
    Ind := third U;
    if EqCar(Nam, 'QUOTE) and EqCar(Ind, 'QUOTE) then
	InitialRemProp(second Nam, second Ind)
    else SaveUncompiledExpression U;
end;

put('RemProp, 'ASMPreEval, 'ASMPreEvalRemProp);

lisp procedure ASMPreEvalDefList U;
begin scalar DList, Ind;
    DList := second U;
    Ind := third U;
    if EqCar(DList, 'QUOTE) and EqCar(Ind, 'QUOTE) then
    <<  DList := second DList;
	Ind := second Ind;
	for each X in Dlist do InitialPut(first X, Ind, second X) >>
    else SaveUncompiledExpression U;
end;

put('DefList, 'ASMPreEval, 'ASMPreEvalDefList);

lisp procedure ASMPreEvalFlag U;
begin scalar NameList, Ind;
    NameList := second U;
    Ind := third U;
    if EqCar(NameList, 'QUOTE) and EqCar(Ind, 'QUOTE) then
    <<  Ind := second Ind;
	for each X in second NameList do
	    InitialFlag1(X, Ind) >>
    else SaveUncompiledExpression U;
end;

put('flag, 'ASMPreEval, 'ASMPreEvalFlag);

lisp procedure ASMPreEvalRemFlag U;
begin scalar NameList, Ind;
    NameList := second U;
    Ind := third U;
    if EqCar(NameList, 'QUOTE) and EqCar(Ind, 'QUOTE) then
    <<  Ind := second Ind;
	for each X in second NameList do
	    InitialRemFlag1(X, Ind) >>
    else SaveUncompiledExpression U;
end;

put('RemFlag, 'ASMPreEval, 'ASMPreEvalRemFlag);

lisp procedure ASMPreEvalGlobal U;
begin scalar NameList;
    NameList := second U;
    if EqCar(NameList, 'QUOTE) then
	for each X in second NameList do
	    InitialPut(X, 'TYPE, 'Global)
    else SaveUncompiledExpression U;
end;

put('Global, 'ASMPreEval, 'ASMPreEvalGlobal);

lisp procedure ASMPreEvalFluid U;
begin scalar NameList;
    NameList := second U;
    if EqCar(NameList, 'QUOTE) then
	for each X in second NameList do
	    InitialPut(X, 'TYPE, 'FLUID)
    else SaveUncompiledExpression U;
end;

put('Fluid, 'ASMPreEval, 'ASMPreEvalFluid);

lisp procedure ASMPreEvalUnFluid U;
begin scalar NameList;
    NameList := second U;
    if EqCar(NameList, 'QUOTE) then
	for each X in second NameList do
	    InitialRemProp(X, 'TYPE)
    else SaveUncompiledExpression U;
end;

put('UnFluid, 'ASMPreEval, 'ASMPreEvalUnFluid);
>>;

lisp procedure SaveUncompiledExpression U;
    if PairP U then
    begin scalar OldOut;
	OldOut := WRS InitOut!*;
	Print U;
	WRS OldOut;
    end;

ToBeCompiledExpressions!* := NIL . NIL;

lisp procedure SaveForCompilation U;
    if atom U or U member car ToBeCompiledExpressions!* then NIL
    else if car U = 'progn then
	for each X in cdr U do SaveForCompilation X
    else TConc(ToBeCompiledExpressions!*, U);

SYMBOLIC PROCEDURE ASMOUT FIL;
begin scalar OldOut;
    ModuleName!* := FIL;
    Prin2T "ASMOUT: IN files; or type in expressions";
    Prin2T "When all done execute ASMEND;";
    CodeOut!* := Open(BldMsg(CodeFileNameFormat!*, ModuleName!*), 'OUTPUT);
    OldOut := WRS CodeOut!*;
    LineLength 1000;
    WRS OldOut;
    CodeFileHeader();
    DataOut!* := Open(BldMsg(DataFileNameFormat!*, ModuleName!*), 'OUTPUT);
    OldOut := WRS DataOut!*;
    LineLength 1000;
    WRS OldOut;
    DataFileHeader();
    InitOut!* := Open(BldMsg(InitFileNameFormat!*, ModuleName!*), 'OUTPUT);
    ReadSYMFile();
    DFPRINT!* := 'DFPRINTASM;
    RemD 'OldLap;
    PutD('OldLap, 'EXPR, cdr RemD 'Lap);
    PutD('Lap, 'EXPR, cdr GetD 'ASMOutLap);
    !*DEFN := T;
    SEMIC!* := '!$ ;			% to turn echo off for IN
    if not ((ModuleName!* = "main")
            or !*Main) then PathIn GlobalDataFileName!*
    else !*Main := T;
end;

lisp procedure ASMEnd;
<<  off SysLisp;
    if !*MainFound then
    <<  CompileUncompiledExpressions();
%	WriteInitFile();
	InitializeSymbolTable() >>
    else WriteSymFile();
    CodeFileTrailer();
    Close CodeOut!*;
    DataFileTrailer();
    Close DataOut!*;
    Close InitOut!*;
    RemD 'Lap;
    PutD('Lap, 'EXPR, cdr GetD 'OldLap);
    DFPRINT!* := NIL;
    !*DEFN := NIL >>;

FLAG('(ASMEND), 'IGNORE);
DEFINEROP('ASMEND,NIL,ESTAT('ASMEND));

lisp procedure CompileUncompiledExpressions();
<<  CommentOutCode <<  AddFluidAndGlobalDecls(); >>;
    DFPRINTASM list('DE, 'INITCODE, '(),
			'PROGN . car ToBeCompiledExpressions!*) >>;

CommentOutCode <<
lisp procedure AddFluidAndGlobalDecls();
<<  SaveUncompiledExpression list('GLOBAL, MkQuote NewGlobals!*);
    SaveUncompiledExpression list('FLUID, MkQuote NewFluids!*) >>;
>>;

lisp procedure ReadSymFile();
    LapIN InputSymFile!*;

lisp procedure WriteSymFile();
begin scalar NewOut, OldOut;
    OldOut := WRS(NewOut := Open(OutputSymFile!*, 'OUTPUT));
    print list('SaveForCompilation,
	       MkQuote('progn . car ToBeCompiledExpressions!*));
    SaveIDList();
    SetqPrint 'NextIDNumber!*;
    SetqPrint 'StringGenSym!*;
    MapObl function PutPrintEntryAndSym;
    WRS OldOut;
    Close NewOut;
end;


CommentOutCode <<
lisp procedure WriteInitFile();
begin scalar OldOut, NewOut;
    NewOut := Open(InitFileName!*, 'OUTPUT);
    OldOut := WRS NewOut;
    for each X in car UncompiledExpressions!* do PrintInit X;
    Close NewOut;
    WRS OldOut;
end;

lisp procedure PrintInit X;
    if EqCar(X, 'progn) then
	for each Y in cdr X do PrintInit Y
    else Print X;
>>;

lisp procedure SaveIDList();
<<  Print list('setq, 'OrderedIDList!*, MkQuote car OrderedIDList!*);
    Print quote(OrderedIDList!* :=
			OrderedIDList!* . LastPair OrderedIDList!*) >>;

lisp procedure SetqPrint U;
    print list('SETQ, U, MkQuote Eval U);

lisp procedure PutPrint(X, Y, Z);
    print list('PUT, MkQuote X, MkQuote Y, MkQuote Z);

lisp procedure PutPrintEntryAndSym X;
begin scalar Y;
    if (Y := get(X, 'EntryPoint)) then PutPrint(X, 'EntryPoint, Y);
    if (Y := get(X, 'IDNumber)) then
	PutPrint(X, 'IDNumber, Y);
CommentOutCode <<
	if (Y := get(X, 'InitialPropertyList)) then
	    PutPrint(X, 'InitialPropertyList, Y);
>>;
    if (Y := get(X, 'InitialValue)) then
	PutPrint(X, 'InitialValue, Y)
    else if FlagP(X, 'NilInitialValue) then
	print list('flag, MkQuote list X, '(quote NilInitialValue));
    if get(X, 'SCOPE) = 'EXTERNAL then
    <<  PutPrint(X, 'SCOPE, 'EXTERNAL);
	PutPrint(X, 'ASMSymbol, get(X, 'ASMSymbol));
	if get(X, 'WVar) then PutPrint(X, 'WVar, X)
	else if get(X, 'WArray) then PutPrint(X, 'WArray, X)
	else if get(X, 'WString) then PutPrint(X, 'WString, X)
	else if (Y := get(X, 'WConst)) then PutPrint(X, 'WConst, Y) >>;
end;

lisp procedure FindIDNumber U;
begin scalar I;
    return if (I := ID2Int U) <= 128 then I
    else if (I := get(U, 'IDNumber)) then I
    else
    <<  put(U, 'IDNumber, I := NextIDNumber!*);
	OrderedIDList!* := TConc(OrderedIDList!*, U);
	NextIDNumber!* := NextIDNumber!* + 1;
	I >>;
end;

OrderedIDList!* := NIL . NIL;
NextIDNumber!* := 129;

lisp procedure InitializeSymbolTable();
begin scalar MaxSymbol;
    MaxSymbol := get('MaxSymbols, 'WConst);
    if MaxSymbol < NextIDNumber!* then
    <<  ErrorPrintF("*** MaxSymbols %r is too small; at least %r is needed",
				MaxSymbol,		NextIDNumber!*);
	MaxSymbol := NextIDNumber!* + 100 >>;
    Flag('(NIL), 'NilInitialValue);
    put('T, 'InitialValue, 'T);
    put('!$EOF!$, 'InitialValue, Int2ID get('EOF, 'CharConst));
    put('!$EOL!$, 'InitialValue, '!
);
    NilNumber!* := CompileConstant NIL;
    DataAlignFullWord();
%/ This is a BUG? M.L. G.
%/    for I := NextIDNumber!* step 1 until MaxSymbol do
%/	DataPrintFullWord NilNumber!*;
    InitializeSymVal();
    DataReserveBlock((MaxSymbol - NextIDNumber!*) + 1);
    InitializeSymPrp();
    DataReserveBlock((MaxSymbol - NextIDNumber!*) + 1);
%/ This is a BUG? M.L. G.
%/    for I := NextIDNumber!* step 1 until MaxSymbol do
%/	DataPrintFullWord NilNumber!*;
    InitializeSymNam MaxSymbol;
    InitializeSymFnc();
    DataReserveFunctionCellBlock((MaxSymbol - NextIDNumber!*) + 1);
    DataAlignFullWord();
    DataPrintGlobalLabel FindGlobalLabel 'NextSymbol;
    DataPrintFullWord NextIDNumber!*;
end;

lisp procedure InitializeSymPrp();
<<  CommentOutCode <<  InitializeHeap(); >>;	% init prop lists
    DataPrintGlobalLabel FindGlobalLabel 'SymPrp;
    for I := 0 step 1 until 128 do
	InitSymPrp1 Int2ID I;
    for each X in car OrderedIDList!* do
	InitSymPrp1 X >>;

lisp procedure InitSymPrp1 X;
<<
CommentOutCode <<
    DataPrintFullWord(if (X := get(X, 'InitialPropertyList)) then
			   X
		      else NilNumber!*);
>>;
    DataPrintFullWord NilNumber!* >>;

CommentOutCode <<
lisp procedure InitializeHeap();
begin scalar L;
    DataPrintGlobalLabel FindGlobalLabel 'Heap;
    for I := 0 step 1 until 128 do
	PrintPropertyList Int2ID I;
    for each X in car OrderedIDList!* do
	PrintPropertyList X;
    L := get('HeapSize, 'WConst);
end;
>>;

lisp procedure InitializeSymNam MaxSymbol;
<<  DataPrintGlobalLabel FindGlobalLabel 'SymNam;
    for I := 0 step 1 until 128 do
	DataPrintFullWord CompileConstant ID2String Int2ID I;
    for each IDName in car OrderedIDList!* do
	DataPrintFullWord CompileConstant ID2String IDName;
    MaxSymbol := MaxSymbol - 1;
    for I := NextIDNumber!* step 1 until MaxSymbol do
	DataPrintFullWord(I + 1);
    DataPrintFullWord 0 >>;

lisp procedure InitializeSymVal();
<<  DataPrintGlobalLabel FindGlobalLabel 'SymVal;
    for I := 0 step 1 until 128 do InitSymVal1 Int2ID I;
    for each X in car OrderedIDList!* do InitSymVal1 X >>;

lisp procedure InitSymVal1 X;
begin scalar Val;
    return DataPrintFullWord(if (Val := get(X, 'InitialValue)) then
				 CompileConstant Val
			     else if FlagP(X, 'NilInitialValue) then
				 NilNumber!*
			     else list('MkItem, get('Unbound, 'WConst),
						FindIDNumber X));
end;

lisp procedure InitializeSymFnc();
<<  DataPrintGlobalLabel FindGlobalLabel 'SymFnc;
    for I := 0 step 1 until 128 do InitSymFnc1 Int2ID I;
    for each X in car OrderedIDList!* do InitSymFnc1 X >>;

lisp procedure InitSymFnc1 X;
begin scalar EP;
    EP := get(X, 'EntryPoint);
    if null EP then DataPrintUndefinedFunctionCell()
    else DataPrintDefinedFunctionCell EP;
end;

lisp procedure ASMOutLap U;
begin scalar LocalLabels!*, OldOut;
    U := Pass1Lap U;			% Expand cmacros, quoted expressions
    CodeBlockHeader();
    OldOut := WRS CodeOut!*;
    for each X in U do ASMOutLap1 X;
    WRS OldOut;
    CodeBlockTrailer();
end;

lisp procedure ASMOutLap1 X;
begin scalar Fn;
    return if StringP X then PrintLabel X
    else if atom X then PrintLabel FindLocalLabel X
    else if (Fn := get(car X, 'ASMPseudoOp)) then Apply(Fn, list X)
    else
    % instruction output form is:
    % "space" <opcode> [ "space" <operand> { "comma" <operand> } ] "newline"
    <<  Prin2 '! ;		% Space
	PrintOpcode car X;
	X := cdr X;
	if not null X then
	<<  Prin2 '! ;		% SPACE
	    PrintOperand car X;
	    for each U in cdr X do
	    <<  Prin2 '!,;		% COMMA
		PrintOperand U >> >>;
	Prin2 !$EOL!$ >>;		% NEWLINE
end;

put('!*Entry, 'ASMPseudoOp, 'ASMPrintEntry);

lisp procedure ASMPrintEntry X;
begin scalar Y;
    PrintComment X;
    X := cadr X;
    Y := FindEntryPoint X;
    if not FlagP(X, 'InternalFunction) then FindIDNumber X;
    if X eq MainEntryPointName!* then
    <<  !*MainFound := T;
	SpecialActionForMainEntryPoint() >>
    else CodeDeclareExportedUse Y;
 end;

Procedure CodeDeclareExportedUse Y;
  if !*DeclareBeforeUse then
	<<  CodeDeclareExported Y;
	    PrintLabel Y >>
	else
	<<  PrintLabel Y;
	    CodeDeclareExported Y >>;

lisp procedure FindEntryPoint X;
begin scalar E;
    return if (E := get(X, 'EntryPoint)) then E
    else if ASMSymbolP X and not get(X, 'ASMSymbol) then
    <<  put(X, 'EntryPoint, X);
	X >>
    else
    <<  E := StringGenSym();
	put(X, 'EntryPoint, E);
	E >>;
end;

lisp procedure ASMPseudoPrintFloat X;
    PrintF(DoubleFloatFormat!*, cadr X);

put('Float, 'ASMPseudoOp, 'ASMPseudoPrintFloat);

lisp procedure ASMPseudoPrintFullWord X;
    for each Y in cdr X do PrintFullWord Y;

put('FullWord, 'ASMPseudoOp, 'ASMPseudoPrintFullWord);

lisp procedure ASMPseudoPrintIndWord X;
    for each Y in cdr X do PrintIndWord Y;

put('IndWord, 'ASMPseudoOp, 'ASMPseudoPrintIndWord);

lisp procedure ASMPseudoPrintByte X;
    PrintByteList cdr X;

put('Byte, 'ASMPseudoOp, 'ASMPseudoPrintByte);

lisp procedure ASMPseudoPrintHalfWord X;
    PrintHalfWordList cdr X;

put('HalfWord, 'ASMPseudoOp, 'ASMPseudoPrintHalfWord);

lisp procedure ASMPseudoPrintString X;
    PrintString cadr X;

put('String, 'ASMPseudoOp, 'ASMPseudoPrintString);

lisp procedure PrintOperand X;
    if StringP X then Prin2 X
    else if NumberP X then PrintNumericOperand X
    else if IDP X then Prin2 FindLabel X
    else begin scalar Hd, Fn;
	Hd := car X;
	if (Fn := get(Hd, 'OperandPrintFunction)) then
	    Apply(Fn, list X)
	else if (Fn := GetD Hd) and car Fn = 'MACRO then
	    PrintOperand Apply(cdr Fn, list X)
	else if (Fn := WConstEvaluable X) then PrintOperand Fn
	else PrintExpression X;
    end;

put('REG, 'OperandPrintFunction, 'PrintRegister);

lisp procedure PrintRegister X;
begin scalar Nam;
    X := cadr X;
    if StringP X then Prin2 X
    else if NumberP X then Prin2 GetV(NumericRegisterNames!*, X)
    else if Nam := RegisterNameP X then Prin2 Nam
    else
    <<  ErrorPrintF("***** Unknown register %r", X);
	Prin2 X >>;
end;

lisp procedure RegisterNameP X;
    get(X, 'RegisterName);

lisp procedure ASMEntry X;
    PrintExpression
    list('plus2, 'SymFnc,
		 list('times2, AddressingUnitsPerFunctionCell,
			       list('IDLoc, cadr X)));

put('Entry, 'OperandPrintFunction, 'ASMEntry);

lisp procedure ASMInternalEntry X;
    Prin2 FindEntryPoint cadr X;

put('InternalEntry, 'OperandPrintFunction, 'ASMInternalEntry);
put('InternalEntry, 'ASMExpressionFunction, 'ASMInternalEntry);

macro procedure ExtraReg U;
    list('plus2, '(WArray ArgumentBlock), (cadr U - (LastActualReg!& + 1))
					     * AddressingUnitsPerItem);

lisp procedure ASMSyslispVarsPrint X;
    Prin2 FindGlobalLabel cadr X;

DefList('((WVar ASMSyslispVarsPrint)
	  (WArray ASMSyslispVarsPrint)
	  (WString ASMSyslispVarsPrint)), 'OperandPrintFunction);

DefList('((WVar ASMSyslispVarsPrint)
	  (WArray ASMSyslispVarsPrint)
	  (WString ASMSyslispVarsPrint)), 'ASMExpressionFunction);

lisp procedure ASMPrintValueCell X;
    PrintExpression list('plus2, 'SymVal,
				 list('times, AddressingUnitsPerItem,
					      list('IDLoc, cadr X)));

DefList('((fluid ASMPrintValueCell)
	  (!$fluid ASMPrintValueCell)
	  (global ASMPrintValueCell)
	  (!$global ASMPrintValueCell)), 'OperandPrintFunction);

% Redefinition of WDeclare for output to assembler file

% if either UpperBound or Initializer are NIL, they are considered to be
% unspecified.

fexpr procedure WDeclare U;
    for each X in cddr U do WDeclare1(car X, car U, cadr U, cadr X, caddr X);

flag('(WDeclare), 'IGNORE);

lisp procedure WDeclare1(Name, Scope, Typ, UpperBound, Initializer);
    if Typ = 'WCONST then
	if Scope = 'EXTERNAL and not get(Name, 'WCONST) then
	    ErrorPrintF("*** A value has not been defined for WConst %r",
								Name)
	else
	<<  put(Name, 'SCOPE, if Scope = 'EXPORTED then 'EXTERNAL else Scope);
	    put(Name, 'WCONST, WConstReform Initializer) >>
    else
    <<  put(Name, Typ, Name);
	if Scope = 'EXTERNAL then
	<<  put(Name, 'SCOPE, 'EXTERNAL);
	    if not RegisterNameP Name then	% kludge to avoid declaring
	    <<  Name := LookupOrAddASMSymbol Name;
		DataDeclareExternal Name;	% registers as variables
		CodeDeclareExternal Name >> >>
	else
	<<  put(Name, 'SCOPE, if Scope = 'EXPORTED then 'EXTERNAL else Scope);
	    Name := LookupOrAddASMSymbol Name;
	    if !*DeclareBeforeUse then DataDeclareExported Name;
	    DataInit(Name,
		      Typ,
		      UpperBound,
		      Initializer);
	    if not !*DeclareBeforeUse then DataDeclareExported Name;
	    CodeDeclareExternal Name >> >>;

lisp procedure DataInit(ASMSymbol, Typ, UpperBound, Initializer);
<<  DataAlignFullWord();
    if Typ = 'WVAR then
    <<  if UpperBound then
	    ErrorPrintF "*** An UpperBound may not be specified for a WVar";
	Initializer := if Initializer then WConstReform Initializer else 0;
	DataPrintVar(ASMSymbol, Initializer) >>
    else
    <<  if UpperBound and Initializer then
	    ErrorPrintF "*** Can't have both UpperBound and initializer"
	else if not (UpperBound or Initializer) then
	    ErrorPrintF "*** Must have either UpperBound or initializer"
	else if UpperBound then
	    DataPrintBlock(ASMSymbol, WConstReform UpperBound, Typ)
	else
	<<  Initializer := if StringP Initializer then Initializer
				else  WConstReformLis Initializer;
	    DataPrintList(ASMSymbol, Initializer, Typ) >> >> >>;

lisp procedure WConstReform U;
begin scalar X;
    return if FixP U or StringP U then U
    else if IDP U then
	if get(U, 'WARRAY) or get(U, 'WSTRING) then U
        else if get(U,'WVAR) then list('GETMEM,U)
	else if (X := get(U, 'WCONST)) then X
	else ErrorPrintF("*** Unknown symbol %r in WConstReform", U)
    else if PairP U then
	if (X := get(car U, 'WConstReformPseudo)) then Apply(X, list U)
	else if (X := get(car U, 'DOFN)) then X . WConstReformLis cdr U
	else if MacroP car U then WConstReform Apply(cdr GetD car U, list U)
	else car U . WConstReformLis cdr U
    else ErrorPrintF("*** Illegal expression %r in WConstReform", U);
end;

lisp procedure WConstReformIdent U;
    U;

put('InternalEntry, 'WConstReformPseudo, 'WConstReformIdent);

lisp procedure WConstReformQuote U;
    CompileConstant cadr U;

put('QUOTE, 'WConstReformPseudo, 'WConstReformQuote);

lisp procedure WConstReformLis U;
    for each X in U collect WConstReform X;

lisp procedure WConstReformLoc U;		%. To handle &Foo[23]
<<  U := WConstReform cadr U;
    if car U neq 'GETMEM then
	ErrorPrintF("*** Illegal constant addressing expression %r",
				list('LOC, U))
    else cadr U >>;

put('LOC, 'WConstReformPseudo, 'WConstReformLoc);

lisp procedure WConstReformIDLoc U;
    FindIDNumber cadr U;

put('IDLoc, 'WConstReformPseudo, 'WConstReformIDLoc);

lisp procedure LookupOrAddASMSymbol U;
begin scalar X;
    if not (X := get(U, 'ASMSymbol)) then X := AddASMSymbol U;
    return X;
end;

lisp procedure AddASMSymbol U;
begin scalar X;
    X := if ASMSymbolP U and not get(U, 'EntryPoint) then U
	 else StringGensym();
    put(U, 'ASMSymbol, X);
    return X;
end;

lisp procedure DataPrintVar(Name, Init);
begin scalar OldOut;
    DataPrintLabel Name;
    OldOut := WRS DataOut!*;
    PrintFullWord Init;
    WRS OldOut;
end;

lisp procedure DataPrintBlock(Name, Siz, Typ);
<<  if Typ = 'WSTRING
	then Siz := list('quotient, list('plus2, Siz, CharactersPerWord + 1),
				    CharactersPerWord)
    else Siz := list('plus2, Siz, 1);
    DataReserveZeroBlock(Name, Siz) >>;

lisp procedure DataPrintList(Nam, Init, Typ);
begin scalar OldOut;
    DataPrintLabel Nam;
    OldOut := WRS DataOut!*;
    if Typ = 'WSTRING then
	if StringP Init then
	<<  PrintFullWord Size Init;
	    PrintString Init >>
	else
	<<  PrintFullWord(Length Init - 1);
	    PrintByteList Append(Init, '(0)) >>
    else
	if StringP Init then begin scalar S;
	    S := Size Init;
	    for I := 0 step 1 until S do
		PrintFullWord Indx(Init, I);
	end else for each X in Init do
	    PrintFullWord X;
    WRS OldOut;
end;

lisp procedure DataPrintGlobalLabel X;
<<  if !*DeclareBeforeUse then DataDeclareExported X;
    DataPrintLabel X;
    if not !*DeclareBeforeUse then DataDeclareExported X;
    CodeDeclareExternal X >>;
    

lisp procedure DataDeclareExternal X;
    if not (X member DataExternals!* or X member DataExporteds!*) then
    <<  DataExternals!* := X . DataExternals!*;
	DataPrintF(ExternalDeclarationFormat!*, X, X) >>;

lisp procedure CodeDeclareExternal X;
    if not (X member CodeExternals!* or X member CodeExporteds!*) then
    <<  CodeExternals!* := X . CodeExternals!*;
	CodePrintF(ExternalDeclarationFormat!*, X, X) >>;

lisp procedure DataDeclareExported X;
<<  if X member DataExternals!* or X member DataExporteds!* then
	ErrorPrintF("***** %r multiply defined", X);
    DataExporteds!* := X . DataExporteds!*;
    DataPrintF(ExportedDeclarationFormat!*, X, X) >>;

lisp procedure CodeDeclareExported X;
<<  if X member CodeExternals!* or X member CodeExporteds!* then
	ErrorPrintF("***** %r multiply defined", X);
    CodeExporteds!* := X . CodeExporteds!*;
    CodePrintF(ExportedDeclarationFormat!*, X, X) >>;

lisp procedure PrintLabel X;
    PrintF(LabelFormat!*, X,X);

lisp procedure DataPrintLabel X;
    DataPrintF(LabelFormat!*, X,X);

lisp procedure CodePrintLabel X;
    CodePrintF(LabelFormat!*, X,X);

lisp procedure PrintComment X;
    PrintF(CommentFormat!*, X);

PrintExpressionForm!* := list('PrintExpression, MkQuote NIL);
PrintExpressionFormPointer!* := cdadr PrintExpressionForm!*;

% Save some consing
% instead of list('PrintExpression, MkQuote X), reuse the same list structure

lisp procedure PrintFullWord X;
<<  RplacA(PrintExpressionFormPointer!*, X);
    PrintF(FullWordFormat!*, PrintExpressionForm!*) >>;

lisp procedure PrintIndWord X;
<<  RplacA(PrintExpressionFormPointer!*, X);
    PrintF(IndWordFormat!*, PrintExpressionForm!*) >>;

lisp procedure DataPrintFullWord X;
<<  RplacA(PrintExpressionFormPointer!*, X);
    DataPrintF(FullWordFormat!*, PrintExpressionForm!*) >>;

lisp procedure CodePrintFullWord X;
<<  RplacA(PrintExpressionFormPointer!*, X);
    CodePrintF(FullWordFormat!*, PrintExpressionForm!*) >>;

lisp procedure DataReserveZeroBlock(Nam, X);
<<  RplacA(PrintExpressionFormPointer!*,
	   list('Times2, AddressingUnitsPerItem, X));
    DataPrintF(ReserveZeroBlockFormat!*, Nam, PrintExpressionForm!*) >>;

lisp procedure DataReserveBlock X;
<<  RplacA(PrintExpressionFormPointer!*,
	   list('Times2, AddressingUnitsPerItem, X));
    DataPrintF(ReserveDataBlockFormat!*, PrintExpressionForm!*) >>;

lisp procedure DataReserveFunctionCellBlock X;
<<  RplacA(PrintExpressionFormPointer!*,
	   list('Times2, AddressingUnitsPerFunctionCell, X));
    DataPrintF(ReserveDataBlockFormat!*, PrintExpressionForm!*) >>;

lisp procedure DataPrintUndefinedFunctionCell();
begin scalar OldOut;
    OldOut := WRS DataOut!*;
    for each X in UndefinedFunctionCellInstructions!* do
	ASMOutLap1 X;
    WRS OldOut;
end;

lisp procedure DataPrintDefinedFunctionCell X;
  <<DataDeclareExternal X;
    DataPrintF(DefinedFunctionCellFormat!*, X, X)>>;
 % in case it's needed twice


lisp procedure DataPrintByteList X;
begin scalar OldOut;
    OldOut := WRS DataOut!*;
    PrintByteList X;
    WRS OldOut;
end;

lisp procedure DataPrintExpression X;
begin scalar OldOut;
    OldOut := WRS DataOut!*;
    PrintExpression X;
    WRS OldOut;
end;

lisp procedure CodePrintExpression X;
begin scalar OldOut;
    OldOut := WRS CodeOut!*;
    PrintExpression X;
    WRS OldOut;
end;

ExpressionCount!* := -1;

lisp procedure PrintExpression X;
(lambda(ExpressionCount!*);
begin scalar Hd, Tl, Fn;
    X := ResolveWConstExpression X;
    if NumberP X or StringP X then Prin2 X
    else if IDP X then Prin2 FindLabel X
    else if atom X then
    <<  ErrorPrintF("***** Oddity in expression %r", X);
	Prin2 X >>
    else
    <<  Hd := car X;
	Tl := cdr X;
	if (Fn := get(Hd, 'BinaryASMOp)) then
	<<  if ExpressionCount!* > 0 then Prin2 ASMOpenParen!*;
	    PrintExpression car Tl;
	    Prin2 Fn;
	    PrintExpression cadr Tl;
	    if ExpressionCount!* > 0 then Prin2 ASMCloseParen!* >>
	else if (Fn := get(Hd, 'UnaryASMOp)) then
	<<  Prin2 Fn;
	    PrintExpression car Tl >>
	else if (Fn := get(Hd, 'ASMExpressionFormat)) then
	    Apply('PrintF, Fn . for each Y in Tl collect
				    list('PrintExpression, MkQuote Y))
	else if (Fn := GetD Hd) and car Fn = 'MACRO then
	    PrintExpression Apply(cdr Fn, list X)
	else if (Fn := get(Hd, 'ASMExpressionFunction)) then
	    Apply(Fn, list X)
	else
	<<  ErrorPrintF("***** Unknown expression %r", X);
	    PrintF("*** Expression error %r ***", X) >> >>;
end)(ExpressionCount!* + 1);

lisp procedure ASMPrintWConst U;
    PrintExpression cadr U;

put('WConst, 'ASMExpressionFunction, 'ASMPrintWConst);

DefList('((Plus2 !+)
	  (WPlus2 !+)
	  (Difference !-)
	  (WDifference !-)
	  (Times2 !*)
	  (WTimes2 !*)
	  (Quotient !/)
	  (WQuotient !/)), 'BinaryASMOp);

DefList('((Minus !-)
	  (WMinus !-)), 'UnaryASMOp);

lisp procedure CompileConstant X;
<<  X := BuildConstant X;
    if null cdr X then car X
    else
    <<  If !*DeclareBeforeUse then CodeDeclareExported cadr X;
        ASMOutLap cdr X;
	DataDeclareExternal cadr X;
        If Not !*DeclareBeforeUse then CodeDeclareExported cadr X;
	car X >> >>;

CommentOutCode <<
lisp procedure CompileHeapData X;
begin scalar Y;
    X := BuildConstant X;
    return if null cdr X then car X
    else
    <<  Y := WRS DataOut!*;
	for each Z in cdr X do ASMOutLap1 Z;
	DataDeclareExported cadr X;
	WRS Y;
	car X >>;
end;
>>;

lisp procedure DataPrintString X;
begin scalar OldOut;
    OldOut := WRS DataOut!*;
    PrintString X;
    WRS OldOut;
end;

lisp procedure FindLabel X;
begin scalar Y;
    return if (Y := Atsoc(X, LocalLabels!*)) then cdr Y
    else if (Y := get(X, 'ASMSymbol)) then Y
    else if (Y := get(X, 'WConst)) then Y
    else FindLocalLabel X;
end;

lisp procedure FindLocalLabel X;
begin scalar Y;
    return if (Y := Atsoc(X, LocalLabels!*)) then cdr Y
    else
    <<  LocalLabels!* := (X . (Y := StringGensym())) . LocalLabels!*;
	Y >>;
end;

lisp procedure FindGlobalLabel X;
    get(X, 'ASMSymbol) or ErrorPrintF("***** Undefined symbol %r", X);

lisp procedure CodePrintF(Fmt, A1, A2, A3, A4);
begin scalar OldOut;
    OldOut := WRS CodeOut!*;
    PrintF(Fmt, A1, A2, A3, A4);
    WRS OldOut;
end;

lisp procedure DataPrintF(Fmt, A1, A2, A3, A4);
begin scalar OldOut;
    OldOut := WRS DataOut!*;
    PrintF(Fmt, A1, A2, A3, A4);
    WRS OldOut;
end;

% Kludge of the year, just to avoid having IDLOC defined during compilation

CompileTime fluid '(MACRO);

MACRO := 'MACRO;

PutD('IDLoc, MACRO,
function lambda X;
    FindIDNumber cadr X);

END;
