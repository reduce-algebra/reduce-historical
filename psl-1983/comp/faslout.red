%
% FASLOUT.RED - Top level of fasl file writer
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        16 February 1982
% Copyright (c) 1982 University of Utah
%
%  <PSL.COMP>FASLOUT.RED.6, 16-Dec-82 12:49:59, Edit by KESSLER
%  Take out Semic!* as a fluid.  Not used by anyone that I can see
%  and is already a global in RLISP.
%  <PSL.COMP>FASLOUT.RED.35, 10-Jun-82 10:41:18, Edit by GRISS
%  Made CompileUncompiledExpressions regular func
%  <PSL.COMP>FASLOUT.RED.12, 30-Apr-82 14:45:59, Edit by BENSON
%  Removed EVAL and IGNORE processing
%  <PSL.COMP>FASLOUT.RED.8, 29-Apr-82 06:23:18, Edit by GRISS
%  moved DEFINEROP call to RLISP-PARSER


CompileTime <<
 flag('(CodeFileHeader CodeFileTrailer AllocateFaslSpaces),
      'InternalFunction);
 load Fast!-Vector;
>>;

fluid '(!*WritingFaslFile
	!*Lower
	!*quiet_faslout
	DfPrint!*
	UncompiledExpressions!*
	ModuleName!*
	CodeOut!*
	InitOffset!*
	CurrentOffset!*
	FaslBlockEnd!*
	MaxFaslOffset!*
	BitTableOffset!*
	FaslFilenameFormat!*);

FaslFilenameFormat!* := "%w.b";

lisp procedure DfPrintFasl U;		%. Called by TOP-loop, DFPRINT!*
begin scalar Nam, Ty, Fn, !*WritingFaslFile;
	!*WritingFaslFile := T;
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
	    LAP U >>
	ELSE				% should never happen
	     SaveUncompiledExpression LIST('PUTD, MKQUOTE NAM,
						  MKQUOTE TY,
						  MKQUOTE U);
	if IGreaterP(Posn(), 0) then WriteChar char BLANK;
        Prin1 NAM;
	RETURN NIL;
DB1:	% Simple S-EXPRESSION, maybe EVAL it;
        IF NOT PAIRP U THEN RETURN NIL;
	if (Fn := get(car U, 'FaslPreEval)) then return Apply(Fn, list U)
	else if (Fn := GetD car U) and car Fn = 'MACRO then
	    return DFPRINTFasl Apply(cdr Fn, list U);
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

FLAG ('(DEFLIST FLAG FLUID GLOBAL REMFLAG REMPROP UNFLUID),'EVAL);

lisp procedure FaslPreEvalLoadTime U;
    DFPrintFasl cadr U;		% remove LOADTIME

put('LoadTime, 'FaslPreEval, 'FaslPreEvalLoadTime);
put('BothTimes, 'FaslPreEval, 'FaslPreEvalLoadTime);
put('StartupTime, 'FaslPreEval, 'FaslPreEvalLoadTime);	% used in kernel

% A few things to save space when loading

put('Flag,
    'FaslPreEval,
    function lambda U;
	if EqCar(second U, 'QUOTE) then
	    DFPrintFasl('progn . for each X in second second U collect
				     list('Flag1, MkQuote X, third U))
	else SaveUncompiledExpression U);

put('fluid,
    'FaslPreEval,
    function lambda U;
	if EqCar(second U, 'QUOTE) then
            DFPrintFasl('progn . for each X in second second U collect
				     list('Fluid1, MkQuote X))
	else SaveUncompiledExpression U);

put('global,
    'FaslPreEval,
    function lambda U;
	if EqCar(second U, 'QUOTE) then
	    DFPrintFasl('progn . for each X in second second U collect
				     list('Global1, MkQuote X))
	else SaveUncompiledExpression U);

put('DefList,
    'FaslPreEval,
    function lambda U;
	if EqCar(second U, 'QUOTE) then
	    DFPrintFasl('progn . for each X in second second U collect
				     list('put, MkQuote first X,
						third U,
						MkQuote second X))
	else SaveUncompiledExpression U);

put('ProgN,
    'FaslPreEval,
    function lambda U;
	for each X in cdr U do
	    DFPrintFasl X);

put('LAP,
    'FaslPreEval,
    function lambda U;
	if EqCar(cadr U, 'QUOTE) then Lap cadr cadr U
	else SaveUncompiledExpression U);

UncompiledExpressions!* := NIL . NIL;

lisp procedure SaveUncompiledExpression U;
<<  if atom U then NIL
    else TConc(UncompiledExpressions!*, U);
    NIL >>;

lisp procedure FaslOut FIL;
<<  ModuleName!* := FIL;
    if not !*quiet_faslout then
    <<  if not FUnBoundP 'Begin1 then
	<<  Prin2T "FASLOUT: IN files; or type in expressions";
	    Prin2T "When all done execute FASLEND;" >>
	else
	<<  Prin2T "FASLOUT: (DSKIN files) or type in expressions";
	    Prin2T "When all done execute (FASLEND)" >> >>;
    CodeOut!* := BinaryOpenWrite BldMsg(FaslFilenameFormat!*, ModuleName!*);
    CodeFileHeader();
    DFPRINT!* := 'DFPRINTFasl;
    !*WritingFaslFile := T;
    !*DEFN := T >>;

lisp procedure FaslEnd;
    if not !*WritingFaslFile then
	StdError "FASLEND not within FASLOUT"
    else
    <<  CompileUncompiledExpressions();
	UncompiledExpressions!* := NIL . NIL;
	CodeFileTrailer();
	BinaryClose CodeOut!*;
	DFPRINT!* := NIL;
        !*WritingFaslFile := NIL;
	!*DEFN := NIL >>;

FLAG('(FaslEND), 'IGNORE);

lisp procedure ComFile Filename;
begin scalar !*Defn, !*WritingFaslFile, TestFile, FileBase, FileExt,
		I, N, DotFound, TestExts, !*quiet_faslout;
    if IDP Filename then
    (lambda (!*Lower); Filename := BldMsg("%w", Filename))(T);
    if not StringP Filename then return
	NonStringError(Filename, 'ComFile);
    N := ISizeS Filename;
    I := 0;
    while not DotFound and ILEQ(I, N) do
    <<  if IGetS(Filename, I) = char '!. then DotFound := T;
	I := IAdd1 I >>;
    if DotFound then
    <<  if not FileP Filename then return ContError(99, "Couldn't find file",
							ComFile Filename)
	else
	<<  FileBase := SubSeq(Filename, 0, I);
	    FileExt := SubSeq(Filename, ISub1 I, IAdd1 N) >> >>
    else
    <<  TestExts := '(".build" ".sl" ".red");
	while not null TestExts
		and not FileP(TestFile := Concat(Filename, first TestExts)) do
	    TestExts := rest TestExts;
	if null TestExts then return ContError(99,
					       "Couldn't find file",
					       ComFile Filename)
	else
	<<  FileExt := first TestExts;
	    FileBase := Filename;
	    Filename := TestFile >> >>;
    ErrorPrintF("*** Compiling %w", Filename);
    !*quiet_faslout := T;
    Faslout FileBase;
    if FileExt member '(".build" ".red") then
	EvIn list Filename
    else DskIn Filename;
    Faslend;
    return T;
end;

lisp procedure CompileUncompiledExpressions();
<<  ErrorPrintF("*** Init code length is %w",
			length car UncompiledExpressions!*);
    DFPRINTFasl list('DE, '!*!*Fasl!*!*InitCode!*!*, '(),
			'PROGN . car UncompiledExpressions!*) >>;

lisp procedure CodeFileHeader();
<<  BinaryWrite(CodeOut!*, const FASL_MAGIC_NUMBER);
    AllocateFaslSpaces() >>;

fluid '(CodeBase!* BitTableBase!* OrderedIDList!* NextIDNumber!*);

lisp procedure FindIDNumber U;
begin scalar I;
    return if ILEQ(I := IDInf U, 128) then I
    else if (I := get(U, 'IDNumber)) then I
    else
    <<  put(U, 'IDNumber, I := NextIDNumber!*);
	OrderedIDList!* := TConc(OrderedIDList!*, U);
	NextIDNumber!* := IAdd1 NextIDNumber!*;
	I >>;
end;

lisp procedure CodeFileTrailer();
begin scalar S;
    SystemFaslFixup();
    BinaryWrite(CodeOut!*, IDifference(ISub1 NextIDNumber!*, 2048));
					% Number of local IDs
    for each X in car OrderedIDList!* do
    <<  RemProp(X, 'IDNumber);
	X := StrInf ID2String X;
	S := StrLen X;
	BinaryWriteBlock(CodeOut!*, X, IAdd1 StrPack S) >>;
    BinaryWrite(CodeOut!*,		% S is size in words
		S := IQuotient(IPlus2(CurrentOffset!*,
				      ISub1 const AddressingUnitsPerItem),
				const AddressingUnitsPerItem));
    BinaryWrite(CodeOut!*, InitOffset!*);
    BinaryWriteBlock(CodeOut!*, CodeBase!*, S);
    BinaryWrite(CodeOut!*, S := IQuotient(IPlus2(BitTableOffset!*,
					   ISub1 const BitTableEntriesPerWord),
					  const BitTableEntriesPerWord));
    BinaryWriteBlock(CodeOut!*, BitTableBase!*, S);
    DelWArray(BitTableBase!*, FaslBlockEnd!*);
end;

lisp procedure UpdateBitTable(NumberOfEntries, FirstEntry);
if !*WritingFaslFile then
<<  PutBitTable(BitTableBase!*, BitTableOffset!*, FirstEntry);
    BitTableOffset!* := IAdd1 BitTableOffset!*;
    for I := 2 step 1 until NumberOfEntries do
    <<  PutBitTable(BitTableBase!*, BitTableOffset!*, 0);
	BitTableOffset!* := IAdd1 BitTableOffset!* >>;
    if IGreaterP(BitTableOffset!*, MaxFaslOffset!*) then
	FatalError "BPS exhausted during FaslOut; output file too large" >>;

lisp procedure AllocateFaslSpaces();
begin scalar B;
    B := GTWarray NIL;			% how much is left?
    B := IDifference(B, IQuotient(B, 3));
    FaslBlockEnd!* := GTWArray 0;	% pointer to top of space
    BitTableBase!* := GTWarray B;	% take 2/3 of whatever's left
    CurrentOffset!* := 0;
    BitTableOffset!* := 0;
    CodeBase!*
	:= Loc WGetV(BitTableBase!*,	% split the space between
		     IQuotient(B,	% bit table and code
			       IQuotient(const BitTableEntriesPerWord,
					 const AddressingUnitsPerItem)));
    MaxFaslOffset!* := IDifference(FaslBlockEnd!*, CodeBase!*);
    OrderedIDList!* := NIL . NIL;
    NextIDNumber!* := 2048;		% local IDs start at 2048
end;

END;
