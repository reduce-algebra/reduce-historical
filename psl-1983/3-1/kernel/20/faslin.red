%  25-May-1983 Mark R. Swanson
%  Changed magic number to differentiate extended-20 fasl files from old ones

on SysLisp;

external WString TokenBuffer;
external WArray ArgumentBlock;

internal WConst CODE_OFFSET = 0,
		RELOC_ID_NUMBER = 1,
		RELOC_VALUE_CELL = 2,
		RELOC_FUNCTION_CELL = 3;

internal WConst RELOC_WORD = 1,
		RELOC_RIGHT_HALF = 2,
		RELOC_INF = 3;

internal WConst FASLMAGIC = 2099;

CompileTime <<

smacro procedure LocalIDNumberP U;
    U >= 2048;

smacro procedure LocalToGlobalID U;
    IDTable[U - 2048];

smacro procedure ExtraArgumentP U;
    U >= 8150;				% Something enough less than 8192

smacro procedure MakeExtraArgument U;
    U - (8150 + (MaxRealRegs + 1));
>>;

internal WVar CodeBase;

syslsp procedure FaslIN File;
begin scalar F, N, M, IDTable, CodeSize, OldCodeBase,
	     E, BT, R, RT, RI, BI, Top, BTop;
    F := BinaryOpenRead File;
    N := BinaryRead F;			% First word is magic number
    if N neq FASLMAGIC then ContError(99,
				      "%r is not a fasl format file",
				      File,
				      FaslIN File);
    M := BinaryRead F;			% Number of local IDs
    Top := GtWArray 0;			% pointer to top of space
    IDTable := GtWArray(M + 1);		% Allocate space for table
    for I := 0 step 1 until M do
    <<  TokenBuffer[0] := BinaryRead F;	% word is length of ID name
	BinaryReadBlock(F, &TokenBuffer[1], StrPack TokenBuffer[0]);
	IDTable[I] := IDInf Intern MkSTR TokenBuffer >>;
    CodeSize := BinaryRead F;		% Size of code segment in words
    OldCodeBase := CodeBase;		% So FASLIN is reentrant
    CodeBase := GtBPS CodeSize;		% Allocate space in BPS
    BTop := GTBPS 0;			% pointer to top
    E := CodeBase + BinaryRead F;	% Next word is offset of init function
					% Will be called after code is read
    BinaryReadBlock(F, CodeBase, CodeSize);	% Put the next N words there
    N := BinaryRead F;		% Next word is size of bit table in words
    BT := GtWArray N;			% Allocate space for bit table
    BinaryReadBlock(F, BT, N);		% read bit table
    BinaryClose F;			% close the file
    CodeSize := CodeSize*AddressingUnitsPerItem - 1;
    for I := 0 step 1 until CodeSize do
    <<  R := BitTable(BT, I);
	BI := CodeBase + I;
	case R of
	    RELOC_WORD:
	    <<  RT := RelocWordTag @BI;
		RI := RelocWordInf @BI;
		case RT of
		    CODE_OFFSET:
			@BI := CodeBase + RI;
		    RELOC_VALUE_CELL:
		    <<  if ExtraArgumentP RI then
			    RI := &ArgumentBlock[MakeExtraArgument RI]
			else if LocalIDNumberP RI then
			    RI := &SymVal LocalToGlobalID RI
			else RI := &SymVal RI;
			@BI := RI >>;
		    RELOC_FUNCTION_CELL:
		    <<  if LocalIDNumberP RI then RI := LocalToGlobalID RI;
			@BI :=
			   SymFnc + AddressingUnitsPerFunctionCell*RI >>;
		    RELOC_ID_NUMBER:	% Must be a local ID number
		    <<  if LocalIDNumberP RI then RI := LocalToGlobalID RI;
			@BI := RI >>;
		end >>;
	    RELOC_RIGHT_HALF:
	    <<  RT := RelocRightHalfTag @BI;
		RI := RelocRightHalfInf @BI;
		case RT of
		    CODE_OFFSET:
			RightHalf @BI := CodeBase + RI;
		    RELOC_VALUE_CELL:
		    <<  if ExtraArgumentP RI then
			    RI := &ArgumentBlock[MakeExtraArgument RI]
			else if LocalIDNumberP RI then
			    RI := &SymVal LocalToGlobalID RI
			else RI := &SymVal RI;
			RightHalf @BI := RI >>;
		    RELOC_FUNCTION_CELL:
		    <<  if LocalIDNumberP RI then RI := LocalToGlobalID RI;
			RightHalf @BI :=
			    SymFnc + AddressingUnitsPerFunctionCell*RI >>;
		    RELOC_ID_NUMBER:	% Must be a local ID number
		    <<  if LocalIDNumberP RI then RI := LocalToGlobalID RI;
			RightHalf @BI := RI >>;
		end >>;
	    RELOC_INF:
	    <<  RT := RelocInfTag @BI;
		RI := RelocInfInf @BI;
		case RT of
		    CODE_OFFSET:
			Inf @BI := CodeBase + RI;
		    RELOC_VALUE_CELL:
		    <<  if ExtraArgumentP RI then
			    RI := &ArgumentBlock[MakeExtraArgument RI]
			else if LocalIDNumberP RI then
			    RI := &SymVal LocalToGlobalID RI
			else RI := &SymVal RI;
			Inf @BI := RI >>;
		    RELOC_FUNCTION_CELL:
		    <<  if LocalIDNumberP RI then RI := LocalToGlobalID RI;
			Inf @BI :=
			    SymFnc + AddressingUnitsPerFunctionCell*RI >>;
		    RELOC_ID_NUMBER:	% Must be a local ID number
		    <<  if LocalIDNumberP RI then RI := LocalToGlobalID RI;
			Inf @BI := RI >>;
		end >>;
	end >>;
    DelWArray(BT, Top);
					% return the space used by tables
    AddressApply0 E;			% Call the init routine
    CodeBase := OldCodeBase;		% restore previous value for CodeBase
    DelBPS(E, BTop);			% deallocate space of init routine
end;

syslsp procedure PutEntry(Name, Type, Offset);
    PutD(Name, Type, MkCODE(CodeBase + Offset));

off Syslisp;

END;
