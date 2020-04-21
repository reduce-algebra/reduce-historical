%
% TOKEN-SCANNER.RED - Table-driven token scanner
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        27 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.NEW>TOKEN-SCANNER.RED.2,  7-Apr-83 12:29:58, Edit by KESSLER
%  Changed MakeBufIntoFloat so it uses FloatZero, instead of '0.0.
% Edit by Cris Perdue, 11 Mar 1983
% Added argument to MakeBufIntoFloat to specify sign of number
% Edit by Cris Perdue, 29 Jan 1983 1338-PST
% Occurrences of "dipthong" changed to "diphthong"
%  <PSL.KERNEL>TOKEN-SCANNER.RED.2, 16-Dec-82 14:55:55, Edit by BENSON
%  MakeBufIntoFloat uses floating point arithmetic on each digit
%  <PSL.INTERP>TOKEN-SCANNER.RED.6, 15-Sep-82 10:49:54, Edit by BENSON
%  Can now scan 1+ and 1-
%  <PSL.INTERP>TOKEN-SCANNER.RED.12, 10-Jan-82 21:53:28, Edit by BENSON
%  Fixed bug in floating point parsing
%  <PSL.INTERP>TOKEN-SCANNER.RED.9,  8-Jan-82 07:06:23, Edit by GRISS
%  MakeBufIntoLispInteger becomes procedure for BigNums
%  <PSL.INTERP>TOKEN-SCANNER.RED.7, 28-Dec-81 22:09:14, Edit by BENSON
%  Made dipthong indicator last element of scan table

fluid '(CurrentScanTable!* !*Raise !*Compressing !*EOLInStringOK);
LoadTime <<
!*Raise := T;
!*Compressing := NIL;
!*EOLInStringOK := NIL;
>>;

CompileTime flag('(ReadInBuf MakeBufIntoID MakeBufIntoString
		   MakeBufIntoLispInteger MakeBufIntoSysNumber
		   MakeBufIntoFloat MakeStringIntoSysInteger
		   MakeStringIntoBitString ScannerError SysPowerOf2P
		   ScanPossibleDiphthong),
		 'InternalFunction);

on SysLisp;

% DIGITS are 0..9
internal WConst LETTER = 10,
		DELIMITER = 11,
		COMMENTCHAR = 12,
		DIPHTHONGSTART = 13,
		IDESCAPECHAR = 14,
		STRINGQUOTE = 15,
		PACKAGEINDICATOR = 16,
		IGNORE = 17,
		MINUSSIGN = 18,
		PLUSSIGN = 19,
		DECIMALPOINT = 20,
		IDSURROUND = 21;

internal WVar TokCh,
	      TokChannel,
	      ChTokenType,
	      CurrentChar,
	      ChangedPackages,
	      TokRadix,
	      TokSign,
	      TokFloatFractionLength,
	      TokFloatExponentSign,
	      TokFloatExponent;

CompileTime <<
syslsp smacro procedure TokenTypeOfChar Ch;
    IntInf VecItm(VecInf LispVar CurrentScanTable!*, Ch);

syslsp smacro procedure CurrentDiphthongIndicator();
    VecItm(VecInf LispVar CurrentScanTable!*, 128);

syslsp smacro procedure ResetBuf();
    CurrentChar := 0;

syslsp smacro procedure BackupBuf();
    CurrentChar := CurrentChar - 1;
>>;

syslsp procedure ReadInBuf();
<<  TokCh := ChannelReadChar TokChannel;
    StrByt(TokenBuffer, CurrentChar) := TokCh;
    ChTokenType := TokenTypeOfChar TokCh;
    if CurrentChar < MaxTokenSize then
	CurrentChar := CurrentChar + 1
    else if CurrentChar = MaxTokenSize then
    <<  ErrorPrintF("***** READ Buffer overflow, Truncating");
        CurrentChar := MaxTokenSize + 1 >>
    else CurrentChar := MaxTokenSize + 1 >>;

CompileTime <<
syslsp smacro procedure UnReadLastChar();
    ChannelUnReadChar(Channel, TokCh);

syslsp smacro procedure LowerCaseChar Ch;
    Ch >= char !a and Ch <= char !z;

syslsp smacro procedure RaiseChar Ch;
    (Ch - char !a) + char A;

syslsp smacro procedure RaiseLastChar();
    if LowerCaseChar TokCh then
	StrByt(TokenBuffer, CurrentChar - 1) := RaiseChar TokCh;
>>;

syslsp procedure MakeBufIntoID();
<<  LispVar TokType!* := '0;
    if CurrentChar eq 1 then MkID StrByt(TokenBuffer, 0)
    else
    <<  StrByt(TokenBuffer, CurrentChar) := char NULL;
	TokenBuffer[0] := CurrentChar - 1;
	if LispVar !*Compressing then NewID CopyString TokenBuffer
	else Intern MkSTR TokenBuffer >> >>;

syslsp procedure MakeBufIntoString();
<<  LispVar TokType!* := '1;
    StrByt(TokenBuffer, CurrentChar) := 0;
    TokenBuffer[0] := CurrentChar - 1;
    CopyString TokenBuffer >>;

syslsp procedure MakeBufIntoSysNumber(Radix, Sign);
<<  StrByt(TokenBuffer, CurrentChar) := 0;
    TokenBuffer[0] := CurrentChar - 1;
    MakeStringIntoSysInteger(TokenBuffer, Radix, Sign) >>;

syslsp procedure MakeBufIntoLispInteger(Radix, Sign);
<<  LispVar TokType!* := '2;
    StrByt(TokenBuffer, CurrentChar) := 0;
    TokenBuffer[0] := CurrentChar - 1;
    MakeStringIntoLispInteger(MkSTR TokenBuffer, Radix, Sign) >>;

internal WArray MakeFloatTemp1[1],
		MakeFloatTemp2[1],
		FloatTen[1],
		FloatZero[1];

% Changed to use floating point arithmetic on the characters, rather
% than converting to an integer.  This avoids overflow problems.

syslsp procedure MakeBufIntoFloat(Exponent, MinusP);
begin scalar F, N;
    !*WFloat(FloatTen, 10);
    !*WFloat(MakeFloatTemp1, 0);
    !*WFloat(FloatZero, 0);
    N := CurrentChar - 1;
    for I := 0 step 1 until N do
    <<  !*WFloat(MakeFloatTemp2, DigitToNumber StrByt(TokenBuffer, I));
	!*FTimes2(MakeFloatTemp1, MakeFloatTemp1, FloatTen);
	!*FPlus2(MakeFloatTemp1, MakeFloatTemp1, MakeFloatTemp2) >>;
    if Exponent > 0 then
	for I := 1 step 1 until Exponent do
	    !*FTimes2(MakeFloatTemp1, MakeFloatTemp1, FloatTen)
    else if Exponent < 0 then
    <<  Exponent := -Exponent;
	for I := 1 step 1 until Exponent do
	    !*FQuotient(MakeFloatTemp1, MakeFloatTemp1, FloatTen) >>;
    if Minusp then
	!*FDifference(MakeFloatTemp1, FloatZero, MakeFloatTemp1);
	%% Gack.  It is necessary to quote 0.0 in SysLISP mode!
	%% Is it because of the direct call on a CMACRO?  Think not. /csp
    LispVar TokType!* := '2;
    F := GtFLTN();
    !*FAssign(FloatBase F, MakeFloatTemp1);
    return MkFLTN F;
end;


syslsp procedure ChannelReadToken Channel;	%. Token scanner
%
% This is the basic Lisp token scanner.  The value returned is a Lisp
% item corresponding to the next token from the input stream.  IDs will
% be interned.  The global Lisp variable TokType!* will be set to
%	0 if the token is an ordinary ID,
%	1 if the token is a string (delimited by double quotes),
%	2 if the token is a number, or
%	3 if the token is an unescaped delimiter.
% In the last case, the value returned by this function will be the single
% character ID corresponding to the delimiter.
%
begin
    TokChannel := Channel;
    ChangedPackages := 0;
    ResetBuf();
StartScanning:
    TokCh := ChannelReadChar Channel;
    ChTokenType := TokenTypeOfChar TokCh;
    if ChTokenType eq IGNORE then goto StartScanning;
    StrByt(TokenBuffer, CurrentChar) := TokCh;
    CurrentChar := CurrentChar + 1;
    case ChTokenType of
    0 to 9:	 % digit
    <<  TokSign := 1;
	goto InsideNumber >>;
    10:	 % Start of ID
    <<  if null LispVar !*Raise then
	    goto InsideID
	else
	<<  RaiseLastChar();
	    goto InsideRaisedID >> >>;
    11:	 % Delimiter, but not beginning of Diphthong
    <<  LispVar TokType!* := '3;
	return MkID TokCh >>;
    12:	 % Start of comment
	goto InsideComment;
    13:	 % Diphthong start - Lisp function uses P-list of starting char
	return ScanPossibleDiphthong(TokChannel, MkID TokCh);
    14:	 % ID escape character
    <<  if null LispVar !*Raise then
	    goto GotEscape
	else goto GotEscapeInRaisedID >>;
    15:	 % string quote
    <<  BackupBuf();
	goto InsideString >>;
    16:	 % Package indicator - at start of token means use global package
    <<  ResetBuf();
	ChangedPackages := 1;
	Package 'Global;
	if null LispVar !*Raise then
	    goto GotPackageMustGetID
	else goto GotPackageMustGetIDRaised >>;
    17:	 % Ignore - can't ever happen
	ScannerError("Internal error - consult a wizard");
    18:	 % Minus sign
    <<  TokSign := -1;
	goto GotSign >>;
    19:	 % Plus sign
    <<  TokSign := 1;
	goto GotSign >>;
    20:  % decimal point
    <<  ResetBuf();
	ReadInBuf();
	if ChTokenType >= 10 then
	<<  UnReadLastChar();
	    return ScanPossibleDiphthong(TokChannel, '!.) >>
	else
	<<  TokSign := 1;
	    TokFloatFractionLength := 1;
	    goto InsideFloatFraction >> >>;
    21:					% IDSURROUND, i.e. vertical bars
    <<  BackupBuf();
	goto InsideIDSurround >>;
    default:
	return ScannerError("Unknown token type")
    end;
GotEscape:
    BackupBuf();
    ReadInBuf();
    goto InsideID;
InsideID:
    ReadInBuf();
    if ChTokenType <= 10
	    or ChTokenType eq PLUSSIGN
	    or ChTokenType eq MINUSSIGN then goto InsideID
    else if ChTokenType eq IDESCAPECHAR then goto GotEscape
    else if ChTokenType eq PACKAGEINDICATOR then
    <<  BackupBuf();
	ChangedPackages := 1;
	Package MakeBufIntoID();
	ResetBuf();
	goto GotPackageMustGetID >>
    else
    <<  UnReadLastChar();
	BackupBuf();
	if ChangedPackages neq 0 then Package LispVar CurrentPackage!*;
	return MakeBufIntoID() >>;
GotPackageMustGetID:
    ReadInBuf();
    if ChTokenType eq LETTER then goto InsideID
    else if ChTokenType eq IDESCAPECHAR then goto GotEscape
    else ScannerError("Illegal to follow package indicator with non ID");
GotEscapeInRaisedID:
    BackupBuf();
    ReadInBuf();
    goto InsideRaisedID;
InsideRaisedID:
    ReadInBuf();
    if ChTokenType < 10 
	    or ChTokenType eq PLUSSIGN
	    or ChTokenType eq MINUSSIGN then goto InsideRaisedID
    else if ChTokenType eq 10 then
	<<  RaiseLastChar();
	    goto InsideRaisedID >>
    else if ChTokenType eq IDESCAPECHAR then goto GotEscapeInRaisedID
    else if ChTokenType eq PACKAGEINDICATOR then
    <<  BackupBuf();
	ChangedPackages := 1;
	Package MakeBufIntoID();
	ResetBuf();
	goto GotPackageMustGetIDRaised >>
    else
    <<  UnReadLastChar();
	BackupBuf();
	if ChangedPackages neq 0 then Package LispVar CurrentPackage!*;
	return MakeBufIntoID() >>;
GotPackageMustGetIDRaised:
    ReadInBuf();
    if ChTokenType eq LETTER then
    <<  RaiseLastChar();
	goto InsideRaisedID >>
    else if ChTokenType eq IDESCAPECHAR then goto GotEscapeInRaisedID
    else ScannerError("Illegal to follow package indicator with non ID");
InsideString:
    ReadInBuf();
    if ChTokenType eq STRINGQUOTE then
    <<  BackupBuf();
	ReadInBuf();
	if ChTokenType eq STRINGQUOTE then goto InsideString
	else
	<<  UnReadLastChar();
	    BackupBuf();
	    return MakeBufIntoString() >> >>
    else if TokCh eq char EOL and not LispVar !*EOLInStringOK then
	ErrorPrintF("*** String continued over end-of-line")
    else if TokCh eq char EOF then
	ScannerError("EOF encountered inside a string");
    goto InsideString;
InsideIDSurround:
    ReadInBuf();
    if ChTokenType eq IDSURROUND then
    <<  BackupBuf();
	return MakeBufIntoID() >>
    else if ChTokenType eq IDESCAPECHAR then
    <<  BackupBuf();
	ReadInBuf() >>
    else if TokCh eq char EOF then
	ScannerError("EOF encountered inside an ID");
    goto InsideIDSurround;
GotSign:
    ResetBuf();
    ReadInBuf();
    if TokCh eq char !. then
    <<  PutStrByt(TokenBuffer, 0, char !0);
	CurrentChar := 2;
	goto InsideFloat >>
    else if ChTokenType eq LETTER	% patch to be able to read 1+ and 1-
	    or ChTokenType eq MINUSSIGN
	    or ChTokenType eq PLUSSIGN then
    <<  ResetBuf();
	StrByt(TokenBuffer, 0) := if TokSign < 0 then char !- else char !+;
	StrByt(TokenBuffer, 1) := TokCh;
	CurrentChar := 2;
	if LispVar !*Raise then
	<<  RaiseLastChar();
	    goto InsideRaisedID >>
	else goto InsideID >>
    else if ChTokenType eq IDESCAPECHAR then
    <<  ResetBuf();
	StrByt(TokenBuffer, 0) := if TokSign < 0 then char !- else char !+;
	CurrentChar := 1;
	if LispVar !*Raise then
	    goto GotEscapeInRaisedID
	else goto GotEscape >>
    else if ChTokenType > 9 then
    <<  UnReadLastChar();	 % Allow + or - to start a Diphthong
	return ScanPossibleDiphthong(Channel,
				    MkID(if TokSign < 0 then char !-
					     else char !+)) >>
    else goto InsideNumber;
InsideNumber:
    ReadInBuf();
    if ChTokenType < 10 then goto InsideNumber;
    if TokCh eq char !# then
    <<  BackupBuf();
	TokRadix := MakeBufIntoSysNumber(10, 1);
	ResetBuf();
	if TokRadix < 2 or TokRadix > 36 then
	    return ScannerError("Radix out of range");
	if TokRadix <= 10 then goto InsideIntegerRadixUnder10
	else goto InsideIntegerRadixOver10 >>
    else if TokCh eq char !. then goto InsideFloat
    else if TokCh eq char B or TokCh eq char !b then
    <<  BackupBuf();
	return MakeBufIntoLispInteger(8, TokSign) >>
    else if TokCh eq char E or TokCh eq char !e then
    <<  TokFloatFractionLength := 0;
	goto InsideFloatExponent >>
    else if ChTokenType eq LETTER	% patch to be able to read 1+ and 1-
	    or ChTokenType eq MINUSSIGN
	    or ChTokenType eq PLUSSIGN then
	if LispVar !*Raise then
	<<  RaiseLastChar();
	    goto InsideRaisedID >>
	else goto InsideID
    else if ChTokenType eq IDESCAPECHAR then
	if LispVar !*Raise then
	    goto GotEscapeInRaisedID
	else goto GotEscape
    else
    <<  UnReadLastChar();
	BackupBuf();
	return MakeBufIntoLispInteger(10, TokSign) >>;
InsideIntegerRadixUnder10:
    ReadInBuf();
    if ChTokenType < TokRadix then goto InsideIntegerRadixUnder10;
    if ChTokenType < 10 then return ScannerError("Digit out of range");
NumReturn:
    UnReadLastChar();
    BackupBuf();
    return MakeBufIntoLispInteger(TokRadix, TokSign);
InsideIntegerRadixOver10:
    ReadInBuf();
    if ChTokenType < 10 then goto InsideIntegerRadixOver10;
    if ChTokenType > 10 then goto NumReturn;
    if LowerCaseChar TokCh then
    <<  TokCh := RaiseChar TokCh;
	StrByt(TokenBuffer, CurrentChar - 1) :=  TokCh >>;
    if TokCh >= char A - 10 + TokRadix then goto NumReturn;
    goto InsideIntegerRadixOver10;
InsideFloat:	 % got decimal point inside number
    BackupBuf();
    ReadInBuf();
    if TokCh eq char E or TokCh eq char !e then
    <<  TokFloatFractionLength := 0;
	goto InsideFloatExponent >>;
    if ChTokenType >= 10 then	 % nnn. is floating point number
    <<  UnReadLastChar();
	BackupBuf();
	return MakeBufIntoFloat(0,TokSign<0) >>;
    TokFloatFractionLength := 1;
InsideFloatFraction:
    ReadInBuf();
    if ChTokenType < 10 then
    <<  if TokFloatFractionLength < 9 then
	    TokFloatFractionLength := TokFloatFractionLength + 1
	else BackupBuf();		% don't overflow mantissa
	goto InsideFloatFraction >>;
    if TokCh eq char E or TokCh eq char lower e then goto InsideFloatExponent;
    UnReadLastChar();
    BackupBuf();
    return MakeBufIntoFloat((-TokFloatFractionLength), TokSign<0);
InsideFloatExponent:
    BackupBuf();
    TokFloatExponentSign := 1;
    TokFloatExponent := 0;
    TokCh := ChannelReadChar TokChannel;
    ChTokenType := TokenTypeOfChar TokCh;
    if ChTokenType < 10 then
    <<  TokFloatExponent := ChTokenType;
	goto DigitsInsideExponent >>;
    if TokCh eq char '!- then TokFloatExponentSign := -1
    else if TokCh neq char '!+ then
	return ScannerError("Missing exponent in float");
    TokCh := ChannelReadChar TokChannel;
    ChTokenType := TokenTypeOfChar TokCh;
    if ChTokenType >= 10 then
	return ScannerError("Missing exponent in float");
    TokFloatExponent := ChTokenType;
DigitsInsideExponent:
    TokCh := ChannelReadChar TokChannel;
    ChTokenType := TokenTypeOfChar TokCh;
    if ChTokenType < 10 then
    <<  TokFloatExponent := TokFloatExponent * 10 + ChTokenType;
	goto DigitsInsideExponent >>;
    ChannelUnReadChar(Channel, TokCh);
    return MakeBufIntoFloat((TokFloatExponentSign * TokFloatExponent
			    - TokFloatFractionLength), TokSign<0);
InsideComment:
    if (TokCh := ChannelReadChar Channel) eq char EOL then
    <<  ResetBuf();
	goto StartScanning >>
    else if TokCh eq char EOF then return LispVar !$EOF!$
    else goto InsideComment;
end;

syslsp procedure RAtom();	%. Read token from current input
    ChannelReadToken LispVar IN!*;

syslsp procedure DigitToNumber D;
%
% if D is not a digit then it is assumed to be an uppercase letter
%
    if D >= char !0 and D <= char !9 then D - char !0 else D - (char A - 10);

syslsp procedure MakeStringIntoLispInteger(S, Radix, Sign);
    Sys2Int MakeStringIntoSysInteger(S, Radix, Sign);

syslsp procedure MakeStringIntoSysInteger(Strng, Radix, Sign);
%
% Unsafe string to integer conversion.  Strng is assumed to contain
% only digits and possibly uppercase letters for radices > 10.  Since it
% uses multiplication, arithmetic overflow may occur. Sign is +1 or -1
%
begin scalar Count, Tot, RadixExponent;
    if RadixExponent := SysPowerOf2P Radix then return
	MakeStringIntoBitString(Strng, Radix, RadixExponent, Sign);
    Strng := StrInf Strng;
    Count := StrLen Strng;	
    Tot := 0;
    for I := 0 step 1 until Count do
	Tot := Tot * Radix + DigitToNumber StrByt(Strng, I);
    return if Sign < 0 then -Tot else Tot;
end;

syslsp procedure MakeStringIntoBitString(Strng, Radix, RadixExponent, Sign);
begin scalar Count, Tot;
    Strng := StrInf Strng;
    Count := StrLen Strng;
    Tot := 0;
    for I := 0 step 1 until Count do
    <<  Tot := LSH(Tot, RadixExponent);
	Tot := LOR(Tot, DigitToNumber StrByt(Strng, I)) >>;
    if Sign < 0 then return -Tot;
    return Tot;
end;

syslsp procedure SysPowerOf2P Num;
    case Num of
      1: 0;
      2: 1;
      4: 2;
      8: 3;
      16: 4;
      32: 5;
      default: NIL
    end;

syslsp procedure ScannerError Message;
    StdError BldMsg("***** Error in token scanner: %s", Message);

syslsp procedure ScanPossibleDiphthong(Channel, StartChar);
begin scalar Alst, Target, Ch;
    LispVar TokType!* := '3;
    if null (Alst := get(StartChar, CurrentDiphthongIndicator())) then
	return StartChar;
    if null (Target := Atsoc(Ch := MkID ChannelReadChar Channel, Alst)) then
    <<  ChannelUnReadChar(Channel, IDInf Ch);
	return StartChar >>;
    return cdr Target;
end;

syslsp procedure ReadLine();
<<  MakeInputAvailable();
    ChannelReadLine LispVar IN!* >>;

syslsp procedure ChannelReadLine Chn;
begin scalar C;
    TokenBuffer[0] := -1;
    while (C := ChannelReadChar Chn) neq char EOL and C neq char EOF do
    <<  TokenBuffer[0] := TokenBuffer[0] + 1;
	StrByt(TokenBuffer, TokenBuffer[0]) := C >>;
    return if TokenBuffer[0] >= 0 then
    <<  StrByt(TokenBuffer, TokenBuffer[0] + 1) := char NULL;
	CopyString MkSTR TokenBuffer >>
    else '"";
end;

% Dummy definition of package conversion function

syslsp procedure Package U;
    NIL;

% Dummy definition of MakeInputAvailable, redefined by Emode

syslsp procedure MakeInputAvailable();
    NIL;

off SysLisp;

END;


