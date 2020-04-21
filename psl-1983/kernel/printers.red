%
% PRINTERS.RED - Printing functions for various data types
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        27 August 1981
% Copyright (c) 1981 University of Utah
%
%  <PSL.KERNEL>PRINTERS.RED.17,  7-Mar-83 11:53:59, Edit by KESSLER
%  Change Channelwriteblankoreol to check linelength = 0 also.
% Edit by MLGriss, 11:31am  Saturday, 5 February 1983
%   Fix ChannelWriteBitstring to put out a single 0 if needed
%   Fixed to handle largest NEGATIVE number correctly
%   Used to get ------, since -(largest neg) NOT=largestPOS
% <PSL.KERNEL>PRINTERS.RED.14, 31-Jan-83 15:45:30, Edit by PERDUE
% Fix to printing of EVECTORs
% Edit by Cris Perdue, 29 Jan 1983 1620-PST
% Removed definition of EVecInf (both compile- and load-time)
% Edit by Cris Perdue, 27 Jan 1983 1436-PST
% Put in Kessler's change so CheckLineFit won't write EOL if LineLength = 0
%  <PSL.KERNEL>PRINTERS.RED.11, 10-Jan-83 13:58:14, Edit by PERDUE
%  Added some code to handle EVectors, especially to represent OBJECTs
%  <PSL.KERNEL>PRINTERS.RED.10, 21-Dec-82 15:24:18, Edit by BENSON
%  Changed order of tests in WriteInteger so that -ive hex #s are done right
%  <PSL.KERNEL>PRINTERS.RED.9,  4-Oct-82 10:04:34, Edit by BENSON
%  Added PrinLength and PrinLevel
%  <PSL.KERNEL>PRINTERS.RED.3, 23-Sep-82 13:16:20, Edit by BENSON
%  Look for # of args in code pointer, changed : to space in #<...> stuff
%  <PSL.INTERP>PRINTERS.RED.12,  2-Sep-82 09:01:31, Edit by BENSON
%  (QUOTE x y) prints correctly, not as 'x
%  <PSL.INTERP>PRINTERS.RED.11,  4-May-82 20:31:32, Edit by BENSON
%  Printers keep tags on, for Emode GC
%  <PSL.VAX-INTERP>PRINTERS.RED.6, 18-Feb-82 16:30:12, Edit by BENSON
%  Added printer for unbound, changed code to #<Code:xx>
%  <PSL.VAX-INTERP>PRINTERS.RED.2, 20-Jan-82 02:11:16, Edit by GRISS
%  fixed prining of zero length vectors
%  <PSL.VAX-INTERP>PRINTERS.RED.1, 15-Jan-82 14:27:13, Edit by BENSON
%  Changed for new integer tags
%  <PSL.INTERP>PRINTERS.RED.13,  7-Jan-82 22:47:40, Edit by BENSON
%  Made (QUOTE xxx) print as 'xxx
%  <PSL.INTERP>PRINTERS.RED.12,  5-Jan-82 21:37:41, Edit by BENSON
%  Changed OBase to OutputBase!*

fluid '(OutputBase!*			% current output base
        PrinLength			% length of structures to print
	PrinLevel			% level of recursion to print
	CurrentScanTable!*
	IDEscapeChar!*
	!*Lower);		% print IDs with uppercase chars lowered
global '(LispScanTable!*);

LoadTime
<<  OutputBase!* := 10;
    IDEscapeChar!* := 33;		% (char !!)
    CurrentScanTable!* := LispScanTable!* >>; % so TokenTypeOfChar works right

on SysLisp;

CompileTime <<
syslsp smacro procedure UpperCaseP Ch;
    Ch >= char A and Ch <= char Z;

syslsp smacro procedure LowerCaseP Ch;
    Ch >= char !a and Ch <= char !z;

syslsp smacro procedure RaiseChar Ch;
    (Ch - char !a) + char A;

syslsp smacro procedure LowerChar Ch;
    (Ch - char A) + char !a;
>>;

CompileTime flag('(CheckLineFit WriteNumber1 ChannelWriteBitString),
		 'InternalFunction);

%. Writes EOL first if given Len causes max line length to be exceeded
syslsp procedure CheckLineFit(Len, Chn, Fn, Itm);
<<  if (LinePosition[Chn] + Len > MaxLine[Chn]) and (MaxLine[Chn] > 0) then
	ChannelWriteChar(Chn, char EOL);
    IDApply2(Chn, Itm, Fn) >>;

syslsp procedure ChannelWriteString(Channel, Strng);
%
% Strng may be tagged or not, but it must have a length field accesible
% by StrLen.
%
begin scalar UpLim;
    UpLim := StrLen StrInf Strng;
    for I := 0 step 1 until UpLim do
	ChannelWriteChar(Channel, StrByt(StrInf Strng, I));
end;

syslsp procedure WriteString S;
    ChannelWriteString(LispVar OUT!*, S);

internal WString DigitString = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
internal WString WriteNumberBuffer[40];

syslsp procedure ChannelWriteSysInteger(Channel, Number, Radix);
begin scalar Exponent,N1;
    return if (Exponent := SysPowerOf2P Radix) then
	ChannelWriteBitString(Channel, Number, Radix - 1, Exponent)
    else if Number < 0 then
    <<  ChannelWriteChar(Channel, char '!-);
        WriteNumber1(Channel,-(Number/Radix),Radix); % To catch largest NEG
	ChannelWriteChar(Channel,
			 StrByt(DigitString, - MOD(Number, Radix))) >>
    else if Number = 0 then ChannelWriteChar(Channel, char !0)
    else WriteNumber1(Channel, Number, Radix);
end;

syslsp procedure WriteNumber1(Channel, Number, Radix);
    if Number = 0 then Channel
    else
    <<  WriteNumber1(Channel, Number / Radix, Radix);
	ChannelWriteChar(Channel,
			 StrByt(DigitString, MOD(Number, Radix))) >>;

syslsp procedure ChannelWriteBitString(Channel, Number, DigitMask, Exponent);
 if Number = 0 then ChannelWriteChar(Channel,char !0)
  else  ChannelWriteBitStrAux(Channel, Number, DigitMask, Exponent);

syslsp procedure ChannelWriteBitStrAux(Channel, Number, DigitMask, Exponent);
    if Number = 0 then Channel		% Channel means nothing here
    else				% just trying to fool the compiler
    <<  ChannelWriteBitStrAux(Channel,
			      LSH(Number, -Exponent),
			      DigitMask,
			      Exponent);
	ChannelWriteChar(Channel,
			 StrByt(DigitString,
				LAND(Number, DigitMask))) >>;

syslsp procedure WriteSysInteger(Number, Radix);
    ChannelWriteSysInteger(LispVar OUT!*, Number, Radix);

syslsp procedure ChannelWriteFixnum(Channel, Num);
    ChannelWriteInteger(Channel, FixVal FixInf Num);

syslsp procedure ChannelWriteInteger(Channel, Num);
begin scalar CurrentBase;
    if (CurrentBase := LispVar OutputBase!*) neq 10 then
    <<  ChannelWriteSysInteger(Channel, CurrentBase, 10);
	ChannelWriteChar(Channel, char !#) >>;
    ChannelWriteSysInteger(Channel,
			   Num,
			   CurrentBase);
end;

syslsp procedure ChannelWriteSysFloat(Channel, FloatPtr);
begin scalar Ch, ChIndex;
    WriteFloat(WriteNumberBuffer, FloatPtr);
    ChannelWriteString(Channel, WriteNumberBuffer);
end;

syslsp procedure ChannelWriteFloat(Channel, LispFloatPtr);
    ChannelWriteSysFloat(Channel, FloatBase FltInf LispFloatPtr);

syslsp procedure ChannelPrintString(Channel, Strng);
begin scalar Len, Ch;
    ChannelWriteChar(Channel, char !");
    Len := StrLen StrInf Strng;
    for I := 0 step 1 until Len do
    <<  Ch := StrByt(StrInf Strng, I);
	if Ch eq char !" then ChannelWriteChar(Channel, char !");
	ChannelWriteChar(Channel, Ch) >>;
    ChannelWriteChar(Channel, char !");
end;

syslsp procedure ChannelWriteID(Channel, Itm);
    if not LispVar !*Lower then
	ChannelWriteString(Channel, SymNam IDInf Itm)
    else begin scalar Ch, Len;
	Itm := StrInf SymNam IDInf Itm;
	Len := StrLen Itm;
	for I := 0 step 1 until Len do
	<<  Ch := StrByt(Itm, I);
	    if UpperCaseP Ch then Ch := LowerChar Ch;
	    ChannelWriteChar(Channel, Ch) >>;
    end;

syslsp procedure ChannelWriteUnbound(Channel, Itm);
<<  ChannelWriteString(Channel, "#<Unbound:");
    ChannelWriteID(Channel, Itm);
    ChannelWriteChar(Channel, char '!>) >>;

syslsp procedure ChannelPrintID(Channel, Itm);
begin scalar Len, Ch, TokenType;
    Itm := StrInf SymNam IDInf Itm;
    Len := StrLen Itm;
    Ch := StrByt(Itm, 0);
    if TokenTypeOfChar Ch neq 10 then ChannelWriteChar(Channel,
						       LispVar IDEscapeChar!*);
    if not LispVar !*Lower then
    <<  ChannelWriteChar(Channel, Ch);
	for I := 1 step 1 until Len do
	<<  Ch := StrByt(Itm, I);
	    TokenType := TokenTypeOfChar Ch;
	    if not (TokenType <= 10
			or TokenType eq PLUSSIGN
			or TokenType eq MINUSSIGN) then
		ChannelWriteChar(Channel, LispVar IDEscapeChar!*);
	    ChannelWriteChar(Channel, Ch) >> >>
    else
    <<  if UpperCaseP Ch then Ch := LowerChar Ch;
	ChannelWriteChar(Channel, Ch);
	for I := 1 step 1 until Len do
	<<  Ch := StrByt(Itm, I);
	    TokenType := TokenTypeOfChar Ch;
	    if not (TokenType <= 10
			or TokenType eq PLUSSIGN
			or TokenType eq MINUSSIGN) then
	        ChannelWriteChar(Channel, LispVar IDEscapeChar!*);
	    if UpperCaseP Ch then Ch := LowerChar Ch;
	    ChannelWriteChar(Channel, Ch) >> >>
end;

syslsp procedure ChannelPrintUnbound(Channel, Itm);
<<  ChannelWriteString(Channel, "#<Unbound ");
    ChannelPrintID(Channel, Itm);
    ChannelWriteChar(Channel, char '!>) >>;

syslsp procedure ChannelWriteCodePointer(Channel, CP);
begin scalar N;
    CP := CodeInf CP;
    ChannelWriteString(Channel, "#<Code ");
    N := !%code!-number!-of!-arguments CP;
    if N >= 0 and N <= MaxArgs then
    <<  ChannelWriteSysInteger(Channel, N, 10);
	ChannelWriteChar(Channel, char BLANK) >>:
    ChannelWriteSysInteger(Channel, CP, CompressedBinaryRadix);
    ChannelWriteChar(Channel, char '!>);
end;

syslsp procedure ChannelWriteUnknownItem(Channel, Itm);
<<  ChannelWriteString(Channel, "#<Unknown ");
    ChannelWriteSysInteger(Channel, Itm, CompressedBinaryRadix);
    ChannelWriteChar(Channel, char !>) >>;

syslsp procedure ChannelWriteBlankOrEOL Channel;
<<  if (LinePosition[Channel] + 1 >= MaxLine[Channel]) and
       (MaxLine[Channel] > 0) then
	ChannelWriteChar(Channel, char EOL)
    else
	ChannelWriteChar(Channel, char ! ) >>;

syslsp procedure ChannelWritePair(Channel, Itm, Level);
    if IntP LispVar PrinLevel and Level >= LispVar PrinLevel then
	ChannelWriteChar(Channel, char '!#)
    else
begin scalar N;
    Level := Level + 1;
    CheckLineFit(1, Channel, 'ChannelWriteChar, char !( );
    if not IntP LispVar PrinLength or 1 <= LispVar PrinLength then
    <<  RecursiveChannelPrin2(Channel, car Itm, Level);
	N := 2;
	Itm := cdr Itm;
	while PairP Itm and
		(not IntP LispVar PrinLength or N <= LispVar PrinLength) do
	<<  ChannelWriteBlankOrEOL Channel;
	    RecursiveChannelPrin2(Channel, car Itm, Level);
	    N := N + 1;
	    Itm := cdr Itm >>;
	if PairP Itm then
	    CheckLineFit(3, Channel, 'ChannelWriteString, " ...")
	else
	if Itm then
	<<  CheckLineFit(3, Channel, 'ChannelWriteString, " . ");
	    RecursiveChannelPrin2(Channel, Itm, Level) >> >>
    else
	CheckLineFit(3, Channel, 'ChannelWriteString, "...");
    CheckLineFit(1, Channel, 'ChannelWriteChar, char !) );
end;

syslsp procedure ChannelPrintPair(Channel, Itm, Level);
    if IntP LispVar PrinLevel and Level >= LispVar PrinLevel then
	ChannelWriteChar(Channel, char '!#)
    else
begin scalar N;
    Level := Level + 1;
    CheckLineFit(1, Channel, 'ChannelWriteChar, char !( );
    if not IntP LispVar PrinLength or 1 <= LispVar PrinLength then
    <<  RecursiveChannelPrin1(Channel, car Itm, Level);
	N := 2;
	Itm := cdr Itm;
	while PairP Itm and
		(not IntP LispVar PrinLength or N <= LispVar PrinLength) do
	<<  ChannelWriteBlankOrEOL Channel;
	    RecursiveChannelPrin1(Channel, car Itm, Level);
	    N := N + 1;
	    Itm := cdr Itm >>;
	if PairP Itm then
	    CheckLineFit(3, Channel, 'ChannelWriteString, " ...")
	else
	if Itm then
	<<  CheckLineFit(3, Channel, 'ChannelWriteString, " . ");
	    RecursiveChannelPrin1(Channel, Itm, Level) >> >>
    else
	CheckLineFit(3, Channel, 'ChannelWriteString, "...");
    CheckLineFit(1, Channel, 'ChannelWriteChar, char !) );
end;

syslsp procedure ChannelWriteVector(Channel, Vec, Level);
    if IntP LispVar PrinLevel and Level >= LispVar PrinLevel then
	ChannelWriteChar(Channel, char '!#)
    else
begin scalar Len, I;
    Level := Level + 1;
    CheckLineFit(1, Channel, 'ChannelWriteChar, char '![ );
    Len := VecLen VecInf Vec;
    If Len<0 then     
      return CheckLineFit(1, Channel, 'ChannelWriteChar, char '!] );
    I := 0;
LoopBegin:
    if not IntP LispVar PrinLength or I < LispVar PrinLength then
    <<  RecursiveChannelPrin2(Channel, VecItm(VecInf Vec, I), Level);
	if (I := I + 1) <= Len then
	<<  ChannelWriteBlankOrEOL Channel;
	    goto LoopBegin >> >>
    else
	CheckLineFit(3, Channel, 'ChannelWriteString, "...");	
    CheckLineFit(1, Channel, 'ChannelWriteChar, char '!] );
end;

syslsp procedure ChannelPrintVector(Channel, Vec, Level);
    if IntP LispVar PrinLevel and Level >= LispVar PrinLevel then
	ChannelWriteChar(Channel, char '!#)
    else
begin scalar Len, I;
    Level := Level + 1;
    CheckLineFit(1, Channel, 'ChannelWriteChar, char '![ );
    Len := VecLen VecInf Vec;
    If Len<0 then     
      return CheckLineFit(1, Channel, 'ChannelWriteChar, char '!] );
    I := 0;
LoopBegin:
    if not IntP LispVar PrinLength or I < LispVar PrinLength then
    <<  RecursiveChannelPrin1(Channel, VecItm(VecInf Vec, I), Level);
	if (I := I + 1) <= Len then
	<<  ChannelWriteBlankOrEOL Channel;
	    goto LoopBegin >> >>
    else
	CheckLineFit(3, Channel, 'ChannelWriteString, "...");	
    CheckLineFit(1, Channel, 'ChannelWriteChar, char '!] );
end;

syslsp procedure ChannelWriteEVector(Channel, EVec, Level);
begin
    scalar handler;
    if IntP LispVar PrinLevel and Level >= LispVar PrinLevel then
	ChannelWriteChar(Channel, char '!#)
    else
        if getd('object!-get!-handler!-quietly)
	   and (handler :=
	         object!-get!-handler!-quietly(EVec, 'ChannelPrin)) then
	   apply(handler, list(EVec, Channel, Level, NIL))
	else
	<< ChannelWriteString(Channel, "#<EVector ");
	   ChannelWriteSysInteger(Channel, EVecInf EVec,
					CompressedBinaryRadix);
	   ChannelWriteChar(Channel, char '!>); >>;
end;

syslsp procedure ChannelPrintEVector(Channel, EVec, Level);
begin
    scalar handler;
    if IntP LispVar PrinLevel and Level >= LispVar PrinLevel then
	ChannelWriteChar(Channel, char '!#)
    else
        if getd('object!-get!-handler!-quietly)
	   and (handler :=
	         object!-get!-handler!-quietly(EVec, 'ChannelPrin)) then
	   apply(handler, list(EVec, Channel, Level, T))
	else
	<< ChannelWriteString(Channel, "#<EVector ");
	   ChannelWriteSysInteger(Channel, EVecInf EVec,
					CompressedBinaryRadix);
	   ChannelWriteChar(Channel, char '!>); >>;
end;

syslsp procedure ChannelWriteWords(Channel, Itm);
begin scalar Len, I;
    ChannelWriteString(Channel, "#<Words:");
    Len := WrdLen WrdInf Itm;
    if Len < 0 then     
      return CheckLineFit(1, Channel, 'ChannelWriteChar, char '!> );
    I := 0;
LoopBegin:
    if not IntP LispVar PrinLength or I < LispVar PrinLength then
    <<  CheckLineFit(10, Channel, 'ChannelWriteInteger, WrdItm(WrdInf Itm, I));
	if (I := I + 1) <= Len then
	<<  ChannelWriteBlankOrEOL Channel;
	    goto LoopBegin >> >>
    else
	CheckLineFit(3, Channel, 'ChannelWriteString, "...");
    CheckLineFit(1, Channel, 'ChannelWriteChar, char '!> );
end;

syslsp procedure ChannelWriteHalfWords(Channel, Itm);
begin scalar Len, I;
    ChannelWriteString(Channel, "#<Halfwords:");
    Len := HalfWordLen HalfWordInf Itm;
    if Len < 0 then     
      return CheckLineFit(1, Channel, 'ChannelWriteChar, char '!> );
    I := 0;
LoopBegin:
    if not IntP LispVar PrinLength or I < LispVar PrinLength then
    <<  CheckLineFit(10, Channel, 'ChannelWriteInteger,
			HalfWordItm(HalfWordInf Itm, I));
	if (I := I + 1) <= Len then
	<<  ChannelWriteBlankOrEOL Channel;
	    goto LoopBegin >> >>
    else
	CheckLineFit(3, Channel, 'ChannelWriteString, "...");
    CheckLineFit(1, Channel, 'ChannelWriteChar, char '!> );
end;

syslsp procedure ChannelWriteBytes(Channel, Itm);
begin scalar Len, I;
    ChannelWriteString(Channel, "#<Bytes:");
    Len := StrLen StrInf Itm;
    if Len < 0 then     
      return CheckLineFit(1, Channel, 'ChannelWriteChar, char '!> );
    I := 0;
LoopBegin:
    if not IntP LispVar PrinLength or I < LispVar PrinLength then
    <<  CheckLineFit(10, Channel, 'ChannelWriteInteger, StrByt(StrInf Itm, I));
	if (I := I + 1) <= Len then
	<<  ChannelWriteBlankOrEOL Channel;
	    goto LoopBegin >> >>
    else
	CheckLineFit(3, Channel, 'ChannelWriteString, "...");
    CheckLineFit(1, Channel, 'ChannelWriteChar, char '!> );
end;

syslsp procedure ChannelPrin2(Channel, Itm);	%. Display Itm on Channel
    RecursiveChannelPrin2(Channel, Itm, 0);

syslsp procedure RecursiveChannelPrin2(Channel, Itm, Level);
<<  case Tag Itm of
	PosInt, NegInt:
	    CheckLineFit(10, Channel, 'ChannelWriteInteger, Itm);
	ID:
	    CheckLineFit(StrLen StrInf SymNam IDInf Itm + 1,
				Channel, 'ChannelWriteID, Itm);
	UNBOUND:
	    CheckLineFit(StrLen StrInf SymNam IDInf Itm + 12,
				Channel, 'ChannelWriteUnbound, Itm);
	STR:
	    CheckLineFit(StrLen StrInf Itm + 1,
				Channel, 'ChannelWriteString, Itm);
	CODE:
	    CheckLineFit(14, Channel, 'ChannelWriteCodePointer, Itm);
	FIXN:
	    CheckLineFit(20, Channel, 'ChannelWriteFixnum, Itm);
	FLTN:
	    CheckLineFit(30, Channel, 'ChannelWriteFloat, Itm);
	WRDS:
	    ChannelWriteWords(Channel, Itm);
	Halfwords:
	    ChannelWriteHalfWords(Channel, Itm);
	Bytes:
	    ChannelWriteBytes(Channel, Itm);
	PAIR:
	    ChannelWritePair(Channel, Itm, Level);
	VECT:
	    ChannelWriteVector(Channel, Itm, Level);
	EVECT:
	    ChannelWriteEVector(Channel, Itm, Level);
	default: 
	    CheckLineFit(20, Channel, 'ChannelWriteUnknownItem, Itm)
    end;
    Itm >>;

syslsp procedure Prin2 Itm;		%. ChannelPrin2 to current channel
    ChannelPrin2(LispVar OUT!*, Itm);

syslsp procedure ChannelPrin1(Channel, Itm);	%. Display Itm in READable form
    RecursiveChannelPrin1(Channel, Itm, 0);

syslsp procedure RecursiveChannelPrin1(Channel, Itm, Level);
<<  case Tag Itm of
	PosInt, NegInt:
	    CheckLineFit(10, Channel, 'ChannelWriteInteger, Itm);
	ID:				% leave room for possible escape chars
	    CheckLineFit(StrLen StrInf SymNam IDInf Itm + 5,
				Channel, 'ChannelPrintID, Itm);
	UNBOUND:			% leave room for possible escape chars
	    CheckLineFit(StrLen StrInf SymNam IDInf Itm + 16,
				Channel, 'ChannelPrintUnbound, Itm);
	STR:
	    CheckLineFit(StrLen StrInf Itm + 4,
				Channel, 'ChannelPrintString, Itm);
	CODE:
	    CheckLineFit(14, Channel, 'ChannelWriteCodePointer, Itm);
	FIXN:
	    CheckLineFit(20, Channel, 'ChannelWriteFixnum, Itm);
	FLTN:
	    CheckLineFit(20, Channel, 'ChannelWriteFloat, Itm);
	WRDS:
	    ChannelWriteWords(Channel, Itm);
	Halfwords:
	    ChannelWriteHalfWords(Channel, Itm);
	Bytes:
	    ChannelWriteBytes(Channel, Itm);
	PAIR:
	    ChannelPrintPair(Channel, Itm, Level);
	VECT:
	    ChannelPrintVector(Channel, Itm, Level);
	EVECT:
	    ChannelPrintEVector(Channel, Itm, Level);
	default: 
	    CheckLineFit(20, Channel, 'ChannelWriteUnknownItem, Itm)
    end;
    Itm >>;

syslsp procedure Prin1 Itm;		%. ChannelPrin1 to current output
    ChannelPrin1(LispVar OUT!*, Itm);

off SysLisp;

END;
