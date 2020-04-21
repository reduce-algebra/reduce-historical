%
% PRINTF.RED - Formatted print routine
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        27 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL>PRINTF.RED.2, 17-Sep-82 16:01:01, Edit by BENSON
%  Added ChannelPrintF
%  <PSL.INTERP>PRINTF.RED.6,  3-May-82 10:45:11, Edit by BENSON
%  %L prints nothing for NIL
%  <PSL.INTERP>PRINTF.RED.9, 23-Feb-82 21:40:31, Edit by BENSON
%  Added %x for hex
%  <PSL.INTERP>PRINTF.RED.7,  1-Dec-81 16:11:11, Edit by BENSON
%  Changed to cause error for unknown character

CompileTime flag('(PrintF1 PrintF2), 'InternalFunction);

fluid '(FormatForPrintF!*);

% First, lambda-bind FormatForPrintF!*

lisp procedure PrintF(FormatForPrintF!*, A1, A2, A3, A4, A5,
					 A6, A7, A8, A9, A10,
					 A11, A12, A13, A14);
 PrintF1(FormatForPrintF!*, A1, A2, A3, A4, A5,
			    A6, A7, A8, A9, A10,
			    A11, A12, A13, A14);


% Then, push all the registers on the stack and set up a pointer to them

lap '((!*entry PrintF1 expr 15)
	(!*PUSH (reg 2))
	(!*LOC (reg 1) (frame 1))
	(!*PUSH (reg 3))
	(!*PUSH (reg 4))
	(!*PUSH (reg 5))
	(!*PUSH (reg 6))
	(!*PUSH (reg 7))
	(!*PUSH (reg 8))
	(!*PUSH (reg 9))
	(!*PUSH (reg 10))
	(!*PUSH (reg 11))
	(!*PUSH (reg 12))
	(!*PUSH (reg 13))
	(!*PUSH (reg 14))
	(!*PUSH (reg 15))
	(!*CALL PrintF2)
	(!*EXIT 14)
);

on SysLisp;

% Finally, actual printf, with 1 argument, pointer to array of parameters

syslsp procedure PrintF2 PrintFArgs; %. Formatted print
%
% Format is a string, either in the heap or not, whose characters will be
% written on the currently selected output channel.  The exception to this is
% that when a % is encountered, the following character is interpreted as a
% format character, to decide how to print one of the other arguments.  The
% following format characters are currently supported:
%	%b - blanks; take the next argument as integer and print that many
%		blanks
%	%c - print the next argument as a single character
%	%d - print the next argument as a decimal integer
%       %e - EVALs the next argument for side-effect -- most useful if the
%            thing EVALed does some printing
%	%f - fresh-line, print end-of-line char if not at beginning of line
%	%l - same as %w, except lists are printed without top level parens
%	%n - print end-of-line character
%	%o - print the next argument as an octal integer
%	%p - print the next argument as a Lisp item, using Prin1
%       %r - print the next argument as a Lisp item, using ErrPrin (`FOO')
%	%s - print the next argument as a string
%	%t - tab; take the next argument as an integer and
%		print spaces to that column
%	%w - print the next argument as a Lisp item, using Prin2
%	%x - print the next argument as a hexidecimal integer
%	%% - print a %
%
% If the character is not one of these (either upper or lower case), then an
% error occurs.
%
begin scalar UpLim, I, Ch, UpCh;
    UpLim := StrLen StrInf LispVar FormatForPrintF!*;
    I := 0;
    while I <= UpLim do
    <<  Ch := StrByt(StrInf LispVar FormatForPrintF!*, I);
	if Ch neq char !% then 
	    WriteChar Ch
	else
	begin
	    I := I + 1;
	    Ch := StrByt(StrInf LispVar FormatForPrintF!*, I);
	    UpCh := if LowerCaseChar Ch then RaiseChar Ch else Ch;
	    case UpCh of
	    char B:
	    <<  Spaces @PrintFArgs;
		PrintFArgs := &PrintFArgs[StackDirection]  >>;
	    char C:
	    <<  WriteChar @PrintFArgs;
		PrintFArgs := &PrintFArgs[StackDirection]  >>;
	    char D:
	    <<  WriteSysInteger(@PrintFArgs, 10);
		PrintFArgs := &PrintFArgs[StackDirection]  >>;
	    char E:
	    <<  Eval @PrintFArgs;
	        PrintFArgs := &PrintFArgs[StackDirection]  >>;
	    char F:
		if Posn() > 0 then WriteChar char EOL;
	    char L:
	    <<  Prin2L @PrintFArgs;
		PrintFArgs := &PrintFArgs[StackDirection]  >>;
	    char N:
		WriteChar char EOL;
	    char O:
	    <<  WriteSysInteger(@PrintFArgs, 8);
		PrintFArgs := &PrintFArgs[StackDirection]  >>;
	    char X:
	    <<  WriteSysInteger(@PrintFArgs, 16);
		PrintFArgs := &PrintFArgs[StackDirection]  >>;
	    char P:
	    <<  Prin1 @PrintFArgs;
		PrintFArgs := &PrintFArgs[StackDirection]  >>;
	    char R:
	    <<  ErrPrin @PrintFArgs;
	        PrintFArgs := &PrintFArgs[StackDirection]  >>;
	    char S:
	    <<  WriteString @PrintFArgs;
		PrintFArgs := &PrintFArgs[StackDirection]  >>;
	    char T:
	    <<  Tab @PrintFArgs;
		PrintFArgs := &PrintFArgs[StackDirection]  >>;
	    char W:
	    <<  Prin2 @PrintFArgs;
		PrintFArgs := &PrintFArgs[StackDirection]  >>;
	    char !%:
		WriteChar char !%;
	    default:
		StdError BldMsg('"Unknown character code for PrintF: %r",
								  MkID Ch);
	    end;
	end;
    I := I + 1 >>;
end;

syslsp procedure ErrorPrintF(Format, A1, A2, A3, A4);	% also A5..A14
begin scalar SaveChannel;
    SaveChannel := WRS LispVar ErrOut!*;
    if LinePosition[IntInf LispVar ErrOut!*] > 0 then TerPri();
    PrintF(Format, A1, A2, A3, A4);
    if LinePosition[IntInf LispVar ErrOut!*] > 0 then TerPri();
    WRS SaveChannel;
end;

syslsp procedure ToStringWriteChar(Channel, Ch); % shares TokenBuffer
<<  if TokenBuffer[0] >= MaxTokenSize - 1 then
    <<  TokenBuffer[0] := 80;		% truncate to 80 chars
	StrByt(TokenBuffer, 80) := char NULL;
	StdError list('"Buffer overflow while constructing error message:",
			LispVar FormatForPrintF!*,
			'"The truncated result was:",
			CopyString MkSTR TokenBuffer) >>
    else
    <<  TokenBuffer[0] := TokenBuffer[0] + 1;
	StrByt(TokenBuffer, TokenBuffer[0]) := Ch >> >>;

syslsp procedure BldMsg(Format, Args1, Args2, Args3, Args4); %. Print to string
begin scalar TempChannel;		% takes up to 14 args
    LinePosition[2] := 0;
    TokenBuffer[0] := -1;
    TempChannel := LispVar OUT!*;
    LispVar OUT!* := '2;
    PrintF(Format, Args1, Args2, Args3, Args4);
    StrByt(TokenBuffer, TokenBuffer[0] + 1) := char NULL;
    LispVar OUT!* := TempChannel;
    return CopyString TokenBuffer;
end;

syslsp procedure ErrPrin U;		%. `Prin1 with quotes'
<<  WriteChar char !`;
    Prin1 U;
    WriteChar char !' >>;

off SysLisp;

lisp procedure Prin2L Itm;		%. Prin2 without top-level parens
    if null Itm then NIL		% NIL is (), print nothing
    else if not PairP Itm then Prin2 Itm
    else
    <<  while << Prin2 car Itm;
		 Itm := cdr Itm;
		 PairP Itm >> do
	    ChannelWriteBlankOrEOL OUT!*;
	if Itm then
	<<  ChannelWriteBlankOrEOL OUT!*;
	    Prin2 Itm >> >>;

syslsp procedure ChannelPrintF(OUT!*, Format, A1, A2, A3, A4, A5, A6, A7, A8,
					    A9, A10, A11, A12, A13);
    PrintF(Format, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13);


END;
