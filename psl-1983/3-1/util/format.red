%
% Format.RED - Formatted print routine
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        27 August 1981
% Copyright (c) 1981 University of Utah
%


CompileTime <<

load(Syslisp, Fast!-Vector);

flag('(format!-freshline format1 format2 clear!-string!-write
	return!-string!-write), 'internalfunction);

fluid '(FormatForFormat!* string!-write!-channel next!-string!-write!-char
        string!-write!-buffer);

>>;

% First, lambda-bind FormatForFormat!*

lisp procedure Format(Stream, FormatForFormat!*, A1, A2, A3, A4, A5,
					 A6, A7, A8, A9, A10,
					 A11, A12, A13);
 Format1(Stream, FormatForFormat!*, A1, A2, A3, A4, A5,
			    A6, A7, A8, A9, A10,
			    A11, A12, A13);


% Then, push all the registers on the stack and set up a pointer to them

lap '((!*entry Format1 expr 15)
	(!*PUSH (reg 3))
	(!*LOC (reg 2) (frame 1))
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
	(!*CALL Format2)
	(!*EXIT 14)
);

on SysLisp;

% Finally, actual Format, with 2 arguments, stream and
% pointer to array of parameters

syslsp procedure Format2(Stream, FormatArgs); %. Formatted print
%
% If the character is not one of these (either upper or lower case), then an
% error occurs.
%
begin scalar UpLim, I, Ch, UpCh;
    if Stream eq NIL then
    <<  Stream := lispvar string!-write!-channel;
	clear!-string!-write() >>
    else if Stream eq T then
	Stream := LispVar OUT!*;
    UpLim := StrLen StrInf LispVar FormatForFormat!*;
    I := 0;
    while I <= UpLim do
    <<  Ch := StrByt(StrInf LispVar FormatForFormat!*, I);
	if Ch neq char !~ then 
	    ChannelWriteChar(Stream, Ch)
	else
	begin
	    I := I + 1;
	    Ch := StrByt(StrInf LispVar FormatForFormat!*, I);
	    UpCh := if Ch >= char lower A and Ch <= char lower Z
			then IPlus2(IDifference(Ch, char lower A), char A)
			else Ch;
	    case UpCh of
	    char A:
	    <<  ChannelPrin2(Stream, FormatArgs[0]);
		FormatArgs := &FormatArgs[StackDirection]  >>;
	    char S:
	    <<  ChannelPrin1(Stream, FormatArgs[0]);
		FormatArgs := &FormatArgs[StackDirection]  >>;
	    char D:
	    <<  ChannelWriteSysInteger(Stream,
				       Int2Sys FormatArgs[0],
				       10);
		FormatArgs := &FormatArgs[StackDirection]  >>;
	    char B:
	    <<  ChannelWriteSysInteger(Stream,
				       Int2Sys FormatArgs[0],
				       2);
		FormatArgs := &FormatArgs[StackDirection]  >>;
	    char O:
	    <<  ChannelWriteSysInteger(Stream,
				       Int2Sys FormatArgs[0],
				       8);
		FormatArgs := &FormatArgs[StackDirection]  >>;
	    char X:
	    <<  ChannelWriteSysInteger(Stream,
				       Int2Sys FormatArgs[0],
				       16);
		FormatArgs := &FormatArgs[StackDirection]  >>;
	    char !~:
		ChannelWriteChar(Stream, char !~);
	    char !%:
		ChannelWriteChar(Stream, char EOL);
	    char '!&:
	        format!-freshline Stream;
	    default:
		StdError BldMsg('"Unknown character code for Format: %r",
								  MkID Ch);
	    end;
	end;
    I := I + 1 >>;
    if Stream eq LispVar string!-write!-channel then return
	return!-string!-write();
end;

off SysLisp;

lisp procedure format!-freshline Stream;
(lambda out!*;
    if IGreaterP(Posn(), 0) then
	ChannelWriteChar(Stream, char EOL))(Stream);


lisp procedure Ferror(Condition, FMT, A1, A2, A3, A4, A5, A6,
					 A7, A8, A9, A10, A11, A12, A13);
    Error(Condition, Format(NIL, FMT, A1, A2, A3, A4, A5, A6,
					 A7, A8, A9, A10, A11, A12, A13));

lisp procedure string!-write!-char(stream, ch);
    if IGEQ(next!-string!-write!-char, 5000) then
	StdError "String overflow in FORMAT"
    else
    <<  next!-string!-write!-char := iadd1 next!-string!-write!-char;
	iputs(string!-write!-buffer, next!-string!-write!-char, ch) >>;

lisp procedure clear!-string!-write();
<<  channelwritechar(string!-write!-channel, char EOL);
    next!-string!-write!-char := -1 >>;

lisp procedure return!-string!-write();
begin scalar x, y;
    y := 0;
    next!-string!-write!-char := iadd1 next!-string!-write!-char;
    x := make!-string(next!-string!-write!-char, char NULL);
    while ILEQ(y, next!-string!-write!-char) do
    <<  iputs(x, y, igets(string!-write!-buffer, y));
	y := iadd1 y >>;
    return x;
end;

string!-write!-buffer := make!-string(5000, char NULL);
specialreadfunction!* := 'WriteOnlyChannel;
specialwritefunction!* := 'string!-write!-char;
specialclosefunction!* := 'IllegalStandardChannelClose;
string!-write!-channel := open("", 'special);
(lambda (x);
<<  LineLength 10000;
    WRS x >> )(WRS string!-write!-channel);

END;
