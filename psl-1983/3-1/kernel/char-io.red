%
% CHAR-IO.RED - Bottom level character IO primitives
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        27 August 1981
% Copyright (c) 1981 University of Utah
%

% Edit by Cris Perdue, 27 Jan 1983 1652-PST
% ChannelReadChar and ChannelWriteChar now check the FileDes argument
%  <PERDUE.PSL>CHAR-IO.RED.2, 29-Dec-82 12:21:51, Edit by PERDUE
%  Added code to ChannelWriteChar to maintain PagePosition for LPOSN

global '(IN!*				% The current input channel
	 OUT!*);			% The current output channel

on SysLisp;

external WArray ReadFunction,		% Indexed by channel # to read char
		WriteFunction,		% Indexed by channel # to write char
		UnReadBuffer,		% For input backup
		LinePosition,		% For Posn()
		PagePosition;		% For LPosn()

syslsp procedure ChannelReadChar FileDes;	%. Read one char from channel
%
% All channel input must pass through this function.  When a channel is
% open, its read function must be set up.
%
begin scalar Ch, FD;
    FD := IntInf FileDes;	%/ Heuristic: don't do Int type test
    if not (0 <= FD and FD <= MaxChannels) then
        NonIOChannelError(FileDes, "ChannelReadChar");
    return if (Ch := UnReadBuffer[FD]) neq char NULL then
    <<  UnReadBuffer[FD] := char NULL;
	Ch >>
    else
	IDApply1(FD, ReadFunction[FD]);
end;

syslsp procedure ReadChar();		%. Read single char from current input
    ChannelReadChar LispVar IN!*;

syslsp procedure ChannelWriteChar(FileDes, Ch);	%. Write one char to channel
%
% All channel output must pass through this function.  When a channel is
% open, its write function must be set up, and line position set to zero.
%
begin scalar FD;
    FD := IntInf FileDes;
    if not (0 <= FD and FD <= MaxChannels) then
	NonIOChannelError(FileDes, "ChannelWriteChar");
    if Ch eq char EOL then
	<< LinePosition[FD] := 0;
	   PagePosition[FD] := PagePosition[FD] + 1 >>
    else if Ch eq char TAB then	 % LPos := (LPos + 8) - ((LPos + 8) MOD 8)
	LinePosition[FD] := LAND(LinePosition[FD] + 8, LNOT 7)
    else if Ch eq char FF then
	<< PagePosition[FD] := 0;
	   LinePosition[FD] := 0 >>
    else
	LinePosition[FD] := LinePosition[FD] + 1;
    IDApply2(FD, Ch, WriteFunction[FD]);
end;

syslsp procedure WriteChar Ch;		%. Write single char to current output
    ChannelWriteChar(LispVar OUT!*, Ch);

syslsp procedure ChannelUnReadChar(Channel, Ch);    %. Input backup function
%
% Any channel input backup must pass through this function.  The following
% restrictions are made on input backup:
%     1. Backing up without first doing input should cause an error, but
%	 will probably cause unpredictable results.
%     2. Only one character backup is supported.
%
    UnReadBuffer[IntInf Channel] := Ch;

syslsp procedure UnReadChar Ch;		%. Backup on current input channel
    ChannelUnReadChar(LispVar IN!*, Ch);

off SysLisp;

END;
