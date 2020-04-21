%
% SYSTEM-IO.RED - System dependent IO routines for Dec-20 PSL
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        16 September 1981
% Copyright (c) 1981 University of Utah
%

global '(IN!* OUT!*);
LoadTime <<
IN!* := 0;
OUT!* := 1;
>>;

fluid '(StdIN!* StdOUT!* ErrOUT!* !*Echo);
LoadTime <<
StdIN!* := 0;
StdOUT!* := 1;
ErrOUT!* := 1;
>>;

CompileTime flag('(RDTTY FindFreeChannel Dec20Open ContOpenError ClearIO1),
		 'InternalFunction);

on SysLisp;

external WArray JFNOfChannel, ReadFunction, WriteFunction, CLoseFunction;

if_system(Tops20,
lap '((!*entry Dec20ReadChar expr 1)
	(!*MOVE (MEMORY (reg 1) (WConst JFNOfChannel)) (reg 1))
Loop					% get JFN for channel
	(bin)				% read a character
	(erjmp CheckEOF)		% check for end-of-file on error
	(!*JUMPEQ (Label Loop) (reg 2) (WConst 0))% try again if it's null char
	(!*JUMPEQ (Label Loop) (reg 2) (WConst 8#15))% or carriage return
	(!*MOVE (reg 2) (reg 1))	% move char to reg 1
	(camn (reg nil) (fluid !*ECHO))	% is echo on?
	(!*EXIT 0)			% no, just return char
	(!*PUSH (reg 1))		% yes, save char
	(!*CALL WriteChar)		% and write it
	(!*POP (reg 1))			% restore it
	(!*EXIT 0)			% and return
CheckEOF
	(gtsts)				% check file status
	(tlnn (reg 2) 2#000000001000000000)	% gs%eof
	(!*JUMP (Label ReadError))
	(!*MOVE (WConst 26) (reg 1))	% return EOF char
	(!*EXIT 0)
ReadError
	(!*MOVE (QUOTE "Attempt to read from file failed") (reg 1))
	(!*JCALL IoError)
));

if_system(Tenex,
lap '((!*entry Dec20ReadChar expr 1)
	(!*MOVE (MEMORY (reg 1) (WConst JFNOfChannel)) (reg 1))
Loop					% get JFN for channel
	(bin)				% read a character
	(erjmp CheckEOF)		% check for end-of-file on error
	(!*JUMPEQ (Label Loop) (reg 2) (WConst 0))% try again if it's null char
	(!*JUMPEQ (Label Loop) (reg 2) (WConst 8#15))% or carriage return
	(cain (reg 2) (WConst 8#37))	% TENEX EOL
	(!*MOVE (WConst 8#12) (reg 2))	% replace it with a linefeed
	(!*MOVE (reg 2) (reg 1))	% move char to reg 1
	(camn (reg nil) (fluid !*ECHO))	% is echo on?
	(!*EXIT 0)			% no, just return char
	(!*PUSH (reg 1))		% yes, save char
	(!*CALL WriteChar)		% and write it
	(!*POP (reg 1))			% restore it
	(!*EXIT 0)			% and return
CheckEOF
	(gtsts)				% check file status
	(tlnn (reg 2) 2#000000001000000000)	% gs%eof
	(!*JUMP (Label ReadError))
	(!*MOVE (WConst 26) (reg 1))	% return EOF char
	(!*EXIT 0)
ReadError
	(!*MOVE (QUOTE "Attempt to read from file failed") (reg 1))
	(!*JCALL IoError)
));

lap '((!*entry Dec20WriteChar expr 2)
	(!*MOVE (MEMORY (reg 1) (WConst JFNOfChannel)) (reg 1))
					% get JFN for channel
	(!*JUMPEQ (Label CRLF) (reg 2) (WConst 8#12))	% if LF, echo CRLF
	(bout)				% no, just echo char
	(!*EXIT 0)			% return
CRLF
	(!*MOVE (WConst 8#15) (reg 2))	% write carriage-return
	(bout)
	(!*MOVE (WConst 8#12) (reg 2))	% write linefeed
	(bout)
	(!*EXIT 0)			% return
);

internal WConst MaxTerminalBuffer = 200;
internal WVar NextTerminalChar = 1;
internal WString TerminalInputBuffer[MaxTerminalBuffer];

lap '((!*entry ClearIO1 expr 0)
%
% ^C from RDTTY and restart causes trouble, but we don't want a full RESET
% (don't want to close files or kill forks), so we'll just do the
% part of RESET that we want, for terminal input
%
	(!*MOVE (WConst 8#100) (reg 1))	% .priin
	(rfmod)
	(tro 2 2#001111100001000000)	% tt%wak + tt%eco + .ttasi, like RESET
	(sfmod)
	(!*EXIT 0)
);

syslsp procedure ClearIO();
<<  ClearIO1();
    TerminalInputBuffer[0] := -1;
    NextTerminalChar := 0;
    LispVar IN!* := LispVar STDIN!*;
    LispVar OUT!* := LispVar STDOUT!* >>;

if_system(Tops20,
lap '((!*entry RDTTY expr 3)
	(dmove (reg t1) (reg 1))
	(!*MOVE (WConst 8#101) (reg 1))	% .priou
	(rfmod)				% read mode word
	(tlze (reg 2) 2#100000000000000000)	% if tt%osp is 0, then skip
	(sfmod)				% otherwise turn on output
	(dmove (reg 1) (reg t1))
	(!*MOVE (reg 2) (reg 4))	% save original count in r4
	(!*WPLUS2 (reg 1) (WConst 1))	% make input buffer into byte pointer
	(hrli (reg 1) 8#440700)
	(!*WPLUS2 (reg 3) (WConst 1))	% make prompt string into byte pointer
	(hrli (reg 3) 8#440700)
	(!*MOVE (reg 1) (reg 5))	% print it once
	(!*MOVE (reg 3) (reg 1))
	(psout)
	(!*MOVE (reg 5) (reg 1))
	(hrli (reg 2) 2#000110000000000000)	% rd%bel + rd%crf
	(jsys 8#523)			% RDTTY
	(!*JUMP (Label CantRDTTY))
	(!*MOVE (reg 4) (reg 1))	% move original count to r1
	(hrrzs (reg 2))			% clear flag bits in r2
	(!*WDIFFERENCE (reg 1) (reg 2))	% return # chars read, not # available
	(!*EXIT 0)
CantRDTTY
	(!*MOVE (QUOTE "Can't read from terminal") (reg 1))
	(!*JCALL IOError)
));

if_system(Tenex,
lap '((!*entry RDTTY expr 3)
	(move (reg t1) (reg 1))
	(move (reg t2) (reg 2))
	(!*MOVE (WConst 8#101) (reg 1))	% .priou
	(rfmod)				% read mode word
	(tlze (reg 2) 2#100000000000000000)	% if tt%osp is 0, then skip
	(sfmod)				% otherwise turn on output
	(move (reg 1) (reg t1))
	(move (reg 2) (reg t2))
	(!*MOVE (reg 2) (reg 4))	% save original count in r4
	(!*WPLUS2 (reg 1) (WConst 1))	% make input buffer into byte pointer
	(hrli (reg 1) 8#440700)
	(!*WPLUS2 (reg 3) (WConst 1))	% make prompt string into byte pointer
	(hrli (reg 3) 8#440700)
	(!*MOVE (reg 1) (reg 5))	% print it once
	(!*MOVE (reg 3) (reg 1))
	(psout)
	(!*MOVE (reg 5) (reg 1))
%	(hrli (reg 2) 2#000110000000000000)	% rd%bel + rd%crf
%	(jsys 8#523)			% RDTTY
%	(!*JUMP (Label CantRDTTY))
	(!*MOVE (WConst MaxTerminalBuffer) (reg 2))	% # of chars
	(setz 3 0)			% clear 3
	(jsys 8#611)			% PSTIN, IMSSS JSYS
	(!*MOVE (WConst 8#12) (reg 3))	% put linefeed at end of buffer
	(dpb (reg 3) (reg 1))		% 1 points to end of what's been read
	(!*MOVE (reg 4) (reg 1))	% move original count to r1
	(hrrzs (reg 2))			% clear flag bits in r2
	(!*WDIFFERENCE (reg 1) (reg 2))	% return # chars read, not # available
	(!*EXIT 0)
));

syslsp procedure TerminalInputHandler Chn;
begin scalar Ch;
    while NextTerminalChar >= StrLen TerminalInputBuffer do
    <<  NextTerminalChar := 0;
	TerminalInputBuffer[0] := RDTTY(TerminalInputBuffer,
					    MaxTerminalBuffer,
					    if StringP LispVar PromptString!*
						then LispVar PromptString!*
						else ">") >>;
    Ch := StrByt(TerminalInputBuffer, NextTerminalChar);
    NextTerminalChar := NextTerminalChar + 1;
    return Ch;
end;

syslsp procedure FindFreeChannel();
begin scalar Chn;
    Chn := 0;
    while JfnOfChannel[Chn] neq 0 do
    <<  if Chn >= MaxChannels then IOError("No free channels left");
	Chn := Chn + 1 >>;
    return Chn;
end;

syslsp procedure SystemMarkAsClosedChannel FileDes;
    JFNOfChannel[IntInf FileDes] := 0;

lap '((!*entry Dec20CloseChannel expr 1)
	(!*MOVE (reg 1) (reg 2))	% save in case of error
	(!*MOVE (MEMORY (reg 1) (WConst JFNOfChannel)) (reg 1))
	(closf)
	(!*JUMP (Label CloseError))
	(!*EXIT 0)
CloseError
	(!*MOVE (QUOTE "Channel could not be closed") (reg 1))
	(!*JCALL ChannelError)
);

syslsp procedure SystemOpenFileSpecial FileName;
<<  JFNOfChannel[FileName := FindFreeChannel()] := -1;
    FileName >>;

syslsp procedure SystemOpenFileForInput FileName;
begin scalar Chn, JFN;
    Chn := FindFreeChannel();
    JFN := Dec20Open(FileName,
		     %  gj%old	    gj%sht
		     2#001000000000000001000000000000000000,
		     % 7*of%bsz		of%rd
		     2#000111000000000000010000000000000000);
    if JFN eq 0 then return ContOpenError(FileName, 'INPUT);
    JFNOfChannel[Chn] := JFN;
    ReadFunction[Chn] := 'Dec20ReadChar;
    CloseFunction[Chn] := 'Dec20CloseChannel;
    return Chn;
end;

syslsp procedure SystemOpenFileForOutput FileName;
begin scalar Chn, JFN;
    Chn := FindFreeChannel();
    JFN := Dec20Open(FileName,
		    % gj%fou gj%new gj%sht
		    2#110000000000000001000000000000000000,
		    % 7*of%bsz		of%wr
		    2#000111000000000000001000000000000000);
    if JFN eq 0 then return ContOpenError(FileName, 'OUTPUT);
    JFNOfChannel[Chn] := JFN;
    WriteFunction[Chn] := 'Dec20WriteChar;
    CloseFunction[Chn] := 'Dec20CloseChannel;
    return Chn;
end;

lap '((!*entry Dec20Open expr 3)
%
%	Dec20Open(Filename string, GTJFN bits, OPENF bits)
%
	(!*WPLUS2 (reg 1) (WConst 1))	% increment r1 to point to characters
	(hrli (reg 1) 8#440700)		% turn r1 into a byte pointer
	(!*MOVE (reg 1) (reg 4))	% save filename string in r4
	(!*MOVE (reg 2) (reg 1))	% GTJFN flag bits in r1
	(!*MOVE (reg 4) (reg 2))	% string in r2
	(gtjfn)
	(!*JUMP (Label CantOpen))
	(!*MOVE (reg 3) (reg 2))	% OPENF bits in r2, JFN in r1
	(openf)
CantOpen
	(!*MOVE (WConst 0) (reg 1))	% return 0 on error
	(!*EXIT 0)			% else return the JFN
);

off SysLisp;

lisp procedure ContOpenError(FileName, AccessMode);
    ContinuableError(99,
		     BldMsg("`%s' cannot be open for %w",
			  FileName,		AccessMode),
		     list('OPEN, MkSTR FileName, MkQuote AccessMode));

END;
