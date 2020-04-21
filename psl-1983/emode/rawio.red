
% RAWIO.RED - Support routines for PSL Emode
% 
% Author:      Eric Benson
%              Computer Science Dept.
%              University of Utah
% Date:        17 August 1981
% Copyright (c) 1981, 1982 University of Utah
% Modified and maintained by William F. Galway.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEC-20 version

FLUID '(!*rawio);       % T if terminal is using "raw" i.o.

CompileTime <<
load if!-system;
load syslisp$
off UserMode;		% csp 8/20/82

if_system(Dec20,
  <<
    load monsym$
    load jsys$
  >>)
>>;

BothTimes if_system(Dec20,      % CompileTime probably suffices.
<<
FLUID '(       % Global?
    OldCCOCWords 
    OldTIW
    OldJFNModeWord
    );

lisp procedure BITS1 U;
    if not NumberP U then Error(99, "Non-numeric argument to BITS")
    else lsh(1, 35 - U);

macro procedure BITS U;
begin scalar V;
    V := 0;
    for each X in cdr U do V := lor(V, BITS1 X);
    return V;
end;

>>);

LoadTime if_system(Dec20,
<<
OldJfnModeWord := NIL;                  % Flag "modes not saved yet"

lap '((!*entry PBIN expr 0)
% Read a single character from the TTY as a Lisp integer
	(pbin)				% Issue PBIN
        (!*CALL Sys2Int)                % Turn it into a number

	(!*exit 0)
);

lap '((!*entry PBOUT expr 1)
% write a single charcter to the TTY, works for integers and single char IDs
% Don't bother with Int2Sys?
	(pbout)
	(!*exit 0)
);

lap '((!*entry CharsInInputBuffer expr 0)
% Returns the number of characters in the terminal input buffer.
	(!*MOVE (WConst 8#101) (reg 1)) % The input file (the terminal, =
                                        % 8#101)
	(sibe)				% skip if input buffer empty
	(skipa (reg 1) (reg 2))         % otherwise # chars in r2
	(setz (reg 1) 0)			% if skipped, then zero
        (!*CALL Sys2Int)                % Turn it into a number

	(!*exit 0)
);

lap '((!*entry RFMOD expr 1)
% returns the JFN mode word as Lisp integer
	(hrrzs (reg 1))
	(rfmod)
	(!*MOVE  (reg 2) (reg 1)) % Get mode word from R2
	(!*CALL Sys2Int)
        (!*exit 0)
);

lap '((!*entry RFCOC expr 1)
% returns the 2 CCOC words for JFN as dotted pair of Lisp integers
	(hrrzs (reg 1))
	(rfcoc)
	(!*PUSH (reg 2))        % save the first word
	(!*MOVE (reg 3) (reg 1))
	(!*CALL Sys2Int)		% make second into number

        (exch (reg 1) (indexed (reg st) 0))     % grab first word, save
                                                % tagged 2nd word.
	(!*CALL Sys2Int)		% make first into number
	(!*POP (reg 2))
	(!*JCALL  Cons)			% and cons them together
);

lap '((!*entry RTIW expr 1)
% Returns terminal interrupt word for specified process, or -5 for entire job,
% as Lisp integer
	(hrrzs (reg 1))			% strip tag
	(rtiw)
	(!*MOVE (reg 2) (reg 1))        % result in r2, return in r1
	(!*JCALL Sys2Int)		% return as Lisp integer
);

lisp procedure SaveInitialTerminalModes();
% Save the terminal modes, if not already saved.
    if null OldJfnModeWord then
    <<  OldJFNModeWord := RFMOD(8#101);
        OldCCOCWords := RFCOC(8#101);
        OldTIW := RTIW(-5);
    >>;

lap '((!*entry SFMOD expr 2)
% SFMOD(JFN, ModeWord);
% set program related modes for the specified terminal
	(hrrzs (reg 1))
	(!*PUSH (reg 1))
	(!*MOVE (reg 2) (reg 1))
	(!*CALL Int2Sys)
	(!*MOVE (reg 1) (reg 2))
	(!*POP (reg 1))
	(sfmod)
	(!*exit 0)
);

lap '((!*entry STPAR expr 2)
% STPAR(JFN, ModeWord);
% set device related modes for the specified terminal
	(hrrzs (reg 1))
	(!*PUSH (reg 1))
	(!*MOVE (reg 2) (reg 1))
	(!*CALL Int2Sys)
	(!*MOVE (reg 1) (reg 2))
	(!*POP (reg 1))
	(stpar)
	(!*exit 0)
);

lap '((!*entry SFCOC expr 3)
% SFCOC(JFN, CCOCWord1, CCOCWord2);
% set control character output control for the specified terminal
	(hrrzs (reg 1))
	(!*PUSH (reg 1))
	(!*PUSH (reg 3))
	(!*MOVE (reg 2) (reg 1))
	(!*CALL Int2Sys)
        (exch (reg 1) (indexed (reg st) 0))
	(!*CALL Int2Sys)
	(!*MOVE (reg 1) (reg 3))
	(!*POP (reg 2))
	(!*POP (reg 1))
	(sfcoc)
	(!*exit 0)
);

lap '((!*entry STIW expr 2)
% STIW(JFN, ModeWord);
% set terminal interrupt word for the specified terminal
	(hrrzs (reg 1))
	(!*PUSH (reg 1))
	(!*MOVE (reg 2) (reg 1))
	(!*CALL Int2Sys)
	(!*MOVE (reg 1) (reg 2))
	(!*POP (reg 1))
	(stiw)
	(!*exit 0)
);

lisp procedure EchoOff();
% A bit of a misnomer, perhaps "on_rawio" would be better.
% Off echo, On formfeed, send all control characters
% Allow input of 8-bit characters (meta key)
if not !*rawio then     % Avoid doing anything if already "raw mode"
<<
    SaveInitialTerminalModes();

    % Note that 8#101, means "the terminal".
    % Clear bit 24 to turn echo off,
    %       bits 28,29 turn off "translation"
    SFMOD(8#101, LAND(OldJFNModeWord, LNOT BITS(24, 28, 29)));

    % Set bit 0 to indicate "has mechanical tab" (so cntrl-L gets
    % through?).
    % Clear bit 34 to turn off cntrl-S/cntrl-Q
    STPAR(8#101, LAND(lor(OldJFNModeWord, BITS 1), LNOT BITS(34)));

    % More nonsense to turn off processing of control characters?
    SFCOC(8#101,
	  LNOT(8#252525252525),
	  LNOT(8#252525252525));

    % Turn off terminal interrupts for entire job (-5), for everything
    % except cntrl-C (the bit number three that's one).
    STIW(-5,8#040000000000);

    !*rawio := T;   % Turn on flag
>>;

lisp procedure EchoOn();
% Restore initial terminal echoing modes
<<
    % Avoid doing anything if OldJFNModeWord is NIL, means terminal mode
    % already "restored".
    if OldJFNModeWord then
    <<
        SFMOD(8#101,OldJFNModeWord);
        STPAR(8#101,OldJFNModeWord);
        SFCOC(8#101,car OldCCOCWords,cdr OldCCOCWords);
        STIW(-5,OldTIW);
    >>;

    % Set to NIL so that things get saved again by
    % SaveInitialTerminalModes.  (The terminal status may have been changed
    % between times.)
    OldJFNModeWord := NIL;
    !*rawio := NIL; % Indicate "cooked" i/o.
>>;

% Flush output buffer for stdoutput.  (On theory that we're using buffered
% I/O to speed things up.)
Symbolic Procedure FlushStdOutputBuffer();
NIL;    % Just a dummy routine for the 20.
>>
);
% END OF DEC-20 version.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% VAX Unix version

LoadTime if_system(Unix,
<<
% EchoOn, EchoOff, and CharsInInputBuffer are part of "kernel".

Symbolic Procedure PBIN();
% Read a "raw character".  NOTE--assumption that 0 gives terminal input.
    VaxReadChar(0);   % Just call this with "raw mode" on.

Symbolic Procedure PBOUT(chr);
% NOTE ASSUMPTION that 1 gives terminal output.
    VaxWriteChar(1,chr);

>>);
% END OF Unix version.

fluid '(!*EMODE);

LoadTime
<<
!*EMODE := NIL;

Symbolic Procedure rawio_break();
% Redefined break handler to turn echoes back on after a break, unless
% EMODE is running.
<<
    if !*rawio and not !*EMODE then
        EchoOn();

    pre_rawio_break();  % May want to be paranoid and use a "catch(nil,
                        % '(pre_rawio_break)" here.
>>;

% Carefully redefine the break handler.
if null getd('pre_rawio_break) then
<<
CopyD('pre_rawio_break, 'Break);
CopyD('break, 'rawio_break);
>>;

>>;

