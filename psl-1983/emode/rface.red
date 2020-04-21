%
% RFACE.RED - Code to support execution of text from within EMODE.
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        8 June 1982
% Copyright (c) 1982 University of Utah
%

FirstCall := T; % Force full init when calling EMODE for first time.

DefConst(MaxChannels, 32);      % Maximum number of channels supported by
                                % PSL.

DefConst(DISPLAYTIME, 1000);    % Number of milliseconds between redisplays
                                % (very roughly--see code)

% Vector of "edit routines" associated with channels.
ChannelEditRoutine := MkVect(const(MaxChannels));

% Vectors of buffers associated with channel (when appropriate).  Each
% entry in the vector is an expression to be evaluated (to allow extra
% indirection).
InputBufferForChannel := MkVect(const(MaxChannels));
OutputBufferForChannel := MkVect(const(MaxChannels));

% A window to "pop up" when the associated buffer is written into.  This
% probably should NOT be associated with a channel?
% UNIMPLEMENTED FOR NOW. Needs MORE THOUGHT!
% OutputWindowForChannel := MkVect(const(MaxChannels));

% See below for definition of RlispDispatchList and LispDispatchList.
RlispMode := '(SetKeys RlispDispatchList) . FundamentalTextMode;

LispMode := '(SetKeys LispDispatchList) . FundamentalTextMode;


% Routines for channel I/O to & from buffers

FLUID '(
    TimeSinceRedisplay  % Used to decide if time to redisplay or not

    % A flag for Rlisp's ON/OFF mechanism.  When T, means that the "output"
    % (or OUT_WINDOW) window should be "popped up" when output
    % occurs.
    !*outwindow

    % Holds the buffername that was selected before BufferPrintChar
    % switches to the output buffer.
    previous_to_ouput_buffer

    % Kludge flag, T when input buffer is OUT_WINDOW buffer (for M-E).
    reading_from_output

    EmodeBufferChannel  % Channel used for EMODE I/O.  Perhaps this should
                        % be expanded to allow different channels for
                        % different purposes (break loops, error messages,
                        % etc.)  (Or, perhaps the whole model needs more
                        % thought! )
);

!*outwindow := T;

Symbolic Procedure OpenBufferChannel(Inbuffer, Outbuffer, Outwindow);
% Open channel for buffer I/O.  Outwindow currently unused.
begin Scalar chn;
    SpecialWriteFunction!* := 'BufferPrintChar;
    SpecialReadFunction!* := 'BufferReadChar;
    SpecialCloseFunction!* := 'CloseBufferChannel;

    TimeSinceRedisplay := time();       % Get time from system

    chn := Open("buffers", 'SPECIAL);

    % Set up "editor" for the channel.
    ChannelEditRoutine[chn] := 'EmodeChannelEdit;
    InputBufferForChannel[chn] := Inbuffer;

    OutputBufferForChannel[chn] := Outbuffer;
    return chn
end;

Symbolic Procedure CloseBufferChannel(chn);
% Close up an EMODE buffer channel.
<<
    chn := Sys2Int chn;         % Sys2Int should be temporary fix?
    ChannelEditRoutine[chn] := NIL;

    InputBufferForChannel[chn] := NIL;
    OutputBufferForChannel[chn] := NIL;
>>;

% Some history keeping stuff for debugging, we (sometimes) keep a circular
% list of characters sent to BufferPrintChar in order to hunt down obscure
% bugs.
FLUID '(BPhist  BPindx);
BPhist := MkString(75, char BLANK);
BPindx := 0;

Symbolic Procedure BufferPrintChar(Chn,ch);
% "Print" a character into the buffer corresponding to channel "Chn".
% Perhaps a future version should "pop up" an associated window (or select
% a "window configuration"?), if any, (and if some flag is set?) CLEARLY,
% this needs more thought!
begin scalar tmp, outbuffername,
        ErrOut!*;       % ErrOut!* is a system FLUID

    % Keep a history of the characters, in the circular history buffer, for
    % debugging.
    % (Not needed right now.)
%    BPhist[BPindx] := ch;
%    BPindx := if BPindx >= size(BPhist) then 0 else 1 + BPindx;

    % Rebind to avoid calling self if there is an ERROR in this routine (?)
    ErrOut!* := OldErrOut;

    % HUM, select the appropriate buffer.
    if not(CurrentBufferName
            eq (outbuffername := eval OutputBufferForChannel[chn]))
    then
    <<
        previous_to_ouput_buffer := CurrentBufferName;
        SelectBuffer(outbuffername);
    >>;

    InsertCharacter(ch);

    % Refresh after every character might be nice, but it's costly!  The
    % compromise is to refresh on every line--or after a time limit is
    % exceeded, whichever comes first.

    if ch = char EOL
    then 
    <<
        % Make sure we're in two window mode, unless also reading from
        % OUT_WINDOW, so the user can see what we print into the buffer.
        % Don't pop up window if !*Outwindow is NIL.
        % NEEDS more thought.
        if !*outwindow and not(reading_from_output) then
            EnsureOutputVisible(outbuffername, previous_to_ouput_buffer);

        Refresh();
    >>
    else if ((tmp := time()) - TimeSinceRedisplay) > const(DISPLAYTIME) then
    <<
        TimeSinceRedisplay := tmp;
        if !*outwindow and not(reading_from_output) then
            EnsureOutputVisible(outbuffername, previous_to_ouput_buffer);

        Refresh();
    >>;
end;

% Ensure the visibility of the outbuffername buffer, oldbuffername gives
% the "context" that the call occurs from.
Symbolic Procedure EnsureOutputVisible(outbuffername,oldbuffername);
    % Don't do anything if the buffer is already visible.
    % Otherwise go through a rather elaborate kludge.
    if not Buffer_VisibleP(outbuffername) then
    <<
      SelectBuffer(oldbuffername);

      % Go to "two window" mode if just one "major window" on screen, and
      % it's a "text window".
      if MajorWindowCount() eq 1
         AND buffers_view_creator eq 'create_text_view
     then
          TwoRFACEWindows()
      else
      % Otherwise, just "create a view" into the OUT_WINDOW buffer.
          select_or_create_buffer('OUT_WINDOW,NIL);

      SelectBuffer(outbuffername);
    >>;

Symbolic Procedure BufferReadChar(Chn);
% Read a character from at location "point" in appropriate buffer for
% channel "Chn", advance point.
begin scalar ch;
    chn := Sys2Int chn;         % Sys2Int should be temporary fix?

%???    if not(CurrentWindowDescriptor eq InputWindowForChannel[chn]) then

    SelectBuffer(eval InputBufferForChannel[chn]);

    % (End of buffer test needs to be cleaned up.)
    if point = length CurrentLine
        and EndOfBufferP(NextIndex CurrentLineIndex)
    then
        return char EOF;        % "End Of File" if at end of buffer

% ****OR, should we do something like this?  (Not very popular when
% tried--end of buffer was typically due to a syntax error, often very hard
% to know how to correct the problem.)

%        % Prompt user for more input if at end of buffer, then continue as
%        % usual.
%    <<
%        EmodeChannelEdit(chn, "END OF BUFFER:  more input expected.");
%
%        % Ultimate kludge! Get back to current buffer.  (Seem to be
%        % mysterious problems with "CurrentLine" inconsistencies.)
%%        if not(CurrentWindowDescriptor eq InputWindowForChannel[chn]) then
%
%        SelectBuffer(eval InputBufferForChannel[chn]);
%    >>;

    ch := CurrentCharacter();   % Get the character

    if !*ECHO then       % Echo to OUT_WINDOW if ECHO flag is set.
    <<
        BufferPrintChar(Int2Sys Chn, Int2Sys ch);        % NOTE Int2Sys
        % Super kludge! Get back to current window
%???        if not(CurrentWindowDescriptor eq InputWindowForChannel[chn]) then
        SelectBuffer(eval InputBufferForChannel[chn]);
    >>;

    !$ForwardCharacter();       % Advance to next in buffer
    return Int2Sys(ch);         % Convert to SYSLISP integer
end;

Two_window_midpoint := NIL;

Symbolic Procedure TwoRFACEWindows();
% Enter two window mode for RLISP interface.  Puts prompt information just
% below the upper window.  ("Prompt" means "message window"--not EMODE's
% prompt window.)
    if MajorWindowCount() neq 2 then
    % Only do something if not already in "two window mode".
    begin scalar old_prompt, old_msg, TopWindow;
        old_prompt :=
          if Prompt_Window then cdr atsoc('window_label, Prompt_Window);

        old_msg :=
          if Message_Window then cdr atsoc('window_label, Message_Window);

        % Two_window_midpoint is location of dividing line of dashes, wrt
        % ScreenBase, roughly speaking.
        % (3 and 5 are rather ad-hoc guesses.)
        if not numberp(two_window_midpoint) OR two_window_midpoint < 3
            OR two_window_midpoint > (Row ScreenDelta) - 5
        then
             two_window_midpoint := Fix (0.5 * (Row ScreenDelta - 2));

        Setup_Windows
            list(
              % Looks into current buffer
              TopWindow :=
              FramedWindowDescriptor(CurrentBufferName,
                               Coords(Column ScreenBase - 1,
                                      Row ScreenBase - 1),
                               Coords(Column ScreenDelta + 2,
                                      two_window_midpoint)),

              % Looks into the "message buffer", used for error messages
              % and general stuff.
              Message_Window :=
              UnframedWindowDescriptor('MESSAGE_BUFFER,
                               % Base is at two_window_midpoint
                               Coords(Column ScreenBase,
                                       Row ScreenBase + two_window_midpoint),
                               % a single line (so delta row = 0)
                               Coords(Column ScreenDelta, 0)),

              % Always looks into the 'OUT_WINDOW buffer,
              % until we can figure out a better way to handle the
              % situation??
              FramedWindowDescriptor('OUT_WINDOW,
                               Coords(Column ScreenBase - 1,
                                      Row ScreenBase +
                                      two_window_midpoint + 1),
                               % Run down to the bottom, minus a one line
                               % window.
                               Coords(Column ScreenDelta + 2,
                                      Row ScreenDelta
                                          - two_window_midpoint - 2)),

              % Looks into the "prompt line" buffer.
              Prompt_Window :=
              UnframedWindowDescriptor('PROMPT_BUFFER,
                               % Base is at bottom
                               Coords(Column ScreenBase,
                                      Row ScreenBase + Row ScreenDelta),
                               % a single line (so delta row = 0)
                               Coords(Column ScreenDelta, 0))
        );

        % Restore the labels from their old values (if any).
        SelectWindowContext(Prompt_Window);
        window_label := old_prompt;
        SelectWindowContext(Message_Window);
        window_label := old_msg;

        % Keep track of "minor windows".
        minor_window_list := list(Prompt_Window, Message_Window);

        SelectWindow TopWindow;        % ??? should this be necessary?
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Set up bindings for Rlisp Mode.
RlispDispatchList :=
list(
    % M-; inserts a comment--isn't nearly as nice as EMACS version yet.
    cons(char meta !;, 'InsertComment),

    % M-E puts us at beginning of line and then simply causes us to return
    % (exit) to the caller (roughly speaking).
    cons(char meta E, 'ReturnFromEmodeEdit),

    % M-C-Y deletes the last "expression" printed in OUT_WINDOW.
    cons(char meta cntrl Y, 'insert_last_expression)
);

% Set up bindings for Lisp Mode.  (See HP-EMODEX for additions to this
% list.)
LispDispatchList :=
list(
    % M-; inserts a comment--isn't nearly as nice as EMACS version yet.
    cons(char meta !;, 'InsertComment),

    % M-E puts us at beginning of line and then simply causes us to return
    % (exit) to the caller (roughly speaking).
    cons(char meta E, 'ReturnFromEmodeEdit),

    % M-C-Y deletes the last "expression" printed in OUT_WINDOW.
    cons(char meta cntrl Y, 'insert_last_expression)
);

Symbolic Procedure insert_last_expression();
% Insert "last expression" typed in the OUT_WINDOW buffer.
begin scalar cbuf;
    cbuf := CurrentBufferName;  % Remember current buffer.
    SelectBuffer('OUT_WINDOW);
    % "Mark" points to start of expression, "Point" gives the end.
    % First, back up over any trailing blank lines.
    while not BeginningOfBufferP(CurrentLineIndex) and point = 0 do
        !$BackwardCharacter();

    % Now, copy the text into the "kill buffer".
    copy_region();
    % Move back to the end of the output buffer.
    !$EndOfBuffer();

    % Select the original buffer.
    SelectBuffer(cbuf);
    insert_kill_buffer();
end;

Symbolic Procedure ReturnFromEmodeEdit();
% (Typically invoked by M-E.)  Causes EMODE to return to procedure that
% called it (via "EmodeChannelEdit").  Arranges for output to go to end of
% OUT_WINDOW buffer.
begin scalar cbuf;
    % Set point and mark for output buffer, unless it's also the input
    % buffer.
    if CurrentBufferName neq 'OUT_WINDOW then
    <<
        cbuf := CurrentBufferName;
        SelectBuffer('OUT_WINDOW);
        !$EndOfBuffer();
        SetMark();
        SelectBuffer(cbuf);     % Switch back to original buffer.

        reading_from_output := NIL;
    >>
    else
        reading_from_output := T;

    % Remember current spot, in case user wants to come back here.
    SetMark();

    % If we're at the end of the buffer, insert an EOL (gratis).
    if Point = Length CurrentLine
       and EndOfBufferP(NextIndex CurrentLineIndex)
    then
    <<
        !$CRLF();
        !$BackwardLine();   % Start out on the previous line.
    >>;

    % Start reading from the start of the line that M-E was typed at.
    !$BeginningOfLine();

    % Set things up to read from and write to EMODE buffers.
    SelectEmodeChannels();
    leave_dispatch_loop();
end;

% Make sure *EMODE's defined (as opposed to unbound?) at load time.  Hope
% we don't load inside EMODE!
!*EMODE := NIL;

% Redefine QUIT so that it restores the terminal to echoing before exiting.
if FUnboundP('original!-quit) then
    CopyD('original!-quit, 'quit);

Symbolic Procedure quit();
<<
    if !*EMODE then     % If invoked from "inside" EMODE.
    <<
        SelectOldChannels();        % Switch to original channels.  
        EchoOn();                   % Turn echoing back on.
    >>;

    original!-quit();

    % Fire up EMODE, if we called quit from inside it.
    if !*EMODE then
        EMODE();    % Select RLISP-INTERFACE mode upon restart.
>>;

Symbolic Procedure EmodeChannelEdit(chn, PromptStr);
% Invoke EMODE as the editor for a buffer channel.  Display the prompt on
% "message_window".
<<
    % Select "old" channels, so if an error occurs we don't get a bad
    % recursive situation where printing into a buffer causes more trouble!
    SelectOldChannels();
    % But, keep echoing turned off,  we need some other hook to restore
    % echoing if an error occurs.

    if null PromptStr then      % Use empty string if no prompt given.
        PromptStr := "";

%??    if not(CurrentWindowDescriptor eq InputWindowForChannel[chn]) then

    SelectBuffer(eval InputBufferForChannel[chn]);

    % Advance to end of next line, on theory that we want to move to next
    % expression to evalute.
    if not EndOfBufferP(NextIndex CurrentLineIndex) then
    <<
        !$ForwardLine();
        !$EndOfLine();
    >>;

    ERRORSET(list('EMODE1, PromptStr),T,!*BACKTRACE);
>>;

Symbolic Procedure PromptAndEdit(PromptStr);
% Allow the user to "edit" the default input channel.
    PromptAndEditOnChannel(IN!*, PromptStr);

Symbolic Procedure PromptAndEditOnChannel(chn, PromptStr);
% If there is an editor associated with the channel, call it, passing the
% channel and prompt string "PromptStr" as arguments.  Always return NIL.
<<
    if not null ChannelEditRoutine[chn] then
        Apply(ChannelEditRoutine[chn], list(chn, PromptStr));

    NIL
>>;

Symbolic Procedure MakeInputAvailable();
% THIS IS THE MAGIC FUNCTION invoked by READ, and other "reader functions".
% PROMPTSTRING!* is a global (FLUID) variable.
    PromptAndEdit(PROMPTSTRING!*);

FLUID '(
    OldStdIn
    OldStdOut
    OldErrOut
    );

Symbolic Procedure SelectOldChannels();
% Select channels that were in effect when "Rlisp Interface" was started
% up.  (But don't turn echoing on.)  NOTE that the "old channels" are
% normally selected while EMODE is actually running (this is somewhat
% counter intuitive).  This is so that any error messages created by bugs
% in EMODE will not be printed into EMODE buffers.  (If they were, it might
% break things recursively! )
<<
    % Postion the cursor to the bottom of the screen.
    SetTerminalCursor(Column ScreenBase, Row ScreenDelta);

% Currently we avoid closing the channels.  Unclear if this is right.  If
% we do decide to close channels, remember not to close a channel after
% it's already closed!  (In case, e.g., ErrOut!* = STDOUT!*.)

    STDIN!* := OldStdIn;
    STDOUT!* := OldStdOut;
    ErrOut!* := OldErrOut;

    RDS STDIN!*;    % Select the channels.
    WRS STDOUT!*;
>>;

Symbolic Procedure InsertComment();
<<
    !$EndOfLine();
    insert_string "% ";
>>;