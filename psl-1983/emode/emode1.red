%
% EMODE1.RED - Screen editor for PSL
% 
% Authors:     W. Galway, M. Griss, R. Armantrout
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        8 June 1982
% Copyright (c) 1982 University of Utah
%
%     This file is the main body of code for the screen oriented editor
% EMODE.  This editor is patterned after EMACS from MIT and also after EM
% written by Robert Armantrout for use on small Unix systems.  

FLUID '(
    Two_window_midpoint % Gives location (roughly) of dividing line for two
                        % window mode.

    FirstCall            % NIL means re-entering EMODE, T means first time.

    kill_opers           % list of (names of) dispatch routines that kill
                         % text.  NEEDS MORE DOCUMENTATION!
    kill_buffer_ring     % Vector of vectors of strings--holds recently
                         % deleted text.
    kill_ring_index      % Pointer to the most recent "kill buffer".
    last_yank_point      % Vector of [buffer lineindex point], giving location
                         % where last "yank" occured.

    last_operation       % The "last" routine dispatched to (before the
                         % "current operation").
    runflag              % EMODE continues READ/DISPATCH/REDISPLAY until NIL
    SelfInsertCharacter  % The last character typed (dispatched on?)
    last_buffername      % Name (a string) of the last buffer visited.

    !*DBG                % T for debugging (not really implemented).
  );


FirstCall := 'T;		% To force init of all structures
last_buffername := "MAIN";       % Set up default, NEEDS more thought? 

!*DBG := NIL;		% No debug

% 8 entries in the kill ring.
kill_buffer_ring := MkVect(7);
kill_ring_index := 0;

kill_opers :=
'(
    kill_line
    kill_region
    kill_forward_word
    kill_backward_word
    kill_forward_sexpr
    kill_backward_sexpr
);


Symbolic Procedure DBG1(x);
 If !*DBG then Print LIST("-> ",x);

Symbolic Procedure DBG2(x);
 If !*DBG then Print LIST("<- ",x);

FLUID '(UserSetupRoutine);
UserSetupRoutine := NIL;

Symbolic Procedure EMODE();
% Rebind channels to use "EMODE buffers", then return.  Use function
% "OldFACE" to switch back to original channels.  (OldFace is typically
% bound to M-C-Z.)
begin scalar chnl;
    if FirstCall then
    <<
        FirstCall := NIL;
        % Why doesn't ALL this code go into EMODEinitialize?  Sigh.
        EMODEinitialize();

        % Any ideas where best to place the following call?
        % ANSWER is, GET RID OF IT, it's not a proper method to allow
        % customizations, since multiple users can't use it.
        % Current practice is for UserSetupRoutine to be a fluid--set to name
        % of procedure to execute inside user's initialization routine, NIL
        % outside of that scope.
        if not null UserSetupRoutine then
            Apply(UserSetupRoutine,NIL);

        % Open up special channel for buffer I/O.  Arguments are
        % expressions to be evaluated to get name of input buffer, name of
        % output buffer, and a window to "pop up" for the output buffer.
        EmodeBufferChannel :=
            OpenBufferChannel('CurrentBufferName,
                              ''OUT_WINDOW,
                              NIL
                              );
    >>;

    EchoOff();
    !*EMODE := T;       % HERE???  Set FLUID flag to show "EMODE running".

    % ErrorSet could be used to make sure echos get turned back on.
    % Use system's idea of backtrace
    ERRORSET('(FullRefresh), T, !*BACKTRACE);
    % (Need to do something if an error!)

    SelectEmodeChannels();
end;

% Save old channels at load (compile) time?
OldStdIn := STDIN!*;
OldStdOut := STDOUT!*;
OldErrOut := ErrOut!*;

Symbolic Procedure EMODEinitialize();
% Sets up data structures for starting up EMODE.  DOESN'T affect terminal
% mode.
begin
    SetScreen();                % Initialise Screen Space

    SetupInitialBufferStructure();

    % A kludge (!?) to implement a pop-up break window.
    % Create the window to look into the "break" buffer.
    BreakWindow :=
        FramedWindowDescriptor('BREAK,
                               % Starts at column 39, Near top of screen
                               Coords(39,1),
                               % Dimensions are roughly 40 wide by 10 high.
                               Coords(39,9));

    % Very carefully (?) redefine the break handler.
    if FUnBoundP('pre_emode_break) then
    % Work with !*usermode OFF, so no objection is made as we redefine
    % Break.  Also !*REDEFMSG OFF so that it happens "quietly".
    begin scalar !*USERMODE, !*REDEFMSG;
        CopyD('pre_emode_break,'Break);
        CopyD('Break, 'EMODEbreak);
    end;

    OneWindow();    % Initialize in one-window mode.
end;

Symbolic Procedure EMODEbreak();
% Redefined break handler for EMODE.
Begin Scalar Oldwindow;
    Oldwindow:=CurrentWindowdescriptor;
    SelectWindow BreakWindow;
    !$BeginningOfBuffer();   % Place point at start of buffer.

    % Transfer control to the original break handler.  Catch may be
    % overkill, but is more certain to catch errors and stuff.
    Catch(NIL, pre_emode_break() );

    % When finished, "clean" our screen off.
    remove_current_view();

    SelectWindow Oldwindow; % Back to the window we originally had.
end;

Symbolic Procedure OldFACE();
% Causes sytem to quit using "Rlisp Interface" mode, go back to "normal mode".
<<
    SelectOldChannels();
    EchoOn();

    !*EMODE := NIL;     % HERE???

    leave_dispatch_loop();  % Set flag to cause EMODE to exit.
>>;

Symbolic Procedure SelectEmodeChannels();
% Select channels that read from and write to EMODE buffers.
<<
    % Most channels just default to these?  ErrOut!* is an exception, so
    % fix it.
    STDIN!* := EmodeBufferChannel;
    STDOUT!* := EmodeBufferChannel;
    ErrOut!* := EmodeBufferChannel;

    RDS STDIN!*;    % Select the channels, "EMODE1" is called when read
                    % routines invoke the "editor routine" for the newly
                    % selected channels.
    WRS STDOUT!*;
>>;

Symbolic Procedure OldEMODE();
% "Old fashioned" version of invoking EMODE.  "New" version invokes "Rlisp
% interface" instead.  This version is being kept for documentation--it's
% basically obsolete.
<<
    If FirstCall then
    <<
        EMODEinitialize();
        FirstCall := NIL;
    >>;

    % Any ideas where best to place the following call?
    % Current practice is for UserSetupRoutine to be a fluid--set to name
    % of procedure to execute inside user's initialization routine, NIL
    % outside of that scope.
    if not null UserSetupRoutine then
        Apply(UserSetupRoutine,NIL);

    % A bit of a kludge to make sure echos get turned back on.
    ECHOoff();
    % Do full refresh on restart, clean up junk on screen.
    ERRORSET('(FullRefresh), T, !*BACKTRACE);
    ERRORSET('(EMODE1 ""),T,!*BACKTRACE);    % Use system's idea of backtrace
    ECHOon();
>>;

Symbolic Procedure EMODE1(msg);
% "msg" is an initial message to put into the "message window".
begin
    show_message(msg);

    EMODEdispatchLoop();    % Execute read/dispatch/refresh loop until
                            % "done"
end;

Symbolic Procedure EMODEdispatchLoop();
% Execute read/dispatch/refresh loop while fluid "runflag" is true.
begin scalar runflag;
    runflag := T;
    while runflag do
    <<
        % Note that it's actually a refresh/read/dispatch loop.
        optional_refresh();

        % READ and then dispatch on character
        ERRORSET('(DISPATCHER),T,T);
        %  Refresh screen (if no user input is pending).
>>;

    PutLine();  % Make sure everything's put away!
end;

Symbolic Procedure FreshEMODE();		% Force Full Init
<<
    FirstCall := T;
    EMODE()
>>;

%. --------------- EMODE error handles

Symbolic Procedure EMODEerror(x);
  Error(666," *** EMODE *** " . x);

%. ---------- Buffer Management ----------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

FLUID '(
    BufferNames          % Buffer names are kept on the fluid association
                         % list "BufferNames", associated with a list of
                         % variable bindings (an "environment") for that
                         % buffer.

% Buffers are described by the following "per buffer" variables.  (The
% bindings of the variables depend on the current "buffer" environment.)

    CurrentBufferText    % Vector of lines making up the buffer.
                         % (CurrentLine is magic, see below.)
    CurrentBufferSize    % Number of lines actually within buffer

    CurrentLine          % The contents (text) of current line--as a linked
                         % list of character codes.  (Takes precedence over
                         % whatever is contained in the text vector.)
    CurrentLineIndex     % Index of "current line" within buffer.
    point                % Number of chars to the left of point within
                         % CurrentLine.
    );

%
% Associated with a Buffer should be:
%	Its MODE (or is this WINDOW attribute?)
%	names of referencing windows (if any)?
%	Associated File (or is this WINDOW attribute?)

%.------------- Basic Buffer Structure ----------------

Symbolic Procedure SetBufferText(i,text);
% Store text into buffer at i.  (Text is a string.)
    CurrentBufferText[i] := text;

Symbolic Procedure GetBufferText(i);
% Return the text stored in buffer at i.
    CurrentBufferText[i];

% Perhaps this is carrying "modularity" a bit too far?  [But, I think not.
% WFG]
Symbolic Procedure NextIndex(i);
% Put in bounds checking?
    i + 1;

Symbolic Procedure PreviousIndex(i);
    i - 1;

Symbolic Procedure SetupInitialBufferStructure();
% Creates initial buffers for EMODE.  Should be done at loadtime?
<<
    BufferNames := NIL;         % Association list of (Name . BufferDescriptor)
    CurrentBufferName := NIL;

    % Second argument does the actual work of creating the buffer.
    CreateBuffer('MAIN, 'create_rlisp_buffer);
    CreateBuffer('OUT_WINDOW, 'create_rlisp_buffer);

    % Not clear what the appropriate mode is, sure to change depending on
    % what's prompted for.
    CreateBuffer('PROMPT_BUFFER, 'create_rlisp_buffer);

    % Perhaps a "null" mode makes more sense here, but it's dangerous,
    % since if person edits this buffer, there's no character defined to
    % get out.  Needs more thought (as usual)!
    CreateBuffer('MESSAGE_BUFFER, 'create_rlisp_buffer);

    % Create the BREAK (input) buffer.  (I anticipate  a break output
    % buffer one of these days.)
    CreateBuffer('BREAK, 'create_rlisp_buffer);

    % Set up the buffer text.

    SelectBuffer 'BREAK;

    % Include semicolons in the text so that both the Lisp and Rlisp
    % readers can handle the break buffer.
    Insert_string("A ;% To abort");
    !$CRLF();

    Insert_string("Q ;% To quit");
    !$CRLF();

    Insert_string("T ;% To traceback");
    !$CRLF();

    Insert_string("I ;% Trace interpreted stuff");
    !$CRLF();

    Insert_string("R ;% Retry");
    !$CRLF();

    Insert_string("C ;% Continue, using last value");
    !$CRLF();

    Insert_string("? ;% For more help");
    !$CRLF();

    % Start by editing in the MAIN buffer.
    SelectBuffer('MAIN);
    EstablishCurrentMode();
>>;

Symbolic Procedure SelectBuffer(BufferName);
% Select a buffer.  (Restore its environment after saving old.)
% (Some confusing subtle points have to be resolved, concerning selecting a
% buffer "BufferName", where "BufferName" equals "CurrentBufferName".  Current
% "solution" is a kludge?)
% As an example of the sort of thing that can happen--it would seem
% unnecesary to restore the environment if we are selecting the
% CurrentBufferName.  BUT, that's not the case in the current
% implementation, since (for example) the REFRESH algorithm will select a
% window--which restores the "CurrentBufferName", and after selecting
% window, it continues to call select the buffer.  (Attempted cure for this
% is to store the CurrentBufferName under some other ID in the window
% environment.  Ultimate cure for this is to refer to buffers, and windows,
% by their values (environment association lists or whatever), rather than
% by some name.)
begin scalar BufferEnv;
    If BufferName neq CurrentBufferName then
    <<
        if  (BufferEnv := atsoc(BufferName,BufferNames)) then
            % (The environment part of (name . env) pair.)
            BufferEnv := cdr BufferEnv
        else
            return
                EMODEError list("Buffer ", BufferName, " can't be selected");

        if CurrentBufferName then
            DeSelectBuffer CurrentBufferName;

        RestoreEnv BufferEnv;     % Restore environment for buffer
        CurrentBufferName := BufferName;
    >>;
end;

Symbolic Procedure DeSelectBuffer(BufferName);
begin scalar BufferEnv;
    if null (BufferEnv := assoc(BufferName,BufferNames)) then
        Return Prin2t LIST("Buffer doesn't exist to deselect:",BufferName);

    SaveEnv(cdr BufferEnv);    % Save current buffer bindings (uses RPLACD)
    CurrentBufferName := NIL;
end;

%. ------------ Line and Char Counting ----------------

% Count lines from P1 to P2 (0 if P1 = P2).
Symbolic Procedure CountLinesFrom(P1,P2);
    P2 - P1;                    % This was harder when a linked list was
                                % used (in the past) to represent buffers.

% Returns number of lines in current buffer.
Symbolic Procedure CountAllLines;
    CurrentBufferSize;

% Returns number of lines from current line (inclusive) to end of buffer.
Symbolic Procedure CountLinesLeft;
    CurrentBufferSize - CurrentLineIndex;

% Returns number of lines before the current line.
Symbolic Procedure CountLinesBefore;
    CurrentLineIndex;                        % zero origin indexing

% -----------CHARACTER Lines (line contents)---------
% Some lines are currently represented as a linked list of ASCII characters .

% Insert SelfInsertCharacter into the current line, update point.
Symbolic Procedure InsertSelfCharacter();
    InsertCharacter SelfInsertCharacter;

Symbolic Procedure InsertCharacter(ch);
<<
    if ch = char EOL then
        !$CRLF()
    else
    <<
        CurrentLine := InsertListEntry(CurrentLine,Point,ch);
        Point := Point + 1;
    >>;
>>;

Symbolic Procedure transpose_characters();
% Transpose the last two characters, if we're at the end of the line, or if
% a character was just inserted.  Otherwise, transpose the characters on
% either side of point.
begin scalar  ch1, ch2;
    if point = length CurrentLine OR
               last_operation eq 'InsertSelfCharacter
    then
        !$BackwardCharacter();

    % Gripe if not enough to the left. (??)
    if point < 1 then
        return Ding();

    ch2 := CurrentCharacter();
    !$BackwardCharacter();
    ch1 := CurrentCharacter();
    DeleteCharacter();
    DeleteCharacter();
    InsertCharacter(ch2);
    InsertCharacter(ch1);
end;

Symbolic Procedure AppendLine(contents, PreviousLine);
% Append line with "contents" just past "PreviousLine"
begin integer putindx;
    CurrentBufferSize := CurrentBufferSize + 1;
    % Grow the buffer if necessary.
    if CurrentBufferSize > size(CurrentBufferText) then
        CurrentBufferText := concat(CurrentBufferText, MkVect(63));

    putindx := CurrentBufferSize - 1;   % Shuffle from the back
    while putindx > PreviousLine + 1 do
    <<
        SetBufferText(putindx, GetBufferText(putindx - 1));
        putindx := putindx - 1;
    >>;

    % Put new line just past "PreviousLine".
    SetBufferText(putindx, contents);
end;

Symbolic Procedure Insert_string(strng);
% Insert a string into the buffer, starting at point, update point to be
% just past string.
begin scalar newline;
    PutLine();                   % Pack the current line in (as a string)
    newline := GetBufferText(CurrentLineIndex);  % Grab it back.

    newline := nary!-concat(
                sub(newline,0,point-1), % head of old string
                strng,                  % new string
                                        % and tail of old string.
                sub(newline, point, size(newline) - point)
               );

    % Update point
    point := point + size(strng) + 1;
    % Put away the new line
    SetBufferText(CurrentLineIndex, newline);

    GetLine(CurrentLineIndex);   % Get it back (I know, wierd!)
end;

Procedure append_line(s);
% Append string as a new line in the current buffer.
<<
    !$CRLF();
    insert_string(s);
>>;

Symbolic Procedure InsertLine(linetext);
% Insert line before current line, then position past newly inserted line.
% (An efficiency crock?)
% "linetext" is a linked list of character codes (for now).
<<
    !$BeginningOfLine();
    !$CRLF();
    !$BackwardLine();
    CurrentLine := linetext;
    PutLine();
    !$ForwardLine();
>>;

Symbolic Procedure insert_kill_buffer();
% Insert the "kill_buffer" into the current location (i.e. "yank").  Record
% location of "point" after the yank, so that unkill_previous can avoid
% doing stuff if not at the last yank point.

% (This code isn't very efficient, it's an order(M*N) algorithm, when it
% should really be order(N)--should be reworked.)
begin scalar kill_buffer;
% Avoid doing anything if kill_buffer not set up.
    kill_buffer := kill_buffer_ring[kill_ring_index];
    if kill_buffer then
    <<
        SetMark();
        PutLine();
        Insert_string(kill_buffer[0]);
        if size(kill_buffer) > 0 then
        <<
            GetLine(CurrentLineIndex);
            !$CRLF();
            !$BackwardLine();
            for i := 1 : size(kill_buffer) - 1 do
            <<
                AppendLine(kill_buffer[i], CurrentLineIndex);
                CurrentLineIndex := NextIndex(CurrentLineIndex);
            >>;

            CurrentLineIndex := NextIndex(CurrentLineIndex);
            GetLine(CurrentLineIndex);  % KLUDGE!
            point := 0;                 % More kludge
            Insert_string(kill_buffer[size(kill_buffer)]);
        >>;

        GetLine(CurrentLineIndex);
    >>;

    % Note precise location of this yank, create the pointer if NIL.
    if null last_yank_point then
        last_yank_point := MkVect(2);

    last_yank_point[0] := CurrentBufferName;
    last_yank_point[1] := CurrentLineIndex;
    last_yank_point[2] := point;
end;

Symbolic Procedure unkill_previous();
% Delete (without saving away) the current region, and then unkill (yank)
% the "previous" entry in the kill ring.  "Ding" if not at location of last
% yank.
    if null last_yank_point
       OR not(CurrentBufferName eq last_yank_point[0])
       OR not(CurrentLineIndex equal last_yank_point[1])
       OR not(point equal last_yank_point[2])
    then
        Ding()
    else
    <<
        Delete_or_copy(T, CurrentLineIndex, point, MarkLineIndex, MarkPoint);
        rotate_kill_index(-1);
        insert_kill_buffer();
    >>;

Symbolic Procedure InsertListEntry(oldlist,pos,val);
% Insert val into oldlist at position pos (or at end of list if pos too big)
        if null oldlist then list(val)
        else if pos = 0 then cons( val , oldlist )
        else cons( car oldlist ,
                        InsertListEntry( cdr oldlist , pos-1 , val ));

% Delete character at point in current line
Symbolic Procedure DeleteCharacter();
    CurrentLine := DeleteListEntry(CurrentLine,Point);

% Delete list entry at pos (or do nothing if pos past end of list)
Symbolic Procedure DeleteListEntry(oldlist,pos);
    if null oldlist then NIL
    else if pos = 0 then cdr oldlist
    else cons(car oldlist,
               DeleteListEntry(cdr oldlist , pos-1 ));

% Return character at point in current line.
Symbolic Procedure CurrentCharacter();
begin scalar linetail;
    linetail := Tail(CurrentLine,point);
    return if null linetail then
        char EOL
    else
        car linetail;
end;

% Return first n entries at head of x.
Symbolic Procedure Head(x,n);
    if null x then
        NIL
    else if n = 0 then
        NIL
    else
        cons(car x , Head(cdr x,n-1));

Symbolic Procedure PackLine(lst);
% Pack a list of character codes into a string.
    List2String lst;

Symbolic Procedure UnpackLine(str);
% Unpack a string, or NIL, into a list of character codes.
    if null str then
        NIL                     % SPECIAL CASE
    else
        String2List str;

Symbolic Procedure PutLine();
% Put away the magical current line (may want to check for necessity?)
    SetBufferText(CurrentLineIndex, PackLine CurrentLine);

Symbolic Procedure GetLine(x);
% "UNPACK" line pointed to by x
<<
    CurrentLine := UnpackLine GetBufferText(x);
    CurrentLineIndex := x;
>>;

Symbolic Procedure SelectLine(x);
% Select a new current line at location x.
if (x neq CurrentLineIndex) then        % If a non-trivial operation
<<
    PutLine();                          % Put away the old line
    GetLine(x);                         % and fetch the  new one.
>>;

Symbolic Procedure delete_or_copy(del_flg, line1,point1, line2, point2);
% Delete (if del_flg is non-NIL) or copy (otherwise) the text between
% line1, point1 (column) through line2, point2, inclusive.  Return the
% deleted (or copied) text as a pair of ((direction_of_deletion) .
% (vector_of_strings)).  The "direction" is +1 if (line1,  point1) <=
% (line2, point2), and -1 otherwise.  Update (CurrentLineIndex, point) if
% it lies within the deleted region.
begin scalar deleted_text,dir , text_length, indx, tmp, tmp2;
    PutLine();

    dir := 1;   % Default

    % Make sure that (line1, point1) comes first.
    if line2 < line1 then
    <<
        dir := -1;
        tmp := line2;
        line2 := line1;
        line1 := tmp;

        tmp := point2;
        point2 := point1;
        point1 := tmp;
    >>
    else if (line1 = line2) and (point2 < point1) then
    <<
        dir := -1;
        tmp := point2;
        point2 := point1;
        point1 := tmp;
    >>;

    % Update (CurrentLineIndex, point), if it lies in deleted region.
    if
        del_flg
      and
        ((line1 < CurrentLineIndex)
            or ((line1 = CurrentLineIndex) and (point1 < point)))
      and
        ((CurrentLineIndex < line2)
            or ((CurrentLineIndex = line2) and (point <= point2)))
    then
    <<
        CurrentLineIndex := line1;
        point := point1;
    >>;

    % Similarly for "mark".  (A kludge, this should at least be a macro.)
    if
        del_flg
      and
        ((line1 < MarkLineIndex)
            or ((line1 = MarkLineIndex) and (point1 < MarkPoint)))
      and
        ((MarkLineIndex < line2)
            or ((MarkLineIndex = line2) and (MarkPoint <= point2)))
    then
    <<
        MarkLineIndex := line1;
        MarkPoint := point1;
    >>;

    % Get length of deleted text, in lines, suitable for 0 indexing (i.e. 0
    % is "length" for one line of text).
    text_length := line2 - line1;
    deleted_text := MkVect(text_length);
    tmp := GetBufferText(line1);    % Grab first line of region to delete.

    % Things are simple if deletion all on the same line.
    if text_length = 0 then
    <<
        if del_flg then
            SetBufferText(line1,
                          concat(sub(tmp, 0, point1-1),
                                 sub(tmp, point2, size(tmp) - point2)));

        % Refetch "current line".
        GetLine(CurrentLineIndex);
        deleted_text[0] := sub(tmp, point1, point2-point1-1);
        return  dir . deleted_text;
    >>;

    % deleted_text[0] gets everything on line1 to the right of point1, and
    % the new line gets everything to the left (with more to be tacked on
    % later).
    deleted_text[0] := sub(tmp, point1, size(tmp) - point1);

    % Store away the deleted part of the last line of the region.
    tmp2 := GetBufferText(line2);
    deleted_text[text_length] := sub(tmp2, 0, point2-1);

    % and tack the tail onto the head of undeleted line1.
    if del_flg then
        SetBufferText(line1, concat(sub(tmp, 0, point1 - 1),
                                sub(tmp2, point2, size(tmp2)-point2)));

    % Copy rest of text into deleted_text.
    for i := line1+1 : line2-1 do
        deleted_text[i-line1] := GetBufferText(i);

    % Shuffle all the text, deleting the lines between line1 and line2.
    if del_flg then
    <<
        indx := 1;
        while not EndOfBufferP(line2+indx) do
        <<
            SetBufferText(line1+indx, GetBufferText(line2 + indx));
            indx := indx + 1;
        >>;

        % Note size change (but don't bother to decrease the actual size of the
        % vector holding the text, for now).
        CurrentBufferSize := CurrentBufferSize - (line2 - line1);
    >>;

    % Refetch "current line".
    GetLine(CurrentLineIndex);
    return dir . deleted_text;
end;

Symbolic Procedure DeleteTextEntry(x);
% Delete the line at x (delete entry from vector of lines).
% Depends on CurrentLine being "put away".
<<
    if not EndOfBufferP(x) then
    <<
        x := x+1;                       % Shuffle the elements down one entry.
        while not EndOfBufferP(x) do
        <<
            SetBufferText(x-1, GetBufferText(x));
            x := x+1;
        >>;

        CurrentBufferSize := CurrentBufferSize - 1;     % Note size change
        % (But don't bother to decrease actual size of line vector.)
    >>;

    GetLine(CurrentLineIndex);
 >>;

 %. ------------- Basic Dispatch Callable Control Procedures

 Symbolic Procedure leave_dispatch_loop();
 % Set flag to cause exit from read/dispatch/refresh loop.
 <<
     PutLine();                  % Make sure current line "put away".
     runflag := NIL;             % (Set flag to be detected by "main loop".)
 >>;

 Symbolic Procedure !$DeleteBuffer();
 % Delete entire contents of buffer (similar to creating new buffer)
 <<
     % Initial vector allows only one line.  (Should really be parameterized.)
     CurrentBufferText :=  MkVect(1);

     CurrentBufferSize :=  1;            % Start with one line of text (but
                                         % zero characters in the line!)
     CurrentLine := NIL;
     CurrentLineIndex := 0;
     point := 0;
  >>;

 % Move to beginning of buffer
 Symbolic Procedure !$BeginningOfBuffer();
 <<
         SelectLine(0);
         point := 0;
 >>;

 % Move to end of buffer
 Symbolic Procedure !$EndOfBuffer();
 <<
     SelectLine(CurrentBufferSize - 1);
     point := length(CurrentLine);
 >>;

 Symbolic Procedure SetMark();
 % Set "mark" pointer from "point".
 <<
     MarkLineIndex := CurrentLineIndex;
     MarkPoint := point;
 >>;

 Symbolic Procedure ExchangePointAndMark();
 begin scalar tmp;
     tmp := point;
     point := MarkPoint;
     MarkPoint := tmp;

     tmp := CurrentLineIndex;    % NOTE, it doesn't work to just set
                                 % CurrentLineIndex := MarkLineIndex.  
     SelectLine(MarkLineIndex);
     MarkLineIndex := tmp;
 end;

 % NOTE, there is a vague asymmetry about EndOfBufferP and
 % BeginningOfBufferP.  These folks need more thought to avoid off by one
 % errors.  (Should work in terms of characters, not lines?)
 Symbolic Procedure EndOfBufferP(i);
 % Return T if i is at end of buffer (past the last line in the buffer).
     i >= CurrentBufferSize;

 Symbolic Procedure BeginningOfBufferP(i);
 % Return T if i at beginning (first line) of buffer.
     i <= 0;                             % Use <= for robustness

 % Insert a CRLF at point (new line character (or end of line character
  % if you prefer))
 Symbolic Procedure !$CRLF();
 <<
     % Store away the head of the current line (at the current line)
     SetBufferText(CurrentLineIndex , PackLine Head(CurrentLine,Point) );

     % Append the tail end of the line just past the current line, and point
     % to it.
     CurrentLine := Tail(CurrentLine,Point);
     AppendLine(PackLine CurrentLine , CurrentLineIndex);
     CurrentLineIndex := NextIndex(CurrentLineIndex);
     Point := 0;
 >>;

 % Move to beginning of current line
 Symbolic Procedure !$BeginningOfLine();
     Point := 0;

 % Move to end of current line
 Symbolic Procedure !$EndOfLine();
     Point := length(CurrentLine);

 % Move up a line (attempting to stay in same column), dont move past; % start of buffer:=
 Symbolic Procedure !$BackwardLine();
    if BeginningOfBufferP(CurrentLineIndex) then
        Ding()
    else
    <<
        SelectLine(PreviousIndex(CurrentLineIndex));
        if Point > Length CurrentLine then
            Point := Length(CurrentLine)
    >>;

 Symbolic Procedure !$ForwardLine();
 % Move down a line (attempting to stay in same column), don't move past
 % end of buffer.
     if EndOfBufferP(NextIndex CurrentLineIndex) then
         Ding()
     else
     <<
         SelectLine(NextIndex CurrentLineIndex);
         % DO WE REALLY want to change point? WFG
         If point > Length(CurrentLine) then
             point := Length CurrentLine
     >>;

 % Move back a character, to previous line if at start of current line.
 Symbolic Procedure !$BackwardCharacter();
     if point = 0 then
         if BeginningOfBufferP(CurrentLineIndex) then
             Ding()
         else
         <<
             SelectLine(PreviousIndex(CurrentLineIndex));
             point := Length(CurrentLine);
         >>
     else
         point := point - 1;

 % Move forward a character, to Next line if at end of current line.
 Symbolic Procedure !$ForwardCharacter();
     % NOTE use of "length" function, assumption of list for CurrentLine.
     if point = length(Currentline) then
         if EndOfBufferP(NextIndex CurrentLineIndex) then Ding()
         else
         <<
             SelectLine(NextIndex(CurrentLineIndex));
             Point := 0;
         >>
     else point := point+1;

 % Delete character before point.
 Symbolic Procedure !$DeleteBackwardCharacter();
 <<
     if point = 0 and BeginningOfBufferP(CurrentLineIndex) then
         Ding()
     else
     <<
         !$BackwardCharacter();
         !$DeleteForwardCharacter();
     >>;
 >>;

 % Delete character after point
 Symbolic Procedure !$DeleteForwardCharacter();
     if point = length(Currentline) then
         if EndOfBufferP(CurrentLineIndex) or    % Complain if at (or near)
            EndOfBufferP(NextIndex CurrentLineIndex)        % end of buffer.
         then
             Ding()
         else
         <<
             % non-destructively append Next line to this line
             CurrentLine :=
                 Append(CurrentLine,
                        UnpackLine GetBufferText(NextIndex(CurrentLineIndex)));
             PutLine();
             DeleteTextEntry NextIndex CurrentLineIndex;
         >>
         else
             DeleteCharacter();

Symbolic Procedure rotate_kill_index(N);
% Step the kill_ring_index by N, modulo the ring size.
begin scalar ring_size;
    kill_ring_index := kill_ring_index + N;

    % Now do "cheap and dirty" modulus function.
    % Get number of entries in ring, compensate for 0 indexing.
    ring_size := size(kill_buffer_ring) +1;

    while kill_ring_index >= ring_size do
        kill_ring_index := kill_ring_index - ring_size;

    while kill_ring_index < 0 do
        kill_ring_index := kill_ring_index + ring_size;
end;

Symbolic Procedure update_kill_buffer(killed_text);
% Update the "kill buffer", either appending/prepending to the current
% buffer, or "pushing" the kill ring, as appropriate.  killed_text is a
% pair, the car of which is +1 if the text was "forward killed", and -1 if
% "backwards killed".  The cdr is the actual text (a vector of strings).
begin scalar new_entry, tmp, tmp1, tmp2;
    % If last operation wasn't a kill, then "push" the new text.
    if not (last_operation memq kill_opers) then
    <<
        rotate_kill_index(1);       % Move to a new kill buffer.
        kill_buffer_ring[kill_ring_index] := cdr killed_text;
    >>
    else
    % Otherwise, append or prepend the text, as appropriate.
    <<
        tmp1 := kill_buffer_ring[kill_ring_index];  % The old text.
        tmp2 := cdr killed_text;                    % The new text to tack on.

        % Swap the two pieces of text if deletion was "backwards".
        if car killed_text < 0 then
        <<
            tmp := tmp1;
            tmp1 := tmp2;
            tmp2 := tmp;
        >>;

        % Allocate space for the new "kill buffer".  (A bit tricky due to 0
        % indexing and fact that the last line of tmp1 is concatenated with
        % first line of tmp2.)
        new_entry := MkVect(size(tmp1) + size(tmp2));
        tmp := 0;       % Now tmp serves as index into the new buffer.
        for i := 0 : size(tmp1) - 1 do
        <<
            new_entry[tmp] := tmp1[i];
            tmp := tmp + 1;
        >>;

        % Concatenate last line of tmp1 with first line of tmp2.
        new_entry[tmp] := concat(tmp1[size tmp1], tmp2[0]);
        tmp := tmp + 1;

        % Tack on the rest of tmp2.
        for i := 1 : size(tmp2) do
        <<
            new_entry[tmp] := tmp2[i];
            tmp := tmp + 1;
        >>;

        kill_buffer_ring[kill_ring_index] := new_entry;
    >>;
end;

Symbolic Procedure kill_region();
% Kill (and save in kill buffer) the region between point and mark.
<<
    update_kill_buffer
        delete_or_copy(T, CurrentLineIndex, point, MarkLineIndex, MarkPoint);

    
>>;

Symbolic Procedure copy_region();
% (Should this be counted as a "kill_oper"?  How about previous kills?)
<<
    update_kill_buffer
        delete_or_copy(NIL, CurrentLineIndex, point, MarkLineIndex, MarkPoint);
>>;

% Kill current line from point onwards, or delete "CRLF" if at end of line.
Symbolic Procedure kill_line();
begin scalar cline, cpoint;
    cline := CurrentLineIndex;
    cpoint := point;
    % Move over region to kill, then kill it.
    if point = length(CurrentLine) then % Delete CRLF at end of line.
        !$ForwardCharacter()            % (Skip over CRLF.)
    else
        !$EndOfLine();

    update_kill_buffer
        delete_or_copy(T, cline, cpoint, CurrentLineIndex, point);
end;

Symbolic Procedure kill_forward_word();
begin scalar cline, cpoint;
    cline := CurrentLineIndex;
    cpoint := point;
    % Move over region to kill, then kill it.
    forward_word();
    update_kill_buffer
        delete_or_copy(T, cline, cpoint, CurrentLineIndex, point);
end;

Symbolic Procedure kill_backward_word();
begin scalar cline, cpoint;
    cline := CurrentLineIndex;
    cpoint := point;
    % Move over region to kill, then kill it.
    backward_word();
    update_kill_buffer
        delete_or_copy(T, cline, cpoint, CurrentLineIndex, point);
end;

Symbolic Procedure kill_forward_sexpr();
begin scalar cline, cpoint;
    cline := CurrentLineIndex;
    cpoint := point;
    % Move over region to kill, then kill it.
    forward_sexpr();
    update_kill_buffer
        delete_or_copy(T, cline, cpoint, CurrentLineIndex, point);
end;

Symbolic Procedure kill_backward_sexpr();
begin scalar cline, cpoint;
    cline := CurrentLineIndex;
    cpoint := point;
    % Move over region to kill, then kill it.
    backward_sexpr();
    update_kill_buffer
        delete_or_copy(T, cline, cpoint, CurrentLineIndex, point);
end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Symbolic Procedure Print1Dispatch(ch1, ch2, fname);
% Print out the dispatch routine for a (possibly "extended") character.
% (Second "character" is NIL for unextended characters.)
% Don't print anything if it's a self inserting character, or "undefined".
<<
    if not(fname memq '(InsertSelfCharacter Ding)) then
        PrintF("%w %w        %w%n", character_name ch1,
                                  character_name ch2, fname);
>>;

Symbolic Procedure PrintAllDispatch;
% Print out the current dispatch table.
% Need a "mode" that dumps stuff in a form appropriate for SCRIBE?
<<
    % First, list the routines bound to single characters.
    for ch := 0:255 do
        Print1Dispatch(ch, NIL, getv(MainDispatch, ch));

    % next, list all the C-X bindings
    for each x in cdr atsoc(char cntrl X, PrefixAssociationLists) do
        Print1Dispatch(char cntrl X, car x, cdr x);
>>;

Symbolic Procedure GetInternalName(ch,DispatchTable);
  if pairp DispatchTable then
	if(ch := atsoc(ch,DispatchTable)) then cdr ch else 'Ding
   else getv(DispatchTable,ch);

fluid '(character_name_table);

% An association list of (character code . name), used by procedure
% character_name.
character_name_table :=
   '(
      (8#7 . "Bell")
      (8#10 . "Backspace")
      (8#11 . "Tab")
      (8#12 . "Linefeed")
      (8#15 . "Return")
      (8#33 . "Escape")
      (8#40 . "Blank")
      (8#177 . "Rubout")
    );

Symbolic Procedure character_name(ch);
% Return a string giving the name for a character code, return "" if "ch"
% not a number.  Names for control characters are typically "C-...", names
% for meta characters are "M-...".  Printing characters name themselves.
begin scalar name;
   % Typically ch will be NIL if it isn't a number.
   if not numberp ch then
       return "";

   name := MkString(0,0);               % A one character string
   if ch > char BLANK and ch <= char '!~ then
       name[0] := ch                    % A "printing" character
   else if LAND(ch, 8#200) neq 0 then   % Meta bit set
       name := concat("M-", character_name LAND(ch,8#177))
   else if name := atsoc(ch, character_name_table) then
       name := cdr name                 % association list catches wild cards.
   else if ch < char BLANK then
       name := concat("C-",
                           if ch = 8#37 then character_name(char RUBOUT)
                           else character_name(ch + 8#100))
   else
       EMODEerror list(ch, " is bad character code for routine `character_name'");

   return name;
end;

Symbolic Procedure !$HelpDispatch();
% Give a little information on the routine bound to a keyboard character
% (or characters, in the case of prefixed things).
% We need to do a better job of merging this code with PrintAllDispatch,
% AND the code that actually dispatches.
begin scalar ch1, ch2, fname;
    ch1 := prompt_for_character("Function of character: ");
    if ch1 = char ESC then              % Treat as meta character
    <<
        ch1 := LOR( 8#200, GetNextCommandCharacter());
        fname := GetInternalName(ch1, MainDispatch)
    >>
    else if ch1 = char meta X OR ch1 = char cntrl X then
    <<
        ch2 := GetNextCommandCharacter();
        fname := GetInternalName(ch2,atsoc(ch1, PrefixAssociationLists))
    >>
    else
        fname := GetInternalName(ch1,MainDispatch);

    show_message BldMsg("%w %w        %w", character_name ch1,
                                           character_name ch2, fname);
end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Symbolic Procedure OpenLine();
% Insert a NEWLINE (or EOL) at POINT, keep POINT before newline
<<
    InsertCharacter(char EOL);
    !$BackwardCharacter();
>>;
