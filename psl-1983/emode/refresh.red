%
% REFRESH.RED - Screen/Window/Refresh utilities for EMODE.
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        8 June 1982
% Copyright (c) 1982 University of Utah
%
% Uses the "virtual-screen" package in VIRTUAL-SCREEN.SL.

FLUID '(
    ShiftDisplayColumn          % Amount to shift things to the left by
                                % before (re)displaying lines.
    WindowList                  % List of active windows
    minor_window_list           % List of windows to be ignored by the
                                % "next_window" routine.
    pos_for_line_refresh

    % Offsets into virtual screen, adjusted depending on whether screen is
    % framed, labled, etc.
    row_offset
    column_offset
    );

% pos_for_line_refresh is kept around so that we don't have to keep consing
% up new coordinate pairs--an efficiency hack.  '(NIL . NIL) may cause
% problems on Vax (when we do RPLACA/RPLACD), since it goes to "pure
% space"?

pos_for_line_refresh := cons(NIL , NIL);

ShiftDisplayColumn := 0;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Construct a screen coordinate pair (x,y) = (column,row)
Symbolic Procedure Coords(col,rw);
  Cons(col,rw);

Symbolic Procedure Column pos;          %. X-coordinate (Column)
  car pos;

Symbolic Procedure Row pos;             %. Y-coordinate  (Row)
  cdr pos;

% Note: All regions defined in terms of Lower Corner (base) and distance
% (delta values) to other corner INCLUSIVE, using 0-origin system.
% Thus 0..3 has base 0, delta 3
%      1..4 has base 1, delta 3

Symbolic Procedure FrameScreen(scrn);
% Generate a border for a screen.
<<
    % Dashes for top and bottom rows.
    for i := 0:VirtualScreenWidth(scrn) do
    <<
        WriteToScreen(scrn, char !-, 0, i);
        WriteToScreen(scrn, char !-, VirtualScreenHeight(scrn), i);
    >>;

    % Vertical bars for the left and right sides.
    for i := 0:VirtualScreenHeight(scrn) do
    <<
        WriteToScreen(scrn, char !|, i, 0);
        WriteToScreen(scrn, char !|, i, VirtualScreenWidth(scrn));
    >>;

    % Finally, put plus signs in the corners.
    WriteToScreen(scrn, char !+, 0, 0);
    WriteToScreen(scrn, char !+, 0, VirtualScreenWidth(scrn));
    WriteToScreen(scrn, char !+, VirtualScreenHeight(scrn), 0);
    WriteToScreen(scrn, char !+,
                    VirtualScreenHeight(scrn), VirtualScreenWidth(scrn));
>>;

Symbolic Procedure FramedWindowDescriptor(BufferName, upperleft, dxdy);
% Create a "descriptor" for a "framed window" (into a text buffer), given
% its associated buffer name, coord. of upper left corner, and its size as
% (Delta X, Delta Y).
begin scalar WindowDescriptor, newscreen;
    % The virtual screen includes room for a border around the edges.
    % (Add one to dimensions, to compensate for 0 indexing.)
    newscreen :=
        CreateVirtualScreen(1 + Row dxdy, 1 + Column dxdy,
                        Row upperleft, Column upperleft);

    % Generate the border.
    FrameScreen(newscreen);

    WindowDescriptor :=
      list(
            % The refresh routine to use.
            'windows_refresher . 'refresh_framed_window,
            'WindowsBufferName . BufferName,          % Associated Buffer
            % Routine to "throw away" the current view.
            'views_cleanup_routine . 'cleanup_text_view,

            % Dimensions, (delta x . delta y), chop off a bit for the
            % frames.  (Remember the 0 indexing! )
            'CurrentWindowDelta .
              ( (Column(dxdy) - 2) . (Row(dxdy) - 2) ),

            % "Window image" information for refresh.
            % Note that Row dxdy = number of lines minus 1
            % (since it is an INCLUSIVE value).  Each entry in NLIST gives
            % info on (Horizontal scroll . line in buffer)
            'Window_Image .
                % ShiftdisplayColumn better than 0 here?
               Nlist(Row(dxdy)+1, '(0 . NIL)),

            % The last "buffer name" that was shown in the label,  this can
            % change if the window starts looking into another buffer.
            'LABEL_BufferName . NIL,

            % The filename associated with this window's buffer (at last
            % refresh).
            'last_filename . NIL,

            % Value of CurrentLineIndex during last refresh.
            'Last_LineIndex . 0, 
            % Size of buffer (number of lines) during last refresh.
            'Last_BufferSize . 0,

            'CurrentVirtualScreen . newscreen,

            'ShiftDisplayColumn . 0,    % Horizontal Scroll value

            % Location in buffer that corresponds to top line in window.
            % Zero is rather implausible if "point" is somewhere in the
            % middle of the buffer, but that's OK since it gets adjusted to
            % the right value.
            'TopOfDisplayIndex . 0
    );

    return WindowDescriptor;
end;

Symbolic Procedure UnframedWindowDescriptor(BufferName, upperleft, dxdy);
% Create a "descriptor" for an "unframed window", given its
% associated buffer name, coord. of upper left corner, and its size as
% (Delta X, Delta Y).  (This version is really meant for one line windows
% only, results may be quite wierd otherwise.)
begin scalar WindowDescriptor, newscreen;
    % The associated virtual screen ...
    % (Add one to dimensions, to compensate for 0 indexing.)
    newscreen :=
        CreateVirtualScreen(1 + Row dxdy, 1 + Column dxdy,
                        Row upperleft, Column upperleft);

    WindowDescriptor :=
      list(
            % The refresh routine to use.
            'windows_refresher . 'refresh_unframed_window,
            'WindowsBufferName . BufferName,          % Associated Buffer
            'views_cleanup_routine . 'cleanup_text_view,

            % A "label" to appear at the beginning line of the window.
            'window_label . "",
            % Value of window_label at last refresh, make it differ from
            % window_label to force initial refresh of label.
            'old_window_label . NIL,

            % Window dimensions as (delta x . delta y).
            'CurrentWindowDelta .
              ( (Column dxdy) . (Row dxdy) ),

            % "Window image" information for refresh.
            % Note that Row dxdy = number of lines minus 1
            % (since it is an INCLUSIVE value).  Each entry in NLIST gives
            % info on (Horizontal scroll . line in buffer)
            'Window_Image .
                % ShiftdisplayColumn better than 0 here?
               Nlist(Row(dxdy)+1, '(0 . NIL)),

            'CurrentVirtualScreen . newscreen,

            'ShiftDisplayColumn . 0,    % Horizontal Scroll value

            % Location in buffer that corresponds to top line in window.
            % Zero is rather implausible if "point" is somewhere in the
            % middle of the buffer, but that's OK since it gets adjusted to
            % the right value.
            'TopOfDisplayIndex . 0
    );

    return WindowDescriptor;
end;

fluid '(Prompt_Window Message_Window);

Symbolic Procedure OneWindow();
% Dispatch to this routine to enter one-window mode.
    if MajorWindowCount() neq 1 then      % If not already one-window
    % then setup windows for one window mode.
    begin scalar old_prompt, old_msg, NewWindow ;
    % Preserve the "prompt" and "message" labels from old windows.
        old_prompt :=
          if Prompt_Window then cdr atsoc('window_label, Prompt_Window);

        old_msg :=
          if Message_Window then cdr atsoc('window_label, Message_Window);

        Setup_Windows
            list(
              % This window looks into the current buffer, other arguments
              % are location of upper left corner, and the size (0
              % indexed).
              % The window is made slightly wider than the screen, so that
              % the left and right frame boundaries don't actually show.
              NewWindow :=
              FramedWindowDescriptor(CurrentBufferName,
                               % Upper left corner
                               coords(Column ScreenBase - 1,
                                      Row ScreenBase - 1),
                               % Size uses entire width, leaves room for
                               % two one line windows at the bottom
                               Coords(Column ScreenDelta + 2,
                                       Row(ScreenDelta) - 1)),

              % Looks into the "prompt line" buffer.  Note this is
              % unframed, so we make it a bit smaller to have it all fit on
              % the screen.
              Prompt_Window :=
              UnframedWindowDescriptor('PROMPT_BUFFER,
                               % Base is one line above bottom
                               Coords(Column ScreenBase,
                                       Row ScreenBase + Row ScreenDelta - 1),
                               % a single line (so delta row = 0)
                               Coords(Column ScreenDelta, 0)),


              % Looks into the "message buffer", used for error messages
              % and general stuff.
              Message_Window :=
              UnframedWindowDescriptor('MESSAGE_BUFFER,
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
        
        SelectWindow NewWindow;        % ??? needs more thought.
    end;

Symbolic Procedure MajorWindowCount();
% Return a count of the "major windows" in WindowList;
    length(WindowList) - length(minor_window_list);

Symbolic Procedure next_window();
% Dispatch to this routine to select "the next"  (or "other") window
begin scalar current_window_pointer;
    current_window_pointer := WindowList;
    % Look up the location of the current window in WindowList.
    while not((car current_window_pointer) eq CurrentWindowDescriptor)
    do
        current_window_pointer := cdr current_window_pointer;

    SelectWindow next_major_window(cdr(current_window_pointer), WindowList);
end;

Symbolic Procedure previous_window_command();
% Dispatch to this routine to select the "previous" window.
begin scalar current_window_pointer, rev_windowlist;
    rev_windowlist := reverse WindowList;
    current_window_pointer := rev_windowlist;
    % Look up the location of the current window in WindowList.
    while not((car current_window_pointer) eq CurrentWindowDescriptor)
    do
        current_window_pointer := cdr current_window_pointer;

    SelectWindow
        next_major_window(cdr(current_window_pointer), rev_windowlist);
end;

Symbolic Procedure next_major_window(pntr, wlist);
% Return the window descriptor for the next "major" window at or after pntr
% in wlist.  It's assumed that there is at least one major window.
    if null pntr then
        next_major_window(wlist,wlist)
    else if not MemQ(car pntr, minor_window_list) then
        car pntr
    else
        next_major_window(cdr pntr, wlist);

% Return T if the buffer is present in some "active" window (not
% necessarily visible, it may be covered up).
Symbolic Procedure Buffer_VisibleP(BufferName);
begin scalar result, Wlist;
    Wlist := WindowList;
    while Wlist and null(result) do
    <<
        result :=
          cdr(atsoc('WindowsBufferName, car Wlist)) eq BufferName;

        Wlist := cdr Wlist;
    >>;

    return result;
end;


Symbolic Procedure Setup_Windows(WindowDescriptorList);
% (Re)build the list of currently active windows.
<<
    % Get rid of the old virtual screens first.
    for each WindowDescriptor in WindowList do
        DeselectScreen cdr atsoc('CurrentVirtualScreen, WindowDescriptor);

    CurrentWindowDescriptor := NIL;
    WindowList := NIL;

    for each WindowDescriptor in WindowDescriptorList do
        SelectWindow WindowDescriptor;
>>;

Symbolic Procedure SelectWindow(WindowDescriptor);
% Select a window's "context", and also put it on top of the screen.
<<
    SelectWindowContext(WindowDescriptor);
    SelectScreen(CurrentVirtualScreen);
>>;

Symbolic Procedure SelectWindowContext(WindowDescriptor);
% Select a new window context (environment)--add it to the list of active
% windows if not already present.
begin
    % Should this (putting onto active WindowList) be part of
    % "SelectWindow" instead of "SelectWindowContext"?
    if null( MemQ(WindowDescriptor, WindowList)) then
        WindowList := WindowDescriptor . WindowList;

    if CurrentWindowDescriptor then
        DeselectCurrentWindow();

    RestoreEnv WindowDescriptor;

    % Additional cleanup after "restoring" environment.  THIS IS A KLUDGE,
    % NEEDS MORE THOUGHT!  Restore the buffer (given its name)
    SelectBuffer(WindowsBufferName);

    CurrentWindowDescriptor := WindowDescriptor;
end;

Symbolic Procedure DeselectCurrentWindow();
% Save current window's environment.  Note that this routine does NOT
% remove the current window from the list of active windows, nor does it
% affect the window's "virtual screen".
begin
   % Do this first!  Save current environment.
   SaveEnv(CurrentWindowDescriptor);
   if CurrentBufferName then
       DeSelectBuffer(CurrentBufferName);    % Important to do this after!

   CurrentWindowDescriptor := NIL;
end;

% Generic version--"clean" current view out of the list of views to be
% refreshed.
Symbolic Procedure remove_current_view();
<<
    WindowList := DelQIP(CurrentWindowDescriptor, WindowList);
    apply(views_cleanup_routine, NIL);

    % Save the current window's environment, not really a "deselect", but
    % does set CurrentWindowDescriptor to NIL.
    DeselectCurrentWindow();
>>;

% Cleanup a current text "view".
Symbolic Procedure cleanup_text_view();
    % "Throw away" the view's virtual screen, that should suffice for
    % cleanup.
    DeselectScreen CurrentVirtualScreen;

Symbolic Procedure CntrlXCscroll();
Begin scalar x;
    x := OneLispRead("Column (left/right) Scroll  by:");
    if numberp x then ShiftDisplayColumn := x;
End;

Symbolic Procedure SetScreen;
% Initialise Screen Space, obviously needs more thought, since it does so
% little.
<<
    WindowList := NIL;
    InitializeScreenPackage();        % ??? (Experimental version! )
>>;

%. ------------------- Window-Buffer-Screen Refresh ---------

Symbolic Procedure WriteScreenPhoto();
% Dispatch to this routine to write a photograph of the screen.  May want
% to get fancy and copy the screen before prompting for the file name?
begin scalar Outchannel;
    Outchannel := Open(prompt_for_string("File for photo: ", NIL), 'OUTPUT);
    WriteScreenImage(PhysicalScreenImage, Outchannel);
    Close Outchannel;
end;

Symbolic Procedure Refresh();
Begin Scalar SaveW;
    SaveW := CurrentWindowDescriptor;   % Remember the current window.

    % Refresh all windows in the list
    for each WindowDescriptor in WindowList do
    <<
        % Select the window's "context" (per-window variable bindings).
        SelectWindowContext WindowDescriptor;
        % Call the per-window refresh algorithm.
        apply(windows_refresher, NIL);
    >>;

    SelectWindowContext SaveW;            % Back to "current window"

    % Refresh up to this point has been to a "physical screen image", now
    % actually update the physical screen.
    RefreshPhysicalScreen(T);
End;

Symbolic Procedure optional_refresh();
% If nothing's waiting in the input buffer then refresh the screen
    if CharsInInputBuffer() = 0 then
        Refresh();

Symbolic Procedure refresh_unframed_window();
<<
    row_offset := 0;
    column_offset := 1 + size(window_label);
    % Refresh the label first (may clear to end of line).
    refresh_unframed_label();
    % then refresh the text (probably on the same line as label).
    refresh_text();
>>;

Symbolic Procedure refresh_unframed_label();
% Refresh the label for an "unframed window".
    % NOTE use of EQ test, avoid destructive operations on the label
    % string since they won't be detected here.
    if not(window_label eq old_window_label) then
    <<
        for i := 0:size(window_label) do
            WriteToScreen(CurrentVirtualScreen, window_label[i],
                          0,i       % Row, column
                         );

        % Then, clear to the end of the old label.  (Note that old label
        % can be NIL, in which case the size is -1.)
        WriteToScreenRange(CurrentVirtualScreen, char BLANK,
                           0,   % Row
                           size(window_label) + 1, % Left margin
                           size(old_window_label)       % Right margin
                         );

        % "Remember" the new label.
        old_window_label := window_label;
    >>;

Symbolic Procedure refresh_framed_window();
% Refresh the currently selected "framed window" (into a text buffer).
<<
    % Set up offsets to compensate for the frame.
    row_offset := 1;
    column_offset := 1;
    refresh_text();
    refresh_frame_label();
>>;

Symbolic Procedure refresh_frame_label();
% Refresh the "label line" for the current (framed) window.  Note that this
% is called on every refresh (typically on every character typed by the
% user), so it should avoid doing too much--and should be as incremental as
% possible.  NOTE:  should really be template driven.
begin scalar strng, lastcol;
   % If the name of the current buffer differs from what it used to be...
   if not(CurrentBufferName eq LABEL_BufferName) then
   <<
       strng := Id2String CurrentBufferName;
       for i := 0:size(strng) do
       % 5 is rather arbitrary point to start ...
           WriteToScreen(CurrentVirtualScreen, strng[i],
                          VirtualScreenHeight(CurrentVirtualScreen), i+5);

       % Write dashes to erase any of the old label that might be left.
       % (Might be better to WriteToScreenRange?)
       for i := 1+size(strng) : size(Id2String LABEL_BufferName) do
           WriteToScreen(CurrentVirtualScreen, char '!-,
                          VirtualScreenHeight(CurrentVirtualScreen), i+5);

       LABEL_BufferName := CurrentBufferName;
    >>;

    % Now, refresh the filename associated with this buffer.
    if not(buffers_file eq last_filename) then
    <<
        % Note the first free column (roughly speaking) past the name of
        % the buffer.
        lastcol := size(Id2String CurrentBufferName)+5;

        % Write a dash to clear things out.
        WriteToScreen(CurrentVirtualScreen, char !-,
                      VirtualScreenHeight(CurrentVirtualScreen),
                      lastcol + 1);

        % Write out the new name, a bit to the right of the buffername,
        % within square brackets.
        WriteToScreen(CurrentVirtualScreen, char '![,
                      VirtualScreenHeight(CurrentVirtualScreen),
                      lastcol + 2);

        % Write out the new filename
        lastcol := lastcol + 3;
        for i := 0:size(buffers_file) do
            WriteToScreen(CurrentVirtualScreen, buffers_file[i],
                          VirtualScreenHeight(CurrentVirtualScreen),
                          i + lastcol);

        % Hum, rather awkward to constantly keep track of column, anyway,
        % now write the closing bracket.
        WriteToScreen(CurrentVirtualScreen, char '!],
                      VirtualScreenHeight(CurrentVirtualScreen),
                      1 + size(buffers_file) + lastcol);
                          
        % Finally (?) write out a bunch of dashes to clear any old stuff.
        % Dashes go out to point where "percentage position" starts.
        WriteToScreenRange(CurrentVirtualScreen, char !-,
                           VirtualScreenHeight(CurrentVirtualScreen),
                           2 + size(buffers_file) + lastcol,
                           VirtualScreenWidth(CurrentVirtualScreen) - 7);

        % "Remember" the filename shown in the label.
        last_filename := CurrentBufferName;
    >>;

    % Now, refresh our "percentage position within buffer" stuff.
    if Last_BufferSize neq CurrentBufferSize
      OR Last_LineIndex neq CurrentLineIndex then
      if CurrentBufferSize >= 0 then
      <<
          strng := PrintF_into_string(MkString(3,char !-), 0, "%w%%",
                          (100*CurrentLineIndex)/CurrentBufferSize);

          % Write it into the label line, use "-" for any digits missing.
          for i := 0:3 do
          WriteToScreen(CurrentVirtualScreen, strng[i],
                        VirtualScreenHeight(CurrentVirtualScreen),
                        VirtualScreenWidth(CurrentVirtualScreen) - 6 + i);

          Last_LineIndex := CurrentLineIndex;
          Last_BufferSize := CurrentBufferSize;
      >>;
end;   

Symbolic Procedure refresh_text();
% Refresh for both framed and unframed windows into text buffers.
begin scalar l,l1,l2;
    % re-center display if needed
    AdjustTopOfDisplayIndex();

    l1 := TopOfDisplayIndex;
    l := 0;                     % start at Virtual row 0;
    while not EndOfBufferP(l1)
            and (l <= Row CurrentWindowDelta) do
    <<
        RefreshLine(l1,l);
        l := l + 1;
        l1 := NextIndex(l1);
    >>;
    ClearToEndOfWindow(l);

    % Position the (virtual) cursor at its final location.
    MoveToScreenLocation(
        CurrentVirtualScreen,
        % Row
        row_offset + CountLinesFrom(TopOfDisplayIndex,CurrentLineIndex),
        % Column
        column_offset + LineColumn(Point,CurrentLine)-ShiftDisplayColumn
      );
end;

% Return a list with n NIL's
Symbolic Procedure Nils(n);
    Nlist(n,NIL);

% Return a list with n copies of element.
Symbolic Procedure Nlist(n,element);
 If n<=0 then NIL
  else (copy element) . Nlist(n-1,element);

% Return a list of n 0's.
Symbolic Procedure Zeroes(n);
    Nlist(n,0);

Symbolic Procedure ClearToEndOfWindow(x);
% Clear in the vertical direction, down the window.  X gives line number to
% start at.
begin
    while x <= Row CurrentWindowDelta do
    <<
        if not null cdr Window_Image[x] then
        <<  % If something is in screen image, clear it and the screen.
            % Store (current column . no text at all)! in image.
            Window_Image[x] :=  ShiftDisplayColumn . NIL;
            ClearEol(Coords(0,x));
        >>;
        x := x+1;
    >>;
end;

Symbolic Procedure ClearEol(x);
% Clear to end of line in current window, starting at coordinate x.
    DisplaySpaces(x, 1 + Column(CurrentWindowDelta) - Column(x));

Symbolic Procedure DisplaySpaces(pos, N);
begin scalar VirtualScreenRow, VirtualScreenColumn;
% Put N spaces in window, starting at pos.
    VirtualScreenRow := row_offset + row(pos);
    VirtualScreenColumn := column_offset + column(pos);

    WriteToScreenRange(CurrentVirtualScreen,
                        char BLANK,     % Character to write
                        VirtualScreenRow,       % Row to start at
                        VirtualScreenColumn,    % Left margin

                        % Compensate for zero indexing to get right margin.
                        N - 1 +  VirtualScreenColumn);

end;

Symbolic Procedure RefreshLine(lineindex,image_linenumber);
% Refresh line if it has changed
begin scalar newline, old_shift, old_line,
    old_shift_and_line, i, tabcolumn, ch;

    if lineindex neq CurrentLineIndex then
        newline := GetBufferText(lineindex)
    else
        newline := CurrentLine; % Special case (currently a list of
                                % character codes)

    % Get dotted pair of last stored (ShiftDisplayColumn . newline)
    old_shift_and_line := Window_Image[image_linenumber];

    old_shift := car old_shift_and_line;
    old_line := cdr old_shift_and_line;

    % See if line is unchanged.  NOTE "equal" test, not "eq" test--this may
    % be a bad decision, since "equal" without "eq" is unlikely, and should
    % be handled by the following code.  (So, in some sense, use of equal
    % is redundant, and may run slower.)

    % ALSO NOTE that this test is WRONG if "destructive" changes were made to
    % the line.  (Changes that preserved eq while changing the contents.)

    if ShiftDisplayColumn = old_shift
              and newline eq old_line       % (Use eq after all!)
    then return;

    % The following code doesn't really handle horizontal scrolling
    % correctly, since matching length is the number of characters that
    % match in original strings, which might not correspond to what would
    % be displayed (due to tabs, etc.)  (Need to change the "units" that
    % MatchLength returns?)

    % Get index of starting point for redisplay
    if ShiftDisplayColumn = old_shift then
        i := MatchLength(old_line,newline)
    else
        i := ShiftDisplayColumn;

    % Save new line and shift value in screen "image"
    RPLACA(old_shift_and_line,ShiftDisplayColumn);
    RPLACD(old_shift_and_line, newline);

    % Get coordinate of starting point (first mismatch, roughly speaking).
    pos_for_line_refresh := coords(LineColumn(i,newline) - ShiftDisplayColumn,
                                               image_linenumber); 
    while not null newline
          and i <= size newline
          and Column pos_for_line_refresh <= Column CurrentWindowDelta do
    <<
        % More kludges!
        ch := newline[i];
        if ch eq char TAB then
        <<
        % May print unnecessary characters
            tabcolumn := 8*(1 + Column(pos_for_line_refresh)/8);
            while Column pos_for_line_refresh < tabcolumn do
                % DESTRUCTIVELY updates pos_for_line_refresh
                DisplayCharacter(pos_for_line_refresh, char BLANK);
        >>
        else if ch < char BLANK % ch is a control character.
        then
        <<
            DisplayCharacter(pos_for_line_refresh, char !^);
            % Convert the control character to a "normal" character.
            DisplayCharacter(pos_for_line_refresh, ch + 8#100);
        >>
        else
            % DESTRUCTIVELY updates pos_for_line_refresh
            DisplayCharacter(pos_for_line_refresh, ch);

        i := i + 1;
    >>;
    ClearEol(pos_for_line_refresh);
end;

Symbolic Procedure DisplayCharacter(pos,chr);
% Display chr at position pos, DESTRUCTIVELY update pos to next column,
% same row.  (Character is written to a "virtual screen", with an offset
% given by row_offset and column_offset.)
begin
    % Map from "window coordinates" to "virtual screen coordinates" and
    % write out the character.
    WriteToScreen(CurrentVirtualScreen, chr,
                  row_offset + Row(pos),
                  column_offset + column(pos)
                 );

    % Destructively update pos too
    RPLACA(pos, 1 +  Column pos);       % New column
    return pos;
end;

Symbolic Procedure nxt_item(strm);
% Get next item in a stream--represented as a pair of
% ("generalized-vector" . last-index), see "create_stream" below.
% Returns NIL if nothing left in stream--so you can't store NIL in the
% middle.
% A quick kludge so that we can step through lists without costly INDX
% function (which always starts at the front and CDRs down).
begin scalar itm, i;
    if PairP car strm then
    <<
        if (itm := cdr strm) then
        <<
            RPLACD(strm, cdr itm);
            itm := car itm;
        >>
    >>
    else
    <<
        i := cdr strm;
        if i <= size (car strm) then
            itm := (car strm)[i]
        else
            itm := NIL;

        RPLACD(strm, i + 1);
    >>;

    return itm;
end;

Symbolic Procedure create_stream(gvec);
    if PairP gvec then
        (gvec . gvec)
    else
        (gvec . 0);

Symbolic Procedure MatchLength(l1,l2);
% Measure lengths of matching heads for l1,l2.
begin scalar itm1, itm2; integer n;
    if null l1 or null l2 then
        return 0;

    l1 := create_stream(l1);
    l2 := create_stream(l2);

    n := 0;
    while (itm1 := nxt_item l1) and (itm2 := nxt_item l2) and itm1 = itm2 do
        n := n + 1;

    return n;
end;

Symbolic Procedure LineColumn(N,line);
% Map character position N within string line into true column position.
% Somewhat non-trivial if string contains tabs or other control characters.
    if null line or line = "" then
        0
    else
    begin scalar pos, itm;
        pos := 0;
        line := create_stream(line);
        while n > 0 and (itm := nxt_item line) do
        <<
            n := n - 1;
            if itm = char TAB then
                pos := 8*(1 + pos/8)        % Kludge
            else if itm < char BLANK then
                pos := pos + 2
            else
                pos := pos + 1;
        >>;

        return pos;
    end;

Symbolic Procedure FullRefresh();
% Force a complete refresh of the screen (but only work at the "virtual
% screen" level, don't bother to delve more deeply into the underlying
% buffers.
<<
    ClearPhysicalScreen();
    RefreshPhysicalScreen();
>>;

Symbolic Procedure AdjustTopOfDisplayIndex();
% Center the display around point.  Modify global TopOfDisplayIndex
begin scalar LinesInBuffer,LinesToPoint,LinesInScreen,MidScreen,LinesToTop;
     LinesInBuffer := CountAllLines(); % Size of file
     LinesInScreen := Row CurrentWindowDelta;  %/ (MAY BE OFF BY ONE?) WFG
     MidScreen := LinesInScreen/2;

     if LinesInBuffer<=LinesInScreen then        % Use top of buffer if it
         return(TopOfDisplayIndex := 0);         % all fits on screen.

     % Lines from start of buffer to first line displayed (exclusive)
     LinesToTop := CountLinesFrom(0,TopOfDisplayIndex);

     % Lines from start of buffer to line where Point is.
     LinesToPoint := CountLinesBefore();

     if LinesToTop<=LinesToPoint     % Point below top and above bottom
        and LinesToPoint <=(LinesToTop+LinesInScreen)
     then
         return(TopOfDisplayIndex);

     LinesToTop := LinesToPoint-MidScreen;    % Desired   
%     TopOfDisplayIndex := 0;
%    While LinesToTop > 0 do
%    <<
%        TopOfDisplayIndex := NextIndex TopOfDisplayIndex;
%        LinesToTop := LinesToTop -1
%    >>;
%
%     return TopOfDisplayIndex;
%%%%%%%%%%%%%%%%%%%% above code is more general, but very inefficient


    % (Depends on fact that "DisplayIndexes" are integers in this
    % implementation.)
     return (TopOfDisplayIndex := max(0,LinesToTop));
end;
