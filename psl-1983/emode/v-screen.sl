%
% V-SCREEN.SL - Utilities to handle "virtual screens" (alias "windows").
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        8 June 1982
% Copyright (c) 1982 University of Utah
%

% These utilities implement "virtual screens" , and do screen refresh.
% (Primarily designed to serve as a support package for EMODE, but may be
% more generally useful.)

% Some support routines for this package reside in the file
% "V-SCREEN-SUPPORT.RED".

% The current implementation is tentative--needs more thought, more
% formalization of how refresh should work, better handling of terminals
% with line insert/delete, better handling of scrolling, more consideration
% of methods used for the Lisp Machine, etc.  (Should there be fewer levels
% of storage?)

% Virtual screens are represented as vectors of strings, one string for
% each row of the "screen".  (Other information, such as virtual cursor
% location, is also stored in the structure.)

% Virtual screens are created with the function "CreateVirtualScreen".  They
% aren't actually displayed until you call "SelectScreen"--which assigns a
% "screen number" for the screen (for masking) if it doesn't already have
% one, and "draws" the new screen "on top" of all the others.  (I.e. it
% "activates" the screen.)  Screens can be made to disappear by covering
% them with other screens, or by calling "DeSelectScreen".  It IS legal to
% operate on inactive screens (i.e. write to them, move the virtual cursor,
% etc).  To completely get rid of a screen, get rid of all references to
% it, and it will go away at the next garbage collection.

% The philosophy is that these arrays will serve as caches for stuff that
% can't actually make it to the "true screen" because of being covered by
% other "virtual screens".  The routines are optimized for writing
% characters onto a virtual screen--moving screens, putting a new screen on
% the top, etc., are much less efficiently handled.

% (Talk about fact that the two "screen images" don't really work the same
% way as virtual screens?)

% Maximum number of "masks" allowed.  (Corresponds to the largest number we
% can fit into a byte.)
(DefConst MaxMaskNumber 127)

% Macro for indexing into a "virtual screen" (vector of strings).
(DS index_screen (Scrn rw col)
  (igets (igetv Scrn rw) col))   % Fast string and vector accessors

% "Left associative" version of "Expand".  (Expand is right associative.)
% Useful for expanding macros for N-ary versions of left associative
% operators.  (We should really have a "robust" version of this
% utility--see "RobustExpand".)
(BothTimes  % CompileTime?
  (DE LeftAssociativeExpand (args Fn)
    (LeftAssociativeExpand1 Fn (car args) (cdr args)))
)

% Utility for implementing LeftAssociativeExpand.
% Similar to tail recursive definition of "(reverse x)" as "(rev1 x nil)".
(BothTimes  % CompileTime?
  (DE LeftAssociativeExpand1 (Fn ProcessedArgs args)
    (cond
      % No arguments left to process
      ((null args) ProcessedArgs)

      (T (LeftAssociativeExpand1
           Fn
           (list Fn ProcessedArgs (car args))
           (cdr args)))))
)

% N-ary version of indx.  (indexn X I J) is same as (indx (indx X I) J).
(BothTimes  % CompileTime?
  (DM indexn (U)
    (LeftAssociativeExpand (cdr U) 'Indx))
)

% Define components for a "range".
(DefStruct (range fast-vector)      % Make vector accesses "fast".
  MinRange                  % Minimum of a range.
  MaxRange                  % Maximum of a range.
)

% Return T if number "x" is within range "rnge".
(DS WithinRangeP (x  rnge)
  (and
    (LeQ (MinRange rnge) x)
    (LeQ x (MaxRange rnge))))

% Update a "range" so that it "brackets" a new value.
(DE PutValueIntoRange (x rnge)
  (progn
    % New minimum if x < old minimum
    (cond
      ((LessP x (MinRange rnge))
        (setf (MinRange rnge) x)))

    % New maximum if x > old maximum.
    (cond
      ((GreaterP x (MaxRange rnge))
        (setf (MaxRange rnge) x)))

    % Return the new (destructively modified) range.
    rnge))

% Define components for a VirtualScreen
(DefStruct (VirtualScreen fast-vector)
  MaskNumber     % A number taken from FreeMaskList when "active",
                 % negative when "inactive".
  VirtualImage   % Vector of strings giving the "screen image".

  BlankRanges    % Vector of ranges--indicating an "all blank" section of
                 % each line of the virtual screen.

  % Position of virtual cursor.  Not used for much except to position the
  % physical cursor at the topmost screen's virtual cursor.  (In
  % particular, the virtual cursor doesn't have anything to do with where
  % the last character was written.)
  ScreensCursorRow
  ScreensCursorColumn

  % Perhaps the location of a screen shouldn't be stored with the
  % screen?  These values may be NIL, when we don't really care?
  % Absolute coordinates (or, perhaps relative to "parent" screen) of upper
  % left hand corner.
  ScreensRowLocation
  ScreensColumnLocation
)

% Return the "height" of a virtual screen.
% Actually returns the maximum row number (height - 1, due to 0 indexing).
(DS VirtualScreenHeight (Scrn)
  (size (VirtualImage Scrn)))

% Return the "width" of a virtual screen.  (See above note about 0
% indexing.)
(DS VirtualScreenWidth (Scrn)
  % Return the "width" of a representative string.
  (size (igetv (VirtualImage Scrn) 0)))

(FLUID
   '(
     MaxPhysicalRow      % Dimensions of the "PhysicalScreenImage" (actual
                         % number of rows is one plus this--due to 0
                         % indexing.)
     MaxPhysicalColumn   % (That was for rows, here's for columns.)

     PhysicalScreenImage % Our idea of what's currently on the screen.

     PhysicalCursorRow   % Current location of the physical cursor.
     PhysicalCursorColumn

     NewScreenImage      % What should go there next.

     MaskInfo    % Used to handle overlapping windows.

     ChangedRowRange     % Rows on NewScreenImage will differ from those on
                         % PhysicalScreenImage only within this range.

     ChangedColumnRanges % Similar information for columns on each row.

     FreeMaskList        % Used to allocate "mask numbers".
     ActiveScreenList    % The allocated screens go onto this list.
  )
)

% Create a "screen image" (a vector of strings), filled with character
% "chr".
(DE CreateScreenImage (chr rws cols)
  (prog (result)
    (setf result (MkVect (sub1 rws)))
    (for (from i 0 (sub1 rws) 1)
      (do (setf (indexn result i) (MkString (sub1 cols) chr))))
    (return result)))

% Write a "screen image" to a channel.  (Not a "virtual screen", but the
% actual vector of strings component of a virtual screen.)
(DE WriteScreenImage (ScrnImage chn)
  (progn
    (WRS chn)    % Select the channel for output.
    (for (from i 0 (size ScrnImage) 1)
        % Write out the line, followed by a "newline".
      (do (Prin2T (indexn ScrnImage i))))

    (WRS NIL)    % Switch back to standard output.
    ))

% Initialize the screen package--allocate space for "screen image", build
% "free" and "active" list, clear the screen, etc.  Must be using "raw" I/O
% when this routine is called.
(DE InitializeScreenPackage ()
  (progn
    % Numbers for "active" virtual screens are allocated from a free screen
    % list, which gets initialized here.
    (setf FreeMaskList NIL)
    (for (from i 1 (const MaxMaskNumber) 1)
      (do (setf FreeMaskList (cons i FreeMaskList))))

    % List of active screens is initially NIL.
    (setf ActiveScreenList NIL)

    % Maximum row number for the physical screen.
    (setf MaxPhysicalRow (Row ScreenDelta))

    % System's idea of width is assumed to always be good.
    (setf MaxPhysicalColumn (Column ScreenDelta))

    (EraseScreen)        % Clear the screen.
    % Create  PhysicalScreenImage--gets a blank screen array.
    (setf PhysicalScreenImage
      (CreateScreenImage
        (char BLANK)
        (add1 MaxPhysicalRow)
        (add1 MaxPhysicalColumn)))

    % Identical sort of thing for NewScreenImage.
    (setf NewScreenImage
      (CreateScreenImage
        (char BLANK)
        (add1 MaxPhysicalRow)
        (add1 MaxPhysicalColumn)))

    % Setup "changed" information (no changes between NewScreenImage and
    % PhysicalScreenImage initially).
    % Set to an "empty range", one where minimum is >= largest possible
    % range, while maximum is <= smallest possible value.
    (setf ChangedRowRange
      (make-range
        MinRange MaxPhysicalRow
        MaxRange 0))

    % One piece of "column change" information per row.
    (setf ChangedColumnRanges (MkVect MaxPhysicalRow))

    (for (from i 0 MaxPhysicalRow 1)       % Set up each row entry.
      (do
        (setf
          (indexn ChangedColumnRanges i)
          (make-range
            MinRange MaxPhysicalColumn
            MaxRange 0))))

    % Set up the MaskInfo array, but fill with 0's (NULLS) instead of blanks.
    (setf MaskInfo
      (CreateScreenImage
        0
        (add1 MaxPhysicalRow)
        (add1 MaxPhysicalColumn)))))

% Create and return (but don't show) a new screen.  Use "SelectScreen" to
% actually display the screen.
(DE CreateVirtualScreen (rws cols CornerRow CornerCol)
  % Allocate and return the screen.
  (prog (NewVS)
    (setf NewVS
      (make-VirtualScreen
        % Don't assign a real (positive) mask number until screen is
        % activated.
        MaskNumber -1

        VirtualImage (CreateScreenImage (char BLANK) rws cols)

        BlankRanges (MkVect (sub1 rws))

        ScreensCursorRow 0       % Initially, cursor is at upper left corner.
        ScreensCursorColumn 0

        ScreensRowLocation CornerRow
        ScreensColumnLocation CornerCol))

    (for (from i 0 (sub1 rws) 1)
      (do
        (setf
          (indexn (BlankRanges NewVS) i)
          (make-range
            MinRange 0
            MaxRange (sub1 cols)))))

    (return NewVS)))

% Clear out (set to all blanks) a virtual screen.
(de ClearVirtualScreen (scrn)
  (let ((right-col (VirtualScreenWidth scrn)))
    (for (from rw 0 (VirtualScreenHeight scrn))
      (do
        (WriteToScreenRange
          scrn (char BLANK) rw 0 right-col)))))

% Return T iff the coordinates are within an "array".  (Vector of
% "vectors".)
(DE WithinArrayP (ScrnArray rw col)
  (and
    (LeQ 0 rw)
    (LeQ rw (size ScrnArray))
    (LeQ 0 col)
    (LeQ col (size (igetv ScrnArray 0)))))

% Write a character to "NewScreenImage" at some coordinate, or ignore it if
% outside the screen.  Don't check coordinates for validity, don't update
% change information--let the caller do that.  (For efficiency reasons,
% dammit.  A compiler that was smart about index calculation within loops
% would make a lot of this hacking unnecessary?)
(DS WriteToNewScreenImage (chr absrow abscol)
  % Store the character
  (setf (index_screen NewScreenImage absrow abscol) chr))
  
% "Write" a character onto a virtual screen, at location (rw, col).
% Let the character "trickle" to the "NewScreenImage" if the cell isn't
% covered.  Ignore characters that would be off the screen.
(DE WriteToScreen (Scrn chr rw col)
  (prog (absrow abscol)
    % If the new character lies on the virtual screen ...
    (cond
      % OPTIMIZE this test!!!
      ((WithinArrayP (VirtualImage Scrn) rw col)
        % Then store the new character and let it "trickle"
        (progn
          (setf (index_screen (VirtualImage Scrn) rw col) chr)

          % Update our idea of the "all blank" region on the screen.
          (cond
            ((not (equal chr (char BLANK)))
              % Character is non-blank, so shrink the range.
              (prog (BlnkRange LeftSize RightSize)
                (setf BlnkRange (igetv (BlankRanges Scrn) rw))

                % If the non-blank character falls within the blank region.
                (cond
                  ((WithinRangeP col BlnkRange)
                    (progn
                      % Find the larger of the two ranges on either side of
                      % col.
                      (setf LeftSize (difference col (MinRange BlnkRange)))
                      (setf RightSize
                        (difference (MaxRange BlnkRange) col))

                      (cond
                        ((LessP LeftSize RightSize)
                          (setf (MinRange BlnkRange) (add1 col)))
                        % Otherwise, the left range is larger.
                        (T (setf (MaxRange BlnkRange) (sub1 col))))))))))

          % Find absolute location for character
          (setf absrow (plus rw (ScreensRowLocation Scrn)))
          (setf abscol (plus col (ScreensColumnLocation Scrn)))
          (cond
            % If the character falls on the screen, and this screen is the
            % one on the top, and the character differs from what's already
            % there ...
            ((and
               (WithinArrayP MaskInfo absrow abscol)
               (equal
                 (MaskNumber Scrn)
                 (index_screen MaskInfo absrow abscol))
               (not (equal chr (index_screen NewScreenImage absrow abscol))))
              % ... then do it
              (progn
                (WriteToNewScreenImage chr absrow abscol)

                % Update the changed "range" (region?) information.  Note
                % that PutValueIntoRange is "destructive".
                (PutValueIntoRange absrow ChangedRowRange)
                (PutValueIntoRange abscol (igetv ChangedColumnRanges
                                            absrow)
                  )))))))))

% Write a character to a range of a row of a virtual screen--useful for
% (and optimized for) clearing to the end of a line.  (Not optimized for
% characters other than blank--could use some more work.)  Writes into the
% range from LeftCol to RightCol inclusive, lets things "trickle out".
(DE WriteToScreenRange (Scrn chr rw LeftCol RightCol)
  (progn

    % Ignore the call if the row is outside the screen range.
    (cond
      ((GreaterP rw (VirtualScreenHeight scrn))
        (return NIL)))

    % Clip the edges of the range to write to
    (setf LeftCol (max LeftCol 0))
    % We look at the 0'th line in (VirtualImage Scrn) to find its width.
    (setf RightCol (min RightCol (size (igetv (VirtualImage Scrn) 0))))

    (cond
      % Treat blanks specially
      ((equal chr (char BLANK))
        (prog (OldLeft OldRight BlnkRange)
          % Get the boundaries of the previous "blank range" for this line.
          (setf BlnkRange (igetv (BlankRanges Scrn) rw))

          (setf OldLeft (MinRange BlnkRange))

          (setf OldRight (MaxRange BlnkRange))

          % Write blanks out to the ranges that are not already blank (we
          % depend on "for" loops gracefully handling "empty" ranges).
          (WriteRange Scrn chr rw LeftCol (min RightCol (sub1 OldLeft)))
          (WriteRange Scrn chr rw (max LeftCol (add1 OldRight)) RightCol)

          % Update the "known blank" range.  Be "pessimistic", there may be
          % more blank than this.  (But it's to much work to make sure?)
          (setf (MinRange BlnkRange) LeftCol)

          (setf (MaxRange BlnkRange) RightCol)))

      % OTHERWISE (character isn't blank).
      (T
        (WriteRange Scrn chr rw LeftCol RightCol)))))

% Support for WriteToScreenRange.
(DE WriteRange (Scrn chr rw LeftCol RightCol)
  (for (from i LeftCol RightCol 1)
    (do
      (WriteToScreen Scrn chr rw i))))

% Refresh the "new screen image" from the active screen list, regenerating
% the mask information and "NewScreenImage".
(DE DrawActiveList ()
  (progn
    
  % Draw from "back to front".
  (foreach Scrn in (reverse ActiveScreenList) do
    (DrawScreenOnTop Scrn))))

% Draw a screen as the topmost "active" screen.  If the screen wasn't
% previously on the active list, put it there.  Otherwise, just put it at
% the front of the list.  In either case, adjust the "mask" so that the
% selected screen dominates anything else--and (re)draw the screen.
(DE SelectScreen (Scrn)
  (cond
    ((or
       % If the list is empty or the new screen on top doesn't equal the
       % current one on top...

       (null ActiveScreenList)
       (not (eq Scrn (car ActiveScreenList))))
      % ... then actually do something.  I.e. don't bother doing anything
      % if we're selecting the current topmost screen.
      (progn
        % If this screen hasn't yet been activated (assigned a mask number)
        (cond
          ((minusp (MaskNumber Scrn))
            % ... then give it one.
            (progn
             % Complain if we've run out of mask numbers.
             (cond ((null FreeMaskList)
                     (ERROR "No masks left to allocate")))
             % otherwise, assign the first free number.
             (setf
               (MaskNumber Scrn)
               (prog1
                 (car FreeMaskList)
                 (setf FreeMaskList (cdr FreeMaskList))))))

          % If it's already there, then delete the screen from its current
          % location in the list.
          (T
            (setf ActiveScreenList (DelQIP Scrn ActiveScreenList))))

        % Put the screen onto the front of the list.
        (setf ActiveScreenList (cons Scrn ActiveScreenList))
        % (re)draw the screen itself, regenerating the mask too.
        (DrawScreenOnTop Scrn)))))

% Remove a screen from the active list (and from the physical screen).
% (Do nothing if the screen isn't on the list?)
(DE DeSelectScreen (Scrn)
  (prog (AbsLeftCol AbsRightCol linewidth)
    (setf ActiveScreenList (DelQIP Scrn ActiveScreenList))

    % Make the mask number available for re-use.
    (setf FreeMaskList (cons (MaskNumber Scrn) FreeMaskList))

    % Give the screen an invalid mask number.
    (setf (MaskNumber Scrn) -1)

    (setf AbsLeftCol
      (max                  %  Absolute location of left column
        0
        (ScreensColumnLocation Scrn)))

    (setf AbsRightCol
      (min
        MaxPhysicalColumn
        (plus (VirtualScreenWidth Scrn) (ScreensColumnLocation Scrn))))

    % Line width--add one to compensate for zero indexing.
    (setf linewidth (add1 (difference AbsRightCol AbsLeftCol)))

    % Erase the virtual screen from NewScreenImage.  Also, get rid of the
    % mask.  (Being a bit sloppy and perhaps erasing stuff covering this
    % screen.)
    (for (from
           absrow
           (max 0 (ScreensRowLocation Scrn))
           (min MaxPhysicalRow
             (plus (ScreensRowLocation Scrn) (VirtualScreenHeight Scrn)))
           1)
      (do
        (progn
          % First, clear up the NewScreenImage.
          (FillSubstring
            (indexn NewScreenImage absrow) % Line to write to
            AbsLeftCol        % Lefthand column of range
            linewidth         % Number of characters to write
            (char BLANK))     % Character to write

          % Next, clear up the mask
          (FillSubstring
            (indexn MaskInfo absrow)
            AbsLeftCol
            linewidth
            0)                % Zero for no mask present.

          % Finally, fix up the "changed" information
          (PutValueIntoRange absrow ChangedRowRange)
          % Put the left margin of change into the range.
          (PutValueIntoRange AbsLeftCol (indexn ChangedColumnRanges
                                          absrow))
          % Then put the right margin into the range.
          (PutValueIntoRange
            AbsRightCol
            (indexn ChangedColumnRanges absrow)))))

    % Redraw the active stuff.
    (DrawActiveList)))

% "Draw" a virtual screen onto the top of the "new screen image",
% regenerate mask information also.
(DE DrawScreenOnTop (Scrn)
  (prog (MskNumber absrow abscol srccol lineimage linewidth)
    (setf MskNumber (MaskNumber Scrn))

    % For each row of the virtual screen ...
    (for (from i 0 (VirtualScreenHeight Scrn) 1)
      % update the screen from that row
      (do
        (progn
          (setf lineimage (indexn (VirtualImage Scrn) i))
          (setf absrow (plus i (ScreensRowLocation Scrn)))
          (cond
            % If this row is (possibly) on the physical screen ...
            ((and (LeQ 0 absrow) (LeQ absrow MaxPhysicalRow))
              % ... then update the mask, and NewScreenImage
              (progn
                % Add1 to compensate for zero indexing.
                (setf linewidth (add1 (VirtualScreenWidth Scrn)))
                (setf abscol (ScreensColumnLocation Scrn))
                % Typically source text comes starting with the leftmost part
                % of lineimage.
                (setf srccol 0)

                % Clip off anything to the left of the physical screen
                (cond
                  ((LessP abscol 0)
                    (progn
                      (setf linewidth
                        (max 0 (plus linewidth abscol)))
                      (setf srccol (minus abscol))
                      (setf abscol 0))))

                % Fill in the new mask information
                (FillSubstring
                  % Destination string, namely MaskInfo indexed by absolute
                  % row number of the screen line.
                  (indexn MaskInfo absrow)

                  abscol      % Starting location within destination string.
                  linewidth   % Number of characters.
                  MskNumber)  % The character (mask number) to fill with.

                % Copy the row on the screen to NewScreenImage.
                (MoveSubstringToFrom
                  (indexn NewScreenImage absrow)  % Destination string
                  lineimage       % Source string
                  abscol          % Destination index
                  srccol          % Source index
                  linewidth)      % number of characters to transfer

                % Update the "change information".
                (PutValueIntoRange absrow ChangedRowRange)

                % Put the left margin of change into the range.
                (PutValueIntoRange abscol (indexn ChangedColumnRanges absrow))

                % Then put the right margin into the range.
                (PutValueIntoRange
                  (min
                    (plus abscol linewidth -1)
                    MaxPhysicalColumn)
                  (indexn ChangedColumnRanges absrow))))))))))

% Redraw the physical screen so that it looks like NewScreenImage.  This is
% the routine that's responsible for minimizing the characters sent to the
% physical terminal.

% If the argument is non-NIL then it's OK to
% quit refreshing if more input is pending from the terminal (checked on
% each line).  BUT, we don't "breakout" if we're on the "current" line?
% BREAKOUT NOT IMPLEMENTED YET.
(DE RefreshPhysicalScreen (BreakoutAllowed)
  (prog (rw)

    (setf rw (MinRange ChangedRowRange))

    % Write the changed characters out to the physical screen.
    (while (and
             (LeQ rw (MaxRange ChangedRowRange))
             % **** (ZeroP (CharsInInputBuffer)) %NEEDS MORE THOUGHT!
             )
      % DO ...
      (progn
        % Call special routine to hunt down the changed characters, and
        % call WritePhysicalCharacter for each such beast.
        (RewriteChangedCharacters
          % Old line.
          (igetv PhysicalScreenImage rw)
          % New line
          (igetv NewScreenImage rw)
          % The row number
          rw
          % Leftmost change 
          (MinRange (igetv ChangedColumnRanges rw))
          % Rightmost change
          (MaxRange (igetv ChangedColumnRanges rw)))

        % Flush the output buffer after every line (even if no characters
        % sent out).
        (FlushStdOutputBuffer)

        % Reset the change information for this row--to indicate that there
        % is no difference between NewScreenImage and PhysicalScreenImage.
        (alter-range (igetv ChangedColumnRanges rw)
          MinRange MaxPhysicalColumn
          MaxRange 0)

        (incr rw)        % Advance to next row.
        ))

    % Reinitialize the "change" information to indicate that NewScreenImage
    % and PhysicalScreenImage agree--up to whatever row we reached before
    % breakout.
    (alter-range ChangedRowRange
      MinRange rw)

    % Finally--move the cursor to the spot corresponding to the topmost
    % virtual screen's cursor.

    (cond
      % If there are any active screens at all ...
      (ActiveScreenList
        % ... then move to appropriate spot.
        (prog (Scrn)
          (setf Scrn (car ActiveScreenList))
          (MoveToPhysicalLocation
            (plus (ScreensCursorRow Scrn) (ScreensRowLocation Scrn))
            (plus (ScreensCursorColumn Scrn) (ScreensColumnLocation Scrn))
            )
          % Make sure the characters actually get sent.
          (FlushStdOutputBuffer))))))

% Write a character onto the physical screen, recording the fact in
% PhysicalScreenImage.  (May want to hack "RewriteChangedCharacters" to do
% the storing into PhysicalScreenImage?)
(DE WritePhysicalCharacter (chr rw col)
  (progn
    % Move to the appropriate physical location (optimizing cursor motion).
    (MoveToPhysicalLocation rw col)
    (PBOUT chr)  % Write out the character
    % Store the new character in the image.
    (setf (index_screen PhysicalScreenImage rw col) chr)

    % Need to update our idea of the physical cursor location.
    % CURRENT CODE IS TERMINAL SPECIFIC (Teleray, maybe others).  Needs
    % to be made more modular.

    % Step our idea of where the cursor is--unless it's already
    % jammed against the right margin.
    (cond
      ((LessP PhysicalCursorColumn MaxPhysicalColumn)
        (incr PhysicalCursorColumn)))))

% Move a screen's virtual cursor to a location.  (The coordinates are
% assumed to be OK--this needs more thought! )
(DE MoveToScreenLocation (Scrn rw col)
  (progn
    (setf (ScreensCursorRow Scrn) rw)
    (setf (ScreensCursorColumn Scrn) col)))

% Move the cursor to a location on the screen, while trying to minimize the
% number of characters sent.  (The coordinates are assumed to be OK.)
(DE MoveToPhysicalLocation (rw col)
  (cond
    % Do nothing if we're already there.
    ((and (equal rw PhysicalCursorRow) (equal col PhysicalCursorColumn))
      NIL)

    % If we're on the same row and just past current position, just type
    % over what's already on the screen.
    ((and
       (equal rw PhysicalCursorRow)
       (LessP PhysicalCursorColumn col)
       (LessP col (plus PhysicalCursorColumn 4)))
      % ... then ...
      (progn
        % DOES THIS WORK when jammed against right margin?
        (for (from i PhysicalCursorColumn (sub1 col) 1)
          (do (PBOUT (index_screen PhysicalScreenImage rw i))))
        % Store our new location
        (setf PhysicalCursorColumn col)))

    % Finally, the most general case
    (T
      (progn
        (SetTerminalCursor col rw)
        (setf PhysicalCursorRow rw)
        (setf PhysicalCursorColumn col)))))

(DE ClearPhysicalScreen ()
  (progn
    (EraseScreen)        % Erase the real live terminal's screen.
    % That should move the cursor to the upper left hand corner, so reflect
    % that fact in our image of the cursor.
    (setf PhysicalCursorRow 0)
    (setf PhysicalCursorColumn 0)

    % Now clear our image of what's on the screen.
    (for (from rw 0 MaxPhysicalRow 1)
      % Fill each row with blanks.
      (do
        (FillSubstring
          (indexn PhysicalScreenImage rw)
          0        % Starting point in destination string
          (add1 MaxPhysicalColumn) % Number of characters
          (char BLANK))))   % Character code to fill with

    % Set "change info" to show the PhysicalScreenImage and NewScreenImage
    % differ, assume that the worst case holds.

    (alter-range ChangedRowRange
      MinRange 0
      MaxRange MaxPhysicalRow)

    (for (from i 0 MaxPhysicalRow 1)
      (do
        (alter-range (indexn ChangedColumnRanges i)
          MinRange 0
          MaxRange MaxPhysicalColumn)))))
