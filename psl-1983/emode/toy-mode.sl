%
% TOY-MODE.SL - A "toy" to demonstrate a "non-text" data mode
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        12 August 1982
% Copyright (c) 1982 University of Utah
%
% In reality, this is really the same as text, but with a different refresh
% algorithm.
% Need to fix clear window problems at creation time, plus misc clear to
% end of line problems plus onewindow/twowindow problems.

(load nstruct)

(declare_data_mode "toy" 'create_toy_buffer)

% Taken from "create_text_buffer"
(de create_toy_buffer ()
  % Environment bindings for this buffer.
  % May prefer to use backquote to do this, but current version is buggy
  % for lists of the form `( (a .b) ).  Also, it's important not to share
  % any substructure with other alists built by this routine.
  (list
    % The following 5 "per buffer" variables should be defined for a buffer
    % of any "data mode".
    (cons 'buffers_view_creator  'create_toy_view)
    (cons 'buffers_file_reader  'read_channel_into_text_buffer)
    (cons 'buffers_file_writer  'write_text_buffer_to_channel)
    (cons 'buffers_file  NIL)    % Name of file associated with buffer.
    (cons 'ModeEstablishExpressions  RlispMode)

    % Variables unique to "text data mode" follow.
    % Initial vector allows only one line.  (Should really be parameterized
    % somehow?)
    (cons 'CurrentBufferText (MkVect 0)) % 0 is upper bound, one element.

    (cons 'CurrentBufferSize  1) % Start with one line of text (but zero
                                 % characters in the line! )
    (cons 'CurrentLine  NIL)
    (cons 'CurrentLineIndex  0)
    (cons 'point  0)
    % MarkLineIndex corresponds to CurrentLineIndex, but for "mark".
    (cons 'MarkLineIndex  0)
    (cons 'MarkPoint  0) % Corresponds to "point".
    ))

% Modified from "create_text_view"
(de create_toy_view (buffer-name)
  (cond
    % If the current buffer also uses a "toy view" or "text view" (hum,
    % needs more work--not very modular! )
    ((memq buffers_view_creator
       '(create_text_view  create_toy_view))

      % Just modify (destructively) the current "view" (or "window")
      % environment to look into the new buffer, use the proper refresh
      % algorithm, return the current environment.
      (SelectBuffer buffer-name)
      % Let window know what buffer it's looking into (wierd)!
      (setf WindowsBufferName buffer-name)
      (setf windows_refresher (function refresh_toy_window))
      % Make sure the virtual screen is properly cleared and framed.
      (ClearVirtualScreen CurrentVirtualScreen)
      (FrameScreen CurrentVirtualScreen)

      % Save (and return) the current "view" environment.
      (SaveEnv CurrentWindowDescriptor))

    % Otherwise (if current view isn't into "text" or "toy"), create a
    % framed window of an appropriate size and at an appropriate location.
    % (For lack of a better idea, just use a large window taking up most of
    % the screen--same as provided by "OneWindow".)
    (T
      (let
        ((new-view
           (FramedWindowDescriptor
             buffer-name
             % Upper left corner
             (coords (sub1 (Column ScreenBase)) (sub1 (Row ScreenBase)))
             % Size of window uses entire width of screen, leaves room for two
             % one line windows at bottom of screen.
             (coords (plus 2 (Column ScreenDelta)) (sub1 (Row ScreenDelta)))
             )))
        (setf (cdr (atsoc 'windows_refresher new-view))
          (function refresh_toy_window))

        new-view))))

(fluid '(row_offset column_offset))

% Taken from refresh_framed_window.
(de refresh_toy_window ()
  (progn
    (setf row_offset 1)
    (setf column_offset 1)
    (quietly_copyd 'original-WriteToScreen 'WriteToScreen)
    (quietly_copyd 'WriteToScreen 'backwards-WriteToScreen)
    (refresh_text)

    (quietly_copyd 'WriteToScreen 'original-WriteToScreen)

    (refresh_frame_label)

    (MoveToScreenLocation
      CurrentVirtualScreen
      (plus
        row_offset (CountLinesFrom TopOfDisplayIndex CurrentLineIndex))
      (difference
        (VirtualScreenWidth CurrentVirtualScreen)
        (plus
          column_offset
          (difference
            (LineColumn point CurrentLine)
            ShiftDisplayColumn))))))

(de backwards-WriteToScreen (Scrn chr rw col)
  (original-WriteToScreen
    Scrn
    chr
    rw
    (difference (VirtualScreenWidth Scrn) col)))

(de quietly_copyd (dest src)
  (let ((*USERMODE NIL) (*REDEFMSG NIL))
    (copyd dest src)))

(de quietly_putd (fname ftype body)
  (let ((*USERMODE NIL) (*REDEFMSG NIL))
    (putd fname ftype body)))
