%
% Buffers.SL - Buffer Collection Manipulation Functions
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        12 July 1982
%
% Further changes by Will Galway, University of Utah.

% This file contains functions that manipulate the set of existing
% buffers.  It is intended that someday EMODE will be reorganized
% so that all such functions will eventually be in this file.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 5-Aug-82, WFG:
% Some functions moved here from EMODE1.RED, changes made to
% support arbitrary "data-modes".

(load common)

(fluid '(declared_data_modes BufferNames CurrentBufferName))

(setf declared_data_modes NIL)

% Declare (or redeclare) a "data-mode" name and associated routine for
% creating a buffer of that mode.

% Also see "declare_file_mode", used to associate data modes with filenames
% (or "file extensions").
(de declare_data_mode (name buffer-creator)
  (let ((old-decl (Ass (function string-equal) name declared_data_modes)))
    (cond
      (old-decl
        (setf (cdr old-decl) buffer-creator))
      (T
        (setf declared_data_modes
          (cons (cons name buffer-creator) declared_data_modes))))))

% Create a buffer with name given by BufferName (an identifier), using
% routine buffer-creator to create the buffer's environment.  Puts the
% (name . environment) pair into "BufferNames" alist, returns the
% environment.
(de CreateBuffer (BufferName buffer-creator)
  (cond
    ((atsoc BufferName BufferNames)
      % Complain if the buffer already exists.
      (EMODEError (list "Buffer" BufferName "exists")))
    % Otherwise, enter the (name . environment) pair into the association
    % list of buffers.
    (T
      (let ((env (apply buffer-creator NIL)))
        (setf BufferNames
          (cons (cons BufferName env) BufferNames))
        env))))

% Switch to a new current buffer, creating it if necessary.  (But without
% establishing that buffer's keyboard bindings.)  Use buffer-creator to
% create the buffer, or ask the user for a hint if buffer-creator is NIL.
% Create a "view" of the selected buffer, "destroying" the "current view".

% NEED TO contrast this with "SelectBuffer", which (in effect) gives us an
% "invisible view" (or "internal view"?) of a buffer?  (A "view" to be used
% for internal purposes, rather than for use from the keyboard.)
(de select_or_create_buffer (buffer-name buffer-creator)
  (cond
    % Don't do anything if trying to select the "current buffer".
    ((not (eq buffer-name CurrentBufferName))
      (prog (new-env)
        (return
          (cond
            % Just select the buffer if it's already present.
            ((setf new-env (atsoc buffer-name BufferNames))
              (setf new-env (cdr new-env))       % get cdr of (name . env)

              % Now "look into" the newly selected buffer.
              % Get rid of the current "view", replace it with the new
              % view.  Go through fancy foot work to create new view in
              % context of current view.
              (let ((new-view
                      (apply
                        (cdr (atsoc 'buffers_view_creator new-env))
                        (list buffer-name))))

                (remove_current_view)
                (SelectWindow new-view)))

            % Otherwise, create the new buffer if not already around.
            (T
              (while (null buffer-creator)
                (let
                  ((mode-name
                     (prompt_for_string
                       (BldMsg "Mode for buffer %w: " buffer-name)
                       % Default mode-name is "text", should this be
                       % parameterized?
                       "text"
                       )))

                  % Use "generalized assoc" function to look up the
                  % associated creator, if any.
                  (setf buffer-creator
                    (Ass
                      (function string-equal)
                      mode-name
                      declared_data_modes))

                  % "Beep" if unknown mode-name (and ask again).
                  (cond
                    ((null buffer-creator) (ding))
                    % Otherwise, extract "good part" of (mode-name .
                    % buffer-creator) pair.
                    (T
                      (setf buffer-creator (cdr buffer-creator))))))

              (show_message (BldMsg "Creating buffer %w" buffer-name))
              (setf new-env (CreateBuffer buffer-name buffer-creator))

              % Get rid of the current "view", replace it with the new view.
              (let ((new-view
                      (apply
                        (cdr (atsoc 'buffers_view_creator new-env))
                        (list buffer-name))))

                (remove_current_view)

                (SelectWindow new-view)))))))))

% "Choose" a buffer (name taken from keyboard), make it the current buffer
% and establish its mode as the current mode.
(de ChooseBuffer ()
  (let
    ((buffer-name
       (String-UpCase (prompt_for_string "Buffer Name: "
                        last_buffername))))

    % Strings with 1 character have size 0, avoid creating something with
    % the empty string for a name!
    (cond
      ((Geq (size buffer-name) 0)
        % Set up new default buffername for next ChooseBuffer.
        (setf last_buffername (Id2String CurrentBufferName))
        (select_or_create_buffer (intern buffer-name) NIL)
        (EstablishCurrentMode)))))

% Create a (default) "view" (or "window") into a text buffer.  Details of
% the window location (etc?) depend on the current window layout.
(de create_text_view (buffer-name)
  (cond
    % If the current buffer also uses a "text view".
    ((eq buffers_view_creator (function create_text_view))
      % Just modify (destructively) the current "view" (or "window")
      % environment to look into the new buffer, return the current
      % environment.
      (SelectBuffer buffer-name)
      % Let window know what buffer it's looking into (wierd)!
      (setf WindowsBufferName buffer-name)
      % Save (and return) the current "view" environment.
      (SaveEnv CurrentWindowDescriptor))
    % Otherwise (if current view isn't into "text"), create a framed window
    % of an appropriate size and at an appropriate location.
    % (For lack of a better idea, just use a window like that used by "two
    % window" mode.)
    (T
      % Make sure two_window_midpoint is a reasonable value.
      (cond
        ((or
           (not (numberp two_window_midpoint))
           (LessP two_window_midpoint 3)
           (GreaterP two_window_midpoint (difference (row ScreenDelta) 5)))
          (setf two_window_midpoint
            (fix (times 0.5 (difference (row ScreenDelta) 2))))))

      (FramedWindowDescriptor
        buffer-name
        % Upper left corner
        (coords
          (sub1 (Column ScreenBase))
          (plus (Row ScreenBase) two_window_midpoint 1))
        (coords
          (plus 2 (Column ScreenDelta))
          (plus (difference (row ScreenDelta) two_window_midpoint) -2))))))

% Declare the routine for creating "text mode" buffers.
(declare_data_mode "text" 'create_text_buffer)

% Return the environment for a "raw" text buffer (everything except
% keyboard bindings).
(de create_raw_text_buffer ()
  % Environment bindings for this buffer.
  % May prefer to use backquote to do this, but current version is buggy
  % for lists of the form `( (a .b) ).  Also, it's important not to share
  % any substructure with other alists built by this routine.
  (list
    % The following 4 "per buffer" variables should be defined for a buffer
    % of any "data mode".  Also need to define ModeEstablishExpressions,
    % but that's left to the caller of this routine.
    (cons 'buffers_view_creator  'create_text_view)
    (cons 'buffers_file_reader  'read_channel_into_text_buffer)
    (cons 'buffers_file_writer  'write_text_buffer_to_channel)
    (cons 'buffers_file  NIL)    % Name of file associated with buffer.

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

% Create a text buffer--uses "raw text" environment "plus" keyboard
% bindings appropriate for "text".
(de create_text_buffer ()
  (cons
    (cons 'ModeEstablishExpressions  FundamentalTextMode)
    (create_raw_text_buffer)))


(declare_data_mode "rlisp" 'create_rlisp_buffer)

(declare_data_mode "lisp" 'create_lisp_buffer)

% Return the environment for a new "Rlisp" buffer.
(de create_rlisp_buffer ()
  % Same as "text buffer" but with a different keyboard dispatch table.
  (cons
    (cons 'ModeEstablishExpressions RlispMode)
    (create_raw_text_buffer)))

% Return the environment for a new "lisp" buffer.
(de create_lisp_buffer ()
  (cons
    (cons 'ModeEstablishExpressions LispMode)
    (create_raw_text_buffer)))

(de buffer-create (buffer-name buffer-creator)

  % Create a new buffer.  The name of the new buffer will be the specified name
  % if no buffer already exists with that name.  Otherwise, a similar name will
  % be chosen.  The actual buffer name is returned.  The buffer is not
  % selected.

  (setq buffer-name (buffer-make-unique-name buffer-name))
  (CreateBuffer buffer-name buffer-creator)
  buffer-name
  )

(de buffer-make-unique-name (buffer-name)
  % Return a buffer name not equal to the name of any existing buffer.

  (for*
    (with (root-name (string-concat (id2string buffer-name) "-")))
    (for count 0 (+ count 1))
    (for name buffer-name
	      (intern (string-concat root-name (BldMsg "%d" count))))
    (do (if (not (buffer-exists name)) (exit name)))
    ))

(de buffer-exists (buffer-name)
  (atsoc buffer-name BufferNames))

(de buffer-kill (buffer-name)
  (if (and (buffer-exists buffer-name) (> (length BufferNames) 1))
    (progn
      (setq BufferNames (DelatQ buffer-name BufferNames))
      (if (eq CurrentBufferName buffer-name)
	(progn (setq CurrentBufferName nil)
	       (SelectBuffer (car (car BufferNames)))))
      (if (eq WindowsBufferName buffer-name)
        (setq WindowsBufferName CurrentBufferName))
      ))

  )

(de select-buffer-if-existing (buffer-name)
  % This function will select and establish the specified buffer, if it exists.
  % Otherwise, it will select and establish an arbitrary existing buffer.

  (prog (buffer-env)
    (if (setq buffer-env (atsoc buffer-name BufferNames))
      (setq buffer-env (cdr buffer-env))
      (if (setq buffer-env (atsoc 'MAIN BufferNames))
	(progn (setq buffer-name 'MAIN) (setq buffer-env (cdr buffer-env)))
	(progn
	      (setq buffer-name (car (car BufferNames)))
	      (setq buffer-env (cdr (car BufferNames)))
	      )
	))
    (if CurrentBufferName (DeSelectBuffer CurrentBufferName))
    (RestoreEnv buffer-env)
    (setq CurrentBufferName buffer-name)
    (EstablishCurrentMode)
    ))
