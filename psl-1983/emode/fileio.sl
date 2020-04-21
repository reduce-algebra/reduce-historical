%
% FILEIO.SL - Simple file I/O for EMODE.
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        27 July 1982
% Copyright (c) 1982 University of Utah
%

%%%%% Changes: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% WFG 23 August 1982
% - Split FIND_FILE to allow use as subroutine.  (Modeled after change made
% by Alan Snyder, but calls "find_file_named" instead of "find-file".)

% Copy a file from filename1 to filename2 (strings).  Currently this
% routine is only used as a test routine.
(de CopyFile (filename1 filename2)
  (let
    ((file-descriptor-1  (open filename1 'INPUT))
      (file-descriptor-2 (open filename2 'OUTPUT)))
    % Copy characters until EOF is hit
    (prog (ch)
      (while
        (neq (setf ch (ChannelReadChar file-descriptor-1)) (char EOF))
        (ChannelWriteChar file-descriptor-2 ch)))

    (close file-descriptor-1)
    (close file-descriptor-2)))

% Write an EMODE text line to a file.  (The line is a STRING.)
(de WriteLine (file-descriptor lin)
  (let ((len (size lin)))        % Number of chars in string, -1
    (for (from i 0 len)
      (do (ChannelWriteChar file-descriptor (IGetS lin i))))

    % Write an EOL (carriage return, linefeed) to end the line.
    (ChannelWriteChar file-descriptor (char EOL))))

% Read EMODE text line from file, return EOF if at end of FILE.
% NEED to make more efficient!  (But how?  The few tests I've done seem to
% show that reading is just as fast (well, within 50% or so) as
% writing--implies that single character I/O is major cost?)
(de read_line_from_file (file-descriptor)
  (prog (ch lin)
    (while
      (and
        (neq (setf ch (ChannelReadChar file-descriptor)) (char EOF))
        (neq ch (char EOL)))
      % Suck up characters until end of line (or file).
      (setf lin (cons ch lin)))

    (return
      (cond
        % Return EOF if that was read.
        ((equal ch (char EOF))
          ch)

        % Otherwise, return the line, with characters in the correct order.
        (T
          (ReversIP lin))))))

% Insert text taken from channel file-descriptor, position point at start
% of inserted text.
(de read_channel_into_text_buffer (file-descriptor)
  (prog (lin old-linepointer old-point)
    (setf old-linepointer CurrentLineIndex)
    (setf old-point point)
    (PutLine)
    (while
      (neq (setf lin (read_line_from_file file-descriptor)) (char EOF))
      (insertline lin))

    (SelectLine old-linepointer)
    (setf point old-point)))

% Write the whole of the current (text) buffer to output channel
% given by "file-descriptor".
(de write_text_buffer_to_channel (file-descriptor)
  (prog (linepointer old-linepointer old-point)
    (setf old-linepointer CurrentLineIndex)
    (setf old-point point)
    (!$BeginningOfBuffer)
    (PutLine)
    (setf linepointer CurrentLineIndex)

    (while (not (EndOfBufferP linepointer))
      (WriteLine file-descriptor (GetBufferText linepointer))
      (setf linepointer (NextIndex linepointer)))

    % Why not SelectLine?
    (GetLine old-linepointer)
    (setf point old-point)))

% Insert file into current EMODE buffer (generic version).
(de ReadFile (filename)
  % Rebind fluid !*BREAK to prevent break loop if the file OPEN fails.
  (prog (file-descriptor !*BREAK)
    (setf file-descriptor
      (ErrorSet `(open ,filename 'INPUT) T NIL))

    % Read the file in, if there were no problems in opening it.  Treat the
    % file as being of the same "data mode" as the buffer.
    (cond
      ((pairp file-descriptor)
        (apply buffers_file_reader (list (car file-descriptor)))
        (close (car file-descriptor))))))

% Write whole of current EMODE buffer to file (generic version).
(de WriteFile (filename)
  (prog (file-descriptor *BREAK)
    (setf file-descriptor
      (ErrorSet `(open ,filename 'OUTPUT) T NIL))    

    (cond
      ((pairp file-descriptor)
        (apply buffers_file_writer (list (car file-descriptor)))
        (close (car file-descriptor))
        % Announce completion in the prompt window (seems more appropriate
        % than the "message window").
        (write-prompt (concat "Written: " filename))))))

% Ask for and read a file into the current buffer.
% Uses the current buffers "buffers_file" as default, updates buffers_file.
(de CntrlXread ()
  (ReadFile
    (setf buffers_file
      (prompt_for_string "Input File: " buffers_file))))

% Ask for filename, write out the buffer to the file.
(de CntrlXwrite ()
  (WriteFile
    (setf buffers_file
      (prompt_for_string "Write File: " buffers_file))))

% Save current buffer on its associated file, ask for file if unknown.
(de save_file ()
  (cond
    (buffers_file
      (WriteFile buffers_file))
    (T
      (CntrlXwrite))))

% Ask for filename and then read it into a buffer created especially for
% that file, or select already existing buffer containing the file.
% Doesn't verify that the file actually exists.
(de find_file ()
  (find_file_named
    (prompt_for_string "Find File: " buffers_file)))

% "Find" file filename.  I.e. read it into a buffer created especially for
% that file, or select already existing buffer containing the file.
% Doesn't verify that the file actually exists.
(de find_file_named (filename)
  (prog (buffer-name)
    (cond
      % Exit immediately if NULL string for filename.
      ((LessP (size filename) 0)
        (return NIL)))
        
    (setf buffer-name (filename-buffername filename))
    (cond
      % Just select the buffer if it already exists.
      ((buffer-exists buffer-name)
        (progn
          (select_or_create_buffer buffer-name NIL)
          % Establish the keyboard bindings for the buffer.
          (EstablishCurrentMode)))

      % Otherwise, create the buffer and read in the file
      (T
        (select_or_create_buffer
          buffer-name
          (files_data_mode filename))

        (EstablishCurrentMode)
        (setf buffers_file filename)
        (ReadFile buffers_file)))))

% Convert from filename to an associated buffer name.
(de filename-buffername (filename)
  (prog (buffer-name)
    % First, hunt through current buffers to see if there's already one
    % containing the associated file.
    % NOTE this test will SCREW UP if file resides in current buffer and
    % its associated environment list hasn't been updated.
    (for (in buffer BufferNames) (while (null buffer-name))
      (do
        % If this buffer contains the filename, pick up associated
        % buffer-name.
        (cond
          ((equal filename (cdr (atsoc 'buffers_file (cdr buffer))))
            (setf buffer-name (car buffer))))))

    (return
      (cond
        % Return the buffer-name if it was found in the search.
        (buffer-name buffer-name)
        % Otherwise, create a new buffername.
        (T
          (buffer-make-unique-name
            (Intern      % ??
              (String-UpCase
                (buffer-name-field filename)))))))))

% On the Dec-20 and Unix systems a files "data mode" is derived from the
% "extension field" of it's name.  This will probably require a more
% general approach when more operating systems are used.

(fluid '(declared_file_extensions))
(setf declared_file_extensions NIL)

% Associate a buffer creator with a file extension.
(de declare_file_mode (file-extension buffer-creator)
  (setf declared_file_extensions
    (cons (cons file-extension buffer-creator) declared_file_extensions)))

(declare_file_mode "txt" 'create_text_buffer)
(declare_file_mode "red" 'create_rlisp_buffer)
(declare_file_mode "sl" 'create_lisp_buffer)

% Return the "buffer creator" appropriate to a given filename, or NIL if
% the appropriate buffer_creator (data mode) is unknown.
(de files_data_mode (filename)
  (let ((buffer-creator
          % Use "generalized atsoc" function to look up the associated
          % creator, if any.
          (Ass
            (function string-equal)
            (file-extension-field filename)
            declared_file_extensions)))
    (cond
      ((pairp buffer-creator)
        (cdr buffer-creator)))))

(if_system Dec20
  % Extract the "buffer-name field" from a filename.
  (de buffer-name-field (filename)       % Dec20 version.
    (prog (left-index right-index)
      % Bracket the subfield and then return the substring, be lazy for
      % now.
      (setf left-index 0)
      (setf right-index 0)
      % Search for a period.
      (while
        (and
          (leq right-index (size filename))
          (neq (indx filename right-index) (char !.)))
        (setf right-index (add1 right-index)))

      % "Bump" the index back one.
      (setf right-index (sub1 right-index))

      (return
        (sub filename left-index (difference right-index left-index))))))

(if_system Unix
  % Extract the "buffer-name field" from a filename.
  (de buffer-name-field (filename)       % Unix version.
    (prog (left-index right-index)
      (setf right-index (size filename))
      (setf left-index right-index)
      (while
        (and
          (geq left-index 0)
          (neq (indx filename left-index) (char !/)))
        (setf left-index (sub1 left-index)))

      % "Bump" the index one right.
      (setf left-index (add1 left-index))

      % Now, search right from the left index.
      (setf right-index left-index)
      % Search for a period.
      (while
        (and
          (leq right-index (size filename))
          (neq (indx filename right-index) (char !.)))
        (setf right-index (add1 right-index)))

      % "Bump" right-index back one.
      (setf right-index (sub1 right-index))

      (return
        (sub filename left-index (difference right-index left-index))))))

% Extract the "file extension" from a filename, should work for both Dec-20
% and Unix.
(de file-extension-field (filename)
  (prog (left-index right-index)
    % Scan from the right, looking for a period.
    (setf left-index (size filename))
    (setf right-index left-index)
    (while
      (and
        (geq left-index 0)
        (neq (indx filename left-index) (char !.)))
      (setf left-index (sub1 left-index)))

    % If no period was found, return the null string.
    (cond
      ((LessP left-index 0)
        (return ""))
      % Otherwise, return appropriate substring.
      (T
        (setf left-index (add1 left-index))      % Skip past the period.
        (return
          (sub filename left-index (difference
                                     right-index left-index)))))))
