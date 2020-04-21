%
% MISC-EMODE.SL - Miscellaneous EMODE routines
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        29 July 1982
% Copyright (c) 1982 University of Utah
%

% Get a "command" (lisp expression) and "execute" (evaluate) it.
% This routine is meant to be bound to the M-X key.
(de execute_command ()
  (let ((old-channels (save-important-channels)))
    (SelectEmodeChannels)

    % Do we need some sort of ErrorSet here?
    (eval
      (read_from_string
        (prompt_for_string "M-X " NIL)))

    (restore-important-channels old-channels)))

% Insert the next character "typed".
(de InsertNextCharacter ()
  (InsertCharacter (GetNextCommandCharacter)))

% Display a list of all the buffers known to EMODE.
% This needs to be redone to fit better with current window/virtual screen
% package.
(de PrintBufferNames ()
  (let ((old-channels (save-important-channels)))

    % Make sure that output goes to "EMODE output" channel.
    (SelectEmodeChannels)

    (for (in buffer-name BufferNames)
      (do
        % car gives name of (name . environment) pair.
        (prin2t (car buffer-name))))

    (restore-important-channels old-channels)))
  
% Return a list of the current "important" channel bindings.
(de save-important-channels ()
  (list STDIN* STDOUT* ErrOut*))

% "Restore" the channels saved by save-important-channels.
(de restore-important-channels (saved-channels)
  (progn
    (setf STDIN* (car saved-channels))
    (setf STDOUT* (cadr saved-channels))
    (setf ErrOut* (caddr saved-channels))
    (RDS STDIN*)
    (WRS STDOUT*)))
