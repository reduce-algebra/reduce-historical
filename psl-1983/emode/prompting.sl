%
% PROMPTING.SL - "Prompting" utilities for EMODE.
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        15 July 1982
% Copyright (c) 1982 University of Utah
%

% This file provides functions for prompting the user for information, and
% for general maintenance of the "MODE", "PROMPT", and "MESSAGE" windows.

%%%%% Changes: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% AS 7/16/82
% - Delay prompting for single character input.

(FLUID
  '(previous_window         % This needs to be rethought!
    prompt-immediately      % T => prompt_for_character always prompts
    prompt-was-output       % T => prompt_for_character prompted last time
    ))

(setq prompt-immediately NIL)
(setq prompt-was-output NIL)

(de prompt_for_character (prompt_string)

  % Prompt for (and echo) a single character.  Avoid prompting if the user has
  % already typed a character or types a character right away.  The fluid
  % variables PROMPT-IMMEDIATELY and PROMPT-WAS-OUTPUT are used to implement
  % sequences of prompts, as done by C-U (for example).  Within a sequence of
  % related prompts, once a prompt is output, further prompting should be done
  % immediately.

  % Echo handling needs to do better job of handling control characters, etc.

  % First check whether a character is typed quickly.  If it is, then
  % return it directly without echoing anything.
    
  (if (not prompt-immediately) (sleep-until-timeout-or-input 30))
  (setq prompt-was-output (or prompt-immediately (= (CharsInInputBuffer) 0)))
  (if (not prompt-was-output)
      (GetNextCommandCharacter)
      % else
      (show_prompt prompt_string)          % Setup & select the prompt window.
      (let ((ch (GetNextCommandCharacter)))
        (cond
          ((MetaP ch)
           (insert_string "M-")
           (InsertCharacter (UnMeta ch)))
          (T
           (InsertCharacter ch)))
        (SelectWindow previous_window)       % Go back to old window.
        ch
        )))

% Prompt for a string (terminated by newline).  Use default_string if an
% empty string is returned, (and if default_string is non-NIL).
(de prompt_for_string (prompt_string  default_string)
  (prog (return_string old-msg-string)
    % Show the default, if non-NIL.
    (cond
      (default_string
        (setf old-msg-string 
          (show_message (concat "Default is: " default_string)))))

    % Show the prompt string, and select the "prompt window" (and buffer).
    (show_prompt prompt_string)
    % Set up mode to pick up a single line of text.
    (setf ModeEstablishExpressions '((setup_insert_single_line_mode)))

    (EstablishCurrentMode)

    % Edit the buffer until an "exit" character is typed.
    (EMODEdispatchLoop)
    % Pick up the string that was typed. 
    (setf return_string (GetBufferText CurrentLineIndex))

    % Switch back to old window, etc.
    (SelectWindow previous_window)
    % Restore original "message window label", if it was "hammered".
    % Important to do this AFTER (SelectWindow previous_window)
    (cond
      (default_string (show_message old-msg-string)))

    (EstablishCurrentMode)

    % If an empty string, use default (unless it's NIL).
    (cond
      ((and
         default_string
         (equal return_string ""))
        (setf return_string default_string)))

    (return return_string)))



% Define a mode for editing a single line of text.  Nearly identical to text
% mode.  (No 100% guarantee that a single line is all that will be put into
% the buffer, since it's possible to yank back text from the kill buffer,
% for example.)
(de setup_insert_single_line_mode ()
  (progn
    (for (from i 0 31 1)
      (do
        (setf (indx MainDispatch i) 'leave_dispatch_loop)))

    (for (from i 127 255 1)
      (do
        (setf (indx MainDispatch i) 'leave_dispatch_loop)))

    % "Normal characters" insert themselves.
    (for (from i 32 126 1)
      (do
        (MakeSelfInserting i)))

    (MakeSelfInserting (char TAB))

    % It would be nice to add some of these folks who are stolen from
    % BasicDispatchSetup.  BUT, they screw up because they invoke
    % prompt_for_character (or some such), which typically will try to grab
    % the same window that this mode is invoked in causing bad confusion.
    % We need a better method (or philosphy) for doing this.

%    (SetKey (char ESC) 'EscapeAsMeta)
%    (SetKey (char (cntrl Z)) 'DoControlMeta)

    % Make right paren "bounce" to matching left paren.
    (SetKey (char '!) ) 'insert_matching_paren)

    % Other reasonable (??) commands for editing within the line.  Includes
    % most of the features of text mode.
    (SetKey (char (cntrl '!@)) 'SetMark)
    (SetKey (char (cntrl A)) '!$BeginningOfLine)
    (SetKey (char (cntrl B)) '!$BackwardCharacter)
    (SetKey (char (cntrl D)) '!$DeleteForwardCharacter)
    (SetKey (char (cntrl E)) '!$EndOfLine)
    (SetKey (char (cntrl F)) '!$ForwardCharacter)
    (SetKey (char DELETE) '!$DeleteBackwardCharacter)
    (SetKey (char (cntrl K)) 'kill_line)
    (SetKey (char (cntrl T)) 'transpose_characters)
    (SetKey (char (cntrl Y)) 'insert_kill_buffer)
    (SetKey (char (meta (cntrl B))) 'backward_sexpr)
    (SetKey (char (meta (cntrl F))) 'forward_sexpr)
    (SetKey (char (meta (cntrl K))) 'kill_forward_sexpr)
    (SetKey (char (meta (cntrl RUBOUT))) 'kill_backward_sexpr)
    (SetKey (char (meta B)) 'backward_word)
    (SetKey (char (meta D)) 'kill_forward_word)
    (SetKey (char (meta F)) 'forward_word)
    (SetKey (char (meta W)) 'copy_region)
    (SetKey (char (meta Y)) 'unkill_previous)
    (SetKey (char (meta DELETE)) 'kill_backward_word)
    (SetKey (CharSequence (cntrl X) (cntrl X))  'ExchangePointAndMark)))

% Setup and select the prompt window, "remember" the old window in Fluid
% "previous_window".
(de show_prompt (prompt_string)
  (string_in_window  prompt_string  prompt_window))

% Display a string in the "message" window, return the previous label
% string for that window.
(de show_message (strng)
  (prog (old-label)
    (setf old-label
      (string_in_window  strng  message_window))

    % Don't stay in message window.
    (SelectWindow previous_window)
    % Refresh in order to update the cursor position
    (optional_refresh)
    (return old-label)))

% "Pop up" and select a window (typically one-line and unframed).  Use
% "strng" to label the window, clear out the associated buffer, return the
% old label string.  "Remember" the previous window in fluid previous_window.
(de string_in_window (strng  window)
  (prog (old-label)
    (setf previous_window CurrentWindowDescriptor)
    (SelectWindow window)

    (!$DeleteBuffer)     % Kill everything in the buffer

    % Save the old label and then put strng into the per-(unframed)window
    % "label" variable.
    (setf old-label window_label)
    (setf window_label strng)
    (optional_refresh)   % Let the user see it!
    (return old-label)))

