%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Prompting.SL - NMODE Prompt Line Manager
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        19 August 1982
% Revised:     28 February 1983
%
% Adapted from Will Galway's EMODE.
%
% 28-Feb-83 Alan Snyder
%   Extend write-prompt to work properly when NMODE is not running.
% 16-Feb-83 Alan Snyder
%   Declare -> Declare-Flavor.
% 7-Feb-83 Alan Snyder
%   Use one-window or one-screen refresh.
% 29-Dec-82 Alan Snyder
%   Revised input completion support to run completion characters as commands
%   rather than terminating and resuming.  Added new functions to manipulate the
%   input buffer.
% 22-Dec-82 Jeffrey Soreff
%   Revised to handle control characters on prompt and message lines.
% 21-Dec-82 Alan Snyder
%   Efficiency improvement: Added declarations for virtual screens and buffer
%   windows.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects extended-char fast-strings numeric-operators))
(on fast-integers)

% External variables used:

(fluid
 '(nmode-prompt-screen
   nmode-message-screen
   nmode-input-window
   nmode-current-window
   *NMODE-RUNNING
   ))

% Global variables defined here:

(fluid
 '(nmode-input-default
   ))

% Internal static variables:

(fluid
 '(nmode-prompt-cursor
   nmode-message-cursor
   nmode-message-string
   nmode-input-level
   nmode-input-special-command-list
   ))

(setf nmode-prompt-cursor 0)
(setf nmode-message-cursor 0)
(setf nmode-message-string "")
(setf nmode-input-level 0)
(setf nmode-input-default NIL)

(declare-flavor virtual-screen nmode-prompt-screen nmode-message-screen)
(declare-flavor buffer-window nmode-input-window nmode-current-window)
(declare-flavor text-buffer input-buffer)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% String input:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de prompt-for-string (prompt-string default-string)

  % Prompt for a string (terminated by CR or NL).  Use default-string if an
  % empty string is returned (and default-string is non-NIL).  The original
  % message line is restored, but not refreshed.  Note: if you attempt to use
  % this function recursively, it will automatically throw '$ERROR$.  The effect
  % of this action is that in string-input mode, commands that request string
  % input appear to be undefined.  (This assumes that all such commands do
  % nothing visible before they first request string input.)

  (prompt-for-string-special prompt-string default-string NIL))

(de prompt-for-string-special (prompt-string default-string command-list)

  % This function is similar to PROMPT-FOR-STRING, except that it accepts a
  % command list that specifies a set of additional commands to be defined
  % while the user is typing at the input window.

  (if (> nmode-input-level 0)
    (throw '$error$ NIL)
    % else
    (setf nmode-input-special-command-list command-list)
    (setf nmode-input-default default-string)
    (let ((old-msg nmode-message-string)
	  (old-window nmode-current-window)
	  (nmode-input-level (+ nmode-input-level 1)) % FLUID
	  )
      (if default-string
	(setf prompt-string
	  (string-concat prompt-string " (Default is: '" default-string "')")))

      (=> (=> nmode-input-window buffer) reset)
      (nmode-select-window nmode-input-window)
      (set-message prompt-string)
      (set-prompt "") % avoid old prompt popping back up when we're done

      % Edit the buffer until an "exit" character is typed or the user aborts.

      (cond ((eq (NMODE-reader T) 'abort)
	     (=> nmode-input-window deexpose)
	     (nmode-select-window old-window)
	     (set-message old-msg)
	     (throw 'abort NIL)
	     ))

      % Show the user that his input has been accepted.
      (move-to-start-of-line)
      (nmode-refresh-one-window nmode-input-window)

      % Pick up the string that was typed. 
      (let ((return-string (current-line)))

	% Switch back to old window, etc.
	(=> nmode-input-window deexpose)
	(nmode-select-window old-window)

	% Restore original "message window".
	(set-message old-msg)

	% If an empty string, use default (unless it's NIL).
	(if (and default-string (equal return-string ""))
	  default-string
	  return-string
	  )))))

(de nmode-substitute-default-input ()
  % If the input buffer is empty and there is a default string, then stuff the
  % default string into the input buffer.

  (let ((input-buffer (=> nmode-input-window buffer)))
    (if (and (=> input-buffer at-buffer-start?)
	     (=> input-buffer at-buffer-end?)
	     nmode-input-default
	     (stringp nmode-input-default)
	     )
      (=> input-buffer insert-string nmode-input-default)
      )))

(de nmode-get-input-string ()
  % Return the contents of the input buffer as a string.  If the buffer contains
  % more than one line, only the current line is returned.

  (let ((input-buffer (=> nmode-input-window buffer)))
    (=> input-buffer current-line)
    ))

(de nmode-replace-input-string (s)
  % Replace the contents of the input buffer with the specified string.
  (let ((input-buffer (=> nmode-input-window buffer)))
    (=> input-buffer reset)
    (=> input-buffer insert-string s)
    ))

(de nmode-terminate-input ()
  % A command bound to this function will act to terminate string input.
  (exit-nmode-reader)
  )

(de nmode-yank-default-input ()
  % A command bound to this function will act to insert the default string into
  % the input buffer.
  (if nmode-input-default
    (insert-string nmode-input-default)
    (Ding)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prompt line functions:
%
% NOTE: if your intent is to display a prompt string for user input, you should
% use a function defined in TERMINAL-INPUT rather than one of these.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de write-prompt (msg)
  % Write the specified string to the prompt line and refresh the prompt
  % line.  Note: the major windows are not refreshed.

  (cond
   (*NMODE-RUNNING
    (set-prompt msg)
    (nmode-refresh-virtual-screen nmode-prompt-screen)
    )
   (t
    (printf "%w%n" msg)
    )))

(de set-prompt (msg)
  % Write the specified string to the prompt window, but do not refresh.
  (setf nmode-prompt-cursor 0)
  (=> nmode-prompt-screen clear)
  (prompt-append-string msg)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Message line functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de write-message (msg)
  % Display a string to the message window and refresh the message window.
  % Note: the major windows are not refreshed.
  % Return the previous message string.

  (prog1
   (set-message msg)
   (nmode-refresh-virtual-screen nmode-message-screen)
   ))

(de rewrite-message ()
  % Rewrite the existing message (used when the default enhancement changes).
  (set-message nmode-message-string)
  )

(de set-message (msg)
  % Display a string in the "message" window, do not refresh.
  % Message will not appear until a refresh is done.
  % Return the previous message string.

  (let ((old-message nmode-message-string))
    (setf nmode-message-string msg)
    (setf nmode-message-cursor 0)
    (=> nmode-message-screen clear)
    (message-append-string msg)
    old-message
    ))

(de reset-message ()
  % Clear the "message" window, but do not refresh.
  (setf nmode-message-string "")
  (setf nmode-message-cursor 0)
  (=> nmode-message-screen clear)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de prompt-append-string (s)
  (for (from i 0 (string-upper-bound s))
       (do (prompt-append-character (string-fetch s i)))))

(de prompt-append-character (ch)
  (cond 
   ((or (< ch #\space) (= ch #\rubout)) % Control Characters
    (=> nmode-prompt-screen write #/^ 0 nmode-prompt-cursor)
    (setf nmode-prompt-cursor (+ nmode-prompt-cursor 1))
    (=> nmode-prompt-screen write (^ ch 8#100) 0 nmode-prompt-cursor)
    (setf nmode-prompt-cursor (+ nmode-prompt-cursor 1)))
   (t (=> nmode-prompt-screen write ch 0 nmode-prompt-cursor) % Normal Char
      (setf nmode-prompt-cursor (+ nmode-prompt-cursor 1)))))

(de message-append-string (s)
  (for (from i 0 (string-upper-bound s))
       (do (message-append-character (string-fetch s i)))))

(de message-append-character (ch)
  (cond 
   ((or (< ch #\space) (= ch #\rubout)) % Control Characters
    (=> nmode-message-screen write #/^ 0 nmode-message-cursor)
    (setf nmode-message-cursor (+ nmode-message-cursor 1))
    (=> nmode-message-screen write (^ ch 8#100) 0 nmode-message-cursor)
    (setf nmode-message-cursor (+ nmode-message-cursor 1)))
   (t (=> nmode-message-screen write ch 0 nmode-message-cursor) % Normal Char
      (setf nmode-message-cursor (+ nmode-message-cursor 1)))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(undeclare-flavor nmode-prompt-screen nmode-message-screen)
(undeclare-flavor nmode-input-window nmode-current-window)
(undeclare-flavor input-buffer)
