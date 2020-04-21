%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Terminal-Input.SL - NMODE Terminal Input Routines
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        27 August 1982
% Revised:     14 March 1983
%
% 14-Mar-83 Alan Snyder
%  Get terminal character from physical screen, to take advantage of its
%  cached method.
% 16-Feb-83 Alan Snyder
%  Declare -> Declare-Flavor.
% 26-Jan-83 Alan Snyder
%  Add ability to read from string.
% 21-Dec-82 Alan Snyder
%  Efficiency improvement: Added declarations for text buffers.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects fast-int fast-strings))
(load wait)

% External variables used:

(fluid '(nmode-terminal
	 nmode-allow-refresh-breakout
	 nmode-physical-screen
	 ))

% Internal static variables (don't use elsewhere!):

(fluid
 '(nmode-prompt-string			% current prompt for character input
   nmode-prompt-immediately		% true => show prompt immediately
   nmode-terminal-script-buffer		% if non-NIL, is a buffer to script to
   nmode-terminal-input-buffer		% if non-NIL, is a buffer to read from
   nmode-terminal-input-string		% if non-NIL, is a string to read from
   nmode-terminal-input-string-pos	% index of next character in string
   ))

(setf nmode-prompt-string "")
(setf nmode-prompt-immediately NIL)
(setf nmode-terminal-script-buffer NIL)
(setf nmode-terminal-input-buffer NIL)
(setf nmode-terminal-input-string NIL)

(declare-flavor text-buffer
		nmode-terminal-input-buffer
		nmode-terminal-script-buffer)
(declare-flavor physical-screen nmode-physical-screen)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A primary goal of this module is to support delayed prompting.  Prompting can
% mean both echoing (some kind of confirmation) of the previous input and
% information relating to expected input.  The basic idea behind delayed
% prompting is that as long as the user is rapidly typing input characters,
% there is no need for the system to display any prompts, since the user
% probably knows what he is doing.  However, should the user ever pause for a
% "sufficiently long" time, then the current prompt should be displayed to
% inform the user of the current state.

% An important notion is that some command interactions form a logical sequence.
% In the case of a logical sequence of prompted inputs, each additional prompt
% string should be appended to the existing prompt string, without first erasing
% the prompt line.  Furthermore, once the prompt line for this sequence is
% displayed, any further prompts within the same sequence should be output
% immediately.  A command sequence is started using the function
% NMODE-SET-DELAYED-PROMPT.  Additional prompting within the same sequence is
% specified using either NMODE-APPEND-DELAYED-PROMPT or
% NMODE-APPEND-SEPARATED-PROMPT.

(de nmode-set-immediate-prompt (prompt-string)

  % This function is used to specify the beginning of a command sequence.  It
  % causes the existing prompt string to be discarded and replaced by the
  % specified string.  The specified string may be empty to indicate that the
  % new command sequence has no initial prompt.  The prompt string will be
  % output immediately upon the next request for terminal input.

  (setf nmode-prompt-string prompt-string)
  (setf nmode-prompt-immediately T)
  )

(de nmode-set-delayed-prompt (prompt-string)

  % This function is used to specify the beginning of a command sequence.  It
  % causes the existing prompt string to be discarded and replaced by the
  % specified string.  The specified string may be empty to indicate that the
  % new command sequence has no initial prompt.  The prompt string will be
  % output when terminal input is next requested, provided that the user has
  % paused.

  (setf nmode-prompt-string prompt-string)
  (setf nmode-prompt-immediately NIL)
  )

(de nmode-append-delayed-prompt (prompt-string)

  % This function is used to specify an additional prompt for the current
  % command sequence.  The prompt string will be appended to the existing prompt
  % string.  The prompt string will be output when terminal input is next
  % requested, provided that the user has paused within the current command
  % sequence.  If the prompt string is currently empty, then the user must pause
  % at some future input request to cause the prompt to be displayed.

  (setf nmode-prompt-string (string-concat nmode-prompt-string prompt-string))
  )

(de nmode-append-separated-prompt (prompt-string)

  % This function is the same as NMODE-APPEND-DELAYED-PROMPT, except that if the
  % existing prompt string is non-null, an extra space is appended before the
  % new prompt-string is appended.

  (nmode-append-delayed-prompt
   (if (not (string-empty? nmode-prompt-string))
     (string-concat " " prompt-string)
     prompt-string
     )))

(de nmode-complete-prompt (prompt-string)

  % This function is used to specify an additional prompt for the current
  % command sequence.  The prompt string will be appended to the existing prompt
  % string.  The prompt string will be output immediately, if the current prompt
  % has already been output.  This function is to be used for "completion" or
  % "echoing" of previously read input.

  (setf nmode-prompt-string (string-concat nmode-prompt-string prompt-string))
  (if nmode-prompt-immediately (write-prompt nmode-prompt-string))
  )

(de input-available? ()

  % Return Non-NIL if and only if new terminal input is available.  Note: this
  % function might be somewhat expensive.

  (or (and nmode-terminal-input-buffer
	   (not (=> nmode-terminal-input-buffer at-buffer-end?)))
      nmode-terminal-input-string
      (~= (CharsInInputBuffer) 0)))

(de input-direct-terminal-character ()

  % Prompt for (but do not echo) a single character from the terminal.  The
  % above functions are used to specify the prompt string.  Avoid displaying the
  % prompt string if the user has already typed a character or types a character
  % right away.  Within a sequence of related prompts, once a non-empty prompt
  % is output, further prompting is done immediately.

  (cond
   (nmode-terminal-input-buffer (&input-character-from-buffer))
   (nmode-terminal-input-string (&input-character-from-string))
   (t (&input-character-from-terminal))
   ))

(de &input-character-from-buffer ()

  % Internal function for reading from a buffer.

  (cond ((=> nmode-terminal-input-buffer at-buffer-end?)
	 (setf nmode-terminal-input-buffer NIL)
	 (setf nmode-allow-refresh-breakout T)
	 (input-direct-terminal-character)
	 )
	((=> nmode-terminal-input-buffer at-line-end?)
	 (=> nmode-terminal-input-buffer move-to-next-line)
	 (input-direct-terminal-character)
	 )
	(t
	 (prog1
	  (=> nmode-terminal-input-buffer next-character)
	  (=> nmode-terminal-input-buffer move-forward)
	  ))
	))

(de &input-character-from-string ()

  % Internal function for reading from a string.

  (let ((upper-bound (string-upper-bound nmode-terminal-input-string))
	(pos nmode-terminal-input-string-pos)
	)
    (cond ((= pos upper-bound)
	   (let ((ch (string-fetch nmode-terminal-input-string pos)))
	     (setf nmode-terminal-input-string NIL)
	     (setf nmode-allow-refresh-breakout T)
	     ch
	     ))
	 (t
	   (let ((ch (string-fetch nmode-terminal-input-string pos)))
	     (setf nmode-terminal-input-string-pos (+ pos 1))
	     ch
	     ))
	 )))

(de &input-character-from-terminal ()

  % Internal function for reading from the terminal.

  (let ((prompt-is-empty (string-empty? nmode-prompt-string)))
    (if (not nmode-prompt-immediately)
      (sleep-until-timeout-or-input
       (if prompt-is-empty 120 30) % don't rush to erase the prompt line
       ))
    (if (or nmode-prompt-immediately (not (input-available?)))
      (progn
       (write-prompt nmode-prompt-string)
       (setf nmode-prompt-immediately (not prompt-is-empty))
       ))
    (let ((ch (=> nmode-physical-screen get-character)))
      (if nmode-terminal-script-buffer (nmode-script-character ch))
      ch
      )))

(de pause-until-terminal-input ()

  % Return when the user has typed a character.  The character is eaten.
  % No refresh is performed.

  (=> nmode-physical-screen get-character)
  )

(de sleep-until-timeout-or-input (n-60ths)
  (wait-timeout 'input-available? n-60ths)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-script-terminal-input (b)

  % Make a script of all terminal (command) input by appending characters to the
  % specified buffer.  Supercedes any previous such request.  If B is NIL, then
  % no scripting is performed.  Note: to keep the lines of reasonable length,
  % free Newlines will be inserted from time to time.  Because of this, and
  % because many file systems cannot represent stray Newlines, the Newline
  % character is itself scripted as a CR followed by a TAB, since this is its
  % normal definition.  Someday, perhaps, this hack will be replaced by a better
  % one.

  (setf nmode-terminal-script-buffer b)
  )

(de nmode-execute-buffer (b)

  % Take input from the specified buffer.  Supercedes any previous such request.
  % If B is NIL, then input is taken from the terminal.  Newline characters are
  % ignored when reading from a buffer!

  (setf nmode-terminal-input-buffer b)
  (if b (=> b move-to-buffer-start))
  )

(de nmode-execute-string (s)

  % Take input from the specified string.  Supercedes any previous such request.
  % If S is NIL or empty, then input is taken from the terminal.

  (if (string-empty? s) (setf s NIL))
  (setf nmode-terminal-input-string s)
  (setf nmode-terminal-input-string-pos 0)
  )

(de nmode-script-character (ch)
  % Write CH to the script buffer.
  (let* ((b nmode-terminal-script-buffer)
	 (old-pos (=> b position))
	 )
    (=> b move-to-buffer-end)
    (cond ((= ch #\LF)
	   (=> b insert-character #\CR)
	   (=> b insert-character #\TAB)
	   )
	  (t (=> b insert-character ch))
	  )
    (if (>= (=> b current-line-length) 60)
      (=> b insert-eol)
      )
    (=> b set-position old-pos)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(undeclare-flavor nmode-terminal-input-buffer nmode-terminal-script-buffer)
(undeclare-flavor nmode-physical-screen)
