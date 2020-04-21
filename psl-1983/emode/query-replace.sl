%
% QUERY-REPLACE.SL - Query/Replace command for EMODE
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        6 July 1982
%
% This file implements a query-replace command.

% Modifications by William Galway:
%   "defun" -> "de" so TAGS can find things.
%   "setq" -> "setf"

% This file requires COMMON, RING-BUFFER, BUFFER-POSITION.

(fluid '(CurrentLineIndex point CurrentWindowDescriptor Prompt_Window
          last_search_string))

(de query-replace-command ()
  (let* ((ask t)
	 ch pattern replacement
	 (pausing nil)
	 (pause-message "Command?")
	 (normal-message "Replace?")
	 (help-message
"Replace? SPACE:yes RUBOUT:no ESC:exit .:yes&exit ,:yes&show !:do all ^:back")
	 (pause-help-message
"Command? SPACE:go on ESC:exit !:do all ^:back")
	 (message normal-message)
	 (ring-buffer (ring-buffer-create 16))
	 )

    % Get string to replace.  Default is last search string (but don't
    % bother to update the default search string. (??))
    (setf pattern
      (prompt_for_string
        "Query Replace (string to replace): "
        last_search_string
        ))

    % Clear out the "default search string" message.
    (show_message "")
    (setf replacement
      (prompt_for_string "Replace string with: " NIL))

    (write-prompt "")
    (while (or pausing (buffer_search pattern 1))
      (if ask
        (progn  (if (not pausing)
		    (ring-buffer-push ring-buffer (buffer-get-position)))
		(show_message message)
		(setf ch (GetNextCommandCharacter))
		(show_message ""))
	(setf ch (char space)))
      (if pausing
	(selectq ch
	  ((#.(char space) #.(char rubout) #/,) (setf pausing nil))
	  (#/! (setf ask nil) (setf pausing nil))
	  ((#.(char escape) #/.) (exit))
	  (#.(char ff) (FullRefresh))
	  (#/^ (ring-buffer-pop ring-buffer)
	       (buffer-set-position (ring-buffer-top ring-buffer)))
	  (#/? (setf message pause-help-message) (next))
	  (t (ding))
	  )
	(selectq ch
	  (#.(char space) (do-string-replacement pattern replacement))
	  (#/, (do-string-replacement pattern replacement)
	       (setf pausing t))
          (#.(char rubout) (advance-over-string pattern))
	  (#/! (do-string-replacement pattern replacement)
		   (setf ask nil))
	  (#/. (do-string-replacement pattern replacement)
		   (exit))
	  (#/? (setf message help-message) (next))
	  (#.(char escape) (exit))
	  (#.(char ff) (FullRefresh))
	  (#/^ (ring-buffer-pop ring-buffer)
	       (buffer-set-position (ring-buffer-top ring-buffer))
	       (setf pausing t))
	  (t (ding))
	  )
	)
    (setf message (if pausing pause-message normal-message))
  )
    % Show we're done in the prompt window (to avoid "harming" message in
    % the message window).
  (write-prompt "Query Replace Done.")
  ))

(de do-string-replacement (pattern replacement)

  % Both PATTERN and REPLACEMENT must be single line strings.
  % PATTERN is assumed to be in the current buffer beginning at POINT.
  % It is deleted and replaced with REPLACEMENT.
  % POINT is left pointing just past the inserted text.

  (let ((pattern-length (add1 (size pattern))))
    (delete_or_copy T CurrentLineIndex point
		      CurrentLineIndex (+ point pattern-length))
    (insert_string replacement)
    ))

(de advance-over-string (pattern)

  % PATTERN must be a single line string.
  % PATTERN is assumed to be in the current buffer beginning at POINT.
  % POINT is advanced past PATTERN.

  (let ((pattern-length (add1 (size pattern))))
    (setf point (+ point pattern-length))
    ))

% "Write a string" into the prompt window (but don't select the prompt
% window).
(de write-prompt (string)
  (let ((old-window CurrentWindowDescriptor))
    % Show the string and select the window.
    (show_prompt string)
    % Back to original window.
    (SelectWindow old-window)))
