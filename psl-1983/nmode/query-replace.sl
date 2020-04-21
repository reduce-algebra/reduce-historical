%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% QUERY-REPLACE.SL - Query/Replace command
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        6 July 1982
% Revised:     17 February 1983
%
% 17-Feb-83 Alan Snyder
%  Define backspace to be a synonym for rubout.  Terminate when a non-command
%  character is read and push back the character (like EMACS).
% 9-Feb-83 Alan Snyder
%  Must now refresh since write-message no longer does.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects extended-char fast-int fast-strings))

% Externals used here:

(fluid '(last-search-string nmode-current-buffer))

% Internal static variables:

(fluid '(query-replace-message
	 query-replace-help
	 query-replace-pause-help))

(setf query-replace-message "Query-Replace")
(setf query-replace-help
  (string-concat
   query-replace-message
   " SPACE:yes RUBOUT:no ESC:exit .:yes&exit ,:yes&show !:do all ^:back"
   ))
(setf query-replace-pause-help
  (string-concat
   query-replace-message
   " SPACE:go on ESC:exit !:do all ^:back"
   ))

(de replace-string-command ()
  (let* ((pattern
	  (setf last-search-string
	    (prompt-for-string "Replace string: " last-search-string)))
	 (replacement (prompt-for-string "Replace string with: " NIL))
	 (count 0)
	 (old-pos (buffer-get-position))
	 )
    (while (buffer-search pattern 1)
      (do-string-replacement pattern replacement)
      (setf count (+ count 1))
      )
    (buffer-set-position old-pos)
    (write-prompt (BldMsg "Number of replacements: %d" count))
    ))

(de query-replace-command ()
  (let* ((ask t)
	 ch pattern replacement
	 (pausing nil)
	 (ring-buffer (ring-buffer-create 16))
	 )

    (setf pattern
      (setf last-search-string
        (prompt-for-string
	 "Query Replace (string to replace): "
	 last-search-string
	 )))

    (setf replacement
      (prompt-for-string "Replace string with: " NIL))

    (set-message query-replace-message)
    (while (or pausing (buffer-search pattern 1))
      (if ask
        (progn
	 (cond (pausing
		(nmode-set-immediate-prompt "Command? ")
		)
	       (t
		(ring-buffer-push ring-buffer (buffer-get-position))
		(nmode-set-immediate-prompt "Replace? ")
		))
	 (nmode-refresh)
	 (setf ch (input-terminal-character))
	 (write-prompt "")
	 )
	(setf ch (x-char space)) % if not asking
	)
      (if pausing
	(selectq ch
	  ((#.(x-char space) #.(x-char rubout)
	    #.(x-char backspace) #.(x-char !,))
	   (write-message query-replace-message)
	   (setf pausing nil))
	  (#.(x-char !!)
	   (setf ask nil) (setf pausing nil))
	  ((#.(x-char escape) #.(x-char !.))
	   (exit))
	  (#.(x-char C-L)
	   (nmode-full-refresh))
	  (#.(x-char ^)
	   (ring-buffer-pop ring-buffer)
	   (buffer-set-position (ring-buffer-top ring-buffer)))
	  (#.(x-char ?)
	   (write-message query-replace-pause-help) (next))
	  (t (push-back-input-character ch) (exit))
	  )
	(selectq ch
	  (#.(x-char space)
	   (do-string-replacement pattern replacement))
	  (#.(x-char !,)
	   (do-string-replacement pattern replacement)
	   (write-message query-replace-message)
	   (setf pausing t))
          ((#.(x-char rubout) #.(x-char backspace))
	   (advance-over-string pattern))
	  (#.(x-char !!)
	   (do-string-replacement pattern replacement)
	   (setf ask nil))
	  (#.(x-char !.)
	   (do-string-replacement pattern replacement)
	   (exit))
	  (#.(x-char ?)
	   (write-message query-replace-help) (next))
	  (#.(x-char escape)
	   (exit))
	  (#.(x-char C-L)
	   (nmode-full-refresh))
	  (#.(x-char ^)
	   (ring-buffer-pop ring-buffer)
	   (buffer-set-position (ring-buffer-top ring-buffer))
	   (setf pausing t))
	  (t (push-back-input-character ch) (exit))
	  )
	)
      )
    (reset-message)
    (write-prompt "Query Replace Done.")
    ))

(de do-string-replacement (pattern replacement)

  % Both PATTERN and REPLACEMENT must be single line strings.  PATTERN is
  % assumed to be in the current buffer beginning at POINT.  It is deleted and
  % replaced with REPLACEMENT.  POINT is left pointing just past the inserted
  % text.

  (let ((old-pos (buffer-get-position)))
    (advance-over-string pattern)
    (extract-region T old-pos (buffer-get-position))
    (insert-string replacement)
    ))

(de advance-over-string (pattern)

  % PATTERN must be a single line string.  PATTERN is assumed to be in the
  % current buffer beginning at POINT.  POINT is advanced past PATTERN.

  (let ((pattern-length (string-length pattern)))
    (set-char-pos (+ (current-char-pos) pattern-length))
    ))
