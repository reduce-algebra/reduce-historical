%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% M-XCMD.SL - Miscellaneous Extended Commands
%
% Author:	Jeffrey Soreff
%		Hewlett-Packard/CRC
% Date:		24 January 1983
% Revised:      17 February 1983
%
% 17-Feb-83 Alan Snyder
%  Revise M-X Set Visited Filename to actualize the new file name (i.e.,
%  convert it to the true name of the file).  Revise M-X Rename Buffer to
%  convert buffer name to upper case and to check for use of an existing
%  buffer name.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(compiletime (load fast-int))

(fluid '(nmode-current-buffer))

(de delete-matching-lines-command () (delete-possibly-matching-lines nil))

(de delete-non-matching-lines-command () (delete-possibly-matching-lines t))

(de delete-possibly-matching-lines (retain-if-match)
  % This function prompts for a string which it searches for in all
  % lines including and after the current one. The search is
  % insensitive to case.  If retain-if-match is true then all lines
  % with the string will be retained and all lines lacking it will be
  % deleted, otherwise all lines with the string will be deleted.
  % Point is left at the start of the line that it was originally on.
  % This function does not return a useful value.
  (move-to-start-of-line)
  (let ((modified-flag (=> nmode-current-buffer modified?))
	(starting-line (current-line-pos))
	(next-unfilled-line (current-line-pos))
	(match-string (string-upcase
		       (prompt-for-string "Comparison String: " ""))))
    (for (from test-line starting-line (- (current-buffer-size) 1) 1)
	 (do (when
	       (if retain-if-match % This sets the sign of the selections.
		 (forward-search-on-line test-line 0 match-string)
		 (not (forward-search-on-line test-line 0 match-string)))
	       (current-buffer-store next-unfilled-line
				     (current-buffer-fetch test-line))
	       (incr next-unfilled-line))))
    (if (= next-unfilled-line (current-buffer-size)) % No lines were tossed.
      (=> nmode-current-buffer set-modified? modified-flag)
      % Else
      (extract-region t
		      (buffer-position-create next-unfilled-line 0)
		      (progn (move-to-buffer-end) (buffer-get-position))))
    (set-line-pos starting-line)))

(de count-occurrences-command ()
  % This function counts the number of instances of a string after the
  % current buffer position.  The counting is insensitive to case.
  % The user is prompted for the string.  If the user supplies an
  % empty string, they are told that it can't be counted. This avoids
  % an infinite loop.  The count obtained is displayed in the prompt
  % line. This function does not return a useful value.
  (let ((count 0)
	(initial-place (buffer-get-position))
	(match-string (string-upcase
		       (prompt-for-string "Count Occurrences: " ""))))
    (if (equal match-string "")
      (write-prompt "One can't count instances of the empty string.")
      (while (forward-search match-string)
	(incr count)
	(move-forward))
      (buffer-set-position initial-place)
      (write-prompt (bldmsg "%d occurrences" count)))))

(de set-key-command ()
  % This binds a user-selected function to a command.  The user is
  % prompted for the function name and the key sequence of the
  % command.  This function then tests to see if the user's function
  % exists, then asks for confirmation just before doing the binding.
  % This function does not return a useful value.
  (let ((function (intern (string-upcase
			   (prompt-for-string "Function Name: " "")))))
    (if (funboundp function)
      (write-prompt (bldmsg "No function %w was found." function))
      (let* ((junk (write-message (bldmsg "Put %p on key:" function)))
	     (command (input-command)))
	(when (nmode-y-or-n? (bldmsg "Load %w with %w" 
				     (command-name command) function))
	  (set-text-command command function))))))

(de set-visited-filename-command ()
  % This command allows a user to alter the filename associated with the
  % current buffer.  Prompt-for-defaulted-filename is used to set default
  % characteristics.  This function does not return a useful value.
  (let* ((new-name
	  (prompt-for-defaulted-filename "Set Visited Filename: " NIL)))
    (=> nmode-current-buffer set-file-name
	(or (actualize-file-name new-name) new-name)
	)))

(de rename-buffer-command ()
  % This function allows the user to rename the current buffer if it is not a
  % system buffer like main or output.  It prompts the user for a new buffer
  % name.  If the user inputs an empty string, the buffer name is set to a
  % converted version of the filename associated with the buffer.  Buffer
  % names are converted to upper case.  An error is reported if the user
  % chooses the name of another existing buffer.  This function does not
  % return a useful value.
  (if (not (buffer-killable? nmode-current-buffer)) % tests for main and output
    (nmode-error (bldmsg "Buffer %w cannot be renamed."
			 (=> nmode-current-buffer name)))
    (let* ((old-name (=> nmode-current-buffer name))
	   (new-name
	    (string-upcase
	     (prompt-for-string
	      "Rename Buffer: "
	      (let ((filename (=> nmode-current-buffer file-name))) % Default
		(if filename
		  (filename-to-buffername filename)
		  % Else, if there is no filename
		  (=> nmode-current-buffer name)))))))
      (when (not (string= new-name old-name))
	(if (buffer-exists? new-name)
	  (nmode-error (bldmsg "Name %w already in use." new-name))
	  (=> nmode-current-buffer set-name new-name)
	  )))))

(de kill-some-buffers-command ()
  % This functions lists the killable buffers one by one, letting the
  % user kill, retain, or examine each one as it is named. This
  % function does not return a useful value.
  (let ((buffer-list (nmode-user-buffers)))
    (while buffer-list
      (let ((buffer-to-die (car buffer-list)))
	(setf buffer-list (cdr buffer-list))
	(when (and (buffer-killable? buffer-to-die)
		   (let ((name (=> buffer-to-die name))
			 (mod-warn (if (=> buffer-to-die modified?)
				     "HAS BEEN EDITED"
				     "is unmodified")))
		     (recursive-edit-y-or-n 
		      buffer-to-die
		      (bldmsg 
		       "Buffer %w %w. Kill it? Type Y or N or ^R to edit"
		       name mod-warn)
		      (bldmsg
		       "Type Y to kill or N to save buffer %w" name))))
	  (buffer-kill-and-detach buffer-to-die))))))

(de insert-date-command ()
  % This inserts the current date into the text, after point, and
  % moves point past it.  It does not return a useful value.
  (insert-string (current-date-time)))

(de revert-file-command ()
  % This function allows the user to replace the current buffer's
  % contents with the contents of the file associated with the current
  % buffer, if there is one.  It asks for confirmation before actually
  % performing the replacement.  This function does not return a
  % useful value.
  (let ((fn (=> nmode-current-buffer file-name))
	(bn (=> nmode-current-buffer name)))
    (if (and 
	 (if fn T (write-prompt "No file to read old copy from") NIL)
	 (nmode-y-or-n? 
	  (BldMsg "Want to replace buffer %w with %w from disk?"
		  bn fn)))
      (read-file-into-buffer nmode-current-buffer fn))))
