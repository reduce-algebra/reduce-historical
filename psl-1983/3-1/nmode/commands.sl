%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Commands.SL - Miscellaneous NMODE commands
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        24 August 1982
% Revised:     9 March 1983
%
% 9-Mar-83 Alan Snyder
%  Create-buffer-unselectable -> Create-Unnamed-Buffer.
% 3-Dec-82 Alan Snyder
%  Changed Insert-Self-Command to handle control- and meta- characters.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects extended-char fast-int))

% External variables used:

(fluid '(nmode-current-buffer nmode-command-argument nmode-current-window
         nmode-command-argument-given nmode-current-command
	 nmode-terminal nmode-allow-refresh-breakout
	 Text-Mode
	 ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de insert-self-command ()
  (if (FixP nmode-current-command)
    (let ((ch (x-base nmode-current-command)))
      (if (x-control? nmode-current-command)
	(let ((nch (char-upcase ch)))
	  (if (and (>= nch #/@) (<= nch #/_))
	    (setf ch (^ nch #/@))
	    )))
      (for (from i 1 nmode-command-argument)
	   (do (insert-character ch)))
      )
    % otherwise
    (Ding)
    ))

(de insert-next-character-command ()
  (nmode-append-separated-prompt "C-Q")
  (let ((ch (x-base (input-direct-terminal-character))))
    (nmode-complete-prompt (string-concat " " (x-char-name ch)))
    (for (from i 1 nmode-command-argument)
	 (do (insert-character ch)))))

(de return-command ()
  % Insert an EOL, unless we are at the end of thee current line and the
  % next line is empty.  Repeat as directed.

  (for (from i 1 nmode-command-argument)
       (do (cond ((and (at-line-end?) (not (at-buffer-end?)))
		  (move-to-next-line)
		  (cond ((not (current-line-empty?))
			 (insert-eol)
			 (move-to-previous-line)
			 )))
		 (t (insert-eol))))))

(de select-buffer-command ()
  (buffer-select (prompt-for-selectable-buffer)))

(de prompt-for-selectable-buffer ()
  (let ((default-b (=> nmode-current-buffer previous-buffer)))
    (if (and default-b (not (buffer-is-selectable? default-b)))
      (setf default-b NIL))
    (prompt-for-buffer "Select Buffer: " default-b)))

(de kill-buffer-command ()
  (let ((b (prompt-for-existing-buffer "Kill buffer: " nmode-current-buffer)))
    (if (or (not (=> b modified?))
	    (YesP "Kill unsaved buffer?"))
	(buffer-kill-and-detach b))))

(de insert-buffer-command ()
  (let ((b (prompt-for-existing-buffer "Insert Buffer:" nmode-current-buffer)))
    (insert-buffer-into-buffer b nmode-current-buffer)
    ))

(de select-previous-buffer-command ()
  (let ((old-buffer nmode-current-buffer))
    (buffer-select-previous nmode-current-buffer)
    (if (eq old-buffer nmode-current-buffer) (Ding)) % nothing visible happened
    ))

(de visit-in-other-window-command ()
  (nmode-2-windows)
  (selectq (char-upcase (input-base-character))
    (#/B (let ((b (prompt-for-selectable-buffer)))
	   (window-select-buffer (nmode-other-window) b)))
    (#/F (find-file-in-window
	  (nmode-other-window)
	  (prompt-for-file-name "Find file: " NIL)
	  ))
    (t (Ding))
    ))

(de nmode-refresh-command ()
  (if nmode-command-argument-given
    (let* ((arg nmode-command-argument)
	   (w nmode-current-window)
	   (height (=> w height))
	   (line (current-line-pos))
	   )
      (if (>= arg 0)
	  (=> w set-buffer-top (- line arg))
	  (=> w set-buffer-top (- (- line height) arg)))
      (nmode-refresh)
      )
    % Otherwise
    (=> nmode-current-window readjust-window)
    (nmode-full-refresh)
    ))

(de open-line-command ()
  (for (from i 1 nmode-command-argument)
       (do (insert-eol)
	   (move-backward)
	   )))

(de Ding ()
  (=> nmode-terminal ring-bell))

(de buffer-not-modified-command ()
  (=> nmode-current-buffer set-modified? NIL)
  )

(de set-mark-command ()
  (cond (nmode-command-argument-given
	 (buffer-set-position (current-mark))
	 (previous-mark)
	 )
	(t
	 (set-mark-from-point)
	 )))

(de mark-beginning-command ()
  (let ((old-pos (buffer-get-position)))
    (move-to-buffer-start)
    (set-mark-from-point)
    (buffer-set-position old-pos)
    ))

(de mark-end-command ()
  (let ((old-pos (buffer-get-position)))
    (move-to-buffer-end)
    (set-mark-from-point)
    (buffer-set-position old-pos)
    ))

(de transpose-characters-command ()
  (cond ((or (at-line-start?) (< (current-line-length) 2))
	 (Ding)
	 )
	(t
	 (if (at-line-end?) % We are at the end of a non-empty line.
	     (move-backward)
	     )
	 % We are in the middle of a line.
	 (let ((ch (previous-character)))
	   (delete-previous-character)
	   (move-forward)
	   (insert-character ch)
	   )
	 )))

(de mark-word-command ()
  (let ((old-pos (buffer-get-position)))
    (move-forward-word-command)
    (set-mark-from-point)
    (buffer-set-position old-pos)
    ))

(de mark-form-command ()
  (let ((old-pos (buffer-get-position)))
    (move-forward-form-command)
    (set-mark-from-point)
    (buffer-set-position old-pos)
    ))

(de mark-whole-buffer-command ()
  (move-to-buffer-end)
  (set-mark-from-point)
  (move-to-buffer-start)
  )

(de nmode-abort-command ()
  (throw 'abort NIL)
  )

(de start-scripting-command ()
  (let ((b (prompt-for-buffer "Script Input to Buffer:" NIL)))
    (nmode-script-terminal-input b)
    ))

(de stop-scripting-command ()
  (nmode-script-terminal-input nil)
  )

(de execute-buffer-command ()
  (let ((b (prompt-for-buffer "Execute from Buffer:" NIL)))
    (setf nmode-allow-refresh-breakout nmode-command-argument-given)
    (nmode-execute-buffer b)
    ))

(de execute-file-command ()
  (nmode-execute-file (prompt-for-file-name "Execute File:" NIL)))

(de nmode-execute-file (fn)
  (let ((b (create-unnamed-buffer Text-Mode)))
    (read-file-into-buffer b fn)
    (setf nmode-allow-refresh-breakout nmode-command-argument-given)
    (nmode-execute-buffer b)
    ))

(de apropos-command ()
  (let ((s (prompt-for-string
	    "Show commands whose names contain the string:"
	    NIL
	    )))
    (nmode-begin-typeout)
    (print-matching-dispatch s)
    (printf "-----")
    (nmode-end-typeout)
    ))
