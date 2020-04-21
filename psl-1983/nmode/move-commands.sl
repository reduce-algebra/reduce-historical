%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Move-Commands.SL - NMODE Move commands
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        23 August 1982
% Revised:     17 February 1983
%
% 17-Feb-83 Alan Snyder
%   Bug fix: permanent goal column wasn't permanent.
% 18-Nov-82 Alan Snyder
%   Added move-up-list, move-over-list, and move-over-defun commands.
%   Changed skip-forward-blanks and skip-backward-blanks.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects fast-int))

(fluid '(nmode-current-buffer
         nmode-command-argument
	 nmode-command-argument-given
         nmode-previous-command-function))

% Internal static variables:

(fluid '(nmode-goal-column		% permanent goal (set by user)
	 nmode-temporary-goal-column	% temporary goal within cmd sequence
	 nmode-goal-column-functions	% cmds that don't reset temp goal
	 ))

(setf nmode-goal-column nil)
(setf nmode-temporary-goal-column nil)
(setf nmode-goal-column-functions
  (list
   (function move-down-command)
   (function move-down-extending-command)
   (function move-up-command)
   (function set-goal-column-command)
   ))

(de move-to-buffer-start-command ()
  (set-mark-from-point)
  (move-to-buffer-start)
  )

(de move-to-buffer-end-command ()
  (set-mark-from-point)
  (move-to-buffer-end)
  )

(de move-to-start-of-line-command ()
  (current-buffer-goto (+ (current-line-pos) (- nmode-command-argument 1)) 0)
  )

(de move-to-end-of-line-command ()
  (move-to-start-of-line-command)
  (move-to-end-of-line))

(de set-goal-column-command ()
  (cond ((= nmode-command-argument 1)
	 (setf nmode-goal-column (current-display-column))
	 (write-prompt (BldMsg "Goal Column = %p" nmode-goal-column))
	 )
	(t
	 (setf nmode-goal-column NIL)
	 (write-prompt "No Goal Column")
	 )))

(de setup-goal-column ()
  % If this is the first in a new (potential) sequence of up/down commands,
  % then set the temporary goal column for that sequence of commands.
  (if (not (memq nmode-previous-command-function nmode-goal-column-functions))
    (setf nmode-temporary-goal-column (current-display-column)))
  )

(de goto-goal-column ()
  % Move the cursor to the current goal column, which is the permanent goal
  % column (if set by the user) or the temporary goal column (otherwise).
  (cond (nmode-goal-column
	 (set-display-column nmode-goal-column))
	(nmode-temporary-goal-column
	 (set-display-column nmode-temporary-goal-column))
	))

(de move-up-command ()
  (setup-goal-column)
  (set-line-pos (- (current-line-pos) nmode-command-argument))
  (goto-goal-column)
  )

(de move-down-extending-command ()
  (when (and (not nmode-command-argument-given) (current-line-is-last?))
    (let ((old-pos (buffer-get-position)))
      (move-to-buffer-end)
      (insert-eol)
      (buffer-set-position old-pos)
      ))
  (move-down-command)
  )

(de move-down-command ()
  (setup-goal-column)
  (set-line-pos (+ (current-line-pos) nmode-command-argument))
  (goto-goal-column)
  )

(de exchange-point-and-mark ()
  (let ((old-mark (current-mark)))
    (previous-mark) % pop off the old mark
    (set-mark-from-point) % push the new one
    (buffer-set-position old-mark)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Skipping Blanks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de char-blank-or-newline? (ch)
  (or (char-blank? ch) (= ch #\LF)))

(de skip-forward-blanks ()
  % Skip over "blanks", return the first non-blank character seen.
  % Cursor is positioned to the left of that character.
  (while (and (not (at-buffer-end?))
	      (char-blank-or-newline? (next-character))
	      )
    (move-forward))
  (next-character))

(de skip-backward-blanks ()
  % Skip backwards over "blanks", return the first non-blank character seen.
  % Cursor is positioned to the right of that character.
  (while (and (not (at-buffer-start?))
	      (char-blank-or-newline? (previous-character))
	      )
    (move-backward))
  (previous-character))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Move-Over-Characters commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de move-forward-character-command ()
  (if (not (move-over-characters nmode-command-argument))
    (Ding)))

(de move-backward-character-command ()
  (if (not (move-over-characters (- nmode-command-argument)))
    (Ding)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Move-Over-Word commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de move-forward-word-command ()
  (if (not (move-over-words nmode-command-argument))
    (Ding)))

(de move-backward-word-command ()
  (if (not (move-over-words (- nmode-command-argument)))
    (Ding)))

(de move-over-words (n)
  % Move forward (n>0) or backwards (n<0) over |n| words.  Return T if the
  % specified number of words were found, NIL otherwise.  The cursor remains at
  % the last word found.

  (let ((flag T))
    (while (and (> n 0) (setf flag (move-forward-word)))
      (setf n (- n 1)))
    (while (and (< n 0) (setf flag (move-backward-word)))
      (setf n (+ n 1)))
    flag))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Move-Over-Form commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de move-forward-form-command ()
  (if (not (move-over-forms nmode-command-argument))
    (Ding)))

(de move-backward-form-command ()
  (if (not (move-over-forms (- nmode-command-argument)))
    (Ding)))

(de move-over-forms (n)
  % Move forward (n>0) or backwards (n<0) over |n| forms.  Return T if the
  % specified number of forms were found, NIL otherwise.  The cursor remains at
  % the last form found.

  (let ((flag T))
    (while (and (> n 0) (setf flag (move-forward-form)))
      (setf n (- n 1)))
    (while (and (< n 0) (setf flag (move-backward-form)))
      (setf n (+ n 1)))
    flag))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Move-Up-List commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de forward-up-list-command ()
  (if (not (move-up-lists nmode-command-argument))
    (Ding)))

(de backward-up-list-command ()
  (if (not (move-up-lists (- nmode-command-argument)))
    (Ding)))

(de move-up-lists (n)
  % Move forward (n>0) or backwards (n<0) out of |n| lists (structures).
  % Return T if the specified number of brackets were found, NIL otherwise.
  % The cursor remains at the last bracket found.

  (let ((flag T))
    (while (and (> n 0) (setf flag (move-forward-up-list)))
      (setf n (- n 1)))
    (while (and (< n 0) (setf flag (move-backward-up-list)))
      (setf n (+ n 1)))
    flag
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Move-Over-List commands
%
% Note: In EMACS, these commands were motivated by the fact that EMACS did
% not understand Lisp comments.  Thus, in EMACS, move-forward-list could be
% used as a move-forward-form that ignored comments.  Since NMODE does
% understand comments, it is not clear that these commands have any use.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de move-forward-list-command ()
  (if (not (move-over-lists nmode-command-argument))
    (Ding)))

(de move-backward-list-command ()
  (if (not (move-over-lists (- nmode-command-argument)))
    (Ding)))

(de move-over-lists (n)
  % Move forward (n>0) or backwards (n<0) over |n| lists (structures).
  % Return T if the specified number of lists were found, NIL otherwise.
  % The cursor remains at the last list found.

  (let ((flag T))
    (while (and (> n 0) (setf flag (move-forward-list)))
      (setf n (- n 1)))
    (while (and (< n 0) (setf flag (move-backward-list)))
      (setf n (+ n 1)))
    flag
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Move-Over-Defun commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de move-forward-defun-command ()
  (if (not (move-over-defuns nmode-command-argument))
    (Ding)))

(de move-backward-defun-command ()
  (if (not (move-over-defuns (- nmode-command-argument)))
    (Ding)))

(de move-over-defuns (n)
  % Move forward (n>0) or backwards (n<0) over |n| defuns.
  % Return T if the specified number of defuns were found, NIL otherwise.
  % The cursor remains at the last defun found.

  (let ((flag T))
    (while (and (> n 0) (setf flag (move-forward-defun)))
      (setf n (- n 1)))
    (while (and (< n 0) (setf flag (move-backward-defun)))
      (setf n (+ n 1)))
    flag
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic Character Movement Primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de move-over-characters (n)
  % Move forward (n>0) or backwards (n<0) over |n| characters.  Return T if the
  % specified number of characters were found, NIL otherwise.  The cursor
  % remains at the last character found.

  (let ((flag T))
    (while (and (> n 0) (setf flag (move-forward-character)))
      (setf n (- n 1)))
    (while (and (< n 0) (setf flag (move-backward-character)))
      (setf n (+ n 1)))
    flag))

(de move-forward-character ()
  % Move forward one character.  If there is no next character, leave cursor
  % unchanged and return NIL; otherwise, return T.

  (if (at-buffer-end?)
    NIL
    (move-forward)
    T
    ))

(de move-backward-character ()
  % Move backward one character.  If there is no previous character, leave
  % cursor unchanged and return NIL; otherwise, return T.

  (if (at-buffer-start?)
    NIL
    (move-backward)
    T
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic Character Movement Primitives (Hacking Tabs Version)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de move-over-characters-hacking-tabs (n)
  % Move forward (n>0) or backwards (n<0) over |n| characters.  Return T if the
  % specified number of characters were found, NIL otherwise.  The cursor
  % remains at the last character found.

  (let ((flag T))
    (while (and (> n 0) (setf flag (move-forward-character-hacking-tabs)))
      (setf n (- n 1)))
    (while (and (< n 0) (setf flag (move-backward-character-hacking-tabs)))
      (setf n (+ n 1)))
    flag))

(de move-forward-character-hacking-tabs ()
  % Move forward one character.  If the next character is a tab, first
  % replace it with the appropriate number of spaces.  If there is no next
  % character, leave cursor unchanged and return NIL; otherwise, return T.

  (if (at-buffer-end?)
    NIL
    (cond ((= (next-character) (char TAB))
	   (delete-next-character)
	   (let ((n (- 8 (& (current-display-column) 7))))
	     (insert-string (substring "        " 0 n))
	     (set-char-pos (- (current-char-pos) n))
	     )))
    (move-forward)
    T
    ))

(de move-backward-character-hacking-tabs ()
  % Move backward one character.  If the previous character is a tab, first
  % replace it with the appropriate number of spaces.  If there is no previous
  % character, leave cursor unchanged and return NIL; otherwise, return T.

  (if (at-buffer-start?)
    NIL
    (cond ((= (previous-character) (char TAB))
	   (delete-previous-character)
	   (let ((n (- 8 (& (current-display-column) 7))))
	     (insert-string (substring "        " 0 n))
	     )))
    (move-backward)
    T
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic Word Movement Primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de word-char? (ch)
  (or (AlphanumericP ch) (= ch (char -))))

(de move-forward-word ()
  % Move forward one "word", starting from point.  Leave cursor to the
  % right of the "word".  If there is no next word, leave cursor unchanged
  % and return NIL; otherwise, return T.

  (let ((old-pos (buffer-get-position)))
    (while (and (not (at-buffer-end?)) % scan for start of word
	        (not (word-char? (next-character)))
	        )
      (move-forward))
    (cond ((at-buffer-end?)
	   (buffer-set-position old-pos)
	   NIL
	   )
	  (t
	   (while (and (not (at-buffer-end?)) % scan for end of word
		       (word-char? (next-character))
		       )
	     (move-forward))
	   T
	   ))))

(de move-backward-word ()
  % Move backward one "word", starting from point.  Leave cursor to the left of
  % the "word".  If there is no previous word, leave cursor unchanged and
  % return NIL; otherwise, return T.

  (let ((old-pos (buffer-get-position)))
    (while (and (not (at-buffer-start?)) % scan for end of word
	        (not (word-char? (previous-character)))
	        )
      (move-backward))
    (cond ((at-buffer-start?)
	   (buffer-set-position old-pos)
	   NIL
	   )
	  (t
	   (while (and (not (at-buffer-start?)) % scan for start of word
		       (word-char? (previous-character))
		       )
	     (move-backward))
	   T
	   ))))
