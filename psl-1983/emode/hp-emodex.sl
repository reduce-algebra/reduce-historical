%
% HP-EMODEX.SL - General HP EMODE Extensions
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        2 August 1982
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%% Changes: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% WFG  23 August 1982
% - Modified transpose-characters-command to behave as if at end of line if
%   the last command dispatched on was InsertSelfCharacter.
% - Made several "lispy" commands specific to Lisp mode rather than text
%   mode.


(BothTimes (load common))

% The following symbolic constants should be used in source code
% instead of the equivalent (Char X) expression to avoid fooling
% EMODE's stupid LISP parser while editing this file:

(CompileTime (setf LEFT-PAREN 40))
(CompileTime (setf RIGHT-PAREN 41))
(CompileTime (setf LEFT-PAREN-ID (int2id 40)))
(CompileTime (setf RIGHT-PAREN-ID (int2id 41)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Window Scrolling Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(CurrentLineIndex))

(de scroll-window-by-lines (n)

  % Scroll the contents of the current window up (n > 0) or down (n < 0)
  % by |n| lines.  CurrentLineIndex may be adjusted to keep it within
  % the desired window location.

  (let* ((window-height (current-window-height))
         (new-top-line (+ (current-window-top-line) n))
         (buffer-last-line (- (current-buffer-visible-size) 1))
         )

    % adjust to keep something in the window
    (cond
      ((< new-top-line 0) (setf new-top-line 0))
      ((> new-top-line buffer-last-line) (setf new-top-line buffer-last-line))
      )

    % adjust cursor if no longer in window
    (cond
      ((< CurrentLineIndex new-top-line)
       (SelectLine new-top-line))
      ((>= CurrentLineIndex (+ new-top-line window-height))
       (SelectLine (+ new-top-line window-height -1)))
      )
    (current-window-set-top-line new-top-line)
    ))

(de scroll-window-by-pages (n)

  % Scroll the contents of the current window up (n > 0) or down (n < 0)
  % by |n| screen-fulls.  CurrentLineIndex may be adjusted to keep it within
  % the desired window location.

  (let* ((old-top-line (current-window-top-line))
	 (window-height (current-window-height))
         (new-top-line (+ (current-window-top-line) (* n window-height)))
         (buffer-last-line (- (current-buffer-visible-size) 1))
         )

    % don't do the scroll if no change is needed
    (cond ((and (> new-top-line (- window-height))
	        (<= new-top-line buffer-last-line))
	   (setf new-top-line (max new-top-line 0))

	   % keep the cursor at the same relative location in the window!
	   (SelectLine (min (+ CurrentLineIndex (- new-top-line old-top-line))
			    (- (current-buffer-size) 1)))
	   (current-window-set-top-line new-top-line)
	   ))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Window Scrolling Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de scroll-window-up-line-command ()
  (scroll-window-by-lines 1)
  )

(de scroll-window-down-line-command ()
  (scroll-window-by-lines -1)
  )

(de scroll-window-up-page-command ()
  (scroll-window-by-pages 1)
  )

(de scroll-window-down-page-command ()
  (scroll-window-by-pages -1)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic Indenting Primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de current-line-indent ()
  % Return the indentation of the current line, in terms of spaces.

  (for (in ch CurrentLine)
       (while (or (= ch (char space)) (= ch (char tab))))
       (sum (if (= ch (char tab)) 8 1))
       ))

(de current-line-strip-indent ()
  % Strip all leading blanks and tabs from the current line.
  (while (and CurrentLine (char-blank? (car CurrentLine)))
    (setf CurrentLine (cdr CurrentLine))
    (if (> point 0) (setf point (- point 1)))
    ))

(de strip-previous-blanks ()
  % Strip all blanks and tabs before point.
  (while (and (> point 0)
	      (char-blank? (current-line-fetch (- point 1))))
	 ($DeleteBackwardCharacter))
  )

(de indent-current-line (n)
 % Adjust the current line to have the specified indentation.
  
  (current-line-strip-indent)
  (let ((n-spaces (remainder n 8))
         (n-tabs (quotient n 8)))
    (for (from i 1 n-spaces 1)
      (do (setf CurrentLine (cons (char space) CurrentLine))
        (setf point (+ 1 point))))
    (for (from i 1 n-tabs 1)
      (do (setf CurrentLine (cons (char tab) CurrentLine))
        (setf point (+ 1 point))))
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic Indenting Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(SetTextKey (char (meta !\)) 'delete-horizontal-space-command)
(de delete-horizontal-space-command ()
  (prog (ch)
    (while (< point (current-line-length))
      (setf ch (current-line-fetch point))
      (if (not (char-blank? ch)) (exit))
      (DeleteCharacter)
      )
    (while (> point 0)
      (setf ch (current-line-fetch (- point 1)))
      (if (not (char-blank? ch)) (exit))
      (setf point (- point 1))
      (DeleteCharacter)
      )
    ))

(SetTextKey (CharSequence (cntrl X) (cntrl O)) 'delete-blank-lines-command)
(de delete-blank-lines-command ()
  (cond ((current-line-blank?)
	 % We are on a blank line.
	 % Replace multiple blank lines with one.
	 % First, search backwards for the first blank line
	 % and save its index.
	 (while (> CurrentLineIndex 0)
	   ($BackwardLine)
	   (cond ((not (current-line-blank?))
		  ($ForwardLine)
		  (exit))
		 )
	   )
	 (delete-following-blank-lines)
	 )
	(t
	 % We are on a non-blank line.  Delete any blank lines
	 % that follow this one.
	 (delete-following-blank-lines)
	 )
    ))

(de delete-following-blank-lines ()

  % Delete any blank lines that immediately follow the current one.

  (if (not (current-line-is-last?))
      (progn
       (let ((old-index CurrentLineIndex)
	     (old-point point)
	     first-index
		   )
	    % Advance past the current line until the next nonblank line.
	    (move-to-next-line)
	    (setf first-index CurrentLineIndex)
	    (while T
		   (cond ((not (current-line-blank?)) (exit))
			 ((current-line-is-last?) ($EndOfLine) (exit))
			 (t (move-to-next-line))
			 ))
	    (delete_or_copy T first-index 0 CurrentLineIndex point)
	    (current-buffer-goto old-index old-point)
	    ))))

(SetTextKey (char (meta M)) 'back-to-indentation-command)
(SetTextKey (char (meta (cntrl M))) 'back-to-indentation-command)

(de back-to-indentation-command ()
  ($BeginningOfLine)
  (while (char-blank? (CurrentCharacter))
	 ($ForwardCharacter)
	 ))

(SetTextKey (char (meta ^)) 'delete-indentation-command)
(de delete-indentation-command ()
  (current-line-strip-indent)
  ($BeginningOfLine)
  (if (not (current-line-is-first?))
      (progn
       ($DeleteBackwardCharacter)
       (if (and (not (= point 0))
		(not (= (current-line-fetch (- point 1)) #.LEFT-PAREN))
		(not (= (CurrentCharacter) #.RIGHT-PAREN))
		)
	   (InsertCharacter (char space))
	   ))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LISP Indenting
% Note: this is a crock - need more sophisticated scanning
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(SetLispKey (char tab) 'lisp-tab-command)
(SetLispKey (char (meta (cntrl tab))) 'lisp-tab-command)
(SetLispKey (char LF) 'lisp-linefeed-command)
(SetLispKey (char (meta (cntrl Q))) 'lisp-indent-sexpr)

(de lisp-tab-command ()
  (indent-current-line (lisp-current-line-indent)))

(de lisp-linefeed-command ()
  ($CRLF)
  (indent-current-line (lisp-current-line-indent)))

(de lisp-indent-sexpr ()
  (if (not (move-down-list))
      (Ding)
      (let ((old-line CurrentLineIndex)
	    (old-point (- point 1))
	    final-line)
	   (if (not (forward-scan-for-right-paren -1))
	       (Ding)
	       (setf final-line CurrentLineIndex)
	       (for (from i (+ old-line 1) final-line 1)
		    (do
		     (SelectLine i)
		     (indent-current-line (lisp-current-line-indent))
		     ))
	       (current-buffer-goto old-line old-point)))
      ))

(de lisp-current-line-indent ()
  (let ((old-point point)
	(old-line CurrentLineIndex)
	indentation
	previous-line)
    (cond ((and (> CurrentLineIndex 0)
		(setf previous-line (GetBufferText (- CurrentLineIndex 1)))
		(>= (size previous-line) 0)
		(= (indx previous-line 0) #.LEFT-PAREN)
		)
	   2)
	  (t
	   (setf point 0)
	   (backward_sexpr)
	   (setf indentation (LineColumn point (List2String CurrentLine)))
	   (current-buffer-goto old-line old-point)
	   indentation
	   ))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Miscellaneous Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(SetTextKey (char (cntrl T)) 'transpose-characters-command)

% Transpose the last two characters, if we're at the end of the line, or if
% a character was just inserted.  Otherwise, transpose the characters on
% either side of point.
(de transpose-characters-command ()
  (progn 
    (if (or
          (= point (current-line-length))
          (eq last_operation 'InsertSelfCharacter))
      % We are at the end of a non-empty line, or last character was self
      % inserting.
      ($BackwardCharacter))

    (cond
      % We are at the beginning of a line, or the line has fewer then two
      % characters?
      ((or (= point 0) (< (current-line-length) 2))
        (Ding))

      (t
        % We are in the middle of a line.
        (prog (ch)
          ($BackwardCharacter)
          (setf ch (CurrentCharacter))
          (DeleteCharacter)
          ($ForwardCharacter)
          (InsertCharacter ch)
          )
        ))))

(SetTextKey (char (meta @)) 'mark-word-command)
(de mark-word-command ()
  (let ((old-index CurrentLineIndex)
	(old-point point))
    (forward_word)
    (SetMark)
    (current-buffer-goto old-index old-point)
    ))

(SetTextKey (char (meta (cntrl @))) 'mark-sexp-command)
(de mark-sexp-command ()
  (let ((old-index CurrentLineIndex)
	(old-point point))
    (forward_sexpr)
    (SetMark)
    (current-buffer-goto old-index old-point)
    ))

(SetTextKey (CharSequence (cntrl X) H) 'mark-whole-buffer-command)
(de mark-whole-buffer-command ()
  ($EndOfBuffer)
  (SetMark)
  ($BeginningOfBuffer)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LISP Defun Commands and Primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(SetLispKey (char (meta (cntrl A))) 'beginning-of-defun-command)
(SetLispKey (char (meta (cntrl ![))) 'beginning-of-defun-command)

(de beginning-of-defun-command ()

  % Move BACKWARD (literally) to the beginning of the current
  % (or previous) DEFUN.  If this is impossible, Ding and don't move.

  (if (at-buffer-start?)
      (Ding)
      ($BackwardCharacter)
      (if (not (beginning-of-defun)) (progn ($ForwardCharacter) (Ding)))
      ))

(de beginning-of-defun ()
  % Move backward to the beginning of the current DEFUN.  A DEFUN is
  % heuristically defined to be a line whose first character is a left
  % parenthesis.  If no DEFUN is found, point is left unchanged and
  % NIL is returned; otherwise T is returned.

  (let ((pos (buffer-get-position))
	)
    ($BeginningOfLine)
    (while T
	   (cond ((= (CurrentCharacter) #.LEFT-PAREN) (exit T))
		 ((current-line-is-first?)
		  (buffer-set-position pos)
		  (exit NIL))
		 (t (move-to-previous-line))
		 ))))

(SetLispKey (char (meta (cntrl E))) 'end-of-defun-command)
(SetLispKey (char (meta (cntrl !]))) 'end-of-defun-command)

(de end-of-defun-command ()
  % Move FORWARD (literally) to the beginning of the next line following
  % the end of a DEFUN.
  (let ((old-line CurrentLineIndex)
	)
    (if (or (not (end-of-defun)) (< CurrentLineIndex old-line))
	% If there is no current defun, or we were past the end of the
	% previous DEFUN, then we should continue onward to look for the
	% next DEFUN.
	(if (forward-defun)
	    (forward_sexpr)
	    (Ding)
	    )))
  (move-to-next-line)
  )

(de forward-defun ()
  % Move forward to the beginning of the next DEFUN.
  % If no DEFUN is found, point is left unchanged and
  % NIL is returned; otherwise T is returned.

  (let ((pos (buffer-get-position))
	)
    (while T
	   (move-to-next-line)
	   (cond ((= (CurrentCharacter) #.LEFT-PAREN) (exit T))
		 ((current-line-is-last?)
		  (buffer-set-position pos)
		  (exit NIL))
		 ))))

(de end-of-defun ()

  % Move forward to the end of the current DEFUN.
  % If there is no current DEFUN, don't move and return NIL.
  % Otherwise, return T.

  (cond ((not (beginning-of-defun)) NIL)
	(t (forward_sexpr) T)
	))

(SetLispKey (char (meta (cntrl H))) 'mark-defun-command)

(de mark-defun-command ()
  (end-of-defun-command)
  (SetMark)
  (beginning-of-defun-command)
  (if (> CurrentLineIndex 0)
      (progn
       (move-to-previous-line)
       (if (not (current-line-blank?))
	   (move-to-next-line))
       ))
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lisp List Commands and Primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(paren_depth)) % see Search.RED

% Perhaps SetLispKey is more appropriate?
(SetTextKey (char (meta (cntrl P))) 'move-past-previous-list)

(de move-past-previous-list ()
  % Move to the beginning of the current or previous list.  In other words,
  % find the previous left paren whose matching right paren is after point
  % or is the first right paren before point.
  % If no such left paren can be found, Ding, but do not move.

  (if (not (reverse-scan-for-left-paren 0)) (Ding))
  )

% (SetTextKey (char (meta (cntrl #.LEFT-PAREN-ID))) 'backward-up-list)
(SetTextKey (char (meta (cntrl U))) 'backward-up-list)
(de backward-up-list ()
  % Move to the left of the current list.  "Dual" to forward-up-list.
  (if (not (reverse-scan-for-left-paren 1)) (Ding))
  )

(de reverse-scan-for-left-paren (depth)

  % Scan backwards (starting with the character before point) for
  % a left paren at depth >= the specified depth.  If found, the
  % left paren will be after point and T will be returned.  Otherwise,
  % point will not change and NIL will be returned.

  (let ((old-position (buffer-get-position))
	ch
	)
    (setf paren_depth 0)
    (while T
      (cond ((and (= ch #.LEFT-PAREN) (>= paren_depth depth))
	     (exit T))
	    ((at-buffer-start?)
	     (buffer-set-position old-position)
	     (exit NIL))
	    (t ($BackwardCharacter)
	       (setf ch (CurrentCharacter))
	       (adjust_depth ch)
	       )
	    ))))

(SetTextKey (char (meta (cntrl N))) 'move-past-next-list)
(de move-past-next-list ()
  % Move to the right of the current or next list.  In other words,
  % find the next right paren whose matching left paren is before point
  % or is the first left paren after point.
  % If no such right paren can be found, Ding, but do not move.

  (if (not (forward-scan-for-right-paren 0)) (Ding))
  )

% (SetTextKey (char (meta (cntrl #.RIGHT-PAREN-ID))) 'forward-up-list)
(SetTextKey (char (meta (cntrl O))) 'forward-up-list)
(de forward-up-list ()
  % Move to the right of the current list.  In other words,
  % find the next right paren whose matching left paren is before point.
  % If no such right paren can be found, Ding, but do not move.

  (if (not (forward-scan-for-right-paren -1)) (Ding))
  )

(de forward-scan-for-right-paren (depth)

  % Scan forward (starting with the character after point) for
  % a right paren at depth <= the specified depth.  If found, the
  % right paren will be before point and T will be returned.  Otherwise,
  % point will not change and NIL will be returned.

  (let ((old-position (buffer-get-position))
	ch
	)
    (setf paren_depth 0)
    (while T
      (cond ((at-buffer-end?)
	     (buffer-set-position old-position)
	     (exit NIL)))
      (setf ch (CurrentCharacter))
      (adjust_depth ch)
      ($ForwardCharacter)
      (cond ((and (= ch #.RIGHT-PAREN) (<= paren_depth depth))
	     (exit T))
	    ))))

(SetTextKey (char (meta (cntrl D))) 'down-list)
(de down-list ()
  % Move inside the next contained list.  In other words,
  % find the next left paren without an intervening right paren.
  % If no such left paren can be found, Ding, but do not move.

  (if (not (move-down-list)) (Ding))
  )

(de move-down-list ()
  (let ((old-position (buffer-get-position))
	ch
	)
    (while T
      (cond ((at-buffer-end?)
	     (buffer-set-position old-position)
	     (exit NIL)))
      (setf ch (CurrentCharacter))
      ($ForwardCharacter)
      (cond ((= ch #.LEFT-PAREN)
	     (exit T))
	    ((= ch #.RIGHT-PAREN)
	     (buffer-set-position old-position)
	     (exit NIL))
	    ))))

(SetTextKey (char (meta #.LEFT-PAREN-ID)) 'insert-parens)
(de insert-parens ()
  (InsertCharacter #.LEFT-PAREN)
  (InsertCharacter #.RIGHT-PAREN)
  ($BackwardCharacter)
  )

(SetTextKey (char (meta #.RIGHT-PAREN-ID)) 'move-over-paren)
(de move-over-paren ()
  (if (forward-scan-for-right-paren 0)
      (progn
       ($BackwardCharacter)
       (strip-previous-blanks)
       ($ForwardCharacter)
       (lisp-linefeed-command)
       )
      (Ding)))
