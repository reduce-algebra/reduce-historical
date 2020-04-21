%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% TELEVIDEO -- Terminal Interface
%	    Lon Willett, 6-Jul-83
%	    Based on file:
%
%   TELERAY.SL
%   Author:      G.Q. Maguire Jr., U of Utah
%   Date:        3 Nov 1982
%   based on VT52X.SL by       Alan Snyder
%                              Hewlett-Packard/CRC
%                              6 October 1982
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))
(CompileTime (load display-char fast-int fast-vectors))
(BothTimes (load JSYS))
(compiletime
 (progn
  (defconst !.MORLW 8#30 % read page width
	    !.MORLL 8#32 % read page length
	    !.PRIOU 8#101) % primary output jfn, it had better be a TTY
% NOTE: since I/O is done with PBIN/PBOUT, using the primary JFN should
% be ok.  This really ought to be written to use an arbitrary JFN.
  (ds get-system-page-height ()
    (jsys3 (const !.priou) (const !.morll) 0 0 (const jsMTOPR)) )
  (ds get-system-line-length ()
    (jsys3 (const !.priou) (const !.morlw) 0 0 (const jsMTOPR)) )
  ))

(defflavor televideo (
  (height 24)           % number of rows (0 indexed)
  (maxrow 23)           % highest numbered row
  (width 80)            % number of columns (0 indexed)
  (maxcol 79)           % highest numbered column
  (auto-wrap 'MAYBE)	% does a CRLF when output to last column: YES NO MAYBE
  (auto-scroll 'YES)	% scrolls when output to (MAXROW,MAXCOL): YES NO MAYBE
  (cursor-row 0)        % cursor position
  (cursor-column 0)     % cursor position
  (raw-mode NIL)
  (terminal-enhancement 0) % current enhancement (applies to most output)
  (terminal-blank #\space) % character used by ClearEOL
  )
  ()
  (gettable-instance-variables height width auto-wrap auto-scroll
			       maxrow maxcol raw-mode)
  (initable-instance-variables height width auto-wrap auto-scroll)
  )

(defmethod (televideo init) (initlis)
  % Pick up the page length & width from the monitor if it is not
  % specified by an initialization argument.  Use default if we don't like
  % what the monitor claims.
  % HEIGHT & MAXROW:
  (unless (memq 'HEIGHT initlis) (setf height (get-system-page-height)))
  (when (or (< height 10) (> height 96)) (setf height 24))
  (setf maxrow (- height 1))
  % WIDTH & MAXCOL:
  (unless (memq 'WIDTH initlis) (setf width (get-system-line-length)))
  (when (or (< width 10) (> width 96)) (setf width 80))
  (setf maxcol (- width 1)) 
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime
  (defmacro out-char (ch)
    `(PBOUT (char ,ch))))

(CompileTime
  (dm out-chars (form)
    (for (in ch (cdr form))
	 (with L)
	 (collect (list 'out-char ch) L)
	 (returns (cons 'progn L)))))

(CompileTime
  (defmacro out-move (row col)
    `(progn
      (out-chars ESC !=)
      (PBOUT (+ ,row 32))
      (PBOUT (+ ,col 32)))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (televideo get-character) ()
  (& (PBIN) 8#177)
  )

(defmethod (televideo ring-bell) ()
  (out-char BELL)
  )

(defmethod (televideo move-cursor) (row column)
  % (ROW COLUMN) is the point we want to move to
  (cond ((< row 0) (setf row 0))
	((>= row height) (setf row maxrow)))
  (cond ((< column 0) (setf column 0))
	((>= column width) (setf column maxcol)))
  (let ((relative-move-number-of-chars
	 (+ % vertical move:
	  (cond ((< cursor-row row) (- row cursor-row)) % 1 char to move down
		((> cursor-row row) (- cursor-row row)) % 1 to move up
		(T 0)) % else no vertical move necessary
	  % horizontal move:
	  (cond ((= cursor-column column) 0) % no horizontal move necessary
		((= column 0) 1) % move to left column
		((> cursor-column column)
		 (- cursor-column column)) % 1 char to move left
		(T (- column cursor-column)) ) % 1 char to move right
	  )))
    (cond ((= relative-move-number-of-chars 0) ) % no move needed
	  ((and (= row 0) (= column 0))
	   (out-char (CONTROL !^))) % cursor HOME
	  ((>= relative-move-number-of-chars 4)
	   (out-move row column)) % move absolute
	  (T % move relative
	   (cond ((= cursor-column column) ) % no horizontal move needed
		 ((= column 0) (out-char CR)) % move to left-most column
		 ((> cursor-column column)
		  (for (from curcol cursor-column (+ column 1) -1)
		       (do (out-char BACKSPACE)) )) % move left
		 (T
		  (for (from curcol cursor-column (- column 1) 1)
		       (do (out-char FF)) )) ) % move right
	   % now take care of the vertical move
	   (cond ((= cursor-row row) ) % no move needed
		 ((< cursor-row row)
		  (for (from currow cursor-row (- row 1) 1)
		       (do (out-char LF)) )) % move down
		 (T (for (from currow cursor-row (+ row 1) -1)
			 (do (out-char (CONTROL K))) )) ) % move up
	   )))
  (setf cursor-row row)
  (setf cursor-column column)
  )

(defmethod (televideo enter-raw-mode) ()
  (when (not raw-mode)
    (EchoOff)
    % Enable Keypad?
    (setf raw-mode T)))

(defmethod (televideo leave-raw-mode) ()
  (when raw-mode
    (=> self &set-terminal-enhancement 0)
    (setf raw-mode NIL)
    % Disable Keypad?
    (EchoOn)))

(defmethod (televideo erase) ()
  % This method should be invoked to initialize the screen to a known state.
  (out-chars (CONTROL !^) ESC !*)
  (setf cursor-row 0)
  (setf cursor-column 0)
  (setf terminal-enhancement NIL) % force resetting when needed
  )

(defmethod (televideo clear-line) ()
  (out-chars ESC (LOWER T))
  )

(defmethod (televideo convert-character) (ch)
  (setf ch (& ch (display-character-cons
		  % no enhancements supporeted
		  (dc-make-enhancement-mask
		   % INVERSE-VIDEO BLINK UNDERLINE INTENSIFY
		   )
		  % only font number 0 supported
		  (dc-make-font-mask 0)
		  % only 7 bit chars
		  16#7F)))
  (let ((code (dc-character-code ch)))
    % replace non-printable chars with a space
    (when (or (< code 8#40) (= code (char rubout))) (setf ch terminal-blank)))
  ch)

(defmethod (televideo normal-enhancement) ()
  (dc-make-enhancement-mask) )

(defmethod (televideo highlighted-enhancement) ()
  (dc-make-enhancement-mask) )

(defmethod (televideo supported-enhancements) ()
  (dc-make-enhancement-mask) )

(defmethod (televideo update-line) (row old-line new-line columns)
  % Old-Line is updated.

  (let ((first-col (car columns))
	(last-col (cdr columns))
	(last-nonblank-column NIL)
	)
    % Find out the minimal actual bounds:
    (while (and (<= first-col last-col)
	        (= (vector-fetch new-line last-col)
		   (vector-fetch old-line last-col)))
      (setf last-col (- last-col 1))
      )
    (while (and (<= first-col last-col)
	        (= (vector-fetch new-line first-col)
		   (vector-fetch old-line first-col)))
      (setf first-col (+ first-col 1))
      )

    % this check prevents index of -1, and also avoids cursor movement
    % when the line doesn't need to be changed
    (when (<= first-col last-col)

      % The purpose of the following code is to determine whether or not to use
      % ClearEOL.  If we decide to use ClearEOL, then we will set the variable
      % LAST-NONBLANK-COLUMN to the obvious index; otherwise, we will set it to
      % NIL.  If we decide to use ClearEOL, then we will clear out the OLD-LINE
      % now, but do the actual ClearEOL later.

      % Use of ClearEOL is appropriate if the rightmost changed character has
      % been changed to a space, and the remainder of the line is blank.  It
      % is appropriate only if it replaces writing at least 3 blanks.

      (when (= (vector-fetch new-line last-col) terminal-blank)
	(setf last-nonblank-column (vector-upper-bound new-line))
	(while (and (>= last-nonblank-column 0)
		    (= (vector-fetch new-line last-nonblank-column)
		       terminal-blank)
		    )
	  (setf last-nonblank-column (- last-nonblank-column 1))
	  )

	% We have computed the column containing the rightmost non-blank
	% character.  Now, we can decide whether to do a ClearEOL or not.

	(if (and (< last-nonblank-column (- last-col 2)))
	  % then
	  (while (> last-col last-nonblank-column)
	    (vector-store old-line last-col terminal-blank)
	    (setf last-col (- last-col 1))
	    )
	  % else
	  (setf last-nonblank-column NIL)
	  ))

      % Output all changed characters (except those ClearEOL will do):
      (for (from col first-col last-col)
	   (do
	    (let ((old (vector-fetch old-line col))
		  (new (vector-fetch new-line col))
		  )
	      (when (~= old new)
		(let ((new-enhancement (dc-enhancement-mask new))
		      (new-code (dc-character-code new))
		      )
		  % Do we need to change the terminal enhancement?
		  (when (~= terminal-enhancement new-enhancement)
		    (=> self &set-terminal-enhancement new-enhancement)
		    )
		  (=> self move-cursor row col)
		  (=> self &print-char new-code)
		  (vector-store old-line col new)
		  )))))

      % Do the ClearEOL, if that's what we decided to do.
      (when last-nonblank-column
	(=> self move-cursor row (+ last-nonblank-column 1))
	(=> self clear-line)
	)
      )))

% The following methods are provided for INTERNAL use only!

% This method outputs a printable character
% (should we check that the character is printable?)
(defmethod (televideo &print-char) (ch)
  (cond ((< cursor-column maxcol) % normal case
	 (PBOUT ch) 
	 (setf cursor-column (+ cursor-column 1)))

	((< cursor-row maxrow) % last character on a line, but not last line
	 % This horrendous hack assures that we have auto-wrap
	 (PBOUT ch)
	 (setf cursor-row (+ cursor-row 1))
	 (setf cursor-column 0)
	 (cond ((eq auto-wrap 'NO) (out-chars CR LF)) 
	       ((eq auto-wrap 'MAYBE) (out-move cursor-row 0))
%	       ((eq auto-wrap 'YES) )
	       ))
	(T % Bottom right corner
	 % Prevent scrolling (put blank there if we can't print). Move to (0,0).
	 (IF (or (eq auto-scroll 'YES) (eq auto-scroll 'MAYBE))
	   % THEN
	   (=> self clear-line)
	   % ELSE (eq auto-scroll 'NO) so
	   (PBOUT ch))
	 (=> self move-cursor 0 0) )
	))

(defmethod (televideo &set-terminal-enhancement) (enh)
  (setf terminal-enhancement 0) )
