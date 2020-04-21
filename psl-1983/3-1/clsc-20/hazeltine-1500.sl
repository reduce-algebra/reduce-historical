%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% HAZELTINE-1500.SL - Terminal Interface
% 
% Author:   Lon Willett
% Date: 6-Jul-83
%
% Based on TELERAY.SL by:
%    Author:      G.Q. Maguire Jr., U of Utah
%    Date:        3 Nov 1982
%    based on VT52X.SL by       Alan Snyder
%                               Hewlett-Packard/CRC
%                               6 October 1982
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))
(CompileTime (load display-char fast-int fast-vectors))
(BothTimes (load jsys))
(compiletime
 (progn
  (defconst !.MORLW 8#30 % read page width
	    !.MORLL 8#32 % read page length
	    !.PRIOU 8#101) % primary output jfn, it had better be a TTY
  (ds get-system-page-height ()
    (jsys3 (const !.priou) (const !.morll) 0 0 (const jsMTOPR)) )
  (ds get-system-line-length ()
    (jsys3 (const !.priou) (const !.morlw) 0 0 (const jsMTOPR)) )
  ))

(BothTimes (Put 'TILDE 'CHARCONST 126))

% This hack redefines !\= as a macro to be replaced by 
% (INTERN (STRING #\TILDE #\=)).  This file shouldn't contain any TILDE's
(CompileTime (DM !\= (u) `(#.(INTERN (STRING #\TILDE #/=)) . ,(CDR u)) ))

(defflavor hazeltine-1500 (

  (height 24)           % number of rows (0 indexed)
  (maxrow 23)           % highest numbered row
  (width 80)            % number of columns (0 indexed)
  (maxcol 79)           % highest numbered column
  (auto-wrap 'MAYBE)	% does a CRLF when output to last column: YES NO MAYBE
  (auto-scroll 'YES)	% scrolls when output (MAXROW,MAXCOL): YES NO MAYBE
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

(defmethod (hazeltine-1500 init) (initlis)
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
  (deflambda out-move (xxxrow xxxcol)
	     (out-chars TILDE (CONTROL Q))
	     (PBOUT (IF (>= xxxcol 31) xxxcol (+ xxxcol 8#140)))
	     (PBOUT (+ xxxrow 32)) ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (hazeltine-1500 get-character) ()
  (& (PBIN) 8#177)
  )

(defmethod (hazeltine-1500 ring-bell) ()
  (out-char BELL)
)

(defmethod (hazeltine-1500 move-cursor) (row column)
  (cond ((< row 0) (setf row 0))
	((>= row height) (setf row maxrow)))
  (cond ((< column 0) (setf column 0))
	((>= column width) (setf column maxcol)))
  (let ((relative-move-number-of-chars
	 (+ %calculate the number of chars for a horizontal move
	  (cond ((= column cursor-column) 0) % no horizontal move required
		((= column 0) 1) % using a CR
		((< column cursor-column)
		 (- cursor-column column)) % move left takes 1 char
		(T (- column cursor-column)) ) % move right takes 1 char
	  % and add in the number of chars for a vertical move
	  (cond ((= row cursor-row) 0) % no vertical move required
		((< row cursor-row)
		 (* 2 (- cursor-row row))) % move up takes 2 chars
		(T (- row cursor-row)) )))) % move down takes 1 char
       (cond ((= relative-move-number-of-chars 0) ) % no move required
	     ((and (= row 0) (= column 0)
		   (<= 2 relative-move-number-of-chars))
	      (out-chars TILDE (CONTROL R)) ) % cursor home
	     ((<= 4 relative-move-number-of-chars)
	      (out-move row column)) % move absolute
	     (T %Move relative to the current point
	      (cond
	       ((= column cursor-column) ) % no horizontal move needed
	       ((= column 0) (out-char CR)) % move to leftmost column
	       ((< column cursor-column)
		(FOR (FROM junk cursor-column (+ column 1) -1)
		     (DO (out-char BACKSPACE)) )) % move left
	       (T
		(FOR (FROM junk cursor-column (- column 1) 1)
		     (DO (out-char (CONTROL P))) ))) % move right
	      (cond ((< row cursor-row)
		     (FOR (FROM junk cursor-row (+ row 1) -1)
			  (DO (out-chars TILDE FF)) )) % move up
		    ((> row cursor-row)
		     (FOR (FROM junk cursor-row (- row 1) 1)
			  (DO (out-char LF)) ))) % move down
	      )) )
  (setf cursor-row row)
  (setf cursor-column column)
  )

(defmethod (hazeltine-1500 enter-raw-mode) ()
  (when (not raw-mode)
    (EchoOff)
    % Enable Keypad?
    (setf raw-mode T)))

(defmethod (hazeltine-1500 leave-raw-mode) ()
  (when raw-mode
    (=> self &set-terminal-enhancement 0)
    (setf raw-mode NIL)
    % Disable Keypad?
    (EchoOn)))

(defmethod (hazeltine-1500 erase) ()
  % This method should be invoked to initialize the screen to a known state.
  (out-chars TILDE (CONTROL R) TILDE (CONTROL X))
  (setf cursor-row 0)
  (setf cursor-column 0)
  (setf terminal-enhancement NIL) % force resetting when needed
  )

(defmethod (hazeltine-1500 clear-line) ()
  (out-chars TILDE (CONTROL O))
  )

(defmethod (hazeltine-1500 convert-character) (ch)
  (setf ch (& ch (display-character-cons
		  % no enhancements
		  (dc-make-enhancement-mask
		   % INVERSE-VIDEO BLINK UNDERLINE INTENSIFY
		   )
		  % only font number 0
		  (dc-make-font-mask 0)
		  % only 7 bits in a character
		  16#7F)))
  (let ((code (dc-character-code ch)))
    % replace non-printable chars with a space
    (when (or (< code 8#40) (>= code 8#176)) (setf ch terminal-blank)) )
  ch)

(defmethod (hazeltine-1500 normal-enhancement) ()
  (dc-make-enhancement-mask) )

(defmethod (hazeltine-1500 highlighted-enhancement) ()
  (dc-make-enhancement-mask) )

(defmethod (hazeltine-1500 supported-enhancements) ()
  (dc-make-enhancement-mask) )

(defmethod (hazeltine-1500 update-line) (row old-line new-line columns)
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

    % this check prevents unchecked index of -1, and also keeps
    % us from moving the cursor when the line doesn't need to be updated
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
		       terminal-blank )  )
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
	      (when (!\= old new)
		(let ((new-enhancement (dc-enhancement-mask new))
		      (new-code (dc-character-code new))
		      )
		  % Do we need to change the terminal enhancement?
		  (when (!\= terminal-enhancement new-enhancement)
		    (=> self &set-terminal-enhancement new-enhancement) )
		  (=> self move-cursor row col)
		  (=> self &print-char new-code)
		  (vector-store old-line col new)
		  )) )))

      % Do the ClearEOL, if that's what we decided to do.
      (when last-nonblank-column
	(=> self move-cursor row (+ last-nonblank-column 1))
	(=> self clear-line)
	)
      )))

  
% The following methods are provided for INTERNAL use only!

% This method outputs a printable character
% (should we check that the character is printable?)
(defmethod (hazeltine-1500 &print-char) (ch)
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

(defmethod (hazeltine-1500 &set-terminal-enhancement) (enh)
% no enhancements supported
  (setf terminal-enhancement 0)
)
