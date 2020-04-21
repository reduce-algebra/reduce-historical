%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% HP2648A.SL - Terminal Interface
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        16 August 1982
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))
(CompileTime (load display-char fast-int fast-vectors))
  
(defflavor hp2648a (
  (height 24)           % number of rows (0 indexed)
  (maxrow 23)           % highest numbered row
  (width 80)            % number of columns (0 indexed)
  (maxcol 79)           % highest numbered column
  (cursor-row 0)        % cursor position
  (cursor-column 0)     % cursor position
  (raw-mode NIL)
  markers		% vector indicating locations of field markers
  (marker-table		% table for generating markers
    (Vector
	(char @) (char B) (char A) (char C)
	(char D) (char F) (char E) (char G)
	(char H) (char J) (char I) (char K)
	(char L) (char N) (char M) (char O)
	))
  )
  ()
  (gettable-instance-variables height width maxrow maxcol raw-mode)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime
  (defmacro out-n (n)
    `(progn
       (if (> ,n 9)
         (PBOUT (+ (char 0) (/ ,n 10))))
       (PBOUT (+ (char 0) (// ,n 10))))))

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
  (defmacro out-move ()
    `(out-chars ESC & !a)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (hp2648a get-character) ()
  (& (PBIN) 8#377)
  )

(defmethod (hp2648a ring-bell) ()
  (out-char BELL)
  )

(defmethod (hp2648a move-cursor) (row column)
  (cond ((< row 0) (setf row 0))
	((>= row height) (setf row maxrow)))
  (cond ((< column 0) (setf column 0))
	((>= column width) (setf column maxcol)))
  (cond ((and (= row cursor-row) (= column cursor-column))) % no action needed
	((and (= row 0) (= column 0))
	 (out-chars ESC H)) % cursor HOME
	((= row cursor-row) % movement on current row
	 (cond ((= column 0)
		(out-char CR)) % move to left margin
	       ((= column (- cursor-column 1))
		(out-chars ESC D)) % move LEFT
	       ((= column (+ cursor-column 1))
		(out-chars ESC C)) % move RIGHT
	       (t (out-move) (out-n column) (out-char C))))
	((= column cursor-column) % movement on same column
	 (cond ((= row (- cursor-row 1))
		(out-chars ESC A)) % move UP
	       ((= row (+ cursor-row 1))
		(out-char LF)) % move DOWN
	       (t (out-move) (out-n row) (out-char R))))
	(t % arbitrary movement
	 (out-move) (out-n row) (out-char (lower R))
		    (out-n column) (out-char C)))
  (setf cursor-row row)
  (setf cursor-column column)
  )

(defmethod (hp2648a enter-raw-mode) ()
  (when (not raw-mode)
    (EchoOff)
    (out-chars ESC & !s 1 A) % Enable Keypad
    (setf raw-mode T)))

(defmethod (hp2648a leave-raw-mode) ()
  (when raw-mode
    (setf raw-mode NIL)
    (out-chars ESC & !s 0 A) % Disable Keypad
    (EchoOn)))

(defmethod (hp2648a erase) ()
  % This method should be invoked to initialize the screen to a known state.
  (out-chars ESC H ESC J)
  (setf cursor-row 0)
  (setf cursor-column 0)
  (for (from row 0 maxrow)
       (do (let ((marker-line (vector-fetch markers row)))
	     (for (from col 0 maxcol)
		  (do (vector-store marker-line col NIL))
		  ))))
  )

(defmethod (hp2648a clear-line) ()
  (out-chars ESC K)
  (let ((marker-line (vector-fetch markers cursor-row)))
    (for (from col cursor-column maxcol)
	 (do (vector-store marker-line col NIL))
	 )))

(defmethod (hp2648a convert-character) (ch)
  (setq ch (& ch (display-character-cons
		     (dc-make-enhancement-mask INVERSE-VIDEO
					       BLINK
					       UNDERLINE
					       INTENSIFY)
		     (dc-make-font-mask 0)
		     16#FF)))
  (let ((code (dc-character-code ch)))
    (if (or (< code #\space) (= code (char rubout)))
      (setq ch #\space)))
  ch)

(defmethod (hp2648a normal-enhancement) ()
  (dc-make-enhancement-mask)
  )

(defmethod (hp2648a highlighted-enhancement) ()
  (dc-make-enhancement-mask INVERSE-VIDEO)
  )

(defmethod (hp2648a supported-enhancements) ()
  (dc-make-enhancement-mask INVERSE-VIDEO BLINK UNDERLINE INTENSIFY)
  )

(defmethod (hp2648a update-line) (row old-line new-line columns)
  % Old-Line is updated.

  % This code is particularly complicated because of the way HP terminals
  % implement display enhancements using field markers.  Most terminals
  % don't require this level of complexity.

  (prog (last-nonblank-column col terminal-enhancement old new marker-line
	first-col last-col)
    (setf first-col (car columns))
    (setf last-col (cdr columns))

    (setf marker-line (vector-fetch markers row))

    % Find out the minimal actual bounds:

    (while (and (<= first-col last-col)
	        (= (vector-fetch new-line last-col) (vector-fetch old-line last-col)))
      (setf last-col (- last-col 1))
      )
    (if (> first-col last-col) (return NIL)) % No change at all!
    (while (and (<= first-col last-col)
	        (= (vector-fetch new-line first-col) (vector-fetch old-line first-col)))
      (setf first-col (+ first-col 1))
      )

    % The purpose of the following code is to determine whether or not to use
    % ClearEOL.  If we decide to use ClearEOL, then we will set the variable
    % LAST-NONBLANK-COLUMN to the obvious index; otherwise, we will set it to
    % NIL.  If we decide to use ClearEOL, then we will clear out the OLD-LINE
    % now, but do the actual ClearEOL later.

    % Use of ClearEOL is appropriate if the rightmost changed character has
    % been changed to a space, and the remainder of the line is blank.  It
    % is appropriate only if it replaces writing at least 3 blanks.

    % Using ClearEOL can cause problems when display enhancements are used.  If
    % you write to the position just to the right of the terminal's
    % end-of-line, the existing field will be extended.  To avoid this problem,
    % we will avoid using ClearEOL where the immediately preceding character
    % has a non-zero enhancement.

    (when (= (vector-fetch new-line last-col) #\space)
      (setf last-nonblank-column (vector-upper-bound new-line))
      (while (and (>= last-nonblank-column 0)
		  (= (vector-fetch new-line last-nonblank-column) #\space)
		  )
        (setf last-nonblank-column (- last-nonblank-column 1))
        )

      % We have computed the column containing the rightmost non-blank
      % character.  Now, we can decide whether we want to do a ClearEOL or not.

      (if (and (< last-nonblank-column (- last-col 2))
	       (or (<= last-nonblank-column 0)
		   (~= (dc-enhancement-mask
			(vector-fetch old-line last-nonblank-column)) 0)))
        % then
	(while (> last-col last-nonblank-column)
	  (vector-store old-line last-col #\space)
	  (setf last-col (- last-col 1))
	  )
	% else
	(setf last-nonblank-column NIL)
	))

    % Output all changed characters (other than those that will be taken care
    % of by ClearEOL):

    (setf col first-col) % current column under examination
    (setf old (vector-fetch old-line col)) % terminal's contents at that location
    (setf new (vector-fetch new-line col)) % new contents for that location
    (setf terminal-enhancement (dc-enhancement-mask old))
	% terminal's enhancement for that location
	% (enhancement in OLD will not always be correct as we go)
    (if (not (and (= cursor-row row) (<= cursor-column col)))
      (=> self move-cursor row col))

    (while (<= col last-col)

      % First, we check to see if we need to write a new field marker.
      % A field marker is needed if the terminal's idea of the current
      % character's enhancement is different than the desired enhancement.

      (when (~= terminal-enhancement (dc-enhancement-mask new))
	(=> self move-cursor-forward col old-line)
	(=> self write-field-marker new)
	)

      % Next, we check to see if we need to write a new character code.

      (when (~= old new) % check this first for efficiency
	(let ((old-code (dc-character-code old))
	      (new-code (dc-character-code new))
	      )
	  (when (or (and (= new-code #\space) (= col last-col))
		  % last SPACE must be written (may extend EOL)
		  (~= old-code new-code))
	    (=> self move-cursor-forward col old-line)
	    (PBOUT new-code)
	    (setf cursor-column (+ cursor-column 1))
	    (when (> cursor-column maxcol)
	      (setf cursor-column 0)
	      (setf cursor-row (+ cursor-row 1))
	      (if (> cursor-row maxrow)
		  (=> self move-cursor 0 0)))
	    ))
	(vector-store old-line col new)
	)

      % The following code is executed only if there is a next character.

      (if (< col maxcol)
	(let* ((next-col (+ col 1))
	       (next-old (vector-fetch old-line next-col))
	       (next-new (vector-fetch new-line next-col))
	       )

	  % Compute the terminal's idea of the enhancement for the next
	  % character.  This is invalid if we are about to ClearEOL, but
	  % that case doesn't matter.

	  (setf terminal-enhancement
	    (if (vector-fetch marker-line next-col) % field marker there
	        (dc-enhancement-mask next-old)
		(dc-enhancement-mask new)))

	  (setf old next-old)
	  (setf new next-new)
	  ))

      (setf col (+ col 1))
      )

    % Check to see if a final field marker is needed.

    (when (and (<= col maxcol)
	     (or (null last-nonblank-column) (<= col last-nonblank-column))
	     (~= terminal-enhancement (dc-enhancement-mask old)))
      (=> self move-cursor-forward col old-line)
      (=> self write-field-marker new)
      )

    % Do the ClearEOL, if that's what we decided to do.

    (when last-nonblank-column
      (=> self move-cursor-forward (+ last-nonblank-column 1) old-line)
      (=> self clear-line)
      )
  ))

% The following methods are provided for INTERNAL use only!

(defmethod (hp2648a init) ()
  (setf markers (MkVect maxrow))
  (for (from row 0 maxrow)
       (do (vector-store markers row (MkVect maxcol)))
       )
  )

(defmethod (hp2648a move-cursor-forward) (column line)
  (cond ((> (- column cursor-column) 4)
	 (out-move) (out-n column) (out-char C)
	 (setf cursor-column column))
	(t (while (< cursor-column column)
		  (PBOUT (dc-character-code (vector-fetch line cursor-column)))
		  (setf cursor-column (+ cursor-column 1))
		  ))))

(defmethod (hp2648a write-field-marker) (ch)
  (out-chars ESC & !d)
  (PBOUT (vector-fetch marker-table (dc-enhancement-index ch)))
  (vector-store (vector-fetch markers cursor-row) cursor-column T)
  )
