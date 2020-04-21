%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% VT52X.SL - Terminal Interface
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        6 October 1982
% Revised:     1 March 1983
%
% 1-Mar-83 Alan Snyder
%  Removed right-corner-of-screen hack (no longer needed).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))
(CompileTime (load display-char fast-int fast-vectors))
  
(defflavor vt52x (
  (height 24)           % number of rows (0 indexed)
  (maxrow 23)           % highest numbered row
  (width 80)            % number of columns (0 indexed)
  (maxcol 79)           % highest numbered column
  (cursor-row 0)        % cursor position
  (cursor-column 0)     % cursor position
  (raw-mode NIL)
  (terminal-enhancement 0) % current enhancement (applies to most output)
  (terminal-blank #\space) % character used by ClearEOL
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
  (defmacro out-move (row col)
    `(progn
      (out-chars ESC Y)
      (PBOUT (+ ,row 32))
      (PBOUT (+ ,col 32)))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (vt52x get-character) ()
  (& (PBIN) 8#377)
  )

(defmethod (vt52x ring-bell) ()
  (out-char BELL)
  )

(defmethod (vt52x move-cursor) (row column)
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
	       (t (out-move row column))))
	((= column cursor-column) % movement on same column
	 (cond ((= row (- cursor-row 1))
		(out-chars ESC A)) % move UP
	       ((= row (+ cursor-row 1))
		(out-char LF)) % move DOWN
	       (t (out-move row column))))
	(t % arbitrary movement
	 (out-move row column)))
  (setf cursor-row row)
  (setf cursor-column column)
  )

(defmethod (vt52x enter-raw-mode) ()
  (when (not raw-mode)
    (EchoOff)
    % Enable Keypad?
    (setf raw-mode T)))

(defmethod (vt52x leave-raw-mode) ()
  (when raw-mode
    (=> self &set-terminal-enhancement 0)
    (setf raw-mode NIL)
    % Disable Keypad?
    (EchoOn)))

(defmethod (vt52x erase) ()
  % This method should be invoked to initialize the screen to a known state.
  (out-chars ESC H ESC J)
  (setf cursor-row 0)
  (setf cursor-column 0)
  (setf terminal-enhancement NIL) % force resetting when needed
  )

(defmethod (vt52x clear-line) ()
  (out-chars ESC K)
  )

(defmethod (vt52x convert-character) (ch)
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

(defmethod (vt52x normal-enhancement) ()
  (dc-make-enhancement-mask)
  )

(defmethod (vt52x highlighted-enhancement) ()
  (dc-make-enhancement-mask INVERSE-VIDEO)
  )

(defmethod (vt52x supported-enhancements) ()
  (dc-make-enhancement-mask INVERSE-VIDEO BLINK UNDERLINE INTENSIFY)
  )

(defmethod (vt52x update-line) (row old-line new-line columns)
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
      % character.  Now, we can decide whether we want to do a ClearEOL or not.

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
    (if (not (and (= cursor-row row) (<= cursor-column first-col)))
      (=> self move-cursor row first-col))

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
             (if (~= terminal-enhancement new-enhancement)
	       (=> self &set-terminal-enhancement new-enhancement)
	       )
	     (=> self &move-cursor-forward col old-line)
	     (PBOUT new-code)
	     (if (< cursor-column maxcol)
		 (setf cursor-column (+ cursor-column 1))
		 % otherwise
		 % (pretend we don't know the cursor position...
		 % the two versions of the emulator differ at this point!)
		 (setf cursor-column 10000)
		 (setf cursor-row 10000)
		 )
	     (vector-store old-line col new)
	     )))))

    % Do the ClearEOL, if that's what we decided to do.
    (when last-nonblank-column
      (=> self &move-cursor-forward (+ last-nonblank-column 1) old-line)
      (=> self clear-line)
      )
    ))

% The following methods are provided for INTERNAL use only!

(defmethod (vt52x init) ()
  )

(defmethod (vt52x &move-cursor-forward) (column line)
  (cond ((> (- column cursor-column) 4)
	 (out-move cursor-row column)
	 (setf cursor-column column))
	(t (while (< cursor-column column)
		  (PBOUT (dc-character-code (vector-fetch line cursor-column)))
		  (setf cursor-column (+ cursor-column 1))
		  ))))

(defmethod (vt52x &set-terminal-enhancement) (enh)
  (setf terminal-enhancement enh)
  (out-char ESC)
  (PBOUT 3)
  (PBOUT (dc-enhancement-index enh))
  )
