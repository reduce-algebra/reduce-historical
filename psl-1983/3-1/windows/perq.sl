%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PERQ.SL - Terminal Interface
% 
% Author:      Robert Kessler, U of Utah
% Date:        27 Jan 1983
% based on teleray.SL by     G.Q.Maguire,Jr.
%                            U of Utah
%                            3 November 1982
% based on VT52X.SL by       Alan Snyder
%                            Hewlett-Packard/CRC
%                            6 October 1982
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))
(CompileTime (load display-char fast-int fast-vectors))
  
(defflavor perq (
  (height 70)           % number of rows (0 indexed)
  (maxrow 69)           % highest numbered row
  (width 84)            % number of columns (0 indexed)
  (maxcol 83)           % highest numbered column
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (perq get-character) ()
  (& (PBIN) 8#377)
  )

(defmethod (perq ring-bell) ()
  (out-char BELL)
  )

(defmethod (perq move-cursor) (row column)
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

(defmethod (perq enter-raw-mode) ()
  (when (not raw-mode)
    (EchoOff)
    % Enable Keypad?
    (setf raw-mode T)))

(defmethod (perq leave-raw-mode) ()
  (when raw-mode
    (=> self &set-terminal-enhancement 0)
    (setf raw-mode NIL)
    % Disable Keypad?
    (EchoOn)))

(defmethod (perq erase) ()
  % This method should be invoked to initialize the screen to a known state.
  (out-chars ESC H ESC J)
  (setf cursor-row 0)
  (setf cursor-column 0)
  (setf terminal-enhancement NIL) % force resetting when needed
  )

(defmethod (perq clear-line) ()
  (out-chars ESC K)
  )

(defmethod (perq convert-character) (ch)
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

(defmethod (perq normal-enhancement) ()
  (dc-make-enhancement-mask)
  )

(defmethod (perq highlighted-enhancement) ()
  (dc-make-enhancement-mask)
  )

(defmethod (perq supported-enhancements) ()
  (dc-make-enhancement-mask)
  )

(defmethod (perq update-line) (row old-line new-line columns)
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

    % The VT52X will scroll if we write to the bottom right position.
    % This (hopefully temporary) hack will avoid writing there.
    (if (and (= row maxrow) (= last-col maxcol))
      (setf last-col (- maxcol 1))
      )

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
	     (setf cursor-column (+ cursor-column 1))
	     (when (> cursor-column maxcol)
	       (setf cursor-column 0)
	       (setf cursor-row (+ cursor-row 1))
	       (if (> cursor-row maxrow)
		 (=> self move-cursor 0 0)
		 ))
	     (vector-store old-line col new)
	     )))))

    % Do the ClearEOL, if that's what we decided to do.
    (when last-nonblank-column
      (=> self &move-cursor-forward (+ last-nonblank-column 1) old-line)
      (=> self clear-line)
      )
    ))

% The following methods are provided for INTERNAL use only!

(defmethod (perq init) ()
  )

(defmethod (perq &move-cursor-forward) (column line)
  (cond ((> (- column cursor-column) 4)
	 (out-move cursor-row column)
	 (setf cursor-column column))
	(t (while (< cursor-column column)
		  (PBOUT (dc-character-code (vector-fetch line cursor-column)))
		  (setf cursor-column (+ cursor-column 1))
		  ))))

(defmethod (perq &set-terminal-enhancement) (enh)
)
