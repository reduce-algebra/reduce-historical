%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Direct-Physical-Screen.SL - Write-Line and Direct-Write Version
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        17 August 1982
% Revised:     20 December 1982
%
% Adapted from Will Galway's EMODE Virtual Screen package.
%
% A physical screen is a rectangular character display.  Changes to the physical
% screen are made using the Write operation.  FULL-REFRESH should be called to
% initialize the state of the display.
%
% 20-Dec-82 Alan Snyder
%   Added cached methods for terminal Convert-Character and Get-Character.
% 17-Dec-82 Alan Snyder
%   Revised for the 9836 to write whole lines at a time, keeping track only
%   of which lines have been modified, or write each character directly,
%   according to the DIRECT? variable.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))
(CompileTime (load fast-int fast-vectors display-char))

(de create-physical-screen (display-terminal)
  (make-instance 'physical-screen 'terminal display-terminal))

(defflavor physical-screen
  (height                % number of rows (0 indexed)
   maxrow                % highest numbered row
   width                 % number of columns (0 indexed)
   maxcol                % highest numbered column
   cursor-row            % desired cursor position after refresh
   cursor-column         % desired cursor position after refresh
   terminal              % the display terminal
   new-image             % image for next refresh
   row-modified?         % which rows need to be rewritten?
   (direct? T)           % write directly to the terminal
   write-char-method     % terminal's write-char method
   write-line-method     % terminal's write-line method
   move-cursor-method    % terminal's move-cursor method
   get-char-method       % terminal's get-character method
   convert-char-method   % terminal's convert-character method
   )
  ()
  (gettable-instance-variables height width cursor-row cursor-column)
  (settable-instance-variables direct?)
  (initable-instance-variables terminal)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Private Macros:

(defmacro image-fetch (image row col)
  `(vector-fetch (vector-fetch ,image ,row) ,col))
(defmacro image-store (image row col value)
  `(vector-store (vector-fetch ,image ,row) ,col ,value))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Public methods:

(defmethod (physical-screen ring-bell) ()
  (=> terminal ring-bell))

(defmethod (physical-screen enter-raw-mode) ()
  (=> terminal enter-raw-mode))

(defmethod (physical-screen leave-raw-mode) ()
  (=> terminal leave-raw-mode))

(defmethod (physical-screen get-character) ()
  (apply get-char-method (list terminal)))

(defmethod (physical-screen convert-character) (ch)
  (apply convert-char-method (list terminal ch)))

(defmethod (physical-screen normal-enhancement) ()
  (=> terminal normal-enhancement))

(defmethod (physical-screen highlighted-enhancement) ()
  (=> terminal highlighted-enhancement))

(defmethod (physical-screen supported-enhancements) ()
  (=> terminal supported-enhancements))

(defmethod (physical-screen write) (ch row col)
  (when (not (= ch (image-fetch new-image row col)))
    (image-store new-image row col ch)
    (if direct?
      (apply write-char-method (list terminal row col ch))
      (vector-store row-modified? row T)
      )))

(defmethod (physical-screen set-cursor-position) (row col)
  (setf cursor-row row)
  (setf cursor-column col)
  (if direct? (apply move-cursor-method (list terminal row col)))
  )

(defmethod (physical-screen refresh) (breakout-allowed)
  (when (and (not direct?)
	     (not (and breakout-allowed (input-available?)))
	     )
    (for (from row 0 maxrow)
	 (when (vector-fetch row-modified? row))
	 (do
	  (apply write-line-method
		 (list terminal row (vector-fetch new-image row)))
	  (vector-store row-modified? row NIL)
	  ))
    (apply move-cursor-method (list terminal cursor-row cursor-column))
    ))

(defmethod (physical-screen full-refresh) (breakout-allowed)
  (=> terminal erase)
  (when (not (and breakout-allowed (input-available?)))
    (for (from row 0 maxrow)
	 (do
	  (apply write-line-method
		 (list terminal row (vector-fetch new-image row)))
	  (vector-store row-modified? row NIL)
	  ))
    (apply move-cursor-method (list terminal cursor-row cursor-column))
    ))

(defmethod (physical-screen write-to-stream) (s)
  (for (from row 0 maxrow)
       (with line)
       (do (setf line (vector-fetch new-image row))
	   (for (from col 0 maxcol)
		(do (=> s putc (dc-character-code (vector-fetch line col))))
		)
	   (=> s put-newline)
	   ))
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Private methods:

(defmethod (physical-screen init) (init-plist) % For internal use only!
  (setf height (=> terminal height))
  (setf maxrow (- height 1))
  (setf width (=> terminal width))
  (setf maxcol (- width 1))
  (setf cursor-row 0)
  (setf cursor-column 0)
  (setf new-image (=> self create-image))
  (setf row-modified? (make-vector height NIL))
  (setf write-char-method (object-get-handler terminal 'write-char))
  (setf write-line-method (object-get-handler terminal 'write-line))
  (setf move-cursor-method (object-get-handler terminal 'move-cursor))
  (setf get-char-method (object-get-handler terminal 'get-character))
  (setf convert-char-method (object-get-handler terminal 'convert-character))
  )

(defmethod (physical-screen create-image) ()
  (let ((image (MkVect maxrow))
	(line (MkVect maxcol))
	)
    (for (from col 0 maxcol)
	 (do (vector-store line col #\space))
	 )
    (for (from row 0 maxrow)
	 (do (vector-store image row (copyvector line)))
	 )
    image))
