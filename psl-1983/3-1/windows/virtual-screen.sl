%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Virtual-Screen.SL
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        18 August 1982
% Revised:     22 February 1983
%
% Inspired by Will Galway's EMODE Virtual Screen package.
%
% A virtual screen is an object that can be used as independent rectangular
% character display, but in fact shares a physical screen with other objects.  A
% virtual screen object maintains a stored representation of the image on the
% virtual screen, which is used to update the physical screen when new areas of
% the virtual screen become "exposed".  A virtual screen does not itself
% maintain any information about changes to its contents.  It sends all changes
% directly to the physical screen as they are made, and sends the entire screen
% contents to the physical screen upon its request.
%
% A virtual screen is a legitimate "owner" for a shared physical screen, in that
% it satisfies the required interface.
%
% 22-Feb-83 Alan Snyder
%  Declare -> Declare-Flavor.
% 28-Dec-82 Alan Snyder
%  Avoid writing to shared screen when virtual screen is not exposed.  Add
%  WRITE-STRING and WRITE-VECTOR methods.  Improve efficiency of CLEAR-TO-EOL
%  method.  Remove patch that avoided old compiler bug.  Reformat.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))
(CompileTime (load fast-int fast-vectors display-char))

(de create-virtual-screen (shared-physical-screen)
  (make-instance 'virtual-screen 'screen shared-physical-screen))

(defflavor virtual-screen
  ((height (=> screen height))	% number of rows (0 indexed)
   maxrow			% highest numbered row
   (width (=> screen width))	% number of columns (0 indexed)
   maxcol			% highest numbered column
   (row-origin 0)		% position of upper left on the shared screen
   (column-origin 0)		% position of upper left on the shared screen
   (default-enhancement (=> screen normal-enhancement))
   (cursor-row 0)		% the virtual cursor position
   (cursor-column 0)		% the virtual cursor position
   (exposed? NIL)
   image			% the virtual image
   screen        	        % the shared-physical-screen
   )
  ()
  (gettable-instance-variables height width row-origin column-origin screen
			       exposed?)
  (settable-instance-variables default-enhancement)
  (initable-instance-variables height width row-origin column-origin screen
			       default-enhancement)
  )

(declare-flavor shared-physical-screen screen)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private Macros:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmacro image-fetch (image row col)
  `(vector-fetch (vector-fetch ,image ,row) ,col))
(defmacro image-store (image row col value)
  `(vector-store (vector-fetch ,image ,row) ,col ,value))

(dm for-all-positions (form)
  % Executes the body repeatedly with the following variables
  % bound: ROW, COL, SCREEN-ROW, SCREEN-COL.
  `(for (from row 0 maxrow)
        (with screen-row)
        (do (setf screen-row (+ row-origin row))
	    (for (from col 0 maxcol)
		 (with screen-col ch)
	         (do (setf screen-col (+ column-origin col))
		     ,@(cdr form)
		     )))))

(dm for-all-columns (form)
  % Executes the body repeatedly with the following variables
  % bound: COL, SCREEN-COL.
  `(for (from col 0 maxcol)
        (with screen-col ch)
        (do (setf screen-col (+ column-origin col))
	    ,@(cdr form)
	    )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public methods:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (virtual-screen set-size) (new-height new-width)
  % Change the size of the screen.  The screen is first DeExposed.  The contents
  % are cleared.  You must Expose the screen yourself if you want it to be
  % displayed.

  (=> self deexpose)
  (setf height new-height)
  (setf width new-width)
  (=> self &new-size)
  )

(defmethod (virtual-screen set-origin) (new-row new-column)
  % Change the location of the screen.  The screen is first DeExposed.  You must
  % Expose the screen yourself if you want it to be displayed.

  (=> self deexpose)
  (setf row-origin new-row)
  (setf column-origin new-column)
  )

(defmethod (virtual-screen set-cursor-position) (row column)
  (cond ((< row 0) (setf row 0))
	((> row maxrow) (setf row maxrow)))
  (cond ((< column 0) (setf column 0))
	((> column maxcol) (setf column maxcol)))
  (setf cursor-row row)
  (setf cursor-column column)
  )

(defmethod (virtual-screen write) (ch row column)
  % Write one character using the default enhancement.
  (if (and (>= row 0) (<= row maxrow) (>= column 0) (<= column maxcol))
    (let ((dc (display-character-cons default-enhancement 0 (& ch 16#FF)))
	  (screen-row (+ row row-origin))
          )
      (setq dc (=> screen convert-character dc))
      (image-store image row column dc)
      (if exposed?
	(=> screen write dc screen-row (+ column column-origin) self))
      )))

(defmethod (virtual-screen write-range) (ch row left-column right-column)
  % Write repeatedly.
  (when (and (>= row 0)
	     (<= row maxrow)
	     (<= left-column maxcol)
	     (>= right-column 0)
	     )
    (if (< left-column 0) (setf left-column 0))
    (if (> right-column maxcol) (setf right-column maxcol))
    (let ((dc (display-character-cons default-enhancement 0 (& ch 16#FF)))
	  (screen-row (+ row row-origin))
          )
      (setq dc (=> screen convert-character dc))
      (for (from col left-column right-column)
	   (do (image-store image row col dc)
	       (if exposed?
		 (=> screen write dc screen-row (+ col column-origin) self))
	       )))))

(defmethod (virtual-screen write-display-character) (dc row column)
  % Write one character (explicit enhancement)
  (when (and (>= row 0) (<= row maxrow) (>= column 0) (<= column maxcol))
    (setq dc (=> screen convert-character dc))
    (image-store image row column dc)
    (if exposed?
      (=> screen write dc (+ row row-origin) (+ column column-origin) self))
    ))

(defmethod (virtual-screen write-string) (row left-column s count)
  % S is a string of characters. Write S[0..COUNT-1] using the default
  % enhancement to the specified row, starting at the specified column.

  (when (and (> count 0)
	     (>= row 0)
	     (<= row maxrow)
	     (<= left-column maxcol)
	     (> (+ left-column count) 0)
	     )
    (let ((smax (- count 1))
	  (image-row (vector-fetch image row))
	  (screen-row (+ row row-origin))
	  )
      (if (< left-column 0) (setf left-column 0))
      (if (> (+ left-column smax) maxcol)
	(setf smax (- maxcol left-column)))
      (for (from i 0 smax)
	   (for col left-column (+ col 1))
	   (for screen-col (+ left-column column-origin) (+ screen-col 1))
	   (do
	    (let ((ch (string-fetch s i)))
	      (setf ch (display-character-cons default-enhancement 0 ch))
	      (setf ch (=> screen convert-character ch))
	      (vector-store image-row col ch)
	      (if exposed?
		(=> screen write ch screen-row screen-col self))
	      ))))))

(defmethod (virtual-screen write-vector) (row left-column v count)
  % V is a vector of display-characters. Write V[0..COUNT-1] to the specified
  % row, starting at the specified column.

  (when (and (> count 0)
	     (>= row 0)
	     (<= row maxrow)
	     (<= left-column maxcol)
	     (> (+ left-column count) 0)
	     )
    (let ((vmax (- count 1))
	  (image-row (vector-fetch image row))
	  (screen-row (+ row row-origin))
	  )
      (if (< left-column 0) (setf left-column 0))
      (if (> (+ left-column vmax) maxcol)
	(setf vmax (- maxcol left-column)))
      (for (from i 0 vmax)
	   (for col left-column (+ col 1))
	   (for screen-col (+ left-column column-origin) (+ screen-col 1))
	   (do
	    (let ((ch (vector-fetch v i)))
	      (vector-store image-row col ch)
	      (if exposed?
		(=> screen write ch screen-row screen-col self))
	      ))))))

(defmethod (virtual-screen clear) ()
  (let ((dc (display-character-cons default-enhancement 0 #\space)))
    (setq dc (=> screen convert-character dc))
    (for-all-positions
     (image-store image row col dc)
     )
    (if exposed?
      (for-all-positions
       (=> screen write dc screen-row screen-col self)
       ))
    ))

(defmethod (virtual-screen clear-to-end) (first-row)
  (if (< first-row 0) (setf first-row 0))
  (let ((dc (display-character-cons default-enhancement 0 #\space)))
    (setq dc (=> screen convert-character dc))
    (for (from row first-row maxrow)
         (with screen-row)
         (do (setf screen-row (+ row-origin row))
             (for-all-columns
	      (image-store image row col dc)
	      )
	     (if exposed?
	       (for-all-columns
		(=> screen write dc screen-row screen-col self)
		))
	     ))))

(defmethod (virtual-screen clear-to-eol) (row first-column)
  (when (and (>= row 0) (<= row maxrow))
    (if (< first-column 0) (setf first-column 0))
    (let ((dc (display-character-cons default-enhancement 0 #\space))
	  (image-row (vector-fetch image row))
	  )
      (setq dc (=> screen convert-character dc))
      (for (from col first-column maxcol)
	   (do (vector-store image-row col dc)))
      (if exposed?
	(let ((screen-row (+ row row-origin)))
	  (for
	   (from col (+ first-column column-origin) (+ maxcol column-origin))
	   (do (=> screen write dc screen-row col self)))))
      )))

(defmethod (virtual-screen expose) ()
  % Expose the screen.  Make it overlap all other screens.
  (=> screen select-primary-owner self)
  (setf exposed? T)
  )

(defmethod (virtual-screen deexpose) ()
  % Remove the screen from the display.
  (when exposed?
    (=> screen remove-owner self)
    (setf exposed? NIL)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Semi-Private methods:
% The following methods are for use ONLY by the shared physical screen.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (virtual-screen send-changes) (breakout-allowed)
  % This method is invoked by the shared physical screen to obtain any buffered
  % changes to the virtual screen image.  Since the virtual screen does not
  % buffer any changes, this method does nothing.
  )

(defmethod (virtual-screen send-contents) (breakout-allowed)
  % This method is invoked by the shared physical screen to obtain the entire
  % virtual screen image.
  (for-all-positions
   (let ((ch (image-fetch image row col)))
     (=> screen write ch screen-row screen-col self)
     )))

(defmethod (virtual-screen assert-ownership) ()
  % This method is invoked by the shared physical screen to obtain the desired
  % area for the virtual screen.
  (=> screen set-owner-region row-origin column-origin height width self)
  )

(defmethod (virtual-screen screen-cursor-position) ()
  % This method is invoked by the shared physical screen to obtain the desired
  % cursor position for the virtual screen.
  (cons
   (+ cursor-row row-origin)
   (+ cursor-column column-origin)
   ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private methods:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (virtual-screen init) (init-plist)
  (=> self &new-size)
  )

(defmethod (virtual-screen &new-size) ()
  (if (< height 0) (setf height 0))
  (if (< width 0) (setf width 0))
  (setf maxrow (- height 1))
  (setf maxcol (- width 1))
  (setf image (make-vector maxrow NIL))
  (let ((line (make-vector maxcol #\space)))
    (for (from row 0 maxrow)
	 (do (vector-store image row (copyvector line))))
    )
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(undeclare-flavor screen)
