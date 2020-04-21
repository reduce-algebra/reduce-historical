%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Buffer-Window.SL
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        18 August 1982
% Revised:     24 February 1983
%
% Inspired by Will Galway's EMODE Virtual Screen package.
%
% A Buffer-Window object maintains an attachment between an editor buffer and a
% virtual screen.  This module is responsible for mapping the contents of the
% editor buffer to an image on the virtual screen.  A "window label" object
% may be specified to maintain a descriptive label at the bottom of the
% virtual screen (see comment for the SET-LABEL method).
%
% 24-Feb-83 Alan Snyder
%   Fixed bug: cursor positioning didn't take buffer-left into account.
% 16-Feb-83 Alan Snyder
%   Declare -> Declare-Flavor.
% 7-Feb-83 Alan Snyder
%   Refresh now returns a flag indicating completion (no breakout).
%   Add cached method for label refresh.
% 31-Jan-83 Alan Snyder
%   Modified to use separate window-label object to write the label area.
%   Note: SET-SIZE height argument is now interpreted as the screen height!
% 20-Jan-83 Alan Snyder
%   Bug fix: adjust window after changing screen size.
% 28-Dec-82 Alan Snyder
%   Replaced call to current-display-column in REFRESH, which was incorrect
%   because it assumes the buffer is current.  Changed to display position of
%   window, rather than position of buffer (meaningful only when the window
%   package can display multiple cursors).  Added methods: CHAR-POSITION,
%   SET-SCREEN, and &NEW-SCREEN.  Changed EXPOSE to refresh first, for more
%   graceful screen update when using direct writing.  Change label writing to
%   clear-eol after writing the label, not before, also for more graceful
%   screen update.  Changed &WRITE-LINE-TO-SCREEN to buffer its changes in a
%   string, for efficiency. General cleanup.
% 20-Dec-82 Alan Snyder
%   Added declarations for buffer and screen instance variables, for
%   efficiency.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))
(CompileTime (load fast-int fast-vectors fast-strings display-char))

(de create-unlabeled-buffer-window (buffer virtual-screen)
  % Create a buffer window object that presents the specified buffer onto
  % the specified virtual-screen.  There will be no label area.
  (make-instance 'buffer-window 'buffer buffer 'screen virtual-screen)
  )

(de create-buffer-window (buffer virtual-screen)
  % Create a buffer window object that presents the specified buffer onto
  % the specified virtual-screen.  There will be a one-line label.
  (let ((w (create-unlabeled-buffer-window buffer virtual-screen)))
    (=> w set-label (create-window-label w))
    w
    ))

(defflavor buffer-window 
  (height			% number of rows of text (rows are 0 indexed)
   maxrow			% highest numbered row
   width			% number of columns of text (cols are 0 indexed)
   maxcol			% highest numbered column
   (buffer-left 0)		% leftmost buffer column displayed
   (buffer-top 0)		% topmost buffer line displayed
   (overflow-marker #/!)	% display character used to mark overlong lines
   (saved-position NIL)		% buffer position saved here while not selected

   (label NIL)			% the optional label-maintaining object
   (label-height 0)		% number of lines occupied by the label
   (label-refresh-method NIL)	% cached method for refreshing the label

   (text-enhancement (dc-make-enhancement-mask))
				% display enhancement used in text area

   line-buffer			% string of characters used to write line

   buffer			% the buffer being displayed
   screen        	        % the virtual screen used for display
   buffer-lines			% vector of buffer lines currently displayed
   %				% NIL used for EQable empty string
   )
  ()
  (gettable-instance-variables
   height
   width
   screen
   buffer
   buffer-left
   buffer-top
   text-enhancement
   )
  (initable-instance-variables
   screen
   buffer
   text-enhancement
   )
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(declare-flavor text-buffer buffer)
(declare-flavor virtual-screen screen)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public methods:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (buffer-window select) ()
  % This method is invoked when the window is selected.  It restores the saved
  % buffer pointer, if any.  It will not scroll the window: instead, it will
  % adjust the buffer position, if necessary, to keep the buffer pointer within
  % the window.
  (when saved-position
    (=> buffer set-position saved-position)
    (setf saved-position NIL)
    )
  (=> self adjust-buffer)
  )

(defmethod (buffer-window deselect) ()
  % This method is invoked when the window is deselected.  It saves the current
  % buffer pointer, which will be restored when the window is again selected.
  % It adjusts the window to ensure that the window shows the saved position.
  (setf saved-position (=> buffer position))
  (=> self adjust-window)
  )

(defmethod (buffer-window expose) ()
  % Expose the window, putting it "on top" (expose the attached virtual screen).
  (=> self refresh nil)
  (=> screen expose)
  )

(defmethod (buffer-window deexpose) ()
  % De-expose the window (de-expose the attached virtual screen).
  (=> screen deexpose)
  )

(defmethod (buffer-window exposed?) ()
  (=> screen exposed?)
  )

(defmethod (buffer-window set-screen) (new-screen)
  (when (not (eq screen new-screen))
    (let ((exposed? (=> screen exposed?))
	  (old-screen screen)
	  )
      (setf screen new-screen)
      (=> self &new-screen)
      (when exposed? (=> self expose) (=> old-screen deexpose))
      )))

(defmethod (buffer-window set-label) (new-label)
  % Specify a "label" object to write a label at the bottom of the screen.  NIL
  % implies that no label area is wanted.  If an object is specified, it
  % must support the following operations:

  % (=> label height)
  %     Return the number of lines occupied by the label area at the bottom
  %     of the buffer-window's virtual screen.
  % (=> label resize)
  %     Tell the label that the window has changed size.  This may cause
  %     the label to change its height, but should not cause a refresh.
  % (=> label refresh)
  %     This instructs the label object to refresh the label area.  The label
  %     area is assumed to be the bottom-most <height> lines on the
  %     buffer-window's virtual screen, although it could be on a totally
  %     different virtual screen, if desired (in which case the "height"
  %     operation should return 0).

  % This operation may change the number of lines available for text, which
  % may require adjusting the window position.  A refresh is not done
  % immediately.

  (setf label new-label)
  (setf label-refresh-method (if label (object-get-handler label 'refresh)))
  (=> self &new-size)
  )

(defmethod (buffer-window position) ()
  % If the window is selected, return the position of the buffer.  Otherwise,
  % return the "saved position".
  (or saved-position (=> buffer position)))

(defmethod (buffer-window line-position) ()
  (if saved-position
    (buffer-position-line saved-position)
    (=> buffer line-pos)
    ))

(defmethod (buffer-window char-position) ()
  (if saved-position
    (buffer-position-column saved-position)
    (=> buffer char-pos)
    ))

(defmethod (buffer-window set-position) (bp)
  % If the window is selected, set the buffer position.  Otherwise, set the
  % "saved position".
  (if saved-position
    (setf saved-position bp)
    (=> buffer set-position bp)
    ))

(defmethod (buffer-window set-line-position) (line)
  % If the window is selected, set the buffer position.
  % Otherwise, set the "saved position".

  (if saved-position
    (setf saved-position (buffer-position-create line 0))
    (=> buffer set-line-pos line)
    ))

(defmethod (buffer-window adjust-window) ()
  % Adjust the window position, if necessary, to ensure that the current
  % buffer location (if the window is selected) or the saved buffer location
  % (if the window is not selected) is within the window.
  (let ((line (=> self line-position)))
    (if (or (< line buffer-top) (>= line (+ buffer-top height)))
      % The desired line doesn't show in the window.
      (=> self readjust-window)
      )))

(defmethod (buffer-window readjust-window) ()
  % Adjust the window position to nicely show the current location.
  (let ((line (=> self line-position))
	(one-third-screen (/ height 3))
	)
    (=> self set-buffer-top
	(if (>= line (- (=> buffer size) one-third-screen))
	  (- line (* 2 one-third-screen))
	  (- line one-third-screen)
	  ))))

(defmethod (buffer-window adjust-buffer) ()
  % Adjust the buffer position, if necessary, to ensure that the current
  % buffer location is visible on the screen.  If the window position is
  % past the end of the buffer, it will be changed.
  (let ((size (=> buffer size)))
    (cond ((>= buffer-top size)
	   % The window is past the end of the buffer.
	   (=> self set-buffer-top (- size (/ height 3)))
	   )))
  (let ((line (=> buffer line-pos)))
    (cond ((or (< line buffer-top) (>= line (+ buffer-top height)))
	   % The current line doesn't show in the window.
	   (=> buffer set-line-pos (+ buffer-top (/ height 3)))
	   ))))

(defmethod (buffer-window set-buffer) (new-buffer)
  (setf buffer new-buffer)
  (setf buffer-left 0)
  (setf buffer-top 0)
  (if saved-position (setf saved-position (=> buffer position)))
  (=> self adjust-window)
  (=> self &reset)
  )

(defmethod (buffer-window set-buffer-top) (new-top)
  (cond ((<= new-top 0) (setf new-top 0))
	((>= new-top (=> buffer visible-size))
	 (setf new-top (- (=> buffer visible-size) 1)))
	)
  (setf buffer-top new-top)
  )

(defmethod (buffer-window set-buffer-left) (new-left)
  (when (~= new-left buffer-left)
    (if (< new-left 0) (setf new-left 0))
    (when (~= new-left buffer-left)
      (setf buffer-left new-left)
      (=> self &reset)
      )))

(defmethod (buffer-window set-size) (new-height new-width)
  % Change the size of the screen to have the specified height and width.
  % The size is adjusted to ensure that there is at least one row of text.

  (setf new-height (max new-height (+ label-height 1)))
  (setf new-width (max new-width 1))
  (when (or (~= new-height (=> screen height))
	    (~= new-width (=> screen width)))
    (=> screen set-size new-height new-width)
    (=> self &new-size)
    ))

(defmethod (buffer-window set-text-enhancement) (e-mask)
  (when (~= text-enhancement e-mask)
    (setf text-enhancement e-mask)
    (=> screen set-default-enhancement e-mask)
    (=> self &reset)
    ))

(defmethod (buffer-window refresh) (breakout-allowed)
  % Update the virtual screen (including the label) to correspond to the
  % current state of the attached buffer.  Return true if the refresh
  % was completed (no breakout occurred).

  (if (not (and breakout-allowed (input-available?)))
    (let ((buffer-end (=> buffer visible-size)))
      (for (from row 0 maxrow)
	   (for line-number buffer-top (+ line-number 1))
	   (do
	    % NIL is used to represent all EMPTY lines, so that EQ will work.
	    (let ((line (and (< line-number buffer-end)
			     (=> buffer fetch-line line-number))))
	      (if (and line (string-empty? line)) (setf line NIL))
	      (when (not (eq line (vector-fetch buffer-lines row)))
		(vector-store buffer-lines row line)
		(=> self &write-line-to-screen line row)
		)))
	   )
      (if (and label label-refresh-method)
	(apply label-refresh-method (list label)))
      (let* ((linepos (=> self line-position))
	     (charpos (=> self char-position))
	     (row (- linepos buffer-top))
	     (line (vector-fetch buffer-lines row))
	     (column (- (map-char-to-column line charpos) buffer-left))
	     )
	(=> screen set-cursor-position row column)
	)
      T % refresh completed
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private methods:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (buffer-window init) (init-plist)
  (=> self &new-screen)
  )

(defmethod (buffer-window &new-screen) ()
  (=> screen set-default-enhancement text-enhancement)
  (=> self &new-size)
  )

(defmethod (buffer-window &new-size) ()
  % The size of the screen and/or label may have changed.  Adjust
  % the internal state of the buffer-window accordingly.

  (if label (=> label resize)) % may change label height
  (setf label-height (if label (max 0 (=> label height)) 0))
  (setf height (- (=> screen height) label-height))
  (setf width (=> screen width))
  (setf maxrow (- height 1))
  (setf maxcol (- width 1))
  (setf buffer-lines (make-vector maxrow 'UNKNOWN))
  (setf line-buffer (make-string (+ maxcol 10) #\space))
  (=> self adjust-window) % ensure that cursor is still visible
  )

(defmethod (buffer-window &reset) ()
  % "Forget" information about displayed lines.
  (for (from i 0 maxrow)
       (do (vector-store buffer-lines i 'UNKNOWN))))

(defmethod (buffer-window &write-line-to-screen) (line row)
  (if (null line)
    (=> screen clear-to-eol row 0)
    % else
    (let ((count (=> self &compute-screen-line line)))
      (cond
       ((> count width)
	(=> screen write-string row 0 line-buffer maxcol)
	(=> screen write overflow-marker row maxcol)
	)
       (t
	(=> screen write-string row 0 line-buffer count)
	(=> screen clear-to-eol row count)
	)))))

(defmacro &write-char (ch)
  % Used by &COMPUTE-SCREEN-LINE.
  `(progn
    (if (>= line-index 0)
      (string-store line-buf line-index ,ch))
    (setf line-index (+ line-index 1))
    (setf line-column (+ line-column 1))
    ))

(defmethod (buffer-window &compute-screen-line) (line)
  % Internal method used by &WRITE-LINE-TO-SCREEN.  It fills the line buffer
  % with the appropriate characters and returns the number of characters in
  % the line buffer.

  (let ((line-buf line-buffer) % local variables are more efficient
	(line-column 0)
	(line-index (- buffer-left))
	(the-width width) % local variables are more efficient
	)
    (for (from i 0 (string-upper-bound line))
	 (until (> line-index the-width)) % have written past the right edge
	 (do (let ((ch (string-fetch line i)))
	       (cond
		((= ch #\TAB) % TABs are converted to spaces.
		 (let ((tabcol (& (+ line-column 8) (~ 7))))
		   (while (< line-column tabcol)
		     (&write-char #\space)
		     )))
		((or (< ch #\space) (= ch #\rubout))
		 % Control characters are converted to "uparrow" form.
		 (&write-char #/^)
		 (&write-char (^ ch 8#100))
		 )
		(t (&write-char ch))
		))))
    line-index
    ))

(de map-char-to-column (line n)
  % Map character position N to the corresponding display column index with
  % respect to the specified LINE.  Handle funny mapping of TABs and control
  % characters.

  (setf n (- n 1))
  (let ((upper-bound (string-upper-bound line)))
    (if (> n upper-bound) (setf n upper-bound)))
  (for* (from i 0 n)
	(with (col 0))
	(do (let ((ch (string-fetch line i)))
	      (cond
	       ((= ch #\TAB)
	        % TABs are converted to an appropriate number of spaces.
	        (setf col (& (+ col 8) (~ 7)))
	        )
	       ((or (< ch #\space) (= ch #\rubout))
	        % Control characters are converted to "uparrow" form.
	        (setf col (+ col 2))
	        )
	       (t
	        (setf col (+ col 1))
	        ))))
	(returns col)))

(de map-column-to-char (line n)
  % Map display column index N to the corresponding character position with
  % respect to the specified LINE.  Handle funny mapping of TABs and control
  % characters.

  (for* (from i 0 (string-upper-bound line))
	(with (col 0))
	(until (>= col n))
	(do (let ((ch (string-fetch line i)))
	      (cond
	       ((= ch #\TAB)
		% TABs are converted to an appropriate number of spaces.
		(setf col (& (+ col 8) (~ 7)))
		)
	       ((or (< ch #\space) (= ch #\rubout))
		% Control characters are converted to "uparrow" form.
	        (setf col (+ col 2))
		)
	       (t
		(setf col (+ col 1))
		))))
	(returns i)
	))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(undeclare-flavor buffer screen)
