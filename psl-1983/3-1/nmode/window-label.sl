%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Window-Label.SL
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        31 January 1983
% Revised:     14 March 1983
%
% A Window-Label object maintains the "label" portion of a buffer-window.
% This always occupies the lowermost "n" lines of the virtual screen,
% where "n" is 1 by default in this implementation.
%
% 14-Mar-83 Alan Snyder
%  Extend to handle buffers with no name.  Extend to display label-string 
%  attribute of buffers.
% 16-Feb-83 Alan Snyder
%  Declare -> Declare-Flavor.
% 10-Feb-83 Alan Snyder
%  Fix bug: minor modes did not display.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))
(CompileTime (load fast-int fast-vectors fast-strings display-char))

(de create-window-label (w)
  % Create a window-label object that will maintain the label portion
  % of the specified buffer-window.
  (make-instance 'window-label 'window w))

(defflavor window-label
  (window			% the buffer-window object

   (height 1)			% number of screen rows occupied by the label
   minrow			% location of top row of the label
   maxrow			% location of the bottom row of the label
   width			% width of the screen
   maxcol			% highest numbered screen column

   pos				% current position while writing label
   screen			% output screen while writing label

   (label-enhancement (dc-make-enhancement-mask INVERSE-VIDEO))
   (prompt-enhancement (dc-make-enhancement-mask INVERSE-VIDEO INTENSIFY))

   % The following instance variables store the various information used
   % in the construction of the label as currently displayed.  This information
   % is saved so that it can be compared against the current information
   % to determine whether the displayed label needs to be recomputed.

   (buffer-name NIL)		% name of buffer (as displayed)
   (buffer-mode NIL)		% buffer's mode (as displayed)
   (minor-modes NIL)		% minor mode list (as displayed)
   (buffer-file NIL)		% buffer's filename (as displayed)
   (buffer-top NIL)		% buffer-top (as used in label)
   (buffer-left NIL)		% buffer-left (as used in label)
   (buffer-size NIL)		% current buffer size (as used in label)
   (buffer-modified NIL)	% buffer-modified flag (as used in label)
   (current-window NIL)		% current-window (at time label was written)
   (prompt-string NIL)		% PromptString* (at time label was written)
   (label-string NIL)		% label-string attribute of buffer
   (browser-filter-count NIL)	% filter count for browser buffer
   )
  ()
  (gettable-instance-variables
   height
   )
  (settable-instance-variables
   label-enhancement
   prompt-enhancement
   )
  (initable-instance-variables
   window
   height
   )
  )

(fluid '(nmode-major-window nmode-output-buffer nmode-minor-modes))

(declare-flavor text-buffer buffer)
(declare-flavor buffer-window window)
(declare-flavor virtual-screen screen)
(declare-flavor browser browser)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public methods:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (window-label refresh) ()

  % Update the label are to correspond to the
  % current state of the attached buffer window.
  % Conditionally rewrite the entire label, if any relevant
  % information has changed.

  (let* ((buffer (=> window buffer))
	 (browser (=> buffer get 'browser))
	 )
    (if (not (and (eq buffer-name (=> buffer name))
		  (eq buffer-mode (=> buffer mode))
		  (eq minor-modes nmode-minor-modes)
		  (eq buffer-file (=> buffer file-name))
		  (= buffer-top (=> window buffer-top))
		  (= buffer-left (=> window buffer-left))
		  (= buffer-size (=> buffer visible-size))
		  (eq buffer-modified (=> buffer modified?))
		  (eq current-window nmode-major-window)
		  (eq prompt-string PromptString*)
		  (eq label-string (=> buffer label-string))
		  (eq browser-filter-count
		      (when browser (=> browser filter-count)))
		  ))
      (=> self &rewrite)
      )))

(defmethod (window-label resize) ()
  % This method must be invoked whenever the window's size may have changed.
  (setf screen (=> window screen))
  (setf width (=> screen width))
  (setf maxrow (- (=> screen height) 1))
  (setf minrow (- maxrow (- height 1)))
  (setf maxcol (- width 1))
  (setf buffer-name T) % force complete rewrite
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private methods:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (window-label init) (init-plist)
  (=> self resize)
  )

(defmethod (window-label &rewrite) ()
  % Unconditionally rewrite the entire label.
  (let* ((buffer (=> window buffer))
	 (browser (=> buffer get 'browser))
	 )
    (setf screen (=> window screen))
    (setf buffer-name (=> buffer name))
    (setf buffer-mode (=> buffer mode))
    (setf minor-modes nmode-minor-modes)
    (setf buffer-file (=> buffer file-name))
    (setf buffer-top (=> window buffer-top))
    (setf buffer-left (=> window buffer-left))
    (setf buffer-size (=> buffer visible-size))
    (setf buffer-modified (=> buffer modified?))
    (setf current-window nmode-major-window)
    (if PromptString* (setf prompt-string PromptString*))
    (setf label-string (=> buffer label-string))
    (setf browser-filter-count (when browser (=> browser filter-count)))
    (let ((old-enhancement (=> screen default-enhancement)))
      (=> screen set-default-enhancement label-enhancement)
      (setf pos 0)
      (if (eq window current-window)
	(=> self &write-string "NMODE ")
	(=> self &write-string "      "))
      (=> self &write-string (=> buffer-mode name))
      (if (and minor-modes (eq window current-window))
	(let ((leader-string " ("))
	  (for (in minor-mode minor-modes)
	       (do 
		(=> self &write-string leader-string)
		(setf leader-string " ")
		(=> self &write-string (=> minor-mode name))
		))
	  (=> self &write-string ")")
	  ))
      % Omit the buffer name if it is directly derived from the file name.
      (cond ((and buffer-name
		  (or (not buffer-file)
		      (not (string= buffer-name
				    (filename-to-buffername buffer-file)))
		      ))
	     (=> self &write-string " [")
	     (=> self &write-string buffer-name)
	     (=> self &write-string "]")
	     ))
      (when buffer-file
	(=> self &write-string " ")
	(=> self &write-string buffer-file)
	)
      (when (and label-string (not (string-empty? label-string)))
	(=> self &write-string " ")
	(=> self &write-string label-string)
	)
      (when (and browser-filter-count (> browser-filter-count 0))
	(=> self &write-string
	    (bldmsg " <%w %w>"
		    browser-filter-count
		    (if (~= browser-filter-count 1) "filters" "filter")
		    ))
	)
      (when (> buffer-left 0)
	(=> self &write-string (bldmsg " >%d" buffer-left))
	)
      (cond
       ((and (= buffer-top 0) (<= buffer-size (=> window height)))
	% The entire buffer is showing on the screen.
	% Do nothing.
	)
       ((= buffer-top 0)
	% The window is showing the top end of the buffer.
	(=> self &write-string " --TOP--")
	)
       ((>= buffer-top (- buffer-size (=> window height)))
	% The window is showing the bottom end of the buffer.
	(=> self &write-string " --BOT--")
	)
       (t % Otherwise...
	(let ((percentage (/ (* buffer-top 100) buffer-size)))
	  (=> self &write-string " --")
	  (=> self &write-char (+ #/0 (/ percentage 10)))
	  (=> self &write-char (+ #/0 (// percentage 10)))
	  (=> self &write-string "%--")
	  )))
      (if buffer-modified
	(=> self &write-string " *"))
      (when (and (StringP prompt-string) (eq buffer nmode-output-buffer))
	(=> self &write-string " ")
	(=> self &advance-pos (- width (string-length prompt-string)))
	(=> screen set-default-enhancement prompt-enhancement)
	(=> self &write-string prompt-string)
	)
      (=> screen clear-to-eol maxrow pos)
      (=> screen set-default-enhancement old-enhancement)
      )))

(defmethod (window-label &write-string) (string)
  (for (from i 0 (string-upper-bound string))
       (do (=> screen write (string-fetch string i) maxrow pos)
	   (setf pos (+ pos 1))
	   )))

(defmethod (window-label &write-char) (ch)
  (=> screen write ch maxrow pos)
  (setf pos (+ pos 1))
  )

(defmethod (window-label &advance-pos) (col)
  (while (< pos col) (=> self &write-char #\space))
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(undeclare-flavor buffer screen window browser)
