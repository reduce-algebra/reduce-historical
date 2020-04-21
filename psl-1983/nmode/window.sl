%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Window.SL - Commands and Functions for manipulating windows.
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        24 August 1982
% Revised:     30 December 1982
%
% 30-Dec-82 Alan Snyder
%  Change scrolling commands to Ding if no scrolling is actually done.  Fix bug
%  in backwards scroll by pages that failed to preserve relative cursor
%  position.  Change behavior of scroll-by-pages upon excessive request.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects fast-int))

(fluid '(nmode-current-window
	 nmode-command-argument
	 nmode-command-number-given
	 nmode-command-argument-given
	 nmode-layout-mode
	 ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de current-window-height ()
  % Return the number of text lines displayable on the current window.
  (=> nmode-current-window height))

(de current-window-top-line ()
  % Return the index of the buffer line at the top of the current window.
  (=> nmode-current-window buffer-top)
  )

(de current-window-set-top-line (new-top-line)
  % Change which buffer line displays at the top of the current window.
  (=> nmode-current-window set-buffer-top new-top-line)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Window Scrolling Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de scroll-window-according-to-command (w)
  % Scroll the contents of the specified window according to the command
  % argument.  If the command argument was set by C-U or C-U -, then scroll the
  % contents of the window up or down one page.  Otherwise, scroll the window up
  % or down the specified number of lines.

  (if (and (or (= nmode-command-argument 1) (= nmode-command-argument -1))
	   (not nmode-command-number-given))
    (scroll-window-by-pages w nmode-command-argument)
    (scroll-window-by-lines w nmode-command-argument)
    ))

(de scroll-window-by-lines (w n)
  % Scroll the contents of the window up (n > 0) or down (n < 0) by |n| lines.
  % The "window position" may be adjusted to keep it within the window.  Ding if
  % the window contents does not move.

  (let* ((old-top-line (=> w buffer-top))
	 (new-top-line (+ old-top-line n))
	 )

    % adjust to keep something in the window
    (let ((buffer-last-line (- (=> (=> w buffer) visible-size) 1)))
      (cond
       ((< new-top-line 0) (setf new-top-line 0))
       ((> new-top-line buffer-last-line) (setf new-top-line buffer-last-line))
       ))

    % adjust "window position" if no longer in window
    (let ((line (=> w line-position))
	  (max (+ new-top-line (- (=> w height) 1)))
	  )
      (cond
       ((< line new-top-line) (=> w set-line-position new-top-line))
       ((> line max) (=> w set-line-position max))
       ))

    (if (~= old-top-line new-top-line)
      (=> w set-buffer-top new-top-line)
      (Ding)
      )))

(de scroll-window-by-pages (w n)
  % Scroll the contents of the window up (n > 0) or down (n < 0) by |n|
  % screenfuls.  The "window position" may be adjusted to keep it within the
  % window.  Ding if the window contents does not move.

  (let* ((old-top-line (=> w buffer-top))
	 (window-height (=> w height))
	 (buffer-last-line (- (=> (=> w buffer) visible-size) 1))
	 (new-top-line old-top-line)
         )
    (if (>= n 0)
      % moving towards the end of the buffer
      (for (from i 1 n) % do as many complete screenfuls as possible
	   (do (let ((next-top-line (+ new-top-line window-height)))
		 (if (<= next-top-line buffer-last-line)
		   (setf new-top-line next-top-line)
		   (exit)
		   ))))
      % moving towards the beginning of the buffer
      (setf new-top-line (max 0 (+ new-top-line (* n window-height))))
      )
    (if (~= new-top-line old-top-line)
      % keep the cursor at the same relative location in the window!
      (let ((delta (- new-top-line old-top-line)))
	(=> w set-line-position
	    (min (+ (=> w line-position) delta) (+ buffer-last-line 1)))
	(=> w set-buffer-top new-top-line)
	)
      % otherwise (no change)
      (Ding)
      )))

(de scroll-window-horizontally (w n)

  % Scroll the contents of the specified window left (n > 0) or right (n < 0)
  % by |n| columns.

  (let ((old-buffer-left (=> w buffer-left)))
    (=> w set-buffer-left (+ old-buffer-left n))
    (if (= old-buffer-left (=> w buffer-left)) (Ding))
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Window Scrolling Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de next-screen-command ()
  (scroll-window-according-to-command nmode-current-window)
  )

(de previous-screen-command ()
  (setf nmode-command-argument (- 0 nmode-command-argument))
  (scroll-window-according-to-command nmode-current-window)
  )

(de scroll-other-window-command ()
  (selectq nmode-layout-mode
    (1 (Ding))
    (2 (scroll-window-according-to-command (nmode-other-window)))
    ))

(de scroll-window-up-line-command ()
  (scroll-window-by-lines nmode-current-window nmode-command-argument)
  )

(de scroll-window-down-line-command ()
  (scroll-window-by-lines nmode-current-window (- nmode-command-argument))
  )

(de scroll-window-up-page-command ()
  (scroll-window-by-pages nmode-current-window nmode-command-argument)
  )

(de scroll-window-down-page-command ()
  (scroll-window-by-pages nmode-current-window (- nmode-command-argument))
  )

(de scroll-window-right-command ()
  (scroll-window-horizontally nmode-current-window nmode-command-argument)
  )

(de scroll-window-left-command ()
  (scroll-window-horizontally nmode-current-window (- nmode-command-argument))
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Window Adjusting Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-adjust-window (w)
  % Adjust BUFFER-TOP to show current position.

  (=> w adjust-window)
  )

(de move-to-screen-edge-command ()
  (let* ((n nmode-command-argument)
	 (line (current-line-pos))
	 (top (current-window-top-line))
	 (height (current-window-height))
	 )
    (set-line-pos (+ top
		     (cond ((not nmode-command-argument-given) (/ height 2))
			   ((>= n 0) n)
			   (t (+ height n))
			   )))))
