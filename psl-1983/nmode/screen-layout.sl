%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Screen-Layout.SL
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        19 August 1982
% Revised:     18 February 1983
%
% This file contains functions that manage the screen layout for NMODE.
%
% 18-Feb-83 Alan Snyder
%  Add new function: find-buffer-in-exposed-windows.
% 16-Feb-83 Alan Snyder
%  Declare -> Declare-Flavor.
% 7-Feb-83 Alan Snyder
%  Revise handling of refresh breakout to allow refresh-one-window to work.
% 31-Jan-83 Alan Snyder
%  Revise for new interpretation of argument to buffer-window$set-size.
%  Make input window an unlabeled buffer-window.
% 27-Jan-83 Alan Snyder
%  Added (optional) softkey label screen.
% 7-Jan-83 Alan Snyder
%  Change ENTER-RAW-MODE to not touch the other screen unless we are in
%  two-screen mode.
% 6-Jan-83 Alan Snyder
%  Change NMODE-SELECT-MAJOR-WINDOW to also deexpose input window.
% 30-Dec-82 Alan Snyder
%  Added two-screen mode.  Minor change to NMODE-SELECT-WINDOW to make
%  things more graceful when using direct writing.
% 20-Dec-82 Alan Snyder
%  Added declarations and made other small changes to improve efficiency by
%  reducing the amount of run-time method lookup.  Fixed efficiency bug in
%  NMODE-NEW-TERMINAL: it failed to de-expose old screens and windows.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))
(CompileTime (load display-char))

% External variables used here:

(fluid '(
	 nmode-command-argument-given
	 nmode-command-argument
	 browser-split-screen
	 ))

% Options:

(fluid '(
  nmode-allow-refresh-breakout	% Abort refresh if user types something
  nmode-normal-enhancement	% Display enhancement for normal text
  nmode-inverse-enhancement	% Display enhancement for "inverse video" text
  ))

% Global variables defined here:

(fluid '(
  nmode-current-buffer		% buffer that commands operate on
  nmode-current-window		% window displaying current buffer
  nmode-major-window		% the user's idea of nmode-current-window 
  nmode-layout-mode		% either 1 or 2
  nmode-two-screens?		% T => each window has its own screen

  nmode-input-window		% window used for string input
  nmode-message-screen		% screen displaying NMODE "message"
  nmode-prompt-screen		% screen displaying NMODE "prompt"
  nmode-main-buffer		% buffer "MAIN"
  nmode-output-buffer		% buffer "OUTPUT" (used for PSL output)
  nmode-input-buffer		% internal buffer used for string input
  nmode-softkey-label-screen	% screen displaying softkey labels (or NIL)

  nmode-terminal		% the terminal object
  nmode-physical-screen		% the physical screen object
  nmode-screen			% the shared screen object

  nmode-other-terminal		% the other terminal object (two-screen mode)
  nmode-other-physical-screen	% the other physical screen object
  nmode-other-screen		% the other shared screen object
  ))

% Internal static variables:

(fluid '(
  nmode-top-window		% the top or full major window
  nmode-bottom-window		% the bottom major window
  full-refresh-needed		% next refresh should clear the screen first
  nmode-breakout-occurred?	% last refresh was interrupted
  nmode-total-lines		% total number of screen lines for window(s)
  nmode-top-lines		% number of screen lines for top window
  nmode-inverse-video?		% Display using "inverse video"
  nmode-blank-screen		% blank screen used to clear the display
  ))

(declare-flavor buffer-window 
		nmode-current-window
		nmode-top-window nmode-bottom-window nmode-input-window)
(declare-flavor virtual-screen
		nmode-blank-screen)
(declare-flavor shared-physical-screen
		nmode-screen
		nmode-other-screen)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialization Function:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-initialize-screen-layout ()

  % This function is called as part of NMODE initialization, which occurs
  % before NMODE is saved.

  (setf nmode-allow-refresh-breakout T)
  (setf nmode-normal-enhancement (dc-make-enhancement-mask))
  (setf nmode-inverse-enhancement
    (dc-make-enhancement-mask INVERSE-VIDEO INTENSIFY))
  (setf nmode-inverse-video? NIL)
  (nmode-default-terminal)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functions for changing the screen layout:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-1-window ()
  (nmode-expand-top-window)
  )

(de nmode-expand-top-window ()

  % This function does nothing if already in 1-window mode.
  % Otherwise: expands the top window to fill the screen; the top window
  % becomes current.

  (when (not (= nmode-layout-mode 1))
     (nmode-select-window nmode-top-window)
     (=> nmode-bottom-window deexpose)
     (setf nmode-layout-mode 1)
     (nmode-set-window-sizes)
     ))

(de nmode-expand-bottom-window ()

  % This function does nothing if already in 1-window mode.
  % Otherwise: expands the bottom window to fill the screen; the bottom
  % window becomes current.

  (when (not (= nmode-layout-mode 1))
     (psetf nmode-top-window nmode-bottom-window
	    nmode-bottom-window nmode-top-window)
     (nmode-expand-top-window)
     ))

(de nmode-2-windows ()

  % This function does nothing if already in 2-window mode.
  % Otherwise: shrinks the top window and exposes the bottom window.

  (cond
    ((not (= nmode-layout-mode 2))
     (setf nmode-layout-mode 2)
     (nmode-set-window-sizes)
     )))

(de nmode-set-window-position (p)
  (selectq p
    (FULL (nmode-1-window))
    (TOP (nmode-2-windows) (nmode-select-window nmode-top-window))
    (BOTTOM (nmode-2-windows) (nmode-select-window nmode-bottom-window))
    ))

(de nmode-exchange-windows ()

  % Exchanges the current window with the other window, which becomes current.
  % In two window mode, the windows swap physical positions.

  (let ((w (nmode-other-window)))
    (psetf nmode-top-window nmode-bottom-window
	   nmode-bottom-window nmode-top-window)
    (nmode-set-window-sizes)
    (nmode-select-window w)
    ))

(de nmode-grow-window (n)
  % Increase (decrease if n<0) the size of the current window by N lines.
  % Does nothing and returns NIL if not in 2-window mode.

  (selectq (nmode-window-position)
    (FULL
     NIL
     )
    (TOP
     (setf nmode-top-lines (+ nmode-top-lines n))
     (nmode-set-window-sizes)
     T
     )
    (BOTTOM
     (setf nmode-top-lines (- nmode-top-lines n))
     (nmode-set-window-sizes)
     T
     )))

(de nmode-expose-output-buffer (b)

  % Buffer B is being used as an output channel.  It is not currently being
  % displayed.  Cause it to be displayed (in the "other window", if we
  % are already in 2-window mode, in the bottom window otherwise).

  (nmode-2-windows)
  (window-select-buffer (nmode-other-window) b)
  )

(de nmode-normal-video ()
  % Cause the display to use "normal" video polarity.
  (when nmode-inverse-video?
    (setf nmode-inverse-video? NIL)
    (nmode-establish-video-polarity)
    ))

(de nmode-inverse-video ()
  % Cause the display to use "inverse" video polarity.
  (when (not nmode-inverse-video?)
    (setf nmode-inverse-video? T)
    (nmode-establish-video-polarity)
    ))

(de nmode-invert-video ()
  % Toggle between normal and inverse video.
  (setf nmode-inverse-video? (not nmode-inverse-video?))
  (nmode-establish-video-polarity)
  )

(de nmode-use-two-screens ()
  % If two screens are available, use them both.
  (when (and nmode-other-screen (not nmode-two-screens?))
    (when (not (=> nmode-other-terminal raw-mode))
      (=> nmode-other-terminal enter-raw-mode)
      (setf full-refresh-needed t)
      )
    (setf nmode-two-screens? T)
    (setf browser-split-screen T)
    (setf nmode-layout-mode 2)
    (nmode-set-window-sizes)
    ))

(de nmode-use-one-screen ()
  % Use only the main screen.
  (when nmode-two-screens?
    (setf nmode-two-screens? NIL)
    (nmode-set-window-sizes)
    (if nmode-other-screen (=> nmode-other-screen refresh)) % clear it
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Screen Layout Commands:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de one-window-command ()

  % The "C-X 1" command.  Return to one window mode.

  (when (not (= nmode-layout-mode 1))
    (if nmode-command-argument-given
	(nmode-expand-bottom-window)
	(nmode-expand-top-window)
	)))

(de two-windows-command ()

  % The "C-X 2" command.  The bottom window is selected.

  (when (not (= nmode-layout-mode 2))
    (nmode-2-windows)
    (if nmode-command-argument-given
	(window-copy-buffer nmode-top-window nmode-bottom-window))
    (nmode-switch-windows)
    ))

(de view-two-windows-command ()

  % The "C-X 3" command.  The top window remains selected.

  (when (not (= nmode-layout-mode 2))
    (nmode-2-windows)
    (if nmode-command-argument-given
	(window-copy-buffer nmode-top-window nmode-bottom-window))
    ))

(de grow-window-command ()
  (if (not (nmode-grow-window nmode-command-argument))
     (nmode-error "Not in 2-window mode!")
     ))

(de other-window-command ()
  (let ((old-buffer nmode-current-buffer))
    (nmode-switch-windows)
    (if nmode-command-argument-given
      (buffer-select old-buffer))
    ))

(de exchange-windows-command ()
  (selectq nmode-layout-mode
    (1 (Ding))
    (2 (nmode-exchange-windows))
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Window Selection Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-select-window (window)

  % Expose the specified window and make it the "current" window.
  % Its buffer becomes the "current" buffer.  This is the only function that
  % should set the variable "NMODE-CURRENT-WINDOW".

  (when (not (eq window nmode-current-window))
    (if nmode-current-window (=> nmode-current-window deselect))
    (when (not (eq window nmode-input-window))
      (setf nmode-major-window window)
      (when (not (eq nmode-current-window nmode-input-window))
	(reset-message)
	))
    (setf nmode-current-window window)
    (=> window expose)
    (=> window select)
    (setf nmode-current-buffer (=> window buffer))
    (nmode-establish-current-mode)
    ))

(de nmode-switch-windows ()

  % Select the "other" window.

  (selectq nmode-layout-mode
    (2 (nmode-select-window (nmode-other-window)))
    (1 (nmode-exchange-windows))
    ))

(de nmode-select-major-window ()

  % This function is used for possible error recovery.  It ensures that the
  % current window is one of the exposed major windows (not, for example, the
  % INPUT window) and that the INPUT window is deexposed.

  (if (not (or (eq nmode-current-window nmode-top-window)
	       (eq nmode-current-window nmode-bottom-window)
	       ))
    (nmode-select-window nmode-top-window)
    )
  (=> nmode-input-window deexpose)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Screen Information Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-window-position ()
  (cond ((= nmode-layout-mode 1) 'FULL)
	((eq nmode-current-window nmode-top-window) 'TOP)
	(t 'BOTTOM)
	))

(de nmode-other-window ()

  % Return the "other" window.

  (if (eq nmode-current-window nmode-top-window)
      nmode-bottom-window
      nmode-top-window
      ))

(de find-buffer-in-windows (b)

  % Return a list containing the windows displaying the specified buffer.
  % The windows may or may not be displayed.

  (for (in w (list nmode-bottom-window nmode-top-window))
	% Put bottom window first in this list so that it will be
	% the one that is automatically adjusted on output if the
	% output buffer is being displayed by both windows.
       (when (eq b (=> w buffer)))
       (collect w))
  )

(de find-buffer-in-exposed-windows (b)

  % Return a list containing the exposed windows displaying the specified
  % buffer.

  (for (in w (find-buffer-in-windows b))
       (when (=> w exposed?))
       (collect w))
  )

(de buffer-is-displayed? (b)

  % Return T if the specified buffer is being displayed by an active window.

  (not
    (for (in w (nmode-active-windows))
         (never (eq b (=> w buffer)))
	 )))

(de nmode-active-windows ()
  (selectq nmode-layout-mode
    (1 (list nmode-top-window))
    (2 (list nmode-top-window nmode-bottom-window))
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Typeout Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-begin-typeout ()

  % Call this function before doing typeout using the standard output channel.
  % Someday this will do something clever, but for now it merely clears the
  % screen.

  (nmode-clear-screen)
  )

(de nmode-end-typeout ()

  % Call this function after doing typeout using the standard output channel.
  % Someday this will do something clever, but for now it merely waits for
  % the user to type a character.

  (pause-until-terminal-input)
  )

(de nmode-clear-screen ()

  % This is somewhat of a hack to clear the screen for normal typeout.  The
  % next time a refresh is done, a full refresh will be done automatically.

  (=> nmode-blank-screen expose)
  (=> nmode-screen full-refresh NIL)
  (setf full-refresh-needed t)
  )

(de Enter-Raw-Mode ()

  % Use this function to enter "raw mode", in which terminal input is not
  % echoed and special terminal keys are enabled.  The next REFRESH will
  % automatically be a "full" refresh.

  (when (not (=> nmode-terminal raw-mode))
    (=> nmode-terminal enter-raw-mode)
    (setf full-refresh-needed t)
    )  
  (when (and nmode-two-screens?
	     nmode-other-terminal
	     (not (=> nmode-other-terminal raw-mode)))
    (=> nmode-other-terminal enter-raw-mode)
    (setf full-refresh-needed t)
    )
  )

(de leave-raw-mode ()

  % Use this function to leave "raw mode", i.e. turn on echoing of terminal
  % input and disable any special terminal keys.  The cursor is positioned
  % on the last line of the screen, which is cleared.

  (when (=> nmode-terminal raw-mode)
    (=> nmode-terminal move-cursor (=> nmode-terminal maxrow) 0)
    (=> nmode-terminal clear-line)
    (=> nmode-terminal leave-raw-mode)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Refresh functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-refresh ()
  % This function refreshes the screen.  It first ensures that all exposed
  % NMODE windows update their corresponding virtual screens.  Then, it
  % asks the window package to update the display.  A "full refresh" will
  % be done if some prior operation has indicated the need for one.

  (cond (full-refresh-needed
	 (nmode-full-refresh))
	(t
	 (nmode-refresh-windows)
	 (when (not nmode-breakout-occurred?)
	   (=> nmode-screen refresh nmode-allow-refresh-breakout)
	   (if (and nmode-other-screen nmode-two-screens?)
	     (=> nmode-other-screen refresh nmode-allow-refresh-breakout))
	   ))))

(de nmode-full-refresh ()
  % This function refreshes the screen after first clearing the terminal
  % display.  It it used when the state of the terminal display is in doubt.

  (nmode-refresh-windows)
  (when (not (setf full-refresh-needed nmode-breakout-occurred?))
    (=> nmode-screen full-refresh nil)
    (if (and nmode-other-screen nmode-two-screens?)
      (=> nmode-other-screen full-refresh nil))
    ))

(de nmode-refresh-one-window (w)
  % This function refreshes the display, but only updates the virtual screen
  % corresponding to the specified window.

  (cond (full-refresh-needed
	 (nmode-full-refresh))
	(nmode-breakout-occurred?
	 (nmode-refresh))
	(t
	 (if (eq (=> nmode-screen owner 0 0) nmode-blank-screen) % hack!
	   (=> nmode-blank-screen deexpose))
	 (nmode-adjust-window w)
	 (nmode-refresh-window w)
	 (nmode-refresh-screen (=> (=> w screen) screen))
	 )))

(de nmode-refresh-virtual-screen (s)
  % This function refreshes the shared screen containing the specified
  % virtual screen.

  (cond (full-refresh-needed
	 (nmode-full-refresh))
	(nmode-breakout-occurred?
	 (nmode-refresh))
	(t
	 (if (eq (=> nmode-screen owner 0 0) nmode-blank-screen) % hack!
	   (=> nmode-blank-screen deexpose))
	 (nmode-refresh-screen (=> s screen))
	 )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-refresh-windows ()
  % Cause all windows to update their corresponding virtual screens.  The
  % variable nmode-breakout-occurred? is set to T if the refresh is
  % interrupted by user input.

  (setf nmode-breakout-occurred? NIL)
  (=> nmode-blank-screen deexpose) % hack!
  (=> nmode-current-window adjust-window)
  (nmode-refresh-window nmode-top-window)
  (nmode-refresh-window nmode-bottom-window)
  (nmode-refresh-window nmode-input-window)
  )

(de nmode-refresh-window (w)
  % Refresh only if window is exposed and no breakout has occurred.
  % Update the flag nmode-breakout-occurred?

  (if (not nmode-breakout-occurred?)
    (if (eq (object-type w) 'buffer-window) % hack for efficiency
      (if (buffer-window$exposed? w)
	(setf nmode-breakout-occurred?
	  (not (buffer-window$refresh w nmode-allow-refresh-breakout))))
      (if (=> w exposed?)
	(setf nmode-breakout-occurred?
	  (not (=> w refresh nmode-allow-refresh-breakout))))
      )))

(de nmode-refresh-screen (s)
  % Refresh the specified shared-screen.

  (if (eq (object-type s) 'shared-physical-screen) % hack for efficiency
    (shared-physical-screen$refresh s nmode-allow-refresh-breakout)
    (=> s refresh nmode-allow-refresh-breakout)
    ))

(de nmode-establish-video-polarity ()
  (let ((mask (if nmode-inverse-video?
		nmode-inverse-enhancement
		nmode-normal-enhancement
		)))
    (=> nmode-top-window set-text-enhancement mask)
    (=> nmode-bottom-window set-text-enhancement mask)
    (=> nmode-input-window set-text-enhancement mask)
    (=> nmode-prompt-screen set-default-enhancement mask)
    (=> nmode-message-screen set-default-enhancement mask)
    (=> nmode-blank-screen set-default-enhancement mask)
    (=> nmode-prompt-screen clear)
    (rewrite-message)
    (=> nmode-blank-screen clear)
    ))

(de nmode-new-terminal ()
  % This function should be called when either NMODE-TERMINAL or
  % NMODE-OTHER-TERMINAL changes.

  (setf full-refresh-needed T)
  (setf nmode-physical-screen (create-physical-screen nmode-terminal))
  (setf nmode-other-physical-screen
    (if nmode-other-terminal
      (create-physical-screen nmode-other-terminal)))
  (if nmode-screen
    (=> nmode-screen set-screen nmode-physical-screen)
    (setf nmode-screen (create-shared-physical-screen nmode-physical-screen))
    )
  (nmode-setup-softkey-label-screen nmode-screen)
  (if nmode-other-terminal
    (if nmode-other-screen
      (=> nmode-other-screen set-screen nmode-other-physical-screen)
      (setf nmode-other-screen
	(create-shared-physical-screen nmode-other-physical-screen))
      )
    (setf nmode-other-screen nil)
    )
  (let ((height (=> nmode-screen height))
	(width (=> nmode-screen width))
	)
    (when nmode-softkey-label-screen
      (setf height (- height (=> nmode-softkey-label-screen height)))
      )
    (setf nmode-total-lines (- height 2)) % all but message and prompt lines
    (setf nmode-top-lines (/ nmode-total-lines 2)) % half for the top window

    % Throw away the old windows and screens!
    (if nmode-blank-screen (=> nmode-blank-screen deexpose))
    (if nmode-message-screen (=> nmode-message-screen deexpose))
    (if nmode-prompt-screen (=> nmode-prompt-screen deexpose))
    (if nmode-input-window (=> nmode-input-window deexpose))

    % Create new windows and screens:
    (setf nmode-blank-screen % hack to implement clear screen
      (nmode-create-screen height width 0 0))
    (setf nmode-message-screen (nmode-create-screen 1 width (- height 2) 0))
    (setf nmode-prompt-screen (nmode-create-screen 1 width (- height 1) 0))
    (setf nmode-input-window
      (create-unlabeled-buffer-window nmode-input-buffer
        (nmode-create-screen 1 width (- height 1) 0)))
    (nmode-fixup-windows)
    (setf nmode-layout-mode (if nmode-two-screens? 2 1))
    (=> nmode-message-screen expose)
    (=> nmode-prompt-screen expose)
    (nmode-select-window nmode-top-window)
    (nmode-establish-video-polarity)
    (nmode-set-window-sizes)
    ))

(de nmode-create-screen (height width row-origin column-origin)
  (make-instance 'virtual-screen
		 'screen nmode-screen
		 'height height
		 'width width
		 'row-origin row-origin
		 'column-origin column-origin)
  )

(de nmode-set-window-sizes ()
  % This function ensures that the top and bottom windows are properly
  % set up and exposed.

  (cond ((< nmode-top-lines 2)
	 (setf nmode-top-lines 2))
	((> nmode-top-lines (- nmode-total-lines 2))
	 (setf nmode-top-lines (- nmode-total-lines 2)))
	)
  (nmode-fixup-windows)
  (cond
   (nmode-two-screens?
    (nmode-position-window nmode-top-window nmode-total-lines 0)
    (nmode-position-window nmode-bottom-window nmode-total-lines 0)
    (nmode-expose-both-windows)
    )
   ((= nmode-layout-mode 1)
    (nmode-position-window nmode-top-window nmode-total-lines 0)
    (nmode-position-window nmode-bottom-window nmode-total-lines 0)
    (=> nmode-top-window expose)
    )
   ((= nmode-layout-mode 2)
    (nmode-position-window nmode-top-window nmode-top-lines 0)
    (nmode-position-window nmode-bottom-window
			   (- nmode-total-lines nmode-top-lines)
			   nmode-top-lines
			   )
    (nmode-expose-both-windows)
    )))

(de nmode-position-window (w height origin)
  (if (eq (=> (=> w screen) screen) nmode-other-screen)
    (setf height (=> nmode-other-screen height)))
  (=> w set-size height (=> w width))
  (let ((s (=> w screen)))
    (=> s set-origin origin 0))
  )

(de nmode-expose-both-windows ()
  (cond ((eq nmode-top-window nmode-current-window)
	 (=> nmode-bottom-window expose)
	 (=> nmode-top-window expose)
	 )
	(t
	 (=> nmode-top-window expose)
	 (=> nmode-bottom-window expose)
	 )))

(de nmode-fixup-windows ()
  % Ensure that the two buffer-windows exist and are attached to the proper
  % shared-screens.

  (let ((top-screen (if (and nmode-two-screens? nmode-other-screen)
		      nmode-other-screen
		      nmode-screen
		      ))
	(bottom-screen nmode-screen)
	)
    (if (or (not nmode-top-window)
	    (neq (=> (=> nmode-top-window screen) screen) top-screen)
	    )
      (nmode-create-top-window)
      )
    (if (or (not nmode-bottom-window)
	    (neq (=> (=> nmode-bottom-window screen) screen) bottom-screen)
	    )
      (nmode-create-bottom-window)
      )
    ))

(de nmode-create-top-window ()
  (let ((vs (if (and nmode-two-screens? nmode-other-screen)
	      (make-instance 'virtual-screen
			     'screen nmode-other-screen
			     'height (=> nmode-other-screen height)
			     'width (=> nmode-other-screen width)
			     'row-origin 0
			     )
	      (make-instance 'virtual-screen
			     'screen nmode-screen
			     'height nmode-total-lines
			     'width (=> nmode-screen width)
			     'row-origin 0
			     )))
	)
    (if nmode-top-window
      (=> nmode-top-window set-screen vs)
      (setf nmode-top-window (create-buffer-window nmode-main-buffer vs))
      )))

(de nmode-create-bottom-window ()
  (let ((vs (make-instance 'virtual-screen
			   'screen nmode-screen
			   'height nmode-total-lines
			   'width (=> nmode-screen width)
			   'row-origin 0
			   ))
	)
    (if nmode-bottom-window
      (=> nmode-bottom-window set-screen vs)
      (setf nmode-bottom-window (create-buffer-window nmode-output-buffer vs))
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(undeclare-flavor nmode-top-window nmode-bottom-window nmode-input-window
		  nmode-current-window nmode-blank-screen nmode-screen)
