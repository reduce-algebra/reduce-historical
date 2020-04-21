%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Browser-Support.SL - General Browser Support
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        18 October 1982
% Revised:     3 February 1983
%
% 3-Feb-83 Alan Snyder
%  Revised to use Browser objects.
%
% This file contains support functions for browsers, such as the Buffer
% Browser and DIRED.  A browser is a buffer that displays a set of items,
% one item per line, and allows the individual items to be manipulated.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(compiletime (load numeric-operators))
(on fast-integers)

% External variables:

(fluid '(
  nmode-current-buffer
  nmode-current-window
  nmode-command-argument
  nmode-command-argument-given
  ))

% Global options:

(fluid '(
  browser-split-screen
  ))
(setf browser-split-screen NIL)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% General Browser Support Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de browser-enter (b)
  % Start up a browser using the buffer B.
  (=> b set-previous-buffer nmode-current-buffer)
  (let ((wp (nmode-window-position)))
    (=> b put 'window-status wp)
    (if browser-split-screen
	(if (eq wp 'bottom) (nmode-switch-windows))
	(nmode-1-window)
	))
  (buffer-select b)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Browser commands: attach these to keys in your browser mode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de browser-kill-and-exit-command ()
  (browser-kill-deleted-items-command)
  (browser-exit-command)
  )

(de browser-exit-command ()
  (let ((ws (=> nmode-current-buffer get 'window-status))
	(browser (=> nmode-current-buffer get 'browser))
	)
    (window-kill-buffer)
    (nmode-set-window-position ws)
    (=> browser exit)
    ))
	     
(de browser-delete-command ()
  % Mark items as 'deleted'.
  (browser-do-repeated-command 'delete-item () nil)
  )

(de browser-undelete-command ()
  % Mark items as not 'deleted'.
  (browser-do-repeated-command 'undelete-item () nil)
  )
  
(de browser-undelete-backwards-command ()
  % Mark items as not 'deleted'.
  (setf nmode-command-argument (- nmode-command-argument))
  (browser-do-repeated-command 'undelete-item () nil)
  )
  
(de browser-kill-command ()
  % Kill items.
  (browser-do-repeated-command 'kill-item () t)
  )

(de browser-ignore-command ()
  % Ignore items: filter them out.
  (browser-do-repeated-command 'ignore-item () t)
  )
  
(de browser-view-command ()
  % View the current item.
  (let* ((use-other (xor browser-split-screen nmode-command-argument-given))
	 (w (if use-other (nmode-other-window) nmode-current-window))
	 )
    (if (browser-view-item w)
      (if use-other
	(nmode-2-windows) % display the other window
	(set-message "C-M-L returns to browser.")
	)
      (Ding)
      )))

(de browser-edit-command ()
  % Edit the current item.
  (let* ((use-other (xor browser-split-screen nmode-command-argument-given))
	 (w (if use-other (nmode-other-window) nmode-current-window))
	 )
    (if (browser-view-item w)
      (cond (use-other
	     (nmode-2-windows) % display the other window
	     (nmode-select-window w)
	     (set-message "C-X O returns to browser.")
	     )
	    (t
	     (set-message "C-M-L returns to browser.")
	     ))
      (Ding)
      )))

(de browser-kill-deleted-items-command ()
  (let ((browser (=> nmode-current-buffer get 'browser)))
    (=> browser kill-deleted-items)
    ))

(de browser-undo-filter-command ()
  (let* ((browser (=> nmode-current-buffer get 'browser))
	 (filter (=> browser undo-filter))
	 )
    (if filter
      (set-prompt (bldmsg "Application of %w undone." filter))
      (nmode-error "No filters have been applied to create this list.")
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Browser functions: use these in browser commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de browser-sort (prompt sorter)
  (let ((browser (=> nmode-current-buffer get 'browser)))
    (=> browser sort sorter)
    (write-prompt prompt)
    ))

(de browser-view-item (w)
  % View the current item in the specified window.  Return T if successful,
  % NIL otherwise.

  (let* ((browser (=> nmode-current-buffer get 'browser))
	 (buffer (=> browser view-item))
	 )
    (when buffer
      (=> buffer set-previous-buffer nmode-current-buffer)
      (window-select-buffer w buffer)
      T
      )))

(de browser-do-repeated-command (msg args removes?)
  % Perform a browser command that takes a signed numeric argument to mean
  % a repetition count.  On each iteration, the browser is sent
  % the specified message with the specified arguments.  If REMOVES? is
  % true, then the browser operation may remove the current item and
  % it will return true if it does.

  (let ((browser (=> nmode-current-buffer get 'browser)))
    (if (> nmode-command-argument 0)
      (for (from i 1 nmode-command-argument)
	   (do (when (not (=> browser current-item))
		 (Ding) (exit))
	       (if (not (and (lexpr-send browser msg args) removes?))
		 (move-to-next-line)
		 )))
      (for (from i 1 (- nmode-command-argument))
	   (do (when (current-line-is-first?)
		 (Ding) (exit))
	       (move-to-previous-line)
	       (when (not (=> browser current-item))
		 (move-to-next-line) (Ding) (exit))
	       (lexpr-send browser msg args)
	       ))
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(off fast-integers)
