%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Browser-Support.SL - General Browser Support
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        18 October 1982
% Revised:     14 March 1983
%
% 14-Mar-83 Alan Snyder
%  Added functions to find existing browsers.  New functions:
%  browser-current-item, browser-view-buffer, browser-edit-buffer,
%  browser-help-command, browser-exit, current-browser, kill-browser,
%  kill-browser-command, browser-update.  Change browser-enter to take browser
%  as arg instead of buffer.  Fix browser-enter and browser-exit to
%  restore old buffers upon exit.
% 4-Mar-83 Alan Snyder
%  New functions: browser-add-item, browser-add-items.
% 3-Feb-83 Alan Snyder
%  Revised to use Browser objects.
%
% This file contains support functions for browsers, such as the Buffer
% Browser and DIRED.  A browser is a buffer that displays a set of items, one
% item per line, and allows the individual items to be manipulated.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(compiletime (load numeric-operators))
(on fast-integers)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% External variables:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(nmode-top-window
	 nmode-bottom-window
	 nmode-current-buffer
	 nmode-current-window
	 nmode-command-argument
	 nmode-command-argument-given
	 ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% User options:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(
  browser-split-screen
  ))
(setf browser-split-screen NIL)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal Static Variables:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(nmode-active-browsers))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% General Browser Support Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-register-browser (browser)
  % Add the specified browser to the list of active browsers.  This list is
  % used to create the items for the browser browser, for example.  This
  % function is called by create-browser.

  (if (unboundp 'nmode-active-browsers)
    (setf nmode-active-browsers ()))
  (when (not (memq browser nmode-active-browsers))
    (setf nmode-active-browsers (cons browser nmode-active-browsers)))
  )

(de nmode-unregister-browser (browser)
  % Remove the specified browser from the list of active browsers.

  (if (unboundp 'nmode-active-browsers)
    (setf nmode-active-browsers ()))
  (when (memq browser nmode-active-browsers)
    (setf nmode-active-browsers (delq browser nmode-active-browsers)))
  )

(de browser-is-active? (browser)
  (memq browser nmode-active-browsers)
  )

(de browser-enter (browser)
  % Start up a browser.
  (let ((wp (nmode-window-position)))
    (=> browser put 'window-status wp)
    (=> browser put 'old-top (=> (=> nmode-top-window buffer) name))
    (=> browser put 'old-bottom
	(when browser-split-screen (=> (=> nmode-bottom-window buffer) name)))
    (if browser-split-screen
      (if (eq wp 'bottom) (nmode-switch-windows))
      (nmode-1-window)
      ))
  (=> browser enter)
  )

(de browser-exit (browser)
  % Exit the browser, which means to detach its buffers from windows and
  % restore the window to its previous state.

  (let* ((ws (=> browser get 'window-status))
	 (old-top (=> browser get 'old-top))
	 (old-bottom (=> browser get 'old-bottom))
	 )
    (nmode-set-window-position ws)
    (when old-top
      (window-select-buffer nmode-top-window (buffer-find old-top)))
    (when old-bottom
      (window-select-buffer nmode-bottom-window (buffer-find old-bottom)))
    (=> browser exit)
    ))

(de kill-browser (browser)
  % Kill the browser, which means exit it and then remove it from the list
  % of active browsers (which should allow it to be garbage collected).

  (=> browser exit)
  (nmode-unregister-browser browser)
  )

(de all-browsers ()
  % Return a list of all active browsers.  The list should not be modified.

  nmode-active-browsers
  )

(de all-browsers-of-a-kind (browser-kind-id)
  % Return a list of all existing browsers of the specified kind.

  (for (in br (all-browsers))
       (when (eq (=> br browser-kind) browser-kind-id))
       (collect br)
       ))

(de find-browser (browser-kind-id info-string)
  % Search for a browser of the specified kind with the specified info string.

  (for (in br (all-browsers-of-a-kind browser-kind-id))
       (when (equal (=> br browser-info-string) info-string))
       (do (exit br))
       ))

(de browser-update (browser)
  (=> browser update-items)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Browser commands: attach these to keys in your browser mode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de browser-kill-and-exit-command ()
  (browser-kill-deleted-items-command)
  (browser-exit-command)
  )

(de browser-exit-command ()
  % Exit the current browser.  This removes the browser from the display,
  % but does not destroy it (it can be reentered).

  (let ((browser (current-browser)))
    (when browser
      (browser-exit browser)
      )))

(de kill-browser-command ()
  % Kill the current browser.  This removes the browser from the display,
  % and removes it from the active browser list (it cannot be reentered).

  (let ((browser (current-browser)))
    (when browser
      (kill-browser browser)
      )))

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
  (let ((buffer (browser-view-item-in-buffer)))
    (if buffer
      (browser-view-buffer buffer nmode-command-argument-given)
      (Ding)
      )))
  
(de browser-edit-command ()
  % Edit the current item.
  (let ((buffer (browser-view-item-in-buffer)))
    (if buffer
      (browser-edit-buffer buffer nmode-command-argument-given)
      (Ding)
      )))

(de browser-kill-deleted-items-command ()
  (let ((browser (current-browser)))
    (=> browser kill-deleted-items)
    ))

(de browser-undo-filter-command ()
  (let* ((browser (current-browser))
	 (filter (=> browser undo-filter))
	 )
    (if filter
      (set-prompt (bldmsg "Application of %w undone." filter))
      (nmode-error "No filters have been applied to create this list.")
      )))

(de browser-help-command ()
  (let ((browser (current-browser)))
    (when browser
      (=> browser display-documentation)
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Browser functions: use these in browser commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de current-browser ()
  (=> nmode-current-buffer get 'browser))

(de browser-sort (prompt sorter)
  (let ((browser (current-browser)))
    (=> browser sort sorter)
    (write-prompt prompt)
    ))

(de browser-current-item ()
  % Return the current item, if any, NIL otherwise.

  (let ((browser (current-browser)))
    (when browser
      (=> browser current-item)
      )))

(de browser-view-item (w)
  % View the current item in the specified window.  Return T if successful,
  % NIL otherwise.

  (let ((buffer (browser-view-item-in-buffer)))
    (when buffer
      (=> buffer set-previous-buffer nmode-current-buffer)
      (window-select-buffer w buffer)
      T
      )))

(de browser-view-item-in-buffer ()
  % View the current item in a buffer.  Return the buffer if successful,
  % NIL otherwise.  The buffer is not attached to any window.

  (let ((browser (current-browser)))
    (when browser
      (=> browser view-item)
      )))

(de browser-view-buffer (b invert-split-screen-option)
  % View the buffer B like an item is viewed.
  (let* ((use-other (xor browser-split-screen invert-split-screen-option))
	 (w (if use-other (nmode-other-window) nmode-current-window))
	 )
    (=> b set-previous-buffer nmode-current-buffer)
    (window-select-buffer w b)
    (if use-other
      (nmode-2-windows) % display the other window
      (set-message "C-M-L returns to browser.")
      )))
  
(de browser-edit-buffer (b invert-split-screen-option)
  % Edit the buffer B like an item is edited.
  (let* ((use-other (xor browser-split-screen invert-split-screen-option))
	 (w (if use-other (nmode-other-window) nmode-current-window))
	 )
    (=> b set-previous-buffer nmode-current-buffer)
    (window-select-buffer w b)
    (cond (use-other
	   (nmode-2-windows) % display the other window
	   (nmode-select-window w)
	   (set-message "C-X O returns to browser.")
	   )
	  (t
	   (set-message "C-M-L returns to browser.")
	   ))))

(de browser-add-item-and-view (new-item)
  % Add the item to the current browser.  Then, if in split screen mode,
  % view the item.

  (browser-add-item new-item)
  (when browser-split-screen
    (setf nmode-command-argument-given NIL)
    (browser-view-command)
    ))

(de browser-add-item (new-item)
  % Add the item to the current browser.

  (let ((browser (current-browser)))
    (when browser
      (=> browser add-item new-item)
      T
      )))

(de browser-add-items (new-item-list)
  % Add the items to the current browser.

  (let ((browser (current-browser)))
    (when browser
      (=> browser add-items new-item-list)
      T
      )))

(de browser-do-repeated-command (msg args removes?)
  % Perform a browser command that takes a signed numeric argument to mean
  % a repetition count.  On each iteration, the browser is sent
  % the specified message with the specified arguments.  If REMOVES? is
  % true, then the browser operation may remove the current item and
  % it will return true if it does.

  (let ((browser (current-browser)))
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
