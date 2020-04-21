%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Browser.SL - Browser object definition
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        4 February 1983
% Revised:     14 March 1983
%
% This file implements browser objects.  These objects form the basis of a
% general browser support mechanism.  See Browser-Support.SL for additional
% support functions and Buffer-Browser.SL for an example of a browser using
% this mechanism.
%
% 14-Mar-83 Alan Snyder
%  New methods: enter, select, display-documentation, set-items, update-items,
%  filter-count, get, put.  New documentation fields, etc.  Create-Browser
%  changed incompatibly.
% 4-Mar-83 Alan Snyder
%  New methods: add-item and add-items.
% 14-Feb-83 Alan Snyder
%  Fix bug in filter application (was trying to apply a macro).
% 11-Feb-83 Alan Snyder
%  Fix &remove-current-item to reset the display buffer's modified flag.
%  Improve comments.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(compiletime (load fast-vectors numeric-operators))
(on fast-integers)
(load gsort)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% External variables:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(nmode-top-window
	 nmode-bottom-window
	 nmode-current-window
	 nmode-current-buffer
	 browser-split-screen
	 read-only-text-mode
	 ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de create-browser (browser-kind
		    browser-kind-string
		    browser-info-string
		    display-buffer-mode
		    view-buffer
		    header-text
		    documentation-text
		    help-text
		    items
		    current-sorter
		    )

  % Create a brower.  BROWSER-KIND should be an ID that identifies the kind of
  % browser this is.  This ID is provided for optional use by the creator of
  % the browser to locate existing browsers of its kind.  BROWSER-KIND-STRING
  % should be a string that identifies the kind of browser this is.  This
  % STRING is used in the browser browser display.  BROWSER-INFO-STRING should
  % be a string that identifies this particular browser, as differentiated
  % from others of the same kind.  This STRING is used in the browser browser
  % display.

  % DISPLAY-BUFFER-MODE is the mode to use for the browser display buffer.
  % VIEW-BUFFER is the buffer to use for viewing an item; if NIL, the item is
  % expected to provide its own buffer.  HEADER-TEXT is a vector of strings to
  % display at the top of the display buffer; it may be NIL.
  % DOCUMENTATION-TEXT is a vector of strings to display in the documentation
  % buffer, which is displayed in the bottom window when there is no
  % currently-viewed item; it may be NIL.  HELP-TEXT is a vector of strings to
  % display at the bottom of the screen; it may be NIL.  The HELP-TEXT should
  % briefly list the available commands.  (Currently the HELP-TEXT should
  % consist of at most one string, which will be displayed in the message
  % window.)  ITEMS is a list or vector containing the set of items to display
  % (this data structure will not be modified).  CURRENT-SORTER may be NIL or
  % a function ID.  If non-NIL, the function will be used to sort the initial
  % set of items.

  (let ((browser (make-instance 'browser
				'browser-kind browser-kind
				'browser-kind-string browser-kind-string
				'browser-info-string browser-info-string
				'display-buffer-mode display-buffer-mode
				'view-buffer view-buffer
				'header-text header-text
				'documentation-text documentation-text
				'help-text help-text
				'items items
				'current-sorter current-sorter
				'display-width (=> nmode-top-window width)
				)))
    (nmode-register-browser browser)
    browser
    ))

(defflavor browser
  ((browser-kind NIL)		% ID identifying kind of browser
   (browser-kind-string "")	% string identifying kind of browser
   (browser-info-string "")	% string describing this particular browser

   (select-function NIL)	% function to invoke when selected (arg: self)
   (update-function NIL)	% function to invoke when updated (arg: self)

   display-width
   (display-buffer-mode NIL)	% mode of browser display buffer
   display-buffer		% buffer used to display items
   (view-buffer NIL)		% buffer used to view items (NIL => ask item)
   documentation-buffer		% buffer used to display documentation

   (header-text NIL)		% text displayed at top of buffer
   first-item-linepos		% line number of first item in display
   (documentation-text NIL)	% text displayed in documentation buffer
   (help-text NIL)		% text displayed in help line

   items			% vector of visible items (may have junk at end)
   last-item-index		% index of last valid item in ITEMS vector
   (viewed-item NIL)		% the item most recently viewed (or NIL)
   filtered-items		% list of lists of items removed by filtering
   (current-sorter NIL)		% sorter used if items are un-filtered

   (p-list NIL)			% association list of properties
   )
  ()
  (gettable-instance-variables
   browser-kind browser-kind-string display-width
   display-buffer help-text documentation-buffer
   )
  (settable-instance-variables
   browser-info-string
   select-function
   update-function
   )
  (initable-instance-variables
   browser-kind browser-kind-string display-width
   display-buffer-mode view-buffer header-text
   documentation-text help-text
   items current-sorter)
  )

% Methods provided by items:
%
% (=> item display-text)
%   Return string used to display the item.
%
% (=> item delete)
%   Mark the item as deleted.  May do nothing if deletion is not supported.
%   May change the display-text.  This method need not be provided if no
%   delete commands are provided in the particular browser.
%
% (=> item undelete)
%   Mark the item as not deleted.  May do nothing if deletion is not
%   supported.  May change the display-text.  This method need not be provided
%   if no delete commands are provided in the particular browser.
%
% (=> item deleted?)
%   Return T if the item has been marked for deletion.  This method need not
%   be provided if no delete commands are provided in the particular browser.
%
% (=> item kill)
%   Kill the real item.  (Instead of just marking the item for deletion, this
%   should actually dispose of the item, if that action is supported.)  May do
%   nothing if killing is not supported.  Return T if the item is actually
%   killed, NIL otherwise.  This method need not be provided if no delete
%   commands are provided in the particular browser.
%
% (=> item view-buffer buffer)
%   Return a buffer containing the item for viewing.  If the buffer argument
%   is non-NIL, then that buffer should be used for viewing.  Otherwise, the
%   item must provide its own buffer.
%
% (=> item cleanup)
%   Throw away any unneeded stuff, such as a buffer created for viewing.  This
%   method is invoked when an item is no longer being viewed, or when the item
%   is being filtered out, or when the browser is being exited.
%
% (=> item update)
%   The item should check for any changes in the object that it represents and
%   update itself accordingly.  This method should return NIL if and only if
%   the object no longer exists, in which case it will be removed.  (The item
%   should clean itself up in this case.)  Updating is performed on active
%   items by the update-items method; in addtion, items that are unfiltered
%   are also updated at that time.
%
% (=> item apply-filter filter)
%   The item should apply the filter to itself and return T if the filter
%   matches the item and NIL otherwise.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public methods for browsers:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (browser select) ()
  % This method is invoked when the browser buffer is newly selected.
  (=> self &display-viewed-item)
  (=> self display-help)
  (when select-function (apply select-function (list self)))
  )
      
(defmethod (browser enter) ()
  % Entering a browser means selecting its display buffer in the current
  % window.

  (when (not (eq display-buffer nmode-current-buffer))
    (=> display-buffer set-previous-buffer nmode-current-buffer))
  (buffer-select display-buffer)
  )

(defmethod (browser exit) ()
  % Exiting a browser means to clean up its items and detach any of its
  % buffers from any windows.  It is still an active browser and may be
  % reentered later.

  (for (from i 0 last-item-index)
       (do (=> (vector-fetch items i) cleanup)))
  (if display-buffer
    (buffer-kill-and-detach display-buffer))
  (if documentation-buffer
    (buffer-kill-and-detach documentation-buffer))
  (if view-buffer
    (buffer-kill-and-detach view-buffer))
  )

(defmethod (browser display-help) ()
  (when (and help-text (not (vector-empty? help-text)))
    (write-message (vector-fetch help-text 0))
    ))

(defmethod (browser display-documentation) ()
  (=> documentation-buffer move-to-buffer-start)
  (=> self &set-viewed-item NIL)
  (cond (browser-split-screen
	 (=> nmode-bottom-window set-line-position 0)
	 (=> nmode-bottom-window adjust-window)
	 )
	(t
	 (browser-view-buffer documentation-buffer NIL)
	 )))

(defmethod (browser current-item) ()
  % Return the current item, which is the item that is displayed on the
  % display-buffer's current line, or NIL, if there is no such item.

  (let ((index (- (=> display-buffer line-pos) first-item-linepos)))
    (when (and (>= index 0) (<= index last-item-index))
      (vector-fetch items index)
      )))

(defmethod (browser current-item-index) ()
  % Return the index of the current item, which is the item that is displayed
  % on the display-buffer's current line, or NIL, if there is no such item.

  (let ((index (- (=> display-buffer line-pos) first-item-linepos)))
    (when (and (>= index 0) (<= index last-item-index))
      index
      )))

(defmethod (browser add-item) (new-item)
  % Add the specified item to the set of items.  If a sort function is
  % currently defined, it will be used to sort the set of items.  The new item
  % becomes the current item.

  (=> self add-items (list new-item))
  )

(defmethod (browser add-items) (new-item-list)
  % Add the specified items to the set of items.  If a sort function is
  % currently defined, it will be used to sort the set of items.  The first
  % new item becomes the current item.

  (when new-item-list
    (let ((new-current-item (first new-item-list)))
      (=> self &insert-items new-item-list)
      (=> self &sort-items)
      (=> self &update-display)
      (=> self select-item new-current-item)
      )))

(defmethod (browser kill-item) ()
  % Kill the current item, if any.  Return T if the item is killed,
  % NIL otherwise.

  (let ((item (=> self current-item)))
    (when (=> item kill)
      (=> self &remove-current-item)
      )))

(defmethod (browser kill-deleted-items) ()
  % Attempts to KILL all items that have been marked for deletion.
  % Returns a list of the items actually killed.
  (=> self &keep-items '&browser-item-not-killed ())
  )

(defmethod (browser delete-item) ()
  % Mark the current item as deleted, if any.  Return T if the item exists,
  % NIL otherwise.

  (let ((item (=> self current-item)))
    (when item
      (=> item delete)
      (=> self &update-current-item)
      T
      )))

(defmethod (browser undelete-item) ()
  % Mark the current item as not deleted, if any.  Return T if the item exists,
  % NIL otherwise.

  (let ((item (=> self current-item)))
    (when item
      (=> item undelete)
      (=> self &update-current-item)
      T
      )))

(defmethod (browser view-item) ()
  % View the current item, if any, in a separate buffer.  Return the buffer if
  % the item exists, NIL otherwise.

  (let ((item (=> self current-item)))
    (when item
      (=> self &set-viewed-item item)
      (=> item view-buffer view-buffer) % return the buffer
      )))

(defmethod (browser ignore-item) ()
  % Ignore the current item, if any.  Return T if the item exists.  Ignoring
  % an item is like running a filter that accepts every item except the
  % current one, except that multiple successive ignores coalesce into one
  % filtered-item-set for undoing purposes.

  (let ((item (=> self &remove-current-item)))
    (when item
      (cond ((and filtered-items (eqcar (car filtered-items) 'IGNORE-COMMAND))
	     % add this item to the previous list of ignored items
	     (let ((filter-set (car filtered-items)))
	       (setf (cdr filter-set) (cons item (cdr filter-set)))
	       ))
	    (t (setf filtered-items
		 (cons (list 'IGNORE-COMMAND item) filtered-items))
	       )))))

(defmethod (browser update-items) ()
  % Ask all active items to update themselves.  Items that report that they
  % are no longer meaningful will be removed.  Then, the update-function
  % is called.  This function may choose to add new items for objects that
  % have been created since the browser was created.

  (=> self &keep-items 'ev-send '(update))
  (when update-function
    (apply update-function (list self))
    ))

(defmethod (browser filter-items) (filter)
  % Remove those items that do not match the specified filter.  If some items
  % are removed, then they are added as a set to the list of filtered items,
  % so that this step can be undone, and T is returned.  Otherwise, no new set
  % is created, and NIL is returned.

  (let ((filtered-list (=> self &keep-items 'ev-send
			   (list 'apply-filter (list filter)))))
    (when filtered-list
      (setf filtered-list (cons filter filtered-list))
      (setf filtered-items (cons filtered-list filtered-items))
      T
      )))

(defmethod (browser undo-filter) ()
  % Undo the effect of the most recent active filtering step.  Return the
  % filter or NIL if there are no active filtering steps.  All unfiltered
  % items are asked to update themselves.  Items that report that they are no
  % longer meaningful will be removed.

  (when filtered-items
    (let ((filter (car (car filtered-items)))
	  (the-items (cdr (car filtered-items)))
	  (current-item (=> self current-item))
	  )
      (setf filtered-items (cdr filtered-items))
      (while the-items
	(let ((item (car the-items)))
	  (setf the-items (cdr the-items))
	  (when (=> item update)
	    (setf last-item-index (+ last-item-index 1))
	    (vector-store items last-item-index item)
	    )))
      (=> self &sort-items)
      (=> self &update-display)
      (=> self select-item current-item)
      filter
      )))

(defmethod (browser filter-count) ()
  % Return the number of active filters.
  (length filtered-items)
  )

(defmethod (browser items) ()
  % Return a list of the active (unfiltered) items.
  (for (from i 0 last-item-index)
       (collect (vector-fetch items i)))
  )

(defmethod (browser set-items) (new-items)
  % Replace the entire existing set of items (both active items and filtered
  % items) with a new set of items.  NEW-ITEMS may be a list or a vector.

  (for (from i 0 last-item-index)
       (do (=> (vector-fetch items i) cleanup)))
  (setf items (cond ((ListP new-items) (List2Vector new-items))
		    ((VectorP new-items) (CopyVector new-items))
		    (t (Vector))
		    ))
  (setf last-item-index (vector-upper-bound items))
  (setf filtered-items ())
  (=> self &set-viewed-item NIL)
  (=> self &sort-items)
  (=> self &update-display)
  )

(defmethod (browser sort) (sorter)
  % Specify a new sorting function and sort the items accordingly.
  (let ((current-item (=> self current-item)))
    (setf current-sorter sorter)
    (=> self &sort-items)
    (=> self &update-display)
    (=> self select-item current-item)
    ))

(defmethod (browser send-item) (msg args)
  % Send the current item, if any, the specified message with the specified
  % arguments.  Return NIL if there is no current item; otherwise, return the
  % result of sending the message to the item.

  (let ((item (=> self current-item)))
    (when item
      (prog1
       (lexpr-send item msg args)
       (=> self &update-current-item)
       ))))

(defmethod (browser select-item) (item)
  % If ITEM is not NIL, then adjust the buffer pointer to point to that item.

  (for (from i 0 last-item-index)
       (do (when (eq item (vector-fetch items i))
	     (=> display-buffer goto (+ i first-item-linepos) 0)
	     (exit)
	     ))))

(defmethod (browser get) (property-name)
  % Return the object associated with the specified property name (ID).
  % Returns NIL if named property has not been defined.

  (let ((pair (atsoc property-name p-list)))
    (if (PairP pair) (cdr pair))))

(defmethod (browser put) (property-name property)
  % Associate the specified object with the specified property name (ID).
  % GET on that property-name will henceforth return the object.

  (let ((pair (atsoc property-name p-list)))
    (if (PairP pair)
      (rplacd pair property)
      (setf p-list (cons (cons property-name property) p-list))
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private methods:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (browser init) (init-plist)
  (setf last-item-index -1)
  (when (not display-buffer-mode)
    (setf display-buffer-mode Read-Only-Text-Mode))
  (setf display-buffer (create-unnamed-buffer display-buffer-mode))
  (when (and browser-info-string (not (string-empty? browser-info-string)))
    (=> display-buffer set-label-string
	(string-concat "(" browser-info-string ")")
	))
  (setf documentation-buffer (create-unnamed-buffer Read-Only-Text-Mode))
  (when documentation-text
    (=> documentation-buffer insert-text documentation-text)
    (=> documentation-buffer insert-eol)
    (=> documentation-buffer set-modified? NIL)
    (=> documentation-buffer move-to-buffer-start)
    (=> documentation-buffer set-label-string
	(string-concat "(Documentation on " browser-kind-string " browser)"))
    )
  (let ((old-browser (=> display-buffer get 'browser)))
    (when old-browser
      (=> old-browser exit)
      ))
  (=> display-buffer put 'browser self)
  (=> self set-items items)
  )

(defmethod (browser &update-display) ()
  % Update the display.  The cursor is moved to the first item.
  (=> display-buffer reset)
  (when header-text
    (=> display-buffer insert-text header-text)
    (=> display-buffer insert-eol)
    )
  (setf first-item-linepos (=> display-buffer line-pos))
  (for (from i 0 last-item-index)
       (do (let ((item (vector-fetch items i)))
	     (=> display-buffer insert-line (=> item display-text))
	     )))
  (=> display-buffer set-modified? NIL)
  (=> display-buffer goto first-item-linepos 0)
  )

(defmethod (browser &set-viewed-item) (item)
  (when (not (eq item viewed-item))
    (if viewed-item (=> viewed-item cleanup))
    (setf viewed-item item)
    (when (not viewed-item) (=> self &display-viewed-item))
    ))

(defmethod (browser &display-viewed-item) ()
  % This method causes the viewed item to be displayed in the bottom window,
  % if the browser is selected in the top window and the split-screen option
  % is selected.  If there is no viewed item, then the documentation buffer is
  % displayed.

  (when (and (eq nmode-current-window nmode-top-window) browser-split-screen)
    (let ((b (if viewed-item
	       (=> viewed-item view-buffer view-buffer)
	       documentation-buffer
	       )))
      (when b
	(=> b set-previous-buffer nmode-current-buffer)
	(window-select-buffer (nmode-other-window) b)
	(nmode-2-windows)
	))))

(defmethod (browser &sort-items) ()
  % Sort the items according to the current sorter, if any.
  % Do not update the display buffer.

  (when current-sorter
    (let ((list ()))
      (for (from i 0 last-item-index)
	   (do (setf list (cons (vector-fetch items i) list)))
	   )
      (setf list (GSort list current-sorter))
      (for (from i 0 last-item-index)
	   (do (vector-store items i (car list))
	       (setf list (cdr list))
	       ))
      )))

(defmethod (browser &insert-items) (item-list)
  % Add the specified items to the end of the current set of items.  The
  % vector size is increased to ensure there is room for all items, including
  % any that may have been filtered out.

  (let ((new-items (mkvect (+ (vector-upper-bound items) (length item-list)))))
    (for (from i 0 last-item-index)
	 (do (vector-store new-items i (vector-fetch items i))))
    (for (in item item-list)
	 (do (setf last-item-index (+ last-item-index 1))
	     (vector-store new-items last-item-index item)
	     ))
    (setf items new-items)
    ))

(defmethod (browser &remove-current-item) ()
  % Remove the current item from ITEMS and the display.
  % Return the item or NIL if there is no current item.

  (let ((index (=> self current-item-index)))
    (when index
      (let ((item (vector-fetch items index)))
	(when (eq item viewed-item) (=> self &set-viewed-item NIL))
	(for (from i (+ index 1) last-item-index)
	     (do (vector-store items (- i 1) (vector-fetch items i))
		 ))
	(vector-store items last-item-index NIL)
	(setf last-item-index (- last-item-index 1))
	(=> display-buffer move-to-start-of-line)
	(let ((start-pos (=> display-buffer position)))
	  (=> display-buffer move-to-next-line)
	  (=> display-buffer extract-region T start-pos
	      (=> display-buffer position))
	  (=> display-buffer set-modified? NIL)
	  )
	item
	))))

(defmethod (browser &update-current-item) ()
  % Update the display for the current item.
  (let ((index (=> self current-item-index)))
    (when index
      (let ((item (vector-fetch items index)))
	(=> display-buffer store-line (+ index first-item-linepos)
	    (=> item display-text))
	(=> display-buffer set-modified? NIL)
	))))

(defmethod (browser &keep-items) (fcn args)
  % Apply the function FCN once for each item.  The first argument to FCN
  % is the item; the remaining items are ARGS (a list).
  % Remove those items for which FCN returns NIL and return them
  % in a list of items.

  (let ((removed-items ())
	(ptr 0)
	(current-item-index (=> self current-item-index))
	(new-current-item-index 0)
	)
    (for (from i 0 last-item-index)
	 (do (let ((item (vector-fetch items i))
		   (this-ptr ptr)
		   )
	       (cond ((apply fcn (cons item args)) % keep it
		      (vector-store items ptr item)
		      (setf ptr (+ ptr 1))
		      )
		     (t % remove it
		      (setf removed-items (cons item removed-items))
		      (=> item cleanup)
		      (when (eq item viewed-item) (=> self &set-viewed-item NIL))
		      ))
	       (when (and current-item-index (= i current-item-index))
		 (setf new-current-item-index this-ptr))
	       )))
    (setf last-item-index (- ptr 1))
    (=> self &update-display)
    (=> display-buffer goto (+ new-current-item-index first-item-linepos) 0)
    removed-items
    ))

(de &browser-item-not-killed (item)
  (or (not (=> item deleted?))
      (not (=> item kill))
      ))
