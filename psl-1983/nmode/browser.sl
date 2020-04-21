%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Browser.SL - Browser object definition
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        4 February 1983
% Revised:     14 February 1983
%
% This file implements browser objects.  These objects form the basis of
% a general browser support mechanism.  See Browser-Support.SL for additional
% support functions and Buffer-Browser.SL for an example of a browser
% using this mechanism.
%
% 14-Feb-83 Alan Snyder
%  Fix bug in filter application (was trying to apply a macro).
% 11-Feb-83 Alan Snyder
%  Fix &remove-current-item to reset the display buffer's modified flag.
%  Improve comments.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(compiletime (load fast-vectors fast-int))
(load gsort)

(de create-browser (display-buffer view-buffer header-text items current-sorter)

  % Create a brower.  DISPLAY-BUFFER is the buffer to use for displaying the
  % items.  VIEW-BUFFER is the buffer to use for viewing an item; if NIL, the
  % item is expected to provide its own buffer.  HEADER-TEXT is a vector of
  % strings to display at the top of the display buffer; it may be NIL.  ITEMS
  % is a list or vector containing the set of items to display (this data
  % structure will not be modified).  CURRENT-SORTER may be NIL or a function
  % ID.  If non-NIL, the function will be used to sort the initial set of
  % items.

  (make-instance 'browser
		 'display-buffer display-buffer
		 'view-buffer view-buffer
		 'header-text header-text
		 'items items
		 'current-sorter current-sorter
		 ))

(defflavor browser
  (
   (display-buffer NIL)		% buffer used to display items
   (view-buffer NIL)		% buffer used to view items (NIL => ask item)
   (viewed-item NIL)		% the item most recently viewed
   (header-text	NIL)		% text displayed at top of buffer
   items			% vector of visible items (may have junk at end)
   first-item-linepos		% line number of first item in display
   last-item-index		% index of last item in ITEMS vector
   (filtered-items ())		% list of lists of items removed by filtering
   (current-sorter NIL)		% sorter used if items are un-filtered
   )
  ()
  (initable-instance-variables display-buffer view-buffer header-text items
			       current-sorter)
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
% (=> item apply-filter filter)
%   The item should apply the filter to itself and return T if the filter
%   matches the item and NIL otherwise.

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
  % View the current item, if any, in a separate buffer.
  % Return the buffer if the item exists, NIL otherwise.

  (let ((item (=> self current-item)))
    (when item
      (when viewed-item
	(=> viewed-item cleanup))
      (setf viewed-item item)
      (=> item view-buffer view-buffer) % return the buffer
      )))

(defmethod (browser ignore-item) ()
  % Ignore the current item, if any.  Return T if the item exists.
  % Ignoring an item is like running a filter that accepts every item
  % except the current one, except that multiple successive ignores
  % coalesce into one filtered-item-set for undoing purposes.

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

(defmethod (browser filter-items) (filter)
  % Remove those items that do not match the specified filter.
  % If some items are removed, then they are added as a set to the
  % list of filtered items, so that this step can be undone, and T
  % is returned.  Otherwise, no new set is created, and NIL is returned.

  (let ((filtered-list (=> self &keep-items 'ev-send
			   (list 'apply-filter (list filter)))))
    (when filtered-list
      (setf filtered-list (cons filter filtered-list))
      (setf filtered-items (cons filtered-list filtered-items))
      T
      )))

(defmethod (browser undo-filter) ()
  % Undo the effect of the most recent active filtering step.
  % Return the filter or NIL if there are no active filtering steps.

  (when filtered-items
    (let ((filter (car (car filtered-items)))
	  (the-items (cdr (car filtered-items)))
	  (current-item (=> self current-item))
	  )
      (setf filtered-items (cdr filtered-items))
      (while the-items
	(let ((item (car the-items)))
	  (setf the-items (cdr the-items))
	  (setf last-item-index (+ last-item-index 1))
	  (vector-store items last-item-index item)
	  ))
      (=> self &sort-items)
      (=> self &update-display)
      (=> self select-item current-item)
      filter
      )))

(defmethod (browser exit) ()
  (setf viewed-item NIL)
  (for (from i 0 last-item-index)
       (do (=> (vector-fetch items i) cleanup)))
  )

(defmethod (browser items) ()
  % Return a list of the items.
  (for (from i 0 last-item-index)
       (collect (vector-fetch items i)))
  )

(defmethod (browser sort) (sorter)
  (let ((current-item (=> self current-item)))
    (setf current-sorter sorter)
    (=> self &sort-items)
    (=> self &update-display)
    (=> self select-item current-item)
    ))

(defmethod (browser send-item) (msg args)
  % Send the current item, if any, the specified message with the specified
  % arguments.  Return NIL if there is no current item; otherwise, return
  % the result of sending the message to the item.

  (let ((item (=> self current-item)))
    (when item
      (prog1
       (lexpr-send item msg args)
       (=> self &update-current-item)
       ))))

(defmethod (browser select-item) (item)
  % If ITEM is not NIL, then adjust the buffer pointer to point to
  % that item.

  (for (from i 0 last-item-index)
       (do (when (eq item (vector-fetch items i))
	     (=> display-buffer goto (+ i first-item-linepos) 0)
	     (exit)
	     ))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private methods:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (browser init) (init-plist)
  (=> display-buffer put 'browser self)
  (setf items (cond ((ListP items) (List2Vector items))
		    ((VectorP items) (CopyVector items))
		    (t (List2Vector ()))
		    ))
  (setf last-item-index (vector-upper-bound items))
  (=> self &sort-items)
  (=> self &update-display)
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

(defmethod (browser &remove-current-item) ()
  % Remove the current item from ITEMS and the display.
  % Return the item or NIL if there is no current item.

  (let ((index (=> self current-item-index)))
    (when index
      (let ((item (vector-fetch items index)))
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

