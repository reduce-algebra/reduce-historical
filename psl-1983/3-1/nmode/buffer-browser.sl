%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Buffer-Browser.SL - Buffer Browser Subsystem
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        18 October 1982
% Revised:     8 April 1983
%
% This file implements a buffer browser subsystem.
%
% 8-April-83 Jeff Soreff
%  Filter commands, predicate, and associated funtions implemented.
%  Declare is used to speed up code somewhat.
% 14-Mar-83 Alan Snyder
%  Convert for revised browser mechanism (with documentation, etc.)
% 4-Mar-83 Alan Snyder
%  Added Create command.
% 16-Feb-83 Alan Snyder
%  Declare -> Declare-Flavor.
% 4-Feb-83 Alan Snyder
%  Rewritten using new browser support.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load extended-char fast-vectors fast-strings stringx
		 numeric-operators))
(on fast-integers)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% External variables:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(
  nmode-selectable-buffers
  ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal static variables:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(buffer-browser-mode
	 buffer-browser-command-list
	 buffer-browser-documentation-text
	 buffer-browser-help-text
	 buffer-browser-argument-list
	 ))

(setf buffer-browser-help-text
  ["? View Edit Filter Write Create Un/Delete Kill uN/Ignore Sort/Reverse Quit"])

(setf buffer-browser-documentation-text
  ["The Buffer Browser displays the existing editor buffers."
   "Terminology: the current buffer is the buffer pointed at by the cursor."
   "The View (V) and Edit (E) commands both display the current buffer."
   "In split-screen mode, Edit selects the bottom window while View does not."
   "The Write (W) command saves the current buffer in its file, if needed."
   "The Create (C) command creates a new buffer, but does not select it."
   "The Delete (D) command marks the current buffer for deletion upon Quit."
   "The Undelete (U) command removes the mark made by the Delete command."
   "The Kill (K) command kills the current buffer immediately."
   "The Ignore (I) command removes the current buffer from the display."
   "The Filter (F) command ignores buffer sets, using names, modes and files."
   "The uNignore (N) command restores all Ignored buffers to the display."
   "The Sort (S) command sorts the buffers in various ways."
   "The Reverse (R) command sorts the buffers in reverse order."
   "The Quit (Q) command exits the browser and deletes any marked buffers."
   ])

(setf buffer-browser-mode (nmode-define-mode "Buffer-Browser" '(
  (nmode-define-commands Buffer-Browser-Command-List)
  (nmode-establish-mode Read-Only-Text-Mode)
  )))

(setf buffer-browser-command-list
  (list
   (cons (x-char ?) 'browser-help-command)
   (cons (x-char C) 'buffer-browser-create-command)
   (cons (x-char D) 'browser-delete-command)
   (cons (x-char E) 'browser-edit-command)
   (cons (x-char W) 'buffer-browser-save-file-command)
   (cons (x-char I) 'browser-ignore-command)
   (cons (x-char K) 'browser-kill-command)
   (cons (x-char F) 'buffer-browser-filter-command)
   (cons (x-char N) 'browser-undo-filter-command)
   (cons (x-char Q) 'browser-kill-and-exit-command)
   (cons (x-char R) 'buffer-browser-reverse-sort)
   (cons (x-char S) 'buffer-browser-sort)
   (cons (x-char U) 'browser-undelete-command)
   (cons (x-char V) 'browser-view-command)
   (cons (x-char X) 'browser-exit-command)
   (cons (x-char BACKSPACE) 'browser-undelete-backwards-command)
   (cons (x-char RUBOUT) 'browser-undelete-backwards-command)
   (cons (x-char SPACE) 'move-down-command)
   (cons (x-char M-~) 'buffer-browser-not-modified-command)
   ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de buffer-browser-command ()
  % Bring up the buffer browser subsystem.
  (let ((browser (or (find-browser 'BUFFER-BROWSER "")
		     (create-buffer-browser)
		     )))
    (browser-enter browser)
    ))

(de create-buffer-browser ()
  % Create the buffer browser subsystem.
  % The set of items is created when the browser is selected.

  (let* ((header-text
	  (vector
	   (string-concat "   "
			  (string-pad-right "Buffer Name" 24)
			  (string-pad-left "Size" 6)
			  "  "
			  "File Name"
			  )
	   ""
	   ))
	 (browser
	   (create-browser 'BUFFER-BROWSER "Buffers" "" buffer-browser-mode
			   NIL header-text buffer-browser-documentation-text
			   buffer-browser-help-text
			   () #'buffer-browser-name-sorter)
	   ))
      (=> browser set-select-function 'browser-update)
      (=> browser set-update-function 'buffer-browser-update)
      (=> browser put 'buffer-list ())
      browser
      ))

(de buffer-browser-update (browser)
  % Add any new buffers to the buffer browser.

  (let* ((width (=> browser display-width))
	 (old-buffer-list (=> browser get 'buffer-list))
	 (old-current-item (=> browser current-item))
	 (new-items
	  (for (in b nmode-selectable-buffers)
	       (when (not (memq b old-buffer-list)))
	       (collect (create-buffer-browser-item b width))
	       ))
	 )
    (=> browser add-items new-items)
    (=> browser put 'buffer-list nmode-selectable-buffers)
    (=> browser select-item old-current-item)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Special Buffer Browser commands:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de buffer-browser-create-command ()
  (let* ((browser (current-browser))
	 (new-buffer-name (prompt-for-string "Create buffer whose name is:" NIL))
	 (b (buffer-create-default new-buffer-name))
	 (item (create-buffer-browser-item b (=> browser display-width)))
	 )
    (write-prompt (bldmsg "Buffer %w created." (=> b name)))
    (=> browser put 'buffer-list
	(cons b (=> browser get 'buffer-list)))
    (browser-add-item-and-view item)
    ))

(de buffer-browser-save-file-command ()
  (browser-do-repeated-command 'send-item '(save-file ()) NIL)
  )

(de buffer-browser-not-modified-command ()
  (browser-do-repeated-command 'send-item '(set-unmodified ()) NIL)
  )

(de buffer-browser-reverse-sort ()
  (nmode-set-immediate-prompt "Reverse Sort by ")
  (buffer-browser-reverse-sort-dispatch)
  )

(de buffer-browser-reverse-sort-dispatch ()
  (selectq (char-upcase (input-base-character))
   (#/N (browser-sort "Reverse Sort by Name" 'buffer-browser-name-reverser))
   (#/S (browser-sort "Reverse Sort by Size" 'buffer-browser-size-reverser))
   (#/F (browser-sort "Reverse Sort by File" 'buffer-browser-file-reverser))
   (#/M
    (browser-sort "Reverse Sort by Modified" 'buffer-browser-modified-reverser))
   (#/?
     (nmode-set-immediate-prompt
      "Reverse Sort by (Name, Size, File, Modified) ")
     (buffer-browser-reverse-sort-dispatch)
     )
   (t (write-prompt "") (Ding))
   ))

(de buffer-browser-sort ()
  (nmode-set-immediate-prompt "Sort by ")
  (buffer-browser-sort-dispatch)
  )

(de buffer-browser-sort-dispatch ()
  (selectq (char-upcase (input-base-character))
   (#/N (browser-sort "Sort by Name" 'buffer-browser-name-sorter))
   (#/S (browser-sort "Sort by Size" 'buffer-browser-size-sorter))
   (#/F (browser-sort "Sort by File" 'buffer-browser-file-sorter))
   (#/M (browser-sort "Sort by Modified" 'buffer-browser-modified-sorter))
   (#/? (nmode-set-immediate-prompt "Sort by (Name, Size, File, Modified) ")
	(buffer-browser-sort-dispatch)
	)
   (t (write-prompt "") (Ding))
   ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Filtering Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de buffer-browser-filter-command ()
  (nmode-set-immediate-prompt "Filter by File-name, Mode, or Name?")
  (buffer-browser-filter-dispatch1))

(de buffer-browser-filter-dispatch1 ()
  (selectq (char-upcase (input-base-character))
    (#/F (buffer-browser-filter-prompter2
	  (list "file-name" #'buffer-browser-file-name-extractor)))
    (#/M (buffer-browser-filter-prompter2
	  (list "mode" #'buffer-browser-mode-extractor)))
    (#/N (buffer-browser-filter-prompter2
	  (list "name" #'buffer-browser-name-extractor)))
    (#/? (nmode-set-immediate-prompt
	  "Type F for File-name, M for Mode, N for Name")
	 (buffer-browser-filter-dispatch1))
    (t (write-prompt "") (Ding))))

(declare-flavor text-buffer item-buffer)

(de buffer-browser-file-name-extractor (item-buffer)
  (=> item-buffer file-name))

(declare-flavor mode mode-temp)

(de buffer-browser-mode-extractor (item-buffer)
  (let ((mode-temp (=> item-buffer mode)))
    (=> mode-temp name)))

(undeclare-flavor mode-temp)

(de buffer-browser-name-extractor (item-buffer)
  (=> item-buffer name))

(undeclare-flavor item-buffer)

(de buffer-browser-filter-prompter2 (aspect)
  (nmode-set-immediate-prompt "Flush or Keep matching buffers?")
  (buffer-browser-filter-dispatch2 aspect))

(de buffer-browser-filter-dispatch2 (aspect)
  (selectq (char-upcase (input-base-character))
    (#/F (buffer-browser-filter-compose t aspect))
    (#/K (buffer-browser-filter-compose nil aspect))
    (#/?
     (nmode-set-immediate-prompt
      (bldmsg
       "Type F to flush or K to keep buffers with matching %ws."
       (first aspect)))
     (buffer-browser-filter-dispatch2 aspect))
    (t (write-prompt "") (Ding))))

(de buffer-browser-filter-compose (flag aspect)
  (let ((browser (current-browser))
	(buffer-browser-argument-list
	 (list
	  (string-upcase % Make the search pattern upper case.
	   (prompt-for-string
	    (bldmsg "%w buffers with %w matching string"	   
		    (if flag "flush" "keep")
		    (first aspect))
	    ""))
	  flag % Keep or flush flag
	  (second aspect)))) % extractor function
    (=> browser filter-items #'buffer-browser-filter-predicate)))

(declare-flavor buffer-browser-item buffer-browser-item)

(de buffer-browser-filter-predicate (buffer-browser-item)
  (let* ((aspect (or (apply (third buffer-browser-argument-list)
			    (list (=> buffer-browser-item buffer))) ""))
	 (match (forward-search-in-string 
		 aspect (first buffer-browser-argument-list))))
    (when (second buffer-browser-argument-list)
      (setf match (not match)))
    match))

(undeclare-flavor buffer-browser-item)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sorting Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(declare-flavor buffer-browser-item b1 b2)

(de buffer-browser-name-sorter (b1 b2)
  (let ((name1 (=> (=> b1 buffer) name))
	(name2 (=> (=> b2 buffer) name))
	)
    (StringSortFn name1 name2)
    ))

(de buffer-browser-name-reverser (b1 b2)
  (not (buffer-browser-name-sorter)))

(de buffer-browser-size-sorter (b1 b2)
  (let ((s1 (=> (=> b1 buffer) visible-size))
	(s2 (=> (=> b2 buffer) visible-size))
	)
    (or (< s1 s2)
	(and (= s1 s2) (buffer-browser-name-sorter b1 b2))
	)))

(de buffer-browser-size-reverser (b1 b2)
  (let ((s1 (=> (=> b1 buffer) visible-size))
	(s2 (=> (=> b2 buffer) visible-size))
	)
    (or (> s1 s2)
	(and (= s1 s2) (buffer-browser-name-sorter b1 b2))
	)))

(de buffer-browser-file-sorter (b1 b2)
  (let ((f1 (or (=> (=> b1 buffer) file-name) ""))
	(f2 (or (=> (=> b2 buffer) file-name) ""))
	)
    (StringSortFn f1 f2)
    ))

(de buffer-browser-file-reverser (b1 b2)
  (not (buffer-browser-file-sorter b1 b2)))

(de buffer-browser-modified-sorter (b1 b2)
  (let ((m1 (=> (=> b1 buffer) modified?))
	(m2 (=> (=> b2 buffer) modified?))
	)
    (cond ((not (eq m1 m2))
	   (=> (=> b1 buffer) modified?)) % saying 'M1' results in compiler bug
	  (t (buffer-browser-name-sorter b1 b2))
	  )))

(de buffer-browser-modified-reverser (b1 b2)
  (let ((m1 (=> (=> b1 buffer) modified?))
	(m2 (=> (=> b2 buffer) modified?))
	)
    (cond ((not (eq m1 m2))
	   (=> (=> b2 buffer) modified?)) % saying 'M2' results in compiler bug
	  (t (buffer-browser-name-sorter b1 b2))
	  )))

(undeclare-flavor b1 b2)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The buffer-browser-item flavor:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de create-buffer-browser-item (b width)
  (make-instance 'buffer-browser-item
		 'buffer b
		 'display-width width
		 ))

(defflavor buffer-browser-item
  (display-text
   display-width
   buffer
   (delete-flag NIL)
   )
  ()
  (gettable-instance-variables display-text buffer)
  (initable-instance-variables)
  )

(defmethod (buffer-browser-item init) (init-plist)
  (=> self &update-display-text)
  )

(defmethod (buffer-browser-item &update-display-text) ()
  (setf display-text
    (string-concat (if delete-flag "D" " ")
		   (if (=> buffer modified?) "*" " ")
		   " "
		   (string-pad-right (=> buffer name) 24)
		   (string-pad-left (bldmsg "%d" (=> buffer visible-size)) 6)
		   "  "
		   (or (=> buffer file-name) "")
		   )
    ))

(defmethod (buffer-browser-item update) ()
  (when (memq buffer nmode-selectable-buffers)
    (=> self &update-display-text)
    ))

(defmethod (buffer-browser-item delete) ()
  (when (not delete-flag)
    (cond ((not (buffer-killable? buffer))
	   (nmode-error
	    (BldMsg "Buffer %w may not be deleted!" (=> buffer name)))
	   )
	  (t
	   (setf delete-flag T)
	   (=> self &update-display-text)
	   ))))

(defmethod (buffer-browser-item undelete) ()
  (when delete-flag
    (setf delete-flag NIL)
    (=> self &update-display-text)
    ))

(defmethod (buffer-browser-item deleted?) ()
  delete-flag
  )

(defmethod (buffer-browser-item kill) ()
  (cond ((not (buffer-killable? buffer))
	 (nmode-error (BldMsg "Buffer %w may not be killed!" (=> buffer name)))
	 NIL
	 )
	((or (not (=> buffer modified?))
	     (YesP (BldMsg "Kill unsaved buffer %w?" (=> buffer name))))
	 (=> buffer set-previous-buffer NIL)
	 (buffer-kill-and-detach buffer)
	 T
	 )))

(defmethod (buffer-browser-item view-buffer) (x)
  (if (buffer-is-selectable? buffer) buffer)
  )

(defmethod (buffer-browser-item cleanup) ()
  )

(defmethod (buffer-browser-item apply-filter) (filter)
  (apply filter (list self))
  )

(defmethod (buffer-browser-item save-file) ()
  (when (=> buffer modified?)
    (save-file buffer)
    (=> self &update-display-text)
    ))  

(defmethod (buffer-browser-item set-unmodified) ()
  (when (=> buffer modified?)
    (=> buffer set-modified? NIL)
    (=> self &update-display-text)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(off fast-integers)
