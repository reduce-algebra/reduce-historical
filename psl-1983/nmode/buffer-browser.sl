%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Buffer-Browser.SL - Buffer Browser Subsystem
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        18 October 1982
% Revised:     16 February 1983
%
% This file implements a buffer browser subsystem.
%
% 16-Feb-83 Alan Snyder
%  Declare -> Declare-Flavor.
% 4-Feb-83 Alan Snyder
%  Rewritten using new browser support.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load extended-char fast-vectors fast-strings stringx))

% External variables:

(fluid '(
  nmode-current-buffer
  nmode-current-window
  nmode-command-argument-given
  nmode-selectable-buffers
  ))

% Internal static variables:

(fluid '(Buffer-Browser-Mode Buffer-Browser-Command-List))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(setf Buffer-Browser-Mode (nmode-define-mode "Buffer-Browser" '(
  (nmode-define-commands Buffer-Browser-Command-List)
  (nmode-establish-mode Read-Only-Text-Mode)
  )))

(setf Buffer-Browser-Command-List
  (list
   (cons (x-char ?) 'buffer-browser-help)
   (cons (x-char D) 'browser-delete-command)
   (cons (x-char E) 'browser-edit-command)
   (cons (x-char F) 'buffer-browser-save-file-command)
   (cons (x-char I) 'browser-ignore-command)
   (cons (x-char K) 'browser-kill-command)
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

(de buffer-browser-command ()
  (buffer-browser nmode-command-argument-given)
  )

(de buffer-browser (all-buffers?)

  % Put up a buffer browser subsystem. If ALL-BUFFERS? is non-NIL, then include
  % buffers whose names begin with "+".

  (let* ((b (buffer-find-or-create "+BUFFERS"))
	 (buffers (find-buffers all-buffers?))
	 (width (=> nmode-current-window width))
	 (current-item NIL)
	 (header-text (vector
		       (string-concat "   "
				      (string-pad-right "Buffer Name" 24)
				      (string-pad-left "Size" 6)
				      "  "
				      "File Name"
				      )
		       ""
		       ))
	 (items
	  (for (in b buffers)
	       (collect
		(let ((item (create-buffer-browser-item b width)))
		  (if (eq b nmode-current-buffer)
		    (setf current-item item))
		  item))
	       ))
	 )
    (buffer-set-mode b Buffer-Browser-Mode)
    (let ((browser
	   (create-browser b NIL header-text items #'buffer-browser-name-sorter)
	   ))
      (=> browser select-item current-item)
      )
    (browser-enter b)
    (buffer-browser-help)
    ))

(de find-buffers (all-buffers?)
  % Return a list of buffers.

  (if all-buffers?
    nmode-selectable-buffers
    (nmode-user-buffers)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Special Buffer Browser commands:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de buffer-browser-help ()
  (write-message
"View Edit File-it Un/Delete Kill-now Ignore uN-ignore Sort Reverse-sort Quit"
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
     (nmode-set-immediate-prompt "Reverse Sort by (Name, Size, File, Modified) ")
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
  (
   display-text
   display-width
   buffer
   (delete-flag NIL)
   )
  ()
  (gettable-instance-variables display-text buffer)
  (initable-instance-variables)
  )

(defmethod (buffer-browser-item init) (init-plist)
  (setf display-text
    (string-concat " "
		   (if (=> buffer modified?) "*" " ")
		   " "
		   (string-pad-right (=> buffer name) 24)
		   (string-pad-left (bldmsg "%d" (=> buffer visible-size)) 6)
		   "  "
		   (or (=> buffer file-name) "")
		   )
    ))

(defmethod (buffer-browser-item delete) ()
  (when (not delete-flag)
    (cond ((not (buffer-killable? buffer))
	   (nmode-error
	    (BldMsg "Buffer %w may not be deleted!" (=> buffer name)))
	   )
	  (t
	   (setf display-text (copystring display-text))
	   (string-store display-text 0 #/D)
	   (setf delete-flag T)
	   ))))

(defmethod (buffer-browser-item undelete) ()
  (when delete-flag
    (setf display-text (copystring display-text))
    (string-store display-text 0 #\space)
    (setf delete-flag NIL)
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
	 (buffer-kill-and-detach buffer)
	 T
	 )))

(defmethod (buffer-browser-item view-buffer) (x)
  (if (buffer-is-selectable? buffer) buffer)
  )

(defmethod (buffer-browser-item cleanup) ()
  )

(defmethod (buffer-browser-item apply-filter) (filter)
  (apply filter (list buffer))
  )

(defmethod (buffer-browser-item save-file) ()
  (when (=> buffer modified?)
    (save-file buffer)
    (when (not (=> buffer modified?))
      (setf display-text (copystring display-text))
      (string-store display-text 1 #\space)
      )))

(defmethod (buffer-browser-item set-unmodified) ()
  (when (=> buffer modified?)
    (=> buffer set-modified? NIL)
    (when (not (=> buffer modified?))
      (setf display-text (copystring display-text))
      (string-store display-text 1 #\space)
      )))
