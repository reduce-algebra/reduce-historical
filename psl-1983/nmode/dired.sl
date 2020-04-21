%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% DIRED.SL - Directory Editor Subsystem
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        16 July 1982
% Revised:     16 February 1983
%
% This file implements a directory editor subsystem.
%
% 16-Feb-83 Alan Snyder
%  Declare -> Declare-Flavor.
%  Fix cleanup method to NIL out the buffer variable to allow the buffer object
%  to be garbage collected.
% 11-Feb-83 Alan Snyder
%  Fix bug in previous change.
% 8-Feb-83 Alan Snyder
%  Enlarge width of size field in display.
% 4-Feb-83 Alan Snyder
%  Rewritten to use new browser support.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load extended-char fast-strings))
(load directory stringx)

% External variables:

(fluid '(
  nmode-current-buffer
  nmode-current-window
  nmode-terminal
  nmode-command-argument
  nmode-command-argument-given
  ))

% Internal static variables:

(fluid '(File-Browser-Mode File-Browser-Command-List))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(setf File-Browser-Mode (nmode-define-mode "File-Browser" '(
  (nmode-define-commands File-Browser-Command-List)
  (nmode-establish-mode Read-Only-Text-Mode)
  )))

(setf File-Browser-Command-List (list
    (cons (x-char ?) 'dired-help)
    (cons (x-char D) 'browser-delete-command)
    (cons (x-char E) 'browser-edit-command)
    (cons (x-char I) 'browser-ignore-command)
    (cons (x-char K) 'browser-kill-command)
    (cons (x-char N) 'browser-undo-filter-command)
    (cons (x-char Q) 'dired-exit)
    (cons (x-char R) 'dired-reverse-sort)
    (cons (x-char S) 'dired-sort)
    (cons (x-char U) 'browser-undelete-command)
    (cons (x-char V) 'browser-view-command)
    (cons (x-char X) 'dired-exit)
    (cons (x-char BACKSPACE) 'browser-undelete-backwards-command)
    (cons (x-char RUBOUT) 'browser-undelete-backwards-command)
    (cons (x-char SPACE) 'move-down-command)
    (cons (x-char control D) 'browser-delete-command)
    (cons (x-char control K) 'browser-kill-command)
    ))

(de dired-command ()
  (let ((fn (=> nmode-current-buffer file-name))
	directory-name
	)
    (cond
     ((or (not fn) (>= nmode-command-argument 4))
      (setf directory-name (prompt-for-string "Edit Directory: " NIL))
      )
     (nmode-command-argument-given
      (setf directory-name (namestring (pathname-without-version fn)))
      )
     (t
      (setf directory-name (directory-namestring fn))
      ))
    (directory-editor directory-name)
    ))

(de edit-directory-command ()
  (let* ((fn (=> nmode-current-buffer file-name))
	 (directory-name
	  (prompt-for-string
	   "Edit Directory:"
	   (and fn (directory-namestring fn))
	   )))
    (directory-editor directory-name)
    ))

(de directory-editor (directory-name)

  % Put up a directory editor subsystem, containing all files that match the
  % specified string.  If the string specifies a directory, then all files in
  % that directory are used.

  (setf directory-name (fixup-directory-name directory-name))
  (write-prompt "Reading directory or directories...")
  (let ((items (dired-create-items (find-matching-files directory-name t))))
    (if (null items)
      (write-prompt (BldMsg "No files match: %w" directory-name))
      % ELSE
      (let* ((b (buffer-create "+FILES" File-Browser-Mode))
	     (header-text (vector
	         (string-concat "Directory List of " directory-name)
		 ""
		 ))
	     )
	(=> b put 'directory-name directory-name)
	(create-browser b NIL header-text items #'dired-filename-sorter)
        (browser-enter b)
	(dired-help)
	))))

(de dired-create-items (file-list)
  % Accepts a list containing one element per file, where each element is
  % a list.  Returns a list of file-browser-items.

  (when file-list
    (let* ((display-width (=> nmode-current-window width))
	   (names (for (in f file-list)
		       (collect (fixup-file-name (nth f 1)))
		       ))
	   (prefix (trim-filename-to-prefix
		    (strings-largest-common-prefix names)))
	   (prefix-length (string-length prefix))
	   )
      (for (in f file-list)
	   (collect
	    (create-file-browser-item
	     display-width
	     (nth f 1) % full-name
	     (string-rest (fixup-file-name (nth f 1)) prefix-length) % nice-name
	     (nth f 2) % deleted?
	     (nth f 3) % size
	     (nth f 4) % write-date
	     (nth f 5) % read-date
	     ))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DIRED command procedures:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de dired-exit ()
  (let ((actions (dired-determine-actions nmode-current-buffer)))
    (if (and (null (first actions)) (null (second actions)))
      (browser-exit-command)
      % else
      (let ((command (dired-present-actions actions)))
	(cond
	 ((eq command 'exit)
	  (browser-exit-command)
	  )
	 ((eq command t)
	  (dired-perform-actions actions)
	  (browser-exit-command)
	  )
	 ))
    )))

(de dired-help ()
  (write-message
"View Edit Un/Delete Kill-now Ignore uN-ignore Sort Reverse-sort Quit"
  ))

(de dired-reverse-sort ()
  (nmode-set-immediate-prompt "Reverse Sort by ")
  (dired-reverse-sort-dispatch)
  )

(de dired-reverse-sort-dispatch ()
  (selectq (char-upcase (input-base-character))
   (#/F (browser-sort "Reverse Sort by Filename" 'dired-filename-reverser))
   (#/S (browser-sort "Reverse Sort by Size" 'dired-size-reverser))
   (#/W (browser-sort "Reverse Sort by Write date" 'dired-write-reverser))
   (#/R (browser-sort "Reverse Sort by Read date" 'dired-read-reverser))
   (#/?
     (nmode-set-immediate-prompt
      "Reverse Sort by (Filename, Size, Read date, Write date) ")
     (dired-reverse-sort-dispatch)
     )
   (t (write-prompt "") (Ding))
   ))

(de dired-sort ()
  (nmode-set-immediate-prompt "Sort by ")
  (dired-sort-dispatch)
  )

(de dired-sort-dispatch ()
  (selectq (char-upcase (input-base-character))
   (#/F (browser-sort "Sort by Filename" 'dired-filename-sorter))
   (#/S (browser-sort "Sort by Size" 'dired-size-sorter))
   (#/W (browser-sort "Sort by Write date" 'dired-write-sorter))
   (#/R (browser-sort "Sort by Read date" 'dired-read-sorter))
   (#/? (nmode-set-immediate-prompt
	 "Sort by (Filename, Size, Read date, Write date) ")
	(dired-sort-dispatch)
	)
   (t (write-prompt "") (Ding))
   ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DIRED Support Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de dired-determine-actions (b)
  % Return a list containing two lists: the first a list of file names to be
  % deleted, the second a list of file names to be undeleted.

  (let ((items (=> (=> b get 'browser) items))
	(delete-list ())
	(undelete-list ())
	)
    (for (in item items)
	 (do (selectq (=> item action-wanted)
	       (delete
		(setf delete-list (aconc delete-list (=> item full-name))))
	       (undelete
		(setf undelete-list (aconc undelete-list (=> item full-name))))
	       )))
    (list delete-list undelete-list)
    ))

(de dired-present-actions (action-list)
  (let ((delete-list (first action-list))
	(undelete-list (second action-list))
        )
    (nmode-begin-typeout)
    (dired-present-list delete-list "These files to be deleted:")
    (dired-present-list undelete-list "These files to be undeleted:")
    (while t
      (printf "%nDo It (YES, N, X)? ")
      (selectq (get-upchar)
       (#/Y
	(if (= (get-upchar) #/E)
	    (if (= (get-upchar) #/S)
		(exit T)
		(Ding) (next))
	    (Ding) (next))
	)
       (#/N (exit NIL))
       (#/X (exit 'EXIT))
       (#/? (printf "%n YES-Do it, N-Return to DIRED, X-Exit from DIRED."))
       (t (Ding))
       ))))

(de get-upchar ()
  % This function is used during "normal PSL" typeout, so we cannot use
  % the NMODE input functions, for they will refresh the NMODE windows.

  (let ((ch (X-Base (=> nmode-terminal get-character))))
    (when (AlphaP ch) (setf ch (char-upcase ch)) (WriteChar ch))
    ch))

(de dired-present-list (list prompt)
  (when list
    (printf "%w%n" prompt)
    (for (in item list)
         (for count 0 (if (= count 1) 0 (+ count 1)))
         (do (printf "%w" (string-pad-right item 38))
	     (if (= count 1) (printf "%n"))
	     )
         )
    (printf "%n")
    ))

(de dired-perform-actions (action-list)
  (let ((delete-list (first action-list))
	(undelete-list (second action-list))
        )
    (for (in file delete-list)
         (do (file-delete file)))
    (for (in file undelete-list)
         (do (file-undelete file)))
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sorting predicates:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(declare-flavor file-browser-item f1 f2)

(de dired-filename-sorter (f1 f2)
  (let ((n1 (=> f1 sort-name))
	(n2 (=> f2 sort-name))
	)
    (if (string= n1 n2)
      (<= (=> f1 version-number) (=> f2 version-number))
      (string<= n1 n2)
      )))

(de dired-filename-reverser (f1 f2)
  (not (dired-filename-sorter f1 f2)))

(de dired-size-sorter (f1 f2)
  (let ((size1 (=> f1 size))
	(size2 (=> f2 size))
	)
    (or (< size1 size2)
	(and (= size1 size2)
	     (dired-filename-sorter f1 f2))
	)))

(de dired-size-reverser (f1 f2)
  (let ((size1 (=> f1 size))
	(size2 (=> f2 size))
	)
    (or (> size1 size2)
	(and (= size1 size2)
	     (dired-filename-sorter f1 f2))
	)))

(de dired-write-sorter (f1 f2)
  (let ((d1 (=> f1 write-date))
	(d2 (=> f2 write-date))
	)
       (or (LessP d1 d2)
	   (and (EqN d1 d2) (dired-filename-sorter f1 f2))
	   )))

(de dired-write-reverser (f1 f2)
  (let ((d1 (=> f1 write-date))
	(d2 (=> f2 write-date))
	)
       (or (GreaterP d1 d2)
	   (and (EqN d1 d2) (dired-filename-sorter f1 f2))
	   )))

(de dired-read-sorter (f1 f2)
  (let ((d1 (=> f1 read-date))
	(d2 (=> f2 read-date))
	)
       (or (LessP d1 d2)
	   (and (EqN d1 d2) (dired-filename-sorter f1 f2))
	   )))

(de dired-read-reverser (f1 f2)
  (let ((d1 (=> f1 read-date))
	(d2 (=> f2 read-date))
	)
       (or (GreaterP d1 d2)
	   (and (EqN d1 d2) (dired-filename-sorter f1 f2))
	   )))

(undeclare-flavor f1 f2)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The file-browser-item flavor:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de create-file-browser-item (width full-name nice-name deleted? size
				    write-date read-date)
  (make-instance 'file-browser-item
		 'full-name full-name
		 'nice-name nice-name
		 'deleted? deleted?
		 'size size
		 'write-date write-date
		 'read-date read-date
		 'display-width width
		 ))

(defflavor file-browser-item
  (
   display-text
   display-width
   full-name		% full name of file
   nice-name		% file name as displayed
   sort-name		% name without version (for sorting purposes)
   version-number	% version number (or 0) (for sorting purposes)
   size			% size of file (arbitrary units)
   write-date		% write date of file (or NIL)
   read-date		% read date of file (or NIL)
   deleted?		% file is actually deleted
   delete-flag		% user wants file deleted
   (buffer NIL)		% buffer created to view file
   )
  ()
  (gettable-instance-variables display-text full-name nice-name
			       sort-name version-number
			       size write-date read-date)
  (initable-instance-variables)
  )

(defmethod (file-browser-item init) (init-plist)
  (let ((pn (pathname full-name)))
    (setf sort-name (namestring (pathname-without-version pn)))
    (setf version-number (pathname-version pn))
    (if (not (fixp version-number)) (setf version-number 0))
    )
  (setf display-text
    (string-concat
     (if deleted? "D " "  ")
     (string-pad-right nice-name (- display-width 48))
     (string-pad-left (BldMsg "%d" size) 8)
     (string-pad-left (if write-date (file-date-to-string write-date) "") 19)
     (string-pad-left (if read-date (file-date-to-string read-date) "") 19)
     ))
  (setf delete-flag deleted?)
  )

(defmethod (file-browser-item delete) ()
  (when (not delete-flag)
    (setf display-text (copystring display-text))
    (string-store display-text 0 #/D)
    (setf delete-flag T)
    ))

(defmethod (file-browser-item undelete) ()
  (when delete-flag
    (setf display-text (copystring display-text))
    (string-store display-text 0 #\space)
    (setf delete-flag NIL)
    ))

(defmethod (file-browser-item deleted?) ()
  delete-flag
  )

(defmethod (file-browser-item kill) ()
  (nmode-delete-file full-name)
  )

(defmethod (file-browser-item view-buffer) (x)
  (or (find-file-in-existing-buffer full-name)
      (setf buffer (find-file-in-buffer full-name T))
      ))

(defmethod (file-browser-item cleanup) ()
  (when (and buffer (not (=> buffer modified?)))
    (if (buffer-is-selectable? buffer) (buffer-kill-and-detach buffer))
    (setf buffer NIL)
    ))

(defmethod (file-browser-item apply-filter) (filter)
  (apply filter (list self))
  )

(defmethod (file-browser-item action-wanted) ()
  % Return 'DELETE, 'UNDELETE, or NIL.
  (if (not (eq deleted? delete-flag)) % user wants some action taken
    (let ((file-status (file-deleted-status full-name)))
      (if file-status % File currently exists (otherwise, forget it)
	(let ((actually-deleted? (eq file-status 'deleted)))
	  (if (not (eq delete-flag actually-deleted?))
	    (if delete-flag 'DELETE 'UNDELETE)
	    ))))))
