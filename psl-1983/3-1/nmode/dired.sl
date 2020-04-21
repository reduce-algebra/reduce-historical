%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% DIRED.SL - Directory Editor Subsystem
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        16 July 1982
% Revised:     11 April 1983
%
% This file implements a directory editor subsystem.
%
% 5-April-83 Jeff Soreff
%  Added filter functions to dired commands.
% 17-Mar-83 Alan Snyder
%  Bug fix: new item made by create command had wrong width.
% 14-Mar-83 Alan Snyder
%  Fix C-X D to view directory of current file, rather than connected
%  directory, when the current filename has only a device field.  Add Create
%  and Look commands.  Change to sort based on displayed name rather than full
%  name (since that's what the user sees).  Check for NIL dates in sort
%  functions.  Change to cleanup item when killed.  Convert for revised
%  browser mechanism.
% 4-Mar-83 Alan Snyder
%  Fix to work with files whose names are not valid pathnames.
% 3-Mar-83 Alan Snyder
%  Add Browse command to browse subdirectories.
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

(BothTimes (load extended-char fast-strings numeric-operators))
(load directory stringx)
(on fast-integers)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% External variables:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(
  nmode-current-buffer
  nmode-terminal
  nmode-command-argument
  nmode-command-argument-given
  ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal static variables:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(file-browser-mode
	 file-browser-command-list
	 file-browser-documentation-text
	 file-browser-help-text
	 dired-argument-list
	 ))

(setf file-browser-help-text
  ["? View Edit Browse Create Filter Un/Delete Kill-now uN/Ignore Sort/Reverse Look Quit"])

(setf file-browser-documentation-text
  ["The File Browser displays the files in a directory."
   "Terminology: the current file is the file pointed at by the cursor." 
   "The View (V) and Edit (E) commands both display the current file." 
   "In split-screen mode, Edit selects the bottom window while View does not." 
   "The Create (C) command creates a new file, but does not select it." 
   "The Filter (F) command removes a set of files from the display."
   "The Delete (D) command marks the current file for deletion upon Quit." 
   "The Undelete (U) command removes the mark made by the Delete command." 
   "The Kill (K) command deletes the current file immediately." 
   "The Ignore (I) command removes the current file from the display."
   "The uNignore (N) command restores all Ignored files to the display."
   "The Sort (S) command sorts the files in various ways."
   "The Reverse (R) command sorts the files in reverse order."
   "The Look (L) command re-reads the directory to get up-to-date info."
   "The Quit (Q) command exits the browser and deletes any marked files,"
   "after first asking for permission."
   ])

(setf file-browser-mode (nmode-define-mode "File-Browser" '(
  (nmode-define-commands File-Browser-Command-List)
  (nmode-establish-mode Read-Only-Text-Mode)
  )))

(setf file-browser-command-list (list
    (cons (x-char ?) 'browser-help-command)
    (cons (x-char B) 'dired-browse-command)
    (cons (x-char C) 'dired-create-command)
    (cons (x-char D) 'browser-delete-command)
    (cons (x-char E) 'browser-edit-command)
    (cons (x-char F) 'dired-filter-command)
    (cons (x-char I) 'browser-ignore-command)
    (cons (x-char K) 'browser-kill-command)
    (cons (x-char L) 'dired-look-command)
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
      (setf directory-name (namestring (pathname-without-name fn)))
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

(define-browser-prototype 'edit-directory-command
			  "File Directory Browser"
			  ["This prototype creates a browser for the"
			   "set of files in a directory."])

(de directory-editor (directory-name)
  % Put up a directory editor subsystem, containing all files that match the
  % specified string.  If the string specifies a directory, then all files in
  % that directory are displayed.

  (setf directory-name (fixup-directory-name directory-name))
  (write-prompt "Reading directory or directories...")
  (let ((file-list (find-matching-files directory-name t)))
    (if (null file-list)
      (write-prompt (BldMsg "No files match: %w" directory-name))
      % otherwise
      (let* ((browser (or (find-browser 'FILE-BROWSER directory-name)
			  (create-file-browser directory-name)
			  ))
	     (items (dired-create-items file-list (=> browser display-width)))
	     )
	(=> browser set-items items)
	(browser-enter browser)
	))))

(de create-file-browser (directory-name)
  (let* ((header-text (vector
		       (string-concat "Directory List of " directory-name)
		       ""
		       ))
	 (browser
	  (create-browser 'FILE-BROWSER "Files" directory-name
			  file-browser-mode NIL header-text
			  file-browser-documentation-text
			  file-browser-help-text
			  () #'dired-filename-sorter)
	  ))
    (=> browser put 'directory-name directory-name)
    browser
    ))

(de dired-create-items (file-list display-width)
  % Accepts a list containing one element per file, where each element is
  % a list.  Returns a list of file-browser-items.

  (when file-list
    (let* ((names (for (in f file-list)
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

(de dired-browse-command ()
  % Browse the current item (presumably, a subdirectory).

  (let* ((browser (current-browser))
	 (item (=> browser current-item))
	 )
    (if item
      (directory-editor (=> item full-name))
      (Ding)
      )))

(de dired-create-command ()
  (let* ((browser (current-browser))
	 (dir-pn (pathname-without-name (=> browser get 'directory-name)))
	 (fn (prompt-for-string "Create file whose name is:" NIL))
	 (pn (maybe-pathname fn))
	 sout
	 )
    (if (not pn)
      (nmode-error (bldmsg "Invalid pathname: %w" fn))
      % otherwise
      (if (not (and (null (pathname-device pn))
		    (null (pathname-directory pn))
		    ))
	(nmode-error "Device and directory may not be specified.")
	% otherwise
	(setf pn (merge-pathname-defaults dir-pn
					  (pathname-name pn)
					  (pathname-type pn)
					  (pathname-version pn)
					  ))
	(setf fn (namestring pn))
	(if (filep fn)
	  (nmode-error (bldmsg "File %w already exists." fn))
	  % otherwise
	  (setf fn (actualize-file-name fn))
	  (if (or (not fn) (not (setf sout (attempt-to-open-output fn))))
	    (nmode-error (bldmsg "Unable to create file: %w" (namestring pn)))
	    % otherwise
	    (=> sout close)
	    (let ((item (create-file-browser-item
			 (=> browser display-width)
			 fn
			 (file-namestring fn)
			 nil 0 nil nil)))
	      (browser-add-item-and-view item)
	      )))))))

(de dired-look-command ()
  % Reinitialize the file directory browser.

  (write-prompt "Reading directory or directories...")
  (let* ((browser (current-browser))
	 (directory-name (=> browser get 'directory-name))
	 (file-list (find-matching-files directory-name t))
	 (items (dired-create-items file-list (=> browser display-width)))
	 )
    (=> browser set-items items)
    ))

(de dired-filter-command ()
  (nmode-set-immediate-prompt "Flush or Keep matching filenames?")
  (dired-filter-dispatch))

(de dired-filter-dispatch ()
  (selectq (char-upcase (input-base-character))
    (#/F (dired-filter-compose t))
    (#/K (dired-filter-compose nil))
    (#/?
     (nmode-set-immediate-prompt
      "Type F to flush or K to keep matching filenames.")
     (dired-filter-dispatch))
    (t (write-prompt "") (Ding))))

(de dired-filter-compose (flag)
  (let ((browser (current-browser))
	(dired-argument-list
	 (list
	  (string-upcase
	   (prompt-for-string
	    (if flag
	      "Flush filenames matching what string?"
	      "Keep filenames matching what string?")
	    ""))
	  flag)))
    (=> browser filter-items #'dired-string-filter-predicate)))

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

(de dired-string-filter-predicate (file-browser-item)
  (let* ((nice-name (=> file-browser-item nice-name))
	 (match (forward-search-in-string 
		 nice-name (first dired-argument-list))))
    (when (second dired-argument-list)
      (setf match (not match)))
    match))

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
  (let ((d1 (or (=> f1 write-date) 0))
	(d2 (or (=> f2 write-date) 0))
	)
       (or (LessP d1 d2)
	   (and (EqN d1 d2) (dired-filename-sorter f1 f2))
	   )))

(de dired-write-reverser (f1 f2)
  (let ((d1 (or (=> f1 write-date) 0))
	(d2 (or (=> f2 write-date) 0))
	)
       (or (GreaterP d1 d2)
	   (and (EqN d1 d2) (dired-filename-sorter f1 f2))
	   )))

(de dired-read-sorter (f1 f2)
  (let ((d1 (or (=> f1 read-date) 0))
	(d2 (or (=> f2 read-date) 0))
	)
       (or (LessP d1 d2)
	   (and (EqN d1 d2) (dired-filename-sorter f1 f2))
	   )))

(de dired-read-reverser (f1 f2)
  (let ((d1 (or (=> f1 read-date) 0))
	(d2 (or (=> f2 read-date) 0))
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
  (let ((pn (maybe-pathname nice-name)))
    (setf sort-name
      (if pn (namestring (pathname-without-version pn)) nice-name))
    (setf version-number (if pn (pathname-version pn) 0))
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

(defmethod (file-browser-item update) ()
  % Updating is too expensive, so we do nothing.
  T
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
  (let ((result (nmode-delete-file full-name)))
    (when result
      (=> self cleanup)
      )
    result
    ))

(defmethod (file-browser-item view-buffer) (x)
  (or (find-file-in-existing-buffer full-name)
      (setf buffer (find-file-in-buffer full-name T))
      ))

(defmethod (file-browser-item cleanup) ()
  (when (and buffer (not (=> buffer modified?)))
    (when (buffer-is-selectable? buffer)
      (=> buffer set-previous-buffer NIL) % don't display the browser
      (buffer-kill-and-detach buffer)
      )
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(off fast-integers)
