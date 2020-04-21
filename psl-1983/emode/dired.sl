%
% DIRED.SL - Directory Editor Subsystem for EMODE
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        16 July 1982
%
% This file implements a directory editor subsystem.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load common strings directory gsort))

(fluid '(CurrentLineIndex point WindowsBufferName BufferPreviousBuffer
	 BufferAuxiliaryInfo CurrentBufferName DefaultMode buffers_file))

(fluid '(DiredMode))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Macros
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmacro fi-full-name (fi) `(nth ,fi 1))   % string for file primitives
(defmacro fi-deleted? (fi) `(nth ,fi 2))    % is file marked 'deleted'?
(defmacro fi-size (fi) `(nth ,fi 3))        % "size" of file
(defmacro fi-write-date (fi) `(nth ,fi 4))  % date/time file last written
(defmacro fi-read-date (fi) `(nth ,fi 5))   % date/time file last read
(defmacro fi-nice-name (fi) `(nth ,fi 6))   % string to show user

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(setf DiredMode
      '((SetKeys DiredDispatchList)
	(SetKeys ReadOnlyTextDispatchList)
	(SetKeys RlispDispatchList)
	(SetKeys BasicDispatchList)))

(setf DiredDispatchList (list

    % These are the DIRED-specific commands.

    (cons (char ?) 'dired-help)
    (cons (char C) 'dired-srccom-file)
    (cons (char D) 'dired-delete-file)
    (cons (char E) 'dired-edit-file)
    (cons (char H) 'dired-automatic-delete)
    (cons (char K) 'dired-delete-file)
    (cons (char N) 'dired-next-hog)
    (cons (char Q) 'dired-exit)
    (cons (char R) 'dired-reverse-sort)
    (cons (char S) 'dired-sort)
    (cons (char U) 'dired-undelete)
    (cons (char X) 'dired-exit)
    (cons (char rubout) 'dired-reverse-undelete)
    (cons (char space) '$ForwardLine)
    (cons (char (cntrl D)) 'dired-delete-file)
    (cons (char (cntrl K)) 'dired-delete-file)
    ))

(de dired-command ()
  (write-prompt "")
  (let* ((directory-name (prompt_for_string "Directory to edit: " buffers_file))
	 file-list
         )
    (write-prompt "Reading directory(ies)...")
    (setf file-list (find-matching-files directory-name t))
    (if (null file-list)
      (write-prompt (BldMsg "No files match: %w" directory-name))
      % ELSE
      (dired-fixup-file-list file-list)
      (SelectBuffer (buffer-create '*Dired DiredMode))
      (setf BufferPreviousBuffer WindowsBufferName)
      (setf BufferAuxiliaryInfo file-list)
      (setf buffers_file directory-name)
      (load-dired-buffer BufferAuxiliaryInfo)
      (setf WindowsBufferName CurrentBufferName)
      (EstablishCurrentMode)
      (write-prompt "")
      )
    )
  )

(de dired-fixup-file-list (file-list)
  % Adds to each element:
  % A cleaned-up file name for display and sorting purposes.

  (for (in file-info file-list)
       (do
	 (aconc file-info (fixup-file-name (fi-full-name file-info)))
	 ))
  (let ((prefix (if file-list (fi-nice-name (first file-list)) ""))
        prefix-length
        name)
    (for (in file-info file-list)
         (do (setf prefix
	       (string-largest-common-prefix prefix (fi-nice-name file-info))
	      ))
	 )
    (setf prefix (trim-filename-to-prefix prefix))
    (setf prefix-length (+ 1 (size prefix)))
    (for (in file-info file-list)
         (do (setf name (fi-nice-name file-info))
	     (setf (fi-nice-name file-info)
		   (sub name
			prefix-length
		        (- (size name) prefix-length))))
	 ))
  )

(de load-dired-buffer (file-list)
  ($DeleteBuffer)
  (for* (in file-info file-list)
        (do (insert_string (file-info-to-string file-info))
            ($CRLF))
        )
  (setf point 0)
  (SelectLine 0)
  )

(de file-info-to-string (file-info)
  (let ((first-part (if (fi-deleted? file-info) "D " "  "))
	(file-name (string-pad-right (fi-nice-name file-info) 34))
	(file-size (string-pad-left (BldMsg "%d" (fi-size file-info)) 4))
	(write-date (file-date-to-string (fi-write-date file-info)))
	(read-date (file-date-to-string (fi-read-date file-info))))
   (string-concat first-part file-name file-size " " write-date " " read-date)
   ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DIRED command procedures:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de dired-exit ()
  (let* ((actions (dired-determine-actions BufferAuxiliaryInfo))
         command
         )
    (if (and (null (first actions)) (null (second actions)))
      (window-kill-buffer)
      % else
      (setf command (dired-present-actions actions))
      (cond
        ((eq command 'exit) (window-kill-buffer))
        ((eq command t) (dired-perform-actions actions) (window-kill-buffer))
        )
    )))

(de dired-delete-file ()
  % Mark the current file as deleted.
  (cond ((current-line-empty) (Ding))
        (t
	  (if (= (current-line-fetch 0) (char space))
	    (current-line-store 0 (char D)))
	  (move-to-next-line)
	)))

(de dired-undelete ()
  % Unmark the current file.
  (cond ((current-line-empty) (Ding))
        (t
	  (if (= (current-line-fetch 0) (char D))
	    (current-line-store 0 (char space)))
	  (move-to-next-line)
	)))

(de dired-reverse-undelete ()
  % Unmark the previous file.
  (cond ((= CurrentLineIndex 0) (Ding))
        (t
          (move-to-previous-line)
	  (if (= (current-line-fetch 0) (char D))
	    (current-line-store 0 (char space)))
	)))

(de dired-help ()
  (write-prompt
 "DIRED: D-delete, U-undelete, E-edit file, S-sort, R-reverse sort, Q-exit")
  )

(de dired-next-hog ()
  (write-prompt "The DIRED NEXT HOG command is unimplemented.") (Ding)
  )

(de dired-automatic-delete ()
  (write-prompt "The DIRED AUTOMATIC DELETE command is unimplemented.") (Ding)
  )

(de dired-edit-file ()
  (write-prompt "")
  (if (not (dired-valid-line)) (Ding)
    (let* ((file-info (nth BufferAuxiliaryInfo (+ CurrentLineIndex 1)))
	   (file-name (fi-full-name file-info))
	   (old-buffer CurrentBufferName)
	   )

      (find-file file-name)
      (setf BufferPreviousBuffer old-buffer)
      (write-prompt "C-M-L returns to DIRED; C-X K kills buffer and returns.")
      )
    )
  )

(de dired-reverse-sort ()
  (write-prompt "Reverse Sort by ")
  (while t
    (let ((ch (RaiseChar (GetNextCommandCharacter))))
      (cond
        ((= ch (char F))
	  (dired-perform-sort "Reverse Sort by Filename" 'dired-filename-reverser)
	  (exit))
        ((= ch (char S))
	  (dired-perform-sort "Reverse Sort by Size" 'dired-size-reverser)
	  (exit))
        ((= ch (char W))
	  (dired-perform-sort "Reverse Sort by Write date" 'dired-write-reverser)
	  (exit))
        ((= ch (char R))
	  (dired-perform-sort "Reverse Sort by Read date" 'dired-read-reverser)
	  (exit))
        ((= ch (char ?))
	  (write-prompt "Reverse Sort by (Filename, Size, Read date, Write date) ")
	  (next))
	(t (write-prompt "") (Ding) (exit))
	))))

(de dired-sort ()
  (write-prompt "Sort by ")
  (while t
    (let ((ch (RaiseChar (GetNextCommandCharacter))))
      (cond
        ((= ch (char F))
	  (dired-perform-sort "Sort by Filename" 'dired-filename-sorter)
	  (exit))
        ((= ch (char S))
	  (dired-perform-sort "Sort by Size" 'dired-size-sorter)
	  (exit))
        ((= ch (char W))
	  (dired-perform-sort "Sort by Write date" 'dired-write-sorter)
	  (exit))
        ((= ch (char R))
	  (dired-perform-sort "Sort by Read date" 'dired-read-sorter)
	  (exit))
        ((= ch (char ?))
	  (write-prompt "Sort by (Filename, Size, Read date, Write date) ")
	  (next))
	(t (write-prompt "") (Ding) (exit))
	))))

(de dired-srccom-file ()
  (write-prompt "The DIRED SRCCOM command is unimplemented.") (Ding)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DIRED Support Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de dired-valid-line ()
  (and
    (>= CurrentLineIndex 0)
    (> (current-line-length) 60)
    (= (current-line-fetch 1) (char space))))

(de dired-determine-actions (file-list)
  % Return a list containing two lists: the first a list of
  % file names to be deleted, the second a list of file names
  % to be undeleted.

  (let ((old-line CurrentLineIndex))
    (SelectLine 0)
    (prog1
    (for*
      (in file-info file-list)
      (with delete-list undelete-list file-name file-status desired-status)
      (do
        (setf file-name (fi-full-name file-info))
        (setf file-status (file-deleted-status file-name))
        (setf desired-status (current-line-fetch 0))
        (move-to-next-line)
        (if file-status
          (cond
	    ((and (eq file-status 'deleted) (= desired-status (char space)))
	      (setf undelete-list (append undelete-list (list file-name))))
	    ((and (neq file-status 'deleted) (= desired-status (char D)))
	      (setf delete-list (append delete-list (list file-name))))
	    )))
      (returns (list delete-list undelete-list))
      )
    (SelectLine old-line))))

(de dired-present-actions (action-list)
  (let ((delete-list (first action-list))
	(undelete-list (second action-list))
        ch)

    % This is a terrible way of outputting information, but it is
    % the way EMODE already does it.

    (SelectOldChannels)
    (ClearScreen)
    (dired-present-list delete-list "These files to be deleted:")
    (dired-present-list undelete-list "These files to be undeleted:")
    (prog1
      (while t
        (printf "%nDo It (YES, N, X)? ")
        (setf ch (get-upchar))
        (cond
	  ((= ch (char Y))
	    (if (= (get-upchar) (char E))
	      (if (= (get-upchar) (char S))
	        (exit T)
	        (Ding) (next))
	      (Ding) (next))
	   )
          ((= ch (char N)) (exit NIL))
	  ((= ch (char X)) (exit 'EXIT))
          ((= ch (char ?))
             (printf "%n YES-Do it, N-Return to DIRED, X-Exit from DIRED.")
             )
	  (t (Ding))
	  ))
      (ClearScreen)
      )
    ))

(de get-upchar ()
  (let ((ch (GetNextCommandCharacter)))
    (cond ((AlphaP ch) (setf ch (char-upcase ch)) (WriteChar ch) ch)
          (t ch))))

(de dired-present-list (list prompt)
  (if list (progn
    (printf "%w%n" prompt)
    (for (in item list)
         (for count 0 (if (= count 1) 0 (+ count 1)))
         (do (printf "%w" (string-pad-right item 38))
	     (if (= count 1) (printf "%n"))
	     )
         )
    (printf "%n")
    )))

(de dired-perform-actions (action-list)
  (let ((delete-list (first action-list))
	(undelete-list (second action-list))
        )
    (for (in file delete-list)
         (do (file-delete file)))
    (for (in file undelete-list)
         (do (file-undelete file)))
    ))

(de dired-perform-sort (prompt sorter)
  (write-prompt prompt)
  (setf BufferAuxiliaryInfo (GSort BufferAuxiliaryInfo sorter))
  (load-dired-buffer BufferAuxiliaryInfo)
  )

(de dired-filename-sorter (f1 f2)
  (StringSortFn (fi-nice-name f1) (fi-nice-name f2)))

(de dired-filename-reverser (f1 f2)
  (StringSortFn (fi-nice-name f2) (fi-nice-name f1)))

(de dired-size-sorter (f1 f2)
  (or (< (fi-size f1) (fi-size f2))
      (and (= (fi-size f1) (fi-size f2))
           (StringSortFn (fi-nice-name f1) (fi-nice-name f2)))
      ))

(de dired-size-reverser (f1 f2)
  (or (> (fi-size f1) (fi-size f2))
      (and (= (fi-size f1) (fi-size f2))
           (StringSortFn (fi-nice-name f1) (fi-nice-name f2)))
      ))

(de dired-write-sorter (f1 f2)
  (or (< (fi-write-date f1) (fi-write-date f2))
      (and (= (fi-write-date f1) (fi-write-date f2))
           (StringSortFn (fi-nice-name f1) (fi-nice-name f2)))
      ))

(de dired-write-reverser (f1 f2)
  (or (> (fi-write-date f1) (fi-write-date f2))
      (and (= (fi-write-date f1) (fi-write-date f2))
           (StringSortFn (fi-nice-name f1) (fi-nice-name f2)))
      ))

(de dired-read-sorter (f1 f2)
  (or (< (fi-read-date f1) (fi-read-date f2))
      (and (= (fi-read-date f1) (fi-read-date f2))
           (StringSortFn (fi-nice-name f1) (fi-nice-name f2)))
      ))

(de dired-read-reverser (f1 f2)
  (or (> (fi-read-date f1) (fi-read-date f2))
      (and (= (fi-read-date f1) (fi-read-date f2))
           (StringSortFn (fi-nice-name f1) (fi-nice-name f2)))
      ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Useful String Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de string-pad-right (s desired-length)
  (let ((len (string-length s)))
    (if (< len desired-length)
      (string-concat s (make-string (- desired-length len) (char space)))
      s)))

(de string-pad-left (s desired-length)
  (let ((len (string-length s)))
    (if (< len desired-length)
      (string-concat (make-string (- desired-length len) (char space)) s)
      s)))

(de string-largest-common-prefix (s1 s2)
  (for (from i 0 (min (size s1) (size s2)) 1)
       (while (= (indx s1 i) (indx s2 i)))
       (returns (sub s1 0 (- i 1)))
       ))
