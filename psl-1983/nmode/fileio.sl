%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FileIO.SL
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        17 September 1982
% Revised:     4 February 1983
%
% File I/O for NMODE.
%
% 4-Feb-83 Alan Snyder
%   Added functions for deleting/undeleting files and writing a message.
%   Find-file-in-buffer changed incompatibly to make it more useful.
%   Use nmode-error to report errors.
% 1-Feb-83 Alan Snyder
%   Added separate default string for Insert File command.
% 27-Dec-82 Alan Snyder
%   Removed runtime LOAD statements, for portability.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects fast-strings pathnames))

% External Variables:

(fluid '(nmode-selectable-buffers nmode-current-buffer nmode-screen
	 nmode-command-argument-given nmode-current-window Text-Mode
	 ))

% Internal static variables:

(fluid '(text-io-default-fn insert-file-default-fn))
(setf text-io-default-fn NIL)
(setf insert-file-default-fn NIL)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File commands:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de visit-file-command ()
  % Ask for and read in a file.
  (let ((fn (prompt-for-defaulted-filename "Visit File: " NIL)))
    (visit-file nmode-current-buffer fn)
    ))

(de insert-file-command ()
  % Ask for and read a file, inserting it into the current buffer.
  (setf insert-file-default-fn
    (prompt-for-file-name "Insert File: " insert-file-default-fn))
  (insert-file-into-buffer nmode-current-buffer insert-file-default-fn)
  )

(de write-file-command ()
  % Ask for filename, write out the buffer to the file.
  (write-buffer-to-file
   nmode-current-buffer
   (prompt-for-defaulted-filename "Write File:" NIL)))

(de save-file-command ()
  % Save current buffer on its associated file, ask for file if unknown.
  (cond
   ((not (=> nmode-current-buffer modified?))
    (write-prompt "(No changes need to be written)"))
   (t (save-file nmode-current-buffer))))

(de save-file-version-command ()
  % Save current buffer on its associated file, ask for file if unknown.
  % The file is written using the current version number.
  (cond
   ((not (=> nmode-current-buffer modified?))
    (write-prompt "(No changes need to be written)"))
   (t (save-file-version nmode-current-buffer))))

(de find-file-command ()
  % Ask for filename and then read it into a buffer created especially for that
  % file, or select already existing buffer containing the file.

  (find-file (prompt-for-defaulted-filename "Find file: " NIL))
  )

(de write-screen-photo-command ()
  % Ask for filename, write out the screen to the file.
  (write-screen-photo (prompt-for-file-name "Write Photo to File: " NIL)))

(de write-region-command ()
  % Ask for filename, write out the region to the file.
  (write-text-to-file
   (cdr (extract-region NIL (buffer-get-position) (current-mark)))
   (setf text-io-default-fn
     (prompt-for-file-name "Write Region to File:" text-io-default-fn))))

(de prepend-to-file-command ()
  % Ask for filename, prepend the region to the file.
  (prepend-text-to-file
   (cdr (extract-region NIL (buffer-get-position) (current-mark)))
   (setf text-io-default-fn
     (prompt-for-file-name "Prepend Region to File:" text-io-default-fn))))

(de append-to-file-command ()
  % Ask for filename, append the region to the file.
  (append-text-to-file
   (cdr (extract-region NIL (buffer-get-position) (current-mark)))
   (setf text-io-default-fn
     (prompt-for-file-name "Append Region to File:" text-io-default-fn))))

(de delete-file-command ()
  (nmode-delete-file (prompt-for-defaulted-filename "Delete File:" NIL)))

(de delete-and-expunge-file-command ()
  (nmode-delete-and-expunge-file
   (prompt-for-defaulted-filename "Delete and Expunge File:" NIL)))

(de undelete-file-command ()
  (nmode-undelete-file (prompt-for-defaulted-filename "Undelete File:" NIL)))

(de save-all-files-command ()
  % Save all files.  Ask first, unless arg given.
  (for
   (in b nmode-selectable-buffers)
   (do
    (cond ((and (=> b file-name)
		(=> b modified?)
		(or nmode-command-argument-given
		    (nmode-y-or-n?
		     (bldmsg "Save %w in %w (Y or N)?"
			     (=> b name) (=> b file-name)))
		    ))
	   (save-file b))
	  ))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de prompt-for-defaulted-filename (prompt b)
  % The default name is the name associated with the specified buffer (without
  % Version number).  Will throw 'ABORT if a bad file name is given.
  % If B is NIL, the "current" buffer is used.

  (let ((fn (=> (or b nmode-current-buffer) file-name)))
    (prompt-for-file-name prompt
			  (and fn (namestring (pathname-without-version fn)))
			  )))

(de prompt-for-file-name (prompt default-name)
  % Default-Name may be NIL.
  % Will throw 'ABORT if a bad file name is given.

  (let ((pn (pathname (prompt-for-string prompt default-name))))
    (if default-name
      (setf pn
	(attempt-to-merge-pathname-defaults pn default-name
					    (pathname-type default-name) NIL)))
    (namestring pn)
    ))

(de attempt-to-merge-pathname-defaults (pn dn type version)
  (let ((result (errset (merge-pathname-defaults pn dn type version) NIL)))
    (cond
     ((listp result) (car result))
     (t (write-prompt EMSG*)
	(throw 'ABORT)))))

(de read-file-into-buffer (b file-name)
  (=> b set-file-name file-name)
  (buffer-set-mode b (pathname-default-mode file-name))
  (let ((s (attempt-to-open-input file-name)))
    (if s
      (read-stream-into-buffer b s)
      % else
      (=> b reset)
      (=> b set-modified? NIL)
      (write-prompt "(New File)")
      )))

(de read-stream-into-buffer (b s)
  (let ((fn (=> s file-name)))
    (write-prompt (bldmsg "Reading file: %w" fn))
    (=> b read-from-stream s)
    (=> s close)
    (write-prompt (bldmsg "File read: %w (%d lines)" fn	(=> b visible-size)))
    ))

(de insert-file-into-buffer (buf pn)
  (let ((b (buffer-create-unselectable "FOO" Text-Mode)))
    (read-file-into-buffer b pn)
    (insert-buffer-into-buffer b buf)
    ))

(de insert-buffer-into-buffer (source destination)
  (let ((old-pos (=> destination position)))
    (=> destination insert-text (=> source contents))
    (=> destination set-mark-from-point)
    (=> destination set-position old-pos)
    ))

(de save-file (b)
  % Save the specified buffer on its associated file, ask for file if unknown.
  (let ((fn (=> b file-name)))
    (cond
     ((not (=> b modified?)) nil)
     (fn (write-buffer-to-file b (pathname-without-version fn)))
     (T (write-file b)))))

(de save-file-version (b)
  % Save the specified buffer on its associated file, ask for file if unknown.
  % The file is written to the current version number.
  (let ((fn (=> b file-name)))
    (cond
     ((not (=> b modified?)) nil)
     (fn (write-buffer-to-file b fn))
     (T (write-file b)))))

(de write-file (b)
  % Ask for filename, write out the buffer to the file.
  (let ((msg (bldmsg "Write Buffer %w to File: " (=> b name))))
    (write-buffer-to-file b (prompt-for-defaulted-filename msg b))))

(de write-buffer-to-file (b pn)
  % Write the specified buffer to a file.
  (write-prompt "")
  (let* ((file-name (namestring pn))
	 (s (attempt-to-open-output file-name))
	 )
    (if s
      (let ((fn (=> s file-name)))
	(write-prompt (bldmsg "Writing file: %w" fn))
	(=> b write-to-stream s)
	(=> s close)
	(write-prompt
	 (bldmsg "File written: %w (%d lines)" fn (=> b visible-size)))
	(=> b set-modified? NIL)
	(=> b set-file-name fn)
	)
      (nmode-error (bldmsg "Unable to write file: %w" file-name))
      )))

(de write-text-to-file (text pn)
  (let ((b (buffer-create-unselectable "FOO" Text-Mode)))
    (=> b insert-text text)
    (write-buffer-to-file b pn)
    ))

(de prepend-text-to-file (text pn)
  (let ((b (buffer-create-unselectable "FOO" Text-Mode)))
    (read-file-into-buffer b pn)
    (=> b move-to-buffer-start)
    (=> b insert-text text)
    (write-buffer-to-file b pn)
    ))

(de append-text-to-file (text pn)
  (let ((b (buffer-create-unselectable "FOO" Text-Mode)))
    (read-file-into-buffer b pn)
    (=> b move-to-buffer-end)
    (=> b insert-text text)
    (write-buffer-to-file b pn)
    ))

(de visit-file (b file-name)
  % If the specified file exists, read it into the specified buffer.
  % Otherwise, clear the buffer for a new file.
  % If the buffer contains precious data, offer to save it first.

  (if (=> b modified?)
    (let* ((fn (=> b file-name))
	   (msg (if fn (bldmsg "file %w" fn)
		  (bldmsg "buffer %w" (=> b name))))
	   )
      (if (nmode-yes-or-no? (bldmsg "Write out changes in %w?" msg))
	(save-file b)
	)))
  (let ((fn (actualize-file-name file-name)))
    (if fn
      (read-file-into-buffer b fn)
      (nmode-error (bldmsg "Unable to read or create file: %w" file-name))
      )))

(de find-file (file-name)
  % Select a buffer containing the specified file.  If the file exists in a
  % buffer already, then that buffer is selected.  Otherwise, a new buffer is
  % created and the file read into it (if the file exists).

  (find-file-in-window nmode-current-window file-name))

(de find-file-in-window (w file-name)
  % Attach a buffer to the specified window that contains the specified file.
  % If the file exists in a buffer already, then that buffer is used.
  % Otherwise, a new buffer is created and the file read into it (if the file
  % exists).

  (let ((b (find-file-in-buffer file-name nil)))
    (if b
      (window-select-buffer w b)
      % otherwise
      (nmode-error (bldmsg "Unable to read or create file: %w" file-name))
      )))

(de find-file-in-buffer (file-name existing-file-only?)
  % Return a buffer containing the specified file.  The buffer is not
  % selected.  If the file exists in a buffer already, then that buffer is
  % returned.  Otherwise, if the file exists and can be read, a new buffer is
  % created and the file read into it.  Otherwise, if EXISTING-FILE-ONLY? is
  % NIL and the file is potentially creatable, a new buffer is created and
  % returned.  Otherwise, NIL is returned.

  (setf file-name (actualize-file-name file-name))
  (if (and file-name (not (string-empty? file-name)))
    (or
     (find-file-in-existing-buffer file-name) % look for existing buffer
     (let ((s (attempt-to-open-input file-name)))
       (when (or s (not existing-file-only?)) % create a buffer
	 (let ((b (buffer-create-default
		   (buffer-make-unique-name
		    (filename-to-buffername file-name)))))
	   (=> b set-file-name file-name)
	   (buffer-set-mode b (pathname-default-mode file-name))
	   (if s
	     (read-stream-into-buffer b s)
	     (write-prompt "(New File)")
	     )
	   b
	   ))))))

(de find-file-in-existing-buffer (file-name)
  % Look for the specified file in an existing buffer.  If found, return
  % that buffer, otherwise return NIL.  The filename should be complete.

  (let ((pn (pathname file-name)))
    (for (in b nmode-selectable-buffers)
	 (do (if (pathnames-match pn (=> b file-name)) (exit b)))
	 (returns nil))
    ))

(de nmode-delete-file (fn)
  (let ((del-fn (file-delete fn)))
    (if del-fn
      (write-prompt (bldmsg "File deleted: %w" del-fn))
      (nmode-error (bldmsg "Unable to delete file: %w" fn))
      )
    del-fn
    ))

(de nmode-delete-and-expunge-file (fn)
  (let ((del-fn (file-delete-and-expunge fn)))
    (if del-fn
      (write-prompt (bldmsg "File deleted and expunged: %w" del-fn))
      (nmode-error (bldmsg "Unable to delete file: %w" fn))
      )
    del-fn
    ))

(de nmode-undelete-file (fn)
  (let ((del-fn (file-undelete fn)))
    (if del-fn
      (write-prompt (bldmsg "File undeleted: %w" del-fn))
      (nmode-error (bldmsg "Unable to undelete file: %w" fn))
      )
    del-fn
    ))

(de write-screen-photo (file-name)
  % Write the current screen to file.
  (let ((s (attempt-to-open-output file-name)))
    (cond (s
	   (nmode-refresh)
	   (=> nmode-screen write-to-stream s)
	   (=> s close)
	   (write-prompt (bldmsg "File written: %w" (=> s file-name)))
	   )
	  (t
	   (nmode-error (bldmsg "Unable to write file: %w" file-name))
	   ))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de actualize-file-name (file-name)
  % If the specified file exists, return its "true" (and complete) name.
  % Otherwise, return the "true" name of the file that would be created if one
  % were to do so.  (Unfortunately, we have no way to do this except by actually
  % creating the file and then deleting it!)  Return NIL if the file cannot be
  % read or created.

  (let ((s (attempt-to-open-input file-name)))
    (cond ((not s)
	   (setf s (attempt-to-open-output file-name))
	   (when s
	     (setf file-name (=> s file-name))
	     (=> s close)
	     (file-delete-and-expunge file-name)
	     file-name
	     )
	   )
	  (t
	   (setf file-name (=> s file-name))
	   (=> s close)
	   file-name
	   ))))

(de filename-to-buffername (pn)
  % Convert from a pathname to the "default" corresponding buffer name.
  (setf pn (pathname pn))
  (string-upcase (file-namestring (pathname-without-version pn)))
  )

(de pathnames-match (pn1 pn2)
  (setf pn1 (pathname pn1))
  (setf pn2 (pathname pn2))
  (and (equal (pathname-device pn1) (pathname-device pn2))
       (equal (pathname-directory pn1) (pathname-directory pn2))
       (equal (pathname-name pn1) (pathname-name pn2))
       (equal (pathname-type pn1) (pathname-type pn2))
       (or (null (pathname-version pn1))
	   (null (pathname-version pn2))
	   (equal (pathname-version pn1) (pathname-version pn2)))
       ))

(de pathname-without-version (pn)
  (setf pn (pathname pn))
  (make-pathname 'host (pathname-host pn)
		 'device (pathname-device pn)
		 'directory (pathname-directory pn)
		 'name (pathname-name pn)
		 'type (pathname-type pn)
		 ))
