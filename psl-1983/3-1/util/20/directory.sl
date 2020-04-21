%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Directory.SL - File Directory Primitives (TOPS-20 Version)
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        13 July 1982
% Revised:     4 March 1983
%
% 4-Mar-83 Alan Snyder
%  Revised to accept FOO.DIRECTORY as the name of a subdirectory.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load common jsys pathnames file-primitives))

(de find-matching-files (filename include-deleted-files)

  % Return a list describing all files that match the specified filename.  The
  % filename may specify a directory and/or may contain wildcard characters.
  % Each element of the returned list corresponds to one matching file.  The
  % format of each list element is:

  % (file-name			full file name string 
  %  deleted-flag		T or NIL
  %  file-size			integer count of pages in file
  %  write-date			integer representing date/time of last write
  %  read-date			integer representing date/time of last read
  %  )

  (setf filename (fixup-directory-name filename))
  (let (jfn-word jfn file-name deleted-flag file-size write-date read-date)
    (cond
      ((and (stringp filename)
	    (setf jfn-word (attempt-to-get-jfn
			    filename
			    (if include-deleted-files
				#.(bits 2 8 11 13 17)
				#.(bits 2 11 13 17)
				)
			    )))
	(for*
	   (while (>= jfn-word 0))
	   (do (setf jfn (lowhalfword jfn-word))
	       (setf file-name (MkString 100 (char space)))
	       (jsys1 file-name jfn
		  #.(bits 2 5 8 11 14 35) 0 (const jsJFNS))
	       (setf file-name (recopystringtonull file-name))
	       (setf deleted-flag (jfn-deleted? jfn))
	       (setf file-size (jfn-page-count jfn))
	       (setf write-date (jfn-write-date jfn))
	       (setf read-date (jfn-read-date jfn))
	       )
	   (collect (list
			file-name
			deleted-flag
			file-size
			write-date
			read-date
			))
	   (do (if (FixP (ErrorSet
		(list 'jsys1 jfn-word 0 0 0 (const jsGNJFN))
		NIL NIL)) (setf jfn-word -1)))
	   ))
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de fixup-directory-name (pn)

  % Replace all missing Name, Type, and Version components of the specified
  % filename with "*".  Recognize FOO.DIRECTORY as the name of a subdirectory.

  (let ((wild-name (make-pathname 'name 'wild)))
    (setf pn (pathname pn))
    (when (and (equal (pathname-host pn) "LOCAL")
	       (stringp (pathname-type pn))
	       (string-equal (pathname-type pn) "DIRECTORY")
	       (stringp (pathname-name pn))
	       (stringp (pathname-directory pn))
	       )
      (setf pn (make-pathname
		'host (pathname-host pn)
		'device (pathname-device pn)
		'directory (string-concat
			    (pathname-directory pn) "." (pathname-name pn))
		)))
    (namestring (merge-pathname-defaults pn wild-name 'wild 'wild))
    ))
