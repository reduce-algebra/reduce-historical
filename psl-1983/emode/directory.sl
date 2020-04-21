%
% Directory.SL - File Directory and related file primitives
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        13 July 1982
%
% *** THIS FILE IS TOPS-20 SPECIFIC ***
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load common jsys useful))

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
      ((and (stringp filename) (listp (setf jfn-word (ErrorSet
		 (list 'jsys1
		       (if include-deleted-files
			   #.(bits 2 8 11 13 17)
			   #.(bits 2 11 13 17))
		       filename 0 0 (const jsGTJFN)) nil nil))))
	(setf jfn-word (first jfn-word))
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de file-deleted-status (file-name)
  % Return either: EXISTS, DELETED, NIL
  (let ((jfn (ErrorSet (list 'jsys1 #.(bits 2 8 17)
			     file-name 0 0 (const jsGTJFN)) nil nil)
	))
      (cond
	((listp jfn)
	   (setf jfn (car jfn))
	   (prog1 (if (jfn-deleted? jfn) 'deleted 'exists)
                  (jsys0 jfn 0 0 0 (const jsRLJFN))
		  )
	   )
        )))

(de file-delete (file-name)
  (let ((jfn (ErrorSet (list 'jsys1 #.(bits 2 17)
			     file-name 0 0 (const jsGTJFN)) nil nil)
	))
      (cond
	((listp jfn)
	   (setf jfn (car jfn))
	   (jsys0 jfn 0 0 0 (const jsDELF))
	   )
        )))

(de file-undelete (file-name)
  (let ((jfn (ErrorSet (list 'jsys1 #.(bits 2 8 17)
			     file-name 0 0 (const jsGTJFN)) nil nil)
	))
      (cond
	((listp jfn)
	   (setf jfn (car jfn))
	   (jsys0 (xword 1 jfn) #.(bits 3) 0 0 (const jsCHFDB))
           (jsys0 jfn 0 0 0 (const jsRLJFN))
	   )
        )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% JFN Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de jfn-deleted? (jfn)
  (not (= (LAnd (Jsys4 jfn #.(xword 1 1) 4 0 (const jsGTFDB))
		(bits 3)) 0)))

(de jfn-write-date (jfn)
  (Jsys4 jfn #.(xword 1 8#14) 4 0 (const jsGTFDB)))

(de jfn-read-date (jfn)
  (Jsys4 jfn #.(xword 1 8#15) 4 0 (const jsGTFDB)))

(de jfn-byte-count (jfn)
  (Jsys4 jfn #.(xword 1 8#12) 4 0 (const jsGTFDB)))

(de jfn-page-count (jfn)
  (lowhalfword (Jsys4 jfn #.(xword 1 8#11) 4 0 (const jsGTFDB))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de file-date-to-string (fdate)

  % Convert a file date as returned by find-matching-files to a meaningful
  % string.  Note that 0 is converted to the string "Never".  All returned
  % strings are 18 characters long, right justified.

  (if (= fdate 0)
    "             Never"
    (let ((buf (MkString 30 (char space))))
	(Jsys0 buf fdate 0 0 (const jsODTIM))
	(recopystringtonull buf))))    

(de fixup-directory-name (name)

  % If NAME is an unadorned directory or device name, append wild cards to it
  % so that it will match all files in the specified directory or directories.

  (let ((n (add1 (size name))))
    (cond ((or (= n 0)
	       (= (indx name (- n 1)) (char :))
	       (= (indx name (- n 1)) (char >))
	       )
	   (concat name "*.*.*"))
	  (t name))))

(de fixup-file-name (name)

  % Make the specified file name nice to print.
  % Remove any control characters (especially ^V).

  (for (in ch (String2List name))
       (with the-list)
       (when (GraphicP ch))
       (collect ch the-list)
       (returns (List2String the-list))
       ))

(de trim-filename-to-prefix (s)
  % Remove trailing characters until the string ends with
  % a device or directory prefix.

  (for* (from i (size s) 0 -1)
        (for ch (indx s i) (indx s i))
        (until (or (= ch (char !:)) (= ch (char !>))))
        (returns (sub s 0 i))
        ))
