%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File-Primitives - File System primitive functions
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        16 September 1982
% Revised:     22 November 1982
%
% *** THIS FILE IS TOPS-20 SPECIFIC ***
%
% This file contains the TOPS-20 implementation of a set of "common"
% file system primitives.
%
% 22-Nov-82 Alan Snyder
%   Added error handling.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load common))
(CompileTime (load jsys))
(load file-support)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de file-deleted-status (file-name)

  % This function will return T if the specified file exists and is not
  % marked as "deleted"; it will return 'DELETED if the file exists and
  % is marked as "deleted"; it will return NIL otherwise.  (On a system
  % that does not support "deleted" files, this function will return
  % either T or NIL.)

  (let ((jfn (attempt-to-get-jfn file-name #.(bits 2 8 17))))
    (when jfn
      (unwind-protect
       (let ((result (errset (jfn-deleted? jfn) nil)))
	 (if (pairp result)
	   (if (car result) 'DELETED T)
	   ))
       (jfn-release jfn)
       ))))

(de file-delete (file-name)

  % This function attempts to delete the specified file.  (This action may
  % be undone using the FILE-UNDELETE function, if the system supports it.)
  % If the attempt fails, NIL is returned (no error is reported).
  % Otherwise, a string is returned which is the true name of the file
  % that was deleted (as best as can be determined).

  (let ((jfn (attempt-to-get-jfn file-name #.(bits 2 17))))
    (when jfn
      (let ((fn (jfn-truename jfn)))
	(if (pairp (errset (jfn-delete jfn) nil)) fn)
	))))

(de file-delete-and-expunge (file-name)

  % This function attempts to delete the specified file and reclaim its
  % storage.  (On systems that do not support UNDELETE, this function is the
  % same as FILE-DELETE.)
  % If the attempt fails, NIL is returned (no error is reported).
  % Otherwise, a string is returned which is the true name of the file
  % that was deleted (as best as can be determined).

  (let ((jfn (attempt-to-get-jfn file-name #.(bits 2 17))))
    (when jfn
      (let ((fn (jfn-truename jfn)))
	(if (pairp (errset (jfn-delete-and-expunge jfn) nil)) fn)
	))))

(de file-undelete (file-name)

  % This function attempts to undelete the specified file.
  % If the attempt fails, NIL is returned (no error is reported).
  % Otherwise, a string is returned which is the true name of the file
  % that was undeleted (as best as can be determined).
  % (On systems that do not support UNDELETE, this function always returns NIL.)

  (let ((jfn (attempt-to-get-jfn file-name #.(bits 2 8 17))))
    (when jfn
      (unwind-protect
       (let ((fn (jfn-truename jfn)))
	 (if (pairp (errset (jfn-undelete jfn) nil)) fn)
	 )
       (jfn-release jfn)
       ))))

(de file-read-date (file-name)

  % This function returns an integer representing the date and time at
  % which the specified file was last read.  It returns NIL if it is
  % unable to obtain that information.

  (let ((jfn (attempt-to-get-jfn file-name #.(bits 2 8 17))))
    (when jfn
      (unwind-protect
       (let ((result (errset (jfn-read-date jfn) nil)))
	 (if (pairp result) (car result))
	 )
       (jfn-release jfn)
       ))))

(de file-write-date (file-name)

  % This function returns an integer representing the date and time at
  % which the specified file was last written.  It returns NIL if it is
  % unable to obtain that information.

  (let ((jfn (attempt-to-get-jfn file-name #.(bits 2 8 17))))
    (when jfn
      (unwind-protect
       (let ((result (errset (jfn-write-date jfn) nil)))
	 (if (pairp result) (car result))
	 )
       (jfn-release jfn)
       ))))

(de file-byte-count (file-name)

  % This function returns an integer representing the number of bytes
  % in the specified file (without necessarily converting CRLF's into
  % LFs).  It returns NIL if it is unable to obtain that information.

  (let ((jfn (attempt-to-get-jfn file-name #.(bits 2 8 17))))
    (when jfn
      (unwind-protect
       (let ((result (errset (jfn-byte-count jfn) nil)))
	 (if (pairp result) (car result))
	 )
       (jfn-release jfn)
       ))))

(de file-page-count (file-name)

  % This function returns an integer representing the number of "pages"
  % in the specified file.  (The notion of a "page" is system-dependent.)
  % It returns NIL if it is unable to obtain that information.

  (let ((jfn (attempt-to-get-jfn file-name #.(bits 2 8 17))))
    (when jfn
      (unwind-protect
       (let ((result (errset (jfn-page-count jfn) nil)))
	 (if (pairp result) (car result))
	 )
       (jfn-release jfn)
       ))))

(de file-original-author (file-name)

  % This function returns the name of the user who created the specified
  % file.  It returns NIL if it is unable to obtain that information.

  (let ((jfn (attempt-to-get-jfn file-name #.(bits 2 8 17))))
    (when jfn
      (unwind-protect
       (let ((result (errset (jfn-original-author jfn) nil)))
	 (if (pairp result) (car result))
	 )
       (jfn-release jfn)
       ))))

(de file-author (file-name)

  % This function returns the name of the user who last modified the specified
  % file.  It returns NIL if it is unable to obtain that information.

  (let ((jfn (attempt-to-get-jfn file-name #.(bits 2 8 17))))
    (when jfn
      (unwind-protect
       (let ((result (errset (jfn-author jfn) nil)))
	 (if (pairp result) (car result))
	 )
       (jfn-release jfn)
       ))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de file-date-to-string (fdate)

  % Convert a file date as returned by FILE-READ-DATE and FILE-WRITE-DATE to
  % a meaningful string.  Note that 0 is converted to the string "Never".

  (if (or (not (integerp fdate)) (= fdate 0))
    "Never"
    (let ((buf (make-string 30 0)))
      (Jsys0 buf fdate 0 0 (const jsODTIM))
      (recopystringtonull buf))))

(de fixup-file-name (name)

  % Make the specified file name nice to print, e.g. by removing escape
  % prefix characters.  In this case, simply remove all control characters
  % (^V is the TOPS-20 escape prefix character).

  (for (in ch (String2List name))
       (with the-list)
       (when (GraphicP ch))
       (collect ch the-list)
       (returns (List2String the-list))
       ))

(de trim-filename-to-prefix (s)
  % Remove trailing characters until the string ends with
  % a device or directory prefix.  (Used to determine a
  % "meaningful" common prefix of a collection of file names.)

  (for (from i (size s) 0 -1)
       (until (let ((ch (indx s i)))
		(or (= ch #\:) (= ch #\>))))
       (returns (substring s 0 (+ i 1)))
       ))
