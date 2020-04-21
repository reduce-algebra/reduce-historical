%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% New-FileIO.SL
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        30 July 1982
%
% Revised File I/O for EMODE.
%
% The combination of buffered file input and string-oriented reading of the
% file into the buffer makes for a 5X improvement in the speed of reading a
% nontrivial file (or more, since it no longer does unnecessary consing).
% In addition, the ^Z EOF bug has been fixed.
%
% A similar speedup has been made to file output.  In addition, an extra
% blank line is no longer written at the end of each file.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects))
(load input-stream output-stream fast-vector)

(de readfile (file-name)
  (write-prompt "")
  (let* ((p (ErrorSet (List 'open-input file-name) NIL NIL))
	 )
    (if (PairP p)
	(read-file-into-buffer (car p))
	(write-prompt (BldMsg "Unable to read file: %w" file-name))
	(Ding)
	)))

(de read-file-into-buffer (s)
  (write-prompt (BldMsg "Reading file: %w" (=> s file-name)))
  (setf CurrentBufferText (MkVect 1))
  (setf CurrentBufferSize 1)
  (append-file-to-buffer s)
  (=> s close)
  (write-prompt (BldMsg "File read: %w (%d lines)"
			(=> s file-name)
			(current-buffer-visible-size)))
  )

(de append-file-to-buffer (s)
  (prog (line-buffer line-size ch)
    (setf line-buffer (MkString 200 0))
    (while T
      (setf line-size 0)
      (setf ch (input-stream$getc s))
      (while (not (or (null ch) (WEq ch (char EOL))))
	(if (WGreaterP line-size (ISizeS line-buffer))
	  (setf line-buffer (concat line-buffer (Mkstring 200 0)))
	  )
	(iputs line-buffer line-size ch)
	(setf line-size (WPlus2 line-size 1))
	(setf ch (input-stream$getc s))
	)
      (if (not (and (null ch) (WEq line-size 0)))
	(append-line-to-buffer (sub line-buffer 0 (WDifference line-size 1)))
	)
      (cond ((null ch)
	     (if (> line-size 0)
		 (setf CurrentBufferSize (- CurrentBufferSize 1))
		 )
	     (exit)))
      )
    (GetLine (setf CurrentLineIndex 0))
    ))

(de append-line-to-buffer (contents)
  % Note: GETLINE must be done after a sequence of appends
  (let ((indx CurrentBufferSize))
    (setf CurrentBufferSize (+ CurrentBufferSize 1))
    (if (> CurrentBufferSize (size CurrentBufferText))
      (setf CurrentBufferText (concat CurrentBufferText (MkVect 63))))
    (SetBufferText (- indx 1) contents)
    (SetBufferText indx "")
    ))

(de WriteFile (file-name)
  % Write whole of current EMODE buffer to file.
  (write-prompt "")
  (let* ((p (ErrorSet (list 'open-output file-name) NIL NIL))
	 )
    (if (PairP p)
      (let ((s (car p)))
	   (write-prompt (BldMsg "Writing file: %w" (=> s file-name)))
	   (write-buffer-to-stream s)
	   (=> s close)
	   (write-prompt (BldMsg "File written: %w (%d lines)"
				 (=> s file-name)
				 (current-buffer-visible-size)))
	   )
      (write-prompt (BldMsg "Unable to write file: %w" file-name))
      (Ding)
      )))

(de write-buffer-to-stream (s)
  (PutLine CurrentLineIndex)
  (for (from i 0 (- CurrentBufferSize 2) 1)
       (do (output-stream$putl s (GetBufferText i)))
       )
  (output-stream$puts s (GetBufferText (- CurrentBufferSize 1)))
  )
