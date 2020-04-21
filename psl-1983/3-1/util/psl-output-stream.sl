%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PSL-Output-Stream.SL - File Output Stream Objects (Portable PSL Version)
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        10 December 1982
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load fast-int fast-strings))
(BothTimes (load objects))

(de attempt-to-open-output (file-name)
  (let ((p (ErrorSet (list 'open-output file-name) NIL NIL)))
    (and (PairP p) (car p))
    ))

(de attempt-to-open-append (file-name)
  (let ((p (ErrorSet (list 'open-append file-name) NIL NIL)))
    (and (PairP p) (car p))
    ))

(de open-output (file-name)
  (let ((s (make-instance 'output-stream)))
    (=> s open file-name)
    s))

(de open-append (file-name)
  (let ((s (make-instance 'output-stream)))
    (=> s open-append file-name)
    s))

(defflavor output-stream ((chn NIL)	% PSL "channel"
			  file-name	% file name given to open
			  )
  ()
  (gettable-instance-variables file-name)
  )

(defmethod (output-stream putc) (ch)

  % Append the character CH to the file.  Line termination is indicated by
  % writing a single NEWLINE (LF) character.

  (ChannelWriteChar chn ch)
  )

(defmethod (output-stream put-newline) ()
  % Output a line terminator.
  (ChannelWriteChar chn #\EOL)
  )

(defmethod (output-stream putc-image) (ch)
  (ChannelWriteChar chn ch)
  )

(defmethod (output-stream puts) (str)
  (for (from i 0 (string-upper-bound str))
       (do (=> self putc (string-fetch str i)))
       ))

(defmethod (output-stream putl) (str)
  % Write string followed by line terminator to output stream.
  (=> self puts str)
  (=> self put-newline)
  )

(defmethod (output-stream open) (name-of-file)

  % Open the specified file for output via SELF.  If the file cannot
  % be opened, a Continuable Error is generated.

  (if chn (=> self close))
  (setf chn (open name-of-file 'output))
  (setf file-name (copystring name-of-file))
  )

(defmethod (output-stream open-append) (name-of-file)
  (=> self open name-of-file))

(defmethod (output-stream close) ()
  (when chn
    (close chn)
    (setf chn NIL)
    ))

(defmethod (output-stream flush) ()
  )
