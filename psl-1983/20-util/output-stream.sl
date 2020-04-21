%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Output-Stream.SL (TOPS-20 Version) - File Output Stream Objects
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        29 July 1982
%
% This package is 6.7 times faster than the standard unbuffered I/O.
% (Using message passing, it is only 1.9 times faster.)
%
% Note: this code will only run COMPILED.
%
% See TESTING code at the end of this file for examples of use.
% Be sure to include "(CompileTime (load objects))" at the beginning
% of any file that uses this package.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load fast-int fast-vectors fast-strings))
(BothTimes (load objects jsys))

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

(defconst FILE-BUFFER-SIZE #.(* 5 512))

(defflavor output-stream ((jfn NIL)	% TOPS-20 file number
			  ptr		% "pointer" to next free slot in buffer
			  file-name	% full name of actual file
			  buffer	% output buffer
			  )
  ()
  (gettable-instance-variables file-name)
  )

(CompileTime (put 'SOUT 'OpenCode '((jsys 43) (move (reg 1) (reg 3)))))
(CompileTime (put 'CLOSF 'OpenCode '((jsys 18) (move (reg 1) (reg 1)))))

(defmethod (output-stream putc) (ch)

  % Append the character CH to the file.  Line termination is indicated by
  % writing a single NEWLINE (LF) character.

  % Implementation note:  It was determined by experiment that the PSL
  % compiler produces much better code if there are no function calls other
  % than tail-recursive ones.  That's why this function is written the way
  % it is.

  (if (= ch #\LF)
    (=> self put-newline)
    % Otherwise:
    (string-store buffer ptr ch)
    (if (>= (setf ptr (+ ptr 1)) (const FILE-BUFFER-SIZE))
      (=> self flush))
    ))

(defmethod (output-stream put-newline) ()

  % Output a line terminator.

  (string-store buffer ptr #\CR)
  (if (>= (setf ptr (+ ptr 1)) (const FILE-BUFFER-SIZE))
      (=> self flush))
  (string-store buffer ptr #\LF)
  (if (>= (setf ptr (+ ptr 1)) (const FILE-BUFFER-SIZE))
      (=> self flush))
  )

(defmethod (output-stream putc-image) (ch)

  % Append the character CH to the file.  No translation of LF character.

  (string-store buffer ptr ch)
  (if (>= (setf ptr (+ ptr 1)) (const FILE-BUFFER-SIZE))
    (=> self flush))
  )

(defmethod (output-stream puts) (str)

  % Write string to output stream (highly optimized!)

  (let ((i 0)
	(high (string-upper-bound str))
	)
    (while (<= i high)
      (string-store buffer ptr (string-fetch str i))
      (if (>= (setf ptr (+ ptr 1)) (const FILE-BUFFER-SIZE))
        (=> self flush))
      (setf i (+ i 1))
      )))

(defmethod (output-stream putl) (str)

  % Write string followed by line terminator to output stream.

  (=> self puts str)
  (=> self put-newline)
  )

(defmethod (output-stream open) (name-of-file)

  % Open the specified file for output via SELF.  If the file cannot
  % be opened, a Continuable Error is generated.

  (if jfn (=> self close))
  (setf jfn (Dec20Open name-of-file 
	         (int2sys 2#100000000000000001000000000000000000)
	         (int2sys 2#000111000000000000001000000000000000)
	         ))
  (if (= jfn 0) (setf jfn NIL))
  (if (null JFN)
    (=> self open
      (ContinuableError 0
			(BldMsg "Unable to Open '%w' for Output" name-of-file)
			name-of-file))
    (=> self &fixup)
    ))

(defmethod (output-stream open-append) (name-of-file)

  % Open the specified file for append output via SELF.  If the file cannot
  % be opened, a Continuable Error is generated.

  (if jfn (=> self close))
  (setf jfn (Dec20Open name-of-file 
	         (int2sys 2#000000000000000001000000000000000000)
	         (int2sys 2#000111000000000000000010000000000000)
	         ))
  (if (= jfn 0) (setf jfn NIL))
  (if (null JFN)
    (=> self open-append
      (ContinuableError 0
			(BldMsg "Unable to Open '%w' for Append" name-of-file)
			name-of-file))
    (=> self &fixup)
    ))

(defmethod (output-stream attach-to-jfn) (new-jfn)

  % Attach the output-stream to the specified JFN.

  (if jfn (=> self close))
  (setf jfn new-jfn)
  (=> self &fixup)
  )

(defmethod (output-stream &fixup) ()
  % Internal method for initializing instance variables after setting JFN.

  (setf buffer (make-string (const FILE-BUFFER-SIZE) #\space))
  % It is necessary to clear out the low-order bit, lest some programs
  % think we are writing "line numbers" (what a crock!).
  (for (from i 0 (- (/ (const FILE-BUFFER-SIZE) 5) 1))
       (do (vector-store buffer i 0)))
  (setf ptr 0)
  (setf file-name (jfn-truename jfn))
  )

(defmethod (output-stream close) ()
  (when jfn
    (=> self flush)
    (CLOSF jfn)
    (setf jfn NIL)
    (setf buffer NIL)
    ))

(defmethod (output-stream flush) ()
  (when (> ptr 0)
    (SOUT jfn (jconv buffer) (- ptr))
    (setf ptr 0)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTING CODE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime
 (setf time-output-test-string "This is a line of text for testing."))

(CommentOutCode (progn

(de time-buffered-output (n-lines)
  % This is the FAST way to do buffered output.

  (setf start-time (time))
  (setf s (open-output "test.output"))
  (for (from i 1 n-lines 1)
       (do (for (in ch '#.(String2List time-output-test-string))
		(do (output-stream$putc s ch))
		)
	   (output-stream$put-newline s)
	   ))
  (=> s close)
  (- (time) start-time)
  )

(de time-buffered-output-1 (n-lines)
  % This is the SLOW (but GENERAL) way to do buffered output.

  (setf start-time (time))
  (setf s (open-output "test.output"))
  (for (from i 1 n-lines 1)
       (do (for (in ch '#.(String2List time-output-test-string))
		(do (=> s putc ch))
		)
	   (=> s put-newline)
	   ))
  (=> s close)
  (- (time) start-time)
  )

(de time-standard-output (n-lines)
  (setf start-time (time))
  (setf chan (open "test.output" 'OUTPUT))
  (for (from i 1 n-lines 1)
       (do (for (in ch '#.(String2List time-output-test-string))
		(do (ChannelWriteChar chan ch))
		)
	   (ChannelWriteChar chan #\LF)
	   ))
  (close chan)
  (- (time) start-time)
  )

(de time-output (n-lines)
  (list
    (time-buffered-output-string n-lines)
    (time-buffered-output n-lines)
    (time-buffered-output-1 n-lines)
    (time-standard-output n-lines)
    ))

(de time-buffered-output-string (n-lines)
  % This is the FAST way to do buffered output from strings.

  (setf start-time (time))
  (setf s (open-output "test.output"))
  (for (from i 1 n-lines 1)
       (do (output-stream$putl s #.time-output-test-string))
       )
  (=> s close)
  (- (time) start-time)
  )

)) % End CommentOutCode
