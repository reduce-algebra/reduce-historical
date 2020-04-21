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

(BothTimes (load objects jsys))

(defun open-output (file-name)
  (let ((s (make-instance 'output-stream)))
    (=> s open file-name)
    s))

(defun open-append (file-name)
  (let ((s (make-instance 'output-stream)))
    (=> s open-append file-name)
    s))

%(CompileTime (setq *pgwd t))

(CompileTime (setq FILE-BUFFER-SIZE (* 5 512)))

(defflavor output-stream ((jfn NIL)	% TOPS-20 file number
			  ptr		% "pointer" to next free slot in buffer
			  file-name	% full name of actual file
			  buffer	% output buffer
			  )
  ()
  (gettable-instance-variables file-name)
  )

(CompileTime (put 'sout 'OpenCode '((jsys 43) (move (reg 1) (reg 3)))))
(CompileTime (put 'closf 'OpenCode '((jsys 18) (move (reg 1) (reg 1)))))

(defmethod (output-stream putc) (ch)

    % Append the character CH to the file.  Line termination
    % is indicated by writing a single NEWLINE (LF) character.

  (if (WEq ch (char lf))
    (output-stream$put-newline self)
    (iputs buffer ptr ch)
    (if (WGEQ (setf ptr (wplus2 ptr 1)) #.FILE-BUFFER-SIZE)
        (output-stream$flush self))
    ))

% The above function was coded to produce good compiled code
% using the current PSL compiler.  Here's the output.  Note
% that no stack variables are used.  The main path uses 16
% instructions.

%                (*ENTRY OUTPUT-STREAM$PUTC EXPR 2)
%                (MOVE (REG 4) (REG 1))
%                (CAIE (REG 2) 10)
%                (JRST G0004)
%                (JRST (ENTRY OUTPUT-STREAM$PUT-NEWLINE))
% G0004          (MOVE (REG 3) (REG 2))
%                (MOVE (REG 2) (INDEXED (REG 1) 5))
%                (MOVE (REG 1) (INDEXED (REG 1) 4))
%                (AOS (REG 1))
%                (ADJBP (REG 2) "L0008")
%                (DPB (REG 3) (REG 2))
%                (MOVE (REG 1) (INDEXED (REG 4) 5))
%                (AOS (REG 1))
%                (MOVEM (REG 1) (INDEXED (REG 4) 5))
%                (CAIGE (REG 1) 2560)
%                (JRST G0007)
%                (MOVE (REG 1) (REG 4))
%                (JRST (ENTRY OUTPUT-STREAM$FLUSH))
% G0007          (MOVE (REG 1) (REG NIL))
%                (POPJ (REG ST) 0)
% L0008          (FULLWORD (FIELDPOINTER (INDEXED (REG 1) 0) 0 7))

(defmethod (output-stream put-newline) ()

  % Output a line terminator.

  (iputs buffer ptr (char cr))
  (if (WGEQ (setf ptr (wplus2 ptr 1)) #.FILE-BUFFER-SIZE)
      (output-stream$flush self))
  (iputs buffer ptr (char lf))
  (if (WGEQ (setf ptr (wplus2 ptr 1)) #.FILE-BUFFER-SIZE)
      (output-stream$flush self))
  )

(defmethod (output-stream puts) (str)

  % Write string to output stream (highly optimized!)

  (let ((i 0)
	(high (isizes str))
	)
    (while (WLEQ i high)
      (iputs buffer ptr (igets str i))
      (if (WGEQ (setf ptr (wplus2 ptr 1)) #.FILE-BUFFER-SIZE)
         (output-stream$flush self))
      (setq i (WPlus2 i 1))
      )))

(defmethod (output-stream putl) (str)

  % Write string followed by line terminator to output stream.

  (output-stream$puts self str)
  (output-stream$put-newline self)
  )

(defmethod (output-stream open) (name-of-file)

  % Open the specified file for output via SELF.  If the file cannot
  % be opened, a Continuable Error is generated.

  (if jfn (output-stream$close self))
  (setf buffer (MkString #.FILE-BUFFER-SIZE (char space)))
  (setf ptr 0)
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
    (setf file-name (MkString 200 (char space)))
    (jsys1 file-name jfn #.(bits 2 5 8 11 14 35) 0 #.(get 'jsJFNS 'NewNam))
    (setf file-name (recopystringtonull file-name))
    ))

(defmethod (output-stream open-append) (name-of-file)

  % Open the specified file for append output via SELF.  If the file cannot
  % be opened, a Continuable Error is generated.

  (if jfn (output-stream$close self))
  (setf buffer (MkString #.FILE-BUFFER-SIZE (char space)))
  (setf ptr 0)
  (setf jfn (Dec20Open name-of-file 
	         (int2sys 2#000000000000000001000000000000000000)
	         (int2sys 2#000111000000000000000010000000000000)
	         ))
  (if (= jfn 0) (setf jfn NIL))
  (if (null JFN)
    (=> self open
      (ContinuableError 0
			(BldMsg "Unable to Open '%w' for Append" name-of-file)
			name-of-file))
    (setf file-name (MkString 200 (char space)))
    (jsys1 file-name jfn #.(bits 2 5 8 11 14 35) 0 #.(get 'jsJFNS 'NewNam))
    (setf file-name (recopystringtonull file-name))
    ))

(defmethod (output-stream close) ()
  (if jfn (progn
	    (output-stream$flush self)
	    (closf jfn)
	    (setf jfn NIL)
	    (setf buffer NIL)
	    )))

(defmethod (output-stream flush) ()
  (if (WGreaterP ptr 0)
    (progn
      (sout jfn (jconv buffer) (WDifference 0 ptr))
      (setf ptr 0)
      ))
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTING CODE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime
 (setq time-output-test-string "This is a line of text for testing."))

(CommentOutCode (progn

(de time-buffered-output (n-lines)
  % This is the FAST way to do buffered output.

  (setq start-time (time))
  (setq s (open-output "test.output"))
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

  (setq start-time (time))
  (setq s (open-output "test.output"))
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
  (setq start-time (time))
  (setq chan (open "test.output" 'OUTPUT))
  (for (from i 1 n-lines 1)
       (do (for (in ch '#.(String2List time-output-test-string))
		(do (ChannelWriteChar chan ch))
		)
	   (ChannelWriteChar chan (char lf))
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

  (setq start-time (time))
  (setq s (open-output "test.output"))
  (for (from i 1 n-lines 1)
       (do (output-stream$putl s #.time-output-test-string))
       )
  (=> s close)
  (- (time) start-time)
  )

)) % End CommentOutCode
