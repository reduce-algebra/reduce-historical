%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Input-Stream.SL (TOPS-20 Version) - File Input Stream Objects
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        29 July 1982
%
% This package is 6.6 times faster than the standard unbuffered I/O.
% (Using message passing, it is only 1.7 times faster.)
%
% Note: this code will only run COMPILED.
%
% See TESTING code at the end of this file for examples of use.
% Be sure to include "(CompileTime (load objects))" at the beginning
% of any file that uses this package.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects jsys))

(defun open-input (file-name)
  (let ((s (make-instance 'input-stream)))
    (=> s open file-name)
    s))

%(CompileTime (setq *pgwd t))

(CompileTime (setq FILE-BUFFER-SIZE (* 5 512)))

(defflavor input-stream ((jfn NIL)	% TOPS-20 file number
			ptr		% "pointer" to next char in buffer
			count		% number of valid chars in buffer
			eof-flag	% T => this bufferfull is the last
			file-name	% full name of actual file
			buffer		% input buffer
			)
  ()
  (gettable-instance-variables file-name)
  )

% Note: The JSYS function can't be used for the 'SIN' JSYS because the function
% handles errors.  The 'SIN' JSYS will report an error on end-of-file if errors
% are being handled.

(CompileTime (put 'sin 'OpenCode '((jsys 42) (move (reg 1) (reg 3)))))
(CompileTime (put 'closf 'OpenCode '((jsys 18) (move (reg 1) (reg 1)))))

(defmethod (input-stream getc) ()

    % Return the next character from the file.  Line termination
    % is represented by a single NEWLINE (LF) character.

    % Note: returns NIL on end of file.

    (if (WLessP ptr count)
        (let ((ch (prog1
		    (igets buffer ptr)
		    (setf ptr (wplus2 ptr 1))
		    )))
	  % Ignore CR's
	  (if (WNEq ch (char CR)) ch (input-stream$getc self))
	  )
	(input-stream$fill-buffer-and-getc self)
	))

% The above function was coded to produce good compiled code
% using the current PSL compiler.  Here's the output.  Note
% that no stack variables are used.  The main path uses 16
% instructions.  There is room for improvement.

%               (*ENTRY INPUT-STREAM$GETC EXPR 1)
% G0002         (MOVE (REG 4) (REG 1))
%               (MOVE (REG T1) (INDEXED (REG 1) 6))
%               (CAMG (REG T1) (INDEXED (REG 1) 5))
%               (JRST G0004)
%               (MOVE (REG 2) (INDEXED (REG 1) 5))
%               (MOVE (REG 1) (INDEXED (REG 1) 4))
%               (AOS (REG 1))
%               (ADJBP (REG 2) "L0010")
%               (LDB (REG 1) (REG 2))
%               (MOVE (REG 3) (REG 1))
%               (MOVE (REG 1) (INDEXED (REG 4) 5))
%               (AOS (REG 1))
%               (MOVEM (REG 1) (INDEXED (REG 4) 5))
%               (MOVE (REG 1) (REG 3))
%               (CAIE (REG 1) 13)
%               (JRST G0001)
%               (MOVE (REG 1) (REG 4))
%               (JRST G0002)
% G0004         (JRST (ENTRY INPUT-STREAM$FILL-BUFFER-AND-GETC))
% G0001         (POPJ (REG ST) 0)
% L0010         (FULLWORD (FIELDPOINTER (INDEXED (REG 1) 0) 0 7))

(defmethod (input-stream fill-buffer-and-getc) ()

  % Implementation note: Removing all of this code from GETC improves the
  % quality of the compiled code for GETC.  In particular, the compiler is able
  % to keep SELF in a register, instead of saving it in a stack variable and
  % (excessively) reloading it every time it is needed.  Making this change
  % increased the performance of buffered input from 4X to 6.6X the standard
  % unbuffered input.

  (if eof-flag
      NIL
      (let ((n (sin jfn (jconv buffer) (WDifference 0 #.FILE-BUFFER-SIZE))))
        (if (not (WEQ n 0)) (setf eof-flag T))
        (setf count (WPlus2 #.FILE-BUFFER-SIZE n))
        (setf ptr 0)
        (input-stream$getc self))))

(defmethod (input-stream getc-image) ()

    % Return the next character from the file.  Do not perform
    % any translation.  In particular, return all <CR>s.
    % Returns NIL on end of file.

    (if (WLessP ptr count)
        (prog1
	 (igets buffer ptr)
	 (setf ptr (wplus2 ptr 1))
	 )
	(input-stream$fill-buffer-and-getc-image self)
	))

(defmethod (input-stream fill-buffer-and-getc-image) ()

  (if eof-flag
      NIL
      (let ((n (sin jfn (jconv buffer) (WDifference 0 #.FILE-BUFFER-SIZE))))
        (if (not (WEQ n 0)) (setf eof-flag T))
        (setf count (WPlus2 #.FILE-BUFFER-SIZE n))
        (setf ptr 0)
        (input-stream$getc-image self))))

(defmethod (input-stream empty?) ()
  (null (input-stream$peekc self)))

(defmethod (input-stream peekc) ()

    % Return the next character from the file, but don't advance
    % to the next character.  Returns NIL on end of file.

    (if (WLessP ptr count)
        (let ((ch (igets buffer ptr)))
	  % Ignore CR's
	  (if (WNEq ch (char CR))
	      ch
	      (setf ptr (wplus2 ptr 1))
	      (input-stream$peekc self))
	  )
	(input-stream$fill-buffer-and-peekc self)
	))

(defmethod (input-stream fill-buffer-and-peekc) ()

  (if eof-flag
      NIL
      (let ((n (sin jfn (jconv buffer) (WDifference 0 #.FILE-BUFFER-SIZE))))
        (if (not (WEQ n 0)) (setf eof-flag T))
        (setf count (WPlus2 #.FILE-BUFFER-SIZE n))
        (setf ptr 0)
        (input-stream$peekc self))))

(defmethod (input-stream open) (name-of-file)

  % Open the specified file for input via SELF.  If the file cannot
  % be opened, a Continuable Error is generated.

  (if jfn (input-stream$close self))
  (setf buffer (MkString #.FILE-BUFFER-SIZE (char space)))
  (setf ptr 0)
  (setf count 0)
  (setf eof-flag NIL)
  (setf jfn (Dec20Open name-of-file 
	         (int2sys 2#001000000000000001000000000000000000)
	         (int2sys 2#000111000000000000010000000000000000)
	         ))
  (if (= jfn 0) (setf jfn NIL))
  (if (null jfn)
   (=> self open
       (ContinuableError 0
		         (BldMsg "Unable to Open '%w' for Input." name-of-file)
		         name-of-file))
   (setf file-name (MkString 200 (char space)))
   (jsys1 file-name jfn #.(bits 2 5 8 11 14 35) 0 #.(get 'jsJFNS 'NewNam))
   (setf file-name (recopystringtonull file-name))
   ))

(defmethod (input-stream close) ()
  (if jfn (progn
	    (closf jfn)
	    (setf jfn NIL)
	    (setf buffer NIL)
	    (setf count 0)
	    (setf ptr 0)
	    (setf eof-flag T)
	    )))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTING CODE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CommentOutCode (progn

(de test-buffered-input (name-of-file)
  (setq s (open-input name-of-file))
  (while (setq ch (input-stream$getc s))
    (WriteChar ch)
    )
  (=> s close)
  (Prin2 "---EOF---")
  NIL
  )

(de time-buffered-input (name-of-file)
  (setq start-time (time))
  (setq s (open-input name-of-file))
  (while (setq ch (input-stream$getc s))
    )
  (=> s close)
  (- (time) start-time)
  )

(de time-buffered-input-1 (name-of-file)
  (setq start-time (time))
  (setq s (open-input name-of-file))
  (while (setq ch (=> s getc))
    )
  (=> s close)
  (- (time) start-time)
  )

(de time-standard-input (name-of-file)
  (setq start-time (time))
  (setq chan (open name-of-file 'INPUT))
  (while (not (= (setq ch (ChannelReadChar chan)) (char EOF)))
    )
  (close chan)
  (- (time) start-time)
  )

(de time-input (name-of-file)
  (list
    (time-buffered-input name-of-file)
    (time-buffered-input-1 name-of-file)
    (time-standard-input name-of-file)
    ))

)) % End CommentOutCode
