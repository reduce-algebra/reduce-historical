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
% Summary of public functions:
%
% (setf s (open-input "file name")) % generates error on failure
% (setf s (attempt-to-open-input "file name")) % returns NIL on failure
% (setf ch (=> s getc)) % read character (map CRLF to LF)
% (setf ch (=> s getc-image)) % read character (don't map CRLF to LF)
% (setf ch (=> s peekc)) % peek at next character
% (setf ch (=> s peekc-image)) % peek at next character (don't map CRLF to LF)
% (setf str (=> s getl)) % Read a line; return string without terminating LF.
% (=> s empty?) % Are there no more characters?
% (=> s close) % Close the file.
% (setf fn (=> s file-name)) % Return "true" name of file.
% (setf date (=> s read-date)) % Return date that file was last read.
% (setf date (=> s write-date)) % Return date that file was last written.
% (=> s delete-file) % Delete the associated file.
% (=> s undelete-file) % Undelete the associated file.
% (=> s delete-and-expunge) % Delete and expunge the associated file.
% (setf name (=> s author)) % Return the name of the file's author.
% (setf name (=> s original-author)) % Return the original author's name.
% (setf count (=> s file-length)) % Return the byte count of the file.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Changes:
%
% 9/29/82 Alan Snyder
%   Changed GETC to return stray CRs.
%   Now uses (=> self ...) form (produces same object code).
%   Added operations PEEKC-IMAGE, GETL, TELL-POSITION, SEEK-POSITION
%    (written by Nancy Kendzierski).
%
% 11/22/82 Alan Snyder
%   Changed SEEK-POSITION to work with large byte pointers (> 256K).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load fast-int fast-strings))
(BothTimes (load objects jsys))
(load directory file-support)

(de attempt-to-open-input (file-name)
  (let ((p (ErrorSet (list 'open-input file-name) NIL NIL)))
    (and (PairP p) (car p))
    ))

(de open-input (file-name)
  (let ((s (make-instance 'input-stream)))
    (=> s open file-name)
    s))

(DefConst FILE-BUFFER-SIZE #.(* 5 512))

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

% Note: The JSYS function can't be used for the 'SIN' JSYS because the JSYS
% function handles errors.  The 'SIN' JSYS will report an error on end-of-file
% if errors are being handled.  We don't want that to happen!

(CompileTime (progn
  (put 'SIN 'OpenCode '((jsys 8#52) (move (reg 1) (reg 3))))
  (put 'BIN 'OpenCode '((jsys 8#50) (move (reg 1) (reg 2))))
  (put 'CLOSF 'OpenCode '((jsys 8#22) (move (reg 1) (reg 1))))
  (put 'RFPTR 'OpenCode '((jsys 8#43) (jfcl) (move (reg 1) (reg 2))))
  (put 'SFPTR 'OpenCode '((jsys 8#27) (jfcl) (move (reg 1) (reg 1))))
  ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (input-stream getc) ()

  % Return the next character from the file.  Line termination is represented
  % by a single NEWLINE (LF) character.  Returns NIL on end of file.

  % Implementation note:  It was determined by experiment that the PSL
  % compiler produces much better code if there are no function calls other
  % than tail-recursive ones.  That's why this function is written the way
  % it is.

    (if (< ptr count)
        (let ((ch (prog1
		    (string-fetch buffer ptr)
		    (setf ptr (+ ptr 1))
		    )))
	  % Ignore CR followed by LF
	  (if (= ch #\CR)
	    (=> self &getc-after-CR)
	    ch
	    ))
	(=> self &fill-buffer-and-getc)
	))

(defmethod (input-stream &getc-after-CR) () % Internal method.
  % We have just read a CR from the buffer.  If the next character
  % is a LF, then we should ignore the CR and return the LF.
  % Otherwise, we should return the CR.

  (if (= (=> self peekc-image) #\LF)
      (=> self getc-image)
      #\CR
      ))

(defmethod (input-stream &fill-buffer-and-getc) () % Internal method.
  (and (=> self &fill-buffer) (=> self getc)))

(defmethod (input-stream getc-image) ()

    % Return the next character from the file.  Do not perform any translation.
    % In particular, return all <CR>s.  Returns NIL on end of file.

    (if (< ptr count)
        (prog1
	 (string-fetch buffer ptr)
	 (setf ptr (+ ptr 1))
	 )
	(=> self &fill-buffer-and-getc-image)
	))

(defmethod (input-stream &fill-buffer-and-getc-image) () % Internal method.
  (and (=> self &fill-buffer) (=> self getc-image)))

(defmethod (input-stream empty?) ()
  (null (=> self peekc-image)))

(defmethod (input-stream peekc) ()

    % Return the next character from the file, but don't advance to the next
    % character.  Returns NIL on end of file.  Maps CRLF to LF.

    (if (< ptr count)
        (let ((ch (string-fetch buffer ptr)))
	  % Ignore CR if followed by LF
	  (if (and (= ch #\CR)
		   (= (=> self &peek2) #\LF)
		   )
	    #\LF
	    ch
	    ))
	(=> self &fill-buffer-and-peekc)
	))

(defmethod (input-stream &fill-buffer-and-peekc) () % Internal method.
  (and (=> self &fill-buffer) (=> self peekc)))

(defmethod (input-stream peekc-image) ()

    % Return the next character from the file, but don't advance to the next
    % character.  Returns NIL on end of file.

    (if (< ptr count)
        (string-fetch buffer ptr)
	(=> self &fill-buffer-and-peekc-image)
	))

(defmethod (input-stream &fill-buffer-and-peekc-image) () % Internal method.
  (and (=> self &fill-buffer) (=> self peekc-image)))

(defmethod (input-stream &peek2) () % Internal method.

    % Return the character after the next character in the file, but don't
    % advance.  Does not map CRLF.  Returns Ascii NUL on end of file.  Requires
    % that the buffer contain at least one character.  This is a hack required
    % to implement PEEKC.

    (let ((next-ptr (+ ptr 1)))
      (cond ((>= next-ptr count)
	     % The next character has not yet been read into the buffer.
	     (let* ((old-pos (RFPTR jfn))
		    (ch (BIN jfn))
		    )
	       (SFPTR jfn old-pos)
	       ch
	       ))
	    (t (string-fetch buffer next-ptr))
	    )))

(defmethod (input-stream &fill-buffer) () % Internal method.
  % Return NIL iff there are no more characters.
  (if eof-flag
      NIL
      (let ((n (SIN jfn (jconv buffer) (- (const FILE-BUFFER-SIZE)))))
        (if (~= n 0) (setf eof-flag T))
        (setf count (+ (const FILE-BUFFER-SIZE) n))
        (setf ptr 0)
	(~= count 0))))

(defmethod (input-stream getl) ()
  % Read and return (the remainder of) the current input line.
  % Read, but don't return the terminating EOL (if any).
  % (EOL is interpreted as LF or CRLF)
  % Return NIL if no characters and end-of-file detected.

  (if (and (>= ptr count) (not (=> self &fill-buffer)))
    NIL
    % Else
    (let ((start ptr) (save-buffer NIL) (eof? NIL))
      (while (and (not eof?) (~= (string-fetch buffer ptr) #\LF))
	 (setf ptr (+ ptr 1))
	 (cond ((>= ptr count)
		(setf save-buffer
		      (concat save-buffer (subseq buffer start ptr)))
		(setf eof? (not (=> self &fill-buffer)))
		(setf start ptr)
		))
	 )
      (if eof?
	save-buffer
	% Else
	(setf ptr (+ ptr 1))
	(if (= ptr 1)
	  (if save-buffer
	    (if (= (string-fetch save-buffer (size save-buffer)) #\CR)
	      (subseq save-buffer 0 (size save-buffer))
	      (sub save-buffer 0 (size save-buffer)))
	    (subseq buffer start ptr))
	  (if (= (string-fetch buffer (- ptr 2)) #\CR)
	    (concat save-buffer (subseq buffer start (- ptr 2)))
	    (concat save-buffer (subseq buffer start (- ptr 1)))
	    )))
      )))

(defmethod (input-stream tell-position) ()
  % Return an integer representing the current "position" of the stream.  About
  % all we can guarantee about this integer is (1) it will be 0 at the
  % beginning of the file and (2) if you later SEEK-POSITION to this integer,
  % the stream will be reset to its current position.  The reason for this
  % fuzziness is that the translation of CRLF into LF performed by the "normal"
  % input operations makes it impossible to predict the relationship between
  % the apparent file position and the actual file position.

  (- (RFPTR jfn) (- count ptr))
  )

(defmethod (input-stream seek-position) (p)
  (setf p (int2sys p))
  (let* ((buffer-end (RFPTR jfn))
	 (buffer-start (- buffer-end count)))
    (if (and (>= p buffer-start) (< p buffer-end))
      (setf ptr (- p buffer-start))
      % Else
      (SFPTR jfn p)
      (setf ptr 0)
      (setf count 0)
      (setf eof-flag NIL)
      )
    ))

(defmethod (input-stream open) (name-of-file)

  % Open the specified file for input via SELF.  If the file cannot be opened,
  % a Continuable Error is generated.

  (if jfn (=> self close))
  (setf buffer (MkString (const FILE-BUFFER-SIZE) #\space))
  (setf ptr 0)
  (setf count 0)
  (setf eof-flag NIL)
  (setf jfn (Dec20Open name-of-file 
	         (int2sys 2#001000000000000001000000000000000000)
	         (int2sys 2#000111000000000000010000000000100000)
	         ))
  (if (= jfn 0) (setf jfn NIL))
  (if (null jfn)
   (=> self open
       (ContinuableError
         0
         (BldMsg "Unable to Open '%w' for Input." name-of-file)
         name-of-file))
   % Else
   (setf file-name (jfn-truename jfn))
   ))

(defmethod (input-stream close) ()
  (when jfn
    (CLOSF jfn)
    (setf jfn NIL)
    (setf buffer NIL)
    (setf count 0)
    (setf ptr 0)
    (setf eof-flag T)
    ))

(defmethod (input-stream read-date) ()
  (jfn-read-date jfn))

(defmethod (input-stream write-date) ()
  (jfn-write-date jfn))

(defmethod (input-stream delete-file) ()
  (jfn-delete jfn))

(defmethod (input-stream undelete-file) ()
  (jfn-undelete jfn))

(defmethod (input-stream delete-and-expunge-file) ()
  (jfn-delete-and-expunge jfn))

(defmethod (input-stream author) ()
  (jfn-author jfn))

(defmethod (input-stream original-author) ()
  (jfn-original-author jfn))

(defmethod (input-stream file-length) ()
  (jfn-byte-count jfn))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTING CODE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CommentOutCode (progn

(de test-buffered-input (name-of-file)
  (setf s (open-input name-of-file))
  (while (setf ch (input-stream$getc s))
    (WriteChar ch)
    )
  (=> s close)
  (Prin2 "---EOF---")
  NIL
  )

(de time-buffered-input (name-of-file)
  (setf start-time (time))
  (setf s (open-input name-of-file))
  (while (setf ch (input-stream$getc s))
    )
  (=> s close)
  (- (time) start-time)
  )

(de time-buffered-input-1 (name-of-file)
  (setf start-time (time))
  (setf s (open-input name-of-file))
  (while (setf ch (=> s getc))
    )
  (=> s close)
  (- (time) start-time)
  )

(de time-standard-input (name-of-file)
  (setf start-time (time))
  (setf chan (open name-of-file 'INPUT))
  (while (not (= (setf ch (ChannelReadChar chan)) $EOF$))
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
