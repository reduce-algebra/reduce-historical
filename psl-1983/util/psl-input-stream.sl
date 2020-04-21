%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PSL-Input-Stream.SL - File Input Stream Objects (Portable PSL Version)
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        10 December 1982
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load fast-int))
(BothTimes (load objects))

(de attempt-to-open-input (file-name)
  (let ((p (ErrorSet (list 'open-input file-name) NIL NIL)))
    (and (PairP p) (car p))
    ))

(de open-input (file-name)
  (let ((s (make-instance 'input-stream)))
    (=> s open file-name)
    s))

(defflavor input-stream ((chn NIL)	% PSL "channel"
			eof-flag	% T => EOF has been detected
			file-name	% file name given to OPEN
			)
  ()
  (gettable-instance-variables file-name)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (input-stream getc) ()

  % Return the next character from the file.  Line termination is represented
  % by a single NEWLINE (LF) character.  Returns NIL on end of file.

    (if (not eof-flag)
      (let ((ch (ChannelReadChar chn)))
	(if (= ch #\EOF)
	  (prog () (setf eof-flag T)) % return NIL on EOF
	  ch % return the character, otherwise
	  ))))

(defmethod (input-stream getc-image) ()
  (=> self getc))

(defmethod (input-stream empty?) ()
  (null (=> self peekc-image)))

(defmethod (input-stream peekc) ()

    % Return the next character from the file, but don't advance to the next
    % character.  Returns NIL on end of file.

  (let ((ch (=> self getc)))
    (when ch
      (ChannelUnReadChar chn ch)
      ch)))

(defmethod (input-stream peekc-image) ()
  (=> self peekc))

(defmethod (input-stream getl) ()
  % Read and return (the remainder of) the current input line.
  % Read, but don't return the terminating EOL (if any).
  % Return NIL if no characters and end-of-file detected.

  (let ((s ""))
    (while T
      (let ((ch (=> self getc)))
	(if (null ch) (exit (if (string-empty? s) NIL s)))
	(if (= ch #\EOL) (exit s))
	(setf s (string-concat s (string ch)))
	))))

(defmethod (input-stream tell-position) ()
  NIL
  )

(defmethod (input-stream seek-position) (p)
 )

(defmethod (input-stream open) (name-of-file)

  % Open the specified file for input via SELF.  If the file cannot be opened,
  % a Continuable Error is generated.

  (if chn (=> self close))
  (setf eof-flag NIL)
  (setf chn (open name-of-file 'input))
  (setf file-name (copystring name-of-file))
  )

(defmethod (input-stream close) ()
  (when chn
    (close chn)
    (setf chn NIL)
    (setf eof-flag T)
    ))

(defmethod (input-stream read-date) ()
  0)

(defmethod (input-stream write-date) ()
  0)

(defmethod (input-stream delete-file) ()
  )

(defmethod (input-stream undelete-file) ()
  )

(defmethod (input-stream delete-and-expunge-file) ()
  )

(defmethod (input-stream author) ()
  "")

(defmethod (input-stream original-author) ()
  "")

(defmethod (input-stream file-length) ()
  0)
