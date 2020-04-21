%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Text-Buffer.SL
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        20 August 1982
% Revised:     23 February 1983
%
% A text buffer.  Supports the primitive editing functions.  The strings in a
% text buffer are never modified.  This allows EQ to be used to minimize
% redisplay.
%
% 23-Feb-83 Alan Snyder
%  Revise stream operations to work with any type of object.
% 15-Feb-83 Alan Snyder
%  Revise insertion code to reduce unnecessary consing.
%  Remove char-blank? macro (NMODE has a function char-blank?).
% 19-Jan-83 Jeff Soreff
%  Name made settable in text buffer.
% 3-Dec-82 Alan Snyder
%  Added cleanup method.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))
(CompileTime (load fast-int fast-vectors fast-strings))
  
(de create-text-buffer (name) % not for direct use in NMODE
  (let ((buffer (make-instance 'text-buffer 'name name)))
    buffer))

(defflavor text-buffer (
  (last-line 0)			% index of last line in buffer (n >= 0)
  (line-pos 0)			% index of "current" line (0 <= n <= last-line)
  (char-pos 0)			% index of "current" character in current line
				% (0 <= n <= linelength)
  lines				% vector of strings
  name				% string name of buffer
  (file-name NIL)  		% string name of attached file (or NIL)
  (modified? NIL)		% T => buffer is different than file
  marks				% ring buffer of marks
  (mode NIL)			% the buffer's Mode
  (previous-buffer NIL)		% (optional) previous buffer
  (p-list NIL)			% association list of properties
  )
  ()
  (gettable-instance-variables line-pos char-pos)
  (settable-instance-variables file-name modified? mode previous-buffer name)
  (initable-instance-variables name)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Private Macros:

(CompileTime (progn

(defmacro with-current-line ((var) . forms)
  `(let ((,var (vector-fetch lines line-pos)))
     ,@forms
     ))

(defmacro with-current-line ((var) . forms) % avoid compiler bug!
  `(let ((**LINES** lines))
     (let ((,var (vector-fetch **LINES** line-pos)))
       ,@forms
       )))

(defmacro with-current-line-copied ((var) . forms)
  `(let ((**LINES** lines) (**LINE-POS** line-pos))
     (let ((,var (copystring (vector-fetch **LINES** **line-pos**))))
       (vector-store **LINES** **line-pos** ,var)
       ,@forms
       )))

)) % End of CompileTime

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (text-buffer position) ()
  % Return the "current position" in the buffer as a BUFFER-POSITION object.

  (buffer-position-create line-pos char-pos)
  )

(defmethod (text-buffer set-position) (bp)
  % Set the "current position" in the buffer from the specified
  % BUFFER-POSITION object.  Clips the line-position and char-position.

  (=> self goto (buffer-position-line bp) (buffer-position-column bp))
  )

(defmethod (text-buffer buffer-end-position) ()
  % Return the BUFFER-POSITION object corresponding to the end of the buffer.
  (buffer-position-create
    last-line
    (string-length (vector-fetch lines last-line))
    ))

(defmethod (text-buffer goto) (lpos cpos)
  % Set the "current position" in the buffer.  Clips the line-position and
  % char-position.

  (if (< lpos 0) (setf lpos 0))
  (if (> lpos last-line) (setf lpos last-line))
  (setf line-pos lpos)
  (=> self set-char-pos cpos)
  )

(defmethod (text-buffer set-line-pos) (lpos)
  % Set the "current line position" in the buffer.  Clips the line-position
  % and char-position.

  (when (~= lpos line-pos)
    (if (< lpos 0) (setf lpos 0))
    (if (> lpos last-line) (setf lpos last-line))
    (setf line-pos lpos)
    (with-current-line (l)
      (if (> char-pos (string-length l))
	  (setf char-pos (string-length l))
	  ))
    ))

(defmethod (text-buffer set-char-pos) (cpos)
  % Set the "current character position" in the buffer.  Clips the specified
  % position to lie in the range 0..line-length.

  (if (< cpos 0) (setf cpos 0))
  (with-current-line (l)
    (if (> cpos (string-length l))
      (setf cpos (string-length l))
      ))
  (setf char-pos cpos)
  )

(defmethod (text-buffer clip-position) (bp)
  % Return BP if BP is a valid position for this buffer, otherwise return a new
  % buffer-position with clipped values.

  (let ((lpos (buffer-position-line bp))
	(cpos (buffer-position-column bp))
	(clipped NIL)
	)
    (cond ((< lpos 0) (setf lpos 0) (setf clipped T))
	  ((> lpos last-line) (setf lpos last-line) (setf clipped T))
	  )
    (cond ((< cpos 0) (setf cpos 0) (setf clipped T))
	  ((> cpos (string-length (vector-fetch lines lpos)))
	   (setf cpos (string-length (vector-fetch lines lpos)))
	   (setf clipped T)
	   ))
    (if clipped
	(buffer-position-create lpos cpos)
	bp
	)))

(defmethod (text-buffer size) ()
  % Return the actual size of the buffer (number of lines).  This number will
  % include the "fake" empty line at the end of the buffer, should it exist.

  (+ last-line 1)
  )  

(defmethod (text-buffer visible-size) ()
  % Return the apparent size of the buffer (number of lines).  This number
  % will NOT include the "fake" empty line at the end of the buffer, should it
  % exist.

  (if (>= (string-upper-bound (vector-fetch lines last-line)) 0)
    (+ last-line 1)  % The last line is real!
    last-line        % The last line is fake!
    ))

(defmethod (text-buffer contents) ()
  % Return the text contents of the buffer (a copy thereof) as a vector of
  % strings (the last string is implicitly without a terminating NewLine).
  (sub lines 0 last-line)
  )

(defmethod (text-buffer current-line) ()
  % Return the current line (as a string).
  (with-current-line (l)
    l))

(defmethod (text-buffer fetch-line) (n)
  % Fetch the specified line (as a string).  Lines are indexed from 0.
  (if (or (< n 0) (> n last-line))
    (ContinuableError
      0
      (BldMsg "Line index %w out of range." n)
      "")
    (vector-fetch lines n)
    ))

(defmethod (text-buffer store-line) (n new-line)
  % Replace the specified line with a new string.
  (if (or (< n 0) (> n last-line))
    (ContinuableError
      0
      (BldMsg "Line index %w out of range." n)
      "")
    % else
    (setf modified? T)
    (vector-store lines n new-line)
    (if (= line-pos n)
      (let ((len (string-length new-line)))
	(if (> char-pos len)
	  (setf char-pos len)
	  )))
    ))

(defmethod (text-buffer select) ()
  % Attach the buffer to the current window, making it the current buffer.
  (buffer-select self)
  )

(defmethod (text-buffer set-mark) (bp)
  % PUSH the specified position onto the ring buffer of marks.
  % The specified position thus becomes the current "mark".
  (ring-buffer-push marks bp)
  )

(defmethod (text-buffer set-mark-from-point) ()
  % PUSH the current position onto the ring buffer of marks.
  % The current position thus becomes the current "mark".
  (ring-buffer-push marks (buffer-position-create line-pos char-pos))
  )

(defmethod (text-buffer mark) ()
  % Return the current "mark".
  (ring-buffer-top marks)
  )

(defmethod (text-buffer previous-mark) ()
  % POP the current mark off the ring buffer of marks.
  % Return the new current mark.
  (ring-buffer-pop marks)
  (ring-buffer-top marks)
  )

(defmethod (text-buffer get) (property-name)
  % Return the object associated with the specified property name (ID).
  % Returns NIL if named property has not been defined.
  (let ((pair (atsoc property-name p-list)))
    (if (PairP pair) (cdr pair))))

(defmethod (text-buffer put) (property-name property)
  % Associate the specified object with the specified property name (ID).
  % GET on that property-name will henceforth return the object.
  (let ((pair (atsoc property-name p-list)))
    (if (PairP pair)
      (rplacd pair property)
      (setf p-list (cons (cons property-name property) p-list))
      )))

(defmethod (text-buffer reset) ()
  % Reset the contents of the buffer to empty and "not modified".

  (setf lines (MkVect 1))
  (vector-store lines 0 "")
  (setf last-line 0)
  (setf line-pos 0)
  (setf char-pos 0)
  (setf modified? NIL)
  )

(defmethod (text-buffer extract-region) (delete-it bp1 bp2)

  % Delete (if delete-it is non-NIL) or copy (otherwise) the text between
  % position BP1 and position BP2.  Return the deleted (or copied) text as a
  % pair (CONS direction-of-deletion vector-of-strings).  The returned
  % direction is +1 if BP1 <= BP2, and -1 otherwise.  The current position is
  % set to the beginning of the region if deletion is performed.

  (setf bp1 (=> self clip-position bp1))
  (setf bp2 (=> self clip-position bp2))
  (prog (dir text text-last l1 c1 l2 c2 line1 line2)
    (setf dir 1) % the default case
    % ensure that BP1 is not beyond BP2
    (let ((comparison (buffer-position-compare bp1 bp2)))
      (if (> comparison 0)
        (psetq dir -1 bp1 bp2 bp2 bp1))
      (if (and delete-it (~= comparison 0))
	(setf modified? T))
      )
    (setf l1 (buffer-position-line bp1))
    (setf c1 (buffer-position-column bp1))
    (setf l2 (buffer-position-line bp2))
    (setf c2 (buffer-position-column bp2))
    % Ensure the continued validity of the current position.
    (if delete-it (=> self set-position bp1))
    % Create a vector for the extracted text.
    (setf text-last (- l2 l1)) % highest index in TEXT vector
    (setf text (MkVect text-last))
    (setf line1 (vector-fetch lines l1)) % first line (partially) in region
    (cond
      ((= l1 l2) % region lies within a single line (easy!)
       (vector-store text 0 (substring line1 c1 c2))
       (if delete-it
	 (vector-store lines l1 (string-concat
				 (substring line1 0 c1)
				 (string-rest line1 c2)
				 )))
       (return (cons dir text))))
    % Here if region spans multiple lines.
    (setf line2 (vector-fetch lines l2)) % last line (partially) in region
    (vector-store text 0 (string-rest line1 c1))
    (vector-store text text-last (substring line2 0 c2))
    % Copy remaining text from region.
    (for (from i 1 (- text-last 1))
	 (do (vector-store text i (vector-fetch lines (+ l1 i)))))
    (when delete-it
      (vector-store lines l1 (string-concat
			      (substring line1 0 c1)
			      (string-rest line2 c2)))
      (=> self &delete-lines (+ l1 1) text-last)
      )
    (return (cons dir text))
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The following methods are not really primitive, but are provided as
% a public service.

(defmethod (text-buffer current-line-length) ()
  % Return the number of characters in the current line.
  (with-current-line (l)
    (string-length l)))

(defmethod (text-buffer current-line-empty?) ()
  % Return T if the current line contains no characters.
  (with-current-line (l)
    (string-empty? l)))

(defmethod (text-buffer current-line-blank?) ()
  % Return T if the current line contains no non-blank characters.
  (with-current-line (l)
    (for (from i 0 (string-upper-bound l))
         (always (char-blank? (string-fetch l i)))
         )))

(defmethod (text-buffer at-line-start?) ()
  % Return T if we are positioned at the start of the current line.
  (= char-pos 0))

(defmethod (text-buffer at-line-end?) ()
  % Return T if we are positioned at the end of the current line.
  (with-current-line (l)
    (> char-pos (string-upper-bound l))))

(defmethod (text-buffer at-buffer-start?) ()
  % Return T if we are positioned at the start of the buffer.
  (and (= line-pos 0) (= char-pos 0)))

(defmethod (text-buffer at-buffer-end?) ()
  % Return T if we are positioned at the end of the buffer.
  (and
    (>= line-pos last-line)
    (> char-pos (string-upper-bound (vector-fetch lines last-line)))))

(defmethod (text-buffer current-line-is-first?) ()
  % Return T if the current line is the first line in the buffer.
  (= line-pos 0))

(defmethod (text-buffer current-line-is-last?) ()
  % Return T if the current line is the last line in the buffer.
  (>= line-pos last-line))

(defmethod (text-buffer current-line-fetch) (n)
  % Return the character at character position N within the current line.
  % An error is generated if N is out of range.
  (with-current-line (l)
    (if (and (>= n 0) (<= n (string-upper-bound l)))
      (string-fetch l n)
      (ContinuableError
        0
        (BldMsg "Character index %w out of range." n)
        "")
      )))

(defmethod (text-buffer current-line-store) (n c)
  % Store the character C at char position N within the current line.
  % An error is generated if N is out of range.
  (with-current-line-copied (l)
    (if (and (>= n 0) (<= n (string-upper-bound l)))
      (progn
	(string-store l n c)
	(vector-store lines line-pos l)
	(setf modified? T)
	)
      (ContinuableError
        0
        (BldMsg "Character index %w out of range." n)
        "")
      )))

(defmethod (text-buffer move-to-buffer-start) ()
  % Move to the beginning of the buffer.
  (setf line-pos 0)
  (setf char-pos 0)
  )

(defmethod (text-buffer move-to-buffer-end) ()
  % Move to the end of the buffer.
  (setf line-pos last-line)
  (with-current-line (l)
    (setf char-pos (string-length l)))
  )

(defmethod (text-buffer move-to-start-of-line) ()
  % Move to the beginning of the current line.
  (setf char-pos 0))

(defmethod (text-buffer move-to-end-of-line) ()
  % Move to the end of the current line.
  (with-current-line (l)
    (setf char-pos (string-length l))))

(defmethod (text-buffer move-to-next-line) ()
  % Move to the beginning of the next line.
  % If already at the last line, move to the end of the line.
  (cond ((< line-pos last-line)
	 (setf line-pos (+ line-pos 1))
	 (setf char-pos 0))
	(t (=> self move-to-end-of-line))))

(defmethod (text-buffer move-to-previous-line) ()
  % Move to the beginning of the previous line.
  % If already at the first line, move to the beginning of the line.
  (if (> line-pos 0)
    (setf line-pos (- line-pos 1)))
  (setf char-pos 0))

(defmethod (text-buffer move-forward) ()
  % Move to the next character in the current buffer.
  % Do nothing if already at the end of the buffer.
  (if (=> self at-line-end?)
    (=> self move-to-next-line)
    (setf char-pos (+ char-pos 1))
    ))

(defmethod (text-buffer move-backward) ()
  % Move to the previous character in the current buffer.
  % Do nothing if already at the start of the buffer.
  (if (> char-pos 0)
    (setf char-pos (- char-pos 1))
    (when (> line-pos 0)
      (setf line-pos (- line-pos 1))
      (=> self move-to-end-of-line)
      )))

(defmethod (text-buffer next-character) ()
  % Return the character to the right of the current position.
  % Return NIL if at the end of the buffer.
  (with-current-line (l)
    (if (>= char-pos (string-length l))
      (if (= line-pos last-line)
	NIL
	(char EOL)
	)
      (string-fetch l char-pos)
      )))

(defmethod (text-buffer previous-character) ()
  % Return the character to the left of the current position.
  % Return NIL if at the beginning of the buffer.
  (if (= char-pos 0)
    (if (= line-pos 0) NIL #\EOL)
    (with-current-line (l)
      (string-fetch l (- char-pos 1)))
    ))

(defmethod (text-buffer insert-character) (c)
  % Insert character C at the current position in the buffer and advance past
  % that character.  Implementation note: some effort is made here to avoid
  % unnecessary consing.

  (if (= c #\EOL)
    (=> self insert-eol)
    % else
    (with-current-line (l)
      (let* ((current-length (string-length l))
	     (head-string
	      (when (> char-pos 0)
		(if (= char-pos current-length) l (substring l 0 char-pos))))
	     (tail-string
	      (when (< char-pos current-length)
		(if (= char-pos 0) l (substring l char-pos current-length))))
	     (s (string c))
	     )
	(when head-string (setf s (string-concat head-string s)))
	(when tail-string (setf s (string-concat s tail-string)))
	(vector-store lines line-pos s)
	(setf char-pos (+ char-pos 1))
	(setf modified? T)
	))))

(defmethod (text-buffer insert-eol) ()
  % Insert a line-break at the current position in the buffer and advance to
  % the beginning of the newly-formed line.  Implementation note: some effort
  % is made here to avoid unnecessary consing.

  (with-current-line (l)
    (=> self &insert-gap line-pos 1)
    (let* ((current-length (string-length l))
	   (head-string
	    (when (> char-pos 0)
	      (if (= char-pos current-length) l (substring l 0 char-pos))))
	   (tail-string
	    (when (< char-pos current-length)
	      (if (= char-pos 0) l (substring l char-pos current-length))))
	   )
      (vector-store lines line-pos (or head-string ""))
      (setf line-pos (+ line-pos 1))
      (vector-store lines line-pos (or tail-string ""))
      (setf char-pos 0)
      (setf modified? T)
      )))

(defmethod (text-buffer insert-line) (l)
  % Insert the specified string as a new line in front of the current line.
  % Advance past the newly inserted line.  Note: L henceforth must never be
  % modified.

  (=> self &insert-gap line-pos 1)
  (vector-store lines line-pos l)
  (setf line-pos (+ line-pos 1))
  (setf modified? T)
  )

(defmethod (text-buffer insert-string) (s)
  % Insert the string S at the current position.  Advance past the
  % newly-inserted string.  Note: S must not contain EOL characters!  Note: S
  % henceforth must never be modified.  Implementation note: some effort is
  % made here to avoid unnecessary consing.

  (let ((insert-length (string-length s)))
    (when (> insert-length 0)
      (with-current-line (l)
	(let* ((current-length (string-length l))
	       (head-string
		(when (> char-pos 0)
		  (if (= char-pos current-length) l (substring l 0 char-pos))))
	       (tail-string
		(when (< char-pos current-length)
		  (if (= char-pos 0) l (substring l char-pos current-length))))
	       )
	  (when head-string (setf s (string-concat head-string s)))
	  (when tail-string (setf s (string-concat s tail-string)))
	  (vector-store lines line-pos s)
	  (setf char-pos (+ char-pos insert-length))
	  (setf modified? T)
	  )))))

(defmethod (text-buffer insert-text) (v)
  % V is a vector of strings similar to LINES (e.g., the last string in V is
  % considered to be an unterminated line).  Thus, V must have at least one
  % element.  Insert this stuff at the current position and advance past it.

  (with-current-line (l)
    (let ((v-last (vector-upper-bound v)))
      (=> self &insert-gap line-pos v-last)
      (let ((vec lines)
	    (prefix-text (substring l 0 char-pos))
	    (suffix-text (string-rest l char-pos))
	    )
        (vector-store vec line-pos
		      (string-concat prefix-text (vector-fetch v 0)))
        (for (from i 1 v-last)
	     (do (setf line-pos (+ line-pos 1))
	         (vector-store vec line-pos (vector-fetch v i))))
        (setf char-pos (string-length (vector-fetch vec line-pos)))
        (vector-store vec line-pos
		      (string-concat (vector-fetch vec line-pos) suffix-text))
	(setf modified? T)
        ))))

(defmethod (text-buffer delete-next-character) ()
  % Delete the next character.
  % Do nothing if at the end of the buffer.

  (with-current-line (l)
    (if (= char-pos (string-length l))
      (if (= line-pos last-line)
	NIL
	% else (at end of line other than last)
	(vector-store lines line-pos
		      (string-concat l (vector-fetch lines (+ line-pos 1))))
	(=> self &delete-lines (+ line-pos 1) 1)
	(setf modified? T)
	)
      % else (not at the end of a line)
      (vector-store lines line-pos
			  (string-concat
			   (substring l 0 char-pos)
			   (string-rest l (+ char-pos 1))
			   ))
      (setf modified? T)
      )))

(defmethod (text-buffer delete-previous-character) ()
  % Delete the previous character.
  % Do nothing if at the beginning of the buffer.

  (if (not (=> self at-buffer-start?))
    (progn
      (=> self move-backward)
      (=> self delete-next-character)
      (setf modified? T)
      )))

% Implementation note: On the 9836, the following implementation of the
% read-from-stream method using GETC is slightly slower than a much simpler
% implementation of read-from-stream using GETL (although the GETL method is
% highly optimized).  For a file with 874 lines, using GETC took 7480 ms vs.
% 7130 ms. when using GETL.  The problem with GETL, however, is that it does
% not report whether the last line of the file is terminated with a Newline or
% not.  This functional difference could conceivably be important.  Luckily,
% the improvement in speed is sufficiently small to be irrelevant.

(defmethod (text-buffer read-from-stream) (s)
  (=> self reset)
  (let* ((line-buffer (make-string 200 0))
	 (buffer-top 200)
	 (getc-method (object-get-handler s 'getc))
	 line-size
	 ch
	 )
    (while T
      (setf line-size 0)
      (setf ch (apply getc-method (list s)))
      (while (not (or (null ch) (= ch #\LF)))
	(cond ((>= line-size buffer-top)
	       (setf line-buffer (concat line-buffer (make-string 200 0)))
	       (setf buffer-top (+ buffer-top 200))
	       ))
	(string-store line-buffer line-size ch)
	(setf line-size (+ line-size 1))
	(setf ch (apply getc-method (list s)))
	)
      (if (not (and (null ch) (= line-size 0)))
	(=> self insert-line (sub line-buffer 0 (- line-size 1)))
	)
      (when (null ch)
	(if (> line-size 0) (=> self delete-previous-character))
	(exit)
	))
    (=> self move-to-buffer-start)
    (=> self set-modified? NIL)
    ))

(defmethod (text-buffer write-to-stream) (s)
  (let* ((vec lines)
	 (putl-method (object-get-handler s 'putl))
	 )
    (for (from i 0 (- last-line 1))
	 (do (apply putl-method (list s (vector-fetch vec i)))))
    (=> s puts (vector-fetch vec last-line))
    ))

(defmethod (text-buffer cleanup) ()
  % Discard any unused storage.
  (if (and previous-buffer (not (buffer-is-selectable? previous-buffer)))
    (setf previous-buffer NIL))
  (TruncateVector lines last-line)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Private methods:

(defmethod (text-buffer init) (init-plist)
  (setf lines (MkVect 0))
  (vector-store lines 0 "")
  (setf marks (ring-buffer-create 16))
  (ring-buffer-push marks (buffer-position-create 0 0))
  )

(defmethod (text-buffer &insert-gap) (lpos n-lines)

  % Insert N-LINES lines at position LPOS, moving the remaining lines upward
  % (if any).  LPOS may range from 0 (insert at beginning of buffer) to
  % LAST-LINE + 1 (insert at end of buffer).  The new lines are not
  % specifically initialized (they retain their old values).

  (when (> n-lines 0)
    (=> self &ensure-room n-lines)
    (let ((vec lines))
      (for (from i last-line lpos -1)
	   (do (vector-store vec (+ i n-lines) (vector-fetch vec i)))
	   )
      (setf last-line (+ last-line n-lines))
      )))

(defmethod (text-buffer &ensure-room) (lines-needed)
  % Ensure that the LINES vector is large enough to add the specified number
  % of additional lines.

  (let* ((current-bound (vector-upper-bound lines))
	 (lines-available (- current-bound last-line))
	 (lines-to-add (- lines-needed lines-available))
	 )
    (when (> lines-to-add 0)
      (let ((minimum-incr (>> current-bound 2))) % Increase by at least 25%
	(if (< minimum-incr 64) (setf minimum-incr 64))
	(if (< lines-to-add minimum-incr) (setf lines-to-add minimum-incr))
	)
      (let ((new-lines (make-vector (+ current-bound lines-to-add) NIL)))
	(for (from i 0 current-bound)
	     (do (vector-store new-lines i (vector-fetch lines i))))
	(setf lines new-lines)
	))))

(defmethod (text-buffer &delete-lines) (lpos n-lines)

  % Remove N-LINES lines starting at position LPOS, moving the remaining lines
  % downward (if any) and NILing out the obsoleted lines at the end of the
  % LINES vector (to allow the strings to be reclaimed).  LPOS may range from
  % 0 to LAST-LINE.

  (when (> n-lines 0)
    (let ((vec lines))
      (for (from i (+ lpos n-lines) last-line)
	   (do (vector-store vec (- i n-lines) (vector-fetch vec i)))
	   )
      (setf last-line (- last-line n-lines))
      (for (from i 1 n-lines)
	   (do (vector-store vec (+ last-line i) NIL))
	   )
      )))
