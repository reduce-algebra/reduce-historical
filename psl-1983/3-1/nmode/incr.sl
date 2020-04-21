%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Incremental-Search.SL - Incremental Search Routines for NMODE
%
% Author:     Jeffrey Soreff
%             Hewlett-Packard/CRC
% Date:       21 December 1982
% Revised:    17 February 1982
%
% 17-Feb-83 Alan Snyder
%  Fixed to allow pushback of bit-prefix characters.
% 7-Feb-83 Alan Snyder
%  Revised to refresh all windows when writing message (write-message no
%  longer does this).
% 18 January 1982 Jeffrey Soreff
%  This was revised to preserve the message existing before a search.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load fast-strings fast-vectors fast-int extended-char))
(BothTimes (load objects))

% Global Variables

(fluid '(text-last-searched-for))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Actual Command Functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de incremental-search-command () (incr-search 1))

(de reverse-search-command () (incr-search -1))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Support Objects and Methods
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defflavor search-state
  ((state-list nil)
   (halt nil) % Halt means that the search should halt on this iteration.
   direct % This is the direction of the search: +1 for forward, -1 for back.
   (repeat-flag nil) % When repeating a search for the same text as before.
   (found-flag t) % This flag indicates that the current text was found.
   (place (buffer-get-position)) % This is set to the start of text found.
   (apparent-place (buffer-get-position))
   % Apparent-place is put where the user should see the cursor: after the
   % text for forward searching, and before it for backward searching.
   (text [""])) % The text being searched for.
  ()
  (gettable-instance-variables halt)
  (initable-instance-variables direct)
  )

(defmethod (search-state push) ()
  % This method stores the information needed when one deletes a
  % character from the search string. It affects only state-list.
  (setf state-list
    (cons
     (vector direct repeat-flag found-flag place apparent-place)
     state-list)))

(defmethod (search-state pop) ()
  % This method restores the last state of the search. The text is
  % recomputed on the fly, while most of the other elements of the
  % state are explicitly retrieved from the list. "Halt" is not
  % retrieved, since the search should never pass a state where halt
  % is true. In addition to altering local variables,
  % text-last-searched-for is set equal to the truncated text, and
  % point is moved to its last location.
  (unless repeat-flag (setf text (trim-text text)))
  (when (cdr state-list)
    (setf state-list (cdr state-list))
    (setf text-last-searched-for text)) % see next line.
  % Don't destroy information from previous search if one is in the
  % first state of a search and a deletion is attempted.
  (let ((state (car state-list)))
    (setf direct (vector-fetch state 0))
    (setf repeat-flag (vector-fetch state 1))
    (setf found-flag (vector-fetch state 2))
    (setf place (vector-fetch state 3))
    (setf apparent-place (vector-fetch state 4)))
  (buffer-set-position apparent-place))

(defmethod (search-state do-search) (next-command)
  % This method sets up searches. It analyses the current command to
  % determine if a search for old text is being repeated, or if a new
  % character is being added on to the existing text. It updates the
  % text being searched for, the record of the last text searched for,
  % the direction of the search, and it sets up point before searches.
  (let ((char-add-list nil))
    (cond ((setf repeat-flag (=> next-command repeat-flag))
	   (setf direct (=> next-command direct))
	   (when (and (= direct (vector-fetch (car state-list) 0))
		      % The direction hasn't changed since the last search.
		      (equal text [""]))
	     (setf repeat-flag nil) % This is not a search for the text last searched for.
	     (setf char-add-list (text2list text-last-searched-for))))
	  (t (setf char-add-list (list (=> next-command char)))))
    (if repeat-flag
      (=> self actual-search)
      % else
      (for (in current-char char-add-list)
	   (do (setf text (new-text text current-char))
	       (buffer-set-position place)
	       (=> self actual-search)))))
  (unless (equal text [""]) (setf text-last-searched-for text)))

(defmethod (search-state actual-search) ()
  % This method does the actual searching for text. It first checks to
  % see if the search could possibly succeed, which it couldn't if the
  % search just extends a previously unsuccessful search in the old
  % direction. This method also stores the location of the start of
  % the new text and the location at which the user should see the
  % cursor after the search.
  (when (or found-flag (~= direct (vector-fetch (car state-list) 0)))
    % One should search if the last text was found or the direction has changed.
    (let ((backed-up (when (and repeat-flag (< direct 0))
		       (move-backward-character))))
      % Avoid jamming at the current string in repeated backward search.
      (setf found-flag (buffer-text-search? text direct))
      (when (not found-flag) (ding))
      (when (and backed-up (not found-flag)) (move-forward-character))))
  (when found-flag
    (setf place (buffer-get-position))
    (if (> direct 0) (move-over-text text))
    (setf apparent-place (buffer-get-position))) % end of text if forward
  (buffer-set-position apparent-place)
  (=> self push))

(defmethod (search-state super-pop) ()
  % This method pops off all unsuccessful searches or, if the last
  % search was successful, undoes all the searching.
  (cond (found-flag (setf state-list (lastpair state-list)) % first state
		    (setf text [""])
		    (setf halt t)
		    (=> self pop))
	(t (while (not found-flag)
	     (=> self pop))
	   (ding))))

(defmethod (search-state init) () 
  (=> self prompt)
  (=> self push))

(defmethod (search-state prompt) ()
  (update-message text found-flag direct))

(defflavor parsed-char
  (char halt pop-flag repeat-flag direct)
  % Char is the next character returned after processing.  Halt is a
  % flag indicating if the searching should halt unconditionally.
  % Pop-flag indicates whether a delete is being done.  Repeat-flag
  % indicates whether one of the commands (^R and ^S) which trigger
  % searching for the same text as before (but possibly in a new
  % direction) has occured.  Direct indicates the direction that the
  % search should take.
  ()
  gettable-instance-variables)

(defmethod (parsed-char parse-next-character) ()
  % This function inputs and parses new characters or commands.
  (setf char (input-terminal-character))
  (setf halt nil)
  (setf pop-flag nil)
  (setf repeat-flag nil)  
  (let ((up-char (X-Char-Upcase char)))
    (cond ((= up-char (x-char C-Q))
	   (setf char (input-direct-terminal-character)))
	  ((or (= up-char (x-char Rubout))(= up-char (x-char Backspace)))
	   (setf repeat-flag nil)
	   (setf pop-flag t))
	  ((= up-char (x-char C-G))
	   (setf repeat-flag t)
	   (setf pop-flag t))
	  ((or (= up-char (x-char C-S))(= up-char (x-char C-R)))
	   (setf repeat-flag t)
	   (if (= up-char (x-char C-S))
	     (setf direct +1)
	     (setf direct -1)))
	  ((= up-char (x-char Escape))
	   (setf halt t))
	  ((or (= up-char (x-char Return))(not (X-Control? up-char))))
	  % The last line detects normal characters.
	  (t % normal control character
	   (push-back-input-character char)
	   (setf halt t)))))

(de incr-search (direct)
  % The main function for the search
  (let* ((old-msg (write-message ""))
	 (search-at (make-instance 'search-state 'direct direct))
	 (next-command (make-instance 'parsed-char)))
    (while (continue search-at next-command) % gets and parses next char
      % The main loop for the search
      (if (=> next-command pop-flag)
	(if (=> next-command repeat-flag)
	  (=> search-at super-pop)
	  (=> search-at pop))
	(=> search-at do-search next-command))
      (=> search-at prompt))
    (write-message old-msg))) % This restores the message after the search.

(de continue (search-state parsed-char)
  % This function parses the next input character, if that is called
  % for, and determines if the search should continue or be halted. It
  % returns a boolean value which is true if the search should
  % continue.
  (unless 
    (=> search-state halt)
    (=> parsed-char parse-next-character)
    (not (=> parsed-char halt))))

(de update-message (text found direct)
  % This function displays the last line of the search string, whether
  % it was found, and in what direction the search proceeded.
  (let* ((line-count (vector-upper-bound text))
	 (last-line (vector-fetch text line-count)))
    (write-message
     (string-concat
      (if found "" "Failing ")
      (if (> direct 0) "" "Reverse ")
      "I-search: "
      last-line))
    (nmode-refresh)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Start of text handling functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de move-over-text (text)
  % This function moves point to the end of a chunk of text, assuming
  % that point is started at the beginning of the text.
  (let ((line-count (vector-upper-bound text)))
    (set-line-pos (+ (current-line-pos) line-count))
    (if (> line-count 0)(move-to-start-of-line))
    (move-over-characters (string-length (vector-fetch text line-count)))))

(de trim-text (old-text)
  % This is a pure function, without side effects.  It trims one
  % character or empty line return off the old text.  It will not,
  % however, delete the last null string from a text vector.  In that
  % case it dings and returns the old text.
  (let*  ((line-count (vector-upper-bound old-text))
	  (short-text (sub old-text 0 (- line-count 1)))
	  (last-line (vector-fetch old-text line-count))
	  (last-count (string-length last-line)))
    (if (> last-count 0)
      (concat short-text (vector (sub last-line 0 (- last-count 2))))
      (if (> line-count 0) short-text (Ding) old-text))))

(de new-text (old-text char)
  % This is a pure function, without side effects.  It returns an
  % updated version of the text vector.  It updates the text vector by
  % adding a new character or a new line.
  (let* ((line-count (vector-upper-bound old-text))
	 (short-text (sub old-text 0 (- line-count 1)))
	 (last-line (vector-fetch old-text line-count)))
    (if (= char (x-char Return))
      (concat old-text [""])
      (concat short-text
	      (vector (string-concat last-line (string char)))))))

(de text2list (text)
  % This function converts text into a list of characters, with cursor
  % returns where the breaks between strings used to be.
  (append (string2list (vector-fetch text 0))
	  (for (from indx 1 (vector-upper-bound text) 1)
	       (join (cons (x-char return) 
			   (string2list (vector-fetch text indx)))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Start of text searching functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de buffer-text-search? (text direct)
  % This function searches in the buffer for the specified text.  The
  % direct is +1 for forward searching and -1 for backward
  % searching.  This function leaves point at the start of the text,
  % if it is found, and at the old point if the text is not found.
  % This function returns a boolean, true if it found the text.
  (let ((current-place (buffer-get-position))
	(match-rest nil))
    (while (and (not match-rest) 
		(buffer-search (vector-fetch text 0) direct))
      (setf match-rest (match-rest-of-text? text))
      (unless match-rest 
	(if (> direct 0)(move-forward)(move-backward))))
    (unless match-rest (buffer-set-position current-place))
    match-rest))

(de match-rest-of-text? (text)
  % This function determines if two conditions are satified: First,
  % that all lines in text except the last fill out their respective
  % lines. Second, that all lines except the first match their
  % respective lines.  This function assumes that point is initially
  % at the start of a string which matches the first string in text.
  % It also assumes that text is in upper case. This function returns
  % a boolean value. It does not move point.
  (let ((temp nil) % This avoids a compiler bug.
	(indx 0)
	(match-rest t)
	(line (current-line-pos))
	(char-pos (current-char-pos)))
    (while (and match-rest (< indx (vector-upper-bound text)))
      (setf temp (+ char-pos (string-length (vector-fetch text indx))))
      (setf match-rest 
	(and match-rest % Check filling out of lines.
	     (= temp
		(string-length (current-buffer-fetch (+ line indx))))))
      (setf char-pos 0) % Only the first string is set back on its line.
      (incr indx)
      (setf match-rest
	(and match-rest % Check matching of lines.
	     (pattern-matches-in-line
	      (string-upcase (vector-fetch text indx))
	      (current-buffer-fetch (+ line indx)) 0))))
    (and match-rest (= indx (vector-upper-bound text)))))