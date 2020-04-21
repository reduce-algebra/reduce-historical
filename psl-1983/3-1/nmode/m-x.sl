%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% M-X.SL - NMODE Extended Command Support
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        20 September 1982
% Revised:     29 December 1982
%
% 29-Dec-82 Alan Snyder
%  Revise PROMPT-FOR-EXTENDED-COMMAND to use new prompted input.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects fast-int fast-strings extended-char))

(fluid '(nmode-input-buffer))

% Internal variables:

(fluid '(prompt-for-extended-command-command-list
	 current-extended-command-list
	 ))

(setf prompt-for-extended-command-command-list
  (list
   (cons (x-char SPACE) 'complete-input-command-name)
   (cons (x-char CR) 'complete-and-terminate-input-command-name)
   (cons (x-char LF) 'complete-and-terminate-input-command-name)
   ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de prompt-for-extended-command (prompt)
  % Ask the user for the name of an extended command.  Return the full command
  % name from the dispatch table (so that EQ can be used to compare).

  (setf current-extended-command-list (lookup-prefix-character (x-char M-X)))
  (let* ((input-name (prompt-for-string-special
		      prompt
		      nil
		      prompt-for-extended-command-command-list))
	 (matching-names (extended-command-names-that-match input-name))
	 )
    (first matching-names)
    ))

% Internal functions:

(de complete-input-command-name ()
  % Extend the string in the input buffer by at most one word to match
  % the existing extended command names.  Ring the bell if the string
  % is not extended.

  (let ((original-length (string-length (nmode-get-input-string))))
    (complete-input-extended-command-name NIL)
    (if (= original-length (string-length (nmode-get-input-string)))
      (Ding)
      )))

(de complete-and-terminate-input-command-name ()
  % Extend the string in the input buffer as far as possible to match the
  % existing extended command names.  If the resulting string uniquely
  % identifies a single command name, refresh and terminate input.  Otherwise,
  % if the string was not extended, ring the bell.

  (let* ((original-length (string-length (nmode-get-input-string)))
	 (name (complete-input-extended-command-name T))
	 )
    (if name
      (progn (nmode-refresh) (nmode-terminate-input))
      (if (= original-length (string-length (nmode-get-input-string)))
	(Ding)
	))))

(de complete-input-extended-command-name (many-ok)
  % Extend the string in the input buffer BY WORDS.  If MANY-OK is non-nil, then
  % extend by as many words as possible; otherwise, by only one word.  If the
  % extended name matches exactly one command name, return that command name.
  % Otherwise, return NIL.

  (let* ((name (nmode-get-input-string))
	 (names (extended-command-names-that-match name))
	 )
    (cond
     ((string-equal name "E")
      (nmode-replace-input-string "Edit ")
      NIL
      )
     ((string-equal name "L")
      (nmode-replace-input-string "List ")
      NIL
      )
     ((string-equal name "K")
      (nmode-replace-input-string "Kill ")
      NIL
      )
     ((string-equal name "V")
      (nmode-replace-input-string "View ")
      NIL
      )
     ((string-equal name "W")
      (nmode-replace-input-string "What ")
      NIL
      )
     ((null names) % The name matches no command.
      NIL
      )
     ((null (cdr names)) % The name matches exactly one command.
      (nmode-replace-input-string (extend-name-by-words name names many-ok))
      (car names)
      )
     (t % The name matches more than one command.
      (nmode-replace-input-string (extend-name-by-words name names many-ok))
      NIL
      ))
    ))

(de extend-name-by-words (name names many-ok)
  % NAME is the current contents of the input buffer.  Extend it "by words" as
  % long as it matches all of the specified NAMES.  NAMES must be a list
  % containing one or more strings.  If MANY-OK is non-NIL, then extend it by as
  % many words as possible.  Otherwise, extend it by at most one word.
  % Extending by words means that you do not append a new partial word, although
  % you may partially complete a word already started.  Return the extended
  % string.

  (let* ((match-prefix (strings-largest-common-prefix names))
	 (partial-word
	  (not (or
		(string-empty? name)
		(= (string-fetch name (string-upper-bound name)) #\space)
		)))
	 (bound (string-length name))
	 )
    % Try to increase the "bound":
    (for (from i bound (string-upper-bound match-prefix))
	 (do (when (= (string-fetch match-prefix i) #\space)
	       (setf bound (+ i 1)) % this far is OK
	       (setf partial-word NIL) % further words will extend only in full
	       (if (not many-ok) (exit))
	       ))
	 (finally
	  (if (or partial-word (null (cdr names)))
	    (setf bound (string-length match-prefix))
	    )))
    (substring match-prefix 0 bound)
    ))

(de extended-command-names-that-match (name)
  (for (in pair (cdr current-extended-command-list))
       (when (name-matches-prefix name (car pair)))
       (collect (car pair))
       ))

(de name-matches-prefix (test-name name)
  (let ((test-len (string-length test-name))
	(name-len (string-length name))
	)
    (and
      (>= name-len test-len)
      (string-equal (substring name 0 test-len) test-name)
      )))
