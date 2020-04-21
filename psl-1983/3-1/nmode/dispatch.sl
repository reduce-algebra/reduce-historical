%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% DISPATCH.SL - NMODE Dispatch table utilities
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        24 August 1982
%
% Adapted from Will Galway's EMODE
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects extended-char fast-int fast-vectors))
(fluid '(nmode-current-buffer nmode-minor-modes))

% A command is represented either as a single extended character (i.e., a
% character including Meta and Control bits) or as a list whose first element
% is an extended character (a command prefix character, e.g. C-X or M-X) and
% whose second element is the "argument", either an extended character or a
% string (for M-X).

% The dispatch table maps commands (as defined above) to functions (of no
% arguments).  There is a single command table that defines the "keyboard
% bindings" for the current mode.  Associated with every buffer is a list of
% forms to evaluate which will establish the keyboard bindings for that
% buffer.

% The dispatch table is represented by a 512-element vector
% NMODE-DISPATCH-TABLE which maps extended characters to functions, augmented
% by an association list for each prefix character (e.g., C-X and M-X) that
% maps extended characters to functions.  The prefix character assocation lists
% are themselves stored in an association list that maps from prefix
% characters.  This master association list is bound to the variable
% NMODE-PREFIX-DISPATCH-LIST.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The following are INTERNAL static variables:

(fluid '(nmode-dispatch-table nmode-prefix-dispatch-list))

(if (null nmode-dispatch-table)
  (setf nmode-dispatch-table (MkVect 511)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dispatch table lookup functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de dispatch-table-lookup (command)
  % Return the dispatch table entry for the specified character or character
  % sequence.  NIL is returned for undefined commands.

  (cond
    % Single character:
    ((FixP command)
     (getv nmode-dispatch-table command)
     )

    % Character sequence:
    ((PairP command)
      (let* ((prefix-char (car command))
	     (argument (cadr command))
	     (prefix-entry (lookup-prefix-character prefix-char))
	     )
        (and prefix-entry
	     % Look up the entry for the prefixed character.
	     (let ((char-entry (Atsoc argument prefix-entry)))
	       (and char-entry (cdr char-entry))
	       ))))

    % If we get here, we were given a bad argument
    (t
      (StdError (BldMsg "Bad argument %p for Dispatch-Table-Lookup" command))
      )))

(de lookup-prefix-character (ch)

  % Return the pair (PREFIX-CHAR .  ASSOCIATION-LIST) for the specified prefix
  % character.  This pair may be modified using RPLACD.

  (let ((assoc-entry (atsoc ch nmode-prefix-dispatch-list)))
    (when (null assoc-entry)
      % Create an entry for this prefix character.
      (setf assoc-entry (cons ch NIL))
      (setf nmode-prefix-dispatch-list
	    (cons assoc-entry nmode-prefix-dispatch-list))
      )
    assoc-entry
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Manipulating the dispatch table:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-make-self-inserting (chr)
  % Define the specified character to be "self inserting".
  (nmode-define-command chr 'insert-self-command))

(de nmode-undefine-command (chr)
  % Remove the command definition of the specified command.
  % If the command is entered, the bell will be rung.
  (nmode-define-command chr NIL))

(de nmode-define-commands (lis)
  (for (in x lis) (do (nmode-define-command (car x) (cdr x)))))

(de nmode-define-normal-self-inserts ()
  (nmode-make-self-inserting (char TAB))
  (for (from i 32 126) (do (nmode-make-self-inserting i))))

(de nmode-define-command (command op)
  % Set up the keyboard dispatch table for a character or a character sequence.
  % If the character is uppercase, define the equivalent lower case character
  % also.

  (cond
    % Single character:
    ((FixP command)
     (vector-store nmode-dispatch-table command op)
     (cond
       ((X-UpperCaseP command)
        (vector-store nmode-dispatch-table (X-Char-DownCase command) op))))

    % Character Sequence:
    ((PairP command)
      (let* ((prefix-char (car command))
	     (argument (cadr command))
	     (prefix-entry (lookup-prefix-character prefix-char))
	     )

        (if (null prefix-entry)
          (StdError (BldMsg "Undefined prefix-character in command %p" command))
	  % else

          % Add the prefixed character to the association list.  Note that in
          % case of duplicate entries the last one added is the one that counts.

          (rplacd prefix-entry
	    (cons (cons argument op) (cdr prefix-entry)))

          % Define the lower case version of the character, if relevent. 
          (cond
            ((and (FixP argument) (X-UpperCaseP argument))
              (rplacd prefix-entry
                (cons (cons (X-Char-DownCase argument) op)
		      (cdr prefix-entry)))
	     )))))

    % If we get here, we were given a bad argument
    (t
      (StdError (BldMsg "Impossible command %p" command))
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Mode Establishing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-establish-current-mode ()
  (when nmode-current-buffer
    (nmode-clear-dispatch-table)
    (nmode-establish-mode (=> nmode-current-buffer mode))
    (for (in minor-mode nmode-minor-modes)
	 (do (nmode-establish-mode minor-mode)))
    ))

(de nmode-establish-mode (mode)

  % "Establish" the specified MODE: evaluate its "establish expressions" to set
  % up the dispatch table.  Use reverse so things on front of list are
  % evaluated last.  (So that later incremental changes are added later.)

  (for (in x (reverse (=> mode establish-expressions)))
       (do
          (if (pairp x)
            (eval x)
            (StdError (BldMsg "Invalid mode expression: %r" x))
	    ))
       ))

(de nmode-clear-dispatch-table ()
  % Set up a "clear" dispatch table.
  (for (from i 0 511)
       (do (nmode-undefine-command i)))
  (setf nmode-prefix-dispatch-list NIL))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Help for Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de help-dispatch ()

  % Give a little information on the routine bound to a keyboard character (or
  % characters, in the case of prefixed things).

  (nmode-set-delayed-prompt "Show function of command: ")
  (let* ((command (input-command))
	 (func (dispatch-table-lookup command))
	 (prompt (BldMsg "%w    %w" (command-name command)
	    (or func "Undefined")))
	 )
    (write-prompt prompt)
    ))

(de print-all-dispatch ()
  % Print out the current dispatch table.
  (print-matching-dispatch NIL))

(fluid '(function-name-match-string))
(de function-name-matcher (f)
  (string-indexs (id2string f) function-name-match-string))

(de string-indexs (s pattern)

  % Search in the string S for the specified pattern.  If we find it, we return
  % the position of the first matching character.  Otherwise, we return NIL.

  (let* ((pattern-length (string-length pattern))
	 (limit (- (string-length s) pattern-length))
	 )
    (for (from pos 0 limit)
	 (do (if (pattern-in-string pattern s pos)
		 (exit pos)))
	 )
    ))

(de pattern-in-string (pattern s pos)
  % Return T if PATTERN occurs as substring of S, starting at POS.
  % No bounds checking is performed on S.

  (let ((i 0) (patlimit (string-upper-bound pattern)))
    (while (and (<= i patlimit)
		(= (string-fetch pattern i)
		   (string-fetch s (+ i pos)))
		)
      (setf i (+ i 1))
      )
    (> i patlimit) % T if all chars matched, NIL otherwise
    ))

(de print-matching-dispatch (s)
  % Print out the current dispatch table, showing only those function
  % whose names contain the string S (if S is NIL, show all functions).

  (let (f)
    (when s
      (setf function-name-match-string (string-upcase s))
      (setf f #'function-name-matcher)
      )

    % List the routines bound to single characters:
    (for (from ch 0 511)
         (do (print-dispatch-entry ch f)))
    % List the routines bound to prefix characters:
    (for (in prefix-entry nmode-prefix-dispatch-list)
         (do (for (in char-entry (cdr prefix-entry))
	          (do (print-dispatch-entry
		 	(list (car prefix-entry) (car char-entry))
			f
			)
		      ))))
    ))

(de print-dispatch-entry (command f)
  % Print out the dispatch routine for a character or character sequence.
  % Don't print anything if F is non-nill and (F fname) returns NIL, the
  % command is a self inserting character, "undefined", or a lower-case
  % character whose upper-case equivalent has the same definition.

  (let ((fname (dispatch-table-lookup command)))
    (if (not (or (null fname)
		 (memq fname 
		       '(insert-self-command argument-or-insert-command Ding))
		 (and f (null (apply f (list fname))))
		 (is-redundant-command? command)
		 ))
	(PrintF "%w %w%n" (string-pad-right (command-name command) 22) fname)
	)))

(de is-redundant-command? (command)
  (let ((ch (if (FixP command) command (cadr command))))
    (and (FixP ch)
	 (X-LowerCaseP ch)
	 (eq (dispatch-table-lookup command)
	     (dispatch-table-lookup
	       (if (FixP command)
		 (X-Char-UpCase command)
		 (list (car command) (X-Char-Upcase (cadr command)))
		 ))))))

(de command-name (command)
  % Return a string giving the name for a character or character sequence.
  (if (PairP command)
    (string-concat
      (prefix-name (car command))
      (let ((argument (cadr command)))
	(cond ((FixP argument) (x-char-name argument))
	      (t argument)
	      )))
    (x-char-name command)
    ))
