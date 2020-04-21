%
% DISPCH.SL - Dispatch table utilities
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        25 July 1982
% Copyright (c) 1982 University of Utah
%

% The dispatch table (determining "keyboard bindings") is the 256 element
% vector "MainDispatch", AUGMENTED by association lists for C-X
% (and possibly other prefix) characters.  We actually use an association
% list of association lists: the top level is a list of 
% (prefixchar .  association-list), the second level is a list of
% (character_to_follow_prefix_char . procedure).  Associated with every
% buffer is a list of forms to evaluate which will establish that buffer's
% mode(s)--namely, the keyboard bindings that are in effect for that
% buffer.

% csp 7/7/82
% - Put all dispatch list and mode functions together, and collected
%   some into this file from EMODE1.
% - Modified EstablishCurrentMode to invoke DefinePrefixChars directly.
%   Generalized the idea of adding to a dispatch list with the function
%   AddToKeyList.
% - Modified mode lists to EVAL entries rather than APPLYing functions
%   to NIL.

% AS 7/12/82
% - Added C-X D (Dired), C-X K (Kill Buffer), M-C-L (Previous BUffer)
%   commands to Basic Dispatch list.
% - Separated out read-only text commands into ReadOnlyTextDispatchList.

% AS 7/21/82
% - Attached C-V and M-V to new scroll-window functions.

% WFG 25 July 1982
% - Dired stuff commented back out for now.  ModeEstablishProcedures
%   renamed to be ModeEstablishExpressions.

% AS 7/15/82
% - Changed AddToKeyList to add the new definition at the end of the
%   list, so that it will override existing definitions.
% - Added C-Q.

% AS 8/2/82
% - Revised $Iterate to use delayed prompting feature.

% WFG  23 August 1982
% - Changed AddToKeyList to call EstablishCurrentMode iff *EMODE is T.

(FLUID
  '(
    MainDispatch         % Dispatch table (vector), an entry for each key

    PrefixAssociationLists       % Additional dispatch information for
                                 % prefixed characters.

    % List of declared prefix characters.
    PrefixCharacterList

    SelfInsertCharacter  % Character being dispatched upon.

    last_operation       % The "last" routine dispatched to (before the
                         % "current operation").

    % List of expressions to be evaluated.  Each expression is expected to
    % modify (add to?) the dispatch table.
    ModeEstablishExpressions

    FundamentalTextMode     % See below
))

% Create MainDispatch vector, 256 entries in all.
(setf MainDispatch (MkVect 255))

% List of valid prefix characters.
(setf PrefixCharacterList NIL)

% Add a new prefix character and associated prompt.
(DE define_prefix_character (chr prompt-string)
  (setf PrefixCharacterList
    (cons (cons chr prompt-string) PrefixCharacterList)))

% Set up initial list of valid prefix characters.  Note that ESC (etc?)
% aren't implemented as "prefix characters", (although, perhaps they should
% be?)  NOTE: there seems to be something wrong in that we're using this
% general tool for only one prefix character.  (Note that M-X is not a
% prefix character.)
(define_prefix_character (char (cntrl X)) "C-X ")

% Generate a list of character codes, or a single character, from a list of
% "character descriptors".  Syntax is similar to that for the "Char"
% macro.
(DM CharSequence (chlist)
  (prog (processed-list)
    (setf processed-list
      (for (in chr-descriptor (cdr chlist))
        (collect (DoChar chr-descriptor))))

    % If there was a single character in the list, just return the
    % character code.
    (return
      (cond
        % Just return the character code if a single character.
        ((equal (length processed-list) 1)
          (car processed-list))
        % Otherwise, return the (quoted) list of character codes.
        (T
          `(quote ,processed-list))))))

% Return T if character has meta bit set.
(DS MetaP (chr)
  (GreaterP chr 127))

% Convert character to meta-character.
(DS MakeMeta (chr)
  (LOR chr 8#200))

% Return character with meta bit "stripped off"--converts meta to normal char.
(DS UnMeta (chr)
  (LAND chr 8#177))

% This version of "UpperCaseP" also handles meta-characters.
(DE X-UpperCaseP (chr)
  (cond
    ((MetaP chr)
      (UpperCaseP (UnMeta chr)))
    (T
      (UpperCaseP chr))))

(DE X-Char-DownCase (chr)
  (cond
    ((MetaP chr)
      (MakeMeta (Char-DownCase (UnMeta chr))))
    (T
      (Char-DownCase chr))))

% Set up a "clear" dispatch table.
(DE ClearDispatch ()
  (progn
    (for (from i 0 255 1)
      (do (Undefine i)))
    (setf PrefixAssociationLists NIL)))

% Set up the keyboard dispatch table for a character or "extended character".
% If the character is uppercase, define the equivalent lower case character
% also.
(DE SetKey (xchar op)
  (cond
    ((NumberP xchar)     % Add table entry for a simple character code.
      (progn
        (setf (indx MainDispatch xchar) op)
        (cond
          ((X-UpperCaseP xchar)
            (setf (indx MainDispatch (X-Char-DownCase xchar)) op)))))

    % If a valid prefixed character.
    ((and (PairP xchar) (Atsoc (car xchar) PrefixCharacterList))
      (prog (prefix-char assoc-entry)
        (setf prefix-char (car xchar))

        % Look up the prefix character in the a-list of a-lists.
        (setf assoc-entry (Atsoc prefix-char PrefixAssociationLists))

        % Add the prefix character if no entry present yet. 
        (cond
          ((null assoc-entry)
              (setf PrefixAssociationLists
                (cons
                  (setf assoc-entry (cons prefix-char NIL))
                  PrefixAssociationLists))))

        % Now, add the prefixed character to the association list.  Note
        % that in case of duplicate entries the last one added is the one
        % that counts.  (Perhaps we should go to a little more work and
        % DelQIP any old entry?)
        (RPLACD assoc-entry
          % (cadr xchar) is the prefixed character.
          (cons (cons (cadr xchar) op) (cdr assoc-entry)))

        % Define the lower case version of the character, if relevent. 
        (cond
          ((X-UpperCaseP (cadr xchar))
            (RPLACD assoc-entry
              (cons (cons
                      (X-Char-DownCase (cadr xchar))
                      op)
                (cdr assoc-entry)))))))

    % If we get here, SetKey was given a bad argument
    (T
      % (Use EMODEerror instead?)
      (Error 666 "Bad argument for SetKey"))))

% Procedure to define a character as "self inserting".
(DE MakeSelfInserting (chr)
  (SetKey chr 'InsertSelfCharacter))

% Define a character so that it just "dings" bell.
(DE Undefine (chr)
  (SetKey chr 'Ding))

(FLUID '(new-oper))

% Dispatch on next command character, "remember" the associated operation.
(DE Dispatcher ()
  (progn
    (Dispatch (GetNextCommandCharacter))
    (setf last_operation new-oper)))

% Dispatch on a character, "remember" the associated dispatch routine.
(DE Dispatch (chr)
  (prog (oper)
    (setf oper (indx MainDispatch chr))
    (setf new-oper oper)
    (apply oper NIL)))

% Read another character, and then perform appropriate operation from
% appropriate prefix "table" (association list).
(DE do-prefix ()
  (prog (prefix-entry char-entry chr)
    (setf prefix-entry (atsoc SelfInsertCharacter PrefixAssociationLists))
    (cond
      % "Complain" if no entry.
      ((null prefix-entry)
        (ding))

      % Otherwise, read a character and look up its entry.
      (T
        (setf chr
          (prompt_for_character
            % Prompt string for prefix
            (cdr (Atsoc SelfInsertCharacter PrefixCharacterList))))

        (setf char-entry (Atsoc chr prefix-entry))
        (cond
          ((null char-entry)
            (progn
              % Make note of the fact that we ding!
              (setf new-oper 'ding)
              (ding)))
          (T
            (apply (setf new-oper (cdr char-entry)) NIL)))))))

% Treat next command character" as "Meta-character".  (This routine is
% normally invoked by the "escape" character.)
(DE EscapeAsMeta ()
  (dispatch (LOR 8#200 (prompt_for_character "M-"))))

% Treat the next character as a "control-meta-character".  (This routine is
% normally invoked by cntrl-Z.)
(DE DoControlMeta ()
  (dispatch (LOR 8#200 (LAND 8#37 (prompt_for_character "M-C-")))))


(FLUID '(pushed_back_characters))

% Get command character, processing keyboard macros (someday! ), etc.
% Parity mask is used to clear "parity bit" for those terminals that don't
% have a meta key.  It should be 8#177 in that case.  Should be 8#377 for
% terminals with a meta key.  (Probably the wrong place to do this--if we
% also expect to handle keyboard macros! )
(DE GetNextCommandCharacter ()
  (cond
    % re-read any pushed back stuff.
    (pushed_back_characters
      (progn
        (setf SelfInsertCharacter (car pushed_back_characters))
        (setf pushed_back_characters (cdr pushed_back_characters))))

    (T
      (setf SelfInsertCharacter (Land parity_mask (PBIN))))))

% "Push back" a character.
(DE push_back (chr)
  (setf pushed_back_characters (cons chr pushed_back_characters)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Manipulating mode tables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Set up dispatch table for current buffer, by evaluating the expressions
% in ModeEstablishExpressions.
(De EstablishCurrentMode ()
  (progn
    (ClearDispatch)

    % Use reverse so things on front of list are evaluated last.  (So that
    % later incremental changes are added later.)
    (for (in x (reverse ModeEstablishExpressions))
      (do
        (cond
          ((pairp x) (eval x))
          (t
            (error 667
              (bldmsg
            "%r is not a valid ""mode establish expression"" (non-list)"))))))

    % csp 7/782
    % Prefix chars are totally global anyway, so let them be
    %  established here, and let them override regular key defns.
    (DefinePrefixChars)))

% This list of (character-sequence . operation) defines a partial set
% of bindings for text mode (and other derived modes).  This list
% contains only commands that don't modify the buffer.

(setf ReadOnlyTextDispatchList (list

    % These commands are read-only commands for text mode.

    (cons (char (cntrl @)) 'SetMark)
    (cons (char (cntrl A)) '$BeginningOfLine)
    (cons (char (cntrl B)) '$BackwardCharacter)
    (cons (char (cntrl E)) '$EndOfLine)
    (cons (char (cntrl F)) '$ForwardCharacter)
    (cons (char (cntrl N)) '$ForwardLine)
    (cons (char (cntrl P)) '$BackwardLine)
    (cons (char (cntrl R)) 'reverse_string_search)
    (cons (char (cntrl S)) 'forward_string_search)
    (cons (char (cntrl V)) 'scroll-window-up-page-command)
    (cons (char (meta (cntrl B))) 'backward_sexpr)
    (cons (char (meta (cntrl F))) 'forward_sexpr)
    (cons (char (meta B)) 'backward_word)
    (cons (char (meta F)) 'forward_word)
    (cons (char (meta V)) 'scroll-window-down-page-command)
    (cons (char (meta W)) 'copy_region)
    (cons (char (meta <)) '$BeginningOfBuffer)
    (cons (char (meta >)) '$EndOfBuffer)
    (cons (CharSequence (cntrl X) (cntrl X)) 'ExchangePointAndMark)

    % Note that these two would be nice to have for other "data modes" than
    % text.  But current versions aren't generic enough.
    (cons (CharSequence (cntrl X) 1) 'OneWindow)
    (cons (CharSequence (cntrl X) 2) 'TwoRfaceWindows)
    ))

% This list of (character-sequence .  operation) defines bindings for text mode
% (and other derived modes).  TextDispatchList includes the initial contents of
% ReadOnlyTextDispatchList (above).  Be sure to put read-only commands on that
% list!

(setf TextDispatchList
  (append
    (list
      (cons (char !)) 'insert_matching_paren)
      (cons (char (cntrl D)) '$DeleteForwardCharacter)
      (cons (char (cntrl K)) 'kill_line)
      (cons (char (cntrl O)) 'OpenLine)
      (cons (char (cntrl Q)) 'InsertNextCharacter)
      (cons (char (cntrl T)) 'transpose_characters)
      (cons (char (cntrl W)) 'kill_region)
      (cons (char (cntrl Y)) 'insert_kill_buffer)
      (cons (char (meta (cntrl K))) 'kill_forward_sexpr)
      (cons (char (meta (cntrl RUBOUT))) 'kill_backward_sexpr)
      (cons (char (meta D)) 'kill_forward_word)
      (cons (char (meta Y)) 'unkill_previous)
      (cons (char (meta RUBOUT)) 'kill_backward_word)
      (cons (char DELETE) '$DeleteBackwardCharacter)
      (cons (char LF) '$CRLF)
      (cons (char CR) '$CRLF)
      (cons (char (meta !%)) 'Query-Replace-Command)
      (cons (CharSequence (cntrl X) (cntrl R)) 'CntrlXread)
      (cons (CharSequence (cntrl X) (cntrl S)) 'save_file)
      (cons (CharSequence (cntrl X) (cntrl W)) 'CntrlXwrite)
      )

    ReadOnlyTextDispatchList
    ))

% Add the (chr opr) binding to a list with name listname.
(de AddToKeyList (listname chr opr)
  (let*
    ((old-list (eval listname))
      (old-binding (atsoc chr old-list))
      (binding (cons chr opr)))
    (cond
      % If the binding isn't already in the a-list.
      ((null old-binding)
        % Add the new binding (Destructively to the end, so it's sure to
        % override any old stuff).
        (set listname (aconc old-list binding)))

      % Otherwise, replace the old operation in the binding.
      (T
        (setf (cdr old-binding) opr)))

    % Update the current mode if EMODE is running, in case it's affected by
    % the list we just modified.
    (cond
      (*EMODE
        (EstablishCurrentMode)))))

% Add a new key binding to "text mode".
(de SetTextKey (chr opr)
  (AddToKeyList 'TextDispatchList chr opr))

% Add a new key binding to "Lisp mode".
(de SetLispKey (chr opr)
  (AddToKeyList 'LispDispatchList chr opr))

% Execute the expressions in this list to establish "Fundamental Text Mode".
(setf FundamentalTextMode
  '((SetKeys TextDispatchList)
     (SetKeys BasicDispatchList)
     (NormalSelfInserts)))

(de SetKeys (lis)
  (for (in x lis) (do (SetKey (car x) (cdr x)))))

(de NormalSelfInserts ()
  (for (from i 32 126) (do (MakeSelfInserting i))))

(setf BasicDispatchList
  (list
	(cons (char ESC) 'EscapeAsMeta)
	(cons (char (cntrl U)) '$Iterate)
	(cons (char (cntrl Z)) 'DoControlMeta)

	% NOT basic?
	(cons (CharSequence (cntrl X) (cntrl B)) 'PrintBufferNames)
	(cons (CharSequence (cntrl X) B) 'ChooseBuffer)

%Dired stuff commented out for now.
%?	(cons (CharSequence (cntrl X) D) 'dired-command)

% window-kill-buffer not implemented yet?
%?	(cons (CharSequence (cntrl X) K) 'window-kill-buffer)

        % "C-X N" switches to "next window" (or "other window" if in "two
        % window mode").
        (cons (CharSequence (cntrl X) N) 'next_window)
        % "C-X O" does the same as "C-X N"
	(cons (CharSequence (cntrl X) O) 'next_window)

        % "C-X P" moves to "previous window".
        (cons (CharSequence (cntrl X) P) 'previous_window_command)

        % C-X C-Z causes us to exit to monitor.
        (cons (CharSequence (cntrl X) (cntrl Z)) 'QUIT)

        % M-C-Z causes us to rebind the channels for "normal" I/O, and
        % leave EMODE.
        (cons (char (meta (cntrl Z))) 'OldFace)

%Dired stuff commented out for now.
%?	(cons (char (meta (cntrl L))) 'SelectPreviousBuffer)

	(cons (char (cntrl L)) 'FullRefresh)

	% Two ways to invoke the help function.
	(cons (char (meta !/ )) '$HelpDispatch)
	(cons (char (meta !?)) '$HelpDispatch)

        (cons (CharSequence (cntrl X) (cntrl F)) 'find_file)

        (cons (CharSequence (cntrl X) (cntrl P)) 'WriteScreenPhoto)
        (cons (char (meta X)) 'execute_command)))

% Define the prefix characters given in PrefixCharacterList.
(de DefinePrefixChars ()
    (for (in prefix-entry PrefixCharacterList)
      (do
        % car gives character code for prefix.
        (SetKey (car prefix-entry) 'do-prefix))))

% IS THE FOLLOWING REALLY APPROPRIATE TO DISPATCH?

% Simulate EMACS's C-U, C-U meaning 4, C-U C-U meaning 16, etc., and C-U
% <integer> meaning <integer>.  This command suffers from the flaw of
% simply iterating the following command, instead of giving it a
% parameter.  Thus, for example, C-U C-A won't do what you expect.
%  Written by Alan Snyder, HP labs.

(fluid '(prompt-immediately prompt-was-output))

% C-U handler.
(de $iterate ()
  (let ((arg 1)
	(ch (char (control U)))
	(previous-ch nil)
	(prompt "")
	(prompt-immediately nil)
       )
    (while T
	(cond ((eqn ch (char (control U)))
	       (if previous-ch (setq prompt (concat prompt " ")))
	       (setq prompt (concat prompt "C-U"))
	       (setq arg (times arg 4))
	       )
              % Note check for non-meta character.  (Since DigitP blows up
              % otherwise?  Test may be obsolete??)
              ((and (LessP ch 128) (digitp ch))
	       (if (and previous-ch (digitp previous-ch))
		   (setq arg (plus (times arg 10) (char-digit ch)))
		   % ELSE
		   (setq arg (char-digit ch))
		   (setq prompt (concat prompt " "))
		   )
	       (setq prompt (concat prompt (string ch)))
	       )
	      (t (exit)))
	(setq previous-ch ch)
	(setq ch (prompt_for_character prompt))
	(setq prompt-immediately prompt-was-output)
	)
    (for (from i 1 arg 1)
         (do (dispatch ch)
             % NOTE KLUDGE!  Need to work this out better!
             (setf last_operation new-oper)))
    ))

% Convert from character code to digit.
(de char-digit (c)
  (cond ((digitp c) (difference (char-int c) (char-int (char 0))))))
