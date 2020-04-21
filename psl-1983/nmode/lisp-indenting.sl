%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Lisp-Indenting.SL - NMODE Lisp Indenting commands
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        25 August 1982
% Revised:     12 November 1982
%
% 25-Feb-83 Alan Snyder
%  Move-down-list renamed to Move-forward-down-list.
% 12-Nov-82 Alan Snyder
%  Improved indenting using new structure-movement primitives.
%  Changed multi-line indenting commands to clear any blank lines.
%  Added LISP-INDENT-REGION-COMMAND.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects fast-int fast-vectors))

(fluid '(nmode-command-argument nmode-command-argument-given))

(de lisp-tab-command ()
  (cond (nmode-command-argument-given
	 (let ((n nmode-command-argument))
	   (cond ((< n 0)
		  (let ((last-line (- (current-line-pos) 1)))
		    (set-line-pos (+ (current-line-pos) n))
		    (let ((first-line (current-line-pos)))
		      (while (<= (current-line-pos) last-line)
			(lisp-indent-or-clear-current-line)
			(move-to-next-line)
			)
		      (current-buffer-goto first-line 0)
		      )))
		 ((> n 0)
		  (while (> n 0)
		    (lisp-indent-or-clear-current-line)
		    (move-to-next-line)
		    (if (at-buffer-end?) (exit))
		    (setf n (- n 1))
		    ))
		 (t
		  (lisp-indent-current-line)
		  (move-to-start-of-line)
		  ))))
	(t (lisp-indent-current-line))))

(de lisp-indent-current-line ()
  (indent-current-line (lisp-current-line-indent)))

(de lisp-indent-or-clear-current-line ()
  (indent-current-line
   (if (current-line-blank?)
     0
     (lisp-current-line-indent))))

(de lisp-indent-sexpr ()
  (if (not (move-forward-down-list)) % Find next open bracket
    (Ding) % None found
    % otherwise
    (move-backward-item) % Move back to the open bracket
    (let ((old-line (current-line-pos))
	  (old-point (current-char-pos))
	  )
      (if (not (move-forward-form)) % Find end of form
	(Ding) % No matching close bracket found
	% otherwise
	(for (from i (+ old-line 1) (current-line-pos))
	     (do
	      (set-line-pos i)
	      (lisp-indent-or-clear-current-line)
	      ))
	(current-buffer-goto old-line old-point)
	))))

(de lisp-indent-region-command ()
  (if nmode-command-argument-given
    (indent-region #'indent-to-argument)
    (indent-region #'lisp-indent-or-clear-current-line)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Basic Indenting Primitive
%
% This function determines what indentation the current line should receive.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de lisp-current-line-indent ()
  % Return the desired indentation for the current line.
  % Point is unchanged.
  (let ((old-pos (buffer-get-position)))
    (unwind-protect
     (unsafe-lisp-current-line-indent)
     (buffer-set-position old-pos)
     )))

(de unsafe-lisp-current-line-indent ()
  % Return the desired indentation for the current line.
  % Point may change.
  (move-to-start-of-line)
  (let ((item (move-backward-form))
	(number-of-forms 0)
	(leftmost-form-type NIL)
	)
    % If there are multiple forms at the same level of nesting
    % on the same line, we want to find the left-most one.
    (while (or (eq item 'ATOM) (eq item 'STRUCTURE))
      (setf number-of-forms (+ number-of-forms 1))
      (setf leftmost-form-type item)
      (let ((next-item (move-backward-form-within-line)))
	(if (not next-item) (exit)) % We have the first item on the line.
	(setf item next-item)
	))
    (selectq item
      ((ATOM STRUCTURE) (current-display-column)) % Line up with form.
      (OPENER (lisp-indent-under-paren leftmost-form-type number-of-forms))
      (t 0) % There is no previous form.
      )))

(de lisp-indent-under-paren (leftmost-form-type number-of-forms)
  % This function is called to determine the indentation for a line
  % that immediately follows (i.e., there is no intervening line
  % containing a form) the line containing the open paren that
  % begins the nesting level for the line being indented.  This
  % function is called with the current position being at the open
  % paren.  NUMBER-OF-FORMS specifies the number of forms that
  % follow the open paren on its line.  LEFTMOST-FORM-TYPE specifies
  % the type of the first such form (either ATOM, STRUCTURE, or NIL).

  (skip-prefixes) % Skip over any "prefix characters" (like ' in Lisp).
  (let ((paren-column (current-display-column))
	the-atom pos1 pos2 atom-text atom-string second-column
	)
    (if (not (eq leftmost-form-type 'ATOM))
      (+ paren-column 1)
      % Otherwise
      (move-forward-item) % Move past the paren.
      (setf pos1 (buffer-get-position))
      (move-forward-form) % Move past the first form.
      (setf pos2 (buffer-get-position))
      (setf atom-text (extract-text NIL pos1 pos2))
      (setf atom-string (string-upcase (vector-fetch atom-text 0)))
      (if (internp atom-string) (setf the-atom (intern atom-string)))
      (when (> number-of-forms 1)
	(move-forward-form)
	(move-backward-form)
	(setf second-column (current-display-column))
	)
      (lisp-indent-under-atom
       the-atom paren-column second-column number-of-forms)
      )))

(de lisp-indent-under-atom (the-id paren-column
				   second-column number-of-forms)
  % This function is called to determine the indentation for a line
  % that immediately follows (i.e., there is no intervening line
  % containing a form) the line containing the open paren that
  % begins the nesting level for the line being indented.
  % The open paren is followed on the same line by at least one form
  % that is not a structure.
  % NUMBER-OF-FORMS specifies the number of forms that
  % follow the open paren on its line.  If there are two or more forms,
  % then SECOND-COLUMN is the display column of the second form;
  % otherwise, SECOND-COLUMN is NIL.  If the first
  % form is recognized as being an
  % interned ID, then THE-ID is that ID; otherwise, THE-ID is NIL.
  % PAREN-COLUMN is the display column of the open paren.

  (or
   (if the-id (id-specific-indent the-id paren-column second-column))
   second-column
   (+ paren-column 1)
   ))

(put 'prog         'indentation 2)
(put 'lambda       'indentation 2)
(put 'lambdaq      'indentation 2)
(put 'while        'indentation 2)
(put 'de           'indentation 2)
(put 'defun        'indentation 2)
(put 'defmacro     'indentation 2)
(put 'df           'indentation 2)
(put 'dm           'indentation 2)
(put 'dn           'indentation 2)
(put 'ds           'indentation 2)
(put 'let          'indentation 2)
(put 'let*         'indentation 2)
(put 'if           'indentation 2)
(put 'when         'indentation 2)
(put 'unless       'indentation 2)
(put 'defmethod    'indentation 2)
(put 'defflavor    'indentation 2)
(put 'selectq      'indentation 2)
(put 'catch        'indentation 2)
(put 'catch-all    'indentation 2)
(put 'setf         'indentation 2)
(put 'setq         'indentation 2)

(de id-specific-indent (id paren-column second-column)

  % The default indentation for a pattern like this:
  %   .... (foo bar ...
  %             bletch ...
  % is to line up bletch with bar.  This pattern applies when FOO
  % is an atom (not a structure) and there is at least one
  % form (e.g. BAR) following it on the same line.  This function
  % is used to specify exceptions to this rule.  It is invoked
  % only when FOO is an INTERNed ID, since the exceptions are
  % defined by putting a property on the ID.

  (let ((indent (get id 'indentation)))
    (if indent (+ paren-column indent))
    ))
