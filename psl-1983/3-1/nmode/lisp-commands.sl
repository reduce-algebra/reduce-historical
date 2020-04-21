%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Lisp-Commands.SL - Miscellaneous NMODE Lisp-related commands
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        12 November 1982
% Revised:     18 February 1983
%
% 18-Feb-83 Alan Snyder
%  Rename down-list to down-list-command; extend to obey the command argument.
%  Rename insert-parens to make-parens-command; extend to obey the command
%  argument and to insert a space if needed (like EMACS).  Rename
%  move-over-paren to move-over-paren-command; revise to follow EMACS more
%  closely.  Remove use of "obsolete" #\ names.
% 12-Nov-82 Alan Snyder
%  This file is the result of a complete rewrite of the Lisp stuff.  The only
%  things that remain in this file are those things that don't fit in elsewhere.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects fast-int))

(fluid '(nmode-command-argument
	 nmode-command-argument-given
	 nmode-current-command
	 ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de insert-closing-bracket ()
  % Insert a closing bracket, then display the matching opening bracket.
  (if (not (fixp nmode-current-command))
    (Ding)
    % otherwise
    (for (from i 1 nmode-command-argument)
	 (do (insert-character nmode-current-command)))
    (display-matching-opener)
    ))

(de down-list-command ()
  % Move inside the next or previous contained list.  If the command argument
  % is positive, move forward past the next open bracket without an
  % intervening close bracket.  If the command argument is negative, move
  % backward to the next previous close bracket without an intervening open
  % bracket.  If the specified bracket cannot be found, Ding, but do not move.

  % Note: this command differs from the EMACS Down-List command in that it
  % always stays within the current list.  The EMACS command moves forward
  % as far as needed to find a list at the next lower level.

  (if (> nmode-command-argument 0)
    (for (from i 1 nmode-command-argument)
	 (do (when (not (move-forward-down-list)) (Ding) (exit))))
    (for (from i 1 (- nmode-command-argument))
	 (do (when (not (move-backward-down-list)) (Ding) (exit))))
    ))

(de make-parens-command ()
  % Insert a space if it looks like we need one.  Insert an open paren.  Skip
  % forward over the requested number of forms, if any.  Insert a close paren.
  % Move back to the open paren.

  (when (not (at-line-start?))
    (let ((ch (previous-character)))
      (when (and (not (char-blank? ch)) (not (= ch #/( )))
	(insert-character #\Space)
	)))
  (insert-character #/( )
  (let ((old-pos (buffer-get-position)))
    (when nmode-command-argument-given
      (if (or (<= nmode-command-argument 0)
	      (not (move-over-forms nmode-command-argument)))
	(Ding)))
    (insert-character #/) )
    (buffer-set-position old-pos)
    ))

(de move-over-paren-command ()
  % Move forward past N closing brackets at any level.  Delete any indentation
  % before the first closing bracket found.  Insert an end of line after the
  % last closing bracket found and indent the new line.  Aside: This
  % definition follows EMACS.  I don't understand the motivation for this way
  % of interpreting the command argument.

  (if (<= nmode-command-argument 0)
    (Ding)
    (for (from i 1 nmode-command-argument)
	 (do
	  (when (not (forward-scan-for-right-paren 10000))
	    (when (> i 1)
	      (insert-eol)
	      (lisp-indent-current-line)
	      )
	    (Ding)
	    (exit)
	    )
	  (when (= i 1)
	    (move-backward-item)
	    (strip-previous-blanks)
	    (move-forward-item)
	    )
	  (when (= i nmode-command-argument)
	    (insert-eol)
	    (lisp-indent-current-line)
	    )
	  ))))

(de insert-comment-command ()
  (move-to-end-of-line)
  (insert-string "% ")
  )
