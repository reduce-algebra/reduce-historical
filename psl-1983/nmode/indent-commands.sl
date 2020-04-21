%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Indent-commands.SL - NMODE indenting commands
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        24 August 1982
% Revised:     18 February 1983
%
% 18-Feb-83 Alan Snyder
%  Removed use of "obsolete" #\ names.
% 11-Nov-82 Alan Snyder
%  DELETE-INDENTATION-COMMAND (M-^) now obeys command argument.
%  INDENT-CURRENT-LINE now avoids modifying buffer if indentation unchanged.
%  Added INDENT-REGION stuff.
%  General clean-up.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects fast-int fast-strings extended-char common))
(load stringx)

(fluid '(nmode-command-argument
         nmode-command-argument-given
	 nmode-command-number-given
	 ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Indenting Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de indent-new-line-command ()
  (let ((func (dispatch-table-lookup (x-char CR))))
    (if func (apply func NIL)))
  (setf nmode-command-argument 1)
  (setf nmode-command-argument-given NIL)
  (setf nmode-command-number-given NIL)
  (let ((func (dispatch-table-lookup (x-char TAB))))
    (if func (apply func NIL))))

(de tab-to-tab-stop-command ()
  (for (from i 1 nmode-command-argument)
       (do (insert-character #\TAB))
       ))

(de delete-horizontal-space-command ()
  (while (and (not (at-line-end?)) (char-blank? (next-character)))
    (delete-next-character)
    )
  (while (and (not (at-line-start?)) (char-blank? (previous-character)))
    (delete-previous-character)
    )
  )

(de delete-blank-lines-command ()
  (cond ((current-line-blank?)
	 % We are on a blank line.
	 % Replace multiple blank lines with one.
	 % First, search backwards for the first blank line
	 % and save its index.
	 (while (not (current-line-is-first?))
	   (move-to-previous-line)
	   (cond ((not (current-line-blank?))
		  (move-to-next-line)
		  (exit))
		 ))
	 (delete-following-blank-lines)
	 )
	(t
	 % We are on a non-blank line.  Delete any blank lines
	 % that follow this one.
	 (delete-following-blank-lines)
	 )
	))

(de back-to-indentation-command ()
  (move-to-start-of-line)
  (while (char-blank? (next-character))
    (move-forward)
    ))

(de delete-indentation-command ()
  (if nmode-command-argument-given (move-to-next-line))
  (current-line-strip-indent)
  (move-to-start-of-line)
  (when (not (current-line-is-first?))
    (delete-previous-character)
    (if (and (not (at-line-start?))
	     (not (= (previous-character) #/( ))
	     (not (= (next-character) #/) ))
	     )
      (insert-character #\SPACE)
      )))

(de split-line-command ()
  (while (char-blank? (next-character))
    (move-forward))
  (if (> nmode-command-argument 0)
    (let ((pos (current-display-column)))
      (for (from i 1 nmode-command-argument)
	   (do (insert-eol)))
      (indent-current-line pos)
      )))

(de indent-region-command ()
  (if nmode-command-argument-given
    (indent-region #'indent-to-argument)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic Indenting Primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de char-blank? (ch)
  (or (= ch #\SPACE) (= ch #\TAB)))

(de current-line-indent ()
  % Return the indentation of the current line, in terms of spaces.

  (let ((line (current-line)))
    (for* (from i 0 (string-upper-bound line))
	  (with ch)
          (while (char-blank? (setf ch (string-fetch line i))))
          (sum (if (= ch #\TAB) 8 1))
          )))

(de current-line-strip-indent ()
  % Strip all leading blanks and tabs from the current line.

  (let ((line (current-line)))
    (for* (from i 0 (string-upper-bound line))
          (while (char-blank? (string-fetch line i)))
	  (finally
	   (when (> i 0)
	     (set-char-pos (- (current-char-pos) i))
	     (current-line-replace (string-rest line i))
	     ))
          )))

(de strip-previous-blanks ()
  % Strip all blanks and tabs before point.
  (while (and (not (at-buffer-start?))
	      (char-blank? (previous-character)))
    (delete-previous-character)
    ))

(de indent-current-line (n)
  % Adjust the current line to have the specified indentation.

  (when (and (~= n (current-line-indent)) (>= n 0))
    (current-line-strip-indent)
    (let ((n-spaces (remainder n 8))
	  (n-tabs (quotient n 8))
	  (line (current-line))
	  (cp (current-char-pos))
	  )
      (for (from i 1 n-spaces)
	   (do (setf line (string-concat #.(string #\SPACE) line))
	       (setf cp (+ 1 cp))))
      (for (from i 1 n-tabs)
	   (do (setf line (string-concat #.(string #\TAB) line))
	       (setf cp (+ 1 cp))))
      (current-line-replace line)
      (set-char-pos cp)
      )))

(de delete-following-blank-lines ()

  % Delete any blank lines that immediately follow the current one.

  (if (not (current-line-is-last?))
    (let ((old-pos (buffer-get-position))
	  first-pos
	  )
      % Advance past the current line until the next nonblank line.
      (move-to-next-line)
      (setf first-pos (buffer-get-position))
      (while (and (not (at-buffer-end?)) (current-line-blank?))
	(move-to-next-line))
      (extract-region T first-pos (buffer-get-position))
      (buffer-set-position old-pos)
      )))

(de indent-to-argument ()
  % Indent the current line to the position specified by nmode-command-argument.
  (indent-current-line nmode-command-argument)
  )

(de indent-region (indenting-function)
  % Indent the lines whose first characters are between point and mark.
  % Attempt to adjust point and mark appropriately should their lines
  % be re-indented.  The function INDENTING-FUNCTION is called to indent
  % the current line.

  (let* ((point (buffer-get-position))
	 (mark (current-mark))
	 (bp1 point)
	 (bp2 mark)
	 )
    (if (< 0 (buffer-position-compare bp1 bp2))
      (psetf bp1 mark bp2 point))
    (let ((first-line (buffer-position-line bp1))
	  (last-line (buffer-position-line bp2))
	  )
      (if (> (buffer-position-column bp1) 0)
	(setf first-line (+ first-line 1)))
      (for (from i first-line last-line)
	   (do
	    (set-line-pos i)
	    (cond
	     ((= i (buffer-position-line point))
	      (set-char-pos (buffer-position-column point)))
	     ((= i (buffer-position-line mark))
	      (set-char-pos (buffer-position-column mark)))
	     )
	    (apply indenting-function ())
	    (cond
	     ((= i (buffer-position-line point))
	      (setf point (buffer-position-create i (current-char-pos))))
	     ((= i (buffer-position-line mark))
	      (setf mark (buffer-position-create i (current-char-pos))))
	     ))))
    (previous-mark) % pop off old mark
    (set-mark mark) % push (possibly adjusted) mark
    (buffer-set-position point)
    ))
