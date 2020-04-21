%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Kill-Commands.SL - NMODE Kill and Delete commands
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        23 August 1982
% Revised:     16 November 1982
%
% 16-Nov-82 Alan Snyder
%   Modified C-Y and M-Y to obey comamnd argument.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects fast-vectors fast-int))
(load gsort)

(fluid '(nmode-current-buffer nmode-command-argument
	 nmode-command-argument-given nmode-command-number-given
	 nmode-previous-command-killed nmode-command-killed
	 ))

% Internal static variables:

(fluid '(nmode-kill-ring))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-initialize-kill-ring ()
  (setf nmode-kill-ring (ring-buffer-create 16))
  (setf nmode-command-killed NIL)
  )

(de insert-kill-buffer ()
  % Insert the specified "kill buffer" into the buffer at the current location.
  (cond
   ((<= nmode-command-argument 0)
    (Ding))
   (nmode-command-number-given
    (insert-from-kill-ring (+ (- nmode-command-argument) 1) NIL))
   (nmode-command-argument-given
    (insert-from-kill-ring 0 T))
   (t
    (insert-from-kill-ring 0 NIL))
   ))
   
(de insert-from-kill-ring (index flip-positions)
  (insert-text-safely (=> nmode-kill-ring fetch index) flip-positions)
  )

(de insert-text-safely (text flip-positions)
  (cond (text
	 (=> nmode-current-buffer set-mark-from-point)
	 (insert-text text)
	 (when flip-positions (exchange-point-and-mark))
	 )
	(t (Ding))
	))

(de safe-to-unkill ()
  % Return T if the current region contains the same text as the current
  % kill buffer.

  (let ((killed-text (ring-buffer-top nmode-kill-ring))
	(region (extract-text NIL (buffer-get-position) (current-mark)))
	)
    (and killed-text (text-equal killed-text region))
    ))

(de unkill-previous ()
  % Delete (without saving away) the current region, and then unkill (yank) the
  % specified entry in the kill ring.  "Ding" if the current region does not
  % contain the same text as the current entry in the kill ring.

  (cond ((not (safe-to-unkill))
	 (Ding))
	((= nmode-command-argument 0)
	 (extract-region T (buffer-get-position) (current-mark)))
	(t
	 (extract-region T (buffer-get-position) (current-mark))
	 (=> nmode-kill-ring rotate (- nmode-command-argument))
	 (insert-from-kill-ring 0 NIL)
	 )
	))

(de update-kill-buffer (kill-info)
  % Update the "kill buffer", either appending/prepending to the current
  % buffer, or "pushing" the kill ring, as appropriate.  kill-info is a pair,
  % the car of which is +1 if the text was "forward killed", and -1 if
  % "backwards killed".  The cdr is the actual text (a vector of strings).

  (let ((killed-text (cdr kill-info))
	(dir (car kill-info))
	)
    (if (not nmode-previous-command-killed)
      % If previous command wasn't a kill, then "push" the new text.
      (ring-buffer-push nmode-kill-ring killed-text)
      % Otherwise, append or prepend the text, as appropriate.
      (let ((text (ring-buffer-top nmode-kill-ring)))
        % Swap the two pieces of text if deletion was "backwards".
	(if (< dir 0) (psetf text killed-text killed-text text))
	% Replace text with the concatenation of the two.
	(ring-buffer-pop nmode-kill-ring)
	(ring-buffer-push nmode-kill-ring (text-append text killed-text))
	))))

(de text-append (t1 t2)
  % Append two text-vectors.
  % The last line of T1 is concatenated with the first line of T2.
  (let ((text (MkVect (+ (vector-upper-bound t1) (vector-upper-bound t2))))
	(ti 0) % index into TEXT
	)
    (for (from i 0 (- (vector-upper-bound t1) 1))
	 (do (vector-store text ti (vector-fetch t1 i))
	     (setf ti (+ ti 1))
	     ))
    (vector-store text ti
      (string-concat (vector-fetch t1 (vector-upper-bound t1))
		     (vector-fetch t2 0)))
    (setf ti (+ ti 1))
    (for (from i 1 (vector-upper-bound t2))
	 (do (vector-store text ti (vector-fetch t2 i))
	     (setf ti (+ ti 1))
	     ))
    text))

(de text-equal (t1 t2)
  % Compare two text vectors for equality.
  (let ((limit (vector-upper-bound t1)))
    (and (= limit (vector-upper-bound t2))
	 (for (from i 0 limit)
	      (always (string= (vector-fetch t1 i) (vector-fetch t2 i)))
	      ))))

(de kill-region ()
  % Kill (and save in kill buffer) the region between point and mark.
  (update-kill-buffer (extract-region T (buffer-get-position) (current-mark)))
  (setf nmode-command-killed T)
  )

(de copy-region ()
  (update-kill-buffer (extract-region NIL (buffer-get-position) (current-mark)))
  )

(de append-to-buffer-command ()
  (let* ((text (cdr (extract-region NIL (buffer-get-position) (current-mark))))
	 (b (prompt-for-buffer "Append Region to Buffer: " NIL))
	 )
    (=> b insert-text text)
    ))

(de prompt-for-register-name (prompt)
  % Prompt for the name of a "Register", which must be a letter
  % or a digit.  Return the corresponding Lisp Symbol.  Return NIL
  % if an invalid name is given.

  (nmode-set-delayed-prompt prompt)
  (let ((ch (input-base-character)))
    (cond ((AlphaNumericP ch)
	   (intern (string-concat "nmode-register-" (string ch))))
	  (t (Ding) NIL))))

(de put-register-command ()
  (let ((register (prompt-for-register-name
		   (if nmode-command-argument-given
		       "Withdraw Region to Register: "
		       "Copy Region to Register: "))))
    (cond (register
	   (set register (cdr (extract-region nmode-command-argument-given
					      (buffer-get-position)
					      (current-mark))))
	   ))))

(de get-register-command ()
  (let ((register (prompt-for-register-name "Insert from Register: "))
	(old-pos (buffer-get-position))
	)
    (cond (register
	   (cond ((BoundP register)
		  (insert-text (ValueCell register))
		  (set-mark-from-point)
		  (buffer-set-position old-pos)
		  (if nmode-command-argument-given
		      (exchange-point-and-mark))
		  )
		 (t (Ding))
		 )))))

(de append-next-kill-command ()
  (if (ring-buffer-top nmode-kill-ring) % If there is a kill buffer...
    (setf nmode-command-killed T)
    ))

(de kill-line ()
  (let ((old-pos (buffer-get-position)))
    (if nmode-command-argument-given
      (cond ((> nmode-command-argument 0)
	     % Kill through that many line terminators
	     (for (from i 1 nmode-command-argument)
		  (do (move-to-next-line)))
	     )
	    ((= nmode-command-argument 0)
	     % Kill preceding text on this line
	     (move-to-start-of-line)
	     )
	    (t
	     % Kill through that many previous line starts
	     % This line counts only if we are not at the beginning of it.
	     (if (not (at-line-start?))
		(progn
		  (move-to-start-of-line)
		  (setf nmode-command-argument (+ nmode-command-argument 1))
		  ))
	     (for (from i 1 (- nmode-command-argument))
		  (do (move-to-previous-line)))
	     ))
      % else (no argument given)
      (while (char-blank? (next-character))
	(move-forward))
      (if (at-line-end?)
        (move-to-next-line)
        (move-to-end-of-line)
        )
      )
    (update-kill-buffer (extract-region T old-pos (buffer-get-position)))
    (setf nmode-command-killed T)
    ))

(de kill-forward-word-command ()
  (delete-words nmode-command-argument)
  (setf nmode-command-killed T)
  )

(de kill-backward-word-command ()
  (delete-words (- nmode-command-argument))
  (setf nmode-command-killed T)
  )

(de kill-forward-form-command ()
  (delete-forms nmode-command-argument)
  (setf nmode-command-killed T)
  )

(de kill-backward-form-command ()
  (delete-forms (- nmode-command-argument))
  (setf nmode-command-killed T)
  )

(de delete-backward-character-command ()
  (cond 
    (nmode-command-argument-given
      (delete-characters (- nmode-command-argument))
      (setf nmode-command-killed T))
    (t
      (if (at-buffer-start?)
	(Ding)
	(delete-previous-character)
	))))

(de delete-forward-character-command ()
  (cond 
    (nmode-command-argument-given
      (delete-characters nmode-command-argument)
      (setf nmode-command-killed T))
    (t
      (if (at-buffer-end?)
	(Ding)
	(delete-next-character)
	))))

(de delete-backward-hacking-tabs-command ()
  (cond 
    (nmode-command-argument-given
      (delete-characters-hacking-tabs (- nmode-command-argument))
      (setf nmode-command-killed T))
    (t
      (if (at-buffer-start?)
	(Ding)
	(move-backward-character-hacking-tabs)
	(delete-next-character)
	))))

(de transpose-words ()
  (let ((old-pos (buffer-get-position)))
    (cond ((not (attempt-to-transpose-words nmode-command-argument))
	   (Ding)
	   (buffer-set-position old-pos)
	   ))))

(de attempt-to-transpose-words (n)
  % Returns non-NIL if successful.
  (prog (bp1 bp2 bp3 bp4 word1 word2)
    (cond ((= n 0)
	   (setf bp1 (buffer-get-position))
	   (if (not (move-forward-word)) (return NIL))
	   (setf bp2 (buffer-get-position))
	   (buffer-set-position (current-mark))
	   (setf bp3 (buffer-get-position))
	   (if (not (move-forward-word)) (return NIL))
	   (setf bp4 (buffer-get-position))
	   (exchange-regions bp3 bp4 bp1 bp2)
	   (move-backward-word)
	   )
	  (t
	   (if (not (move-backward-word)) (return NIL))
	   (setf bp1 (buffer-get-position))
	   (if (not (move-forward-word)) (return NIL))
	   (setf bp2 (buffer-get-position))
	   (if (not (move-over-words (if (< n 0) (- n 1) n))) (return NIL))
	   (setf bp4 (buffer-get-position))
	   (if (not (move-over-words (- 0 n))) (return NIL))
	   (setf bp3 (buffer-get-position))
	   (exchange-regions bp1 bp2 bp3 bp4)
	   ))
    (return T)
    ))

(de transpose-lines ()
  (let ((old-pos (buffer-get-position)))
    (cond ((not (attempt-to-transpose-lines nmode-command-argument))
	   (Ding)
	   (buffer-set-position old-pos)
	   ))))

(de attempt-to-transpose-lines (n)
  % Returns non-NIL if successful.
  (prog (bp1 bp2 bp3 bp4 line1 line2 current marked last)
    (setf current (current-line-pos))
    (setf last (- (current-buffer-size) 1))
    % The last line doesn't count, because it is unterminated.
    (setf marked (buffer-position-line (current-mark)))
    (cond ((= n 0)
	   (if (or (>= current last) (>= marked last)) (return NIL))
	   (setf bp1 (buffer-position-create current 0))
	   (setf bp2 (buffer-position-create (+ current 1) 0))
	   (setf bp3 (buffer-position-create marked 0))
	   (setf bp4 (buffer-position-create (+ marked 1) 0))
	   (exchange-regions bp3 bp4 bp1 bp2)
	   (move-to-previous-line)
	   )
	  (t
	   % Dragged line is the previous one.
	   (if (= current 0) (return NIL))
	   (setf bp1 (buffer-position-create (- current 1) 0))
	   (setf bp2 (buffer-position-create current 0))
	   (setf marked (- (+ current n) 1))
	   (if (or (< marked 0) (>= marked last)) (return NIL))
	   (setf bp3 (buffer-position-create marked 0))
	   (setf bp4 (buffer-position-create (+ marked 1) 0))
	   (exchange-regions bp1 bp2 bp3 bp4)
	   ))
    (return T)
    ))

(de transpose-forms ()
  (let ((old-pos (buffer-get-position)))
    (cond ((not (attempt-to-transpose-forms nmode-command-argument))
	   (Ding)
	   (buffer-set-position old-pos)
	   ))))

(de attempt-to-transpose-forms (n)
  % Returns non-NIL if successful.
  (prog (bp1 bp2 bp3 bp4 form1 form2)
    (cond ((= n 0)
	   (setf bp1 (buffer-get-position))
	   (if (not (move-forward-form)) (return NIL))
	   (setf bp2 (buffer-get-position))
	   (buffer-set-position (current-mark))
	   (setf bp3 (buffer-get-position))
	   (if (not (move-forward-form)) (return NIL))
	   (setf bp4 (buffer-get-position))
	   (exchange-regions bp3 bp4 bp1 bp2)
	   (move-backward-form)
	   )
	  (t
	   (if (not (move-backward-form)) (return NIL))
	   (setf bp1 (buffer-get-position))
	   (if (not (move-forward-form)) (return NIL))
	   (setf bp2 (buffer-get-position))
	   (if (not (move-over-forms (if (< n 0) (- n 1) n))) (return NIL))
	   (setf bp4 (buffer-get-position))
	   (if (not (move-over-forms (- 0 n))) (return NIL))
	   (setf bp3 (buffer-get-position))
	   (exchange-regions bp1 bp2 bp3 bp4)
	   ))
    (return T)
    ))

(de transpose-regions ()
  (let ((old-pos (buffer-get-position)))
    (cond ((not (attempt-to-transpose-regions nmode-command-argument))
	   (Ding)
	   (buffer-set-position old-pos)
	   ))))

(de attempt-to-transpose-regions (n)
  % Returns non-NIL if successful.
  % Transpose regions defined by cursor and three most recent marks.
  % EMACS resets all of the marks; we just reset the cursor to the
  % end of the higher region.

  (prog (bp1 bp2 bp3 bp4 bp-list)
    (setf bp1 (buffer-get-position))
    (setf bp2 (current-mark))
    (setf bp3 (previous-mark))
    (setf bp4 (previous-mark))
    (previous-mark)
    (setf bp-list (list bp1 bp2 bp3 bp4))
    (gsort bp-list (function buffer-position-lessp))
    (exchange-regions (first bp-list)
		      (second bp-list)
		      (third bp-list)
		      (fourth bp-list))
    (buffer-set-position (fourth bp-list))
    (return T)
    ))

% Support functions:

(de delete-characters (n)
  (let ((old-pos (buffer-get-position)))
    (move-over-characters n)
    (update-kill-buffer
      (extract-region T old-pos (buffer-get-position)))
    ))

(de delete-characters-hacking-tabs (n)

  % Note: EMACS doesn't try to hack tabs when deleting forward.
  % We do, but it's a crock.  What should really happen is that all
  % consecutive tabs are converted to spaces.

  (cond ((< n 0)
	 % Deleting backwards is tricky because the conversion of tabs to
	 % spaces may change the numeric value of the original "position".
	 % Our solution is to first move backwards the proper number of
	 % characters (converting tabs to spaces), and then move back over them.

	 (let ((count (- n)))
	   (setf n 0)
	   (while (and (> count 0)
		       (move-backward-character-hacking-tabs))
	     (setf count (- count 1))
	     (setf n (- n 1))
	     )
	   (move-over-characters (- n))
	   )))

  (let ((old-pos (buffer-get-position)))
    (move-over-characters-hacking-tabs n)
    (update-kill-buffer
      (extract-region T old-pos (buffer-get-position)))
    ))

(de delete-words (n)
  (let ((old-pos (buffer-get-position)))
    (move-over-words n)
    (update-kill-buffer
      (extract-region T old-pos (buffer-get-position)))
    ))

(de delete-forms (n)
  (let ((old-pos (buffer-get-position)))
    (move-over-forms n)
    (update-kill-buffer
      (extract-region T old-pos (buffer-get-position)))
    ))

(de exchange-regions (bp1 bp2 bp3 bp4)
  % The specified positions define two regions: R1=<BP1,BP2> and
  % R2=<BP3,BP4>.  These regions should not overlap, unless they
  % are identical.  The contents of the two regions will be exchanged.
  % The cursor will be moved to the right of the region R1 (in its new
  % position).

  (let ((dir (buffer-position-compare bp1 bp3))
	(r1 (cdr (extract-region NIL bp1 bp2)))
	(r2 (cdr (extract-region NIL bp3 bp4)))
	)
    (cond ((< dir 0) % R1 is before R2
	   (extract-region T bp3 bp4)
	   (insert-text r1)
	   (extract-region T bp1 bp2)
	   (insert-text r2)
	   (buffer-set-position bp4)
	   )
	  ((> dir 0) % R2 is before R1
	   (extract-region T bp1 bp2)
	   (insert-text r2)
	   (extract-region T bp3 bp4)
	   (insert-text r1)
	   ))
    ))
