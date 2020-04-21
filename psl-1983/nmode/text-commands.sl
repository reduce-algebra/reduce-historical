%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% TEXT-COMMANDS.SL - NMODE Sentence, Paragraph, Filling, and Formatting
%
% Author:      Jeff Soreff
%              Hewlett-Packard/CRC
% Date:        8 December 1982
% Revised:     1 February 1983
% Revised:     15 February 1983
%
% 15-Feb-83 Jeff Soreff
%  Bugs were removed from fill-comment-command and from next-char-list.
%      A test for arriving at a line end was added to fill-comment-command
%  in the while loop which locates the fill prefix to be used.  It fixed an
%  infinite loop in this while which occurred when one did a
%  fill-comment-command while on the last line in the buffer, if the
%  prefix-finding loop got to the buffer's end.  An at-line-end? test was used
%  instead of an at-buffer-end? test since the fill prefix found should never
%  go over a line.
%      In next-char-list the initialization of final-char-pos was changed
%  from 0 to char-count.  This removed a bug that led to setting the point
%  at the start of a prefixed line after a fill which moved point to the first
%  availible position on that new line.  Point should have been left AFTER the
%  prefix.  Changing the initialization of final-char-position allows
%  next-char-list to accurately account for the spaces taken up by the prefix,
%  since this count is passed to its char-count argument.
% 1-Feb-83 Alan Snyder
%  Changed literal ^L in source to #\FF.
% 30-Dec-82 Alan Snyder
%  Extended C-X = to set the current line number if a command number is
%  given.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load extended-char fast-strings fast-int))

(fluid '(nmode-current-buffer text-mode fill-prefix fill-column
nmode-command-argument nmode-command-argument-given nmode-command-number-given
nmode-command-killed sentence-terminators sentence-extenders))

(setf sentence-terminators '(#/! #/. #/?))
(setf sentence-extenders '(#/' #/" #/) #/]))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% User/Enhancer option sensitive function:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The text-justifier function may be altered if one wishes to have the
% same flexibility as EMACS'S TECO search strings provide.

(de text-justifier-command? ()
  % This function checks to see if the rest of the line is a text
  % justifier command. It returns a boolean and leaves point alone.
  (= (next-character) #/.))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start of Sentence Functions and Associated Support Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de valid-sentence-end? ()
  % This function checks that a sentence is followed by two blanks, a
  % newline or a blank and a newline.  It advances point one space.
  % It returns a boolean value.
  (if (at-line-end?) t
    (move-forward)
    (and (= (previous-character) #\blank)
	 (or (at-line-end?)(= (next-character) #\blank)))))

(de move-to-end-of-last-sentence ()
  % This function moves point to the end of the preceding sentence,
  % after extenders.  This function does not return a useful value
  (while (not
	  (or (at-buffer-start?)
	      (when		  
		% This when returns true if it hits a valid sentence end.
		(member (previous-character) sentence-terminators)
		(let ((scan-place (buffer-get-position)))
		  (while 
		    (member (next-character) sentence-extenders)
		    (move-forward))
		  (let* ((tentative-sentence-end (buffer-get-position))
			 (true-end (valid-sentence-end?)))
		    (buffer-set-position
		     (if true-end tentative-sentence-end scan-place))
		    true-end)))))
    (move-backward)))

(de start-of-last-sentence ()
  % This function restores point to its former place.  It returns the
  % location of the start of the preceding sentence.
  (let ((place (buffer-get-position))(start nil)(end nil))
    (move-to-end-of-last-sentence)
    (setf end (buffer-get-position))
    (skip-forward-blanks) % possibly past starting position this time
    (setf start (buffer-get-position))
    (when (buffer-position-lessp place start)
      (buffer-set-position end) % end of last sentence, after extenders
      (while % push back past extenders
	(member (previous-character) sentence-extenders)
	(move-backward))
      (move-backward) % push back past sentence terminator character
      (move-to-end-of-last-sentence)
      (skip-forward-blanks)
      (setf start (buffer-get-position)))
    (buffer-set-position place)
    start))

(de end-of-next-sentence ()
  % This function restores point to its former place.  It returns the
  % location of the end of the next sentence.
  (let ((place (buffer-get-position)))
    (while (not 
	    % the next sexp detects sentence ends and moves point to them
	    (or (at-buffer-end?)
		(when % note that this returns (valid-sentence-end?)'s value
		  (member (next-character) sentence-terminators)
		  (move-forward)
		  (while 
		    (member (next-character) sentence-extenders)
		    (move-forward))
		  (let ((tentative-sentence-end (buffer-get-position)))
		    (if (valid-sentence-end?)
		      (buffer-set-position tentative-sentence-end))))))
      (move-forward))
    (prog1 
     (buffer-get-position)
     (buffer-set-position place))))

(de forward-one-sentence ()
  % This function moves point to the end of the next sentence or
  % paragraph, whichever one is closer, and does not return a useful
  % value.
  (let ((sentence-end (end-of-next-sentence)))
    (if (at-line-end?)(move-forward)) % kludge to get around xtra newline
    (forward-one-paragraph)
    (if (at-line-start?)(move-backward)) % kludge to get around xtra newline
    (let ((paragraph-end (buffer-get-position)))
      (buffer-set-position
       (if (buffer-position-lessp sentence-end paragraph-end)
	 % "closer" is "earlier" or "before", in this case
	 sentence-end paragraph-end)))))

(de backward-one-sentence ()
  % This function moves point to the start of the preceding sentence
  % or paragraph, whichever one is closer. It does not return a useful
  % value
  (let ((sentence-start (start-of-last-sentence)))
    (skip-backward-blanks)
    (backward-one-paragraph)
    (skip-forward-blanks)
    (let ((paragraph-start (buffer-get-position)))
      (buffer-set-position
       (if (buffer-position-lessp sentence-start paragraph-start)
	 % "closer" is "later" or "after", in this case
	 paragraph-start sentence-start)))))

(de forward-sentence-command ()
  % If nmode-command-argument is positive this function moves point
  % forward by nmode-command-argument sentences , leaving it at the
  % end of a sentence.  If nmode-command-argument is negative it moves
  % backwards by abs(nmode-command-argument) sentences, leaving it at
  % the start of a sentence.  This function does not return a useful
  % value.
  (if (minusp nmode-command-argument)
    (for (from i 1 (- nmode-command-argument) 1)
	 (do (backward-one-sentence)))
    (for (from i 1 nmode-command-argument 1)
	 (do (forward-one-sentence)))))

(de backward-sentence-command ()
  % If nmode-command-argument is positive this function moves point
  % backward by nmode-command-argument sentences , leaving it at the
  % start of a sentence.  If nmode-command-argument is negative it
  % moves forwards by abs(nmode-command-argument) sentences, leaving
  % it at the end of a sentence.  This function does not return a
  % useful value.
  (if (minusp nmode-command-argument)
    (for (from i 1 (- nmode-command-argument) 1)
	 (do (forward-one-sentence)))
    (for (from i 1 nmode-command-argument 1)
	 (do (backward-one-sentence)))))

(de kill-sentence-command ()
  % This function kills whatever forward-sentence-command jumps over.
  % It leaves point after the killed text.  This function is sensitive
  % to the nmode command argument through forward-sentence-command.
  (let ((place (buffer-get-position)))
    (forward-sentence-command)
    (update-kill-buffer (extract-region t place (buffer-get-position)))
    (setf nmode-command-killed t)))

(de backward-kill-sentence-command ()
  % This function kills whatever backward-sentence-command jumps over.
  % It leaves point after the killed text.  This function is sensitive
  % to the nmode command argument through forward-sentence-command.
  (let ((place (buffer-get-position)))
    (backward-sentence-command)
    (update-kill-buffer (extract-region t place (buffer-get-position)))
    (setf nmode-command-killed t)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start of Paragraph Functions and Associated Support Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de rest-of-current-line-blank? () 
  % This function detects if the rest of the line is blank.  It
  % returns a boolean value.  It restores point.
  (let ((last-position (buffer-get-position)))
    (while (and (not (at-line-end?))
		(char-blank? (next-character)))
      (move-forward))
    (prog1 (at-line-end?)
	   (buffer-get-position last-position))))

(de mismatched-prefix? ()
  % This function checks to see if there is a fill prefix which
  % doesn't match the start of the current line.  It leaves point at
  % the start of the current line if there is a mismatch, or just
  % after the prefix if matched.  It returns t if there is a fill
  % prefix which does NOT match the line's start.
  (move-to-start-of-line)
  (when fill-prefix
    (let ((start-line (buffer-get-position)))
      (move-over-characters
       (string-length % count of characters in fill-prefix
	(getv fill-prefix 0)))
      (when (not (text-equal
		  (extract-text nil 
				start-line
				(buffer-get-position))
		  fill-prefix))
	(buffer-set-position start-line)
	t))))

(de pseudo-blank-line? ()
  % This function tests to see if the current line should be kept out
  % of paragraphs.  It tests for: lines which don't match an existing
  % fill prefix, blank lines, lines with only the fill prefix present,
  % text justifier commands, and properly prefixed text justifier
  % commands.  It only checks for the text justifier commands in text
  % mode.  It leaves point at the start of the current line and
  % returns a boolean value.
  (or (mismatched-prefix?)
      (prog1
       (or (and (text-justifier-command?)
		(eq text-mode (=> nmode-current-buffer mode)))
	   (rest-of-current-line-blank?))
       (move-to-start-of-line))))

(de pseudo-indented-line? ()
  % This function looks for page break characters or (in text mode)
  % indentation (after a fill prefix, if present) which signal the
  % start of a real paragraph. It always leaves point at the start of
  % the current line and returns a boolean.
  (prog1 (or
	  (= #\FF (next-character)) % page break character
	  (progn  (mismatched-prefix?)
		  (and (char-blank? (next-character))
		       (eq text-mode (=> nmode-current-buffer mode)))))
	 (move-to-start-of-line)))

(de start-line-paragraph? ()
  % This function tests the current line to see if it is the first
  % line (not counting an empty line) in a paragraph.  It leaves point
  % at the start of line and returns a boolean value.
  (and (not (pseudo-blank-line?))
       (or (pseudo-indented-line?)
	   % next sexp checks for a previous blank line
	   (if (current-line-is-first?)
	     t
	     (move-to-previous-line)
	     (prog1 
	      (pseudo-blank-line?)
	      (move-to-next-line))))))

(de end-line-paragraph? ()
  % This function tests the current line to see if it is the last line
  % in a paragraph.  It leaves point at the start of line and returns
  % a boolean value.
  (and (not (pseudo-blank-line?))
       % The next sexp checks for the two things on the next line of
       % text that can end a paragraph: a blank line or an indented
       % line which would start a new paragraph.
       (if (current-line-is-last?)
	 t
	 (move-to-next-line)
	 (prog1 
	  (or (pseudo-indented-line?)
	      (pseudo-blank-line?))
	  (move-to-previous-line)))))

(de forward-one-paragraph ()
  % This function moves point to the end of the next or current
  % paragraph, as EMACS defines it. This is either start of the line
  % after the last line with any characters or, if the paragraph
  % extends to the end of the buffer, then the end of the last line
  % with characters. This function returns a boolean which is true if
  % the function was stopped by a real paragraph end, rather than by
  % the buffer's end.
  (let ((true-end nil))
    (while (not (or (setf true-end (end-line-paragraph?))
		    (current-line-is-last?)))
      (move-to-next-line))
    (move-to-next-line)
    true-end))

(de forward-paragraph-command ()
  % If nmode-command-argument is positive this function moves point
  % forward by nmode-command-argument paragraphs , leaving it at the
  % end of a paragraph.  If nmode-command-argument is negative it moves
  % backwards by abs(nmode-command-argument) paragraphs, leaving it at
  % the start of a paragraph.  This function does not return a useful
  % value.
  (if (minusp nmode-command-argument)
    (for (from i 1 (- nmode-command-argument) 1)
	 (do (backward-one-paragraph)))
    (for (from i 1 nmode-command-argument 1)
	 (do (forward-one-paragraph)))))

(de backward-one-paragraph ()
  % This function moves point backward to the start of the previous
  % paragraph. It returns a boolean which is true if the function was
  % stopped by a real paragraph's start, instead of by the buffer's
  % start.
  (if (and (at-line-start?) % if past start of start line, don't miss
	   (start-line-paragraph?)) % start of current paragraph
    (move-to-previous-line))
  (let ((real-start nil))
    (while (not (or (setf real-start (start-line-paragraph?))
		    (current-line-is-first?)))
      (move-to-previous-line))
    (unless (current-line-is-first?) % this sexp gets previous empty line on
      (move-to-previous-line)
      (unless (current-line-empty?)
	(move-to-next-line)))
    real-start))

(de backward-paragraph-command ()
  % If nmode-command-argument is positive this function moves point
  % backward by nmode-command-argument paragraphs , leaving it at the
  % start of a paragraph.  If nmode-command-argument is negative it
  % moves forwards by abs(nmode-command-argument) paragraphs, leaving
  % it at the end of a paragraph.  This function does not return a
  % useful value.
  (if (minusp nmode-command-argument)
    (for (from i 1 (- nmode-command-argument) 1)
	 (do (forward-one-paragraph)))
    (for (from i 1 nmode-command-argument 1)
	 (do (backward-one-paragraph)))))

(de paragraph-limits ()
  % This function returns a list of positions marking the next
  % paragraph.  Only real paragraph limits are returned. If there is
  % only stuff that should be excluded from a paragraph between point
  % and the end or the start of the buffer, then the appropriate limit
  % of the paragraph is filled with the current buffer position.  This
  % function restores point.
  (let* ((temp (buffer-get-position))(top temp)(bottom temp))
    (when (forward-one-paragraph)
      (setf bottom (buffer-get-position)))
    (when (backward-one-paragraph)
      (setf top (buffer-get-position)))
    (buffer-set-position temp)
    (list top bottom)))

(de mark-paragraph-command ()
  % This function sets the mark to the end of the next paragraph, and
  % moves point to its start. It returns nothing useful.
  (let ((pair (paragraph-limits)))
    (buffer-set-position (first pair))
    (set-mark (second pair))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start of Fill Functions and Associated Support Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de next-char-list (end char-count init-pos)
  % This function returns a list, the first element of which is a list
  % of characters, with their order the reverse of that in the
  % original text, spaces squeezed to a single space between words,
  % and with two spaces between sentences. The second element on the
  % list returned is how far along the new line the position
  % corresponding to "init-pos" wound up.  Point is left after the
  % last character packed in but before "end" or the next nonblank
  % character.
  (let* ((from-end-last-blanks 0)
	 (from-start-last-blanks 0)
	 (final-char-pos char-count)
	 (line-not-full (lessp char-count fill-column))
	 (first-end (buffer-get-position))
	 (next-sentence-wont-exhaust-region
	  (not (buffer-position-lessp end first-end)))
	 (new-char (next-character))
	 (line-list ()))
    % start of loop for successive sentences
    (while (and next-sentence-wont-exhaust-region line-not-full)
      % The next sexp checks to see if the next sentence fits within
      % the main region (from initial "point" to "end") with a
      % character to spare for the next sentence iteration.
      (let* ((next-sentence-end (end-of-next-sentence)))
	(setf next-sentence-wont-exhaust-region
	  (not (buffer-position-lessp end next-sentence-end)))
	(setf first-end (if next-sentence-wont-exhaust-region
			  next-sentence-end end)))
      (skip-forward-blanks) % ignore blanks just before next sentence
      % start of loop for successive characters
      (while (and (setf line-not-full (or (lessp char-count fill-column)
					  % next sexp allows oversize words
					  (eq char-count from-end-last-blanks)))
		  (not (buffer-position-lessp first-end
					      (buffer-get-position))))
	(setf new-char
	  % character compression sexp
	  (let ((next (next-character)))
	    (if (not (= (skip-forward-blanks)
			next))
	      #\blank
	      (move-forward)
	      next)))
	(setq line-list (cons new-char line-list))
	(incr char-count)
	(when (buffer-position-lessp (buffer-get-position) init-pos)
	  (setf final-char-pos char-count))
	(cond ((= new-char #\blank)
	       (setf from-end-last-blanks 0)
	       (setf from-start-last-blanks 1))
	      (t % normal character  
	       (incr from-end-last-blanks)
	       (incr from-start-last-blanks))))
      % The next sexp terminates sentences properly.
      (when (and line-not-full next-sentence-wont-exhaust-region)
	(setf line-list (append '(#\blank #\blank) line-list))
	(incr char-count 2)
	(setf from-end-last-blanks 0)
	(setf from-start-last-blanks 2)))
    % The next sexp trims off the last partial word or extra blank(s).
    (when (or (char-blank? (car line-list)) % extra blank(s)
	      (not (or line-not-full % last partial word
		       (at-line-end?)
		       (char-blank? (next-character)))))
      (for (from i 1 from-start-last-blanks 1)
	   (do (setf line-list (cdr line-list))))
      (move-over-characters (- from-end-last-blanks)))
    % guarantee that buffer-position is left at or before end
    (if (buffer-position-lessp end (buffer-get-position))
      (buffer-set-position end))
    (list line-list final-char-pos)))

(de justify (input desired-length)
  % This function pads its input with blanks and reverses it.  It
  % leaves point alone.
  (let*
    ((input-length (length input))
     (output ())
     (needed-blanks (- desired-length input-length))
     % total number needed to fill out line
     (input-blanks % count preexisting blanks in input
      (for (in char input)
	   (with blanks)
	   (count (= char #\blank) blanks)
	   (returns blanks))))
    (for (in char input)
	 (with (added-blanks 0) % number of new blanks added so far
	       (handled-blanks 0)) % number of input blanks considered so far
	 (do
	  (setf output (cons char output))
	  (when (= char #\blank)
	    (incr handled-blanks)
	    % calculate number of new blanks needed here
	    % fraction of original blanks passed=handled-blanks/input-blanks
	    % blanks needed here~fraction*[needed-blanks(for whole line)]-(added-blanks)
	    (let ((new-blanks (- (/ (* needed-blanks handled-blanks)
				    input-blanks)
				 added-blanks)))
	      (when (> new-blanks 0)
		(for (from new 1 new-blanks 1)
		     (do 
		      (setf output (cons #\blank output))))
		(incr added-blanks new-blanks))))))
    output))

(de position-adjusted-for-prefix (position)
  % This is a pure function which returns a position, corrected for
  % the length of the prefix on the position's line.
  (let ((current-place (buffer-get-position)))
    (buffer-set-position position)
    (mismatched-prefix?)
    (let ((prefix-length-or-zero (current-char-pos)))
      (buffer-set-position current-place)
      (let ((adjusted-char-pos (- (buffer-position-column position)
				  prefix-length-or-zero)))
	(if (< adjusted-char-pos 0)(setf adjusted-char-pos 0))
	(buffer-position-create (buffer-position-line position)
				adjusted-char-pos)))))

(de remove-prefix-from-region (start end)
  % The main effect of this function is to strip the fill prefix off a
  % region in the buffer. this function does not return a useful value
  % or move point.
  (let ((current-place (buffer-get-position)))
    (buffer-set-position start)
    (if (current-line-empty?)(move-to-next-line))
    (while (not (buffer-position-lessp end (buffer-get-position)))
      (setf start (buffer-get-position))
      (unless (or 
	       (mismatched-prefix?)
	       (buffer-position-lessp end (buffer-get-position)))
	(extract-text t start (buffer-get-position)))
      (move-to-next-line))
    (buffer-set-position current-place)))

(de fill-directed-region (start end init-pos)
  % The main effect of this function is to replace text with filled or
  % justified text.  This function returns a list.  The first element
  % is the increase in the number of lines in the text due to filling.
  % The second element is the filled position equivalent to "init-pos"
  % in the original text.  The point is left at the end of the new
  % text
  (let ((modified-flag (=> nmode-current-buffer modified?))
	(old-text (extract-text nil start end))
	(final-pos init-pos)
	(adj-end (position-adjusted-for-prefix end))	
	(adj-init-pos (position-adjusted-for-prefix init-pos)))
    (when fill-prefix (remove-prefix-from-region start end))
    (setf end adj-end)
    (buffer-set-position start)
    (let*
      ((list-of-new-lines (when % handles first blank line
			    (current-line-empty?)
			    (move-to-next-line)
			    '("")))
       (new-packed-line '(nil 0))
       (prefix-list
	(if fill-prefix 
	  (string-to-list 
	   (getv fill-prefix 0))))
       (prefix-column (map-char-to-column
		       (list2string prefix-list)
		       (length prefix-list)))
       (new-line nil)
       (place (buffer-get-position))               % handles indentation
       (junk (skip-forward-blanks))                % handles indentation
       (start-char-pos (+ (current-display-column) % handles indentation
			  prefix-column)) % and first time switch
       (indent-list (string-to-list                % handles indentation
		     (getv (extract-text
			    nil place (buffer-get-position)) 0))))
      (while
	(let* ((after-line-start (buffer-position-lessp
				  (buffer-get-position) adj-init-pos))
	       (new-packed-line 
		(next-char-list end start-char-pos adj-init-pos))
	       (before-line-end (buffer-position-lessp
				 adj-init-pos (buffer-get-position))))
	  (when (and after-line-start before-line-end)
	    (setf final-pos (buffer-position-create
			     (+ (buffer-position-line start)
				(length list-of-new-lines))
			     (second new-packed-line))))
	  % test that anything is left in the region, as well as getting line
	  (setf new-line (first new-packed-line)))
	(setf new-line
	  (list2string 
	   (append % add in fill prefix and indentation
	    (append prefix-list
		    (unless (= start-char-pos prefix-column) indent-list))
	    (if (and nmode-command-argument-given % triggers justification
		     (not (or % don't justify the last line in a paragraph
			   (buffer-position-lessp end (buffer-get-position))
			   (at-buffer-end?))))
	      (justify new-line (- fill-column start-char-pos))
	      (reverse new-line)))))
	(setf list-of-new-lines (cons new-line list-of-new-lines))
	% only the first line in a paragraph is indented
	(setf start-char-pos prefix-column))
      (setf list-of-new-lines (cons (list2string nil) list-of-new-lines))
      % The last line in the new paragraph is added in last setf.
      (let ((line-change 0)
	    (new-text (list2vector (reverse list-of-new-lines))))
	(when list-of-new-lines
	  (extract-text t start end)
	  (setf line-change
	    (- (size new-text)
	       (size old-text)))
	  (insert-text new-text)
	  (if (and (not modified-flag)
		   (text-equal new-text old-text))
	    (=> nmode-current-buffer set-modified? nil)))
	(list line-change final-pos)))))

(de clip-region (limits region)
  % This is a pure function with no side effects.  It returns the
  % "region" position pair, sorted so that first buffer position is
  % the first element, and clipped so that the region returned is
  % between the buffer-positions in "limits".
  (let ((limit-pair (if (buffer-position-lessp (cadr limits) (car limits))
		      (reverse limits) limits))
	(region-pair (copy
		      (if (buffer-position-lessp (cadr region) (car region))
			(reverse region) region))))
    (if (buffer-position-lessp (car region-pair) (car limit-pair))
      (setf (car region-pair) (car limit-pair)))
    (if (buffer-position-lessp (cadr region-pair) (car limit-pair))
      (setf (cadr region-pair) (car limit-pair)))
    (if (buffer-position-lessp (cadr limit-pair) (car region-pair))
      (setf (car region-pair) (cadr limit-pair)))
    (if (buffer-position-lessp (cadr limit-pair) (cadr region-pair))
      (setf (cadr region-pair) (cadr limit-pair)))
    region-pair))
	 
(de fill-region-command ()
  % This function replaces the text between point and the current mark
  % with a filled version of the same text.  It leaves the
  % buffer-position at the end of the new text.  It does not return
  % anything useful.
  (let* ((current-place (buffer-get-position))
	 (limits (list (current-mark) current-place)))
    (setf limits
      (if (buffer-position-lessp (car limits) (cadr limits))
	limits (reverse limits)))
    (buffer-set-position (car limits))
    (let ((at-limits nil)(new-region nil)(lines-advance 0))
      (while (not at-limits) % paragraph loop
	(setf new-region (paragraph-limits))
	(setf new-region (clip-region limits new-region))
	(setf at-limits (= (car new-region) (cadr new-region)))
	(unless at-limits
	  (setf lines-advance
	    (first (fill-directed-region % expansion-of-text-information used
		    (car new-region) (cadr new-region) current-place)))
	  (setf limits % compensate for expansion of filled text
	    (list (first limits)
		  (let ((bottom (second limits)))
		    (buffer-position-create
		     (+ lines-advance (buffer-position-line bottom))
		     (buffer-position-column bottom))))))
	(setf limits % guarantee that no text is filled twice
	  (list (buffer-get-position)(second limits)))))))

(de fill-paragraph-command ()
  % This function replaces the next paragraph with filled version.  It
  % leaves point at the a point bearing the same relation to the
  % filled text that the old point did to the old text.  It does not
  % return a useful value.
  (let* ((current-place (buffer-get-position))
	 (pos-list (paragraph-limits)))
    (buffer-set-position (second (fill-directed-region
				  (first pos-list)
				  (second pos-list)
				  current-place)))))

(de fill-comment-command ()
  % This function creates a temporary fill prefix from the start of
  % the current line.  It replaces the surrounding paragraph
  % (determined using fill-prefix) with a filled version.  It leaves
  % point at the a position bearing the same relation to the filled
  % text that the old point did to the old text.  It does not return a
  % useful value.
  (let ((current-place (buffer-get-position)))
    (move-to-start-of-line)
    (let ((place (buffer-get-position))) % get fill prefix ends set up
      (skip-forward-blanks-in-line)
      (while (not (or (alphanumericp (next-character))
		      (at-line-end?)
		      (char-blank? (next-character))))
	(move-forward))
      (skip-forward-blanks-in-line)
      (let* ((fill-prefix (extract-text nil place (buffer-get-position)))
	     (pos-list (paragraph-limits)))
	(if (buffer-position-lessp (first pos-list) current-place)
	  (buffer-set-position (second (fill-directed-region
					(first pos-list)
					(second pos-list)
					current-place)))
	  (buffer-set-position current-place))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start of Misc Functions and Associated Support Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de center-current-line ()
  % This function trims and centers the current line.  It does not
  % return a useful value.  It leaves point at a point in the text
  % equivalent to that before centering.
  (current-line-strip-indent)
  (let ((current-place (buffer-get-position)))
    (move-to-end-of-line)
    (strip-previous-blanks)
    (buffer-set-position current-place))
  (let ((needed-blanks (/ (- fill-column (current-display-column)) 2)))
    (unless (minusp needed-blanks)
      (indent-current-line needed-blanks))))

(de center-line-command ()
  % This function centers a number of lines, depending on the
  % argument.  It leaves point at the end of the last line centered.
  % It does not return a useful value.
  (center-current-line)
  (when (> (abs nmode-command-argument) 1)
    (if (minusp nmode-command-argument)
      (for (from i 2 (- nmode-command-argument) 1)
	   (do (move-to-previous-line)
	       (center-current-line)))
      (for (from i 2 nmode-command-argument 1)
	   (do (move-to-next-line)
	       (center-current-line))))))

(de what-cursor-position-command ()
  % This function tells the user where they are in the buffer or sets
  % point to the specified line number.  It does not return a useful
  % value.
  (cond
   (nmode-command-number-given
    (set-line-pos nmode-command-argument)
    )
   (t
    (write-message
     (if (at-buffer-end?)
       (bldmsg "X=%w Y=%w line=%w (%w percent of %w lines)"
	       (current-display-column)
	       (- (current-line-pos)(current-window-top-line))
	       (current-line-pos)
	       (/ (* 100 (current-line-pos))
		  (current-buffer-visible-size))
	       (current-buffer-visible-size))
       (bldmsg "X=%w Y=%w CH=%w line=%w (%w percent of %w lines)"
	       (current-display-column)
	       (- (current-line-pos)(current-window-top-line))
	       (next-character) % omitted at end of buffer
	       (current-line-pos)
	       (/ (* 100 (current-line-pos))
		  (current-buffer-visible-size))
	       (current-buffer-visible-size))))
    )))
