%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Case-Commands.SL - NMODE Case Conversion commands
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        6 October 1982
%
% The original code was contributed by Jeff Soreff.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load fast-int fast-vectors fast-strings))

(fluid '(
  nmode-command-argument
  nmode-current-buffer
  ))

% Global variables:

(fluid '(shifted-digits-association-list))
(setf shifted-digits-association-list NIL)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Case Conversion Commands:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de uppercase-word-command ()
  (transform-region-with-next-word-or-fragment #'string-upcase))

(de lowercase-word-command ()
  (transform-region-with-next-word-or-fragment #'string-downcase))

(de uppercase-initial-command ()
  (transform-region-with-next-word-or-fragment #'string-capitalize))

(de uppercase-region-command ()
  (transform-marked-region #'string-upcase))

(de lowercase-region-command ()
  (transform-marked-region #'string-downcase))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Upcase Digit Command:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de upcase-digit-command ()
  % Convert the previous digit to the corresponding "shifted character"
  % on the keyboard.  Search only within the current line or the previous
  % line.  Ding if no digit found.


  (let ((point (buffer-get-position))
	(limit-line-pos (- (current-line-pos) 1))
	(ok NIL)
	)
    (while (and (>= (current-line-pos) limit-line-pos)
		(not (at-buffer-start?))
		(not (setf ok (digitp (previous-character))))
		)
      (move-backward)
      )
    (cond ((and ok (set-up-shifted-digits-association-list))
	   (let* ((old (previous-character))
		  (new (cdr (assoc old shifted-digits-association-list)))
		  )
	     (delete-previous-character)
	     (insert-character new)
	     ))
	  (t (Ding))
	  )
    (buffer-set-position point)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% General Transformation Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de transform-region (string-conversion-function bp1 bp2)
  % Transform the region in the current buffer between the positions
  % BP1 and BP2 by applying the specified function to each partial or
  % complete line.  The function should accept a single string argument
  % and return the transformed string.  Return 1 if BP2 > BP1;
  % return -1 if BP2 < BP1.  The buffer pointer is left at the "end"
  % of the transformed region (the greater of BP1 and BP2).

  (let* ((modified-flag (=> nmode-current-buffer modified?))
	 (extracted-pair (extract-region t bp1 bp2))
	 (newregion (cdr extracted-pair))
	 (oldregion (if (not modified-flag) (copyvector newregion)))
	 )
    (for (from index 0 (vector-upper-bound newregion) 1)
	 (do (vector-store newregion index 
	       (apply string-conversion-function
		      (list (vector-fetch newregion index))))))
    (insert-text newregion)
    (if (and (not modified-flag) (text-equal newregion oldregion))
	(=> nmode-current-buffer set-modified? nil)
	)
    (car extracted-pair)
    ))
		
(de transform-region-with-next-word-or-fragment (string-conversion-function)
  % Transform the region consisting of the following N words, where N is
  % the command argument.  N may be negative, meaning previous words.

  (let ((start (buffer-get-position)))
    (move-over-words nmode-command-argument)
    (transform-region string-conversion-function start (buffer-get-position))
    ))

(de transform-marked-region (string-conversion-function)
  % Transform the region defined by point and mark.

  (let ((point (buffer-get-position))
	(mark (current-mark))
	)
    (when (= (transform-region string-conversion-function point mark) 1)
      % The mark was at the end of the region. If the transformation changed
      % the length of the region, the mark may need to be updated.
      (previous-mark) % pop off old mark
      (set-mark-from-point) % set the mark to the end of the transformed region
      (buffer-set-position point)
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary Function:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de set-up-shifted-digits-association-list ()
  % Ensure that the "shifted digits association list" is set up properly.
  % If necessary, ask the user for the required information.  Returns the
  % association list if properly set up, NIL if an error occurred.

  (if (not shifted-digits-association-list)
    (let ((shifted-digits
	   (prompt-for-string 
	    "Type the digits 1, 2, ... 9, 0, holding down Shift:" nil)))
      (cond ((= (string-length shifted-digits) 10) 
	     (setq shifted-digits-association-list
		   (pair 
		    (string-to-list "1234567890")
		    (string-to-list shifted-digits))))
	    ((> (string-length shifted-digits) 10)
	     (nmode-error "Typed too many shifted digits!"))
	    (t
	     (nmode-error "Typed too few shifted digits!"))
	    )))
  shifted-digits-association-list
  )
