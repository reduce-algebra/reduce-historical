%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AUTOFILL.SL - NMODE Auto-Fill Mode
% 
% Author:      Jeff Soreff
%              Hewlett-Packard/CRC
% Date:        3 November 1982
% Revised:     18 January 1983
%
% 16-Nov-82 Jeff Soreff
%   Fixed bugs (handling very long lines, breaking at punctuation)
%   and improved efficiency.
% 29-Nov-82 Jeff Soreff
%   Fixed bug with too-long word.
% 18-Jan-83 Jeff Soreff
%   Made autofill preserve textual context of buffer position.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load extended-char fast-int fast-strings fast-vectors))

% Externals used here:
(fluid '(nmode-command-argument nmode-command-argument-given))

% Globals defined here:
(fluid '(fill-prefix fill-column auto-fill-mode))

(setf fill-prefix nil)
(setf fill-column 70)
(setf auto-fill-mode
  (nmode-define-mode "Fill" '((auto-fill-setup))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de auto-fill-mode-command ()
  (toggle-minor-mode auto-fill-mode))

(de auto-fill-setup ()
  (if (eq (dispatch-table-lookup (x-char SPACE)) 'insert-self-command)
    (nmode-define-command (x-char SPACE) 'auto-fill-space)
    ))

(de set-fill-column-command ()
  (if nmode-command-argument-given
    (setq fill-column nmode-command-argument)
    (setq fill-column (current-display-column)))
  (write-message
   (bldmsg "%w%p" "Fill Column = " fill-column)))

(de set-fill-prefix-command ()
  (let ((temp (buffer-get-position)))
    (cond ((at-line-start?)
	   (setq fill-prefix nil)
	   (write-message "Fill Prefix now empty"))
	  (t (move-to-start-of-line)
	     (setq fill-prefix
		   (extract-text 
		    nil (buffer-get-position) 
		    temp))
	     (buffer-set-position temp)
	     (write-message
	      (bldmsg "%w%p" "Fill Prefix now "
		      (vector-fetch fill-prefix 0)))))))

(de blank-char (char) (or (= char #\tab) (= char #\blank)))

(de skip-forward-blanks-in-line ()
  (while (and (not (at-line-end?))
	      (blank-char (next-character)))
    (move-forward)))

(de skip-backward-blanks-in-line ()
  (while (and (not (at-line-start?))
	      (blank-char (previous-character)))
    (move-backward)))

(de skip-forward-nonblanks-in-line ()
  (while (and (not (at-line-end?))
	      (not (blank-char (next-character))))
	 (move-forward)))

(de auto-fill-space ()
  (for (from i 1 nmode-command-argument 1)
       (do  (insert-character #\blank)))
  (when (> (current-display-column) fill-column)
    (let ((word-too-long nil)
	  (current-place (buffer-get-position)))
      (set-display-column fill-column)
      (while (or (not (at-line-end?)) word-too-long)
	(let ((start nil)(end nil))
	  (while (not (or (at-line-start?)
			  (and (blank-char % start natural break
				(next-character))
			       (not (blank-char
				     (previous-character))))))
	    (move-backward))
	  (unless (setf word-too-long 
		    (and (at-line-start?)
			 (not (blank-char (next-character)))))
	    (setf start (buffer-get-position))
	    (skip-forward-blanks-in-line)
	    (setf end (buffer-get-position))
	    (when (buffer-position-lessp start current-place) % Correct for
	      (if (buffer-position-lessp current-place end)   % the extraction.
		(setf current-place start) % Within extracted interval
		(setf current-place        % After extracted interval
		  (buffer-position-create
		   (buffer-position-line current-place)
		   (- (buffer-position-column current-place)
		      (- (buffer-position-column end)
			 (buffer-position-column start)))))))
	    (extract-text t start end)
	    (when (buffer-position-lessp (buffer-get-position) current-place)
	      (setf current-place % Correct for new line break being added
		(buffer-position-create
		 (+ (buffer-position-line current-place) 1)
		 (- (buffer-position-column current-place)
		    (current-char-pos)))))
	    (insert-eol)
	    (when fill-prefix 
	      (insert-text fill-prefix)
	      (setf current-place % Correct for prefix length
		(buffer-position-create 
		 (buffer-position-line current-place)
		 (+ (buffer-position-column current-place)
		    (string-length (vector-fetch fill-prefix 0))))))))
	(if word-too-long
	  (move-to-end-of-line)
	  (set-display-column fill-column)))
      (buffer-set-position current-place))))
