%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Dabbrevs.SL - Dynamic Abbreviations for NMODE
% 
% Author:      Mark R. Swanson
%              University of Utah
% Date:        15 June 1983
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Methods provided: (all internal, really)
%   initial-expansion
%   expand-aux
%   find-next-expansion
%   get-expansion-from-buffer
%   expand
%   save-expansion
%
% Commands defined:
%   instant-abbrev-command
%     Tries to "expand" the word (or prefix) before point by searching for other
%     words with the same prefix.  The search goes back from point (or from the
%     location of the last expansion found for the current abbreviation); if
%     unsuccessful, a search is done forward from point.  Re-issuing the command
%     causes a search for the next possible expansion.  The command is initially
%     bound to the M-<space> key.

(CompileTime
 (load objects fast-int))

(fluid '(current-abbrev-expansion))
(setf current-abbrev-expansion nil)

(defflavor abbrev-expansion 
 (abbrev                    % original abbreviation string 
  abbrev-start-pos           
  abbrev-end-pos
  (expansion-list nil)      % list of all expansions tried (including abbrev)
  expansion-start-pos       % start of latest expansion
  expansion-end-pos         % end of latest expansion
  last-pos                  % position of end of latest expansion/abbrev in 
                            %  buffer
  (direction -1)            % initially look backwards (-1)
  (word-delim-list '(#\!( #\!) #\!' #\- #\space #\<)) % word delimitors
  )
  ()
)

(defmethod (abbrev-expansion initial-expansion) ()
% Initial attempt to find an expansion for "word" before point.  Search goes
%  first backward, then forward, through buffer for an appropriate expansion.

  (setf last-pos (setf abbrev-end-pos (buffer-get-position)))
  (if (not (move-backward-word)) % is there a word to expand?
    (ding)                       % no
    % else                         yes
    (setf abbrev-start-pos (buffer-get-position)) % bracket its position
    (setf abbrev (cdr (extract-region nil abbrev-start-pos abbrev-end-pos)))
    (=> self save-expansion abbrev) % abbrev is its own initial "expansion"
    (=> self expand-aux)
    ))

(defmethod (abbrev-expansion expand-aux) ()
% Actually do the expansion (or re-expansion); search backwards first, then
%  forwards if necessary;  do not re-present duplicate expansions which have
%  already been tried.

  (write-message (concat "Expanding " (vector-fetch abbrev 0)))
  (let ((found-one nil)
	 new-expansion)
    (while (and (~= direction 0)	% if zero we have searched in both directions
	     (not found-one))
      (setf new-expansion (=> self find-next-expansion direction))
      (if new-expansion % then 
	(progn 
	  (if (< direction 0)	% move ptr for next search (may not be necessary)
	    (move-backward) (move-forward))
	  (setf found-one (not (member new-expansion expansion-list))))
%else
	(setf direction (if (= direction -1) 1 0))	% change directions
	(buffer-set-position last-pos)	% and start from original location
	))
% Finally insert expansion and add it to history
    (if found-one
      (progn
	(extract-region T abbrev-start-pos last-pos)	%remove old abbrev/expans.
	(insert-string (vector-fetch new-expansion 0))	% put in new expans.
	(setf last-pos (buffer-get-position))	% note end of expans.
	(=> self save-expansion new-expansion))
% else
      (buffer-set-position last-pos)	% put point back where we started
      (ding)	% let user know we failed
      )))

(defmethod (abbrev-expansion find-next-expansion) (dir)
% Search backward/forward from current location for an expansion (string match of
%  abbreviation preceded by a word delimitor. Returns NIL on failure, 
%  expansion-string on success; leaves point at start of last string match.

  (let ((found-one nil))
    (while (and (not found-one)
		(buffer-text-search? abbrev dir))
      (if (or (=> nmode-current-buffer at-line-start?)
	      (member (=> nmode-current-buffer previous-character) 
		      word-delim-list))
	(setf found-one T)
	(if (< dir 0)
	  (move-backward)
	  (move-forward))))
    (if found-one
      (=> self get-expansion-from-buffer))))

(defmethod (abbrev-expansion get-expansion-from-buffer) ()
  % Extracts the expansion from the buffer; on entry point should be at start
  %  of expansion, on exit it will be returned to that position.  Form of
  %  result should be a vector containing 1 string.

  (let (expans)
    (setf expansion-start-pos (buffer-get-position))
    (move-forward-word)
    (setf expansion-end-pos (buffer-get-position))
    (setf expans (cdr (extract-region NIL expansion-start-pos expansion-end-pos)))
    (buffer-set-position expansion-start-pos)
    expans))

(defmethod (abbrev-expansion expand) ()
  % Attempt to re-expand last expansion.  Point must be at end of previous
  %  expansion, word itself should not have been changed.

  (let ((cur-pos (buffer-get-position)))
    (if (and
	 (equal last-pos (buffer-get-position))
	 (move-backward-word)
	 (equal abbrev-start-pos (buffer-get-position))
	 (equal (car expansion-list)
		(cdr (extract-region nil abbrev-start-pos last-pos))))
      (progn
       (buffer-set-position expansion-start-pos)
       (=> nmode-current-buffer move-backward)
       (=> self expand-aux))
      (buffer-set-position cur-pos)
      nil
      )))

(defmethod (abbrev-expansion save-expansion) (expansion)
	(setf expansion-list (adjoin expansion expansion-list)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% dynamic abbreviation command and its installation
%

(de instant-abbrev-command ()
  (cond ((or 
	  (null current-abbrev-expansion)
	  (null (=> current-abbrev-expansion expand)))
	 (setf current-abbrev-expansion (make-instance 'abbrev-expansion))
	 (=> current-abbrev-expansion initial-expansion))))

(setf Text-Command-List
  (NConc Text-Command-List
	 (list
	  (cons (x-char M-!  ) 'instant-abbrev-command)
	  )))

