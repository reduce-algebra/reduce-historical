%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Structure-Functions.SL - NMODE functions for moving about structured text
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        12 November 1982
% Revised:     18 February 1983
%
% This file contains functions for moving about structured text, such as Lisp
% source code.  The functions are based on the primitives in the module
% NMODE-Parsing; the variable NMODE-CURRENT-PARSER determines the actual syntax
% (e.g., Lisp, RLISP, etc.). See the document NMODE-PARSING.TXT for a
% description of the parsing strategy.
%
% 18-Feb-83 Alan Snyder
%  Replaced move-down-list with move-forward-down-list and
%  move-backward-down-list.
% 6-Jan-83 Alan Snyder
%  Use LOAD instead of FASLIN to get macros (for portability); reformat source.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects fast-int nmode-parsing))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Form Movement Functions
%
% A form is an ATOM or a nested structure.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de move-forward-form ()
  % Move to the end (just past the last character) of the current (if any) or
  % the next (otherwise) complete form or unmatched closing bracket.  Returns
  % either NIL (no complete form found), 'ATOM, 'CLOSER (unmatched closing
  % bracket), or 'STRUCTURE (complete structure).  If NIL is returned, then
  % point is unchanged.

  (let* ((old-pos (buffer-get-position)) % save current position
         (first-item (move-forward-item)) % find next item (see below)
         )
    (if (eq first-item 'OPENER) % it is an opening bracket
      (while T % scan past complete forms until an unmatched closing bracket
	(selectq (move-forward-form)
	  (NIL (buffer-set-position old-pos) (exit NIL)) % end of text
	  (CLOSER (exit 'STRUCTURE)) % found the matching closing bracket
	  ))
      first-item % Otherwise, just return the information.
      )))

(de move-backward-form ()
  % Move backward at least one character to the preceding character that is not
  % part of whitespace; then move to the beginning of the smallest form that
  % contains that character.  If no form is found, return NIL and leave point
  % unchanged.  Otherwise, return either 'ATOM, 'STRUCTURE (passed over complete
  % structure), or 'OPENER (passed over unmatched open bracket).

  (let* ((old-pos (buffer-get-position)) % save current position
         (first-item (move-backward-item)) % find previous item (see below)
         )
    (if (eq first-item 'CLOSER) % it is a closing bracket
      (while T % scan past complete forms until an unmatched opening bracket
	(selectq (move-backward-form)
	  (NIL (buffer-set-position old-pos) (exit NIL)) % beginning of text
	  (OPENER (exit 'STRUCTURE)) % found the matching opening bracket
	  ))
      first-item % Otherwise, just return the information.
      )))

(de move-backward-form-interruptible ()
  % This function is like move-backward-form, except it can be interrupted by
  % user type-ahead.  If it is interrupted, it returns 'INTERRUPT and restores
  % the old position.

  (let ((old-pos (buffer-get-position))
	(paren-depth 0)
	)
    (while T
      (when (input-available?) (buffer-set-position old-pos) (exit 'INTERRUPT))
      (let ((item (move-backward-item)))
	(selectq item
	  (NIL (buffer-set-position old-pos) (exit NIL))
	  (OPENER (setf paren-depth (- paren-depth 1))
		  (if (= paren-depth 0) (exit 'STRUCTURE))
		  )
	  (CLOSER (setf paren-depth (+ paren-depth 1)))
	  )
	(if (<= paren-depth 0) (exit item))
	))))

(de move-backward-form-within-line ()
  % This is the same as MOVE-BACKWARD-FORM, except that it looks only within the
  % current line.

  (let* ((old-pos (buffer-get-position)) % save current position
         (first-item (move-backward-item-within-line)) % find previous item
         )
    (if (eq first-item 'CLOSER) % it is a closing bracket
      (while T % scan past complete forms until an unmatched opening bracket
	(selectq (move-backward-form-within-line)
	  (NIL (buffer-set-position old-pos) (exit NIL)) % beginning of text
	  (OPENER (exit 'STRUCTURE)) % found the matching opening bracket
	  ))
      first-item % Otherwise, just return the information.
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Item Movement Functions
%
% An item is an ATOM or a structure bracket.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de move-forward-item ()
  % Move to the end (just past the last character) of the current (if any) or
  % the next (otherwise) atom or bracket.  Returns either NIL (no item found),
  % 'ATOM, 'OPENER, or 'CLOSER.  If NIL is returned, then point is unchanged.

  (let ((item-type (move-forward-to LAST NOT-SPACE)))
    (if item-type (move-forward-character))
    item-type
    ))

(de move-backward-item ()
  % Move backward at least one character to the preceding character that is not
  % part of whitespace; then move to the beginning of the atom or bracket that
  % contains that character.  Returns either NIL (no item found), 'ATOM,
  % 'OPENER, or 'CLOSER.  If NIL is returned, then point is unchanged.

  (let ((old-pos (buffer-get-position))
	(item-type nil)
	)
    (if (move-backward-character)
      (setf item-type (move-backward-to FIRST NOT-SPACE)))
    (if (not item-type) (buffer-set-position old-pos))
    item-type
    ))

(de move-backward-item-within-line ()
  % This is the same as MOVE-BACKWARD-ITEM, except that it looks only within the
  % current line.

  (if (not (at-line-start?))
    (let ((old-pos (buffer-get-position))
	  (item-type nil)
	  )
      (move-backward-character)
      (setf item-type (move-backward-within-line-to FIRST NOT-SPACE))
      (if (not item-type) (buffer-set-position old-pos))
      item-type
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Move-Up-Forms Functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de move-forward-up-list ()
  % Move to the right of the current structure (e.g. list).  In other words,
  % find the next closing structure bracket whose matching opening structure
  % bracket is before point.  If no such bracket can be found, return NIL and
  % leave point unchanged.

  (forward-scan-for-right-paren -1)
  )

(de move-backward-up-list ()
  % Move to the beginning of the current structure (e.g. list).  In other words,
  % find the previous opening structure bracket whose matching closing structure
  % bracket is after point.  If no such bracket can be found, return NIL and
  % leave point unchanged.

  (reverse-scan-for-left-paren 1)
  )
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% List Movement Functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de move-forward-list ()
  % Move to the right of the current or next structure (e.g. list).  In other
  % words, find the next closing structure bracket whose matching opening
  % structure bracket is before point or is the first opening structure bracket
  % after point.  If no such bracket can be found, return NIL and leave point
  % unchanged.

  (forward-scan-for-right-paren 0)
  )

(de move-backward-list ()
  % Move to the beginning of the current or previous structure (e.g. list).  In
  % other words, find the previous opening structure bracket whose matching
  % closing structure bracket is after point or is the first closing structure
  % bracket before point.  If no such bracket can be found, return NIL and leave
  % point unchanged.

  (reverse-scan-for-left-paren 0)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Display Commands
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de display-matching-opener ()
  % If the previous character is the last character of a closing bracket, then
  % move backward to the beginning of the form, wait a while so that the user
  % can see where it is, then return to the previous position.
  (let ((old-pos (buffer-get-position)))
    (unwind-protect
     (unsafe-display-matching-opener)
     (buffer-set-position old-pos)
     )))

(de unsafe-display-matching-opener ()
  (move-backward-character)
  (when (test-current-attributes LAST CLOSER)
    (move-forward-character)
    (selectq (move-backward-form-interruptible)
      (STRUCTURE
       (nmode-refresh) % Show the user where we are.
       (sleep-until-timeout-or-input 30) % wait a while
       )
      (INTERRUPT)
      (t (Ding))
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal List Scanning Primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de reverse-scan-for-left-paren (depth)
  % Scan backwards (starting with the character before point) for a left paren
  % at depth >= the specified depth.  If found, the left paren will be after
  % point and T will be returned.  Otherwise, point will not change and NIL will
  % be returned.
  (let ((old-pos (buffer-get-position))
	(paren-depth 0)
	)
    (while T
      (selectq (move-backward-item)
	(NIL (buffer-set-position old-pos) (exit NIL))
	(CLOSER (setf paren-depth (- paren-depth 1)))
	(OPENER (setf paren-depth (+ paren-depth 1))
		(if (>= paren-depth depth) (exit T))
		)
	))))

(de forward-scan-for-right-paren (depth)
  % Scan forward (starting with the character after point) for a right paren at
  % depth <= the specified depth.  If found, the right paren will be before
  % point and T will be returned.  Otherwise, point will not change and NIL will
  % be returned.
  (let ((old-pos (buffer-get-position))
	(paren-depth 0)
	)
    (while T
      (selectq (move-forward-item)
	(NIL (buffer-set-position old-pos) (exit NIL))
	(CLOSER (setf paren-depth (- paren-depth 1))
		(if (<= paren-depth depth) (exit T))
		)
	(OPENER (setf paren-depth (+ paren-depth 1)))
	))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Move-Down-List functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de move-forward-down-list ()
  % Move forward past the next open bracket at the current level.
  (let ((old-pos (buffer-get-position)))
    (while T
      (selectq (move-forward-item)
	((NIL CLOSER) (buffer-set-position old-pos) (exit NIL))
	(OPENER (exit T))
	))))

(de move-backward-down-list ()
  % Move backward past the previous close bracket at the current level.
  (let ((old-pos (buffer-get-position)))
    (while T
      (selectq (move-backward-item)
	((NIL OPENER) (buffer-set-position old-pos) (exit NIL))
	(CLOSER (exit T))
	))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de skip-prefixes ()
  % Skip over any "prefix characters" (like ' in Lisp).
  (while (test-current-attributes PREFIX) (move-forward))
  )
