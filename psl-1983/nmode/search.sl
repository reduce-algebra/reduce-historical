%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Search.SL - Search utilities
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        23 August 1982
%
% Adapted from Will Galway's EMODE
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% These routines to implement minimal string searches for EMODE.  Searches
% are non-incremental, limited to single line patterns, and always ignore
% case.

(CompileTime (load objects fast-strings fast-int))

(fluid '(last-search-string))
(setf last-search-string NIL)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de forward-string-search ()
  % Invoked from keyboard, search forward from point for string, leave
  % "point" unchanged if not found.

  (let ((strng (prompt-for-string "Forward search: " last-search-string)))
    (setf last-search-string strng)
    (if (buffer-search strng 1)
      (for (from i 0 (string-upper-bound strng))
	   (do (move-forward))
	   )
      % else
      (write-prompt "Search failed.")
      (Ding)
      )))

(de reverse-string-search ()
  % Invoked from keyboard, search backwards from point for string, leave
  % "point unchanged if not found.

  (let ((strng (prompt-for-string "Reverse search: " last-search-string)))
    (setf last-search-string strng)
    (move-backward)
    (if (not (buffer-search strng -1))
	(progn (move-forward) (write-prompt "Search failed.") (Ding)))
    ))

(de buffer-search (pattern dir)

  % Search in buffer for the specified pattern.  Dir should be +1 for forward,
  % -1 for backward.  If the pattern is found, the buffer cursor will be set to
  % the beginning of the matching string and T will be returned.  Otherwise,
  % the buffer cursor will remain unchanged and NIL will be returned.

  (setf pattern (string-upcase pattern))
  (if (> dir 0)
    (forward-search pattern)
    (reverse-search pattern)
    ))

(de forward-search (pattern)

  % Search forward in the current buffer for the specified pattern.
  % If the pattern is found, the buffer cursor will be set to
  % the beginning of the matching string and T will be returned.  Otherwise,
  % the buffer cursor will remain unchanged and NIL will be returned.

  (let ((line-pos (current-line-pos))
	(char-pos (current-char-pos))
	(limit (current-buffer-size))
	found-pos
	)

    (while
      (and (< line-pos limit)
	   (not (setf found-pos
		  (forward-search-on-line line-pos char-pos pattern)))
	   )
      (setf line-pos (+ line-pos 1))
      (setf char-pos NIL)
      )
    (if found-pos
	(progn (current-buffer-goto line-pos found-pos) T)))
    ))

(de forward-search-on-line (line-pos char-pos pattern)

  % Search on the current line for the specified string.  If CHAR-POS is
  % non-NIL, then begin at that location, otherwise begin at the beginning of
  % the line.  We look to see if the string lies to the right of the current
  % search location.  If we find it, we return the CHAR-POS of the first
  % matching character.  Otherwise, we return NIL.

  (let* ((line (current-buffer-fetch line-pos))
	 (pattern-length (string-length pattern))
	 (limit (- (string-length line) pattern-length))
	 )
    (if (null char-pos) (setf char-pos 0))
    (while (<= char-pos limit)
      (if (pattern-matches-in-line pattern line char-pos)
	(exit char-pos)
	)
      (setf char-pos (+ char-pos 1))
      )))

(de reverse-search (pattern)

  % Search backward in the current buffer for the specified pattern.
  % If the pattern is found, the buffer cursor will be set to
  % the beginning of the matching string and T will be returned.  Otherwise,
  % the buffer cursor will remain unchanged and NIL will be returned.

  (let ((line-pos (current-line-pos))
	(char-pos (current-char-pos))
	found-pos
	)

    (while
      (and (>= line-pos 0)
	   (not (setf found-pos
		  (reverse-search-on-line line-pos char-pos pattern)))
	   )
      (setf line-pos (- line-pos 1))
      (setf char-pos NIL)
      )
    (if found-pos
	(progn (current-buffer-goto line-pos found-pos) T)))
    ))

(de reverse-search-on-line (line-pos char-pos pattern)

  % Search on the current line for the specified string.  If CHAR-POS is
  % non-NIL, then begin at that location, otherwise begin at the end of
  % the line.  We look to see if the string lies to the right of the current
  % search location.  If we find it, we return the CHAR-POS of the first
  % matching character.  Otherwise, we return NIL.

  (let* ((line (current-buffer-fetch line-pos))
	 (pattern-length (string-length pattern))
	 (limit (- (string-length line) pattern-length))
	 )
    (if (or (null char-pos) (> char-pos limit))
      (setf char-pos limit))
    (while (>= char-pos 0)
      (if (pattern-matches-in-line pattern line char-pos)
	(exit char-pos)
	)
      (setf char-pos (- char-pos 1))
      )))

(de pattern-matches-in-line (pattern line pos)
  % Return T if PATTERN occurs as substring of LINE, starting at POS.
  % Ignore case differences.  No bounds checking is performed on LINE.

  (let ((i 0) (patlimit (string-upper-bound pattern)))
    (while (and (<= i patlimit)
		(= (string-fetch pattern i)
		   (char-upcase (string-fetch line (+ i pos))))
		)
      (setf i (+ i 1))
      )
    (> i patlimit) % T if all chars matched, NIL otherwise
    ))
