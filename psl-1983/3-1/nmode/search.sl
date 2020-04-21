%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Search.SL - Search utilities
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        23 August 1982
% Revised:     5 April 1983
%
% 19-June-83 Mark R. Swanson
% Added PATTERN-STARTS-IN-LINE to traverse entire line looking for first
% character of PATTERN; this avoids many, many procedure calls.
% 5-Apr-83  Nancy Kendzierski
% Removed extra right parenthesis at end of forward-search and reverse-search.
% 5-April-83 Jeff Soreff
% Forward-Search-In-String was added to allow use of searching within a
% string, as well as within a buffer.
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
    )

(de forward-search-in-string (string pattern)
  % Search in the string for the specified pattern, starting at the
  % beginning of the string.  If we find it, we return the CHAR-POS of
  % the first matching character.  Otherwise, we return NIL.
  (let* ((pattern-length (string-length pattern))
	 (limit (- (string-length string) pattern-length))
	 (char-pos 0))
    (while (<= char-pos limit)
      (if (pattern-matches-in-line pattern string char-pos)
	(exit char-pos))
      (incr char-pos))))

(de forward-search-on-line (line-pos char-pos pattern)
  % Returns START-POSITION of pattern if it occurs in line, NIL otherwise.
  % Uses two subroutines: 
  %   PATTERN-STARTS-IN-LINE, which scans LINE for the first character of
  %      PATTERN, constrained by the length of pattern 
  %   PATTERN-MATCHES-IN-LINE, which tries to match PATTERN with contents of
  %     LINE at POS
  
  (let* ((line (current-buffer-fetch line-pos))
	 (pattern-length (string-length pattern))
	 (limit (- (string-length line) pattern-length))
	 (pattern-char (string-fetch pattern 0)) 
	 )
    (if (null char-pos) (setf char-pos 0))
    (while (<= char-pos limit)
      (setf char-pos (pattern-starts-in-line pattern-char limit line char-pos))
      (if (> char-pos limit)
	(exit nil))
      (if (pattern-matches-in-line pattern line char-pos)
	(exit char-pos))
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
    )

(de reverse-search-on-line (line-pos char-pos pattern)
  % Returns START-POSITION of pattern if it occurs in line, NIL otherwise.
  % Uses two subroutines: 
  %   REV-PATTERN-STARTS-IN-LINE, which scans LINE for the first character of
  %      PATTERN, constrained by the length of pattern 
  %   PATTERN-MATCHES-IN-LINE, which tries to match PATTERN with contents of
  %     LINE at POS
  
  (let* ((line (current-buffer-fetch line-pos))
	 (pattern-length (string-length pattern))
	 (limit (- (string-length line) pattern-length))
	 (pattern-char (string-fetch pattern 0)) 
	 )
    (if (or (null char-pos) (> char-pos limit))
      (setf char-pos limit))
    (while (>= char-pos 0)
      (setf char-pos (rev-pattern-starts-in-line pattern-char line char-pos))
      (if (< char-pos 0)
	(exit nil))
      (if (pattern-matches-in-line pattern line char-pos)
	(exit char-pos))
      (setf char-pos (- char-pos 1))
      )))

(de pattern-starts-in-line (pattern-char search-limit line pos)
  % Return position if PATTERN-CHAR occurs in LINE, with sufficient room 
  % for rest of pattern; start looking at POS.
  % Ignore case differences.  No bounds checking is performed on LINE.

  (let ((i pos))
    (while (<= i search-limit)
      (if (= pattern-char
	     %(char-upcase (string-fetch line i))
	     (let ((xchar (string-fetch line i)))
	       (cond
		((< xchar #/a) xchar)
		((> xchar #/z) xchar)
		(T
		 (- xchar 32)))))
	(exit i)
	(setf i (+ i 1))))
      (exit i) % nothing matched, i > limit will indicate such
      ))

(de rev-pattern-starts-in-line (pattern-char line pos)
  % Return position if PATTERN-CHAR occurs in LINE, with sufficient room 
  % for rest of pattern; start looking at POS.
  % Ignore case differences.  No bounds checking is performed on LINE.

  (let ((i pos))
    (while (>= i 0)
      (if (= pattern-char
	     %(char-upcase (string-fetch line i))
	     (let ((xchar (string-fetch line i)))
	       (cond
		((< xchar #/a) xchar)
		((> xchar #/z) xchar)
		(T
		 (- xchar 32)))))
	(exit i)
	(setf i (- i 1))))
      (exit i) % nothing matched, i > limit will indicate such
      ))

(de pattern-matches-in-line (pattern line pos)
  % Return T if PATTERN occurs as substring of LINE, starting at POS.
  % Ignore case differences.  No bounds checking is performed on LINE.

  (let ((i 0) (patlimit (string-upper-bound pattern)))
    (while (and (<= i patlimit)
		(= (string-fetch pattern i)
                  %(char-upcase (string-fetch line (+ i pos)))
		   (let ((xchar (string-fetch line (+ i pos))))
		     (cond
		      ((< xchar #/a) xchar)
		      ((> xchar #/z) xchar)
		      (T
		       (- xchar 32))))
		   )
		)
      (setf i (+ i 1))
      )
    (> i patlimit) % T if all chars matched, NIL otherwise
    ))

