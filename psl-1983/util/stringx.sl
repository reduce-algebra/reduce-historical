%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% STRINGX - Useful String Functions
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        9 September 1982
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load fast-int fast-strings common))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Private Macros:

(CompileTime (progn

(put 'make-string 'cmacro % temporary bug fix
  '(lambda (sz init)
	   (mkstring (- sz 1) init)))

)) % End of CompileTime

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de string-rest (s i)
  (substring s i (string-length s)))

(de string-pad-right (s desired-length)

  % Pad the specified string with spaces on the right side to the specified
  % length.  Returns a new string.

  (let ((len (string-length s)))
    (if (< len desired-length)
      (string-concat s (make-string (- desired-length len) #\space))
      s)))

(de string-pad-left (s desired-length)

  % Pad the specified string with spaces on the left side to the specified
  % length.  Returns a new string.

  (let ((len (string-length s)))
    (if (< len desired-length)
      (string-concat (make-string (- desired-length len) #\space) s)
      s)))

(de string-largest-common-prefix (s1 s2)

  % Return the string that is the largest common prefix of S1 and S2.

  (for (from i 0 (min (string-upper-bound s1) (string-upper-bound s2)) 1)
       (while (= (string-fetch s1 i) (string-fetch s2 i)))
       (returns (substring s1 0 i))
       ))

(de strings-largest-common-prefix (l)

  % Return the string that is the largest common prefix of the elements
  % of L, which must be a list of strings.

  (cond ((null l) "")
	((null (cdr l)) (car l))
	(t
	 (let* ((prefix (car l))
		(limit (string-length prefix))
		)
	   % Prefix[0..LIMIT-1] is the string that is a prefix of all
	   % strings so far examined.

	   (for (in s (cdr l))
		(with i)
		(do (let ((n (string-length s)))
		      (if (< n limit) (setf limit n))
		      )
		    (setf i 0)
		    (while (< i limit)
		      (if (~= (string-fetch prefix i) (string-fetch s i))
		        (setf limit i)
		        (setf i (+ i 1))
		        ))
		    ))
	   (substring prefix 0 limit)
	   ))))
