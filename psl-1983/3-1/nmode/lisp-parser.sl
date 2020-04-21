%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Lisp-Parser.SL - NMODE's Lisp parser
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        10 December 1982
% Revised:     18 February 1983
%
% See the document NMODE-PARSING.TXT for a description of the parsing strategy.
%
% 18-Feb-1983 Alan Snyder
%  Removed use of "obsolete" #\ names.
% 6-Jan-83 Alan Snyder
%   Use LOAD instead of FASLIN to get macros (for portability).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects fast-int fast-strings fast-vectors nmode-attributes))

% Imported variables:

(fluid '(nmode-defun-predicate
	 nmode-defun-scanner
	 nmode-current-parser
	 ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de establish-lisp-parser ()
  (setf nmode-defun-predicate #'lisp-current-line-is-defun?)
  (setf nmode-defun-scanner #'lisp-scan-past-defun)
  (setf nmode-current-parser #'lisp-parse-line)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This file defines the basic primitive used by NMODE to
% analyze Lisp source code.  It currently recognizes:
%
%      ( and ) as list brackets
%      [ and ] as vector brackets
%      comments beginning with %
%      #/x as character constants
%      " ... " as string literals
%      !x as a quoted character
%      ' ` #' #. , ,@ as prefixes to ( and [

(de lisp-parse-line (str vec)
  % Fill Vec[i] to be the attributes of Str[i].

  (let* ((previous-attributes -1)
	 attributes ch is-first
	 (high (string-upper-bound str))
	 (in-comment NIL)
	 (in-string NIL)
	 (last-was-sharp NIL)
	 (last-was-sharp-slash NIL)
	 (last-was-sharp-quote NIL)
	 (last-was-sharp-dot NIL)
	 (last-was-quoter NIL)
	 (last-was-comma NIL)
	 (last-was-comma-atsign NIL)
	 (last-prefix-ending-index NIL)
	 (last-prefix-length NIL)
	 )
    (for (from i 0 high)
	 (do
	  (setf ch (string-fetch str i))
	  % Determine the type attributes of the current character and update
	  % the parsing state for the next character.
	  (cond
	   (in-comment (setf attributes (attributes COMMENT)))
	   (in-string
	    (setf attributes (attributes ATOM))
	    (setf in-string (not (= ch #/")))
	    )
	   (last-was-sharp-slash
	    (setf attributes (attributes ATOM))
	    (setf last-was-sharp-slash NIL)
	    )
	   (last-was-quoter
	    (setf attributes (attributes ATOM))
	    (setf last-was-quoter NIL)
	    )
	   (t
	    (setf attributes (lisp-character-attributes ch))
	    (setf in-comment (= ch #/%))
	    (setf in-string (= ch #/"))
	    (setf last-was-sharp-slash (and last-was-sharp (= ch #//)))
	    (setf last-was-sharp-quote (and last-was-sharp (= ch #/')))
	    (setf last-was-sharp-dot (and last-was-sharp (= ch #/.)))
	    (setf last-was-sharp (= ch #/#))
	    (setf last-was-quoter (= ch #/!))
	    (setf last-was-comma-atsign (and last-was-comma (= ch #/@)))
	    (setf last-was-comma (= ch #/,))
	    (let ((prefix-length
		   (cond
		    (last-was-sharp-quote 2)
		    (last-was-sharp-dot 2)
		    ((= ch #/') 1)
		    ((= ch #/`) 1)
		    (last-was-comma 1)
		    (last-was-comma-atsign 1) % is 1 because comma is a prefix
		    (t 0)
		    )))
	      (when (> prefix-length 0)
		% We just passed a prefix.
		% Does it merge with the previous prefix?
		(if (and last-prefix-ending-index
			 (= last-prefix-ending-index (- i prefix-length))
			 )
		  (setf last-prefix-length (+ last-prefix-length prefix-length))
		  % Otherwise
		  (setf last-prefix-length prefix-length)
		  )
		(setf last-prefix-ending-index i)
		))
	    ))
	  % Determine the position attributes:
	  % LISP is simple: brackets are single characters (except for
	  % prefixes, which are handled below), atoms are maximal
	  % contiguous strings of atomic-characters.
	  (setf is-first (or (= attributes (attributes OPENER))
			     (= attributes (attributes CLOSER))
			     (~= attributes previous-attributes)))
	  (setf previous-attributes attributes)
	  (cond 
	   % First we test for an open bracket immediately preceded
	   % by one or more prefixes.
	   ((and (= attributes (attributes OPENER))
		 last-prefix-ending-index
		 (= last-prefix-ending-index (- i 1))
		 )
	    (let ((prefix-start (- i last-prefix-length)))
	      (vector-store vec prefix-start (attributes FIRST PREFIX OPENER))
	      (lp-set-last vec (- prefix-start 1))
	      (for (from j (+ prefix-start 1) (- i 1))
		   (do (vector-store vec j (attributes MIDDLE PREFIX OPENER))))
	      ))
	   (is-first
	    (setf attributes (| attributes (attributes FIRST)))
	    (lp-set-last vec (- i 1))
	    )
	   (t
	    (setf attributes (| attributes (attributes MIDDLE)))
	    ))
	  (vector-store vec i attributes)
	  ))
    (lp-set-last vec high)
    ))

(de lisp-character-attributes (ch)
  (selectq ch
    (NIL (attributes))
    ((#/( #/[) (attributes OPENER))
    ((#/) #/]) (attributes CLOSER))
    ((#\SPACE #\TAB #\LF #\CR) (attributes BLANKS))
    (#/% (attributes COMMENT))
    (t (attributes ATOM))
    ))

(de lp-set-last (vec i)
  (if (>= i 0)
    (vector-store vec i (& (| (attributes LAST) (vector-fetch vec i))
			   (~ (attributes MIDDLE))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lisp Defun Primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de lisp-current-line-is-defun? ()
  (and (not (current-line-empty?))
       (= (current-line-fetch 0) #/()
       ))

(de lisp-scan-past-defun ()
  % This function should be called with point at the start of a defun.
  % It will scan past the end of the defun (not to the beginning of the
  % next line, however).  If the end of the defun is not found, it returns
  % NIL and leaves point unchanged.

  (move-forward-form)
  )
