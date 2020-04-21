%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% STRING-SEARCH
%%%
%%% Author: Cris Perdue
%%% 11/23/82
%%% 
%%% General-purpose searches for substring.  Case is important.
%%% If the target is found, the index in the domain of the
%%% leftmost character of the leftmost match is returned,
%%% otherwise NIL.
%%%
%%% (STRING-SEARCH TARGET DOMAIN).
%%% 
%%% If passed two strings, Common LISP "search" will give the
%%% same results.
%%%
%%% (STRING-SEARCH-FROM TARGET DOMAIN START)
%%%
%%% Like string-search, but the search effectively starts at index
%%% START in the domain.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Implementation note: In both of these, the value of the first
%%% character of the target is precomputed and it is tested against
%%% characters of the domain separately from the other characters of
%%% the target.

(compiletime (load fast-int if))

(defun string-search (target domain)
  (if (not (and (stringp target) (stringp domain))) then
      (error 0 "Arg to string-search not a string"))
  (let* ((s (isizes target))
	 (m (- (isizes domain) s)))
    (if (= s -1) then 0
	else
	(let ((c (igets target 0)))
	  (for (from i 0 m)
	       (do (if (eq (igets domain i) c) then
		       (if
			(for (from u 1 s)
			     (from v (+ i 1))
			     (do (if (neq (igets target u)
					  (igets domain v)) then
				     (return nil)))
			     (finally (return t))) then
			(return i)))))))))

%%% Like string-search, but takes an explicit starting index
%%% in the domain string.

(defun string-search-from (target domain start)
  (if (not (and (stringp target) (stringp domain))) then
      (error 0 "Arg to substring-search not a string"))
  (let* ((s (isizes target))
	 (m (- (isizes domain) s)))
    (if (= s -1) then start
	else
	(let ((c (igets target 0)))
	  (for (from i start m)
	       (do (if (eq (igets domain i) c) then
		       (if
			(for (from u 1 s)
			     (from v (+ i 1))
			     (do (if (neq (igets target u)
					  (igets domain v)) then
				     (return nil)))
			     (finally (return t))) then
			(return i)))))))))

