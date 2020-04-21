% IF macro
% Cris Perdue 8/19/82

(setq *usermode nil)

% Syntax of new IF is:
% (if <expr> [then <expr> ... ] [<elseif-part> ... ] [else <expr> ... ])
% <elseif-part> = elseif <expr> [then <expr> ... ]
% This syntax allows construction of arbitrary CONDs.
(defun construct-new-if (form)
  (let (
       (clause)
       (next-clause)
       (stmt (list 'cond))
       (e form))
    (while e
	   (cond
	    ((or (sym= (first e) 'if)
		 (sym= (first e) 'elseif))
	     (cond ((or (null (rest e))
			(not (or (null (rest (rest e)))
				 (sym= (third e) 'then)
				 (sym= (third e) 'else)
				 (sym= (third e) 'elseif))))
		    (error 0 "Can't expand IF.")))
	     (setq next-clause (next-if-clause e))
	     (setq clause
		   (cond ((and (rest (rest e))
			       (sym= (third e) 'then))
			  (cons (second e)
				(ldiff (pnth e 4) next-clause)))
			 (t (list (second e)))))
	     (nconc stmt (list clause))
	     (setq e next-clause)
	     (next))
	    ((sym= (first e) 'else)
	     (cond ((or (null (rest e)) (next-if-clause e))
		    (error 0 "Can't expand IF.")))
	     (nconc stmt (list (cons t (rest e))))
	     (exit))))
    stmt))

(defun next-if-clause (tail)
  (for (on x (rest tail))
       (do (cond ((or (sym= (first x) 'else)
		      (sym= (first x) 'elseif))
		  (return x))))
       (returns nil)))

(defun sym= (a b) (eq a b))

(defun ldiff (x y)
  (cond ((null x) nil)
	((eq x y) nil)
	(t (cons (first x) (ldiff (rest x) y)))))

% Checks for (IF <expr> <KEYWORD> . . .  ) form.  If keyword form,
% does fancy expansion, otherwise expands compatibly with MacLISP
% IF expression.  <KEYWORD> ::= THEN | ELSE | ELSEIF
(dm if (form)
  (let ((b (rest (rest form)))
	(test (second form)))
       (cond
	((or (sym= (first b) 'then)
	     (sym= (first b) 'else)
	     (sym= (first b) 'elseif))
	 (construct-new-if form))
	((eq (length b) 1) `(cond (,test ,(nth b 1))))
	(t `(cond (,test ,(nth b 1)) (t ,@(pnth b 2)))))))
