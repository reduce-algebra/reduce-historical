% BACKQUOTE.SL - tool for building partially quoted lists
%
% Author:      Don Morrison
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        Wednesday, 12 May 1982
% Copyright (c) 1981 University of Utah

% Backquote is  similar  to MACLISP's  `  (that's backwards!)   mechanism.   In
% essence the  body  of  the  backquote is  quoted,  except  for  those  things
% surrounded by unquote, which are evaluated at macro expansion time.  UNQUOTEL
% splices in a  list, and  unquoted splices  in a  list destructively.   Mostly
% useful for defining macro's.

(dm backquote (u) (backquote-form (cadr u)))

(de backquote-form (u)
  (cond
    ((vectorp u) (backquote-vector u))
    ((atom u)
      (cond
	((and (idp u) (not (memq u '(t nil)))) (mkquote u))
	(t u)))
    ((eq (car u) 'unquote) (cadr u))
    ((eq (car u) 'backquote) (backquote-form (backquote-form (cadr u))))
    ((memq (car u) '(unquotel unquoted))
      (ContinuableError 99 (BldMsg "%r can't be spliced in here." u)) u)
    ((eqcar (car u) 'unquotel)
      (cond
	((cdr u) (list 'append (cadar u) (backquote-form (cdr u))))
	(t (cadar u))))
    ((eqcar (car u) 'unquoted)
      (cond
	((cdr u) (list 'nconc (cadar u) (backquote-form (cdr u))))
	(t (cadar u))))    
    (t (backquote-list u))))

(de backquote-vector (u)
  ((lambda (n rslt all-quoted)  % can't use LET 'cause it ain't defined yet
     ((lambda (i)
	(while (not (minusp i)) % can't use FOR or DO for the same reason
	  ((lambda (x)
	     (setq all-quoted (and all-quoted (backquote-constantp x)))
	     (setq rslt (cons x rslt)))
	    (backquote-form (getv u i)))
	  (setq i (sub1 i))))
       n)
     (cond
       (all-quoted
	 ((lambda (i vec)
	    (while (not (greaterp i n))
	      (putv vec i (backquote-constant-value (car rslt)))
	      (setq rslt (cdr rslt))
	      (setq i (add1 i)))
	    vec)
	   0
	   (mkvect n)))
       (t (cons 'vector rslt))))
    (upbv u)
    nil
    t))

(de backquote-list (u)
  ((lambda (car-u cdr-u)  % can't use LET 'cause it ain't defined yet
     (cond
       ((null cdr-u)
	 (cond
	   ((backquote-constantp car-u)
	     (list 'quoted-list (backquote-constant-value car-u)))
	   (t (list 'list car-u))))
       ((constantp cdr-u)
	 (cond
	   ((backquote-constantp car-u)
	     (list 'quoted-list* (backquote-constant-value car-u) cdr-u))
	   (t (list 'list* car-u cdr-u))))
       ((and (pairp cdr-u) (memq (car cdr-u) '(list list*)))
	 (cons (car cdr-u) (cons car-u (cdr cdr-u))))
       ((and
	  (pairp cdr-u)
	  (memq (car cdr-u) '(quoted-list quoted-list*)))
	 (cond
	   ((backquote-constantp car-u)
	     (cons
	       (car cdr-u)
	       (cons (backquote-constant-value car-u) (cdr cdr-u))))
	   (t (list
		'list*
		car-u
		(mkquote (backquote-constant-value cdr-u))))))
       ((eqcar cdr-u 'quote)
	 (cond
	   ((backquote-constantp car-u)
	      (list
	       'quoted-list*
	       (backquote-constant-value car-u)
	       (cadr cdr-u)))
	   (t (list 'list* car-u cdr-u))))
       (t (list 'list* car-u cdr-u))))
    (backquote-form (car u))
    (backquote-form (cdr u))))

(de backquote-constantp (u)
  (cond
    ((pairp u) (memq (car u) '(quote quoted-list quoted-list*)))
    (t (not (idp u)))))

(de backquote-constant-value (x)
  (cond
    ((eqcar x 'quote) (cadr x))
    ((eqcar x 'quoted-list) (cdr x))
    ((eqcar x 'quoted-list*)
      (cadr (apply 'quoted-list* (list x))))
    (t x)))

% The following, while possibly useful in themselves, are mostly included
% for use by backquote and friends.

(dm quoted-list (u) (mkquote (cdr u)))
  
(dm list* (u) (expand (cdr u) 'cons))

(dm quoted-list* (u)
  (cond
    ((pairp (cdr u))
      (setq u (reverse (cdr u)))
      ((lambda (a)
	 (foreach elem in (cdr u) do
	   (setq a (cons elem a)))
	 (mkquote a))
	(car u)))))
%     (t (error ... ?     

% Since unquote and friends should be completely stripped out by backquote,
% make it an error to try and evaluate them.  These could be much better...

(dm unquote (u) (ContinuableError
		  99
		  (BldMsg "%r is not within backquote." u)
		  u))

(copyd 'unquotel 'unquote)

(copyd 'unquoted 'unquote)
