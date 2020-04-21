% ITER-MACROS.SL - macros for generalized iteration
%
% Author:      Don Morrison
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        Wednesday, 12 May 1982
% Copyright (c) 1981 University of Utah

% <PSL.UTIL>ITER-MACROS.SL.9, 15-Sep-82 17:06:49, Edit by BENSON
% Fixed typo, ((null (cdr result) nil)) ==> ((null (cdr result)) nil)

(defmacro do (iterators result . body)
  (let (vars steps)
    (setq vars
      (foreach U in iterators collect
	(if (and (pairp U) (cdr U) (cddr U))
	  (progn
	    (setq steps (cons
			  (if (atom (car U)) (car U) (caar U))
			  (cons (caddr U) steps)))
	    (list (car U) (cadr U)))
	  U)))
    (let ((form `(prog ()
		   ***DO-LABEL***
		   (cond
		     (,(car result)
		       (return ,(cond
				  ((null (cdr result)) nil)
				  ((and
				     (pairp (cdr result))
				     (null (cddr result)))
				    (cadr result))
				  (t `(progn ,@(cdr result)))))))
		   ,@body
		   (psetq ,.steps)
		   (go ***DO-LABEL***))))
      (if vars `(let ,vars ,form) form))))

(defmacro do* (iterators result . body)
  (let (vars steps)
    (setq vars
      (foreach U in iterators collect
	(if (and (pairp U) (cdr U) (cddr U))
	  (progn
	    (push
	      `(setq ,(if (atom (car U)) (car U) (caar U)) ,(caddr U))
	      steps)
	    (list (car U) (cadr U)))
	  U)))
    (let ((form `(prog ()
		   ***DO-LABEL***
		   (cond
		     (,(car result)
		       (return ,(cond
				  ((null (cdr result)) nil)
				  ((and
				     (pairp (cdr result))
				     (null (cddr result)))
				    (cadr result))
				  (t `(progn ,@(cdr result)))))))
		   ,@body
		   ,.(reversip steps)
		   (go ***DO-LABEL***))))
      (if vars `(let* ,vars ,form) form))))

(defmacro do-loop (iterators prologue result . body)
  (let (vars steps)
    (setq vars
      (foreach U in iterators collect
	(if (and (pairp U) (cdr U) (cddr U))
	  (progn
	    (setq steps (cons
			  (if (atom (car U)) (car U) (caar U))
			  (cons (caddr U) steps)))
	    (list (car U) (cadr U)))
	  U)))
    (let ((form `(prog ()
		   ,@prologue
		   ***DO-LABEL***
		   (cond
		     (,(car result)
		       (return ,(cond
				  ((null (cdr result)) nil)
				  ((and
				     (pairp (cdr result))
				     (null (cddr result)))
				    (cadr result))
				  (t `(progn ,@(cdr result)))))))
		   ,@body
		   (psetq ,.steps)
		   (go ***DO-LABEL***))))
      (if vars `(let ,vars ,form) form))))

(defmacro do-loop* (iterators prologue result . body)
  (let (vars steps)
    (setq vars
      (foreach U in iterators collect
	(if (and (pairp U) (cdr U) (cddr U))
	  (progn
	    (push
	      `(setq ,(if (atom (car U)) (car U) (caar U)) ,(caddr U))
	      steps)
	    (list (car U) (cadr U)))
	  U)))
    (let ((form `(prog ()
		   ,@prologue
		   ***DO-LABEL***
		   (cond
		     (,(car result)
		       (return ,(cond
				  ((null (cdr result)) nil)
				  ((and
				     (pairp (cdr result))
				     (null (cddr result)))
				    (cadr result))
				  (t `(progn ,@(cdr result)))))))
		   ,@body
		   ,.(reversip steps)
		   (go ***DO-LABEL***))))
      (if vars `(let* ,vars ,form) form))))

