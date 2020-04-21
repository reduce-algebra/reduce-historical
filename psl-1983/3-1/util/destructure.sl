% DESTRUCTURE.SL - Tools for destructuring and macro definition
%
% Author:      Don Morrison
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        Wednesday, 12 May 1982
% Copyright (c) 1981 University of Utah

(de destructure-form (target path)
 (cond ((null target) nil)
       ((idp target)
	`((setq ,target ,path)))
       ((atom target)
	 (destructure-form
	   (ContinuableError 99 (BldMsg "Can't assign to %r" target) target)
	   path))
       (t (nconc
	    (destructure-form (car target) `(car ,path))
	    (destructure-form (cdr target) `(cdr ,path))))))

(de flatten (U)
 (cond ((null U) nil)
       ((atom U) (list U))
       ((null (car U)) (cons nil (flatten (cdr U))))
       (t (append (flatten (car U)) (flatten (cdr U))))))

(fluid '(*defmacro-displaces))

((lambda (ub-flg)
   (fluid '(*macro-displace))
   (cond (ub-flg (setq *macro-displace t)))) % Only do if not already set
 (unboundp '*macro-displace))
	     
(de defmacro-1 (U)
% This, too, can be made more efficient if desired.  Seems unnecessary, though.
  `(dm ,(cadr U) (***DEFMACRO-ARG***)
     (prog ,(flatten (caddr U))
       ,.(destructure-form (caddr U) '(cdr ***DEFMACRO-ARG***))
       (return ,(cond
		  (*defmacro-displaces
		    `(macro-displace ***DEFMACRO-ARG*** (progn ,@(cdddr U))))
		  (t `(progn ,@(cdddr U))))))))

(de macro-displace (u v)
  (cond
    (*macro-displace
      (rplacw u `(!%displaced-macro
		   ',(cons (car u) (cdr u))
		   ,(macroexpand v))))
    (t v)))
  
(dm defmacro (u) (defmacro-1 u))
 
(dm defmacro-displace (u)
  ((lambda (*defmacro-displaces) (defmacro-1 u)) t))

(dm defmacro-no-displace (u)
  ((lambda (*defmacro-displaces) (defmacro-1 u)) nil))

(copyd '!%displaced-macro 'prog2)

(setf (get '!%displaced-macro 'compfn) #'&comprogn)

(defmacro desetq (U V)
% a destructuring setq - should be made more efficient and robust
 `((lambda (***DESETQ-VAR***)
       ,.(destructure-form U '***DESETQ-VAR***)
       ***DESETQ-VAR***)
   ,V))

(fluid '(*macro-debug))

(defmacro-no-displace deflambda (nam vars . bod)
  (if *macro-debug % T => deflambdas are functions and can be traced, etc.
    `(de ,nam ,vars ,@bod)
    `(defmacro ,nam ,vars
       `((lambda ,',vars ,.',bod) ,.(list ,@vars)))))
