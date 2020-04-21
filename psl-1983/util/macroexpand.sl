% MACROEXPAND.SL - tools for expanding macros in forms
%
% Author:      Don Morrison
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        Wednesday, 12 May 1982
% Copyright (c) 1981 University of Utah

%  <PSL.UTIL>MACROEXPAND.SL.15,  2-Sep-82 10:32:10, Edit by BENSON
%  Fixed multiple argument SETQ macro expansion

(defmacro macroexpand (form . macros)
 `(macroexpand1 ,form (list ,@macros)))

(fluid '(macroexpand-signal*))

(de macroexpand1 (U L)
  (let ((macroexpand-signal* nil)(*macro-displace nil))
    (while (null macroexpand-signal*)
      (setq macroexpand-signal* t)
      (setq U (macroexpand2 U L))))
  U)
    
(de macroexpand2 (U L)
  (cond
    ((or (atom U) (constantp (car U))) U)
    ((eqcar (car U) 'lambda)
      `((lambda ,(cadar U) ,.(foreach V in (cddar U)
			       collect (macroexpand2 V L)))
	 ,.(foreach V in (cdr U) collect (macroexpand2 V L))))
    ((not (idp (car U))) U)
    (t
      (let ((fn (getd (car U)))(spfn (get (car U) 'macroexpand-func)))
	(cond
	  (spfn (apply spfn (list U L)))
	  ((eqcar fn 'fexpr) U)
	  ((and (eqcar fn 'macro) (or (null L) (memq (car U) L)))
	    (setq macroexpand-signal* nil)
	    (apply (cdr fn) (list U)))
	  (t
	    (cons
	      (car U)
	      (foreach  V in (cdr U) collect (macroexpand2 V L)))))))))

(de macroexpand-cond (U L)
  (cons 'cond (foreach V in (cdr U) collect
		(foreach W in V collect (macroexpand2 W L)))))

(de macroexpand-prog (U L)
  `(prog ,(cadr U) ,.(foreach V in (cddr U) collect (macroexpand2 V L))))

(de macroexpand-random (U L)
  (cons (car U) (foreach V in (cdr U) collect (macroexpand2 V L))))

(deflist '( % Should probably add a bunch more...
  (prog macroexpand-prog)
  (progn macroexpand-random)
  (cond macroexpand-cond)
  (and macroexpand-random)
  (or macroexpand-random)
  (setq macroexpand-random)
  (function macroexpand-random)
           ) 'macroexpand-func)

(de macroexpand-loop ()
  (catch 'macroexpand-loop
    `(toploop
       ',(and toploopread* #'read)
       ',#'prettyprint
       ',#'(lambda (u) (if (atom u) (throw 'macroexpand-loop) (macroexpand u)))
       "expand"
       ',(bldmsg
	   "Entering macroexpand loop (atomic input forces exit) %w..."
	   (if (and
		 toploopread*
		 (idp toploopread*)
		 (not (eq toploopread* 'read)))
	     (bldmsg "[reading with %w]" toploopread*)
	     ""))))
    (printf "... Leaving macroexpand loop."))
