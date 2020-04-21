%
% BIND-MACROS.SL - convenient macros for binding variables
%
% Author:      Don Morrison
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        Wednesday, 12 May 1982
% Copyright (c) 1981 University of Utah

% <PSL.UTIL>BIND-MACROS.SL.2, 18-Oct-82 14:31:17, Edit by BENSON
% Reversed vars and vals after collecting them in LET, so that the order
%  of things in the LAMBDA is the same as the LET.  Not necessary,
%  but it makes it easier to follow macroexpanded things.

(defmacro prog1 (first . body)
  (if (null body)
    first
    `((lambda (***PROG1-VAR***) ,@body ***PROG1-VAR***) ,first)))

(defmacro let (specs . body)
 (if (null specs)
   (cond
     ((null body) nil)
     ((and (pairp body) (null (cdr body))) (car body))
     (t `(progn ,@body)))
   (prog (vars vals)
     (foreach U in specs do
       (cond ((atom U)
	       (setq vars (cons U vars))
	       (setq vals (cons nil vals)))
	 (t
	   (setq vars (cons (car U) vars))
	   (setq vals (cons (and (cdr U) (cadr U)) vals)))))
     (return `((lambda ,(reversip vars) ,@body ) ,@(reversip vals))))))

(defmacro let* (specs . body)
 (if (null specs)
   (cond
     ((null body) nil)
     ((and (pairp body) (null (cdr body))) (car body))
     (t `(progn ,@body)))
   (let*1 specs body)))

(de let*1 (specs body)
 (let ((s (car specs))(specs (cdr specs)))
  `((lambda (,(if (atom s) s (car s)))
      ,@(if specs (list (let*1 specs body)) body))
    ,(if (and (pairp s) (cdr s)) (cadr s) nil))))

