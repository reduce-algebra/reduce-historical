% SET-MACROS.SL - macros for various flavors of assignments
%
% Author:      Don Morrison
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        Wednesday, 12 May 1982
% Copyright (c) 1981 University of Utah

% <PSL.UTIL>SET-MACROS.SL.2, 12-Oct-82 15:53:58, Edit by BENSON
% Added IGETV to SETF-SAFE list

% Somewhat expanded setf macro.  Major difference between this and the builtin
% version is that it always returns the RHS, instead of something 
% indeterminant.  Note that the setf-safe flag can be used to indicate that
% the assignment function itself returns the "right thing", so setf needn't
% do anything special.  Also a lot more functions are represented in this
% version, including c....r (mostly useful for macros) and list/cons (which
% gives a primitive sort of destructuring setf).

(defmacro setf u
  (cond
    ((atom u) nil)
    ((atom (cdr u)) (stderror "Odd number of arguments to setf."))
    ((atom (cddr u)) (setf2 (car u) (cadr u)))
    (t `(progn ,@(setf1 u)))))

(de setf1 (u)
  (cond
    ((atom u) nil)
    ((atom (cdr u)) (stderror "Odd number of arguments to setf."))
    (t (cons (setf2 (car u) (cadr u)) (setf1 (cddr u))))))

(de setf2 (lhs rhs)
  (if (atom lhs)
    `(setq ,lhs ,rhs)
    (cond
      ((and (idp (car lhs)) (flagp (car lhs) 'setf-safe))
	(expand-setf lhs rhs))
      ((atom rhs)
	`(progn ,(expand-setf lhs rhs) ,rhs))
      (t
	`(let ((***SETF-VAR*** ,rhs))
	   ,(expand-setf lhs '***SETF-VAR***)
	   ***SETF-VAR***)))))

(de expand-setf (lhs rhs)
  (let ((fn (car lhs)) (op))
    (cond
      ((and (idp fn) (setq op (get fn 'assign-op)))
	`(,op ,@(cdr lhs) ,rhs))
      ((and (idp fn) (setq op (get fn 'setf-expand)))
	(apply op (list lhs rhs)))
      ((and (idp fn) (setq op (getd fn)) (eqcar op 'macro))
	(expand-setf (apply (cdr op) (list lhs)) rhs))
      (t
	(expand-setf
	  (ContinuableError
	    99
	    (BldMsg "%r is not a known form for assignment" `(setf ,lhs ,rhs))
	    lhs)
	  rhs)))))

(flag '(getv indx eval value get list cons vector getd igetv) 'setf-safe)

(defmacro-no-displace car-cdr-setf (rplacfn pathfn)
  `#'(lambda (lhs rhs) `(,',rplacfn (,',pathfn ,(cadr lhs)) ,rhs)))
	       
(deflist '(
  (car rplaca)
  (cdr rplacd)
  (getv putv)
  (igetv iputv)
  (indx setindx)
  (sub setsub)
  (eval set)
  (value set)
  (get put)
  (flagp flag-setf)
  (getd getd-setf)
    ) 'assign-op)

(remprop 'nth 'assign-op) % Remove default version (which is incorrect anyway)

(deflist `(
  (caar ,(car-cdr-setf rplaca car))
  (cadr ,(car-cdr-setf rplaca cdr))
  (caaar ,(car-cdr-setf rplaca caar))
  (cadar ,(car-cdr-setf rplaca cdar))
  (caadr ,(car-cdr-setf rplaca cadr))
  (caddr ,(car-cdr-setf rplaca cddr))
  (caaaar ,(car-cdr-setf rplaca caaar))
  (cadaar ,(car-cdr-setf rplaca cdaar))
  (caadar ,(car-cdr-setf rplaca cadar))
  (caddar ,(car-cdr-setf rplaca cddar))
  (caaadr ,(car-cdr-setf rplaca caadr))
  (cadadr ,(car-cdr-setf rplaca cdadr))
  (caaddr ,(car-cdr-setf rplaca caddr))
  (cadddr ,(car-cdr-setf rplaca cdddr))
  (cdar ,(car-cdr-setf rplacd car))
  (cddr ,(car-cdr-setf rplacd cdr))
  (cdaar ,(car-cdr-setf rplacd caar))
  (cddar ,(car-cdr-setf rplacd cdar))
  (cdadr ,(car-cdr-setf rplacd cadr))
  (cdddr ,(car-cdr-setf rplacd cddr))
  (cdaaar ,(car-cdr-setf rplacd caaar))
  (cddaar ,(car-cdr-setf rplacd cdaar))
  (cdadar ,(car-cdr-setf rplacd cadar))
  (cdddar ,(car-cdr-setf rplacd cddar))
  (cdaadr ,(car-cdr-setf rplacd caadr))
  (cddadr ,(car-cdr-setf rplacd cdadr))
  (cdaddr ,(car-cdr-setf rplacd caddr))
  (cddddr ,(car-cdr-setf rplacd cdddr))
  (nth ,#'(lambda (lhs rhs) `(rplaca (pnth ,@(cdr lhs)) ,rhs)))
  (pnth ,#'expand-pnth-setf)
  (lastcar ,#'(lambda (lhs rhs) `(rplaca (lastpair ,(cadr lhs)) ,rhs)))
  (list ,#'list-setf)
  (cons ,#'cons-setf)
  (vector ,#'vector-setf)
    ) 'setf-expand)

(fluid '(*setf-debug))

(de expand-pnth-setf (lhs rhs)
  (let ((L (cadr lhs))(n (caddr lhs)))
    (cond
      ((onep n) `(setf ,L ,rhs))
      ((fixp n) `(rplacd (pnth ,L (sub1 ,n)) ,rhs))
      (t
	(let ((expnsn (errorset `(setf2 ',L ',rhs) *setf-debug *setf-debug)))
	  (if (atom expnsn)
	    `(rplacd (pnth ,L (sub1 ,n) ,rhs))
	    `(let ((***PNTH-SETF-VAR*** ,n))
	       (if (onep ***PNTH-SETF-VAR***)
		 ,(car expnsn)
		 (rplacd (pnth ,L (sub1 ***PNTH-SETF-VAR***)) ,rhs)))))))))

(de flag-setf (nam flg val)
  (cond
    (val (flag (list nam) flg) t)
    (t (remflag (list nam) flg) nil)))

(de getd-setf (trgt src)
  (cond
% not correct for the parallel case...
%   ((idp src) (copyd trgt src))
    ((or (codep src) (eqcar src 'lambda)) % is this kludge worthwhile?
      (progn (putd trgt 'expr src) (cons 'expr src)))
    ((pairp src)
      (progn (putd trgt (car src) (cdr src)) src))
    (t
      (ContinuableError
	99
	(bldmsg "%r is not a funtion spec." src)
	src))))

(de list-setf (lhs rhs)
  (if (atom rhs)
    `(progn ,.(destructure-form (cdr lhs) rhs) ,rhs)
    `(let ((***LIST-SETF-VAR*** ,rhs)) 
       ,.(destructure-form (cdr lhs) '***LIST-SETF-VAR***)
       ***LIST-SETF-VAR***)))

(de cons-setf (lhs rhs)
  (if (atom rhs)
    `(progn
       (setf ,(cadr lhs) (car ,rhs))
       (setf ,(caddr lhs) (cdr ,rhs))
       ,rhs)
    `(let ((***CONS-SETF-VAR*** ,rhs))
       (setf ,(cadr lhs) (car ***CONS-SETF-VAR***))
       (setf ,(caddr lhs) (cdr ***CONS-SETF-VAR***))
       ***CONS-SETF-VAR***)))

(de vector-setf (lhs rhs)
  (let ((x (if (atom rhs) rhs '***VECTOR-SETF-VAR***)))
    (let ((L (for (in u (cdr lhs)) (from i 0)
	       (collect `(setf ,u (getv ,x ,i))))))
      (if (atom rhs)
	`(progn ,.L ,x)
	`(let ((***VECTOR-SETF-VAR*** ,rhs)) ,.L ,x)))))

% Some more useful assignment macros

(defmacro push (item stack) `(setf ,stack (cons ,item ,stack)))

(defmacro pop (stack . rst)
  (let ((x `(prog1 (car ,stack) (setf ,stack (cdr ,stack)))))
    (if rst `(setf ,(car rst) ,x) x)))

(defmacro adjoin-to (e s) `(setf ,s (adjoin ,e ,s)))

(defmacro adjoinq-to (e s) `(setf ,s (adjoinq ,e ,s)))

(defmacro incr (var . rst)
  `(setf ,var ,(if rst `(plus ,var ,@rst) `(add1 ,var))))

(defmacro decr (var . rst)
  `(setf ,var ,(if rst `(difference ,var (plus ,@rst)) `(sub1 ,var))))

(defmacro clear L
  `(setf ,.(foreach u in L conc `(,u nil))))

% Parallel assignment macros

(defmacro psetq rst
% psetq looks like a multi-arg setq but does its work in parallel.
     (cond ((null rst) nil)
           ((cddr rst)
	    `(setq ,(car rst)
		   (prog1 ,(cadr rst) (psetq . ,(cddr rst)))))
           % the last pair.  keep it simple;  no superfluous
	   % (prog1 (setq...) (psetq)).
	   ((cdr rst) `(setq . ,rst))
	   (t (StdError "psetq passed an odd number of arguments"))))

(defmacro psetf rst
% psetf looks like a multi-arg setf but does its work in parallel.
     (cond ((null rst) nil)
           ((cddr rst)
	    `(setf ,(car rst)
		   (prog1 ,(cadr rst) (psetf . ,(cddr rst)))))
	   ((cdr rst) `(setf . ,rst))
	   (t (StdError "psetf passed an odd number of arguments"))))

(defmacro defswitch (nam var . acts)
  (let ((read-act (if (pairp acts) (car acts) nil))
	(set-acts (if (pairp acts) (cdr acts) nil)))
    (when (null var)
      (setf var (newid (bldmsg "%w-SWITCH-VAR*" nam)))) 
    `(progn
       (fluid '(,var))
       (de ,nam () (let ((,nam ,var)) ,read-act) ,var)
       (setf
	 (get ',nam 'assign-op)
	 #'(lambda (,nam) ,@set-acts (setq ,var ,nam)))
       (flag '(,nam) 'setf-safe))))

