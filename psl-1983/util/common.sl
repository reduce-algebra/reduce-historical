%
% COMMON.SL - Compile- and read-time support for Common Lisp compatibility.
%		In a few cases, actually LISP Machine Lisp compatibility?
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        31 March 1982
% Copyright (c) 1982 University of Utah
%

% Edit by Cris Perdue,  4 Feb 1983 1047-PST
% Removed ERRSET (redundant and not COMMON Lisp) and MOD (incorrect).
% <PSL.UTIL.NEWVERSIONS>COMMON.SL.2, 13-Dec-82 21:30:58, Edit by GALWAY
%    Fixed bugs in copylist and copyalist that copied the first element
%    twice.  Also fixed bug in copyalist where it failed to copy first pair
%    in the list.
%    Also started commenting the functions defined here.

% These are only the Common Lisp definitions that do not conflict with
% Standard Lisp or other PSL functions.  Currently growing on a daily basis

(imports '(useful fast-vector))

(compiletime
(defmacro cl-alias (sl-name cl-name)
  `(defmacro ,cl-name form
     `(,',sl-name . ,form)))

(flag '(expand-funcall* butlast-aux nbutlast-aux
	 left-expand left-expand-aux) 'internalfunction)

)

(cl-alias de defun)

(defmacro defvar (name . other)
  (if *defn (fluid (list name)))
  (if (atom other)
      `(fluid `(,',name))
      `(progn (fluid `(,',name))
	      (setq ,name ,(car other)))))

(cl-alias idp symbolp)

(cl-alias pairp consp)

(defun listp (x) (or (null x) (consp x)))

(put 'listp 'cmacro '(lambda (x) ((lambda (y) (or (null y) (consp y))) x)))

(cl-alias fixp integerp)

(cl-alias fixp characterp)

(put 'characterp 'cmacro '(lambda (x) (posintp x)))

(cl-alias vectorp arrayp)

(cl-alias codep subrp)

(defun functionp (x)
  (or (symbolp x) (codep x) (and (consp x) (eq (car x) 'lambda))))

(cl-alias eqn eql)

(cl-alias equal equalp)

(cl-alias valuecell symeval)

(defmacro fsymeval (symbol)
  `((lambda (***fsymeval***)
	    (or (cdr (getd ***fsymeval***))
		(stderror (bldmsg "%r has no function definition"
				  ***fsymeval***))))
    ,symbol))

(defmacro boundp (name)
  `(not (unboundp ,name)))

(defmacro fboundp (name)
  `(not (funboundp ,name)))

(defmacro macro-p (x)
  `(let ((y (getd ,x)))
        (if (and (consp y) (equal (car y) 'macro)) (cdr y) nil)))

(defmacro special-form-p (x)
  `(let ((y (getd ,x)))
        (if (and (consp y) (equal (car y) 'fexpr)) (cdr y) nil)))

(defmacro fset (symbol value)
  `(putd ,symbol 'expr ,value))

(defmacro makunbound (x)
  `(let ((y ,x) (makunbound y) y)))

(defmacro fmakunbound (x)
  `(let ((y ,x) (remd y) y)))

(defmacro funcall* (fn . args)
  `(apply ,fn ,(expand-funcall* args)))

(defun expand-funcall* (args)
  (if (null (cdr args))
      (car args)
      `(cons ,(car args) ,(expand-funcall* (cdr args)))))

(cl-alias funcall* lexpr-funcall)

% only works when calls are compiled right now
% need to make a separate special form and compiler macro prop.
(defmacro progv (symbols values . body)
  `(let ((***bindmark*** (captureenvironment)))
	(do ((symbols ,symbols (cdr symbols))
	     (values ,values (cdr values)))
	    ((null symbols) nil)
	  (lbind1 (car symbols) (car values)))
	(prog1 (progn ,@body)
	       (restoreenvironment ***bindmark***))))
       
(defmacro dolist (bindspec . progbody)
  `(prog (***do-list*** ,(first bindspec))
     (setq ***do-list*** ,(second bindspec))
$loop$
     (if (null ***do-list***)
         (return ,(if (not (null (cddr bindspec)))
		      (third bindspec)
		      ())))
     (setq ,(first bindspec) (car ***do-list***))
     ,@progbody
     (setq ***do-list*** (cdr ***do-list***))
     (go $loop$)))

(defmacro dotimes (bindspec . progbody)
  `(prog (***do-times*** ,(first bindspec))
     (setq ,(first bindspec) 0)
     (setq ***do-times*** ,(second bindspec))
$loop$
     (if (= ,(first bindspec) ***do-times***)
         (return ,(if (not (null (cddr bindspec)))
		      (third bindspec)
		      ())))
     (setq ,(first bindspec) (+ ,(first bindspec) 1))
     ,@progbody
     (go $loop$)))

(cl-alias map mapl)

% neither PROG or PROG* supports initialization yet
(cl-alias prog prog*)

(cl-alias dm macro)

% DECLARE, LOCALLY ignored now
(defmacro declare forms
  ())

(defmacro locally forms
  `(let () ,forms))

% version of THE which does nothing
(defmacro the (type form)
  form)

(cl-alias get getpr)

(cl-alias put putpr)

(cl-alias remprop rempr)

(cl-alias prop plist)

(cl-alias id2string get-pname)

(defun samepnamep (x y)
  (equal (get-pname x) (get-pname y)))

(cl-alias newid make-symbol)

(cl-alias internp internedp)

(defun plusp (x)
  (and (not (minusp x)) (not (zerop x))))

(defun oddp (x)
  (and (integerp x) (equal (remainder x 2) 1)))

(defun evenp (x)
  (and (integerp x) (equal (remainder x 2) 0)))

(cl-alias eqn =)

(cl-alias lessp <)

(cl-alias greaterp >)

(cl-alias leq <=)

(cl-alias geq >=)

(cl-alias neq /=)

(cl-alias plus +)

(defmacro - args
  (cond ((null (cdr args))
	 `(minus ,@args))
        ((null (cddr args))
	  `(difference ,@args))
	(t (left-expand args 'difference))))

(cl-alias times *)

(defmacro / args
  (cond ((null (cdr args))
	 `(recip ,(car args)))
        ((null (cddr args))
	 `(quotient ,@args))
	(t (left-expand args 'quotient))))

(defun left-expand (arglist op)
  (left-expand-aux `(,op ,(first arglist) ,(second arglist))
                    (rest (rest arglist))
		    op))

(defun left-expand-aux (newform arglist op)
  (if (null arglist) newform
      (left-expand-aux `(,op ,newform ,(first arglist))
	               (rest arglist)
		       op)))

(cl-alias add1 !1+)

(cl-alias sub1 !1-)

(cl-alias incr incf)

(cl-alias decr decf)

(defmacro logior args
  (robustexpand args 'lor 0))

(defmacro logxor args
  (robustexpand args 'lxor 0))

(defmacro logand args
  (robustexpand args 'land -1))

(cl-alias lnot lognot)

(cl-alias lshift ash)

(put 'ldb 'assign-op 'dpb)		% Not defined, but used in NSTRUCT

(put 'rplachar 'cmacro '(lambda (s i x) (iputs s i x)))

(put 'char-int 'cmacro '(lambda (x) x))

(put 'int-char 'cmacro '(lambda (x) x))

(put 'char= 'cmacro '(lambda (x y) (eq x y)))

(put 'char< 'cmacro '(lambda (x y) (ilessp x y)))

(put 'char> 'cmacro '(lambda (x y) (igreaterp x y)))

(cl-alias indx elt)

(cl-alias setindx setelt)

(defun copyseq (seq)
  (subseq seq 0 (+ (size seq) 1)))

(defun endp (x)
  (cond ((consp x) ())
        ((null x) t)
	(t (stderror (bldmsg "%r is not null at end of list" x)))))

(cl-alias length list-length)

(cl-alias reversip nreverse)

(cl-alias getv vref)

(cl-alias putv vset)

(put 'string= 'cmacro '(lambda (x y) (eqstr x y)))

(put 'string-length 'cmacro '(lambda (x) (iadd1 (isizes x))))

(put 'string-to-list 'cmacro '(lambda (x) (string2list x)))

(put 'list-to-string 'cmacro '(lambda (x) (list2string x)))

(put 'string-to-vector 'cmacro '(lambda (x) (string2vector x)))

(put 'vector-to-string 'cmacro '(lambda (x) (vector2string x)))

(put 'substring
     'cmacro
     '(lambda (s low high) (sub s low (idifference high (iadd1 low)))))

(defun nthcdr (n l)
  (do ((n n (isub1 n))
       (l l (cdr l)))
      ((izerop n) l)))

(cl-alias copy copytree)

(cl-alias pair pairlis)

(put 'make-string 'cmacro '(lambda (i c) (mkstring (isub1 i) c)))

(defmacro putprop (symbol value indicator)
  `(put ,symbol ,indicator ,value))

(defmacro defprop (symbol value indicator)
  `(putprop `,',symbol `,',value `,',indicator))

(defmacro eval-when (time . forms)
  (if *defn
      (progn (when (memq 'compile time) (evprogn forms))
	     (when (memq 'load time) `(progn ,@forms)))
      (when (memq 'eval time) `(progn ,@forms))))

% This name is already used by PSL /csp
% (defmacro case tail
%   (cons 'selectq tail)

% Selectq is actually a LISP Machine LISP name /csp
(defmacro selectq (on . s-forms)
  (if (atom on)
      `(cond ,@(expand-select s-forms on))
      `((lambda (***selectq-arg***)
		(cond ,@(expand-select s-forms '***selectq-arg***)))
	 ,on)))

(defun expand-select (s-forms formal)
  (cond ((null s-forms) ())
        (t `((,(let ((selector (first (first s-forms))))
		(cond ((consp selector)
		       `(memq ,formal `,',selector))
		      ((memq selector '(otherwise t))
			t)
		      (t `(eq ,formal `,',selector))))
	       ,@(rest (first s-forms)))
	      ,@(expand-select (rest s-forms) formal)))))

(defmacro comment form
  ())

(defmacro special args
  `(fluid `,',args))

(defmacro unspecial args
  `(unfluid `,',args))

(cl-alias atsoc assq)

(cl-alias lastpair last)

(cl-alias flatsize2 flatc)

(cl-alias explode2 explodec)

% swapf, exchf ...?


(defun nthcdr (n l)
  (do ((n n (isub1 n))
       (l l (cdr l)))
      ((izerop n) l)))


(defun tree-equal (x y)
  (if (atom x)
      (eql x y)
      (and (tree-equal (car x) (car y))
	   (tree-equal (cdr x) (cdr y)))))

% Return a "top level copy" of a list.
(defun copylist (x)
  (if (atom x)
      x
      (let* ((x1 (cons (car x) ()))
              (x (cdr x)))
	   (do ((x2 x1 (cdr x2)))
	       ((atom x) (rplacd x2 x) x1)
             (rplacd x2 (cons (car x) ()))
             (setq x (cdr x))))))

% Return a copy of an a-list (copy down to the pairs but no deeper).
(defun copyalist (x)
  (if (atom x)
      x
      (let* ((x1 (cons (cons (caar x) (cdar x)) ()))
              (x (cdr x)))
           (do ((x2 x1 (cdr x2)))
	       ((atom x) (rplacd x2 x) x1)
             (rplacd x2 (cons (cons (caar x) (cdar x)) ()))
             (setq x (cdr x))))))

(defun revappend (x y)
  (if (atom x) y
      (revappend (cdr x) (cons (car x) y))))

(defun nreconc (x y)
  (if (atom x) y
      (let ((z (cdr x)))
	(rplacd x y)
	(nreconc z x))))

(defun butlast (x)
  (if (or (atom x) (atom (cdr x))) x
      (butlast-aux x ())))

(defun butlast-aux (x y)
  (let ((z (cons (car x) y)))
    (if (atom (cddr x)) z
      (butlast-aux (cdr x) z))))

(defun nbutlast (x)
  (if (or (atom x) (atom (cdr x)))
      x
      (do ((y x (cdr y)))
	((atom (cddr y)) (rplacd y ())))
      x))

(defun buttail (list sublist)
  (if (atom list)
      list
      (let ((list1 (cons (car list) ())))
	   (setq list (cdr list))
	   (do ((list2 list1 (cdr list2)))
	       ((or (atom list) (eq list sublist)) list1)
	       (rplacd list2 (cons (car list) ()))
	       (setq list (cdr list))))))

(cl-alias substip nsubst)

(defmacro ouch (char . maybe-channel)
  (if maybe-channel
      `(channelwritechar ,(car maybe-channel) ,char)
      `(writechar ,char)))

(defmacro inch maybe-channel
  (if maybe-channel
      `(channelreadchar ,(car maybe-channel))
      `(readchar)))

(defmacro uninch (char . maybe-channel)
  (if maybe-channel
      `(channelunreadchar ,(car maybe-channel) ,char)
      `(unreadchar ,char)))

