%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Numeric-Operators.SL - Definitions of Numeric Operators with "Fast" Option
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        7 January 1983 (based on the earlier Fast-Int module)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load common useful))

% This file defines a set of C-like numeric operators that are a superset of the
% numeric operators defined by the Common Lisp compatibility package.

% The operators are:
%
%	=	Numeric Equal
%	~=	Numeric Not Equal
%	<	Numeric Less Than
%	>	Numeric Greater Than
%	<=	Numeric Less Than or Equal
%	>=	Numeric Greater Than or Equal
%	+	Numeric Addition
%	-	Numeric Minus or Subtraction
%	*	Numeric Multiplication
%	/	Numeric Division
%	//	Numeric Remainder
%	~	Integer Bitwise Logical Not
%	&	Integer Bitwise Logical And
%	|	Integer Bitwise Logical Or
%	^	Integer Bitwise Logical Xor
%	<<	Integer Bitwise Logical Left Shift
%	>>	Integer Bitwise Logical Right Shift

% The switch FAST-INTEGERS controls an option that provides for an efficient
% compiled implementation of these operators using Syslisp arithmetic.  When the
% switch is on, uses of these operators will compile into the corresponding
% Syslisp arithmetic operators, which generally are open-compiled and fast.
% However, the Syslisp operators perform machine arithmetic on untagged
% integers: they will work only if their inputs are untagged integers, and they
% produce untagged integer outputs.  The (undocumented) functions Int2Sys and
% Sys2Int can be used to convert between tagged Lisp integers and Syslisp
% integers; however, no conversion is needed to convert between INUMs and
% Syslisp integers within the valid range of INUMs.

% This module modifies the FOR macro to use the numeric operators to implement
% the FROM clause; thus, the FOR statement will use Syslisp arithmetic when the
% FAST-INTEGERS switch is on.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The Implementation:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Generic definitions of functions defined in the Common Lisp package:

(de = (a b) (EqN a b))
(de < (a b) (LessP a b))
(de > (a b) (GreaterP a b))
(de <= (a b) (LEq a b))
(de >= (a b) (GEq a b))
(de + (a b) (Plus2 a b))
(de * (a b) (Times2 a b))

(defmacro - args
  (cond ((null (cdr args))
	 `(fast-minus ,@args))
        ((null (cddr args))
	 `(fast-difference ,@args))
	(t (left-expand args 'fast-difference))))

(defmacro / args
  (cond ((null (cdr args))
	 `(recip ,(car args)))
        ((null (cddr args))
	 `(fast-quotient ,@args))
	(t (left-expand args 'fast-quotient))))

% Generic definitions of functions not defined by the Common Lisp package:

(de ~= (a b) (not (EqN a b)))
(de fast-minus (a) (Minus a))
(de fast-difference (a b) (Difference a b))
(de fast-quotient (a b) (Quotient a b))
(de // (a b) (Remainder a b))
(de ~ (a) (LNot a))
(de & (a b) (LAnd a b))
(de | (a b) (LOr a b))
(de ^ (a b) (LXor a b))
(de << (a b) (LShift a b))
(de >> (a b) (LShift a (Minus b)))

% Enable and Disable "fast" compiled definitions:

(fluid '(*fast-integers))
(put 'fast-integers 'simpfg '((T (enable-fast-numeric-operators))
			       (NIL (disable-fast-numeric-operators))
			       ))

(de enable-fast-numeric-operators ()
  (put '= 'cmacro '(lambda (a b) (WEQ a b)))
  (put '~= 'cmacro '(lambda (a b) (WNEQ a b)))
  (put '< 'cmacro '(lambda (a b) (WLessP a b)))
  (put '> 'cmacro '(lambda (a b) (WGreaterP a b)))
  (put '<= 'cmacro '(lambda (a b) (WLEQ a b)))
  (put '>= 'cmacro '(lambda (a b) (WGEQ a b)))
  (put '+ 'cmacro '(lambda (a b) (WPlus2 a b)))
  (put 'fast-difference 'cmacro '(lambda (a b) (WDifference a b)))
  (put 'fast-minus 'cmacro '(lambda (a) (WDifference 0 a)))
  (put '* 'cmacro '(lambda (a b) (WTimes2 a b)))
  (put 'fast-quotient 'cmacro '(lambda (a b) (WQuotient a b)))
  (put '// 'cmacro '(lambda (a b) (WRemainder a b)))
  (put '~ 'cmacro '(lambda (a) (WNot a)))
  (put '& 'cmacro '(lambda (a b) (WAnd a b)))
  (put '| 'cmacro '(lambda (a b) (WOr a b)))
  (put '^ 'cmacro '(lambda (a b) (WXor a b)))
  (put '<< 'cmacro '(lambda (a b) (WShift a b)))
  (put '>> 'cmacro '(lambda (a b) (WShift a (WDifference 0 b))))
  )

(de disable-fast-numeric-operators ()
  (remprop '= 'cmacro)
  (remprop '~= 'cmacro)
  (remprop '< 'cmacro)
  (remprop '> 'cmacro)
  (remprop '<= 'cmacro)
  (remprop '>= 'cmacro)
  (remprop '+ 'cmacro)
  (remprop 'fast-difference 'cmacro)
  (remprop 'fast-minus 'cmacro)
  (remprop '* 'cmacro)
  (remprop 'fast-quotient 'cmacro)
  (remprop '// 'cmacro)
  (remprop '~ 'cmacro)
  (remprop '& 'cmacro)
  (remprop '| 'cmacro)
  (remprop '^ 'cmacro)
  (remprop '<< 'cmacro)
  (remprop '>> 'cmacro)
  )

% Here we redefine the FROM clause of FOR statements:

(fluid '(for-vars* for-outside-vars* for-tests* for-prologue* for-conditions*
		   for-body* for-epilogue* for-result*))

(de for-from-function (clause)
  (let* ((var (car clause))
	 (var1 (if (pairp var) (car var) var))
	 (clause (cdr clause))
	 (init (if (pairp clause) (or (pop clause) 1) 1))
	 (fin (if (pairp clause) (pop clause) nil))
	 (fin-var (if (and fin (not (numberp fin))) (gensym) nil))
	 (step (if (pairp clause) (car clause) 1))
	 (step-var (if (and step (not (numberp step))) (gensym) nil)))
    (tconc
     for-vars*
     (list* var init (cond
		      (step-var `((+ ,var1 ,step-var)))
		      ((zerop step) nil)
		      ((onep step) `((+ ,var1 1)))
		      ((eqn step -1) `((- ,var1 1)))
		      (t `((+ ,var1 ,step))))))
    (if fin-var (tconc for-vars* `(,fin-var ,fin)))
    (if step-var (tconc for-vars* `(,step-var ,step)))
    (cond (step-var
	   (tconc for-tests* `(if (< ,step-var 0)
				(< ,var1 ,(or fin-var fin))
				(> ,var1 ,(or fin-var fin)))))
	  ((null fin))
	  ((minusp step) (tconc for-tests* `(< ,var1 ,(or fin-var fin))))
	  (t (tconc for-tests* `(> ,var1 ,(or fin-var fin)))))))
