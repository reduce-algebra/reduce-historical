%
% CHAR-MACRO.SL - Character constant macro
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        10 August 1981
% Copyright (c) 1981 University of Utah
%

% Edit by Cris Perdue,  1 Feb 1983 1355-PST
% pk:char.red merged with the version in USEFUL.  Some symbolic names
% for characters removed (not needed, I hope).

(dm Char (U)		%. Character constant macro
  (DoChar (cadr U)))

% Table driven char macro expander
(de DoChar (u)
  (cond
    ((idp u) (or
	       (get u 'CharConst)
	       ((lambda (n) (cond ((lessp n 128) n))) (id2int u))
	       (CharError u)))
    ((pairp u) % Here's the real change -- let users add "functions"
      ((lambda (fn)
	 (cond
	   (fn (apply fn (list (dochar (cadr u)))))
	   (t (CharError u))))
       (cond ((idp (car u)) (get (car u) 'char-prefix-function)))))
    ((and (fixp u) (geq u 0) (leq u 9)) (plus u #\!0))
    (t (CharError u))))

(deflist
  `((lower ,(function (lambda(x) (lor x 2#100000))))
    (quote ,(function (lambda(x) x)))
    (control ,(function (lambda(x) (land x 2#11111))))
    (cntrl ,(function (lambda(x) (land x 2#11111))))
    (meta ,(function (lambda(x) (lor x 2#10000000)))))
  'char-prefix-function)

(de CharError (u)
  (ErrorPrintF "*** Unknown character constant: %r" u)
  0)

(DefList '((NULL 0)
	   (BELL 7)
	   (BACKSPACE 8)
	   (TAB 8#11)
	   (LF 8#12)
	   % (RETURN 8#12)	% RETURN is LF: it's end-of-line.  Out! /csp
	   (EOL 8#12)
	   (FF 8#14)
	   (CR 8#15)
	   (ESC 27)
	   (ESCAPE 27)
	   (BLANK 32)
	   (SPACE 32)
	   (RUB 8#177)
	   (RUBOUT 8#177)
	   (DEL 8#177)
	   (DELETE 8#177)
	   ) 'CharConst)
