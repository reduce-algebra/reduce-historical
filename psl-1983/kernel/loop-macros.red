%
% LOOP-MACROS.RED - Various macros to make pure Lisp more tolerable
% 
% Author:      Eric Benson and M. Griss
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        5 October 1981
% Copyright (c) 1981 University of Utah
%

% Edit by MLG,9:35am  Tuesday, 29 December 1981
% Add EXIT, NEXT, REPEAT, add 'Join, improve FOR

macro procedure ForEach U;		%. Macro for MAP functions
%
% From RLISP
%
% Possible forms are:
% (foreach x in u do (foo x))	   --> (mapc u (function (lambda (x) (foo x))))
% (foreach x in u collect (foo x)) --> (mapcar u ...)
% (foreach x in u conc (foo x))	   --> (mapcan u ...)
% (foreach x in u join (foo x))	   --> (mapcan u ...)
% (foreach x on u do (foo x))	   --> (map u ...)
% (foreach x on u collect (foo u)) --> (maplist u ...)
% (foreach x on u conc (foo x))	   --> (mapcon u ...)
% (foreach x on u join (foo x))	   --> (mapcon u ...)
%
begin scalar Action, Body, Fn, Lst, Mod, Var;
    Var := cadr U;
    U := cddr U;
    Mod := car U;
    U := cdr U;
    Lst := car U;
    U := cdr U;
    Action := car U;
    Body := cdr U;
    Fn := if Action eq 'DO then
	      if Mod eq 'IN then 'MAPC else 'MAP
	  else if Action eq 'CONC or Action eq 'JOIN then
	      if Mod eq 'IN then 'MAPCAN else 'MAPCON
	  else if Action eq 'COLLECT then
	      if Mod eq 'IN then 'MAPCAR else 'MAPLIST
	  else StdError BldMsg("%r is an illegal action in ForEach", Action);
    return list(Fn, Lst,
		    list('FUNCTION, 'LAMBDA . list Var . Body))
end;

macro procedure Exit U;                 %. To leave current Iteration
    if null cdr U then
	'(return NIL)
    else if cddr U then
	list('return, 'progn . cdr U)
    else
	'return . cdr U;

macro procedure Next U;                 %. Continue Loop
    '(go !$Loop!$);			% no named DO's yet (no DO at all)

macro procedure While U;		%. Iteration macro
%
% From RLISP
% 
% Form is (while bool exp1 ... expN)
%
    'prog . '()
	. '!$Loop!$
	    . list('Cond, list(list('not, cadr U),
			       '(return NIL)))
	    . Append(cddr U, '((go !$Loop!$)));

macro procedure Repeat U;
%
% From RLISP
% Form is (repeat exp1 ... expN bool)
% Repeat until bool is true, similar to Pascal, etc.
%
       'prog . '() .
	  '!$Loop!$.
		for each X on cdr U collect
		    if null cdr X then
			list('Cond, list(list('not, car X),'(go !$Loop!$)))
		    else car X;

MACRO PROCEDURE FOR U;
%
% From RLISP
% 
% Form is (FOR (FROM var init final step) (key form))
%/ Limited right now to key=DO
   BEGIN SCALAR ACTION,BODY,EXP,INCR,RESULT,TAIL,VAR,X;
      VAR := second second U;
      INCR := cddr second U;  %(init final step)
      ACTION := first third U;
      BODY := second third U;
      RESULT := LIST LIST('SETQ,VAR,CAR INCR);
      INCR := CDR INCR;
      X := LIST('DIFFERENCE,first INCR,VAR);
      IF second INCR NEQ 1 THEN X := LIST('TIMES,second INCR,X);
      TAIL :='(RETURN NIL);
      IF NOT ACTION EQ 'DO
	THEN <<ACTION := GET(ACTION,'BIN);
		EXP := GENSYM();
		BODY := LIST('SETQ,EXP,
			      LIST(CAR ACTION,LIST('SIMP,BODY),EXP));
		RESULT := LIST('SETQ,EXP,MKQUOTE CDR ACTION) . RESULT;
		TAIL := LIST('RETURN, LIST('MK!*SQ,EXP));
		EXP := LIST EXP>>;
      RETURN ('PROG . 
              (VAR . EXP) .
                  NCONC(RESULT,
		'!$LOOP!$ .
		LIST('COND,LIST(LIST('MINUSP,X), TAIL)) .
		BODY .
		LIST('SETQ,VAR,LIST('PLUS2,VAR,second INCR)) .
		'((GO !$LOOP!$))
              ));
   END;


END;
