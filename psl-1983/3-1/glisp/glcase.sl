
% GSN 10-FEB-83 12:56 
% Compile code for Case statement. 
(DE GLDOCASE (EXPR)
(PROG
  (SELECTOR SELECTORTYPE RESULT TMP RESULTTYPE TYPEOK ELSECLAUSE TMPB)
  (SETQ TYPEOK T)
  (SETQ TMP (GLPUSHEXPR (LIST (CADR EXPR))
			NIL CONTEXT T))
  (SETQ SELECTOR (CAR TMP))
  (SETQ SELECTORTYPE (CADR TMP))
  (SETQ EXPR (CDDR EXPR))
  
% Get rid of of if present 

  (COND ((MEMQ (CAR EXPR)
	       '(OF Of of))
	 (SETQ EXPR (CDR EXPR))))
  A
  (COND
    ((NULL EXPR)
     (RETURN (LIST (GLGENCODE (CONS 'SELECTQ
				    (CONS SELECTOR (ACONC RESULT ELSECLAUSE))))
		   RESULTTYPE)))
    ((MEMQ (CAR EXPR)
	   '(ELSE Else
	      else))
     (SETQ TMP (GLPROGN (CDR EXPR)
			CONTEXT))
     (SETQ ELSECLAUSE (COND ((CDAR TMP)
			     (CONS 'PROGN
				   (CAR TMP)))
			    (T (CAAR TMP))))
     (SETQ EXPR NIL))
    (T
      (SETQ TMP (GLPROGN (CDAR EXPR)
			 CONTEXT))
      (SETQ
	RESULT
	(ACONC RESULT
	       (CONS (COND
		       ((ATOM (CAAR EXPR))
			(OR (AND (SETQ TMPB (GLSTRPROP SELECTORTYPE
						       'VALUES
						       (CAAR EXPR)
						       NIL))
				 (CADR TMPB))
			    (CAAR EXPR)))
		       (T (MAPCAR (CAAR EXPR)
				  (FUNCTION
				    (LAMBDA (X)
				      (OR (AND (SETQ TMPB (GLSTRPROP
						   SELECTORTYPE
						   'VALUES
						   X NIL))
					       (CADR TMPB))
					  X))))))
		     (CAR TMP))))))
  
% If all the result types are the same, then we know the result of the 
%   Case statement. 

  (COND (TYPEOK (COND ((NULL RESULTTYPE)
		       (SETQ RESULTTYPE (CADR TMP)))
		      ((EQUAL RESULTTYPE (CADR TMP)))
		      (T (SETQ TYPEOK NIL)
			 (SETQ RESULTTYPE NIL)))))
  (cond (expr (SETQ EXPR (CDR EXPR)) ))
  (GO A)))
