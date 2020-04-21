
% GSN 11-JAN-83 09:59 
% Create a function call to retrieve the field IND from a 
%   property-list type structure. FLG is true if a PROPLIST is inside 
%   an ATOM structure. 
(DE GLPROPSTRFN (IND DES DESLIST FLG)
(PROG (DESIND TMP RECNAME N)
      
% Handle a PROPLIST by looking inside each property for IND. 

      (COND ((AND (EQ (SETQ DESIND (pop DES))
		      'RECORD)
		  (ATOM (CAR DES)))
	     (SETQ RECNAME (pop DES))))
      (SETQ N 0)
      P
      (COND ((NULL DES)
	     (RETURN NIL))
	    ((AND (PAIRP (CAR DES))
		  (ATOM (CAAR DES))
		  (CDAR DES)
		  (SETQ TMP (GLSTRFN IND (CAR DES)
				     DESLIST)))
	     (SETQ TMP (GLSTRVAL
		     TMP
(glgencode     (CASEQ DESIND (ALIST (LIST 'GLGETASSOC
						(KWOTE (CAAR DES))
						'*GL*))
			    ((RECORD OBJECT)
			     (COND ((EQ DESIND 'OBJECT)
				    (SETQ N (ADD1 N))))
			     (LIST 'GetV
				   '*GL*
				   N))
			    ((PROPLIST ATOMOBJECT)
			     (LIST (COND ((OR FLG (EQ DESIND 'ATOMOBJECT))
					  'GETPROP)
					 (T 'LISTGET))
				   '*GL*
				   (KWOTE (CAAR DES))))))))

	     (RETURN TMP))
	    (T (pop DES)
	       (SETQ N (ADD1 N))
	       (GO P)))))

