% GEVAUX.SL.21     28 March 83
% Auxiliary functions for PSL version of GEV, HP 9836 version.
% GSN   07 March 83

% Interlisp Substring function.
(de substring (string first last)
    (cond ((not (stringp string)) (setq string (gevstringify string))))
    (cond ((minusp first)
             (setq first (add1 (plus (add1 (size string)) first)))))
    (cond ((minusp last)
             (setq last (add1 (plus (add1 (size string)) last)))))
    (subseq string (sub1 first) last) )


% Make a string out of anything
(de gevstringify (x)
  (cond ((stringp x) x)
        (t (bldmsg "%p" x))))



% Concatenate an arbitrary number of items
(de concatn (l)
  (cond ((null l) "")
        ((null (cdr l)) (gevstringify (car l)))
        (t (concat (gevstringify (car l)) (concatn (cdr l))))))

(de concatln (l)
  (cond ((null l) "")
        ((null (cdr l)) (gevstringify (eval (car l))))
        (t (concat (gevstringify (eval (car l))) (concatln (cdr l))))))

(df concatl (concatlarg) (concatln concatlarg))
(de gevconcat (l) (concatn l))

(de dreverse (l) (reversip l))

(de mkatom (s) (intern s))

(de gevputd (fn form)
  (put fn 'gloriginalexpr (cons 'lambda (cdr form)))
  (put fn 'glcompiled nil)
  (remd fn)
  (putd fn 'macro '(lambda (gldgform) (glhook gldgform))))

% Apply a function to arguments, Glisp-compiling first if needed.
(de gevapply (fn args)
  (cond ((and (atom fn)
              (or (null (get fn 'glcompiled))
                  (not (eq (getddd fn) (get fn 'glcompiled)))))
           (glcc fn)
           (apply fn args))
        (t (apply fn args))))


% TTY input replacement for mouse operations.
% GSN   07 March 83
(dg gevmouseloop ()
  (prog (input n tmp)
lp  (prin2 "GEV: ")
    (input _ (read))
    (if input='t and (n _ (read))
                      is numeric then (gevnselect n nil)
                              (go lp)
                 elseif input is numeric
                   then (gevnselect input t) (go lp)
                 elseif (tmp _ (assoc input
       '((q  quit)(pop  pop)(e  edit)(pr  program)
         (p prop)(a  adj)(i  isa)(m  msg))))
                   then (gevcommandfn (cadr tmp))
                        (if (cadr tmp)='quit or ~gevactiveflg
                            then (return nil)
                            else (go lp)))
err (prin2 "?   Quit POP Edit PRogram Prop Adj Isa Msg")
    (terpri)
    (go lp) ))


% GEVCRT.SL.4     28 March 83
% derived from <NOVAK>GEVCRT.PSL.1 20-Mar-83 12:41:24 





(GLOBAL '(GEVMENUWINDOW GEVSAVEESC GEVSAVEGLQUIET GEVSAVEGCGAG GEVMOUSEAREA))

(DE GEVENTER NIL
  (setq gevsavegcgag !*GC)
  (setq !*GC nil)
  (SETQ GEVSAVEGLQUIET GLQUIETFLG)
  (SETQ GLQUIETFLG T)
  (window-init nil))


(DE GEVEXIT NIL
  (setq !*GC gevsavegcgag)
  (SETQ GLQUIETFLG GEVSAVEGLQUIET)
  (window-term nil))


% edited: 19-Mar-83 22:41 
(DG GEVINITEDITWINDOW NIL
(PROG NIL (GEVWINDOW _ (A WINDOW WITH START =
			  (A VECTOR WITH X = 0 Y = 0)
			  SIZE =
			  (A VECTOR WITH X = 300 Y = 500)
			  TITLE = "GEV Structure Inspector"))
      (RETURN GEVWINDOW)))



% edited: 19-Mar-83 21:42 
% Select the Nth item in the display and push down to zoom in on it. 
(DG GEVNSELECT (N:INTEGER FLAG:BOOLEAN)
(PROG (L TOP SUBLIST GROUP ITEM)
      (GROUP _ 0)
      (TOP _ GEVEDITCHAIN:TOPFRAME)
      LP
      (IF ~TOP THEN (RETURN NIL))
      (SUBLIST -_ TOP)
      (GROUP _+ 1)
      (IF GROUP=1 AND (L _ (LENGTH SUBLIST))
	  >=N THEN (ITEM _ (CAR (PNth SUBLIST (L + 1 - N))))
	  ELSEIF ~ (ITEM _ (GEVNTHITEM SUBLIST))
	  THEN
	  (GO LP))
      (IF ITEM:NODETYPE <= '(STRUCTURE SUBTREE LISTOF)
	  THEN
	  (RETURN NIL)
	  ELSE
	  (RETURN (GEVITEMEVENTFN ITEM GROUP FLAG)))))


% edited: 19-Mar-83 22:15 
% Find the Nth item in a tree structure of items. 
(DG GEVNTHITEM (L: (LISTOF GSEITEM))
(GLOBAL N:INTEGER)(PROG (TMP RES)
			(IF N<=0 THEN (ERROR 0 NIL)
			    ELSEIF ~L THEN (RETURN NIL)
			    ELSEIF N=1 THEN (RETURN (CAR L))
			    ELSE
			    (N _- 1)
			    (TMP -_ L)
			    (IF TMP:NODETYPE <= '(STRUCTURE SUBTREE LISTOF)
				AND
				(RES _ (GEVNTHITEM TMP:SUBVALUES))
				THEN
				(RETURN RES)
				ELSE
				(RETURN (GEVNTHITEM L))))))


(GLISPCONSTANTS
(GEVNUMBERCHARS 2 INTEGER)
(GEVNUMBERPOS 1 INTEGER)
)


(SETQ GEVMENUWINDOW NIL)

(SETQ GEVMOUSEAREA NIL)

