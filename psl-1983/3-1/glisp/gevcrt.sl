% GEVCRT.SL.9     07 April 83
% derived from <NOVAK>GEVCRT.PSL.1 20-Mar-83 12:41:24 

% Written by Gordon Novak Jr.
% Copyright (c) Hewlett-Packard 1983


(fluid '(n p))

(GLOBAL '(GEVMENUWINDOW GEVSAVEESC GEVSAVEGLQUIET GEVSAVEGCGAG GEVMOUSEAREA
          glquietflg gllispdialect gevtypenames gluserstrnames mouse terminal
))

(DE GEVENTER NIL
(setq gevsavegcgag !*GC)
(setq !*GC nil)
(SETQ GEVSAVEGLQUIET GLQUIETFLG)
(SETQ GLQUIETFLG T)
(echooff))


(DE GEVEXIT NIL
(setq !*GC gevsavegcgag)
(SETQ GLQUIETFLG GEVSAVEGLQUIET)
(echoon))


% edited: 19-Mar-83 22:41 
(DG GEVINITEDITWINDOW NIL
(PROG NIL (GEVWINDOW _ (A WINDOW WITH START =
			  (A VECTOR WITH X = 0 Y = 3)
			  SIZE =
			  (A VECTOR WITH X = 46 Y = 20)
			  TITLE = "GEV Structure Inspector"))
      (RETURN GEVWINDOW)))


% edited: 19-Mar-83 21:12 
% Wait in a loop for mouse actions within the edit window. 
(DG GEVMOUSELOOP NIL
(PROG (INP N TMP)
      LP
      (SEND GEVWINDOW MOVETOXY 0 -1)
      (SEND TERMINAL ERASEEOL)
      (SEND GEVWINDOW MOVETOXY 0 -1)
      (SEND TERMINAL PRINTSTRING "GEV: ")
      (echoon)
      (INP _ (READ))
      (echooff)
      (SEND TERMINAL ERASEEOL)
      (IF INP=T AND (N _ (READ))
	  IS NUMERIC THEN (GEVNSELECT N NIL)
	  (GO LP)
	  ELSEIF INP IS NUMERIC THEN (GEVNSELECT INP T)
	  (GO LP)
	  ELSEIF
	  (TMP _ (ASSOC INP '((Q QUIT)
			      (POP POP)
			      (E EDIT)
			      (PR PROGRAM)
			      (P PROP)
			      (A ADJ)
			      (I ISA)
			      (M MSG))))
	  THEN
	  (GEVCOMMANDFN (CADR TMP))
	  (IF (CADR TMP)
	      ='QUIT OR ~GEVACTIVEFLG THEN (SEND GEVWINDOW MOVETOXY 0 -1)
	      (SEND TERMINAL ERASEEOL)
	      (RETURN NIL)
	      ELSE
	      (GO LP))
	  ELSEIF INP = 'R
	  THEN
	  (SEND GEVWINDOW OPEN)
	  (GEVFILLWINDOW)
	  (GO LP)
	  ELSE
	  (PRIN1 "? Quit POP Edit PRogram Prop Adj Isa Msg Redraw")
	  (TERPRI)
	  (GO LP))))


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

