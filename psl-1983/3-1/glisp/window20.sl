
% {DSK}WINDOW.PSL;1  4-MAR-83 16:25:00 





(GLISPOBJECTS


(MENU (listobject (ITEMS (LISTOF ATOM))
                  (window window))
MSG     ((SELECT MENU-select RESULT ATOM)))


(MOUSE ANYTHING)


(WINDOW (listobject (start vector)
                    (size vector)
                    (title string)
                    (lastfilledline integer))

PROP    ((leftmargin (left + 1))
         (rightmargin (right - 2)))

MSG     ((CLEAR window-clear)
	 (OPEN window-open)
	 (CLOSE window-close)
         (movetoxy window-movetoxy)
         (invertvideo ((pbout escapechar)(pbout (char !p))))
         (normalvideo ((pbout escapechar)(pbout (char !q))))
         (graphicsmode (nil))
         (normalmode   (nil))
         (eraseeol ((pbout escapechar)(pbout (char K))))
	 (INVERTAREA WINDOW-INVERTAREA)
	 (MOVETO WINDOW-MOVETO)
	 (PRINTAT WINDOW-PRINTAT)
         (printatxy window-printatxy)
	 (PRETTYPRINTAT WINDOW-PRETTYPRINTAT)
	 (UNPRINTAT WINDOW-UNPRINTAT)
         (unprintatxy window-unprintatxy)
	 (DRAWLINE WINDOW-DRAWLINE)
         (drawlinexy window-drawlinexy)
	 (UNDRAWLINE WINDOW-UNDRAWLINE)
         (undrawlinexy window-undrawlinexy)
	 (CENTEROFFSET WINDOW-CENTEROFFSET))
supers (region) )

)

  

(GLISPGLOBALS
(MOUSE MOUSE)
)

(glispconstants
(windowcharwidth 8 integer)
(windowlineyspacing 12 integer)
(verticalbarchar 73 integer)
(horizontalbarchar 33 integer)
(escapechar 27 integer)
(blankchar 32 integer)
)

% Initialize graphics routines.
(dg window-init (w:window)
)

% Done with graphics
(dg window-term (w:window)
)



% Open a graphics window.
(dg window-open (w:window)
(prog (ttl nbl)
  (send w movetoxy w:left + 1 w:top)
  (ttl _ w:title or " ")
  (l _ ttl:length)
  (send w invertvideo)
  (if ttl:length > w:width - 2
      then (ttl _ (substring ttl 1 w:width - 2)))
  (nbl _ (w:width - ttl:length)/2 - 1)
  (printnc nbl blankchar)
  (prin2 ttl)
  (printnc (w:width - ttl:length - nbl - 2) blankchar)
  (send w normalvideo)
  (terpri)
  (w:lastfilledline _ w:bottom + 1)
  (send w movetoxy w:left w:top)
  (pbout verticalbarchar)
  (send w movetoxy w:right - 1 w:top)
  (pbout verticalbarchar)
  (send w movetoxy w:left w:bottom)
  (pbout verticalbarchar)
  (printnc w:width - 2 horizontalbarchar)
  (pbout verticalbarchar)
  (terpri)
  (send w clear)
  (send w movetoxy 0 2))
)

% Close a graphics window.
(dg window-close (w:window)

)


% GSN  2-FEB-83 13:57 
(DG WINDOW-CENTEROFFSET (W:WINDOW V:VECTOR)
(SEND W:REGION CENTEROFFSET V))


% GSN 28-FEB-83 16:10 
(DG WINDOW-DRAWLINE (W:WINDOW FROM:VECTOR TO:VECTOR)
  (if from:y=to:y then (send w moveto from)
                       (printnc (to:x - from:x + 1) horizontalbarchar)))


% GSN 28-FEB-83 16:58 
(DG WINDOW-INVERTAREA (W:WINDOW AREA:REGION)
  nil)


% GSN 13-JAN-83 15:29 
(DG WINDOW-MOVETO (W:WINDOW POS:VECTOR)
  (send w movetoxy pos:x pos:y))

% Move to position specified as separate x and y coordinates.
(dg window-movetoxy (w:window x:integer y:integer)
  (if x < 0 then (x _ 0) elseif x > 79 then (x _ 79))
  (if y < 0 then (y _ 0) elseif Y > 23 then (y _ 23))
  (pbout escapechar)
  (pbout (char Y))
  (pbout (55 - y))
  (pbout (32 + x)))

% GSN  2-MAR-83 13:58 
(DG WINDOW-PRETTYPRINTAT (W:WINDOW VALUE POSITION:VECTOR)
  (send w printat value position))


% GSN 13-JAN-83 16:25 
(DG WINDOW-PRINTAT (W:WINDOW S:STRING POS:VECTOR)
  (send w moveto pos)
  (prin2 s))


% GSN 28-FEB-83 16:10 
(DG WINDOW-unDRAWLINE (W:WINDOW FROM:VECTOR TO:VECTOR)
  (if from:y=to:y then (send w moveto from)
                       (printnc (to:x - from:x + 1) blankchar)))

% GSN 13-JAN-83 16:24 
(DG WINDOW-UNPRINTAT (W:WINDOW S:STRING POS:VECTOR)
  (send w moveto pos)
  (printnc s:length " "))

% Present a pop-up menu and select an item from it.    GSN   14 March 83
(dg menu-select (m:menu)
(prog (maxw i n)
  (maxw _ 0)
  (for x in m:items do (maxw _ (max maxw x:pname:length)))
  (maxw _ (min maxw 20))
  (m:window _ (a window with start = menustart
                   size = (a vector with x = (maxw + 5)* windowcharwidth
                            y = (min (length n:items) 19) * windowlineyspacing)
                   title = "Menu"))
  (send m:window open)
  (I _ 0)
  (for x in m:items do
    (i _+ 1)
    (send m:window printatxy (concat (gevstringify i)
                                     (if i<10 then "  " else " ")
                                     (gevstringify x))))
  (send m:window movetoxy 0 2)
  (send m:window eraseeol)
lp
  (send m:window movetoxy 0 2)
  (prin2 "Menu:")
  (n _ (read))
  (if n is integer and n > 0 and n <= (length m:items)
      then (return (nth m:items n))
      else (prin1 n)
           (prin2 " ?")
           (send m:window eraseeol)
           (go lp) )))

% Print the same character n times.
(dg printnc (n:integer c:integer)
  (while n > 0 do (n _- 1) (prin2 c)))

(dg window-clear (w:window)
  (prog (y)
    (y _ w:top - 1)
    (while y >= w:lastfilledline do
      (send w movetoxy w:left y)
      (prin2 verticalbarchar)
      (send w eraseeol)
      (send w movetoxy w:right - 1 y)
      (prin2 verticalbarchar)
      (y _- 1))
))