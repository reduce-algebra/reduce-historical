% WINDOW.SL.10     28 March 83

% derived from {DSK}WINDOW.PSL;1  4-MAR-83 16:25:00 




(glispconstants

(screenxoffset -255 integer)
(screenyoffset -255 integer)
(screenxscale 256.0 real)
(screenyscale 256.0 real)
)



(GLISPOBJECTS


(MENU (listobject (ITEMS (LISTOF ATOM))
                  (window window))
MSG     ((SELECT MENU-select RESULT ATOM)))


(MOUSE ANYTHING)

(grpos integer
prop ((screenvalue ((self  + screenxoffset) / screenxscale ))))

(grvector (list (x grpos) (y grpos))
  supers (vector))

(WINDOW (listobject (start grvector)
                    (size grvector)
                    (title string)
                    (lastfilledline integer)
                    (lastposition grvector))

PROP    ((leftmargin (left + 1))
         (rightmargin (right - 2)))

MSG     ((CLEAR window-clear)
	 (OPEN window-open)
	 (CLOSE window-close)
         (movetoxy window-movetoxy OPEN T)
	 (INVERTAREA WINDOW-INVERTAREA)
	 (MOVETO WINDOW-MOVETO OPEN T)
	 (PRINTAT WINDOW-PRINTAT OPEN T)
         (printatxy window-printatxy)
	 (PRETTYPRINTAT WINDOW-PRETTYPRINTAT)
	 (UNPRINTAT WINDOW-UNPRINTAT OPEN T)
         (unprintatxy window-unprintatxy)
	 (DRAWLINE WINDOW-DRAWLINE OPEN T)
         (drawlinexy window-drawlinexy OPEN T)
	 (UNDRAWLINE WINDOW-UNDRAWLINE OPEN T)
         (undrawlinexy window-undrawlinexy OPEN T)
	 (CENTEROFFSET WINDOW-CENTEROFFSET))
supers (region) )

)

  

(GLISPGLOBALS
(MOUSE MOUSE)
)

(glispconstants
(windowcharwidth 8 integer)
(windowlineyspacing 20 integer)
)


(setq mouse 'mouse)
(setq gevmenuwindow nil)
(setq menustart (a vector with x = 320 y = 0))

% Initialize graphics routines.
(dg window-init (w:window)
 (prog ()
  (graphics-init)
  (color-display)
  (set-color white)
  (set-line-style solid)
  (set-char-size (quotient 7.0 screenxscale) (quotient 16.0 screenyscale))
))

% Done with graphics
(dg window-term (w:window)
  (prog ()
    (graphics-term)))


% Alias graphics function names without underline characters
(de graphics-init () (graphics_init))
(de graphics-term () (graphics_term))
(de display-init (unit mode) (display_init unit mode))
(de set-color (x) (set_color x))
(de set-line-style (x) (set_line_style x))
(de clear-display () (clear_display))
(de set-char-size (w h) (set_char_size w h))
(de set-text-rot (x y) (set_text_rot x y))
(de set-display-lim (x0 x1 y0 y1) (set_display_lim x0 x1 y0 y1))
(de set-viewport (x0 x1 y0 y1) (set_viewport x0 x1 y0 y1))
(de init-9111 () (init_9111))
(de sample-locator () (sample_locator))
(de await-locator () (await_locator))
(de color-display () (color_display))


% Clear a graphics window.
(dg window-clear (w:window)
)

% Open a graphics window.
(dg window-open (w:window)
(send w drawlinexy w:left w:bottom w:left w:top)
(send w drawlinexy w:left w:top w:right w:top)
(send w drawlinexy w:right w:top w:right w:bottom)
(send w drawlinexy w:right w:bottom w:left w:bottom)
)

% Open a graphics window.
(dg window-close (w:window)
(send w undrawlinexy w:left w:bottom w:left w:top)
(send w undrawlinexy w:left w:top w:right w:top)
(send w undrawlinexy w:right w:top w:right w:bottom)
(send w undrawlinexy w:right w:bottom w:left w:bottom)
)

% GSN  2-MAR-83 16:19 
(DG MOUSE-POSITIONIN (M:MOUSE W:WINDOW)
(GETMOUSESTATE)(A VECTOR WITH X = (LASTMOUSEX W)
		  Y = (LASTMOUSEY W)))


% GSN  2-MAR-83 16:19 
(DG MOUSE-TESTBUTTON (M:MOUSE BUTTON:INTEGER)
(GETMOUSESTATE)(NOT (ZEROP (LOGAND LASTMOUSEBUTTONS BUTTON))))


% GSN  2-FEB-83 13:57 
(DG WINDOW-CENTEROFFSET (W:WINDOW V:VECTOR)
(SEND W:REGION CENTEROFFSET V))


% GSN 28-FEB-83 16:10 
(DG WINDOW-DRAWLINE (W:WINDOW FROM:grVECTOR TO:grVECTOR)
  (send w drawlinexy from:x from:y to:x to:y))

(DG WINDOW-DRAWLINExy (W:WINDOW fromx:grpos fromy:grpos tox:grpos toy:grpos)
  (gdraw white solid fromx:screenvalue fromy:screenvalue
                     tox:screenvalue   toy:screenvalue))

% GSN 28-FEB-83 16:58 
(DG WINDOW-INVERTAREA (W:WINDOW AREA:REGION)
  nil)


% GSN 13-JAN-83 15:29 
(DG WINDOW-MOVETO (W:WINDOW POS:grVECTOR)
  (send w movetoxy pos:x pos:y))

% Move to position specified as separate x and y coordinates.
(dg window-movetoxy (w:window x:grpos y:grpos)
  (gmove x:screenvalue y:screenvalue))

% GSN  2-MAR-83 13:58 
(DG WINDOW-PRETTYPRINTAT (W:WINDOW VALUE POSITION:grVECTOR)
  (set-color white)
  (send w moveto pos)
  (w:lastposition _ position)
  (gtext value))


% GSN 13-JAN-83 16:25 
(DG WINDOW-PRINTAT (W:WINDOW S:STRING POS:grVECTOR)
  (set-color white)
  (send w moveto pos)
  (gtext s))

(DG WINDOW-PRINTATxy (W:WINDOW S:STRING x:grpos y:grpos)
  (set-color white)
  (send w movetoxy x y)
  (gtext s))


% GSN 28-FEB-83 16:11 
(DG WINDOW-UNDRAWLINE (W:WINDOW FROM:VECTOR TO:grVECTOR)
  (send w undrawlinexy from:x from:y to:x to:y))

(DG WINDOW-unDRAWLINExy (W:WINDOW fromx:grpos fromy:grpos tox:grpos toy:grpos)
  (gdraw background solid fromx:screenvalue fromy:screenvalue
                     tox:screenvalue   toy:screenvalue))


% GSN 13-JAN-83 16:24 
(DG WINDOW-UNPRINTAT (W:WINDOW S:STRING POS:grVECTOR)
  (set-color background)
  (send w moveto pos)
  (gtext s))

(DG WINDOW-UNPRINTATxy (W:WINDOW S:STRING x:grpos y:grpos)
  (set-color background)
  (send w movetoxy x y)
  (gtext s))

% Present a pop-up menu and select an item from it.    GSN   14 March 83
(dg menu-select (m:menu)
(prog (maxw i n saveglq result)
  (if ~gevactiveflg then (geventer))
  (saveglq _ glquietflg)
  (glquiteflg _ t)
  (maxw _ 0)
  (for x in m:items do (maxw _ (max maxw x:pname:length)))
  (maxw _ (min maxw 20))
  (m:window _ (a window with start = menustart
                   size = (a vector with x = (maxw + 5)* windowcharwidth
                            y = (min (length m:items) 19) * windowlineyspacing)
                   title = "Menu"))
  (send m:window open)
  (I _ 0)
  (for x in m:items do
    (i _+ 1)
    (send m:window printatxy (concat (gevstringify i)
                                     (concat (if i<10 then "  " else " ")
                                             (gevstringify x)))
           1 (m:window:height - i * windowlineyspacing) ))
lp
  (prin2 "Menu:")
  (n _ (read))
  (if n is integer and n > 0 and n <= (length m:items)
      then (result _ (car (PNth m:items n))) (go out)
      elseif n = 'q then (result _ nil) (go out)
      else (prin1 n)
           (prin2 " ?")
           (terpri)
           (go lp) )
out
  (setq glquietflg saveglq)
  (if ~gevactiveflg then (gevexit))
  (return result)
))
