
% <NOVAK>H19.PSL.1 20-Mar-83 12:40:06 





(GLISPOBJECTS


(TERMINAL ATOM
MSG     ((MOVETOXY TERMINAL-MOVETOXY)
	 (PRINTCHAR TERMINAL-PRINTCHAR OPEN T)
	 (PRINTSTRING TERMINAL-PRINTSTRING OPEN T)
	 (INVERTVIDEO ((PRIN1 ESCAPECHAR)
		       (PRIN1 "p")))
	 (NORMALVIDEO ((PRIN1 ESCAPECHAR)
		       (PRIN1 "q")))
	 (GRAPHICSMODE ((PRIN1 ESCAPECHAR)
			(PRIN1 "F")))
	 (NORMALMODE ((PRIN1 ESCAPECHAR)
		      (PRIN1 "G")))
	 (ERASEEOL ((PRIN1 ESCAPECHAR)
		    (PRIN1 "K")))))

)



(GLISPGLOBALS
(TERMINAL TERMINAL)

)



(GLISPCONSTANTS
(BLANKCHAR " " STRING)
(HORIZONTALLINECHAR "-" STRING)
(HORIZONTALBARCHAR "{" STRING)
(LVERTICALBARCHAR "}" STRING)
(RVERTICALBARCHAR "|" STRING)
(ESCAPECHAR (CHARACTER 27) STRING)
)



% edited: 14-Mar-83 22:48 
% Move cursor to a specified X Y position. 
(DG TERMINAL-MOVETOXY (TERM:TERMINAL X:INTEGER Y:INTEGER)
(IF X<0 THEN X_0 ELSEIF X>79 X_79)(IF Y<0 THEN Y_0 ELSEIF Y>23 THEN Y_23)(SEND
  TERMINAL PRINTCHAR ESCAPECHAR)(SEND TERMINAL PRINTCHAR "Y")(SEND
  TERMINAL PRINTCHAR (CHARACTER 55 - Y))(SEND TERMINAL PRINTCHAR
					      (CHARACTER 32 + X)))


% edited: 19-Mar-83 20:29 
(DG TERMINAL-PRINTCHAR (TERM:TERMINAL S:STRING)
(PRIN1 S))


% edited: 19-Mar-83 20:29 
(DG TERMINAL-PRINTSTRING (TERM:TERMINAL S:STRING)
(PRIN1 S))

(SETQ TERMINAL 'H19)