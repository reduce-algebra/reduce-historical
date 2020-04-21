% CRT.SL.14     07 April 83
% derived from <NOVAK>H19.PSL.1 20-Mar-83 12:40:06 

% Written by Gordon Novak Jr.
% Copyright (c) 1983 Hewlett-Packard




(GLOBAL '(TERMINAL))


(GLISPOBJECTS


(TERMINAL ATOM
MSG     ((MOVETOXY TERMINAL-MOVETOXY)
	 (PRINTCHAR TERMINAL-PRINTCHAR OPEN T)
	 (PRINTSTRING TERMINAL-PRINTSTRING)
	 (INVERTVIDEO (nil)) 
		      
	 (NORMALVIDEO (nil))
		      
	 (GRAPHICSMODE (nil))
			
	 (NORMALMODE (nil))
		     
	 (ERASEEOL ((PBOUT (CHAR ESC))
		    (PBOUT (char K))))))

)



(GLISPGLOBALS
(TERMINAL TERMINAL)

)



(GLISPCONSTANTS
(BLANKCHAR 32 integer)
(HORIZONTALLINECHAR 45 integer)
(HORIZONTALBARCHAR 95 integer)
(LVERTICALBARCHAR 124 integer)
(RVERTICALBARCHAR 124 integer)
(escapechar 27 INTEGER)
)



% edited: 14-Mar-83 22:48 
% Move cursor to a specified X Y position. 
(DG TERMINAL-MOVETOXY (TERM:TERMINAL X:INTEGER Y:INTEGER)
(IF X<0 THEN X_0 ELSEIF X>79 X_79)(IF Y<0 THEN Y_0 ELSEIF Y>23 THEN Y_23)(SEND
  TERMINAL PRINTCHAR (CHAR ESC))(SEND TERMINAL PRINTCHAR (char Y))(SEND
  TERMINAL PRINTCHAR (55 - Y))(SEND TERMINAL PRINTCHAR
					      (32 + X)))


% edited: 19-Mar-83 20:29 
(DG TERMINAL-PRINTCHAR (TERM:TERMINAL S:STRING)
(PBOUT S))


% edited: 19-Mar-83 20:29 
(DG TERMINAL-PRINTSTRING (TERM:TERMINAL S:STRING)
  (prog (i n)
    (if s is not a string then (S _ (gevstringify s)))
    (n _ s:length)
    (i _ 0)
    (while (i<n) do (pbout (indx s i)) (i _+ 1)) ))


(SETQ TERMINAL 'VT52)





