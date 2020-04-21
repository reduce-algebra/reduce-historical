% CIRCLE.SL.3     31 Jan. 83
% Test program to draw a circle on a graphics screen.
% G. Novak

(DG CIRCLE
   (XSTART:integer YSTART:integer RADIUS:INTEGER)
%          (* edited: "19-MAR-82 16:31")
%          (* Draw a circle incrementally.)
   (PROG (X Y YLAST DELTA NP2)
         (X_RADIUS)
         (Y_0)
         (DELTA_0)
         (WHILE Y<X DO (YLAST_Y)
		       (DELTA _+
			      X + X - 1)
		       (WHILE DELTA>0 DO (DELTA _-
						Y+Y+1)
					 (Y_+1))
		       (NP2 _(Y - YLAST + 1)/2)
		       (WHILE NP2>0 DO (NP2_-1)
			       (DRAWCIRCLEPOINT X YLAST XSTART YSTART)
				       (YLAST_+1))
		       (X_-1)
		       (WHILE YLAST<Y DO
                          (DRAWCIRCLEPOINT X YLAST XSTART YSTART)
					 (YLAST_+1)))))

% for testing:
(de drawcirclepoint (x y xstart ystart)
   (prin1 x)(prin2 '! )(print y))

(dg oldDRAWCIRCLEPOINT
   (X:integer Y:integer XSTART:integer YSTART:INTEGER)
%          (* edited: "19-MAR-82 15:40")
   (BITMAPBIT XSTART+X YSTART+Y 1)
   (BITMAPBIT (XSTART - X)
	      YSTART+Y 1)
   (BITMAPBIT (XSTART - X)
	      (YSTART - Y)
	      1)
   (BITMAPBIT XSTART+X (YSTART - Y)
	      1)
   (BITMAPBIT XSTART+Y YSTART+X 1)
   (BITMAPBIT XSTART+Y (YSTART - X)
	      1)
   (BITMAPBIT (XSTART - Y)
	      YSTART+X 1)
   (BITMAPBIT (XSTART - Y)
	      (YSTART - X)
	      1))

