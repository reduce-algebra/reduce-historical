% PR-DEMO.RED: A small 3D version Picture RLISP demo file
% See also the LISP syntax form in PR-DEMO.SL
% Use IN "PU:PR-DEMO.RED"$ for best effects

LOAD PRLISP;
HP!.INIT();  % For HP2648a

Outline := { 10, 10} _ {-10, 10} _            % Outline is 20 by 20 
          {-10,-10} _ { 10,-10} _ {10, 10}$   % Square

Arrow := {0,-1} _ {0,2}  &  {-1,1} _ {0,2} _ {1,1}$
                              
Cubeface   :=   (Outline & Arrow)  |  ZMOVE 10$

Cube   :=   Cubeface   
        &  Cubeface | XROT (180)  % 180 degrees
        &  Cubeface | YROT ( 90)
        &  Cubeface | YROT (-90)
        &  Cubeface | XROT ( 90)
        &  Cubeface | XROT (-90)$

% Make it larger for better viewing
BigCube := Cube | Scale 5$

% and show it
ESHOW  BigCube$

% Some more views

ESHOW  (BigCube | XROT 20 | YROT 30 | ZROT 10)$
ESHOW (Cube | scale 2 | XMOVE (-240) | REPEATED(5, XMOVE 80))$

% Some curves:

ESHOW {10,10} | circle(70)$
SHOW {10,10} | circle(50) | Xmove 20$

% Some control points for BSPLINE and BEZIER curves
Cpts := {0,0} _ {70,-60} _ {189,-69} _ {206,33} _ {145,130} _ {48,130}
       _ {0,84} $


ESHOW (Cpts & Cpts | BEZIER())$

ESHOW (Cpts & Cpts | BSPLINE())$

END;
