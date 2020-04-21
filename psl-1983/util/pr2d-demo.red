% This is a small Picture RLISP demo file
% For the simpler 2D version

Load prlisp2d$

HP!.Init()$

Outline := { 10, 10} _ {-10, 10} _            % Outline is 20 by 20 
          {-10,-10} _ { 10,-10} _ {10, 10}$   % Square

Arrow := {0,-1} _ {0,2}  &  {-1,1} _ {0,2} _ {1,1}$
                              
Cube   :=   (Outline & Arrow)$

BigCube := Cube | Scale 5$

Eshow Cube$

Show Cube | Xmove 30$

SHOW  BigCube$

ESHOW BigCube | Zrot 30$

ESHOW {10,10} | circle(70)$

Cpts := {0,0} _ {70,-60} _ {189,-69} _ {206,33} _ {145,130} _ {48,130}
       _ {0,84} $

ESHOW ( {10,10} | CIRCLE(50))$

ESHOW (Cpts & Cpts | BEZIER())$

ESHOW (Cpts & Cpts | BSPLINE())$

ESHOW (Cube | scale 2 | XMOVE (-240) | REPEATED(5, XMOVE 80))$


ESHOW {0,0} | Text("ABC DEF")$

ESHOW {5,5} | Text("123 456") | Zrot 25 | Scale 2$

Eshow { 10,10} | Text("123")$

Show {30,30} | Text("456") | scale 3$

END$
