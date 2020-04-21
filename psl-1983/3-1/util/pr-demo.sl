% PR-DEMO.SL: A small 3D Picture RLISP demo file, using LISP syntax
% Is equivalent to the PR-DEMO.RED form in RLISP syntax
% Use (LAPIN "PU:PR-DEMO.SL") for best effects

(LOAD PRLISP)

% First call the xxx!.INIT routine,

(HP!.INIT)  % For HP2648a

% Define a 20 x 20 square
(SETQ OUTLINE
      (POINTSET (ONEPOINT 10 10)
                (ONEPOINT -10 10)
                (ONEPOINT -10 -10)
                (ONEPOINT 10 -10)
                (ONEPOINT 10 10)))

% and an Arrow to place in square
(SETQ ARROW
      (GROUP (POINTSET (ONEPOINT 0 -1) (ONEPOINT 0 2))
             (POINTSET (ONEPOINT -1 1) (ONEPOINT 0 2) (ONEPOINT 1 1))))

% to produce the CubeFace. Will be shifted out by 10 units
(SETQ CUBEFACE (TRANSFORM (GROUP OUTLINE ARROW) (ZMOVE 10)))

% to produce a 20 x 20 x 20 Cube
(SETQ CUBE
      (GROUP CUBEFACE
             (TRANSFORM CUBEFACE (XROT 180))
             (TRANSFORM CUBEFACE (YROT 90))
             (TRANSFORM CUBEFACE (YROT -90))
             (TRANSFORM CUBEFACE (XROT 90))
             (TRANSFORM CUBEFACE (XROT -90))))

% This is a bigger cube to be seen more clearly
(SETQ BIGCUBE (TRANSFORM CUBE (SCALE 5)))

% as can be seen
(ESHOW BIGCUBE)

% Some more views of the CUBE
(ESHOW
 (TRANSFORM (TRANSFORM (TRANSFORM BIGCUBE (XROT 20)) (YROT 30)) (ZROT 10)))
(ESHOW
 (TRANSFORM (TRANSFORM (TRANSFORM CUBE (SCALE 2)) (XMOVE -240))
            (REPEATED 5 (XMOVE 80))))

% Draw a circle
(ESHOW (TRANSFORM (ONEPOINT 10 10) (CIRCLE 70)))
% and another
(SHOW (TRANSFORM (TRANSFORM (ONEPOINT 10 10) (CIRCLE 50))
	         (XMOVE 20)))

% Define Some control points for Bspline and Bezier
(SETQ CPTS
      (POINTSET (ONEPOINT 0 0)
                (ONEPOINT 70 -60)
                (ONEPOINT 189 -69)
                (ONEPOINT 206 33)
                (ONEPOINT 145 130)
                (ONEPOINT 48 130)
                (ONEPOINT 0 84)))

% And show the BSPLINE and BEZIER curves
(ESHOW (GROUP CPTS (TRANSFORM CPTS (BEZIER))))
(ESHOW (GROUP CPTS (TRANSFORM CPTS (BSPLINE))))

