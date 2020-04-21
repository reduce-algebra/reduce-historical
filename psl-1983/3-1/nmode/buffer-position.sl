%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% BUFFER-POSITION.SL - Buffer Position Objects
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        6 July 1982
%
% This file implements objects that store buffer positions.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load fast-int))

(de buffer-position-create (line-number column-number)
  (cons line-number column-number))

(de buffer-position-line (bp)
  (car bp))

(de buffer-position-column (bp)
  (cdr bp))

(de buffer-position-equal (bp1 bp2)
  (and (= (car bp1) (car bp2)) (= (cdr bp1) (cdr bp2))))

(de buffer-position-compare (bp1 bp2)
  (cond ((< (buffer-position-line bp1)   (buffer-position-line bp2))   -1)
	((> (buffer-position-line bp1)   (buffer-position-line bp2))    1)
	((< (buffer-position-column bp1) (buffer-position-column bp2)) -1)
	((> (buffer-position-column bp1) (buffer-position-column bp2))  1)
	(t 0)))

(de buffer-position-lessp (bp1 bp2)
  (<= (buffer-position-compare bp1 bp2) 0))
