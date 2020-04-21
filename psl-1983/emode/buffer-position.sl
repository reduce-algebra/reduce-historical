%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% BUFFER-POSITION.SL - EMODE Buffer Position Objects
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        6 July 1982
%
% This file implements objects that store buffer positions.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load common))

(fluid '(CurrentLineIndex point))

(de buffer-position-create (line-number column-number)
  (cons line-number column-number))

(de buffer-position-line (bp)
  (car bp))

(de buffer-position-column (bp)
  (cdr bp))

(de buffer-position-compare (bp1 bp2)
  (cond ((< (buffer-position-line bp1)   (buffer-position-line bp2))   -1)
	((> (buffer-position-line bp1)   (buffer-position-line bp2))    1)
	((< (buffer-position-column bp1) (buffer-position-column bp2)) -1)
	((> (buffer-position-column bp1) (buffer-position-column bp2))  1)
	(t 0)))

(de buffer-get-position ()
  (buffer-position-create CurrentLineIndex point))

(de buffer-set-position (bp)
  (if bp (progn
    (PutLine)
    (setf CurrentLineIndex (buffer-position-line bp))
    (setf point (buffer-position-column bp))
    (GetLine CurrentLineIndex)
    )))
