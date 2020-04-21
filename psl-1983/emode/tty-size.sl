%  JSYS call to get dimensions of "TTY" screen.
% Written by M. L. Griss.  Modifications by William Galway.

% **** SubField should be included as part of the JSYS system? ****
% Return a subfield from a "word".  (Bit 0 is leftmost on DEC-20.)
% (FieldSize might be better?)

(DM SubField (args)
  `(Land ,(indx args 3)
      (LSH ,(indx args 1)
        (difference ,(indx args 2)
          35))))

% Return JFN mode word for terminal.
(DE TTyWord ()
  (JSYS2 8#101 0 0 0 8#107))                            % jsRFMOD

% Return system's idea of the terminal's "page length".
(DE PageLength ()
  (SubField (TTyWord) 10 8#177))

(DE PageWidth ()
  (SubField (TTyWord) 17 8#177))
