%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Set-Terminal-20.SL (Tops-20 Version)
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        1 November 1982
%
% This file contains functions that set NMODE's terminal.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))

% External variables used here:

(fluid '(nmode-terminal))

% Global variables defined here:

(fluid '(terminal-type))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Terminal Selection Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-default-terminal ()
  (nmode-set-terminal)
  )

(de nmode-set-terminal ()
  (setf terminal-type (jsys2 65 0 0 0 (const jsgttyp)))
  (selectq terminal-type
    (21 % HP2621
     (ensure-terminal-type 'hp2648a)
     )
    (6 % HP264X
     (ensure-terminal-type 'hp2648a)
     )
    (15 % VT52
     (ensure-terminal-type 'vt52x)
     )
    (t
     (or nmode-terminal (ensure-terminal-type 'hp2648a))
     )
    ))

(de ensure-terminal-type (type)
  (cond ((or (null nmode-terminal)
	     (not (eq type (object-type nmode-terminal))))
	 (setf nmode-terminal (make-instance type))
	 (nmode-new-terminal)
	 )))

% These functions defined for compatibility:

(de hp2648a () (ensure-terminal-type 'hp2648a))
(de vt52x () (ensure-terminal-type 'vt52x))
