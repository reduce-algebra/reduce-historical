%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Set-Terminal-9836.SL (9836 Version)
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        27 December 1982
%
% This file contains functions that set NMODE's terminal.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))

% External variables used here:

(fluid '(nmode-terminal nmode-other-terminal))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Terminal Selection Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-default-terminal ()
  (nmode-set-terminal)
  )

(de nmode-set-terminal ()
  (or nmode-terminal (ensure-terminal-type '9836-alpha))
  (or nmode-other-terminal (ensure-other-terminal-type '9836-color))
  )

(de ensure-terminal-type (type)
  (cond ((or (null nmode-terminal)
	     (not (eq type (object-type nmode-terminal))))
	 (setf nmode-terminal (make-instance type))
	 (nmode-new-terminal)
	 )))

(de ensure-other-terminal-type (type)
  (cond ((or (null nmode-other-terminal)
	     (not (eq type (object-type nmode-other-terminal))))
	 (setf nmode-other-terminal (make-instance type))
	 (nmode-new-terminal)
	 )))

