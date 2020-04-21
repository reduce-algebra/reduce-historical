% BUG.SL - Send bug reports
% 
% Author:      Martin Griss and Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        11 December 1981
% Copyright (c) 1981 University of Utah
%

%  <PERDUE.PSL>BUG.SL.2,  7-Jan-83 16:52:07, Edit by PERDUE
%  Changed to LISP syntax, added bug-mail-to variable.
%  Each site may set bug-mail-to as desired.

(imports '(exec))

(fluid '(bug-mail-to))

(cond ((null bug-mail-to) (setq bug-mail-to "")))

(defun bug ()
  (printf "*** PSL Bug reporter, ^N to abort%n")
  (putrescan (bldmsg "mail %w%n" bug-mail-to))
  (mm)
  (terpri)
  t)
