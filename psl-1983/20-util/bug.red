% BUG.RED - Send bug reports
% 
% Author:      Martin Griss and Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        11 December 1981
% Copyright (c) 1981 University of Utah
%

IMPORTS '(EXEC);

lisp procedure Bug();
<<  PrintF "*** PSL Bug reporter, ^N to abort%n";
    PutRescan BldMsg "MAIL *PSL:USER-BUG-REPORTS.TXT,BENSON,GRISS%n";
    MM();
    TerPri() >>;

END;
