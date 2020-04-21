%
% SCAN-TABLE.RED - Lisp character table for DEC-20
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        27 November 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL-20>SCAN-TABLE.RED.6, 10-Feb-83 16:12:38, Edit by PERDUE
%  Changed the "put EOF" to be a STARTUPTIME form
% Edit by Cris Perdue, 28 Jan 1983 2039-PST
% LispDipthong -> LispDiphthong

fluid '(LispScanTable!* CurrentScanTable!*);

LispScanTable!* := '
[17 10 10 10 10 10 10 10 10 17 17 10 17 17 10 10 10 10 10 10 10 10 10 10 
10 10 11 10 10 10 10 10 17 14 15 10 10 12 10 11 11 11 10 19 10 18 20 10 
0 1 2 3 4 5 6 7 8 9 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 
10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 11 16 11 10 10 10 10 10 
10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 
10 10 10 10 10 LispDiphthong];

CurrentScanTable!* := LispScanTable!*;

% Done as "startuptime" because "char" is available at compile
% time but not necessarily init time /csp
startuptime
    put('EOF, 'CharConst, char cntrl Z);

END;
