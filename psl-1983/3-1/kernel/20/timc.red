%
% TIMC.RED - get run time in milliseconds
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        1 October 1981
% Copyright (c) 1981 University of Utah
%

lap '((!*entry TimC expr 0)
	(!*MOVE (WConst -5) (reg 1))
	(runtm)
	(!*EXIT 0)
);

end;
