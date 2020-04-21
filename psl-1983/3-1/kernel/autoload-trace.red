%
% AUTOLOAD-TRACE.RED - Autoloading stubs for DEBUG
% 
% Author:      Eric Benson
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        24 September 1982
% Copyright (c) 1982 University of Utah
%

% This file is used instead of MINI-TRACE.RED for those systems which
%  can load files

lisp macro procedure TR U;
<<  load Debug;
    Apply('TR, list U) >>;

lisp macro procedure TRST U;
<<  load Debug;
    Apply('TRST, list U) >>;

END;
