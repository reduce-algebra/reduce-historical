%
% SYSTEM-GC.RED - System dependent before and after GC hooks
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        5 March 1982
% Copyright (c) 1982 University of Utah
%

% Do nothing on the Dec-20

on Syslisp;

CompileTime <<

syslsp smacro procedure BeforeGCSystemHook();
    NIL;

syslsp smacro procedure AfterGCSystemHook();
    NIL;

>>;

off Syslisp;

END;
