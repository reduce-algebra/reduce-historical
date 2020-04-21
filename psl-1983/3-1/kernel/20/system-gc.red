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
%  21-May-1983 Mark R. Swanson
%   Unmap old heap space after copying GC has been called, so we don't
%   occupy as much swapping space.

on Syslisp;

CompileTime <<

external WVar
	      OldHeapLast, OldHeapLowerBound, OldHeapUpperBound;

syslsp smacro procedure BeforeGCSystemHook();
    NIL;

syslsp smacro procedure AfterGCSystemHook();
% Unmap all of old heap except first page, which is assumed to be the first
%  page in a section;  else after a savesystem, sections with no pages will
%  not exist (and we don't want to re-create them).

    unmap!-space( OldHeapLowerBound + 1,OldHeapLast+8#1777);

>>;

off Syslisp;

END;
