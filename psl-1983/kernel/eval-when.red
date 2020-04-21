%
% EVAL-WHEN.RED - Funny business to make things happen at different times
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        30 August 1981
% Copyright (c) 1981 University of Utah
%

% Functions flagged IGNORE are evaluated immediately when invoked at the top
% level while compiling to a file.  Those flagged EVAL are evaled immediately
% and also passed to the file.  These functions are defined to make those
% actions more visible and mnemonic.

macro procedure CommentOutCode U;	%. Comment out a single expression
    NIL;

lisp procedure CompileTime U;		%. Evaluate at compile time only
    U;				% just return the already evaluated argument

flag('(CommentOutCode CompileTime), 'IGNORE);

% The functions above need only be present at compile time.  Those below must
% be present at both compile and load time to be effective.

lisp procedure BothTimes U;		%. Evaluate at compile and load time
    U;

flag('(BothTimes), 'EVAL);

lisp procedure LoadTime U;		%. Evaluate at load time only
    U;

PutD('StartupTime, 'EXPR, cdr GetD 'LoadTime);
					% StartupTime is kernel hack
RemFlag('(LoadTime), 'IGNORE);		% just to be sure it doesn't
RemFlag('(LoadTime), 'EVAL);		% happen until load time

END;
