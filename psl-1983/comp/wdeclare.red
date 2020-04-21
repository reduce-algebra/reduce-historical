%
% WDECLARE.RED - Skeleton WDeclare for WConsts
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        9 March 1982
% Copyright (c) 1982 University of Utah
%

% <PSL.COMP>WDECLARE.RED.2, 17-Nov-82 17:09:39, Edit by PERDUE
% Flagged WDeclare IGNORE rather than EVAL, so it takes effect
%  at compile time rather than load time!

fexpr procedure WDeclare U;
    for each X in cddr U do WDeclare1(car X, car U, cadr U, cadr X, caddr X);

flag('(WDeclare), 'IGNORE);

lisp procedure WDeclare1(Name, Scope, Typ, UpperBound, Initializer);
    if Typ = 'WCONST then
	if Scope = 'EXTERNAL and not get(Name, 'WCONST) then
	    ErrorPrintF("*** A value has not been defined for WConst %r",
								Name)
	else% EvDefConst(Name, Initializer)
		put(Name, 'WConst, Initializer)
    else StdError BldMsg("%r is not currently supported", Typ);
