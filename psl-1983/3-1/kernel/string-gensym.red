%
% STRING-GENSYM.RED - Complement to GenSym, makes a string instead of ID
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        14 January 1982
% Copyright (c) 1982 University of Utah
%

% Edit by Cris Perdue,  9 Feb 1983 1620-PST
% Modified to avoid using the CHAR macro in a top level form

fluid '(StringGenSym!*);
StringGenSym!* := copystring("L0000");	% Copy to force into heap /csp

CompileTime flag('(StringGenSym1), 'InternalFunction);

lisp procedure StringGenSym();		%. Generate unique string
    StringGenSym1 4;

lisp procedure StringGenSym1 N;		%. Auxiliary function for StringGenSym
begin scalar Ch;
    return if N > 0 then
	if (Ch := Indx(StringGenSym!*, N)) < char !9 then
	<<  SetIndx(StringGenSym!*, N, Ch + 1);
	    TotalCopy StringGenSym!* >>
	else
	<<  SetIndx(StringGenSym!*, N, char !0);
	    StringGenSym1(N - 1) >>
    else				% Increment starting letter
    <<  SetIndx(StringGenSym!*, 0, Indx(StringGenSym!*, 0) + 1);
	StringGenSym() >>;
end;

END;
