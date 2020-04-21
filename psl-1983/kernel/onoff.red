%
% ONOFF.RED - Macros for setting/resetting flags, with SIMPFG hook
% 
% Author:      Martin Griss
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        21 July 1982
% Copyright (c) 1982 University of Utah
%

% ONOFF.RED - ON and OFF for Bare PSL
% MLG, from PU:RLISP-PARSER.RED

lisp procedure OnOff!*(IdList, U);
%
% IdList is list of variables without !* prefix, U is T or NIL
%
begin scalar Y;
    for each X in IdList do
	if not IDP X then NonIDError(X, if null U then 'OFF else 'ON)
	else
	<<  Set(MkFlagVar X, U);
	    if (Y := Atsoc(U, get(X, 'SIMPFG))) then Eval second Y >>;
end;

lisp procedure MkFlagVar U;		% Should be redefined in PACKAGE.RED
  Intern Concat("*", ID2String U);	% to lambda-bind current pkg to GLOBAL

macro procedure ON U;
    list('OnOff!*, MkQuote cdr U, T);

macro procedure OFF U;
    list('OnOff!*, MkQuote cdr U, NIL);

flag('(ON OFF), 'IGNORE);

END;
