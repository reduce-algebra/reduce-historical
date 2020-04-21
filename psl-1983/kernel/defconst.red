%
% DEFCONST.RED - Definition and use of symbolic constants
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        8 January 1982
% Copyright (c) 1982 University of Utah
%

% DefConst is used to define a value for a name, to be used in const(Name)

macro procedure DefConst Form;		%. DefConst(Name, Value, ...);
begin scalar ResultForm;
    ResultForm := list 'ProgN;
    Form := cdr Form;
    while not null Form do
    <<  ResultForm := list('EvDefConst, MkQuote car Form, MkQuote cadr Form)
			. ResultForm;
	Form := cddr Form >>;
    return ReversIP ResultForm;
end;

flag('(DefConst), 'Eval);

lisp procedure EvDefConst(ConstName, ConstValue);
    put(ConstName, 'Const, ConstValue);

macro procedure Const Form;
    get(cadr Form, 'Const) or StdError BldMsg("Unknown const form %r", Form);

END;
