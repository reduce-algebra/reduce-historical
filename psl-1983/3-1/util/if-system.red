%
% IF-SYSTEM.RED - Conditional compilation for system-dependent code
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        10 March 1982
% Copyright (c) 1982 University of Utah
%

fluid '(system_list!*);

macro procedure if_system U;
    do_if_system(cadr U, caddr U, if cdddr U then cadddr U else NIL);

expr procedure do_if_system(system_name, true_case, false_case);
    if system_name memq system_list!* then true_case else false_case;

END;
