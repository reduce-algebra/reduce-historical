%
% ERROR-ERRORSET.RED - The most basic ERROR and ERRORSET
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        20 August 1981
% Copyright (c) 1981 University of Utah
%

% Edit by Cris Perdue,  4 Feb 1983 1208-PST
% Moved ERRSET here from CATCH-THROW.RED.
% Edit by Cris Perdue,  3 Feb 1983 1526-PST
% Tidied up definition of ERRORSET.
%  <PSL.KERNEL>ERROR-ERRORSET.RED.3, 11-Oct-82 17:57:30, Edit by BENSON
%  Changed CATCH/THROW to new definition
%  <PSL.KERNEL>ERROR-ERRORSET.RED.2, 20-Sep-82 11:31:23, Edit by BENSON
%  Removed printing of error number in ERROR
%  <PSL.INTERP>ERROR-ERRORSET.RED.7, 26-Feb-82 23:44:01, Edit by BENSON
%  Added BreakLevel!* check
%  <PSL.INTERP>ERROR-ERRORSET.RED.5, 28-Dec-81 17:07:18, Edit by BENSON
%  Changed 3rd formal in ErrorSet to !*Inner!*Backtrace

global '(EMsg!*);			% gets current error message
fluid '(!*BackTrace			% controls backtrace printing (actual)
	!*Inner!*Backtrace		% controls backtrace printing (formal)
	!*EMsgP				% controls message printing
	!*Break				% controls breaking
	BreakLevel!*			% nesting level of breaks
	MaxBreakLevel!*			% maximum permitted ...
	!*ContinuableError);		% if T, inside a continuable error

LoadTime
<<  !*EmsgP := T;
    !*BackTrace := NIL;
    !*Break := T >>;

lisp procedure Error(Number, Message);	%. Throw to ErrorSet
begin scalar !*ContinuableError;
    EMsg!* := Message;
    if !*EMsgP then
    <<  ErrorPrintF("***** %l", Message);	% Error number is not printed
	if !*Break and BreakLevel!* < MaxBreakLevel!* then
	    return Break() >>;
    return
    <<  if !*Inner!*BackTrace then BackTrace();
	Throw('!$Error!$, Number) >>;
end;

% More useful version of ERRORSET
macro procedure errset u;
(lambda(form, flag);
    list(list('lambda, '(!*Emsgp),
		  list('catch, ''!$error!$, list('ncons, form))),
         flag))(cadr u, if null cddr u then t else caddr u);

lisp procedure ErrorSet(Form, !*EMsgP, !*Inner!*BackTrace); %. Protected Eval
    Catch('!$Error!$, list(Eval Form));	% eval form

END;
