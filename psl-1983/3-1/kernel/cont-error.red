%
% CONT-ERROR.RED - Nice macro to set up arguments for ContinuableError
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        23 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.INTERP>CONT-ERROR.RED.3,  2-Sep-82 09:10:04, Edit by BENSON
%  Made handling of ReEvalForm more robust

% format is:
% ContError(ErrorNumber, FormatString, {arguments to PrintF}, ReEvalForm)

% ReEvalForm is something like
% Foo(X, Y)
% which becomes
% list('Foo, MkQuote X, MkQuote Y)

macro procedure ContError U;		%. Set up for ContinuableError
begin scalar ErrorNumber, Message, ReEvalForm;
    U := cdr U;
    ErrorNumber := car U;
    U := cdr U;
    if null cddr U then			% if it's just a string, don't
    <<  Message := car U;		% generate call to BldMsg
	U := cdr U >>
    else
    <<  while cdr U do
	<<  Message := AConc(Message, car U);
	    U := cdr U >>;
	Message := 'BldMsg . Message >>;
    ReEvalForm := car U;
    ReEvalForm := if not PairP ReEvalForm then list('MkQuote, ReEvalForm)
		  else 'list
		  . MkQuote car ReEvalForm
		  . for each X in cdr ReEvalForm collect list('MkQuote, X);
    return list('ContinuableError,
		ErrorNumber,
		Message,
		ReEvalForm);
end;

END;
