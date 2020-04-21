%
% ERROR-HANDLERS.RED - Low level error handlers
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        18 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PERDUE.PSL>ERROR-HANDLERS.RED.2,  9-Dec-82 18:16:42, Edit by PERDUE
%  Changed continuable error message; also allows for no (NIL) retry form
%  <PSL.KERNEL>ERROR-HANDLERS.RED.2, 20-Sep-82 14:55:56, Edit by BENSON
%  Error number isn't printed
%  <PSL.INTERP>ERROR-HANDLERS.RED.11, 26-Feb-82 23:43:16, Edit by BENSON
%  Added BreakLevel!* check
%  <PSL.INTERP>ERROR-HANDLERS.RED.8, 28-Dec-81 17:02:43, Edit by BENSON
%  Compressed output in ContinuableError
%  MLG 7:18am  Tuesday, 24 November 1981 - To print ErrorForm!* on ErrorOut!*

fluid '(!*ContinuableError		% if true, inside continuable error
	ErrorForm!*
	BreakLevel!*			% nesting level of break loops
	MaxBreakLevel!*			% maximum permitted ...
	!*EMsgP);			% value of 2nd arg to previous errorset
global '(EMsg!*);			% gets message from most recent error

on SysLisp;

syslsp procedure FatalError S;
<<  ErrorPrintF("***** Fatal error: %s", S);
    while T do Quit; >>;

off SysLisp;

lisp procedure RangeError(Object, Index, Fn);
    StdError BldMsg("Index %r out of range for %p in %p", Index, Object, Fn);

lisp procedure StdError Message;	%. Error without number
    Error(99, Message);

SYMBOLIC PROCEDURE YESP U;
   BEGIN SCALAR BOOL,X,Y, OLDOUT, OLDIN, PROMPTSTRING!*;
	OLDIN := RDS NIL;
	OLDOUT := WRS ERROUT!*;
%	TERPRI();
%	PRIN2L U;
%	TERPRI();
%	TERPRI();
	if_system(Tops20,	% ? in col 1, so batch jobs get killed
	PROMPTSTRING!* := BldMsg("?%l (Y or N) ", U),
	PROMPTSTRING!* := BldMsg("%l (Y or N) ", U));
    A:	X := READ();
	IF (Y := (X MEMQ '(Y YES))) OR X MEMQ '(N NO) THEN GO TO B;
%	IF NULL BOOL THEN PRIN2T "TYPE Y OR N";
	if X = 'B then ErrorSet('(Break), NIL, NIL);
	if_system(Unix,		% If read EOF, croak so shell scripts terminate
	if X eq !$EOF!$ then return (lambda(!*Break);
		StdError "End-of-file read in YesP")(NIL));
	BOOL := T;
	GO TO A;
    B:	WRS OLDOUT;
	RDS OLDIN;
	CURSYM!* := '!*SEMICOL!*;
	RETURN Y
   END;

lisp procedure ContinuableError(ErrNum, Message, ErrorForm!*);	%. maybe fix
begin scalar !*ContinuableError;
    !*ContinuableError := T;
    EMsg!* := Message;
    return if !*Break and !*EMsgP and BreakLevel!* < MaxBreakLevel!* then
    <<  ErrorPrintF("***** %l", Message);	% Don't print number
	if null ErrorForm!* then
	    ErrorPrintF("***** Continuable error.")
	else
	if FlatSize ErrorForm!* < 40 then
	    ErrorPrintF("***** Continuable error: retry form is %r",
			ErrorForm!*)
	else
	<<  ErrorPrintF("***** Continuable error, retry form is:");
	    ErrorPrintF("%p", ErrorForm!*) >>;
	Break() >>
    else Error(ErrNum, Message);
end;

END;
