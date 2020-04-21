%
% DSKIN.RED - Read/Eval/Print from files
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        24 September 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL>DSKIN.RED.2,  5-Oct-82 11:32:28, Edit by BENSON
%  Changed DSKIN from FEXPR to 1 argument EXPR
%  <PSL.INTERP>DSKIN.RED.11,  7-May-82 06:14:27, Edit by GRISS
%  Added XPRINT in loop to handle levels of output
%  <PSL.INTERP>DSKIN.RED.6, 30-Apr-82 12:49:59, Edit by BENSON
%  Made !*DEFN call DfPrint instead of own processing
%  <PSL.INTERP>DSKIN.RED.3, 29-Apr-82 04:23:49, Edit by GRISS
%  Added !*DEFN flag, cf TOPLOOP

CompileTime <<

flag('(DskInDefnPrint), 'InternalFunction);

>>;

expr procedure DskIN F;		%. Read a file (dskin "file")
%
% This is reasonably standard Standard Lisp, except for file name format
% knowledge.
%
begin scalar OldIN, NewIN, TestOpen, Exp;
    TestOpen := ErrorSet(list('OPEN, F, '(QUOTE INPUT)), NIL, NIL);
    if not PairP TestOpen then return
	ContError(99, "Couldn't open file `%w'", F, DskIN F);
    NewIN := car TestOpen;
    OldIN := RDS NewIN;
    while PairP(Exp := ErrorSet(quote Read(), T, !*Backtrace))
		and not (car Exp eq !$EOF!$)
		and PairP(Exp := ErrorSet(list('DskInEval, MkQuote car Exp),
					  T,
					  !*Backtrace)) do
	if not !*Defn then PrintF("%f%p%n", car Exp);
		%/ no error protection for printing, maybe should be
    RDS OldIN;
    Close NewIN;
end;

lisp procedure DskInEval U;
    if not !*DEFN then Eval U else DskInDefnPrint U;

lisp procedure DskInDefnPrint U; % handle case of !*Defn:=T
%
% Looks for special action on a form, otherwise prettyprints it;
% Adapted from DFPRINT
%
    if PairP U and FlagP(car U,'Ignore) then Eval U
    else				% So 'IGNORE is EVALED, not output
    <<  if DfPrint!* then Apply(DfPrint!*, list U)
	else PrettyPrint U;		% So 'EVAL gets EVALED and Output
	if PairP U and FlagP(Car U,'EVAL) then Eval U >>;

flag('(DskIn), 'IGNORE);

fluid '(!*RedefMSG !*Echo);

SYMBOLIC PROCEDURE LAPIN FIL;
BEGIN SCALAR OLDIN, EXP, !*REDEFMSG, !*ECHO;
    OLDIN := RDS OPEN(FIL,'INPUT);
    WHILE (EXP := READ()) NEQ !$EOF!$ 
     DO EVAL EXP;
    CLOSE RDS OLDIN;
END;

END;
