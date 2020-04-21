% BIG-FASLEND.RED - Patch to FASLEND for huge files
% 
% Author:      Eric Benson
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        7 May 1982
% Copyright (c) 1982 University of Utah
%  <PSL.COMP>BIG-FASLEND.RED.4, 10-Jun-82 10:39:32, Edit by GRISS
%  Added InitCodeMax!* for testing
%

lisp procedure CompileUncompiledExpressions();
    <<ErrorPrintF("%n*** Init code length is %w%n",
			length car UncompiledExpressions!*);
      CompileInitCode('!*!*Fasl!*!*InitCode!*!*, 
         car UncompiledExpressions!*)>>;

FLUID '(InitCodeMax!*);

LoadTime <<InitCodeMax!*:=350>>;

lisp procedure CompileInitCode(Name, InitCodeList);
begin scalar X, Len, LastHalf;
    return if ILessP(Len := length InitCodeList, InitCodeMax!*) then
	DfPrintFasl list('de, Name, '(), 'progn . InitCodeList)
    else
    <<  ErrorPrintF(
"*** Initcode length %w too large, splitting into smaller pieces", Len);
	ErrorPrintF("*** Please use smaller files in FASL");
	X := PNTH(InitCodeList, IQuotient(Len, 2));
	LastHalf := cdr X;
	Rplacd(X, NIL);			% tricky, split the code in 2
	X := Intern Concat(ID2String Name, StringGensym());
	Flag1(X, 'InternalFunction);	% has to be internal to get called!
	CompileInitCode(X,
			InitCodeList);
	CompileInitCode(Name, list X . LastHalf) >>;	% call previous
end;
