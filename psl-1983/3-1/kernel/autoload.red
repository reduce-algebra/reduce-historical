%
% AUTOLOAD.RED - Autoloading entry stubs
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        25 March 1982
% Copyright (c) 1982 University of Utah
%

%  07-Mar-83 Nancy Kendzierski
%   Changed PrettyPrint to use PP, not PrettyPrint.
%   Added PP as an autoloaded function.
%  <PSL.KERNEL>AUTOLOAD.RED.3, 17-Sep-82 16:35:02, Edit by BENSON
%  Changed PrettyPrint to use PrettyPrint, not Pretty

CompileTime <<

macro procedure DefAutoload U;
%
% (DefAutoload name), (DefAutoload name loadname),
% (DefAutoload name loadname fntype), or
% (DefAutoload name loadname fntype numargs)
%
% Default is 1 Arg EXPR in module of same name
%
begin scalar Name, NumArgs, LoadName, FnType;
    U := rest U;
    Name := first U;
    U := rest U;
    if not null U then
    <<  LoadName := first U;
	U :=rest U >>
    else LoadName := Name;
    if EqCar(Name, 'QUOTE) then Name := second Name;
    if EqCar(LoadName, 'QUOTE) then LoadName := second LoadName;
    if not null U then
    <<  FnType := first U;
	U := rest U >>
    else FnType := 'EXPR;
    if not null U then
	NumArgs := first U
    else NumArgs := 1;
    NumArgs := MakeArgList NumArgs;
    return list('PutD, MkQuote Name,
		       MkQuote FnType,
		       list('function, list('lambda, NumArgs,
					    list('load, LoadName),
					    list('Apply, MkQuote Name,
						     'list . NumArgs))));
end;

lisp procedure MakeArgList N;
    GetV('[() (X1) (X1 X2) (X1 X2 X3) (X1 X2 X3 X4) (X1 X2 X3 X4 X5)],
	 N);

>>;

DefAutoload(PrettyPrint, PP);
DefAutoload(PP, PP, FEXPR);

DefAutoload(DefStruct, DefStruct, FEXPR);

DefAutoload(Step);

DefAutoload Mini;

DefAutoload('Help, 'Help, FEXPR);

DefAutoload(Emode, Emode, EXPR, 0);

DefAutoload(Invoke, Mini);

PUT('CREF ,'SIMPFG ,'((T (CREFON)) (NIL (CREFOFF))));

DefAutoload(CrefOn, RCref, EXPR, 0);

put('Syslisp,
    'SimpFg,
    '((T (load Syslisp))));

DefAutoload(CompD, Compiler, EXPR, 3);

DefAutoload(FaslOUT, Compiler);

if_system(Tops20, <<

DefAutoload(Bug, Bug, EXPR, 0);

DefAutoload(MM, Exec, EXPR, 0);

DefAutoload(Exec, Exec, EXPR, 0);

>>);

END;
