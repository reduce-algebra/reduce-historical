%
% FLUID-GLOBAL.RED - Fluid and Global declarations
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        17 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.INTERP>FLUID-GLOBAL.RED.3, 10-Sep-82 09:18:04, Edit by BENSON
%  Uses indicator VARTYPE instead of TYPE

%  <PSL.INTERP>FLUID-GLOBAL.RED.3, 22-Jan-82 12:35:25, Edit by BENSON
%  GlobalP now only checks for variables, not functions

% The functions dealing with FLUID and GLOBAL declarations use the property
% list indicator TYPE, which is also used by PUTD and GETD.
% Not true anymore!

% Non-Standard Lisp functions used:
% ErrorPrintF -- in IO.RED

CompileTime flag('(DeclareFluidOrGlobal DeclareFluidOrGlobal1),
		 'InternalFunction);

lisp procedure DeclareFluidOrGlobal(IDList, FG);
    for each U in IDList do DeclareFluidOrGlobal1(U, FG);

lisp procedure DeclareFluidOrGlobal1(U, FG);
    if not IDP U then NIL else
    begin scalar X;
	X := get(U, 'VARTYPE);
	return if null X then
	<<  put(U, 'VARTYPE, FG);
	    if UnBoundP U then Set(U, NIL) >>
	else if X eq FG then NIL
	else ErrorPrintF("*** %p %r cannot become %p",
			       X, U,		  FG);
    end;

lisp procedure Fluid IDList;		%. Declare all in IDList as fluid vars
    DeclareFluidOrGlobal(IDList, 'FLUID);

lisp procedure Fluid1 U;		%. Declare U fluid
    DeclareFluidOrGlobal1(U, 'FLUID);

lisp procedure FluidP U;		%. Is U a fluid variable?
    get(U, 'VARTYPE) = 'FLUID;

lisp procedure Global IDList;		%. Declare all in IDList as global vars
    DeclareFluidOrGlobal(IDList, 'GLOBAL);

lisp procedure Global1 U;		%. Declare U global
    DeclareFluidOrGlobal1(U, 'GLOBAL);

lisp procedure GlobalP U;		%. Is U a global variable
    get(U, 'VARTYPE) = 'GLOBAL;

lisp procedure UnFluid IDList;		%. Undeclare all in IDList as fluid
    for each U in IDList do UnFluid1 U;

lisp procedure UnFluid1 U;
    if FluidP U then RemProp(U, 'VARTYPE);

END;
