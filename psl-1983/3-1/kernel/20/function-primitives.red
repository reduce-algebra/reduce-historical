%
% FUNCTION-PRIMITIVES.RED - primitives used by PUTD/GETD and EVAL/APPLY
%              P20: version
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        23 August 1981
% Copyright (c) 1981 University of Utah
%

% 22-May-83 Mark R. Swanson
%  Changes to support extended addressing on -20: essentially making
%  references to SYMFNC explicit array refences.

% Every ID has a "function cell".  It does not necessarily contain a legal
% Lisp item, and therefore should not be accessed directly by Lisp functions.
% In this implementation the function cell contains an instruction to be
% executed.  There are 3 possibilites for this instruction, for which the
% following predicates and updating functions exist:
%
%	FUnBoundP(ID) -- the function is not defined
%	FLambdaLinkP(ID) -- the function is interpreted
%	FCodeP(ID) -- the function is compiled
%
%	MakeFUnBound(ID) -- undefine the function
%	MakeFLambdaLink(ID) -- specify that the function is interpreted
%	MakeFCode(ID, CodePtr) -- specify that the function is compiled,
%				   and that the code resides at the address
%				   associated with CodePtr
%
%	GetFCodePointer(ID) -- returns the contents of the function cell as a
%				code pointer

% These functions currently check that they have proper arguments, but this may
% change since they are only used by functions that have checked them already.

% Note that MakeFCode is necessarily machine-dependent -- this file currently
% contains the PDP-10 version. This function should be moved to a file of
% system-dependent routines.  Of course, other things in this file will
% probably have to change for a different machine as well.

on SysLisp;
internal WConst SymfncJsp = 8#265500000000 + &SymFnc[0];
internal WVar UnDefn = SymFncJsp + IDLoc UndefinedFunction;
internal WVar LamLnk = SymFncJsp + IDLoc CompiledCallingInterpreted;

% currently the WVars UnDefn and LamLnk contain the instructions which will
% be found in the function cells of undefined and interpreted functions.

syslsp procedure FUnBoundP U;		%. does U not have a function defn?
    if IDP U then SymFnc[Inf U] eq  Undefn
    else NonIDError(U, 'FUnBoundP);

syslsp procedure FLambdaLinkP U;	%. is U an interpreted function?
    if IDP U then SymFnc [Inf U] eq LamLnk
    else NonIDError(U, 'FLambdaLinkP);

syslsp procedure FCodeP U;		%. is U a compiled function?
    if IDP U then SymFnc[Inf U] neq UnDefn
              and SymFnc[Inf U] neq LamLnk
    else NonIDError(U, 'FCodeP);

syslsp procedure MakeFUnBound U;	%. Make U an undefined function
    if IDP U then
    <<  SymFnc[Inf U] := UnDefn;
	NIL >>
    else NonIDError(U, 'MakeFUnBound);

syslsp procedure MakeFLambdaLink U;	%. Make U an interpreted function
    if IDP U then
    <<  SymFnc[Inf U] := LamLnk;
	NIL >>
    else NonIDError(U, 'MakeFLambdaLink);


syslsp procedure MakeFCode(U, CodePtr);	%. Make U a compiled function
    if IDP U then
	if CodeP CodePtr then
	<<  SymFnc[Inf U] := Field(CodePtr, 18, 18) + 8#254000000000;
%	    PutField(SymFnc U, 0, 9, 8#254);	% JRST
	    NIL >>
    else NonIDError(U, 'MakeFCode);

syslsp procedure GetFCodePointer U;	%. Get code pointer for U
    if IDP U then MkCODE Field(SymFnc[Inf U], 12, 24)
    else NonIDError(U, 'GetFCodePointer);

off SysLisp;

END;

