%
% SYMBOL-VALUES.RED - ValueCell, UnboundP, MakeUnbound and Set
% 
% Author:      Eric Benson
%              Computer Science Dept.
%              University of Utah
% Date:        20 August 1981
% Copyright (c) 1981 Eric Benson
%

on SysLisp;

syslsp procedure UnboundP U;	 %. Does U not have a value?
    if IDP U then
	if Tag SymVal IDInf U eq Unbound then T else NIL
    else
	NonIDError(U, 'UnboundP);

syslsp procedure MakeUnbound U;		%. Make U an unbound ID
    if IDP U then
	SymVal IDInf U := MkItem(Unbound, IDInf U)
    else
	NonIDError(U, 'MakeUnbound);

syslsp procedure ValueCell U;		%. Safe access to SymVal entry
begin scalar V;				% This guy is called from Eval
    return if IDP U then
    <<  V := SymVal IDInf U;
	if Tag V eq Unbound then
	    ContinuableError('99, BldMsg('"%r is an unbound ID", U), U)
	else V >>
    else
	NonIDError(U, 'ValueCell);
end;

% This version of SET differs from the Standard Lisp report in that Exp is
% not declared fluid, in order to maintain compatibility between compiled
% and interpreted code.

syslsp procedure Set(Exp, Val);		%. Assign Val to ID Exp
    if IDP Exp then
	if not (null Exp or Exp eq 'T) then
	<<  SymVal IDInf Exp := Val;
	    Val >>
	else StdError '"T and NIL cannot be SET"
    else NonIDError(Exp, 'Set);

off SysLisp;

END;
