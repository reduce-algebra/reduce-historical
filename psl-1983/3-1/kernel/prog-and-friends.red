%
% PROG-AND-FRIENDS.RED - PROG, GO, and RETURN
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        20 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL>PROG-AND-FRIENDS.RED.2, 11-Oct-82 17:55:57, Edit by BENSON
%  Changed CATCH/THROW to *CATCH/*THROW

% Error numbers:
% 3000 - Unknown label
% 3100 - outside the scope of a PROG
% +1 in GO
% +2 in RETURN

fluid '(ProgJumpTable!*			% A-List of labels and expressions
	ProgBody!*);			% Tail of the current PROG

fexpr procedure Prog ProgBody!*;	%. Program feature function
begin scalar ProgJumpTable!*, N, Result;
    if not PairP ProgBody!* then return NIL;
    N := 0;
    for each X in car ProgBody!* do
    <<  PBind1 X;
	N := N + 1 >>;
    ProgBody!* := cdr ProgBody!*;
    for each X on ProgBody!* do
	if IDP car X then
	    ProgJumpTable!* := X . ProgJumpTable!*;
    while << while PairP ProgBody!* and IDP car ProgBody!* do
		ProgBody!* := cdr ProgBody!*;	% skip over labels
	     PairP ProgBody!* >> do	% eval the expression
    <<  Result := !*Catch('!$Prog!$, Eval car ProgBody!*);
	if not ThrowSignal!* then
	<<  Result := NIL;
	    ProgBody!* := cdr ProgBody!* >> >>;
    UnBindN N;
    return Result;
end;

lisp fexpr procedure GO U;		%. Goto label within PROG
begin scalar NewProgBody;
    return if ProgBody!* then
    <<  NewProgBody := Atsoc(car U, ProgJumpTable!*);
	if null NewProgBody then
	    ContinuableError(3001,
			     BldMsg(
		"%r is not a label within the current scope", car U),
			     'GO . U)
	else
	<<  ProgBody!* := NewProgBody;
	    !*Throw('!$Prog!$, NIL) >> >>
    else ContinuableError(3101,
			  "GO attempted outside the scope of a PROG",
			  'GO . U);
end;

lisp procedure Return U;		%. Return value from PROG
    if ProgBody!* then
    <<  ProgBody!* := NIL;
	!*Throw('!$Prog!$, U) >>
    else ContError(3102, "RETURN attempted outside the scope of a PROG",
			Return U);

END;
