%
% OTHERS-SL.RED - Random Standard Lisp functions
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        20 August 1981
% Copyright (c) 1981 University of Utah
%

% These are functions that didn't have a logical home
% Most could have been defined portably, but were not for efficiency reasons

on SysLisp;

off R2I;

syslsp procedure FixP U;		%. Is U an integer?
    FixP U;

on R2I;

syslsp procedure Digit U;	%. Is U an ID whose print name is a digit?
    IDP U and (U := IDInf U) >= char !0 and U <= char !9;

syslsp procedure Liter U;	%. Is U a single character alphabetic ID?
    IDP U and ((U := IDInf U) >= char A and U <= char Z
		or U >= char !a and U <= char !z);

off SysLisp;

CompileTime flag('(Length1), 'InternalFunction);

lisp procedure Length U;		%. Length of list U
    Length1(U, 0);

lisp procedure Length1(U, N);
    if PairP U then Length1(cdr U, IAdd1 N) else N;

END;
