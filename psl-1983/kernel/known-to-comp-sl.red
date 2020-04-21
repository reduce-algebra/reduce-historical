%
% KNOWN-TO-COMPILER.RED - Standard Lisp functions which are handled entirely
%				by the compiler
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        17 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.INTERP>KNOWN-TO-COMP-SL.RED.4,  4-Jul-82 13:30:59, Edit by BENSON
%  CAR and CDR of NIL are legal == NIL

off R2I;	% can't do recursion removal, will get infinte recursion

% Section 3.1 -- Elementary predicates

lisp procedure CodeP U;			%. Is U a code pointer?
    CodeP U;

lisp procedure Eq(U, V);		%. Are U and V identical?
    U eq V;

lisp procedure FloatP U;		%. Is U a floating point number?
    FloatP U;

lisp procedure BigP U;			%. Is U a bignum?
    BigP U;

lisp procedure IDP U;			%. Is U an ID?
    IDP U;

lisp procedure PairP U;			%. Is U a pair?
    PairP U;

lisp procedure StringP U;		%. Is U a string?
    StringP U;

lisp procedure VectorP U;		%. Is U a vector?
    VectorP U;


% Section 3.2 -- Functions on Dotted-Pairs

% NonPairError found in TYPE-ERRORS.RED

lisp procedure Car U;			%. left subtree of pair
    if null U then NIL
    else if PairP U then car U else NonPairError(U, 'CAR);

lisp procedure Cdr U;			%. right subtree of pair
    if null U then NIL
    else if PairP U then cdr U else NonPairError(U, 'CDR);

lisp procedure RplacA(U, V);		%. RePLAce CAr of pair
    if PairP U then RplacA(U, V) else NonPairError(U, 'RPLACA);

lisp procedure RplacD(U, V);		%. RePLACe CDr of pair
    if PairP U then RplacD(U, V) else NonPairError(U, 'RPLACD);

on R2I;					% Turn recursion removal back on

END;
