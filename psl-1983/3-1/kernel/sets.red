%
% SETS.RED - Functions acting on lists as sets
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        12 December 1981
% Copyright (c) 1981 University of Utah
%

lisp procedure List2Set L;		%. Remove redundant elements from L
    if not PairP L then NIL
    else if car L member cdr L then List2Set cdr L
    else car L . List2Set cdr L;

lisp procedure List2SetQ L;		%. EQ version of List2Set
    if not PairP L then NIL		% Don't confuse it with SetQ!
    else if car L memq cdr L then List2Set cdr L
    else car L . List2Set cdr L;

lisp procedure Adjoin(Element, ASet);	%. Add Element to Set
    if Element member ASet then ASet else Element . ASet;

lisp procedure AdjoinQ(Element, ASet);	%. EQ version of Adjoin
    if Element memq ASet then ASet else Element . ASet;

lisp procedure Union(X, Y);		%. Set union
    if not PairP X then Y
    else Union(cdr X, if car X Member Y then Y else car X . Y);

lisp procedure UnionQ(X, Y);		%. EQ version of UNION
    if not PairP X then Y
    else UnionQ(cdr X, if car X memq Y then Y else car X . Y);

lisp procedure XN(U, V);		%. Set intersection
    if not PairP U then NIL
    else if car U Member V then car U . XN(cdr U, Delete(car U, V))
    else XN(cdr U, V);

lisp procedure XNQ(U, V);		%. EQ version of XN
    if null PairP U then NIL
    else if car U memq V then car U . XN(cdr U, DelQ(car U, V))
    else XN(cdr U, V);

LoadTime
<<  PutD('Intersection, 'EXPR, cdr GetD 'XN);	% for those who like to type
    PutD('IntersectionQ, 'EXPR, cdr GetD 'XNQ) >>;

END;
