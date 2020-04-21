%
% COMP-SUPPORT.RED - Run-time support for optimized Cons and List compilation
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        20 September 1981
% Copyright (c) 1981 University of Utah
%

CommentOutCode <<			% defined in CONS-MKVECT.RED
CompileTime(SavedCompFn := RemProp('Cons, 'CompFn));	% else can't compile

lisp procedure NCons U;			%. U . NIL, or 1-argument EXPR for LIST
    U . NIL;

lisp procedure XCons(U, V);		%. V . U
    V . U;

CompileTime put('Cons, 'CompFn, SavedCompFn);
>>;

lisp procedure List5(U, V, W, X, Y);	%. 5-argument EXPR for LIST
    U . List4(V, W, X, Y);

lisp procedure List4(U, V, W, X);	%. 4-argument EXPR for LIST
    U . List3(V, W, X);

lisp procedure List3(U, V, W);		%. 3-argument EXPR for LIST
    U . List2(V, W);

lisp procedure List2(U, V);		%. 2-argument EXPR for LIST
    U . NCons V;

END;
