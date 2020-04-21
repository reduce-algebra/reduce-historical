%
% CARCDR.RED - Composites of CAR and CDR, up to 4 levels
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        17 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.INTERP>CARCDR.RED.3,  4-Jul-82 13:29:21, Edit by BENSON
%  CAR and CDR of NIL are legal == NIL

CompileTime for each X in '(		% remove all compiler optimizations
CAAAAR     CAAAR     CAAR		% for CAR and CDR composites
CAAADR     CAADR     CADR	
CAADAR     CADAR     CDAR
CAADDR     CADDR     CDDR
CADAAR     CDAAR
CADADR     CDADR
CADDAR     CDDAR
CADDDR     CDDDR
CDAAAR
CDAADR
CDADAR
CDADDR
CDDAAR
CDDADR
CDDDAR
CDDDDR
) do Put(X, 'SaveCMACRO, RemProp(X, 'CMACRO));

lisp procedure CAAAAR U;		%.
    if null U then NIL
    else if PairP U then CAAAR CAR U else NonPairError(U, 'CAAAAR);

lisp procedure CAAADR U;		%.
    if null U then NIL
    else if PairP U then CAAAR CDR U else NonPairError(U, 'CAAADR);

lisp procedure CAADAR U;		%.
    if null U then NIL
    else if PairP U then CAADR CAR U else NonPairError(U, 'CAADAR);

lisp procedure CAADDR U;		%.
    if null U then NIL
    else if PairP U then CAADR CDR U else NonPairError(U, 'CAADDR);

lisp procedure CADAAR U;		%.
    if null U then NIL
    else if PairP U then CADAR CAR U else NonPairError(U, 'CADAAR);

lisp procedure CADADR U;		%.
    if null U then NIL
    else if PairP U then CADAR CDR U else NonPairError(U, 'CADADR);

lisp procedure CADDAR U;		%.
    if null U then NIL
    else if PairP U then CADDR CAR U else NonPairError(U, 'CADDAR);

lisp procedure CADDDR U;		%.
    if null U then NIL
    else if PairP U then CADDR CDR U else NonPairError(U, 'CADDDR);

lisp procedure CDAAAR U;		%.
    if null U then NIL
    else if PairP U then CDAAR CAR U else NonPairError(U, 'CDAAAR);

lisp procedure CDAADR U;		%.
    if null U then NIL
    else if PairP U then CDAAR CDR U else NonPairError(U, 'CDAADR);

lisp procedure CDADAR U;		%.
    if null U then NIL
    else if PairP U then CDADR CAR U else NonPairError(U, 'CDADAR);

lisp procedure CDADDR U;		%.
    if null U then NIL
    else if PairP U then CDADR CDR U else NonPairError(U, 'CDADDR);

lisp procedure CDDAAR U;		%.
    if null U then NIL
    else if PairP U then CDDAR CAR U else NonPairError(U, 'CDDAAR);

lisp procedure CDDADR U;		%.
    if null U then NIL
    else if PairP U then CDDAR CDR U else NonPairError(U, 'CDDADR);

lisp procedure CDDDAR U;		%.
    if null U then NIL
    else if PairP U then CDDDR CAR U else NonPairError(U, 'CDDDAR);

lisp procedure CDDDDR U;		%.
    if null U then NIL
    else if PairP U then CDDDR CDR U else NonPairError(U, 'CDDDDR);


lisp procedure CAAAR U;			%.
    if null U then NIL
    else if PairP U then CAAR CAR U else NonPairError(U, 'CAAAR);

lisp procedure CAADR U;			%.
    if null U then NIL
    else if PairP U then CAAR CDR U else NonPairError(U, 'CAADR);

lisp procedure CADAR U;			%.
    if null U then NIL
    else if PairP U then CADR CAR U else NonPairError(U, 'CADAR);

lisp procedure CADDR U;			%.
    if null U then NIL
    else if PairP U then CADR CDR U else NonPairError(U, 'CADDR);

lisp procedure CDAAR U;			%.
    if null U then NIL
    else if PairP U then CDAR CAR U else NonPairError(U, 'CDAAR);

lisp procedure CDADR U;			%.
    if null U then NIL
    else if PairP U then CDAR CDR U else NonPairError(U, 'CDADR);

lisp procedure CDDAR U;			%.
    if null U then NIL
    else if PairP U then CDDR CAR U else NonPairError(U, 'CDDAR);

lisp procedure CDDDR U;			%.
    if null U then NIL
    else if PairP U then CDDR CDR U else NonPairError(U, 'CDDDR);


lisp procedure SafeCAR U;
    if null U then NIL
    else if PairP U then CAR U else NonPairError(U, 'CAR);

lisp procedure SafeCDR U;
    if null U then NIL
    else if PairP U then CDR U else NonPairError(U, 'CDR);


lisp procedure CAAR U;			%.
    if null U then NIL
    else if PairP U then SafeCAR CAR U else NonPairError(U, 'CAAR);

lisp procedure CADR U;			%.
    if null U then NIL
    else if PairP U then SafeCAR CDR U else NonPairError(U, 'CADR);

lisp procedure CDAR U;			%.
    if null U then NIL
    else if PairP U then SafeCDR CAR U else NonPairError(U, 'CDAR);

lisp procedure CDDR U;			%.
    if null U then NIL
    else if PairP U then SafeCDR CDR U else NonPairError(U, 'CDDR);

CompileTime for each X in '(		% restore compiler optimizations
CAAAAR     CAAAR     CAAR		% for CAR and CDR composites
CAAADR     CAADR     CADR	
CAADAR     CADAR     CDAR
CAADDR     CADDR     CDDR
CADAAR     CDAAR
CADADR     CDADR
CADDAR     CDDAR
CADDDR     CDDDR
CDAAAR
CDAADR
CDADAR
CDADDR
CDDAAR
CDDADR
CDDDAR
CDDDDR
) do Put(X, 'CMACRO, RemProp(X, 'SaveCMACRO));

END;
