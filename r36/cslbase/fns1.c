/*  fns1.c                           Copyright (C) 1989-96 Codemist Ltd */

/*
 * Basic functions part 1.
 */

/* Signature: 4e5258ab 07-Mar-2000 */

#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#include "machine.h"
#include "tags.h"
#include "cslerror.h"
#include "externs.h"
#include "entries.h"
#include "arith.h"
#ifdef TIMEOUT
#include "timeout.h"
#endif


/*****************************************************************************/
/*      Some basic functions                                                 */
/*****************************************************************************/

Lisp_Object integerp(Lisp_Object p)
{
    Lisp_Object nil = C_nil;
    int tag = ((int)p) & TAG_BITS;
    if (tag == TAG_FIXNUM) return lisp_true;
    if (tag == TAG_NUMBERS)
    {   Header h = *(Header *)((char *)p - TAG_NUMBERS);
        if (type_of_header(h) == TYPE_BIGNUM) return lisp_true;
    }
    return nil;
}

/*****************************************************************************/
/*      Storage allocation.                                                  */
/*****************************************************************************/


Lisp_Object cons(Lisp_Object a, Lisp_Object b)
{
    nil_as_base
    Lisp_Object r = (Lisp_Object)((char *)fringe - sizeof(Cons_Cell));
    qcar(r) = a;
    qcdr(r) = b;
    fringe = r;
    if ((char *)r <= (char *)heaplimit)
        return reclaim((Lisp_Object)((char *)r + TAG_CONS),
                       "internal cons", GC_CONS, 0);
    else return (Lisp_Object)((char *)r + TAG_CONS);
}

Lisp_Object cons_no_gc(Lisp_Object a, Lisp_Object b)
{
    nil_as_base
    Lisp_Object r = (Lisp_Object)((char *)fringe - sizeof(Cons_Cell));
    qcar(r) = a;
    qcdr(r) = b;
    fringe = r;
    return (Lisp_Object)((char *)r + TAG_CONS);
}

/*
 * cons_gc_test() MUST be called after any sequence of cons_no_gc() calls.
 */

Lisp_Object cons_gc_test(Lisp_Object p)
{
    nil_as_base
    if ((char *)fringe <= (char *)heaplimit)
        return reclaim(p, "cons gc test", GC_CONS, 0);
    else return p;
}

Lisp_Object ncons(Lisp_Object a)
{
    Lisp_Object nil = C_nil;
    Lisp_Object r = (Lisp_Object)((char *)fringe - sizeof(Cons_Cell));
    qcar(r) = a;
    qcdr(r) = nil;
    fringe = r;
    if ((char *)r <= (char *)heaplimit)
        return reclaim((Lisp_Object)((char *)r + TAG_CONS),
                       "internal ncons", GC_CONS, 0);
    else return (Lisp_Object)((char *)r + TAG_CONS);
}

Lisp_Object list2(Lisp_Object a, Lisp_Object b)
{
/* Note that building two cons cells at once saves some overhead here */
    Lisp_Object nil = C_nil;
    Lisp_Object r = (Lisp_Object)((char *)fringe - 2*sizeof(Cons_Cell));
    qcar(r) = a;
    qcdr(r) = (Lisp_Object)((char *)r + sizeof(Cons_Cell) + TAG_CONS);
    qcar((char *)r+sizeof(Cons_Cell)) = b;
    qcdr((char *)r+sizeof(Cons_Cell)) = nil;
    fringe = r;
    if ((char *)r <= (char *)heaplimit)
        return reclaim((Lisp_Object)((char *)r + TAG_CONS),
                       "internal list2", GC_CONS, 0);
    else return (Lisp_Object)((char *)r + TAG_CONS);
}

Lisp_Object list2star(Lisp_Object a, Lisp_Object b, Lisp_Object c)
{
    nil_as_base
    Lisp_Object r = (Lisp_Object)((char *)fringe - 2*sizeof(Cons_Cell));
    qcar(r) = a;
    qcdr(r) = (Lisp_Object)((char *)r + sizeof(Cons_Cell) + TAG_CONS);
    qcar((char *)r+sizeof(Cons_Cell)) = b;
    qcdr((char *)r+sizeof(Cons_Cell)) = c;
    fringe = r;
    if ((char *)r <= (char *)heaplimit)
        return reclaim((Lisp_Object)((char *)r + TAG_CONS),
                       "internal list2*", GC_CONS, 0);
    else return (Lisp_Object)((char *)r + TAG_CONS);
}

Lisp_Object acons(Lisp_Object a, Lisp_Object b, Lisp_Object c)
{
    nil_as_base
    Lisp_Object r = (Lisp_Object)((char *)fringe - 2*sizeof(Cons_Cell));
    qcar(r) = (Lisp_Object)((char *)r + sizeof(Cons_Cell) + TAG_CONS);
    qcdr(r) = c;
    qcar((char *)r+sizeof(Cons_Cell)) = a;
    qcdr((char *)r+sizeof(Cons_Cell)) = b;
    fringe = r;
    if ((char *)r <= (char *)heaplimit)
        return reclaim((Lisp_Object)((char *)r + TAG_CONS),
                       "internal acons", GC_CONS, 0);
    else return (Lisp_Object)((char *)r + TAG_CONS);
}

Lisp_Object list3(Lisp_Object a, Lisp_Object b, Lisp_Object c)
{
    Lisp_Object nil = C_nil;
    Lisp_Object r = (Lisp_Object)((char *)fringe - 3*sizeof(Cons_Cell));
    qcar(r) = a;
    qcdr(r) = (Lisp_Object)((char *)r + sizeof(Cons_Cell) + TAG_CONS);
    qcar((char *)r+sizeof(Cons_Cell)) = b;
    qcdr((char *)r+sizeof(Cons_Cell)) =
        (Lisp_Object)((char *)r + 2*sizeof(Cons_Cell) + TAG_CONS);
    qcar((char *)r+2*sizeof(Cons_Cell)) = c;
    qcdr((char *)r+2*sizeof(Cons_Cell)) = nil;
    fringe = r;
    if ((char *)r <= (char *)heaplimit)
        return reclaim((Lisp_Object)((char *)r + TAG_CONS),
                       "internal list3", GC_CONS, 0);
    else return (Lisp_Object)((char *)r + TAG_CONS);
}

/*****************************************************************************/
/*****************************************************************************/
/***              Lisp-callable versions of all the above                  ***/
/*****************************************************************************/
/*****************************************************************************/

/*
 * The set of car/cdr combinations here seem pretty dull, but they
 * are fairly important for performance...
 */

Lisp_Object Lcar(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else return onevalue(qcar(a));
}

/*
 * (car* a) = (car a) if a is non-atomic, but just a otherwise.
 */

Lisp_Object Lcar_star(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return onevalue(a);
    else return onevalue(qcar(a));
}

Lisp_Object Lcdr(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else return onevalue(qcdr(a));
}

Lisp_Object Lcaar(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else return onevalue(qcar(a));
}

Lisp_Object Lcadr(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else return onevalue(qcar(a));
}

Lisp_Object Lcdar(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else return onevalue(qcdr(a));
}

Lisp_Object Lcddr(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else return onevalue(qcdr(a));
}

Lisp_Object Lcaaar(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else return onevalue(qcar(a));
}

Lisp_Object Lcaadr(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else return onevalue(qcar(a));
}

Lisp_Object Lcadar(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else return onevalue(qcar(a));
}

Lisp_Object Lcaddr(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else return onevalue(qcar(a));
}

Lisp_Object Lcdaar(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else return onevalue(qcdr(a));
}

Lisp_Object Lcdadr(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else return onevalue(qcdr(a));
}

Lisp_Object Lcddar(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else return onevalue(qcdr(a));
}

Lisp_Object Lcdddr(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else return onevalue(qcdr(a));
}

Lisp_Object Lcaaaar(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else return onevalue(qcar(a));
}

Lisp_Object Lcaaadr(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else return onevalue(qcar(a));
}

Lisp_Object Lcaadar(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else return onevalue(qcar(a));
}

Lisp_Object Lcaaddr(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else return onevalue(qcar(a));
}

Lisp_Object Lcadaar(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else return onevalue(qcar(a));
}

Lisp_Object Lcadadr(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else return onevalue(qcar(a));
}

Lisp_Object Lcaddar(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else return onevalue(qcar(a));
}

Lisp_Object Lcadddr(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else return onevalue(qcar(a));
}

Lisp_Object Lcdaaar(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else return onevalue(qcdr(a));
}

Lisp_Object Lcdaadr(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else return onevalue(qcdr(a));
}

Lisp_Object Lcdadar(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else return onevalue(qcdr(a));
}

Lisp_Object Lcdaddr(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else return onevalue(qcdr(a));
}

Lisp_Object Lcddaar(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else return onevalue(qcdr(a));
}

Lisp_Object Lcddadr(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else return onevalue(qcdr(a));
}

Lisp_Object Lcdddar(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_car, a);
    else a = qcar(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else return onevalue(qcdr(a));
}

Lisp_Object Lcddddr(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else a = qcdr(a);
    if (!car_legal(a)) return error(1, err_bad_cdr, a);
    else return onevalue(qcdr(a));
}

Lisp_Object Lrplaca(Lisp_Object nil,
                           Lisp_Object a, Lisp_Object b)
{
    CSL_IGNORE(nil);
    if (!consp(a)) return error(1, err_bad_rplac, a);
    qcar(a) = b;
    return onevalue(a);
}

Lisp_Object Lrplacd(Lisp_Object nil,
                           Lisp_Object a, Lisp_Object b)
{
    CSL_IGNORE(nil);
    if (!consp(a)) return error(1, err_bad_rplac, a);
    qcdr(a) = b;
    return onevalue(a);
}

Lisp_Object Lsymbolp(Lisp_Object nil, Lisp_Object a)
{
    return onevalue(Lispify_predicate(symbolp(a)));
}

Lisp_Object Latom(Lisp_Object nil, Lisp_Object a)
{
    return onevalue(Lispify_predicate(!consp(a)));
}

Lisp_Object Lconsp(Lisp_Object nil, Lisp_Object a)
{
    return onevalue(Lispify_predicate(consp(a)));
}

Lisp_Object Lconstantp(Lisp_Object nil, Lisp_Object a)
/*
 * This version is as required for Standard Lisp - it is inadequate
 * for Common Lisp.
 */
{
/*
 * Standard Lisp requires that I report that "Function Pointers" are
 * "constant" here. It is not at all clear that I have a way of
 * doing that. I will go some way my ensuring that code-vectors are.
 */
#ifdef COMMON
    return onevalue(Lispify_predicate(
            a == nil || a == lisp_true ||
            is_char(a) ||
            is_number(a) ||
            is_vector(a) ||
            is_bps(a)));
#else
    return onevalue(Lispify_predicate(
            is_number(a) ||
            is_vector(a) ||  /* Vectors include strings here */
            is_bps(a)));
#endif
}

Lisp_Object Lidentity(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    return onevalue(a);
}

#ifdef COMMON

Lisp_Object Llistp(Lisp_Object nil, Lisp_Object a)
{
    return onevalue(Lispify_predicate(is_cons(a)));
}

#endif

Lisp_Object Lnumberp(Lisp_Object nil, Lisp_Object a)
{
    return onevalue(Lispify_predicate(is_number(a)));
}

Lisp_Object Lintegerp(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    return onevalue(integerp(a));
}

Lisp_Object Leq_safe(Lisp_Object nil, Lisp_Object a)
{
/*
 * True if you can safely use EQ tests to check equality. Thus true for
 * things that are represented in "immediate" form...
 */
#ifdef COMMON
    return onevalue(is_fixnum(a) ||
                    is_sfloat(a) ||
                    is_odds(a) ? lisp_true : nil);
#else
    return onevalue(is_fixnum(a) ||
                    is_odds(a) ? lisp_true : nil);
#endif
}

Lisp_Object Lfixp(Lisp_Object nil, Lisp_Object a)
{
#ifdef COMMON
    return onevalue(is_fixnum(a) ? lisp_true : nil);
#else
/*
 * Standard Lisp defines fixp to say yes to bignums as well as
 * fixnums.
 */
    CSL_IGNORE(nil);
    return onevalue(integerp(a));
#endif
}

Lisp_Object Lfloatp(Lisp_Object nil, Lisp_Object p)
{
    int tag = TAG_BITS & (int)p;
#ifdef COMMON
    if (tag == TAG_SFLOAT) return onevalue(lisp_true);
#endif
    if (tag == TAG_BOXFLOAT) return onevalue(lisp_true);
    else return onevalue(nil);
}

#ifdef COMMON

static Lisp_Object Lshort_floatp(Lisp_Object nil, Lisp_Object p)
{
    int tag = TAG_BITS & (int)p;
    if (tag == TAG_SFLOAT) return onevalue(lisp_true);
    else return onevalue(nil);
}

static Lisp_Object Lsingle_floatp(Lisp_Object nil, Lisp_Object p)
{
    int tag = TAG_BITS & (int)p;
    if (tag == TAG_BOXFLOAT &&
        type_of_header(flthdr(p)) == TYPE_SINGLE_FLOAT)
        return onevalue(lisp_true);
    else return onevalue(nil);
}

static Lisp_Object Ldouble_floatp(Lisp_Object nil, Lisp_Object p)
{
    int tag = TAG_BITS & (int)p;
    if (tag == TAG_BOXFLOAT &&
        type_of_header(flthdr(p)) == TYPE_DOUBLE_FLOAT)
        return onevalue(lisp_true);
    else return onevalue(nil);
}

static Lisp_Object Llong_floatp(Lisp_Object nil, Lisp_Object p)
{
    int tag = TAG_BITS & (int)p;
    if (tag == TAG_BOXFLOAT &&
        type_of_header(flthdr(p)) == TYPE_LONG_FLOAT)
        return onevalue(lisp_true);
    else return onevalue(nil);
}

Lisp_Object Lrationalp(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    return onevalue(
      Lispify_predicate(
        is_fixnum(a) ||
        (is_numbers(a) && 
           (is_bignum(a) || is_ratio(a)))));
}

Lisp_Object Lcomplexp(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    return onevalue(Lispify_predicate(is_numbers(a) && is_complex(a)));
}

CSLbool complex_stringp(Lisp_Object a)
/*
 * true if the arg is a string, but NOT a simple string.  In general
 * when this is true simplify_string() will then be called to do
 * an adjustment.
 */
{
    Header h;
    Lisp_Object w, nil = C_nil;
    if (!is_vector(a)) return NO;
    h = vechdr(a);
    if (type_of_header(h) != TYPE_ARRAY) return NO;
/*
 * Note that the cheery Common Lisp Committee decided the abolish the
 * separate type 'string-char, so the test here is maybe dubious...
 */
    else if (elt(a, 0) != string_char_sym) return NO;
    w = elt(a, 1);
    if (!consp(w) || consp(qcdr(w))) return NO;
    else return YES;
}

#endif

CSLbool stringp(Lisp_Object a)
/*
 * True if arg is a simple OR a general string
 */
{
    Header h;
#ifdef COMMON
    Lisp_Object w, nil = C_nil;
#endif
    if (!is_vector(a)) return NO;
    h = vechdr(a);
    if (type_of_header(h) == TYPE_STRING) return YES;
#ifdef COMMON
    else if (type_of_header(h) != TYPE_ARRAY) return NO;
/*
 * Beware abolition of 'string-char
 */
    else if (elt(a, 0) != string_char_sym) return NO;
    w = elt(a, 1);
    if (!consp(w) || consp(qcdr(w))) return NO;
    else return YES;
#else
    else return NO;
#endif
}

Lisp_Object Lstringp(Lisp_Object nil, Lisp_Object a)
/*
 * simple-string-p
 */
{
    if (!(is_vector(a)) || type_of_header(vechdr(a)) != TYPE_STRING)
        return onevalue(nil);
    else return onevalue(lisp_true);
}

#ifdef COMMON

static Lisp_Object Lc_stringp(Lisp_Object nil, Lisp_Object a)
{
    return onevalue(Lispify_predicate(stringp(a)));
}

#endif

Lisp_Object Lhash_table_p(Lisp_Object nil, Lisp_Object a)
/*
 * hash-table-p
 */
{
    if (!(is_vector(a)) || type_of_header(vechdr(a)) != TYPE_HASH)
        return onevalue(nil);
    else return onevalue(lisp_true);
}

#ifdef COMMON

static Lisp_Object Lsimple_bit_vector_p(Lisp_Object nil,
                                        Lisp_Object a)
/*
 * simple-bit-vector-p
 */
{
    if (!(is_vector(a))) return onevalue(nil);
    else return onevalue(Lispify_predicate(header_of_bitvector(vechdr(a))));
}

#endif

Lisp_Object Lsimple_vectorp(Lisp_Object nil, Lisp_Object a)
/*
 * simple-vector-p
 */
{
    if (!(is_vector(a))) return onevalue(nil);
    else return onevalue(Lispify_predicate(
                             type_of_header(vechdr(a))==TYPE_SIMPLE_VEC));
}

Lisp_Object Lbpsp(Lisp_Object nil, Lisp_Object a)
{
    if (!(is_bps(a))) return onevalue(nil);
    else return onevalue(lisp_true);
}

Lisp_Object Lthreevectorp(Lisp_Object nil, Lisp_Object a)
/*
 * This is useful for REDUCE - it checks if something is a vector
 * of size 3!
 */
{
    if (!(is_vector(a))) return onevalue(nil);
/*
 * With 3 elements a vector has 12 bytes of data plus 4 of header - hence
 * the number 16 used here.
 */
    return onevalue(Lispify_predicate(
        vechdr(a) == (TAG_ODDS + TYPE_SIMPLE_VEC + (16<<10))));
}

#ifdef COMMON

static Lisp_Object Larrayp(Lisp_Object nil, Lisp_Object a)
{
    Header h;
    if (!(is_vector(a))) return onevalue(nil);
    h = vechdr(a);
/*
 * I could consider accepting TYPE_VEC16 and TYPE_VEC32 etc here...
 */
    if (type_of_header(h)==TYPE_ARRAY ||
        type_of_header(h)==TYPE_STRING ||
        type_of_header(h)==TYPE_SIMPLE_VEC ||
        header_of_bitvector(h)) return onevalue(lisp_true);
    else return onevalue(nil);
}

static Lisp_Object Lcomplex_arrayp(Lisp_Object nil, Lisp_Object a)
{
    if (!(is_vector(a))) return onevalue(nil);
    else return onevalue(Lispify_predicate(
                             type_of_header(vechdr(a))==TYPE_ARRAY));
}

static Lisp_Object Lconvert_to_array(Lisp_Object nil, Lisp_Object a)
{
    if (!(is_vector(a))) return onevalue(nil);
    vechdr(a) = TYPE_ARRAY + (vechdr(a) & ~header_mask);
    return onevalue(a);
}

#endif

static Lisp_Object Lstructp(Lisp_Object nil, Lisp_Object a)
/*
 * structp
 */
{
    if (!(is_vector(a))) return onevalue(nil);
    else return onevalue(Lispify_predicate(
                             type_of_header(vechdr(a))==TYPE_STRUCTURE));
}

static Lisp_Object Lconvert_to_struct(Lisp_Object nil, Lisp_Object a)
{
    if (!(is_vector(a))) return onevalue(nil);
    vechdr(a) = TYPE_STRUCTURE + (vechdr(a) & ~header_mask);
    return onevalue(a);
}

Lisp_Object Lcons(Lisp_Object nil, Lisp_Object a, Lisp_Object b)
{
    Lisp_Object r;
    CSL_IGNORE(nil);
    r = (Lisp_Object)((char *)fringe - sizeof(Cons_Cell));
    qcar(r) = a;
    qcdr(r) = b;
    fringe = r;
    if ((char *)r <= (char *)heaplimit)
        return onevalue(reclaim((Lisp_Object)((char *)r + TAG_CONS),
                                                "cons", GC_CONS, 0));
    else return onevalue((Lisp_Object)((char *)r + TAG_CONS));
}

Lisp_Object Lxcons(Lisp_Object nil, Lisp_Object a, Lisp_Object b)
{
    Lisp_Object r;
    CSL_IGNORE(nil);
    r = (Lisp_Object)((char *)fringe - sizeof(Cons_Cell));
    qcar(r) = b;
    qcdr(r) = a;
    fringe = r;
    if ((char *)r <= (char *)heaplimit)
        return onevalue(reclaim((Lisp_Object)((char *)r + TAG_CONS),
                                                "xcons", GC_CONS, 0));
    else return onevalue((Lisp_Object)((char *)r + TAG_CONS));
}

Lisp_Object Lncons(Lisp_Object nil, Lisp_Object a)
{
    Lisp_Object r;
    r = (Lisp_Object)((char *)fringe - sizeof(Cons_Cell));
    qcar(r) = a;
    qcdr(r) = nil;
    fringe = r;
    if ((char *)r <= (char *)heaplimit)
        return onevalue(reclaim((Lisp_Object)((char *)r + TAG_CONS),
                                                "ncons", GC_CONS, 0));
    else return onevalue((Lisp_Object)((char *)r + TAG_CONS));
}

Lisp_Object Llist2(Lisp_Object nil, Lisp_Object a, Lisp_Object b)
{
    a = list2(a, b);
    errexit();
    return onevalue(a);
}

Lisp_Object Lmkquote(Lisp_Object nil, Lisp_Object a)
{
    a = list2(quote_symbol, a);
    errexit();
    return onevalue(a);
}

Lisp_Object MS_CDECL Llist2star(Lisp_Object nil, int nargs, ...)
{
    va_list aa;
    Lisp_Object a, b, c;
    argcheck(nargs, 3, "list2*");
    va_start(aa, nargs);
    a = va_arg(aa, Lisp_Object);
    b = va_arg(aa, Lisp_Object);
    c = va_arg(aa, Lisp_Object);
    va_end(aa);
    a = list2star(a,b,c);
    errexit();
    return onevalue(a);
}

Lisp_Object MS_CDECL Lacons(Lisp_Object nil, int nargs, ...)
{
    va_list aa;
    Lisp_Object a, b, c;
    argcheck(nargs, 3, "acons");
    va_start(aa, nargs);
    a = va_arg(aa, Lisp_Object);
    b = va_arg(aa, Lisp_Object);
    c = va_arg(aa, Lisp_Object);
    va_end(aa);
    a = acons(a,b,c);
    errexit();
    return onevalue(a);
}

Lisp_Object MS_CDECL Llist3(Lisp_Object nil, int nargs, ...)
{
    va_list aa;
    Lisp_Object a, b, c;
    argcheck(nargs, 3, "list3");
    va_start(aa, nargs);
    a = va_arg(aa, Lisp_Object);
    b = va_arg(aa, Lisp_Object);
    c = va_arg(aa, Lisp_Object);
    va_end(aa);
    a = list3(a,b,c);
    errexit();
    return onevalue(a);
}

#ifdef COMMON
/*
 * In non-COMMON mode I implement list and list* as special forms
 * rather than as functions, guessing that that will be more efficient.
 */

Lisp_Object MS_CDECL Llist(Lisp_Object nil, int nargs, ...)
{
    Lisp_Object r = nil, w, w1;
    va_list a;
    va_start(a, nargs);
    push_args(a, nargs);
    while (nargs > 1)
    {   pop2(w, w1);
        nargs-=2;
        r = list2star(w1, w, r);
        errexitn(nargs);
    }
    while (nargs > 0)
    {   pop(w);
        nargs--;
        r = cons(w, r);
        errexitn(nargs);
    }
    return onevalue(r);
}

static Lisp_Object MS_CDECL Lliststar(Lisp_Object nil, int nargs, ...)
{
    Lisp_Object r, w, w1;
    va_list a;
    if (nargs == 0) return onevalue(nil);
    va_start(a, nargs);
    push_args(a, nargs);
    pop(r);
    nargs--;
    while (nargs > 1)
    {   pop2(w, w1);
        nargs-=2;
        r = list2star(w1, w, r);
        errexitn(nargs);
    }
    while (nargs > 0)
    {   pop(w);
        nargs--;
        r = cons(w, r);
        errexitn(nargs);
    }
    return onevalue(r);
}

/*
 * fill-vector is used for open-compilation of (vector ...) to avoid
 * passing grossly unreasonable numbers of arguments. The expansion of
 * (vector e1 ... en) should be
 *    (let ((v (mkvect <n-1>)) (i 0))
 *       (setq i (fill-vector v i e1 e2 ... e10))
 *       (setq i (fill-vector v i e11 e12 ... ))
 *       ...
 *       v)
 */
static Lisp_Object MS_CDECL Lfill_vector(Lisp_Object nil, int nargs, ...)
{
    va_list a;
    Lisp_Object v, il;
    int32 i;
    CSL_IGNORE(nil);
    if (nargs < 3) return aerror("fill-vector");
    va_start(a, nargs);
    v = va_arg(a, Lisp_Object);
    il = va_arg(a, Lisp_Object);
    if (!is_vector(v) || !is_fixnum(il)) return aerror("fill-vector");
    i = int_of_fixnum(il);
    nargs -= 2;
    while (nargs != 0)
    {   elt(v, i++) = va_arg(a, Lisp_Object);
        nargs--;
    }
    return onevalue(fixnum_of_int(i));
}

#endif

Lisp_Object Lpair(Lisp_Object nil, Lisp_Object a, Lisp_Object b)
{
    Lisp_Object r = nil;
    while (consp(a) && consp(b))
    {   push2(a, b);
        r = acons(qcar(a), qcar(b), r);
        pop2(b, a);
        errexit();
        a = qcdr(a);
        b = qcdr(b);
    }
    a = nil;
    while (r != nil)
    {   b = qcdr(r);
        qcdr(r) = a;
        a = r;
        r = b;
    }
    return onevalue(a);
}


static int32 membercount(Lisp_Object a, Lisp_Object b)
/*
 * Counts how many times a is a member of the list b
 */
{
    int32 r = 0;
#ifdef COMMON
    Lisp_Object nil = C_nil;
#endif
    if (is_symbol(a) || is_fixnum(a))
    {   while (consp(b))
        {   if (a == qcar(b)) r++;
            b = qcdr(b);
        }
        return r;
    }
    while (consp(b))
    {   Lisp_Object cb = qcar(b);
        if (equal(a, cb)) r++;
        b = qcdr(b);
    }
    return r;
}

/*
 * INTERSECTION(A,B)
 * The result will have its items in the order that they occur in A.
 * If lists A and B contain duplicate items these will appear in the
 * output if and only if the items involved are duplicated in both
 * input lists.
 */
Lisp_Object Lintersect(Lisp_Object nil, Lisp_Object a, Lisp_Object b)
{
    Lisp_Object r = nil, w;
    push(b);
    while (consp(a))
    {   push2(a, r);
        w = Lmember(nil, qcar(a), stack[-2]);
        errexitn(3);
/* Here I ignore any item in a that is not also in b */
        if (w != nil)
        {   int32 n1 = membercount(qcar(stack[-1]), stack[0]);
            errexitn(3);
/*
 * Here I want to arrange that items only appear in the result list multiple
 * times if they occur multipl times in BOTH the input lists.
 */
            if (n1 != 0)
            {   int32 n2 = membercount(qcar(stack[-1]), stack[-2]);
                errexitn(3);
                if (n2 > n1) n1 = 0;
            }
            if (n1 == 0)
            {   pop(r);
                a = stack[0];
                r = cons(qcar(a), r);
                errexitn(2);
                pop(a);
            }
            else pop2(r, a);
        }
        else pop2(r, a);
        a = qcdr(a);
    }
    popv(1);
    a = nil;
    while (consp(r))
    {   b = r;
        r = qcdr(r);
        qcdr(b) = a;
        a = b;
    }
    return onevalue(a);
}

/*
 * UNION(A, B)
 * This works by consing onto the front of B each element of A that
 * is not already in B.  Thus items in A (but not already in B) get
 * added in reversed order.  Duplicates in B remain there, and but
 * duplicates in A are dropped.
 */
Lisp_Object Lunion(Lisp_Object nil, Lisp_Object a, Lisp_Object b)
{
    while (consp(a))
    {   Lisp_Object c;
        push2(a, b);
        c = Lmember(nil, qcar(a), b);
        errexitn(2);
        pop(b);
        if (c == nil)
        {   b = cons(qcar(stack[0]), b);
            errexitn(1);
        }
        pop(a);
        a = qcdr(a);
    }
    return onevalue(b);
}

Lisp_Object Lenable_backtrace(Lisp_Object nil, Lisp_Object a)
{
    int32 n = miscflags;
    if (a == nil) miscflags &= ~MESSAGES_FLAG;
    else miscflags |= MESSAGES_FLAG;
    return onevalue(Lispify_predicate((n & MESSAGES_FLAG) != 0));
}

#ifdef NAG

Lisp_Object MS_CDECL Lunwind(Lisp_Object nil, int nargs, ...)
{
    argcheck(nargs, 0, "unwind");
    exit_reason = (miscflags & MESSAGES_FLAG) ? UNWIND_ERROR : UNWIND_UNWIND;
    exit_count = 0;
    exit_tag = nil;
    flip_exception();
    return nil;
]

#endif

/*
 * If the variable *break-function* has as its value a symbol, and that
 * symbol names a function, then the function concerned will be called
 * with one argument after the headline for the diagnostic. When it returns
 * the system will unwind in the usual manner.
 */

Lisp_Object MS_CDECL Lerror(Lisp_Object nil, int nargs, ...)
{
    va_list a;
    Lisp_Object w;
#ifdef COMMON
    Lisp_Object r = nil, w1;
#else
    int i;
#endif
    if (nargs == 0) return aerror("error");
    va_start(a, nargs);
    push_args(a, nargs);
#ifdef COMMON
    while (nargs > 1)
    {   pop2(w, w1);
        nargs -= 2;
        w = list2star(w1, w, r);
        nil = C_nil;
        if (exception_pending()) flip_exception();
        else r = w;
    }
    while (nargs > 0)
    {   pop(w);
        nargs--;
        w = cons(w, r);
        nil = C_nil;
        if (exception_pending()) flip_exception();
        else r = w;
    }
    if (miscflags & HEADLINE_FLAG)
    {   push(r);
        err_printf("\n+++ error: ");
/*
 * I will use FORMAT to handle error messages provided the first arg
 * to error had been a string and also provided (for bootstrapping) that
 * the function FORMAT seems to be defined.
 */
        if (qfn1(format_symbol) == undefined1 ||
            !consp(r) ||
            !stringp(qcar(r))) loop_print_error(r);
        else Lapply_n(nil, 3, format_symbol, qvalue(error_output), r);
        ignore_exception();
        err_printf("\n");
        pop(r);
        ignore_exception();
    }
    qvalue(emsg_star) = r;               /* "Error message" in CL world */
    exit_value = fixnum_of_int(0);       /* "Error number"  in CL world */
#else
    if (miscflags & HEADLINE_FLAG)
    {   err_printf("\n+++ error: ");
        loop_print_error(stack[1-nargs]);
        for (i=1; i<nargs; i++)
        {   err_printf(" ");
            loop_print_error(stack[1+i-nargs]);
        }
        err_printf("\n");
    }
    if (nargs == 1)
    {   push(nil);
        nargs++;
    }
    qvalue(emsg_star) = stack[2-nargs];  /* "Error message" in SL world */
    exit_value = stack[1-nargs];         /* "Error number"  in SL world */
    popv(nargs);
#endif
    if ((w = qvalue(break_function)) != nil &&
        symbolp(w) &&
        qfn1(w) != undefined1)
    {   (*qfn1(w))(qenv(w), qvalue(emsg_star));
        ignore_exception();
    }
    exit_reason = (miscflags & MESSAGES_FLAG) ? UNWIND_ERROR : UNWIND_UNWIND;
    exit_count = 0;
    exit_tag = nil;
    flip_exception();
    return nil;
}

Lisp_Object Lerror1(Lisp_Object nil, Lisp_Object a1)
{
    return Lerror(nil, 1, a1);
}

Lisp_Object Lerror2(Lisp_Object nil, Lisp_Object a1, Lisp_Object a2)
{
    return Lerror(nil, 2, a1, a2);
}

Lisp_Object MS_CDECL Lerror0(Lisp_Object nil, int nargs, ...)
{
/*
 * Silently provoked error - unwind to surrounding errorset level. Note that
 * this will NEVER enter a user-provided break loop...
 */
    argcheck(nargs, 0, "error0");
    miscflags &= ~(MESSAGES_FLAG | HEADLINE_FLAG);
    exit_reason = UNWIND_UNWIND;
    exit_value = exit_tag = nil;
    exit_count = 0;
    flip_exception();
    return nil;
}

Lisp_Object Lstop(Lisp_Object env, Lisp_Object code)
{
/*
 * I ignore "env" and set up nil for myself here to make it easier to call
 * this function from random places in my interface code...
 */
    Lisp_Object nil = C_nil;
    CSL_IGNORE(env);
    if (!is_fixnum(code)) return aerror("stop");
    exit_value = code;
    exit_tag = fixnum_of_int(0);    /* Flag to say "stop" */
    exit_reason = UNWIND_RESTART;
    exit_count = 1;
    flip_exception();
    return nil;
}

Lisp_Object Lmake_special(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!symbolp(a)) return aerror1("make-special", a);
    qheader(a) |= SYM_SPECIAL_VAR;
    return onevalue(a);
}

Lisp_Object Lmake_global(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!symbolp(a)) return aerror("make-global");
    qheader(a) |= (SYM_SPECIAL_VAR | SYM_GLOBAL_VAR);
    return onevalue(a);
}

Lisp_Object Lunmake_special(Lisp_Object nil, Lisp_Object a)
{
    if (!symbolp(a)) return onevalue(nil);
    qheader(a) &= ~(SYM_SPECIAL_VAR | SYM_GLOBAL_VAR);
    return onevalue(a);
}

Lisp_Object Lunmake_global(Lisp_Object nil, Lisp_Object a)
{
    if (!symbolp(a)) return onevalue(nil);
    qheader(a) &= ~(SYM_SPECIAL_VAR | SYM_GLOBAL_VAR);
    return onevalue(a);
}

Lisp_Object Lsymbol_specialp(Lisp_Object nil, Lisp_Object a)
{
    if (!symbolp(a)) return onevalue(nil);
    else if ((qheader(a) & (SYM_SPECIAL_VAR | SYM_GLOBAL_VAR)) ==
                            SYM_SPECIAL_VAR) return onevalue(lisp_true);
    else return onevalue(nil);
}

Lisp_Object Lsymbol_globalp(Lisp_Object nil, Lisp_Object a)
{
    if (!symbolp(a)) return onevalue(nil);
    else if ((qheader(a) & SYM_GLOBAL_VAR) != 0) return onevalue(lisp_true);
    else return onevalue(nil);
}

Lisp_Object Lboundp(Lisp_Object nil, Lisp_Object a)
{
    if (!symbolp(a)) return onevalue(nil);
#ifndef COMMON
/*
 * In COMMON Lisp it seems that this is intended to just check if the
 * value cell in a shallow-bound implementation contains some marker value
 * that stands for "junk".  In Standard Lisp mode I deem that variables
 * that have not been declared fluid are unbound.  Seems to me like a
 * classical mix-up between the concept of binding and of having some
 * particular value...  Oh well.
 */
    else if ((qheader(a) & SYM_SPECIAL_VAR) == 0) return onevalue(nil);
#endif
    else if (qvalue(a) == unset_var) return onevalue(nil); /* no value yet */
    else return onevalue(lisp_true);
}

Lisp_Object Lsymbol_value(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    if (!symbolp(a)) return onevalue(a);
    else return onevalue(qvalue(a));
}

Lisp_Object Lset(Lisp_Object nil, Lisp_Object a, Lisp_Object b)
{
    if (!symbolp(a) || a == nil || a == lisp_true) return aerror("set");
    qvalue(a) = b;
    return onevalue(b);
}

Lisp_Object Lsymbol_function(Lisp_Object nil, Lisp_Object a)
{
    one_args *f1;
    two_args *f2;
    n_args *fn;
    if (!symbolp(a)) return onevalue(nil);
    f1 = qfn1(a); f2 = qfn2(a); fn = qfnn(a);
    if ((qheader(a) & (SYM_SPECIAL_FORM | SYM_MACRO)) != 0 ||
        (f1 == undefined1 && f2 == undefined2 &&
         fn == undefinedn)) return onevalue(nil);
    else if (f1 == interpreted1 ||
             f2 == interpreted2 ||
             fn == interpretedn)
/* I wonder if onevalue(cons(...)) is really valid here. It is OK in SL mode */
        return onevalue(cons(lambda, qenv(a)));
    else if (f1 == funarged1 ||
             f2 == funarged2 ||
             fn == funargedn)
        return onevalue(cons(funarg, qenv(a)));
    else if (f1 == traceinterpreted1 ||
             f2 == traceinterpreted2 ||
             fn == traceinterpretedn)
        return onevalue(cons(lambda, qcdr(qenv(a))));
    else if (f1 == tracefunarged1 ||
             f2 == tracefunarged2 ||
             fn == tracefunargedn)
        return onevalue(cons(funarg, qcdr(qenv(a))));
    else
    {
#ifdef COMMON
        Lisp_Object b = get(a, work_symbol, nil);
#else
        Lisp_Object b = get(a, work_symbol);
#endif
/*
 * If I have already manufactured a code pointer for this function I
 * can find it on the property list - in that case I will re-use it.
 */
      while (b != nil)
        {   Lisp_Object c = qcar(b);
            if ((qheader(c) & (SYM_C_DEF | SYM_CODEPTR)) == SYM_CODEPTR)
                return onevalue(c);
            b = qcdr(b);
        }
        push(a);
/*
 * To carry a code-pointer I manufacture a sort of gensym, flagging
 * it in its header as a "code pointer object" and sticking the required
 * definition in with it.  I need to link this to the originating
 * definition in some cases to allow for preserve/restart problems wrt
 * the initialisation of function addresses that refer to C code.
 * I make the carrier using GENSYM1, but need to clear the gensym flag bit
 * to show I have a regular name for the object, and that I will not need
 * to append a serial number later on. In Common Lisp mode I let the name
 * of the gensym be just the name of the function, while in Standard Lisp
 * mode I will append a numeric suffix. I do this because in Common Lisp
 * mode the thing will print as (say) #:apply which is visibly different
 * from the name 'apply of the base function, while in Standard Lisp a name
 * like apply775 is needed to make the distinction (easily) visible.
 */
#ifdef COMMON
        b = Lgensym2(nil, a);
#else
        b = Lgensym1(nil, a);
#endif
        pop(a);
        errexit();
        set_fns(b, f1, f2, fn);
        qenv(b) = qenv(a);
#ifdef COMMON
/* in Common Lisp mode gensyms that are "unprinted" are not special */
        qheader(b) ^= (SYM_ANY_GENSYM | SYM_CODEPTR);
#else
        qheader(b) ^= (SYM_UNPRINTED_GENSYM | SYM_ANY_GENSYM | SYM_CODEPTR);
#endif
        if ((qheader(a) & SYM_C_DEF) != 0)
        {   Lisp_Object c, w;
#ifdef COMMON
            c = get(a, unset_var, nil);
#else
            c = get(a, unset_var);
#endif
            if (c == nil) c = a;
            push3(a, b, c);
            qheader(b) |= SYM_C_DEF;
            putprop(b, unset_var, c);
            errexitn(3);
            c = stack[0]; b = stack[-1];
#ifdef COMMON
            w = get(c, work_symbol, nil);
#else
            w = get(c, work_symbol);
#endif
            w = cons(b, w);
            pop(c);
            errexitn(2);
            putprop(c, work_symbol, w);
            pop2(b, a);
            errexit();
        }
        return onevalue(b);
    }
}

Lisp_Object Lspecial_form_p(Lisp_Object nil, Lisp_Object a)
{
    if (!symbolp(a)) return onevalue(nil);
    else if ((qheader(a) & SYM_SPECIAL_FORM) != 0) return onevalue(lisp_true);
    else return onevalue(nil);
}

Lisp_Object Lcodep(Lisp_Object nil, Lisp_Object a)
/*
 * This responds TRUE for the special pseudo-symbols that are used to
 * carry compiled code objects.  It returns NIL on the symbols that
 * are normally used by the user.
 */
{
    if (!symbolp(a)) return onevalue(nil);
    if ((qheader(a) & (SYM_CODEPTR | SYM_C_DEF)) == SYM_CODEPTR)
        return onevalue(lisp_true);
    else return onevalue(nil);
}

Lisp_Object getvector(int tag, int32 type, int32 size)
{
/*
 * tag is the value (e.g. TAG_SYMBOL) that will go in the low order
 * 3 bits of the pointer result.
 * type is the code (e.g. TYPE_SYMBOL) that gets packed, together with
 * the size, into a header word.
 * size is measured in bytes and must allow space for the header word.
 */
    Lisp_Object nil = C_nil;
#ifdef CHECK_FOR_CORRUPT_HEAP
    validate_all();
#endif
    for (;;)
    {   char *r = (char *)vfringe;
        unsigned int free = (unsigned int)((char *)vheaplimit - r);
        int32 alloc_size = (int32)doubleword_align_up(size);
/*
 * There is a real NASTY here - it is quite possible that I ought to implement
 * a scheme whereby large vectors can be allocated as a series of chunks so as
 * to avoid the current absolute limit on size.  Remember that the page size
 * is about 64 Kbytes for small machines but on larger ones I can have bigger
 * pages (typically 256K) and hence bigger vectors.
 */
        if (alloc_size > CSL_PAGE_SIZE - 32)
            return aerror("vector request too big");
        if (alloc_size > (int32)free)

        {   char msg[40];
/*
 * I go to a whole load of trouble here to tell the user what sort of
 * vector request provoked this garbage collection.  I wonder if the user
 * really cares - but I do very much when I am chasing after GC bugs!
 */
            switch (tag)
            {
        case TAG_SYMBOL:
                sprintf(msg, "symbol header");
                break;
        case TAG_NUMBERS:
                switch (type)
                {
            case TYPE_BIGNUM:
                    sprintf(msg, "bignum(%ld)", (long)size);
                    break;
            default:
                    sprintf(msg, "numbers(%lx,%ld)", (long)type, (long)size);
                    break;
                }
                break;
        case TAG_VECTOR:
                switch (type)
                {
            case TYPE_STRING:
                    sprintf(msg, "string(%ld)", (long)size);
                    break;
            case TYPE_BPS:
                    sprintf(msg, "BPS(%ld)", (long)size);
                    break;
            case TYPE_SIMPLE_VEC:
                    sprintf(msg, "simple vector(%ld)", (long)size);
                    break;
            case TYPE_HASH:
                    sprintf(msg, "hash table(%ld)", (long)size);
                    break;
            default:
                    sprintf(msg, "vector(%lx,%ld)", (long)type, (long)size);
                    break;
                }
                break;
        case TAG_BOXFLOAT:
                sprintf(msg, "float(%ld)", (long)size);
                break;
        default:
                sprintf(msg, "getvector(%lx,%ld)", (long)tag, (long)size);
                break;
            }
            reclaim(nil, msg, GC_VEC, alloc_size);
            errexit();
            continue;
        }
        vfringe = (Lisp_Object)(r + alloc_size);
        *((Header *)r) = type + (size << 10) + TAG_ODDS;
/*
 * DANGER: the vector allocated here is left uninitialised at this stage.
 * This is OK if the vector will contain binary information, but if it
 * will hold any Lisp_Objects it needs safe values put in PDQ.
 */
        return (Lisp_Object)(r + tag);
    }
}

Lisp_Object getvector_init(int32 n, Lisp_Object k)
{
    Lisp_Object p, nil;
    push(k);
    p = getvector(TAG_VECTOR, TYPE_SIMPLE_VEC, n);
    pop(k);
    errexit();
    if ((n & 4) != 0) n += 4;   /* Ensure last doubleword is tidy */
    while (n > 4)
    {   n -= 4;
        *(Lisp_Object *)((char *)p - TAG_VECTOR + n) = k;
    }
    return p;
}

clock_t base_time;
double *clock_stack, consolidated_time[10], gc_time;

void push_clock()
{
    clock_t t0 = read_clock();
/*
 * Provided that I do this often enough I will not suffer clock
 * wrap-around or overflow.
 */
    double delta = (double)(t0 - base_time)/(double)CLOCKS_PER_SEC;
    base_time = t0;
    *clock_stack += delta;
    *++clock_stack = 0.0;
}

double pop_clock()
{
    clock_t t0 = read_clock();
    double delta = (double)(t0 - base_time)/(double)CLOCKS_PER_SEC;
    base_time = t0;
    return delta + *clock_stack--;
}

Lisp_Object MS_CDECL Ltime(Lisp_Object nil, int nargs, ...)
{
    unsigned32 tt;
    Lisp_Object r;
    if (clock_stack == &consolidated_time[0])
    {   clock_t t0 = read_clock();
        double delta = (double)(t0 - base_time)/(double)CLOCKS_PER_SEC;
        base_time = t0;
        consolidated_time[0] += delta;
    }
    argcheck(nargs, 0, "time");
    CSL_IGNORE(nil);
    tt = (unsigned32)(1000.0 * consolidated_time[0]);
/*
 * Overflow here is around 49 days. I suppose that that is almost a
 * thinkable amount of CPU time to use.... Oh dear!
 */
    if ((tt & fix_mask) == 0) return onevalue(fixnum_of_int(tt));
    if (!signed_overflow(tt)) r = make_one_word_bignum(tt);
    else r = make_two_word_bignum((tt>>31) & 1, tt & 0x7fffffff);
    errexit();
    return onevalue(r);
}

Lisp_Object MS_CDECL Lgctime(Lisp_Object nil, int nargs, ...)
{
    argcheck(nargs, 0, "gctime");
    CSL_IGNORE(nil);
    return onevalue(fixnum_of_int((int32)(1000.0 * gc_time)));
}

#ifdef COMMON

Lisp_Object MS_CDECL Ldecoded_time(Lisp_Object nil, int nargs, ...)
{
    time_t t0 = time(NULL);
/*
 *        tm_sec      -- seconds 0..59
 *        tm_min      -- minutes 0..59
 *        tm_hour     -- hour of day 0..23
 *        tm_mday     -- day of month 1..31
 *        tm_mon      -- month 0..11
 *        tm_year     -- years since 1900
 *        tm_wday     -- day of week, 0..6 (Sunday..Saturday)
 *        tm_yday     -- day of year, 0..365
 *        tm_isdst    -- >0 if daylight savings time
 *                    -- ==0 if not DST
 *                    -- <0 if don't know
 */
    struct tm *tbuf = localtime(&t0);
    Lisp_Object r, *p = &mv_2;
    int w;
    argcheck(nargs, 0, "get-decoded-time");
    r = fixnum_of_int(tbuf->tm_sec);
    *p++ = fixnum_of_int(tbuf->tm_min);
    *p++ = fixnum_of_int(tbuf->tm_hour);
    *p++ = fixnum_of_int(tbuf->tm_mday);
    *p++ = fixnum_of_int(tbuf->tm_mon+1);
    *p++ = fixnum_of_int(tbuf->tm_year+1900);
    w = tbuf->tm_wday;
    *p++ = fixnum_of_int(w == 0 ? 6 : w-1);
    *p++ = tbuf->tm_isdst > 0 ? lisp_true : nil;
    *p++ = fixnum_of_int(0);  /* Time zone info not available? */
    return nvalues(r, 9);
}

#endif

Lisp_Object MS_CDECL Ldate(Lisp_Object nil, int nargs, ...)
{
    Lisp_Object w;
    time_t t = time(NULL);
    char today[32];
    argcheck(nargs, 0, "date");
    CSL_IGNORE(nil);
    strcpy(today, ctime(&t));  /* e.g. "Sun Sep 16 01:03:52 1973\n" */
    today[24] = 0;             /* loses final '\n' */
    w = make_string(today);
    errexit();
    return onevalue(w);
}

Lisp_Object MS_CDECL Ldatestamp(Lisp_Object nil, int nargs, ...)
/*
 * Returns date-stamp integer, which on many systems will be the
 * number of seconds between 1970.0.0 and now, but which could be
 * pretty-well other things, as per the C "time_t" type.
 */
{
    Lisp_Object w;
    time_t t = time(NULL);
    unsigned32 n = (unsigned32)t;   /* NON-PORTABLE assumption about time_t */
    argcheck(nargs, 0, "datestamp");
    CSL_IGNORE(nil);
    if ((n & fix_mask) == 0) w = fixnum_of_int(n);
    else if ((n & 0xc0000000U) == 0) w = make_one_word_bignum(n);
    else w = make_two_word_bignum((n >> 31) & 1, n & 0x7fffffff);
    errexit();
    return onevalue(w);
}

#define STR24HDR (TAG_ODDS+TYPE_STRING+((24+4)<<10))

static int getint(char *p, int len)
{
    int r = 0;
    while (len-- != 0)
    {   int c = *p++;
        if (c == ' ') c = '0';
        r = 10*r + (c - '0');
    }
    return r;
}

static int getmon(char *s)
{
    int c1 = s[0], c2 = s[1], c3 = s[2], r = -1, w;
    char *m = "janfebmaraprmayjunjulaugsepoctnovdec";
    if (isupper(c1)) c1 = tolower(c1);
    if (isupper(c2)) c2 = tolower(c2);
    if (isupper(c3)) c3 = tolower(c3);
    for (w=0; w<12; w++)
    {   if (c1==m[0] && c2==m[1] && c3==m[2])
        {   r = w;
            break;
        }
        m += 3;
    }
    return r;
}

static Lisp_Object Ldatelessp(Lisp_Object nil, Lisp_Object a, Lisp_Object b)
/*
 * This is maybe a bit of an abomination!  The functions (date) and
 * (filedate "filename") [and also (modulep 'modulename)] return times
 * as strings of 24 characters.  This function decodes these and
 * sorts out which time is earlier.  The alternative would be to provide
 * a collection of functions that returned coded times (as in C "time_t"),
 * but I have greater doubts about making those utterly portable, while the
 * textual arrangement used here seems fairly robust (until you start
 * worrying about carrying a portable machine across time zones or switching
 * to daylight savings time).
 */
{
    char *aa, *bb;
    CSLbool res;
    int wa, wb;
    if (!is_vector(a) || !is_vector(b) ||
        vechdr(a) != STR24HDR ||
        vechdr(b) != STR24HDR) return aerror2("datelessp", a, b);
    aa = (char *)a + (4 - TAG_VECTOR);
    bb = (char *)b + (4 - TAG_VECTOR);
/*
 * Layout is eg. "Wed May 12 15:50:23 1993"
 *                012345678901234567890123
 * Note that the year is 4 digits so that the year 2000 should hold
 * no special terrors JUST here.
 */
    if ((wa = getint(aa+20, 4)) != (wb = getint(bb+20, 4))) res = wa < wb;
    else if ((wa = getmon(aa+4)) != (wb = getmon(bb+4))) res = wa < wb;
    else if ((wa = getint(aa+8, 2)) != (wb = getint(bb+8, 2))) res = wa < wb;
    else if ((wa = getint(aa+11, 2)) != (wb = getint(bb+11, 2))) res = wa < wb;
    else if ((wa = getint(aa+14, 2)) != (wb = getint(bb+14, 2))) res = wa < wb;
    else if ((wa = getint(aa+17, 2)) != (wb = getint(bb+17, 2))) res = wa < wb;
    else res = NO;
    return onevalue(Lispify_predicate(res));
}

static Lisp_Object Lrepresentation1(Lisp_Object nil, Lisp_Object a)
/*
 * Intended for debugging, and use with indirect (q.v.)
 */
{
    int32 top = (int32)a & 0xf8000000U;
    CSL_IGNORE(nil);
    if (top == 0 || top == 0xf8000000U)
        return onevalue(fixnum_of_int((int32)a));
    a = make_one_word_bignum((int32)a);
    errexit();
    return onevalue(a);
}

static Lisp_Object Lrepresentation2(Lisp_Object nil,
                                   Lisp_Object a, Lisp_Object b)
/*
 * Intended for debugging, and use with indirect (q.v.).  arg2, if
 * present and non-nil makes this more verbose.
 */
{
    int32 top = (int32)a & 0xf8000000U;
    CSL_IGNORE(nil);
    if (b != nil) trace_printf(" %.8lx ", (long)(unsigned32)a);
    if (top == 0 || top == 0xf8000000U)
        return onevalue(fixnum_of_int((int32)a));
    a = make_one_word_bignum((int32)a);
    errexit();
    return onevalue(a);
}

Lisp_Object Lindirect(Lisp_Object nil, Lisp_Object a)
{
    CSL_IGNORE(nil);
    return onevalue(*(Lisp_Object *)thirty_two_bits(a));
}

setup_type const funcs1_setup[] =
{
    {"acons",                   wrong_no_na, wrong_no_nb, Lacons},
    {"atom",                    Latom, too_many_1, wrong_no_1},
    {"boundp",                  Lboundp, too_many_1, wrong_no_1},

    {"car",                     Lcar, too_many_1, wrong_no_1},
    {"car*",                    Lcar_star, too_many_1, wrong_no_1},
    {"cdr",                     Lcdr, too_many_1, wrong_no_1},
    {"caar",                    Lcaar, too_many_1, wrong_no_1},
    {"cadr",                    Lcadr, too_many_1, wrong_no_1},
    {"cdar",                    Lcdar, too_many_1, wrong_no_1},
    {"cddr",                    Lcddr, too_many_1, wrong_no_1},
    {"caaar",                   Lcaaar, too_many_1, wrong_no_1},
    {"caadr",                   Lcaadr, too_many_1, wrong_no_1},
    {"cadar",                   Lcadar, too_many_1, wrong_no_1},
    {"caddr",                   Lcaddr, too_many_1, wrong_no_1},
    {"cdaar",                   Lcdaar, too_many_1, wrong_no_1},
    {"cdadr",                   Lcdadr, too_many_1, wrong_no_1},
    {"cddar",                   Lcddar, too_many_1, wrong_no_1},
    {"cdddr",                   Lcdddr, too_many_1, wrong_no_1},
    {"caaaar",                  Lcaaaar, too_many_1, wrong_no_1},
    {"caaadr",                  Lcaaadr, too_many_1, wrong_no_1},
    {"caadar",                  Lcaadar, too_many_1, wrong_no_1},
    {"caaddr",                  Lcaaddr, too_many_1, wrong_no_1},
    {"cadaar",                  Lcadaar, too_many_1, wrong_no_1},
    {"cadadr",                  Lcadadr, too_many_1, wrong_no_1},
    {"caddar",                  Lcaddar, too_many_1, wrong_no_1},
    {"cadddr",                  Lcadddr, too_many_1, wrong_no_1},
    {"cdaaar",                  Lcdaaar, too_many_1, wrong_no_1},
    {"cdaadr",                  Lcdaadr, too_many_1, wrong_no_1},
    {"cdadar",                  Lcdadar, too_many_1, wrong_no_1},
    {"cdaddr",                  Lcdaddr, too_many_1, wrong_no_1},
    {"cddaar",                  Lcddaar, too_many_1, wrong_no_1},
    {"cddadr",                  Lcddadr, too_many_1, wrong_no_1},
    {"cdddar",                  Lcdddar, too_many_1, wrong_no_1},
    {"cddddr",                  Lcddddr, too_many_1, wrong_no_1},

    {"qcar",                    Lcar, too_many_1, wrong_no_1},
    {"qcdr",                    Lcdr, too_many_1, wrong_no_1},
    {"qcaar",                   Lcaar, too_many_1, wrong_no_1},
    {"qcadr",                   Lcadr, too_many_1, wrong_no_1},
    {"qcdar",                   Lcdar, too_many_1, wrong_no_1},
    {"qcddr",                   Lcddr, too_many_1, wrong_no_1},

    {"bpsp",                    Lbpsp, too_many_1, wrong_no_1},
    {"codep",                   Lcodep, too_many_1, wrong_no_1},
    {"constantp",               Lconstantp, too_many_1, wrong_no_1},
    {"date",                    wrong_no_na, wrong_no_nb, Ldate},
    {"datestamp",               wrong_no_na, wrong_no_nb, Ldatestamp},
    {"enable-backtrace",        Lenable_backtrace, too_many_1, wrong_no_1},
    {"error",                   Lerror1, Lerror2, Lerror},
    {"error1",                  wrong_no_na, wrong_no_nb, Lerror0},
#ifdef NAG
    {"unwind",                  wrong_no_na, wrong_no_nb, Lunwind},
#endif
    {"eq-safe",                 Leq_safe, too_many_1, wrong_no_1},
    {"fixp",                    Lfixp, too_many_1, wrong_no_1},
    {"floatp",                  Lfloatp, too_many_1, wrong_no_1},
    {"fluidp",                  Lsymbol_specialp, too_many_1, wrong_no_1},
    {"gctime",                  wrong_no_na, wrong_no_nb, Lgctime},
    {"globalp",                 Lsymbol_globalp, too_many_1, wrong_no_1},
    {"hash-table-p",            Lhash_table_p, too_many_1, wrong_no_1},
    {"indirect",                Lindirect, too_many_1, wrong_no_1},
    {"integerp",                Lintegerp, too_many_1, wrong_no_1},
    {"intersection",            too_few_2, Lintersect, wrong_no_2},
    {"list2",                   too_few_2, Llist2, wrong_no_2},
    {"list2*",                  wrong_no_na, wrong_no_nb, Llist2star},
    {"list3",                   wrong_no_na, wrong_no_nb, Llist3},
    {"make-global",             Lmake_global, too_many_1, wrong_no_1},
    {"make-special",            Lmake_special, too_many_1, wrong_no_1},
    {"mkquote",                 Lmkquote, too_many_1, wrong_no_1},
    {"ncons",                   Lncons, too_many_1, wrong_no_1},
    {"numberp",                 Lnumberp, too_many_1, wrong_no_1},
    {"pair",                    too_few_2, Lpair, wrong_no_2},
    {"put",                     wrong_no_na, wrong_no_nb, Lputprop},
    {"remprop",                 too_few_2, Lremprop, wrong_no_2},
    {"representation",          Lrepresentation1, too_many_1, wrong_no_1},
    {"representation",          too_few_2, Lrepresentation2, wrong_no_2},
    {"rplaca",                  too_few_2, Lrplaca, wrong_no_2},
    {"rplacd",                  too_few_2, Lrplacd, wrong_no_2},
    {"set",                     too_few_2, Lset, wrong_no_2},
    {"special-form-p",          Lspecial_form_p, too_many_1, wrong_no_1},
    {"stop",                    Lstop, too_many_1, wrong_no_1},
    {"symbol-function",         Lsymbol_function, too_many_1, wrong_no_1},
    {"symbol-value",            Lsymbol_value, too_many_1, wrong_no_1},
    {"time",                    wrong_no_na, wrong_no_nb, Ltime},
    {"datelessp",               too_few_2, Ldatelessp, wrong_no_2},
    {"union",                   too_few_2, Lunion, wrong_no_2},
    {"unmake-global",           Lunmake_global, too_many_1, wrong_no_1},
    {"unmake-special",          Lunmake_special, too_many_1, wrong_no_1},
    {"xcons",                   too_few_2, Lxcons, wrong_no_2},
/* I provide both IDP and SYMBOLP in both modes... */
    {"symbolp",                 Lsymbolp, too_many_1, wrong_no_1},
    {"idp",                     Lsymbolp, too_many_1, wrong_no_1},
/* I support the Common Lisp names here in both modes */
    {"simple-string-p",         Lstringp, too_many_1, wrong_no_1},
    {"simple-vector-p",         Lsimple_vectorp, too_many_1, wrong_no_1},
#ifdef COMMON
    {"fill-vector",             wrong_no_na, wrong_no_nb, Lfill_vector},
    {"get",                     too_few_2, Lget, Lget_3},
    {"get-decoded-time",        wrong_no_0a, wrong_no_0b, Ldecoded_time},
    {"arrayp",                  Larrayp, too_many_1, wrong_no_1},
    {"complex-arrayp",          Lcomplex_arrayp, too_many_1, wrong_no_1},
    {"short-floatp",            Lshort_floatp, too_many_1, wrong_no_1},
    {"single-floatp",           Lsingle_floatp, too_many_1, wrong_no_1},
    {"double-floatp",           Ldouble_floatp, too_many_1, wrong_no_1},
    {"long-floatp",             Llong_floatp, too_many_1, wrong_no_1},
    {"rationalp",               Lrationalp, too_many_1, wrong_no_1},
    {"complexp",                Lcomplexp, too_many_1, wrong_no_1},
    {"consp",                   Lconsp, too_many_1, wrong_no_1},
    {"convert-to-array",        Lconvert_to_array, too_many_1, wrong_no_1},
    {"convert-to-struct",       Lconvert_to_struct, too_many_1, wrong_no_1},
    {"identity",                Lidentity, too_many_1, wrong_no_1},
    {"list",                    Lncons, Llist2, Llist},
    {"list*",                   Lidentity, Lcons, Lliststar},
    {"listp",                   Llistp, too_many_1, wrong_no_1},
    {"bit-vector-p",            Lsimple_bit_vector_p, too_many_1, wrong_no_1},
    {"simple-bit-vector-p",     Lsimple_bit_vector_p, too_many_1, wrong_no_1},
    {"stringp",                 Lc_stringp, too_many_1, wrong_no_1},
    {"structp",                 Lstructp, too_many_1, wrong_no_1},
    {"flag",                    too_few_2, Lflag, wrong_no_2},
    {"flagp",                   too_few_2, Lflagp, wrong_no_2},
    {"flagpcar",                too_few_2, Lflagpcar, wrong_no_2},
    {"remflag",                 too_few_2, Lremflag, wrong_no_2},
    {"time*",                   wrong_no_na, wrong_no_nb, Ltime},
#else
    {"get",                     too_few_2, Lget, wrong_no_2},
    {"convert-to-evector",      Lconvert_to_struct, too_many_1, wrong_no_1},
    {"evectorp",                Lstructp, too_many_1, wrong_no_1},
    {"get*",                    too_few_2, Lget, wrong_no_2},
    {"pairp",                   Lconsp, too_many_1, wrong_no_1},
/* I provide CONSP as well as PAIRP since otherwise I get muddled */
    {"consp",                   Lconsp, too_many_1, wrong_no_1},
    {"flag",                    too_few_2, Lflag, wrong_no_2},
    {"flagp",                   too_few_2, Lflagp, wrong_no_2},
    {"flagpcar",                too_few_2, Lflagpcar, wrong_no_2},
    {"flagp**",                 too_few_2, Lflagp, wrong_no_2},
    {"remflag",                 too_few_2, Lremflag, wrong_no_2},
    {"stringp",                 Lstringp, too_many_1, wrong_no_1},
    {"threevectorp",            Lthreevectorp, too_many_1, wrong_no_1},
    {"vectorp",                 Lsimple_vectorp, too_many_1, wrong_no_1},
#endif
    {NULL,                      0, 0, 0}
};

/* end of fns1.c */
