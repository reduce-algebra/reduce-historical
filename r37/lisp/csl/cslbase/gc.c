/* File gc.c                    Copyright (c) Codemist Ltd, 1990-2003 */

/*
 * Garbage collection.
 *    Fourth major version - now using Foster-style
 *    algorithm for relocating vector heap, and support for handling
 *    BPS via segmented handles. Pointer-reversing mark phase to go
 *    with same.
 *
 *    Furthermore there is (optionally) a copying 2-space garbage
 *    collector as well as the mark/slide one.  Now do you understand
 *    why this file seems so very long?
 *
 * The code in parts of this file (and also in preserve.c & restart.c)
 * is painfully sensitive to memory layout and I have some messy
 * conditional inclusion of code depending on whether a Lisp_Object is
 * a 32 or 64-bit value.
 */

/*
 * This code may be used and modified, and redistributed in binary
 * or source form, subject to the "CCL Public License", which should
 * accompany it. This license is a variant on the BSD license, and thus
 * permits use of code derived from this in either open and commercial
 * projects: but it does require that updates to this code be made
 * available back to the originators of the package.
 * Before merging other code in with this or linking this code
 * with other packages or libraries please check that the license terms
 * of the other material are compatible with those of this.
 */


/* Signature: 0651f4cb 13-Jul-2003 */

#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "machine.h"
#include "tags.h"
#include "cslerror.h"
#include "externs.h"
#include "arith.h"
#include "entries.h"
#include "sys.h"
#include "stream.h"
#ifdef SOCKETS
#include "sockhdr.h"
#endif
#ifdef TIMEOUT
#include "timeout.h"
#endif

/*
 * This function is provided so that if some C compiler generates bad
 * code and I can not see a better way of correcting things, and if the
 * bad code goes away when there are lots of function calls to cause the
 * compiler to unload its registers etc, then I can write things like
 *     dummy_function_call("Ha Ha", reference_this_var, and_this);
 * without it having any big effect on the results I obtain. I will
 * generally want to guard any such un-natural calls by a test that identifies
 * the C compiler involved. Of course I must try not to use this as a way
 * of permitting system-sensitive code to creep into CSL...
 */

void MS_CDECL dummy_function_call(char *why, ...)
{
    CSL_IGNORE(why);
    return;
}

CSLbool gc_method_is_copying;     /* YES if copying, NO if sliding */
int32 gc_number = 0;

static intxx cons_cells, symbol_heads, strings, user_vectors,
             big_numbers, box_floats, bytestreams, other_mem,
             litvecs, getvecs;

#define is_immed(x) (is_immed_or_cons(x) && !is_cons(x))

#ifndef DEMO_MODE

static void non_recursive_mark(Lisp_Object *top)
{
/*
 * This code is written about as neatly as I know how ... I want to think of
 * it in terms of three chunks - descending down into lists, regular
 * climbing back out, and the special case of climbing back out when I have
 * just processed a vector.  I like to think of this as a finite state
 * machine with three major groups of states, and a bunch of subsidiary
 * states that deal with (e.g.) scanning along vectors.
 */
    Lisp_Object b = (Lisp_Object)top,
                p = *top,
                w,
                nil = C_nil;
    Header h, *q;
    intxx i;
/*
 * When I get to descend I have b as an unmarked address that is either
 * equal to top, or is a back-pointer as set up below.  p is a normal
 * (i.e. unmarked) Lisp pointer, representing a tree to be marked. Only
 * at the very top of a call can p be immediate data at descend, and in that
 * case no marking is needed.
 * NB that the initial back pointer will seem tagged as either a CONS or a
 * SYMBOL, but certainly as a pointer datatype.
 */

descend:
    switch ((int)p & TAG_BITS)
    {
/*
 * If I have a cons cell I need to check if it has been visited before or
 * if one or both components are immediate - and extend my backwards
 * chain one step.
 */
case TAG_CONS:
#ifdef COMMON
        if (p == nil) goto ascend;
#endif
        w = qcar(p);
        if (is_immed(w))
        {   if (is_marked_i(w)) goto ascend;
/*
 * To test if this cons cell was marked I had to classify the item
 * in its car, and if this was immediate data it makes sense to go
 * right ahead and probe the cdr.
 */
            qcar(p) = flip_mark_bit_i(w);
            w = qcdr(p);
/*
 * Since I am not allowing myself to descend onto immediate data
 * I check for it here, and if both car and cdr of p were immediate
 * I can ascend forthwith.
 */
            if (is_immed(w) || w == nil) goto ascend;
/*
 * Here I fill in a back-pointer and descend into the cdr of a pair.
 */
            qcdr(p) = flip_mark_bit_p(b);
            b = p;
            p = w;
            goto descend;
        }
        else if (is_marked_p(w)) goto ascend;
/*
 * Here I fill in a back-pointer and descend into the car of a pair.
 * [would it be worth taking a special case on w == nil here?]
 */
        qcar(p) = flip_mark_bit_p(b);
        b = p;
        p = w;
        goto descend;

/*
 *  case TAG_FIXNUM:
 *  case TAG_ODDS:
 *  case TAG_SFLOAT:
 */
default:
        return;             /* assert (b==(Lisp_Object)top) here. */

case TAG_SYMBOL:
#ifndef COMMON
        if (p == nil) goto ascend;
#endif
        h = qheader(p);
/*
 * When I have finished every item that has been visited must be marked,
 * with cons cells marked in their car fields and vectors etc in the header
 * word.  Furthermore the header of all vectors (including symbols) must
 * have been replaced by the start of a back-pointer chain identifying the
 * words that started off pointing at the vector.  The pointers in this
 * chain must be marked, word-aligned pointers. Note a special curiosity:
 * the back-chain of references to a vector can thread through the CDR
 * field of CONS cells and through either odd or even words within vectors.
 * Thus althouh marked with the pointer mark bit the rest of tagging on these
 * chain words is a bit funny! Specifically the tag bits will say "0" or "4",
 * ie CONS or SYMBOL (and not ODDS).
 */
        if (!is_odds(h) || is_marked_h(h))  /* Visited before       */
        {   q = &qheader(p);                /* where I should chain */
            p = h;                          /* the previous header  */
            goto ascend_from_vector;
        }
/*
 * Now this is the first visit to a symbol.
 */
        qheader(p) = h = flip_mark_bit_h(h);
/*
 * When components of a symbol are immediate or nil I do nothing.
 * (the test for nil is because I expect it to be cheap and to catch
 * common cases)
 */
        w = qvalue(p);
        if (!is_immed(w) && w != nil)
        {   qvalue(p) = flip_mark_bit_p(b);
            b = p;
            p = w;
            goto descend;
        }
        w = qenv(p);
        if (!is_immed(w) && w != nil)
        {   qenv(p) = flip_mark_bit_p(b);
            b = p;
            p = w;
            goto descend;
        }
        w = qpname(p);
        if (!is_immed(w) && w != nil)
        {   qpname(p) = flip_mark_bit_p(b);
            b = p;
            p = w;
            goto descend;
        }
        w = qplist(p);
        if (!is_immed(w) && w != nil)
        {   qplist(p) = flip_mark_bit_p(b);
            b = p;
            p = w;
            goto descend;
        }
        w = qfastgets(p);
        if (!is_immed(w) && w != nil)
        {   qfastgets(p) = flip_mark_bit_p(b);
            b = p;
            p = w;
            goto descend;
        }
#ifdef COMMON
        w = qpackage(p);
        if (!is_immed(w) && w != nil)
        {   qpackage(p) = flip_mark_bit_p(b);
            b = p;
            p = w;
            goto descend;
        }
#endif
/*
 * When all components of the vector are marked I climb up the
 * back-pointer chain.
 */
        q = &qheader(p);
        p = h;
        goto ascend_from_vector;

case TAG_NUMBERS:
        h = numhdr(p);
        if (!is_odds(h) || is_marked_h(h)) /* marked already. */
        {   q = &numhdr(p);
            p = h;
            goto ascend_from_vector;
        }
/*
 * For CSL the only case here is that of big integers, which have just
 * binary data in them.  For Common Lisp I also have to cope with
 * ratios and complex numbers.
 */
        if (is_bignum_header(h))
        {   q = &numhdr(p);
            p = flip_mark_bit_h(h);
            goto ascend_from_vector;
        }
#ifdef COMMON
        numhdr(p) = h = flip_mark_bit_h(h);
        w = real_part(p);   /* Or numerator of a ratio! */
        if (!is_immed(w))
        {   real_part(p) = flip_mark_bit_p(b);
            b = p;
            p = w;
            goto descend;
        }
        w = imag_part(p);   /* Or denominator of a ratio! */
        if (!is_immed(w))
        {   imag_part(p) = flip_mark_bit_p(b);
            b = p;
            p = w;
            goto descend;
        }
/*
 * get here if both components of a ratio/complex are immediate (e.g fixnums)
 */
        q = &numhdr(p);
        p = h;
        goto ascend_from_vector;
#else
        term_printf("Bad numeric code detected in GC\n");
        ensure_screen();
        abort(); /* Bad numeric type in CSL mode. */
#endif

case TAG_BOXFLOAT:
        h = flthdr(p);
        if (!is_odds(h) || is_marked_h(h))
        {   q = &flthdr(p);
            p = h;
            goto ascend_from_vector;
        }
        q = &flthdr(p);
        p = flip_mark_bit_h(h);
        goto ascend_from_vector;

case TAG_VECTOR:
        h = vechdr(p);
        if (!is_odds(h) || is_marked_h(h))
        {   q = &vechdr(p);
            p = h;
            goto ascend_from_vector;
        }
        if (vector_holds_binary(h))
        {   q = &vechdr(p);
            p = flip_mark_bit_h(h);
            goto ascend_from_vector;
        }
        vechdr(p) = h = flip_mark_bit_h(h);
        i = (intxx)doubleword_align_up(length_of_header(h));
        if (is_mixed_header(h)) 
            i = 4*CELL;  /* Only use first few pointers */
        while (i >= 2*CELL)
        {   i -= CELL;
            q = (Header *)((char *)p - TAG_VECTOR + i);
            w = *q;
            if (is_immed(w) || w == nil) continue;
/*
 * For vectors I have to use all available mark bits to keep track of
 * where I am...
 */
            if (i == CELL)
/*
 * When descending into the first (or only) word of a vector I leave the
 * back-pointer marked, and note that the header word just before it
 * will be marked (either as a header word or as a pointer)
 */
            {   *q = flip_mark_bit_p(b);
                b = p;
                p = w;
            }
#ifndef ADDRESS_64
            else if ((i & 4) == 0)
#endif
/*
 * When descending a pointer at an even (word) address I leave the
 * back-pointer unmarked.
 */
            {   *q = b;
                b = (Lisp_Object)((char *)p + i);
                p = w;
            }
#ifndef ADDRESS_64
            else
/*
 * Finally when I descend into a pointer at an odd (word) address other
 * than the special case of the first such, I leave an unmarked back-pointer
 * but mark the word before the one I am following.  The effect of all this is
 * that when I get back to the vector I am able to discriminate between these
 * various cases...
 */
            {   *q = b;
                b = (Lisp_Object)((char *)p + i - 4);
                p = w;
                w = *(Lisp_Object *)((char *)b - TAG_VECTOR);
                if (is_immed(w)) w = flip_mark_bit_i(w);
                else w = flip_mark_bit_p(w);
                *(Lisp_Object *)((char *)b - TAG_VECTOR) = w;
            }
#endif /* ADDRESS_64 */
            goto descend;
        }
/*
 * I drop through to here if all items in the vector were in fact
 * immediate values (e.g. fixnums), and thus there was no need to
 * dig deeper.
 */
        q = &vechdr(p);
        p = h;
        goto ascend_from_vector;
    }





/*
 * When I get to ascend b is a back-pointer, and p is an unmarked pointer
 * to be put back into the place I descended through.
 */
ascend:
    if (b == (Lisp_Object)top) return;

    switch ((int)b & TAG_BITS)
    {
default:
        term_printf("Bad tag bits in GC\n");
        ensure_screen();
        abort();

case TAG_CONS:
        w = qcdr(b);
        if (is_immed(w) || w == nil)
        {   w = qcar(b);
            qcar(b) = flip_mark_bit_p(p);
            p = b;
            b = flip_mark_bit_p(w);
            goto ascend;
        }
        else if (is_marked_p(w))
        {   qcdr(b) = p;
            p = b;
            b = flip_mark_bit_p(w);
            goto ascend;
        }
        else
        {   qcdr(b) = qcar(b);
            qcar(b) = flip_mark_bit_p(p);
            p = w;
            goto descend;
        }

case TAG_SYMBOL:
#ifdef COMMON
        w = qpackage(b);
        if (!is_immed(w) && is_marked_p(w))
        {   qpackage(b) = p;
            goto try_nothing;
        }
#endif
        w = qfastgets(b);
        if (!is_immed(w) && is_marked_p(w))
        {   qfastgets(b) = p;
            goto try_package;
        }
        w = qplist(b);
        if (!is_immed(w) && is_marked_p(w))
        {   qplist(b) = p;
            goto try_fastgets;
        }
        w = qpname(b);
        if (!is_immed(w) && is_marked_p(w))
        {   qpname(b) = p;
            goto try_plist;
        }
        w = qenv(b);
        if (!is_immed(w) && is_marked_p(w))
        {   qenv(b) = p;
            goto try_pname;
        }
        w = qvalue(b);
        if (!is_immed(w) && is_marked_p(w))
        {   qvalue(b) = p;
            goto try_env;
        }
        term_printf("Backpointer not found in GC\n");
        ensure_screen();
        abort();            /* backpointer not found */

try_env:
        p = qenv(b);
        if (!is_immed(p) && p != nil && !is_marked_p(p))
        {   qenv(b) = w;
            goto descend;
        }
try_pname:
        p = qpname(b);
        if (!is_immed(p) && p != nil && !is_marked_p(p))
        {   qpname(b) = w;
            goto descend;
        }
try_plist:
        p = qplist(b);
        if (!is_immed(p) && p != nil && !is_marked_p(p))
        {   qplist(b) = w;
            goto descend;
        }
try_fastgets:
        p = qfastgets(b);
        if (!is_immed(p) && p != nil && !is_marked_p(p))
        {   qfastgets(b) = w;
            goto descend;
        }
try_package:
#ifdef COMMON
        p = qpackage(b);
        if (!is_immed(p) && p != nil && !is_marked_p(p))
        {   qpackage(b) = w;
            goto descend;
        }
try_nothing:
#endif
        q = &qheader(b);
        p = *q;
        b = flip_mark_bit_p(w);
        goto ascend_from_vector;

#ifdef COMMON
case TAG_NUMBERS:
/*
 * If I get back to a NUMBERS than it must have been a ratio or a complex.
 */
        w = imag_part(b);
        if (is_immed(w))
        {   w = real_part(b);
            real_part(b) = p;
            q = &numhdr(b);
            p = *q;
            b = flip_mark_bit_p(w);
            goto ascend_from_vector;
        }
        else if (is_marked_p(w))
        {   imag_part(b) = p;
            q = &numhdr(p);
            p = *q;
            b = flip_mark_bit_p(w);
            goto ascend_from_vector;
        }
        else
        {   imag_part(b) = real_part(b);
            real_part(b) = p;
            p = w;
            goto descend;
        }
#endif

case TAG_VECTOR:
/*
 * If I get back to a vector it must have been a vector of Lisp_Objects,
 * not a vector of binary data.  My back-pointer points part-way into it.
 * The back-pointer will be doubleword aligned, so on 32-bit systems
 * it is not quite enough to tell me which cell of the vector was involved,
 * and so in that case I do a further inspection of mark bits in the two
 * parts of the doubelword concerned.
 */
        w = *(Lisp_Object *)((char *)b - TAG_VECTOR);
#ifndef ADDRESS_64
        if (is_immed(w) || is_marked_p(w))
/*
 * Here I had been marking the pointer that was stored at an odd (word)
 * address.
 */
        {   Lisp_Object w1 = *(Lisp_Object *)((char *)b - TAG_VECTOR + 4);
            *(Lisp_Object *)((char *)b - TAG_VECTOR + 4) = p;
            if (is_marked_p(w1))     /* End of this vector */
            {   q = (Header *)((char *)b - TAG_VECTOR);
                p = w;
                b = flip_mark_bit_p(w1);
                goto ascend_from_vector;
            }
            p = w;
            w = w1;
            if (!is_immed(p))
            {   p = flip_mark_bit_p(p);
                if (p != nil)
                {   *(Lisp_Object *)((char *)b - TAG_VECTOR) = w1;
                    goto descend;
                }
            }
            else p = flip_mark_bit_i(p);
        }
#endif /* ADDRESS_64 */
        *(Lisp_Object *)((char *)b - TAG_VECTOR) = p;
/*
 * Now the doubleword I returned to has been marked (and tidied up),
 * so I need to scan back towards the header.
 */
scan_vector_more:
        for (;;)
        {   Lisp_Object w2;
/*
 * NB on the next line I step back by 8 on both 32 and 64-bit machines!
 * That is because the back-pointers I use can only refer to a doubleword
 * so on 32-bit systems I have to go 2 cells at a go. Ugh.
 */
            b = (Lisp_Object)((char *)b - 8);
            w2 = *(Lisp_Object *)((char *)b - TAG_VECTOR);
#ifndef ADDRESS_64
            p = *(Lisp_Object *)((char *)b - TAG_VECTOR + CELL);
#endif
            if ((is_odds(w2) && is_header(w2)) ||
                (!is_immed(w2) && is_marked_p(w2)))
/*
 * In this case I have reached the doubleword containing the header.
 */
            {
#ifndef ADDRESS_64
                if (!is_immed(p) && p != nil)
                {   *(Lisp_Object *)((char *)b - TAG_VECTOR + CELL) =
                        flip_mark_bit_p(w);
                    goto descend;
                }
                else
#endif
                {   q = (Header *)((char *)b - TAG_VECTOR);
                    p = w2;
                    b = w;
                    goto ascend_from_vector;
                }
            }
/*
 * Otherwise I have another general doubleword to cope with.
 */
#ifndef ADDRESS_64
            if (!is_immed(p) && p != nil)
            {   if (is_immed(w2)) w2 = flip_mark_bit_i(w2);
                else w2 = flip_mark_bit_p(w2);
                *(Lisp_Object *)((char *)b - TAG_VECTOR) = w2;
                *(Lisp_Object *)((char *)b - TAG_VECTOR + CELL) = w;
                goto descend;
            }
#endif
            if (!is_immed(w2) && w2 != nil)
            {   p = w2;
                *(Lisp_Object *)((char *)b - TAG_VECTOR) = w;
                goto descend;
            }
            continue;   /* Step back another doubleword */
        }
    }

ascend_from_vector:
/*
 * Here the item just marked was a vector.  I need to leave a reversed
 * chain of pointers through its header word. q points to that header,
 * and p contains what used to be in the word at q.
 */
    if (b == (Lisp_Object)top)
    {   *q = flip_mark_bit_p(b);
        *top = p;
        return;
    }

    switch ((int)b & TAG_BITS)
    {
default:
        term_printf("Bad tag bits in GC\n");
        ensure_screen();
        abort();

case TAG_CONS:
        w = qcdr(b);
        if (is_immed(w) || w == nil)
        {   w = qcar(b);
            qcar(b) = p;
            *q = flip_mark_bit_p((Lisp_Object *)&qcar(b));
            p = b;
            b = flip_mark_bit_p(w);
            goto ascend;
        }
        else if (is_marked_p(w))
        {   qcdr(b) = p;
            *q = flip_mark_bit_p((Lisp_Object *)&qcdr(b));
            p = b;
            b = flip_mark_bit_p(w);
            goto ascend;
        }
        else
        {   qcdr(b) = qcar(b);
            qcar(b) = p;
            *q = flip_mark_bit_p((Lisp_Object *)&qcar(b));
            p = w;
            goto descend;
        }

case TAG_SYMBOL:
#ifdef COMMON
        w = qpackage(b);
        if (!is_immed(w) && is_marked_p(w))
        {   qpackage(b) = p;
            *q = flip_mark_bit_p((Lisp_Object *)&qpackage(b));
            goto try_nothing;
        }
#endif
        w = qfastgets(b);
        if (!is_immed(w) && is_marked_p(w))
        {   qfastgets(b) = p;
            *q = flip_mark_bit_p((Lisp_Object *)&qfastgets(b));
            goto try_package;
        }
        w = qplist(b);
        if (!is_immed(w) && is_marked_p(w))
        {   qplist(b) = p;
            *q = flip_mark_bit_p((Lisp_Object *)&qplist(b));
            goto try_fastgets;
        }
        w = qpname(b);
        if (!is_immed(w) && is_marked_p(w))
        {   qpname(b) = p;
            *q = flip_mark_bit_p((Lisp_Object *)&qpname(b));
            goto try_plist;
        }
        w = qenv(b);
        if (!is_immed(w) && is_marked_p(w))
        {   qenv(b) = p;
            *q = flip_mark_bit_p((Lisp_Object *)&qenv(b));
            goto try_pname;
        }
        w = qvalue(b);
        if (!is_immed(w) && is_marked_p(w))
        {   qvalue(b) = p;
            *q = flip_mark_bit_p((Lisp_Object *)&qvalue(b));
            goto try_env;
        }
        term_printf("Failure in GC\n");
        ensure_screen();
        abort();

#ifdef COMMON
case TAG_NUMBERS:
/*
 * If I get back to a NUMBERS than it must have been a ratio or a complex.
 */
        w = imag_part(b);
        if (is_immed(w))
        {   w = real_part(b);
            real_part(b) = p;
            *q = flip_mark_bit_p((Lisp_Object *)&real_part(b));
            q = &numhdr(b);
            p = *q;
            b = flip_mark_bit_p(w);
            goto ascend_from_vector;
        }
        else if (is_marked_p(w))
        {   imag_part(b) = p;
            *q = flip_mark_bit_p((Lisp_Object *)&imag_part(b));
            q = &numhdr(p);
            p = *q;
            b = flip_mark_bit_p(w);
            goto ascend_from_vector;
        }
        else
        {   imag_part(b) = real_part(b);
            real_part(b) = p;
            *q = flip_mark_bit_p((Lisp_Object *)&real_part(b));
            p = w;
            goto descend;
        }
#endif

case TAG_VECTOR:
/*
 * If I get back to a vector it must have been a vector of Lisp_Objects,
 * not a vector of binary data.  My back-pointer points part-way into it.
 * I can tell where I am by inspecting the state of mark bits on both parts
 * of the doubleword so identified.
 */
        w = *(Lisp_Object *)((char *)b - TAG_VECTOR);
#ifndef ADDRESS_64
        if (is_immed(w) || is_marked_p(w))
/*
 * Here I had been marking the pointer that was stored at an odd (word)
 * address.
 */
        {   Lisp_Object w1 = *(Lisp_Object *)((char *)b - TAG_VECTOR + 4);
            *(Lisp_Object *)((char *)b - TAG_VECTOR + 4) = p;
            *q = flip_mark_bit_p((Lisp_Object)((char *)b - TAG_VECTOR + 4));
            if (is_marked_p(w1))     /* End of this vector */
            {   q = (Header *)((char *)b - TAG_VECTOR);
                p = *q;              /* May not be same as w still! */
                b = flip_mark_bit_p(w1);
                goto ascend_from_vector;
            }
            p = w;
            w = w1;
            if (!is_immed(p))
            {   p = flip_mark_bit_p(p);
                if (p != nil)
                {   *(Lisp_Object *)((char *)b - TAG_VECTOR) = w1;
                    goto descend;
                }
            }
            else p = flip_mark_bit_i(p);
            *(Lisp_Object *)((char *)b - TAG_VECTOR) = p;
        }
        else
#endif /* ADDRESS_64 */
        {   *(Lisp_Object *)((char *)b - TAG_VECTOR) = p;
            *q = flip_mark_bit_p((Lisp_Object)((char *)b - TAG_VECTOR));
        }

/*
 * Now the doubleword I returned to has been marked (and tidied up),
 * so I need to scan back towards the header.
 */
        goto scan_vector_more;
    }
}

static void mark(Lisp_Object *pp)
{
/*
 * This mark procedure works by using the regular Lisp stack to
 * store things while traversing the lists.  A null pointer on the
 * stack marks the end of the section that is being used.  If too
 * much stack is (about to be) used I switch over to the pointer-
 * reversing code given above, which is slower but which uses
 * bounded workspace.  My measurements (on just one computer) show the
 * stack-based code only 25% faster than the pointer-reversing version,
 * which HARDLY seems enough to justify all this extra code, but then
 * fast garbage collection is very desirable and every little speed-up
 * will help.
 */
    Lisp_Object p, q, nil = C_nil;
    Lisp_Object *sp = stack, *sl = stacklimit;
    Header h;
    intxx i;
    *++sp = (Lisp_Object)NULL;
top:
/*
 * normally here pp is a pointer to a Lisp_Object and hence an even
 * number - I exploit this so that if I find an odd number stacked I
 * view it as indicating a return into a vector...
 */
    if (((intxx)pp & 1) != 0)
    {   i = ((intxx)pp) - 1;        /* saved value of i */
        p = *sp--;
        goto in_vector;
    }
    p = *pp;
    if (is_immed_or_cons(p))
    {
#ifdef COMMON
        if (!is_cons(p) || p == nil || flip_mark_bit_p(p) == nil)
        {   pp = (Lisp_Object *)(*sp--);
            if (pp == NULL) return;
            else goto top;
        }
#else
        if (!is_cons(p))
        {   pp = (Lisp_Object *)(*sp--);
            if (pp == NULL) return;
            else goto top;
        }
#endif
/*
 * Here, and in analagous places, I have to unset the mark bit - this is
 * because I set the mark bit on a cons cell early (as I need to) then
 * call mark(&car(p)) [in effect], and the effect is that p here sees the
 * marked pointer...
 */
        if (is_marked_p(p)) p = flip_mark_bit_p(p);
        q = qcar(p);
        if (is_immed_or_cons(q) && !is_cons(q))
        {   if (is_marked_i(q))
            {   pp = (Lisp_Object *)(*sp--);
                if (pp == NULL) return;
                else goto top;
            }
            qcar(p) = flip_mark_bit_i(q);
            pp = &qcdr(p);
            goto top;
        }
        else if (is_marked_p(q))
        {   pp = (Lisp_Object *)(*sp--);
            if (pp == NULL) return;
            else goto top;
        }
        else
        {   qcar(p) = flip_mark_bit_p(q);
            q = qcdr(p);
            if (!is_immed(q) && q != nil)
            {   if (sp >= sl) non_recursive_mark(&qcdr(p));
                else *++sp = (Lisp_Object)&qcdr(p);
            }
            pp = &qcar(p);
            goto top;
        }
    }
/* here we have a vector of some sort */
    switch ((int)p & TAG_BITS)
    {
default:            /* The case-list is exhaustive! */
case TAG_CONS:      /* Already processed */
case TAG_FIXNUM:    /* Invalid here */
case TAG_ODDS:      /* Invalid here */
#ifdef COMMON
case TAG_SFLOAT:    /* Invalid here */
#endif
        /* Fatal error really called for here */
        term_printf("\nBad object in GC (%.8lx)\n", (long)p);
        ensure_screen();
        abort();
        return;

case TAG_SYMBOL:
        if (is_marked_p(p)) p = flip_mark_bit_p(p);
#ifndef COMMON
/*
 * NIL is outside the main heap, and so marking it must NOT involve
 * the regular pointer-chaining operations.
 */
        if (p == nil)
        {   pp = (Lisp_Object *)(*sp--);
            if (pp == NULL) return;
            else goto top;
        }
#endif
        h = qheader(p);
        if (!is_odds(h)) /* already visited */
        {   *pp = (Lisp_Object)h;
            qheader(p) = (Header)flip_mark_bit_p((Lisp_Object)pp);
            pp = (Lisp_Object *)(*sp--);
            if (pp == NULL) return;
            else goto top;
        }
#ifdef DAMAGED_SYMBOLS
        term_printf("Symbol at %.8x ", p);
        q = qpname(p);
        if (is_vector(q) && is_odds(vechdr(q)))
        {   term_printf("(%.*s)\n",
                        (int)(length_of_header(vechdr(q))-CELL),
                        &celt(q, 0));
        }
        else term_printf("(pname not available)\n");
        ensure_screen();
#endif
        *pp = flip_mark_bit_i(h);
        qheader(p) = (Header)flip_mark_bit_p((Lisp_Object)pp);
        if (sp >= sl)
        {   non_recursive_mark(&qvalue(p));
            non_recursive_mark(&qenv(p));
            non_recursive_mark(&qpname(p));
#ifdef COMMON
            non_recursive_mark(&qpackage(p));
#endif
        }
        else
        {   q = qvalue(p);
            if (!is_immed(q) && q != nil)
                *++sp = (Lisp_Object)&qvalue(p);
            q = qenv(p);
            if (!is_immed(q) && q != nil)
                *++sp = (Lisp_Object)&qenv(p);
            q = qpname(p);
            if (!is_immed(q) && q != nil)
                *++sp = (Lisp_Object)&qpname(p);
            q = qfastgets(p);
            if (!is_immed(q) && q != nil)
                *++sp = (Lisp_Object)&qfastgets(p);
#ifdef COMMON
            q = qpackage(p);
            if (!is_immed(q) && q != nil)
                *++sp = (Lisp_Object)&qpackage(p);
#endif
        }
        pp = &qplist(p);    /* iterate into plist not value? */
        goto top;

case TAG_NUMBERS:
        if (is_marked_p(p)) p = flip_mark_bit_p(p);
        h = numhdr(p);
        if (!is_odds(h)) /* already visited */
        {   *pp = (Lisp_Object)h;
            numhdr(p) = (Header)flip_mark_bit_p((Lisp_Object)pp);
            pp = (Lisp_Object *)(*sp--);
            if (pp == NULL) return;
            else goto top;
        }
        *pp = flip_mark_bit_i(h);
        numhdr(p) = (Header)flip_mark_bit_p((Lisp_Object)pp);
        if (is_bignum_header(h))
        {   pp = (Lisp_Object *)(*sp--);
            if (pp == NULL) return;
            else goto top;
        }
#ifdef COMMON
        q = real_part(p);
        if (!is_immed(q))
        {   if (sp >= sl) non_recursive_mark(&real_part(p));
            else *++sp = (Lisp_Object)&real_part(p);
        }
        pp = (Lisp_Object *)&imag_part(p);
        goto top;
#else
        term_printf("Bad numeric type found %.8lx\n", (long)h);
        ensure_screen();
        abort();
        return;
#endif

case TAG_BOXFLOAT:
        if (is_marked_p(p)) p = flip_mark_bit_p(p);
        h = flthdr(p);
        if (!is_odds(h)) /* already visited */
        {   *pp = (Lisp_Object)h;
            flthdr(p) = (Header)flip_mark_bit_p((Lisp_Object)pp);
            pp = (Lisp_Object *)(*sp--);
            if (pp == NULL) return;
            else goto top;
        }
        *pp = flip_mark_bit_i(h);
        flthdr(p) = (Header)flip_mark_bit_p((Lisp_Object)pp);
        pp = (Lisp_Object *)(*sp--);
        if (pp == NULL) return;
        else goto top;

case TAG_VECTOR:
        if (is_marked_p(p)) p = flip_mark_bit_p(p);
        h = vechdr(p);
        if (!is_odds(h)) /* already visited */
        {   *pp = (Lisp_Object)h;
            vechdr(p) = (Header)flip_mark_bit_p((Lisp_Object)pp);
            pp = (Lisp_Object *)(*sp--);
            if (pp == NULL) return;
            else goto top;
        }
        *pp = flip_mark_bit_i(h);
        vechdr(p) = (Header)flip_mark_bit_p((Lisp_Object)pp);
        if (vector_holds_binary(h))  /* strings & bitvecs */
        {   pp = (Lisp_Object *)(*sp--);
            if (pp == NULL) return;
            else goto top;
        }
        i = (intxx)doubleword_align_up(length_of_header(h));
        if (is_mixed_header(h)) 
            i = 4*CELL;  /* Only use first few pointers */
in_vector:
        if (sp >= sl)
        {   while (i >= 3*CELL)
            {   i -= CELL;
                non_recursive_mark((Lisp_Object *)((char *)p - TAG_VECTOR + i));
            }
        }
        else
        {   while (i >= 3*CELL)
            {   i -= CELL;
                pp = (Lisp_Object *)((char *)p - TAG_VECTOR + i);
                q = *pp;
                if (!is_immed(q) && q != nil)
                {   *++sp = p;
                    *++sp = i + 1;
#ifdef DAMAGED_SYMBOLS
                    term_printf("Marking item %d of vector %.8x\n", i, p);
                    ensure_screen();
#endif
                    goto top;
                }
            }
        }
/*
 * Because we padded up to an even number of words for the vector in total
 * there are always an odd number of pointers to trace, and in particular
 * always at least one - so it IS reasonable to iterate on the first item in
 * the vector, and there can not be any worries about zero-length vectors
 * to hurt me.  WELL actually in ADDRESS_64 mode I might have had a zero
 * length vector! I should have treated that as if it contained binary..
 */
#ifdef ADDRESS_64
/* /* #  error messed up here */
#endif
        pp = (Lisp_Object *)((char *)p - TAG_VECTOR + i - CELL);
        goto top;
    }
}

#endif /* DEMO_MODE */

Lisp_Object MS_CDECL Lgc0(Lisp_Object nil, int nargs, ...)
{
    argcheck(nargs, 0, "reclaim");
    return Lgc(nil, lisp_true);
}

Lisp_Object Lgc(Lisp_Object nil, Lisp_Object a)
{
/*
 * If GC is called with a non-nil argument the garbage collection
 * will be a full one - otherwise it will be soft and may do hardly
 * anything.
 */
#ifdef DEMO_MODE
    return onevalue(nil);
#else
    return reclaim(nil, "user request",
                   a != nil ? GC_USER_HARD : GC_USER_SOFT, 0);
#endif
}

Lisp_Object Lverbos(Lisp_Object nil, Lisp_Object a)
/*
 * (verbos 0) or (verbos nil)       silent garbage collection
 * (verbos 1) or (verbos t)         standard GC messages
 * (verbos 2)                       messages when FASL module loaded
 * (verbos 4)                       extra timing info for GC process
 * These bits can be added to get combination effects, except that
 * "4" has no effect unless "1" is present.
 */
{
    int32 code, old_code = verbos_flag;
    if (a == nil) code = 0;
    else if (is_fixnum(a)) code = int_of_fixnum(a);
    else code = 1;
/*
 * -G on the command line makes garbage collection noisy always...
 */
    if (miscflags & ALWAYS_NOISY) code |= 3;
    miscflags = (miscflags & ~GC_MSG_BITS) | (code & GC_MSG_BITS);
    return onevalue(fixnum_of_int(old_code));
}


CSLbool volatile already_in_gc, tick_on_gc_exit;
CSLbool volatile interrupt_pending, tick_pending, polltick_pending;
Lisp_Object volatile saveheaplimit;
Lisp_Object volatile savevheaplimit;
Lisp_Object volatile savecodelimit;
Lisp_Object * volatile savestacklimit;


static int stop_after_gc = 0;


static int fold_cons_heap(void)
{
/*
 * This is amazingly messy because the heap is segmented.
 */
    nil_as_base
    int top_page_number = 0,
        bottom_page_number = (int)heap_pages_count - 1;
    void *top_page = heap_pages[top_page_number],
         *bottom_page = heap_pages[bottom_page_number];
    char *top_low = (char *)quadword_align_up((intxx)top_page),
         *bottom_low = (char *)quadword_align_up((intxx)bottom_page);
    char *top_start = top_low + CSL_PAGE_SIZE,
         *bottom_start = bottom_low + CSL_PAGE_SIZE;
    char *top_fringe = top_low + car32(top_low),
         *bottom_fringe = bottom_low + car32(bottom_low);
    if (bottom_fringe != (char *)fringe)
    {   term_printf("disaster wrt heap fringe %.8lx %.8lx\n",
                 (long)bottom_fringe, (long)fringe);
        my_exit(EXIT_FAILURE);
    }
    fringe -= sizeof(Cons_Cell);
    for (;;)
    {
        Lisp_Object p;
/* scan up from fringe to find a busy cell */
        for (;;)
        {   fringe += sizeof(Cons_Cell);
            if (top_page_number == bottom_page_number &&
                top_start == (char *)fringe)
/*
 * The cast to (unsigned) on the next line is unexpectedly delicate.  The
 * difference between two pointers is of type ptrdiff_t, which is a signed
 * type.  If this is implemented as int (and that in turn is a 16 bit value)
 * then the following subtraction can overflow and give a value that appears
 * to have the wrong sign.  The implicit widening to (Lisp_Object) could
 * then propagate the sign bit in an unhelpful manner.  Going via a variable
 * of type (unsigned) ought to mend things. Ok so 16-bit ints are now a
 * thing of that past so this no longer worried me!
 */
            {   unsignedxx len = (unsignedxx)((char *)fringe - top_low);
                car32(top_low) = len;
                return bottom_page_number;
            }
            if ((char *)fringe >= bottom_start)
            {
/*
 * If the heap were to be left totally empty this would be WRONG
 */
                bottom_page = heap_pages[--bottom_page_number];
                bottom_low = (char *)quadword_align_up((intxx)bottom_page);
                bottom_start = bottom_low + CSL_PAGE_SIZE;
                fringe = (Lisp_Object)(bottom_low + car32(bottom_low));
                heaplimit = (Lisp_Object)(bottom_low + SPARE);
                fringe -= sizeof(Cons_Cell);
#ifdef TICK_STREAM
/*
 * From time to time in the GC I poll for clock ticks, just to keep
 * responsiveness of the window system up.
 */
                if (tick_on_gc_exit)
                {   Lisp_Object nil;
                    inject_randomness((int)clock());
                    inject_randomness((int)fringe);
                    accept_tick();
                    nil = C_nil;
                    if (exception_pending())
                    {   stop_after_gc = 1;
                        flip_exception();
                    }
                    tick_on_gc_exit = NO;
                }
#endif
                continue;
            }
            p = qcar(fringe);
            if (is_immed_or_cons(p) && !is_cons(p))
            {   if (is_marked_i(p))
                {   qcar(fringe) = flip_mark_bit_i(p);
                    break;
                }
            }
            else if (is_marked_p(p))
            {   qcar(fringe) = flip_mark_bit_p(p);
                break;
            }
        }
/* scan down from the top to find a free cell, unmarking is I go */
        for (;;)
        {   top_start -= sizeof(Cons_Cell);
            if (top_page_number == bottom_page_number &&
                top_start == (char *)fringe)
            {   unsignedxx len = (unsignedxx)((char *)fringe - top_low);
                car32(top_low) = len;
                return bottom_page_number;
            }
            if (top_start < top_fringe)
            {   top_page_number++;
                top_page = heap_pages[top_page_number];
                top_low = (char *)quadword_align_up((intxx)top_page);
                top_start = top_low + CSL_PAGE_SIZE;
                top_fringe = top_low + car32(top_low);
                continue;
            }
            p = qcar(top_start);
            if (is_immed_or_cons(p) && !is_cons(p))
            {   if (!is_marked_i(p)) break;
                else qcar(top_start) = flip_mark_bit_i(p);
            }
            else if (!is_marked_p(p)) break;
            else qcar(top_start) = flip_mark_bit_p(p);
        }
/* Now relocate one cell */
        qcar(top_start) = qcar(fringe);
        qcdr(top_start) = qcdr(fringe);
        {   Lisp_Object forward = flip_mark_bit_p(top_start + TAG_VECTOR);
            qcar(fringe) = forward;
            qcdr(fringe) = forward;
        }
    }
}

static void adjust_vec_heap(void)
/*
 * This scans over the vector heap working out where each vector
 * is going to get relocated to, and then changing pointers to reflect
 * where the vectors will end up.
 */
{
    Lisp_Object nil = C_nil;
    int32 page_number, new_page_number = 0;
    void *new_page = vheap_pages[0];
    char *new_low = (char *)doubleword_align_up((intxx)new_page);
    char *new_high = new_low + (CSL_PAGE_SIZE - 8);
    char *p = new_low + 8;
    for (page_number = 0; page_number < vheap_pages_count; page_number++)
    {   void *page = vheap_pages[page_number];
        char *low = (char *)doubleword_align_up((intxx)page);
        char *fr = low + car32(low);
#ifdef TICK_STREAM
        if (tick_on_gc_exit)
        {
            inject_randomness((int)clock());
            inject_randomness((int)fr);
            accept_tick();
            tick_on_gc_exit = NO;
        }
#endif
        *(Lisp_Object *)fr = nil;
        low += 8;
        for (;;)
        {   Header h;
            Lisp_Object h1;
            char *p1;
            int32 l;
            unsignedxx free;
/*
 * Here a vector will have an ordinary vector-header (which is tagged
 * as ODDS) if it was not marked.
 */
            while (is_odds(h = *(Header *)low))
            {
                if (is_symbol_header(h)) low += symhdr_length;
                else low += doubleword_align_up(length_of_header(h));
            }
/*
 * It could be that all (remaining) the vectors in this page are unmarked...
 */
            if (low >= fr) break;
/*
 * Otherwise I have found an active vector. Its header should have been
 * left with a back-pointer chain through all places that refereed to the
 * vector.
 */
            h1 = h;
            while (!is_odds(h1))
            {   Lisp_Object h2;
                h2 = *(Lisp_Object *)clear_mark_bit_p(h1);
                if (is_vector(h2))
/*
 * Forwarding pointer for relocated cons cell. This is delicate because
 * of the number of spare bits I have on a 32-bit system. The back-pointer
 * chain via the heaver word of a vector can run through other vector
 * cells (in the middle of vectors) and it can also go via either CAR or CDR
 * field of a cons cell. The funny case here is if it is via the CDR field
 * of a CONS cell and that CONS has been relocated. Then the CONS contains
 * a forwarding address that points to the start of the relocated object.
 * Sometimes I want to end up with a pointer to the CDR bit again. The
 * "+ (h1 & CELL)" is there to achieve that. I somewhat feel that I ought to
 * have been able to do something cleaner, but changing it now seems to me
 * to be too delicate.
 */
                    h1 = (Lisp_Object)((char *)h2 - TAG_VECTOR + (h1 & CELL));
                else h1 = h2;
            }

            if (is_symbol_header(h1)) l = symhdr_length;
            else l = doubleword_align_up(length_of_header(h1));
/*
 * I subtract the pointers (new_high - p) into an unsigned int because
 * on a 16-bit machine that might be vital!  The type ptrdiff_t is a signed
 * type and in bad cases the subtraction might overflow, but I know that the
 * answer here is supposed to be positive.  Hmm I think that these days
 * worry about 16 bit machines is no longer worthwhile...
 */
            free = (unsignedxx)(new_high - p);
            if (l > (intxx)free)
            {   new_page_number++;
                new_page = vheap_pages[new_page_number];
                new_low = (char *)doubleword_align_up((intxx)new_page);
                new_high = new_low + (CSL_PAGE_SIZE - 8);
                p = new_low + 8;
            }

/*
 * Because I did not have enough bits to store the critical information
 * somewhere nicer I have to reconstruct the tag bits to go with the
 * vector out of the header word associated with it.
 * Here is had BETTER be a vector!
 */
            if (is_symbol_header(h1)) p1 = p + TAG_SYMBOL;
            else if (is_numbers_header(h1)) p1 = p + TAG_NUMBERS;
            else if (is_boxfloat_header(h1)) p1 = p + TAG_BOXFLOAT;
            else p1 = p + TAG_VECTOR;

            while (!is_odds(h))
            {   h = clear_mark_bit_p(h);
                h1 = *(Lisp_Object *)h;
/*
 * The two above lines fail if amalgamated - both on Zortech C 3.0.1 and
 * on a VAX/VMS C compiler.  Hence two lines of code where once I had one.
 */
                if (is_vector(h1))
                    h = (Lisp_Object)((char *)h1 - TAG_VECTOR + (h & CELL));
                else
                {   *(Lisp_Object *)h = (Lisp_Object)p1;
                    h = h1;
                }
            }
            *(Lisp_Object *)low = set_mark_bit_h(h);
            low += l;
            p += l;
            if (low >= fr) break;
        }
    }
}

static void move_vec_heap(void)
/*
 * This moves data down in the vector heap, supposing that all pointer
 * relocation will be dealt with elsewhere.  Calculations made here must remain
 * in step with those in adjust_vecheap.
 */
{
    nil_as_base
    int32 page_number, new_page_number = 0;
    void *new_page = vheap_pages[0];
    char *new_low = (char *)doubleword_align_up((intxx)new_page);
    char *new_high = new_low + (CSL_PAGE_SIZE - 8);
    char *p = new_low + 8;
    for (page_number = 0; page_number < vheap_pages_count; page_number++)
    {   void *page = vheap_pages[page_number];
        char *low = (char *)doubleword_align_up((intxx)page);
        char *fr = low + car32(low);
#ifdef TICK_STREAM
        if (tick_on_gc_exit)
        {
            inject_randomness((int)clock());
            inject_randomness((int)fr);
            accept_tick();
            tick_on_gc_exit = NO;
        }
#endif
        *(Lisp_Object *)fr = set_mark_bit_h(TAG_ODDS + (8<<10));
        low += 8;
        for (;;)
        {   Header h;
            intxx l;
            unsignedxx free;
            while (!is_marked_h(h = *(Header *)low))
                if (is_symbol_header(h)) low += symhdr_length;
                else low += doubleword_align_up(length_of_header(h));
            if (low >= fr) break;

            if (is_symbol_header(h)) l = symhdr_length;
            else l = doubleword_align_up(length_of_header(h));
#ifdef DEBUG
            if (l >= CSL_PAGE_SIZE)
            {   term_printf("heap mangled - vector length wrong\n");
                ensure_screen();
                abort();
            }
#endif
            free = (unsignedxx)(new_high - p);
            if (l > (intxx)free)
            {   unsignedxx len = (unsignedxx)(p - new_low);
                car32(new_low) = len;
/*
 * I fill the end of the page with zero words so that the data there is
 * definite in value, and to help file-compression when I dump a heap
 * image.
 */
#ifdef CLEAR_OUT_MEMORY
                while (free != 0)
                {   *(int32 *)p = 0;
                    p += 4;
                    free -= 4;
                }
#endif
                new_page_number++;
                new_page = vheap_pages[new_page_number];
                new_low = (char *)doubleword_align_up((intxx)new_page);
                new_high = new_low + (CSL_PAGE_SIZE - 8);
                p = new_low + 8;
            }

            *(Header *)p = clear_mark_bit_h(h);
            p += CELL;
            low += CELL;
            l -= CELL;
            while (l != 0)
            {   *(int32 *)p = *(int32 *)low;
                p += 4;
                low += 4;
                l -= 4;
            }
        }
    }
    {   unsignedxx len = (unsignedxx)(p - new_low);
#ifdef CLEAR_OUT_MEMORY
        unsignedxx free = (unsignedxx)(new_high - p);
#endif
        car32(new_low) = len;
#ifdef CLEAR_OUT_MEMORY
        while (free != 0)
        {   *(int32 *)p = 0;
            p += 4;
            free -= 4;
        }
#endif
    }
    vfringe = (Lisp_Object)p;
    vheaplimit = (Lisp_Object)(new_low + (CSL_PAGE_SIZE - 8));
    new_page_number++;
    while (vheap_pages_count > new_page_number)
        pages[pages_count++] = vheap_pages[--vheap_pages_count];

}

static int compress_heap(void)
{
    int n = fold_cons_heap();
    adjust_vec_heap();
    move_vec_heap();
    return n;
}

static void relocate(Lisp_Object *cp)
/*
 * If p is a pointer to a cons cell that has been moved, fix it up.
 */
{
    Lisp_Object nil = C_nil,
                p = (*cp);   /* BEWARE "p =* cp;" anachronism here! */
    if (p == nil) return; /* nil is separate from the main heap */
    else if (is_cons(p))
    {   Lisp_Object p1;
        p1 = qcar(p);
        if (is_vector(p1) && is_marked_p(p1))
            *cp = clear_mark_bit_p(p1 - TAG_VECTOR + TAG_CONS);
    }
}

static void relocate_consheap(int bottom_page_number)
{
    int page_number;
    for (page_number = 0; page_number <= bottom_page_number; page_number++)
    {   void *page = heap_pages[page_number];
        char *low = (char *)quadword_align_up((intxx)page);
        char *start = low + CSL_PAGE_SIZE;
        char *fr = low + car32(low);
#ifdef TICK_STREAM
        if (tick_on_gc_exit)
        {
            inject_randomness((int)clock());
            inject_randomness((int)fr);
            accept_tick();
            tick_on_gc_exit = NO;
        }
#endif
        while (fr < start)
        {   relocate((Lisp_Object *)fr);
            fr += sizeof(Lisp_Object);
            cons_cells += sizeof(Lisp_Object);
        }
    }
}

static void relocate_vecheap(void)
{
    int page_number;
    intxx i;
    for (page_number = 0; page_number < vheap_pages_count; page_number++)
    {   void *page = vheap_pages[page_number];
        char *low = (char *)doubleword_align_up((intxx)page);
        char *fr = low + car32(low);
#ifdef TICK_STREAM
        if (tick_on_gc_exit)
        {
            inject_randomness((int)clock());
            inject_randomness((int)fr);
            accept_tick();
            tick_on_gc_exit = NO;
        }
#endif
        low += 8;
        while (low < fr)
        {   Header h = *(Header *)low;
            if (is_symbol_header(h))
            {   Symbol_Head *s = (Symbol_Head *)low;
                relocate(&(s->value));
                relocate(&(s->env));
/*
 * To keep track of literal vectors I suppose here that they are never shared,
 * and I then account for things that are either V or (B . V) in an environment
 * cell, where B is binary code and V is a vector.
 */
                {   Lisp_Object e = s->env;
                    if (is_cons(e) && is_bps(qcar(e))) e = qcdr(e);
                    if (is_vector(e))
                        litvecs += doubleword_align_up(
                                       length_of_header(vechdr(e)));
                }
/*              relocate(&(s->pname));  can never be a cons cell */
                relocate(&(s->plist));
                relocate(&(s->fastgets));
                {   Lisp_Object e = s->fastgets;
                    if (is_vector(e))
                        getvecs += doubleword_align_up(
                                       length_of_header(vechdr(e)));
                }
#ifdef COMMON
                relocate(&(s->package));
#endif
                low += symhdr_length;
                symbol_heads += symhdr_length;
                continue;
            }
            else switch (type_of_header(h))
            {
#ifdef COMMON
        case TYPE_RATNUM:
        case TYPE_COMPLEX_NUM:
                relocate((Lisp_Object *)(low+CELL));
                relocate((Lisp_Object *)(low+2*CELL));
                other_mem += 2*CELL;
                break;
#endif
        case TYPE_MIXED1:
        case TYPE_MIXED2:
        case TYPE_MIXED3:
        case TYPE_STREAM:
                for (i=CELL; i<4*CELL; i+=CELL)
                    relocate((Lisp_Object *)(low+i));
                other_mem += doubleword_align_up(length_of_header(h));
                break;
        case TYPE_HASH:
        case TYPE_SIMPLE_VEC:
        case TYPE_ARRAY:
        case TYPE_STRUCTURE:
                for (i=CELL; i<doubleword_align_up(length_of_header(h)); i+=CELL)
                    relocate((Lisp_Object *)(low+i));
                if (type_of_header(h) == TYPE_SIMPLE_VEC)
                    user_vectors += doubleword_align_up(length_of_header(h));
                else other_mem += doubleword_align_up(length_of_header(h));
                break;
        case TYPE_STRING:
                strings += doubleword_align_up(length_of_header(h));
                break;
        case TYPE_BIGNUM:
                big_numbers += doubleword_align_up(length_of_header(h));
                break;
#ifdef COMMON
        case TYPE_SINGLE_FLOAT:
        case TYPE_LONG_FLOAT:
#endif
        case TYPE_DOUBLE_FLOAT:
                box_floats += doubleword_align_up(length_of_header(h));
                break;

        default:
                break;
            }
            low += doubleword_align_up(length_of_header(h));
        }
    }
}

static void abandon_heap_pages(int bottom_page_number)
{
    bottom_page_number++;
    while (heap_pages_count > bottom_page_number)
        pages[pages_count++] = heap_pages[--heap_pages_count];
}

static void zero_out(void *p)
{
    char *p1 = (char *)doubleword_align_up((intxx)p);
    memset(p1, 0, CSL_PAGE_SIZE);
}

#ifndef NO_COPYING_GC

/*
 * You may like to observe how much more compact the code for the copying
 * garbage collector is when compared with the mark/slide mess.  It is
 * naturally and easily non-recursive and does not get involved in any
 * over-dubious punning on bit-patterns... It just requires a lot of spare
 * memory for the new semi-space.
 */

static int trailing_heap_pages_count,
           trailing_vheap_pages_count;

static void copy(Lisp_Object *p)
/*
 * This copies the object pointed at by p from the old to the new semi-space,
 * and returns a copy to the pointer.  If scans the copied material to copy
 * all relevent sub-structures to the new semi-space.
 */
{
    Lisp_Object nil = C_nil;
    char *fr = (char *)fringe, *vfr = (char *)vfringe;
    char *tr_fr = fr, *tr_vfr = vfr;
    void *p1;
#define CONT           0
#define DONE_CAR      -1
#define DONE_VALUE    -2
#define DONE_ENV      -3
#define DONE_PNAME    -4
#define DONE_PLIST    -5
#define DONE_FASTGETS -6
    int next = CONT;
    char *tr;
#ifdef DEBUG_GC
    term_printf("Copy [%.8lx] %.8lx\n", (long)p, (long)*p);
#endif
/*
 * The code here is a simulation of multiple procedure calls to the
 * code that copies a single object.  What might otherwise have been
 * a "return address" in the calls is handled by the variable "next" which
 * takes positive values while copying vectors, and negative ones in
 * the more common cases. I use "for (;;)" blocks a lot so that I can
 * use "break" and "continue" to leap around in the code - maybe I
 * would do better to be honest and use regular labels and "goto"
 * statements.
 */
    for (;;)
    {
/*
 * Copy one object, pointed at by p, from the old semi-space into the new
 * one.
 */
        Lisp_Object a = *p;
#ifdef DEBUG_GC
    term_printf("Next copy [%.8lx] %.8lx\n", (long)p, (long)*p);
#endif
        for (;;)
        {
            if (a == nil) break;    /* common and cheap enough to test here */
            else if (is_immed_or_cons(a))
            {   if (is_cons(a))
                {
                    Lisp_Object w;
                    w = qcar(a);
                    if (is_cons(w) && is_marked_p(w)) /* a forwarding address */
                    {   *p = flip_mark_bit_p(w);
                        break;
                    }
                    fr = fr - sizeof(Cons_Cell);
                    cons_cells += 2*CELL;
/*
 * When I am doing regular calculation I leave myself a bunch of spare
 * words (size SPARE bytes) so that I can afford to do several cons operations
 * between tests.  Here I do careful tests on every step, and so I can
 * sail much closer to the wind wrt filling up space.
 */
                    if (fr <= (char *)heaplimit - SPARE + 32)
                    {   char *hl = (char *)heaplimit;
                        void *p;
                        unsignedxx len = (unsignedxx)(fr - (hl - SPARE) +
                                                      sizeof(Cons_Cell));
                        car32(hl - SPARE) = len;
                        qcar(fr) = SPID_GCMARK;
                        if (pages_count == 0)
                        {   term_printf("pages_count = 0 in GC\n");
                            ensure_screen();
                            abort();
                            return;
                        }
                        p = pages[--pages_count];
                        zero_out(p);
                        new_heap_pages[new_heap_pages_count++] = p;
                        heaplimit = quadword_align_up((intxx)p);
                        hl = (char *)heaplimit;
                        car32(heaplimit) = CSL_PAGE_SIZE;
                        fr = hl + CSL_PAGE_SIZE - sizeof(Cons_Cell);
                        heaplimit = (Lisp_Object)(hl + SPARE);
                    }
                    qcar(fr) = w;
                    qcdr(fr) = qcdr(a);
                    *p = w = (Lisp_Object)(fr + TAG_CONS);
                    qcar(a) = flip_mark_bit_p(w);
                    break;
                }
                else if (is_bps(a))
                {   char *d = data_of_bps(a) - CELL, *rr;
                    intxx alloc_size;
                    Header h = *(Header *)d;
                    intxx len;
                    if (is_bps(h))  /* Replacement handle in header field? */
                    {   *p = h ;
                        break;
                    }
                    len = length_of_header(h);
                    alloc_size = (intxx)doubleword_align_up(len);
                    bytestreams += alloc_size;
                    for (;;)
                    {   char *cf = (char *)codefringe,
                             *cl = (char *)codelimit;
                        unsignedxx free = (unsignedxx)(cf - cl);
                        if (alloc_size > (intxx)free)
                        {
                            void *p;
                            if (codelimit != 0)
                            {   unsignedxx len = (unsignedxx)(cf - (cl - 8));
                                car32(cl - 8) = len;
                            }
                            if (pages_count == 0)
                            {   term_printf("pages_count = 0 in GC\n");
                                ensure_screen();
                                abort();
                                return;
                            }
                            p = pages[--pages_count];
                            zero_out(p);
                            new_bps_pages[new_bps_pages_count++] = p;
                            cl = (char *)doubleword_align_up((intxx)p);
                            codefringe = (Lisp_Object)(cl + CSL_PAGE_SIZE);
                            codelimit = (Lisp_Object)(cl + 8);
                            continue;
                        }
                        rr = cf - alloc_size;
                        codefringe = (Lisp_Object)rr;
/*
 * See comments in fns2.c for the curious packing here!
 */
                        *(Header *)d = *p = TAG_BPS +
                           (((intxx)((rr + CELL) - (cl - 8)) &
                             (PAGE_POWER_OF_TWO-4)) << 6) +
                           (((intxx)(new_bps_pages_count-1))<<(PAGE_BITS+6));
                        /* Wow! How obscure!! */
                        *(Header *)rr = h;
                        memcpy(rr+CELL, d+CELL, alloc_size-CELL);
                        break;
                    }
                    break;
                }
                else break;        /* Immediate data drops out here */
            }
            else                    /* Here I have a symbol or vector */
            {   Header h;
                int tag;
                intxx len;
                tag = ((int)a) & TAG_BITS;
                a = (Lisp_Object)((char *)a - tag);
                h = *(Header *)a;
#ifdef DEBUG_GC
                term_printf("Header is %.8lx\n", (long)h);
#endif
                if (!is_odds(h))
                {   *p = h;
                    break;
                }
                if (tag == TAG_SYMBOL)
                    len = symhdr_length, symbol_heads += symhdr_length;
                else
                {   len = doubleword_align_up(length_of_header(h));
                    switch (type_of_header(h))
                    {
                case TYPE_STRING:
                        strings += len; break;
                case TYPE_BIGNUM:
                        big_numbers += len; break;
#ifdef COMMON
                case TYPE_SINGLE_FLOAT:
                case TYPE_LONG_FLOAT:
#endif
                case TYPE_DOUBLE_FLOAT:
                        box_floats += len; break;
                case TYPE_SIMPLE_VEC:
                        user_vectors += len; break;
                default:
                        other_mem += len; break;
                    }
                }
                for (;;)
                {   char *vl = (char *)vheaplimit;
                    unsignedxx free = (unsignedxx)(vl - vfr);
                    if (len > (intxx)free)
                    {   unsignedxx free1 =
                            (unsignedxx)(vfr - (vl - (CSL_PAGE_SIZE - 8)));
                        car32(vl - (CSL_PAGE_SIZE - 8)) = free1;
                        qcar(vfr) = 0;          /* sentinel value */
                        if (pages_count == 0)
                        {   term_printf("pages_count = 0 in GC\n");
                            ensure_screen();
                            abort();
                            return;
                        }
                        p1 = pages[--pages_count];
                        zero_out(p1);
                        new_vheap_pages[new_vheap_pages_count++] = p1;
                        vfr = (char *)doubleword_align_up((intxx)p1) + 8;
                        vl = vfr + (CSL_PAGE_SIZE - 16);
                        vheaplimit = (Lisp_Object)vl;
                        free1 = (unsignedxx)(vfr - (vl - (CSL_PAGE_SIZE - 8)));
                        car32(vl - (CSL_PAGE_SIZE - 8)) = free1;
                        continue;
                    }
                    *(Lisp_Object *)a = *p = (Lisp_Object)(vfr + tag);
                    *(Header *)vfr = h;
                    memcpy((char *)vfr+CELL, (char *)a+CELL, len-CELL);
                    vfr += len;
                    break;
                }
                break;
            }
        }
/*
 * Now I have copied one object - the next thing to do is to scan to see
 * if any further items are in the new space, and if so I will copy
 * their offspring.
 */
        for (;;)
        {
            switch (next)
            {
        case CONT:
                if (tr_fr != fr)
                {   tr_fr = tr_fr - sizeof(Cons_Cell);
                    if (qcar(tr_fr) == SPID_GCMARK)
                    {   char *w;
                        p1 = new_heap_pages[trailing_heap_pages_count++];
                        w = (char *)quadword_align_up((intxx)p1);
                        tr_fr = w + (CSL_PAGE_SIZE - sizeof(Cons_Cell));
                    }
                    next = DONE_CAR;
                    p = &qcar(tr_fr);
                    break;              /* Takes me to the outer loop */
                }
                else if (tr_vfr != vfr)
                {   Header h;
                    h = *(Header *)tr_vfr;
                    if (h == 0)
                    {   char *w;
                        p1 = new_vheap_pages[trailing_vheap_pages_count++];
                        w = (char *)doubleword_align_up((intxx)p1);
                        tr_vfr = w + 8;
                        h = *(Header *)tr_vfr;
                    }
                    if (is_symbol_header(h))
                    {   next = DONE_VALUE;   
                        p = &(((Symbol_Head *)tr_vfr)->value);
                        break;
                    }
                    else
                    {   intxx len = doubleword_align_up(length_of_header(h));
                        tr = tr_vfr;
                        tr_vfr = tr_vfr + len;
                        switch (type_of_header(h))
                        {
#ifdef COMMON
                    case TYPE_SINGLE_FLOAT:
                    case TYPE_LONG_FLOAT:
#endif
                    case TYPE_DOUBLE_FLOAT:
                    case TYPE_BIGNUM:
                            continue;
                    case TYPE_MIXED1: case TYPE_MIXED2:
                    case TYPE_MIXED3: case TYPE_STREAM:
                            next = 2*CELL;
                            break;
/*
 * There is a slight delight here. The test "vector_holds_binary" is only
 * applicable if the header to be checked is a header of a genuine vector,
 * ie something that would have TAG_VECTOR in the pointer to it. But here
 * various numeric data types also live in the vector heap, so I need to
 * separate them out explicitly. The switch block here does slightly more than
 * it actually HAS to, since the vector_holds_binary test would happen to
 * deal with several of the numeric types "by accident", but I feel that
 * the security of listing them as separate cases is more important than the
 * minor speed-up that might come from exploiting such marginal behaviour.
 */
                    default:
                            if (vector_holds_binary(h)) continue;
#ifdef COMMON
                    case TYPE_RATNUM:
                    case TYPE_COMPLEX_NUM:
#endif
                            next = len - 2*CELL;
                            break;
                        }
                        p = (Lisp_Object *)(tr + next + CELL);
                        break;
                    }
                }
                else
                {   fringe = (Lisp_Object)fr;
                    vfringe = (Lisp_Object)vfr;
                    return;        /* Final exit when all has been copied */
                }
        case DONE_CAR:
                next = CONT;
                p = &qcdr(tr_fr);
                break;
        case DONE_VALUE:
                next = DONE_ENV;   
                p = &(((Symbol_Head *)tr_vfr)->env);
                break;
        case DONE_ENV:
                next = DONE_FASTGETS;   
                p = &(((Symbol_Head *)tr_vfr)->fastgets);
                break;
        case DONE_FASTGETS:
                next = DONE_PNAME;   
                p = &(((Symbol_Head *)tr_vfr)->pname);
                break;
        case DONE_PNAME:
#ifndef COMMON
                next = CONT;
                p = &(((Symbol_Head *)tr_vfr)->plist);
                tr_vfr = tr_vfr + symhdr_length;
                break;
#else
                next = DONE_PLIST;   
                p = &(((Symbol_Head *)tr_vfr)->plist);
                break;
        case DONE_PLIST:
                next = CONT;
                p = &(((Symbol_Head *)tr_vfr)->package);
                tr_vfr = tr_vfr + symhdr_length;
                break;
#endif
        default:
                p = (Lisp_Object *)(tr + next);
                next -= CELL;
                break;
            }
            break;
        }
    }
}

#endif  /* NO_COPYING_GC */


#ifndef DEMO_MODE

typedef struct mapstore_item
{
    double w;
    double n;
    unsigned32 n1;
    Lisp_Object p;
} mapstore_item;

int profile_count_mode;

static int MS_CDECL profile_cf(const void *a, const void *b)
{
    mapstore_item *aa = (mapstore_item *)a,
                  *bb = (mapstore_item *)b;
    
    if (profile_count_mode)
    {   if (aa->n1 == bb->n1) return 0;
	if (aa->n1 < bb->n1) return 1;
	else return -1;
    }
    if (aa->w == bb->w) return 0;
    else if (aa->w < bb->w) return 1;
    else return -1;
}

#endif

Lisp_Object Lmapstore(Lisp_Object nil, Lisp_Object a)
/*
 * Argument controls what happens:
 *    nil or 0      print statistics and reset to zero
 *           1      print, but do not reset
 *           2      return list of stats, reset to zero
 *           3      return list, do not reset
 *           4      reset to zero, do not print, return nil
 *	     8	    Toggle call count mode
 */
{
#ifdef DEMO_MODE
    return onevalue(nil);
#else
    int pass, what;
    int32 j, gcn;
    double itotal = 0.0, total = 0.0;
    Lisp_Object res = nil;
    mapstore_item *buff;
    int32 buffp, buffn;
    if (a == nil) a = fixnum_of_int(0);
    if (is_fixnum(a)) what = int_of_fixnum(a);
    else what = 0;
    if ((what & 6) == 0)
    {   buff = (mapstore_item *)(*malloc_hook)(100*sizeof(mapstore_item));
        if (buff == NULL) return onevalue(nil); /* fail */
        buffp = 0;
        buffn = 100;
    }
    if ((what & 2) != 0)
    {   Lgc0(nil, 0); /* Force GC at start to avoid one in the middle */
        nil = C_nil;
        if (exception_pending()) return nil;
        gcn = gc_number;
    }

    if ((what & 8) != 0)
	profile_count_mode = !profile_count_mode;
#ifdef PROFILED
/*
 * PROFILED is intended to be defined if we were compiled with a -p option,
 * and we take system dependent action to dump out results (e.g. on some systems
 * it may be useful to invoke monitor() or moncontrol() here.
 */

#ifdef SHOW_COUNTS_AVAILABLE
    show_counts();
    write_profile("counts");   /* Useful if -px option to compiler */
#endif

#endif /* PROFILED */
    {   char *vf = (char *)vfringe,
             *vl = (char *)vheaplimit;
        unsignedxx len = (unsignedxx)(vf - (vl - (CSL_PAGE_SIZE - 8)));
/*
 * Set up the current page so I can tell where the active data is.
 */
        car32(vl - (CSL_PAGE_SIZE - 8)) = len;
    }
    for (pass=0; pass<2; pass++)
    {   for (j=0; j<vheap_pages_count; j++)
        {   void *page = vheap_pages[j];
            char *low = (char *)doubleword_align_up((intxx)page);
            char *high = low + car32(low);
            low += 8;
            while (low<high)
            {   Header h = *(Header *)low;
                if (is_symbol_header(h))
                {   Lisp_Object e = qenv(low + TAG_SYMBOL);
                    intxx clen = 0;
                    unsignedxx n;
                    if (is_cons(e))
                    {   e = qcar(e);
                        if (is_bps(e))
                        {   Header ch = *(Header *)(data_of_bps(e) - CELL);
                            clen = length_of_header(ch);
                        }
                    }
                    n = qcount(low + TAG_SYMBOL);
                    if (n != 0 && clen != 0)
                    {   double w = (double)n/(double)clen;
/*
 * Here I want a measure that will give a good idea of how worthwhile it
 * would be to compile the given function into C - what I have chosen is
 * a count of bytecodes executed scaled by the length
 * of the bytestream code defining the function.  This will cause "good value"
 * cases to show up best.  I scale this relative to the total across all
 * functions recorded to make the numbers less sensitive to details of
 * how I generate test cases.  For interest I also display the proportion
 * of actual bytecodes interpreted.  In each case I record these out of
 * a total of 100.0 (percent) to give comfortable ranges of numbers to admire.
 */
                        if (pass == 0) itotal += (double)n, total += w;
                        else
                        {   if (w/total > 0.00001 ||
                                (double)n/itotal > 0.0001)
                            {   if ((what & 6) == 0)
                                {   if (buffp == buffn)
                                    {   buffn += 100;
                                        buff = (mapstore_item *)
                                            (*realloc_hook)((void *)buff,
                                                    sizeof(mapstore_item)*buffn);
                                        if (buff == NULL) return onevalue(nil);
                                    }
                                    buff[buffp].w = 100.0*w/total;
                                    buff[buffp].n = 100.0*(double)n/itotal;
                                    buff[buffp].n1 = n;
                                    buff[buffp].p = (Lisp_Object)(low + TAG_SYMBOL);
                                    buffp++;
                                }
                                if ((what & 2) != 0)
                                {   Lisp_Object w1;
/* Result is a list of items ((name size bytes-executed) ...).
 * You might think that I needed to push res here - but I abort if there
 * is a GC, so it is not necessary after all.
 */
                                    w1 = list3((Lisp_Object)(low + TAG_SYMBOL),
                                              fixnum_of_int(clen),
                                              fixnum_of_int(n));
                                    nil = C_nil;
                                    if (exception_pending() || gcn != gc_number)
                                        return nil;
                                    res = cons(w1, res);
                                    nil = C_nil;
                                    if (exception_pending() || gcn != gc_number)
                                        return nil;
                                }
                            }
/*
 * Reset count unless 1 bit of arg is set
 */
                            if ((what & 1) == 0)
                                qcount(low + TAG_SYMBOL) = 0;
                        }
                    }
                    low += symhdr_length;
                }
                else low += (intxx)doubleword_align_up(length_of_header(h));
            }
        }
    }
    if ((what & 6) == 0)
    {   double running = 0.0;
        qsort((void *)buff, buffp, sizeof(buff[0]), profile_cf);
        trace_printf("\n  Value  %%bytes (So far) Bytecodes  Function name\n");
        for (j=0; j<buffp; j++)
        {   running += buff[j].n;
            trace_printf("%7.2f %7.2f (%6.2f) %9lu: ",
                buff[j].w, buff[j].n, running, (long unsigned)buff[j].n1);
            prin_to_trace(buff[j].p);
            trace_printf("\n");
        }
        trace_printf("\n");
        (*free_hook)((void *)buff);
    }
    return onevalue(res);
#endif /* DEMO_MODE */
}

Lisp_Object MS_CDECL Lmapstore0(Lisp_Object nil, int nargs, ...)
{
    argcheck(nargs, 0, "mapstore");
    return Lmapstore(nil, nil);
}

static CSLbool reset_limit_registers(intxx vheap_need,
                                     intxx bps_need,
                                     intxx native_need)
/*
 * returns YES if after resetting the limit registers there was
 * enough space left for me to proceed. Return NO on failure, ie
 * need for a more genuine GC.
 */
{
    void *p;
    nil_as_base
    unsignedxx len;
    CSLbool full;
#ifndef NO_COPYING_GC
    if (gc_method_is_copying)
        full = (pages_count <=
                 heap_pages_count + vheap_pages_count +
                 bps_pages_count + native_pages_count);
    else
#endif
        full = (pages_count == 0);
    if (fringe <= heaplimit)
    {   if (full) return NO;
        p = pages[--pages_count];
        zero_out(p);
        heap_pages[heap_pages_count++] = p;
        heaplimit = quadword_align_up((intxx)p);
        car32(heaplimit) = CSL_PAGE_SIZE;
        fringe = (Lisp_Object)((char *)heaplimit + CSL_PAGE_SIZE);
        heaplimit = (Lisp_Object)((char *)heaplimit + SPARE);
    }
    {   char *vh = (char *)vheaplimit,
             *vf = (char *)vfringe;
        len = (unsignedxx)(vh - vf);
    }
    if (vheap_need > (intxx)len)
    {   char *vf, *vh;
        if (full) return NO;
        p = pages[--pages_count];
        zero_out(p);
        vheap_pages[vheap_pages_count++] = p;
        vf = (char *)doubleword_align_up((intxx)p) + 8;
        vfringe = (Lisp_Object)vf;
        vh = vf + (CSL_PAGE_SIZE - 16);
        vheaplimit = (Lisp_Object)vh;
        len = (unsignedxx)(vf - (vh - (CSL_PAGE_SIZE - 8)));
        car32(vh - (CSL_PAGE_SIZE - 8)) = len;
    }
    {   char *cl = (char *)codelimit,
             *cf = (char *)codefringe;
        len = (unsignedxx)(cf - cl);
    }
    if (bps_need != 0 && bps_need >= (intxx)len)
    {   char *cl;
        if (full || bps_pages_count >= MAX_BPS_PAGES - 1) return NO;
        p = pages[--pages_count];
        zero_out(p);
        bps_pages[bps_pages_count++] = p;
        cl = (char *)doubleword_align_up((intxx)p);
        codefringe = (Lisp_Object)(cl + CSL_PAGE_SIZE);
        codelimit = (Lisp_Object)(cl + 8);
    }
    if (native_need != 0)
    {   if (full || native_pages_count >= MAX_NATIVE_PAGES - 1) return NO;
        p = pages[--pages_count];
        zero_out(p);
        native_pages[native_pages_count++] = p;
        native_fringe = 8;
    }
    return (stack < stacklimit);
}

static void tidy_fringes(void)
/*
 * heaplimit was SPARE bytes above the actual base of the page,
 * so the next line dumps fringe somewhere where it can be found
 * later on when needed while scanning a page of heap.  Similarly
 * vfringe is stashed away at the end of its page.
 */
{   nil_as_base
    char *fr = (char *)fringe,
         *vf = (char *)vfringe,
         *cf = (char *)codefringe,
         *hl = (char *)heaplimit,
         *vl = (char *)vheaplimit,
         *cl = (char *)codelimit;
    unsignedxx len = (unsignedxx)(fr - (hl - SPARE));
    car32(hl - SPARE) = len;
    len = (unsignedxx)(vf - (vl - (CSL_PAGE_SIZE - 8)));
    car32(vl - (CSL_PAGE_SIZE - 8)) = (Lisp_Object)len;
    if (codelimit != 0)
    {   len = (unsignedxx)(cf - (cl - 8));
        car32(cl - 8) = len;
    }
}

static void lose_dead_hashtables(void)
/*
 * This splices out from the list of hash tables all entries that point to
 * tables that have not been marked or copied this garbage collection.
 */
{
    Lisp_Object *p = &eq_hash_tables, q, r;
    while ((q = *p) != C_nil)
    {   Header h;
        r = qcar(q);
        h = vechdr(r);
        if (is_odds(h) && !is_marked_h(h)) *p = qcdr(q);
        else p = &qcdr(q);
    }
    p = &equal_hash_tables;
    while ((q = *p) != C_nil)
    {   Header h;
        r = qcar(q);
        h = vechdr(r);
        if (is_odds(h) && !is_marked_h(h)) *p = qcdr(q);
        else p = &qcdr(q);
    }
}

#ifdef DEMO_MODE

extern CSLbool terminal_pushed;

void give_up()
{
    Lisp_Object nil;
#define m(s) err_printf(s)
m("\n+++ DEMONSTRATION VERSION OF REDUCE - RESOURCE LIMIT EXCEEDED +++\n");
m("This version of REDUCE has been provided for testing and\n");
m("demonstration purposes.  It has a built-in cut-out that will\n");
m("terminate processing after a time that should be sufficient for\n");
m("various small tests to run, but which will probably stop it\n");
m("from being useful as a serious tool.  You are permitted to copy\n");
m("the demonstration version and pass it on to friends subject to\n");
m("not changing it, and in particular neither changing the various\n");
m("messages it prints nor attempting to circumvent the time-out\n");
m("mechanism.  Full versions of REDUCE are available to run on a\n");
m("wide range of types of computer, and a machine-readable file\n");
m("listing suppliers was provided with the documentation that goes\n");
m("with this version.  Some suppliers are:\n");
m("  Codemist Ltd, Alta, Horsecombe Vale, Combe Down, Bath BA2 5QR,\n");
m("    England.  Phone and fax +44-225-837430,\n");
m("    http://www.codemist.co.uk\n");
m("  Winfried Neun, Konrad-Zuse-Zentrum fuer Informationstechnik Berlin\n");
m("    Heilbronner Str. 10, D 10711 Berlin-Wilmersdorf, GERMANY\n");
m("    Phone: +44-30-89604-195  Fax +49-30-89604-125.\n");
m("  (Codemist provided this version, the ZIB differs slightly)\n");
m("<Close window/type RETURN to exit>\n");
#undef m
    nil = C_nil;
    prompt_thing = CHAR_EOF;    /* Disables the prompt */
    ensure_screen();
    terminal_pushed = NOT_CHAR;
    tty_count = 0;
    char_from_terminal(0);
    my_exit(EXIT_FAILURE);
}

#endif

Lisp_Object reclaim(Lisp_Object p, char *why, int stg_class, intxx size)
{
    intxx i;
    clock_t t0, t1, t2, t3;
    int bottom_page_number;
    Lisp_Object *sp, nil = C_nil;
    intxx vheap_need = 0, bps_need = 0, native_need = 0;
#if defined(FLEX) && defined(UNIX)
    extern int timnag_();
    timnag_();
#endif
    stop_after_gc = 0;
    if (stg_class == GC_VEC) vheap_need = size;
    else if (stg_class == GC_BPS) bps_need = size;
    else if (stg_class == GC_NATIVE) native_need = size;
    already_in_gc = YES;
#ifdef SOCKETS
    if (socket_server != 0)
    {   time_t tt0 = time(NULL);
        t0 = clock();
        tt0 = time(NULL);
        if (t0 > cpu_timeout ||
            tt0 > elapsed_timeout)
        {   cpu_timeout = t0 + 20;
            elapsed_timeout = tt0 + 20;
            term_printf("\nSorry: timeout on this session. Closing down\n");
            return Lstop(nil, fixnum_of_int(1));
        }
    }
#endif

    push_clock(); t0 = base_time;

#ifdef TICK_STREAM
    if (tick_pending != 0)
    {   tick_pending = 0;
        heaplimit = saveheaplimit;
        vheaplimit = savevheaplimit;
        codelimit = savecodelimit;
        stacklimit = savestacklimit;
        tidy_fringes();
        if (stg_class != GC_PRESERVE &&
             stg_class != GC_USER_HARD &&
             reset_limit_registers(vheap_need, bps_need, native_need))
        {   already_in_gc = NO;
            tick_on_gc_exit = NO;
#ifdef POLL_FOR_ATTN
            poll_for_attn();
            nil = C_nil;
            if (exception_pending())
            {   pop_clock();
                return nil;
            }
#endif
            if (interrupt_pending)
            {   interrupt_pending = NO;
                pop_clock();
                return interrupted(p);
            }
            else
            {
                inject_randomness((int)clock());
                accept_tick();
                pop_clock();
                return onevalue(p);
            }
        }
        else tick_on_gc_exit = YES;
    }
    else
#else
	if (interrupt_pending)
	{   if (tick_pending)
            {   tick_pending = 0;
                heaplimit = saveheaplimit;
	        vheaplimit = savevheaplimit;
	        codelimit = savecodelimit;
	        stacklimit = savestacklimit;
            }
	    tidy_fringes();
	    interrupt_pending = NO;
	    pop_clock();
	    return interrupted(p);
	}
#endif
    {   tidy_fringes();
        if (stg_class != GC_PRESERVE &&
            stg_class != GC_USER_HARD &&
            reset_limit_registers(vheap_need, bps_need, native_need))
        {   already_in_gc = NO;
            pop_clock();
            return onevalue(p); /* Soft GC */
        }
    }

    if (stack >= stacklimit)
    {   stacklimit = &stacklimit[50];  /* Allow a bit of slack */
        pop_clock();
        return error(0, err_stack_overflow);
    }

#ifdef MEMORY_TRACE
#ifndef CHECK_ONLY
    identify_page_types();
    memory_comment(4);
#endif
#endif

#ifdef DEMO_MODE
    give_up();
    pop_clock();
    return nil;
#else
    push(p);

    gc_number++;

#ifdef WINDOW_SYSTEM
/*
 * If I have a window system I tell it the current time every so often
 * just to keep things cheery...
 */
    {   long int t = (long int)(100.0 * consolidated_time[0]);
        long int gct = (long int)(100.0 * gc_time);
/* /*
 * I guess that I want garbage collection messages, if any, to
 * be sent to stderr rather than whatever output stream happens to
 * be selected at the time of the garbage collection?
 * At present messages go to the normal output stream, which only makes
 * sense if GC messages are almost always disabled - maybe that will
 * be the case!
 */
        report_time(t, gct);
        if (verbos_flag & 1)
        {   freshline_trace();
            trace_printf(
        "+++ Garbage collection %ld (%s) after %ld.%.2ld+%ld.%.2ld seconds\n",
                 (long)gc_number, why, t/100, t%100, gct/100, gct%100);
        }
    }
#else
    if (verbos_flag & 1)
    {   long int t = (long int)(100.0 * consolidated_time[0]);
        long int gct = (long int)(100.0 * gc_time);
/* /* I guess that I want garbage collection messages, if any, to
 * be sent to stderr rather than whatever output stream happens to
 * be selected at the time of the garbage collection?
 * At present messages go to the normal output stream, which only makes
 * sense if GC messages are almost always disabled - maybe that will
 * be the case!
 */
        freshline_trace();
        trace_printf(
         "+++ Garbage collection %ld (%s) after %ld.%.2ld+%ld.%.2ld seconds\n",
         (long)gc_number, why, t/100, t%100, gct/100, gct%100);
    }
#endif
/*
 * If things crash really badly maybe I would rather have my output up
 * to date.
 */
    ensure_screen();
    nil = C_nil;
    if (exception_pending())
    {   stop_after_gc = 1;
        flip_exception();
    }
    if (spool_file != NULL) fflush(spool_file);

    copy_into_nilseg(NO);

    cons_cells = symbol_heads = strings = user_vectors =
             big_numbers = box_floats = bytestreams = other_mem =
             litvecs = getvecs = 0;

#ifndef NO_COPYING_GC
    if (gc_method_is_copying)
    {
        t2 = t1 = t0;   /* Time is not split down in this case */
/*
 * Set up the new half-space initially empty.
 */
        new_heap_pages_count = 0;
        new_vheap_pages_count = 0;
        new_bps_pages_count = 0;
        trailing_heap_pages_count = 1;
        trailing_vheap_pages_count = 1;
        {   void *pp = pages[--pages_count];
            char *vf, *vl;
            unsignedxx len;
/*
 * A first page of (cons-)heap
 */
            zero_out(pp);
            new_heap_pages[new_heap_pages_count++] = pp;
            heaplimit = quadword_align_up((intxx)pp);
            car32(heaplimit) = CSL_PAGE_SIZE;
            vl = (char *)heaplimit;
            fringe = (Lisp_Object)(vl + CSL_PAGE_SIZE);
            heaplimit = (Lisp_Object)(vl + SPARE);
#ifdef DEBUG_GC
            term_printf("fr = %.8lx, hl = %.8lx\n", (long)fringe, (long)heaplimit);
#endif
/*
 * A first page of vector heap.
 */
            pp = pages[--pages_count];
            zero_out(pp);
            new_vheap_pages[new_vheap_pages_count++] = pp;
            vf = (char *)doubleword_align_up((intxx)pp) + 8;
            vfringe = (Lisp_Object)vf;
            vl = vf + (CSL_PAGE_SIZE - 16);
            vheaplimit = (Lisp_Object)vl;
            len = (unsignedxx)(vf - (vl - (CSL_PAGE_SIZE - 8)));
            car32(vl - (CSL_PAGE_SIZE - 8)) = (Lisp_Object)len;
/*
 * The BPS heap can start of utterly non-existent.
 */
            codefringe = codelimit = 0;
        }

/*
 * The very first thing that I will copy will be the main object-vector,
 * this is done early to ensure that it gets a full empty page of vector
 * heap to fit into.
 */
        copy(&BASE[current_package_offset]);
/*
 * The above line is "really"
 *          copy(&current_package);
 * but I use an offset into the nilseg in explicit for because otherwise
 * there is a big foul-up with the NILSEG_EXTERNS option...  Sorry!
 */

/*
 * I should remind you, gentle reader, that the value cell
 * and env cells of nil will always contain nil, which does not move,
 * and so I do not need to copy them here.
 */
        copy(&(qplist(nil)));
        copy(&(qpname(nil)));
        copy(&(qfastgets(nil)));
#ifdef COMMON
        copy(&(qpackage(nil)));
#endif

/*
 * I dislike the special treatment of current_package that follows. Maybe
 * I should arrange something totally different for copying the package
 * structure...
 */
        for (i = first_nil_offset; i<last_nil_offset; i++)
            if (i != current_package_offset)
            /* current-package - already copied by hand */
                copy(&BASE[i]);

        for (sp=stack; sp>(Lisp_Object *)stackbase; sp--) copy(sp);
/*
 * Now I need to perform some magic on the list of hash tables...
 */
        lose_dead_hashtables();
        copy(&eq_hash_tables);
        copy(&equal_hash_tables);

        tidy_fringes();

/*
 * Throw away the old semi-space - it is now junk.
 */
        while (heap_pages_count!=0)
            pages[pages_count++] = heap_pages[--heap_pages_count];
        while (vheap_pages_count!=0)
            pages[pages_count++] = vheap_pages[--vheap_pages_count];
        while (bps_pages_count!=0)
            pages[pages_count++] = bps_pages[--bps_pages_count];

/*
 * Flip the descriptors for the old and new semi-spaces.
 */
        {   void **w = heap_pages;
            heap_pages = new_heap_pages;
            new_heap_pages = w;
            w = vheap_pages;
            vheap_pages = new_vheap_pages;
            new_vheap_pages = w;
            w = bps_pages;
            bps_pages = new_bps_pages;
            new_bps_pages = w;
            heap_pages_count = new_heap_pages_count;
            new_heap_pages_count = 0;
            vheap_pages_count = new_vheap_pages_count;
            new_vheap_pages_count = 0;
            bps_pages_count = new_bps_pages_count;
            new_bps_pages_count = 0;
        }
    }
    else
#endif /* NO_COPYING_GC */
    {
/*
 * The list bases to mark from are
 * (a) nil    [NB: mark(nil) would be ineffective],
 * (b) the special ones addressed relative to nil,
 * (c) everything on the Lisp stack,
 * (d) the package structure,
 * (e) the argument (p) passed to reclaim().
 */
        qheader(nil) = set_mark_bit_h(qheader(nil));
                                    /* nil has nil as value & env   ...  */
        mark(&qplist(nil));         /* ... thus only its plist and ...   */
        mark(&qpname(nil));         /* ... pname cell need marking,      */
                                    /* ... since packages are done later */
        mark(&qfastgets(nil));      /* + the fastgets vector, if any     */

        for (i = first_nil_offset; i<last_nil_offset; i++)
        {
            mark(&BASE[i]);
#ifdef TICK_STREAM
            if (tick_on_gc_exit)
            {
                inject_randomness((int)clock());
                accept_tick();
                tick_on_gc_exit = NO;
            }
#endif
        }
        for (sp=stack; sp>(Lisp_Object *)stackbase; sp--)
        {
            mark(sp);
#ifdef TICK_STREAM
            if (tick_on_gc_exit)
            {
                inject_randomness((int)clock());
                accept_tick();
                tick_on_gc_exit = NO;
            }
#endif
        }
/*
 * Now I need to perform some magic on the list of hash tables...
 */
        lose_dead_hashtables();
        mark(&eq_hash_tables);
        mark(&equal_hash_tables);
/*
 * What about the package structure... ? I assume it has been marked by
 * what I have just done.
 */
        qheader(nil) = clear_mark_bit_h(qheader(nil));
        t1 = read_clock();

        bottom_page_number = compress_heap();    /* Folds cons cells upwards */

        t2 = read_clock();

/*
 * Again I should remind you, gentle reader, that the value cell
 * and env cells of nil will always contain nil, which does not move,
 * and so I do not need to relocate them here.
 */
        relocate(&(qplist(nil)));
/*      relocate(&(qpname(nil)));   never a cons cell */
        relocate(&(qfastgets(nil)));
#ifdef COMMON
        relocate(&(qpackage(nil)));
#endif

        for (i = first_nil_offset; i<last_nil_offset; i++)
            relocate(&BASE[i]);

        for (sp=stack; sp>(Lisp_Object *)stackbase; sp--) relocate(sp);

        relocate_consheap(bottom_page_number);
        relocate(&eq_hash_tables);
        relocate(&equal_hash_tables);
        relocate_vecheap();

        {   char *fr = (char *)fringe,
                 *vf = (char *)vfringe,
                 *cf = (char *)codefringe,
                 *hl = (char *)heaplimit,
                 *vl = (char *)vheaplimit,
                 *cl = (char *)codelimit;
            unsignedxx len = (unsignedxx)(fr - (hl - SPARE));
            car32(hl - SPARE) = len;
            len = (unsignedxx)(vf - (vl - (CSL_PAGE_SIZE - 8)));
            car32(vl - (CSL_PAGE_SIZE - 8)) = len;
            if (codelimit != 0)
            {   len = (unsignedxx)(cf - (cl - 8));
                car32(cl - 8) = len;
            }
        }

        abandon_heap_pages(bottom_page_number);
    }

    {   Lisp_Object qq;
/*
 * Note that EQUAL hash tables do not need to be rehashed here, though
 * they do if a heap image is exported from one system to another.
 */
        for (qq = eq_hash_tables; qq!=nil; qq=qcdr(qq))
            rehash_this_table(qcar(qq));
    }

    gc_time += pop_clock();
    t3 = base_time;

    copy_out_of_nilseg(NO);

    if ((verbos_flag & 5) == 5)
/*
 * (verbos 4) gets the system to tell me how long each phase of GC took,
 * but (verbos 1) must be ORd in too.
 */
    {
#ifndef NO_COPYING_GC
    if (gc_method_is_copying)
      trace_printf("Copy %ld ms\n",
        (long int)(1000.0 * (double)(t3-t0)/(double)CLOCKS_PER_SEC));
    else
#endif
      trace_printf("Mark %ld, compact %ld, relocate %ld ms\n",
        (long int)(1000.0 * (double)(t1-t0)/(double)CLOCKS_PER_SEC),
        (long int)(1000.0 * (double)(t2-t1)/(double)CLOCKS_PER_SEC),
        (long int)(1000.0 * (double)(t3-t2)/(double)CLOCKS_PER_SEC));
    }

/* (verbos 5) causes a display breaking down how space is used */
    if ((verbos_flag & 5) == 5)
    {   trace_printf(
            "cons_cells=%d, symbol_heads=%d, strings=%d, user_vectors=%d\n",
             cons_cells, symbol_heads, strings, user_vectors-litvecs-getvecs);
        trace_printf(
            "bignums=%d, floats=%d, bytestreams=%d, other=%d, litvecs=%d\n",
            big_numbers, box_floats, bytestreams, other_mem, litvecs);
        trace_printf("getvecs=%d\n", getvecs);
    }

    pop(p);

    if (!reset_limit_registers(vheap_need, bps_need, native_need))
    {   term_printf("\n+++ No space left at all\n");
        my_exit(EXIT_FAILURE);    /* totally drastic... */
    }
#ifndef HOLD_BACK_MEMORY
#ifndef MEMORY_TRACE
/*
 * Here I grab more memory (if I am allowed to).
 * An initial version here, and one still suitable on machines that will
 * have plenty of real memory, will be to defined ok_to_grab_memory(n) as
 * 3*n + 2. This expands until the proportion of the heap active at the
 * end of garbage collection is less than 1/4.
 * If the attempt to grab more memory fails I clear the bit in init_flags
 * that allows me to try to expand, so I will not waste time again.  If
 * HOLD_BACK_MEMORY was asserted (for machines where grabbing all seemingly
 * available memory may cause a crash) I do not try this operation.  The
 * aim of keeping the heap less than half full is an heuristic and could be
 * adjusted on the basis of experience with this code.
 * The "+2" at the end of calculating the ideal heap size is intended
 * to keep us (mostly) in the copying GC domain.  If it is omitted the
 * heap tends to stay just 25% full and sliding GC is used. Overall this is
 * roughly as expensive as copying, but it is more disruptive since it comes
 * in larger gulps.
 * On systems where it is possible to measure the amount of available
 * real memory more sophisticated calculations may be possible.
 */
    if (init_flags & INIT_EXPANDABLE)
    {   int32 ideal = ok_to_grab_memory(heap_pages_count +
                                        vheap_pages_count +
                                        bps_pages_count);
        int32 more;
        if (ideal > MAX_PAGES) ideal = MAX_PAGES;
        more = ideal - pages_count;
        while (more-- > 0)
        {   void *page = (void *)my_malloc((size_t)(CSL_PAGE_SIZE + 8));
            intxx pun, pun1;
/*
 * When I first grabbed memory in restart.c I used my_malloc_1(), which
 * gobbles a large stack frame and then called regular malloc - the idea
 * there was to avoid malloc grabbing space needed for the stack. I can
 * not properly do that here since reclaim() may be called with a deep
 * stack already active.  There is thus a danger that expanding the heap here
 * may cause me to run out of stack elsewhere.  Oh well, I guess I can not
 * win in all ways.
 */
/*
 * Verify that new block does not span zero & has correct sign bit
 */
            pun = (intxx)page;
            pun1 = (intxx)((char *)page + CSL_PAGE_SIZE + 8);
            if ((pun ^ pun1) < 0) page = NULL;
#ifdef ADDRESS_SIGN_UNKNOWN
            if ((pun + address_sign) < 0) page = NULL;
#else
#ifdef ADDRESSES_HAVE_TOP_BIT_SET
            if (pun > 0) page = NULL;
#else
            if (pun < 0) page = NULL;
#endif
#endif
            if (page == NULL)
            {   init_flags &= ~INIT_EXPANDABLE;
                break;
            }
            else pages[pages_count++] = page;
        }
    }
#endif /* MEMORY_TRACE */
#endif /* HOLD_BACK_MEMORY */
#ifdef WINDOW_SYSTEM
    {   int32 n = heap_pages_count + vheap_pages_count + bps_pages_count;
        int32 n1 = n + pages_count;
        double z = (100.0*n)/n1;
        report_space(gc_number, z);
        if (verbos_flag & 1) trace_printf(
           "At gc end about %.1f Mbytes of %.1f (%.1f%%) of heap is in use\n",
           ((double)n)*(CSL_PAGE_SIZE/(1024.0*1024.0)),
           ((double)n1)*(CSL_PAGE_SIZE/(1024.0*1024.0)), z);
    }
#else
    if (verbos_flag & 1)
    {   int32 n = heap_pages_count + vheap_pages_count + bps_pages_count;
        int32 n1 = n + pages_count;
        trace_printf(
            "At gc end about %.1f Mbytes of %.1f (%.1f%%) of heap is in use\n",
            (double)n*(CSL_PAGE_SIZE/(1024.0*1024.0)),
            (double)n1*(CSL_PAGE_SIZE/(1024.0*1024.0)),
            (100.0*n)/n1);
    }
#endif
#ifndef NO_COPYING_GC
/*
 * I will make the next garbage collection a copying one if the heap is
 * at most 25% full, or a sliding one if it is more full than that.
 */
    gc_method_is_copying = (pages_count >
                 3*(heap_pages_count + vheap_pages_count + bps_pages_count));
#endif
    if (stop_after_gc)
    {
#ifdef MEMORY_TRACE
#ifndef CHECK_ONLY
        memory_comment(15);
#endif
#endif
        return Lstop(nil, fixnum_of_int(0));
    }
#ifdef POLL_FOR_ATTN
    poll_for_attn();   /* Should cause ctrl-C checking */
    nil = C_nil;
    if (exception_pending())
    {
#ifdef MEMORY_TRACE
#ifndef CHECK_ONLY
        memory_comment(15);
#endif
#endif
        return nil;
    }
#endif
#ifdef MEMORY_TRACE
#ifndef CHECK_ONLY
    memory_comment(15);
#endif
#endif
    if (interrupt_pending)
    {   interrupt_pending = NO;
        already_in_gc = NO;
        tick_on_gc_exit = NO;
        return interrupted(p);
    }
    already_in_gc = NO;
    if (tick_on_gc_exit)
    {
        inject_randomness((int)clock());
        accept_tick();
        nil = C_nil;
        if (exception_pending()) return nil;
        tick_on_gc_exit = NO;
        return onevalue(p);
    }

    return onevalue(p);
#endif /* DEMO_MODE */
}

/* end of file gc.c */
