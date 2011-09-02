/*  csl.c                            Copyright (C) 1989-2002 Codemist Ltd */

/*
 * This is Lisp system for use when delivering Lisp applications
 * (such as REDUCE) on pretty-well any computer that has an ANSI
 * C compiler where sizeof(void *)==4 and there is in integral
 * type that is also 4 bytes wide.  In fact I can also manage if
 * sizeof(void *)==8 provided that it can be arranged that all
 * addresses returned by malloc() have only their bottom 32 bits
 * set... And with even more care I can manage on true 64 bit systems,
 * although arithmetic then does not take advantage of the wider words.
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


/* Signature: 5d375d00 17-Feb-2003 */

#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#include "machine.h"

#include "tags.h"
#include "externs.h"
#include "arith.h"
#include "read.h"
#include "stream.h"
#include "entries.h"
#include "version.h"
#define  INCLUDE_ERROR_STRING_TABLE 1
#include "cslerror.h"
#undef   INCLUDE_ERROR_STRING_TABLE
#ifdef TIMEOUT
#include "timeout.h"
#endif

#ifdef OLD_THINK_C
#include <console.h>
#include <memory.h>
#undef nil  /* Yuk - this is defined by <types.h> which <memory.h> loads */
#endif

#ifdef __WATCOMC__
#include <float.h>
#endif

#ifdef SOCKETS

#include "sockhdr.h"

#ifndef ms_windows
#include <sys/wait.h>
#endif

static int port_number, remote_store, current_users, max_users;
SOCKET socket_server;
int sockets_ready;
clock_t cpu_timeout;
time_t elapsed_timeout;

static int char_to_socket(int c);

#endif

/*
 * These flags are used to ensure that protected symbols don't get
 * overwritten by default, and that the system keeps quiet about it.
 */

CSLbool symbol_protect_flag = YES;
CSLbool warn_about_protected_symbols = NO;

#ifdef WINDOW_SYSTEM
CSLbool use_wimp;
#endif

#ifdef USE_MPI
int32 mpi_rank,mpi_size;
#endif

/*****************************************************************************/
/*      Error reporting and recovery                                         */
/*****************************************************************************/

#ifdef CHECK_STACK
/*
 * Some computers are notably unhelpful about their behaviour when the system
 * stack overflows. As a debugging tool on such machines I can do limited
 * software checking by inserting explicit calls to this function in places
 * I think may be critical.  I impose an arbitrary limit on the stack size,
 * but that is better than no checking and random corruption - maybe. Please
 * do not enable CHECK_STACK unless it is really necessary to hunt a bug,
 * since it is miserably expensive and crude.
 */

#define C_STACK_ALLOCATION 240000

static int spset = 0;
static int32 spbase = 0, spmin;

static int stack_depth[C_STACK_ALLOCATION], stack_line[C_STACK_ALLOCATION];
static char *stack_file[C_STACK_ALLOCATION];
static int c_stack_ptr = 0;

int check_stack(char *file, int line)
{
    int32 temp = (int32)&temp;
    if (!spset)
    {   spbase = spmin = temp;
        spset = 1;
        c_stack_ptr = 0;
        stack_depth[0] = temp;
        stack_line[0] = line;
        stack_file[0] = file;
    }
    if (temp < stack_depth[c_stack_ptr] && c_stack_ptr<C_STACK_ALLOCATION-2)
        c_stack_ptr++;
    else while (temp > stack_depth[c_stack_ptr] && c_stack_ptr>0)
        c_stack_ptr--;
    stack_depth[c_stack_ptr] = temp;
    stack_line[c_stack_ptr] = line;
    stack_file[c_stack_ptr] = file;
    if (temp < spmin-250)  /* Only check at granularity of 250 bytes */
    {   int i;
        term_printf("Stack depth %d at file %s line %d\n",
                     spbase-temp, file, line);
        for (i=c_stack_ptr; i>=0 && i > c_stack_ptr-30; i--)
            term_printf(" %s:%d", stack_file[i], stack_line[i]);
        term_printf("\n");
        spmin = temp;
        if (temp < spbase-C_STACK_ALLOCATION) return 1;
    }
    return 0;
}
#endif

/*
 * error_message_table was defined in cslerror.h since that way I can keep its
 * contents textually close to the definitions of the message codes that it
 * has to relate to.
 */

#define errcode(n) error_message_table[n]

Lisp_Object MS_CDECL error(int nargs, int code, ...)
/*
 * nargs indicates how many values have been provided AFTER the
 * code.  Thus nargs==0 will just display a simple message, nargs==1
 * will be a message plus a value and so on.  I will expect that the
 * number of actual args here is well within any limits that I ought to
 * impose.
 */
{
    va_list a;
    int i;
    Lisp_Object nil = C_nil, w1;
    Lisp_Object *w = (Lisp_Object *)&work_1;
    if (nargs > ARG_CUT_OFF) nargs = ARG_CUT_OFF;
    if (miscflags & (HEADLINE_FLAG|ALWAYS_NOISY))
    {   err_printf("\n+++ Error %s: ", errcode(code));
/*
 * There is now some painful shuffling around to get all the args
 * to error() moved over onto the Lisp stack so that is garbage collection
 * is triggered during printing all will be well.
 */
        va_start(a, code);
        for (i=0; i<nargs; i++) *w++ = va_arg(a, Lisp_Object);
        va_end(a);
        for (i=0; i<nargs; i++) push(*--w);
        if (code != err_stack_overflow)  /* Be cautious here! */
        {   stackcheck0(nargs);
        }
        for (i=0; i<nargs; i++)
        {   Lisp_Object p;
            pop(p);
            loop_print_error(p);
            err_printf("\n");
        }
    }
    if ((w1 = qvalue(break_function)) != nil &&
        symbolp(w1) &&
        qfn1(w1) != undefined1)
    {   (*qfn1(w1))(qenv(w1), nil);
        ignore_exception();
    }
/*
 * After doing this is is necessary to be VERY careful, since nil is
 * used as a base register for lots of things...  Still this is the
 * cheapest way I can see to mark the need for unwinding.
 */
    exit_reason = (miscflags & (MESSAGES_FLAG|ALWAYS_NOISY)) ? UNWIND_ERROR :
                  UNWIND_UNWIND;
    exit_value = exit_tag = nil;
    exit_count = 0;
    flip_exception();
    return nil;
}

Lisp_Object MS_CDECL cerror(int nargs, int code1, int code2, ...)
/*
 * nargs indicated the number of EXTRA args after code1 & code2.
 */
{
    Lisp_Object nil = C_nil, w1;
    va_list a;
    int i;
    Lisp_Object *w = (Lisp_Object *)&work_1;
    if (nargs > ARG_CUT_OFF) nargs = ARG_CUT_OFF;
    if (miscflags & (HEADLINE_FLAG|ALWAYS_NOISY))
    {   err_printf("\n+++ Error %s, %s: ", errcode(code1), errcode(code2));
        va_start(a, code2);
        for (i=0; i<nargs; i++) *w++ = va_arg(a, Lisp_Object);
        va_end(a);
        for (i=0; i<nargs; i++) push(*--w);
        stackcheck0(nargs-2);
        nil = C_nil;
        for (i=0; i<nargs; i++)
        {   Lisp_Object p;
            pop(p);
            loop_print_error(p);
            err_printf("\n");
        }
    }
    if ((w1 = qvalue(break_function)) != nil &&
        symbolp(w1) &&
        qfn1(w1) != undefined1)
    {   (*qfn1(w1))(qenv(w1), nil);
        ignore_exception();
    }
/*
 * After doing this is is necessary to be VERY careful, since nil is
 * used as a base register for lots of things...  Still this is the
 * cheapest way I can see to mark the need for unwinding.
 */
    exit_reason = (miscflags & (MESSAGES_FLAG|ALWAYS_NOISY)) ? UNWIND_ERROR :
                  UNWIND_UNWIND;
    exit_value = exit_tag = nil;
    exit_count = 0;
    flip_exception();
    return nil;
}

Lisp_Object interrupted(Lisp_Object p)
/*
 * Could return onevalue(p) to proceed from the interrupt event...
 */
{
    Lisp_Object nil = C_nil, w;
#ifdef WINDOW_SYSTEM
/*
 * If I have a windowed system I expect that the mechanism for
 * raising an exception will have had a menu that gave me a chance
 * to decide whether to proceed or abort.  Thus the following code
 * is only needed if there is no window system active.  On some systems
 * this may be an active check.
 */
    if (!use_wimp)
#endif
    {
        if (clock_stack == &consolidated_time[0])
        {   clock_t t0 = read_clock();
/*
 * On at least some (Unix) systems clock_t is a 32-bit signed value
 * and CLOCKS_PER_SEC = 1000000. The effect is that integer overflow
 * occurs after around 35 minutes of running. I must therefore check the
 * clock and move information into a floating point variable at least
 * every half-hour.  With luck I will do it more like 20 times per second
 * because I have code muck like this in a tick handler that is activated
 * on a rather regular basis.
 */
            double delta = (double)(t0 - base_time)/(double)CLOCKS_PER_SEC;
            base_time = t0;
            consolidated_time[0] += delta;
        }
#ifndef NAG
        term_printf(
            "\n+++ [%.2f+%.2f] Type C to continue, A to abort, X to exit\n",
            consolidated_time[0], gc_time);
        ensure_screen(); nil = C_nil;
        if (exception_pending()) return nil;
        push(prompt_thing);
        prompt_thing = CHAR_EOF;
        other_read_action(READ_FLUSH, lisp_terminal_io);
        for (;;)
        {   int c = char_from_terminal(nil);
/*
 * Note that I explicitly say "char_from_terminal()" here - this is because
 * I do not expect to be interrupted unless there was a terminal available
 * to send the interrupt! This is in fact a slightly marginal assumption.
 */
            switch (c)
            {
        case 'c': case 'C':         /* proceed as if no interrupt */
                pop(prompt_thing);
                return onevalue(p);
        case 'a': case 'A':         /* raise an exception */
                break;
        case 'x': case 'X':
                my_exit(EXIT_FAILURE); /* Rather abrupt */
        case '\n':
                term_printf("C to continue, A to abort, X to exit: ");
                ensure_screen(); nil = C_nil;
                if (exception_pending()) return nil;
                continue;
        default:                    /* wait for A or C */
                continue;
            }
            break;
        }
        pop(prompt_thing);
#endif
    }
    if (miscflags & (HEADLINE_FLAG|ALWAYS_NOISY))
	err_printf("+++ Interrupted\n");
    if ((w = qvalue(break_function)) != nil &&
        symbolp(w) &&
        qfn1(w) != undefined1)
    {   (*qfn1(w))(qenv(w), nil);
        ignore_exception();
    }
    exit_reason = (miscflags & (MESSAGES_FLAG|ALWAYS_NOISY)) ? UNWIND_ERROR :
                  UNWIND_UNWIND;
    exit_value = exit_tag = nil;
    exit_count = 0;
    flip_exception();
    return nil;
}

Lisp_Object aerror(char *s)
{
    Lisp_Object nil = C_nil, w;
    if (miscflags & (HEADLINE_FLAG|ALWAYS_NOISY))
        err_printf("+++ Error bad args for %s\n", s);
    if ((w = qvalue(break_function)) != nil &&
        symbolp(w) &&
        qfn1(w) != undefined1)
    {   (*qfn1(w))(qenv(w), nil);
        ignore_exception();
    }
    exit_reason = (miscflags & (MESSAGES_FLAG|ALWAYS_NOISY)) ? UNWIND_ERROR :
                  UNWIND_UNWIND;
    exit_value = exit_tag = nil;
    exit_count = 0;
    flip_exception();
    return nil;
}

Lisp_Object aerror0(char *s)
{
    Lisp_Object nil = C_nil, w;
    if (miscflags & (HEADLINE_FLAG|ALWAYS_NOISY))
        err_printf("+++ Error: %s\n", s);
    if ((w = qvalue(break_function)) != nil &&
        symbolp(w) &&
        qfn1(w) != undefined1)
    {   (*qfn1(w))(qenv(w), nil);
        ignore_exception();
    }
    exit_reason = (miscflags & (MESSAGES_FLAG|ALWAYS_NOISY)) ? UNWIND_ERROR :
                  UNWIND_UNWIND;
    exit_value = exit_tag = nil;
    exit_count = 0;
    flip_exception();
#ifdef COMMON
/*
 * This is to help me debug in the face of low level system crashes
 */
    if (spool_file) fflush(spool_file);
#endif
    return nil;
}

Lisp_Object aerror1(char *s, Lisp_Object a)
{
    Lisp_Object nil = C_nil, w;
    if (miscflags & (HEADLINE_FLAG|ALWAYS_NOISY))
    {   err_printf("+++ Error: %s ", s);
        loop_print_error(a);
        err_printf("\n");
    }
    if ((w = qvalue(break_function)) != nil &&
        symbolp(w) &&
        qfn1(w) != undefined1)
    {   (*qfn1(w))(qenv(w), nil);
        ignore_exception();
    }
    exit_reason = (miscflags & (MESSAGES_FLAG|ALWAYS_NOISY)) ? UNWIND_ERROR :
                  UNWIND_UNWIND;
    exit_value = exit_tag = nil;
    exit_count = 0;
    flip_exception();
#ifdef COMMON
/*
 * This is to help me debug in the face of low level system crashes
 */
    if (spool_file) fflush(spool_file);
#endif
    return nil;
}

Lisp_Object aerror2(char *s, Lisp_Object a, Lisp_Object b)
{
    Lisp_Object nil = C_nil, w;
    if (miscflags & (HEADLINE_FLAG|ALWAYS_NOISY))
    {   err_printf("+++ Error: %s ", s);
        loop_print_error(a);
        err_printf(" ");
        loop_print_error(b);
        err_printf("\n");
    }
    if ((w = qvalue(break_function)) != nil &&
        symbolp(w) &&
        qfn1(w) != undefined1)
    {   (*qfn1(w))(qenv(w), nil);
        ignore_exception();
    }
    exit_reason = (miscflags & (MESSAGES_FLAG|ALWAYS_NOISY)) ? UNWIND_ERROR :
                  UNWIND_UNWIND;
    exit_value = exit_tag = nil;
    exit_count = 0;
    flip_exception();
#ifdef COMMON
/*
 * This is to help me debug in the face of low level system crashes
 */
    if (spool_file) fflush(spool_file);
#endif
    return nil;
}

static Lisp_Object wrong(int wanted, int given, Lisp_Object env)
{
    char msg[64];
    Lisp_Object nil = C_nil;
    CSL_IGNORE(nil);
    sprintf(msg, "Function called with %d args where %d wanted", given, wanted);
    if (is_cons(env)) env = qcdr(env);
    if ((miscflags & (HEADLINE_FLAG|ALWAYS_NOISY)) && is_vector(env))
    {   Lisp_Object fname = elt(env, 0);
        err_printf("\nCalling ");
        loop_print_error(fname);
        err_printf("\n");
    }
    return aerror(msg);
}

Lisp_Object too_few_2(Lisp_Object env, Lisp_Object a1)
{
    CSL_IGNORE(a1);
    return wrong(2, 1, env);
}

Lisp_Object too_many_1(Lisp_Object env, Lisp_Object a1, Lisp_Object a2)
{
    CSL_IGNORE(a1);
    CSL_IGNORE(a2);
    return wrong(1, 2, env);
}

Lisp_Object wrong_no_0a(Lisp_Object env, Lisp_Object a1)
{
    CSL_IGNORE(a1);
    return wrong(0, 1, env);
}

Lisp_Object wrong_no_0b(Lisp_Object env, Lisp_Object a1, Lisp_Object a2)
{
    CSL_IGNORE(a1);
    CSL_IGNORE(a2);
    return wrong(0, 2, env);
}

Lisp_Object wrong_no_3a(Lisp_Object env, Lisp_Object a1)
{
    CSL_IGNORE(a1);
    return wrong(3, 1, env);
}

Lisp_Object wrong_no_3b(Lisp_Object env, Lisp_Object a1, Lisp_Object a2)
{
    CSL_IGNORE(a1);
    CSL_IGNORE(a2);
    return wrong(3, 2, env);
}

Lisp_Object wrong_no_na(Lisp_Object env, Lisp_Object a1)
{
    CSL_IGNORE(a1);
    if (is_cons(env) && is_bps(qcar(env)))
        return wrong(((unsigned char *)data_of_bps(qcar(env)))[0], 1, env);
    else return aerror("function called with 1 arg when 0 or >= 3 wanted");
}

Lisp_Object wrong_no_nb(Lisp_Object env, Lisp_Object a1, Lisp_Object a2)
{
    CSL_IGNORE(a1);
    CSL_IGNORE(a2);
    if (is_cons(env) && is_bps(qcar(env)))
        return wrong(((unsigned char *)data_of_bps(qcar(env)))[0], 2, env);
    else return aerror("function called with 2 args when 0 or >= 3 wanted");
}

Lisp_Object MS_CDECL wrong_no_1(Lisp_Object env, int nargs, ...)
{
    CSL_IGNORE(env);
    CSL_IGNORE(nargs);
    return wrong(1, nargs, env);
}

Lisp_Object MS_CDECL wrong_no_2(Lisp_Object env, int nargs, ...)
{
    CSL_IGNORE(env);
    CSL_IGNORE(nargs);
    return wrong(2, nargs, env);
}

Lisp_Object bad_special2(Lisp_Object env, Lisp_Object a1, Lisp_Object a2)
{
    CSL_IGNORE(env);
    CSL_IGNORE(a1);
    CSL_IGNORE(a2);
    return aerror("call to special form");
}

Lisp_Object MS_CDECL bad_specialn(Lisp_Object env, int nargs, ...)
{
    CSL_IGNORE(env);
    CSL_IGNORE(nargs);
    return aerror("call to special form");
}

void MS_CDECL fatal_error(int code, ...)
{
/*
 * Note that FATAL error messages are sent to the terminal, not to the
 * error output stream. This is because the error output stream may be
 * corrupted in such dire circumstances.
 */
    term_printf("+++ Fatal error %s\n", errcode(code));
    if (spool_file != NULL) 
    {
#ifdef COMMON
        fprintf(spool_file, "\nFinished dribbling to %s.\n", spool_file_name);
#else
        fprintf(spool_file, "\n+++ Transcript terminated after error +++\n");
#endif
        fclose(spool_file);
        spool_file = NULL;
    }
    my_exit(EXIT_FAILURE);
}

#ifndef __cplusplus
jmp_buf my_exit_buffer;
#endif

void my_exit(int n)
{
#ifdef USE_MPI
    MPI_Finalize();
#endif
#if defined(FLEX) && defined(WINDOWS_NT) && defined(CWIN) && defined(NAG)
    extern void close_lm();
    close_lm();
/* /* What is realy needed here??? */
    extern void rlnag();
    rlnag();
#endif
#ifdef BUFFERED_STDOUT
    ensure_screen();
#endif
#ifdef SOCKETS
    if (sockets_ready) WSACleanup();
#endif
#ifdef WINDOW_SYSTEM
    pause_for_user();
#endif
#ifdef CWIN
#ifdef __cplusplus
    throw n;
#else
    if (n == 0) n = 0x80000000;
    longjmp(my_exit_buffer, n);
#endif
#else
#if defined(WINDOWS_NT) && defined(NAG)
    {   extern void sys_abort(int);
        sys_abort(n);
    }
#else
#ifdef TICK_STREAM
    remove_ticker();
#endif
    exit(n);
#endif
#endif
}

static int return_code = 0;
CSLbool segvtrap = YES;
CSLbool batch_flag = NO;
CSLbool ignore_restart_fn = NO;

static void lisp_main(void)
{
    Lisp_Object nil;
    
#ifndef __cplusplus
/*
 * The setjmp here is to provide a long-stop for untrapped
 * floating point exceptions.
 */
    jmp_buf this_level, *save_level = errorset_buffer;
#endif
    tty_count = 0;
    while (YES)
/*
 * The sole purpose of the while loop here is to allow me to proceed
 * for a second try if I get a (cold-start) call.
 */
    {   Lisp_Object *save = stack;
        nil = C_nil;
#ifndef __cplusplus
        errorset_buffer = &this_level;
#endif
        errorset_msg = NULL;
#ifdef __cplusplus
        try
#else
        if (!setjmp(this_level))
#endif
        {   if (supervisor != nil && !ignore_restart_fn)
            {   miscflags |= HEADLINE_FLAG | MESSAGES_FLAG;
                if (exit_charvec != NULL)
                {   Lisp_Object a = read_from_vector(exit_charvec);
                    nil = C_nil;
                    if (exception_pending())
                    {   flip_exception();
                        a = nil;
                    }
                    exit_charvec = NULL;
                    push(a);
                    apply(supervisor, 1, nil, supervisor);
                }
                else apply(supervisor, 0, nil, supervisor);
            }
/*
 * Here the default read-eval-print loop used if the user has not provided
 * a supervisor function.
 */
            else read_eval_print(lisp_true);
        }
#ifdef __cplusplus
        catch (char *)
#else
        else
#endif
        {   if (errorset_msg != NULL)
            {   term_printf("\n%s detected\n", errorset_msg);
                errorset_msg = NULL;
            }
            unwind_stack(save, NO);
            exit_reason = UNWIND_ERROR;
            flip_exception();
            signal(SIGFPE, low_level_signal_handler);
#ifdef __WATCOMC__
            _control87(_EM_OVERFLOW | _EM_INVALID | _EM_DENORMAL |
                       _EM_ZERODIVIDE | _EM_INEXACT | _EM_UNDERFLOW,
                       _MCW_EM);
#endif
            if (segvtrap) signal(SIGSEGV, low_level_signal_handler);
#ifdef SIGBUS
            if (segvtrap) signal(SIGBUS, low_level_signal_handler);
#endif
#ifdef SIGILL
            if (segvtrap) signal(SIGILL, low_level_signal_handler);
#endif
        }
        nil = C_nil;
        if (exception_pending())
        {   flip_exception();
            if (exit_reason == UNWIND_RESTART)
            {   if (exit_tag == fixnum_of_int(0))      /* "stop" */
                    return_code = (int)int_of_fixnum(exit_value);
                else if (exit_tag == fixnum_of_int(1)) /* "preserve" */
                {   char *msg = "";
                    return_code = EXIT_SUCCESS;
                    compression_worth_while = 128;
                    if (is_vector(exit_value) &&
                        type_of_header(vechdr(exit_value)) == TYPE_STRING)
                        msg = &celt(exit_value, 0);
                    preserve(msg);
                    nil = C_nil;
                    if (exception_pending())
                    {   flip_exception();
                        return_code = EXIT_FAILURE;
                    }
                }
                else                                   /* "restart" */
                {   int32 fd = stream_pushed_char(lisp_terminal_io);
                
                    char new_module[64], new_fn[64]; /* Limited name length */
                    int cold_start;
                    cold_start = (exit_value == nil);
#ifdef TICK_STREAM
                    remove_ticker();
/*
 * Of course a tick may very well have happened rather recently - so
 * I will flush it out now just to clear the air.
 */
                    if (stack >= stacklimit)
                    {   reclaim(nil, "stack", GC_STACK, 0);
                        ignore_exception();
                    }
#endif
                    cold_start = (exit_value == nil);
                    Lrds(nil, nil);
                    Lwrs(nil, nil);
/*
 * If either of the above two calls to rds/wrs were to fail I would
 * be in a big mess.
 */
                    if (!cold_start)
                    {   new_module[0] = 0;
                        new_fn[0] = 0;
                        if (exit_value != lisp_true)
                        {   Lisp_Object modname = nil;
                            if (is_cons(exit_value))
                            {   modname = qcar(exit_value);
                                exit_value = qcdr(exit_value);
                                if (is_cons(exit_value))
                                    exit_value = qcar(exit_value);
                            }
                            if (symbolp(modname) && modname != nil)
                            {   modname = get_pname(modname);
                                if (exception_pending()) ignore_exception();
                                else
                                {   Header h = vechdr(modname);
                                    int32 len = length_of_header(h) - CELL;
                                    if (len > 63) len = 63;
                                    memcpy(new_module,
                                           (char *)modname + (CELL - TAG_VECTOR),
                                           (size_t)len);
                                    new_module[len] = 0;
                                }
                            }
                            if (symbolp(exit_value) && exit_value != nil)
                            {   exit_value = get_pname(exit_value);
                                if (exception_pending()) ignore_exception();
                                else
                                {   Header h = vechdr(exit_value);
                                    int32 len = length_of_header(h) - CELL;
                                    if (len > 63) len = 63;
                                    memcpy(new_fn,
                                           (char *)exit_value + (CELL - TAG_VECTOR),
                                           (size_t)len);
                                    new_fn[len] = 0;
                                }
                            }
                        }
                    }
                    while (vheap_pages_count != 0)
                        pages[pages_count++] = vheap_pages[--vheap_pages_count];
                    while (heap_pages_count != 0)
                        pages[pages_count++] = heap_pages[--heap_pages_count];
                    while (bps_pages_count != 0)
                        pages[pages_count++] = bps_pages[--bps_pages_count];
/*
 * When I call restart-csl I will leave the random number generator where it
 * was. Anybody who wants to reset if either to a freshly randomised
 * configuration or to a defined condition must do so for themselves. For
 * people who do not care too much what I do here is probably acceptable!
 */
                    MD5_Init();
                    MD5_Update((unsigned char *)errcode(err_registration), 32);
                    IreInit();
                    setup(cold_start ? 0 : 1, 0.0);
                    exit_tag = exit_value = nil;
                    exit_reason = UNWIND_NULL;
                    stream_pushed_char(lisp_terminal_io) = fd;
                    interrupt_pending = already_in_gc = NO;
                    polltick_pending = tick_pending = tick_on_gc_exit  = NO;
                    if (!cold_start && new_fn[0] != 0)
                    {   Lisp_Object w;
                        if (new_module[0] != 0)
                        {   w = make_undefined_symbol(new_module);
                            Lload_module(nil, w);
                            ignore_exception();
                        }
                        w = make_undefined_symbol(new_fn);
                        nil = C_nil;
                        if (exception_pending()) ignore_exception();
                        else supervisor = w;
                    }
#ifdef TICK_STREAM
                    add_ticker();
#endif
                    continue;
                }
            }
        }
/*
 * In all normal cases when read_eval_print exits (i.e. all cases except
 * if it terminates after (cold-start)) I exit here.
 */
#ifndef __cplusplus
        errorset_buffer = save_level;
#endif
        break;
    }
}

#ifndef MS_DOS
#ifndef WINDOWS_NT
#ifndef WXWIN

CSLbool sigint_must_longjmp = NO;
#ifndef __cplusplus
jmp_buf sigint_buf;
#endif

void sigint_handler(int code)
{
/*
 * Note that the only things that I am really allowed to do in a routine
 * like this involve setting variables of type sig_atomic_t, which can not
 * be viewed as much more than boolean.  The code actually used here is
 * somewhat more ambitious (== non-portable!) so must be viewed as delicate.
 * ANSI guarantee that longjmp-ing out of a non-nested signal handler
 * is valid, but some earlier C libraries have not supported this. Note that
 * with C++ I will use throw rather than longjmp.
 */
/*
 * tick_pending etc allow a steady stream of clock events to
 * be handed to Lisp.
 */
    interrupt_pending = 1;
    signal(SIGINT, sigint_handler);
    if (sigint_must_longjmp)
    {
        sigint_must_longjmp = NO;
#ifdef __cplusplus
        throw((int *)0);
#else
        longjmp(sigint_buf, 1);
#endif
    }
#ifndef TICK_STREAM
/*
 * If there is not a separate regular stream of ticks I will simulate
 * the receipt of a tick here. Thus I need to be able to recognize "ticks"
 * even on systems where there are no "real" ones.
 */
    if (!tick_pending)
    {
        if (already_in_gc) tick_on_gc_exit = YES;
        else
        {
#ifndef NILSEG_EXTERNS
            Lisp_Object nil = C_nil;
            CSLbool xxx = NO;
            if (exception_pending()) flip_exception(), xxx = YES;
#endif
            tick_pending = YES;
            saveheaplimit = heaplimit;
            heaplimit = fringe;
            savevheaplimit = vheaplimit;
            vheaplimit = vfringe;
            savecodelimit = codelimit;
            codelimit = codefringe;
            savestacklimit = stacklimit;
            stacklimit = stackbase;
#ifndef NILSEG_EXTERNS
            if (xxx) flip_exception();
#endif
        }
    }
#endif /* TICK_STREAM */
    return;
}
#endif
#endif
#endif

#ifdef SOFTWARE_TICKS

int32 number_of_ticks = 0;

int32 countdown;

int deal_with_tick(void)
{
    number_of_ticks++;
    if (!tick_pending)
    {
        if (already_in_gc) tick_on_gc_exit = YES;
        else
        {
#ifndef NILSEG_EXTERNS
            Lisp_Object nil = C_nil;
            CSLbool xxx = NO;
            if (exception_pending()) flip_exception(), xxx = YES;
#endif
            tick_pending = YES;
            saveheaplimit = heaplimit;
            heaplimit = fringe;
            savevheaplimit = vheaplimit;
            vheaplimit = vfringe;
            savecodelimit = codelimit;
            codelimit = codefringe;
            savestacklimit = stacklimit;
            stacklimit = stackbase;
#ifndef NILSEG_EXTERNS
            if (xxx) flip_exception();
#endif
        }
    }
#ifdef SOFTWARE_TICKS
    countdown = SOFTWARE_TICKS;
#endif
    return 1;
}

#endif

static long int initial_random_seed, seed2;

char *files_to_read[MAX_INPUT_FILES],
     *symbols_to_define[MAX_SYMBOLS_TO_DEFINE],
     *fasl_paths[MAX_FASL_PATHS];
int output_directory;
character_reader *procedural_input;
character_writer *procedural_output;

CSLbool undefine_this_one[MAX_SYMBOLS_TO_DEFINE];

int number_of_input_files = 0,
    number_of_symbols_to_define = 0,
    number_of_fasl_paths = 0,
    init_flags = 0;

#ifdef WINDOW_SYSTEM
FILE *alternative_stdout = NULL;
#endif

/*
 * standard_directory holds the name of the default image file that CSL
 * would load.
 */
char *standard_directory;

/*
 * If non-NULL the string module_enquiry is a name presenetd on the
 * command line in a "-T name" request, and this will cause the system
 * to behave in a totally odd manner - it does not run Lisp at all but
 * performs a directory enquiry within the image file.
 */
static char *module_enquiry = NULL;

/*
 * The next lines mean that (if you can get in before cslstart is
 * called) you can get memory allocation done in a custom way.
 */

static void *CSL_malloc(size_t n)
{
    return malloc(n);
}

static void  CSL_free(void *p)
{
    free(p);
}

static void *CSL_realloc(void *p, size_t n)
{
    return realloc(p, n);
}

malloc_function  *malloc_hook = CSL_malloc;
realloc_function *realloc_hook = CSL_realloc;
free_function    *free_hook   = CSL_free;

void cslstart(int argc, char *argv[], character_writer *wout)
{
    int i;
    CSLbool restartp, always_noisy = NO;
    double store_size = 0.0;
    stack_segsize = 1;
    module_enquiry = NULL;
#ifdef SOFTWARE_TICKS
    countdown = 0x7fffffff;
#endif
/*
 * Note that I will set up clock_stack AGAIN later on! The one further down
 * happens after command line options have been decoded and is where I really
 * want to consider Lisp to be starting. The setting here is because
 * if I call ensure_screen() it can push and pop the clock stack, and
 * especially if I have an error in my options I may print to the terminal
 * and then flush it. Thus I need SOMETHING set up early to prevent any
 * possible frivolous disasters in that area.
 */
    base_time = read_clock();
    consolidated_time[0] = gc_time = 0.0;
    clock_stack = &consolidated_time[0];
#ifdef WINDOW_SYSTEM
/*
 * On some systems (Archimedes/RISCOS) the same executable
 * may run either under a window manager or using a command line.
 * I select which to use based on a command line option, which
 * I scan for VERY early since until I know what I am doing
 * I can not report errors etc etc
 */
#ifdef RISCOS
    use_wimp = NO;
    for (i=1; i<argc; i++)
    {   char *opt = argv[i];
        if (opt == NULL) continue;
        if (opt[0] == '-' && tolower(opt[1] == 'w'))
        {   use_wimp = !use_wimp;
            break;   /* Repeating "-w" flips the effect */
        }
    }
#else
    use_wimp = YES;  /* Other than on RISCOS always enable window system */
#ifdef CWIN
    cwin_pause_at_end = 1;
#endif
#ifdef macintosh
    {   extern int _w_font;
        extern void winit(void);
        extern int wgetargs(char ***);
        SetApplLimit(GetApplLimit() - 64000); /* Try to reserve plenty of stack space */
        _w_font = 4; winit();
        winitargs(&argc, &argv);              /* THINK_C does not do arguments! */
        argc = wgetargs(&argv); /* Use dialog box for arguments */
    }
#endif
    start_up_window_manager(use_wimp);
#endif
#ifdef SOCKETS
    sockets_ready = 0;
    socket_server = 0;
#endif
/*
 * Now that the window manager is active I can send output through
 * xx_printf() and get it on the screen neatly.
 * On the Archimedes start_up_window_manager() hangs up until somebody
 * hits the right icon with a mouse click, so most of the mallocs etc
 * will not happen until then.
 */
#endif
    procedural_input = NULL;
    procedural_output = wout;
    standard_directory = find_image_directory(argc, argv);
#ifdef OLD_THINK_C
/*
 * Note amazingly well that on the Macintosh I identify the image directory
 * BEFORE I grab command-line options etc, since the Think C "ccommand"
 * function can reset the current directory, and I need access to same
 * (but not to the faked command line!) while establishing my image
 * location.
 */
    argc = ccommand(&argv);
#endif
    restartp = YES;
    ignore_restart_fn = NO;
    spool_file = NULL;
    spool_file_name[0] = 0;
    output_directory = 0x80000000;
    number_of_input_files = 0;
    number_of_symbols_to_define = 0;
    number_of_fasl_paths = 0;
    gc_method_is_copying = NO;
    fasl_output_file = NO;
    initial_random_seed = seed2 = 0;
    init_flags = INIT_EXPANDABLE;
    return_code = EXIT_SUCCESS;
    segvtrap = YES;
    batch_flag = NO;
    {   char *s = REGISTRATION_VERSION;
#define hexval(c) ('0'<=c && c<='9' ? c - '0' : c - 'a' + 10)
#define gx() (s+=2, hexval(s[-1]) + 16*hexval(s[-2]))
        unsigned char *p = registration_data;
        memset(registration_data, 0, sizeof(REGISTRATION_SIZE));
        while (*s != 0) *p++ = *s++;
        s = REG1;
        while (*s != 0) *p++ = gx();
        s = REG2;
        while (*s != 0) *p++ = gx();
        MD5_Init();
        MD5_Update((unsigned char *)errcode(err_registration), 32);
    }
#ifdef MEMORY_TRACE
    car_counter = 0x7fffffff;
    car_low = 0;
    car_high = 0xffffffff;
#endif

#ifdef __WATCOMC__
    _control87(_EM_OVERFLOW | _EM_INVALID | _EM_DENORMAL |
               _EM_ZERODIVIDE | _EM_INEXACT | _EM_UNDERFLOW,
               _MCW_EM);
#endif

    argc--;
    for (i=1; i<=argc; i++)
    {   char *opt = argv[i];
/*
 * The next line ought never to be activated, but I have sometimes seen
 * unwanted NULL args on the end of command lines so I filter them out
 * here as a matter of security.
 */
        if (opt == NULL || *opt == 0) continue;
        if (opt[0] == '-')
        {   char *w;
            int c1 = opt[1], c2 = opt[2];
            if (isupper(c1)) c1 = tolower(c1);
            switch (c1)
            {

/*
 * -- <outfile> arranges that output is sent to the indicated file. It is
 * intended to behave a little like "> outfile" as command-line output
 * redirection, but is for usin in windowed environments (in particular
 * Windows NT) where this would not work.  I had intended to use "->" here,
 * but then the ">" tends to get spotted as a command-line request for
 * redirection, and I would not be using this if command-line redirection
 * worked properly!
 */
        case '-':
                if (c2 != 0) w = &opt[2];
                else if (i != argc) w = argv[++i];
                else break; /* Illegal at end of command-line */
                {   char filename[LONGEST_LEGAL_FILENAME];
                    FILE *f;
#ifdef WINDOW_SYSTEM
                    f = open_file(filename, w, strlen(w), "w", NULL);
                    if (f == NULL)
                    {
#ifdef CWIN
/*
 * Under CWIN if there is a "--" among the arguments I will start off
 * with the main window minimized. Thus if an error is detected at a
 * stage that the transcript file is not properly opened I need to
 * maximize the window so I can see the error!
 */
                        cwin_maximize();
#endif
                        term_printf("Unable to write to \"%s\"\n", filename);
                        continue;
                    }
                    else
                    {   term_printf("Output redirected to \"%s\"\n",
                                    filename);
                    }
                    if (alternative_stdout != NULL)
                        fclose(alternative_stdout);
                    alternative_stdout = f;
#else
/*
 * I use freopen() on stdout here to get my output sent elsewhere.  Quite
 * what sort of mess I am in if the freopen fails is hard to understand!
 * Thus I write a message to stderr and exit promptly in case of trouble.
 * I print a message explaining what I am doing BEFORE actually performing
 * the redirection.
 */
                    fprintf(stderr, "Output to be redirected to \"%s\"\n", w);
                    f = open_file(filename, w, strlen(w), "w", stdout);
                    if (f == NULL)
                    {   fprintf(stderr, "Unable to write to \"%s\"\n",
                                        filename);
#ifdef CWIN
#ifdef __cplusplus
                        throw EXIT_FAILURE;
#else
                        longjmp(my_exit_buffer, EXIT_FAILURE);
#endif
#else
                        exit(EXIT_FAILURE);
#endif
                    }
#endif
                }
                continue;

/*
 * -b is a curious option, not intended for general or casual use. If given
 * it causes the (batchp) function to return the opposite result from
 * normal!  Without "-b" (batchp) returns T either if at least one file
 * was specified on the command line, or if the standard input is "not
 * a tty" (under some operating systems this makes sense - for instance
 * the standard input might not be a "tty" if it is provided via file
 * redirection).  Otherwise (ie primary input is directly from a keyboard)
 * (batchp) returns nil.  Sometimes this judgement about how "batch" the
 * current run is will be wrong or unhelpful, so "-b" allows the user to
 * coax the system into better behaviour.
 */
        case 'b':
                batch_flag = YES;
                continue;

/*
 * The option "-C" just prints a dull and unimaginative copyright notice -
 * having this option in there will tend to ensure that a copyright
 * message is embedded in the object code somehow, while with luck nobody
 * will be bothered too much by the fact that there is a stray option to get
 * it displayed.  Note that on some systems there is a proper character
 * for the Copyright symbol... but there is little agreement about what
 * that code is!
 */
        case 'c':
#ifdef CWIN
                cwin_maximize();
#endif
#ifdef macintosh
                term_printf("\nCopyright \231 Codemist Ltd, 1988-2003\n");
#else
                term_printf("\nCopyright (C) Codemist Ltd, 1988-2003\n");
#endif
                continue;

/*
 * -D name=val   defines a symbol at the start of a run
 * I permit either
 *                  -Dname=val
 * or               -D name=val
 */
        case 'd':
                if (c2 != 0) w = &opt[2];
                else if (i != argc) w = argv[++i];
                else break; /* Illegal at end of command-line */
                if (number_of_symbols_to_define < MAX_SYMBOLS_TO_DEFINE)
                    symbols_to_define[number_of_symbols_to_define] = w,
                    undefine_this_one[number_of_symbols_to_define++] = NO;
                else
                {
#ifdef CWIN
                    cwin_maximize();
#endif
                    term_printf("Too many \"-D\" requests: ignored\n");
                }
                continue;

        case 'e':
/*
 *                      -E
 * This option is for an EXPERIMENT.  It may do different things in different
 * releases of CSL.
 */
                continue;

#ifdef SOCKETS
        case 'f':
/*
 *                     -F
 * This is used with syntax -Fnnn or -F nnn (with nnn a number above
 * 1000 but less than 65536) to cause the system to run not as a normal
 * interactive application but as a server listening on the indicated port.
 * The case -F- (which could of course be "-F -") indicates use of the
 * default port for CSL, which I hereby declare to be 1206. This number may
 * need to be changed later if I find it conflicts with some other common
 * package or usage, but was selected in memory of the project number
 * at one time allocated to the Archimedeans Computing Group.
 * On some systems if I want to set up a server that can serve multiple
 * clients I may need to re-invoke CSL afresh for each new client, and in
 * such cases the internally generated tasks can be passed information
 * from their parent task using -F followed by non-numeric information.
 * Any user who attempts such usage will get "what they deserve".
 */
                if (c2 != 0) w = &opt[2];
                else if (i != argc) w = argv[++i];
                else break; /* Illegal at end of command-line */
                port_number = default_csl_server_port;
                remote_store = REMOTE_STORE;
                max_users = MAX_USERS;
                if (strcmp(w, "-") == 0)
                    port_number = default_csl_server_port;
                else if (sscanf(w, "%d:%d:%d",
                                &port_number, &max_users, &remote_store) < 1 ||
                    port_number <= 1000 ||
                    port_number >= 65536 ||
                    max_users < 2 || max_users > 50 ||
                    remote_store < 4000 || remote_store > 20000)
                {
#ifdef CWIN
                    cwin_maximize();
#endif
                    term_printf("\"%s\" is valid (want port:users:store\n", w);
                    continue;
                }
                store_size = (double)remote_store;
                init_flags &= ~INIT_EXPANDABLE;
                current_users = 0;
/*
 * The code here is probably a bit painfully system-specific, and so one
 * could argue that it should go in a separate file. However a LOT of the
 * socket interface is the same regardless of the host, or a few simple
 * macros can have made it so. So if SOCKETS has been defined I will
 * suppose I can continue here on that basis. I do quite want to put the
 * basic socket code in csl.c since it is concerned with system startup and
 * the selection of sources and sinks for IO.
 */
                if (ensure_sockets_ready() == 0)
                {   SOCKET sock1, sock2;
                    struct sockaddr_in server_address, client_address;
                    int sin_size;
                    sock1 = socket(AF_INET, SOCK_STREAM, 0);
                    if (sock1 == SOCKET_ERROR)
                    {
#ifdef CWIN
                        cwin_maximize();
#endif
                        term_printf("Unable to create a socket\n");
                        continue;
                    }
                    server_address.sin_family = AF_INET;
                    server_address.sin_port = htons(port_number);
                    server_address.sin_addr.s_addr = INADDR_ANY;
                    memset((char *)&(server_address.sin_zero), 0, 8);
                    if (bind(sock1, (struct sockaddr *)&server_address,
                             sizeof(struct sockaddr)) == SOCKET_ERROR)
                    {
#ifdef CWIN
                        cwin_maximize();
#endif
                        term_printf("Unable to bind socket to port %d\n",
                                     port_number);
                        closesocket(sock1);
                        continue;
                    }
                    if (listen(sock1, PERMITTED_BACKLOG) == SOCKET_ERROR)
                    {
#ifdef CWIN
                        cwin_maximize();
#endif
                        term_printf("Failure in listen() on port %d\n",
                                     port_number);
                        closesocket(sock1);
                        continue;
                    }
                    for (;;)
                    {   struct hostent *h;
                        time_t t0;
                        sin_size = sizeof(struct sockaddr_in);
                        sock2 = accept(sock1,
                                       (struct sockaddr *)&client_address,
                                       &sin_size);
                        if (sock2 == SOCKET_ERROR)
                        {
#ifdef CWIN
                            cwin_maximize();
#endif
                            term_printf("Trouble with accept()\n");
                            continue;  /* NB local continue here */
                        }
                        t0 = time(NULL);
                        term_printf("%.24s from %s",
                                    ctime(&t0),
                                    inet_ntoa(client_address.sin_addr));
                        h = gethostbyaddr((char *)&client_address.sin_addr,
                                    sizeof(client_address.sin_addr), AF_INET);
                        if (h != NULL)
                            term_printf(" = %s", h->h_name);
                        else term_printf(" [unknown host]");
/*
 * Here I have a bit of a mess. Under Unix I can do a fork() so that the
 * requests that are coming in are handled by a separate process. The
 * code is pretty easy. However with Windows I can only create a fresh process
 * by re-launching CSL from the file it was originally fetched from. I
 * will try to do that in a while, but for now I will leave the
 * Windows version of this code only able to handle a single client
 * session.
 */
#ifdef ms_windows
                        closesocket(sock1);
                        socket_server = sock2;
                        cpu_timeout = clock() + CLOCKS_PER_SEC*MAX_CPU_TIME;
                        elapsed_timeout = time(NULL) + 60*MAX_ELAPSED_TIME;
                        procedural_output = char_to_socket;
                        term_printf("Welcome to the Codemist server\n");
                        ensure_screen();
                        break;
#else /* ms_windows */
                        while (waitpid(-1, NULL, WNOHANG) > 0) current_users--;
                        if (current_users >= max_users)
                        {   term_printf(" refused\n");
                            socket_server = sock2;
                            ensure_screen();
                            procedural_output = char_to_socket;
                            term_printf(
            "\nSorry, there are already %d people using this service\n",
                                 current_users);
                            term_printf("Please try again later.\n");
                            ensure_screen();
                            procedural_output = NULL;
                            closesocket(socket_server);
                            socket_server = 0;
                            continue;
                        }
                        else term_printf(" %d of %d\n",
                                          ++current_users, max_users);
                        ensure_screen();
                        if (!fork())
                        {   /* Child process here */
                            closesocket(sock1);
                            fcntl(sock2, F_SETFL, O_NONBLOCK);
                            socket_server = sock2;
                            cpu_timeout = clock() + CLOCKS_PER_SEC*MAX_CPU_TIME;
                            elapsed_timeout = time(NULL) + 60*MAX_ELAPSED_TIME;
                            ensure_screen();
                            procedural_output = char_to_socket;
                            term_printf("Welcome, you are user %d of %d\n",
                                   current_users, max_users);
                            term_printf(
                                 "You have been allocated %d seconds CPU time"
                                 " and %d minutes elapsed time\n",
                                 MAX_CPU_TIME, MAX_ELAPSED_TIME);
                            break;
                        }
                        else
                        {   closesocket(sock2);
                            if (current_users < 0) current_users = 0;
                            continue;
/*
 * This loops serving as many clients as happen to come along. Having said
 * "csl -fnnn" it will be necessary (in due course) to kill the daemon
 * by interrupting it with a ^C or some such. When the master process is
 * terminated in that way any clients that remain active may continue to
 * hang around until they have finished in the usual way.
 */
                        }
#endif /* ms_windows */
                    }
                }
/*
 * The "continue" here gets executed when I have been contacted by some
 * client and have an active socket open. It parses the rest of the
 * command line and then completes the process of getting CSL running.
 */
                continue;
#endif


/*
 *                      -G
 * is a debugging option - it sets !*backtrace to true, which applications
 * may inspect when they want to do errorsets etc.  These days I will
 * make it FORCE all errors to be noisy whatever the user tries to do!
 */
        case 'g':
                if (number_of_symbols_to_define < MAX_SYMBOLS_TO_DEFINE)
                    symbols_to_define[number_of_symbols_to_define] =
                        "*backtrace",
                    undefine_this_one[number_of_symbols_to_define++] = NO;
                else
                {
#ifdef CWIN
                    cwin_maximize();
#endif
                    term_printf("Too many requests: \"-G\" ignored\n");
                }
                always_noisy = YES;
                continue;

/*
 * -I is used to specify an image file to be used when CSL starts up.
 * The case -I- indicated the "standard" file associated with this
 * executable binary.  Several images can be given.
 */
        case 'i':
                if (c2 != 0) w = &opt[2];
                else if (i != argc) w = argv[++i];
                else break; /* Illegal at end of command-line */
                if (w[0] == '-' && w[1] == 0) w = standard_directory;
                if (number_of_fasl_paths < MAX_FASL_PATHS-1)
                    fasl_paths[number_of_fasl_paths++] = w;
                else
                {
#ifdef CWIN
                    cwin_maximize();
#endif
                    term_printf("Too many \"-I/-O\" requests: ignored\n");
                }
                continue;

/*
 * -K nnn sets the size of heap to be used.  If it is given then that much
 * memory will be allocated and the heap will never expand.  Without this
 * option a default amount is used, and (on many machines) it will grow
 * if space seems tight.
 * The extended version of this option is "-K nnn/ss" and then ss is the
 * number of "CSL pages" to be allocated to the Lisp stack. The default
 * value (which is 1) should suffice for almost all users, and it should
 * be noted that the C stack is separate from and independent of this one and
 * it too could overflow.
 */
        case 'k':
                if (c2 != 0) w = &opt[2];
                else if (i != argc) w = argv[++i];
                else break; /* Illegal at end of command-line */
                {   char buffer[16];
                    int i = 0;
                    while ((*w != '/') && (*w != 0) && (i<sizeof(buffer)-1))
                        buffer[i++] = *w++;
                    buffer[i] = 0;
/*
 * store size gets set here - 0.0 is left if either that is specified
 * explictly or if no -K option is given.
 */
                    store_size = atof(buffer);
                    if (store_size == 0.0) init_flags |= INIT_EXPANDABLE;
                    else
                    {   init_flags &= ~INIT_EXPANDABLE;
/* here I impose a minimum store-size... 32 Mbytes! */
                        if (store_size < 32000) store_size = 32000;
                    }
                    if (*w == '/')
                    {   stack_segsize = atoi(w+1);
                        if (stack_segsize < 1 || stack_segsize > 10)
                            stack_segsize = 1;
                    }
                }
                continue;

/*
 * -L <logfile> arranges that a transcript of the standard output is
 * sent to the given file, just as if (spool '<logfile>) had been executed
 * at the start of the run.
 */
        case 'l':
                if (c2 != 0) w = &opt[2];
                else if (i != argc) w = argv[++i];
                else break; /* Illegal at end of command-line */
                {   char filename[LONGEST_LEGAL_FILENAME];
                    spool_file = open_file(filename, w,
                           strlen(w), "w", NULL);
                    if (spool_file == NULL)
                    {
#ifdef CWIN
                        cwin_maximize();
#endif
                        term_printf("Unable to write to \"%s\"\n", filename);
                    }
                    else
                    {   time_t t0 = time(NULL);
                        strncpy(spool_file_name, filename, 32);
                        spool_file_name[31] = 0;
#ifdef COMMON
                        fprintf(spool_file, 
                            "Starts dribbling to %s (%.24s).\n",
                            spool_file_name, ctime(&t0));
#else
                        fprintf(spool_file, 
                            "+++ Transcript to %s started at %.24s +++\n",
                            spool_file_name, ctime(&t0));
#endif
                    }
                }
                continue;

#ifdef MEMORY_TRACE
/*
 * If MEMORY_TRACE is set up then I can cause an exception by providing
 * an option -M n:l:h 
 * This interrupts after n memory records when a reference in the (inclusive)
 * range l..h is next made.
 */
        case 'm':
                if (c2 != 0) w = &opt[2];
                else if (i != argc) w = argv[++i];
                else break; /* Illegal at end of command-line */
                switch(sscanf(w, "%ld:%lu:%lu",
                               &car_counter, &car_low, &car_high))
                {
            case 0: car_counter = 0x7fffffff;
            case 1: car_low = 0;
            case 2: car_high = 0xffffffff;
            default:break;
                }
                continue;
#endif
/*
 * -N tells CSL that even if the image being loaded contains a restart-
 * function this should be ignored, and Lisp should run the default
 * read-eval-print loop. The only expected use for this is when an image
 * has been created but it is seriously broken, so the way it would
 * usually restart would crash - then "-N" may allow a suitable expert to
 * test and diagnose the trouble at the Lisp level. Ordinary users are
 * NOT expected to want to know about this!
 */
        case 'n':               /* Ignore restart function (-N) */
                ignore_restart_fn = YES;
                continue;
/*
 * -O <file>  specifies an image file for output (via FASLOUT or PRESERVE).
 */
        case 'o':
                if (c2 != 0) w = &opt[2];
                else if (i != argc) w = argv[++i];
                else break; /* Illegal at end of command-line */
                if (w[0] == '-' && w[1] == 0) w = standard_directory;
                if (number_of_fasl_paths < MAX_FASL_PATHS-1)
                {   output_directory = number_of_fasl_paths;
                    fasl_paths[number_of_fasl_paths++] = w;
                }
                else
                {
#ifdef CWIN
                    cwin_maximize();
#endif
                    term_printf("Too many \"-I/-O\" requests: ignored\n");
                }
                continue;

/*
 * -P is reserved for profile options.
 */
        case 'p':
/*
 * Please implement something for your favourite system here... what I would
 * like would be a call to monitor() or some such...
 */
#ifdef CWIN
                cwin_maximize();
#endif
                term_printf("Unimplemented option \"-%c\"\n", c1);
                continue;

/*
 * -Q selects "quiet" mode.  See -V for the converse.
 */
        case 'q':
                if (number_of_symbols_to_define < MAX_SYMBOLS_TO_DEFINE)
/*
 *                  symbols_to_define[number_of_symbols_to_define] =
 *                      "*echo=nil",
 *                  undefine_this_one[number_of_symbols_to_define++] = NO,
 */
                    init_flags &= ~INIT_VERBOSE,
                    init_flags |= INIT_QUIET;
                else
                {
#ifdef CWIN
                    cwin_maximize();
#endif
                    term_printf("Too many requests: \"-Q\" ignored\n");
                }
                continue;

/*
 * -R nnn   sets the initial random seed, for reproducible runs.  -R 0
 * (the default) sets the initial seed based on the time of day etc.
 * The version -R  nnn,mmm makes it possible to pass 64-bits of seed info.
 */
        case 'r':
                if (c2 != 0) w = &opt[2];
                else if (i != argc) w = argv[++i];
                else break; /* Illegal at end of command-line */
                if (sscanf(w, "%ld,%ld", &initial_random_seed, &seed2) != 2)
                {   initial_random_seed = seed2 = 0;
                    sscanf(w, "%ld", &initial_random_seed);
                }
                continue;

/*
 * -S  sets the variable !*plap, which causes the compiler to list the
 * bytecodes that it generates.
 */
        case 's':
                if (number_of_symbols_to_define < MAX_SYMBOLS_TO_DEFINE)
                    symbols_to_define[number_of_symbols_to_define] =
                        "*plap",
                    undefine_this_one[number_of_symbols_to_define++] = NO;
                else
                {
#ifdef CWIN
                    cwin_maximize();
#endif
                    term_printf("Too many requests: \"-S\" ignored\n");
                }
                continue;
/*
 * -T name     reports the time-stamp on the named module, and then
 *             exits. This is for use in perl scripts and the like, and is
 *             needed because the stamps on modules within an image or
 *             library file are not otherwise instantly available.
 *
 *             Note that especially on windowed systems it may be
 *             necessary to use this with "-- filename" since the information
 *             generated here goes to the default output unit, which in
 *             some cases is just the screen.
 */
        case 't':
                if (c2 != 0) w = &opt[2];
                else if (i != argc) w = argv[++i];
                else break; /* Illegal at end of command-line */
                module_enquiry = w;
                continue;

/*
 * -U name     undefines the symbol <name> at the start of the run
 */
        case 'u':
                if (c2 != 0) w = &opt[2];
                else if (i != argc) w = argv[++i];
                else break; /* Illegal at end of command-line */
                if (number_of_symbols_to_define < MAX_SYMBOLS_TO_DEFINE)
                    symbols_to_define[number_of_symbols_to_define] = w,
                    undefine_this_one[number_of_symbols_to_define++] = YES;
                else
                {
#ifdef CWIN
                    cwin_maximize();
#endif
                    term_printf("Too many \"-U\" requests: ignored\n");
                }
                continue;
/*
 * -V selects "verbose" options at the start of the run
 */
        case 'v':
                if (number_of_symbols_to_define < MAX_SYMBOLS_TO_DEFINE)
/*
 *                  symbols_to_define[number_of_symbols_to_define] =
 *                      "*echo",
 *                  undefine_this_one[number_of_symbols_to_define++] = NO,
 */
                    init_flags &= ~INIT_QUIET,
                    init_flags |= INIT_VERBOSE;
                else
                {
#ifdef CWIN
                    cwin_maximize();
#endif
                    term_printf("Too many requests: \"-V\" ignored\n");
                }
                continue;
    
#ifdef WINDOW_SYSTEM
/*
 * For the Archimedes I insist on a command-line option "-w" to enable
 * use of the windowed version of the code.  For other window systems I
 * reserve the option for fine control over window behaviour.
 */
        case 'w':
/*
 * I need to detect and process this flag especially early, and so by the time
 * I get to regular command decoding there is nothing to be done.
 */
                continue;
#endif

/*
 * -x is an "undocumented" option intended for use only by system
 * support experts - it disables trapping if segment violations by
 * errorset and so makes it easier to track down low level disasters -
 * maybe!  Only those who have access to the source code can make
 * good use of the -X option, so it is only described here!
 */
        case 'x':
                segvtrap = NO;
                continue;
/*
 * -Y  sets the variable !*hankaku , which causes the lisp reader convert
 * a Zenkaku code to Hankaku one when read. I leave this option decoded
 * on the command line even if the Kanji support code is not otherwise
 * compiled into CSL just so I can reduce conditional compilation.
 */
        case 'y':
                if (number_of_symbols_to_define < MAX_SYMBOLS_TO_DEFINE)
                    symbols_to_define[number_of_symbols_to_define] =
                        "*hankaku",
                    undefine_this_one[number_of_symbols_to_define++] = NO;
                else
                    term_printf("Too many requests: \"-Y\" ignored\n");
                continue;

/*
 * -Z tells CSL that it should not load an initial heap image, but should
 * run in "cold start" mode.  This is only intended to be useful for
 * system builders.
 */
        case 'z':               /* Cold start option -z */
                restartp = NO;
                continue;

        default:
#ifdef CWIN
                cwin_maximize();
#endif
                term_printf("Unrecognized option \"-%c\"\n", c1);
                continue;
            }
/*
 * I do a "break" out of the switch() block if a key occurs at the end
 * of the command line in an invalid manner.
 */
#ifdef CWIN
            cwin_maximize();
#endif
            term_printf("Option \"-%c\" needs an argument: ignored\n", c1);
            break;
        }
        else files_to_read[number_of_input_files++] = opt;
    }

    if (number_of_fasl_paths == 0)
    {   char *p = standard_directory, *p1;
        char cur[LONGEST_LEGAL_FILENAME];
/*
 * If the user does not specify any image files then the behaviour
 * defaults as follows:
 *   Suppose that the current executable is xxx/yyy/zzz then the
 * system behaves as if the user had written
 *    zzz -o zzz.img -i xxx/yyy/zzz.img
 * however if the executable seemed to be in the current directory
 * already this is reduced to just
 *    zzz -o zzz.img
 * so that I do not have two different handles on the same image file
 * (with the potential muddle that that could result in).
 *
 * NOTE: this used very generally mean that you ended up with an empty image
 * file (eg csl.img or r37.img) in whatever directory you run this
 * code from. This could be avoided by running it as
 *    xxx -i-
 * that explicitly sets up the normal image file as the one to use with
 * no extras. However these days I try to arrange that an output image file
 * only ever gets created if somebody calls FASLOUT or PRESERVE, so what
 * I describe here will usually not cause confusion....
 */
        if (standard_directory[0] == '.' &&
            (standard_directory[1] == '/' ||
             standard_directory[1] == '\\')) strcpy(cur, standard_directory);
        else get_current_directory(cur, sizeof(cur));
        p += strlen(p);
        while (p != standard_directory &&
               *--p != '/' &&
               *p != '\\') /* nothing */;
        if (strncmp(standard_directory, cur, p-standard_directory) != 0)
            p1 = (char *)(*malloc_hook)(strlen(p));
        else p1 = NULL;
        if (p == standard_directory || p1 == NULL)
        {   fasl_paths[0] = standard_directory;
/*
 * If output_directory has the 0x40000000 bit set then the directory
 * involved is one that should be opened now if it exists, but if
 * it does not its creation should be deferred for as long as possible.
 */
            output_directory = 0x40000000 + 0;
            number_of_fasl_paths = 1;
            if (p1 != NULL) (*free_hook)(p1);
        }
        else
        {   strcpy(p1, p+1);
            fasl_paths[0] = p1;
            fasl_paths[1] = standard_directory;
            output_directory = 0x40000000 + 0;
            number_of_fasl_paths = 2;
        }
    }

    Iinit();

#ifdef WINDOW_SYSTEM
#ifdef RISCOS
/*
 * Under RISCOS I must delay starting up the window manager until about
 * here for two reasons.  Firstly a command-line option might have been
 * used to decide whether or not it was wanted after all.  Secondly a
 * side effect of find_image_directory() under RISCOS is the discovery
 * of the name that this application is being run as, and I want that to
 * go as the title in my window.
 */
    start_up_window_manager(use_wimp);
#endif
#endif

    if (module_enquiry != NULL)
    {   char datestamp[32], fullname[LONGEST_LEGAL_FILENAME];
        int32 size;
        int i;
        Lisp_Object nil;
/*
 * Imodulep expects input_libraries to be set up. So I will fudge the
 * creation of something that looks sufficiently like a list to pass muster
 * here despite the full system not being loaded. I use references to the
 * nil-segment and cons().
 */

        nilsegment = (Lisp_Object *)my_malloc(NIL_SEGMENT_SIZE);
#ifdef COMMON
        nil = doubleword_align_up(nilsegment) + TAG_CONS + 8;
#else
        nil = doubleword_align_up(nilsegment) + TAG_SYMBOL;
#endif
        C_nil = nil;
        pages_count = heap_pages_count = vheap_pages_count =
            bps_pages_count = native_pages_count = 0;
        stacksegment = (Lisp_Object *)my_malloc(CSL_PAGE_SIZE);
/*
 * I am lazy about protection against malloc failure here.
 */
        heaplimit = doubleword_align_up(stacksegment);
        fringe = heaplimit + CSL_PAGE_SIZE - 16;
        input_libraries = heaplimit + 16 + TAG_SYMBOL;
        heaplimit += 64;
/*
 * I have now fudged up enough simulation of a Lisp heap that maybe I can
 * build the library search-list.
 */
        qheader(input_libraries)  |= SYM_SPECIAL_FORM;
        qvalue(input_libraries) = nil;
        for (i=number_of_fasl_paths-1; i>=0; i--)
            qvalue(input_libraries) = cons(SPID_LIBRARY + (((int32)i)<<20),
                                           qvalue(input_libraries));

        if (Imodulep(module_enquiry, strlen(module_enquiry),
                     datestamp, &size, fullname))
        {   strcpy(datestamp, "unknown");
            size = 0;
            strcpy(fullname, module_enquiry);
        }
        term_printf("%.24s   size=%ld file=%s\n",
                    datestamp, (long)size, fullname);
        init_flags &= ~INIT_VERBOSE;
#ifdef CWIN
        cwin_pause_at_end = 0;
#endif
    }
    else
    {   base_time = read_clock();
        consolidated_time[0] = gc_time = 0.0;
        clock_stack = &consolidated_time[0];
        push_clock();

        if (init_flags & INIT_VERBOSE)
        {
#ifndef WINDOW_SYSTEM
/*
 * If I do NOT have a window system I will print a newline here so that I
 * can be very certain that my banner appears at the start of a line.
 * With a window system I should have a brand-new frash window for output
 * and the newline would intrude as an initial blank line.
 */
            term_printf("\n");
#endif
#ifndef COMMON
            term_printf("Codemist Standard Lisp %s for %s: %s\n",
                     VERSION, IMPNAME, __DATE__);
#else
            term_printf("Codemist Common Lisp %s for %s: %s\n",
                     VERSION, IMPNAME, __DATE__);
#endif
        }
#ifdef MEMORY_TRACE
        if (car_counter != 0x7fffffff)
            term_printf("Stop after %ld %lu..%lu\n",
                        car_counter, car_low, car_high);
#endif
#ifdef WINDOW_SYSTEM
        ensure_screen();
/* If the user hits the close button here I may be in trouble */
#endif

/*
 * Here (potentially) I could set qualifier information into native_code_tag
 * based on run-time checks. This might allow me to discriminate between
 * systems that are exactly the same as far as the CSL-controlled bits of C
 * code are concerned but have extra differences that a bit of system-specific
 * code here can detect. I will reserve the bits 0xe0 (3 bits) for such
 * information. Note that a system that does not support hard code MUST leave
 * hard_code_tag set to zero.
 */
        native_code_tag = NATIVE_CODE_TAG;
/*
 * Now dynamic code detects the floating point representation that is in use.
 * I thougt/hoped that doing it this way would be safer than relying on having
 * pre-defined symbols that tracked the machine architecture.
 */
        {   union fpch { double d; unsigned char c[8]; } d;
/*
 * The following looks at the floating point representation of the
 * number 1/7 (in double precision) and picks out two bytes from
 * the middle of the first word - where I hope that rounding issues
 * will be remote.  Investigation shows that these two bytes can be
 * used to discriminate among at least a worthwhile range of
 * representations, and I will exploit this to help me re-load
 * heap-images in a way that allows images to be portable across
 * different architectures.
 */
            d.d = 1.0/7.0;
            switch ((d.c[1] << 8) | d.c[2])
            {
        case 0x49c2:    current_fp_rep = FP_ARM;    break;
        case 0x2492:    current_fp_rep = FP_370;    break;
        case 0x2449:    current_fp_rep = FP_MIPS;   break;
        case 0x3f24:    current_fp_rep = FP_VAX;    break;
        case 0xc249:    current_fp_rep = FP_88K;    break;
/*
 * The next line is probably not very good under a window manager, but
 * it is a case that ought never to arise, so I will not bother.
 */
        default:        term_printf("Unknown floating point format\n");
                        my_exit(EXIT_FAILURE);
            }
        }

/*
 * Up until the time I call setup() I may only use term_printf for
 * output, because the other relevant streams will not have been set up.
 */
        setup(restartp ? 3 : 2, store_size);
/*
 * I need to set the NOISY flag after doing setup to avoid it getting
 * reloaded from a heap image
 */
        {   nil_as_base
            if (always_noisy) miscflags |= (ALWAYS_NOISY | 3);
	    else miscflags &= ~ALWAYS_NOISY;
        }

#ifndef COMMON
#ifdef CWIN
/*
 * Note that it may make sense to enable this for windowed versions other
 * than CWIN-based ones...
 */
        cwin_menus(loadable_packages, switches);
#endif
#endif

/*
 * Now that setup is complete (and I have done any authorisation I want to)
 * I will seed the random number generator as requested by the user. The
 * default will be to put it in an unpredictable (well hard to predict!)
 * state
 */
        Csrand((unsigned32)initial_random_seed, (unsigned32)seed2);

        gc_time += pop_clock();

        interrupt_pending = already_in_gc = NO;
        polltick_pending = tick_pending = tick_on_gc_exit  = NO;

        sigint_must_longjmp = NO;
        signal(SIGINT, sigint_handler);
#ifdef TICK_STREAM
/*
 * Now I have enough in place that I am prepared to accept regular clock-
 * tick events.
 */
        add_ticker();
/*
 *  atexit(remove_ticker);  Maybe "atexit() is itself dangerous!
 *                          I should try (quite hard) to disable the
 *                          ticker at some earlier safer stage.
 */
#endif
        ensure_screen();
        procedural_output = NULL;
#ifdef CWIN
/*
 * OK, if I get this far I will suppose that any messages that report utter
 * disasters will have reached the user, so I can allow CWIN to terminate
 * rather more promptly.
 */
        cwin_pause_at_end = 0;
#endif
    }
}

#ifdef SOCKETS

#define SOCKET_BUFFER_SIZE 1024
#define CH_PROMPT          0x9a
#define CH_ENDPROMPT       0x9c

static char socket_in[SOCKET_BUFFER_SIZE], socket_out[SOCKET_BUFFER_SIZE];
static int socket_in_p = 0, socket_in_n = 0,
           socket_out_p = 0, socket_prev = '\n';

static int char_from_socket(void)
{
    int c;
    clock_t c0;
    time_t t0;
    if (socket_server == 0)
    {   socket_prev = ' ';
        return EOF;
    }
/*
 * I generate a prompt whenever I am about to read the character that
 * follows a newline. The prompt is issued surrounded by control
 * characters 0x9a and 0x9c. That curious arrangement is inherited from
 * internal behaviour in my Windows interface code and could be altered
 * if something truly better could be invented.
 */
    if (socket_prev == '\n')
    {   term_printf("%c%s%c", CH_PROMPT, prompt_string, CH_ENDPROMPT);
        ensure_screen();
    }
    if (socket_in_n == 0)
    {   for (;;)
        {   socket_in_n = recv(socket_server, socket_in, SOCKET_BUFFER_SIZE, 0);
            c0 = clock();
            t0 = time(NULL);
            if (c0 > cpu_timeout || t0 > elapsed_timeout)
            {    cpu_timeout = c0 + 20;
                 elapsed_timeout = t0 + 20;
                 term_printf(
                    "\nSorry: timeout on this session. Closing down.\n");
                 socket_prev = ' ';
                 return EOF;
            }
            if (socket_in_n <= 0)
#ifndef EWOULDBLOCK
#  define EWOULDBLOCK WSAEWOULDBLOCK
#endif
            {   if (errno == EWOULDBLOCK)
                {
#ifdef _MSC_VER
                    Sleep(300);  /* Arg in milliseconds here */
#else
                    sleep(1);  /* Delay 1 second before re-polling */
#endif
                    continue;
                }
                closesocket(socket_server);
                socket_server = 0;
                socket_prev = ' ';
                return EOF;
            }
            else break;
        }
        socket_in_p = 0;
    }
    c = socket_in[socket_in_p++];
    if (c == 0x0a || c == 0x0d) c = '\n';
    socket_in_n--;
    socket_prev = c;
    return c & 0xff;
}

static int char_to_socket(int c)
{
    if (socket_server == 0) return 1;
    socket_out[socket_out_p++] = (char)c;
    if (c == '\n' || socket_out_p == SOCKET_BUFFER_SIZE)
    {   if (send(socket_server, socket_out, socket_out_p, 0) < 0)
        {   closesocket(socket_server);
            socket_server = 0;
            return 1;
        }
        socket_out_p = 0;
    }
    return 0;
}

void flush_socket(void)
{
    if (socket_server == 0) return;
    if (send(socket_server, socket_out, socket_out_p, 0) < 0)
    {   closesocket(socket_server);
        socket_server = 0;
    }
    socket_out_p = 0;
}

#endif

static void cslaction(void)
/*
 * This is the "standard" route into CSL activity - it uses file-names
 * from the decoded command-line as files to be read and processed
 * unless the system was launched with the flag that says it ought to try
 * to provide a network service on some socket.
 */
{
#ifdef __cplusplus
    errorset_msg = NULL;
    try
#else
    jmp_buf this_level;
    errorset_buffer = &this_level;
    errorset_msg = NULL;
    if (!setjmp(this_level))
#endif
    {   signal(SIGFPE, low_level_signal_handler);
#ifdef __WATCOMC__
        _control87(_EM_OVERFLOW | _EM_INVALID | _EM_DENORMAL |
                   _EM_ZERODIVIDE | _EM_INEXACT | _EM_UNDERFLOW,
                   _MCW_EM);
#endif
        if (segvtrap) signal(SIGSEGV, low_level_signal_handler);
#ifdef SIGBUS
        if (segvtrap) signal(SIGBUS, low_level_signal_handler);
#endif
#ifdef SIGILL
        if (segvtrap) signal(SIGILL, low_level_signal_handler);
#endif
        non_terminal_input = NULL;
#ifdef SOCKETS
        if (socket_server)
        {   ensure_screen();
            procedural_input = char_from_socket;
            procedural_output = char_to_socket;
            lisp_main();
            ensure_screen();
            procedural_input = NULL;
            procedural_output = NULL;
        }
        else
#endif
        if (number_of_input_files == 0) lisp_main();
        else
        {   int i;
            for (i=0; i<number_of_input_files; i++)
            {   char filename[LONGEST_LEGAL_FILENAME];
                FILE *f = open_file(filename, files_to_read[i],
                                            strlen(files_to_read[i]), "r", NULL);
                if (f == NULL)
                    err_printf("\n+++ Could not read file \"%s\"\n",
                               files_to_read[i]);
                else
                {   nil_as_base
                    if (init_flags & INIT_VERBOSE)
                        term_printf("\n+++ About to read file \"%s\"\n",
                                    files_to_read[i]);
                    non_terminal_input = f;
                    lisp_main();
                    fclose(f);
                }
            }
        }
    }
#ifdef __cplusplus
    catch (char *)
#else
    else
#endif
    {   if (errorset_msg != NULL)
        {   term_printf("\n%s detected\n", errorset_msg);
            errorset_msg = NULL;
        }
        return;
    }
}

int cslfinish(character_writer *w)
{
    procedural_output = w;
    if (Ifinished())
        term_printf("\n+++ Errors on checkpoint-image file\n");
#ifdef TRACED_EQUAL
    dump_equals();
#endif
/*
 * clock_t is an arithmetic type but I do not know what sort - so I
 * widen to double to do arithmetic on it. Actually what I MUST do is
 * to compute a time difference in the type clock_t and hope I never
 * get a difference that that overflows. The worst case I know of overflows
 * after 35 minutes.
 */
    if (init_flags & INIT_VERBOSE)
    {   long int t = (long int)(100.0 * (consolidated_time[0] +
                                 (double)(read_clock() - base_time)/
                                 (double)CLOCKS_PER_SEC));
        long int gct = (long int)(100.0 * gc_time);
        term_printf("\n\nEnd of Lisp run after %ld.%.2ld+%ld.%.2ld seconds\n",
                 t/100, t%100, gct/100, gct%100);
    }
#ifdef TICK_STREAM
#ifdef SOFTWARE_TICKS
#ifdef DEBUG_SOFTWARE_TICKS
    term_printf("%d ticks processed (%d)\n",
                number_of_ticks, SOFTWARE_TICKS);
#endif
#endif
    remove_ticker();
#endif
    drop_heap_segments();
    if (spool_file != NULL) 
    {
#ifdef COMMON
        fprintf(spool_file, "\nFinished dribbling to %s.\n", spool_file_name);
#else
        fprintf(spool_file, "\n+++ Transcript closed at end of run +++\n");
#endif
#ifndef _DEBUG
        fclose(spool_file);
        spool_file = NULL;
#endif
    }
    ensure_screen();
    procedural_output = NULL;
    return return_code;
}

/*
 * People who want to use this in an embedded context can predefine
 * NO_STARTUP_CODE and provide their own entrypoint...
 */

#ifndef NO_STARTUP_CODE

/*
 * The next fragment of code is to help with the use of CSL (and hence
 * packages written in Lisp and supported under CSL) as OEM products
 * embedded within larger C-coded packages.  There is (of course) a
 * significant issue about clashes between the names of external symbols
 * if CSL is to be linked with anything else, but I will not worry about that
 * just yet.
 * The protocol for calling Lisp code from C is as follows:
 *
 *     cslstart(argc, argv, writer);allocate memory and Lisp heap etc. Args
 *                                  should be "as if" CSL was being called
 *                                  directly and this was the main entrypoint.
 *                                  The extra arg accepts output from this
 *                                  stage.  Use NULL to get standard I/O.
 *     execute_lisp_function(fname, reader, writer);
 *                                  fname is a (C) string that names a Lisp
 *                                  function of 0 args.  This is called with
 *                                  stdin/stdout access redirected to use the
 *                                  two character-at-a-time functions passed
 *                                  down.  [Value returned indicates if
 *                                  the evaluation succeeded?]
 *     cslfinish(writer);           Tidies up ready to stop.
 */
 
int execute_lisp_function(char *fname,
    character_reader *r, character_writer *w)
{
    Lisp_Object nil;
    Lisp_Object ff = make_undefined_symbol(fname);
    nil = C_nil;
    if (exception_pending())
    {   flip_exception();
        return 1;  /* Failed to make the symbol */
    }
    procedural_input = r;
    procedural_output = w;
    Lapply0(nil, ff);
    ensure_screen();
    procedural_input = NULL;
    procedural_output = NULL;
    nil = C_nil;
    if (exception_pending())
    {   flip_exception();
        return 2;  /* Failure during evaluation */
    }
    return 0;
}

#ifdef SAMPLE_OF_PROCEDURAL_INTERFACE

static char ibuff[100], obuff[100];
static int ibufp = 0, obufp = 0;
static int iget()
{
    int c = ibuff[ibufp++];
    if (c == 0) return EOF;
    else return c;
}

static void iput(int c)
{
    if (obufp < sizeof(obuff)-1)
    {   obuff[obufp++] = c;
        obuff[obufp] = 0;
    }
}

#endif

static int submain(int argc, char *argv[])
{
    cslstart(argc, argv, NULL);
#ifdef FLEX
    {
      extern void init_lm();
      init_lm();
    }
#endif
#ifdef SAMPLE_OF_PROCEDURAL_INTERFACE
    strcpy(ibuff, "(print '(a b c d))");
    execute_lisp_function("oem-supervisor", iget, iput);
    printf("Buffered output is <%s>\n", obuff);
#else
    if (module_enquiry == NULL) cslaction();
#endif
    my_exit(cslfinish(NULL));
/*
 * The "return 0" here is unreachable but it still quietens down as many
 * C compilers as it causes to moan noisily!
 */
    return 0;
}

#if !defined(WINDOWS_NT) || defined(CWIN) || !defined(NAG)

#ifdef CWIN
#define ENTRYPOINT cwin_main
#else
#define ENTRYPOINT main
#endif


int ENTRYPOINT(int argc, char *argv[])
{
    int res;
#if defined(FLEX) && !defined(WINDOWS_NT)
    extern void init_lm();
    init_lm();
#endif
#ifdef USE_MPI
    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    printf("I am mpi instance %d of %d.\n", mpi_rank+1, mpi_size);
#endif

#ifdef CWIN
#ifdef NAG
    strcpy(about_box_title, "About AXIOM for Windows");
    strcpy(about_box_description, "CWIN interface");
    strcpy(about_box_rights_1,"Copyright NAG Ltd.");
    strcpy(about_box_rights_2,"1991-6");
#else
    strcpy(about_box_title, "About CSL");
    strcpy(about_box_description, "Codemist Standard Lisp");
#endif
#endif
#ifdef __cplusplus
    try { res = submain(argc, argv); }
    catch(int r) { res = r; }
#else
    res = setjmp(my_exit_buffer);
    if (res == 0)  res = submain(argc, argv);
    if (res == 0x80000000) res = 0;
#endif
#ifdef TICK_STREAM
    remove_ticker();
#endif
    return res;
#ifdef USE_MPI
    MPI_Finalize();
#endif
}

#endif /* NAG */
#endif /* NO_STARTUP_CODE */

/* End of csl.c */
