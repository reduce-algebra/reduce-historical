@Device(lpt)
@style(justification yes)
@style(spacing 1)
@use(Bibliography "<griss.docs>mtlisp.bib")
@make(article)
@modify(enumerate,numbered=<@a. @,@i. >, spread 1)
@modify(appendix,numbered=<APPENDIX @A: >)
@modify(itemize,spread 1)
@modify(description,leftmargin +2.0 inch,indent -2.0 inch)
@define(up,use text,capitalized on,  break off)
@define(mac,use text, underline off,  break off)
@define(LISPmac,use text, underline alphanumerics,  break off)
@pageheading(Left  "Utah Symbolic Computation Group",
             Right "December 1981", 
             Line "Operating Note 60"
            )
@set(page=1)
@newpage()
@begin(titlepage)
@begin(titlebox)
@b(A PASCAL Based Standard LISP for the Apollo Domain)
@center[
by

M. L. Griss and R. Ottenheimer

Department of Computer Science
University of Utah
Salt Lake City, Utah 84112

@b(Preliminary  Version)

Last Revision: @value(date)]

@end(titlebox)
@begin(abstract)
This report describes an interim implementation of Standard LISP for the
Apollo DOMAIN. This LISP is based upon the Standard LISP report, and a
newly developing Portable Standard LISP.  This interim implementation is
designed to explore LISP implementations in PASCAL on the Apollo DOMAIN and
similar machines.  The system consists of a kernel, handcoded in PASCAL,
with the rest of the system written in LISP and compiled to PASCAL.
@End(abstract)
@begin(Researchcredit)
Work supported in part by the National Science Foundation
under Grant No. MCS80-07034.
@end(Researchcredit)
@end(titlepage)
@pageheading(Left "Apollo Pascal LISP",Center "@value(date)",
             Right "@value(Page)"
            )
@set(page=1)
@newpage
@section(Introduction)
In this preliminary report, we describe an implementation of Standard LISP
in PASCAL, PASLSP. Versions of PASLSP have been run on a number of
machines, ranging from an LSI-11 based TERAK to Apollo and PERQ. This report
concentrates on the Apollo DOMAIN implementation. This report is to be read in
conjunction with the Standard LISP report@cite(Marti79); we will
highlight the differences from the functions documented in the Standard
LISP, describe the implementation strategy, and discuss future work.

PASLSP is based on a series of small and medium sized LISP interpreters
that have been developed at the University of Utah to explore LISP
implementations in higher level languages. Each of these LISP systems
consists of a small kernel handcoded in some language, with the rest of the
system written in LISP and compiled to the target language.  We have used
FORTRAN, PASCAL and assembly language as targets. The PASLSP series use
PASCAL for the kernel, and have a LISP to PASCAL compiler for the rest of
the system. 

Recent work has concentrated on reducing the size of the hand-coded kernel,
and extending the compiler to handle systems level constructs. This has
resulted in a new Portable Standard LISP, PSL, running on the DEC-20 and
VAX-11/750@cite(Benson81,Griss81). An implementation of PSL for MC68000 is
underway. The PSL system is a modern, efficient LISP, written entirely in
itself; it uses an efficient LISP to machine code compiler to produce the
kernel, and then the rest of LISP is loaded. In the future we hope to
produce a complete PSL targeted at a higher level languages, such as
PASCAL, C or ADA, and this will replace the current PASLSP.

@subsection(History of PASLSP)
The system now called PASLSP was originally developed (by M. Griss and W.
Galway), as a small LISP like kernel to support a small computer algebra
system on an LSI-11 TERAK; this was to be used as an answer analysis module
within a CAI system@cite(Brandt81), written entirely in PASCAL. It was
decided to hand-code a very small kernel, and compile additional functions
written in LISP (LISP support functions, parser and
simplifier) to PASCAL,
using a modified Portable LISP compiler@cite(griss79). This version (call
it V0) did not even have user defined functions, since space on the TERAK
was at a premium.

About June 1981, PASLSP came to the attention of a number people evaluating
Apollo's and PERQ's, and it was suggested that we enhance V0 PASLSP for
this purpose. During the space of a few days, features taken from the
Standard LISP Report and newly developing PSL files were added to produce
PASLSP-V1, running on a DEC-20 and Terak. This was a fairly complete LISP
(including Catch and Throw), but lacked a few features (OPEN, CLOSE, RDS,
WRS, PROG, GO, RETURN, COMPRESS, EXPLODE, Vectors and Strings, etc.).  V1
PASLSP was adapted to a PERQ, VAX and Apollo by Paul Milazo of Schlumberge
in the space of a few weeks (we did not have a PERQ or Apollo at that
time).

We subsequently obtained a PERQ and an Apollo, and recent work has been
aimed at producing an enhanced PASLSP for these machines, maintaining all
versions in one set of source files.  The current system, PASLSP-V2, is
produced from a single PASCAL kernel and set of LISP support files; the
machine specific features are handled by a simple Source Code
Conditionalizer, changing the definition of certain constants and data
types. Only a few features of the Standard LISP report are missing,
and there are a number of additions.

@subsection(Acknowledgement)

We would like to acknowledge the contributions and support of
Eric Benson, Dick Brandt, Will Galway,   and Paul Milazo.

@section(Features of PASLSP and relation to Standard LISP)
PASLSP as far as possible provides all the functions mentioned
in the attached Standard LISP Report (note the hand-written
comments added to this appendix); some of the functions are simply
stubs, so that a Standard LISP Test-file can be run without major
modification.

PASLSP-V2  does not implement the following features of Standard LISP:
@begin(enumeration,spread 0)
VECTORS (only a simple garbage collector is used).

Strings are implemented as identifiers (not garbage collected).

Integers are limited in size (INTs and FIXNUMs, no BIGNUMs).

FLOATING Point is not implemented.

IDs can not be REMOB'ed or INTERN'd.

Only 3 Input Channels and 2 Output Channels are available to OPEN,
RDS, WRS, and CLOSE. Thus file input statements can not be nested
very deeply in files.

Line, Page and Character counting (POSN, LPOSN, etc) are not implemented.
@end(enumeration)

PASLSP-V2 provides some extensions over Standard LISP:
@begin(enumerate,spread 0)
(CATCH form) and (THROW form) and the tagged versions: (TCATCH tag form)
and (TTHROW tag form) are used to implement error and errorset, 
and higher level control functions.

Implicit PROGN in COND, and LAMBDA expressions.

(WHILE pred action-1 action-2 ... action-n).

(DSKIN 'filename) or (DSKIN "filename")
@end(enumerate)

PASLSP-V2 has not been extensively tested, and there may still be a number
of bugs. While some effort has been spent in adjusting PASLSP to the Apollo
DOMAIN, it is clear that the various heap sizes are not yet optimal. 
See appendix A for current list of functions, and appendix B for a copy
of the Standard LISP Report annotated to reflect the current status of 
PASLSP.

@section(Using PASLSP on the Apollo DOMAIN)
	Initializing the system from the floppy looks like this:
@begin(verbatim)
Create a directory (call it pl):
	crd /pl
Mount the floppy:
	mtvol f 1 /f
Copy the files of interest:
	cpt /f/pascallisp /pl

    The files copied will be: paslsp (executable file)
                              paslsp.ini (initialization file)
                              paslsp.tst (a test file)
@end(verbatim)

Run paslsp as you would any other file.  If you
get an error it is most likely because the paslsp.ini file couldn't be found.
If this happens, locate paslsp.ini and try again.  If it still hangs,
try calling Ralph Ottenheimer at (801) 355-0226 or M. Griss at (801) 581-6542.


Previously prepared files of LISP (e.g., library procedures)
can be input by
using the function "DSKIN".  For Example,
@begin(verbatim)
(DSKIN 'Paslsp!.tst) or (DSKIN "Paslsp.tst")
@end
would load the paslsp test file. The PASLSP test is adapted from an extensive
test of Standard LISP (avoiding features not yet implemented).  This is a
good excercise, try it. [Note that if the filename is given as an ID,
that special characters should be prefaced by an "escape character",
! . This is  also the case for filenames in OPEN.  Alternately the string
form may be used, in that case special characters need not be escaped.]

  Paslsp is "case-sensitive" with regard to identifiers.  All of the
kernel procedures have upper-case identifiers associated with them.  This
means that ordinarily the expression (dskin 'paslsp!.tst) would not be
recognized since "dskin" is in lowercase.  However, there is a global flag
!*RAISE which if true will convert all lower-case typin to upper-case.
This Apollo DOMAIN paslsp implementation sets !*RAISE to T as a default by
having (SETQ !*RAISE T) in the paslsp.ini file.  You may put any special
initialization code you like at the end of paslsp.ini as indicated by the
comments in the file.
Toggling would be accomplished by typing the following lisp-expressions:
@begin(verbatim)
	(ON !*RAISE)     equivalent to  (SETQ !*RAISE T)
        (OFF !*RAISE)    equivalent to  (SETQ !*RAISE NIL)
@end(verbatim)

	Any Apollo DOMAIN filename (60 characters maximum)is allowable
 as a paslsp filename.
Remember to prefix all special characters with an exclamation-mark: "!". 
Special characters include all non-alphanumerics. For example: !*RAISE
 goforit!! paslsp!.test !/login!/smith!/foo!.sl .

If the global !*ECHO is not NIL (default is NIL), input will be echoed to
the selected output channel.  It is sometimes convienient to put:
@begin(verbatim)
        (SETQ !*ECHO T)
@end(verbatim)
at the beginning of a file to be read by DSKIN, and:
@begin(verbatim)
        (SETQ !*ECHO NIL)
@end(verbatim)
at the end.  This will echo the file to the screen (or to a file) as it is
read. 

Certain low level errors do not display any explanatory message but
instead display a numeric code (such as *** # 2), below is a summary of these
codes and their meanings:

@begin(verbatim)
  (* error codes.  corresponding to tag = errtag. *)
  noprspace = 1;    (* no more "pair space"--can't cons. *)
  notpair = 2;      (* a pair operation attempted on non-pair.*)
  noidspace = 3;    (* no more free identifiers *)
  undefined = 4;    (* used to mark undefined function cells *)
  noint = 5;        (* no free integer space after gc. *)
  notid = 6;        (* id was expected *)
@end(verbatim)


@section(Implementation of PASLSP)
@subsection(Building PASLSP)
PASLSP is built in the following steps:

@u(Kernel files), PAS0.PRE, and trailer file (main program) PASN.PRE
are run through a filter program to produce PAS0.PAS and PASN.PAS,
tailored to the Apollo DOMAIN (appropriate Include files, Consts, etc).
This kernel provides the Basic I/O (Token reading and printing),
handcoded storage allocator and garbage collector, lowlevel arithmetic
primitives, lowlevel calls (via Case statement) from LISP to kernel, etc.

@u(Rest of LISP), currently files PAS1.RED, PAS2.RED and PAS3.RED are
compiled to PASCAL using a version of the Portable LISP Compiler
(PLC)@cite(griss79). During compilation, a Symbol Table file, PASn.SYM is
read in and written out. These files record (for "incremental" compilation)
the names and ID table locations of each ID encountered, so that the compiler
can refer to an ID by its offset in the ID table. LISP constants are also
recorded in the PASn.SYM files. PAS0.SYM is modified by hand as the kernel
is changed.  

The compilation model used is that of a Register Machine: Arguments to LISP
functions are passed in registers (a PASCAL array), and the result returned
in Register 1. Space is allocated on a software stack (not the PASCAL
recursion stack), for any temporaries or save arguments required. Short
functions usually do not require any stack. The reason for this choice was
the existence of the PLC (targeted at comventional machines), and the fact
that inline access to the register array compiles quite well, while a
"PUSH/POP" stack would be much less efficient.

@u(Initialization). 
After the PAS0.PAS,..PASN.PAS are produced,
the symbol table file (pas3.sym) is converted into a file
PASLSP.INI, which contains the names of all ID's, the LISP constants
used, and also ID's for all kernel functions that should be known to the
user LISP level. Also produced is a file, EXEC.PAS, that contains a case
statement associating each user callable kernel function with an integer.
The PAS0.PAS ... PASN.PAS and EXEC.PAS are compiled and linked into an
executable file. When this file is executed, PASLSP.INI is read in:
each id is read and stored in the appropriate location in the symbol-table,
the kernel function names have the associated Case index put into
a function cell, and the LISP s-expressions are READ in. Finally,
some s-expressions will be executed (with care, the user can add his own
expressions, including requests to (DSKIN 'library), etc.
@subsection(Internal data structures)
The data spaces (or heaps) in PASLSP are divided into 4 sections: the
pair space, id space (the oblist), string space and large integer
(fixnum) space.  These are all arrays of objects of the appropriate type
(see declarations below).  The system is fully tagged, that is, every LISP
item has associated with it a tag field which denotes the type of the item 
and an 'info' field which either points to the item in an array (in the
case of pairs, identifiers and fixnums), or contains the information 
itself (in the case of inums, character codes and error conditions). The
info field of a code pointer contains the index into a case staement (see
procedure 'execute') by means of which any LISP callable function may be
invoked.

@begin(verbatim,leftmargin 0)
itemref = RECORD
           tag:  integer;   (* Small integer denoting  type.   *)
           info: integer;   (* Item or a pointer to it         *)
                            (* depending upon the type.        *)
          END;

   pair = PACKED RECORD
            prcar: itemref;
            prcdr: itemref;
          END;

  ident = PACKED RECORD           (* identifier *)
            idname: stringp;
               val: itemref; (* value *)
             plist: itemref; (* property list *)
           funcell: itemref; (* function cell *)
           idhlink: id_ptr;  (* hash link *)
                   END;
@end(verbatim)
@subsection(Adding user functions to the kernel)
It is fairly easy to add handcoded Pascal functions to
the kernel so that they can be called from LISP. For example,
consider adding the function SQR(x), that squares its integer argument.
Since SQR is already the name of an existing PASCAL function, we will
call it "Xsqr" in PASCAL, and SQR in LISP.

The function Xsqr has to take its argument from R[1], check that it is an intege, square the information part, and retag as integer:
@begin(verbatim)
PROCEDURE Xsqr;
    VAR i1 : longint;

    BEGIN
    int_val(r[1], i1);  (* Test type and extract Info *)
    mkint(i1 * i1, 1)   (* Square, retag, and put in R[1] *)
    END;
@end(verbatim)

Now procedure Xsqr needs be to be installed into the EXECUTE table, so that
it can be found as the N'th code item. The number of defined procedures
will have to be increased by 1 in the 3'rd line of procedure EXECUTE,
(currently 201 defined), and an additional case added:
@begin(verbatim)
202:    Xsqr;
@end(verbatim)

Note also that this table gives the Internal names of each available
procedure, should one of these be required in your handcoded procedure.
Finally, the Identifier SQR needs to be associated with case 202 in
PASLSP.INI.  Note that PASLAP.INI has 3 tables of objects, each prefixed by
a count and terminated by a 0. The first is the Random ID table, consisting
of special ID's used for messages etc. The second block is for S-expression
constants, which get loaded into the base of the stack as Globals. The
next batch are the names of LISP callable functions in the order
corresponding to the EXECUTE procedure. Simply modify the count form
201 to 202 (or whatever), and add SQR at the end, just before the 0.

In general, look for a sample procedure in the kernel if possible,
or in the compiled part (although these are hard to follow), and adapt
to the specific needs. Note the use of the ALLOC(n) and DEALLOC(n)
procedures to allocate a block of temporaries on the stack.
These should be used, rather than PASCAL VAR's, since the garbage collector
may need to trace from one of the saved objects.
@Section(Future work on PASLSP)
PASLSP V2 is based on a fairly old model of a portable LISP, and
has been used mainly to explore the capbilities of PASCAL as a
target language. In particular, V2 PASCAL is not yet powerful enough to
run the PLC compiler  itself;
instead, the PLC is run on our PSL system on the DEC-20. In order for the
full benefits of PASLSP (or PSL) to be realized, the user should be able to
compile his own LISP modules into PASCAL and link them with the kernel.
In order to make the system even more adapatable, we would like to write
even less of the kernel in PASCAL by hand. This goal has lead us to the
development of PSL. 

@subsection(Goals of the Utah PSL Project)

The goal of the PSL project is to produce an efficient and transportable
Standard LISP system that may be used to:
@begin(enumeration)
Experimentally  explore
a variety of LISP implementation issues (storage management, binding,
environments, etc.).

Effectively support the REDUCE computer algebra system@cite(hearn73)
on a number of machines.

Provide the same, uniform, modern LISP programming environment on all of
the machines that we use (DEC-20, VAX/750, PDP-11/45, PERQ, and Apollo), of
the power and complexity of UCI-LISP, FranzLISP or MACLISP, with some
extensions and enhancements derived from LISP Machine LISP or CommonLISP.
@end(enumeration)

The approach we have been using is to write the @b(entire) LISP system in
PSL (using LISP extensions for dealing with 
machine words and operations), and to bootstrap it to the desired target
machine
in two steps:
@begin(enumeration)
Cross compile an appropriate kernel to the assembly language of the
target machine;

Once the kernel is running, use a resident compiler and loader, or
fast-loader, to build the rest of the system.
@end(enumeration)

 The PASLSP system, and other early implementations, have the problem that
the implementation language (PASCAL) is a distinct language from LISP, so
that communication between "system" code and "LISP" code was difficult.  We
have incorporated all of the good features of the earlier work into a new
efficient LISP-like systems language, SYSLISP, recoded all useful modules
into SYSLISP, and proceeded from there.  SYSLISP currently produces
targeted assembly code; earlier verisions were targeted at high-level
languages such as FORTRAN, PASCAL, C or ADA.  The goal is a portability
strategy that leads to an efficient enough system for a production quality,
yet portable system. We currently think of the extensions to Standard LISP
as having two levels: the SYSLISP level, dealing with words and bytes and
machine operations, enabling us to write essentially all of the kernel in
Standard LISP; and, the LISP level, incorporating all of the features that
make PSL into a modern LISP.  Both modes of PSL are compiled by an improved
version of the Portable Standard LISP Compiler. The SYSLISP mode of the PSL
compiler does compile-time folding of constants, and more comprehensive
register allocation than the previous LISP-only version of the compiler.

The current state of PSL is fully described in an "overview" document
obtainable from the authors @cite(griss81e).  Currently PSL runs on the
DEC-20 under TOPS-20, and on the DEC VAX-11/750 under Unix.  We are now
concentrating on the MC68000 PSL for the Apollo. All of the code-generators
and assembler support is complete, and a number of large files have been
compiled from LISP to assembly code, and correctly assembled and executed
on the Apollo, testing basic I/O and arithmetic. We are now in the process
of writing the PSL support code (small functions in LAP), and testing that
various decisions about register and memory usage are correct. Based on the
development history on the VAX, we are about 1-2 months away from a
preliminary PSL on the Apollo.
@section(References)
@Bibliography
@appendix(A List of Current PASLSP Functions and Globals)
@begin(verbatim,leftmargin 0)
@include(Appendix-A.table)
@end(verbatim)
