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
             Right "November 1981", 
             Line "Operating Note xx"
            )
@set(page=1)
@newpage()
@begin(titlepage)
@begin(titlebox)
@b(A PASCAL Based Standard LISP for the PERQ)
@center[
by

M. L. Griss, R. Ottenheimer, S. Voelker, K. Boekleheide

Department of Computer Science
University of Utah
Salt Lake City, Utah 84112

@b(Preliminary  Version)

Last Revision: @value(date)]

@end(titlebox)
@begin(abstract)
This report describes  an interim implementation
of Standard LISP for the PERQ. This LISP is based upon the
Standard LISP report, and a newly developing Portable Standard LISP.
This interim implementation is designed to explore LISP implementations
in PASCAL on the PERQ and similar machines. The system consists of
a kernel, handcoded in PASCAL, with the rest of the system written in
LISP and compiled to PASCAL.
@End(abstract)
@begin(Researchcredit)
Work supported in part by the National Science Foundation
under Grant No. MCS80-07034, and by xxxx.
@end(Researchcredit)
@end(titlepage)
@pageheading(Left "PERQ Standard LISP",Center "@value(date)",
             Right "@value(Page)"
            )
@set(page=1)
@newpage
@section(Introduction)
In this preliminary report, we describe an implementation of Standard LISP
in PASCAL, PASLSP. Versions of PASLSP have been run on a number of
machines, ranging from LSI-11 based TERAK to APOLLO and PERQ. This report
concentrates on the PERQ implementation. This report is to be read in
conjunction with the Standard LISP report@cite(Marti79); we will
highlight the differences from the functions documented in the Standard
LISP, describe the implementation strategy, and discuss future work.

PASLSP is based on a series of small and medium sized LISP interpreters
that have been developed at the University of Utah; each of these LISP
systems consists of a small kernel handcoded in some language, with the
rest of the system written in LISP and compiled to the target language.
We have used FORTRAN, PASCAL and assembly language as targets. The PASLSP
series use PASCAL for the kernel, and have a LISP to PASCAL for the rest of the
system. Recent work has concentrated on reducing the hand-coded kernel,
and has extended the compiler to compile more systems level constructs
(SYSLISP level), resulting in a new Portable Standard LISP running
on the DEC-20@cite(xxx). The PSL system is a modern, efficient system,
and it is hoped to replace PASLSP with a PSL implemented in PASCAL.

@subsection(History of PASLSP)
The system now called PASLSP was originally developed (by M. Griss and W.
Galway), as a small LISP like kernel to support a small algebra system on
an LSI-11 TERAK; this was to be used as an answer analysis module within a
CAI system@cite(Brandtxx), written entirely in PASCAL. It was decided to
hand-code a very small kernel, and compile additional functions written in
LISP (LISP support functions and algebra package) to PASCAL, using a
modified Portable LISP compiler@cite(griss79). This version (call it V0)
did not even have user defined functions, since space on the TERAK was
at a premium.

About June 1981, PASLSP came to the attention of a number people evaluating
Apollo's and PERQ's, and it was suggested that we enhance V0 PASLSP for
this purpose. During the space of  a few days, sufficient features taken
from the Standard LISP Report were added to the kernel and support files
to produce V1 of PASLSP, running on a DEC-20 and Terak. This was
a fairly complete LISP (including Catch and Throw), but lacked a few
features (OPEN, CLOSE, RSD, WRS, PROG, GO, RETURN, Vectors and Strings).
V1 PASLSP was adapted to a PERQ, VAX and Apollo by Paul Milazo of Schlumberge
in the space of a few weeks (we did not have a PERQ or Apollo at that time).

We subsequently obtained a PERQ and an Apollo, and recent work has been
aimed at producing an enhanced PASLSP for these machines, as well as
the TERAK, and other personal machines. The current system, V2 PASLSP,
is produced from a single PASCAL kernel and set of LISP support files;
the machine specific features are handled by a simple Source Code
conditionalizer, changing the definition of certain constants and data
types. 

We are releasing a copy of V2 PASLSP as an small, interim LISP, until
a better LISP based on a more modern Portable Standard LISP can
be completed.
@subsection(Acknowledgement)

I would like to acknowledge the advice, and software contributions of
Will Galway,  Eric Benson and Paul Milazo.

@section(Implementation of PASLSP)

@section(Features of PASLSP and relation to Standard LISP)
PASLSP as far as possible provides all the functions mentioned
in the attached Standard LISP Report (note the hand-written
comments added to this appendix); some of the functions are simply
stubs, so that a Standard LISP Test-file can be run with out major
modification.

PASLSP-V2  does not implement the following features of Standard LISP:
@begin(enumeration,spread 0)
STRINGS or VECTORS (only a simple garbage collector is used).

Integers are limited in size (INTs and FIXNUMs,no BIGNUMs).

FLOATING Point. 

IDs can not be REMOB'ed or INTERN'd.

Only 3 Input Channels and 2 Output Channels are available to OPEN,
RDS, WRS, and CLOSE. Thus file input statements can not be nested
very deeply in files.

Line, Page and Character counting (POSN, LPOSN, etc).
@end(enumeration)

PASLSP-V2 provides some extensions over Standard LISP:
@begin(enumerate,spread 0)
CATCH and THROW (both tagged and Untagged).

Implicit PROGN in COND, and LAMBDA expressions.

WHILE loop.

CntrlC handlers.
@end(enumerate)
@Section(Features of PSL that will be incorporated in next PASLSP)

@subsection(Goals of the Utah PSL Project)

The goal of the PSL project is to produce an efficient and transportable
Standard LISP system that may be used to:
@begin(enumeration)
Experimentally  explore
a variety of LISP implementation issues (storage management, binding,
environments, etc.);

Effectively support the REDUCE algebra system on a number of machines;

Provide the same, uniform, modern LISP programming environment on all of
the
machines that we use (DEC-20, VAX/750, PDP-11/45 and some personal machine, perhaps 68000
based), of the power and complexity of UCI-LISP or MACLISP, with some
extensions and enhancements.
@end(enumeration)

The approach we have been using is to write the @b(entire) LISP system in
Standard LISP (with extensions for dealing with 
machine words and operations), and to bootstrap it to the desired target
machine
in two steps:
@begin(enumeration)
Cross compile an appropriate kernel to the assembly language of the
target machine;

Once the kernel is running, use a resident compiler and loader, or
fast-loader, to build the rest of the system.
@end(enumeration)

We currently think of the extensions to Standard LISP as having two levels:
the SYSLISP level, dealing with words and bytes and machine operations,
enabling us to write essentially all of the kernel in Standard LISP; and,
the STDLISP level, incorporating all of the features that make Standard
LISP into a modern LISP.

In our environment, we write LISP code using an ALGOL-like preprocessor
language, RLISP, that provides a number of syntactic niceties that
we find convenient; we do not distinguish LISP from RLISP, and can
mechanically translate from one to the other in either direction.
@section(References)
@Bibliography
