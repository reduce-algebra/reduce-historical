@section[A Brief History of @PSL]
@begin[Comment]
  This section NEEDS MORE WORK!!  (WFG)

  Major ideas I think we should cover are:

    -Influence of REDUCE on the system (e.g. Rlisp syntax).
    -Work on "Standard Lisp".
    -Work on portable compiler.

  The major focus of this chapter should be clarifying why PSL is what it
  is, and explaining other alternatives that were explored.  [But BRIEFLY!]
  e.g.
   - Why Rlisp syntax (an outgrowth of REDUCE)
   - Why syslisp instead of (e.g.) C,  (or BIL, or whatever).
   - Why "DE" instead of "Defun" (perhaps this is getting into too much
     detail).

  (Also, perhaps, give more credit to various folks?)

@end[Comment]

@topic[History of PSL]
@Comment{TALK a bit more about REDUCE and Rlisp, mention some of the
systems they ran on (e.g. Lisp 1.6 (or 1.5?), IBM dialect (namely?), ...}
@Comment{Is my impression correct that REDUCE was once written in LISP
syntax, later converted to Rlisp?}

@Comment{Then go into this paragraph, but don't need to explain what REDUCE is.}
In 1966, a model for a standard @Lisp subset was proposed@cite(Hearn66) as
part of a general effort to make @Reduce@cite(Hearn73), a large
@Lisp-based algebraic manipulation system, as portable as possible.
The goal of this proposal was to define a uniform subset of @lng[Lisp 1.5]
and its variants so that programs written in this subset could run on any
of those @Lisp systems.

@Comment{"intervening"?  Between what and what?}
In the intervening years, two deficiencies in the original proposal
emerged.  First, in order to be as general as possible, the specific
semantics of several key functions were left undefined.  Consequently,
programs built on this subset could not be written with any assumptions
made about the form of the values of such functions.  The second deficiency
was in the proposed method of implementation of @lng[Standard Lisp].  The
model considered two versions of @Lisp on any given machine, namely
@lng[Standard Lisp] and the @Lisp of the host machine, which we shall refer to
as @lng[Target Lisp].  
@Comment{I CAN'T MAKE SENSE OF THE FOLLOWING (WFG).}
This meant that if any definition were stored as
interpretive Target @Lisp, it would vary from implementation to
implementation; consequently, one could not write programs in Standard
@LISP which needed to assume any knowledge about the structure of such
forms.  This deficiency became apparent during recent work on the
development of a portable compiler for
@Lisp@cite[Griss81b].  It is clearly easier to write a compiler if we
deal with a single dialect (Standard @Lisp) than if we must
change it to conform with the various Target @Lisp@xs.

As a result of this study, we produced a more aggressive definition of
Standard @LISP in the Standard @LISP Report@cite(Marti79).
That paper can serve as a standard for a reasonably large subset of
@Lisp with as precise as possible a statement about the semantics
of each function.

Recent work has concentrated on producing a @i(complete) specification and
portable implementation of a @lisp based on @lng[Standard LISP].
Experience with a Portable @Lisp Compiler@cite(Griss81b) and with an
earlier experimental portable @Lisp implementation@cite(Griss79)) has led
to the current @PSL implementation strategy: write most of the system in
@Lisp, compiled with the portable compiler.  A small non-@Lisp kernel is
written in a portable,
@Lisp-like systems language, @Syslisp.

The previous systems had the problem that the special implementation
language (called @lng<BIL>), although oriented to @Lisp implementations,
was a distinct language from @Lisp, so that communication between "system"
code and "@Lisp" code was difficult.  The pattern-driven @lng(BIL) compiler
was not very efficient.  Consequently, the @lng(BIL) work resulted in a
number of experimental @Lisp@xs on a number of machines.  These
implementations were quite flexible, portable, and useful for @Lisp and
@Reduce on machines that did not already have any @Lisp, but somewhat
inefficient.  We therefore developed the much more powerful, @Lisp-like
systems language, @SYSLisp, in which to recode all useful modules.  @SYSLisp
has been targeted to high-level languages (such as @Fortran, @Pascal,
@lng(C) or @Ada), and also to assembly code.  We believe this approach will
advance our goal of producing a portability strategy which could lead to a
system efficient enough for realistic experiments with computer algebra and
ultimately to portable, production quality systems.
