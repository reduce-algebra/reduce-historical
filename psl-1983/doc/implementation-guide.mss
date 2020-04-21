@make(article)
@Case(Draft, 1 <@device(Omnitech)>,
             else <@device(LPT)>
      )
@Comment{ For use with the final versions }
@Style(WidowAction=warn)
@Style(Hyphenation Off) @comment(on)
@Style(DoubleSided no) @comment(yes)
@style(Spacing 1)
@comment[See G:MSS-junk.MSS]
@use(Bibliography "<griss.docs>mtlisp.bib")
@comment{ Font related stuff }
@Define(OP,FaceCode Y,TabExport)@comment{ used for indicating opcodes in
                                          C-macros }
@modify(enumerate,numbered=<@a. @,@i. >, spread 1)
@modify(itemize,spread 1)
@modify(description,leftmargin +2.0 inch,indent -2.0 inch)
@LibraryFile(PSLMacrosNames)
@LibraryFile(SpecialCharacters)
@comment{ The logos and other fancy macros }
@PageHeading(Left  "Utah Symbolic Computation Group",
                        Right "May 1982",
                        Line "Operating Note No. xx"
            )
@set(page=1)
@newpage()
@Begin(TitlePage)
@begin(TitleBox)
@MajorHeading(@PSL Implementation Guide)
@Heading(M. L. Griss, E. Benson, R. Kessler, S. Lowder, 
G. Q. Maguire, Jr. and J. W. Peterson)
Utah Symbolic Computation Group
Computer Science Department
University of Utah
Salt Lake City, Utah 84112
(801)-581-5017

Last Update: @value(date)
@end(TitleBox)
@begin(abstract)
This note describes the steps involved in bringing PSL up on a new
machine.  It combines information from the previous BOOTSTRAP, LAP,
CMACRO and TEST guides.
@end(abstract)
@center[
File: @Value(SourceFile)
Printed: @value(date)]
@copyrightnotice(Griss, Benson, Lowder, Maguire and Peterson)
@begin(ResearchCredit)
Work supported in part by the National Science Foundation under Grant
No. MCS80-07034, and by Livermore Lawrence Laboratories under
Subcontract No. 7752601, IBM and HP.
@end(ResearchCredit)
@end(TitlePage)

@pageheading(Left "Implementation Guide", Center "@value(date)",
                 Right "Page @Value(Page)"
            ) @comment{@pageheading(Even,Left "Page @Value(Page)",
                  Right "Operating Note No. xx"
            )} @set(page=1) @newpage()

@section(Introduction)

This document describes the techniques used to implement PSL on a new
machine.  This note assumes that the reader has some familiarity with
the basic strategy of @PSL implementation (see the 1982 LISP Conference
Paper on PSL, UCP-83), and has also read the papers on the @PSL Portable
@xlisp compiler (Griss and Hearn, "Software Practice and Experience",
and Griss, Hearn and Benson, 1982 Compiler Conference).  Also see the
compiler chapter (19) of the @PSL manual@cite[Griss81].  Finally, a
basic understanding of how to use PSL and LISP is required@cite[Griss81].

In order to explain a new PSL implementation, we will first describe the
PSL compilation model, hopefully providing some insight into the various
steps involved in the transformation of PSL sources into code executable
on the target machine.  @comment{May want to add a description of each
section to follow}

The initial level of transformation takes the RLISP format and
translates it into LISP for those source files that are written in RLISP
format; those files already in LISP may be directly input into the
system (see the figure below).  The LISP code is then compiled into
instructions for an Abstract Lisp Machine (ALM).  The ALM is a
general-purpose register machine designed for its ease as a target for
compilation@cite(Griss81b) in which temporary variables are allocated in
a block of locations on a @ei[stack].  The ALM instructions are
expressed in LAP format (LISP Assembly Program) which
consists of a list whose first element is the ALM opecode
followed by zero or more ALM operands which are ALM addressing
modes. The ALM format is (ALMopcode ALMoperand ... ALMoperand).
 The ALMopcode is a macro referred to as a CMACRO and the
addressing modes of the ALMoperands are referred to as ANYRegs.

The ALM instructions are macro expanded into instructions for the Target Lisp
Machine (TLM).  TLM instructions have the same LAP format, except the
operators are now TLM operators and the operands are TLM addressing modes.

From here, a number of alternate routes are possible for the final code
generation. So far the LISP or RLISP has transformed into
into a set of TLM instructions that can take one of three paths.

@begin(enumerate)
Fist, the TLM instructions can be printed out as Target Machine Assembly
code (ASM) for assembly on the
target machine.  This route is followed in the initial phases of the PSL 
implementation process to produce code for the target machine.

Secondly, a file of the target machine code can be produced in a
format that can be loaded directly into a running PSL system.  This
process is called FASLing, producing a FASt Load format file.

Finally, the TLM code can be assembled and deposited directly into memopry
of the running PSL system.
This is basically analogous to the process used to load in a FASL file
produced above except the code is not written to or read from a FASL file.
@end(enumerate)

This process is illustrated below:

@begin(verbatim,leftmargin 0,group)
    .-----------------.   Rlisp:        Procedure SelectOne x;
    | RLISP input code|                   x := car x;
    `-----------------'
             v
         .------.      
         | LISP |         Lisp:        (de selectone (x) 
         `------'                          (setq x (car x)))
             v
        .----------.
        | Compiler |
        `----------'
             v
.------------------------.  ALM:       (!*entry selectone expr 1)
|ALM instructions in LAP |             (!*alloc 0)
| format                 |             (!*move (car (reg 1))
`------------------------'                (reg 1))
            v                          (!*exit 0)
       .----------.
       | Pass1Lap |
       `----------'
            |             
            v
.---------------------.      TLM:      [68000 code]
| TLM instructions in |                (Fullword 1) Count of Args
|  LAP format.        |                (!*Entry selectone expr 1)
`---------------------'                (movea!.l (indirect 
     |           |                       (reg 1)) (reg 1))
     |           v                     (rts)
     |       .------------.  
     |       | TLM to ASM |
     |       | converter  |
     |       `------------'
     |           v
     |	  .-------------------.   ASM: dc.l 1
     |    |                   |        movea.l (a1),a1
     |	  | Asm code suitable |        rts
     |    |  for TM assembler | 
     |    `-------------------'
     v
.--------------.      .-----------------.
| LAP resident |----->| Resident binary |
|   assembler  |  |   `-----------------'
+--------------+  |   .------------.
                  `-->| FASL files |
                      `------------'
@end(verbatim)

In summary, here is an overview of the steps necessary to implement
PSLon your target machine.  More details will be given in the
following sections.
@begin(enumerate)
Prelimaries:
@begin(enumerate)
Believe in yourself.

Choose the host machine.

Test file transfer.
@end(enumerate)

Decide how to map the ALM architecture to the TLM.

Implement the TLM to ASM.

Implement the ALM to TLM.

Build the Cross Compiler and test.

Run Cmacro Tests.

Build Bare PSL.

Implement a resident TLM assembler.

Implement FASL.

Bootstrap the compiler.
@end(enumerate)


@section(Overview of the Abstract LISP Machine)
The abstract machine is really a class of related machines rather than a
single fixed machine (such as PASCAL P-code, or some true @xlisp machines).
The exact set of @CMACRO@XS, the number of registers, etc@. are under the
control of parameters, flags and compiler code-generator patterns defined
for the specific machine.  This flexibility permits the match between the
compilation model and the target machine to be better set, producing better
code.  Therefore, the exact set and meaning of @CMACRO@XS are not
fixed by this definition; rather, they form an adjustable @dq[convention]
between the compilation and @CMACRO/Assembly phase.  The compiler itself is
defined in PC:COMPILER.RED@Foot[dir: represents a logical directory name,
in this PC: stands for <PSL.Comp> under Tops-20 or /psl/comp under UNIX.]
and is augmented by machine-specific files, described later.

The  ABSTRACT LISP MACHINE (ALM) used by our compiler has the following
characteristics.



@begin(enumerate)
There are 15 general purpose registers, 1 ..@. 15;
and a stack for call/return addresses.

Locals and temporaries variables are allocated on the stack by
allocating a frame of temporaries large enough to hold them all, not
by the use of push and pop instructions.

The function calling mechanism loads N args into 1 ..@. N, and
then transfers to the function entry point, pushing the return
address onto the stack if necessary.
The functions result is returned in register 1.

Each procedure is responsible to save any values it needs on stack;
small procedures often do not use the stack at all.

The following is a brief lisp of all the ALM opcodes (CMACROS).

@begin(verbatim)
(!*ALLOC nframe:integer)
(!*ASHIFT dest:any-alterable source:any)
(!*CALL name:id)
(!*DEALLOC nframe:integer)
(!*EXIT nframe:integer)
(!*FIELD operand:any-alterable starting-bit:integer
         bit-length:integer)
(!*FOREIGNLINK name:id type:id
         number-of-arguments:integer)
(!*FREERSTR l:nonlocalvars-list)
(!*JCALL name:id)
(!*JUMP label:any)
(!*JUMPEQ label:any source1:any source2:any)
(!*JUMPINTYPE label:any source1:any type-name:id)
(!*JUMPNOTEQ label:any source1:any source2:any)
(!*JUMPNOTINTYPE label:any source1:any type-name:id)
(!*JUMPNOTTYPE label:any source1:any type-name:id)
(!*JUMPON source:any lower-bound:integer
          upper-bound:integer l:label-list)
(!*JUMPTYPE label:any source1:any type-name:id)
(!*JUMPWGEQ label:any source1:any source2:any)
(!*JUMPWGREATERP label:any source1:any source2:any)
(!*JUMPWITHIN label:any lower-bound:integer
              upper-bound:integer)
(!*JUMPWLEQ label:any source1:any source2:any)
(!*JUMPWLESSP label:any source1:any source2:any)
(!*LAMBIND r:registers-list l:nonlocalvars-list)
(!*LBL label:tagged-label)
(!*LINK name:id type:id number-of-arguments:integer)
(!*LINKE nframe:integer name:id type:id 
         number-of-arguments:integer)
(!*LOC dest:any-alterable source:any)
(!*MKITEM inf:any-alterable tag:any)
(!*MOVE source:any dest:any-alterable)
(!*POP dest:any-alterable)
(!*PROGBIND l:nonlocalvars-list)
(!*PUSH source:any)
(!*PUTFIELD source:any dest:any-alterable
            starting-bit:integer bit-length:integer)
(!*SIGNEDFIELD operand:any-alterable 
               starting-bit:integer
               bit-length:integer)
(!*WAND dest:any-alterable source:any)
(!*WDIFFERENCE dest:any-alterable source:any)
(!*WMINUS dest:any-alterable source:any)
(!*WNOT dest:any-alterable source:any)
(!*WOR dest:any-alterable source:any)
(!*WPLUS2 dest:any-alterable source:any)
(!*WSHIFT dest:any-alterable source:any)
(!*WTIMES2 dest:any-alterable source:any)
(!*WXOR dest:any-alterable source:any)

(LABELGEN tag:id)
(LABELREF tag:id)
(!*CERROR message:any)

(FULLWORD [exp:wconst-expression])
(HALFWORD [exp:wconst-expression])
(BYTE [exp:wconst-expression])
(STRING s:string)
(FLOAT f:float)

@end(verbatim)

ALM operand forms ("addressing" modes)

@begin(verbatim)
(FLUID name:id)
(!$FLUID name:id)
(GLOBAL name:id)
(!$GLOBAL name:id)
(WVAR name:id)

(WARRAY name:id)
(WSTRING name:id)
(WCONST expr:wconst-expression)
(IMMEDIATE wconst-expression:any)
(QUOTE s-exp:s-expression)
(LABEL l:id)

(MEMORY base:any offset:wconst-expression)
(CAR base:any)
(CDR base:any)

(FRAME n:integer)
(REG reg-descriptor:{integer,id})

(LIT [any-instruction-or-label:{list,id}])
(LABELGEN tag:id)
(LABELREF tag:id)

(IDLOC symbol:id)
@end(verbatim)
@end(enumerate)

@Section(System Overview for Bootstrapping)
Currently PSL is half bootstrapped from a complete PSL system on a 
host machine. At the moment only the Decsystem 20 and the VAX 750 
can be used as hosts; shortly we expect the Apollo and HP9836 to
be also usuable.
If you have a choice for your host machine, one important consideration
will be the ease in shipping code between the host and target. It is worth
taking the time initially to be sure this pathway is as smooth and troublefree
as possible. The need for easy file transfers is derived from the half 
bootstrap method and the iterative nature of developing and debugging the
tables used in the ALM to TLM transformation. The size of the transferred
files will be in the range of 1 to 70 KBytes.  
Having a fast network or a tape transfer from host to target is worth
considering in the beginning of a PSL implementation.

The first major step in the implementation will be to modify  the host PSL
to become a cross compiler, turning lisp or rlisp into the target machines
assembly language. 

@SubSection(Overview of the Cross Compiler)
Three modules are created, compiled and loaded into a host PSL to transform
it into a cross compiler.

@begin(enumerate)
The first module will be xxx-comp.red (we will use XXX to represent
the name of the target machine, like DEC20, VAX, etc.); a file
containing patterns used by the compiler to control which ALM
instructions are emitted for certain instructions.  Basically it is
used in LISP to ALM transformations and initially will only require
you to copy the same file used on your host machine.

The second module will be xxx-cmac.sl. This file contains the
tables(CMacroPatternTables) used to convert ALM opcodes to TLM opcodes,
the tables used to convert ALM addressingmodes into TLM addressingmodes
(ANYREGS), and some miscellaneous required opencoded functions.

The last module, xxx-asm, consists of two files, xxx-asm.red and
xxx-data-machine.red. The first file, xxx-asm.red, specifies the necessary
formats, costants, and procedures for converting TLM instructions into the
host's actual assembly language.  The file, xxx-data-machine.red, provides
constants for describing to the compiler some of the specific choices for
what registers to use and how the lisp item will be used in the machine
words.
@end(enumerate)
All of these modules are compiled and loaded into a host PSL to turn
it into the cross compiler.  The next few sections will try to
describe to the reader how these three modules are actually designed
and built from the bottom up. It will be worth getting a listing of
these modules for your host machine and also for a machine most similar
to your target machine, if available.

@Section(Designing the TLM instruction format).

The implementor must decide first the specifics of the TLM instruction
format patterned around the form (TLMopcode TLMoperand ... TLMoperand). 
The TLM to ASM translation occurs in a parallel manner.

(TLMopcode       TLMoperand      TLMoperand)       TLM format.
    |                 |              |
 ASMopcode        ASMoperand      ASMoperand         Some ASM format.


The closer the ASM format approaches the TLM format the better. However in
some cases this will not be possible and the reader must devise a scheme. 
Take a look at the case studies for some ideas of ways to handle some of
these issues.

TLM opcodes are usually passed through unchanged to the ASM code.
However the TLM operands will require extensive changes.  [Mention
terminal operands!!!].  The TLM operands are of the form
(addressingmode value-expression). The addressingmode is a tag which
will direct what procedures will be used to convert and print the ASM
operands. The reader should pick these addressingmode names to closely
match the addressingmodes of the target machine.  Some examples of
these would be (immediate ...), (indirect ...), (displacement ...), or
(indexed ...).  Here again the case studies will give you some
information for proceeding.  [Mention CRAY mismatch of TLM].

@Section(Implementing the TLM to ASM conversion)

You can begin by creating the xxx-data-machine.red file and begin to add
some definitions. First pick a name for your system, anything
representative will do like the name of its operating system or its
manufacturers identifier. Some examples are dec20, vax, apollo, or m68000.

@begin[verbatim]
fluid '(system_list!*);
system_list!* := '(MC68000 Chipmunk HP9836);
@end[verbatim]


The next step is quite important.  You must decide how you are going to
implement the LISP item on the target machine.
The LISP item consists of 2 or three fields; each field
having a position and size in the machines item picked by the
implementor.  All LISP items must have a tag field and an INFormation
field and some implementations have a garbage collector field.  The
tag field must be at least 5 bits long@Foot[Nineteen (19) different tags are
presently used.] and the inf field should be large
enough to hold a target machine address. Some implementations, such
as the Vax, will choose an inf smaller than the largest address
possible on the machine and will have to mask tag bits out when using
the inf field as an address.  This does cause problems and should be
avoided if possible.  If space allows it the INF
field may be larger to allow larger numeric operands to be stored in
registers.  

Currently PSL provides two different garbage collection methods, one
of which should be chosen (or a new one developed if needed).  One is
a two-space copying collector, which requires no extra garbage
collection bits, but is very wasteful of space and is best for a
virtual memory machine (in fact, there are two copies of the heap).
The other is a one space compacting collector, and requires at least
one bit for marking, and ideally additional bits for relocation
(sometimes, these extra bits can be stored in a separate bit table).
Naturally these fields may be larger to make their accessing easier,
like aligning on a byte boundary.

Once you have decided upon how the LISP item will be implemented on the
machine you can begin filling in the constant definitions for the
xxx-data-machine.red file.  When numbering bits in a machine word, we have
settled upon the convention that the most significant bit is zero and
counts up to the max-1 bit. 
The current constants are 
@begin(verbatim)
TagStartingBit 
TagBitLength 
InfStartingBit 
InfBitLength 
AddressingUnitsPerItem 
CharactersPerWord 
BitsPerWord 
AddressingUnitsPerFunctionCell 
StackDirection 

and optionally

GCStartingBit
GCBitLength
@end(verbatim)
The following figure illustrates the positions of these constants:
@begin(verbatim)

      .-----------------------------------------.
      | TAG    |  [gc]  |    INF                |
      `-----------------------------------------' 
  FILL IN LATER

@end(verbatim)
Some other decisions that must be made include:
@begin(enumerate)
Which and how many registers to dedicate as the compiler-allocated
@ei[Registers];

How large an integer will be supported in the @xlisp item;

How many tags are to be supported

How to implement the recursion stack and check for stack overflow
(either using an explicit test, or some machine-interrupt);

How to pack and unpack strings;

@Comment{PSL must have explicitly tagged items, and the current allocator
is a simple linear model, so this is not relevant.

Whether to have a heterogeneous heap, multiple heaps, a @ei[page] per type,
or whatever;}

@Comment{This is also not relevant.  Pairs are the same on all machines.
How pairs are referenced, i.e. does the pointer to a pair point to the
first element, to the second element, are the pairs allocated
separately in parallel areas, or is there some type of CDR coding being
done.}
@end(enumerate)

The next step is to implement the tables that accept the ALM
form and emits assembly code for the target machine.
Most of the program is machine-independent (using
PC:LAP-TO-ASM.RED), and an @dq[xxxx-ASM.RED] file is to be
written.  We have the following already written as a guide: @DEC20
@dq[MACRO], @VAX750 @UNIX @dq[as], @68000 for @apollo and WICAT, and CRAY
CTSS CIVIC.  The main problem is to emit the correct format, such as:
placement of tabs, commas, spaces, parentheses; renaming symbols (certain
legal @xlisp IDs are not legal in some assemblers); and determining how and
where to place EXTERNAL, ENTRY and GLOBAL declarations, how to declare and
reserve blocks of storage, and how to overcome certain problems involved
with large files and restrictions on addressing modes and relocation.

Finally, the ALM to ASM needs to be tested.  This is usually
accomplished by Hand-coding some small test routines, and
then convert from ALM to machine X assembly code, assemble, and run.  This
checks the final details of required Prologues and
Epilogues@Foot[Prologues and Epilogues contain operating system-specific
standard module headers and trailers.], understanding of the instruction
set, and so on.  Suggested LAP tests are described @ei[generically], but
will have to be translated by the implementor into machine-dependent LAP
for machine X, and depending on the flavor of assembler and LAP, other
tests will have to be devised by the implementor. This is a good time to
investigate how Assembly coded routine can call (and be called) by the
most common language used on machine X (such as FORTRAN, PASCAL, C, etc.).
This "Foreign" language can be used for initial operating system support.

@section(Implementing the ALM instructions) 

The ALM instructions consists of a set of operations and their
addressing mode operands.  These ALM instructions are commonly
referred to as CMACRO's and the addressing modes are ANYREG's.  The
purpose of this part of the PSL implementation is to implement the
functionality of each ALM instruction in terms of other ALM
instructions and TLM instructions.  The ability to recursively define
the ALM instructions in terms of other ALM instructions is a benefit
because it greatly decreases the amount of code required to implement
a particular instruction.  For example, a good technique in designing
the ALM instructions is to carefully implement the !*MOVE instruction
(to distinguish ALM instructions, they generally have a !* in the front
of their name) to
efficiently handle transfer between any possible locations (memory to
register, stack frame to memory, etc.).  Then when implementing
another instruction, the code for moving the actual operands to
locations necessary for the TLM instruction can be accomplished using
a recursive call to the !*MOVE ALM instruction.

The important tasks of the implementor are to
@begin(enumerate)
Carefully examine the instruction set and architecture of the TLM to
see which instruction (instructions) correspond to each ALM CMACRO;

Decide how to map the ALM registers and addressing modes onto the
TLM registers and addressing modes (some will map one-to-one, others
will take some thought, and a sequence of actions);

Decide on a set of classifications of the TLM modes that distinguish
which of a related set of TLM opcodes should be used to implement
a particular ALM opcode, and write predicates that examine ALM and TLM
modes to decide which class they are in;

Write tables to map ALM modes into TLM modes, using these predicates,
and then ALM opcodes into a (sequence of) TLM opcodes with the correct
TLM modes.
@end(enumerate)

@subsection(Mechanics of ALM Instruction Definition)
Before we get into the description of the ALM instructions, we must first
define the table-driven pattern matching approach used to implement
them.  This approach allows definition of
an ALM instruction in terms of a pattern predicate which is used to match
the operands of the ALM instruction and a body that may consist of a
mixture of ALM instructions (for recursive decomposition) and TLM
instructions (for direct code generation).  This is exactly analogous to
the COND construct in LISP.  Just like COND, any number of predicate/body
pairs may be included in the expansion of an ALM instruction.  Also, the
order of the pairs is quite important (since they are compared in order
from first to last).  Typically, the most specific predicates are described
first followed by gradually more and more general ones.  The table
definition for a specific ALM instruction is compiled into a single
procedure.  The instruction name must then be flagged with 'MC to
indicate that it is a legal ALM instruction.  The pattern table itself
must then be stored under the indicator 'CMACROPATTERNTABLE on the ALM
instruction property list.  To simplify this process, the DefCmacro
Macro has been defined:
@begin(verbatim)

   (DefCMacro ALMInstructionName
	(pred1  body1)
	(pred2  body2)
        ...
	 lastbody)  

@end(verbatim)

Each ALM instruction is defined with a set number of arguments and the
predicates are used to compare the types and/or values of the arguments.  A
predicate need not test all arguments, with non-tested arguments defaulting
to T for a value.  For example, one could define the following patterns:
@begin(verbatim)

         Predicate               Body
   (DefCMacro ALMInst
         ((FOOP)		(Body1))
	 ((FEEP BARP)		(Body2))
	 ((ANYP)		(Body3))
				(Body4))

@end(verbatim)
Note that this looks almost exactly like the LISP operation COND.  The
one difference lies with the Body4 in the above example, which has no
predicate and will always be evaluated if all others fail (Similar to
the final 'T case in a Cond without the T).  This last predicate/body
pair may NOT have a predicate.  If it doesn't, it will be evaluted just
like the body.  [!!Future change - CERROR on the default case, and make
the defined use ANYP for his default case]  
The predicate
functions are automatically passed one argument which is the ALM operand in
the position of the test.  So, in the above example, FOOP is passed the
first operand and BARP is passed the second, after failure in the FOOP
test.

The body can be thought of as an implicit PROGN that contains a set of ALM
and TLM instructions.  These instructions then reference the various
operands as ARGONE, ARGTWO, ARGTHREE, etc. using lexical ordering in the
instruction.  For example, if an ALM instruction mapped directly to a TLM
one, it may be defined as:
@begin(verbatim)

  ((FOOP BARP)      (TLMOperator ARGONE ARGTWO))

@end(verbatim)
Or, it may map into a number of ALM and TLM instructions:
@begin(verbatim)

  ((FEEP)           (ALMOperator ARGONE Something)
                    (TLMOperator Something ARGTWO)
                    (ALMOperator Something ARGONE))

@end(verbatim)
Notice that even though the predicates only test the first operand ARGONE,
the other operands may be referenced in the body.  Also, "Something" can be
thought of as a kind of constant operand (like a particular register, an
integer constant, a memory location or whatever).

In order to facilitate more complicated instructions within the body, we
must now introduce a number of other features.  First, suppose that you
wish to include code generation time constants within the body.  This can
be accomplished by placing on the property of a variable name, 'WCONST with
its value being the desired constant.  Then when the variable is
encountered in the instruction expansion, it will be replaced by the value
on its property list under the 'WCONST indicator.  A useful function to
perform this operation would be:
@begin(verbatim)

  (DE MakeReferencedConst (ConstName ConstValue)
      (Put ConstName 'WCONST ConstValue))

@end(verbatim)
Therefore, if you perform a (MakeReferencedConst 'TAGPOSITION 10) then the
body may reference TAGPOSITION directly:
@begin(verbatim)

   ((FOOP)     (ALMOperator ARGONE TAGPOSITION))

@end(verbatim)
Now, that we have constants, it is sometimes desirable to have constant
expressions.  As long as all of the operands are either direct or
referenced constants, the expression can be evaluated in an ALM or TLM
instruction (the function may also be called if it doesn't have any
operands).  For example, the following could be imbedded within an
instruction body:
@begin(verbatim)

	(Plus2 (Foo 35 TagPosition) WordWidth)

@end(verbatim)
The system also provides for an alias mechanism, so you can map one name
into another.  This is accomplished by placing on the property of the
alias, the name of the acutal function under the property DOFN.  Thus, if
you wanted to map FEE into PLUS2, you would simply: (Put 'FEE 'DOFN
'PLUS2).  Therefore, another useful function would be:
@begin(verbatim)
    (DE Alias (AliasFunction ActualFunction)
        (Put AliasFunction 'DOFN ActualFunction))
@end(verbatim)

Sometimes in the process of generating the TLM instructions, it is
necessary to make use of a temporary label (i.e. to generate a forward
branch).  This can be accomplished by referencing TEMPLABEL (just like a
reference to ARGONE), which will create a label name consistent with a
particular body.  For example:
@begin(verbatim)

	((FOOP)			(Test ARGONE)
				(GO (Label TEMPLABEL))
				(Operate ARGONE ARGTWO)
				(Label TEMPLABEL))

@end(verbatim)
Notice that even if the label references are separated by recursive ALM
instructions, it will still create a unique reference to the label in both
places.  There is another mechanism to accomplish the same task in a more
general fashion, that allows referencing of multiple labels.  This
mechanism is used with two functions:
@begin(description)
LabelGen@\This function takes one argument and returns a generated label.
The argument and label are stored on an A-List for later reference.  The
argument may be any atom.

LabelRef@\Look up the argument on the label's A-List and return the
associated label.
@end(description)
An example of the use of these two functions is:
@begin(verbatim)

   ((FOOP)              (Label (LabelGen 'L1))
			(Test ARGONE)
			(Go (LabelGen 'L2))
			(Operator ARGTWO))
			(Go (LabelRef 'L1))
			(Label (LabelRef 'L2)))

@end(verbatim)

Finally, if the need arises to be able to call a function within an ALM
instruction expansion.  This can be accomplished by using the ANYREG
mechanism.  It is important to know that this technique will not work for a
function call within a TLM instruction, only in the recursive expansion of
an ALM instruction (there is no method for calling a function within
a TLM instruction).  (Note: ANYREG's will be explained in detail later, but
the mechanism can be used to call a function).  The technique is to first
define the function that you wish to call, with one extra argument (the
first one) that will be ignored.  Then define an anyreg function that calls
your function.  For example, suppose you want a function that returns an
associated register based upon a register argument (with the association
stored in an A-List).  The code would be implemented as follows:
@begin(verbatim)
   (De GetOtherRegFunction (DummyArgument RegName)
       (Assoc RegName '((A1 S3) (A2 S2) (A3 S1))))
   (DefAnyReg GetOtherReg GetOtherRegFunction)
@end(verbatim)
Then the pattern that may use the function would be:
@begin(verbatim)

    ((FOOP)		(ALMOperator (GetOtherReg ARGONE)
		        (GetOtherReg ARGTWO)))

@end(Verbatim)
[Future Change - Implement a technique so if it is necessary for a
random function to be called, all one has to do is define it and flag it
as something appropriate - like 'ALMRandomFunction]

@subsection(@ANYREG and @CMACRO patterns)

Certain of the ALM operands are @ei[tagged] with a very
special class of functions thought of as extended addressing modes; these
@ANYREG@xs are essentially Pseudo instructions, indicating computations
often done by the addressing hardware (such as field extract, indexing,
multiple indexing, offset from certain locations, etc.).  For example, the
@xlisp operations CAR and CDR often are compiled in one instruction,
accessing a field of a word or item.  Using @ANYREG in this case, CAR and
CDR are done as part of some other operations.  In most cases, the @ANYREG
feature is reserved for operations/addressing modes usable with most
instructions.   In some cases, the @ANYREG is too complicated to be done in
one instruction, so its expansion emits some code to @ei[simplify] the
requested addressing operation and returns a simpler addressing mode.  The
main thing is all desired computations are done using 1 or zero registers,
hence the name @dq[@ANYREG].

The @ANYREG@xs have an associated function and possible table, with the
name of the function under the property 'ANYREGRESOLUTIONFUNCTION and
the pattern under 'ANYREGPATTERNTABLE.  Just like the DefCMacro macro
has been defined to aid ALM instruction description, the macro DefAnyReg
has been provided to help set up these associations:

@begin(verbatim)

(DEFANYREG anyregname anyregfunction
	(pred1  body1)
	(pred2  body2)
        ...
	 lastbody)  

@end(verbatim)
As you can see, the structure of a DefAnyReg is exactly the same as
DefCMacro, except an additional operand AnyRegFunction must be supplied.
When an AnyReg is found in the instruction expansion, the function is
called with two or more arguments:
@begin(enumerate)
Temp Register - Since the anyreg must perform its operation using zero
or one register, this is the register that it may use to perform its
task.  (CAVEAT: The current implementation provides either (Reg T1) or
(Reg T2) as the temporary register in all cases except one.  That is
when the anyreg is the source of a move and the destination is a
register.  In that case, the destination register is passed as the
temporary.  This can cause a problem if any part of the anyreg requires
the destination to first be a source.  [Future change - Eliminate this
problem used in move and always pass in T1 or T2]).

Source - This is the actual body of the anyreg.  It may be referenced
within the AnyRegPatternTable as SOURCE.

ArgTwo - Only one anyreg (Memory) currently has more than two arguments.
If they are desired, this third argument may be referenced by ARTTWO.
@end(enumerate)
A defect in the current system is that the pattern predicates following
the anyreg function may not test the Temporary Register.  This is quite
inconsistent, since the function definition must consider the operand,
while the pattern table must ignore it.  [Future change - Fix This
problem]

@subsection(ALM Instruction Expansion)
Now that we understand the mechanics of defining ALM instructions and
anyreg tables we need to explore the order of expansion of the
instructions.  The compiler emits ALM instructions, with the operands
being legal ALM "addressing" modes.  These instructions are collected in
a list and passed to the Pass1Lap function.  Pass1Lap looks at each
instruction and attempts to simplify it.  It looks on the property of
the opcode and checks to see if it has been flagged with 'MC.  If so, it
calls the function of the same name with the operands unchanged.  

Most ALM expansion functions first apply the function
@begin(verbatim)

	ResolveOperand(Reg, Source)

@end(verbatim)
to each operand, passing a temporary register as the first argument,
REG. This resolution process converts ALM operand forms into TLM
operand forms i.e, legal addressing modes of the TLM.
After each operand has been "resolved", the CMACRO pattern table
is used, and the resulting LIST of CMACROS processed recursively.

This is what is accomplished in the three functions:
@begin(verbatim)

	EXPAND1OPERANDCMACRO(Arg1,Name)
	EXPAND2OPERANDCMACRO(Arg1,ARg2,Name)
	EXPAND4OPERANDCMACRO(Arg1,ARg2,Arg3,Arg4,Name)

@end(verbatim)
which first resolves the arguments using the available registers and
then calls the routine (CMACROPATTERNEXPAND) which finds the pattern
table of the Name argument (ALM instruction) stored on the property list
under the indicator 'CMACROPATTERNTABLE.

For example, 
  (de !*WPlus2 (Arg1 Arg2)
      (Expand2OperandCMacro Arg1 Arg2 '!*WPlus2))

Only the (!*MOVE s d) ALM opcode tries to be smarter about temporary regs:
		d:=RESOLVEOPERAND('(Reg t2),d)
		If d is a register, then RESOLVEOPERAND(d,S)
		 else RESOLVEOPERAND('(REG t1),s);

[Future change - This should be changed in the future]

Recall also that Processing an arugment with RESOLVEOPERAND may
require other CMACRO's to be emitted first, to "simplify" the complex
addressing mode; each Operand is free to destroy/modify its given
register. For example, note how register t1 is reused below to
resolve multiple CAR's and CDR's into MOVE's and simpler CAR's and
CDR's:

 (!*MOVE (CAR (CAR x)) d) => (!*MOVE (CAR x) (REG t1))
                             (!*MOVE (CAR (REG t1)) d) 
 (!*MOVE (CAR (CAR(reg 1))) (CDR (CDR (reg 2))))
	 => (!*MOVE (CDR (reg 2)) (REG t2))
            (!*MOVE (CAR (REG 1)) (REG t1))
   	    (!*MOVE (CAR (reg t1)) (CDR (reg t2)))

Therefore, typically the operands are first processed before the ALM
instruction table is used.

AnyReg processing works the same way as with the ALM instructions.  The
operands are first resolved by calling the ResolveOperand function and
then ExpandOneArgumentAnyReg (or TwoArgument) is called to process the
pattern table.  This has also been combined into a single function:
OneOperandAnyReg and TwoOperandAnyReg.
[[WARNING - There is an inconsistency in the naming here.  For CMacro
expansion the combined functions are called EXPANDxOPERANDCMACRO where
for anyregs it is ONEOPERANDANYREG.  BE CAREFUL!!!!!!! Another
inconsistency is that CMacros are flagged with 'MC, which AnyRegs are
not flagged]]

@paragraph(ResolveOperand)
The ResolveOperand function takes two arguments, a temporary register
and the source to resolve.  It performs the following resolution, in the
order given:
@begin(Description)
an ID@\cals ResolveWConst on the ID;

number or string@\returned unchanged;

(OP s)@\If OP is flagged 'TerminalOperand, it is returned as is.

(OP s)@\If OP is an @anyreg (has an 'AnyregResolutionFunction), it is
applied to (Register s).

(OP s)@\Otherwise, it is examined to see if it is a WCONST expression.
@end(description)

The function ResolveWConst tests its operand to see if it is a constant
or constant expression, and returns its value.  It performs the
following resolution:
@begin(description)
(WCONST number)@\returns the number

ID@\If WCONST indicator is on the ID's property, the associated number
is returned otherwise the ID is returned.

Expression@\Each operand is tested to determine if it can be resolved as
a WCONST and if so, the function is applied to all of the operands (ANY
FUNCTION CAN BE CALLED)
@end(description)

?????Insert some SUMMARY USING THE FOLLOWING????????
Most ANYREGS use OneOperandAnyReg, ie recursively process arguments
inside out (CAR anyreg), (CDR anyreg), etc
%	(de AnyRegCAR(R S) (OneOperandAnyReg R S 'CAR))
%	(defAnyReg CAR AnyRegCar ....)

Those that do not permit anyregs as  args, use ExpandOneOperandAnyReg
eg, (QUOTE s), (WCONST w), (WVAR v), (REG r)
or flag name as TERMINALOPERAND to pass direct to ASM

so here is a simple WCONST expression.
As long as args are WCONSTEVALUABEL themselves, any
function can be applied:

@section(Predicates)
  Provided in the common machine independent files are a number of
useful predicates.  Those include:

[[[[List the predicates provided in common-predicates]]]]

Each of the following predicates expects one argument; call it X:
@begin(Description)
RegisterP@\(EqCAR X 'REG)  tests for any register

AnyP@\ Always  T, used as filler

EqTP@\ (equal X T)

MinusOneP@\(equal X -1)

InternallyCallableP@\Check if legal to make a fast internal call.
Essentially checks the following:
@begin(format)
[(or !*FastLinks
             % all calls Fastlinks?
 (and !*R2I (memq X EntryPoints!*)) 
             % or specially declared
      (FlagP X 'InternalFunction)
      (FlagP X 'FastLink)))]
@end(format)

AddressConstantP@\(or (NumberP X) (EqCar X 'Immediate)))
@end(Description)

@section(Standard ANYREGS)

The following are the basic @ANYREG functions, which in many cases
look for an AnyregTable:
@begin(Description)
@B[ID]@\@B[Flagged]

CAR@\OneOperandAnyreg, 'CAR table@comment{ need to explain all of these
                                           tables - particularly the WVar
                                           table }

CDR@\OneOperandAnyreg,  'CDR table

QUOTE@\ExpandOneArgumentAnyreg,  'QUOTE table

WVAR@\ExpandOneArgumentAnyreg,  'WVar table

REG@\ExpandOneArgumentAnyreg,  'REG table

WCONST@\OneOperandAnyreg,  'WConst table, default normally just SOURCE.

FRAME@\ExpandOneArgumentAnyreg, computes offset from stack pointer,
       and passes this (in bytes) to 'FRAME table

FRAMESIZE (Register)@\Computes (NAlloc!* @Value(Times)
AddressingUnitsPerItem) to give size of frame to any special code  needing it.

MEMORY (Register Source ArgTwo)@\Used to
compute indexed memory access: TwoOperandAnyreg, Look for 'MEMORY table.

LABEL@\Flags a label, does no processing.
@end(Description)

The implementor of @PSL for any particular machine is free to add additional
@ANYREG@xs (addressing modes), that are emitted as part of @CMACRO@XS by
machine specific compiler patterns or COMPFNs.


IMMEDIATE is a tag used to @ei[suggest] address or immediate constant.

@subsection(Some AUXILLIARY Operand Modes for the TLM)
Each of the following functions expects one argument; call it X:
@begin(Description)
UnImmediate@\If X @Value(Eq)(Immediate Y), removes tag to get Y.

ExtraReg@\Converts argument X into Access to ArgumentBlock[X-LastActualReg]

QUOTE@\Compiles X into a constant.  If !*ImmediateQuote is T, returns an
ITEM for object, else emits ITEM into a memory location, returns its address.
@end(Description)

Note @CMACRO@XS (flagged 'MC) are first expanded, then the PASS1PSEUDO@xs.
This means the @CMACRO@XS are able to insert and manage TAGS that are
removed or modified by final PASS1PSEUDO.


@section(more junk)
@i[Implement the Compiler Patterns and Tables].  This requires selecting
certain alternative routes and parameterizations allowed by the compiler,
trying to improve the match between the Abstract @PSL machine used by the
compiler and the target architecture X.  Mostly this phase is reserved for
optimization, but the basic tables have to be installed to map @xlisp
function names to corresponding @cmacro names and select the Compiler
functions (COMPFNs and OPENFNs) to be used for each construct.  This file,
@dq[xxxx-COMP.RED], is usually copied from one of the existing machines and
modified as needed. Most of the modifications relate to the legality of
certain addressing combinations. These tables are briefly described in the
Compiler chapter of the manual, but currently this task is still somewhat
"arcane".@comment{ There needs to be some mention of what the usual
modifications are! }

@i[Build and Test the CROSS Compiler].  Now compile a series of LAP (mostly
@CMACRO tests), @xlisp and
@syslisp files to X assembly code, link and run.  As the tests proceed,
certain small I/O and function calling procedures are written in LAP.  A
common way to do I/O is to implement a @ei[Foreign Function]-calling
protocol,  used from @xlisp to call functions according to
FORTRAN, PASCAL, C or other useful conventions.  Calls in compiled
@xlisp/@syslisp code to function names flagged with the 'FOREIGN-FUNCTION
flag are called with a non-@xlisp protocol.  This permits a
standard I/O library to be called and allows simple routines to be
written in another language.  The purpose of this separate
function-calling mechanism is to allow the @xlisp system to use the
most efficient calling method possible, compatible with the needs of
@syslisp and @xlisp.  This method is not necessarily the most flexible,
general, or safe method and need not be used by other languages.
However, to allow the @xlisp/@syslisp system to call upon existing
routines, particularly system-provided services, this additional
function-calling mechanism should be provided. Some care needs to be taken
to preserve and restore registers appropriately.

@chapter(Test Series)
In order to accomplish the PSL bootstrap with a
minimum of fuss, a carefully graded set of tests is being developed,
to help pinpoint each error as rapidly as possible. This section
describes the current status of the test files. The first phase
requires the coding of an initial machine dependent I/O package and
its testing using a familar system language.  Then the code-generator
macros can be succesively tested, making calls on this I/O package as
needed. Following this is a series of graded SYSLISP files, each
relying on the correct working of a large set of SYSLISP constructs.
At the end of this sequence, a fairly complete "mini-LISP" is
obtained.  At last the complete PSL interpreter is bootstrapped, and a
variety of PSL functional and timing tests are run.

@section(Basic I/O Support)
The test suite requires a package of I/O routines to read and print
characters, and print integers.  These support routines are usually written
in a "foreign" language (call it "F"), such as PASCAL, C or FORTRAN; they
could also be coded in LAP, using CMACROs to call operating system
commands, if simple enough. (E.g., JSYS's on DEC-20, Traps on 68000, etc.).
These routines typically are limited to using the user's terminal/console
for input and output. Later steps in the bootstraping sequence introduce a
more complete stream based I/O module, with file-IO.

On some systems, it is appropriate to have a main routine written in "F"
which initializes various things, and then calls the "LISP" entry point; on
others, it is better to have "LISP" as the main routine, and have it call
the initialization routines itself. In any event, it is best to first write
a MAIN routine in "F", have it call a subroutine (called, say TEST), which
then calls the basic I/O routines to test them.  The documentation for the
operating system should be consulted to determine the subroutine calling
conventions. Often, the "F" compiler has an "ASSEMBLY Listing switch",
which can be turned on to see how the standard "F" to "F" calling sequence
is constructed, and to give some useful guidance to writing correct
assembly code. This can also be misleading, if the assembler switch only
shows part of the assembly code, thus the user is cautioned to examine
both the code and the documentation.

On directory PT: (which stands for /psl/tests or <PSL.TESTS>), or its
subdirectories, we have a number of sample I/O packages, written in various
languages: PASCAL, FORTRAN, C and DEC20 assembly code. Each has been used
successfully with some PSL bootstrap. The primitives provided in these
files are often named XXX-yyyy, where XXX is the machine name, and yyyy is
the primitive, provided that these are legal symbols.  Of course, the name
XXX-yyyy may have to be changed to conform to "F" and the associated linker
symbol conventions. Each name XXX-yyyy will be flagged as a
"ForeignFunction", and called by a non-LISP convention.

The following is a brief description of each primitive, and its use. For
uniformity we assume each "foreign" primitive gets a single integer
argument, which it may use, ignore, or change (VAR c:integer in PASCAL).
@Comment{Is this assumed to be a WORD size quantity, i.e. on the 68000 a 32
bit quantity or can it be a small integer???}
The following routines ("yyyy") in LISP, will be associated with the
corresponding "foreign" routine "XXX-yyyy" in an appropriate way:
@begin(description)
init()@\Called once to set up I/O channels, open devices, print welcome
message,  initialize timer.

Quit()@\Called to terminate execution; may close all open files. 

PutC(C)@\C is the ASCII equivalent of a character, and is printed out
without line termination (I/O buffering may be needed). C=EOL=10 (ASCII LF)
@Comment{does this mean that the character should appear right away, or can
it wait till the EOL is sent???}
will be used to signal end-of-line, C=EOF=26 (ASCII SUB) will be used to
signal end of file.

GetC()@\Returns the ASCII equivalent of the next input character;
C=EOL=10 for end of line, and C=EOF=26 for end of file. Note it is
assumed that GetC does not echo the character.

TimC()@\Returns the runtime since the start of this program, in
milli-seconds, unless micro-seconds is more appropriate. For testing
purposes this routine could also print out the time since last called.

PutINT(C)@\Print C as an integer, until a SYSLISP based Integer printer that
calls XXX-PutC works. This function is used to print integers in the
initial tests before the full I/O implementation is ready.

@comment{Err(C)@\Called in test code if an error occurs, and prints C as an
error number. It should then call Quit() .}
@end(description)
The following functions will probably need to be defined in LAP, using
either the ALM (cmacro level ) or machine specific (TLM) level:
@begin(description)
!%Store!-Jcall(Code-Address,Storage-Address)@\The Storage-Address is
the address of the slot in the SYMFNC table where a jump instruction
to the Code-Address must be stored.  This implements a compiled call
to a compiled function.  You may have to insert padding or legal code
to make the code match the call to the compiled code.  The LAP for the
Dec20 is:
@begin(verbatim)

LAP
 '((!*entry !%Store!-Jcall Expr 2)
    % CodeAddress, Storage Address
   (!*alloc 0) 
   (!*WOR (reg 1) 8#254000000000)
    % Load a JRST in higher-bits
   (!*MOVE (reg 1) (memory (reg 2)
     (wconst 0)))
   (!*EXIT 0));

@end(verbatim)

!%Copy!-Function!-Cell(From-Address,To-Address)@\Copies the SYMFNC
cell located at the From-Address to the SYMFNC cell located at the
To-Address.  If your machine has the SYMFNC cell the same width as
that of MEMORY, the following code used on the Dec-20 will work:
@begin(verbatim)

LAP
 '((!*entry !%copy!-function!-cell
      Expr 2) % from to
   (!*alloc 0) 
   (!*move (memory (reg 1) 
                   (Wconst 0))
           (memory (reg 2)
                   (wconst 0)))
   (!*exit 0));

@end(verbatim)

UndefinedFunction()@\In general, we think of the storage of the number
of arguments in a register (Reg NargReg) and the index of the called
function in a register (Reg LinkReg).  This function must store the
linkage register in the fluid UndefnCode!* and the Narg register in
the fluid UndefnNarg!*.  Finally, it must !*JCALL to the
UndefinedFunctionAux.  The following code implements this function in
a manner that is portable across all machines that use the LinkReg and
NargReg as real register:
@begin(verbatim)

FLUID '(UndefnCode!* UndefnNarg!*);

LAP 
 '((!*ENTRY UndefinedFunction expr 0)
    % No alloc 0 ? and no LINKE 
    %  because we don't want to 
    %  change LinkReg.
   (!*Move (reg LinkReg)
           (Fluid UndefnCode!*))
   (!*Move (reg NargReg) 
           (Fluid UndefnNarg!*))
   (!*JCALL UndefinedFunctionAux)
);

@end(verbatim)

Flag(Dummy1,Dummy2)@\A call to this function is automatically
generated by the compiler, but is never used.  So, you must implement
this function to call your error routine if it is actually called
(This function will be redefined in a later test).  The code for the
Dec-20 is portable except the linkage to the Machine Dependent Error
routine Err20:
@begin(verbatim)

LAP '((!*ENTRY FLAG expr 2)
      (!*alloc 0) 
      (!*MOVE  2 (REG 1))
      (!*LINKE 0 Err20 Expr 1)
);

@end(verbatim)
@end(description)
Finally, the following three functions must be implemented to allow
arithmetic operations of sufficient length.
@begin(description)
LongTimes(Arg1,Arg2)@\Compute the product of Arg1 and Arg2 and return:
@begin(verbatim)

procedure LongTimes(x,y);
  x*y;

@end(verbatim)

LongDiv(Arg1,Arg2)@\Compute the quotient of Arg1 and Arg2 and return
the value:
@begin(verbatim)

procedure LongDiv(x,y);
  x/y;

@end(verbatim)

LongRemainder(Arg1,Arg2)@\Compute the Remainder of Arg1 with respect
to Arg2:
@begin(verbatim)

procedure LongRemainder(x,y);
  Remainder(x,y);

@end(verbatim)
@end(description)

As a simple test of these routines implement in "F" the following.
Based on the "MainEntryPointName!*" set in XXX-ASM.RED, and the
decision as to whether the Main routine is in "F" or in "LISP",
XXX-MAIN() is the main routine or first subroutine called:
@begin(verbatim)
% MAIN-ROUTINE:
	CALL XXX-INIT(0);
        CALL XXX-MAIN(0);
        CALL XXX-QUIT(0);

% XXX-MAIN(DUMMY):
    INTEGER DUMMY,C;

	CALL XXX-PUTI(1);  % Print a 1 for first test
        CALL XXX-PUTC(10); % EOL to flush line

	CALL XXX-PUTI(2);  % Second test
        CALL XXX-PUTC(65); % A capital "A"
        CALL XXX-PUTC(66); % A capital "B"
        CALL XXX-PUTC(97); % A lowercase "a"
        CALL XXX-PUTC(98); % A lowercase "b"
        CALL XXX-PUTC(10); % EOL to flush line

	CALL XXX-PUTI(3);  % Third test, type "AB<cr>"
        CALL XXX-GETC(C);
         CALL XXX-PUTC(C); % Should print A65
         CALL XXX-PUTI(C);
        CALL XXX-GETC(C);
         CALL XXX-PUTC(C); % Should print B66
         CALL XXX-PUTI(C);
        CALL XXX-GETC(C);
         CALL XXX-PUTI(C); % should print 10 and EOL
         CALL XXX-PUTC(C);

	CALL XXX-PUTI(4);  % Last Test
	CALL XXX-ERR(100);

        CALL XXX-PUTC(26); % EOF to flush buffer
        CALL XXX-QUIT(0);
% END

@end(verbatim)

For examples, see PT20:20IO.MAC for DEC-20 version, PHP:HP.TEXT for HP9836
PASCAL version, PCR:shell for CRAY fortran version.

@section(LAP-TO-ASM and CMACRO Tests)
After the basic XXX-ASM.RED file has been written and the XXX-CROSS.EXE has
been built, and seems to be working, an exhastive set of CMACRO tests
should be run. The emitted code should be carefully examined, and the
XXX-CMAC.SL adjusted as seems necessary.  Part of the CMACRO tests are to
ensure that !*MOVEs in and out of the registers, and the ForeignFunction
calling mechanism work.

The goal of this test, and the following few sections is to guide you
in getting the first piece of ALM code to translate to TLM form,
correctly assemble, and finally execute on the target machine. There
are a large number of details to worry about, and one will have to
come back and refine decisions a number of times. Some of the
decisions you will have to make are based on incomplete information,
and are based on an interaction of the ALM model, LISP usage
statistics and unknown oddities of the target machine. In many cases,
you will have to make the decision just to proceed to get the skeleton
together, and then immediately come back to fix the code.

The first major milestone will be to set up enough of the basic
cross-compiler to be able to translate and assemble the following
file, called PT:MAIN0.RED:
@begin(verbatim)
% MAIN0.RED - A "trivial" file of ALM level LAP to test
%              basic set of tools: LAP-TO-ASM mostly,
%              and CMACROs

LAP '((!*ENTRY DummyFunctionDefinition Expr 1)
      (!*ALLOC 0)
      (!*MOVE (REG 1) (REG 2))
      (!*EXIT 0));

END;
@end(verbatim)


It consists of a single procedure, written in LAP using only 4
CMACROs, each quite simple. Notice the procedure defined has a "long"
name, which may have to be mapped to a simpler symbol (for your
assembler) by a routine in your xxx-ASM.RED file.  The !*ENTRY cmacro
is actually handled by LAP itself, so there are 3 CMACROs to be
written: 
@Begin(description)

(!*ALLOC n)@\Issues instructions to
allocate a frame of n items on the stack. May also have to issue
instructions to check stack overflow if the system hardware does not.
For some machines, with n=0, no code is emitted, while for others,
!*ALLOC is a good place to establish certain registers for the code
body. (On the CRAY, the call instruction puts the return address in
a register, which get saved on the stack in the !*ALLOC).

(!*MOVE source dest)@\Issue code to move the contents of source to
the destination. In the MAIN0 example, a register to register move is
desired. ALM (REG 1) and (REG 2) are almost always allocated to real
TLM registers. An "anyreg" for the REG mapping will have to be
written.

(!*EXIT n)@\Issues code to clean up the stack, by removing the frame
that was allocated by a corresponding (!*ALLOC n), and then returns
to the caller, whose address was saved on the stack (usually) by
an appropriate  TLM instruction. (On CRAY, the return address
is restored to the special register).
@end(description)

Here is an example of the processing of this file on the
DEC-20. On the DEC20 we produce 2 files, the CODE-FILE and the DATA-FILE:

@begin(verbatim)
CODE-FILE, MAIN0.MAC

DATA-FILE, DMAIN0.MAC
@end(verbatim)
In summary, here are the initial steps you will have to follow, with some
indication of the decisions you will have to make:

@begin(description)
Decide on PSL Item layout@\How many bits for the tag; should there be
a GC field; will the tag have to be masked out when the INF field is
used as an address; should the fields be aligned to byte, word or
other boundaries to make TAG and INF access faster;


Decide on TLM register use@\Some registers will be used for the ALM
registers (rest simulated by memory locations), some used for CMACRO
temporaries, some for Target OS interface or addressibility, some for
Linkage registers and some for the stack.

Stack Implementation@\Should the LISP stack be same as system stack; can we
use stack hardware; how about stack overflow; which way should stack
grow; ALM needs to access elements inside the stack relative to the
stack pointer; the stack pointer needs to be accessible so that the GC
and other things can access and examine elements.  

@end(description)

@section(More details on Arcitecture mapping)
Need to explain why currently 1 tags used, expect more or less in future.
Perhaps explain which tests are MOST important so at least those can be done
efficiently, even if others encoded in a funny wya.

Mention idea that in future may want to put (say) 3 bits of tag in lower
word, force double or quadword alignment, and put rest of tag in object.
Mention how some data-types are immediate, others point into memory,
and some already have headers. Mention possibel user-defind extension types.


Need to clarify how ALM registers are used so can be mapped to
TLM or memory.

Need to explain Stack registers, CMACRO temporary registers, link
registers.

Need to explain relative importance of certain CMACROs and order in
which they should be written and debugged. Make a CMACRO test file to
be examined by hand, to be assembled, and maybe even run.

Need to give more detailed steps on how to get MAIN1 running; seems
like a BIG step. Perhaps break down into smaller MAIN0, just to get
off the ground. (Ie, might not execute, but should assemble).  Give a
check list of steps. Explain that at first, just get all pieces
together, then can fill in details once the skeleton is correct, and
flesh out stubs.

Explain data-file versus code-file model.

@section(SysLisp Tests)
This set of tests involve the compilation to target assmbly code, the
linking and execution of a series of increasingly more complex tests. The
tests are organized as a set of modules, called by a main driver.  Two of
these files are machine dependent, associating convenient LISP names and
calling conventions with the "Foreign" XXX-yyyy function, define
basic data-spaces, define external definitions of them for inclusion, and
also provide the appropriate MAIN routine, if needed. These files
should probably be put on a separte subdirectory of PT: (e.g., PT20:,
PT68:, etc.)

The machine dependent files are:
@begin(description)

XXX-HEADER.RED@\Is a machine dependent "main" include file, read into each
MAINn.RED file, to define the data-spaces needed, and perhaps define a main
routine in LAP, and have the appropriate XXX-MAIN call the "FirstCall"
function, used to start the body of the test. Also included are the
interface routines to the "F" coded I/O package.  providing a set of LISP
entry-points to the XXX-yyy functions.  This should be copied and edited
for the new target machine as needed. Notice that in most cases, it simply
defines "procedure yyyy(x); XXX-yyyy(x);", relying on "ForeignFunction"
declaration of XXX-yyyy.  

XXX-TEST-GLOBAL-DATA.RED@\This contains a series of external declarations
to correspond to the Global Data definitions in the above header file
file. It is automatically included in all but the MAINn module via the
"GlobalDataFileName!*" option of XXX-ASM.RED.
@end(description)
The machine independent test files and drivers are:
@begin(description)
MAIN1.RED@\Is a very simple driver, that calls Getc and Putc, does a few
tests.  It does an 'IN "XXX-HEADER.RED";'. The "FirstCall" procedure
then calls "init", uses "putc" to print AB on one
line.  It should then print factorial 10, and some timings for 1000 calls
on Factorial 9 and Tak(18,12,6). Build by itself, and run with IO.
@Comment{This seems to hide the assumption that 10! can be done in the
integer size of the test implementation.??? }

SUB2.RED@\Defines a simple print function, to print ID's, Integer's,
Strings and Dotted pairs in terms of repeated calls on PutC.  Defines
PRIN1, PRIN2, PRINT, PRIN2T, TERPRI and a few other auxilliary print functions
used in other tests. Tries to print "nice" list notation.

MAIN2.RED@\Tests printing and access to strings.  It peforms most of the
useful string operations, printing messages to verify that they
function properly.
Uses Prin2String to print a greeting, solicit a sequence of
characters to be input, terminated by "#". Watch how end-of-line is handled.
Then Print is called, to check that TAG's are correctly recognized,
by printing a LISP integer, an ID and 2 dotted pairs. Requires SUB2
and IO modules.  Finally, it tests the undefined function calling
mechanism to verify that it does print out an error message.
Therefore, the UndefinedFunction routine must be defined in xxx-header
by this test 2.

SUB3.RED@\Defines a mini-allocator, with the functions GtHEAP, GtSTR,
GtVECT, GtCONS, Cons, XCons, NCons, MkVect and MkString.  Requires
primitives in SUB2 module.

MAIN3.RED@\First Executes a Casetest, trying a variety of Branches and
Defaults in the case staement. There are a number of calls on Ctest with an
integer from -1 to 12; Ctest tries to classify its argument using a case
statement.  ConsTest simply calls the mini-allocator version of CONS to build
up a list and then prints it.  Requires SUB2, SUB3 and IO modules.

SUB4.RED@\Defines a mini-reader, with InitRead, RATOM and READ.  It
has the facilities to convert case input, using the !*RAISE switch
(and the SetRaise function).  This mini-READ does not yet read vectors.
Requires SUB3, SUB2, and IO modules.

MAIN4.RED@\First, this test checks to see that EQSTR works.  Then it
tests FindId to see if it can find Identifiers known to exist.  After
that, it tests to see if new Id's can be found and then found in the
same place.  Then a test loop is created that calls RATOM, printing
the internal representation of each token.  Type in a series of id's,
integer's, string's etc.  Watch that the same ID goes to same place.
When the user types a Q, it should go into a READ-PRINT loop.  You
should type in a variety of S-Expressions, checking that they are
correctly printed.  Once again, you should finally type a Q to exit.
Requires SUB3, SUB2 and IO modules.

SUB5.RED@\Defines a mini-EVAL. Does not permit user defined functions.
Can eval ID's, numbers, and simple forms. No LAMBDA expressions can be
applied.  FEXPR Functions known are: QUOTE, SETQ, COND, PROGN and
WHILE. The Nexpr LIST is also known.  Can call any compiled EXPR, with
the standard 15 arguments. Requires SUB4, SUB3, SUB2 and I/O.

MAIN5.RED@\Starts a mini-READ-EVAL-PRINT loop, to which random simple
forms may be input and evaluated. When ready, input (TESTSERIES) to
test PUT, GET and REMPROP. Then an undefined function is called to
test the UNDEFINED function mechanism.  Requires SUB5, SUB4, SUB3,
SUB2 and IO modules.  Note that input ID's are case raised (!*RAISE
has been set to T by default) so input can be in in lowercase for
built-in functions.  Terminates on Q input.

SUB6.RED@\Defines a more extensive set of primitives to support the
EVAL, including LAMBDA expressions, and user defined EXPR, FEXPR,
NEXPR and MACRO functions. This is a complete model of PSL, but has a
restriced set of the PSL functions present.  Can call any compiled or
interpreted function.  Requires SUB5, SUB4, SUB3, SUB2 and I/O.

MAIN6.RED@\Tests the full PSL BINDING modules (PI:BINDING.RED and
PT:P-FAST-BINDER.RED). Call the (TESTSERIES) routine to do a test of
Binding, the Interpretive LAMBDA expression evaluator, and binding in
compiled functions.    Requires SUB6,SUB5, SUB4,
SUB3, SUB2 and IO modules.  !*RAISE is once again on.  Terminates on Q
input.

SUB7.RED@\A set of routines to define a minimal file-io package, loading
the machine independent files: PT:SYSTEM-IO.RED and PT:IO-DATA.RED, and a
machine dependent file XXX-SYSTEM-IO.RED. The latter file defines
primitives to OPEN and CLOSE files, and read and write RECORDS of some
size. The following definitions are used in the routines: 
@begin(verbatim)
FileDescriptor: A machine dependent
   word to references an open file.
FileName:       A Lisp string
@end(verbatim)
@begin(description)
SYSCLEARIO()@\Called by Cleario to do any machine specific initialization
needed, such as clearing buffers, initialization tables, setting interrupt
characters, etc.

SysOpenRead(Channel,FileName)@\Open FileName for input and return a file
descriptor used in later references to the file. Channel may be used to
index a table of "unit" numbers in FORTRAN-like systems.

SysOpenWrite(Channel,FileName)@\Open FileName for Output and return a file
descriptor used in later references to the file. Channel may be used to
index a table of "unit" numbers in FORTRAN-like systems.

SysReadRec(FileDescriptor,StringBuffer)@\Read from the FileDescriptor, a
record into the StringBuffer.  Return the length of the string read.

SysWriteRec (FileDescriptor, StringToWrite, StringLength)@\ StringLength
characters from StringToWrite from the first position.

SysClose (FileDescriptor)@\Close FileDescriptor, allowing
it to be reused.

SysMaxBuffer(FileDesc)@\Return a number  to allocate the file-buffer
as a string; this should be maximum for this descriptor.
@end(description)
RDS, WRS, OPEN, CLOSE, DSKIN and TYPEFILE are defined.

MAIN7.RED@\Starts the LISP READ-EVAL-PRINT loop tested before, and now
permits the user to test io. Call (IOTEST). Other functions to try are
(OPEN "foo" 'OUTPUT), (WRS n), (RDS n) etc. [Now the GETC and PUTC IO
routines in XXX-HEADER will finally call the file-oriented
IndependentReadChar and IndependentWriteChar].  Also includes the
standard PSL-TIMER.RED (described below), which can be invoked by
doing (DSKIN "PT:TIME-PSL.SL").  Since the garbage collector not yet
present, may run out of space.

FIELD.RED@\A a set of extensive tests of the Field and Shift  functions.
Needs a WCONST BitsPerWord defined in XXX-HEADER.RED. Build by itself,
and execute with the IO support.
@end(description)

Test set "n" is run by using a set of command files to set up
a multi-module program. These files are stored on the
approriate subdirectory (PT20: for the DEC20). Note that each module
usually produces 2-3 files ("code", "data" and "init")
@begin(Enumerate)
First Connect to the Test subdirectory for XXX:
@verbatim[
@@CONN PTxxx:]

Then initialize a  fresh symbol table for program MAINn, MAINn.SYM:
@verbatim[

@@MIC FRESH MAINn]

Now successively compile each module, SUB2..SUBn
@verbatim[
@@MIC MODULE SUB2,MAINn
@@MIC MODULE SUB3,MAINn

@@MIC MODULE SUBn,MAINn]

Now compile the MAIN program itself
@verbatim[
@@MIC PROGRAM MAINn]

As appropriate, compile or assemble the output "F" language modules
(after shipping to the remote machine, removing tabs, etc..). Then
"link" the modules, with the XXX-IO support, and execute. On the
DEC-20, the 
@verbatim[
@@EX @@MAINn.CMD]

command files are provided as a guide]

Rather than including output from some older test runs, we insist that
you run the tests yourself on the HOST machine to be absolutley sure
of what output they produce, and what input is expected. Also, if
errors occur during testing, the examination of the HOST tests will
help. This will also help as additonal tests are added by new
implementors.
@end(enumerate)
@section(Mini PSL Tests)

The next step is to start incorporating portions of the PSL kernel into the
test series (the "full" Printer, the "full" reader, the "full" Allocator,
the "full" Eval, etc.), driving each with more comprehensive tests. Most of
these should just "immediately" run. There some peices of Machine specific
code that have to be written (in LAP or SYSLISP), to do channel I/O,
replacing the simple XXX-IO; to do fast APPLY; Fluid Binding and
Arithmetic. This set of tests will help check these peices out before
getting involved with large files.

@section(Full PSL Tests)
Now that PSL seems to be running, a spectrum of functional tests and timing
tests should be run to catch any oversights, missing modules or bugs, and as a
guide to optimization. The following tests exist:
@Description[
PSLTEST.SL@\A fairly comprehensive test of the Standard LISP subset of PSL.
Do (DSKIN "pt:psltest.sl"). There are a few tests of the error mechanism that
have to be "pushed" through for a full test.

MATHLIB.TST@\A series of tests of MATHLIB. First LAOD MATHLIB; into RLISP,
then do IN "MATHLIB.TST"; .

PSL-TIMER.SL, TIME-PSL.SL@\A standard timimg test covering PSL basics.
Compile PSL-TIMER.SL into kernel, or with resident compiler, then
(LAPIN "PT:TIME-PSL.TEST").
]

@section(Stabilize Basic PSL)
Finally, compile the kernel modules of @PSL, link with the
additional machine-dependent modules, and @PSL (hopefully) comes right
up@Foot[Presently an unlikely possibility, as the system may still change
arbitrarily from under the implementor!]. Additional work is underway to
develop a much more comprehensive test set, that will not change while the
implementor is proceeding with the bootstrap; unfortunately, @PSL is still
undergoing continuous development at Utah, resulting in some "out-of-phase"
communication problems.

After the basic interpreter is working, additional modules can also be
compiled from @xlisp to X and linked with the kernel.  The most common of these
might be the @RLISP parser and even the @REDUCE@cite[Hearn73] computer
algebra system@Comment{???or should this be symbolic algebra system??? }.  As
more files are compiled to machine X and linked, the task
becomes more tedious.  At this point, we need to consider the bootstrap of
the @ei[Resident] Compiler, LAP and fast-loader (FASL).  The most common way
to build and maintain large @PSL programs is to build the kernel @PSL with a
resident FASLIN for loading fast-load files, and then compile required
modules to FASL (xxxx.b) files.  A @PSL-based system is built by loading the
appropriate FASL files, and then saving the @dq[core] image as an
executable file.  On some machines this is easy; on others it is quite
hard; see the discussions below.

These additional steps are:

@begin(enumerate)
@i[Implement Resident LAP].  Using an existing LAP.RED as a guide, write a
table-driven program that does the actual assembly of code written in
LAP form for machine X, to the appropriate bit-patterns; the details of
this process are discussed at length in @dq[Reading, Writing and Testing
LAP]@cite[Griss82h].  @PSL provides many tools to make this task quite
easy, but the process is still very machine dependent. Future work may
lead to the use of an architectural description language.

@i[Test LAP].   The depositing of bit-patterns into
BPS@Foot[BPS is Binary Program Space.  The name BPS is a remnant of
@xlisp 1.6.  The desire to have a separate code space is based on the desire
to @ei<not> relocate compiled code.] needs to be checked.  Check also that
procedures can be constructed with LAP, compile LAP into the kernel,
and assemble some small files.

@i[Implement FASLIN].  FASLIN requires some binary I/O and other small
support procedures described in a separate section below.


@i[Implement FASLOUT].  Once LAP works, the FASLOUT process seems quite
simple, requiring only the Binary I/O etc@. used by FASLIN.  It should be
possible to get xxxx-FASLOUT working on an existing @PSL, and cross-FASL
for machine X.  This has not yet been tested.  When it works, FASLIN could be
made part of the @PSL kernel very early on.

@i[Test FASL files].  Check that FASL files can be easily written and read.
@Comment{What kind of tests should be done??? This "easily written and
read" sounds like apple pie, but it would seem that a piece of SYSLISP
could be written that would give the FASL mechanism a good work out,
perhaps two pieces with cross references to one another. }

@i[Implement and test Core saving].  Determine how to save the image of an
executing program, so that it can be restarted.  We only require that it be
restarted at the beginning, not where it was when it was saved.  We usually
change the MAIN entry function to call an appropriate TopLoop.
See the more extensive discussion below.
@foot[Actually, the only part which
must be saved is the impure data part; the pure data section, the pure code
section and the control stack need not be preserved - however, if only the
impure data part is saved, the restart mechanism must map the pure data and
code back in.  For an example of programs which do selective dumping see
EMACS MKDUMP and @interlisp SYSOUT.  @Comment{We probably need to think
about some way of loading the libraries similar to EMACS, such that it is
easy to reload the libraries (particularly if they remain pure).}]
@end(enumerate)

@chapter(DETAILED REFERENCE MATERIAL)

@section(Details on the ALM Operand forms)

The following are references to a variety of memory locations: In the
current implementation the following 4 reference the same location,
the SYMVAL cell of the associated ID. This is the contents of the
location SYMVAL+AddressingUnitsPerItem*IDLOC(id):
@begin(verbatim)
(FLUID name:id)
(!$FLUID name:id)
(GLOBAL name:id)
(!$GLOBAL name:id)
@end(verbatim)

@begin(description)
(WVAR name:id)@\This references the contents of the static location
named by the ID.
@end(description)

The following are all constants, either absolute bit-patterns, or
address expressions.

@begin(description)
(WARRAY name:id)@\Address of the base of a static array

(WSTRING name:id)@\Address of the base of a static string

(WCONST expr:wconst-expression)@\Any constant expression, either
numeric, a declared constant, addresses of thinsg that could also be
passed as WARRAY or WSTRING, or other expressions that can be handled
by the TLM assembler.

(IMMEDIATE wconst-expression:any)@\Really only introduced as a "tag"
to make later processing easier; a constant is either an explict
constant or (IMMEDIATE expression). This is default TLM mode wrapped
when RESOLVEOPERAND is "unsure".  We are confused about the
differences between WConsts and Immediates in some cases.

(QUOTE s-exp:s-expression)@\Is the constant bit-pattern representing a
tagged PSL item.

(LABEL l:id)@\Reference to a local location (symbol) in the current
set of ALM instructions, processed in a single call to LAP, usually a
single function.

(MEMORY base:any offset:wconst-expression)@\This is the basic ALM "indexing"
operation, and represents the contents of the location (base)+offset. 

(CAR base:any)@\Reference the contents of the ITEM pointed at by
INF(base).  It is assumed that base is actually a PAIR (not checked).
In principle this is sort of like (MEMORY (INF base) (WCONST 0)).

(CDR base:any)@\Refernce the contents of the ITEM pointed at by
INF(base).  It is assumed that base is actually a PAIR (not checked).
In principle this is sort of like (MEMORY (INF base) (WCONST
AddressingUnitsPerItem)).


(FRAME n:integer)@\Contents of the n'th location in the current stack
frame.  In most versions of the ALM, there is an explicit register,
(REG ST), which points at the base of the frame. The stack grows in
some direction determined by features on the TLM, so that this could
in principle be expressed as (MEMORY (reg ST)
  (WCONST (times StackDirection -1 AddressingUnitsPerItem (SUB1 n))))

(REG reg-descriptor:{integer,id})@\Reference to an ALM  register.

(LIT [any-instruction-or-label:{list,id}])@\Plants the instruction sequence
elswhere, and leaves a reference to its start. Essetially equivalent to
	(label g), with g starting a block of the instructions, in "literal"
	space.

(LABELGEN tag:id)@\A mechnism (with LABELREF) to generate and
reference a label local to a particular CMACRO pattern. Meant mostly
for implementing conditional jumps of various kinds.

(LABELREF tag:id)@\Reference a label that was assigned to the Tag.
@end(description)


The following set of ALM instruction forms are used to define constant data
which is intermixed with instructions.

@begin(description)
(FULLWORD [exp:wconst-expression])@\The expressions are deposited in
successive "words" (item-sized units).

(HALFWORD [exp:wconst-expression])@)\The expressions are deposited in
succesive halfwords (two per item-sized unit).

(BYTE [exp:wconst-expression])@\The expressions are deposited in successive
"bytes" (character-sized units).

(STRING s:string)@\The ASCII values of the characters of the string are
deposited in successive bytes, terminated by a zero byte.

(FLOAT f:float)@\The 2 word bit pattern for the floating point number is
deposited.
@end(description)

These must be processed by the TLM to ASM translator (and later by the resident
assmbler).


@subsection(Standard @CMACRO@xs)

The following are the basic @CMACRO@XS; additional @CMACRO@XS are of course
frequently added either to aid in writing the @CMACRO@XS (a @CMACRO
@ei[subroutine]), or to aid some aspect of the machine-specific details.
Recall that each @CMACRO returns a list of LAP instructions (which are simpler
to generate code for, although it may be a more complex list of operations)
representing the appropriate expansion of this @CMACRO (these may also call
other @CMACRO@XS).  These instructions are then recursively processed by the
@CMACRO expander (i.e@. LAP).  The !*MOVE @CMACRO is very commonly used for
this purpose, to get a @ei[general] operand into a register, so the
particular @CMACRO can operate on it.

The following @CMACRO@XS deal with function ENTRY, EXIT and function call:


@begin(Description)
!*Entry((FunctionName FunctionType NumberOfArguments)@\Normally the user
does not code this @CMACRO, since it is processed completely by LAP
itself.  It is used to indicate the start of a function (or entry point
within a function).  Normally just plants a label corresponding to
FunctionName.

!*Exit (N)@\Exits (@dq[returns]) from procedure, deallocating N items, as
needed.  N corresponds to the N items allocated by !*Alloc, see below.

!*Link (FunctionName FunctionType NumberOfArguments)@\If FunctionName
is flagged 'FOREIGNFUNCTION, emit a call (!*ForeignLink FunctionName
FunctionType NumberOfArguments), else emit a (!*Call FunctionName).
This is the basic function call macro.  It assumes the appropriate
number of arguments are in the registers (previously loaded) in the
registers, @w[(REG 1) ... (REG n)].  We currently do not check either
NumberOfArguments or FunctionType, so a simpler @CMACRO, !*CALL is
provided for basic function call.

!*Call (FunctionName)@\Basic or @dq[Standard] function call.  Checks
to see if FunctionName has an 'OPENCODE property, and returns the
stored instruction list if any.  Otherwise it looks for an
appropriate pattern table stored by DEFCMACRO under
'CMACROPATTERNTABLE, as described above.

!*LinkE (DeAllocCount FunctionName FunctionType NumberOfArguments)@\An
@dq[exit] call.  Emitted when the caller does not need to examine the
result, but returns it directly.  The !*LinkE @CMACRO does not save
the return address, so a return from the called function is not to
this caller, but to the previous !*LINK.  Essentially deallocates the
frame (if any), does either an ordinary !*ForeignCall and then
!*Exit(0), or does a !*JCALL which does no return address saving.

!*JCall (FunctionName)@\First checks for an EXITOPENCODE table, then
for an OPENCODE table (followed by a normal return, !*EXIT(0)) or
looks for the general '!*JCALL table.  The generated code is supposed
to call the function without saving a return address, essentially a
JUMP.

!*ForeignLink (FunctionName FunctionType NumberOfArguments)@\
This is the basic linkage to a foreign function.  It assumes the appropriate
number of arguments are in the registers (previously loaded) in the
registers, @w[(REG 1) ... (REG n)].  It then pushes the arguments on a
stack, or moves them to a global location, as appropriate and
transfers to the ForeignFunction in an appropriate manner (REWRITE).
Some care must be taken in interfacing to the LISP world, with cleanup
on return.
@end(description)

The following @CMACRO@XS handle the allocation and deallocation of a Frame of
temporary items on the stack, used for argument saving, PROG local
variables, etc.


@Begin(description)
!*Alloc (N)@\Allocates a frame of N @Value(Times)
AddressingUnitsPerItem units by adjusting the stack (generally
increasing it) by using a stack operation that invokes an overflow
signal, if any.  Otherwise the stack register should be compared
against an appropriate UpperBound.  It passes N @Value(Times)
AddressingUnitsPerItem to the pattern, to be used for indexing or
displacement.  Note some stacks grow in the @ei[negative] direction,
and this is a major source of @CMACRO errors.  Currently, there is a
major problem, that this MACRO may not be called recursively.  FIX in
the future.

!*DeAlloc (N)@\Decrement stack by N @Value(Times) AddressingUnitsPerItem units,
deallocating the temporary FRAME.  Passes N*AddressingUnitsPerItem to the
pattern.
@end(Description)

The following @CMACRO@XS deal with the binding and unbinding of FLUID
variables used as Lambda or Prog parameters.  They are usually quite
complex to code.  The basic idea is to follow the call on a Lambind or
Progbind procedure by a compact table of Fluid addresses or offsets.  The
call may have to be special, and @ei[internal], so that the support code
(usually hand-coded in LAP) can pick up and process each entry in the
compact table.


@begin(Description)
!*LamBind(Registers FluidsList)@\Registers is of the form
@w[(REGISTERS (REG a) (REG b) ... (REG c))], and FluidsList is of the form
@w[(NONLOCALVARS (FLUID f) ...)].  The intent of this @CMACRO is to save the
current value of each
Fluid in the list on the Binding Stack, paired with the Fluid name.  Then
the value in the corresponding register is stored into the Value cell.
Later unbinding by !*FreeRstr or the Catch and Throw mechanism, restores
the saved value.

!*ProgBind (FluidsList)@\Emitted for Fluid variables in Prog parameter
lists.  Idea is as above, but stores a NIL in the value cell after saving
the old contents.  Usually implemented as
@w[(!*LamBind '(REGISTERS) FluidsList))], but may be able to use a more compact
table.

!*FreeRstr (FluidsList)@\Restores the old values of the fluids.  Since we use
a special binding stack with Fluid names stored on it, we really only need the
number to unbind.  [Perhaps we should use !*UnBind(N) to make this decision
explicit.]
@end(Description)

Data-moving @CMACRO@XS.  Most of the work is done by !*MOVE, with some PUSH/POP
optimizations if the !*MOVE is close to an !*ALLOC or !*DEALLOC.  Other data
moving may be done in conjuction some of the operations, such as !*WAND,
!*WOR, !*WPLUS2, !*WMINUS, etc.


@begin(Description)
!*Move (Source Destination)@\The major work horse.  Generates code to move
SOURCE to DESTINATION.   Uses (REG t1) and (REG t2) as temporary
registers if needed.  First simplifies destination (@ei[Anyreg resolution]),
using (REG t1) as a temporary if needed.  It then simplifies the SOURCE,
using the as temporary either the destination (if a register), or (REG
t2).  Finally, the !*MOVE table is used.

!*Push (Arg1)@\Emitted during peep hole optimization to
replace a pair !*ALLOC(1) and !*MOVE(arg1,(FRAME 1)).  This is a very common
optimization.

!*Pop (Arg1)@\Emitted during the peep hole phase
to replace the common pair !*MOVE((FRAME 1),Arg1), followed by
!*DEALLOC(1).  This modifies the argument ARG1.

@end(Description)

The JUMP @CMACRO@XS are given the label as the first operand, but
they pass the label as the third (and last) argument to the pattern
(usually as ARGTHREE) after resolving the other arguments.  The label
is tagged (LABEL Label).


@begin(Description)

@begin(group)
!*Lbl (Label)@\This @CMACRO is emitted when a label is inserted in the
generated code.  Its body is usually trivial, but can be more complex
if some form of short and long jump optimization is  attempted.
@hinge

!*Jump (Label)@\Emit code to jump to Label.  Label often involves memory.
@hinge

!*JumpEQ (Label Arg1 Arg2)@\Generate  code to JUMP if Arg1 EQ Arg2.
Used for @xlisp EQ and @syslisp WEQ.
@hinge

!*JumpNotEQ (Label Arg1 Arg2)@\Generate code to JUMP if not(Arg1 EQ Arg2).
Used for @xlisp EQ and @syslisp WEQ.
@hinge

!*JumpWLessP (Label Arg1 Arg2)@\Generate code to JUMP if Arg1 @Value(LT) Arg2.
Used for @syslisp WLESSP.
@hinge

!*JumpWGreaterP (Label Arg1 Arg2)@\Generate code to JUMP if Arg1 @Value(GT) Arg2.
Used for @syslisp WGREATERP.
@hinge

!*JumpWLEQ (Label Arg1 Arg2)@\Generate code to JUMP if Arg1 @Value(LTE) Arg2.
Used for @syslisp WLEQ.

!*JumpWGEQ (Label Arg1 Arg2)@\Generate code to JUMP if Arg1 @Value(GTE) Arg2.
Used for @syslisp WGEQ.

!*JumpType (Label Arg TypeTag)@\Generate code to JUMP if TAG(Arg)
@Value(Eq) TypeTag.  The TypeTags are small integers, defined in the
xxxx-Data-Machine file.  This @CMACRO is emitted for opencoded Type
checking, such as IDP(x), etc.  It should be implemented very efficiently.
Instead of extracting the TAG and comparing with the small integer, it may
be easier just to mask the INF portion of Arg, and compare with a shifted
version of TypeTag (previously saved, of course).
@hinge

!*JumpNotType (Label Arg TypeTag)@\Generate code to JUMP if not(TAG(Arg)
@Value(Eq) TypeTag).  See comments above.
@hinge

!*JumpInType (Label Arg TypeTag)@\Generate code to JUMP if Tag(Arg) is in the
range @w([0 ... TypeTag,NegInt]).  This is used to support the numeric
Types, which are encoded as 0,...M, and -1 for negative Inums.  Thus NumberP,
FixP, etc@. have to test a range.  Note that NegInt is tested specially.
@hinge

!*JumpNotInType (Label Arg TypeTag)@\Generate code to JUMP if Tag(Arg) is
not in the range @w([0 ... TypeTag, NegInt]).  See above comment.
@hinge


!*JumpOn (Register LowerBound UpperBound LabelList)@\Used to support the
CASE statement.  This is usually written by hand and no pattern is used.
It tests if Register is in range LowerBound @value[Lte] Register
@value[Lte] UpperBound; if so, it jumps to the appropriate label in
labellist, using (Register @value[MinusSign] LowerBound) as the index.  If
not in range, it Jumps to a label planted at the end of the label table.  In
some implementations, the label table has to be a jump table.
@hinge

!*JumpWithin (Label LowerBound UpperBound)@\This is also used to support
the CASE statement, in the situation where the overall label range is
large, and there are many sub-ranges.  This generates code to JUMP to Label
if LowerBound @value(LTE) (REG 1) @value(LTE) UpperBound.  A default version
uses !*JumpWLessP and !*JumpWLeq tests.  [Perhaps should be modified to use
ANY reg].
@end(group)
@end(Description)

 The following @CMACRO@XS perform simple computations on their arguments.
Binary operations take two arguments, (Dest Source), and leave the result
in DEST.


@begin(description)
!*MkItem (Arg1 Arg2)@\Computes Arg1 @Value(Eq) Item(Arg1,Arg2); construct an
Item into Arg1 from the tag in Arg1 and Information part in ARg2.  May have
to shift and mask both Arg1 and Arg2.  Equivalent to
!*WOR(!*Wshift(Arg1,24),!*Wand(Arg2,16#FFFFFF)) on the 68000 [This may
actually use a stored preshifted version of the tag].
[[[[[Check the ORDER!!!!  and use parameters rather than 24 and fffff]]]]]]

!*WPlus2 (Arg1 Arg2)@\Compute Arg1 @Value(Eq) Arg1 + Arg2.  Look for special
cases of 1, -1, 0, etc.  Note on the 68000 it checks for a small integer, i.e.
-8..8 since these are done with a @dq[QUICK] instruction.  [Ignore overflow?]

!*WDifference (Arg1 Arg2)@\Compute Arg1 @Value(Eq) Arg1-Arg2.  Look for special
cases of 1, -1, 0, etc.

!*WTimes2 (Arg1 Arg2)@\Compute Arg1 @Value(Eq) Arg1*Arg2.  It first looks to
see if Arg2 is constant and a power of 2.  If so, it emits a corresponding
!*Ashift(Arg1,PowerOfTwo Arg2).  This check for special cases is in the
pattern.

!*AShift (Arg1 Arg2)@\Shift Arg1 by Arg2, using Arithmetic shift.  Used to
support !*WTIMES2.  Should do appropriate Sign Extend.

!*WShift (Arg1 Arg2)@\Shift Arg1 by Arg2, logically, doing 0 fill.

!*WAnd (Arg1 Arg2)@\Arg1 @Value(Eq) Arg1 AND Arg2.  BitWise AND, each bit of
Arg1 is 1 only if BOTH corresponding bits of Arg1 and Arg2 are 1.

!*WOr (Arg1 Arg2)@\Arg1 @Value(Eq) Arg1 OR Arg2.  BitWise OR.

!*WXOr (Arg1 Arg2)@\Arg1 @Value(Eq) Arg1 Xor Arg2.

!*WMinus (Arg1 Arg2)@\Arg1 @Value(Eq) @Value(MinusSign) Arg2.

!*WNot (Arg1 Arg2)@\Arg1 @Value(Eq) Logical NOT Arg2.

!*Loc (Arg1 Arg2)@\Arg1 @Value(Eq) Address (Arg2).

@end(description)

The following are important optimizations, that may be initially
implemented as procedures:
@begin(description)
!*Field (Arg1 Arg2 Arg3 Arg4)@\Arg1 @Value(Eq) Extract Field of Arg2
starting at Bit Arg3, of Length Arg4.  Bits are numbered
0...Size(Word)@Value(MinusSign)1.  The most significant bit is numbered 0 in
our model.  There is an assumption that Arg3 Arg4 are constants.

!*SignedField (Arg1 Arg2 Arg3 Arg4)@\Arg1 @Value(Eq) Extract Field of Arg2
starting at Bit Arg3, or Length Arg4.  Bits are numbered
0...Size(Word)@Value(MinusSign)1.  The field is to be sign extended into
Arg1.

!*PutField (Arg1 Arg2 Arg3 Arg4)@\Deposit into Arg1 a field of Arg2
starting at Bit Arg3, or Length Arg4.  Bits are numbered
0...Size(Word)@Value(MinusSign)1.  @end(Description)




@section(Organization of the Compiler and Assembler Source Files)


The code is organized as a set of common files kept on the PC:
directory, augmented by machine-specific files kept on other
directories@Foot[These generally have logical names of the form
PxxxC: where xxx is the root name of the directories for a given machine/OS
implementation.].  The @dq[skeletal] common files and machine-specific
files (mostly kept as compiled FASL files) make up the CROSS compiler
and assembler.  The machine-specific files customize the compiler for
the specific target machine and assembler (currently we compile for
@DEC20, @VAX750, @Apollo, @WICAT, and Cray-1).

@subsection(Common Files)

The  machine-independent part of compiler is kept as
PL:COMPILER.B@Foot[PL: is <PSL.LAP> or ~psl/lap.],
built by PC:COMPILER.CTL.  It consists of the files:

@begin(description)
PC:COMPILER.RED@\The basic compiler

PC:COMP-DECLS.RED@\Common declarations configuring the compiler:
installing the compiler specific functions, such as PA1FNs, COMPFNs,
OPENFNS etc.  These are described in the compiler chapter.

PC:PASS-1-LAP.SL@\Basic PASS1 of @CMACRO/LAP process.

PC:ANYREG-CMACRO.SL@\The @CMACRO and @anyreg pattern matcher and support
functions.

PC:COMMON-CMACROS.SL@\Standard or default @CMACRO@xs and @anyreg@xs used by
most implementations.

PC:COMMON-PREDICATES.SL@\Useful predicates to aid in writing the @CMACRO@xs.
@end(Description)

In addition, the following file is needed:

@Begin(Description)
PC:LAP-TO-ASM.RED@\Standard functions to convert LAP into machine-dependent
assembly code.
@end(Description)

@subsection(Machine-Specific Files)
For machine xxxx, the files:

@begin(description)
xxxx-COMP.RED@\Machine-Specific Compiler Patterns and Function installations.
This file may have some special @CMACRO support in it@Foot{This is the case
of extending the abstract machine for a particular implementation.}.

xxxx-CMAC.SL@\Machine-Specific @CMACRO@xs and @anyreg@xs.

xxxx-ASM.RED@\Definition of FORMATS, and special addressing mode conversion
functions, declaration Pseudos, etc.

xxxx-DATA-MACHINE.RED@\Smacros and constants to define @syslisp macros
needed for the implementation.  This file associates @syslisp functions with
@CMACRO@xs for special cases.
@end(description)
Finally, during the compilation of XXXX- user files, the following two files:

@begin(description)
xxxx:GLOBAL-DATA.Red@\Describes GLOBAL symbols used everywhere.
@end(description)

@subsection(Building the CROSS Compiler)
[For the moment, see the distribution guide for the Host machine].


@section(Design of LAP Format)

The argument to the function LAP is a list of lists and atoms.  The
lists are instructions, pseudo-ops and @cmacro@xs, and the atoms are labels
which are used to refer to positions in the code.  Note these need not
be IDs, but can also be strings, saving on ID space.  Instructions
should be of the form @w[(@i(opcode) . @i(operands))], where @i(opcode) is a
mnemonic for an opcode, and @i(operands) is a list of operands.  Each
operand should be either an integer, which represents an immediate integer
operand, a label, or a list of the form @w[(@i(mode) . @i(suboperands))].  A
@i(mode) is an addressing mode, such as INDEXED or INDIRECT on the PDP-10,
and DISPLACEMENT, DEFERRED, AUTOINCREMENT, etc@. for the VAX-11.  REG must
exist on all machines; others will be chosen as appropriate for the system.
Remember that these are mainly used for @cmacro expansions rather than
for writing code, so choose names for mnemonic value rather than brevity.
@i(Suboperands) may also be operands, or they may be specific to the mode,
e.g@. register names.@comment(more on @xlisp specific ones, QUOTE and FLUID)

See also the READING/WRITING/TESTING of LAP operating note@cite[Griss82h].
@comment[We have a LOT to write here!]

@subsection(Addressing Modes)
@subsection(Register Designators)
@subsection(Labels)
@subsection(Storage Pseudos)


@section(Implement LAP-TO-ASM)
@SubSection(Needed Values)
        Values must be given for:

@begin(description)
MainEntryPointName!*@\An ID which is the main procedure name.

NumericRegisterNames!*@\A vector of the symbolic names for the compiler
registers.

@end(description)
        In addition, each of the registers (as IDs) must be declared, using
DefList to provide the string name of the register and flagging the
property list of the ID with 'RegisterName.

@subsection(Tables)
        The list ForeignExternList!* is used to remember each of the
foreign functions that has been called in the course of a module so that
the proper externs can be emitted.

@SubSection(Printing routines)
         A number of routines which are used to print the
strings, constants, etc@. are listed as follows:

@begin(format)
PrintString(S)
PrintByte!,(X)
TruncateString(S,n)
PrintByteList(L)
PrintByte(X)
PrintHalfWordList(L)
PrintHalfWord(X)
PrintHalfWords(X)
PrintOpcode(X)
SpecialActionForMainEntryPoint()
PrintNumericOperand(X)
@end(format)

@subsection(Symbol Mapping)
        The function ASMSymbolP(X) must be written to check whether a @Xlisp
ID is also a legal symbol for the target assembler.

@Subsection(Formats)
        The following formats must be declared to tell the LAP-TO-ASM
routines how to print objects and the format of file names to use:
CodeFileNameFormat!*, DataFileNameFormat!*, LabelFormat!*, CommentFormat!*,
ExportedDeclarationFormat!*, ExternalDeclarationFormat!*, FullWordFormat!*,
HalfWordFormat!*, ReserveDataBlockFormat!*, ReserveZeroBlockFormat!*,
DefinedFunctionCellFormat!*, UndefinedFunctionCellInstructions!*, and the
description for how to construct an item (for MkItem).


@section(Independent Compilation)

 In order to maintain the PSL kernel as a set of reasonable sized
modules (about 15) a method to permit (semi-)independent translation
from LISP (or RLISP) to TLM assembly format was devised. This method
records information about symbols and structures defined in one module
and needed in another in a file called the SYM file.

When a set of modules is to be assembled into a program, a fresh SYM
file is allocated (usually called XXX-PSL.SYM or "Program-name.SYM").
Then as each module, MMM.RED is translated, the SYM file is first read
in to initialize various SYMBOL counters. After the translation is
complete an updated SYM file is written for the next step. When all
modules are tranlated, a last (MAIN) module is translated, and some of
the data information gathered in the SYM file is converted into global
data declarations in the assembly file.

Each module, MMM.RED (perhaps described by a MMM.BUILD file), is
converted
into 3 files, and updates to the SYM file:
@begin(description)
Code-File@\Contains the actual instructions for the procedues in the
MMM file. May also contain "read-only" data, such as some strings or
s-expressions. Typically called something like MMM.asm

Data-file@\Contains data-objects that may get changed, typically
WVAR and WARRAYs. This file typically called DMMM.asm or MMMd.asm.

Init-file@\Contains S-expressions that were not compilable procedures
found in the MMM.red file. Typically FLUID declarations, SETQ's and
PUT's dominate this sort of code. This file will be read-in by the
executing PSL after basic INITCODE is executed. Typically called
MMM.INIT.
@end(description)

The .SYM file data structures are updated. These structures are:
@begin(description)
Startup-Sexpressions@\Certain s-expressions must be evaluated
during INITCODE, before the .INIT files can be read. These are
collected into a single procedure, and compiled as INITCODE in the
MAIN module.  This is the (SAVEFORCOMPILATION (QUOTE ...))
expression in the SYM file.

ID list@\New IDs encountered in this file are added to a list
of IDs in ID# order. IDs are referred to by ID#; list is called 
ORDEREDIDLIST!*.

NEXTIDNUMBER!*@\The next ID# that will be allocated to the next new
ID.

STRINGGENSYM!*@\A string representing the last generated symbol-name.
Used for internal labels, and external names that are too complex.

Individual ID descriptors@\Each ID is now "installed" with a set of
PUT's, indicating its ID#, the assembly symbol that is its entry
point, if it is a WCONST, WVAR ,WARRAY etc. for example:
@begin(Verbatim)
(PUT 'INFBITLENGTH 'SCOPE 'EXTERNAL) % An exported WCONST 
(PUT 'INFBITLENGTH 'ASMSYMBOL 'NIL)  % no symbol allocated
(PUT 'INFBITLENGTH 'WCONST '18)      % Its compile time value

(PUT 'STACKUPPERBOUND 'SCOPE 'EXTERNAL) % An exported WVAR
(PUT 'STACKUPPERBOUND 'ASMSYMBOL '"L2041") % The Assembly SYMBOL
(PUT 'STACKUPPERBOUND 'WVAR 'STACKUPPERBOUND) % Type of VAR

(PUT 'TWOARGDISPATCH 'ENTRYPOINT '"L1319") % An internal FUNCTION
                                           % and its Assembly SYMBOL

(PUT 'RELOAD 'ENTRYPOINT 'RELOAD) % A simple entry point, not renamed
(PUT 'RELOAD 'IDNUMBER '552)      % Its ID number.
			          % SYMFNC(552)-> JUMP RELOAD

(PUT 'CADR 'ENTRYPOINT 'CADR)  % Another simple entry point
(PUT 'CADR 'IDNUMBER '229)


(PUT 'LIST2STRING 'ENTRYPOINT '"L0059") % Entry point, renamed because
					% too long
			                % SYMFNC(147)->JUMP L0059
(PUT 'LIST2STRING 'IDNUMBER '147)

(PUT 'SPECIALRDSACTION!* 'IDNUMBER '598) % A Global variable,
					 % INITIALLY NIL
(FLAG '(SPECIALRDSACTION!*) 'NILINITIALVALUE)

(PUT 'GLOBALLOOKUP 'ENTRYPOINT '"L3389")
(PUT 'GLOBALLOOKUP 'IDNUMBER '772)

(PUT 'CLEARCOMPRESSCHANNEL 'ENTRYPOINT '"L2793")
(PUT 'CLEARCOMPRESSCHANNEL 'IDNUMBER '678)

@end(Verbatim)
@end(description)

The contents of SYMFNC are filled in during the translation of the
MAIN module, and JUMPs to the entrypoints of symbols that have them
are filled in. Other symbols get a JUMP to the UndefinedFunction Entry
point.

In general, individual modules can be retranslated, since the
information they generate is initially taken from the SYM file
(ensuring that ID's and SYMBOLS get the same IDNUMBER and ENTRYPOINT
as before). The procedure is to translate the desired model (modules)
again, replacing the CODE-FILE, DATE-FILE and INIT-FILE previously
produced, and also to retranslate the MAIN module, since additonal
symbols S-expressions etc may have been produced, and therefor need to
be converted into INIOTCODE or HEAP or SYMBOL data.


@subsection(Data Pseudos)
The following are pseudo operations (from the @68000 version) which
must have a procedure to implement them in xxxx-ASM.RED:
HalfWord, Deferred, Displacement, Indexed, Immediate, Iconst,
AutoIncrement, AutoDecrement, Absolute, and ForeignEntry.



@section(Configure the Compiler)
This is still somewhat arcane. Basically, the compiler tables that select the
COMPFN's and OPENFN's and patterns need to be installed. The most
common method of doing this is to start from the xxxx-COMP.RED file most
like the target machine X@Foot[It is still the case that you need a
compiler wizard to help you with this as the details are still changing and
often undocumented, with a lot of "You have to do this, to do that, but ..."].

[Effort is required to describe this more clearly]


@Section(Write the Additional LAP Modules)
A variety of small LAP routines are required for I/O, system interface,
core-saving, efficient function-linkage, variable binding, etc. Some of these
are described in the following System Dependent Section. Others are:

@subsection(Apply-LAP)
These procedures are rather important, and unfortunately tricky to write.
They are used to enable compiled-code to call interpreted code and
vice versa. When they are used, the registers R1...Rn have the arguments
loaded in them, so SYSLISP can't be used.

The routines are CodeApply(codePtr,Arglst), CodeEvalApply(CodePtr,Arglst),
BindEval(Formals,Args), CompileCallingInterpreted(IdOfFunction), FastApply(),
and UndefinedFunction(). These are partially described in SYSLISP, and
written in LAP with mostly @CMACRO@XS@Foot[See P20:APPLY-LAP.RED and
PV:APPLY-LAP.RED.].

Need to discuss tricks in more detail, devise a set of tests.

@subsection(Fast-Bind)
This consists of efficient routines written in LAP (using mostly
@CMACRO@xs) to BIND and UNBIND fluid variables. The specifics depend
on how the !*LAMBIND, !*PROGBIND and !*FREERESTR @CMACRO@xs are
implemented.  In general, a machine specific "fast-call" is used, rather
than the more general recursive LISP call, and a list of ID numbers and
values ( NIL or register numbers) are passed in a block. The FASTBIND
routine uses the ID number to find the current value of the ID, and saves
the ID number and this value on the binding stack. Then NIL (for PROGBIND),
or the register value (for LAMBIND) is installed in SYMVAL(ID#). Note that
the compiler registers R1...Rn should not be changed, so either they have
to be saved, or other "hidden" registers have to be used. Since some hidden
registers may be used in the implementation of certain @CMACRO@xs, care has
to be exercized.

FASTUNBIND is usually simpler, since all it needs is a number of
@W[(ID# . Old-value)] pairs to pop off the Binding stack, and restore
@Foot[See P20:FAST-BINDER.RED or PV:FAST-BINDER.RED for some ideas.].


@SECTION(System Dependent Primitives)
The following set of functions are needed to complete the
system-dependent part of @PSL:

@subsection(System-dependent input and output)

@PSL uses a one-character-at-a-time stream model for I/O.  I/O channels are
just small integers in a range from 0 to 32 (32 was chosen for no
particular reason and could easily be increased if desired).  They are used
as indices to the WArrays ReadFunction, WriteFunction and CloseFunction,
which contain the names (as @xlisp items) of the functions to be called.
Thus a stream is an object with a set of operations, buffer(s), and static
vaiables associated with it. The current implementation of streams uses
parallel vectors for each of the operations that can be associated with a
stream. The Channel Number is used as an index into these vectors.
For example, the standard input channel is 0@Foot[This corresponds to the
@UNIX STDIO channel "stdin".] thus ReadFunction[0] contains
'TerminalInputHandler, which is a function used to get a character from the
terminal.  The system-dependent file input and output functions are
responsible for associating these channels with @ei[file pointers] or
@ei[JFNs] or whatever is appropriate to your system.  These functions must
also perform any buffering required.  We have been lucky so far because the
@UNIX and Tops-20 systems have single character primitives@Foot[Thus the
operating system hides the buffering.].

The reading function is responsible for echoing characters if the flag
!*ECHO is T.  It may not be appropriate for a read function to echo
characters.  For example, the "disk" reading function does echoing, while
the reader used to implement the @b[Compress] function does not.  The read
function should return the ASCII code for a line feed (EOL) character to
indicate an end of line (or "newline").  This may require that the ASCII
code for carriage return be ignored when read, not returned.


The VAX UNIX version of SYSTEM-IO.RED (stored on PV:@Foot[PV: is
<PSL.VAX-Interp> or ~benson/psl/vax-interp.]) is the simplest,
since the UNIX STDIO library is so close to this model.  This is a good
starting point for a new version.  It also uses the file PSLIO.C, which
contains the array @w[@Value(UnderScore)FILEPOINTEROFCHANNEL], used for
channel allocation.

The function @b(ClearIO) is called at system-startup time and when the
function RESET is called.  It should do all dynamic initialization of the
system, but should not close any open files.  Static initialization of
slots in the function arrays is done in the system-dependent file
IO-DATA.RED, and the array used for channel allocation should also have
initialized slots for the channels used for terminal input (STDIN!* = 0),
terminal output (STDOUT!* = 1) and channels 2 thru 4, used by BLDMSG,
COMPRESS/EXPLODE and FLATSIZE.  The variable ERROUT!* should have a
terminal output channel associated with it.  This may be shared with
STDOUT!* as in the @Dec20, or be associated with a separate error
diagnostic stream, as on the VAX.

Channel allocation is handled by the system-dependent part of I/O, so when
the @Xlisp function Open calls the function @b(SystemOpenFileSpecial) for a
non-file-oriented I/O stream, it should just mark a free channel as being
in use and return it.  @b(SystemMarkAsClosedChannel) does the opposite,
returning a channel to the pool of available ones.

@b(SystemOpenFileForInput) and @b(SystemOpenFileForOutput) each takes a
string as an argument and should return a channel and set appropriate
functions in the corresponding slots in ReadFunction, WriteFunction and
CloseFunction.  If a file cannot be opened, a continuable error should be
generated whose error form is (OPEN @dq[file name] 'TYPE), where TYPE is either
INPUT or OUTPUT.

Terminal output should be unbuffered if possible.  If it must be buffered,
it should be flushed when terminal input is done and when EOLs are written.
Terminal input should be line buffered, using line editing facilities
provided by the operating system if possible.  The terminal input routine
is responsible for the display of the variable PromptString!*, using a @PSL
channel for output if desired, as the VAX version does.  The @Dec20
terminal input routine uses a line editing facility that redisplays the
prompt and previously typed characters when a Control-R is typed.

End of file on input is indicated by returning a character which is CHAR
EOF, Control-Z (ASCII 26) on the @Dec20 and Control-D (ASCII 4) on UNIX.
This can be changed to any control character.  The file SCAN-TABLE.RED will
contain the CharConst definition for EOF, and a copy of LispScanTable!*
with an 11 (delimiter) in that position.


@subsection(Terminate Execution)
The function QUIT(); terminates execution.  It should probably close open
files, perhaps restore system state to "standard" if special I/O
capabilities were enabled.  On some systems, execution can continue after
the QUIT() at the next instruction, using a system command such as
START or CONTINUE; on others, the core-image cannot be
continued or restarted (see DUMPLISP(), below).  On the DEC-20, the HALTF
jsys is used, and execution can be continued.  On the VAX under UNIX, a Stop
signal (18) is sent via the "kill(0,18)" call.  This also can be continued
under Berkeley 4.1 UNIX.

See the file SYSTEM-EXTRAS.RED on PV: and P20:

@subsection(Date and Time)
The function TIMC(); is supposed to return the run-time in milliseconds.
This time should be from the start of this core-image, rather than JOB or
SYSTEM time.  It is used to time execution of functions.  Return it as a
full-word, untagged integer in register 1.  On the DEC-20, we use the RUNTM
jsys, on the VAX the C call on "times" is used, and multipled by 17,
to get 1/1020'ths of a second.  While not yet required, a TIMR() to get REAL,
or WALL, time may be useful@Foot[See TIMC.RED on P20: and PV:.].

The DATE(); function is supposed to return a Tagged @XLISP string
containing the current date.  No particular format is currently assumed,
and the string is used to create welcome messages, etc.  Later developments
may require a standard for TIMESTAMPS on files, and may also require a
CLOCK-time function.  The Allocator function GtSTR(nbytes) may be useful to
get a fresh string to copy the string returned by a system call into.  The
string should be 0-terminated.  The DEC-20 uses ODTIM, and "writes" to the
string in "6-jun-82" format.  On the VAX, the "ctime" call is used, and the
result "shuffled" into the same format as the DEC-20@Foot[See
SYSTEM-EXTRAS.RED on PV: and P20:].

@subsection(ReturnAddressP)
The function RETURNADDRESSP(x); supports the backtrace mechanism, and is
supposed to check that the instruction before the supposed address X, is in
fact a legal CALL instruction.  It is used to scan the stack, looking for
return addresses@Foot[Very TRICKY, see SYSTEM-EXTRAS.RED on PV: and P20:].


@subsection(Interrupt Handler)
Also very crude at present; on the DEC-20, written as a loadable module,
P20:20-INTERRUPT.RED, using the JSYS package.  This enables CTRL-G, CTRL-T,
some stack and arithmetic overflows, binding them to some sort of Throw
or Error routine.

 On the VAX, the file PV:TRAP.RED defines some signal setup, and
InitializeInterrupts routine, and is included in the kernel.
It associates each trap with a STDERROR call with a given message.

Not yet standardized. 

We really should "bind" all trappable interupts to an
appropriate THROW('!$SIGNAL!$,n), and indicate whether
to treat as a Fatal Error, a Continuable Error, or not an
Error at all.

@subsection(Core Image Saving)
A way in which @PSL (and most @XLISP@xs) get used involves the ability to
load @XLISP and FASL code into an executing @PSL, saving this
augmented "core-image" in a named file for subsequent restart later.  Some
Operating Systems permit a running program to be saved into an executable
file, and then restarted from the beginning; others permit the saved
program to be continued at the instruction following the call to the SAVE
routine.  Some operating systems do not normally permit or encourage the
saving of a running program into an executable file, and there is a lot of
work to be done.

The model currently used in @PSL is that a call on DUMPLISP(); does the
following (this is based on VAX and DEC-20 experience, and could
change as Apollo and CRAY are completed):


@begin(enumerate)
calls RECLAIM(); to compact the heap, or move the upper heap into
the lower heap. @Comment{How is it told that this is a cleanup reclaim that
is to put the results in the "lower" heap???}

makes some system calls to free unused space, decreasing the executable
image; space is returned from HEAP, BPS and STACK.

the core-image is saved in  a file, whose name is the string in the
global variable, DumpFileName!* (this string may have to be passed
to the system routine, similar to I/O, using a small peice of LAP
as interface, or using the Foreign function protocol);

execution continues without leaving the running program; to terminate,
the QUIT(); function must be called explicitly [this may not be possible
on some systems, and may require a change in the model, or a
machine specific restriction].

the saved executable file will restart "from-the-top", i.e. by calling the
machine specific "startup" function defined in MAIN-START.RED, which calls
initialization functions CLEARBINDINGS(), CLEARIO(),
INITIALIZEINTERRUPTS(), etc.  Then the Startup function calls MAIN();,
which can be redefined by the user before calling DUMPLISP();.  MAIN()
typically calls StandardLISP() or RLISP(), or some other TopLoop.  This
startup function also has a @XLISP accesible name, RESET.
@end(Enumerate)

On some machines, the core-image will automatically start "from-the-top",
unless effort is expended to change the "restart-vector" (e.g@. the TOPS-20
SSAVE jsys on the DEC-20);
on others, an explicit LINKE CALL (a JUMP) to RESET should be included
after the core-save call, to ensure execution of RESET (e.g@. the CTSS
DROPFILE call on the CRAY-1). 

On the VAX under UNIX, a new function UNEXEC
was written in C, to convert an executing program back into "a.out" format.

See the files MAIN-START.RED and DUMPLISP.RED on P20: and PV:, and the
preliminary documentation on the @apollo MAP_CODE.TXT, on PD:.


@section(How LAP/TLM assembler works)

@Section(How the LAP works)
This discription of how the resident assembler (LAP) works is taken
from the 68000 implementations.  Refer to the diagram below to aid the 
understanding of this description.  ALM instructions are passed into the
procedure called LAP. The first thing LAP does is to pass them through the
procedure PASS1LAP to transform ALM into TLM. The TLM is handed to
OptimizeBranches to check to see if long branches are needed.
OptimizeBranches is responsible for computing the offset of each label from
the beginning of the function. A list called BranchAndLabelAlist is created
which stores the labels and their offsets from the start of the code for
this function.

Upon the exit from OptimizeBranches the user may turn on the flag "PGWD"
and will be able to see the current state of the code. If the code is to 
be compiled into memory and not fasled to a file then BPS space is
allocated. 

Now the code make take one of three parallel paths.
If the code is a label then it is ignored.
If the instruction is an instance of !*Entry then the instruction
is passed to the procedure SaveEntry to establish the address of the 
entry point of the code. 
On all other cases the instruction is passed to the procedure
deposit instruction. This is often a good procedure to trace when 
debugging lap so that one can see what is actually heading off to be
depsoited. 

Once the code has passed through one of the above three paths,
the function defineEntries is called which loads the new code pointer into
the function cell in the SYMFNC table. Following this the code pointer is 
tagged as code and returned as the result value of the function LAP.

The following details are provideed as a guide to writing your own
assembler.
Consderation should be give to
@begin(enumerate)
Regular vs Irregular Machines

Templates to Assemble Portions of Instruction

Variable Length Instructions

Alignment Problems

Data Psuedos

@xlisp Specific Pseudos
@end(enumerate)

@section(How do opcodes get defined for the LAP assembly process)

There are three procedures used to define the opcodes.

The first is DefineOpcode which defines, sets the necessary properties on
the opcode's property list, for 680000 opcodes that have no ,byte,word, or
long variants.

The second function is DefineOpcodes (notice it is simply the plural of the
first function) which defines an opcode with variants for byte,word, and
long mode.  

And third is the function DefineCCOpcodes which sets up the properties for
all the condition codes.

@Section(Description of DefineOpcode)
The function DefineOpcode an have three, four, or five arguments.
They are defined to be:
@begin(enumerate)
The opcode name or id.

The base 2 value of the opcode, only the constant bits in the opcodes
binary value are given initially, the varible fields of an opcode are 
ORed into the word later.  These are all two bytes long. This is tagged
on a functions property list as its OpcodeValue.

The function to be used to assemble this opcode, referred to on the
property list by a functions InstructionDepositFunction.

The forth field if present represents the mode to be used with this
instruction: either byte, word, or long mode. The default is always word
mode.  This value is stored on the property list under the tag of Size.

The fifth field is the number of bytes that the instruction will take up
in the resulting binary code. Generally, only instructions that take no
arguments will have this field filled in.  This value is stored on the
property list under the tag of InstructionLength.

@end(enumerate)
DefOpcode finally calls the function EvDefopcode which puts all the
properties on the property list.

@Section(How the Function DefOpcodes works)
This function works just like the previous function DefOpcode except that
it takes one less field, the size field which tells how the opcode will be
used: byte, word, or long. This procedure will define an opcode for each
case.
For example if an opcode name is move then an id with associated property
list will be created for move.b, move.w, and move.l.

@Section(How the procedure  DefCCOpcodes Works)
This function was written just to save typing in all the cases of opcodes
that use the condition codes. It does that same thing as DefOpcode above
but for each condition code variant of an opcode.

@section(Ok so what happens in a functions instruction depositfunction??)
The opcode and oprands are selected out of the list and if the operands are
not normal then they are passed throught the function effective address
which classifies then as to the 68000 convention of register and mode.

 Purpose: convert an operand from symbolic to numeric form.
 Returns: Addressing mode in the range 0..7
 --------------------------------------------------
 M68K addressing modes (from appendix B of the M68K User's Manual)
 Addressing Mode         Mode  Reg        Valid Modes*         Assembler
                                       Data MEM Cont Alter      Syntax
 Data Register Direct    000   reg no.   X   -   -    X           Dn
 Address Register Direct 001   reg no.   -   -   -    X           An
 Addr Reg Indirect       010   reg no.   X   X   X    X          (An)
  with PostIncrement     011   reg no.   X   X   -    X          (An)+
  with PreDecrement      100   reg no.   X   X   -    X         -(An)
  with Displacement      101   reg no.   X   X   X    X         d(An)
  with Index             110   reg no.   X   X   X    X         d(An,Ri)
 Absolute Short          111   000       X   X   X    X          xxxx
 Absolute Long           111   001       X   X   X    X        xxxxxxxx
 PC with Displacement    111   010       X   X   X    -         d(PC)
 PC with Index           111   011       X   X   X    -         d(PC,Ri)
 Immediate               111   100       X   X   -    -        #xxxxxxxx

 * = Valid Addressing modes for each type of Addressing Category
 Data              - used to refer to data operands
 Mem   = Memory    - used to refer to memory operands
 Cont  = Control   - used to refer to memory operands without an associated
                     size
 Alter = Alterable - used to refer to alterable (writeable) operands
 --------------------------------------------------
 Operand is of the form:

 case 1:  numeric                 immediate data
       or (immediate x)
 case 2: non-numeric atom         a local label, which uses PC with
                                  displacement
 case 3: (reg x)                  x is a number or symbolic register name
 case 4: (deferred (reg x))       address register indirect in Motorola jargon
 case 5: (autoincrement (reg x))  address register indirect with postincrement
 case 6: (autodecrement (reg x))  address register indirect with predecrement
 case 7: (displacement (reg x) n) if (reg x) is an A reg
                                    then if n is 0
                                           then (deferred (reg x))
                                           else address register indirect
                                                 with displacement
                                     else if (reg x) is a D reg
                                            then address register indirect
                                                   with index, using A6 (zero)
 case 8: (indexed (reg x) (displacement (reg y) n))
                       address register indirect with index

 case 9+: various Lisp addressing modes, all of which are absolute long
                                         addresses

 The value returned by this function is the mode field of the instruction
 for the operand.
 In addition, the fluid variables OperandRegisterNumber!*
                              and OperandExtension!*
 will be set.
 If there are no words to follow, OperandExtension!* will be set to NIL.
 Otherwise, possible values of    OperandExtension!* are:

       number or (immediate exp)  immediate data
       (number)                   16-bit signed displacement
       non-numeric atom           pc relative label
       (displacement reg disp)    index extension word
       other                      absolute long, i.e. LISP addressing mode


LAP is a complete assembly form and can
be used by @xlisp programmers to write any legal assembly
code@Foot{There is no real guarantee that the entire set of machine
opcodes is supported by the LAP.  An implementor may have chosen to
implement only those constructs used by the compiler-produced code or
explicitly used in hand written LAP.  The reason for this partial
implementation is that many modern processors have included operations
to facilitate @ei[high level language compilation], which often seem
to be less than useful.}

@section(Binary FAST Loader,FASL)
[Explain FASL in general]

[Explain essential problem, relocation of machine addresses and LISP
ids]

[Give big-picture of FASL]

[Find MAGUIREs pictures of FASL blocks or regenerate
]
This section is a guide to the internal workings of faslout and then
faslin.

The user begins the faslout procedure by calling the procedure faslout with
a string that does not have the extension (because it will add the
appropriate binary extension for you).  However, when fasling in, the file
name requires the binary extension [Change this inconsistency].  

Inside the procedure faslout, the file name is assigned to the fluid
variable ModuleName!*.  Depending upon the setting of the flag
!*Quiet_Faslout, the system will either print out a greeting message or
not.  Next, an output binary file is opened using the argument file name.
It will return the channel number to a fluid variable CodeOut!*.
CodeFileHeader is called to put in a header in the output file.  

CodeFileHeader writes out a word consisting of the Fasl Magic Number
(currently set to 99).  This magic word is used to check consistency
between old and current fasl format files (an error is given upon fasling
in the file if there is not a 99 as the first word).  Therefore, the system
must consistently modify that number when a new fasl format is produced.
To continue, we need to understand the allocation that takes place within
the Binary Program Space (BPS).  The BPS is a large, non-collected space
that contains compiled code, warrays, the string assocaited with interned
ID's, constant data in fasl files, etc.  Space is allocated from both
ends of the space.  Compiled code is allocated from the bottom (using
NextBPS as a pointer) and warrays are allocated from the top (using LastBPS
as the pointer).  When an allocation is attempted, the desired size is
checked to see if it will cause LastBPS and NextBPS to cross; if it will,
an error message will be printed.  The next step is to allocate 2/3 or the
remaining BPS from the top.
@begin(verbatim,leftmargin 0)

         .------------------------------------.
         |                                    |
         |     WArrays                        |
         |                                    |
         |                                    |
Last_BPS>|------------------------------------| <-FaslBlockEnd!* ---.
         |      Code                          |                     |  
         |                                    |                     |
         |                                    |                     |
         |                                    |                    2/3
         |====================================| <-CodeBase!*        |
         |      Bit Table                     |                     |
         |====================================| <-BitTableBase!* ---'
         |                                    |
         |                                    |
Next_BPS>|------------------------------------|
         |                                    |
         |                                    |
         |                                    |
         `------------------------------------'

               Binary Program Space

@end(verbatim)
The procedure AllocateFaslSpaces will setup the following fluid variables.
FaslBlockEnd!* will be the address to the top of the available space for
this particular allocation.

BitTableBase!* points to the beginning of the BitTable.

CurrentOffset!* keeps a pointer into the codespace of this allocation to
the next available point to add more code.

BitTableOffset!* is a running pointer to the current location in the
BitTable where the next entry will go. 

CodeBase!* is the base pointer to the beginning of the code segment for
this allocation.

MaxFaslOffset!* is the max size of the codespace allowed for this
implementation.

OrderedIDList!* keeps record of the ID's as they are added.

NextIDNumber!* is a base number used just in fasl files to indicate which
IDs are local and which are global. It is assumed that there will never be
more than 2048 pre-allocated ID's, currently there are 129. The first 128
preallocated IDs are ASCII codes(0-127) and the last one is NIL(128).

Everything is now setup to begin fasling PSL code out to the file.
The remainder of the faslout procedure sets up three more fluid variables.

!*DEFN is set to T which indicates that you are not going to do normal
evaluation from the top loop and from files such as using the functions IN
and DSKIN.

DFPRINT!* signals that DFPRINT!* is now used as the printing function.
The procedure used will be DFPRINTFasl!*.

!*WritingFaslFile is set to T to let the system know that fasling out is
goping on as opposed to compiling code directly into memory inside the PSL
system.


@subsection(Binary I/O and File Format)
@u[Current FASL file format:]

Check accuracy, this was PC:fasl-file.Specs

@begin(description)
Word@\Magic number (currently 99).@comment{ Why the magic number 99??? }

Word@\Number of local IDs.

Block@\Local ID names, in order, in regular @xlisp format 
(string size followed by block of chars).@comment{ need to specify that the
                                                  string size is given as a
                                                  word, and the character
                                                  counts is interms of bytes}

Word@\Size of code segment in words.

Word@\Offset in addressing units of initialization procedure.

Block@\Code segment.

Word@\Size of bit table in words      (redundant, could be eliminated).

Block@\Bit table.
@end(description)

@subsection(Relocation/Bit Table)
Describes how to adjust addresses and ID numbers in previous Code Segment.
[Should add GENSYM generator option.]  This is a block of 2 bit items, one
for each \addressing unit/ in the code block.@comment{ Are we committed to
two bits forever? }

@begin(description)
0@\Don't relocate at this offset.

1@\Relocate the word at this offset in the code segment.

2@\Relocate the (halfword on VAX, right half on 20) at this offset.
@comment[Can this be generalized some more????]

3@\Relocate the info field of the @xlisp item at this offset.
@end(description)

The data referred to by relocation entries in the bit table are split into
tag and info fields.  The tag field specifies the type of relocation to be
done:@comment{ Where is this data stored??? }

@begin(description)
0@\Add the code base to the info part.

1@\Replace the local ID number in the info part by its global ID number.

2@\Replace the local ID number in the info part by the location of its
value cell.

3@\Replace the local ID number in the info part by the location of its
function cell.
@end(description)

Local ID numbers begin at 2048@comment{why this magic number???}, to allow
for statically allocated ID numbers (those which will be the same at
compile time and load time).

@subsection(Internal Functions)
[IS there any special handling of these, or restrictions]

@subsection(Foreign Functions, Externs, etc)
[Explain why cant do in FASL now. Need to do run-time look up of
LOADER symbols, and use in LAP/FASL part of things. Will need to
add extra RELOC types to FASL].

@subsection(Init Code)
[Explain how executable -sexpressions that are not procedure
definitions
are gathered into a single LISP procedure, compiled, and given
name, sort of !*!*FASL-INIRTCODE!*!*, or some such.

Is called as last action of LOAD.

Explain current restriction on FASL initcode size, suggest soluitions]
@subsection(Annotated FASL file example)
@begin(verbatim)
*Annotated version of a dump*

procedure adder(x);
begin scalar y;
  y:=x;
  return y+1;
end;

Dump of "trythis.b"

000000:  0020 0001 E7DF FEDF  0000 0080 0000 00A0
000010:  1800 0000 0000 0000  0000 0000 0000 0000
000020:  0000 0080
         0000 0063 16#63 is the magic number which
                   indicates that is a FASL file
         0000 0003 Number of local IDs
         0000 0004 The first ID, in the form Length
                   of String, String name
000030:  4144 4445 ADDER
         5200 0000
         0000 0003 Second ID, 3 (+1) characters "ADD1"
         4144 4431 ADD1
000040:  0000 0000
         0000 0007 Third ID, 7 (+1) characters of 
                   "PUTENTRY"
         5055 5445 PUTENTRY
         4E54 5259
000050:  0000 0000
         0000 0003 Fourth ID, 3 (+1) characters "EXPR"
         4558 5052 EXPR
         0000 0000
000060:  0000 000A CodeSize = 10 words
         0000 000A Offset of INIT function
 -------------------- Code Block
         2649       		MOVEA.L	A1,A3
         2449			MOVEA.L	A1,A2
         4EF9 C000		JMP C000 0801
                                    ^ Relocate 
                                       Function cell
                                 (ID.1 call on "ADD1")
000070:  0801
---------- The init code
         267C 0000 0000		MOVEA.L #0,A3
         247A 0010		MOVEA.L 10(pc),A2
         227A 0008		MOVEA.L  8(pc),A1
000080:  4EF9 C000 0802		JMP C000 0802
                                    ^ Relocate
				        Function cell
                                   (ID.2 = "PUTENTRY")
         FE40 0800	           (ID.0 the procedure
           ^ Relocate ID number     name "ADDER")
         FE40 0803		   (ID.3 the procedure
           ^ Relocate ID number     type "EXPR")
         0000
 -------------------- Bit Table Section
000090:  0000 0003   Length of Bit table in words
 -------------------- Bit Table 
 0004 0000   : 0000 0000 0000 0100 0000 0000 0000 0000
                               ^ = Relocate Word
 0000 040C   : 0000 0000 0000 0000 0000 0100 0000 1100
                           Relocate Word ^         ^
		           Relocate Inf------------'
 0C00 0000   : 0000 1100 0000 0000 0000 0000 0000 0000
 		     ^ Relocate Inf
@end(verbatim)

[Explain how to use a BDUMP routine to examine this]


@subsection(Binary I/O)

The following functions are needed for FASLIN and FASLOUT:

@i(BinaryOpenRead(Filename:string):system-channel)

This should take a filename and open it so that binary input can be done.
The value returned is used only by the other functions in this group, and
so can be whatever is appropriate on your system.

@i(BinaryOpenWrite(Filename:string):system-channel)

Similar to BinaryOpenRead, open a file for binary output.

@i(BinaryClose(SChn:system-channel):none returned)

SChn is the value returned by BinaryOpenRead or BinaryOpenWrite.  The file
is closed.

@i(BinaryRead(SChn:system-channel):word)

One word (i.e. Lisp item sized quantity) is read from the binary file.  On
the Dec-20 this is done using the @i(BIN) jsys with the file opened in
36-bit mode using a 36-bit byte pointer.  The VAX Unix implementation uses
@i(getw) from the stdio library.

@i(BinaryReadBlock(SChn:system-channel, A:word-address, S:integer):none
returned)

S words are read from the binary file and deposited starting at the word
address A.  The Dec-20 version uses the @i(SIN) jsys and VAX Unix uses the
@i(fread) function.

@i(BinaryWrite(SChn:system-channel, W:word):none returned)

One word is written to the binary file.  On the Dec-20 this is done using
the @i(BOUT) jsys with the file opened in 36-bit mode using a 36-bit byte
pointer.  The VAX Unix implementation uses @i(putw) from the stdio library.

@i(BinaryWriteBlock(SChn:system-channel, A:word-address, S:integer):none
returned)

S words starting at the word address A are written to the binary file.  The
Dec-20 version uses the @i(SOUT) jsys and VAX Unix uses the @i(fwrite)
function.

@i(BitTable(A:word-address, B:bit-table-offset):integer)

This is similar to @i(Byte) and @i(HalfWord), except that a 2-bit unit is
being extracted.  A is a word address, the base of a table of 2-bit
entries.  The one B entries from the beginning is returned.

@i(PutBitTable(A:word-address, B:bit-table-offset, I:integer):)

Analagous to @i(PutByte) and @i(PutHalfWord), except that a 2-bit unit is
being deposited.  A is a word address, the base of a table of 2-bit
entries.  The low-order 2 bits of the integer I are stored at offset B.

[Explain how to test Binary I/O, in test N]

@subsection(Miscellaneous)
To use EMODE/NMODE and PRLISP on some systems, a "raw" I/O mode may be
required.  See the PBIN, PBOUT, CHARSININPUTBUFFER, ECHOON and ECHOOFF
functions in EMOD2:RAWIO.RED and SYSTEM-EXTRAS.RED.

Some sort of system-call, fork or similar primitives are useful,
clearly system dependent.  See the JSYS and EXEC package on P20:, the
SYSTEM call in PV:SYSTEM-EXTRAS.RED (written in C as a Foreign
Function), or the SYSCALL on the APOLLO.

This set is not yet standardized.

