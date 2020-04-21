
@Make(article)
@device(LPT)
@style(Spacing 1)
@use(Bibliography "<griss.docs>mtlisp.bib")
@modify(enumerate,numbered=<@a. @,@i. >, spread 1)
@modify(itemize,spread 1)
@modify(description,leftmargin +2.0 inch,indent -2.0 inch)

@LibraryFile(PSLMacrosNames)
@comment{ The logos and other fancy macros }

@pageheading(Left  "Utah Symbolic Computation Group",
             Right "July 1982",
             Line "Operating Note No. 71"
            )
@set(page=1)
@newpage()
@Begin(TitlePage)
@begin(TitleBox)
@center[

@b(The PSL Bootstrap Test Files)


M. L. Griss, S. Lowder, E. Gibson, E. Benson,
R. R. Kessler, and G. Q. Maguire Jr.

Utah Symbolic Computation Group
Computer Science Department
University of Utah
Salt Lake City, Utah 84112
(801)-581-5017

@value(date)]
@end(TitleBox)
@begin(abstract)

This note describes how use a suite of tests designed to exhaustively
exercise all facets of the PSL bootstrap sequence. Each test is a step
towards boostrapping a complete mini-LISP and then complete PSL.
@end(abstract)
@begin(ResearchCredit)
Work supported in part by the National Science Foundation
under Grant No. MCS-8204247, and by Lawrence Livermore Laboratories under
Subcontract No. 7752601.
@end(ResearchCredit)
@end(TitlePage)
@pageheading(Left  "PSL Testing",
             Right "Page @Value(Page)"
            )
@set(Page=1)
@newpage()
@section(Introduction)
In order to accomplish the PSL bootstrap with a minimum of fuss, a carefully
graded set of tests is being developed, to help pinpoint each error as
rapidly as possible. This preliminary note describes the current status
of the test files. The first phase requires the coding of an initial
machine dependent I/O package and its testing using a familar system language.
Then the code-generator macros can be succesively tested, making calls on this
I/O package as needed. Following this is a series of graded SYSLISP files,
each relying on the correct working of a large set of SYSLISP constructs.
At the end of this sequence, a fairly complete "mini-LISP" is obtained.
At last the complete PSL interpreter is bootstrapped, and a variety of
PSL functional and timing tests are run.

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
init(C)@\Called once to set up I/O channels, open devices, print welcome
message,  initialize timer. Ignores the argument C.

Quit()@\Called to terminate execution; may close all open files. C is
ignored.

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

PutI(C)@\Print C as an integer, until a SYSLISP based Integer printer that
calls XXX-PutC works. This function is used to print integers in the
initial tests before the full I/O implementation is ready.

Err(C)@\Called in test code if an error occurs, and prints C as an
error number. It should then call Quit() .
@end(description)

As a simple test of these routines implement in "F" the following. Based on
the "MainEntryPointName!*" set in XXX-ASM.RED, and the decision as to
whether the Main toutine is in "F" or in "LISP", XXX-MAIN() is the main
routine or first subroutine called:
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

	CALL XXX-PUTI(3);  % Third test, type in "AB<cr>"
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

@section(LAP and CMACRO Tests)
After the basic XXX-ASM.RED file has been written and the XXX-CROSS.EXE has
been built, and seems to be working, an exhastive set of CMACRO tests
should be run. The emitted code should be carefully examined, and the
XXX-CMAC.SL adjusted as seems necessary.  Part of the CMACRO tests are to
ensure that !*MOVEs in and out of the registers, and the ForeignFunction
calling mechanism work.

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
declaration of XXX-yyyy.  Notice that "UndefinedFunction" is defined in
LAP, to call Err, as appropriate. This will trap some erroneous calls,
since a call to it is planted in all "unused" SYMFNC cells. Some effort to
make it pick up the ID number of the offending undefined function (by
carefully choosing the instructions to be planted in the function cell),
will be a great help. Once coded and tested by running MAIN1, it need not
be changed for the subsequent MAINn/SUBn combinations to work.

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
on Factorial 9 and Tak(18,12,6). Build by iteself, and run with IO.
@Comment{This seems to hide the assumption that 10! can be done in the
integer size of the test implementation.??? }

SUB2.RED@\Defines a simple print function, to print ID's, Integer's,
Strings and Dotted pairs in terms of repeated calls on PutC. Defines
TERPRI, PRIN1, PRIN2, PRINT, PRIN2T and a few other auxilliary print functions
used in other tests. Tries to print "nice" list notation.

MAIN2.RED@\Uses Prin2String to print a welcome message, solicit a sequence of
characters to be input, terminated by "#". Watch how end-of-line is handled.
Then Print is called, to check that TAG's are correctly recognized,
by printing a LISP integer, an ID and 2 dotted pairs. Requires SUB2 and IO modules.

SUB3.RED@\Defines a mini-allocator, with the functions CONS, XCONS and NCONS,
GTHEAP, GTSTR. Requires primitives in SUB2 module.

MAIN3.RED@\First Executes a Casetest, trying a variety of Branches and
Defaults in the case staement. There a number of calls on Ctest with an
integer from -1 to 12; Ctest tries to classify its argument using a case
statement. ConsTest simply calls the mini-allocator version of CONS to build
up a list and then prints it. Requires SUB2, SUB3 and IO modules.

SUB4.RED@\Defines a mini-reader, with RATOM and READ.   This mini-READ
does not read vectors, and does not know about the escape character, ! .
Requires SUB3, SUB2, and IO modules.

MAIN4.RED@\The test loop calls
RATOM, printing the internal representation of each token.
Type in a series of id's, integer's, string'ss etc. Watch that same ID goes
to same place. After typing a Q, goes into a READ-PRINT loop, until Q is
again input. Requires SUB3, SUB2 and IO modules.

SUB5.RED@\Defines a mini-EVAL. Does not permit user define functions.
Can eval ID's, numbers, and simple forms. No LAMBDA expressions.
FEXPR Functions known are: QUOTE, SETQ and LIST.
Can call any compiled EXPR, with upto 4 arguments. Rather inefficient, but
could be used for quick bootstrap.
Requires  SUB4, SUB3, SUB2 and I/O.

MAIN5.RED@\Tests the IDAPPLY constructs, and FUNBOUNDP. Then starts a
mini-READ-EVAL-PRINT loop. Requires SUB5, SUB4, SUB3, SUB2 and IO modules.
Note that input ID's are not case raised, so input should be in UPPERCASE
for builtin functions.  Terminates on Q input.

SUB6.RED@\Defines a more extensive set of primitives to support the
mini-EVAL, including LAMBDA expressions, and user defined EXPR and FEXPR
functions.  Can call any compiled EXPR, with up to 4 arguments. COND,
WHILE, etc. are defined.  Requires SUB5, SUB4, SUB3, SUB2 and I/O.

MAIN6.RED@\Tests the full PSL BINDING module (PI:BINDING.RED).
Also includes the standard PSL-TIMER.RED (describd below), which must be
driven by hand, since file I/O is not yet present.
Requires SUB6,SUB5, SUB4, SUB3, SUB2 and IO modules.
Note that input ID's are not case raised, so input should be in UPPERCASE
for builtin functions.  Terminates on Q input.

SUB7.RED@\A set of routines to define a minimal file-io package, loading
the machine independent files: PT:SYSTEM-IO.RED and PT:IO-DATA.RED, and a
machine dependent file XXX-SYSTEM-IO.RED. The latter file defines
primitives to OPEN and CLOSE files, and read and write RECORDS of some
size. The following definitions are used in the routines: 
@begin(verbatim)
FileDescriptor: A machine dependent word to
                references an open file.
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

MAIN7.RED@\Is an interface to the Mini-Eval in SUB5.RED and SUB6.RED
and defines an (IOTEST) function that should be called. Other functions to
try are (OPEN "foo" 'OUTPUT), (WRS n), (RDS n) etc. Note also that
XXX-HEADER will have to be changed at this point to have GETC and PUTC
use the IndependentReadChar and IndependentWriteChar.

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
@@MIC MAIN MAINn]

As appropriate, compile or assemble the output "F" language modules
(after shipping to the remote machine, removing tabs, etc..). Then
"link" the modules, with the XXX-IO support, and execute. On the
DEC-20, the 
@verbatim[
@@EX @@MAINn.CMD

command files are provided as a guide]

See the Appendix (file PT20:20-TEST.OUTPUT) for an example of the
output on the DEC-20.
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
@section(References)
@bibliography
@NewPage()
@appendix(Sample DEC-20 Output)
@begin(verbatim)
@include(PT20:20-TEST.OUTPUT)
@end(verbatim)
