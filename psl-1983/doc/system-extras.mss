@make(article)
@section(System Dependent Functions)
The following set of functions are needed to complete the system
dependent part of PSL:
@subsection(I/O)
OPEN, CLOSE, READ, WRITE, CLEARIO, ECHO control for EMODE

@subsection(Terminate Execution)
The function QUIT(); terminates execution. It should probably close open
files, perhaps restore system state to "standard" if special I/O
capabilities were enabled. On some systems, execution can continue after
the QUIT(), with the next instruction; on others, the core-image can not be
continued or restarted.  (See DUMPLISP(), below). On the DEC-20, the HALTF
jsys is used, and execution can be continued. On the VAX under UNIX, a Stop
signal (18) is sent via the "kill(0,18)" call. This also can be continued
under Berkely 4.1.

See the file SYSTEM-EXTRAS.RED on PV: and P20:

@subsection(Date and Time)
The function TIMC(); is supposed to return the run-time in milliseconds.
This time should be from the start of this core-image, rather than JOB or
SYSTEM time. It is used to time execution of functions.  Return it as a
full-word, untagged integer in register 1. On the DEC-20, we use the RUNTM
jsys, on the VAX the C call on "times" is used, and multipled by 17,
to get 1/1020'ths of a second. While not yet required, a TIMR() to get REAL
time may be useful.

See TIMC.RED on P20: and PV:.

The DATE(); function is supposed to return a Tagged LISP string continue the
current date. No particular format is currently assumed, and the string is
used to create welcome messages, etc. Later developments may require a standard
(for TIMESTAMPS on files), and may also require a CLOCK-time function.
The Allocator function GtSTR(nbytes) may be useful to get a fresh string
into which to copy the string returned by a system call. The string
should be 0 terminated. The DEC-20 uses ODTIM, and "writes" to the
string in "6-jun-82" format. On the VAX, the "ctime" call is used,
and the result "shuffled" into the same format as the DEC-20.

See SYSTEM-EXTRAS.RED on PV: and P20:

@subsection(ReturnAddressP)
The function RETURNADDRESSP(x); supports the backtrace mechanism, and is
supposed to check that the instruction before the supposed address X, is in
fact a legal CALL instruction. It is used to scan the stack, looking for
return addresses. Very TRICKY, see SYSTEM-EXTRAS.RED on PV: and P20:
@subsection(Interrupt Handler)
Also very crude at present; on the DEC-20, written as a loadable module,
P20:20-INTERRUPT.RED, using the JSYS package. This enables CNTRL-G, CTRL-T,
some stack and arithmetic overflows, bbinding them to some sortof throw
or Error routine.

 On the VAX, the file PV:TRAP.RED defines some signal setup, and
InitializeInterrupts routine, and is included in the kernel.
It associates each rap with a STDERROR call with a given message.

Not yet standardized. 

We really should to "bind" all trappable interupts to an
appropriate THROW('!$SIGNAL!$,n), and indicate whether
to treat as a Fatal Error, a Continuable Error, or not an
Error at all.

@subsection(Core Image Saving)
A way in which PSL (and most LISP@xs) get used, involves the ability to
load LISP and FASL code into an executing PSL, and then saving this
augmented "core-image" in a named file for subsequent restart later. Some
Operating Systems permit a running program to be saved into an executable
file, and then restarted from the beginning; others permit the saved
program to be continued at the instruction following the call to the SAVE
routine.  Some operating systems do not normally permit or encourage the
saving of a running program into an executable file, and there is a lot of
work to be done.

The model currently used in PSL is that a call on DUMPLISP(); does the
following:

@begin(enumerate)
calls RECLAIM(); to compact the heap, or move the upper heap into
the lower heap.

makes some system calls to free unused space, decreasing the executable
image; space is returned from HEAP, BPS and STACK.

the core-image is save a file, whose name is the string in the
global variable, DumpFileName!*.

execution continues without leaving the running program; to terminate,
the QUIT(); function must be called explicitly.

the saved executable file will restart "from-the-top", i.e. by calling the
machine specific "startup" function defined in MAIN-START.RED, which calls initialization
functions CLEARBINDINGS(), CLEARIO(), INITIALIZEINTERRUPTS(), etc.; . Then
the Startup function calls MAIN();, which can be redefined by the user
before calling DUMPLISP(); .  MAIN() typically calls StandardLISP() or
RLISP(), or some other TopLoop.  This startup function also has a LISP
accesible name, RESET.
@end(Enumerate)

On some machines, the core-image will automatically start "from-the-top",
unless effort is expended to change the "restart-vector' (e.g. the TOPS-20
SSAVE jsys on the DEC-20);
on others, an explicit LINKE CALL (a JUMP) to RESET should be included
after the core-save call, to ensure execution of RESET (e.g., the CTSS
DROPFILE call on the CRAY-1). 

On the VAX under UNIX, a new function UNEXEC
was written in C, to convert an executing program back into "a.out" format.

[What about VAX and APOLLO].

See the files MAIN-START.RED and DUMPLISP.RED on P20: and PV:.

@subsection(Miscellaneous)
To use EMODE and PRLISP on some systems, a "raw" I/O mode may be required.
See the PBIN, PBOUT, CHARSININPUTBUFFER, ECHOON and ECHOOFF functions in
EMOD2:RAWIO.RED and SYSTEM-EXTRAS.RED.

Some sort of system-call, fork or smilarch primitives are useful, clearly
system dependent. See the JSYS and EXEC package on P20:, or the SYSTEM
call in PV:SYSTEM-EXTRAS.RED (written in C as Foreign Function).

This set is not yet standardized.

