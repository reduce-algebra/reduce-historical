@use[bibliography = "<galway.scribe>biblio.bib"]

@make[article]
@style[references = STDalphabetic]
@style[spacing 1]
@style[indentation 5]
@modify[enumerate, numbered=<@a. @,@i. >, spread 0, above 1, below 1]
@modify[itemize,spread 0, above 1, below 1]
@modify[example, above 1, below 1]
@modify[description, spread 1, above 1, below 1]
@modify[appendix, numbered=<APPENDIX @A: >]
@pageheading[Left  "Utah Symbolic Computation Group",
             Right "June 1982",
             Line "Operating Note No. 69"
            ] 
@set[page=1]
@newpage[]
@begin[titlepage]
@begin[titlebox]
@begin[center]
@b[A Guide to EMODE]

by

William F. Galway and Martin L. Griss

Department of Computer Science
University of Utah
Salt Lake City, Utah  84112

Last Revision: @value[date]
@end[center]
@end[titlebox]

@begin[abstract]
EMODE is a LISP-based EMACS-like editor that runs on the PSL system.  This
document is meant to serve as a guide to using EMODE--but will only be
roughly up to date, since the system is in a state of transition.
@end[abstract]

@begin[Researchcredit]
Work supported in part by the National Science Foundation under Grant No.
MCS80-07034.
@end[Researchcredit]
@end[titlepage]

@pageheading[Left "Guide to EMODE",
             Right "@value(Page)"]

@set[page=1]
@newpage[]

@section[Introduction and Acknowledgments]
@Comment{Needs more?}
This paper describes the EMODE editor being developed for PSL
@cite[PSL-manual].  EMODE is an interactive, EMACS like
@cite[STALLMAN-ARTICLE-81], screen editor.  EMODE provides multiple
windows, can simultaneously support different "modes" of editing in
different buffers, and supports a variety of CRT terminals such as the
Teleray 1061 and the DEC VT-100.

Several people have made contributions to EMODE.  EMODE itself is based on
an earlier editor EMID @cite[Armantrout81], written by Robert Armantrout
and Martin Griss for LISP 1.6.  Tony Carter has used EMODE to develop
several large packages for VLSI circuitry design @cite[Carter81,
Carter-THESIS].  Optimizations for the Vax version, and many useful
comments, have been provided by Russ Fish.  Several features have been
added by Alan Snyder and Cris Perdue at Hewlett Packard Research Labs.
Cris implemented the current version of "mode lists", while Alan has
implemented a huge number of commands and improved the efficiency of
several operations.   

@section[Running EMODE]
EMODE is available as a "loadable" file.  It can be invoked as follows:
@begin[example]
@@PSL:RLISP
[1] load emode;
[2] emode();
@end[example]

Of course, you may choose to invoke RLISP (or PSL) differently, and to
perform other operations before loading and running EMODE.  From this point
on the term "PSL" will be used to refer to this family of systems,
independently of whether they use Lisp or RLISP syntax.

The terminal that EMODE uses by default is determined by its
LOADing the file DEFAULT-TERMINAL.  At the University of Utah this
is the TELERAY driver.  At other sites, some other driver may be
chosen as the default.  To use a different terminal you must LOAD
in a different "driver file" after loading EMODE.  For example, to
run EMODE on the Hewlett Packard 2648A terminal, you could type:
@begin[example]
@@PSL:RLISP
[1] load emode, hp2648a;
[2] emode();
@end[example]

The following drivers are currently available:
@begin[description,spread 0]
AAA@\For the Ann Arbor Ambassador.

DM1520@\For the Datamedia 1520.

HP2648A@\For the Hewlett Packard 2648A and similar Hewlett Packard
terminals.

@Comment{Should we be this specific?}
TELERAY@\For the Teleray 1061.

VT52@\For the DEC VT52.

VT100@\For the DEC VT100.
@end[description]
See section @ref[terminal-drivers] for information on creating new terminal
drivers.

EMODE is quite similar to EMACS @cite[EMACS-manual, STALLMAN-ARTICLE-81],
although it doesn't have nearly as many commands.  A detailed list of
commands is given in appendix @ref[key-bindings].  This information can
also be obtained by typing @w["HELP EMODE;"] to RLISP, or (equivalently) by
reading the file PH:EMODE.HLP.

The notation used here to describe character codes is basically the same as
that used for EMACS.  For example: C-Z means "control-Z", the character
code produced by typing Z while holding down the control key.  The ascii
code for a control character is the same as the 5 low order bits of the
original character--the code for Z is 132 octal, while the code for C-Z is
32 octal.  M-Z means "meta-Z", the character produced by typing Z while
holding down the meta key.  To support those terminals without a meta key,
the same result can normally be achieved by typing two characters--first
the ESCAPE character, then the Z character.  The ascii code for a meta
character is the same as the original character with the parity bit
set--the code for M-Z is 332 octal.  (Some terminals use the ESCAPE
character for other purposes, in which case the "META prefix" will be some
other character.)  Rather than using the EMACS convention, we write
"control-meta" characters (such as C-M-Z) as "meta-control" characters
(M-C-Z), since the latter notation better reflects the internal code (232
octal for M-C-Z).  The C-Z character is used as a "meta-control" prefix, so
one way to type M-C-Z is to type @w[C-Z C-Z].  (Another way to type it is
to hold down the meta and control keys and type "Z".)

When EMODE is started up as described above, it will immediately enter "two
window mode".  To enter "one window mode", you can type "C-X 1" (as in
EMACS).  Commands can be typed into a buffer shown in the top window.  The
result of evaluating a command is printed into the OUT_WINDOW buffer (shown
in the bottom window).  To evaluate the expression starting on the current
line, type M-E.  M-E will (normally) automatically enter two window mode if
anything is "printed" to the OUT_WINDOW buffer.  If you don't want to see
things being printed to the output window, you can set the variable
!*OUTWINDOW to NIL.  (Or use the RLISP command "OFF OUTWINDOW;".)  This
prevents EMODE from automatically going into two window mode when something
is printed to OUT_WINDOW.  You must still use the "C-X 1" command to enter
one window mode initially.

Figure @ref[two-window-figure] shows EMODE in two window mode.  In this
mode the top window includes everything above (and including) the first
line of dashes.  This is followed by a single line window, showing the
current prompt from PSL.  Beneath this is the "output window", the window
which usually shows the OUT_WINDOW buffer.  This is followed by another
single line window, which EMODE uses to prompt the user for values (not the
same as PSL's prompt).

@begin[figure]
@begin[example]
% Commands can be typed in the top window.
% When they're executed the value is printed into
% the OUT_WINDOW buffer.

x := '(now is the time);
y := cddr x;


----MAIN-----------------------------------------85%---
[7]
-------------------------------------------------------
NIL
(NOW IS THE TIME)
(THE TIME)






----OUT_WINDOW-----------------------------------75%---
File for photo: s:twowindow.photo
@end[example]
@caption[Two window mode]
@tag[two-window-figure]
@end[figure]

Figure @ref[one-window-figure] shows EMODE in one window mode.  The "top
window" takes up most of the screen, followed by EMODE's prompt line, and
then by PSL's prompt line.

@begin[figure]
@begin[example]
% Commands can be typed in the top window.
% When they're executed the value is printed into
% the OUT_WINDOW buffer.

x := '(now is the time);
y := cddr x;













----MAIN-----------------------------------------85%---
File for photo: s:onewindow.photo
[7]
@end[example]
@caption[One window mode]
@tag[one-window-figure]
@end[figure]

The BREAK handler has been modified by EMODE to "pop up" a "break window
menu".  This is illustrated in figure @ref[break-window-figure].  The
commands in the menu can be executed with the M-E command, and you can also
edit the BREAK buffer just like any other buffer.  If you wish to move to
another window, use the @w[C-X N] command.  This may cause the break window
to disappear as it is covered by some other window, but @w[C-X P] will find
it and pop it to the "top" of the screen again.
@begin[figure]
@begin[example]

cdr 2;             +------------------------------+
                   |A ;% To abort                 |
                   |Q ;% To quit                  |
                   |T ;% To traceback             |
                   |I ;% Trace interpreted stuff  |
                   |R ;% Retry                    |
                   |C ;% Continue,                |
                   |   % using last value         |
----MAIN-----------|? ;% For more help            |-
4 lisp break>      +----BREAK---------------11%---+
----------------------------------------------------
NIL
***** An attempt was made to do CDR on `2', which is
 not a pair {99}
Break loop




----OUT_WINDOW-----------------------------------75%---
File for photo: s:breakwindow.photo
@end[example]
@caption[A break window (doctored from the original)]
@tag[break-window-figure]
@end[figure]

EMODE is not very robust in its handling of errors.  Here's a summary of
known problems and suggestions on how to deal with them:
@begin[description]
Garbage collection messages "blow up":@\Printing messages into EMODE
buffers involves CONSing, so the system blows up if it tries to print a
message from inside the garbage collector.  EMODE sets GC OFF at load time.
Always run EMODE with GC OFF.

@begin[multiple]
Terminal doesn't echo:@\This can be caused by abnormal exits from EMODE.
If PSL is still running, you can call the routine "EchoOn" to turn
echoing back on.  (It's the routine "EchoOff" that turns echoing off, and
starts "raw output" mode.)

Otherwise, as may happen on the Vax running Unix, you will have to give
shell commands to turn echoing back on.  This is best done by defining the
following alias in your ".login" file.
@begin[example]
alias rst 'reset; stty -litout intr ^C'
@end[example]
(That's a "control-C", not "uparrow C".)  The "rst" command must be typed
as "<LF>rst<LF>" because carriage-return processing is turned off.
@end[multiple]

"Garbled" printout:@\This is probably caused by EMODE's not running in "raw
output" mode--a problem which can be caused by some other errors.  A cure
is to type @w[C-Z C-Z] to leave EMODE, and then to call EMODE again.  This
should reset the terminal mode to "raw mode" (by calling EchoOff).  (The
@w[C-Z C-Z] must be followed by a linefeed on the Vax, to force the
@w[C-Z C-Z] to be read.)

@begin[multiple]
Stuck in an error:@\This is often caused by trying to evaluate an expression
that lacks a closing parenthesis (or some other terminator)--producing a
message something like:
@begin[example]
***** Unexpected EOF while reading ...
@end[example]
If it's obvious that an additional parenthesis will cure the problem, you
can use @w[C-X N] to select the input window and insert it.  Then position
the cursor to the left of the parenthesis and use @w[C-X N] to select the
break window and "Quit".

Otherwise you should use the "Abort" option of the break handler.
Currently this resets the terminal mode (at least on the DEC-20), so you'll
have to restart EMODE as described above.  The BREAK window will still be
present on the screen after restarting, even though you are no longer in
the break loop.  You can use the @w[C-X 2] or @w[C-X 1] command to get rid
of the break window, and then use the @w[C-X B] command to select some
buffer other than the break buffer.
@end[multiple]
@end[description]

@section[A Guide to the Sources and Rebuilding]
The "primary" sources for EMODE reside on UTAH-20:

@begin[description]
PES:@\Is defined locally as <GALWAY.EMODE.V2>.  This directory is for the
"version 2" of EMODE--being maintained now.  The corresponding "logical
name" on the VAX is "$pes".

PE:@\Is defined as <PSL.EMODE>.  Holds sources and documentation which may
be generally useful to the public.  It includes sources for the various
terminal drivers available for EMODE.  (Further described in section
@ref[terminal-drivers].)  The corresponding logical name on the VAX is
"$pe".
@end[description]

The file PES:BUILD-EMODE.CTL is the command file for building EMODE on the
DEC-20.  Use SUBMIT or DO to run the command file, which builds EMODE in
two parts on the local directory: EMODE-B-1.B and EMODE-B-2.B.
PES:BUILD-EMODE.CSH (or $pes/build-emode.csh) is the build file for the
VAX.  It also builds the binary files on the "local directory".  On both
machines the ".B" files for the terminal drivers and for RAWIO.B are built
separately.

The PES:EMODE.TAGS file can be used with the TAGS facility provided by
EMACS on the DEC-20.  (Highly recommended!)

@section[Terminology:  Buffers, Views/Windows, and Virtual Screens]
@Comment{Need to say more about NSTRUCT, refer to some manual.}

"Buffers", "views", and "virtual screens" are the three major data
structures in EMODE.  Virtual screens correspond fairly closely to what are
often called @i[windows] in other systems.  They are rectangular regions on
the screen, possibly overlapping, that characters can be written to.
A virtual screen provides a sort of pseudo-hardware.  The operations that
can be performed on a virtual screen are modeled after what can be done
with a real terminal.  The use of a virtual screen provides these
advantages:
@begin[itemize]
Operations on a virtual screen are machine independent.  (To some extent,
this will be less true if we try to support "fancier" graphics.)

The "bandwidth problem" of maintaining the screen image is isolated to the
virtual screen package--other programs don't have to worry about the
problem.

Several virtual screens can be shown on one physical screen.
@end[itemize]
Virtual screens are implemented as "Structs" using the "DefStruct" facility
provided by the loadable file "NSTRUCT".

Buffers hold the data to be edited, possibly something other than text,
depending on the buffer's "data mode".  Views are data structures used to
display buffers on the screen, they may be made of several virtual screens.
The term @i["window"] is often used instead of "view", when you see the one
term it should be possible to substitute the other.

Buffers and views are implemented as "environments".  An environment is an
association list of @w[(NAME . VALUE)] pairs.  (These association lists are
sometimes referred to as "descriptors".)  The usual method for working with
an environment is "restoring" (or "selecting") the environment by calling
the procedure "RestoreEnv".  This sets each variable name in the list to
its associated value.  The procedure "SaveEnv" does the inverse operation
of updating the values of each variable name in the association list.
(This is done "destructively", using RPLACD.)  The names in an environment
are sometimes called "per-environment" variables.  Names in "buffer
environments" are called "per-buffer variables", and similarly for
"per-view variables".

Buffers and views are just environments that follow certain conventions.
These conventions are that they always include certain @w[(name . value)]
pairs--i.e. that they always include certain "per-buffer" or "per-view"
variables.  For example, the required per-buffer variables include:
@begin[description]
buffers_file@\The name (a string) of a file associated with the buffer, or
NIL if no file is associated with the buffer.

buffers_view_creator@\A routine that creates a "view" (or "window") looking
into the buffer.
@end[description]
In addition to the required per-buffer variables, text buffers include
variables containing things like the text being edited in the buffer and
the location of "point" in the buffer.

The required per-view variables include:
@begin[description]
windows_refresher@\(Which should actually be called the "views_refresher")
defines a routine to be the refresh algorithm for whatever data structure
this view looks into.

WindowsBufferName@\Is the name (an ID) of the buffer that the view looks
into.
@end[description]
Views into text buffers include additional information such as a virtual
screen to display the text in, and "cache" information to make refreshing
faster.

The choice of whether variables should be per-buffer or per-view is
sometimes unclear.  For example, it would seem to make better sense to have
"point" be part of a view, rather than a buffer.  This would allow the user
to have two windows looking into different parts of the same buffer.
However, it would also require the selection of a window for the many
functions that insert strings into the buffer, delete strings from the
buffer, etc., since these routines all work around the current "point".
Somehow it seems unnatural to require the selection of a @i[view] for these
@i[buffer] operations.  The current decision is to make point a per-buffer
variable.

Further details on buffers and views for different modes are given in
section @ref[creating-modes].

A list of all the buffers in EMODE is stored in the variable "BufferNames"
as a list of @w[(name . environment)] pairs .  These pairs are created with
the routine "CreateBuffer".

A list of "active" views in EMODE is stored in the variable "WindowList".
This is simply a list of "environments" (association lists as described
above).  Unlike buffers, views are not referred to by name.  Instead,
specific views can be referred to by storing their environment in a
variable (such as "BreakWindow").

@section[Modes and Key bindings in EMODE]
@label[key-modes]
There are two aspects to "modes" in EMODE.  One is the choice of the data
structure to be edited within a buffer.  Until recently there has only been
one kind of structure: "text".  As discussed in section
@ref[creating-modes] EMODE now provides tools for editing other, user
defined, structures.

@begin[Comment]
Is this DISTINCTION between key bindings and the binding of other variables
really VALID?
@end[Comment]

The other aspect of "modes", discussed in this section, is the binding of
"handler" routines to terminal keys (or sequences of keys for multi-key
commands).  A simple version of this would associate a table of handlers
(indexed by character code) with each buffer (or view).  The method
actually used is more complicated due to a desire to divide keyboard
bindings into groups that can be combined in different ways.  For example,
we might have a text mode and an Rlisp mode, and an optional Word
Abbreviation Mode that could be combined with either of them to cause
automatic expansion of abbreviations as they are typed.

Implementing optional keyboard bindings that can @i[removed] as well as
@i[added] is difficult.  Consider the situation with an optional
"Abbreviation Mode" and an optional "Auto Fill Mode".  Turning on either
mode redefines the space character to act differently.  In each case, the
new definition for space would be something like "do some fancy stuff for
this submode, and then do whatever space used to do".  Imagine the
difficulties involved in turning on "Abbreviation Mode" and then "Auto Fill
Mode" and then turning off "Abbreviation Mode".

EMODE's solution to the problem is based on the method suggested in
@cite[FINSETH].  A @i[single], @i[global] "dispatch vector" is used, but is
rebuilt when switching between buffers.  The mode for each buffer is stored
as a list of expressions to be evaluated.  Evaluating each expression
enters the bindings for an associated group of keys into the vector.
Incremental modes can be added or deleted by adding or deleting expressions
from the list.  Although changing modes is fairly time consuming (more than
a few microseconds), we assume that this is rare enough that the overhead
is acceptable.  NOTE that simply changing an entry in the dispatch vector
will not work--since any switching between buffers will cause the entry to
be permanently lost.

The dispatch "vector" is actually implemented as a combination of a true
PSL vector "MainDispatch", indexed by character code, and an association
list "PrefixAssociationLists" used to implement two character commands.
Currently the only two character commands start with the "prefix character"
C-X, although the mechanism is more general.  Prefix characters are
"declared" by calling the routine "define_prefix_character" (refer to code
for details).  Bindings for prefix-character commands are stored in
PrefixAssociationLists as an association list of association lists.  The
top level of the list is "indexed" by the prefix character, the next level
contains @w[(character . handler)] pairs indexed by the character following
the prefix character.

The list of expressions for building the dispatch vector is called the
"mode list", and is stored in the per-buffer variable
"ModeEstablishExpressions".  See the following section for more on how
ModeEstablishExpressions is used in the declaration of a mode.  The
procedure "EstablishCurrentMode" evaluates these expressions in reverse
order (the last expression in the list is evaluated first) to establish the
keyboard dispatch vector used for editing the current buffer.  Reverse
order is used so that the @i[last] expression added to the @i[front] of the
list will be evaluated last.  EstablishCurrentMode must be called after
changing the mode list for the current buffer and when switching to a
different buffer @i[for editing from the keyboard].  The routine
SelectBuffer switches to a buffer without "establishing" the buffer's mode.
This saves the cost of setting up the dispatch vector when it isn't needed
(which is the case for most "internal operations" on buffers).

The expressions in ModeEstablishExpressions can execute @i[any] code
desired.  This generality is rarely needed, the usual action is to call the
routine SetKeys with a list of @w[(character . handler)] pairs.  For
example, the mode list for text mode is defined by this Lisp code:
@begin[example]
(setf FundamentalTextMode
  '((SetKeys TextDispatchList)
     (SetKeys BasicDispatchList)
     (NormalSelfInserts)))
@end[example]
The RLISP mode is built "on top of" FundamentalTextMode as follows:
@begin[example]
(setf RlispMode
  (cons
    '(SetKeys RlispDispatchList)
    FundamentalTextMode))
@end[example]

This section taken from the code that builds BasicDispatchList shows what a
"key list" for the SetKeys routine should look like:
@begin[example]
(setf BasicDispatchList
  (list
    (cons (char ESC) 'EscapeAsMeta)
    (cons (char (cntrl U)) '$Iterate)
    (cons (char (cntrl Z)) 'DoControlMeta)

    % "C-X O" switches to "next window" (or "other
    % window" if in "two window mode").
    (cons (CharSequence (cntrl X) O) 'next_window)

    (cons (CharSequence (cntrl X) (cntrl F)) 'find_file)
          .
          .
          .
@end[example]
Note that the pairs in a key list can specify character sequences like
"@w[(cntrl X) O]" as well as single characters.

At runtime, after they're created, key lists can be most easily modified by
calling the routine AddToKeyList.  For example
@begin[example]
(AddToKeyList
  'RlispDispatchList
  (char (meta (cntrl Z)))
  'DeleteComment)
@end[example]
could be executed to add a new, "delete comment" handler to RLISP mode.

The routine SetTextKey is equivalent to adding to the key list
TextDispatchList (see code).  For example
@begin[example]
(SetTextKey (char (meta !$)) 'CheckSpelling)
@end[example]
could be executed to add a new "spelling checker" command to text mode (and
other modes such as RLISP mode that incorporate text mode).  SetTextKey
seems to correspond most closely to EMACS's "Set Key" command.

The routine "SetLispKey" is also defined for adding bindings to "Lisp
mode".  (There is no "SetRlispKey" routine in EMODE, although it would be
easy to define for yourself if desired.)

@section[Creating New Modes]
@label[creating-modes]
To define a new mode you must provide a "buffer creator" routine that
returns a "buffer environment" with the required per-buffer variables along
with any other state information needed for the type of data being edited.
You need to "declare" the mode by calling the routine "declare_data_mode".
It's also possible to associate the mode with a file extension by calling
the routine "declare_file_mode".

For example, the current EMODE declares the modes, "text" and
"rlisp", as follows:
@begin[example]
(declare_data_mode "text" 'create_text_buffer)
(declare_data_mode "rlisp" 'create_rlisp_buffer)

(declare_file_mode "txt" 'create_text_buffer)
(declare_file_mode "red" 'create_rlisp_buffer)
@end[example]
The second argument to both routines is the "buffer creator" routine for
that mode.  The first argument to declare_data_mode is a "name" for the
mode.  The first argument to declare_file_mode is a file extension
associated with that mode.

The conventions for "buffer environments" are that they always include certain
@w[(name . value)] pairs--i.e. that they always include certain
"per-buffer" variables.  These variables are:
@begin[description]
ModeEstablishExpressions@\A list of expressions to evaluate for
establishing the keyboard bindings for the buffer's mode.

buffers_file@\The name (a string) of a file associated with the buffer, or
NIL if no file is associated with the buffer.

buffers_file_reader@\A routine to APPLY to one argument--a PSL io-channel.
The routine should read the channel into the current buffer.

buffers_file_writer@\A routine to APPLY to an io-channel.  The routine
writes the current buffer out to that channel.

buffers_view_creator@\A routine to create a "view" (or "window") looking
into the buffer.  This is described in more detail below.
@end[description]

For example, the buffer creator for "text mode" is:
@begin[example]
(de create_text_buffer ()
  (cons
    (cons 'ModeEstablishExpressions  FundamentalTextMode)
    (create_raw_text_buffer)))
@end[example]
Most of the work is done by create_raw_text_buffer, which does everything
but determine the keyboard bindings for the buffer.  Here's the code with
comments removed:
@begin[example]
(de create_raw_text_buffer ()
  (list
    (cons 'buffers_view_creator  'create_text_view)
    (cons
      'buffers_file_reader
      'read_channel_into_text_buffer)
    (cons
      'buffers_file_writer
      'write_text_buffer_to_channel)
    (cons 'buffers_file  NIL)

    (cons 'CurrentBufferText (MkVect 0))
    (cons 'CurrentBufferSize  1)
    (cons 'CurrentLine  NIL)
    (cons 'CurrentLineIndex  0)
    (cons 'point  0)
    (cons 'MarkLineIndex  0)
    (cons 'MarkPoint  0)
    ))
@end[example]
Other modes based on text can be similarly defined by consing an
appropriate binding for ModeEstablishExpressions to the environment
returned by create_raw_text_buffer.

Of course we need some way of "viewing" buffers once they've been created.
The per-buffer variable "buffers_view_creator" is responsible for creating
a view into a buffer.  The "view creator" is typically invoked by the
routine "select_or_create_buffer".

The required per-view variables are:
@begin[description]
@begin[group]
windows_refresher@\Which should actually be called the "views_refresher",
is a routine to APPLY to no arguments.  This routine is the refresh
algorithm for whatever data structure this view looks into.
@end[group]

@begin[group]
WindowsBufferName@\Is the name (an ID) of the buffer that the view looks
into.
@end[group]

@begin[group]
views_cleanup_routine@\A routine that's called when a view is being deleted
from the screen.  Different views may require different kinds of cleaning
up at this point.  For example, they should "deselect" any "virtual
screens" that make up the view.
@end[group]
@end[description]

The view creator for text structures is "create_text_view".  This routine
typically modifies and returns the current view (which is almost certainly
also looking into text in the current system) so that the current view
looks into the new text buffer.  Most of the real work of creating text
views is done by the routine "FramedWindowDescriptor", which is typically
invoked by the routines "OneWindow" and "TwoRFACEWindows".  (So, although
select_or_create_buffer is one way of creating views into a buffer, there's
quite a bit of freedom in using other methods for creating views.)

@section[Manipulating Text Buffers]
The text in "text buffers" is stored as a vector of strings in the
per-buffer variable "CurrentBufferText"--with the exception of a "current
line" (stored in the per-buffer variable "CurrentLine"), which is a linked
list of character codes.  The CurrentLine is the line indexed by
"CurrentLineIndex".  Refer to the routine create_text_buffer for details of
the contents of a text buffer.

It's an easy mistake to modify CurrentLine but to forget to update the
CurrentBufferText when moving to a new line.  For this reason, and because
the representation used for text may change in the future, you should use
the utilities provided (mostly) in PES:EMODE1.RED to manipulate text.  The
procedure "GetLine(x)" can be used to get line x as the current line.  The
procedure "PutLine()" is used to store the current line back into
CurrentBufferText.  The procedure "SelectLine(x)" first "puts away" the
current line, and then "gets" line x.

It would seem natural to move forward a line in the text by doing something
like
@begin[example]
SelectLine(CurrentLineIndex + 1);
@end[example]
but you should resist the temptation.  For one thing, SelectLine makes
little attempt to check that you stay within the limits of the buffer.
Furthermore, future representations of text may not use integers to index
lines.  For example, some future version may use a doubly linked list of
"line structures" instead of a vector of strings.

So, you should use the routines "NextIndex" and "PreviousIndex" to
calculate new "indices" into text, and you should also check to make sure
that CurrentLineIndex is within the bounds of the buffer.  You can probably
just use the routines "!$ForwardLine" and "!$BackwardLine", (or
"!$ForwardCharacter" and "!$BackwardCharacter").  You should also read some
of the code in EMODE1.RED before attempting your own modifications.  (Much
of the code is rather ugly, but it does seem to work!)

@section[Evaluating Expressions in EMODE Buffers]
The "M-E" command for evaluating an expression in a buffer (of the
appropriate mode) depends on I/O channels that read from and write to EMODE
buffers.  This is implemented in a fairly straightforward manner, using the
general I/O hooks provided by PSL.  (See the Input/Output chapter of the
PSL Manual for further details.)  The code for EMODE buffer I/O resides in
the file RFACE.RED.

The tricky part of implementing M-E is making it fit with the
READ/EVAL/PRINT loop that Lisp and other front ends use.  The most obvious
scheme would be to have EMODE invoke one "READ/EVAL/PRINT" for each M-E
typed.  However, this doesn't work well when a break loop, or a user's
program, unexpectedly prompts for input.

Instead, the top level read functions in PSL call the "hook" function,
MakeInputAvailable(), which allows the user to edit a buffer before the
reader actually takes characters from the current standard input channel.
Examples of top level read functions are READ (for Lisp), and XREAD (for
RLISP).  If you define your own read function, for example--to use with the
general TopLoop mechanism, it should also call MakeInputAvailable before
trying to actually read anything.

When EMODE dispatches on M-E, it RETURNS to the routine that called it
(e.g. READ), which then reads from the selected channel (which gets
characters from an EMODE buffer).  After evaluating the expression, the
program then PRINTs to an output channel which inserts into another EMODE
buffer.  EMODE is then called again by the read routine (indirectly, via
MakeInputAvailable).

The fact that EMODE @i[returns to the reader] means that different buffers
cannot use different readers.  This can be a bit confusing when editing
several buffers with different kinds of code.  Simply switching to a buffer
with Lisp code does not cause the system to return to READ instead of
XREAD.  Implementing this would require some sort of coroutine or process
mechanism--neither of which are currently provided in PSL.  (However, it
may be possible to provide an acceptable approximation by having M-E
normally invoke a READ/EVAL/PRINT operation, while preserving the
MakeInputAvailable hook for exceptional situations.)

@section[Customizing EMODE for New Terminals]
@label[terminal-drivers]
The files PE:AAA.SL, PE:DM1520.SL, PE:HP2648A.SL, PE:TELERAY.SL, PE:VT52.SL,
and PE:VT100.SL define the different terminal drivers currently available.
Terminal drivers define some values and functions used to emit the
appropriate character strings to position the cursor, erase the screen and
clear to end of line. To define a new terminal, use one of the files as a
guide.  A listing of TELERAY.SL follows:
@begin[verbatim]
%
% TELERAY.SL - EMODE support for Teleray terminals
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        27 June 1982
% Copyright (c) 1982 University of Utah
%

% Screen starts at (0,0), and other corner is offset by (79,23)
% (total dimensions are 80 wide by 24 down).
(setf ScreenBase (Coords 0 0))
(setf ScreenDelta (Coords 79 23))

% Parity mask is used to clear "parity bit" for those terminals
% that don't have a meta key.  It should be 8#177 in that case.
% Should be 8#377 for terminals with a meta key.
(setf parity_mask 8#377)

(DE EraseScreen ()
  (progn
    (PBOUT (Char ESC))
    (PBOUT (Char (lower J)))))

(DE Ding ()
  (PBOUT (Char Bell)))

% Clear to end of line from current position (inclusive).
(DE TerminalClearEol ()
  (progn
    (PBOUT (Char ESC))
    (PBOUT (Char K))))

% Move physical cursor to Column,Row
(DE SetTerminalCursor (ColLoc RowLoc)
  (progn
    (PBOUT (char ESC))
    (PBOUT (char Y))
    (PBOUT (plus (char BLANK) RowLoc))
    (PBOUT (plus (char BLANK) ColLoc))))


@end[verbatim]
@Comment{Newpage???}
@newpage[]
@Comment{Section???}
@section[Bibliography]
@Bibliography[]
@newpage[]
@appendix[Default Keyboard Bindings for EMODE]
@label[key-bindings]
@include[keybindings.mss]

@newpage[]
@appendix[Some Important Fluid Variables]
Here is an incomplete list of the fluid ("global") variables in EMODE.
@begin[description]

@begin[group]
*outwindow@\A flag for PSL's ON/OFF mechanism.  When T, means that the
"output" (or OUT_WINDOW) window should be "popped up" when output occurs.
@end[group]

@begin[group]
*EMODE@\T when EMODE is running.  (Not quite the same as "runflag"
described below.  For example, runflag will be set NIL to cause EMODE to
leave a "recursive edit", but *EMODE stays T.)
@end[group]

@begin[group]
*RAWIO@\T when "raw I/O" is in effect.
@end[group]

@begin[group]
BasicDispatchList@\The "key list" for "basic" operations.
@end[group]

@begin[group]
BreakWindow@\The view for the "popup" break window.
@end[group]

@begin[group]
BufferNames@\An association list of the @w[(name . buffer-environment)]
pairs for all the buffers.
@end[group]

@begin[group]
CurrentBufferName@\The name of the currently selected buffer.
@end[group]

@begin[group]
CurrentBufferSize@\A per-buffer variable for text buffers, gives number of
lines actually within buffer.
@end[group]

@begin[group]
CurrentBufferText@\A per-buffer variable for text buffers.  A vector of
lines making up the buffer.
@end[group]

@begin[group]
CurrentLine@\A per-buffer variable for text buffers.  The contents (text)
of current line--as a linked list of character codes.  (Takes precedence
over whatever is contained in the text vector.)
@end[group]

@begin[group]
CurrentLineIndex@\A per-buffer variable for text buffers.  Index of the
"current line" within buffer.
@end[group]

@begin[group]
CurrentVirtualScreen@\Per-view variable for text windows (views), holds the
virtual screen used by the view.
@end[group]

@begin[group]
CurrentWindowDelta@\Per-view variable for text windows, gives window
dimensions as @w[(delta x . delta y)].
@end[group]

@begin[group]
CurrentWindowDescriptor@\The currently selected window environment.
@end[group]

@begin[group]
declared_data_modes@\List of @w[(mode-name . buffer-creator)] pairs for all
the declared modes.
@end[group]

@begin[group]
declared_file_extensions@\List of @w[(file-extension . buffer-creator)]
pairs for all modes with declared file extensions.
@end[group]

@begin[group]
EmodeBufferChannel@\Channel used for EMODE I/O.  Perhaps this should be
expanded to allow different channels for different purposes (break loops,
error messages, etc.)  (Or, perhaps the whole model needs more thought! )
@end[group]

@begin[group]
FirstCall@\NIL means re-entering EMODE, T means first time.
@end[group]

@begin[group]
FundamentalTextMode@\Mode list (list of expressions) for establishing
"fundamental" text mode.
@end[group]

@begin[group]
kill_buffer_ring@\Vector of vectors of strings--holds recently
deleted text.
@end[group]

@begin[group]
kill_opers@\list of (names of) handler routines that kill text.  NEEDS
MORE DOCUMENTATION!
@end[group]

@begin[group]
kill_ring_index@\Pointer to the most recent "kill buffer".
@end[group]

@begin[group]
last_buffername@\Name (a string) of the last buffer visited.
@end[group]

@begin[group]
last_operation@\The "last" routine dispatched to (before the "current
operation").
@end[group]

@begin[group]
last_search_string@\The last string searched for by a search command--used
as default for next search command.
@end[group]

@begin[group]
last_yank_point@\Vector of [buffer lineindex point], giving location
where last "yank" occured.
@end[group]

@begin[group]
LispDispatchList@\The "key list" for Lisp mode.
@end[group]

@begin[group]
LispMode@\The mode list for Lisp mode. 
@end[group]

@begin[group]
MainDispatch@\Dispatch table (vector), an entry for each key.
@end[group]

@begin[group]
minor_window_list@\List of windows to be ignored by the "next_window"
routine.
@end[group]

@begin[group]
ModeEstablishExpressions@\List of expressions to be evaluated.  Each
expression is expected to modify (add to?) the dispatch table.
@end[group]

@begin[group]
OldErrOut@\The error output channel in effect before EMODE was started.
@end[group]

@begin[group]
OldStdIn@\The standard input channel in effect before EMODE was started.
@end[group]

@begin[group]
OldStdOut@\The standard output channel in effect before EMODE was started.
@end[group]

@begin[group]
point@\A per-buffer variable for text buffers.  Number of chars to the left
of point within CurrentLine.
@end[group]

@begin[group]
PrefixAssociationLists@\Additional dispatch information for prefixed
characters.
@end[group]

@begin[group]
PrefixCharacterList@\A list of the declared prefix characters.
@end[group]

@begin[group]
pushed_back_characters@\A list of characters pushed back for EMODE's
command reader.  This may be used when a command isn't recognized by one
dispatcher, so it can push the characters back and pass control to another
dispatcher.
@end[group]

@begin[group]
reading_from_output@\Kludge flag, T when input buffer is OUT_WINDOW buffer
(for M-E).
@end[group]

@begin[group]
RlispDispatchList@\The "key list" for RLISP mode.
@end[group]

@begin[group]
RlispMode@\The mode list for RLISP mode. 
@end[group]

@begin[group]
runflag@\EMODE continues its READ/DISPATCH/REDISPLAY until this flag is NIL.
@end[group]

@begin[group]
SelfInsertCharacter@\Character being dispatched upon.  (Usually the last
character typed.)
@end[group]

@begin[group]
ShiftDisplayColumn@\Amount to shift things to the left by before
(re)displaying lines in a text view.
@end[group]

@begin[group]
TextDispatchList@\The "key list" for fundamental text mode.
@end[group]

@begin[group]
Two_window_midpoint@\Gives location (roughly) of dividing line for two
window mode.
@end[group]

@begin[group]
WindowList@\List of active windows (views).
@end[group]

@begin[group]
WindowsBufferName@\Required per-view variable giving the name of the buffer
being viewed.
@end[group]

@begin[group]
Windows_Refresher@\Required per-view variable giving the refresh algorithm
to be APPLYed for this view.
@end[group]

@begin[group]
Window_Image@\Per-view variable for text views, holding information for
speeding up refresh.
@end[group]

@end[description]
