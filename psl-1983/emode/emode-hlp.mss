@Comment{This file generates the help file EMODE.HLP}
@device[file]
@heading[EMODE - A PSL Screen Editor]
Comments and questions about EMODE should be addressed to Will Galway
(GALWAY@@UTAH-20).  Further documentation is available in the file EMODE.LPT
on logical device PE:

@subheading[Running EMODE]
@Comment{The following text should really be implemented as an include
file?  Shared with EMODE.MSS?}
EMODE is available as a "loadable" file.  It can be invoked as follows:
@begin[example]
@@PSL:RLISP
[1] load emode;
[2] emode();
@end[example]

Of course, you may choose to invoke RLISP (or "just plain Lisp")
differently, and to perform other operations before loading and running
EMODE.

EMODE is built to run on a site dependent "default terminal" as the default
(a Teleray terminal at the University of Utah).  To use some other terminal
you must LOAD in a set of different driver functions after loading EMODE.
For example, to run EMODE on the Hewlett Packard 2648A terminal, you could
type:
@begin[example]
@@PSL:RLISP
[1] load emode;
[2] load hp2648a;
[3] emode();
@end[example]

The following drivers are currently available:
@begin[description,spread 0]
AAA@\For the Ann Arbor Ambassador.

DM1520@\For the Datamedia 1520.

HP2648A@\For the Hewlett Packard 2648A (and similar HP terminals).

@Comment{Should we be this specific?}
TELERAY@\For the Teleray 1061.

VT52@\For the DEC VT52.

VT100@\For the DEC VT100.
@end[description]
See the file PE:EMODE.LPT for information on creating new terminal drivers.

When EMODE starts up, it will typically be in "two window mode".  To enter
"one window mode", you can type "C-X 1" (as in EMACS).  Commands can be
typed into a buffer shown in the top window.  The result of evaluating a
command is printed into the OUT_WINDOW buffer (shown in the bottom window).
To evaluate the expression starting on the current line, type M-E.  M-E
will (normally) automatically enter two window mode if anything is
"printed" to the OUT_WINDOW buffer.  If you don't want to see things being
printed to the output window, you can set the variable !*OUTWINDOW to NIL.
(Or use the RLISP command "OFF OUTWINDOW;".)  This prevents EMODE from
automatically going into two window mode when something is printed to
OUT_WINDOW.  You must still use the "C-X 1" command to enter one window
mode initially.

@subheading[Commands for EMODE]
@include[keybindings.mss]