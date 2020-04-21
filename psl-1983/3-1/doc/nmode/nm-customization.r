.so pndoc:nman
.part NM-CUSTOMIZATION manual
@Chapter[Simple Customization]
@label[NMODECustomization]
@node("customization")
  In this chapter we describe simple ways of customizing NMODE.

  NMODE is designed to be customizable; each user can rearrange things
to suit his taste.  Simple customizations are primarily of two types:
moving functions from one character to another, and setting variables
which functions refer to so as to direct their actions.  Beyond this,
extensions can involve redefining existing functions, or writing
entirely new functions and creating sharable libraries of them.
@index{redefining commands}
@Section[Init Files]
@node("init")
@index{init files}
@index{customization}
This section explains how to customize NMODE by redefining the effect of input
keystrokes.  NMODE is customized by executing Lisp forms.  These forms may be
executed directly within NMODE (using Lisp-E), or may be stored in an INIT
file, which is read by NMODE when it first starts up.  The name of the INIT
file read by NMODE is "NMODE.INIT" in the user's home directory.

There are three concepts that must be understood to customize NMODE: Commands,
Functions, and Modes.

@index{control}
@index{meta}
@index{character set}
1) Commands.  The effect of given keystroke or sequence of keystrokes in
NMODE is based on a mapping between "commands" and "functions".
A "command" may be either a single "extended character" or a sequence
of characters.  An extended character is a 9-bit character with
distinct "Control" and "Meta" bits.  Thus "C-M-A" is a single "extended
character", even though on many terminals you have to use two keystrokes
to enter it.  Extended characters are specified using the macro X-CHAR,
for example:
@verbatim{
  (x-char A)		the letter "A" (upper case)
  (x-char C-F)		Control-F
  (x-char C-M-Z)	Control-Meta-Z
  (x-char CR)		Carriage-Return
  (x-char TAB)		Tab
  (x-char BACKSPACE)	Backspace
  (x-char NEWLINE)	Newline
  (x-char RUBOUT)	Rubout
  (x-char C-M-RUBOUT)	Control-Meta-Rubout
}
(The macros described in this section are defined in the load module
EXTENDED-CHAR.)  It is important to note that on most terminals, some Ascii
control characters are mapped to extended "Control" characters and some aren't.
Those that aren't are: Backspace, CR, Newline, Tab, and Escape.  Even if you
type "CTRL-I" on the keyboard, you will get "Tab" and not "Control-I".  The
remaining Ascii control characters are mapped to extended "Control" characters,
thus typing "CTRL-A" on the keyboard gives "Control-A".

As mentioned above, a command can be a sequence of characters.  There are two
forms: Prefix commands and Extended commands.

@keyindex{C-X}
@index{prefix characters}
Prefix commands: A prefix command consists of two characters, the first of
which is a defined "prefix character".  In NMODE, there are 3 predefined prefix
characters: C-X, ESC, and C-].  Prefix commands are specified using the X-CHARS
macro, for example:
@verbatim{
  (x-chars C-X C-F)
  (x-chars ESC A)
  (x-chars C-] E)
}
@index{extended commands}
@keyindex{M-X}
@index{functions}
@index{commands}
Extended commands: An extended command consists of the character M-X and a
string.  Extended commands are defined using the M-X macro, for example:
@verbatim{ 
  (M-X "Lisp Mode")
  (M-X "Revert File")
}
The case of the letters in the string is irrelevant, except to specify how the
command name will be displayed when "completion" is used by the user.  By
convention, the first letter of each word in an extended command name is
capitalized.

2) Functions.  NMODE commands are implemented by PSL functions.  By convention,
most (but not all) PSL functions that implement NMODE commands have names
ending with "-COMMAND", for example, @fnc{move-forward-character-command}.

An NMODE command function should take no arguments.  The function can perform
its task using a large number of existing support functions; see PN:BUFFER.SL
and PN:MOVE-COMMANDS.SL for examples.  A command function can determine the
command argument (given by C-U) by inspecting global variables:
@verbatim{
  nmode-command-argument: the numeric value (default: 1)
  nmode-command-argument-given: T if the user specified an argument
  nmode-command-number-given: T if the user typed digits in the argument
}
See the files PN:MOVE-COMMANDS.SL, PN:LISP-COMMANDS.SL, and PN:COMMANDS.SL for
many examples of NMODE command functions.

3) Modes.  The mapping between commands and functions is dependent on the
current "mode".  Examples of existing modes are "Text Mode", which is the basic
mode for text editing, "Lisp Mode", which is an extension of "Text Mode" for
editing and executing Lisp code, and "Dired Mode", which is a specialized mode
for the Directory Editor Subsystem.

A mode is defined by a list of Lisp forms which are evaluated to determine the
state of a Dispatch Table.  The Dispatch Table is what is actually used to map
from commands to functions.  Every time the user selects a new buffer, the
Dispatch Table is cleared and the Lisp forms defining the mode for the new
buffer are evaluated to fill the Dispatch Table.  The forms are evaluated in
reverse order, so that the first form is evaluated last.  Thus, any command
definitions made by one form supersede those made by forms appearing after it
in the list.

Two functions are commonly invoked by mode-defining forms: 
@fnc{nmode-establish-mode}
and @fnc{nmode-define-commands}.  
@fnc{nmode-establish-mode} takes one argument, a list of
mode defining forms, and evaluates those forms.
Thus, @fnc{nmode-establish-mode} can
be used to define one mode in terms of (as an extension of or a modification
to) another mode.

@fnc{nmode-define-commands} takes one argument, a list of pairs, where
each pair consists of a COMMAND and a FUNCTION.  This form of list is
called a "command list".  Command lists are not used directly to map
from commands to functions.  Instead, @fnc{nmode-define-commands}
reads the command list it is given and for each COMMAND-FUNCTION pair
in the command list (in order), it alters the Dispatch Table to map
the specified COMMAND to the corresponding FUNCTION.

Note that as a convenience, whenever you define an "upper case" command, the
corresponding "lower case" command is also defined to map to the same function.
Thus, if you define C-M-A, you automatically define C-M-a to map to the same
function.  If you want the lower case command to map to a different function,
you must define the lower case command "after" defining the upper case command.

The usual technique for modifying one or more existing modes is to modify one
of the command lists given to @fnc{nmode-define-commands}.  The file PN:MODE-DEFS.SL
contains the definition of most predefined NMODE command lists, as well as the
definition of most predefined modes.  To modify a mode or modes, you must alter
one or more command lists by adding (or perhaps removing) entries.  Command
lists are manipulated using two functions:
@verbatim{
  (add-to-command-list list-name command func)
  (remove-from-command-list list-name command)
}
Here are some examples:
@verbatim{
(add-to-command-list
 'read-only-text-command-list (x-char M-@) 'set-mark-command)
 
  [The above form makes M-@ set the mark.]

(add-to-command-list
 'read-only-terminal-command-list (x-chars ESC Y) 'print-buffer-names-command)
 
  [The above form makes Esc-Y print a list of all buffer names.  Esc-Y is
   sent by HP264X terminals when the "Display Functions" key is hit.]
}
Note that these functions change only the command lists, not the Dispatch Table
which is actually used to map from commands to functions.  To cause the
Dispatch Table to be updated to reflect any changes in the command lists, you
must invoke the function @fnc{nmode-establish-current-mode}.
@Section[Variables]
@node("variables")
@index{variables}
@index{options}
@index{Fill Column}
  Since the init file consists of a series of PSL forms, it can
contain simple assignment statements which set up global variables in
NMODE.  A variable is a name which is associated with a value.  NMODE
uses many variables internally, and has others whose purpose is to be
set by the user for customization.  If you want to set a variable a
particular way each time you use NMODE, you can use your init file to
do so.  Global variables may also be set automatically by major modes.

  Two examples of global variables are *outwindow and nmode-default-mode.
Nmode-default-mode is the mode used for most newly created buffers. 
It is normally set to text-mode, but might be set to lisp-interface-mode
by a user who expects to be editing programs most of the time.
The other variable controls the automatic pop up of the output window.
If *outwindow is T, the output buffer will automatically appear if it is
not already displayed when output (i.e. from a lisp calculation) occurs.

Another example of such a variable is the Fill Column variable, which
specifies the position of the right margin (in characters from the
left margin) to be used by the fill and justify commands.

@Index{NMODE.VARS}
@index{variables}
  To set a variable, include in the init file a line containing
@verbatim{
(setq <variable_name> <variable_value>).
}
This is just an assignment statement in PSL.
To adjust the fill column to 60, for instance, include a line:
@verbatim{
(setq fill-column 60).  
}
@Section[Minor Modes]
@node("minormodes")
@index{minor modes}
@index{numeric arguments}
@index{mode line}
@index{toggling}
  Since init files can execute arbitrary PSL forms, they can run the
same functions that one can call from the terminal by appropriate commands.
In particular they can turn major or minor modes on or off.

  Minor modes are options which you can use or not.  For example, Auto
Fill mode is a minor mode in which Spaces break lines between words as
you type.  All the minor modes are independent of each other and of
the selected major mode.  Most minor modes say in the mode line when
they are on; for example, "Fill" in the mode line means that Auto Fill
mode is on.

  Minor modes are controlled by a global variable: nmode-minor-modes.
This is a list of currently active minor modes.  Rather than directly
setting this list, it is generally preferable to use some existing
functions to turn the modes on and off, since they correctly handle
some side effects.  Minor modes can be added to this list with
@fnc{activate-minor-mode} and removed from it with
@fnc{deactivate-minor-mode}.    For example, auto
fill mode can be turned on when NMODE is started by including
@verbatim{
(activate-minor-mode auto-fill-mode)
}
in the init file.

  Each minor mode is associated with a function that can be used to
turn it on or off.  The function turns the mode on if it was off and
off if it was on.  This is known as @dfn[toggling].  All the minor
mode functions are suitable for connecting to single or double
character commands if you want to enter and exit a minor mode
frequently.

@index{Auto Fill mode}
@keyindex{C-X F}
@index{Fill Column}
@fncindex{set-fill-column-command}
  Auto Fill mode allows you to type text endlessly without worrying
about the width of your screen.  Line separators are be inserted where
needed to prevent lines from becoming too long.  A variable called
fill-column sets the maximum number of columns allowed in a line.
@Note("Filling").
@node("kbdmac")

@Section[Keyboard Macros]

@WideCommands[
C-X (	Start defining a keyboard macro.

C-X )	End the definition of a keyboard macro.

C-X E	Execute the most recent keyboard macro.

C-U C-X (	Re-execute last keyboard macro and append to its definition.

C-X Q	Ask for confirmation when the keyboard macro is executed.

C-U C-X Q	Allow the user to edit for a while, each time the keyboard
macro is executed.

M-X Name Kbd Macro	Make the most recent keyboard macro into the
permanent definition of a command.

M-X Write Kbd Macro	Save a keyboard macro in a file.
]

@index{keyboard macros}
  A @dfn[keyboard macro] is a command defined by the user to abbreviate a
sequence of other commands.  If you discover that you are about to
type C-N C-D forty times, you can define a keyboard macro to do C-N
C-D and call it with a repeat count of forty.

@index{TECO}
  Keyboard macros differ from ordinary NMODE commands, in that they
are written in the NMODE command language rather than in TECO.  This
makes it easier for the novice to write them, and makes them more
convenient as temporary hacks.  However, the NMODE command language is
not powerful enough as a programming language to be useful for writing
anything intelligent or general.  For such things, TECO must be used.

  NMODE functions were formerly known as macros (which is part of the
explanation of the name NMODE), because they were macros within the
context of TECO as an editor.  We decided to change the terminology
because, when thinking of NMODE, we consider TECO a programming
language rather than an editor.  The only "macros" in NMODE now are
keyboard macros.

  You define a keyboard macro while executing the commands which are
the definition.  Put differently, as you are defining a keyboard
macro, the definition is being executed for the first time.  This way,
you can see what the effects of your commands are, so that you don't
have to figure them out in your head.  When you are finished, the
keyboard macro is defined and also has been, in effect, executed once.
You can then do the whole thing over again by invoking the macro.

@SubSection[Basic Use]

@index{C-X (}@index{C-X )}@index{C-X E}@fncindex{start kbd macro-command}@fncindex{end kbd macro-command}
@fncindex{execute kbd macro-command}
  To start defining a keyboard macro, type the @w[C-X (] command
(@fnc{start kbd macro-command}).  From then on, your commands continue to be
executed, but also become part of the definition of the macro.  "Def"
appears in the mode line to remind you of what is going on.  When you
are finished, the @w[C-X )] command (@fnc{end kbd macro-command}) terminates
the definition (without becoming part of it!).

  The macro thus defined can be invoked again with the C-X E command
(@fnc{execute kbd macro-command}), which may be given a repeat count as a
numeric argument to execute the macro many times.  @w[C-X )] can also
be given a repeat count as an argument, in which case it repeats the
macro that many times right after defining it, but defining the macro
counts as the first repetition (since it is executed as you define
it).  So, giving @w[C-X )] an argument of 2 executes the macro
immediately one additional time.  An argument of zero to @w[C-X E] or
@w[C-X )] means repeat the macro indefinitely (until it gets an
error).

  If you want to perform an operation on each line, then either you
should start by positioning point on the line above the first one to
be processed and then begin the macro definition with a C-N, or you
should start on the proper line and end with a C-N.  Either way,
repeating the macro will operate on successive lines.

  After you have terminated the definition of a keyboard macro, you
can add to the end of its definition by typing C-U @w[C-X (].  This is
equivalent to plain @w[C-X (] followed by retyping the whole
definition so far.  As a consequence it re-executes the macro as
previously defined.

@index{Name Kbd Macro}
  If you wish to save a keyboard macro for longer than until you
define the next one, you must give it a name.  If you do M-X Name Kbd
MacroFOO@return2{}, the last keyboard macro defined (the one which C-X E
would invoke) is turned into a function and given the name FOO.  M-X
FOO will from then on invoke that particular macro.  Name Kbd Macro
also reads a character from the keyboard and redefines that character
command to invoke the macro.  You can use a bit prefix character in
specifying the command; you can also type a C-X command to be
redefined.  When you have finished typing the command characters, Name
Kbd Macro asks you whether it should go ahead and redefine the
character.

@index{Write Kbd Macro}
  To save a keyboard macro permanently, do M-X Write Kbd Macro.
Supply the function name of the keyboard macro as a string argument,
or else it will ask you to type the character which invokes the
keyboard macro.  The keyboard macro is saved as a library which, when
loaded, automatically redefines the keyboard macro.  The filename is
read from the terminal.  Its second name should be :EJ, like other
libraries; that is the default.

@index{View Kbd Macro}
  To examine the definition of a keyboard macro, use the function View
Kbd Macro.  Either supply the name of the function which runs the
macro, as a string argument, or type the command which invokes the
macro when View Kbd Macro asks for it.

@SubSection[Executing Macros with Variations]

@index{C-X Q}@fncindex{kbd macro query-command}
  If you want to be allowed to do arbitrary editing at a certain point
each time around the macro (different each time, and not remembered as
part of the macro), you can use the C-U C-X Q command (@fnc{kbd macro
query-command}).  When you are defining the macro, this lets you do some
editing, which does @xxii[not] become part of the macro.  When you are done,
exit with @CMC[] to return to defining the macro.  When
you execute the macro, at that same point, you will again be allowed
to do some editing.  When you exit this time with @CMC[], the execution
of the macro will resume.  If you abort the recursive editing level
with C-], you will abort the macro definition or execution.

@index{Query Replace}@index{Space}@index{Rubout}@index{C-L}@index{C-R}@index{Altmode}
  You can get the effect of Query Replace, where the macro asks you
each time around whether to make a change, by using the command C-X Q
with no argument in your keyboard macro.  When you are defining
the macro, the C-X Q does nothing, but when the macro is invoked the
C-X Q reads a character from the terminal to decide whether to
continue.  The special answers are Space, Rubout, Altmode, C-L, C-R.
A Space means to continue.  A Rubout means to skip the
remainder of this repetition of the macro, starting again from the
beginning in the next repetition.  An Altmode ends all repetitions of
the macro, but only the innermost macro (in case it was called from
another macro).  C-L clears the screen and asks you again for a
character to say what to do.  C-R enters a recursive editing level;
when you exit, you are asked again (if you type a Space, the macro
will continue from wherever you left things when you exited the C-R).
Anything else exits all levels of keyboard macros and is reread as a
command.
