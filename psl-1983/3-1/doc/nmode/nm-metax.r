.so pndoc:nman
.part NM-METAX manual
@Chapter[Extended (Meta-X) Commands and Functions]
@node("m-x")
  Not all NMODE commands are of the one or two character variety you
have seen so far.  Most commands have long invocations composed of English
words.  This is for two reasons: the long invocations are easier to remember
and more suggestive, and there are not enough two-character
combinations for every command to have one.

  The commands with long names are known as @dfn[extended commands]
because they extend the set of two-character commands.
@Section[Issuing Extended Commands]
@DoubleWideCommands[
M-X	Begin an extended command.  Follow by the command invocation only;
the command will ask for any arguments.

C-M-X	Same as M-X.
]
@index{extended commands}
@keyindex{M-X}
@fncindex{m-x-prefix}
@index{functions}
@index{commands}
@fncindex{auto-fill-mode-command}
  Extended commands are also called @dfn[M-X commands], because they
all start with the character Meta-X (@fnc{m-x-prefix}).
The M-X is followed by the command's long, suggestive invocation.
The invocation is terminated with a @Return3{}.  For example, Meta-X
Auto Fill Mode@return2{} invokes @fnc{auto-fill-mode-command}.
This function when executed turns Auto Fill mode on or off.

  There are a great many functions in NMODE for you to call.  They
will be described elsewhere in the manual, according to what they do.
Here we are concerned only with extended commands in general.
@SubSection[Typing The Command Name]
@index{Backspace}
@keyindex{C-D}
@keyindex{C-G}
@index{echo area}
  When you type M-X, the cursor moves down to the echo
area at the bottom of the screen.  "Extended Command:" is printed there, and
when you type the command name it echoes there.  This is known as
@dfn[reading a line in the echo area].  You can use any moving
or deleting command (C-A, C-E, C-F, C-B , C-D, Backspace, etc.)
to help construct the M-X command.
A C-G cancels the whole M-X.  These editing characters apply any
time NMODE reads a line in the echo area, not just within M-X.

@index{prompting}
@index{Read Command Prompt}
  The string "Extended Command:" which appears in the echo area is called a
@dfn[prompt].  The prompt always tells you what sort of argument is
required and what it is going to be used for; "Extended Command:" means that you are
inside of the command M-X, and should type the invocation of a function to be
called.
@SubSection[Completion]
@index{command completion}
@index{return3{}}
@index{Space}
  You can abbreviate the name of the command, typing only the
beginning of the name, as much as is needed to identify the command
unambiguously.  You can also use completion on the function name.
This means that you type part of the command name, and NMODE visibly
fills in the rest, or as much as can be determined from the part you
have typed.

  You request completion by typing @Return3{}.  For example, if you
type @W[M-X Au@Return2{}], the "Au" expands to @W["Auto Fill Mode"] because
"Auto Fill Mode" is the only command invocation that starts with "Au".
If you ask for
completion when there are several alternatives for the next character,
the bell rings and nothing else happens.

  Space is another way to request completion, but it completes only
one word.  Successive Spaces complete one word each, until either
there are multiple possibilities or the end of the name is reached.
If the first word of a command is Edit, List, Kill, View or What, it
is sufficient to type just the first letter and complete it with a Space.
(This does not follow from the usual definition of completion, since
the single letter is ambiguous; it is a special feature added because
these words are so common).
@INFO{
@Note("MMArcana" "MM"), for more information on this and
other topics related to how extended commands work, how they are
really the foundation of everything in NMODE, and how they relate to
customization.}
@Section[Arcane Information about M-X Commands]
@node("mmarcana")
@keyindex{M-X}
  You can skip this section if you are not interested in
customization, unless you want to know what is going on behind the
scenes.

@index{customization}
@index{Connected}
@index{Functions}
  Actually, @xxi[every] command in NMODE simply runs a function.  For
example, when you type the command C-N, it runs the function
"@fnc{move-down-extending-command}".
C-N can be thought of as a sort of
abbreviation.  We say that the command C-N has been @dfn[connected] to the
function @fnc{move-down-extending-command}.
The name is looked up once when the
command and function are connected, so that it does not have to be
looked up again each time the command is used.  The
documentation for individual NMODE commands usually gives the name of
the function which really implements the command in parentheses after
the command itself.

@fncindex{set-key-command}
  Just as any function can be called directly with M-X, so almost any
function can be connected to a command.  
You can use the command M-X Set Key (@fnc{set-key-command}) to do this.
M-X Set Key reads the name of the function from the keyboard, then
reads the character command (including metizers or other prefix
characters) directly from the terminal. 
To define C-N, you could type
@example[
M-X Set Key@Return1{}move-down-extending-command@Return1{}
]
and then type C-N.  If, for instance,
you use the function @fnc({auto-fill-mode-command})
often, you could
connect it to the command C-X Z (not normally defined).  You could
even connect it to the command C-M-V, replacing that command's normal
definition.  Set Key is good for redefining commands in the middle of
editing.  An init file can do it each time you run
NMODE.  @Note("Init").
