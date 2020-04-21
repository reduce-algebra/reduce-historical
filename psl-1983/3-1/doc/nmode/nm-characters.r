.so pndoc:nman
.part NM-CHARACTERS manual
@Chapter[Character Sets and Command Input Conventions]
@node("characters")
  In this chapter we introduce the terminology and concepts used to
talk about NMODE commands.  NMODE is designed to be used with a kind
of keyboard with two special shift keys which can type 512 different
characters, instead of the 128 different characters which ordinary
ASCII keyboards can send.  The terminology of NMODE commands is
formulated in terms of these shift keys.  So that NMODE can be used on
ASCII terminals, we provide two-character ASCII circumlocutions for
the command characters which are not ASCII.
@Section[The 9-bit Command Character Set]
@index{control}
@index{meta}
@index{character set}
@keyindex{C-}
@keyindex{M-}
@index{ASCII}
  NMODE is designed ideally to be used with terminals whose keyboards
have a pair of shift keys, labeled "Control" and "Meta", either or
both of which can be combined with any character that you can type.
These shift keys produce @dfn[Control] characters and @dfn[Meta]
characters, which are the editing commands of NMODE.  We name each of
these characters by prefixing "Control-" (or "C-"), "Meta-" (or "M-")
or both to the basic character: thus, Meta-F or M-F is the character
which is F typed with the Meta key held down.  C-M-; is the Semicolon
character with both the Control and Meta keys.  Control in the NMODE
command character set is not precisely the same as Control in the
ASCII character set, but the general purpose is the same.

  There are 128 basic characters.  Multiplied by the four possibilities of the
Control and Meta keys, this makes 512 characters in the NMODE command
character set.  So it is called the 512-character set, to distinguish
it from ASCII, which has only 128 characters.  It is also called the
@dfn[9-bit] character set because 9 bits are required to express a number
from 0 to 511.  Note that the 512-character set is used only for
keyboard commands.  Characters in files being edited with NMODE are
ASCII characters.

  Sadly, most terminals do not have ideal NMODE keyboards.  In fact,
the only ideal keyboards are at MIT.  On nonideal keyboards, the Control key is somewhat
limited (it can only be combined with some characters, not with
all), and the Meta key may not exist at all.  We make it possible to
use NMODE on a nonideal terminal by providing two-character
circumlocutions, made up of ASCII characters that you can type, for the
characters that you can't type.  These circumlocutions start with a
@dfn[bit prefix] character; see below.  For example, to use the Meta-A
command, you could type C-\ A.
On the hp9836, the key labelled tab sends C-\ and acts as a meta prefix.

  Both the NMODE 9-bit character set and ASCII have Control
characters, but the 9-bit character set has more different ones.  In
ASCII, only letters and a few punctuation marks can be made into
Control characters; in the 9-bit character set every character has a
Control version.  For example, we have Control-Space, Control-1, and
Control-=.  We also have two different characters Control-A and
Control-a!  But they always do the same thing in NMODE, so you can
ignore the distinction between them, unless you are doing
customization.  In practice, you can forget all about the distinction
between ASCII Control and NMODE Control, except to realize that NMODE
uses some "Control" characters which ASCII keyboards cannot type.

@keyindex{Altmode}
@keyindex{Rubout}
@keyindex{Space}
@index{@return1{}}
  We have given some command characters special names which we always
capitalize.  "@Return1{}" or "@return3{}" 
stands for the carriage return
character, code 015 (all character codes are in octal).  Note that C-R
means the character Control-R, never @Return1{}.  "Rubout" is the
character with code 177, labeled "Delete" on some keyboards.
"Altmode" is the character with code 033, sometimes labeled "Escape".
Other command characters with special names are Tab (code 011),
Backspace (code 010), Linefeed (code 012), Space (code 040), Excl
("!", code 041), Comma (code 054), and Period (code 056).  Control is
represented in the numeric code for a character by 400, and Meta by
200; thus, Meta-Period is code 256 in the 9-bit character set.
@section[Prefix Characters]
@node("prefix")
@index{prefix characters}
@keyIndex{Tab}
@Keyindex{C-^}
@Twenex{@index[C-Z]}
@index{Metizer}
  A non-ideal keyboard can only send certain Control characters, and
may completely lack the ability to send Meta characters.  To use these
commands on such keyboards, you need to use two-character
circumlocutions starting with a @dfn[bit prefix] character which turns on
the Control or Meta bit in the second character.  The C-\
character turns on the Meta bit, so C-\ X can be used to type a
Meta-X, and C-\ Control-O can be used to type a C-M-O.  C-\ is
known as the @dfn[Metizer].  Other bit prefix characters are C-^ for
Control, and @CC[] for Control and Meta together.  Thus, C-^ < is a
way of typing a Control-<, and @CC[] < can be used to type C-M-<.
Because C-^ is awkward to type on most keyboards, we have tried to
minimize the number of commands for which you will need it.

@fncindex{c-x-prefix}
@keyindex{C-X}
@fncindex{m-x-prefix}
@keyindex{M-X}
  There are two other prefix characters, Control-X and Meta-X
which are used as the
beginning of a large set of multi-character commands known as @dfn[C-X
commands] and @dfn[M-X commands].  
C-X is not a bit prefix character; C-X A is not a
circumlocution for any single character, and it must be typed as two
characters on any terminal.  C-X actually runs the function @fnc{c-x-prefix},
while M-X runs @fnc{m-x-prefix}.
@keyindex{C-]}
@fncindex{lisp-prefix}
@keyindex{ESC}
@fncindex{esc-prefix}
Two prefixes which are also used are ESC (@fnc{esc-prefix}) and C-]
(@fnc{lisp-prefix}) (also called Lisp-).  Each of these is used with a
small set of single character suffixes.
You can create new prefix characters when
you customize.
@section[Commands, Functions, and Variables]
@index{Functions}
@index{Connected}
@index{Customization}
  Most of the NMODE commands documented herein are members of this
9-bit character set.  Others are pairs of characters from that set.
However, NMODE doesn't really implement commands directly.  Instead,
NMODE is composed of @dfn[functions], which have long names such as
@fnc{move-down-extending-command} and which are 
programs that perform the editing
operations.  @dfn[Commands] such as C-N are connected to
functions through the @dfn[command dispatch table].
When we say that C-N moves the cursor
down a line, we are glossing over a distinction which is unimportant
for ordinary use, but essential for customization: it is the function
@fnc{move-down-extending-command}
which knows how to move down a line, and C-N moves
down a line @xxi[because] it is connected to that function.  We
usually ignore this subtlety to keep things simple.  To give the
extension-writer the information he needs, we state the name of the
function which really does the work in parentheses after mentioning
the command name.  For example:
"C-N (@fnc{move-down-extending-command})
moves the
cursor down a line".  In the NMODE wall chart, the function names are
used as a form of very brief documentation for the command characters.
@Note("MMArcana" "Functions").

@index{Variables}
  While we are on the subject of customization information which you
should not be frightened of, it's a good time to tell you about
@dfn[variables].  Often the description of a command will say "to
change this, set the variable Mumble Foo".  A variable is a name used
to remember a value.  NMODE contains many variables which are there so
that you can change them if you want to customize.  The variable's
value is examined by some command, and changing the value makes the
command behave differently.  Until you are interested in customizing,
you can ignore this information.  When you are ready to be interested,
read the basic information on variables, and then the information on
individual variables will make sense.  @Note("Variables").
@section[Notational Conventions for ASCII Characters]
@index{ASCII}
@index{control}
@index{uparrow}
@index{caret}
@index{^}
  Control characters in files, your NMODE buffer, or PSL programs,
are ordinary ASCII characters.  The special 9-bit character set
applies only to typing NMODE commands.  ASCII contains the printing
characters, rubout, and some control characters.  Most ASCII control
characters are represented in this manual as uparrow or caret followed
by the corresponding non-control character: control-E is represented
as @CTL[E].

@keyindex{tab}
@keyindex{backspace}
@keyindex{linefeed}
@index{@return1{}}
@keyindex{altmode}
@keyindex{space}
@keyindex{rubout}
  Some ASCII characters have special names.  These include tab (011),
backspace (010), linefeed (012), @return3{} (015), altmode (033), space
(040), and rubout (177).  To make it clear whether we are talking
about a 9-bit character or an ASCII character, we capitalize names of
9-bit characters and leave names of ASCII characters in lower case.
Note that the 9-bit characters Tab and Control-I are different, but
the ASCII characters tab and control-I are the same.

@index{CRLF}
@index{@Return1{}, stray}
@index{Linefeed, stray}
@index{line separator}
  On the Dec-20
lines in files are separated by a sequence of two ASCII control
characters, carriage return followed by linefeed.  This sequence is
called @dfn[CRLF].
On the hp9836 lines in files are separated by other means.
Normally, NMODE treats this two-character sequence
as if it were a single character, a @dfn[line separator], linefeed.
A @return3{} 
which is not part of a CRLF is called @dfn[stray].  NMODE
usually treats them as part of the text of a line and displays them as
^Ms.

@index{Backspace}
@index{Control characters, display of}
  Most control characters when present in the NMODE buffer are
displayed with a caret; thus, ^A for ASCII @CTL[A].  Rubout is displayed as
^?, because by stretching the meaning of "control" it can be
interpreted as ASCII control-?.  A backspace is usually displayed as
^H since it is ASCII control-H, because most displays cannot do
overprinting.


