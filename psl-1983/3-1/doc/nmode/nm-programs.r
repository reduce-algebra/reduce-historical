.so pndoc:nman
.part NM-PROGRAMS manual
@Chapter(Editing Programs)
@node("programs")
  Special features for editing lisp programs include automatic
indentation, parenthesis matching, and the ability to move over and
kill balanced expressions.

Lisp mode defines paragraphs to be separated only by blank lines and
page boundaries.  This makes the paragraph commands useful for editing
programs.  @Note("Sentences" "Paragraphs").

Moving over words is useful for editing programs as well as text.
@Note("Words").
@Section[Major Modes]
@node("majormodes")
@index{major modes}
@keyindex{Tab}
@keyindex{Rubout}
@keyindex{Linefeed}
@keyindex{backspace}
@index{comments}
@fncindex{indent-new-line-command}
  NMODE has many different @dfn[major modes].  Two such modes are Text
mode and Lisp mode.  Each of these customizes NMODE, one for text, the
other for Lisp programs.  The major modes are mutually exclusive, and
one major mode is current at any time.  When at top level, NMODE
always says in the mode line which major mode you are in.  These modes
tell NMODE to change the meanings of a few commands to become more
specifically adapted to the language being edited.  Most commands
remain unchanged; the ones which usually change are Tab, Backspace, and
Linefeed.  In addition, a few special move and mark commands are turned
on in Lisp mode which are not available in text mode.

@fncindex{text-mode-command}
@fncindex{lisp-mode-command}
  Selecting a new major mode can be done with a M-X command.  For
example M-X Text Mode (@fnc{text-mode-command}) enters text mode and
M-X Lisp Mode (@fnc{lisp-mode-command}) enters lisp mode.  As can be
seen from these examples, some major mode's names are the same as the
invocations of the functions to select those modes.

  Often NMODE enters the correct major mode for a file simply
based on the file's extension, and you do not have to worry about
selecting a mode.

  Lisp mode specifies that only blank lines separate paragraphs.  This
is so that the paragraph commands remain useful.
They also cause Auto
Fill mode to use the definition of Tab to indent the new lines it
creates.  This is because most lines in a program are usually indented.
@Section[Indentation Commands for Code]
@node("indenting")
@WideCommands[
Tab	Indents current line.

Linefeed	Equivalent to @Return3{} followed by Tab.

M-^	Joins two lines, leaving one space between if appropriate.

C-M-O	Split the current line.

M-\	Deletes all spaces and tabs around point.

M-M	Moves to the first nonblank character on the line.
]
@keyindex{Tab}
@index{indentation}
@index{Lisp}
  Most programming languages have some indentation convention.  For
Lisp code, lines are indented according to their nesting in
parentheses.

  Whatever the language, to indent a line, use the Tab command.  Each
major mode defines this command to perform the sort of indentation
appropriate for the particular language.  In Lisp mode, Tab aligns the
line according to its depth in parentheses.  No matter where in the
line you are when you type Tab, it aligns the line as a whole.

@index{Linefeed}
@fncindex{indent-new-line-command}
  The command Linefeed (@fnc{indent-new-line-command}) does a @Return3{} and then
does a Tab on the next line.  Thus, Linefeed at the end of the line
makes a following blank line and supplies it with the usual amount of
indentation.  Linefeed in the middle of a line breaks the line and
supplies the usual indentation in front of the new line.

@keyindex{M-^}
@fncindex{delete-indentation-command}
@keyindex{M-\}
@fncindex{delete-horizontal-space-command} 
  The inverse of Linefeed is Meta-^ or C-M-^
(@fnc{delete-indentation-command}).  This command deletes the
indentation at the front of the current line, and the line separator
as well.  They are replaced by a single space, or by no space if
before a ")" or after a "(", or at the beginning of a line. 
With an argument, M-^ joins the current line and the @xxi[next] line,
removing indentation at the front of the next line beforehand.
To delete
just the indentation of a line, go to the beginning of the line and
use Meta-\ (@fnc{delete-horizontal-space-command}), which deletes all 
spaces and tabs around the cursor.

@keyindex{C-M-O}
@fncindex{split-line-command}
  Another command which affects indentation is 
C-M-O (@fnc{split-line-command}).  It moves the rest of the current line, after
point, down vertically.  It indents the new line so that the rest of the line
winds up in the same column that it was in before the split.  If this command
is given a positive argument, it adds enough empty lines between the old line
and the new line that the total number of lines added equals the argument.
The command leaves point unchanged.

@keyindex{C-A}
@keyindex{C-O}
@keyindex{C-E}
  To insert an indented line before the current one, do C-A, C-O, and
then Tab.
To make an indented line after the current one, use C-E Linefeed. 

@keyindex{M-M}
@keyindex{C-M-M}
  To move over the indentation on a line, use Meta-M or C-M-M
(@fnc{back-to-indentation-command}).  These commands move the cursor forward
or back to the first nonblank character on the line.
@Section[Automatic Display Of Matching Parentheses]
@index{matching}
@index{parentheses}
@node("matching")
@fncindex{insert-closing-bracket}
  The NMODE parenthesis-matching feature is designed to show
automatically how parentheses balance in text as it is typed in.  When
this feature is enabled, after a close parenthesis or other close
bracket character is inserted (using @fnc{insert-closing-bracket})
the cursor automatically moves for an
instant to the open bracket
which balances the newly inserted character.  The
cursor stays at the open parenthesis for a second before returning
home, unless you type another command before the second is up.

  It is worth emphasizing that the location of point, the place where
your type-in will be inserted, is not affected by the parenthesis
matching feature.  It stays after the close parenthesis, where it
ought to be.  Only the cursor on the screen moves away and back.  You
can type ahead freely as if the parenthesis display feature did not
exist.  In fact, if you type fast enough, you won't see the cursor
move.  You must pause after typing a close parenthesis to let the
cursor move to the open parenthesis.

  An additional function is whether NMODE should warn you by ringing
the bell if you type an unmatched close parenthesis.  NMODE will warn
you if you are editing a language in which parentheses are paramount,
such as Lisp, but will not do so for languages in which parentheses
are not so crucial.
@Section[Manipulating Comments]
@index{comments}
@node("comments")
@keyindex{M-;}
@keyindex{M-Z}
@fncindex{insert-comment-command}
@fncindex{fill-comment-command}
@WideCommands[
M-;	Insert comment.

M-Z	Fill a block of comments.
]
  There are two NMODE commands which affect comments.  First there is
M-; (@fnc{insert-comment-command}), which jumps to the end of the
current line and inserts a percent sign and a space, thus starting a
comment.  Second, there is M-Z (@fnc{fill-comment-command}), which
allows filling of blocks of comments.  It fills a paragraph using
whatever text is adjacent to the current line and begins with the same
sequence of blank characters, nonalphanumeric characters, and more
blank characters as the current line.  As a result, it will fill all
lines starting with " % ", for instance.  Notice that it will NOT do
any filling if the current line differs in indentation from the rest
of the paragraph of comments (i.e. if it is an indented first line).
@Section[Lisp Mode]
@node("lisp")
  Lisp's simple syntax makes it much easier for an editor to
understand; as a result, NMODE can do more for Lisp, and with less
work, than for any other language.

@fncindex{lisp-tab-command}
@keyindex{Tab}
@index{Lisp mode}
  Lisp programs should be edited in Lisp mode.  In this mode, Tab is
defined to indent the current line according to the conventions of
Lisp programming style.  It does not matter where in the line Tab is
used; the effect on the line is the same.  The function which does the
work is called @fnc{lisp-tab-command}.  Linefeed, as usual, does a @Return3{} 
and a Tab, so it moves to the next line and indents it.

@index{Backspace}
@fncindex{delete-backward-hacking-tabs-command}
  As in most modes where indentation is likely to vary from line to
line, Backspace (@fnc{delete-backward-hacking-tabs-command} in Lisp
mode) is redefined to treat a tab as if it were the equivalent number
of spaces.  This makes it possible to rub out indentation one position
at a time without worrying whether it is made up of spaces or tabs.

@index{Paragraphs}
@index{syntax table}
@index{comments}
@index{Auto Fill}
@index{blank lines}
  Paragraphs are defined to start only with blank lines so that the
paragraph commands can be useful.  Auto Fill indents the new lines
which it creates.  Comments start with "%".
@SubSection[Moving Over and Killing Lists and forms]
@index{Lists}
@index{forms}
@node("lists")
@DoubleWideCommands[

C-M-F	Move Forward over form.

C-M-B	Move Backward over form.

C-M-K	Kill form forward.

C-M-Rubout	Kill form backward.

C-M-U	Move Up and backward in list structure.

C-M-(	Same as C-M-U.

C-(	Same as C-M-U.

C-M-)	Move up and forward in list structure.

C-)	Same as C-M-).

C-M-D	Move Down and forward in list structure.

C-M-N	Move forward over a list.

C-M-P	Move backward over a list.

C-M-T	Transpose forms.

C-M-@	Put mark after form.

M-(	Put parentheses around next form(s).

M-)	Move past next close parenthesis and re-indent.
]
@index{Control-Meta}
  By convention, NMODE commands that deal with balanced parentheses
are usually Control-Meta- characters.  They tend to be analogous in
function to their Control- and Meta- equivalents.  These commands are
usually thought of as pertaining to Lisp, but can be useful with any
language in which some sort of parentheses exist (including English).
They are, however, only defined in Lisp mode.

@index{motion}
@keyindex{C-M-F}
@keyindex{C-M-B}
@fncindex{move-forward-form-command}
@fncindex{move-backward-form-command}
  To move forward over a form, use C-M-F (@fnc{move-forward-form-command}).
If the first significant character after point is an "(", C-M-F
moves past the matching ")".  If the first character is a ")", C-M-F
just moves past it.  If the character begins an atom, C-M-F moves to
the end of the atom.  C-M-F with an argument
repeats that operation the specified number of times; with a negative
argument, it moves backward instead.

  The command C-M-B (@fnc{move-backward-form-command}) moves backward over a
form;  it is like C-M-F with the argument's sign reversed.  If there
are "'"-like characters in front of the form moved over, they
are moved over as well.  Thus, with point after @w[" 'FOO "], C-M-B
leaves point before the "'", not before the "F".

@index{comments}
  These two commands (and the commands in this section)
know how to handle comments, string literals, and all other token
syntax in (unaltered) PSL.
NMODE makes one restriction: it will not handle string
literals that extend over multiple lines.

@keyindex{C-M-N}
@keyindex{C-M-P}
@fncindex{move-forward-list-command}
@fncindex{move-backward-list-command}
  Two other commands move over lists instead of
forms are often useful.  They are C-M-N
(@fnc{move-forward-list-command}) and C-M-P
(@fnc{move-backward-list-command}).  They act like C-M-F and C-M-B
except that they don't stop on atoms; after moving over an atom, they
move over the next expression, stopping after moving over a list.
With these commands, you can avoid stopping after all of the 
atomic arguments to a function.

@index{killing}
@keyindex{C-M-Rubout}
@keyindex{C-M-K}
@fncindex{kill-backward-form-command}
@fncindex{kill-forward-form-command}
  Killing a form at a time can be done with C-M-K
(@fnc{kill-forward-form-command}) and C-M-Rubout
(@fnc{kill-backward-form-command}) commands.  C-M-K kills the
characters that C-M-F would move over, and C-M-Rubout kills what C-M-B
would move over.

@keyindex{C-M-U}
@keyindex{C-M-(}
@keyindex{C-M-)}
@keyindex{C-M-D}
@fncindex{backward-up-list-command}
@fncindex{forward-up-list-command}
@fncindex{down-list-command}
  C-M-F and C-M-B stay at the same level in parentheses, when that's
possible.  To move @xxii[up] one (or n) levels, use C-M-( or C-M-)
(@fnc{backward-up-list} and @fnc{forward-up-list-command}).
C-M-( moves backward
up past one containing "(".  C-M-) moves forward up past one
containing ")".  Given a positive argument, these commands move up the
specified number of levels of parentheses.  C-M-U is another name for
C-M-(, which is easier to type, especially on non-Meta keyboards.  If
you use that name, it is useful to know that a negative argument makes
the command move up forwards, like C-M-). 
C-M-( and C-M-) are also availible as C-( and C-), respectively,
which are easier to type on the hp9836 keyboard.

  To move @xxii[down] in list structure, use C-M-D
(@fnc{down-list-command}).  It is nearly the same as searching for a
"(".

@index{transposition}
@keyindex{C-M-T}
@fncindex{transpose-forms}
  A somewhat random-sounding command which is nevertheless easy to use
is C-M-T (@fnc{transpose-forms}), which drags the previous
form across the next one.  An argument
serves as a repeat count, and a negative argument drags backwards
(thus canceling out the effect of C-M-T with a positive argument).  An argument
of zero, rather than doing nothing, transposes the forms at the
point and the mark. 

@index{mark}
@keyindex{C-M-@}
@fncindex{mark-form-command}
  To make the region be the next form in the buffer, use
C-M-@ (@fnc{mark-form-command}) which sets mark at the same place that C-M-F
would move to.  C-M-@ takes arguments like C-M-F.  In particular, a
negative argument is useful for putting the mark at the beginning of
the previous form. 

@keyindex{M-(}
@keyindex{M-)}
@fncindex{make-parens-command}
@fncindex{move-over-paren-command}
  The commands M-( (@fnc{make-parens-command})
and M-) (@fnc{move-over-paren-command})
are designed for a style of editing which keeps parentheses balanced at
all times.  M-( inserts a pair of parentheses, either together as in
"()", or, if given an argument, around the next several forms,
and leaves point after the open parenthesis.  Instead of typing
"(FOO)", you can type M-( FOO, which has the same effect except for
leaving the cursor before the close parenthesis.  Then you type M-),
which moves past the close parenthesis, deleting any indentation
preceding it (in this example there is none), and indenting with
Linefeed after it. 
@SubSection[Commands for Manipulating Defuns]
@index{Defuns}
@node("defuns")
@DoubleWideCommands(

C-M-[, C-M-A	Move to beginning of defun.

C-M-], C-M-E	Move to end of defun.

C-M-H	Put region around whole defun.
)
@keyindex{C-M-A}
@fncindex{move-backward-defun-command}
@keyindex{C-M-E}
@fncindex{end-of-defun-command}
@keyindex{C-M-H}
@fncindex{mark-defun-command}
@index{mark}
@index{Region}
@index{motion}
@keyindex{C-M-[}
@keyindex{C-M-]}
  For historical reasons, an expression 
at the top level in the buffer is called a
@dfn[defun], regardless of what function is actually called by the
expression.

  One might imagine that NMODE finds
defuns by moving upward a level of
parentheses until there were no more levels to go up.  This would require
scanning all the way back to the beginning of the file.  To speed up
the operation, NMODE assumes that any "("
in column 0 is the start of a defun.
This heuristic is nearly always right and avoids the costly scan.

  The commands to move to the beginning and end of the current defun
are C-M-[ (@fnc{move-backward-defun-command}) and 
C-M-] (@fnc{end-of-defun-command}).
Alternate names for these two commands are C-M-A for C-M-[ and C-M-E
for C-M-].  The alternate names are easier to type on many non-Meta
keyboards.

  If you wish to operate on the current defun, use C-M-H
(@fnc{mark-defun-command}) which puts point at the beginning and mark
at the end of the current or next defun.
@Section[Lisp Grinding]
@node("grinding")
@index{indentation}
@index{formatting}
@index{grinding}
@keyindex{Tab}
@keyindex{C-M-Tab}

  The best way to keep Lisp code properly indented ("ground") is to
use NMODE to re-indent it when it is changed.  NMODE has commands to
indent properly either a single line, a specified number of lines, or
all of the lines inside a single form.
@WideCommands[
Tab	In Lisp mode, re-indents line according to parenthesis depth.

Linefeed	Equivalent to @Return3{} followed by Tab.

M-^	Join two lines, leaving one space between them if appropriate.

C-M-Q	Re-indent all the lines within one list.
]
@fncindex{lisp-tab-command}
@keyindex{C-M-Tab}
  The basic indentation function is @fnc{lisp-tab-command}, which gives
the current line the correct indentation as determined from the
previous lines' indentation and parenthesis structure.  This function
is placed on Tab in Lisp mode
(Use Meta-Tab or C-Q Tab to insert a tab).  If executed at the
beginning of a line, it leaves point after the indentation; when given
inside the text on the line, it leaves point fixed with respect to the
characters around it.

@index{Linefeed}
@fncindex{indent-new-line-command}
  When entering a large amount of new code, use Linefeed
(@fnc{indent-new-line-command}), which is equivalent to a @Return3{} 
followed by a Tab.  In Lisp mode, a Linefeed creates or moves down
onto a blank line, and then gives it the appropriate indentation.

@keyindex{C-M-^}
@keyindex{M-^}
@fncindex{delete-indentation-command}
  To join two lines together, use the Meta-^ or Control-Meta-^ command
(@fnc{delete-indentation-command}), which is approximately the opposite of
Linefeed.  It deletes any spaces and tabs at the front of the current
line, and then deletes the line separator before the line.  A single
space is then inserted, if NMODE thinks that one is needed there.
Spaces are not needed before a close parenthesis, or after an open parenthesis.

  If you are dissatisfied about where Tab indents the second
and later lines of an form, you can override it.  If you alter
the indentation of one of the lines yourself, then Tab will indent
successive lines of the same list to be underneath it.  This is the
right thing for functions which Tab indents unaesthetically.

@index{numeric arguments}
  When you wish to re-indent code which has been altered or moved to a
different level in the list structure, you have several commands
available.  You can re-indent a specific number of lines by giving the
ordinary indent command (Tab, in Lisp mode) an argument.  This
indents as many lines as you say and moves to the line following them. 
Thus, if you underestimate, you can repeat the process later.

@keyindex{C-M-Q}
@fncindex{lisp-indent-sexpr}
  You can re-indent the contents of a single form by
positioning point before the beginning of it and typing Control-Meta-Q
(@fnc{lisp-indent-sexpr}).  The line the form starts on is not
re-indented;  thus, only the relative indentation with in the
form, and not its position, is changed.  To correct the
position as well, type a Tab before the C-M-Q.

@keyindex{C-M-\}
@index{Region}
@fncindex{lisp-indent-region-command}
  Another way to specify the range to be re-indented is with point and
mark.  The command C-M-\ (@fnc{lisp-indent-region-command}) applies
Tab to every line whose first character is between point and mark.  In
Lisp mode, this does a Lisp indent.

  The standard pattern of indentation is as follows: 
the second line
of the expression is indented under the first argument, 
if that is on
the same line as the beginning of the expression; otherwise, the
second line is indented 
two spaces
more than the entire expression.
Each following line is indented under the previous line whose nesting
depth is the same.
@section[Lisp Language Interface]
  The following section contains many commands starting with "Lisp-".
This prefix is equivalent to C-], but can sometimes be typed using
a soft key.
@subsection[Evaluation]
  NMODE contains a number of facilities to allow the user to use the
underlying LISP language.
In addition to editing and pretty-printing LISP expressions with the
commands in the preceding sections, the user can execute the LISP
expressions in the buffer.
@doublewidecommands(
Lisp-D	Execute the current Defun.

Lisp-E	Execute the form starting on this line.

Lisp-Y	Yanks the last output into current buffer.)
@fncindex{execute-defun-command}
Lisp-D (@fnc{execute-defun-command}) causes the Lisp reader to read
and evaluate the current defun.  If there is no current defun, the
Lisp reader will read a form starting at the current location.  We
arrange for output to be appended to the end of the output buffer.  The mark is
set at the current location in the input buffer, in case user wants to
go back.

@fncindex{execute-form-command}
Lisp-E (@fnc{execute-form-command}) causes the Lisp reader to read and
evaluate a form starting at the beginning of the current line.  We
arrange for output to be appended to the end of the output buffer.
The mark is set at the current location in the input buffer, in case
user wants to go back.

@fncindex{yank-last-output-command}
Lisp-Y (@fnc{yank-last-output-command})
copies the last piece of output from the output buffer back into the
current buffer, allowing it to be added to some code or text within the
current buffer.

@subsection[Debugging]
  The commands of the last subsection allow one to use the underlying
LISP, provided that no errors occur in the evaluation of expressions.
The commands of this subsection allow recovery from errors in
evaluations.  When an error occurs, one enters a "break loop".  This
is indicated by the presence of more than one angle bracket on the
lisp prompt at the right hand side of the mode line under the output
buffer.  When one is in a break loop, one can still evaluate lisp
expressions.  Additional errors at this point will wrap additional
break loops around the current one.  Commands available in break loops
include:
@doublewidecommands(
Lisp-A	Abort break loops.

Lisp-Q	Quit current break loop.

Lisp-B	Backtrace function calls.

Lisp-C	Continue execution.

Lisp-R	Retry expression.

Lisp-?	Help command)
@fncindex{lisp-abort-command}
@keyindex{lisp-A}
  Lisp-A (@fnc{lisp-abort-command})
will pop out of an arbitrarily deep break loop.
@fncindex{lisp-quit-command}
@keyindex{lisp-Q}
Lisp-Q (@fnc{lisp-quit-command})
exits the current break loop. It only pops up one level, unlike abort.

@fncindex{lisp-backtrace-command}
@keyindex{lisp-B}
Lisp-B (@fnc{lisp-backtrace-command})
lists all the function calls on the stack. 
The most recently invoked function is listed first.
It is a good way to
see how the offending expression got generated.
Unfortunately, many internal functions of Lisp and NMODE are shown, so the
list may get somewhat cluttered.

@fncindex{lisp-continue-command}
@keyindex{lisp-C}
Lisp-C (@fnc{lisp-continue-command})
causes the expression last printed to be returned as the value of the
offending expression.  This allows a user to recover from a low level error
in an involved calculation if they know what should have been returned by the
offending expression.  This is also often useful as an automatic stub:
If an expression containing an undefined function is evaluated, a Break loop is
entered, and this may be used to return the value of the function call.

@fncindex{lisp-retry-command}
@keyindex{lisp-R}
Lisp-R (@fnc{lisp-retry-command})
tries to evaluate the offending expression again, and to continue the
computation.  This is often useful after defining a missing function,
or assigning a value to a variable.

@fncindex{lisp-help-command}
@keyindex{lisp-?}
Lisp-? (@fnc{lisp-help-command})
lists the lisp commands available.
When in a break loop it prints:
    "Lisp break commands: Q-quit;A-abort;R-retry;C-continue;B-backtrace"
Otherwise it prints:
    "Lisp commands: E-execute form;Y-yank last output;L-invoke Lisp Listener"

