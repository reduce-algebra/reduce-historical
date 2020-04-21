.so pndoc:nman
.part NM-EDITING manual
@Chapter[Basic Editing Commands]
@node("editing")
@node("basic")
  We now give the basics of how to enter text, make corrections, and
save the text in a file.  If this material is new to you, you might
learn it more easily by running the NTEACH program.
@Section[Inserting Text]
@index{insertion}
@index{point}
@index{cursor}
@index{printing characters}
  To insert printing characters into the text you are editing, just
type them.
When the selected buffer is an editing buffer,
When NMODE is in either Text or Lisp mode,
all printing characters you
type are inserted into the text at the cursor (that is, at
@dfn[point]), and the cursor moves forward.
Any characters after the
cursor move forward too.  If the text in the buffer is FOOBAR, with
the cursor before the B, then if you type XX, you get FOOXXBAR, with
the cursor still before the B.

@index{Backspace}
@index{deletion}
@fncindex{delete-backward-character-command}
  To correct text you have just inserted, you can use Backspace.  Backspace
deletes the character @xxii[before] the cursor (not the one that the cursor
is on top of or under;  that is the character @xxii[after] the cursor).  The
cursor and all characters after it move backwards.  Therefore, if you
type a printing character and then type Backspace, they cancel out.

@index{@Return1{}}
@index{CRLF}
@fncindex{return-command}
@index{line separator}
  To end a line and start typing a new one, type @Return3{} (Customizers,
note: this runs the function @fnc{return-command}).  @Return3{} operates by
inserting a line separator, so if you type @Return3{} in the middle of a
line, you break the line in two.

@index{!}
  If you add too many characters to one line, without breaking it with
a @Return3{}, the line 
will display a "!" at the extreme right margin.
This does not stop you from adding further characters,
but those characters will not be visible until the line is somehow broken,
or until you scroll the window horizontally using C-X >.

@index{Quoting}
@index{Control characters, inserting}
@keyindex{C-Q}
@fncindex{insert-next-character-command}
  Direct insertion works for printing characters and space, but other
characters act as editing commands and do not insert themselves.  If
you need to insert a control character, Altmode, Tab, Backspace
or Rubout, you
must @dfn[quote] it by typing the Control-Q
(@fnc{insert-next-character-command})
command first.  @Note("Characters" "Control").
@Section[Moving The Cursor]
  To do more than insert characters, you have to know how to move the
cursor.  Here are a few of the commands for doing that.

@keyindex{C-A}
@keyindex{C-E}
@keyindex{C-F}
@keyindex{ESC-C}
@keyindex{C-B}
@keyindex{ESC-D}
@keyindex{C-N}
@keyindex{ESC-B}
@keyindex{C-P}
@keyindex{ESC-A}
@keyindex{C-L}
@keyindex{C-T}
@keyindex{M->}
@keyindex{M-<}
@fncindex{move-down-command}
@fncindex{move-to-start-of-line-command}
@fncindex{move-to-end-of-line-command}
@fncindex{move-forward-character-command}
@fncindex{move-backward-character-command}
@fncindex{move-down-extending-command}
@fncindex{move-up-command}
@fncindex{nmode-refresh-command}
@fncindex{transpose-characters-command}
@fncindex{move-to-buffer-start-command}
@fncindex{move-to-buffer-end-command}
@Commands[
C-A	Move to the beginning of the line.

C-E	Move to the end of the line.

C-F	Move forward over one character.

ESC-C	Same as C-F.
	Many terminals have an arrow key pointing right which sends
	this escape sequence.

C-B	Move backward over one character.

ESC-D	Same as C-B.
	Many terminals have an arrow key pointing left which sends
	this escape sequence.

C-N	Move down one line, vertically.  If you start in the
middle of one line, you end in the middle of the next.
From the last line of text, it creates a new line.

ESC-B	Same as C-N except that it will not create a new line.
	Many terminals have an arrow key pointing down which sends
	this escape sequence.

C-P	Move up one line, vertically.

ESC-A	Same as C-P.
	Many terminals have an arrow key pointing up which sends
	this escape sequence.

C-L	Clear the screen and reprints everything.
@w[C-U C-L] reprints just the line that the cursor is on.

C-T	Transpose two characters
(the ones before and after the cursor).

M-<	Move to the top of your text.

M->	Move to the end of your text.
]
@keyindex{C-X C-N}
@fncindex{set-goal-column-command}
  There is a special command: C-X C-N (@fnc{set-goal-column-command}),
which affects how C-P, ESC-A, C-N, and ESC-B act.  Without an argument,
C-X C-N will store the current column so that the vertical movement
commands will try to move into it when they move point up or down,
regardless of the column that point is in prior to the vertical movement.
To remove the goal column, give the C-X C-N command with an argument.

@keyindex{C-X =}
@fncindex{what-cursor-position-command}
  There is a command, C-X = (@fnc{what-cursor-position-command}), which is
normally used to obtain information about where one is in a buffer.
If given an argument, however, it will treat the argument as a line-number and
it will jump to the corresponding line.
@Section[Erasing Text]
@Commands[
Backspace 	Delete the character before the cursor.

C-D 	Delete the character after the cursor.

C-K 	Kill to the end of the line.
]
@Index{Backspace}
@Keyindex{C-D}
@Keyindex{C-K}
  You already know about the Backspace command which deletes the
character before the cursor.  Another command, Control-D, deletes the
character after the cursor, causing the rest of the text on the line
to shift left.  If Control-D is typed at the end of a line, that line
and the next line are joined together.

  To erase a larger amount of text, use the Control-K command, which
kills a line at a time.  If Control-K is done at the beginning or
middle of a line, it kills all the text up to the end of the line.  If
Control-K is done at the end of a line, it joins that line and the
next line.
@Note("Killing"), for more flexible ways of killing text.
@Section[Files]
@index{files}
@keyindex{C-X C-V}
@index{visiting}
@keyindex{C-X C-S}
@fncindex{visit-file-command}
@fncindex{save-file-command}
  The commands above are sufficient for creating text in the NMODE
buffer.  The more advanced NMODE commands just make things easier.
But to keep any text permanently you must put it in a @dfn[file].
Files are the objects which the operating system uses for storing
data for communication between different programs or to hold onto for a
length of time.  To tell NMODE to edit text in a file, choose a
@dfn[filename], such as FOO, and type C-X C-V
FOO@return2{}.  This @dfn[visits] the file FOO
so that its
contents appear on the screen for editing.  You can make changes, and
then @dfn[save] the file by typing C-X C-S.  This makes the changes
permanent and actually changes the file FOO.  Until then,
the changes are only inside your NMODE, and the file FOO
is not really changed.  If the file FOO doesn't exist,
and you want to create it, visit it as if it did exist.  When you save
your text with C-X C-S the file will be created.

  Of course, there is a lot more to learn about using files.
@Note("Files").
@Section[Using Blank Lines Can Make Editing Faster]
@WideCommands[
C-O	Insert one or more blank lines after the cursor.

C-X C-O	Delete all but one of many consecutive blank lines.
]
@keyindex{C-O}
@keyindex{C-X C-O}
@index{blank lines}
@fncindex{open-line-command}
@fncindex{delete-blank-lines-command}
  It is much more efficient to
insert text at the end of a line than in the middle.  So if you want
to stick a new line before an existing one, the best way is to make a
blank line there first and then type the text into it, rather than
inserting the new text at the beginning of the existing line and finally
inserting a line separator.  Making the blank line first also makes
the meaning of the text clearer while you are typing it in.

  To make a blank line, you can type @Return3{} and then C-B.  But there
is a single character for this: C-O (Customizers: this is the function
@fnc{open-line-command})
So, FOO@Return2{} is equivalent to C-O FOO C-F.

  If you want to insert many lines, you can type many C-O's at the
beginning (or you can give C-O an argument to tell it how many blank
lines to make.  @Note("Arguments"), for how).  As you then insert
lines of text, you will notice that @Return3{} behaves strangely: it "uses
up" the blank lines instead of pushing them down.

  If you don't use up all the blank lines, you can type C-X C-O (the
function @fnc{delete-blank-lines-command}) to get rid of all but one.  When
point is on a blank line, C-X C-O replaces all the blank lines around
that one with a single blank line.  When point is on a nonblank line,
C-X C-O deletes any blank lines following that nonblank line.
