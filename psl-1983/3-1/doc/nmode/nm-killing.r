.so pndoc:nman
.part NM-KILLING manual
@chapter(Killing and Moving Text)
  The commonest way of moving or copying text with NMODE is to kill
it, and get it back again in one or more places.  This is very safe
because the last several pieces of killed text are all remembered, and
it is versatile, because the many commands for killing syntactic units
can also be used for moving those units.  There are also other ways of
moving text for special purposes.
@node("killing")
@section(Deletion and Killing)
@keyindex{M-D}
@fncindex{kill-forward-word-command}
@keyindex{M-Backspace}
@fncindex{kill-backward-word-command}
@keyindex{C-M-K}
@fncindex{kill-forward-form-command}
@keyindex{C-M-Rubout}
@fncindex{kill-backward-form-command}
@keyindex{C-X Rubout}
@fncindex{backward-kill-sentence-command}
@keyindex{M-K}
@fncindex{kill-sentence-command}
@keyindex{C-D}
@fncindex{delete-forward-character-command}
@index{Backspace}
@fncindex{delete-backward-hacking-tabs-command}
@keyindex{C-K}
@fncindex{kill-line}
@keyindex{C-W}
@fncindex{kill-region}
@index{killing}
@index{deletion}
@keyindex{C-D}
@index{Backspace}
@keyindex{C-K}
@keyindex{C-W}
@index{lines}
  Most commands which erase text from the buffer save it so that you
can get it back if you change your mind, or move or copy it to other
parts of the buffer.  These commands are known as @dfn[kill] commands.
The rest of the commands that erase text do not save it; they are
known as @dfn[delete] commands.  The delete commands include C-D and
Backspace, which delete only one character at a time, and those commands
that delete only spaces or line separators.  Commands that can destroy
significant amounts of nontrivial data generally kill.  The commands'
names and individual descriptions use the words "kill" and "delete" to
say which they do.
@DoubleWideCommands[
C-D	Delete next character.

Backspace	Delete previous character.

M-\	Delete spaces and tabs around point.

C-X C-O	Delete blank lines around the current line.

M-^	Join two lines by deleting the line separator and any indentation.

C-K	Kill rest of line or one or more lines.

C-W	Kill region (from point to the mark).

M-D	Kill word.

M-Backspace	Kill word backwards.

C-X Rubout	Kill back to beginning of sentence.

M-K	Kill to end of sentence.

C-M-K	Kill Lisp form.

C-M-Rubout	Kill Lisp form backwards.
]
@Subsection[Deletion]
  The most basic delete commands are C-D and Backspace.  C-D deletes the
character after the cursor, the one the cursor is "on top of" or
"underneath".  The cursor doesn't move.  Backspace deletes the character
before the cursor, and moves the cursor back.  Line separators act
like single characters when deleted.  Actually, C-D and Backspace aren't
always delete commands; if you give an argument, they kill instead.
This prevents you from losing a great deal of text by typing a large
argument to a C-D or Backspace.

@keyindex{M-\}
@fncindex{delete-horizontal-space-command}
@Keyindex{C-X C-O}
@fncindex{delete-blank-lines-command}
@keyindex{M-^}
@fncindex{delete-indentation-command}
  The other delete commands are those which delete only formatting
characters: spaces, tabs and line separators.  M-\
(@fnc{delete-horizontal-space-command}) deletes all the spaces and tab
characters before and after point.  C-X C-O
(@fnc{delete-blank-lines-command}) deletes all blank lines after the
current line, and if the current line is blank deletes all blank lines
preceding the current line as well (leaving one blank line, the
current line).  M-^ (@fnc{delete-indentation-command}) joins the
current line and the previous line, or the current line and the next
line if given an argument.
@Note("TextIndent" "Indentation").
@Subsection[Killing by Lines]
@index{blank lines}
  The simplest kill command is the C-K command (@fnc{kill-line}).
If given at the beginning of a line, it kills all the text on the
line, leaving it blank.  If given on a blank line, the blank line
disappears.  As a consequence, if you go to the front of a non-blank
line and type two C-K's, the line disappears completely.

  More generally, C-K kills from point up to the end of the line,
unless it is at the end of a line.  In that case it kills the line
separator following the line, thus merging the next line into the
current one.  Invisible spaces and tabs at the end of the line are
ignored when deciding which case applies, so if point appears to be at
the end of the line, you can be sure the line separator will be
killed.

@index{numeric arguments}
  If C-K is given a positive argument, it kills that many lines, and
the separators that follow them (however, text on the current line
before point is spared).  With a negative argument, it kills back to a
number of line beginnings.  An argument of -2 means kill back to the
second line beginning.  If point is at the beginning of a line, that
line beginning doesn't count, so @w[C-U - 2 C-K] with point at the front
of a line kills the two previous lines.

  C-K with an argument of zero kills all the text before point
on the current line.
@Subsection[Other Kill Commands]
@index{mark}
@index{Region}
@fncindex{kill-region}
  A kill command which is very general is C-W (@fnc{kill-region}), which
kills everything between point and the mark.  With this command, you
can kill any contiguous characters, if you first set the mark at one
end of them and go to the other end.

  Other syntactic units can be killed: words, with M-Backspace and M-D
(@Note("Words").); forms, with C-M-Rubout and C-M-K
(@Note("Lists" "Forms").); sentences, with C-X
Rubout and M-K (@Note("Sentences").).
@Section[Un-Killing]
@node("un-killing")
@index{killing}
@index{moving text}
@index{kill ring}
  Un-killing is getting back text which was killed.  The usual way to
move or copy text is to kill it and then un-kill it one or more times.
@Commands[
C-Y	Yank (re-insert) last killed text.

M-Y	Replace re-inserted killed text with the
previously killed text.

M-W	Save region as last killed text without killing.

C-M-W	Append next kill to last batch of killed text.
]
@keyindex{C-Y}
@fncindex{insert-kill-buffer}
  Killed text is pushed onto a @dfn[ring buffer] called the @dfn[kill
ring] that remembers the last 16 blocks of text that were killed.
(Why it is called a ring buffer will be explained below).  The command
C-Y (@fnc{insert-kill-buffer}) reinserts the text of the most recent
kill.  It leaves the cursor at the end of the text, and puts the mark
at the beginning.  Thus, a single C-W undoes the C-Y.  @w[C-U C-Y]
leaves the cursor in front of the text, and the mark after.  This is
only if the argument is specified with just a C-U, precisely.  Any
other sort of argument, including C-U and digits, has an effect
described below.

@index{mark}
@index{Region}
@keyindex{M-W}
@fncindex{copy-region}
  If you wish to copy a block of text, you might want to use M-W
(@fnc{copy-region}), which copies the region into the kill ring without
removing it from the buffer.  This is approximately equivalent to C-W
followed by C-Y, except that M-W does not mark the buffer as "changed"
and does not temporarily change the screen.

  There is only one kill ring, and switching buffers or files has no
effect on it.  After visiting a new file, whatever was last killed in
the previous file is still on top of the kill ring.  This is important
for moving text between files.
@Subsection[Appending Kills]
@keyindex{C-M-W}
@fncindex{append-next-kill-command}
  Normally, each kill command pushes a new block onto the kill ring.
However, two or more kill commands in a row combine their text into a
single entry on the ring, so that a single C-Y command gets it all
back as it was before it was killed.  This means that you don't have
to kill all the text in one command; you can keep killing line after
line, or word after word, until you have killed it all, and you can
still get it all back at once.  (Thus we join television in
leading people to kill thoughtlessly).

  Commands that kill forward from point add onto the end of the
previous killed text.  Commands that kill backward from point add onto
the beginning.  This way, any sequence of mixed forward and backward
kill commands puts all the killed text into one entry without
rearrangement.

  If a kill command is separated from the last kill command by other
commands, it starts a new entry on the kill ring, unless you tell it
not to by saying C-M-W (@fnc{append-next-kill-command}) in front of
it.  The C-M-W tells the following command, if it is a kill command,
to append the text it kills to the last killed text, instead of
starting a new entry.  With C-M-W, you can kill several separated
pieces of text and accumulate them to be yanked back in one place.
@Subsection[Un-killing Earlier Kills]
@keyindex{M-Y}
@fncindex{unkill-previous}
  To recover killed text that is no longer the most recent kill, you
need the Meta-Y (@fnc{unkill-previous}) command.  The M-Y command should
be used only after a C-Y command or another M-Y.  It takes the
un-killed text inserted by the C-Y and replaces it with the text from
an earlier kill.  So, to recover the text of the next-to-the-last
kill, you first use C-Y to recover the last kill, and then use M-Y to
move back to the previous kill.

  You can think of all the last few kills as living in a ring.  After
a C-Y command, the text at the front of the ring is also present in
the buffer.  M-Y "rotates" the ring, bringing the previous string of
text to the front, and this text replaces the other text in the buffer
as well.  Enough M-Y commands can rotate any part of the ring to the
front, so you can get at any killed text as long as it is recent
enough to be still in the ring.  Eventually the ring rotates all
the way around and the most recent killed text comes to the front
(and into the buffer) again.  M-Y with a negative argument rotates the
ring backwards.  If the region doesn't match the text at the front of
the ring, M-Y is not allowed.

  In any case, when the text you are looking for is brought into the
buffer, you can stop doing M-Y's and it will stay there.  It's really
just a copy of what's at the front of the ring, so editing it does not
change what's in the ring.  And the ring, once rotated, stays rotated,
so that doing another C-Y gets another copy of what you rotated to
the front with M-Y.

  If you change your mind about un-killing, a C-W gets
rid of the un-killed text at any point, after any number of M-Y's.
C-W pushes the text onto the ring again.

@index{numeric arguments}
  If you know how many M-Y's it would take to find the text you want,
then there is an alternative.  C-Y with an argument greater than one
restores the text the specified number of entries down on the ring.
Thus, @w[C-U 2 C-Y] gets the next to the last block of killed text.  It
differs from C-Y M-Y in that @w[C-U 2 C-Y] does not permanently rotate the
ring.
@Section[Other Ways of Copying Text]
@node("copying")
  Usually we copy or move text by killing it and un-killing it, but
there are other ways that are useful for copying one block of text in
many places, or for copying many scattered blocks of text into one
place.

@Subsection[Accumulating Text]
@keyindex{C-X A}
@fncindex{append-to-buffer-command}
@fncindex{insert-buffer-command}
@fncindex{append-to-file-command}
@fncindex{prepend-to-file-command}
  You can accumulate blocks of text from scattered locations either
into a buffer or into a file if you like.

  To append them into a buffer, use the command C-X A
(@fnc{append-to-buffer-command}), which inserts a copy of the region
into the specified buffer at the location of point in that buffer.
This command will prompt for the name of a buffer, which should be
terminated with @Return3{}.
If there is no buffer with the name you specify, one is
created.  If you append text into a buffer which has been used for
editing, the copied text goes into the middle of the text of the
buffer, wherever point happens to be in it.

  Point in that buffer is left at the end of the copied text, so
successive uses of C-X A accumulate the text in the specified buffer
in the same order as they were copied.  If C-X A is given an argument,
point in the other buffer is left before the copied text, so
successive uses of C-X A add text in reverse order.

  You can retrieve the accumulated text from that buffer with M-X
Insert Buffer (@fnc{insert-buffer-command}).  This inserts a copy of
the text in that buffer into the selected buffer.  It prompts for the
buffer name needed.  You can also select the other buffer for editing.
@Note("Buffers"), for background information on buffers.

  Strictly speaking, C-X A does not always append to the text already
in the buffer.  But if it is used on a buffer which starts out empty,
it does keep appending to the end.

  Instead of accumulating text within NMODE, in a buffer, you can
append text directly into a disk file with the command M-X Append to
File (@fnc{append-to-file-command}).  It adds the text of the region
to the end of the specified file.  M-X Prepend to File
(@fnc{prepend-to-file-command}) adds the text to the beginning of the
file instead.  Both commands prompt for the file name.  The file is
changed immediately on disk.  These commands are normally used with
files that are @xxi(not) being visited in NMODE.  They have the
advantage of working even on files too large to fit into the NMODE
address space.
@Subsection[Copying Text Many Times]
@keyindex{C-X X}
@keyindex{C-X G}
@fncindex{put-register-command}
@fncindex{get-register-command}
@index{registers}
@label{NMODEregisters}
@label{NMODE-registers}
  When you want to insert a copy of the same piece of text frequently,
the kill ring becomes impractical, since the text moves down on the
ring as you edit, and will be in an unpredictable place on the ring
when you need it again.  For this case, you can use the commands C-X X
(@fnc{put-register-command}) and C-X G (@fnc{get-register-command}) to
move the text.

  C-X X stores a copy of the text of the region in a place called a
register.  With an argument, C-X X deletes the text as well.  C-X G
inserts the text from a register into the buffer.
Both these commands
prompt for the register name, which must be a single letter or digit.
This gives 36 places in which you can store a piece of text.  Normally
C-X G leaves point before the text and places the mark after, but with
a numeric argument it puts point after the text and the mark before.
