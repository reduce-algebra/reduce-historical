.so pndoc:nman
.part NM-MARK manual
@Chapter[The Mark and the Region]
@node("mark")
@index{mark}
@index{Region}
@keyindex{C-X C-U}
@fncindex{uppercase-region-command}
  In general, a command which processes an arbitrary part of the
buffer must know where to start and where to stop.  In NMODE, such
commands usually operate on the text between point and @dfn[the mark].
This range of text is called @dfn[the region].  To specify a region,
you set point to one end of it and mark at the other.  It doesn't
matter which one is set first chronologically, or which one comes
earlier in the text.
Here are some commands for setting the mark:
@WideCommands[
C-@	Set the mark where point is.

C-Space	The same.

C-X C-X	Interchange mark and point.

M-@	Set mark after end of next word.  This command and the following
three do not move point.

C-M-@	Set mark after end of next Lisp form.

C-<	Set mark at beginning of buffer.

C->	Set mark at end of buffer.

M-H	Put region around current paragraph.

C-M-H	Put region around current Lisp defun.

C-X H	Put region around entire buffer.
]
  For example, if you wish to convert part of the buffer to all
upper-case, you can use the C-X C-U command, which operates on the
text in the region.  You can first go to the beginning of the text to
be capitalized, put the mark there, move to the end, and then type C-X
C-U.  Or, you can set the mark at the end of the text, move to the
beginning, and then type C-X C-U.  C-X C-U runs the function
@fnc{uppercase-region-command}, whose name signifies that the region, or
everything between point and the mark, is to be capitalized.

@keyindex{C-@}
@keyindex{C-Space}
@fncindex{set-mark-command}
  The most common way to set the mark is with the C-@ command or the
C-Space command (@fnc{set-mark-command}).  They set the mark where
point is.  Then you can move point away, leaving the mark
behind.

  It isn't actually possible to type C-Space on non-Meta keyboards.
Yet on many terminals the command appears to work anyway!  This is
because trying to type a Control-Space on those terminals actually
sends the character C-@, which means the same thing as C-Space.  A
few keyboards just send a Space.  If you have one of them, you type C-@,
or customize your NMODE.

@keyindex{C-X C-X}
@fncindex{exchange-point-and-mark}
  Since terminals have only one cursor, there is no way for NMODE to
show you where the mark is located.  You have to remember.  The usual
solution to this problem is to set the mark and then use it soon,
before you forget where it is.  But you can see where the mark is with
the command C-X C-X (@fnc{exchange-point-and-mark}) which puts the
mark where point was and point where the mark was.  The extent of the
region is unchanged, but the cursor and point are now at the previous
location of the mark.

  C-X C-X is also useful when you are satisfied with the location of
point but want to move the mark; do C-X C-X to put point there
and then you can move it.  A second use of C-X C-X, if necessary,
puts the mark at the new location with point back at its original
location.

  If you insert or delete before the mark, the mark may drift through
the text.  If the buffer contains "FOO BAR" and the mark is before the
"B", then if you delete the "F" the mark will be before the "A".  This
is an unfortunate result of the simple way the mark is implemented.
It is best not to delete or insert at places above the mark until you
are finished using it and don't care where it drifts to.
@Section[Commands to Mark Textual Objects]
@keyindex{M-@}
@keyindex{C-M-@}
@index{words}
@index{lists}
@keyindex{C->}
@keyindex{C-<}
@fncindex{mark-word-command} 
@fncindex{mark-form-command}
@fncindex{mark-beginning-command} 
@fncindex{mark-end-command}
  There are commands for placing the mark on the other side of a
certain object such as a word or a list, without having to move there
first.  M-@ (@fnc{mark-word-command}) puts the mark at the end of the
next word, while C-M-@ (@fnc{mark-form-command}) puts it at the end
of the next s-expression.  C-> (@fnc{mark-end-command}) puts the mark
at the end of the buffer, while C-< (@fnc{mark-beginning-command})
puts it at the beginning.  These characters allow you to save a little
typing or redisplay, sometimes.

@index{paragraphs}
@index{Defuns}
@index{pages}
@keyindex{M-H}
@keyindex{C-M-H}
@keyindex{C-X H}
@fncindex{mark-paragraph-command}
@fncindex{mark-defun-command}
@fncindex{mark-whole-buffer-command}
  Other commands set both point and mark, to delimit an object in the
buffer.  M-H (@fnc{mark-paragraph-command}) puts point at the
beginning of the paragraph it was inside of (or before), and puts the
mark at the end.  M-H does all that's necessary if you wish to
case-convert or kill a whole paragraph.  C-M-H
(@fnc{mark-defun-command}) similarly puts point before and the mark
after the current or next defun.  Finally, C-X H
(@fnc{mark-whole-buffer-command}) makes the region the entire buffer
by putting point at the beginning and the mark at the end.
@Section[The Ring of Marks]
@keyindex{C-U C-@}
@keyindex{C-U C-Space}
  Aside from delimiting the region, the mark is also useful for
remembering a spot that you may want to go back to.  To make this
feature more useful, NMODE remembers 16 previous locations of the mark
for each buffer.
Most commands that set the mark push the old mark onto this stack.  To
return to a marked location, use @w[C-U C-@] (or @w[C-U C-Space]).  This
moves point to where the mark was, and restores the mark from the
stack of former marks.  So repeated use of this command moves
point to all of the old marks on the stack, one by one.  Since the
stack is actually a ring, enough uses of @w[C-U C-@] bring point
back to where it was originally.  Insertion and deletion can cause
the saved marks to drift, but they will still be good for this purpose
because they are unlikely to drift very far.

  Some commands whose primary purpose is to move point a great
distance take advantage of the stack of marks to give you a way to
undo the command.  The best example is M-<, which moves to the
beginning of the buffer.  It sets the mark first, so that you can use
@w[C-U C-@] or @w[C-X C-X] to go back to where you were.
