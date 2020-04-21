.so pndoc:nman
.part NM-WINDOWS manual
@Chapter[Two Window Mode]
@node("windows")
  NMODE allows you to split the screen into two @dfn[windows] and use
them to display parts of two files, or two parts of the same file.
@WideCommands[
C-X 2	Start showing two windows.

C-X 3	Show two windows but stay "in" the top one.

C-X 1	Show only one window again.

C-X O	Switch to the Other window

C-X E	Exchange Windows

C-X 4	Find buffer or file in other window.

C-X ^	Make this window bigger.

C-M-V	Scroll the other window.
]
@index{windows}
@index{two window mode}
  In @dfn[two window] mode, the text display portion of the screen is
divided into two parts called @dfn[windows], which display different
pieces of text.  The two windows can display two different files, or
two parts of the same file.  Only one of the windows is selected; that
is the window which the cursor is in.  Editing normally takes place in
that window alone.  To edit in the other window, you would give a
special command to move the cursor to the other window, and then edit
there.

@index{OUTPUT}
@keyindex{C-X 2}
@fncindex{two-windows-command}
  The command C-X 2 (@fnc{two-windows-command}) enters two-window
mode.  A second mode line appears across the middle of the screen,
dividing the text display area into two halves.  Window one,
containing the same text as previously occupied the whole screen,
fills the top half, while window two fills the bottom half.  The
cursor moves to window two.  If this is your first entry to two-window
mode, window two contains the output buffer OUTPUT.  Otherwise,
it contains the same text it held the last time you looked at it.  If
given an argument, the same buffer that previously occupied the whole
screen will appear in the lower window as well.

@keyindex{C-X 1}
@fncindex{one-window-command}
  To return to viewing only one window, use the command @w[C-X 1]
(@fnc{one-window-command}).  Window one expands to fill the whole screen, and
window two disappears until the next @w[C-X 2].  @w[C-U C-X 1] gets
rid of window one and makes window two use the whole screen.  Neither
of these depends on which window the cursor is in when the command is
given.

@keyindex{C-X O}
@fncindex{other-window-command}
  While you are in two window mode you can use C-X O
(@fnc{other-window-command}) to switch between the windows.  After
doing C-X 2, the cursor is in window two.  Doing C-X O moves the
cursor back to window one, to exactly where it was before the @w[C-X
2].  The difference between this and doing C-X 1 is that C-X O leaves
window two visible on the screen.  A second C-X O moves the cursor
back into window two, to where it was before the first @w[C-X O].
And so on...

@keyindex{C-X E}
@fncindex{exchange-windows-command}
  While you are in two window mode you can also call C-X E
(@fnc{exchange-windows-command}) , which exchanges the physical
positions of the two windows.  This leaves the cursor in the current
window, and leaves the division of the screen unchanged, but it swaps
the buffers displayed in the two portions of the screen.  As a result
it can change the portion of each buffer that is displayed.

@index{scrolling}
@index{numeric arguments}
@keyindex{C-M-V}
@fncindex{scroll-other-window-command}
  Often you will be editing one window while using the other just for
reference.  Then, the command C-M-V (@fnc{scroll-other-window-command}) is very
useful.  It scrolls the other window without switching to it and
switching back.  It scrolls the same way C-V does:  with no argument, a
whole screen up;  with an argument, that many lines up (or down, for a
negative argument).  With just a minus sign (no digits) as an
argument, C-M-V scrolls a whole screenful backwards (what M-V does).

@keyindex{C-X 3}
@fncindex{view-two-windows-command}
  The C-X 3 (@fnc{view-two-windows-command}) command is like C-X 2 but
leaves the cursor in window one.  That is, it makes window two appear
at the bottom of the screen but leaves the cursor where it was.  C-X 2
is equivalent to C-X 3 @w[C-X O].  C-X 3 is equivalent to C-X 2 C-X
O, but C-X 3 is much faster.

@keyindex{C-X ^}
@fncindex{grow-window-command}
  Normally, the screen is divided evenly between the two windows.  You
can also redistribute screen space between the windows with the @w[C-X
^] (@fnc{grow-window-command}) command.  It makes the currently
selected window get one line bigger, or as many lines as is specified
with a numeric argument.  With a negative argument, it makes the
selected window smaller.  Neither window can be squeezed to less than
one line of visible text by C-X ^.  Overly large arguments squeeze one
window to a line of text, then stop.  The allocation of space to the
windows is remembered while you are in one window mode and the same
allocation is used when you return to two window mode.  The allocation
changes only when you give a @w[C-X ^] command.

  After leaving two-window mode, you can still use C-X O, but its
meaning is different.  Window two does not appear, but whatever was
being shown in it appears, in window one (the whole screen).  Whatever
buffer used to be in window one is stuck, invisibly, into window two.
Another C-X O reverses the effect of the first.  For example, if
window one shows buffer B and window two shows buffer OUTPUT (the
usual case), and only window one is visible, then after a C-X O window
one shows buffer OUTPUT and window two shows buffer B.
@Section[Multiple Windows and Multiple Buffers]
@index{buffers}
  Buffers can be selected independently in each window.  The C-X B
command selects a new buffer in whichever window the cursor is in.
The other window's buffer does not change.  Window two's buffer is
remembered while you are in one window mode, and when you return to
two window mode that same buffer reappears in window two.
@Note("Buffers").

@index{numeric arguments}
  You can view one buffer in both windows.  Give C-X 2 an argument as
in C-U C-X 2 to go into two window mode, with both windows showing the
buffer which used to be in window one alone.  Although the same buffer
appears in both windows, they have different values of point, so you
can move around in window two while window one continues to show the
same text.  Then, having found in window two the place you wish to
refer to, you can go back to window one with C-X O to make your
changes.  Finally you can do C-X 1 to make window two leave the
screen.  If you are already in two window mode, C-U C-X O switches
windows carrying the buffer from the old window to the new one so that
both windows show that buffer.

  If you have the same buffer in both windows, you must
beware of trying to visit a different file in one of the windows
with C-X C-V, because if you bring a new file into this buffer, it
will replace the old file in @xxii[both] windows.  To view different
files in the two windows again, you must switch buffers in one of the
windows first (with C-X B or C-X C-F, perhaps).

@keyindex{C-X 4}
@index{visiting}
@index{buffers}
@index{files}
@fncindex{visit-in-other-window-command}
  A convenient "combination" command for viewing something in the
other window is C-X 4 (@fnc{visit-in-other-window-command}).  With
this command you can ask to see any specified buffer or file in the
other window.  Follow the C-X 4 with either B and a buffer name, F or
C-F and a file name.  This switches to the other window and finds
there what you specified.  If you were previously in one-window mode,
two-window mode is entered.  C-X 4 B is similar to to C-X 2 C-X B.
C-X 4 F is similar to C-X 2 C-X C-F.  The difference is one of
efficiency, and also that C-X 4 works equally well if you are already
using two windows.
