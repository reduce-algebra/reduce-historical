.so pndoc:nman
.part NM-DISPLAY manual
@Chapter[Controlling the Display]
@node("display")
@index{scrolling}
@index{screen}
  Since only part of a large file fits on the screen, NMODE tries to
show the part that is likely to be interesting.  The display control
commands allow you to ask to see a different part of the file.
@Commands[
C-L	Clear and redisplay screen, putting point at a specified vertical position.

ESC-J	Clear and rewrite display, but without moving text or point.

C-V	Scroll forwards (a screen or a few lines).

M-V	Scroll backwards.

M-R	Move point to the text at a given vertical position.

C-M-R	Shift the function point is in onto the screen.

ESC-S	scroll window up line

ESC-T	scroll window down line

ESC-U	scroll window up page

ESC-V	scroll window down page

C-X <	scroll window left

C-X >	scroll window right
]
  The terminal screen is rarely large enough to display all of your
file.  If the whole buffer doesn't fit on the screen, NMODE shows a
contiguous portion of it, containing point.  It continues to
show approximately the same portion until point moves outside of
it; then NMODE chooses a new portion centered around the new
point.  This is NMODE's guess as to what you are most interested in
seeing.  But if the guess is wrong, you can use the display control
commands to see a different portion.  The finite area of screen
through which you can see part of the buffer is called @dfn[the window],
and the choice of where in the buffer to start displaying is also
called @dfn[the window].

@keyindex{C-L}
@index{clear screen}
@fncindex{nmode-refresh-command}
  The basic display control command is C-L
(@fnc{nmode-refresh-command}).  In its simplest form, with no
argument, it clears the screen and tells NMODE to choose a new window
position.  If enough of the buffer is above point, NMODE will pick the
window's position in the file so that point is about two-thirds of the
way down the screen.  If there is not enough of the buffer above point
to fill up two-thirds of the screen, NMODE will pick the window
position so that point is one-third of the way down the screen.  If
there isn't even enough of the buffer above point to fill a third of
the screen, NMODE will put the top of the buffer at the top of the
screen and let point fall where it may.

@keyindex{ESC-J}
@fncindex{nmode-full-refresh}
  Another command that can be used to help clear up the screen is ESC-J
(@fnc{nmode-full-refresh}).  This clears and rewrites the display, but
without changing the portion of the buffer displayed on the screen.

@index{numeric arguments}
  C-L with a positive argument chooses a new window so as to put point
that many lines from the top.  An argument of zero puts point on the
very top line.  Point does not move with respect to the text;
rather, the text and point move rigidly on the screen.  C-L with a
negative argument puts point that many lines from the bottom of the
window.  For example, @w[C-U -1] C-L puts point on the bottom line, and
@w[C-U -5] C-L puts it five lines from the bottom.  C-L with an argument
does not clear the screen, so that it can move the text on the screen
instead of printing it again if the terminal allows that.

@keyindex{C-V}
@keyindex{M-V}
@fncindex{next-screen-command}
@fncindex{previous-screen-command}
@index{Scrolling}
  The @dfn[scrolling] commands C-V and M-V let you move the whole
display up or down a few lines.  C-V (@fnc{next-screen-command}) with an
argument shows you that many more lines at the bottom of the screen,
moving the text and point up together as C-L might.  C-V with a
negative argument shows you more lines at the top of the screen, as
does Meta-V (@fnc{previous-screen-command}) with a positive argument.

@keyindex{ESC-S}
@fncindex{scroll-window-up-line-command}
@keyindex{ESC-T}
@fncindex{scroll-window-down-line-command}
  There are two other commands that let you move the whole display up
or down by a few lines.  These are ESC-S
(@fnc{scroll-window-up-line-command}) and ESC-T
(@fnc{scroll-window-down-line-command}).  These move text and point
together up and down respectively relative to the screen.

  To read the buffer a screenful at a time, use the C-V command with
no argument.  Each C-V shows the "next screenful" of text.  Point is
put at the same point on the screen as on the previous screen.  To
move backward, use M-V without an argument, which moves a whole
screenful backwards.

@keyindex{ESC-U}
@fncindex{scroll-window-up-page-command}
@keyindex{ESC-V}
@fncindex{scroll-window-down-page-command}
  To move by multiple screenfuls in the buffer, ESC-U
(@fnc{scroll-window-up-page-command}) and ESC-V
(@fnc{scroll-window-down-page-command}) can be used.  These functions
accept command arguments and then move the text in the screen up or
down by command-argument pages.  They will reverse direction if given
negative arguments.

@keyindex{C-M-R}
@fncindex{reposition-window-command}
  In Lisp mode,  one can
use the C-M-R command
(@fnc{reposition-window-command})
to scroll the buffer so that the current function (defun) is
positioned conveniently on the screen.   This command tries to get as much
as possible of the current function, preferring the beginning to the
end, but not moving point off the screen.

@keyindex{C-X <}
@fncindex{scroll-window-left-command}
@keyindex{C-X >}
@fncindex{scroll-window-right-command}
  There are also commands to scroll the window horizontally.  C-X <
(@fnc{scroll-window-left-command}) and C-X >
(@fnc{scroll-window-right-command}).  These scroll the portion of the
buffer viewed by the screen to the left or right respectively.  These
commands have the opposite movement conventions from the other
scrolling commands.  In all the other commands, one gets the correct
direction of movement by imagining that it is the characters visible
on the CRT that are moving.  For these commands one should think of
the screen as a movable hole looking at the buffer, and it is the
movement of the hole that is named by the commands.

@keyindex{M-R}
@fncindex{move-to-screen-edge-command}
  C-L in all its forms changes the position of point on the screen,
carrying the text with it.  Another command moves point the same way
but leaves the text fixed.  It is called Meta-R
(@fnc{move-to-screen-edge-command}).
With no argument, it puts point in the line
at the center of the screen, at the current vertical column.
An argument is used to specify the line
to put it on, counting from the top if the argument is positive, or
from the bottom if it is negative.  Thus, Meta-R with an argument of 0
puts point on the top line of the screen.  Meta-R never causes
any text to move on the screen; it causes point to move with respect
to the screen and the text.
