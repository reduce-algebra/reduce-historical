.so pndoc:nman
.part NM-BUFFERS manual
@Chapter[Using Multiple Buffers]
@Node("buffers")
@index{buffers}
  When we speak of "the buffer", which contains the text you are
editing, we have given the impression that there is only one.  In fact,
there may be many of them, each with its own body of text.  At any
time only one buffer can be @dfn[selected] and available for editing,
but it isn't hard to switch to a different one.  Each buffer
individually remembers which file it is visiting, what modes are in
effect, and whether there are any changes that need saving.
@WideCommands{
C-X B	Select or create a buffer.

C-M-L	Select previous buffer.

C-X C-F	Visit a file in its own buffer.

C-X C-B	List the existing buffers.

C-X K	Kill a buffer.
}
@index{mode line}
  Each buffer in NMODE has a single name, which normally doesn't
change.  A buffer's name can be any length.  The name of the currently
selected buffer, and the name of the file visited in it, are visible
in the mode line when you are at top level.  A newly started NMODE has
only one buffer, named "Main".
@Section[Creating and Selecting Buffers]
@keyindex{C-X B}
@fncindex{select-buffer-command}
@index{Select Buffer}
@index{nmode-default-mode}
@index{Major Modes}
  To create a new buffer, you need only think of a name for it (say,
"FOO") and then do C-X B FOO@return2{}, which is the command C-X B (Select
Buffer) followed by the name.  This makes a new, empty buffer and
selects it for editing.  The new buffer is not visiting any
file, so if you try to save it you will be asked for the filename to
use.  Each buffer has its own major mode; the new buffer's major mode
is taken from the value of the variable nmode-default-mode.
Normally nmode-default-mode is text mode.

  To return to buffer FOO later after having switched to another, the
same command C-X B FOO@return2{} is used, since C-X B can tell whether a
buffer named FOO exists already or not.  It does not matter whether
you use upper case or lower case in typing the name of a buffer.  C-X
B Main@return2{} reselects the buffer Main that NMODE started out with.
Just C-X B@return2{} reselects the previous buffer.

@keyindex{C-M-L}
@fncindex{select-previous-buffer-command}
  One can also return to the previous buffer with
C-M-L (@fnc{select-previous-buffer-command}).  This will select the previous
buffer, if possible.  Otherwise, it will select the MAIN buffer.

@keyindex{C-X C-F}
@index{visiting}
@index{Find File}
@fncindex{find-file-command}
  You can also read a file into its own newly created buffer, all with
one command: C-X C-F (@fnc{find-file-command}), followed by the filename.
The name of the file (within its directory)
becomes the buffer name.  C-F stands for "Find",
because if the specified file already resides in a buffer in your
NMODE, that buffer is reselected.  So you need not remember
whether you have brought the file in already or not.  A buffer created
by C-X C-F can be reselected later with C-X B or C-X C-F, whichever
you find more convenient.  Nonexistent files can be created with C-X
C-F just as they can be with C-X C-V.  @Note("Visiting").
@Section[Using Existing Buffers]
@keyindex{C-X C-B}
@fncindex{buffer-browser-command}
@index{List Buffers}
  To get a list of all the buffers that exist, do C-X C-B 
(@fnc{buffer-browser-command}).
Each buffer's name, size, and visited filenames are
printed.  A star at the beginning of a line indicates a buffer
which contains changes that have not been saved.

@index{Save All Files}
@keyindex{C-X C-S}
@fncindex{save-file-command}
@fncindex{save-all-files-command}
  If several buffers have stars, you should save some of them with
M-X Save All Files (@fnc{save-all-files-command}).
This finds all the buffers that need
saving and asks about each one individually.  Saving the buffers this
way is much easier and more efficient than selecting each one and
typing C-X C-S.

@index{Rename Buffer}
@fncindex{rename-buffer-command}
@fncindex{append-to-buffer-command}
@fncindex{insert-buffer-command}
  M-X Rename Buffer@return1{}<new name>@return2{} (@fnc{rename-buffer-command})
changes the name of the currently
selected buffer.  If <new name> is the null string,
a truncated version of the filename
of the visited file is used as the new name of the buffer.

  The commands C-X A (@fnc{append-to-buffer-command}) and M-X Insert
Buffer (@fnc{insert-buffer-command}) can be used to copy text from one
buffer to another.  @Note("Copying").
@Section[Killing Buffers]
@index{Kill Buffer}
@index{Kill Some Buffers}
@keyindex{C-X K}
@index{recursive editing level}
@fncindex{kill-some-buffers-command}
  After you use an NMODE for a while, it may fill up with buffers which
you no longer need.  Eventually you can reach a point where trying to
create any more results in running out of memory space.  So whenever it is
convenient you should do M-X Kill Some Buffers, (@fnc{kill-some-buffers-command})
which asks about each
buffer individually.  You can say Y or N to kill it or not.  Or you
can say Control-R to take a look at it first.  This gives you a recursive
editing level in which you can move around and look at things.  When
you have seen enough to make up your mind, exit the recursive editing
level with a y or n to kill or save the buffer.  If you
say to kill a buffer that needs saving, you will be asked whether it
should be saved.

@fncindex{kill-buffer-command}
  You can kill the buffer FOO by doing C-X K FOO@return2{} 
(@fnc{kill-buffer-command}).
If the buffer being killed has been modified since it was last saved,
NMODE will ask you to confirm your command to kill it.
You can kill
the selected buffer, a common thing to do if you use C-X C-F, by doing
C-X K@return1{}.
If you kill the selected buffer, in any way, NMODE
will move you to another buffer.
