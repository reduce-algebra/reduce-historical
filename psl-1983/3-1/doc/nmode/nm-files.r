.so pndoc:nman
.part NM-FILES manual
@Chapter[File Handling]
@node("files")
  The basic unit of stored data is the file.  Each program, each
paper, lives usually in its own file.  To edit a program or paper, the
editor must be told the name of the file that contains it.  This is
called @dfn[visiting] the file.  To make your changes to the file
permanent on disk, you must @dfn[save] the file.  NMODE also has
facilities for deleting files conveniently, and for listing your
file directory.
@Section[Visiting Files]
@node("visiting")
@WideCommands{
C-X C-V	Visit a file.

C-X C-S	Save the visited file.

Meta-~	Tell NMODE to forget that the buffer has been changed.
}
@index{files}
@index{visiting}
@index{saving}
  @dfn[Visiting] a file means copying its contents into NMODE where
you can edit them.  NMODE remembers the name of the file you visited.
Unless you use the multiple buffer or window features of NMODE, you
can only be visiting one file at a time.  The name of the file you are
visiting in the currently selected buffer is visible in the mode line.

  The changes you make with NMODE are made in a copy inside NMODE.
The file itself is not changed.  The changed text is not permanent
until you @dfn[save] it in a file.  The first time you change the
text, a star appears at the end of the mode line; this indicates that
the text contains fresh changes which will be lost unless you save
them.

@keyindex{C-X C-V}
@keyindex{C-G}
@fncindex{visit-file-command}
  To visit a file, use the command C-X C-V (@fnc{visit-file-command}).
Follow
the command with the name of the file you wish to visit, terminated by
a @Return3{}.  
After C-X C-V is entered, @fnc{visit-file-command} will display a prompt.
This prompt may contain a default filename, if so then
any component of the filename which you don't
specify is taken from it.
You can abort the command by typing
C-G, or edit the filename with normal NMODE editing commands.
If you do type
a @Return3{} to finish the command, the new file's text appears on the
screen, and its name appears in the mode line.

@keyindex{C-X C-S}
@fncindex{save-file-command}
  When you wish to save the file and make your changes permanent, type
C-X C-S (@fnc{save-file-command}).  After the save is finished, C-X C-S prints
"Written: <filename>" in the echo area at the bottom of the screen.
If there are no changes
to save (no star at the end of the mode line), the file is not saved;
it would be redundant to save a duplicate of the previous version.

@Index{Create File}
  What if you want to create a file?  Just visit it.  NMODE prints
@w["(New File)"] but aside from that behaves as if you had visited an
existing empty file.  If you make any changes and save them, the file
is created.  If you visit a nonexistent file unintentionally (because
you typed the wrong file name), go ahead and visit the file you meant.
If you don't save the unwanted file, it is not created.

@ITS{
@index{Set Visited Filename}
  When you read a file which is a link, you get the contents of the
target file, but if you save under the name of the link, you break the
link and a new file is created.  The target does not change.  If you
would prefer to alter the target file, use Set Visited Filename to
change the visited name to the target file's name.  @Note("Filadv"
"Set Visited Filename").
}
@index{Visit File Save Old}
  If you alter one file and then visit another in the same buffer,
NMODE offers to save the old one.  If you answer YES, the old file is
saved; if you answer NO, all the changes you have made to it since the
last save are lost.

@fncindex{buffer-not-modified-command}
@keyindex{M-~}
  Sometimes you will change a buffer by accident.  Even if you undo
the change by hand, NMODE still knows that "the buffer has been
changed".  You can tell NMODE to believe that there have been no
changes with the Meta-~ (@fnc{buffer-not-modified-command}) command.  This
command simply clears the "modified" flag which says that the buffer
contains changes which need to be saved.  Even if the buffer really
@xxi(is) changed NMODE will still act as if it were not.  If we take
"~" to mean "not", then Meta-~ is "not", metafied.
@Section[How to Undo Drastic Changes to a File]
@node("revert")
@fncindex{revert-file-command}
@index{files}
@index{Drastic Changes}
  If you have made extensive changes to a file and then change your
mind about them, you can get rid of them by reading in the previous
version of the file.  To do this, use M-X Revert File
(@fnc{revert-file-command}).

  M-X Revert File does not change point, so that if the file was only
edited slightly, you will be at approximately the same piece of text
after the Revert as before.  If you have made drastic changes, the
same value of point in the old file may address a totally different
piece of text.
@Section[Listing a File Directory]
@node("listdir")
@index{file directory}
@keyindex{C-X D}
@fncindex{dired-command}
@keyindex{M-X DIRED}
@fncindex{edit-directory-command}
  To look at a file directory, use the C-X D command
(@fnc{dired-command}).  With no argument, it shows
you the directory of the file you are visiting.  @w[C-U C-X D] reads a
directory specification 
from the keyboard and shows you the files related to that
directory specification.
M-X DIRED (@fnc{edit-directory-command}) differs in that it prompts
for a directory specification even without an argument.
@Section[DIRED, the Directory Editor Subsystem]
@node("dired")
@index{DIRED}
@index{file deletion}
  DIRED makes it easy to delete many of the files in a single
directory at once.  It presents a copy of a listing of the directory,
which you can move around in, marking files for deletion.  When you
are satisfied, you can tell DIRED to go ahead and delete the marked
files.

@index{recursive editing level}
  Invoke DIRED with C-X D or M-X DIRED@Return1{}@Return2{} 
to edit the current default directory,
or M-X DIRED@Return1{}<dir>@Return2{} to edit directory <dir>.  You are then
given a listing of the directory which you can move around in with
all the normal NMODE motion commands.  Some NMODE commands are made
undefined and others do special things, but it's still a recursive
editing level which you can exit normally with Q.
@SubSection[Basic DIRED Commands]
  You can mark a file for deletion by moving to the line describing the
file and typing D.  The deletion mark is
visible as a D at the beginning of the line.  Point is moved to the
beginning of the next line, so that several D's delete several
files.  Alternatively, if you give D an argument it marks that
many consecutive files.  Given a negative argument, it marks the
preceding file (or several files) and puts point at the first (in the
buffer) line marked.  Most of the DIRED commands (D, U, E, Space)
repeat this way with numeric arguments.

  If you wish to remove a deletion mark, use the U (for Undelete)
command, which is invoked like D: it removes the deletion mark
from the current line (or next few lines, if given an argument).  The
Rubout command removes the deletion mark from the previous line,
moving up to that line.  Thus, a Rubout after a D precisely cancels
the D.

  For extra convenience, Space is made a command similar to C-N.
Moving down a line is done so often in DIRED that it deserves to be
easy to type.  Rubout is often useful simply for moving up.

  If you are not sure whether you want to delete a file, you can
examine it by typing E.  This enters a recursive editing mode on the
file, which you can exit with C-M-L.
This also allows you to modify files.
When you exit the
recursive editing level, you return to DIRED.

@index{confirmation}
  When you have marked the files you wish to mark, you can exit DIRED
with Q.  If any files were marked for deletion, DIRED lists them in a
concise format, several per line.  You can type "YES" (Just "Y" won't
do) to go ahead and delete them, "N" to return to editing the
directory so you can change the marks, or "X" to give up and delete
nothing.  No @Return3{} character is needed.  No other inputs are accepted
at this point.
@SubSection[Other DIRED Commands]
  S sorts the files into a different order.  It reads another
character to say which order: F for filename (the default), S for
size, R for read date, or W for write date.

  R does the same sorting as S, but uses the reverse order (small
files, older files or end of alphabet first).

  ? displays documentation on DIRED.
@SubSection[Invoking DIRED]
@keyindex{C-X D}
@index{directory}
@fncindex{dired-command}
  There are some other ways to invoke DIRED.  The command C-X D
(@fnc{dired-command}) puts you in DIRED on the directory containing the file you
are currently editing.  With a numeric argument of 1 (@w[C-U 1] C-X D),
only the current file is displayed instead of the whole directory.
This is present for historical reasons.
On file systems which contain multiple versions of files, such as twenex,
this allows one to see how much space old versions of a file are consuming.
With a
numeric argument of 4 (C-U C-X D), it asks you for the directory name.
Type a directory name and/or a file
name.  If you explicitly specify a file name only versions of that
file are displayed, otherwise the whole directory is displayed.
@Section[Miscellaneous File Operations]
@node("filadv")
@index{insertion}
@index{files}
  NMODE has extended commands for performing many other operations on
files.

@fncindex{write-file-command}
@keyindex{M-X Write File}
@keyindex{C-X C-W}
  M-X Write File@return1{}<file>@return2{} (@fnc{write-file-command})
writes the contents of the buffer into
the file <file>, and then visits that file.  It can be thought of as a
way of "changing the name" of the file you are visiting.  Unlike C-X
C-S, Write File saves even if the buffer has not been changed.  C-X
C-W is another way of getting at this command.

@fncindex{insert-file-command}
@keyindex{M-X Insert File}
  M-X Insert File@return1{}<file>@return2{} (@fnc{insert-file-command})
inserts the contents of <file> into the
buffer at point, leaving point unchanged before the contents and mark
after them.

@index{mark}
@index{Region}
@fncindex{write-region-command}
@keyindex{M-X Write Region}
  M-X Write Region@return1{}<file>@return2{} (@fnc{write-region-command})
writes the region (the text between
point and mark) to the specified file.  It does not set the visited
filename.  The buffer is not changed.

@fncindex{append-to-file-command}
@keyindex{M-X Append to File}
  M-X Append to File@return1{}<file>@return2{} (@fnc{append-to-file-command})
appends the region to <file>.  The text
is added to the end of <file>.

@fncindex{prepend-to-file-command}
@keyindex{M-X Prepend to File}
  M-X Prepend to File@return1{}<file>@return2{} (@fnc{prepend-to-file-command})
adds the text to the beginning of
<file> instead of the end.

@index{Set Visited Filename}
@fncindex{set-visited-filename-command}
  M-X Set Visited Filename@return1{}<file>@return2{} (@fnc{set-visited-filename-command})
changes the name of the file
being visited without reading or writing the data in the buffer.  M-X
Write File is approximately equivalent to this command followed by a
C-X C-S.

@fncindex{delete-file-command}
@index{Delete File}
@keyindex{M-X Delete File}
  M-X Delete File@return1{}<file>@return2{} (@fnc{delete-file-command})
deletes the file.
In twenex this has the effect of putting the file in the directory of
deleted files, from which it can be retrieved until the next expunge.
On the hp9836, this has the effect of irretrievably removing the file.

@fncindex{delete-and-expunge-file-command}
@index{Delete File}
@keyindex{M-X Delete and Expunge File}
  M-X Delete and Expunge File@return1{}<file>@return2{}
(@fnc{delete-and-expunge-file-command}) will, if possible,
irretrievably delete a file.  If the operation fails, a bell will sound.

@fncindex{undelete-file-command}
@keyindex{M-X Undelete File}
  M-X Undelete File@return1{}<file>@return2{} (@fnc{undelete-file-command})
will attempt to retrieve a deleted file.  This only works on Twenex.
