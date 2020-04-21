.so pndoc:nman
.part NM-BUGS manual
@Chapter[Correcting Mistakes and NMODE Problems]
If you type an NMODE command you did not intend, the results are often
mysterious.  This chapter tells what you can do to cancel your mistake
or recover from a mysterious situation.  NMODE bugs and system crashes
are also considered.
@Section[Quitting and Aborting]
@node("quitting")
@fncindex{nmode-abort-command}
@keyindex{C-G}
@Commands{
C-G	Quit.  Cancel partially typed command.
}
  There are two ways of cancelling commands which are not finished
executing: @dfn[quitting] with C-G (@fnc{nmode-abort-command}),
and @dfn[aborting] with
C-C on Twenex or STOP on the hp9836.
Quitting is cancelling a partially typed
command.  Aborting is cancelling a command which is already running.
Aborting generally doesn't allow a clean re-entry into the old NMODE
environment so it is generally not recommended.

@index{quitting}@keyindex{C-G}
  Quitting with C-G is used for getting rid of a partially typed
command, or a numeric argument that you don't want.  Quitting an
incremental search does special things documented under searching; in
general, it may take two successive C-G's to get out of a search.
@SubSection[Garbage on the Screen]
  If the data on the screen looks wrong, it could be due to line noise
on input or output, a bug in the terminal, a bug in NMODE redisplay,
or a bug in an NMODE command.  To find out whether there is really
anything wrong with your text, the first thing to do is type C-L.
This is a command to clear the screen and redisplay it.  Often this
will display the text you expected.  Think of it as getting an
opinion from another doctor.
@SubSection[Garbage Displayed Persistently]

@index{terminal type}
@Twenex{@Index[Set Terminal Type]}
@ITS{@index[TCTYP]}
  If NMODE persistently displays garbage on the screen, or if it
outputs the right things but scattered around all the wrong places on
the screen, it may be that NMODE has the wrong idea of your terminal
type.  The first thing to do in this case is to exit from NMODE and
restart it.  Each time NMODE is restarted it asks the system what
terminal type you are using.  Whenever you detach and move to a
terminal of a different type you should restart NMODE as a matter of
course.  If you stopped NMODE with the exit command, or by
interrupting it when it was awaiting a command, then this is sure to
be safe.

  The system itself may not know what type of terminal you have.  You
should try telling the system with the @ITS{:TCTYP
command.}@Twenex{TERMINAL TYPE command in EXEC.  If your terminal is
compatible with one of the standard types but has a different size
screen, you must tell the system the size with the TERMINAL LENGTH and
TERMINAL WIDTH commands, because NMODE uses whatever size the system
says it knows.  Alternatively, you can use Set Terminal Type.
@Note("Term Types" "Terminal Types"), for more information.}

@SubSection[URK Error (Address Space Exhausted)]
@label[NMODEURK]

@Index{Make Space}@INDEX{URK}@Index{Kill Ring}@Index{Undo}
@Index{Kill Libraries}@Index{Kill Some Buffers}
  If attempting to visit a file or load a library causes an "URK"
error, it means you have filled up the address space; there is no room
inside NMODE for any more files or libraries.  In this situation NMODE
will try to run the function Make Space for you.  If NMODE is unable
to do it for you, you may still be able to do M-X Make Space yourself.
This command compacts the data inside NMODE
to free up some space.  It also offers to discard data that may be
occupying a lot of space, such as the kill ring
(@Note("Killing").), the undo memory (@Note("Undo").), and
buffers created by @ITS(RMAIL,) TAGS and INFO.  Another way of freeing
space is to kill buffers with M-X Kill Some Buffers
(@Note("Buffers")@.) or unload libraries with M-X Kill Libraries
(@Note("Libraries").).

@index{What Available Space}
  Use the command M-X What Available Space to find out how close you
are to running out of space.  It tells you how many K of space you
have available for additional files or libraries.
@Section[Reporting Bugs]
@node("bugs")
@index{Bugs}
  Sometimes you will encounter a bug in NMODE.  To get it fixed, you
must report it.  It is your duty to do so; but you must know when to
do so and how if it is to be constructive.
@Subsection[When Is There a Bug]
  If NMODE executes an illegal instruction, or dies with an operating
system error message that indicates a problem in the program (as
opposed to "disk full"), then it probably is a bug.

  We say "probably" because you can also cause these errors yourself if you
execute your own code or modify NMODE by redefining its functions or
changing its variables.

  If NMODE updates the display in a way that does not correspond to
what is in the buffer, then it is probably a bug.  If a command seems
to do the wrong thing but the problem is gone if you type C-L, then it
is a case of incorrect display updating.

  Taking forever to complete a command can be a bug, but you must make
certain that it was really NMODE's fault.  Some commands simply take a
long time.

  If a command you are familiar with causes an NMODE error message in
a case where its usual definition ought to be reasonable, it is
probably a bug.

  If a command does the wrong thing, that is a bug.  But be sure you
know for certain what it ought to have done.  If you aren't
familiar with the command, or don't know for certain how the command
is supposed to work, then it might actually be working right.  Rather
than jumping to conclusions, show the problem to someone who knows for
certain.

  Finally, a command's intended definition may not be best for editing
with.  This is a very important sort of problem, but it is also a
matter of judgment.  Also, it is easy to come to such a conclusion
out of ignorance of some of the existing features.  It is probably
best not to complain about such a problem until you have checked the
documentation in the usual ways, feel confident that
you understand it, and know for certain that what you want is not
available.  If you feel confused about the documentation instead, then
you don't have grounds for an opinion about whether the command's
definition is optimal.  Make sure you read it through and check the
index or the menus for all references to subjects you don't fully
understand.  If you have done this diligently and are still confused,
or if you finally understand but think you could have said it better,
then you have a constructive complaint to make @xxi(about the
documentation).  It is just as important to report documentation bugs
as program bugs.
@Subsection[How to Report a Bug]
  When you decide that there is a bug, it is important to report it
and to report it in a way which is useful.  What is most useful is an
exact description of what commands you type, starting with a fresh
NMODE just loaded, until the problem happens.  Send the bug report to
the author (see the preface for the address).

  The most important principle in reporting a bug is to report @xxii[facts],
not hypotheses or conditions.  It is always easier to report the
facts, but people seem to prefer to strain to think up explanations
and report them instead.  If the explanations are based on guesses
about how NMODE is implemented, they will be useless; we will
have to try to figure out what the facts must have been to lead to
such speculations.  Sometimes this is impossible.  But in any case, it
is unnecessary work for us.

  For example, suppose that you type C-X C-V <GLORP>BAZ.UGH@return1{},
visiting a file which
(you know) happens to be rather large, and NMODE prints out "I
feel pretty today".  The best way to report the bug is with a
sentence like the preceding one, because it gives all the facts
and nothing but the facts.

  Do not assume that the problem is due to the size of the file and
say "When I visit a large file, NMODE prints out 'I feel pretty
today'".  This is what we mean by "guessing explanations".  The
problem is just as likely to be due to the fact that there is a "Z" in
the filename.  If this is so, then when we got your report, we would
try out the problem with some "big file", probably with no "Z" in its
name, and not find anything wrong.  There is no way in the world that
we could guess that we should try visiting a file with a "Z" in its
name.

  Alternatively, the problem might be due to the fact that the file
starts with exactly 25 spaces.  For this reason, you should make sure
that you don't change the file until we have looked at it.  Suppose
the problem only occurs when you have typed the C-X C-A command
previously?  This is why we ask you to give the exact sequence of
characters you typed since loading the NMODE.

  You should not even say "visit the file ..." instead of "C-X C-V"
unless you @xxi[know] that it makes no difference which visiting
command is used.  Similarly, rather than saying "if I have three
characters on the line", say "after I type @return1{}A B
C@return1{}C-P", if that is the way you entered the text.  In
addition, you should say what mode you are in.
@index{FS Flags}@index{minibuffer}
  Be sure to say what version of NMODE and TECO are running.  If you
don't know, type Meta-Altmode QNMODE Version= FS Version=  and
NMODE will print them out.  (This is a use of the minibuffer.
@Note("Minibuffer").)

  If the bug occurred in a customized NMODE, it is helpful to try to
reproduce the bug in a more standard NMODE.  It is best if you can
make the problem happen in a completely standard NMODE.  If the
problem does @xxii[not] occur in a standard NMODE, it is very
important to report that fact, because otherwise we will try to debug
it in a standard NMODE, not find the problem, and give up.  If the
problem does depend on an init file, then you should make sure it is
not a bug in the init file by complaining to the person who wrote the
file, first.  He should check over his code, and verify the
definitions of the PSL commands he is using.  Then if he verifies that
the bug is in NMODE he should report it.  We cannot be responsible for
maintaining users' init files; we might not even be able to tell what
they are supposed to do.

  If you can tell us a way to cause the problem without reading in any
files, please do so.  This makes it much easier to debug.  If you
do need files, make sure you arrange for us to see their exact
contents.  For example, it can often matter whether there are spaces
at the ends of lines, or a line separator after the last line in the
buffer (nothing ought to care whether the last line is terminated, but
tell that to the bugs).
  If NMODE gets an operating system error message, such as for an
illegal instruction, then you can probably recover by restarting it.
But before doing so, you should make a dump file.  If you restart or
continue the NMODE before making the dump, the trail will be covered
and it will probably be too late to find out what happened.
@Twenex{Use the SAVE command to do this; however, this does not record
the contents of the accumulators.  To do that, use the EXEC commands
EXAMINE 0, EXAMINE 1, etc., through EXAMINE 17.  Include the numbers
printed by these commands as part of your bug report.}@ITS{Use the DDT
command
@;@example[
:PDUMP CRASH;NMODE <yourname>
@;]
(or use any other suitable filename) to do this.  Your bug report
should contain the filename you used for the dump, and the error
message printed when the NMODE stopped, as well as the events leading
up to the bug.  The first number in the error message is the PC, which
is not recorded by :PDUMP, so it must be copied precisely.  Also type
.JPC/ and include DDT's response in your report.}

  A dump is also useful if NMODE gets into a wedged state in which
commands that usually work do strange things.

@manual{@include(wordab.mss)@String(Filename="NMODE")}
