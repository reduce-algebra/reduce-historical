.so pndoc:nman
.part NM-SCREEN manual
@Chapter[The Organization of the Screen]
@node("screen")
@index{cursor}
@index{screen}
@index{Point}
  NMODE divides the screen into several areas, each of which contains
its own sorts of information.  The biggest area, of course, is the one
in which you usually see the text you are editing.  The terminal's
cursor usually appears in the middle of the text, showing the position
of @dfn[point], the location at which editing takes place.  While the
cursor appears to point @xxii[at] a character, point should be thought
of as @xxii[between] two characters; it points @xxii[before] the
character that the cursor appears on top of.  Terminals have only one
cursor, and when output is in progress it must appear where the
typing is being done.  This does not mean that point is moving.  It is
only that NMODE has no way to show you the location of point except
when the terminal is idle.

@fncindex{nmode-invert-video}
One terminal function which @xxii[is] flexible is the choice of normal
or inverse video for displaying text.
Nmode lets you toggle this feature with the C-X V (@fnc{nmode-invert-video})
command.

@index{echo area}
@index{prompting}
  A few lines at the bottom of the screen compose what is called the
@dfn[echo area].  @dfn[Echoing] means printing out the commands that
you type.
NMODE commands are usually not echoed at all, but if you pause
for more than a second in the middle of a multi-character command then
all the characters typed so far are echoed.  This is intended to
@dfn[prompt] you for the rest of the command.  The rest of the command
is echoed, too, as you type it.  This behavior is designed to give
confident users optimum response, while giving hesitant users
maximum feedback.

  NMODE also uses the echo area for reading and displaying the
arguments for some commands, such as searches, and for printing
brief information in response to certain commands.
@INFO{
  The line above the echo area is known as the @dfn[mode line].  It is the
line that usually starts with "NMODE something".  Its purpose is to
tell what is going on in the NMODE, and to show any reasons why
commands may not be interpreted in the standard way.  The mode line
is very important, and if you are surprised by how NMODE reacts to
your commands you should look there for enlightenment.}
@Section[The Mode Line]
@index{mode line}
@node("modeline")
  The line above the echo area is known as the @dfn[mode line].
It is the line that usually starts with "NMODE something".
Its purpose is to tell you anything that may affect the meaning of
your commands aside from the text itself.
@Example[
NMODE major (minor) [bfr] file --pos-- *
]
@index{major modes}@index{submode}
  @dfn[major] is always the name of the @dfn[major mode] you are in.
At any time, NMODE is in one and only one of its possible major modes.
The major modes available include Text mode,
Lisp mode (which NMODE starts out in),
Recurse mode,
Browser modes, and others.
@Note("MajorModes" "Major Modes"), for details of how the
modes differ and how to select one.

@index{minor modes}
@index{Auto Fill mode}
  @dfn[minor] is a list of some of the @dfn[minor modes] that are
turned on at the moment.  "Fill" means that Auto Fill mode is on.

@index{buffers}
  @dfn[bfr] is the name of the currently selected @dfn[buffer].  Each
buffer has its own name and holds a file being edited; this is how
NMODE can hold several files at once.  But at any time you are editing
only one of them, the @dfn[selected] buffer.  When we speak of what
some command does to "the buffer", we are talking about the currently
selected buffer.  Multiple buffers make it easy to switch around
between several files, and then it is very useful that the mode line
tells you which one you are editing at any time.  However, before you
learn how to use multiple buffers, you will always be in the buffer
called "Main", which is one that exists when NMODE starts up.
If the name of the buffer is the same as the name of the file
you are visiting, then the buffer name is left out of the mode line.
@Note("Buffers"), for how to use more than one buffer in one
NMODE.

@index{files}
  @dfn[file] is the name of the file that you are editing.  It is the
last file that was visited in the buffer you are in.

  The star at the end of the mode line means that there are changes in
the buffer that have not been saved in the file.  If the file has not
been changed since it was read in or saved, there is no star.

  @dfn[pos] tells you whether there is additional text above the top of
the screen, or below the bottom.  If your file is small and it is all
on the screen, --pos-- is omitted.  Otherwise, it is --TOP-- if you
are looking at the beginning of the file, --BOT-- if you are looking
at the end of the file, or --nn%-- where nn is the percentage of the
file above the top of the screen.

  If you are accustomed to other display editors, you may be surprised
that NMODE does not always display the page number and line number of
point in the mode line.  This is because the text is stored in a way
that makes it difficult to compute this information.  Displaying them
all the time would be too slow to be borne.
However, once you are adjusted to NMODE, you will rarely have any
reason to be concerned with page numbers or line numbers.
