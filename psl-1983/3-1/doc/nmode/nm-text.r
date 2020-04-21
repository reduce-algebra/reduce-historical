.so pndoc:nman
.part NM-TEXT manual
@Chapter[Commands for English Text]
@node("text")
@manual{NMODE enables you to manipulate words, sentences, or
paragraphs of text.  In addition, there are commands to fill text,
and convert case.
}
@fncindex{tab-to-tab-stop-command}
@index{Text mode}
@keyindex{Tab}
@fncindex{text-mode-command}
@index{parentheses}
  Editing files of text in a human language ought to be done using
Text mode.  Invoke M-X Text Mode to enter
Text mode.  @Note("MajorModes" "Major Modes").  M-X Text Mode
(@fnc{text-mode-command})
causes Tab to run the function @fnc{tab-to-tab-stop-command}.
Automatic display of
parenthesis matching is turned off, which is what most people want.
@Section[Word Commands]
@node("words")
@index{words}
@index{Meta}
  NMODE has commands for moving over or operating on words.  By
convention, they are all Meta- characters.
@WideCommands[
M-F	Move Forward over a word.

M-B	Move Backward over a word.

M-D	Kill up to the end of a word.

M-Backspace	Kill back to the beginning of a word.

M-@	Mark the end of the next word.

M-T	Transpose two words;  drag a word forward
or backward across other words.
]
  Notice how these commands form a group that parallels the character
based commands C-F, C-B, C-D, C-T and Backspace.  M-@ is related to C-@.

@index{motion}
@keyindex{M-F}
@keyindex{M-B}
@fncindex{move-forward-word-command}
@fncindex{move-backward-word-command}
  The commands Meta-F (@fnc{move-forward-word-command}) and Meta-B 
(@fnc{move-backward-word-command}) move forward and 
backward over words.  They are thus analogous
to Control-F and Control-B, which move over single characters.  Like
their Control- equivalents, Meta-F and Meta-B move several words if
given an argument.  Meta-F with a negative argument moves backward
like Meta-B, and Meta-B with a negative argument moves forward.
Forward motion stops right after the last letter of the word, while
backward motion stops right before the first letter.

@index{killing}
@keyindex{M-Backspace}
@keyindex{M-D}
@fncindex{kill-forward-word-command}
@fncindex{kill-backward-word-command}
  It is easy to kill a word at a time.  Meta-D
(@fnc{kill-forward-word-command}) kills the word after point.  To be
precise, it kills everything from point to the place Meta-F would move
to.  Thus, if point is in the middle of a word, only the part after
point is killed.  If some punctuation occurs between point and the end
of the next word it will be killed.  If you wish to kill only the next
word but not the punctuation, simply do Meta-F to get the end, and
kill the word backwards with Meta-Backspace.  Meta-D takes arguments
just like Meta-F.

  Meta-Backspace (@fnc{kill-backward-word-command}) kills the word before point.
It kills everything from point back to where Meta-B would move to.  If
point is after the space in @w["FOO, BAR"], then @w["FOO, "] is
killed.  If you wish to kill just "FOO", then do
a Meta-B and a Meta-D instead of a Meta-Backspace.

@index{transposition}
@index{numeric arguments}
@keyindex{M-T}
@fncindex{transpose-words}
  Meta-T (@fnc{transpose-words}) moves the cursor forward over a
word, dragging the word preceding or containing the cursor forward as
well.  A numeric argument serves as a repeat count.  Meta-T with a
negative argument undoes the effect of Meta-T with a positive
argument; it drags the word behind the cursor backward over a word.
An argument of zero, instead of doing nothing, transposes the word at
point (surrounding or adjacent to it) with the word at mark.  In any
case, the delimiter characters between the words do not move.  For
example, @w["FOO, BAR"] transposes into @w["BAR, FOO"] rather than
@w["BAR FOO,"].

@index{mark}
@keyindex{M-@}
@fncindex{mark-word-command}
  To operate on the next n words with an operation which applies
between point and mark, you can either set the mark at point and then
move over the words, or you can use the 
command Meta-@ (@fnc{mark-word-command})
which does not move point, but sets the mark where Meta-F would move
to.  It can be given arguments just like Meta-F.
@Section[Sentence and Paragraph Commands]
@node("sentences")
@index{sentences}
@index{paragraphs}
  The NMODE commands for manipulating sentences and paragraphs are mostly
Meta- commands, so as to resemble the word-handling commands.
@Commands{
M-A		Move back to the beginning of the sentence.

M-E		Move forward to the end of the sentence.

M-K		Kill forward to the end of the sentence.

M-[		Move back to previous paragraph beginning.

M-]		Move forward to next paragraph end.

M-H		Put point and mark around this paragraph
(around the following one, if between paragraphs).

C-X Rubout		Kill back to the beginning of the sentence.
}
@SubSection[Sentences]
@index{motion}
@keyindex{M-A}
@keyindex{M-E}
@fncindex{backward-sentence-command}
@fncindex{forward-sentence-command}
  The commands Meta-A and Meta-E (@fnc{backward-sentence-command} and
@fnc{forward-sentence-command}) move to the beginning and end of the current
sentence, respectively.  They were chosen to resemble Control-A and
Control-E, which move to the beginning and end of a line.  Unlike
them, Meta-A and Meta-E if repeated or given numeric arguments move
over successive sentences.  NMODE considers a sentence to end wherever
there is a ".", "?" or "!" followed by the end of a line or two
spaces, with any number of ")"'s, "]"'s, "'"'s, or '"' 's allowed in
between.  Neither M-A nor M-E moves past the line separator or spaces which
delimit the sentence.

@keyindex{C-A}
@keyindex{C-E}
@keyindex{C-K}
@index{killing}
@keyindex{M-K}
@keyindex{C-X Rubout}
@fncindex{kill-sentence-command}
@fncindex{backward-kill-sentence-command}
  Just as C-A and C-E have a kill command, C-K, to go with them, so
M-A and M-E have a corresponding kill command M-K (@fnc{kill-sentence-command})
which kills from point to the end of the sentence.  With minus one as
an argument it kills back to the beginning of the sentence.  Larger
arguments serve as a repeat count.

  There is a special command, C-X Rubout (@fnc{backward-kill-sentence-command})
for killing back to the beginning of a sentence, because this is
useful when you change your mind in the middle of composing text.
It also accepts arguments, acting as C-U (minus argument given) M-K would.
@SubSection[Paragraphs]
@keyindex{M-[}
@keyindex{M-]}
@fncindex{backward-paragraph-command}
@fncindex{forward-paragraph-command}
  Meta-[ (@fnc{backward-paragraph-command}) moves to the beginning of the
current or previous paragraph, while Meta-] (@fnc{forward-paragraph-command})
moves to the end of the current or next paragraph.  Blank lines and
text justifier command lines (text mode only for these!)
separate paragraphs and are not part of
any paragraph.  Also, an indented line starts a new paragraph. (text mode only!)

@index{Paragraph Delimiter}
A text justifier command line is part of no paragraph in text mode.
A text justifier command line is any line that begins with a period.

@index{blank lines}
  In major modes for programs (as opposed to Text mode), paragraphs
are determined only by blank lines.  This makes the paragraph commands
continue to be useful even though there are no paragraphs per se.

@index{fill-prefix}
  When there is a fill prefix, then paragraphs are delimited by all
lines which don't start with the fill prefix.  @Note("Filling").

@index{Region}
@index{mark}
@keyindex{C-W}
@keyindex{C-U C-@}
@keyindex{M-H}
@fncindex{mark-paragraph-command}
  When you wish to operate on a paragraph, you can use the command
Meta-H (@fnc{mark-paragraph-command}) to set the region around it.  This
command puts point at the beginning and mark at the end of the
paragraph point was in.  Before setting the new mark at the end, a
mark is set at the old location of point; this allows you to undo a
mistaken Meta-H with two C-U C-@'s.  If point is between paragraphs
(in a run of blank lines, or at a boundary), the paragraph following
point is surrounded by point and mark.  Thus, for example, Meta-H C-W
kills the paragraph around or after point.
@Section[Indentation Commands for Text]
@node("textindent")
@index{indentation}
@index{formatting}
@WideCommands[
Tab	Indents "appropriately" in a mode-dependent fashion.

M-Tab	Inserts a tab character.

Linefeed	Is the same as @Return3{} followed by Tab.

M-^	Undoes a Linefeed.  Merges two lines.

M-M	Moves to the line's first nonblank character.

M-I	Indent to tab stop.  In Text mode, Tab does this also.

C-M-\	Indent several lines to same column.

C-X Tab	Shift block of lines rigidly right or left.
]
@keyindex{Tab}
@index{Linefeed}
@fncindex{tab-to-tab-stop-command}
@keyindex{M-Tab}
@keyindex{C-Q}
  The way to request indentation is with the Tab command.  Its precise
effect depends on the major mode.  In Text mode, it runs 
@fnc{tab-to-tab-stop-command}, which inserts a Tab character.
If you are not in Text mode, this function can be found on M-I anyway.
You can also do this with M-Tab or C-Q Tab.

@keyindex{C-M-\}
@fncindex{indent-region-command}
  One also indent a group of lines to a known column by using C-M-\
(@fnc{indent-region-command}).  This must be given a command argument.
It will then indent all the lines in the current region to the
argument-the column.

@index{Auto Fill Mode}
  For English text, usually only the first line of a paragraph should
be indented.  So, in Text mode, new lines created by Auto Fill mode
are not indented.  But sometimes you want to have an indented paragraph.
This can be done by setting fill prefix to the desired indentation.

@keyindex{M-\}
@keyindex{M-^}
@fncindex{delete-horizontal-space-command}
@fncindex{delete-indentation-command}
  To undo a line-break, whether done manually or by Auto Fill, use
Meta-^ (@fnc{delete-indentation-command}) to delete the indentation at
the front of the current line, and the line boundary as well.  They
are replaced by a single space, or by no space if before a ")" or
after a "(", or at the beginning of a line.  To delete just the
indentation of a line, go to the beginning of the line and use Meta-\
(@fnc{delete-horizontal-space-command}), which deletes all
spaces and tabs around the cursor.

  To insert an indented line before the current line, do C-A, C-O, and
then Tab.
To make an indented line after the current line, use C-E Linefeed. 

@keyindex{M-M}
@keyindex{C-M-M}
@fncindex{back-to-indentation-command}
  To move over the indentation on a line, do Meta-M or 
C-M-M (@fnc{back-to-indentation-command}).
These commands, given anywhere on a line,
position the cursor at the first nonblank character on the line.
@index{numeric arguments}
@index{C-M-\}
@index{C-X Tab}
@fncindex{indent region}
@fncindex{indent rigidly}
  There are also commands for changing the indentation of several
lines at once.  Control-Meta-\ (@fnc{indent region}) gives each line
which begins in the region the "usual" indentation by invoking Tab at
the beginning of the line.  A numeric argument specifies the
indentation, and each line is shifted left or right so that it has
exactly that much.  C-X Tab (@fnc{indent
rigidly}) moves all of the lines in the region right by its argument
(left, for negative arguments).  The whole group of lines move rigidly
sideways, which is how the command gets its name.
@Index{Tabify}
@Index{Untabify}
To convert all tabs in a file to spaces, you can use M-X Untabify.
M-X Tabify performs the opposite transformation, replacing spaces with
tabs whenever possible, but only if there are at least three of them
so as not to obscure ends of sentences.  A numeric argument to Tabify
or Untabify specifies the interval between tab stops to use for
computing how to change the file.  By default, they use the same
interval being used for display.  The visual appearance of the text
should never be changed by Tabify or Untabify without a numeric
argument.
@Section[Text Filling]
@node("filling")
@index{filling}
@Commands[
Space	in Auto Fill mode, breaks lines when appropriate.

M-Q	Fill paragraph.

M-G	Fill region (G is for Grind, by analogy with Lisp).

M-S	Center a line.

C-X =	Show current cursor position.
]
@index{Auto Fill Mode}
@keyindex{Space}
  Auto Fill mode lets you type in text that is @dfn[filled] (broken
up into lines that fit in a specified width) as you go.  If you
alter existing text and thus cause it to cease to be properly filled,
NMODE can fill it again if you ask.

@fncindex{auto-fill-mode-command}
  Entering Auto Fill mode is done with M-X Auto Fill
(@fnc{auto-fill-mode-command}).
From then on,
lines are broken automatically at spaces when they get longer than the
desired width. 
To leave Auto Fill mode, execute M-X
Auto Fill again.  When Auto Fill mode is in effect, the word "Fill"
appears in the mode line.

@index{numeric arguments}
  When you finish a paragraph, you can type Space with an argument of
zero.  This doesn't insert any spaces, but it does move the last word
of the paragraph to a new line if it doesn't fit in the old line.
@Return3{} also moves the last word, but it may create another blank line.

@keyindex{M-Q}
@index{paragraphs}
@keyindex{M-G}
@fncindex{fill-region-command}
@fncindex{fill-paragraph-command}
  If you edit the middle of a paragraph, it may no longer be correctly
filled.  To refill a paragraph, use the command Meta-Q 
(@fnc{fill-paragraph-command}).  
It causes the paragraph that point is inside, or the one
after point if point is between paragraphs, to be refilled.  All the
line-breaks are removed, and then new ones are inserted where
necessary.

@keyindex{M-H}
  If you are not happy with Meta-Q's idea of where paragraphs start
and end (the same as Meta-H's.  
@note("Sentences" "Paragraphs").), 
you can use Meta-G (@fnc{fill-region-command}) which
refills everything between point and mark.  Sometimes, it is ok to
fill a region of several paragraphs at once.  Meta-G recognizes a
blank line or (in text mode) an indented 
line as starting a paragraph and does not fill it
in with the preceding line.  The purpose of M-G is to allow you to
override NMODE's usual criteria for paragraph boundaries.  

@index{justification}
  Giving an argument to M-G or M-Q causes the text to be @dfn[justified]
as well as filled.  This means that extra spaces are inserted
between the words so as to make the right margin come out exactly
even.  I do not recommend doing this.  If someone else has uglified
some text by justifying it, you can unjustify it (remove the spaces)
with M-G or M-Q without an argument.

@keyindex{M-S}
@index{centering}
@fncindex{center-line-command}
  The command Meta-S (@fnc{center-line-command}) centers a line within the
current line width.  With an argument, it centers several lines
individually and moves past them.
With a negative argument it centers lines above the current one.

@index{Fill Column}
@keyindex{C-X F}
@fncindex{set-fill-column-command}
  The maximum line width for filling is in the variable Fill-Column.
Both M-Q and Auto Fill make sure that no line exceeds this width.  The
easiest way to set the variable is to use the command C-X F
(@fnc{set-fill-column-command}), 
which places the margin at the column point is on, or at
the column specified by a numeric argument.  The fill column is
initially column 70.

@index{Fill Prefix}
@keyindex{C-X .}
@fncindex{set-fill-prefix-command}
  To fill a paragraph in which each line starts with a special marker
(which might be a few spaces, giving an indented paragraph), use the
@dfn[fill prefix] feature.  Move point to a spot right after the
special marker and give the command @w[C-X Period] 
(@fnc{set-fill-prefix-command}).  Then,
filling the paragraph will remove the marker from each line
beforehand, perform the filling,
and put the marker back in on each line afterward.  Auto
Fill when there is a fill prefix inserts the fill prefix at the
front of each new line.  Also, any line which does not start with the
fill prefix is considered to delimit a paragraph.  To turn off the
fill prefix, do C-X Period with point at the front of a line.
The fill prefix is kept in the variable Fill-Prefix.

@keyindex{C-X =}
@index{echo area}
@fncindex{what-cursor-position-command}
  The command @w[C-X =] (@fnc{what-cursor-position-command})
can be used to find out the
column that the cursor is in, and other miscellaneous information
about point which is quick to compute.  It prints a line in the
echo area that looks like this:
@example[
X=2 Y=19 CH=10 line=428 (74 percent of 574 lines)
]
In this line, the X value is the column the cursor is in (zero at
the left), the Y value is the screen line that the cursor is in (zero
at the top), the CH value is the ascii value of the character after
point and the other values show how large the buffer is and where the
current line is in it.
@Section[Case Conversion Commands]
@node("case")
@index{case conversion}
  NMODE has commands for converting either a single word or any
arbitrary range of text to upper case or to lower case.
@WideCommands[
M-L	Convert following word to lower case.

M-U	Convert following word to upper case.

M-C	Capitalize the following word.

C-X C-L	Convert region to lower case.

C-X C-U	Convert region to upper case.
]
@keyindex{M-L}
@keyindex{M-U}
@keyindex{M-C}
@index{words}
@fncindex{lowercase-word-command}
@fncindex{uppercase-word-command}
@fncindex{uppercase-initial-command}
  The word conversion commands are the most useful.  Meta-L
(@fnc{lowercase-word-command}) converts the word after point to lower case,
moving past it.  Thus, successive Meta-L's convert successive
words.  Meta-U (@fnc{uppercase-word-command}) converts to all capitals instead,
while Meta-C (@fnc{uppercase-initial-command}) puts the first letter of the word
into upper case and the rest into lower case.  All these commands
convert several words at once if given an argument.  They are
especially convenient for converting a large amount of text from all
upper case to mixed case, because you can move through the text
using M-L, M-U or M-C on each word as appropriate.

@index{numeric arguments}
  When given a negative argument, the word case conversion commands
apply to the appropriate number of words before point, but do not move
point.  This is convenient when you have just typed a word in the
wrong case.  You can give the case conversion command and continue
typing.

  If a word case conversion command is given in the middle of a
word, it applies only to the part of the word which follows the
cursor, treating it as a whole word.

@keyindex{C-X C-L}
@keyindex{C-X C-U}
@index{Region}
@fncindex{lowercase-region-command}
@fncindex{uppercase-region-command}
  The other case conversion commands are C-X C-U (@fnc{uppercase-region-command})
and C-X C-L (@fnc{lowercase-region-command}), which convert everything between
point and mark to the specified case.  Point and mark do not move.
