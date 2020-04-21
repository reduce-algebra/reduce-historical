.so pndoc:nman
.part NM-TYPOS manual
@Chapter[Commands for Fixing Typos]
@node("fixit")
@index{typos}
  In this section we describe the commands that are especially
useful for the times when you catch a mistake in your text just after
you have made it, or change your mind while composing text on line.
@DoubleWideCommands[
Backspace	Delete last character.

M-Backspace	Kill last word.

C-X Rubout	Kill to beginning of sentence.

C-T	Transpose two characters.

C-X C-T	Transpose two lines.

C-X T	Transpose two arbitrary regions.
]
The next three commands are just M-L, M-U and
M-C with arguments of -1.  The argument could be
entered with M-Minus, C-Minus, or C-U -1.
@DoubleWideCommands[
M-Minus M-L	Convert last word to lower case.

M-Minus M-U	Convert last word to all upper case.

M-Minus M-C	Convert last word to lower case with
capital initial.

M-'	Fix up omitted shift key on digit.
]
@Section[Killing Your Mistakes]
@index{Backspace}
@index{deletion}
@fncindex{delete-backward-character-command}
  The Backspace command is the most important correction command.  When
used among printing (self-inserting) characters, it can be thought of
as canceling the last character typed.

@keyindex{M-Backspace}
@keyindex{C-X Rubout}
@index{words}
@index{sentences}
@index{killing}
@fncindex{kill-backward-word-command}
@fncindex{backward-kill-sentence-command}
  When your mistake is longer than a couple of characters, it might be
more convenient to use M-Backspace (@fnc{kill-backward-word-command})
or C-X Rubout (@fnc{backward-kill-sentence-command}).
M-Backspace kills back to
the start of the last word, and C-X Rubout kills back to the start of
the last sentence.  C-X Rubout is particularly useful when you are
thinking of what to write as you type it, in case you change your mind
about phrasing.   M-Backspace and C-X Rubout save the killed text for C-Y
and M-Y to retrieve (@Note("Un-killing").).

  M-Rubout is often useful even when you have typed only a few
characters wrong, if you know you are confused in your typing and
aren't sure exactly what you typed.  At such a time, you cannot
correct with Rubout except by looking at the screen to see what you
did.  It requires less thought to kill the whole word and start over
again.
@Section[Transposition]
@index{transposition}
@keyindex{C-T}
@fncindex{transpose-characters-command}
  The common error of transposing two characters can be fixed, when
they are adjacent, with the C-T command (@fnc{transpose-characters-command}).
Normally, C-T transposes the
two characters on either side of the cursor.  When given at the end of
a line, rather than transposing the last character of the line with
the line separator, which would be useless, C-T transposes the last
two characters on the line.  So, if you catch your transposition error
right away, you can fix it with just a C-T.  If you don't catch it so
fast, you must move the cursor back to between the two transposed
characters.  If you transposed a space with the last character of the
word before it, the word motion commands are a good way of getting
there.  Otherwise, a reverse search (C-R) is often the best way.
@Note("Search").

@keyindex{C-X C-T}
@fncindex{transpose-lines}
  To transpose two lines, use the C-X C-T command (@fnc{transpose-lines}).
M-T transposes words and C-M-T transposes Lisp forms (in Lisp mode).

@Keyindex{C-X T}
@fncindex{transpose-regions}
  A more general transpose command is C-X T (@fnc{transpose-regions}).
This transposes two arbitrary blocks of text, which need not even
be next to each other.  To use it, set the mark at one end of one of the blocks,
then at the other end of this block; then go to the other block and set
the mark at one end, and put point at the other.  In other words,
point and the last three marks should be at the four locations which
are the ends of the two blocks.  It does not matter which of the four
locations point is at, or which order the others were marked.  C-X T
transposes the two blocks of text thus identified.
, and relocates point
and the three marks without changing their order.
@Section[Case Conversion]
@fncindex{lowercase-word-command}
@fncindex{uppercase-word-command}
@fncindex{uppercase-initial-command}
@keyindex{M-- M-L}
@keyindex{M-- M-U}
@keyindex{M-- M-C}
@keyindex{M-L}
@keyindex{M-U}
@keyindex{M-C}
@index{case conversion}
@index{words}
  A very common error is to type words in the wrong case.  Because of
this, the word case-conversion commands M-L, M-U and M-C have a
special feature when used with a negative argument: they do not move
the cursor.  As soon as you see you have mistyped the last word, you
can simply case-convert it and go on typing.  @Note("Case").

@keyindex{M-'}
@index{typos}
@fncindex{upcase-digit-command}
@index{shifted-digits-association-list}
  Another common error is to type a special character and miss the
shift key, producing a digit instead.  There is a special command for
fixing this: M-' (@fnc{upcase-digit-command}), which fixes the last digit
before point in this way (but only if that digit appears on the
current line or the previous line.  Otherwise, to minimize random
effects of accidental use, M-' does nothing).  Once again, the cursor
does not move, so you can use M-' when you notice the error and
immediately continue typing.  Because M-' needs to know the
arrangement of your keyboard, the first time you use it you must
supply the information by typing the row of digits 1, 2, ... , 9, 0
but @xxii[holding down the shift key].  This tells M-' the
correspondence between digits and special characters, which is
remembered for the duration of the NMODE in 
the variable shifted-digits-association-list.
This command is called M-' because its main use is to replace
"7" with a single-quote.
