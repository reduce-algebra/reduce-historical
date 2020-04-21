@Comment{This file describes keyboard bindings and useful commands for
EMODE--to be included in other files that need to document them.}

The following commands are notable either for their difference from EMACS,
or for their importance to getting started with EMODE:

@begin[itemize, spread 1]
To leave EMODE type @w[C-X C-Z] to "QUIT" to the EXEC, or @w[C-Z C-Z] to
return to "normal" PSL input/output.

While in EMODE, the "M-?"  (meta- question mark) character asks for a
command character and prints the name of the routine attached to that
character.

The function "PrintAllDispatch()" will print out the current dispatch
table.  You must call EMODE first, to set this table up.

M-C-Y inserts into the current buffer the text printed as a result of the
last M-E.

M-X prompts for a one line string and then executes it as a Lisp
expression.  Of course, similar results can be achieved by using M-E in a
buffer.
@end[itemize]

A (fairly) complete table of keyboard bindings follows:
@begin[description, spread 0]
C-@@@\Runs the function SETMARK.

C-A@\Runs the function !$BEGINNINGOFLINE.

C-B@\Runs the function !$BACKWARDCHARACTER.

C-D@\Runs the function !$DELETEFORWARDCHARACTER.

C-E@\Runs the function !$ENDOFLINE.

C-F@\Runs the function !$FORWARDCHARACTER.

Tab@\In Lisp mode, runs the function LISP-TAB-COMMAND.  Indents as
appropriate for Lisp.

@begin[multiple]
Linefeed@\In text mode, runs the function !$CRLF and acts like a carriage
return.

In Lisp mode, runs the function LISP-LINEFEED-COMMAND.  Inserts a newline
and indents as appropriate for Lisp.
@end[multiple]

C-K@\Runs the function KILL_LINE.

C-L@\Runs the function FULLREFRESH.

Return@\Runs the function $CRLF (inserts a carriage return).

C-N@\Runs the function !$FORWARDLINE.

C-O@\Runs the function OPENLINE.

C-P@\Runs the function !$BACKWARDLINE.

C-Q@\Runs the function INSERTNEXTCHARACTER.  Acts like a "quote" for the
next character typed.

C-R@\Backward search for string, type a carriage return to terminate the
search string.  Default (for a null string) is the last string previously
searched for.

C-S@\Forward search for string.

C-T@\Transpose the last two characters typed (if the last character typed
was self inserting).  Otherwise, transpose the characters to the left and
right of point, or the two characters to the left of point if at the end of
a line.

C-U@\Repeat a command.  Similar to EMACS's C-U.

C-V@\Runs the function SCROLL-WINDOW-UP-PAGE-COMMAND.

C-W@\Runs the function KILL_REGION.

C-X@\As in EMACS, control-X is a prefix for "fancier" commands.

C-Y@\Runs the function INSERT_KILL_BUFFER.  Yanks back killed text.

C-Z@\Runs the function DOCONTROLMETA.  As in EMACS, acts like
"Control-Meta" (or "Meta-Control").

ESCAPE@\Runs the function ESCAPEASMETA.  As in EMACS, ESCAPE acts like the
"Meta" key.

)@\Inserts a "matching" right parenthesis.  Bounces back to the
corresponding left parenthesis, or beeps if no matching parenthesis is
found.

RUBOUT@\Runs the function !$DELETEBACKWARDCHARACTER.

M-C-@@@\Runs the function MARK-SEXP-COMMAND.  Sets mark at the end of the
s-expression following point.

M-C-A@\In Lisp mode, runs the function BEGINNING-OF-DEFUN-COMMAND.  Moves
backward to the beginning of the current or previous) DEFUN.  A DEFUN is
heuristically defined to be a line whose first character is a left
parenthesis.

M-C-B@\Runs the function BACKWARD_SEXPR.

M-C-D@\Runs the function DOWN-LIST.  Moves "deeper" into the next contained
list.

M-C-E@\In Lisp mode, runs the function END-OF-DEFUN-COMMAND.  Moves forward
to the beginning of the next line following the end of a DEFUN.

M-C-F@\Runs the function FORWARD_SEXPR.

M-Backspace@\In Lisp mode, runs the function MARK-DEFUN-COMMAND.

M-Tab@\In Lisp mode, runs the function LISP-TAB-COMMAND.

M-C-K@\Runs the function KILL_FORWARD_SEXPR.

M-Return@\Runs the function BACK-TO-INDENTATION-COMMAND.  Similar to C-A,
but skips past any leading blanks.

M-C-N@\Runs the function MOVE-PAST-NEXT-LIST.  Moves to the right of the
@i[current] or next list.

M-C-O@\Runs the function FORWARD-UP-LIST.  Moves to the right of the
@i[current] list.

M-C-P@\Runs the function MOVE-PAST-PREVIOUS-LIST.  Moves to the beginning
of the current or previous list.

M-C-Q@\Runs the function LISP-INDENT-SEXPR.  "Lisp indents" each line in
the next s-expr.

M-C-U@\Runs the function BACKWARD-UP-LIST.  Does the "opposite" of
FORWARD-UP-LIST.

M-C-Y@\In Lisp and Rlisp mode runs the function INSERT_LAST_EXPRESSION.
Inserts the last body of text typed as the result of a M-E.

M-C-Z@\Runs the function OLDFACE.  Leaves EMODE, goes back to "regular"
PSL input/output.

M-Escape@\In Lisp mode, runs the function BEGINNING-OF-DEFUN-COMMAND.  (See
M-C-A.)

M-C-]@\In Lisp mode, runs the function END-OF-DEFUN-COMMAND.  (See M-C-E.)

M-C-RUBOUT@\Runs the function KILL_BACKWARD_SEXPR.

M-%@\Runs the function QUERY-REPLACE-COMMAND.  Similar to EMACS's query
replace.

M-(@\Runs the function INSERT-PARENS.  Inserts a matching pair of
parenthesis, leaving point between them.

M-)@\Runs the function MOVE-OVER-PAREN.  Moves over a ")" updating
indentation (as appropriate for Lisp).

M-/@\Runs the function !$HELPDISPATCH, see the description of M-? below.

M-;@\In Lisp and Rlisp mode runs the function INSERTCOMMENT.

M-<@\Runs the function !$BEGINNINGOFBUFFER.  Move to beginning of buffer.

M->@\Runs the function !$ENDOFBUFFER.  Move to end of buffer.

M-?@\Runs the function !$HELPDISPATCH.  Asks for a character and prints the
name of the routine attached to that character.

M-@@@\Runs the function MARK-WORD-COMMAND.

M-B@\Runs the function BACKWARD_WORD.  Backs up over a word.

M-D@\Runs the function KILL_FORWARD_WORD.

M-E@\In Lisp and RLISP modes evaluates the expression starting at the
beginning of the current line.

M-F@\Runs the function FORWARD_WORD.  Moves forward over a word.

M-M@\Runs the function BACK-TO-INDENTATION-COMMAND.  (See M-Return for more
description.)

M-V@\Runs the function SCROLL-WINDOW-DOWN-PAGE-COMMAND.  Moves up a window.

M-W@\Runs the function COPY_REGION.  Like C-W only it doesn't kill the
region.

M-X@\Runs the function EXECUTE_COMMAND.  Prompts for a string and then
converts it to Lisp expression and evaluates it.

M-Y@\Runs the function UNKILL_PREVIOUS.  Used to cycle through the kill
buffer.  Deletes the last yanked back text and then proceeds to yank back
the previous piece of text in the kill buffer.

M-\@\Runs the function DELETE-HORIZONTAL-SPACE-COMMAND.  Deletes all blanks
(and tabs) around point.

M-^@\Runs the function DELETE-INDENTATION-COMMAND.  Deletes CRLF and
indentation at front of line, leaves one space in place of them.

M-RUBOUT@\Runs the function KILL_BACKWARD_WORD.

C-X C-B@\Runs the function PRINTBUFFERNAMES.  Prints a list of all the
buffers present.

C-X C-F@\Runs the function FIND_FILE.  Asks for a filename and then selects
the buffer that that file resides in, or creates a new buffer and reads the
file into it.

C-X C-O@\Runs the function DELETE-BLANK-LINES-COMMAND.  Deletes blank lines
around point (leaving one left).

C-X C-P@\Runs the function WRITESCREENPHOTO.  Write a "photograph" of the
screen to a file.

C-X C-R@\Runs the function CNTRLXREAD.  Read a file into the buffer.

C-X C-S@\Runs the function SAVE_FILE.  Writes the buffer to the file
associated with that buffer, asks for an associated file if none defined.

C-X C-W@\Runs the function CNTRLXWRITE.  Write the buffer out to a file.

C-X C-X@\Runs the function EXCHANGEPOINTANDMARK

C-X C-Z@\As in EMACS, exits to the EXEC.

C-X 1@\Goes into one window mode.

C-X 2@\Goes into two window mode.

C-X B@\Runs the function CHOOSEBUFFER.  EMODE asks for a buffer name, and
then selects (or creates) that buffer for editing.

C-X H@\Runs the function MARK-WHOLE-BUFFER-COMMAND.

C-X N@\Runs the function NEXT_WINDOW.  Selects the "next" window in the
list of active windows.  Note that some active windows may be covered by
other screens, so they will be invisible until @w[C-X N] reaches them and
"pops" them to the "top" of the screen.

C-X O@\An alternate way to invoke NEXT_WINDOW.

C-X P@\Runs the function PREVIOUS_WINDOW.  Selects the "previous" window in
the list of active windows.
@end[description]
