@fnc(append-next-kill-command)
@cmd(Append Next Kill)
@key(C-M-W)
@seeglobal(Kill Ring)
@acttype(Move Data)
@cmddoc
Make following kill commands append to last batch.  Thus, C-K C-K,
cursor motion, this command, and C-K C-K, generate one block of killed
stuff, containing two lines.
@end

@fnc(append-to-buffer-command)
@cmd(Append To Buffer)
@key(C-X A)
@topic(Buffers)
@seedef(Region)
@acttype(Move Data)
@cmddoc
Append region to specified buffer.  The buffer's name is read from
the keyboard; the buffer is created if nonexistent.
A numeric argument causes us
to "prepend" instead.  We always insert the text at that buffer's
pointer, but when "prepending" we leave the pointer before the
inserted text.
@end

@fnc(append-to-file-command)
@cmd(Append To File)
@key(M-X Append To File)
@topic(Files)
@seedef(Region)
@acttype(Move Data)
@cmddoc
Append region to end of specified file.
@end

@fnc(apropos-command)
@cmd(Apropos)
@key(M-X Apropos)
@key(Esc-_)
@acttype(Inform)
@cmddoc
M-X Apropos lists functions with names containing a string for which the
user is prompted.  The functions are displayed using a documentation
browser, which allows the user to view additional information on each
function or further filter the list of displayed functions by matching
on addtional strings.
@end

@fnc(argument-digit)
@cmd(Argument Digit)
@key(C-0)
@key(C-1)
@key(C-2)
@key(C-3)
@key(C-4)
@key(C-5)
@key(C-6)
@key(C-7)
@key(C-8)
@key(C-9)
@key(C-M-0)
@key(C-M-1)
@key(C-M-2)
@key(C-M-3)
@key(C-M-4)
@key(C-M-5)
@key(C-M-6)
@key(C-M-7)
@key(C-M-8)
@key(C-M-9)
@key(M-0)
@key(M-1)
@key(M-2)
@key(M-3)
@key(M-4)
@key(M-5)
@key(M-6)
@key(M-7)
@key(M-8)
@key(M-9)
@acttype(Subsequent Command Modifier)
@cmddoc
Specify numeric argument for next command.  Several such digits typed
in a row all accumulate.
@end

@fnc(auto-fill-mode-command)
@cmd(Auto Fill Mode)
@key(M-X Auto Fill Mode)
@acttype(Change Mode)
@seecmd(Set Fill Column)
@cmddoc
Break lines between words at the right margin.  A positive argument
turns Auto Fill mode on; zero or negative, turns it off.  With no
argument, the mode is toggled.  When Auto Fill mode is on, lines are
broken at spaces to fit the right margin (position controlled by Fill
Column).
You can set the Fill Column with the Set Fill Column command.
@end

@fnc(back-to-indentation-command)
@cmd(Back To Indentation)
@key(C-M-M)
@key(C-M-RETURN)
@key(M-M)
@key(M-RETURN)
@acttype(Move Point)
@cmddoc
Move to end of this line's indentation.
@end

@fnc(backward-kill-sentence-command)
@cmd(Backward Kill Sentence)
@key(C-X RUBOUT)
@seeglobal(Kill Ring)
@seedef(Sentence)
@acttype(Remove)
@cmddoc
Kill back to beginning of sentence.
With a command argument n kills backward (n>0) or
forward (n>0) by |n| sentences.
@end

@fnc(backward-paragraph-command)
@cmd(Backward Paragraph)
@key(M-[)
@seedef(Paragraph)
@acttype(Move Point)
@cmddoc
Move backward to start of paragraph.
When given argument moves backward (n>0) or forward (n<0) by |n| paragraphs
where n is the command argument.
@end

@fnc(backward-sentence-command)
@cmd(Backward Sentence)
@key(M-A)
@seedef(Sentence)
@acttype(Move Point)
@cmddoc
Move to beginning of sentence.
When given argument moves backward (n>0) or forward (n<0) by |n| sentences
where n is the command argument.
@end

@fnc(backward-up-list-command)
@cmd(Backward Up List)
@key[C-(]
@key[C-M-(]
@mode(Lisp)
@key(C-M-U)
@acttype(Move Point)
@topic(Lisp)
@cmddoc
Move up one level of list structure, backward.
Given a command argument n move up |n| levels backward (n>0) or forward (n<0).
@end

@fnc(buffer-browser-command)
@cmd(Buffer Browser)
@key(C-X C-B)
@key(M-X List Buffers)
@topic(Buffers)
@acttype(Inform)
@cmddoc
Put up a buffer browser subsystem. If an argument is given,
then include
buffers whose names begin with "+".
@end

@fnc(buffer-not-modified-command)
@cmd(Buffer Not Modified)
@key(M-~)
@topic(Buffers)
@acttype(Set Global Variable)
@cmddoc
Pretend that this buffer hasn't been altered.
@end

@fnc(c-x-prefix)
@cmd(C-X Prefix)
@key(C-X)
@acttype(Subsequent Command Modifier)
@cmddoc
The command Control-X is an escape-prefix for more commands.
It reads a character (subcommand) and dispatches on it.
@end

@fnc(center-line-command)
@cmd(Center Line)
@key(M-S)
@topic(Text)
@seeglobal(Fill Column)
@acttype(Alter Existing Text)
@cmddoc
Center this line's text within the line.
With argument, centers that many lines and moves past.
Centers current and preceding lines with negative argument.
The width is Fill Column.
@end

@fnc(copy-region)
@cmd(Copy Region)
@key(M-W)
@acttype(Preserve)
@seeglobal(Kill Ring)
@seedef(Region)
@cmddoc
Stick region into kill-ring without killing it.
Like killing and getting back, but doesn't mark buffer modified.
@end

@fnc(count-occurrences-command)
@cmd(Count Occurrences)
@key(M-X Count Occurrences)
@key(M-X How Many)
@acttype(Inform)
@cmddoc
Counts occurrences of a string, after point.
The user is prompted for the string.
Case is ignored in the count.
@end

@fnc(delete-and-expunge-file-command)
@cmd(Delete And Expunge File)
@key(M-X Delete And Expunge File)
@acttype(Remove)
@topic(Files)
@cmddoc
This command prompts the user for the name of the file. NMODE will fill in
defaults in a partly specified filename (eg filetype can be defaulted).
If possible, the file will then be deleted and expunged, and a message
to that effect will be displayed. If the operation fails, the bell will sound.
@end

@fnc(delete-backward-character-command)
@cmd(Delete Backward Character)
@key(BACKSPACE)
@key(RUBOUT)
@mode(Text)
@acttype(Remove)
@cmddoc
Delete character before point.
With positive arguments this operation is performed multiple times on the
text before point.
With negative arguments this operation is performed multiple times on
the text after point.
@end

@fnc(delete-backward-hacking-tabs-command)
@cmd(Delete Backward Hacking Tabs)
@key(BACKSPACE)
@key(C-RUBOUT)
@mode(Lisp)
@key(RUBOUT)
@acttype(Remove)
@cmddoc
Delete character before point, turning tabs into spaces.
Rather than deleting a whole tab, the tab is converted into the appropriate
number of spaces and then one space is deleted.
With positive arguments this operation is performed multiple times on the
text before point.
With negative arguments this operation is performed multiple times on
the text after point.
@end

@fnc(delete-blank-lines-command)
@cmd(Delete Blank Lines)
@key(C-X C-O)
@acttype(Remove)
@cmddoc
Delete all blank lines around this line's end.
If done on a non-blank line, deletes all spaces and tabs
at the end of it, and all following blank lines
(Lines are blank if they contain only spaces and tabs).
If done on a blank line, deletes all preceding blank lines as well.
@end

@fnc(delete-file-command)
@cmd(Delete File)
@key(M-X Delete File)
@key(M-X Kill File)
@acttype(Remove)
@topic(Files)
@cmddoc
Delete a file.  Prompts for filename.
@end

@fnc(delete-forward-character-command)
@cmd(Delete Forward Character)
@key(C-D)
@key(ESC-P)
@acttype(Remove)
@seeglobal(Kill Ring)
@cmddoc
Delete character after point.
With argument, kill that many characters (saving them).
Negative args kill characters backward.
@end

@fnc(delete-horizontal-space-command)
@cmd(Delete Horizontal Space)
@key(M-\)
@acttype(Remove)
@cmddoc
Delete all spaces and tabs around point.
@end

@fnc(delete-indentation-command)
@cmd(Delete Indentation)
@key(M-^)
@acttype(Remove)
@cmddoc
Delete CRLF and indentation at front of line.
Leaves one space in place of them.  With argument,
moves down one line first (deleting CRLF after current line).
@end

@fnc(delete-matching-lines-command)
@cmd(Delete Matching Lines)
@key(M-X Delete Matching Lines)
@key(M-X Flush Lines)
@acttype(Select)
@acttype(Remove)
@cmddoc
Delete Matching Lines:
Prompts user for string.
Deletes all lines containing specified string.
@end

@fnc(delete-non-matching-lines-command)
@cmd(Delete Non-Matching Lines)
@key(M-X Delete Non-Matching Lines)
@key(M-X Keep Lines)
@acttype(Select)
@acttype(Remove)
@cmddoc
Delete Non-Matching Lines:
Prompts user for string.
Deletes all lines not containing specified string.
@end

@fnc(dired-command)
@cmd(Dired)
@key(C-X D)
@cmddoc
Run Dired on the directory of the current buffer file.
With no argument, edits that directory.
With an argument of 1, shows only the versions of the file in the buffer.
With an argument of 4, asks for input, only versions of that file are shown.
@end

@fnc(down-list-command)
@cmd(Down List)
@key(C-M-D)
@acttype(Move Point)
@mode(Lisp)
@topic(Lisp)
@cmddoc
Move down one level of list structure, forward.
In other words, move forward past the next open bracket, unless there
is in an intervening close bracket.
With a positive command argument, move forward down that many levels.
With a negative command argument, move backward down that many levels.
@end

@fnc(edit-directory-command)
@cmd(Edit Directory)
@key(M-X Dired)
@key(M-X Edit Directory)
@cmddoc
DIRED:
Edit a directory.
The string argument may contain the filespec (with wildcards of course)
        D deletes the file which is on the current line. (also K,^D,^K)
        U undeletes the current line file.
        Rubout undeletes the previous line file.
        Space is like ^N - moves down a line.
        E edit the file.
        S sorts files according to size, read or write date.
        R does a reverse sort.
        ? types a list of commands.
        Q lists files to be deleted and asks for confirmation:
          Typing YES deletes them; X aborts; N resumes DIRED.
@end

@fnc(end-of-defun-command)
@cmd(End Of Defun)
@key(C-M-E)
@key(C-M-])
@acttype(Move Point)
@mode(Lisp)
@topic(Lisp)
@seedef(Defun)
@cmddoc
Move to end of this or next defun.
With argument of 2, finds end of following defun.
With argument of -1, finds end of previous defun, etc.
@end

@fnc(esc-prefix)
@cmd(Esc Prefix)
@key(ESCAPE)
@acttype(Subsequent Command Modifier)
@cmddoc
The command esc-prefix is an escape-prefix for more commands.
It reads a character (subcommand) and dispatches on it.
Used for escape sequences sent by function keys on the keyboard.
@end

@fnc(exchange-point-and-mark)
@cmd(Exchange Point And Mark)
@key(C-X C-X)
@acttype(Mark)
@acttype(Move Point)
@cmddoc
Exchange positions of point and mark.
@end

@fnc(exchange-windows-command)
@cmd(Exchange Windows)
@key(C-X E)
@acttype(Alter Display Format)
@cmddoc
Exchanges the current window with the other window, which becomes current.
In two window mode, the windows swap physical positions.
@end

@fnc(execute-buffer-command)
@cmd(Execute Buffer)
@key(M-X Execute Buffer)
@topic(Buffers)
@cmddoc
This command makes NMODE take input from the specified buffer
as if it were typed in.
This command supercedes any such previous request.
Newline characters are ignored when reading from a buffer.
If a command argument is given then
only the last refresh of the screen triggered by the commands 
actually occurs, otherwise all of the updating of the screen is visible.
@end

@fnc(execute-defun-command)
@cmd(Execute Defun)
@key(Lisp-D)
@mode(Lisp)
@topic(Lisp)
@acttype(Mark)
@seedef(Defun)
@cmddoc
Causes the Lisp reader to read and evaluate the current defun.
If there is no current defin, the Lisp reader will read
a form starting at the current location.
We arrange for output to go to the end of the output buffer.
The mark is set at
the current location in the input buffer, in case user wants to
go back.
@end

@fnc(execute-file-command)
@cmd(Execute File)
@key(M-X Execute File)
@topic(Files)
@cmddoc
This command 
makes NMODE take input from the specified file as if it were typed in.
This command supercedes any such previous request.
Newline characters are ignored when reading from a buffer.
If a command argument is given then
only the last refresh of the screen triggered by the commands 
actually occurs, otherwise all of the updating of the screen is visible.
@end

@fnc(execute-form-command)
@cmd(Execute Form)
@key(Lisp-E)
@mode(Lisp)
@topic(Lisp)
@acttype(Mark)
@cmddoc
Causes the Lisp reader to read and evaluate a form starting at the
beginning of the current line.
We arrange for output to go to the end of the output buffer.
The mark is set at
the current location in the input buffer, in case user wants to
go back.
@end

@fnc(exit-nmode)
@cmd(Exit Nmode)
@key(Lisp-L)
@mode(Lisp)
@topic(Lisp)
@acttype(Escape)
@cmddoc
Leave NMODE, return to normal listen loop.
@end

@fnc(fill-comment-command)
@cmd(Fill Comment)
@key(M-Z)
@seeglobal(Fill Prefix)
@seeglobal(Fill Column)
@seedef(Paragraph)
@acttype(Alter Existing Text)
@cmddoc
This command creates a temporary fill prefix from the start of
the current line.  It replaces the surrounding paragraph
(determined using fill-prefix) with a filled version.
It leaves point at the a position bearing the same relation to the
filled text that the old point did to the old text.
@end

@fnc(fill-paragraph-command)
@cmd(Fill Paragraph)
@key(M-Q)
@seeglobal(Fill Prefix)
@seeglobal(Fill Column)
@seedef(Paragraph)
@topic(Text)
@acttype(Alter Existing Text)
@cmddoc
This fills (or justifies) this (or next) paragraph.
It leaves point at the a position bearing the same relation to the
filled text that the old point did to the old text.
A numeric argument triggers justification rather than filling.
@end

@fnc(fill-region-command)
@cmd(Fill Region)
@key(M-G)
@acttype(Alter Existing Text)
@seeglobal(Fill Prefix)
@seeglobal(Fill Column)
@seedef(Paragraph)
@seedef(Sentence)
@seecmd(Set Fill Column)
@seecmd(Set Fill Prefix)
@topic(Text)
@cmddoc
Fill text from point to mark.
Fill Column specifies the desired text width.
Fill Prefix if present is a string that goes
at the front of each line and is not included in the filling.
See Set Fill Column and Set Fill Prefix.
An explicit argument causes justification instead of filling.
Each sentence which ends within a line is followed by two spaces.
@end

@fnc(find-file-command)
@cmd(Find File)
@key(C-X C-F)
@key(M-X Find File)
@acttype(Move Data)
@acttype(Move Point)
@topic(Files)
@topic(Buffers)
@cmddoc
Visit a file in its own buffer.
If the file is already in some buffer, select that buffer.
Otherwise, visit the file in a buffer named after the file.
@end

@fnc(forward-paragraph-command)
@cmd(Forward Paragraph)
@key(M-])
@acttype(Move Point)
@seedef(Paragraph)
@topic(Text)
@cmddoc
Move forward to end of this or the next paragraph.
When given argument moves forward (n>0) or backward (n<0) by |n| paragraphs
where n is the command argument.
@end

@fnc(forward-sentence-command)
@cmd(Forward Sentence)
@key(M-E)
@topic(Text)
@acttype(Move Point)
@seedef(Sentence)
@cmddoc
Move forward to end of this or the next sentence.
When given argument moves forward (n>0) or backward (n<0) by |n| sentences.
where n is the command argument.
@end

@fnc(forward-up-list-command)
@cmd(Forward Up List)
@key[C-)]
@key[C-M-)]
@mode(Lisp)
@topic(Lisp)
@acttype(Move Point)
@cmddoc
Move up one level of list structure, forward.
Given a command argument n move up |n| levels forward (n>0) or backward (n<0).
@end

@fnc(get-register-command)
@cmd(Get Register)
@key(C-X G)
@acttype(Move Data)
@acttype(Mark)
@cmddoc
Get contents of register (reads name from keyboard).
The name is a single letter or digit.
Usually leaves the pointer before, and the mark after, the text.
With argument, puts point after and mark before.
@end

@fnc(grow-window-command)
@cmd(Grow Window)
@key(C-X ^)
@acttype(Alter Display Format)
@cmddoc
Make this window use more lines.
Argument is number of extra lines (can be negative).
@end

@fnc(help-dispatch)
@cmd(Help Dispatch)
@key(C-?)
@key(M-/)
@key(M-?)
@acttype(Inform)
@cmddoc
Prints the documentation of a command (not a function).
The command character is read from the terminal.
@end

@fnc(incremental-search-command)
@cmd(Incremental Search)
@key(C-S)
@acttype(Move Point)
@acttype(Select)
@cmddoc
Search for character string as you type it.
C-Q quotes special characters.  Rubout cancels last character.
C-S repeats the search, forward, and C-R repeats it backward.
C-R or C-S with search string empty changes the direction of search
or brings back search string from previous search.
Altmode exits the search.
Other Control and Meta chars exit the search and then are executed.
If not all the input string can be found, the rest is not discarded.
You can rub it out, discard it all with C-G, exit,
or use C-R or C-S to search the other way.
Quitting a successful search aborts the search and moves point back;
quitting a failing search just discards whatever input wasn't found.
@end

@fnc(indent-new-line-command)
@cmd(Indent New line)
@key(NEWLINE)
@acttype(Insert Constant)
@cmddoc
This function performs the following actions:
Executes whatever function, if any, is associated with <CR>.
Executes whatever function, if any, is associated with TAB, 
as if no command argument was given.
@end

@fnc(indent-region-command)
@cmd(Indent Region)
@key(C-M-\)
@mode(Text)
@cmddoc
Indent all lines between point and mark.
With argument, indents each line to exactly that column.
A line is processed if its first character is in the region.
It tries to preserve the textual context of point and mark.
@end

@fnc(insert-buffer-command)
@cmd(Insert Buffer)
@key(M-X Insert Buffer)
@acttype(Move Data)
@topic(Buffers)
@cmddoc
Insert contents of another buffer into existing text.
The user is prompted for the buffer name.
Point is left just before the inserted material,
and mark is left just after it.
@end

@fnc(insert-closing-bracket)
@cmd(Insert Closing bracket)
@key[)]
@key(])
@acttype(Insert Constant)
@mode(Lisp)
@topic(Lisp)
@cmddoc
Insert the character typed, which should be a closing bracket, 
then display the matching opening bracket.
@end

@fnc(insert-comment-command)
@cmd(Insert Comment)
@key(M-;)
@mode(Lisp)
@topic(Lisp)
@acttype(Insert Constant)
@cmddoc
Move to the end of the current line, then add a "%" and a space at its end.
Leave point after the space.
@end

@fnc(insert-date-command)
@cmd(Insert Date)
@key(M-X Insert Date)
@acttype(Move Data)
@cmddoc
Insert the current time and date after point.
The mark is put after the inserted text.
@end

@fnc(insert-file-command)
@cmd(Insert File)
@key(M-X Insert File)
@topic(Files)
@acttype(Move Data)
@cmddoc
Insert contents of file into existing text.
File name is string argument.
The pointer is left at the beginning, and the mark at the end.
@end

@fnc(insert-kill-buffer)
@cmd(Insert Kill Buffer)
@key(C-Y)
@seeglobal(Kill Ring)
@acttype(Move Data)
@acttype(Mark)
@cmddoc
Re-insert the last stuff killed.
Puts point after it and the mark before it.
An argument n says un-kill the n'th most recent
string of killed stuff (1 = most recent).  A null
argument (just C-U) means leave point before, mark after.
@end

@fnc(insert-next-character-command)
@cmd(Insert Next Character)
@key(C-Q)
@acttype(Move Data)
@cmddoc
Reads a character and inserts it.
@end

@fnc(kill-backward-form-command)
@cmd(Kill Backward Form)
@key(C-M-RUBOUT)
@mode(Lisp)
@topic(Lisp)
@seeglobal(Kill Ring)
@acttype(Remove)
@cmddoc
Kill the last form.
With a command argument kill the last (n>0) or next (n<0) |n| forms,
where n is the command argument.
@end

@fnc(kill-backward-word-command)
@cmd(Kill Backward Word)
@key(M-RUBOUT)
@acttype(Remove)
@topic(Text)
@seeglobal(Kill Ring)
@cmddoc
Kill last word.
With a command argument kill the last (n>0) or next (n<0) |n| words,
where n is the command argument.
@end

@fnc(kill-buffer-command)
@cmd(Kill Buffer)
@key(C-X K)
@key(M-X Kill Buffer)
@topic(Buffers)
@acttype(Remove)
@cmddoc
Kill the buffer with specified name.
The buffer name is taken from the keyboard.
Name completion is performed by SPACE and RETURN.
If the buffer has changes in it, the user is asked for confirmation.
@end

@fnc(kill-forward-form-command)
@cmd(Kill Forward Form)
@key(C-M-K)
@mode(Lisp)
@topic(Lisp)
@seeglobal(Kill Ring)
@acttype(Remove)
@cmddoc
Kill the next form.
With a command argument kill the next (n>0) or last (n<0) |n| forms,
where n is the command argument.
@end

@fnc(kill-forward-word-command)
@cmd(Kill Forward Word)
@key(M-D)
@seeglobal(Kill Ring)
@topic(Text)
@acttype(Remove)
@cmddoc
Kill the next word.
With a command argument kill the next (n>0) or last (n<0) |n| words,
where n is the command argument.
@end

@fnc(kill-line)
@cmd(Kill Line)
@key(C-K)
@key(ESC-M)
@seeglobal(Kill Ring)
@acttype(Remove)
@cmddoc
Kill to end of line, or kill an end of line.
At the end of a line (only blanks following) kill through the CRLF.
Otherwise, kill the rest of the line but not the CRLF.
With argument (positive or negative), kill specified number of lines
forward or backward respectively.
An argument of zero means kill to the beginning of the 
ine, nothing if at the beginning.
Killed text is pushed onto the kill ring for retrieval.
@end

@fnc(kill-region)
@cmd(Kill Region)
@key(C-W)
@seeglobal(Kill Ring)
@seedef(Region)
@acttype(Remove)
@cmddoc
Kill from point to mark.
Use Control-Y and Meta-Y to get it back.
@end

@fnc(kill-sentence-command)
@cmd(Kill Sentence)
@key(M-K)
@seedef(Sentence)
@seeglobal(Kill Ring)
@topic(Text)
@acttype(Remove)
@cmddoc
Kill forward to end of sentence.
With minus one as an argument it kills back to the beginning of the sentence.
Positive or negative arguments mean to kill that many sentences forward or
backward respectively.
@end

@fnc(kill-some-buffers-command)
@cmd(Kill Some Buffers)
@key(M-X Kill Some Buffers)
@acttype(Remove)
@topic(Buffers)
@cmddoc
Kill Some Buffers:
Offer to kill each buffer, one by one.
If the buffer contains a modified file and you say to kill it,
you are asked for confirmation.
@end

@fnc(lisp-abort-command)
@cmd(Lisp Abort)
@key(Lisp-A)
@mode(Lisp)
@topic(Lisp)
@acttype(Escape)
@cmddoc
This command will pop out of an arbitrarily deep break loop.
@end

@fnc(lisp-backtrace-command)
@cmd(Lisp Backtrace)
@key(Lisp-B)
@mode(Lisp)
@topic(Lisp)
@acttype(Inform)
@cmddoc
This lists all the function calls on the stack. It is a good way to
see how the offending expression got generated.
@end

@fnc(lisp-continue-command)
@cmd(Lisp Continue)
@key(Lisp-C)
@mode(Lisp)
@topic(Lisp)
@acttype(Escape)
@cmddoc
This causes the expression last printed to be returned as the value of the
offending expression.  This allows a user to recover from a low level error
in an involved calculation if they know what should have been returned by the
offending expression.  This is also often useful as an automatic stub:
If an expression containing an undefined function is evaluated, a Break loop is
entered, and this may be used to return the value of the function call.
@end

@fnc(lisp-help-command)
@cmd(Lisp Help)
@key(Lisp-?)
@mode(Lisp)
@topic(Lisp)
@acttype(Inform)
@cmddoc
If in break print:
    "Lisp break commands: Q-quit;A-abort;R-retry;C-continue;B-backtrace"
else print:
    "Lisp commands: E-execute form;Y-yank last output;L-invoke Lisp Listener"
@end

@fnc(lisp-indent-region-command)
@cmd(Lisp Indent Region)
@key(C-M-\)
@mode(Lisp)
@topic(Lisp)
@cmddoc
Indent all lines between point and mark.
With argument, indents each line to exactly that column.
Otherwise, lisp indents each line.
A line is processed if its first character is in the region.
It tries to preserve the textual context of point and mark.
@end

@fnc(lisp-indent-sexpr)
@cmd(Lisp Indent sexpr)
@mode(Lisp)
@topic(Lisp)
@key(C-M-Q)
@cmddoc
Lisp Indent each line contained in the next form.
This command does NOT respond to command arguments.
@end

@fnc(lisp-mode-command)
@cmd(Lisp Mode)
@key(M-X Lisp Mode)
@acttype(Change Mode)
@topic(Lisp)
@cmddoc
Set things up for editing Lisp code.
Tab indents for Lisp.
Rubout hacks tabs.
Lisp execution commands availible.
Paragraphs are delimited only by blank lines.
@end

@fnc(lisp-prefix)
@cmd(Lisp Prefix)
@key(C-])
@mode(Lisp)
@topic(Lisp)
@acttype(Subsequent Command Modifier)
@cmddoc
The command lisp-prefix is an escape-prefix for more commands.
It reads a character (subcommand) and dispatches on it.
@end

@fnc(lisp-quit-command)
@cmd(Lisp Quit)
@key(Lisp-Q)
@mode(Lisp)
@topic(Lisp)
@acttype(Escape)
@cmddoc
This exits the current break loop. It only pops up one level, unlike abort.
@end

@fnc(lisp-retry-command)
@cmd(Lisp Retry)
@key(Lisp-R)
@mode(Lisp)
@topic(Lisp)
@acttype(Escape)
@cmddoc
This tries to evaluate the offending expression again, and to continue the
computation.  This is often useful after defining a missing function,
or assigning a value to a variable.
@end

@fnc(lisp-tab-command)
@cmd(Lisp Tab)
@key(C-M-I)
@key(C-M-TAB)
@mode(Lisp)
@topic(Lisp)
@key(TAB)
@seecmd(Tab To Tab Stop)
@acttype(Alter Existing Text)
@cmddoc
 Indent this line for a Lisp-like language.
With arg, moves over and indents that many lines.
With negative argument, indents preceding lines.
 Note that the binding of TAB to this function holds only in Lisp mode.
In text mode TAB is bound to the Tab To Tab Stop command and the other keys
bound to this function are undefined.
@end

@fnc(lowercase-region-command)
@cmd(Lowercase Region)
@key(C-X C-L)
@seedef(Region)
@acttype(Alter Existing Text)
@cmddoc
Convert region to lower case.
@end

@fnc(lowercase-word-command)
@cmd(Lowercase Word)
@topic(Text)
@key(M-L)
@acttype(Alter Existing Text)
@cmddoc
Convert one word to lower case, moving past it.
With arg, applies to that many words backward or forward.
If backward, the cursor does not move.
@end

@fnc(m-x-prefix)
@cmd(M-X Prefix)
@key(C-M-X)
@key(M-X)
@acttype(Subsequent Command Modifier)
@cmddoc
Read an extended command from the terminal with completion.
Completion is performed by SPACE and RETURN.
This command reads the name of an extended command, with completion,
then executes that command.
The command may itself prompt for input.
@end

@fnc(make-parens-command)
@cmd(Make Parens)
@key[M-(]
@acttype(Insert Constant)
@mode(Lisp)
@topic(Lisp)
@cmddoc
Insert () putting point after the (.
Also make a space before the (, if appropriate.
With argument, put the ) after the specified number
of already existing forms.  Thus, with argument 1,
puts extra parens around the following form.
@end

@fnc(mark-beginning-command)
@cmd(Mark Beginning)
@key(C-<)
@acttype(Mark)
@cmddoc
Set mark at beginning of buffer.
@end

@fnc(mark-defun-command)
@cmd(Mark Defun)
@key(C-M-BACKSPACE)
@key(C-M-H)
@key(M-BACKSPACE)
@acttype(Mark)
@seedef(Defun)
@mode(Lisp)
@topic(Lisp)
@cmddoc
Put point and mark around this defun (or next).
@end

@fnc(mark-end-command)
@cmd(Mark End)
@key(C->)
@acttype(Mark)
@cmddoc
Set mark at end of buffer.
@end

@fnc(mark-form-command)
@cmd(Mark Form)
@mode(Lisp)
@topic(Lisp)
@key(C-M-@)
@acttype(Mark)
@cmddoc
Set mark after (n>0) or before (n<0) |n| forms from point
where n is the command argument.
@end

@fnc(mark-paragraph-command)
@cmd(Mark Paragraph)
@key(M-H)
@acttype(Mark)
@topic(Text)
@seedef(Paragraph)
@acttype(Move Point)
@cmddoc
Put point and mark around this paragraph.
In between paragraphs, puts it around the next one.
@end

@fnc(mark-whole-buffer-command)
@cmd(Mark Whole Buffer)
@key(C-X H)
@acttype(Mark)
@acttype(Move Point)
@cmddoc
Set point at beginning and mark at end of buffer.
Pushes the old point on the mark first, so two pops restore it.
@end

@fnc(mark-word-command)
@cmd(Mark Word)
@key(M-@)
@acttype(Mark)
@topic(Text)
@cmddoc
Set mark after (n>0) or before (n<0) |n| words from point
where n is the command argument.
@end

@fnc(move-backward-character-command)
@cmd(Move Backward Character)
@key(C-B)
@key(ESC-D)
@acttype(Move Point)
@cmddoc
Move back one character.
With argument, move that many characters backward.
Negative arguments move forward.
@end

@fnc(move-backward-defun-command)
@cmd(Move Backward Defun)
@key(C-M-A)
@key(C-M-[)
@seedef(Defun)
@mode(Lisp)
@topic(Lisp)
@acttype(Move Point)
@cmddoc
Move to beginning of this or previous defun.
With a negative argument, moves forward to the beginning of a defun.
@end

@fnc(move-backward-form-command)
@cmd(Move Backward Form)
@key(C-M-B)
@mode(Lisp)
@topic(Lisp)
@acttype(Move Point)
@cmddoc
Move back one form.
With argument, move that many forms backward.
Negative arguments move forward.
@end

@fnc(move-backward-list-command)
@cmd(Move Backward List)
@key(C-M-P)
@mode(Lisp)
@topic(Lisp)
@acttype(Move Point)
@cmddoc
Move back one list.
With argument, move that many lists backward.
Negative arguments move forward.
@end

@fnc(move-backward-word-command)
@cmd(Move Backward Word)
@key(ESC-4)
@key(M-B)
@topic(Text)
@acttype(Move Point)
@cmddoc
Move back one word.
With argument, move that many words backward.
Negative arguments move forward.
@end

@fnc(move-down-command)
@cmd(Move Down)
@key(ESC-B)
@acttype(Move Point)
@seeglobal(Goal Column)
@cmddoc
Move point down a line.
If a command argument n is given, move point down (n>0) or up (n<0)
by |n| lines.
@end

@fnc(move-down-extending-command)
@cmd(Move Down Extending)
@key(C-N)
@acttype(Move Point)
@seeglobal(Goal Column)
@cmddoc
Move down vertically to next line.
If given an argument moves down (n>0) or up (n<0) |n| lines where
n is the command argument.
If given without an argument after the
last LF in the buffer, makes a new one at the end.
@end

@fnc(move-forward-character-command)
@cmd(Move Forward Character)
@key(C-F)
@key(ESC-C)
@acttype(Move Point)
@cmddoc
Move forward one character.
With argument, move that many characters forward.
Negative args move backward.
@end

@fnc(move-forward-form-command)
@cmd(Move Forward Form)
@key(C-M-F)
@mode(Lisp)
@topic(Lisp)
@acttype(Move Point)
@cmddoc
Move forward one form.
With argument, move that many forms forward.
Negative args move backward.
@end

@fnc(move-forward-list-command)
@cmd(Move Forward List)
@key(C-M-N)
@mode(Lisp)
@topic(Lisp)
@acttype(Move Point)
@cmddoc
Move forward one list.
With argument, move that many lists forward.
Negative args move backward.
@end

@fnc(move-forward-word-command)
@cmd(Move Forward Word)
@key(ESC-5)
@key(M-F)
@topic(Text)
@acttype(Move Point)
@cmddoc
Move forward one word.
With argument, move that many words forward.
Negative args move backward.
@end

@fnc(move-over-paren-command)
@cmd(Move Over Paren)
@key[M-)]
@mode(Lisp)
@topic(Lisp)
@acttype(Move Point)
@cmddoc
Move forward past the next closing bracket.  If a positive command
argument is given, move forward past that many closing brackets.
Delete all indentation before the first closing bracket passed.
After the last closing bracket passed, insert an end-of-line and
then indent the new line according to Lisp.
@end

@fnc(move-to-buffer-end-command)
@cmd(Move To Buffer End)
@key(ESC-F)
@key(M->)
@acttype(Move Point)
@cmddoc
Go to end of buffer (leaving mark behind).
@end

@fnc(move-to-buffer-start-command)
@cmd(Move To Buffer Start)
@key(ESC-H)
@key(M-<)
@acttype(Move Point)
@cmddoc
Go to beginning of buffer (leaving mark behind).
@end

@fnc(move-to-end-of-line-command)
@cmd(Move To End Of Line)
@key(C-E)
@acttype(Move Point)
@cmddoc
Move point to end of line.
With positive argument n goes down n-1 lines, then to the end of line.
With zero argument goes up a line, then to line end.
With negative argument n goes up |n|+1 lines, then to the end of line.
@end

@fnc(move-to-screen-edge-command)
@cmd(Move To Screen Edge)
@key(M-R)
@acttype(Move Point)
@cmddoc
Jump to top or bottom of screen.
Like Control-L except that point is changed instead of the window.
With no argument, jumps to the center.
An argument specifies the number of lines from the top,
(negative args count from the bottom).
@end

@fnc(move-to-start-of-line-command)
@cmd(Move To Start Of Line)
@key(C-A)
@acttype(Move Point)
@cmddoc
Move point to beginning of line.
With positive argument n goes down n-1 lines, then to the beginning of line.
With zero argument goes up a line, then to line beginning.
With negative argument n goes up |n|+1 lines, then to the beginning of line.
@end

@fnc(move-up-command)
@cmd(Move Up)
@key(C-P)
@key(ESC-A)
@seeglobal(Goal Column)
@acttype(Move Point)
@cmddoc
Move up vertically to next line.
If given an argument moves up (n>0) or down (n<0) |n| lines where
n is the command argument.
@end

@fnc(negative-argument)
@cmd(Negative Argument)
@key(C--)
@key(C-M--)
@key(M--)
@acttype(Subsequent Command Modifier)
@cmddoc
Make argument to next command negative.
@end

@fnc(next-screen-command)
@cmd(Next Screen)
@key(C-V)
@acttype(Move Point)
@cmddoc
Move down to display next screenful of text.
With argument, moves window down <arg> lines (negative moves up).
Just minus as an argument moves up a full screen.
@end

@fnc(nmode-abort-command)
@cmd(Nmode Abort)
@key(C-G)
@acttype(Escape)
@cmddoc
This command provides a way of aborting input requests.
@end

@fnc(nmode-exit-to-superior)
@cmd(Nmode Exit To Superior)
@key(C-X C-Z)
@acttype(Escape)
@cmddoc
Go back to EMACS's superior job.
@end

@fnc(nmode-full-refresh)
@cmd(Nmode Full Refresh)
@key(ESC-J)
@acttype(Alter Display Format)
@cmddoc
This function refreshes the screen after first clearing the
display.  It it used when the state of the display is in doubt.
@end

@fnc(nmode-gc)
@cmd(Nmode Gc)
@key(M-X Make Space)
@cmddoc
Reclaims any internal wasted space.
@end

@fnc(nmode-invert-video)
@cmd(Nmode Invert Video)
@key(C-X V)
@acttype(Alter Display Format)
@cmddoc
Toggle between normal and inverse video.
@end

@fnc(nmode-refresh-command)
@cmd(Nmode Refresh)
@key(C-L)
@acttype(Alter Display Format)
@cmddoc
Choose new window putting point at center, top or bottom.
With no argument, chooses a window to put point at the center.
An argument gives the line to put
point on;  negative args count from the bottom.
@end

@fnc(one-window-command)
@cmd(One Window)
@key(C-X 1)
@acttype(Alter Display Format)
@cmddoc
Display only one window.
Normally, we display what used to be in the top window,
but a numeric argument says to display what was in the bottom one.
@end

@fnc(open-line-command)
@cmd(Open Line)
@key(C-O)
@key(ESC-L)
@acttype(Insert Constant)
@cmddoc
Insert a CRLF after point.
Differs from ordinary insertion in that point remains
before the inserted characters.
With positive argument, inserts several CRLFs.
With negative argument does nothing.
@end

@fnc(other-window-command)
@cmd(Other Window)
@key(C-X O)
@acttype(Alter Display Format)
@acttype(Move Point)
@cmddoc
Switch to the other window.
In two-window mode, moves cursor to other window.
In one-window mode, exchanges contents of visible window
with remembered contents of (invisible) window two.
An argument means switch windows but select the same
buffer in the other window.
@end

@fnc(prepend-to-file-command)
@cmd(Prepend To File)
@topic(Files)
@key(M-X Prepend To File)
@seedef(Region)
@acttype(Move Data)
@cmddoc
Append region to start of specified file.
@end

@fnc(previous-screen-command)
@cmd(Previous Screen)
@key(M-V)
@acttype(Move Point)
@cmddoc
Move up to display previous screenful of text.
When an argument is present, move the window back (n>0)
or forward (n<0) |n| lines, where n is the command argument.
@end

@fnc(put-register-command)
@cmd(Put Register)
@key(C-X X)
@acttype(Preserve)
@cmddoc
Put point to mark into register (reads name from keyboard).
With an argument, the text is also deleted.
@end

@fnc(query-replace-command)
@cmd(Query Replace)
@key(M-%)
@key(M-X Query Replace)
@acttype(Alter Existing Text)
@acttype(Select)
@cmddoc
Replace occurrences of a string from point to the
end of the buffer, asking about each occurrence.
Query Replace prompts for the string to be replaced and for its
potential replacement.
Query Replace displays each occurrence of the string to be replaced,
you then type a character to say what to do.
Space => replace it with the potential replacement and show the next copy.
Rubout or Backspace => don't replace, but show next copy.
Comma => replace this copy and show result, waiting for next command.
^ => return to site of previous copy.
C-L => redisplay screen.
Exclamation mark => replace all remaining copys without asking.
Period => replace this copy and exit.
Escape => just exit.
Anything else exits and is reread.
@end

@fnc(rename-buffer-command)
@cmd(Rename Buffer)
@key(M-X Rename Buffer)
@topic(Buffers)
@acttype(Set Global Variable)
@cmddoc
Change the name of the current buffer.
The new name is read from the keyboard.
If the user provides an empty string, the buffer name will be set to
a truncated version of the filename associated with the buffer.
The buffer name is automatically converted to upper case.
An error is reported if the user provides the name of another existing
buffer.  The buffers MAIN and OUTPUT may not be renamed.
@end

@fnc(replace-string-command)
@cmd(Replace String)
@key(C-%)
@key(M-X Replace String)
@acttype(Alter Existing Text)
@acttype(Select)
@cmddoc
Replace string with another from point to buffer end.
@end

@fnc(reposition-window-command)
@cmd(Reposition Window)
@key(C-M-R)
@mode(Lisp)
@topic(Lisp)
@acttype(Alter Display Format)
@cmddoc
Reposition screen window appropriately.
Tries to get all of current defun on screen.
Never moves the pointer.
@end

@fnc(return-command)
@cmd(Return)
@key(RETURN)
@acttype(Insert Constant)
@cmddoc
Insert CRLF, or move onto empty line.
Repeated by positive argument.
No action with negative argument.
@end

@fnc(reverse-search-command)
@cmd(Reverse Search)
@key(C-R)
@acttype(Move Point)
@acttype(Select)
@seecmd(Incremental Search)
@cmddoc
Incremental Search Backwards.
Like Control-S but in reverse.
@end

@fnc(revert-file-command)
@cmd(Revert File)
@topic(Files)
@key(M-X Revert File)
@acttype(Remove)
@cmddoc
Undo changes to a file.
Reads back the file being edited from disk
@end

@fnc(save-all-files-command)
@cmd(Save All Files)
@key(M-X Save All Files)
@topic(Buffers)
@topic(Files)
@acttype(Preserve)
@cmddoc
Offer to write back each buffer which may need it.
For each buffer which is visiting a file and which
has been modified, you are asked whether to save it.
A numeric arg means don't ask;  save everything.
@end

@fnc(save-file-command)
@cmd(Save File)
@key(C-X C-S)
@topic(Files)
@acttype(Preserve)
@cmddoc
Save visited file on disk if modified.
@end

@fnc(scroll-other-window-command)
@cmd(Scroll Other Window)
@key(C-M-V)
@acttype(Alter Display Format)
@cmddoc
Scroll other window up several lines.
Specify the number as a numeric argument, negative for down.
The default is a whole screenful up.  Just Meta-Minus as argument
means scroll a whole screenful down.
@end

@fnc(scroll-window-down-line-command)
@cmd(Scroll Window Down Line)
@key(ESC-T)
@acttype(Alter Display Format)
@cmddoc
Scroll the contents of the window down (n > 0) or up (n < 0) by |n| lines
where n is the command argument.
The "window position" may be adjusted to keep it within the window.  Ding if
the window contents does not move.
@end

@fnc(scroll-window-down-page-command)
@cmd(Scroll Window Down Page)
@key(ESC-V)
@acttype(Alter Display Format)
@cmddoc
Scroll the contents of the window down (n > 0) or up (n < 0) by |n| screenfuls
where n is the command argument.
The "window position" may be adjusted to keep it within the
window.  Ding if the window contents does not move.
@end

@fnc(scroll-window-left-command)
@cmd(Scroll Window Left)
@key(C-X <)
@acttype(Alter Display Format)
@cmddoc
Scroll the contents of the specified window right (n > 0) or left (n < 0)
by |n| columns where n is the command argument.
@end

@fnc(scroll-window-right-command)
@cmd(Scroll Window Right)
@key(C-X >)
@acttype(Alter Display Format)
@cmddoc
Scroll the contents of the specified window left (n > 0) or right (n < 0)
by |n| columns where n is the command argument.
@end

@fnc(scroll-window-up-line-command)
@cmd(Scroll Window Up Line)
@key(ESC-S)
@acttype(Alter Display Format)
@cmddoc
Scroll the contents of the window up (n > 0) or down (n < 0) by |n| lines
where n is the command argument.
The "window position" may be adjusted to keep it within the window.  Ding if
the window contents does not move.
@end

@fnc(scroll-window-up-page-command)
@cmd(Scroll Window Up Page)
@key(ESC-U)
@acttype(Alter Display Format)
@cmddoc
Scroll the contents of the window up (n > 0) or down (n < 0) by |n| screenfuls
where n is the command argument.
The "window position" may be adjusted to keep it within the
window.  Ding if the window contents does not move.
@end

@fnc(select-buffer-command)
@cmd(Select Buffer)
@key(C-X B)
@key(M-X Select Buffer)
@acttype(Move Point)
@topic(Buffers)
@cmddoc
Select or create buffer with specified name.
Buffer name is read from keyboard.
Name completion is performed by SPACE and RETURN.
@end

@fnc(select-previous-buffer-command)
@cmd(Select Previous Buffer)
@key(C-M-L)
@topic(Buffers)
@acttype(Move Point)
@cmddoc
Select the previous buffer of the current buffer, if it exists and
is selectable.
Otherwise, select the MAIN buffer.
@end

@fnc(set-fill-column-command)
@cmd(Set Fill Column)
@seeglobal(Fill Column)
@key(C-X F)
@acttype(Set Global Variable)
@cmddoc
Set fill column to numeric arg or current column.
If there is an argument, that is used.
Otherwise, the current position of the cursor is used.
The Fill Column variable controls where Auto Fill mode
and the fill commands put the right margin.
@end

@fnc(set-fill-prefix-command)
@cmd(Set Fill Prefix)
@seeglobal(Fill Prefix)
@key(C-X .)
@acttype(Set Global Variable)
@cmddoc
Defines Fill Prefix from current line.
All of the current line up to point becomes the value
of Fill Prefix.  Auto Fill Mode inserts the
prefix on each line;  the Fill Paragraph command assumes that each
non-blank line starts with the prefix (which is ignored
for filling purposes).
To stop using a Fill Prefix, do Control-X .
at the front of a line.
@end

@fnc(set-goal-column-command)
@cmd(Set Goal Column)
@key(C-X C-N)
@acttype(Set Global Variable)
@cmddoc
Set (or flush) a permanent goal for vertical motion.
With no argument, makes the current column the goal for vertical
motion commands.  They will always try to go to that column.
With argument, clears out any previously set goal.  Only
Control-P and Control-N are affected.
@end

@fnc(set-key-command)
@cmd(Set Key)
@key(M-X Set Key)
@acttype(Set Global Variable)
@cmddoc
Put a function on a key.
The function name is a string argument.
The key is always read from the terminal (not a string argument).
It may contain metizers and other prefix characters.
@end

@fnc(set-mark-command)
@cmd(Set Mark)
@key(C-@)
@key(C-SPACE)
@acttype(Mark)
@cmddoc
Sets or pops the mark.
With no ^U's, pushes point as the mark.
With one ^U, pops the mark into point.
With two ^U's, pops the mark and throws it away.
@end

@fnc(set-visited-filename-command)
@cmd(Set Visited Filename)
@key(M-X Set Visited Filename)
@topic(Files)
@acttype(Set Global Variable)
@cmddoc
Change visited filename, without writing or reading any file.
The user is prompted for a filename.
What NMODE believes to be the name
of the visited file associated with the current buffer
is set from the user's input.
No file's name is actually changed.
If possible, the new name will be adjusted to reflect an actual
file name, as if the specified file were visited.
@end

@fnc(split-line-command)
@cmd(Split Line)
@key(C-M-O)
@acttype(Insert Constant)
@cmddoc
Move rest of this line vertically down.
Inserts a CRLF, and then enough tabs/spaces so that
what had been the rest of the current line is indented as much as
it had been.  Point does not move, except to skip over indentation
that originally followed it. 
With positive argument, makes extra blank lines in between.
No action with negative argument.
@end

@fnc(start-scripting-command)
@cmd(Start Scripting)
@key(M-X Start Scripting)
@acttype(Change Mode)
@cmddoc
This function prompts the user for a buffer name, into which it will copy
all the user's commands (as well as executing them) until the
stop-scripting-command is invoked.
This command supercedes any such previous request.
Note that to keep the lines of reasonable length,
free Newlines will be inserted from time to time.  Because of this, and
because many file systems cannot represent stray Newlines, the Newline
character is itself scripted as a CR followed by a TAB, since this is its
normal definition.  Someday, perhaps, this hack will be replaced by a better
one.
@end

@fnc(start-timing-command)
@cmd(Start Timing)
@key(M-X Start Timing Nmode)
@acttype(Change Mode)
@cmddoc
This cleans up a number of global variables associated with timing,
prompts for a file in which to put the timing data (or defaults to a
file named "timing", of type "txt"), and starts the timing. Information
is collected on the total time, refresh time, read time, command execution
time, total number of cons cells built, and total number of garbage collections
performed.
@end

@fnc(stop-scripting-command)
@cmd(Stop Scripting)
@key(M-X Stop Scripting)
@acttype(Change Mode)
@cmddoc
This command stops the echoing of user commands into a script buffer.
This command is itself echoed before the creation of the script stops.
@end

@fnc(stop-timing-command)
@cmd(Stop Timing)
@key(M-X Stop Timing Nmode)
@acttype(Change Mode)
@cmddoc
This stops the timing, formats the output data, and closes the file into
which the timing information is going.  Information is collected on the
total time, refresh time, read time, command execution time, total number
of cons cells built, and total number of garbage collections performed.
In addition to these numbers, some ratios are printed.
@end

@fnc(tab-to-tab-stop-command)
@cmd(Tab To Tab Stop)
@key(M-I)
@key(M-TAB)
@key(TAB)
@seecmd(Lisp Tab)
@acttype(Insert Constant)
@cmddoc
Insert a tab character.
Note that the binding of TAB to this command only holds in text mode,
not in lisp mode, where it is bound to the Lisp Tab command. 
In lisp mode, the other keys continue to be bound to this command.
@end

@fnc(text-mode-command)
@cmd(Text Mode)
@key(M-X Text Mode)
@topic(Text)
@acttype(Change Mode)
@cmddoc
Set things up for editing English text.
Tab inserts tab characters.
There are no comments.
Auto Fill does not indent new lines.
@end

@fnc(transpose-characters-command)
@cmd(Transpose Characters)
@key(C-T)
@acttype(Alter Existing Text)
@seecmd(Transpose Words)
@cmddoc
Transpose the characters before and after the cursor.
For more details, see Meta-T, reading "character" for "word".
However: at the end of a line, with no argument, the preceding
two characters are transposed.
@end

@fnc(transpose-forms)
@cmd(Transpose Forms)
@key(C-M-T)
@mode(Lisp)
@topic(Lisp)
@seecmd(Transpose Words)
@acttype(Alter Existing Text)
@cmddoc
Transpose the forms before and after the cursor.
For more details, see Meta-T, reading "Form" for "Word".
@end

@fnc(transpose-lines)
@cmd(Transpose Lines)
@key(C-X C-T)
@seecmd(Transpose Words)
@acttype(Alter Existing Text)
@cmddoc
Transpose the lines before and after the cursor.
For more details, see Meta-T, reading "Line" for "Word".
@end

@fnc(transpose-regions)
@cmd(Transpose Regions)
@key(C-X T)
@seedef(Region)
@acttype(Alter Existing Text)
@cmddoc
Transpose regions defined by cursor and last 3 marks.
To transpose two non-overlapping regions, set the mark successively at three
of the four boundaries, put point at the fourth, and call this function.
@end

@fnc(transpose-words)
@cmd(Transpose Words)
@key(M-T)
@topic(Text)
@acttype(Alter Existing Text)
@cmddoc
Transpose the words before and after the cursor.
With a positive argument it transposes the words before and
after the cursor, moves right, and repeats the specified number of
times, dragging the word to the left of the cursor right.  With a
negative argument, it transposes the two words to the left of
the cursor, moves between them, and repeats the specified number of
times, exactly undoing the positive argument form.  With a zero
argument, it transposes the words at point and mark.
@end

@fnc(two-windows-command)
@cmd(Two Windows)
@key(C-X 2)
@acttype(Alter Display Format)
@cmddoc
Show two windows and select window two.
An argument > 1 means give window 2 the same buffer as in Window 1.
@end

@fnc(undelete-file-command)
@cmd(Undelete File)
@key(M-X Undelete File)
@acttype(Move Data)
@acttype(Preserve)
@topic(Files)
@cmddoc
This command prompts the user for the name of the file. NMODE will fill in
a partly specified filename (eg filetype can be defaulted).
If possible, the file will then be undeleted, and a message
to that effect will be displayed. If the operation fails, the bell will sound.
@end

@fnc(universal-argument)
@cmd(Universal Argument)
@key(C-U)
@acttype(Subsequent Command Modifier)
@cmddoc
Sets argument or multiplies it by four.
Followed by digits, uses them to specify the
argument for the command after the digits.
If not followed by digits, multiplies the argument by four.
@end

@fnc(unkill-previous)
@cmd(Unkill Previous)
@seedef(Region)
@seeglobal(Kill Ring)
@key(M-Y)
@acttype(Alter Existing Text)
@cmddoc
Delete (without saving away) the current region, and then unkill (yank) the
specified entry in the kill ring.  "Ding" if the current region does not
contain the same text as the current entry in the kill ring.
If one has just retrieved the top entry from the kill ring this has the
effect of displaying the item just beneath it, then the item beneath that
and so on until the original top entry rotates back into view.
@end

@fnc(upcase-digit-command)
@cmd(Upcase Digit)
@key(M-')
@acttype(Alter Existing Text)
@cmddoc
Convert last digit to shifted character.
Looks on current line back from point, and previous line.
The first time you use this command, it asks you to type
the row of digits from 1 to 9 and then 0, holding down Shift,
to determine how your keyboard is set up.
@end

@fnc(uppercase-initial-command)
@cmd(Uppercase Initial)
@key(M-C)
@topic(Text)
@acttype(Alter Existing Text)
@cmddoc
Put next word in lower case, but capitalize initial.
With arg, applies to that many words backward or forward.
If backward, the cursor does not move.
@end

@fnc(uppercase-region-command)
@cmd(Uppercase Region)
@key(C-X C-U)
@seedef(Region)
@acttype(Alter Existing Text)
@cmddoc
Convert region to upper case.
@end

@fnc(uppercase-word-command)
@cmd(Uppercase Word)
@key(M-U)
@topic(Text)
@acttype(Alter Existing Text)
@cmddoc
Convert one word to upper case, moving past it.
With arg, applies to that many words backward or forward.
If backward, the cursor does not move.
@end

@fnc(view-two-windows-command)
@cmd(View Two Windows)
@key(C-X 3)
@acttype(Alter Display Format)
@cmddoc
Show two windows but stay in first.
@end

@fnc(visit-file-command)
@cmd(Visit File)
@key(C-X C-V)
@topic(Files)
@key(M-X Visit File)
@acttype(Move Data)
@acttype(Move Point)
@cmddoc
Visit new file in current buffer.
The user is prompted for the filename.
If the current buffer is modified, the user is asked whether to write it out.
@end

@fnc(visit-in-other-window-command)
@cmd(Visit In Other Window)
@key(C-X 4)
@acttype(Move Point)
@acttype(Alter Display Format)
@topic(Files)
@topic(Buffers)
@cmddoc
Find buffer or file in other window.
Follow this command by B and a buffer name, or by
F and a file name.
We find the buffer or file in the other window,
creating the other window if necessary.
@end

@fnc(what-cursor-position-command)
@cmd(What Cursor Position)
@key(C-=)
@key(C-X =)
@acttype(Inform)
@cmddoc
Print various things about where cursor is.
Print the X position, the Y position,
the octal code for the following character,
point absolutely and as a percentage of the total file size,
and the virtual boundaries, if any.
If a positive argument is given point will jump to the line number
specified by the argument.
A negative argument triggers a jump to the first line in the buffer.
@end

@fnc(write-file-command)
@cmd(Write File)
@key(C-X C-W)
@key(M-X Write File)
@topic(Files)
@acttype(Preserve)
@cmddoc
Prompts for file name.
Stores the current buffer in specified file.
This file becomes the one being visited.
@end

@fnc(write-region-command)
@cmd(Write Region)
@key(M-X Write Region)
@seedef(Region)
@topic(Files)
@acttype(Preserve)
@cmddoc
Write region to file.
Prompts for file name.
@end

@fnc(write-screen-command)
@cmd(Write Screen)
@key(C-X P)
@topic(Files)
@acttype(Preserve)
@cmddoc
Ask for filename, write out the screen to the file.
@end

@fnc(yank-last-output-command)
@cmd(Yank Last Output)
@key(Lisp-Y)
@mode(Lisp)
@topic(Lisp)
@acttype(Move Data)
@cmddoc
Insert "last output" typed in the OUTPUT buffer.
@end
