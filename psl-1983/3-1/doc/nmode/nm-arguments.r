.so pndoc:nman
.part NM-ARGUMENTS manual
@Chapter[Giving Numeric Arguments to NMODE Commands]
@node("arguments")
@index{numeric arguments}
  Any NMODE command can be given a @dfn[numeric argument].  Some commands
interpret the argument as a repetition count.  For example, giving an
argument of ten to the C-F command (move forward one character)
moves forward ten characters.  With these commands, no argument is
equivalent to an argument of 1.

  Some commands care only about whether there is an argument, and not
about its value; for example, the command M-Q
(@fnc{fill-paragraph-command})
with no arguments fills text, but with an argument justifies the text as
well.

  Some commands use the value of the argument, but do something
peculiar when there is no argument.  For example, the C-K (@fnc{kill-line})
command with an argument <n> kills <n> lines and the line
separators that follow them.  But C-K with no argument is special; it
kills the text up to the next line separator, or, if point is right at
the end of the line, it kills the line separator itself.  Thus, two
C-K commands with no arguments can kill a nonblank line, just like C-K
with an argument of one.

@keyindex{C-U}
@fncindex{universal-argument}
@keyindex{C-O}
@fncindex{open-line-command}
@fncindex{argument-digit}
@fncindex{negative-argument}
  The fundamental way of specifying an argument is to use the C-U
(@fnc{universal-argument})
command followed by the digits of the
argument.  Negative arguments are allowed.  Often they tell a command
to move or act backwards.  A negative argument is entered with C-U
followed by a minus sign and the digits of the value of the argument.
Another option for entering arguments is to use C-digit or strings
there of.
This runs the function @fnc{argument-digit} each time C-digit is entered.
For example, C-U 1 2 3 does the same thing as C-1 C-2 C-3, both apply
an argument of 123 to the next command.
Negative arguments can also be specified with C-- (C-minus)
which runs the function @fnc{negative-argument}.

  C-U followed by a character which is neither a digit nor a minus
sign has the special meaning of "multiply by four".  It multiplies the
argument for the next command by four.  Two such C-U's multiply it by
sixteen.  Thus, @w[C-U C-U C-F] moves forward sixteen characters.  This
is a good way to move forward "fast", since it moves about 1/4 of a
line on most terminals.  Other useful combinations are @w[C-U C-N],
@w[C-U C-U C-N] (move down a good fraction of a screen), @w[C-U C-U C-O]
(make "a lot" of blank lines), and @w[C-U C-K] (kill four lines).
With commands like M-Q that care whether there is an argument but not
what the value is, C-U is a good way of saying "I want an argument".

  A few commands treat a plain C-U differently from an ordinary
argument.  A few others may treat an argument of just a minus sign
differently from an argument of -1.  These unusual cases will be
described when they come up; they are always for reasons of
convenience of use.
