.so pndoc:nman
.part NM-REPLACEMENT manual
@Chapter[Replacement Commands]
@node("replace")
@index{searching}
@index{replacement}
@index{Replace String}
  Global search-and-replace operations are not needed as often in NMODE
as they are in other editors, but they are available.  In
addition to the simple Replace operation which is like that found in
most editors, there is a Query Replace operation which asks you, for
each occurrence of the pattern, whether to replace it.

@fncindex{replace-string-command}
  To replace every instance of FOO after point with BAR, you can do
@example[
M-X Replace@return1{}FOO@return1{}BAR@return1{}
]
This invokes @fnc{replace-string-command}.
Replacement occurs only after point, so if you want to cover the
whole buffer you must go to the beginning first.  Replacement
continues to the end of the buffer.
@Section[Query Replace]
@index{Query Replace}
@fncindex{query-replace-command}
  If you want to change only some of the occurrences of FOO, not all,
then you cannot use an ordinary Replace.
Instead, use M-X Query Replace@return1{}FOO@return1{}BAR@return2{} 
(@fnc{query-replace-command}).
This displays each occurrence of FOO and waits
for you to say whether to replace it with a BAR.  The things you can
type when you are shown an occurrence of FOO are:
@index{Space}
@index{Rubout}
@index{Comma}
@keyindex{ESCape (Execute)}
@index{.}
@index{!}
@index{^}
@WideCommands{
Space	to replace the FOO

Rubout	to skip to the next FOO without replacing this one.

Comma  	to replace this FOO and display the result.
You are then asked for another input character,
except that since the replacement has already been
made, Rubout and Space are equivalent.

Escape	to exit without doing any more replacements.

Period 	to replace this FOO and then exit.

!	to replace all remaining FOO's without asking.

^	to go back to the previous FOO (or, where it was),
in case you have made a mistake.
}
If you type any other character, the Query Replace is exited, and
the character executed as a command.
@Section[Other Search-and-loop Functions]
  Here are some other functions related to replacement.  Their
arguments are strings.

@fncindex{count-occurrences-command} 
@fncindex{delete-non-matching-lines-command}
@fncindex{delete-matching-lines-command}
@index{deletion}@index{replacement}
@GrossCommands[
M-X How Many@return1{}FOO@return1{}
invoke @fnc{count-occurrences-command} and
print the number of occurrences of FOO after point.

M-X Count Occurrences@return1{}FOO@return1{} Same as M-X How Many.

M-X Keep Lines@return1{}FOO@return1{}
invoke @fnc{delete-non-matching-lines-command} and
kill all lines after point that don't contain FOO.

M-X Delete Non-Matching Lines@return1{}FOO@return1{} Same as M-X Keep Lines.

M-X Flush Lines@return1{}FOO@return1{}
invoke @fnc{delete-matching-lines-command} and
kill all lines after point that contain FOO.

M-X Delete Matching Lines@return1{}FOO@return1{} Same as M-X Flush Lines.
]
