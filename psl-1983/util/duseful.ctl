@cd pu:
@psl:rlisp
load useful;
off redefmsg,usermode,macro!-displace;
on defmacro!-displaces;
faslout "pl:duseful";
in "backquote.sl"$
in "read-macros.sl"$
in "destructure.sl"$
in "cond-macros.sl"$
in "bind-macros.sl"$
in "set-macros.sl"$
in "iter-macros.sl"$
in "for-macro.sl"$
in "misc-macros.sl"$
in "macroexpand.sl"$
push('useful,options!*)$
faslend;
quit;