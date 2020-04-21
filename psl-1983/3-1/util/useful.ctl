@cd pu:
@psl:rlisp
load build,useful;
off redefmsg,usermode;
in "backquote.sl"$
in "read-macros.sl"$
in "destructure.sl"$
in "cond-macros.sl"$
in "bind-macros.sl"$
in "set-macros.sl"$
in "iter-macros.sl"$
remflag('(for),'lose);
in "for-macro.sl"$
in "misc-macros.sl"$
in "macroexpand.sl"$
build 'useful;
quit;
@tags
pu:useful.tags
pu:backquote.sl
pu:read-macros.sl
pu:destructure.sl
pu:cond-macros.sl
pu:bind-macros.sl
pu:set-macros.sl
pu:iter-macros.sl
pu:for-macro.sl
pu:misc-macros.sl
pu:macroexpand.sl
*
