%
% This file runs the "*.tex" to "redhelp.html" conversion code. As
% well as providing the top level direction to the process it patches up
% for at least some of the places where the conversion code had been
% written in a manner not strongly related to the portability objectives of
% Standard Lisp...
%

symbolic;
off echo;
on backtrace;
on comp;
!*windows := t;

fluid '(package);

package := 'redhelp;

symbolic procedure deletip(a, b);
   delete(a, b);

symbolic procedure inf x;
   char!-code x;

symbolic procedure channellinelength(f, l);
  begin
    f := wrs f;
    l := linelength l;
    wrs f;
    return l
  end;

symbolic procedure channelprin2(f, x);
  begin
    f := wrs f;
    prin2 x;
    wrs f;
    return x
  end;

symbolic procedure channelprin2t(f, x);
  begin
    f := wrs f;
    prin2t x;
    wrs f;
    return x
  end;

symbolic macro procedure channelprintf u;
  begin
    scalar g;
    g := gensym();
    return list('prog, list g,
       list('setq, g, list('wrs, cadr u)),
       'printf . cddr u,
       list('wrs, g))
  end;

symbolic procedure channelterpri f;
  begin
    f := wrs f;
    terpri();
    wrs f;
  end;

symbolic procedure channelreadch f;
  begin
    scalar c;
    f := rds f;
    c := readch();
    rds f;
    return c
  end;

in "comphelp.red"$
in "helphtml.red"$
in "minitex.red"$

dir_src := "../help/";

tr get_label, node, emit_node_label,
   emit_node_title, emit_node_browse, emit_node_keys, make_label,
   emit_hidden_node_key;

reset_html();

job(bldmsg("%w.tex",package), "null.fil");

reset_html();

job(bldmsg("%w.tex",package), "null.fil");

html_indexfile();

LISP_indexfile();

delete!-file "null.fil";

bye;

