% "ccomp.red"                 Copyright 1991-2002,  Codemist Ltd
%
% Compiler that turns Lisp code into C in a way that fits in
% with the conventions used with CSL/CCL
%
%                                                        A C Norman
%

%
% This code may be used and modified, and redistributed in binary
% or source form, subject to the "CCL Public License", which should
% accompany it. This license is a variant on the BSD license, and thus
% permits use of code derived from this in either open and commercial
% projects: but it does require that updates to this code be made
% available back to the originators of the package.
% Before merging other code in with this or linking this code
% with other packages or libraries please check that the license terms
% of the other material are compatible with those of this.
%

symbolic;

global '(!*fastvector !*unsafecar);
flag('(fastvector unsafecar), 'switch);

%
% I start with some utility functions that provide something
% related to a FORMAT or PRINTF facility
%

fluid '(C_file L_file O_file L_contents File_name);

symbolic macro procedure c!:printf(u,!&optional,env);
% inspired by the C printf function, but much less general.
% This macro is to provide the illusion that printf can take an
% arbitrary number of arguments.
  list('c!:printf1, cadr u, 'list . cddr u);

symbolic procedure c!:printf1(fmt, args);
% this is the inner works of print formatting.
% the special sequences that can occur in format strings are
%               %s       use princ (to print a name?)
%               %d       use princ (to print a number?)
%               %a       use prin
%               %t       do a ttab()
%               %v       print a variable.... magic for this compiler
%               \n       do a terpri()
%               \q       princ '!" to display quote marks
  begin
    scalar a, c;
    fmt := explode2 fmt;
    while fmt do <<
      c := car fmt;
      fmt := cdr fmt;
      if c = '!\ and (car fmt = '!n or car fmt = '!N) then <<
         terpri();
         fmt := cdr fmt >>
      else if c = '!\ and (car fmt = '!q or car fmt = '!Q) then <<
         princ '!";
         fmt := cdr fmt >>
      else if c = '!% then <<
         c := car fmt;
         if null args then a := 'missing_arg
         else a := car args;
         if c = '!v or c = '!V then
            if flagp(a, 'c!:live_across_call) then <<
               princ "stack[";
               princ(-get(a, 'c!:location));
               princ "]" >>
            else princ a
         else if c = '!a or c = '!A then prin a
         else if c = '!t or c = '!T then ttab a
         else princ a;
         if args then args := cdr args;
         fmt := cdr fmt >>
      else princ c >>
  end;

% This establishes a default handler for each special form so that
% any that I forget to treat more directly will cause a tidy error
% if found in compiled code.

symbolic procedure c!:cspecform(x, env);
   error(0, list("special form", x));

<< put('and,                    'c!:code, function c!:cspecform);
!#if common!-lisp!-mode
   put('block,                  'c!:code, function c!:cspecform);
!#endif
   put('catch,                  'c!:code, function c!:cspecform);
   put('compiler!-let,          'c!:code, function c!:cspecform);
   put('cond,                   'c!:code, function c!:cspecform);
   put('declare,                'c!:code, function c!:cspecform);
   put('de,                     'c!:code, function c!:cspecform);
!#if common!-lisp!-mode
   put('defun,                  'c!:code, function c!:cspecform);
!#endif
   put('eval!-when,             'c!:code, function c!:cspecform);
   put('flet,                   'c!:code, function c!:cspecform);
   put('function,               'c!:code, function c!:cspecform);
   put('go,                     'c!:code, function c!:cspecform);
   put('if,                     'c!:code, function c!:cspecform);
   put('labels,                 'c!:code, function c!:cspecform);
!#if common!-lisp!-mode
   put('let,                    'c!:code, function c!:cspecform);
!#else
   put('!~let,                  'c!:code, function c!:cspecform);
!#endif
   put('let!*,                  'c!:code, function c!:cspecform);
   put('list,                   'c!:code, function c!:cspecform);
   put('list!*,                 'c!:code, function c!:cspecform);
   put('macrolet,               'c!:code, function c!:cspecform);
   put('multiple!-value!-call,  'c!:code, function c!:cspecform);
   put('multiple!-value!-prog1, 'c!:code, function c!:cspecform);
   put('or,                     'c!:code, function c!:cspecform);
   put('prog,                   'c!:code, function c!:cspecform);
   put('prog!*,                 'c!:code, function c!:cspecform);
   put('prog1,                  'c!:code, function c!:cspecform);
   put('prog2,                  'c!:code, function c!:cspecform);
   put('progn,                  'c!:code, function c!:cspecform);
   put('progv,                  'c!:code, function c!:cspecform);
   put('quote,                  'c!:code, function c!:cspecform);
   put('return,                 'c!:code, function c!:cspecform);
   put('return!-from,           'c!:code, function c!:cspecform);
   put('setq,                   'c!:code, function c!:cspecform);
   put('tagbody,                'c!:code, function c!:cspecform);
   put('the,                    'c!:code, function c!:cspecform);
   put('throw,                  'c!:code, function c!:cspecform);
   put('unless,                 'c!:code, function c!:cspecform);
   put('unwind!-protect,        'c!:code, function c!:cspecform);
   put('when,                   'c!:code, function c!:cspecform) >>;

fluid '(current_procedure current_args current_block current_contents
        all_blocks registers stacklocs);

fluid '(available used);

available := used := nil;

symbolic procedure c!:reset_gensyms();
 << remflag(used, 'c!:live_across_call);
    remflag(used, 'c!:visited);
    while used do <<
      remprop(car used, 'c!:contents);
      remprop(car used, 'c!:why);
      remprop(car used, 'c!:where_to);
      remprop(car used, 'c!:count);
      remprop(car used, 'c!:live);
      remprop(car used, 'c!:clash);
      remprop(car used, 'c!:chosen);
      remprop(car used, 'c!:location);
      if plist car used then begin
         scalar o; o := wrs nil;
         princ "+++++ "; prin car used; princ " ";
         prin plist car used; terpri();
         wrs o end;
      available := car used . available;
      used := cdr used >> >>;

!#if common!-lisp!-mode

fluid '(my_gensym_counter);
my_gensym_counter := 0;

!#endif

symbolic procedure c!:my_gensym();
  begin
    scalar w;
    if available then << w := car available; available := cdr available >>
!#if common!-lisp!-mode
    else w := compress1
       ('!v . explodec (my_gensym_counter := my_gensym_counter + 1));
!#else
    else w := gensym1 "v";
!#endif
    used := w . used;
    if plist w then << princ "????? "; prin w; princ " => "; prin plist w; terpri() >>;
    return w
  end;

symbolic procedure c!:newreg();
  begin
    scalar r;
    r := c!:my_gensym();
    registers := r . registers;
    return r
  end;

symbolic procedure c!:startblock s;
 << current_block := s;
    current_contents := nil
 >>;

symbolic procedure c!:outop(a,b,c,d);
  if current_block then
     current_contents := list(a,b,c,d) . current_contents;

symbolic procedure c!:endblock(why, where_to);
  if current_block then <<
% Note that the operations within a block are in reversed order.
    put(current_block, 'c!:contents, current_contents);
    put(current_block, 'c!:why, why);
    put(current_block, 'c!:where_to, where_to);
    all_blocks := current_block . all_blocks;
    current_contents := nil;
    current_block := nil >>;

%
% Now for a general driver for compilation
%

symbolic procedure c!:cval_inner(x, env);
  begin
    scalar helper;
% NB use the "improve" function from the regular compiler here...
    x := s!:improve x;
% atoms and embedded lambda expressions need their own treatment.
    if atom x then return c!:catom(x, env)
    else if eqcar(car x, 'lambda) then
       return c!:clambda(cadar x, 'progn . cddar x, cdr x, env)
% a c!:code property gives direct control over compilation
    else if helper := get(car x, 'c!:code) then
       return funcall(helper, x, env)
% compiler-macros take precedence over regular macros, so that I can
% make special expansions in the context of compilation. Only used if the
% expansion is non-nil
    else if (helper := get(car x, 'c!:compile_macro)) and
            (helper := funcall(helper, x)) then
       return c!:cval(helper, env)
% regular Lisp macros get expanded
    else if idp car x and (helper := macro!-function car x) then
       return c!:cval(funcall(helper, x), env)
% anything not recognised as special will be turned into a
% function call, but there will still be special cases, such as
% calls to the current function, calls into the C-coded kernel, etc.
    else return c!:ccall(car x, cdr x, env)
  end;

symbolic procedure c!:cval(x, env);
  begin
     scalar r;
     r := c!:cval_inner(x, env);
     if r and not member!*!*(r, registers) then
        error(0, list(r, "not a register", x));
     return r
  end;

symbolic procedure c!:clambda(bvl, body, args, env);
  begin
    scalar w, fluids, env1;
    env1 := car env;
    w := for each a in args collect c!:cval(a, env);
    for each v in bvl do <<
       if globalp v then begin scalar oo;
           oo := wrs nil;
           princ "+++++ "; prin v;
           princ " converted from GLOBAL to FLUID"; terpri();
           wrs oo;
           unglobal list v;
           fluid list v end;
       if fluidp v then <<
          fluids := (v . c!:newreg()) . fluids;
          flag(list cdar fluids, 'c!:live_across_call); % silly if not
          env1 := ('c!:dummy!:name . cdar fluids) . env1;
          c!:outop('ldrglob, cdar fluids, v, c!:find_literal v);
          c!:outop('strglob, car w, v, c!:find_literal v) >>
       else <<
          env1 := (v . c!:newreg()) . env1;
          c!:outop('movr, cdar env1, nil, car w) >>;
       w := cdr w >>;
    if fluids then c!:outop('fluidbind, nil, nil, fluids);
    env := env1 . append(fluids, cdr env);
    w := c!:cval(body, env);
    for each v in fluids do
       c!:outop('strglob, cdr v, car v, c!:find_literal car v);
    return w
  end;

symbolic procedure c!:locally_bound(x, env);
   atsoc(x, car env);

flag('(nil t), 'c!:constant);

fluid '(literal_vector);

symbolic procedure c!:find_literal x;
  begin
    scalar n, w;
    w := literal_vector;
    n := 0;
    while w and not (car w = x) do <<
      n := n + 1;
      w := cdr w >>;
    if null w then literal_vector := append(literal_vector, list x);
    return n
  end;

symbolic procedure c!:catom(x, env);
  begin
    scalar v, w;
    v := c!:newreg();
    if idp x and (w := c!:locally_bound(x, env)) then
       c!:outop('movr, v, nil, cdr w)
    else if null x or x = 't or c!:small_number x then
       c!:outop('movk1, v, nil, x)
    else if not idp x or flagp(x, 'c!:constant) then
       c!:outop('movk, v, x, c!:find_literal x)
    else c!:outop('ldrglob, v, x, c!:find_literal x);
    return v
  end;

symbolic procedure c!:cjumpif(x, env, d1, d2);
  begin
    scalar helper, r;
    x := s!:improve x;
    if atom x and (not idp x or
         (flagp(x, 'c!:constant) and not c!:locally_bound(x, env))) then
       c!:endblock('goto, list (if x then d1 else d2))
    else if not atom x and (helper := get(car x, 'c!:ctest)) then
       return funcall(helper, x, env, d1, d2)
    else <<
       r := c!:cval(x, env);
       c!:endblock(list('ifnull, r), list(d2, d1)) >>
  end;

fluid '(current);

symbolic procedure c!:ccall(fn, args, env);
  c!:ccall1(fn, args, env);

fluid '(visited);

symbolic procedure c!:has_calls(a, b);
  begin
    scalar visited;
    return c!:has_calls_1(a, b)
  end;

symbolic procedure c!:has_calls_1(a, b);
% true if there is a path from node a to node b that has a call instruction
% on the way.
  if a = b or not atom a or memq(a, visited) then nil
  else begin
    scalar has_call;
    visited := a . visited;
    for each z in get(a, 'c!:contents) do
       if eqcar(z, 'call) then has_call := t;
    if has_call then return
       begin scalar visited;
       return c!:can_reach(a, b) end;
    for each d in get(a, 'c!:where_to) do
       if c!:has_calls_1(d, b) then has_call := t;
    return has_call
  end;

symbolic procedure c!:can_reach(a, b);
  if a = b then t
  else if not atom a or memq(a, visited) then nil
  else <<
    visited := a . visited;
    c!:any_can_reach(get(a, 'c!:where_to), b) >>;

symbolic procedure c!:any_can_reach(l, b);
  if null l then nil
  else if c!:can_reach(car l, b) then t
  else c!:any_can_reach(cdr l, b);
    
symbolic procedure c!:pareval(args, env);
  begin
    scalar tasks, tasks1, merge, split, r;
    tasks := for each a in args collect (c!:my_gensym() . c!:my_gensym());
    split := c!:my_gensym();
    c!:endblock('goto, list split);
    for each a in args do begin
      scalar s;
% I evaluate each arg as what is (at this stage) a separate task
      s := car tasks;
      tasks := cdr tasks;
      c!:startblock car s;
      r := c!:cval(a, env) . r;
      c!:endblock('goto, list cdr s);
% If the task did no procedure calls (or only tail calls) then it can be
% executed sequentially with the other args without need for stacking
% anything.  Otherwise it more care will be needed.  Put the hard
% cases onto tasks1.
!#if common!-lisp!-mode
      tasks1 := s . tasks1
!#else
      if c!:has_calls(car s, cdr s) then tasks1 := s . tasks1
      else merge := s . merge
!#endif
    end;
%-- % if there are zero or one items in tasks1 then again it is easy -
%-- % otherwise I flag the problem with a notionally parallel construction.
%--     if tasks1 then <<
%--        if null cdr tasks1 then merge := car tasks1 . merge
%--        else <<
%--           c!:startblock split;
%--           printc "***** ParEval needed parallel block here...";
%--           c!:endblock('par, for each v in tasks1 collect car v);
%--           split := c!:my_gensym();
%--           for each v in tasks1 do <<
%--              c!:startblock cdr v;
%--              c!:endblock('goto, list split) >> >> >>;
    for each z in tasks1 do merge := z . merge; % do sequentially
%--
%--
% Finally string end-to-end all the bits of sequential code I have left over.
    for each v in merge do <<
      c!:startblock split;
      c!:endblock('goto, list car v);
      split := cdr v >>;
    c!:startblock split;
    return reversip r
  end;

symbolic procedure c!:ccall1(fn, args, env);
  begin
    scalar tasks, merge, r, val;
    fn := list(fn, cdr env);
    val := c!:newreg();
    if null args then c!:outop('call, val, nil, fn)
    else if null cdr args then
      c!:outop('call, val, list c!:cval(car args, env), fn)
    else <<
      r := c!:pareval(args, env);
      c!:outop('call, val, r, fn) >>;
    c!:outop('reloadenv, 'env, nil, nil);
    return val
  end;
      
fluid '(restart_label reloadenv does_call current_c_name);

%
% The "proper" recipe here arranges that functions that expect over 2 args use
% the "va_arg" mechanism to pick up ALL their args.  This would be pretty
% heavy-handed, and at least on a lot of machines it does not seem to
% be necessary.  I will duck it for a while more at least.
%

fluid '(proglabs blockstack);

symbolic procedure c!:cfndef(current_procedure, current_c_name, argsbody);
  begin
    scalar env, n, w, current_args, current_block, restart_label,
           current_contents, all_blocks, entrypoint, exitpoint, args1,
           registers, stacklocs, literal_vector, reloadenv, does_call,
           blockstack, proglabs, checksum, args, body;
    checksum := md60 argsbody;
    args := car argsbody;
    body := cdr argsbody;
    if atom body then body := nil
    else if atom cdr body then body := car body
    else body := 'progn . body;
% normally comment out the next line...
    print list(current_procedure, current_c_name, args, body);
    c!:reset_gensyms();
    wrs C_file;
    linelength 200;
    c!:printf("\n\n/* Code for %a */\n\n", current_procedure);

    c!:find_literal current_procedure; % For benefit of backtraces
%
% cope with fluid vars in an argument list by mapping the definition
%    (de f (a B C d) body)     B and C fluid
% onto
%    (de f (a x y c) (prog (B C) (setq B x) (setq C y) (return body)))
% so that the fluids get bound by PROG.
%
    current_args := args;
    for each v in args do
       if v = '!&optional or v = '!&rest then
          error(0, "&optional and &rest not supported by this compiler (yet)")
       else if globalp v then begin scalar oo;
          oo := wrs nil;
          princ "+++++ "; prin v;
          princ " converted from GLOBAL to FLUID"; terpri();
          wrs oo;
          unglobal list v;
          fluid list v;
          n := (v . c!:my_gensym()) . n end
       else if fluidp v then n := (v . c!:my_gensym()) . n;
% Local fluid/special declarations are not coped with here... as in
% (DECLARE (SPECIAL ...)) at the head of the body.
    if !*r2i then body := s!:r2i(current_procedure, args, body);
    restart_label := c!:my_gensym();
    body := list('c!:private_tagbody, restart_label, body);
    if n then <<
       body := list list('return, body);
       args := subla(n, args);
       for each v in n do
         body := list('setq, car v, cdr v) . body;
       body := 'prog . (for each v in reverse n collect car v) . body >>;
    c!:printf "static Lisp_Object ";
    if null args or length args >= 3 then c!:printf("MS_CDECL ");
    c!:printf("%s(Lisp_Object env", current_c_name);
    if null args or length args >= 3 then c!:printf(", int nargs");
    n := t;
    env := nil;
    for each x in args do begin
       scalar aa;
       c!:printf ",";
       if n then << c!:printf "\n                        "; n := nil >>
       else n := t;
       aa := c!:my_gensym();
       env := (x . aa) . env;
       registers := aa . registers;
       args1 := aa . args1;
       c!:printf(" Lisp_Object %s", aa) end;
    if null args or length args >= 3 then c!:printf(", ...");
    c!:printf(")\n{\n");
    c!:startblock (entrypoint := c!:my_gensym());
    exitpoint := current_block;
    c!:endblock('goto, list list c!:cval(body, env . nil));

    c!:optimise_flowgraph(entrypoint, all_blocks, env,
                        length args . current_procedure, args1);

    c!:printf("}\n\n");
    wrs O_file;

    L_contents := (current_procedure . literal_vector . checksum) .
                  L_contents;
    return nil
  end;

% c!:ccompile1 directs the compilation of a single function, and bind all the
% major fluids used by the compilation process

flag('(rds deflist flag fluid global
       remprop remflag unfluid
       unglobal dm carcheck C!-end), 'eval);

flag('(rds), 'ignore);

fluid '(!*backtrace);

symbolic procedure c!:ccompilesupervisor;
  begin
    scalar u, w;
top:u := errorset('(read), t, !*backtrace);
    if atom u then return;      % failed, or maybe EOF
    u := car u;
    if u = !$eof!$ then return; % end of file
    if atom u then go to top
% the apply('C!-end, nil) is here because C!-end has a "stat"
% property and so it will mis-parse if I just write "C!-end()".  Yuk.
    else if eqcar(u, 'C!-end) then return apply('C!-end, nil)
    else if eqcar(u, 'rdf) then <<
!#if common!-lisp!-mode
       w := open(u := eval cadr u, !:direction, !:input,
                 !:if!-does!-not!-exist, nil);
!#else
       w := open(u := eval cadr u, 'input);
!#endif
       if w then <<
          terpri();
          princ "Reading file "; print u;
          w := rds w;
          c!:ccompilesupervisor();
          princ "End of file "; print u;
          close rds w >>
       else << princ "Failed to open file "; print u >> >>
    else c!:ccmpout1 u;
    go to top
  end;

global '(c!:char_mappings);

c!:char_mappings := '(
  (!  . !A)  (!! . !B)  (!# . !C)  (!$ . !D)
  (!% . !E)  (!^ . !F)  (!& . !G)  (!* . !H)
  (!( . !I)  (!) . !J)  (!- . !K)  (!+ . !L)
  (!= . !M)  (!\ . !N)  (!| . !O)  (!, . !P)
  (!. . !Q)  (!< . !R)  (!> . !S)  (!: . !T)
  (!; . !U)  (!/ . !V)  (!? . !W)  (!~ . !X)
  (!` . !Y));

symbolic procedure c!:inv_name n;
  begin
    scalar r, w;
    r := '(_ !C !C !");
!#if common!-lisp!-mode
    for each c in explode2 package!-name symbol!-package n do <<
      if c = '_ then r := '_ . r
      else if alpha!-char!-p c or digit c then r := c . r
      else if w := atsoc(c, c!:char_mappings) then r := cdr w . r
      else r := '!Z . r >>;
    r := '!_ . '!_ . r;
!#endif
    for each c in explode2 n do <<
      if c = '_ then r := '_ . r
!#if common!-lisp!-mode
      else if alpha!-char!-p c or digit c then r := c . r
!#else
      else if liter c or digit c then r := c . r
!#endif
      else if w := atsoc(c, c!:char_mappings) then r := cdr w . r
      else r := '!Z . r >>;
    r := '!" . r;
!#if common!-lisp!-mode
    return compress1 reverse r
!#else
    return compress reverse r
!#endif
  end;

fluid '(defnames pending_functions);

symbolic procedure c!:ccmpout1 u;
  begin
    scalar pending_functions;
    pending_functions := list u;
    while pending_functions do <<
       u := car pending_functions;
       pending_functions := cdr pending_functions;
       c!:ccmpout1a u >>
  end;

symbolic procedure c!:ccmpout1a u;
  begin
    scalar w;
    if atom u then return
    else if eqcar(u, 'progn) then <<
       for each v in cdr u do c!:ccmpout1a v;
       return >>
    else if eqcar(u, 'C!-end) then nil
    else if flagp(car u, 'eval) or
          (car u = 'setq and not atom caddr u and flagp(caaddr u, 'eval)) then
       errorset(u, t, !*backtrace);
    if eqcar(u, 'rdf) then begin
!#if common!-lisp!-mode
       w := open(u := eval cadr u, !:direction, !:input,
                 !:if!-does!_not!-exist, nil);
!#else
       w := open(u := eval cadr u, 'input);
!#endif
       if w then <<
          princ "Reading file "; print u;
          w := rds w;
          c!:ccompilesupervisor();
          princ "End of file "; print u;
          close rds w >>
       else << princ "Failed to open file "; print u >> end
!#if common!-lisp!-mode
    else if eqcar(u, 'defun) then return c!:ccmpout1a macroexpand u
!#endif
    else if eqcar(u, 'de) then <<
        u := cdr u;
!#if common!-lisp!-mode
        w := compress1 ('!" . append(explodec package!-name
                                       symbol!-package car u,
                        '!@ . '!@ . append(explodec symbol!-name car u,
                        append(explodec "@@Builtin", '(!")))));
        w := intern w;
        defnames := list(car u, c!:inv_name car u, length cadr u, w) . defnames;
!#else
        defnames := list(car u, c!:inv_name car u, length cadr u) . defnames;
!#endif
        if posn() neq 0 then terpri();
        princ "Compiling "; prin caar defnames; princ " ... ";
        c!:cfndef(caar defnames, cadar defnames, cdr u);
!#if common!-lisp!-mode
        L_contents := (w . car L_contents) . cdr L_contents;
!#endif
        terpri() >>
  end;

fluid '(!*defn dfprint!* dfprintsave);

!#if common!-lisp!-mode
symbolic procedure c!:concat(a, b);
   compress1('!" . append(explode2 a, append(explode2 b, '(!"))));
!#else
symbolic procedure c!:concat(a, b);
   compress('!" . append(explode2 a, append(explode2 b, '(!"))));
!#endif

symbolic procedure c!:ccompilestart(name, !&optional, dir);
  begin
    scalar o, d, w;
    File_name := name;
    if dir then name := c!:concat(dir, c!:concat("/", name));
!#if common!-lisp!-mode
    C_file := open(c!:concat(name, ".c"), !:direction, !:output);
!#else
    C_file := open(c!:concat(name, ".c"), 'output);
!#endif
    L_file := c!:concat(name, ".lsp");
    L_contents := nil;
% Here I turn a date into a form like "12-Oct-1993" as expected by the
% file signature mechanism that I use. This seems a pretty ugly process.
    o := reverse explode date();
    for i := 1:5 do << d := car o . d; o := cdr o >>;
    d := '!- . d;
    o := cdddr cdddr cddddr o;
    w := o;
    o := cdddr o;
    d := caddr o . cadr o . car o . d;
!#if common!-lisp!-mode
    d := compress1('!" . cadr w . car w . '!- . d);
!#else
    d := compress('!" . cadr w . car w . '!- . d);
!#endif
    O_file := wrs C_file;
    defnames := nil;
    c!:printf("\n/* %s.c%tMachine generated C code */\n\n", name, 25);
    c!:printf("/* Signature: 00000000 %s */\n\n", d);

%   c!:printf "#include <stdio.h>\n";   Included by "machine.h"
%   c!:printf "#include <stdlib.h>\n"; 
    c!:printf "#include <stdarg.h>\n"; 
    c!:printf "#include <string.h>\n"; 
%   c!:printf "#include <time.h>\n"; 
    c!:printf "#include <ctype.h>\n\n"; 
    c!:printf "#include \qmachine.h\q\n";  
    c!:printf "#include \qtags.h\q\n";  
    c!:printf "#include \qcslerror.h\q\n";  
    c!:printf "#include \qexterns.h\q\n";
    c!:printf "#include \qarith.h\q\n";
    c!:printf "#include \qentries.h\q\n\n\n";
    wrs O_file;
    return nil
  end;

symbolic procedure C!-end;
  begin
    scalar checksum, c1, c2, c3;
    wrs C_file;
    c!:printf("\n\nsetup_type const %s_setup[] =\n{\n", File_name);
    defnames := reverse defnames;
    while defnames do begin
       scalar name, nargs, f1, f2, cast, fn;
!#if common!-lisp!-mode
       name := cadddr car defnames;
!#else
       name := caar defnames;
!#endif
       f1 := cadar defnames;
       nargs := caddar defnames;
       cast := "(n_args *)";
       if nargs = 1 then <<
          f2 := '!t!o!o_!m!a!n!y_1; cast := ""; fn := '!w!r!o!n!g_!n!o_1 >>
       else if nargs = 2 then <<
          f2 := f1; f1 := '!t!o!o_!f!e!w_2; cast := "";
          fn := '!w!r!o!n!g_!n!o_2 >>
       else << fn := f1; f1 := '!w!r!o!n!g_!n!o_!n!a;
               f2 := '!w!r!o!n!g_!n!o_!n!b >>;
       c!:printf("    {\q%s\q,%t%s,%t%s,%t%s%s},\n",
          name, 32, f1, 48, f2, 63, cast, fn);
       defnames := cdr defnames end;
    c3 := checksum := md60 L_contents;
    c1 := remainder(c3, 10000000);
    c3 := c3 / 10000000;
    c2 := remainder(c3, 10000000);
    c3 := c3 / 10000000;
    checksum := list!-to!-string append(explodec c3,
                     '!  . append(explodec c2, '!  . explodec c1));
    c!:printf("    {NULL, (one_args *)%a, (two_args *)%a, 0}\n};\n\n",
              list!-to!-string explodec File_name, checksum);
    c!:printf "/* end of generated code */\n";
    close C_file;
!#if common!-lisp!-mode
    L_file := open(L_file, !:direction, !:output);
!#else
    L_file := open(L_file, 'output);
!#endif
    wrs L_file;
    linelength 72;
    terpri();
!#if common!-lisp!-mode
    princ ";;;  "; 
!#else
    princ "% ";
!#endif
    princ File_name;
    princ ".lsp"; ttab 20;
    princ "Machine generated Lisp";
%   princ "  "; princ date();  % I omit the date now because it makes
                               % file comparisons messier
    terpri(); terpri();
!#if common!-lisp!-mode
    princ "(in-package lisp)"; terpri(); terpri();
    princ "(c::install ";
!#else
    princ "(c!:install ";
!#endif
    princ '!"; princ File_name; princ '!";
    princ " "; princ checksum; printc ")";
    terpri();
    for each x in reverse L_contents do <<
!#if common!-lisp!-mode
       princ "(c::install '";
       prin car x;
       princ " '";
       x := cdr x;
!#else
       princ "(c!:install '";
!#endif
       prin car x;
       princ " '";
       prin cadr x;
!#if (not common!-lisp!-mode)
       princ " ";
       prin cddr x;
!#endif
       princ ")";
       terpri(); terpri() >>;
    terpri();
!#if common!-lisp!-mode
    princ ";;; End of generated Lisp code";
!#else
    princ "% End of generated Lisp code";
!#endif
    terpri(); terpri();
    L_contents := nil;
    wrs O_file;
    close L_file;
    !*defn := nil;
    dfprint!* := dfprintsave
  end;

put('C!-end, 'stat, 'endstat);

symbolic procedure C!-compile u;
  begin
    terpri();
    princ "C!-COMPILE ";
    prin u; princ ": IN files;  or type in expressions"; terpri();
    princ "When all done, execute C!-END;"; terpri();
    verbos nil;
    c!:ccompilestart car u;
    dfprintsave := dfprint!*;
    dfprint!* := 'c!:ccmpout1;
    !*defn := t;
    if getd 'begin then return nil;
    c!:ccompilesupervisor();
  end;

put('C!-compile, 'stat, 'rlis);

%
% Global treatment of a flow-graph...
%

symbolic procedure c!:print_opcode(s, depth);
  begin
    scalar op, r1, r2, r3, helper;
    op := car s; r1 := cadr s; r2 := caddr s; r3 := cadddr s;
    helper := get(op, 'c!:opcode_printer);
    if helper then funcall(helper, op, r1, r2, r3, depth)
    else << prin s; terpri() >>
  end;

symbolic procedure c!:print_exit_condition(why, where_to, depth);
  begin
    scalar helper, lab1, drop1, lab2, drop2, negate;
% An exit condition is one of
%     goto          (lab)
%     goto          ((return-register))
%     (ifnull v)    (lab1 lab2)    ) etc, where v is a register and
%     (ifatom v)    (lab1 lab2)    ) lab1, lab2 are labels for true & false
%     (ifeq v1 v2)  (lab1 lab2)    ) and various predicates are supported
%     ((call fn) a1 a2) ()         tail-call to given function
%
    if why = 'goto then <<
       where_to := car where_to;
       if atom where_to then <<
          c!:printf("    goto %s;\n", where_to);
          c!:display_flowgraph(where_to, depth, t) >>
       else << c!:printf "    "; c!:pgoto(where_to, depth) >>;
       return nil >>
    else if eqcar(car why, 'call) then return begin
       scalar args, locs, g, w;
       if w := get(cadar why, 'c!:direct_entrypoint) then <<
          for each a in cdr why do
            if flagp(a, 'c!:live_across_call) then <<
               if null g then c!:printf "    {\n";
               g := c!:my_gensym();
               c!:printf("        Lisp_Object %s = %v;\n", g, a);
               args := g . args >>
            else args := a . args;
          if depth neq 0 then <<
              if g then c!:printf "    ";
              c!:printf("    popv(%s);\n", depth) >>;
          if g then c!:printf "    ";
!#if common!-lisp!-mode
          c!:printf("    { Lisp_Object retVal = %s(", cdr w);
!#else
          c!:printf("    return %s(", cdr w);
!#endif
          args := reversip args;
          if args then <<
             c!:printf("%v", car args);
             for each a in cdr args do c!:printf(", %v", a) >>;
          c!:printf(");\n");
!#if common!-lisp!-mode
          if g then c!:printf "    ";
          c!:printf("    errexit();\n");
          if g then c!:printf "    ";
          c!:printf("    return onevalue(retVal); }\n");
!#endif
          if g then c!:printf "    }\n" >>
       else if w := get(cadar why, 'c!:c_entrypoint) then <<
% I think there may be an issue here with functions that can accept variable
% numbers of args. I seem to support just ONE C entrypoint which I will
% call in all circumstances...
          for each a in cdr why do
            if flagp(a, 'c!:live_across_call) then <<
               if null g then c!:printf "    {\n";
               g := c!:my_gensym();
               c!:printf("        Lisp_Object %s = %v;\n", g, a);
               args := g . args >>
            else args := a . args;
          if depth neq 0 then c!:printf("        popv(%s);\n", depth);
          c!:printf("        return %s(nil", w);
          if null args or length args >= 3 then c!:printf(", %s", length args);
          for each a in reversip args do c!:printf(", %v", a);
          c!:printf(");\n");
          if g then c!:printf "    }\n" >>
       else begin
          scalar nargs;
          nargs := length cdr why;
          c!:printf "    {\n";
          for each a in cdr why do
            if flagp(a, 'c!:live_across_call) then <<
               g := c!:my_gensym();
               c!:printf("        Lisp_Object %s = %v;\n", g, a);
               args := g . args >>
            else args := a . args;
          if depth neq 0 then c!:printf("        popv(%s);\n", depth);
          c!:printf("        fn = elt(env, %s); /* %a */\n",
                    c!:find_literal cadar why, cadar why);
          if nargs = 1 then c!:printf("        return (*qfn1(fn))(qenv(fn)")
          else if nargs = 2 then c!:printf("        return (*qfn2(fn))(qenv(fn)")
          else c!:printf("        return (*qfnn(fn))(qenv(fn), %s", nargs);
          for each a in reversip args do c!:printf(", %s", a);
          c!:printf(");\n    }\n") end;
       return nil end;
    lab1 := car where_to;
    drop1 := atom lab1 and not flagp(lab1, 'c!:visited);
    lab2 := cadr where_to;
    drop2 := atom lab2 and not flagp(drop2, 'c!:visited);
    if drop2 and get(lab2, 'c!:count) = 1 then <<
       where_to := list(lab2, lab1);
       drop1 := t >>
    else if drop1 then negate := t;
    helper := get(car why, 'c!:exit_helper);
    if null helper then error(0, list("Bad exit condition", why));
    c!:printf("    if (");
    if negate then <<
       c!:printf("!(");
       funcall(helper, cdr why, depth);
       c!:printf(")") >>
    else funcall(helper, cdr why, depth);
    c!:printf(") ");
    if not drop1 then <<
       c!:pgoto(car where_to, depth);
       c!:printf("    else ") >>;
    c!:pgoto(cadr where_to, depth);
    if atom car where_to then c!:display_flowgraph(car where_to, depth, drop1);
    if atom cadr where_to then c!:display_flowgraph(cadr where_to, depth, nil)
  end;

symbolic procedure c!:pmovr(op, r1, r2, r3, depth);
   c!:printf("    %v = %v;\n", r1, r3);

put('movr, 'c!:opcode_printer, function c!:pmovr);

symbolic procedure c!:pmovk(op, r1, r2, r3, depth);
   c!:printf("    %v = elt(env, %s); /* %a */\n", r1, r3, r2);

put('movk, 'c!:opcode_printer, function c!:pmovk);

symbolic procedure c!:pmovk1(op, r1, r2, r3, depth);
   if null r3 then c!:printf("    %v = nil;\n", r1)
   else if r3 = 't then c!:printf("    %v = lisp_true;\n", r1)
   else c!:printf("    %v = (Lisp_Object)%s; /* %a */\n", r1, 16*r3+1, r3);

put('movk1, 'c!:opcode_printer, function c!:pmovk1);

symbolic procedure c!:preloadenv(op, r1, r2, r3, depth);
% will not be encountered unless reloadenv variable has been set up.
   c!:printf("    env = stack[%s];\n", -reloadenv);

put('reloadenv, 'c!:opcode_printer, function c!:preloadenv);

symbolic procedure c!:pldrglob(op, r1, r2, r3, depth);
   c!:printf("    %v = qvalue(elt(env, %s)); /* %a */\n", r1, r3, r2);

put('ldrglob, 'c!:opcode_printer, function c!:pldrglob);

symbolic procedure c!:pstrglob(op, r1, r2, r3, depth);
   c!:printf("    qvalue(elt(env, %s)) = %v; /* %a */\n", r3, r1, r2);

put('strglob, 'c!:opcode_printer, function c!:pstrglob);

symbolic procedure c!:pnilglob(op, r1, r2, r3, depth);
   c!:printf("    qvalue(elt(env, %s)) = nil; /* %a */\n", r3, r2);

put('nilglob, 'c!:opcode_printer, function c!:pnilglob);

symbolic procedure c!:pnull(op, r1, r2, r3, depth);
   c!:printf("    %v = (%v == nil ? lisp_true : nil);\n", r1, r3);

put('null, 'c!:opcode_printer, function c!:pnull);
put('not,  'c!:opcode_printer, function c!:pnull);
flag('(null not), 'c!:uses_nil);

symbolic procedure c!:pfastget(op, r1, r2, r3, depth);
 <<
   c!:printf("    if (!symbolp(%v)) %v = nil;\n", r2, r1);
   c!:printf("    else { %v = qfastgets(%v);\n", r1, r2);
   c!:printf("           if (%v != nil) { %v = elt(%v, %s); /* %s */\n",
                                       r1, r1, r1, car r3, cdr r3);
   c!:printf("#ifdef RECORD_GET\n");
   c!:printf("             if (%v != SPID_NOPROP)\n", r1);
   c!:printf("                record_get(elt(fastget_names, %s), 1);\n", car r3);
   c!:printf("             else record_get(elt(fastget_names, %s), 0),\n", car r3);
   c!:printf("                %v = nil; }\n", r1);
   c!:printf("           else record_get(elt(fastget_names, %s), 0); }\n", car r3);
   c!:printf("#else\n");
   c!:printf("             if (%v == SPID_NOPROP) %v = nil; }}\n", r1, r1);
   c!:printf("#endif\n");
  >>;

put('fastget, 'c!:opcode_printer, function c!:pfastget);
flag('(fastget), 'c!:uses_nil);

symbolic procedure c!:pfastflag(op, r1, r2, r3, depth);
 <<
   c!:printf("    if (!symbolp(%v)) %v = nil;\n", r2, r1);
   c!:printf("    else { %v = qfastgets(%v);\n", r1, r2);
   c!:printf("           if (%v != nil) { %v = elt(%v, %s); /* %s */\n",
                                       r1, r1, r1, car r3, cdr r3);
   c!:printf("#ifdef RECORD_GET\n");
   c!:printf("             if (%v == SPID_NOPROP)\n", r1);
   c!:printf("                record_get(elt(fastget_names, %s), 0),\n", car r3);
   c!:printf("                %v = nil;\n", r1);
   c!:printf("             else record_get(elt(fastget_names, %s), 1),\n", car r3);
   c!:printf("                %v = lisp_true; }\n", r1);
   c!:printf("           else record_get(elt(fastget_names, %s), 0); }\n", car r3);
   c!:printf("#else\n");
   c!:printf("             if (%v == SPID_NOPROP) %v = nil; else %v = lisp_true; }}\n", r1, r1, r1);
   c!:printf("#endif\n");
  >>;

put('fastflag, 'c!:opcode_printer, function c!:pfastflag);
flag('(fastflag), 'c!:uses_nil);

symbolic procedure c!:pcar(op, r1, r2, r3, depth);
  begin
    if not !*unsafecar then <<
        c!:printf("    if (!car_legal(%v)) ", r3);
        c!:pgoto(c!:find_error_label(list('car, r3), r2, depth), depth) >>;
    c!:printf("    %v = qcar(%v);\n", r1, r3)
  end;

put('car, 'c!:opcode_printer, function c!:pcar);

symbolic procedure c!:pcdr(op, r1, r2, r3, depth);
  begin
    if not !*unsafecar then <<
        c!:printf("    if (!car_legal(%v)) ", r3);
        c!:pgoto(c!:find_error_label(list('cdr, r3), r2, depth), depth) >>;
    c!:printf("    %v = qcdr(%v);\n", r1, r3)
  end;

put('cdr, 'c!:opcode_printer, function c!:pcdr);

symbolic procedure c!:pqcar(op, r1, r2, r3, depth);
    c!:printf("    %v = qcar(%v);\n", r1, r3);

put('qcar, 'c!:opcode_printer, function c!:pqcar);

symbolic procedure c!:pqcdr(op, r1, r2, r3, depth);
    c!:printf("    %v = qcdr(%v);\n", r1, r3);

put('qcdr, 'c!:opcode_printer, function c!:pqcdr);

symbolic procedure c!:patom(op, r1, r2, r3, depth);
   c!:printf("    %v = (!consp(%v) ? lisp_true : nil);\n", r1, r3);

put('atom, 'c!:opcode_printer, function c!:patom);

symbolic procedure c!:pnumberp(op, r1, r2, r3, depth);
   c!:printf("    %v = (is_number(%v) ? lisp_true : nil);\n", r1, r3);

put('numberp, 'c!:opcode_printer, function c!:pnumberp);

symbolic procedure c!:pfixp(op, r1, r2, r3, depth);
   c!:printf("    %v = integerp(%v);\n", r1, r3);

put('fixp, 'c!:opcode_printer, function c!:pfixp);

symbolic procedure c!:piminusp(op, r1, r2, r3, depth);
   c!:printf("    %v = ((int32)(%v) < 0 ? lisp_true : nil);\n", r1, r3);

put('iminusp, 'c!:opcode_printer, function c!:piminusp);

symbolic procedure c!:pilessp(op, r1, r2, r3, depth);
   c!:printf("    %v = ((int32)%v < (int32)%v) ? lisp_true : nil;\n",
             r1, r2, r3);

put('ilessp, 'c!:opcode_printer, function c!:pilessp);

symbolic procedure c!:pigreaterp(op, r1, r2, r3, depth);
   c!:printf("    %v = ((int32)%v > (int32)%v) ? lisp_true : nil;\n",
             r1, r2, r3);

put('igreaterp, 'c!:opcode_printer, function c!:pigreaterp);

symbolic procedure c!:piminus(op, r1, r2, r3, depth);
   c!:printf("    %v = (Lisp_Object)(2-((int32)(%v)));\n", r1, r3);

put('iminus, 'c!:opcode_printer, function c!:piminus);

symbolic procedure c!:piadd1(op, r1, r2, r3, depth);
   c!:printf("    %v = (Lisp_Object)((int32)(%v) + 0x10);\n", r1, r3);

put('iadd1, 'c!:opcode_printer, function c!:piadd1);

symbolic procedure c!:pisub1(op, r1, r2, r3, depth);
   c!:printf("    %v = (Lisp_Object)((int32)(%v) - 0x10);\n", r1, r3);

put('isub1, 'c!:opcode_printer, function c!:pisub1);

symbolic procedure c!:piplus2(op, r1, r2, r3, depth);
   c!:printf("    %v = (Lisp_Object)((int32)%v + (int32)%v - TAG_FIXNUM);\n",
               r1, r2, r3);

put('iplus2, 'c!:opcode_printer, function c!:piplus2);

symbolic procedure c!:pidifference(op, r1, r2, r3, depth);
   c!:printf("    %v = (Lisp_Object)((int32)%v - (int32)%v + TAG_FIXNUM);\n",
               r1, r2, r3);

put('idifference, 'c!:opcode_printer, function c!:pidifference);

symbolic procedure c!:pitimes2(op, r1, r2, r3, depth);
   c!:printf("    %v = fixnum_of_int(int_of_fixnum(%v) * int_of_fixnum(%v));\n",
               r1, r2, r3);

put('itimes2, 'c!:opcode_printer, function c!:pitimes2);

symbolic procedure c!:pmodular_plus(op, r1, r2, r3, depth);
 <<
    c!:printf("    {   int32 w = int_of_fixnum(%v) + int_of_fixnum(%v);\n",
                    r2, r3);
    c!:printf("        if (w >= current_modulus) w -= current_modulus;\n");
    c!:printf("        %v = fixnum_of_int(w);\n", r1);
    c!:printf("    }\n")
 >>;

put('modular!-plus, 'c!:opcode_printer, function c!:pmodular_plus);

symbolic procedure c!:pmodular_difference(op, r1, r2, r3, depth);
 <<
    c!:printf("    {   int32 w = int_of_fixnum(%v) - int_of_fixnum(%v);\n",
                    r2, r3);
    c!:printf("        if (w < 0) w += current_modulus;\n");
    c!:printf("        %v = fixnum_of_int(w);\n", r1);
    c!:printf("    }\n")
 >>;

put('modular!-difference, 'c!:opcode_printer, function c!:pmodular_difference);

symbolic procedure c!:pmodular_minus(op, r1, r2, r3, depth);
 <<
    c!:printf("    {   int32 w = int_of_fixnum(%v);\n", r3);
    c!:printf("        if (w != 0) w = current_modulus - w;\n");
    c!:printf("        %v = fixnum_of_int(w);\n", r1);
    c!:printf("    }\n")
 >>;

put('modular!-minus, 'c!:opcode_printer, function c!:pmodular_minus);

!#if (not common!-lisp!-mode)

symbolic procedure c!:passoc(op, r1, r2, r3, depth);
   c!:printf("    %v = Lassoc(nil, %v, %v);\n", r1, r2, r3);

put('assoc, 'c!:opcode_printer, function c!:passoc);
flag('(assoc), 'c!:uses_nil);

!#endif

symbolic procedure c!:patsoc(op, r1, r2, r3, depth);
   c!:printf("    %v = Latsoc(nil, %v, %v);\n", r1, r2, r3);

put('atsoc, 'c!:opcode_printer, function c!:patsoc);
flag('(atsoc), 'c!:uses_nil);

!#if (not common!-lisp!-mode)

symbolic procedure c!:pmember(op, r1, r2, r3, depth);
   c!:printf("    %v = Lmember(nil, %v, %v);\n", r1, r2, r3);

put('member, 'c!:opcode_printer, function c!:pmember);
flag('(member), 'c!:uses_nil);

!#endif

symbolic procedure c!:pmemq(op, r1, r2, r3, depth);
   c!:printf("    %v = Lmemq(nil, %v, %v);\n", r1, r2, r3);

put('memq, 'c!:opcode_printer, function c!:pmemq);
flag('(memq), 'c!:uses_nil);

!#if common!-lisp!-mode

symbolic procedure c!:pget(op, r1, r2, r3, depth);
   c!:printf("    %v = get(%v, %v, nil);\n", r1, r2, r3);

flag('(get), 'c!:uses_nil);
!#else

symbolic procedure c!:pget(op, r1, r2, r3, depth);
   c!:printf("    %v = get(%v, %v);\n", r1, r2, r3);

!#endif

put('get, 'c!:opcode_printer, function c!:pget);

symbolic procedure c!:pqgetv(op, r1, r2, r3, depth);
 << c!:printf("    %v = *(Lisp_Object *)((char *)%v + (CELL-TAG_VECTOR) +",
              r1, r2);
    c!:printf(" ((int32)%v/(16/CELL)));\n", r3) >>;

put('qgetv, 'c!:opcode_printer, function c!:pqgetv);

symbolic procedure c!:pqputv(op, r1, r2, r3, depth);
 <<
  c!:printf("    *(Lisp_Object *)((char *)%v + (CELL-TAG_VECTOR) +", r2);
  c!:printf(" ((int32)%v/(16/CELL))) = %v;\n", r3, r1) >>;

put('qputv, 'c!:opcode_printer, function c!:pqputv);

symbolic procedure c!:peq(op, r1, r2, r3, depth);
   c!:printf("    %v = (%v == %v ? lisp_true : nil);\n", r1, r2, r3);

put('eq, 'c!:opcode_printer, function c!:peq);
flag('(eq), 'c!:uses_nil);

!#if common!-lisp!-mode
symbolic procedure c!:pequal(op, r1, r2, r3, depth);
   c!:printf("    %v = (cl_equal(%v, %v) ? lisp_true : nil);\n",
      r1, r2, r3, r2, r3);
!#else
symbolic procedure c!:pequal(op, r1, r2, r3, depth);
   c!:printf("    %v = (equal(%v, %v) ? lisp_true : nil);\n",
      r1, r2, r3, r2, r3);
!#endif

put('equal, 'c!:opcode_printer, function c!:pequal);
flag('(equal), 'c!:uses_nil);

symbolic procedure c!:pfluidbind(op, r1, r2, r3, depth);
   nil;

put('fluidbind, 'c!:opcode_printer, function c!:pfluidbind);

symbolic procedure c!:pcall(op, r1, r2, r3, depth);
  begin
% r3 is (name <fluids to unbind on error>)
    scalar w, boolfn;
    if w := get(car r3, 'c!:direct_entrypoint) then <<
       c!:printf("    %v = %s(", r1, cdr w);
       if r2 then <<
          c!:printf("%v", car r2);
          for each a in cdr r2 do c!:printf(", %v", a) >>;
       c!:printf(");\n") >>
    else if w := get(car r3, 'c!:direct_predicate) then <<
       boolfn := t;
       c!:printf("    %v = (Lisp_Object)%s(", r1, cdr w);
       if r2 then <<
          c!:printf("%v", car r2);
          for each a in cdr r2 do c!:printf(", %v", a) >>;
       c!:printf(");\n") >>
    else if car r3 = current_procedure then <<
       c!:printf("    %v = %s(env", r1, current_c_name);
       if null r2 or length r2 >= 3 then c!:printf(", %s", length r2);
       for each a in r2 do c!:printf(", %v", a);
       c!:printf(");\n") >>
    else if w := get(car r3, 'c!:c_entrypoint) then <<
       c!:printf("    %v = %s(nil", r1, w);
       if null r2 or length r2 >= 3 then c!:printf(", %s", length r2);
       for each a in r2 do c!:printf(", %v", a);
       c!:printf(");\n") >>
    else begin
       scalar nargs;
       nargs := length r2;
       c!:printf("    fn = elt(env, %s); /* %a */\n",
              c!:find_literal car r3, car r3);
       if nargs = 1 then c!:printf("    %v = (*qfn1(fn))(qenv(fn)", r1)
       else if nargs = 2 then c!:printf("    %v = (*qfn2(fn))(qenv(fn)", r1)
       else c!:printf("    %v = (*qfnn(fn))(qenv(fn), %s", r1, nargs);
       for each a in r2 do c!:printf(", %v", a);
       c!:printf(");\n") end;
    if not flagp(car r3, 'c!:no_errors) then <<
       if null cadr r3 and depth = 0 then c!:printf("    errexit();\n")
       else <<
           c!:printf("    nil = C_nil;\n");
           c!:printf("    if (exception_pending()) ");
           c!:pgoto(c!:find_error_label(nil, cadr r3, depth) , depth) >> >>;
    if boolfn then c!:printf("    %v = %v ? lisp_true : nil;\n", r1, r1);
  end;

put('call, 'c!:opcode_printer, function c!:pcall);

symbolic procedure c!:pgoto(lab, depth);
  begin
    if atom lab then return c!:printf("goto %s;\n", lab);
    lab := get(car lab, 'c!:chosen);
    if zerop depth then c!:printf("return onevalue(%v);\n", lab)
    else if flagp(lab, 'c!:live_across_call) then
      c!:printf("{ Lisp_Object res = %v; popv(%s); return onevalue(res); }\n", lab, depth)
    else c!:printf("{ popv(%s); return onevalue(%v); }\n", depth, lab)
  end;

symbolic procedure c!:pifnull(s, depth);
  c!:printf("%v == nil", car s);

put('ifnull, 'c!:exit_helper, function c!:pifnull);

symbolic procedure c!:pifatom(s, depth);
  c!:printf("!consp(%v)", car s);

put('ifatom, 'c!:exit_helper, function c!:pifatom);

symbolic procedure c!:pifsymbol(s, depth);
  c!:printf("symbolp(%v)", car s);

put('ifsymbol, 'c!:exit_helper, function c!:pifsymbol);

symbolic procedure c!:pifnumber(s, depth);
  c!:printf("is_number(%v)", car s);

put('ifnumber, 'c!:exit_helper, function c!:pifnumber);

symbolic procedure c!:pifizerop(s, depth);
  c!:printf("(%v) == 1", car s);

put('ifizerop, 'c!:exit_helper, function c!:pifizerop);

symbolic procedure c!:pifeq(s, depth);
  c!:printf("%v == %v", car s, cadr s);

put('ifeq, 'c!:exit_helper, function c!:pifeq);

!#if common!-lisp!-mode
symbolic procedure c!:pifequal(s, depth);
  c!:printf("cl_equal(%v, %v)",
           car s, cadr s, car s, cadr s);
!#else
symbolic procedure c!:pifequal(s, depth);
  c!:printf("equal(%v, %v)",
           car s, cadr s, car s, cadr s);
!#endif

put('ifequal, 'c!:exit_helper, function c!:pifequal);

symbolic procedure c!:pifilessp(s, depth);
  c!:printf("((int32)(%v)) < ((int32)(%v))", car s, cadr s);

put('ifilessp, 'c!:exit_helper, function c!:pifilessp);

symbolic procedure c!:pifigreaterp(s, depth);
  c!:printf("((int32)(%v)) > ((int32)(%v))", car s, cadr s);

put('ifigreaterp, 'c!:exit_helper, function c!:pifigreaterp);

symbolic procedure c!:display_flowgraph(s, depth, dropping_through);
  if not atom s then <<
    c!:printf "    ";
    c!:pgoto(s, depth) >>
  else if not flagp(s, 'c!:visited) then begin
    scalar why, where_to;
    flag(list s, 'c!:visited);
    if not dropping_through or not (get(s, 'c!:count) = 1) then
        c!:printf("\n%s:\n", s);
    for each k in reverse get(s, 'c!:contents) do c!:print_opcode(k, depth);
    why := get(s, 'c!:why);
    where_to := get(s, 'c!:where_to);
    if why = 'goto and (not atom car where_to or
                        (not flagp(car where_to, 'c!:visited) and
                         get(car where_to, 'c!:count) = 1)) then
       c!:display_flowgraph(car where_to, depth, t)
    else c!:print_exit_condition(why, where_to, depth);
  end;

fluid '(startpoint);

symbolic procedure c!:branch_chain(s, count);
  begin
    scalar contents, why, where_to, n;
% do nothing to blocks already visted or return blocks.
    if not atom s then return s
    else if flagp(s, 'c!:visited) then <<
       n := get(s, 'c!:count);
       if null n then n := 1 else n := n + 1;
       put(s, 'c!:count, n);
       return s >>;
    flag(list s, 'c!:visited);
    contents := get(s, 'c!:contents);
    why := get(s, 'c!:why);
    where_to := for each z in get(s, 'c!:where_to) collect
                    c!:branch_chain(z, count);
% Turn movr a,b; return a; into return b;
    while contents and eqcar(car contents, 'movr) and
        why = 'goto and not atom car where_to and
        caar where_to = cadr car contents do <<
      where_to := list list cadddr car contents;
      contents := cdr contents >>;
    put(s, 'c!:contents, contents);
    put(s, 'c!:where_to, where_to);
% discard empty blocks
    if null contents and why = 'goto then <<
       remflag(list s, 'c!:visited);
       return car where_to >>;
    if count then <<
      n := get(s, 'c!:count);
      if null n then n := 1
      else n := n + 1;
      put(s, 'c!:count, n) >>;
    return s
  end;

symbolic procedure c!:one_operand op;
 << flag(list op, 'c!:set_r1);
    flag(list op, 'c!:read_r3);
    put(op, 'c!:code, function c!:builtin_one) >>;

symbolic procedure c!:two_operands op;
 << flag(list op, 'c!:set_r1);
    flag(list op, 'c!:read_r2);
    flag(list op, 'c!:read_r3);
    put(op, 'c!:code, function c!:builtin_two) >>;

for each n in '(car cdr qcar qcdr null not atom numberp fixp iminusp
                iminus iadd1 isub1 modular!-minus) do c!:one_operand n;
!#if common!-lisp!-mode
for each n in '(eq equal atsoc memq iplus2 idifference
                itimes2 ilessp igreaterp qgetv get
                modular!-plus modular!-difference
                ) do c!:two_operands n;
!#else
for each n in '(eq equal atsoc memq iplus2 idifference
                assoc member
                itimes2 ilessp igreaterp qgetv get
                modular!-plus modular!-difference
                ) do c!:two_operands n;
!#endif


flag('(movr movk movk1 ldrglob call reloadenv fastget fastflag), 'c!:set_r1);
flag('(strglob qputv), 'c!:read_r1);
flag('(qputv fastget fastflag), 'c!:read_r2);
flag('(movr qputv), 'c!:read_r3);
flag('(ldrglob strglob nilglob movk call), 'c!:read_env);
% special opcodes:
%   call fluidbind

fluid '(fn_used nil_used nilbase_used);

symbolic procedure c!:live_variable_analysis all_blocks;
  begin
    scalar changed, z;
    repeat <<
      changed := nil;
      for each b in all_blocks do
        begin
          scalar w, live;
          for each x in get(b, 'c!:where_to) do
             if atom x then live := union(live, get(x, 'c!:live))
             else live := union(live, x);
          w := get(b, 'c!:why);
          if not atom w then <<
             if eqcar(w, 'ifnull) or eqcar(w, 'ifequal) then nil_used := t;
             live := union(live, cdr w);
             if eqcar(car w, 'call) and
                (flagp(cadar w, 'c!:direct_predicate) or
                 (flagp(cadar w, 'c!:c_entrypoint) and
                  not flagp(cadar w, 'c!:direct_entrypoint))) then
                 nil_used := t;
             if eqcar(car w, 'call) and
                not (cadar w = current_procedure) and
                not get(cadar w, 'c!:direct_entrypoint) and
                not get(cadar w, 'c!:c_entrypoint) then <<
                    fn_used := t; live := union('(env), live) >> >>;
          for each s in get(b, 'c!:contents) do
            begin % backwards over contents
              scalar op, r1, r2, r3;
              op := car s; r1 := cadr s; r2 := caddr s; r3 := cadddr s;
              if op = 'movk1 then <<
                  if r3 = nil then nil_used := t
                  else if r3 = 't then nilbase_used := t >>
              else if atom op and flagp(op, 'c!:uses_nil) then nil_used := t;
              if flagp(op, 'c!:set_r1) then
!#if common!-lisp!-mode
                 if memq(r1, live) then live := remove(r1, live)
!#else
                 if memq(r1, live) then live := delete(r1, live)
!#endif
                 else if op = 'call then nil % Always needed
                 else op := 'nop;
              if flagp(op, 'c!:read_r1) then live := union(live, list r1);
              if flagp(op, 'c!:read_r2) then live := union(live, list r2);
              if flagp(op, 'c!:read_r3) then live := union(live, list r3);
              if op = 'call then <<
                 if not flagp(car r3, 'c!:no_errors) or
                    flagp(car r3, 'c!:c_entrypoint) or
                    get(car r3, 'c!:direct_predicate) then nil_used := t;
                 does_call := t;
                 if not eqcar(r3, current_procedure) and
                    not get(car r3, 'c!:direct_entrypoint) and
                    not get(car r3, 'c!:c_entrypoint) then fn_used := t;
                 if not flagp(car r3, 'c!:no_errors) then
                     flag(live, 'c!:live_across_call);
                 live := union(live, r2) >>;
              if flagp(op, 'c!:read_env) then live := union(live, '(env))
            end;
!#if common!-lisp!-mode
          live := append(live, nil); % because CL sort is destructive!
!#endif
          live := sort(live, function orderp);
          if not (live = get(b, 'c!:live)) then <<
            put(b, 'c!:live, live);
            changed := t >>
        end
    >> until not changed;
    z := registers;
    registers := stacklocs := nil;
    for each r in z do
       if flagp(r, 'c!:live_across_call) then stacklocs := r . stacklocs
       else registers := r . registers
  end;

symbolic procedure c!:insert1(a, b);
  if memq(a, b) then b
  else a . b;

symbolic procedure c!:clash(a, b);
  if flagp(a, 'c!:live_across_call) = flagp(b, 'c!:live_across_call) then <<   
    put(a, 'c!:clash, c!:insert1(b, get(a, 'c!:clash)));
    put(b, 'c!:clash, c!:insert1(a, get(b, 'c!:clash))) >>;

symbolic procedure c!:build_clash_matrix all_blocks;
  begin
    for each b in all_blocks do
      begin
        scalar live, w;
        for each x in get(b, 'c!:where_to) do
           if atom x then live := union(live, get(x, 'c!:live))
           else live := union(live, x);
        w := get(b, 'c!:why);
        if not atom w then <<
           live := union(live, cdr w);
           if eqcar(car w, 'call) and
                not get(cadar w, 'c!:direct_entrypoint) and
                not get(cadar w, 'c!:c_entrypoint) then
              live := union('(env), live) >>;
        for each s in get(b, 'c!:contents) do
          begin
            scalar op, r1, r2, r3;
            op := car s; r1 := cadr s; r2 := caddr s; r3 := cadddr s;
            if flagp(op, 'c!:set_r1) then
               if memq(r1, live) then <<
!#if common!-lisp!-mode
                  live := remove(r1, live);
!#else
                  live := delete(r1, live);
!#endif
                  if op = 'reloadenv then reloadenv := t;
                  for each v in live do c!:clash(r1, v) >>
               else if op = 'call then nil
               else <<
                  op := 'nop;
                  rplacd(s, car s . cdr s); % Leaves original instrn visible
                  rplaca(s, op) >>;
            if flagp(op, 'c!:read_r1) then live := union(live, list r1);
            if flagp(op, 'c!:read_r2) then live := union(live, list r2);
            if flagp(op, 'c!:read_r3) then live := union(live, list r3);
% Maybe CALL should be a little more selective about need for "env"?
            if op = 'call then live := union(live, r2);
            if flagp(op, 'c!:read_env) then live := union(live, '(env))
          end
      end;
% The next few lines are for debugging...
%%-    c!:printf "Scratch registers:\n";
%%-    for each r in registers do
%%-        c!:printf("%s clashes: %s\n", r, get(r, 'c!:clash));
%%-    c!:printf "Stack items:\n";
%%-    for each r in stacklocs do
%%-        c!:printf("%s clashes: %s\n", r, get(r, 'c!:clash));
    return nil
  end;

symbolic procedure c!:allocate_registers rl;
  begin
    scalar schedule, neighbours, allocation;
    neighbours := 0;
    while rl do begin
      scalar w, x;
      w := rl;
      while w and length (x := get(car w, 'c!:clash)) > neighbours do
        w := cdr w;
      if w then <<
        schedule := car w . schedule;
        rl := deleq(car w, rl);
        for each r in x do put(r, 'c!:clash, deleq(car w, get(r, 'c!:clash))) >>
      else neighbours := neighbours + 1
    end;
    for each r in schedule do begin
      scalar poss;
      poss := allocation;
      for each x in get(r, 'c!:clash) do
        poss := deleq(get(x, 'c!:chosen), poss);
      if null poss then <<
         poss := c!:my_gensym();
         allocation := append(allocation, list poss) >>
      else poss := car poss;
%     c!:printf("/* Allocate %s to %s, to miss %s */\n",
%               r, poss, get(r, 'c!:clash));
      put(r, 'c!:chosen, poss)
    end;
    return allocation
  end;
        
symbolic procedure c!:remove_nops all_blocks;
% Remove no-operation instructions, and map registers to reflect allocation
  for each b in all_blocks do
    begin
      scalar r;
      for each s in get(b, 'c!:contents) do
        if not eqcar(s, 'nop) then
          begin
            scalar op, r1, r2, r3;
            op := car s; r1 := cadr s; r2 := caddr s; r3 := cadddr s;
            if flagp(op, 'c!:set_r1) or flagp(op, 'c!:read_r1) then
               r1 := get(r1, 'c!:chosen);
            if flagp(op, 'c!:read_r2) then r2 := get(r2, 'c!:chosen);
            if flagp(op, 'c!:read_r3) then r3 := get(r3, 'c!:chosen);
            if op = 'call then
               r2 := for each v in r2 collect get(v, 'c!:chosen);
            if not (op = 'movr and r1 = r3) then
               r := list(op, r1, r2, r3) . r
          end;
      put(b, 'c!:contents, reversip r);
      r := get(b, 'c!:why);
      if not atom r then
         put(b, 'c!:why, 
                car r . for each v in cdr r collect get(v, 'c!:chosen))
    end;

fluid '(error_labels);

symbolic procedure c!:find_error_label(why, env, depth);
  begin
    scalar w, z;
    z := list(why, env, depth);
    w := assoc!*!*(z, error_labels);
    if null w then <<
       w := z . c!:my_gensym();
       error_labels := w . error_labels >>;
    return cdr w
  end;

symbolic procedure c!:assign(u, v, c);
  if flagp(u, 'fluid) then list('strglob, v, u, c!:find_literal u) . c
  else list('movr, u, nil, v) . c;

symbolic procedure c!:insert_tailcall b;
  begin
    scalar why, dest, contents, fcall, res, w;
    why := get(b, 'c!:why);
    dest := get(b, 'c!:where_to);
    contents := get(b, 'c!:contents);
    while contents and not eqcar(car contents, 'call) do <<
      w := car contents . w;
      contents := cdr contents >>;
    if null contents then return nil;
    fcall := car contents;
    contents := cdr contents;
    res := cadr fcall;
    while w do <<
      if eqcar(car w, 'reloadenv) then w := cdr w
      else if eqcar(car w, 'movr) and cadddr car w = res then <<
        res := cadr car w;
        w := cdr w >>
      else res := w := nil >>;
    if null res then return nil;
    if c!:does_return(res, why, dest) then
       if car cadddr fcall = current_procedure then <<
          for each p in pair(current_args, caddr fcall) do
             contents := c!:assign(car p, cdr p, contents);
          put(b, 'c!:contents, contents);
          put(b, 'c!:why, 'goto);
          put(b, 'c!:where_to, list restart_label) >>
       else <<
          nil_used := t;
          put(b, 'c!:contents, contents);
          put(b, 'c!:why, list('call, car cadddr fcall) . caddr fcall);
          put(b, 'c!:where_to, nil) >>
  end;

symbolic procedure c!:does_return(res, why, where_to);
  if not (why = 'goto) then nil
  else if not atom car where_to then res = caar where_to
  else begin
    scalar contents;
    where_to := car where_to;
    contents := reverse get(where_to, 'c!:contents);
    why := get(where_to, 'c!:why);
    where_to := get(where_to, 'c!:where_to);
    while contents do
      if eqcar(car contents, 'reloadenv) then contents := cdr contents
      else if eqcar(car contents, 'movr) and cadddr car contents = res then <<
        res := cadr car contents;
        contents := cdr contents >>
      else res := contents := nil;
    if null res then return nil
    else return c!:does_return(res, why, where_to)
  end;

symbolic procedure c!:pushpop(op, v);
%  for each x in v do c!:printf("        %s(%s);\n", op, x);
  begin
    scalar n, w;
    if null v then return nil;
    n := length v;
    if n = 1 then return c!:printf("        %s(%s);\n", op, car v);
    while n > 0 do <<
       w := n;
       if w > 6 then w := 6;
       n := n-w;
       c!:printf("        %s%d(%s", op, w, car v);
       v := cdr v;
       for i := 2:w do <<
          c!:printf(",%s", car v);
          v := cdr v >>;
       c!:printf(");\n") >>
  end;

symbolic procedure c!:optimise_flowgraph(startpoint, all_blocks,
                                          env, argch, args);
  begin
    scalar w, n, locs, stacks, error_labels, fn_used, nil_used, nilbase_used;
!#if common!-lisp!-mode
    nilbase_used := t;  % For onevalue(xxx) at least 
!#endif
    for each b in all_blocks do c!:insert_tailcall b;
    startpoint := c!:branch_chain(startpoint, nil);
    remflag(all_blocks, 'c!:visited);
    c!:live_variable_analysis all_blocks;
    c!:build_clash_matrix all_blocks;
    if error_labels and env then reloadenv := t;
    for each u in env do
      for each v in env do c!:clash(cdr u, cdr v); % keep all args distinct
    locs := c!:allocate_registers registers;
    stacks := c!:allocate_registers stacklocs;
    flag(stacks, 'c!:live_across_call);
    c!:remove_nops all_blocks;
    startpoint := c!:branch_chain(startpoint, nil); % after tailcall insertion
    remflag(all_blocks, 'c!:visited);
    startpoint := c!:branch_chain(startpoint, t); % ... AGAIN to tidy up
    remflag(all_blocks, 'c!:visited);
    if does_call then nil_used := t;
    if nil_used then c!:printf "    Lisp_Object nil = C_nil;\n"
    else if nilbase_used then c!:printf "    nil_as_base\n";
    if locs then <<
      c!:printf("    Lisp_Object %s", car locs);
      for each v in cdr locs do c!:printf(", %s", v);
      c!:printf ";\n" >>;
    if fn_used then c!:printf "    Lisp_Object fn;\n";
    if car argch = 0 or car argch >= 3 then
       c!:printf("    argcheck(nargs, %s, \q%s\q);\n", car argch, cdr argch);
% I will not do a stack check if I have a leaf procedure, and I hope
% that this policy will speed up code a bit.
    if does_call then <<
       c!:printf "    if (stack >= stacklimit)\n";
       c!:printf "    {\n";
% This is slightly clumsy code to save all args on the stack across the
% call to reclaim(), but it is not executed often...
       c!:pushpop('push, args);
       c!:printf "        env = reclaim(env, \qstack\q, GC_STACK, 0);\n";
       c!:pushpop('pop, reverse args);
       c!:printf "        nil = C_nil;\n";
       c!:printf "        if (exception_pending()) return nil;\n";
       c!:printf "    }\n" >>;
    if reloadenv then c!:printf("    push(env);\n")
    else c!:printf("    CSL_IGNORE(env);\n");
    n := 0;
    if stacks then <<
       c!:printf "/* space for vars preserved across procedure calls */\n";
       for each v in stacks do <<
          put(v, 'c!:location, n);
          n := n+1 >>;
       w := n;
       while w >= 5 do <<
          c!:printf "    push5(nil, nil, nil, nil, nil);\n";
          w := w - 5 >>;
       if w neq 0 then <<
          if w = 1 then c!:printf "    push(nil);\n"
          else <<
             c!:printf("    push%s(nil", w);
             for i := 2:w do c!:printf ", nil";
             c!:printf ");\n" >> >> >>;
    if reloadenv then <<
       reloadenv := n;
       n := n + 1 >>;
    if env then c!:printf "/* copy arguments values to proper place */\n";
    for each v in env do
      if flagp(cdr v, 'c!:live_across_call) then
         c!:printf("    stack[%s] = %s;\n",
               -get(get(cdr v, 'c!:chosen), 'c!:location), cdr v)
      else c!:printf("    %s = %s;\n", get(cdr v, 'c!:chosen), cdr v);
    c!:printf "/* end of prologue */\n";
    c!:display_flowgraph(startpoint, n, t);
    if error_labels then <<
       c!:printf "/* error exit handlers */\n";
       for each x in error_labels do <<
          c!:printf("%s:\n", cdr x);
          c!:print_error_return(caar x, cadar x, caddar x) >> >>;
    remflag(all_blocks, 'c!:visited);
  end;

symbolic procedure c!:print_error_return(why, env, depth);
  begin
    if reloadenv and env then
       c!:printf("    env = stack[%s];\n", -reloadenv);
    if null why then <<
% One could imagine generating backtrace entries here...
       for each v in env do
          c!:printf("    qvalue(elt(env, %s)) = %v; /* %a */\n",
                 c!:find_literal car v, get(cdr v, 'c!:chosen), car v);
       if depth neq 0 then c!:printf("    popv(%s);\n", depth);
       c!:printf "    return nil;\n" >>
    else if flagp(cadr why, 'c!:live_across_call) then <<
       c!:printf("    {   Lisp_Object res = %v;\n", cadr why);
       for each v in env do
          c!:printf("        qvalue(elt(env, %s)) = %v;\n",
                 c!:find_literal car v, get(cdr v, 'c!:chosen));
       if depth neq 0 then c!:printf("        popv(%s);\n", depth);
       c!:printf("        return error(1, %s, res); }\n",
          if eqcar(why, 'car) then "err_bad_car"
          else if eqcar(why, 'cdr) then "err_bad_cdr"
          else error(0, list(why, "unknown_error"))) >>
    else <<
       for each v in env do
          c!:printf("    qvalue(elt(env, %s)) = %v;\n",
                 c!:find_literal car v, get(cdr v, 'c!:chosen));
       if depth neq 0 then c!:printf("    popv(%s);\n", depth);
       c!:printf("    return error(1, %s, %v);\n",
          (if eqcar(why, 'car) then "err_bad_car"
           else if eqcar(why, 'cdr) then "err_bad_cdr"
           else error(0, list(why, "unknown_error"))),
          cadr why) >>
  end;


%
% Now I have a series of separable sections each of which gives a special
% recipe that implements or optimises compilation of some specific Lisp
% form.
%

symbolic procedure c!:cand(u, env);
  begin
    scalar w, r;
    w := reverse cdr u;
    if null w then return c!:cval(nil, env);
    r := list(list('t, car w));
    w := cdr w;
    for each z in w do
       r := list(list('null, z), nil) . r;
    r := 'cond . r;
    return c!:cval(r, env)
  end;
%--    scalar next, done, v, r;
%--    v := c!:newreg();
%--    done := c!:my_gensym();
%--    u := cdr u;
%--    while cdr u do <<
%--      next := c!:my_gensym();
%--      c!:outop('movr, v, nil, c!:cval(car u, env));
%--      u := cdr u;
%--      c!:endblock(list('ifnull, v), list(done, next));
%--      c!:startblock next >>;
%--    c!:outop('movr, v, nil, c!:cval(car u, env));
%--    c!:endblock('goto, list done);
%--    c!:startblock done;
%--    return v
%--  end;

put('and, 'c!:code, function c!:cand);

!#if common!-lisp!-mode

symbolic procedure c!:cblock(u, env);
  begin
    scalar progret, progexit, r;
    progret := c!:newreg();
    progexit := c!:my_gensym();
    blockstack := (cadr u . progret . progexit) . blockstack;
    u := cddr u;
    for each a in u do r := c!:cval(a, env);
    c!:outop('movr, progret, nil, r);
    c!:endblock('goto, list progexit);
    c!:startblock progexit;
    blockstack := cdr blockstack;
    return progret
  end;


put('block, 'c!:code, function c!:cblock);

!#endif

symbolic procedure c!:ccatch(u, env);
   error(0, "catch");

put('catch, 'c!:code, function c!:ccatch);

symbolic procedure c!:ccompile_let(u, env);
   error(0, "compiler-let");

put('compiler!-let, 'c!:code, function c!:ccompiler_let);

symbolic procedure c!:ccond(u, env);
  begin
    scalar v, join;
    v := c!:newreg();
    join := c!:my_gensym();
    for each c in cdr u do begin
      scalar l1, l2;
      l1 := c!:my_gensym(); l2 := c!:my_gensym();
      if atom cdr c then <<
         c!:outop('movr, v, nil, c!:cval(car c, env));
         c!:endblock(list('ifnull, v), list(l2, join)) >>
      else <<
         c!:cjumpif(car c, env, l1, l2);
         c!:startblock l1;    % if the condition is true
         c!:outop('movr, v, nil, c!:cval('progn . cdr c, env));
         c!:endblock('goto, list join) >>;
      c!:startblock l2 end;
    c!:outop('movk1, v, nil, nil);
    c!:endblock('goto, list join);
    c!:startblock join;
    return v
  end;

put('cond, 'c!:code, function c!:ccond);

symbolic procedure c!:cdeclare(u, env);
   error(0, "declare");

put('declare, 'c!:code, function c!:cdeclare);

symbolic procedure c!:cde(u, env);
   error(0, "de");

put('de, 'c!:code, function c!:cde);

symbolic procedure c!:cdefun(u, env);
   error(0, "defun");

put('!~defun, 'c!:code, function c!:cdefun);

symbolic procedure c!:ceval_when(u, env);
   error(0, "eval-when");

put('eval!-when, 'c!:code, function c!:ceval_when);

symbolic procedure c!:cflet(u, env);
   error(0, "flet");

put('flet, 'c!:code, function c!:cflet);


symbolic procedure c!:cfunction(u, env);
  begin
    scalar v;
    u := cadr u;
    if not atom u then <<
       if not eqcar(u, 'lambda) then 
           error(0, list("lambda expression needed", u));
       v := dated!-name 'lambda;
       pending_functions :=
          ('de . v . cdr u) . pending_functions;
       u := v >>;
    v := c!:newreg();
    c!:outop('movk, v, u, c!:find_literal u);
    return v;
  end;

put('function, 'c!:code, function c!:cfunction);

symbolic procedure c!:cgo(u, env);
  begin
    scalar w, w1;
    w1 := proglabs;
    while null w and w1 do <<
       w := assoc!*!*(cadr u, car w1);
       w1 := cdr w1 >>;
    if null w then error(0, list(u, "label not set"));
    c!:endblock('goto, list cadr w);
    return nil      % value should not be used
  end;

put('go, 'c!:code, function c!:cgo);

symbolic procedure c!:cif(u, env);
  begin
    scalar v, join, l1, l2, w;
    v := c!:newreg();
    join := c!:my_gensym();
    l1 := c!:my_gensym();
    l2 := c!:my_gensym();
    c!:cjumpif(car (u := cdr u), env, l1, l2);
    c!:startblock l1;
    c!:outop('movr, v, nil, c!:cval(car (u := cdr u), env));
    c!:endblock('goto, list join);
    c!:startblock l2;
    u := cdr u;
    if u then u := car u; % permit 2-arg version...
    c!:outop('movr, v, nil, c!:cval(u, env));
    c!:endblock('goto, list join);
    c!:startblock join;
    return v
  end;

put('if, 'c!:code, function c!:cif);

symbolic procedure c!:clabels(u, env);
   error(0, "labels");

put('labels, 'c!:code, function c!:clabels);

symbolic procedure c!:expand!-let(vl, b);
  if null vl then 'progn . b
  else if null cdr vl then c!:expand!-let!*(vl, b)
  else begin scalar vars, vals;
    for each v in vl do
      if atom v then << vars := v . vars; vals := nil . vals >>
      else if atom cdr v then << vars := car v . vars; vals := nil . vals >>
      else << vars := car v . vars; vals := cadr v . vals >>;
    return ('lambda . vars . b) . vals
  end;

symbolic procedure c!:clet(x, env);
   c!:cval(c!:expand!-let(cadr x, cddr x), env);

!#if common!-lisp!-mode
put('let, 'c!:code, function c!:clet);
!#else
put('!~let, 'c!:code, function c!:clet);
!#endif

symbolic procedure c!:expand!-let!*(vl, b);
  if null vl then 'progn . b
  else begin scalar var, val;
    var := car vl;
    if not atom var then <<
       val := cdr var;
       var := car var;
       if not atom val then val := car val >>;
    b := list list('return, c!:expand!-let!*(cdr vl, b));
    if val then b := list('setq, var, val) . b;
    return 'prog . list var . b
  end;

symbolic procedure c!:clet!*(x, env);
   c!:cval(c!:expand!-let!*(cadr x, cddr x), env);

put('let!*, 'c!:code, function c!:clet!*);

symbolic procedure c!:clist(u, env);
  if null cdr u then c!:cval(nil, env)
  else if null cddr u then c!:cval('ncons . cdr u, env)
  else if eqcar(cadr u, 'cons) then
    c!:cval(list('acons, cadr cadr u, caddr cadr u, 'list . cddr u), env)
  else if null cdddr u then c!:cval('list2 . cdr u, env)
  else c!:cval(list('list2!*, cadr u, caddr u, 'list . cdddr u), env);

put('list, 'c!:code, function c!:clist);

symbolic procedure c!:clist!*(u, env);
  begin
    scalar v;
    u := reverse cdr u;
    v := car u;
    for each a in cdr u do
      v := list('cons, a, v);
    return c!:cval(v, env)
  end;    

put('list!*, 'c!:code, function c!:clist!*);

symbolic procedure c!:ccons(u, env);
  begin
    scalar a1, a2;
    a1 := s!:improve cadr u;
    a2 := s!:improve caddr u;
    if a2 = nil or a2 = '(quote nil) or a2 = '(list) then
       return c!:cval(list('ncons, a1), env);
    if eqcar(a1, 'cons) then
       return c!:cval(list('acons, cadr a1, caddr a1, a2), env);
    if eqcar(a2, 'cons) then
       return c!:cval(list('list2!*, a1, cadr a2, caddr a2), env);
    if eqcar(a2, 'list) then
       return c!:cval(list('cons, a1,
                     list('cons, cadr a2, 'list . cddr a2)), env);
    return c!:ccall(car u, cdr u, env)
  end;

put('cons, 'c!:code, function c!:ccons);

symbolic procedure c!:cget(u, env);
  begin
    scalar a1, a2, w, r, r1;
    a1 := s!:improve cadr u;
    a2 := s!:improve caddr u;
    if eqcar(a2, 'quote) and idp(w := cadr a2) and
       (w := symbol!-make!-fastget(w, nil)) then <<
        r := c!:newreg();
        c!:outop('fastget, r, c!:cval(a1, env), w . cadr a2);
        return r >>
    else return c!:ccall(car u, cdr u, env)
  end;

put('get, 'c!:code, function c!:cget);

symbolic procedure c!:cflag(u, env);
  begin
    scalar a1, a2, w, r, r1;
    a1 := s!:improve cadr u;
    a2 := s!:improve caddr u;
    if eqcar(a2, 'quote) and idp(w := cadr a2) and
       (w := symbol!-make!-fastget(w, nil)) then <<
        r := c!:newreg();
        c!:outop('fastflag, r, c!:cval(a1, env), w . cadr a2);
        return r >>
    else return c!:ccall(car u, cdr u, env)
  end;

put('flagp, 'c!:code, function c!:cflag);

symbolic procedure c!:cgetv(u, env);
  if not !*fastvector then c!:ccall(car u, cdr u, env)
  else c!:cval('qgetv . cdr u, env);

put('getv, 'c!:code, function c!:cgetv);
!#if common!-lisp!-mode
put('svref, 'c!:code, function c!:cgetv);
!#endif

symbolic procedure c!:cputv(u, env);
  if not !*fastvector then c!:ccall(car u, cdr u, env)
  else c!:cval('qputv . cdr u, env);

put('putv, 'c!:code, function c!:cputv);

symbolic procedure c!:cqputv(x, env);
  begin
    scalar rr;
    rr := c!:pareval(cdr x, env);
    c!:outop('qputv, caddr rr, car rr, cadr rr);
    return caddr rr
  end;

put('qputv, 'c!:code, function c!:cqputv);

symbolic procedure c!:cmacrolet(u, env);
   error(0, "macrolet");

put('macrolet, 'c!:code, function c!:cmacrolet);

symbolic procedure c!:cmultiple_value_call(u, env);
   error(0, "multiple_value_call");

put('multiple!-value!-call, 'c!:code, function c!:cmultiple_value_call);

symbolic procedure c!:cmultiple_value_prog1(u, env);
   error(0, "multiple_value_prog1");

put('multiple!-value!-prog1, 'c!:code, function c!:cmultiple_value_prog1);

symbolic procedure c!:cor(u, env);
  begin
    scalar next, done, v, r;
    v := c!:newreg();
    done := c!:my_gensym();
    u := cdr u;
    while cdr u do <<
      next := c!:my_gensym();
      c!:outop('movr, v, nil, c!:cval(car u, env));
      u := cdr u;
      c!:endblock(list('ifnull, v), list(next, done));
      c!:startblock next >>;
    c!:outop('movr, v, nil, c!:cval(car u, env));
    c!:endblock('goto, list done);
    c!:startblock done;
    return v
  end;

put('or, 'c!:code, function c!:cor);

symbolic procedure c!:cprog(u, env);
  begin
    scalar w, w1, bvl, local_proglabs, progret, progexit, fluids, env1;
    env1 := car env;
    bvl := cadr u;
    for each v in bvl do
       if globalp v then error(0, list(v, "attempt to bind a global"))
       else if fluidp v then <<
          fluids := (v . c!:newreg()) . fluids;
          flag(list cdar fluids, 'c!:live_across_call); % silly if not
          env1 := ('c!:dummy!:name . cdar fluids) . env1;
          c!:outop('ldrglob, cdar fluids, v, c!:find_literal v);
          c!:outop('nilglob, nil, v, c!:find_literal v) >>
       else <<
          env1 := (v . c!:newreg()) . env1;
          c!:outop('movk1, cdar env1, nil, nil) >>;
    if fluids then c!:outop('fluidbind, nil, nil, fluids);
    env := env1 . append(fluids, cdr env);
    u := cddr u;
    progret := c!:newreg();
    progexit := c!:my_gensym();
    blockstack := (nil . progret . progexit) . blockstack;
    for each a in u do if atom a then
       if atsoc(a, local_proglabs) then <<
          if not null a then <<
             w := wrs nil;
             princ "+++++ multiply defined label: "; prin a; terpri(); wrs w >> >>
       else local_proglabs := list(a, c!:my_gensym()) . local_proglabs;
    proglabs := local_proglabs . proglabs;
    for each a in u do
      if atom a then <<
        w := cdr(assoc!*!*(a, local_proglabs));
        if null cdr w then <<
           rplacd(w, t);
           c!:endblock('goto, list car w);
           c!:startblock car w >> >>
      else c!:cval(a, env);
    c!:outop('movk1, progret, nil, nil);
    c!:endblock('goto, list progexit);
    c!:startblock progexit;
    for each v in fluids do
      c!:outop('strglob, cdr v, car v, c!:find_literal car v);
    blockstack := cdr blockstack;
    proglabs := cdr proglabs;
    return progret
  end;

put('prog, 'c!:code, function c!:cprog);

symbolic procedure c!:cprog!*(u, env);
   error(0, "prog*");

put('prog!*, 'c!:code, function c!:cprog!*);

symbolic procedure c!:cprog1(u, env);
  begin
    scalar g;
    g := c!:my_gensym();
    g := list('prog, list g,
              list('setq, g, cadr u),
              'progn . cddr u,
              list('return, g));
    return c!:cval(g, env)
  end;

put('prog1, 'c!:code, function c!:cprog1);

symbolic procedure c!:cprog2(u, env);
  begin
    scalar g;
    u := cdr u;
    g := c!:my_gensym();
    g := list('prog, list g,
              list('setq, g, cadr u),
              'progn . cddr u,
              list('return, g));
    g := list('progn, car u, g);
    return c!:cval(g, env)
  end;

put('prog2, 'c!:code, function c!:cprog2);

symbolic procedure c!:cprogn(u, env);
  begin
    scalar r;
    u := cdr u;
    if u = nil then u := '(nil);
    for each s in u do r := c!:cval(s, env);
    return r
  end;

put('progn, 'c!:code, function c!:cprogn);

symbolic procedure c!:cprogv(u, env);
   error(0, "progv");

put('progv, 'c!:code, function c!:cprogv);

symbolic procedure c!:cquote(u, env);
  begin
    scalar v;
    u := cadr u;
    v := c!:newreg();
    if null u or u = 't or c!:small_number u then
         c!:outop('movk1, v, nil, u)
    else c!:outop('movk, v, u, c!:find_literal u);
    return v;
  end;

put('quote, 'c!:code, function c!:cquote);

symbolic procedure c!:creturn(u, env);
  begin
    scalar w;
    w := assoc!*!*(nil, blockstack);
    if null w then error "RETURN out of context";
    c!:outop('movr, cadr w, nil, c!:cval(cadr u, env));
    c!:endblock('goto, list cddr w);
    return nil      % value should not be used
  end;

put('return, 'c!:code, function c!:creturn);

!#if common!-lisp!-mode

symbolic procedure c!:creturn_from(u, env);
  begin
    scalar w;
    w := assoc!*!*(cadr u, blockstack);
    if null w then error "RETURN-FROM out of context";
    c!:outop('movr, cadr w, nil, c!:cval(caddr u, env));
    c!:endblock('goto, list cddr w);
    return nil      % value should not be used
  end;

!#endif

put('return!-from, 'c!:code, function c!:creturn_from);

symbolic procedure c!:csetq(u, env);
  begin
    scalar v, w;
    v := c!:cval(caddr u, env);
    u := cadr u;
    if not idp u then error(0, list(u, "bad variable in setq"))
    else if (w := c!:locally_bound(u, env)) then
       c!:outop('movr, cdr w, nil, v)
    else if flagp(u, 'c!:constant) then
       error(0, list(u, "attempt to use setq on a constant"))
    else c!:outop('strglob, v, u, c!:find_literal u);
    return v
  end;

put('setq, 'c!:code, function c!:csetq);
put('noisy!-setq, 'c!:code, function c!:csetq);

!#if common!-lisp!-mode

symbolic procedure c!:ctagbody(u, env);
  begin
    scalar w, bvl, local_proglabs, res;
    u := cdr u;
    for each a in u do if atom a then
       if atsoc(a, local_proglabs) then <<
          if not null a then <<
             w := wrs nil;
             princ "+++++ multiply defined label: "; prin a; terpri(); wrs w >> >>
       else local_proglabs := list(a, c!:my_gensym()) . local_proglabs;
    proglabs := local_proglabs . proglabs;
    for each a in u do
      if atom a then <<
        w := cdr(assoc!*!*(a, local_proglabs));
        if null cdr w then <<
           rplacd(w, t);
           c!:endblock('goto, list car w);
           c!:startblock car w >> >>
      else res := c!:cval(a, env);
    if null res then res := c!:cval(nil, env);
    proglabs := cdr proglabs;
    return res
  end;

put('tagbody, 'c!:code, function c!:ctagbody);

!#endif

symbolic procedure c!:cprivate_tagbody(u, env);
% This sets a label for use for tail-call to self.
  begin
    u := cdr u;
    c!:endblock('goto, list car u);
    c!:startblock car u;
% This seems to be the proper place to capture the internal names associated
% with argument-vars that must be reset if a tail-call is mapped into a loop.
    current_args := for each v in current_args collect begin
       scalar z;
       z := assoc!*!*(v, car env);
       return if z then cdr z else v end;
    return c!:cval(cadr u, env)
  end;

put('c!:private_tagbody, 'c!:code, function c!:cprivate_tagbody);

symbolic procedure c!:cthe(u, env);
   c!:cval(caddr u, env);

put('the, 'c!:code, function c!:cthe);

symbolic procedure c!:cthrow(u, env);
   error(0, "throw");

put('throw, 'c!:code, function c!:cthrow);

symbolic procedure c!:cunless(u, env);
  begin
    scalar v, join, l1, l2;
    v := c!:newreg();
    join := c!:my_gensym();
    l1 := c!:my_gensym();
    l2 := c!:my_gensym();
    c!:cjumpif(cadr u, env, l2, l1);
    c!:startblock l1;
    c!:outop('movr, v, nil, c!:cval('progn . cddr u, env));
    c!:endblock('goto, list join);
    c!:startblock l2;
    c!:outop('movk1, v, nil, nil);
    c!:endblock('goto, list join);
    c!:startblock join;
    return v
  end;

put('unless, 'c!:code, function c!:cunless);

symbolic procedure c!:cunwind_protect(u, env);
   error(0, "unwind_protect");

put('unwind!-protect, 'c!:code, function c!:cunwind_protect);

symbolic procedure c!:cwhen(u, env);
  begin
    scalar v, join, l1, l2;
    v := c!:newreg();
    join := c!:my_gensym();
    l1 := c!:my_gensym();
    l2 := c!:my_gensym();
    c!:cjumpif(cadr u, env, l1, l2);
    c!:startblock l1;
    c!:outop('movr, v, nil, c!:cval('progn . cddr u, env));
    c!:endblock('goto, list join);
    c!:startblock l2;
    c!:outop('movk1, v, nil, nil);
    c!:endblock('goto, list join);
    c!:startblock join;
    return v
  end;

put('when, 'c!:code, function c!:cwhen);

%
% End of code to handle special forms - what comes from here on is
% more concerned with performance than with speed.
%

!#if (not common!-lisp!-mode)

% mapcar etc are compiled specially as a fudge to achieve an effect as
% if proper environment-capture was implemented for the functional
% argument (which I do not support at present).

symbolic procedure c!:expand_map(fnargs);
  begin
    scalar carp, fn, fn1, args, var, avar, moveon, l1, r, s, closed;
    fn := car fnargs;
% if the value of a mapping function is not needed I demote from mapcar to
% mapc or from maplist to map.
%   if context > 1 then <<
%      if fn = 'mapcar then fn := 'mapc
%      else if fn = 'maplist then fn := 'map >>;
    if fn = 'mapc or fn = 'mapcar or fn = 'mapcan then carp := t;
    fnargs := cdr fnargs;
    if atom fnargs then error(0,"bad arguments to map function");
    fn1 := cadr fnargs;
    while eqcar(fn1, 'function) or
          (eqcar(fn1, 'quote) and eqcar(cadr fn1, 'lambda)) do <<
       fn1 := cadr fn1;
       closed := t >>;
% if closed is false I will insert FUNCALL since I am invoking a function
% stored in a variable - NB this means that the word FUNCTION becomes
% essential when using mapping operators - this is because I have built
% a 2-Lisp rather than a 1-Lisp.
    args := car fnargs;
    l1 := c!:my_gensym();
    r := c!:my_gensym();
    s := c!:my_gensym();
    var := c!:my_gensym();
    avar := var;
    if carp then avar := list('car, avar);
    if closed then fn1 := list(fn1, avar)
    else fn1 := list('apply1, fn1, avar);
    moveon := list('setq, var, list('cdr, var));
    if fn = 'map or fn = 'mapc then fn := sublis(
       list('l1 . l1, 'var . var,
            'fn . fn1, 'args . args, 'moveon . moveon),
       '(prog (var)
             (setq var args)
       l1    (cond
                ((not var) (return nil)))
             fn
             moveon
             (go l1)))
    else if fn = 'maplist or fn = 'mapcar then fn := sublis(
       list('l1 . l1, 'var . var,
            'fn . fn1, 'args . args, 'moveon . moveon, 'r . r),
       '(prog (var r)
             (setq var args)
       l1    (cond
                ((not var) (return (reversip r))))
             (setq r (cons fn r))
             moveon
             (go l1)))
    else fn := sublis(
       list('l1 . l1, 'l2 . c!:my_gensym(), 'var . var,
            'fn . fn1, 'args . args, 'moveon . moveon,
            'r . c!:my_gensym(), 's . c!:my_gensym()),
       '(prog (var r s)
             (setq var args)
             (setq r (setq s (list nil)))
       l1    (cond
                ((not var) (return (cdr r))))
             (rplacd s fn)
       l2    (cond
                ((not (atom (cdr s))) (setq s (cdr s)) (go l2)))
             moveon
             (go l1)));
    return fn
  end;


put('map,     'c!:compile_macro, function c!:expand_map);
put('maplist, 'c!:compile_macro, function c!:expand_map);
put('mapc,    'c!:compile_macro, function c!:expand_map);
put('mapcar,  'c!:compile_macro, function c!:expand_map);
put('mapcon,  'c!:compile_macro, function c!:expand_map);
put('mapcan,  'c!:compile_macro, function c!:expand_map);

!#endif

% caaar to cddddr get expanded into compositions of
% car, cdr which are compiled in-line

symbolic procedure c!:expand_carcdr(x);
  begin
    scalar name;
    name := cdr reverse cdr explode2 car x;
    x := cadr x;
    for each v in name do
        x := list(if v = 'a then 'car else 'cdr, x);
    return x
  end;

<< put('caar, 'c!:compile_macro, function c!:expand_carcdr);
   put('cadr, 'c!:compile_macro, function c!:expand_carcdr);
   put('cdar, 'c!:compile_macro, function c!:expand_carcdr);
   put('cddr, 'c!:compile_macro, function c!:expand_carcdr);
   put('caaar, 'c!:compile_macro, function c!:expand_carcdr);
   put('caadr, 'c!:compile_macro, function c!:expand_carcdr);
   put('cadar, 'c!:compile_macro, function c!:expand_carcdr);
   put('caddr, 'c!:compile_macro, function c!:expand_carcdr);
   put('cdaar, 'c!:compile_macro, function c!:expand_carcdr);
   put('cdadr, 'c!:compile_macro, function c!:expand_carcdr);
   put('cddar, 'c!:compile_macro, function c!:expand_carcdr);
   put('cdddr, 'c!:compile_macro, function c!:expand_carcdr);
   put('caaaar, 'c!:compile_macro, function c!:expand_carcdr);
   put('caaadr, 'c!:compile_macro, function c!:expand_carcdr);
   put('caadar, 'c!:compile_macro, function c!:expand_carcdr);
   put('caaddr, 'c!:compile_macro, function c!:expand_carcdr);
   put('cadaar, 'c!:compile_macro, function c!:expand_carcdr);
   put('cadadr, 'c!:compile_macro, function c!:expand_carcdr);
   put('caddar, 'c!:compile_macro, function c!:expand_carcdr);
   put('cadddr, 'c!:compile_macro, function c!:expand_carcdr);
   put('cdaaar, 'c!:compile_macro, function c!:expand_carcdr);
   put('cdaadr, 'c!:compile_macro, function c!:expand_carcdr);
   put('cdadar, 'c!:compile_macro, function c!:expand_carcdr);
   put('cdaddr, 'c!:compile_macro, function c!:expand_carcdr);
   put('cddaar, 'c!:compile_macro, function c!:expand_carcdr);
   put('cddadr, 'c!:compile_macro, function c!:expand_carcdr);
   put('cdddar, 'c!:compile_macro, function c!:expand_carcdr);
   put('cddddr, 'c!:compile_macro, function c!:expand_carcdr) >>;

symbolic procedure c!:builtin_one(x, env);
  begin
    scalar r1, r2;
    r1 := c!:cval(cadr x, env);
    c!:outop(car x, r2:=c!:newreg(), cdr env, r1);
    return r2
  end;

symbolic procedure c!:builtin_two(x, env);
  begin
    scalar a1, a2, r, rr;
    a1 := cadr x;
    a2 := caddr x;
    rr := c!:pareval(list(a1, a2), env);
    c!:outop(car x, r:=c!:newreg(), car rr, cadr rr);
    return r
  end;

symbolic procedure c!:narg(x, env);
  c!:cval(expand(cdr x, get(car x, 'c!:binary_version)), env);

for each n in
   '((plus plus2)
     (times times2)
     (iplus iplus2)
     (itimes itimes2)) do <<
        put(car n, 'c!:binary_version, cadr n);
        put(car n, 'c!:code, function c!:narg) >>;

!#if common!-lisp!-mode
for each n in
   '((!+ plus2)
     (!* times2)) do <<
        put(car n, 'c!:binary_version, cadr n);
        put(car n, 'c!:code, function c!:narg) >>;
!#endif

symbolic procedure c!:cplus2(u, env);
  begin
    scalar a, b;
    a := s!:improve cadr u;
    b := s!:improve caddr u;
    return if numberp a and numberp b then c!:cval(a+b, env)
       else if a = 0 then c!:cval(b, env)
       else if a = 1 then c!:cval(list('add1, b), env)
       else if b = 0 then c!:cval(a, env)
       else if b = 1 then c!:cval(list('add1, a), env)
       else if b = -1 then c!:cval(list('sub1, a), env)
       else c!:ccall(car u, cdr u, env)
  end;

put('plus2, 'c!:code, function c!:cplus2);

symbolic procedure c!:ciplus2(u, env);
  begin
    scalar a, b;
    a := s!:improve cadr u;
    b := s!:improve caddr u;
    return if numberp a and numberp b then c!:cval(a+b, env)
       else if a = 0 then c!:cval(b, env)
       else if a = 1 then c!:cval(list('iadd1, b), env)
       else if b = 0 then c!:cval(a, env)
       else if b = 1 then c!:cval(list('iadd1, a), env)
       else if b = -1 then c!:cval(list('isub1, a), env)
       else c!:builtin_two(u, env)
  end;

put('iplus2, 'c!:code, function c!:ciplus2);

symbolic procedure c!:cdifference(u, env);
  begin
    scalar a, b;
    a := s!:improve cadr u;
    b := s!:improve caddr u;
    return if numberp a and numberp b then c!:cval(a-b, env)
       else if a = 0 then c!:cval(list('minus, b), env)
       else if b = 0 then c!:cval(a, env)
       else if b = 1 then c!:cval(list('sub1, a), env)
       else if b = -1 then c!:cval(list('add1, a), env)
       else c!:ccall(car u, cdr u, env)
  end;

put('difference, 'c!:code, function c!:cdifference);

symbolic procedure c!:cidifference(u, env);
  begin
    scalar a, b;
    a := s!:improve cadr u;
    b := s!:improve caddr u;
    return if numberp a and numberp b then c!:cval(a-b, env)
       else if a = 0 then c!:cval(list('iminus, b), env)
       else if b = 0 then c!:cval(a, env)
       else if b = 1 then c!:cval(list('isub1, a), env)
       else if b = -1 then c!:cval(list('iadd1, a), env)
       else c!:builtin_two(u, env)
  end;

put('idifference, 'c!:code, function c!:cidifference);

symbolic procedure c!:ctimes2(u, env);
  begin
    scalar a, b;
    a := s!:improve cadr u;
    b := s!:improve caddr u;
    return if numberp a and numberp b then c!:cval(a*b, env)
       else if a = 0 or b = 0 then c!:cval(0, env)
       else if a = 1 then c!:cval(b, env)
       else if b = 1 then c!:cval(a, env)
       else if a = -1 then c!:cval(list('minus, b), env)
       else if b = -1 then c!:cval(list('minus, a), env)
       else c!:ccall(car u, cdr u, env)
  end;

put('times2, 'c!:code, function c!:ctimes2);

symbolic procedure c!:citimes2(u, env);
  begin
    scalar a, b;
    a := s!:improve cadr u;
    b := s!:improve caddr u;
    return if numberp a and numberp b then c!:cval(a*b, env)
       else if a = 0 or b = 0 then c!:cval(0, env)
       else if a = 1 then c!:cval(b, env)
       else if b = 1 then c!:cval(a, env)
       else if a = -1 then c!:cval(list('iminus, b), env)
       else if b = -1 then c!:cval(list('iminus, a), env)
       else c!:builtin_two(u, env)
  end;

put('itimes2, 'c!:code, function c!:citimes2);

symbolic procedure c!:cminus(u, env);
  begin
    scalar a, b;
    a := s!:improve cadr u;
    return if numberp a then c!:cval(-a, env)
       else if eqcar(a, 'minus) then c!:cval(cadr a, env)
       else c!:ccall(car u, cdr u, env)
  end;

put('minus, 'c!:code, function c!:cminus);

symbolic procedure c!:ceq(x, env);
  begin
    scalar a1, a2, r, rr;
    a1 := s!:improve cadr x;
    a2 := s!:improve caddr x;
    if a1 = nil then return c!:cval(list('null, a2), env)
    else if a2 = nil then return c!:cval(list('null, a1), env);
    rr := c!:pareval(list(a1, a2), env);
    c!:outop('eq, r:=c!:newreg(), car rr, cadr rr);
    return r
  end;

put('eq, 'c!:code, function c!:ceq);

symbolic procedure c!:cequal(x, env);
  begin
    scalar a1, a2, r, rr;
    a1 := s!:improve cadr x;
    a2 := s!:improve caddr x;
    if a1 = nil then return c!:cval(list('null, a2), env)
    else if a2 = nil then return c!:cval(list('null, a1), env);
    rr := c!:pareval(list(a1, a2), env);
    c!:outop((if c!:eqvalid a1 or c!:eqvalid a2 then 'eq else 'equal),
          r:=c!:newreg(), car rr, cadr rr);
    return r
  end;

put('equal, 'c!:code, function c!:cequal);


%
% The next few cases are concerned with demoting functions that use
% equal tests into ones that use eq instead

symbolic procedure c!:is_fixnum x;
   fixp x and x >= -134217728 and x <= 134217727;

symbolic procedure c!:certainlyatom x;
   null x or x=t or c!:is_fixnum x or
   (eqcar(x, 'quote) and (symbolp cadr x or c!:is_fixnum cadr x));

symbolic procedure c!:atomlist1 u;
  atom u or
  ((symbolp car u or c!:is_fixnum car u) and c!:atomlist1 cdr u);

symbolic procedure c!:atomlist x;
  null x or
  (eqcar(x, 'quote) and c!:atomlist1 cadr x) or
  (eqcar(x, 'list) and
   (null cdr x or
    (c!:certainlyatom cadr x and
     c!:atomlist ('list . cddr x)))) or
  (eqcar(x, 'cons) and
   c!:certainlyatom cadr x and
   c!:atomlist caddr x);

symbolic procedure c!:atomcar x;
  (eqcar(x, 'cons) or eqcar(x, 'list)) and
  not null cdr x and
  c!:certainlyatom cadr x;

symbolic procedure c!:atomkeys1 u;
  atom u or
  (not atom car u and
   (symbolp caar u or c!:is_fixnum caar u) and 
   c!:atomlist1 cdr u);

symbolic procedure c!:atomkeys x;
  null x or
  (eqcar(x, 'quote) and c!:atomkeys1 cadr x) or
  (eqcar(x, 'list) and
   (null cdr x or
    (c!:atomcar cadr x and
     c!:atomkeys ('list . cddr x)))) or
  (eqcar(x, 'cons) and
   c!:atomcar cadr x and
   c!:atomkeys caddr x);

!#if (not common!-lisp!-mode)

symbolic procedure c!:comsublis x;
   if c!:atomkeys cadr x then 'subla . cdr x
   else nil;

put('sublis, 'c!:compile_macro, function c!:comsublis);

symbolic procedure c!:comassoc x;
   if c!:certainlyatom cadr x or c!:atomkeys caddr x then 'atsoc . cdr x
   else nil;

put('assoc, 'c!:compile_macro, function c!:comassoc);
put('assoc!*!*, 'c!:compile_macro, function c!:comassoc);

symbolic procedure c!:commember x;
   if c!:certainlyatom cadr x or c!:atomlist caddr x then 'memq . cdr x
   else nil;

put('member, 'c!:compile_macro, function c!:commember);

symbolic procedure c!:comdelete x;
   if c!:certainlyatom cadr x or c!:atomlist caddr x then 'deleq . cdr x
   else nil;

put('delete, 'c!:compile_macro, function c!:comdelete);

!#endif

symbolic procedure c!:ctestif(x, env, d1, d2);
  begin
    scalar l1, l2;
    l1 := c!:my_gensym();
    l2 := c!:my_gensym();
    c!:jumpif(cadr x, l1, l2);
    x := cddr x;
    c!:startblock l1;
    c!:jumpif(car x, d1, d2);
    c!:startblock l2;
    c!:jumpif(cadr x, d1, d2)
  end;

put('if, 'c!:ctest, function c!:ctestif);

symbolic procedure c!:ctestnull(x, env, d1, d2);
  c!:cjumpif(cadr x, env, d2, d1);

put('null, 'c!:ctest, function c!:ctestnull);
put('not, 'c!:ctest, function c!:ctestnull);

symbolic procedure c!:ctestatom(x, env, d1, d2);
  begin
    x := c!:cval(cadr x, env);
    c!:endblock(list('ifatom, x), list(d1, d2))
  end;

put('atom, 'c!:ctest, function c!:ctestatom);

symbolic procedure c!:ctestconsp(x, env, d1, d2);
  begin
    x := c!:cval(cadr x, env);
    c!:endblock(list('ifatom, x), list(d2, d1))
  end;

put('consp, 'c!:ctest, function c!:ctestconsp);

symbolic procedure c!:ctestsymbol(x, env, d1, d2);
  begin
    x := c!:cval(cadr x, env);
    c!:endblock(list('ifsymbol, x), list(d1, d2))
  end;

put('idp, 'c!:ctest, function c!:ctestsymbol);

symbolic procedure c!:ctestnumberp(x, env, d1, d2);
  begin
    x := c!:cval(cadr x, env);
    c!:endblock(list('ifnumber, x), list(d1, d2))
  end;

put('numberp, 'c!:ctest, function c!:ctestnumberp);

symbolic procedure c!:ctestizerop(x, env, d1, d2);
  begin
    x := c!:cval(cadr x, env);
    c!:endblock(list('ifizerop, x), list(d1, d2))
  end;

put('izerop, 'c!:ctest, function c!:ctestizerop);

symbolic procedure c!:ctesteq(x, env, d1, d2);
  begin
    scalar a1, a2, r;
    a1 := cadr x;
    a2 := caddr x;
    if a1 = nil then return c!:cjumpif(a2, env, d2, d1)
    else if a2 = nil then return c!:cjumpif(a1, env, d2, d1);
    r := c!:pareval(list(a1, a2), env);
    c!:endblock('ifeq . r, list(d1, d2))
  end;

put('eq, 'c!:ctest, function c!:ctesteq);

symbolic procedure c!:ctesteqcar(x, env, d1, d2);
  begin
    scalar a1, a2, r, d3;
    a1 := cadr x;
    a2 := caddr x;
    d3 := c!:my_gensym();
    r := c!:pareval(list(a1, a2), env);
    c!:endblock(list('ifatom, car r), list(d2, d3));
    c!:startblock d3;
    c!:outop('qcar, car r, nil, car r);
    c!:endblock('ifeq . r, list(d1, d2))
  end;

put('eqcar, 'c!:ctest, function c!:ctesteqcar);

global '(least_fixnum greatest_fixnum);

least_fixnum := -expt(2, 27);
greatest_fixnum := expt(2, 27) - 1;

symbolic procedure c!:small_number x;
  fixp x and x >= least_fixnum and x <= greatest_fixnum;

symbolic procedure c!:eqvalid x;
  if atom x then c!:small_number x
  else if flagp(car x, 'c!:fixnum_fn) then t
  else car x = 'quote and (idp cadr x or c!:small_number cadr x);

flag('(iplus iplus2 idifference iminus itimes itimes2), 'c!:fixnum_fn);

symbolic procedure c!:ctestequal(x, env, d1, d2);
  begin
    scalar a1, a2, r;
    a1 := s!:improve cadr x;
    a2 := s!:improve caddr x;
    if a1 = nil then return c!:cjumpif(a2, env, d2, d1)
    else if a2 = nil then return c!:cjumpif(a1, env, d2, d1);
    r := c!:pareval(list(a1, a2), env);
    c!:endblock((if c!:eqvalid a1 or c!:eqvalid a2 then 'ifeq else 'ifequal) .
                  r, list(d1, d2))
  end;

put('equal, 'c!:ctest, function c!:ctestequal);

symbolic procedure c!:ctestilessp(x, env, d1, d2);
  begin
    scalar r;
    r := c!:pareval(list(cadr x, caddr x), env);
    c!:endblock('ifilessp . r, list(d1, d2))
  end;

put('ilessp, 'c!:ctest, function c!:ctestilessp);

symbolic procedure c!:ctestigreaterp(x, env, d1, d2);
  begin
    scalar r;
    r := c!:pareval(list(cadr x, caddr x), env);
    c!:endblock('ifigreaterp . r, list(d1, d2))
  end;

put('igreaterp, 'c!:ctest, function c!:ctestigreaterp);

symbolic procedure c!:ctestand(x, env, d1, d2);
  begin
    scalar next;
    for each a in cdr x do <<
      next := c!:my_gensym();
      c!:cjumpif(a, env, next, d2);
      c!:startblock next >>;
    c!:endblock('goto, list d1)
  end;

put('and, 'c!:ctest, function c!:ctestand);

symbolic procedure c!:ctestor(x, env, d1, d2);
  begin
    scalar next;
    for each a in cdr x do <<
      next := c!:my_gensym();
      c!:cjumpif(a, env, d1, next);
      c!:startblock next >>;
    c!:endblock('goto, list d2)
  end;

put('or, 'c!:ctest, function c!:ctestor);

% Here are some of the things that are built into the Lisp kernel
% and that I am happy to allow the compiler to generate direct calls to.
% But NOTE that if any of these were callable with eg either 1 or 2 args
% I would need DIFFERENT C entrypoints for each such case. To that effect
% I need to change this to have
%      c!:c_entrypoint1, c!:c_entrypoint2 and c!:c_entrypointn
% rather than a single property name.

<< put('abs,                     'c!:c_entrypoint, "Labsval");
%  put('acons,                   'c!:c_entrypoint, "Lacons");
%  put('add1,                    'c!:c_entrypoint, "Ladd1");
!#if common!-lisp!-mode
   put('!1!+,                    'c!:c_entrypoint, "Ladd1");
!#endif
!#if (not common!-lisp!-mode)
   put('append,                  'c!:c_entrypoint, "Lappend");
!#endif
%  put('apply,                   'c!:c_entrypoint, "Lapply");
   put('apply0,                  'c!:c_entrypoint, "Lapply0");
   put('apply1,                  'c!:c_entrypoint, "Lapply1");
   put('apply2,                  'c!:c_entrypoint, "Lapply2");
   put('apply3,                  'c!:c_entrypoint, "Lapply3");
%  put('ash,                     'c!:c_entrypoint, "Lash");
   put('ash1,                    'c!:c_entrypoint, "Lash1");
!#if (not common!-lisp!-mode)
   put('assoc,                   'c!:c_entrypoint, "Lassoc");
!#endif
   put('atan,                    'c!:c_entrypoint, "Latan");
   put('atom,                    'c!:c_entrypoint, "Latom");
   put('atsoc,                   'c!:c_entrypoint, "Latsoc");
   put('batchp,                  'c!:c_entrypoint, "Lbatchp");
   put('boundp,                  'c!:c_entrypoint, "Lboundp");
   put('bps!-putv,               'c!:c_entrypoint, "Lbpsputv");
   put('caaaar,                  'c!:c_entrypoint, "Lcaaaar");
   put('caaadr,                  'c!:c_entrypoint, "Lcaaadr");
   put('caaar,                   'c!:c_entrypoint, "Lcaaar");
   put('caadar,                  'c!:c_entrypoint, "Lcaadar");
   put('caaddr,                  'c!:c_entrypoint, "Lcaaddr");
   put('caadr,                   'c!:c_entrypoint, "Lcaadr");
   put('caar,                    'c!:c_entrypoint, "Lcaar");
   put('cadaar,                  'c!:c_entrypoint, "Lcadaar");
   put('cadadr,                  'c!:c_entrypoint, "Lcadadr");
   put('cadar,                   'c!:c_entrypoint, "Lcadar");
   put('caddar,                  'c!:c_entrypoint, "Lcaddar");
   put('cadddr,                  'c!:c_entrypoint, "Lcadddr");
   put('caddr,                   'c!:c_entrypoint, "Lcaddr");
   put('cadr,                    'c!:c_entrypoint, "Lcadr");
   put('car,                     'c!:c_entrypoint, "Lcar");
   put('cdaaar,                  'c!:c_entrypoint, "Lcdaaar");
   put('cdaadr,                  'c!:c_entrypoint, "Lcdaadr");
   put('cdaar,                   'c!:c_entrypoint, "Lcdaar");
   put('cdadar,                  'c!:c_entrypoint, "Lcdadar");
   put('cdaddr,                  'c!:c_entrypoint, "Lcdaddr");
   put('cdadr,                   'c!:c_entrypoint, "Lcdadr");
   put('cdar,                    'c!:c_entrypoint, "Lcdar");
   put('cddaar,                  'c!:c_entrypoint, "Lcddaar");
   put('cddadr,                  'c!:c_entrypoint, "Lcddadr");
   put('cddar,                   'c!:c_entrypoint, "Lcddar");
   put('cdddar,                  'c!:c_entrypoint, "Lcdddar");
   put('cddddr,                  'c!:c_entrypoint, "Lcddddr");
   put('cdddr,                   'c!:c_entrypoint, "Lcdddr");
   put('cddr,                    'c!:c_entrypoint, "Lcddr");
   put('cdr,                     'c!:c_entrypoint, "Lcdr");
   put('char!-code,              'c!:c_entrypoint, "Lchar_code");
   put('close,                   'c!:c_entrypoint, "Lclose");
   put('code!-char,              'c!:c_entrypoint, "Lcode_char");
   put('codep,                   'c!:c_entrypoint, "Lcodep");
!#if (not common!-lisp!-mode)
   put('compress,                'c!:c_entrypoint, "Lcompress");
!#endif
   put('constantp,               'c!:c_entrypoint, "Lconstantp");
%  put('cons,                    'c!:c_entrypoint, "Lcons");
   put('date,                    'c!:c_entrypoint, "Ldate");
   put('deleq,                   'c!:c_entrypoint, "Ldeleq");
!#if (not common!-lisp!-mode)
   put('delete,                  'c!:c_entrypoint, "Ldelete");
!#endif
%  put('difference,              'c!:c_entrypoint, "Ldifference2");
   put('digit,                   'c!:c_entrypoint, "Ldigitp");
!#if (not common!-lisp!-mode)
   put('divide,                  'c!:c_entrypoint, "Ldivide");
!#endif
   put('eject,                   'c!:c_entrypoint, "Leject");
   put('endp,                    'c!:c_entrypoint, "Lendp");
   put('eq,                      'c!:c_entrypoint, "Leq");
   put('eqcar,                   'c!:c_entrypoint, "Leqcar");
   put('eql,                     'c!:c_entrypoint, "Leql");
   put('eqn,                     'c!:c_entrypoint, "Leqn");
!#if common!-lisp!-mode
   put('equal,                   'c!:c_entrypoint, "Lcl_equal");
!#else
   put('equal,                   'c!:c_entrypoint, "Lequal");
!#endif
   put('error,                   'c!:c_entrypoint, "Lerror");
   put('error1,                  'c!:c_entrypoint, "Lerror1");
%  put('errorset,                'c!:c_entrypoint, "Lerrorset");
   put('evenp,                   'c!:c_entrypoint, "Levenp");
   put('evlis,                   'c!:c_entrypoint, "Levlis");
   put('explode,                 'c!:c_entrypoint, "Lexplode");
   put('explode2,                'c!:c_entrypoint, "Lexplodec");
   put('explodec,                'c!:c_entrypoint, "Lexplodec");
   put('expt,                    'c!:c_entrypoint, "Lexpt");
   put('fasldef,                 'c!:c_entrypoint, "Lfasldef");
   put('faslstart,               'c!:c_entrypoint, "Lfaslstart");
   put('faslwrite,               'c!:c_entrypoint, "Lfaslwrite");
   put('fix,                     'c!:c_entrypoint, "Ltruncate");
   put('fixp,                    'c!:c_entrypoint, "Lfixp");
   put('flag,                    'c!:c_entrypoint, "Lflag");
   put('flagp!*!*,               'c!:c_entrypoint, "Lflagp");
   put('flagp,                   'c!:c_entrypoint, "Lflagp");
   put('flagpcar,                'c!:c_entrypoint, "Lflagpcar");
   put('float,                   'c!:c_entrypoint, "Lfloat");
   put('floatp,                  'c!:c_entrypoint, "Lfloatp");
   put('fluidp,                  'c!:c_entrypoint, "Lsymbol_specialp");
   put('gcdn,                    'c!:c_entrypoint, "Lgcd");
   put('gctime,                  'c!:c_entrypoint, "Lgctime");
   put('gensym,                  'c!:c_entrypoint, "Lgensym");
   put('gensym1,                 'c!:c_entrypoint, "Lgensym1");
   put('geq,                     'c!:c_entrypoint, "Lgeq");
   put('get!*,                   'c!:c_entrypoint, "Lget");
%  put('get,                     'c!:c_entrypoint, "Lget");
   put('getenv,                  'c!:c_entrypoint, "Lgetenv");
   put('getv,                    'c!:c_entrypoint, "Lgetv");
!#if common!-lisp!-mode
   put('svref,                   'c!:c_entrypoint, "Lgetv");
!#endif
   put('globalp,                 'c!:c_entrypoint, "Lsymbol_globalp");
   put('greaterp,                'c!:c_entrypoint, "Lgreaterp");
   put('iadd1,                   'c!:c_entrypoint, "Liadd1");
   put('idifference,             'c!:c_entrypoint, "Lidifference");
   put('idp,                     'c!:c_entrypoint, "Lsymbolp");
   put('igreaterp,               'c!:c_entrypoint, "Ligreaterp");
   put('ilessp,                  'c!:c_entrypoint, "Lilessp");
   put('iminus,                  'c!:c_entrypoint, "Liminus");
   put('iminusp,                 'c!:c_entrypoint, "Liminusp");
   put('indirect,                'c!:c_entrypoint, "Lindirect");
   put('integerp,                'c!:c_entrypoint, "Lintegerp");
!#if (not common!-lisp!-mode)
   put('intern,                  'c!:c_entrypoint, "Lintern");
!#endif
   put('iplus2,                  'c!:c_entrypoint, "Liplus2");
   put('iquotient,               'c!:c_entrypoint, "Liquotient");
   put('iremainder,              'c!:c_entrypoint, "Liremainder");
   put('irightshift,             'c!:c_entrypoint, "Lirightshift");
   put('isub1,                   'c!:c_entrypoint, "Lisub1");
   put('itimes2,                 'c!:c_entrypoint, "Litimes2");
%  put('lcm,                     'c!:c_entrypoint, "Llcm");
   put('length,                  'c!:c_entrypoint, "Llength");
   put('lengthc,                 'c!:c_entrypoint, "Llengthc");
   put('leq,                     'c!:c_entrypoint, "Lleq");
   put('lessp,                   'c!:c_entrypoint, "Llessp");
   put('linelength,              'c!:c_entrypoint, "Llinelength");
%  put('list2!*,                 'c!:c_entrypoint, "Llist2star");
%  put('list2,                   'c!:c_entrypoint, "Llist2");
%  put('list3,                   'c!:c_entrypoint, "Llist3");
!#if (not common!-lisp!-mode)
   put('liter,                   'c!:c_entrypoint, "Lalpha_char_p");
!#endif
   put('load!-module,            'c!:c_entrypoint, "Lload_module");
%  put('lognot,                  'c!:c_entrypoint, "Llognot");
   put('lposn,                   'c!:c_entrypoint, "Llposn");
   put('macro!-function,         'c!:c_entrypoint, "Lmacro_function");
   put('macroexpand!-1,          'c!:c_entrypoint, "Lmacroexpand_1");
   put('macroexpand,             'c!:c_entrypoint, "Lmacroexpand");
   put('make!-bps,               'c!:c_entrypoint, "Lget_bps");
   put('make!-global,            'c!:c_entrypoint, "Lmake_global");
   put('make!-simple!-string,    'c!:c_entrypoint, "Lsmkvect");
   put('make!-special,           'c!:c_entrypoint, "Lmake_special");
   put('mapstore,                'c!:c_entrypoint, "Lmapstore");
   put('max2,                    'c!:c_entrypoint, "Lmax2");
!#if (not common!-lisp!-mode)
   put('member,                  'c!:c_entrypoint, "Lmember");
!#endif
   put('memq,                    'c!:c_entrypoint, "Lmemq");
   put('min2,                    'c!:c_entrypoint, "Lmin2");
   put('minus,                   'c!:c_entrypoint, "Lminus");
   put('minusp,                  'c!:c_entrypoint, "Lminusp");
   put('mkquote,                 'c!:c_entrypoint, "Lmkquote");
   put('mkvect,                  'c!:c_entrypoint, "Lmkvect");
   put('mod,                     'c!:c_entrypoint, "Lmod");
   put('modular!-difference,     'c!:c_entrypoint, "Lmodular_difference");
   put('modular!-expt,           'c!:c_entrypoint, "Lmodular_expt");
   put('modular!-minus,          'c!:c_entrypoint, "Lmodular_minus");
   put('modular!-number,         'c!:c_entrypoint, "Lmodular_number");
   put('modular!-plus,           'c!:c_entrypoint, "Lmodular_plus");
   put('modular!-quotient,       'c!:c_entrypoint, "Lmodular_quotient");
   put('modular!-reciprocal,     'c!:c_entrypoint, "Lmodular_reciprocal");
   put('modular!-times,          'c!:c_entrypoint, "Lmodular_times");
   put('nconc,                   'c!:c_entrypoint, "Lnconc");
%  put('ncons,                   'c!:c_entrypoint, "Lncons");
   put('neq,                     'c!:c_entrypoint, "Lneq");
%  put('next!-random!-number,    'c!:c_entrypoint, "Lnext_random");
   put('not,                     'c!:c_entrypoint, "Lnull");
   put('null,                    'c!:c_entrypoint, "Lnull");
   put('numberp,                 'c!:c_entrypoint, "Lnumberp");
   put('oddp,                    'c!:c_entrypoint, "Loddp");
   put('onep,                    'c!:c_entrypoint, "Lonep");
   put('orderp,                  'c!:c_entrypoint, "Lorderp");
%  put('ordp,                    'c!:c_entrypoint, "Lorderp");
   put('pagelength,              'c!:c_entrypoint, "Lpagelength");
   put('pairp,                   'c!:c_entrypoint, "Lconsp");
   put('plist,                   'c!:c_entrypoint, "Lplist");
%  put('plus2,                   'c!:c_entrypoint, "Lplus2");
   put('plusp,                   'c!:c_entrypoint, "Lplusp");
   put('posn,                    'c!:c_entrypoint, "Lposn");
!#if (not common!-lisp!-mode)
   put('prin,                    'c!:c_entrypoint, "Lprin");
   put('prin1,                   'c!:c_entrypoint, "Lprin");
   put('prin2,                   'c!:c_entrypoint, "Lprinc");
   put('princ,                   'c!:c_entrypoint, "Lprinc");
   put('print,                   'c!:c_entrypoint, "Lprint");
   put('printc,                  'c!:c_entrypoint, "Lprintc");
!#endif
   put('put,                     'c!:c_entrypoint, "Lputprop");
   put('putv!-char,              'c!:c_entrypoint, "Lsputv");
   put('putv,                    'c!:c_entrypoint, "Lputv");
   put('qcaar,                   'c!:c_entrypoint, "Lcaar");
   put('qcadr,                   'c!:c_entrypoint, "Lcadr");
   put('qcar,                    'c!:c_entrypoint, "Lcar");
   put('qcdar,                   'c!:c_entrypoint, "Lcdar");
   put('qcddr,                   'c!:c_entrypoint, "Lcddr");
   put('qcdr,                    'c!:c_entrypoint, "Lcdr");
   put('qgetv,                   'c!:c_entrypoint, "Lgetv");
%  put('quotient,                'c!:c_entrypoint, "Lquotient");
%  put('random,                  'c!:c_entrypoint, "Lrandom");
%  put('rational,                'c!:c_entrypoint, "Lrational");
   put('rdf,                     'c!:c_entrypoint, "Lrdf");
   put('rds,                     'c!:c_entrypoint, "Lrds");
!#if (not common!-lisp!-mode)
   put('read,                    'c!:c_entrypoint, "Lread");
   put('readch,                  'c!:c_entrypoint, "Lreadch");
!#endif
   put('reclaim,                 'c!:c_entrypoint, "Lgc");
%  put('remainder,               'c!:c_entrypoint, "Lrem");
   put('remd,                    'c!:c_entrypoint, "Lremd");
   put('remflag,                 'c!:c_entrypoint, "Lremflag");
   put('remob,                   'c!:c_entrypoint, "Lunintern");
   put('remprop,                 'c!:c_entrypoint, "Lremprop");
   put('representation,          'c!:c_entrypoint, "Lrepresentation");
   put('reverse,                 'c!:c_entrypoint, "Lreverse");
   put('reversip,                'c!:c_entrypoint, "Lnreverse");
   put('rplaca,                  'c!:c_entrypoint, "Lrplaca");
   put('rplacd,                  'c!:c_entrypoint, "Lrplacd");
   put('schar,                   'c!:c_entrypoint, "Lsgetv");
   put('seprp,                   'c!:c_entrypoint, "Lwhitespace_char_p");
   put('set!-small!-modulus,     'c!:c_entrypoint, "Lset_small_modulus");
   put('set,                     'c!:c_entrypoint, "Lset");
   put('smemq,                   'c!:c_entrypoint, "Lsmemq");
   put('spaces,                  'c!:c_entrypoint, "Lxtab");
   put('special!-char,           'c!:c_entrypoint, "Lspecial_char");
   put('special!-form!-p,        'c!:c_entrypoint, "Lspecial_form_p");
   put('spool,                   'c!:c_entrypoint, "Lspool");
   put('stop,                    'c!:c_entrypoint, "Lstop");
   put('stringp,                 'c!:c_entrypoint, "Lstringp");
%  put('sub1,                    'c!:c_entrypoint, "Lsub1");
!#if common!-lisp!-mode
   put('!1!-,                    'c!:c_entrypoint, "Lsub1");
!#endif
   put('subla,                   'c!:c_entrypoint, "Lsubla");
!#if (not common!-lisp!-mode)
   put('sublis,                  'c!:c_entrypoint, "Lsublis");
!#endif
   put('subst,                   'c!:c_entrypoint, "Lsubst");
   put('symbol!-env,             'c!:c_entrypoint, "Lsymbol_env");
   put('symbol!-function,        'c!:c_entrypoint, "Lsymbol_function");
   put('symbol!-name,            'c!:c_entrypoint, "Lsymbol_name");
   put('symbol!-set!-definition, 'c!:c_entrypoint, "Lsymbol_set_definition");
   put('symbol!-set!-env,        'c!:c_entrypoint, "Lsymbol_set_env");
   put('symbol!-value,           'c!:c_entrypoint, "Lsymbol_value");
   put('system,                  'c!:c_entrypoint, "Lsystem");
   put('terpri,                  'c!:c_entrypoint, "Lterpri");
   put('threevectorp,            'c!:c_entrypoint, "Lthreevectorp");
   put('time,                    'c!:c_entrypoint, "Ltime");
%  put('times2,                  'c!:c_entrypoint, "Ltimes2");
   put('ttab,                    'c!:c_entrypoint, "Lttab");
   put('tyo,                     'c!:c_entrypoint, "Ltyo");
   put('unmake!-global,          'c!:c_entrypoint, "Lunmake_global");
   put('unmake!-special,         'c!:c_entrypoint, "Lunmake_special");
   put('upbv,                    'c!:c_entrypoint, "Lupbv");
!#if common!-lisp!-mode
   put('vectorp,                 'c!:c_entrypoint, "Lvectorp");
!#else
   put('vectorp,                 'c!:c_entrypoint, "Lsimple_vectorp");
!#endif
   put('verbos,                  'c!:c_entrypoint, "Lverbos");
   put('wrs,                     'c!:c_entrypoint, "Lwrs");
   put('xcons,                   'c!:c_entrypoint, "Lxcons");
   put('xtab,                    'c!:c_entrypoint, "Lxtab");
%  put('orderp,                  'c!:c_entrypoint, "Lorderp"); being retired.
   put('zerop,                   'c!:c_entrypoint, "Lzerop");

% The following can be called without having to provide an environment
% or arg-count.  The compiler should check the number of args being
% passed matches the expected number.

   put('cons,                    'c!:direct_entrypoint, 2 . "cons");
   put('ncons,                   'c!:direct_entrypoint, 1 . "ncons");
   put('list2,                   'c!:direct_entrypoint, 2 . "list2");
   put('list2!*,                 'c!:direct_entrypoint, 3 . "list2star");
   put('acons,                   'c!:direct_entrypoint, 3 . "acons");
   put('list3,                   'c!:direct_entrypoint, 3 . "list3");
   put('plus2,                   'c!:direct_entrypoint, 2 . "plus2");
   put('difference,              'c!:direct_entrypoint, 2 . "difference2");
   put('add1,                    'c!:direct_entrypoint, 1 . "add1");
   put('sub1,                    'c!:direct_entrypoint, 1 . "sub1");
!#if (not common!-lisp!-mode)
   put('get,                     'c!:direct_entrypoint, 2 . "get");
!#endif
   put('lognot,                  'c!:direct_entrypoint, 1 . "lognot");
   put('ash,                     'c!:direct_entrypoint, 2 . "ash");
   put('quotient,                'c!:direct_entrypoint, 2 . "quot2");
   put('remainder,               'c!:direct_entrypoint, 2 . "Cremainder");
   put('times2,                  'c!:direct_entrypoint, 2 . "times2");
   put('minus,                   'c!:direct_entrypoint, 1 . "negate");
   put('rational,                'c!:direct_entrypoint, 1 . "rational");

   put('lessp,                   'c!:direct_predicate, 2 . "lessp2");
   put('leq,                     'c!:direct_predicate, 2 . "lesseq2");
   put('greaterp,                'c!:direct_predicate, 2 . "greaterp2");
   put('geq,                     'c!:direct_predicate, 2 . "geq2");
   put('zerop,                   'c!:direct_predicate, 1 . "zerop");

   "C entrypoints established" >>;

flag(
 '(atom atsoc codep constantp deleq digit endp eq eqcar evenp
   eql fixp flagp flagpcar floatp get globalp iadd1 idifference idp
   igreaterp ilessp iminus iminusp indirect integerp iplus2 irightshift
   isub1 itimes2 liter memq minusp modular!-difference modular!-expt
   modular!-minus modular!-number modular!-plus modular!-times not
   null numberp onep pairp plusp qcaar qcadr qcar qcdar qcddr
   qcdr remflag remprop reversip seprp special!-form!-p stringp
   symbol!-env symbol!-name symbol!-value threevectorp vectorp zerop),
 'c!:no_errors);

end;

% End of ccomp.red

