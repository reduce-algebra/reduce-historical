module matrix;  % Header for matrix package.

% Author: Anthony C. Hearn.

% This module is rife with essential references to RPLAC-based
% functions.

create!-package('(matrix matpri extops bareiss det glmat nullsp rank
                  resultant cofactor),nil);

fluid '(!*sub2 subfg!*);

global '(nxtsym!*);

symbolic procedure matrix u;
   % Declares list U as matrices.
   begin scalar v,w,x;
        for each j in u do
           if atom j then if null (x := gettype j)
                            then put(j,'rtype,'matrix)
                           else if x eq 'matrix
                            then <<lprim list(x,j,"redefined");
                                   put(j,'rtype,'matrix)>>
                           else typerr(list(x,j),"matrix")
            else if not idp car j
                   or length (v := revlis cdr j) neq 2
                   or not natnumlis v
             then errpri2(j,'hold)
            else if not (x := gettype car j) or x eq 'matrix
             then <<w := nil;
                    for n := 1:car v do w := nzero cadr v . w;
                    put(car j,'rtype,'matrix);
                    put(car j,'avalue,list('matrix,'mat . w))>>
            else typerr(list(x,car j),"matrix")
   end;

symbolic procedure natnumlis u;
   % True if U is a list of natural numbers.
   null u or fixp car u and car u>0 and natnumlis cdr u;

rlistat '(matrix);

symbolic procedure nzero n;
   % Returns a list of N zeros.
   if n=0 then nil else 0 . nzero(n-1);

% Parsing interface.

symbolic procedure matstat;
   % Read a matrix.
   begin scalar x,y;
      if not (nxtsym!* eq '!() then symerr("Syntax error",nil);
   a: scan();
      if not (scan() eq '!*lpar!*) then symerr("Syntax error",nil);
      y := xread 'paren;
      if not eqcar(y,'!*comma!*) then y := list y else y := remcomma y;
      x := y . x;
      if nxtsym!* eq '!)
        then return <<scan(); scan(); 'mat . reversip x>>
       else if not(nxtsym!* eq '!,) then symerr("Syntax error",nil);
      go to a
   end;

put('mat,'stat,'matstat);

symbolic procedure formmat(u,vars,mode);
   'list . mkquote car u
     . for each x in cdr u collect('list . formlis(x,vars,mode));

put('mat,'formfn,'formmat);

put('mat,'i2d,'mkscalmat);

put('mat,'inversefn,'matinverse);

put('mat,'lnrsolvefn,'lnrsolve);

put('mat,'rtypefn,'quotematrix);

symbolic procedure quotematrix u; 'matrix;

flag('(mat tp),'matflg);

flag('(mat),'noncommuting);

put('mat,'prifn,'matpri);

flag('(mat),'struct);      % for parsing

put('matrix,'fn,'matflg);

put('matrix,'evfn,'matsm!*);

flag('(matrix),'sprifn);

put('matrix,'tag,'mat);

put('matrix,'lengthfn,'matlength);

put('matrix,'getelemfn,'getmatelem);

put('matrix,'setelemfn,'setmatelem);

symbolic procedure mkscalmat u;
   % Converts id u to 1 by 1 matrix.
   list('mat,list u);

symbolic procedure getmatelem u;
   begin scalar x;
      x := get(car u,'avalue);
      if null x or not(car x eq 'matrix) then typerr(car u,"matrix")
       else if not eqcar(x := cadr x,'mat)
        then if idp x
         then return getmatelem (x . cdr u)
         else rerror(matrix,1,list("Matrix",car u,"not set"))
        else if not numlis (u := revlis cdr u) or length u neq 2
         then errpri2(x . u,t)
         else return nth(nth(cdr x,car u),cadr u);
   end;

symbolic procedure setmatelem(u,v);
   begin scalar x;
     x := get(car u,'avalue);
     if null x then typerr(car u,"matrix")
      else return letmtr(u,v,cadr x)
   end;

symbolic procedure matlength u;
   if not eqcar(u,'mat) then rerror(matrix,2,list("Matrix",u,"not set"))
    else list('list,length cdr u,length cadr u);

% Aggregate Property.  Commented out for now.

% symbolic procedure matrixmap(u,v);
%    if flagp(car u,'matmapfn)
%     then matsm!*1 for each j in matsm cadr u collect
%             for each k in j collect simp!*(car u . mk!*sq k . cddr u)
%    else if flagp(car u,'matfn) then reval2(u,v)
%     else typerr(car u,"matrix operator");

% put('matrix,'aggregatefn,'matrixmap);

% flag('(int df),'matmapfn);

% flag('(det trace),'matfn);

symbolic procedure matsm!*(u,v);
   % Matrix expression simplification function.
   matsm!*1 matsm u;

% symbolic procedure matsm!*1 u;
%    begin scalar sub2;
%       sub2 := !*sub2;  % Since we need value for each element.
%       u := 'mat . for each j in u collect
%                      for each k in j
%                         collect <<!*sub2 := sub2; !*q2a subs2 k>>;
%       !*sub2 := nil;   % Since all substitutions done.
%       return u
%    end;

symbolic procedure matsm!*1 u;
   begin
      % We use subs2!* to make sure each element simplified fully.
      u := 'mat . for each j in u collect
                     for each k in j collect !*q2a subs2!* k;
      !*sub2 := nil;   % Since all substitutions done.
      return u
   end;

symbolic procedure mk!*sq2 u;
   begin scalar x;
        x := !*sub2;   % Since we need value for each element.
        u := subs2 u;
        !*sub2 := x;
        return mk!*sq u
   end;

symbolic procedure matsm u;
   begin scalar x,y;
      for each j in nssimp(u,'matrix) do
         <<y := multsm(car j,matsm1 cdr j);
           x := if null x then y else addm(x,y)>>;
      return x
   end;

symbolic procedure matsm1 u;
   %returns matrix canonical form for matrix symbol product U;
   begin scalar x,y,z; integer n;
    a:  if null u then return z
         else if eqcar(car u,'!*div) then go to d
         else if atom car u then go to er
         else if caar u eq 'mat then go to c1
         else x := lispapply(caar u,cdar u);
    b:  z := if null z then x
              else if null cdr z and null cdar z then multsm(caar z,x)
              else multm(x,z);
    c:  u := cdr u;
        go to a;
    c1: if not lchk cdar u then rerror(matrix,3,"Matrix mismatch");
        x := for each j in cdar u collect
                for each k in j collect xsimp k;
        go to b;
    d:  y := matsm cadar u;
        if (n := length car y) neq length y
          then rerror(matrix,4,"Non square matrix")
         else if (z and n neq length z)
          then rerror(matrix,5,"Matrix mismatch")
         else if cddar u then go to h
         else if null cdr y and null cdar y then go to e;
        x := subfg!*;
        subfg!* := nil;
        if null z then z := apply1(get('mat,'inversefn),y)
         else if null(x := get('mat,'lnrsolvefn))
          then z := multm(apply1(get('mat,'inversefn),y),z)
         else z := apply2(get('mat,'lnrsolvefn),y,z);
        subfg!* := x;
        % Make sure there are no power substitutions.
        z := for each j in z collect for each k in j collect
                 <<!*sub2 := t; subs2 k>>;
        go to c;
    e:  if null caaar y then rerror(matrix,6,"Zero divisor");
        y := revpr caar y;
        z := if null z then list list y else multsm(y,z);
        go to c;
     h: if null z then z := generateident n;
        go  to c;
    er: rerror(matrix,7,list("Matrix",car u,"not set"))
   end;

symbolic procedure lchk u;
   begin integer n;
        if null u or atom car u then return nil;
        n := length car u;
        repeat u := cdr u
           until null u or atom car u or length car u neq n;
        return null u
   end;

symbolic procedure addm(u,v);
   % Returns sum of two matrix canonical forms U and V.
   % Returns U + 0 as U. Patch by Francis Wright.
   if v = '(((nil . 1))) then u else       % FJW.
   for each j in addm1(u,v,function cons)
      collect addm1(car j,cdr j,function addsq);

symbolic procedure addm1(u,v,w);
   if null u and null v then nil
    else if null u or null v then rerror(matrix,8,"Matrix mismatch")
    else apply2(w,car u,car v) . addm1(cdr u,cdr v,w);

symbolic procedure tp u; tp1 matsm u;

put('tp,'rtypefn,'getrtypecar);

symbolic procedure tp1 u;
   %returns transpose of the matrix canonical form U;
   %U is destroyed in the process;
   begin scalar v,w,x,y,z;
        v := w := list nil;
        while car u do
         <<x := u;
           y := z := list nil;
           while x do
             <<z := cdr rplacd(z,list caar x);
               x := cdr rplaca(x,cdar x)>>;
           w := cdr rplacd(w,list cdr y)>>;
        return cdr v
   end;

symbolic procedure scalprod(u,v);
   %returns scalar product of two lists (vectors) U and V;
   if null u and null v then nil ./ 1
    else if null u or null v then rerror(matrix,9,"Matrix mismatch")
    else addsq(multsq(car u,car v),scalprod(cdr u,cdr v));

symbolic procedure multm(u,v);
   %returns matrix product of two matrix canonical forms U and V;
   (for each y in u
      collect for each k in x collect subs2 scalprod(y,k))
     where x = tp1 v;

symbolic procedure multsm(u,v);
   %returns product of standard quotient U and matrix standard form V;
   if u = (1 ./ 1) then v
    else for each j in v collect for each k in j collect multsq(u,k);

symbolic procedure letmtr(u,v,y);
   %substitution for matrix elements;
   begin scalar z;
        if not eqcar(y,'mat)
          then rerror(matrix,10,list("Matrix",car u,"not set"))
         else if not numlis (z := revlis cdr u) or length z neq 2
          then return errpri2(u,'hold);
        rplaca(pnth(nth(cdr y,car z),cadr z),v);
   end;

% Explicit substitution code for matrices.

symbolic procedure matsub(u,v);
   'mat . for each x in cdr v collect
              for each y in x collect subeval1(u,y);

put('matrix,'subfn,'matsub);

endmodule;


module matpri;   % Matrix printing routines.

% Author: Anthony C. Hearn.
% Modified by Arthur C. Norman.

fluid '(!*nat obrkp!* orig!* pline!* posn!* ycoord!* ymax!* ymin!*);

symbolic procedure setmatpri(u,v);
   matpri1(cdr v,u);

put('mat,'setprifn,'setmatpri);

symbolic procedure matpri u;
   matpri1(cdr u,nil);

symbolic procedure matpri1(u,x);
   % Prints a matrix canonical form U with name X.
   % Tries to do fancy display if nat flag is on.
   begin scalar m,n,r,l,w,e,ll,ok,name,nw,widths,firstflag,toprow,lbar,
                rbar,realorig;
      if !*fort
        then <<m := 1;
               if null x then x := "MAT";
               for each y in u do
                  <<n := 1;
                    for each z in y do
                       <<assgnpri(z,list list(x,m,n),'only);
                         n := n+1>>;
                    m := m+1>>;
               return nil>>;
      terpri!* t;
      if x and !*nat then <<
         name := layout!-formula(x, 0, nil);
         if name then <<
           nw := cdar name + 4;
           ok := !*nat >>>>
       else <<nw := 0; ok := !*nat>>;
      ll := linelength nil - spare!* - orig!* - nw;
      m := length car u;
      widths := mkvect(1 + m);
      for i := 1:m do putv(widths, i, 1);
      % Collect sizes for all elements to see if it will fit in
      % displayed matrix form.
      % We need to compute things wrt a zero orig for the following
      % code to work properly.
      realorig := orig!*;
      orig!* := 0;
      if ok then for each y in u do
       <<n := 1;
         l := nil;
         w := 0;
         if ok then for each z in y do if ok then <<
            e := layout!-formula(z, 0, nil);
              if null e then ok := nil
              else begin
                scalar col;
                col := max(getv(widths, n), cdar e);
% this allows for 2 blanks between cols, and also 2 extra chars, one
% for the left-bar and one for the right-bar.
                if (w := w + col + 2) > ll then ok := nil
                else <<
                  l := e . l;
                  putv(widths, n, col) >> end;
            n := n+1>>;
         r := (reverse l) . r >>;
         if ok then <<
         % Matrix will fit in displayed representation.
         % Compute format with respect to 0 posn.
         firstflag := toprow := t;
         r := for each py on reverse r collect begin
            scalar y, ymin, ymax, pos, pl, k, w;
            ymin := ymax := 0;
            pos := 1;    % Since "[" is of length 1.
            k := 1;
            pl := nil;
            y := car py;
            for each z in y do <<
               w := getv(widths, k);
               pl := append(update!-pline(pos+(w-cdar z)/2,0,caar z),
                            pl);      % Centre item in its field
               pos := pos + w + 2;    % 2 blanks between cols
               k := k + 1;
               ymin := min(ymin, cadr z);
               ymax := max(ymax, cddr z) >>;
            k := nil;
            if firstflag then firstflag := nil
             else ymax := ymax + 1;   % One blank line between rows
            for h := ymax step -1 until ymin do <<
               if toprow then <<
                  lbar := symbol 'mat!-top!-l;
                  rbar := symbol 'mat!-top!-r;
                  toprow := nil >>
                else if h = ymin and null cdr py then <<
                  lbar := symbol 'mat!-low!-l;
                  rbar := symbol 'mat!-low!-r >>
%               else lbar := rbar := symbol 'vbar;
                else <<lbar := symbol 'mat!-low!-l;
                       rbar := symbol 'mat!-low!-r>>;
               pl := ((((pos - 2) . (pos - 1)) . h) . rbar) . pl;
               k := (((0 . 1) . h) . lbar) . k >>;
            return (append(pl, k) . pos) . (ymin . ymax) end;
         orig!* := realorig;
         w := 0;
         for each y in r do w := w + (cddr y - cadr y + 1);
               % Total height.
         n := w/2;  % Height of mid-point.
         u := nil;
         for each y in r do <<
            u := append(update!-pline(0, n - cddr y, caar y), u);
            n := n - (cddr y - cadr y + 1) >>;
         if x then <<maprin x; oprin 'setq >>;
         pline!* := append(update!-pline(posn!*,ycoord!*,u),
                           pline!*);
         ymax!* := max(ycoord!* + w/2, ymax!*);
         ymin!* := min(ycoord!* + w/2 - w, ymin!*);
         terpri!*(not !*nat)>>
      else <<if x then <<maprin x; oprin 'setq>>; matpri2 u>>
   end;

symbolic procedure matpri2 u;
   begin scalar y;
      prin2!* 'mat;
      prin2!* "(";
      obrkp!* := nil;
      y := orig!*;
      orig!* := if posn!*<18 then posn!* else orig!*+3;
      while u do
         <<prin2!* "(";
           orig!* := orig!*+1;
           inprint('!*comma!*,0,car u);
           prin2!* ")";
           if cdr u
             then <<oprin '!*comma!*; orig!* := orig!*-1;
                    terpri!* !*nat>>;
           u := cdr u>>;
      obrkp!* := t;
      orig!* := y;
      prin2!* ")";
      if null !*nat then prin2!* "$";
      terpri!* t
   end;

endmodule;


module extops;  % Support for exterior multiplication.

% Author: Eberhard Schrufer.
% Modifications by: David Hartley.

Comment. Data structure for simple exterior forms is

        ex ::= nil | lpow ex .* lc ex .+ ex
        lpow ex ::= list of kernel
        lc ex   ::= sf

All forms have degree > 0. lpow ex is a list of factors in a basis form;

symbolic procedure !*sf2ex(u,v);
   %Converts standardform u into a form distributed w.r.t. v
%*** Should we check here if lc is free of v?
   if null u then nil
    else if domainp u or null(mvar u memq v) then list nil .* u .+ nil
    else list mvar u .* lc u .+ !*sf2ex(red u,v);

symbolic procedure !*ex2sf u;
   % u: ex -> !*ex2sf: sf
   % reconverts 1-form u, but doesn't check ordering
   if null u then nil
   else if car lpow u = nil then subs2chk lc u
   else car lpow u .** 1 .* subs2chk lc u .+ !*ex2sf red u;

symbolic procedure extmult(u,v);
   % u,v: ex -> extmult: ex
   % Special exterior multiplication routine.  Degree of form v is
   % arbitrary, u is a one-form. No subs2 checking
   if null u or null v then nil
    else (if x then cdr x .* (if car x then negf multf(lc u,lc v)
                               else multf(lc u,lc v))
                          .+ extadd(extmult(!*t2f lt u,red v),
                                    extmult(red u,v))
           else extadd(extmult(red u,v),extmult(!*t2f lt u,red v)))
          where x = ordexn(car lpow u,lpow v);


symbolic procedure extadd(u,v);
   % u,v: ex -> extadd: ex
   % a non-recursive exterior addition routine
   % u and v are of same degree
   % relies on setq functions for red
   if null u then v
   else if null v then u
   else
      begin scalar s,w,z;
      s := z := nil .+ nil;
      while u and v do
         if lpow v = lpow u then                % add coefficients
          <<if w := addf(lc u,lc v) then        % replace coefficient
               <<red z := lpow u .* w .+ nil; z := red z>>;
            u := red u; v := red v>>
         else if ordexp(lpow v,lpow u) then     % swap v and u
          <<red z := lt v .+ nil; v := red v; z := red z>>
         else
          <<red z := lt u .+ nil; u := red u; z := red z>>;
      red z := if u then u else v;
      return red s;
      end;

symbolic procedure ordexp(u,v);
   if null u then t
    else if car u eq car v then ordexp(cdr u,cdr v)
    else if null car u then nil
    else if null car v then t
    else ordop(car u,car v);

symbolic procedure ordexn(u,v);
   %u is a single variable, v a list. Returns nil if u is a member
   %of v or a dotted pair of a permutation indicator and the ordered
   %list of u merged into v.
   begin scalar s,x;
     a: if null v then return(s . reverse(u . x))
         else if u eq car v then return nil
         else if u and ordop(u,car v) then
                 return(s . append(reverse(u . x),v))
         else  <<x := car v . x;
                 v := cdr v;
                 s := not s>>;
         go to a
   end;

symbolic procedure quotexf!*(u,v);
   % u: ex, v: sf -> quotexf!*: ex
   % catastrophe if division fails
   if null u then nil
   else lpow u .* quotf!*(lc u,v) .+ quotexf!*(red u,v);


symbolic procedure negex u;
   % u: ex -> negex: ex
   if null u then nil
   else lpow u .* negf lc u .+ negex red u;


symbolic procedure splitup(u,v);
   % u: ex, v: list of kernel -> splitup: {ex,ex}
   % split 1-form u into part free of v (not containing nil), and rest
   % assumes u ordered wrt v
   if null u then {nil,nil}
   else if null x or memq(x,v) where x = car lpow u then {nil,u}
   else {lt u .+ car x, cadr x} where x = splitup(red u,v);


symbolic procedure innprodpex(v,u);
   % v: lpow ex, u: ex -> innprodpex: ex
   % v _| u = v _| lt u .+ v _| red u (order is correct)
   if null u then nil else
   (if x then cdr x .* (if car x then negf lc u else lc u)
                    .+ innprodpex(v,red u)
    else innprodpex(v,red u))
    where x = innprodp2(v,lpow u);


symbolic procedure innprodp2(v,u);
   % u,v: lpow ex -> innprodp2: nil or bool . lpow ex
   % returns sign of permutation as well
   % (x^y) _| u =  y _| (x _| u)
   begin
   u := nil . u;
   while v and u do
    <<u := innprodkp(nil,car v,cdr u,car u);
      v := cdr v>>;
   return u;
   end;


symbolic procedure innprodkp(w,v,u,s);
   % w,u: lpow ex or nil, v: kernel, s: bool
   % -> innprodkp: nil or bool . lpow ex
   % w,u are exterior forms, v is vector in dual space
   % calulates w^(v _| u), assuming degree u > 1 and returns sign
   % permutation as well
   if null u then nil
   else if v = car u then s . nconc(reversip w,cdr u)
   else innprodkp(car u . w,v,cdr u,not s);


symbolic procedure subs2chkex u;
   % u:ex -> subs2chkex:ex
   % Leading coefficient of return value has been subs2chk'ed
   if null u then nil
   else (if x then lpow u .* x .+ red u else subs2chkex red u)
         where x = subs2chk lc u;


symbolic procedure subs2chk u;
   % This definition allows for a power substitution that can lead to
   % a denominator in subs2.  We omit the test for !*sub2 and powlis1!*
   % to make sure the check is made.  Maybe this can be optimized.
   begin scalar x;
      if subfg!* and denr(x := subs2f u)=1 then u := numr x;
      return u
     end;

endmodule;


module bareiss; % Inversion routines using the Bareiss 2-step method.

% Author: Anthony C. Hearn.
% Modifications by: David Hartley.

% This module is rife with essential references to RPLAC-based
% functions.

fluid '(!*exp asymplis!* subfg!* wtl!* !*trsparse
        bareiss!-step!-size!*);   % !*solveinconsistent

global '(assumptions requirements);

bareiss!-step!-size!* := 2;     % Seems fastest on average.

symbolic procedure matinverse u;
   lnrsolve(u,generateident length u);

symbolic procedure lnrsolve(u,v);
   %U is a matrix standard form, V a compatible matrix form.
   %Value is U**(-1)*V.
   begin scalar temp,vlhs,vrhs,ok,
                !*exp,!*solvesingular;
   !*exp := t;
   if asymplis!* or wtl!* then
    <<temp := asymplis!* . wtl!*;
      asymplis!* := wtl!* := nil>>;
   vlhs := for i:=1:length car u collect intern gensym();
   vrhs := for i:=1:length car v collect intern gensym();
   u := car normmat augment(u,v);
   v := append(vlhs,vrhs);
   ok := setkorder v;
   u := foreach r in u collect prsum(v,r);
   v := errorset!*({function solvebareiss, mkquote u,mkquote vlhs},t);
   if caar v memq {'singular,'inconsistent} then
      <<setkorder ok; rerror(matrix,13,"Singular matrix")>>;
   v := pair(cadr s,car s) where s = cadar v;
   u := foreach j in vlhs collect
           coeffrow(negf numr q,vrhs,denr q) where q = cdr atsoc(j,v);
   setkorder ok;
   if temp then <<asymplis!* := car temp; wtl!* := cdr temp>>;
   return for each j in u collect
             for each k in j collect
                if temp then resimp k else cancel k;
   end;

symbolic procedure prsum(kl,cl);
   % kl: list of kernel, cl: list of sf -> prsum: sf
   % kl and cl assumed to have same length
   if null kl then nil
   else if null car cl then prsum(cdr kl,cdr cl)
   else car kl .** 1 .* car cl .+ prsum(cdr kl,cdr cl);

symbolic procedure solvebareiss(exlis,varlis);
   % exlis: list of sf, varlis: list of kernel
   % -> solvebareiss: tagged solution list
   % Solve linear system exlis for variables in varlis using multi-step
   % Bareiss elimination and fraction-free back-substitution.  The
   % equations in exlis are not converted to a matrix, but kept as
   % (sparse) standard forms.
   begin scalar temp;
   if asymplis!* or wtl!* then
    <<temp := asymplis!* . wtl!*; asymplis!* := wtl!* := nil>>;
   exlis := sparse_bareiss(exlis,varlis,bareiss!-step!-size!*);
   if car exlis = 'inconsistent then return 'inconsistent . nil;
   exlis := cdr exlis;
   if not !*solvesingular and length exlis < length varlis then
      return 'singular . nil;
   if !*trsparse then
      solvesparseprint("Reduced system",reverse exlis,varlis);
   exlis := sparse_backsub(exlis,varlis);
   varlis := foreach p in exlis collect car p;
   exlis := foreach p in exlis collect cdr p;
   if temp then
     <<asymplis!* := car temp;
       wtl!* := cdr temp;
       exlis := for each ex in exlis collect resimp subs2!* ex>>;
   return t . {{exlis,varlis,1}};
   end;

symbolic procedure coeffrow(u,v,d);
   % u:sf, v:list of kernel, d:sf -> coeffrow: list of sq
   % u is linear homogeneous in the kernels in v
   if null v then nil
   else if null u or mvar u neq car v
    then (nil ./ 1) . coeffrow(u,cdr v,d)
   else (lc u ./ d) . coeffrow(red u,cdr v,d);


symbolic procedure augment(u,v);
   if null u then nil else append(car u,car v) . augment(cdr u,cdr v);

symbolic procedure generateident n;
  %returns matrix canonical form of identity matrix of order N.
   begin scalar u,v;
        for i := 1:n do
         <<u := nil;
           for j := 1:n do u := ((if i=j then 1 else nil) . 1) . u;
           v := u . v>>;
        return v
   end;

symbolic procedure normmat u;
   %U is a matrix standard form.
   %Value is dotted pair of matrix polynomial form and factor.
   begin scalar x,y,z;
      x := 1;
      for each v in u do
         <<y := 1;
           for each w in v do y := lcm(y,denr w);
           z := (for each w in v
                    collect multf(numr w,quotf(y,denr w)))
              . z;
           x := multf(y,x)>>;
      return reverse z . x
   end;

symbolic procedure sparse_bareiss(u,v,k);
   % u: list of sf, v: list of kernel, k: posint
   % -> sparse_bareiss: (t|'inconsistent) . list of sf
   % Multi-step Bareiss elimination using exterior multiplication to
   % calculate and organise determinants efficiently. Individual blocks
   % are solved using Cramer's rule. Exterior forms are decomposed into
   % {constant,linear} parts in non-pivot variables (non-linear part is
   % not needed). The leading coefficient of the first expression
   % returned is the determinant of the system.
   begin scalar p,d,w,pivs,s;
   d := 1;
   u := foreach f in u join if f then {!*sf2ex(f,v)};
   while p := choose_pivot_rows(u,v,k,d) do
      begin
      u := car p; v := cadr p;  % throws out free vars as well
      p := cddr p;
      pivs := lpow car p;       % pivot variables
      u := foreach r in u join  % multi-step elim. on remaining rows
              begin
              r := splitup(r,v);
              r := extadd(extmult(cadr r,car p),extmult(car r,cadr p));
              if null (r := subs2chkex r) then return;
              return {innprodpex(pivs,quotexf!*(r,d))};
              end;
      d := lc car p;            % update divisor
      assumptions := 'list . mk!*sq !*f2q d .
                            (pairp assumptions and cdr assumptions);
      p := extadd(car p,cadr p);% recombine pivot rows
      s := evenp length pivs;
      foreach x in pivs do      % Cramer's rule on pivot rows
         w := if (s := not s) then innprodpex(delete(x,pivs),p) . w
              else negex innprodpex(delete(x,pivs),p) . w;
      end;
   foreach f in u do % inconsistent system
      requirements := 'list . mk!*sq !*f2q !*ex2sf f .
                          (pairp requirements and cdr requirements);
   return if u then 'inconsistent . nil
          else t . foreach f in w collect !*ex2sf f;
   end;

symbolic procedure choose_pivot_rows(u,v,k,d);
   % u: list of ex, v: list of kernel, k: posint, d: sf
   % -> choose_pivot_rows: nil or (list of ex).(list of kernel).ex
   % Choose pivots in the first k variables from v (or the first k-1
   % variables from the first pivot variable in v). If k pivots can't be
   % found, don't waste time looking in further columns (so number of
   % pivot rows is <= k).  If pivots found, return remaining rows,
   % remaining variables and decomposed exterior product of pivot rows.
   if null u or null v then nil else
   begin scalar w,s,ss,p,x,y,rows,pivots;
   w := u;
   for i:=1:k do if v then v := cdr v;
   while k neq 0 do
      if null u then % ran out of rows before finding k pivots
         if null v or null w or pivots then k := 0
         else <<for i:=1:k do if v then v := cdr v; s := ss; u := w>>
      else if car(x := splitup(car u,v)) and
              (y := if null pivots then car x
                    else subs2chkex extmult(car x,car pivots)) then
         begin % found one
         rows := x . rows;
         pivots := (if null pivots then y else quotexf!*(y,d)) . pivots;
         w := delete(car u,w); u := cdr u; ss := s; k := k - 1;
         end
      else <<u := cdr u; s := not s>>;
   if null pivots then return; % couldn't find any pivots
   % next line ensures return value is sgn*row1^...^rowk where
   % sgn = (-1)**(number of rows skipped).
   if remainder(length lpow car pivots,4) member {2,3} then s := not s;
   rows := reverse rows;       % calculate determinants along pivot rows
   pivots := reverse pivots;
   p := car rows;
   foreach r in cdr rows do
      p := {car(pivots := cdr pivots),
            quotexf!*(extadd(extmult(cadr r,car p),
                             extmult(car r,cadr p)),d)};
   return w . v . if s then {negex car p,negex cadr p} else p;
   end;

symbolic procedure sparse_backsub(exlis,varlis);
   % exlis: list of sf, varlis: list of kernel
   % -> sparse_backsub: list of kernel.sq
   % Fraction-free back-substitution for exlis, where reverse exlis is
   % a list of rows of an upper-triangular matrix wrt varlis.  Since
   % exlis has been produced in a fraction-free way, the leading
   % coefficient of the first row is the determinant of the system.

   begin scalar d,z,c;
   if null exlis then return nil;       % trivial case
   d := lc car exlis;                   % determinant
   foreach x in exlis do % almost redundant for first x
      begin scalar s,p,v,r;
      p := lc x;   % pivot
      v := mvar x;
      x := red x;
      while not domainp x and mvar x member varlis do
          <<if (c := atsoc(mvar x,z)) then % back-substitute
               s := addf(multf(lc x,cdr c),s)
            else % move free variable terms to rhs
               r := addf(!*t2f lt x,r);
            x := red x>>;
      x := addf(r,x);
      s := negf quotf!*(addf(multf(x,d),s),p);
      z := (v . s) . z;
      end;
   foreach p in z do
      cdr p := cancel(cdr p ./ d);
   return z;
   end;

endmodule;


module det;   % Determinant and trace routines.

% Author: Anthony C. Hearn.

fluid '(!*cramer !*rounded asymplis!* bareiss!-step!-size!* wtl!*);

bareiss!-step!-size!* := 2;     % seems fastest on average.

symbolic procedure simpdet u;
   % We can't use the Bareiss code when rounded is on, since exact
   % division is required.
   if !*cramer or !*rounded then detq matsm carx(u,'det)
    else bareiss!-det u;

% The hashing and determinant routines below are due to M. L. Griss.

Comment Some general purpose hashing functions;

flag('(array),'eval);      % Declared again for bootstrapping purposes.

array !$hash 64;  % General array for hashing.

symbolic procedure gethash key;
   % Access previously saved element.
   assoc(key,!$hash(remainder(key,64)));

symbolic procedure puthash(key,valu);
   begin integer k; scalar buk;
      k := remainder(key,64);
      buk := (key . valu) . !$hash k;
      !$hash k := buk;
      return car buk
   end;

symbolic procedure clrhash;
   for i := 0:64 do !$hash i := nil;

Comment Determinant Routines;

symbolic procedure detq u;
   % Top level determinant function.
   begin integer len;
      len := length u;   % Number of rows.
      for each x in u do
        if length x neq len then rederr "Non square matrix";
      if len=1 then return caar u;
      clrhash();
      u := detq1(u,len,0);
      clrhash();
      return u
   end;

symbolic procedure detq1(u,len,ignnum);
   % U is a square matrix of order LEN. Value is the determinant of U.
   % Algorithm is expansion by minors of first row.
   % IGNNUM is packed set of column indices to avoid.
   begin integer n2; scalar row,sign,z;
      row := car u;   % Current row.
      n2 := 1;
      if len=1
        then return <<while twomem(n2,ignnum)
                         do <<n2 := 2*n2; row := cdr row>>;
                      car row>>;   % Last row, single element.
      if z := gethash ignnum then return cdr z;
      len := len-1;
      u := cdr u;
      z := nil ./ 1;
      for each x in row do
        <<if not twomem(n2,ignnum)
            then <<if numr x
                     then <<if sign then x := negsq x;
                            z:= addsq(multsq(x,detq1(u,len,n2+ignnum)),
                                        z)>>;
                   sign := not sign>>;
          n2 := 2*n2>>;
      puthash(ignnum,z);
      return z
   end;

symbolic procedure twomem(n1,n2);
   % For efficiency reasons, this procedure should be coded in assembly
   % language.
   not evenp(n2/n1);

put('det,'simpfn,'simpdet);

flag('(det),'immediate);

% A version of det using the Bareiss code.

symbolic procedure bareiss!-det u;
   % Compute a determinant using the Bareiss code.
   begin scalar nu,bu,n,temp,v,!*exp;
   !*exp := t;
   nu := matsm car u;
   n := length nu;
   for each x in nu do
      if length x neq n then rederr "Non square matrix";
   if n=1 then return caar nu;
   if asymplis!* or wtl!* then
    <<temp := asymplis!* . wtl!*;
      asymplis!* := wtl!* := nil>>;
   nu := normmat nu;
   v := for i:=1:n collect intern gensym();
   car nu := foreach r in car nu collect prsum(v,r);
   bu := cdr sparse_bareiss(car nu,v,bareiss!-step!-size!*);
   bu := if length bu = n then (lc car bu ./ cdr nu) else (nil ./ 1);
   if temp then <<asymplis!* := car temp; wtl!* := cdr temp>>;
   return if temp then resimp bu else cancel bu;
   end;

symbolic procedure simptrace u;
   begin integer n; scalar z;
        u := matsm carx(u,'trace);
        if length u neq length car u then rederr "Non square matrix";
        n := 1;
        z := nil ./ 1;
        for each x in u do <<z := addsq(nth(x,n),z); n := n+1>>;
        return z
   end;

put('trace,'simpfn,'simptrace);

endmodule;


module glmat; % Routines for inverting matrices and finding eigen-values
              % and vectors. Methods are the same as in glsolve module.

% Author: Eberhard Schruefer.
% Modification: James Davenport and Fran Burstall.

fluid '(!*cramer !*factor !*gcd !*sqfree !*sub2 kord!*);

global '(!!arbint);

if null !!arbint then !!arbint := 0;

switch cramer;

put('cramer,'simpfg,
    '((t (put 'mat 'lnrsolvefn 'clnrsolve)
     (put 'mat 'inversefn 'matinv))
      (nil (put 'mat 'lnrsolvefn 'lnrsolve)
       (put 'mat 'inversefn 'matinverse))));

% algebraic operator arbcomplex;

% Done this way since it's also defined in the solve1 module.

deflist('((arbcomplex simpiden)),'simpfn);

symbolic procedure clnrsolve(u,v);
   % Interface to matrix package.
   multm(matinv u,v);

symbolic procedure minv u;
   matinv matsm u;

put('minv,'rtypefn,'quotematrix);

%put('mateigen,'rtypefn,'quotematrix);

remprop('mateigen,'rtypefn);

symbolic procedure matinv u;
   % U is a matrix form. Result is the inverse of matrix u.
   begin scalar sgn,x,y,z,!*exp;
     integer l,m,lm;
     !*exp := t;
     z := 1;
     lm := length car u;
     for each v in u do
       <<y := 1;
     for each w in v do y := lcm(y,denr w);
     m := lm;
     x := list(nil . (l := l + 1)) .* negf y .+ nil;
     for each j in reverse v do
       <<if numr j then
        x := list m .* multf(numr j,quotf(y,denr j)) .+ x;
         m := m - 1>>;
     z := c!:extmult(x,z)>>;
      if singularchk lpow z then rerror(matrix,13,"Singular matrix");
     sgn := evenp length lpow z;
      return for each k in lpow z collect
          <<sgn := not sgn;
            for each j in lpow z collect mkglimat(k,z,sgn,j)>>
   end;

symbolic procedure singularchk u; pairp car lastpair u;

flag('(mateigen),'opfn);

flag('(mateigen),'noval);

symbolic procedure mateigen(u,eival);
   % U is a matrix form, eival an indeterminate naming the eigenvalues.
   % Result is a list of lists:
   %   {{eival-eq1,multiplicity1,eigenvector1},....},
   % where eival-eq is a polynomial and eigenvector is a matrix.
%    How much should we attempt to solve the eigenvalue eq.? sqfr?
%    Sqfr is necessary if we want to have the full eigenspace. If there
%    are multiple roots another pass through eigenvector calculation
%    is needed(done).
%    We should actually perform the calculations in the extension
%    field generated by the eigenvalue equation(done inside).
  begin scalar arbvars,exu,sgn,q,r,s,x,y,z,eivec,!*factor,!*sqfree,
               !*exp;
        integer l;
     !*exp := t;
     if not(getrtype u eq 'matrix) then typerr(u,"matrix");
     eival := !*a2k eival;
     kord!* := eival . kord!*;
     exu := mateigen1(matsm u,eival);
     q := car exu;
     y := cadr exu;
     z := caddr exu;
     exu := cdddr exu;
     !*sqfree := t;
     for each j in cdr fctrf numr subs2(lc z ./ 1)
          do if null domainp car j and mvar car j eq eival
                then s := (if null red car j
                              then !*k2f mvar car j . (ldeg car j*cdr j)
                            else j) . s;
     for each j in q
       do (if x then rplacd(x,cdr x + cdr j)
             else s := (y . cdr j) . s)
            where x := assoc(y,s) where y := absf reorder car j;
     l := length s;
     r := 'list .
       for each j in s collect
     <<if null((cdr j = 1) and (l = 1)) then
         <<y := 1;
           for each k in exu do
         if x := reduce!-mod!-eig(car j,c!:extmult(k,y))
           then y := x>>;
       arbvars := nil;
       for each k in lpow z do
          if (y=1) or null(k member lpow y) then
         arbvars := (k . makearbcomplex()) . arbvars;
       sgn := (y=1) or evenp length lpow y;
       eivec := 'mat . for each k in lpow z collect list
                           if x := assoc(k,arbvars)
                              then mvar cdr x
                            else prepsq!* mkgleig(k,y,
                                    sgn := not sgn,arbvars);
       list('list,prepsq!*(car j ./ 1),cdr j,eivec)>>;
     kord!* := cdr kord!*;
     return r
   end;

symbolic procedure mateigen1(u,eival);
   begin scalar q,x,y,z; integer l,lm,m;
      lm := length car u;
      z := 1;
      u := for each v in u collect
          <<y := 1;
        for each w in v do y := lcm(y,denr w);
        m := lm;
        l := l + 1;
        x := nil;
        for each j in reverse v do
          <<if numr j or l = m then
            x := list m .* multf(if l = m then
                                  addf(numr j,
                                       negf multf(!*k2f eival,
                                                  denr j)) else numr j,
                                 quotf(y,denr j)) .+ x;
            m := m - 1>>;
        y := z;
        z := c!:extmult(if null red x then <<
               q := (if p then (car p  . (cdr p + 1)) . delete(p,q)
                      else (lc x  . 1) . q) where p = assoc(lc x,q);
                        !*p2f lpow x>> else x,z);
        x>>;
      return q . y . z . u
   end;

symbolic procedure reduce!-mod!-eig(u,v);
   % Reduces exterior product v wrt eigenvalue equation u.
   begin scalar x,y;
     for each j on v do
       if numr(y := reduce!-mod!-eigf(u,lc j)) then
      x := lpow j .* y .+ x;
     y := 1;
     for each j on x do y := lcm(y,denr lc j);
     return for each j on reverse x collect
          lpow j .* multf(numr lc j,quotf(y,denr lc j))
   end;

symbolic procedure reduce!-mod!-eigf(u,v);
   (subs2 reduce!-eival!-powers(lpow u . negsq cancel(red u ./ lc u),v))
    where !*sub2 = !*sub2;

symbolic procedure reduce!-eival!-powers(v,u);
   if domainp u or null(mvar u eq caar v) then u ./ 1
    else reduce!-eival!-powers1(v,u ./ 1);

symbolic procedure reduce!-eival!-powers1(v,u);
   % Reduces powers with the help of the eigenvalue polynomial.
   if domainp numr u or (ldeg numr u<pdeg car v) then u
    else if ldeg numr u=pdeg car v then
        addsq(multsq(cdr v,lc numr u ./ denr u),
          red numr u ./ denr u)
   else reduce!-eival!-powers1(v,
    addsq(multsq(multpf(mvar numr u .** (ldeg numr u-pdeg car v),
                lc numr u) ./ denr u,
         cdr v),red numr u ./ denr u));

% Determinant calculation using exterior multiplication.

symbolic procedure detex u;
   % U is a matrix form, result is determinant of u.
   begin scalar f,x,y,z;
     integer m,lm;
     z := 1;
     u := matsm car u;
     lm := length car u;
     f := 1;
     for each v in u do
       <<y := 1;
     for each w in v do y := lcm(y,denr w);
     f := multf(y,f);
     m := lm;
     x := nil;
     for each j in v do
       <<if numr j then
        x := list m .* multf(numr j,quotf(y,denr j)) .+ x;
         m := m - 1>>;
     z := c!:extmult(x,z)>>;
      return cancel(lc z ./ f)
   end;

% Not supported at algebraic user level since it is in general slower
% than other methods.

% put('detex,'simpfn,'detex);

symbolic procedure mkglimat(u,v,sgn,k);
   begin scalar s,x,y;
     x := nil ./ 1;
     y := lpow v;
     for each j on red v do
       if s := glmatterm(u,y,j,k)
      then x := addsq(cancel(s ./ lc v),x);
     return if sgn then negsq x else x
   end;

symbolic procedure glmatterm(u,v,w,k);
   begin scalar x,y,sgn;
     x := lpow w;
     a: if null x then return
       if pairp car y and (cdar y = k) then lc w else nil;
    if car x = u then return nil
         else if car x member v then <<x := cdr x;
                     if y then sgn := not sgn>>
         else if y then return nil
               else <<y := list car x; x := cdr x>>;
        go to a
   end;

symbolic procedure mkgleig(u,v,sgn,arbvars);
   begin scalar s,x,y,!*gcd;
     x := nil ./ 1;
     y := lpow v;
     !*gcd := t;
     for each j on red v do
       if s := glsoleig(u,y,j,arbvars)
      then x := addsq(cancel(s ./ lc v),x);
     return if sgn then negsq x else x
   end;

symbolic procedure glsoleig(u,v,w,arbvars);
   begin scalar x,y,sgn;
     x := lpow w;
     a: if null x then return
           if null car y then lc w
        else multf(cdr assoc(car y,arbvars),
               if sgn then negf lc w else lc w);
        if car x = u then return nil
         else if car x member v then <<x := cdr x;
                     if y then sgn := not sgn>>
         else if y then return nil
               else <<y := list car x; x := cdr x>>;
        go to a
   end;


%**** Support for exterior multiplication ****
% Data structure is lpow ::= list of col.-ind. in exterior product
%                            | nil . number of eq. for inhomog. terms.
%                   lc   ::= standard form


% Exterior multiplication and p-forms:
% Let V be a vector space of dimension n.
% We call the elements of V 1-forms and build new objects called
% p-forms as follows: define a multiplication on 1-forms ^ such that
%                v^w=-w^v
% then the linear span of such objects is the space of 2-forms and has
% dimension n(n-1)/2.  Indeed, if v_1,...,v_n is a basis of V then
% v_i^v_j for i<j is a basis for the 2-forms.
% We extend this multiplication (called exterior multiplication)
% to get products of p vectors, linear combinations of which are
% called p-forms: this extension is defined by the rule that v_1^...^v_p
% vanishes whenever some v_i=v_j (for i not j).  Thus the effect of
% permuting the order of the vectors in such a product is to multiply
% the product by the sign of the permutation.
% Using this it is not difficult to show:
% Theorem: Vectors v_1,...,v_p are linear independent iff their exterior
% product v_1^...^v_p is non-zero.
%
% For more information see F. Warner "Foundations of Differentiable
% Manifolds and Lie Groups" (Springer) Chapter 2. (Or any other book
% on differential geometry or multilinear algebra

symbolic procedure c!:extmult(u,v);
   % Special exterior multiplication routine. Degree of form v is
   % arbitrary, u is a one-form.
   if null u or null v then  nil
    else if v = 1 then u                   %unity
%   else (if x then cdr x .* (if car x
%                        then negf c!:subs2multf(lc u,lc v)
%          else c!:subs2multf(lc u,lc v))
%             .+ c!:extadd(c!:extmult(!*t2f lt u,red v),
%             ^^ this is bogus: .+ may not be valid in this circumstance
%                      c!:extmult(red u,v))
    else (if x then
             c!:extadd(cdr x .* (if car x
                                 then negf c!:subs2multf(lc u,lc v)
                   else c!:subs2multf(lc u,lc v)) .+ nil,
                       c!:extadd(c!:extmult(!*t2f lt u,red v),
                                 c!:extmult(red u,v)))
       else c!:extadd(c!:extmult(!*t2f lt u,red v),
              c!:extmult(red u,v)))
      where x = c!:ordexn(car lpow u,lpow v);

symbolic procedure c!:subs2multf(u,v);
   (if denr x neq 1 then rerror(matrix,14,"Sub error in glnrsolve")
     else numr x)
   where x = subs2(multf(u,v) ./ 1) where !*sub2 = !*sub2;


symbolic procedure c!:extadd(u,v);
   if null u then v
    else if null v then u
    else if lpow u = lpow v then
            (lambda x,y; if null x then y else lpow u .* x .+ y)
        (addf(lc u,lc v),c!:extadd(red u,red v))
    else if c!:ordexp(lpow u,lpow v) then lt u .+ c!:extadd(red u,v)
    else lt v .+ c!:extadd(u,red v);

symbolic procedure c!:ordexp(u,v);
   if null u then t
    else if car u = car v then c!:ordexp(cdr u,cdr v)
    else c!:ordxp(car u,car v);

symbolic procedure c!:ordexn(u,v);
   % U is a single index, v a list. Returns nil if u is a member
   % of v or a dotted pair of a permutation indicator and the ordered
   % list of u merged into v.
   begin scalar s,x;
     a: if null v then return(s . reverse(u . x))
     else if (u = car v) or (pairp u and pairp car v)
         then return nil
     else if c!:ordxp(u,car v) then
         return(s . append(reverse(u . x),v))
         else  <<x := car v . x;
                 v := cdr v;
                 s := not s>>;
         go to a
   end;

symbolic procedure c!:ordxp(u,v);
   if pairp u then if pairp v then cdr u < cdr v
            else nil
    else if pairp v then t
    else u < v;

endmodule;


module nullsp;  % Compute the nullspace (basis vectors) of a matrix.

% Author: Herbert Melenk <melenk@sc.zib-berlin.de>.

% Algorithm:  Rational Gaussian elimination with standard qutotients.

put('nullspace,'psopfn,'nullspace!-eval);

symbolic procedure nullspace!-eval u;
   % interface for the nullspace calculation.
   begin scalar v,n,matinput;
     v := reval car u;
     if eqcar(v,'mat) then
        <<matinput:=t; v := cdr v>>
     else if eqcar(v,'list) then
      v := for each row in cdr v collect
        if not eqcar(row,'list) then typerr ("matrix",u) else
        <<row := cdr row;
          if null n then n := length row else
          if n neq length row
            then rerror(matrix,15,"lists not in matrix shape");
          row>> else rerror(matrix,16,"Not a matrix");
     v := nullspace!-alg v;
     return 'list . for each vect in v collect
         if matinput then 'mat . for each x in vect collect list x
           else 'list . vect;
   end;

symbolic procedure nullspace!-alg(m);
   % "M" is a Matrix, encoded as list of lists(=rows) of algebraic
   % expressions.
   % Result is the basis of the kernel of M in the same encoding.
   begin scalar mp,vars,rvars,r,res,oldorder; integer n;
     n := length car m;
     vars := for i:=1:n collect gensym();
     rvars := reverse vars;
     oldorder := setkorder rvars;
     mp := for each row in m collect
     <<r := nil ./ 1;
      for each col in pair(vars,row) do
         r := addsq(r,simp list('times,car col,cdr col));
      r>>;
     res := nullspace!-elim(mp,rvars);
     setkorder oldorder;
     return reverse for each q in res collect
       for each x in vars collect
        cdr atsoc(x,q);
   end;

symbolic procedure nullspace!-elim(m,vars);
   % "M" is a matrix encoded as list of linear polnomials (sq's) in
   % the variables "vars". The current korder cooresponds to vars.
   % Result is a basis for the null space of the matrix, encoded
   % as list of vectors, where each vector is an alist over vars.
   % A rational Gaussian elimination is performed and unit vectors
   % are substituted for the remaining unrestricted variables.
  begin scalar c,s,x,w,arbvars,depvars,row,res,break;
     while vars and not break do
     <<for each p in m do
        if domainp numr p then if numr p then break:=t %unsolvable
                                else m:=delete(p,m);
       if not break then
       <<x:=car vars; vars:=cdr vars; row:=nil;
            % select row with x as leading variable.
         for each p in m do
           if null row and mvar numr p = x then row:=p;
            % if none, then x is a free variable.
        if null row then arbvars:=x.arbvars else
        <<m:=delete(row,m);
          c:=multsq(negf denr row ./1, 1 ./ lc numr row);
          row := multsq(row,c);
            % collect formula for x,
          depvars := (x . (red numr row ./ denr row)) . depvars;
            % and perform elimination with this row.
          m:=for each p in m collect
          <<if mvar numr p=x then
              <<p:=addsq(p, multsq(row,lc numr p ./ denr p));
                 % Simplification for non-numeric coefficients.
                if not domainp (w:=numr p) and not domainp lc w then
                    p:=subs2!* p>>;
            p>>;
        >>;
      >>;
    >>;
    if break then return nil;
         % Constuct solutions by assigning unit vectors to the
         % free variables and perform backsubstitution.
    for each x in arbvars do
    << s := for each y in arbvars collect
          (y . if y=x then 1 else 0);
       c := 1;
       for each y in depvars do
       << s := (car y . prepsq (w:=subsq(cdr y,s))) . s;
          c := lcm!*(c,denr w)
       >>;
       if not(c=1) then <<c:=prepf c;
         s:=for each q in s collect car q.reval{'times,cdr q,c}>>;
       res := s . res;
    >>;
    return res;
  end;

endmodule;


module rank;

% Author: Eberhard Schruefer.

% Module for calculating the rank of a matrix or a system of linear
% equations.
% Format: rank <matrix> : rank <list of equations>.

symbolic procedure rank!-eval u;
   begin scalar n;
      if cdr u then rerror(matrix,17,"Wrong number of arguments")
       else if getrtype (u := car u) eq 'matrix
         then return rank!-matrix matsm u
       else if null eqcar(u := aeval u,'list) then typerr(u,"matrix")
       else return rank!-matrix
         for each row in cdr u collect
           if not eqcar(row,'list)
               then rerror(matrix,15,"list not in matrix shape")
            else <<row := cdr row;
                   if null n then n := length row
                    else if n neq length row
                     then rerror(matrix,151,"list not in matrix shape");
                   for each j in row collect simp j>>
   end;


put('rank,'psopfn,'rank!-eval);

symbolic procedure rank!-matrix u;
   begin scalar x,y,z; integer m,n;
     z := 1;
     for each v in u do
          <<y := 1;
            for each w in v do y := lcm(y,denr w);
            m := 1;
            x := nil;
            for each j in v do
              <<if numr j then
                x := list m .* multf(numr j,quotf(y,denr j)) .+ x;
                m := m + 1>>;
            if y := c!:extmult(x,z)
               then <<z := y; n := n + 1>>>>;
     return n
    end;

endmodule;


module resultant;

% Author: Eberhard Schruefer.

%**********************************************************************
%                                                                     *
% The resultant function defined here has the following properties:   *
%                                                                     *
%                           degr(p1,x)*degr(p2,x)                     *
%  resultant(p1,p2,x) = (-1)                     *resultant(p2,p1,x)  *
%                                                                     *
%                         degr(p2,x)                                  *
%  resultant(p1,p2,x) = p1             if p1 free of x                *
%                                                                     *
%  resultant(p1,p2,x) = 1  if p1 free of x and p2 free of x           *
%                                                                     *
%**********************************************************************

%exports resultant;

%imports reorder,setkorder,degr,addf,negf,multf,multpf;

fluid '(!*exp kord!*);

symbolic procedure resultant(u,v,w);
   %u and v are standard forms. Result is resultant of u and v
   %w.r.t. kernel w. Method is Bezout's determinant using exterior
   %multiplication for its calculation.
   begin scalar ap,ep,uh,ut,vh,vt;
         integer n,nm;
     if domainp u and domainp v then return 1;
     kord!* := w . kord!*;
     if null domainp u and null(mvar u eq w) then u := reorder u;
     if null domainp v and null(mvar v eq w) then v := reorder v;
     if domainp u or null(mvar u eq w)
        then <<setkorder cdr kord!*;
               return if not domainp v and mvar v eq w
                        then exptf(u,ldeg v)
                       else 1>>
      else if domainp v or null(mvar v eq w)
        then <<setkorder cdr kord!*;
               return if mvar u eq w then exptf(v,ldeg u)
                       else 1>>;
      n := ldeg u - ldeg v;
      ep := 1;
      if n<0 then
          <<for j := (-n-1) step -1 until 1 do
              ep := b!:extmult(!*sf2exb(multpf(w to j,u),w),ep);
              ep := b!:extmult(!*sf2exb(multd((-1)**(-n*ldeg u),u),
                                        w),
                               ep)>>
       else if n>0 then
            <<for j := (n-1) step -1 until 1 do
                ep := b!:extmult(!*sf2exb(multpf(w to j,v),w),ep);
              ep := b!:extmult(!*sf2exb(v,w),ep)>>;
     nm := max(ldeg u,ldeg v);
     uh := lc u;
     vh := lc v;
     ut := if n<0 then multpf(w to -n,red u)
           else red u;
     vt := if n>0 then multpf(w to n,red v)
            else red v;
     ap := addf(multf(uh,vt),negf multf(vh,ut));
     ep := if null ep then !*sf2exb(ap,w)
        else b!:extmult(!*sf2exb(ap,w),ep);
     for j := (nm - 1) step -1 until (abs n + 1) do
        <<if degr(ut,w) = j then
         <<uh := addf(lc ut,multf(!*k2f w,uh));
                   ut := red ut>>
       else    uh := multf(!*k2f w,uh);
          if degr(vt,w) = j then
         <<vh := addf(lc vt,multf(!*k2f w,vh));
                   vt := red vt>>
       else    vh := multf(!*k2f w,vh);
      ep := b!:extmult(!*sf2exb(addf(multf(uh,vt),
                    negf multf(vh,ut)),w),ep)>>;
     setkorder cdr kord!*;
     return if null ep then nil else lc ep
   end;

put('resultant,'simpfn,'simpresultant);

symbolic procedure simpresultant u;
   begin scalar !*exp;
     if length u neq 3
       then rerror(matrix,19,
                   "RESULTANT called with wrong number of arguments");
     !*exp := t;
     return resultant(!*q2f simp!* car u,
                      !*q2f simp!* cadr u,
                      !*a2k caddr u) ./ 1
   end;

symbolic procedure !*sf2exb(u,v);
   %distributes s.f. u with respect to powers in v.
   if degr(u,v)=0 then if null u then nil
                        else list 0 .* u .+ nil
    else list ldeg u .* lc u .+ !*sf2exb(red u,v);

%**** Support for exterior multiplication ****
% Data structure is lpow ::= list of degrees in exterior product
%                   lc   ::= standard form

symbolic procedure b!:extmult(u,v);
   %Special exterior multiplication routine. Degree of form v is
   %arbitrary, u is a one-form.
   if null u or null v then  nil
    else if v = 1 then u
    else (if x then cdr x .* (if car x then negf multf(lc u,lc v)
                   else multf(lc u,lc v))
              .+ b!:extadd(b!:extmult(!*t2f lt u,red v),
                    b!:extmult(red u,v))
       else b!:extadd(b!:extmult(red u,v),
              b!:extmult(!*t2f lt u,red v)))
      where x = b!:ordexn(car lpow u,lpow v);

symbolic procedure b!:extadd(u,v);
   if null u then v
    else if null v then u
    else if lpow u = lpow v then
            (lambda x,y; if null x then y else lpow u .* x .+ y)
        (addf(lc u,lc v),b!:extadd(red u,red v))
    else if b!:ordexp(lpow u,lpow v) then lt u .+ b!:extadd(red u,v)
    else lt v .+ b!:extadd(u,red v);

symbolic procedure b!:ordexp(u,v);
   if null u then t
    else if car u > car v then t
    else if car u = car v then b!:ordexp(cdr u,cdr v)
    else nil;

symbolic procedure b!:ordexn(u,v);
   %u is a single integer, v a list. Returns nil if u is a member
   %of v or a dotted pair of a permutation indicator and the ordered
   %list of u merged into v.
   begin scalar s,x;
     a: if null v then return(s . reverse(u . x))
     else if u = car v then return nil
     else if u and u > car v then
                 return(s . append(reverse(u . x),v))
         else  <<x := car v . x;
                 v := cdr v;
                 s := not s>>;
         go to a
   end;

endmodule;


module cofactor;   % Cofactor operator.

% Author: Alan Barnes <barnesa@kirk.aston.ac.uk>.

Comment

Syntax:  COFACTOR(MATRIX:matrix,ROW:integer,COLUMN:integer):algebraic

The cofactor of the element in row ROW and column COLUMN of matrix
MATRIX is returned.  Errors occur if ROW or COLUMN do not simplify to
integer expressions or if MATRIX is not square;

symbolic procedure cofactorq (u,i,j);
   begin integer len;
      len:= length u;
      if not(i>0 and i<len+1)
        then rerror(matrix,20,"Row number out of range");
      if not(j>0 and j<len+1)
       then rerror(matrix,21,"Column number out of range");
       foreach x in u do
         if length x neq len then rerror(matrix,22,"non-square matrix");
      u := remove(u,i);
      clrhash();
      u := detq1(u,len-1,2**(j-1));
      clrhash();
      if remainder(i+j,2)=1 then u := negsq u;
      return u;
  end;

put ('cofactor,'simpfn,'simpcofactor);

symbolic procedure simpcofactor u;
   cofactorq(matsm car u,ieval cadr u,ieval carx(cddr u,'cofactor));

endmodule;


end;
