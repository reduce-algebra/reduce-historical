module numfit; % approximation of functions with least spares.

% Author: H. Melenk, ZIB, Berlin

% Copyright (c): ZIB Berlin 1991, all rights resrved

fluid '(!*noequiv accuracy!* singularities!*);
global '(iterations !*trnumeric);

symbolic procedure fiteval u;
     % interface function;
  begin scalar a,b,e,r,l,q,x,v,var,pars,fcn,fl,basis,pts,
        grad,oldmode,!*noequiv;
        integer n,i;
    if not(length u=3) then goto synerr;
    u := for each x in u collect reval x;
    u := accuracycontrol(u,6,200);
    fcn :=car u; u :=cdr u;
    if eqcar(fcn,'list) then fl:=cdr fcn;
    basis:=car u; u :=cdr u;
    if not eqcar(basis,'list) then goto synerr;
    basis := for each x in cdr basis collect simp reval x;
    var:= car u; u := cdr u;
    if not eqcar(var,'equal) then goto synerr;
    if not eqcar(pts:=caddr var,'list) then goto synerr;
    var := cadr var; pts:=cdr pts; n:=length pts;
    if !*trnumeric then prin2t "build generic approximation function";
    a:=nil./1;
    for each b in basis do
    <<v:=gensym(); pars:=v.pars; a:=addsq(multsq(simp v,b),a)>>;
      % generate the error expression
    oldmode:=switch!-mode!-rd nil;!*noequiv:=nil;
    b:=a:=simp prepsq a;
    fcn:=simp if null fl then fcn else 'dummy; e:=nil./1;
    for each p in pts do
    <<i:=i+1;l:=list(var.p); % value to be substituted.
      if fl then l:=('dummy . reval nth(fl,i)).l;
      q:=subtrsq(subsq(fcn,l),subsq(b,l));
      e:=addsq(e,multsq(q,q))>>;
    e:=prepsq e;
    if !*trnumeric then
      <<lprim "error function is:";writepri(mkquote e,'only)>>;
      % find minimum.
      % build gradient.
    grad:='list . for each v in pars collect reval {'df,e,v};
    r:=rdsolveeval
       list (grad,'list . for each p in pars collect list('equal,p,0),
                  {'equal,'accuracy,accuracy!*},
                  {'equal,'iterations,iterations!*});
      % resubstitute into target function.
    if !*trnumeric then lprim "resubstituting in approximating form";
    l:=nil; pars:= nil;
    for each p in cdr r collect
       <<x:=caddr p; pars:=x.pars; l:=(cadr p.x).l>>;
    a:=prepsq subsq(a,l);
    switch!-mode!-rd oldmode;
    return list('list, a ,'list . pars);
  synerr:
    rederr "illegal parameters in fit";
  end;

put('num_fit,'psopfn,'fiteval);

endmodule;

end;
