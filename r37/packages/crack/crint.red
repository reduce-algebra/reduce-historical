%*********************************************************************
module integration$
%*********************************************************************
%  Routines for integration of pde's
%  Author: Thomas Wolf, Andreas Brand
%  1993 1995
%
% $Id: crint.red,v 1.13 1998/06/25 18:21:25 tw Exp tw $
%

symbolic procedure ldlist(p,f,vl)$
% provides a reverse list of leading derivatives of f in p, vl is list
% of variables
begin scalar a$
  if not atom p then
  if member(car p,list('EXPT,'PLUS,'MINUS,'TIMES,'QUOTIENT,'DF,'EQUAL))
  then <<
    if (car p='PLUS) or (car p='TIMES) or
       (car p='QUOTIENT) or (car p='EQUAL) then
    <<p:=cdr p$
      while p do
      <<a:=diffincl(a,ldlist(car p,f,vl))$
	p:=cdr p>>
    >>                                     else
    if car p='MINUS then a:=ldlist(cadr p,f,vl) else
    if car p='EXPT         then   % if numberp caddr p then
    a:=ldlist(cadr p,f,vl) else   % fuehrende Abl. aus der Basis
				  % else a:=nil
    if car p='DF then if cadr p=f then
    <<p:=cddr p;
      while vl do
      <<a:=cons(dfdeg(p,car vl),a);
	vl:=cdr vl>>;
      a:=list a
    >>
  >>$
  return a
end$

symbolic procedure diffincl(a,b)$
% a,b are lists of leading derivatives which are to be united
begin
  scalar p;
  while a and b do
  <<a:=ddroplow(a,car b);
    if car a then p:=cons(car a,p);
    a:=cdr a;
    b:=cdr b>>;
  return
  if null a then if p then nconc(p,b)
		      else b
	    else if p then a:=nconc(p,a)
		      else a
end$

symbolic procedure ddroplow(a,cb)$
% loescht Elemente von a, von denen cb eine Ableitung ist, loescht cb,
% wenn ein Element von a eine Ableitung von cb ist
begin
  scalar h;
  return
  if null a then list(cb)
	    else
  <<h:=compdiffer(car a,cb);
    if numberp(h) then if h>0 then cons(nil,a)        % car a=df(cb,..
			      else ddroplow(cdr a,cb) % cb=df(car a,..
		  else <<h:=ddroplow(cdr a,cb);       % neither
			 cons(car h,cons(car a,cdr h))>>
  >>
end$

symbolic procedure compdiffer(p,q)$
% finds whether p is a derivative of q or q of p or neither
begin
  scalar p!>q,q!>p;
  while p and ((null p!>q) or (null q!>p)) do
  <<if car p>car q then p!>q:=t else  % compare orders of derivatives
    if car p<car q then q!>p:=t;
    p:=cdr p;q:=cdr q
  >>;
  return
  if q!>p then if p!>q then nil     %  neither
		       else 0       %  q is derivative of p
	  else if p!>q then 2       %  p is derivative of q
		       else 1       %  p equal q
end$


symbolic procedure ldintersec(a)$
% determines the intersection of derivatives in the list a
begin
  scalar b;
  return
  if null a then nil else
  <<b:=car a;a:=cdr a;
    while a do
    <<b:=isec(b,car a)$
      a:=cdr a
    >>;
    b
  >>
end$


symbolic procedure isec(ca,b)$
% determines the minimum derivatives between ca and b
begin
  scalar newb;
  while ca do
  <<newb:=cons(if car b<car ca then car b else car ca, newb);
    ca:=cdr ca;b:=cdr b
  >>;
  return reverse newb
end$


symbolic procedure disjun(a,b)$
<<while a do
  if (car a neq 0) and (car b neq 0) then a:=nil
				     else <<a:=cdr a;b:=cdr b>>;
  if b then nil else t
>>$


symbolic procedure ddrophigh(a,cb)$
% loescht Elemente von a, die Ableitung von cb sind,
% loescht cb, wenn cb Ableitung eines Elements von a ist oder =a ist,
% haengt cb ansonsten an
begin
  scalar h;
  return
  if null a then list(cb)
	    else
  <<h:=compdiffer(car a,cb);
    if numberp(h) then if h<2 then a         % cb is deriv or = car a
			      else ddrophigh(cdr a,cb) % car a=df(cb,..
		  else cons(car a,ddrophigh(cdr a,cb)) % neither
  >>
end$


symbolic procedure elibar(l1,l2,lds)$
begin
  scalar found1,found2,h;
  % found1=t if an LD=l1 is found, found2=t if contradiction found
  while lds and (not found2) do
  <<if car lds=l1 then found1:=t else
    <<h:=compdiffer(l2,car lds);
      if (null h) or (h eq 2) then found2:=t
    >>;
    lds:=cdr lds
  >>;
  return found1 and (not found2)
end$

symbolic procedure intminderiv(p,ftem,vlrev,maxvanz,nfsub)$
% yields a list {nv1,nv2, ... } such that nvi is the minimum
% of the highest derivatives w.r.t. vi of all the leading derivatives
% of all functions of ftem which are functions of all maxvanz variables.
% Only those are kept for which nvi>0.
% further a list ((f1 ld's of f1) (f2 ld's of f2)...),
begin scalar l,a,listoflds$
  while ftem do
  <<if (maxvanz=(fctlength car ftem)) or (nfsub=0) then
    <<l:=ldlist(p,car ftem,vlrev);
      listoflds:=cons(cons(car ftem,l),listoflds)$
      a:=if a then ldintersec(cons(a,l))
	      else ldintersec(l)
    >>$
    ftem:=cdr ftem
  >>$
  return list(a,listoflds)
end$


symbolic procedure potintegrable(listoflds)$
begin
  scalar h,l1,l2,f,n,lds,f1,f2$
  if tr_genint then write "Does a potential exist?"$
  %------- Determining 2 minimal sets of integration variables
  % There must be two disjunct LDs such that all others are in their
  % ideal. This is necessary if we want to eliminate 2 functions.
  h:=listoflds;
  l1:=nil;
  while h do
  <<l2:=cdar h; % the list of LDs for the function caar h
    while l2 do
    <<l1:=ddrophigh(l1,car l2)$
      l2:=cdr l2>>;
    h:=cdr h
  >>;
  return
  if length l1 neq 2 then nil else
  if not disjun(car l1,cadr l1) then nil else
  % if there would be an overlap between l1 and l2 then it would have
  % to be integrated for elimination but it cannot --> no overlap
  % possible
  <<% selecting interesting functions for which one LD is = l1 and all
    % others are derivatives of l2 or equal l2 and vice versa. Two
    % necessary (one with an LD=l1 and one with an LD=l2) from which at
    % least one function (f) has no further LD.
    % Exception: 2 functions with each 2 LDs equal to (l1,l2) (these
    % functions are counted by n)
    l2:=cadr l1;l1:=car l1;
    f:=nil;f1:=nil;f2:=nil;n:=0;
    h:=listoflds;
    while h and ((not f1) or (not f2) or ((not f) and (n neq 2))) do
    <<lds:=cdar h;
      if (not f1) or (not f) then
      if elibar(l1,l2,lds) then
      <<f1:=cons(caar h,f1);
	if length lds eq 1 then f:=caar h else
	if length lds eq 2 then
	if (car lds = l2) or (cadr lds = l2) then n:=n+1
      >>;
      if (not f2) or (not f) then
      if elibar(l2,l1,lds) then
      <<f2:=cons(caar h,f2);
	if length lds eq 1 then f:=caar h
      >>;
      h:=cdr h
    >>;
    if f1 and ((n>1) or (f2 and f)) then list(l1,l2)
				    else nil
  >>
end$ % of potintegrable

symbolic procedure vlofintlist(vl,intlist)$
% provides a list of elements of vl for which the corresponding
% elements of intlist are non-zero
begin scalar a;
  while intlist do
  <<if (car intlist) and (not zerop car intlist) then a:=cons(car vl,a);
    vl:=cdr vl;
    intlist:=cdr intlist
  >>;
  return a
end$

symbolic procedure vlofintfaclist(vl,intfaclist)$
% determining the list of all variables of vl in intfaclist
begin scalar e1,a;
  for each e1 in vl do
  if not my_freeof(intfaclist,e1) then a:=cons(e1,a);
  return a
end$

symbolic procedure multipleint(intvar,ftem,q,vari,vl,genflag,
			       potflag,partial,doneintvar)$
% multiple integration of q wrt. variables in vl, max. number of
% integrations specified by intvar
% integrating factors must not depend on doneintvar, doneintvar is
% a list of integration variables so far
% partial=t then as much as possible of an expression is integrated
% even if there is a remainder
begin
  scalar pri,vlcop,v,nmax,geni,intlist,iflag,n,nges,newcond,
	 intfaclist,ph,pih,qh,qih,intfacdepnew,intfacdep$
  % intfacdep is a list of variables on which factors of integration
  % depend so far, other than the integration variable in their
  % integration --> no integration wrt. these variables by potint
  % because there the diff. operators wrt. to different variables
  % need not commute because the integrations are not done

  % pri:=t$
  if (not vari) and (zerop q) then return nil;
  nges:=0;
  vlcop:=vl;
  pih:=t;

  % Splitting of the equation into the homogeneous and inhomogeneous
  % part which is of advantage for finding integrating factors
  q:=splitinhom(q,ftem,vl)$
  qh:=car q; qih:=cdr q; q:=nil;

  while (vari or vlcop) and (pih or (not potflag)) do
  %------- if for potflag=t one variable can not be integrated the
  %------- maximal number of times (nmax) then immediate stop because
  %------- then no elimination of two functions will be possible
  << %-- The next integration variable: x, no of integrations: nmax
    if vari then <<v:=vari;nmax:=1>>
	    else <<v:=car vlcop;     vlcop:=cdr vlcop;
		   nmax:=car intvar; intvar:=cdr intvar>>;


    if zerop nmax then intlist:=cons(nil,intlist)
		  else
    <<if pri then write"anf: intvar=",intvar," vari=",vari,"    q=",q$
      if vari and (not member(v,vl)) then
      <<qh :=reval list('INT,qh ,v)$
        if freeof(qh,'INT) then <<
          qih:=reval list('INT,qih,v)$
          iflag:=if freeint_ and
                    (null freeof(qih,'INT)) then nil
                                            else <<
	           intlist:=cons(list(1),intlist)$
                   'success>>$
          if pri then <<write"232323 qh=",qh;terpri();
                        write"qih=",qih;terpri()>>
        >>
      >>                  else
      <<n:=0$
	if pri then write"333"$
	intfaclist:=nil; %-- the list of integr. factors in v-integr.
	if potflag or my_freeof(intfacdep,v) then
	% otherwise v-integration not allowed because one integrating
	% factor already depends on v
	% for potflag=t this `commutativity'-demand plays no role
	repeat << %--- max nmax integrations of qh and qih wrt. v
	  if pri then <<write"444  vor intpde:"$eqprint q$terpri()$
			write"potflag=",potflag," v=",v,
			"  ftem=",ftem>>$
	  % At first trying a direct integration of the homog. part qh
	  ph:=intpde(qh,ftem,vl,v,partial)$  % faster if potflag=nil
	  if pri then <<write"nach intpde(qh):"$deprint ph>>$

	  %------ At first the integration of the homogeneous part
	  intfacdepnew:=intfacdep;
	  if ph and (partial or (zerop cadr ph)) then <<
	    %---- For the homogen. part cadr ph must be zero
	    intfaclist:=cons(1,intfaclist);
	    ph:=car ph;
            if pri then <<write"565656 ph=",ph;terpri()>>;
	  >>                                     else
          if vari then ph:=nil
		  else
	  if facint_ then <<
	    ph:=findintfac(list(qh),ftem,vl,v,doneintvar,intfacdep,
			   not zerop n,not potflag);
            % factorize before ivestig., no report of int. factors
	    if ph then << %--- Complete integr. of qh was possible
              if print_ then write"of the homogeneous part"$terpri()$
	      %--- update the list of variables on which all integr.
	      %--- factors depend apart from the integration variable
	      intfacdepnew:=caddr ph;
	      %--- extend the list of integrating factors, cadr ph
	      %--- is a list of integr. factors, here only one
	      intfaclist:=cons(caadr ph,intfaclist);
	      %--- multiply the inhomogeneous part with integ. factor
	      qih:=reval reval reval list('TIMES,car intfaclist,qih);
              if pri then <<write"454545 qih=",qih;terpri()>>;
	      ph:=car ph  % the integral of qh
	    >>
	  >>;

	  %------ Now the integration of the inhomogeneous part
	  if not ph then pih:=nil %--- no integration possible
		    else <<
	    if zerop qih then pih:=list(0,0) else
	    pih:=intpde(qih,ftem,vl,v,partial)$

	    if null pih then
	    write"error2: inhom. expr. can not be integrated"$
	    if pri then <<write"nach intpde(qih):",pih$terpri()$
                          write"genflag=",genflag$terpri()>>$

            if pih then
	    if zerop cadr pih then
	    <<qih:=car pih$n:=add1 n$iflag:='success$
              if pri then write" success "$
            >>
			      else if not genflag then pih:=nil
						  else
	    <<if pri then write"555"$
	      geni:=partint(cadr pih,smemberl(ftem,cadr pih),
	                    vl,v,genflag)$
              if geni then
	      <<qih:=reval list('PLUS,car pih,car geni)$
		n:=add1 n$
		ftem:=union(fnew_,ftem)$
		newcond:=append(cdr geni,newcond)$  % additional de's
		if pri then
		<<terpri()$write"nach gen newcond:",newcond>>$
		iflag:='genint
	      >>                       else
	      if partial then qih:=car pih
			 else pih:=nil
	    >>;
	    if pih then <<
              if pri then write"AAA"$
 	      qh:=ph;
	      if (not potflag) and (n neq 1) then
	      intfacdep:=intfacdepnew
	      %-The first integr. factor of all v-integrations does not
	      % depend on any earlier integration variables and can
	      % therefore be taken out of all integrals --> no incease
	      % of intfacdep for n=1.
	      %-For potential integration the integration variables and
	      % extra-dependencies-variables of integr. factors need not
	      % be disjunct therefore the intfacdep-update only for
	      % not-potflag
	    >>     else <<
              if pri then write"BBB"$
	      % inhomogeneous part can not be integrated therefore
	      % reversing the succesful integration of the hom. part
	      if car intfaclist neq 1 then
	      qih:=reval list('QUOTIENT,qih,car intfaclist);
	      intfaclist:=cdr intfaclist
	    >>;
	  >>; %-- end of the integration of the inhomog. part
          if pri then write"n=",n," nmax=",nmax," not pih=",not pih$
	>> until (n=nmax) or (not pih)$ %---- end of v-integration
	%------- variables of done integrations are collected for
	%------- determining integrating factors in later integr.
	if not zerop n then doneintvar:=cons(v,doneintvar)$
	nges:=nges+n;
	intlist:=cons(intfaclist,intlist)
      >>$  % of    not ( vari and (not member(v,vl)))
      if vari then <<vari:=nil;vlcop:=nil>>;
      if pri then write"ende: intvar=",intvar," vari=",vari,
      "    qh=",qh,"   qih=",qih$
    >> % of (nmax neq zero)
  >>$  % of ( while (vari or vlcop) and (pih or (not potflag)) )

  % intlist and its elements intfaclist are in the inverse order
  % to vl and the sequence of integrations done
  q:=reval list('PLUS,qh,qih)$ %--- adding homog. and inhomog. part
  if pri then <<terpri()$write"\\\  newcond:"$deprint newcond;
    write"multiple result:",if null iflag then nil
    else list(q,intlist,newcond,nges)
  >>;
  return if null iflag then nil
		       else list(q,intlist,newcond,nges)
end$  % of multipleint

symbolic procedure uplistoflds(intlist,listoflds)$
begin
  scalar f,h1,h2,h3,h4,lds,itl;
  while listoflds do
  <<f:=caar listoflds;
    lds:=cdar listoflds;
    listoflds:=cdr listoflds;
    h2:=nil;            % h2 becomes the new list of lds of f
    while lds do
    <<h3:=car lds; lds:=cdr lds;
      itl:=intlist;
      h4:=nil;          % h4 becomes one new ld of f
      while h3 do
      <<h4:=cons(car h3 - if null car itl then 0
					  else length car itl, h4);
	h3:=cdr h3;itl:=cdr itl
      >>;
      h2:=cons(reverse h4,h2)
    >>;
    h1:=cons(cons(f,h2),h1)
  >>;
  return h1  % updated listoflds
end$ % of uplistoflds

symbolic procedure addintco(q, ftem, ifac, vl, vari)$
begin scalar v,f,l,vl1;
  % multi.ing factors to the constants/functions of integration
  if zerop q then l:=1
	     else
  <<ftem:=fctsort ftem$
    while ftem do
    if fctlength car ftem<length vl then ftem:=nil
				    else if fctlinear(q,f)          then
					 <<f:=car ftem$ ftem:=nil>> else
					 ftem:=cdr ftem$
    if f then
    <<l:=lderiv(q,f,fctargs f)$
      l:=reval coeffn(q,reval car l,cdr l)
    >>   else l:=1
  >>;
  % the constants and functions of integration
  if vari then q:=list('PLUS,q,intconst(l,vl,vari,list(1)))
	  else
  <<vl1:=vl;
    while vl1 do
    <<v:=car vl1; vl1:=cdr vl1;
      if car ifac then
      q:=list('PLUS,q,intconst(l,vl,v,car ifac))$
      % l..product of factors in the coefficient of the function to be
      % eliminated, car ifac .. list of integrating factors
      ifac:=cdr ifac;
    >>
  >>$
  return reval q
end$ % of addintco

symbolic procedure integratepde(p,ftem,vari,genflag,potflag)$
%  Generalized integration of the expression p
%     if not genflag then "normal integration"
%  Equation p must not be directly separable, otherwise the depen-
%  dencies of functions of integration on their variables is wrong,
%  i.e. no dependence on explicit variables
%  ftem are all functions from the `global' ftem which occur in p, i.e.
%  ftem:=smemberl(ftem,p)$
%  if vari=nil then integration w.r.t. all possible variables within
%                   the equation
%              else only w.r.t. vari one time

begin
  scalar vl,vlrev,v,intlist,
  ili1a,ili2a,maxvanz,fsub,h,hh,nfsub,iflag,newcond,
  n1,n2,pot1,pot2,p1,p2,listoflds,secnd,ifac0,
  ifac1a,ifac1b,ifac2a,ifac2b,cop,v1a,v2a,pri$

  % pri:=t;
  if pri then <<terpri()$write"Start Integratepde">>$
  vl:=argset ftem$
  vlrev:=reverse vl;
  if vari then <<potflag:=nil;
		 if zerop p then iflag:='success>>
	  else
  <<%------- determining fsub=list of functions of all variables
    maxvanz:=length vl$
    fsub:=nil;
    h:=ftem;
    while h do
    <<if fctlength car h=maxvanz then
      fsub:=cons(car h,fsub)$
      h:=cdr h
    >>$
    nfsub:=length fsub$  % must be >1 for potential-integration
    h:=intminderiv(p,ftem,vlrev,maxvanz,nfsub)$ % fsub is also for below
    intlist:=car h$
    %-- list of necessary integrations of the whole equation to solve
    %-- for a function of all variables
    listoflds:=cadr h$ %-- list of leading derivatives
  >>$
  if pri then <<terpri()$
		write"complete integrations:",intlist," for:",vl>>;
  %-- n1 is the number of integrations which must be done to try
  %-- potential integration which must enable to eliminate 2 functions
  %-- n2 is the number of integrations actually done
  n1:=for each h in intlist sum h;
  if (not vari) and (zerop n1) then
  <<n2:=0;
    if potflag then % else not necessary
    for h:=1:(length vl) do ifac0:=cons(nil,ifac0)
  >>                           else
  <<if tr_genint then
    <<terpri()$write "integration of the expression : "$
      eqprint p>>$
    if pri then
    <<terpri()$write"at first all multiple complete integration">>;
    %-- At first if possible n2 integrations of the whole equation
    h:=multipleint(intlist,ftem,p,vari,vl,genflag,nil,nil,nil)$
		   % potflag=nil, partial=nil, doneintvar=nil
    if h then
    <<p:=car h;
      ifac0:=cadr h;  % ifac0 is the list of lists of integr. factors
      newcond:=caddr h;
      n2:=cadddr h;   % number of done integrations
      % doneintvar & intfacdep for the two halfs of integrations
      % from the two parts of ifac0
      h:=nil;
      iflag:='success;
    >>   else n2:=0;
    ftem:=union(fnew_,ftem)$
  >>;
  %------------ Existence of a potential ?
  if (n1=n2) and potflag and (nfsub>1) then
  %---- at least 2 functions to solve for
  <<if not zerop n2 then            %---- update listoflds
    listoflds:=uplistoflds(reverse ifac0,listoflds)$
    if pri then <<terpri()$write"uplistoflds:",listoflds>>$
    if h:=potintegrable(listoflds) then
    <<ili1a:=car h; ili2a:=cadr h;
      % The necess. differentiations of the potential
      if pri then
      <<terpri()$write"potintegrable:",ili1a,"  ",ili2a>>$

      if pri then <<write"+++ intlist=",intlist,
			   "    ili1a=",ili1a,
			   "    ili2a=",ili2a>>$
      %-- distributing the integrating factors of ifac0 among
      %-- the two lists ifac1b and ifac2b which are so far nil
      %-- such that (ifac1b and ili1a are disjunct) and
      %--           (ifac2b and ili2a are disjunct)
      v1a:=vlofintlist(vl,ili1a);
      v2a:=vlofintlist(vl,ili2a);

      hh:=t;
      cop:=reverse ifac0;
      ifac1a:=ili1a;ifac2a:=ili2a;
      while hh and cop do <<
	% cop is a list of lists of integr. factors
	if car cop then h:=vlofintfaclist(vl,cdar cop)
		   else h:=nil;
	if freeoflist(h,v2a) and (car ifac2a=0) then <<
	  ifac1b:=cons( nil, ifac1b);
	  ifac2b:=cons( reverse car cop, ifac2b)
	>>                   else
	if freeoflist(h,v1a) and (car ifac1a=0) then <<
	  ifac2b:=cons( nil, ifac2b);
	  ifac1b:=cons( reverse car cop, ifac1b)
	>>                   else
        if car cop then hh:=nil;
        ifac1a:=cdr ifac1a;
        ifac2a:=cdr ifac2a;
	cop:=cdr cop;
      >>;
      % the elements of ifac1b,ifac2b are in reverse order to
      % ifac1a,ifac2a and are in the same order as vl, also
      % the elements in the infac-lists are in inverse order,
      % i.e. in the order the integrations have been done
      if pri then <<terpri()$
		    write  "ifac1a=",ifac1a,"  ifac1b=",ifac1b,
		    "  ifac2a=",ifac2a,"  ifac2b=",ifac2b >>$

      %-- lists of integrations to be done to both parts
      if hh then
      repeat % possibly a second try with part2 integrated first
      <<n1:=for each n1 in ili1a sum n1;
	% n1 .. number of remaining integrations of the first half
	p1:=multipleint(ili1a,ftem,p,nil,vl,genflag,t,t,
			% potflag=t, partial=t
			union(vlofintlist(vl,ili2a),
			      vlofintlist(vl,ifac1b)))$
	% that the variables of integration are not in ifac1b
	% was already checked. Only restriction: the integrating
	% factors must not depend on the last argument.

	ftem:=union(fnew_,ftem)$
	if p1 then <<
	  ifac1a:=cadr p1;
	  % ifac1a is now the list of integrating factors
	  if newcond then newcond:=nconc(newcond,caddr p1)
		     else newcond:=caddr p1;
	  if pri then <<terpri()$write"mul2: newcond=",newcond>>$
	  n2:=cadddr p1;
	  p1:=car p1
	>>;
        if p1 and (n1=n2) then
        %--- if the first half has been integrated suff. often
        <<%--- integrating the second half sufficiently often
	  n1:=for each n1 in ili2a sum n1;
	  % calculation of the 2. part which is not contained in p1
	  p2:=p1;
	  cop:=ifac1a; hh:=vlrev; % because ifac1a is reversed
	  while cop do <<
	    h:=car cop;cop:=cdr cop;
	    v:=car hh;hh:=cdr hh;
	    % h is the list of integrating factors of the v-integr.
	    while h do <<
	      p2:=reval list('QUOTIENT,list('DF,p2,v),car h);
	      h:=cdr h
	    >>
	  >>;
	  p2:=reval reval list('PLUS,p,list('MINUS,p2));
	  p2:=multipleint(ili2a,ftem,p2,nil,vl,genflag,t,nil,
			  % potflag=t, partial=nil
			  union(vlofintlist(vl,ili1a),
			        vlofintlist(vl,ifac2b)))$
	  ftem:=union(fnew_,ftem)$
	  if p2 then <<
	    ifac2a:=cadr p2;
	    % ifac2a is now list of integrating factors
	    if newcond then newcond:=nconc(newcond,caddr p2)
		       else newcond:=caddr p2;
	    if pri then <<terpri()$write"mul3: newcond=",newcond>>$
	    n2:=cadddr p2;
	    p2:=car p2
	  >>;
	  if p2 and (n1=n2) then
	  % if the second half has been integrated sufficiently often
	  <<% can both halfes be solved for different functions
            % i.e. are they algebraic and linear in different functions?
            pot1:=nil;
            pot2:=nil;
            for each h in fsub do <<
              if ld_deriv_search(p1,h,vl) = (nil . 1) then
              pot1:=cons(h,pot1);
              if ld_deriv_search(p2,h,vl) = (nil . 1) then
              pot2:=cons(h,pot2);
            >>$
            if (null not_included(pot1,pot2)) or
               (null not_included(pot2,pot1)) then p2:=nil
          >>$
	  if p2 and (n1=n2) then
	  <<iflag:='potint;
	    % ifac1b,ifac2b are in reverse order to ifac1a,ifac2a!
	    pot1:=newfct(fname_,vl,nfct_)$  % the new potential fct.
	    pot2:=pot1;
	    nfct_:=add1 nfct_$
	    fnew_:=cons(pot1,fnew_);
	    v:=vlrev;
	    while v do
	    <<cop:=car ifac1a; ifac1a:=cdr ifac1a;
	      while cop do <<
	        pot1:=reval list('QUOTIENT,list('DF,pot1,car v),car cop);
	        cop:=cdr cop
	      >>;
	      cop:=car ifac2a; ifac2a:=cdr ifac2a;
	      while cop do <<
	        pot2:=reval list('QUOTIENT,list('DF,pot2,car v),car cop);
	        cop:=cdr cop
	      >>;
	      v:=cdr v;
	    >>;
	    p:=addintco(list('PLUS,p1,reval pot2),
		        ftem,ifac1b,vlrev,nil)$
	    newcond:=cons(addintco(list('PLUS,p2,
				        list('MINUS,reval pot1)),
				   ftem,ifac2b,vlrev,nil),
			  newcond) % vari=nil
	  >>
	  ;if pri then write":::"$
	>>;
	secnd:=not secnd;
	% retry in different order of integration, p is still the same
	if (iflag neq 'potint) and secnd then
	<<cop:=ili1a;ili1a:=ili2a;ili2a:=cop>>
      >> until (iflag eq 'potint) or (not secnd)
    >>;
  >>$

  %--------- returning the result
  return if not iflag then nil
		      else
  <<if iflag neq 'potint then  % constants of integration
    p:=addintco(p, ftem, % the completely reversed ifac0
    <<h:=nil;
      while ifac0 do <<h:=cons(reverse car ifac0,h);ifac0:=cdr ifac0>>;
      h
    >>, vl, vari)$
    if pri then
    <<terpri()$write"ENDE INTEGRATEPDE"$deprint(cons(p,newcond))>>$
    cons(p,newcond)
  >>
end$ % of integratepde

symbolic procedure intpde(p,ftem,vl,x,potint)$
% integration of an polynomial expr. p w.r.t. x
begin scalar f,ft,l,l1,l2,vl,k,s,a,iflag,flag$
  ft:=ftem$
  vl:=cons(x,delete(x,vl))$
  while ftem and not flag do
  <<f:=car ftem$ % integrating all terms including car ftem
    if member(x,fctargs f) then
    <<l1:=l:=lderiv(p,f,vl)$
      while not (flag or (iflag:=intlintest(l,x))) do
      <<k:=reval coeffn(p,car l,cdr l)$
	if intcoefftest(car lderiv(k,f,vl),car l,vl) then
	<<a:=decderiv(car l,x)$
	  k:=reval list('INT,subst('v_a_r_,a,k),'v_a_r_)$
	  k:=reval subst(a,'v_a_r_,k)$
	  s:=cons(k,s)$
	  p:=reval aeval list('DIFFERENCE,p,list('DF,k,x))$
	  if (l:=lderiv(p,f,vl))=l1 then flag:='neverending
                                             else l1:=l
	>>                                        else
	flag:='coeffld
      >>$
      % iflag='nofct is the so far integrable case
      % the non-integrable cases are: flag neq nil,
      % (iflag neq nil) and (iflag neq 'nofct), an exception to the
      % second case is potint where non-integrable rests are allowed
      if iflag='nofct then ftem:=smemberl(ftem,p)
		      else if potint or (fctlength f<length vl) then
			   <<ftem:=cdr ftem$flag:=nil>>         else
			   flag:=(iflag or flag)
    >>                     else
    ftem:=cdr ftem
  >>$
  return
  if not flag then
  <<l:=explicitpart(p,ft,x)$
    l1:=list('INT,l,x)$
    s:=reval aeval cons('PLUS,cons(l1,s))$
    if freeint_ and
       (null freeof(s,'INT)) then nil
                             else <<
      k:=start_let_rules()$
      l2 := reval {'DF,l1,x} where !*precise=nil;
      if 0 neq (reval {'DIFFERENCE,l,l2} where !*precise=nil) then <<
        write"REDUCE integrator error:"$terpri()$
        algebraic write "int(",l,",",x,") neq ",l1;terpri()$
        write"Result ignored.";terpri()$
        stop_let_rules(k)$
        nil
      >> else <<
        p:=reval reval aeval list('DIFFERENCE,p,l2)$
        stop_let_rules(k)$
        if poly_only then if ratexp(s,ft) then list(s,p)
	    			          else nil
		     else list(s,p)
      >>
    >>
  >>          else nil$
end$ % of intpde

symbolic procedure explicitpart(p,ft,x)$
% part of a sum p which only explicitly depends on a variable x
begin scalar l$
if not member(x,argset smemberl(ft,p)) then l:=p
else if pairp p then
   <<if car p='MINUS then
	l:=reval list('MINUS,explicitpart(cadr p,ft,x))$
   if (car p='QUOTIENT) and not member(x,argset smemberl(ft,caddr p))
   then
      l:=reval list('QUOTIENT,explicitpart(cadr p,ft,x),caddr p)$
   if car p='PLUS then
      <<for each a in cdr p do
	  if not member(x,argset smemberl(ft,a)) then l:=cons(a,l)$
      if pairp l then l:=reval cons('PLUS,l)>> >>$
if not l then l:=0$
return l$
end$

symbolic procedure intconst(co,vl,x,ifalist)$
% The factors in ifalist must be in the order of integrations done
if null ifalist then 0 else
begin scalar l,l2,f,coli,cotmp$
  while ifalist do <<
    cotmp:=coli;
    coli:=nil;
    while cotmp do <<
      coli:=cons(list('INT,list('TIMES,car ifalist,car cotmp),x),coli);
      cotmp:=cdr cotmp
    >>;
    coli:=cons(1,coli);
    ifalist:=cdr ifalist
  >>;

  while coli do
  <<f:=newfct(fname_,delete(x,vl),nfct_)$
    nfct_:=add1 nfct_$
    fnew_:=cons(f,fnew_)$
    l:=cons(list('TIMES,f,car coli),l)$
    coli:=cdr coli
  >>$
  if length l>1 then l:=cons('PLUS,l)
		else if pairp l then l:=car l
				else l:=0$
  if co and co neq 1 then
  if pairp co then
  <<if car co='TIMES then co:=cdr co
		     else co:=list co$
    while co do <<if my_freeof(car co,x) then l2:=cons(car co,l2)$
		  co:=cdr co>>
  >>
	      else if co neq x then l2:=list co$
  return reval if l2 then cons('TIMES,cons(l,l2))
		     else l
end$

symbolic procedure intcoefftest(l,l1,vl)$
begin scalar s$
return if not pairp l then t
       else if car l='DF then
	       <<s:=decderiv(l1,car vl)$
	       if pairp s and pairp cdr s then s:=cddr s
					  else s:=nil$
	       if diffrelp(cons(cddr l,1),cons(s,1),vl) then t
						       else nil>>
else t$
end$

symbolic procedure fctlinear(p,f)$
<<p:=ldiffp(p,f)$
(null car p) and (cdr p=1)>>$

symbolic procedure intlintest(l,x)$
%  Test,ob in einem Paar (fuehrende Ableitung.Potenz)
%  eine x-Ableitung linear auftritt
if not car l then
   if zerop cdr l then 'nofct
		  else 'nonlin
else                                    %  Fkt. tritt auf
  if (car l) and (cdr l=1) then         %  fuehr. Abl. tritt linear auf
		if pairp car l then     %  Abl. der Fkt. tritt auf
		    if caar l='DF then
			if member(x,cddar l) then nil
					%  x-Abl. tritt auf
			else if member(x,fctargs cadar l) then 'noxdrv
				else 'noxdep
		    else 'nodrv
		else 'nodrv
  else 'nonlin$

symbolic procedure decderiv(l,x)$
%  in Diff.ausdr. l wird Ordn. d. Abl. nach x um 1 gesenkt
begin scalar l1$
return if l then if car l='DF then
	<<l1:=decderiv1(cddr l,x)$
	if l1 then cons('DF,cons(cadr l,l1))
		 else cadr l>>
			    else l
	   else nil$
end$

symbolic procedure decderiv1(l,x)$
if null l then nil
else
if car l=x then
     if cdr l then
	    if numberp cadr l then
		  if cadr l>2 then cons(car l,cons(sub1 cadr l,cddr l))
		  else cons(car l,cddr l)
	    else cdr l
     else nil
else cons(car l,decderiv1(cdr l,x))$

symbolic procedure integratede(q,ftem,genflag)$
%  Integration of a de
%  result: newde if successfull, nil otherwise
begin scalar l,l1,l2,fl$
 ftem:=smemberl(ftem,q)$

 again:
 if l1:=integrableode(q,ftem) then       % looking for an integrable ode
 if l1:=integrateode(q,car l1,cadr l1,caddr l1,ftem) then
				       % trying to integrate it
 <<l:=append(cdr l1,l);
   q:=simplifypde(car l1,ftem,nil)$
   ftem:=smemberl(union(fnew_,ftem),q)$
   fl:=t
 >>$
 if l1:=integratepde(q,ftem,nil,genflag,potint_) then
				       % trying to integrate a pde
 <<q:=car l1$
   for each a in cdr l1 do
   <<ftem:=union(fnew_,ftem)$
     if (l2:=integratede(a,ftem,nil)) then l:=append(l2,l)
				      else l:=cons(a,l)
   >>$
   fl:=t$
   if null genflag then l1:=nil$
   ftem:=smemberl(union(fnew_,ftem),q);
   goto again
 >>$

 if fl then
 <<l:=cons(q,l)$
   l:=for each a in l collect reval aeval a$
   l:=for each a in l collect
	  if pairp a and (car a='QUOTIENT) then cadr a
					   else a>>$
 return l$
end$

symbolic procedure intflagtest(q,fullint)$
if flagp(q,'to_int) then
 if fullint then
  if get(q,'full_int) then t else
  if get(q,'starde) then nil else
  begin scalar fl,vl,dl,l,n,mi$
   n:=get(q,'nvars)$
   for each f in get(q,'rational) do            % only rational fcts
       if fctlength f=n then fl:=cons(f,fl)$
   if null fl then return nil$
   vl:=get(q,'vars)$
   dl:=get(q,'derivs)$
   for each d in dl do
    if member(caar d,fl) then
       put(caar d,'maxderivs,maxderivs(get(caar d,'maxderivs),cdar d,vl))$
   dl:=fl$
   while vl do
    <<mi:=car get(car fl,'maxderivs)$
    l:=list car fl$
    put(car fl,'maxderivs,cdr get(car fl,'maxderivs))$
    for each f in cdr fl do
      <<if (n:=car get(f,'maxderivs))=mi then l:=cons(f,l)
        else if n<mi then
          <<l:=list f$
          mi:=n>>$
        put(f,'maxderivs,cdr get(f,'maxderivs))
      >>$
    dl:=intersection(l,dl)$
    if dl then vl:=cdr vl
          else vl:=nil>>$
   for each f in fl do remprop(f,'maxderivs)$
  return dl
  end
 else t$

symbolic procedure integrate(q,genintflag,fullint)$
%  integrate pde q; if genintflag is not nil then indirect
%  integration is allowed
%  if fullint is not nil then only full integration is allowed
%  Es wird noch nicht ausgenutzt:
%    1) Fcts, die rational auftreten
%    2) starde
begin scalar l$
  if intflagtest(q,fullint) then
  <<if (l:=integratede(get(q,'val),get(q,'fcts),genintflag)) then
    <<ftem_:=reverse ftem_$
      for each f in reverse fnew_ do
        ftem_:=fctinsert(f,ftem_)$
      ftem_:=reverse ftem_$
      fnew_:=nil$
      flag1(q,'to_eval)$
      update(q,car l,ftem_,get(q,'vars),t,list(0))$
      l:=cons(q,mkeqlist(cdr l,ftem_,get(q,'vars),
                         allflags_,t,get(q,'orderings)))$
      remflag1(q,'to_int)$
      if print_ then
         <<terpri()$
         if cdr l
         then write "Indirect integration of ",q," yields ",l
         else write "Integration of ",q>>$
    >>$
    remflag1(q,'to_int)>>$
  return l$
end$

symbolic procedure quick_integrate_one_pde(pdes)$
begin scalar m,q,p$
  % find the lowest order derivative wrt. only one variable
  while pdes and
        (get(car pdes,'length) = 1) do <<  % only 1 term
    q:=caar get(car pdes,'derivs)$
    if ( length q = 2      ) or
       ((length q = 3) and
        (fixp caddr q) and
        ((null p) or
         (caddr q<m) )     ) then
    <<p:=car pdes;
      m:=if (length q = 2) then 1
                           else caddr q>>;
    pdes:=cdr pdes
  >>$
  if p then p:=integrate(p,nil,t)$
  return p
end$

symbolic procedure integrate_one_pde(pdes,genintflag,fullint)$
%  trying to integrate one pde
begin scalar l,l1,m,p$
  % at first selecting all eligible de's
  m:=-1$
  while pdes do <<
    if flagp(car pdes,'to_int) and not(get(car pdes,'starde)) then <<
      l:=cons(car pdes,l);
      if get(car pdes,'nvars)>m then m:=get(car pdes,'nvars)$
    >>;
    pdes:=cdr pdes
  >>;
  l:=reverse l;

  % find an equation with as many as possible variables for integration
  while m>=0 do <<
    l1:=l$
    while l1 do
    if (get(car l1,'nvars)=m) and
       (p:=integrate(car l1,genintflag,fullint)) then <<
      m:=-1$
      l1:=nil
    >>                                           else l1:=cdr l1$
    m:=sub1 m
  >>$
return p$
end$

endmodule$

%********************************************************************
module generalized_integration$
%********************************************************************
%  Routines for generalized integration of pde's containing unknown
%  functions
%  Author: Andreas Brand
%  December 1991

symbolic procedure gintorder(p,ftem,vl,x)$
%  reorder equation p
begin scalar l,l1,q,m,b,c,q1,q2$
  if pairp(p) and (car p='QUOTIENT) then <<
   q:=caddr p$
   p:=cadr p$
   l1:=gintorder1(q,ftem,x,t)$
   if DepOnAllVars(car l1,x,vl) then return nil;
   q1:=car l1;
   q2:=cadr l1;
  >>$
  if pairp(p) and (car p='PLUS) then p:=cdr p   % list of summands
				else p:=list p$
  while p do
  <<l1:=gintorder1(car p,ftem,x,nil)$
    if DepOnAllVars(car l1,x,vl) then l:=p:=nil
				 else <<l:=termsort(l,l1)$p:=cdr p>> >>$
  if l then
  <<l:=for each a in l collect if cddr a then
	       <<b:=car a$
		 c:=cdr reval coeff1(cons('PLUS,cdr a),x,nil)$
		 m:=0$
		 while c and (car c=0) do <<c:=cdr c$m:=add1 m>>$
		 if m>0 then b:=list('TIMES,list('EXPT,x,m),b)$
		 cons(reval b,c)>>
					 else
		 cons(reval car a,cdr reval coeff1(cadr a,x,nil))$
    if q then <<
       l:=for each a in l collect
	      cons(car a,for each s in cdr a collect
			     reval list('QUOTIENT,s,q2))$
       l:=for each a in l collect
	      cons(reval list('QUOTIENT,car a,q1),cdr a)
    >>$
>>$
return l$
end$

symbolic procedure DepOnAllVars(c,x,vl)$
% tests for occurence of vars from vl in factors of c depending on x
begin scalar l$
if pairp c and (car c='TIMES) then c:=cdr c
			      else c:=list c$
while c and vl do
<<if not my_freeof(car c,x) then
     for each v in vl do if not my_freeof(car c,v) then l:=cons(v,l)$
  vl:=setdiff(vl,l)$
  c:=cdr c
>>$
return (null vl)$
end$

symbolic procedure gintorder1(p,ftem,x,mode2)$
%  reorder a term p
begin scalar l1,l2,sig$
% mode2 = nil then
%    l2:list of factors of p not depending
%       on x or beeing a power of x
%    l1:all other factors
% mode2 = t then
%    l2:list of factors of p not depending on x
%    l1:all other factors

if pairp p and (car p='MINUS) then <<sig:=t$p:=cadr p>>$
if pairp p and (car p='TIMES) then p:=cdr p
			      else p:=list p$
for each a in p do
   <<if my_freeof(a,x) and freeoflist(a,ftem) then l2:=cons(a,l2)
     % freeoflist(a,ftem) to preserve linearity
     else if mode2 then l1:=cons(a,l1)
     else if a=x then l2:=cons(a,l2)
     else if pairp a and (car a='EXPT) and (cadr a=x) and fixp caddr a
     then l2:=cons(a,l2)
     else l1:=cons(a,l1)>>$
if pairp l1 then
   if cdr l1 then l1:=cons('TIMES,l1)
	     else l1:=car l1$
if pairp l2 then
   if cdr l2 then l2:=cons('TIMES,l2)
	     else l2:=car l2$
if sig then if l2 then l2:=list('MINUS,l2)
		  else l2:=list('MINUS,1)$
return list(if l1 then l1 else 1,if l2 then l2 else 1)$
end$

symbolic procedure partint(p,ftem,vl,x,genint)$
begin scalar f,neg,l1,l2,n,k,l,h$
  if tr_genint then <<
    terpri()$
    write "generalized integration of the unintegrated rest : "$
    eqprint p
  >>$
  l:=gintorder(p,ftem,vl,x)$
  % would too many new equations and functions be necessary?
  if pairp(l) and (length(l)>genint) then return nil;
  l:=for each s in l collect <<
    h:=varslist(car s,ftem,vl)$
    if h=nil then <<
      list('TIMES,x,car s,cons('PLUS,cdr s))
    >>       else <<
      f:=newfct(fname_,h,nfct_)$
      nfct_:=add1 nfct_$
      fnew_:=cons(f,fnew_)$
      neg:=t$
      n:=sub1 length cdr s$
      k:=-1$
      if (pairp car s) and (caar s='DF) then
        <<h:=reval reval list('DIFFERENCE,cadar s,list('DF,f,x,add1 n))$
        if not zerop h then l1:=cons(h,l1)$
        l2:=cddar s>>
      else
        <<h:=signchange reval reval list('DIFFERENCE,car s,
                                   list('DF,f,x,add1 n))$
        if not zerop h then l1:=cons(h,l1)$
        l2:=nil>>$
      reval cons('PLUS, for each sk on cdr s collect
                 <<neg:=not neg$
                   k:=add1 k$
                   reval list('TIMES,if neg then -1 else 1,
                              append(list('DF,f,x,n-k),l2),
                              tailsum(sk,k,x)               )
                 >>
                )
    >>
  >>$
  if l then l:=cons(reval cons('PLUS,l),l1)$
  if tr_genint then
  <<terpri()$
    write "result (without constant or function of integration): "$
    if l then <<
     eqprint car l$
     write "additional equations : "$
     deprint cdr l
    >>   else write " nil "$
  >>$
  return l$
end$

symbolic procedure tailsum(sk,k,x)$
begin scalar j$
j:=-1$
return reval cons('PLUS,
for each a in sk collect
    <<j:=j+1$
    list('TIMES,a,prod(j+1,j+k),list('EXPT,x,j)) >> )
end$

symbolic procedure prod(m,n)$
if m>n then 1
       else for i:=m:n product i$

endmodule$


%********************************************************************
module intfactor$
%********************************************************************
%  Routines for finding integrating factors of PDEs
%  Author: Thomas Wolf
%  July 1994

% The following without factorization --> faster but less general
%symbolic procedure fctrs(p,vl,v)$
%begin scalar fl1,fl2,neg;
%
%write"p=",p;
%
% if car p='MINUS then <<neg:=t;p:=cdr p>>$
% return
% if not pairp p then if my_freeof(p,v) and (not freeoflist(p,vl)) then
%                     list(p,1,neg)                             else
%                     list(1,p,neg)
%                else if car p='PLUS then list(1,p,neg)
%                                    else
% if car p='TIMES then
% <<for each el in cdr p do if my_freeof(el,v) and (not freeoflist(p,vl)) then
%   fl1:=cons(el,fl1)                                                  else
%   fl2:=cons(el,fl2);
%   if pairp fl1 then fl1:=cons('TIMES,fl1);
%   if pairp fl2 then fl2:=cons('TIMES,fl2);
%   if not fl1 then fl1:=1;
%   if not fl2 then fl2:=1;
%   list(fl1,fl2,neg)
% >>              else if my_freeof(p,v) and (not freeoflist(p,vl)) then
% list(p,1,neg)                                                  else
% list(1,p,neg)
%end$ % of fctrs
%

symbolic procedure fctrs(p,indep,v)$
begin scalar fl1,fl2;
 p:=cdr reval factorize p;
 for each el in p do
 if freeoflist(el,indep) and
    ((v=nil) or (not my_freeof(el,v))) then fl1:=cons(el,fl1)
				       else fl2:=cons(el,fl2);
 if null fl1 then fl1:=1;
 if null fl2 then fl2:=1;
 if pairp fl1 then if length fl1 = 1 then fl1:=car fl1
				     else fl1:=cons('TIMES,fl1);
 if pairp fl2 then if length fl2 = 1 then fl2:=car fl2
				     else fl2:=cons('TIMES,fl2);
 return list(fl1,fl2)
end$ % of fctrs


symbolic procedure extractfac(p,indep,v)$
% looks for factors of p dependent of v and independent of indep
% and returns a list of the numerator factors and a list of the
% denominator factors
begin scalar nu,de$
 return
 if (pairp p) and (car p='QUOTIENT) then
 <<nu:=fctrs( cadr p,indep,v);
   de:=fctrs(caddr p,indep,v);
   list( reval if car  de neq 1 then list('QUOTIENT,  car nu,  car de)
                                else car nu,
	       if cadr de neq 1 then list('QUOTIENT, cadr nu, cadr de)
                                else cadr nu
       )
 >>                                 else fctrs(p,indep,v)
end$ % of extractfac

%----------------------------

symbolic procedure get_kernels(ex)$
% gets the list of all kernels in ex
begin scalar res,pri$
 % pri:=t;
 ex:=reval ex$
 if pri then <<terpri()$write"ex=",ex>>;
 if pairp ex then
 if (car ex='QUOTIENT) or (car ex='PLUS) or (car ex='TIMES) then
 for each s in cdr ex do res:=union(get_kernels s,res)      else
 if (car ex='MINUS) or
    ((car ex='EXPT)    and
%    (numberp caddr ex)) then % not for e.g. (quotient,2,3)
     (cadr ex neq 'E)  and
     (cadr ex neq 'e)  and
     (not fixp cadr ex)   ) then res:=get_kernels cadr ex
			    else res:=list ex
	     else if idp ex then res:=list ex$
 if pri then <<terpri()$write"res=",res>>;
 return res$
end$

%------------------

symbolic procedure specialsol(p,vl,fl,x,indep,gk)$
% tries a power ansatz for the functions in fl in the kernels
% of p to make p to zero
% indep is a list of kernels, on which the special solution should
% not depend. Is useful, to reduce the search-space, e.g. when an
% integrating factor for a linear equation for, say f, is to be
% found then f itself can not turn up in the integrating factor fl
% gk are kernels which occur in p and possibly extra ones which
% e.g. are not longer in p because of factorizing p but which are
% likely to play a role, if nil then determined below
% x is a variable on which each factor in the special solution has
% to depend on.
begin
 scalar e1,e2,n,nl,h,hh,ai,sublist,eqs,startval,pri,printold,pcopy;
 %pri:=t;
 p:=num p;
 pcopy:=p;
 if pri then <<
  terpri()$write"The equation for the integrating factor:";
  terpri()$eqprint p;
 >>;
 if null gk then gk:=get_kernels(p);
 for each e1 in fl do <<
  h:=nil; %---- h is the power ansatz
  if pri then
  for each e2 in gk do <<
   terpri()$write"e2=",e2;
   if my_freeof(e2,x) then write" freeof1";
   if not freeoflist(e2,fl) then write" not freeoflist"$
   if not freeoflist(e2,indep) then write" dependent on indep"
  >>;
  %----- nl is the list of constants to be found
  for each e2 in gk do
  if (not my_freeof(e2,x)) and % integ. fac. should depend on x
     freeoflist(e2,fl)  and % the ansatz for the functions to be
			    % solved should not include these functions
     freeoflist(e2,indep) then <<
   n:=gensym();nl:=cons(n,nl);
   h:=cons(list('EXPT,e2,n),h);
  >>;
  if h then <<
   if length h > 1 then h:=cons('TIMES,h)
		   else h:=car h;
   %-- the list of substitutions for the special ansatz
   sublist:=cons((e1 . h),sublist);
   if pri then <<terpri()$write"Ansatz: ",e1," = ",h>>;
   p:= reval num reval subst(h,e1,p);
   if pri then <<terpri()$write"p=";eqprint p>>
  >>
 >>;
 if null h then return nil;
 %------- An numerical approach to solve  for the constants
 if nil then << % numerical approach
  %--- Substituting all kernels in p by numbers
  on rounded;
  precision 20;
  terpri()$terpri()$write"Before substituting numerical values:";
  eqprint p;
  terpri()$terpri()$write"Constants to be calculated: ";
  for each n in nl do write n,"  ";

  for each e1 in nl do <<
   h:=p;
   for each e2 in gk do
   if freeoflist(e2,fl) then
   if pairp e2 and ((car e2 = 'DF) or (car e2 = 'INT)) then <<
    n:=list('QUOTIENT,1+random 30028,30029);
    terpri();write"substitution done: ",e2," = ",n;
    h:=subst(list('QUOTIENT,1+random 30028,30029),e2,h)
   >>;
   for each e2 in gk do
   if freeoflist(e2,fl) then
   if not(pairp e2 and ((car e2 = 'DF) or (car e2 = 'INT))) then <<
    n:=list('QUOTIENT,1+random 30028,30029);
    terpri();write"substitution done: ",e2," = ",n;
    h:=subst(list('QUOTIENT,1+random 30028,30029),e2,h)
   >>;

   terpri()$terpri()$write"The equation after all substitutions: ";
   terpri()$
   eqprint h;

   eqs:=cons(reval h,eqs);
   startval:=cons(list('EQUAL,e1,1),startval)
  >>;
  if length eqs = 1 then eqs:=cdr eqs
		    else eqs:=cons('LIST,eqs);
  if length startval = 1 then startval:=cdr startval
			 else startval:=cons('LIST,startval);
  terpri()$write"start rdsolveeval!";terpri()$terpri()$
  h:=rdsolveeval list(eqs,startval);
  eqs:=nil;
  off rounded;
 >>;

 %----- An analytical approach to solve for the constants
 if null pri then <<printold:=print_;print_:=nil>>;
 if p and not zerop p then                  % uebernommen aus SEPAR
 if not (pairp p and (car p='QUOTIENT) and  %      "       "    "
	intersection(argset smemberl(fl,cadr p),vl)) then
 p:=separ2(p,fl,vl)                                  else
 p:=nil;
 if null pri then print_:=printold;

 if p then <<
  % possibly investigating linear dependencies of different caar p
  % solve(lasse x-abhaengige und nl-unabhaengige faktoren fallen von
  %       factorize df(reval list('QUOTIENT, caar p1, caar p2),x),nl).
  while p do
  if freeoflist(cdar p,nl) then <<eqs:=nil;p:=nil>>
  % singular system --> no solution
			   else <<
   eqs:=cons(cdar p,eqs);
   p:=cdr p
  >>;
 >>;
 if pri then <<terpri()$write"eqs1=",eqs>>;
 if (null eqs) or (length eqs > maxalgsys_) then return nil
                                            else <<
  if pri then <<
   terpri()$write"The algebraic system to solve for ",nl," is:";
   if length eqs > 1 then deprint eqs
                     else eqprint car eqs
  >>;
  if length eqs > 1 then eqs:=cons('LIST,eqs)
		    else eqs:=car eqs;

  if pri then <<terpri()$write"eqs2=",eqs;terpri();write"nl=",nl>>$

  % for catching the error message `singular equations'
  hh:=cons('LIST,nl);
  eqs:=<<
   ai:=!!arbint;
   h:=errorset({'solveeval,mkquote{eqs, hh}},nil,nil)
   where !*protfg=t;
   erfg!*:=nil;
   if errorp h then nil else cdar h    % cdr for deleting 'LIST
  >>;

  if pri then <<terpri()$write"eqs3a=",eqs,"  ai=",ai,"  !!arbint=",
		!!arbint;terpri()>>$
  if not freeof(eqs,'ARBCOMPLEX) then <<
   eqs:=reval car eqs;
   for h:=(ai+1):!!arbint do
   eqs:=subst(0,list('ARBCOMPLEX,h),eqs);
   if pri then <<terpri()$write"eqs3b=",eqs;terpri()>>$
   eqs:=<<
    h:=errorset({'solveeval,mkquote{eqs, hh}},nil,nil)
    where !*protfg=t;
    erfg!*:=nil;
    if errorp h then nil else cdar h    % cdr for deleting 'LIST
   >>;
  >>;

  if pri then <<terpri()$write"eqs3c=",eqs;terpri()>>$

  %--- eqs is the list of solutions for the constant exponents of the
  %--- integrating factor

  if null eqs then return nil;
  if length nl=1 then eqs:=list eqs;
  if pri then <<write"nl=",nl,"  eqs4=",eqs;terpri()>>;

  for each e1 in eqs do <<  % each e1 is a list of substitutions
   if pri then <<terpri()$write"e2=",e1;terpri()>>$
   if car e1='LIST then e1:=cdr e1;
   if pri then <<terpri()$write"e3=",e1;terpri()>>$
   for each e2 in e1 do <<
    if pri then algebraic write"solution:",symbolic e2;
    sublist:=subst(caddr e2,cadr e2,sublist)
   >>;
   if pri then <<terpri()$write"The sublist is:",sublist>>
  >>;
 >>;
 if pri then <<terpri()$write"pcopy1=",pcopy;terpri()>>$
 for each e1 in sublist do <<
  pcopy:=subst(cdr e1,car e1,pcopy);
  if pri then <<terpri()$write"e1=",e1;terpri();
                write"pcopy2=",pcopy;terpri()>>$
 >>$
 if pri then <<terpri()$write"pcopy3=",reval pcopy;terpri()>>$
 if pri then <<terpri()$write"pcopy4=",reval reval pcopy;terpri()>>$
 if not zerop reval reval pcopy then return nil else
 return for each e1 in sublist collect (car e1 . reval cdr e1)
end$   % of specialsol

%------------------

symbolic operator findintfac$
symbolic procedure findintfac(pl,ftem,vl,x,doneintvar,intfacdep,
			      factr,verbse)$

% - pl is a list of equations from which the *-part (inhomogeneous
%   terms) have been dropped.
% - each equation of pl gets an integrating factor h
% - doneintvar is a list of variables, on which the integrating factor
%   should not depend. The chances to find an integrating factor
%   increase if the inhomogeneous part of pl is dropped and
%   separately integrated with generalized integration later.
% - if factr is not nil then the equation(s) pl is(are) at first
%   factorized, e.g. if integration(s) have already been done
%   and there is a chance that the equation can factorize, thereby
%   simplify and giving a higher chance for integrability.

begin
 scalar h,newequ,tozero,fl,e1,pri,factr,exfactors,ftr,gk;
 % exfactors is the list of factors extracted at the beginning
 % pri:=t;
 factr:=t; % whether tozero should be factorized

 if pri then <<terpri()$write"START VON FINDINTFAC">>;
 %--- Generation of the condition for the integrating factor(s) in fl
 for each e1 in pl do <<
  %--- extracting factors dependend on x and independent of
  %--- doneintvar but only if integrations have already been done,
  %--- i.e. (doneintvar neq nil)
  gk:=union(gk,get_kernels(e1));
  if factr then <<ftr:=extractfac(e1,append(doneintvar,ftem),x);
		  if not evalnumberp car ftr then
		  gk:=union(gk,list car ftr);
                >>
	   else ftr:=list(1,nil);
  exfactors:=cons(car ftr,exfactors);
  if car ftr neq 1 then <<
   e1:=cadr ftr;
   if pri then <<terpri()$write"extracted factor:",eqprint car ftr>>;
  >>;
  %--- fl is to become the list of integrating factors h
  h:=gensym();
  depl!*:=cons(list(h,x),depl!*)$
  depend h,x;
  fl:=h . fl;
  e1:=intpde(reval list('TIMES,h,e1),ftem,vl,x,t);
  if e1 and car e1 then <<
   newequ:=car e1 . newequ;
   tozero:=cadr e1 . tozero;
   if pri then <<
    terpri()$write" the main part of integration:"$ eqprint(car e1);
    terpri()$write"car e1=",car e1;
    terpri()$write" the remainder of integration:"$ eqprint(cadr e1);
    terpri()$write"cadr e1=",cadr e1;
   >>
  >>;
 >>;
 if null tozero then return nil;
 %-------- newequ is the integral
 newequ:=if length pl > 1 then cons('PLUS,newequ)
			  else car newequ;
 %-------- tozero is the PDE for the integrating factor
 tozero:=reval if length pl > 1 then cons('PLUS,tozero)
				else car tozero;

 if pairp tozero and (car tozero='QUOTIENT) then tozero:=cadr tozero$

 if factr then <<
  h:=cdr reval list('FACTORIZE,tozero)$
  if pri then <<terpri()$write"The factors of tozero:",h>>;
  tozero:=nil;
  for each e1 in h do
  if smemberl(fl,e1) then tozero:=cons(e1,tozero)$
  tozero:= reval if length tozero > 1 then cons('TIMES,tozero)
				      else car tozero;
 >>;
 if nil and pri then <<write"tozero =";eqprint tozero >>;
 h:=nil;
 % actually only those f in ftem, in which pl is nonlinear, but also
 % then only integrating factors with a leading derivative low enough
 h:=specialsol(tozero,vl,fl,x,append(ftem,doneintvar),gk);
 % h:=specialsol(tozero,vl,fl,x,doneintvar,gk);
 if pri then <<write"h=",h;terpri()>>;
 if h then <<
  for each e1 in h do << % each e1 is one integrating factor determined
   if pri then <<terpri()$write"e1=",e1;
		   terpri()$write"newequ=",newequ;terpri()>>;
   newequ:=reval subst(cdr e1,car e1,newequ);
   if pri then <<terpri()$write"newequ=",newequ>>;
  >>
 >>   else if pri then write"no integrating factor";

 %--- delete all dependencies of the functions in fl
 %--- must come before the following update
 for each e1 in fl do <<
   depl!*:=delete(assoc(e1,depl!*),depl!*)$
   depl!*:=delete(assoc(mkid(e1,'_),depl!*),depl!*)$
 >>;

 %--- update intfacdep
 for each e1 in vl do
 if (e1 neq x) and my_freeof(intfacdep,e1) and
    ((not my_freeof(h,e1)) or (not my_freeof(exfactors,e1)))
 then intfacdep:=cons(e1,intfacdep);

 %--- returns nil if no integrating factor else a list of the
 %--- factors and the integral
 if h and print_ and verbse then <<
  terpri()$write"The integrated equation:";
  eqprint newequ;
  terpri()$
  if length pl = 1 then write"An integrating factor has been found:"
                   else write"Integrating factors have been found: "$
 >>;
 return if (null h) or (zerop newequ) then nil else
 list(newequ,
      for each e1 in h collect <<
       ftr:=car exfactors;
       exfactors:=cdr exfactors;
       gk:=if ftr=1 then cdr e1
		    else reval list('QUOTIENT,cdr e1,ftr);
       if print_ and verbse then mathprint gk;
       gk
      >>,
      intfacdep)
end$

endmodule$


%********************************************************************
module odeintegration$
%********************************************************************
%  Routines for integration of ode's containing unnown functions
%  Author: Thomas Wolf
%  August 1991

symbolic procedure integrateode(de,fold,xnew,ordr,ftem)$
% once equations are factorized before integration the % * lines
% can be droped or reduced
begin scalar newde,newnewde,l,l1,h,factrs,fc,changd,newcond,facnum$
 if pairp de and (car de='QUOTIENT) then de:=cadr de$   % *
 factrs:=cdr reval list('FACTORIZE,de);                 % *
 facnum:=length factrs;                                 % *
 l:=for each fc in factrs collect                       % *
 if not smember(fold,fc) then <<facnum:=facnum-1;fc>>   % *
                         else                           % *
 <<if facnum>1 then <<l1:=integrableode(fc,ftem);       % *
                      if l1 then <<fold:=car l1;        % *
                                   xnew:=cadr l1;       % *
                                   ordr:=caddr l1       % *
                                 >>                     % *
                    >>                                  % *
               else l1:=t;

  h:= % the integrated equation
  if not l1 then nil else                               % *
  if not xnew then <<    % Integr. einer alg. Gl. fuer eine Abl.
   newde:=cadr solveeval list(fc,fold)$

   if not freeof(newde,'ROOT_OF) then nil
				 else <<
    newde:=reval list('PLUS,cadr newde,list('MINUS,caddr newde))$
    if (l:=integratepde(newde,ftem,nil,genint_,nil)) then
    <<newcond:=append(newcond,cdr l);car l>>
		%genflag=t,potflag=nil
					       else nil
   >>
  >>         else                % eine ode fuer ein f?
  if not pairp fold then         % i.e. not df(...,...), i.e. fold=f
			 odeconvert(fc,fold,xnew,ordr,ftem)
				 % --> ode fuer eine Abl. von f
		    else <<
   newde:=odeconvert(fc,fold,xnew,ordr,ftem)$
   if not newde then nil
		else <<
     newnewde:=cadr solveeval list(newde,fold)$
     newnewde:=reval list('PLUS,cadr newnewde,list('MINUS,
						   caddr newnewde))$
     ftem:=union(fnew_,ftem)$
     newnewde:=integratede(newnewde,ftem,nil)$
     if newnewde then <<newcond:=append(newcond,cdr newnewde);
			car newnewde>>
		 else newde
   >>
  >>;
  if h then <<changd:=t;h>>     % factors to be collected % *
       else fc                                            % *
 >>;

 return if not changd then nil
                      else
 cons(if length l > 1 then cons('TIMES,l)                 % *
                      else car l         ,newcond)        % *

end$  % of integrateode

symbolic procedure odecheck(ex,fint,ftem)$
% assumes an revaled expression ex
% Does wrong if car ex is a list!
begin scalar a,b,op,ex1$
		   %***** ex is a ftem-function *****
 if ex=fint then             % list(ex,0,0,..)
   <<a:=list ex$
     ex:=fctargs ex$
     while ex do
      <<a:=append(list(0,0),a)$
      ex:=cdr ex>>$
      % not checked if it is a function of an expression of x
     op:=reverse a>>
 else if pairp ex then
			  %***** car ex is 'df *****
 if (car ex)='df then
  <<a:=odecheck(cadr ex,fint,ftem)$
  if not pairp a then op:=a
  else                            % a is list(fctname,0,0,..,0,0)
   <<op:=list(car a)$
   a:=fctargs car a$              % a is list(variables), not checked
   ex:=cddr ex$                   % ex is list(derivatives)
   while a do
    <<ex1:=ex$
    while ex1 and ((car a) neq (car ex1)) do ex1:=cdr ex1$
    if null ex1 then op:=cons(0,cons(0,op))
    else
     <<if not cdr ex1 then b:=1   % b is number of deriv. of that var.
     else
      <<b:=cadr ex1$
      if not numberp b then b:=1>>$
     op:=cons(b,cons(b,op))>>$
    a:=cdr a>>$
   op:=reverse op>> >>
 else
	     %***** car ex is a standard or other function *****
  <<a:=car ex$                    % for linearity check
  ex:=cdr ex$
  if a='INT then ex:=list reval car ex$
  while (op neq '!_abb) and ex do
   <<b:=odecheck(car ex,fint,ftem)$
   if b then                                  % function found
     if b eq '!_abb then op:='!_abb           % occures properly
                    else op:=odetest(op,b)$
   ex:=cdr ex>> >>$
 return op
end$

symbolic procedure integrableode(p,ftem)$
if delength p>(if odesolve_ then odesolve_ else 0) then
   (if cont_ then
      if yesp("expression to be integrated ? ") then
	   integrableode1(p,ftem))
else integrableode1(p,ftem)$

symbolic procedure integrableode1(p,ftem)$
begin scalar a,b,u,vl,le,uvar,
	   fint,fivar,% the function to be integrated and its variables
	   fold,      % the new function of the ode
	   xnew,      % the independ. variable of the ode
	   ordr1,     % order of the ode
	   ordr2,     % order of the derivative for which it is an ode
	   intlist$   % list of ode's
  ftem:=smemberl(ftem,p)$
  vl:=argset ftem$
% p muss genau eine Funktion aus ftem von allen Variablen enthalten.
% Die Integrationsvariable darf nicht Argument anderer in p enthaltener
% ftem-Funktionen sein.
  a:=ftem$
  b:=nil$
  le:=length vl$
  while a and vl do
    <<u:=car a$
    uvar:=fctargs u$
    if (length uvar) = le then
       if b then
	  <<vl:=nil$a:=list(nil)>>
       else
	  <<b:=t$
	  fint:=u$
	  fivar:=uvar>>
    else vl:=setdiff(vl,uvar)$
    a:=cdr a>>$
  if not b then vl:=nil$
  le:=length p$
  if ((1<le) and vl) then
    <<a:=odecheck(p,fint,ftem)$
    if not atom a then                     % The equation is an ode
      <<ordr1:=0$
      ordr2:=0$
      xnew:=nil$
      a:=cdr a$
      b:=fivar$
      while b do
	<<if (car a) neq 0 then
	   <<fold:=cons(car b,fold)$
	   if (car a) > 1 then fold:=cons(car a,fold)>>$
	ordr2:=ordr2+car a$
	if (car a) neq (cadr a) then
	   <<xnew:=car b$
	   if not member(xnew,vl) then
	      <<b:=list(nil)$vl:=nil>>$
	   ordr1:=(cadr a) - (car a)>>$
	b:=cdr b$
	a:=cddr a>>$
      fold:=reverse fold$
	%fold is the list of diff.variables + number of diff.
      if fold then fold:=cons('df,cons(fint,fold))
	      else fold:=fint$
      if vl and ((ordr1 neq 0) or (ordr2 neq 0)) then
	intlist:=list(fold,xnew,ordr1,ordr2)
    >>     % of variable found
  >>$    % of if
  return intlist
end$  % of integrableode1

symbolic procedure odetest(op,b)$
if not op then b
else                           % op=nil --> first function found
  if (car op) neq (car b) then '!_abb else  % f occurs in differ. fct.s
begin scalar dif,a$
 dif:=nil$                     % dif=t --> different derivatives
 a:=list(car op)$              % in one variable already found
 op:=cdr op$
 b:=cdr b$
 while op do
  <<a:=cons(max(cadr op,cadr b),cons(min(car op,car b),a))$
  if (car a) neq ( cadr a) then
  if dif then
    <<a:='!_abb$
    op:=list(1,1)>>
  else dif:=t$
  op:=cddr op$
  b:=cddr b>>$
 if not atom a then a:=reverse a$
 return a      % i.e. '!_abb or  (fctname,min x1-der.,max x1-der.,...)
end$

symbolic procedure odeconvert(de,ford,xnew,ordr,ftem)$
begin scalar j,ford_,newco,oldde,newde,newvl,null_,ruli,zd$
%             trig1,trig2,trig3,trig4,trig5,trig6$
 ford_:=gensym()$
 depl!*:=delete(assoc(ford_,depl!*),depl!*)$
 depend1(ford_,xnew,t)$
 oldde:=reval subst(ford_,reval ford,de)$
 if pairp ford then                 % i.e.  (car ford) eq 'DF then
 << for j:=1:ordr do
   oldde:= subst( reval list('DF,ford_,xnew,j),
		  reval list('DF,ford,xnew,j), oldde)>>$
 algebraic !!arbconst:=0$
 newde:=algebraic first
        odesolve(symbolic oldde,symbolic ford_,symbolic xnew)$
 ruli:= start_let_rules()$
 newde:=reval newde;
 % Instead of the following test one should return several cases
 zd:=zero_den(newde,cons(ford_,ftem),union(list xnew,argset ftem));
% if safeint_ and zero_den(newde,ftem,argset ftem) then newde:=nil;
 if freeint_ and null freeof(newde,'INT) then newde:=nil;
 if newde and (cadr newde neq oldde) then <<   % solution found
  % Test der Loesung klappt nur, wenn Loesung explizit gegeben
  if cadr newde neq ford_ then <<
   if print_ then
    <<write "Solution of the ode is not explicitly given."$
    algebraic write "Equation is: ",algebraic symbolic oldde$
    algebraic write "Solution is: ",algebraic symbolic newde
   >>;
   if poly_only then % The solution must be rational in the
                     % function and constants of integration
   if not rationalp(newde,ford_) then newde:=nil else <<
    j:=1;
    while (j leq ordr) and
          rationalp(subst(ford_,list('arbconst,j),newde),ford_) do j:=j+1;
    if j leq ordr then newde:=nil
   >>;
   if newde then
   if (caadr newde = 'QUOTIENT) and (zerop caddr newde) then
   newde:={'EQUAL,cadadr newde,0}                       else
   if (caaddr newde = 'QUOTIENT) and (zerop cadr newde) then
   newde:={'EQUAL,0,cadr caddr newde}
  >>                      else <<
   null_:=reval reval aeval subst(caddr newde, ford_, oldde)$
%  reval reval because of a REDUCE bug for special data,
%  to be dropped as soon as possible
   if (null_ neq 0) then <<
%    newde:=nil$
    if print_ then <<
     write "odesolve solves :  "$
     deprint list oldde$
     write "to"$
     deprint list newde$
     Write "which inserted in the equation yields"$
     deprint list null_$
    >>
   >>
  >>
 >>$
 if newde then
 <<newde:=list('PLUS,cadr newde,list('MINUS,caddr newde))$
   if zerop reval list('PLUS,newde,list('MINUS,oldde)) then newde:=nil$
   if newde and (zd neq nil) then
   newde:=cons('TIMES,append(zd,list newde))$
 >>$
 depl!*:=delete(assoc(ford_,depl!*),depl!*)$
 stop_let_rules(ruli)$
 return
 if null newde then nil
	       else
 <<newde:=subst(ford,ford_,newde)$
   newvl:=delete(xnew,if (pairp ford and (car ford='DF))
			 then fctargs cadr ford
			 else fctargs ford)$
%   if pairp ford then newvl:=delete(xnew,cdr assoc(cadr ford,depl!*))
%                 else newvl:=delete(xnew,cdr assoc(ford,depl!*))$
   for j:=1:ordr do <<
    newco:=newfct(fname_,newvl,nfct_)$
    nfct_:=add1 nfct_$
    fnew_:=cons(newco,fnew_)$
    newde:=subst(newco,list('arbconst,j),newde)
%    newde:=subst(newco, prepf !*kk2f list('arbconst,j),newde)
%    newde:=reval subst(newco,list('arbconst,j),newde)
%    newde:=reval subst(newco, prepf !*kk2f list('arbconst,j),newde)
   >>$
   newde>>
end$

endmodule$

end$
