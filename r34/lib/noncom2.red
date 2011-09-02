%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        N O N C O M 2                             %
%                                                                  %
%                     A Package to redefine                        %
%                   noncommutativity in REDUCE                     %
%                                                                  %
%  Author:                Mathias Warns                            %
%                       Physics Institute                          %
%                       University of Bonn                         %
%                         Nussallee 12                             %
%                     D-5300 BONN 1 (F.R.G.)                       %
%                    <UNP008@DBNRHRZ1.bitnet>                      %
%                                                                  %
%  Version:                 2.0 250591                             %
%                                                                  %
%                                                                  %
% Designed for: REDUCE version 3.3 / 3.4                           %
% Tested on   : - IBM 3081/3084 VM/CMS MVS/XA                      %
%                  SLISP implementation of REDUCE                  %
%               - Intel 386/486 AT compatible computers            %
%                 PSL implemnetation of REDUCE                     %
%                                                                  %
%             Copyright (c) Mathias Warns 1990,1991                %
%                                                                  %
%                                                                  %
%   Permission is granted to any individual or institution to      %
%   use, copy or re-distribute this software as long as it is      %
%   not sold for profit, provided that this copyright  notice      %
%   is retained and the file is not altered.                       %
%                                                                  %
% **** Summary of changes since last issued version (1.0) ****     %
%                                                                  %
%   - Various small bugs have been corrected in the utility        %
%     functions                                                    %
%   - The sloppy use of CAR on atoms allowed in SLISP systems has  %
%     been removed                                                 %
%   - The pattern matching routine SUBS3TNC has been entirely      %
%      recoded for greater efficiency and is now used for ALL      %
%      terms (not only for the noncommuting cases)                 %
%      Procedures SUBLIST, LOCATE!_N and MTCHP1!*  added           %
%   - Enhanced tracing utilities added                             %
%   - NONCOMP has been changed to NONCOMP!* since the former       %
%     cannot be redefined on som systems                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%------------------------------------------------------------------
% this package relies on modified standard reduce routines         %
% and is therefore version dependent                               %
%-------------------------------------------------------------------
symbolic;
fluid '(!*nosq wtl!*);
!*nosq := t;

%------------------------------%
%   general utility functions  %
%------------------------------%
symbolic procedure trwrite u;
begin  scalar x;
if not flagp(car u,'tracing) then return nil;
write "**in procedure: ", car u; terpri();
for each x in cdr u do write x;
terpri();
end;

symbolic procedure funtrace u;
for each x in u do flag(list(x),'tracing);

deflist('((trwrite rlis) (funtrace rlis)),'stat);

symbolic procedure pnth!*(u,n); % slightly modified from pnth
   if null u then nil
    else if n=1 then u
    else pnth!*(cdr u,n-1);

symbolic procedure nth!*(u,n);
if length(u) < n then nil
else car pnth!*(u,n);

symbolic procedure kernelp u;  %new
% checks if an algebraic expression is a kernel
if null u or domain!*p u then nil
else if idp u then t
else if listp u and idp car u and not (car u memq
'(!*sq set setq plus minus difference times quotient))
 then t
else nil;

symbolic procedure spp u;  %new
% checks if u is a standard power
pairp u and kernelp car u;

symbolic procedure stp u;  %new
% checks if u is a s.t.
pairp u and spp car u;

symbolic procedure sfp2 u; %new
% checks if u if a s.f.
% sfp seems to be ill defined
pairp u and stp car u;

symbolic procedure tstp u; %new
% checks if u is a "true" standard term, i.e. a product term
stp u and (car !*f2a !*t2f u neq 'plus);

symbolic procedure !*!*a2f u;  %new
%converts u without call of subs2
begin scalar flg,res;
flg := subfg!*; subfg!* := nil;
res := !*a2f u;
subfg!* := flg;
return res
end;

symbolic procedure !*!*a2q u;  %new
%converts an algebraic expression into a s.q. using !*!*a2f
if car u eq 'quotient  then !*!*a2f cadr u . !*!*a2f caddr u
else  !*f2q !*!*a2f u;

symbolic procedure !*a2q u;  %new
%converts an algebraic expression into a s.q. using !*a2f
if (not atom u and car u eq 'quotient) then
        !*a2f cadr u . !*a2f caddr u
else  !*f2q !*a2f u;

symbolic procedure atsoc2(u,v);
% same as atsoc but looks for the caar part
begin scalar res;
for each x in v do
if (not atom car x and caar x eq u) then res:= x;
return res
end;

symbolic procedure sublist(u,v);
% u and v are lists of sp
% checks if all elements of u are included in v in the right order
% return a sublist of containing the elements of u + the rest of v
begin scalar x,z,y,w,reslist,n,u1;
if not (listp u and listp v) then
    rederr " invalid arguments to sublist";
%initialization
if null u or null v or not (V:= member(car u,v)) then return;
a : if null u then return append(reslist,append(u1,v));
    z:= v;
    x := car u;
    u := cdr u;
    if not (v:= member(x,z)) then return;
    v := cdr v;
    n:= length(z) - length(v) - 1;
    z := for k:= 1 : n collect nth(z,k);
    trwrite(sublist,"z= ",z," v= ",v," x= ",x);
a0: if null z then
    <<
       u1 := nconc(u1,list(x));
       go to a;
    >>;
    w := car z;
    z := cdr z;
    if noncommuting!_splist(w,u1) then go to a1
    else reslist := nconc(reslist,list(w));
    go to a0;
a1:
    z := reverse (w . z);
    if noncommutingsp(car z,x) then return;
    v := (car z) . v;
    z := reverse cdr  z;
    go to a0;
end;

symbolic procedure deleteall(x,u);
% deletes all occurrences of x in u
begin scalar y;
a:
y:= u;
if y equal (u:=delete(x,u)) then  return u
else go to a
end;

symbolic procedure deletemult(x,u);
% deletes multiples occurences of x in u
% keeping only one left
begin scalar y,n;
if null (y:= cdr member(x,u)) then return u;
n:=length(u)-length(y);
u := for  k:=1 :n collect nth(u,k);
while member(x,y) do y:=delete(x,y);
return nconc(u,y)
end;

symbolic procedure deletemult!* u;
% deletes all multiple occurences of elements in u
begin scalar x;
if null u then return u;
x:=list(car u);
u := cdr u;
for each y in u do
if not member(y,x) then nconc(x,list(y));
return x
end;

symbolic procedure listofvarnames u; %new
% u is a list of s.p.
%  returns list of vars in u
% we keep nil as  placeholder for numbers in u
if not listp u then rederr "invalid argument to listofvarnames"
else for each x in u collect if domain!*p x then (nil . 'free)
                             else if atom x then (nil . 'free)
                             else if idp car x then ((car x) . 'free)
                             else if idp caar x then ((caar x) . 'free);



symbolic procedure replsublist(u,v,w); %new
% v and w are p-lists
% u is anything
% replaces the sublist v in w by u
begin scalar n,x,res;
if not (x:= sublist(v,w)) then return w;
n:= length(w)-length(x);
% trwrite "n= ",n," x= ",x;
% u := if listp u then u else list(u);
% trwrite "u= ",u,listp u;
res := if zerop n then nil
       else for k:= 1 :n  collect nth(w,k);
res := if null res then u else nconc(res,u);
% trwrite "res= ",res;
return if (length(v) = length(x)) then res
       else  nconc(res,pnth(x,length(v)+1))
end;

symbolic procedure locate!_n(x,lst,n);
% returns the position of the n-th occurence of x in lst
% nil if not succesful
begin scalar n2,lst2,ntot;
if null lst then return nil;
lst2 := lst;
ntot:= 0;
a: if n = 0 then return ntot;
   n2:= locate(x,lst2);
%   trwrite "n2=",n2," lst2= ",lst2;
   if null n2 then return nil;
   lst2 := cdr pnth(lst2,n2);
   ntot := ntot+n2;
   n:= n-1;
   go to a;
end;


symbolic procedure term2listpows u;  %new
% u is a  s.t. containing only products
% return a list of the s.p. of u
begin
trwrite(term2listpows,"u= ",u);
return
if null u then u
else if atom u then list u
else if domain!*p cdr u then car u . list cdr u
else car u . term2listpows cadr u;
end;

symbolic procedure listprod2term u; %new
%  u is a list of product terms (numbers,s.p.,s.t.,s.f.)
% value is the s.q. obtained by multiplying all the terms together
begin scalar x,res;
if not listp u then rederr "invalid argument to listprod2term";
if null u then return u;
res:= car u;
res :=  if domain!*p res then !*d2q res
        else if spp res then  !*p2q res else if stp res then !*t2q res
        else if sfp2 res then  res . 1 else res;
% trwrite "res= ",res;
u :=cdr u;
a: if null u then return  res;
x := car u;
x := if domain!*p x then !*d2q x
     else if spp x then !*p2q x else if stp x then !*t2q x
     else if sfp2 x then x . 1 else x;
u := cdr u;
res := multsq(res,x);
go to a;
end;

% this routine gives the position of an object in a list. the first
% object is numbered 1. returns nil if the object can't be found.

symbolic procedure locate(u,v);
  if not member(u,v) then nil
  else if u=car v then 1
  else 1+locate(u,cdr v);

global '(domainlist!*);
symbolic procedure domain!*p u;
% this is a much more precise domain checker than domainp
null u or numberp u or (not atom u and memq(car u,domainlist!*));

%------------------------------------------------%
% new defintions of noncom and testing functions %
%------------------------------------------------%

% clear previous definitions of noncom
remflag('(noncom),'flagop);
remprop('noncom,'stat);

symbolic procedure noncomp!* u; % changed
% u is a kernel checks for noncom flag
if atom u then flagp(u,'noncom)
else flagpcar(u,'noncom);

symbolic procedure noncom u; %new
begin scalar y,liste;
if not listp u  then rederr(u, "invalid argument to noncom");
for each x in u do <<
 if not idp x then rederr(x, "invalid argument to noncom");
 flag(list(x),'noncom);
 liste:=get(x,'noncommutes);
 y := delete(x,u);
 put(x,'noncommutes,deletemult!* append(liste,y));
   >>;
 return nil
 end;
deflist('((noncom rlis)),'stat);

symbolic procedure noncommuting(u,v);  % new
% u and v are two kernels
% checks for noncommuting
begin scalar list,res;
u := if atom u then u else car u;
v := if atom v then v else car v;
% the following is needed in the physop package
u := reverse explode u;
if length(u) > 2 then <<
     if (car u eq '!1) and (cadr u eq '!-) then u := pnth(u,4);
     if (car u eq '!+) and (cadr u eq '!!) then u := pnth(u,3); >>;
u := intern compress reverse u;
v := reverse explode v;
if length(v) > 2 then <<
     if (car v eq '!1) and (cadr v eq '!-) then v := pnth(v,4);
     if (car v eq '!+) and (cadr v eq '!!) then v := pnth(v,3); >>;
v := intern compress reverse v;
  if not (noncomp!* u and noncomp!* v) then nil
  else <<
       list :=get(u,'noncommutes);
       res:=member(v,list);
             >>;
return res
end;

symbolic procedure noncommutingterm u;  %new
% u is a standard term
% checks if there are some noncommuting products in u
begin scalar x,y;
if null u or domain!*p u or spp u then return nil;
x := tvar u;  % <-- term variable
u := cdr u;  % <-- tc (s.f.)
a: if null u or domain!*p u then return nil;
   y := car u;  % <-- lt
   if noncommutingf(x,list(y)) or noncommutingterm y then return t;
   u := cdr u;
   go to a
end;

symbolic  procedure noncommutingf(x,u); % new
  % x is a kernel, u  is a standard form
  % checks for noncommuting
    if domain!*p u then nil
    else noncommuting(x, mvar u) or noncommutingf(x, lc u)
         or noncommutingf(x, red u);

symbolic procedure noncommutingsp(u,v);
% u and v are sp or numbers
if null u or null v or numberp u or numberp v then nil
else noncommuting(car u,car v);

symbolic procedure noncommuting!_splist(u,v);
% u is a sp, v is a list of sp
% checks if u commutes with all elements of v
if null v or null u then nil
else noncommutingsp(u,car v) or noncommuting!_splist(u,cdr v);


%----------------------------------%
% modified multiplication routine  %
%----------------------------------%
symbolic procedure multf(u,v); % changed
   %u and v are standard forms.
   %value is standard form for u*v;
   begin scalar ncmp,x,y;
    a:  if null u or null v then return nil
         else if u=1 then return v     % onep
         else if v=1 then return u     % onep
         else if domainp u then return multd(u,v)
         else if domainp v then return multd(v,u)
         else if not(!*exp or ncmp!* or wtl!* or x)
          then <<u := mkprod u; v := mkprod v; x := t; go to a>>;
        x := mvar u;
        y := mvar v;
%  the following line has been replaced
%       if (ncmp := noncomp!* y) and noncomp!* x then return multfnc(u,v)
        if noncommuting(x,y) then return multfnc(u,v)
%    we have to put this clause here to prevent evaluation in case
%    of equal main vars
        else if noncommutingf(y, lc u) or (ordop(x,y) and (x neq y))
          then << x := multf(lc u,v);
                 y := multf(red u,v);
                 return if null x then y else lpow u .* x .+ y>>
         else if x eq y
         % two forms have the same mvars
          then << x := mkspm(x,ldeg u+ldeg v);
          y := addf(multf(red u,v),multf(!*t2f lt u,red v));
                 return if null x or null(u := multf(lc u,lc v))
                    then <<!*asymp!* := t; y>>
                   else if x=1 then addf(u,y)
                   else if null !*mcd then addf(!*t2f(x .* u),y)
                   else x .* u .+ y>>;
        x := multf(u,lc v);
        y := multf(u,red v);
       return if null x then y else lpow v .* x .+ y
   end;

%--------------------------------------------%
% procedures for ordering of expressions     %
%--------------------------------------------%



symbolic procedure ordp(u,v); % modified
   %returns true if u ordered ahead or equal to v, nil otherwise.
   %an expression with more structure at a given level is ordered
   % behind (and not ahead) of one with less;
   % ordering of numbers is left as default
   if null u then t
   else if null v then nil
   else if atom u then
           if atom v then
              if numberp u then
                  if numberp v then not u < v
                  else t
              else if numberp v then nil
                   else orderp(u,v)
           else t
    else if atom v then nil
    else if car u=car v then ordp(cdr u,cdr v)
    else ordp(car u,car v);

symbolic procedure reordop(u,v);  %changed
% modilfied so that every commuting op is ordered ahead
% of every noncommuting op
   if noncommuting(u,v) then t
   else if noncomp!* u and not noncomp!* v then nil
   else if noncomp!* v and not noncomp!* u then t
   else ordop(u,v);


%--------------------------------------------------%
% procedures for handling noncommutative           %
% terms in pattern matching                        %
%--------------------------------------------------%
% we have to modify subs3f1 since the handling of noncom mvars
% in subs3t is not correct  so we must prevent the system from
% calling this procedure

symbolic procedure subs3f1(u,l,bool); %modified
   %u is a standard form.
   %l is a list of possible matches.
   %bool is a boolean variable which is true if we are at top level.
   %value is a standard quotient with all product substitutions made;
   begin scalar x,z;
        z := nil ./ 1;
    a:  if null u then return z
         else if domainp u then return addsq(z,u ./ 1)
         else if bool and domainp lc u then go to c;
 %      the following line has been changed
 %      x := subs3t(lt u,l);
        x := !*subs3tnc(lt u,l);
 %      x := if noncommutingterm lt u then !*subs3tnc(lt u,l)
 %          else subs3t(lt u,l);
        if not bool                             %not top level;
         or not mchfg!* then go to b;           %no replacement made;
        mchfg!* := nil;
        if numr x = u and denr x = 1 then <<x := u ./ 1; go to b>>
         % also shows no replacement made (sometimes true with non
         % commuting expressions)
         else if null !*resubs then go to b
         else if !*sub2 or powlis1!* then x := subs2q x;
           %make another pass;
        x := subs3q x;
    b:  z := addsq(z,x);
        u := cdr u;
        go to a;
    c:  x := list lt u ./ 1;
        go to b
   end;


symbolic procedure !*subs3tnc(u,v); %new
% header procedure for subs3tnc
% u is a standard term, v a list of matching templates
% call subs3tnc on every product term of u and return a s.q.
begin scalar x,y,res,flg,mchflg;
% if u not standard term
% trwrite "before: mchfg!*= ",mchfg!*;
if domain!*p u then return !*d2q u;
if kernelp u then return !*k2q u;
if spp u then return !*p2q u;
% now comes the interesting cases
y := !*f2a !*t2f u; % convert u in an algebraic expression
  if car y eq 'quotient then rederr "!*subs3tnc cannot handle s.q.!";
  if car y eq 'times then return subs3tnc(u,v);
  if car y eq 'minus then return
       negsq(subs3tnc(car !*!*a2f cadr y,v));
  res := nil . 1;
a:  y := cdr y;
% trwrite "y= ",y;
    if null y then << mchfg!* := mchflg; return res >>;
    x := !*!*a2f car y;
    if mchfg!* then <<mchflg := mchfg!*; mchfg!* := nil >>;
    res := if numberp x then addsq(!*d2q x,res)
           else addsq(res,subs3tnc(car x,v));
%   trwrite "after: mchfg!*= ",mchfg!*;
%   trwrite "res= ",res;
  go to a
end;

symbolic procedure subs3tnc(u,v);  %new
% new version including more general templates
% u is a product term in s. t. form,
% v a list of matching templates.
% value is the s.t. modified by relevant substitutions
% (eg a s.q. in general case)
begin scalar termlist,termlist2,templ,temp,tempsp,tempvar,freetemp,rhs,
        lhs,bool,boolp,matchinglist,x,y,z,z1,w,w1,termlist3,na,ka,n,k;
% return trivial cases
if domain!*p u then return !*d2q u;
% build a list of s.p. in u
  termlist := term2listpows u;
  trwrite(subs3tnc, "termlist= ",termlist);
% these are the variable names in termlist
  termlist2:= listofvarnames termlist;
  mchfg!* := nil;
% this is the main loop scanning each template
% terminating if no match found
a: if null v then return !*f2q !*t2f u;
% refresh the list of variable names
   termlist2 := subst('free,'used,termlist2);
% select a template
    templ := car v;
    v := cdr v;
    trwrite(subs3tnc," templ= ",templ," v= ",v);
% rhs is an algebraic expression
    rhs := nth(templ,3);
% boolean  expression to be satisfied by the matching args
    bool := cdadr templ;
% flag to indicate if exact power matching required
    boolp := caadr templ;
    trwrite(subs3tnc, "bool= ",bool," boolp= ",boolp);
% lhs of templ is already a list of s.p.
    lhs := car templ;
    temp := nil; freetemp := nil; % initialization
% first we separate the lhs in a list of free  and of nonfree
% variables
    for each x in reverse lhs do
       if memq(car x,frlis!*) then freetemp := x . freetemp
       else temp := x . temp;
    lhs := nil; % will be rebuilt later on
    trwrite(subs3tnc, "temp= ",temp,"freetemp= ",freetemp);
    if null temp then go to b;
% we allow nonexact power matching only in the case of 2 sp in lhs
    boolp := if length(temp) = 2 then boolp
              else t;
    k := 1; % counter for number of terms in lhs
    na:= 1;
    z1 := nil;
    matchinglist := nil;
a1: if (k > length(temp)) then go to b;
aa: if (k < na) then go to a;
    tempsp := nth(temp,k);
    tempvar := if idp car tempsp then car tempsp
               else caar tempsp;
a2: n:= locate((tempvar . 'free),termlist2);
    if numberp n then go to ab;
    k := k-1;
    z1 := nil;
    lhs := if null lhs then lhs
           else cdr lhs;
    go to aa;
ab: % mark tempvar as being used in the pattern matching process
    termlist2 :=append(for k:=1 :(n-1) collect nth(termlist2,k),
                       ((tempvar . 'used) . pnth(termlist2,n+1)));
    trwrite(subs3tnc, "termlist2= ",termlist2);
    x:= nth(termlist,n);
    z:= mtchp1!*(x,tempsp,boolp,bool,z1);
    if null cdr z then go to a2;
    if car z then
    <<
        if not sublist(car z ,matchinglist) then
           matchinglist:= append(matchinglist,car z);
        trwrite(subs3tnc, "matchinglist= ",matchinglist);
% do the substitutions of car z in temp and bool
        for each y in car z do
        <<
           bool := subst(cdr y,car y,bool);
           temp := subst(cdr y,car y,temp)
        >>;
    >>;
    lhs := x . lhs;
    trwrite(subs3tnc, "lhs= ",lhs);
    z1:= cdr z;
    na:= k;
    k:= k + 1;
    go to a1;
b:  if not sublist(car z1,matchinglist) then
       matchinglist:= append(matchinglist,car z1);
%  special hack for nonexact power matching
    if (length(lhs) = 2) then
    <<
       x := cadr lhs; % this is the first term !
       y := nth(temp,1);
       if ((na:= cdr y) neq (ka := cdr x))  then
       <<
           termlist := replsublist(list(car x .** (ka - na),
                                        car x .** na),
                                   list(car x .** ka),termlist);
            w := list(car x . na);
       >>
       else w:= list(x);
       x:= car lhs;  % this is the second term
       y := nth(temp,2);
       if (na:= cdr y) neq (ka := cdr x) then
       <<
          termlist := replsublist(list(car x .** na,
                                       car x .** (ka - na)),
                                  list(car x .** ka),termlist);
          lhs := (car x . na) . w;
       >>
       else lhs := x . w;
    >>;
% from here on in principle all the terms in lhs are matched
    lhs := reverse lhs;
% cross check
    if null (termlist3 := sublist(lhs,termlist)) then go to a;
    n := length(termlist)-length(termlist3);
    trwrite(subs3tnc, "n= ",n);
% rebuild the termlist after rearrangement
    termlist := append(for k := 1 : n collect nth(termlist,k),
                       termlist3);
    na := length(freetemp);
    if (na = 0) then go to d;
    freetemp := reverse freetemp;
%  recalculation  of n is necessary because lhs do not sit
% in front of termlist3
    n:= length(termlist) - length(member(car lhs,termlist));
% match the free variable(s) to be placed in front
    if (n < na) then go to a;
% take all the terms in front in this case
    if (na = 1) and (cdar freetemp = 1) then
    <<
        lhs := termlist;
        matchinglist:= append(matchinglist,list(caar freetemp .
                       !*q2a listprod2term append(
                       for k:=1 :n collect nth(termlist,k),
                       for k:= (length(lhs)+1) : length(termlist3)
                           collect nth(termlist3,k))));
    >>
    else for k:=1 :na do
    <<
        x := nth(termlist,n-k+1);
        y := nth(freetemp,k);
        z:= mtchp1(x,y,boolp,bool);
        if not sublist(car z ,matchinglist) then
              matchinglist:= append(matchinglist,car z);
        for each w in car z do
            y:= subst(cdr w,car w,y);
        lhs := y . lhs;
        if (na:= cdr y) neq (ka := cdr x) then
        <<
           termlist := replsublist(list(car x .** (ka - na),
                          car x .** na),list(car x .** ka),termlist);
           n:= n+1;
        >>
    >>;
d:
  trwrite(subs3tnc,"lhs= ",lhs);
  trwrite(susb3tnc," termlist= ",termlist);
  trwrite(subs3tnc,"matchinglist= ",matchinglist);
% replace the free variables in the rhs
  for each x in matchinglist do
             rhs:= subst(cdr x, car x,rhs);
  trwrite(subs3tnc," rhs= ",rhs);
% and finally we replace the lhs in u by the rhs
% for this we have to replace in the termlist the s.p. of lhs by
% the rhs  converted to a standard quotient
  rhs := list(simp rhs);
  trwrite(subs3tnc," rhs= ",rhs);
  termlist:= replsublist(rhs,lhs,termlist);
  trwrite(subs3tnc, "resulting termlist = ",termlist);
  mchfg!* := t;
  return listprod2term termlist
  end;


symbolic  procedure mtchp1!*(u,v,boolp,bool,z);
% u is a sp, v is  a sp to be matched against x
% boolp is a flg (t if exact power matching required)
%  bool is a boolean expr to be satisfied during matching
% z is a list of possible matchings for the free variables in y
% returns a list of matching pair lists first is that element of z
% which leads to a succesful matching  or nil
begin scalar temp1,bool1,x,z1;
   if null z then return append(list(nil),mtchp2(u,v,boolp,bool));
a: if null z then return list(nil);
   x:= car z;
   z:= cdr z;
 % trwrite "x= ",x," z= ",z;
   temp1:= v;
   bool1 := bool;
   for each  w in x do
   <<
      temp1:= subst(cdr w,car w, temp1);
      bool1 := subst(cdr w,car w,bool1);
   >>;
   if (z1:=mtchp2(u,temp1,boolp,bool1)) then return x . z1;
   go to a;
   end;

symbolic procedure mtchp2(u,v,boolp,bool);
% does the same job as mtchp1 but more accurately
% since mtchp1 does not check bool at all
begin scalar z,x,reslist,bool1,bool2;
z := reverse mtchp1(u,v,boolp,bool);
if (bool = t) then return z;
a: if null z then return reslist;
   x := car z;
   z := cdr z;
   bool1 := bool;
   for each w in x do bool1 := subst(cdr w,car w,bool1);
   bool2:= bool1;
% trick used here to check for remaining free variables in bool
   for each w in frlis!* do bool2:=subst(nil,w, bool2);
   trwrite(mtchp2, "bool1= ",bool1," bool2= ",bool2);
   if ((bool2 = bool1) and null eval bool1) then return nil
   else reslist := x . reslist;
   go to a
end;

end;
