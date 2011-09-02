module a2dip;
  %/*Convert an algebraic (prefix) form to distributive polynomial*/

%/*Authors: R. Gebauer, A. C. Hearn, H. Kredel*/

% Modified by: H. Melenk.

fluid '(dipvars!* dipzero !*vdpinteger);

symbolic procedure a2dip u;
%   /*Converts the algebraic (prefix) form u to a distributive poly.
%     We assume that all variables used have been previously
%     defined in dipvars!*, but a check is also made for this*/
   if atom u then a2dipatom u
    else if not atom car u or not idp car u 
     then typerr(car u,"dipoly operator")
       % Handling expt separately because the exponents should
       % not be simplified as domain elements.
    else if car u = 'expt then dipfnpow(a2dip cadr u,caddr u)
    else (if x then apply(x,list for each y in cdr u collect a2dip y)
         else a2dipatom u)
          where x = get(car u,'dipfn);

expr procedure a2dipatom u;
%   /*Converts the atom (or kernel) u into a distributive polynomial*/
   if u=0 then dipzero
    else if numberp u or not(u member dipvars!*)
      then dipfmon(a2bc u,evzero())
    else dipfmon(a2bc 1,mkexpvec u);

expr procedure dipfnsum u;
%   /*U is a list of dip expressions. Result is the distributive poly
%    representation for the sum*/
   (<<for each y in cdr u do x := dipsum(x,y); x>>) where x = car u;

put('plus,'dipfn,'dipfnsum);

put('plus2,'dipfn,'dipfnsum);

expr procedure dipfnprod u;
%   /*U is a list of dip expressions. Result is the distributive poly
%    representation for the product*/
%   /*Maybe we should check for a zero*/
   (<<for each y in cdr u do x := dipprod(x,y); x>>) where x = car u;

put('times,'dipfn,'dipfnprod);

put('times2,'dipfn,'dipfnprod);

expr procedure dipfndif u;
%   /*U is a list of two dip expressions. Result is the distributive
%    polynomial representation for the difference*/
   dipsum(car u,dipneg cadr u);

put('difference,'dipfn,'dipfndif);

symbolic procedure dipfnpow(v,n);
%  V is a dip. Result is the distributive poly v**n.
  (if not fixp n or n<0 
     then typerr(n,"distributive polynomial exponent")
    else if n=0 then if dipzero!? v then rerror(dipoly,1,"0**0 invalid")
                  else w
    else if dipzero!? v or n=1 then v
    else if dipzero!? dipmred v
     then dipfmon(bcpow(diplbc v,n),intevprod(n,dipevlmon v))
    else <<while n>0 do
         <<if not evenp n then w := dipprod(w,v);
             n := n/2;
           if n>0 then v := dipprod(v,v)>>;
         w>>)
    where w := dipfmon(a2bc 1,evzero());

% put('expt,'dipfn,'dipfnpow);

expr procedure dipfnneg u;
%   /*U is a list of one dip expression. Result is the distributive
%    polynomial representation for the negative*/
   (if dipzero!? v then v
    else dipmoncomp(bcneg diplbc v,dipevlmon v,dipmred v))
    where v = car u;

put('minus,'dipfn,'dipfnneg);

symbolic procedure dipfnquot u;
%   /*U is a list of two dip expressions. Result is the distributive
%    polynomial representation for the quotient*/
   if dipzero!? cadr u or not dipzero!? dipmred cadr u
       or not evzero!? dipevlmon cadr u
       or (!*vdpinteger and not bcone!? diplbc cadr u)
      then typerr(dip2a cadr u,"distributive polynomial denominator")
    else dipfnquot1(car u,diplbc cadr u);

expr procedure dipfnquot1(u,v);
   if dipzero!? u then u
    else dipmoncomp(bcquot(diplbc u,v),
                dipevlmon u,
                dipfnquot1(dipmred u,v));

put('quotient,'dipfn,'dipfnquot);

endmodule;

end;
