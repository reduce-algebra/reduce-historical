module ezgcd;  % Header module for ezgcd package.

% Authors: A. C. Norman and P. M. A. Moore.

create!-package('(ezgcd alphas coeffts ezgcdf facmisc facstr interfac
		  linmodp mhensfns modpoly multihen unihens),
		'(factor));

fluid '(!*trallfac !*trfac factor!-level factor!-trace!-list);

factor!-level:=0;  % start with a numeric value.

symbolic procedure !*d2n a; if null a then 0 else a;

symbolic procedure adjoin!-term (p,c,r);
   if null c then r else (p .* c) .+ r;

symbolic smacro procedure ttab n; spaces(n-posn());

symbolic smacro procedure polyzerop u; null u;

symbolic smacro procedure didntgo q; null q;

symbolic smacro procedure depends!-on!-var(a,v);
  (lambda !#!#a; (not domainp !#!#a) and (mvar !#!#a=v)) a;

symbolic procedure errorf u;
   rerror(ezgcd,1,list("Factorizer error:",u));

smacro procedure printstr l; << prin2!* l; terpri!*(nil) >>;

smacro procedure printvar v; printstr v;

smacro procedure prinvar v; prin2!* v;

symbolic smacro procedure factor!-trace action;
   begin scalar stream;
      if !*trallfac or (!*trfac and factor!-level = 1)
	then stream := nil . nil
       else stream := assoc(factor!-level,factor!-trace!-list);
      if stream then <<stream := wrs cdr stream; action; wrs stream>>
   end;
 
symbolic smacro procedure getm2(a,i,j);
   % Store by rows, to ease pivoting process.
   getv(getv(a,i),j);

symbolic smacro procedure putm2(a,i,j,v);
   putv(getv(a,i),j,v);

symbolic smacro procedure !*f2mod u; u;

symbolic smacro procedure !*mod2f u; u;

% A load of access smacros for image sets follow:

symbolic smacro procedure get!-image!-set s; car s;

symbolic smacro procedure get!-chosen!-prime s; cadr s;

symbolic smacro procedure get!-image!-lc s; caddr s;

symbolic smacro procedure get!-image!-mod!-p s; cadr cddr s;

symbolic smacro procedure get!-image!-content s; cadr cdr cddr s;

symbolic smacro procedure get!-image!-poly s; cadr cddr cddr s;

symbolic smacro procedure get!-f!-numvec s; cadr cddr cdddr s;

symbolic smacro procedure put!-image!-poly!-and!-content
   (s,imcont,impol);
  list(get!-image!-set s,
       get!-chosen!-prime s,
       get!-image!-lc s,
       get!-image!-mod!-p s,
       imcont,
       impol,
       get!-f!-numvec s);

symbolic procedure printvec(str1,n,str2,v);
<< for i:=1:n do <<
    prin2!* str1;
    prin2!* i;
    prin2!* str2;
    printsf getv(v,i) >>;
   terpri!*(nil) >>;

endmodule;

end;
