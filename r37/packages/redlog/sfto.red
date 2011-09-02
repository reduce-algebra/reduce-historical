% ----------------------------------------------------------------------
% $Id: sfto.red,v 1.8 1999/03/22 15:26:43 dolzmann Exp $
% ----------------------------------------------------------------------
% Copyright (c) 1995-1999 Andreas Dolzmann and Thomas Sturm
% ----------------------------------------------------------------------
% $Log: sfto.red,v $
% Revision 1.8  1999/03/22 15:26:43  dolzmann
% Changed copyright information.
% Added and reformatted comments.
%
% Revision 1.7  1999/01/17 15:33:13  dolzmann
% Added procedure sfto_sqfpartz for computing the square-free part of an
% integer.
%
% Revision 1.6  1999/01/10 12:09:16  dolzmann
% Added procedures sfto_zdeqn, sfto_zdgtn, sfto_zdgen for the decomposition of
% integers.
%
% Revision 1.5  1996/10/08 13:54:59  dolzmann
% Renamed "degree parity decomposition" to "parity decomposition".
% Adapted names of procedures and switches accordingly.
%
% Revision 1.4  1996/09/05 11:17:48  dolzmann
% Added procedure sfto_monfp.
%
% Revision 1.3  1996/07/07 12:56:12  dolzmann
% Fixed a bug in sfto_preducef and sfto_greducef.
%
% Revision 1.2  1996/05/13 13:54:26  dolzmann
% Added procedure sfto_sqrtf.
%
% Revision 1.1  1996/04/30 12:06:46  sturm
% Merged ioto, lto, and sfto into rltools.
%
% Revision 1.2  1996/04/30 09:13:33  sturm
% Added procedure sfto_gcdf implementing the Davenport test.
%
% Revision 1.1  1996/03/22 12:19:17  sturm
% Moved.
%
% Revision 1.5  1996/03/04 17:15:46  sturm
% Added procedure sfto_decdegf.
% Moved sfto_reorder from module ofsf to this module.
%
% Revision 1.4  1996/03/04 13:09:54  dolzmann
% Moved procedures sfto_groebnerf, sfto_preducef, sfto_greducef and
% loading of groebner packages from module ofsfgs to this module.
%
% Revision 1.3  1995/08/30  08:26:45  sturm
% Fixed a bug in procedure sfto_dprpartf.
%
% Revision 1.2  1995/08/30  07:35:19  sturm
% Added procedures sfto_dprpartf and sfto_dprpartf1.
%
% Revision 1.1  1995/05/29  14:47:23  sturm
% Initial check-in.
%
% ----------------------------------------------------------------------
lisp <<
   fluid '(sfto_rcsid!* sfto_copyright!*);
   sfto_rcsid!* := "$Id: sfto.red,v 1.8 1999/03/22 15:26:43 dolzmann Exp $";
   sfto_copyright!* := "Copyright (c) 1995-1999 by A. Dolzmann and T. Sturm"
>>;

module sfto;
% Standard form tools.

load!-package 'groebner;
load!-package 'groebnr2;

fluid '(!*ezgcd !*gcd !*rldavgcd);

switch sfto_yun,sfto_tobey,sfto_musser;
!*sfto_yun := T;

put('sqfpart,'polyfn,'sfto_sqfpartf);
put('tsqsum,'psopfn,'sfto_tsqsum!$);
put('sqfdec,'psopfn,'sfto_sqfdec!$);
put('pdec,'psopfn,'sfto_pdec!$);
put('sfto_yun,'simpfg,
   '((T (setq !*sfto_tobey nil) (setq !*sfto_musser nil))));
put('sfto_tobey,'simpfg,
   '((T (setq !*sfto_yun nil) (setq !*sfto_musser nil))));
put('sfto_musser,'simpfg,
   '((T (setq !*sfto_tobey nil) (setq !*sfto_yun nil))));

procedure sfto_dcontentf(u);
   % Standard form tools domain content standard form. [u] is an SF.
   % Returns a domain element, which is the content of [u] as a
   % multivariate polynomial over the current domain.
   sfto_dcontentf1(u,nil);

procedure sfto_dcontentf1(u,g);
   % Standard form tools domain content standard form subroutine. [u]
   % is a term; [g] is a domain element. Returns the gcd of the
   % content of [u] and [g], which is a domain element.
   if g = 1 then
      g
   else if domainp u then
      sfto_gcdf(absf u,g)
   else
      sfto_dcontentf1(red u,sfto_dcontentf1(lc u,g));

procedure sfto_dprpartf(u);
   % Standard form tools domain primitive part standard form. [u] is
   % an SF. Returns an SF which is the primitive part of [u] as a
   % multivariate polynomial over the current domain.
   sfto_dprpartf1(u,sfto_dcontentf u);

procedure sfto_dprpartf1(u,c);
   % Standard form tools domain primitive part standard form
   % subroutine. [u] and [c] are SF's. Returns an SF which is the
   % primitive part of [u] as a multivariate polynomial over the
   % current domain.
   (if minusf w then negf w else w) where w = quotf(u,c);

procedure sfto_sqfpartf(u);
   % Standard form tools square-free part. [u] is a non-zero SF.
   % Returns an SF which is the square-free part of [u] as a
   % multivariate polynomial. The (domain) content is dropped.
   begin scalar c,pp;
      if domainp u then return 1;
      c := sfto_ucontentf u;
      pp := quotf(u,c);
      return multf(sfto_sqfpartf(c),quotf(pp,sfto_gcdf!*(pp,diff(pp,mvar u))))
   end;

procedure sfto_ucontentf(u);
   % Standard form tools univariate content standard form. [u] is an
   % SF. Returns the content of [u] as a univariate polynomial in its
   % [mvar] over the polynomial ring in all other contained variables.
   if domainp u then u else sfto_ucontentf1(u,mvar u);

procedure sfto_ucontentf1(u,v);
   % Standard form tools univariate content standard form subroutine.
   % [v] is a kernel; [u] is an SF with main variable [v]. Returns an
   % SF which is the content of [u] as an univariate polynomial in [v]
   % over the polynomial ring in all other contained variables.
   if domainp u or mvar u neq v then u else
      sfto_gcdf!*(lc u,sfto_ucontentf1(red u,v));

procedure sfto_uprpartf(u);
   % Standard form tools univariate primitive part. [u] is an SF.
   % Returns the primitive part of [u] as a univariate polynomial in
   % its [mvar] over the polynomial ring in all other contained
   % variables.
   quotf(u,sfto_ucontentf u);

procedure sfto_tsqsumf(u);
   % Standard form tools trivial square sum standard form. [u] is an
   % SF. Returns one of [nil], ['stsq], or ['tsq]. ['stsq] means that
   % in the sparse distributive representation of [u] all exponents
   % are even and all coefficients are positive. ['tsq] means that all
   % exponents are even and all coefficients are positive except for
   % that there is no absolute summand.
   if domainp u then
      (if null u then 'tsq else if not minusf u then 'stsq)
   else
      evenp ldeg u and sfto_tsqsumf lc u and sfto_tsqsumf red u;

procedure sfto_tsqsum!$(argl);
   sfto_tsqsumf(numr simp car argl);

procedure sfto_sqfdecf(u);
   % Standard form tools multivariate square-free decomposition
   % standard form. [u] is an SF. Returns a (dense) list $((q_1 .
   % 1),(q_2 . 2),...,(q_n . n))$ such that $\prod q_i^i = u$ with the
   % $q_i$ square-free and pairwise relatively prime. The (integer)
   % content of u is dropped. Decomposition is performed by merging
   % univariate decompositions. The univariate decomposition method is
   % selected by turning on one of the switches [sfto_yun] (default),
   % [sfto_tobey], or [sfto_musser].
   begin scalar c,pp;
      if domainp u then return {1 . 1};
      c := sfto_ucontentf u;
      pp := quotf(u,c);
      return sfto_sqdmerge(sfto_sqfdecf(c),sfto_usqfdecf(pp))
   end;

procedure sfto_sqfdec!$(argl);
   % Standard form tools square free decomposition. [argl] is an
   % argument list. Returns an AM list of AM lists of the form
   % $(p_i,m_i)$, where the $p_i$ are polynomials represented as a
   % Lisp-prefix-form and the $m_i$ are integers.
   begin scalar w;
      return 'list . for each x in sfto_sqfdecf numr simp car argl join
      	 if (w := prepf car x) neq 1 then {{'list,w,cdr x}}
   end;

procedure sfto_usqfdecf(u);
   if !*sfto_yun then
      sfto_yun!-usqfdecf u
   else if !*sfto_musser then
      sfto_musser!-usqfdecf u
   else if !*sfto_tobey then
      sfto_tobey!-usqfdecf u
   else
      rederr {"sfto_usqfdecf: select a decomposition method"};

procedure sfto_yun!-usqfdecf(p);
   % Standard form tools univariate square-free decomposition after
   % Yun. [p] is a an SF that is viewed a univariate Polynomial in its
   % [mvar] over the polynomial ring in all other variables; in this
   % sense, [p] must be primitive. Returns the square-free
   % decomposition of [p] as a (dense) list $((q_1 . 1),(q_2 .
   % 2),...,(q_n . n))$ such that $\prod q_i^i = u$ with the $q_i$
   % square-free and pairwise relatively prime.
   begin scalar !*gcd,x,g,c,d,w,l; integer n;
      !*gcd := T;
      x := mvar p;
      g := sfto_gcdf(p,w := diff(p,x));
      c := quotf(p,g);
      d := addf(quotf(w,g),negf(diff(c,x)));
      repeat <<
	 p := sfto_gcdf(c,d);
	 l := (p . (n := n+1)) . l;
	 c := quotf(c,p);
	 d := addf(quotf(d,p),negf(diff(c,x)))
      >> until domainp c;
      return reversip l
   end;

procedure sfto_musser!-usqfdecf(u);
   % Standard form tools univariate square-free decomposition after
   % Musser. [p] is a an SF that is viewed a univariate Polynomial in
   % its [mvar] over the polynomial ring in all other variables; in
   % this sense, [p] must be primitive. Returns the square-free
   % decomposition of [p] as a (dense) list $((q_1 . 1),(q_2 .
   % 2),...,(q_n . n))$ such that $\prod q_i^i = u$ with the $q_i$
   % square-free and pairwise relatively prime.
   begin scalar !*gcd,v,u1,sqfp,sqfp1,l; integer n;
      !*gcd := T;
      v := mvar u;
      u1 := sfto_gcdf(u,diff(u,v));
      sqfp := quotf(u,u1);
      while degr(u1,v)>0 do <<
	 sqfp1 := sfto_gcdf(sqfp,u1);
	 l := (quotf(sqfp,sqfp1) . (n := n+1)) . l;
	 u1 := quotf(u1,sqfp1);
	 sqfp := sqfp1
      >>;
      l := (sqfp . (n := n+1)) . l;
      return reversip l
   end;

procedure sfto_tobey!-usqfdecf(u);
   % Standard form tools univariate square-free decomposition after
   % Tobey and Horowitz. [p] is a an SF that is viewed a univariate
   % Polynomial in its [mvar] over the polynomial ring in all other
   % variables; in this sense, [p] must be primitive. Returns the
   % square-free decomposition of [p] as a (dense) list $((q_1 .
   % 1),(q_2 . 2),...,(q_n . n))$ such that $\prod q_i^i = u$ with the
   % $q_i$ square-free and pairwise relatively prime.
   begin scalar !*gcd,v,h,q1,q2,l; integer n;
      !*gcd := T;
      v := mvar u;
      h := sfto_gcdf(u,diff(u,v));
      q2 := quotf(u,h);
      while degr(u,v)>0 do <<
	 u := h;
	 q1 := q2;
	 h := sfto_gcdf(u,diff(u,v));
	 q2 := quotf(u,h);
	 l := (quotf(q1,q2) . (n := n+1)) . l
      >>;
      return reversip l
   end;

procedure sfto_sqdmerge(l1,l2);
   % Standard form tools square-free decomposition merge
   begin scalar l;
      l := l1;
      while l1 and l2 do <<
	 caar l1 := multf(caar l1,caar l2);
      	 l1 := cdr l1;
	 l2 := cdr l2
      >>;
      if l2 then l := nconc(l,l2);
      return l
   end;

procedure sfto_pdecf(u);
   % Standard form tools multivariate parity decomposition. [u] is an
   % SF. Returns a consed pair $a . d$ such that $a$ is the product of
   % all square-free factors with an odd multiplicity in [u] and $d$
   % is that of the even multiplicity square-free factors. The
   % (integer) content of u is dropped. Decomposition is performed by
   % merging univariate decompositions. The univariate decomposition
   % method is selected by turning on one of the switches [sfto_yun]
   % (default), [sfto_musser].
   begin scalar c,dpdc,dpdpp;
      if domainp u then return 1 . 1;
      c := sfto_ucontentf u;
      dpdc := sfto_pdecf c;
      dpdpp := sfto_updecf quotf(u,c);
      return multf(car dpdc,car dpdpp) . multf(cdr dpdc,cdr dpdpp)
   end;

procedure sfto_updecf(u);
   if !*sfto_yun then
      sfto_yun!-updecf u
   else if !*sfto_musser then
      sfto_musser!-updecf u
   else
      rederr {"sfto_updecf: select a decomposition method"};

procedure sfto_yun!-updecf(p);
   begin scalar !*gcd,x,g,c,d,w,l,od;
      !*gcd := T;
      l := 1 . 1;
      x := mvar p;
      g := sfto_gcdf(p,w := diff(p,x));
      c := quotf(p,g);
      d := addf(quotf(w,g),negf(diff(c,x)));
      repeat <<
	 od := not od;
	 p := sfto_gcdf(c,d);
	 if od then car l := multf(car l,p) else cdr l := multf(cdr l,p);
	 c := quotf(c,p);
	 d := addf(quotf(d,p),negf(diff(c,x)))
      >> until domainp c;
      return l
   end;

procedure sfto_musser!-updecf(u);
   begin scalar !*gcd,od,v,u1,sqfp,sqfp1,l;
      !*gcd := T;
      od := T;
      l := 1 . 1;
      v := mvar u;
      u1 := sfto_gcdf(u,diff(u,v));
      sqfp := quotf(u,u1);
      while degr(u1,v)>0 do <<
	 sqfp1 := sfto_gcdf(sqfp,u1);
	 if od then
 	    car l := multf(car l,quotf(sqfp,sqfp1))
	 else
	    cdr l := multf(cdr l,quotf(sqfp,sqfp1));
	 u1 := quotf(u1,sqfp1);
	 sqfp := sqfp1;
	 od := not od
      >>;
      if od then
	 car l := multf(car l,sqfp)
      else
	 cdr l := multf(cdr l,sqfp);
      return l
   end;

procedure sfto_pdec!$(argl);
   {'list,prepf car w,prepf cdr w}
      where w=sfto_pdecf numr simp car argl;

procedure sfto_decdegf(u,k,n);
   % Standard form tools decrement degree standard form. [u] is an SF;
   % [k] is a variable; [n] is an integer. Returns an SF. Replace each
   % occurence of $[k]^d$ by $k^(d/n)$.
   reorder sfto_decdegf1(sfto_reorder(u,k),k,n);

procedure sfto_decdegf1(u,k,n);
   % Standard form tools decrement degree standard form. [u] is an SF
   % with main variable [k]; [k] is a variable; [n] is an integer.
   % Returns an SF. Replace each occurence of $[k]^d$ by $k^(d/n)$.
   if degr(u,k)=0 then
      u
   else
      mvar u .** (ldeg u / n) .* lc u .+ sfto_decdegf1(red u,k,n);

procedure sfto_reorder(u,v);
   % Standard form tools reorder. [u] is an SF; [v] is a kernel.
   % Returns the SF [u] reorderd wrt. [{v}].
   begin scalar w;
      w := setkorder {v};
      u := reorder u;
      setkorder w;
      return u
   end;

procedure sfto_groebnerf(l);
   % Standard form tools Groebner calculation standard form. [l] is a
   % list of SF's. Returns a list of SF's. The returned list is the
   % reduced Groebner basis of [l] wrt. the current term order.
   begin scalar w;
      if null l then return nil;
      w := groebnereval {'list . for each sf in l collect prepf sf};
      return for each x in cdr w collect
	 numr simp x
   end;

procedure sfto_preducef(f,gl);
   % Standard form tools polynomial reduction standard form. [f] is an
   % SF and [gl] a list of SF's. Returns the SF [f] reduced wrt. [gl].
   if null gl then
      f
   else if (null cdr gl) and (domainp car gl) then
      nil
   else
      numr simp preduceeval {
	 prepf f,'list . for each sf in gl collect prepf sf};

procedure sfto_greducef(f,gl);
   % Standard form tools polynomial reduction standard form. [f] is an
   % SF and [gl] a list of SF's. Returns the SF [f] reduced wrt. a
   % Groebner basis of [gl].
   if null gl then
      f
   else if (null cdr gl) and (domainp car gl) then
      nil
   else
      numr simp greduceeval {
	 prepf f,'list . for each sf in gl collect prepf sf};

procedure sfto_gcdf!*(f,g);
   % Standard form tools greatest common divisor of standard forms.
   % [f] and [g] are SF's. Returns an SF, the GCD of [f] and [g].
   % Compute the GCD of [f] and [g] via [gcdf!*] or [ezgcdf] according
   % to Davenport's criterion: If, in one polynomial, the number of
   % variables of a degree greater than 2 is greater than 1, then use
   % [ezgcd].
   sfto_gcdf(f,g) where !*gcd=T;

procedure sfto_gcdf(f,g);
   % Standard form tools greatest common divisor of standard forms.
   % [f] and [g] are SF's. Returns an SF, the GCD of [f] and [g].
   % Compute the GCD of [f] and [g] via [gcdf!*] or [ezgcdf] according
   % to Davenport's criterion: If, in one polynomial, the number of
   % variables of a degree greater than 2 is greater than 1, then use
   % [ezgcd]. For computing the real gcd of [f] ang [g] this
   % procedures require, that [!*gcd] is set to [T].
   if null !*rldavgcd then
      gcdf(f,g)
   else if sfto_davp(f,nil) or sfto_davp(g,nil) then
      gcdf(f,g) where !*ezgcd=nil
   else
      ezgcdf(f,g);

procedure sfto_davp(f,badv);
   % Standard form tools Davenport predicate. [f] is an SF; [v] is a
   % kernel or [nil]. Returns Boolean. [T] means [gcdf] can be used.
   if domainp f then
      T
   else if ldeg f > 2 then
      if badv and mvar f neq badv then
	 nil
      else
	 sfto_davp(lc f,mvar f) and sfto_davp(red f,mvar f)
   else
      sfto_davp(lc f,badv) and sfto_davp(red f,badv);

procedure sfto_sqrtf(f);
   % Standard form tools square root standard form. Returns [nil] or
   % an SF $g$, such that $g**2=[f]$.
   begin scalar a,c,w,sd,result;
      c := sfto_dcontentf(f);
      result := fix sqrt c;
      if result**2 neq c then
	 return nil;
      sd := sfto_sqfdecf(f);
      w := sd;
      while sd do <<
	 a := car sd;
      	 sd := cdr sd;
	 if not(evenp cdr a) and car a neq 1 then <<
 	    sd := nil;
	    a := 'break
	 >> else
	    result := multf(result,exptf(car a,cdr a / 2 ))
      >>;
      if a neq 'break and exptf(result,2) = f then
	 return result
   end;

procedure sfto_monfp(sf);
   % Standard form tools monomial predicate. [f] is an SF. Returns an
   % SF. Check if [sf] is of the form $a x_1 \dots x_n$ for a domain
   % element $a$ and kernels $x_i$.
   domainp sf or (null red sf and sfto_monfp lc sf);

procedure sfto_sqfpartz(z);
   % Standard form tools square free part of an integer. [z] is an
   % integer with prime decomposition $p_1^{e_1}\cdots p_n^{e_n}$.
   % Returns $\prod \{p_i\}$.
   sfto_zdgen(z,0);

procedure sfto_zdeqn(z,n);
   % Standard form tools z decomposition equal n. [z] is an integer
   % with prime decomposition $p_1^{e_1}\cdots p_n^{e_n}$; [n] is a
   % positive integer. Returns $\prod \{p_i:e_i=n\}$.
   for each x in zfactor z product
      if cdr x = n then car x else 1;

procedure sfto_zdgtn(z,n);
   % Standard form tools z decomposition greater than n. [z] is an
   % integer with prime decomposition $p_1^{e_1}\cdots p_n^{e_n}$; [n]
   % is a positive integer. Returns $\prod \{p_i:e_i>n\}$.
   for each x in zfactor z product
      if cdr x > n then car x else 1;

procedure sfto_zdgen(z,n);
   % Standard form tools z decomposition greater than or equal to n.
   % [z] is an integer with prime decomposition $p_1^{e_1}\cdots
   % p_n^{e_n}$; [n] is a positive integer. Returns $\prod
   % \{p_i:e_i\geq n\}$.
   for each x in zfactor z product
      if cdr x >= n then car x else 1;

endmodule;  % [sfto]

end;  % of file
