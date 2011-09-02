% ----------------------------------------------------------------------
% $Id: ofsfqe.red,v 1.33 2003/02/03 09:57:53 seidl Exp $
% ----------------------------------------------------------------------
% Copyright (c) 1995-1999 Andreas Dolzmann and Thomas Sturm
% ----------------------------------------------------------------------
% $Log: ofsfqe.red,v $
% Revision 1.33  2003/02/03 09:57:53  seidl
% Modified ofsf_fbqe to new format of ofsf_cad.
%
% Revision 1.32  2003/01/31 17:10:21  sturm
% Removed unused scalar w from procedure ofsf_sqsc.
%
% Revision 1.31  2003/01/31 17:05:44  sturm
% Verbosity output also for ofsf_qea.
%
% Revision 1.30  2003/01/31 15:31:54  sturm
% Worked on verbosity output of QE routines.
%
% Revision 1.29  2003/01/27 11:49:34  sturm
% Introduced ofsf_fbqe, s.t. fallbackqe is compatible with new ofsf_cad,
% which has got a second argument now.
%
% Revision 1.28  2003/01/14 16:07:50  dolzmann
% Added switch rlxopt.
%
% Revision 1.27  2003/01/13 10:00:13  dolzmann
% Added definitions for ofsfxopt. Changed services qe and qea.
%
% Revision 1.26  2002/08/23 12:32:20  dolzmann
% Added local quantifier elimination.
%
% Revision 1.25  2002/08/23 08:44:41  dolzmann
% Minor code cosmetic.
%
% Revision 1.24  2002/01/25 12:12:39  sturm
% Modified ofsf_decdeg0 to return the performed variable substitutions
% as its cdr. Thus it can be used as an entry point from inside ofsf
% if the user wants to create verbosity output.
%
% Revision 1.23  1999/03/23 07:41:28  dolzmann
% Changed copyright information.
% Changed comments for exc.
%
% Revision 1.22  1999/03/21 13:37:38  dolzmann
% Changed in procedure ofsf_thregen '(false) into {'false}.
% Fixed a bug in ofsf_thregen: ofsf_thregen returned an atomic formula
% instead of a list of atomic formulas for an disjunctive f.
% Corrected comments.
%
% Revision 1.21  1999/03/18 14:08:21  sturm
% Added new service rl_specelim!* in cl_qe for covering the "super
% quadratic special case' for ofsf. This method is toggled by switch
% rlsqsc, which is off by default. Context dvfsf uses cl_specelim which
% is constantly "false." Context acfsf does not use cl_qe at all.
%
% Revision 1.20  1999/01/17 16:10:35  dolzmann
% Added and corrected comments.
%
% Revision 1.19  1998/04/09 11:00:04  sturm
% Added switch rlqeqsc for quadratic special case. This now OFF by default!
%
% Revision 1.18  1997/10/02 09:14:13  sturm
% Fixed a bug in answer computation with shift.
%
% Revision 1.17  1997/08/14 10:10:31  sturm
% Renamed rldecdeg to rldecdeg1.
% Added service rldecdeg.
%
% Revision 1.16  1997/06/27 13:04:51  sturm
% Fixed a bug in ofsf_decdeg1.
%
% Revision 1.15  1997/04/15 11:31:44  dolzmann
% New procedure ofsf_decdeg offers a symbolic mode interface for
% decrementing the degree of variables in formulas.
% Modified procedure ofsf_transform accordingly.
% ofsf_subsimpl now outputs an exclamation mark if it enlarges the
% theory.
%
% Revision 1.14  1997/04/08 14:31:12  sturm
% Sort the answer substitution list wrt. ordp of the right hand side kernels.
%
% Revision 1.13  1996/10/23 11:24:16  dolzmann
% Added and corrected comments.
% Moved procedure ofsf_mkstrict into module ofsf.
%
% Revision 1.12  1996/10/15 15:47:21  dolzmann
% Fixed a bug in ofsf_qefsolset.
%
% Revision 1.11  1996/10/08 13:54:35  dolzmann
% Renamed "degree parity decomposition" to "parity decomposition".
% Adapted names of procedures and switches accordingly.
%
% Revision 1.10  1996/10/07 12:03:30  sturm
% Added fluids for CVS and copyright information.
%
% Revision 1.9  1996/09/30 16:53:54  sturm
% Fixed a bug in ofsf_gelimset.
% Cleaned up the use of several (conditional) negate-relation procedures.
%
% Revision 1.8  1996/09/05 11:15:56  dolzmann
% Added comments.
% Minor changes in ofsf_mksol21q and ofsf_elimsetscq. New handling of
% root expressions with c=1.
% Renamed procedure ofsf_translat1lin to ofsf_translatlin.
% Renamed procedure ofsf_translat1qua to ofsf_translatqua.
% Completely rewritten Gauss elimination code: removed procedures
% ofsf_trygauss, ofsf_findeqsol, and ofsf_bettergaussp. Added
% implementation for black boxes rl_qefsolset, rl_bettergaussp!*,
% rl_bestgaussp, and rl_esetunion.
% Introduced new switch !*rlqegenct and related code.
%
% Revision 1.7  1996/07/07 14:43:06  sturm
% Removed use of fluid zehn!*.
% Call cl_nnfnot instead of cl_nnf1.
% Fixed a bug in ofsf_gelimset.
%
% Revision 1.6  1996/06/07 08:49:54  sturm
% Fixed bugs in ofsf_translat, ofsf_gelimset, and ofsf_decdegat.
%
% Revision 1.5  1996/05/13 13:45:24  dolzmann
% Improved ordering between the several kinds of Gauss elimination.
%
% Revision 1.4  1996/05/12 14:54:27  dolzmann
% Check for occurrence of variable in substitution.
% Modified ofsf_transform: Optimized treatment of atomic formulas x^n*r R 0.
%
% Revision 1.3  1996/05/12 08:27:33  sturm
% Added code for generic branch computation.
% Changes in ofsf_trygauss: Introduced an ordering between the several
% kinds of Gauss elimination.
% Added code for service ofsf_thsimpl.
%
% Revision 1.2  1996/04/18 14:30:47  sturm
% Improved root substitution in procedure ofsf_qesubrord1.
% Fixed a bug in ofsf_getsubrcoeffs.
%
% Revision 1.1  1996/03/22 12:14:14  sturm
% Moved and split.
%
% ----------------------------------------------------------------------
lisp <<
   fluid '(ofsf_qe_rcsid!* ofsf_qe_copyright!*);
   ofsf_qe_rcsid!* := "$Id: ofsfqe.red,v 1.33 2003/02/03 09:57:53 seidl Exp $";
   ofsf_qe_copyright!* := "Copyright (c) 1995-1999 by A. Dolzmann and T. Sturm"
>>;

module ofsfqe;
% Ordered field standard form quantifier elimination. Submodule of [ofsf].

%DS
% <variable> ::= <kernel>

procedure ofsf_qe(f,theo);
   if !*rlxopt and null theo and ofsf_xopt!-check f then <<
      if !*rlverbose then
	 ioto_tprin2t "++++ Entering xopt-qe";
      ofsf_xopt!-qe f
   >> else <<
      if !*rlverbose then
	 ioto_tprin2t "++++ Entering cl_qe";
      cl_qe(f,theo)
   >>;

procedure ofsf_qea(f,theo);
   if !*rlxopt and null theo and ofsf_xopt!-check f then <<
      if !*rlverbose then
	 ioto_tprin2t "++++ Entering xopt-qea";
      ofsf_xopt!-qea f
   >> else <<
      if !*rlverbose then
	 ioto_tprin2t "++++ Entering cl_qea";
      cl_qea(f,theo)
   >>;

procedure ofsf_varsel(f,vl,theo);
   % Ordered field standard form variable selection. [vl] is a list
   % of variables; [f] is a quantifier-free formula; [theo] is the
   % current theory. Returns a variable.
   begin scalar v,a,scvl,atl,ifacl,terml;
      atl := cl_atl1 f;
      scvl := vl;
      while scvl and not v do <<
	 a := car scvl;
	 scvl := cdr scvl;
	 if ofsf_linp(atl,a,delq(a,vl)) then v := a
      >>;
      if v then return v;
      scvl := vl;
      while scvl and not v do <<
	 a := car scvl;
	 scvl := cdr scvl;
	 if ofsf_qscp(atl,a) then v := a
      >>;
      if v then return v;
      terml := for each x in atl collect ofsf_arg2l x;
      scvl := vl;
      while scvl and not v do <<
	 a := car scvl;
	 scvl := cdr scvl;
	 if ofsf_pseudp(terml,a,1) then v := a
      >>;
      if v then return v;
      scvl := vl;
      while scvl and not v do <<
	 a := car scvl;
	 scvl := cdr scvl;
	 if ofsf_pseudp(terml,a,2) then v := a
      >>;
      if v then return v;
      if !*rlverbose then ioto_prin2 "(SVF";
      ifacl := for each x in atl join
	 for each p in cdr fctrf ofsf_arg2l x collect car x;
      if !*rlverbose then ioto_prin2 ")";
      scvl := vl;
      while scvl and not v do <<
	 a := car scvl;
	 scvl := cdr scvl;
	 if ofsf_pseudp(ifacl,a,1) then v := a
      >>;
      if v then return v;
      scvl := vl;
      while scvl and not v do <<
	 a := car scvl;
	 scvl := cdr scvl;
	 if ofsf_pseudp(ifacl,a,2) then v := a
      >>;
      if v then return v;
      return car vl
   end;

procedure ofsf_linp(atl,v,vl);
   % Ordered field standard form linear formula predicate. [atl] is a
   % list of atomic formulas; [v] is a variable; [vl] is a list of
   % variables. Returns [T] if every formula containing the atomic
   % formulas from [atl] is linear in [v] wrt. to [vl], i.e. the total
   % degree of [v] is 1 and no coefficient from [v] contains variables
   % from [vl].
   begin scalar linp,w,u,g;
      linp := T;
      w := setkorder {v};
      while atl and linp do <<
	 u := reorder ofsf_arg2l car atl;
	 atl := cdr atl;
	 g := degr(u,v);
	 if g > 1 or (g = 1 and intersection(kernels lc u,vl)) then
	    linp := nil
      >>;
      setkorder w;
      return linp
   end;

procedure ofsf_qscp(atl,v);
   % Ordered field standard form quadratic special case predicate.
   % [atl] is a list of atomic formulas; [v] is a variable. Returns
   % [T] if the quadratic special case is applicable to each formula
   % containing the atomic formulas from [atl].
   begin scalar a,hit,d;
      if not !*rlqeqsc then
	 return nil;
      while atl do <<
	 a := car atl;
	 atl := cdr atl;
	 d := degreef(ofsf_arg2l a,v);
	 if d>2 then
	    atl := hit := nil
	 else if d=2 and ofsf_op a memq '(greaterp lessp geq leq neq) then
	    if hit then
	       atl := hit := nil
	    else
	       hit := T
      >>;
      return hit
   end;

procedure ofsf_pseudp(ifacl,v,n);
   % Ordered field standard form pseudo high degree predicate.
   % [ifacl] is a list of SF's; [v] is a variable; [n] is a
   % non-negative integer. Returns [T] if the degree of each SF in
   % [ifacl] wrt. [v] is less than or equal to [n].
   begin scalar ok;
      ok := T;
      while ifacl and ok do
	 if degreef(car ifacl,v) > n then
	    ok := nil
	 else
	    ifacl := cdr ifacl;
      return ok
   end;

%DS root expression
% A list $(a,b,c,d)$ of SF's encoding the expression $(a+b\sqrt{c})/d$
% The denominator of a root expression $r=(a,b,c,d)$ is $d$ and the
% disciminante of $r$ is $c$. A root expression $r$ is called valid
% iff the demominator of $r$ is not equal to zero and the
% discriminante of $r$ is greater then 0.

procedure ofsf_qesubcr1(bvl,theo,f,v,co,u);
   % Ordered field standard form quantifier elimination substitute
   % conditionally 1 root. [bvl] is a list of variables; [theo] is the
   % current theory; [f] is a quantifier-free formula; [v] is a
   % variable; [u] is a root expression; [co] is a quantifier-free
   % formula which implies that [u] is valid. Returns a pair $(\Theta'
   % . \phi)$ where $\Theta'$ is a theory and $\phi$ is a
   % quantifier-free formula. $\phi$ is equivalent to $[co] \land
   % [f]([v]/[u])$ under the theory $[th] \cup \Theta'$.
   begin scalar w;
      w := ofsf_subsimpl(bvl,co,theo);
      if cdr w eq 'false then
      	 return car w . 'false;
      return car w . rl_mkn('and,{cdr w,ofsf_qesubr(f,v,u)})
   end;

procedure ofsf_qesubcr2(bvl,theo,f,v,co,u1,u2);
   % Ordered field standard form quantifier elimination substitute
   % conditionally 1 root. [bvl] is a list of variables; [theo] is the
   % current theory; [f] is a quantifier-free formula; [v] is a
   % variable; [u1], [u2] are root expression; [co] is a
   % quantifier-free formula which implies that both [u1] and [u2] are
   % valid. Returns a pair $(\Theta' . \phi)$ where $\Theta'$ is a
   % theory and $\phi$ is a quantifier-free formula. $\phi$ is
   % equivalent to $[co] \land ([f]([v]/[u1]) \lor [f]([v]/[u2]))$
   % under the theory $[th] \cup \Theta'$.
   begin scalar w;
      w := ofsf_subsimpl(bvl,co,theo);
      if cdr w eq 'false then
      	 return car w . 'false;
      return car w . rl_mkn('and,{cdr w,rl_mkn('or,{
      	 ofsf_qesubr(f,v,u1),ofsf_qesubr(f,v,u2)})})
   end;

procedure ofsf_qesubr(f,v,u);
   % Ordered field standard form quantifier elimination substitute
   % root. [f] is a quantifier-free formula; [v] is a variable; [u] is
   % a root expression. Returns a quantifier-free formula equivalent
   % to $[f]([v]/[u])$ provided that [u] is valid..
   if caddr u = 1 then
      cl_apply2ats1(f,'ofsf_qesubqat,{v,
	 quotsq(!*f2q addf(car u,cadr u),!*f2q cadddr u)})
   else
      cl_apply2ats1(f,'ofsf_qesubrat,{v,u});

procedure ofsf_qesubrat(atf,v,u);
   % Ordered field standard form quantifier elimination substitute
   % root into atomic formula. [atf] is an atomic formula; [v] is a
   % variable; [u] is a root expression. Returns a quantifier-free
   % formula equivalent to $[f]([v]/[u])$ provided that that [u] is
   % valid.
   if not (v memq ofsf_varlat atf) then
      atf
   else
      ofsf_qesubrat1(ofsf_op atf,ofsf_arg2l atf,v,u);

procedure ofsf_qesubrat1(r,f,x,rform);
   % Ordered field standard form quantifier elimination substitute
   % root into atomic formula subroutine. [r] is a relation; [f] is an
   % SF; [x] is a variable; [r] is a root expression. Returns a
   % quantifier-free formula equivalent to $[r]([f],0)([x]/[rform])$
   % that does not contain any root provided [rform] is valid.
   begin scalar w,dd;
      w := ofsf_getsubrcoeffs(f,x,rform);
      if r eq 'equal or r eq 'neq then
 	 return ofsf_qesubreq(r,car w,cadr w,caddr w);
      dd := car sfto_pdecf cadddr w;
      return ofsf_qesubrord(r,car w,cadr w,caddr w,dd)
   end;

procedure ofsf_qesubreq(r,aa,bb,c);
   % Ordered field standard form quantifier elimination substitute
   % root with equality relation. [r] is one of ['equal], ['neq]; [aa],
   % [bb], and [c] are SF's. Returns a quantifier-free formula
   % equivalent to $[r](([aa]+[bb]\sqrt{[c]})/d,0)$ for any nonzero
   % $d$ provided that $c \geq 0$.
   (if r eq 'equal then w else cl_nnfnot w)
      where w=ofsf_qesubreq1(aa,bb,c);

procedure ofsf_qesubreq1(aa,bb,c);
   % Ordered field standard form quantifier elimination substitute
   % root with equation. [aa], [bb], and [c] are SF's. Returns a
   % quantifier-free formula equivalent to $([aa]+[bb]\sqrt{[c]})/d=0$
   % for any nonzero $d$ provided that $c \geq 0$.
   if null bb then
      ofsf_0mk2('equal,aa)
   else
      rl_mkn('and,{ofsf_0mk2('leq,multf(aa,bb)),
	 ofsf_0mk2('equal,addf(exptf(aa,2),negf multf(exptf(bb,2),c)))});

procedure ofsf_qesubrord(r,aa,bb,c,dd);
   % Ordered field standard form quantifier elimination substitute
   % root with ordering relation. [r] is any ordering relation;
   % [delta] is $0$ or $1$; [aa], [bb], [c], and [dd] are SF's.
   % Returns a quantifier-free formula equivalent to
   % $[r](([aa]+[bb]\sqrt{[c]})/d^[delta],0)$ provided that $d \neq 0$
   % and $c \geq 0$.
   if r eq 'leq or r eq 'lessp then
      ofsf_qesubrord1(r,aa,bb,c,dd)
   else  % [r eq 'geq or r eq 'greaterp]
      cl_nnfnot ofsf_qesubrord1(ofsf_lnegrel r,aa,bb,c,dd);

procedure ofsf_qesubrord1(r,aa,bb,c,dd);
   % Ordered field standard form quantifier elimination substitute
   % root with ordering relation subroutine. [r] is one of [leq],
   % [lessp]; [delta] is $0$ or $1$; [aa], [bb], [c], and [d] are
   % SF's. Returns a quantifier-free formula equivalent to
   % $[r](([aa]+[bb]\sqrt{[c]})/d^[delta],0)$ provided that $d \neq 0$
   % and $c \geq 0$.
   begin scalar ad,a2b2c,w;
      ad := multf(aa,dd);
      if null bb then
      	 return ofsf_0mk2(r,ad);
      a2b2c := addf(exptf(aa,2),negf multf(exptf(bb,2),c));
      w := if r eq 'leq then
	 ofsf_0mk2('leq,a2b2c)
      else
	 rl_mkn('or,{ofsf_0mk2('lessp,ad),ofsf_0mk2('lessp,a2b2c)});
      return rl_mkn('or,{
	 rl_mkn('and,{ofsf_0mk2(r,ad),ofsf_0mk2(ofsf_anegrel r,a2b2c)}),
	 rl_mkn('and,{ofsf_0mk2('leq,multf(bb,dd)),w})})
   end;

procedure ofsf_getsubrcoeffs(f,x,rform);
   % Ordered field standard form get coefficients for root
   % substitution. [f] is an SF; [x] is a variable; [rform] is a root
   % expression $(a,b,c,d)$. Returns a list $(a',b',c,d')$ of SF's
   % such that $a'+b'\sqrt{c}/d'$ is $[f]([x]/[rform])$ reduced to
   % lowest terms. We assume $d \neq 0$ and $c \geq 0$.
   begin scalar w,rpol,aa,bb,dd,a,b,c,d;
      a := prepf car rform;
      b := prepf cadr rform;
      c := caddr rform;
      d := prepf cadddr rform;
      rpol := {'quotient,{'plus,a,{'times,b,'ofsf_sqrt}},d};
      w := subf(f,{x . rpol});
      dd := denr w;
      w := sfto_reorder(numr w,'ofsf_sqrt);
      while not domainp w and mvar w eq 'ofsf_sqrt do <<
	 if evenp ldeg w then
	    aa := addf(aa,multf(reorder lc w,exptf(c,ldeg w / 2)))
	 else
	    bb := addf(bb,multf(reorder lc w,exptf(c,ldeg w / 2)));
	 w := red w
      >>;
      aa := addf(aa,reorder w);
      return {aa,bb,c,dd}
   end;

procedure ofsf_qesubcq(bvl,theo,f,v,co,u);
   % Ordered field standard form quantifier elimination substitute
   % conditionally 1 quotient. [bvl] is a list of variables, [theo] is
   % the current theory, [f] is a quantifier-free formula; [v] is a
   % variable; [co] is a formula which implies that the denominator of
   % [u] is nonzero; [u] is an SQ. Returns a pair $(\Theta' . \phi)$
   % where $\Theta'$ is a theory and $\phi$ is a quantifier-free
   % formula. $\phi$ is equivalent to $[co] \land [f]([v]/[u])$ under
   % the theory $[th] \cup \Theta'$.
   begin scalar w;
      w := ofsf_subsimpl(bvl,co,theo);
      if cdr w eq 'false then
      	 return car w . 'false;
      return car w . rl_mkn('and,{cdr w,ofsf_qesubq(f,v,u)})
   end;

procedure ofsf_qesubq(f,v,u);
   % Ordered field standard form quantifier elimination substitute
   % quotient. [f] is a quantifier-free formula; [v] is a variable;
   % [u] is an SQ. Returns a quantifier-free formula equivalent to
   % $[f]([v]/[u])$ provided that the denominator of [u] is nonzero.
   cl_apply2ats1(f,'ofsf_qesubqat,{v,u});

procedure ofsf_qesubqat(atf,v,u);
   % Ordered field standard form quantifier elimination substitute
   % quotient into atomic formula. [atf] is an atomic formula; [v] is
   % a variable; [u] is an SQ. Returns a quantifier-free formula
   % equivalent to $[atf]([v]/[u])$ provided that the denominator of
   % [u] is nonzero.
   begin scalar w,op;
      if not (v memq ofsf_varlat atf) then return atf;
      w := subf(ofsf_arg2l atf,{v . prepsq u});
      op := ofsf_op atf;
      if !*rlqelocal then
	 return ofsf_qesubqat!-local(op,w);
      w := if op eq 'equal or op eq 'neq then numr w else multf(numr w,denr w);
      return ofsf_0mk2(op,w)
   end;

procedure ofsf_qesubqat!-local(op,w);
   <<
      if op eq 'equal or op eq 'neq then
      	 ofsf_0mk2(op,numr w)
      else if ofsf_0mk2('greaterp,denr w) member cl_theo!* then
	 ofsf_0mk2(op,numr w)
      else if ofsf_0mk2('lessp,denr w) member cl_theo!* then
	 ofsf_0mk2(ofsf_anegrel op,numr w)
      else
      	 ofsf_0mk2(op,multf(numr w,denr w))
   >>;

procedure ofsf_qesubi(bvl,theo,f,v,inf);
   % Ordered field standard form quantifier elimination substitute
   % infinite element. [bvl] is a list of variables, [theo] is the
   % current theory; [f] is a quantifier-free formula; [v] is a
   % variable; [inf] is one of ['minf], ['pinf] which stand for
   % $-\infty$ and $\infty$ resp. Returns a pair $(\Theta' . \phi)$
   % where $\Theta'$ is a theory and $\phi$ is a quantifier-free
   % formula. $\phi$ is equivalent to $[f]([v]/[inf])$ under the
   % theory $[th] \cup \Theta'$. $\Theta' is currently always [nil].
   nil . cl_apply2ats1(f,'ofsf_qesubiat,{v,inf});

procedure ofsf_qesubiat(atf,v,inf);
   % Ordered field standard form quantifier elimination substitute
   % infinite element into atomic formula. [atf] is an atomic formula;
   % [v] is a variable; [inf] is one of ['minf], ['pinf] which stand for
   % $-\infty$ and $\infty$ resp. Returns a quantifier-free formula
   % equivalent to $[atf]([v]/[inf])$.
   begin scalar op,lhs;
      if not (v memq ofsf_varlat atf) then return atf;
      op := ofsf_op atf;
      lhs := ofsf_arg2l atf;
      if op eq 'equal or op eq 'neq then
	 return ofsf_qesubtranseq(op,lhs,v);
      % [op] is an ordering relation.
      return ofsf_qesubiord(op,lhs,v,inf)
   end;

procedure ofsf_qesubtranseq(op,lhs,v);
   % Ordered field standard form quantifier elimination substitute
   % transcendental element with equality relation. [op] is one of
   % ['equal], ['neq]; [lhs] is an SF; [v] is a variable. Returns a
   % quantifier-free formula equivalent to $[r]([lhs],0)([v]/\alpha)$
   % for any transcendental $\alpha$.
   if op eq 'equal then
      ofsf_qesubtransequal(lhs,v)
   else  % [op eq 'neq]
      cl_nnfnot ofsf_qesubtransequal(lhs,v);

procedure ofsf_qesubtransequal(lhs,v);
   % Ordered field standard form quantifier elimination substitute
   % transcendental element into equation. [lhs] is an SF; [v] is a
   % variable. Returns a quantifier-free formula equivalent to
   % $[lhs]([v]/\alpha)=0$ for any transcendental $\alpha$.
   ofsf_qesubtransequal1(sfto_reorder(lhs,v),v);

procedure ofsf_qesubtransequal1(lhs,v);
   % Ordered field standard form quantifier elimination substitute
   % transcendental element into equation. [lhs] is an SF reordered
   % wrt. [v]; [v] is a variable. Returns a quantifier-free formula
   % equivalent to $[lhs]([v]/\alpha)=0$ for any transcendental
   % $\alpha$.
   begin scalar cl;
      while not domainp lhs and mvar lhs eq v do <<
	 cl := ofsf_0mk2('equal,reorder lc lhs) . cl;
	 lhs := red lhs
      >>;
      cl := ofsf_0mk2('equal,reorder lhs) . cl;
      return rl_smkn('and,cl)
   end;

procedure ofsf_qesubiord(op,f,v,inf);
   % Ordered field standard form quantifier elimination substitute
   % infinite element with ordering relation. [op] is an ordering
   % relation. [f] is an SF; [v] is a variable; [inf] is one of
   % ['minf], ['pinf] which stand for $-\infty$ and $\infty$ resp.
   % Returns a quantifier-free formula equivalent to
   % $[op]([lhs]([v]/[inf]),0)$.
   ofsf_qesubiord1(op,sfto_reorder(f,v),v,inf);

procedure ofsf_qesubiord1(op,f,v,inf);
   % Ordered field standard form quantifier elimination substitute
   % infinite element with ordering relation subroutine. [op] is an
   % ordering relation. [f] is an SF, which is reordered wrt. [v]; [v]
   % is a variable; [inf] is one of ['minf], ['pinf] which stand for
   % $-\infty$ and $\infty$ resp. Returns a quantifier-free formula
   % equivalent to $[op]([lhs]([v]/[inf]),0)$.
   begin scalar an;
      if domainp f or mvar f neq v then
      	 return ofsf_0mk2(op,reorder f);
      an := if inf eq 'minf and not evenp ldeg f then
 	 negf reorder lc f
      else
 	 reorder lc f;
      % The use of [an] is correct in the equal case.   % Generic QE!
      return rl_mkn('or,{ofsf_0mk2(ofsf_mkstrict op,an),rl_mkn(
	 'and,{ofsf_0mk2('equal,an),ofsf_qesubiord1(op,red f,v,inf)})})
   end;

procedure ofsf_qesubcrpe1(bvl,theo,f,v,co,r);
   % Ordered field standard form quantifier elimination substitute
   % conditionally 1 root plus epsilon. [bvl] is a list of variables;
   % [theo] is the current theory; [f] is a quantifier-free formula;
   % [v] is a variable; [r] is a root expression; [co] is a formula
   % which implies that [r] is valid. Returns a pair $(\Theta' .
   % \phi)$ where $\Theta'$ is a theory and $\phi$ is a
   % quantifier-free formula. $\phi$ is equivalent to $[co] \land
   % [f]([v]/[r1]+\epsilon)$ under the theory $[th] \cup \Theta'$.
   begin scalar w;
      w := ofsf_subsimpl(bvl,co,theo);
      if cdr w eq 'false then
      	 return car w . 'false;
      return car w . rl_mkn('and,{cdr w,ofsf_qesubrpe(f,v,r)})
   end;

procedure ofsf_qesubcrme1(bvl,theo,f,v,co,r);
   % Ordered field standard form quantifier elimination substitute
   % conditionally 1 root minus epsilon. [bvl] is a list of variables;
   % [theo] is the current theory; [f] is a quantifier-free formula;
   % [v] is a variable; [r] is a root expression; [co] is a formula
   % which implies that [r] is valid. Returns a pair $(\Theta' .
   % \phi)$ where $\Theta'$ is a theory and $\phi$ is a
   % quantifier-free formula. $\phi$ is equivalent to $[co] \land
   % [f]([v]/[r1]-\epsilon)$ under the theory $[th] \cup \Theta'$.
   begin scalar w;
      w := ofsf_subsimpl(bvl,co,theo);
      if cdr w eq 'false then
      	 return car w . 'false;
      return car w . rl_mkn('and,{cdr w,ofsf_qesubrme(f,v,r)})
   end;

procedure ofsf_qesubcrpe2(bvl,theo,f,v,co,r1,r2);
   % Ordered field standard form quantifier elimination substitute
   % conditionally 2 roots plus epsilon. [bvl] is a list of variables;
   % [theo] is the current theory; [f] is a quantifier-free formula;
   % [v] is a variable; [r1] and [r2] are root expression; [co] is a
   % formula which implies that both [r1] and [r2] are valid. Returns
   % a pair $(\Theta' . \phi)$ where $\Theta'$ is a theory and $\phi$
   % is a quantifier-free formula. $\phi$ is equivalent to $[co] \land
   % ([f]([v]/[r1]+\epsilon) \lor [f]([v]/[r2]+\epsilon))$ under the
   % theory $[th] \cup \Theta'$.
   begin scalar w;
      w := ofsf_subsimpl(bvl,co,theo);
      if cdr w eq 'false then
      	 return car w . 'false;
      return car w . rl_mkn('and,{cdr w,rl_mkn('or,{
      	 ofsf_qesubrpe(f,v,r1),ofsf_qesubrpe(f,v,r2)})})
   end;

procedure ofsf_qesubcrme2(bvl,theo,f,v,co,r1,r2);
   % Ordered field standard form quantifier elimination substitute
   % conditionally 2 roots minus epsilon. [bvl] is a list of variables;
   % [theo] is the current theory; [f] is a quantifier-free formula;
   % [v] is a variable; [r1] and [r2] are root expression; [co] is a
   % formula which implies that both [r1] and [r2] are valid. Returns
   % a pair $(\Theta' . \phi)$ where $\Theta'$ is a theory and $\phi$
   % is a quantifier-free formula. $\phi$ is equivalent to $[co] \land
   % ([f]([v]/[r1]-\epsilon) \lor [f]([v]/[r2]-\epsilon))$ under the
   % theory $[th] \cup \Theta'$.
   begin scalar w;
      w := ofsf_subsimpl(bvl,co,theo);
      if cdr w eq 'false then
      	 return car w . 'false;
      return car w . rl_mkn('and,{cdr w,rl_mkn('or,{
      	 ofsf_qesubrme(f,v,r1),ofsf_qesubrme(f,v,r2)})})
   end;

procedure ofsf_qesubrpe(f,v,r);
   % Ordered field standard form quantifier elimination substitute
   % root plus epsilon. [f] is a quantifier-free formula; [v] is a
   % variable; [r] is a root expression- Returns a formula equivalent
   % to $[f]([v]/[r]+\epsilon)$ provided that [r] is valid.
   cl_apply2ats1(f,'ofsf_qesubpmeat,{v,r,'ofsf_qesubr,T});

procedure ofsf_qesubrme(f,v,r);
   % Ordered field standard form quantifier elimination substitute
   % root minus epsilon. [f] is a quantifier-free formula; [v] is a
   % variable; [r] is a root expression- Returns a formula equivalent
   % to $[f]([v]/[r]-\epsilon)$ provided that [r] is valid.
   cl_apply2ats1(f,'ofsf_qesubpmeat,{v,r,'ofsf_qesubr,nil});

procedure ofsf_qesubcqpe(bvl,theo,f,v,co,q);
   % Ordered field standard form quantifier elimination substitute
   % conditionally 1 quotient plus epsilon. [bvl] is a list of
   % variables, [theo] is the current theory, [f] is a quantifier-free
   % formula; [v] is a variable; [co] is a formula which implies that
   % the denominator of [q] is nonzero; [q] is an SQ. Returns a pair
   % $(\Theta' . \phi)$ where $\Theta'$ is a theory and $\phi$ is a
   % quantifier-free formula. $\phi$ is equivalent to $[co] \land
   % [f]([v]/[q]+\epsilon)$ under the theory $[th] \cup \Theta'$.
   begin scalar w;
      w := ofsf_subsimpl(bvl,co,theo);
      if cdr w eq 'false then
      	 return car w . 'false;
      return car w . rl_mkn('and,{cdr w,ofsf_qesubqpe(f,v,q)})
   end;

procedure ofsf_qesubcqme(bvl,theo,f,v,co,q);
   % Ordered field standard form quantifier elimination substitute
   % conditionally 1 quotient minus epsilon. [bvl] is a list of
   % variables, [theo] is the current theory, [f] is a quantifier-free
   % formula; [v] is a variable; [co] is a formula which implies that
   % the denominator of [q] is nonzero; [q] is an SQ. Returns a pair
   % $(\Theta' . \phi)$ where $\Theta'$ is a theory and $\phi$ is a
   % quantifier-free formula. $\phi$ is equivalent to $[co] \land
   % [f]([v]/[q]-\epsilon)$ under the theory $[th] \cup \Theta'$.
   begin scalar w;
      w := ofsf_subsimpl(bvl,co,theo);
      if cdr w eq 'false then
      	 return car w . 'false;
      return car w . rl_mkn('and,{cdr w,ofsf_qesubqme(f,v,q)})
   end;

procedure ofsf_qesubqpe(f,v,q);
   % Ordered field standard form quantifier elimination substitute
   % quotient plus epsilon. [f] is a quantifier-free formula; [v] is a
   % variable; [q] is an SQ. Returns a quantifier-free formula
   % equivalent to $[f]([v]/[q]+\epsilon)$ provided that the
   % denominator of [q] is nonzero.
   cl_apply2ats1(f,'ofsf_qesubpmeat,{v,q,'ofsf_qesubq,T});

procedure ofsf_qesubqme(f,v,q);
   % Ordered field standard form quantifier elimination substitute
   % quotient minus epsilon. [f] is a quantifier-free formula; [v] is a
   % variable; [q] is an SQ. Returns a quantifier-free formula
   % equivalent to $[f]([v]/[q]-\epsilon)$ provided that the
   % denominator of [q] is nonzero.
   cl_apply2ats1(f,'ofsf_qesubpmeat,{v,q,'ofsf_qesubq,nil});

procedure ofsf_qesubpmeat(atf,v,u,finsub,ple);
   % Ordered field standard form quantifier elimination substitute
   % plus/minus epsilon into atomic formula. [atf] is an atomic
   % formula; [v] is a variable; [u] is any field element;
   % [finsub(atf,v,u)] is a procedure that can substitute [u] into a
   % formula; [ple] is Boolean, non-[nil] means $+\epsilon$. Returns a
   % quantifier-free formula equivalent to $[atf]([v]/[u]\pm\epsilon)$
   % provided that the denominator of [u] is nonzero.
   begin scalar op,lhs;
      if not (v memq ofsf_varlat atf) then return atf;
      op := ofsf_op atf;
      lhs := ofsf_arg2l atf;
      if op eq 'equal or op eq 'neq then
	 return ofsf_qesubtranseq(op,lhs,v);
      % [op] is an ordering relation.
      return apply(finsub,{ofsf_qesubpmeord(op,lhs,v,ple),v,u})
   end;

procedure ofsf_qesubpmeord(op,f,v,ple);
   % Ordered field standard form quantifier elimination substitute
   % plus/minus epsilon with ordering relation. [op] is an ordering
   % relation. [f] is an SF; [v] is a variable; [ple] is Boolean,
   % non-[nil] means $+\epsilon$. Returns a quantifier-free formula
   % $\phi$ such that $\phi(v/u)$ is equivalent to
   % $[op]([f]([v]/u\pm\epsilon),0)$ for any field element $u$ with
   % nonzero denominator.
   if degreef(f,v) eq 0 then
      ofsf_0mk2(op,f)
   else
      rl_mkn('or,{ofsf_0mk2(ofsf_mkstrict op,f),rl_mkn('and,{
	 ofsf_0mk2('equal,f),ofsf_qesubpmeord(
	    op,if ple then diff(f,v) else negf diff(f,v),v,ple)})});

procedure ofsf_subsimpl(bvl,f,th);
   % Ordered field standard form substitution condition
   % simplification. [bvl] is a list of variables; [f] is a formula;
   % [th] is the current theory. Returns a pair $(\Theta'.\phi)$, such
   % that $phi$ is equivalent to [f] under the theory
   % $[th]\cup\Theta'$. All atomic formulas in $\Theta'$ contain only
   % terms [u] such that [ofsf_valassp(bvl,u)] holds.
   begin scalar nth;
      f := cl_simpl(f,th,-1);
      if not !*rlqegen then
	 return nil . f;
      nth := for each atf in cl_atl1 f join
	 if ofsf_op atf='equal and ofsf_valassp(bvl,ofsf_arg2l atf) then
	    {ofsf_0mk2('neq,ofsf_arg2l atf)};
      if nth then <<
	 ioto_prin2 "!";
      	 return nth . cl_simpl(f,append(nth,th),-1)
      >>;
      return nil . f
   end;

procedure ofsf_valassp(bvl,sf);
   % Ordered field standard form valid assumption. [bvl] is a list of
   % variables; [sf] is a standard form. Returns [T] if an assumption
   % containing [sf] is valid. Depends on switch [!*rlqegenct].
   (!*rlqegenct or sfto_monfp sf) and null intersection(bvl,kernels sf);

%DS ALP
% A pair of ALIST's encoding the set of possible elimination terms.

% Keys created by ofsf_translat1:
% equal1: linear equations
% equal21q: quadratic equations 1 quotient
% equal22r: quadratic equations 2 roots
% neq1: linear inequalities
% neq21q: quadratic inequalities 1 quotient
% neq22r: quadratic inequalities 2 roots
% geq1: linear weak lower bounds
% leq1: linear weak upper bounds
% greaterp1: linear strong lower bounds
% lessp1: linear strong upper bounds
% wo1: linear weak orderings
% wo21q: quadratic weak orderings 1 quotient
% wo22r: quadratic weak orderings 2 roots
% so1: linear strong orderings
% so21q: quadratic strong orderings 1 quotient
% so22r: quadratic strong orderings 2 roots

smacro procedure ofsf_mkalp(tag,l);
   % Ordered field standard form make alist pair. [tag] is a key; [l]
   % is an entry. Returns an ALP.
   {tag . l} . {tag . 1};

smacro procedure ofsf_ceterm1a(m,u);
   % Ordered field standard form conditional elimination term 1
   % condition atomic other parameter. [m] is a SF; [u] is an
   % elimination term.
   {ofsf_0mk2('neq,m),u};

smacro procedure ofsf_ceterm2a(a,m,u);
   % Ordered field standard form conditional elimination term 2
   % conditions atomic other parameter. [a], [m] are SF's; [u] is an
   % elimination term.
   if a then
      {rl_mkn('and,{ofsf_0mk2('equal,a),ofsf_0mk2('neq,m)}),u}
   else
      {ofsf_0mk2('neq,m),u};

smacro procedure ofsf_ceterm1l(a,l);
   % Ordered field standard form conditional elimination term 1
   % condition parameter list.
   ofsf_0mk2('neq,a) . l;

smacro procedure ofsf_ceterm2l(a,d,l);
   % Ordered field standard form conditional elimination term 2
   % conditions parameter list. [a], [d] are SF's; [l] is a list of
   % elimination terms.
   rl_mkn('and,{ofsf_0mk2('neq,a),ofsf_0mk2('geq,d)}) . l;

smacro procedure ofsf_mktag1(x);
   % Ordered field standard form make tag linear case. [x] is an
   % identifier. Returns the interned identifier [x]1.
   intern compress(nconc(explode x,'(!1)));

smacro procedure ofsf_mktag2(x,y);
   % Ordered field standard form make tag quadratic case. [x], [y] are
   % identifiers. Returns the interned identifier [x]2[y].
   intern compress(nconc(explode x,'!2 . explode y));

procedure ofsf_translat(atf,v,theo,pos,ans);
   % Ordered field standard form translate atomic formula. [atf] is an
   % atomic formula $\rho(t,0)$; [v] is a variable; [theo] is the
   % current theory; [pos], [ans] are Bool. Returns an ALP. If [pos]
   % is non-[nil] [atf] is consided as [not(atf)]. The switch [rlqesr]
   % is turned on if [ans] is non-[nil]. If [v] is not in $t$ the
   % result is $([nil] . [nil])$. Else $t$ is of the form $\prod_i
   % a_i[v]^2+b_i[v]+c_i$, and the result is $(((\rho' . (-b . a))) .
   % ((\rho' . 1)))$ where $\rho'=\rho$ for non-[nil] [pos] and the
   % negation of $\rho$ else.
   begin scalar svrlqesr,res;
      if ans then <<
      	 svrlqesr := !*rlqesr;
	 on1 'rlqesr
      >>;
      if v memq ofsf_varlat atf then <<
      	 res := if pos then
 	    ofsf_translat1(atf,v,theo)
      	 else
	    ofsf_translat1(ofsf_negateat atf,v,theo);
	 if res = '(nil . nil) then
	    res := {'anypoint . nil} . {'anypoint . 1}
      >> else
 	 res := nil . nil;
      if ans and null svrlqesr then
	 off1 'rlqesr;
      return res
   end;

procedure ofsf_translat1(atf,v,theo);
   % Ordered field standard form translate atomic formula subroutine.
   % [atf] is an atomic formula; [v] is a variable; [theo] is the
   % current theory. Returns an ALP or a pair of the key ['failed] and
   % an error message.
   begin scalar w,rel;
      w := ofsf_mktriplel(ofsf_arg2l atf,v);
      if car w eq 'failed then return w;
      rel := ofsf_op atf;
      if null car w then
	 return ofsf_translat2(rel,cadr w,theo);
      return cl_alpunion for each x in cdr w join
	 if rel memq '(geq leq lessp greaterp) then
	    {ofsf_translat2(rel,x,theo),
	       ofsf_translat2(ofsf_anegrel rel,x,theo)}
	 else
	    {ofsf_translat2(rel,x,theo)}
   end;

procedure ofsf_translat2(rel,trip,theo);
   % Ordered field standard form translate atomic formula subroutine.
   % [rel] is a relation, [trip] is a triple; [theo] is the current
   % theory. Returns an ALP.
   if null car trip then
      ofsf_translatlin(rel,cadr trip,caddr trip,theo,nil)
   else
      ofsf_translatqua(rel,car trip,cadr trip,caddr trip,theo);

procedure ofsf_translatlin(r,m,b,theo,xc);
   % Ordered field standard form translate atomic formula linear case.
   % [r] is a relation; [m], [b] are the 2nd and 3rd constituent of a
   % triple generated from a linear term; [theo] is the current
   % theory; [xc] is a SF encoding an extra condition. Returns an ALP.
   if !*rlqelocal and null setdiff(kernels m,cl_lps!*) then
      ofsf_translatlin!-local(r,m,b,theo,xc)
   else
      ofsf_mkalp(ofsf_tlltag(r,m,theo),{ofsf_ceterm2a(xc,m,ofsf_mksol1(m,b))});

procedure ofsf_translatlin!-local(r,m,b,theo,xc);
   begin scalar w;
      w := ofsf_tlltag!-local1(r,m,theo);
      if null car w then
	 return nil . nil;
      return ofsf_mkalp(cdr w,{ofsf_ceterm2a(xc,car w,ofsf_mksol1(m,b))})
   end;

procedure ofsf_tlltag(r,m,theo);
   % Ordered field standard form translate atomic formula linear case
   % make tag. [r] is a relation; [m] is the 2nd constituent of a
   % triple generated from a linear term; [theo] is the current
   % theory. Returns a tag.
   if r eq 'equal or r eq 'neq then
      ofsf_mktag1 r
   else if ofsf_surep(ofsf_0mk2('geq,m),theo) then
      ofsf_mktag1 r
   else if ofsf_surep(ofsf_0mk2('leq,m),theo) then
      ofsf_mktag1 ofsf_anegrel r
   else if !*rlqelocal and null setdiff(kernels m,cl_lps!*) then
      ofsf_tlltag!-local(r,m,theo)
   else if r eq 'lessp or r eq 'greaterp then
      'so1
   else % [r memq '(leq geq)]
      'wo1;

procedure ofsf_tlltag!-local(r,m,theo);
   cdr ofsf_tlltag!-local1(r,m,theo);

procedure ofsf_tlltag!-local1(r,m,theo);
   begin scalar w;
      w := numr subf(m,cl_pal!*);
      if null w then <<
	 cl_theo!* := ofsf_0mk2('equal,m) . cl_theo!*;
      	 return nil . ofsf_mktag1 r
      >>;
      if minusf w then <<
	 cl_theo!* := ofsf_0mk2('lessp,m) . cl_theo!*;
      	 return (-1) . ofsf_mktag1 ofsf_anegrel r
      >>;
      cl_theo!* := ofsf_0mk2('greaterp,m) . cl_theo!*;
      return 1 . ofsf_mktag1 r
   end;

procedure ofsf_translatqua(r,a,b,c,theo);
   % Ordered field standard form translate atomic formula subroutine
   % quadratic case. [r] is a relation; [a], [b], and [c] are the
   % constituent of a triple; [theo] is the current theory. Returns an
   % ALP.
   begin scalar w,tagbase,tag,eset;
      w := ofsf_mksol2(a,b,c);
      if w eq 'failed then
	 return nil . nil;
      tagbase := if r memq '(lessp greaterp) then
 	 'so
      else if r memq '(leq geq) then
 	 'wo
      else  % [if r memq '(equal neq) then]
 	 r;
      if car w eq 'onequot then <<
	 tag := ofsf_mktag2(tagbase,'!1q);
	 eset := {ofsf_ceterm1a(a,cdr w)}
      >> else if car w eq 'tworoot then <<
	 if !*rlqesr then <<
	    tag := ofsf_mktag2(tagbase,'!1r);
	    eset := {ofsf_ceterm2l(a,cadr w,{caddr w}),
	       ofsf_ceterm2l(a,cadr w,{cadddr w})}
	 >> else <<
	    tag := ofsf_mktag2(tagbase,'!2r);
	    eset := {ofsf_ceterm2l(a,cadr w,{caddr w,cadddr w})}
	 >>
      >>;
      if !*rlqelocal and null setdiff(kernels a,cl_lps!*) then <<
      	 w := numr subf(a,cl_pal!*);
	 if not null w then <<
	    cl_theo!* := ofsf_0mk2('neq,a) . cl_theo!*;
	    return ofsf_mkalp(tag,eset)
	 >>;
	 cl_theo!* := ofsf_0mk2('equal,a) . cl_theo!*;
	 return ofsf_translatlin(r,b,c,theo,a)
      >>;
      if not null b then <<
      	 w :=  ofsf_translatlin(r,b,c,theo,a);
      	 return {tag . eset,caar w} . {tag . 1,cadr w}
      >>;
      return ofsf_mkalp(tag,eset)
   end;

procedure ofsf_surep(f,theo);
   % Ordered field standard form sure predicat. [f] is a formula;
   % [theo] is a theory. Returns [T] if $f$ holds under the theory
   % [theo].
   cl_simpl(f,theo,-1) eq 'true;

procedure ofsf_mktriplel(u,v);
   % Ordered field standard form make triple list. [v] is a variable,
   % [u] is a SF containing [v]. Returns a pair $k . l$, where $k$ is
   % one off ['failed], ['fac], [nil] and $l$ is a list. If $k$ is
   % [nil], then the degree of [u] in [v] is less than or equal to 2,
   % if [k] is ['fac] then the degree of all irreducible factors of
   % [u] in [v] is less than or equal to 2, and if $k$ is ['failed]
   % then at least one factor of [u] has an degree greater than 2 in
   % [v]. If $k$ is not ['failed] then $l$ is the list of all triples
   % of the factors of [u]. If $k$ is ['failed] then $l$ encodes a
   % warning-message. Notice that if $k$ is [nil] the list $l$
   % contains only one element.
   begin scalar w,g,fl,a,ul;
      w := setkorder {v};
      u := reorder u;
      if ldeg u <= 2 then <<
	 setkorder w;
	 return nil . {ofsf_reotrip ofsf_mktriple u}
      >>;
      % Try to factorize.
      if !*rlverbose then ioto_prin2{"."};
      fl := cdr fctrf u;
      while fl do <<
	 a := car fl;
	 fl := cdr fl;
	 g := degr(car a,v);
	 if g > 2 then <<
	    ul := 'failed . {"degree of",v,"is",g,"in",prepf car a};
	    fl := nil
	 >> else if g > 0 then
 	    ul := car a . ul
      >>;
      setkorder w;
      if car ul = 'failed then return ul;
      return 'fac . for each x in ul collect ofsf_reotrip ofsf_mktriple x
   end;

procedure ofsf_mktriple(x);
   % Ordered field standard form make triple. [x] is a SF of the form
   % $a[v]^2+b[v]+c$, not necessarily in the current kernel order.
   % Returns the triple $(a,b,c)$.
   begin scalar a,v;
      v := mvar x;
      if ldeg x eq 2 then <<
      	 a := lc x;
      	 x := red x
      >>;
      return if not domainp x and mvar x eq v then
   	 {a,lc x,red x}
      else
   	 {a,nil,x}
   end;

procedure ofsf_reotrip(trip);
   % Orderd field standard form reorder triple. [trip] is a triple
   % $(a,b,c)$ of SF's. Returns the triple $(a',b',c')$ of SF's, where
   % $a'$, $b'$, and $c'$ are reorderd wrt. the current kernel order.
   {reorder car trip,reorder cadr trip,reorder caddr trip};

procedure ofsf_mksol1(m,b);
   % Orderd field standard form make solution linear case. [m] and [b]
   % are standard forms. Returns $-[b]/m$ as SQ.
   quotsq(!*f2q negf b,!*f2q m);

procedure ofsf_mksol2(a,b,c);
   % Orderd field standard form make solution quadratic case. [a],
   % [b], and [c] are SF's. Returns either ['failed] or a pair $(k .
   % f)$. $k$ is one of ['onequot], ['tworoot]. If $k$ is ['onequot]
   % then $[b]^2-4[a][c]=0$ and $f$ is the SQ $-[b]/2[a]$. If $k$ is
   % ['tworoot] then $f$ is a pair $(\delta . l)$ where $\delta$ is
   % the discriminante of $a x^2+b x+c$ and $l$ is a list of the two
   % root expressions coding $(-[b]\pm\sqrt{[b]^2-4[a][c]})/2[a]$.
   begin scalar disc,w,c,ww;
      disc := addf(exptf(b,2),negf multf(4,multf(a,c)));
      if domainp disc and minusf disc then
      	 return 'failed;
      a := multf(2,a);
      b := negf b;
      if null disc then
      	 return 'onequot . quotsq(!*f2q b,!*f2q a);
      if !*rlqelocal and null setdiff(kernels disc,cl_lps!*) then <<
	 ww := numr subf(disc,cl_pal!*);
	 if minusf ww then <<
	    cl_theo!* := ofsf_0mk2('lessp,disc) . cl_theo!*;
      	    return 'failed
	 >>;
	 if null ww then <<
      	    cl_theo!* := ofsf_0mk2('equal,disc) . cl_theo!*;
      	    return 'onequot . quotsq(!*f2q b,!*f2q a)
      	 >>
      >>;
      w := sfto_sqrtf disc;
      if w then
	 return 'tworoot . nil . ofsf_mksol21q(b,w,a);
      return 'tworoot . disc . ofsf_mksol21r(b,disc,a)
   end;

procedure ofsf_mksol21q(mb,discr,ta);
   % Orderd field standard form make solution quadratic case 1
   % quotient. [mb], [discr] and [ta] are SF's. Returns a list of the
   % two root expressions $([mb],\pm[discr],1,ta)$.
   {{mb,negf discr,1,ta},{mb,discr,1,ta}};

procedure ofsf_mksol21r(mb,disc,ta);
   % Orderd field standard form make solution quadratic case 1 root.
   % [mb], [disc] and [ta] are SF's. Returns a list of the two root
   % expressions $([mb],\pm1,[disc],ta)$.
   {{mb,-1,disc,ta},{mb,1,disc,ta}};

%DS elimination_set
% A list $(...,(p . (l_1,...,l_n)),...)$ where the $p$ are procedures
% and the $l_i$ are parameter lists $(l_{i1},...,l_{im})$ such that
% there is $p(f,v,l_{i1},...,l_{im})$ called for substitution, where
% $f$ is the considered formula, and $v$ the considered variable.

procedure ofsf_elimset(v,alp);
   % Ordered field standard form elimination set. [v] is a variable;
   % [alp] is a pair of alists. Returns an elimination set.
   begin scalar atfal,w,lpart,qpart,npart;
      atfal := car alp;
      if null cdr atfal and caar atfal = 'anypoint then
 	 return '((ofsf_qesubcq . ((true (nil . nil)))));
      % Treat some special cases.
      w := ofsf_elimsetscq(atfal);
      if w then <<
	 if !*rlverbose then ioto_prin2 "#q";
	 return w
      >>;
      w := ofsf_elimsetscl(atfal);
      if w then <<
	 if !*rlverbose then ioto_prin2 "#l";
	 return w
      >>;
      w := ofsf_elimsetlin1s(atfal);
      lpart := cdr w;
      qpart := ofsf_elimsetqua(atfal,car w);
      npart := ofsf_elimsetneq(atfal,car w);
      return lto_nconcn {lpart,qpart,npart}
   end;

procedure ofsf_elimsetscq(atfal);
   % Elimination set computation quadratic special case. [atfal] is an
   % alist. Returns an elimination set or [nil]. Check if there is
   % exactly one point coming from a quadratic non-equation. If so, we
   % test the zero of the corresponding derivative, $\pm \infty$, and
   % all linear upper and lower bounds. Equations and inequations are
   % treated as usual.
   begin scalar w,l,a,nzf,zero,d,dfzero,hl;
      if not !*rlqeqsc then
	 return nil;
      l := '(neq21q neq22r wo21q wo22r so21q so22r neq21r wo21r so21r);
      while l do <<
	 a := car l;
	 l := cdr l;
	 if (w := lto_catsoc(a,atfal)) then
	    if nzf or a memq '(neq21r wo21r so21r) and cddr w or
	       a memq '(neq21q neq22r wo21q wo22r so21q so22r) and cdr w
 	    then <<
	       l := nil;
	       a := 'failed
	    >> else <<
	       zero := car w;  % The only entry in w
	       nzf := car reversip explode a
	    >>
      >>;
      if a eq 'failed or null nzf then return nil;
      % Construct the zero of the derivative from [zero] which is a
      % zero of the polynomial itself.
      if nzf = 'q then   % bad, but not relevant with !*rlsipd on
      	 dfzero := zero
      else <<  % [nzf = 'r]
	 zero := cadr zero;  % first solution
      	 d := cadddr zero;
      	 dfzero := {ofsf_0mk2('neq,d),ofsf_mksol1(d,negf car zero)}
      >>;
      hl := {'ofsf_qesubcq . (dfzero . lto_catsoc('equal21q,atfal)),
	 'ofsf_qesubcr2 . lto_catsoc('equal22r,atfal),
	    '(ofsf_qesubi (pinf) (minf))};
      return lto_nconcn {hl,ofsf_elimsetlinbs(atfal),ofsf_elimsetneqbs(atfal)}
   end;

smacro procedure ofsf_setvlin();
   % Ordered field standard form set variables for elimination set
   % computation linear case.
   <<
      equal1 := lto_catsoc('equal1,atfal);
      leq1 := lto_catsoc('leq1,atfal);
      geq1 := lto_catsoc('geq1,atfal);
      greaterp1 := lto_catsoc('greaterp1,atfal);
      lessp1 := lto_catsoc('lessp1,atfal);
      wo1 := lto_catsoc('wo1,atfal);
      so1 := lto_catsoc('so1,atfal)
   >>;

procedure ofsf_elimsetlinbs(atfal);
   % Ordered field standard form elimination set linear case both
   % sides. [atfal] is an alist. Returns an elimination set.
   begin
      scalar equal1,leq1,geq1,greaterp1,lessp1,wo1,so1,qesubcql,
	 qesubcqmel,qesubcqpel;
      ofsf_setvlin();
      qesubcql := 'ofsf_qesubcq . lto_nconcn{equal1,leq1,geq1,wo1};
      qesubcqmel := 'ofsf_qesubcqme . lto_nconcn{so1,lessp1};
      qesubcqpel := 'ofsf_qesubcqpe . lto_nconcn{so1,greaterp1};
      return {qesubcql,qesubcqmel,qesubcqpel}
   end;

procedure ofsf_elimsetneqbs(atfal);
   % Elimination set [neq] test both sides.
   begin scalar neq1,neq21q,neq21r,neq22r;
      neq1 := lto_catsoc('neq1,atfal);
      neq21q := lto_catsoc('neq21q,atfal);
      neq22r := lto_catsoc('neq22r,atfal);
      neq21r := lto_catsoc('neq21r,atfal);
      return {'ofsf_qesubcqme . nconc(neq1,neq21q),'ofsf_qesubcrme2 . neq22r,
	 'ofsf_qesubcrme1 . neq21r,'ofsf_qesubcrpe1 . neq21r,
      	 'ofsf_qesubcqpe . nconc(neq1,neq21q),'ofsf_qesubcrpe2 . neq22r}
   end;

smacro procedure ofsf_setvscl();
   % Ordered field standard form set variables for elimination set
   % computation linear special case.
   <<
      equal1 := lto_catsoc('equal1,atfal);
      equal21q := lto_catsoc('equal21q,atfal);
      equal21r := lto_catsoc('equal21r,atfal);
      equal22r := lto_catsoc('equal22r,atfal);
      leq1 := lto_catsoc('leq1,atfal);
      geq1 := lto_catsoc('geq1,atfal);
      greaterp1 := lto_catsoc('greaterp1,atfal);
      lessp1 := lto_catsoc('lessp1,atfal);
      wo1 := lto_catsoc('wo1,atfal);
      so1 := lto_catsoc('so1,atfal);
      o2p := lto_catsoc('wo21q,atfal) or lto_catsoc('wo21r,atfal) or
	 lto_catsoc('wo22r,atfal) or lto_catsoc('so21q,atfal) or
 	 lto_catsoc('so21r,atfal) or lto_catsoc('so22r,atfal)
   >>;

procedure ofsf_elimsetscl(atfal);
   % Elimination set computation linear special case. [atfal] is an
   % alist. Returns an elimination set or [nil]. Computes an
   % elimination set for the following two special cases: (1) There is
   % no quadratic bound, the linear bounds there are either all upper
   % bounds or all lower bounds. Then the opposite inifinity can be
   % tested. The inequations can be ignored. (2) There is exactly one
   % bound, which is linear and parametric. Then $\pm \infty$ can be
   % tested. The inequations can be ignored. In both cases the
   % equations are treated as usual.
   begin
      scalar equal1,equal21q,equal21r,equal22r,leq1,geq1,greaterp1,lessp1,
	 o2p,nub,nlb,infsubl,wo1,so1;
      ofsf_setvscl();
      if o2p then return nil;  % Any quadratic bound
      nub := null (leq1 or lessp1);  % No concrete upper bound
      nlb := null (geq1 or greaterp1);  % No concrete lower bound
      if null (wo1 or so1) then  % No parametric bound
      	 (if nub then
	    infsubl := '(ofsf_qesubi . ((pinf)))
      	 else if nlb then
            infsubl := '(ofsf_qesubi . ((minf))))
      else if nub and nlb and
      	 (null wo1 and null cdr so1 or null so1 and null cdr wo1)
      then  % Exactly one bound, which is linear and parametric.
	 infsubl := '(ofsf_qesubi . ((pinf) (minf)));
      if infsubl then
	 return {infsubl,'ofsf_qesubcr1 . equal21r,
	    'ofsf_qesubcq . nconc(equal1,equal21q),'ofsf_qesubcr2 . equal22r}
   end;

procedure ofsf_elimsetlin1s(atfal);
   % Ordered field standard form elimination set linear part decide
   % for one side. [atfal] is an alist. Returns a pair $a . d$ where
   % $d$ is an elimination set, and $a$ is one of [T], [nil] which
   % means we have decided to test lower bounds or upper bound resp.
   begin
      scalar equal1,leq1,geq1,greaterp1,lessp1,wo1,so1,qesubcql,qesubil,esubl;
      integer l1n,g1n;
      ofsf_setvlin();
      l1n := length leq1 + length lessp1;
      g1n := length geq1 + length greaterp1;
      if l1n <= g1n then <<
      	 qesubcql := 'ofsf_qesubcq . lto_nconcn{equal1,leq1,wo1};
	 esubl := 'ofsf_qesubcqme . nconc(so1,lessp1);
	 qesubil := '(ofsf_qesubi . ((pinf)));
	 return nil . {qesubcql,esubl,qesubil}
      >>;
      qesubcql := 'ofsf_qesubcq . lto_nconcn{equal1,geq1,wo1};
      esubl := 'ofsf_qesubcqpe . nconc(so1,greaterp1);
      qesubil := '(ofsf_qesubi . ((minf)));
      return T . {qesubcql,esubl,qesubil}
   end;

procedure ofsf_elimsetqua(atfal,ple);
   % Ordered field standard form elimination set quadratic part.
   % [atfal] is an alist; [ple] is bool where [T] means we have
   % decided for lower bounds in the linear part. Returns an
   % elimination set.
   begin
      scalar equal21q,equal22r,wo21q,wo22r,so21q,so22r,qesubcql,qesubcr1l,
	 qesubcr2l,esubcql,esubcr1l,esubcr2l,equal21r,wo21r,so21r;
      equal21q := lto_catsoc('equal21q,atfal);
      equal21r := lto_catsoc('equal21r,atfal);
      equal22r := lto_catsoc('equal22r,atfal);
      wo21q := lto_catsoc('wo21q,atfal);
      wo21r := lto_catsoc('wo21r,atfal);
      wo22r := lto_catsoc('wo22r,atfal);
      so21q := lto_catsoc('so21q,atfal);
      so21r := lto_catsoc('so21r,atfal);
      so22r := lto_catsoc('so22r,atfal);
      if ple then <<
	 esubcql := 'ofsf_qesubcqpe . so21q;
	 esubcr1l := 'ofsf_qesubcrpe1 . so21r;
	 esubcr2l := 'ofsf_qesubcrpe2 . so22r
      >> else <<
	 esubcql := 'ofsf_qesubcqme . so21q;
	 esubcr1l := 'ofsf_qesubcrme1 . so21r;
	 esubcr2l := 'ofsf_qesubcrme2 . so22r
      >>;
      qesubcql := 'ofsf_qesubcq . nconc(equal21q,wo21q);
      qesubcr1l := 'ofsf_qesubcr1 . nconc(equal21r,wo21r);
      qesubcr2l := 'ofsf_qesubcr2 . nconc(equal22r,wo22r);
      return {qesubcql,qesubcr1l,qesubcr2l,esubcql,esubcr1l,esubcr2l}
   end;

smacro procedure ofsf_setvneq();
   % Ordered field standard form set variables for elimination set
   % computation [neq] treatment.
   <<
      neq1 := lto_catsoc('neq1,atfal);
      neq21q := lto_catsoc('neq21q,atfal);
      neq21r := lto_catsoc('neq21r,atfal);
      neq22r := lto_catsoc('neq22r,atfal);
      leq1 := lto_catsoc('leq1,atfal);
      geq1 := lto_catsoc('geq1,atfal);
      wo1 := lto_catsoc('wo1,atfal);
      wo21q := lto_catsoc('wo21q,atfal);
      wo21r := lto_catsoc('wo21r,atfal);
      wo22r := lto_catsoc('wo22r,atfal)
   >>;

procedure ofsf_elimsetneq(atfal,ple);
   % Ordered field standard form elimination set treatment of ['neq].
   % [atfal] is an alist; [ple] is bool where [T] means we have
   % decided for lower bounds in the linear part. Returns an
   % elimination set.
   begin
      scalar neq1,neq21q,neq21r,neq22r,leq1,geq1,wo1,wo21q,wo21r,wo22r,
	 neqn,wbn,esubcq,esubcr1,esubcr2,wb1;
      ofsf_setvneq();
      neqn := length neq1 + length neq21q + length neq21r + 2*(length neq22r);
      if neqn = 0 then return nil;
      wbn := length wo1 + length wo21q + length wo21r +
 	 2*(length wo22r);  % + ...
      if ple then <<
	 esubcq := 'ofsf_qesubcqpe;
	 esubcr1 := 'ofsf_qesubcrpe1;
	 esubcr2 := 'ofsf_qesubcrpe2;
	 wb1 := geq1;
	 wbn := wbn + length geq1
      >> else <<
	 esubcq := 'ofsf_qesubcqme;
	 esubcr1 := 'ofsf_qesubcrme1;
	 esubcr2 := 'ofsf_qesubcrme2;
	 wb1 := leq1;
	 wbn := wbn + length leq1
      >>;
      if neqn < wbn then
	 return {esubcq .
 	    nconc(neq1,neq21q),esubcr1 . neq21r,esubcr2 . neq22r};
      if !*rlverbose then ioto_prin2 {"(ANEQ:",neqn,"|",wbn,")"};
      return {esubcq . lto_nconcn{wb1,wo1,wo21q},esubcr1 . wo21r,
	 esubcr2 . wo22r}
   end;

procedure ofsf_bettergaussp(grv1,grv2);
   % Ordered field standard form better Gauss predicate. [grv1] and
   % [grv2] are GRV's. Returns [T] if [grv1] encodes a better Gauss
   % application than [grv2] encodes.
   begin scalar w1,w2;
      if car grv1 eq 'failed then
      	 return nil;
      if car grv2 eq 'failed then
      	 return T;
      w1 := cadar grv1;
      w2 := cadar grv2;
      if w1 neq w2 then
      	 return (w1 memq cdr (w2 memq '(fac quar qua2q quaq lin)));
      w1 := caddar grv1;
      w2 := caddar grv2;
      if w1 neq w2 then
      	 return w1 memq cdr (w2 memq '(gen td con));
      w1 := ofsf_esetlength cadr grv1;
      w2 := ofsf_esetlength cadr grv2;
      if w1 neq w2 then
      	 return w1 < w2;
      w1 := caddar grv1;
      w2 := caddar grv2;
%      if w1 neq w2 then
      return w1 memq cdr (w2 memq '(gen td con));
   end;

procedure ofsf_esetlength(e);
   % Ordered field standard form elimination set length. [e] is an
   % elimination set. Returns the number of elimination terms in [e].
   for each p in e sum
      for each x in p sum
	 length cdr p;

procedure ofsf_esetunion(e1,e2);
   % Ordered field standard form elimination set union. [e1] and [e2]
   % are elimination sets. Returns the union of [e1] and [e2].
   lto_alunion({e1,e2});

procedure ofsf_bestgaussp(grv);
   % Ordered field standard form best Gauss predicate. [grv] is a GRV.
   % Returns [T] if the Gauss application encoded in GRV is the best
   % Gauss application under all possible Gauss applications.
   not(car grv eq 'failed) and not(car grv eq 'gignore) and
      cadar grv eq 'lin and caddar grv eq 'con and   % Linear, concrete coeff.
      null cdr cadr grv and null cddar cadr grv;     % Only one elim. term

procedure ofsf_qefsolset(a,v,theo,ans,bvl);
   % Ordered field standard form quantifier elimination finite
   % solution set. [a] is an atomic formula; [v] is a variable; [theo]
   % is the current theory; [ans] is Boolean; [bvl] is a list of
   % variables. Returns an IGRV.
   begin scalar w;
      if ofsf_op a neq 'equal then
	 return '(failed . nil);
      w := ofsf_varlat a;
      if v memq w then
      	 return  ofsf_findeqsol(a,v,theo,ans,bvl);
      if !*rlqegen and ofsf_valassp(bvl,ofsf_arg2l a) then
      	 return ('gignore . (nil . {ofsf_0mk2('neq,ofsf_arg2l a)}));
      return '(failed . nil);
   end;

procedure ofsf_findeqsol(a,v,theo,ans,bvl);
   % Ordered field standard form find solution of non-trivial equation
   % subroutine. [a] is an atomic formula; [v] is a variable; [theo]
   % is a list of atomic formulas, the current theory; [ans] is
   % Boolean; [bvl] is a list of variables that are considered
   % non-parametric. Returns $[failed] . [nil]$ or a form $(\tau . (e
   % . \theta))$ where $\tau$ is an identifier tag encoding the degree
   % of the Gauss application, [e] is an elimination set, and $\theta$
   % is the new theory. If [!*rlqegen] is off, we know
   % $\theta'=[nil]$.
   begin scalar w,d,theop,tag;
      w := ofsf_pnontrivial(ofsf_arg2l a,v,theo,bvl);
      tag := car w;
      if not tag then
      	 return '(failed . nil);
      if cdr w then
      	 theop := {cdr w};
      d := degreef(ofsf_arg2l a,v);
      w := ofsf_gelimset ofsf_translat(a,v,theo,T,ans);
      if w eq 'failed then return '(failed . nil);
      return ofsf_mkgtag(d,tag,w,theo) . (w . theop)
   end;

procedure ofsf_mkgtag(d,tag,eset,theo);
   % Ordered field standard form make Gauss tag. [d] is positive
   % integer; [tag] is an identifier; [eset] is an elimination set;
   % [theo] is the current theory.
   begin scalar w,v;
      w := if d=1 then 'lin else if d=2 then ofsf_mkgtagq(eset,theo) else 'fac;
      v := if d=1 then v := "l" . v else if d=2 then v := "q" . v;
      if tag eq 'gen then v := "!" . v;
      return {v,w,tag}
   end;

procedure ofsf_mkgtagq(eset,theo);
   % Ordered field standard form make Gauss tag quadratic case. [eset]
   % is an elimination set; [theo] is the current theory.
   begin scalar a;
      if null cdr eset and caar eset eq 'ofsf_qesubcq then
 	 return 'quaq;
      a := atsoc('ofsf_qesubcr2,eset) or atsoc('ofsf_qesubcr1,eset);
      % We know [a neq nil].
      if null cadr cadr cadr a then  % $b$ of the first root expression.
   	 return 'qua2q;
      return 'quar
   end;

procedure ofsf_gelimset(alp);
   % Gauss elimination set. [alp] is a pair of alists obtained from
   % [ofsf_translat]. Returns an elimination set.
   begin scalar eset;
      eset := car alp;
      if eset = 'failed then return 'failed;
      if null cdr eset and caar eset = 'anypoint then
 	 return {'ofsf_qesubcq . {'(true (nil . nil))}};
      for each x in eset do
	 if car x memq '(equal1 equal21q) then
 	    car x := 'ofsf_qesubcq
	 else if car x = 'equal21r then
	    car x := 'ofsf_qesubcr1
	 else if car x = 'equal22r then
	    car x := 'ofsf_qesubcr2
	 else
	    rederr "BUG IN ofsf_gelimset";
      return eset
   end;

procedure ofsf_pnontrivial(u,v,theo,bvl);
   % Possibly non-trivial. [u] is an SF; [v] is a variable; [theo] is
   % a list of atomic formulas, the current theory; [bvl] is a list of
   % variables that are considered non-parametric. Returns a pair $p .
   % \theta'$ where $\theta'$ is an inequation or [nil], and $p$ is
   % non-[nil] iff one of the coefficients of [u] wrt. [v] may be
   % assumed nonzero under the assumption $[theo] \cup \{\theta'\}$.
   % If [!*rlqegen] is off, we know $\theta'=[nil]$.
   begin scalar vcoeffs;
      vcoeffs := for each x in coeffs sfto_reorder(u,v) collect reorder x;
      return ofsf_maybenonzerol(vcoeffs,theo,bvl)
   end;

procedure ofsf_maybenonzerol(l,theo,bvl);
   % Maybe not a list of zero SF's. [l] is a list of SF's; [theo] is a
   % list of atomic formulas, the current theory; [bvl] is a list of
   % variables that are considered non-parametric. Returns a pair $p .
   % \theta'$ where $\theta'$ is an inequation or [nil], and $p$ is
   % non-[nil] iff one of the elements of [l] may be assumed nonzero under
   % the assumption $[theo] \cup \{\theta'\}$. If [!*rlqegen] is
   % off, we know $\theta'=[nil]$.
   begin scalar w,result;
      result := '(nil . nil);
      while l do <<
	 w := ofsf_maybenonzero(car l,theo,bvl);
	 l := cdr l;
	 if car w then <<
	    result := w;
	    l := nil
      	 >>
      >>;
      return result
   end;

procedure ofsf_maybenonzero(u,theo,bvl);
   % Maybe a non-zero SF's. [u] is an SF's; [theo] is a list of atomic
   % formulas, the current theory; [bvl] is a list of variables that
   % are considered non-parametric. Returns a pair $p . \theta'$ where
   % $\theta'$ is an inequation or [nil], and $p$ is non-[nil] iff [u] may
   % be assumed nonzero under the assumption $[theo] \cup
   % \{\theta'\}$. If [!*rlqegen] is off, we know $\theta'=[nil]$.
   if domainp u then
      if null u then
      	 '(nil . nil)
      else
	 '(con . nil)   % con = concrete
   else if cl_simpl(ofsf_0mk2('equal,u),theo,-1) eq 'false then
      '(td . nil)   % td = theory derived
   else if !*rlqelocal and null setdiff(kernels u,cl_lps!*) then
      ofsf_maybenonzero!-local(u,theo,bvl)
   else if !*rlqegen and ofsf_valassp(bvl,u) then
      'gen . ofsf_0mk2('neq,u)  % gen = generic
   else
      '(nil . nil);

procedure ofsf_maybenonzero!-local(u,theo,bvl);
   begin scalar w;
      w := numr subf(u,cl_pal!*);
      if null w then
	 return '(nil . nil);
      cl_theo!* := ofsf_0mk2('neq,u) . cl_theo!*;
      return 'gen . ofsf_0mk2('neq,u)
   end;

procedure ofsf_qemkans(an,atr);
   sort(
      ofsf_qeapplyatr ofsf_qebacksub ofsf_qemkans1 an,
      function(lambda(x,y); ordp(cadr x,cadr y)));

procedure ofsf_qemkans1(an);
   % Ordered field standard form quantifier elimination make answer
   % subroutine. [an] is an answer. Returns a list $((e,a),...)$,
   % where $e$ is an equation and $a$ is an answer translation.
   begin scalar v,sub,xargl,w,atr;
      return for each y in an collect <<
	 v := car y;
	 sub := cadr y;
	 xargl := caddr y;
	 atr := cadddr y;
	 w := if sub eq 'ofsf_qesubi then <<
	    atr := nil;
      	    (if car xargl = 'pinf then
	       'infinity
      	    else if car xargl = 'minf then
	       '(minus infinity))
	 >> else if sub eq 'ofsf_qesubcq then
	    prepsq cadr xargl
	 else if sub eq 'ofsf_qesubcr1 then
	    ofsf_preprexpr(cadr xargl)
	 else if sub eq 'ofsf_qesubcqme then
	    {'difference,prepsq cadr xargl,'epsilon}
	 else if sub eq 'ofsf_qesubcqpe then
	    {'plus,prepsq cadr xargl,'epsilon}
	 else if sub eq 'ofsf_qesubcrme1 then
	    {'difference,ofsf_preprexpr(cadr xargl),'epsilon}
	 else if sub eq 'ofsf_qesubcrpe1 then
	    {'plus,ofsf_preprexpr(cadr xargl),'epsilon}
	 else
	    rederr "BUG IN ofsf_qemkans";
	 {{'equal,v,w},atr}
      >>
   end;

procedure ofsf_qebacksub(eql);
   % Quantifier elimination back substitution. [eql] is a list
   % $((e,a),...)$, where $e$ is an equation and $a$ is an answer
   % translation. Returns a list of the same form.
   begin scalar subl,rhs,ioe,atr,e; integer ic,ec;
      return for each w in eql collect <<
	 e := car w;
	 atr := cadr w;
	 rhs := simp caddr e;
	 if smemq('infinity,rhs) then <<
	    ic := ic+1;
	    ioe := mkid('infinity,ic);
	    rhs := subsq(rhs,{'infinity . ioe})
	 >>;
	 if smemq('epsilon,rhs) then <<
	    ec := ec+1;
	    ioe := mkid('epsilon,ec);
	    flag({ioe},'constant);
	    put(ioe,'!:rd!:,'rdzero!*);
	    rhs := subsq(rhs,{'epsilon . ioe})
	 >>;
	 e := {'equal,cadr e,prepsq subsq(rhs,subl)};
	 subl := (cadr e . caddr e) . subl;
	 {e,atr}
      >>
   end;

procedure ofsf_qeapplyatr(eql);
   % Ordered field standard form quantifier elimination apply answer
   % translation. [eql] is a list $((e,a),...)$, where $e$ is an
   % equation and $a$ is an answer translation. Returns a list of
   % equations.
   begin scalar rhs,atr,pow,eqn;
      return for each w in eql collect <<
	 eqn := car w;
	 rhs := caddr eqn;
	 atr := cadr w;
	 if null atr then
	    eqn
	 else <<
	    pow := lto_catsoc(cadr eqn,atr) or 1;
	    {'equal,cadr eqn,ofsf_croot(rhs,pow)}
      	 >>
      >>
   end;

procedure ofsf_croot(u,n);
   if null n then u else reval {'expt,u,{'quotient,1,n}};

procedure ofsf_preprexpr(r);
   {'quotient,{'plus,prepf car r,{'times,prepf cadr r,{'sqrt,prepf caddr r}}},
      prepf cadddr r};

procedure ofsf_decdeg(f);
   % Ordered field standard form decrease degrees. [f] is a formula.
   % Returns a formula equivalent to [f], hopefully decreasing the
   % degrees of the bound variables.
   car ofsf_decdeg0 cl_rename!-vars f;

procedure ofsf_decdeg0(f);
   begin scalar op,w,gamma,newmat,dvl,nargl;
      op := rl_op f;
      if rl_boolp op then <<
	 nargl := for each subf in rl_argn f collect <<
   	    w := ofsf_decdeg0 subf;
	    dvl := nconc(dvl,cdr w);
	    car w
	 >>;
	 return rl_mkn(op,nargl) . dvl
      >>;
      if rl_quap op then <<
	 w := ofsf_decdeg0 rl_mat f;
	 dvl := cdr w;
	 w := ofsf_decdeg1(car w,{rl_var f});
	 dvl := nconc(dvl,cdr w);
	 newmat := if null cdr w or not evenp cdr car cdr w then
	    car w
	 else <<
	    gamma := ofsf_0mk2('geq,numr simp car car cdr w);
	    rl_mkn(if op eq 'ex then 'and else 'impl,{gamma,car w})
	 >>;
	 return rl_mkq(op,rl_var f,newmat) . dvl
      >>;
      % [f] is not complex.
      return f . nil
   end;

procedure ofsf_decdeg1(f,vl);
   % Ordered field standard form decremet degrees. [f] is a formula;
   % [vl] is a list of variables $v$ such that $v$ does not occur
   % boundly in [f], or ['fvarl]. Returns a pair $(\phi . l)$; $\phi$
   % is a formula, and $l$ is a list of pairs $(v . d)$ where $v$ in
   % [vl] and $d$ is an integer. We have $\exists [vl] [f]$ equivalent
   % to $\exists [vl] (\phi \land \bigwedge_{(v . d) \in [vl]}(v^d
   % \geq 0))$, where $\phi$ is obtained from [f] by substituting $v$
   % for $v^d$ for each $(v . d)$ in $l$. ['fvarl] stands for the list
   % of all free variables in [f].
   begin scalar dvl; integer n;
      if vl eq 'fvarl then
	 vl := cl_fvarl1 f;
      for each v in vl do <<
	 n := ofsf_decdeg2(f,v);
	 if n>1 then <<
	    f := ofsf_decdeg3(f,v,n);
	    dvl := (v . n) . dvl
	 >>
      >>;
      return f . dvl
   end;

procedure ofsf_decdeg2(f,v);
   % Ordered field standard form decrement degree subroutine. [f] is a
   % formula; [v] is a variable. Returns an INTEGER $n$. The degree of [v]
   % in [f] can be decremented using the substitution $[v]^n=v$.
   begin scalar a,w,atl,dgcd,!*gcd,oddp;
      !*gcd := T;
      atl := cl_atl1 f;
      dgcd := 0;
      while atl and dgcd neq 1 do <<
	 a := car atl;
	 atl := cdr atl;
	 w := ofsf_ignshift(a,v);
	 if w eq 'odd and null oddp then
	    oddp := 'odd
	 else if null w then <<
	    a := sfto_reorder(ofsf_arg2l a,v);
	    while (not domainp a) and (mvar a eq v) and dgcd neq 1 do <<
	       dgcd := gcdf(dgcd,ldeg a);
	       a := red a
	    >>
      	 >>;
	 if dgcd > 0 and oddp eq 'odd then <<
	    oddp := T;
	    while w := quotf(dgcd,2) do
	       dgcd := w
	 >>
      >>;
      if dgcd = 0 then
	 return 1;
      return dgcd
   end;

procedure ofsf_transform(f,v);
   % Ordered field standard form transform formula. [f] is a
   % quantifier-free formula; [v] is a variable. Returns a pair $(\phi
   % . a)$. $\phi$ is a formula such that $\exists [v]([f])$ is
   % equivalent to $\exists [v](\phi)$. $a$ is either [nil] or a pair
   % $([v] . d)$. If $a$ is not [nil] then the degree $d'$ of [v] in
   % [f] is reduced to $d'/d$. If $a$ is nil then $[f]=\phi$.
   begin scalar dgcd;
      dgcd := ofsf_decdeg2(f,v);
      if dgcd = 1 then
	 return f . nil;
      if !*rlverbose then ioto_prin2 {"(",v,"^",dgcd,")"};
      f := ofsf_decdeg3(f,v,dgcd);
      if evenp dgcd then
	 f := rl_mkn('and,{ofsf_0mk2('geq,numr simp v),f});
      return f . (v . dgcd)
   end;

procedure ofsf_ignshift(at,v);
   % Orderd field standard form ignore shift. [at] is an atomic
   % formula; [v] is a variable. Returns [nil], ['ignore], or ['odd].
   begin scalar w;
      w := sfto_reorder(ofsf_arg2l at,v);
      if not domainp w and null red w and mvar w eq v then
	 if ofsf_op at memq '(equal neq) or evenp ldeg w then
	    return 'ignore
	 else
	    return 'odd
   end;

procedure ofsf_decdeg3(f,v,n);
   % Ordered field standard form decrement degree. [f] is a formula;
   % [v] is a variable; [n] is an integer. Returns a formula.
   cl_apply2ats1(f,'ofsf_decdegat,{v,n});

procedure ofsf_decdegat(atf,v,n);
   % Ordered field standard form decrement degree atomic formula. [f]
   % is an atomic formula; [v] is a variable; [n] is an integer. Returns
   % an atomic formula.
   if ofsf_ignshift(atf,v) then
      atf
   else
      ofsf_0mk2(ofsf_op atf,sfto_decdegf(ofsf_arg2l atf,v,n));

procedure ofsf_updatr(atr,upd);
   % Ordered field standard form update answer translation. [atr] is
   % an answer translation; [upd] is information which has to be added
   % to [atr]. Returns an answer translation.
   upd . atr;

procedure ofsf_thsimpl(atl);
   % Ordered field standard form theory simplification. [atl] is a
   % theory. Returns an equivalent theory. The returned theory is
   % hopefully somehow simpler than the original one.
   begin scalar !*rlsiexpla,!*rlsipo;
      !*rlsiexpla := T;
      return sort(ofsf_thregen cl_simpl(rl_smkn('and,atl),nil,-1),'rl_ordatp)
   end;

procedure ofsf_thregen(f);
   % Ordered field standard form re-generate theory. [f] is a formula.
   % Returns a possibly empty list of atomic formulas equivalent to
   % [f] or the list [{'false}] if [f] is recognized as a
   % contradiction.
   begin scalar op;
      op := rl_op f;
      if op = 'and then
 	 return for each x in rl_argn f collect ofsf_thregen!-or x;
      if op = 'or then
	 return {ofsf_thregen!-or f};
      if op = 'true then
	 return nil;
      if op = 'false then
	 {'false};
      % [f] is atomic.
      return {f}
   end;

procedure ofsf_thregen!-and(f);
   % Ordered field standard form re-generate theory conjunction case.
   % [f] is a conjunction. Returns an atomic formula equivalent to
   % [f].
   cl_nnfnot ofsf_thregen!-or cl_nnfnot f;

procedure ofsf_thregen!-or(f);
   % Ordered field standard form re-generate theory disjunction case.
   % [f] is a disjunction. Returns an atomic formula equivalent to
   % [f].
   begin scalar w;
      if cl_atfp f then
	 return f;
      w := car rl_argn f;
      if rl_op w = 'and then
	 w := ofsf_thregen!-and w;
      if rl_op w = 'equal then
      	 return ofsf_thregen!-equal(w . cdr rl_argn f);
      if rl_op w = 'neq then
      	 return ofsf_thregen!-neq(w . cdr rl_argn f);
      rederr "BUG IN ofsf_thregen!-or"
   end;

procedure ofsf_thregen!-equal(eql);
   % Ordered field standard form re-generate theory equality
   % disjunction case. [eql] is a list of equations or complex
   % formulas which can be contracted to one equation. The list is
   % considered disjunctive. Returns an atomic formula equivalent to
   % $\bigvee [eql]$ constructed by multiplication of the left hand
   % sides.
   begin scalar w;
      w := 1;
      for each x in eql do <<
	 if rl_op x = 'and then
	    x := ofsf_thregen!-and x;
	 if rl_op x neq 'equal then
            rederr "BUG IN ofsf_thregen!-equal";
	 w := multf(w,ofsf_arg2l x)
      >>;
      return ofsf_0mk2('equal,w)
   end;

procedure ofsf_thregen!-neq(neql);
   % Ordered field standard form re-generate theory [neq] disjunction
   % case. [neql] is a list of inequalities or complex formulas which
   % can be contracted to one inequality. The list is considered
   % disjunctive. Returns an atomic formula equivalent to $\bigvee
   % [neql]$ constructed by addition of the squares of the left hand
   % sides.
   begin scalar w;
      for each x in neql do <<
	 if rl_op x = 'and then
	    x := ofsf_thregen!-and x;
	 if rl_op x neq 'neq then
            rederr "BUG IN ofsf_thregen!-neq";
	 w := addf(w,exptf(ofsf_arg2l x,2))
      >>;
      return ofsf_0mk2('neq,w)
   end;

procedure ofsf_specelim(f,vl,theo,ans,bvl);
   % Ordered field standard form special elimination.
   if (not !*rlqesqsc) or ans or !*rlqegen then
      'failed
   else
      ofsf_sqsc(f,vl,theo,ans,bvl);

procedure ofsf_sqsc(f,vl,theo,ans,bvl);
   % Ordered field standard form super quadratic special case.
   begin scalar atl,scvl,lin,a,at;
      atl := cl_atl1 f;
      scvl := if !*rlqevarsel then vl else {car vl};
      while scvl and not lin do <<
	 a := car scvl;
	 scvl := cdr scvl;
	 lin := ofsf_linp(atl,a,delq(a,vl))
      >>;
      if lin then
	 return 'failed;
      scvl := if !*rlqevarsel then vl else {car vl};
      while scvl and not at do <<
	 a := car scvl;
	 scvl := cdr scvl;
	 at := ofsf_sqsc!-test(atl,a)
      >>;
      if not at then
 	 return 'failed;
      if !*rlverbose then
	 ioto_prin2 "#Q";
      vl := delq(a,vl);
      f := cl_simpl(ofsf_sqsc1(f,at,a,theo),theo,-1);
      return (t . {cl_mkcoel(vl,f,nil,nil)}) . theo
   end;

procedure ofsf_sqsc1(f,at,v,theo);
   if cl_cxfp f then
      rl_mkn(rl_op f,for each x in rl_argn f collect ofsf_sqsc1(x,at,v,theo))
   else if f eq at then
      ofsf_sqsc1at(at,v,theo)
   else
      f;

procedure ofsf_sqsc1at(at,v,theo);
   begin scalar op,w,a,b,c,discr;
      op := ofsf_op at;
      w := ofsf_mktriple(sfto_reorder(ofsf_arg2l at,v));
      a := reorder car w;
      b := reorder cadr w;
      c := reorder caddr w;
      if op eq 'neq then
	 return rl_mkn('or,
	    {ofsf_0mk2('neq,a),ofsf_0mk2('neq,b),ofsf_0mk2('neq,c)});
      discr := addf(exptf(b,2),negf multf(4,multf(a,c)));
      if op eq 'equal then <<
	 if ofsf_surep(ofsf_0mk2('neq,a),theo) then
	    return ofsf_0mk2('geq,discr);
	 return rl_mkn('or,
	    {ofsf_0mk2('greaterp,discr),ofsf_0mk2('equal,c),
	       rl_mkn('and,{ofsf_0mk2('equal,discr),ofsf_0mk2('neq,b)})})
      >>;
      if op eq 'leq then <<
	 if ofsf_surep(ofsf_0mk2('greaterp,a),theo) then
	    return ofsf_0mk2('geq,discr);
	 return rl_mkn('or,
	    {ofsf_0mk2('lessp,a),ofsf_0mk2('leq,c),
	       rl_mkn('and,{ofsf_0mk2('geq,discr),ofsf_0mk2('neq,b)})})
      >>;
      if op eq 'geq then <<
	 if ofsf_surep(ofsf_0mk2('lessp,a),theo) then
	    return ofsf_0mk2('geq,discr);
	 return rl_mkn('or,
	    {ofsf_0mk2('greaterp,a),ofsf_0mk2('geq,c),
	       rl_mkn('and,{ofsf_0mk2('geq,discr),ofsf_0mk2('neq,b)})})
      >>;
      if op eq 'lessp then <<
	 if ofsf_surep(ofsf_0mk2('greaterp,a),theo) then
	    return ofsf_0mk2('greaterp,discr);
      	 return rl_mkn('or,{ofsf_0mk2('greaterp,discr),
	    ofsf_0mk2('lessp,a),ofsf_0mk2('lessp,c)})
      >>;
      if op eq 'greaterp then <<
	 if ofsf_surep(ofsf_0mk2('lessp,a),theo) then
	    return ofsf_0mk2('greaterp,discr);
      	 return rl_mkn('or,{ofsf_0mk2('greaterp,discr),
	    ofsf_0mk2('greaterp,a),ofsf_0mk2('greaterp,c)})
      >>;
      rederr {"ofsf_sqsc1at: unknown operator ",op}
   end;

procedure ofsf_sqsc!-test(atl,v);
   begin scalar hit,a,d;
      while atl do <<
	 a := car atl;
	 atl := cdr atl;
	 d := degreef(ofsf_arg2l a,v);
	 if d=1 then
	    atl := hit := nil
	 else if d=2 then
	    if hit then
	       atl := hit := nil
	    else
	       hit := a
      >>;
      return hit
   end;

procedure ofsf_lthsimpl(l);
   % Local theory simplification.
   begin scalar w;
      w := rl_smkn('and,l);
      w := rl_simpl(w,nil,-1) where
	 !*rlsiexpl=nil,!*rlsiexpla=nil,!*rlsiatadv=nil,!*rlsifac=nil;
      return ofsf_cj2atl w
   end;

procedure ofsf_cj2atl(f);
   % Conjunction to atomic formula list.
   if f eq 'true then
      {}
   else if f eq 'false then
      rederr "ofsf_cj2atl: inconsistent theory"
   else if rl_op f eq 'and then
      rl_argn f
   else
      {f};

procedure ofsf_fbqe(f);
   <<
      if !*rlverbose then
	 ioto_prin2t "ofsf_cad with optimization of projection order";
      cdr ofsf_cad(f,ofsf_cadporder f)
   >>;

endmodule;  % [ofsfqe]

end;  % of file
