% ----------------------------------------------------------------------
% $Id: clbnf.red,v 1.20 2003/12/08 15:30:00 dolzmann Exp $
% ----------------------------------------------------------------------
% Copyright (c) 1995-2003 A. Dolzmann, A. Seidl, and T. Sturm
% ----------------------------------------------------------------------
% $Log: clbnf.red,v $
% Revision 1.20  2003/12/08 15:30:00  dolzmann
% Renamed variable state to kstate, because hephys/physop.red defines
% state as an stat function. This causes errors reading clbnf provided
% physop is loaded.
%
% Revision 1.19  2003/06/11 08:45:23  dolzmann
% Rewritten Quine simplification such that rl_simpl is not called by
% default. Calling rl_simpl can cause male function due to unwanted
% algebraic simplifications.
% Added procedure for converting a bnf into set representation wrt. a
% given operator.
% Added black boxes rl_qssimpl and rl_qssiadd.
% Added black box implementations cl_qssimpl and cl_qssibysimpl.
%
% Revision 1.18  2003/06/06 08:06:14  dolzmann
% Added power set computation by enumeration.
%
% Revision 1.17  2003/06/04 07:23:04  dolzmann
% Added CNF support to cl_quine by computing the minimal DNF of the negated
% input formula.
%
% Revision 1.16  2003/06/04 06:08:09  dolzmann
% Added procedure cl_qssusubytab as a generic implementation for
% blackbox rl_qssubsumtion. It requires rl_qssusuat.
% Improved procedure cl_qsimpltestccl.
%
% Revision 1.15  2003/06/03 16:09:40  dolzmann
% Fixed a bug in cl_qsnconsens1: The return value nil of rl_qstrycons
% entered the list of consensus.
% Fixed a bug in cl_qssusubyit: The eq test to 'true was missing.
%
% Revision 1.14  2003/06/03 11:18:10  dolzmann
% Completely overworked quine simplification. In particular: Removed
% bugs during prime implicant computation. Added missing tautology test
% for implication test. Added comments on use of black boxes. Moved and
% sorted procedure definitions.
%
% Revision 1.13  2003/05/27 08:19:54  dolzmann
% Procedure cl_qsconsens now returns a list of consensus. Adapted
% procedure cl_qscpi accordingly.
%
% Revision 1.12  2003/05/27 07:27:51  dolzmann
% Use cl_qssub instead of rl_subfof;
% Added black box rl_qssubat.
% Added default implementation cl_qssubat for black box rl_qssubat.
% Added procedure cl_qssimplc as a future replacement for rl_simpl.
% Changed cl_bnf2set. cl_quine can now deal with BNFs containing
% identical clauses.
%
% Revision 1.11  2003/05/23 16:01:01  dolzmann
% Added selection from all prime implicants as descibed in Quine 1955.
% Fixed a bug in consensus computation: nil as an legal conses was not
% recognized.
%
% Revision 1.10  2003/05/21 09:03:27  dolzmann
% Added first experimental implementation of service rlquine for
% simplifying Boolean normal forms in the style of W. V. Quine.
%
% Revision 1.9  2003/05/20 11:35:46  dolzmann
% Moved procedures.
%
% Revision 1.8  1999/04/13 13:10:55  sturm
% Updated comments for exported procedures.
%
% Revision 1.7  1999/04/01 11:26:47  dolzmann
% Reformatted one procedure.
%
% Revision 1.6  1999/03/22 17:07:12  dolzmann
% Changed copyright information.
% Reformatted comments.
%
% Revision 1.5  1999/03/21 13:34:06  dolzmann
% Corrected comments.
%
% Revision 1.4  1996/10/07 11:45:47  sturm
% Added fluids for CVS and copyright information.
%
% Revision 1.3  1996/07/13 10:53:07  dolzmann
% Added black box implementations cl_bnfsimpl, cl_sacatlp, and cl_sacat.
%
% Revision 1.2  1996/07/07 14:34:19  sturm
% Turned some cl calls into service calls.
%
% Revision 1.1  1996/03/22 10:31:27  sturm
% Moved and split.
%
% ----------------------------------------------------------------------
lisp <<
   fluid '(cl_bnf_rcsid!* cl_bnf_copyright!*);
   cl_bnf_rcsid!* := "$Id: clbnf.red,v 1.20 2003/12/08 15:30:00 dolzmann Exp $";
   cl_bnf_copyright!* := "(c) 1995-2003 by A. Dolzmann, A. Seidl, and T. Sturm"
>>;

module clbnf;
% Common logic boolean normal forms. Submodule of [cl]. This module
% provides CNF and DNF computation.

%DS
% <SG-DNF> ::= <GOR> . <SGCL>
% <SGCL> ::= (<SGCONJ>,...)
% <SGCONJ> ::= <GAND> . <SATOTVL>
% <GOR> ::= ['or] | ['and]
% <GAND> ::= ['and] | ['or] "opposite to <GOR>"
% <SATOTVL> ::= (<TRUTH VALUE>) | (<ATOMIC FORMULA>, ...)

procedure cl_dnf(f);
   % Common logic disjunctive normal form. [f] is a formula. Returns a
   % DNF of [f].
   rl_simpl(cl_gdnf(f,'or),nil,-1);

procedure cl_cnf(f);
   % Common logic conjunctive normal form. [f] is a formula. Returns a
   % CNF of [f].
   rl_simpl(cl_gdnf(f,'and),nil,-1);

procedure cl_gdnf(f,gor);
   % Common logic generic disjunctive normal form. [f] is a formula;
   % [gor] is one of [and], [or]. Returns a G-DNF of [f].
   begin scalar strictgdnf,gdnf,svrlsiexpla;
      f := rl_simpl(rl_nnf f,nil,-1);
      svrlsiexpla := !*rlsiexpla;
      !*rlsiexpla := nil;
      (strictgdnf := cl_strict!-gdnf(f,gor)) where !*rlbnfsm=nil;
      if !*rlbnfsm then
	 strictgdnf := gor . cl_subsume(rl_argn strictgdnf,gor);
      !*rlsiexpla := svrlsiexpla;
      gdnf := cl_unstrict(strictgdnf,gor);
      return gdnf
   end;

procedure cl_strict!-gdnf(f,gor);
   % Common logic strict generic disjunctive normal form. [f] is a
   % formula; [gor] is one of [and], [or]. Returns a strict g-DNF,
   % i.e. a formula upto unary [and]'s and [or]'s, which is in g-DNF.
   begin scalar w;
      w := cl_mkstrict(rl_simpl(cl_strict!-gdnf1(f,gor),nil,-1),gor);
      return rl_bnfsimpl(w,gor)
   end;

procedure cl_subsume(gcl,gor);
   % Common logic subsume. [gcl] is a generic conjunction list; [gor]
   % is one of [and], [or]. Returns a generic conjunction list
   % equivalent to [gcl]. Performs simplification by subsumption.
   begin scalar w;
      if null gcl or null cdr gcl then return gcl;
      w := cl_subsume1(gcl,gor);
      if car w then <<
	 cddr w := cl_subsume(cddr w,gor);
	 return cdr w
      >>;
      return cl_subsume(cdr w,gor)
   end;

procedure cl_subsume1(gcl,gor);
   % Common logic subsume 1. [gcl] is a generic conjunction list;
   % [gor] is one of [and], [or]. A pair $(c,l)$ is returned, where
   % $c$ is [nil] or a list of atomic formulas and $l$ is a generic
   % conjunction list. [gcl] is modified. The subsumption relation
   % beween [car gcl] and all elements of [cdr gcl] is tested. If $c$
   % is nil, [car gcl] was suberflous. $l$ is the modified [gcl] in
   % which all superflous conjunctions are deleted. If $c$ is non-nil,
   % it is [car gcl] and [car gcl] cannot be dropped. If [cl_setrel]
   % is used this requires, that [!*rlsiso] and [!*rlidentify] are on.
   begin scalar a,w,x,scgcl,oscgcl;
      x := cdar gcl;
      oscgcl := gcl;
      scgcl := cdr gcl;
      while scgcl do <<
	 a := car scgcl; scgcl := cdr scgcl;
	 w := if !*rlbnfsm then
 	    rl_subsumption(x,cdr a,gor)
 	 else
 	    cl_setrel(x,cdr a,gor);
	 if w eq 'keep1 then
	    cdr oscgcl := scgcl
	 else if w eq 'keep2 then
	    x := scgcl := nil
	 else
	    oscgcl := cdr oscgcl
      >>;
      if null x then gcl := cdr gcl;
      return x . gcl
   end;

procedure cl_setrel(l1,l2,gor);
   % Common logic set relation. [l1] and [l2] are list of atomic
   % formulas. [gor] is one of [and], [or]. Returns [nil], [keep1], or
   % [keep2]. If [l1] is a subset of [l2] [keep1] is returned; if [l2]
   % is a subset of [l1] [keep2] is returned otherwise [nil] is
   % returned.
   begin scalar kstate,a1,hlp;
      while l1 and l2 and car l1 eq car l2 do <<
	 l1 := cdr l1;
	 l2 := cdr l2
      >>;
      if null (l1 and l2) then <<
      	 if null (l1 or l2) then return 'keep1;  % both equal.
       	 return l2 and 'keep1 or 'keep2
      >>;
      kstate := 'keep1;
      if rl_ordatp(car l1,car l2) then <<
	 hlp := l1; l1 := l2; l2 := hlp;
	 kstate := 'keep2
      >>;
      repeat <<
	 a1 := car l1; l1 := cdr l1;
	 l2 := memq(a1,l2);
	 if null l2 then a1 := l1 := nil
      >> until null l1;
      return a1 and kstate
   end;

procedure cl_strict!-gdnf1(f,gor);
   % Common logic disjunctive normal form in strict representation.
   % [f] is a formula containing no first-order operators but $\land$
   % and $\lor$; [gor] is one of ['and], ['or]. Returns a strict g-DNF
   % of [f], i.e. a g-disjunction of g-conjunctions of atomic formulas
   % including unary $\lor$ and $\land$ if necessary.
   begin scalar gand,op,subgdnfl,noop,noopgdnf;
      gand := if gor eq 'or then 'and else 'or;
      op := rl_op f;
      if op eq gor then
	 return rl_mkn(gor,for each subf in rl_argn(f) join
	    rl_argn(cl_strict!-gdnf(subf,gor)));
      if op eq gand then <<
	 subgdnfl := for each subf in rl_argn(f) collect
	    cl_strict!-gdnf(subf,gor);
	 % Switch to noop form.
	 noop := for each subf in subgdnfl collect
	    for each gconj in rl_argn subf collect rl_argn gconj;
	 % Computing the cartesian product of the conjunctive lists is
	 % now equivalent to an application of the law of
	 % distributivity, though the result is not flat yet.
	 noopgdnf := cl_bnf!-cartprod noop;
	 % Switch back to our normal representation.
	 return rl_mkn(gor,for each gconj in noopgdnf collect
	    rl_mkn(gand,for each x in gconj join append(x,nil)))
      >>;
      if rl_cxp op and not rl_tvalp op then
      	 rederr {"cl_strict!-gdnf: illegal operator",op,"in BNF computation"};
      return rl_mkn(gor,{rl_mkn(gand,{f})})
   end;

procedure cl_mkstrict(f,gor);
   % Common logic make strict. [f] is a g-DNF. Returns a strict g-DNF,
   % possibly including one truth value.
   begin scalar op,gand;
      gand := cl_flip gor;
      op := rl_op f;
      if not rl_cxp op or rl_tvalp op then
 	 return rl_mkn(gor,{rl_mkn(gand,{f})});
      if op eq gand then
 	 return rl_mkn(gor,{f});
      if op neq gor then
 	 rederr {"BUG IN cl_mkstrict"};
      return rl_mkn(gor,for each subf in rl_argn f collect
	 if rl_op subf eq gand then subf else rl_mkn(gand,{subf}))
   end;

procedure cl_unstrict(sgdnf,gor);
   % Common logic unstrict, [sdnf] is a sg-DNF; [gor] is one of [and],
   % [or]. Returns a g-DNF.
   rl_smkn(gor,for each conj in rl_argn sgdnf collect
      % A unary g-and does not have a cddr, ignore it.
      if cdr rl_argn conj then conj else car rl_argn conj);

procedure cl_bnf!-cartprod(s);
   % Common logic boolean normal form cartesian product. [s] is a list
   % $(s_1,...,s_n)$ of lists. Returns $s_1 \times ... \times s_n$ as
   % a list of $n$-element lists. The empty set and singletons are
   % their own cartesian product.
   if null s or null cdr s then s else cl_bnf!-cartprod1 s;

procedure cl_bnf!-cartprod1(s);
   % Common logic boolean normal form cartesian product. [s] is a list
   % $(s_1,...,s_n)$ of lists with $n \geq 2$. Returns $s_1 \times ...
   % \times s_n$ as a list of $n$-element lists.
   begin scalar w;
      if null cdr s then
      	 return for each m in car s collect {m};
      w := cl_bnf!-cartprod1 cdr s;
      return for each m in car s join
      	 for each y in w collect m . y
   end;

procedure cl_bnfsimpl(sgdnf,gor);
   % Common logic boolean normal form simplification. [sgdnf] is an
   % SG-DNF; [gor] is one of the operators [and], [or]. Returns an
   % SG-DNF equivalent to [sgdnf]. Performs simplification of [gcl].
   % Accesses switch [rlbnfsac].
   if !*rlbnfsac then cl_sac(sgdnf,gor) else sgdnf;

procedure cl_sac(sgdnf,gor);
   % Common logic subsumption and cut. [sgdnf] is a sg-DNF; [gor] is
   % one of [or], [and]. Returns a sg-DNF equivalent to [sgdnf]. This
   % procedures performs simplifications based on order theoretical
   % subsumption and cut. There are no possible applications of order
   % theoretical subsumption and cut between subformulas of the
   % returned sg-DNF.
   begin scalar w,gand;
      if rl_tvalp car rl_argn car rl_argn sgdnf then return sgdnf;
      gand := cl_flip(gor);
      % switch to noop form
      w := for each x in rl_argn sgdnf collect
	 rl_argn x;
      w := cl_applysac(w,gor);
      if w eq 'break then
	 return rl_mkn(gor,{rl_mkn(gand,{cl_cflip('true,gor eq 'or)})});
      w := for each x in w join
	 if x then
	    {rl_mkn(gand,x)}
	 else
	    nil;
      if null w then
	 return rl_mkn(gor,{rl_mkn(gand,{cl_cflip('true,gor eq 'or)})});
      return gor . w
   end;

procedure cl_applysac(l,gor);
   % Common logic apply subsumption and cut. [l] is a list of lists of
   % atomic formulas; [gor] is one of [or], [and]. Returns ['break] or
   % a list $k$ of list of atomic formulas. If ['break] is returned
   % [l] is as a g-DNF equivalent to ['true] in case of ['gor eq 'or]
   % and equivalent to ['false] in case ['gor eq 'and]. The lists are
   % considered as generic disjunctive normal forms and are in this
   % sense equivalent. There is no possible application of order
   % theoretical subsumption or cut between elements of $k$.
   begin scalar w,ll,res;
      ll := l;
      while ll do <<
	 w := cl_applysac1(car ll,res,gor);
	 if w eq 'break then <<
	    ll := nil;
	    res := 'break
	 >> else <<
	    ll := cdr ll;
	    if car w then
	       res := cdar w . cdr w
	    else
	       res := cdr w
	 >>
      >>;
      return res
   end;

procedure cl_applysac1(c,l,gor);
   % Common logic apply subsumption and cut 1. [c] is a list of atomic
   % formulas; [l] is a list of list of atomic formulas; [gor] is one
   % of [or], [and]. Returns ['break] or a pair $(c' . \lambda)$. If
   % ['break] is returned [l] is as a g-DNF equivalent to ['true] in
   % case of ['gor eq 'or] and equivalent to ['false] in case ['gor eq
   % 'and]. $c'$ is either [nil] or a pair $(\tau . \gamma)$, where
   % $\tau$ is one of [T] and [nil] and $\gamma$ is a list of atomic
   % formulas. $\lambda$ is a list of list of atomic formulas. If $c'$
   % is [nil] then the conjunction over [c] is implied by a
   % conjunction over an element in [l]. If $\tau$ is [T] then
   % $\gamma$ is equal to $c$, otherwise $\gamma$ is the result of a
   % cut between $c$ and an element of $l$. In all cases there is no
   % possible application of subsumption or cut between $\gamma$ and
   % an arbitrary element of $\lambda$. [l] is modified.
   begin scalar w,flg;
      flg:=T;
      repeat <<
	 w := cl_applysac2(c,l,gor);
	 if w eq 'break then <<
	    w := '(nil);  % leave the loop
	    flg := 'break
	 >>;
	 if car w and null caar w then <<
	    flg:=nil;
	    c := cdar w;
	    l := cdr w
	 >>;
      >> until null car w or caar w;
      if flg eq 'break then
	 return 'break;
      if null car w then
	 return w;
      return (flg . cdar w) . cdr w
   end;

procedure cl_applysac2(c,l,gor);
   % Common logic apply subsumption and cut 1. [c] is a list of atomic
   % formulas; [l] is a list of list of atomic formulas; [gor] is one
   % of [or], [and]. Returns ['break] or a pair ($c'$ . $\lambda$). If
   % ['break] is returned [l] is as a g-DNF equivalent to ['true] in
   % case of ['gor eq 'or] and equivalent to ['false] in case ['gor eq
   % 'and]. $c'$ is either [nil] or a pair $(\tau . \gamma)$, where
   % $\tau$ is one of [T] and [nil] and $\gamma$ is a list of atomic
   % formulas. $\lambda$ is a list of list of atomic formulas. If $c'$
   % is [nil] then the conjunction over [c] is implied by a
   % conjunction over an element in [l]. If $\tau$ is [T] then
   % $\gamma$ is equal to $c$, otherwise $\gamma$ is the result of a
   % cut between $c$ and an element of $l$. [l] is modified. If
   % ['break] is returned then the formula $['gor]([c],\phi)$ is
   % equivalent to ['true] in the case ['gor eq 'or] or to ['false] in
   % the case ['gor eq 'and].
   begin scalar w,ll;
      if null l then return ( (T . c) . nil);
      ll := l;
      while ll and ((w := cl_subandcut(c, car ll,gor)) eq 'keep1) do
	 ll := cdr ll;
      if null w then return 'break;
      if null ll then return ((T . c) . nil);
      if w eq 'keep2 then return (nil . ll);
      if w neq 'failed then  % [w] is the result of the cut
	                     % between [c] and [car ll].
	 return (nil . w) . cdr ll;
      % We know, that there is no interaction between [c] and [car ll]
      w := cl_applysac2(c,cdr ll,gor);
      if w eq 'break then
	 return 'break;
      cdr ll := cdr w;
      return car w . ll;
   end;

procedure cl_subandcut(l1,l2,gor);
   % Common logic subsumption and cut. [l1] and [l2] are sorted lists
   % of atomic formulas; [gor] is one of ['or], ['and]. Returns
   % ['failed], ['keep1], ['keep2] or a list of atomic formulas. Both
   % [l1] and [l2] are considered as conjunctions. ['keep1] is
   % returned if [l2] subsumes [l1]; ['keep2] is returned if [l1]
   % subsumes [l2]. If a list [l] of atomic formulas is returned then
   % [l] is the result of a cut between [l1] and [l2]. Both
   % subsumption and cut means order theoretical generalizations of
   % the respective notions of the propositional calculus.
   begin scalar kstate,w,x; integer c;
      x := l1;  % Save one of [l1] and [l2] for computing a cut.
      % Determing the maximal common prefix of [l1] and [l2] and its length.
      while l1 and l2 and (car l1 equal car l2) do <<
	 c := c+1;
	 l1 := cdr l1; l2 := cdr l2
      >>;
      if null (l1 and l2) then <<  % on of [l1] and [l2] are empty
      	 if null (l1 or l2) then return 'keep1;  % both equal.
	 % [l1] is a ``subset'' of [l2] or vice versa.
       	 return (l2 and 'keep1) or 'keep2
      >>;
      % We have [l1 and l2] and [car l1 neq car l2].
      kstate := 'keep1;
      w := rl_sacat(car l1,car l2,gor);  % [w neq 'keep]
      if w eq 'keep2 then <<
	 kstate := 'keep2;
	 % swap [l1] and [l2] upto the first element.
	 w := cdr l1; l1 := cdr l2; l2 := w
      >> else if w eq 'keep1 then <<
	 l1 := cdr l1; l2 := cdr l2
      >> else if w then
	 return cl_trycut(x,c,w,cdr l1,cdr l2)
      else if rl_ordatp(car l1,car l2) then <<  % [car l1 neq car l2]
	 kstate := 'keep2;
	 w := l1; l1 := l2; l2 := w
      >>;
      % Now [l1] is ``shorter'' than [l2]; no cuts are possible.
      while l1 do <<
	 w := cl_sacatl(car l1, l2,gor);
      	 l2 := cdr w; w := car w;
	 l1 := cdr l1;
	 if w neq 'keep1 then
	    l1 := nil  % Leave the loop.
      >>;
      if w eq 'keep1 then return kstate;
      return 'failed
   end;

procedure cl_trycut(l,c,at,l1,l2);
   % Common logic try cut. [l], [l1], and [l2] are lists of atomic
   % formulas; [c] is an integer; [at] is an atomic formula or
   % ['drop]. Returns ['failed] or a sorted list $\lambda$ of atomic
   % formulas. If a cut beween [l1] and [l2] are possible then a list
   % of atomic formulas is returned, otherwise [nil] is returned. [l]
   % is a list $(a_1,...,a_n)$, [l1] is a list $(c_1,...,c_m)$.
   % $lambda$ is a list $(a_1,...,a_c,b,c_1,...,c_m)$, where $b$ is
   % the atomic formula [at] if [at] is not [drop], otherwise $b$ is
   % ommitted.
   begin scalar a;
      if null l1 and null l2 then <<
	 l := for i := 1 : c collect <<
	    a := car l; l := cdr l; a
	 >>;
	 if at eq 'drop then
	    return sort(l,'rl_ordatp);
	 return sort(at . l,'rl_ordatp)
      >>;
      if l1 neq l2 then return 'failed;
      % [l1] and [l2] are equal.
      for i:=1:c do << l1 := car l . l1; l := cdr l >>;
      if at neq 'drop then
	 l1 := at . l1;
      return sort(l1,'rl_ordatp)
   end;

% Generic black box implementations.

procedure cl_sacatl(a,l,gor);
   % Common logic subsume and cut atomic formula list. [a] is an
   % atomic formula; [l] is a sorted list of atomic formulas; [gor] is
   % one of [or], [and]. Returns a pair $(\alpha . \lambda)$ where
   % $\alpha$ is a relation, ['keep1], or [nil]; [l] is a possibly
   % empty list of atomic formulas. $\alpha$ is [T] if [a] is implied
   % by an atomic formula from [l]; if $\alpha$ is [nil] then neither
   % [a] is implied by an atomic formula from [l] nor a cut between
   % [a] and an atomic formula from [l] is possible, otherwise
   % $\alpha$ is the result of such a cut. $\lambda$ is the rest of
   % [l] not involved in the computation of $\alpha$.
   begin scalar w;
      if null l then
      	 return '(nil . nil);
      if not rl_sacatlp(a,l) then
      	 return (nil . l);
      w := rl_sacat(a,car l,gor);
      if not w then
      	 return cl_sacatl(a,cdr l,gor);
      if w memq '(keep1 keep) then
      	 return ('keep1 . cdr l);
      if w eq 'keep2 then
      	 return (nil . cdr l);
      return (w . cdr l)  % [w] is a relation or [drop]
   end;

procedure cl_sacatlp(a,l);
   % Common logic subsumption and cut atomic formula list predicate.
   % [a] is an atomic formula; [l] is a list of atomic formulas.
   % Returns [T] a subsumption or cut beween [a] and an element of [l]
   % is possible.
   T;

procedure cl_sacat(a1,a2,gor);
   % Common logic subsumption and cut atomic formula. [a1] and [a2]
   % are atomic formulas; [gor] is one of the operators [or], [and].
   % Returns [nil], one of the identifiers [keep], [keep1], [keep2],
   % [drop], or an atomic formula. The return value [nil] indicates
   % that neither a cut nor a subsumption can be applied. If [keep] is
   % returned, then the atomic formulas are identical. In the case of
   % [keep1] or [keep2] the corresponding atomic formula can be kept,
   % and the other one can be dropped. If an atomic formula $a$ is
   % returned, then this atomic formula is the result of the cut
   % beween [a1] and [a2]. If [drop] is returned, then a cut with
   % result [true] or [false] can be performed.
   if a1 = a2 then 'keep else nil;

% ----------------------------------------------------------------------
% Simplification of Boolean normal forms in the style of W. V. Quine.
% ----------------------------------------------------------------------

% The following services are required:
% * rl_simpl

% The following black boxes are required:
% * rl_qscsaat(at)      Default:  cl_qscsa
% * rl_qssubat(sl,at) Default:  cl_qssubat (requires rl_negateat)
% * rl_qsconsens      Default:  cl_qs1consens (requires rl_qstrycons)
%                     Default;  cl_qsnconsens (requires rl_qstrycons)
% * rl_qsimpltestccl  Default:  cl_qsimpltestccl (req. rl_qscsa,
%                                                rl_qssubat, rl_qstautp
%                                                rl_qssimpl)
% * rl_qstautp        Default:  cl_qstautp
% * rl_qssubsumep     Default:  cl_qssusubymem
%                               cl_qssusubysi
%                               cl_qssusubyit (requires rl_simpl)
%                               cl_qssusubytab (requires rl_qssusuat)
% * rl_qstrycons      Default   cl_qstrycons (requires rl_negateat)
% * rl_qssimpl        Defualt   cl_qssibysimpl
%                     Default   cl_qssimpl (requires rl_qssiadd)
% * rl_qssiadd        kein default

% Warning: The blackboxes rl_qscsaat and rl_qssubat belong to each
% other. They cannot be implemented independently.


% For rapid prototyping, using only the relation betwen $alpha$ and
% $\overline{\alpha}$ one can use the following assignment of generic
% implementations to black boxes. We use the service rl_simpl and the
% black boxes rl_negateat and rl_ordatp from the general context
% environment.
%  * rl_qssubsumep  ->  cl_qssusubymem
%  * rl_qsconsens  ->  cl_qsnconsens
%  * rl_qstrycons  ->  cl_qstrycons
%  * rl_qsimpltestccl  ->  cl_qsimpltestccl
%  * rl_qscsaat  ->  cl_qscsaat
%  * rl_qssubat  ->  cl_qssubat
%  * rl_qstautp  ->  cl_qstautp

% To gain maximal use of algebraic dependencies between relations one
% have to implement at least the following black boxes in a context
% specific manner:   rl_qssubsumep   rl_qstrycons   rl_qssubat

% TODO: Truth values in BNF
% TODO: List2set vermeiden -> adjoin
% TODO: CNF's
% TODO: rl_simpl beschraenken oder eigenes.
% TODO: Substituierende Simplifikation

procedure cl_quine(f);
   % Common logic Quine simplification. [f] is a a formula in a BNF.
   %  Returns a formula in BNF.
   begin scalar w,op,s;
      w := cl_bnf2set f;
      op := car w;      
      if not op then
	 return f;
      if op eq 'and then
	 s := cl_qsnot cdr w
      else
      	 s := cdr w;
      s := cl_qs(s,'or);  % Non generic
      if op eq 'and then
	 s := cl_qsnot s;
      return cl_set2bnf(s,op)
   end;

procedure cl_qsnot(s);
   for each c in s collect cl_qsnot1 c;

procedure cl_qsnot1(c);
   for each a in c collect rl_negateat a;

procedure cl_bnf2set(f);
   cl_bnf2set1(f,nil);

procedure cl_bnf2set1(f,xop);
   % Common logic bnf to set representation. [f] is a formula in bnf.
   % Returns a pair $(\gamma,l)$ where $l$ is a list of list of atomic
   % formulas and $\gamma$ is [nil] or an operator. if $\gamma$ is
   % [or] then [f] is a DNF and $l$ is the set representation of $f$;
   % if it is [and] then f is a CNF and again $f$ is a set
   % representation of f; if $\gamma$ is [nil] then $l$ is identical
   % to [f].
   begin scalar op,w;
      if rl_tvalp f then
	 return nil  . f;
      if not cl_cxfp f then
	 return nil . {{f}};
      op := rl_op f;
      if not(op memq '(and or)) then
	 rederr {"cl_bnf2set: not in bnf: ",f};
      w := cl_bnf2set2(rl_argn f,op);
      if not car w then  % List of atomic formulas, translated to singletons
	 if op eq xop then
	    return op  . w
	 else if cl_flip op eq xop then
	    return op . {rl_argn f}
	 else      	    
	    return nil  . f;
      return op . cdr w
   end;

procedure cl_bnf2set2(fl,op);
   % Common logic bnf to set representation subroutine. [fl] is a list
   % of atomic formulas or flat formulas not containing truth values.
   % Returns a pair $(\gamma,l)$ where l is a list of list of atomic
   % formulas; $\gamma$ is a flag. If it is [T] then fl contains a
   % non-atomic formula.
   begin scalar xop,flg,w;
      xop := cl_flip op;
      w := for each f in fl collect <<
	 if not cl_cxfp f then
	    {f}
	 else if rl_op f eq xop then <<
	    flg := T;
	    sort(list2set rl_argn f,'rl_ordatp)
	 >> else
	    rederr {"cl_bnf2set1: not in bnf: ",f}
      >>;
      return flg . list2set w
   end;

procedure cl_set2bnf(ll,op);
   begin scalar flop;
      flop := cl_flip(op);
      return rl_smkn(op,for each l in ll collect
	 rl_smkn(flop,l))
   end;

procedure cl_qs(s,op);
   % Common logic quine simplification. [s] is a set representation of
   % a BNF. [op] is one of [and], [or]. Returns a set representation.
   begin scalar w;
      if !*rlverbose then
	 ioto_tprin2 {"[Quine: ",cl_qssize s,"/",length s};
      w := cl_qscpi(s,op);      
      if w eq 'break then <<
	 ioto_prin2t {" -> 0/0]"};
	 return {{}} % True for DNF; False for CNF
      >>;
      if !*rlverbose then
	 ioto_prin2 {" -> ",cl_qssize w,"/",length w};
      return  cl_qsselect(w,op);
   end;

procedure cl_qscpi(cl,op);
   % Common logic quine simplification compute prime implicants. [s]
   % is a set representation of a BNF. [op] is one of [and], [or].
   % Returns a set representation.
   begin scalar firstl,scfirstl,first,secondl,scsecondl,second,newl,w;
      newl := cl;
      while newl do <<
	 newl := cl_qssisu(newl,op);
	 firstl := cl_qssisutwo(firstl,newl,op);
	 secondl := newl;
	 newl := nil;
	 scfirstl := firstl;
	 while scfirstl do <<
	    first := car scfirstl;
	    scfirstl := cdr scfirstl;
	    scsecondl := secondl;
	    while scsecondl do <<
	       second := car scsecondl;
	       scsecondl := cdr scsecondl;
	       if not(second eq first) then <<
	       	  w := rl_qsconsens(first,second,op);
	       	  if w eq 'break then
	       	     newl := scsecondl := scfirstl := nil
	       	  else
	       	     foreach cs in w do
	       	     	if cs and not(cl_qssubsumelp(cs,firstl,op)) and
		     	   not(cs member newl)
		     	then
	       	     	   newl := cs . newl
	       >>
	    >>  % while scsecondl
	 >>  % while scfirstl
      >>;  % newl
      if (w eq 'break) then
	 return 'break;
      return firstl
   end;
   
procedure cl_qssisu(l,op);
   % Simplify by subsumtion. [l] is a list of clauses. Returns a list
   % of clauses.
   for each x in l join
      if not(cl_qssubsumelp(x,delq(x,l),op)) then
	 {x};

procedure cl_qssisutwo(l1,l2,op);
   % Neither to l1 nor to l2 subsumption can be applied. No clause of
   % l2 subsumes a clause of l1.
   begin scalar w;
      w := for each x in l1 join
      	 if not(cl_qssubsumelp(x,l2,op)) then
	    {x};
      return nconc(w,l2)
   end;

procedure cl_qssubsumelp(c,cl,op);
   % Returns [T] if [c] subsumes one of the clauses in cl.
   begin scalar r;
      while (cl and not(r)) do <<
	 r := cl_qssubsumep(c,car cl,op);
	 cl := cdr cl;
      >>;
      return r
   end;

procedure cl_qssubsumep(c1,c2,op);
    if op eq 'or then
       rl_qssubsumep(c1,c2,op)
    else
       rl_qssubsumep(c2,c1,op);

switch psen;
on1 'psen;

procedure cl_qsselect(s,op);
   begin scalar w,core,dcs; integer csize,wsize,clen,wlen;
      w := cl_qsspltcore(s,op);
      core := car w;
      dcs := cdr w;
      if !*rlverbose then <<
	 csize := cl_qssize core;
	 clen := length core;
	 ioto_prin2 {" -> ",csize,"/",clen,"+",cl_qssize dcs,"/",length dcs}
      >>;
      dcs := cl_qsrmabsdisp(core,dcs,op);
      if !*rlverbose then
	 ioto_prin2 {" -> ",csize,"/",clen,"+",cl_qssize dcs,"/",length dcs};
      if !*psen then
	 w := cl_qsselect2(core,dcs,op)
      else	 
      	 w := cl_qsselect1(core,dcs,cl_subsets dcs,op);
      if !*rlverbose then <<
	 wsize := cl_qssize w;
	 wlen := length w;
	 ioto_prin2t {" -> ",csize,"/",clen,"+",wsize,"/",wlen,
	    " = ",csize+wsize,"/",clen+wlen,"]"}
      >>;
      w := nconc(w,core);
      return w;
   end;

procedure cl_qsselect1(core,dcs,potdcs,op);
   begin scalar r,w;
      potdcs := cdr reversip potdcs;
      while potdcs and not(r) do <<
	 w := setdiff(dcs,car potdcs);
	 if cl_qsimpltestclcl(car potdcs,append(core,w),op) then
	    r := w;
	 potdcs := cdr potdcs;
      >>;
      return r
   end;

procedure cl_qsselect2(core,dcs,op);
   begin scalar r,w,kstate,potdcs;
      kstate := dcs . nil;
      cl_ps kstate;  % Remove leading nil;
      while (potdcs:=cl_ps kstate) neq 'final and not(r) do <<
	 w := setdiff(dcs,potdcs);
	 if cl_qsimpltestclcl(w,append(core,potdcs),op) then
	    r := potdcs;
      >>;
      return r
   end;

procedure cl_qsspltcore(s,op);
   begin scalar core,dcs;
      for each x in s do
	 if rl_qsimpltestccl(x,delq(x,s),op) then
      	    dcs := x . dcs
	 else
	    core := x . core;
      return core . dcs;
   end;

procedure cl_qsrmabsdisp(core,dcs,op);
   for each c in dcs join
      if not(rl_qsimpltestccl(c,core,op)) then
	 {c};

procedure cl_qsimpltestclcl(pl,cl,op);
   % quine simplification implication test clauses list clauses list.
   % [pl] is the list of all premise clauses; [cl] is the list of all
   % conclusion clauses; [op] is one of [and] or [or]. Returns [T] or
   % [nil].
   begin scalar r;
      r := T;
      while pl and r do <<
	 r := rl_qsimpltestccl(car pl,cl,op);
	 pl := cdr pl;
      >>;
      return r
   end;

procedure cl_subsets(l);
   sort(cl_subsets1 l,'cl_lengthp);

procedure cl_lengthp(l1,l2);
   length l1 < length l2;

procedure cl_subsets1(l);
   begin scalar w,r,x;
      if null l then
      	 return {nil};
      w := cl_subsets1 cdr l;
      x := car l;
      for each y in w do
	 r := (x . y) . r;
      return nconc(r,w)
   end;

procedure cl_qssize(s);
   for each x in s sum length x;

%------------------------------------------------------------------------
% Generic Implementations of rl_qssubsumep
%------------------------------------------------------------------------

procedure cl_qssusubysi(c1,c2,op);
   % Subsumtion by Simplification.
      rederr "Out of order -> ueber rl_simpl";
% cl_qssimplc1(c2,c1,op) eq 'true;  

procedure cl_qssusubyit(c1,c2,op);
   % Subsumtion by implication test.
   cl_qsimpltestcc(c1,c2,op) eq 'true;

procedure cl_qssusubymem(c1,c2,op);
   % Subsumtion by member. Returns [T] is [c1] g-subsumes [c2] and
   % hence [c1] implies [c2].
   begin scalar l1,l2;
      l1 := length c1;
      l2 := length c2;
      if not(l1>=l2) then
	    return nil;
      return cl_qssusubymem1(c1,c2)
   end;

procedure cl_qssusubymem1(c1,c2);
   begin scalar r;
      r := T;
      while c2 and r do <<
	 if not(car c2 member c1) then
	    r := nil;
	 c2 := cdr c2;
      >>;
      return r
   end;

procedure cl_qssusubytab(c1,c2,op);
   % Subsumtion by table. Returns [T] is [c1] g-subsumes [c2] and
   % hence [c1] implies [c2].
   begin scalar l1,l2;
      l1 := length c1;
      l2 := length c2;
      if not(l1>=l2) then
	 return nil;
      return cl_qssusubytab1(c1,c2,op)
   end;

procedure cl_qssusubytab1(c1,c2,op);
   % Subsumtion by table. Returns [T] is [c1] g-subsumes [c2] and
   % hence [c1] implies [c2].
   begin scalar r;
      r := T;
      while c2 and r do <<
	 r := cl_qssusubytab2(c1,car c2,op);
	 c2 := cdr c2
      >>;
      return r
   end;

procedure cl_qssusubytab2(c1,a,op);
   begin scalar r;
      while c1 and not(r) do <<
	 r := rl_qssusuat(car c1,a,op);
	 c1 := cdr c1
      >>;
      return r
  end;

%------------------------------------------------------------------------
% Generic implementations of rl_qssimpl
%------------------------------------------------------------------------

% Variant 1: Special simplifier
procedure cl_qssimpl(s,theo,op);
   begin scalar r,a,w,ats;
      while s and not(rl_tvalp r) do <<
	 a := car s;
	 w := cl_qssimplc(a,theo,op);
	 if w eq 'true then
	    r := 'true
	 else if w neq 'false then
	    r := w . r;
	 s := cdr s;
      >>;
      if r eq 'true then
	 'true;
      w := nconc(ats,r);
      return if null w then
	 'false
      else w
   end;

procedure cl_qssimplc(c,theo,op);
   % simplification of one clause.
   begin scalar r,a;
      while c and r neq 'false do <<
	 a := car c;
	 if a eq 'false then
	    r := 'false
	 else if a neq 'true then
	    r := rl_qssiadd(a,r,theo,op);
	 c := cdr c;
      >>;
      return if null r then
	 'true
      else
	 r
   end;

% Variant 1: Using rl_simpl

procedure cl_qssibysimpl(s,theo,op);
   begin scalar !*rlsiexpla,!*rlsiexpl,f,w;
      f := rl_simpl(cl_set2bnf(s,op),nil,-1);  
      if rl_tvalp f then
	 return f;
      w := cl_bnf2set1(f,'or);
      return cdr w
   end;

%------------------------------------------------------------------------
% Generic implementations of rl_qsimpltestccl
%------------------------------------------------------------------------

procedure cl_qsimpltestccl(cp,clc,op);
   % .. clause clauses list.
   begin scalar w,r,sl;
      sl := cl_qscsa cp;
      while clc and r neq 'true do <<
	 w := cl_qsimpltestcc1(sl,cp,car clc,op);
	 if w eq 'true then
	    r := 'true
	 else if w neq 'false then
	    r := w . r;
	 clc := cdr clc;
      >>;
      %      w := cl_qssimplf(rl_smkn(op,r),nil);
      %      if not rl_tvalp w and cl_qe(rl_all(w,nil)) eq 'true then
      %	 rederr {"cl_asimpltestcc: computed non-atomic formula:",w};      
      return rl_qstautp r
   end;

procedure cl_qsimpltestcc(cp,cc,op);
   cl_qsimpltestcc1(cl_qscsa cp,cp,cc,op);

procedure cl_qsimpltestcc1(sl,cp,cc,op);  % TODO
   begin scalar w;
      w := rl_qssimpl({cl_qssubc(sl,cc)},nil,op);  % Warnung: Clause -> Clause
      return if rl_tvalp w then
	 w
      else if cdr w then
	 rederr {"cl_qssimpltestcc1: Unexpected complex formula",w}
      else
	 car w
   end;

procedure cl_qstautp(f);
   if f eq 'true then
      T
   else if f eq 'false then
      nil
   else
      (cl_qscpi(f,'or)) eq 'break;

procedure cl_qscsa(c);
   % Compute satisfying assignment.
   for each a in c collect
      rl_qscsaat a;

procedure cl_qssub(pl,s);
   for each c in s collect
      cl_qssubc(pl,c);

procedure cl_qssubc(pl,c);
   for each a in c collect
      rl_qssubat(pl,a);

%------------------------------------------------------------------------
% Generic Implementations of rl_qscsaat
%------------------------------------------------------------------------

procedure cl_qscsaat(a);
   a;

%------------------------------------------------------------------------
% Generic Implementations of rl_qscsaat
%------------------------------------------------------------------------

procedure cl_qssubat(pl,a);
   if a member pl then
      'true
   else if rl_negateat(a) member pl then
      'false
   else
      a;

%------------------------------------------------------------------------
% Generic Implementations of rl_qsconsens
%------------------------------------------------------------------------

% First variant: Assume there is maximal one consens of two clauses.

procedure cl_qs1consens(c1,c2,op);
   % consens computation of 1 consensus. [c1] and [c2] are clausels.
   % Computes the unique determined consenus of [c1] and [c2] provided
   % it exists.
   begin scalar l1,l2,w;
      l1 := length c1;
      l2 := length c2;
      w := if l1<l2 then
	 cl_qs1consens1(c1,c2,op)
      else
      	 cl_qs1consens1(c2,c1,op);
      return if w eq 'break then 'break else {w};
   end;

procedure cl_qs1consens1(c1,c2,op);
   begin scalar w,sc1;
      sc1 := c1;
      while sc1 and not(w) do <<
	 w := rl_qstrycons(car sc1,c1,c2,op);
	 sc1 := cdr sc1
      >>;
      return w
   end;

% Second variant: Multiple consensus of two claues allowed.

procedure cl_qsnconsens(c1,c2,op);
   % consens computation of n consensus. [c1] and [c2] are clausels.
   % Computes the unique determined consenus of [c1] and [c2] provided
   % it exists.
   begin scalar l1,l2;
      l1 := length c1;
      l2 := length c2;
      return if l1<l2 then
	 cl_qsnconsens1(c1,c2,op)
      else
      	 cl_qsnconsens1(c2,c1,op)
   end;

procedure cl_qsnconsens1(c1,c2,op);
   begin scalar w,r,sc1;
      sc1 := c1;
      while sc1 and w neq 'break do <<
	 w := rl_qstrycons(car sc1,c1,c2,op);
	 if w then
	    r := w . r;
	 sc1 := cdr sc1;
      >>;
      return if w eq 'break then 'break else r
   end;

%------------------------------------------------------------------------
% Generic Implementations of rl_qstrycons
%------------------------------------------------------------------------

procedure cl_qstrycons(a,c1,c2,op);
   % quine simplification try consensus. [a] is an atomic formula,
   % [c1] and [c2] are clauses, op is one of ['and], ['or]. Returns
   % [T], [nil] or [break].
   begin scalar na,sc1,r,cc1;
      cc1 := delete(a,c1);  % Copy... % TODO: delq or delete?
      na := rl_negateat a;
      if not(na member c2) then
	 return nil;
      sc1 := cc1;
      r := T;
      while sc1 and r do <<
	 if rl_negateat car sc1 member c2 then
	    r := nil;
	 sc1 := cdr sc1;
      >>;
      if not r then
	 return nil;
      r := sort(list2set append(cc1,delete(na,c2)),'rl_ordatp); %TODO: nconc
      if null r then
	 return 'break;
      return r
   end;

% ------------------------------------------------------------------------
% Enumeration of power set
% ------------------------------------------------------------------------

%DS
% <STATE> ::= l . [z_1,...,z_n]

% Der pointer $v_n$ steht immer vor $v_n-1$

procedure cl_ps(s);
   % [s] is a STATE. Returns an element of thepower set. Modifies [s].
   begin scalar v,w,r; integer i,n;
      v := cdr s;
      if cdr s eq 'final then
	 return 'final;      
      if null cdr s then
	 v := cdr s := mkvect (length car s-1);
      n := upbv v;
      while i<=n and (w:=getv(v,i)) do <<
	 r := car w . r;
	 i:=i+1;
      >>;      
      cl_psnext s;	    
      return  r
   end;

procedure cl_psnext(s);
   cl_psnext1(s,0);

procedure cl_psnext1(s,n);
   begin scalar w,v;
      v := cdr s;
      if n > upbv v then
	 return cdr s := 'final;  % Mark and return;
      w := getv(v,n);      
      if null w then  % Introduce pointer
	 return putv(v,n,car s);
      w := cdr w;  % Try to move      
      if w then  % Success
	 return putv(v,n,w);      
      % Overflow occurs.
      repeat <<
	 w := cl_psnext1(s,n+1);
	 if w neq 'final then
	    w := cdr getv(v,n+1);
      >> until w;
      if w eq 'final then
	 return 'final;
      putv(v,n,w)
   end;

endmodule;  % [clbnf]

end;  % of file
