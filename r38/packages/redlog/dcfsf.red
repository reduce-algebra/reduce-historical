% ----------------------------------------------------------------------
% $Id: dcfsf.red,v 1.1 2004/03/22 12:31:49 sturm Exp $
% ----------------------------------------------------------------------
% Copyright (c) 2004 Thomas Sturm
% ----------------------------------------------------------------------
% $Log: dcfsf.red,v $
% Revision 1.1  2004/03/22 12:31:49  sturm
% Initial check-in.
% Mostly copied from acfsf.
% Includes Diploma Thesis by Kacem plus wrapper for this.
%
% ----------------------------------------------------------------------
lisp <<
   fluid '(dcfsf_rcsid!* dcfsf_copyright!*);
   dcfsf_rcsid!* := "$Id: dcfsf.red,v 1.1 2004/03/22 12:31:49 sturm Exp $";
   dcfsf_copyright!* := "Copyright (c) 2004 T. Sturm"
>>;

module dcfsf;
% Diferentially closed field standard form. Main module. Algorithms on
% first-order formulas over diferentially closed fields. The language
% contains binary relations ['equal], ['neq], ring operations and a
% binary derivative operator ['d].

create!-package('(dcfsf dcfsfmisc dcfsfqe dcfsfkacem),nil);

load!-package 'rltools;
load!-package 'cl;
remflag('(load!-package),'eval);  % for bootstrapping
load!-package 'cgb;
flag('(load!-package),'eval);
load!-package 'acfsf;

exports dcfsf_simpterm,dcfsf_prepat,dcfsf_resimpat,dcfsf_lengthat,
   dcfsf_chsimpat,dcfsf_simpat,dcfsf_op,dcfsf_arg2l,dcfsf_arg2r,dcfsf_argn,
   dcfsf_mk2,dcfsf_0mk2,dcfsf_mkn,dcfsf_opp;

imports rltools,cl,cgb;

fluid '(!*rlsiatadv !*rlsiexpl !*rlsiexpla !*rlgssub !*rlsiso !*rlgsrad
   !*rlgsred !*rlgsprod !*rlgserf !*rlverbose !*rlsifac !*rlbnfsac !*rlgsvb
   !*rlgsbnf !*rlgsutord !*rlnzden !*rladdcond !*rlqegen !*cgbgen !*cgbreal
   !*gbverbose dcfsf_gstv!* !*cgbverbose !*groebopt);

flag('(dcfsf),'rl_package);

% Parameters
put('dcfsf,'rl_params,'(
   (rl_subat!* . dcfsf_subat)
   (rl_subalchk!* . dcfsf_subalchk)
   (rl_eqnrhskernels!* . dcfsf_eqnrhskernels)
   (rl_ordatp!* . dcfsf_ordatp)
   (rl_simplat1!* . acfsf_simplat1)
   (rl_smupdknowl!* . acfsf_smupdknowl)
   (rl_smrmknowl!* . acfsf_smrmknowl)
   (rl_smcpknowl!* . acfsf_smcpknowl)
   (rl_smmkatl!* . acfsf_smmkatl)
   (rl_smsimpl!-impl!* . cl_smsimpl!-impl)
   (rl_smsimpl!-equiv1!* . cl_smsimpl!-equiv1)
   (rl_negateat!* . dcfsf_negateat)
   (rl_varlat!* . dcfsf_varlat)
   (rl_varsubstat!* . dcfsf_varsubstat)
   (rl_subsumption!* . acfsf_subsumption)
   (rl_bnfsimpl!* . cl_bnfsimpl)
   (rl_sacat!* .acfsf_sacat)
   (rl_sacatlp!* . acfsf_sacatlp)
   (rl_fctrat!* . acfsf_fctrat)
   (rl_tordp!* . ordp)
   (rl_a2cdl!* . acfsf_a2cdl)
   (rl_t2cdl!* . acfsf_t2cdl)
   (rl_getineq!* . acfsf_getineq)
   (rl_structat!* . acfsf_structat)
   (rl_ifstructat!* . acfsf_ifstructat)
   (rl_termmlat!* . acfsf_termmlat)
   (rl_multsurep!* . acfsf_multsurep)
   (rl_fbqe!* . cl_fbqe)));

% Switches
put('dcfsf,'rl_cswitches,'(
   (rlsusi . nil)
));

% Services
put('dcfsf,'rl_services,'(
   (rl_subfof!* . cl_subfof)
   (rl_identifyonoff!* . cl_identifyonoff)
   (rl_simpl!* . cl_simpl)
   (rl_thsimpl!* . acfsf_thsimpl)
   (rl_nnf!* . cl_nnf)
   (rl_nnfnot!* . cl_nnfnot)
   (rl_pnf!* . cl_pnf)
   (rl_cnf!* . acfsf_cnf)
   (rl_dnf!* . acfsf_dnf)
   (rl_all!* . cl_all)
   (rl_ex!* . cl_ex)
   (rl_atnum!* . cl_atnum)
   (rl_tab!* . cl_tab)
   (rl_atab!* . cl_atab)
   (rl_itab!* . cl_itab)
   (rl_gsc!* . acfsf_gsc)
   (rl_gsd!* . acfsf_gsd)
   (rl_gsn!* . acfsf_gsn)
   (rl_ifacl!* . cl_ifacl)
   (rl_ifacml!* . cl_ifacml)
   (rl_matrix!* . cl_matrix)
   (rl_apnf!* . cl_apnf)
   (rl_atml!* . cl_atml)
   (rl_tnf!* . cl_tnf)
   (rl_atl!* . cl_atl)
   (rl_struct!* . cl_struct)
   (rl_ifstruct!* . cl_ifstruct)
   (rl_termml!* . cl_termml)
   (rl_terml!* . cl_terml)
   (rl_varl!* . cl_varl)
   (rl_fvarl!* . cl_fvarl)
   (rl_bvarl!* . cl_bvarl)
   (rl_gentheo!* . cl_gentheo)
   (rl_decdeg!* . acfsf_decdeg)
   (rl_decdeg1!* . acfsf_decdeg1)
   (rl_surep!* . cl_surep)
   (rl_qe!* . dcfsf_qe)
   (rl_qeipo!* . cl_qeipo)
   (rl_siaddatl!* . cl_siaddatl)));

% Admin
put('dcfsf,'simpfnname,'dcfsf_simpfn);
put('dcfsf,'simpdefault,'dcfsf_simprel);

put('dcfsf,'rl_prepat,'dcfsf_prepat);
put('dcfsf,'rl_resimpat,'dcfsf_resimpat);
put('dcfsf,'rl_lengthat,'dcfsf_lengthat);

put('dcfsf,'rl_prepterm,'prepf);
put('dcfsf,'rl_simpterm,'dcfsf_simpterm);

algebraic infix equal;
put('equal,'dcfsf_simpfn,'dcfsf_chsimpat);
put('equal,'number!-of!-args,2);

algebraic infix neq;
put('neq,'dcfsf_simpfn,'dcfsf_chsimpat);
put('neq,'number!-of!-args,2);
put('neq,'rtypefn,'quotelog);
newtok '((!< !>) neq);

algebraic infix d;
put('d,'number!-of!-args,2);
put('d,'simpfn,'dcfsf_simpd);
precedence d,**;

flag('(equal neq d),'spaced);
flag('(dcfsf_chsimpat),'full);

procedure dcfsf_simpterm(u);
   % Differentially closed field simp term. [u] is Lisp Prefix. Returns
   % the [u] as an DCFSF term.
   numr simp u;

procedure dcfsf_prepat(f);
   % Differentially closed field prep atomic formula. [f] is an DCFSF
   % atomic formula. Returns [f] in Lisp prefix form.
   {dcfsf_op f,prepf dcfsf_arg2l f,prepf dcfsf_arg2r f};

procedure dcfsf_resimpat(f);
   % Differentially closed field resimp atomic formula. [f] is an DCFSF
   % atomic formula. Returns the atomic formula [f] with resimplified
   % terms.
   dcfsf_mk2(dcfsf_op f,
      numr resimp !*f2q dcfsf_arg2l f,numr resimp !*f2q dcfsf_arg2r f);

procedure dcfsf_simpd(u);
   begin scalar v,n;
      if length u neq 2 then
	 rederr "dcfsf_simpd: d is infix with 2 arguments";
      v := car u;
      if not idp v then
	 rederr {"dcfsf_simpd:",v,"is not a variable"};
      n := cadr u;
      if not (numberp n and n >=0) then
	 rederr {"dcfsf_simpd:",n,"is not a natural number"};
      if eqn(n,0) then
	 return mksq(v,1);
      return mksq('d . u,1)
   end;

procedure dcfsf_lengthat(f);
   % Differentially closed field length of atomic formula. [f] is an
   % atomic formula. Returns a number, the length of [f].
   2;

procedure dcfsf_chsimpat(u);
   % Differentially closed field chain simp atomic formula. [u] is the
   % Lisp prefix representation of a chain of atomic formulas, i.e.,
   % the operators are nested right associatively. Returns a formula,
   % which is the corresponding conjunction.
   rl_smkn('and,for each x in dcfsf_chsimpat1 u collect dcfsf_simpat x);

procedure dcfsf_chsimpat1(u);
   % Differentially closed field chain simp atomic formula. [u] is the
   % Lisp prefix representation of a chain of atomic formulas, i.e.,
   % the operators are nested right associatively.
   begin scalar leftl,rightl,lhs,rhs;
      lhs := cadr u;
      if pairp lhs and dcfsf_opp car lhs then <<
	 leftl := dcfsf_chsimpat1 lhs;
	 lhs := caddr lastcar leftl
      >>;
      rhs := caddr u;
      if pairp rhs and dcfsf_opp car rhs then <<
	 rightl := dcfsf_chsimpat1 rhs;
	 rhs := cadr car rightl
      >>;
      return nconc(leftl,{car u,lhs,rhs} . rightl)
   end;

procedure dcfsf_simpat(u);
   % Differentially closed field simp atomic formula. [u] is Lisp
   % prefix. Returns [u] as an atomic formula.
   begin scalar op,lhs,rhs,nlhs,f;
      op := car u;
      lhs := simp cadr u;
      if not (!*rlnzden or (domainp denr lhs)) then
 	 typerr(u,"atomic formula");
      rhs := simp caddr u;
      if not (!*rlnzden or (domainp denr rhs)) then
 	 typerr(u,"atomic formula");
      lhs := subtrsq(lhs,rhs);
      nlhs := numr lhs;
      if !*rlnzden and not domainp denr lhs then <<
	 f := dcfsf_0mk2(op,nlhs);
	 if !*rladdcond then
	    f := rl_mkn('and,{dcfsf_0mk2('neq,denr lhs),f});
	 return f
      >>;
      return dcfsf_0mk2(op,nlhs)
   end;

procedure dcfsf_op(atf);
   % Differentially closed field operator. [atf] is an atomic formula
   % $R(t_1,t_2)$. Returns $R$.
   car atf;

procedure dcfsf_arg2l(atf);
   % Differentially closed field binary operator left hand side
   % argument. [atf] is an atomic formula $R(t_1,t_2)$. Returns $t_1$.
   cadr atf;

procedure dcfsf_arg2r(atf);
   % Differentially closed field binary operator right hand side
   % argument. [atf] is an atomic formula $R(t_1,t_2)$. Returns $t_2$.
   caddr atf;

procedure dcfsf_argn(atf);
   % Differentially closed field n-ary operator argument list. [atf] is
   % an atomic formula $R(t_1,t_2)$. Returns the list $(t_1,t_2)$.
   {cadr atf,caddr atf};

procedure dcfsf_mk2(op,lhs,rhs);
   % Differentially closed field make atomic formula for binary
   % operator. [op] is a relation; [lhs] and [rhs] are terms. Returns
   % the atomic formula $[op]([lhs],[rhs])$.
   {op,lhs,rhs};

procedure dcfsf_0mk2(op,lhs);
   % Differentially closed field make zero right hand side atomic
   % formula for binary operator. [op] is a relation [lhs] is a term.
   % Returns the atomic formula $[op]([lhs],0)$.
   {op,lhs,nil};

procedure dcfsf_mkn(op,argl);
   % Differentially closed field make atomic formula for n-ary
   % operator. [op] is a relation; [argl] is a list $(t_1,t_2)$ of
   % terms. Returns the atomic formula $[op](t_1,t_2)$.
   {op,car argl,cadr argl};

procedure dcfsf_opp(op);
   % Differentially closed field operator predicate. [op] is an
   % S-expression. Returns non-[nil] iff op is a relation.
   op memq '(equal neq);

endmodule;  % [dcfsf]

end;  % of file
