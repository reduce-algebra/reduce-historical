module driver;  % Driving routines for integration program.

% Author: Mary Ann Moore and Arthur C. Norman.
% Modifications by: John P. Fitch, David Hartley, Francis J. Wright.

fluid '(!*algint
	!*backtrace
	!*exp
	% !*failhard
	!*gcd
	!*intflag!*
	!*keepsqrts
	!*limitedfactors
	!*mcd
        !*noncomp
	!*nolnr
	!*partialintdf
	!*precise
	!*purerisch
	!*rationalize
	!*structure
	!*trdint
	!*trint
	!*uncached
	basic!-listofnewsqrts
	basic!-listofallsqrts
	gaussiani
	intvar
	kord!*
	listofnewsqrts
	listofallsqrts
	loglist
	powlis!*
	sqrt!-intvar
	sqrt!-places!-alist
	subfg!*
	varlist
	varstack!*
	xlogs
	zlist);

exports integratesq,simpint,simpint1;

imports algebraiccase,algfnpl,findzvars,getvariables,interr,printsq,
  transcendentalcase,varsinlist,kernp,simpcar,prepsq,mksq,simp,
   opmtch,formlnr;

switch algint,nolnr,trdint,trint;
switch hyperbolic;

% Form is   int(expr,var,x1,x2,...);
% meaning is integrate expr wrt var, given that the result may
% contain logs of x1,x2,...
% x1, etc are intended for use when the system has to be helped
% in the case that expr is algebraic.
% Extended arguments x1, x2, etc., are not currently supported.

symbolic procedure simpint u;
   % Simplifies an integral.  First two components of U are the integrand
   % and integration variable respectively.  Optional succeeding
   % components are log forms for the final integral.
   if atom u or null cdr u or cddr u and (null cdddr u or cddddr u)
     then rerror(int,1,"Improper number of arguments to INT")
    else if cddr u then simpdint u
%    then if getd 'simpdint then simpdint u
%          else rerror(int,2,"Improper number of arguments to INT")
    else begin scalar ans,dmod,expression,variable,loglist,oldvarstack,
		 !*intflag!*,!*purerisch,cflag,intvar,listofnewsqrts,
		 listofallsqrts,sqrtfn,sqrt!-intvar,sqrt!-places!-alist,
		 basic!-listofallsqrts,basic!-listofnewsqrts,coefft,
		 varchange,w;
    !*intflag!* := t;     % Shows we are in integrator.
    variable := !*a2k cadr u;
    if not(idp variable or pairp variable and numlistp cdr variable)
%     then typerr(variable,"integration variable");
      then <<varchange := variable . intern gensym();
	     if !*trint
	       then printc {"Integration kernel", variable,
			  "replaced by simple variable", cdr varchange};
	     variable := cdr varchange>>;
    intvar := variable;   % Used in SIMPSQRT and algebraic integrator.
    w := cddr u;
    if w then rerror(int,3,"Too many arguments to INT");
    listofnewsqrts:= list mvar gaussiani; % Initialize for SIMPSQRT.
    listofallsqrts:= list (argof mvar gaussiani . gaussiani);
    sqrtfn := get('sqrt,'simpfn);
    put('sqrt,'simpfn,'proper!-simpsqrt);
    % We need explicit settings of several switches during integral
    % evaluation.  In addition, the current code cannot handle domains
    % like floating point, so we suppress it while the integral is
    % calculated.  UNCACHED is turned on since integrator does its own
    % caching.
    % Any changes made to these settings must also be made in wstrass.
    if dmode!* then
       << % added by Alan Barnes
	  if (cflag:=get(dmode!*, 'cmpxfn)) then onoff('complex, nil);
	  if (dmod := get(dmode!*,'dname)) then
	     onoff(dmod,nil)>> where !*msg := nil;
    begin scalar dmode!*,!*exp,!*gcd,!*keepsqrts,!*limitedfactors,!*mcd,
		 !*rationalize,!*structure,!*uncached,kord!*,
		 ans1,denexp,badbit,nexp,oneterm,!*precise;
       !*keepsqrts := !*limitedfactors := t;     % !*sqrt := t;
       !*exp := !*gcd := !*mcd := !*structure := !*uncached := t;
       dmode!* := nil;
       if !*algint
	 then <<
	    % The algint code now needs precise off.
%           !*precise := t;
	    % Start a clean slate (in terms of SQRTSAVE) for this
	    % integral.
	    sqrt!-intvar:=!*q2f simpsqrti variable;
	    if (red sqrt!-intvar) or (lc sqrt!-intvar neq 1)
		or (ldeg sqrt!-intvar neq 1)
	      then interr "Sqrt(x) not properly formed"
	      else sqrt!-intvar:=mvar sqrt!-intvar;
	    basic!-listofallsqrts:=listofallsqrts;
	    basic!-listofnewsqrts:=listofnewsqrts;
	    sqrtsave(basic!-listofallsqrts,basic!-listofnewsqrts,
			 list(variable . variable))>>;
       coefft := (1 ./ 1);           % Collect simple coefficients.
       expression := int!-simp car u;
       if varchange
	 then <<depend1(car varchange,cdr varchange,t);
		expression := int!-subsq(expression,{varchange})>>;
       denexp := 1 ./ denr expression;          % Get into two bits
       nexp := numr expression;
       while not atom nexp and null cdr nexp and
	  not depends(mvar nexp,variable) do
	      <<coefft := multsq(coefft,(((caar nexp) . 1) . nil) ./ 1);
		nexp := lc nexp>>;
       ans1 := nil;
       while nexp do begin              % Collect by zvariables
	   scalar x,zv,tmp;
	   if atom nexp then << x:=!*f2q nexp; nexp:=nil >>
	   else << x:=!*t2q car nexp; nexp:=cdr nexp >>;
	   x := multsq(x,denexp);
	   zv := findzvars(getvariables x,list variable,variable,nil);
	   begin scalar oldzlist;
	       while oldzlist neq zv do <<
		    oldzlist := zv;
		    foreach zz in oldzlist do
		    zv:=findzvars(distexp(pseudodiff(zz,variable)),
				  zv,variable,t)>>;
	       % The following line was added to make, for example,
	       % int(df(sin(x)/x),x) return the expected result.
	       zv := sort(zv, function ordp)
	   end;
	   tmp := ans1;
	   while tmp do
	      <<if zv=caar tmp
		  then <<rplacd(car tmp,addsq(cdar tmp,x));
			 tmp := nil; zv := nil>>
		 else tmp := cdr tmp>>;
	   if zv then ans1 := (zv . x) . ans1
       end;
       if length ans1 = 1 then oneterm := t; % Efficiency
       nexp := ans1;
       ans := nil ./ 1;
       badbit:=nil ./ 1;                        % SQ zero
       while nexp do                            % Run down the terms
	<<u := cdar nexp;
	  if !*trdint
	    then <<princ "Integrate"; printsq u;
		   princ "with Zvars "; print caar nexp>>;
	  ans1 := errorset!*(list('integratesq,mkquote u,
			     mkquote variable,mkquote loglist,
			     mkquote caar nexp),
			     !*backtrace);
	  nexp := cdr nexp;
	  if errorp ans1 then badbit := addsq(badbit,u)
	   else <<ans := addsq(caar ans1, ans);
		  badbit:=addsq(cdar ans1,badbit)>>>>;
       if !*trdint
	 then <<prin2 "Partial answer="; printsq ans;
		prin2 "To do="; printsq badbit>>;
       % We have run down the terms.  If there are any bad bits, redo
       % them.  However, since a non-zero badbit implies that
       % integratesq aborted, the internal variable order may be
       % confused.  So we reset kord!* and reorder expressions in this
       % case.
       if badbit neq '(nil . 1)
	 then <<setkorder nil;
		badbit := reordsq badbit;
		ans := reordsq ans;
		coefft := reordsq coefft;
	  if !*trdint then <<princ "Retrying..."; printsq badbit>>;
	  if oneterm and ans = '(nil . 1) then ans1 := nil
	    else ans1 := errorset!*(list('integratesq,mkquote badbit,
				  mkquote variable,mkquote loglist,nil),
				  !*backtrace);
	  if null ans1 or errorp ans1
	    then ans := addsq(ans,simpint1(badbit . variable . w))
	   else <<ans := addsq(ans,caar ans1);

	      %% FJW: It is possible for ans here to be just a
	      %% spurious constant term, in which case we discard it.
	      if not smemq(variable, ans) then ans := nil ./ 1;
	      %% This may not be the best place for this fix, but I
	      %% don't see how it can ever do any harm.  [I don't
	      %% think we need a full depend test here.]

		  if cdar ans1 neq '(nil . 1)
		    then ans := addsq(ans,
				    simpint1(cdar ans1 . variable . w))
		>>>>;
    end;
    ans := multsq(coefft,ans); %Put back coefficient, preserving order.
%    if errorp ans
%      then return <<put('sqrt,'simpfn,sqrtfn);
%                    if !*failhard then error1();
%                    simpint1(expression . variable . w)>>
%     else ans := car ans;
%   expression := sqrtchk numr ans ./ sqrtchk denr ans;
    if !*trdint then << printc "Resimp and all that"; printsq ans >>;
    % We now need to check that all simplifications have been done
    % but we have to make sure INT is not resimplified, and that SIMP
    % does not complain at getting the same argument again.
    put('int,'simpfn,'simpiden);
    put('sqrt,'simpfn,sqrtfn);
    << if dmod then onoff(dmod,t);
       % added by Alan Barnes
       if cflag then onoff('complex,t)>> where !*msg := nil;
    oldvarstack := varstack!*;
    varstack!* := nil;
%   ans := errorset!*(list('resimp,mkquote ans),t);
    ans := errorset!*(list('int!-resub,mkquote ans,mkquote
			   varchange),t);
    put('int,'simpfn,'simpint);
    varstack!* := oldvarstack;
    return if errorp ans then error1() else car ans
   end;

symbolic procedure int!-resub(x,v);
   % {sq,alist} -> sq
   % Undo any variable change and resimplify.
   if v then <<x := int!-subsq(x,{revpr v}); depend1(car v,cdr v,nil);
	       resimp x>>
    else resimp x;

symbolic procedure int!-subsq(x,v);
   % {sq,alist} -> sq
   % A version of subsq with the int and df operators unprotected.
   % Intended for straightforward change of variable names only.
   begin scalar subfuncs,subfg!*;
      subfuncs := {remprop('df,'subfunc),remprop('int,'subfunc)};
      x := subsq(x,v);
      put('df,'subfunc,car subfuncs);
      put('int,'subfunc,cadr subfuncs);
      return x
   end;

symbolic procedure numlistp u;
   % True if u is a list of numbers.
   null u or numberp car u and numlistp cdr u;

% symbolic procedure sqrtchk u;
%    % U is a standard form. Result is another standard form with square
%    % roots replaced by half powers.
%    if domainp u then u
%     else if not eqcar(mvar u,'sqrt)
%      then addf(multpf(lpow u,sqrtchk lc u),sqrtchk red u)
% %   else if mvar u = '(sqrt -1)
% %    then addf(multpf(mksp('i,ldeg u),sqrtchk lc u),sqrtchk red u)
%     else addf(multpf(mksp(list('expt,cadr mvar u,'(quotient 1 2)),
%                           ldeg u),
%                      sqrtchk lc u),
%               sqrtchk red u);

symbolic procedure int!-simp u;
   % Converts U to canonical form, including the resimplification of
   % *sq forms.
   subs2 resimp simp!* u;

put('int,'simpfn,'simpint);

symbolic procedure integratesq(integrand,var,xlogs,zv);
 begin scalar varlist,x,zlist,!*noncomp;
    if !*trint then <<
	printc "Start of Integration; integrand is ";
	printsq integrand >>;
    !*noncomp := noncomfp numr integrand 
                    or noncomfp denr integrand;
    varlist:=getvariables integrand;
    varlist:=varsinlist(xlogs,varlist); %in case more exist in xlogs
    if zv then zlist := zv
     else <<zlist := findzvars(varlist,list var,var,nil);
	    % Important kernels.
    % The next section causes problems with nested exponentials or logs.
	    begin scalar oldzlist;
		while oldzlist neq zlist do
		 <<oldzlist := zlist;
		   foreach zz in oldzlist do
		   zlist := findzvars(distexp(pseudodiff(zz,var)),
					zlist,var,t)>>
	    end>>;
    if !*trint  then <<
      printc "Determination of the differential field descriptor";
      printc "gives the functions:";
      print zlist >>;
%% Look for rational powers in the descriptor
%% If there is make a suitable transformation and do the sub integral
%% and return the revised integral
    x := look_for_substitute(integrand, var, zlist);
    if x then return x;
%% End of rational patch
    if !*purerisch and not allowedfns zlist
      then return (nil ./ 1) . integrand;
      % If it is not suitable for Risch.
    varlist := setdiff(varlist,zlist);
%   varlist := purge(zlist,varlist);
    % Now zlist is list of things that depend on x, and varlist is list
    % of constant kernels in integrand.
    if !*algint and cdr zlist and algfnpl(zlist,var)
      then return algebraiccase(integrand,zlist,varlist)
     else return transcendentalcase(integrand,var,xlogs,zlist,varlist)
 end;

symbolic procedure distexp(l);
    if null l then nil
    else if atom car l then car l . distexp cdr l
    else if (caar l = 'expt) and (cadar l = 'e) then
	begin scalar ll;
	    ll:=caddr car l;
	    if eqcar(ll,'plus) then <<
		ll:=foreach x in cdr ll collect list('expt,'e,x);
		return ('times . ll) . distexp cdr l >>
	    else return car l . distexp cdr l
	end
    else distexp car l . distexp cdr l;

symbolic procedure pseudodiff(a,var);
    if atom a then      % **** Treat diffs correctly??
	if depends(a,var) then list prepsq simpdf(list(a,var)) else nil
    else if car a
	       memq '(atan equal log plus quotient sqrt times minus)
	then begin scalar aa,bb;
	    foreach zz in cdr a do <<
		bb:=pseudodiff(zz,var);
		aa:= union(bb,aa) >>;
	    return aa
	end
      else if car a eq 'expt
	then if depends(cadr a,var) then
	    if depends(caddr a,var) then
		prepsq simp list('log,cadr a) . %% a(x)^b(x)
		cadr a .
		caddr a .
		union(pseudodiff(cadr a,var),pseudodiff(caddr a,var))
	    else cadr a . pseudodiff(cadr a,var)        %% a(x)^b
	else caddr a . pseudodiff(caddr a,var)          %% a^b(x)
    else list prepsq simpdf(list(a,var));

symbolic procedure look_for_substitute(integrand, var, zz);
% Search for rational power transformations
begin
  scalar res;
  if atom zz then return nil
  else if (res := look_for_rational(integrand, var, zz)) then return res
  else if (res := look_for_quad(integrand, var, zz)) then return res
  else if (res := look_for_substitute(integrand, var, car zz))
   then return res
  else return look_for_substitute(integrand, var, cdr zz)
end;

symbolic procedure look_for_rational(integrand, var, zz);
% Look for a form x^(n/m) in the field descriptor, and transform
% the integral if it is found.  Note that the sqrt form may be used
% as well as exponentials.  Return nil if no transformation
  if (car zz = 'sqrt and cadr zz = var) then
	look_for_rational1(integrand, var, 2)
  else if (car zz = 'expt) and (cadr zz = var) and
     (listp caddr zz) and (caaddr zz = 'quotient) and
     (numberp cadr caddr zz) and (numberp caddr caddr zz) then
		look_for_rational1(integrand, var, caddr caddr zz)
  else nil;

symbolic procedure look_for_rational1(integrand, var, m);
% Actually do the transformation and integral
begin
	scalar newvar, res, ss, mn2m!-1;
	newvar := gensym();
	mn2m!-1 := !*f2q(((newvar .** (m-1)) .* m) .+ nil);
%%      print ("Integrand was " . integrand);
% x => y^m, and dx => m y^(m-1)
	integrand := multsq(subsq(integrand,
				  list(var . list('expt,newvar,m))),
			    mn2m!-1);
	if !*trint then <<
	    prin2 "Integrand is transformed to ";
	    printsq integrand
	>>;
	begin scalar intvar;
	    intvar := newvar;   % To circumvent an algint bug.
	res := integratesq(integrand, newvar, nil, nil);
	end;
	ss := list(newvar . list('expt,var, list('quotient, 1, m)));
	res := subsq(car res, ss) .
	       subsq(quotsq(cdr res, mn2m!-1), ss);
	if !*trint then <<
	    printc "Transforming back...";
	    printsq car res;
	    prin2 " plus a bad part of ";
	    printsq cdr res
	>>;
	return res
 end;

symbolic procedure look_for_quad(integrand, var, zz);
% Look for a form sqrt(a+bx+cx^2) in the field descriptor
% and transform to the appropriate asin, acosh or asinh.
% Return nil if no transformation found
if !*algint then nil % as Algint does it better??
else begin
  if (car zz = 'sqrt and listp cadr zz and caadr zz = 'plus) or
     (car zz = 'expt and listp cadr zz and caadr zz = 'plus and
      listp caddr zz and car caddr zz = 'quotient
         and fixp caddr caddr zz)
   then <<
    zz := simp cadr zz;
    if (cdr zz = 1) then <<
        zz := cdr coeff1(prepsq zz, var, nil);
        if length zz = 2 then return begin      % Linear
          scalar a, b;
          scalar nvar, res, ss;
          a := car zz; b := cadr zz;
          if (depends(a,var) or depends(b,var)) then return nil;
          nvar := gensym();
          if !*trint then <<
                prin2 "Linear shift suggested ";
                prin2 a; prin2 " "; prin2 b; terpri();
          >>;
          integrand := subsq(integrand,         % Make the substitution
                             list(var . list('quotient,
                                             list('difference,
                                                  list('expt,nvar,2),a),
                                                  b)));
          integrand := multsq(integrand,        % and the dx component
                              simp list('quotient,list('times,nvar,2),
                                        b));
%         integrand := subsq(integrand,
%                             list(var . list('difference, nvar, a)));
%         integrand := multsq(integrand, simp b);
          if !*trint then <<
                prin2 "Integrand is transformed by substitution to ";
                printsq integrand;
                prin2 "using substitution "; prin2 var; prin2 " -> ";
                printsq simp list('quotient,
                                 list('difference,list('expt,nvar,2),a),
                                 b);
           >>;
           res := integratesq(integrand, nvar, nil, nil);
           ss := list(nvar . list('sqrt,list('plus,list('times,var,b),
                                  a)));
           res := subsq(car res, ss) .
                  subsq(multsq(cdr res, simp list('quotient,b,
                                                  list('times,nvar,2))), ss);
        %% Should one reject if there is a bad bit??
           return res;
        end
        else if length zz = 3 then return begin % A quadratic
          scalar a, b, c;
          a := car zz; b := cadr zz; c:= caddr zz;
          if (depends(a,var) or depends(b,var) or depends(c,var)) then
                return nil;
          a := simp list('difference, a,        % Re-centre
                         list('times,b,b,
                         list('quotient,1,list('times,4,c))));
	  if null numr a then return nil;   % Power occurred.
          b := simp list('quotient, b, list('times, 2, c));
          c := simp c;
          return
           if minusf numr c then <<
           if minusf numr a then  begin
                            scalar !*hyperbolic;
                            !*hyperbolic := t;
                            return
                                look_for_invhyp(integrand,nil,var,a,b,c)
                        end
            else                look_for_asin(integrand,var,a,b,c)>>
          else <<
            if minusf numr a then look_for_invhyp(integrand,t,var,a,b,c)
          else                  look_for_invhyp(integrand,nil,var,a,b,c)
          >>
        end
        else if length zz = 5 then return begin % A quartic
          scalar a, b, c, d, e, nn, dd, mm;
          a := car zz; b := cadr zz; c:= caddr zz;
          d := cadddr zz; e := car cddddr zz;
          if not(b = 0) or not(d = 0) then return nil;
          if (depends(a,var) or depends(c,var)) or depends(e,var) then
                return nil;
          nn := numr integrand;  dd := denr integrand;
          if denr(mm :=quotsq(nn ./ 1, !*kk2q var)) = 1 and
             even_power(numr mm, var) and even_power(dd, var) then <<
        % substitute x -> sqrt(y)
              return sqrt_substitute(numr mm, dd, var);
          >>;
          if denr(mm :=quotsq(dd ./ 1, !*kk2q var)) = 1 and
             even_power(nn, var) and even_power(numr mm, var) then <<
        % substitute x -> sqrt(y)
              return sqrt_substitute(nn, multf(dd,!*kk2f var), var);
          >>;
          return nil;
        end;
  >>>>;
  return nil;
end;

symbolic procedure look_for_asin(integrand, var, a, b, c);
% Actually do the transformation and integral
begin
    scalar newvar, res, ss, sqmn, onemth, m, n;
    m := prepsq a;
    n := prepsq c;
    b := prepsq b;
    newvar := gensym();
    sqmn := prepsq apply1(get('sqrt, 'simpfn),
			  list list('quotient, list('minus,n), m));
    onemth := list('cos, newvar);
    ss := list('sin, newvar);
    powlis!* := list(ss, 2, '(nil . t),
		     list('difference,1,list('expt,onemth,2)),
		     nil) .
		powlis!*;
    integrand := subs2q
	multsq(subsq(integrand,
		     list(var . list('difference,
				     list('quotient,ss,sqmn), b))),
	       quotsq(onemth := simp onemth, simp sqmn));
    if !*trint then <<
	prin2 "Integrand is transformed by substitution to ";
	printsq integrand;
	prin2 "using substitution "; prin2 var; prin2 " -> ";
	printsq simp list('difference, list('quotient, ss, sqmn), b);
    >>;
    res := integratesq(integrand, newvar, nil, nil);
    powlis!* := cdr powlis!*;
    ss:= list(newvar . list('asin,list('times,list('plus,var,b),sqmn)));
    res := subsq(car res, ss) . subsq(quotsq(cdr res, onemth), ss);
    if !*trint then <<
	printc "Transforming back...";
	printsq car res;
	prin2 " plus a bad part of ";
	printsq cdr res
    >>;
    if (car res = '(nil . 1)) then return nil;
    return res;
 end;

symbolic procedure look_for_invhyp(integrand, do_acosh, var, a, b, c);
% Actually do the transformation and integral; uses acosh/asinh form
% depending on second argument
begin
    scalar newvar, res, ss, sqmn, onemth, m, n, realdom;
    m := prepsq a;
    n := prepsq c;
    b := prepsq b;
    newvar := gensym();
    if do_acosh then <<
      sqmn := prepsq apply1(get('sqrt, 'simpfn),
                            list list('quotient, n, list('minus, m)));
      onemth := list('sinh, newvar);
      ss := list('cosh, newvar)
    >>
    else <<
      sqmn:= prepsq apply1(get('sqrt,'simpfn),list list('quotient,n,m));
      onemth := list('cosh, newvar);
      ss := list('sinh, newvar)
    >>;
    powlis!* := list(ss, 2, '(nil . t),
                     list((if do_acosh then 'plus else 'difference),
                          list('expt, onemth, 2),1),
                     nil) .
                powlis!*;
%   print ("sqmn" . sqmn); print("onemth" . onemth); print ("ss" . ss);
%   print cdddar powlis!*;
    integrand := subs2q
        multsq(subsq(integrand,
               list(var . list('difference,list('quotient,ss,sqmn),b))),
               quotsq(onemth := simp onemth, simp sqmn));
    if !*trint then <<
        prin2 "Integrand is transformed by substitution to ";
        printsq integrand;
        prin2 "using substitution "; prin2 var; prin2 " -> ";
        printsq simp list('difference, list('quotient, ss, sqmn), b);
    >>;
    realdom := not smember('(sqrt -1),integrand);
%   print integrand; print realdom;
    res := integratesq(integrand, newvar, nil, nil);
    powlis!* := cdr powlis!*;
    if !*hyperbolic then <<
      ss := list(if do_acosh then 'acosh else 'asinh,
                 list('times,list('plus,var,b), sqmn));
    >>
    else <<
      ss := list('times,list('plus,var,b), sqmn);
      ss := if do_acosh then
        subst(ss,'ss,
              '(log (plus ss (sqrt (difference (times ss ss) 1)))))
      else
        subst(ss,'ss,'(log (plus ss (sqrt (plus (times ss ss) 1)))))
    >>;
    ss := list(newvar . ss);
    res := sqrt2top subsq(car res, ss) .
           sqrt2top subsq(quotsq(cdr res, onemth), ss);
    if !*trint then <<
        printc "Transforming back...";
        printsq car res;
        prin2 " plus a bad part of ";
        printsq cdr res
    >>;
    if (car res = '(nil . 1)) then return nil;
    if realdom and smember('(sqrt -1),res) then <<
	if !*trint then print "Wrong sheet"; return nil;  % Wrong sheet?
    >>;
    return res
end;

symbolic procedure simpint1 u;
   % Varstack* rebound, since FORMLNR use can create recursive
   % evaluations.  (E.g., with int(cos(x)/x**2,x)).
   begin scalar !*keepsqrts,v,varstack!*;
      u := 'int . prepsq car u . cdr u;
      if (v := formlnr u) neq u
	then if !*nolnr
	       then <<v := simp subst('int!*,'int,v);
		      return remakesf numr v ./ remakesf denr v>>
	      else <<!*nolnr := nil . !*nolnr;
		     v:=errorset!*(list('simp,mkquote v),!*backtrace);
		     if pairp v then v := car v else v := simp u;
		     !*nolnr := cdr !*nolnr;
		     return v>>;
      return if (v := opmtch u) then simp v
      else symint u                     % FJW: symbolic integral
   end;

mkop 'int!*;

put('int!*,'simpfn,'simpint!*);

symbolic procedure simpint!* u;
   begin scalar x;
      return if (x := opmtch('int . u)) then simp x
	      else simpiden('int!* . u)
   end;

symbolic procedure remakesf u;
   %remakes standard form U, substituting operator INT for INT!*;
   if domainp u then u
    else addf(multpf(if eqcar(mvar u,'int!*)
		       then mksp('int . cdr mvar u,ldeg u)
		      else lpow u,remakesf lc u),
	       remakesf red u);

symbolic procedure allowedfns u;
   if null u then t
     else if atom car u then (car u=intvar) or not depends(car u,intvar)
     else if (caar u = 'expt and not (cadar u = 'e)
	and not depends(cadar u, intvar)
	and depends(caddar u, intvar)) then nil
     else if flagp(caar u,'transcendental) then allowedfns cdr u
    else nil;

symbolic procedure look_for_power(integrand, var);
begin
    scalar nn, dd, mm;
    nn := numr integrand;  dd := denr integrand;
    if denr(mm :=quotsq(nn ./ 1, !*kk2q var)) = 1 and
       even_power(numr mm, var) and even_power(dd, var) then <<
	% substitute x -> sqrt(y)
	return sqrt_substitute(numr mm, dd, var);
    >>;
    if denr(mm :=quotsq(dd ./ 1, !*kk2q var)) = 1 and
       even_power(nn, var) and even_power(numr mm, var) then <<
	% substitute x -> sqrt(y)
	return sqrt_substitute(nn, numr mm, var);
    >>;
    return nil;
end;

symbolic procedure even_power(xpr, var);
  if atom xpr then t
  else if mvar xpr = var then <<
    if evenp pdeg lpow xpr then even_power(lc xpr, var) and
				even_power(red xpr, var)
    else nil >>
  else if eqcar(mvar xpr, 'expt) and
	  cadr mvar xpr = var and
	  evenp caddr mvar xpr then t
  else if atom mvar xpr then
	  even_power(lc xpr, var) and even_power(red xpr, var)
  else if even_power(red xpr, var) and even_power(lc xpr, var) then
	even_prep(mvar xpr, var);

symbolic procedure even_prep(xpr,var);
if xpr = var then nil
else if atom xpr then t
else if eqcar(xpr, 'expt) and cadr xpr = var and evenp caddr xpr then t
else if even_prep(car xpr, var) then even_prep(cdr xpr, var);


symbolic procedure sqrt_substitute(nn, dd, var);
begin
    scalar newvar, integrand, res, ss, !*keepsqrts;
    newvar := gensym();
    integrand := subst(list('sqrt,newvar), var,
		       list('quotient, prepsq (nn ./ dd), 2));
    integrand := prepsq simp integrand;
    integrand := simp integrand;
    begin scalar intvar;
	intvar := newvar;   % To circumvent an algint bug/oddity
	res := integratesq(integrand, newvar, nil, nil);
    end;
    ss := list(newvar . list('expt, var, 2));
    res := subsq(car res, ss) . multsq((((var .^ 1) .* 2) .+ nil) ./ 1,
				       subsq(cdr res, ss));
    if !*trint then <<
	printc "Transforming back...";
	printsq car res;
	prin2 " plus a bad part of ";
	printsq cdr res
    >>;
    return res
end;

% The following rules probably belong in other places.

%-----------------------------------------------------------------------
algebraic;

operator ci,si;  % ei.
% FJW: ci,si also defined in specfn(sfint.red), so ...
symbolic((algebraic operator ci,si) where !*msg=nil);

intrules :=
  {e^(~n*acosh(~x)) => (sqrt(x^2-1)+x)^n when numberp n,
   e^(~n*asinh(~x)) => (sqrt(x^2+1)+x)^n when numberp n,
   e^(acosh(~x)) => (sqrt(x^2-1)+x),
   e^(asinh(~x)) => (sqrt(x^2+1)+x),
   cosh(log(~x)) => (x^2+1)/(2*x),
   sinh(log(~x)) => (x^2-1)/(2*x),
   % These next two are rather uncertain.
   int(log(~x)/(~b-x),x) => dilog(x/b),
   int(log(~x)/(~b*x-x^2),x) => dilog(x/b)/b + log(x)^2/(2b),

%% FJW: Next 2 rules replaced by ~~ rules below
%% int(e^(~x^2),x) => erf(i*x)*sqrt(pi)/(2i),
%% int(1/e^(~x^2),x) => erf(x) * sqrt(pi)/2,
%% FJW: Missing sqrt(b):
%% int(e^(~b*~x^2),x) => erf(i*x)*sqrt(pi)/(2i*sqrt(b)),
   int(e^(~~b*~x^2),x) => erf(i*sqrt(b)*x)*sqrt(pi)/(2i*sqrt(b)),
%% FJW: Rule missing:
   int(e^(~x^2/~b),x) => erf(i*x/sqrt(b))*sqrt(pi)*sqrt(b)/(2i),
%% FJW: Missing sqrt(b):
%% int(1/e^(~b*~x^2),x) => erf(x)*sqrt(pi)/(2sqrt(b)),
   int(1/e^(~~b*~x^2),x) => erf(sqrt(b)*x)*sqrt(pi)/(2sqrt(b)),
%% FJW: Rule missing:
   int(1/e^(~x^2/~b),x) => erf(x/sqrt(b))*sqrt(pi)*sqrt(b)/2,

   df(ei(~x),x) => exp(x)/x,
   int(e^(~~b*~x)/x,x) => ei(b*x),      % FJW
   int(e^(~x/~b)/x,x) => ei(x/b),
   int(1/(exp(~x*~~b)*x),x) => ei(-x*b), % FJW
   int(1/(exp(~x/~b)*x),x) => ei(-x/b),
%% FJW: Next 2 rules replaced by ~~ rules above
%% int(e^~x/x,x) => ei(x),
%% int(1/(e^~x*x),x) => ei(-x),
   int(~a^~x/x,x) => ei(x*log(a)),
   int(1/((~a^~x)*x),x) => ei(-x*log(a)),
   df(si(~x),x) => sin(x)/x,
   int(sin(~~b*~x)/x,x) => si(b*x),     % FJW
   int(sin(~x/~b)/x,x) => si(x/b),      % FJW
%% int(sin(~x)/x,x) => si(x),           % FJW
   int(sin(~x)/x^2,x) => -sin(x)/x +ci(x),
   int(sin(~x)^2/x,x) =>(log(x)-ci(2x))/2,
   df(ci(~x),x) => cos(x)/x,
   int(cos(~~b*~x)/x,x) => ci(b*x),     % FJW
   int(cos(~x/~b)/x,x) => ci(x/b),      % FJW
%% int(cos(~x)/x,x) => ci(x),           % FJW
   int(cos(~x)/x^2,x) => -cos(x)/x -si(x),
   int(cos(~x)^2/x,x) =>(log(x)+ci(2x)/2),
   int(1/log(~~b*~x),x) => ei(log(b*x))/b, % FJW
   int(1/log(~x/~b),x) => ei(log(x/b))*b, % FJW
%% int(1/log(~x),x) => ei(log(x)),      % FJW
%% int(1/log(~x+~b),x) => ei(log(x+b)), % FJW
   int(1/log(~~a*~x+~b),x) => ei(log(a*x+b))/b, % FJW
   int(1/log(~x/~a+~b),x) => ei(log(x/a+b))/b, % FJW
   int(~x/log(~x),x) => ei(2*log(x)),
   int(~x^~n/log(x),x) => ei((n+1)*log(x)) when fixp n,
   int(1/(~x^~n*log(x)),x) => ei((-n+1)*log(x)) when fixp n};

let intrules;

endmodule;

end;
