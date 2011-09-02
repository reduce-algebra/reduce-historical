module sqprint;   % Routines for printing standard forms and quotients.

% Author: Anthony C. Hearn.

% Copyright (c) 1996 RAND.  All rights reserved.

% Modified by A. C. Norman, 1987.

fluid  '(!*fort
	 !*horner
	 !*nat
	 !*nero
	 !*pri
	 !*prin!#
         overflowed!*
         orig!*
	 outputhandler!*
         posn!*
	 testing!-width!*
         ycoord!*
         ymax!*
	 ymin!*
	 wtl!*);

testing!-width!* := overflowed!* := nil;

global '(!*eraise);

switch horner;

% When nat is enabled I use some programmable characters to
% draw pi, fraction bars and integral signs. (symbol 's) returns
% a character-object, and I use
%   .pi         pi
%   bar         solid horizontal bar                    -
%   int-top     top hook of integral sign               /
%   int-mid     vertical mid-stroke of integral sign    |
%   int-low     lower hook of integral sign             /
%   d           curly-d for use with integral display   d
%   sqrt        square root sign                        sqrt
%   sum-top     ---
%   sum-mid     >              for summation
%   sum-low     ---
%   prod-top             ---
%   prod-mid             | |   for products
%   prod-low             | |
%   infinity    infinity sign
%   mat!-top!-l     /          for display of matrices
%   mat!-top!-r     \
%   mat!-low!-l     \
%   mat!-low!-r     /
%   vbar            |


symbolic procedure !*sqprint u; sqprint cadr u;

put('!*sq,'prifn,'!*sqprint);

symbolic procedure printsq u; <<terpri!* t; sqprint u; terpri!* u; u>>;

symbolic procedure sqprint u;
   % Mathprints the standard quotient u.
   begin scalar flg,z,!*prin!#;
	!*prin!# := t;
        z := orig!*;
        if !*nat and posn!*<20 then orig!* := posn!*;
	if !*pri or wtl!* then maprin prepreform prepsq!* sqhorner!* u
	 else if cdr u neq 1
	   then <<flg := not domainp numr u and red numr u;
		  xprinf(car u,flg,nil);
		  prin2!* " / ";
		  flg := not domainp denr u
			     and (red denr u or lc denr u neq 1);
		  xprinf(cdr u,flg,nil)>>
	 else xprinf2 car u;
        return (orig!* := z)
   end;

symbolic procedure prepreform u;
   % U is an algebraic expression prepared for output by prepsq*.
   % Reform inner kernel arguments if these contain references to a
   % variable which has been declared in a factor or order statement.
   prepreform1(u,append(ordl!*,factors!*));

symbolic procedure prepreform1(u,l);
   if atom u or get(car u,'dname) then u else
      begin scalar w,l1;
	 l1 := l;
	 while null w and l1 do
	   if smemq(car l1,cdr u) then w:=t else l1:=cdr l1;
	 if null w then return u;
	 if memq(car u,'(plus difference minus times quotient))
	   or null get(car u,'simpfn) then w := nil;
	return if car u eq '!*sq
		 then prepreform1(prepsq!* sqhorner!* cadr u,l)
		else car u . for each p in cdr u collect
	       prepreform1(if w
			     then prepsq!* sqhorner!* simp!* p else p,l)
      end;

symbolic procedure sqhorner!* u;
  if not !*horner then u else
   hornersq(reorder numr u ./ hornerf reorder denr u) 
     where kord!* = append(ordl!*,kord!*);

symbolic procedure printsf u; <<prinsf u; terpri!* nil; u>>;

symbolic procedure prinsf u; if null u then prin2!* 0 else xprinf2 u;

symbolic procedure xprinf(u,flg,w);
   % U is a standard form, flg determines whether parens are needed.
   % W is currently unused.
   % Procedure prints the form and returns NIL.
   begin flg and prin2!* "("; xprinf2 u; flg and prin2!* ")" end;

symbolic procedure xprinf2 u;
   begin scalar v;
      while not domainp u do <<xprint(lt u,v); u := red u; v := t>>;
      if null u then return nil
       else if minusf u then <<oprin 'minus; u := !:minus u>>
       else if v then oprin 'plus;
      if atom u then prin2!* u else maprin u
   end;

symbolic procedure xprint(u,flg);
   % U is a standard term.
   % Flg is a flag which is true if a term has preceded this term.
   % Procedure prints the term and returns NIL.
   begin scalar v,w;
      v := tc u;
      u := tpow u;
      if (w := kernlp v) and w neq 1
	then <<v := quotf(v,w);
	       if minusf w
		 then <<oprin 'minus; w := !:minus w; flg := nil>>>>;
      if flg then oprin 'plus;
      if w and w neq 1 then <<prin2!* w; oprin 'times>>;
      xprinp u;
      if v neq 1 then <<oprin 'times; xprinf(v,red v,nil)>>
   end;

symbolic procedure xprinp u;
   % U is a standard power.  Procedure prints term and returns NIL.
   begin
      % Process main variable.
      if atom car u then prin2!* car u
       else if not atom caar u or caar u eq '!*sq then
	<<prin2!* "(";
	  if not atom caar u then xprinf2 car u else sqprint cadar u;
	  prin2!* ")">>
       else if caar u eq 'plus then maprint(car u,100)
       else maprin car u;
      % Process degree.
      if (u := cdr u)=1 then return nil
       else if !*nat and !*eraise
	then <<ycoord!* := ycoord!*+1;
	       if ycoord!*>ymax!* then ymax!* := ycoord!*>>
       else prin2!* get('expt,'prtch);
      prin2!* if numberp u and minusp u then list u else u;
      if !*nat and !*eraise
	then <<ycoord!* := ycoord!*-1;
	       if ymin!*>ycoord!* then ymin!* := ycoord!*>>
   end;

endmodule;

end;
