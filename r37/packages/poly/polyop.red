module polyop; % Functions for algebraic mode operations on polynomials.

% Author: Anthony C. Hearn.

% Modified by: F. Kako, F.J. Wright.

% Copyright (c) 1995 RAND. All rights reserved.

% This code has been modified to be consistent with the rules

%      lterm(f,x) = lcof(f,x)*lpower(f,x)
%      f = lterm(f,x) + reduct(f,x)

fluid '(!*ratarg gdmode!*);

symbolic procedure deg(u,kern);
   <<u := simp!* u; tstpolyarg(denr u,u); numrdeg(numr u,kern)>>
   where dmode!* = gdmode!*;

symbolic procedure numrdeg(u,kern);
   begin scalar x;
      kern := !*a2k kern;
      if domainp u then return 0
       else if mvar u eq kern then return !*f2a ldeg u;
      x := updkorder kern;
      u := reorder u;
      if not(mvar u eq kern) then u := 0 else u := ldeg u;
      setkorder x;
%     return !*f2a u
      return u
   end;

symbolic procedure lcofeval u;
   begin scalar kern,x,y;
      if null u or null cdr u or not null cddr u
        then rerror(poly,280,
                    "LCOF called with wrong number of arguments");
      kern := !*a2k cadr u;
      u := simp!* car u;
      y := denr u;
      tstpolyarg(y,u);
      u := numr u;
      if domainp u then return if null u then 0 else mk!*sq (u . 1)
       else if mvar u eq kern then return !*ff2a(lc u,y);
      x := updkorder kern;
      u := reorder u;
      if mvar u eq kern then u := lc u;
      setkorder x;
      return if null u then 0 else !*ff2a(u,y)
   end;

put('lcof,'psopfn,'lcofeval);

% Note. This is an older definition still used by some packages.

symbolic procedure lcof(u,kern);
   begin scalar x,y;
      u := simp!* u;
      y := denr u;
      tstpolyarg(y,u);
      u := numr u;
      kern := !*a2k kern;
      if domainp u then return 0
       else if mvar u eq kern then return !*ff2a(lc u,y);
      x := updkorder kern;
      u := reorder u;
      if mvar u eq kern then u := lc u;
      setkorder x;
      return if null u then 0 else !*ff2a(u,y)
   end;

symbolic procedure lpower(u,kern);
   begin scalar x,y;
      u := simp!* u;
      y := denr u;
      tstpolyarg(y,u);
      u := numr u;
      kern := !*a2k kern;
      if domainp u then return 1
       else if mvar u eq kern then return !*ff2a(lpow u.*1 .+ nil,y);
      x := updkorder kern;
      u := reorder u;
      if mvar u eq kern then u := lpow u.*1 .+ nil else u := 1;
      setkorder x;
      return !*ff2a(u,y)
   end;

symbolic procedure lterm(u,kern);
   begin scalar x,y;
      u := simp!* u;
      y := denr u;
      tstpolyarg(y,u);
      u := numr u;
      kern := !*a2k kern;
      if domainp u then return if null u then 0 else u
       else if mvar u eq kern then return !*ff2a(lt u .+ nil,y);
      x := updkorder kern;
      u := reorder u;
%     if mvar u eq kern then u := lt u .+ nil else u := nil;
      if mvar u eq kern then u := lt u .+ nil;
      setkorder x;
      u := reorder u;
      return !*ff2a(u,y)
   end;

% symbolic procedure !*lterm u; lt u .+ nil;

symbolic procedure mainvar u;
   if domainp(u := numr simp!* u) then 0
    else sfchk(u := mvar u);

symbolic procedure sfchk u; if sfp u then prepf u else u;

symbolic procedure reduct(u,kern);
   begin scalar x,y;
      u := simp!* u;
      y := denr u;
      tstpolyarg(y,u);
      u := numr u;
      kern := !*a2k kern;
%     if domainp u then return !*ff2a(u,y)
      if domainp u then return 0
       else if mvar u eq kern then return !*ff2a(cdr u,y);
      x := updkorder kern;
      u := reorder u;
%     if mvar u eq kern then u := cdr u;
      if mvar u eq kern then u := cdr u else u := nil;
      setkorder x;
      u := reorder u;
      return !*ff2a(u,y)
   end;

symbolic procedure tstpolyarg(y,u);
   null !*ratarg and y neq 1 and typerr(prepsq u,"polynomial");

% symbolic operator deg,lpower,lterm,mainvar,reduct;

flag('(deg lpower lterm mainvar reduct),'opfn); % This way for booting.

endmodule;

end;
