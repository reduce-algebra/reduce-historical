module smacro;  % Support for SMACRO expansion.

% Author: Anthony C. Hearn.

% Copyright (c) 1987 The RAND Corporation.  All rights reserved.

symbolic procedure applsmacro(u,vals,name);
   % U is smacro body of form (lambda <varlist> <body>), VALS is
   % argument list, NAME is name of smacro.
   begin scalar body,remvars,varlist,w;
      varlist := cadr u;
      body := caddr u;
      if length varlist neq length vals
        then rerror(rlisp,15,list("Argument mismatch for SMACRO",name));
      if no!-side!-effect!-listp vals or one!-entry!-listp(varlist,body)
        then return subla!-q(pair(varlist,vals),body)
       else if length varlist>1
        then <<w := for each x in varlist collect (x . gensym());
               body := subla!-q(w,body);
               varlist := for each x in w collect cdr x>>;
      for each x in vals do
         <<if no!-side!-effectp x or one!-entryp(car varlist,body)
             then body := subla!-q(list(car varlist . x),body)
            else remvars := aconc(remvars,car varlist . x);
           varlist := cdr varlist>>;
      if null remvars then return body
       else <<w := list('lambda,
                         for each x in remvars collect car x,
                         body) .
                    for each x in remvars collect cdr x;
%             if not eqcar(cadr w,'setq)
%               then <<prin2 "*** smacro: "; print cdr w>>;
              return w>>
   end;

symbolic procedure no!-side!-effectp u;
   if atom u then numberp u or idp u and not(fluidp u or globalp u)
    else if car u eq 'quote then t
    else if flagp(car u,'nosideeffects)
     then no!-side!-effect!-listp cdr u
    else nil;

symbolic procedure no!-side!-effect!-listp u;
   null u or no!-side!-effectp car u and no!-side!-effect!-listp cdr u;

flag('(car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr
       cddar cdddr cons),'nosideeffects);

symbolic procedure one!-entryp(u,v);
   % determines if id U occurs less than twice in V.
   if atom v then t
    else if smemq(u,car v)
     then if smemq(u,cdr v) then nil else one!-entryp(u,car v)
    else one!-entryp(u,cdr v);

symbolic procedure one!-entry!-listp(u,v);
   null u or one!-entryp(car u,v) and one!-entry!-listp(cdr u,v);

symbolic procedure subla!-q(u,v);
   begin scalar x;
        if null u or null v then return v
         else if atom v
                 then return if x:= atsoc(v,u) then cdr x else v
         else if car v eq 'quote then return v
         else return(subla!-q(u,car v) . subla!-q(u,cdr v))
   end;

put('smacro,'macrofn,'applsmacro);

endmodule;

end;
