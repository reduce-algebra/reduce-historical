module parser;  % Functions for parsing RLISP expressions.

% Author: Anthony C. Hearn.

% Copyright (c) 1991 The RAND Corporation.  All rights reserved.

fluid '(!*backtrace);

global '(cursym!* letl!* nxtsym!*);

%With the exception of assignment statements, which are handled by
%XREAD, statements in REDUCE are introduced by a key-word, which
%initiates a reading process peculiar to that statement.  The key-word
%is recognized (in XREAD1) by the indicator STAT on its property list.
%The corresponding property is the name of the function (of no
%arguments) which carries out the reading sequence.

% ***** COMMENTS *****

symbolic procedure comm1 u;
   begin scalar bool;
        if u eq 'end then go to b;
  a:    if cursym!* eq '!*semicol!*
           or u eq 'end
                and cursym!* memq
		   '(end else then until !*rpar!* !*rsqbkt!*)
          then return nil
         else if u eq 'end and null bool
          then progn(lprim list("END-COMMENT NO LONGER SUPPORTED"),
                     bool := t);
  b:    scan();
        go to a
   end;


% ***** CONDITIONAL STATEMENT *****

symbolic procedure ifstat;
   begin scalar condx,condit;
    a:  condx := xread t;
	if not(cursym!* eq 'then) then symerr('if,t);
        condit := aconc!*(condit,list(condx,xread t));
	if not(cursym!* eq 'else) then nil
         else if scan() eq 'if then go to a
         else condit := aconc!*(condit,list(t,xread1 t));
        return ('cond . condit)
   end;

put('if,'stat,'ifstat);

flag ('(then else),'delim);


% ***** FUNCTION STATEMENT *****

symbolic procedure functionstat;
   begin scalar x;
      x := scan();
      return list('function,
                  if x eq '!*lpar!* then xread1 t
                   else if idp x and null(x eq 'lambda)
                    then progn(scan(),x)
                   else symerr("Function",t))
   end;

put('function,'stat,'functionstat);


% ***** LAMBDA STATEMENT *****

symbolic procedure lamstat;
   begin scalar x,y;
        x:= xread 'lambda;
%       x := flagtype(if null x then nil else remcomma x,'scalar);
        if x then x := remcomma x;
        y := list('lambda,x,xread t);
%       remtype x;
        return y
   end;

put ('lambda,'stat,'lamstat);


% ***** GROUP STATEMENT *****

symbolic procedure readprogn;
   %Expects a list of statements terminated by a >>;
   begin scalar lst;
    a:  lst := aconc!*(lst,xread 'group);
	if null(cursym!* eq '!*rsqbkt!*) then go to a;
        scan();
        return ('progn . lst)
   end;

put('!*lsqbkt!*,'stat,'readprogn);

flag('(!*rsqbkt!*),'delim);

flag('(!*rsqbkt!*),'nodel);


% ***** END STATEMENT *****

symbolic procedure endstat;
  %This procedure can also be used for any key-words  which  take  no
  %arguments;
   begin scalar x; x := cursym!*; comm1 'end; return list x end;

put('end,'stat,'endstat);

put('endmodule,'stat,'endstat);

put('bye,'stat,'endstat);

put('quit,'stat,'endstat);

flag('(bye quit),'eval);

put('showtime,'stat,'endstat);

endmodule;

end;
