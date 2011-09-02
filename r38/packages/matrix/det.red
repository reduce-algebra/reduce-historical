module det;   % Determinant and trace routines.

% Author: Anthony C. Hearn.

fluid '(!*cramer !*rounded asymplis!* bareiss!-step!-size!* kord!*
	powlis!* powlis1!* subfg!* wtl!*);

bareiss!-step!-size!* := 2;     % seems fastest on average.

symbolic procedure simpdet u;
   begin scalar x,!*protfg;
      !*protfg := t;
      return if !*cramer or !*rounded or
                errorp(x := errorset({'bareiss!-det,mkquote u},nil,nil))
               then detq matsm carx(u,'det)
              else car x
   end;

% The hashing and determinant routines below are due to M. L. Griss.

Comment Some general purpose hashing functions;

flag('(array),'eval);      % Declared again for bootstrapping purposes.

array !$hash 64;  % General array for hashing.

symbolic procedure gethash key;
   % Access previously saved element.
   assoc(key,!$hash(remainder(key,64)));

symbolic procedure puthash(key,valu);
   begin integer k; scalar buk;
      k := remainder(key,64);
      buk := (key . valu) . !$hash k;
      !$hash k := buk;
      return car buk
   end;

symbolic procedure clrhash;
   for i := 0:64 do !$hash i := nil;

Comment Determinant Routines;

symbolic procedure detq u;
   % Top level determinant function.
   begin integer len;
      len := length u;   % Number of rows.
      for each x in u do
        if length x neq len then rederr "Non square matrix";
      if len=1 then return caar u;
      clrhash();
      u := detq1(u,len,0);
      clrhash();
      return u
   end;

symbolic procedure detq1(u,len,ignnum);
   % U is a square matrix of order LEN. Value is the determinant of U.
   % Algorithm is expansion by minors of first row.
   % IGNNUM is packed set of column indices to avoid.
   begin integer n2; scalar row,sign,z;
      row := car u;   % Current row.
      n2 := 1;
      if len=1
        then return <<while twomem(n2,ignnum)
                         do <<n2 := 2*n2; row := cdr row>>;
		      car row>>;   % Last row, single element.
      if z := gethash ignnum then return cdr z;
      len := len-1;
      u := cdr u;
      z := nil ./ 1;
      for each x in row do
        <<if not twomem(n2,ignnum)
            then <<if numr x
                     then <<if sign then x := negsq x;
                            z:= addsq(multsq(x,detq1(u,len,n2+ignnum)),
                                        z)>>;
                   sign := not sign>>;
          n2 := 2*n2>>;
      puthash(ignnum,z);
      return z
   end;

symbolic procedure twomem(n1,n2);
   % For efficiency reasons, this procedure should be coded in assembly
   % language.
   not evenp(n2/n1);

put('det,'simpfn,'simpdet);

flag('(det),'immediate);

% A version of det using the Bareiss code.

symbolic procedure bareiss!-det u;
   % Compute a determinant using the Bareiss code.
   begin scalar nu,bu,n,ok,temp,v,!*exp;
   !*exp := t;
   nu := matsm car u;
   n := length nu;
   for each x in nu do
      if length x neq n then rederr "Non square matrix";
   if n=1 then return caar nu;
%  Note in an earlier version, these were commented out.
   if asymplis!* or wtl!* then
    <<temp := asymplis!* . wtl!*;
      asymplis!* := wtl!* := nil>>;
   nu := normmat nu;
   v := for i:=1:n collect intern gensym();
   % Cannot rely on the ordering of the gensyms.
   ok := setkorder append(v,kord!*);
   car nu := foreach r in car nu collect prsum(v,r);
   bu := cdr sparse_bareiss(car nu,v,bareiss!-step!-size!*)
	    where powlis!* = nil,powlis1!* = nil,subfg!* = nil;
      % It is probably sufficient to set subfg* to nil in previous line.
   bu := if length bu = n then (lc car bu ./ cdr nu) else (nil ./ 1);
   setkorder ok;
   if temp then <<asymplis!* := car temp; wtl!* := cdr temp>>;
   return resimp bu
   end;

symbolic procedure simptrace u;
   begin integer n; scalar z;
        u := matsm carx(u,'trace);
        if length u neq length car u then rederr "Non square matrix";
        n := 1;
        z := nil ./ 1;
        for each x in u do <<z := addsq(nth(x,n),z); n := n+1>>;
        return z
   end;

put('trace,'simpfn,'simptrace);

endmodule;

end;
