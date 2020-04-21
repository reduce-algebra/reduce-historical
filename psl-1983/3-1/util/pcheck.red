%  <PSL.UTIL>PCHECK.RED.3, 11-Oct-82 18:14:36, Edit by BENSON
%  Changed CATCH to *CATCH

% A little program to check parens in a LISP file

Fluid '(LastSexpr!*);
procedure Pcheck F;
 begin scalar Chan,OldChan;
    LastSexpr!*:=NIL;
    Chan:=Open(F,'Input);
    OldChan:=RDS(Chan);
    !*Catch(NIL,Pcheck1());
    Rds(OldChan);
    Close chan;
%   Printf("last Full S-expression%r%n",LastSexpr!*);
 end;

%/ can we enable Line counter somehow?

procedure Pcheck1();
 Begin Scalar x;
  L:   x:=Read();
       if x eq !$EOF!$ then return NIL;
       LastSexpr!*:=x;
       PrintSome x;
       Goto L;
 End;

procedure printsome x;
 <<Prinsomelevel(x,2,3);terpri()>>;

procedure prinsomelevel(x,l1,l2);
If not pairp x then <<prin1 x; prin2 " ">>
 else if l1 <=0 then prin2 " ... "
 else if l2 <=0 then prin2 " ... "
 else <<prin2 "("; prinsomelevel(car x,l1-1,l2);
        if null cdr x then prin2 ")"
         else if ListP cdr x then <<prinsomelevel(cdr x,l1,l2-1); prin2 ")">>
         else <<prin2 " . "; prinsomelevel(cdr x,l1,l2-1); prin2 ")">>
      >>;

procedure ListP x;
 null x or (Pairp x and ListP cdr x);

end;

