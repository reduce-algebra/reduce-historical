% MINI-NON-SL.RED Simple non sl functions

Procedure Atsoc(x,y);
 If Not PAIRP y then NIL
  else if Not PAIRP car y then Atsoc(x,cdr y)
  else if x EQ car car y then car y
  else Atsoc(x, cdr y);

Procedure GEQ(N1,N2);
 not(N1< N2);

Procedure LEQ(N1,N2);
  not(N1 > N2);

Procedure EqCar(x,y);
 PairP x and (Car(x) eq y);

procedure COPYD(newId,OldId);
 Begin scalar x;
    x:=Getd OldId;
    If not Pairp x 
      then return <<Print List(OLDID, " has no definition in COPYD ");
                    NIL>>;
    Return PUTD(newId,car x,cdr x);
 End;


Procedure Delatq(x,y);
  If not Pairp y then NIL
   else if not Pairp car y then CONS(car y,Delatq(x,cdr y))
   else if x eq caar y then cdr y
   else CONS(car y,Delatq(x,cdr y));

procedure MkQuote x;
 List('quote,x);

End;
