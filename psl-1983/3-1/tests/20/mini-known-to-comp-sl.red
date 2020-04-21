% MINI-KNOWN-TO-COMP-SL.RED


Procedure Car x;
 if Pairp x then car x else NonPairError(x,'CAR);

Procedure Cdr x;
 if Pairp x then cdr x  else NonPairError(x,'CDR);

procedure CodeP x;
  CodeP x;

Procedure Pairp x;
 Pairp x;

Procedure Idp x;
 Idp x;

procedure Eq(x,y);
  eq(x,y);

procedure Null x;
 x eq 'NIL;

procedure Not x;
 x eq 'NIL;

End;
