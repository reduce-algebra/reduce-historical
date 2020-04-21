% MINI-ARITHMETIC.RED  simple ARITHmetic functions


Procedure Plus2(x,y);
 if numberp x and numberp y then sys2int(wplus2(intinf x,intinf y))
  else NonNumberError(cons(x,y),'Plus2);

Procedure Minus(x);
 if numberp x then sys2int wminus intinf x
  else NonNumberError(x,'Minus);

Procedure Add1 N;
 If Numberp N then sys2int wplus2(N,1) else 
  else NonNumberError(N,'Add1);

Procedure SUB1 N;
 If Numberp N then sys2int wdifference(N,1)
  else  NonNumberError(N,'SUB1);


Procedure GreaterP(N1,N2);
 If NumberP N1 and NumberP N2 then wGreaterp(intinf N1,intinf N2) else NIL;

Procedure LessP(N1,N2);
 If NumberP N1 and NumberP N2 then Wlessp(intinf N1,intinf N2) else NIL;

Procedure DIFFERENCE(N1,N2);
 If NumberP N1 and NumberP N2 then sys2int wdifference(intinf N1,intinf N2)
  else  NonNumberError(cons(N1,N2),'Difference);

Procedure TIMES2(N1,N2);
 If NumberP N1 and NumberP N2 then sys2int Wtimes2(intinf N1,intinf N2)
  else NonNumberError(cons(N1,N2),'TIMES2);

End;
