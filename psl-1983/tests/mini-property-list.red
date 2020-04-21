% MINI-PROPERTY-LIST.RED - Small GET and PUT

on syslisp;

Procedure Prop x;
 If not IDP x then NIL
  else SYMPRP IDINF x;

Procedure Get(x,y);
 Begin scalar z,L;
   If Not IDP x  then return NIL;
   L:=SYMPRP IDINF x;
   If (Z:=Atsoc(y,L)) then return CDR Z;
   Return NIL;
 End;

Procedure Put(x,y,z);
 Begin scalar P,L;
   If Not IDP x  then return NIL;
   L:=SYMPRP IDINF x;
   If (P:=Atsoc(y,L)) then return	% 
      <<CDR(PairInf P):=z; z>>;
   L:=CONS(CONS(y,z),L);
   SYMPRP(IDINF x):=L;
   Return z;
 End;

Procedure RemProp(x,y);
 Begin scalar P,L;
   If Not IDP x  then return NIL;
   L:=SYMPRP IDINF x;
   If not(P:=Atsoc(y,L)) then return NIL;
   L:=Delatq(y,L);
   SYMPRP(IDINF x):=L;
   Return CDR P;
 End;

Procedure GetFnType x;
  Get(x,'Ftype);

off syslisp;

end;
