% Scan the *.ins files
% for a special Token
Loadtime Load DIR!-STUFF$

InsList!*:=Vector2List GetCleanDir "<psl.util.ins>*.ins"$

Procedure ShowAllIns();
Begin scalar  R,C,OldC;
 For each F in InsList!* do
    <<C:=OPEN(F,'input);
      OldC:=RDS C; R:=READ(); RDS OldC;
      Close C;
      Print F;
      Print R>>;
End;

Procedure LoadAllIns();
Begin scalar  R,C,OldC;
 For each F in InsList!* do
    <<C:=OPEN(F,'input);
      OldC:=RDS C; R:=READ(); RDS OldC;
      Close C;
      For Each x in R do Put(x,'DefinedIn,F);
      PrintF(" %r  loaded %n",F)>>
End;

Procedure WhereIs X;
 Begin scalar y;
   if(y:=get(x,'DefinedIn)) then Return y;
   if getd x then return "In The Kernel ";
   return NIL;
 end;

