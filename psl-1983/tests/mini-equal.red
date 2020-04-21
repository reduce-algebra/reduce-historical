% MINI-EQUAL.RED

on syslisp;

Procedure EqStr(s1,S2);
 Begin scalar n;
   s1:=strinf(s1); s2:=strinf(s2);
   n:=strlen(s1);
   if n neq strlen(s2) then return 'NIL;
 L:if n<0 then return 'T;
   if strbyt(s1,n) neq strbyt(s2,n) then return 'NIL;
   n:=n-1;
   goto L;
 End;

off syslisp;

end;
