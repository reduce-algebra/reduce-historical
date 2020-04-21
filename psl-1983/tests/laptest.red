% LAPTEST.RED - A selection of small procedures for testing LAP
% MLG
% Run through LAPOUT for CMACRO (ALM) level,
% and turn on DOPASS1LAP for TLM level.

procedure foo1 x;
 x;

procedure foo2 x;
 1;

procedure foo3 x;
 x+3;

procedure foo4 x;
 print(x+4);

procedure foo5 x;
 if x=1 then 'one else 'not!-one;

FLUID '(FLU1 FLU2);

procedure foo6a(Flu1,Flu2);
 begin	Print List('before,FLU1,Flu2);
	Flu1:=10;
	Flu2:=20;
        Print List('after,FLU1,Flu2);
 end;

procedure foo6();
 <<Flu1:=1; Flu2 :=2;
   Print List('before,FLU1,Flu2);
   Foo6a('a,'b);
   Print List('after,FLU1,Flu2);
  >>;


End;
