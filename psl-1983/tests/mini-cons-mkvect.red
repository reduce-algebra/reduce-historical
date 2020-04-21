% MINI-CONS.RED : Cons, MkVect etc for testing
%/Almost identical to PK:CONS-MKVECT

on syslisp;

procedure HardCons(x,y);
 Begin scalar c;
  c:=GtHeap PairPack();
  c[0]:=x;
  c[1]:=y;
  Return MkPAIR(c);
 End;

procedure Cons(x,y);
  HardCons(x,y);

procedure Xcons(x,y);
  HardCons(y,x);

procedure Ncons x;
  HardCons(x,'NIL);

syslsp procedure MkVect N;		
%  Allocate vector, init all to NIL
    if IntP N then
    <<  N := IntInf N;
	if N < (-1) then
	    StdError
		'"A vector with fewer than zero elements cannot be allocated"
	else begin scalar V;
	    V := GtVect N;
	    for I := 0 step 1 until N do VecItm(V, I) := NIL;
	    return MkVEC V;		% Tag it
	end >>
    else NonIntegerError(N, 'MkVect);

off syslisp;

End;
