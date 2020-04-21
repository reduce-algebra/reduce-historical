% MINI-RDS-WRS.RED 

Fluid '(IN!* Out!*);

Procedure RDS N;
 If NULL N then RDS 0
  else begin scalar K;
      K:=IN!*;
      IN!*:=N;
      Return K
      end;

Procedure WRS N;
 If NULL N then WRS 1
  else begin scalar K;
      K:=Out!*;
      Out!*:=N;
      Return K
      end;

End;
