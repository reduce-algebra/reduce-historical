% MINI-COMP-SUPPORT.RED - Support for LIST etc
%/ Identical to PK:COMP-SUPPORT?

procedure List2(A1,A2);
 Cons(A1,Ncons A2);

procedure List3(A1,A2,A3);
  Cons(A1,List2(A2,A3));

procedure List4(A1,A2,A3,A4);
  Cons(A1,List3(A2,A3,A4));

procedure List5(A1,A2,A3,A4,A5);
  Cons(A1,List4(A2,A3,A4,A5));

end;
