% MINI-READ.RED - A small reader

CompileTime <<GLOBAL '(DEBUG);
              FLUID '(TOK!* TOKTYPE!* CH!* !*RAISE);>>;

Procedure READ;        
% start RATOM, get first fresh token
  Read1(Ratom());

Procedure READ1(x);
   If x eq '!( then  READLIST(RATOM()) % Skip the (
    else if  x eq '!' then CONS('QUOTE, NCONS READ())
    else x;

Procedure ReadList(x);    
% read LIST, starting at token x
 Begin scalar y;
  If x eq '!) then Return NIL;
  y:=Read1(x);   % Finish read CAR of pair
  x:=Ratom();    % Check dot
  If x eq '!. then return CONS(y,car READLIST(RATOM()));
  Return CONS(y , READLIST(x))
End;

End;
