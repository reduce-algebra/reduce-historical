% MINI-PRINT.RED  - More comprehensive Mini I/O

% A mini Print routine
% uses PutC and PutInt

On syslisp;

syslsp procedure Prin1 x;
 if IDP x then Prin1ID x
  else if IntP x then Prin1Int x
  else if StringP x then Prin1String x
  else if PairP x then Prin1Pair x
  else PrtItm x;

syslsp procedure Prin2 x;
 if IDP x then Prin2ID x
  else if IntP x then Prin1Int x
  else if StringP x then Prin2String x
  else if PairP x then Prin2Pair x
  else PrtItm x;

syslsp procedure Print x;
 <<Prin1 X; Terpri(); x>>;

syslsp procedure Prin2t x;
 <<Prin2 X; Terpri(); x>>;

% Support

syslsp procedure Pblank;
  PutC Char '! ;

syslsp procedure Prin1Int x;
<<if x=0 then PutC Char 0
   else if x<0 then <<PutC Char '!-;
                     Prin1Int (-x)>>
   else Prin1IntX x;
  x>>;

Procedure Prin1IntX x;
 If x=0 then NIL
  else <<Prin1IntX LongDiv(x,10);
         PutC (LongRemainder(x,10)+Char 0)>>;

syslsp procedure Prin1ID x;
   <<Prin2String Symnam IdInf x;
     PBlank();
     x>>;

syslsp procedure Prin2Id x;
  prin1Id x;

syslsp procedure Prin1String x;
<<PutC Char '!"; 
  Prin2String  x; 
  PutC Char '!";
  Pblank();
  x>>;

syslsp procedure Prin2String x;
  Begin scalar s;
     s:=StrInf x;
     For i:=0:StrLen(s) do PutC StrByt(S,I);
     return x
  End;

syslsp procedure Prin1Pair x;
  <<PutC Char '!(;
    Prin1 Car x;
    x:=Cdr X;
    While Pairp X do <<Pblank(); Prin1 Car X; X:=Cdr x>>;
    If Not NULL X then <<Prin2String " . ";
                         Prin1 x>>;
    PutC Char '!) ;
    Pblank();
    x>>;

syslsp procedure Prin2Pair x;
  <<PutC Char '!(;
    Prin2 Car x;
    x:=Cdr X;
    While Pairp X do <<Pblank(); Prin2 Car X; X:=Cdr x>>;
    If Not NULL X then <<Prin2String " . ";
                         Prin2 x>>;
    PutC Char '!) ;
    Pblank();
    x>>;

syslsp procedure terpri();
 Putc Char EOL;

syslsp procedure PrtItm x;
 <<Prin2String " <"; 
   Prin1Int Tag x; 
   PutC Char '!:;
   Prin1Int Inf x;
   Prin2String "> ";
   x>>;

% Some stubs for later stuff

Procedure ChannelPrin2(chn,x);
  Prin2 x;

Off syslisp;


End;
