% FIELD.RED - Exhaustively Test the Field Operator

On SYSLISP;

In "XXX-Header.red"$

Procedure FirstCall;
 Begin Scalar X,BPW;
  Msg5(Char M, Char S, Char G, Char '! ,Char EOL);
  TestOK Char '!?;  %/ Confirm the test message
  TestErr Char '!?; 

% Set up test pattern
         %0001122233444556 % Bit Number T
         %0482604826048260              U

BPW:=BitsPerWord; % For bug in !*JUMPxx
  If BPW eq 64 then
     X:=16#0123456789ABCDEF  % 16 nibbles=8 bytes
   else if BPW eq 32 then
     X:=16#01234567          % 8 nibbles=4 bytes
   else if BPW eq 36 then
     X:=16#012345678         % 9 nibbles=4.5 bytes
   else ERR 99;

  AShiftTest(X);     %/ Arithmetic Test
  FieldTest(X);      %/ FieldExtract
  LshiftTest(X);     %/ Shift and Masks with Field
  Quit;
 End;

% Ashift can only be tested by a multiply of a 2 to a power.  Therefore
%  it is only used in the left shift case.
Procedure AShiftTest TestVal;
 Begin Scalar X, Y;
  Msg5(Char A,Char S,Char H,Char I,Char F);
  Msg5(Char T,Char '! ,Char '! ,Char '! , Char EOL);
  Y := 10;
  Y := Y*4;
  If Y NEQ 40 Then TestErr Char 1 Else TestOk Char 1;
  Y := -5;
  Y := Y*16;
  If Y NEQ -80 Then TestErr Char 2 Else TestOk Char 2;
  Y := 6;
  X := 4;
  Y := Y * 4;
  If Y NEQ 6*X Then TestErr Char 3 Else TestOk Char 3;
 End;


Procedure FieldTest(x);
%   Extract a field from a variable and see if it works.
 Begin scalar Y;
  Msg5(Char F,Char I,Char E,Char L,Char D);
  PutC Char EOL;
  Y:=Field(X, 0, BitsPerWord);% FullWord
  If Y NEQ X Then TestErr Char 1 Else TestOk Char 1;
  Y:=Field(X, 0, 8);          % First Byte
  If Y NEQ 16#01 Then TestErr Char 2 Else TestOk Char 2;
  Y:=Field(X, 8, 8);          % Second Byte
  If Y NEQ 16#23 Then TestErr Char 3 Else TestOk Char 3;
  Y:=Field(X, 16, 8);         % Third Byte
  If Y NEQ 16#45 Then TestErr Char 4 Else TestOk Char 4;
  Y:=Field(X, 24, 8 );        % Fourth Byte
  If Y NEQ 16#67 Then TestErr Char 5 Else TestOk Char 5;
  Y:=Field(X, 0, 16);         % First 16 bit
  If Y NEQ 16#0123  Then TestErr Char 6 Else TestOk Char 6;
  Y:=Field(X, 16, 16);        % Second 16 bit
  If Y NEQ 16#4567  Then TestErr Char 7 Else TestOk Char 7;
 End;

Procedure LshiftTest x;
 Begin Scalar Y;
  Msg5(Char L,Char S,Char H,Char I,Char F);
  Msg5(Char T ,Char '! ,Char '!  ,Char '! , Char EOL);
  Y:=Extract(X, 0, BitsPerWord);         % FullWord
  If Y NEQ X Then TestErr Char 1 Else TestOk Char 1;
  Y:=Extract(X, 0, 8);          % First Byte
  If Y NEQ 16#01 Then TestErr Char 2 Else TestOk Char 2;
  Y:=Extract(X, 8, 8);          % Second Byte
  If Y NEQ 16#23 Then TestErr Char 3 Else TestOk Char 3;
  Y:=Extract(X, 16, 8);         % Third Byte
  If Y NEQ 16#45 Then TestErr Char 4 Else TestOk Char 4;
  Y:=Extract(X, 24, 8 );        % Fourth Byte
  If Y NEQ 16#67 Then TestErr Char 5 Else TestOk Char 5;
  Y:=Extract(X, 0, 16);         % First 16 bit
  If Y NEQ 16#0123  Then TestErr Char 6 Else TestOk Char 6;
  Y:=Extract(X, 16, 16);        % Second 16 bit
  If Y NEQ 16#4567  Then TestErr Char 7 Else TestOk Char 7;
 End;

%%% Signals that Test OK or Error %%%%%

Procedure Msg5(C1,C2,C3,C4,C5);
  <<PutC C1;
    PutC C2;
    PutC C3;
    PutC C4;
    PutC C5>>;

Procedure TestNum X;
 <<Msg5(Char T,Char Lower e,Char Lower s,Char lower t, Char '! );
   PutC X;
   PutC Char '! ;>>;

Procedure TestErr X;
 <<TestNum X;
   Msg5(Char E, Char lower r,Char Lower r,Char '! , Char Eol)>>;

Procedure TestOk X;
 <<TestNum X;
   Msg5(Char O, Char lower k,Char '! ,Char '! , Char Eol)>>;

%%% Dynamic Field Extracts %%%%%

Procedure MakeMask(N);
 % Make a mask of N 1's
  LSH(1,N)-1;

Procedure Extract(Z,sbit,lfld); 
 % Dynamic Field Extract
  Begin scalar m,s;
   m:=MakeMask(Lfld);
   s:=Sbit+Lfld-BitsPerWord;
   Return LAnd(m,Lsh(Z,s));
 end;


End;

