% 8 * 12  Vector Characters

CV := MkVect(127)$

BlankChar := 'NIL$  

% Labeled Points on Rectangle (8 x 12 )

% C4   Q6   S3   Q5   C3
%
%
% Q7        M3        Q4
%
%
% S4   M4   M0   M2   S2
%
%
% Q8        M1        Q3
%
%
% C1   Q1   S1   Q2   C2

% Corners:
C1:={0,0}$ C2 := {8,0}$ C4:={0,12}$ C3:= {8,12}$

% Side MidPoints:
S1 := {4,0}$ S3 := {4,12}$
S4 := {0,6}$ S2 := {8,6}$

% Middle:
M0 := {4,6}$
M1 := {4,3}$
M2 := {6,6}$
M3 := {4,9}$
M4 := {2,6}$

% Side Quarter Points:

Q1 := {2,0}$ Q2 := {6,0}$
Q3 := {8,3}$ Q4 := {8,9}$
Q5 := {6,12}$ Q6 := {2,12}$ 
Q7 := {0,9}$  Q8 := {0,3}$

For i:=0:127 do CV[I]:=BlankChar;

% UpperCase:

CV[Char A] := C1  _  S3  _  C2 & M4  _  M2$
CV[Char B] := C1  _  C4  _  Q5  _  Q4  _  M2  _  S4 & M2  _  Q3  _  Q2  _ C1 $
CV[Char C] := Q3  _  Q2  _  Q1  _  Q8  _  Q7  _  Q6  _  Q5  _  Q4$
CV[Char D] := C1  _  C4  _  Q5  _  Q4  _  Q3  _  Q2  _  C1$
CV[Char E] := C3  _  C4  _  C1  _  C2 & S4  _  S2$
CV[Char F] := C3  _  C4  _  C1  & S4  _  S2$
CV[Char G] := M0  _  S2  _  Q2  _  Q1  _  Q8  _  Q7  _  Q6  _  Q5  _  Q4$
CV[Char H] := C4  _  C1 & S4  _  S2 & C3  _  C2$
CV[Char I] := S1  _  S3$
CV[Char J] := C3  _  Q3  _  Q2  _  Q1  _  Q8$
CV[Char K] := C4  _  C1 & C3  _  S4  _  C2$
CV[Char L] := C4  _  C1  _  C2$
CV[Char M] := C1  _  C4  _  M0  _  C3  _  C2$
CV[Char N] := C1  _  C4  _  C2  _  C3$
CV[Char O] := Q3  _  Q2  _  Q1  _  Q8  _  Q7  _  Q6  _  Q5  _  Q4  _  Q3$
CV[Char P] := C1  _  C4  _  Q5  _  Q4  _  M2 _ S4$
CV[Char Q] := Q3  _  Q2  _  Q1  _  Q8  _  Q7  _  Q6  _  Q5  _  Q4  _  Q3 & C2  _  M1$
CV[Char R] := C1  _  C4  _  Q5  _  Q4  _  M2  _ S4 & M0 _ C2$
CV[Char S] := Q4  _  Q5  _  Q6  _  Q7  _  M4  _ M2  _  Q3  _  Q2  _  Q1  _  Q8$
CV[Char T] := C4  _  C3 & S3  _  S1$
CV[Char U] := C4  _  Q8  _  Q1  _  Q2  _  Q3  _  C3$
CV[Char V] := C4  _  S1  _  C3$
CV[Char W] := C4  _  Q1  _  M0  _  Q2  _  C3$
CV[Char X] := C1  _  C3 & C4  _  C2$
CV[Char Y] := C4   _   M0   _   C3 & M0   _   S1$
CV[Char Z] := C4  _  C3  _  C1  _  C2$

% Lower Case, Alias for Now:

CV[Char Lower A] := CV[Char A]$
CV[Char Lower B] := CV[Char B]$
CV[Char Lower C] := CV[Char C]$
CV[Char Lower D] := CV[Char D]$
CV[Char Lower E] := CV[Char E]$
CV[Char Lower F] := CV[Char F]$
CV[Char Lower G] := CV[Char G]$
CV[Char Lower H] := CV[Char H]$
CV[Char Lower I] := CV[Char I]$
CV[Char Lower J] := CV[Char J]$
CV[Char Lower K] := CV[Char K]$
CV[Char Lower L] := CV[Char L]$
CV[Char Lower M] := CV[Char M]$
CV[Char Lower N] := CV[Char N]$
CV[Char Lower O] := CV[Char O]$
CV[Char Lower P] := CV[Char P]$
CV[Char Lower Q] := CV[Char Q]$
CV[Char Lower R] := CV[Char R]$
CV[Char Lower S] := CV[Char S]$
CV[Char Lower T] := CV[Char T]$
CV[Char Lower U] := CV[Char U]$
CV[Char Lower V] := CV[Char V]$
CV[Char Lower W] := CV[Char W]$
CV[Char Lower X] := CV[Char X]$
CV[Char Lower Y] := CV[Char Y]$
CV[Char Lower Z] := CV[Char Z]$


% Digits:

CV[Char 0] := CV[Char O]$
CV[Char 1] := CV[Char I]$
CV[Char 2] := Q7  _  Q6  _  Q5  _  Q4  _  M0  _  C1  _  C2$
CV[Char 3] := C4  _  C3  _  M0  _  Q3  _  Q2  _  Q1  _  Q8$
CV[Char 4] := S1  _  S3  _  S4  _  S2$
CV[Char 5] :=  C3  _  C4  _  S4  _  M0  _  Q3  _  Q2  _  Q1  _  Q8$
CV[Char 6] :=  Q4 _ Q5  _  Q6 _ Q7 _ Q8  _  Q1  _  Q2  _  Q3  _  
                M2  _  M4 _ Q8$
CV[Char 7] := C4  _  C3  _  S1$
CV[Char 8] := M0  _  M4  _  Q8  _  Q1  _  Q2  _  Q3  _  M2  _  M0 
              & M2  _  Q4  _  Q5  _  Q6  _  Q7  _  M4$
CV[Char 9] := Q8  _  Q1  _  Q2  _  Q3  _  Q4  _  Q5  _  
                Q6  _  Q7  _  M4  _ M2  _  Q4$

% Some Special Chars:

CV[Char !+ ] := S1 _ S3 & S4 _ S2$
CV[Char !- ] := S4 _ S2 $

CV[Char !* ] := S1 _ S3 & S4 _ S2 & C1 _ C3 & C4 _ C2 $
CV[Char !/ ] := C1 _ C3 $
CV[Char !\ ] := C4 _ C2 $

CV[Char !( ] := Q6 _ Q7 _ Q8 _ Q1 $
CV[Char !) ] := Q5 _ Q4 _ Q3 _ Q2 $

CV[Char ![ ] := Q6 _ C4 _ C1 _ Q1$
CV[Char !] ] := Q5 _ C3 _ C2 _ Q2$

CV[Char != ] := Q7 _ Q4 & Q8 _ Q3 $


% Some Simple Display Routines:

Xshift := Xmove(10)$
Yshift := Ymove(15)$

Procedure ShowString(S);
 <<Graphon();
   ShowString1(S,Global!.Transform);
   Graphoff()>>; 

Procedure ShowString1(S,Current!.Transform);
 Begin scalar i,ch;
   For i:=0:Size S
     do <<Draw1(CV[S[i]],Current!.Transform);
          Current!.Transform := Mat!*mat(XShift,Current!.TRansform)>>;
 End;

Procedure C x;
  if x:=CV[x] then EShow x;

Procedure FullTest();
 <<Global!.Transform := MAT!*1;
   ShowString "ABCDEFGHIJKLMNOPQRTSUVWXYZ 0123456789";
   NIL>>;

Procedure SpeedTest();
 <<Global!.Transform := Mat!*1;
   For i:=0:127 do C i;
   NIL>>;


Procedure SlowTest();
 <<Global!.Transform := Mat!*1;
   For i:=0:127 do
      <<C i;
        Delay()>>;
   NIL>>;


Procedure Delay;
  For i:=1:500 do nil;


Procedure Text(S);
  List('TEXT,S);

Put('TEXT,'PBINTRP,'DrawTEXT)$


Procedure DrawText(StartPoint,S);    %. Draw a Text String
Begin scalar MoveP;
      If IDP StartPoint then StartPoint := EVAL StartPoint$
      S := CAR1 S$
      If IDP S then 
        S := EVAL S$ 
     MoveP:=PositionAt StartPoint;
     ShowString1(S,Mat!*Mat(MoveP,Current!.Transform));     
     Return NIL;
end$

Procedure PositionAt StartPoint; % return A matrix to set relative Origin
 << If IDP StartPoint then StartPoint := EVAL StartPoint$
    Mat16(1,0,0,StartPoint[1],
         0,1,0,StartPoint[2],
         0,0,1,StartPoint[3],
         0,0,0,StartPoint[4])>>;
