% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    TELERAY specIfic Procedures      %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Basic Teleray 1061 Plotter
%	Screen Range Is X :=  (-40,40) :=  (Left .  . Right)
%			Y :=  (-12,12) :=  (Bottom .  . Top)
% Physical Size is  D.X=~8inch, D.Y=~6inch
% Want square asp[ect ratio for 100*100

Procedure TEL!.OutChar x;
  PBOUT x;

Procedure TEL!.OutCharString S;		% Pbout a string
  For i:=0:Size S do TEL!.OutChar S[i];

Procedure TEL!.NormX X;
  FIX(X)+40;

Procedure TEL!.NormY Y;
  12 - FIX(Y);

Procedure  TEL!.ChPrt(X,Y,Ch);
   <<TEL!.OutChar Char ESC;
     TEL!.OutChar 89;
     TEL!.OutChar (32+TEL!.NormY Y);
     TEL!.OutChar (32+ TEL!.NormX X);
     TEL!.OutChar Ch>>;

Procedure  TEL!.IdPrt(X,Y,Id);
    TEL!.ChPrt(X,Y,ID2Int ID);

Procedure  TEL!.StrPrt   (X,Y,S);
   <<TEL!.OutChar Char ESC;
     TEL!.OutChar 89;
     TEL!.OutChar (32+TEL!.NormY Y);
     TEL!.OutChar (32+ TEL!.NormX X);
     TEL!.OutCharString  S>>;

Procedure  TEL!.HOME   ();	% Home  (0,0)
  <<TEL!.OutChar CHAR ESC;
    TEL!.OutChar 'H>>;

Procedure TEL!.EraseS   ();	% Delete Entire Screen
  <<TEL!.OutChar CHAR ESC;
    TEL!.OutChar '!j>>;

Procedure  TEL!.DDA   (X1,Y1,X2,Y2,dotter);
   Begin scalar Dx,Dy,Xc,Yc,I,R,S;
   % From N & S, Page 44, Draw Straight Pointset
      Dx := X2-X1; Dy := Y2-Y1; R := 0.5;
      If Dx >= 0 then Xc := 1 else <<Xc := -1;Dx := -Dx >>;
      If Dy >= 0 then Yc := 1 else <<Yc := -1;Dy := -Dy >>;
      If Dx <= Dy then Goto doy;
      S := FLOAT(Dy)/Dx;
      For I := 1:Dx do 
         <<R := R+S;
         If R>=1.0 then <<Y1 := Y1+Yc;R := R-1.0 >>;
         X1 := X1+Xc;
         APPLY(dotter,LIST(X1,Y1)) >>;
        Return NIL;
   doy:S := float(Dx) / Dy;
      For I := 1:Dy do 
         <<R := R+S;
         If R>=1.0 then <<X1 := X1+Xc;R := R-1 >>;
         Y1 := Y1+Yc;
         APPLY(dotter,LIST (X1,Y1)) >>;
      Return NIL
   end;

Procedure Tel!.MoveS   (X1,Y1);
   <<Xhere := X1;
     Yhere := Y1>>;

Procedure Tel!.DrawS   (X1,Y1);
  << TEL!.DDA (Xhere,Yhere, X1, Y1,function TEL!.dotc);
     Xhere :=X1; Yhere :=Y1>>;
   
Procedure  Idl2chl   (X);	% Convert Idlist To Char List
   Begin scalar Y;
      While Pairp (X) do <<Y := getv (Sfromid car X, 1) . Y;X := Cdr X >>;
      Return (Reverse (Y))
   end;

FLUID '(Tchars);

Procedure  Texter   (X1,Y1,X2,Y2,Txt);
   Begin scalar Tchars;
      Tchars := Idl2chl (Explode2 (Txt));
      Return (TEL!.DDA (X1,Y1,X2,Y2,function Tdotc))
   end;

Procedure  Tdotc   (X1,Y1);
   Begin 
      If Null Tchars then Return (Nil);
      If  (X1 > X2clip) Or  (X1 < X1clip) then Goto No;
      If  (Y1 > Y2clip) Or  (Y1 < Y1clip) then Goto No;
      TEL!.ChPrt (X1 , Y1,Car Tchars);
   No:Tchars := Cdr Tchars;
      Return ('T)
   end;

Procedure  TEL!.dotc   (X1,Y1);	% Draw And Clip An X
 TEL!.ChClip (X1,Y1,Char X) ;

Procedure  TEL!.ChClip   (X1,Y1,Id);
   Begin 
      If  (X1 > X2clip) Or  (X1 < X1clip) then Goto No;
      If  (Y1 > Y2clip) Or  (Y1 < Y1clip) then Goto No;
      TEL!.ChPrt (X1 , Y1,Id);
   No:Return ('T)
   end;

Procedure Tel!.VwPort(X1,X2,Y1,Y2);
   <<X1clip := Max2 (-40,X1); 
     X2clip := Min2 (40,X2);
     Y1clip := Max2 (-12,Y1);
     Y2clip := Min2 (12,Y2)>>;

Procedure  Tel!.Wfill   (X1,X2,Y1,Y2,Id);
   Begin scalar X,Y;
      For Y := Y1 : Y2 do 
       For X := X1 : X2 do TEL!.ChClip (X,Y,Id);
   end;

Procedure  TEL!.Wzap   (X1,X2,Y1,Y2);
   TEL!.Wfill (X1,X2,Y1,Y2,'! ) ;

Procedure TEL!.Delay;
 NIL;

Procedure TEL!.GRAPHON();
If not !*emode then echooff();

Procedure TEL!.GRAPHOFF();
If not !*emode then echoon();

Procedure TEL!.INIT  ();	% Setup For TEL As Device;
 Begin
      Dev!. := 'TEL; 
      FNCOPY('EraseS,'TEL!.EraseS);
      FNCOPY('MoveS,'TEL!.MoveS);
      FNCOPY('DrawS,'TEL!.DrawS);
      FNCOPY( 'NormX, 'TEL!.NormX)$                
      FNCOPY( 'NormY, 'TEL!.NormY)$                
      FNCOPY('VwPort,'TEL!.VwPort); 
      FNCOPY('Delay,'TEL!.Delay);
      FNCOPY( 'GraphOn, 'TEL!.GraphOn)$
      FNCOPY( 'GraphOff, 'TEL!.GraphOff)$
      Erase();
      VwPort (-40,40,-12,12);
      Print "Device Now TEL";
  end;

%  Basic ANN ARBOR AMBASSADOR Plotter
%
%	Screen Range Is X :=  (-40,40) :=  (Left .  . Right)
%			Y :=  (-30,30) :=  (Bottom .  . Top)

Procedure ANN!.OutChar x;
  PBOUT x;

Procedure ANN!.OutCharString S;		% Pbout a string
  For i:=0:Size S do ANN!.OutChar S[i];

Procedure ANN!.NormX X;           % so --> X
   40 + FIX(X+0.5);

Procedure ANN!.NormY Y;           % so ^
   30 - FIX(Y+0.5);                  %    | Y

Procedure ANN!.XY(X,Y);
<<      Ann!.OutChar(char ESC);
        Ann!.OutChar(char ![);
        x:=Ann!.NormX(x);
        y:=Ann!.NormY(y);
        % Use "quick and dirty" conversion to decimal digits.
        Ann!.OutChar(char 0 + (1 + Y)/10);
        Ann!.OutChar(char 0 + remainder(1 + Y, 10));

        Ann!.OutChar(char !;);
          % Delimiter between row digits and column digits.

        Ann!.OutChar(char 0 + (1 + X)/10);
        Ann!.OutChar(char 0 + remainder(1 + X, 10));

        Ann!.OutChar(char H);  % Terminate the sequence
>>;


Procedure  ANN!.ChPrt(X,Y,Ch);
   <<ANN!.XY(X,Y);
     ANN!.OutChar Ch>>;

Procedure  ANN!.IdPrt(X,Y,Id);
    ANN!.ChPrt(X,Y,ID2Int ID);

Procedure  ANN!.StrPrt(X,Y,S);
   <<ANN!.XY(X,Y);
     ANN!.OutCharString  S>>;

Procedure ANN!.EraseS();	% Delete Entire Screen
  <<ANN!.OutChar CHAR ESC;
    ANN!.OutChar Char '![;
    Ann!.OutChar Char 2;
    Ann!.OutChar Char J;
    Ann!.XY(0,0);>>;

Procedure  ANN!.DDA(X1,Y1,X2,Y2,dotter);
   Begin scalar Dx,Dy,Xc,Yc,I,R,S;
   % From N & S, Page 44, Draw Straight Pointset
      Dx := X2-X1; Dy := Y2-Y1; R := 0.5;
      If Dx >= 0 then Xc := 1 else <<Xc := -1;Dx := -Dx >>;
      If Dy >= 0 then Yc := 1 else <<Yc := -1;Dy := -Dy >>;
      If Dx <= Dy then Goto doy;
      S := FLOAT(Dy)/Dx;
      For I := 1:Dx do 
         <<R := R+S;
         If R>=1.0 then <<Y1 := Y1+Yc;R := R-1.0 >>;
         X1 := X1+Xc;
         APPLY(dotter,LIST(X1,Y1)) >>;
        Return NIL;
   doy:S := float(Dx) / Dy;
      For I := 1:Dy do 
         <<R := R+S;
         If R>=1.0 then <<X1 := X1+Xc;R := R-1 >>;
         Y1 := Y1+Yc;
         APPLY(dotter,LIST(X1,Y1)) >>;
      Return NIL
   end;

Procedure ANN!.MoveS(X1,Y1);
   <<Xhere := X1;
     Yhere := Y1>>;

Procedure ANN!.DrawS(X1,Y1);
  << ANN!.DDA(Xhere,Yhere, X1, Y1,function ANN!.dotc);
     Xhere :=X1; Yhere :=Y1>>;
   
Procedure  Idl2chl(X);	% Convert Idlist To Char List
   Begin scalar Y;
      While Pairp(X) do <<Y := getv(Sfromid car X, 1) . Y;X := Cdr X >>;
      Return(Reverse(Y))
   end;

FLUID '(Tchars);

Procedure  Texter(X1,Y1,X2,Y2,Txt);
   Begin scalar Tchars;
      Tchars := Idl2chl(Explode2(Txt));
      Return(ANN!.DDA(X1,Y1,X2,Y2,function ANN!.Tdotc))
   end;

Procedure  ANN!.Tdotc(X1,Y1);
   Begin 
      If Null Tchars then Return(Nil);
      If(X1 > X2clip) Or(X1 < X1clip) then Goto No;
      If(Y1 > Y2clip) Or(Y1 < Y1clip) then Goto No;
      ANN!.ChPrt(X1 , Y1,Car Tchars);
   No:Tchars := Cdr Tchars;
      Return('T)
   end;

Procedure  ANN!.dotc(X1,Y1);	% Draw And Clip An X
   ANN!.ChClip(X1,Y1,Char !*) ;
  
Procedure  ANN!.ChClip(X1,Y1,Id);
   Begin 
      If(X1 > X2clip) Or(X1 < X1clip) then Goto No;
      If(Y1 > Y2clip) Or(Y1 < Y1clip) then Goto No;
      ANN!.ChPrt(X1 , Y1,Id);
   No:Return('T)
   end;

Procedure ANN!.VwPort(X1,X2,Y1,Y2);
   <<X1clip := Max2(-40,X1); 
     X2clip := Min2(40,X2);
     Y1clip := Max2(-30,Y1);
     Y2clip := Min2(30,Y2)>>;

Procedure  ANN!.Wfill(X1,X2,Y1,Y2,Id);
   Begin scalar X,Y;
      For Y := Y1 : Y2 do 
       For X := X1 : X2 do ANN!.ChClip(X,Y,Id);
   end;

Procedure  ANN!.Wzap(X1,X2,Y1,Y2);
   ANN!.Wfill(X1,X2,Y1,Y2,'! ) ;

Procedure ANN!.Delay;
 NIL;

Procedure ANN!.GRAPHON();
 If not !*emode then echooff();

Procedure ANN!.GRAPHOFF();
 If not !*emode then echoon();

Procedure ANN!.INIT();	% Setup For ANN As Device;
 Begin
      Dev!. := 'ANN60; 
      FNCOPY('EraseS,'ANN!.EraseS);
      FNCOPY('MoveS,'ANN!.MoveS);
      FNCOPY('DrawS,'ANN!.DrawS);
      FNCOPY('NormX, 'ANN!.NormX)$                
      FNCOPY('NormY, 'ANN!.NormY)$                
      FNCOPY('VwPort,'ANN!.VwPort); 
      FNCOPY('Delay,'ANN!.Delay);
      FNCOPY('GraphOn, 'ANN!.GraphOn)$
      FNCOPY('GraphOff, 'ANN!.GraphOff)$
      Erase();
      VwPort(-40,40,-30,30);
      Print "Device Now ANN60";
  end;

