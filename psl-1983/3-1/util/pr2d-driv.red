%---------------------------------

%. PRLISP-DRIVER.RED   Terminal/Graphics Drivers for PRLISP
%. Date: ~December 1981
%. Authors: M.L. Griss, F. Chen, P. Stay
%.           Utah Symbolic Computation Group
%.           Department of Computer Science
%.           University of Utah, Salt Lake City.
%. Copyright (C) University of Utah 1982

% Also, need either EMODE or RAWIO files for EchoON/EchoOff

% Note that under EMODE (!*EMODE= T), EchoOn and EchoOff
% Already Done, so GraphOn and GraphOff need to test !*EMODE

FLUID '(!*EMODE);
loadtime <<!*EMODE:=NIL;>>;			% initialize emode to off


		%***************************
		%  setup functions for     *
		%  terminal devices        *
		%***************************

FLUID '(!*UserMode);

Procedure FNCOPY(NewName,OldName)$          %. to copy equivalent 
 Begin scalar !*UserMode;
   CopyD(NewName,OldName);
 end;

Procedure  DDA   (X1,Y1,X2,Y2,dotter);
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

      % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      %          hp specific Procedures             %
      % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Procedure HP!.OutChar x;               % Raw Terminal I/O
 Pbout x;

Procedure HP!.OutCharString S;		% Pbout a string
  For i:=0:Size S do HP!.OutChar S[i];

Procedure HP!.grcmd (acmd)$           %. prefix to graphic command
<<HP!.OutChar char ESC$			       
  HP!.OutChar char !*$
  HP!.OutCharString ACMD$
  DELAY() >>$


Procedure HP!.OutInt X;			% Pbout a integer
 <<HP!.OutChar (char !0 + (X/100));
   X:=Remainder(x,100);
   HP!.OutChar (char !0 + (x/10));
   HP!.OutChar (char !0+Remainder(x,10));
	nil>>;

Procedure HP!.Delay$                  %. Delay to wait for the display
 HP!.OutChar CHAR EOL;                % Flush buffer

Procedure HP!.EraseS()$               %. EraseS graphic diaplay screen
<<HP!.GRCMD("dack")$                       
  MoveToXY(0,0)>>;

Procedure HP!.Erase()$               %. EraseS graphic diaplay screen
 <<HP!.GraphOn();  HP!.Erases(); HP!.GraphOff()>>;

Procedure HP!.NormX XX$               %. absolute position along 
  FIX(XX+0.5)+360$                    % X axis
                                            
Procedure HP!.NormY YY$               %. absolute position along 
  FIX(YY+0.5)+180$                    % Y axis.

Procedure HP!.MoveS (XDEST,YDEST)$    %. Move pen to absolute location
<< HP!.GRCMD("d")$
   HP!.OutInt HP!.NormX XDEST$
   HP!.OutChar Char '!,$
   HP!.OutInt HP!.NormY YDEST$
   HP!.OutCharString "oZ"$
   HP!.GRCMD("pacZ") >>$

Procedure HP!.DrawS (XDEST,YDEST)$       %. MoveS pen to the pen position
      <<HP!.GRCMD("d")$
	HP!.OutInt HP!.NormX XDEST$      %. line to it rom previous
	HP!.OutChar Char '!,$            %. pen position.             
	HP!.OutInt HP!.NormY YDEST$           
	HP!.OutCharString "oZ"$
	HP!.GRCMD("pbcZ")$'NIL>>$
 
Procedure HP!.VWPORT(X1,X2,Y1,Y2)$         %. set the viewport
<< X1CLIP := MAX2 (-360,X1)$                        %. for HP2648A terminal.
   X2CLIP := MIN2 (360,X2)$
   Y1CLIP := MAX2 (-180,Y1)$
   Y2CLIP := MIN2 (180,Y2) >>$

Procedure HP!.GRAPHON();                 %. No special GraphOn/GraphOff
  If not !*emode then echooff();

Procedure HP!.GRAPHOFF();
  If not !*emode then echoon();

Procedure HP!.INIT$                        %. HP device specIfic 
Begin                                               %. Procedures equivalent.
     PRINT "HP IS DEVICE"$
     DEV!. := 'HP;
     FNCOPY( 'EraseS, 'HP!.EraseS)$              % should be called as for
     FNCOPY( 'Erase, 'HP!.Erase)$              % should be called as for
     FNCOPY( 'NormX, 'HP!.NormX)$                   % initialization when 
     FNCOPY( 'NormY, 'HP!.NormY)$                   % using HP2648A.
     FNCOPY( 'MoveS, 'HP!.MoveS)$
     FNCOPY( 'DrawS, 'HP!.DrawS)$
     FNCOPY( 'VWPORT, 'HP!.VWPORT)$
     FNCOPY( 'Delay,  'HP!.Delay)$
     FNCOPY( 'GraphOn, 'HP!.GraphOn)$
     FNCOPY( 'GraphOff, 'HP!.GraphOff)$
     Erase()$                          
     VWPORT(-800,800,-800,800)$
     GLOBAL!.TRANSFORM := MAT!*1;
end$


        % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %    TEKTRONIX specIfic Procedures      %
        % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Procedure TEK!.OutChar x;
  Pbout x;

Procedure TEK!.EraseS();           %. EraseS screen, Returns terminal 
  <<TEK!.OutChar Char ESC;         %. to Alpha mode and places cursor.
    TEK!.OutChar Char FF>>;

Procedure TEK!.EraseS();           %. EraseS screen, Returns terminal 
  <<Tek!.GraphOn(); Tek!.Erases(); TEK!.GraphOff()>>;


Procedure TEK!.4BYTES (XDEST, YDEST)$    %. Convert graphic plot 
<< TEK!.OutChar HIGHERY NormY YDEST$                 %. information to the
   TEK!.OutChar LOWERY NormY YDEST$                  %. terminal in a 4 byte 
   TEK!.OutChar HIGHERX NormX XDEST$                 %. sequences containing the 
   TEK!.OutChar LOWERX NormX XDEST >>$               %. High and Low order Y 
                                                  %. informationand High and
                                                  %. Low order X information.

Procedure HIGHERY YDEST$            %. convert Y to higher order Y.
FIX(YDEST) / 32 + 32$

Procedure LOWERY YDEST$             %. convert Y to lower order Y.  
  REMAINDER (FIX YDEST,32) + 96$


Procedure HIGHERX XDEST$            %. convert X to higher order X.
  FIX(XDEST) / 32 + 32$

Procedure LOWERX XDEST$             %. convert X to lower order X.  
  REMAINDER (FIX XDEST,32) + 64$


Procedure TEK!.MoveS(XDEST,YDEST)$ 
  <<TEK!.OutChar 29 $                     %. GS: sets terminal to Graphic mode.
    TEK!.4BYTES (XDEST,YDEST)$
%/ Dont do 31 unless go back to text mode
    TEK!.OutChar 31>> $                   %. US: sets terminal to Alpha mode.

Procedure TEK!.DrawS (XDEST,YDEST)$    %. Same as Tek!.MoveS but 
<< TEK!.OutChar 29$                                %. Draw the line.
   TEK!.4BYTES (HerePointX, HerePointY)$
 %/ Can just do this, ignore reset TEXT or GRPAHICS mode, see ST!
   TEK!.4BYTES (XDEST, YDEST)$
   TEK!.OutChar 31>> $

Procedure TEK!.NormX DESTX$               %. absolute location along
 DESTX + 512$                                      %. X axis.

Procedure TEK!.NormY DESTY$               %. absolute location along 
 DESTY + 390$                                      %. Y axis.

Procedure TEK!.VWPORT(X1,X2,Y1,Y2)$       %. set the viewport for
 <<  X1CLIP := MAX2 (-512,X1)$                     %. Tektronix 4006-1.
     X2CLIP := MIN2 (512,X2)$
     Y1CLIP := MAX2 (-390,Y1)$
     Y2CLIP := MIN2 (390,Y2) >>$

Procedure TEK!.Delay();
 NIL;

Procedure TEK!.GRAPHON();          %. No special GraphOn (? what of GS/US)
If not !*emode then echooff();

Procedure TEK!.GRAPHOFF();
If not !*emode then echoon();

Procedure TEK!.INIT$                %. TEKTRONIX device specIfic 
Begin                                        %. Procedures equivalent.
     PRINT "TEKTRONIX IS DEVICE"$
     DEV!. := ' TEK;
     FNCOPY( 'EraseS, 'TEK!.EraseS)$            % should be called as for 
     FNCOPY( 'Erase, 'TEK!.Erase)$            % should be called as for 
     FNCOPY( 'NormX, 'TEK!.NormX)$           % initialization when using 
     FNCOPY( 'NormY, 'TEK!.NormY)$           % Tektronix 4006-1.  
     FNCOPY( 'MoveS, 'TEK!.MoveS)$
     FNCOPY( 'DrawS, 'TEK!.DrawS)$
     FNCOPY( 'VWPORT, 'TEK!.VWPORT)$
     FNCOPY( 'Delay, 'TEK!.Delay)$
     FNCOPY( 'GraphOn, 'TEK!.GraphOn)$
     FNCOPY( 'GraphOff, 'TEK!.GraphOff)$
     Erase()$                     
     VWPORT(-800,800,-800,800)$
     GLOBAL!.TRANSFORM := MAT!*1;
end$

        % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %    TELERAY specIfic Procedures      %
        % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Basic Teleray 1061 Plotter
%	Screen Range Is X :=  (-40,40) :=  (Left .  . Right)
%			Y :=  (-12,12) :=  (Bottom .  . Top)

Procedure TEL!.OutChar x;
  PBOUT x;

Procedure TEL!.OutCharString S;		% Pbout a string
  For i:=0:Size S do TEL!.OutChar S[i];

Procedure TEL!.NormX X;
  FIX(X+0.5)+40;

Procedure TEL!.NormY Y;
  12- FIX(Y+0.5);

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

Procedure TEL!.Erase   ();	% Delete Entire Screen
  <<TEL!.GraphON(); TEL!.Erases(); TEL!.GraphOff()>>;


Procedure Tel!.MoveS   (X1,Y1);
   <<Xprevious := X1;
     Yprevious := Y1>>;

Procedure Tel!.DrawS   (X1,Y1);
  << DDA (Xprevious,Yprevious, X1, Y1,function TEL!.dotc);
     Xprevious :=X1; Yprevious :=Y1>>;
   
Procedure  Idl2chl   (X);	% Convert Idlist To Char List
   Begin scalar Y;
      While Pairp (X) do <<Y := getv (Sfromid car X, 1) . Y;X := Cdr X >>;
      Return (Reverse (Y))
   end;

FLUID '(Tchars);

Procedure  Texter   (X1,Y1,X2,Y2,Txt);
   Begin scalar Tchars;
      Tchars := Idl2chl (Explode2 (Txt));
      Return (DDA (X1,Y1,X2,Y2,function TEL!.Tdotc))
   end;

Procedure  TEL!.Tdotc   (X1,Y1);
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
      FNCOPY('Erase,'TEL!.Erase);
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
      Global!.Transform := MAT!*1;
      Print "Device Now TEL";
  end;

%  Basic ANN ARBOR AMBASSADOR Plotter
%
%	Screen Range Is X :=  (-40,40) :=  (Left .  . Right)
%			Y :=  (-30,30) :=  (Top .  . Bottom)

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

Procedure ANN!.Erase();
 <<ANN!.Graphon(); ANN!.Erases(); Ann!.GraphOff()>>;

Procedure ANN!.MoveS(X1,Y1);
   <<Xprevious := X1;
     Yprevious := Y1>>;

Procedure ANN!.DrawS(X1,Y1);
  << DDA(Xprevious,Yprevious, X1, Y1,function ANN!.dotc);
     Xprevious :=X1; Yprevious :=Y1>>;
   
Procedure  Idl2chl(X);	% Convert Idlist To Char List
   Begin scalar Y;
      While Pairp(X) do <<Y := getv(Sfromid car X, 1) . Y;X := Cdr X >>;
      Return(Reverse(Y))
   end;

FLUID '(Tchars);

Procedure  Texter(X1,Y1,X2,Y2,Txt);
   Begin scalar Tchars;
      Tchars := Idl2chl(Explode2(Txt));
      Return(DDA(X1,Y1,X2,Y2,function ANN!.Tdotc))
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
      FNCOPY('Erase,'ANN!.Erase);
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
      Global!.Transform := Mat!*1;
      Print "Device Now ANN60";
  end;

	%***************************************
	% Apollo terminal driver and functions *
	%***************************************

Procedure ST!.OutChar x;			 % use Pbout instead
 PBOUT x;

Procedure ST!.EraseS();			% erase screen
<< GraphOff();
   ST!.OutChar 27;
   ST!.OutChar 12;
   Graphon()>>;

Procedure ST!.Erase();			% erase screen
<< EchoOff();
   ST!.OutChar 27;
   ST!.OutChar 12;
   If Not !*EMODE then EchoOn()>>;


Procedure ST!.GraphOn();
<< EchoOff();
   ST!.OutChar 29>>$        % Should be same for TEK

Procedure ST!.GraphOff();
<<ST!.OutChar 31$        % Maybe mixed VT-52/tek problem
  If Not !*Emode Then EchoOn()>>;   


Procedure ST!.MoveS(XDEST,YDEST)$ 
<< ST!.OutChar 29 $                 %. GS: sets terminal to Graphic mode.
   ST!.4BYTES (XDEST,YDEST)$        %. US: sets terminal to Alpha mode.
>>;

Procedure ST!.DrawS (XDEST,YDEST)$    %. Same as MoveS but 
<< %/ ST!.OutChar 29$  % Always after move
   %/ ST!.4bytes(HerePointX, HerePointY)>>$
   ST!.4BYTES (XDEST, YDEST)$               %. Draw the line.
 >>;

Procedure ST!.4BYTES (XDEST, YDEST)$    %. Convert graphic plot 
<< ST!.OutChar HIGHERY NormY YDEST$            %. information to the
   ST!.OutChar LOWERY NormY YDEST$             %. terminal in a 4 byte 
   ST!.OutChar HIGHERX NormX XDEST$            %. sequences containing the 
   ST!.OutChar LOWERX NormX XDEST >>$          %. High and Low order Y 
                                                  %. informationand High and
                                                  %. Low order X information.
Procedure ST!.Delay();
 NIL;

Procedure ST!.NormX DESTX$               %. absolute location along
 DESTX + 400$                                      %. X axis.

Procedure ST!.NormY DESTY$               %. absolute location along 
 DESTY + 300$                                      %. Y axis.

Procedure ST!.VWPORT(X1,X2,Y1,Y2)$       %. set the viewport for
 <<  X1CLIP := MAX2 (-400,X1)$                     %. Tektronix 4006-1.
     X2CLIP := MIN2 (400,X2)$
     Y1CLIP := MAX2 (-300,Y1)$
     Y2CLIP := MIN2 (300,Y2) >>$

Procedure ST!.INIT$                 %. JW's fake TEKTRONIX
Begin                                       %. Procedures equivalent.
     PRINT "Apollo/ST is device"$
     DEV!. := 'Apollo;
     FNCOPY( 'EraseS, 'ST!.EraseS)$            % should be called as for 
     FNCOPY( 'Erase, 'ST!.Erase)$            % should be called as for 
     FNCOPY( 'NormX, 'ST!.NormX)$           % initialization when using 
     FNCOPY( 'NormY, 'ST!.NormY)$           % APOtronix 4006-1.  
     FNCOPY( 'MoveS, 'ST!.MoveS)$
     FNCOPY( 'DrawS, 'ST!.DrawS)$
     FNCOPY( 'VWPORT, 'ST!.VWPORT)$
     FNCOPY( 'Delay, 'ST!.Delay)$
     FNCOPY( 'GraphOn, 'ST!.GraphOn);
     FNCOPY( 'GraphOff, 'ST!.GraphOff);
     Erase()$                     
     VWPORT(-400,400,-300,300)$
     GLOBAL!.TRANSFORM := MAT!*1;
end$


        % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %    HP2382 specIfic Procedures      %
        % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Basic Hp2382  Plotter
%	Screen Range Is X :=  (-40,40) :=  (Left .  . Right)
%			Y :=  (-12,12) :=  (Bottom .  . Top)

Procedure HP2382!.OutChar x;
  PBOUT x;

Procedure HP2382!.OutCharString S;		% Pbout a string
  For i:=0:Size S do HP2382!.OutChar S[i];

Procedure HP2382!.NormX X;
  FIX(X+0.5)+40;

Procedure HP2382!.NormY Y;
  12- FIX(Y+0.5);

Procedure  HP2382!.ChPrt(X,Y,Ch);
   <<HP2382!.OutChar Char ESC;
     HP2382!.OutChar Char '!&;
     HP2382!.OutChar Char '!a;

     HP2382!.OutINT (HP2382!.NormY Y);
     HP2382!.OutChar Char '!r;
     HP2382!.OutINT (HP2382!.NormX X);
     HP2382!.OutChar Char '!C;
     HP2382!.OutChar Ch>>;

procedure HP2382!.OutINT x;
 <<If x>9 then HP2382!.OutChar(Char 0 +(x/10));
   HP2382!.OutChar(Char 0 +remainder(x,10))>>;

Procedure  HP2382!.IdPrt(X,Y,Id);
    HP2382!.ChPrt(X,Y,ID2Int ID);

Procedure  HP2382!.StrPrt   (X,Y,S);
   <<HP2382!.OutChar Char ESC;
     HP2382!.OutChar 89;
     HP2382!.OutChar (32+HP2382!.NormY Y);
     HP2382!.OutChar (32+ HP2382!.NormX X);
     HP2382!.OutCharString  S>>;

Procedure  HP2382!.HOME   ();	% Home  (0,0)
  <<HP2382!.OutChar CHAR ESC;
    HP2382!.OutChar 'H>>;

Procedure HP2382!.EraseS   ();	% Delete Entire Screen
  <<HP2382!.HOME();
    HP2382!.OutChar CHAR ESC;
    HP2382!.OutChar 'J>>;

Procedure HP2382!.Erase   ();	% Delete Entire Screen
  <<HP2382!.GraphON(); HP2382!.Erases(); HP2382!.GraphOff()>>;


Procedure HP2382!.MoveS   (X1,Y1);
   <<Xprevious := X1;
     Yprevious := Y1>>;

Procedure HP2382!.DrawS   (X1,Y1);
  << DDA (Xprevious,Yprevious, X1, Y1,function HP2382!.dotc);
     Xprevious :=X1; Yprevious :=Y1>>;
   
Procedure  Idl2chl   (X);	% Convert Idlist To Char List
   Begin scalar Y;
      While Pairp (X) do <<Y := getv (Sfromid car X, 1) . Y;X := Cdr X >>;
      Return (Reverse (Y))
   end;

FLUID '(Tchars);

Procedure  Texter   (X1,Y1,X2,Y2,Txt);
   Begin scalar Tchars;
      Tchars := Idl2chl (Explode2 (Txt));
      Return (DDA (X1,Y1,X2,Y2,function HP2382!.Tdotc))
   end;

Procedure  HP2382!.Tdotc   (X1,Y1);
   Begin 
      If Null Tchars then Return (Nil);
      If  (X1 > X2clip) Or  (X1 < X1clip) then Goto No;
      If  (Y1 > Y2clip) Or  (Y1 < Y1clip) then Goto No;
      HP2382!.ChPrt (X1 , Y1,Car Tchars);
   No:Tchars := Cdr Tchars;
      Return ('T)
   end;

Procedure  HP2382!.dotc   (X1,Y1);	% Draw And Clip An X
 HP2382!.ChClip (X1,Y1,Char X) ;

Procedure  HP2382!.ChClip   (X1,Y1,Id);
   Begin 
      If  (X1 > X2clip) Or  (X1 < X1clip) then Goto No;
      If  (Y1 > Y2clip) Or  (Y1 < Y1clip) then Goto No;
      HP2382!.ChPrt (X1 , Y1,Id);
   No:Return ('T)
   end;

Procedure HP2382!.VwPort(X1,X2,Y1,Y2);
   <<X1clip := Max2 (-40,X1); 
     X2clip := Min2 (40,X2);
     Y1clip := Max2 (-12,Y1);
     Y2clip := Min2 (12,Y2)>>;

Procedure  HP2382!.Wfill   (X1,X2,Y1,Y2,Id);
   Begin scalar X,Y;
      For Y := Y1 : Y2 do 
       For X := X1 : X2 do HP2382!.ChClip (X,Y,Id);
   end;

Procedure  HP2382!.Wzap   (X1,X2,Y1,Y2);
   HP2382!.Wfill (X1,X2,Y1,Y2,'! ) ;

Procedure HP2382!.Delay;
 NIL;

Procedure HP2382!.GRAPHON();
If not !*emode then echooff();

Procedure HP2382!.GRAPHOFF();
If not !*emode then echoon();

Procedure HP2382!.INIT  ();	% Setup For TEL As Device;
 Begin
      Dev!. := 'TEL; 
      FNCOPY('EraseS,'HP2382!.EraseS);
      FNCOPY('Erase,'HP2382!.Erase);
      FNCOPY('MoveS,'HP2382!.MoveS);
      FNCOPY('DrawS,'HP2382!.DrawS);
      FNCOPY( 'NormX, 'HP2382!.NormX)$                
      FNCOPY( 'NormY, 'HP2382!.NormY)$                
      FNCOPY('VwPort,'HP2382!.VwPort); 
      FNCOPY('Delay,'HP2382!.Delay);
      FNCOPY( 'GraphOn, 'HP2382!.GraphOn)$
      FNCOPY( 'GraphOff, 'HP2382!.GraphOff)$
      Erase();
      VwPort (-40,40,-12,12);
      Global!.Transform := MAT!*1;
      Print "Device Now TEL";
  end;

