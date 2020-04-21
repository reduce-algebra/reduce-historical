%. PRLISP-DRIVER.RED   Terminal/Graphics Drivers for PRLISP
%. Date: ~December 1981
%. Authors: M.L. Griss, F. Chen, P. Stay
%.           Utah Computation Group
%.           Department of Computer Science
%.           University of Utah, Salt Lake City.
%. Copyright (C) University of Utah 1982

% Also, need either EMODE or RAWIO files for EchoON/EchoOff

% Note that under EMODE (!*EMODE= T), EchoOn and EchoOff
% Already Done, so GraphOn and GraphOff need to test !*EMODE

% csp 7/13/82
% Change to only set !*EMODE to NIL if it is unbound.

FLUID '(!*EMODE);
% initialize emode to off
loadtime <<if UnboundP '!*EMODE then !*EMODE:=NIL;>>;


		%***************************
		%  setup functions for     *
		%  terminal devices        *
		%***************************

FLUID '(!*UserMode);

Procedure FNCOPY(NewName,OldName)$          %. to copy equivalent 
 Begin scalar !*UserMode;
   CopyD(NewName,OldName);
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
  MOVETOPOINT ORIGIN >>$

Procedure HP!.NormX XX$               %. absolute position along 
  FIX(XX+0.5)+360$                    % X axis
                                            
Procedure HP!.NormY YY$               %. absolute position along 
  FIX(YY+0.5)+180$                    % Y axis.

Procedure HP!.MoveS (XDEST,YDEST)$    %. move pen to absolute location
<< HP!.GRCMD("d")$
   X := HP!.NormX XDEST$
   Y := HP!.NormY YDEST$
   HP!.OutInt HP!.NormX XDEST$
   HP!.OutChar Char '!,$
   HP!.OutInt HP!.NormY YDEST$
   HP!.OutCharString "oZ"$
   HP!.GRCMD("pacZ") >>$

Procedure HP!.DrawS (XDEST,YDEST)$       %. MoveS pen to the pen position
      <<HP!.GRCMD("d")$
        X := HP!.NormX XDEST$            %. destination and  draw a 
        Y := HP!.NormY YDEST$
	HP!.OutInt HP!.NormX XDEST$      %. line to it rom previous
	HP!.OutChar Char '!,$            %. pen position.             
	HP!.OutInt HP!.NormY YDEST$           
	HP!.OutCharString "oZ"$
	HP!.GRCMD("pbcZ")$'NIL>>$
 
Procedure HP!.CRSRWT()$                   %. waiting for input a 
Begin scalar P,C1,C2,a$                            %. character to position 
      HP!.GRCMD("s4^")$                            %. a cursor. 
      C1:= READ()$ 
      C2:= READ()$ 
      a := READ()$
      P := LIST ('POINT,C1-360,C2-180,HEREPOINT[3])$
      HP!.GRCMD("dkZ")$
      Return a.P$
   end$

Procedure HP!.BUILDP()$                    %. builds a list of 
Begin scalar PNTLST,UNFINISHED,PNT,PNT2,ACT,GRP,    %. points from cursor
      PRVPNT,RAD$                                   %. MoveS.
      UNFINISHED := 'T$                              
      PNTLST := LIST(HERE,'POINTSET)$        
      GRP  := LIST('GROUP)$                    
      While UNFINISHED do 
         <<UNFINISHED := HP!.CRSRWT()$
           HP!.OutInt UNFINISHED$
           ACT := CAR1 UNFINISHED$
           PNT := PRLISPCDR UNFINISHED$
           HP!.OutInt PNT$HP!.OutInt ACT$

           If ACT = 32 then                         % draw : using "space-bar"
              <<DrawModel PNT$                           % key.
                PNTLST :=PNT . PNTLST>>

           else If ACT = 127 then                   % move : using "del" key.
              <<MOVEPOINT (PRLISPCDR PNT)$
                PNTLST := REVERSE PNTLST$
                GRP := PNTLST . GRP $
		PNTLST := LIST (PNT,'POINTSET)>>

          else If ACT = 67 then                    % draw circle around center 
            <<PNT2 := POINT                        % passing through cursor 
                      (NILTOZERO CAR2 PNT,       % using "uppercase c" key.
                       NILTOZERO CAR3  PNT)$
              RAD := DISTANCE(CCNTR, PNT2)$
		DRAWCIRCLE(LIST RAD)$
                PNT := LIST('CIRCLE,RAD)$
                PNTLST := PNT . PNTLST >>

          else If ACT = 99 then                    % sets circle center : 
              <<MOVEPOINT (PRLISPCDR PNT)$         % using "lowercase c" key.
                SETCENTER LIST PNT$
                PNTLST := LIST('CENTER,PNT) . PNTLST >>

                                    
          else If ACT = 13 then                    % finish : using "Return" 
              <<UNFINISHED := NIL$                 % key.
		GRP := REVERSE PNTLST . GRP >>
           >>$
      Return REVERSE GRP$
end$

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
     FNCOPY( 'NormX, 'HP!.NormX)$                   % initialization when 
     FNCOPY( 'NormY, 'HP!.NormY)$                   % using HP2648A.
     FNCOPY( 'MoveS, 'HP!.MoveS)$
     FNCOPY( 'DrawS, 'HP!.DrawS)$
     FNCOPY( 'CRSRWT, 'HP!.CRSRWT)$
     FNCOPY( 'VWPORT, 'HP!.VWPORT)$
     FNCOPY( 'Delay,  'HP!.Delay)$
     FNCOPY( 'GraphOn, 'HP!.GraphOn)$
     FNCOPY( 'GraphOff, 'HP!.GraphOff)$
     Erase()$                          
     VWPORT(-800,800,-800,800)$
     GLOBAL!.TRANSFORM := WINdoW(-300,60)
end$


        % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %    TEKTRONIX specIfic Procedures      %
        % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Procedure TEK!.OutChar x;
  Pbout x;

Procedure TEK!.EraseS();           %. EraseS screen, Returns terminal 
  <<TEK!.OutChar Char ESC;         %. to Alpha mode and places cursor.
    TEK!.OutChar Char FF>>;

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
    TEK!.OutChar 31>> $                   %. US: sets terminal to Alpha mode.

Procedure TEK!.DrawS (XDEST,YDEST)$    %. Same as Tek!.MoveS but 
<< TEK!.OutChar 29$                                %. draw the line.
   TEK!.4BYTES (CAR2 HERE, CAR3 HERE)$
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
     GLOBAL!.TRANSFORM := WINdoW(-300,60)
end$

        % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %    TELERAY specIfic Procedures      %
        % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Basic Teleray 1061 Plotter
%	Screen Range Is X :=  (-40,40) :=  (Left .  . Right)
%			Y :=  (-12,12) :=  (Top .  . Bottom)

Procedure TEL!.OutChar x;
  PBOUT x;

Procedure TEL!.OutCharString S;		% Pbout a string
  For i:=0:Size S do TEL!.OutChar S[i];

Procedure TEL!.NormX X;
  FIX(X)+40;

Procedure TEL!.NormY Y;
  FIX(Y)+12;

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
  << TEL!.DDA (Xhere,Yhere, X1, Y1,function dotc);
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

Procedure  dotc   (X1,Y1);	% Draw And Clip An X
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

		%**********************************
		% MPS device routines will only   *
		% work If the MPS C library is    *
		% resident in the system          *
		% contact Paul Stay or Russ Fish  *
		%    University of Utah           *
		%**********************************


Procedure MPS!.DrawS (XDEST, YDEST);
<<
	X := XDEST;
	Y := YDEST;
	PSdraw2d(LIST(X,Y) ,DDDD,ABS,0,1);	%draw a line from cursor
	0;					%do x and y coordinates
>>;

Procedure MPS!.MoveS (XDEST, YDEST);
<<
	X := XDEST;
	Y := YDEST;
	PSdraw2d( LIST(X,Y) , MDDD,ABS,0,1);	%move to point x,y
	0;
>>;

Procedure MPS!.Delay();		% no Delay function for mps
	NIL;

Procedure MPS!.EraseS();		% setdisplay list to nil 
	DISPLAY!.LIST := NIL$

Procedure MPS!.VWPORT( X1, X2, Y1, Y2); %set up viewport
<<
        PSsetscale(300);			%set up scale factor
	X1CLIP := MAX2(-500, X1);
	X2CLIP := MIN2(500, X2);
	Y1CLIP := MAX2(-500, Y1);
	Y2CLIP := MIN2(500, Y2);
>>;

Procedure MPS!.GRAPHON();                     % Check this
If not !*emode then echooff();

Procedure MPS!.GRAPHOFF();
If not !*emode then echoon();

Procedure MPS!.INIT$
<<
	PRINT "MPS IS DISPLAY DEVICE";
	DEV!. := 'MPS;
	FNCOPY ( 'EraseS, 'MPS!.ERASE)$
% Add NORM functions
	FNCOPY ( 'MoveS, 'MPS!.MoveS)$
	FNCOPY ( 'DrawS, 'MPS!.DrawS)$
	FNCOPY ( 'VWPORT, 'MPS!.VWPORT)$
	FNCOPY ( 'Delay, 'MPS!.Delay)$
        FNCOPY( 'GraphOn, 'MPS!.GraphOn)$
        FNCOPY( 'GraphOff, 'MPS!.GraphOff)$
	PSINIT(1,0);				% initialize device
        ERASE();
	MPS!.VWPORT(-500,500,-500,500);		% setup viewport
	Psscale(1,1,1,500);			% setup scale hardware
	GLOBAL!.TRANSFORM := WINdoW(-300,60);
>>;

	%***************************************
	% Apollo terminal driver and functions *
	%***************************************

Procedure ST!.OutChar x;			 % use Pbout instead
 PBOUT x;

Procedure ST!.EraseS();			% erase screen
<< ST!.OutChar 27;
   ST!.OutChar 12>>;

Procedure ST!.GraphOn();
<< If Not !*Emode Then EchoOff();
   If !*emode then ST!.OutChar 29>>$        % Should be same for TEK

Procedure ST!.GraphOff();
<< If Not !*Emode Then EchoOn();
   If !*emode then ST!.OutChar 31>>$        % Maybe mixed VT-52/tek problem


Procedure ST!.MoveS(XDEST,YDEST)$ 
<< ST!.OutChar 29 $                 %. GS: sets terminal to Graphic mode.
   ST!.4BYTES (XDEST,YDEST)$        %. US: sets terminal to Alpha mode.
   If not !*emode then ST!.OutChar 31>>$

Procedure ST!.DrawS (XDEST,YDEST)$    %. Same as MoveS but 
<< If not !*emode then << ST!.OutChar 29$ 
			  ST!.4bytes(car2 here, car3 here)>>$
   ST!.4BYTES (XDEST, YDEST)$               %. draw the line.
   If not !*emode then ST!.OutChar 31 >>$

Procedure PRLISP();
  <<PRIN2T "Set Up for Apollo under EMODE";
    !*Emode:=T;
    ST!.INIT()>>;

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
     GLOBAL!.TRANSFORM := WINdoW(-300,60)
end$


% --------- OTHER UTILITIES ------------

Procedure SAVEPICT (FIL,PICT,NAM)$         %. save a picture with no 
Begin scalar OLD;                                   %. vectors.    
      FIL := OPEN (FIL,'OUTPUT)$                    % fil : list('dir,file.ext)
      OLD := WRS FIL$                               % nam : id 
      PRIN2 NAM$ PRIN2 '! !:!=! !'$ PRIN2 PICT$     % pict: name of pict to 
      PRIN2 '!;$ WRS 'NIL$ CLOSE FIL$               %       be saved.
      Return PICT$                        
                                                    %  fil: file name to save 
                                                    %       "pict".
end$                                                %  nam: name to be used 
                                                    %       after TAILore.
                                                    %  type "in fil" to TAILore
                                                    %  old picture.







