%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                %
%  PictureRLISP : A Lisp-Based Graphics Language System with     %
%                      Flexible Syntax and Hierarchical          %
%                           Data Structure                       %
% 2D version................
%
%  Author: Fuh-Meei Chen, Paul Stay and Martin L. Griss          %
%	       Symbolic Computation Group			 %
%              Computer Science Dept.				 %
%              University of Utah                                %
%                                                                %  
%  <PSL.UTIL>PRLISP.RED.21,  9-Jan-82 22:47:43, Edit by GRISS	 %
%  <STAY.PICT>PRLISP.B       12-april-82 8:00:00 by Paul Stay    %
%  changed bezier circle and bspline drivers and hp terminal     %
%  on 10-april-82 by Paul Stay					 %
%  Added MPS support software for use on the graphics vax        %
%  Added ST.INIT						 %
%  Copyright (c) 1981 University of Utah			 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Part of the parser to accomplish the Pratt parser written  %
%       in New-Rlisp runs at DEC-20.                           %
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

RemFlag('(MKVECT),'TWOREG);                 %/ Seems in Error
RemProp('!{,'NEWNAM!-OP);                   %. left and right brackets 
RemProp('!},'NEWNAM!-OP);                   %. handling.
RemProp('!{,'NEWNAM);                       %  left and right brackets are
RemProp('!},'NEWNAM);                       %  used to Define points.
Put('!{, 'NEWNAM,'!*LBRAC!*);               
Put('!}, 'NEWNAM,'!*RBRAC!*);               %  Put on to the property list.

DefineROP('!*LBRAC!*,NIL,LBC);              % Define the precedence. 
DefineBOP('!*RBRAC!*,1,0);      

FLUID '(OP);

Procedure LBC X; 
Begin scalar RES; 
      If X EQ '!*RBRAC!* then 
         <<OP := X; RES := '!*EMPTY!*>>
           else RES:= RDRIGHT(2,X);
      If OP EQ '!*RBRAC!* then 
         OP := SCAN()
           else PARERR("Missing } after argument list",NIL); 
      Return  REPCOM('OnePoint,RES)
end;

Procedure REPCOM(TYPE,X); 	%.  Create ARGLIST
   IF EQCAR(X,'!*COMMA!*) THEN  (TYPE . CDR X)
    ELSE IF X EQ '!*EMPTY!* THEN LIST(TYPE)
    ELSE LIST(TYPE,X);


RemProp('!_,'NEWNAM);                            %. underscore handling.
Put('!_,'NEWNAM,'POINTSET);                      %  "_" is used for Pointset. 
DefineBOP('POINTSET,17,18,NARY('POINTSET,X,Y));  


Put('!&,'NEWNAM,'GROUP);                         %. and sign handling.
DefineBOP('GROUP,13,14,NARY('GROUP,X,Y));        % "&" is used for Group.


Put('!|,'NEWNAM,'TRANSFORM);                     %. back slash handling.
DefineROP('TRANSFORM,20,                         % "|" is used for transform.
   If EQCAR(X,'!*COMMA!*) then 
             REPCOM('TRANSFORM,X));
DefineBOP('TRANSFORM,15,16);              

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% conversion of external Procedures to  %
% internal form.                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% **************************************
%  conversion on structures of models. *
% **************************************

NExpr Procedure POINTSET L$              
 'POINTSET .  L$

NExpr Procedure GROUP L$
 'GROUP .  L$

NExpr Procedure TRANSFORM L$
 'TRANSFORM .  L$

% ***********************************
% conversion on interpreter level   *
% Procedures.                       *
% ***********************************

Procedure BSPLINE;         
 LIST 'BSPLINE;                           

Procedure BEZIER;
 LIST 'BEZIER;

Procedure LINE;
 LIST 'LINE;

Procedure CIRCLE(R);
 LIST('CIRCLE,R);

Procedure COLOR N;
 List('Color,N);

Procedure REPEATED(COUNT,TRANS);
  LIST('REPEATED,COUNT,TRANS);

BothTimes <<Procedure MKLIST L$
            'LIST . L; >>;

MACRO Procedure OnePoint L$
   LIST('MKPOINT, MKLIST CDR L)$

MACRO Procedure Mat8 L;
   LIST('LIST2VECTOR, MKLIST (CDR L))$

Procedure Pnt2(X1,X2,X3); % create a vector of a point
  Begin scalar V;
	V:=MKVECT 2;
	V[0]:=X1;
	V[1]:=X2;
	V[2]:=X3;
	Return V;
  end;

% %%%%%%%%%%%%%%%%%%%%%%%%%
%      PAIR KLUDGES       %
% %%%%%%%%%%%%%%%%%%%%%%%%%

Procedure PRLISPCDR  L$                 %. PRLISPCDR of a list.
If PAIRP L then CDR L else 'NIL$

Procedure CAR1 L$                       %. the Car1 element of 
If PAIRP L then CAR L else 'NIL$                 %. a list.

Procedure CAR2 L$                       %. the CAR2 element of 
If LENGTH L > 1 then CADR L else 'NIL$           %. a list.

Procedure CAR3 L$                       %. the CAR3 element of
If LENGTH L > 2 then CADDR L else 'NIL$          %. a list.

Procedure CAR4 L$                       %. the CAR4 element of
If LENGTH L > 3 then CADDDR L else 'NIL$         %. a list.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    interpreter supporting Procedures    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Procedure V!.COPY V1$                    %. Copy a vector
Begin scalar N, V2$
      V2 := MKVECT(N := SIZE V1)$
      FOR I := 0 : N DO  
         V2[I] := V1[I]$   
      Return V2$
end$

                  % *********************
                  %   point primitive   *
                  % *********************

Procedure MKPOINT (POINTLIST)$           %. make a vector form for 
 Begin scalar P,I;
   P:=Pnt2(0,0,1);
   I:=0;
   While PairP PointList and I<=2 do
    <<P[I]:=Car PointList;
      I:=I+1;
      PointList:=Cdr PointList>>;
   Return P
 End;

                  % **************************
                  %  initialize globals and  *
                  %      and  fluids         *
		  %    set up for compiled   *
		  %       version            *
                  % **************************

FLUID '(
        DISPLAY!.LIST		    %. Used for object definition for MPS
        MAT!*0                      %. 3 x 3 Zero Matrix
        MAT!*1                      %. 3 x 3 Unit Matrix
        FirstPoint!*                % FirstPoint of PointSet is MOVED to
        GLOBAL!.TRANSFORM           %. Accumulation Transform
        CURRENT!.TRANSFORM 
	CURRENT!.LINE               %. Line Style
	CURRENT!.COLOR              %. Default Color
        X1CLIP                      % Set by VWPORT for Clipping
        X2CLIP 
        Y1CLIP 
        Y2CLIP 
        ThreeClip                    % Vector to return New Clipped point
        HEREPOINTX                  %/ Same as Xprevious?
        HEREPOINTY
	Xprevious                       % To do  DDA on TEL and AAA 
        Yprevious                       %  Set by Move, used by DRAW
        DEV!.                       % Device Name, set by xxx!.Init()
     )$


Procedure SetUpVariables;           % Intialize Globals and Fluids
 Begin
  MAT!*0 := Mat8 (  0,0,0,
                    0,0,0,
                    0,0,0)$
  MAT!*1 := Mat8 (1,0,0,
                  0,1,0,
                  0,0,1)$                                  % unit matrix.
  GLOBAL!.TRANSFORM := MAT!*1$
  CURRENT!.TRANSFORM := MAT!*1$             % current transformation matrix
                                          % initialized as mat!*1.
  CURRENT!.LINE := 'LINE$
  CURRENT!.COLOR := 'BLACK$
  HEREPOINTX := 0; HEREPOINTY:=0;
  ThreeClip := Vector(0,0,0,0);
  FirstPoint!* := NIL$
  End;

% ---------------- BASIC Moving and Drawing -------------------
% Project from Normalized 3 Vector to X,Y plane

Procedure MoveToXY(X,Y)$        %. Move current cursor to x,y of P
 <<MoveS(X,Y);
   HEREPOINTX := X;
   HEREPOINTY := Y>>$

Procedure DrawToXY(X,Y)$        %. Move cursor to "P" and draw from Previous 
 <<DrawS(X,Y);
   HEREPOINTX := X;
   HEREPOINTY := Y>>$

            % **************************************
            %    clipping-- on 2-D display screen  *
            % **************************************

Smacro procedure MakeThreeClip(X1,Y1,X2,Y2);
 <<ThreeClip[0]:=x1; ThreeClip[1]:=y1;
   ThreeClip[2]:=x2; ThreeClip[3]:=y2;
   ThreeClip>>;

Procedure InView (L);
 NULL(Car L) and NULL(cadr L) and NULL(caddr L) and NULL (cadddr L);

Procedure CLIP2D (x1,y1,x2,y2);   % Iterative Clipper
Begin scalar P1,P2,TMP;
      % Newmann and Sproull 
      P1 := TESTPOINT(x1,y1); % Classify EndPoints, get 4 List
      P2 := TESTPOINT(x2,y2);
      If InView(P1) and InView(P2) then Return MakeThreeClip(x1,y1,X2,Y2);
      WHILE NOT(InView(P1) AND InView(P2) OR LOGICAND(P1,P2)) DO
        << If InView(P1) then % SWAP to get Other END
              <<TMP := P1$ P1 := P2$ P2 := TMP$
                TMP := X1$ X1 := X2$ X2 := TMP$
                TMP := Y1$ Y1 := Y2$ Y2 := TMP>>$
           If CADDDR P1 then 
               <<Y1 := Y1 + ((Y2-Y1)*(X1CLIP-X1)) / (X2-X1)$
                 X1 := X1CLIP>>
           else If CADDR P1 then 
               <<Y1 := Y1 + ((Y2-Y1)*(X2CLIP-X1)) / (X2-X1)$
                 X1 := X2CLIP>>
           else If CADR P1 then
               <<X1 := X1 + ((X2-X1)*(Y1CLIP-Y1)) / (Y2-Y1)$
                 Y1 := Y1CLIP>>
           else If CAR P1 then 
               <<X1 := X1 + ((X2-X1)*(Y2CLIP-Y1)) / (Y2-Y1)$
                 Y1 := Y2CLIP>>$
           P1 := TESTPOINT(X1,Y1)>>; % reTest P1 after clipping
      If Not LOGICAND(P1,P2) then Return MakeThreeClip(X1,Y1,X2,Y2);
      Return NIL 
   end$

Procedure LOGICAND (P1, P2)$                %. logical "and". 
   (CAR P1 AND CAR P2)     OR			     %. use in clipping
   (CADR P1 AND CADR P2)   OR
   (CADDR P1 AND CADDR P2)     OR 
   (CADDDR P1 AND CADDDR P2) $

Procedure TESTPOINT(x,y)$                %. test If "P"  
   LIST (If y > Y2CLIP then T else NIL,      %. inside the viewport.
         If y < Y1CLIP then T else NIL,      %.used in clipping
         If x > X2CLIP then T else NIL,
         If x < X1CLIP then T else NIL)$
 % All NIL if Inside

           % **********************************
           % tranformation matrices           *
           % matrices internal are stored as  *
           % OnePoint = [x y w]               *
           % matrix = [v0 v3 v6               *
           %           v1 v4 v7               *
           %           v2 v5 v8 ]             *
           % **********************************


	%*******************************************************
	%    Matrix Multiplication given two 3 by 3 matricies  *
	%*******************************************************

Procedure  MAT!*MAT   (V1,V2)$	     %. multiplication of matrices.
Mat8 (                                   %  V1 and V2 are 3 by 3 matrices.
  V1[0] * V2[0] + V1[3] * V2[1] + V1[6] * V2[2],
  V1[1] * V2[0] + V1[4] * V2[1] + V1[7] * V2[2],
  V1[2] * V2[0] + V1[5] * V2[1] + V1[8] * V2[2],

  V1[0] * V2[3] + V1[3] * V2[4] + V1[6] * V2[5],
  V1[1] * V2[3] + V1[4] * V2[4] + V1[7] * V2[5],
  V1[2] * V2[3] + V1[5] * V2[4] + V1[8] * V2[5],

  V1[0] * v2[6] + V1[3] * V2[7] + V1[6] * V2[8],
  V1[1] * v2[6] + V1[4] * V2[7] + V1[7] * V2[8],
  V1[2] * v2[6] + V1[5] * V2[7] + V1[8] * V2[8]);




Procedure PNT!*PNT(U,V)$      %. multiplication of matrices 
  U[0] * V[0] +
  U[1] * V[1] +                        %. 1 by 3 and 3 by 1.
  U[2] * V[2] $                        %  Returning a value.



Procedure PNT!*MAT(U,V)$      %. multiplication of matrices 
Begin scalar U0,U1,U2$              %. 1 by 3 with 3 by 3.
      U0 := U[0]$
      U1 := U[1]$                      %  Returning a 1 by 3 vector.
      U2 := U[2]$
      U:=Mkvect 2;
      u[0]:= U0 * V[0] + U1 * V[3] + U2 * V[6];
      u[1]:= U0 * V[1] + U1 * V[4] + U2 * V[7];
      u[2]:= U0 * V[2] + U1 * V[5] + U2 * V[8];
      Return U;
end$

                 % **********************
                 %      translation     *
                 % **********************

Procedure  XMove(TX)$            %. x translation only
   Move (TX,0) $

Procedure  YMove(TY)$            %. y translation only 
   Move (0,TY) $

Procedure  Move(TX,TY)$	     %. Move origin / object$
    Mat8(1, 0, TX,                     %. make a translation 
         0, 1, TY,                     %. transformation  matrix
         0, 0, 1)$

                 % *******************
                 % Z   rotation     *
                 % *******************


Procedure  ZROT(Theta)$              %. rotation about z
 Begin scalar S,C;
      S := SIND (THETA)$		     %. sin in degrees uses mathlib
      C := COSD (THETA)$		     %. cos in degrees uses mathlib
 Return  Mat8( C,-S,0,
               S,C,0,
               0,0,1);
 end $

                 % ******************
                 %      scaling     *
                 % ******************

Procedure  XSCALE   (SX)$          %. scaling along X axis only.
 SCALE1 (SX,1) $

Procedure  YSCALE   (SY)$          %. scaling along Y axis only.
 SCALE1 (1,SY) $

Procedure  SCALE1(XT,YT)$       %. scaling transformation
     Mat8 ( XT, 0, 0,                    %. matrix.
             0 ,YT, 0,
             0, 0, 1)$

Procedure SCALE SFACT;             %. scaling along 2 axes.
  SCALE1(SFACT,SFACT);

              % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
              %       Procedure definitions          %
              %         in the interpreter           %
              % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Put('OnePoint,'PBINTRP,'DrawPOINT)$
Put('POINTSET,'PBINTRP,'DrawPOINTSET)$
Put('GROUP,'PBINTRP,'DrawGROUP)$
Put('TRANSFORM,'PBINTRP,'PERFORMTRANSFORM)$
Put('PICTURE,'PBINTRP,'DrawModel)$
Put('CIRCLE,'PBINTRP,'DrawCIRCLE)$
Put('BEZIER,'PBINTRP,'DOBEZIER)$
Put('LINE,'PBINTRP,'DOLINE)$
Put('BSPLINE,'PBINTRP,'DOBSPLINE)$
Put('REPEATED, 'PBINTRP,'DOREPEATED)$
Put('Color,'pbintrp,'Docolor);

	%******************************************
	%  SETUP Procedure FOR BEZIER AND BSPLINE *
	%      LINE and COLOR
	%******************************************

procedure DoColor(Object,N);
  Begin scalar SaveColor;
	SaveColor:=Current!.color;
        N:=Car1 N;  % See CIRCLE example, huh?
        If IDP N then N:=EVAL N;
	ChangeColor N;
	Draw1(Object,CURRENT!.TRANSFORM);
	ChangeColor SaveColor;
        Return NIL;
 End;

Procedure DOBEZIER OBJECT$
Begin scalar  CURRENT!.LINE$
      CURRENT!.LINE := 'BEZIER$
      Draw1(Object,CURRENT!.TRANSFORM);
end$

Procedure DOBSPLINE OBJECT$
Begin scalar CURRENT!.LINE$
      CURRENT!.LINE := 'BSPLINE$
      Draw1(Object,CURRENT!.TRANSFORM);
end$

Procedure DOLINE OBJECT$
Begin scalar CURRENT!.LINE$
      CURRENT!.LINE := 'LINE$
      Draw1(Object,CURRENT!.TRANSFORM);
end$


		%*************************************
		%  interpreted function calls        *
		%*************************************


Procedure DOREPEATED(MODEL,REPTFUN)$      %. repeat applying 
Begin scalar  TEMP,I,TRANS,COUNT,TS,TA,GRP$        %. transformations.
      TRANS := PRLISPCDR REPTFUN$                    
      If LENGTH TRANS  = 1 then 
           TRANS := EVAL CAR1 TRANS
        else                                       % "TRANS": transformation
         << TS :=CAR1 TRANS$                      %          matrix.
            TA := PRLISPCDR TRANS $                     % "MODEL": the model.
            TRANS := APPLY(TS,TA) >> $             % "COUNT": the times "MODEL"
      COUNT := CAR1 REPTFUN$                      %          is going to be 
      GRP := LIST('GROUP)$                         %          repeated.
      TEMP := V!.COPY TRANS$       
      FOR I := 1 : COUNT DO        
      << GRP := LIST('TRANSFORM,MODEL,TEMP) . GRP$  
         TEMP := MAT!*MAT(TEMP,TRANS) >>$  
         GRP := REVERSE GRP$
      Return  GRP
end$

		%***********************************
		% Define SHOW ESHOW Draw AND EDraw *
		% ESHOW AND EDraw ERASE THE SCREEN *
		%***********************************


Procedure SHOW X;                         %. ALIAS FOR Draw
<<
  If DEV!. = 'MPS then				%. MPS driver don't call
  <<						%. echo functions for diplay 
						%. device
		DISPLAY!.LIST := LIST (X, DISPLAY!.LIST);
		FOR EACH Z IN DISPLAY!.LIST DO
			If Z neq NIL then 
			  Draw1(Z,GLOBAL!.TRANSFORM); % Draw object list
						       % to frame
		PSnewframe();			       % display frame
  >>
  else
  <<  GraphOn();				% call echo off If not emode
         			                % If neccessary turn low level
      Draw1(X,GLOBAL!.TRANSFORM);	        % Draw model tekronix style

      GraphOff();				% call echoon
  >>;

>>;                                       

Procedure ESHOW ZZ$                       %. erases the screen and
 <<Erase();                                       %. display the picture "ZZ"
   GraphOn();
   DELAY();
   Draw1(ZZ,GLOBAL!.TRANSFORM);	        % Draw model tekronix style
   If DEV!. = 'MPS then <<			   % Mps display frame
		PSnewframe();
		DISPLAY!.LIST := ZZ; >>;
   GraphOff();
   0 >>;

DefineROP('SHOW,10);				   %. set up precedence
DefineROP('ESHOW,10);

Procedure Draw X;                         %. ALIAS FOR SHOW
   SHOW X$

Procedure EDraw ZZ$                       %. erases the screen and
   ESHOW ZZ$


DefineROP('Draw,10);
DefineROP('EDraw,10);


Procedure Col N;                     % User top-level color
 <<GraphOn(); ChangeColor N; GraphOff()>>;


		%*************************************
		% Define Draw FUNCTIONS FOR VARIOUS  *
		% TYPES OF DISPLAYABLE OBJECTS       *
		%*************************************


Procedure DrawModel PICT$                %. given picture "PICT" will 
 Draw1(PICT,CURRENT!.TRANSFORM)$                   %. be applyied with global 

Procedure DERROR(MSG,OBJECT);
  <<PRIN2 " Draw Error `"; PRIN2T MSG;
    PRIN2 OBJECT; ERROR(700,MSG)>>;

Procedure Draw1 (PICT,CURRENT!.TRANSFORM)$   % Draw PICT with TRANSFORMATION 
Begin scalar ITM,ITSARGS$
      If NULL Pict then Return NIL;
      If IDP PICT then PICT:=EVAL PICT; 
      If VECTORP PICT AND SIZE(PICT)=2 then Return DrawPOINT PICT$
      If NOT PAIRP PICT then DERROR("Non Pair in Draw1: ",PICT);
      ITM := CAR1 PICT$
      ITSARGS := PRLISPCDR PICT$
      If NOT (ITM = 'TRANSFORM) then 
         ITSARGS := LIST ITSARGS$                  % gets LIST of args
      ITM := GET (ITM,'PBINTRP)$
      If NULL ITM then DERROR("Unknown Operator in Draw1:",PICT);
      APPLY(ITM,ITSARGS)$
      Return PICT$
end$


Procedure DrawGROUP(GRP)$		% Draw a group object
Begin scalar ITM,ITSARGS,LMNT$
      If PAIRP GRP then 
      FOR EACH LMNT IN GRP DO
        If PAIRP LMNT then Draw1 (LMNT,CURRENT!.TRANSFORM)
        else Draw1 (EVAL LMNT,CURRENT!.TRANSFORM)
       else Draw1 (EVAL GRP,CURRENT!.TRANSFORM)$
      Return GRP$
end$


Procedure DrawPOINTSET (PNTSET)$
Begin scalar ITM,ITSARGS,PT$                    
      FirstPoint!* := 'T$
      If PAIRP PNTSET then 
      << If CURRENT!.LINE = 'BEZIER then
           PNTSET := DrawBEZIER PNTSET
         else If CURRENT!.LINE = 'BSPLINE then
           PNTSET := DrawBSPLINE PNTSET$
         FOR EACH PT IN PNTSET DO
            <<If PAIRP PT then Draw1 (PT,CURRENT!.TRANSFORM)
                 else Draw1 (EVAL PT,CURRENT!.TRANSFORM)$ 
	         FirstPoint!* := 'NIL>> >>
      else Draw1 (EVAL PNTSET,CURRENT!.TRANSFORM)$
      Return PNTSET$
end$

   
Procedure DrawPOINT (PNT)$
Begin scalar CLP,X1,Y1,W1,V,U0,U1,U2;
      If IDP PNT then PNT := EVAL PNT$
      If PAIRP PNT then  PNT := MKPOINT PNT; 
      V:=CURRENT!.TRANSFORM;
      % Transform Only x,y and W

      U0:=PNT[0]; U1:=PNT[1]; U2:=PNT[2]; 

      X1:=U0 * V[0] + U1 * V[1] + U2 * V[2];
      Y1:=U0 * V[3] + U1 * V[4] + U2 * V[5];
      W1:=U0 * V[6] + U1 * V[7] + U2 * V[8];

      IF NOT( (W1=1) or  (W1 = 1.0)) then <<x1:=x1/w1; y1:=y1/w1>>;
      If FirstPoint!* then  Return MoveToXY(X1,Y1);
                  % back to w=1 plane If needed.      
      CLP := CLIP2D(HEREPOINTX,HerePointY, X1,Y1)$   
      If CLP then  <<MoveToXY(CLP[0],CLP[1])$
                     DrawToXY(CLP[2],CLP[3])>>$
end$


Procedure PERFORMTRANSFORM(PCTSTF,TRNSFRM)$
Begin scalar PROC,OLDTRNS,TRNSFMD,TRANSFOP,
             TRANSARG,ITM,ITSARGS$
      If IDP TRNSFRM then
         TRNSFRM := EVAL TRNSFRM$
         If VECTORP TRNSFRM AND SIZE(TRNSFRM) = 8 then    
            Draw1 (PCTSTF,MAT!*MAT(TRNSFRM,CURRENT!.TRANSFORM))  
       else If PAIRP TRNSFRM then 
        <<TRANSFOP := CAR1 TRNSFRM$
          If (TRANSARG := PRLISPCDR TRNSFRM)
             then TRANSARG := LIST (PCTSTF,TRANSARG)
             else TRANSARG := LIST PCTSTF$
             If (TRANSFOP = 'BEZIER OR TRANSFOP = 'BSPLINE) then
             APPLY(GET(TRANSFOP,'PBINTRP),TRANSARG)
             else
              Draw1 (APPLY(GET(TRANSFOP,'PBINTRP),TRANSARG),
                     CURRENT!.TRANSFORM) >>
end$

		%***************************************
		%  circle bezier and bspline functions *
		%***************************************

Procedure DrawCIRCLE(CCNTR,RADIUS);    %. Draw a circle 
Begin scalar APNT,POLY,APNTX, APNTY$   
      POLY := LIST('POINTSET)$
      If IDP CCNTR then CCNTR := EVAL CCNTR$
      RADIUS := CAR1 RADIUS$
      If IDP RADIUS then 
        RADIUS := EVAL RADIUS$ 
      FOR ANGL := 180 STEP -15 UNTIL -180 DO	% each line segment
     << APNTX := CCNTR[0] + RADIUS * COSD ANGL$ % represents an arc of 15 dgrs
	APNTY := CCNTR[1] + RADIUS * SIND ANGL$
        POLY := LIST('Onepoint,APNTX,APNTY) . POLY>>$
     Return REVERSE POLY
end$


Procedure DrawBspline CONPTS$            %. a "closed" Periodic  bspline curve 
  Begin scalar N,CURPTS,                % See CATMUL thesis Appendix
             CPX,CPY,                   % Note correction in Matrix!
             X0,X1,X2,X3,
             Y0,Y1,Y2,Y3,
             T1,T2,T3, 
             J0,J1,J2,
             NPTS;
         
         NPTS := 4;

         N := LENGTH CONPTS$  %/ Check at least 4 ?

         CONPTS := Append (CONPTS,CONPTS)$  % To make a Closed Loop
     % Set the Initial 4 points
         X0:=0; % Dummy
         Y0:=0;
         X1:=GETV(CAR CONPTS,0); % Will Be X0,Y0 in loop
         Y1:=GETV(CAR CONPTS,1);

         CONPTS := CDR CONPTS;
         X2:=GETV(CAR CONPTS,0);
         Y2:=GETV(CAR CONPTS,1);

         CONPTS := CDR CONPTS;
         X3:=GETV(CAR CONPTS,0);
         Y3:=GETV(CAR CONPTS,1);

      WHILE N > 0 DO
      << X0 := X1;  Y0 := Y1;  % Cycle Points
         X1 := X2;  Y1 := Y2;
         X2 := X3;  Y2 := Y3;
         CONPTS := CDR CONPTS;
         X3:=GETV(CAR CONPTS,0);
         Y3:=GETV(CAR CONPTS,1);
   % Compute X(t) and Y(t) for NPTS points on [0.0,1.0]
         FOR I := 0:NPTS-1 DO
         << T1 := FLOAT(I)/NPTS$ % Powers of  t
            T2 := T1 * T1;
            T3 := T2 * T1;
%/             ( -1  3 -3 1
%/                3 -6  3 0 
%/               -3  0  3 0
%/                1  4  1 0 )

            J0:=  (1.0-T3) + 3.0*(T2-T1);
            J1 := 3.0*T3 - 6*T2 +4.0;
            J2 := 1.0+ 3.0*(T1 +T2- T3);

            CPX  := (X0*J0 +X1*J1 + X2 *J2 +X3*T3)/6.0;
            CPY  := (Y0*J0 +Y1*J1 + Y2 *J2 +Y3*T3)/6.0;

            CURPTS := Pnt2(CPX, CPY,1.0) . CURPTS >>$
          N := N - 1>>;

      Return  CURPTS
end$

% Faster 2-d Bezier

procedure DrawBEZIER CNTS;            % Give list of Points
Begin
	scalar LEN, NALL, SAVEX, SAVEY, CPX, CPY,
	       CURPTS, T0, T1, TEMP, FACTL, TI, FI,COEFF;

	LEN := Isub1 LENGTH(CNTS);
        SaveX := MKVect Len;
        SaveY := MKVect Len;       
	FACTL := IFACT LEN;
	FOR I := 0:LEN DO
	 <<Coeff := FactL/(IFACT(i)*IFACT(Len-i));
           SAVEX[I] := GETV(CAR CNTS, 0) * Coeff;
	   SAVEY[I] := GETV(CAR CNTS, 1) * Coeff;;
	   CNTS := CDR CNTS>>;

	NALL := 1.0/(8.0  * LEN);   % Step Size

	FOR T0 := 0.0 STEP NALL UNTIL 1.0 DO 
	<<  T1 := 1.0-T0;
            TI := T0;
            TEMP := T1**LEN;
	    CPX := TEMP * SAVEX[0];
	    CPY := TEMP * SAVEY[0];
	    FOR I := 1:LEN DO
	    <<	TEMP := (TI * (T1**(LEN - I)));
                TI := TI * T0;
		CPX := TEMP * SAVEX[I] + CPX;
		CPY := TEMP * SAVEY[I] + CPY >>;

	    CURPTS := LIST ('ONEPOINT, CPX, CPY) . CURPTS
	>>;
	Return REVERSE CURPTS;
end;

procedure IFACT N;   % fast factorial
 Begin scalar M;
    M:=1;
    While Igreaterp(N,1) do <<M:=Itimes2(N,M); N :=Isub1 N>>;
    Return M;
 end;

LoadTime SetUpVariables();

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

