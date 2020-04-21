%
% EXEC.RED -   Simple TOPS20 Interfaces, "EXEC Fork", etc
% 
% Author:      Martin L. Griss and Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        8 March 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.UTIL.20>EXEC.RED.6, 25-Mar-83 14:32:06, Edit by BARBOUR
%  Updated clocktimedate  to return the string with nulls stripped off
% Edit by Cris Perdue, 23 Mar 1983 1453-PST
% Changed from clocktime to ClockTimeDate
% Edit by Cris Perdue, 21 Mar 1983 1003-PST
% Added Kessler's clocktime and getloadaverage from CLOCKTIME.RED
%  <PERDUE>EXEC.RED.2, 21-Mar-83 11:02:46, Edit by PERDUE
%  Put JSYS names in const(<name>) form to match current JSYS module
%  <PSL.UTIL>EXEC.RED.5, 24-May-82 13:01:50, Edit by BENSON
%  Changed <EDITORS> and <SUBSYS> to SYS: in filenames
%/ Changed FILNAM->FileName, due to GLOBAL conflict
%/ Changed JSYS calls, so LIST(..) rather than '(..) used
%/ Changed for V3:JSYS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Simple JSYS interfaces

CompileTime load(Syslisp, Jsys, Monsym);
imports '(JSYS);

GLOBAL '(ForkNAMES!* EXECFork EMacsFork MMFork);

Lisp procedure GetOLDJfn FileName; %. test If file OLD and return Jfn
   Begin scalar Jfn; 
      If NULL StringP FileName then return NIL; 
      Jfn := JSYS1(Bits(2,3,17),FileName,0,0,const(jsGTJfn)); 
	 % OLD!MSG!SHORT
      If Jfn<0 then return NIL; 
      return Jfn
   END;

Lisp procedure GetNEWJfn FileName; 	 %. test If file NEW and return Jfn
   Begin scalar Jfn; 
      If NULL StringP FileName then return NIL; 
      Jfn := JSYS1(Bits(0,1,3,17),FileName,0,0,const(jsGTJfn)); 
	% GEN!NEW!MSG!SHORT
      If Jfn<0 then return NIL; 
      return Jfn
   END;

Lisp procedure RELJfn Jfn;	 %. return Jfn to system
 JSYS0(Jfn,0,0,0,const(jsRLJfn));

Lisp procedure OPENOLDJfn Jfn;	 %. OPEN to READ
 JSYS0(Jfn,Bits( (7 . 5),19),0,0,const(jsOPENF));

Lisp procedure OPENNEWJfn Jfn;	 %. Open to WRITE
 JSYS0(Jfn,Bits( (7 . 5),20),0,0,const(jsOPENF));

Lisp procedure GetFork Jfn; 	 %. Create Fork, READ File on Jfn
   Begin scalar FH; 
      FH := JSYS1(Bits(1),0,0,0,const(jsCFork)); 
      JSYS0(Xword(FH ,Jfn),0,0,0,const(jsGet)); 
      return FH
   END;

Lisp procedure STARTFork FH;	 %. Start (Restart) a Fork
  JSYS0(FH, 0,0,0,const(jsSFRKV));

Lisp procedure WAITFork FH;	 %. Wait for completion
 JSYS0(FH,0,0,0,const(jsWFork));

Lisp procedure RUNFork FH;	 %. Normal use, to run a Fork
 <<STARTFork FH; WAITFork FH>>;

Lisp procedure KILLFork FH;	 %. Kill a Fork
   JSYS0(FH,0,0,0,const(jsKFork));

Lisp procedure SETPRIMARYJfnS(FH,INJfn,OUTJfn);
   JSYS0(FH,Xword(INJfn , OUTJfn),0,0,const(JSSPJfn));  %. Change PRIMARY Jfns (BAD?)

Lisp procedure OPENFork FileName; 	 %. Get a File into a Fork
   Begin scalar FH,Jfn; 
      If NULL FileP FileName then StdError CONCAT("Cant find File ",FileName); 
      Jfn := GetOLDJfn FileName; 
      FH := GetFork Jfn; 
      return FH
   END;

Lisp procedure RUN FileName;	 %. Run A File
   Begin scalar FH; FH := OPENFork FileName; RUNFork FH; KILLFork FH END;

Lisp Procedure ForkP FH;         %. test if Valid Fork Handle
  FixP FH and not Zerop FH; %/Kludge

Lisp procedure EXEC; 
  <<If Not ForkP EXECFork then EXECFork := OPENFork "SYSTEM:EXEC.EXE"; 
    RUNFork EXECFork>>;

Lisp procedure EMACS; 
  <<If Not ForkP EMacsFork then EMACSFork := OPENFork "SYS:EMACS.EXE"; 
    RUNFork EMACSFork>>;

Lisp procedure MM; 
  <<If Not ForkP MMFork then  MMFork := OPENFork "SYS:MM.EXE";
    RUNFork MMFork>>;

Lisp procedure GetUNAME; 	 %. USER name
 Begin Scalar S;
   S:=Mkstring 80;
   JSYS0(s,JSYS1(0,0,0,0,const(JSGJINF)),0,0,const(JSDIRST));
   Return RecopyStringToNULL S
 End;

Lisp procedure GetCDIR;	 %. Connected DIRECTORY
  Begin scalar s;
   S:=Mkstring 80;
   JSYS0(S,JSYS2(0,0,0,0,const(jsGJINF)),0,0,const(jsDIRST));
   return RecopyStringToNULL S
 end;

%   Determine the current time or date or both and  stripped off trailing 
% nulls, with ONE blank Char concatenated on the end of the returned string.
%
%                  RETURNS STRING FORMS ARE SHOWN BELOW:
%    1     -> Returns Date & Time          ..  Day Date First & 24 hr format
%    2     -> Returns Date & Time          ..  Day Date First & 12 hr format
%    3     -> Returns Date & Time          ..  Month first & 24 hr format
%    4     -> Returns Date & Time          ..  Month first & 12 hr format
%    5     -> Returns Weekday,Date, & Time ..  Month first & 24 hr format
%    6     -> Returns Weekday,Date, & Time ..  Month first & 12 hr format
%    7     -> Returns Weekday,Date, & Time ..  Month first & 12 hr format
%                                              day-3 letters and no seconds
%    8     -> Returns time only     ...  hh:mm:ss  12 hr format
%Otherwise -> Returns time only     ...  hh:mm:ss  24 hr format
%
%
 PROCEDURE ClockTimeDate (Time_Selector);       % old ClockTime 
  BEGIN SCALAR Ret_String ;
   Ret_String := MKSTRING 30;
   CASE Time_Selector OF
     1:       <<  JSYS1( Ret_String,-1,bits(2),0,const jsODTIM) ;
                  Ret_String := SUB(Ret_String, 0, 17 )                    >>;
     2:       <<  JSYS1(Ret_String, -1,bits(2,11),0, const jsODTIM) ;
                  Ret_String := SUB(Ret_String, 0, 19 )                    >> ;
     3:       <<  JSYS1(Ret_String, -1,bits(6),0, const jsODTIM) ; 
                  Ret_String := SUB(Ret_String, 0, 17 )                    >> ;
     4:       <<  JSYS1(Ret_String, -1,bits(6,11),0, const jsODTIM) ; 
                  Ret_String := SUB(Ret_String, 0, 19 )                    >> ;
     5:       <<  JSYS1(Ret_String, -1,bits(1,2,6),0, const jsODTIM) ; 
                  Ret_String := SUB(Ret_String, 0, 27 )                    >> ;
     6:       <<  JSYS1(Ret_String, -1,bits(1,2,6,11),0, const jsODTIM) ;
                  Ret_String := SUB(Ret_String, 0, 29 )                    >> ;
     7:       <<  JSYS1(Ret_String, -1,bits(1,6,10,11),0, const jsODTIM) ;
                  Ret_String := SUB(Ret_String, 0, 20 )                    >> ;
     8:       <<  JSYS1(Ret_String, -1,bits(0,11),0, const jsODTIM) ;
                  Ret_String := SUB(Ret_String, 0, 9 )                     >> ;
  Otherwise:  <<  JSYS1(Ret_String, -1,bits(0),0, const jsODTIM) ;
                  Ret_String := SUB(Ret_String, 0, 7 )                     >> ;
    END ; %end for case
    Ret_String := ConCat( Ret_String, " ") ;
    RETURN Ret_String ;
 END;

% Determine the current 1 minute load average and return as a string.
procedure GetLoadAverage;
begin scalar s;
 s:=mkstring 6;
 jsys1(s,Jsys1(8#000014000014, 0, 0, 0, const jsGETAB),8#024037020200,
       0, const jsFLOUT);
 return s
end;

Lisp procedure PSOUT S;	 %. Print String
 JSYS0(S,0,0,0,const(jsPSOUT));

Lisp procedure GTJfn L;	 %. Get a Jfn
 JSYS1(L,0,0,0,const(jsGTJFN));

Lisp procedure NAMEFROMJfn J;	 %. name of File on a Jfn
  Begin scalar S;
       s:=Mkstring 100;
       JSYS0(S,J,0,0,const(JSJfnS));
  return RecopyStringToNULL S;
 end;

Fexpr Procedure InFile(U);   %. INPUT FILE, (prompt for name too?)
 If StringP U then DskIn EVAL CAR U
  else
    Begin scalar Jfn,Fname;
      PSOUT "Input file:";
	Jfn:=Jsys1(BITS(2,3,4,16,17),Xword(8#100,8#101),0,0,const(jsGTJFN));
	Fname:= NAMEFROMJFN JFN;
	RELJFN JFN;
        PRINTF("reading file %r %n", FNAME);
        DSKIN Fname;
    end;

%-- Command string processor and take

Lisp procedure  PutRescan(S);	%. Enter String
 <<JSYS0(S,0,0,0,const(jsRSCAN));
   JSYS0(0,0,0,0,const(jsRSCAN))>>;

On SYSLISP;

syslsp procedure  GetRescan();	%. Return as String
 Begin scalar N,S;
   XJSYS1(0,0,0,0,const(jsRSCAN));      % Announce to Get
   N:=XJSYS1(1,0,0,0,const(jsRSCAN)); % How Many
   IF N=0 then return 'Nil;
   S:=GtStr N-1;   % To Drop Trailing EOL
   For I:=0:N-2 do
	StrByt(S,I):=XJsys1(0,0,0,0,const(JsPBIN));
   Return MkSTR S; % Will include Program name
 end;


OFF SYSLISP;

Global '(CRLF BL);

CRLF :=STRING(8#15,8#12);	%. CR-LF
BL :=STRING(8#40);		%. Blank

Lisp procedure  CONCATS (L);			%. Combine list of strings
 If PAIRP L then CONCAT(CAR L,CONCATS CDR L)
   else CRLF;

Lisp Fexpr Procedure CMDS (!%L);            %. user COMMAND submit
  DOCMDS EVLIS !%L;

Lisp procedure  DOCMDS (L);                  %. Submit via PutRescan
 <<PutRescan CONCATS L;		% Add CR, plant in RSCAN
   EXEC()>>;			% Run 'em

%. -------- Sample Commands

Lisp procedure  VDIR (L);
 DOCMDS LIST("VDIR ",L,CRLF,"POP");

Lisp procedure HelpDir();
 DOCMDS  LIST("DIR PH:*.HLP",CRLF,"POP");

Lisp procedure Take (FileName);
  If FileP FileName then DOCMDS LIST("Take ",FileName,CRLF,"POP");

Lisp procedure  SYS (L);
  DOCMDS LIST("SYS ", L, CRLF, "POP");

Lisp procedure  TALK (L);
  DOCMDS LIST("TALK ",L,CRLF);

Lisp procedure  TYPE (L);
  DOCMDS LIST("TYPE ",L,CRLF,"POP");

END;
