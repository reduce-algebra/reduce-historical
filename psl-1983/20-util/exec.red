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

%  <PSL.UTIL>EXEC.RED.5, 24-May-82 13:01:50, Edit by BENSON
%  Changed <EDITORS> and <SUBSYS> to SYS: in filenames
%/ Changed FILNAM->FileName, due to GLOBAL conflict
%/ Changed JSYS calls, so LIST(..) rather than '(..) used
%/ Changed for V3:JSYS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Simple JSYS interfaces

imports '(JSYS);

GLOBAL '(ForkNAMES!* EXECFork EMacsFork MMFork);

Lisp procedure GetOLDJfn FileName; %. test If file OLD and return Jfn
   Begin scalar Jfn; 
      If NULL StringP FileName then return NIL; 
      Jfn := JSYS1(Bits(2,3,17),FileName,0,0,jsGTJfn); 
	 % OLD!MSG!SHORT
      If Jfn<0 then return NIL; 
      return Jfn
   END;

Lisp procedure GetNEWJfn FileName; 	 %. test If file NEW and return Jfn
   Begin scalar Jfn; 
      If NULL StringP FileName then return NIL; 
      Jfn := JSYS1(Bits(0,1,3,17),FileName,0,0,jsGTJfn); 
	% GEN!NEW!MSG!SHORT
      If Jfn<0 then return NIL; 
      return Jfn
   END;

Lisp procedure RELJfn Jfn;	 %. return Jfn to system
 JSYS0(Jfn,0,0,0,jsRLJfn);

Lisp procedure OPENOLDJfn Jfn;	 %. OPEN to READ
 JSYS0(Jfn,Bits( (7 . 5),19),0,0,jsOPENF);

Lisp procedure OPENNEWJfn Jfn;	 %. Open to WRITE
 JSYS0(Jfn,Bits( (7 . 5),20),0,0,jsOPENF);

Lisp procedure GetFork Jfn; 	 %. Create Fork, READ File on Jfn
   Begin scalar FH; 
      FH := JSYS1(Bits(1),0,0,0,jsCFork); 
      JSYS0(Xword(FH ,Jfn),0,0,0,jsGet); 
      return FH
   END;

Lisp procedure STARTFork FH;	 %. Start (Restart) a Fork
  JSYS0(FH, 0,0,0,jsSFRKV);

Lisp procedure WAITFork FH;	 %. Wait for completion
 JSYS0(FH,0,0,0,jsWFork);

Lisp procedure RUNFork FH;	 %. Normal use, to run a Fork
 <<STARTFork FH; WAITFork FH>>;

Lisp procedure KILLFork FH;	 %. Kill a Fork
   JSYS0(FH,0,0,0,jsKFork);

Lisp procedure SETPRIMARYJfnS(FH,INJfn,OUTJfn);
   JSYS0(FH,Xword(INJfn , OUTJfn),0,0,JSSPJfn);  %. Change PRIMARY Jfns (BAD?)

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
   JSYS0(s,JSYS1(0,0,0,0,JSGJINF),0,0,JSDIRST);
   Return RecopyStringToNULL S
 End;

Lisp procedure GetCDIR;	 %. Connected DIRECTORY
  Begin scalar s;
   S:=Mkstring 80;
   JSYS0(S,JSYS2(0,0,0,0,jsGJINF),0,0,jsDIRST);
   return RecopyStringToNULL S
 end;

Lisp procedure PSOUT S;	 %. Print String
 JSYS0(S,0,0,0,jsPSOUT);

Lisp procedure GTJfn L;	 %. Get a Jfn
 JSYS1(L,0,0,0,jsGTJFN);

Lisp procedure NAMEFROMJfn J;	 %. name of File on a Jfn
  Begin scalar S;
       s:=Mkstring 100;
       JSYS0(S,J,0,0,JSJfnS);
  return RecopyStringToNULL S;
 end;

Fexpr Procedure InFile(U);   %. INPUT FILE, (prompt for name too?)
 If StringP U then DskIn EVAL CAR U
  else
    Begin scalar Jfn,Fname;
      PSOUT "Input file:";
	Jfn:=Jsys1(BITS(2,3,4,16,17),Xword(8#100,8#101),0,0,jsGTJFN);
	Fname:= NAMEFROMJFN JFN;
	RELJFN JFN;
        PRINTF("reading file %r %n", FNAME);
        DSKIN Fname;
    end;

%-- Command string processor and take

Lisp procedure  PutRescan(S);	%. Enter String
 <<JSYS0(S,0,0,0,jsRSCAN);
   JSYS0(0,0,0,0,jsRSCAN)>>;

On SYSLISP;

syslsp procedure  GetRescan();	%. Return as String
 Begin scalar N,S;
   XJSYS1(0,0,0,0,jsRSCAN);      % Announce to Get
   N:=XJSYS1(1,0,0,0,jsRSCAN); % How Many
   IF N=0 then return 'Nil;
   S:=GtStr N-1;   % To Drop Trailing EOL
   For I:=0:N-2 do
	StrByt(S,I):=XJsys1(0,0,0,0,JsPBIN);
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
