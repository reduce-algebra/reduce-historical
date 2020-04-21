% INSPECT.RED - Scan files for defined functions
% 
% Author:      Martin Griss
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        31 May 1982
% Copyright (c) 1982 University of Utah
%
% adapted from CREF and BUILD

Imports '(Gsort Dir!-Stuff);

FLUID '(!*UserMode            % To control USER Redef message
        !*ECHO
        !*RedefMsg            % To suppress REDEF messages
         CurrentFile!*        % To keep tack of this file
         FileList!*           % Files seen so far
         ProcedureList!*      % procedures seen so far
         ProcFileList!*       % (PROC . FILE) so far
         !*PrintInspect       % Print each proc
         !*QuietInspect       % Suppress INSPECTOUT messages
);

!*PrintInspect:=T;
!*QuietInspect:=NIL;

Procedure Inspect X;
begin scalar !*UserMode,!*Redefmsg,!*QuietInspect;
    !*QuietInspect:=T;
    INSPECTOut();
    !*ECHO:=NIL;
    If Not FunboundP 'Begin1 then EvIn list X
     else EVAL LIST('Dskin, x);
    INSPECTEnd();
end;

Procedure InspectOut; % Scan Files for Definitions
 Begin
    !*DEFN:=T; !*ECHO:=NIL; SEMIC!*:= '!$ ;
    DFPRINT!* := 'InspectPrint;
    ProcedureList!*:=FileList!* :=ProcFileList!*:=NIL;
    CurrentFile!* := NIL;
    if not !*QuietInspect then
    <<  if not FUnBoundP 'Begin1 then
	<<  Prin2T "INSPECTOUT: IN files; or type in expressions";
	    Prin2T "When all done execute INSPECTEND;" >>
	else
	<<  Prin2T "INSPECTOUT: (DSKIN files) or type in expressions";
	    Prin2T "When all done execute (INSPECTEND)" >> >>;
 End;

Procedure InspectEnd;
 Begin
    If !*PrintInspect then PrintF "%n%% --- Done with INSPECTION ---%n";
    Dfprint!*:=NIL;
    !*Defn:=NIL;
    ProcedureList!* := IdSort ProcedureList!*;
    If !*PrintInspect then <<Prin2T "% --- PROCS: --- "; 
                             Print ProcedureList!*>>;
 End;

Procedure InspectPrint U;
 BEGIN scalar x;
   !*ECHO:=NIL;
   SEMIC!*:='!$;
   x:=IF PairP CLOC!* THEN CAR CLOC!* ELSE "*TTYInput*";
   If x NEQ CurrentFile!* and !*PrintInspect then
     PrintF("%n%% --- Inspecting File : %r --- %n",x);
   CurrentFile!* := x;
   % Find current FILE name, see if new
  IF Not MEMBER(CurrentFile!*,FileList!*) THEN
   FileList!*:=CurrentFile!* . FileList!*;
  InspectForm U;
 END;

FLAG('(INSPECTEND),'IGNORE);
PUT('InspectEnd,'RlispPrefix,'(NIL LAMBDA(X) (ESTAT 'Inspectend)));

procedure InspectForm U;		%. Called by TOP-loop, DFPRINT!*
begin scalar Nam, Ty, Fn;
	if not PairP  U then return NIL;
	Fn := car U;
	IF FN = 'PUTD THEN GOTO DB2;
	IF NOT (FN MEMQ '(DE DF DM DN)) THEN GOTO DB1;
	NAM:=CADR U;
	U:='LAMBDA . CDDR U;
	TY:=CDR ASSOC(FN, '((DE . EXPR)
			    (DF . FEXPR)
			    (DM . MACRO)
			    (DN . NEXPR)));
DB3:	if Ty = 'MACRO then 
         begin scalar !*Comp;
          PutD(Nam, Ty, U);		% Macros get defined now
    	 end;
	if FlagP(Nam, 'Lose) then <<
	ErrorPrintF("*** %r has not been defined, because it is flagged LOSE",
			Nam);
	return NIL >>;
        InspectProc(Nam,Ty);
	RETURN NIL;
DB1:	% Simple S-EXPRESSION look for LAP etc.
        IF EQCAR(U,'LAP) Then Return InspectLap U;
        IF EQCAR(U,'Imports) 
	  then Return PrintF("%% --- Imports: %w in %w%n",Cadr U, CurrentFile!*);
	% Maybe indicate IMPORTS etc.
        RETURN NIL;
DB2:	% analyse PUTD
	NAM:=CADR U;
	TY:=CADDR U;
	FN:=CADDDR U;
	IF EQCAR(NAM,'QUOTE) THEN <<  NAM:=CADR NAM;
	IF EQCAR(TY,'QUOTE) THEN << TY:=CADR TY;
	IF PAIRP FN AND CAR FN MEMBER '(FUNCTION QUOTE) THEN <<  FN:=CADR FN;
	IF TY MEMQ '(EXPR FEXPR MACRO NEXPR) THEN
	<<  U:=FN; GOTO DB3 >> >> >> >>;
	GOTO DB1;
   END;

Procedure InspectProc(Nam,Ty);
<<If !*PrintInspect then <<Prin1 NAM; Prin2 " ">>;
  ProcedureList!*:=NAM . ProcedureList!*;
  ProcFileList!*:=(NAM . CurrentFile!*) . ProcFileList!*>>;

Procedure InspectLap U;
  For each x in U do if EQcar(x,'!*ENTRY) then InspectProc(Cadr U,Caddr U);

% -- Handle LISTs of files and dirs ---

Fluid '(!*PrintInspect !*QuietInspect);

Nexpr procedure GetFileList L;
 GetFiles1 L;

Procedure GetFiles1 L;
 If null L then Nil
  else append(Vector2List GetCleandir Car L, GetFiles1 Cdr L);

procedure InspectToFile F;
 Begin scalar f1,c;
     f1:=Bldmsg("%s-%s.ins",GetFileName(f),GetExtension(f));
     Printf(" Inspecting %r to %r%n",F,F1);
     c:=open(f1,'output);
     WRS c;
     !*PrintInspect:=NIL;
     Inspect F$
     Prin2 "(ProcList '"$
     Print ProcedureList!*;
     Prin2T ")";
     WRS NIL;
     close c;
 End;

procedure InspectAllFiles Files;
For each x in files do
 <<PrintF("Doing file: %w%n",x);
   InspectToFile x>>;

Procedure InspectAllPU();
 InspectAllFiles getFileList("pu:*.red","PU:*.sl");


END;
