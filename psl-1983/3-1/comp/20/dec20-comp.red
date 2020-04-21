%
% 20-COMP.RED - Compiler patterns for Dec-20 PSL, plus a few cmacro expanders
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        11 January 1982
% Copyright (c) 1982 University of Utah
%

%  21-May-83 Mark R. Swanson
%    Changed *JumpOn to generate Instruction Format Indirect Words for
%    "case" addresses.
%  <PSL.COMP-20>DEC20-COMP.RED.4,  2-Mar-83 18:07:16, Edit by PERDUE
%  Added a USESDEST case to the pattern for SUBPAT
%  <PSL.20-COMP>20-COMP.RED.1, 25-Feb-82 16:34:42, Edit by BENSON
%  Converted from VAX version


PUT('TVPAT,'PATTERN,'(
    !&REGMEM ('!*DESTROY DEST)
    ((DEST ANY) (MAC L1 A1 A2) ('!*LOAD DEST '(QUOTE NIL))
		('!*JUMP L2) ('!*LBL L1) ('!*LOAD DEST '(QUOTE T)) ('!*LBL L2))
    ((ANY DEST) (MAC L1 A1 A2) ('!*LOAD DEST '(QUOTE NIL))
		('!*JUMP L2) ('!*LBL L1) ('!*LOAD DEST '(QUOTE T)) ('!*LBL L2))
    ((USESDEST ANY) (MAC L1 A1 A2) ('!*LOAD DEST '(QUOTE NIL))
		('!*JUMP L2) ('!*LBL L1) ('!*LOAD DEST '(QUOTE T)) ('!*LBL L2))
    ((ANY USESDEST) (MAC L1 A1 A2) ('!*LOAD DEST '(QUOTE NIL))
		('!*JUMP L2) ('!*LBL L1) ('!*LOAD DEST '(QUOTE T)) ('!*LBL L2))
    (ANY ('!*LOAD DEST '(QUOTE T)) (MAC L1 A1 A2)
 	 ('!*LOAD DEST '(QUOTE NIL)) ('!*LBL L1))));


PUT('TVPAT1,'PATTERN,'(
    !&REGMEM ('!*DESTROY DEST)
    ((DEST) (MAC L1 A1 P2) ('!*LOAD DEST '(QUOTE NIL))
		('!*JUMP L2) ('!*LBL L1) ('!*LOAD DEST '(QUOTE T)) ('!*LBL L2))
    ((USESDEST) (MAC L1 A1 P2) ('!*LOAD DEST '(QUOTE NIL))
		('!*JUMP L2) ('!*LBL L1) ('!*LOAD DEST '(QUOTE T)) ('!*LBL L2))
    (ANY ('!*LOAD DEST '(QUOTE T)) (MAC L1 A1 P2)
 	 ('!*LOAD DEST '(QUOTE NIL)) ('!*LBL L1))));


PUT('TSTPAT,'PATTERN,'(
    NIL
    !&FIXREGTEST
    ((REGN ANY) (MAC DEST A1 A2))
    (ANY (MAC DEST A2 A1))));

PUT('TSTPATC,'PATTERN,'(
     NIL
    !&SETREGS1
     ((REGN ANY) (MAC DEST A1 A2))
     (ANY (P2 DEST A2 A1))));

PUT('TSTPAT2, 'PATTERN, '(
     NIL !&SETREGS1
     (ANY (MAC DEST A1 P2))));

PUT('SETQPAT,'PATTERN,'(
 NIL NIL
 ((NOVAL ANY NOTANYREG) ('!*STORE A2 A1))
 ((NOVAL DEST ANY) ('!*STORE A2 DEST))
 ((NOVAL USESDEST ANY) ('!*LOAD T1 A2) ('!*STORE T1 A1))
 ((NOVAL ANY ANY) ('!*LOAD DEST A2) ('!*STORE DEST A1))
 ((ANY DEST) ('!*STORE DEST A1))
 ((DEST ANY) ('!*STORE A2 DEST))
 ((USESDEST ANY) ('!*STORE A2 A1) ('!*STORE A2 DEST))
 (ANY ('!*LOAD DEST A2) ('!*STORE DEST A1))));

PUT('RPLACPAT,'PATTERN,'(
   NIL NIL
   ((NOVAL ANY ANY) ('!*STORE A2 (MAC A1)))
   ((DEST ANY) ('!*STORE A2 (MAC A1)))
   ((USESDEST ANY) ('!*STORE A2 (MAC A1)) ('!*LOAD DEST A1))
   ((ANY DEST) ('!*STORE A2 (MAC A1)) ('!*LOAD DEST A1))
   ((ANY USESDEST) ('!*STORE A2 (MAC A1)) ('!*LOAD DEST A1))
   (ANY ('!*LOAD DEST A1) ('!*STORE A2 (MAC DEST)))));

PUT('ASSOCPAT,'PATTERN,'(
   NIL ('!*SET DEST (FN A1 A2))
  ((DEST ANY) (MAC A1 A2))
  ((ANY DEST) (MAC A2 A1))
  ((USESDEST USESDEST) ('!*LOAD T1 A1) ('!*LOAD DEST A2) (MAC DEST T1))
  ((ANY USESDEST) ('!*LOAD DEST A2) (MAC DEST A1))
  (ANY ('!*LOAD DEST A1) (MAC DEST A2))));

PUT('SUBPAT,'PATTERN,'(
  NIL ('!*SET DEST (FN A1 A2))
   ((DEST ANY) (MAC A1 A2))
   ((ANY DEST) ('!*WMINUS DEST DEST) ('!*WPLUS2 A2 A1))
   ((ANY USESDEST) ('!*LOAD T1 A2) ('!*LOAD DEST A1) (MAC DEST T1))
   (ANY ('!*LOAD DEST A1) (MAC DEST A2))));

PUT('NONASSOCPAT,'PATTERN,'(
   NIL ('!*SET DEST (FN A1 A2))
   ((DEST ANY) (MAC A1 A2))
   ((ANY USESDEST) ('!*LOAD T1 A2) ('!*LOAD DEST A1) (MAC DEST T1))
   (ANY ('!*LOAD DEST A1) (MAC DEST A2))));

PUT('FIELDPAT,'PATTERN,'(
   NIL ('!*SET DEST (FN A1 A2 A3))
   (ANY (MAC DEST A1 A2 A3))));

PUT('PUTFIELDPAT,'PATTERN,'(
   NIL NIL
   ((NOVAL ANY ANY ANY ANY) (MAC A1 A2 A3 A4))
   (ANY (MAC A1 A2 A3 A4) ('!*STORE A1 DEST))));

PUT('UNARYPAT,'PATTERN,'(
   !&NOANYREG ('!*SET DEST (FN A1))
   (ANY (MAC DEST A1))));

PUT('MODMEMPAT,'PATTERN,'(
   NIL NIL
   (ANY (MAC A1 A2))));

PUT('MODMEMPAT1,'PATTERN,'(
  NIL NIL
   (ANY (MAC A1 A1))));

% Potential trouble spot!!!!!!! (for extend addressing)

lisp procedure !*LamBind(Regs, FLst);
begin scalar X, Y;
    FLst := reverse cdr FLst;
    Regs := reverse cdr Regs;
    while FLst do
    <<  if null Regs then
	    X := 0
	else
	<<  X := cadr car Regs;
	    Regs := cdr Regs >>;
	Y := list('halfword, X, list('IDLoc, cadar FLst)) . Y;
	FLst := cdr FLst >>;
    return '(jsp (reg t5) (Entry FastBind)) . Y;
end;

DefCMacro !*Lambind;

lisp procedure !*JumpOn(Register, LowerBound, UpperBound, LabelList);
begin scalar ExitLbl, BaseLbl, Result;
    ExitLbl := GenSym();
    BaseLbl := GenSym();
    Result := NIL . NIL;
    TConc(Result,if LowerBound < 0 then
		     list('caml, Register, list('lit, LowerBound))
		 else
		     list('cail, Register, LowerBound));
    TConc(Result,if UpperBound < 0 then
		     list('camle, Register, list('lit, UpperBound))
		 else
		     list('caile, Register, UpperBound));
    TConc(Result,list('jrst, ExitLbl));
    TConc(Result,
	list('jrst,
	     list('Indirect,
		  list('Indexed,
		       Register,
		       list('difference, BaseLbl, LowerBound)))));
    TConc(Result, BaseLbl);
    for each X in LabelList do
	TConc(Result, list('indword, cadr X));
    TConc(Result, ExitLbl);
    return car Result;
end;

DefCMacro !*JumpOn;

END;
