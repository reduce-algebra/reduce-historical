@section(Examples of various kinds) 

Recall that when compiling code, variables which are used extended in
one procedure, and bound as LAMBDA or PROG variables in another, must
be declared fluids.

Example: 
@begin(verbatim)
(de foo(X) (PLUS2 X 1)), compiles to:

         (!*entry foo expr 1)
         (!*alloc 0)
         (!*move (quote 1) (reg 2))
         (!*linke 0 plus2 expr 2)

(de fee(X Y) (Fum (foo X) (foo Y)), compiles to:

         (!*entry fee expr 2)
         (!*alloc 2)
         (!*move (reg 2) (frame 2))
         (!*link foo expr 1)
         (!*move (reg 1) (frame 1))
         (!*move (frame 2) (reg 1))
         (!*link foo expr 1)
         (!*move (reg 1) (reg 2))
         (!*move (frame 1) (reg 1))
	 (!*linke 2 fum expr 2)

Finally, (de fac (N) (cond ((Lessp N 1) 1)
                     (T (Times2 N (fac SUB 1 N))
compiles to:

         (!*entry fac expr 1)
         (!*alloc 1)
         (!*move (reg 1) (frame 1))
         (!*move (quote 1) (reg 2))
         (!*link LessP expr 1)
         (!*jumpeq (label L) (quote nil) (reg 1))
         (!*move (quote 1) (reg 1))
	 (!*exit 1)
         (!*lbl (label L))
         (!*move (frame 1) (reg 1))
         (!*link sub1 expr 1)
         (!*link fac expr 1)
         (!*move (reg 1) (reg 2))
         (!*move (frame 1) (reg 1))
         (!*linke 1 times2 expr 2)
@end(verbatim)

@section(BUILDING the CROSS Compiler)

The executable @dq[xxxx-CROSS.EXE] is built as follows:
@begin(verbatim)

@@psl:rlisp          ! an RLISP
*mapobl function lambda X;
*<<  RemProp(X, 'OpenCode);
*    RemProp(X, 'ExitOpenCode) >>;  % Remove old compiler opts
*                                   % Load common modules
*load(zboot, pass!-one!-lap, if!-system, syslisp, lap!-to!-asm);
*                                   % Load XXXX specific modules
*load(XXXX!-comp, XXXX!-cmac, XXXX!-asm);
*off UserMode;
*DumpFileName!* := "filename.exe";      % Establish the executable name
*Date!*:=Concat("XXXX Cross Assmbler ", Date()); % Establish greeting
*DumpLisp();                            % Does a Reclaim and save
*Quit;
@end(verbatim)


@subsection(An example of the process)
The following is a complete example, from @syslisp to @CMACRO@xs:
@begin(verbatim,leftmargin 0)
@@PSL:RLISP
PSL 3.0 Rlisp,  9-May-82

syslsp procedure Test1();      % Input RLISP syntax code
 begin scalar x;
  x  := 5;
  x  := x+7;
  L  := '(A B C D);
  L1 := (CAR L) . CAR(CDR L);
  print L1;
end;
@End(verbatim)

@begin(verbatim,leftmargin 0)
% This is the output from the Compiler/LAP system.  
% The lines beginning with "(!* ... " are the Abstract 
% machine CMACRO's output from the compiler.

% The indented lines following them are the VAX @sq[LAP]
% assembly code the CMACRO patterns 
% (in the *-CMAC.SL files) produced by the expansion process.

(!*PUSH '5)
   (@op{PUSHL} 5)
(!*WPLUS2 (FRAME 1) (WCONST 7))       % WPLUS2 is actually a 
                                      %  CMACRO (OpenFunct)
   (@op{ADDL2} 7 (DEFERRED (REG ST)))      % Note how the FRAME AnyReg 
          			      % is converted directly to 
                                      % a machine specific 
				      % addressing mode.
(!*MOVE '(A B C D) (!$FLUID L))
    (@op{MOVL} '(A B C D) (!$FLUID L))
(!*MOVE (CAR (CDR (!$FLUID L))) (REG 2))  
	        		      % The AnyReg patterns 
    (@op{EXTZV} 0 27 (!$FLUID L) (REG 2))  % for CAR and CDR are used
    (@op{EXTZV} 0 27 (DISPLACEMENT (REG 2) 4) (REG 2))
    (@op{MOVL} (DEFERRED (REG 2)) (REG 2))
(!*MOVE (CAR (!$FLUID L)) (REG 1))
    (@op{EXTZV} 0 27 (!$FLUID L) (REG 1))
    (@op{MOVL} (DEFERRED (REG 1)) (REG 1))
(!*LINK CONS EXPR 2)                  % Standard Function Cell
                                      %   call.
     (@op{JSB} (ENTRY CONS))                 
(!*MOVE (REG 1) (!$FLUID L1))
     (@op{MOVL} (REG 1) (!$FLUID L1))
(!*LINK PRINT EXPR 1)
     (@op{JSB} (ENTRY PRINT))
(!*MOVE 'NIL (REG 1))
     (@op{MOVL} (REG NIL) (REG 1))         % Reg NIL evaluates to an 
(!*EXIT 1)                            % immediate constant.
     (@op{ADDL2} 4 (REG ST))
     (@op{RSB})
TEST1
@end(verbatim)

@subsection(Prologues and Epilogues)
        An example of Prologues and Epilogues for (@APOLLO  version of) the
@68000 is given below:

@begin(ProgramExample,leftmargin 0)
lisp procedure CodeFileHeader();        % Pure Code Segment
If !*MAIN then
<<CodePrintF("   program %w,m0001%n",ModName!*); 
  CodePrintF "	 data%n";
  DataProcState!*:='data;
  CodePrintF "* Start of execution of the program%n";

  CodeDeclareExternal 'SYMVAL;       %/ Issue EXTERN.D early
  CodeDeclareExternal 'SYMFNC;       %/ Issue EXTERN.D early

  CodePrintF "m0001 EQ *%n";
  CodePrintF "   move.l  db,-(sp)      Save caller db%n";
  CodePrintF "   clr.l      -(sp)      Push reserved word%n";
  CodePrintF "   move.l  a0,-(sp)      Push address of ECB%n";
  CodePrintF "   move.l SYMVAL+512,d0  Init NIL Reg%n";
  CodePrintF "   link sb,#0            Balance unlink%n";
  CodePrintF "   movea.l #0,a6	       Setup zeroareg%n";
  CodePrintF "   lea m0001,db	       Setup db reg%n";
  CodePrintF("   jsr   %w              Call Main routine%n",
		MainEntryPointNAme!*);

  CodePrintF "* now return to OS%n";
  CodePrintF "   movea.l A_PGM_$EXIT,a6%n";
  CodePrintF "   jsr     (a6)%n";
  CodePrintF "   unlk   sb             Reload callers SB%n";        
  CodePrintF "   addq.w  #8,sp         Pop linkage%n";
  CodePrintF "   movea.l (sp)+,db      Reload callers db%n";
  CodePrintF "   rts                   Return%n";
   ForeignExternList!*:=NIL;
   CheckForeignExtern 'PGM!_!$EXIT;
 >>
else
<<CodePrintF ("	module %w,m0000%n",ModName!*); 
	%/ Kludge, since ModuleName set in ASMOUT
  CodePrintF "	data%n";
  DataProcState!*:='data;
  CodeDeclareExternal 'SYMVAL; %/ Issue EXTERN.D early
  CodeDeclareExternal 'SYMFNC; %/ Issue EXTERN.D early
  CodePrintF "* this is an Independent Module %n";
  ForeignExternList!*:=NIL;
 >>;

lisp procedure DataFileHeader();
 Begin
  DataPrintF("  module %w_D%n",ModName!*);
  DataPrintF "	 data%n";
 End;

lisp procedure DataFileTrailer();
 DataPrintF "end%n";

lisp procedure CodeFileTrailer();
 <<Foreach Fn in Reverse ForeignExternList!* do
   <<CodePrintF("	extern.p %w%n",Fn);
     CodePrintF("A_%w      ac   %w%n",Fn,Fn)>>;
     CodePrintF "	end%n">>;

@end(ProgramExample)

        The general use of the headers given above is to declare the module
name, tell the assembler that this is a data section@Foot[On the @Apollo
all of the code and data were put in a data section since the operating
system and assembler had a problem with mixed code and data due to
expecting a pure code segment with all data references relative to the data
base register.], and in the
case of the main routine performing the proper operating system dependent
linkage for program entry and exit.

        Note that CodePrintF and DataPrintF are used to direct output to
either the @ei[code] segment or @ei[data] segment.  This is to allow
seperate segements for those machines that allow for pure code segments (on
the @Apollo a pure code segment is directly maped into the address space
rather than copied, which results in a large difference in start up speed).
This could probably be extended to PureCode, PureData, and ImpureData.


procedure WW(X);
 <<print LIST('WW,x); x+1>>;


Now a plain resolve function.
That does not argument processing
best for register conversion:

procedure MYREGFN(R,S);
 <<Print LIST('MYREG, R,S); 	
   List('REG,S+10)>>;

PUT('MYREG,'ANYREGRESOLUTIONFUNCTION,'MYREGFN);

procedure MYANYFN(R,S);
 <<Print LIST('MYANY, R,S); 	
   S:= ResolveOperand('(REG t3),S);
   List('Weird,S)>>;

FLAG('(WEIRD),'TERMINALOPERAND);
PUT('MYANY,'ANYREGRESOLUTIONFUNCTION,'MYANYFN);

(!*MOVE (WW 1) (WW 2)));   ARgs must be WCONSTEVALUABEL
(!*MOVE (WW (WW 1)) (WW 2)));
(!*MOVE (WW A) (WW 2)));   % First WW shouldnt convert

(!*MOVE (MYREG 1) (MYREG 2)));   % OK

(!*MOVE (MYREG (WW 1)) (WW (MYREG 2)))); % Fails since args not processed
(!*MOVE (MYREG (MYREG 1)) (MYREG 2)));

(!*MOVE (MYANY 1) (MYANY 2)));   % OK

(!*MOVE (MYANY (WW 1)) (MYANY (MYREG 2)))); %  Args  processed
(!*MOVE (MYANY (MYANY 1)) (MYANY 2)));

@section(Sample ANYREGs and CMACROs from various machines)

The following choice pieces from the @VAX750, @DEC20 and @68000
illustrate a range of addressing modes, predicates and style.

@subsection(VAX)
@begin(verbatim,leftmargin 0)
(DefCMacro !*Move               % ARGONE -> ARGTWO
   (Equal)                      % Don't do anything
   ((ZeroP AnyP) (@op{clrl} ARGTWO)) %  0 -> ARGTWO
   ((NegativeImmediateP AnyP)   % -n -> ARGTWO
    (@op{mnegl} (immediate (minus ARGONE)) ARGTWO))
   ((@op{movl} ARGONE ARGTWO)))      % General case

(DefCMacro !*WPlus2             % ARGONE+ARGTWO->ARGONE
   ((AnyP OneP) (@op{incl} ARGONE))  % add 1
   ((AnyP MinusOneP) (@op{decl} ARGONE)) % Subtract 1
   ((AnyP MinusP) (@op{subl2} (immediate (minus ARGTWO)) ARGONE))
   ((@op{addl2} ARGTWO ARGONE)))

The Predicates used:

@begin(description,spread 0)
Equal@\As an atom, rather than in (...), it check both arguments same.

Zerop@\Check if argument is 0

AnyP@\Just returns T

NegativeImmediateP@\Check that a negative, 32 bit constant.

@end(Description)
@end(verbatim)

@subsection(DEC-20)
@begin(verbatim,leftmargin 0)
(DefCMacro !*Move    % Move ArgOne -> ArgTwo
   (Equal)
   ((ZeroP AnyP) (@op{setzm} ARGTWO))
   ((MinusOneP AnyP) (@op{setom} ARGTWO))
   ((RegisterP AnyP) (@op{movem} ARGONE ARGTWO))
   ((NegativeImmediateP RegisterP)
    (@op{movni} ARGTWO (immediate (minus ARGONE))))
   ((ImmediateP RegisterP) (@op{hrrzi} ARGTWO ARGONE))
   ((AnyP RegisterP) (@op{move} ARGTWO ARGONE))
   ((!*MOVE ARGONE (reg t1)) (@op{movem} (reg t1) ARGTWO)))

(DefCMacro !*WPlus2
   ((AnyP OneP) (@op{aos} ARGONE))
   ((AnyP MinusOneP) (@op{sos} ARGONE))
   ((AnyP RegisterP) (@op{addm} ARGTWO ARGONE))
   ((RegisterP NegativeImmediateP) 
     (@op{subi} ARGTWO (minus ARGONE)))
   ((RegisterP ImmediateP) (@op{addi} ARGTWO ARGONE))
   ((RegisterP AnyP) (@op{add} ARGONE ARGTWO))
   ((!*MOVE ARGTWO (reg t2)) (@op{addm} (reg t2) ARGONE)))

The Predicates used:

@begin(description,spread 0)
Equal@\As an atom, rather than in (...), it check both arguments same.

Zerop@\Check if argument is 0

AnyP@\Just returns T

MinusOneP@\Check that argument is -1.

ImmediateP@\Check that an address or 18 bit constant.  Will
change for extended addressing.

NegativeImmediateP@\Check that a negative 18 bit constant.

RegisterP@\Check that is (REG r), a register.
@end(Description)
@end(verbatim)

@subsection(APOLLO)
@begin(verbatim,leftmargin 0)
(DefCMacro !*Move           %  (!*Move Source Destination)
   (Equal)                  % if source @Value(Eq) dest then do nothing
   ((ZeroP AregP)(@op{suba!.l} ARGTWO ARGTWO))
   ((ZeroP AnyP) (@op{clr!.l} ARGTWO))  % if source @Value(Eq) 0 then dest  :=  0
   ((InumP AregP) (@op{movea!.l} (Iconst ARGONE) ARGTWO))
   ((AddressP AregP) (@op{lea} ARGONE ARGTWO))
   ((InumP AnyP) (@op{move!.l} (Iconst ARGONE) ARGTWO))
   ((AddressP AnyP) 
(lea ARGONE (reg a0)) (@op{move!.l} (reg a0) ARGTWO))
   ((AnyP AregP) (@op{movea!.l} ARGONE ARGTWO))
   ((@op{move!.l} ARGONE ARGTWO)))

(DefCMacro !*WPlus2                %  (!*WPlus2 dest source) 
   ((AnyP QuickIconstP) (@op{addq!.l} (Iconst ARGTWO) ARGONE))
   ((AnyP NegativeQuickIconstP)
                  (@op{subq!.l} (Iconst (minus ARGTWO)) ARGONE))
   ((AregP MinusP) (@op{suba!.l} (Iconst (Minus ARGTWO)) ARGONE))
   ((AnyP MinusP) (@op{subi!.l} (Minus ARGTWO) ARGONE))
   ((AregP InumP) (@op{adda!.l} (Iconst ARGTWO) ARGONE))
   ((AnyP InumP) (@op{addi!.l} (Iconst ARGTWO) ARGONE))
   ((AregP AddressP) (@op{lea} ARGTWO (reg a0))
                            (@op{adda!.l} (reg a0) ARGONE))
   ((AnyP AddressP) (@op{lea} ARGTWO (reg a0))
                            (@op{add!.l} (reg a0) ARGONE))
   ((AregP AnyP)(@op{adda!.l} ARGTWO ARGONE))
   ((@op{add!.l} ARGTWO ARGONE)))   % really need one a DREG


The Predicates used:

@begin(description,spread 0)
Equal@\As an atom, rather than in (...), it check both arguments same.

Zerop@\Check if argument is 0

AregP@\Check that is one of the A registers (which can not be used for
arithmetic), and require  modified mnemonics.

DregP@\Check that is one of the D registers, used for most
arithmetic.

InumP@\Check that a small integer.

AddressP@\Check that an address, not a constant, since we need to use
different instruction for Address's, e.g@. @op{lea} vs @op{movi}.

AnyP@\Just returns T.

NegativeImmediateP@\Check that a negative, 32 bit constant.

QuickIconstP@\Small integer in range 1 ..@. 8 for the xxxxQ instructions on
68000.

NegativeQuickIconstP@\Small integer in range -8 ..@. -1 for the xxxxQ
instructions on 68000.
@end(Description)
@end(verbatim)


@begin(verbatim,leftmargin 0)
For example, on the @VAX750:
@begin(Group)
(DefAnyreg CAR	                      % First ITEM of pair
	   AnyregCAR                  % Associated function
	   ((@op{extzv} 0 27 SOURCE REGISTER)
				      % Code to extract 27 bit
				      %  address, masking TAG
            (Deferred REGISTER)))     % Finally indexed mode used
@hinge
(DefAnyreg CDR                        % Second item
	   AnyregCDR
	   ((@op{extzv} 0 27 SOURCE REGISTER) 
            (Displacement REGISTER 4)))
                              % Displace 4 bytes off Register

% Both CAR and CDR use a single instruction, so do not use a
% predicate to test SOURCE.
@hinge
(DefAnyreg QUOTE             % Note a set of different choices
	   AnyregQUOTE
	   ((Null) (REG NIL))
	   ((EqTP) (FLUID T))
	   ((InumP) SOURCE)
	   ((QUOTE SOURCE)))
@hinge

(DefCMACRO !*Move            % !*MOVE Usually has the most cases
	   (Equal)
	   ((ZeroP AnyP) (@op{clrl} ARGTWO))
	   ((NegativeImmediateP AnyP)
	    (@op{mnegl} (immediate (minus ARGONE)) ARGTWO))
	   ((@op{movl} ARGONE ARGTWO)))
@hinge

(DefCMACRO !*Alloc
	   ((ZeroP))   % No BODY - nothing to allocate
	   ((@op{subl2} ARGONE (REG st))))
@end(group)
@end(verbatim)

