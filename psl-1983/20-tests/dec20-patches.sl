% DEC20-PATCHES.SL
% to convert to Portable, 2 reg for LINK model
% From DEC20-Asm.RED
% These will now be simpler than 20, just JRST
% Should even be InternalEntry for efficiency, avoid circular defns
% Right now, expect same as !%Store!-JCALL would install

(SETQ UndefinedFunctionCellInstructions!*
	       '((!*JCALL  UndefinedFunction)))
                       
(SETQ LambdaFunctionCellInstructions!* 
	       '((!*JCALL  CompiledCallingInterpreted)))

(Put 'LinkReg 'RegisterName 12)
(Put 'NargReg 'RegisterName 13)

% From PC:Common-Cmacros.sl

(de MakeLinkRegs(Fn Nargs)
  (cond ((FlagP Fn 'NoLinkage) NIL)
      (T (list (list '!*Move (list 'IdLoc FunctionName) '(reg LinkReg) )
               (list '!*Move (list 'Wconst NumberofArguments) '(reg NargReg) )
      ))))

(FLAG '(IDapply0 IDapply1 IDapply2 IDapply3 IDapply4) 'NoLinkage)

(de !*Link (FunctionName FunctionType NumberOfArguments)
  (cond ((FlagP FunctionName 'ForeignFunction)
	     (list  (list '!*ForeignLink
		             FunctionName
		             FunctionType
		             NumberOfArguments)))
   (t (append (MakeLinkRegs FunctionName NumberofArguments)
              (list (list '!*Call FunctionName))))))


(de !*LinkE (DeAllocCount FunctionName FunctionType NumberOfArguments)
  (cons (list '!*DeAlloc DeAllocCount)
	(cond ((FlagP FunctionName 'ForeignFunction)
	       (list (list '!*ForeignLink
			   FunctionName
			   FunctionType
			   NumberOfArguments)
		     '(!*Exit 0)))
    (t (Append (MakeLinkRegs FunctionName NumberofArguments)
               (list (list '!*JCall FunctionName)))))))

(DefList '((IDApply0  (
                (!*move (Wconst 0) (reg NargReg))
                (!*move (reg 1) (reg LinkReg))
      %         (!*Wtimes2 (reg 1) (Wconst AddressingUnitsPerFunctionCell))
		(pushj (reg st) (Indexed (reg 1) (WArray SymFnc)))))
	   (IDApply1  (
                (!*move (Wconst 1) (reg NargReg))
                (!*move (reg 2) (reg LinkReg))
      %	        (!*Wtimes2 (reg 2) (Wconst AddressingUnitsPerFunctionCell))
		(pushj (reg st) (Indexed (reg 2) (WArray SymFnc)))))
	   (IDApply2  (
                (!*move (Wconst 2) (reg NargReg))
                (!*move (reg 3) (reg LinkReg))
      %	        (!*Wtimes2 (reg 3) (Wconst AddressingUnitsPerFunctionCell))
		(pushj (reg st) (Indexed (reg 3) (WArray SymFnc)))))
	   (IDApply3  (
                (!*move (Wconst 3) (reg NargReg))
                (!*move (reg 4) (reg LinkReg))
      %	        (!*Wtimes2 (reg 4) (Wconst AddressingUnitsPerFunctionCell))
		(pushj (reg st) (Indexed (reg 4) (WArray SymFnc)))))
	   (IDApply4  (
                (!*move (Wconst 4) (reg NargReg))
                (!*move (reg 5) (reg LinkReg))
      %	        (!*Wtimes2 (reg 5) (Wconst AddressingUnitsPerFunctionCell))
		(pushj (reg st) (Indexed (reg 5) (WArray SymFnc)))))
)   'OpenCode)


(DefList '((IDApply0  (
                (!*move (Wconst 0) (reg NargReg))
                (!*move (reg 1) (reg LinkReg))
	      % (!*wtimes2 (reg 1) (Wconst AddressingUnitsPerFunctionCell))
		(jrst (Indexed (reg 1) (WArray SymFnc)))))
	   (IDApply1 (
                (!*move (Wconst 1) (reg NargReg))
                (!*move (reg 2) (reg LinkReg))
	      % (!*wtimes2 (reg 2) (Wconst AddressingUnitsPerFunctionCell))
		(jrst (Indexed (reg 2) (WArray SymFnc)))))
	   (IDApply2 (
                (!*move (Wconst 2) (reg NargReg))
                (!*move (reg 3) (reg LinkReg))
	      % (!*wtimes2 (reg 3) (Wconst AddressingUnitsPerFunctionCell))
		(jrst (Indexed (reg 3) (WArray SymFnc)))))
	   (IDApply3 (
                (!*move (Wconst 3) (reg NargReg))
                (!*move (reg 4) (reg LinkReg))
	      % (!*wtimes2 (reg 4) (Wconst AddressingUnitsPerFunctionCell))
		(jrst (Indexed (reg 4) (WArray SymFnc)))))
	   (IDApply4 (
                (!*move (Wconst 4) (reg NargReg))
                (!*move (reg 5) (reg LinkReg))
	      % (!*wtimes2 (reg 5) (Wconst AddressingUnitsPerFunctionCell))
		(jrst (Indexed (reg 5) (WArray SymFnc)))))
)	 'ExitOpenCode)

% From PC:lap-to-asm.red

(de DataPrintUndefinedFunctionCell ()
  (Prog (OldOut)
    (setq OldOut (WRS DataOut!*))
    (foreach X in (Pass1Lap UndefinedFunctionCellInstructions!*) do
	(ASMOutLap1 X))
    (WRS OldOut)))

(DSKIN "PC:P-LAMBIND.SL")

% new SYSLISP bug, perhaps useful refefined it?

(off usermode)

(dm for(u) ( MkFor1 u))
