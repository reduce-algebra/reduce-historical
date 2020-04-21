%
% 20-CMAC.SL - Patterns and predicates for Dec-20 PSL cmacro expansion
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        14 January 1982
% Copyright (c) 1982 University of Utah
%

% <PSL.20-COMP>20-CMAC.SL.1, 21 October 1982, Griss
% Fixed foreign function for CROSS compiler

% <PSL.20-COMP>20-CMAC.SL.1, 24-Feb-82 12:08:45, Edit by BENSON
% Adapted VAX version for Dec-20


(fluid '(AddressingUnitsPerItem
	 CharactersPerWord
	 StackDirection
	 !*ImmediateQuote
	 AddressingUnitsPerFunctionCell))

(setq AddressingUnitsPerItem 1)

(setq CharactersPerWord 5)

(setq AddressingUnitsPerFunctionCell 1)

(setq StackDirection 1)

(setq !*ImmediateQuote NIL)
(*
(* "MkItem may be used when evaluating WConst expressions.")

(de MkItem (TagPart InfPart)
  (lor (lsh TagPart 27) (land InfPart 16#7ffffff)))
)

(ds BitMask (Start End)
  (land (lsh -1 (minus Start)) (lsh -1 (difference 35 End))))

(dm Bit (U)
  (progn (setq U (cdr U))
	 (cond ((null U) 0)
	       (t (ExpandBit U)))))

(de ExpandBit (U)
  (cond ((null (cdr U)) (list 'lsh 1 (list 'difference 35 (car U))))
	(t (list 'lor
		 (list 'lsh 1 (list 'difference 35 (car U)))
		 (ExpandBit (cdr U))))))

(* "InumP tells what numbers can be immediate operands on the target machine.")

(de InumP (Expression)
  (and (FixP Expression)
       (leq Expression 8#777777)		% 8#177777777777 for extended
       (geq Expression (minus 8#1000000))))	% 8#200000000000

(de TagNumber (X)
  (cond ((IDP X) (get 'ID 'WConst))
	((PairP X) (get 'PAIR 'WConst))
	((StringP X) (get 'STR 'WConst))
	((InumP X) (cond ((MinusP X) 31) (t 0)))
	((CodeP X) (get 'CODE 'WConst))
	((FloatP X) (get 'FltN 'WConst))
	((VectorP X) (get 'VECT 'WConst))
	((FixP X) (get 'FixN 'WConst))))

(de ImmediateP (X)
  (or (EqCar X 'Immediate)
      (and (FixP X) (leq X 8#777777) (geq X (minus 8#777777)))))

(de MemoryP (X)
  (not (ImmediateP X)))

(de NegativeImmediateP (X)
  (and (FixP X)
       (MinusP X)
       (geq X (minus 8#777777))))

(de EighteenP (X)
  (equal X 18))

(de NonIndirectP (Expression)
  (not (EqCar Expression 'Indirect)))

(de FakeRegisterNumberP (Expression)
  (and (IntP Expression) (GreaterP Expression 5)))


(* "Leave Indexed and Indirect alone in recursive c-macro")

(flag '(Indexed Indirect UnImmediate) 'TerminalOperand)

(DefAnyreg CAR
	   AnyregCAR
	   ((RegisterP) (Indexed SOURCE 0))
	   ((move REGISTER SOURCE) (Indexed REGISTER 0)))

(DefAnyreg CDR
	   AnyregCDR
	   ((RegisterP) (Indexed SOURCE 1))
	   ((move REGISTER SOURCE) (Indexed REGISTER 1)))

(DefAnyreg QUOTE
	   AnyregQUOTE
	   ((Null) (REG NIL))
	   ((EqTP) (FLUID T))
	   ((InumP) SOURCE)
	   ((QUOTE SOURCE)))

(DefAnyreg WVAR
	   AnyregWVAR
	   ((RegisterNameP) (REG SOURCE))
	   ((WVAR SOURCE)))

(DefAnyreg MEMORY
	   AnyregMEMORY
	   ((RegisterP AnyP) (Indexed SOURCE ARGTWO))
	   ((AddressConstantP ZeroP) (UnImmediate SOURCE))
	   ((NonIndirectP ZeroP) (Indirect SOURCE))
	   ((!*MOVE SOURCE REGISTER)
	    (Indexed REGISTER ARGTWO)))

(DefAnyreg FRAME
	   AnyregFRAME
	   ((Indexed (REG st) SOURCE)))

(DefAnyreg REG
	   AnyregREG
	   ((FakeRegisterNumberP) (ExtraReg SOURCE))
	   ((REG SOURCE)))

(DefCMacro !*Call
	   ((InternallyCallableP) (pushj (reg st) (InternalEntry ARGONE)))
	   ((pushj (reg st) (Entry ARGONE))))

(DefCMacro !*JCall
	   ((InternallyCallableP) (jrst (InternalEntry ARGONE)))
	   ((jrst (Entry ARGONE))))

(DefCMacro !*Move
	   (Equal)
	   ((ZeroP AnyP) (setzm ARGTWO))
	   ((MinusOneP AnyP) (setom ARGTWO))
	   ((NegativeImmediateP RegisterP)
	    (movni ARGTWO (minus ARGONE)))
	   ((ImmediateP RegisterP) (hrrzi ARGTWO ARGONE))
	   ((AnyP RegisterP) (move ARGTWO ARGONE))
	   ((RegisterP AnyP) (movem ARGONE ARGTWO))
	   ((!*MOVE ARGONE (reg t1)) (movem (reg t1) ARGTWO)))

(DefCMacro !*Alloc
	   ((ZeroP))
	   ((adjsp (REG st) ARGONE)))

(DefCMacro !*DeAlloc
	   ((ZeroP))
	   ((adjsp (REG st) (minus ARGONE))))

(DefCMacro !*Exit
	   ((!*DeAlloc ARGONE)
	    (popj (reg st) 0)))

(DefCMacro !*Jump
	   ((jrst ARGONE)))

(DefCMacro !*Lbl
	   (ARGONE))

(DefCMacro !*WPlus2
	   ((AnyP OneP) (aos ARGONE))
	   ((AnyP MinusOneP) (sos ARGONE))
	   ((AnyP RegisterP) (addm ARGTWO ARGONE))
	   ((RegisterP NegativeImmediateP) (subi ARGONE (minus ARGTWO)))
	   ((RegisterP ImmediateP) (addi ARGONE ARGTWO))
	   ((RegisterP AnyP) (add ARGONE ARGTWO))
	   ((!*MOVE ARGTWO (reg t2)) (addm (reg t2) ARGONE)))

(DefCMacro !*WDifference
	   ((AnyP OneP) (sos ARGONE))
	   ((AnyP MinusOneP) (aos ARGONE))
	   ((RegisterP NegativeImmediateP) (addi ARGONE (minus ARGTWO)))
	   ((RegisterP ImmediateP) (subi ARGONE ARGTWO))
	   ((RegisterP AnyP) (sub ARGONE ARGTWO))
	   ((!*WMINUS (reg t2) ARGTWO) (addm (reg t2) ARGONE)))

(DefCMacro !*WTimes2
	   ((AnyP MinusOneP) (!*WMINUS ARGONE ARGONE))
	   ((RegisterP NegativeImmediateP)
	    (imul ARGONE (lit (fullword ARGTWO))))
	   ((RegisterP ImmediateP) (imuli ARGONE ARGTWO))
	   ((RegisterP AnyP) (imul ARGONE ARGTWO))
	   ((AnyP RegisterP) (imulm ARGTWO ARGONE))
	   ((!*MOVE ARGTWO (reg t2)) (imulm (reg t2) ARGONE)))

(DefCMacro !*WAnd
	   ((RegisterP NegativeImmediateP)
	    (and ARGONE (lit (fullword ARGTWO))))
	   ((RegisterP ImmediateP) (andi ARGONE ARGTWO))
	   ((RegisterP AnyP) (and ARGONE ARGTWO))
	   ((AnyP RegisterP) (andm ARGTWO ARGONE))
	   ((!*MOVE (reg t2) ARGTWO) (andm (reg t2) ARGONE)))

(DefCMacro !*WOr
	   ((RegisterP NegativeImmediateP)
	    (ior ARGONE (lit (fullword ARGTWO))))
	   ((RegisterP ImmediateP) (iori ARGONE ARGTWO))
	   ((RegisterP AnyP) (ior ARGONE ARGTWO))
	   ((AnyP RegisterP) (iorm ARGTWO ARGONE))
	   ((!*MOVE (reg t2) ARGTWO) (iorm (reg t2) ARGONE)))

(DefCMacro !*WXOr
	   ((RegisterP NegativeImmediateP)
	    (xor ARGONE (lit (fullword ARGTWO))))
	   ((RegisterP ImmediateP) (xori ARGONE ARGTWO))
	   ((RegisterP AnyP) (xor ARGONE ARGTWO))
	   ((AnyP RegisterP) (xorm ARGTWO ARGONE))
	   ((!*MOVE (reg t2) ARGTWO) (xorm (reg t2) ARGONE)))

(DefCMacro !*AShift
	   ((RegisterP ImmediateP) (ash ARGONE ARGTWO))
	   ((RegisterP RegisterP) (ash ARGONE (Indexed ARGTWO 0)))
	   ((RegisterP AnyP)
	    (move (reg t2) ARGTWO)
	    (ash ARGONE (Indexed (reg t2) 0)))
	   ((AnyP ImmediateP)
	    (move (reg t3) ARGONE)
	    (ash (reg t3) ARGTWO)
	    (movem (reg t3) ARGONE))
	   ((AnyP RegisterP)
	    (move (reg t3) ARGONE)
	    (ash (reg t3) (Indexed ARGTWO 0))
	    (movem (reg t3) ARGONE))
	   ((move (reg t2) ARGTWO)
	    (move (reg t3) ARGONE)
	    (ash (reg t3) (Indexed (reg t2) 0))
	    (movem (reg t3) ARGONE)))

(DefCMacro !*WShift
	   ((RegisterP ImmediateP) (lsh ARGONE ARGTWO))
	   ((RegisterP RegisterP) (lsh ARGONE (Indexed ARGTWO 0)))
	   ((RegisterP AnyP)
	    (move (reg t2) ARGTWO)
	    (lsh ARGONE (Indexed (reg t2) 0)))
	   ((AnyP ImmediateP)
	    (move (reg t3) ARGONE)
	    (lsh (reg t3) ARGTWO)
	    (movem (reg t3) ARGONE))
	   ((AnyP RegisterP)
	    (move (reg t3) ARGONE)
	    (lsh (reg t3) (Indexed ARGTWO 0))
	    (movem (reg t3) ARGONE))
	   ((move (reg t2) ARGTWO)
	    (move (reg t3) ARGONE)
	    (lsh (reg t3) (Indexed (reg t2) 0))
	    (movem (reg t3) ARGONE)))

(DefCMacro !*WNot
	   (Equal (setcmm ARGONE))
	   ((RegisterP AnyP) (setcm ARGONE ARGTWO))
	   ((AnyP RegisterP) (setcam ARGTWO ARGONE))
	   ((move (reg t1) ARGTWO) (setcam (reg t1) ARGONE)))

(DefCMacro !*WMinus
	   (Equal (movns ARGONE))
	   ((RegisterP AnyP) (movn ARGONE ARGTWO))
	   ((AnyP RegisterP) (movnm ARGTWO ARGONE))
	   ((move (reg t1) ARGTWO) (movnm (reg t1) ARGONE)))

(DefCMacro !*MkItem
	   ((RegisterP ImmediateP)
	    (tlz ARGONE 2#111110000000000000)
	    (tlo ARGONE (lsh ARGTWO 13)))
	   ((AnyP RegisterP)
	    (dpb ARGTWO (lit (fullword (FieldPointer ARGONE 0 5)))))
	   ((!*MOVE ARGTWO (reg t1))
	    (dpb (reg t1) (lit (fullword (FieldPointer ARGONE 0 5))))))

(DefCMacro !*JumpType
	   ((RegisterP ZeroP)
	    (tlnn ARGONE 2#111110000000000000)
	    (jrst ARGTHREE))
	   ((ldb (reg t6) (lit (fullword (FieldPointer ARGONE 0 5))))
	    (!*JUMPEQ ARGTHREE (reg t6) ARGTWO)))

(DefCMacro !*JumpNotType
	   ((RegisterP ZeroP)
	    (tlne ARGONE 2#111110000000000000)
	    (jrst ARGTHREE))
	   ((ldb (reg t6) (lit (fullword (FieldPointer ARGONE 0 5))))
	    (!*JUMPNOTEQ ARGTHREE (reg t6) ARGTWO)))

(DefCMacro !*JumpInType
	   ((ldb (reg t6) (lit (fullword (FieldPointer ARGONE 0 5))))
	    (caig (reg t6) ARGTWO)
	    (jrst ARGTHREE)
	    (cain (reg t6) 31)
	    (jrst ARGTHREE)))		% (WConst NegInt)

(DefCMacro !*JumpNotInType
	   ((ldb (reg t6) (lit (fullword (FieldPointer ARGONE 0 5))))
	    (cain (reg t6) 31)		% (WConst NegInt)
	    (jrst TEMPLABEL)
	    (caile (reg t6) ARGTWO)
	    (jrst ARGTHREE)
	    TEMPLABEL))

(DefCMacro !*JumpEQ
	   ((RegisterP ZeroP) (jumpe ARGONE ARGTHREE))
	   ((ZeroP RegisterP) (jumpe ARGTWO ARGTHREE))
	   ((AnyP ZeroP)
	    (skipn ARGONE)
	    (jrst ARGTHREE))
	   ((ZeroP AnyP)
	    (skipn ARGTWO)
	    (jrst ARGTHREE))
	   ((RegisterP NegativeImmediateP)
	    (camn ARGONE (lit (fullword ARGTWO)))
	    (jrst ARGTHREE))
	   ((NegativeImmediateP RegisterP)
	    (camn ARGTWO (lit (fullword ARGONE)))
	    (jrst ARGTHREE))
	   ((RegisterP ImmediateP)
	    (cain ARGONE ARGTWO)
	    (jrst ARGTHREE))
	   ((ImmediateP RegisterP)
	    (cain ARGTWO ARGONE)
	    (jrst ARGTHREE))
	   ((RegisterP AnyP)
	    (camn ARGONE ARGTWO)
	    (jrst ARGTHREE))
	   ((AnyP RegisterP)
	    (camn ARGTWO ARGONE)
	    (jrst ARGTHREE))
	   ((MemoryP AnyP)
	    (move (reg t1) ARGONE)
	    (!*JUMPEQ ARGTHREE (reg t1) ARGTWO))
	   ((move (reg t2) ARGTWO)
	    (!*JUMPEQ ARGTHREE ARGONE (reg t2))))

(DefCMacro !*JumpNotEQ
	   ((RegisterP ZeroP) (jumpn ARGONE ARGTHREE))
	   ((ZeroP RegisterP) (jumpn ARGTWO ARGTHREE))
	   ((AnyP ZeroP)
	    (skipe ARGONE)
	    (jrst ARGTHREE))
	   ((ZeroP AnyP)
	    (skipe ARGTWO)
	    (jrst ARGTHREE))
	   ((RegisterP NegativeImmediateP)
	    (came ARGONE (lit (fullword ARGTWO)))
	    (jrst ARGTHREE))
	   ((NegativeImmediateP RegisterP)
	    (came ARGTWO (lit (fullword ARGONE)))
	    (jrst ARGTHREE))
	   ((RegisterP ImmediateP)
	    (caie ARGONE ARGTWO)
	    (jrst ARGTHREE))
	   ((ImmediateP RegisterP)
	    (caie ARGTWO ARGONE)
	    (jrst ARGTHREE))
	   ((RegisterP AnyP)
	    (came ARGONE ARGTWO)
	    (jrst ARGTHREE))
	   ((AnyP RegisterP)
	    (came ARGTWO ARGONE)
	    (jrst ARGTHREE))
	   ((MemoryP AnyP)
	    (move (reg t1) ARGONE)
	    (!*JUMPNOTEQ ARGTHREE (reg t1) ARGTWO))
	   ((move (reg t2) ARGTWO)
	    (!*JUMPNOTEQ ARGTHREE ARGONE (reg t2))))

(DefCMacro !*JumpWLessP
	   ((RegisterP ZeroP) (jumpl ARGONE ARGTHREE))
	   ((ZeroP RegisterP) (jumpg ARGTWO ARGTHREE))
	   ((RegisterP OneP) (jumple ARGONE ARGTHREE))
	   ((MinusOneP RegisterP) (jumpge ARGTWO ARGTHREE))
	   ((AnyP ZeroP)
	    (skipge ARGONE)
	    (jrst ARGTHREE))
	   ((ZeroP AnyP)
	    (skiple ARGTWO)
	    (jrst ARGTHREE))
	   ((AnyP OneP)
	    (skipg ARGONE)
	    (jrst ARGTHREE))
	   ((MinusOneP AnyP)
	    (skipl ARGTWO)
	    (jrst ARGTHREE))
	   ((RegisterP NegativeImmediateP)
	    (camge ARGONE (lit (fullword ARGTWO)))
	    (jrst ARGTHREE))
	   ((NegativeImmediateP RegisterP)
	    (camle ARGTWO (lit (fullword ARGONE)))
	    (jrst ARGTHREE))
	   ((RegisterP ImmediateP)
	    (caige ARGONE ARGTWO)
	    (jrst ARGTHREE))
	   ((ImmediateP RegisterP)
	    (caile ARGTWO ARGONE)
	    (jrst ARGTHREE))
	   ((RegisterP AnyP)
	    (camge ARGONE ARGTWO)
	    (jrst ARGTHREE))
	   ((AnyP RegisterP)
	    (camle ARGTWO ARGONE)
	    (jrst ARGTHREE))
	   ((MemoryP AnyP)
	    (move (reg t1) ARGONE)
	    (!*JUMPWLESSP ARGTHREE (reg t1) ARGTWO))
	   ((move (reg t2) ARGTWO)
	    (!*JUMPWLESSP ARGTHREE ARGONE (reg t2))))

(DefCMacro !*JumpWGreaterP
	   ((RegisterP ZeroP) (jumpg ARGONE ARGTHREE))
	   ((ZeroP RegisterP) (jumpl ARGTWO ARGTHREE))
	   ((RegisterP MinusOneP) (jumpge ARGONE ARGTHREE))
	   ((OneP RegisterP) (jumple ARGTWO ARGTHREE))
	   ((AnyP ZeroP)
	    (skiple ARGONE)
	    (jrst ARGTHREE))
	   ((ZeroP AnyP)
	    (skipge ARGTWO)
	    (jrst ARGTHREE))
	   ((AnyP MinusOneP)
	    (skipl ARGONE)
	    (jrst ARGTHREE))
	   ((OneP AnyP)
	    (skipg ARGTWO)
	    (jrst ARGTHREE))
	   ((RegisterP NegativeImmediateP)
	    (camle ARGONE (lit (fullword ARGTWO)))
	    (jrst ARGTHREE))
	   ((NegativeImmediateP RegisterP)
	    (camge ARGTWO (lit (fullword ARGONE)))
	    (jrst ARGTHREE))
	   ((RegisterP ImmediateP)
	    (caile ARGONE ARGTWO)
	    (jrst ARGTHREE))
	   ((ImmediateP RegisterP)
	    (caige ARGTWO ARGONE)
	    (jrst ARGTHREE))
	   ((RegisterP AnyP)
	    (camle ARGONE ARGTWO)
	    (jrst ARGTHREE))
	   ((AnyP RegisterP)
	    (camge ARGTWO ARGONE)
	    (jrst ARGTHREE))
	   ((MemoryP AnyP)
	    (move (reg t1) ARGONE)
	    (!*JUMPWGreaterP ARGTHREE (reg t1) ARGTWO))
	   ((move (reg t2) ARGTWO)
	    (!*JUMPWGreaterP ARGTHREE ARGONE (reg t2))))

(DefCMacro !*JumpWLEQ
	   ((RegisterP ZeroP) (jumple ARGONE ARGTHREE))
	   ((ZeroP RegisterP) (jumpge ARGTWO ARGTHREE))
	   ((RegisterP MinusOneP) (jumpl ARGONE ARGTHREE))
	   ((OneP RegisterP) (jumpg ARGTWO ARGTHREE))
	   ((AnyP ZeroP)
	    (skipg ARGONE)
	    (jrst ARGTHREE))
	   ((ZeroP AnyP)
	    (skipl ARGTWO)
	    (jrst ARGTHREE))
	   ((AnyP MinusOneP)
	    (skipge ARGONE)
	    (jrst ARGTHREE))
	   ((OneP AnyP)
	    (skiple ARGTWO)
	    (jrst ARGTHREE))
	   ((RegisterP NegativeImmediateP)
	    (camg ARGONE (lit (fullword ARGTWO)))
	    (jrst ARGTHREE))
	   ((NegativeImmediateP RegisterP)
	    (caml ARGTWO (lit ARGTHREE))
	    (jrst ARGTHREE))
	   ((RegisterP ImmediateP)
	    (caig ARGONE ARGTWO)
	    (jrst ARGTHREE))
	   ((ImmediateP RegisterP)
	    (cail ARGTWO ARGONE)
	    (jrst ARGTHREE))
	   ((RegisterP AnyP)
	    (camg ARGONE ARGTWO)
	    (jrst ARGTHREE))
	   ((AnyP RegisterP)
	    (caml ARGTWO ARGONE)
	    (jrst ARGTHREE))
	   ((MemoryP AnyP)
	    (move (reg t1) ARGONE)
	    (!*JUMPWLEQ ARGTHREE (reg t1) ARGTWO))
	   ((move (reg t2) ARGTWO)
	    (!*JUMPWLEQ ARGTHREE ARGONE (reg t2))))

(DefCMacro !*JumpWGEQ
	   ((RegisterP ZeroP) (jumpge ARGONE ARGTHREE))
	   ((ZeroP RegisterP) (jumple ARGTWO ARGTHREE))
	   ((RegisterP OneP) (jumpg ARGONE ARGTHREE))
	   ((MinusOneP RegisterP) (jumpl ARGTWO ARGTHREE))
	   ((AnyP ZeroP)
	    (skipl ARGONE)
	    (jrst ARGTHREE))
	   ((ZeroP AnyP)
	    (skipg ARGTWO)
	    (jrst ARGTHREE))
	   ((AnyP OneP)
	    (skiple ARGONE)
	    (jrst ARGTHREE))
	   ((MinusOneP AnyP)
	    (skipge ARGTWO)
	    (jrst ARGTHREE))
	   ((RegisterP NegativeImmediateP)
	    (caml ARGONE (lit (fullword ARGTWO)))
	    (jrst ARGTHREE))
	   ((NegativeImmediateP RegisterP)
	    (camg ARGTWO (lit (fullword ARGONE)))
	    (jrst ARGTHREE))
	   ((RegisterP ImmediateP)
	    (cail ARGONE ARGTWO)
	    (jrst ARGTHREE))
	   ((ImmediateP RegisterP)
	    (caig ARGTWO ARGONE)
	    (jrst ARGTHREE))
	   ((RegisterP AnyP)
	    (caml ARGONE ARGTWO)
	    (jrst ARGTHREE))
	   ((AnyP RegisterP)
	    (camg ARGTWO ARGONE)
	    (jrst ARGTHREE))
	   ((MemoryP AnyP)
	    (move (reg t1) ARGONE)
	    (!*JUMPWGEQ ARGTHREE (reg t1) ARGTWO))
	   ((move (reg t2) ARGTWO)
	    (!*JUMPWGEQ ARGTHREE ARGONE (reg t2))))

(DefCMacro !*Push
	   ((ImmediateP) (push (reg st) (lit (fullword ARGONE))))
	   ((push (reg st) ARGONE)))

(DefCMacro !*Pop
	   ((ImmediateP) (pop (reg st) (lit (fullword ARGONE))))
	   ((pop (reg st) ARGONE)))

(DefCMacro !*Freerstr
	   ((jsp (reg t5) (Entry FastUnbind)) (fullword ARGONE)))

(DefCMacro !*Loc
	   ((RegisterP AnyP) (movei ARGONE ARGTWO))
	   ((movei (reg t2) ARGTWO) (movem (reg t2) ARGONE)))

(DefCMacro !*Field
	   ((RegisterP AnyP ZeroP EighteenP) (hlrz ARGONE ARGTWO))
	   ((RegisterP AnyP EighteenP EighteenP) (hrrz ARGONE ARGTWO))
	   ((AnyP RegisterP ZeroP EighteenP) (hlrzm ARGTWO ARGONE))
	   ((AnyP RegisterP EighteenP EighteenP) (hrrzm ARGTWO ARGONE))
	   ((RegisterP)
	    (ldb ARGONE
		 (lit (fullword (FieldPointer
					      ARGTWO ARGTHREE
					      ARGFOUR)))))
	   ((ldb (reg t2)
		 (lit (fullword (FieldPointer
					      ARGTWO ARGTHREE
					      ARGFOUR))))
	    (movem (reg t2) ARGONE)))

(DefCMacro !*SignedField
	   ((RegisterP AnyP ZeroP EighteenP) (hlre ARGONE ARGTWO))
	   ((RegisterP AnyP EighteenP EighteenP) (hrre ARGONE ARGTWO))
	   ((AnyP RegisterP ZeroP EighteenP) (hlrem ARGTWO ARGONE))
	   ((AnyP RegisterP EighteenP EighteenP) (hrrem ARGTWO ARGONE))
	   ((RegisterP)
	    % could optimize to use tlne tlo trne tro
	    (ldb ARGONE
		 (lit (fullword (FieldPointer
					      ARGTWO ARGTHREE
					      ARGFOUR))))
	    (tdne ARGONE (lit (fullword (bit ARGTHREE))))
	    (tdo ARGONE (lit (fullword (bitmask 0 ARGTHREE)))))
	   ((ldb (reg t2)
		 (lit (fullword (FieldPointer
					      ARGTWO ARGTHREE
					      ARGFOUR))))
	    (tdne (reg t2) (lit (fullword (bit ARGTHREE))))
	    (tdo (reg t2) (lit (fullword (bitmask 0 ARGTHREE))))
	    (movem (reg t2) ARGONE)))

(DefCMacro !*PutField
	   ((RegisterP)
	    (dpb ARGONE
		 (lit (fullword (FieldPointer
					      ARGTWO ARGTHREE
					      ARGFOUR)))))
	   ((!*MOVE ARGONE (reg t1))
	    (dpb (reg t1)
		 (lit (fullword (FieldPointer
					      ARGTWO ARGTHREE
					      ARGFOUR))))))

(DefCMacro !*ADJSP
	   ((RegisterP ImmediateP) (adjsp ARGONE ARGTWO))
	   ((RegisterP RegisterP) (adjsp ARGONE (Indexed ARGTWO 0)))
	   ((RegisterP)
	    (move (reg t2) ARGTWO)
	    (adjsp ARGONE (Indexed (reg t2) 0)))
	   ((move (reg t1) ARGONE)
	    (!*ADJSP (reg t1) ARGTWO)
	    (movem (reg t1) ARGONE)))

(DefList '((WQuotient ((idiv (reg 1) (reg 2))))
	   (WRemainder ((idiv (reg 1) (reg 2)) (move (reg 1) (reg 2)))))
	 'OpenCode)

(!&Tworeg '(WQuotient WRemainder))

(loadtime
(DefList '((Byte ((adjbp (reg 2)
			 (lit (fullword (FieldPointer
					  (Indexed (reg 1) 0) 0 7))))
		  (ldb (reg 1) (reg 2))))
	   (PutByte ((adjbp (reg 2)
			    (lit (fullword (FieldPointer
					     (Indexed (reg 1) 0) 0 7))))
		     (dpb (reg 3) (reg 2))))
	   (HalfWord ((adjbp (reg 2)
			     (lit (fullword (FieldPointer
					      (Indexed (reg 1) 0) 0 18))))
		      (ldb (reg 1) (reg 2))))
	   (PutHalfWord ((adjbp (reg 2)
				(lit (fullword (FieldPointer
						 (Indexed (reg 1) 0) 0 18))))
			 (dpb (reg 3) (reg 2))))
	   (BitTable ((adjbp (reg 2)
			     (lit (fullword (FieldPointer
					      (Indexed (reg 1) 0) 0 2))))
		      (ldb (reg 1) (reg 2))))
	   (PutBitTable ((adjbp (reg 2)
				(lit (fullword (FieldPointer
						 (Indexed (reg 1) 0) 0 2))))
			 (dpb (reg 3) (reg 2)))))
	 'OpenCode))

(loadtime
(!&TwoReg '(Byte PutByte HalfWord PutHalfWord BitTable PutBitTable)))

(DefList '((IDApply0 ((pushj (reg st)
			     (Indexed (reg 1) (WArray SymFnc)))))
	   (IDApply1 ((pushj (reg st)
			     (Indexed (reg 2) (WArray SymFnc)))))
	   (IDApply2 ((pushj (reg st)
			     (Indexed (reg 3) (WArray SymFnc)))))
	   (IDApply3 ((pushj (reg st)
			     (Indexed (reg 4) (WArray SymFnc)))))
	   (IDApply4 ((pushj (reg st)
			     (Indexed (reg 5) (WArray SymFnc))))))
	 'OpenCode)

(DefList '((IDApply0 ((jrst (Indexed (reg 1) (WArray SymFnc)))))
	   (IDApply1 ((jrst (Indexed (reg 2) (WArray SymFnc)))))
	   (IDApply2 ((jrst (Indexed (reg 3) (WArray SymFnc)))))
	   (IDApply3 ((jrst (Indexed (reg 4) (WArray SymFnc)))))
	   (IDApply4 ((jrst (Indexed (reg 5) (WArray SymFnc))))))
	 'ExitOpenCode)

(DefList '((CodeApply0 ((pushj (reg st) (Indexed (reg 1) 0))))
	   (CodeApply1 ((pushj (reg st) (Indexed (reg 2) 0))))
	   (CodeApply2 ((pushj (reg st) (Indexed (reg 3) 0))))
	   (CodeApply3 ((pushj (reg st) (Indexed (reg 4) 0))))
	   (CodeApply4 ((pushj (reg st) (Indexed (reg 5) 0)))))
	 'OpenCode)

(DefList '((CodeApply0 ((jrst (Indexed (reg 1) 0))))
	   (CodeApply1 ((jrst (Indexed (reg 2) 0))))
	   (CodeApply2 ((jrst (Indexed (reg 3) 0))))
	   (CodeApply3 ((jrst (Indexed (reg 4) 0))))
	   (CodeApply4 ((jrst (Indexed (reg 5) 0)))))
	 'ExitOpenCode)

(DefList '((AddressApply0 ((pushj (reg st) (Indexed (reg 1) 0))))
	   (AddressApply1 ((pushj (reg st) (Indexed (reg 2) 0))))
	   (AddressApply2 ((pushj (reg st) (Indexed (reg 3) 0))))
	   (AddressApply3 ((pushj (reg st) (Indexed (reg 4) 0))))
	   (AddressApply4 ((pushj (reg st) (Indexed (reg 5) 0)))))
	 'OpenCode)

(DefList '((AddressApply0 ((jrst (Indexed (reg 1) 0))))
	   (AddressApply1 ((jrst (Indexed (reg 2) 0))))
	   (AddressApply2 ((jrst (Indexed (reg 3) 0))))
	   (AddressApply3 ((jrst (Indexed (reg 4) 0))))
	   (AddressApply4 ((jrst (Indexed (reg 5) 0)))))
	 'ExitOpenCode)

(* "*FEQ, *FGreaterP and !*FLessP can only occur once in a function.")

(DefList '((!*WFix ((fix (reg 1) (indexed (reg 1) 0))))
	   (!*WFloat ((fltr (reg 2) (reg 2))
		      (movem (reg 2) (indexed (reg 1) 0))
		      (setzm (indexed (reg 1) 1))))
	   (!*FAssign ((dmove (reg 2) (indexed (reg 2) 0))
		       (dmovem (reg 2) (indexed (reg 1) 0))))
	   (!*FEQ ((dmove (reg 3) (indexed (reg 2) 0))
		   (came (reg 3) (indexed (reg 1) 0))
		   (jrst !*NotEQ!*)
		   (camn (reg 4) (indexed (reg 1) 1))
		   !*NotEQ!*
		   (move (reg 1) (reg nil))))
	   (!*FGreaterP ((dmove (reg 3) (indexed (reg 2) 0))
			 (camge (reg 3) (indexed (reg 1) 0))
			 (jrst !*IsGreaterP!*)
			 (camn (reg 3) (indexed (reg 1) 0))
			 (caml (reg 4) (indexed (reg 1) 1))
			 (move (reg 1) (reg nil))
			 !*IsGreaterP!*))
	   (!*FLessP ((dmove (reg 3) (indexed (reg 2) 0))
		      (camle (reg 3) (indexed (reg 1) 0))
		      (jrst !*IsLessP!*)
		      (camn (reg 3) (indexed (reg 1) 0))
		      (camg (reg 4) (indexed (reg 1) 1))
		      (move (reg 1) (reg nil))
		      !*IsLessP!*))
	   (!*FPlus2 ((dmove (reg 3) (indexed (reg 3) 0))
		      (dfad (reg 3) (indexed (reg 2) 0))
		      (dmovem (reg 3) (indexed (reg 1) 0))))
	   (!*FDifference ((dmove (reg 4) (indexed (reg 2) 0))
			   (dfsb (reg 4) (indexed (reg 3) 0))
			   (dmovem (reg 4) (indexed (reg 1) 0))))
	   (!*FTimes2 ((dmove (reg 3) (indexed (reg 3) 0))
		       (dfmp (reg 3) (indexed (reg 2) 0))
		       (dmovem (reg 3) (indexed (reg 1) 0))))
	   (!*FQuotient ((dmove (reg 4) (indexed (reg 2) 0))
			 (dfdv (reg 4) (indexed (reg 3) 0))
			 (dmovem (reg 4) (indexed (reg 1) 0)))))
	 'OpenCode)

% Later, do as FORTRAN call?
(DE  !*ForeignLink (FunctionName  FunctionType NumberOfArguments)
  (prog NIL
    (CodeDeclareExternal FunctionName) % To emit Extern
    (return (LIST (LIST 'Pushj '(REG st) (LIST 'InternalEntry FunctionName))))
))

(DefCMacro !*ForeignLink)
