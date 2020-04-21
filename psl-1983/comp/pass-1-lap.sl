(*
"% PASS-1-LAP.SL - Expand c-macros and allocate quoted expressions
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        14 December 1981
% Copyright (c) 1981 University of Utah
%
% Added MCprint and InstructionPrint - MLG

% <PSL.COMP>PASS-1-LAP.SL.17,  4-Aug-82 00:35:54, Edit by BENSON
% Added bignum constants; won't work for cross-compilation, though

%")

(*
"Pass1Lap takes a list of c-macros and instructions, and attempts to simplify
them whenever possible.  C-macros are expanded by APPLY(CAR X, CDR X), which
will return another instruction list to be processed recursively by Pass1Lap.
Quoted expressions are allocated at the end of the code, in the following way:

In an instruction or c-macro
(.... (QUOTE (A B C)) ...)

the following is tacked onto the end of the constructed code list:

L2
(MKITEM ID A)
(MKITEM PAIR L3)
L3
(MKITEM ID B)
(MKITEM PAIR L4)
L4
(MKITEM ID C)
(MKITEM ID NIL)

If *ImmediateQuote is NIL, the quoted reference becomes:

(... L1 ...)
...
L1
(fullword (MKITEM PAIR L2))

Otherwise, it becomes:

(... (immediate (MKITEM PAIR L2)) ...)")

(fluid '(!*ImmediateQuote
	 !*PCMAC
	 !*PrintedOneCMacro
	 Pass1CodeList
	 Pass1ConstantList
	 Pass1ConstantContentsList
	 Pass1AddedCode
	 EntryPoints!*
	 AddressingUnitsPerItem
	 LastActualReg!&))

(CompileTime (flag '(Pass1Code OneLapPass1 AddInstruction
		     ExpandPseudoOps ExpandOnePseudoOp
		     GenerateLabel GenerateCodeLabel AddCodeLabel AddCode
		     ExpandQuote1 ExpandImmediateQuote ExpandItem
		     ExpandNonImmediateQuote SaveConstant SaveContents
		     AppendConstants AppendOneConstant AppendItem
		     AddFullWord AppendContents MakeMkItem)
	       'InternalFunction))

(CompileTime (load fast-vector))

(de Pass1Lap (InstructionList)
  (prog (Pass1CodeList
	 Pass1ConstantList
	 Pass1ConstantContentsList
	 EntryPoints!*
	 Pass1AddedCode)
    (setq Pass1CodeList (cons NIL NIL))	(* "Init a TCONC pointer")
    (setq Pass1ConstantContentsList (cons NIL NIL))
    (Pass1Code InstructionList)         (* "Expand macros")
    (Pass1Code Pass1AddedCode)
    (AppendConstants)			(* "Tack the constants on the end")
    (return (car Pass1CodeList))))

(* "BuildConstant takes an S-expression and returns the LAP version of it.")

(* "The car is the expanded item, cdr is the contents")

(de BuildConstant (Expression)
  (prog (Pass1CodeList
	 Pass1ConstantList
	 Pass1ConstantContentsList
	 ExpandedExpression)
    (setq Pass1CodeList (cons NIL NIL))	(* "Init a TCONC pointer")
    (setq Pass1ConstantContentsList (cons NIL NIL))
    (setq ExpandedExpression (ExpandItem Expression)) (* "Expand the item")
    (AppendConstants)			(* "Tack the contents on the end")
    (return (cons ExpandedExpression (car Pass1CodeList)))))

(de Pass1Code (InstructionList)
    (ForEach Instruction in InstructionList do (OneLapPass1 Instruction)))

(de OneLapPass1 (Instruction)
  (cond ((atom Instruction) (AddCodeLabel Instruction))
	((eq (car Instruction) '!*ENTRY)
	 (progn (* "ENTRY directives are passed unchanged")
	        (cond ((and (not (or (FlagP (second Instruction)
					    'InternalFunction)
				     (equal (second Instruction)
					    '**fasl**initcode**)))
			    (null (car Pass1CodeList)))
		       (* "Header word says how many arguments to expect")
		       (AddCode (list 'FULLWORD (fourth Instruction)))))
		(setq EntryPoints!*
		      (cons (second Instruction) EntryPoints!*))
		(cond (!*PCMAC (MCPrint Instruction)))
		(AddCode Instruction)))
	((FlagP (car Instruction) 'MC)
	 (progn (cond ((and !*PCMAC (not !*PrintedOneCMacro))
		       (MCPrint Instruction)))
		((lambda (!*PrintedOneCMacro)
			 (Pass1Code (Apply (car Instruction)
					   (cdr Instruction))))
		 T)))
	(t (progn (cond (!*PCMAC (InstructionPrint Instruction)))
		  (AddInstruction Instruction)))))

(de MCPrint(x) (print x))
(de InstructionPrint(x) (PrintF "	%p%n" x))

(de AddInstruction (Instruction)
  (AddCode (ExpandPseudoOps Instruction)))

(de ExpandPseudoOps (X)
  (cond ((atom X) X)
	(t (cons (ExpandOnePseudoOp (car X))
		 (ExpandPseudoOps (cdr X))))))

(de ExpandOnePseudoOp (X)
  (prog (PseudoOpFunction)
	(return (cond ((atom X) X)
		      ((setq PseudoOpFunction
			     (get (car X) 'Pass1PseudoOp))
		       (ExpandOnePseudoOp (Apply PseudoOpFunction
						 (list X))))
		      ((setq PseudoOpFunction (WConstEvaluable X))
		       PseudoOpFunction)
		      (t (cons (car X) (ExpandPseudoOps (cdr X))))))))


(de PassOneUnImmediate (X)
  (progn (setq X (cadr X))
	 (cond ((EqCar X 'Immediate) (cadr X))
	   (t X))))

(put 'UnImmediate 'Pass1PseudoOp 'PassOneUnImmediate)

(de PassOneLabel (U)
  (cadr U))

(put 'Label 'Pass1PseudoOp 'PassOneLabel)

(de PassOneUnDeferred (X)
  (progn (setq X (cadr X))
	 (cond ((EqCar X 'Deferred) (cadr X))
	   (t X))))

(put 'UnDeferred 'Pass1PseudoOp 'PassOneUnDeferred)

(* "Removed because ExtraReg has to be processed differently by resident LAP"
(de PassOneExtraReg (X)
  (progn (setq X (cadr X))
	 (list 'plus2
	       '(WArray ArgumentBlock)
	       (times (difference (Add1 LastActualReg!&) X)
		      AddressingUnitsPerItem))))

(put 'ExtraReg 'Pass1PseudoOp 'PassOneExtraReg)
)

(de GenerateCodeLabel ()
  (prog (NewLabel)
	(setq NewLabel (GenerateLabel))
	(AddCodeLabel NewLabel)
	(return NewLabel)))

(de GenerateLabel ()
  (StringGenSym))

(de AddCodeLabel (Label)
  (AddCode Label))

(de AddCode (C)
  (TConc Pass1CodeList C))

(de ExpandLit (U)
  (prog (L)
    (cond ((setq L (FindPreviousLit (cdr U))) (return L)))
    (setq L (GenerateLabel))
    (setq Pass1AddedCode (NConc Pass1AddedCode
			   (cons L (ForEach X in (cdr U) collect X))))
    (return L)))

(de FindPreviousLit (U)
  (cond ((not (null (rest U))) NIL)
    (t (prog (L)
	 (setq L Pass1AddedCode)
	 (cond ((null L) (return NIL)))
	 (setq U (first U))
        loop
	 (cond ((null (rest L)) (return NIL)))
	 (cond ((equal U (second L))
		(return (cond ((atom (first L)) (first L))
			  (t (prog (B)
			       (setq L (rest L))
			       (rplacd L (cons (first L) (rest L)))
			       (rplaca L (setq B (GenerateLabel)))
			       (return B)))))))
	 (setq L (rest L))
	 (go loop)))))

(put 'lit 'Pass1PseudoOp 'ExpandLit)
(flag '(lit) 'TerminalOperand)

(de ExpandQuote (QuotedExpression)
  (ExpandQuote1 (cadr QuotedExpression)))

(put 'Quote 'Pass1PseudoOp 'ExpandQuote)

(de ExpandQuote1 (Expression)
  (cond (!*ImmediateQuote (ExpandImmediateQuote Expression))
        (t (ExpandNonImmediateQuote Expression))))

(de ExpandImmediateQuote (Expression)
  (list 'IMMEDIATE (ExpandItem Expression)))

(de ExpandItem (Expression)
  (prog (LabelOfContents)
	(return (cond ((InumP Expression) Expression)
		      ((IDP Expression)
		       (MakeMkItem (TagNumber Expression)
				   (list 'IDLoc Expression)))
		      ((CodeP Expression)
		       (MakeMkItem (TagNumber Expression)
			           Expression))
		      (t (progn (setq LabelOfContents
				      (SaveContents Expression))
				(MakeMkItem (TagNumber Expression)
					    LabelOfContents)))))))

(de ExpandNonImmediateQuote (Expression)
  (SaveConstant Expression))

(de SaveConstant (Expression)
  (prog (TableEntry)
	(return (cond ((setq TableEntry
			     (Assoc Expression Pass1ConstantList))
		       (cdr TableEntry))
		      (t (progn (setq TableEntry (GenerateLabel))
				(setq Pass1ConstantList
				      (cons (cons Expression
						  TableEntry)
					    Pass1ConstantList))
				TableEntry))))))


(de SaveContents (Expression)
  (prog (TableEntry)
	(return (cond ((setq TableEntry
			     (Assoc Expression
				    (car Pass1ConstantContentsList)))
		       (cdr TableEntry))
		      (t (progn (setq TableEntry (GenerateLabel))
				(TConc Pass1ConstantContentsList
				       (cons Expression TableEntry))
				TableEntry))))))


(de AppendConstants ()
  (prog (TempCodeList)
	(cond ((not !*ImmediateQuote)
	       (ForEach TableEntry in Pass1ConstantList do
			(AppendOneConstant TableEntry))))
	(setq TempCodeList Pass1CodeList)
	(setq Pass1CodeList (cons NIL NIL))
	(ForEach TableEntry in (car Pass1ConstantContentsList) do
		 (AppendContents TableEntry))
	(* "The contents go on the begininning of the list")
	(LConc Pass1CodeList (car TempCodeList))))

(de AppendOneConstant (ExpressionLabelPair)
  (progn (AddCodeLabel (cdr ExpressionLabelPair))
         (AppendItem (car ExpressionLabelPair))))

(de AppendItem (Expression)
  (AddFullWord (ExpandItem Expression)))

(de AddFullWord (Expression)
  (AddCode (list 'FULLWORD Expression)))

(de AppendContents (ExpressionLabelPair)
  (prog (Expression UpperBound I)
	(AddCodeLabel (cdr ExpressionLabelPair))
	(setq Expression (car ExpressionLabelPair))
	(cond ((PairP Expression)
	       (progn (AppendItem (car Expression))
		      (AppendItem (cdr Expression))))
	      ((StringP Expression)
	       (progn (AddFullWord (Size Expression))
		      (AddCode (list 'STRING Expression))))
	      ((VectorP Expression)
	       (progn (setq UpperBound (ISizeV Expression))
		      (AddFullWord UpperBound)
		      (setq I 0)
		      (while (ILEQ I UpperBound)
			     (progn (AppendItem (IGetV Expression I))
				    (setq I (IAdd1 I))))))
	      ((BigP Expression)
	       (progn (setq UpperBound (ISizeV Expression))
		      (AddFullWord UpperBound)
		      (setq I 0)
		      (while (ILEQ I UpperBound)
			     (progn (AppendItem (IGetV Expression I))
				    (setq I (IAdd1 I))))))
	      ((FixP Expression)
	       (progn (AddFullWord 0)	(* "Header of full word fixnum")
		      (AddFullWord Expression)))
	      ((FloatP Expression)
	       (progn (AddFullWord 1)	(* "Header of float")
		      (AddCode (list 'FLOAT Expression)))))))

(de MakeMkItem (TagPart InfPart)
  (list 'MKITEM TagPart InfPart))

(de InumP (N) (IntP N))	       (* "Must be changed for cross-compilation")

(de TagNumber (Expression)
  (MkINT (Tag Expression)))	(* "Must be redefined for cross-compilation")
