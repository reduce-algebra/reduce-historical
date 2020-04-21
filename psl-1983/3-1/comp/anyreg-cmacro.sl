(*
"% ANYREG-CMACRO.SL - Table-driven Anyreg and C-macro expander
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        17 December 1981
% Copyright (c) 1981 University of Utah
%")

(fluid '(ResultingCode!* TempLabel!* TempLabel2!*))

(* "Generated code is collected in reverse order in ResultingCode*")

(CompileTime (flag '(SafePair PatternSublA WConstEvaluabLis
		     AnyregPatternMatch1 MatchAll AnyregSubstitute1
		     TempLabelGen
		     CMacroSubstitute1)
	       'InternalFunction))

(dm DefAnyreg (Form)
  (prog (AnyregName FunctionName Pattern)
	(setq Form (cdr Form))
	(setq AnyregName (car Form))
	(setq Form (cdr Form))
	(setq FunctionName (car Form))
	(setq Pattern (cdr Form))
	(return (list 'progn
		      (list 'put
			    (MkQuote AnyregName)
			    '(quote AnyregResolutionFunction)
			    (MkQuote FunctionName))
		      (list 'put
			    (MkQuote AnyregName)
			    '(quote AnyregPatternTable)
			    (MkQuote Pattern))))))

(dm DefCMacro (Form)
  (prog (CMacroName Pattern)
	(setq Form (cdr Form))
	(setq CMacroName (car Form))
	(setq Pattern (cdr Form))
	(return (list 'progn
		      (list 'flag
			    (MkQuote (list CMacroName))
			    '(quote MC))
		      (list 'put
			    (MkQuote CMacroName)
			    '(quote CMacroPatternTable)
			    (MkQuote Pattern))))))

(de ResolveOperand (Register Source)
  (prog (ResolveAnyregFunction)
    (return (cond ((IDP Source) (ResolveWConst Source))
		  ((atom Source) Source)
		  ((FlagP (car Source) 'TerminalOperand) Source)
		  ((setq ResolveAnyregFunction
			 (get (car Source) 'AnyregResolutionFunction))
		   (Apply ResolveAnyregFunction
			  (cons Register (cdr Source))))
		  (t (ResolveWConst Source))))))

(de ResolveWConst (Expression)
  (prog (ResolvedExpression)
	(setq ResolvedExpression (ResolveWConstExpression Expression))
	(return (cond ((NumberP ResolvedExpression) ResolvedExpression)
		      (t (list 'Immediate Expression))))))

(de ResolveWConstExpression (Expression)
  (cond ((EqCar Expression 'WConst)
	 (ResolveWConstExpression (cadr Expression)))
    (t (prog (ResultExpression)
	 (return
	   (cond
	     ((or (NumberP Expression) (StringP Expression)) Expression)
	     ((IDP Expression)
	       (cond ((setq ResultExpression (get Expression 'WConst))
		       ResultExpression)
		 (t Expression)))
	     (t (progn
		  (cond
		    ((MacroP (car Expression))
		     (return
		       (ResolveWConstExpression (Apply (car Expression)
						       (list Expression))))))
		  (setq Expression
			(cons (car Expression)
			      (MapCar (cdr Expression)
				      (Function ResolveWConstExpression))))
		  (cond ((setq ResultExpression
			       (WConstEvaluable Expression))
			 ResultExpression)
			(t Expression))))))))))

(de WConstEvaluable (Expression)
  (prog (WC WCLis DoFn)
    (return
      (cond ((NumberP Expression) Expression)
	    ((and (IDP Expression) (setq WC (get Expression 'WConst)))
	     WC)
	    ((and (PairP Expression) (IDP (setq WC (car Expression))))
	     (cond ((MacroP WC)
		    (WConstEvaluable (apply (car Expression)
					    (list Expression))))
		   ((and (or (and (setq DoFn (get WC 'DoFn))
				  (setq WC DoFn))
			     (not (FUnBoundP WC)))
			 (not (eq (setq WCLis
					(WConstEvaluabLis (cdr
							   Expression)))
				  'not)))
		    (Eval (cons WC WCLis)))
		   (T NIL)))
	    (T NIL)))))

(de WConstEvaluabLis (ExpressionTail)
  (prog (WC WCLis)
    (return
      (cond ((null ExpressionTail) NIL)
	    ((not (setq WC (WConstEvaluable (car ExpressionTail)))) 'not)
	    ((eq (setq WCLis (WConstEvaluabLis (cdr ExpressionTail)))
		 'not)
	     'not)
	    (T (cons WC WCLis))))))
        
(de OneOperandAnyreg (Register Source AnyregName)
  (ExpandOneArgumentAnyreg Register
			   (ResolveOperand Register Source)
			   AnyregName))

(* "SecondArg must not require a register for evaluation.
It is currently used only for (MEMORY reg const).")

(de TwoOperandAnyreg (Register Source SecondArg AnyregName)
  (ExpandTwoArgumentAnyreg Register
			   (ResolveOperand Register Source)
			   (ResolveOperand '(REG Error) SecondArg)
			   AnyregName))

(de ExpandOneArgumentAnyreg (Register Source AnyregName)
  (AnyregPatternExpand (list Register Source)
		       (get AnyregName 'AnyregPatternTable)))

(de ExpandTwoArgumentAnyreg (Register Source SecondArg AnyregName)
  (AnyregPatternExpand (list Register Source SecondArg)
		       (get AnyregName 'AnyregPatternTable)))

(de ExpandThreeArgumentAnyreg (Register Source SecondArg ThirdArg AnyregName)
  (AnyregPatternExpand (list Register Source SecondArg ThirdArg)
		       (get AnyregName 'AnyregPatternTable)))

(de AnyregPatternExpand (ArgumentList PatternTable)
  (AnyregSubstitute ArgumentList
		    (AnyregPatternMatch (cdr ArgumentList) PatternTable)))

(* "The label operand must not require a register to resolve.")

(de Expand2OperandAndLabelCMacro (Arg1 Arg2 Label CMacroName)
  (prog (ResultingCode!*)
    (return (CMacroPatternExpand (list (ResolveOperand '(REG t1) Arg1)
				       (ResolveOperand '(REG t2) Arg2)
				       (ResolveOperand '(REG Error) Label))
				 (get CMacroName 'CMacroPatternTable)))))

(de Expand4OperandCMacro (Arg1 Arg2 Arg3 Arg4 CMacroName)
  (prog (ResultingCode!*)
    (return (CMacroPatternExpand (list (ResolveOperand '(REG t1) Arg1)
				       (ResolveOperand '(REG t2) Arg2)
				       (ResolveOperand '(REG Error) Arg3)
				       (ResolveOperand '(REG Error) Arg4))
				 (get CMacroName 'CMacroPatternTable)))))

(de Expand2OperandCMacro (Arg1 Arg2 CMacroName)
  (prog (ResultingCode!*)
    (return (CMacroPatternExpand (list (ResolveOperand '(REG t1) Arg1)
				       (ResolveOperand '(REG t2) Arg2))
				 (get CMacroName 'CMacroPatternTable)))))

(de Expand1OperandCMacro (Arg1 CMacroName)
  (prog (ResultingCode!*)
    (return (CMacroPatternExpand (list (ResolveOperand '(REG t1) Arg1))
				 (get CMacroName 'CMacroPatternTable)))))

(de CMacroPatternExpand (ArgumentList PatternTable)
  (CMacroSubstitute ArgumentList
		    (AnyregPatternMatch ArgumentList PatternTable)))

(de AnyregPatternMatch (ArgumentList PatternTable)
  (cond ((null (cdr PatternTable)) (car PatternTable))
	((AnyregPatternMatch1 ArgumentList (caar PatternTable))
	 (cdar PatternTable))
	(t (AnyregPatternMatch ArgumentList (cdr PatternTable)))))

(de AnyregPatternMatch1 (ArgumentList PredicateOrPredicateList)
  (cond ((atom PredicateOrPredicateList)
	 (Apply PredicateOrPredicateList ArgumentList))
	(t (MatchAll ArgumentList PredicateOrPredicateList))))

(de MatchAll (ArgumentList PredicateList)
  (or (atom ArgumentList)
      (atom PredicateList)
      (and (Apply (car PredicateList) (list (car ArgumentList)))
	   (MatchAll (cdr ArgumentList) (cdr PredicateList)))))

(de AnyregSubstitute (ArgumentList CodeAndAddressExpressionList)
  (AnyregSubstitute1 (SafePair '(Register Source ArgTwo ArgThree)
			       ArgumentList)
		     CodeAndAddressExpressionList))

(de AnyregSubstitute1 (NameExpressionAList CodeAndAddressExpressionList)
  (cond ((null (cdr CodeAndAddressExpressionList))
	 (SublA NameExpressionAList (car CodeAndAddressExpressionList)))
	(t (progn (setq ResultingCode!*
			(cons (SublA NameExpressionAList
				     (car CodeAndAddressExpressionList))
			      ResultingCode!*))
		  (AnyregSubstitute1 NameExpressionAList
				     (cdr CodeAndAddressExpressionList))))))

(de CMacroSubstitute (ArgumentList CodeTemplateList)
  (prog (TempLabel!* TempLabel2!*)
	(return (CMacroSubstitute1 (SafePair '(ArgOne ArgTwo
						      ArgThree
						      ArgFour
						      ArgFive)
					     ArgumentList)
				   CodeTemplateList))))

(de CMacroSubstitute1 (NameExpressionAList CodeTemplateList)
  (cond ((null CodeTemplateList) (ReversIP ResultingCode!*))
	(t (progn (setq ResultingCode!*
			(cons (PatternSublA NameExpressionAList
					    (car CodeTemplateList))
			      ResultingCode!*))
		  (CMacroSubstitute1 NameExpressionAList
				     (cdr CodeTemplateList))))))

(de SafePair (CarList CdrList)
  (cond ((and (PairP CarList) (PairP CdrList))
	 (cons (cons (car CarList) (car CdrList))
	       (SafePair (cdr CarList) (cdr CdrList))))
	(t NIL)))

(de PatternSublA (AList Expression)
  (prog (X)
	(return (cond ((null Expression) Expression)
		      ((atom Expression)
		       (cond ((eq Expression 'TempLabel)
			      (TempLabelGen 'TempLabel!*))
			     ((eq Expression 'TempLabel2)
			      (TempLabelGen 'TempLabel2!*))
			     ((setq X (atsoc Expression AList))
			      (cdr X))
			     (t Expression)))
		      (t (cons (PatternSublA AList (car Expression))
			       (PatternSublA AList (cdr Expression))))))))

(de TempLabelGen (X)
  ((lambda (Y)
     (cond ((StringP Y) Y)
	   (T (set X (StringGensym)))))
   (Eval X)))
