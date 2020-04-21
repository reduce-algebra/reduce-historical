%
% ARITHMETIC.RED - Arithmetic routines for PSL with new integer tags
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        17 January 1982
% Copyright (c) 1982 University of Utah
%

on SysLisp;

syslsp procedure IsInum U;
    SignedField(U, InfStartingBit - 1, InfBitLength + 1) eq U;

CompileTime <<
internal WConst IntFunctionEntry = 0,
		BigFunctionEntry = 1,
		FloatFunctionEntry = 2,
		FunctionNameEntry = 3;

>>;

syslsp procedure TwoArgDispatch(FirstArg, SecondArg);
    TwoArgDispatch1(FirstArg, SecondArg, Tag FirstArg, Tag SecondArg);

lap '((!*entry TwoArgDispatch1 expr 4)
	(!*JUMPNOTEQ (Label NotNeg1) (reg 3) (WConst NegInt))
	(!*MOVE (WConst PosInt) (reg 3))
NotNeg1
	(!*JUMPNOTEQ (Label NotNeg2) (reg 4) (WConst NegInt))
	(!*MOVE (WConst PosInt) (reg 4))
NotNeg2
	(!*JUMPWGREATERP (Label NonNumeric) (reg 3) (WConst FltN))
	(!*JUMPWGREATERP (Label NonNumeric) (reg 4) (WConst FltN))
	(!*WSHIFT (reg 3) (WConst 2))
	(!*WPLUS2 (reg 4) (reg 3))
	(!*POP (reg 3))
	(!*JUMPON (reg 4) 0 15 ((Label IntInt)
				(Label IntFix)
				(Label IntBig)
				(Label IntFloat)
				(Label FixInt)
				(Label FixFix)
				(Label FixBig)
				(Label FixFloat)
				(Label BigInt)
				(Label BigFix)
				(Label BigBig)
				(Label BigFloat)
				(Label FloatInt)
				(Label FloatFix)
				(Label FloatBig)
				(Label FloatFloat)))
	(!*JCALL TwoArgError)
FixBig
	(!*FIELD (reg 1) (reg 1)	% grab the value for the fixnum
		 (WConst InfStartingBit) (WConst InfBitLength))
	(!*MOVE (MEMORY (reg 1) (WConst AddressingUnitsPerItem)) (reg 1))
IntBig
	(!*PUSH (reg 3))
	(!*PUSH (reg 2))
	(!*CALL StaticIntBig)
	(!*POP (reg 2))
	(!*POP (reg 3))
BigBig
	(!*MOVE (MEMORY (reg 3)
			(WConst (times2 (WConst AddressingUnitsPerItem)
					(WConst BigFunctionEntry))))
		(reg t1))
	(!*JCALL FastApply)
BigFix
	(!*FIELD (reg 2) (reg 2)	% grab the value for the fixnum
		 (WConst InfStartingBit) (WConst InfBitLength))
	(!*MOVE (MEMORY (reg 2) (WConst AddressingUnitsPerItem)) (reg 2))
BigInt
	(!*PUSH (reg 3))
	(!*PUSH (reg 1))
	(!*MOVE (reg 2) (reg 1))
	(!*CALL StaticIntBig)
	(!*MOVE (reg 1) (reg 2))
	(!*POP (reg 1))
	(!*POP (reg 3))
	(!*MOVE (MEMORY (reg 3)
			(WConst (times2 (WConst AddressingUnitsPerItem)
					(WConst BigFunctionEntry))))
		(reg t1))
	(!*JCALL FastApply)
FixInt
	(!*FIELD (reg 1) (reg 1)	% grab the value for the fixnum
		 (WConst InfStartingBit) (WConst InfBitLength))
	(!*MOVE (MEMORY (reg 1) (WConst AddressingUnitsPerItem)) (reg 1))
	(!*MOVE (MEMORY (reg 3) (WConst 0)) (reg t1))
	(!*JCALL FastApply)
FixFix
	(!*FIELD (reg 1) (reg 1)	% grab the value for the fixnum
		 (WConst InfStartingBit) (WConst InfBitLength))
	(!*MOVE (MEMORY (reg 1) (WConst AddressingUnitsPerItem)) (reg 1))
IntFix
	(!*FIELD (reg 2) (reg 2)
		 (WConst InfStartingBit) (WConst InfBitLength))
	(!*MOVE (MEMORY (reg 2) (WConst AddressingUnitsPerItem)) (reg 2))
IntInt
	(!*MOVE (MEMORY (reg 3) (WConst 0)) (reg t1))
	(!*JCALL FastApply)
FixFloat
	(!*FIELD (reg 1) (reg 1)
		 (WConst InfStartingBit) (WConst InfBitLength))
	(!*MOVE (MEMORY (reg 1) (WConst AddressingUnitsPerItem)) (reg 1))
IntFloat
	(!*PUSH (reg 3))
	(!*PUSH (reg 2))
	(!*CALL StaticIntFloat)
	(!*POP (reg 2))
	(!*POP (reg 3))
	(!*MOVE (MEMORY (reg 3)
			(WConst (times2 (WConst AddressingUnitsPerItem)
					(WConst FloatFunctionEntry))))
		(reg t1))
	(!*JCALL FastApply)
FloatFix
	(!*FIELD (reg 2) (reg 2)
		 (WConst InfStartingBit) (WConst InfBitLength))
	(!*MOVE (MEMORY (reg 2) (WConst AddressingUnitsPerItem)) (reg 2))
FloatInt
	(!*PUSH (reg 3))
	(!*PUSH (reg 1))
	(!*MOVE (reg 2) (reg 1))
	(!*CALL StaticIntFloat)
	(!*MOVE (reg 1) (reg 2))
	(!*POP (reg 1))
	(!*POP (reg 3))
	(!*MOVE (MEMORY (reg 3)
			(WConst (times2 (WConst AddressingUnitsPerItem)
					(WConst FloatFunctionEntry))))
		(reg t1))
	(!*JCALL FastApply)
FloatFloat
	(!*MOVE (MEMORY (reg 3)
			(WConst (times2 (WConst AddressingUnitsPerItem)
					(WConst FloatFunctionEntry))))
		(reg t1))
	(!*JCALL FastApply)
BigFloat
	(!*PUSH (reg 3))
	(!*PUSH (reg 2))
	(!*CALL StaticBigFloat)
	(!*POP (reg 2))
	(!*POP (reg 3))
	(!*MOVE (MEMORY (reg 3)
			(WConst (times2 (WConst AddressingUnitsPerItem)
					(WConst FloatFunctionEntry))))
		(reg t1))
	(!*JCALL FastApply)
FloatBig
	(!*PUSH (reg 3))
	(!*PUSH (reg 1))
	(!*MOVE (reg 2) (reg 1))
	(!*CALL StaticBigFloat)
	(!*MOVE (reg 1) (reg 2))
	(!*POP (reg 1))
	(!*POP (reg 3))
	(!*MOVE (MEMORY (reg 3)
			(WConst (times2 (WConst AddressingUnitsPerItem)
					(WConst FloatFunctionEntry))))
		(reg t1))
	(!*JCALL FastApply)
NonNumeric
	(!*POP (reg 3))
	(!*JCALL TwoArgError)
);

syslsp procedure TwoArgError(FirstArg, SecondArg, DispatchTable);
    ContinuableError('99,
		     '"Non-numeric argument in arithmetic",
		     list(DispatchTable[FunctionNameEntry],
			  FirstArg,
			  SecondArg));

syslsp procedure NonInteger2Error(FirstArg, SecondArg, DispatchTable);
    ContinuableError('99,
		     '"Non-integer argument in arithmetic",
		     list(DispatchTable[FunctionNameEntry],
			  FirstArg,
			  SecondArg));

syslsp procedure NonInteger1Error(Arg, DispatchTable);
    ContinuableError('99,
		     '"Non-integer argument in arithmetic",
		     list(DispatchTable[FunctionNameEntry],
			  Arg));

syslsp procedure OneArgDispatch FirstArg;
    OneArgDispatch1(FirstArg, Tag FirstArg);

lap '((!*entry OneArgDispatch1 expr 2)
	(!*JUMPNOTEQ (Label NotNeg1) (reg 2) (WConst NegInt))
	(!*MOVE (WConst PosInt) (reg 2))
NotNeg1
	(!*POP (reg 3))
	(!*JUMPON (reg 2) 0 3 ((Label OneInt)
			       (Label OneFix)
			       (Label OneBig)
			       (Label OneFloat)))
	(!*JCALL OneArgError)
OneBig
	(!*MOVE (MEMORY (reg 3)
			(WConst (times2 (WConst AddressingUnitsPerItem)
					(WConst BigFunctionEntry))))
		(reg t1))
	(!*JCALL FastApply)
OneFix
	(!*FIELD (reg 1) (reg 1)
		 (WConst InfStartingBit) (WConst InfBitLength))
	(!*MOVE (MEMORY (reg 1) (WConst AddressingUnitsPerItem)) (reg 1))
OneInt
	(!*MOVE (MEMORY (reg 3) (WConst 0)) (reg t1))
	(!*JCALL FastApply)
OneFloat
	(!*MOVE (MEMORY (reg 3)
			(WConst (times2 (WConst AddressingUnitsPerItem)
					(WConst FloatFunctionEntry))))
		(reg t1))
	(!*JCALL FastApply)
);

syslsp procedure OneArgError(FirstArg, Dummy, DispatchTable);
    ContinuableError('99,
		     '"Non-numeric argument in arithmetic",
		     list(DispatchTable[FunctionNameEntry],
			  FirstArg));

syslsp procedure OneArgPredicateDispatch FirstArg;
    OneArgPredicateDispatch1(FirstArg, Tag FirstArg);

lap '((!*entry OneArgPredicateDispatch1 expr 2)
	(!*JUMPNOTEQ (Label NotNeg1) (reg 2) (WConst NegInt))
	(!*MOVE (WConst PosInt) (reg 2))
NotNeg1
	(!*POP (reg 3))
	(!*JUMPON (reg 2) 0 3 ((Label OneInt)
			       (Label OneFix)
			       (Label OneBig)
			       (Label OneFloat)))
	(!*MOVE (QUOTE NIL) (reg 1))
	(!*EXIT 0)
OneBig
	(!*MOVE (MEMORY (reg 3)
			(WConst (times2 (WConst AddressingUnitsPerItem)
					(WConst BigFunctionEntry))))
		(reg t1))
	(!*JCALL FastApply)
OneFix
	(!*FIELD (reg 1) (reg 1)
		 (WConst InfStartingBit) (WConst InfBitLength))
	(!*MOVE (MEMORY (reg 1) (WConst AddressingUnitsPerItem)) (reg 1))
OneInt
	(!*MOVE (MEMORY (reg 3) (WConst 0)) (reg t1))
	(!*JCALL FastApply)
OneFloat
	(!*MOVE (MEMORY (reg 3)
			(WConst (times2 (WConst AddressingUnitsPerItem)
					(WConst FloatFunctionEntry))))
		(reg t1))
	(!*JCALL FastApply)
);

syslsp procedure MakeFixnum N;
begin scalar F;
    F := GtFIXN();
    FixVal F := N;
    return MkFIXN F;
end;

syslsp procedure BigFloatFix N;
    StdError List('"Bignums not yet supported [BigFloatFix]",N);

syslsp procedure ReturnNIL();
    NIL;

syslsp procedure ReturnFirstArg Arg;
    Arg;

%internal WArray StaticFloatBuffer = [1, 0, 0];
%
%internal WConst StaticFloatItem = MkItem(FLTN, StaticFloatBuffer);
%
syslsp procedure StaticIntFloat Arg;
%<<  !*WFloat(&StaticFloatBuffer[1], Arg);
%    StaticFloatItem >>;
FloatIntArg Arg;

syslsp procedure StaticIntBig Arg;
   StdError LIST('"Bignums not yet supported [StaticIntBig]",Arg);

syslsp procedure StaticBigFloat Arg;
   StdError LIST('"Bignums not yet supported [StaticBigFloat]",Arg);

off SysLisp;

CompileTime <<
macro procedure DefArith2Entry U;
    DefArithEntry(2 . 'TwoArgDispatch . StupidParserFix cdr U);

macro procedure DefArith1Entry U;
    DefArithEntry(1 . 'OneArgDispatch . StupidParserFix cdr U);

macro procedure DefArith1PredicateEntry U;
    DefArithEntry(1 . 'OneArgPredicateDispatch . StupidParserFix cdr U);

lisp procedure StupidParserFix X;
% Goddamn Rlisp parser won't let me just give "Difference" as the parameter
% to a macro
    if null X then X
    else RemQuote car X . StupidParserFix cdr X;

lisp procedure RemQuote X;
    if EqCar(X, 'QUOTE) then cadr X else X;

lisp procedure DefArithEntry L;
    SublA(Pair('(NumberOfArguments
		 DispatchRoutine
		 NameOfFunction
		 IntFunction
		 BigFunction
		 FloatFunction),
		L),
	  quote(lap '((!*entry NameOfFunction expr NumberOfArguments)
		      (!*Call DispatchRoutine)	% 30 is ID, won't do for 68000
		      (fullword (MkItem 30 (IDLoc IntFunction)))
		      (fullword (MkItem 30 (IDLoc BigFunction)))
		      (fullword (MkItem 30 (IDLoc FloatFunction)))
		      (fullword (MkItem 30
					(IDLoc NameOfFunction))))));
>>;

DefArith2Entry(Plus2, IntPlus2, BigPlus2, FloatPlus2);

syslsp procedure IntPlus2(FirstArg, SecondArg);
    if IsInum(FirstArg := WPlus2(FirstArg, SecondArg)) then
	FirstArg
    else
	MakeFixnum FirstArg;

syslsp procedure FloatPlus2(FirstArg, SecondArg);
begin scalar F;
    F := GtFLTN();
    !*FPlus2(FloatBase F, FloatBase FltInf FirstArg,
			  FloatBase FltInf SecondArg);

    return MkFLTN F;
end;

DefArith2Entry('Difference, IntDifference, BigDifference, FloatDifference);

syslsp procedure IntDifference(FirstArg, SecondArg);
    if IsInum(FirstArg := WDifference(FirstArg, SecondArg)) then
	FirstArg
    else
	MakeFixnum FirstArg;

syslsp procedure FloatDifference(FirstArg, SecondArg);
begin scalar F;
    F := GtFLTN();
    !*FDifference(FloatBase F, FloatBase FltInf FirstArg,
			       FloatBase FltInf SecondArg);

    return MkFLTN F;
end;

DefArith2Entry(Times2, IntTimes2, BigTimes2, FloatTimes2);

% What about overflow?

syslsp procedure IntTimes2(FirstArg, SecondArg);
begin scalar Result;
    Result := WTimes2(FirstArg, SecondArg);
    return if not IsInum Result then MakeFixnum Result else Result;
end;

syslsp procedure FloatTimes2(FirstArg, SecondArg);
begin scalar F;
    F := GtFLTN();
    !*FTimes2(FloatBase F, FloatBase FltInf FirstArg,
			       FloatBase FltInf SecondArg);

    return MkFLTN F;
end;

DefArith2Entry('Divide, IntDivide, BigDivide, FloatDivide);
DefArith2Entry('Quotient, IntQuotient, BigQuotient, FloatQuotient);

syslsp procedure IntDivide(FirstArg, SecondArg);
 IntQuotient(FirstArg, SecondArg) . IntRemainder(FirstArg, SecondArg);

syslsp procedure FloatDivide(FirstArg, SecondArg);
 FloatQuotient(FirstArg, SecondArg) . FloatRemainder(FirstArg, SecondArg);

syslsp procedure IntQuotient(FirstArg, SecondArg);
begin scalar Result;
    if SecondArg eq 0 then return
	ContError(99,
		  "Attempt to divide by zero in Quotient",
		  Quotient(FirstArg, SecondArg));
    Result := WQuotient(FirstArg, SecondArg);
    return if not IsInum Result then MakeFixnum Result else Result;
end;

syslsp procedure FloatQuotient(FirstArg, SecondArg);
begin scalar F;
    if FloatZeroP SecondArg then return
	ContError(99,
		  "Attempt to divide by zero in Quotient",
		  Quotient(FirstArg, SecondArg));
    F := GtFLTN();
    !*FQuotient(FloatBase F, FloatBase FltInf FirstArg,
			       FloatBase FltInf SecondArg);

    return MkFLTN F;
end;

DefArith2Entry(Remainder, IntRemainder, BigRemainder, FloatRemainder);

syslsp procedure IntRemainder(FirstArg, SecondArg);
begin scalar Result;
    if SecondArg eq 0 then return
	ContError(99,
		  "Attempt to divide by zero in Remainder",
		  Remainder(FirstArg, SecondArg));
    Result := WRemainder(FirstArg, SecondArg);
    return if not IsInum Result then MakeFixnum Result else Result;
end;

syslsp procedure FloatRemainder(FirstArg, SecondArg);
begin scalar F;
    F := GtFLTN();
    !*FRemainder(FloatBase F, FloatBase FltInf FirstArg,
			       FloatBase FltInf SecondArg);

    return MkFLTN F;
end;

DefArith2Entry(LAnd, IntLAnd, BigLAnd, NonInteger2Error);

syslsp procedure IntLAnd(FirstArg, SecondArg);
    if IsInum(FirstArg := WAnd(FirstArg, SecondArg)) then
	FirstArg
    else MakeFixnum FirstArg;

DefArith2Entry(LOr, IntLOr, BigLOr, NonInteger2Error);

syslsp procedure IntLOr(FirstArg, SecondArg);
    if IsInum(FirstArg := WOr(FirstArg, SecondArg)) then
	FirstArg
    else MakeFixnum FirstArg;

DefArith2Entry(LXOr, IntLXOr, BigLXOr, NonInteger2Error);

syslsp procedure IntLXOr(FirstArg, SecondArg);
    if IsInum(FirstArg := WXOr(FirstArg, SecondArg)) then
	FirstArg
    else MakeFixnum FirstArg;

DefArith2Entry(LShift, IntLShift, BigLShift, NonInteger2Error);

PutD('LSH, 'EXPR, cdr GetD 'LShift);

procedure IntLShift(FirstArg, SecondArg);
    BigLShift(Int2B FirstArg, Int2B SecondArg);

DefArith2Entry('GreaterP, IntGreaterP, BigGreaterP, FloatGreaterP);

syslsp procedure IntGreaterP(FirstArg, SecondArg);
    WGreaterP(FirstArg, SecondArg);

syslsp procedure FloatGreaterP(FirstArg, SecondArg);
    !*FGreaterP(FloatBase FltInf FirstArg,
		FloatBase FltInf SecondArg) and T;

DefArith2Entry('LessP, IntLessP, BigLessP, FloatLessP);

syslsp procedure IntLessP(FirstArg, SecondArg);
    WLessP(FirstArg, SecondArg);

syslsp procedure FloatLessP(FirstArg, SecondArg);
    !*FLessP(FloatBase FltInf FirstArg,
	     FloatBase FltInf SecondArg) and T;

DefArith1Entry(Add1, IntAdd1, BigAdd1, FloatAdd1);

syslsp procedure IntAdd1 FirstArg;
    if IsInum(FirstArg := WPlus2(FirstArg, 1)) then
	FirstArg
    else
	MakeFixnum FirstArg;

lisp procedure FloatAdd1 FirstArg;
    FloatPlus2(FirstArg, 1.0);

DefArith1Entry(Sub1, IntSub1, BigSub1, FloatSub1);

lisp procedure IntSub1 FirstArg;
    if IsInum(FirstArg := WDifference(FirstArg, 1)) then
	FirstArg
    else
	MakeFixnum FirstArg;

lisp procedure FloatSub1 FirstArg;
    FloatDifference(FirstArg, 1.0);

DefArith1Entry(LNot, IntLNot, BigLNot, NonInteger1Error);

lisp procedure IntLNot X;
    if IsInum(X := WNot X) then X else MakeFixnum X;

DefArith1Entry('Minus, IntMinus, BigMinus, FloatMinus);

lisp procedure IntMinus FirstArg;
    if IsInum(FirstArg := WMinus FirstArg) then
	FirstArg
    else
	MakeFixnum FirstArg;

lisp procedure FloatMinus FirstArg;
    FloatDifference(0.0, FirstArg);

DefArith1Entry(Fix, ReturnFirstArg, ReturnFirstArg, FloatFix);

syslsp procedure FloatFix Arg;
begin scalar R;
    return if IsInum(R :=!*WFix FloatBase FltInf Arg) then R
	   else MakeFixnum R;
end;

DefArith1Entry(Float, FloatIntArg, FloatBigArg, ReturnFirstArg);

syslsp procedure FloatIntArg Arg;
begin scalar F;
    F := GtFLTN();
    !*WFloat(FloatBase F, Arg);
    return MkFLTN F;
end;


DefArith1PredicateEntry(MinusP, IntMinusP, BigMinusP, FloatMinusP);

syslsp procedure IntMinusP FirstArg;
    WLessP(FirstArg, 0);

lisp procedure FloatMinusP FirstArg;
    FloatLessP(FirstArg, 0.0);

DefArith1PredicateEntry(ZeroP, IntZeroP, ReturnNIL, FloatZeroP);

lisp procedure IntZeroP FirstArg;
    FirstArg = 0;

lisp procedure FloatZeroP FirstArg;
    EQN(FirstArg, 0.0);

DefArith1PredicateEntry(OneP, IntOneP, ReturnNIL, FloatOneP);

lisp procedure IntOneP FirstArg;
    FirstArg = 1;

lisp procedure FloatOneP FirstArg;
    EQN(FirstArg, 1.0);

END;
