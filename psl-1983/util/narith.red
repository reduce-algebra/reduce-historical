%
% ARITHMETIC.RED - Generic arithmetic routines for PSL
% 	           New model, much less hairy lap

% Author:      Eric Benson
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        9 August 1982
% Copyright (c) 1982 University of Utah
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Note: Loading BIGFACE is supposed to define (or redefine)
%       the functions:
%		ISINUM
%		StaticIntBig
%		StaticBigFloat
%		Sys2Int
%		Int2Sys
%		FloatFix
%
% Mods by MLG, 21 dec 1982
% 	Take off INTERNALFUNCTION form FLOATFIX and StaticFloatBig
% 	Change IsInum to be a procedure
%       Change names of FAKE and SFL to xxxxLOC

CompileTime << % Some aliases
	Fluid '(ArithArgLoc StaticFloatLoc);
        put('ArithArg, 'NewNam, '(LispVar ArithArgLoc));
        put('StaticFloat, 'NewNam, '(LispVar StaticFloatLoc));
>>;

LoadTime <<     % Allocate Physical Space
	ArithArgLoc := GtWArray 2;
        StaticFloatLoc := GtWArray 3;
>>;

on Syslisp;

%internal WArray ArithArg[1], StaticFloat = [1, 0, 0];

CompileTime <<

flag('(Coerce2 FloatPlus2 FloatDifference FloatTimes2
       FloatQuotient FloatGreaterP FloatLessP IntFloat
       NonInteger2Error NonNumber1Error
), 'InternalFunction);

syslsp macro procedure IsInumMac U;
<<  U := second U;
    if atom U then
	list('eq, list('SignedField, U, '(ISub1 (WConst InfStartingBit)),
					'(IAdd1 (WConst InfBitLength))), U)
    else
    list('(lambda (X) (eq (SignedField X
				       (ISub1 (WConst InfStartingBit))
				       (IAdd1 (WConst InfBitLength)))
			  X)),
	 U) >>;

expr procedure NameGen Name;
    Intern Concat(ID2String Name, StringGensym());

macro procedure DefArith2Entry U;
begin scalar generic, wgen, fgen, bgen, hardgen, gen0;
    U :=rest U;
    generic := first U;
    U := rest U;
    wgen := first U;
    U := rest U;
    fgen := first U;
    U := rest U;
    bgen := first U;
    hardgen := NameGen generic;
    gen0 := NameGen generic;
    Flag1(hardgen, 'InternalFunction);
    Flag1(gen0, 'InternalFunction);
    return SublA(Pair('(GENERIC WGEN FGEN BGEN HARDGEN GEN0),
		      list(generic, wgen, fgen, bgen, hardgen, gen0)),
		 quote <<

expr procedure GENERIC(x,y);
    if intp x and intp y then GEN0(x, y, WGEN(x, y)) else HARDGEN(x, y);

expr procedure GEN0(x, y, z);
    if isinum z then z else HARDGEN(x, y);

expr procedure HARDGEN(x, y);
    case Coerce2(x, y, 'GENERIC) of
	POSINT:
	    Sys2Int WGEN(WGetV(ArithArg, 0), WGetV(ArithArg, 1));
	FLTN:
	    FGEN(WGetV(ArithArg, 0), WGetV(ArithArg, 1));
	BIGN:
	    BGEN(WGetV(ArithArg, 0), WGetV(ArithArg, 1));
    end;

>>);
end;

macro procedure DefArithPred2Entry U;
begin scalar generic, wgen, fgen, bgen, hardgen, gen0;
    U :=rest U;
    generic := first U;
    U := rest U;
    wgen := first U;
    U := rest U;
    fgen := first U;
    U := rest U;
    bgen := first U;
    hardgen := NameGen generic;
    gen0 := NameGen generic;
    Flag1(hardgen, 'InternalFunction);
    Flag1(gen0, 'InternalFunction);
    return SublA(Pair('(GENERIC WGEN FGEN BGEN HARDGEN GEN0),
		      list(generic, wgen, fgen, bgen, hardgen, gen0)),
		 quote <<

expr procedure GENERIC(x,y);
    if intp x and intp y then WGEN(x, y) else HARDGEN(x, y);

expr procedure HARDGEN(x, y);
    case Coerce2(x, y, 'GENERIC) of
	POSINT:
	    WGEN(WGetV(ArithArg, 0), WGetV(ArithArg, 1));
	FLTN:
	    FGEN(WGetV(ArithArg, 0), WGetV(ArithArg, 1));
	BIGN:
	    BGEN(WGetV(ArithArg, 0), WGetV(ArithArg, 1));
    end;

>>);
end;

macro procedure DefInt2Entry U;
begin scalar generic, wgen, bgen, hardgen, gen0;
    U :=rest U;
    generic := first U;
    U := rest U;
    wgen := first U;
    U := rest U;
    bgen := first U;
    hardgen := NameGen generic;
    gen0 := NameGen generic;
    Flag1(hardgen, 'InternalFunction);
    Flag1(gen0, 'InternalFunction);
    return SublA(Pair('(GENERIC WGEN BGEN HARDGEN GEN0),
		      list(generic, wgen, bgen, hardgen, gen0)),
		 quote <<

expr procedure GENERIC(x,y);
    if intp x and intp y then GEN0(x, y, WGEN(x, y)) else HARDGEN(x, y);

expr procedure GEN0(x, y, z);
    if isinum z then z else HARDGEN(x, y);

expr procedure HARDGEN(x, y);
    case Coerce2(x, y, 'GENERIC) of
	POSINT:
	    Sys2Int WGEN(WGetV(ArithArg, 0), WGetV(ArithArg, 1));
	FLTN:
	    NonInteger2Error(x, y, 'GENERIC);
	BIGN:
	    BGEN(WGetV(ArithArg, 0), WGetV(ArithArg, 1));
    end;

>>);
end;

macro procedure DefArith1Entry U;
begin scalar generic, wgen, fgen, bgen, hardgen, gen0;
    U :=rest U;
    generic := first U;
    U := rest U;
    wgen := first U;
    U := rest U;
    fgen := first U;
    U := rest U;
    bgen := first U;
    hardgen := NameGen generic;
    gen0 := NameGen generic;
    Flag1(hardgen, 'InternalFunction);
    Flag1(gen0, 'InternalFunction);
    return SublA(Pair('(GENERIC WGEN FGEN BGEN HARDGEN GEN0),
		      list(generic, wgen, fgen, bgen, hardgen, gen0)),
		 quote <<

expr procedure GENERIC x;
    if intp x then GEN0(x, WGEN x) else HARDGEN x;

expr procedure GEN0(x, z);
    if isinum z then z else HARDGEN x;

expr procedure HARDGEN x;
    case Tag x of
	NEGINT, POSINT:
	    Sys2Int WGEN x;
	FIXN:
	    Sys2Int WGEN FixVal FixInf x;
	FLTN:
	    FGEN x;
	BIGN:
	    BGEN x;
	default:
	    NonNumber1Error(x, 'GENERIC);
    end;

>>);
end;

macro procedure DefArithPred1Entry U;
begin scalar generic, wgen, fgen, bgen, hardgen, gen0;
    U :=rest U;
    generic := first U;
    U := rest U;
    wgen := first U;
    U := rest U;
    fgen := first U;
    U := rest U;
    bgen := first U;
    hardgen := NameGen generic;
    gen0 := NameGen generic;
    Flag1(hardgen, 'InternalFunction);
    Flag1(gen0, 'InternalFunction);
    return SublA(Pair('(GENERIC WGEN FGEN BGEN HARDGEN GEN0),
		      list(generic, wgen, fgen, bgen, hardgen, gen0)),
		 quote <<

expr procedure GENERIC x;
    if intp x then WGEN x else HARDGEN x;

expr procedure HARDGEN x;
    case Tag x of
	NEGINT, POSINT:
	    WGEN x;
	FIXN:
	    WGEN FixVal FixInf x;
	FLTN:
	    FGEN x;
	BIGN:
	    BGEN x;
	default:
	    NIL;
    end;

>>);
end;

smacro procedure DefFloatEntry(Name, Prim);
procedure Name(x, y);
begin scalar f;
    f := GtFLTN();
    Prim(FloatBase f, FloatBase FltInf x,
		      FloatBase FltInf y);
    return MkFLTN f;
end;


>>;

procedure Coerce2(X, Y, F);
%
% Returns type tag of strongest type and sets ArithArg[0] to be coerced X
% and ArithArg[1] to coerced Y.
%
begin scalar T1, T2, P, C;
    T1 := Tag X;
    case T1 of
	NEGINT:
	    T1 := POSINT;
	FIXN:
	<<  T1 := POSINT;
	    X := FixVal FixInf X >>;
    end;
    T2 := Tag Y;
    case T2 of
	NEGINT:
	    T2 := POSINT;
	FIXN:
	<<  T2 := POSINT;
	    Y := FixVal FixInf Y >>;
    end;
    ArithArg[0] := X;
    ArithArg[1] := Y;
    if T1 eq T2 then return T1;		% no coercion to be done
    if T1 < T2 then			% coerce first arg to second
    <<  P := &ArithArg[0];		% P points to first (to be coerced)
	C := T2;			% swap T1 and T2
	T2 := T1;
	T1 := C >>
    else
	P := &ArithArg[1];		% P points to second
    if T1 > FLTN then return
	ContinuableError(99, "Non-numeric argument in arithmetic",
			     list(F, MkQuote X, MkQuote Y));
    case T1 of
	FLTN:
	    case T2 of
		POSINT:
		    @P := StaticIntFloat @P;
		BIGN:
		    @P := StaticBigFloat @P;
	    end;
	BIGN:
	    @P := StaticIntBig @P;	% @P must be inum
    end;
    return T1;
end;

procedure StaticIntFloat X;
<<  !*WFloat(&StaticFloat[1], X);
    MkFLTN &StaticFloat[0] >>;

procedure NonInteger2Error(X, Y, F);
    ContinuableError(99, "Non-integer argument in arithmetic",
			 list(F, MkQuote X, MkQuote Y));

procedure NonNumber1Error(X, F);
    ContinuableError(99, "Non-numeric argument in arithmetic",
			 list(F, MkQuote X));


DefArith2Entry(Plus2, WPlus2, FloatPlus2, BigPlus2);

DefFloatEntry(FloatPlus2, !*FPlus2);

DefArith2Entry(Difference, WDifference, FloatDifference, BigDifference);

DefFloatEntry(FloatDifference, !*FDifference);

DefArith2Entry(Times2, WTimes2, FloatTimes2, BigTimes2);

DefFloatEntry(FloatTimes2, !*FTimes2);

DefArith2Entry(Quotient, WQuotient, FloatQuotient, BigQuotient);

DefFloatEntry(FloatQuotient, !*FQuotient);

DefArithPred2Entry(GreaterP, WGreaterP, FloatGreaterP, BigGreaterP);

procedure FloatGreaterP(X, Y);
    if !*FGreaterP(FloatBase FltInf X, FloatBase FltInf Y) then T else NIL;

DefArithPred2Entry(LessP, WLessP, FloatLessP, BigLessP);

procedure FloatLessP(X, Y);
    if !*FLessP(FloatBase FltInf X, FloatBase FltInf Y) then T else NIL;

DefInt2Entry(Remainder, WRemainder, BigRemainder);

DefInt2Entry(LAnd, WAnd, BigLAnd);

DefInt2Entry(LOr, WOr, BigLOr);

DefInt2Entry(LXOr, WXOr, BigLXOr);

DefInt2Entry(LShift, WShift, BigLShift);

PutD('LSH, 'EXPR, cdr GetD 'LShift);

DefArith1Entry(Add1, IAdd1, lambda X; FloatPlus2(X, '1.0), BigAdd1);

DefArith1Entry(Sub1, ISub1, lambda X; FloatDifference(X, '1.0), BigSub1);

DefArith1Entry(Minus, IMinus, lambda X; FloatDifference('0.0, X), BigMinus);

DefArith1Entry(Fix, lambda X; X, FloatFix, lambda X; X);

procedure FloatFix X;
    Sys2Int !*WFix FloatBase FltInf X;

procedure Float X;
    case Tag X of
	POSINT, NEGINT:
	    IntFloat X;
	FIXN:
	    IntFloat FixVal FixInf X;
	FLTN:
	    X;
	BIGN:
	    FloatBigArg X;
	default:
	    NonNumber1Error(X, 'Float);
    end;

procedure IntFloat X;
begin scalar F;
    F := GtFLTN();
    !*WFloat(FloatBase F, X);
    return MkFLTN F;
end;

DefArithPred1Entry(MinusP, IMinusP, lambda X; FloatLessP(X, '0.0), BigMinusP);

DefArithPred1Entry(ZeroP, IZeroP, lambda X; EQN(X, '0.0), ReturnNil);

DefArithPred1Entry(OneP, IOneP, lambda X; EQN(X, '1.0), ReturnNil);

syslsp procedure ReturnNil U;
    NIL;

syslsp procedure IsInum U;
 IsInumMac U;

off Syslisp;

END;
