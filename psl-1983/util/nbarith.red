% NBARITH.RED - Generic arithmetic routines for PSL
% 	       New model, much less hairy lap

% Author:      Eric Benson and Martin Griss
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        9 August 1982
% Copyright (c) 1982 University of Utah
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The MODEL:
% It is assumed that there is a range of INUMs (subset) called
% BETAnums that can be safely operated on by the Wxxx or Ixxx routines
% without loss of precesion or overflow, and return an INUM (or at least
% a SYSINT.
%
% A UNARY operation (UN x) is done as:
%  Procedure UN x;
%    If BetaP x then <<x:=WUN x; if IntRangeP x then x else Sys2Int x>>
%      else UN!-HARD(x);

% A UNARY predicate  (UNP x) is done as:
%  Procedure UNP x;
%    If BetaP x then WUNP x
%      else UNP!-HARD(x);


% A BINARY operation (BIN x y) is done as:
%  Procedure BIN(x,y);
%    If BetaP x and BetaP y 
%	then <<x:=WBIN(x,y); 
%	       if IntRangeP x then x else Sys2Int x>>
%     else BIN!-HARD(x,y);

% A BINARY predicate (BINP x y) is done as:
%  Procedure BINP(x,y);
%    If BetaP x and BetaP y then WBINP(x,y) 
%     else BINP!-HARD(x,y);

% IN some "safe" cases, BetaP can become IntP (beware of *)
% In others, BetaP(y) may be too weak (eg, Lshift and Expt)

% Note: Loading NBIG0 is supposed to define (or redefine)
%       the functions:
%		BetaP
%               Beta2P
%               BetaRangeP
%		Sys2Big
%		FloatFromBignum
%		Sys2Int
%		FloatFix
% Removed IsInum and INTP in favor of BetaP
%
% Mods by MLG, 21 dec 1982
% 	Take off INTERNALFUNCTION form FLOATxxx
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

expr procedure BetaP x;
% Test tagged number is in Beta Range when BIGNUM loaded
% Will redefine if NBIG loaded
   IntP x;

expr procedure BetaRangeP w;
% Test Word is in Beta Range when BIGNUM loaded
% Ie, is FIXNUM size with no NBIG
% Will redefine if NBIG loaded
   'T;

expr procedure Beta2P(x,y);
% Test if BOTH in Beta range
% Will be redefined if NBIG loaded
  if IntP x then Intp y else NIL;

expr procedure Sys2Big W;
% Out of safe range, convert to BIGN
    ContinuableError(99, "Sys2Big cant convert Word to BIGNUM, no BIGNUM's loaded",
                          Sys2Int W);

on Syslisp;

CompileTime <<

%flag('(Coerce2 FloatPlus2 FloatDifference FloatTimes2
%       FloatQuotient FloatGreaterP FloatLessP IntFloat
%       NonInteger2Error NonNumber1Error  NonNumber2Error
%), 'NotYetInternalFunction);

expr procedure NameGen(Name,Part);
% Generate Nice specific name from Generic name 
    Intern Concat(ID2String Name,ID2String Part);

smacro procedure NextArg();
% Just substitute in the context of U
  <<U:=cdr U; car U>>;

smacro procedure Prologue();
% Common Prologue
<<  generic := NextArg();
    wgen := NextArg();
    fgen := NextArg();
    bgen := NextArg();
    hardgen := NameGen(generic,'!-Hardcase);
    Flag1(hardgen, 'NotYetInternalFunction);
>>;

macro procedure DefArith2Entry U;
begin scalar generic, wgen, fgen, bgen, hardgen;
    Prologue();
    return SublA(Pair('(GENERIC WGEN FGEN BGEN HARDGEN),
		      list(generic, wgen, fgen, bgen, hardgen)),
		 quote <<

expr procedure GENERIC(x,y);
    if Beta2P(x,y) then <<x:=WGEN(x,y);
		          If IntP x then x else Sys2Int x>>
      else HARDGEN(x, y);

expr procedure HARDGEN(x, y);
    case Coerce2(x, y, 'GENERIC) of
	POSINT:   Sys2Int WGEN(WGetV(ArithArg, 0), WGetV(ArithArg, 1));
	 %/ Beware of Overflow, WGEN maybe should test args
	 %/ Coerce2 is supposed to check this case
	FLTN:     FGEN(WGetV(ArithArg, 0), WGetV(ArithArg, 1));
	BIGN:     BGEN(WGetV(ArithArg, 0), WGetV(ArithArg, 1));
    end;

>>);
end;

macro procedure DefArithPred2Entry U;
begin scalar generic, wgen, fgen, bgen, hardgen;
    Prologue();
    return SublA(Pair('(GENERIC WGEN FGEN BGEN HARDGEN),
		      list(generic, wgen, fgen, bgen, hardgen)),
		 quote <<

expr procedure GENERIC(x,y);
    if Beta2P(x,y) then WGEN(x, y) else HARDGEN(x, y);

expr procedure HARDGEN(x, y);
    case Coerce2(x, y, 'GENERIC) of
	POSINT:   WGEN(WGetV(ArithArg, 0), WGetV(ArithArg, 1));
%/ Assumes Preds are safe against Overflow
	FLTN:     FGEN(WGetV(ArithArg, 0), WGetV(ArithArg, 1));
	BIGN:     BGEN(WGetV(ArithArg, 0), WGetV(ArithArg, 1));
    end;

>>);
end;

macro procedure DefInt2Entry U;
begin scalar generic, wgen, fgen, bgen, hardgen;
    Prologue();	
    return SublA(Pair('(GENERIC WGEN BGEN HARDGEN),
		      list(generic, wgen, bgen, hardgen)),
		 quote <<

expr procedure GENERIC(x,y);
    if Beta2P(x,y) then <<x:=WGEN(x, y);
	                  if IntP x then x else Sys2Int x>>
     else HARDGEN(x, y);

expr procedure HARDGEN(x, y);
    case Coerce2(x, y, 'GENERIC) of
	POSINT:   Sys2Int WGEN(WGetV(ArithArg, 0), WGetV(ArithArg, 1));
	FLTN:     NonInteger2Error(x, y, 'GENERIC);
	BIGN:     BGEN(WGetV(ArithArg, 0), WGetV(ArithArg, 1));
    end;

>>);
end;

macro procedure DefArith1Entry U;
begin scalar generic, wgen, fgen, bgen, hardgen;
    Prologue();
    return SublA(Pair('(GENERIC WGEN FGEN BGEN HARDGEN),
		      list(generic, wgen, fgen, bgen, hardgen)),
		 quote <<

expr procedure GENERIC x;
    if BetaP x then <<x:=WGEN x;
	              if IntP x then x else Sys2Int x>>
     else HARDGEN x;

expr procedure HARDGEN x;
    case Coerce1(x,'GENERIC) of
	POSINT:   Sys2Int WGEN WGetv(ArithArg,0);
	FLTN:     FGEN WGetv(ArithArg,0);
	BIGN:     BGEN WGetv(ArithArg,0);
        default:  NonNumber1Error(x,'GENERIC);
    end;

>>);
end;

macro procedure DefArithPred1Entry U;
begin scalar generic, wgen, fgen, bgen, hardgen;
    Prologue();
    return SublA(Pair('(GENERIC WGEN FGEN BGEN HARDGEN),
		      list(generic, wgen, fgen, bgen, hardgen)),
		 quote <<

expr procedure GENERIC x;
    if BetaP x then WGEN x else HARDGEN x;

expr procedure HARDGEN x;
    case Coerce1(x,'GENERIC) of
	POSINT:  WGEN Wgetv(ArithArg,0);
	FLTN:    FGEN Wgetv(ArithArg,0);
	BIGN:    BGEN Wgetv(ArithArg,0);
	default: NIL;
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

% The support procedures for coercing types

procedure Coerce1(X, F);
% Returns type tag of coerced X type and sets ArithArg[0] to be coerced X
% Beware of ADD1/SUB1 cases, maybe can optimize later
begin scalar T1;
    T1 := Tag X;
    case T1 of
	NEGINT:   T1 := POSINT;
	FIXN:    <<  T1 := POSINT;    X := FixVal FixInf X >>;
    end;
    If T1=POSINT and not BetaRangeP(x) then <<T1:=BIGN; x:=Sys2Big x>>;
    WPutv(ArithArg,0,X);
    return T1;
end;

procedure Coerce2(X, Y, F);
% Returns type tag of strongest type and sets ArithArg[0] to be coerced X
% and ArithArg[1] to coerced Y.
begin scalar T1, T2, P, C;
    T1 := Tag X;
    case T1 of
	NEGINT:     T1 := POSINT;
	FIXN:   <<  T1 := POSINT;   X := FixVal FixInf X >>;
    end;
    If T1=POSINT and not BetaRangeP(x) then <<T1:=BIGN; x:=Sys2Big x>>;
    T2 := Tag Y;
    case T2 of
	NEGINT:     T2 := POSINT;
	FIXN:   <<  T2 := POSINT;   Y := FixVal FixInf Y >>;
    end;
    If T2=POSINT and not BetaRangeP(Y) then <<T2:=BIGN; y:=Sys2Big y>>;
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
    if T1 > FLTN then return NonNumber2Error(X,Y,F);
 % Here, since no 2 arg Arith Preds that accept 1 number, one not
    case T1 of
	FLTN:  case T2 of
		 POSINT:    @P := StaticIntFloat @P;
		 BIGN: 	    @P := FloatFromBignum @P;
	       end;
	BIGN:     @P := Sys2Big @P;	% @P must be SYSint
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

procedure NonNumber2Error(X, Y, F);
    ContinuableError(99, "Non-numeric argument in arithmetic",
			 list(F, MkQuote X,Mkquote Y));


% Now generate the entries for each operator

DefArith2Entry(Plus2, WPlus2, FloatPlus2, BigPlus2);
DefFloatEntry(FloatPlus2, !*FPlus2);
DefArith2Entry(Difference, WDifference, FloatDifference, BigDifference);
DefFloatEntry(FloatDifference, !*FDifference);
DefArith2Entry(Times2, WTimes2, FloatTimes2, BigTimes2);
	 % Beware of Overflow 
DefFloatEntry(FloatTimes2, !*FTimes2);
DefArith2Entry(Quotient, WQuotient, FloatQuotient, BigQuotient);
	DefFloatEntry(FloatQuotient, !*FQuotient);
DefArithPred2Entry(GreaterP, WGreaterP, FloatGreaterP, BigGreaterP);
	procedure FloatGreaterP(X, Y);
	    if !*FGreaterP(FloatBase FltInf X, FloatBase FltInf Y) 
			then T else NIL;
DefArithPred2Entry(LessP, WLessP, FloatLessP, BigLessP);
	procedure FloatLessP(X, Y);
          if !*FLessP(FloatBase FltInf X, FloatBase FltInf Y) then T else NIL;
        procedure Fdummy(x,y);
          StdError "Fdummy should never be called";
DefInt2Entry(Remainder, WRemainder, Fdummy, BigRemainder);
DefInt2Entry(LAnd, WAnd, Fdummy, BigLAnd);
DefInt2Entry(LOr, WOr, Fdummy, BigLOr);
DefInt2Entry(LXOr, WXOr, Fdummy, BigLXOr);
% Cant DO Lshift in terms of BETA sized shifts
% Will toatlly redefine in BIG package
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
		POSINT, NEGINT:     IntFloat X;
		FIXN:     IntFloat FixVal FixInf X;
		FLTN:     X;
		BIGN:     FloatFromBigNum X;
		default:     NonNumber1Error(X, 'Float);
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

off Syslisp;

END;
