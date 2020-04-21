%
% EQUAL.RED - EQUAL, EQN and friends
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        19 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL>EQUAL.RED.2, 21-Sep-82 10:38:28, Edit by BENSON
%  Made HalfWordsEqual, etc. internal

% EQ is handled by the compiler and is in KNOWN-TO-COMP-SL.RED

CompileTime flag('(HalfWordsEqual VectorEqual WordsEqual), 'InternalFunction);

on SysLisp;

syslsp procedure Eqn(U, V);		%. Eq or numeric equality
    U eq V or case Tag U of		% add bignums later
		FLTN:
		    FloatP V and
			FloatHighOrder FltInf U eq FloatHighOrder FltInf V
		    and FloatLowOrder FltInf U eq FloatLowOrder FltInf V;
		FIXN:
		  FixNP V and  FixVal FixInf U eq FixVal FixInf V;
		BIGN:
		  BigP V and WordsEqual(U, V);
		default:
		  NIL
	      end;

% Called LispEqual instead of Equal, to avoid name change due to Syslisp parser

syslsp procedure LispEqual(U, V);	%. Structural equality
    U eq V or case Tag U of
		VECT:
		  VectorP V and VectorEqual(U, V);
		STR, BYTES:
		  StringP V and StringEqual(U, V);			
		PAIR:
		  PairP V and
			LispEqual(car U, car V) and LispEqual(cdr U, cdr V);
		FLTN:
		    FloatP V and
			FloatHighOrder FltInf U eq FloatHighOrder FltInf V
		    and FloatLowOrder FltInf U eq FloatLowOrder FltInf V;
		FIXN:
		  FixNP V and  FixVal FixInf U eq FixVal FixInf V;
		BIGN:
		  BigP V and WordsEqual(U, V);
		WRDS:
		  WrdsP V and WordsEqual(U, V);
		HalfWords:
		  HalfWordsP V and HalfWordsEqual(U, V);
		default:
		  NIL
	      end;

syslsp procedure EqStr(U, V);		%. Eq or string equality
    U eq V or StringP U and StringP V and StringEqual(U, V);

syslsp procedure StringEqual(U, V);	% EqStr without typechecking or eq
begin scalar Len, I;
    U := StrInf U;
    V := StrInf V;
    Len := StrLen U;
    if Len neq StrLen V then return NIL;
    I := 0;
Loop:
    if I > Len then return T;
    if StrByt(U, I) neq StrByt(V, I) then return NIL;
    I := I + 1;
    goto Loop;
end;

syslsp procedure WordsEqual(U, V);
begin scalar S1, I;
    U := WrdInf U;
    V := WrdInf V;
    if not ((S1 := WrdLen U) eq WrdLen V) then return NIL;
    I := 0;
Loop:
    if I eq S1 then return T;
    if not (WrdItm(U, I) eq WrdItm(V, I)) then return NIL;
    I := I + 1;
    goto Loop;
end;

syslsp procedure HalfWordsEqual(U, V);
begin scalar S1, I;
    U := HalfWordInf U;
    V := HalfWordInf V;
    if not ((S1 := HalfWordLen U) eq HalfWordLen V) then return NIL;
    I := 0;
Loop:
    if I eq S1 then return T;
    if not (HalfWordItm(U, I) eq HalfWordItm(V, I)) then return NIL;
    I := I + 1;
    goto Loop;
end;

syslsp procedure VectorEqual(U, V);	% Vector equality without type check
begin scalar Len, I;
    U := VecInf U;
    V := VecInf V;
    Len := VecLen U;
    if Len neq VecLen V then return NIL;
    I := 0;
Loop:
    if I > Len then return T;
    if not LispEqual(VecItm(U, I), VecItm(V, I)) then return NIL;
    I := I + 1;
    goto Loop;
end;

off SysLisp;

LoadTime PutD('Equal, 'EXPR, cdr GetD 'LispEqual);

END;
