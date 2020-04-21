
%. BIGFACE.RED  - Bignum Interfacing
%  M.L. Griss and B Morrison
%  25 June 1982
% --------------------------------------------------------------------------
% Revision History:
% 21 December, 82: MLG
%	Change PRIN1 and PRIN2 hooks to refer to RecursiveChannelprinx
%        which changed in PK:PRINTERS.RED for prinlevel stuff
%  November: Variety of Bug Fixes by A. Norman

off usermode;

% Use the BIGN tag for better Interface

imports '(vector!-fix arith bigbig);

compiletime<<load syslisp;
	     load fast!-vector;
	     load inum;
	     load if!-system>>;

on comp;

fluid '(WordHi!* WordLow!* BBase!* FloatHi!* FloatLow!*);


smacro procedure PutBig(b,i,val);
  IputV(b,i,val);

smacro procedure GetBig(b,i);
  IgetV(B,i);

% on syslisp;
% 
% procedure BigP x;
%   Tag(x) eq BIGN;
% 
% off syslisp;

lisp procedure BignumP (V);
  BigP V and ((GetBig(V,0) eq 'BIGPOS) or (GetBig(V,0) eq 'BIGNEG));

lisp procedure NonBigNumError(V,L);
  StdError BldMsg(" Expect a BIGNUM in %r, given %p%n",L,V);

lisp procedure BSize V;
  (BignumP V and VecLen VecInf V) or 0;

lisp procedure GtPOS N;
 Begin Scalar B;
    B:=MkVect N;
    IPutV(B,0,'BIGPOS);
    Return MkBigN Vecinf B;
 End;
 
lisp procedure GtNeg N;
 Begin Scalar B;
    B:=MkVect N;
    IPutV(B,0,'BIGNEG);
    Return MkBigN VecInf B;
 End;
 
lisp procedure TrimBigNum V3; % truncate trailing 0
 If Not BignumP V3 then NonBigNumError(V3,'TrimBigNum)
   else TrimBigNum1(V3,BSize V3);

lisp procedure TrimBigNum1(B,L3);
  Begin scalar v3;
     V3:=BigAsVec B;
     While IGreaterP(L3,0) and IZeroP IGetV(V3,L3) do L3:=ISub1 L3;
     If IZerop UpBv TruncateVector(V3,L3) then return GtPOS 0 
		else return B;
  end;

lisp procedure BigAsVec B;
 MkVec Inf B;

lisp procedure VecAsBig V;
 MkBig Inf V;
% -- Output---

if_system(VAX, 
	<<setbits 32;
	FloatHi!*:=btimes2(bdifference(btwopower 67, btwopower 11), 
			btwopower 60);% Largest representable float.
	FloatLow!*:=BMinus FloatHi!*>>);

if_system(PDP10,
	<<setbits 36;
	FloatHi!*:=btimes2(bsub1 btwopower 62, btwopower 65);
	FloatLow!*:=BMinus FloatHi!*>>);

% MLG Change to interface to Recursive hooks, added for
%  Prinlevel stuff
CopyD('OldChannelPrin1,'RecursiveChannelPrin1);
CopyD('OldChannelPrin2,'RecursiveChannelPrin2);

Lisp Procedure RecursiveChannelPrin1(Channel,U,Level);
  <<if BigNumP U then BChannelPrin2(Channel,U)
	else OldChannelPrin1(Channel, U,Level);U>>;

Lisp Procedure RecursiveChannelPrin2(Channel,U,level);
  <<If BigNumP U then BChannelPrin2(Channel, U)
	else OldChannelPrin2(Channel, U,level);U>>;

lisp procedure big2sys U;
 begin scalar L,Sn,res,I;
  L:=BSize U;
  if IZeroP L then return 0;
  Sn:=BMinusP U;
  res:=IGetV(U,L);
  I:=ISub1 L;
  while I neq 0 do <<res:=ITimes2(res, bbase!*);
		     res:=IPlus2(res, IGetV(U,I));
		     I:=ISub1 I>>;
  if Sn then Res:=IMinus Res;
  return Res;
 end;

smacro procedure checkifreallybig U;
 (lambda UU;  % This construction needed to avoid repeated evaluation;
 if BLessP(UU, WordLow!*) or BGreaterp(UU,WordHi!*) then UU
  else sys2int big2sys UU)(U);

smacro procedure checkifreallybigpair U;
 (lambda VV;
 checkifreallybig car VV . checkifreallybig cdr VV)(U);

smacro procedure checkifreallybigornil U;
 (lambda UU;
 if Null UU or BLessp(UU, WordLow!*) or BGreaterP(UU,WordHi!*) then UU
  else sys2int big2sys UU)(U);

lisp procedure BigPlus2(U,V);
 CheckIfReallyBig BPlus2(U,V);
  
lisp procedure BigDifference(U,V);
 CheckIfReallyBig BDifference(U,V);

lisp procedure BigTimes2(U,V);
 CheckIfReallyBig BTimes2(U,V);

lisp procedure BigDivide(U,V);
 CheckIfReallyBigPair BDivide(U,V);

lisp procedure BigQuotient(U,V);
 CheckIfReallyBig BQuotient(U,V);

lisp procedure BigRemainder(U,V);
 CheckIfReallyBig BRemainder(U,V);

lisp procedure BigLAnd(U,V);
 CheckIfReallyBig BLand(U,V);

lisp procedure BigLOr(U,V);
 CheckIfReallyBig BLOr(U,V);

lisp procedure BigLXOr(U,V);
 CheckIfReallyBig BLXor(U,V);

lisp procedure BigLShift(U,V);
 CheckIfReallyBig BLShift(U,V);

lisp procedure BigGreaterP(U,V);
 CheckIfReallyBigOrNil BGreaterP(U,V);

lisp procedure BigLessP(U,V);
 CheckIfReallyBigOrNil BLessP(U,V);

lisp procedure BigAdd1 U;
 CheckIfReallyBig BAdd1 U;

lisp procedure BigSub1 U;
 CheckIfReallyBig BSub1 U;

lisp procedure BigLNot U;
 CheckIfReallyBig BLNot U;

lisp procedure BigMinus U;
 CheckIfReallyBig BMinus U;

lisp procedure FloatBigArg U;
 FloatFromBigNum U;

lisp procedure BigMinusP U;
 CheckIfReallyBigOrNil BMinusP U;


% ---- Input ----

lisp procedure MakeStringIntoLispInteger(Str,Radix,Sn);
 CheckIfReallyBig BRead(Str,Radix,Sn);

% Coercion/Transfer Functions

copyd('oldFloatFix,'FloatFix);

procedure floatfix U;
 if U < BBase!* then OldFloatFix U
  else bigfromfloat U;

copyd('oldMakeFixNum, 'MakeFixNum);

procedure MakeFixNum N;		% temporary; check range?
 Begin;
  n:=oldMakeFixNum N;
  return int2b N;
 end;

syslsp procedure StaticIntBig Arg;    % Convert an INT to a BIG 
  int2b Arg;

syslsp procedure StaticBigFloat Arg;   % Convert a BigNum to a FLOAT;
  FloatFromBignum Arg;

copyd('oldInt2Sys, 'Int2Sys);

procedure Int2Sys N;
 if BigP N then Big2Sys N
  else OldInt2Sys n;


on syslisp;

 syslsp procedure IsInum U;
  U < lispvar bbase!* and U > minus lispvar bbase!*;

off syslisp;


on usermode;

