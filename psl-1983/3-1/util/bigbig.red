% BIGBIG.RED - Vector based BIGNUM package with INUM operations
% M. L. Griss & B Morrison
% 25 June 1982.
%
% Revision log:
% 20 Dec:
% MLG, changed TrimBigNUM to TrimBigNum1 in BhardDivide

% 14 Dec:
% Changed by MLG to put LOAD and IMPORTS in BUILD file

% A. C . Norman - adjstments to many routines!
% in particular corrections to BHardDivide (case D6 utterly wrong),
%    and adjustments to BExpt (for performance) and all logical
%    operators (for treatment of negative inputs);
% 31 August 1982: 
% Copyright (C) 1982, A. C. Norman, B. Morrison, M. Griss
% ---------------------------------------------------------------

% -----------------------
% A bignum will be a VECTOR of Bigits: (digits in base BigBase):
%  [BIGPOS b1 ... bn] or [BIGNEG b1 ... bn].  BigZero is thus [BIGPOS]
% All numbers are positive, with BIGNEG as 0 element to indicate negatives.

Fluid '(BBase!* BBits!* LogicalBits!* WordHi!* WordLow!* Digit2Letter!*
	FloatHi!* FloatLow!* SysHi!* SysLo!* Carry!* OutputBase!*);

% --------------------------------------------------------------------------
% --------------------------------------------------------------------------
% Support functions:
%
% U, V, V1, V2 for arguments are Bignums.  Other arguments are usually
% fix/i-nums.

lisp procedure setbits x;
%
% This function sets the globals for big bignum package.
% "x" should be total # of bits per word.
  <<BBits!*:=iquotient(isub1 x,2); % Total number of bits per word used.
  BBase!*:=TwoPower BBits!*;	% "Beta", where n=A0 + A1*beta + A2*(beta^2)...
  WordHi!*:=BNum Isub1 BBase!*;	% Highest value of Ai
  WordLow!*:=BMinus WordHi!*;	% Lowest value of Ai
  LogicalBits!*:=ISub1 BBase!*;	% Used in LAnd,Lor, etc.
  SysHi!*:=bsub1 btwopower isub1 x; % Largest representable Syslisp integer.
  SysLo!*:=BMinus BAdd1 SysHi!*;    % Smallest representable Syslisp integer.
  BBase!*>>;

lisp procedure BignumP (V);
  VectorP V and ((V[0] eq 'BIGPOS) or (V[0] eq 'BIGNEG));

lisp procedure NonBigNumError(V,L);
  StdError BldMsg(" Expect a BIGNUM in %r, given %p%n",L,V);

lisp procedure BSize V;
  (BignumP V and UpbV V) or 0;

lisp procedure GtPOS N;	% Creates a positive Bignum with N "Bigits".
 Begin Scalar B;
    B:=MkVect N;
    IPutV(B,0,'BIGPOS);
    Return B;
 End;
 
lisp procedure GtNeg N;	% Creates a negative Bignum with N "Bigits".
 Begin Scalar B;
    B:=MkVect N;
    IPutV(B,0,'BIGNEG);
    Return B;
 End;
 
lisp procedure TrimBigNum V3;		% Truncate trailing 0.
 If Not BignumP V3 then NonBigNumError(V3,'TrimBigNum)
   else TrimBigNum1(V3,BSize V3);

lisp procedure TrimBigNum1(V3,L3);
  % V3 is a bignum and L3 is the position in it of the highest
  % possible non-zero digit. Truncate V3 to remove leading zeros,
  % and if this leaves V3 totally zero make its sign positive;
  Begin
     While IGreaterP(L3,0) and IZeroP IGetV(V3,L3) do L3:=ISub1 L3;
     If IZerop Bsize TruncateVector(V3,L3) then IPutV(V3,0,'BIGPOS);
     return V3;
  end;

lisp procedure big2sys U;
 if BLessP(U, SysLo!*) or BGreaterP(U, SysHi!*) then
	Error(99,list(U," is too large to be a Syslisp integer for BIG2SYS"))
  else begin scalar L,Sn,res,I;
   L:=BSize U;
   if IZeroP L then return 0;
   Sn:=BMinusP U;
   res:=IGetV(U,L);
   I:=ISub1 L;
   while not IZeroP I do <<res:=ITimes2(res, bbase!*);
		           res:=IPlus2(res, IGetV(U,I));
		           I:=ISub1 I>>;
   if Sn then Res:=IMinus Res;
   return Res;
  end;

lisp procedure TwoPower N;	%fix/i-num 2**n
 2**n;

lisp procedure BTwoPower N;	% gives 2**n; n is fix/i-num; result BigNum
 if not (fixp N or BignumP N) then NonIntegerError(N, 'BTwoPower)
  else begin scalar quot, rem, V;
   if bignump N then n:=big2sys n;
   quot:=Quotient(N,Bbits!*);
   rem:=Remainder(N,Bbits!*);
   V:=GtPOS(IAdd1 quot);
   IFor i:=1:quot do IPutV(v,i,0);
   IPutV(V,IAdd1 quot,twopower rem);
   return TrimBigNum1(V,IAdd1 quot);
  end;

lisp procedure BZeroP V1;
 IZerop BSize V1 and not BMinusP V1;

lisp procedure BOneP V1;
 Not BMinusP V1 and IOneP (BSize V1) and IOneP IGetV(V1,1);

lisp procedure BAbs V1;
 if BMinusP V1 then BMinus V1 else V1;

lisp procedure BMax(V1,V2);
 if BGreaterP(V2,V1) then V2 else V1; 

lisp procedure BMin(V1,V2);
 if BLessP(V2,V1) then V2 else V1;

lisp procedure BExpt(V1,N);	% V1 is Bignum, N is fix/i-num
 if not fixp N then NonIntegerError(N,'BEXPT)
 else if IZeroP N then int2B 1 
 else if IOneP N then V1
 else if IMinusP N then BQuotient(int2B 1,BExpt(V1,IMinus N))
 else begin scalar V2;
    V2 := BExpt(V1,IQuotient(N,2));
    if IZeroP IRemainder(N,2) then return BTimes2(V2,V2)
    else return BTimes2(BTimes2(V2,V1),V2)
 end;


% ---------------------------------------
% Logical Operations
%
% All take Bignum arguments


lisp procedure BLOr(V1,V2);
% The main body of the OR code is only obeyed when both arguments
% are positive, and so the result will be positive;
 if BMinusp V1 or BMinusp V2 then BLnot BLand(BLnot V1,BLnot V2)
 else begin scalar L1,L2,L3,V3;
     L1:=BSize V1;
     L2:=BSize V2;
     IF L2>L1 then <<L3:=L2; L2:=L1;L1:=L3;
                     V3:=V2; V2:=V1;V1:=V3>>;
     V3:=GtPOS L1;
     IFor I:=1:L2 do IPutV(V3,I,ILor(IGetV(V1,I),IGetV(V2,I)));
     IFor I:=(IAdd1 L2):L1 do IPutV(V3,i,IGetV(V1,I));
     Return V3
 end;

lisp procedure BLXor(V1,V2);
% negative arguments are coped with using the identity
% LXor(a,b) = LNot LXor(Lnot a,b) = LNor LXor(a,Lnot b);
 begin scalar L1,L2,L3,V3,S;
     if BMinusp V1 then << V1 := BLnot V1; S := t >>;
     if BMinusp V2 then << V2 := BLnot V2; S := not S >>;
     L1:=BSize V1;
     L2:=BSize V2;
     IF L2>L1 then <<L3:=L2; L2:=L1;L1:=L3;
                     V3:=V2; V2:=V1;V1:=V3>>;
     V3:=GtPOS L1;
     IFor I:=1:L2 do IPutV(V3,I,ILXor(IGetV(V1,I),IGetV(V2,I)));
     IFor I:=(IAdd1 L2):L1 do IPutV(V3,i,IGetV(V1,I));
     V1:=TrimBigNum1(V3,L1);
     if S then V1:=BLnot V1;
     return V1
 end;

% Not Used Currently:
%
% lisp Procedure BLDiff(V1,V2);	
% ***** STILL NEEDS ADJUSTING WRT -VE ARGS *****
%  begin scalar V3,L1,L2;
%    L1:=BSize V1;
%    L2:=BSize V2;
%    V3:=GtPOS(max(L1,L2));
%    IFor i:=1:min(L1,L2) do 
% 	IPutV(V3,i,ILAnd(IGetV(V1,i),ILXor(LogicalBits!*,IGetV(V2,i))));
%    if IGreaterP(L1,L2) then IFor i:=(IAdd1 L2):L1 do IPutV(V3,i,IGetV(V1,i));
%    if IGreaterP(L2,L1) then IFor i:=(IAdd1 L1):L2 do IPutV(V3,i,0);
%    return TrimBigNum1(V3,max(L1,L2));
%  end;

lisp procedure BLAnd(V1,V2);
% If both args are -ve the result will be too. Otherwise result will
% be positive;
 if BMinusp V1 and BMinusp V2 then BLnot BLor(BLnot V1,BLnot v2)
 else begin scalar L1,L2,L3,V3;
     L1:=BSize V1;
     L2:=BSize V2;
     L3:=Min(L1,L2);
     V3:=GtPOS L3;
     if BMinusp V1 then
       IFor I:=1:L3 do IPutV(V3,I,ILand(ILXor(Logicalbits!*,IGetV(V1,I)),
					IGetV(V2,I)))
     else if BMinusp V2 then
       IFor I:=1:L3 do IPutV(V3,I,ILand(IGetV(V1,I),
                                        ILXor(Logicalbits!*,IGetV(V2,I))))
     else IFor I:=1:L3 do IPutV(V3,I,ILand(IGetV(V1,I),IGetV(V2,I)));
     return TrimBigNum1(V3,L3);
 End;

lisp procedure BLNot(V1);
 BMinus BSmallAdd(V1,1);

lisp procedure BLShift(V1,V2);
% This seems a grimly inefficient way of doing things given that
% the representation of big numbers uses a base that is a power of 2.
% However it will do for now;
if BMinusP V2 then BQuotient(V1, BTwoPower BMinus V2)
  else BTimes2(V1, BTwoPower V2);



% -----------------------------------------
% Arithmetic Functions:
%
% U, V, V1, V2 are Bignum arguments.

lisp procedure BMinus V1;	% Negates V1.
 if BZeroP V1 then V1
  else begin scalar L1,V2;
	L1:=BSize V1;
	if BMinusP V1 then V2 := GtPOS L1
	 else V2 := GtNEG L1;
	IFor I:=1:L1 do IPutV(V2,I,IGetV(V1,I));
	return V2;
  end;

% Returns V1 if V1 is strictly less than 0, NIL otherwise.
%
lisp procedure BMinusP V1;
 if (IGetV(V1,0) eq 'BIGNEG) then V1 else NIL;

% To provide a conveninent ADD with CARRY.
lisp procedure AddCarry A;
 begin scalar S;
   S:=IPlus2(A,Carry!*);
   if IGeq(S,BBase!*) then <<Carry!*:= 1; S:=IDifference(S,BBase!*)>>
    else Carry!*:=0;
   return S;
 end;

lisp procedure BPlus2(V1,V2);
 begin scalar Sn1,Sn2;
     Sn1:=BMinusP V1;
     Sn2:=BMinusP V2;
     if Sn1 and Not Sn2 then return BDifference2(V2,BMinus V1,Nil);
     if Sn2 and Not Sn1 then return BDifference2(V1,BMinus V2,Nil);
     return BPlusA2(V1,V2,Sn1);
  end;

lisp procedure BPlusA2(V1,V2,Sn1);	% Plus with signs pre-checked and
 begin scalar L1,L2,L3,V3,temp;		% identical.
     L1:=BSize V1;
     L2:=BSize V2;
     If IGreaterP(L2,L1) then <<L3:=L2; L2:=L1;L1:=L3;
				V3:=V2; V2:=V1;V1:=V3>>;
     L3:=IAdd1 L1;
     If Sn1 then V3:=GtNeg L3
      else V3:=GtPOS L3;
     Carry!*:=0;
     IFor I:=1:L2 do <<temp:=IPlus2(IGetV(V1,I),IGetV(V2,I));
			IPutV(V3,I,AddCarry temp)>>;
     temp:=IAdd1 L2;
     IFor I:=temp:L1 do IPutV(V3,I,AddCarry IGetV(V1,I));
     IPutV(V3,L3,Carry!*); % Carry Out
     Return TrimBigNum1(V3,L3);
 end;

lisp procedure BDifference(V1,V2);
 if BZeroP V2 then V1
  else if BZeroP V1 then BMinus V2
  else begin scalar Sn1,Sn2;
     Sn1:=BMinusP V1;
     Sn2:=BMinusP V2;
     if (Sn1 and Not Sn2) or (Sn2 and Not Sn1) 
	then return BPlusA2(V1,BMinus V2,Sn1);
     return BDifference2(V1,V2,Sn1);
  end;

lisp procedure SubCarry A;
 begin scalar S;
  S:=IDifference(A,Carry!*);
  if ILessP(S,0) then <<Carry!*:=1; S:=IPlus2(BBase!*,S)>> else Carry!*:=0;
  return S;
 end;

Lisp procedure BDifference2(V1,V2,Sn1);  % Signs pre-checked and identical.
 begin scalar i,L1,L2,L3,V3;
  L1:=BSize V1;
  L2:=BSize V2;
  if IGreaterP(L2,L1) then <<L3:=L1;L1:=L2;L2:=L3;
			V3:=V1;V1:=V2;V2:=V3; Sn1:=not Sn1>>
   else if L1 Eq L2 then <<i:=L1;
		while (IGetV(V2,i) Eq IGetV(V1,i) and IGreaterP(i,1))
		  do i:=ISub1 i;
		if IGreaterP(IGetV(V2,i),IGetV(V1,i)) 
		   then <<L3:=L1;L1:=L2;L2:=L3;
			V3:=V1;V1:=V2;V2:=V3;Sn1:=not Sn1>> >>;
  if Sn1 then V3:=GtNEG L1
   else V3:=GtPOS L1;
  carry!*:=0;
  IFor I:=1:L2 do IPutV(V3,I,SubCarry IDifference(IGetV(V1,I),IGetV(V2,I)));
  IFor I:=(IAdd1 L2):L1 do IPutV(V3,I,SubCarry IGetV(V1,I));
  return TrimBigNum1(V3,L1);
 end;

lisp procedure BTimes2(V1,V2);
 begin scalar L1,L2,L3,Sn1,Sn2,V3;
    L1:=BSize V1;
    L2:=BSize V2;
    if IGreaterP(L2,L1)
	 then <<V3:=V1; V1:=V2; V2:=V3;   % If V1 is larger, will be fewer
		L3:=L1; L1:=L2; L2:=L3>>; % iterations of BDigitTimes2.
    L3:=IPlus2(L1,L2);
    Sn1:=BMinusP V1;
    Sn2:=BMinusP V2;
    If (Sn1 and Sn2) or not(Sn1 or Sn2) then V3:=GtPOS L3 else V3:=GtNEG L3;
    IFor I:=1:L3 do IPutV(V3,I,0);
    IFor I:=1:L2 do BDigitTimes2(V1,IGetV(V2,I),L1,I,V3);
    return TrimBigNum1(V3,L3);
  end;

Lisp procedure BDigitTimes2(V1,V2,L1,I,V3);
% V1 is a bignum, V2 a fixnum, L1=BSize L1, I=position of V2 in a bignum,
% and V3 is bignum receiving result.  I affects where in V3 the result of
% a calculation goes; the relationship is that positions I:I+(L1-1)
% of V3 receive the products of V2 and positions 1:L1 of V1.
% V3 is changed as a side effect here.
 begin scalar J,carry,temp1,temp2;
 if zerop V2 then return V3
  else <<
	carry:=0;
	IFor H:=1:L1 do <<
	    temp1:=ITimes2(IGetV(V1,H),V2);
	    temp2:=IPlus2(H,ISub1 I);
	    J:=IPlus2(IPlus2(temp1,IGetV(V3,temp2)),carry);
	    IPutV(V3,temp2,IRemainder(J,BBase!*));
	    carry:=IQuotient(J,BBase!*)>>;
	IPutV(V3,IPlus2(L1,I),carry)>>; % carry should be < BBase!* here 
    return V3;
 end;

Lisp procedure BSmallTimes2(V1,C);	% V1 is a BigNum, C a fixnum.
					% Assume C positive, ignore sign(V1)
					% also assume V1 neq 0.
 if ZeroP C then return GtPOS 0		% Only used from BHardDivide, BReadAdd.
  else begin scalar J,carry,L1,L2,L3,V3;
   L1:=BSize V1;
   L2:=IPlus2(IQuotient(C,BBase!*),L1);
   L3:=IAdd1 L2;
   V3:=GtPOS L3;
   carry:=0;
   IFor H:=1:L1 do <<
	J:=IPlus2(ITimes2(IGetV(V1,H),C),carry);
	IPutV(V3,H,IRemainder(J,BBase!*));
	carry:=IQuotient(J,BBase!*)>>;
   IFor H:=(IAdd1 L1):L3 do <<
	IPutV(V3,H,IRemainder(J:=carry,BBase!*));
        carry:=IQuotient(J,BBase!*)>>;
   return TrimBigNum1(V3,L3);
 end;

lisp procedure BQuotient(V1,V2);
 car BDivide(V1,V2);

lisp procedure BRemainder(V1,V2);
 cdr BDivide(V1,V2);

% BDivide returns a dotted pair, (Q . R).  Q is the quotient and R is 
% the remainder.  Both are bignums.  R is of the same sign as V1.
%;

smacro procedure BSimpleQuotient(V1,L1,C,SnC);
 car BSimpleDivide(V1,L1,C,SnC);

smacro procedure BSimpleRemainder(V1,L1,C,SnC);
 cdr BSimpleDivide(V1,L1,C,SnC);

lisp procedure BDivide(V1,V2);
 begin scalar L1,L2,Q,R,V3;
     L2:=BSize V2;
     If IZerop L2 then error(99, "Attempt to divide by 0 in BDIVIDE");
     L1:=BSize V1;
     If ILessP(L1,L2) or (L1 Eq L2 and ILessP(IGetV(V1,L1),IGetV(V2,L2)))
					% This also takes care of case
	then return (GtPOS 0 . V1);	% when V1=0.
     if IOnep L2 then return BSimpleDivide(V1,L1,IGetV(V2,1),BMinusP V2);
     return BHardDivide(V1,L1,V2,L2);
  end;


% C is a fixnum (inum?); V1 is a bignum and L1 is its length.
% SnC is T if C (which is positive) should be considered negative.
% Returns quotient . remainder; each is a bignum.
%
lisp procedure BSimpleDivide(V1,L1,C,SnC);
 begin scalar I,P,R,RR,Sn1,V2;
  Sn1:=BMinusP V1;
  if (Sn1 and SnC) or not(Sn1 or SnC) then V2:=GtPOS L1 else V2:=GtNEG L1;
  R:=0;
  I:=L1;
  While not IZeroP I do <<P:=IPlus2(ITimes2(R,BBase!*),IGetV(V1,I));
							% Overflow.
		    IPutV(V2,I,IQuotient(P, C));
		    R:=IRemainder(P, C);
		    I:=ISub1 I>>;
  If Sn1 then RR:=GtNeg 1 else RR:=GtPOS 1;
  IPutV(RR,1,R);
  return (TrimBigNum1(V2,L1) . TrimBigNum1(RR,1));
 end;


lisp procedure BHardDivide(U,Lu,V,Lv);
% This is an algorithm taken from Knuth.
 begin scalar U1,V1,A,D,LCV,LCV1,f,f2,J,K,Lq,carry,temp,
	      LL,M,N,N1,P,Q,QBar,SnU,SnV,U2;
     N:=Lv;
     N1:=IAdd1 N;
     M:=IDifference(Lu,Lv);
     Lq:=IAdd1 M;

     % Deal with signs of inputs;

     SnU:=BMinusP U;
     SnV:=BMinusp V;  % Note that these are not extra-boolean, i.e.
		      % for positive numbers MBinusP returns nil, for
		      % negative it returns its argument. Thus the
		      % test (SnU=SnV) does not reliably compare the signs of
		      % U and V;
     if SnU then if SnV then Q := GtPOS Lq else Q := GtNEG Lq
        else if SnV then Q := GtNEG Lq else Q := GtPOS Lq;

     U1 := GtPOS IAdd1 Lu;  % U is ALWAYS stored as if one digit longer;

     % Compute a scale factor to normalize the long division;
     D:=IQuotient(BBase!*,IAdd1 IGetV(V,Lv));
     % Now, at the same time, I remove the sign information from U and V
     % and scale them so that the leading coefficeint in V is fairly large;

     carry := 0;
     IFor i:=1:Lu do <<
	 temp := IPlus2(ITimes2(IGetV(U,I),D),carry);
	 IPutV(U1,I,IRemainder(temp,BBase!*));
	 carry := IQuotient(temp,BBase!*) >>;
     Lu := IAdd1 Lu;
     IPutV(U1,Lu,carry);

     V1:=BSmallTimes2(V,D);  % So far all variables contain safe values,
			     % i.e. numbers < BBase!*;
     IPutV(V1,0,'BIGPOS);

     if ILessp(Lv,2) then NonBigNumError(V,'BHARDDIVIDE); % To be safe;

     LCV := IGetV(V1,Lv);
     LCV1 := IGetv(V1,ISub1 Lv); % Top two digits of the scaled V accessed once
				 % here outside the main loop;

     % Now perform the main long division loop;

     IFor I:=0:M do <<
		J:=IDifference(Lu,I); 	        % J>K; working on U1[K:J] 
		K:=IDifference(J,N1);		% in this loop.
		A:=IGetV(U1,J);

		P := IPlus2(ITimes2(A,BBase!*),IGetv(U1,Isub1 J));
		   % N.B. P is up to 30 bits long. Take care! ;

		if A Eq LCV then QBar := ISub1 BBase!*
		else QBar := Iquotient(P,LCV);  % approximate next digit;

		f:=ITimes2(QBar,LCV1);
		f2:=IPlus2(ITimes2(IDifference(P,ITimes2(QBar,LCV)),BBase!*),
			   IGetV(U1,IDifference(J,2)));

		while IGreaterP(f,f2) do << % Correct most overshoots in Qbar;
			QBar:=ISub1 QBar;
			f:=IDifference(f,LCV1);;
		        f2:=IPlus2(f2,ITimes2(LCV,BBase!*)) >>;

		carry := 0;    % Ready to subtract QBar*V1 from U1;

		IFor L:=1:N do <<
		    temp := IPlus2(
				Idifference(
				   IGetV(U1,IPlus2(K,L)),
				   ITimes2(QBar,IGetV(V1,L))),
		                carry);
                    carry := IQuotient(temp,BBase!*);
		    temp := IRemainder(temp,BBase!*);
		    if IMinusp temp then <<
		       carry := ISub1 carry;
		       temp := IPlus2(temp,BBase!*) >>;
                    IPutV(U1,IPlus2(K,L),temp) >>;

		% Now propagate borrows up as far as they go;

                LL := IPlus2(K,N);
		while (not IZeroP carry) and ILessp(LL,J) do <<
		    LL := IAdd1 LL;
		    temp := IPlus2(IGetV(U1,LL),carry);
		    carry := IQuotient(temp,BBase!*);
		    temp := IRemainder(temp,BBase!*);
		    if IMinusP temp then <<
			carry := ISub1 carry;
			temp := IPlus2(temp,BBase!*) >>;
                    IPutV(U1,LL,temp) >>;

                if not IZerop carry then <<
		   % QBar was still wrong - correction step needed.
		   % This should not happen very often;
		   QBar := ISub1 QBar;

		   % Add V1 back into U1;
		   carry := 0;

		   IFor L := 1:N do <<
		       carry := IPlus2(
				   IPlus2(IGetV(U1,Iplus2(K,L)),
				          IGetV(V1,L)),
                                   carry);
                       IPutV(U1,IPlus2(K,L),IRemainder(carry,BBase!*));
		       carry := IQuotient(carry,BBase!*) >>;

                   LL := IPlus2(K,N);
		   while ILessp(LL,J) do <<
		       LL := IAdd1 LL;
		       carry := IPlus2(IGetv(U1,LL),carry);
		       IPutV(U1,LL,IRemainder(carry,BBase!*));
		       carry := IQuotient(carry,BBase!*) >> >>;

                IPutV(Q,IDifference(Lq,I),QBar)

		>>;        % End of main loop;


     U1 := TrimBigNum1(U1,IDifference(Lu,M));

     f := 0; f2 := 0; % Clean up potentially wild values;

     if not BZeroP U1 then <<
	% Unnormalize the remainder by dividing by D

        if SnU then IPutV(U1,0,'BIGNEG);
        if not IOnep D then <<
	    Lu := BSize U1;
	    carry := 0;
	    IFor L:=Lu step -1 until 1 do <<
	         P := IPlus2(ITimes2(carry,BBase!*),IGetV(U1,L));
	         IPutv(U1,L,IQuotient(P,D));
	         carry := IRemainder(P,D) >>;
     
	    P := 0;
	    if not IZeroP carry then BHardBug("remainder when unscaling",
	                            U,V,TrimBigNum1(U1,Lu),TrimBigNum1(Q,Lq));

	    U1 := TrimBigNum1(U1,Lu) >> >>;

     Q := TrimBigNum1(Q,Lq);     % In case leading digit happened to be zero;
     P := 0;  % flush out a 30 bit number;

% Here, for debugging purposes, I will try to validate the results I
% have obtained by testing if Q*V+U1=U and 0<=U1<V. I Know this slows things
% down, but I will remove it when my confidence has improved somewhat;

%    if not BZerop U1 then <<
%       if (BMinusP U and not BMinusP U1) or
%           (BMinusP U1 and not BMinusP U) then
%                  BHardBug("remainder has wrong sign",U,V,U1,Q) >>;
%    if not BAbs U1<BAbs V then BHardBug("remainder out of range",U,V,U1,Q)
%    else if not BZerop(BDifference(BPlus2(BTimes2(Q,V),U1),U)) then 
%         BHardBug("quotient or remainder incorrect",U,V,U1,Q);

     return (Q . U1)
  end;

lisp procedure BHardBug(msg,U,V,R,Q);
% Because the inputs to BHardDivide are probably rather large, I am not
% going to rely on BldMsg to display them;
 << Prin2T "***** Internal error in BHardDivide";
    Prin2 "arg1="; Prin2T U;
    Prin2 "arg2="; Prin2T V;
    Prin2 "computed quotient="; Prin2T Q;
    Prin2 "computed remainder="; Prin2T R;
    StdError msg >>;


lisp procedure BGreaterP(U,V);
    if BMinusP U then
       if BMinusP V then BUnsignedGreaterP(V,U)
       else nil
    else if BMinusP V then U
       else BUnsignedGreaterP(U,V);

lisp procedure BLessp(U,V);
    if BMinusP U then
       if BMinusP V then BUnsignedGreaterP(U,V)
       else U
    else if BMinusP V then nil
       else BUnsignedGreaterP(V,U);

lisp procedure BGeq(U,V);
    if BMinusP U then
       if BMinusP V then BUnsignedGeq(V,U)
       else nil
    else if BMinusP V then U
       else BUnsignedGeq(U,V);

lisp procedure BLeq(U,V);
    if BMinusP U then
       if BMinusP V then BUnsignedGeq(U,V)
       else U
    else if BMinusP V then nil
       else BUnsignedGeq(V,U);

lisp procedure BUnsignedGreaterP(U,V);
% Compare magnitudes of two bignums;
  begin
    scalar Lu,Lv,I;
    Lu := BSize U;
    Lv := BSize V;
    if not (Lu eq Lv) then <<
       if IGreaterP(Lu,Lv) then return U
       else return nil >>;
    while IGetV(U,Lv) eq IGetV(V,Lv) and IGreaterP(Lv,1) do Lv := ISub1 Lv;
    if IGreaterP(IGetV(U,Lv),IGetV(V,Lv)) then return U
    else return nil
  end;

symbolic procedure BUnsignedGeq(U,V);
% Compare magnitudes of two unsigned bignums;
  begin
    scalar Lu,Lv;
    Lu := BSize U;
    Lv := BSize V;
    if not (Lu eq Lv) then <<
       if IGreaterP(Lu,Lv) then return U
       else return nil >>;
    while IGetV(U,Lv) eq IGetV(V,Lv) and IGreaterP(Lv,1) do Lv := ISub1 Lv;
    If IGreaterP(IGetV(V,Lv),IGetV(U,Lv)) then return nil
    else return U
  end;



lisp procedure BAdd1 V;
 BSmallAdd(V,1);

lisp procedure BSub1 U;
 BSmallDiff(U,1);

% ------------------------------------------------
% Conversion to Float:

lisp procedure FloatFromBigNum V;
 if BZeroP V then 0.0
  else if BGreaterP(V, FloatHi!*) or BLessp(V, FloatLow!*) 
	then Error(99,list("Argument, ",V," to FLOAT is too large"))
  else begin scalar L,Res,Sn,I;
    L:=BSize V;
    Sn:=BMinusP V;
    Res:=float IGetv(V,L);
    I:=ISub1 L;
    While not IZeroP I do << Res:=res*BBase!*;
		            Res:=Res +IGetV(V,I);
			    I:=ISub1 I>>;
    if Sn then Res:=minus res;
    return res;
  end;


% ------------------------------------------------
% Input and Output:
Digit2Letter!* :=		% Ascii values of digits and characters.
'[48 49 50 51 52 53 54 55 56 57 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
80 81 82 83 84 85 86 87 88 89 90];

% OutputBase!* is assumed to be positive and less than 37.

lisp procedure BChannelPrin2(Channel,V);
 If not BignumP V then NonBigNumError(V, 'BPrin) %need?
  else begin scalar quot, rem, div, result, resultsign, myobase;
   myobase:=OutputBase!*;
   resultsign:=BMinusP V;
   div:=BSimpleDivide(V,Bsize V,OutputBase!*,nil);
   quot:=car div;
   rem:=cdr div;
   if Bzerop rem then rem:=0 else rem:=IGetV(rem,1);
   result:=rem . result;
   while Not BZeroP quot do
	<<div:=BSimpleDivide(quot,Bsize quot,OutputBase!*,nil);
	quot:=car div;
	rem:=cdr div;
	if Bzerop rem then rem:=0 else rem:=IGetV(rem,1);
	result:=rem . result>>;
   if resultsign then channelwritechar(Channel,char !-);
   if myobase neq 10 then <<ChannelWriteSysInteger(channel,myobase,10);
			ChannelWriteChar(Channel, char !#)>>;
   For each u in result do ChannelWriteChar(Channel, IGetV(digit2letter!*,u));
   OutputBase!*:=myobase;
   return;
  end;

lisp procedure BRead(s,radix,sn);	% radix is < Bbase!*
			%s=string of digits, radix=base, sn=1 or -1
 begin scalar sz, res, ch;
  sz:=size s;
  res:=GtPOS 1;
  ch:=indx(s,0);
  if IGeq(ch,char A) and ILeq(ch,char Z)
		then ch:=IPlus2(IDifference(ch,char A),10);
  if IGeq(ch,char 0) and ILeq(ch,char 9) 
		then ch:=IDifference(ch,char 0);
  IPutV(res,1,ch);
  IFor i:=1:sz do <<ch:=indx(s,i);
		if IGeq(ch,char A) and ILeq(ch,char Z)
			then ch:=IDifference(ch,IDifference(char A,10));
		if IGeq(ch,char 0) and ILeq(ch,char 9)
			then ch:=IDifference(ch,char 0);
		res:=BReadAdd(res, radix, ch)>>;
  if iminusp sn then res:=BMinus res;
  return res;
 end;

lisp procedure BReadAdd(V, radix, ch);
  << V:=BSmallTimes2(V, radix);
     V:=BSmallAdd(V,ch)>>;

lisp procedure BSmallAdd(V,C);	%V big, C fix.
 if IZerop C then return V
  else if Bzerop V then return int2B C
  else if BMinusp V then BMinus BSmallDiff(BMinus V, C)
  else if IMinusP C then BSmallDiff(V, IMinus C)
  else begin scalar V1,L1;
   Carry!*:=C;
   L1:=BSize V;
   V1:=GtPOS(IAdd1 L1);
   IFor i:=1:L1 do IPutV(V1,i,addcarry IGetV(V,i));
   if IOneP carry!* then IPutV(V1,IAdd1 L1,1) else return TrimBigNum1(V1,L1);
   return V1
  end;

lisp procedure BNum N;	% temporary?  Creates a Bignum of one digit, value N.
 begin scalar B;
  if IZerop n then return GtPOS 0
   else if IMinusp N then <<b:=GtNEG 1; n:= IMinus n>> else b:=GtPos 1;
  IPutV(b,1,N);
  Return b;
 end;

lisp procedure BSmallDiff(V,C);	%V big, C fix
 if IZerop C then V
  else if BZeroP V then int2B IMinus C
  else if BMinusP V then BMinus BSmallAdd(BMinus V, C)
  else if IMinusP C then BSmallAdd(V, IMinus C)
  else begin scalar V1,L1;
   Carry!*:=C;
   L1:=BSize V;
   V1:=GtPOS L1;
   IFor i:=1:L1 do IPuTV(V1,i,subcarry IGetV(V,i));
   if not IZeroP carry!* then
      StdError BldMsg(" BSmallDiff V<C %p %p%n",V,C);
   return TrimBigNum1(V1,L1);
  end;

lisp procedure int2B n;		% Temporary?  Creates BigNum of value N.
 if not fixp n then NonIntegerError(n, 'int2B)
  else if ILessP(n,Bbase!*) then BNum n
  else begin scalar Str,ind,rad,Sn,r;
   Str:=bldmsg("%w",n);		% like an "int2string"
   if indx(str,0)=char '!- then <<Sn:=-1;
	str:=sub(str,1,ISub1 (size str))>>
    else Sn:=1;
   IFor i:=0:size str do
	if indx(str,i)=char '!# then ind:=i;
   if ind then <<r:=sub(str,0,ISub1 ind);
		rad:=0;
		IFor i:=0:size r do
		  rad:=IPlus2(ITimes2(rad,10),IDifference(indx(r,i),char 0));
		str:=sub(str,IAdd1 ind,IDifference(size str,IAdd1 ind))>>
    else rad:=10;
   return Bread(str,rad,sn);
  end;

%-----------------------------------------------------
% "Fix" for Bignums

lisp procedure bigfromfloat X;
 if fixp x or bigp x then x
  else begin scalar bigpart,floatpart,power,sign,thispart;
     if minusp X then <<sign:=-1; X:=minus X>> else sign:=1;
     bigpart:=bnum 0;
     while neq(X, 0) and neq(x,0.0) do <<
	if X < bbase!* then << bigpart:=bplus2(bigpart, bnum fix x);
				X:=0 >>
	 else <<floatpart:=x;
		power:=0;
		while floatpart>=bbase!* do	% get high end of number.
			<<floatpart:=floatpart/bbase!*;
			power:=power + bbits!* >>;
		thispart:=btimes2(btwopower power, bnum fix floatpart);
		X:=X- floatfrombignum thispart;
		bigpart:=bplus2(bigpart, thispart) >> >>;
     if minusp sign then bigpart := bminus bigpart;
     return bigpart;
  end;

if_system(VAX, 
	<<setbits 32;
	FloatHi!*:=btimes2(bdifference(btwopower 67, btwopower 11), 
			btwopower 60);% Largest representable float.
	FloatLow!*:=BMinus FloatHi!*>>);

if_system(PDP10,
	<<setbits 36;
	FloatHi!*:=btimes2(bsub1 btwopower 62, btwopower 65);
	FloatLow!*:=BMinus FloatHi!*>>);

% End of BIGBIG.RED ;


