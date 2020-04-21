
% WRITE-REAL.RED - Procedure to write a floating point number
%  Author: Martin Griss
%  Date:   ~July 1982.

% Notes by Maguire on 27.7.82:
% Original version will use ~18K bytes for it's tables on the Apollo 
% due to the large exponent allowed.

% See the common lisp manual, for names for base-B operations;
% and constants for a re-write of this, to handle rounding etc.

% Algorithm: By searching a table of powers of 10, previously
%            set up in a vector, determine
%            the Exponent and Mantissa of the given Float.
%            Then convert the mantissa to a pair of integers
%            and finally assembly the printed form as a string


Fluid '(FltZero!*   % Representation of 0.0
        FltTen!*  %                  10.0
        FltExponents          % vector of (10^n)     
	MinFltExponent        % range of Exponents in table
        MaxFltExponent
        MaxFlt
        MinFlt
        MaxFltDigits          % Maximum number of digits of precision
        FltDigits             % Digits 0.0 ... 9.0
);

Procedure InitWriteFloats(MinEx,MaxEx,NDig);
 % Declare Maximum Number of Exponents and Digits
 Begin scalar Flt1,Flt!.1; 
  FltZero!* := Float(0);
  Flt1 := Float(1);
  FltTen!* :=Float(10);
  Flt!.1 := Flt1/FltTen!*;
  MinFltExponent :=MinEx;
  MaxFltExponent:=MaxEx;
  NumberOfExponents := MaxEx-MinEx; % For UpLim on vector.
  MaxFltDigits:=Ndig;
  FltDigits:=MkVect 9;
  For I:=0:9 do FltDigits[I]:=Float I;
  FltExponents:=MkVect(NumberOfExponents);
  FltExponents[-MinEx]:=Flt1;
  FltExponents[1-Minex]:=FltTen!*;
  FltExponents[-1-Minex]:=Flt!.1;
  For i:=2-Minex:NumberOfExponents 
     do FltExponents[i] := FltTen!* *   FltExponents[i-1];
  For i:=-2-MinEx Step -1 Until 0 
     do FltExponents[i] := Flt!.1 *   FltExponents[i+1];
  MinFlt := FltExponents[0];
  MaxFlt := FltExponents[NumberOfExponents];
end;

InitWriteFloats(-10,10,10);

Procedure FindExponent(Flt);
% return Exponent as Integer
% First reduce Flt to table range then search.
% Should Be Primitive, and done in Appropriate Float Base (2, or 16?)
If Flt=FltZero!* then 0
 else if Flt <FltZero!* then FindExponent(-Flt)
 else
  Begin scalar N;
   If Flt >= MaxFlt then
     return(MaxFltExponent+FindExponent(Flt/MaxFlt));
   If Flt <= MinFlt then
     return(MinFltExponent+FindExponent(Flt/MinFlt));
   N:=0;
   While N < NumberOfExponents and FltExponents[N] < Flt do N:=N+1;
   Return (N+MinFltExponent);
 End;

Procedure FindMantissa(Flt);
% return Mantissa as a (signed)float in [0.0 ..1.0)
  Flt/FloatPower10(FindExponent(Flt));

Procedure FloatPower10(n);
 % Returns 1FltZero!*^n, using table
 If N>MaxFltExponent 
    then MaxFlt*FloatPower10(n-MaxFltExponent)
  else if N<MinFltExponent then MinFlt*FloatPower10(n-MinFltExponent)
  else FltExponents[n-MinFltExponent];

Procedure Flt2String(Flt); 
  ScaledFloat2String(Flt,MaxFltDigits,0,-3,3);

Procedure ScaledFloat2String(Flt,Ndigits,Scale, MinNice,MaxNice);
 % "print" a float, either in IIII.FFFF format, or SS.FFFFFeN
 %  First format, if MinNice <=N<=MaxNice
 %  ss controlled by Scale if second chosen
 %
 Begin Scalar Fsign,Fex,Fdigits,K,N,Flist,Ilist;
     If Flt = FltZero!* then return "0.0";
     If Flt < FltZero!* then <<Fsign:='T; Flt:=-Flt>>;
     Fex:=FindExponent(Flt);
     Flt:=Flt/FloatPower10(Fex); % Ie, FindMantissa

   % At this point,
   %  FEX is an integer
   %  and 0.0 =< Flt <1.0

   % Now we can move the Point and adjust the Exponent by a scale
   % factor for "nicety", or to eliminate En
  
   If Fex>=MinNice and Fex<=maxNice then
      <<Flt:=Flt*FloatPower10(Fex);
        Fex:=0>>
    else if scale neq 0 then
      <<Flt:=Flt*FloatPower10(Scale); 
        Fex:=Fex-Scale>>;

   % Remove and convert the Integer Part (0 if scale=0 and not-nice).

     Ilist:=Fix(Flt);  
     Flt:=Flt-Float(Ilist);
     If Fsign then Ilist:=-Ilist;
     Ilist:=Char('!.) . Reverse Int2List Ilist;  % Reverse 

   % Start shifting off digits in fraction by multiplying by 10
   % Also Round here.
   % Should we adjust Ndigits if "nice/scale" ??

     Flist:=Ilist;  % Add in fraction digits, remember point for trailing
                    % Zero Removal

     For K:=1:NDigits do
      << Flt := Flt * FltTen!*;
         N:=Fix(Flt);
         Flt:=Flt-FltDigits[N];
         Flist := (N + Char '0) . Flist;
     >>;

  % Truncate excess trailing 0's
     While PairP Flist and Not (Cdr Flist eq Ilist) 
         and Car(Flist)=Char '0
	    do Flist:=cdr Flist;

% Now Optimize format, omitting En if 0
     If Fex=0 then Return List2String Reverse Flist;

% Now convert the Exponent and Insert
     Fex:=Int2List Fex;
     Flist := Char('E) . Flist; % The "E"

     For each x in Fex do Flist:= x . Flist;
     Return List2String Reverse Flist;
 end;

procedure Int2String N;
% Convert signed integer into a string
   List2String Int2List N;

Procedure Int2List N;
 % Return "exploded" number, forward order
 Begin scalar L,Nsign;
   If N=0 then return List Char '0;
   If N<0 then <<N := -N; Nsign :=T>>;
   While N>0 do
    <<L := (Remainder(N,10) + Char '!0 ) . L;
      N := N / 10>>;
   If Nsign then L := Char('!-) . L;
   Return L;
 End;


%Syslsp Procedure WriteFloat(Buffer,Fbase);
% Buffer is Wstring[0..40],
% Fbase  is FloatBase FltInf Flt
% Begin Scalar s,flt,i,ss;
%  flt := MKFLTN (Fbase-4); %/4 or 1
%  s:=Flt2String flt;
%  ss:=strinf(s);
%  i:=strlen(ss);
%  strlen(Buffer):=i;
%  i:=i+1;
%  while i>=0 do <<strbyt(Buffer,i) := StrByt(ss,i);
%                  i:=i-1>>;
% end;

End;
