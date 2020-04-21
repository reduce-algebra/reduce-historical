%. MATHLIB.RED - Some useful mathematical functions for PSL
%
% Most of these routines not very heavily tested. 
% Contributions from Galway, Griss, Irish, Morrison, and others.
%
%  MATHLIB.RED, 16-Dec-82 21:56:52, Edit by GALWAY
%   Various fixes and enhancements too numerous for me to remember.
%   Includes fixes in SQRT function, modifications of RANDOM and other
%   functions to bring them more in line with Common Lisp, addition of MOD
%   and FLOOR.
%  <PSL.UTIL>MATHLIB.RED.13, 13-Sep-82 08:49:52, Edit by BENSON
%  Bug in EXP, changed 2**N to 2.0**N
%  <PSL.UTIL>MATHLIB.RED.12,  2-Sep-82 09:22:19, Edit by BENSON
%  Changed all calls in REDERR to calls on STDERROR
%  <PSL.UTIL>MATHLIB.RED.2, 17-Jan-82 15:48:21, Edit by GRISS
%  changed for PSL

% Should these names be changed so that they all begin with an F or some
% other distinguishing mark?  Are they in conflict with anything?  Or should
% we wait until we have packages?

% Consider using Sasaki's BigFloat package -- it has all this and more, to
% arbitrary precision.  The only drawback is speed.

%***************** Constants declared as NewNam's ****************************

% We can't use these long ones in Lisp1.6 'cause the reader craps out (and
% it would truncate instead of round, anyway).  These are here for reference
% for implementation on other machines.
% put('NumberPi,'NewNam,3.14159265358979324);
% put('NumberPi!/2,'NewNam,1.57079632679489662);
% put('NumberPi!/4,'NewNam,0.785398163397448310);

BothTimes <<
put('Number2Pi,'NewNam,6.2831853);
put('NumberPi,'NewNam,3.1415927);
put('NumberPi!/2,'NewNam,1.5707963);
put('NumberPi!/4,'NewNam,0.78539816);
put('Number3Pi!/4,'NewNam,2.3561945);
put('Number!-2Pi,'Newnam,-6.2831853);
put('Number!-Pi,'NewNam,-3.1415927);
put('Number!-Pi!/2,'NewNam,-1.5707963);
put('Number!-Pi!/4,'NewNam,-0.78539816);

put('SqrtTolerance,'NewNam,0.0000001);
put('NumberE, 'NewNam, 2.718281828);
put('NumberInverseE, 'NewNam, 0.36787944);     % 1/e
put('NaturalLog2,'NewNam,0.69314718);
put('NaturalLog10,'NewNam,2.3025851);
put('TrigPrecisionLimit,'NewNam,80);

>>;
%********************* Basic functions ***************************************

lisp procedure mod(M,N);
% Return M modulo N.  Unlike remainder function--it returns positive result
% in range 0..N-1, even if M is negative.  (Needs more work for case of
% negative N.)
begin scalar result;
    result := remainder(M,N);
    if result >= 0 then
        return result;
    % else
    return
        N + result;
end;

lisp procedure Floor X;
% Returns the largest integer less than or equal to X.  (I.e. the "greatest
% integer" function.)
if fixp X then
  X
else begin scalar N;
  N := fix X;
  % Note the trickiness to compensate for fact that (unlike APL's "FLOOR"
  % function) FIX truncates towards zero.
  return if X = float N then N else if X>=0 then N else N-1;
end;

lisp procedure Ceiling X;
% Returns the smallest integer greater than or equal to X.
if fixp X then
  X
else begin scalar N;
  N := fix X;
  % Note the trickiness to compensate for fact that (unlike APL's "FLOOR"
  % function) FIX truncates towards zero.
  return if X = float N then N else if X>0 then N+1 else N;
end;

lisp procedure Round X;
% Rounds to the closest integer.
% Kind of sloppy -- it's biased when the digit causing rounding is a five,
% it's a bit weird with negative arguments, round(-2.5)= -2.
if fixp X then
  X
else 
  floor(X+0.5);

%***************** Trigonometric Functions ***********************************

% Trig functions are all in radians.  The following few functions may be used
% to convert to/from degrees, or degrees/minutes/seconds.

lisp procedure DegreesToRadians x;
x*0.017453292; % 2*pi/360

lisp procedure RadiansToDegrees x;
  x*57.29578;    % 360/(2*pi)

lisp procedure RadiansToDMS x;
% Converts radians to a list of degrees, minutes, and seconds (rounded, not
% truncated, to the nearest integer).
begin scalar Degs,Mins;
  x := RadiansToDegrees x;
  Degs := fix x;
  x := 60*(x-Degs);
  Mins := fix x;
  return list(Degs,Mins, Round(60*(x-Mins)))
end;

lisp procedure DMStoRadians(Degs,Mins,Sex);
% Converts degrees, minutes, seconds to radians.
% DegreesToRadians(Degs+Mins/60.0+Sex/3600.0)
DegreesToRadians(Degs+Mins*0.016666667+Sex*0.00027777778);

lisp procedure sin x;
% Accurate to about 6 decimal places, so long as the argument is 
% of commensurate precision.  This will, of course, NOT be true for
% large arguments, since they will be coming in with small precision.
begin scalar neg;
  if minusp x then <<
    neg := T;
    x := - x >>;
  if x > TrigPrecisionLimit then
    LPriM "Possible loss of precision in computation of SIN";
  if x > NumberPi then
    x := x-Number2Pi*fix((x+NumberPi)/Number2Pi);
  if minusp x then <<
    neg := not neg;
    x :=  -x >>;
  if x > NumberPi!/2 then
    x := NumberPi-x;
  return if neg then -ScaledSine x else ScaledSine x
end;

lisp procedure ScaledSine x;
% assumes its argument is scaled to between 0 and pi/2.
begin scalar xsqrd;
  xsqrd := x*x;
  return x*(1+xsqrd*(-0.16666667+xsqrd*(0.0083333315+xsqrd*(-0.0001984090+
              xsqrd*(0.0000027526-xsqrd*0.0000000239)))))
end;

lisp procedure cos x;
% Accurate to about 6 decimal places, so long as the argument is 
% of commensurate precision.  This will, of course, NOT be true for
% large arguments, since they will be coming in with small precision.
<< if minusp x then
     x := - x;
   if x > TrigPrecisionLimit then
     LPriM "Possible loss of precision in computation of COS";
   if x > NumberPi then
     x := x-Number2Pi*fix((x+NumberPi)/Number2Pi);
   if minusp x then
     x := - x;
   if x > NumberPi!/2 then
     -ScaledCosine(NumberPi-x)
   else
     ScaledCosine x >>;

lisp procedure ScaledCosine x;
% Expects its argument to be between 0 and pi/2.
begin scalar xsqrd;
  xsqrd := x*x;
  return 1+xsqrd*(-0.5+xsqrd*(0.041666642+xsqrd*(-0.0013888397+
              xsqrd*(0.0000247609-xsqrd*0.0000002605))))
end;

lisp procedure tan x;
% Accurate to about 6 decimal places, so long as the argument is 
% of commensurate precision.  This will, of course, NOT be true for
% large arguments, since they will be coming in with small precision.
begin scalar neg;
  if minusp x then <<
    neg := T;
    x := - x >>;
  if x > TrigPrecisionLimit then
    LPriM "Possible loss of precision in computation of TAN";
  if x > NumberPi!/2 then
    x := x-NumberPi*fix((x+NumberPi!/2)/NumberPi);
  if minusp x then <<
    neg := not neg;
    x := - x >>;
  if x < NumberPi!/4 then
    x := ScaledTangent x
  else
    x := ScaledCotangent(-(x-numberpi!/2));
  return if neg then -x else x
end;

lisp procedure cot x;
% Accurate to about 6 decimal places, so long as the argument is 
% of commensurate precision.  This will, of course, NOT be true for
% large arguments, since they will be coming in with small precision.
begin scalar neg;
  if minusp x then <<
    neg := T;
    x := - x >>;
  if x > NumberPi!/2 then
    x := x-NumberPi*fix((x+NumberPi!/2)/NumberPi);
  if x > TrigPrecisionLimit then
    LPriM "Possible loss of precision in computation of COT";
  if minusp x then <<
    neg := not neg;
    x := - x >>;
  if x < NumberPi!/4 then
    x := ScaledCotangent x
  else
    x := ScaledTangent(-(x-numberpi!/2));
  return if neg then -x else x
end;

lisp procedure ScaledTangent x;
% Expects its argument to be between 0 and pi/4.
begin scalar xsqrd;
  xsqrd := x*x;
  return x*(1.0+xsqrd*(0.3333314+xsqrd*(0.1333924+xsqrd*(0.05337406 +
           xsqrd*(0.024565089+xsqrd*(0.002900525+xsqrd*0.0095168091))))))
end;

lisp procedure ScaledCotangent x;
% Expects its argument to be between 0 and pi/4.
begin scalar xsqrd;
  xsqrd := x*x;
  return (1.0-xsqrd*(0.33333334+xsqrd*(0.022222029+xsqrd*(0.0021177168 +
           xsqrd*(0.0002078504+xsqrd*0.0000262619)))))/x
end;

lisp procedure sec x;
1.0/cos x;

lisp procedure csc x;
1.0/sin x;

lisp procedure sinD x;
sin DegreesToRadians x;

lisp procedure cosD x;
cos DegreesToRadians x;

lisp procedure tanD x;
tan DegreesToRadians x;

lisp procedure cotD x;
cot DegreesToRadians x;

lisp procedure secD x;
sec DegreesToRadians x;

lisp procedure cscD x;
csc DegreesToRadians x;

lisp procedure asin x;
begin scalar neg;
  if minusp x then <<
    neg := T;
    x := -x >>;
  if x > 1.0 then
    stderror list("Argument to ASIN too large:",x);
  return if neg then CheckedArcCosine x - NumberPi!/2 
		else NumberPi!/2 - CheckedArcCosine x
end;

lisp procedure acos x;
begin scalar neg;
  if minusp x then <<
    neg := T;
    x := -x >>;
  if x > 1.0 then
    stderror list("Argument to ACOS too large:",x);
  return if neg then NumberPi - CheckedArcCosine x
		else CheckedArcCosine x
end;

lisp procedure CheckedArcCosine x;
% Return cosine of a "checked number", assumes its argument is in the range
% 0 <= x <= 1.
sqrt(1.0-x)*(1.5707963+x*(-0.2145988+x*(0.088978987+x*(-0.050174305+
        x*(0.030891881+x*(-0.017088126+x*(0.0066700901-x*(0.0012624911))))))));

lisp procedure atan x;
if minusp x then
  if x < -1.0 then
    Number!-Pi!/2 + CheckedArcTangent(-1.0/x)
  else
    -CheckedArcTangent(-x)
else
  if x > 1.0 then
    NumberPi!/2 - CheckedArcTangent(1.0/x)
  else
    CheckedArcTangent x;

lisp procedure acot x;
if minusp x then
  if x < -1.0 then
    -CheckedArcTangent(-1.0/x)
  else
    Number!-Pi!/2 + CheckedArcTangent(-x)
else
  if x > 1.0 then
   CheckedArcTangent(1.0/x)
  else
    NumberPi!/2 - CheckedArcTangent x;

lisp procedure CheckedArcTangent x;
begin scalar xsqrd;
  xsqrd := x*x;
  return x*(1+xsqrd*(-0.33333145+xsqrd*(0.19993551+xsqrd*(-0.14208899+
             xsqrd*(0.10656264+xsqrd*(-0.07528964+xsqrd*(0.042909614+
	     xsqrd*(-0.016165737+xsqrd*0.0028662257))))))))
end;

lisp procedure asec x;
acos(1.0/x);

lisp procedure acsc x;
asin(1.0/x);

lisp procedure asinD x;
RadiansToDegrees asin x;

lisp procedure acosD x;
RadiansToDegrees acos x;

lisp procedure atanD x;
RadiansToDegrees atan x;

lisp procedure acotD x;
RadiansToDegrees acot x;

lisp procedure asecD x;
RadiansToDegrees asec x;

lisp procedure acscD x;
RadiansToDegrees acsc x;

%****************** Roots and such *******************************************

lisp procedure sqrt N;
% Simple Newton-Raphson floating point square root calculator.
% Not waranted against truncation errors, etc.
begin integer answer,scale;
  N:=FLOAT N;
  if N < 0.0 then stderror list("SQRT given negative argument:",N);
  if zerop N then
    return N;
  % Scale argument to within 1e-10 to 1e+10;
  scale := 0;
  while N > 1.0E10 do
  <<
    scale := scale + 1;
    N := N * 1.0E-10 >>;
  while N < 1.0E-10 do
  <<
    scale := scale - 1;
    N := N * 1.0E10 >>;
  answer := if N>2.0 then (N+1)/2
         else if N<0.5 then 2/(N+1)
         else N;

  % Here's the heart of the algorithm.
  while abs(answer**2/N - 1.0) > SqrtTolerance do
    answer := 0.5*(answer+N/answer);
  return answer * 10.0**(5*scale)
end;

%******************** Logs and Exponentials **********************************

lisp procedure exp x;
% Returns the exponential (ie, e**x) of its floatnum argument as
% a flonum. The argument is scaled to
% the interval -ln  2 to  0, and a  Taylor series  expansion
% used (formula 4.2.45 on page 71 of Abramowitz and  Stegun,
% "Handbook of Mathematical  Functions").
begin scalar N;
  N := ceiling(x / NaturalLog2);
  x := N * NaturalLog2 - x;
  return 2.0**N * (1.0+x*(-0.9999999995+x*(0.4999999206+x*(-0.1666653019+
        x*(0.0416573475+x*(-0.0083013598+x*(0.0013298820+
        x*(-0.0001413161))))))))
end;


lisp procedure log x;
% See Abramowitz and Stegun, page 69.

 if x <= 0.0 then
   stderror list("LOG given non-positive argument:",x)
 else if x < 1.0 then
   -log(1.0/x)
 else
 % Find natural log of x > 1;
 begin scalar nextx, ipart;      % ipart is the "integer part" of the
                                 % logarithm.
   ipart := 0;

   % Keep multiplying by 1/e until x is small enough, may want to be more
   % "efficient" if we ever use really big numbers.
   while (nextx := NumberInverseE * x) > 1.0 do
   <<
       x := nextx;
       ipart := ipart + 1;
   >>;

   return
       ipart +
       if x < 2.0 then
         CheckedLogarithm x
       else
         2.0 * CheckedLogarithm(sqrt(x));
 end;
 
lisp procedure CheckedLogarithm x;
% Should have 1 <= x <= 2.  (i.e. x = 1+y  0 <= y <= 1)
<< x := x-1.0;
    x*(0.99999642+x*(-0.49987412+x*(0.33179903+x*(-0.24073381+x*(0.16765407+
         x*(-0.09532939+x*(0.036088494-x*0.0064535442))))))) >>;

lisp procedure log2 x;
log x / NaturalLog2;

lisp procedure log10 x;
log x / NaturalLog10;

%********************* Random Number Generator *******************************

% The declarations below  constitute a linear,  congruential
% random number generator (see  Knuth, "The Art of  Computer
% Programming: Volume 2: Seminumerical Algorithms", pp9-24).
% With the given  constants it  has a period  of 392931  and
% potency  6.    To   have  deterministic   behaviour,   set
% RANDOMSEED.
%
% Constants are:        6   2
%    modulus: 392931 = 3 * 7 * 11
%    multiplier: 232 = 3 * 7 * 11 + 1
%    increment: 65537 is prime
%
% Would benefit from being recoded in SysLisp, when full word integers should
% be used with "automatic" modular arithmetic (see Knuth).  Perhaps we should
% have a longer period version?
% By E. Benson, W. Galway and M. Griss

fluid '(RandomSeed RandomModulus);

RandomModulus := 392931;
RandomSeed := remainder(time(),RandomModulus);

lisp procedure next!-random!-number;
% Returns a pseudo-random number between 0 and RandomModulus-1 (inclusive).
RandomSeed := remainder(232*RandomSeed + 65537, RandomModulus);

lisp procedure Random(N);
% Return a pseudo-random number uniformly selected from the range 0..N-1.
% NOTE that this used to be called RandomMod(N).  Needs to be made more
% compatible with Common LISP's random?
    fix( (float(N) * next!-random!-number()) / RandomModulus);

procedure FACTORIAL N;   % Simple factorial
 Begin scalar M;
    M:=1;
    for i:=1:N do M:=M*I;
    Return M;
 end;


% Some functions from ALPHA_1 users

lisp procedure Atan2D( Y, X );
    RadiansToDegrees Atan2( Y, X );

lisp procedure Atan2( Y, X );
<<
    X := float X; Y := Float Y;

    if X = 0.0 then			% Y axis.
	if  Y >= 0.0  then  NumberPI!/2  else  NumberPi + NumberPI!/2

    else if X >= 0.0 and Y >= 0.0 then	% First quadrant.
	Atan( Y / X )

    else if X < 0.0 and Y >= 0.0 then	% Second quadrant.
	NumberPI - Atan( Y / -X )

    else if X < 0.0 and Y < 0.0 then	% Third quadrant.
	NumberPI + Atan( Y / X )

    else				% Fourth quadrant.
	Number2Pi - Atan( -Y / X )
>>;

lisp procedure TransferSign( S, Val );
% Transfers the sign of S to Val by returning abs(Val) if S >= 0,
% otherwise -abs(Val).
    if S >= 0 then abs(Val) else -abs(Val);

lisp procedure DMStoDegrees(Degs,Mins,Sex);
% Converts degrees, minutes, seconds to degrees
% Degs+Mins/60.0+Sex/3600.0
    Degs+Mins*0.016666667+Sex*0.00027777778;

lisp procedure DegreesToDMS x;
% Converts degrees to a list of degrees, minutes, and seconds (all integers,
% rounded, not truncated).
begin scalar Degs,Mins;
  Degs := fix x;
  x := 60*(x-Degs);
  Mins := fix x;
  return list(Degs,Mins, round(60*(x-Mins)))
end;

end;
