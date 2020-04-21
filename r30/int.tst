COMMENT

                 THE REDUCE INTEGRATION TEST PACKAGE

                              Edited By

                           Anthony C. Hearn
                         The Rand Corporation


This file is designed to provide a set of representative tests of the
Reduce integration package.  Not all examples go through, even when an
integral exists, since some of the arguments are outside the domain of
applicability of the current package.  However, future improvements to
the package will result in more closed-form evaluations in later
releases.  We would appreciate any additional contributions to this test
file either because they illustrate some feature (good or bad) of the
current package, or suggest domains which future versions should handle.
Any suggestions for improved organization of this test file (e.g., in a
way which corresponds more directly to the organization of a standard
integration table book such as Gradshteyn and Ryznik) are welcome.

Acknowledgments:

The examples in this file have been contributed by the following.
Any omissions to this list should be reported to the Editor.

David M. Dahm
John P. Fitch
Steven Harrington
Anthony C. Hearn
K. Siegfried Koelbig
Ernst Krupnikov
Arthur C. Norman
Herbert Stoyan
;

Comment we first set up a suitable testing function;

SYMBOLIC OPERATOR TIME;

PROCEDURE TESTINT(A,B);
  BEGIN SCALAR DIFFCE,RES,TT;
      TT:=TIME();
      RES:=INT(A,B);
      WRITE "Time for Integral:  ",TIME()-TT," ms";
      DIFFCE := DF(RES,B)-A;
      IF DIFFCE NEQ 0
	THEN  BEGIN FOR ALL X LET TAN X=SIN(2*X)/(1+COS(2*X)),
				  SIN X**2=1-COS X**2,
		    		  TANH X=
				     (E**(X)-E**(-X))/(E**X+E**(-X));
	       	    DIFFCE := DIFFCE;
	            FOR ALL X CLEAR TAN X,SIN X**2,TANH X
	      END;
	%hopefully, difference appeared non-zero due to absence of
	%above transformations;
      IF DIFFCE NEQ 0
	THEN WRITE "DERIVATIVE OF INTEGRAL NOT EQUAL TO INTEGRAND";
    RETURN RES
  END;

% REFERENCES ARE TO GRADSHTEYN & RYZHIK;
testint(1/x,x);  % 2.01 #2;
testint((x+1)**3/(x-1)**4,x);

testint(log x,x);
testint(x*log x,x);
testint(x**2*log x,x);
testint(x**p*log x,x);
testint((log x)**2,x);
testint(x**9*log x**11,x);
testint(log x**2/x,x);
testint(1/log x,x);
testint(1/(x*log x),x);
testint(sin log x,x);
testint(cos log x,x);
testint((log x)**p/x,x);
testint(log x *(a*x+b),x);
testint((a*x+b)**2*log x,x);
testint(log x/(a*x+b)**2,x);
testint(log x/sqrt(a*x+b),x);
testint(x*log (a*x+b),x);
testint(x**2*log(a*x+b),x);
testint(log(x**2+a**2),x);
testint(x*log(x**2+a**2),x);
testint(x**2*log(x**2+a**2),x);
testint(x**4*log(x**2+a**2),x);
testint(log(x**2-a**2),x);
testint(log(log(log(log(x)))),x);

testint(sin x,x); % 2.01 #5;
testint(cos x,x); %     #6;
testint(tan x,x); %     #11;
testint(1/tan(x),x); % 2.01 #12;
testint(1/cos x,x);
testint(1/sin x,x);
testint(sin x**2,x);
testint(x**3*sin(x**2),x);
testint(sin x**3,x);
testint(sin x**p,x);
testint((sin x**2+1)**2*cos x,x);
testint(cos x**2,x);
testint(cos x**3,x);
testint(sin(a*x+b),x);
testint(1/cos x**2,x);
testint(1/(1+cos x),x);
testint(1/(1-cos x),x);
testint(sqrt(1-cos x),x);
testint(sin x* sin (2*x),x);
testint(x*sin x,x);
testint(x**2*sin x,x);
testint(x*sin x**2,x);
testint(x**2*sin x**2,x);
testint(x*sin x**3,x);
testint(x*cos x,x);
testint(x**2*cos x,x);
testint(x*cos x**2,x);
testint(x**2*cos x**2,x);
testint(x*cos x**3,x);
testint(sin x/x,x);
testint(cos x/x,x);
testint(sin x/x**2,x);
testint(sin x**2/x,x);
testint(tan x**3,x);

testint(e**x,x); % 2.01 #3;
testint(a**x,x); % 2.01 #4;
testint(e**(a*x),x);
testint(e**(a*x)/x,x);
testint(1/(a+b*e**(m*x)),x);
testint(e**(2*x)/(1+e**x),x);
testint(1/(a*e**(m*x)+b*e**(-m*x)),x);
testint(x*e**(a*x),x);
testint(x**20*e**x,x);
testint(a**x/b**x,x);
testint(a**x*b**x,x);
testint(a**x/x**2,x);
testint(x*a**x/(1+b*x)**2,x);
testint(x*e**(a*x)/(1+a*x)**2,x);
testint(x*k**(x**2),x);
testint(e**(x**2),x);
testint(x*e**(x**2),x);
testint((2*x**3+x)*(e**(x**2))**2*e**(1-x*e**(x**2))/(1-x*e**(x**2))**2,
	x);
testint(e**(e**(e**(e**x))),x);

testint(e**x*log x,x);
testint(x*e**x*log x,x);
testint(e**(2*x)*log(e**x),x);

z:=a+b*x;
testint(z**p,x);
testint(x*z**p,x);
testint(x**2*z**p,x);
testint(1/z,x);
testint(1/z**2,x);
testint(x/z,x);
testint(x**2/z,x);
testint(1/(x*z),x);
testint(1/(x**2*z),x);
testint(1/(x*z)**2,x);
testint(1/(c**2+x**2),x);
testint(1/(c**2-x**2),x);
u:=sqrt(a+b*x); v:=sqrt(c+d*x);
testint(u*v,x);
testint(u,x);
testint(x*u,x);
testint(x**2*u,x);
testint(u/x,x);
testint(u/x**2,x);
testint(1/u,x);
testint(x/u,x);
testint(x**2/u,x);
testint(1/(x*u),x);
testint(1/(x**2*u),x);
testint(u**p,x);
testint(x*u**p,x);
testint(sin z,x);
testint(cos z,x);
testint(tan z,x);
testint(1/tan z,x);
testint(1/sin z,x);
testint(1/cos z,x);
testint(sin z**2,x);
testint(sin z**3,x);
testint(cos z**2,x);
testint(cos z**3,x);
testint(1/cos z**2,x);
testint(1/(1+sin x),x);
testint(1/(1-sin x),x);
testint(x**2*sin z**2,x);
testint(cos x*cos(2*x),x);
testint(x**2*cos z**2,x);
testint(1/tan x**3,x);
testint(x**3*tan(x)**4,x);
testint(x*tan(x)**2,x);
testint(sin(2*x)*cos(3*x),x);
testint(sin x**2*cos x**2,x);
testint(1/(sin x**2*cos x**2),x);
testint(d**x*sin x,x);
testint(x*d**x*sin x,x);
testint(x**2*d**x*sin x,x);
testint(d**x*cos x,x);
testint(x*d**x*cos x,x);
testint(x**2*d**x*cos x,x);
testint(x**3*d**x*sin x,x);
testint(x**3*d**x*cos x,x);
testint(sin x*sin(2*x)*sin(3*x),x);
testint(cos x*cos(2*x)*cos(3*x),x);
testint(x*cos(xi/sin(x))*cos(x)/sin(x)**2,x);

Comment this integral has given trouble at various times;

testint(atan((-sqrt(2)+2*x)/sqrt(2)),x);


Comment many of these integrals used to require Steve Harrington's
	code to evaluate. They originated in Novosibirsk as examples
	of using Analytik. There are still a few examples which could
	be evaluated using better heuristics;

testint(a*sin(3*x+5)**2*cos(3*x+5),x);
testint(log(x**2)/x**3,x);
testint(x*sin(x+a),x);
testint((log(x)*(1-x)-1)/(e**x*log(x)**2),x);
testint(x**3*(a*x**2+b)**(-1),x);
testint(x**(1/2)*(x+1)**(-7/2),x);
testint(x**(-1)*(x+1)**(-1),x);
testint(x**(-1/2)*(2*x-1)**(-1),x);
testint((x**2+1)*x**(1/2),x);
testint(x**(-1)*(x-a)**(1/3),x);
testint(x*sinh(x),x);
testint(x*cosh(x),x);
testint(x**2*(2*x**2+x)**2,x);
testint(x*(x**2+2*x+1),x);
testint(sinh(2*x)/cosh(2*x),x);
testint(sin(2*x+3)*cos(x)**2,x);
testint(x*atan(x),x);
testint(x*acot(x),x);
testint(x*log(x**2+a),x);
testint(sin(x+a)*cos(x),x);
testint(cos(x+a)*sin(x),x);
testint((2+2*sin(x))**(1/2),x);
testint((2-2*sin(x))**(1/2),x);
testint((2+2*cos(x))**(1/2),x);
testint((2-2*cos(x))**(1/2),x);
testint(1/(x**(1/2)-(x-1)**(1/2)),x);
testint(1/(1-(x+1)**(1/2)),x);
testint(x/(x**4+36)**(1/2),x);
int(1/(x**(1/3)+x**(1/2)),x);
testint(log(2+3*x**2),x);
testint(cot(x),x);
int(cot x**4,x);
testint(tanh(x),x);
testint(coth(x),x);
testint(b**x,x);
testint((x**4+x**(-4)+2)**(1/2),x);
testint((2*x+1)/(3*x+2),x);
testint(x*log(x+(x**2+1)**(1/2)),x);
testint(x*(e**x*sin(x)+1)**2,x);
testint(x*e**x*cos(x),x);

Comment the following set came from Herbert Stoyan who used to be
	in Dresden;

testint(1/(x-3)**4,x);
testint(x/(x**3-1),x);
testint(x/(x**4-1),x);
testint(log(x)*(x**3+1)/(x**4+2),x);
testint(log(x)+log(x+1)+log(x+2),x);
testint(1/(x**3+5),x);
testint(sqrt(x**2+3),x);
testint(x/(x+1)**2,x);

COMMENT The following integrals were contributed by David M. Dahm.
	He also developed the code to make most of them integrable;

testint(1/(2*x**3-1),x);

testint(1/(x**3-2),x);

testint(1/(a*x**3-b),x);

testint(1/(x**4-2),x);

testint(1/(5*x**4-1),x);

testint(1/(3*x**4+7),x);

testint(1/(x**4+3*x**2-1),x);

testint(1/(x**4-3*x**2-1),x);

testint(1/(x**4-3*x**2+1),x);

testint(1/(x**4-4*x**2+1),x);

testint(1/(x**4+4*x**2+1),x);

testint(1/(x**4+x**2+2),x);

testint(1/(x**4-x**2+2),x);

testint(1/(x**6-2),x);

testint(1/(x**6+2),x);

testint(1/(x**8+1),x);

testint(1/(x**8-x**4+1),x);


COMMENT The following integrals were used among others as a test of
	Moses' SIN program;

testint(asin x,x);
testint(x**2*asin x,x);
testint(sec x**2/(1+sec x**2-3*tan x),x);
testint(1/sec x**2,x);
testint((5*x**2-3*x-2)/(x**2*(x-2)),x);
testint(1/(4*x**2+9)**(1/2),x);
testint((x**2+4)**(-1/2),x);
testint(1/(9*x**2-12*x+10),x);
testint(1/(x**8-2*x**7+2*x**6-2*x**5+x**4),x);
testint((a*x**3+b*x**2+c*x+d)/((x+1)*x*(x-3)),x);
testint(1/(2-log(x**2+1))**5,x);
testint((2*x**3+x)*e**(x**2)**2*e**(1-x*e**(x**2))/(1-x*e**(x**2))**2
	,x);
testint(2*x*e**(x**2)*log(x)+e**(x**2)/x+(log(x)-2)/(log(x)**2+x)**2+
    ((2/x)*log(x)+(1/x)+1)/(log(x)**2+x),x);

Comment here is an example of using the integrator with pattern
	matching;

for all m,n let int(k1**m*log(k1)**n/(p**2-k1**2),k1)=foo(m,n),
		int(k1*log(k1)**n/(p**2-k1**2),k1)=foo(1,n),
		int(k1**m*log(k1)/(p**2-k1**2),k1)=foo(m,1),
		int(k1*log(k1)/(p**2-k1**2),k1)=foo(1,1),
		int(log(k1)**n/(k1*(p**2-k1**2)),k1)=foo(-1,n);

int(k1**2*log(k1)/(p**2-k1**2),k1);

COMMENT It is interesting to see how much of this one can be done;

let f1s= (12*log(s/mc**2)*s**2*pi**2*mc**3*(-8*s-12*mc**2+3*mc)
	+ pi**2*(12*s**4*mc+3*s**4+176*s**3*mc**3-24*s**3*mc**2
	-144*s**2*mc**5-48*s*mc**7+24*s*mc**6+4*mc**9-3*mc**8))
	 /(384*e**(s/y)*s**2);

int(f1s,s);

factor int;

ws;

Comment Some definite integrals;

algebraic procedure dint(f,x,x1,x2);
   begin scalar y;
      y := int(f,x);
      return sub(x=x2,y) - sub(x=x1,y)
   end;

dint(sin x,x,0,pi/2);
dint(x/(x+2),x,2,6);
dint(log(x),x,1,5);
dint((1+x**2/p**2)**(1/2),x,0,p);
dint(x**9+y+y**x+x,x,0,2);

Comment the following integrals reveal deficiencies in the current
integrator;

%this one seems to run forever;
%testint(x**7/(x**12+1),x);

%high degree denominator;
%testint(1/(2-log(x**2+1))**5,x);

%the next two integrals should return a closed-form solution;
testint(1/(a+b*sin x),x);
testint(1/(a+b*sin x+cos x),x);

%this example should evaluate;
testint(sin(2*x)/cos(x),x);

%this example, which appeared in Tobey's thesis, needs factorization
%over algebraic fields. It currently gives an ugly answer;

int((7*x**13+10*x**8+4*x**7-7*x**6-4*x**3-4*x**2+3*x+3)/
    (x**14-2*x**8-2*x**7-2*x**4-4*x**3-x**2+2*x+1),x);


end;
