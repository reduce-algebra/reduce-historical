%. MATHLIB.TST

% A simple set of tests for MAthLIB

LOAD MATHLIB$

Global '(EPS);

EPS:=1.0/(1.0E6);

Fexpr procedure TS L$ % (Function,Arg,Expected Value)
 Begin scalar Fn,Arg,Val,x,y;
	Fn:=car L$
	Arg:=EVAL cadr L$
	Val:=EVAL Caddr L$
	x:=Apply(fn, list arg)$
	PrintF(" %r(%p) = %p, expect %p%n",Fn,arg,x,val)$
        y:=abs(x-val);
        if y>=EPS then PrintF(" ***** %p exceeds EPS%n",y);
 End$

TS(Ceiling,3,3);
TS(Ceiling,3.1,4);
TS(Ceiling,3.7,4);
TS(Ceiling,-3,-3);
TS(Ceiling,-3.5,-2);

TS(Round,3,3);
TS(Round,3.1,3);
TS(Round,3.5,4);
TS(Round,3.7,4);
TS(Round,-3,-3);
TS(Round,-3.4,-2);
TS(Round,-3.7,-3);

TwoPI := 6.2831853;
PI:=TwoPI/2;
PI2:=PI/2;
PI4:=PI/4;
PI8:=PI/8;

Root2:=1.4142136;
Root2**2 - 2.0;

TS(sin, 0.0, 0.0)$
TS(cos, 0.0, 1.0)$
TS(sin, PI4, Root2/2)$
TS(cos, PI4, Root2/2)$
TS(sin, PI2, 1.0)$
TS(cos, PI2, 0.0)$
TS(sin, 3*PI4, Root2/2)$
TS(cos, 3*PI4, -Root2/2)$
TS(sin, PI, 0.0)$
TS(cos, PI, -1.0)$


procedure SC2 x;
 sin(x)**2+cos(x)**2;

TS(SC2,0.0,1)$
TS(SC2,0.25,1)$
TS(SC2,0.5,1)$
TS(SC2,0.75,1)$
TS(SC2,1.0,1)$
TS(SC2,1.25,1)$
TS(SC2,1.5,1)$
TS(SC2,1.75,1)$
TS(SC2,2.0,1)$
TS(SC2,2.25,1)$
TS(SC2,2.5,1)$
TS(SC2,2.75,1)$
TS(SC2,3.0,1)$

TS(TAN,0.0,0.0)$
TS(TAN,PI8,SIN(PI8)/COS(PI8))$
TS(TAN,PI4,1.0)$

TS(COT,PI8,COS(pi8)/SIN(pi8))$
TS(COT,PI4,1.0)$

TS(SIND,30.0,0.5)$
TS(ASIND,0.5,30.0)$

TS(SQRT,2.0,Root2)$
TS(SQRT,9.0,3.0)$
TS(SQRT,100.0,10.0)$

NaturalE:=2.718281828$

TS(EXP,1.0,NaturalE)$

TS(LOG,SQRT(NaturalE),0.5)$
TS(LOG,NaturalE,1.0)$
TS(LOG,NaturalE**2,2.0)$
TS(LOG,1.0/NaturalE**2, -2.0)$


TS(LOG2,Root2,0.5)$
TS(LOG2,2.0,1.0)$
TS(LOG2,4.0,2.0)$
TS(LOG2,0.5, -1.0)$

TS(LOG10,SQRT(10.0),0.5)$
TS(LOG10,10.0,1.0)$
TS(LOG10,100.0,2.0)$
TS(LOG10, 1.0E30, 30.0)$
TS(LOG10, 1.0E-30, -30.0)$
End$
