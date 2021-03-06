comment
        Test and demonstration file for the Taylor expansion package,
        by Rainer M. Schoepf.  Works with version 1.3 (31-Jan-91);

showtime;

on errcont; % disable interruption on errors

comment Simple Taylor expansion;

xx := taylor (e**x, x, 0, 4);
yy := taylor (e**y, y, 0, 4);

comment Basic operations, i.e. addition, subtraction, multiplication,
        and division are possible: this is not done automatically if
        the switch TAYLORAUTOCOMBINE is OFF.  In this case it is
        necessary to use taylorcombine;

taylorcombine (xx**2);
taylorcombine (ws - xx);

comment The result is again a Taylor kernel;

if taylorseriesp ws then write "OK";

comment It is not possible to combine Taylor kernels that were
        expanded with respect to different variables;

taylorcombine (xx**yy);

comment But we can take the exponential or the logarithm
        of a Taylor kernel;

taylorcombine (e**xx);
taylorcombine log ws;

comment We may try to expand about another point;

taylor (xx, x, 1, 2);

comment Arc tangent is one of the functions this package knows of;

xxa := taylorcombine atan ws;

comment Expansion with respect to more than one kernel is possible;

xy := taylor (e**(x+y), x, 0, 2, y, 0, 2);

taylorcombine (ws**2);

comment We take the inverse and convert back to REDUCE's standard
        representation;

taylorcombine (1/ws);
taylortostandard ws;

comment An example of Taylor kernel divsion;

xx1 := taylor (sin (x), x, 0, 4);
taylorcombine (xx/xx1);
taylorcombine (1/xx1);

comment Here's what I call homogeneous expansion;

xx := taylor (e**(x*y), {x,y}, 0, 2);
xx1 := taylor (sin (x+y), {x,y}, 0, 2);
xx2 := taylor (cos (x+y), {x,y}, 0, 2);
temp := taylorcombine (xx/xx2);
taylorcombine (ws*xx2);

comment The following shows a principal difficulty:
        since xx1 is symmetric in x and y but has no constant term
        it is impossible to compute 1/xx1;

taylorcombine (1/xx1);

comment Substitution in Taylor expressions is possible;

sub (x=z, xy);

comment Expression dependency in substitution is detected;

sub (x=y, xy);

comment It is possible to replace a Taylor variable by a constant;

sub (x=4, xy);

sub (x=4, xx1);

comment This package has three switches:
        TAYLORKEEPORIGINAL, TAYLORAUTOEXPAND, and TAYLORAUTOCOMBINE;

on taylorkeeporiginal;

temp := taylor (e**(x+y), x, 0, 5);

taylorcombine (log (temp));

taylororiginal ws;

taylorcombine (temp * e**x);

on taylorautoexpand;

taylorcombine ws;

taylororiginal ws;

taylorcombine (xx1 / x);

on taylorautocombine;

xx / xx2;

ws * xx2;

comment Another example that shows truncation if Taylor kernels
        of different expansion order are combined;

p := taylor (x**2 + 2, x, 0, 10);
p - x**2;
p - taylor (x**2, x, 0, 5);
taylor (p - x**2, x, 0, 6);
off taylorautocombine;
taylorcombine(p-x**2);
taylorcombine(p - taylor(x**2,x,0,5));

comment A problem are non-analytic terms: there are no precautions
        taken to detect or handle them;

taylor (sqrt (x), x, 0, 2);

taylor (e**(1/x), x, 0, 2);

comment Even worse: you can substitute a non analytical kernel;

sub (y = sqrt (x), yy);

comment Expansion about infinity is possible in principle...;

taylor (e**(1/x), x, infinity, 5);
xi := taylor (sin (1/x), x, infinity, 5);

y1 := taylor(x/(x-1), x, infinity, 3);
z := df(y1, x);

comment ...but far from being perfect;

taylor (1 / sin (x), x, infinity, 5);

comment The template of a Taylor kernel can be extracted;

taylortemplate yy;

taylortemplate xxa;

taylortemplate xi;

taylortemplate xy;

taylortemplate xx1;

comment Here is a slightly less trivial example;

exp := (sin (x) * sin (y) / (x * y))**2;

taylor (exp, x, 0, 1, y, 0, 1);
taylor (exp, x, 0, 2, y, 0, 2);

tt := taylor (exp, {x,y}, 0, 2);

comment An application is the problem posed by Prof. Stanley:
        we prove that the finite difference expression below
        corresponds to the given derivative expression;

comment We use gg to avoid conflicts with the predefined g operator;

define g=gg;

operator diff,a,f,g;

for all f,arg let diff(f,arg) = df(f,arg);

derivative!_expression :=
diff(a(x,y)*diff(g(x,y),x)*diff(g(x,y),y)*diff(f(x,y),y),x) +
diff(a(x,y)*diff(g(x,y),x)*diff(g(x,y),y)*diff(f(x,y),x),y) ;

finite!_difference!_expression :=
+a(x+dx,y+dy)*f(x+dx,y+dy)*g(x+dx,y+dy)^2/(32*dx^2*dy^2)
+a(x+dx,y)*f(x+dx,y+dy)*g(x+dx,y+dy)^2/(32*dx^2*dy^2)
+a(x,y+dy)*f(x+dx,y+dy)*g(x+dx,y+dy)^2/(32*dx^2*dy^2)
+a(x,y)*f(x+dx,y+dy)*g(x+dx,y+dy)^2/(32*dx^2*dy^2)
-f(x,y)*a(x+dx,y+dy)*g(x+dx,y+dy)^2/(32*dx^2*dy^2)
-f(x,y)*a(x+dx,y)*g(x+dx,y+dy)^2/(32*dx^2*dy^2)
-f(x,y)*a(x,y+dy)*g(x+dx,y+dy)^2/(32*dx^2*dy^2)
-a(x,y)*f(x,y)*g(x+dx,y+dy)^2/(32*dx^2*dy^2)
-g(x,y)*a(x+dx,y+dy)*f(x+dx,y+dy)*g(x+dx,y+dy)/(16*dx^2*dy^2)
-g(x,y)*a(x+dx,y)*f(x+dx,y+dy)*g(x+dx,y+dy)/(16*dx^2*dy^2)
-g(x,y)*a(x,y+dy)*f(x+dx,y+dy)*g(x+dx,y+dy)/(16*dx^2*dy^2)
-a(x,y)*g(x,y)*f(x+dx,y+dy)*g(x+dx,y+dy)/(16*dx^2*dy^2)
+f(x,y)*g(x,y)*a(x+dx,y+dy)*g(x+dx,y+dy)/(16*dx^2*dy^2)
+f(x,y)*g(x,y)*a(x+dx,y)*g(x+dx,y+dy)/(16*dx^2*dy^2)
+f(x,y)*g(x,y)*a(x,y+dy)*g(x+dx,y+dy)/(16*dx^2*dy^2)
+a(x,y)*f(x,y)*g(x,y)*g(x+dx,y+dy)/(16*dx^2*dy^2)
-g(x+dx,y)^2*a(x+dx,y+dy)*f(x+dx,y+dy)/(32*dx^2*dy^2)
+g(x,y+dy)*g(x+dx,y)*a(x+dx,y+dy)*f(x+dx,y+dy)/(16*dx^2*dy^2)
-g(x,y+dy)^2*a(x+dx,y+dy)*f(x+dx,y+dy)/(32*dx^2*dy^2)
+g(x,y)^2*a(x+dx,y+dy)*f(x+dx,y+dy)/(32*dx^2*dy^2)
-a(x+dx,y)*g(x+dx,y)^2*f(x+dx,y+dy)/(32*dx^2*dy^2)
-a(x,y+dy)*g(x+dx,y)^2*f(x+dx,y+dy)/(32*dx^2*dy^2)
-a(x,y)*g(x+dx,y)^2*f(x+dx,y+dy)/(32*dx^2*dy^2)
+g(x,y+dy)*a(x+dx,y)*g(x+dx,y)*f(x+dx,y+dy)/(16*dx^2*dy^2)
+a(x,y+dy)*g(x,y+dy)*g(x+dx,y)*f(x+dx,y+dy)/(16*dx^2*dy^2)
+a(x,y)*g(x,y+dy)*g(x+dx,y)*f(x+dx,y+dy)/(16*dx^2*dy^2)
-g(x,y+dy)^2*a(x+dx,y)*f(x+dx,y+dy)/(32*dx^2*dy^2)
+g(x,y)^2*a(x+dx,y)*f(x+dx,y+dy)/(32*dx^2*dy^2)
-a(x,y+dy)*g(x,y+dy)^2*f(x+dx,y+dy)/(32*dx^2*dy^2)
-a(x,y)*g(x,y+dy)^2*f(x+dx,y+dy)/(32*dx^2*dy^2)
+g(x,y)^2*a(x,y+dy)*f(x+dx,y+dy)/(32*dx^2*dy^2)
+a(x,y)*g(x,y)^2*f(x+dx,y+dy)/(32*dx^2*dy^2)
+f(x,y)*g(x+dx,y)^2*a(x+dx,y+dy)/(32*dx^2*dy^2)
-f(x,y)*g(x,y+dy)*g(x+dx,y)*a(x+dx,y+dy)/(16*dx^2*dy^2)
+f(x,y)*g(x,y+dy)^2*a(x+dx,y+dy)/(32*dx^2*dy^2)
-f(x,y)*g(x,y)^2*a(x+dx,y+dy)/(32*dx^2*dy^2)
+a(x+dx,y-dy)*f(x+dx,y-dy)*g(x+dx,y-dy)^2/(32*dx^2*dy^2)
+a(x+dx,y)*f(x+dx,y-dy)*g(x+dx,y-dy)^2/(32*dx^2*dy^2)
+a(x,y-dy)*f(x+dx,y-dy)*g(x+dx,y-dy)^2/(32*dx^2*dy^2)
+a(x,y)*f(x+dx,y-dy)*g(x+dx,y-dy)^2/(32*dx^2*dy^2)
-f(x,y)*a(x+dx,y-dy)*g(x+dx,y-dy)^2/(32*dx^2*dy^2)
-f(x,y)*a(x+dx,y)*g(x+dx,y-dy)^2/(32*dx^2*dy^2)
-f(x,y)*a(x,y-dy)*g(x+dx,y-dy)^2/(32*dx^2*dy^2)
-a(x,y)*f(x,y)*g(x+dx,y-dy)^2/(32*dx^2*dy^2)
-g(x,y)*a(x+dx,y-dy)*f(x+dx,y-dy)*g(x+dx,y-dy)/(16*dx^2*dy^2)
-g(x,y)*a(x+dx,y)*f(x+dx,y-dy)*g(x+dx,y-dy)/(16*dx^2*dy^2)
-g(x,y)*a(x,y-dy)*f(x+dx,y-dy)*g(x+dx,y-dy)/(16*dx^2*dy^2)
-a(x,y)*g(x,y)*f(x+dx,y-dy)*g(x+dx,y-dy)/(16*dx^2*dy^2)
+f(x,y)*g(x,y)*a(x+dx,y-dy)*g(x+dx,y-dy)/(16*dx^2*dy^2)
+f(x,y)*g(x,y)*a(x+dx,y)*g(x+dx,y-dy)/(16*dx^2*dy^2)
+f(x,y)*g(x,y)*a(x,y-dy)*g(x+dx,y-dy)/(16*dx^2*dy^2)
+a(x,y)*f(x,y)*g(x,y)*g(x+dx,y-dy)/(16*dx^2*dy^2)
-g(x+dx,y)^2*a(x+dx,y-dy)*f(x+dx,y-dy)/(32*dx^2*dy^2)
+g(x,y-dy)*g(x+dx,y)*a(x+dx,y-dy)*f(x+dx,y-dy)/(16*dx^2*dy^2)
-g(x,y-dy)^2*a(x+dx,y-dy)*f(x+dx,y-dy)/(32*dx^2*dy^2)
+g(x,y)^2*a(x+dx,y-dy)*f(x+dx,y-dy)/(32*dx^2*dy^2)
-a(x+dx,y)*g(x+dx,y)^2*f(x+dx,y-dy)/(32*dx^2*dy^2)
-a(x,y-dy)*g(x+dx,y)^2*f(x+dx,y-dy)/(32*dx^2*dy^2)
-a(x,y)*g(x+dx,y)^2*f(x+dx,y-dy)/(32*dx^2*dy^2)
+g(x,y-dy)*a(x+dx,y)*g(x+dx,y)*f(x+dx,y-dy)/(16*dx^2*dy^2)
+a(x,y-dy)*g(x,y-dy)*g(x+dx,y)*f(x+dx,y-dy)/(16*dx^2*dy^2)
+a(x,y)*g(x,y-dy)*g(x+dx,y)*f(x+dx,y-dy)/(16*dx^2*dy^2)
-g(x,y-dy)^2*a(x+dx,y)*f(x+dx,y-dy)/(32*dx^2*dy^2)
+g(x,y)^2*a(x+dx,y)*f(x+dx,y-dy)/(32*dx^2*dy^2)
-a(x,y-dy)*g(x,y-dy)^2*f(x+dx,y-dy)/(32*dx^2*dy^2)
-a(x,y)*g(x,y-dy)^2*f(x+dx,y-dy)/(32*dx^2*dy^2)
+g(x,y)^2*a(x,y-dy)*f(x+dx,y-dy)/(32*dx^2*dy^2)
+a(x,y)*g(x,y)^2*f(x+dx,y-dy)/(32*dx^2*dy^2)
+f(x,y)*g(x+dx,y)^2*a(x+dx,y-dy)/(32*dx^2*dy^2)
-f(x,y)*g(x,y-dy)*g(x+dx,y)*a(x+dx,y-dy)/(16*dx^2*dy^2)
+f(x,y)*g(x,y-dy)^2*a(x+dx,y-dy)/(32*dx^2*dy^2)
-f(x,y)*g(x,y)^2*a(x+dx,y-dy)/(32*dx^2*dy^2)
+f(x,y)*a(x+dx,y)*g(x+dx,y)^2/(16*dx^2*dy^2)
+f(x,y)*a(x,y+dy)*g(x+dx,y)^2/(32*dx^2*dy^2)
+f(x,y)*a(x,y-dy)*g(x+dx,y)^2/(32*dx^2*dy^2)
+a(x,y)*f(x,y)*g(x+dx,y)^2/(16*dx^2*dy^2)
-f(x,y)*g(x,y+dy)*a(x+dx,y)*g(x+dx,y)/(16*dx^2*dy^2)
-f(x,y)*g(x,y-dy)*a(x+dx,y)*g(x+dx,y)/(16*dx^2*dy^2)
-f(x,y)*a(x,y+dy)*g(x,y+dy)*g(x+dx,y)/(16*dx^2*dy^2)
-a(x,y)*f(x,y)*g(x,y+dy)*g(x+dx,y)/(16*dx^2*dy^2)
-f(x,y)*a(x,y-dy)*g(x,y-dy)*g(x+dx,y)/(16*dx^2*dy^2)
-a(x,y)*f(x,y)*g(x,y-dy)*g(x+dx,y)/(16*dx^2*dy^2)
+f(x,y)*g(x,y+dy)^2*a(x+dx,y)/(32*dx^2*dy^2)
+f(x,y)*g(x,y-dy)^2*a(x+dx,y)/(32*dx^2*dy^2)
-f(x,y)*g(x,y)^2*a(x+dx,y)/(16*dx^2*dy^2)
+a(x-dx,y+dy)*f(x-dx,y+dy)*g(x-dx,y+dy)^2/(32*dx^2*dy^2)
+a(x-dx,y)*f(x-dx,y+dy)*g(x-dx,y+dy)^2/(32*dx^2*dy^2)
+a(x,y+dy)*f(x-dx,y+dy)*g(x-dx,y+dy)^2/(32*dx^2*dy^2)
+a(x,y)*f(x-dx,y+dy)*g(x-dx,y+dy)^2/(32*dx^2*dy^2)
-f(x,y)*a(x-dx,y+dy)*g(x-dx,y+dy)^2/(32*dx^2*dy^2)
-f(x,y)*a(x-dx,y)*g(x-dx,y+dy)^2/(32*dx^2*dy^2)
-f(x,y)*a(x,y+dy)*g(x-dx,y+dy)^2/(32*dx^2*dy^2)
-a(x,y)*f(x,y)*g(x-dx,y+dy)^2/(32*dx^2*dy^2)
-g(x,y)*a(x-dx,y+dy)*f(x-dx,y+dy)*g(x-dx,y+dy)/(16*dx^2*dy^2)
-g(x,y)*a(x-dx,y)*f(x-dx,y+dy)*g(x-dx,y+dy)/(16*dx^2*dy^2)
-g(x,y)*a(x,y+dy)*f(x-dx,y+dy)*g(x-dx,y+dy)/(16*dx^2*dy^2)
-a(x,y)*g(x,y)*f(x-dx,y+dy)*g(x-dx,y+dy)/(16*dx^2*dy^2)
+f(x,y)*g(x,y)*a(x-dx,y+dy)*g(x-dx,y+dy)/(16*dx^2*dy^2)
+f(x,y)*g(x,y)*a(x-dx,y)*g(x-dx,y+dy)/(16*dx^2*dy^2)
+f(x,y)*g(x,y)*a(x,y+dy)*g(x-dx,y+dy)/(16*dx^2*dy^2)
+a(x,y)*f(x,y)*g(x,y)*g(x-dx,y+dy)/(16*dx^2*dy^2)
-g(x-dx,y)^2*a(x-dx,y+dy)*f(x-dx,y+dy)/(32*dx^2*dy^2)
+g(x,y+dy)*g(x-dx,y)*a(x-dx,y+dy)*f(x-dx,y+dy)/(16*dx^2*dy^2)
-g(x,y+dy)^2*a(x-dx,y+dy)*f(x-dx,y+dy)/(32*dx^2*dy^2)
+g(x,y)^2*a(x-dx,y+dy)*f(x-dx,y+dy)/(32*dx^2*dy^2)
-a(x-dx,y)*g(x-dx,y)^2*f(x-dx,y+dy)/(32*dx^2*dy^2)
-a(x,y+dy)*g(x-dx,y)^2*f(x-dx,y+dy)/(32*dx^2*dy^2)
-a(x,y)*g(x-dx,y)^2*f(x-dx,y+dy)/(32*dx^2*dy^2)
+g(x,y+dy)*a(x-dx,y)*g(x-dx,y)*f(x-dx,y+dy)/(16*dx^2*dy^2)
+a(x,y+dy)*g(x,y+dy)*g(x-dx,y)*f(x-dx,y+dy)/(16*dx^2*dy^2)
+a(x,y)*g(x,y+dy)*g(x-dx,y)*f(x-dx,y+dy)/(16*dx^2*dy^2)
-g(x,y+dy)^2*a(x-dx,y)*f(x-dx,y+dy)/(32*dx^2*dy^2)
+g(x,y)^2*a(x-dx,y)*f(x-dx,y+dy)/(32*dx^2*dy^2)
-a(x,y+dy)*g(x,y+dy)^2*f(x-dx,y+dy)/(32*dx^2*dy^2)
-a(x,y)*g(x,y+dy)^2*f(x-dx,y+dy)/(32*dx^2*dy^2)
+g(x,y)^2*a(x,y+dy)*f(x-dx,y+dy)/(32*dx^2*dy^2)
+a(x,y)*g(x,y)^2*f(x-dx,y+dy)/(32*dx^2*dy^2)
+f(x,y)*g(x-dx,y)^2*a(x-dx,y+dy)/(32*dx^2*dy^2)
-f(x,y)*g(x,y+dy)*g(x-dx,y)*a(x-dx,y+dy)/(16*dx^2*dy^2)
+f(x,y)*g(x,y+dy)^2*a(x-dx,y+dy)/(32*dx^2*dy^2)
-f(x,y)*g(x,y)^2*a(x-dx,y+dy)/(32*dx^2*dy^2)
+a(x-dx,y-dy)*f(x-dx,y-dy)*g(x-dx,y-dy)^2/(32*dx^2*dy^2)
+a(x-dx,y)*f(x-dx,y-dy)*g(x-dx,y-dy)^2/(32*dx^2*dy^2)
+a(x,y-dy)*f(x-dx,y-dy)*g(x-dx,y-dy)^2/(32*dx^2*dy^2)
+a(x,y)*f(x-dx,y-dy)*g(x-dx,y-dy)^2/(32*dx^2*dy^2)
-f(x,y)*a(x-dx,y-dy)*g(x-dx,y-dy)^2/(32*dx^2*dy^2)
-f(x,y)*a(x-dx,y)*g(x-dx,y-dy)^2/(32*dx^2*dy^2)
-f(x,y)*a(x,y-dy)*g(x-dx,y-dy)^2/(32*dx^2*dy^2)
-a(x,y)*f(x,y)*g(x-dx,y-dy)^2/(32*dx^2*dy^2)
-g(x,y)*a(x-dx,y-dy)*f(x-dx,y-dy)*g(x-dx,y-dy)/(16*dx^2*dy^2)
-g(x,y)*a(x-dx,y)*f(x-dx,y-dy)*g(x-dx,y-dy)/(16*dx^2*dy^2)
-g(x,y)*a(x,y-dy)*f(x-dx,y-dy)*g(x-dx,y-dy)/(16*dx^2*dy^2)
-a(x,y)*g(x,y)*f(x-dx,y-dy)*g(x-dx,y-dy)/(16*dx^2*dy^2)
+f(x,y)*g(x,y)*a(x-dx,y-dy)*g(x-dx,y-dy)/(16*dx^2*dy^2)
+f(x,y)*g(x,y)*a(x-dx,y)*g(x-dx,y-dy)/(16*dx^2*dy^2)
+f(x,y)*g(x,y)*a(x,y-dy)*g(x-dx,y-dy)/(16*dx^2*dy^2)
+a(x,y)*f(x,y)*g(x,y)*g(x-dx,y-dy)/(16*dx^2*dy^2)
-g(x-dx,y)^2*a(x-dx,y-dy)*f(x-dx,y-dy)/(32*dx^2*dy^2)
+g(x,y-dy)*g(x-dx,y)*a(x-dx,y-dy)*f(x-dx,y-dy)/(16*dx^2*dy^2)
-g(x,y-dy)^2*a(x-dx,y-dy)*f(x-dx,y-dy)/(32*dx^2*dy^2)
+g(x,y)^2*a(x-dx,y-dy)*f(x-dx,y-dy)/(32*dx^2*dy^2)
-a(x-dx,y)*g(x-dx,y)^2*f(x-dx,y-dy)/(32*dx^2*dy^2)
-a(x,y-dy)*g(x-dx,y)^2*f(x-dx,y-dy)/(32*dx^2*dy^2)
-a(x,y)*g(x-dx,y)^2*f(x-dx,y-dy)/(32*dx^2*dy^2)
+g(x,y-dy)*a(x-dx,y)*g(x-dx,y)*f(x-dx,y-dy)/(16*dx^2*dy^2)
+a(x,y-dy)*g(x,y-dy)*g(x-dx,y)*f(x-dx,y-dy)/(16*dx^2*dy^2)
+a(x,y)*g(x,y-dy)*g(x-dx,y)*f(x-dx,y-dy)/(16*dx^2*dy^2)
-g(x,y-dy)^2*a(x-dx,y)*f(x-dx,y-dy)/(32*dx^2*dy^2)
+g(x,y)^2*a(x-dx,y)*f(x-dx,y-dy)/(32*dx^2*dy^2)
-a(x,y-dy)*g(x,y-dy)^2*f(x-dx,y-dy)/(32*dx^2*dy^2)
-a(x,y)*g(x,y-dy)^2*f(x-dx,y-dy)/(32*dx^2*dy^2)
+g(x,y)^2*a(x,y-dy)*f(x-dx,y-dy)/(32*dx^2*dy^2)
+a(x,y)*g(x,y)^2*f(x-dx,y-dy)/(32*dx^2*dy^2)
+f(x,y)*g(x-dx,y)^2*a(x-dx,y-dy)/(32*dx^2*dy^2)
-f(x,y)*g(x,y-dy)*g(x-dx,y)*a(x-dx,y-dy)/(16*dx^2*dy^2)
+f(x,y)*g(x,y-dy)^2*a(x-dx,y-dy)/(32*dx^2*dy^2)
-f(x,y)*g(x,y)^2*a(x-dx,y-dy)/(32*dx^2*dy^2)
+f(x,y)*a(x-dx,y)*g(x-dx,y)^2/(16*dx^2*dy^2)
+f(x,y)*a(x,y+dy)*g(x-dx,y)^2/(32*dx^2*dy^2)
+f(x,y)*a(x,y-dy)*g(x-dx,y)^2/(32*dx^2*dy^2)
+a(x,y)*f(x,y)*g(x-dx,y)^2/(16*dx^2*dy^2)
-f(x,y)*g(x,y+dy)*a(x-dx,y)*g(x-dx,y)/(16*dx^2*dy^2)
-f(x,y)*g(x,y-dy)*a(x-dx,y)*g(x-dx,y)/(16*dx^2*dy^2)
-f(x,y)*a(x,y+dy)*g(x,y+dy)*g(x-dx,y)/(16*dx^2*dy^2)
-a(x,y)*f(x,y)*g(x,y+dy)*g(x-dx,y)/(16*dx^2*dy^2)
-f(x,y)*a(x,y-dy)*g(x,y-dy)*g(x-dx,y)/(16*dx^2*dy^2)
-a(x,y)*f(x,y)*g(x,y-dy)*g(x-dx,y)/(16*dx^2*dy^2)
+f(x,y)*g(x,y+dy)^2*a(x-dx,y)/(32*dx^2*dy^2)
+f(x,y)*g(x,y-dy)^2*a(x-dx,y)/(32*dx^2*dy^2)
-f(x,y)*g(x,y)^2*a(x-dx,y)/(16*dx^2*dy^2)
+f(x,y)*a(x,y+dy)*g(x,y+dy)^2/(16*dx^2*dy^2)
+a(x,y)*f(x,y)*g(x,y+dy)^2/(16*dx^2*dy^2)
-f(x,y)*g(x,y)^2*a(x,y+dy)/(16*dx^2*dy^2)
+f(x,y)*a(x,y-dy)*g(x,y-dy)^2/(16*dx^2*dy^2)
+a(x,y)*f(x,y)*g(x,y-dy)^2/(16*dx^2*dy^2)
-f(x,y)*g(x,y)^2*a(x,y-dy)/(16*dx^2*dy^2)
-a(x,y)*f(x,y)*g(x,y)^2/(8*dx^2*dy^2)$

comment We define abbreviations for the partial derivatives;

operator ax,ay,fx,fy,gx,gy;

for all x,y let df(a(x,y),x) = ax(x,y);
for all x,y let df(a(x,y),y) = ay(x,y);
for all x,y let df(f(x,y),x) = fx(x,y);
for all x,y let df(f(x,y),y) = fy(x,y);
for all x,y let df(g(x,y),x) = gx(x,y);
for all x,y let df(g(x,y),y) = gy(x,y);

operator axx,axy,ayy,fxx,fxy,fyy,gxx,gxy,gyy;

for all x,y let df(ax(x,y),x) = axx(x,y);
for all x,y let df(ax(x,y),y) = axy(x,y);
for all x,y let df(ay(x,y),x) = axy(x,y);
for all x,y let df(ay(x,y),y) = ayy(x,y);
for all x,y let df(fx(x,y),x) = fxx(x,y);
for all x,y let df(fx(x,y),y) = fxy(x,y);
for all x,y let df(fy(x,y),x) = fxy(x,y);
for all x,y let df(fy(x,y),y) = fyy(x,y);
for all x,y let df(gx(x,y),x) = gxx(x,y);
for all x,y let df(gx(x,y),y) = gxy(x,y);
for all x,y let df(gy(x,y),x) = gxy(x,y);
for all x,y let df(gy(x,y),y) = gyy(x,y);

operator axxx,axxy,axyy,ayyy,fxxx,fxxy,fxyy,fyyy,gxxx,gxxy,gxyy,gyyy;

for all x,y let df(axx(x,y),x) = axxx(x,y);
for all x,y let df(axy(x,y),x) = axxy(x,y);
for all x,y let df(ayy(x,y),x) = axyy(x,y);
for all x,y let df(ayy(x,y),y) = ayyy(x,y);
for all x,y let df(fxx(x,y),x) = fxxx(x,y);
for all x,y let df(fxy(x,y),x) = fxxy(x,y);
for all x,y let df(fxy(x,y),y) = fxyy(x,y);
for all x,y let df(fyy(x,y),x) = fxyy(x,y);
for all x,y let df(fyy(x,y),y) = fyyy(x,y);
for all x,y let df(gxx(x,y),x) = gxxx(x,y);
for all x,y let df(gxx(x,y),y) = gxxy(x,y);
for all x,y let df(gxy(x,y),x) = gxxy(x,y);
for all x,y let df(gxy(x,y),y) = gxyy(x,y);
for all x,y let df(gyy(x,y),x) = gxyy(x,y);
for all x,y let df(gyy(x,y),y) = gyyy(x,y);

operator axxxy,axxyy,axyyy,fxxxy,fxxyy,fxyyy,
         gxxxx,gxxxy,gxxyy,gxyyy,gyyyy;

for all x,y let df(axyy(x,y),x) = axxyy(x,y);
for all x,y let df(axxy(x,y),x) = axxxy(x,y);
for all x,y let df(ayyy(x,y),x) = axyyy(x,y);
for all x,y let df(fxxy(x,y),x) = fxxxy(x,y);
for all x,y let df(fxyy(x,y),x) = fxxyy(x,y);
for all x,y let df(fyyy(x,y),x) = fxyyy(x,y);
for all x,y let df(gxxx(x,y),x) = gxxxx(x,y);
for all x,y let df(gxxy(x,y),x) = gxxxy(x,y);
for all x,y let df(gxyy(x,y),x) = gxxyy(x,y);
for all x,y let df(gyyy(x,y),x) = gxyyy(x,y);
for all x,y let df(gyyy(x,y),y) = gyyyy(x,y);

operator axxxyy,axxyyy,fxxyyy,fxxxyy,gxxxxy,gxxxyy,gxxyyy,gxyyyy;

for all x,y let df(axxyy(x,y),x) = axxxyy(x,y);
for all x,y let df(axyyy(x,y),x) = axxyyy(x,y);
for all x,y let df(fxxyy(x,y),x) = fxxxyy(x,y);
for all x,y let df(fxyyy(x,y),x) = fxxyyy(x,y);
for all x,y let df(gxxxy(x,y),x) = gxxxxy(x,y);
for all x,y let df(gxxyy(x,y),x) = gxxxyy(x,y);
for all x,y let df(gxyyy(x,y),x) = gxxyyy(x,y);
for all x,y let df(gyyyy(x,y),x) = gxyyyy(x,y);

operator gxxxxyy,gxxxyyy,gxxyyyy;

for all x,y let df(gxxxyy(x,y),x) = gxxxxyy(x,y);
for all x,y let df(gxxyyy(x,y),x) = gxxxyyy(x,y);
for all x,y let df(gxyyyy(x,y),x) = gxxyyyy(x,y);

texp := taylor (finite!_difference!_expression, dx, 0, 1, dy, 0, 1);

comment You may also try to expand further but this needs a lot
        of CPU time.  Therefore the following line is commented out;

%texp := taylor (finite!_difference!_expression, dx, 0, 2, dy, 0, 2);

factor dx,dy;

result := taylortostandard texp;

derivative!_expression - result;

comment That's all, folks;

showtime;

end;
