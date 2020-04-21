on time;

123/100;

%this used the ordinary rational number system;

on bigfloat;

%now we shall use big-floats;

ws/2;

%Note that trailing zeros have been suppressed, although we know
%that this number was calculated to a default precision of 10;

%Let us raise this to a high power;

ws**24;

%Now let us evaluate pi;

pi;

%Of course this was treated symbolically;

on numval;

%However, this will force numerical evaluation;

ws;

%Let us try a higher precision;

precision 50;

pi;

%Now find the cosine of pi/6;

cos(ws/6);

%This should be the sqrt(3)/2;

ws**2;


%Here are some well known examples which show the power of the big 
%float system;

precision 10;

%the usual default again;

let xx=e**(pi*sqrt(163));
let yy=1-2*cos((6*log(2)+log(10005))/sqrt(163));

%now ask for numerical values of constants;

on numval;

%first notice that xx looks like an integer;

xx;

%and that yy looks like zero;

yy;

%but of course it's an illusion;

precision 50;

xx;

yy;

%now let's look at an unusual way of finding an old friend;

 nn := 8$
 a := 1$ b := 1/sqrt 2$ u:= 1/4$ x := 1$
for i:=1:nn do 
   <<y := a; a := (a+b)/2; b := sqrt(y*b); %arith-geom mean;
     u := u-x*(a-y)**2; x := 2*x;
     write a**2/u>>;

%the limit is obviously:

pi;


end;
