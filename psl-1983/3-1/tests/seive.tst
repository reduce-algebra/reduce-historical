27-Mar-83 09:09:18-MST,4778;000000000001
Return-path: <GRISS@HP-HULK>
Received: from UTAH-CS by UTAH-20; Sun 27 Mar 83 09:07:41-MST
Date: 27 Mar 1983 0753-PST
From: GRISS@HP-HULK
Subject: String and vector
Message-Id: <417628520.17208.hplabs@HP-VENUS>
Received: by HP-VENUS via CHAOSNET; 27 Mar 1983 07:55:19-PST
Received: by UTAH-CS.ARPA (3.320.3/3.7.4)
	id AA28476; 27 Mar 83 08:59:13 MST (Sun)
To: kessler@HP-VENUS, griss@HP-VENUS

I was doing some timings on SIEVE.RED (attached) on VAX and 20. 
Havent  yet done for 68000. Compared with C on VAX:

a) Proportionately, VECTOR much slower on VAX; due to need to multiply
   by 4 to convert VECITM(V,i)=> V+4*(i+1) on VAX; if I work with P4=4*P,
   (CheatVtest), am getting code about as fast as C on the VAX for Vectors.


b) On VAX, string pointer of course just byte address, while on 20  have to
   unpack bytes, using LDB and ADJBP, so that STRING much slower than
   even on VAX!

26 March, tests of SIEVE.C and SIEVE.RED on MARS, vax-790
---------------------------------------------------------

100 loops of sieve of Eratosthenes, on 1000 length sieve.
This is a set of LOOPs with no procedure calls (in C or SYSLISP).

Test		C	Fast C	       PSL	 SYSLISP    SYSLISP/fast C

STRING	       3264      2941         66130        3519        1.2
VECTOR         3077      2720         26520        4284 (a)    1.6


On DEC-20, String                     33970        5970 (b)
           Vector                     11370        1896 (c)


Notes:

(a) on VAX, use 4*index as pointer, get 2618, and code similar to C.
(b) notice that this slower than VAX, since using LDB and ADJBP on 20
     but direct BYTE address on VAX.
(c) on 20, if we use pointer rather than index, get  1541 which is not as 
     dramatic as on the VAx, since not saving the 4* to convert index 
     to BYTE address
(d) Fast-C uses the -O code improvment option, and some  loops seem to use
    a AOBLEQ (on VAX, like AOBJN on 20).


May want to start thinking about Code-Gen improvments, and source to
source improvements to catch these and similar constructs. Discuss
with Mark, Jed, Bobbie

%  sieve.red -----
on comp;
Fluid '(Tim1 Tim2);

on syslisp;

procedure start();
 Lispvar(tim1) :=timc();

procedure done s;
 <<lispvar(tim2):=timc();
   printf(" ---- %p ---%p%n",s,lispvar(tim2)-lispvar(tim1));
>>;

procedure TestSL n;
begin scalar primes;
	primes := Mkstring(1000,1);
	start();
	for i:=1:n do Lsieve primes;
	done "lsieve, string";
 end;

procedure TestVL n;
begin scalar primes;
	primes := MkVect(1000);
	start();
	for i:=1:n do Lsieve primes;
	done "lsieve, vector";
 end;

procedure TestV n;
begin scalar primes;
	primes := Mkvect 1000;
	start();
	for i:=1:n do Vsieve primes;
	done "Vsieve";
 end;

procedure TestCheatV n;
begin scalar primes;
	primes := Mkvect 1000;
	start();
	for i:=1:n do CheatVsieve primes;
	done "CheatVsieve";
 end;

procedure TestS n;
begin scalar primes;
	primes := Mkstring(1000,1);
	start();
	for i:=1:n do Ssieve primes;
	done "Ssieve";
 end;

off syslisp;

lisp procedure lsieve(primes);
 begin
    scalar  p, mp;
    for i:=0:1000 do setindx(primes,1);
%    printf("Primes%n");
    for p := 2:1000 do
      if indx(primes, p) eq 1 then
      <<
%	printf("        %d%n", p);
	for mp := 2*p step p until 1000 do
	    setindx(primes, mp, 0)
      >>
end;

on syslisp;

syslisp procedure ssieve(primes);
begin
   scalar  p, mp;
    primes := strinf primes;
    for i:=0:1000 do strbyt(primes,i):=1;
%    printf("Primes%n");
    for p := 2:1000 do
      if strbyt(primes, p) eq 1 then
      <<
%	printf("        %d%n", p);
	for mp := 2*p step p until 1000 do
	    strbyt(primes, mp) := 0
      >>
end;

syslisp procedure vsieve(primes);
begin
    scalar  p, mp;
    primes := vecinf(primes);
    for p:=0:1000 do vecitm(vecinf primes,p):=1;
%    printf("Primes%n");
    for p := 2:1000 do
      if vecitm(primes, p) eq 1 then
      <<
%	printf("        %d%n", p);
	for mp := 2*p step p until 1000 do
	    vecitm(primes, mp) := 0
      >>

end;

syslisp procedure Cheatvsieve(primes);
begin
    scalar  p, p4, mp,mp4, base;
    primes := vecinf(primes);
	base := primes +addressingunitsperitem;
    p4:=  base +0;
    for p:=0:1000 do <<putmem(p4,1); p4:=p4+addressingunitsperitem>>;
%    printf("Primes%n");
    p4:=base+2*addressingunitsperitem;
    for p := 2:1000 do
    <<  if getmem( p4) eq 1 then
      <<
%	printf("        %d%n", p);
        mp4 := base +2*addressingunitsperitem*p;
	for mp := 2*p step p until 1000 do
	    <<putmem(mp4,0); mp4:=mp4+addressingunitsperitem >> >>;
      p4 :=p4 +addressingunitsperitem>>

end;


off syslisp;
end;

-------


