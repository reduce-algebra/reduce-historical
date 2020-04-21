% NBTEST.RED - Test Bignum Numeric transition points
% 	       And other numeric tests
% M. L. Griss, 6 Feb 1983

procedure fact N;
 Begin scalar m;
	m:=1;
	while n>0 do <<m:=m*n; n:=n-1>>;
	return m;
 End;

on syslisp;

syslsp procedure Ifact N;
 Begin scalar m;
	m:=1;
	while n>0 do <<m:=m*n; n:=n-1>>;
	return m;
 End;

syslsp procedure ftest(n,m);
 for i:=1:n do fact m;

syslsp procedure Iftest(n,m);
 for i:=1:n do ifact m;

off syslisp;

procedure Ntest0;
  Begin scalar n;
	N:=36;
	pos:=mkvect n; 
	neg:=mkvect n;
        pos[0]:=1; neg[0]:=-1;
        for i:=1:N do <<pos[i]:=2*pos[i-1];
                         neg[i]:=(-pos[i])>>;
end;

procedure show0 n;
<<show(n,pos,'ntype0);
  show(n,neg,'ntype0)>>;

procedure Ntest1;
  Begin scalar n;
	N:=40;
	newpos:=mkvect n; 
	newneg:=mkvect n;
        newpos[0]:=1; newneg[0]:=-1;
        for i:=1:n do <<newpos[i]:=2*newpos[i-1];
                        newneg[i]:=(-newpos[i])>>;
end;

procedure show1 n;
<<show(n,newpos,'ntype1);
  show(n,newneg,'ntype1)>>;

on syslisp;

procedure NType0 x;
 case tag x of
	posint: 'POSINT;
	negint: 'negint;
	fixn: 'FIXN;
	bign: 'BIGN;
	fltn: 'fltn;
	default: 'NIL;
 end;

procedure NType1 x;
 if Betap x and x>=0 then 'POSBETA
  else if Betap x and x<0 then 'NEGBETA
  else  case tag x of
	posint: 'POSINT;
	negint: 'negint;
	fixn: 'FIXN;
	bign: 'BIGN;
	fltn: 'fltn;
	default: 'NIL;
 end;

off syslisp;

procedure show(N,v,pred);
 for i:=0:N do
   printf("%p%t%p%t%p%t%p%n",i,5,apply(pred,list(v[i])),20,v[i],40,float v[i]);

end;



