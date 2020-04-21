% A small timing test to compare DEC-20, VAX and Cray
% in syslisp and FORTRAN and C
% An iterative FACTORIAL

on comp;
on syslisp;

syslsp procedure IFAC n;
 begin scalar m;
     m:=1;
     while n >0 do <<m:=m*n; n := n-1>>;
     return m;
 end;

procedure NCALL(N,M);
 begin scalar tim1,tim2,i;
     tim1:=time();     
     while N>0 do <<i:=Ifac(m);n:=n-1>>;
     tim2:=time()-tim1; %/had bug if same tim
     printf(" took %p ms%n",tim2);
 end;


off syslisp;