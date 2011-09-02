module substexp;

algebraic;

depend ff,x;
depend f,x;
depend f,!~k;

operator pssubst;

operator a,Pproduct;

subst_rules :=

{ pssubst(- ~g,~x,~a,~n) => -pssubst(g,x,a,n),
  pssubst(~g+~h,~x,~a,~n) => pssubst(g,x,a,n) + pssubst(h,x,a,n),
  pssubst(~c*~g,~x,~a,~n) => c*pssubst(g,x,a,n) 
  			when freeof(c,x) and freeof(c,g),
  pssubst(df(~f,~x,~k),~x,~a,~n) => Pochhammer(n+1,k) * a(n+k),
  pssubst(df(~f,~x),~x,~a,~n) => (n + 1)* a(n + 1),
  pssubst(~x^~j * df(~f,~x),~x,~a,~n) => Pochhammer(n+1-j,1)*a(n+1-j),
  pssubst(~x^~j * df(~f,~x,~k),~x,~a,~n)=> Pochhammer(n+1-j,k)*a(n+k-j),

  pssubst(ff*~x^~j,~x,~a,~n) => a(n-j),
  pssubst(ff*~x,~x,~a,~n)    => a(n-1),

  pssubst(df(~f,~x) *x,~x,~a,~n) => Pochhammer(n,1)*a(n),
  pssubst(df(~f,~x,~k) *x,~x,~a,~n) => Pochhammer(n,k)*a(n+k-1),

  pssubst(f,~x,~a,~n) => a(n),
  pssubst(ff,~x,~a,~n) => a(n),
 
  pssubst(~c,~x,~a,~n) => 0 when freeof(c,x),
  pssubst(~x^~j,~x,~a,~n) => 0 when fixp j,
  pssubst(~x,~x,~a,~n) => 0
  };

spec_pochhammer :=

{ Pochhammer(~a,~k)//Pochhammer(~b,~k) => (a + k -1)/(a - 1)
     when (a - b)=1,
  Pochhammer(~a,~k)//Pochhammer(~b,~k) => (b - 1)/(b + k -1)
     when (b - a)=1,
  Pochhammer(~z,~k) * Pochhammer(~cz,~k) =>
     prod((repart(z) + (j - 1))^2 + (impart(z))^2,j,1,k)
	when not(impart(z) = 0) and z = conj cz,
  Pochhammer(~k,~n) => 1 when n=0,
  Pochhammer(~k,~n) => Pproduct (k,n) when fixp n,
  Pproduct (~k,~ii) => 1 when ii =0,
  Pproduct (~k,~ii) => (k + ii - 1) * Pproduct (k,ii -1)}$

spec_factorial :=

{ Factorial (~n) // Factorial (~n+1) => 1/(n+1),
  Factorial (~n) * Factorial (~n) // Factorial (~n+1) =>
				Factorial (n)/(n+1),
  Factorial (~n+1) // Factorial (~n) => (n+1),
  Factorial (~n+1) * Factorial (~n+1) // Factorial (~n) =>
				(n+1) * Factorial (~n+1),
  (~otto ^(~k)) * Factorial (~n) // Factorial (~n +1) => otto^k  /(n+1),
  (~otto ^(~k)) * Factorial (~n+1) // Factorial (~n) =>  otto^k * (n+1),
  (~otto ^~k) * ~hugo * Factorial (~n) // Factorial (~n +1) =>
      otto^k * hugo/(n+1),
  (~otto ^~k) * ~hugo * Factorial (~n+1) // Factorial (~n) =>
      otto^k * hugo *(n+1)}$

endmodule;

end;


