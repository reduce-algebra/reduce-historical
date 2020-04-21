% Simple 1 file test
% This is program MAIN1.RED


IN "XXX-HEADER.RED"$

On SYSLISP;

Procedure FirstCall;
 <<Init();
   PutC Char F;
   PutC Char !a;
   PutC Char !c;
   PutC Char !=;
   PutInt Ifact 10;
   Terpri();
   PutC Char T;
   PutC Char !e;
   PutC Char !s;
   PutC Char !t;
   PutC Char F;
   PutC Char !a;
   PutC Char !c;
   PutC Char !t;
   Terpri();
   TestFact();
   Terpri();
   PutC Char T;
   PutC Char !e;
   PutC Char !s;
   PutC Char !t;
   PutC Char T;
   PutC Char !a;
   PutC Char !k;
   Terpri();
   TestTak();
   Quit;>>;

procedure terpri();
   PutC Char EOL;

Procedure TestFact();
<< PutInt Timc(); 
   Terpri();
   ArithmeticTest 10000;
   PutInt Timc();
   Terpri();
>>;

Procedure ArithmeticTest (N);
 begin scalar I;
    I:= 0;
loop:
    if Igreaterp(I,N) then return NIL;
    Fact 9;
    I := iadd1 I;
    goto loop
end;

procedure TestTak();
 <<PutInt Timc();
   Terpri();
   PutInt TopLevelTak (18,12,6);
   Terpri();
   PutInt Timc();
   Terpri();>>;

syslsp procedure Fact (N);
 If ilessp(N,2) then  1 else LongTimes(N,Fact isub1 N);

syslsp procedure Ifact u;
 Begin scalar m;
   m:=1;
 L1: if u eq 1 then return M;
   M:=LongTimes(U,M);
   u:=u-1;
   PutInt(u);
   Terpri();
   PutInt(M);
   Terpri();
   goto  L1;
 end;

in "pt:tak.sl"$

off syslisp;

procedure UndefinedFunctionAux;
 <<Putc Char U;
   Putc Char !n;
   Putc Char !d;
   Putc Char !e;
   Putc Char !f;
   Putc Char Blank;
   Putint UndefnCode!*;
   Terpri();
   Quit;>>;
  end;

