% Simple 1 file test
% This is program MAIN1.RED

On SYSLISP;

IN "XXX-HEADER.RED"$

Procedure FirstCall;
 <<Init();
   PutC Char A;
   PutC Char B;
   Terpri();
   PutInt Ifact 10;
   Terpri();
   TestFact();
   Terpri();
   TestTak();
   Quit;>>;

procedure terpri();
   PutC Char EOL;

Procedure TestFact();
<< Timc(); 
   Terpri();
   ArithmeticTest 10000;
   Timc();>>;

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
 <<Timc();
   PutInt TopLevelTak (18,12,6);
   Terpri();
   Timc();>>;

in "pt:tak.sl";

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

end;

