% MAIN3.RED - Test CASE and CONS
% Need:  SUB2.RED simple print routines
%        SUB3.RED simple allocator


IN "XXX-HEADER.RED"$
IN "PT:STUBS3.RED"$

on syslisp;


syslsp Procedure FirstCall;
  begin scalar X, Y;
    Init();
    Print '"MAIN3: Casetest"$
    CaseTest();
    Print '"MAIN3: test CONS"$
    InitHeap();
    ConsTest();
    quit;
end;

syslsp procedure CaseTest;
 <<Prin2t '"Test case from -1 to 11";
   Prin2t '"Will classify argument";
   Ctest (-1);
   Ctest 0;
   Ctest 1;
   Ctest 2;
   Ctest 3;
   Ctest 4;
   Ctest 5;
   Ctest 6;
   Ctest 7;
   Ctest 8;
   Ctest 9;
   Ctest 10;
   Ctest 11;
   Ctest 12>>;

syslsp procedure CTest N;
  Case N of
    0: Show(N,"0 case");
    1,2,3: Show(N,"1,2,3 case");
    6 to 10:Show(N,"6 ... 10 case");
    default:Show(N,"default case");
  end;

syslsp procedure Show(N,S);
 <<Prin2String "Show for N=";
   Prin1Int N;
   Prin2String ", expect ";
   Prin2String S;
   Terpri()>>;

Procedure CONStest();
 Begin scalar Z,N;
    Z:='1;
    N:='2;
    While N<10 do
      <<z:=cons(N,z);
        Print z;
        N:=N+1>>;
 End;

FLUID '(UndefnCode!* UndefnNarg!*);

syslsp procedure UndefinedFunctionAux; 
% Should preserve all regs
 <<Terpri();
   Prin2String "**** Undefined Function: ";
   Prin1ID LispVar UndefnCode!*;
   Prin2String " , called with ";
   Prin2  LispVar UndefnNarg!*;
   Prin2T " arguments";
   Quit;>>;

Off syslisp;

End;
