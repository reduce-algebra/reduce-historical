% STUBS5.RED - Stubs for TEST5 and Above

Fluid '(UndefnCode!* UndefnNarg!*);
on syslisp;

syslsp procedure UndefinedFunctionAuxAux;
% Interim version of UndefinedFunctionAux;
 Begin scalar FnId,Nargs;
    Nargs:=LispVar UndefnNarg!*;
    FnId := MkID (LispVar UndefnCode!*);
    Prin2 "Undefined Function ";
      Prin1 FnId;
       Prin2 " called with ";
        Prin2 Nargs;
         prin2T " args from compiled code";
     Quit;
  End;


% Some SYSLISP tools for debugging:

syslsp procedure INF x;
  Inf x;

syslsp procedure TAG x;
  TAG x;

syslsp procedure MKITEM(x,y);
  MkItem(X,y);

off syslisp;

End;


