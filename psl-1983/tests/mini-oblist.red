% MINI-OBLIST.RED

on syslisp;
% ---- Small MAPOBL and printers

Procedure MapObl(Fn);
 For i:=0:NextSymbol-1 do IdApply1(MkItem(ID,I),Fn);

Procedure PrintFexprs;
 MapObl 'Print1Fexpr;

Procedure Print1Fexpr(x);
 If FexprP x then Print x;

Procedure PrintFunctions;
 MapObl 'Print1Function;

Procedure Print1Function(x);
 If Not FUnboundP x then Print x;

off syslisp;

End;
