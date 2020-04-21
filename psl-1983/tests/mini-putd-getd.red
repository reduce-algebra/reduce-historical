
% MINI-PUTD-GETD.RED Small COPYD, GETD, PUTD

on syslisp;

Procedure Getd(fn);
 Begin scalar type;
    if Not IDP fn then return
       <<Prin2 "*** Can only GETD off ID's: ";
         Print fn;
         NIL>>;
    if FunBoundP fn then return NIL;
    if null(type:=Get(fn,'Ftype)) then type:='Expr;
    if FCodeP fn then return ( type . GetFcodePointer fn);
    If FLambdaLinkP fn then return (type .Get(fn,'!*LambdaLink));
    Prin2 "*** GETD should find a LAMBDA or CODE";
    print fn;
    return NIL;
 End;

Procedure PutD(fn,type,body);
 Begin
    if Not IDP fn then return
       <<Prin2 "*** Can only define ID's as functions: ";
         Print fn;
         NIL>>;
    if FCodeP fn then 
       <<Prin2 "*** Redefining a COMPILED function: ";
         Print fn>>
     else if not FunBoundP fn then
       <<prin2 " Redefining function ";
         print fn>>;
    Remprop(fn,'!*LambdaLink);
    Remprop(fn,'Ftype);
    MakeFUnBound fn;
    If LambdaP body then
      << Put(fn,'!*LambdaLink,body);
         MakeFlambdaLink fn>>
     else if CodeP body then
          MakeFcode(fn,body)
     else return  <<Prin2 "*** Body must be a LAMBDA or CODE";
                    prin1 fn; prin2 " "; print body; NIL>>;
    If not(type eq 'expr) then Put(fn,'Ftype,type);
    return fn;
 End;

off syslisp;

End;
