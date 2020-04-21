% MINI-EVAL-APPLY.RED - A small EVAL, uses P-APPLY-LAP

On syslisp;

Procedure InitEval;
 Begin
     Put('Quote,'TYPE,'FEXPR);
     Put('Setq,'TYPE,'FEXPR);
     Put('Cond,'TYPE,'FEXPR);
     Put('Progn,'TYPE,'FEXPR);
     Put('While,'TYPE,'FEXPR);
     Put('List,'TYPE,'NEXPR);
     Put('De,'TYPE,'FEXPR);
     Put('Df,'TYPE,'FEXPR);
     Put('Dn,'TYPE,'FEXPR);
     Put('Dm,'TYPE,'FEXPR);
 End;

syslsp procedure Eval x;
 If IDP x then SYMVAL(IdInf x)
  else if not PairP x then x
  else begin scalar fn,a,FnType;
     fn:=car x; a:=cdr x;
     if LambdaP fn then Return LambdaEvalApply(GetLambda fn, a);
     if CodeP fn then Return CodeEvalApply(fn,a);
     if not Idp fn then Return <<Prin2('"**** Non-ID function in EVAL: ");
                                 Print fn;
                                 NIL>>;
     if FunBoundP fn then Return <<Prin2('"**** UnBound Function in EVAL: ");
                                   Print fn;
                                   NIL>>;
     FnType :=GetFnType Fn;

     if FnType = 'FEXPR then  return IDApply1(a, Fn); 
     if FnType = 'NEXPR then  return IDApply1(Evlis a, Fn); 
     if FnType = 'MACRO then  return Eval IDApply1(x, Fn); 

     if FLambdaLinkP fn then return LambdaEvalApply(GetLambda fn,a);
     return CodeEvalApply(GetFcodePointer fn, a);
  end;


procedure Apply(fn,a);
 Begin scalar N;
  If LambdaP fn then return LambdaApply(fn,a);
  If CodeP fn then CodeApply(fn,a);
  If Not Idp Fn then return
        <<prin2 '" **** Non-ID function in APPLY: ";
          prin1 fn; prin2 " "; Print a;
          NIL>>;
  if FLambdaLinkP fn then return LambdaApply(GetLambda fn,a);
  If FunBoundP Fn then return
        <<prin2 '" **** Unbound function in APPLY: ";
          prin1 fn; prin2 " "; Print a;
          NIL>>;
  Return CodeApply(GetFcodePointer Fn,a);
End;

% -- User Function Hooks ---
Procedure LambdaApply(x,a);
 Begin scalar v,b;
   x:=cdr x;
   v:=car x;
   b:=cdr x;
   Return DoLambda(v,b,a)
 End;

Procedure LambdaEvalApply(x,y);
  LambdaApply(x,Evlis y);

Procedure DoLambda(vars,body,args);
% Args already EVAL'd as appropriate
 Begin scalar N,x,a;
     N:=Length vars;
     For each v in VARS do
        <<if pairp args then <<a:=car args; args:=cdr args>>
           else a:=Nil;
          LBIND1(v,a)>>;
%/ Should try BindEVAL here
     x:=EvProgn Body;
     UnBindN N;
     Return x;
End;


Procedure LambdaP(x);
 EqCar(x,'LAMBDA);

Procedure GetLambda(fn);
  Get(fn,'!*LambdaLink);

off syslisp;

End;
