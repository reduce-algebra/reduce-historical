% MINI-EASY-SL.RED --- Simple functions

% 3.1 -- Some basic predicates
% Note that the bodies open compile, so this is just for
% interpreter entries

Procedure Atom x;
  Atom x;

procedure ConstantP U;
  Not PairP U and not IDP U;

Procedure Null U;
  U eq NIL;

% 3.2 -- Simple LIST stuff

nexpr procedure List x;
 x;


% 3.5 -- Function definition

fexpr Procedure De(x);
  PutD(car x,'Expr,'LAMBDA . cdr x);

fexpr Procedure Df(x);
  PutD(car x,'Fexpr,'LAMBDA . Cdr x);

fexpr Procedure Dn(x);
  PutD(car x,'NExpr,'LAMBDA . cdr x);

fexpr Procedure Dm(x);
  PutD(car x,'Macro,'LAMBDA . Cdr x);

% 3.6 -- Variables and Binding

Fexpr Procedure SETQ a;
 Set(car a,Eval Cadr a);

% 3.7 -- Program function features

fexpr procedure Progn x;
  EvProgn x;


procedure EvProgn fl;
  Begin scalar x;
    While PairP fl do <<x:=Eval Car fl;
                        fl:=Cdr fl>>;
    Return x;
  End;

% 3.10 -- Boolean functions

procedure EvCond fl;
  if not PairP fl then 'NIL
   else if not PairP car fl then EvCond cdr fl
   else if Eval car car fl then EvProgn cdr car fl
   else EvCond cdr fl;

fexpr procedure Cond x;
  EvCond x;

procedure Not U;
  U eq NIL;

% 3.13 -- Composite

Procedure append(U,V);
 if not PairP U then V
  else Cons(Car U,Append(Cdr U,V));

Procedure MemQ(x,y);
 If Not PAIRP y then NIL
  else if x EQ car y then T
  else MemQ(x, cdr y);

Procedure REVERSE U;
 Begin Scalar V;
   While PairP U do <<V:=CONS(Car U,V); 
                      U:=CDR U>>;
   Return V;
 End;

% Simple EVAL support

procedure Evlis x;
 if Not Pairp x then x
  else Eval(car x) . Evlis(cdr x);

Fexpr Procedure Quote a;
 Car a;

End;

