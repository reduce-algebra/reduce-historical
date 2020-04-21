%F PT MINI-OBLIST RED  18-MAR-83

on syslisp;

internal WConst DeletedSlotValue = -1,
		EmptySlotValue = 0;


syslsp procedure Intern s;
 % Lookup string, find old ID or return a new one
 Begin scalar D;
  If IDP s then s :=SymNam IdInf s;
  If (D:=LookupString( s)) then return MkItem(ID,D);
  Return NewId s;
End;

syslsp procedure NewId S;
   InitNewId(GtId(),s);

Syslsp procedure InitNewId(D,s);
Begin
  If LispVar(DEBUG) then <<Prin2 '"New ID# ";  Print D>>;
  Symval(D):=NIL;
  SymPrp(D):=NIL;
  SymNam(D):=s;
  D:=MkItem(ID,D);
  MakeFUnBound(D); % Machine dependent, in XXX-HEADER
  Obarray(D):=D;   % For GC hook
  Return D;
 End;


Syslsp procedure LookupString(s);
 % Linear scan of SYMNAM field to find string s
 Begin scalar D;
     D:=NextSymbol;
     If LispVar(DEBUG) then  
       <<Prin2 '"Lookup string=";Prin1String s; Terpri()>>;
  L: If D<=0 then  return
        <<If LispVar(DEBUG) then Prin2T '"Not Found in LookupString";  
          NIL>>;
      D:=D-1;
      If EqStr(SymNam(D),s) then return 
        <<If LispVar(DEBUG) then <<Prin2 '"Found In LookupString="; print D>>;
          D>>;
    goto L
  End;


% ---- Small MAPOBL and printers


Syslsp procedure MapObl(Fn);
 For i:=0:NextSymbol-1 do IdApply1(MkItem(ID,I),Fn);

Syslsp procedure PrintFexprs;
 MapObl 'Print1Fexpr;

Syslsp procedure Print1Fexpr(x);
 If FexprP x then Print x;

Syslsp procedure PrintFunctions;
 MapObl 'Print1Function;

Syslsp procedure Print1Function(x);
 If Not FUnboundP x then Print x;

syslisp procedure InitObList();
% Dummy, non hashed version
 Begin scalar Tmp;
	For i:=0 step 1 until MaxObarray do
	  ObArray I := EmptySlotValue;
	Tmp:= NextSymbol -1;
	For I := 128 step 1 until Tmp do
	  ObArray I := I;
  End;

off syslisp;

End;
