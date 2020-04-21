% MINI-TOKEN.RED - Small Token scanner for testing

CompileTime <<GLOBAL '(DEBUG);
              FLUID '(TOK!* TOKTYPE!* CH!* !*RAISE);>>;

ON SYSLISP;

Wstring Buffer[100];
 % Will hold characters as they are parsed for ID, INT and string

Procedure InitRead;
 % Initialize various RATOM and READ properties
 Begin
    LISPVAR(!*RAISE) := 'NIL;
    LISPVAR(CH!*) := Char '! ;
    LispVar(Tok!*):= 'NIL;
    LispVar(TokType!*) := 2;
    If LispVar(DEBUG) then  <<Prin2 '"NextSymbol ="; Print Nextsymbol>>;
 End;

Procedure SetRaise x;
     LISPVAR(!*RAISE) := x;

Procedure Ratom;
 % Read a single ATOM: ID, POSINT, STRING or SPECIAL
 Begin 
      ClearWhite();
      If LispVar(CH!*) eq Char '!% then ClearComment();      	
      If LISPVAR(CH!*) eq Char '!"
        then Return <<LispVar(TokType!*):=0;LispVar(Tok!*):=ReadStr()>>;
      If DigitP LISPVAR(CH!*) 
       then Return <<LispVar(TokType!*):=1;LispVar(Tok!*):=ReadInt()>>;
      If AlphaEscP LISPVAR(CH!*)
        then Return <<LispVar(TokType!*):=2;LispVar(Tok!*):=ReadId()>>;
      LispVar(TokType!*):=3;
      LispVar(Tok!*):=MkItem(ID,LISPVAR(CH!*));
      LISPVAR(CH!*):=Char '! ; % For read Ahead
      Return LispVar(Tok!*)
 End;

Procedure ClearWhite();
% Clear out white space
   While WhiteP LISPVAR(CH!*) do LISPVAR(CH!*):=GetC();

Procedure ClearComment();
% Scan for Comment EOL
<< While LispVar(CH!*) neq char EOL do LISPVAR(CH!*):=GetC();
   ClearWhite()>>;

Procedure ReadInt;
% Parse NUMERIC characters into a POSITIVE integer
 Begin scalar N;
    N:=LISPVAR(CH!*)-Char 0;
    While DigitP(LISPVAR(CH!*):=GetC()) 
       do N:=LongTimes(10,N)+(LISPVAR(CH!*)-Char 0);
    Return Mkitem(POSINT,N);
 End;

Procedure BufferToString n;
% Convert first n chars of Buffer into a heap string
 Begin scalar s;
    s:=GtStr(n);
    for i:=0:n do strbyt(s,i):=strbyt(Buffer,i);
    return MkStr s;
 End;

Procedure ReadStr;
% Parse "...." into a heap string
 Begin scalar n;
  n:=-1;
  While ((LISPVAR(CH!*):=Getc())neq Char '!") 
    do <<N:=N+1;Strbyt(Buffer,n):=LISPVAR(CH!*)>>;
  LISPVAR(CH!*):=char '! ;
  Return BufferToString(n);
 End;

Procedure ReadID;
% Parse Characters into Buffer, Make into an ID
 Begin scalar n,s,D;
  n:=0;
  StrByt(Buffer,0):=RaiseChar LISPVAR(CH!*);
  While AlphaNumEscP(LISPVAR(CH!*):=Getc()) 
    do <<N:=N+1;Strbyt(Buffer,n):=RaiseChar LISPVAR(CH!*)>>;
  Return Intern BufferToString(n);
 End;


Procedure RaiseChar c;
 If EscapeP c then Getc()
 else if not LispVar !*Raise then c
  else if not AlphaP c then c
  else if LowerCaseP c then Char A +(c-Char Lower a)
  else c;

Procedure Intern s;
 % Lookup string, find old ID or return a new one
 Begin scalar D;
  If IDP s then s :=SymNam IdInf s;
  If (D:=LookupId( s)) then return MkItem(ID,D);
  D:=GtId();
  If LispVar(DEBUG) then <<Prin2 '"New ID# ";  Print D>>;
  Return  InitNewId(D,s);
End;

Procedure InitNewId(D,s);
Begin
  Symval(D):=NIL;
  SymPrp(D):=NIL;
  SymNam(D):=MkItem(Str,s);
  D:=MkItem(ID,D);
  MakeFUnBound(D); % Machine dependent, in XXX-HEADER
  Return D;
 End;

Procedure LookupId(s);
 % Linear scan of SYMNAM field to find string s
 Begin scalar D;
     D:=NextSymbol;
     If LispVar(DEBUG) then  
       <<Prin2 '"Lookup string=";Prin1String s; Terpri()>>;
  L: If D<=0 then  return
        <<If LispVar(DEBUG) then Prin2T '"Not Found in LookupId";  
          NIL>>;
      D:=D-1;
      If EqStr(SymNam(D),s) then return 
        <<If LispVar(DEBUG) then <<Prin2 '"Found In LookUpId="; print D>>;
          D>>;
    goto L
  End;

Procedure WhiteP x;
  x=CHAR(BLANK) or x=CHAR(EOL) or x=CHAR(TAB) or x=CHAR(LF)
   or x=CHAR(FF) or x =CHAR(CR);

Procedure DigitP x;
  Char(0) <=x and x <=Char(9);

Procedure AlphaP(x);
  UpperCaseP x or LowerCaseP x;

Procedure UpperCaseP x;
  Char(A)<=x and x<=Char(Z);

Procedure LowerCaseP x;
  Char(Lower A)<=x and x<=Char(Lower Z);

Procedure EscapeP x;
  x eq Char '!!;

Procedure AlphaEscP x;
 EscapeP x or AlphaP x;

Procedure AlphaNumP x;
  DigitP(x) or AlphaP(x);

Procedure AlphaNumEscP x;
  EscapeP x or AlphaNumP x;

Off syslisp;

End;
