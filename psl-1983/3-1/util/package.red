%
% PACKAGE.RED - Start of small package system
%
% Author:      Martin Griss 
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        Friday, 23 October 1981
% Copyright (c) 1981 University of Utah
%

% Idea is that Hierachical ObLists created
% Permit Root at NIL, ie Forest Of Trees
% CurrentPackage!* is Name of package
% Structure [Name,Father,Getfn,PutFn,RemFn,MapFn] under 'Package
% Have set of Localxxxx(s) and Pathxxxx(s) for
%  xxxx= InternP Intern RemOb MapObl
% By Storing Functions, have possibility of different
%   Oblist models at each level (Abstract data Type for Local Obarray )

CompileTime <<
Lisp Procedure PACKAGE x;                %. Called from Token reader
   NIL;                %  dummy            % To chnge package
>>;

Fluid '(\CurrentPackage!*		 %. Start of Search Path
        \PackageNames!*                  %. List of ALL package names
	PackageCharacter!*		%. Character prefix for package
 );

PackageCharacter!* := char !\;		% used for output

Global '(SymPak!* MaxSym!*);             % Dummy Package Field, to be SYSLSP
<<MaxSym!*:=8000;
  SymPak!*:=Mkvect MaxSym!*; 
  MaxSym!*>>;

Lisp  procedure SymPak d;                % Access SYPAK field
  SymPak!*[d];

Lisp  procedure PutSymPak(d,v);
  SymPak!*[d]:=v;

CompileTime Put('SymPak,'Assign!-op,'PutSymPak);

% -Hook in GetFn,PutFn, RemFn and MapFn for \Global ------

CopyD('GlobalMapObl,'MapObl);

Lisp Procedure \SetUpInitialPackage;
Begin
 Put('\Global,'\Package, 
     '[\Global NIL \GlobalLookup \GlobalInstall \GlobalRemove \GlobalMapObl]);
 % Package is [name of self, father, GetFn, PutFn,RemFn,MapFn]
 \PackageNames!* := '(\Global);
 \CurrentPackage!* := '\Global;
End;

CompileTime <<
Lisp Smacro Procedure PackageName x;
  x[0];

Lisp Smacro Procedure PackageFather x;
  x[1];

Lisp Smacro Procedure PackageGetFn x;
  x[2];

Lisp Smacro Procedure PackagePutFn x;
  x[3];

Lisp Smacro Procedure PackageRemFn x;
  x[4];

Lisp Smacro Procedure PackageMapFn x;
  x[5];
>>;

\SetupInitialPackage();

Lisp Procedure \PackageP(Name);		%. test if legal package
  IdP(Name) and Get(Name,'\Package);

Lisp Procedure \CreateRawPackage(Name,Father, GetFn, PutFn, RemFn, MapFn); 
                  %. Build New Package
 Begin Scalar V;
      If \PackageP Name then 
        return ErrorPrintF("*** %r is already a package",Name);
      If Not \PackageP Father then
        return ErrorPrintF("*** %r cant be Father package",Father);
      V:=Mkvect(5);
      V[0]:=Name;
      V[1]:=Father;
      V[2]:=GetFn;
      V[3]:=PutFn; 
      V[4] := RemFn;
      V[5] := MapFn;
      \PackageNames!* := Name . \PackageNames!*;
      Put(Name,'\Package,V);
      Return V
 End;

Lisp Procedure \SetPackage(Name); 		%. Change Default
 If \PackageP(Name) then
    <<%PrintF(" Pack: %r->%r %n",\CurrentPackage!*,Name);
      \CurrentPackage!*:=Name>>

  else if Null Name then \SetPackage('\Global)
  else \PackageError(Name);

Lisp procedure \PackageError(Name);
 Error(99, LIST(Name, " Is not a Package "));

% Note that we have to cleanup to some default package if
% there is an error during ID name reading:

CopyD('UnSafeToken,'ChannelReadToken);

Lisp Procedure SafeToken(Channel);
  (LAMBDA (\CurrentPackage!*); UnSafeToken(Channel)) (\CurrentPackage!*);

CopyD('ChannelReadToken,'SafeToken);

Lisp Procedure PACKAGE x;                %. Called from Token reader
 \SetPackage x;

% --- User Package Stuff
% --- Simple Buck Hash, using PAIRs (could later use Blocks)

lisp Procedure HashFn(S,Htab);
begin scalar Len, HashVal;		% Fold together a bunch of bits
    S := StrInf S;
    HashVal := 0;			% from the first 28 characters of the
    Len := StrLen S;			% string.
    if IGreaterP(Len, 25) then Len := 25;
    for I := 0 step 1 until Len do
	HashVal := ILXOR(HashVal, ILSH(StrByt(S, I), IDifference(25, I)));
    return  IRemainder(HashVal, VecLen VecInf Htab);
end;

Lisp Procedure HashGetFn(S,Htab);         %. See if String S is There
 % Htab is Vector of Buckets
 Begin Scalar H,Buk,Hashloc;
    If not StringP S then Return NonStringError(S,'HashGetFn);
    HashLoc:=HashFn(S,Htab);
    Buk:=Htab[HashLoc];
Loop: If Null Buk then return 0;
      H:=Car Buk; Buk:=cdr Buk;
      If S=ID2String H then return H;
      goto Loop;
End;

Lisp Procedure HashPutFn(S,Htab);    %. Install String at HashLoc
 Begin Scalar H,TopBuk,Buk,HashLoc;
    If not StringP S then NonStringError(S,'HashPutFn);
    HashLoc :=HashFn(S,Htab);
    TopBuk:=Buk:=Htab[HashLoc];
Loop: If Null Buk then goto new;
      H:=Car Buk; Buk:=cdr Buk;
      If S=ID2String H then return H;
      goto Loop;
New:
    S:=CopyString S;   % So doesnt grab I/O buffer
    H:=NewID  S;
    SymPak(ID2Int H) := CurrentPackage!*;
    TopBuk:= H . TopBuk;
    Htab[HashLoc] := TopBuk;
    Return H;
End;

Lisp Procedure HashRemFn(S,Htab);    %. remove String if there
 Begin Scalar H,TopBuk,Buk,HashLoc;
    If not StringP S then Return NonStringError(S,'HashRemFn);
    HashLoc :=HashFn(S,Htab);
    TopBuk:=Buk:=Htab[HashLoc];
Loop: 
      If Null Buk then return 0;
      H:=Car Buk; Buk:=cdr Buk;
      If S=ID2String H then goto Rem;
      goto Loop;
Rem:
    Htab[HashLoc] :=DelQ(H,TopBuk);
    SymPak(ID2Int H) := NIL;
    Return H
End;

Lisp Procedure HashMapFn(F,Htab);
 Begin Scalar H,Buk,HashLoc,Hmax;
    Hmax:=UPBV Htab;
    For HashLoc:=0:Hmax do
      <<Buk:=Htab[HashLoc];
        For each H in Buk do Apply(F, List H)>>;
    Return Hmax;
End;


% -------- Generic routines over hash tables
% --- Local Only

Lisp procedure LocalIntern S;                %. Force Into Current Package
 If IDP S then return LocalIntern Id2String S
  else if not StringP S then NonStringError(S,'LocalIntern)
  else if CurrentPackage!* eq NIL
    or CurrentPackage!* eq '\Global then GlobalInstall S
  else begin scalar P,H;
       P:=Get(CurrentPackage!*,'\Package);
       H:=Apply(PackageGetFn P,list S);
       If IDP H then return H;   % already there
       Return Apply(PackagePutFn P,list S);
  End;

Lisp procedure LocalInternP S;                %. Test in Current Package
 If IDP S then LocalInternP ID2String S
  else if not StringP S then NonStringError(S,'LocalInternP)
  else if CurrentPackage!* eq NIL
    or CurrentPackage!* eq '\Global then GlobalLookup S
  else begin scalar P;
       P:=Get(CurrentPackage!*,'\Package);
       Return Apply(PackageGetFn P,list S);
  End;

Lisp procedure LocalRemOb S;                %. Remove from Current Package
 If IDP S then LocalRemob ID2String S
  else if not StringP S then NonStringError(S,'LocalRemob)
  else if CurrentPackage!* eq NIL
    or CurrentPackage!* eq '\Global then GlobalRemove S
  else begin scalar P,H;
       P:=Get(CurrentPackage!*,'\Package);
       Return Apply(PackageRemFn P,list S);
  End;

Lisp procedure LocalMapObl F;                %. Force Into Current Package
 if CurrentPackage!* eq NIL
    or CurrentPackage!* eq '\Global then GlobalMapObl F
  else begin scalar P;
       P:=Get(CurrentPackage!*,'\Package);
       Return Apply(PackageMapFn P,list F);
  End;

% Over Full Tree From CurrentPackage!*

Lisp procedure PathIntern S;                %. Do in Current If not Internd
 If IDP S then PathIntern ID2String S
  else if not StringP S then NonStringError(S,'PathIntern)
  else  if CurrentPackage!* eq NIL
    or CurrentPackage!* eq '\Global then GlobalInstall S  
  else begin scalar H,P;
      If IDP(H:=PathIntern1(S,CurrentPackage!*)) then return H;
      P:=Get(CurrentPackage!*,'\Package);
      Return Apply(PackagePutFn P,list S); % Do it at top level
  end;

Lisp Procedure PathIntern1(S,CurrentPackage!*); % Search Ancestor Chain
  if CurrentPackage!* eq NIL
    or CurrentPackage!* eq '\Global then GlobalLookup S
  else begin scalar P,H;
       P:=Get(CurrentPackage!*,'\Package);
       H:=Apply(PackageGetFn P,list S);
       If IDP H then return H;
       Return PathIntern1(S,PackageFather P); % try ancestor
  End;

Lisp Procedure AlternatePathIntern S;
 begin scalar H;
  H:=PathInternP S;
  If IDP H then return H;
  return LocalIntern S;
 End;

Lisp procedure PathInternP S;                %. TEST if Interned on Path
 PathInternP1(S,CurrentPackage!*);

Lisp Procedure PathInternP1(S,CurrentPackage!*);
 If IDP S then PathInternP1(ID2String S,CurrentPackage!*)
  else if not StringP S then NonStringError(S,'PathInternP)
   else  if CurrentPackage!* eq NIL
    or CurrentPackage!* eq '\Global then GlobalLookup S  
  else begin scalar P,H;
       P:=Get(CurrentPackage!*,'\Package);
       H:=Apply(PackageGetFn P,list S);
       If IDP H then return H;
       return PathInternP1(S,PackageFather P); % try ancestor
  End;

Lisp procedure PathRemOb S;                %. Remove First On Path
 PathRemOb1(S,CurrentPackage!*);

Lisp Procedure PathRemOb1(S,CurrentPackage!*);
 If IDP S then PathRemOb1(ID2String S,CurrentPackage!*)
  else if not StringP S then NonStringError(S,'PathRemob)
  else  if CurrentPackage!* eq  NIL
    or CurrentPackage!* eq '\Global then GlobalRemove S  
  else begin scalar P,H;
       P:=Get(CurrentPackage!*,'\Package);
       H:=Apply(PackageRemFn P,list S);
       If IDP H then return H;
       return PathRemob1(S,PackageFather P); % try ancestor
  End;

Lisp procedure PathMapObl F;                %.  Full path
 PathMapObl1(F,CurrentPackage!*);

Lisp procedure PathMapObl1(F,Pack);
 if Pack eq NIL
    or Pack  eq '\Global then GlobalMapObl F
  else begin scalar P,H;
       P:=Get(Pack,'\Package);
       Apply(PackageMapFn P,list F);
       Return PathMapObl1(F,PackageFather P);
  End;

% ---- Build default Htabs for Bucket Hashed Case

Lisp Procedure \CreateHashedPackage(Name,Father,n);
  Begin Scalar Gf,Pf,Rf,Mf,G;
     G:=Gensym();
     Set(G, Mkvect n);
     Gf:=Gensym();
     Pf:=Gensym();
     Rf:=Gensym();
     Mf:=Gensym();
     PutD(Gf,'Expr,LIST('Lambda,'(S),LIST('HashGetFn,'S,G)));
     PutD(Pf,'Expr,LIST('Lambda,'(S),LIST('HashPutFn,'S,G)));
     PutD(Rf,'Expr,LIST('Lambda,'(S),LIST('HashRemFn,'S,G)));
     PutD(Mf,'Expr,LIST('Lambda,'(F),LIST('HashMapFn,'F,G)));
     Return \CreateRawPackage(Name,Father,Gf,Pf,Rf,Mf);
End;

Lisp Procedure \CreatePackage(Name,Father);
 \CreateHashedPackage(Name,Father,100);

% ------ OutPut Functions

CopyD('OldCprin2,'ChannelPrin2);
CopyD('OldCprin1,'ChannelPrin1);
%/ Take Channel and Itm

Lisp Procedure NewCprin1(Channel,Itm);
If IDP Itm then
 Begin Scalar IDN,PN;
    IDN:=ID2Int Itm;
    PN:=SymPak IDN;
    If IDP PN and PN  then
      <<NewCprin1(Channel,PN);ChannelWriteChar(Channel,PackageCharacter!*)>>;
    OldCprin1(Channel,Itm);
 End
else OldCprin1(Channel,Itm);

Lisp Procedure NewCprin2(Channel,Itm);
If IDP Itm then
 Begin Scalar IDN,PN;
    IDN:=ID2Int Itm;
    PN:=SymPak IDN;
    If IDP PN and PN then
      <<NewCprin2(Channel,PN);ChannelWriteChar(Channel,PackageCharacter!*)>>;
    OldCprin2(Channel,Itm);
 End
else
    OldCprin2(Channel,Itm);

% ----- A simple Demo ---------------

Procedure redef;
Begin
 CopyD('Intern,'PathIntern );
 CopyD('InternP,'PathInternP );
 CopyD('RemOb ,'PathRemOb );
 CopyD('MapObl ,'PathMapObl);
 CopyD('ChannelPrin1,'NewCPrin1); 
 CopyD('ChannelPrin2,'NewCPrin2);
end;

CopyD('CachedGlobalLookup,'GlobalLookup);

Procedure GlobalLookup S;
 <<LastLookedUp:=NIL;          %/ Fix Cache Bug that always said YES
   CachedGlobalLookup S>>;

CopyD('NonCopyInstall,'GlobalInstall); % Some Bug in this too, clobers string
Procedure GlobalInstall(S);
 NonCopyInstall CopyString S;

Redef();

\CreatePackage('\P1,'\Global);
\CreatePackage('\P2,'\Global);

end;
