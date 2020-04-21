% READ-TABLE-UTILS.RED -  Read Table Utils
% 
% Author:      M. L. Griss
%              Computer Science Dept.
%              University of Utah
% Date:        28 August 1981
% Copyright (c) 1981 University of Utah

% NOTE: Rather Crude, needs some work.

% Edit by Cris Perdue, 28 Jan 1983 2040-PST
% Occurrences of dipthong changed to diphthong

Fluid '( CharacterClass!* );

Lisp procedure PrintScanTable (Table);
 Begin Scalar I;
	I := 0;
	For I :=0:127 do
	     <<Prin1 I;
               TAB 5;
	       prin2 Int2Id I;
	       Tab 15;
               print CharacterClass!*[Table[I]] >>;
       PrintF(" Diphthong    name: %r%n",Table[128]);
%/       PrintF(" ReadMacro   name: %r%n",Table[129]);
%/       PrintF(" SpliceMacro name: %r%n",Table[130]);
  End;
%%% Some id names for the classes

Lisp Procedure CopyScanTable(OldTable);
 Begin
     If Null OldTable then OldTable:=CurrentScanTable!*;
     If not (vectorp OldTable and UpbV(oldTable)=130) then
        return StdError "CopyScanTable expects a valid Readtable";
     OldTable:=Copy OldTable;
     OldTable[128]:=Gensym();
     OldTable[129]:=Gensym();
     OldTable[130]:=Gensym();
     Return OldTable;
 End;

LoadTime <<
CharacterClass!*:=
'[Digit Digit Digit Digit Digit Digit Digit Digit Digit Digit 
 Letter Delimiter Comment Diphthong IdEscape StringQuote Package Ignore
 Minus Plus Decimal];

Put('Letter, 'CharacterClass!*, 10);
Put('Delimiter, 'CharacterClass!*, 11);
Put('Comment, 'CharacterClass!*, 12);
Put('Diphthong, 'CharacterClass!*, 13);
Put('IdEscape, 'CharacterClass!*, 14);
Put('StringQuote, 'CharacterClass!*, 15);
Put('Package, 'CharacterClass!*, 16);
Put('Ignore, 'CharacterClass!*, 17);
Put('Minus, 'CharacterClass!*, 18);
Put('Plus, 'CharacterClass!*, 19);
Put('Decimal, 'CharacterClass!*, 20) >>;

Lisp procedure PutCharacterClass(Table,Ch,Val);
  ChangeCharType(Table,Ch,Val);

Symbolic Procedure ChangeCharType(TBL,Ch,Ty);	%. Set Character type
begin scalar IDNum;
 If IdP Ty then Ty := Get(Ty,'CharacterClass!*);
 If IDP Ch  and (IDNum := ID2Int Ch) < 128 and 
		Numberp Ty and Ty >=0 and Ty <=20 then
  PutV(TBL,IDNum,Ty)
 Else Error(99,"Cant Set ReadTable");
end;

Symbolic Procedure PutDiphthong(TBL,StartCh, FollowCh, Diphthong);
 If IDP Startch and IDP FollowCh and IDP Diphthong
  then <<ChangeCharType(TBL,StartCh,13);
         PUT(StartCh,TBL[128],
             (FollowCh . Diphthong) . GET(StartCh,TBL[128]))>>
 else Error(99, "Cant Declare Diphthong");

Symbolic Procedure MakeDiphthong(TBL,DipIndicator,StartCh, FollowCh, Diphthong);
 If IDP Startch and IDP FollowCh and IDP Diphthong
  then <<ChangeCharType(TBL,StartCh,13);
         PUT(StartCh,DipIndicator,
             (FollowCh . Diphthong) . GET(StartCh,DipIndicator))>>
 else Error(99, "Cant Declare Diphthong");

Lisp procedure PutReadMacro(Table,x,Fn);
  Begin 
      If not IdP x then IdError(x,'PutReadMacro);
      If Not IdP Fn then return IDError(x,'PutReadMacro);
      % Check Delimiter Class as 11 or 23
      Put(x,Table[129],Fn);
      Remprop(x,Table[130]);
 End;

%/ Splice macros currently "frowned" upon

Lisp procedure PutSpliceMacro(Table,x,Fn);
  Begin 
      If not IdP x then IdError(x,'PutSpliceMacro);
      If Not IdP Fn then return IDError(x,'PutSpliceMacro);
      % Check Delimiter Class as 11 or 13
      Put(x,Table[130],Fn);
      Remprop(x,Table[129]);
 End;

end;
