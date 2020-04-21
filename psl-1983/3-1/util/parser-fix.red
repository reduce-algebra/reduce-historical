%7:51am  Sunday, 4 April 1982 Some parser fixes.

FLUID '(!*BREAK);

procedure ParErr(x,y);
 Begin Scalar !*BREAK; % Turn off BREAK
     StdError(x);
 End;

procedure ElseError x;
  ParErr("ELSE should appear only in IF statement",T);

procedure ThenError x;
  ParErr("THEN should appear only in IF statement",T);

DefineRop('THEN,4,ThenError);
DefineRop('ELSE,4,ElseError);

procedure DoError x;
  ParErr("DO should appear only in WHILE or FOR statements",T);

procedure UntilError x;
  ParErr("UNTIL should appear only in REPEAT statement",T);

DefineRop('Do,4,DoPError);
DefineRop('Until,4,UntilMError);

procedure SUMError x;
  ParErr("SUM should appear only in FOR statements",T);

procedure STEPError x;
  ParErr("STEP should appear only in FOR statement",T);

procedure ProductError x;
  ParErr("PRODUCT should appear only in FOR statement",T);

DefineRop('STEP,4,STEPError);
DefineRop('SUM,4,SUMError);
DefineRop('PRODUCT,4,ProductError);

procedure CollectError x;
  ParErr("COLLECT should appear only in FOR EACH statements",T);

procedure CONCError x;
  ParErr("CONC should appear only in FOR EACH statement",T);

procedure JOINError x;
  ParErr("JOIN should appear only in FOR EACH statement",T);

DefineRop('CONC,4,CONCError);
DefineRop('Collect,4,CollectError);
DefineRop('JOIN,4,JOINError);

% Parse Simple ATOM list

SYMBOLIC PROCEDURE ParseAtomList(U,V,W);  %. parse LIST of Atoms, maybe quoted
 % U=funcname, V=following Token, W=arg treatment
   BEGIN Scalar Atoms;
     IF V EQ '!*SEMICOL!* THEN 
        RETURN ParErr("Missing AtomList after KEYWORD",T);
    L:  Atoms:=V . Atoms;
        SCAN();
        IF CURSYM!* eq '!*COMMA!* then <<V:=SCAN(); goto L>>;
        IF CURSYM!* eq '!*SEMICOL!* then Return
          <<OP := CURSYM!*;
            If W eq 'FEXPR then U . Reverse Atoms
             else LIST(U,MkQuotList Reverse Atoms)>>;
        ParErr("Expect only Comma delimeter in ParseAtomList",T);
   END;

DefineRop('Load,NIL,ParseAtomList('Load,X,'Fexpr));
Definerop('A1,NIL,ParseAtomList('A0,X,'Expr));
Definerop('A2,NIL,ParseAtomList('A0,X,'FExpr));

procedure a0 x;
 print x;
