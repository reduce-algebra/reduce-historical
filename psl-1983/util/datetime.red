% MAKE.RED

% Will read in two directories and compare them for DATE and TIME

% Segment a string into fields:

Procedure SegmentString(S,ch); % "parse" string in pieces at CH
 Begin scalar s0,sN,sN1, Parts, sa,sb;
   s0:=0; 
   sn:=Size(S);
   sN1:=sN+1;
 L1:If s0>sn then goto L2;
   sa:=NextNonCh(Ch,S,s0,sN);
   if sa>sN then goto L2;
   sb:=NextCh(Ch,S,sa+1,sN);
   if sb>SN1 then goto L2;
   Parts:=SubSeq(S,sa,sb) . Parts;
   s0:=sb;
   goto L1;
  L2:Return Reverse Parts;
 End;

Procedure NextCh(Ch,S,s1,s2);
 <<While (S1<=S2) and not(S[S1] eq Ch) do s1:=s1+1;
   S1>>;

Procedure NextNonCh(Ch,S,s1,s2);
 <<While (S1<=S2) and (S[S1] eq Ch)  do s1:=s1+1;
   S1>>;
   
Fluid '(Months!*);

Months!*:='(
            ("JAN" . 1) ("FEB" . 2) ("MAR" . 3)
            ("APR" . 4) ("MAY" . 5) ("JUN" . 6)
            ("JUL" . 7) ("AUG" . 8) ("SEP" . 9)
            ("OCT" . 10) ("NOV" . 11) ("DEC" . 12)
            ("Jan" . 1) ("Feb" . 2) ("Mar" . 3)
            ("Apr" . 4) ("May" . 5) ("Jun" . 6)
            ("Jul" . 7) ("Aug" . 8) ("Sep" . 9)
            ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)
);

Procedure Month2Integer m;
 cdr assoc(m,Months!*);

Procedure DateTime2IntegerList(wdate,wtime);
  Begin Scalar V;
    V:=0;
    wdate:=SegmentString(wdate,char '!-);
    wtime:=SegmentString(wtime,char '!:);
    Rplaca(cdr WDate,Month2Integer Cadr Wdate);
    wdate:=MakeNumeric(wdate);
    wtime:=MakeNumeric(wtime);
    return append(wdate , wtime);
 end;

 procedure MakeNumeric(L);
  If null L then NIL
   else    
     String2Integer(car L) . MakeNumeric(cdr L);

 procedure String2Integer S;
  if numberP s then s
   else if stringp s then MakeStringIntoLispInteger(s,10,1)
   else StdError "Non-string in String2Integer";

procedure CompareIntegerLists(L1,L2);  % L1 <= L2
 If Null L1 then T
  else if Null L2 then Nil
  else if Car L1 < Car L2 then T
  else if Car L1 > Car L2 then NIL
  else CompareIntegerLists(cdr L1, cdr L2);

end;
