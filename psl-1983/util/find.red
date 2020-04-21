%. FIND.RED - Start of recognition and search OBLIST functions
%. M. L. Griss

% 30 Dec 1982, Mlg
%	Move IMPORTS etc to BUILD file

Fluid '(CollectID!* TestString!*);

Lisp Procedure FindPrefix(TestString!*);	%. Scan ObLIST for prefix
 Begin 
	CollectId!*:=NIL;
	If IDp TestString!* then TestString!*:=ID2String TestString!*;
	If Not StringP TestString!* 
	 then StdError "Expect String or ID in FindPrefix";
	MapObl Function FindPrefix1;
	Return IDSort CollectId!*
 end;

Lisp procedure FindPrefix1 x;
 If IsPrefixString(TestString!*,ID2String x)
   then CollectId!* := x . CollectId!*;

Lisp Procedure FindSuffix(TestString!*); %. Scan ObLIST for prefix
 Begin 
	CollectId!*:=NIL;
	If IDp TestString!* then TestString!*:=ID2String TestString!*;
	If Not StringP TestString!* 
	 then StdError "Expect String or ID in FindPrefix";
	MapObl Function FindSuffix1;
	Return IDSort CollectId!*
 end;

Lisp procedure FindSuffix1 x;
 If IsSuffixString(TestString!*,ID2String x)
   then CollectId!* := x . CollectId!*;

Lisp procedure IsPrefixString(s1,s2);	%. test if exact string prefix
 begin scalar l1,l2,L;
	l1:=size s1; 
        l2:=size s2; 
        L:=0;
    	if l1> l2 then return NIL;
    Loop: if not( s1[L] eq s2[L] ) then return NIL;
	  if (L:=add1 L)> L1 then return T;
	  goto loop;
 end;

Lisp procedure IsSuffixString(s1,s2);	%. test if exact string prefix
 begin scalar l1,l2,L;
	l1:=size s1; 
        l2:=size s2; 
    	if l1> l2 then return NIL;
    Loop: if not( s1[L1] eq s2[L2] ) then return NIL;
	  if L1<=0 then return T;
	  l1:=L1-1;
	  L2:=L2-1;
	  goto loop;
 end;

% More extensive String matcher

procedure StringMatch(p,s);
  StringMatch1(p,0,size(p),s,0,size(s));

procedure StringMatch1(p,p1,p2,s,s1,s2);
 Begin scalar c;
  L1: % test Range
    if p1>p2 then
        return (if s1>s2 then T else NIL)
      else if s1>s2 then return NIL;

      % test if % something
     if (c:=p[p1]) eq char !% then goto L3;

  L2: % exact match
     if c eq s[s1] then <<p1:=p1+1;
                            s1:=s1+1;
                            goto L1>>;
      return NIL;

  L3: % special cases
      p1:=p1+1;
      if p1>p2 then return stderror "pattern ran out in % case of StringMatch";
      c:=p[p1];
      if c eq char !% then goto L2;
      if c eq char !? then <<p1:=p1+1;
                             s1:=s1+1;
                             goto L1>>;

      if c eq char !* then  % 0 or more vs 1 or more
       return <<while not(c:=StringMatch1(p,p1+1,p2,s,s1,s2)) and s1<=s2
                  do s1:=s1+1;
                c>>;
      Return Stderror Bldmsg(" %% %r not known in StringMatch",int2id c);
 end;

Lisp Procedure Find(TestString!*);		%. Scan ObLIST for prefix
 Begin 
	CollectId!*:=NIL;
	If IDp TestString!* then TestString!*:=ID2String TestString!*;
	If Not StringP TestString!* 
	 then StdError "Expect String or ID in FindPrefix";
	MapObl Function FindStringMatch;
	Return IDSort CollectId!*
 end;

Lisp procedure FindStringMatch x;
 If StringMatch(TestString!*,ID2String x)
   then CollectId!* := x . CollectId!*;


End;
