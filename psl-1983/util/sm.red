% SM.RED - String match to replace find
% M.L.G

procedure sm(p,s);
  Sm1(p,0,size(p),s,0,size(s));

procedure sm1(p,p1,p2,s,s1,s2);
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
      if p1>p2 then return stderror "pattern ran out in % case of sm";
      c:=p[p1];
      if c eq char !% then goto L2;
      if c eq char !? then <<p1:=p1+1;
                             s1:=s1+1;
                             goto L1>>;

      if c eq char !* then  % 0 or more vs 1 or more
       return <<while not(c:=sm1(p,p1+1,p2,s,s1,s2)) and s1<=s2
                  do s1:=s1+1;
                c>>;
      Return Stderror Bldmsg(" %% %r not known in sm",int2id c);
 end;