 5-Apr-83 07:45:58-MST,6502;000000000001
Return-path: <@UTAH-CS:GRISS@HP-HULK>
Received: from UTAH-CS by UTAH-20; Tue 5 Apr 83 07:43:05-MST
Date:  5 Apr 1983 0633-PST
From: GRISS@HP-HULK
Subject: New-test-case.red
Message-Id: <418401289.19796.hplabs@HP-VENUS>
Received: by HP-VENUS via CHAOSNET; 5 Apr 1983 06:34:46-PST
Received: by UTAH-CS.ARPA (3.320.5/3.7.6)
	id AA04736; 5 Apr 83 07:41:40 MST (Tue)
To: kessler@HP-VENUS, griss@HP-VENUS


% Tools to analyse the standard timing tests
Fluid '(TestNames Fullnames Tests);
imports '(mathlib);

procedure readtest(name,fil);
 Begin scalar chan,body;
	chan := open(fil,'input);
        body:=channelread chan;
	put(name,'fullname,car body);
	body:=list(name) . cdr body;
	set(name,body); 
	TestNames := name  . TestNames;
	close chan;
	return body;
 End;

procedure readalltests;
 Begin  TestNames:=nil;
	Readtest('TestCray,"test-cray.tim");
	Readtest('Std20,"standard-20.tim");
	Readtest('Test20,"test-20.tim");
	Readtest('Ext20,"extended-20.tim");
	Readtest('TestExt20,"extended-test-20.tim");
	Readtest('Fasthp9836,"16mhz-hp9836.tim");
	Readtest('Std780,"standard-vax-780.tim");
	Readtest('Fast780,"fast-780.tim");
	Readtest('Franz780,"Franz-780.tim");
	Readtest('Std750,"standard-vax-750.tim");
	Readtest('Franz750,"Franz-750.tim");
	Readtest('Stdhp9836,"standard-hp9836.tim");
	Readtest('StdApollo,"standard-Apollo.tim");
% Non PSL
	Readtest('LM2,"LM2-hp.tim");
	Readtest('BlkDolphin,"Block-dolphin.tim");
	Print Testnames;
	Tests :=Evlis TestNames;
	return TestNames;
 End;

Procedure Show body;
Begin scalar HDR,fn;
	HDR:=car body; 
	If (fn:=Get(car HDR,'ShowFn)) then return Apply(fn,list body);
% Default Case
	Terpri();
	prin2l car body; % Header
	Terpri();
	While (body:=cdr body) do 
 	 printf("%w%t%w%n",trimblanks caar body,Tab!*,NiceNum cdar body);
End;

procedure Lookup(Body,Facet);
 Begin scalar value;
	If pairp(value:=assoc(Facet,cdr Body)) then return cdr value;
	return 0.0;
 End;

procedure ShowTotal Body;
Begin scalar Hdr;
	Hdr:=car Body;
	printf("%p: %tTot%w, avg%w, dev %w , %w tests%n",
	      Hdr, 10, 	Nicenum Lookup(Body,'total),
	      		nicenum Lookup(Body,'Average),
			nicenum Lookup(Body,'Deviation),
			Nicenum Lookup(Body,'Number));
End;

put('total, 'showfn,' ShowTotal);

Procedure Total body;
 Begin scalar Hdr,knt,tot,avg,dev,b;
	Knt:=0;
	Tot:=0;
	Dev:=0;
	Hdr:=car Body;
	While body:=cdr body do
	 <<knt:=knt+1; 
	   b:=cdar body; 
	   tot:=tot + b;
	   dev := b*b+dev;
        >>;
	Avg:=float(Tot)/knt;
        dev:=float(dev)/knt;
	dev:=dev-(avg*avg);
	dev:=sqrt(dev);
	b:=list('Total . Hdr,
                'Total . tot,
	        'Average . avg,
		'Deviation . dev,
	        'Number .knt);
        return b
 End;

procedure Ratio(Body1,Body2); 
% Divide elements of Body1  by Elements of Body2
  Begin scalar Hdr1,Hdr2,Rat,b1,b2,r,knt,avg,dev;
	Hdr1:=car body1; Hdr2:= car Body2;
	Body1:=cdr body1; Body2:=cdr Body2;
	If length body1 neq length body2 Then return "Length mismatch";
	knt:=0; avg:=0; dev:=0;
	While Body1 do
	  <<b1:=cdar body1; c:= caar body1; body1:=cdr body1;
	    b2:=cdar body2;                 body2:=cdr body2;
	    r:=float(b1)/b2;
	    avg:=r + avg;
            dev:=r*r +dev;
	    knt:=knt+1;
            rat := (c . r) . rat;
          >>;
	avg:=float(avg)/knt;
        dev:=float(dev)/knt;
        dev:=dev-(avg*avg);
	dev:=sqrt  dev;
 	rat := list('ratio,hdr1,hdr2) . reverse rat;
	return rat;
end;

procedure ratio20 body;
  Ratio(Body,std20);

procedure Ratio780 body;
  Ratio(Body,std780);

procedure Ratio750 body;
  Ratio(body,std780);

procedure Ratiohp9836 body;
 Ratio(body,stdhp9836);

procedure MapTest(Fns,TestList);
% Apply each Fn in Fns to each test in list
  for each Test in TestList
        collect applyFns(Reverse FnS,list Test);

Procedure ApplyFns(Fns,Args);
 If Not Pairp Fns then Car Args % Pass back
  else  ApplyFns(cdr Fns, List Apply(car Fns,Args));

procedure MapBody(Fns,Body);
% Apply series of Fns to each Element in Body of test
 Begin 
	For each Fn in Fns do
	   Body:=(Fn . car Body) . MapBody1(Fn, cdr body);
	return Body;
 End;

procedure MapBody1(Fn,Body);
  If Null Body then NIL
   else ( caar body . Apply(Fn,list cdar body)) . MapBody1 (fn,cdr Body);

%standard Maps

Procedure Invert Body;
 MapBody('(Inverted), Body);

Procedure Inverted x;
 1.0/x;

procedure Logarithm Body;
 MapBody('(LOG),Body);

procedure summary();
	<<readalltests();
	  wrs open("summary.tim",'output);
	  printf("%n%n SUMMARY TESTS on %w%n%n",DATE());
	  mapall();
	  close wrs nil>>;

Procedure MapAll;
 Begin scalar t20;

	T20:=Total Std20;

	Printf "%n     Total Times %n";
	MapTest('(show total),Tests);

	Printf "%n     Ratio of Total Times to STD20%n";
	for each test in Tests do
	   showtotal ratio(Total test,t20);

	Printf "%n     Average Each test Ratios to STD20%n";
	MapTest('(show total ratio20),Tests);

	PrintF "%n     68000 Total times%n";
	showtotal ratio(total StdHp9836,total FastHp9836);
	showtotal ratio(total StdApollo,total StdHp9836);

	PrintF "%n     68000 average ratios%n";
	show total ratio(StdHp9836,FastHp9836);
	show total ratio(StdApollo,StdHp9836);
 End;

procedure MapFileAll(fil,Fns);
 Begin scalar chan;
	chan:=open(fil,'output);
	wrs chan;
	MapTest(Fns,Tests);
	wrs nil;
	close chan;
 End;

% Nicer printing

procedure MakePowers(Base,M);
 Begin scalar V;
	V:=Mkvect M;
	v[0]:=1;
	for i:=1:M do V[i]:=Base* V[i-1];
	return V;
 End;

Tens!* := MakePowers(10,10);

Procedure FLTRND(N,fld);
 If floatp N then Fix(FLD*N+.5)/float(fld) else N;

Procedure NiceNum N;
   PadNM(N,nice!*,Fld!*);

FLD!*:=3;
Nice!*:=7;
Tab!*:=30;

Procedure PADNM(Num,n,m);
% LeftPAD number in Field of N;
 Begin scalar m1,m2,FixPart;
        FixPart :=Fix Num;
        m1:=BLDMSG("%p",FIXPART);
	N:=N-Size(m1)-1; % Number of Blanks
	if n>0 then m1:=Concat(MkString(n-1,32),m1);
	if m>0 then <<NUM := NUM-Fixpart;
                      m2:=BLDMSG("%p",FIX(num*Tens!*[m]+0.5));
	              M:=M-size(m2)-1; % Number of 0s
		      if m>0 then m2:=Concat(MkString(m-1,48),m2);
		      m1:=Concat(m1,concat(".",m2))>>;
	return m1;
 End;

procedure TrimBlanks S;
 Begin scalar N;
	if not stringp s then return s;
	n:=Size s;
	While n>0 and (s[n]=char BLANK  or s[n] = char TAB)   do n:=n-1;
	return sub(s,0,n);
  End;

End;
-------


