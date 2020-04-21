% MAIN5.RED : Small READ-EVAL-PRINT Loop
%             Needs IO, SUB2, SUB3, SUB4, SUB5

IN "xxx-header.red"$
IN "PT:STUBS3.RED"$
IN "PT:STUBS4.RED"$
IN "PT:STUBS5.RED"$

on syslisp;

Compiletime FLUID '(DEBUG FnTypeList !*RAISE !$EOF!$ !*PVAL !*ECHO);

Procedure FirstCall;
Begin scalar x, Done, Hcount;
  Init();
  InitHeap();
  TestGet();
  InitEval();
  Prin2t '"(very) MINI-PSL: A Read-Eval-Print Loop, terminate with Q";
  Prin2T '"       !*RAISE and !*PVAL have been set T";
  Prin2T '"       Should be able to execute any COMPILED expressions";
  Prin2T '"       typed in. Run (TESTSERIES) when ready";
  LispVar(DEBUG) := 'NIL; % For nice I/O
  InitRead();
  LispVar(!$EOF!$) := MkID Char EOF$ 
  Hcount :=0;
  LispVar(!*RAISE) := 'T; %  Upcase input IDs
  While Not Done do 
    <<Hcount:=Hcount+1;
      Prin2 Hcount; Prin2 '" lisp> "; 
      x:=READ();
      if x eq 'Q then Done := 'T
       else if x eq !$EOF!$ then
            <<terpri();
              Prin2T " **** Top Level EOF ****">>
       else <<Terpri();
              x:=EVAL x;
              If LISPVAR(!*PVAL) then Print x>>;
  >>;
  Quit; 
 End;

% ----  Test Routines:

syslsp procedure TestSeries();
 <<Dashed "TESTs called by TESTSERIES";
   TestUndefined()>>;

syslsp procedure TestGet();
Begin
	Dashed "Tests of GET and PUT";
	Shouldbe("GET('FOO,'FEE)",GET('FOO,'FEE),NIL);
	Shouldbe("PUT('FOO,'FEE,'FUM)",PUT('FOO,'FEE,'FUM),'FUM);
	Shouldbe("GET('FOO,'FEE)",GET('FOO,'FEE),'FUM);
	Shouldbe("REMPROP('FOO,'FEE)",REMPROP('FOO,'FEE),'FUM);
	Shouldbe("GET('FOO,'FEE)",GET('FOO,'FEE),NIL);
 end;

syslsp procedure TestUndefined;
  <<Print "Calling SHOULDBEUNDEFINED";
    ShouldBeUndefined(1)>>;
% Some dummies:

procedure UnbindN N;
 Stderror '"UNBIND only added at MAIN6";

procedure Lbind1(x,y);
 StdError '"LBIND1 only added at MAIN6";

Off syslisp;

End;



