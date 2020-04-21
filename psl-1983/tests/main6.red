% MAIN6.RED : Small READ-EVAL-PRINT Loop
%             Needs IO, SUB2, SUB3, SUB4, SUB5, SUB6


IN "xxx-header.red"$
IN "PT:STUBS3.RED"$
IN "PT:STUBS4.RED"$
IN "PT:STUBS5.RED"$
IN "PT:STUBS6.RED"$

on syslisp;

Compiletime GLOBAL '(DEBUG !*RAISE !$EOF!$);

Procedure FirstCall;
Begin scalar x, Done, Hcount;
  Init();
  InitHeap();
  InitEval();
  Prin2t '"MINI-PSL: A Read-Eval-Print Loop, terminate with Q";
  Prin2T '"      !*RAISE has been set T";
  Prin2T '"      Run (TESTSERIES) to check BINDING etc";
  LispVar(DEBUG) := 'NIL; % For nice I/O
  InitRead();
  LispVar(!*RAISE) := 'T;            % Upcase Input IDs
  LispVar(!$EOF!$) := MKID Char EOF; %  Check for EOF
  Hcount :=0;
  Prin2t " .... Now Call INITCODE";
  InitCode();
  Prin2t " .... Return from INITCode, Now toploop";
  While Not Done do 
    <<Hcount:=Hcount+1;
      Prin2 Hcount; Prin2 '" lisp> "; 
      x:=READ();
      if x eq 'Q then Done := 'T
       else if x = !$EOF!$ then
            <<Terpri();
              Prin2T " **** Top Level EOF **** ">>
       else <<Terpri();
              x:=EVAL x;
              Print x>>;
  >>;
  Quit; 
 End;


CompileTime FLUID '(AA);

Procedure TESTSERIES();
 Begin
	BindingTest();
        InterpTest();
        CompBindTest();
 End;

Procedure BindingTest;
Begin
  Dashed "Test BINDING Primitives"$
  LispVar(AA):=1;
  PBIND1('AA);   % Save the 1, insert a NIL
  LBIND1('AA,3); % save the NIL, insert a 3
  ShouldBe('"3rd bound AA",LispVar(AA),3);
  UnBindN 1;
  ShouldBe('"2rd bound AA",LispVar(AA),NIL);
  UnBindN 1;
  ShouldBe('"Original AA",LispVar(AA),1);
End;


Global '(Lambda1 Lambda2 CodeForm!*);

Procedure InterpTest();
Begin
     Dashed "TEST of Interpreter Primitives for LAMBDA's ";
     Lambda1:='(LAMBDA (X1 X2) (PRINT (LIST 'LAMBDA1 X1 X2)) 'L1);
     Lambda2:='(LAMBDA (Y1 Y2) (PRINT (LIST 'LAMBDA2 Y1 Y2)) 'L2);


     Spaced "LAMBDA1: ";   Print Lambda1;
     Dashed "FastLambdaApply on Lambda1";

     CodeForm!*:=Lambda1;
     ShouldBe("FastLambdaApply", FastLambdaApply(10,20),'L1);

     Dashed "Now Test FASTAPPLY";
     TestApply(" Compiled ID 1 ", 'Compiled1,'C1);
     TestApply(" CodePointer 2 ", GetFcodePointer 'Compiled2,'C2);
     TestApply(" Lambda Expression 1 ", Lambda1,'L1);

     Dashed "Test a compiled call on Interpreted code ";
     PutD('Interpreted3,'Expr,
	'(LAMBDA (ag1 ag2 ag3) (Print (list 'Interpreted3 Ag1 Ag2 Ag3)) 'L3));

     ShouldBe(" FlambdaLinkP",FlambdaLinkP 'Interpreted3,T);

     ShouldBe(" Interp3", Interpreted3(300,310,320),'L3);

     PutD('Interpreted2,'Expr,Lambda2);
     TestApply(" Interpreted ID 2 ", 'Interpreted2,'L2);

End;

LAP '((!*entry TestFastApply expr 0) 
% Args loaded so move to fluid and go
      (!*Move (FLUID TestCode!*) (reg t1))
      (!*JCALL FastApply));

Procedure TestApply(Msg,Fn,Answer);
 Begin scalar x;
     Prin2 "   Testapply case "; prin2 Msg;
      Prin2 " given ";
       Print Fn;
      TestCode!* := Fn;
      x:=TestFastApply('A,'B);
      Return ShouldBe("  answer",x,Answer);
 End;

Procedure Compiled1(xxx,yyy);
 <<Prin2 "     Compiled1(";
   Prin1 xxx; Prin2 " "; Prin1 yyy; Prin2T ")";
   'C1>>;

Procedure Compiled2(xxx,yyy);
 <<Prin2 "     Compiled2(";
   Prin1 xxx; Prin2 " "; Prin1 yyy; Prin2T ")";
   'C2>>;

CompileTime Fluid '(CFL1 CFL2 CFL3);

Procedure CompBindTest();
Begin
	 Dashed "Test LAMBIND and PROGBIND in compiled code";
         CFL1:='TOP1;
         CFL2:='TOP2;
         Cbind1('Mid0,'Mid1,'Mid2);
         Shouldbe("CFL1",CFL1,'Top1);
         Shouldbe("CFL2",CFL2,'Top2);
End;

procedure Cbind1(x,CFL1,CFL2);
 Begin
         Shouldbe("x   ",x   ,'Mid0);
         Shouldbe("CFL1",CFL1,'Mid1);
         Shouldbe("CFL2",CFL2,'Mid2);
         Cbind2();
         Shouldbe("CFL1",CFL1,'Bot1);
         Shouldbe("CFL2",CFL2,'Mid2);
  End;

Procedure Cbind2();
 Begin
         Shouldbe("CFL1",CFL1,'Mid1);
         Shouldbe("CFL2",CFL2,'Mid2);
    Begin scalar x,CFL2;
         CFL1:='Bot1;
         CFL2:='Bot2;
         Shouldbe("CFL1",CFL1,'Bot1);
         Shouldbe("CFL2",CFL2,'Bot2);
    End;
         Shouldbe("CFL1",CFL1,'Bot1);
         Shouldbe("CFL2",CFL2,'Mid2);
  End;

End;
