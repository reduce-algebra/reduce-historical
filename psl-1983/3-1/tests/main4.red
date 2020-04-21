% MAIN4.RED : Test Mini reader and function primitives, 
%             needs IO, SUB2, SUB3 and SUB4


IN "xxx-header.red"$
In "PT:P-function-primitives.red"$
IN "PT:STUBS4.RED"$
IN "PT:STUBS3.RED"$

on syslisp;

Compiletime GLOBAL '(DEBUG);


Procedure FirstCall;
Begin scalar x,s1,s2,s3, Done,D1,D2;
  Init();
  InitHeap();
  InitObList();	
  LispVar(DEBUG) := 'T;  % To get ID stuff out

  Dashed "Test EQSTR";
  s1:='"AB";
  s2:='"Ab";
  s3:='"ABC";
  ShouldBe("EqStr(AB,AB)",EqStr(s1,s1),'T);
  ShouldBe("EqStr(AB,AB)",EqStr(s1,"AB"),'T);
  ShouldBe("EqStr(AB,Ab)",EqStr(s1,s2),'NIL);  
  ShouldBe("EqStr(AB,ABC)",EqStr(s1,s3),'NIL);

  Dashed "Test Intern on existing ID's";
  ShouldBe("Intern(A)",Intern "A", 'A);
  ShouldBe("Intern(AB)",Intern S1, 'AB);

  Dashed "Test Intern on new ID, make sure same place";
  D1:=Intern S3;
  ShouldBe("Intern(ABC)",Intern("ABC"),D1);

  D2:=Intern "FOO";
  ShouldBe("Intern(ABC) again",Intern("ABC"),D1);

  Dashed "Test RATOM loop. Type various ID's, STRING's and INTEGER's";
  MoreStuff();
  InitRead();
  While Not Done do 
    <<x:=Ratom();
      prin2 "Item read=";
      Prtitm x;
      Print x;
      if x eq 'Q then Done := 'T;>>;

  LispVar(DEBUG) := 'NIL;  % Turn off PRINT

  Dashed "Test READ loop. Type various S-expressions";
  MoreStuff();
  Done:= 'NIL;
  While Not Done do 
    <<x:=READ();
      Prin2 '"  Item read=";
      Prtitm x;
      Print x;
      if x eq 'Q then Done := 'T;>>;
  
      Functiontest();
   Quit;
 End;


Procedure MoreStuff;
 <<Spaced "Move to next part of test by typing the id Q";
   Spaced "Inspect printout carefully">>;

Fluid '(CodePtr!* CodeForm!* CodeNarg!*);

procedure FunctionTest();
  Begin scalar c1,c2,ID1,x;
	Dashed "Tests of FUNCTION PRIMITIVES ";

	ShouldBe("FunBoundP(Compiled1)",FunBoundP 'Compiled1,NIL);
	ShouldBe("FunBoundP(ShouldBeUnbound)",FunBoundP 'ShouldBeUnBound,T);

	ShouldBe("FCodeP(Compiled1)",FCodeP 'Compiled1,T);
	ShouldBe("FCodeP(ShouldBeUnbound)",FcodeP 'ShouldBeUnBound,NIL);

	ShouldBe("FCodeP(Compiled2)",FCodeP 'Compiled2,T);

        Dashed "Now MakeFunBound";
        MakeFunBound('Compiled2);
	ShouldBe("FCodeP(Compiled2)",FCodeP 'Compiled2,NIL);
	ShouldBe("FUnBoundP(Compiled2)",FUnBoundP 'Compiled2,T);

        Dashed "Now copy CODEPTR of Compiled1 to Compiled2 ";
        C1:=GetFCodePointer('Compiled1);
        C2:=GetFCodePointer('Compiled2);

	ShouldBe("CodeP(C1)",CodeP C1,T);
	ShouldBe("CodeP(C2)",CodeP C2,NIL); 

        MakeFcode('Compiled2,C1);
	ShouldBe("C1=GetFcodePointer 'Compiled2",
                   C1=GetFCodePointer 'Compiled2,T);
	ShouldBe("Compiled2()",Compiled2(),12345);

        Dashed "Now test CodePrimitive";
        CodePtr!* := GetFCodePointer 'Compiled3;
        X:= CodePrimitive(10,20,30,40);
        Shouldbe(" X=1000",1000,X);

        Dashed "Test CompiledCallingInterpreted hook";
        CompiledCallingInterpreted();

        Dashed "Now Create PRETENDINTERPRETIVE";
        MakeFlambdaLink 'PretendInterpretive;
        Shouldbe("FlambdaLinkP",FlambdaLinkP 'PretendInterpretive,T);
        Shouldbe("Fcodep",FCodeP 'PretendInterpretive,NIL);
        Shouldbe("FUnBoundP",FUnBoundP 'PretendInterpretive,NIL);

        Dashed "Now call PRETENDINTERPRETIVE";
        x:=PretendInterpretive(500,600);
        ShouldBe("PretendInterpretive",x,1100);
   End;

% Auxilliary Compiled routines for CodeTests:

Procedure Compiled1;
  << Dotted "Compiled1 called";
     12345>>;

Procedure Compiled2;
  << Dotted"Compiled2 called";
     67890>>;

Procedure Compiled3(A1,A2,A3,A4);
 <<Dotted "Compiled3 called with 4 arguments , expect 10,20,30,40";
   Prin2 "   A1=";Prin2T A1;
   Prin2 "   A2=";Prin2T A2;
   Prin2 "   A3=";Prin2T A3;
   Prin2 "   A4=";Prin2T A4;
   Prin2t "Now return 1000 to caller";
   1000>>;


syslsp procedure UndefinedFunctionAuxAux ;
 Begin scalar FnId;
    FnId := MkID UndefnCode!*;
    Prin2 "Undefined Function ";
      Prin1 FnId;
       Prin2 " called with ";
        Prin2 LispVar UndefnNarg!*;
         prin2T " args from compiled code";
     Quit;
  End;

% some primitives use by FastApply

syslsp procedure CompiledCallingInterpretedAux();
 Begin scalar FnId,Nargs;
  Prin2t "COMPILED Calling INTERPRETED";
  Prin2  "CODEFORM!*= ";  Print LispVar CodeForm!*;
    Nargs:=LispVar CodeNarg!*;
    FnId := MkID LispVar CodeForm!*;
     Prin2 "Function: ";
      Prin1 FnId;
       Prin2 " called with ";
        Prin2 Nargs;
         prin2T " args from compiled code";
        Return 1100;
  End;

Off syslisp;

End;
