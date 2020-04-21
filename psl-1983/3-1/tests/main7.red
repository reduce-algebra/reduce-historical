% main7.red : Small READ-EVAL-PRINT Loop WITH IO
%             Needs IO, SUB2, SUB3, SUB4, SUB5, SUB6,SUB7


IN "xxx-header.red"$
in "pt:stubs3.red"$
in "pt:stubs4.red"$
in "pt:stubs5.red"$
in "pt:stubs6.red"$  
in "pt:stubs7.red"$
in "pt:psl-timer.sl"$

on syslisp;

Compiletime GLOBAL '(DEBUG IN!* OUT!* !$EOF!$ !*PVAL);

Procedure FirstCall;
Begin scalar x, Done, Hcount;
  INIT();
  InitHeap();
  InitObList();	
  InitEval();
  Prin2t '"MINI-PSL with File I/O";
  Prin2T '"   Type (IOTEST) to test basic file I/O";
  Prin2T '"   Future tests will be READ in this way";
  Prin2T '"   !*RAISE and !*PVAL set T";
  LispVar(DEBUG) := 'NIL; % For nice I/O
  InitRead();
  LispVar(!*RAISE) := 'T;            % Upcase Input IDs
  LispVar(!*PVAL) := 'T;             % Print VALUEs
  LispVar(!$EOF!$) := MKID Char EOF; %  Check for EOF
  Hcount :=0;
  Prin2t " .... Now we test INITCODE";
  InitCode();
  LISPVAR(IN!*):=0;
  LISPVAR(OUT!*):=1;
  Hcount :=0;
  ClearIo();
  While Not Done do 
    <<Hcount:=Hcount+1;
      Prin2 Hcount; Prin2 '" lisp> "; 
      x:=READ();
      if x EQ !$EOF!$ then
             <<Terpri();
               Prin2T " *** Top Level EOF *** ">>
      else if x eq 'QUIT then Done := 'T
       else <<Terpri();
              x:=EVAL x;
              if Lispvar(!*PVAL) then Print x>>;
  >>;
  Quit; 
 End;





%---- File Io tests ----

Off syslisp;

Procedure Iotest;
 Begin scalar InFile, OutFile,Ch,S,InString,OutString;
   Prin2T "---- Test of File IO";
   IN!*:=0; 
   Out!*:=1;
   Prin2T "     Test CLEARIO";
A: Prin2T "     Input String for Input File";
   Instring:=Read();
   Terpri();
   If not StringP Instring then goto A;

B: Prin2T "     Input String for OutPut File";
   OutString:=Read();
   Terpri();
   If not StringP Outstring then goto B;

  Infile:=Open(InString,'Input);
  prin2 "      Input File Opened on ";
   Prin2 Infile;
    PRIN2T ", copy to TTY ";
  While Not ((ch:=IndependentReadChar(InFILE)) eq 26) do PutC Ch;
  Close Infile;
  Prin2T "     File Closed, Input test done";

  Infile:=Open(InString,'Input);
  OutFile:=Open(OutString,'OutPut);
  prin2 "      Input File  on ";
   Prin2 Infile;
    PRIN2 ", copy to Output File on";
     Prin2T OutFile;
  While Not ((ch:=IndependentReadChar(InFILE)) eq 26)
     do IndependentWriteChar(outFile,Ch);
  Close Infile;
  Close OutFile;
  Prin2 "Both Files Closed, Inspect File:";
   Prin2T OutString;
 End;


End;
