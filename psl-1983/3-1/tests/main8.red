% MAIN8.RED Small READ-EVAL-PRINT Loop WITH IO
%             Needs IO, SUB2, SUB3, SUB4, SUB5, SUB6,SUB7


IN "xxx-header.red"$
%/ in "pt:stubs3.red" real gc installed$
in "pt:stubs4.red"$
in "pt:stubs5.red"$
in "pt:stubs6.red"$  
in "pt:stubs7.red"$
in "pt:stubs8.red"$
in "pt:psl-timer.sl"$
in "PT:GC-TEST.RED"$

on syslisp;

Compiletime GLOBAL '(DEBUG IN!* OUT!* !$EOF!$ !*PVAL);
FLUID '(Heap!-Warn!-Level);


Procedure FirstCall;
Begin scalar x, Done, Hcount;
  INIT();
  InitHeap();
  InitObList();	
  InitEval();
  InitRead();
  LispVar(DEBUG) := 'NIL; % For nice I/O
  Lispvar(Heap!-Warn!-Level) := 0; % Set for Non-trap
  LispVar(!*GC) :=T;
  LispVar(GCKnt!*) :=0;
  LispVar(GCTime!*) :=0;
  LispVar(!*RAISE) := 'T;            % Upcase Input IDs
  LispVar(!*PVAL) := 'T;             % Print VALUEs
  LispVar(!$EOF!$) := MKID Char EOF; %  Check for EOF
  Hcount :=0;
  Prin2t "Invoke STARTUP Code";
  InitCode();
  LISPVAR(IN!*):=0;
  LISPVAR(OUT!*):=1;
  Hcount :=0;
  ClearIo();
  Prin2T "Reading Init Files";
  Lapin "INIT8";
  Prin2t '"MINI-PSL with File I/O and RECLAIM";
  Prin2T "Invoke (TESTMARKING) and then (GCTEST)";
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

off syslisp;

End;
