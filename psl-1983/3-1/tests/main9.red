% MAIN9.RED  READ-EVAL-PRINT, RECLAIM, CATCH and PROG


IN "xxx-header.red"$
%/ in "pt:stubs3.red" 	% -- real gc installed as SUB8
in "pt:stubs4.red"$
in "pt:stubs5.red"$
in "pt:stubs6.red"$  
in "pt:stubs7.red"$
in "pt:stubs8.red"$
in "pt:stubs9.red"$

in "pt:psl-timer.sl"$

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
  Prin2t "Invoking STARTUP Code";
  InitCode();
  LISPVAR(IN!*):=0;
  LISPVAR(OUT!*):=1;
  Hcount :=0;
  ClearIo();
  Prin2T "Reading the INIT files";
  Lapin "INIT9";
  Prin2t '"MINI-PSL with File I/O, RECLAIM and CATCH/THROW";
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

Off syslisp;

End;

