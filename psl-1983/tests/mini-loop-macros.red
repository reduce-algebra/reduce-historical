% MINI-LOOP-MACROS.RED

fexpr procedure While fl;
  Begin 
    if not PairP fl then return 'NIL;
    While Eval Car fl do EvProgn cdr fl;
  End;

End;
