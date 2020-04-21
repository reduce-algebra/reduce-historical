% MINI-TYPE-ERRORS.RED

procedure TypeError(Offender, Fn, Typ);
  <<Errorheader();
    Prin2 "An attempt was made to do";
    prin1 Fn;
    prin2 " on `";
    prin1 Offender;
    prin2 "', which is not ";
    print Typ;
    quit; 
>>;

procedure UsageTypeError(Offender, Fn, Typ, Usage);
<<Errorheader();
    Prin2 "An attempt was made to use";
    prin1 Offender;
    Prin2 " as ";
    Prin1 Usage; 
    prin2 " in `";
    prin1 Fn;
    prin2 "`, where ";
    prin1 Typ;
    prin2t " is needed";
    quit;
>>;
  
procedure NonIdError(Offender, Fn);
    TypeError(Offender, Fn, "an identifier");

procedure NonNumberError(Offender, Fn);
    TypeError(Offender, Fn, "a number");

procedure NonIntegerError(Offender, Fn);
    TypeError(Offender, Fn, "an integer");

procedure NonPositiveIntegerError(Offender, Fn);
    TypeError(Offender, Fn, "a non-negative integer");

End;