% MINI-CAR-CDR.RED

% ----  Some Basic LIST support Functions 

Procedure Car x;
 if Pairp x then car x else <<Print "*** Cant take CAR of NON PAIR";NIL>>;

Procedure Cdr x;
 if Pairp x then cdr x  else <<Print "*** Cant take CDR of NON PAIR";NIL>>;

% -- CxxR -- may need in EVAL if not open coded

Procedure Caar x;
 Car Car x;

Procedure Cadr x;
 Car Cdr x;

Procedure Cdar x;
 Cdr Car x;

Procedure Cddr x;
 Cdr Cdr x;

end;
