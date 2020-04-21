% MINI-CARCDR.RED

% ----  Some Basic LIST support Functions 

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
