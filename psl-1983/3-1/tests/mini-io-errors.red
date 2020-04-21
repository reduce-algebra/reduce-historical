% MINI-IO-ERRORS.RED

Procedure IoError M;
 <<terpri();
   ErrorHeader();
   Prin2t M;
   RDS 0;
   WRS 1;
   NIL>>;

procedure ContOpenError(fil,how);
  IoError List("Cant Open file ",fil," for ",how);

End;
