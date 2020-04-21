% MINI-IO-ERRORS.RED

Procedure IoError M;
 <<terpri();
   ErrorHeader();
   Prin2t M;
   RDS 0;
   WRS 1;
   NIL>>;

End;
