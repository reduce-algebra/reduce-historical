% Simple 1 file test
% This is program MAIN1.RED

On SYSLISP;

IN "XXX-HEADER.RED"$

Procedure FirstCall;
 <<Init();
   PutC Char A;
   PutC Char B;
   Terpri();
   PutInt 1;
   Terpri();
   PutInt 2;
   Terpri();
   Putint Timc(); Terpri();
   Putint Timc(); Terpri();
   Quit;>>;

procedure terpri();
   PutC Char EOL;

end;

