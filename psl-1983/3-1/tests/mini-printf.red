% MINI-PRINTF.RED

procedure PrintF(FMT, A1,A2,A3,A4,A5,A6);
% Dummy PRINTF
<< Prin2 FMT;
   Prin2 " ";
   Prin2 A1;
   Prin2 " ";
   Prin2 A2;
   Prin2 " ";
   Prin2 A3;
   Prin2 " ";
   Prin2T A4;   
 >>;

procedure errorprintf(FMT,a1,a2,a3,a4);
% Dummy ErrorPrintf
  PrintF(FMT,A1,A2,A3,A4);

procedure BLDMSG(FMT,A1,A2,A3,A4,A5,A6);
% Dummy BLDMSG
   LIST ('BLDMSG, FMT,A1,A2,A3,A4);


procedure ErrPrin U;
 <<Prin2 '!`; Prin1 U; Prin2 '!' >>;

End;
