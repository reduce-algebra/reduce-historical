% MINI-ERROR-ERRORSET 
on syslisp;

syslsp procedure ErrorHeader;
 Prin2String "*** ERROR *** ";

syslsp procedure Error s;
 <<ErrorHeader();
   ErrorTrailer s>>;

syslsp procedure ErrorTrailer s;
   <<If pairp s then Prin2L s else Prin2T s;
     Quit;>>;

syslsp procedure Prin2L s;
% Should be in PrintF?
 <<While Pairp s do <<prin2 car s; s:=cdr s; prin2 " ">>;
   Terpri()>>;

off syslisp;
End;