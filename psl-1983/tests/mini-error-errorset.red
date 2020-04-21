% MINI-ERROR-ERRORSET 
on syslisp;

syslsp procedure ErrorHeader;
 Prin2String "*** ERROR *** ";

syslsp procedure Error s;
 <<ErrorHeader();
   ErrorTrailer s>>;

syslsp procedure ErrorTrailer s;
   <<Prin2T s;
     Quit;>>;

off syslisp;
End;