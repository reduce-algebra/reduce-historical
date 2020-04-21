% MINI-SYMBOL-VALUES.RED

Procedure Set(x,y);
 Begin 
   If IDP x then SYMVAL(IDINF x):=y
    else <<prin2 '"**** Non-ID in SET: ";Print x>>;
   return y;
 End;

End;
