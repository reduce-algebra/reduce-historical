% MINI-TYPE-CONVERSIONS.RED

on syslisp;

syslsp procedure Sys2Int N;		%. Convert word to Lisp number
    if SignedField(N, InfStartingBit - 1, InfBitLength + 1) eq N then N
    else Sys2FIXN N;

syslsp procedure SYS2FIXN N;
 STDerror LIST(N, "too big for mini arith");

off syslisp;

End;
