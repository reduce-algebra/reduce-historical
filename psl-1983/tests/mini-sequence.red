% MINI-SEQUENCE.RED: Susbet of Strings, sequence etc for testing

on syslisp;

syslsp procedure MkString(L, C); 
%  Make str with upb L, all chars C
begin scalar L1, S;
    if IntP L then L1 := IntInf L else return NonIntegerError(L, 'MkString);
    if L1 < -1 then return NonPositiveIntegerError(L, 'MkString);
    S := GtStr L1;
    for I := 0 step 1 until L1 do
	StrByt(S, I) := C;
    return MkSTR S;
end;
off syslisp;
End;
