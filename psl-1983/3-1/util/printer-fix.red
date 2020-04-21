% Some patches to I/O modules

Fluid '(DigitStrBase);
DigitStrBase:='"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

on syslisp;

smacro procedure DigitStr();
 strinf LispVar DigitstrBase;

syslsp procedure SysPowerOf2P Num;
    case Num of
      1: 0;
      2: 1;
      4: 2;
      8: 3;
      16: 4;
      32: 5;
      default: NIL
    end;


syslsp procedure ChannelWriteSysInteger(Channel, Number, Radix);
begin scalar Exponent,N1;
    return if (Exponent := SysPowerOf2P Radix) then
	ChannelWriteBitString(Channel, Number, Radix - 1, Exponent)
    else if Number < 0 then
    <<  ChannelWriteChar(Channel, char '!-);
        WriteNumber1(Channel,-(Number/Radix),Radix); % To catch largest NEG
	ChannelWriteChar(Channel, strbyt(DigitStr(), - MOD(Number, Radix))) >>
    else if Number = 0 then ChannelWriteChar(Channel, char !0)
    else WriteNumber1(Channel, Number, Radix);
end;

syslsp procedure WriteNumber1(Channel, Number, Radix);
    if Number = 0 then Channel
    else
    <<  WriteNumber1(Channel, Number / Radix, Radix);
	ChannelWriteChar(Channel, 
	strbyt(Digitstr(),  MOD(Number, Radix))) >>;


syslsp procedure ChannelWriteBitString(Channel, Number, DigitMask, Exponent);
 if Number = 0 then ChannelWriteChar(Channel,char !0)
  else  ChannelWriteBitStrAux(Channel, Number, DigitMask, Exponent);

syslsp procedure ChannelWriteBitStrAux(Channel, Number, DigitMask, Exponent);
    if Number = 0 then Channel		% Channel means nothing here
    else				% just trying to fool the compiler
    <<  ChannelWriteBitStrAux(Channel,
			      LSH(Number, -Exponent),
			      DigitMask,
			      Exponent);
	ChannelWriteChar(Channel,
			 StrByt(DigitStr(),
				LAND(Number, DigitMask))) >>;
