% RAWBREAK.RED - A safer break loop if RAWIO is loaded
% MLG 16 Jan 1983

FLUID '(!*RAWIO);

CopyD('OldBreak,'break);

procedure newbreak();
 Begin scalar OldRaw,x;
	OldRaw :=!*RawIo;
	If OldRaw then EchoOn();
	x:=OldBreak();
	If OldRaw Then EchoOff();
	return x;
 End;

Copyd('break,'newbreak);
flag('break,'lose);

