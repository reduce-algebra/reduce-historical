%
% P-FAST-BINDER.RED - Portable version of binding from compiled code
% 
% Author:      Eric Benson
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        6 August 1982
% Copyright (c) 1982 University of Utah
%

% This file is for use with *LAMBIND and *PROGBIND in P-LAMBIND

StartupTime <<

LambindArgs!* := GtWArray 15;

>>;

on Syslisp;

syslsp procedure LamBind V;		% V is vector of IDs
begin scalar N;
    V := VecInf V;
    N := VecLen V;
    for I := 0 step 1 until N do
	LBind1(VecItm(V, I), (LispVar LambindArgs!*)[I]);
end;

syslsp procedure ProgBind V;
begin scalar N;
    V := VecInf V;
    N := VecLen V;
    for I := 0 step 1 until N do
	PBind1 VecItm(V, I);
end;

off Syslisp;

END;
