%
% VECTORS.RED - Standard Lisp Vector functions
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        20 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL>VECTORS.RED.2, 10-Jan-83 15:54:19, Edit by PERDUE
%  Added EGetV etc. for EVectors, paralleling Vectors

% MkVect and MkEVector are found in PK:CONS-MKVECT.RED

on SysLisp;

syslsp procedure GetV(Vec, I);		%. Retrieve the I'th entry of Vec
begin scalar StripV, StripI;
    return if VectorP Vec then
	if IntP I then			% can't have vectors bigger than INUM
	<<  StripV := VecInf Vec;
	    StripI := IntInf I;
	    if StripI >= 0 and StripI <= VecLen StripV then
		VecItm(StripV, StripI)
	    else
		StdError BldMsg('"Subscript %r in GetV is out of range",
					     I) >>
	else
	    IndexError(I, 'GetV)
    else
	NonVectorError(Vec, 'GetV);
end;

syslsp procedure PutV(Vec, I, Val);	%. Store Val at I'th position of Vec
begin scalar StripV, StripI;
    return if VectorP Vec then
	if IntP I then			% can't have vectors bigger than INUM
	<<  StripV := VecInf Vec;
	    StripI := IntInf I;
	    if StripI >= 0 and StripI <= VecLen StripV then
		VecItm(StripV, StripI) := Val
	    else
		StdError BldMsg('"Subscript %r in PutV is out of range",
					     I) >>
	else
	    IndexError(I, 'PutV)
    else
	NonVectorError(Vec, 'PutV);
end;

syslsp procedure UpbV V;		%. Upper limit of vector V
    if VectorP V then MkINT VecLen VecInf V else NIL;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EVectors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

syslsp procedure EVECTORP V;
 TAG(V) EQ EVECT;

syslsp procedure EGETV(Vec, I);         %. Retrieve the I'th entry of Vec
begin scalar StripV, StripI;
    return if EvectorP Vec then
        if IntP I then                  % can't have vectors bigger than INUM
        <<  StripV := VecInf Vec;
            StripI := IntInf I;
            if StripI >= 0 and StripI <= VecLen StripV then
                VecItm(StripV, StripI)
            else
                StdError BldMsg('"Subscript %r in EGETV is out of range",
                                             I) >>
        else
            IndexError(I, 'EGETV)
    else
        NonVectorError(Vec, 'EGETV);
end;

syslsp procedure Eputv(Vec, I, Val);    %. Store Val at I'th position of Vec
begin scalar StripV, StripI;
    return if EvectorP Vec then
        if IntP I then                  % can't have vectors bigger than INUM
        <<  StripV := VecInf Vec;
            StripI := IntInf I;
            if StripI >= 0 and StripI <= VecLen StripV then
                VecItm(StripV, StripI) := Val
            else
                StdError BldMsg('"Subscript %r in Eputv is out of range",
                                             I) >>
        else
            IndexError(I, 'Eputv)
    else
        NonVectorError(Vec, 'Eputv);
end;

syslsp procedure EUpbV V;               %. Upper limit of vector V
    if EvectorP V then MkINT EVecLen EVecInf V else NIL;

off SysLisp;

END;
