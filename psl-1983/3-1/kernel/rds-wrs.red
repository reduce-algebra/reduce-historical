%
% RDS-WRS.RED - Switch the current input or output channel
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        28 August 1981
% Copyright (c) 1981 University of Utah
%

global '(SpecialRDSAction!*		% possibly apply to old and new channel
	 SpecialWRSAction!*		% ditto
	 IN!*				% Current input channel
	 OUT!*);			% Current output channel

fluid '(StdIN!*				% Standard input - may be rebound
	StdOUT!*);			% Standard output - may be rebound

on SysLisp;

syslsp procedure RDS Channel;		%. Switch input channels, return old
begin scalar OldIN, ReadFn;
    if LispVar SpecialRDSAction!* then
	Apply(LispVar SpecialRDSAction!*, list(LispVar IN!*, Channel));
    OldIN := LispVar IN!*;
    if null Channel then Channel := LispVar StdIN!*;
    ReadFn := ReadFunction[IntInf Channel];
    if ReadFn eq 'ChannelNotOpen or ReadFn eq 'WriteOnlyChannel then return
	ChannelError(Channel, "Channel not open for input in RDS");
    LispVar IN!* := Channel;
    return OldIN;
end;

syslsp procedure WRS Channel;		%. Switch output channels, return old
begin scalar OldOUT, WriteFn;
    if LispVar SpecialWRSAction!* then
	Apply(LispVar SpecialWRSAction!*, list(LispVar OUT!*, Channel));
    OldOUT := LispVar OUT!*;
    if null Channel then Channel := LispVar StdOUT!*;
    WriteFn := WriteFunction[IntInf Channel];
    if WriteFn eq 'ChannelNotOpen or WriteFn eq 'ReadOnlyChannel then return
	ChannelError(Channel, "Channel not open for output in WRS");
    LispVar OUT!* := Channel;
    return OldOUT;
end;

off SysLisp;

END;
