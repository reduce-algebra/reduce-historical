%
% OTHER-IO.RED - Miscellaneous input and output functions
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        28 August 1981
% Copyright (c) 1981 University of Utah
%

% Edit by Cris Perdue, 27 Jan 1983 1428-PST
% put in Kessler's change so ChannelLineLength allows Len=0 to mean that
% EOL is not to be automatically written.
%  <PSL.KERNEL>OTHER-IO.RED.3, 29-Dec-82 12:23:52, Edit by PERDUE
%  added LPosn and ChannelLPosn
%  <PSL.KERNEL>OTHER-IO.RED.2, 17-Sep-82 15:46:38, Edit by BENSON
%  Added ChannelLinelength, ChannelPosn, ChannelEject, ChannelTerPri
%   ChannelReadCH, ChannelPrinC
%  <PSL.INTERP>OTHER-IO.RED.3, 21-Jul-82 00:48:35, Edit by BENSON
%  Made ReadCh do case conversion for *Raise

% Most of the uninteresting I/O functions from the Standard Lisp report

global '(OUT!*);			% Current output channel

fluid '(!*Raise);			% controls case conversion of IDs

on SysLisp;

external WArray LinePosition,		% Array indexed by channel
		MaxLine;		% ditto

syslsp procedure ChannelEject C;	%. Skip to top of next output page
<<  ChannelWriteChar(C, char FF);	% write a formfeed
    NIL >>;

syslsp procedure Eject();		%. Skip to top of next output page
    ChannelEject LispVar OUT!*;

syslsp procedure ChannelLineLength(Chn, Len);	%. Set maximum line length
begin scalar OldLen, StripLen;
    OldLen := MaxLine[Chn];
    if Len then
	if IntP Len and Len >= 0 then
	    MaxLine[Chn] := Len
	else
	    StdError BldMsg('"%r is an invalid line length", Len);
    return OldLen;		% if Len is NIL, just return current
end;

syslsp procedure LineLength Len;	%. Set maximum line length
    ChannelLineLength(LispVar OUT!*, Len);

syslsp procedure ChannelPosn Chn;	%. Number of characters since last EOL
    LinePosition[Chn];

syslsp procedure Posn();		%. Number of characters since last EOL
    ChannelPosn LispVar OUT!*;

syslsp procedure ChannelLPosn Chn;	%. Number of EOLs since last FF
    PagePosition[Chn];

syslsp procedure LPosn();		%. Number of EOLs since last FF
    ChannelLPosn LispVar OUT!*;

syslsp procedure ChannelReadCH Chn;	%. Read a single character ID
begin scalar X;				% for Standard Lisp compatibility
    X := ChannelReadChar Chn;		% converts lower to upper when *RAISE
    if LispVar !*Raise and X >= char lower a and X <= char lower z then
	X := char A + (X - char lower a);
    return MkID X;
end;

syslsp procedure ReadCH();		%. Read a single character ID
    ChannelReadCH LispVar IN!*;

syslsp procedure ChannelTerPri Chn;	%. Terminate current output line
<<  ChannelWriteChar(Chn, char EOL);
    NIL >>;

syslsp procedure TerPri();		%. Terminate current output line
    ChannelTerPri LispVar OUT!*;

off SysLisp;

LoadTime PutD('PrinC, 'EXPR, cdr GetD 'Prin2);	% same definition as Prin2
LoadTime PutD('ChannelPrinC, 'EXPR, cdr GetD 'ChannelPrin2);
					% same definition as ChannelPrin2
END;
