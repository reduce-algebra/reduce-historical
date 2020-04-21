%
% OPEN-CLOSE.RED - File primitives
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        27 August 1981
% Copyright (c) 1981 University of Utah
%

% Edit by Cris Perdue, 27 Jan 1983 1700-PST
% Close now checks for a legitimate FileDes argument

fluid '(SpecialReadFunction!*		% These must be set up for special
	SpecialWriteFunction!*		% Open call
	SpecialCloseFunction!*);

on SysLisp;

external WArray ReadFunction,		% indexed by channel to read a char
		WriteFunction,		% indexed by channel to write a char
		CloseFunction,		% indexed by channel to close channel
		UnReadBuffer,		% indexed by channel for input backup
		LinePosition,		% indexed by channel for Posn()
		MaxLine;		% when to force an end-of-line

syslsp procedure Open(FileName, AccessType);	%. Get access to file
begin scalar FileDes;
    if AccessType eq 'INPUT then
    <<  FileDes := SystemOpenFileForInput FileName;
	UnReadBuffer[FileDes] := char NULL;
	WriteFunction[FileDes] := 'ReadOnlyChannel >>
    else if AccessType eq 'OUTPUT then
    <<  FileDes := SystemOpenFileForOutput FileName;
	LinePosition[FileDes] := 0;
	MaxLine[FileDes] := 80;
	ReadFunction[FileDes] := 'WriteOnlyChannel >>
    else if AccessType eq 'SPECIAL then
	if IDP LispVar SpecialReadFunction!*
		and IDP LispVar SpecialWriteFunction!*
		and IDP LispVar SpecialCloseFunction!* then
	<<  FileDes := SystemOpenFileSpecial FileName;
	    LinePosition[FileDes] := 0;
	    MaxLine[FileDes] := 80;
	    UnReadBuffer[FileDes] := char NULL;
	    ReadFunction[FileDes] := IdInf LispVar SpecialReadFunction!*;
	    WriteFunction[FileDes] := IdInf LispVar SpecialWriteFunction!*;
	    CloseFunction[FileDes] := IdInf LispVar SpecialCloseFunction!* >>
	else IOError "Improperly set-up special IO open call"
    else IOError "Unknown access type";
    return MkINT FileDes;
end;

syslsp procedure Close FileDes;		%. End access to file
begin scalar BareFileDes;
    BareFileDes := IntInf FileDes;
    if not (0 <= BareFileDes and BareFileDes <= MaxChannels) then
	NonIOChannelError(FileDes, "Close");
    IDApply1(BareFileDes, CloseFunction[BareFileDes]);
    SystemMarkAsClosedChannel FileDes;
    ReadFunction[BareFileDes] := 'ChannelNotOpen;
    WriteFunction[BareFileDes] := 'ChannelNotOpen;
    CloseFunction[BareFileDes] := 'ChannelNotOpen;
    return FileDes;
end;

off SysLisp;

END;
