%==============================================================================
%
% SYSTEM-IO.RED - System independent IO routines for PSL
% 
% Author:      Modified by Robert R. Kessler
%              From System-io.red for the VAX by Eric Benson
%              Computer Science Dept.
%              University of Utah
% Date:        Modified 16 August 1982
%	       Original Date 16 September 1981
%
% Copyright (c) 1982 University of Utah
%
%==============================================================================

% Each individual system must have the following routines defined.
%
%   The following definitions are used in the routines:
%    FileDescriptor - A machine dependent word that references a file once
%		      opened; generated by the Open
%    FileName - A Lisp string of the file name.
%
%  FileDescriptor := SysOpenRead (Channel,FileName);
%                                             % Open FileName for input and
%					      % return a file descriptor used
%					      % in later references to the
%					      % file. Channel used only
%                                             % if needed to generate FileDesc
%  FileDescriptor := SysOpenWrite (Channel,FileName); 
%                                             % Open FileName for output and
%					      % return a file descriptor used
%					      % in later references to the
%					      % file. Channel used only
%                                             % if needed to generate FileDesc
%  SysWriteRec (FileDescriptor, StringToWrite, StringLength); 
%					      % Write StringLength characters
%					      % from StringToWrite from the 
%					      % first position.  
%  LengthRead := SysReadRec (FileDescriptor, StringBuffer);
%					      % Read from the FileDescriptor, a
%					      %  record into the StringBuffer.
%					      %  Return the length of the 
%					      %  string read.
%  SysClose (FileDescriptor);		      % Close FileDescriptor, allowing
%					      %  it to be reused.
%  TerminalInputHandler (FileDescriptor);     % Input from the terminal, on
%                  			      %  FileDescriptor.  This routine
%					      %  is expected to use the prompt
%					      %  in PromptString!*.
%
%==============================================================================

CompileTime Load Fast!-Vector;

global '(IN!* OUT!*);
LoadTime <<
IN!* := 0;
OUT!* := 1;
>>;

fluid '(StdIN!* StdOUT!* ErrOUT!* PromptOUT!* !*Echo);
LoadTime <<
StdIN!* := 0;
StdOUT!* := 1;
ErrOUT!* := 5;
PromptOUT!* := 6;
>>;

%==============================================================================
%
on SysLisp;

%  The channel table contains the actual file descriptor as returned from
%   the open routines.  Since the file descriptor may be any value, it
%   may not be used in finding a free channel.  Therefore, we now have a
%   warray ChannelStatus that is the current status of the channel.
%  NOTE: ChannelStatus must be initialized to all closed.

%  The following constants are used to indicate the status of the Channel.
WConst ChannelClosed = 0, 
       ChannelOpenRead = 1,
       ChannelOpenWrite = 2,
       ChannelOpenSpecial = 3;

%  Look into the ChannelStatus array for a free channel.
syslsp procedure FindFreeChannel();
begin scalar Channel;
    Channel := 0;
    while ChannelStatus [Channel] neq ChannelClosed do
    << if Channel >= MaxChannels then
        IOError "No free channels left";
       Channel := Channel + 1 >>;
    return Channel;
end;

CompileTime fluid '(IOBuffer);

%   Open the argument filename as a read only file.
syslsp procedure SystemOpenFileForInput FileName;
begin scalar Channel;
    Channel := FindFreeChannel();
    ChannelTable [Channel] := SysOpenRead (Channel,FileName);
    ChannelStatus[Channel] := ChannelOpenRead;
    MaxBuffer    [Channel] := SysMaxBuffer (ChannelTable [Channel]);
    ReadFunction   [Channel] := 'IndependentReadChar;
    WriteFunction  [Channel] := 'ReadOnlyChannel;
    CloseFunction  [Channel] := 'IndependentCloseChannel;
    IGetV (LispVar IOBuffer, Channel) := 
        MkString (MaxBuffer [Channel], 32);
    NextPosition [Channel] := 0; % Will be post Incremented
    BufferLength [Channel] := -1;
    return Channel;
end;

syslsp procedure SystemOpenFileForOutput FileName;
begin scalar Channel;
    Channel := FindFreeChannel();
    ChannelTable [Channel] := SysOpenWrite (Channel,FileName);
    ChannelStatus[Channel] := ChannelOpenWrite;
    MaxBuffer    [Channel] := SysMaxBuffer (ChannelTable [Channel]);
    ReadFunction   [Channel] := 'WriteOnlyChannel;
    WriteFunction  [Channel] := 'IndependentWriteChar;
    CloseFunction  [Channel] := 'IndependentCloseChannel;
    Igetv(LispVar IOBuffer,Channel) := MkString (MaxBuffer [Channel], 32);
    NextPosition [Channel] := -1; % Will be set pre-incremented
    BufferLength [Channel] := MaxBuffer [Channel];
    return Channel;
end;

%  Mark a channel as open for a special purpose.
syslsp procedure SystemOpenFileSpecial FileName;
begin scalar Channel;
 ChannelStatus [Channel] := ChannelOpenSpecial;
 return Channel
end;

syslsp procedure TestLegalChannel Channel;
 If not( PosIntP Channel and Channel <=MaxChannels)
  then IoError List(Channel," is not a legal channel ");

%   This function will read in a character from the buffer.  It will read
%    the record on buffer length overflow only.  Thus when an EOL character
%    is read, it is processed as any other character, except, if it is the last
%    one, in the record, it will do the read automatically.
%    Note, this will not read the next record until after the final character
%    has been processed.  
syslsp procedure IndependentReadChar Channel;
begin scalar Chr;
    TestLegalChannel Channel;
    if NextPosition [Channel] > BufferLength [Channel] then
    << BufferLength [Channel] := 
         SysReadRec (ChannelTable[Channel], 
	   IGetV(LispVar IOBuffer, Channel));
       NextPosition [Channel] := 0 >>;
    Chr := StrByt (IGetV (LispVar IOBuffer, Channel), 
                   NextPosition [Channel]);
    NextPosition [Channel] := NextPosition [Channel] + 1;
    if LispVar !*Echo then WriteChar Chr;
    return Chr;
end;

%   Write a character into the buffer.  Actually dump the buffer when the
%    EOL character is found, or when the buffer is full.  This happens 
%    immediately upon meeting this condition, not waiting for the 
%    next character.  Note, that this places the EOL character into the
%    buffer for machine dependent treatment as CR/LF etc
syslsp procedure IndependentWriteChar (Channel, Chr);
 Begin
   TestLegalChannel Channel;
   NextPosition [Channel] := NextPosition [Channel] + 1;
   StrByt (IGetV (LispVar IOBuffer, Channel), NextPosition [Channel]) 
       := Chr;
   if (Chr eq char EOL) or
      (NextPosition [Channel] >= BufferLength [Channel]) then
%     12/13/82 - rrk Placed code in FlushBuffer and added a call.
       FlushBuffer Channel;
  End;

%  12/13/82 - rrk Added FlushBuffer procedure.
%   Flush out the buffer whether or not we have an EOL character.
Procedure FlushBuffer Channel;
<< SysWriteRec (ChannelTable[Channel], 
                IGetV (LispVar IOBuffer, Channel),
                NextPosition [Channel]);
   NextPosition[Channel] :=-1 >>; % Start Fresh Buffer

%   Mark the argument channel as closed and update the read, write and
%    close functions likewise.  Careful, if the caller does this first
%    and then trys to access a read, write or close function we are
%    in big trouble.  Is it correct to do this?????  Or is a marking of
%    the channel status table sufficient.
syslsp procedure SystemMarkAsClosedChannel Channel;
<< TestLegalChannel Channel;
   ChannelStatus [Channel] := ChannelClosed;
   ReadFunction [Channel] := WriteFunction [Channel] :=
    CloseFunction [Channel] := 'ChannelNotOpen >>;

%   Actually close the argument channel.
syslsp procedure IndependentCloseChannel Channel;
  <<    TestLegalChannel Channel;
        SysClose ChannelTable [Channel]>>;

% Initialize Channel Tables etc
Syslsp procedure ClearOneChannel(Chn,Bufflen,How);
 << MaxBuffer [Chn] := Bufflen;
    NextPosition [Chn] := 0;
   % SAL - Next two not properly initialized.
    LinePosition [Chn] := 0;
    UnreadBuffer [Chn] := 0;
    If how eq 'Input then   BufferLength [Chn] := -1
     else  BufferLength [Chn] := 0;
    IGetV (LispVar IOBuffer, Chn) := MkString(Bufflen,32)>>;

syslsp procedure ClearIO();
<< SysClearIo();
   If not VectorP LispVar Iobuffer then
     <<LispVar IOBuffer := MkVect (MaxChannels);
       ClearOneChannel(LispVar StdIn!*,200,'Input);
       ClearOneChannel(LispVar StdOut!*,200,'Output);
       ClearOneChannel(LispVar ErrOut!*,200,'OutPut);
       ClearOneChannel(LispVar PromptOut!*,200,'Output)>>;
    LispVar IN!* := LispVar StdIN!*;
    LispVar OUT!* := LispVar StdOUT!* >>;

syslsp procedure TerminalInputHandler Channel;
begin scalar Chr;
    TestLegalChannel Channel;
    if NextPosition [Channel] > BufferLength [Channel] then
    << ChannelWriteString(LispVar PromptOUT!*, 
	   		   if StringP LispVar PromptString!*
		             then LispVar PromptString!*
			     else ">");
%     12/13/82 - rrk Flush out the Prompt character.
       FlushBuffer LispVar PromptOut!*;
       BufferLength [Channel] := SysReadRec (ChannelTable[Channel], 
           IGetV (LispVar IOBuffer, Channel));
       NextPosition [Channel] := 0 >>;
    Chr := StrByt (IGetV (LispVar IOBuffer, Channel), 
                   NextPosition [Channel]);
    NextPosition [Channel] := NextPosition [Channel] + 1;
    if LispVar !*Echo then WriteChar Chr;
    return Chr;
end;

off SysLisp;

END;
