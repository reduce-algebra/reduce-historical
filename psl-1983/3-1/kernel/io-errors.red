%
% IO-ERRORS.RED - Error handlers for input and output
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        27 August 1981
% Copyright (c) 1981 University of Utah
%

on SysLisp;

syslsp procedure ChannelNotOpen(Chn, Ch);
    ChannelError(Chn, "Channel not open");

syslsp procedure WriteOnlyChannel Chn;
    ChannelError(Chn, "Channel open for write only");

syslsp procedure ReadOnlyChannel(Chn, Ch);
    ChannelError(Chn, "Channel open for read only");

syslsp procedure IllegalStandardChannelClose Chn;
    ChannelError(Chn, "Illegal to close standard channel");

syslsp procedure IOError(Message);
    StdError BldMsg("I/O Error: %s", Message);

syslsp procedure ChannelError(Channel, Message);
    StdError BldMsg("I/O Error on channel %d: %s", IntInf Channel, Message);

off SysLisp;

END;
