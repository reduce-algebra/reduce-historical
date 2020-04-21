%
% IO-DATA.RED - Data structures used by input and output
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        21 September 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL-20>IO-DATA.RED.2, 29-Dec-82 12:19:36, Edit by PERDUE
%  Added PagePosition array to support LPOSN

on SysLisp;

internal WConst MaxTokenSize = 5000;

exported WString TokenBuffer[MaxTokenSize];

exported WConst MaxChannels = 31;

exported WArray ReadFunction = ['TerminalInputHandler,
				'WriteOnlyChannel,	
				'WriteOnlyChannel,	
				'CompressReadChar,      
				'WriteOnlyChannel,      
				'ChannelNotOpen,        
				'ChannelNotOpen,        
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen],
		WriteFunction = ['ReadOnlyChannel,
				'Dec20WriteChar,
				'ToStringWriteChar,
				'ExplodeWriteChar,
				'FlatSizeWriteChar,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen],
		CloseFunction = ['IllegalStandardChannelClose,
				'IllegalStandardChannelClose,
				'IllegalStandardChannelClose,
				'IllegalStandardChannelClose,
				'IllegalStandardChannelClose,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen,
				'ChannelNotOpen],
		UnReadBuffer[MaxChannels],
		LinePosition[MaxChannels],
		PagePosition[MaxChannels],
		MaxLine = [0, 80,80, 10000, 10000,
					  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
			   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
		JFNOfChannel = [8#100,8#101,-1,-1,-1,
					  0,0,0,0,0,0,0,0,0,0,0, 
				0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0];


off SysLisp;

global '(!$EOL!$);
LoadTime(!$EOL!$ := '!
);

END;
