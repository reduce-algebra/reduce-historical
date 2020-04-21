%==============================================================================
%
% PT20:XXX-SYSTEM-IO.RED - 20 specific IO routines for PSL
% 
% Author:      Modified by Robert R. Kessler and MLG
%              From System-io.red for the 20 by Eric Benson
%              Computer Science Dept.
%              University of Utah
% Date:        Modified 16 August 1982
%	       Original Date 16 September 1981
%
% Copyright (c)  1982 University of Utah
%
%==============================================================================

ON Syslisp;

% Each individual system must have the following routines defined.
% SysClearIo, SysOpenRead, SysOpenWrite, SysReadRec, SysWriteRec, SysClose,
% SysMaxBuffer
%
%   The following definitions are used in the routines:
%    FileDescriptor - A machine dependent word that references a file once
%		      opened.
%    FileName - A Lisp string of the file name.
%
% ---------- SysClearIo:
%                      called by Cleario for system dep extras

lap '((!*entry SysClearIO expr 0)
%
% ^C from RDTTY and restart causes trouble, but we don't want a full RESET
% (don't want to close files or kill forks), so we'll just do the
% part of RESET that we want, for terminal input
%
	(!*MOVE (WConst 8#100) (reg 1))	% .priin
	(rfmod)
	(tro 2 2#001111100001000000)	% tt%wak + tt%eco + .ttasi, like RESET
	(sfmod)
	(!*EXIT 0)
);


syslsp procedure SysOpenRead(Channel,FileName);
%                                             % Open FileName for input and
%					      % return a file descriptor used
%					      % in later references to the
%					      % file.
 Begin scalar Jfn;
  Jfn:=Dec20Open(FileName,
		     %  gj%old	    gj%sht
		     2#001000000000000001000000000000000000,
		     % 7*of%bsz		of%rd
		     2#000111000000000000010000000000000000);
 if JFN eq 0 then return ContOpenError(FileName, 'INPUT);
 return Jfn;
End;

syslsp procedure SysOpenWrite(Channel,FileName);
 Begin scalar Jfn;
   Jfn:=Dec20Open(FileName,
		    % gj%fou gj%new gj%sht
		    2#110000000000000001000000000000000000,
		    % 7*of%bsz		of%wr
		    2#000111000000000000001000000000000000);
  if JFN eq 0 then return ContOpenError(FileName, 'OUTPUT);
  return Jfn;
 End;

lap '((!*entry Dec20Open expr 3)
%
%	Dec20Open(Filename string, GTJFN bits, OPENF bits)
%
	(!*WPLUS2 (reg 1) (WConst 1))	% increment r1 to point to characters
	(hrli (reg 1) 8#440700)		% turn r1 into a byte pointer
	(!*MOVE (reg 1) (reg 4))	% save filename string in r4
	(!*MOVE (reg 2) (reg 1))	% GTJFN flag bits in r1
	(!*MOVE (reg 4) (reg 2))	% string in r2
	(gtjfn)
	(!*JUMP (Label CantOpen))
	(!*MOVE (reg 3) (reg 2))	% OPENF bits in r2, JFN in r1
	(openf)
CantOpen
	(!*MOVE (WConst 0) (reg 1))	% return 0 on error
	(!*EXIT 0)			% else return the JFN
);


syslsp procedure SysReadRec(FileDescriptor,StringBuffer);
%					      % Read from the FileDescriptor, a
%					      %  record into the StringBuffer.
%					      %  Return the length of the 
%					      %  string read.
 Begin scalar N,Ch;
        N:=0;
  Loop: Ch:=Dec20ReadChar(FileDescriptor);
        StrByt(StringBuffer,N):=Ch;
        If Ch eq Char EOL or Ch eq Char EOF then return N;
        N:=N+1;
        % Check buffer size here
        goto Loop;
  End;

lap '((!*entry Dec20ReadChar expr 1)
Loop
	(bin)				% read a character
	(erjmp CheckEOF)		% check for end-of-file on error
	(!*JUMPEQ (Label Loop) (reg 2) (WConst 0))% try again if it's null char
	(!*JUMPEQ (Label Loop) (reg 2) (WConst 8#15))% or carriage return
	(!*MOVE (reg 2) (reg 1))	% move char to reg 1
%/      (camn (reg nil) (fluid !*ECHO))	% is echo on?
	(!*EXIT 0)			% no, just return char
%/	(!*PUSH (reg 1))		% yes, save char
%/	(!*CALL WriteChar)		% and write it
%/	(!*POP (reg 1))			% restore it
%/	(!*EXIT 0)			% and return
CheckEOF
	(gtsts)				% check file status
	(tlnn (reg 2) 2#000000001000000000)	% gs%eof
	(!*JUMP (Label ReadError))
	(!*MOVE (WConst 26) (reg 1))	% return EOF char
	(!*EXIT 0)
ReadError
	(!*MOVE (QUOTE "Attempt to read from file failed") (reg 1))
	(!*JCALL IoError)
);


syslsp procedure  SysWriteRec (FileDescriptor, StringToWrite, StringLength); 
%					      % Write StringLength characters
%					      % from StringToWrite from the 
%					      % first position.  
 for i:=0:StringLength do 
   Dec20WriteChar(FileDescriptor,strbyt(StringToWrite,i));

lap '((!*entry Dec20WriteChar expr 2)
 % Jfn,Chr
	(!*JUMPEQ (Label CRLF) (reg 2) (WConst 8#12))	% if LF, echo CRLF
	(bout)				% no, just echo char
	(!*EXIT 0)			% return
CRLF
	(!*MOVE (WConst 8#15) (reg 2))	% write carriage-return
	(bout)
	(!*MOVE (WConst 8#12) (reg 2))	% write linefeed
	(bout)
	(!*EXIT 0)			% return
);

%  SysClose (FileDescriptor);		      % Close FileDescriptor, allowing
%					      %  it to be reused.
lap '((!*entry SysClose expr 1)
	(closf)
	(!*JUMP (Label CloseError))
	(!*EXIT 0)
CloseError
	(!*MOVE (QUOTE "Channel could not be closed") (reg 1))
	(!*JCALL ChannelError)
);

syslsp procedure SysMaxBuffer(FileDesc);
 200;

End;
