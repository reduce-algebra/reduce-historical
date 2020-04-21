% MINI-CHAR-IO.RED

Procedure ChannelWriteChar(chn,x);
  PutC x;

Procedure WriteChar Ch;
  IndependentWriteChar(Out!*,Ch);

End;
