% MINI-OPEN-CLOSE.RED   Some minimal User Level I/O routines:

Procedure Open(FileName,How);
 If how eq 'Input then SystemOpenFileForInput FileName
  else  if how eq 'OutPut then SystemOpenFileForOutPut FileName
  else IoError "Cant Open";

Procedure Close N;
  IndependentCloseChannel N;

end;
