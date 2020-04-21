%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Processor-Time.SL (TOPS-20 Version)
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        22 September 1982
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (put 'hptim 'OpenCode '((jsys 8#501) (jfcl))))

(de processor-time ()
  % Return accumulated processor time for the current process in microseconds.
  (WTimes2 (hptim 1) 10)
  )
