%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FAST-STRINGS - Fast (unchecked) version of String Functions
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        17 September 1982
%
% Load this at compile-time to make compiled invocations of the following
% functions fast (and unchecked):
%
% (string-fetch s i)
% (string-store s i ch)
% (string-length s)
% (string-upper-bound s)
% (string-empty? s)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(load slow-strings) % for the interpreted versions
(CompileTime (load fast-vector)) % for machine-dependent primitives

(put 'string-fetch 'cmacro '(lambda (s i) (igets s i)))
(put 'string-store 'cmacro '(lambda (s i c) (iputs s i c)))
(put 'string-length 'cmacro '(lambda (s) (Wplus2 (isizes s) 1)))
(put 'string-upper-bound 'cmacro '(lambda (s) (isizes s)))
(put 'string-empty? 'cmacro '(lambda (s) (WLessP (isizes s) 0)))
