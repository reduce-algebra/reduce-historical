%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FAST-VECTORS - Fast (unchecked) version of Vector Functions
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        17 September 1982
%
% Load this at compile-time to make compiled invocations of the following
% functions fast (and unchecked):
%
% (vector-fetch v i)
% (vector-store v i x)
% (vector-size v)
% (vector-upper-bound v)
% (vector-empty? v)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(load slow-vectors) % for the interpreted versions
(CompileTime (load fast-vector)) % for machine-dependent primitives

(put 'vector-fetch 'cmacro '(lambda (v i) (igetv v i)))
(put 'vector-store 'cmacro '(lambda (v i x) (iputv v i x)))
(put 'vector-size 'cmacro '(lambda (v) (Wplus2 (isizev v) 1)))
(put 'vector-upper-bound 'cmacro '(lambda (v) (isizev v)))
(put 'vector-empty? 'cmacro '(lambda (v) (WLessP (isizev v) 0)))
