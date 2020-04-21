%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% SLOW-VECTORS - Useful Vector Functions (with lots of error checking)
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        17 September 1982
%
% Defines the following functions:
%
% (vector-fetch v i)
% (vector-store v i x)
% (vector-size v)
% (vector-upper-bound v)
% (vector-empty? v)
%
% See FAST-VECTORS for faster (unchecked) compiled versions of these functions.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de vector-fetch (v i)
  (cond ((not (Vectorp v)) (NonVectorError v 'Vector-Fetch))
	((not (FixP i)) (NonIntegerError i 'Vector-Fetch))
	(t (indx v i))
	))

(de vector-store (v i x)
  (cond ((not (Vectorp v)) (NonVectorError v 'Vector-Store))
	((not (FixP i)) (NonIntegerError i 'Vector-Store))
	(t (setindx v i x))
	))

(de vector-size (v)
  (cond ((not (Vectorp v)) (NonVectorError v 'Vector-Size))
	(t (Plus2 (size v) 1))
	))

(de vector-upper-bound (v)
  (cond ((not (Vectorp v)) (NonVectorError v 'Vector-Upper-Bound))
	(t (size v))
	))

(de vector-empty? (v)
  (cond ((not (Vectorp v)) (NonVectorError v 'Vector-Empty?))
	(t (EqN (size v) -1))
	))
