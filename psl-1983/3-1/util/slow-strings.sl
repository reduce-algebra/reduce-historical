%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% SLOW-STRINGS - Useful String Functions (with lots of error checking)
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        17 September 1982
%
% Defines the following functions:
%
% (string-fetch s i)
% (string-store s i ch)
% (string-length s)
% (string-upper-bound s)
% (string-empty? s)
%
% See FAST-STRINGS for faster (unchecked) compiled versions of these functions.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de string-fetch (s i)
  (cond ((not (StringP s)) (NonStringError s 'String-Fetch))
	((not (FixP i)) (NonIntegerError i 'String-Fetch))
	(t (indx s i))
	))

(de string-store (s i c)
  (cond ((not (StringP s)) (NonStringError s 'String-Store))
	((not (FixP i)) (NonIntegerError i 'String-Store))
	((not (FixP c)) (NonCharacterError c 'String-Store))
	(t (setindx s i c))
	))

(de string-length (s)
  (cond ((not (StringP s)) (NonStringError s 'String-Length))
	(t (Plus2 (size s) 1))
	))

(de string-upper-bound (s)
  (cond ((not (StringP s)) (NonStringError s 'String-Upper-Bound))
	(t (size s))
	))

(de string-empty? (s)
  (cond ((not (StringP s)) (NonStringError s 'String-Empty?))
	(t (EqN (size s) -1))
	))
