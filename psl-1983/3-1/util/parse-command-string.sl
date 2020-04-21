%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Parse-Command-String.SL - Parse Program Command String
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        10 August 1982
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load common fast-vector))

(de parse-command-string (s)

  % This procedure accepts a string and parses it into a sequence
  % of substrings separated by spaces.  It is used to parse the
  % "command string" given to the PSL program when it is invoked.

  (let (s-list j
	(high (size s))
	(i 0))
    (while T
	   % Scan for the beginning of an argument.
           (while (<= i high)
		  (cond ((= (igets s i) (char space))
			 (setq i (+ i 1))
			 )
			(t (exit)))
		  )
	   (if (> i high) (exit))
	   % Scan for the end of the argument.
           (setq j i)
	   (while (<= j high)
		  (cond ((= (igets s j) (char space))
			 (exit)
			 )
			(t (setf j (+ j 1))))
		  )
	   (setq s-list (aconc s-list (substring s i j)))
	   (setq i (+ j 1))
	   )
    s-list))
