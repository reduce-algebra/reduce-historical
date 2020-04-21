%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Wait.SL - Wait Primitive (TOPS-20 Version)
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        23 September 1982
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  19-June-1983 Mark R. Swanson
%  Changed timeout-wait to accept a third argument: a list of args for F, its
%  first arg.  This routine is nearly identical to WAIT-TIMEOUT, found in
%  P20U:WAIT.SL and could replace it if calls on WAIT-TIMEOUT are converted to
%  three args.

(CompileTime (load fast-int))
(BothTimes (load jsys))

(de timeout-wait (f args n-60ths)

  % Return when either of two conditions are met: (1) The function F (of no
  % arguments) returns non-NIL; (2) The specified elapsed time (in units of
  % 1/60th second) has elapsed.  Don't waste CPU cycles!  Return the last
  % value returned by F (which is always invoked at least once).

  (let (result)
    (while (and (not (setf result (apply f args)))
	        (> n-60ths 0))
      (Jsys0 250 0 0 0 (const jsDISMS))
      (setf n-60ths (- n-60ths 15))
      )
    result
    ))
