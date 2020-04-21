%
% Sleep.SL - Sleep Primitive
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        15 July 1982
%
% 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 6-Aug-82, WFG:  Modified to include an "inefficient" VAX version.

(CompileTime (load if-system))

(BothTimes
  (progn
    (load common)
    (if_system Dec20
      (load jsys))))

(if_system Dec20
  (de sleep-until-timeout-or-input (n-60ths)     % Dec-20 version

    % Return when either of two conditions are met: (1) Input is available.
    % (2) The specified elapsed time (in units of 1/60th second) has elapsed.
    % Don't waste CPU cycles!

    (for (from i 1 n-60ths 2)
         (until (> (CharsInInputBuffer) 0))
         (do (Jsys0 33 0 0 0 (const jsDISMS)))
         ))
)

(if_system Unix
  (de sleep-until-timeout-or-input (n-60ths)     % Unix version
    % Should use the SELECT system call?
    % Return when either of two conditions are met: (1) Input is available.
    % (2) The specified elapsed time (in units of 1/60th second) has elapsed.
    (let ((timer (time)) % Get "current time" in milliseconds.
           % Approximate number of 1000ths to count (17 roughly equal
           % 16.6666...)
           (n-1000ths (* 17 n-60ths)))
      (for
        % Pause until time runs out,
        (while (< (- (time) timer) n-1000ths))
        % or a character is typed.
        (until (> (CharsInInputBuffer) 0))))))
