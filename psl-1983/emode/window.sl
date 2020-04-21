%
% Window.SL - Individual Window Manipulation Functions
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        20 July 1982
%
% This file contains functions that manipulate individual windows.
% It is intended that someday EMODE will be reorganized
% so that all such functions will eventually be in this file.
%
% This file requires COMMON.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(CurrentWindowDelta TopOfDisplayIndex))

(de current-window-height ()
  % Return the number of rows in the current window.
  (+ (Row CurrentWindowDelta) 1)
  )

(de current-window-top-line ()
  % Return the index of the buffer line at the top of the current window.
  TopOfDisplayIndex
  )

(de current-window-set-top-line (new-top-line)
  % Change which buffer line displays at the top of the current window.
  (setf TopOfDisplayIndex new-top-line)
  )
