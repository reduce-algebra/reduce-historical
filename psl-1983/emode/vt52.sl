%
% VT52.SL - EMODE support for VT52 terminals.  (Same as Teleray except for
% parity_mask?)
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        27 June 1982
% Copyright (c) 1982 University of Utah
%

% Screen starts at (0,0), and other corner is offset by (79,23)  (total
% dimensions are 80 wide by 24 down)
(setf ScreenBase (Coords 0 0))
(setf ScreenDelta (Coords 79 23))

% Parity mask is used to clear "parity bit" for those terminals that don't
% have a meta key.  It should be 8#177 in that case.  Should be 8#377 for
% terminals with a meta key.
(setf parity_mask 8#177)

(DE EraseScreen ()
  (PBOUT (Char FF)))     % Form feed to clear the screen

(DE Ding ()
  (PBOUT (Char Bell)))

% Clear to end of line from current position (inclusive).
(DE TerminalClearEol ()
  (progn
    (PBOUT (Char ESC))
    (PBOUT (Char K))))

% Move physical cursor to Column,Row
(DE SetTerminalCursor (ColLoc RowLoc)
  (progn
    (PBOUT (char ESC))
    (PBOUT (char Y))
    (PBOUT (plus (char BLANK) RowLoc))
    (PBOUT (plus (char BLANK) ColLoc))))
