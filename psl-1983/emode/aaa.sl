%
% AAA.SL - EMODE support for Ann Arbor Ambassador terminals (nearly
% identical to DEC VT100).
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        27 June 1982
% Copyright (c) 1982 University of Utah
%

% Screen starts at (0,0), and other corner is offset by (79,47)  (total
% dimensions are 80 wide by 48 down).  This corresponds to the values that
% seem popular at the University of Utah CS Department.  With a bit more
% work, we might change the driver so that it set up the screen dimensions
% by transmitting the appropriate character sequence to the terminal.
(setf ScreenBase (Coords 0 0))
(setf ScreenDelta (Coords 79 47))

% Parity mask is used to clear "parity bit" for those terminals that don't
% have a meta key.  It should be 8#177 in that case.  Should be 8#377 for
% terminals with a meta key.
(setf parity_mask 8#377)

(DE EraseScreen ()
  (progn
    % First, erase the screen
    (PBOUT (Char ESC))
    (PBOUT (Char ![))
    (PBOUT (Char 2))
    (PBOUT (Char J))

    % then put the cursor at "home".
    (SetTerminalCursor 0 0)))

(DE Ding ()
  (PBOUT (Char Bell)))

% Clear to end of line from current position (inclusive).
(DE TerminalClearEol ()
  (progn
    (PBOUT (Char ESC))
    (PBOUT (Char ![))
    (PBOUT (char !0))
    (PBOUT (Char K))))

% Move physical cursor to Column,Row
(DE SetTerminalCursor (ColLoc RowLoc)
  (progn
    (PBOUT (char ESC))
    (PBOUT (Char ![))
    % Use "quick and dirty" conversion to decimal digits.
    (PBOUT (plus (char 0) (quotient (add1 RowLoc) 10)))
    (PBOUT (plus (char 0) (remainder (add1 RowLoc) 10)))

    % Delimiter between row digits and column digits.
    (PBOUT (char !;))

    (PBOUT (plus (char 0) (quotient (add1 ColLoc) 10)))
    (PBOUT (plus (char 0) (remainder (add1 ColLoc) 10)))

    (PBOUT (char H))     % Terminate the sequence
    ))
