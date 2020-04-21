%
% HP2648A.SL - EMODE support for HP2648A terminals.
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        27 June 1982
% Copyright (c) 1982 University of Utah
%

%%%%% Changes: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% CSP 7/7/82
% - Changed Meta- prefix char to C-\.
% - Defined ESCAPE as genuine prefix character.
% - Changed parity_mask for HP terminals to 8#377.

% CSP 7/8/82
% - This file now redefines quit.

% AS 7/20/82
% - Added ESC-x hooks for line and page scrolling (defined in hp-emodex).

% AS 8/6/82
% - Simple optimization of SetTerminalCursor to reduce number of characters
%   sent to the terminal.

% AS 8/12/82
% - Define Terminal-Enter-Raw-Mode and Terminal-Leave-Raw-Mode to
%   enable and disable keypad.  Removed unnecessary redefinitions of
%   EMODE functions that now invoke these new functions.

(fluid '(*EMODE ScreenBase ScreenDelta parity_mask))

% Screen starts at (0,0), and other corner is offset by (79,23)  (total
% dimensions are 80 wide by 24 down)

(setf ScreenBase (Coords 0 0))
(setf ScreenDelta (Coords 79 23))

% Parity mask is used to clear "parity bit" for those terminals that don't
% have a meta key.  It should be 8#177 in that case.  Should be 8#377 for
% terminals with a meta key.
(setq parity_mask 8#377)

(de EraseScreen ()
    % Cursor home
    (PBOUT (char ESC))
    (PBOUT (char H))

    % Now clear to end of screen
    (PBOUT (char ESC))
    (PBOUT (char J)))

(de Ding ()
    (PBOUT (char BELL)))

(de TerminalClearEol ()
% Clear to end of line from current position (inclusive).
    (PBOUT (char ESC))
    (PBOUT (char K)))

(de SetTerminalCursor (ColLoc RowLoc)

% Move physical cursor to Column,Row

  (if (and (= RowLoc 0) (= ColLoc 0))
    (progn (PBOUT (char ESC)) (PBOUT (char H)))
    % Else
    (PBOUT (char ESC))
    (PBOUT (char '!&))
    (PBOUT (char !a))

    % Use "quick and dirty" conversion to decimal digits.
    (if (> RowLoc 9)
        (PBOUT (plus (char 0) (quotient RowLoc 10)))
	)
    (PBOUT (plus (char 0) (remainder RowLoc 10)))

    % Delimiter between row digits and column digits.
    (PBOUT (char (lower R)))

    (if (> ColLoc 9)
        (PBOUT (plus (char 0) (quotient ColLoc 10)))
	)
    (PBOUT (plus (char 0) (remainder ColLoc 10)))

    (PBOUT (char C))  % Terminate the sequence
    ))

% EMODE must be loaded first!

(define_prefix_character (char Escape) "Esc-")

(mapc (list
       (list (char (cntrl !\)) 'EscapeAsMeta)
       (list (CharSequence escape J) 'FullRefresh)
       (list (CharSequence escape A) '!$BackwardLine)
       (list (CharSequence escape B) '!$ForwardLine)
       (list (CharSequence escape C) '!$ForwardCharacter)
       (list (CharSequence escape D) '!$BackwardCharacter)
       (list (CharSequence escape !h) '!$BeginningOfBuffer)
       (list (CharSequence escape F) '!$EndOfBuffer)
       (list (CharSequence escape 5) 'forward_word)
       (list (CharSequence escape 4) 'backward_word)
       (list (CharSequence escape U) 'scroll-window-up-page-command)
       (list (CharSequence escape V) 'scroll-window-down-page-command)
       (list (CharSequence escape P) '$DeleteForwardCharacter)
       (list (CharSequence escape M) 'kill_line)
       (list (CharSequence escape L) 'OpenLine)
       (list (CharSequence escape S) 'scroll-window-up-line-command)
       (list (CharSequence escape T) 'scroll-window-down-line-command)
       )
      (function
       (lambda (lis)
	 (AddToKeyList 'BasicDispatchList (car lis) (cadr lis)))))

(de terminal-enter-raw-mode ()
    % Enable Keypad
    (PBOUT (char escape))
    (pbout (char !&))
    (pbout (char !s))
    (pbout (char 1))
    (pbout (char A)))

(de terminal-leave-raw-mode ()
    % Disable Keypad
    (PBOUT (char escape))
    (pbout (char !&))
    (pbout (char !s))
    (pbout (char 0))
    (pbout (char A)))
