%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 9836-Alpha.SL - Terminal Interface for 9836 Alpha Memory
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        13 December 1982
% Revised:     27 January 1983
%
% Note: uses efficiency hacks that require 80-column width!
% Note: contains 68000 LAP code; must be compiled!
% Note: uses all 25 lines; assumes keyboard input buffer has been relocated
%
% 27-Jan-83 Alan Snyder
%  Revise to use all 25 lines of the screen.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))
(CompileTime (load display-char fast-int syslisp))
  
(defflavor 9836-alpha (
  (height 25)           % number of rows (0 indexed)
  (maxrow 24)           % highest numbered row
  (width 80)            % number of columns (0 indexed)
  (maxcol 79)           % highest numbered column
  (cursor-row 0)        % cursor position
  (cursor-column 0)     % cursor position
  (raw-mode NIL)
  (buffer-address (int2sys 16#512000)) % an absolute address
  )
  ()
  (gettable-instance-variables height width maxrow maxcol raw-mode)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (9836-alpha get-character) ()
  (keyboard-input-character)
  )

(defmethod (9836-alpha ring-bell) ()
  (ChannelWriteChar 1 #\Bell)
  )

(defmethod (9836-alpha move-cursor) (row column)
  (setf cursor-row row)
  (setf cursor-column column)
  (screen-set-cursor-position row column)
  )

(defmethod (9836-alpha enter-raw-mode) ()
  (when (not raw-mode)
    % (EchoOff)
    % Enable Keypad?
    (setf raw-mode T)
    ))

(defmethod (9836-alpha leave-raw-mode) ()
  (when raw-mode
    (setf raw-mode NIL)
    % Disable Keypad?
    % (EchoOn)
    ))

(defmethod (9836-alpha erase) ()
  % This method should be invoked to initialize the screen to a known state.
  (setf cursor-column 0)
  (for (from row 0 maxrow)
       (do (setf cursor-row row)
	   (=> self clear-line)
	   ))
  (setf cursor-row 0)
  )

(defmethod (9836-alpha clear-line) ()
  (=> self write-line cursor-row #.(make-vector 80 32))
  )

(defmethod (9836-alpha convert-character) (ch)
  (setq ch (& ch (display-character-cons
		     (dc-make-enhancement-mask INVERSE-VIDEO
					       BLINK
					       UNDERLINE
					       INTENSIFY)
		     (dc-make-font-mask 0)
		     16#FF)))
  ch)

(defmethod (9836-alpha normal-enhancement) ()
  (dc-make-enhancement-mask)
  )

(defmethod (9836-alpha highlighted-enhancement) ()
  (dc-make-enhancement-mask INVERSE-VIDEO)
  )

(defmethod (9836-alpha supported-enhancements) ()
  (dc-make-enhancement-mask INVERSE-VIDEO BLINK UNDERLINE INTENSIFY)
  )

(defmethod (9836-alpha write-char) (row column ch)
  (screen80-write-char buffer-address row column ch)
  )

(defmethod (9836-alpha write-line) (row data)
  (screen80-write-line buffer-address row data)
  )

(defmethod (9836-alpha read-char) (row column)
  (let ((offset (+ column (* row width))))
    (halfword buffer-address offset)
    ))

% The following methods are provided for INTERNAL use only!

(defmethod (9836-alpha init) ()
  )

(lap '((*entry screen80-write-char expr 4) % buffer-address row column word
       (move!.l (reg 2) (reg t1))
       (moveq 80 (reg t2))
       (mulu (reg t1) (reg t2))
       (add!.l (reg 3) (reg t2))
       (lsl!.l 1 (reg t2))
       (move!.w (reg 4) (indexed (reg t2) (displacement (reg 1) 0)))
       (rts)
       ))

(lap '((*entry screen80-write-line expr 3) % buffer-address row data
       (move!.l (reg 2) (reg t1))       % move row address to T1
       (moveq 80 (reg t2))              % move 80 to T2
       (mulu (reg t1) (reg t2))         % multiply row address by 80
       (lsl!.l 1 (reg t2))              % convert to byte offset
       (adda!.l (reg t2) (reg 1))       % A1: address of line in buffer
       (move!.l (minus 80) (reg t1))
       (addq!.l 4 (reg 3))              % skip data header word
       (*lbl (label loop))
       (addq!.l 2 (reg 3))              % skip upper halfword in data 
       (move!.w (autoincrement (reg 3)) (autoincrement (reg 1)))
       (addq!.l 1 (reg t1))
       (bmi (label loop))
       (rts)
       ))
