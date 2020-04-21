%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 9836-Color.SL - Terminal Interface for 9836 Color Display
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        23 December 1982
% Revised:     16 March 1983
%
% 16-Mar-83 Alan Snyder
%  Removed font definition (now in Font8.SL).  New font definition supports
%  8-bit characters.  Speed up write-char using hand-coded assembly language
%  routines.  Speed up erase using tail recursion.
% 4-Mar-83 Alan Snyder
%  Check for 8-bit characters being displayed.
% 29-Dec-82 Alan Snyder
%  Added SET-CHARACTER-PATTERN method.
%  Font hacking; changed: ' ` " a b d p q r s u
%  Use WPUTV instead of PutWord (it's faster, because it's open-coded).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))
(CompileTime (load display-char fast-vectors numeric-operators syslisp))
(on fast-integers)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% External variables:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(font8-patterns))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defflavor 9836-color
  (
   (height 28)			% number of rows (0 indexed)
   (maxrow 27)			% highest numbered row
   (width 64)			% number of columns (0 indexed)
   (maxcol 63)			% highest numbered column
   (cursor-row 0)		% cursor position
   (cursor-column 0)		% cursor position
   (raw-mode NIL)
   (inverse-video? NIL)
   (color-card (+ 16#600000 (* 28 16#10000)))
   (blue-plane (+ color-card 32768))
   (green-plane (+ blue-plane 32768))
   (red-plane (+ green-plane 32768))
   (text-plane green-plane)
   (cursor-plane red-plane)
   (background-plane blue-plane)
   (color-register-values [41 32 34 3 50 5 49 49 0 7 0 0 0 0 0 0 0 0])
   (color-raster-width 512)
   (color-raster-height 392)
   (color-raster-area (* color-raster-width color-raster-height))
   (color-raster-area-bytes (/ color-raster-area 8))
   (color-raster-area-halfwords (/ color-raster-area 16))
   (color-raster-area-words (/ color-raster-area 32))
   (bytes-per-row (/ color-raster-width 8))
   (character-height 14)
   (character-row-spacing 14)
   (bytes-per-character-row (* bytes-per-row character-row-spacing))
   (blank-pattern (make-vector character-height 0))
   (full-pattern (make-vector character-height -1))
   patterns
   )
  ()
  (gettable-instance-variables height width maxrow maxcol raw-mode)
  (settable-instance-variables inverse-video?)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (9836-color select-color) (new-color)
  (selectq new-color
    (GREEN (setf text-plane green-plane))
    (BLUE (setf text-plane blue-plane))
    (RED (setf text-plane red-plane))
    ))

(defmethod (9836-color select-cursor-color) (new-color)
  (=> self write-cursor 0)
  (selectq new-color
    (GREEN (setf cursor-plane green-plane))
    (BLUE (setf cursor-plane blue-plane))
    (RED (setf cursor-plane red-plane))
    )
  (=> self write-cursor -1)
  )

(defmethod (9836-color select-background-color) (new-color)
  (selectq new-color
    (GREEN (setf background-plane green-plane))
    (BLUE (setf background-plane blue-plane))
    (RED (setf background-plane red-plane))
    (nil (setf background-plane nil))
    )
  )

(defmethod (9836-color get-character) ()
  (keyboard-input-character)
  )

(defmethod (9836-color ring-bell) ()
  (ChannelWriteChar 1 #\Bell)
  )

(defmethod (9836-color move-cursor) (row column)
  (=> self write-cursor 0)
  (setf cursor-row row)
  (setf cursor-column column)
  (=> self write-cursor -1)
  )

(defmethod (9836-color write-cursor) (bits)
  (let ((byte-offset (* cursor-row bytes-per-character-row)))
    (setf byte-offset (+ byte-offset cursor-column))
    (for (from i 0 13)
	 (do
	  (putbyte cursor-plane byte-offset bits)
	  (setf byte-offset (+ byte-offset bytes-per-row))
	  ))))

(defmethod (9836-color enter-raw-mode) ()
  (when (not raw-mode)
    % (EchoOff)
    % Enable Keypad?
    (=> self display-on)
    (setf raw-mode T)
    ))

(defmethod (9836-color leave-raw-mode) ()
  (when raw-mode
    (setf raw-mode NIL)
    % Disable Keypad?
    % (EchoOn)
    ))

(defmethod (9836-color display-on) ()
  (for (from i 0 17)
       (do (putbyte color-card 16 i)
	   (putbyte color-card 18 (vector-fetch color-register-values i))
	   ))
  (putbyte color-card 1 -128)
  )

(defmethod (9836-color display-off) ()
  (putbyte color-card 1 0)
  )

(defmethod (9836-color erase) ()
  % This method should be invoked to initialize the screen to a known state.
  (let ((blue-word (if (= background-plane blue-plane) -1 0))
	(green-word (if (= background-plane green-plane) -1 0))
	(red-word (if (= background-plane red-plane) -1 0))
	(count color-raster-area-words)
	)
    (=> self &fill-plane blue-plane blue-word count)
    (=> self &fill-plane green-plane green-word count)
    (=> self &fill-plane red-plane red-word count)
    )
  (setf cursor-column 0)
  (setf cursor-row 0)
  (=> self move-cursor 0 0)
  )

(defmethod (9836-color &fill-plane) (plane word-value count)
  % Fill the specified plane with the specified word.
  (when (> count 0)
    (wputv plane 0 word-value)
    (=> self &fill-plane (+ plane 4) word-value (- count 1))
    ))

(defmethod (9836-color clear-line) ()
  % Not implemented yet.
  )

(defmethod (9836-color convert-character) (ch)
  (setq ch (& ch (display-character-cons
		  (dc-make-enhancement-mask INVERSE-VIDEO
					    % BLINK
					    % UNDERLINE
					    % INTENSIFY
					    )
		  (dc-make-font-mask 0)
		  16#FF))) % 8 bits
  ch)

(defmethod (9836-color normal-enhancement) ()
  (dc-make-enhancement-mask)
  )

(defmethod (9836-color highlighted-enhancement) ()
  (dc-make-enhancement-mask INVERSE-VIDEO)
  )

(defmethod (9836-color supported-enhancements) ()
  (dc-make-enhancement-mask INVERSE-VIDEO
			    % BLINK UNDERLINE INTENSIFY
			    )
  )

(defmethod (9836-color write-line) (row line)
  (for (from col 0 maxcol)
       (do (=> self write-char row col (vector-fetch line col)))
       ))

(defmethod (9836-color write-char) (row column ch)
  (let* ((pattern (vector-fetch patterns (dc-character-code ch)))
	 (inverse-bit (& ch (dc-make-enhancement-mask INVERSE-VIDEO)))
	 (byte-offset (mul16 row bytes-per-character-row))
	 (address (+ text-plane (+ byte-offset column)))
	 (inverse? (xor (~= 0 inverse-bit) inverse-video?))
	 )
    (if inverse?
      (write-inverted-char-raster pattern address bytes-per-row 14)
      (write-char-raster pattern address bytes-per-row 14)
      )))

(defmethod (9836-color set-character-pattern) (ch pattern)
  % CH must be an ASCII code (0..255); pattern must be a vector
  % of bytes or NIL.

  (when (and (fixp ch) (>= ch 0) (<= ch (vector-upper-bound patterns))
	     (or (null pattern) (vectorp pattern))
	     )
    (if (null pattern)
      (setf pattern blank-pattern)
      (setf pattern (copyvector pattern))
      )
    (when (< (vector-size pattern) character-height)
      (setf pattern
	(concat pattern
		(make-vector (- character-height (vector-size pattern)) 0))))
    (vector-store patterns ch pattern)
    ))

% The following methods are provided for INTERNAL use only!

(defmethod (9836-color init) (init-plist)
  (setf patterns font8-patterns)
  (fixup-font-patterns patterns character-height)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(off fast-integers)
