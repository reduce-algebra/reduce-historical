%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 9836-Bitmap.SL - Terminal Interface for 9836 Bitmap Display
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        16 March 1983
%
% This code is adapted from 9836-COLOR.SL.  It assumes a contiguous bitmap
% memory, one bit per pixel, byte-aligned, with an integral number of bytes
% per scan row.
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

(defflavor 9836-bitmap
  (
   % The following parameters may be set at initialization:

   (device-address (+ 16#600000 (* 28 16#10000))) % address of device
   (plane device-address)	% address of bitmap
   (raster-width 512)		% must be a multiple of 8!
   (raster-height 392)
   (character-height 14)	% raster lines in each character
   (interline-spacing 0)	% raster lines between each text row
   (patterns font8-patterns)	% raster images of characters
   (display-on-function NIL)	% optional function to turn on display
   (display-off-function NIL)	% optional function to turn off display

   % the following variables are computed from the above:

   character-row-spacing	% number of raster lines per text row
   height			% number of rows of characters
   width			% number of columns of characters
   maxrow			% highest numbered row of characters
   maxcol			% highest numbered column of characters
   raster-area			% number of bits in display raster
   raster-area-words		% number of words in display raster
   bytes-per-row		% number of bytes per raster row
   bytes-per-character-row	% number of bytes per character row
   blank-pattern		% raster for blank character

   % State variables:

   (cursor-row 0)		% cursor position
   (cursor-column 0)		% cursor position
   (raw-mode NIL)
   (inverse-video? NIL)
   )
  ()
  (gettable-instance-variables height width maxrow maxcol raw-mode)
  (settable-instance-variables inverse-video?)
  (initable-instance-variables device-address plane raster-width
			       raster-height character-height
			       interline-spacing patterns
			       display-on-function display-off-function
			       )
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod (9836-bitmap get-character) ()
  (keyboard-input-character)
  )

(defmethod (9836-bitmap ring-bell) ()
  (ChannelWriteChar 1 #\Bell)
  )

(defmethod (9836-bitmap move-cursor) (row column)
  (=> self xor-cursor)
  (setf cursor-row row)
  (setf cursor-column column)
  (=> self xor-cursor)
  )

(defmethod (9836-bitmap xor-cursor) ()
  (when (and cursor-row cursor-column)
    (let ((byte-offset (* cursor-row bytes-per-character-row)))
      (setf byte-offset (+ byte-offset cursor-column))
      (for (from i 1 character-height)
	   (do
	    (putbyte plane byte-offset (~ (byte plane byte-offset)))
	    (setf byte-offset (+ byte-offset bytes-per-row))
	    )))))

(defmethod (9836-bitmap enter-raw-mode) ()
  (when (not raw-mode)
    % (EchoOff)
    % Enable Keypad?
    (=> self display-on)
    (setf raw-mode T)
    ))

(defmethod (9836-bitmap leave-raw-mode) ()
  (when raw-mode
    (setf raw-mode NIL)
    % Disable Keypad?
    % (EchoOn)
    ))

(defmethod (9836-bitmap display-on) ()
  (when display-on-function
    (apply display-on-function (list device-address))
    ))

(defmethod (9836-bitmap display-off) ()
  (when display-off-function
    (apply display-off-function (list device-address))
    ))

(defmethod (9836-bitmap erase) ()
  % This method should be invoked to initialize the screen to a known state.
  (=> self &fill-plane plane 0 raster-area-words)
  (setf cursor-column NIL)
  (setf cursor-row NIL)
  (=> self move-cursor 0 0)
  )

(defmethod (9836-bitmap &fill-plane) (address word-value count)
  (when (> count 0)
    (wputv address 0 word-value)
    (=> self &fill-plane (+ address 4) word-value (- count 1))
    ))

(defmethod (9836-bitmap clear-line) ()
  % Not implemented yet.
  )

(defmethod (9836-bitmap convert-character) (ch)
  (setq ch (& ch (display-character-cons
		  (dc-make-enhancement-mask INVERSE-VIDEO)
		  (dc-make-font-mask 0)
		  16#FF))) % 8 bits
  ch)

(defmethod (9836-bitmap normal-enhancement) ()
  (dc-make-enhancement-mask)
  )

(defmethod (9836-bitmap highlighted-enhancement) ()
  (dc-make-enhancement-mask INVERSE-VIDEO)
  )

(defmethod (9836-bitmap supported-enhancements) ()
  (dc-make-enhancement-mask INVERSE-VIDEO)
  )

(defmethod (9836-bitmap write-line) (row line)
  (for (from col 0 maxcol)
       (do (=> self write-char row col (vector-fetch line col)))
       ))

(defmethod (9836-bitmap write-char) (row column ch)
  (let* ((pattern (vector-fetch patterns (dc-character-code ch)))
	 (inverse-bit (& ch (dc-make-enhancement-mask INVERSE-VIDEO)))
	 (byte-offset (mul16 row bytes-per-character-row))
	 (address (+ plane (+ byte-offset column)))
	 (inverse? (xor (~= 0 inverse-bit) inverse-video?))
	 )
    (if (xor inverse? (and (= cursor-row row)
			   (= cursor-column column)))
      (write-inverted-char-raster pattern address bytes-per-row 14)
      (write-char-raster pattern address bytes-per-row 14)
      )))

(defmethod (9836-bitmap set-character-pattern) (ch pattern)
  % CH must be an ASCII code (0..255); pattern must be a vector of bytes or
  % NIL.

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

(defmethod (9836-bitmap init) (init-plist)
  (setf raster-area (* raster-width raster-height))
  (setf raster-area-words (/ raster-area 32))
  (setf character-row-spacing (+ character-height interline-spacing))
  (setf height (/ (+ raster-height interline-spacing) character-row-spacing))
  (setf width (/ raster-width 8))
  (setf maxrow (- height 1))
  (setf maxcol (- width 1))
  (setf bytes-per-row (/ raster-width 8))
  (setf bytes-per-character-row (* bytes-per-row character-row-spacing))
  (setf blank-pattern (make-vector character-height 0))
  (fixup-font-patterns patterns character-height)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Examples of bitmap devices:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de create-color-bitmap ()
  (create-color-bitmap-selectcode 28)
  )

(de create-color-bitmap-selectcode (select-code)
  (let ((device-address (+ 16#600000 (* select-code 16#10000))))
    (make-instance '9836-bitmap
		   'device-address device-address
		   'plane (+ device-address (* 2 32768))
		   'raster-width 512
		   'raster-height 392
		   'character-height 14
		   'interline-spacing 0
		   'patterns font8-patterns
		   'display-on-function #'color-display-on-function
		   'display-off-function #'color-display-off-function
		   )))

(de color-display-on-function (device-address)
  (let ((device-register-values [41 32 34 3 50 5 49 49 0 7 0 0 0 0 0 0 0 0]))
    (for (from i 0 17)
	 (do (putbyte device-address 16 i)
	     (putbyte device-address 18 (vector-fetch device-register-values i))
	     ))
    (putbyte device-address 1 -128)
    ))

(de color-display-off-function (device-address)
  (putbyte device-address 1 0)
  )

(de create-graphics-bitmap ()
  (let ((device-address 16#530000))
    (make-instance '9836-bitmap
		   'device-address device-address
		   'plane device-address
		   'raster-width 512
		   'raster-height 392
		   'character-height 14
		   'interline-spacing 0
		   'patterns font8-patterns
		   )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(off fast-integers)
