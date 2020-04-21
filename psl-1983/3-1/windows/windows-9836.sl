%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% WINDOWS-9836.SL - HP9836 Windows Stuff (intended only for HP9836 version)
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        20 January 1983
% Revised:     5 April 1983
%
% 5-Apr-83 Alan Snyder
%  Changes relating to keeping WINDOWS source and binary files in separate
%  directories.  Rename Shared-Screen to Shared-Physical-Screen, for
%  compatibility with other systems.
% 16-Mar-83 Alan Snyder
%  Add font8, LAP support.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(compiletime (load fast-strings fast-int))
(bothtimes (load strings common))

(fluid '(window-file-list window-source-prefix window-binary-prefix))

(if (or (unboundp 'window-source-prefix) (null window-source-prefix))
  (setf window-source-prefix "pw:"))

(if (or (unboundp 'window-binary-prefix) (null window-binary-prefix))
  (setf window-binary-prefix "pwb:"))

(de charsininputbuffer () (if (keyboard-input-available?) 1 0))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Stuff for Building WINDOWS:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de window-fixup-name (s) s)

(de window-load-all ()
  (for (in s window-file-list)
       (do (window-load s))
       ))

(de window-load (s)
  (window-faslin window-binary-prefix s)
  )

(de window-faslin (directory-name module-name)
  (setf module-name (window-fixup-name module-name))
  (setf module-name (string-concat module-name ".b"))
  (let ((object-name (string-concat directory-name module-name)))
    (if (filep object-name)
      (faslin object-name)
      (continuableerror 99
       (bldmsg "Unable to FASLIN %w" object-name)
       (list 'faslin object-name)
       ))))

(setf window-file-list
  (list
   "font8"
   "9836-alpha"
   "9836-color"
   "direct-physical-screen"
   "shared-physical-screen"
   "virtual-screen"
   ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LAP support for Window operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(lap '((*entry mul16 expr 2)
       (move!.l (reg 1) (reg t1))
       (move!.l (reg 2) (reg t2))
       (muls (reg t1) (reg t2))
       (movea!.l (reg t2) (reg 1))
       (rts)
       ))

(lap '((*entry write-char-raster expr 4)

       % Arguments are:
       % 1. the raster pattern (vector of integers)
       % 2. the initial screen address (address of top scan line)
       % 3. the row-size (number of bytes per row of screen)
       % 4. count (the number of scan lines in the pattern) (must be positive)

       (move!.l (reg 4) (reg t2)) % loop control
       (addq!.l 4 (reg 1)) % skip vector header
       (*lbl (label loop))
       (move!.l (autoincrement (reg 1)) (reg t1)) % read next row from pattern
       (move!.b (reg t1) (displacement (reg 2) 0)) % store in screen memory
       (adda!.l (reg 3) (reg 2)) % advance to next row of screen
       (subq!.l 1 (reg t2)) % decrement loop counter
       (bgt (label loop)) % loop if more bytes to copy
       (move!.l (reg nil) (reg 1)) % avoid returning bad pointer
       (rts)
       ))

(lap '((*entry write-inverted-char-raster expr 4)

       % Arguments are:
       % 1. the raster pattern (vector of integers)
       % 2. the initial screen address (address of top scan line)
       % 3. the row-size (number of bytes per row of screen)
       % 4. count (the number of scan lines in the pattern) (must be positive)

       (move!.l (reg 4) (reg t2)) % loop control
       (addq!.l 4 (reg 1)) % skip vector header
       (*lbl (label loop))
       (move!.l (autoincrement (reg 1)) (reg t1)) % read next row from pattern
       (not!.l (reg t1)) % complement the raster pattern
       (move!.b (reg t1) (displacement (reg 2) 0)) % store in screen memory
       (adda!.l (reg 3) (reg 2)) % advance to next row of screen
       (subq!.l 1 (reg t2)) % decrement loop counter
       (bgt (label loop)) % loop if more bytes to copy
       (move!.l (reg nil) (reg 1)) % avoid returning bad pointer
       (rts)
       ))
