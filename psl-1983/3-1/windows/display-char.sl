%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% DISPLAY-CHAR.SL
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        8 October 1982
%
% This file defines MACROS.  Load it at Compile Time!
%
% Display characters are ASCII characters that are "tagged" with display
% enhancement bits.  They are used by the Windows package.  This file defines
% macros for creating and manipulating display characters.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(load fast-int)

(put 'INVERSE-VIDEO 'enhancement-bits 1)
(put 'BLINK 'enhancement-bits 2)
(put 'UNDERLINE 'enhancement-bits 4)
(put 'INTENSIFY 'enhancement-bits 8)

(dm dc-make-enhancement-mask (form)
  (setf form (cdr form))
  (let ((mask 0) bits)
    (for (in keyword form)
         (do (if (setf bits (get keyword 'enhancement-bits))
		 (setf mask (| mask bits))
		 (StdError (BldMsg "Undefined enhancement: %p" keyword))
		 )))
    (<< mask 8)))

(defmacro dc-make-font-mask (font-number)
  `(<< ,font-number 12))

(defmacro display-character-cons (enhancement-mask font-mask char-code)
  `(| (| ,enhancement-mask ,font-mask) ,char-code))

(defmacro dc-enhancement-mask (dc)
  `(& ,dc 16#F00))

(defmacro dc-enhancement-index (dc)
  % Use this to index an array.
  `(& (>> ,dc 8) 16#F))

(defmacro dc-font-mask (dc)
  `(& ,dc 16#F000))

(defmacro dc-font-number (dc)
  `(>> ,dc 12))

(defmacro dc-character-code (dc)
  `(& ,dc 16#FF))
