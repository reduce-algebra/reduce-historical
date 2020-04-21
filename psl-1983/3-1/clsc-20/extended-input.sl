%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Extended-Input.SL - 9-bit terminal input (for 7 or 8 bit terminals)
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        31 August 1982
% Revised:     11 April 1983
%
% 11-Apr-83 Alan Snyder
%  Change "obsolete" #\BS to #\BackSpace.
% 17-Feb-83 Alan Snyder
%  Added PUSH-BACK-INPUT-CHARACTER function.  Revise mapping so that
%  bit prefix characters are recognized after mapping.
% 22-Dec-82 Jeffrey Soreff
%  Added PUSH-BACK-EXTENDED-CHARACTER function.
%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load extended-char fast-int fast-vectors))

% Global variables:

(fluid '(nmode-meta-bit-prefix-character
	 nmode-control-bit-prefix-character
	 nmode-control-meta-bit-prefix-character))

(setf nmode-meta-bit-prefix-character (x-char C-!\))
(setf nmode-control-bit-prefix-character (x-char C-^))
(setf nmode-control-meta-bit-prefix-character (x-char C-Z))

% Internal static variables:

(fluid '(nmode-terminal-map nmode-lookahead-extended-char nmode-lookahead-char))
(setf nmode-lookahead-extended-char nil)
(setf nmode-lookahead-char nil)

(de nmode-initialize-extended-input ()
  (setf nmode-terminal-map (MkVect 255))

  % Most input characters map to themselves.
  (for (from i 0 255)
       (do (vector-store nmode-terminal-map i i)))

  % Some ASCII control character map to Extended Control characters.
  % Exceptions: BACKSPACE, TAB, RETURN, LINEFEED, ESCAPE
  (for (from i 0 31)
       (unless (member i '#.(list #\BackSpace #\Tab #\CR #\LF #\ESC)))
       (do (let ((mch (X-Set-Control (+ i 64))))
	     (vector-store nmode-terminal-map i mch)
	     (vector-store nmode-terminal-map (+ i 128) (+ mch 128))
	     )))
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de input-extended-character ()
  (if nmode-lookahead-extended-char
    (prog1 nmode-lookahead-extended-char
	   (setf nmode-lookahead-extended-char nil))
    (input-direct-extended-character)))

(de push-back-extended-character (ch)
  (setf nmode-lookahead-extended-char ch))

(de input-direct-extended-character ()
  % Read an extended character from the terminal.
  % Recognize and interpret bit-prefix characters.

  (let* ((ch (input-terminal-character)))
    (cond
      ((= ch nmode-meta-bit-prefix-character)
	(nmode-append-separated-prompt "M-")
	(setf ch (input-terminal-character))
	(nmode-complete-prompt (x-char-name (x-unmeta ch)))
	(x-set-meta ch)
	)
      ((= ch nmode-control-bit-prefix-character)
	(nmode-append-separated-prompt "C-")
	(setf ch (input-terminal-character))
	(nmode-complete-prompt (x-char-name (x-uncontrol ch)))
	(x-set-control ch)
	)
      ((= ch nmode-control-meta-bit-prefix-character)
	(nmode-append-separated-prompt "C-M-")
	(setf ch (input-terminal-character))
	(nmode-complete-prompt (x-char-name (x-base ch)))
	(x-set-meta (x-set-control ch))
	)
      (t ch)
      )))

(de push-back-input-character (ch)
  (setf nmode-lookahead-char ch)
  )

(de input-terminal-character ()
  % Read an extended character from the terminal.  Perform mapping from 8-bit
  % to 9-bit characters.  Do not interpret bit prefix characters.

  (if nmode-lookahead-char
    (prog1 nmode-lookahead-char (setf nmode-lookahead-char nil))
    (vector-fetch nmode-terminal-map (input-direct-terminal-character))
    ))
