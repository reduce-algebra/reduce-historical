%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Extender-Char.SL - 9-bit terminal input characters
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        31 August 1982
%
% Changes:
% 10/15/82: added M-X macro, for convenience
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Note: this file defines MACROS, so you may need to load it at compile-time.
% Note: this file loads FAST-INT.

(load fast-int common strings)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extended Character Manipulation Functions (or Macros)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(ds X-Base (chr)
  % Return the base character corresponding to CHR.  In other words, clear the
  % Meta and Control bits.
  (& chr 2#001111111))

(ds X-Zero-Base (chr)
  % Return the given character with its base code set to 0.
  (& chr 2#110000000))

(ds X-UnMeta (chr)
  % Turn off the Meta bit in the given character.
  (& chr 2#101111111))

(ds X-UnControl (chr)
  % Turn off the Control bit in the given character.
  (& chr 2#011111111))

(ds X-Meta? (chr)
  % Does CHR have the Meta bit set?
  (not (= (& chr 2#010000000) 0)))

(ds X-Control? (chr)
  % Does CHR have the Control bit set?
  (not (= (& chr 2#100000000) 0)))

(ds X-Set-Meta (chr)
  % Set the Meta bit in CHR.
  (| chr 2#010000000))

(ds X-Set-Control (chr)
  % Set the Control bit in CHR.
  (| chr 2#100000000))

% This version of "UpperCaseP" handles extended characters.
(de X-UpperCaseP (chr)
  (UpperCaseP (X-Base chr)))

% This version of "LowerCaseP" handles extended characters.
(de X-LowerCaseP (chr)
  (LowerCaseP (X-Base chr)))

(de X-Char-DownCase (chr)
  (let ((bits (X-Zero-Base chr))
	(base (X-Base chr))
	)
    (| bits (Char-DownCase base))))

(de X-Char-UpCase (chr)
  (let ((bits (X-Zero-Base chr))
	(base (X-Base chr))
	)
    (| bits (Char-UpCase base))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extended Character Creation Macro
%
% Examples of legal uses:
% (x-char a) => A
% (x-char lower a) => a
% (x-char control a) => C-A
% (x-char c-a) => C-A
% (x-char ^A) => (ascii control A - code 1)
% (x-char meta control TAB) => C-M-Tab
% (x-char control ^A) => C-^A (^A is ASCII code 1)
% (x-char C-M-^A) => C-M-^A (^A is ASCII code 1)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(dm X-Char (form)
  (Create-Extended-Character (cdr form)))

(de Create-Extended-Character (L)
  (let ((plist (gensym)))
    (for (in x L)
	 (do (cond ((IdP x) (X-Char-process-id x plist))
		   ((FixP x) (X-Char-process-fix x plist))
		   (t (put plist 'error T))
		   )))
    (let ((base (get plist 'base)))
      (if (or (get plist 'error) (null base))
        (StdError (BldMsg "Invalid X-CHAR: %p" (cons 'X-CHAR L))))
      (if (and (get plist 'Lower) (>= base #\A) (<= base #\Z))
        (setf base (+ base 2#100000)))
      (if (get plist 'Control)
        (setf base (X-Set-Control base)))
      (if (get plist 'Meta)
        (setf base (X-Set-Meta base)))
      base
      )))

(de X-char-process-id (id plist)
  (prog (temp id2)
    (cond ((eq id 'Meta) (put plist 'Meta T))
	  ((eq id 'Control) (put plist 'Control T))
	  ((eq id 'Lower) (put plist 'Lower T))
	  ((eq id 'Return) (put plist 'base 13))
	  ((< (setf temp (ID2Int id)) 128) (put plist 'base temp))
	  ((setf temp (get id 'CharConst)) (put plist 'base temp))
	  ((and (>= (size (setf temp (id2string id))) 2)
		(= (indx temp 1) #\-))
	   (setf id2 (intern (substring temp 2 (+ 1 (size temp)))))
	   (selectq (indx temp 0)
	     (#\M (put plist 'Meta T) (X-char-process-id id2 plist))
	     (#\C (put plist 'Control T) (X-char-process-id id2 plist))
	     (t (put plist 'error T))
	     ))
	  ((and (= (size temp) 1) (= (indx temp 0) #\^))
	   (put plist 'Ascii-Control T)
	   (put plist 'base (& (indx temp 1) 2#11111))
	   )
	  (t (put plist 'error T))
	  )))

(de X-Char-process-fix (x plist)
  (cond ((and (>= x 0) (<= x 9)) (put plist 'base (+ x #\0)))
	(t (put plist 'error T))
	))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% X-Chars
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Generate a list of character codes from a list of "character descriptors",
% which are argument lists to the X-CHAR macro.

(dm x-chars (chlist)
  (cons 'list
    (for (in x (cdr chlist))
         (collect (cons 'x-char (if (pairp x) x (list x)))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Printable names for extended characters:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(character-name-table))

% An association list of (character code .  name), used by x-char-name.

(setf character-name-table '(
  (8#0   . "Null")
  (8#7   . "Bell")
  (8#10  . "Backspace")
  (8#11  . "Tab")
  (8#12  . "Newline")
  (8#15  . "Return")
  (8#33  . "Escape")
  (8#40  . "Space")
  (8#177 . "Rubout")
  ))

(de x-char-name (ch)
  % Return a string giving the name for an extended character.

  (cond
    ((not (FixP ch)) (BldMsg "<%o>" ch))
    ((atsoc ch character-name-table) (cdr (atsoc ch character-name-table)))
    ((X-Control? ch) (string-concat "C-" (x-char-name (X-UnControl ch))))
    ((X-Meta? ch) (string-concat "M-" (x-char-name (X-UnMeta ch))))
    ((GraphicP ch) (string ch))
    ((and (>= ch 0) (< ch (char space)))
     (string-concat "^" (x-char-name (LXor ch 8#100))))
    (t (BldMsg "<%o>" ch))
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% M-X Macro
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmacro m-x (command-string)
  `(list (x-char M-X) ,command-string))
