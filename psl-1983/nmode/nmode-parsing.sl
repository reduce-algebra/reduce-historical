%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% NMODE-Parsing.SL - NMODE parsing primitives
% [This file used to be Parsing-Functions.SL]
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        10 December 1982
% Revised:     6 January 1983
%
% This file defines Macros!  Load it at compile-time!
%
% This file defines the basic primitives used by NMODE functions to analyze
% source code.  See the document NMODE-PARSING.TXT for a description of the
% parsing strategy.
%
% 6-Jan-83 Alan Snyder
%   Use LOAD instead of FASLIN to get macros (for portability).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects fast-int fast-strings fast-vectors))
(BothTimes (load nmode-attributes))

% Global Variables:

(fluid '(nmode-current-parser))
(setf nmode-current-parser 'lisp-parse-line)

% Internal Static Variables:

(fluid '(nmode-parsed-line
         nmode-parsed-line-info
	 ))

(setf nmode-parsed-line NIL)
(setf nmode-parsed-line-info (make-vector 200 0))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% These are the exported functions:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmacro test-current-attributes attributes-list
  `(test-current-attributes-bits (test-attributes ,@attributes-list))
  )

(defmacro move-forward-to attributes-list
  `(move-forward-to-bits (test-attributes ,@attributes-list))
  )

(defmacro move-backward-to attributes-list
  `(move-backward-to-bits (test-attributes ,@attributes-list))
  )

(defmacro move-forward-within-line-to attributes-list
  `(move-forward-within-line-to-bits (test-attributes ,@attributes-list))
  )

(defmacro move-backward-within-line-to attributes-list
  `(move-backward-within-line-to-bits (test-attributes ,@attributes-list))
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% These are internal, non-primitive functions:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de test-current-attributes-bits (bits)
  (let* ((x (current-attributes))
	 (match-bits (& x bits))
	 )
    (and (~= 0 (& match-bits (const POSITION-BITS)))
	 (~= 0 (& match-bits (const TYPE-BITS)))
	 )))

(de move-forward-to-bits (bits)
  (move-forward-to-bits-until bits #'at-buffer-end?))

(de move-backward-to-bits (bits)
  (move-backward-to-bits-until bits #'at-buffer-start?))

(de move-forward-within-line-to-bits (bits)
  (move-forward-to-bits-until bits #'at-line-end?))

(de move-backward-within-line-to-bits (bits)
  (move-backward-to-bits-until bits #'at-line-start?))

(de move-forward-to-bits-until (bits stop-predicate)
  (let ((old-pos (buffer-get-position)))
    (while T
      (when (apply stop-predicate ()) (buffer-set-position old-pos) (exit NIL))
      (when (test-current-attributes-bits bits)
	(exit (decode-character-attribute-type (current-attributes))))
      (move-forward-character)
      )))

(de move-backward-to-bits-until (bits stop-predicate)
  (let ((old-pos (buffer-get-position)))
    (while T
      (when (test-current-attributes-bits bits)
	(exit (decode-character-attribute-type (current-attributes))))
      (when (apply stop-predicate ()) (buffer-set-position old-pos) (exit NIL))
      (move-backward-character)
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The (internal) primitive parsing function:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de current-attributes ()
  (let* ((str (current-line))
	 (len (string-length str))
	 (pos (current-char-pos))
	 )
    (if (>= pos len)
      (attributes FIRST LAST BLANKS)
      % Otherwise
      (when (not (eq nmode-parsed-line str))
	(setf nmode-parsed-line str)
	(if (< (vector-size nmode-parsed-line-info) len)
	  (setf nmode-parsed-line-info (make-vector len 0)))
	(apply nmode-current-parser
	       (list nmode-parsed-line nmode-parsed-line-info))
	)
      (vector-fetch nmode-parsed-line-info pos)
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Testing code:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load extended-char))
(de show-current-character ()
  (write-prompt
   (bldmsg "%l" (unparse-character-attributes (current-attributes)))))
%(set-text-command (x-char C-=) 'show-current-character)
