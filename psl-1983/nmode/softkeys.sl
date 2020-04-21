%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% SoftKeys.SL - NMODE SoftKeys
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        28 January 1983
%
% This implementation of softkeys is intended primarily for the HP9836
% implementation.  It recognizes the escape-sequence Esc-/, followed by
% a single character, as instructing NMODE to execute the softkey
% corresponding to that character.  In the HP9836 implementation,
% we can cause the keys K0-K9 to send the appropriate escape sequence.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load fast-int fast-strings fast-vectors display-char))

% Global variables defined here:

(fluid '(nmode-softkey-label-screen
	 nmode-softkey-label-screen-height % number of rows of keys
	 nmode-softkey-label-screen-width % number of keys per row
	 ))

% Internal static variables (don't use elsewhere!):

(fluid '(nmode-softkey-defs	% vector of softkey definitions (see below)
	 nmode-softkey-labels	% vector of softkey label strings
	 nmode-softkey-label-width	% number of characters wide
	 nmode-softkey-label-count	% number of displayed labels
	 ))

(when (or (unboundp 'nmode-softkey-defs) (null nmode-softkey-defs))
  (setf nmode-softkey-label-screen NIL)
  (setf nmode-softkey-label-screen-height 0)
  (setf nmode-softkey-label-screen-width 0)
  (setf nmode-softkey-defs (make-vector 40 NIL))
  (setf nmode-softkey-labels (make-vector 40 NIL))
  (setf nmode-softkey-label-width 0)
  (setf nmode-softkey-label-count 0)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-define-softkey (n fcn label-string)
  % N should be a softkey number.  FCN should be a function ID, a string,
  % or NIL.  Define softkey #n to run the specified function, execute the
  % specified string (as if typed), or be undefined, respectively.
  % LABEL-STRING should be a string or NIL.  The string will be centered.

  (if (and (valid-softkey-number? n)
	   (or (null fcn) (idp fcn) (stringp fcn))
	   (or (null label-string) (stringp label-string))
	   )
    (progn
     (vector-store nmode-softkey-defs n fcn)
     (vector-store nmode-softkey-labels n label-string)
     (nmode-write-softkey-label n)
     )
    (nmode-error "Invalid arguments to Define Softkey")
    ))

(de valid-softkey-number? (n)
  (and (fixp n) (>= n 0) (<= n (vector-upper-bound nmode-softkey-defs)))
  )

(de softkey-char-to-number (ch)
  (- (char-code ch) #/0))

(de softkey-number-to-char (n)
  (+ n #/0))

(de nmode-execute-softkey (n)
  % Execute softkey #n.

  (if (valid-softkey-number? n)
    (let ((fcn (vector-fetch nmode-softkey-defs n)))
      (cond ((null fcn)
	     (nmode-error (bldmsg "Softkey %w is undefined." n)))
	    ((stringp fcn)
	     (nmode-execute-string fcn))
	    ((idp fcn)
	     (apply fcn ()))
	    (t
	     (nmode-error (bldmsg "Softkey %w has a bad definition." n)))
	    ))
    (nmode-error (bldmsg "Invalid Softkey specified."))
    ))

(de execute-softkey-command (n)
  (nmode-set-delayed-prompt "Execute Softkey: ")
  (let ((ch (input-direct-terminal-character)))
    (nmode-execute-softkey (softkey-char-to-number ch))
    ))

(de nmode-setup-softkey-label-screen (sps)
  % If the requested size of the softkey label screen is nonzero, then
  % create a virtual screen of that size on the given shared screen.
  % The requested size is obtained from global variables.

  (setf nmode-softkey-label-width 0)
  (setf nmode-softkey-label-count 0)
  (let ((height nmode-softkey-label-screen-height)
	(width nmode-softkey-label-screen-width)
	(screen-height (=> sps height))
	(screen-width (=> sps width))
	)
    (setf nmode-softkey-label-screen
      (when (and (> height 0) (> width 0) (> screen-width (* 2 width))
		 (>= screen-height height)
		 )
	(let ((s (make-instance 'virtual-screen 
				'screen sps
				'height height
				'width screen-width
				'row-origin (- screen-height height)
				'column-origin 0
				)))
	  (setf nmode-softkey-label-width (/ screen-width width))
	  (setf nmode-softkey-label-count (* width height))
	  (=> s set-default-enhancement (=> sps highlighted-enhancement))
	  s
	  )))
    (when nmode-softkey-label-screen
      (for (from i 0 (- nmode-softkey-label-count 1))
	   (do (nmode-write-softkey-label i)))
      (=> nmode-softkey-label-screen expose)
      )
    ))

(de nmode-write-softkey-label (n)
  (when (and nmode-softkey-label-screen
	     (>= n 0)
	     (< n nmode-softkey-label-count)
	     )
    (let* ((row (/ n nmode-softkey-label-screen-width))
	   (lcol (// n nmode-softkey-label-screen-width))
	   (col (* lcol nmode-softkey-label-width))
	   (enhancement (if (xor (= (// row 2) 0) (= (// lcol 2) 0))
			  (dc-make-enhancement-mask INVERSE-VIDEO INTENSIFY)
			  (dc-make-enhancement-mask INVERSE-VIDEO)
			  ))
	   (label (vector-fetch nmode-softkey-labels n))
	   (bound (if label (string-upper-bound label) -1))
	   (padding (/ (- nmode-softkey-label-width (+ bound 1)) 2))
	   )
      (=> nmode-softkey-label-screen set-default-enhancement enhancement)
      (if (< padding 0) (setf padding 0))
      (for (from i 1 padding)
	   (do (=> nmode-softkey-label-screen write #\space row col)
	       (setf col (+ col 1))
	       ))
      (for (from i 0 (- (- nmode-softkey-label-width padding) 1))
	   (do (let ((ch (if (<= i bound)
			   (string-fetch label i)
			   #\space
			   )))
		 (=> nmode-softkey-label-screen write ch row (+ col i))
		 )))
      )))
