%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Nmode-Attributes.SL - macros for NMODE parsing primitives
% [This file used to be Parsing-Attributes.SL]
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        22 November 1982
%
% This file defines Macros!  Load it at compile-time!
%
% See the document NMODE-PARSING.TXT for a description of the parsing strategy.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects fast-int))

% Internal Constants:

% Type attributes:
% Exactly one of these should always be on.

(defconst     OPENER-BITS 2#000000001) % part of an opening "bracket"
(defconst     CLOSER-BITS 2#000000010) % part of a closing "bracket"
(defconst       ATOM-BITS 2#000000100) % part of an "atom"
(defconst     BLANKS-BITS 2#000001000) % part of a "blank region"
(defconst    COMMENT-BITS 2#000010000) % part of a comment

% Secondary attributes:
% Zero or more of these may be on.

(defconst     PREFIX-BITS 2#000100000) % a subclass of opening bracket

% Position attributes:
% One or two of these should always be on.

(defconst      FIRST-BITS 2#001000000) % the first character of an item
(defconst     MIDDLE-BITS 2#010000000) % neither first nor last
(defconst       LAST-BITS 2#100000000) % the last character of an item

% Masks:
(defconst       POSITION-BITS #.(| (const FIRST-BITS) 
				   (| (const MIDDLE-BITS) (const LAST-BITS))))
(defconst        BRACKET-BITS #.(| (const OPENER-BITS) (const CLOSER-BITS)))
(defconst     WHITESPACE-BITS #.(| (const BLANKS-BITS) (const COMMENT-BITS)))

(defconst      NOT-SPACE-BITS #.(| (const BRACKET-BITS) (const ATOM-BITS)))
(defconst   PRIMARY-TYPE-BITS #.(| (const NOT-SPACE-BITS)
				   (const WHITESPACE-BITS)))
(defconst SECONDARY-TYPE-BITS #.(const PREFIX-BITS))
(defconst           TYPE-BITS #.(| (const PRIMARY-TYPE-BITS)
				   (const SECONDARY-TYPE-BITS)))

(de parse-character-attributes (attribute-list)
  % Given a list of attribute names, return an integer containing
  % all of their bits.

  (let ((bits 0))
    (for (in attribute-name attribute-list)
	 (do
	  (selectq attribute-name
	    (OPENER      (setf bits (| bits (const OPENER-BITS))))
	    (CLOSER      (setf bits (| bits (const CLOSER-BITS))))
	    (BRACKET     (setf bits (| bits (const BRACKET-BITS))))
	    (ATOM        (setf bits (| bits (const ATOM-BITS))))
	    (BLANKS      (setf bits (| bits (const BLANKS-BITS))))
	    (COMMENT     (setf bits (| bits (const COMMENT-BITS))))
	    (WHITESPACE  (setf bits (| bits (const WHITESPACE-BITS))))
	    (NOT-SPACE   (setf bits (| bits (const NOT-SPACE-BITS))))
	    (PREFIX      (setf bits (| bits (const PREFIX-BITS))))
	    (FIRST       (setf bits (| bits (const FIRST-BITS))))
	    (MIDDLE      (setf bits (| bits (const MIDDLE-BITS))))
	    (LAST        (setf bits (| bits (const LAST-BITS))))
	    (t (StdError
		(BldMsg "Invalid character attribute: %p" attribute-name)))
	    )))
    bits
    ))

(de unparse-character-attributes (bits)
  % Return a list of attribute names.

  (let ((l ()))
    (if (~= 0 (& bits (const OPENER-BITS))) (setf l (cons 'OPENER l)))
    (if (~= 0 (& bits (const CLOSER-BITS))) (setf l (cons 'CLOSER l)))
    (if (~= 0 (& bits (const ATOM-BITS))) (setf l (cons 'ATOM l)))
    (if (~= 0 (& bits (const BLANKS-BITS))) (setf l (cons 'BLANKS l)))
    (if (~= 0 (& bits (const COMMENT-BITS))) (setf l (cons 'COMMENT l)))
    (if (~= 0 (& bits (const PREFIX-BITS))) (setf l (cons 'PREFIX l)))
    (if (~= 0 (& bits (const LAST-BITS))) (setf l (cons 'LAST l)))
    (if (~= 0 (& bits (const MIDDLE-BITS))) (setf l (cons 'MIDDLE l)))
    (if (~= 0 (& bits (const FIRST-BITS))) (setf l (cons 'FIRST l)))
    l
    ))

(de decode-character-attribute-type (bits)
  % Return a primary type attribute name or NIL.

  (cond
   ((~= 0 (& bits (const OPENER-BITS))) 'OPENER)
   ((~= 0 (& bits (const CLOSER-BITS))) 'CLOSER)
   ((~= 0 (& bits (const ATOM-BITS))) 'ATOM)
   ((~= 0 (& bits (const BLANKS-BITS))) 'BLANKS)
   ((~= 0 (& bits (const COMMENT-BITS))) 'COMMENT)
   (t NIL)
   ))

(de fix-attribute-bits (bits)
  (if (= (& bits (const POSITION-BITS)) 0)
    % No position specified? Then any position will do.
    (setf bits (| bits (const POSITION-BITS))))
  (if (= (& bits (const TYPE-BITS)) 0)
    % No type specified? Then any type will do.
    (setf bits (| bits (const TYPE-BITS))))
  bits
  )

(defmacro attributes attributes-list
  (parse-character-attributes attributes-list)
  )

(defmacro test-attributes attributes-list
  (fix-attribute-bits (parse-character-attributes attributes-list))
  )
