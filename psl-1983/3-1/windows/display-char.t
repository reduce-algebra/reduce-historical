             NOTES ON THE DISPLAY CHARACTER DATATYPE
                           Cris Perdue
                            10/11/82
                     File: PW:DISPLAY-CHAR.T
               -----------------------------------

This module provides a set of macros for manipulating
"display-character" objects.  These objects are represented to
LISP as integers, but are dealt with as a separate type of
object.

(DC-MAKE-ENHANCEMENT-MASK KEYWORD . . . )	Macro

This macro generates a specific enhancement mask object.  The
keywords are unevaluated identifiers.  At present, the possible
keywords are INVERSE-VIDEO, BLINK, UNDERLINE, and INTENSIFY,
which should be meaningful with respect to HP terminals.

(DC-MAKE-FONT-MASK FONT-NUMBER)		Macro

This makes a font mask object, given a font number.  Font numbers
have no definition yet, because we have no fonts.

(DISPLAY-CHARACTER-CONS ENHANCEMENT-MASK FONT-MASK CHAR-CODE)	Macro

This macro generates a display character object, given an
enhancement mask, a font mask, and a character code.  The mask
objects' purpose in life is to be used as arguments to this
function and to be compared against each other.

(DC-ENHANCEMENT-MASK DC)		Macro

Extracts the enhancement mask from a display character.

(DC-ENHANCEMENT-INDEX DC)		Macro

There are a finite number of different combinations of display
enhancements that are possible for a display-character.  This
macro returns an integer in the range from 0 that uniquely
identifies the combination of enhancements in effect for this
display-character.  There should probably be a symbolic constant
giving the maximum value for the identifying integer.  With N
different enhancements, the value turns out to be 2 raised to the
Nth power, minus 1.

(DC-FONT-MASK DC)			Macro

Extracts the font mask from a display character.

(DC-FONT-NUMBER DC)			Macro

Obtains the font number from a display character.

(DC-CHARACTER-CODE DC)			Macro

Obtains the character code from a display character object.
