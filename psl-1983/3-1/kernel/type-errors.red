%
% TYPE-ERRORS.RED - Error handlers for common type mismatches
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        15 September 1981
% Copyright (c) 1981 University of Utah
%

% Edit by Cris Perdue, 27 Jan 1983 1621-PST
% Added NonIOChannelError
%  <PSL.INTERP>TYPE-ERRORS.RED.6, 20-Jan-82 03:10:00, Edit by GRISS
%  Added NonWords Error

lisp procedure TypeError(Offender, Fn, Typ);
    StdError BldMsg("An attempt was made to do %p on %r, which is not %w",
						Fn, Offender,	      Typ);

lisp procedure UsageTypeError(Offender, Fn, Typ, Usage);
    StdError
	BldMsg("An attempt was made to use %r as %w in %p, where %w is needed",
					Offender, Usage, Fn,	Typ);

lisp procedure IndexError(Offender, Fn);
    UsageTypeError(Offender, Fn, "an integer", "an index");

lisp procedure NonPairError(Offender, Fn);
    TypeError(Offender, Fn, "a pair");

lisp procedure NonIDError(Offender, Fn);
    TypeError(Offender, Fn, "an identifier");

lisp procedure NonNumberError(Offender, Fn);
    TypeError(Offender, Fn, "a number");

lisp procedure NonIntegerError(Offender, Fn);
    TypeError(Offender, Fn, "an integer");

lisp procedure NonPositiveIntegerError(Offender, Fn);
    TypeError(Offender, Fn, "a non-negative integer");

lisp procedure NonCharacterError(Offender, Fn);
    TypeError(Offender, Fn, "a character");

lisp procedure NonStringError(Offender, Fn);
    TypeError(Offender, Fn, "a string");

lisp procedure NonVectorError(Offender, Fn);
    TypeError(Offender, Fn, "a vector");

lisp procedure NonWords(Offender, Fn);
    TypeError(Offender, Fn, "a words vector");

lisp procedure NonSequenceError(Offender, Fn);
    TypeError(Offender, Fn, "a sequence");

lisp procedure NonIOChannelError(Offender, Fn);
    TypeError(Offender, Fn, "a legal I/O channel");

END;
