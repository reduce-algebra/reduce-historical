(*
"% COMMON-PREDICATES.SL - Predicates used for Anyreg and C-macro expansion
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        21 December 1981
% Copyright (c) 1981 University of Utah
%")

(fluid '(EntryPoints!*
	 !*FastLinks))

(global '(!*R2I))

(de RegisterP (Expression)
  (EqCar Expression 'REG))

(de AnyP (Expression)
  T)

(de TaggedLabel (X)
  (EqCar X 'Label))

(de EqTP (Expression)
  (equal Expression T))

(de MinusOneP (Expression)
  (equal Expression -1))

(de InternallyCallableP (X)		% only when writing a file
  (and (or !*WritingFaslFile (not (FUnBoundP 'AsmOut)))
       (or !*FastLinks
	   (and !*R2I (memq X EntryPoints!*))
	   (FlagP X 'InternalFunction)
	   (FlagP X 'FastLink))))

(de AddressConstantP (Expression)
  (or (atom Expression) (equal (car Expression) 'Immediate)))
