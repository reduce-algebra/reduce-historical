%
% NON-KL-COMP.SL - Patches to compiler for KI processor
% 
% Author:      Eric Benson
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        10 May 1982
% Copyright (c) 1982 University of Utah
%

% <PSL.COMP-20>NON-KL-COMP.SL.6, 13-Oct-82 13:39:27, Edit by BENSON
% Removed unnecessary patch of floating point arith for DMOVE

(setq system_list* (delete 'KL10 system_list*))_

(DefCMacro !*Alloc
	   ((ZeroP))
	   ((add (REG st) (lit (halfword ARGONE ARGONE)))
	    (jumpge (REG st) (Entry StackOverflow))))

(DefCMacro !*DeAlloc
	   ((ZeroP))
	   ((sub (REG st) (lit (halfword ARGONE ARGONE)))))

(ForEach X in '(Byte PutByte HalfWord PutHalfWord BitTable PutBitTable) do
  (RemProp X 'OpenCode)
  (RemProp X 'Destroys))

(RemProp 'AdjustStackPointer 'OpenFn)

(dm AdjustStackPointer (U)
  (list 'WPlus2
	(cadr U)
	(list 'WPlus2 (caddr U) (list 'WShift (caddr U) 18))))
