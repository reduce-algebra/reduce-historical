% TEST-FUNCTION-PRIMITIVES Machine Independent for Test 5 and 6
%
% Author:      M. L. Griss
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        21 October 1982
% Copyright (c) 1982 University of Utah
%
% Based on P20:Function-Primitives.Red
%  <PSL.TESTS>P-FUNCTION-PRIMITIVES.RED.4,  2-Mar-83 11:46:30, Edit by KESSLER
%  Put in Dealloc's before jump and jcall (search rrk)

% Every ID has a "function cell".  It does not necessarily contain a legal
% Lisp item, and therefore should not be accessed directly by Lisp functions.
% In this implementation the function cell contains an instruction to be
% executed.  There are 3 possibilites for this instruction, for which the
% following predicates and updating functions exist:
%
%	FUnBoundP(ID) -- the function is not defined
%	FLambdaLinkP(ID) -- the function is interpreted
%	FCodeP(ID) -- the function is compiled
%
%	MakeFUnBound(ID) -- undefine the function
%	MakeFLambdaLink(ID) -- specify that the function is interpreted
%	MakeFCode(ID, CodePtr) -- specify that the function is compiled,
%				   and that the code resides at the address
%				   associated with CodePtr
%
%	GetFCodePointer(ID) -- returns the contents of the function cell as a
%				code pointer
%
% See the templates in XXX-ASM.RED:
%
%       DefinedFunctionCellFormat!*
%	UndefinedFunctionCellFormat!*


% These functions currently check that they have proper arguments, 
% but this may change since they are only used by functions that 
% have checked them already.

% Note that on some machines, SYMFNC(x) is entire SYMFNC cell.
%           on others it points into the cell, at the "address" part.
% 
% Fairly Portable versions, based on assumption that
%      Starts with OPCODE, probably !*JCALL
%      !*Jcall SymfncBase UndefinedFunction  in ShouldBeUndefined cell

% Needs the machine-dependent procedures in XXX-HEADER:

%    !%Store!-JCALL(CodeAddress,StoreAddress)
%        to Create a !*Jcall(CodeAddress) at StoreAddress

%    !%Copy!-Function!-Cell(From,to)
%        to copy appropriate # words or bytes of Function cell
on syslisp;

smacro procedure SymFncBase D;   % The Address of CELL, 
				 %  to which !*JCALL and !*CALL jump
  Symfnc + AddressingUnitsPerFunctionCell*D;


% Unbound Functions have a JCALL UndefinedFunction:
% in the function cell, installed by the template

syslsp procedure FUnBoundP Fn;       
% Check If undefn or Not
 If not IDP Fn then NonIdError(Fn,'FunboundP)
  else  if (SymFnc IdLoc ShouldBeUndefined eq SymFnc IdInf Fn)
   % Instead of SYMFNCBASE Idloc UndefinedFunction, since its
   % of course DEFINED, and has to agree with the KernelTime template
    then 'T else 'NIL;

syslsp procedure MakeFUnBound(D);
% Install the correct Bit Pattern in SYMFNC cell
 If not IDP D then NonIdError(D,'MakeFUnbound)
  else !%copy!-function!-cell(symfncbase Idloc ShouldBeUndefined,
			      symfncbase IdInf D);

syslsp procedure FLambdaLinkP fn;
 If not IDP Fn then NonIdError(Fn,'FunboundP)
  else  if (SymFnc IdLoc CompiledCallingInterpreted eq SymFnc(IdInf Fn))
  % This installed by MakeFlambdaLink
     then 'T else 'NIL;

syslsp procedure MakeFlambdaLink D;
% Install the correct Bit Pattern in SYMFNC cell
 If not IDP D then NonIdError(D,'MakeFUnbound)
  else !%store!-JCALL(symfnc Idloc CompiledCallingInterpreted,
                              Symfncbase IdInf D); % SetUp as above

syslsp procedure FcodeP Fn;          
% Check if Code or Interp
 If not IDP Fn then NonIdError(Fn,'FcodeP)
  else if FUnboundP Fn or FLambdaLinkP Fn then NIL else T;

syslsp procedure MakeFCode(U, CodePtr);
%  Make U a compiled function
 if IDP U then
	if CodeP CodePtr then
	<<!%Store!-JCALL(CodeInf Codeptr,
                         SymfncBase IdInf U);
	    NIL >>
    else NonIDError(U, 'MakeFCode);


syslsp procedure GetFCodePointer U;
%  Get code pointer for U
  if IDP U then if FCodeP U then MkCODE SymFnc U % do we want Fcodep check
                 else NIL
    else NonIDError(U, 'GetFCodePointer);
   %/Check that IS codeP?


% Code Calling Primitives

% See PI: P-APPLY-LAP.RED by BENSON
% See also Pxxx:APPLY-LAP.RED

Fluid '(CodePtr!* CodeForm!* CodeNarg!*);

LAP '((!*entry CodePrimitive expr 15)
%	Takes the code pointer stored in the fluid variable CodePtr!*
%	and jumps to its address, without disturbing any of the argument
%	registers.  This can be flagged 'InternalFunction for compilation
%	before this file is compiled or done as an 'OpenCode and 'ExitOpenCode
%	property for the compiler.
	(!*ALLOC 0)
	(!*MOVE (Fluid CodePtr!*) (reg t1))
        (!*FIELD (reg t1) (reg t1)    % get CodeINF
 		 (WConst InfStartingBit) (WConst InfBitLength))
% rrk - 03/02/83 If alloc did anything we need to get rid of it before the jump
        (!*Dealloc 0)
        (!*JUMP (memory (reg t1) (Wconst 0)))
	(!*EXIT 0)
);


LAP '((!*entry CompiledCallingInterpreted expr 15)
%	Called by some convention from the function cell of an ID which
%	has an interpreted function definition.  It should store the
%       Linkreg into
%       the fluid variable CodeForm!* without disturbing the argument
%	registers
%
%
      (!*ALLOC 0)
      (!*CALL SaveRegisters)     % !*CALL to avoid resetting LinkInfo
      (!*Move (reg LinkReg) (fluid CodeForm!*))
      (!*Move (reg NargReg) (fluid CodeNarg!*))
% rrk - 03/02/83 If alloc did anything we need to get rid of it before the jump
      (!*Dealloc 0)
      (!*JCALL CompiledCallingInterpretedAux)
      (!*Exit 0)
);


LAP '((!*entry FastApply expr 0)
%	Called with a functional form in (reg t1) and argument registers
%	loaded.  If it is a code pointer or an ID, the function address
%	associated with either should be jumped to.  If it is anything else
%	except a lambda form, an error should be signaled.  If it is a lambda
%	form, store (reg t1) in the fluid variable CodeForm!* and
%	(!*JCALL FastLambdaApply)
%	(FastLambdaApply may be flagged 'InternalFunction).
	(!*ALLOC 0)
	(!*MOVE (reg t1) (FLUID CodeForm!*))	% save input form
	(!*FIELD (reg t2) (reg t1)
		 (WConst TagStartingBit) (WConst TagBitLength))
	(!*FIELD (reg t1) (reg t1)
		 (WConst InfStartingBit) (WConst InfBitLength))
	(!*JUMPNOTEQ (Label NotAnID) (reg t2) (WConst ID))
        (!*MOVE  (reg t1) (reg LinkReg))    % Reset IDLOC name
                                            % NargReg is OK
   	(!*WTIMES2 (reg t1) (WConst AddressingUnitsPerFunctionCell))
% rrk 03/03/83
	(!*Dealloc 0)
	(!*JUMP (MEMORY (reg t1) (WArray SymFnc)))
NotAnID
	(!*JUMPNOTEQ (Label NotACodePointer) (reg t2) (WConst CODE))
% rrk 03/03/83
	(!*Dealloc 0)
	(!*JUMP (MEMORY (reg t1) (WConst 0)))
NotACodePointer
	(!*JUMPNOTEQ (Label IllegalFunctionalForm) (reg t2) (WConst PAIR))
	(!*MOVE (MEMORY (reg t1) (WConst 0)) (reg t2))
					% CAR with pair already untagged
	(!*JUMPNOTEQ (Label IllegalFunctionalForm) (reg t2) (QUOTE LAMBDA))
% rrk 03/03/83
	(!*Dealloc 0)
    % Note that t1 is INF of the PAIR
	(!*JCALL FastLambdaApply)               % CodeForm!*
						% Already Loaded
IllegalFunctionalForm
	(!*MOVE (QUOTE "Illegal functional form in Apply") (reg 1))
	(!*MOVE (FLUID CodeForm!*) (reg 2))
	(!*CALL List2)
% rrk 03/03/83
	(!*Dealloc 0)
	(!*JCALL StdError)
%	(!*EXIT 0) --> what is this!
);

Exported Warray CodeArgs[15];

syslsp procedure SaveRegisters(A1, A2, A3, A4, A5, 
% Duplicate in P-APPLY
			       A6, A7, A8, A9, A10,
			       A11, A12, A13, A14, A15);
<<  CodeArgs[14] := A15;
    CodeArgs[13] := A14;
    CodeArgs[12] := A13;
    CodeArgs[11] := A12;
    CodeArgs[10] := A11;
    CodeArgs[9]  := A10;
    CodeArgs[8]  := A9;
    CodeArgs[7]  := A8;
    CodeArgs[6]  := A7;
    CodeArgs[5]  := A6;
    CodeArgs[4]  := A5;
    CodeArgs[3]  := A4;
    CodeArgs[2]  := A3;
    CodeArgs[1]  := A2;
    CodeArgs[0]  := A1 >>;


LAP '((!*ENTRY UndefinedFunctionAux expr 0) 
%	Called by some convention from the function cell of an ID (probably
%	the same as CompiledCallingInterpreted) for an undefined function.
%	Should call Error with the ID as part of the error message.
      (!*ALLOC 0)	
      (!*CALL SaveRegisters)   % !*CALL so as not to change LinkInfo
                               % Was stored in UndefnCode!* UndefnNarg!*
% rrk 03/03/83
      (!*Dealloc 0)
      (!*JCALL UndefinedFunctionAuxAux)
%     (!*EXIT 0)
);

off syslisp;

  End;
