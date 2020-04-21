%
% BINDING.RED - Primitives to support Lambda binding
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        18 August 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL>BINDING.RED.2, 21-Dec-82 15:57:06, Edit by BENSON
%  Added call to %clear-catch-stack in ClearBindings

% Support for binding in compiled code is in FAST-BINDER.RED

on SysLisp;

internal WConst BndStkSize = 2000;

internal WArray BndStk[BndStkSize];

% Only these WVars, which contain addresses rather than indexes, will be
% used to access the binding stack

exported WVar BndStkLowerBound = &BndStk[0],
	      BndStkUpperBound = &BndStk[BndStkSize-1],
	      BndStkPtr = &BndStk[0];

% Only the macros BndStkID, BndStkVal and AdjustBndStkPtr will be used
% to access or modify the binding stack and pointer.

syslsp procedure BStackOverflow();
<<  ChannelPrin2(LispVar ErrOUT!*,
		 "***** Binding stack overflow, restarting...");
    ChannelWriteChar(LispVar ErrOUT!*,
		     char EOL);
    Reset() >>;

syslsp procedure BStackUnderflow();
<<  ChannelPrin2(LispVar ErrOUT!*,
		 "***** Binding stack underflow, restarting...");
    ChannelWriteChar(LispVar ErrOUT!*,
		     char EOL);
    Reset() >>;

syslsp procedure CaptureEnvironment();	 %. Save bindings to be restored
    BndStkPtr;

syslsp procedure RestoreEnvironment Ptr;	%. Restore old bindings
<<  if Ptr < BndStkLowerBound then BStackUnderflow()
    else while BndStkPtr > Ptr do
    <<  SymVal BndStkID BndStkPtr := BndStkVal BndStkPtr;
	BndStkPtr := AdjustBndStkPtr(BndStkPtr, -1) >> >>;

syslsp procedure ClearBindings();	 %. Restore bindings to top level
<<  RestoreEnvironment BndStkLowerBound;
    !%clear!-catch!-stack() >>;

syslsp procedure UnBindN N;		%. Support for Lambda and Prog interp
    RestoreEnvironment AdjustBndStkPtr(BndStkPtr, -IntInf N);

syslsp procedure LBind1(IDName, ValueToBind);	%. Support for Lambda
    if not IDP IDName then
	NonIDError(IDName, "binding")
    else if null IDName or IDName eq 'T then
	StdError '"T and NIL cannot be rebound"
    else
    <<  BndStkPtr := AdjustBndStkPtr(BndStkPtr, 1);
	if BndStkPtr > BndStkUpperBound then BStackOverflow()
	else
	<<  IDName := IDInf IDName;
	    BndStkID BndStkPtr := IDName;
	    BndStkVal BndStkPtr := SymVal IDName;
	    SymVal IDName := ValueToBind >> >>;

syslsp procedure PBind1 IDName;		%. Support for PROG
    LBind1(IDName, NIL);

off SysLisp;

END;
