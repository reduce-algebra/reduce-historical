%
% 20-INTERRUPT.RED  -- Crude Interrupt Handler for DEC-20
% Author:      M. L. Griss  and D. Morrison
%              Utah Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        20 May 1981
% Copyright (c) University of Utah

% It is assumed that the system dependent portion of an implementation will
%supply the following 3 functions:
%
%   InitializeInterrupts
%   EnableInterrupts
%   DisableInterrupts
%   DismissInterrupt
%
% While these are machine dependent, the interrupt handlers themselves are
% are expected to generally be machine independent, simply calling
% DismissInterrupt when done.  The assignment of terminal-initiated interrupts
% to keys is machine dependent.

imports '(Addr2ID);			% for code-address-to-symbol

on Syslisp;

%internal WARRAY InterruptLevelTable[2], 
%                InterruptPCStorage[2],
%                InterruptChannelTable[35];

FLUID '(InterruptLevelTable
	LoadAverageStore
        InterruptPCStorage
        InterruptChannelTable
);

compiletime << WCONST !.FHSLF=8#400000;>>;

if FUnBoundP 'XJsysError then <<
syslsp procedure XJsysError();		% autoloading stub
<<  Load JSYS;
    Apply(function XJsysError, '()) >>;
>>;

syslsp procedure InitializeInterrupts();
% Initializes interrupt handlers for both machine- and terminal-initiated
% interrupts.  Most cases should dispatch to machine-independent handlers.
% Leaves the interrupt system enabled.
% In this Tops-20 (machine-code) version we currently handle:
%   just playing, for now
begin
  (LispVar InterruptLevelTable):=GtWarray 3;
  (LispVar InterruptPCStorage):=GtWarray 3;
  (LispVar InterruptChannelTable):=GtWarray 36;
  (LispVar LoadAverageStore) := MkString(4, char BLANK);
  ClearInterrupts();

  % set up interrupt tables -- see Monitor Calls Manual for details
  For i := 0:35 do             %/ Some bug, wiped out next one when after
    (LispVar InterruptChannelTable)[i]:=0;

  for i := 0:2 do
      (LispVar InterruptLevelTable)[i]:=(LispVar InterruptPCStorage) + i;

  % Terminal Interupts (Procedure on channel/level)
  % Note LEVEL is 1,2,3
  PutInterrupt(0,1,'DoControlG);
  PutInterrupt(1,1,'SaveAndCallControlT);	% control T not working yet
  PutInterrupt(2,1,'SaveAndBreak);
  % special channels
  PutInterrupt(6,1,'ArithOverflow);
  PutInterrupt(7,1,'FloatArithOverflow);
  PutInterrupt(9,1,'PushDownOverflow);

  % Now Install tables
  Xjsys0(!.FHSLF,
     XWD((LispVar InterruptLevelTable),
       (LispVar InterruptChannelTable)),0,0,const jsSIR);
  EnableInterrupts();
  ActivateChannel(0);
  ActivateChannel(1);
  ActivateChannel(2);
  ActivateChannel(6);
  ActivateChannel(7);
  ActivateChannel(9);
  PutTerminalInterrupt(7,0); % Char CNTRL-G on 0
  PutTerminalInterrupt(4,0); % Char CNTRL-D on 2
  PutTerminalInterrupt(20,1); % Char cntrl-t on 1, not working yet
  PutTerminalInterrupt(0,2); % Char BREAK on 2
  PutTerminalInterrupt(2,2); % Char cntrl-B on 2
  
  ClearInterrupts(); 
end;

syslsp procedure SetContinueAddress(Level,Address);
begin scalar x;
 x:=(LispVar InterruptLevelTable)[Level-1];
 x[0]:=address;
 end;

% FunctionCellLocation is used by LAP

off Syslisp;

fluid '(!*WritingFaslFile);

lisp procedure SetContinueFunction(Level,FunctionName);
begin scalar !*WritingFaslFile;
    SetContinueAddress(Level, FunctionCellLocation FunctionName);
end;

lisp procedure PutInterrupt(Channel,Level,ActionId);
begin scalar !*WritingFaslFile;
    WPutV(InterruptChannelTable,
	  Channel,
	  XWD(Level, FunctionCellLocation ActionId));
end;

on Syslisp;

syslsp procedure XWD(a,b);
 Lor(Lsh(a,18),b);

syslsp procedure PutTerminalInterrupt(CntrlChar,Channel);
  Xjsys0(XWD(CntrlChar,Channel),0,0,0,const jsATI);

syslsp procedure RemoveTerminalInterrupt(CntrlChar,Channel);
  Xjsys0(XWD(CntrlChar,Channel),0,0,0,const jsDTI);

syslsp procedure ReadTerminalWord;
  Xjsys1(0,0,0,0,Const jsRTIW);

syslsp procedure SetTerminalWordBit(n);
 <<XJsys0(Lor(ReadTerminalLWord(),Dec20Bit n),0,0,const jsSTIW);
   ReadTerminalWord()>>;

syslsp procedure SetTerminalWord(MSK);
 <<Xjsys0(Lor(ReadTerminalWord(),MSK),0,0,0,const jsSTIW);
   ReadTerminalWord()>>;

syslsp procedure ClearInterrupts;
  Xjsys0(0,0,0,0,const jsCIS); % clear any pending interrupts

syslsp procedure SignalChannel n; %. Test on channel n
  Xjsys0(!.FHSLF,Dec20Bit n,0,0,const jsIIC);

syslsp procedure EnableInterrupts;
 Xjsys0(!.FHSLF,0,0,0,const jsEIR);

syslsp procedure DisableInterrupts;
 Xjsys0(!.FHSLF,0,0,0,const jsDIR);

syslsp procedure ActivateChannel(n); %. Inform OS of channel
 Xjsys0(!.FHSLF,Dec20Bit n,0,0,const jsAIC);

syslsp procedure DeActivateChannel(n); %. Inform OS of channel
 Xjsys0(!.FHSLF,Dec20Bit n,0,0,const jsDIC);

syslsp procedure Dec20Bit n; %. Bits [0 to 35]
  Dec20Fld(1,35-n);

syslsp procedure Dec20Fld(x,y);
   LSH(x,y);

syslsp procedure DismissInterrupt;
% Warning: an interrupt handler should not attempt to resume if may have
% caused a garbage collection.  
Xjsys0(0,0,0,0,const jsDEBRK);


% ----- Some default handlers ----------

syslsp procedure DoControlG;
<<  ClearTerminalInputBuffer();	 % CFIBF
    ChannelWriteChar(LispVAR StdOUT!*, Char BELL);
    ErrorPrintF "*** Restarting";
    SetContinueFunction(1,'Reset);
    DismissInterrupt()>>;

syslsp procedure ClearTerminalInputBuffer();
  Xjsys0(8#100,0,0,0,const jsCFIBF);

syslsp procedure ArithOverflow;
 <<SetContinueFunction(1,'ArithOverFlowError);
   DismissInterrupt()>>;

syslsp procedure ArithOverFlowError;
   StdError('"Integer overflow");

syslsp procedure FloatArithOverflow;
 <<SetContinueFunction(1,'FloatArithOverFlowError);
   DismissInterrupt()>>;

syslsp procedure FloatArithOverFlowError;
    StdError('"Floating point overflow");

lap '((!*entry PushDownOverflow expr 0)
	(sub (reg st) (lit (halfword 1000 1000)))	% move the stack back
	(!*MOVE (WConst 1) (REG 1))
	(movei 2 ErrorAddress)
	(!*CALL SetContinueAddress)
	(!*JCALL DismissInterrupt)
ErrorAddress
	(!*MOVE '"Stack overflow" (reg 1))
	(!*JCALL StdError)		% normal error
);

lap '((!*entry FindLoadAverage expr 0)
	(move 1 (lit (fullword 8#000014000014)))	% 1 min avg, .systa
	(getab)
	(!*EXIT 0)
	(hrrz 2 (fluid LoadAverageStore))
	(hrli 2 8#10700)		% make a byte pointer
	(exch 1 2)
	(move 3 (lit (fullword 8#024037020200)))
	(flout)
	(!*EXIT 0)
	(!*EXIT 0)
);

syslsp procedure DoControlT();
begin scalar RunningFunctionID, CameFrom;
%    ClearTerminalInputBuffer();
    FindLoadAverage();
    CameFrom := INF ((LispVar InterruptPCStorage)[0]);
    RunningFunctionID := code!-address!-to!-symbol CameFrom or 'UNKNOWN;
    ErrorPrintF("^T: in %p at %o,   load %w",
	    RunningFunctionID, CameFrom, LispVar LoadAverageStore);
end;
>>;

syslsp procedure DoBreak();
begin scalar RunningFunctionID, CameFrom, CurrentChannel;
    ClearTerminalInputBuffer();
    CameFrom := INF( (LispVar InterruptPCStorage)[0]);
    RunningFunctionID := code!-address!-to!-symbol CameFrom or 'UNKNOWN;
    CurrentChannel := WRS NIL;
    ErrorPrintF("*** Break in %p at %o", RunningFunctionID, CameFrom);
    ErrorSet(quote Break(), NIL, NIL);
    WRS CurrentChannel;
end;


lap '((!*Entry SaveAndCallControlT expr 0) 
%
% Save all regs, call DoControlT and dismiss
%
	(adjsp (reg st) 14)		% allocate 14 slots on the stack
	(hrri (reg nil) (indexed (reg st) -13))	% set up BLT pointer
	(hrli (reg nil) 1)		% move regs 1..14 onto the stack
	(blt (reg nil) (indexed (reg st) 0))
	(move (reg nil) (fluid nil))	% fix reg nil
	(!*CALL DoControlT)		% call the function
	(hrli (reg nil) (indexed (reg st) -13))
	(hrri (reg nil) 1)
	(blt (reg nil) 14)		% move the registers back off the stack
	(move (reg nil) (fluid nil))	% restore reg nil again
	(adjsp (reg st) -14)
	(debrk)
);
>>;

lap '((!*Entry SaveAndBreak expr 0) 
%
% Save all regs, call DoBreak and dismiss
%
	(adjsp (reg st) 14)		% allocate 14 slots on the stack
	(hrri (reg nil) (indexed (reg st) -13))	% set up BLT pointer
	(hrli (reg nil) 1)		% move regs 1..14 onto the stack
	(blt (reg nil) (indexed (reg st) 0))
	(move (reg nil) (fluid nil))	% fix reg nil
	(!*CALL DoBreak)		% call the function
	(hrli (reg nil) (indexed (reg st) -13))
	(hrri (reg nil) 1)
	(blt (reg nil) 14)		% move the registers back off the stack
	(move (reg nil) (fluid nil))	% restore reg nil again
	(adjsp (reg st) -14)
	(debrk)
);

InitializeInterrupts();

off syslisp;

END;
