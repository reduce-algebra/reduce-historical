% XXX-HEADER.RED for DEC20
% Defines Data spaces, MAIN!. for 20 and I/O interface
%
% Revisions: MLG, 18 Feb 1983
%	     Move HEAP declarations from PT:SUB3
%            and P20T:20-TEST-GLOBAL-DATA.RED 
%	   Add dummy DATE and VersionName routines
on syslisp;
% -----Allocate the stack area

Internal WConst StackSize = 5000;
Internal WArray Stack[StackSize];

exported WVar StackLowerBound = &Stack[0],
	      StackUpperBound = &Stack[StackSize];

external WVar ST;

%--- Allocate HEAP and BPS areas

Internal Wconst HeapSize = 150000;		% Enough for PSL-TIMER
Internal Warray HEAP[HeapSize];			% Could do a Dynamic alloc

exported Wvar   HeapLowerBound = &Heap[0],	% bottom of heap
	        HeapUpperBound = &Heap[HeapSize],
		HeapLast,		        % next free slot in heap	
	 	HeapPreviousLast;		% save start of new block

CommentOutcode <<                              % If Copying GC
Internal Warray OtherHeap[HeapSize];
exported WVar OldHeapLast,
	      OldHeapLowerBound = &OtherHeap[0],
	      OldHeapUpperBound = &OtherHeap[HeapSize];
>>;

Internal Wconst	BPSSize  = 500;
internal Warray BPS[BPSsize];			% Could do a Dynamic alloc

exported WVar FirstBPS=&BPS[0],  		% Base of BPS, for info
	      NextBPS = &BPS[0],                % allocate CODE up
	      LastBPS = &BPS[BPSSize],          % allocate Warray down
              FinalBPS= &BPS[BPSSize]; 		% For info purposes

syslsp procedure InitHeap();
% Set up Heap base etc.
 <<HeapLast:=HeapLowerBound;
   HeapPreviousLast := 0>>;


% allocate for the "extra" arguments
% 0..MaxArgBlock are arguments (MaxRealRegs + 1)..MaxArgs

internal WConst MaxArgBlock = (MaxArgs - MaxRealRegs) - 1;
exported WArray ArgumentBlock[MaxArgBlock];

% For the ForeignFunction calling protocol
exported Wvar Arg1,Arg2,Arg3,ARg4,Arg5,Arg6,Arg7,Arg8,
              Arg9, Arg10,Arg11,Arg12,Arg13,Arg14,Arg15;

%--- End of Data Definitions ----------

%--- Now do 20 Specific MAIN!. and I/O Interface:

lap '((!*entry Main!. expr 0)
      (reset)
      (move (reg st) (lit (halfword (minus (WConst StackSize))
				      (difference (WConst Stack) 1))))
      (move (reg NIL) (fluid NIL))
      (!*LINKE 0 FirstCall Expr 0)  % Call the MAINn firstroutine
);

% Define "standard" LISP equivalents for the DEC20-MACRO foreign
% functions defined in 20IO.MAC

FLAG('(
   Init20  % Initialize I/O, Timer, etc
   PutC20  % Print Ascii Character, use 10=EOL to get end of line
   GetC20  % Return Ascii Character
   Timc20  % Return CPU time (can also print time check)
   Quit20  % Terminate execution, finalize
   Err20   % Print error message
   PutI20  % print an Integer
),'ForeignFunction);


Global '(IN!* OUT!*);

Procedure Init();
 <<Init20 0;
   LispVar IN!*:=0;
   LispVar Out!*:=1;
   >>;         % Always need one dummy argument

Procedure GetC();
 If LispVar IN!* eq 0 then Getc20 0         % Always need one dummy argument
  else IndependentReadChar LispVar IN!*;

Procedure TimC();
  TimC20 0;         % Always need one dummy argument

procedure PutC x;
 If LispVar Out!* eq 1 then Putc20 x     
  else IndependentWriteChar(LispVar Out!*,x);

procedure Quit;
  Quit20 0;         % always need 1 argument

procedure Date;
  '"No-Date-Yet";

Procedure VersionName;
  '"DEC-20 test system";

procedure PutInt I;
  PutI20 I;

% SYMFNC storage routine:
LAP '((!*entry !%Store!-Jcall Expr 2) % CodeAddress, Storage Address
      (!*alloc 0) 
      (!*WOR (reg 1) 8#254000000000)  % Load a JRST in higher-bits
      (!*MOVE (reg 1) (memory (reg 2) (wconst 0)))
      (!*EXIT 0));

LAP '((!*entry !%copy!-function!-cell Expr 2) % from to
      (!*alloc 0) 
      (!*move (memory (reg 1) (Wconst 0)) (memory (reg 2) (wconst 0)))
      (!*exit 0));

FLUID '(UndefnCode!* UndefnNarg!*);

LAP '((!*ENTRY UndefinedFunction expr 0) % For missing Function
 % No alloc 0 ? and no LINKE because dont want to change LinkReg
      (!*MOVE (reg LinkReg) (Fluid UndefnCode!*))
      (!*Move (reg NargReg) (Fluid UndefnNarg!*))
      (!*JCALL UndefinedFunctionAux)
);

LAP '((!*ENTRY FLAG expr 2)      % Dummy for INIT
      (!*alloc 0) 
      (!*MOVE  2 (REG 1))
      (!*LINKE 0 Err20 Expr 1)
);

procedure LongTimes(x,y);
  x*y;

procedure LongDiv(x,y);
  x/y;

procedure LongRemainder(x,y);
  Remainder(x,y);

off syslisp;

end;

