%
% FAST-BINDER.RED - Fast binding and unbinding routines in LAP for Dec-20 PSL
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        12 July 1981
% Copyright (c) 1981 University of Utah
%

%  25-May-1983 Mark R. Swanson
%  Changed FastBind to zero out left half of a symbol table index (for extended
%  addressing 20).

on SysLisp;

external WVar BndStkPtr,	% The binding stack pointer
	      BndStkLowerBound,	% Bottom of the binding stack
	      BndStkUpperBound;	% Top of the binding stack

% TAG( FastBind )

lap '((!*Entry FastBind expr 0)		% Bind IDs to values in registers
%
% FastBind is called with JSP T5, followed by
%  regnum,,idnum
%  ...
%
	(!*MOVE (WVar BndStkPtr) (reg t2))	% load binding stack pointer
Loop
	(!*MOVE (Indexed (reg t5) (WConst 0)) (reg t1))	% get next entry
	(tlnn (reg t1) 8#777000)	% if it's not an instruction
	(!*JUMP (Label MoreLeft))	% keep binding
	(!*MOVE (reg t2) (WVar BndStkPtr)) % Otherwise store bind stack pointer
	(!*JUMP (MEMORY (reg t5) (WConst 0)))	% and return
MoreLeft
	(!*WPLUS2 (reg t2) (WConst 2))	% add 2 to binding stack pointer
	(caml (reg t2) (WVar BndStkUpperBound))	% if overflow occured
	(!*JCALL BStackOverflow)	% then error
	(hlrz (reg t3) (reg t1))	% stick register number in t3
	(caile (reg t3) (WConst MaxRealRegs))	% is it a real register?
	(!*WPLUS2 (reg t3)		% no, move to arg block
		  (WConst (difference (WArray ArgumentBlock)
				      (plus (WConst MaxRealRegs) 1))))
				        
	(hrrzm (reg t1) (Indexed (reg t2) (WConst -1)))
					% store ID number in BndStk
        (hrrz  (reg t1) (reg t1))	% zero out left half of reg t1 for
				        % extended memory
	(!*MOVE (MEMORY (reg t1) (WConst SymVal)) (reg t4))
					% get old value for ID in t4
	(!*MOVE (reg t4) (MEMORY (reg t2) (WConst 0)))	% store value in BndStk
	(!*MOVE (MEMORY (reg t3) (WConst 0)) (reg t3))  % get reg value in t3
	(!*MOVE (reg t3) (MEMORY (reg t1) (WConst SymVal)))
					% store in ID value cell
	(aoja (reg t5) Loop)		% try again
);

% TAG( FastUnBind )

lap '((!*Entry FastUnBind expr 0)	% Unbind last N entries in bind stack
%
% FastUnBind is called with JSP T5, followed by word containing count to
% unbind.
%
	(!*MOVE (WVar BndStkPtr) (reg t1)) % get binding stack pointer in t1
	(!*MOVE (MEMORY (reg t5) (WConst 0)) (reg t2))	% count in t2
Loop
	(!*JUMPWGREATERP (Label MoreLeft) (reg t2) (WConst 0))
					% continue if count is > zero
	(!*MOVE (reg t1) (WVar BndStkPtr)) % otherwise store bind stack pointer
	(!*JUMP (MEMORY (reg t5) (WConst 1)))	% and return
MoreLeft
	(camge (reg t1) (WVar BndStkLowerBound))	% check for underflow
	(!*JCALL BStackUnderflow)
	(dmove (reg t3) (Indexed (reg t1) -1)) % get ID # in t3, value in t4
	(!*MOVE (reg t4) (MEMORY (reg t3) (WConst SymVal)))
					% restore to value cell
	(!*WDIFFERENCE (reg t1) (WConst 2)) % adjust binding stack pointer -2
	(soja (reg t2) Loop)		% and count down by 1, then try again
);

off SysLisp;

END;
