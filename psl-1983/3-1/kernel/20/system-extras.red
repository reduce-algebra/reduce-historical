%
% 20-EXTRAS.RED - System-specific functions for Dec-20 PSL
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        4 March 1982
% Copyright (c) 1982 University of Utah
%

%  21-May-83 Mark R. Swanson
%    Made local byte pointer into global byte pointer in DATE; changed 
%    ReturnAddressP to use only low halfword of value in SYMFNC table.
%  <PSL.KERNEL-20>SYSTEM-EXTRAS.RED.3,  5-Jan-83 16:46:34, Edit by PERDUE
%  Added ExitLISP, for the DEC-20 a synonym of QUIT

fluid '(system_list!*);

if_system(Tenex,
    if_system(KL10,
	system_list!* := '(Dec20 PDP10 Tenex KL10),
	system_list!* := '(Dec20 PDP10 Tenex)),
    system_list!* := '(Dec20 PDP10 Tops20 KL10));

lap '((!*entry Quit expr 0)
      (haltf)
      (!*MOVE '"Continued" (reg 1))
      (!*EXIT 0)
);

CopyD('ExitLISP, 'Quit);

lap '((!*entry Date expr 0)
      (!*MOVE (WConst 8) (reg 1))	% allocate a 9 character string
      (!*CALL GtStr)
      (!*MOVE (reg 1) (reg 4))		% save it in 4
      (!*WPLUS2 (reg 1) (WConst 1))
      (tlo 1 8#610000)			% create a byte pointer to it
      (!*MOVE (WConst -1) (reg 2))	% current date
      (hrlzi (reg 3) 2#0000000001)	% ot%ntm, don't output time
      (odtim)
      (!*MOVE (reg 4) (reg 1))
      (!*MKITEM (reg 1) (WConst STR))	% tag it as a string
      (!*EXIT 0)
);

if_system(KL10, NIL,
lap '((!*Entry StackOverflow expr 0)
      (sub (reg ST) (lit (halfword 1000 1000)))	% back up stack
      (!*MOVE '"Stack overflow" (reg 1))
      (!*JCALL StdError)
));

on SysLisp;

syslsp procedure ReturnAddressP X;
begin scalar Y, Z;
    Z := Field(&SymFnc, 18, 18); % don't want any opcode bits in Z
				 % may someday want to use 23 bits, though.
    return Field(X, 0, 18) = 2#011001000000000000	% PC flags
    and Field(@(X - 1), 0, 18) = 8#260740	% pushj 17,
    and (Y := Field(@(X - 1), 18, 18) - Z) > 0 and Y < MaxSymbols
    and MkID Y;
end;

off SysLisp;

END;
