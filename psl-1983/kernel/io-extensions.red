%
% IO-EXTENSIONS.RED - Random, possibly useful functions
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        22 October 1981
% Copyright (c) 1981 University of Utah
%

on SysLisp;

syslsp procedure ChannelTYI Chn;	%. Read one char ASCII value
    MkINT ChannelReadChar Chn;

syslsp procedure ChannelTYO(Chn, Ch);	%. Write one char ASCII value
    ChannelWriteChar(Chn, Lisp2Char Ch);

off SysLisp;

global '(IN!* OUT!*);

lisp procedure TYI();		%. Read ASCII value from curent input
    ChannelTYI IN!*;

lisp procedure TYO Ch;		%. Write ASCII value to current output
    ChannelTYO(OUT!*, Ch);

END;
