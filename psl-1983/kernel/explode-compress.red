%
% EXPLODE-COMPRESS.RED - Write to/read from a list; includes FlatSize
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        24 September 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.KERNEL>EXPLODE-COMPRESS.RED.3, 12-Oct-82 16:49:54, Edit by BENSON
%  Changed CompressReadChar to use Lisp2Char, so ASCII characters are OK,
%  but digits 0..9 as !0..!9 are not.

fluid '(ExplodeEndPointer!*	% pointer used to RplacD new chars onto
	CompressList!*			% list being compressed
	!*Compressing);			% if T, don't intern IDs when read

external WArray LinePosition,UnReadBuffer;

on SysLisp;

syslsp procedure ExplodeWriteChar(Channel, Ch);
<<  RplacD(LispVar ExplodeEndPointer!*, list MkID Ch);
    LispVar ExplodeEndPointer!* := cdr LispVar ExplodeEndPointer!* >>;

syslsp procedure Explode U;		%. S-expr --> char-list
begin scalar Result;
    Result := LispVar ExplodeEndPointer!* := NIL . NIL;
    LinePosition[3] := 0;
    ChannelPrin1('3, U);
    return cdr Result;
end;

syslsp procedure Explode2 U;		%. Prin2 version of Explode
begin scalar Result;
    Result := LispVar ExplodeEndPointer!* := NIL . NIL;
    LinePosition[3] := 0;
    ChannelPrin2('3, U);
    return cdr Result;
end;

internal WVar FlatSizeAccumulator;

syslsp procedure FlatSizeWriteChar(Channel, Ch);
    FlatSizeAccumulator := FlatSizeAccumulator + 1;

syslsp procedure FlatSize U;		%. character length of S-expression
<<  FlatSizeAccumulator := 0;
    LinePosition[4] := 0;
    ChannelPrin1('4, U);
    MkINT FlatSizeAccumulator >>;

lisp procedure FlatSize2 U;		%. Prin2 version of FlatSize
<<  FlatSizeAccumulator := 0;
    LinePosition[4] := 0;
    ChannelPrin2('4, U);
    MkINT FlatSizeAccumulator >>;

internal WVar AtEndOfList;

syslsp procedure CompressReadChar Channel;
begin scalar NextEntry;
    if AtEndOfList then return CompressError();
    if not PairP LispVar CompressList!* then
    <<  AtEndOfList := 'T;
	return char BLANK >>;
    NextEntry := car LispVar CompressList!*;
    LispVar CompressList!* := cdr LispVar CompressList!*;
    return Lisp2Char NextEntry;
end;

syslsp procedure ClearCompressChannel();
<<  UnReadBuffer[3] := char NULL;
    AtEndOfList := 'NIL >>;

off SysLisp;

lisp procedure CompressError();
    StdError "Poorly formed S-expression in COMPRESS";

lisp procedure Compress CompressList!*;	%. Char-list --> S-expr
begin scalar !*Compressing;
    !*Compressing := T;
    ClearCompressChannel();
    return ChannelRead 3;
end;

lisp procedure Implode CompressList!*;	%. Compress with IDs interned
<<  ClearCompressChannel();
    ChannelRead 3 >>;

END;
