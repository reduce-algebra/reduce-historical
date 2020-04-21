%
% READ.RED - S-expression parser
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        28 August 1981
% Copyright (c) 1981 University of Utah
%

%  03-Mar-83  Nancy Kendzierski
%  Changed declaration of LispScanTable!* from global to fluid.
%  <PSL.KERNEL>READ.RED.6, 20-Oct-82 11:07:28, Edit by BENSON
%  Extra right paren in file only prints warning, not error
%  <PSL.KERNEL>READ.RED.5,  6-Oct-82 11:37:33, Edit by BENSON
%  Took away CATCH in READ, EOF error binds *InsideStructureRead to NIL
%  <PSL.KERNEL>READ.RED.2, 20-Sep-82 11:24:32, Edit by BENSON
%  Right parens at top level cause an error in a file
%  <PSL.INTERP>READ.RED.6,  2-Sep-82 14:07:37, Edit by BENSON
%  Right parens are ignored at the top level

fluid '(CurrentReadMacroIndicator!*	% Get to find read macro function
	CurrentScanTable!*		% vector of character types
	LispScanTable!*			% CurrentScanTable!* when READing
	!*InsideStructureRead);		% indicates within compound read

global '(TokType!*			% Set by token scanner, type of token
	 IN!*				% Current input channel
	 !$EOF!$);			% has value returned when EOF is read
	
CurrentReadMacroIndicator!* := 'LispReadMacro;

CompileTime flag('(DotContextError), 'InternalFunction);

lisp procedure ChannelReadTokenWithHooks Channel;  % Scan token w/read macros
%
% This is ReadToken with hooks for read macros
%
begin scalar Tkn, Fn;
    Tkn := ChannelReadToken Channel;
    if TokType!* eq 3 and (Fn := get(Tkn, CurrentReadMacroIndicator!*)) then
	return IDApply2(Channel, Tkn, Fn);
    return Tkn;
end;

lisp procedure ChannelRead Channel;	%. Parse S-expression from channel
begin scalar CurrentScanTable!*, CurrentReadMacroIndicator!*;
    CurrentScanTable!* := LispScanTable!*;
    CurrentReadMacroIndicator!* := 'LispReadMacro;
    return ChannelReadTokenWithHooks Channel;
end;

lisp procedure Read();			%. Parse S-expr from current input
<<  MakeInputAvailable();
    ChannelRead IN!* >>;

lisp procedure ChannelReadEof(Channel, Ef);	% Handle end-of-file in Read
    if !*InsideStructureRead then return
    begin scalar !*InsideStructureRead;
	return 
	StdError BldMsg("Unexpected EOF while reading on channel %r",
								Channel);
    end else !$EOF!$;

lisp procedure ChannelReadQuotedExpression(Channel, Qt);	% read macro '
    MkQuote ChannelReadTokenWithHooks Channel;

lisp procedure ChannelReadListOrDottedPair(Channel, Pa);	% read macro (
%
% Read list or dotted pair.  Collect items until closing right paren.
% Check for dot context errors.
%
begin scalar Elem, StartPointer, EndPointer, !*InsideStructureRead;
    !*InsideStructureRead := T;
    Elem := ChannelReadTokenWithHooks Channel;
    if TokType!* eq 3 then
	if Elem eq '!. then return DotContextError()
	else if Elem eq '!) then return NIL;
    StartPointer := EndPointer := list Elem;
LoopBegin:
    Elem := ChannelReadTokenWithHooks Channel;
    if TokType!* eq 3 then
	if Elem eq '!) then return StartPointer
	else if Elem eq '!. then
	<<  Elem := ChannelReadTokenWithHooks Channel;
	    if TokType!* eq 3 and (Elem eq '!) or Elem eq '!.) then
		return DotContextError()
	    else
	    <<  RplacD(EndPointer, Elem);
		Elem := ChannelReadTokenWithHooks Channel;
		if TokType!* eq 3 and Elem eq '!) then return StartPointer
		else return DotContextError() >> >>;
% If we had splice macros, I think they would be checked here
    RplacD(EndPointer, list Elem);
    EndPointer := cdr EndPointer;
    goto LoopBegin;
end;

lisp procedure ChannelReadRightParen(Channel, Tok);
% Ignore right parens at the top
    if !*InsideStructureRead then Tok
    else
    <<  if not (Channel eq StdIN!*) then % if not reading from the terminal
	    ErrorPrintF "*** Unmatched right parenthesis";
	ChannelReadTokenWithHooks Channel >>;

lisp procedure DotContextError();	% Parsing error
    IOError "Dot context error";

% List2Vector is found in TYPE-CONVERSIONS.RED

lisp procedure ChannelReadVector Channel;	% read macro [
begin scalar Elem, StartPointer, EndPointer, !*InsideStructureRead;
    !*InsideStructureRead := T;
    StartPointer := EndPointer := (NIL . NIL);
    while << Elem := ChannelReadTokenWithHooks Channel;
	     TokType!* neq 3 or Elem neq '!] >> do
    <<  RplacD(EndPointer, list Elem);
	EndPointer := cdr EndPointer >>;
    return List2Vector cdr StartPointer;
end;

StartupTime <<
    put('!', 'LispReadMacro, function ChannelReadQuotedExpression);
    put('!( , 'LispReadMacro, function ChannelReadListOrDottedPair);
    put('!) , 'LispReadMacro, function ChannelReadRightParen);
    put('![, 'LispReadMacro, function ChannelReadVector);
    put(MkID char EOF, 'LispReadMacro, function ChannelReadEOF);
>>;

END;
