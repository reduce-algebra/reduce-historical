%
% CHAR.RED - Character constant macro
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        10 August 1981
% Copyright (c) 1981 University of Utah
%

macro procedure Char U;			%. Character constant macro
    DoChar cadr U;

lisp procedure DoChar U;
begin scalar ChDef, CharFn;
    return if IDP U then
	if (ChDef := get(U, 'CharConst)) then ChDef
	else if (ChDef := ID2Int U) < 128 then ChDef
	else CharError U
    else if PairP U then
    <<  CharFn := car U;
	U := cadr U;
	if CharFn eq 'QUOTE then DoChar U
	else if CharFn eq 'LOWER then LOr(DoChar U, 2#100000)
	else if CharFn memq '(CNTRL CONTROL) then LAnd(DoChar U, 2#11111)
	else if CharFn eq 'META then LOr(DoChar U, 2#10000000)
	else CharError U >>
    else if FixP U and U >= 0 and U <= 9 then U + char !0
    else CharError U;
end;

lisp expr procedure CharError U;
<<  ErrorPrintF("*** Unknown character constant: %r", U);
    0 >>;

DefList('((NULL 0)
	  (BELL 7)
	  (BACKSPACE 8)
	  (TAB 8#11)
	  (LF 8#12)
	  (RETURN 8#12)		% RETURN is LF because it's end-of-line
	  (EOL 8#12)
	  (FF 8#14)
	  (CR 8#15)
	  (ESC 27)
	  (ESCAPE 27)
	  (BLANK 32)
	  (SPACE 32)
	  (RUB 8#177)
	  (RUBOUT 8#177)
	  (DEL 8#177)
	  (DELETE 8#177)), 'CharConst);

END;
