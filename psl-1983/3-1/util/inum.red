%
% INUM.RED - Interpreter entries for open-compiled integer arithmetic
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        18 March 1982
% Copyright (c) 1982 University of Utah
%

off R2I;

CompileTime
<<

smacro procedure InumTwoArg IName;
lisp procedure IName(Arg1, Arg2);
begin scalar Result;
    return if IntP Arg1 and IntP Arg2
		and IntP(Result := IName(Arg1, Arg2)) then Result
    else Inum2Error(Arg1, Arg2, quote IName);
end;

smacro procedure InumTwoArgBool IName;
lisp procedure IName(Arg1, Arg2);
    if IntP Arg1 and IntP Arg2 then IName(Arg1, Arg2)
    else Inum2Error(Arg1, Arg2, quote IName);

smacro procedure InumOneArg IName;
lisp procedure IName Arg;
begin scalar Result;
    return if IntP Arg and IntP(Result := IName Arg) then
	Result
   else Inum1Error(Arg, quote IName);
end;

smacro procedure InumOneArgBool IName;
lisp procedure IName Arg;
    if IntP Arg then IName Arg
   else Inum1Error(Arg, quote IName);

>>;

lisp procedure Inum2Error(Arg1, Arg2, Name);
    ContinuableError(99, "Inum out of range", list(Name, Arg1, Arg2));

lisp procedure Inum1Error(Arg, Name);
    ContinuableError(99, "Inum out of range", list(Name, Arg));

InumTwoArg IPlus2;

InumTwoArg IDifference;

InumTwoArg ITimes2;

InumTwoArg IQuotient;

InumTwoArg IRemainder;

InumTwoArgBool ILessP;

InumTwoArgBool IGreaterP;

InumTwoArgBool ILEQ;

InumTwoArgBool IGEQ;

InumTwoArg ILOR;

InumTwoArg ILAND;

InumTwoArg ILXOR;

InumTwoArg ILSH;

InumOneArg IAdd1;

InumOneArg ISub1;

InumOneArg IMinus;

InumOneArgBool IZeroP;

InumOneArgBool IOneP;

InumOneArgBool IMinusP;

on R2I;

macro procedure IFor U;
    MkSysFor U;

if not FUnBoundP 'Begin1 then <<

DEFINEROP('IFOR,NIL,ParseIFOR);

SYMBOLIC PROCEDURE ParseIFOR X; 
   BEGIN SCALAR INIT,STP,UNTL,ACTION,ACTEXPR; 
       IF (OP := SCAN()) EQ 'SETQ THEN INIT := PARSE0(6,T)
       ELSE PARERR("FOR missing loop VAR assignment",T); 
      IF OP EQ '!*COLON!* THEN <<STP := 1; OP := 'UNTIL>>
       ELSE IF OP EQ 'STEP THEN STP := PARSE0(6,T)
       ELSE PARERR("FOR missing : or STEP clause",T); 
      IF OP EQ 'UNTIL THEN UNTL := PARSE0(6,T) 
	ELSE PARERR("FOR missing UNTIL clause",T); 
      ACTION := OP; 
      IF ACTION MEMQ '(DO SUM PRODUCT) THEN ACTEXPR := PARSE0(6,T)
       ELSE PARERR("FOR missing action keyword",T); 
      RETURN LIST('IFOR,
                  LIST('FROM,X,INIT,UNTL,STP),
		  LIST(ACTION,ACTEXPR))
   END;
>>;

END;
