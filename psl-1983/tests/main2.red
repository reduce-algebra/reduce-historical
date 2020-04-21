% MAIN2.RED - Test Byte and String I/O, some PRINT ing
%  Need:  SUB2.RED simple print routines



IN "XXX-HEADER.RED"$

on SysLisp;

% some strings to work with
WString TestString = "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUnVvWwXxYyZz";
Wstring Buffer[100];

syslsp Procedure FirstCall;
  begin scalar X, Y;
    init();
  % test STRINF
    Putc Char S; 
      PutC Char Lower t; 
        PutC Char Lower r; 
	   Putc Char I; 
  	     Putc Char Lower n ; 
     	       Putc Char Lower f; 
	          Putc Char Eol;
    X:=TestString;
    Y:=StrInf(X);
    PutInt X; PutC Char '! ; PutInt Y;PutC Char EOL;
% test STrlen
    Putc Char S; 
      PutC Char Lower t; 
        PutC Char Lower r; 
	   Putc Char Lower l; 
  	     Putc Char Lower e; 
     	       Putc Char Lower n; 
	          Putc Char Eol;
X:=StrLen(testString);
PutInt X;PutC Char '! ;PutInt 51;PutC Char EOL;
% test Byte access.
    X:=TestString+AddressingUnitsPerItem;
    Putc Char B; 
      PutC Char Lower y; 
        PutC Char Lower t; 
	   Putc Char Lower e; 
	     Putc Char Eol;
    For i:=0:10 do
     <<Y:=Byte(X,i);
       PutInt i; PutC Char '! ; 
       PutInt Y; PutC Char '! ;
       PutC Y; PutC Char EOL>>;
% Now a string:
    Putc Char S; 
      PutC Char Lower t; 
        PutC Char Lower r; 
	   Putc Char Lower i; 
       	     Putc Char Lower n; 
	        Putc Char Lower g; 
                   Putc Char Eol;
    Prin2String TestString;
    Terpri();
    Prin1String "----- Now input characters until #";
    Terpri();
    while (X := GetC X) neq char !# do PutC X;
    Print '"----- First Print Called";
    Print '1;
    Print 'ANATOM;
    Print '( 1 . 2 );
    Print '(AA (B1 . B2) . B3);
    Print '(AA (B1 . NIL) . NIL);
    Prin2T 
    "Expect UNDEFINED FUNCTION MESSAGE for a function of 3 arguments";
    ShouldNotBeThere(1,2,3);
    quit;
end;

Fluid '(UndefnCode!* UndefnNarg!*);

syslsp procedure UndefinedFunctionAux; 
% Should preserve all regs
 <<Terpri();
   Prin2String "**** Undefined Function: ";
   Prin1ID LispVar UndefnCode!*;
   Prin2String " , called with ";
   Prin2  LispVar UndefnNarg!*;
   Prin2T " arguments";
   Quit;>>;


Off syslisp;


End;
