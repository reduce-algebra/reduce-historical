% File to read PASLSP.INI to produce sorted tables

FLUID '(NID!* IDLIST!* NCONST!* CONSTLIST!* NFN!* FNLIST!*);

lisp procedure IniErr x;
  Error LIST("Bad Ini File ",x);

load gsort;

lisp procedure prinl l;
 for each x in l do print x;

lisp procedure Sorts;
 Begin					
     ReadPaslspInit();
     Prin2t "--------------- Functions ----------------";
     prinl idsort FNLIST!*;
     Prin2t "--------------- Other IDS ----------------";
     prinl idsort IDLIST!*;
     Prin2t "--------------- CONST ----------------";
     prinl CONSTLIST!*;
 End;

lisp procedure ReadPaslspInit;
  BEGIN scalar infil,oldfil;
	% load "symbol table" with identifiers, constants, and functions.  
      infil:=open("paslsp.ini",'input);
      oldfil:=rds(infil);
      NID!*:=RATOM();     % get count of identifiers. 
      IF not fixp NID!* THEN
	    IniErr("*****BAD SYMBOL TABLE, INTEGER EXPECTED AT START");
      IDLIST!*:=NIL;
      FOR i :=  1:NID!* DO
	    IDLIST!* := RATOM() . IDLIST!*;
	% reading token magically loads it into id space. 
	IF not ZeroP RATOM()         % look for zero terminator. 
           then
	    IniErr("*****BAD SYMBOL TABLE, ZERO EXPECTED AFTER IDENTIFIERS");
	NCONST!*:=RATOM();         % count of constants  
	IF not FIXP NCONST!* THEN
          IniErr("*****BAD SYMBOL TABLE, INTEGER EXPECTED BEFORE CONSTANTS");
        CONSTLIST!*:=NIL;
	FOR i := 1:NCONST!* DO
	  CONSTLIST!*:=READ() . CONSTLIST!*;
	IF  not ZeroP RATOM() then
         IniErr("*****BAD SYMBOL TABLE, ZERO EXPECTED AFTER CONSTANTS");

	NFN!*:=RATOM();     % count of functions. 
	IF  not FIXP NFN!* then
	   IniErr("*****BAD SYMBOL TABLE, INTEGER EXPECTED BEFORE FUNCTIONS");
	FNLIST!*:=NIL;
	FOR i := 1:NFN!* DO
	    % for each function 
	    % store associated code 
	    FNLIST!*:=RATOM(). FNLIST!*;
	If not Zerop RATOM() then
         IniErr("*****BAD SYMBOL TABLE, ZERO EXPECTED AFTER FUNCTIONS");
        RDS(oldfil);
	CLOSE infil;
  END;
