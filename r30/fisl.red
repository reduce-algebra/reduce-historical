COMMENT MODULE FISL;

COMMENT FASLOD uses a vector in high BPS for accessing incoming
	atom names.  For Tenex and Tops-20 with high memory BPS
	available, 2K is permanently allocated.  For Tops-10 with a
	constricted low memory BPS area, 1K is assigned at the end of
	BPS, and is de-allocated after each FISL for other use...
	Note that Tops-10 users with a large FAP file to read can
	alter FISLSIZE at any time to obtain a larger/smaller table;

COMMENT FASLOD expects the FISLTABLE vector to be allocated at the end
	of BPS, somewhere beyond BPORG, as its origin is
	the uppermost bound within FASLOD for BPORG stores;

COMMENT FASLOD has a potentially useful routine to expand the vector
	being used as FISLTABLE, but in current LISP this is not
	used. Since LAP stores upward in memory, and the expander code
	would have to dynamically relocate the vector and its
	SUBR routine downaward and adjust the property list and
	GCMKL accordingly, FISL is fastest as is (with fixed vector).
	However, if large files/programs are to be input, either
	a lower vector base or dynamic allocation would be necessary;


SYMBOLIC PROCEDURE !%DEVP U;
 IDP U AND CAR REVERSE EXPLODE U EQ '!:
  OR PAIRP U AND PAIRP CDR U;

SYMBOLIC PROCEDURE FILNAM FIL;
 IF ATOM FIL THEN FIL
  ELSE CAR IF NOT !%DEVP CAR FIL THEN FIL
            ELSE CDR IF NOT CAR FIL EQ 'DIR!: THEN FIL
                      ELSE CDR FIL;

GLOBAL '(FISLSIZE INC!* MLIST!*);

FLUID '(BPEND BPORG FISLTABLE !*BAKGAG !*PREDEF !*PURIFY);

SYMBOLIC PROCEDURE LOAD!-MODULE FIL;
   BEGIN SCALAR MODUL;
      IF NOT ATOM(MODUL := FILNAM FIL) THEN MODUL := CAR MODUL;
      IF MODUL MEMBER MLIST!*
	THEN <<TERPRI();
	       PRIN2 LIST("***",MODUL,"already loaded");
	       TERPRI(); RETURN NIL>>
       ELSE IF ATOM FIL THEN FIL := LIST(FIL . 'FAP);
      IF NOT FILEP FIL AND NOT !%DEVP CAR FIL
         THEN FIL := 'SYS!: . FIL;
      IF NOT FILEP FIL THEN ERROR(40,LIST(MODUL,"MODULE NOT FOUND"));
      IF GETD 'EXCORE THEN BEGIN SCALAR X,Y;
 	IF NOT(X := GET(MODUL,'FAPSIZE)) THEN X := 1;   %minimum value;
	Y := (BPEND-BPORG-FISLSIZE)/1000;   %what's left;
	IF Y>X THEN RETURN;
	X := X+1;
	IF BPORG>131071. THEN EXCORE(X-Y)
	 ELSE IF NULL INC!* THEN <<EXCORE X;
			     WARNING LIST('EXCORE,X,"PERFORMED")>>
	 ELSE ERROR(41,LIST('EXCORE,X,"AT TOP LEVEL REQUIRED"))
       END;
      MLIST!* := MODUL . MLIST!*;
      RETURN FISLF FIL
   END;

SYMBOLIC FEXPR PROCEDURE LOAD LST;
   %LST is a list of fast-loading format module names;
   FOR EACH MODUL IN LST DO LOAD!-MODULE MODUL;

SYMBOLIC PROCEDURE FISLF FILE;
   BEGIN SCALAR OCH;
	OCH:= RDS OPEN(FILE,'INBIN);
	IF GETD 'EXCORE THEN
	   BEGIN SCALAR X;
		IF (X:=BPORG) > (BPEND-FISLSIZE) 
			THEN ERROR(170,"NO FISLTABLE ROOM");
		BPORG:=BPEND-FISLSIZE-1;
		FISLTABLE:=MKVECT(2*FISLSIZE-5);
		BPORG:=X;
	   END;
	ERRORSET('(FASLOD FISLTABLE !*PREDEF !*PURIFY),T,!*BAKGAG);
	CLOSE RDS OCH;
	IF GETD 'EXCORE THEN <<DLVECT FISLTABLE; FISLTABLE:=NIL>>;
	LDFERR();
   END;


END;
    