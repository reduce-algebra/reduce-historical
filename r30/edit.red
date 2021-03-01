COMMENT MODULE EDIT;

%PUT('EDIT,'IMPORTS,'(IO));   %needs CLOSE;

FLUID '(BASE);

GLOBAL '(FILE!* PAGE!* LINE!* EDIT!* FLG!*);

COMMENT EDIT!* indicates that an edit fork has just been left,
	FLG!* that CMD or EDIT has been called;

GLOBAL '(CRST!* CRLFST!* EDITFORK!* SYSTEM!* !$EOL!$);

CRST!* := LIST(IF SYSTEM!* = 1 THEN !$EOL!$ ELSE INTERN ASCII 13,'!");

CRLFST!* := LIST(INTERN ASCII 13,INTERN ASCII 10,'!");

EDITFORK!* :=
   IF SYSTEM!* = 1 THEN "<SUBSYS>SOS.SAV" ELSE "SYS:EDIT.EXE";

FLUID '(BASE);

SYMBOLIC PROCEDURE CREATE U; CALLEDITOR(U,NIL,NIL,2);

SYMBOLIC PROCEDURE CALLEDITOR(FILE,PAGE,LINE,CREATEF);
 BEGIN SCALAR BASE;
  BASE := 10.;
  IF NULL FILE THEN GO RET;
  IF NULL LINE THEN GO NL;
  IF PAGE THEN PAGE := '!/ . EXPLODE2 PAGE;
  LINE := IF ATOM LINE THEN EXPLODE2 LINE
	 ELSE '!^ . '!+ . EXPLODE2 CAR LINE;
  IF SYSTEM!* = 1 THEN LINE := NCONC(!$EOL!$ . 'P . NCONC(LINE,PAGE),CRST!*)
   ELSE LINE := COMPRESS('!" . 'P . NCONC(LINE,NCONC(PAGE,CRST!*)));
 NL:
  IF SYSTEM!* = 1 THEN FILE := IF CREATEF=1 THEN APPEND('(!" !/ R ! ),FILE)
				ELSE '!" . FILE
   ELSE FILE := APPEND(IF CREATEF=1 THEN '(!" E D I T !  !/ R ! )
			ELSE IF CREATEF=2 THEN '(!" C R E A T E ! )
			ELSE '(!" E D I T ! ),
		       NCONC(FILE,CRLFST!*));
  FILE := COMPRESS FILE . LINE;
 RET:
  RETURN XEQKEEP('EDITFORK!*,EDITFORK!*,FILE)
 END;

SYMBOLIC PROCEDURE EDITLINE;
   BEGIN INTEGER VAL; SCALAR XECHO;
	EDIT!* := NIL;
	IF IFL!*
	  THEN <<LPRIW("*****","Editing can only be done from terminal");
		 RETURN NIL>>
	 ELSE IF NOT FILEP(FILE!* := MKFIL FILE!*)
          THEN <<LPRIW("*****","Unknown file name");
		 RETURN IFL!* := NIL>>;
	IFL!* := FILE!* . OPEN(FILE!*,'INPUT);
	RDS CDR IFL!*;
	IPL!* := IFL!* . IPL!*;
	XECHO := !*ECHO; !*ECHO := NIL;
	!%FPAGE PAGE!*;
    LOOP: !%NEXTTYI();
	VAL := CDR PGLINE();
	IF PAIRP VAL THEN VAL := CAR VAL;
	IF VAL<LINE!* THEN <<SKIPTO !$EOL!$; GO TO LOOP>>;
	!*ECHO := XECHO;
	IF VAL>LINE!* THEN REDERR "Line not found";
	IF !*ECHO THEN TYO !%NEXTTYI();
	   %If !*RAISE is on this will be upper case;
   END;

SYMBOLIC PROCEDURE EDITSTAT;
   BEGIN SCALAR X,Y,Z;
      X := RLIS();
      Y := CDR X;
      X := NULL(CAR X EQ 'EDIT);
      IF NULL CDR Y
	 THEN IF X THEN REDERR "Invalid argument for CMD"
	      ELSE IF STRINGP CAR Y OR IDP CAR Y AND FILEP CAR Y
	      THEN RETURN LIST('CALLEDITOR,MKQUOTE EXPLODE2 CAR Y,
				NIL,NIL,0)
	      ELSE RETURN LIST('EDIT0,MKQUOTE Y,NIL);
      Y := CAR Y . REMCOM CDR Y;
      IF NULL CDR Y
	THEN IF X THEN REDERR "Invalid argument for CMD"
	ELSE RETURN LIST('CALLEDITOR,
			MKQUOTE EXPLODE2 CAR Y,NIL,NIL,0)
       ELSE RETURN LIST('EDIT0,MKQUOTE Y,X)
   END;

SYMBOLIC PROCEDURE REMCOM U;
   IF NULL U THEN NIL
    ELSE IF CAR U EQ '!, THEN REMCOM CDR U
    ELSE CAR U . REMCOM CDR U;

SYMBOLIC PROCEDURE EDIT0(U,V);
   %U is function name or file description.
   %V is T if CMD, NIL if EDIT;
   <<FLG!* := T;
	IF NULL CDR U THEN IF V THEN REDERR "Invalid argument for CMD"
			    ELSE EDIT11(CAR U,NIL,T)
%         ELSE IF IDP CADR U THEN EDIT11(CAR U,CADR U,T)
	 ELSE EDIT2(CAR U,IF CDDR U THEN CADDR U ELSE 1,CADR U,T,V)>>;

SYMBOLIC PROCEDURE EDIT11(U,W,V);
   %U is name of function being edited
   %V is T if called;
   BEGIN SCALAR LOC;
	LOC:=IF NULL V THEN U
	 ELSE IF NULL W THEN GET(U,'LOCN)
	 ELSE IF (LOC:=ATSOC(GET(U,'LOCNF),W)) THEN CDR LOC;
	IF NOT LOC THEN RETURN EDITDEF1 U;
	EDIT2(CAR LOC,CADR LOC,CDDR LOC,V,NIL)
   END;

SYMBOLIC PROCEDURE EDIT2(FILE,PAGE,LINE,CALLED,NOCHANGE);
   BEGIN %!*DEFN := NIL; ?;
	IF NOT FIXP PAGE THEN TYPERR(PAGE,"integer")
	 ELSE IF NOT FIXP LINE THEN TYPERR(LINE,"integer");
	FILE!* := FILE;
	PAGE!* := PAGE;
	LINE!* := LINE;
	EDIT!* := T;
	RETURN IF NOCHANGE THEN BEGIN1()
		ELSE CALLEDITOR(EXPLODE2 FILE,PAGE,LINE,0)
   END;

%SYMBOLIC PROCEDURE FILEMK U;
   % Convert a file specification from lisp format to a string.
   % This is essentially the inverse of MKFILE;
%    BEGIN SCALAR DEV,NAME,FLG,FLG2;
%  IF NULL U THEN RETURN NIL
%   ELSE IF ATOM U THEN NAME := EXPLODE2 U
%   ELSE FOR EACH X IN U DO
%    IF X EQ 'DIR!: THEN FLG := T
%     ELSE IF ATOM X THEN
%      IF FLG THEN <<FLG := NIL;
%                   DEV := '!< . NCONC(EXPLODE2 X,LIST '!>)>>
%       ELSE IF X EQ 'DSK!: THEN DEV:=NIL
%       ELSE IF !%DEVP X THEN DEV := EXPLODE2 X
%       ELSE NAME := EXPLODE2 X
%     ELSE IF ATOM CDR X THEN
%      NAME := NCONC(EXPLODE2 CAR X,'!. . EXPLODE2 CDR X)
%     ELSE <<FLG2 := T;
%            DEV := '![ . NCONC(EXPLODE2 CAR X,
%                              '!, . NCONC(EXPLODE2 CADR X,LIST '!]))>>;
%      U := IF FLG2 THEN NCONC(NAME,DEV) ELSE NCONC(DEV,NAME);
%      RETURN COMPRESS('!" . NCONC(U,'(!")))
%   END;

SYMBOLIC PROCEDURE EDIT1(U,V);
 <<CLOSE CDR IFL!*; IPL!*:=CDR IPL!*;
   RDS IF IPL!* THEN CDR (IFL!*:=CAR IPL!*) ELSE IFL!*:=NIL;
   EDIT11(U,NIL,V)>>;


END;