% ===============================================================
% General Purpose I/O package for CREF, adapted to PSL
% MLG, 6:19am  Tuesday, 15 December 1981
% ===============================================================
%==============================================================================
% 11/18/82 - rrk - The function REMPROPSS was being called from RECREF in the
%  redefintion of a procedure with a single procedure name as the first 
%  argument.  This somehow caused the routine to go into an infinite loop.  A
%  quick to turn the ID into a list within REMPROPSS solves the problem.  The
%  reason that the call to REMPROPSS was not changed, is because it is not
%  clear if in some cases the argument will be a list.
%==============================================================================


GLOBAL '(!*FORMFEED   ORIG!* LNNUM!* MAXLN!* TITLE!* PGNUM!*  );

% FLAGS: FORMFEED (ON)  controls ^L or spacer of ====;

SYMBOLIC PROCEDURE INITIO();
% Set-up common defaults;
   BEGIN
	!*FORMFEED:=T;
	ORIG!*:=0;
	LNNUM!*:=0;
	LINELENGTH(75);
	MAXLN!*:=55;
	TITLE!*:=NIL;
	PGNUM!*:=1;
   END;

SYMBOLIC PROCEDURE LPOSN();
   LNNUM!*;

INITIO();

SYMBOLIC PROCEDURE SETPGLN(P,L);
  BEGIN IF P THEN MAXLN!*:=P;
	IF L THEN LINELENGTH(L);
  END;

% We use EXPLODE to produce a list of chars from atomname,
% and TERPRI() to terminate a buffer..all else
% done in package..spaces,tabs,etc. ;

COMMENT Character lists are (length . chars), for FITS;

SYMBOLIC  PROCEDURE GETES U;
% Returns for U , E=(Length . List of char);
   BEGIN SCALAR E;
	IF NOT IDP U THEN RETURN<<E:=EXPLODE U;LENGTH(E).E>>;
   	IF NOT(E:=GET(U,'RCCNAM)) THEN <<E:=EXPLODE(U);
				   E:=LENGTH(E) . E;
				   PUT(U,'RCCNAM,E)>>;
	RETURN E;
   END;

SYMBOLIC SMACRO PROCEDURE PRTWRD U;
   IF NUMBERP U THEN PRTNUM U
    ELSE PRTATM U;

SYMBOLIC PROCEDURE PRTATM U;
	PRIN2 U;	% For a nice print;

SYMBOLIC PROCEDURE PRTLST U;
 IF ATOM U THEN PRIN2 U ELSE FOR EACH X IN U DO PRIN2 X;

SYMBOLIC PROCEDURE PRTNUM N;
	PRIN2 N;

SYMBOLIC PROCEDURE PRINCN E;
% output a list of chars, update POSN();
	 WHILE (E:=CDR E) DO PRINC CAR E;

CommentOutCode <<			% Defined in PSL
SYMBOLIC PROCEDURE SPACES N;
	FOR I:=1:N DO PRINC '!  ;

SYMBOLIC PROCEDURE SPACES2 N;
   BEGIN SCALAR X;
        X := N - POSN();
	IF X<1 THEN NEWLINE N
	 ELSE SPACES X;
   END;
>>;

SYMBOLIC PROCEDURE SETPAGE(TITLE,PAGE);
% Initialise current page and title;
   BEGIN
	TITLE!*:= TITLE ;
	PGNUM!*:=PAGE;
   END;

SYMBOLIC PROCEDURE NEWLINE N;
% Begins a fresh line at posn N;
   BEGIN
	LNNUM!*:=LNNUM!*+1;
	IF LNNUM!*>=MAXLN!* THEN NEWPAGE()
	 ELSE TERPRI();
	SPACES(ORIG!*+N);
   END;

SYMBOLIC PROCEDURE NEWPAGE();
% Start a fresh page, with PGNUM and TITLE, if needed;
   BEGIN SCALAR A;
	A:=LPOSN();
	LNNUM!*:=0;
	IF POSN() NEQ 0 THEN NEWLINE 0;
	IF A NEQ 0 THEN FORMFEED();
	IF TITLE!* THEN
	  <<SPACES2 5; PRTLST TITLE!*>>;
	SPACES2 (LINELENGTH(NIL)-4);
	IF PGNUM!* THEN <<PRTNUM PGNUM!*; PGNUM!*:=PGNUM!*+1>>
	 ELSE PGNUM!*:=2;
	NEWLINE 10;
	NEWLINE 0;
   END;

SYMBOLIC PROCEDURE UNDERLINE2 N;
	IF N>=LINELENGTH(NIL) THEN
	  <<N:=LINELENGTH(NIL)-POSN();
	    FOR I:=0:N DO PRINC '!- ;
	    NEWLINE(0)>>
	 ELSE BEGIN SCALAR J;
		J:=N-POSN();
		FOR I:=0:J DO PRINC '!-;
	      END;

SYMBOLIC PROCEDURE LPRINT(U,N);
% prints a list of atoms within block LINELENGTH(NIL)-n;
   BEGIN SCALAR E, L,M;
	SPACES2 N;
	L := LINELENGTH NIL-POSN();
	IF L<=0 THEN ERROR(13,"WINDOW TOO SMALL FOR LPRINT");
	WHILE U DO
	   <<E:=GETES CAR U; U:=CDR U;
 	     IF LINELENGTH NIL<POSN() THEN NEWLINE N;
	     IF CAR E<(M := LINELENGTH NIL-POSN()) THEN PRINCN E
	      ELSE IF CAR E<L THEN <<NEWLINE N; PRINCN E>>
	      ELSE BEGIN
		 E := CDR E;
	      A: FOR I := 1:M DO <<PRINC CAR E; E := CDR E>>;
		 NEWLINE N;
		 IF NULL E THEN NIL
		  ELSE IF LENGTH E<(M := L) THEN PRINCN(NIL . E)
		  ELSE GO TO A
		END;
	     PRINC '! >>
   END;


% 11/18/82 rrk - Infinite loop caused by calls to this function with an
%  id as the ATMLST instead of a list.  A quick patch to turn the single
%  id into a list is provided, eliminating the infinite loop.
SYMBOLIC PROCEDURE REMPROPSS(ATMLST,LST);
<< IF NOT PAIRP ATMLST THEN
    ATMLST := LIST (ATMLST);
   WHILE ATMLST DO
   <<WHILE LST DO <<REMPROP(CAR ATMLST,CAR LST); LST:=CDR LST>>;
     ATMLST:=CDR ATMLST>> >>;

SYMBOLIC PROCEDURE REMFLAGSS(ATMLST,LST);
	WHILE LST DO <<REMFLAG(ATMLST,CAR LST); LST:=CDR LST>>;

CommentOutCode <<	% These are defined EXPRs in PSL
SMACRO PROCEDURE REMFLAG1(U,V); REMFLAG(LIST U,V);

SMACRO PROCEDURE FLAG1(U,V); FLAG(LIST U,V);
>>;

SYMBOLIC PROCEDURE FORMFEED;
	IF !*FORMFEED THEN EJECT()
	 ELSE <<TERPRI();
		PRIN2 " ========================================= ";
		TERPRI()>>;

