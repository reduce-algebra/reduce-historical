%Patterns for Lisp to Pascal compilation.  
% Taken from  FORTRAN version
%"system" lisp to Fortran work: "SYSASM.PAT".
%
%Version of 4:23pm  Monday, 13 October 1980.

LISP$ OFF ECHO$ OFF RAISE$

OFF COMP;
ON SYSLISP;

% Very optimized with inline consts, etc.

RULEBLOCK (PAS2,

'(!*ENTRY &1 &2 &3)->
  (BEGIN
	NALLOC:=0;
	W "(*  ",&2," ",&1," *)"$
	W "procedure ",MAPFUN &1,";"$
	DCLRLABELS();		%Declare the labels generated for this routine.
	W "begin";
  RETURN T END),

% Exit VS end of procedure?  Works now since we suppress !*LINKE.
'(!*EXIT)->	
  (BEGIN
	W "end;";
  RETURN T END),

'(!*ALLOC 0)->		T,

'(!*ALLOC 1)->
  (BEGIN
	W "    alloc1;" $
	NALLOC:=1;
  RETURN T END),

'(!*ALLOC 2)->
  (BEGIN
	W "    alloc2;" $
	NALLOC:=2;
  RETURN T END),

'(!*ALLOC 3)->
  (BEGIN
	W "    alloc3;" $
	NALLOC:=3;
  RETURN T END),

'(!*ALLOC &1)->
  (BEGIN
	W "    alloc(",&1,");" $
	NALLOC:=&1;
  RETURN T END),

'(!*DEALLOC 0)->
	<<NALLOC:=0;T>>,

'(!*DEALLOC 1)->
	<<NALLOC:=0;
	  W "      dealloc1;" $
	  T>>,

'(!*DEALLOC 2)->
	<<NALLOC:=0;
	  W "      dealloc2;" $
	T>>,

'(!*DEALLOC 3)->
	<<NALLOC:=0;
	  W "      dealloc3;" $
	T>>,

'(!*DEALLOC &1)->
	<<NALLOC:=0;
	IF &1 NEQ 0 THEN W "      dealloc(",&1,");" $
	T>>,

'(!*LINK &1 &2 &3)->		
  (BEGIN SCALAR X$
	IF X:=GET(&1,'OPENCOD) THEN
	<<% Has OPENCOD form, no retadr needed
	    WLST X$
	    RETURN T$
	>>
	ELSE
	<<
	    W "     ",MAPFUN &1,";";	% simply invoke as proc;
	    RETURN T$
	>>$
 END),

% Suppress LINKE by using ON NOLINKE;
%'(!*LINKE &1 &2 &3 &4)->  NOTHING!

'(!*LOAD 1 0)->
	<<W "      load10;";
	  T>>,

'(!*LOAD &1 &2)->
	(BEGIN SCALAR Y;
	IF &1 NEQ &2 THEN Y:=LOADIT(&1,&2)$   %LOADIT may emit some code.
	IF (REGNAM &1) NEQ Y THEN
	    IF NUMBERP(&1) AND NUMBERP(&2) AND (&2 <= 0) THEN
		W "      load(", &1 , "," , -&2 , ");"
	    ELSE
		W "      ",REGNAM &1," := ",Y,";" $
	RETURN T END),

'(!*MOVE &1 &2) -> % Need to FIX so RXX not used as much.  If no YY then
  (BEGIN SCALAR V1,V2;
	IF &1 EQ &2 THEN RETURN T$
	IF(V1:=EASYSTORE(&1)) THEN
          RETURN <<STOREIT('XX,&2,V1);T>>$
        V2:=LOADIT('XX,&2);
        V1:=LOADIT('YY,&1);
	W "       ",V1," := ",V2,";"$
   RETURN T END),

%**********   Delete--not needed?
%'(!*PUTARR &1 &2 &3) ->
% (BEGIN SCALAR V1,V2;
%	V1:=LOADIT('XX,&2);
%	V2:=LOADIT('YY,&3);
%	W "       ",&1,"(",V1,")=",V2$
%  RETURN T END),
%**********

'(!*STORE 1 0)->
	<<W "      store10;";
	  T>>,

'(!*STORE &1 (FLUID &2))->	PAS2 LIST('!*STORE,&1,LIST('GLOBAL,&2)),

'(!*STORE &1 (GLOBAL &2))->
  (BEGIN SCALAR V;
	IF !*SYSLISP THEN
	    W "      ",WSYSEVAL &2,":=",REGNAM &1,";"
	ELSE
	<<  V :=FNDID &2;
	    W "      idspace[",V,"].val := ",REGNAM &1,";">>$
  RETURN T END),

'(!*STORE NIL &1)->
	<< W "      storenil(", -&1 , ");" ;
	   T>>,

'(!*STORE &1 &2)->
	<<IF NUMBERP(&1) AND NUMBERP(&2) AND (&2 <=0 ) THEN
	    W "      store(", &1 , "," , -&2 , ");"
	  ELSE
	    W "      stk[st",&2,"] := ",REGNAM &1,";"$
	  T>>,

'(!*LBL &1)->	<<W MAPLBL &1,": "$ T>>,

'(!*JUMP &1)->	<<W "      GOTO ",MAPLBL &1,";"$ T>>,

%Delete? --> MAP to CASE?/MLG
'(!*JUMPTABLE &1)->
   <<	W "       JMPIT=R[1]+1"$
	W "       IF((JMPIT.LE.0).OR.(R[1].GE.",LENGTH &1,"))GOTO ",MAPLBL CAR &1;
	WX "      GOTO(",LBLLST CDR &1,")JMPIT"$ T>>,

'(!*JUMPE &1 &2)->
  (BEGIN SCALAR V;
	V:=LOADIT('XX,&2)$
	W "      IF R[1]=",V," THEN GOTO ",MAPLBL &1,";"$
  RETURN T END),

'(!*JUMPN &1 &2)->
  (BEGIN SCALAR V;
	V:=LOADIT('XX,&2)$
	W "      IF R[1] <> ",V," THEN GOTO ",MAPLBL &1,";"$
  RETURN T END),

'(!*JUMPWEQ &1 &2)->
  (BEGIN SCALAR V;
	V:=LOADIT('XX,&2)$
	W "      IF R[1]=",V," THEN GOTO ",MAPLBL &1,";"$
  RETURN T END),

'(!*JUMPWNE &1 &2)->
  (BEGIN SCALAR V;
	V:=LOADIT('XX,&2)$
	W "      IF info_of(R[1]) <> info_of(",V,") THEN GOTO ",MAPLBL &1,";"$
  RETURN T END),

'(!*JUMPWG &1 &2)->
  (BEGIN SCALAR V;
	V:=LOADIT('XX,&2)$
	W "      IF info_of(R[1]) > info_of(",V,") THEN GOTO ",MAPLBL &1,";"
  RETURN T END),

'(!*JUMPWGE &1 &2)->
  (BEGIN SCALAR V;
	V:=LOADIT('XX,&2)$
	W "      IF info_of(R[1]) >= info_of(",V,") THEN GOTO ",MAPLBL &1,";"
  RETURN T END),

'(!*JUMPWL &1 &2)->
  (BEGIN SCALAR V;
	V:=LOADIT('XX,&2)$
	W "      IF info_of(R[1]) < info_of(",V,") THEN GOTO ",MAPLBL &1,";"
  RETURN T END),

'(!*JUMPWLE &1 &2)->
  (BEGIN SCALAR V;
	V:=LOADIT('XX,&2)$
	W "      IF info_of(R[1]) <= info_of(",V,") THEN GOTO ",MAPLBL &1,";" $
  RETURN T END),

'(!*JUMPT &1)->
  <<W "      IF R[1] <> nilref THEN GOTO ",MAPLBL &1,";"; T>>,

'(!*JUMPNIL &1)->
  <<W "      IF R[1] = nilref THEN GOTO ",MAPLBL &1,";"; T>>,

% !*TEST stuff has been replaced by !*JUMPC and !*JUMPNC stuff.
% Form is (!*JUMPC LABL REG TYPE)
'(!*JUMPNC &1 &2 ATOM)->PAS2 LIST('!*JUMPC,&1,&2,'PAIRTAG),

'(!*JUMPC &1 &2 ATOM)->	PAS2 LIST('!*JUMPNC,&1,&2,'PAIRTAG),

'(!*JUMPC &1 &2 NUMTAG)->
  <<W "      IF (tag_of(",REGNAM &2,") = INTTAG)"$
    W "       or (tag_of(",REGNAM &2,") = FIXTAG) THEN GOTO ",MAPLBL &1,";" $
    T>>,

'(!*JUMPNC &1 &2 NUMTAG)->
  <<W "      IF not((tag_of(",REGNAM &2,") = INTTAG)"$
    W "       or (tag_of(",REGNAM &2,") = FIXTAG)) THEN GOTO ",MAPLBL &1,";" $
    T>>,

'(!*JUMPC &1 &2 &3)->
  <<W "      IF tag_of(",REGNAM &2,") = ",&3," THEN GOTO ",MAPLBL &1,";" $
    T>>,

'(!*JUMPNC &1 &2 &3)->
  <<W "      IF tag_of(",REGNAM &2,") <> ",&3," THEN GOTO ",MAPLBL &1,";" $
    T>>,

'(!*FREERSTR &1)->	<<W "      UNBIND(",LENGTH &1,");"$T>>,

'(!*PROGBIND &1)->	
  (BEGIN SCALAR Y$
	FOR EACH X IN &1 DO
	 <<FNDID CAR X$
	W "      PBIND(",-CADR X,!, ,V,");" $T>>$
  RETURN T END),

'(!*LAMBIND &1 &2)->	
  (BEGIN SCALAR X,Y$
	X:=&1$ Y:=&2$
	WHILE X DO
	 <<FNDID CAAR Y$
	   W "      LBIND(",REGNAM CAR X,!,,-CADAR Y,!,,V,");"$
	   X:=CDR X$ Y:=CDR Y>>$
  RETURN T END),

'( &1 &2 BASE &3 WORDS &4 LEFT )-> T,

'(!*CHECK &1 &2 &3) ->
  <<W "       IF tag_of(",REGNAM &1,") <> ",&2,"THEN GOTO ",MAPLBL &3,";"$ T>>,

'(!*CODE &1) -> <<W &1; T>>,

'(!*EVAL &1) -> <<EVAL &1; T>>,

&1->	<<WX "1*** Unknown ",&1," ***** "$T>> )$


PUT('CAAR,'CARCDRFN,'(CAR . CAR))$
PUT('CDAR,'CARCDRFN,'(CDR . CAR))$
PUT('CADR,'CARCDRFN,'(CAR . CDR))$
PUT('CDDR,'CARCDRFN,'(CDR . CDR))$
PUT('CAAAR,'CARCDRFN,'(CAAR . CAR))$
PUT('CADAR,'CARCDRFN,'(CADR . CAR))$
PUT('CAADR,'CARCDRFN,'(CAAR . CDR))$
PUT('CADDR,'CARCDRFN,'(CADR . CDR))$
PUT('CDAAR,'CARCDRFN,'(CDAR . CAR))$
PUT('CDDAR,'CARCDRFN,'(CDDR . CAR))$
PUT('CDADR,'CARCDRFN,'(CDAR . CDR))$
PUT('CDDDR,'CARCDRFN,'(CDDR . CDR))$
PUT('CAAAAR,'CARCDRFN,'(CAAAR . CAR))$
PUT('CAADAR,'CARCDRFN,'(CAADR . CAR))$
PUT('CAAADR,'CARCDRFN,'(CAAAR . CDR))$
PUT('CAADDR,'CARCDRFN,'(CAADR . CDR))$
PUT('CADAAR,'CARCDRFN,'(CADAR . CAR))$
PUT('CADDAR,'CARCDRFN,'(CADDR . CAR))$
PUT('CADADR,'CARCDRFN,'(CADAR . CDR))$
PUT('CADDDR,'CARCDRFN,'(CADDR . CDR))$
PUT('CDAAAR,'CARCDRFN,'(CDAAR . CAR))$
PUT('CDADAR,'CARCDRFN,'(CDADR . CAR))$
PUT('CDAADR,'CARCDRFN,'(CDAAR . CDR))$
PUT('CDADDR,'CARCDRFN,'(CDADR . CDR))$
PUT('CDDAAR,'CARCDRFN,'(CDDAR . CAR))$
PUT('CDDDAR,'CARCDRFN,'(CDDDR . CAR))$
PUT('CDDADR,'CARCDRFN,'(CDDAR . CDR))$
PUT('CDDDDR,'CARCDRFN,'(CDDDR . CDR))$


% Some of the OPEN coded functions;
% Take a LIST of strings, operating on R[1],R[2],...;


PUT('!*INF,'OPENCOD,'("      mkitem(INTTAG,info_of(R[1]),R[1]);"));
PUT('!*TAG,'OPENCOD,'("      mkitem(INTTAG,tag_of(R[1]),R[1]);"));

PUT('!*MKITEM,'OPENCOD,'("      mkitem(tag_of(R[1]),info_of(R[2]),R[1]);"));
PUT('!*INTINF,'OPENCOD,'("      mkitem(INTTAG,info_of(R[1]),R[1]);"));

%Only appropriate for systems lisp.  Solution used here is questionable.
PUT('!*WPLUS2,'OPENCOD,'("       R[1].info:=R[1].info+R[2].info;"));
PUT('!*WDIFFERENCE,'OPENCOD,'("       R[1].info:=R[1].info-R[2].info;"));
PUT('!*WADD1,'OPENCOD,'("       R[1].info:=R[1].info+1;"));
PUT('!*WSUB1,'OPENCOD,'("       R[1].info:=R[1].info-1;"));
PUT('!*WMINUS,'OPENCOD,'("       R[1].info:=-R[1].info;"));
PUT('!*WTIMES2,'OPENCOD,'("       R[1].info:=R[1].info*R[2].info;"));
PUT('!*WQUOTIENT,'OPENCOD,'("       R[1].info:=R[1].info div R[2].info;"));
PUT('!*WREMAINDER,'OPENCOD,'("       R[1].info:=R[1].info mod R[2].info;"));

%NEED support functions for these!
PUT('!*WAND,'OPENCOD,'("       R[1].info:=land(R[1].info, R[2].info);"));
PUT('!*WOR,'OPENCOD, '("       R[1].info:=lor(R[1].info, R[2].info);"));
PUT('!*WXOR,'OPENCOD,'("       R[1].info:=lxor(R[1].info, R[2].info);"));
PUT('!*WNOT,'OPENCOD,'("       R[1].info:=not R[1].info;"));

END$
