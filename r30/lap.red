COMMENT MODULE LAP;

SYMBOLIC;


COMMENT definition of LAP ops;

SYMBOLIC FEXPR PROCEDURE MACOPS L; 
   BEGIN 
    A: 
      IF NULL L THEN RETURN T; 
      PUT(CAR L,'MACOP,CADR L); 
      L := CDDR L; 
      GO TO A
   END;

MACOPS(PUSHJ,
       176,
       POPJ,
       179,
       PUSH,
       177,
       POP,
       178,
       CALL,
       28,
       JCALL,
       29,
       CALLF,
       30,
       JCALLF,
       31,
       JRST,
       172,
       JSP,
       181,
       CALLF!@,
       15376,
       JCALLF!@,
       15888,
       MOVE,
       128,
       MOVEI,
       129,
       MOVEM,
       130,
       HRRZS,363,
       MOVNI,
       137,
       HLLZS,331,
       CAIE,
       194,
       CAIN,
       198,
       CAME,
       202,
       CAMGE,
       205,
       CAMLE,
       203,
       CAMN,
       206,
       ADD,
       184,
       SUB,
       188,
       IMUL,
       144,
       CLEARM,
       258,
       CLEARB,
       259,
       EXCH,
       168,
       TDZA,
       412,
       JUMP,
       208,
       JUMPE,
       210,
       JUMPN,
       214,
       HRRZ,
       360,
       HLRZ,
       364,
       HRRM,
       354,
       HRLM,
       326,
       HRLI,
       325,
       HRRZ!@,
       184336,
       HLRZ!@,
       186384,
       HRRM!@,
       181264,
       HRLM!@,
       166928,
       HRRZS!@,
       185872,
       HLLZS!@,
       169488,
       JUMPGE,
       213);

MACOPS(NIL,0,A,1,B,2,C,3,TT,7,D,10,R,11,P,12,SP,15);

MACOPS(CARA,
       364,
       CARA!@,
       186384,
       CDRA,
       360,
       CDRA!@,
       184336,
       RPLCA,
       326,
       RPLCA!@,
       166928,
       RPLCD,
       354,
       RPLCD!@,
       181264,
       JSYS,
       68);

MACOPS(SETO,
       316,
       MOVSI,
       133,
       ILDB,
       92,
	IDPB,
	94,
       TRZ,
       400,
       HRRI,
       353,
       HRROI,
       369,
       HRL,
       324,
       HRRZ,
       360,
       TRO,
       432,
       ADDI,
       185,
       AOBJN,
       171,
       CAIL,
       193,
       SKIPA,
       220,
       SKIPE,
       218,
       SETZM,
       258,
       BLT,
       169,
       SUBI,
       189,
       AOJN,
       230,
       SKIPG,
       223,
       LDB,
       93,
       AOJA,
       228,
       SOJA,
       244,
       CAIG,
       199,
       CAILE,
       195,
       LSH,
       162,
       IORM,
       286,
       HRLZ,
       332,
       HRLZM,
       334,
       SOJE,
       242,
       SOJN,
       246,
       DPB,
       95,
       ANDI,
       261);


FLUID '(BPORG BPEND CLIST QLIST);

FLUID '(!*PWRDS
          !*PGWD
          !*SAVECOM
          CONLIST
          GEN
          REMSYMS);

SYMBOLIC PROCEDURE LAP U; LAP10 U;

SYMBOLIC PROCEDURE LAP10 U; 
   BEGIN SCALAR SL,LOC,CONLIST,GEN,REMSYMS,X; 
      GEN := GENSYM();   %entry point for constants;
      CONLIST := LIST NIL; %constant list;
      LOC := BPORG;  %entry point for function;
      WHILE U DO
	<<IF ATOM(X := CAR U)
	    THEN <<IF !*PGWD THEN PRINT X; DEFSYM(X,BPORG)>>
	 ELSE IF CAR X EQ '!*ENTRY 
	  THEN <<IF SL THEN RPLACD(CDAR SL,BPORG);
		 SL := LIST(CDR X,BPORG) . SL;
		 LOC := BPORG;
		 IF !*COUNTMC
		  THEN RPLACD(U,APPEND(
		   <<PUT(CAR X,'MCCOUNT,ADD1 GET(CAR X,'MCCOUNT));
		     COUNTMC CAR X>>,CDR U));
		 IF !*PGWD THEN PRINT X>>
	 ELSE IF CADR X MEMBER '(EXPR FEXPR)
	  THEN <<IF SL THEN RPLACD(CDAR SL,BPORG);
		 SL := LIST(X,BPORG) . SL;
		 LOC := BPORG;
		 IF !*PGWD THEN PRINT X>>
	 ELSE IF NOT NUMBERP CAR X AND FLAGP(CAR X,'MC)
	  THEN RPLACD(U,APPEND(IF !*COUNTMC THEN 
	   <<PUT(CAR X,'MCCOUNT,ADD1 GET(CAR X,'MCCOUNT));
		COUNTMC CAR X>>,
			  APPEND(EVAL(CAR X .
				  FOR EACH J IN CDR X COLLECT MKQUOTE J),
				  CDR U)))
	 ELSE <<DEPOSIT(BPORG,KWD X);
		IF (BPORG := BPORG+1)>BPEND
		  THEN REDERR "BINARY PROGRAM SPACE EXCEEDED">>;
       U := CDR U>>;
      IF SL THEN <<RPLACD(CDAR SL,BPORG);
		   SL := REVERSIP SL;
		   IF !*PWRDS THEN FOR EACH X IN SL DO
				LPRIM LIST(CAAR X,CADR X,'BASE,
					   CDDR X-CADR X,
					   'WORDS,BPEND-CDDR X,'LEFT)>>;
      DEFSYM(GEN,BPORG);  %define entry point for constants;
      WHILE CONLIST := CDR CONLIST DO 
         <<CLIST := (CAR CONLIST . BPORG) . CLIST; 
           DEPOSIT(BPORG,KWD CAR CONLIST); 
           IF (BPORG := BPORG+1)>BPEND 
	     THEN REDERR "BINARY PROGRAM SPACE EXCEEDED">>; 
      FOR EACH X IN REMSYMS DO REMSYM X;
      IF !*SAVECOM
	THEN FOR EACH X IN SL DO 
	   <<REMD CAAR X;
	     !%PUTD(CAAR X,CADAR X,MKCODE(CADR X,CADDAR X))>>;
   END;

SYMBOLIC PROCEDURE KWD U; 
   BEGIN SCALAR X;
      X := GWD U;
      IF !*PGWD
	THEN BEGIN INTEGER N;
	   PRIN1 U;
	   SPACES2 30;
	   N := BASE;
	   BASE := 7+1;
	   PRINT(IF X < 0 THEN X + 68719476736 ELSE X);
	   BASE := N
	 END;
      RETURN X
   END;

SYMBOLIC PROCEDURE SPACES2 N;
   BEGIN SCALAR M;
      M := N-POSN();
      IF M<1 THEN PRIN2 " "
       ELSE WHILE M>0 DO <<PRIN2 " "; M := M-1>>
   END;


% PRINT MACROS FIRST, IF T; 

!*PWRDS := T;

% PRINT SPACE-USAGE, IF T; 

!*PGWD := NIL;

% PRINT EXPANDED CODE IF T; 

!*SAVECOM := T;

% ACTUALLY LOAD IF T; 

!*SAVEDEF := NIL;

% RETAIN EXPR/FEXPR IF T; 

QSET('QLIST,NIL);

QSET('CLIST,NIL);

SYMBOLIC PROCEDURE GWD X; 
   BEGIN SCALAR WRD,FLD; 
      WRD := LAPEVAL CAR X;
      WRD := LSH(WRD,IF WRD<512 THEN 27 ELSE 18); 
      FLD := '((23 . 15) (0 . 262143) (18 . -1)); 
      MAPC(CDR X,
	   FUNCTION LAMBDA ZZ; 
                         <<WRD := 
                            WRD
                              + LSH(BOOLE(1,CDAR FLD,LAPEVAL ZZ),
                                    CAAR FLD); 
                           FLD := CDR FLD>>);
      RETURN WRD
   END;

SYMBOLIC PROCEDURE RELOC L; LAPEVAL CAR L + 96;

SYMBOLIC PROCEDURE LAPEVAL X; 
   IF NUMBERP X THEN X
    ELSE IF ATOM X THEN GVAL X
    ELSE IF CAR X MEMBER '(E QUOTE)
     THEN !*BOX IF (NOT ATOM (X := CADR X)
                      OR NUMBERP X AND NOT INUMP X)
                     OR STRINGP X
                  THEN BEGIN SCALAR Y; 
                          Y := QLIST; 
                        A: 
                          IF NULL Y
                            THEN RETURN CAR (QLIST := X . QLIST)
                           ELSE IF X=CAR Y
                                     AND FLOATP X EQ FLOATP CAR Y
                            THEN RETURN CAR Y; 
                          Y := CDR Y; 
                          GO TO A
                       END
                 ELSE X
    ELSE IF CAR X EQ 'FLUID OR CAR X EQ 'SPECIAL
     THEN <<QSET(CADR X,NIL);
            !*BOX GET(CADR X,'VALUE)>>
    ELSE IF CAR X EQ 'C
     THEN BEGIN SCALAR N,CPTR; 
             CPTR := CLIST; 
           L11: 
             IF NULL CPTR THEN GO TO L12
              ELSE IF CDR X=CAAR CPTR THEN RETURN CDAR CPTR; 
             CPTR := CDR CPTR; 
             GO TO L11; 
           L12: 
             GVAL GEN; 
             N := 0; 
             CPTR := CONLIST; 
           A: 
             IF NULL CDR CPTR THEN RPLACD(CPTR,LIST CDR X); 
             IF CDR X=CADR CPTR THEN RETURN N; 
             N := N + 1; 
             CPTR := CDR CPTR; 
             GO TO A
          END
    ELSE IF CAR X EQ 'RELOC THEN LAPEVAL CADR X + 96
    ELSE IF CAR X EQ 'EXARG AND NOT ATOM CDR X
     THEN LAPEVAL 'EXARG + LAPEVAL CADR X
    ELSE LAPEVAL CAR X + LAPEVAL CDR X;

SYMBOLIC PROCEDURE DEFSYM(SYM,VAL); 
   BEGIN SCALAR Z; 
      IF Z := GET(SYM,'UNDEF) THEN GO TO PATCH; 
      REMSYMS := SYM . REMSYMS; 
    A: 
      RETURN PUT(SYM,'SYM,VAL); 
    PATCH: 
      IF NULL Z THEN <<REMPROP(SYM,'UNDEF); GO TO A>>; 
      DEPOSIT(CAR Z,EXAMINE CAR Z + VAL); 
      Z := CDR Z; 
      GO TO PATCH
   END;

SYMBOLIC PROCEDURE GVAL SYM; 
   BEGIN SCALAR X; 
      IF X := GET(SYM,'MACOP) THEN RETURN X
       ELSE IF X := GET(SYM,'SYM) THEN RETURN X
       ELSE IF GET(SYM,'VALUE) THEN RETURN !*BOX SYM; 
      PUT(SYM,
          'UNDEF,
          BPORG
            . IF X := GET(SYM,'UNDEF) THEN X
               ELSE <<REMSYMS := SYM . REMSYMS; NIL>>); 
      RETURN 0
   END;

SYMBOLIC PROCEDURE REMSYM L; 
   IF GET(L,'UNDEF) THEN LPRIE LIST(L,"UNDEFINED SYMBOL")
    ELSE IF NULL REMPROP(L,'SYM)
     THEN LPRIE LIST(L,"MULTIPLY DEFINED")
    ELSE IF CAADR L EQ 'PNAME THEN REMOB L   %means L has no props;
    ELSE NIL;

BPORG1 := BPORG;

LAP10 '((GWD EXPR 1)
        (PUSH P (C 0))
        (PUSH P 1)
        (PUSHJ P TAG04)
        (CAIG 1 511)
        (LSH 1 9)
        (HLRZ 2 1)
        (HRRZ 3 1)
        (CAIN 2 34816)
        (CAIL 3 512)
        (JRST 0 TAG01)
        (MOVEM 1 -1 P)
        (JUMPN 3 TAG02)
        TAG01
        (HRLZM 1 -1 P)
        (PUSHJ P TAG04)
        (ANDI 1 15)
        (LSH 1 23)
        (IORM 1 -1 P)
        (PUSHJ P TAG04)
        (HRRM 1 -1 P)
        (PUSHJ P TAG04)
        (HRLZ 1 1)
        (IORM 1 -1 P)
        TAG02
        (POP P 1)
        (POP P 1)
        (JCALL 1 (E !*BOX))
        TAG03
        (POP P 1)
        (JRST 0 TAG02)
        TAG04
        (MOVE 2 -1 P)
        (JUMPE 2 TAG03)
        (CARA 1 0 2)
        (CDRA 2 0 2)
        (MOVEM 2 -1 P)
        (CALL 1 (E LAPEVAL))
        (JCALL 1 (E NUMVAL)));

CLIST := NIL;

IF BPEND<131072 THEN BPORG := BPORG1;   %means DECUS version;


END;
