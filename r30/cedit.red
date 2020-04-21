COMMENT REDUCE INPUT STRING EDITOR;

GLOBAL '(CRBUF!* CRBUF1!* CRBUFLIS!* ESC!* STATCOUNTER RPRIFN!* RTERFN!*
         !$EOL!$ !*EAGAIN !*FULL);

!*EAGAIN := NIL;

%ESC!* := INTERN ASCII 125;   %this is system dependent and defines
                              %a terminator for strings;

SYMBOLIC PROCEDURE RPLACW(U,V);
   IF ATOM U OR ATOM V THEN ERRACH LIST('RPLACW,U,V)
    ELSE RPLACD(RPLACA(U,CAR V),CDR V);

SYMBOLIC PROCEDURE CEDIT N;
   BEGIN SCALAR X,OCHAN;
      OCHAN := WRS NIL;
      IF N EQ 'FN THEN X := REVERSIP CRBUF!*
       ELSE IF NULL N
        THEN IF NULL CRBUFLIS!*
               THEN <<STATCOUNTER := STATCOUNTER-1;
                      REDERR "No previous entry">>
              ELSE X := CDAR CRBUFLIS!*
       ELSE IF (X := ASSOC(CAR N,CRBUFLIS!*)) THEN X := CDR X
       ELSE <<STATCOUNTER := STATCOUNTER-1;
              REDERR LIST("Entry",CAR N,"not found")>>;
      CRBUF!* := NIL;
      X := FOR EACH J IN X COLLECT J;   %to make a copy;
      TERPRI();
      EDITP X;
      TERPRI();
      X := CEDIT1 X;
      WRS OCHAN;
      IF X EQ 'FAILED THEN NIL ELSE CRBUF1!* := X
   END;

GLOBAL '(!*BLANKNOTOK!*);

SYMBOLIC PROCEDURE CEDIT1 U;
   BEGIN SCALAR X,Y,Z;
      Z := SETPCHAR '!>;
      IF NOT !*EAGAIN
        THEN <<PRIN2T "For help, type ?"; !*EAGAIN := T>>;
      WHILE U AND (CAR U EQ !$EOL!$) DO U := CDR U;
      U := APPEND(U,LIST '! );   %to avoid 'last char' problem;
      IF !*FULL THEN EDITP U;
    TOP:
      X := U;   %current pointer position;
    A:
      Y := READCH();   %current command;
      IF Y EQ 'P OR Y EQ 'p THEN EDITP X
       ELSE IF Y EQ 'I OR Y EQ 'i THEN EDITI X
       ELSE IF Y EQ 'C OR Y EQ 'c THEN EDITC X
       ELSE IF Y EQ 'D OR Y EQ 'd THEN EDITD X
       ELSE IF Y EQ 'F OR Y EQ 'f THEN X := EDITF(X,NIL)
       ELSE IF Y EQ 'E OR Y EQ 'e
        THEN <<TERPRI(); EDITP1 U; SETPCHAR Z; RETURN U>>
       ELSE IF Y EQ 'Q OR Y EQ 'q THEN <<SETPCHAR Z; RETURN 'FAILED>>
       ELSE IF Y EQ '!? THEN EDITH X
       ELSE IF Y EQ 'B OR Y EQ 'b THEN GO TO TOP
       ELSE IF Y EQ 'K OR Y EQ 'k THEN EDITF(X,T)
       ELSE IF Y EQ 'S OR Y EQ 's THEN X := EDITS X
       ELSE IF Y EQ '!  AND NOT !*BLANKNOTOK!* OR Y EQ 'X OR Y EQ 'x
        THEN X := EDITN X
       ELSE IF Y EQ '!  AND !*BLANKNOTOK!* THEN GO TO A
       ELSE IF Y EQ !$EOL!$ THEN GO TO A
       ELSE LPRIM!* LIST(Y,"Invalid editor character");
      GO TO A
   END;

SYMBOLIC PROCEDURE EDITC X;
   IF NULL CDR X THEN LPRIM!* "No more characters"
    ELSE RPLACA(X,READCH());

SYMBOLIC PROCEDURE EDITD X;
   IF NULL CDR X THEN LPRIM!* "No more characters"
    ELSE RPLACW(X,CADR X . CDDR X);

SYMBOLIC PROCEDURE EDITF(X,BOOL);
   BEGIN SCALAR Y,Z;
      Y := CDR X;
      Z := READCH();
      IF NULL Y THEN RETURN <<LPRIM!* LIST(Z,"Not found"); X>>;
      WHILE CDR Y AND NOT Z EQ CAR Y DO Y := CDR Y;
      RETURN IF NULL CDR Y THEN <<LPRIM!* LIST(Z,"Not found"); X>>
                ELSE IF BOOL THEN RPLACW(X,CAR Y . CDR Y)
                ELSE Y
   END;

SYMBOLIC PROCEDURE EDITH X;
   <<PRIN2T "THE FOLLOWING COMMANDS ARE SUPPORTED:";
     PRIN2T "   B              move pointer to beginning";
     PRIN2T "   C<character>   replace next character by <character>";
     PRIN2T "   D              delete next character";
     PRIN2T "   E              end editing and reread text";
     PRIN2T
    "   F<character>   move pointer to next occurrence of <character>";
     PRIN2T
       "   I<string><escape>   insert <string> in front of pointer";
     PRIN2T "   K<character>   delete all chars until <character>";
     PRIN2T "   P              print string from current pointer";
     PRIN2T "   Q              give up with error exit";
     PRIN2T
       "   S<string><escape> search for first occurrence of <string>";
     PRIN2T "                      positioning pointer just before it";
     PRIN2T "   <space> or X   move pointer right one character";
     TERPRI();
     PRIN2T
       "ALL COMMAND SEQUENCES SHOULD BE FOLLOWED BY A CARRIAGE RETURN";
     PRIN2T "    TO BECOME EFFECTIVE">>;

SYMBOLIC PROCEDURE EDITI X;
   BEGIN SCALAR Y,Z;
      WHILE (Y := READCH()) NEQ ESC!* DO Z := Y . Z;
      RPLACW(X,NCONC(REVERSIP Z,CAR X . CDR X))
   END;

SYMBOLIC PROCEDURE EDITN X;
   IF NULL CDR X THEN LPRIM!* "NO MORE CHARACTERS"
    ELSE CDR X;

SYMBOLIC PROCEDURE EDITP U;
   <<EDITP1 U; TERPRI()>>;

SYMBOLIC PROCEDURE EDITP1 U;
   FOR EACH X IN U DO IF X EQ !$EOL!$ THEN TERPRI() ELSE PRIN2 X;

SYMBOLIC PROCEDURE EDITS U;
   BEGIN SCALAR X,Y,Z;
      X := U;
      WHILE (Y := READCH()) NEQ ESC!* DO Z := Y . Z;
      Z := REVERSIP Z;
  A:  IF NULL X THEN RETURN <<LPRIM!* "not found"; U>>
       ELSE IF EDMATCH(Z,X) THEN RETURN X;
      X := CDR X;
      GO TO A
   END;

SYMBOLIC PROCEDURE EDMATCH(U,V);
   %matches list of characters U against V. Returns rest of V if
   %match occurs or NIL otherwise;
   IF NULL U THEN V
    ELSE IF NULL V THEN NIL
    ELSE IF CAR U=CAR V THEN EDMATCH(CDR U,CDR V)
    ELSE NIL;

SYMBOLIC PROCEDURE LPRIM!* U; <<LPRIM U; TERPRI()>>;

COMMENT Editing Function Definitions;

REMPROP('EDITDEF,'STAT);

SYMBOLIC PROCEDURE EDITDEF U; EDITDEF1 CAR U;

SYMBOLIC PROCEDURE EDITDEF1 U;
   BEGIN SCALAR TYPE,X;
      IF NULL(X := GETD U) THEN RETURN LPRIM LIST(U,"not defined")
       ELSE IF CODEP CDR X OR NOT EQCAR(CDR X,'LAMBDA)
        THEN RETURN LPRIM LIST(U,"cannot be edited");
      TYPE := CAR X;
      X := CDR X;
      IF TYPE EQ 'EXPR THEN X := 'DE . U . CDR X
       ELSE IF TYPE EQ 'FEXPR THEN X := 'DF . U . CDR X
       ELSE IF TYPE EQ 'MACRO THEN X := 'DM . U . CDR X
       ELSE REDERR LIST("strange function type",TYPE);
      RPRIFN!* := 'ADD2BUF;
      RTERFN!* := 'ADDTER2BUF;
      CRBUF!* := NIL;
      X := ERRORSET(LIST('RPRINT,MKQUOTE X),T,NIL);
      RPRIFN!* := NIL;
      RTERFN!* := NIL;
      IF ERRORP X THEN RETURN (CRBUF!* := NIL);
      CRBUF!* := CEDIT 'FN;
      RETURN NIL
   END;

SYMBOLIC PROCEDURE ADD2BUF U; CRBUF!* := U . CRBUF!*;

SYMBOLIC PROCEDURE ADDTER2BUF; CRBUF!* := !$EOL!$ . CRBUF!*;

PUT('EDITDEF,'STAT,'RLIS);

COMMENT Displaying past input expressions;

PUT('DISPLAY,'STAT,'RLIS);

SYMBOLIC PROCEDURE DISPLAY U;
  BEGIN SCALAR X;
      U := CAR U;
      X := CRBUFLIS!*;
      TERPRI();
      IF NOT NUMBERP U THEN U := LENGTH X;
      WHILE U>0 AND X DO
       <<PRIN2 CAAR X; PRIN2 ": "; EDITP CDAR X; TERPRI();
         X := CDR X; U := U-1>>;
  END;


END;
