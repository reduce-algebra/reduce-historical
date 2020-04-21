(!* 
"ZMACRO contains two macro packages --
    (1) YMACS -- basically useful macros and fexprs.
    (2) YSAIMACS -- macros used to simulate many SAIL constructs. ")

(!* 
" YMACS -- USEFUL MACROS AND FEXPRS (see also YSAIMAC)

*       ( X:any ): NIL                      MACRO
**      ( X:list )                          MACRO
NEQ     ( X:any Y:any ):boolean             MACRO
NEQN    ( X:any Y:any ):boolean             MACRO
NEQUAL  ( X:any Y:any ):boolean             MACRO
MAKE    ( variable template )               MACRO
SETQQ   ( variable value )                  MACRO
EXTEND  ( function series )                 MACRO
DREVERSE( list ):list                       MACRO
APPENDL ( lists )                           MACRO
NCONCL  ( lists )                           MACRO
NCONC1  ( lst exp1 ... expn ): any          MACRO
SELECTQ ( exp cases last-resort )           MACRO
WHILE   ( test body )                       MACRO
REPEAT  ( body test )                       MACRO
FOREACH ( var in/of lst do/collect exp )    MACRO
SAY     ( test expressions )                MACRO
DIVERT  ( channel expressions )             MACRO
CAT     ( list of any ):string              MACRO
CAT-ID  ( list of any ):<uninterned id>     MACRO
TTY     ( L:list ):NIL                      MACRO
TTY-TX  ( L:list ):NIL                      MACRO
TTY-XT  ( L:list ):NIL                      MACRO
TTY-TT  ( L:list ):NIL                      MACRO
ERRSET  ( expression label )                MACRO
GRAB    ( file )                            MACRO
GRABFNS ( ids file-dscr )                   MACRO
DUMP    ( file-dscr )                       MACRO
DUMPFNS ( ids file-dscr )                   MACRO

used to expand macros:
XP#SELECTQ (#L#)                            EXPR
XP#WHILE   (#BOOL #BODY)                    EXPR
XP#FOREACH (#VAR #MOD #LST #ACTION #BODY)   EXPR
XP#SAY1    ( expression )                   EXPR

")

(GLOBAL '(!#SELECTQ G!:SHOW!:ERRORS G!:SHOW!:TRACE))

(!* "In ZBOOT, not needed here."
(CDM !* (!#X) NIL)
)

(!* 
"*( X:any ): NIL                             MACRO
    ===> NIL
    For comments--doesn't evaluate anything.  Returns NIL.
    Note: expressions starting with * which are read by the
    lisp scanner must obey all the normal syntax rules.")

(!* 
"**( X:list )                                MACRO
    ===> (PROGN <lists>)
    For comments--all atoms are ignored, lists evaluated as in PROGN.")

(CDM !*!* (!#X) (CONS 'PROGN (ABSTRACT (FUNCTION PAIRP) (CDR !#X))))

(!* 
"NEQ( X:any Y:any ):boolean                  MACRO
    ===> (NOT (EQ X Y)) ")

(!* 
"Changed to CDM because NEQ in PSL means NOT EQUAL.  We hope to change
that situation, however.")

(CDM NEQ (!#X) (LIST 'NOT (CONS 'EQ (CDR !#X))))

(!* 
"NEQN( X:any Y:any ):boolean                 MACRO
    ===> (NOT (EQN X Y)) ")

(DM NEQN (!#X) (LIST 'NOT (CONS 'EQN (CDR !#X))))

(!* 
"NEQUAL( X:any Y:any ):boolean               MACRO
    ===> (NOT (EQUAL X Y)) ")

(DM NEQUAL (!#X) (LIST 'NOT (CONS 'EQUAL (CDR !#X))))

(!* 
"MAKE( variable template )                   MACRO
    ===> (SETQ <var> <some form using var>)
    To change the value of a variable depending upon template.
    Uses similar format for template as editor MBD.  There are 3 cases.

    1) template is numerical:
            (MAKE VARIABLE 3)
          = (SETQ VARIABLE (PLUS VARIABLE 3))

    2) Template is a series, whose first element is an atom:
            (MAKE VARIABLE ASSOC ITEM)
          = (SETQ VARIABLE (ASSOC ITEM VARIABLE))

    3) Otherwise, variable is substituted for occurrences of * in template.
            (MAKE VARIABLE (ASSOC (CADR *) (CDDR *))
          = (SETQ VARIABLE (ASSOC (CADR VARIABLE) (CDDR VARIABLE))")

(CDM MAKE (!#X)
 (PROGN (SETQ !#X (CDR !#X))
        (LIST 'SETQ
              (CAR !#X)
              (COND ((NUMBERP (CADR !#X)) (CONS 'PLUS !#X))
                    ((ATOM (CADR !#X)) (APPEND (CDR !#X) (LIST (CAR !#X))))
                    (T (SUBST (CAR !#X) '!* (CADR !#X)))))))

(!* 
"SETQQ( variable value )                     MACRO
    ===> (SETQ VARIABLE 'VALUE) ")

(CDM SETQQ (!#X) (LIST 'SETQ (CADR !#X) (MKQUOTE (CADDR !#X))))

(!* 
"EXTEND( function series )                   MACRO
    ===> (FN ELT1 (FN ELT2 ... (FN ELTn-1 ELTn)))
    Applies 2-place function to series, similarly to PLUS.
    E.g.: (EXTEND SETQ A B C D 5) = (SETQ A (SETQ B (SETQ C (SETQ D 5))))")

(CDM EXTEND (!#X) (EXPAND (CDDR !#X) (CADR !#X)))

(!* 
"DREVERSE( L: list ):list                    MACRO
    ===> (REVERSIP L)
    Synonym for REVERSIP.")

(DM DREVERSE (!#X) (CONS 'REVERSIP (CDR !#X)))

(!* 
"APPENDL( lists )                            MACRO
    ===> (APPEND LIST1 (APPEND LIST2 ....))
    EXPAND's APPEND to a list of arguments instead of just 2.")

(CDM APPENDL (!#X) (EXPAND (CDR !#X) 'APPEND))

(!* 
"NCONCL( lists )                             MACRO
    ===> (NCONC LST1 (NCONC LST2 ....))
    EXPAND's NCONC to a list of arguments instead of just 2.")

(CDM NCONCL (!#X) (EXPAND (CDR !#X) 'NCONC))

(!* 
"NCONC1( lst exp1 ... expn ): any            MACRO
    ===> (NCONC LST (LIST EXP1 ... EXPn))
    Destructively add exp1 ... exp-n to the end of lst.")

(CDM NCONC1 (!#X)
 (LIST 'NCONC (CADR !#X) (CONS 'LIST (CDDR !#X))))

(!* 
"SELECTQ( exp cases last-resort )            MACRO
    ===> (COND ...)
    Exp is a lisp expression to be evaluated.
    Each case-i is of the form (key-i exp1 exp2...expm).
    Last-resort is a lisp expression to be evaluated.

    Generates a COND statement:
        If key-i is an atom, case-i becomes the cond-pair:
           ((EQUAL exp key-i) (PROGN exp1 exp2 ... expm))
        If key-i is a list, case-i becomes the cond-pair:
           ((MEMBER exp key-i) (PROGN exp1 exp2 ... expm))
        Last-resort becomes the final cond-pair:
           (T last-resort)

    If exp is non-atomic, it should not be re-evaluated in each clause,
    so a dummy variable (#SELECTQ) is set to the value of exp in the
    first test and that dummy variable is used in all successive tests.

    Note:
    (1) A FEXPR version of SELECTQ would forbid use of RETURN and GO.
    (2) The form created must NOT have a prog or lambda wrapped around
        the cond expression, as this would also forbid RETURN and GO.
        Since #SELECTQ can't be lambda-bound by any means whatsoever
        and remain consistent with the standard-lisp report (if GO or
        RETURN appears inside a consequent), there is no way we can make
        SELECTQ re-entrant.  If you go into a break with ^B or ^H and
        execute another SELECTQ you will clobber the one and only
        incarnation of #SELECTQ, and if it happened to be in the middle
        of deciding which consequent to execute, then when you continue
        the computation it won't work correctly.
        Update -- IMSSS break pkg now tries to protect #SELECTQ.
        Update -- uses XP#SELECTQ which can be compiled to speed up
                  macro expansion.
    ")

(CDM SELECTQ (!#SLQ) (XP!#SELECTQ (CDR !#SLQ)))

(DE XP!#SELECTQ (!#L!#)
 (PROG (!#FIRSTCL !#RESTCL !#RSLT)
       (SETQ !#RSLT (NCONS 'COND))
       (COND ((ATOM (CAR !#L!#)) (SETQ !#FIRSTCL (SETQ !#RESTCL (CAR !#L!#))))
             ((EQ (CAAR !#L!#) 'SETQ)
              (PROGN (SETQ !#FIRSTCL (CAR !#L!#))
                     (SETQ !#RESTCL (CADAR !#L!#))))
             (T (SETQ !#FIRSTCL
                      (LIST 'SETQ (SETQ !#RESTCL '!#SELECTQ) (CAR !#L!#)))))
  LP   (COND ((CDR (SETQ !#L!# (CDR !#L!#)))
              (PROGN
               (NCONC !#RSLT
                      (NCONS
                       (CONS (LIST (COND ((ATOM (CAAR !#L!#)) 'EQUAL)
                                         (T 'MEMBER))
                                   !#FIRSTCL
                                   (LIST 'QUOTE (CAAR !#L!#)))
                             (COND ((NULL (CDDAR !#L!#)) (CDAR !#L!#))
                                   (T (NCONS (CONS 'PROGN (CDAR !#L!#))))))))
               (SETQ !#FIRSTCL !#RESTCL)
               (GO LP))))
       (NCONC !#RSLT (NCONS (CONS T !#L!#)))
       (RETURN !#RSLT)))

(!* 
"WHILE( test body )                          MACRO
    ===> (PROG ...) <while loop>
    While test is true do body.")

(!*
(CDM WHILE (!#X) (XP!#WHILE (CADR !#X) (CDDR !#X)))

(DE XP!#WHILE (!#BOOL !#BODY)
 (PROG (!#LAB)
       (SETQ !#LAB (GENSYM))
       (RETURN
        (NCONC
         (LIST 'PROG
               NIL
               !#LAB
               (LIST 'COND (LIST (LIST 'NOT !#BOOL) (LIST 'RETURN NIL))))
         (APPEND !#BODY (LIST (LIST 'GO !#LAB)))))))
)

(!*
(!* 
"REPEAT( body test )                         MACRO
    ===> (PROG ...) <repeat loop>
    Repeat body until test is true.
    Jim found that this fn as we had it was causing compiler errors.
    The BODY was (CDDR U) and the BOOL was (CADR U).  Question:
    Does the fact that Utah was unable to reproduce our compiler
    errors lie in this fact. Does function until test becomes non-NIL.")

(CDM REPEAT (!#X) (XP!#REPEAT (CADR !#X) (CADDR !#X)))

(DE XP!#REPEAT (!#BODY !#BOOL)
 (PROG (!#LAB)
       (SETQ !#LAB (GENSYM))
       (RETURN
        (LIST 'PROG
              NIL
              !#LAB
              !#BODY
              (LIST 'COND (LIST (LIST 'NOT !#BOOL) (LIST 'GO !#LAB)))))))
)

(!*
(!* 
"FOREACH( var in/of lst do/collect exp )     MACRO
    ===> (MAPxx LST (FUNCTION (LAMBDA (VAR) EXP)))
    Undocumented FOREACH supplied by Utah.  Required by compiler.
    Update: modified to call xp#foreach which can be compiled
            to speed up macro expansion.")

(CDM FOREACH (!#X)
 (XP!#FOREACH (CADR !#X)
              (CADDR !#X)
              (CAR (SETQ !#X (CDDDR !#X)))
              (CADR !#X)
              (CADDR !#X)))

(DE XP!#FOREACH (!#VAR !#MOD !#LST !#ACTION !#BODY)
 (PROG (!#FN)
       (SETQ !#FN
             (COND ((EQ !#ACTION 'DO) (COND ((EQ !#MOD 'IN) 'MAPC) (T 'MAP)))
                   ((EQ !#MOD 'IN) 'MAPCAR)
                   (T 'MAPLIST)))
       (RETURN
        (LIST !#FN !#LST (LIST 'FUNCTION (LIST 'LAMBDA (LIST !#VAR) !#BODY))))))
)

(!* 
"SAY( test expressions )                     MACRO
    ===> (COND (<test> (PROGN (PRIN2 ...) (PRIN2 ...) ...)))
    If test is true then evaluate and prin2 all expressions.
    Exceptions: the value of printing functions, those flaged with
    SAY:PRINT (including: PRINT PRIN1 PRIN2 PRINC TYO PPRINT TERPRI
    POSN DOHOME DORIGH DOLEFT DOUP DODOWN DPYNCH DPYCHR SETCUR MOVECUR)
    are just evaluated.  E.g.:  (In the example @ is used for quotes)
                (SAY T @this @ (PRIN1 '!!AND!!) @ that@)
    appears as:
                this !!AND!! that   ")

(DM SAY (!#X)
 (LIST 'COND
       (LIST (CADR !#X) (CONS 'PROGN (MAPCAR (CDDR !#X) (FUNCTION XP!#SAY1))))))

(DE XP!#SAY1 (!#Y)
 (COND ((AND (PAIRP !#Y) (EQ (CAR !#Y) 'PRINTER)) (CADR !#Y))
       ((AND (PAIRP !#Y) (FLAGP (CAR !#Y) 'SAY!:PRINT)) !#Y)
       (T (LIST 'Q!-PRIN2 !#Y))))

(FLAG '(Q!-PRINT Q!-PRIN1 Q!-PRIN2 Q!-PRINC SETCUR Q!-TYO PPRINT POSN PPOS 
TTY)  'SAY!:PRINT)

(!* 
"DIVERT( channel expressions )               MACRO
    ===> (PROG (ochan) <select given chan> <eval exps> <select ochan>)
    Yields PROG that selects channel for output,
    evaluates each expression, and then reselects prior channel.")

(CDM DIVERT (!#L)
 (CONS 'PROG
       (CONS (LIST 'OLD!#CHAN)
             (CONS (LIST 'SETQ 'OLD!#CHAN (LIST 'WRS (CADR !#L)))
                   (APPEND (CDDR !#L) (LIST (LIST 'WRS 'OLD!#CHAN)))))))

(!* 
"CAT( list of any ):string                   MACRO
    ===> (CAT-DE (LIST <list>))
    Evaluates all arguments given and forms a string from the
    concatenation of their prin2 names.
")

(CDM CAT (!#X) (LIST 'CAT!-DE (CONS 'LIST (CDR !#X))))

(!* 
"CAT-ID( list of any ):<uninterned id>       MACRO
    ===> (CAT-ID-DE (LIST <list>))
    Evaluates all arguments given and forms an id from the
    concatenation of their prin2 names. ")

(CDM CAT!-ID (!#X) (LIST 'CAT!-ID!-DE (CONS 'LIST (CDR !#X))))

(!* 
"TTY   ( L:list ):NIL                        MACRO
    TTY-TX( L:list ):NIL                        MACRO
    TTY-XT( L:list ):NIL                        MACRO
    TTY-TT( L:list ):NIL                        MACRO
    ===> (TTY-xx-DE (LIST <list>))

    TTY is selected for output, then each elt of list is evaluated and
     PRIN2'ed, except for $EOL$'s, which cause a TERPRI.
     Then prior output channel is reselected.
    TTY-TX adds leading  TERPRI.   TTY-XT adds trailing TERPRI.
    TTY-TT adds leading and trailing TERPRI's. ")

(!* 
"CDMs were making all of the following unloadable into existing
    QDRIVER.SAV core image.  I flushed the 'C' July 27")

(!* 
"TTY-DE now takes two extra arguments, for the number of TERPRIs
    to preceed and follow the other printed material.")

(DM TTY (!#X) (LIST 'TTY!-DE (CONS 'LIST (CDR !#X))))

(DM TTY!-TX (!#X) (LIST 'TTY!-TX!-DE (CONS 'LIST (CDR !#X))))

(DM TTY!-XT (!#X) (LIST 'TTY!-XT!-DE (CONS 'LIST (CDR !#X))))

(DM TTY!-TT (!#X) (LIST 'TTY!-TT!-DE (CONS 'LIST (CDR !#X))))

(!* 
"ERRSET (expression label)                   MACRO
    ===> (ERRSET-DE 'exp 'label)
    Named errset.  If error matches label, then acts like errorset.
    Otherwise propagates error upward.
    Matching:  Every label stops errors NIL, $EOF$.
               Label 'ERRORX stops any error.
               Other labels stop errors whose first arg is EQ to them.")

(CDM ERRSET (!#X)
 (LIST 'ERRSET!-DE (MKQUOTE (CADR !#X)) (MKQUOTE (CADDR !#X))))

(!* 
"GRAB( <file description> )                  MACRO
    ===> (GRABBER NIL '<file-dscr>)
    Reads in entire file, whose system name is created using
    conventions described in FORM-FILE.")

(DM GRAB (!#X) (LIST 'GRABBER NIL (MKQUOTE (CDR !#X))))

(!* 
"GRABFNS( <ids> . <file description> )       MACRO
    ===> (GRABBER FNS <file-dscr>)
    Like grab, but only reads in specified fns/vars.")

(DM GRABFNS (!#X) (LIST 'GRABBER (CADR !#X) (MKQUOTE (CDDR !#X))))

(!* 
"DUMP( <file description> )                  MACRO
    ===> (DUMPER '<file-dscr>)
    Dumps file onto disk.  Filename as in GRAB.  Prettyprints.")

(DM DUMP (!#X) (LIST 'DUMPER (MKQUOTE (CDR !#X))))

(!* 
"DUMPFNS( <ids> . <file dscr> )              MACRO
    ===> (DUMPFNS-DE <fns> '<file-dscr>)
    Like DUMP, but copies old file, inserting new defs for
    specified fns/vars")

(DM DUMPFNS (!#X) (LIST 'DUMPFNS!-DE (CADR !#X) (MKQUOTE (CDDR !#X))))

(!* 
" We are currently defining these to be macros everywhere, but might
     want them to be exprs while interpreted, in which case use the
     following to get compile-time macros.")

(!* PUT 'NEQ 'CMACRO '(LAMBDA (!#X !#Y) (NOT (EQ !#X !#Y))))

(!* PUT 'NEQN 'CMACRO '(LAMBDA (!#X !#Y) (NOT (EQN !#X !#Y))))

(!* PUT 'NEQUAL 'CMACRO '(LAMBDA (!#X !#Y) (NOT (EQUAL !#X !#Y))))

(!* 
" YSAIMAC -- MACROS used to simulate SAIL constructs.

macros:
  DO-UNTIL SAI-IF SAI2-IF SAI-DONE SAI-CONTINUE SAI-WHILE SAI-FOREACH
  SAI-FOR SAI-BEGIN PBEGIN PRETURN SAI-ASSIGN MSETQ SAI-COLLECT IFC
  OUTSTR SAI-SAY SAI-& SAI-LENGTH CVSEST CVSEN CVS SUBSTRING-FOR
  SUBSTRING-TO PUSHES PUSHVARS SLIST SAI-MAPC SAI-EQU

auxiliary exprs used to expand macros:
  XP#SAY-IF XP#SAI-WHILE XP#SAI-FOREACH XP#SAI-FOR XP#SUBSTRING-TO

")

(DM DO!-UNTIL (FORM)
 (LIST 'PROG
       NIL
       'L
       (CADR FORM)
       (LIST 'COND (LIST (CADDDR FORM) NIL) (LIST 1 '(GO L)))))

(!* 
"SAI-IF ( sailish if-expression )           MACRO
    (IF test1 THEN exp1 [ ELSEIF testi THEN expi ] [ELSE expn])
    ===> (COND (test1 exp1) ... (testi expi) ... (T expn))

    Embedded expressions do not cause embedded COND's, (unlike ALGOL!).
    Examples:
        (IF (ATOM Y) THEN (CAR X))
        (IF (ATOM Y) THEN (CAR X) ELSE (CADR X))
        (IF (ATOM Y) THEN (CAR X) ELSEIF (ATOM Z) THEN (CADR X)) ")

(DM SAI!-IF (IF!#X) (XP!#SAI!-IF (CDR IF!#X)))

(DM SAI2!-IF (IF!#X) (XP!#SAI!-IF (CDR IF!#X)))

(DE XP!#SAI!-IF (IF!#X)
 (PROG (!#ANTE !#CONSEQ !#TEMP !#ANS)
       (SETQ !#ANS NIL)
       (PROG NIL
        WHTAG(COND (IF!#X
                    (PROGN (SETQ !#ANTE (CAR IF!#X))
                           (SETQ IF!#X (CDR IF!#X))
                           (COND ((EQ (SETQ !#TEMP (CAR IF!#X)) 'THEN)
                                  (SETQ IF!#X (CDR IF!#X))))
                           (SETQ !#CONSEQ NIL)
                           (PROG NIL
                            WHTAG(COND (IF!#X
                                        (PROGN (SETQ !#TEMP (CAR IF!#X))
                                               (COND ((OR
                                                       (EQ !#TEMP 'ELSE)
                                                       (EQ !#TEMP 'ELSEIF)
                                                       (EQ !#TEMP 'EF))
                                                      (RETURN NIL)))
                                               (SETQ !#CONSEQ
                                                     (CONS !#TEMP !#CONSEQ))
                                               (SETQ IF!#X (CDR IF!#X))
                                               (GO WHTAG)))))
                           (SETQ !#ANS
                                 (CONS (CONS !#ANTE (REVERSE !#CONSEQ)) !#ANS))
                           (COND ((NOT IF!#X) (RETURN NIL)))
                           (SETQ !#TEMP (CAR IF!#X))
                           (SETQ IF!#X (CDR IF!#X))
                           (COND ((EQ !#TEMP 'ELSE)
                                  (PROGN
                                   (SETQ !#ANS (CONS (CONS 'T IF!#X) !#ANS))
                                   (RETURN NIL))))
                           (!* " MUST BE ELSEIF")
                           (GO WHTAG)))))
       (RETURN (CONS 'COND (REVERSE !#ANS)))))

(DM SAI!-DONE (C!#X) '(RETURN NIL))

(DM SAI!-CONTINUE (C!#X) '(GO CONTINUE!:))

(!* 
"SAI-WHILE ( sailish while-expression )      MACRO
    (WHILE b DO e1 e2 ...  en) does e1,..., en as long as b is non-nil.
    ===> (PROG NIL CONTINUE:
               (COND ((NULL b) (RETURN NIL)))
               e1 ... en
               (GO CONTINUE:))
    N.B.  (WHILE b DO ...  (RETURN e)) has the RETURN relative to the PROG
    in the expansion.  As in SAIL, (CONTINUE) and DONE work as statements.
    (They are also macros.) ")

(DM SAI!-WHILE (WH!#X) (XP!#SAI!-WHILE WH!#X))

(DE XP!#SAI!-WHILE (WH!#X)
 (APPENDL
  (LIST 'PROG
        NIL
        'CONTINUE!:
        (LIST 'COND (LIST (LIST 'NOT (CADR WH!#X)) (LIST 'RETURN NIL))))
  (SAI!-IF (EQ (CADDR WH!#X) 'DO) THEN (CDDDR WH!#X) ELSE (CDDR WH!#X))
  '((GO CONTINUE!:))))

(DM SAI!-FOREACH (FOREACH!#X) (XP!#SAI!-FOREACH FOREACH!#X))

(DE XP!#SAI!-FOREACH (FORE!#X)
 (APPENDL
  (LIST 'PROG
        '(FORE!#TEMP)
        (LIST 'SETQ 'FORE!#TEMP (CADDDR FORE!#X))
        'CONTINUE!:
        '(SAI!-IF (NULL FORE!#TEMP) THEN (RETURN NIL))
        (LIST 'SETQ (CADR FORE!#X) '(CAR FORE!#TEMP))
        '(SETQ FORE!#TEMP (CDR FORE!#TEMP)))
  (CDR (CDDDDR FORE!#X))
  '((GO CONTINUE!:))))

(DM SAI!-FOR (FOR!#X) (XP!#SAI!-FOR FOR!#X))

(DE XP!#SAI!-FOR (FOR!#X)
 (CONS 'PROG
       (CONS NIL
             (CONS (LIST 'SETQ (CADR FOR!#X) (CADDDR FOR!#X))
                   (CONS 'FOR!#LOOP!:
                         (CONS (LIST 'SAI!-IF
                                     (LIST (COND ((GREATERP
                                                   (EVAL
                                                    (CADR (CDDDDR FOR!#X)))
                                                   0)
                                                  'GREATERP)
                                                 (T 'LESSP))
                                           (CADR FOR!#X)
                                           (CADDDR (CDDDDR FOR!#X)))
                                     'THEN
                                     '(RETURN NIL))
                               (APPEND (CDR (CDDDDR (CDDDDR FOR!#X)))
                                       (LIST 'CONTINUE!:
                                             (LIST 'SETQ
                                                   (CADR FOR!#X)
                                                   (LIST
                                                    'PLUS
                                                    (CADR FOR!#X)
                                                    (CADR (CDDDDR FOR!#X))))
                                             '(GO FOR!#LOOP!:)))))))))

(DM SAI!-BEGIN (BEG!#X) (CONS 'DO (CDR BEG!#X)))

(DM PBEGIN (PBEG!#X)
 (LIST 'CATCH (KWOTE (CONS 'PROG (CDR PBEG!#X))) ''!$PLAB))

(DM PRETURN (PRET!#X)
 (LIST 'THROW (KWOTE (CADR PRET!#X)) (KWOTE '!$PLAB)))

(DM SAI!-ASSIGN (!#X) (LIST 'SETQ (CADR !#X) (CADDR !#X)))

(DM MSETQ (MSETQ!#X)
 (CONS 'PROG
       (CONS '(!#!#RESULT)
             (CONS (LIST 'SETQ '!#!#RESULT (CADDR MSETQ!#X))
                   (MAPCAR (CADR MSETQ!#X)
                           (FUNCTION
                            (LAMBDA (X) (LIST 'SETQ X '(POP !#!#RESULT)))))))))

(DM SAI!-COLLECT (X)
 (LIST 'SETQ (CADDDR X) (LIST 'CONS (CADR X) (CADDDR X))))

(DM IFC (X)
 (COND ((EVAL (CADR X)) (CADDDR X))
       ((EQ (CAR (CDDDDR X)) 'ELSEC) (CADR (CDDDDR X)))
       (T NIL)))

(DM OUTSTR (!#X) (CONS 'TTY (CDR !#X)))

(!* DE TTYMSG (!#X)
   (MAPC !#X
         (FUNCTION
          (LAMBDA (!#ELT)
           (COND ((STRINGP !#ELT) (PRIN2 !#ELT))
                 ((EQ !#ELT 'T) (TERPRI))
                 (T (PRINT (EVAL !#ELT))))))))

(DM SAI!-SAY (!#X) (CONS 'TTY (CDR !#X)))

(DM SAI!-!& (!#X) (CONS 'CAT (CDR !#X)))

(DM SAI!-LENGTH (!#X) (CONS 'FLATSIZE2 (CDR !#X)))

(DM CVSEST (!#X) (CADR !#X))

(DM CVSEN (!#X) (CADR !#X))

(DM CVS (!#X) (CADR !#X))

(DM SUBSTRING!-FOR (!#L)
 (LIST 'SUBSTR (CADR !#L) (LIST 'SUB1 (CADDR !#L)) (CADDDR !#L)))

(!* 
"REM is planning on cleaning this up so it works in all cases...
  The form that  (SUBSTRING-TO stringexpr low high)  should expand into is
        ((LAMBDA (#STRING) (SUBSTR #STRING low high)) stringexpr)
  except that low and high have been modified to replace INF by
  explicit calls to (FLATSIZE2 #STRING).  Thus things like
        (SUBSTRING-TO (READ) 2 (SUB1 INF))
  should work without requiring the user to type the same string twice.
  Probably that inner (SUBSTR ...) should simply be
        ((LAMBDA (INF) (SUBSTR #STRING low high)) (FLATSIZE2 #STRING))
  where we don't have to internally modify low or high at all!")

(DM SUBSTRING!-TO (!#L) (XP!#SUBSTRING!-TO (CDR !#L)))

(DE XP!#SUBSTRING!-TO (!#L)
 (PROG (STREXP LOWEXP HIEXP IN!:LOW!:BOUND INNER!:INF!:BOUND
        OUTER!:STRING!:BOUND OLDRES NEWRES)
       (SETQ STREXP (CAR !#L))
       (SETQ LOWEXP (CADR !#L))
       (SETQ HIEXP (CADDR !#L))
       (SETQ IN!:LOW!:BOUND
             (LIST (LIST 'LAMBDA
                         '(!#LOW !#HIGH)
                         '(SUBSTR !#STRING !#LOW (DIFFERENCE !#HIGH !#LOW)))
                   (LIST 'SUB1 (LIST 'MAX 1 LOWEXP))
                   HIEXP))
       (SETQ INNER!:INF!:BOUND
             (LIST (LIST 'LAMBDA '(INF) IN!:LOW!:BOUND) '(FLATSIZE2 !#STRING)))
       (SETQ OUTER!:STRING!:BOUND
             (LIST (LIST 'LAMBDA '(!#STRING) INNER!:INF!:BOUND) STREXP))
       (RETURN OUTER!:STRING!:BOUND)))

(DM PUSHES (!#X) NIL)

(DM PUSHVARS (!#X) NIL)

(DM SLIST (!#X) (CONS 'LIST (CDR !#X)))

(DM SAI!-MAPC (!#L) (LIST 'MAPC (CADDR !#L) (CADR !#L)))

(DM SAI!-EQU (!#L) (CONS 'EQUAL (CDR !#L)))

