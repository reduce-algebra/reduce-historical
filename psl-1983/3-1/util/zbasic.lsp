(!* 
"ZBASIC contains 6 packages --
    (1) YLSTS -- useful functions for lists.
    (2) YNUMS -- useful functions for numbers.
    (3) YSTRS -- useful functions for strings.
    (4) YIO   -- useful functions for user io.
    (5) YCNTRL -- useful functions for program control.
    (6) YRARE -- functions we use now, but may eliminate.  ")

(!* 
" YLSTS -- BASIC LIST UTILITIES

CCAR    ( X:any ):any
CCDR    ( X:any ):any
LAST    ( X:list ):any
NTH-CDR ( L:list N:number ):list
NTH-ELT ( L:list N:number ):elt of list
NTH-TAIL( L:list N:number ):list
TAIL-P  ( X:list Y:list ):extra-boolean
NCONS   ( X:any ): (CONS X NIL)
KWOTE   ( X:any ): '<eval of #X>
MKQUOTE ( X:any ): '<eval of #X>
RPLACW  ( X:list Y:list ):list
DREMOVE ( X:any L:list ):list
REMOVE  ( X:any L:list ):list
DSUBST  ( X:any Y:any Z:list ):list
LSUBST  ( NEW:list OLD:list X:any ):list
COPY    ( X:list ):list
TCONC   ( P:list X:any ): tconc-ptr
LCONC   ( P:list X:list ):list
CVSET   ( X:list ):set
ENTER   ( ELT:element SET:list ):set
ABSTRACT( FN:function L:list ):list
EACH    ( L:list FN:function ):extra-boolean
SOME    ( L:list FN:function ):extra-boolean
INTERSECTION  ( SET1:list SET2:list ):extra-boolean
SETDIFFERENCE ( SET1:list SET2:list ):extra-boolean
SUBSET  ( SET1:any SET2:list ):extra boolean
UNION   ( X:list Y:list ):list
SEQUAL  ( X:list Y:list ):extra boolean
MAP2C   ( X:list Y:list FN:function ):NIL
MAP2    ( X:list Y:list FN:function ):NIL
ATSOC   ( ALST:list, KEY:atom ):any
")

(FLUID '(!#SET2))

(!* 
"CCAR( X:any ):any
    ----
    Careful Car.  Returns car of x if x is a list, else NIL.")

(CDE CCAR (!#X) (COND ((PAIRP !#X) (CAR !#X))))

(!* 
"CCDR( X:any ):any
    ----
    Careful Cdr.  Returns cdr of x if x is a list, else NIL.")

(CDE CCDR (!#X) (COND ((PAIRP !#X) (CDR !#X))))

(!* 
"LAST( X:list ):any
    ----
    Returns the last cell in X.
    E.g.  (LAST '(A B C)) = (C),  (LAST '(A B . C)) = C.")

(!*
(CDE LAST (!#X)
 (COND ((ATOM !#X) !#X) ((NULL (CDR !#X)) !#X) (T (LAST (CDR !#X)))))
)

(CDM LAST (!#X) (CONS 'LASTPAIR (CDR !#X)))

(!* 
"NTH-CDR( L:list N:number ):list
    -------
    Returns the nth cdr of list--0 is the list, 1 the cdr ...")

(CDE NTH!-CDR (!#L !#N)
 (COND ((LESSP !#N 1) !#L)
       ((ATOM !#L) NIL)
       (T (NTH!-CDR (CDR !#L) (SUB1 !#N)))))

(!* 
"NTH-TAIL( L:list N:number ):list
    -------
    Returns the nth tail of list--1 is the list, 2 the cdr ...")

(CDE NTH!-TAIL (!#L !#N)
 (COND ((LESSP !#N 2) !#L)
       ((ATOM !#L) NIL)
       (T (NTH!-TAIL (CDR !#L) (SUB1 !#N)))))

(!* 
"NTH-ELT( L:list N:number ):list
    -------
    Returns the nth elt of list--1 is the car, 2 the cadr ...")

(CDE NTH!-ELT (!#L !#N) (CAR (NTH!-TAIL !#L !#N)))

(!* 
"TAIL-P( X:list Y:list ):extra-boolean
    ------
    If X is a non-nil tail of Y (X eq cdr Y or cddr Y or...), return X.
    Renamed to avoid a conflict with TAILP in compiler")

(CDE TAIL!-P (!#X !#Y)
 (COND (!#X (PROG NIL
             LP   (COND ((ATOM !#Y) (RETURN NIL)) ((EQ !#X !#Y) (RETURN !#X)))
                  (SETQ !#Y (CDR !#Y))
                  (GO LP)))))

(!* " NCONS( X:any ): (CONS X NIL)
     -----
     Returns (CONS X NIL) ")

(!*
(CDE NCONS (!#X) (CONS !#X NIL))
)

(!* 
"  KWOTE( X:any ): '<eval of #X>
    MKQUOTE( X:any ): '<eval of #X>
    -------
    Returns the quoted value of its argument. ")

(CDM KWOTE (!#X) (CONS 'MKQUOTE (CDR !#X)))

(!*
(CDE MKQUOTE (!#X) (LIST 'QUOTE !#X))
)

(!* 
"RPLACW( X:list Y:list ):list
    ------
    Destructively replace the Whole list X by Y.")

(!*
(CDE RPLACW (!#X !#Y) (RPLACA (RPLACD !#X (CDR !#Y)) (CAR !#Y)))
)

(!* 
"DREMOVE( X:any L:list ):list
    -------
    Remove destructively all equal occurrances of X from L.")

(CDE DREMOVE (!#X !#L)
 (COND ((ATOM !#L) NIL)
       ((EQUAL !#X (CAR !#L))
        (COND ((CDR !#L)
               (PROGN (RPLACA !#L (CADR !#L))
                      (RPLACD !#L (CDDR !#L))
                      (DREMOVE !#X !#L)))))
       (T (PROG (!#Z)
                (SETQ !#Z !#L)
           LP   (COND ((ATOM (CDR !#L)) (RETURN !#Z))
                      ((EQUAL !#X (CADR !#L)) (RPLACD !#L (CDDR !#L)))
                      (T (SETQ !#L (CDR !#L))))
                (GO LP)))))

(!* 
"REMOVE( X:any  L:list ):list
    ------
    Return copy of L with all equal occurrences of X removed.")

(CDE REMOVE (!#X !#L)
 (COND ((ATOM !#L) !#L)
       ((EQUAL (CAR !#L) !#X) (REMOVE !#X (CDR !#L)))
       (T (CONS (CAR !#L) (REMOVE !#X (CDR !#L))))))

(!* 
"COPY( X:list ):list
    ----
    Make a copy of X--EQUAL but not EQ (except for atoms).")

(!*
(CDE COPY (!#X) (SUBST 0 0 !#X))
)

(!* 
"DSUBST( X:any Y:any Z:list ):list
    ------
    Destructively substitute copies(??) of X for Y in Z.")

(!*
(CDE DSUBST (!#X !#Y !#Z)
 (PROG (!#B)
       (COND ((EQUAL !#Y (SETQ !#B !#Z)) (RETURN (COPY !#X))))
  LP   (COND ((VECTORP !#Z)
              (RETURN
               (PROG (!#I)
                     (SETQ !#I (UPBV !#Z))
                LOOP (COND ((LESSP !#I 1) (RETURN NIL)))
                     (PUTV !#Z !#I (DSUBST !#X !#Y (GETV !#Z !#I)))
                     (SETQ !#I (SUB1 !#I))
                     (GO LOOP))))
             ((ATOM !#Z) (RETURN !#B))
             ((EQUAL !#Y (CAR !#Z)) (RPLACA !#Z (COPY !#X)))
             (T (DSUBST !#X !#Y (CAR !#Z))))
       (COND ((AND !#Y (EQUAL !#Y (CDR !#Z)))
              (PROGN (RPLACD !#Z (COPY !#X)) (RETURN !#B))))
       (SETQ !#Z (CDR !#Z))
       (GO LP)))
)

(!* "DSUBST is the same as SubstIP.")

(CDM DSUBST (!#X) (CONS 'SUBSTIP (CDR !#X)))

(!* 
"LSUBST( NEW:list OLD:list X:any ):list
    ------
    Substitute elts of NEW (splicing) for the element old in X")

(CDE LSUBST (!#NEW !#OLD !#X)
 (COND ((NULL !#X) NIL)
       ((VECTORP !#X)
        (PROG (!#V !#I)
              (SETQ !#I (UPBV !#X))
              (SETQ !#V (MKVECT !#I))
         LOOP (COND ((LESSP !#I 1) (RETURN !#V)))
              (PUTV !#V !#I (LSUBST !#NEW !#OLD (GETV !#V !#I)))
              (SETQ !#I (SUB1 !#I))
              (GO LOOP)))
       ((ATOM !#X) (COND ((EQUAL !#OLD !#X) !#NEW) (T !#X)))
       ((EQUAL !#OLD (CAR !#X))
        (NCONC (COPY !#NEW) (LSUBST !#NEW !#OLD (CDR !#X))))
       (T (CONS (LSUBST !#NEW !#OLD (CAR !#X)) (LSUBST !#NEW !#OLD (CDR !#X))))
  ))

(!*
(!* 
"TCONC( P:list X:any ): tconc-ptr
    -----
    Pointer consists of (CONS LIST (LAST LIST)).
    Returns (and alters) pointer consisting of (CONS LIST1 (LAST LIST1)),
    where LIST1 = (NCONC1 LIST X).
    Avoids searching down the list as nconc1 does, by pointing at last elt
    of list for nconc1.
    To use, setq ptr to (NCONS NIL), tconc elts, return car of ptr.")

(CDE TCONC (!#P !#X)
 (COND ((NULL !#P) (CONS (SETQ !#X (NCONS !#X)) !#X))
       ((ATOM !#P) (PROGN (PRINT !#P) (ERROR 24 "BAD ARGUMENT 0 TCONC")))
       ((CDR !#P) (RPLACD !#P (CDR (RPLACD (CDR !#P) (NCONS !#X)))))
       (T (RPLACA (RPLACD !#P (SETQ !#X (NCONS !#X))) !#X))))

(!* 
"LCONC( P:list X:list ):list
    -----
    Same as TCONC, but NCONCs instead of NCONC1s.")

(CDE LCONC (!#P !#X)
 (PROG (!#Y)
       (COND ((NULL !#X) (RETURN !#P))
             ((OR (ATOM !#X) (CDR (SETQ !#Y (LAST !#X)))) (PRINT !#X))
             ((NULL !#P) (RETURN (CONS !#X !#Y)))
             ((ATOM !#P) (PRINT !#P))
             ((NULL (CAR !#P)) (RETURN (RPLACA (RPLACD !#P !#Y) !#X)))
             (T (PROGN (RPLACD (CDR !#P) !#X) (RETURN (RPLACD !#P !#Y)))))
       (ERROR 25 "BAD ARGUMENT 0 LCONC")))
)

(!* 
"CVSET( X:list ):list
    --------------------
    Converts list to set, i.e., removes redundant elements.")

(CDE CVSET (!#X)
 (PROG (!#RES)
       (COND ((NULL !#X) (RETURN NIL)))
       (SETQ !#RES (NCONS NIL))
  LOOP (COND ((NULL !#X) (RETURN (CAR !#RES))))
       (COND ((NOT (MEMBER (CAR !#X) (CDR !#X))) (TCONC !#RES (CAR !#X))))
       (SETQ !#X (CDR !#X))
       (GO LOOP)))

(!* 
"ENTER( ELT:element SET:list ):list
    -----
    Returns (ELT . SET) if ELT is not member of SET, else SET.")

(CDE ENTER (!#ELT !#SET)
 (COND ((MEMBER !#ELT !#SET) !#SET) (T (CONS !#ELT !#SET))))

(!* 
"ABSTRACT( FN:function L:list ):list
    --------
    Returns list of elts of list satisfying FN.")

(CDE ABSTRACT (!#FN !#L)
 (PROG (!#ABSTRACTED)
       (SETQ !#ABSTRACTED (NCONS NIL))
       (MAPC !#L
             (FUNCTION
              (LAMBDA (!#Z)
               (COND ((APPLY !#FN (LIST !#Z)) (TCONC !#ABSTRACTED !#Z))))))
       (RETURN (CAR !#ABSTRACTED))))

(!* 
"EACH( L:list FN:function ):extra boolean
    ----
    Returns L if each elt satisfies FN, else NIL.")

(CDE EACH (!#L !#FN)
 (PROG (!#LIS)
       (SETQ !#LIS !#L)
  LOOP (COND ((NULL !#LIS) (RETURN (COND (!#L !#L) (T T))))
             ((NOT (APPLY !#FN (NCONS (CAR !#LIS)))) (RETURN NIL)))
       (SETQ !#LIS (CDR !#LIS))
       (GO LOOP)))

(!* 
"SOME( L:list FN:function ):extra boolean
     ----
    Returns the first tail of the list whose CAR satisfies function.")

(CDE SOME (!#L !#FN)
 (PROG NIL
  LOOP (COND ((NULL !#L) (RETURN NIL))
             ((APPLY !#FN (LIST (CAR !#L))) (RETURN !#L)))
       (SETQ !#L (CDR !#L))
       (GO LOOP)))

(!* 
"INTERSECTION( #SET1:list #SET2:list ):extra boolean
     ------------
     Returns list of elts in SET1 which are also members of SET2 ")

(CDE INTERSECTION (!#SET1 !#SET2) (ABSTRACT (FUNCTION INTERSECTION1) !#SET1))

(CDE INTERSECTION1 (!#ELT) (MEMBER !#ELT !#SET2))

(!* 
"SETDIFFERENCE( #SET1:list #SET2:list ):extra boolean
     -------------
     Returns all elts of SET1 not members of SET2.")

(CDE SETDIFFERENCE (!#SET1 !#SET2) (ABSTRACT (FUNCTION SETDIFFERENCE1) !#SET1))

(CDE SETDIFFERENCE1 (!#ELT) (NOT (MEMBER !#ELT !#SET2)))

(!* 
"SUBSET( #SET1:any #SET2:list ):extra boolean
    ------
    Returns SET1 if each element of SET1 is a member of SET2.")

(CDE SUBSET (!#SET1 !#SET2) (AND !#SET1 (EACH !#SET1 (FUNCTION SUBSET1))))

(CDE SUBSET1 (!#ELT) (MEMBER !#ELT !#SET2))

(!* 
"UNION( X:list Y:list ):list
     -----
     Returns the union of lists X, Y")

(CDE UNION (!#X !#Y) (APPEND !#X (SETDIFFERENCE !#Y !#X)))

(!* 
"SEQUAL( X:list Y:list ):extra boolean
     ------
     Returns X if X and Y are set-equal: same length and X subset of Y.")

(CDE SEQUAL (!#X !#Y) (AND (EQUAL (LENGTH !#X) (LENGTH !#Y)) (SUBSET !#X !#Y)))

(!* 
"MAP2( X:list Y:list FN:function ):NIL
    ------
    Applies FN (of two arguments) to successive paired tails of X and Y.")

(DE MAP2 (!#L1 !#L2 !#FN)
 (PROG NIL
  LOOP (COND ((NULL (AND !#L1 !#L2))
              (COND ((OR !#L1 !#L2) (ERROR 0 "MAP2: mismatched lists"))
                    (T (RETURN NIL)))))
       (APPLY !#FN (LIST !#L1 !#L2))
       (SETQ !#L1 (CDR !#L1))
       (SETQ !#L2 (CDR !#L2))
       (GO LOOP)))

(!* 
"MAP2C( X:list Y:list FN:function ):NIL
    ------
    Applies FN (of two arguments) to successive paired elts of X and Y.")

(DE MAP2C (!#L1 !#L2 !#FN)
 (PROG NIL
  LOOP (COND ((NULL (AND !#L1 !#L2))
              (COND ((OR !#L1 !#L2) (ERROR 0 "MAP2C: mismatched lists"))
                    (T (RETURN NIL)))))
       (APPLY !#FN (LIST (CAR !#L1) (CAR !#L2)))
       (SETQ !#L1 (CDR !#L1))
       (SETQ !#L2 (CDR !#L2))
       (GO LOOP)))

(!* 
"ATSOC( ALST:list, KEY:atom ):any
    -----
    Like ASSOC, except uses an EQ check.  Returns first element of
    ALST whose CAR is KEY.")

(!*
(CDE ATSOC (KEY ALST)
 (COND ((NULL ALST) NIL)
       ((EQ (CAAR ALST) KEY) (CAR ALST))
       (T (ATSOC KEY (CDR ALST)))))
)

(!* 
" YNUMS -- BASIC NUMBER UTILITIES

ADD1    ( number ):number                       EXPR
SUB1    ( number ):number                       EXPR
ZEROP   ( any ):boolean                         EXPR
MINUSP  ( number ):boolean                      EXPR
PLUSP   ( number ):boolean                      EXPR
POSITIVE( X:any ):extra-boolean                 EXPR
NEGATIVE( X:any ):extra-boolean                 EXPR
NUMERAL ( X:number/digit/any ):boolean          EXPR
GREAT1  ( X:number Y:number ):extra-boolean     EXPR
LESS1   ( X:number Y:number ):extra-boolean     EXPR
GEQ     ( X:number Y:number ):extra-boolean     EXPR
LEQ     ( X:number Y:number ):extra-boolean     EXPR
ODD     ( X:integer ):boolean                   EXPR
SIGMA   ( L:list FN:function ):integer          EXPR
RAND16  ( ):integer                             EXPR
IRAND   ( N:integer ):integer                   EXPR
")

(!* 
"The DEC compiler may optimize calls to PLUS2, DIFFERENCE, EQUAL,
    LESSP, etc. by converting them to calls to ADD1, SUB1, ZEROP,
    MINUSP, etc.  This will create circular defintions in the
    conditional defintions, about which the compiler will complain.
    Such complaints can be ignored.")

(!*
(COND ((AND (CODEP (CCDR (GETD 'ADD1)))
            (CODEP (CCDR (GETD 'SUB1)))
            (CODEP (CCDR (GETD 'MINUSP))))
       (PROGN (TERPRI)
              (PRIN2
                   "Ignore any circular definition msg for ADD1, SUB1, MINUSP")
              (TERPRI))))

(!* 
"ADD1( number ):number                        EXPR
    ----
    Note: DEC compiler optimizes (PLUS2 N 1) into (ADD1 N). ")

(CDE ADD1 (!#N) (PLUS2 !#N 1))

(!* 
"SUB1( number ):number                        EXPR
    ----
    Note: DEC compiler optimizes (DIFFERENCE N 1) into (SUB1 N). ")

(CDE SUB1 (!#N) (DIFFERENCE !#N 1))

(!* 
"ZEROP( X:any ):boolean                       EXPR
    -----
    Returns non-nil iff X equals 0.")

(CDE ZEROP (!#X) (EQN !#X 0))

(!* 
"MINUSP( N:number ):boolean                   EXPR
    ------
    Returns non-nil iff N is less than 0.")

(CDE MINUSP (!#N) (LESSP !#N 0))
)

(!* 
"PLUSP( N:number ):boolean                    EXPR
    -----
    Returns non-nil iff N is greater than 0.")

(CDE PLUSP (!#N) (GREATERP !#N 0))

(!* 
"ODD( X:integer ):boolean                     EXPR
    ---
    Returns T if x is odd, else NIL.
    WARNING: EVENP is used by REDUCE to test if a list has even
    length.  ODD and EVENP are thus highly distinct.")

(CDE ODD (!#X) (EQN 1 (REMAINDER !#X 2)))

(!* 
"POSITIVE( X:any ):boolean                   EXPR
    --------
    Returns non-nil iff X is a positive number.")

(CDE POSITIVE (!#X) (AND (NUMBERP !#X) (GREATERP !#X 0)))

(!* 
"NEGATIVE( X:any ):boolean                   EXPR
    --------
    Returns non-nil iff X is a negative number.")

(CDE NEGATIVE (!#X) (AND (NUMBERP !#X) (LESSP !#X 0)))

(!* 
"NUMERAL( X:any ): boolean                   EXPR
    -------
    Returns true for both numbers and digits.  Some dialects
    had been treating the digits as numbers, and this fn is
    included as a replacement for NUMBERP where NUMBERP might
    really be checking for digits.
    N.B.:  Digits are characters and thus ID's")

(DE NUMERAL (!#X) (OR (DIGIT !#X) (NUMBERP !#X)))

(!* 
"GREAT1( X:number Y:number ):extra-boolean   EXPR
    ------
    Returns X if it is strictly greater than Y, else NIL.
    GREATERP is simpler if only T/NIL is needed.")

(CDE GREAT1 (!#X !#Y)
 (COND ((AND (NUMBERP !#X) (NUMBERP !#Y) (GREATERP !#X !#Y)) !#X)))

(!* 
"LESS1( X:number Y:number ):extra-boolean    EXPR
    -----
    Returns X if it is strictly less than Y, else NIL
    LESSP is simpler if only T/NIL is needed.")

(CDE LESS1 (!#X !#Y)
 (COND ((AND (NUMBERP !#X) (NUMBERP !#Y) (LESSP !#X !#Y)) !#X)))

(!*
(!* 
"GEQ( X:number Y:number ):extra-boolean      EXPR
    ---
    Returns X if it is greater than or equal to Y, else NIL.")

(CDE GEQ (!#X !#Y)
 (COND ((AND (NUMBERP !#X) (NUMBERP !#Y) (NOT (LESSP !#X !#Y))) !#X)))

(!* 
"LEQ( X:number Y:number ):extra-boolean      EXPR
    ---
    Returns X if it is less than or equal to Y, else NIL.")

(CDE LEQ (!#X !#Y)
 (COND ((AND (NUMBERP !#X) (NUMBERP !#Y) (NOT (GREATERP !#X !#Y))) !#X)))
)

(!* 
"SIGMA( L:list, FN:function ):integer        EXPR
    -----
    Returns sum of results of applying FN to each elt of LST.")

(CDE SIGMA (!#L !#FN)
 (COND ((NULL !#L) 0)
       (T (PLUS2 (APPLY !#FN (LIST (CAR !#L))) (SIGMA (CDR !#L) !#FN)))))

(!* 
"RAND16( ):integer                           EXPR
    IRAND ( N:integer ):integer                 EXPR
    ------
    Linear-congruential random-number generator.  To avoid dependence
    upon the big number package, we are forced to use 16-bit numbers,
    which means the generator will cycle after only 2^16.
    The randomness obtained should be sufficient for selecting choices
    in VOCAL, but not for monte-carlo experiments and other sensitive
    stuff.")

(GLOBAL '(G!:RANDOM G!:RADD G!:RMUL G!:RMOD))

(!* "decimal 14933 = octal 35125, decimal 21749 = octal 52365 ")

(SETQ G!:RANDOM 0)

(SETQ G!:RADD 14933)

(SETQ G!:RMUL 21749)

(SETQ G!:RMOD (TIMES 256 256))

(!* 
"Returns a new 16-bit unsigned random integer.  Leftmost bits are
    most random so you shouldn't use REMAINDER to scale this to range")

(DE RAND16 NIL
 (SETQ G!:RANDOM (REMAINDER (TIMES G!:RMUL (PLUS G!:RADD G!:RANDOM)) G!:RMOD)))

(!* 
"Scale new random number to range 0 to N-1 with approximately equal
    probability.  Uses times/quotient instead of remainder to make best
    use of high-order bits which are most random")

(DE IRAND (N) (QUOTIENT (TIMES (RAND16) N) G!:RMOD))

(!* 
" YSTRS --  BASIC STRING UTILITIES

EXPLODEC ( X:any ):char-list                      EXPR
EXPLODE2 ( X:any ):char-list                      EXPR
FLATSIZE ( X:str ):integer                        EXPR
FLATSIZE2( X:str ):integer                        EXPR
NTHCHAR  ( X:str N:number ):char-id               EXPR
ICOMPRESS( LST:lst ):<interned id>                EXPR
SUBSTR   ( STR:str START:num LENGTH:num ):string  EXPR
CAT-DE   ( L: list of strings ):string            EXPR
CAT-ID-DE( L: list of strings ):<uninterned id>   EXPR
SSEXPR   ( S: string ):<interned id>              EXPR
")

(!*
(!* 
"EXPLODE2( X:any ):char-list                 EXPR
    EXPLODEC( X:any ):char-list                 EXPR
    --------
    List of characters which would appear in PRIN2 of X.  If either
    is built into the interpreter, we will use that defintion for both.
    Otherwise, the definition below should work, but inefficiently.
    Note that this definition does not support vectors and lists.
    (The DEC and IBM interpreters support EXPLODE and EXPLODE2 by using
     the same internal algorithm that is used for PRIN1 (PRIN2), but put
     the chars generated into a list instead of printing them.
     Thus, they work on arbitrary s-expressions.) ")

(!* "If either EXPLODEC or EXPLODE2 is defined, the CDE does nothing.")

(COND ((GETD 'EXPLODEC) (FLAG '(EXPLODE2) 'LOSE)))

(CDE EXPLODE2 (!#X)
 (PROG (!#BIG !#TAIL)
       (COND ((IDP !#X) (GO IDS))
             ((STRINGP !#X) (GO STRS))
             ((NUMBERP !#X) (RETURN (EXPLODE !#X)))
             ((CODEP !#X) (RETURN (EXPLODE !#X)))
             (T (ERROR "EXPLODE2 -- bad argument")))
       (!* 
"For ids -- Note: last elt of #BIG will never be bang
            unless char before it was also a bang.")
  IDS  (SETQ !#TAIL (SETQ !#BIG (EXPLODE !#X)))
  IDLP (COND ((EQUAL (CAR !#TAIL) '!!) (RPLACW !#TAIL (CDR !#TAIL)))
             ((NULL (CDR !#TAIL)) (RETURN !#BIG)))
       (SETQ !#TAIL (CDR !#TAIL))
       (GO IDLP)
       (!* "For strings.  #BIG has at least 2 elts, the quotes")
  STRS (SETQ !#TAIL (SETQ !#BIG (EXPLODE !#X)))
  STRLP(COND ((NULL (CDDR !#TAIL))
              (PROGN (RPLACD !#TAIL NIL) (RETURN (CDR !#BIG))))
             ((EQUAL (CAR (SETQ !#TAIL (CDR !#TAIL))) '!")
              (RPLACD !#TAIL (CDDR !#TAIL))))
       (GO STRLP)))

(REMFLAG '(EXPLODEC EXPLODE2) 'LOSE)

(CDE EXPLODEC (!#X) (EXPLODE2 !#X))

(CDE EXPLODE2 (!#X) (EXPLODEC !#X))

(!* 
"Note: According to the STANDARD LISP REPORT, EXPLODE and EXPLODE2
    are only defined for atoms.  If your interpreter does not support
    extended EXPLODE and EXPLODE2, then change the second CDE's below
    for FLATSIZE and FLATSIZE2 to get recursive versions of them.")

(!* 
" FLATSIZE( X:any ):integer                  EXPR
     --------
     Number of chars in a PRIN1 of X.
     Also equals length of list created by EXPLODE of X,
     assuming that EXPLODE extends to arbitrary s-expressions.
     DEC and IBM interpreters use the same internal algorithm that
     is used for PRIN1, but count chars instead of printing them. ")

(CDE FLATSIZE (!#X) (LENGTH (EXPLODE !#X)))

(!* 
"If your EXPLODE only works for atoms, comment out the above
    CDE and turn the CDE below into DE.")

(CDE FLATSIZE (E)
 (COND ((ATOM E) (LENGTH (EXPLODE E)))
       (T ((LAMBDA (L1 D)
            (COND ((NULL D) (PLUS L1 2))
                  (T ((LAMBDA (L2)
                       (COND ((ATOM D) (PLUS 5 L1 L2)) (T (PLUS 1 L1 L2))))
                      (FLATSIZE D)))))
           (FLATSIZE (CAR E))
           (CDR E)))))

(!* 
" FLATSIZE2( X:any ):integer                 EXPR
     ---------
     Number of chars in a PRIN2 of X.
     Also equals length of list created by EXPLODE2 of X,
     assuming that EXPLODE2 extends to arbitrary s-expressions.
     DEC and IBM interpreters use the same internal algorithm that
     is used for PRIN2, but count chars instead of printing them. ")

(!* " FLATSIZE will often suffice for FLATSIZE2 ")

(CDE FLATSIZE2 (!#X) (LENGTH (EXPLODE2 !#X)))

(!* 
"If your EXPLODE2 only works for atoms, comment out the CDE above
    and turn the CDE below into DE.")

(CDE FLATSIZE2 (E)
 (COND ((ATOM E) (LENGTH (EXPLODE2 E)))
       (T ((LAMBDA (L1 D)
            (COND ((NULL D) (PLUS L1 2))
                  (T ((LAMBDA (L2)
                       (COND ((ATOM D) (PLUS 5 L1 L2)) (T (PLUS 1 L1 L2))))
                      (FLATSIZE2 D)))))
           (FLATSIZE2 (CAR E))
           (CDR E)))))
)

(!* 
" NTHCHAR( X:any, N:number ):character-id      EXPR
     -------
     Returns nth character of EXPLODE2 of X.")

(CDE NTHCHAR (!#X !#N)
 (PROG (!#Y)
       (COND ((SETQ !#Y (NTH!-TAIL (EXPLODE2 !#X) !#N)) (RETURN (CAR !#Y))))))

(!* 
"ICOMPRESS( LST:list ):interned atom           EXPR
    ---------
    Returns INTERN'ed atom made by COMPRESS.")

(!*
(CDE ICOMPRESS (!#LST) (INTERN (COMPRESS !#LST)))
)

(!* "Implode is the same as ICOMPRESS, but more efficient.")

(CDM ICOMPRESS (!#X) (CONS 'IMPLODE (CDR !#X)))

(!* 
"SUBSTR( STR:string START:number LENGTH:number ):string  EXPR
    ------
    Returns a substring of the given LENGTH beginning with the
    character at location START in the string.
    NB: The first location of the string is 0.
        If START or LENGTH is negative, 0 is assumed.
        If the length given would exceed the end of the string, the
        subtring returned quietly goes to end of string, no error.")

(!*
(CDE SUBSTR (!#STR !#START !#LENGTH)
 (PROG (!#BIG !#TAIL)
       (COND ((NOT (STRINGP !#STR))
              (ERROR 0 "SUBSTR -- argument not a string."))
             ((OR (NOT (NUMBERP !#START)) (NOT (NUMBERP !#LENGTH)))
              (ERROR 0 "SUBSTR -- start or length not number"))
             ((LESSP !#LENGTH 1) (RETURN ""))
             ((EQUAL !#STR "") (RETURN ""))
             ((MINUSP !#START) (SETQ !#START 0)))
       (!* "Fall thru when CDR of #BIG is desired first character")
       (SETQ !#BIG (EXPLODE !#STR))
  LP   (COND ((MINUSP (SETQ !#START (SUB1 !#START))) NIL)
             ((NULL (CDR (SETQ !#BIG (CDR !#BIG)))) (RETURN ""))
             ((EQUAL (CAR !#BIG) '!")
              (PROGN (!* "Next char must also be quote")
                     (SETQ !#BIG (CDR !#BIG))
                     (GO LP)))
             (T (GO LP)))
       (!* "CDR of #BIG is desired first character")
       (!* "When length drops below zero, chop off remainder")
       (!* "If list ends first, make string from what we have")
       (SETQ !#TAIL !#BIG)
  LP2  (COND ((MINUSP (SETQ !#LENGTH (SUB1 !#LENGTH)))
              (RPLACD !#TAIL (LIST '!")))
             ((NULL (CDR (SETQ !#TAIL (CDR !#TAIL)))) NIL)
             ((EQUAL (CAR !#TAIL) '!")
              (PROGN (SETQ !#TAIL (CDR !#TAIL)) (GO LP2)))
             (T (GO LP2)))
       (RETURN (COMPRESS (RPLACA !#BIG '!")))))
)

(!* "SUBSTR is handled more efficiently by PSL function SUB")

(CDE SUBSTR (!#S !#ST !#LEN)
 (SUB !#S (COND ((MINUSP !#ST) 0) (T !#ST)) (SUB1 !#LEN)))

(!* 
"CAT-DE( L: list of expressions ):string        EXPR
    -------
    Returns a string made from the concatenation of the prin2 names
    of the expressions in the list.  Usually called via CAT macro.")

(DE CAT!-DE (!#L)
 (COMPRESS (CONS '!" (NCONC (MAPCAN !#L (FUNCTION EXPLODE2)) (LIST '!")))))

(!* 
"CAT-ID-DE( L: list of any ):uninterned id     EXPR
    -------
    Returns an id made from the concatenation of the prin2 names
    of the expressions in the list.  Usually called via CAT-ID macro.")

(DE CAT!-ID!-DE (!#L) (COMPRESS (MAPCAN !#L (FUNCTION EXPLODE2))))

(!* 
"SSEXPR( S: string ): id                        EXPR
    ------
    Returns ID `read' from string.  Not very robust.")

(DE SSEXPR (!#STR)
 (COND ((STRINGP !#STR) (ICOMPRESS (EXPLODE2 !#STR))) (T !#STR)))

(!* 
"YIO -- simple I/O utilities.  All EXPR's.

CONFIRM       (#QUEST: string ):boolean
EATEOL        ():NIL
TTY-DE        (#L: list ):NIL
TTY-TX-DE     (#L: list ):NIL
TTY-XT-DE     (#L: list ):NIL
TTY-TT-DE     (#L: list ):NIL
TTY-ELT       (#X: elt ):NIL
PRINA         (#X: any ):NIL
PRIN1SQ       (#X: any ):NIL
PRIN2SQ       (#X: any ):NIL
PRINCS        (#X: single-char-id ):NIL
--queue-code--
SEND          ():NIL
SEND-1        (#EE)
ENQUEUE       (#FN #ARG)
Q-PRIN1       (#E: any ):NIL
Q-PRINT       (#E: any ):NIL
Q-PRIN2       (#E: any ):NIL
Q-TERPRI      ()
ONEARG-TERPRI (#E: any ):NIL
Q-TYO         (#N: ascii-code ):NIL
Q-PRINC       (#C: single-char-id ):NIL
* Q-TTY-DE      (#CMDS: list ):NIL
* Q-TTY-XT-DE   (#CMDS: list ):NIL
* Q-TTY-TX-DE   (#CMDS: list ):NIL
* Q-TTY-TT-DE   (#CMDS: list ):NIL
")

(GLOBAL '(!#SELECTQ G!:SHOW!:ERRORS G!:SHOW!:TRACE))

(FLAG '(PRINT PRIN1 PRIN2 PRINC SETCUR TYO PPRINT TERPRI POSN PPOS)
      'SAY!:PRINT)

(DE PRINT2 (!#X) (PROGN (PRIN2 !#X) (TERPRI) !#X))

(DE CONFIRM (!#QUEST)
 (PROG (!#ANS)
  LP0  (TTY!-XT !#QUEST)
  LP1  (SEND)
       (SETQ !#ANS (UPPER!-CASE (READCH)))
       (COND ((EQUAL !#ANS !$EOL!$) (SETQ !#ANS (UPPER!-CASE (READCH)))))
       (COND ((EQUAL !#ANS 'Y) (PROGN (EATEOL) (RETURN T)))
             ((EQUAL !#ANS 'N) (PROGN (EATEOL) (RETURN NIL)))
             ((EQUAL !#ANS '!?) (PROGN (EATEOL) (GO LP0)))
             (T (PROGN (EATEOL) (TTY!-XT "Please type Y, N or ?."))))
       (GO LP1)))

(CDE UPPER!-CASE (CH)
 (PROG (TMP)
       (COND ((AND (LITER CH)
                   (SETQ TMP
                         (MEMQ CH
                               '(A B C D E F G H I J K L M N O P Q R S T U V 
W X Y Z))))   (RETURN
               (CAR (NTH!-TAIL
                     '(Z Y X W V U T S R Q P O N M L K J I H G F E D C B A)
                     (LENGTH TMP)))))
             (T (RETURN CH)))))

(!* DE CONFIRM (!#QUEST)
   (PROG (!#ANS)
    LP0  (TTY!-XT !#QUEST)
    LP1  (SEND)
         (SETQ !#ANS (CAR (EXPLODEC (READ))))
         (COND ((EQ !#ANS 'Y) (PROGN (EATEOL) (RETURN T)))
               ((EQ !#ANS 'N) (PROGN (EATEOL) (RETURN NIL)))
               ((EQ !#ANS '!?) (GO LP0))
               (T (TTY!-XT "Please type Y, N or ?.")))
         (GO LP1)))

(!* 
"Eat (discard) text until $EOL$ or <ESC> seen.
    <ESC> meaningful only on PDP-10 systems.
    $EOL$ meaningful only on correctly-implemented Standard-LISP systems. ")

(DE EATEOL NIL
 (PROG (!#CH)
  LP   (SETQ !#CH (READCH))
       (COND ((MEMQ !#CH (LIST '!$EOL!$ !$EOL!$)) (RETURN NIL)))
       (GO LP)))

(!* "An idea whose time has not yet come... ")

(!* DE TTY!-DE (EOLS!#BEFORE !#L EOLS!#AFTER)
   (PROG (OLD!#CHAN)
         (SETQ OLD!#CHAN (WRS NIL))
    LP1  (COND ((ONEP EOLS!#BEFORE) (TTY!-ELT !$EOL!$))
               ((ZEROP EOLS!#BEFORE) NIL)
               (T (PROGN (TTY!-ELT !$EOL!$)
                         (SETQ EOLS!#BEFORE (SUB1 EOLS!#BEFORE))
                         (GO LP1))))
         (MAPC !#L (FUNCTION TTY!-ELT))
    LP1  (COND ((ONEP EOLS!#AFTER) (TTY!-ELT !$EOL!$))
               ((ZEROP EOLS!#AFTER) NIL)
               (T (PROGN (TTY!-ELT !$EOL!$)
                         (SETQ EOLS!#AFTER (SUB1 EOLS!#AFTER))
                         (GO LP2))))
         (WRS OLD!#CHAN)))

(!* "So, for now at least, ... ")

(DE TTY!-DE (!#L)
 (PROG (OLD!#CHAN)
       (SETQ OLD!#CHAN (WRS NIL))
       (MAPC !#L (FUNCTION TTY!-ELT))
       (WRS OLD!#CHAN)))

(DE TTY!-TX!-DE (!#L)
 (PROG (OLD!#CHAN)
       (SETQ OLD!#CHAN (WRS NIL))
       (TTY!-ELT !$EOL!$)
       (MAPC !#L (FUNCTION TTY!-ELT))
       (WRS OLD!#CHAN)))

(DE TTY!-XT!-DE (!#L)
 (PROG (OLD!#CHAN)
       (SETQ OLD!#CHAN (WRS NIL))
       (MAPC !#L (FUNCTION TTY!-ELT))
       (TTY!-ELT !$EOL!$)
       (WRS OLD!#CHAN)))

(DE TTY!-TT!-DE (!#L)
 (PROG (OLD!#CHAN)
       (SETQ OLD!#CHAN (WRS NIL))
       (TTY!-ELT !$EOL!$)
       (MAPC !#L (FUNCTION TTY!-ELT))
       (TTY!-ELT !$EOL!$)
       (WRS OLD!#CHAN)))

(DE TTY!-ELT (!#E) (COND ((EQ !#E !$EOL!$) (Q!-TERPRI)) (T (Q!-PRIN2 !#E))))

(!* 
"PRINA( X:any ): any
    -----
    Prin2s expression, after TERPRIing if it is too big for line, or spacing
    if it is not at the beginning of a line.  Returns the value of X.
    Except for the space, this is just PRIN2 in the IBM interpreter.")

(DE PRINA (!#X)
 (PROGN
  (COND ((LEQ (CHRCT) (FLATSIZE !#X)) (TERPRI))
        ((GREATERP (POSN) 0) (PRIN2 " ")))
  (PRIN2 !#X)))

(!* 
"CHRCT (): <number>
     -----
  CHaRacter CounT left in line.
  Also a CDE in YPP.LSP -- built into IMSSS DEC interpreter.")

(CDE CHRCT NIL (DIFFERENCE (MIN 80 (LINELENGTH NIL)) (POSN)))

(!* 
"BINARY (#X: boolean): old-value
     ------
     Stub for non-IMSSS interpreters.
     In IMSSS interpreter, will put terminal into binary mode or
     take it out, according to argument, and return old value.")

(CDE BINARY (!#X) NIL)

(!* 
"PRIN1SQ (#X: any)
     -------
  PRIN1, Safe, use apostrophe for Quoted expressions.
  This is essentially a PRIN1 which tries not to exceed the right margin.
  It exceeds it only in those cases where the pname of a single atom
  exceeds the entire linelength.  In such cases, <big> is printed at the
  terminal as a warning.
  (QUOTE xxx) structures are printed in 'xxx form to save space.
  Again, this is a little superfluous for the IBM interpreter.
")

(DE PRIN1SQ (!#X)
 (PROG (!#SIZE)
       (COND ((ATOM !#X)
              (PROGN (SETQ !#SIZE (FLATSIZE !#X))
                     (COND ((LESSP (CHRCT) !#SIZE)
                            (PROGN (TERPRI)
                                   (COND ((LESSP (CHRCT) !#SIZE)
                                          (TTY "<big>"))))))
                     (RETURN (PRIN1 !#X))))
             ((AND (EQ (CAR !#X) 'QUOTE)
                   (CDR !#X)
                   (NULL (CDDR !#X))
                   (NOT (NUMBERP (CADR !#X))))
              (PROGN (PRINCS "'") (RETURN (PRIN1SQ (CADR !#X))))))
       (PRINCS "(")
  LP   (PRIN1SQ (CAR !#X))
       (SETQ !#X (CDR !#X))
       (COND ((NULL !#X) (RETURN (PRINCS ")"))))
       (PRINCS " ")
       (COND ((NULL (ATOM !#X)) (GO LP)))
       (PRINCS ".")
       (PRINCS " ")
       (PRIN1SQ !#X)
       (PRINCS ")")))

(!* 
"PRIN2SQ (#X: any)
    -------
  PRIN2, Safe, use apostrophe for Quoted expressions.
  Just like PRIN1SQ, but uses PRIN2 as a basis.
")

(DE PRIN2SQ (!#X)
 (PROG (!#SIZE)
       (COND ((ATOM !#X)
              (PROGN (SETQ !#SIZE (FLATSIZE !#X))
                     (COND ((LESSP (CHRCT) !#SIZE)
                            (PROGN (TERPRI)
                                   (COND ((LESSP (CHRCT) !#SIZE)
                                          (TTY "<big>"))))))
                     (RETURN (PRIN2 !#X))))
             ((AND (EQ (CAR !#X) 'QUOTE)
                   (CDR !#X)
                   (NULL (CDDR !#X))
                   (NOT (NUMBERP (CADR !#X))))
              (PROGN (PRINCS "'") (RETURN (PRIN2SQ (CADR !#X))))))
       (PRINCS "(")
  LP   (PRIN2SQ (CAR !#X))
       (SETQ !#X (CDR !#X))
       (COND ((NULL !#X) (RETURN (PRINCS ")"))))
       (PRINCS " ")
       (COND ((NULL (ATOM !#X)) (GO LP)))
       (PRINCS ".")
       (PRINCS " ")
       (PRIN2SQ !#X)
       (PRINCS ")")))

(!* 
"PRINCS (#X: single-character-atom)
    -------
  PRINC Safe.  Does a PRINC, but first worries about right margin.
")

(DE PRINCS (!#X) (PROGN (COND ((LESSP (CHRCT) 1) (TERPRI))) (PRINC !#X)))

(!* 
"1980 Jul 24 -- New Queued-I/O routines.
To interface other code to this new I/O method, the following changes
must be made in other code:
 PRIN2 --> TTY
 TERPRI --> $EOL$ inside a TTY, which causes Q-TERPRI to be called
 TYO --> Q-TYO
 PRIN1, PRINT -- These are used only for debugging.  Do a (SEND) just
        before starting to print things in realtime, or use Q-PRIN1 etc.
 TTY -- Ok, expands into TTY-DE which calls Q-PRIN2 and Q-TERPRI.
 SAY -- I don't know what to do with this crock.  It seems to be
        a poor substitute for TTY.  If so it can be changed to TTY
        with the arguments fixed to be correct.  <!GRAM>LPARSE.LSP
")

(GLOBAL
 '(!*BATCHOUT !*BATCHQUEUE !*BATCHMAX !*BATCHCNT G!:WASTED!:SENDS
   G!:GOOD!:SENDS G!:GOOD!:OUTPUTS))

(!* 
"When *BATCHOUT is NIL, output is done in realtime and *BATCHQUEUE
    remains NIL.  When *BATCHOUT is true, output is queued and SEND
    executes&dequeues it later.")

(!* "Initialize *BATCHQUEUE for TCONC operations.")

(SETQ !*BATCHQUEUE (NCONS NIL))

(!* "Initialize *BATCHMAX and *BATCHCNT ")

(SETQ !*BATCHMAX 100)

(SETQ !*BATCHCNT !*BATCHMAX)

(DE SEND NIL
 (PROGN
  (COND ((CAR !*BATCHQUEUE)
         (PROGN (SETQ G!:GOOD!:SENDS (ADD1 G!:GOOD!:SENDS))
                (SETQ G!:GOOD!:OUTPUTS
                      (PLUS G!:GOOD!:OUTPUTS (LENGTH (CAR !*BATCHQUEUE))))
                (MAPC (CAR !*BATCHQUEUE) (FUNCTION SEND!-1))
                (SETQ !*BATCHCNT !*BATCHMAX)
                (!* "Set it again up for TCONC's.")
                (SETQ !*BATCHQUEUE (NCONS NIL))))
        (T (SETQ G!:WASTED!:SENDS (ADD1 G!:WASTED!:SENDS))))))

(DE SEND!-1 (!#EE) (APPLY (CAR !#EE) (NCONS (CDR !#EE))))

(DE ENQUEUE (!#FN !#ARG)
 (PROGN (COND ((ZEROP (SETQ !*BATCHCNT (SUB1 !*BATCHCNT))) (SEND)))
        (SETQ !*BATCHQUEUE (TCONC !*BATCHQUEUE (CONS !#FN !#ARG)))))

(DE Q!-PRIN1 (!#E)
 (COND (!*BATCHOUT (ENQUEUE 'PRIN1 !#E)) (1 (PRIN1 !#E))))

(DE Q!-PRINT (!#E)
 (COND (!*BATCHOUT (ENQUEUE 'PRINT !#E)) (1 (PRINT !#E))))

(DE Q!-PRIN2 (!#E)
 (COND (!*BATCHOUT (ENQUEUE 'PRIN2 !#E)) (1 (PRIN2 !#E))))

(DE Q!-TERPRI NIL
 (COND (!*BATCHOUT (ENQUEUE 'ONEARG!-TERPRI NIL)) (1 (TERPRI))))

(DE ONEARG!-TERPRI (!#E) (TERPRI))

(DE Q!-TYO (!#N) (COND (!*BATCHOUT (ENQUEUE 'TYO !#N)) (1 (TYO !#N))))

(DE Q!-PRINC (!#C)
 (COND (!*BATCHOUT (ENQUEUE 'PRINC !#C)) (1 (PRINC !#C))))

(!* " These call PRIN2, so they would cause double-enqueuing. ")

(!* DE Q!-TTY!-DE (!#CMDS)
   (COND (!*BATCHOUT (ENQUEUE 'TTY!-DE !#CMDS)) (1 (TTY!-DE !#CMDS))))

(!* DE Q!-TTY!-XT!-DE (!#CMDS)
   (COND (!*BATCHOUT (ENQUEUE 'TTY!-XT!-DE !#CMDS)) (1 (TTY!-XT!-DE !#CMDS))))

(!* DE Q!-TTY!-TX!-DE (!#CMDS)
   (COND (!*BATCHOUT (ENQUEUE 'TTY!-TX!-DE !#CMDS)) (1 (TTY!-TX!-DE !#CMDS))))

(!* DE Q!-TTY!-TT!-DE (!#CMDS)
   (COND (!*BATCHOUT (ENQUEUE 'TTY!-TT!-DE !#CMDS)) (1 (TTY!-TT!-DE !#CMDS))))

(SETQ G!:WASTED!:SENDS (SETQ G!:GOOD!:SENDS (SETQ G!:GOOD!:OUTPUTS 0)))

(!* 
" YCNTRL -- ROUTINES INVOLVED WITH PROGRAM CONTROL STRUCTURES

CATCH     ( EXP:s-expression LABELS:id or idlist ):any    EXPR
THROW     ( VALU:any LABEL:id ): error label              EXPR
ERRSET-DE ( #EXP #LBL ):any                               EXPR
APPLY#    ( ARG1: function ARG2: argument:list ):any      EXPR
BOUND     ( X:any ):boolean                               EXPR
MKPROG    ( VARS:id-lst BODY:exp ):prog                   EXPR
BUG-STOP  (): any                                         EXPR
")

(GLOBAL '(!$THROWN!$ G!:SHOW!:ERRORS G!:SHOW!:TRACE))

(!*
(!* 
"CATCH( EXP:s-expression LABELS:id or idlist ): any  EXPR
    -----
    For use with throw.  If no THROW occurs in expression, then
    returns value of expression.  If thrown label is MEMQ or EQ to
    labels, then returns thrown value.  OW, thrown label is passed
    up higher.  Expression should be quoted, as in ERRORSET.")

(CDE CATCH (!#EXP !#LABELS)
 (PROG (!#EE)
       (COND ((PAIRP
               (SETQ !#EE (ERRORSET !#EXP G!:SHOW!:ERRORS G!:SHOW!:TRACE)))
              (RETURN (CAR !#EE)))
             ((OR (EQ !#LABELS T) (EQ !#EE !#LABELS) (MEMQ !#EE !#LABELS))
              (RETURN !$THROWN!$))
             (T (ERROR !#EE NIL)))))

(!* 
"THROW( VALU:any LABEL:id ): error label             EXPR
    -----
    Throws value with label up to enclosing CATCH having label.
    If there is no such CATCH, causes error.")

(CDE THROW (!#VALU !#LABEL)
 (PROGN (SETQ !$THROWN!$ !#VALU) (ERROR !#LABEL NIL)))
)

(!* 
"ERRSET-DE ( EXP LBL ):any                     EXPR
    Named errset.  If error matches label, then acts like errorset.
    Otherwise propagates error upward.
    Matching:  Every label stops errors NIL, $EOF$.
               Label 'ERRORX stops any error.
               Other labels stop errors whose first arg is EQ to them.
    Usually called via ERRSET macro.")

(DE ERRSET!-DE (!#EXP !#LBL)
 (PROG (!#Y)
       (SETQ !#Y (ERRORSET !#EXP G!:SHOW!:ERRORS G!:SHOW!:TRACE))
       (COND ((OR (PAIRP !#Y)
                  (NULL !#Y)
                  (EQ !#Y '!$EOF!$)
                  (EQ !#Y !#LBL)
                  (EQ !#LBL 'ERRORX))
              (RETURN !#Y))
             (T (ERROR !#Y "propagated")))))

(!* 
"APPLY#(ARG1: function ARG2: argument:list): any     EXPR
    ------
    Like APPLY, but can use fexpr and macro functions.")

(CDE APPLY!# (!#ARG1 !#ARG2) (EVAL (CONS !#ARG1 !#ARG2)))

(!* 
"BOUND( X:any ): boolean                             EXPR
    -----
    Returns T if X is a bound id.")

(CDE BOUND (!#X) (AND (IDP !#X) (PAIRP (ERRORSET !#X NIL NIL))))

(!* 
"MKPROG( VARS:id-lst BODY:exp )       EXPR
    ------
    Makes a prog around the body, binding the vars.")

(CDE MKPROG (!#VARS !#BODY) (CONS 'PROG (CONS !#VARS !#BODY)))

(!* 
"BUGSTOP ():NIL                       EXPR
    -------
    Enter a read/eval/print loop, exit when OK is seen.")

(DE BUG!-STOP (!#STR)
 (PROG (!#EXP OLD!#ICHAN OLD!#OCHAN OLD!#LENGTH)
       (SETQ OLD!#ICHAN (RDS NIL))
       (SETQ OLD!#OCHAN (WRS NIL))
       (SETQ OLD!#LENGTH (LINELENGTH NIL))
       (LINELENGTH 78)
       (COND ((PAIRP !#STR) (TTY!-DE !#STR)) (T (PRIN2 !#STR)))
  LOOP (TERPRI)
       (PRIN2 "--Bug Stop-- Type OK to continue.")
       (TERPRI)
       (SETQ !#EXP (ERRORSET '(READ) T NIL))
       (COND ((ATOM !#EXP) (PROGN (PRIN2 " --Read failed-- ") (GO LOOP))))
       (SETQ !#EXP (CAR !#EXP))
       (COND ((EQ !#EXP 'OK)
              (PROGN (EATEOL)
                     (PRIN2 "resuming... ")
                     (TERPRI)
                     (LINELENGTH OLD!#LENGTH)
                     (RDS OLD!#ICHAN)
                     (WRS OLD!#OCHAN)
                     (RETURN NIL)))
             ((AND (PAIRP !#EXP) (EQ (CAR !#EXP) 'RETURN))
              (PROGN (EATEOL)
                     (PRIN2 "returning... ")
                     (TERPRI)
                     (LINELENGTH OLD!#LENGTH)
                     (RDS OLD!#ICHAN)
                     (WRS OLD!#OCHAN)
                     (RETURN (EVAL (CADR !#EXP))))))
       (SETQ !#EXP (ERRORSET !#EXP T NIL))
       (COND ((ATOM !#EXP) (PRIN2 " --EVAL failed-- "))
             (T (PRIN1 (CAR !#EXP))))
       (GO LOOP)))

(!* 
" YRARE -- ROUTINES WHICH ARE USED, BUT OF DUBIOUS USEFULNESS
                ?? DELETE THESE ??

LOADV   ( V:vector FN:function ):vector         EXPR
AMONG   ( ALST KEY ITEM )                       EXPR
INSERT  ( ITEM ALST KEY )                       EXPR
DCONS   ( X:any Y:list ):list                   EXPR
SUBLIST ( X:list P1:integer P2:integer ):list   EXPR
SUBLIST1( Y )                                   EXPR
LDIFF   ( X:list Y:list ):list          EXPR  used in editor/copy in ZEDIT
MAPCAR# ( L:list FN:function ):any              EXPR
MAP#    ( L:list FN:function ):any              EXPR
INITIALP( X:list Y:list ):boolean               EXPR
SUBLISTP( X:list Y:list ):list                  EXPR
INITQ   ( X:any Y:list R:fn ):boolean           EXPR

")

(!* 
"LOADV( V:vector FN:function ):vector        EXPR
    -----
    Loads vector with values.  Function should be 1-place numerical.
    V[I] _ FN( I ).
    If value of function is 'novalue, then doesn't change value. ??")

(CDE LOADV (!#V !#FN)
 (PROG (!#CTR !#LEN)
       (COND ((NOT (SETQ !#LEN (VECTORP !#V))) (RETURN !#V)))
       (SETQ !#CTR 0)
  LOOP (PUTV !#V !#CTR (APPLY !#FN (LIST !#CTR)))
       (COND ((LESSP !#CTR !#LEN) (PROGN (MAKE !#CTR 1) (GO LOOP))))
       (RETURN !#V)))

(!* 
"AMONG(ALST:association-list KEY:atom ITEM:atom):boolean     EXPR
    -----
    Tests if item is found under key in association list.
    Uses EQUAL tests.")

(CDE AMONG (!#ALST !#KEY !#ITEM)
 (PROG (RES)
       (SETQ RES
             (ERRORSET
              (LIST 'AMONG1 (MKQUOTE !#ALST) (MKQUOTE !#KEY) (MKQUOTE !#ITEM))
              NIL
              NIL))
       (COND ((EQ RES 'FOUND) (RETURN T))
             ((NULL RES) (RETURN NIL))
             ((ATOM RES) (ERROR RES NIL)))))

(CDE AMONG1 (!#ALST !#KEY !#ITEM)
 (MAPC !#ALST
       (FUNCTION
        (LAMBDA (!#ENTRY)
         (AND (EQUAL (CAR !#ENTRY) !#KEY)
              (MEMQ !#ITEM (CDR !#ENTRY))
              (ERROR 'FOUND NIL))))))

(!* 
"INSERT (ITEM:item ALST:association:list KEY:any):association list
    ------
    EXPR (destructive operation on ALST)
    Inserts item in association list under key  or if key not present
    adds (KEY ITEM) to the ALST.")

(CDE INSERT (!#ITEM !#ALST !#KEY)
 (PROG (!#AS!:ITEM)
       (COND ((SETQ !#AS!:ITEM (ASSOC !#KEY !#ALST))
              (COND ((NOT (MEMBER !#ITEM (CCDR !#AS!:ITEM)))
                     (RPLACD !#AS!:ITEM (CONS !#ITEM (CDR !#AS!:ITEM))))))
             (T (DCONS (LIST !#KEY !#ITEM) !#ALST)))
       (RETURN !#ALST)))

(!* 
"DCONS( X:any Y:list ):list                          EXPR
    -----
    Destructively cons x to list.")

(CDE DCONS (!#X !#Y)
 (PROGN (RPLACD !#Y (CONS (CAR !#Y) (CDR !#Y))) (RPLACA !#Y !#X)))

(!* 
"SUBLIST( X:list P1:integer P2:integer ):list        EXPR
    -------
    Returns sublist from p1 to p2 positions, negatives counting from end.
    I.e., (SUBLIST '(A B C D E) 2 -2) = (B C D)")

(CDE SUBLIST (!#X !#P1 !#P2)
 (LDIFF (NTH!-TAIL !#X (SETQ !#P1 (SUBLIST1 !#X !#P1)))
        (NTH!-TAIL !#X (ADD1 (SUBLIST1 !#X !#P2)))))

(CDE SUBLIST1 (!#X !#Y)
 (COND ((LESSP !#Y 0) (MAX 1 (PLUS 1 !#Y (LENGTH !#X)))) (T !#Y)))

(!* 
"LDIFF( X:list Y:list ):list                         EXPR
    -----
    If X is a tail of Y, returns the list difference of X and Y,
    a list of the elements of Y preceeding X.")

(CDE LDIFF (!#X !#Y)
 (COND ((OR (EQ !#X !#Y) (ATOM !#X)) NIL)
       ((NULL !#Y) !#X)
       (T (PROG (!#V !#Z)
                (SETQ !#Z (SETQ !#V (NCONS (CAR !#X))))
           LOOP (SETQ !#X (CDR !#X))
                (COND ((OR (EQ !#X !#Y) (ATOM !#X)) (RETURN !#Z)))
                (SETQ !#V (CDR (RPLACD !#V (NCONS (CAR !#X)))))
                (GO LOOP)))))

(!* 
"MAPCAR#( L:list FN:function ):any                   EXPR
    -------
    Extends mapcar to work on general s-expressions as well as lists.
    The return is of same form, i.e.
                (MAPCAR# 'ATOM '(A B C . D)) = (T T T . T)
    Also, if for any member of list the variable SPLICE is set to
    true by function, then for that member the return from the
    function is spliced into the return.")

(CDE MAPCAR!# (!#L !#FN)
 (PROG (!#M !#SPLICE !#TEMP)
       (SETQ !#M (NCONS NIL))
  LOOP (COND ((NULL !#L) (RETURN (CAR !#M)))
             ((ATOM !#L)
              (RETURN
               (COND ((NULL (CAR !#M)) (APPLY !#FN (LIST !#L)))
                     (T (PROGN (RPLACD (CDR !#M) (APPLY !#FN (LIST !#L)))
                               (CAR !#M)))))))
       (SETQ !#TEMP (APPLY !#FN (LIST (CAR !#L))))
       (COND (!#SPLICE (PROGN (SETQ !#SPLICE NIL) (LCONC !#M !#TEMP)))
             (T (TCONC !#M !#TEMP)))
       (SETQ !#L (CDR !#L))
       (GO LOOP)))

(!* 
"MAP#( L:list FN:function ):any                      EXPR
    ----
    Extends map to work on general s-expressions as well as lists.")

(CDE MAP!# (!#L !#FN)
 (PROG (!#MAPPED)
  LOOP (COND ((NULL !#L) (RETURN !#MAPPED)))
       (APPLY !#FN (LIST !#L))
       (COND ((ATOM !#L) (RETURN !#MAPPED)))
       (SETQ !#L (CDR !#L))
       (GO LOOP)))

(!* 
"INITIALP( X:list Y:list ):boolean           EXPR
    --------
    Returns T if X is EQUAL to some ldiff of Y.")

(CDE INITIALP (!#X !#Y)
 (COND ((NULL !#X) (COND (!#Y !#Y) (T T)))
       ((NULL !#Y) NIL)
       ((NOT (EQUAL (CAR !#X) (CAR !#Y))) NIL)
       (T (INITIALP (CDR !#X) (CDR !#Y)))))

(!* 
"SUBLISTP( X:list Y:list ):list              EXPR
    --------
    Returns a tail of Y (or T) if X is a sublist of Y.")

(CDE SUBLISTP (!#X !#Y)
 (COND ((NULL !#X) (COND (!#Y !#Y) (T T)))
       ((NULL !#Y) NIL)
       ((INITIALP !#X !#Y) T)
       (T (SUBLISTP !#X (CDR !#Y)))))

(!* 
"INITQ( X:any Y:list R:fn ):boolean          EXPR
    -----
    Returns T if x is an initial portion of Y under the relation R.")

(CDE INITQ (!#X !#Y !#R)
 (COND ((OR (NULL !#X) (NULL !#Y)) NIL)
       ((APPLY !#R (LIST (CAR !#X) (CAR !#Y)))
        (CONS (CAR !#X) (INITQ (CDR !#X) (CDR !#Y) !#R)))))

