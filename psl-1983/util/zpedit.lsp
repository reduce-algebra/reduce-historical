(!* 
"ZPEDIT contains two packages --
     (1) YPP -- a derivative of the ILISP pretty-printer.
     (2) YEDIT -- a derivative of the ILISP form-oriented editor. ")

(!* 
" YPP -- THE PRETTYPRINTER

PP( LST:list )                        FEXPR
PP1( X:any )                          EXPR
PP-VAL ( X:id )                       EXPR
PP-DEF ( X:id )                       EXPR
SPRINT( X:any COL:number )            EXPR
and others...

")

(FLUID
 '(PP!#PROPS PP!#FLAGS PRINTMACRO COMMENTCOL COMMENTFLG CONTOURFLG PPPRINT))

(FLUID '(!#FILE))

(SETQ PP!#PROPS '(READMACRO PRINTMACRO))

(SETQ PP!#FLAGS '(FLUID GLOBAL))

(SETQ COMMENTCOL 50)

(SETQ COMMENTFLG NIL)

(SETQ CONTOURFLG T)

(!* "Tell the loader we need ZBasic and ZMacro.")

(IMPORTS '(ZBOOT ZBASIC ZMACRO))

(!* "Change the system prettyprint function to use this one.")

(DE PRETTYPRINT (!#X) (PROGN (SPRINT !#X 1) (TERPRI)))

(!* "Tell editor to use SPRINT for PP command.")

(SETQ PPPRINT 'SPRINT)

(PUT 'QUOTE 'PRINTMACRO '!#QUOTE)

(PUT '!* 'PRINTMACRO '!#!*)

(CDF PP (!#L) (PROGN (MAPC !#L (FUNCTION PP1)) (TERPRI) T))

(DF PPL (!#L)
 (PROG (!#FILE)
       (SETQ !#L
             (APPLY (FUNCTION APPEND) (MAPCAR !#L (FUNCTION ADD!#SELF!#REF))))
       (!* "Print the readmacros at the front of the file in a PROGN")
       (!* "#FILE becomes non-nil when printing to files")
       (WRS (SETQ !#FILE (WRS NIL)))
       (COND ((AND !#FILE (MEMQ 'READMACRO PP!#PROPS))
              (PROGN (MAPC !#L (FUNCTION FPP!#READMACRO))
                     (!* "Trick: #FILE is now NIL if readmacros were printed")
                     (COND ((NULL !#FILE)
                            (PROGN (SPRINT ''READMACROS!-LOADED 1)
                                   (PRIN2 ")")))))))
       (MAPC !#L (FUNCTION PP1))))

(!* "SETCHR is only meaningful in the dec slisp, where it is defined")

(CDE SETCHR (CHR FLAGS) NIL)

(DE FPP!#READMACRO (!#A)
 (COND ((GET !#A 'READMACRO)
        (PROGN (!* "Put the readmacros inside a PROGN")
               (COND (!#FILE
                      (PROGN (TERPRI) (PRIN2 "(PROGN") (SETQ !#FILE NIL))))
               (SPRINT (LIST 'SETCHR (LIST 'QUOTE !#A) (SETCHR !#A NIL)) 
2)))))

(DE PP1 (!#EXP)
 (PROG NIL
       (TERPRI)
       (COND ((IDP !#EXP)
              (PROG (!#PROPS !#FLAGS)
                    (SETQ !#PROPS PP!#PROPS)
               LP1  (COND (!#PROPS
                           (PROGN (PP!-PROP !#EXP (CAR !#PROPS))
                                  (SETQ !#PROPS (CDR !#PROPS))
                                  (GO LP1))))
                    (SETQ !#FLAGS PP!#FLAGS)
               LP2  (COND (!#FLAGS
                           (PROGN (PP!-FLAG !#EXP (CAR !#FLAGS))
                                  (SETQ !#FLAGS (CDR !#FLAGS))
                                  (GO LP2))))
                    (PP!-VAL !#EXP)
                    (PP!-DEF !#EXP)))
             (T (PROGN (SPRINT !#EXP 1) (TERPRI))))))

(DE PP!-VAL (!#ID)
 (PROG (!#VAL)
       (COND ((ATOM (SETQ !#VAL (ERRORSET !#ID NIL NIL))) (RETURN NIL)))
       (TERPRI)
       (PRIN2 "(SETQ ")
       (PRIN1 !#ID)
       (S2PRINT " '" (CAR !#VAL))
       (PRIN2 ")")
       (TERPRI)))

(DE PP!-DEF (!#ID)
 (PROG (!#DEF !#TYPE ORIG!#DEF)
       (SETQ !#DEF (GETD !#ID))
  TEST (COND ((NULL !#DEF)
              (RETURN
               (AND ORIG!#DEF
                    (WARNING
                     (LIST "Gack. " !#ID " has no unbroken definition.")))))
             ((ATOM !#DEF)
              (RETURN (WARNING (LIST "Bad definition for " !#ID " : " !#DEF))))
             ((CODEP (CDR !#DEF))
              (RETURN (WARNING (LIST "Can't PP compiled def for " !#ID))))
             ((NOT (AND (CDR !#DEF)
                        (EQ (CADR !#DEF) 'LAMBDA)
                        (CDDR !#DEF)
                        (CDDDR !#DEF)
                        (NULL (CDDDDR !#DEF))))
              (WARNING (LIST !#ID " has ill-formed definition.")))
             ((AND (NOT ORIG!#DEF) (BROKEN !#ID))
              (PROGN (WARNING (LIST "Note: " !#ID " is broken or traced."))
                     (SETQ ORIG!#DEF !#DEF)
                     (SETQ !#DEF (GET!#GOOD!#DEF !#ID))
                     (GO TEST))))
       (SETQ !#TYPE (CAR !#DEF))
       (TERPRI)
       (COND ((EQ !#TYPE 'EXPR) (PRIN2 "(DE "))
             ((EQ !#TYPE 'FEXPR) (PRIN2 "(DF "))
             ((EQ !#TYPE 'MACRO) (PRIN2 "(DM "))
             (T (RETURN (WARNING (LIST "Bad fntype for " !#ID " : " !#TYPE)))))
       (PRIN1 !#ID)
       (PRIN2 " ")
       (PRIN1 (CADDR !#DEF))
       (MAPC (CDDDR !#DEF) (FUNCTION (LAMBDA (!#X) (S2PRINT " " !#X))))
       (PRIN2 ")")
       (TERPRI)))

(DE BROKEN (!#X) (GET !#X 'TRACE))

(DE GET!#GOOD!#DEF (!#X)
 (PROG (!#XX!#)
       (COND ((AND (SETQ !#XX!# (GET !#X 'TRACE))
                   (IDP (SETQ !#XX!# (CDR !#XX!#))))
              (RETURN (GETD !#XX!#))))))

(DE PP!-PROP (!#ID !#PROP)
 (PROG (!#VAL)
       (COND ((NULL (SETQ !#VAL (GET !#ID !#PROP))) (RETURN NIL)))
       (TERPRI)
       (PRIN2 "(PUT '")
       (PRIN1 !#ID)
       (PRIN2 " '")
       (PRIN1 !#PROP)
       (S2PRINT " '" !#VAL)
       (PRIN2 ")")
       (TERPRI)))

(DE PP!-FLAG (!#ID !#FLAG)
 (PROG NIL
       (COND ((NULL (FLAGP !#ID !#FLAG)) (RETURN NIL)))
       (TERPRI)
       (PRIN2 "(FLAG '(")
       (PRIN1 !#ID)
       (PRIN2 ") '")
       (PRIN1 !#FLAG)
       (PRIN2 ")")
       (TERPRI)))

(DE ADD!#SELF!#REF (!#ID)
 (PROG (!#L)
       (COND ((NOT (MEMQ !#ID (SETQ !#L (EVAL !#ID))))
              (PROGN (RPLACD !#L (CONS (CAR !#L) (CDR !#L)))
                     (RPLACA !#L !#ID))))
       (RETURN !#L)))

(!* "S2PRINT: prin2 a string and then sprint an expression.")

(DE S2PRINT (!#S !#EXP)
 (PROGN
  (OR (GREATERP (SPACES!#LEFT) (PLUS (FLATSIZE2 !#S) (FLATSIZE !#EXP)))
      (TERPRI))
  (PRIN2 !#S)
  (SPRINT !#EXP (ADD1 (POSN)))))

(DE SPRINT (!#EXP LEFT!#MARGIN)
 (PROG (ORIGINAL!#SPACE NEW!#SPACE CAR!#EXP P!#MACRO CADR!#MARGIN ELT!#MARGIN
        LBL!#MARGIN !#SIZE)
       (COND ((ATOM !#EXP)
              (PROGN (SAFE!#PPOS LEFT!#MARGIN (FLATSIZE !#EXP))
                     (RETURN (PRIN1 !#EXP)))))
       (PPOS LEFT!#MARGIN)
       (SETQ LEFT!#MARGIN (ADD1 LEFT!#MARGIN))
       (SETQ ORIGINAL!#SPACE (SPACES!#LEFT))
       (COND ((PAIRP (SETQ CAR!#EXP (CAR !#EXP)))
              (PROGN (PRIN2 "(") (SPRINT CAR!#EXP LEFT!#MARGIN)))
             ((AND (IDP CAR!#EXP) (SETQ P!#MACRO (GET CAR!#EXP 'PRINTMACRO)))
              (COND ((STRINGP P!#MACRO)
                     (PROGN (SAFE!#PPOS (POSN1) (FLATSIZE2 P!#MACRO))
                            (PRIN2 P!#MACRO)
                            (RETURN
                             (AND (CDR !#EXP) (SPRINT (CADR !#EXP) (POSN1))))))
                    (T (PROGN (SETQ PRINTMACRO NIL)
                              (SETQ !#EXP (APPLY P!#MACRO (LIST !#EXP)))
                              (COND ((NULL PRINTMACRO) (RETURN NIL))
                                    ((ATOM PRINTMACRO)
                                     (PROGN (SETQ CAR!#EXP PRINTMACRO)
                                            (PRIN2 "(")
                                            (SPRINT (CAR !#EXP) LEFT!#MARGIN)))
                                    (T (PROGN
                                        (SETQ CADR!#MARGIN
                                              (SETQ ELT!#MARGIN
                                                    (CDR PRINTMACRO)))
                                        (SETQ LBL!#MARGIN
                                              (COND ((EQ
                                                      (CAR PRINTMACRO)
                                                      'PROG)
                                                     LEFT!#MARGIN)
                                                    (T CADR!#MARGIN)))
                                        (GO B))))))))
             (T (PROGN (PRIN2 "(")
                       (SAFE!#PPOS (POSN1) (FLATSIZE CAR!#EXP))
                       (PRIN1 CAR!#EXP))))
       (COND ((ATOM (SETQ !#EXP (CDR !#EXP))) (GO C)))
       (SETQ CADR!#MARGIN (POSN2))
       (SETQ NEW!#SPACE (SPACES!#LEFT))
       (SETQ !#SIZE (PPFLATSIZE CAR!#EXP))
       (COND ((NOT (LESSP !#SIZE ORIGINAL!#SPACE))
              (SETQ CADR!#MARGIN
                    (SETQ ELT!#MARGIN (SETQ LBL!#MARGIN LEFT!#MARGIN))))
             ((EQ CAR!#EXP '!*)
              (PROGN
               (SETQ LEFT!#MARGIN (SETQ CADR!#MARGIN (PLUS LEFT!#MARGIN 
2)))           (SETQ ELT!#MARGIN (SETQ LBL!#MARGIN NIL))))
             ((OR (LESSP (PPFLATSIZE !#EXP) NEW!#SPACE)
                  (PROG (!#E1)
                        (SETQ !#E1 !#EXP)
                   LP   (COND ((PAIRP (CAR !#E1)) (RETURN NIL))
                              ((ATOM (SETQ !#E1 (CDR !#E1))) (RETURN T))
                              (T (GO LP)))))
              (SETQ ELT!#MARGIN (SETQ LBL!#MARGIN NIL)))
             ((LESSP NEW!#SPACE 24)
              (PROGN
               (COND ((NOT (AND (MEMQ CAR!#EXP
                                      '(SETQ LAMBDA PROG SELECTQ SET))
                                (LESSP (PPFLATSIZE (CAR !#EXP)) NEW!#SPACE)))
                      (SETQ CADR!#MARGIN LEFT!#MARGIN)))
               (SETQ ELT!#MARGIN (SETQ LBL!#MARGIN LEFT!#MARGIN))))
             ((EQ CAR!#EXP 'LAMBDA)
              (SETQ ELT!#MARGIN (SETQ LBL!#MARGIN LEFT!#MARGIN)))
             ((EQ CAR!#EXP 'PROG)
              (PROGN (SETQ ELT!#MARGIN CADR!#MARGIN)
                     (SETQ LBL!#MARGIN LEFT!#MARGIN)))
             ((OR (GREATERP !#SIZE 14)
                  (AND (GREATERP !#SIZE 4)
                       (NOT (LESSP (PPFLATSIZE (CAR !#EXP)) NEW!#SPACE))))
              (SETQ CADR!#MARGIN
                    (SETQ ELT!#MARGIN (SETQ LBL!#MARGIN LEFT!#MARGIN))))
             (T (SETQ ELT!#MARGIN (SETQ LBL!#MARGIN CADR!#MARGIN))))
       (COND ((ATOM (SETQ CAR!#EXP (CAR !#EXP)))
              (PROGN (SAFE!#PPOS CADR!#MARGIN (PPFLATSIZE CAR!#EXP))
                     (PRIN1 CAR!#EXP)))
             (T (SPRINT CAR!#EXP CADR!#MARGIN)))
  A    (COND ((ATOM (SETQ !#EXP (CDR !#EXP))) (GO C)))
  B    (SETQ CAR!#EXP (CAR !#EXP))
       (COND ((ATOM CAR!#EXP)
              (PROGN (SETQ !#SIZE (PPFLATSIZE CAR!#EXP))
                     (COND (LBL!#MARGIN (SAFE!#PPOS LBL!#MARGIN !#SIZE))
                           ((LESSP !#SIZE (SPACES!#LEFT)) (PRIN2 " "))
                           (T (SAFE!#PPOS LEFT!#MARGIN !#SIZE)))
                     (PRIN1 CAR!#EXP)))
             (T (SPRINT CAR!#EXP (COND (ELT!#MARGIN ELT!#MARGIN) (T (POSN2)))))
        )
       (GO A)
  C    (COND (!#EXP
              (PROGN (COND ((LESSP (SPACES!#LEFT) 3) (PPOS LEFT!#MARGIN)))
                     (PRIN2 " . ")
                     (SETQ !#SIZE (PPFLATSIZE !#EXP))
                     (COND ((GREATERP !#SIZE (SPACES!#LEFT))
                            (SAFE!#PPOS LEFT!#MARGIN !#SIZE)))
                     (PRIN1 !#EXP))))
       (COND ((LESSP (SPACES!#LEFT) 1) (PPOS LEFT!#MARGIN)))
       (PRIN2 ")")))

(DE SPRIN1 (!#EXP !#C1 !#C2)
 (PROG (!#ROOM)
       (SETQ !#ROOM (DIFFERENCE (LINELENGTH NIL) !#C1))
       (COND ((GREATERP (PLUS (FLATSIZE !#EXP) 3) !#ROOM)
              (COND ((NULL (STRINGP !#EXP)) (SPRINT !#EXP !#C2))
                    ((FIRSTLINE!-FITS !#EXP !#ROOM)
                     (PROGN (PPOS !#C1) (PRIN1 !#EXP)))
                    (T (PROGN (TERPRI) (PRIN1 !#EXP)))))
             (T (SPRINT !#EXP !#C1)))))

(DE SPRINL (!#EXP !#C1 !#C2)
 (PROG (!#SIZE)
       (COND ((ATOM !#EXP) (RETURN (SPRIN1 !#EXP !#C1 !#C2)))
             (T (PROGN (PPOS !#C1) (PRIN2 "("))))
  A    (SPRIN1 (CAR !#EXP) (ADD1 !#C1) !#C2)
       (COND ((NULL (SETQ !#EXP (CDR !#EXP)))
              (PROGN (COND ((LESSP (SPACES!#LEFT) 1) (PPOS !#C2)))
                     (RETURN (PRIN2 ")"))))
             ((ATOM !#EXP)
              (PROGN (COND ((LESSP (SPACES!#LEFT) 3) (PPOS !#C1)))
                     (PRIN2 " . ")
                     (SETQ !#SIZE (ADD1 (PPFLATSIZE !#EXP)))
                     (COND ((GREATERP !#SIZE (SPACES!#LEFT))
                            (SAFE!#PPOS !#C1 !#SIZE)))
                     (PRIN1 !#EXP)
                     (PRIN2 ")")))
             (T (PROGN (SETQ !#C1 (POSN1)) (GO A))))))

(DE !#QUOTE (!#L)
  (!#QUOTES !#L "'"))

(DE !#QUOTES (!#L !#CH)
 (PROG (!#N)
       (COND ((ATOM (CDR !#L))
	      (PROGN (SETQ !#N (POSN1)) (SPRINL !#L !#N (PLUS !#N 3))))
	     (T (PROGN (PRIN2 !#CH)
		       (SETQ !#N (POSN1))
		       (SPRIN1 (CADR !#L) !#N !#N))))))

(!* "Addition for PSL, backquote and friends.")

(PUT 'BACKQUOTE 'PRINTMACRO '!#BACKQUOTE)

(DE !#BACKQUOTE (!#L)
  (!#QUOTES !#L "`"))

(PUT 'UNQUOTE 'PRINTMACRO '!#UNQUOTE)

(DE !#UNQUOTE (!#L)
  (!#QUOTES !#L ","))

(PUT 'UNQUOTEL 'PRINTMACRO '!#UNQUOTEL)

(DE !#UNQUOTEL (!#L)
  (!#QUOTES !#L ",@"))

(PUT 'UNQUOTED 'PRINTMACRO '!#UNQUOTED)

(DE !#UNQUOTED (!#L)
  (!#QUOTES !#L ",."))

(DE !#!* (!#L)
 (PROG (!#F !#N)
       (COND ((ATOM (CDR !#L))
              (RETURN (SPRINL !#L (SETQ !#N (POSN1)) (PLUS !#N 3)))))
       (!* COND ((EQ (CADR !#L) 'E) (EVAL (CADDR !#L))))
       (WRS (SETQ !#F (WRS NIL)))
       (COND ((OR !#F COMMENTFLG)
              (SPRINL !#L
                      (COND (CONTOURFLG (POSN1)) (T COMMENTCOL))
                      (PLUS (COND (CONTOURFLG (POSN1)) (T COMMENTCOL)) 
3)))         (T (PRIN2 "(* ...)")))))

(!* DE SPRINL (!#EXP !#C1 !#C2)
   (PROG NIL
         (COND ((ATOM !#EXP) (RETURN (SPRIN1 !#EXP !#C1 !#C2)))
               (T (PROGN (PPOS !#C1) (PRIN2 "("))))
    A    (SPRIN1 (CAR !#EXP) (ADD1 !#C1) !#C2)
         (COND ((NULL (SETQ !#EXP (CDR !#EXP)))
                (PROGN (COND ((LESSP (SPACES!#LEFT) 1) (PPOS !#C2)))
                       (RETURN (PRIN2 ")"))))
               (T (PROGN (SETQ !#C1 (POSN1)) (GO A))))))

(!* DE !#QUOTE (!#L)
   (PROG (!#N)
         (COND ((NUMBERP (CADR !#L))
                (SPRINL !#L (SETQ !#N (POSN1)) (PLUS !#N 3)))
               (T (PROGN (PRIN2 "'")
                         (SETQ !#N (POSN1))
                         (SPRIN1 (CADR !#L) !#N !#N))))))

(!* DE !#!* (!#L)
   (PROG (!#F)
         (COND ((EQ (CADR !#L) 'E) (EVAL (CADDR !#L))))
         (WRS (SETQ !#F (WRS NIL)))
         (COND ((OR !#F COMMENTFLG)
                (SPRINL !#L
                        (COND (CONTOURFLG (POSN1)) (T COMMENTCOL))
                        (PLUS (COND (CONTOURFLG (POSN1)) (T COMMENTCOL)) 
3)))           (T (PRIN2 "(* ...)")))))

(DE PRINCOMMA (!#LIST FIRST!#COL)
 (COND (!#LIST
        (PROGN (PRIN2 (CAR !#LIST))
               (MAPC (CDR !#LIST)
                     (FUNCTION
                      (LAMBDA (ELT)
                       (PROGN (PRIN2 ", ")
                              (COND ((LESSP (SPACES!#LEFT)
                                            (PLUS 2 (FLATSIZE2 ELT)))
                                     (PROGN (TERPRI) (PPOS FIRST!#COL))))
                              (PRIN2 ELT)))))
               (PRIN2 ".")))))

(CDE CHRCT NIL (DIFFERENCE (MIN 80 (LINELENGTH NIL)) (POSN)))

(DE SPACES!#LEFT NIL (SUB1 (CHRCT)))

(DE SAFE!#PPOS (!#N !#SIZE)
 (PROG (MIN!#N)
       (SETQ MIN!#N (SUB1 (DIFFERENCE (LINELENGTH NIL) !#SIZE)))
       (COND ((LESSP MIN!#N !#N)
              (PROGN (OR (GREATERP MIN!#N (POSN1)) (TERPRI)) (PPOS MIN!#N)))
             (T (PPOS !#N)))))

(DE PPFLATSIZE (!#EXP) (DIFFERENCE (FLATSIZE !#EXP) (PP!#SAVINGS !#EXP)))

(DE PP!#SAVINGS (Y)
 (PROG (N)
       (COND ((ATOM Y) (RETURN 0))
             ((AND (EQ (CAR Y) 'QUOTE) (CDR Y) (NOT (NUMBERP (CADR Y))))
              (RETURN (PLUS 7 (PP!#SAVINGS (CDR Y))))))
       (SETQ N 0)
  LP   (COND ((ATOM Y) (RETURN N)))
       (SETQ N (PLUS N (PP!#SAVINGS (CAR Y))))
       (SETQ Y (CDR Y))
       (GO LP)))

(DE FIRSTLINE!-FITS (!#STR !#N)
 (PROG (!#BIG)
       (!* "This addition is an empirical hack")
       (SETQ !#N (PLUS2 !#N 2))
       (SETQ !#BIG (EXPLODE !#STR))
  LP   (COND ((EQ (CAR !#BIG) !$EOL!$) (RETURN T))
             ((NULL (SETQ !#BIG (CDR !#BIG))) (RETURN T))
             ((ZEROP (SETQ !#N (SUB1 !#N))) (RETURN NIL)))
       (GO LP)))

(DE POSN1 NIL (ADD1 (POSN)))

(DE POSN2 NIL (PLUS 2 (POSN)))

(DE PPOS (N)
 (PROG NIL
       (OR (GREATERP N (POSN)) (TERPRI))
       (SETQ N (SUB1 N))
  LOOP (COND ((LESSP (POSN) N) (PROGN (PRIN2 " ") (GO LOOP))))))

(!* " YEDIT -- THE EDITOR "

" Originally from ilisp editor -- see zedit.doc for evolution.

EDITF (X)                 FEXPR
EDITFNS (X)               FEXPR
EDITV (X)                 FEXPR
EDITP (X)                 FEXPR
EDITE (EXPR COMS ATM)     EXPR

")

(!* "Due to deficiency in standard-lisp")

(GLOBAL '(!#SELECTQ G!:SHOW!:ERRORS G!:SHOW!:TRACE))

(!* "G!:EDIT!:ERRORS and G!:EDIT!:TRACE switch editor errorset args on/off")

(GLOBAL '(G!:EDIT!:ERRORS G!:EDIT!:TRACE))

(!* " Global to editor")

(FLUID
 '(F!:E!#LOOKDPTH F!:E!#TRACEFLG F!:E!#LAST!#ID F!:E!#MAXLEVEL F!:E!#UPFINDFLG
   F!:E!#MAXLOOP F!:E!#EDITCOMSL F!:E!#USERMACROS F!:E!#MACROS F!:E!#OPS
   F!:E!#MAX!#PLENGTH))

(!* " Fluid in editor, but initialized to non-NIL at top level")

(FLUID '(F!:E!#DEPTH))

(!* " Fluid in editor ")

(FLUID
 '(F!:E!#LOCLST F!:E!#LOCLST!#0 F!:E!#MARKLST F!:E!#UNDOLST F!:E!#UNDOLST!#1
   F!:E!#OLDPROMPT F!:E!#ID F!:E!#INBUF F!:E!#CMD F!:E!#UNFIND F!:E!#FINDFLAG
   F!:E!#COM0 F!:E!#TOPFLG F!:E!#COPYFLG F!:E!#LASTP1 F!:E!#LASTP2 F!:E!#LCFLG
   F!:E!#LASTAIL F!:E!#SN F!:E!#TOFLG F!:E!#1 F!:E!#2 F!:E!#3))

(!* 
"EDITLINEREAD():list            EXPR
    ------------
    Prints a supplementary prompt before the READ generated prompt.
    Reads a line of input containing a series of LISP expressions.
    But the several expressions on the line must be separated by
    spaces or commas and terminated with a bare CR.  ")

(FLUID '(PROMPTSTRING!*))

(DE EDITLINEREAD NIL
 (PROG (!#NEXT !#RES PROMPTSTRING!*)
       (!* "PromptString!* for PSL (EAB 2:08am  Friday, 6 November 1981)")
       (SETQ PROMPTSTRING!* "-E- ")
       (!* (PRIN2 "-E-"))
       (TERPRI)
  LOOP (SETQ !#RES (NCONC !#RES (LIST (READ))))
       (COND ((NOT (MEMQ (SETQ !#NEXT (READCH)) '(!, ! ))) (RETURN !#RES))
             (T (GO LOOP)))))

(DM EDIT!#!# (!#X) (LIST 'EDIT!#!#DE (MKQUOTE (CDR !#X))))

(DE EDIT!#!#DE (!#COMS)
 ((LAMBDA (F!:E!#LOCLST F!:E!#UNDOLST!#1) (EDITCOMS !#COMS)) F!:E!#LOCLST 
NIL))

(DF EDITFNS (!#X)
 (PROG (!#Y)
       (SETQ !#Y (EVAL (CAR !#X)))
  LP   (COND ((NULL !#Y) (RETURN NIL)))
       (ERRORSET (CONS 'EDITF (CONS (PRIN1 (CAR !#Y)) (CDR !#X)))
                 G!:EDIT!:ERRORS
                 G!:EDIT!:TRACE)
       (SETQ !#Y (CDR !#Y))
       (GO LP)))

(DF EDITF (!#X)
 (PROG (!#Y !#FN)
       (COND ((NULL !#X)
              (PROGN (PRIN2 " = ") (SETQ !#X (LIST (PRIN1 F!:E!#LAST!#ID))))))
       (COND ((IDP (CAR !#X))
              (PROGN
               (COND ((SETQ !#Y (GET (SETQ !#FN (CAR !#X)) 'TRACE))
                      (SETQ !#FN (CDR !#Y))))
               (COND ((SETQ !#Y (GETD !#FN))
                      (PROGN (RPLACD !#Y
                                     (EDITE (CDR !#Y) (CDR !#X) (CAR !#X)))
                             (RETURN (SETQ F!:E!#LAST!#ID (CAR !#X)))))
                     ((AND (SETQ !#Y (GET !#FN 'VALUE)) (PAIRP (CDR !#Y)))
                      (GO L1)))))
             ((PAIRP (CAR !#X)) (GO L1)))
       (PRIN1 (CAR !#X))
       (PRIN2 " not editable.")
       (ERROR NIL NIL)
  L1   (PRINT2 "=EDITV")
       (RETURN (EVAL (CONS 'EDITV !#X)))))

(DF EDITV (!#X)
 (PROG (!#Y)
       (COND ((NULL !#X)
              (PROGN (PRIN2 " = ") (SETQ !#X (LIST (PRIN1 F!:E!#LAST!#ID))))))
       (COND ((PAIRP (CAR !#X))
              (PROGN (EDITE (EVAL (CAR !#X)) (CDR !#X) NIL) (RETURN T)))
             ((AND (IDP (CAR !#X))
                   (PAIRP (ERRORSET (CAR !#X) G!:EDIT!:ERRORS G!:EDIT!:TRACE)))
              (PROGN
               (SET (CAR !#X) (EDITE (EVAL (CAR !#X)) (CDR !#X) (CAR !#X)))
               (RETURN (SETQ F!:E!#LAST!#ID (CAR !#X)))))
             (T (PROGN (TERPRI)
                       (PRIN1 (CAR !#X))
                       (PRIN2 " not editable")
                       (ERROR NIL NIL))))))

(!* "For PSL, the BREAK function uses an EXPR, EDIT.  I don't know how else
to edit a form but to call the FEXPR EDITV.")

(FLUID '(EDIT!:FORM))

(DE EDIT (EDIT!:FORM)
  (PROGN (EDITV EDIT!:FORM)
         EDIT!:FORM))

(DF EDITP (!#X)
 (PROGN
  (COND ((NULL !#X)
         (PROGN (PRIN2 " = ") (SETQ !#X (LIST (PRIN1 F!:E!#LAST!#ID))))))
  (COND ((PAIRP (CAR !#X)) (PROGN (PRIN2 "=EDITV") (EVAL (CONS 'EDITV !#X))))
        ((IDP (CAR !#X))
         (PROGN (!* "For PSL, changed (CDAR !#X) to (PROP (CAR !#X))")
		(EDITE (PROP (CAR !#X)) (CDR !#X) (CAR !#X))
		(SETQ F!:E!#LAST!#ID (CAR !#X))))
        (T (PROGN (TERPRI)
                  (PRIN1 (CAR !#X))
                  (PRIN2 " not editable.")
                  (ERROR NIL NIL))))))

(DE EDITE (!#EXPR !#COMS !#ATM)
 (COND ((NULL (PAIRP !#EXPR))
        (PROGN (PRINT !#EXPR) (PRIN2 " not editable.") (ERROR NIL NIL)))
       (T (CAR (LAST (EDITL (LIST !#EXPR) !#COMS !#ATM NIL NIL))))))

(DE EDITL (F!:E!#LOCLST !#COMS !#ATM F!:E!#MARKLST !#MESS)
 (PROG (F!:E!#CMD F!:E!#LASTAIL F!:E!#UNDOLST F!:E!#UNDOLST!#1 F!:E!#FINDFLAG
        F!:E!#LCFLG F!:E!#UNFIND F!:E!#LASTP1 F!:E!#LASTP2 F!:E!#INBUF
        F!:E!#LOCLST!#0 F!:E!#COM0 F!:E!#OLDPROMPT)
       (SETQ F!:E!#LOCLST
             (ERRORSET
              (LIST 'EDITL0
                    (ADD1 F!:E!#DEPTH)
                    (MKQUOTE !#COMS)
                    (MKQUOTE !#MESS)
                    (MKQUOTE !#ATM))
              G!:EDIT!:ERRORS
              G!:EDIT!:TRACE))
       (COND ((PAIRP F!:E!#LOCLST) (RETURN (CAR F!:E!#LOCLST)))
             (T (ERROR NIL NIL)))))

(DE EDITL0 (F!:E!#DEPTH !#COMS !#MESS F!:E!#ID)
 (PROG (!#RES)
       (COND ((NULL !#COMS) NIL)
             ((EQ (CAR !#COMS) 'START) (SETQ F!:E!#INBUF (CDR !#COMS)))
             ((PAIRP
               (ERRORSET (LIST 'EDIT1 (MKQUOTE !#COMS))
                         G!:EDIT!:ERRORS
                         G!:EDIT!:TRACE))
              (RETURN F!:E!#LOCLST))
             (T (ERROR NIL NIL)))
       (TERPRI)
       (PRINT2 (OR !#MESS "EDIT"))
       (COND ((OR (EQ (CAR F!:E!#LOCLST)
                      (CAR (LAST (CAR (COND ((SETQ F!:E!#CMD
                                                   (GET 'EDIT 'LASTVALUE))
                                             F!:E!#CMD)
                                            (T '((NIL))))))))
                  (AND F!:E!#ID
                       (EQ (CAR F!:E!#LOCLST)
                           (CAR (LAST (CAR (COND ((SETQ F!:E!#CMD
                                                        (GET
                                                         F!:E!#ID
                                                         'EDIT!-SAVE))
                                                  F!:E!#CMD)
                                                 (T '((NIL))))))))))
              (PROGN (SETQ F!:E!#LOCLST (CAR F!:E!#CMD))
                     (SETQ F!:E!#MARKLST (CADR F!:E!#CMD))
                     (SETQ F!:E!#UNDOLST (CADDR F!:E!#CMD))
                     (COND ((CAR F!:E!#UNDOLST)
                            (SETQ F!:E!#UNDOLST (CONS NIL F!:E!#UNDOLST))))
                     (SETQ F!:E!#UNFIND (CDDDR F!:E!#CMD)))))
  LP   (SETQ !#RES (ERRORSET '(EDITL1) G!:EDIT!:ERRORS G!:EDIT!:TRACE))
       (COND ((EQ !#RES 'OK) (RETURN F!:E!#LOCLST))
             ((EQ !#RES 'STOP) (ERROR 'STOP NIL))
             (T (GO LP)))))

(DE EDIT1 (!#COMS)
 (PROG (!#X)
       (SETQ !#X !#COMS)
  L1   (COND ((NULL !#X) (RETURN NIL)))
       (EDITCOM (SETQ F!:E!#CMD (CAR !#X)) NIL)
       (SETQ !#X (CDR !#X))
       (GO L1)))

(DE EDITVAL (!#X)
 (PROG (!#RES)
       (SETQ !#RES (ERRORSET !#X G!:EDIT!:ERRORS G!:EDIT!:TRACE))
       (AND !#RES (ATOM !#RES) (ERROR !#RES NIL))
       (RETURN !#RES)))

(DE EDITL1 NIL
 (PROG (!#RES)
  CT   (SETQ F!:E!#FINDFLAG NIL)
       (COND ((NULL F!:E!#OLDPROMPT)
              (SETQ F!:E!#OLDPROMPT (CONS F!:E!#DEPTH '!#))))
  A    (SETQ F!:E!#UNDOLST!#1 NIL)
       (SETQ F!:E!#CMD (EDITREAD))
       (SETQ F!:E!#LOCLST!#0 F!:E!#LOCLST)
       (SETQ F!:E!#COM0
             (COND ((ATOM F!:E!#CMD) F!:E!#CMD) (T (CAR F!:E!#CMD))))
       (SETQ !#RES
             (ERRORSET (LIST 'EDITCOM (MKQUOTE F!:E!#CMD) T)
                       G!:EDIT!:ERRORS
                       G!:EDIT!:TRACE))
       (COND ((EQ !#RES 'OK) (ERROR 'OK NIL))
             ((EQ !#RES 'STOP) (ERROR 'STOP NIL))
             (F!:E!#UNDOLST!#1
              (PROGN
               (SETQ F!:E!#UNDOLST!#1
                     (CONS F!:E!#COM0 (CONS F!:E!#LOCLST!#0 F!:E!#UNDOLST!#1)))
               (SETQ F!:E!#UNDOLST (CONS F!:E!#UNDOLST!#1 F!:E!#UNDOLST)))))
       (COND ((PAIRP !#RES) (GO A)))
       (SETQ F!:E!#INBUF NIL)
       (TERPRI)
       (COND (F!:E!#CMD (PROGN (PRIN1 F!:E!#CMD) (PRIN2 "  ?"))))
       (GO CT)))

(DE EDITREAD NIL
 (PROG (!#X)
       (COND ((NULL F!:E!#INBUF)
              (PROG NIL
               LP   (TERPRI)
                    (COND ((NOT (EQUAL (CAR F!:E!#OLDPROMPT) 0))
                           (PRIN2 (CAR F!:E!#OLDPROMPT))))
                    (SETQ F!:E!#INBUF
                          (ERRORSET '(EDITLINEREAD)
                                    G!:EDIT!:ERRORS
                                    G!:EDIT!:TRACE))
                    (COND ((ATOM F!:E!#INBUF) (PROGN (TERPRI) (GO LP))))
                    (SETQ F!:E!#INBUF (CAR F!:E!#INBUF)))))
       (SETQ !#X (CAR F!:E!#INBUF))
       (SETQ F!:E!#INBUF (CDR F!:E!#INBUF))
       (RETURN !#X)))

(DE EDITCOM (!#CMD F!:E!#TOPFLG)
 (PROGN (SETQ F!:E!#CMD !#CMD)
        (COND (F!:E!#TRACEFLG (EDITRACEFN !#CMD)))
        (COND (F!:E!#FINDFLAG
               (COND ((EQ F!:E!#FINDFLAG 'BF)
                      (PROGN (SETQ F!:E!#FINDFLAG NIL) (EDITBF !#CMD NIL)))
                     (T (PROGN (SETQ F!:E!#FINDFLAG NIL) (EDITQF !#CMD)))))
              ((NUMBERP !#CMD)
               (SETQ F!:E!#LOCLST (EDIT1F !#CMD F!:E!#LOCLST)))
              ((ATOM !#CMD) (EDITCOMA !#CMD (NULL F!:E!#TOPFLG)))
              (T (EDITCOML !#CMD (NULL F!:E!#TOPFLG))))
        (CAR F!:E!#LOCLST)))

(DE EDITCOMA (!#CMD F!:E!#COPYFLG)
 (PROG (!#TEM)
       (SELECTQ !#CMD
                (NIL NIL)
                (OK (COND (F!:E!#ID (REMPROP F!:E!#ID 'EDIT!-SAVE)))
                    (PUT 'EDIT
                         'LASTVALUE
                         (CONS (LAST F!:E!#LOCLST)
                               (CONS F!:E!#MARKLST
                                     (CONS F!:E!#UNDOLST F!:E!#LOCLST))))
                    (ERROR 'OK NIL))
                (STOP (ERROR 'STOP NIL))
                (SAVE (COND (F!:E!#ID
                             (PUT 'EDIT
                                  'LASTVALUE
                                  (PUT F!:E!#ID
                                       'EDIT!-SAVE
                                       (CONS F!:E!#LOCLST
                                             (CONS F!:E!#MARKLST
                                                   (CONS F!:E!#UNDOLST
                                                    F!:E!#UNFIND)))))))
                      (ERROR 'OK NIL))
                (TTY!: (SETQ F!:E!#CMD F!:E!#COM0)
                       (SETQ F!:E!#LOCLST
                             (EDITL F!:E!#LOCLST NIL NIL NIL 'TTY!:)))
                (E (COND (F!:E!#TOPFLG
                          (COND ((PAIRP (SETQ !#TEM (EDITVAL (EDITREAD))))
                                 (EDIT!#PRINT (CAR !#TEM) F!:E!#LOOKDPTH NIL)))
                          )
                         (T (PROGN (EDITQF !#CMD) T))))
                (P (EDITBPNT0 (CAR F!:E!#LOCLST) 2))
                (!? (EDITBPNT0 (CAR F!:E!#LOCLST) 100))
                (PP (EDITBPNT0 (CAR F!:E!#LOCLST) NIL))
                (!^ (AND (CDR F!:E!#LOCLST) (SETQ F!:E!#UNFIND F!:E!#LOCLST))
                    (SETQ F!:E!#LOCLST (LAST F!:E!#LOCLST)))
                (!@0 (COND ((NULL (CDR F!:E!#LOCLST)) (ERROR NIL NIL)))
                     (PROG NIL
                      LP   (SETQ F!:E!#LOCLST (CDR F!:E!#LOCLST))
                           (COND ((TAIL!-P (CAR F!:E!#LOCLST)
                                           (CADR F!:E!#LOCLST))
                                  (GO LP)))))
                (MARK (SETQ F!:E!#MARKLST (CONS F!:E!#LOCLST F!:E!#MARKLST)))
                (UNDO (EDITUNDO F!:E!#TOPFLG
                                NIL
                                (COND (F!:E!#INBUF (EDITREAD)))))
                (TEST (SETQ F!:E!#UNDOLST (CONS NIL F!:E!#UNDOLST)))
                (!@UNDO (EDITUNDO T T NIL))
                (UNBLOCK
                 (COND ((SETQ !#TEM (MEMQ NIL F!:E!#UNDOLST))
                        (EDITSMASH !#TEM (LIST NIL) (CDR !#TEM)))
                       (T (PRINT2 " not blocked"))))
                (!_ (COND (F!:E!#MARKLST
                           (PROGN
                            (AND (CDR F!:E!#LOCLST)
                                 (SETQ F!:E!#UNFIND F!:E!#LOCLST))
                            (SETQ F!:E!#LOCLST (CAR F!:E!#MARKLST))))
                          (T (ERROR NIL NIL))))
                (!\ (COND (F!:E!#UNFIND
                           (PROGN (SETQ !#CMD F!:E!#LOCLST)
                                  (SETQ F!:E!#LOCLST F!:E!#UNFIND)
                                  (AND (CDR !#CMD) (SETQ F!:E!#UNFIND !#CMD))))
                          (T (ERROR NIL NIL))))
                (!\P (COND ((AND F!:E!#LASTP1
                                 (NOT (EQ F!:E!#LASTP1 F!:E!#LOCLST)))
                            (SETQ F!:E!#LOCLST F!:E!#LASTP1))
                           ((AND F!:E!#LASTP2
                                 (NOT (EQ F!:E!#LASTP2 F!:E!#LOCLST)))
                            (SETQ F!:E!#LOCLST F!:E!#LASTP2))
                           (T (ERROR NIL NIL))))
                (!_!_ (COND (F!:E!#MARKLST
                             (AND (CDR F!:E!#LOCLST)
                                  (SETQ F!:E!#UNFIND F!:E!#LOCLST)
                                  (SETQ F!:E!#LOCLST (CAR F!:E!#MARKLST))
                                  (SETQ F!:E!#MARKLST (CDR F!:E!#MARKLST))))
                            (T (ERROR NIL NIL))))
                ((F BF)
                 (COND ((NULL F!:E!#TOPFLG)
                        (PROGN (SETQ F!:E!#FINDFLAG !#CMD) (RETURN NIL)))
                       (T (PROGN (SETQ !#TEM (EDITREAD))
                                 (SELECTQ !#CMD
                                          (F (EDITQF !#TEM))
                                          (BF (EDITBF !#TEM NIL))
                                          (ERROR NIL NIL))))))
                (UP (EDITUP))
                (DELETE (SETQ !#CMD '(DELETE)) (EDIT!: '!: NIL NIL))
                (NX (EDIT!* 1))
                (BK (EDIT!* -1))
                (!@NX (SETQ F!:E!#LOCLST
                            ((LAMBDA (F!:E!#LOCLST)
                              (PROG (!#UF)
                                    (SETQ !#UF F!:E!#LOCLST)
                               LP   (COND ((OR (NULL (SETQ F!:E!#LOCLST
                                                      (CDR F!:E!#LOCLST)))
                                               (NULL (CDR F!:E!#LOCLST)))
                                           (ERROR NIL NIL))
                                          ((OR (NULL (SETQ !#TEM
                                                      (MEMQ
                                                       (CAR F!:E!#LOCLST)
                                                       (CADR F!:E!#LOCLST))))
                                               (NULL (CDR !#TEM)))
                                           (GO LP)))
                                    (EDITCOM 'NX NIL)
                                    (SETQ F!:E!#UNFIND !#UF)
                                    (RETURN F!:E!#LOCLST)))
                             F!:E!#LOCLST)))
                (!?!? (EDITH F!:E!#UNDOLST))
                (COND ((AND (NULL (SETQ !#TEM
                                        (EDITMAC !#CMD F!:E!#MACROS NIL)))
                            (NULL (SETQ !#TEM
                                        (EDITMAC !#CMD F!:E!#USERMACROS NIL))))
                       (RETURN (EDITDEFAULT !#CMD)))
                      (T (EDITCOMS (COPY (CDR !#TEM))))))))

(DE EDITCOML (!#CMD F!:E!#COPYFLG)
 (PROG (!#C2 !#C3 !#TEM)
  LP   (COND ((PAIRP (CDR !#CMD))
              (PROGN (SETQ !#C2 (CADR !#CMD))
                     (COND ((PAIRP (CDDR !#CMD)) (SETQ !#C3 (CADDR !#CMD)))))))
       (COND ((AND F!:E!#LCFLG
                   (SELECTQ !#C2
                            ((TO THRU THROUGH)
                             (COND ((NULL (CDDR !#CMD))
                                    (PROGN (SETQ !#C3 -1) (SETQ !#C2 'THRU))))
                             T)
                            NIL))
              (PROGN (EDITTO (CAR !#CMD) !#C3 !#C2) (RETURN NIL)))
             ((NUMBERP (CAR !#CMD))
              (PROGN (EDIT2F (CAR !#CMD) (CDR !#CMD)) (RETURN NIL)))
             ((EQ !#C2 '!:!:)
              (PROGN (EDITCONT (CAR !#CMD) (CDDR !#CMD)) (RETURN NIL))))
       (SELECTQ (CAR !#CMD)
                (S (SET !#C2
                        (COND ((NULL !#C2) (ERROR NIL NIL))
                              (T ((LAMBDA (F!:E!#LOCLST)
                                   (EDITLOC (CDDR !#CMD)))
                                  F!:E!#LOCLST)))))
                (R (SETQ !#C2 (EDITNEWC2 (LIST (CAR F!:E!#LOCLST)) !#C2))
                   (EDITDSUBST !#C3 !#C2 (CAR F!:E!#LOCLST)))
                (E (SETQ !#TEM (EVAL !#C2))
                   (COND ((NULL (CDDR !#CMD)) (PRINT !#TEM)))
                   (RETURN !#TEM))
                (I (SETQ !#CMD
                         (CONS (COND ((ATOM !#C2) !#C2) (T (EVAL !#C2)))
                               (MAPCAR (CDDR !#CMD)
                                       (FUNCTION
                                        (LAMBDA (X)
                                         (COND (F!:E!#TOPFLG (PRINT (EVAL X)))
                                               (T (EVAL X))))))))
                   (SETQ F!:E!#COPYFLG NIL)
                   (GO LP))
                (N (COND ((ATOM (CAR F!:E!#LOCLST)) (ERROR NIL NIL)))
                   (EDITNCONC (CAR F!:E!#LOCLST)
                              (COND (F!:E!#COPYFLG (COPY (CDR !#CMD)))
                                    (T (APPEND (CDR !#CMD) NIL)))))
                (P (COND ((NOT (EQ F!:E!#LASTP1 F!:E!#LOCLST))
                          (PROGN (SETQ F!:E!#LASTP2 F!:E!#LASTP1)
                                 (SETQ F!:E!#LASTP1 F!:E!#LOCLST))))
                   (EDITBPNT (CDR !#CMD)))
                (F (EDIT4F !#C2 !#C3))
                (FS (PROG NIL
                     L1   (COND ((SETQ !#CMD (CDR !#CMD))
                                 (PROGN (EDITQF (SETQ F!:E!#CMD (CAR !#CMD)))
                                        (GO L1))))))
                (F!= (EDIT4F (CONS '!=!= !#C2) !#C3))
                (ORF (EDIT4F (CONS '!*ANY!* (CDR !#CMD)) 'N))
                (BF (EDITBF !#C2 !#C3))
                (NTH (COND ((NOT (EQ (SETQ !#TEM
                                           (EDITNTH (CAR F!:E!#LOCLST) !#C2))
                                     (CAR F!:E!#LOCLST)))
                            (SETQ F!:E!#LOCLST (CONS !#TEM F!:E!#LOCLST)))))
                (IF (COND ((AND (PAIRP (SETQ !#TEM (EDITVAL !#C2)))
                                (CAR !#TEM))
                           (COND ((CDR !#CMD) (EDITCOMS !#C3))))
                          ((AND (CDDR !#CMD) (CDDDR !#CMD))
                           (EDITCOMS (CADDDR !#CMD)))
                          (T (ERROR NIL NIL))))
                (BI (EDITBI !#C2
                            (COND ((CDDR !#CMD) !#C3) (T !#C2))
                            (AND (CDR !#CMD) (CAR F!:E!#LOCLST))))
                (RI (EDITRI !#C2
                            !#C3
                            (AND (CDR !#CMD) (CDDR !#CMD) (CAR F!:E!#LOCLST))))
                (RO (EDITRO !#C2 (AND (CDR !#CMD) (CAR F!:E!#LOCLST))))
                (LI (EDITLI !#C2 (AND (CDR !#CMD) (CAR F!:E!#LOCLST))))
                (LO (EDITLO !#C2 (AND (CDR !#CMD) (CAR F!:E!#LOCLST))))
                (BO (EDITBO !#C2 (AND (CDR !#CMD) (CAR F!:E!#LOCLST))))
                (M (EDITM !#CMD !#C2))
                (NX (EDIT!* !#C2))
                (BK (EDIT!* (MINUS !#C2)))
                (ORR (EDITOR (CDR !#CMD)))
                (MBD (EDITMBD NIL (CDR !#CMD)))
                (XTR (EDITXTR NIL (CDR !#CMD)))
                ((THRU TO) (EDITTO NIL !#C2 (CAR !#CMD)))
                ((A B !: AFTER BEFORE) (EDIT!: (CAR !#CMD) NIL (CDR !#CMD)))
                (MV (EDITMV NIL (CADR !#CMD) (CDDR !#CMD)))
                ((LP LPQ) (EDITRPT (CDR !#CMD) (EQ (CAR !#CMD) 'LPQ)))
                (LC (EDITLOC (CDR !#CMD)))
                (LCL (EDITLOCL (CDR !#CMD)))
                (!_ (SETQ F!:E!#LOCLST (EDITNEWLOCLST F!:E!#LOCLST !#C2)))
                (BELOW (EDITBELOW !#C2 (COND ((CDDR !#CMD) !#C3) (T 1))))
                (SW (EDITSW (CADR !#CMD) (CADDR !#CMD)))
                (BIND (PROG (F!:E!#1 F!:E!#2 F!:E!#3) (EDITCOMS (CDR !#CMD))))
                (COMS (PROG NIL
                       L1   (COND ((SETQ !#CMD (CDR !#CMD))
                                   (PROGN
                                    (EDITCOM
                                     (SETQ F!:E!#CMD (EVAL (CAR !#CMD)))
                                     NIL)
                                    (GO L1))))))
                (COMSQ (EDITCOMS (CDR !#CMD)))
                (COND ((AND (NULL (SETQ !#TEM
                                        (EDITMAC (CAR !#CMD) F!:E!#MACROS T)))
                            (NULL (SETQ !#TEM
                                        (EDITMAC (CAR !#CMD)
                                                 F!:E!#USERMACROS
                                                 T))))
                       (RETURN (EDITDEFAULT !#CMD)))
                      ((NOT (ATOM (SETQ !#C3 (CAR !#TEM))))
                       (EDITCOMS (SUBLIS (PAIR !#C3 (CDR !#CMD)) (CDR !#TEM))))
                      (T (EDITCOMS (SUBST (CDR !#CMD) !#C3 (CDR !#TEM))))))))

(DE EDITNEWC2 (F!:E!#LOCLST !#C2)
 (PROGN (EDIT4F !#C2 T)
        (SETQ F!:E!#UNFIND F!:E!#LOCLST)
        (COND ((AND (ATOM !#C2) F!:E!#UPFINDFLG (PAIRP (CAR F!:E!#LOCLST)))
               (CAAR F!:E!#LOCLST))
              (T (CAR F!:E!#LOCLST)))))

(DE EDITM (!#CMD !#C2)
 (PROG (!#NEWMACRO !#TEM)
       (COND ((ATOM !#C2)
              (COND ((SETQ !#TEM (EDITMAC !#C2 F!:E!#USERMACROS NIL))
                     (PROGN (RPLACD !#TEM (CDDR !#CMD)) (RETURN NIL)))
                    (T (SETQ !#NEWMACRO (CONS !#C2 (CONS NIL (CDDR !#CMD)))))))
             ((SETQ !#TEM (EDITMAC (CAR !#C2) F!:E!#USERMACROS T))
              (PROGN (RPLACA !#TEM (CADDR !#CMD))
                     (RPLACD !#TEM (CDDDR !#CMD))
                     (RETURN NIL)))
             (T (PROGN (NCONC F!:E!#EDITCOMSL (LIST (CAR !#C2)))
                       (SETQ !#NEWMACRO (CONS (CAR !#C2) (CDDR !#CMD))))))
       (SETQ F!:E!#USERMACROS (CONS !#NEWMACRO F!:E!#USERMACROS))))

(DE EDITNEWLOCLST (F!:E!#LOCLST !#C2)
 (PROG (!#UF !#TEM)
       (SETQ !#UF F!:E!#LOCLST)
       (SETQ !#C2 (EDITFPAT !#C2))
  LP   (COND ((COND ((AND (ATOM !#C2) (PAIRP (CAR F!:E!#LOCLST)))
                     (EQ !#C2 (CAAR F!:E!#LOCLST)))
                    ((EQ (CAR !#C2) 'IF)
                     (COND ((ATOM (SETQ !#TEM (EDITVAL (CADR !#C2)))) NIL)
                           (T !#TEM)))
                    (T (EDIT4E !#C2
                               (COND ((EQ (CAR !#C2) '!') (CAAR F!:E!#LOCLST))
                                     (T (CAR F!:E!#LOCLST))))))
              (PROGN (SETQ F!:E!#UNFIND !#UF) (RETURN F!:E!#LOCLST)))
             ((SETQ F!:E!#LOCLST (CDR F!:E!#LOCLST)) (GO LP)))
       (ERROR NIL NIL)))

(DE EDITMAC (!#C !#LST !#FLG)
 (PROG (!#X !#Y)
  LP   (COND ((NULL !#LST) (RETURN NIL))
             ((EQ !#C (CAR (SETQ !#X (CAR !#LST))))
              (PROGN (SETQ !#Y (CDR !#X))
                     (COND ((COND (!#FLG (CAR !#Y)) (T (NULL (CAR !#Y))))
                            (RETURN !#Y))))))
       (SETQ !#LST (CDR !#LST))
       (GO LP)))

(DE EDITCOMS (!#COMS)
 (PROG NIL
  L1   (COND ((ATOM !#COMS) (RETURN (CAR F!:E!#LOCLST))))
       (EDITCOM (CAR !#COMS) NIL)
       (SETQ !#COMS (CDR !#COMS))
       (GO L1)))

(DE EDITH (!#LST)
 (PROG NIL
       (TERPRI)
       (MAPC !#LST
             (FUNCTION
              (LAMBDA (!#ELT)
               (PROGN
                (COND ((NULL !#ELT) (PRIN2 " block"))
                      ((NULL (CAR !#ELT)) NIL)
                      ((NUMBERP (CAR !#ELT)) (PRIN2 (LIST (CAR !#ELT) "--")))
                      (T (PRIN1 (CAR !#ELT))))
                (PRIN2 " ")))))))

(DE EDITUNDO (!#PRINTFLG !#UNDOFLG !#UNDOP)
 (PROG (!#LST !#FLG)
       (SETQ !#LST F!:E!#UNDOLST)
  LP   (COND ((OR (NULL !#LST) (NULL (CAR !#LST))) (GO OUT)))
       (COND ((NULL !#UNDOP)
              (SELECTQ (CAAR !#LST)
                       ((NIL !@UNDO UNBLOCK) (GO LP1))
                       (UNDO (COND ((NULL !#UNDOFLG) (GO LP1))))
                       NIL))
             ((NOT (EQ !#UNDOP (CAAR !#LST))) (GO LP1)))
       (EDITUNDOCOM (CAR !#LST) !#PRINTFLG)
       (COND ((NULL !#UNDOFLG) (RETURN NIL)))
       (SETQ !#FLG T)
  LP1  (SETQ !#LST (CDR !#LST))
       (GO LP)
  OUT  (COND (!#FLG NIL)
             ((AND !#LST (CDR !#LST)) (PRINT2 " blocked"))
             (T (PRINT2 " nothing saved")))))

(DE EDITUNDOCOM (!#X !#FLG)
 (PROG (!#C !#Y !#Z)
       (COND ((ATOM !#X) (ERROR NIL NIL))
             ((NOT (EQ (CAR (LAST F!:E!#LOCLST)) (CAR (LAST (CADR !#X)))))
              (PROGN (PRINT2 " different expression")
                     (SETQ F!:E!#CMD NIL)
                     (ERROR NIL NIL))))
       (SETQ !#C (CAR !#X))
       (SETQ F!:E!#LOCLST (CADR !#X))
       (SETQ !#Y (CDR !#X))
  L1   (COND ((SETQ !#Y (CDR !#Y))
              (PROGN (SETQ !#Z (CAR !#Y))
                     (COND ((EQ (CAR !#Z) 'R)
                            ((LAMBDA (F!:E!#LOCLST)
                              (EDITCOM (LIST 'R (CADR !#Z) (CADDR !#Z)) NIL))
                             (CADDDR !#Z)))
                           (T (EDITSMASH (CAR !#Z) (CADR !#Z) (CDDR !#Z))))
                     (GO L1))))
       (EDITSMASH !#X NIL (CONS (CAR !#X) (CDR !#X)))
       (COND (!#FLG
              (PROGN
               (COND ((NUMBERP !#C) (PRINT2 (LIST !#C "--"))) (T (PRIN1 !#C)))
               (PRIN2 " undone"))))
       (RETURN T)))

(DE EDITSMASH (!#OLD !#A !#D)
 (PROGN (COND ((ATOM !#OLD) (ERROR NIL NIL)))
        (SETQ F!:E!#UNDOLST!#1
              (CONS (CONS !#OLD (CONS (CAR !#OLD) (CDR !#OLD)))
                    F!:E!#UNDOLST!#1))
        (RPLACA !#OLD !#A)
        (RPLACD !#OLD !#D)))

(DE EDITNCONC (!#X !#Y)
 (PROG (!#TEM)
       (RETURN
        (COND ((NULL !#X) !#Y)
              ((ATOM !#X) (ERROR NIL NIL))
              (T (PROGN (EDITSMASH (SETQ !#TEM (LAST !#X)) (CAR !#TEM) !#Y)
                        !#X))))))

(DE EDITDSUBST (!#X !#Y !#Z)
 (PROG NIL
  LP   (COND ((NULL (PAIRP !#Z)) (RETURN NIL))
             ((EQUAL !#Y (CAR !#Z)) (EDITSMASH !#Z (COPY !#X) (CDR !#Z)))
             (T (EDITDSUBST !#X !#Y (CAR !#Z))))
       (COND ((AND !#Y (EQ !#Y (CDR !#Z)))
              (PROGN (EDITSMASH !#Z (CAR !#Z) (COPY !#X)) (RETURN NIL))))
       (SETQ !#Z (CDR !#Z))
       (GO LP)))

(DE EDIT1F (!#C F!:E!#LOCLST)
 (COND ((EQUAL !#C 0)
        (COND ((NULL (CDR F!:E!#LOCLST)) (ERROR NIL NIL))
              (T (CDR F!:E!#LOCLST))))
       ((ATOM (CAR F!:E!#LOCLST)) (ERROR NIL NIL))
       ((GREATERP !#C 0)
        (COND ((GREATERP !#C (LENGTH (CAR F!:E!#LOCLST))) (ERROR NIL NIL))
              (T (CONS (CAR (SETQ F!:E!#LASTAIL
                                  (NTH!-TAIL (CAR F!:E!#LOCLST) !#C)))
                       F!:E!#LOCLST))))
       ((GREATERP (MINUS !#C) (LENGTH (CAR F!:E!#LOCLST))) (ERROR NIL NIL))
       (T (CONS (CAR (SETQ F!:E!#LASTAIL
                           (NTH!-TAIL (CAR F!:E!#LOCLST)
                                      (PLUS (LENGTH (CAR F!:E!#LOCLST))
                                            (PLUS !#C 1)))))
                F!:E!#LOCLST))))

(DE EDIT2F (!#N !#X)
 (PROG (!#CL)
       (SETQ !#CL (CAR F!:E!#LOCLST))
       (COND ((ATOM !#CL) (ERROR NIL NIL))
             (F!:E!#COPYFLG (SETQ !#X (COPY !#X)))
             (T (SETQ !#X (APPEND !#X NIL))))
       (COND ((GREATERP !#N 0)
              (COND ((GREATERP !#N (LENGTH !#CL)) (ERROR NIL NIL))
                    ((NULL !#X) (GO DELETE))
                    (T (GO REPLACE))))
             ((OR (EQUAL !#N 0)
                  (NULL !#X)
                  (GREATERP (MINUS !#N) (LENGTH !#CL)))
              (ERROR NIL NIL))
             (T (PROGN
                 (COND ((NOT (EQUAL !#N -1))
                        (SETQ !#CL (NTH!-TAIL !#CL (MINUS !#N)))))
                 (EDITSMASH !#CL (CAR !#X) (CONS (CAR !#CL) (CDR !#CL)))
                 (COND ((CDR !#X)
                        (EDITSMASH !#CL
                                   (CAR !#CL)
                                   (NCONC (CDR !#X) (CDR !#CL)))))
                 (RETURN NIL))))
  DELETE
       (COND ((EQUAL !#N 1)
              (PROGN (OR (PAIRP (CDR !#CL)) (ERROR NIL NIL))
                     (EDITSMASH !#CL (CADR !#CL) (CDDR !#CL))))
             (T (PROGN (SETQ !#CL (NTH!-TAIL !#CL (DIFFERENCE !#N 1)))
                       (EDITSMASH !#CL (CAR !#CL) (CDDR !#CL)))))
       (RETURN NIL)
  REPLACE
       (COND ((NOT (EQUAL !#N 1)) (SETQ !#CL (NTH!-TAIL !#CL !#N))))
       (EDITSMASH !#CL (CAR !#X) (CDR !#CL))
       (COND ((CDR !#X)
              (EDITSMASH !#CL (CAR !#CL) (NCONC (CDR !#X) (CDR !#CL)))))))

(DE EDIT4E (!#PAT !#Y)
 (COND ((EQ !#PAT !#Y) T)
       ((ATOM !#PAT) (OR (EQ !#PAT '!&) (EQUAL !#PAT !#Y)))
       ((EQ (CAR !#PAT) '!*ANY!*)
        (PROG NIL
         LP   (COND ((NULL (SETQ !#PAT (CDR !#PAT))) (RETURN NIL))
                    ((EDIT4E (CAR !#PAT) !#Y) (RETURN T)))
              (GO LP)))
       ((AND (EQ (CAR !#PAT) '!') (ATOM !#Y))
        (PROG (!#Z)
              (SETQ !#PAT (CDR !#PAT))
              (SETQ !#Z (EXPLODE2 !#Y))
         LP   (COND ((EQ (CAR !#PAT) '!')
                     (PROGN (FREELIST !#Z)
                            (PRINT2 "=")
                            (PRIN1 !#Y)
                            (RETURN T)))
                    ((NULL !#Z) (RETURN NIL))
                    ((NOT (EQ (CAR !#PAT) (CAR !#Z)))
                     (PROGN (FREELIST !#Z) (RETURN NIL))))
              (SETQ !#PAT (CDR !#PAT))
              (SETQ !#Z (CDR !#Z))
              (GO LP)))
       ((EQ (CAR !#PAT) '!-!-)
        (OR (NULL (SETQ !#PAT (CDR !#PAT)))
            (PROG NIL
             LP   (COND ((EDIT4E !#PAT !#Y) (RETURN T))
                        ((ATOM !#Y) (RETURN NIL)))
                  (SETQ !#Y (CDR !#Y))
                  (GO LP))))
       ((EQ (CAR !#PAT) '!=!=) (EQ (CDR !#PAT) !#Y))
       ((ATOM !#Y) NIL)
       ((EDIT4E (CAR !#PAT) (CAR !#Y)) (EDIT4E (CDR !#PAT) (CDR !#Y)))))

(DE EDITQF (!#PAT)
 (PROG (!#Q1)
       (COND ((AND (PAIRP (CAR F!:E!#LOCLST))
                   (PAIRP (SETQ !#Q1 (CDAR F!:E!#LOCLST)))
                   (SETQ !#Q1 (MEMQ !#PAT !#Q1)))
              (SETQ F!:E!#LOCLST
                    (CONS (COND (F!:E!#UPFINDFLG !#Q1)
                                (T (PROGN (SETQ F!:E!#LASTAIL !#Q1)
                                          (CAR !#Q1))))
                          F!:E!#LOCLST)))
             (T (EDIT4F !#PAT 'N)))))

(DE EDIT4F (!#PAT F!:E!#SN)
 (PROG (!#LL !#X !#FF)
       (SETQ !#FF (LIST NIL))
       (SETQ F!:E!#CMD !#PAT)
       (SETQ !#PAT (EDITFPAT !#PAT))
       (SETQ !#LL F!:E!#LOCLST)
       (COND ((EQ F!:E!#SN 'N)
              (PROGN (SETQ F!:E!#SN 1)
                     (COND ((ATOM (CAR F!:E!#LOCLST)) (GO LP1))
                           ((AND (ATOM (CAAR F!:E!#LOCLST)) F!:E!#UPFINDFLG)
                            (PROGN
                             (SETQ !#LL
                                   (CONS (CAAR F!:E!#LOCLST) F!:E!#LOCLST))
                             (GO LP1)))
                           (T (SETQ !#LL
                                    (CONS (CAAR F!:E!#LOCLST) F!:E!#LOCLST)))))
              ))
       (COND ((AND F!:E!#SN (NOT (NUMBERP F!:E!#SN))) (SETQ F!:E!#SN 1)))
       (COND ((AND (EDIT4E
                    (COND ((AND (PAIRP !#PAT) (EQ (CAR !#PAT) '!:!:!:))
                           (CDR !#PAT))
                          (T !#PAT))
                    (CAR !#LL))
                   (OR (NULL F!:E!#SN)
                       (EQUAL (SETQ F!:E!#SN (DIFFERENCE F!:E!#SN 1)) 0)))
              (RETURN (SETQ F!:E!#LOCLST !#LL))))
       (SETQ !#X (CAR !#LL))
  LP   (COND ((EDIT4F1 !#PAT !#X F!:E!#MAXLEVEL !#FF)
              (PROGN (AND (CDR F!:E!#LOCLST) (SETQ F!:E!#UNFIND F!:E!#LOCLST))
                     (RETURN
                      (CAR (SETQ F!:E!#LOCLST
                                 (NCONC (CAR !#FF)
                                        (COND ((EQ (CADR !#FF) (CAR !#LL))
                                               (CDR !#LL))
                                              (T !#LL))))))))
             ((NULL F!:E!#SN) (ERROR NIL NIL)))
  LP1  (SETQ !#X (CAR !#LL))
       (COND ((NULL (SETQ !#LL (CDR !#LL))) (ERROR NIL NIL))
             ((AND (SETQ !#X (MEMQ !#X (CAR !#LL)))
                   (PAIRP (SETQ !#X (CDR !#X))))
              (GO LP)))
       (GO LP1)))

(DE EDITFPAT (!#PAT)
 (COND ((PAIRP !#PAT)
        (COND ((OR (EQ (CAR !#PAT) '!=!=) (EQ (CAR !#PAT) '!')) !#PAT)
              (T (MAPCAR !#PAT (FUNCTION EDITFPAT)))))
       ((EQ (NTHCHAR !#PAT -1) '!') (CONS '!' (EXPLODE2 !#PAT)))
       (T !#PAT)))

(DE EDIT4F1 (!#PAT !#X !#LVL !#FF)
 (PROG NIL
  LP   (COND ((NOT (GREATERP !#LVL 0))
              (PROGN (PRINT2 " maxlevel exceeded") (RETURN NIL)))
             ((ATOM !#X) (RETURN NIL))
             ((AND (PAIRP !#PAT)
                   (EQ (CAR !#PAT) '!:!:!:)
                   (EDIT4E (CDR !#PAT) !#X)
                   (OR (NULL F!:E!#SN)
                       (EQUAL (SETQ F!:E!#SN (DIFFERENCE F!:E!#SN 1)) 0)))
              T)
             ((AND (OR (ATOM !#PAT) (NOT (EQ (CAR !#PAT) '!:!:!:)))
                   (EDIT4E !#PAT (CAR !#X))
                   (OR (NULL F!:E!#SN)
                       (EQUAL (SETQ F!:E!#SN (DIFFERENCE F!:E!#SN 1)) 0)))
              (COND ((OR (NULL F!:E!#UPFINDFLG) (PAIRP (CAR !#X)))
                     (PROGN (SETQ F!:E!#LASTAIL !#X) (SETQ !#X (CAR !#X))))))
             ((AND !#PAT
                   (EQ !#PAT (CDR !#X))
                   (OR (NULL F!:E!#SN)
                       (EQUAL (SETQ F!:E!#SN (DIFFERENCE F!:E!#SN 1)) 0)))
              (SETQ !#X (CDR !#X)))
             ((AND F!:E!#SN
                   (PAIRP (CAR !#X))
                   (EDIT4F1 !#PAT (CAR !#X) (DIFFERENCE !#LVL 1) !#FF)
                   (EQUAL F!:E!#SN 0))
              (SETQ !#X (CAR !#X)))
             (T (PROGN (SETQ !#X (CDR !#X))
                       (SETQ !#LVL (DIFFERENCE !#LVL 1))
                       (GO LP))))
       (COND ((AND !#FF (NOT (EQ !#X (CADR !#FF)))) (TCONC !#FF !#X)))
       (RETURN (OR !#FF T))))

(DE EDITFINDP (!#X !#PAT !#FLG)
 (PROG (F!:E!#SN F!:E!#LASTAIL !#FF)
       (SETQ F!:E!#SN 1)
       (AND (NULL !#FLG) (SETQ !#PAT (EDITFPAT !#PAT)))
       (RETURN (OR (EDIT4E !#PAT !#X) (EDIT4F1 !#PAT !#X F!:E!#MAXLEVEL !#FF)))
  ))

(DE EDITBF (!#PAT !#N)
 (PROG (!#LL !#X !#Y !#FF)
       (SETQ !#LL F!:E!#LOCLST)
       (SETQ !#FF (LIST NIL))
       (SETQ F!:E!#CMD !#PAT)
       (SETQ !#PAT (EDITFPAT !#PAT))
       (COND ((AND (NULL !#N) (CDR !#LL)) (GO LP1)))
  LP   (COND ((EDITBF1 !#PAT (CAR !#LL) F!:E!#MAXLEVEL !#Y !#FF)
              (PROGN (SETQ F!:E!#UNFIND F!:E!#LOCLST)
                     (RETURN
                      (CAR (SETQ F!:E!#LOCLST
                                 (NCONC (CAR !#FF)
                                        (COND ((EQ (CAR !#LL) (CADR !#FF))
                                               (CDR !#LL))
                                              (T !#LL)))))))))
  LP1  (SETQ !#X (CAR !#LL))
       (COND ((NULL (SETQ !#LL (CDR !#LL))) (ERROR NIL NIL))
             ((OR (SETQ !#Y (MEMQ !#X (CAR !#LL)))
                  (SETQ !#Y (TAIL!-P !#X (CAR !#LL))))
              (GO LP)))
       (GO LP1)))

(DE EDITBF1 (!#PAT !#X !#LVL !#TAIL !#FF)
 (PROG (!#Y)
  LP   (COND ((NOT (GREATERP !#LVL 0))
              (PROGN (PRINT2 " maxlevel exceeded") (RETURN NIL)))
             ((EQ !#TAIL !#X)
              (RETURN
               (COND ((EDIT4E
                       (COND ((AND (PAIRP !#PAT) (EQ (CAR !#PAT) '!:!:!:))
                              (CDR !#PAT))
                             (T !#PAT))
                       !#X)
                      (TCONC !#FF !#X))))))
       (SETQ !#Y !#X)
  LP1  (COND ((NULL (OR (EQ (CDR !#Y) !#TAIL) (ATOM (CDR !#Y))))
              (PROGN (SETQ !#Y (CDR !#Y)) (GO LP1))))
       (SETQ !#TAIL !#Y)
       (COND ((AND (PAIRP (CAR !#TAIL))
                   (EDITBF1 !#PAT (CAR !#TAIL) (DIFFERENCE !#LVL 1) NIL))
              (SETQ !#TAIL (CAR !#TAIL)))
             ((AND (EQ (CAR !#PAT) '!:!:!:) (EDIT4E (CDR !#PAT) !#TAIL)) T)
             ((AND (OR (ATOM !#PAT) (NOT (EQ (CAR !#PAT) '!:!:!:)))
                   (EDIT4E !#PAT (CAR !#TAIL)))
              (COND ((OR (NULL F!:E!#UPFINDFLG) (PAIRP (CAR !#TAIL)))
                     (PROGN (SETQ F!:E!#LASTAIL !#TAIL)
                            (SETQ !#TAIL (CAR !#TAIL))))))
             ((AND !#PAT (EQ !#PAT (CDR !#TAIL))) (SETQ !#X (CDR !#X)))
             (T (PROGN (SETQ !#LVL (DIFFERENCE !#LVL 1)) (GO LP))))
       (COND ((NOT (EQ !#TAIL (CADR !#FF))) (TCONC !#FF !#TAIL)))
       (RETURN !#FF)))

(DE EDITNTH (!#X !#N)
 (COND ((ATOM !#X) (ERROR NIL NIL))
       ((NOT (NUMBERP !#N))
        (OR (MEMQ !#N !#X)
            (MEMQ (SETQ !#N (EDITELT !#N (LIST !#X))) !#X)
            (TAIL!-P !#N !#X)))
       ((EQUAL !#N 0) (ERROR NIL NIL))
       ((NULL (SETQ !#N
                    (COND ((OR (NOT (LESSP !#N 0))
                               (GREATERP (SETQ !#N (PLUS (LENGTH !#X) !#N 
1))                                      0))
                           (NTH!-TAIL !#X !#N)))))
        (ERROR NIL NIL))
       (T !#N)))

(DE EDITBPNT0 (!#EXP !#DEPTH)
 (PROGN
  (COND ((NOT (EQUAL F!:E!#LASTP1 F!:E!#LOCLST))
         (PROGN (SETQ F!:E!#LASTP2 F!:E!#LASTP1)
                (SETQ F!:E!#LASTP1 F!:E!#LOCLST))))
  (TERPRI)
  (!* " 3nd arg to edit#print indicates whether print should start with ... ")
  (!* " 2nd arg to sprint is left margin")
  (COND (!#DEPTH
         (EDIT!#PRINT !#EXP
                      !#DEPTH
                      (TAIL!-P (CAR F!:E!#LOCLST) (CADR F!:E!#LOCLST))))
        (T (SPRINT !#EXP 1)))))

(DE EDITBPNT (!#X)
 (PROG (!#Y !#N)
       (COND ((EQUAL (CAR !#X) 0) (SETQ !#Y (CAR F!:E!#LOCLST)))
             (T (SETQ !#Y (CAR (EDITNTH (CAR F!:E!#LOCLST) (CAR !#X))))))
       (COND ((NULL (CDR !#X)) (SETQ !#N 2))
             ((NOT (NUMBERP (SETQ !#N (CADR !#X)))) (ERROR NIL NIL))
             ((LESSP !#N 0) (ERROR NIL NIL)))
       (TERPRI)
       (!* " 3nd arg indicates whether print should start with ... ")
       (EDIT!#PRINT !#Y !#N (TAIL!-P (CAR F!:E!#LOCLST) (CADR F!:E!#LOCLST)))
       (RETURN !#Y)))

(DE EDITRI (!#M !#N !#X)
 (PROG (!#A !#B)
       (SETQ !#A (EDITNTH !#X !#M))
       (SETQ !#B (EDITNTH (CAR !#A) !#N))
       (COND ((OR (NULL !#A) (NULL !#B)) (ERROR NIL NIL)))
       (EDITSMASH !#A (CAR !#A) (EDITNCONC (CDR !#B) (CDR !#A)))
       (EDITSMASH !#B (CAR !#B) NIL)))

(DE EDITRO (!#N !#X)
 (PROGN (SETQ !#X (EDITNTH !#X !#N))
        (COND ((OR (NULL !#X) (ATOM (CAR !#X))) (ERROR NIL NIL)))
        (EDITSMASH (SETQ !#N (LAST (CAR !#X))) (CAR !#N) (CDR !#X))
        (EDITSMASH !#X (CAR !#X) NIL)))

(DE EDITLI (!#N !#X)
 (PROGN (SETQ !#X (EDITNTH !#X !#N))
        (COND ((NULL !#X) (ERROR NIL NIL)))
        (EDITSMASH !#X (CONS (CAR !#X) (CDR !#X)) NIL)))

(DE EDITLO (!#N !#X)
 (PROGN (SETQ !#X (EDITNTH !#X !#N))
        (COND ((OR (NULL !#X) (ATOM (CAR !#X))) (ERROR NIL NIL)))
        (EDITSMASH !#X (CAAR !#X) (CDAR !#X))))

(DE EDITBI (!#M !#N !#X)
 (PROG (!#A !#B)
       (SETQ !#B (CDR (SETQ !#A (EDITNTH !#X !#N))))
       (SETQ !#X (EDITNTH !#X !#M))
       (COND ((AND !#A (NOT (GREATERP (LENGTH !#A) (LENGTH !#X))))
              (PROGN (EDITSMASH !#A (CAR !#A) NIL)
                     (EDITSMASH !#X (CONS (CAR !#X) (CDR !#X)) !#B)))
             (T (ERROR NIL NIL)))))

(DE EDITBO (!#N !#X)
 (PROGN (SETQ !#X (EDITNTH !#X !#N))
        (COND ((ATOM (CAR !#X)) (ERROR NIL NIL)))
        (EDITSMASH !#X (CAAR !#X) (EDITNCONC (CDAR !#X) (CDR !#X)))))

(DE EDITDEFAULT (!#X)
 (PROG (!#Y)
       (COND (F!:E!#LCFLG
              (RETURN
               (COND ((EQ F!:E!#LCFLG T) (EDITQF !#X))
                     (T (EDITCOM (LIST F!:E!#LCFLG !#X) F!:E!#TOPFLG)))))
             ((PAIRP !#X)
              (RETURN
               (COND ((SETQ !#Y (ATSOC (CAR !#X) F!:E!#OPS))
                      (EDITRAN !#X (CDR !#Y)))
                     (T (ERROR NIL NIL)))))
             ((NULL F!:E!#TOPFLG) (ERROR NIL NIL))
             ((MEMQ !#X F!:E!#EDITCOMSL)
              (COND (F!:E!#INBUF
                     (PROGN (SETQ !#X (CONS !#X F!:E!#INBUF))
                            (SETQ F!:E!#INBUF NIL)))
                    (T (ERROR NIL NIL))))
             ((AND (EQ (NTHCHAR !#X -1) 'P)
                   (MEMQ (SETQ !#X
                               (ICOMPRESS
                                (REVERSIP (CDR (REVERSIP (EXPLODE !#X))))))
                         '(!^ !_ UP NX BK !@NX UNDO)))
              (SETQ F!:E!#INBUF (CONS 'P F!:E!#INBUF)))
             (T (ERROR NIL NIL)))
       (RETURN
        (COND ((SETQ !#Y (ATSOC (CAR !#X) F!:E!#OPS)) (EDITRAN !#X (CDR !#Y)))
              (T (EDITCOM (SETQ F!:E!#CMD !#X) F!:E!#TOPFLG))))))

(DE EDITUP NIL
 (PROG (!#CL F!:E!#LOCLST!#1 !#X !#Y)
       (SETQ !#CL (CAR F!:E!#LOCLST))
       (!* "unused LP was here")
       (COND ((NULL (SETQ F!:E!#LOCLST!#1 (CDR F!:E!#LOCLST)))
              (ERROR NIL NIL))
             ((TAIL!-P !#CL (CAR F!:E!#LOCLST!#1)) (RETURN NIL))
             ((NOT (SETQ !#X (MEMQ !#CL (CAR F!:E!#LOCLST!#1))))
              (ERROR NIL NIL))
             ((OR (EQ !#X F!:E!#LASTAIL)
                  (NOT (SETQ !#Y (MEMQ !#CL (CDR !#X)))))
              NIL)
             ((AND (EQ !#CL (CAR F!:E!#LASTAIL)) (TAIL!-P F!:E!#LASTAIL !#Y))
              (SETQ !#X F!:E!#LASTAIL))
             (T (PROGN (TERPRI) (PRIN2 !#CL) (PRINT2 " - location uncertain")))
        )
       (COND ((EQ !#X (CAR F!:E!#LOCLST!#1))
              (SETQ F!:E!#LOCLST F!:E!#LOCLST!#1))
             (T (SETQ F!:E!#LOCLST (CONS !#X F!:E!#LOCLST!#1))))
       (RETURN NIL)))

(DE EDIT!* (!#N)
 (CAR (SETQ F!:E!#LOCLST
            ((LAMBDA (F!:E!#CMD F!:E!#LOCLST !#M)
              (PROGN (COND ((NOT (GREATERP !#M !#N)) (ERROR NIL NIL)))
                     (EDITCOM '!@0 NIL)
                     (EDITCOM (DIFFERENCE !#N !#M) NIL)
                     F!:E!#LOCLST))
             NIL
             F!:E!#LOCLST
             ((LAMBDA (F!:E!#LOCLST)
               (PROGN (EDITUP) (LENGTH (CAR F!:E!#LOCLST))))
              F!:E!#LOCLST)))))

(DE EDITOR (!#COMS)
 (PROG (!#RES)
  LP   (COND ((NULL !#COMS) (ERROR NIL NIL)))
       (SETQ !#RES
             (ERRORSET (LIST 'EDITOR1 (MKQUOTE !#COMS))
                       G!:EDIT!:ERRORS
                       G!:EDIT!:TRACE))
       (COND ((PAIRP !#RES) (RETURN (CAR F!:E!#LOCLST)))
             (!#RES (ERROR !#RES NIL)))
       (SETQ !#COMS (CDR !#COMS))
       (GO LP)))

(DE EDITOR1 (!#COMS)
 (SETQ F!:E!#LOCLST
       ((LAMBDA (F!:E!#LOCLST)
         (PROGN
          (COND ((ATOM (CAR !#COMS)) (EDITCOM (CAR !#COMS)))
                (T (EDITCOMS (CAR !#COMS))))
          F!:E!#LOCLST))
        F!:E!#LOCLST)))

(DE EDITERRCOM (!#COMS)
 (ERRORSET (LIST 'EDITCOMS (MKQUOTE !#COMS)) G!:EDIT!:ERRORS G!:EDIT!:TRACE))

(DE EDITRPT (!#EDRX !#QUIET)
 (PROG (!#EDRL !#EDRPTCNT)
       (SETQ !#EDRL F!:E!#LOCLST)
       (SETQ !#EDRPTCNT 0)
  LP   (COND ((GREATERP !#EDRPTCNT F!:E!#MAXLOOP)
              (PRINT2 " maxloop exceeded"))
             ((PAIRP (EDITERRCOM !#EDRX))
              (PROGN (SETQ !#EDRL F!:E!#LOCLST)
                     (SETQ !#EDRPTCNT (PLUS !#EDRPTCNT 1))
                     (GO LP)))
             ((NULL !#QUIET) (PROGN (PRIN1 !#EDRPTCNT)
                                    (PRINT2 " occurrences"))))
       (SETQ F!:E!#LOCLST !#EDRL)))

(DE EDITLOC (!#X)
 (PROG (!#OLDL !#OLDF F!:E!#LCFLG !#L)
       (SETQ !#OLDL F!:E!#LOCLST)
       (SETQ !#OLDF F!:E!#UNFIND)
       (SETQ F!:E!#LCFLG T)
       (COND ((ATOM !#X) (EDITCOM !#X NIL))
             ((AND (NULL (CDR !#X)) (ATOM (CAR !#X))) (EDITCOM (CAR !#X) NIL))
             (T (GO LP)))
       (SETQ F!:E!#UNFIND !#OLDL)
       (RETURN (CAR F!:E!#LOCLST))
  LP   (SETQ !#L F!:E!#LOCLST)
       (COND ((PAIRP (EDITERRCOM !#X))
              (PROGN (SETQ F!:E!#UNFIND !#OLDL) (RETURN (CAR F!:E!#LOCLST)))))
       (COND ((EQUAL !#L F!:E!#LOCLST)
              (PROGN (SETQ F!:E!#LOCLST !#OLDL)
                     (SETQ F!:E!#UNFIND !#OLDF)
                     (ERROR NIL NIL))))))

(DE EDITLOCL (!#COMS)
 (CAR (SETQ F!:E!#LOCLST
            (NCONC
             ((LAMBDA (F!:E!#LOCLST F!:E!#UNFIND)
               (PROGN (EDITLOC !#COMS) F!:E!#LOCLST))
              (LIST (CAR F!:E!#LOCLST))
              NIL)
             (CDR F!:E!#LOCLST)))))

(DE EDIT!: (!#TYPE !#LC !#X)
 (PROG (F!:E!#TOFLG F!:E!#LOCLST!#0)
       (SETQ F!:E!#LOCLST!#0 F!:E!#LOCLST)
       (SETQ !#X
             (MAPCAR !#X
                     (FUNCTION
                      (LAMBDA (!#X)
                       (COND ((AND (PAIRP !#X) (EQ (CAR !#X) '!#!#))
                              ((LAMBDA (F!:E!#LOCLST F!:E!#UNDOLST!#1)
                                (COPY (EDITCOMS (CDR !#X))))
                               F!:E!#LOCLST
                               NIL))
                             (T !#X))))))
       (COND (!#LC (PROGN (COND ((EQ (CAR !#LC) 'HERE) (SETQ !#LC (CDR !#LC))))
                          (EDITLOC !#LC))))
       (EDITUP)
       (COND ((EQ F!:E!#LOCLST!#0 F!:E!#LOCLST) (SETQ !#LC NIL)))
       (SELECTQ !#TYPE
                ((B BEFORE) (EDIT2F -1 !#X))
                ((A AFTER)
                 (COND ((CDAR F!:E!#LOCLST) (EDIT2F -2 !#X))
                       (T (EDITCOML (CONS 'N !#X) F!:E!#COPYFLG))))
                ((!: FOR)
                 (COND ((OR !#X (CDAR F!:E!#LOCLST)) (EDIT2F 1 !#X))
                       ((MEMQ (CAR F!:E!#LOCLST) (CADR F!:E!#LOCLST))
                        (PROGN (EDITUP) (EDIT2F 1 (LIST NIL))))
                       (T (EDITCOMS '(0 (NTH -2) (2)))))
                 (RETURN (COND ((NULL !#LC) F!:E!#LOCLST))))
                (ERROR NIL NIL))
       (RETURN NIL)))

(DE EDITMBD (!#LC !#X)
 (PROG (!#Y F!:E!#TOFLG)
       (COND (!#LC (EDITLOC !#LC)))
       (EDITUP)
       (SETQ !#Y
             (COND (F!:E!#TOFLG (CAAR F!:E!#LOCLST))
                   (T (LIST (CAAR F!:E!#LOCLST)))))
       (EDIT2F 1
               (LIST (COND ((OR (ATOM (CAR !#X)) (CDR !#X)) (APPEND !#X !#Y))
                           (T (LSUBST !#Y '!* (CAR !#X))))))
       (SETQ F!:E!#LOCLST
             (CONS (CAAR F!:E!#LOCLST)
                   (COND ((TAIL!-P (CAR F!:E!#LOCLST) (CADR F!:E!#LOCLST))
                          (CDR F!:E!#LOCLST))
                         (T F!:E!#LOCLST))))
       (RETURN (COND ((NULL !#LC) F!:E!#LOCLST)))))

(DE EDITXTR (!#LC !#X)
 (PROG (F!:E!#TOFLG)
       (COND (!#LC (EDITLOC !#LC)))
       ((LAMBDA (F!:E!#LOCLST F!:E!#UNFIND)
         (PROGN (EDITLOC !#X)
                (SETQ !#X
                      (COND ((TAIL!-P (CAR F!:E!#LOCLST) (CADR F!:E!#LOCLST))
                             (CAAR F!:E!#LOCLST))
                            (T (CAR F!:E!#LOCLST))))))
        (LIST (COND ((TAIL!-P (CAR F!:E!#LOCLST) (CADR F!:E!#LOCLST))
                     (CAAR F!:E!#LOCLST))
                    (T (CAR F!:E!#LOCLST))))
        NIL)
       (EDITUP)
       (EDIT2F 1 (COND (F!:E!#TOFLG (APPEND !#X NIL)) (T (LIST !#X))))
       (AND (NULL F!:E!#TOFLG)
            (PAIRP (CAAR F!:E!#LOCLST))
            (SETQ F!:E!#LOCLST
                  (CONS (CAAR F!:E!#LOCLST)
                        (COND ((TAIL!-P (CAR F!:E!#LOCLST)
                                        (CADR F!:E!#LOCLST))
                               (CDR F!:E!#LOCLST))
                              (T F!:E!#LOCLST)))))))

(DE EDITELT (!#LC F!:E!#LOCLST)
 (PROG (!#Y)
       (EDITLOC !#LC)
  LP   (SETQ !#Y F!:E!#LOCLST)
       (COND ((CDR (SETQ F!:E!#LOCLST (CDR F!:E!#LOCLST))) (GO LP)))
       (RETURN (CAR !#Y))))

(DE EDITCONT (!#LC1 F!:E!#SN)
 (SETQ F!:E!#LOCLST
       ((LAMBDA (F!:E!#LOCLST)
         (PROG (!#RES)
               (SETQ !#LC1 (EDITFPAT !#LC1))
          LP   (COND ((NULL (EDIT4F !#LC1 'N)) (ERROR NIL NIL)))
               (SETQ !#RES
                     (ERRORSET (LIST 'EDITLOCL (MKQUOTE F!:E!#SN))
                               G!:EDIT!:ERRORS
                               G!:EDIT!:TRACE))
               (COND ((NULL !#RES) (GO LP)) ((ATOM !#RES) (ERROR !#RES NIL)))
          LP1  (COND ((NULL (SETQ F!:E!#LOCLST (CDR F!:E!#LOCLST)))
                      (ERROR NIL NIL))
                     ((COND ((ATOM !#LC1) (EQ !#LC1 (CAAR F!:E!#LOCLST)))
                            ((EQ (CAR !#LC1) '!')
                             (EDIT4E !#LC1 (CAAR F!:E!#LOCLST)))
                            (T (EDIT4E !#LC1 (CAR F!:E!#LOCLST))))
                      (RETURN F!:E!#LOCLST)))
               (GO LP1)))
        F!:E!#LOCLST)))

(DE EDITSW (!#M !#N)
 (PROG (!#Y !#Z !#TEM)
       (SETQ !#Y (EDITNTH (CAR F!:E!#LOCLST) !#M))
       (SETQ !#Z (EDITNTH (CAR F!:E!#LOCLST) !#N))
       (SETQ !#TEM (CAR !#Y))
       (EDITSMASH !#Y (CAR !#Z) (CDR !#Y))
       (EDITSMASH !#Z !#TEM (CDR !#Z))))

(DE EDITMV (!#LC !#OP !#X)
 (PROG (F!:E!#LOCLST!#0 F!:E!#LOCLST!#1 !#Z F!:E!#TOFLG)
       (SETQ F!:E!#LOCLST!#0 F!:E!#LOCLST)
       (AND !#LC (EDITLOC !#LC))
       (COND ((EQ !#OP 'HERE)
              (PROGN (COND ((NULL !#LC) (PROGN (EDITLOC !#X) (SETQ !#X NIL))))
                     (SETQ !#OP '!:)))
             ((EQ (CAR !#X) 'HERE)
              (COND ((NULL !#LC) (PROGN (EDITLOC (CDR !#X)) (SETQ !#X NIL)))
                    (T (SETQ !#X (CDR !#X))))))
       (EDITUP)
       (SETQ F!:E!#LOCLST!#1 F!:E!#LOCLST)
       (SETQ !#Z (CAAR F!:E!#LOCLST))
       (SETQ F!:E!#LOCLST F!:E!#LOCLST!#0)
       (AND !#X (EDITLOC !#X))
       (EDITCOML
        (COND (F!:E!#TOFLG (CONS !#OP (APPEND !#Z NIL))) (T (LIST !#OP !#Z)))
        NIL)
       (PROG (F!:E!#LOCLST)
             (SETQ F!:E!#LOCLST F!:E!#LOCLST!#1)
             (EDITCOMS '(1 DELETE)))
       (RETURN
        (COND ((NULL !#LC)
               (PROGN (SETQ F!:E!#UNFIND F!:E!#LOCLST!#1) F!:E!#LOCLST))
              ((NULL !#X)
               (PROGN (SETQ F!:E!#UNFIND F!:E!#LOCLST!#1) F!:E!#LOCLST!#0))
              (T (PROGN (SETQ F!:E!#UNFIND F!:E!#LOCLST) F!:E!#LOCLST!#0))))))

(DE EDITTO (!#LC1 !#LC2 !#FLG)
 (PROGN
  (SETQ F!:E!#LOCLST
        ((LAMBDA (F!:E!#LOCLST)
          (PROGN (COND (!#LC1 (PROGN (EDITLOC !#LC1) (EDITUP))))
                 (EDITBI 1
                         (COND ((AND (NUMBERP !#LC1)
                                     (NUMBERP !#LC2)
                                     (GREATERP !#LC2 !#LC1))
                                (DIFFERENCE (PLUS !#LC2 1) !#LC1))
                               (T !#LC2))
                         (CAR F!:E!#LOCLST))
                 (COND ((AND (EQ !#FLG 'TO) (CDAAR F!:E!#LOCLST))
                        (EDITRI 1 -2 (CAR F!:E!#LOCLST))))
                 (EDITCOM 1 NIL)
                 F!:E!#LOCLST))
         F!:E!#LOCLST))
  (SETQ F!:E!#TOFLG T)))

(DE EDITBELOW (!#PLACE !#DEPTH)
 (PROGN (COND ((LESSP (SETQ !#DEPTH (EVAL !#DEPTH)) 0) (ERROR NIL NIL)))
        (PROG (!#N1 !#N2)
              (SETQ !#N1
                    (LENGTH
                     ((LAMBDA (F!:E!#LOCLST F!:E!#LCFLG)
                       (PROGN (EDITCOM !#PLACE NIL) F!:E!#LOCLST))
                      F!:E!#LOCLST
                      '!_)))
              (SETQ !#N2 (LENGTH F!:E!#LOCLST))
              (COND ((LESSP !#N2 (PLUS !#N1 !#DEPTH)) (ERROR NIL NIL)))
              (SETQ F!:E!#UNFIND F!:E!#LOCLST)
              (SETQ F!:E!#LOCLST
                    (NTH!-TAIL F!:E!#LOCLST
                               (DIFFERENCE (DIFFERENCE (PLUS !#N2 1) !#N1)
                                           !#DEPTH))))))

(DE EDITRAN (!#C !#DEF)
 (SETQ F!:E!#LOCLST
       (OR ((LAMBDA (F!:E!#LOCLST)
             (PROG (!#Z !#W)
                   (COND ((NULL !#DEF) (ERROR NIL NIL))
                         ((NULL (SETQ !#Z (CAR !#DEF))) (GO OUT)))
              LP   (COND ((NULL !#Z) (ERROR NIL NIL))
                         ((NULL (SETQ !#W (MEMQ (CAR !#Z) !#C)))
                          (PROGN (SETQ !#Z (CDR !#Z)) (GO LP))))
              OUT  (SETQ !#Z
                         (APPLY (CAR (SETQ !#DEF (CADR !#DEF)))
                                (PROG (F!:E!#1 F!:E!#2 F!:E!#3)
                                      (SETQ F!:E!#1 (CDR (LDIFF !#C !#W)))
                                      (SETQ F!:E!#2 (CAR !#Z))
                                      (SETQ F!:E!#3 (CDR !#W))
                                      (RETURN
                                       (MAPCAR (CDR !#DEF)
                                               (FUNCTION
                                                (LAMBDA (!#X)
                                                 (SELECTQ !#X
                                                  (!#1 F!:E!#1)
                                                  (!#2 F!:E!#2)
                                                  (!#3 F!:E!#3)
                                                  (EVAL !#X)))))))))
                   (RETURN
                    (COND ((NULL !#Z)
                           (PROGN (SETQ F!:E!#UNFIND F!:E!#LOCLST) NIL))
                          (T !#Z)))))
            F!:E!#LOCLST)
           F!:E!#LOCLST)))

(DE EDIT!#PRINT (!#E !#DEPTH !#DOTFLG)
 (PROG (!#RES)
       (SETQ !#RES
             (ERRORSET
              (LIST 'DEPTH!#PRINT (MKQUOTE !#E) !#DEPTH 0 (MKQUOTE !#DOTFLG))
              G!:EDIT!:ERRORS
              G!:EDIT!:TRACE))
       (COND ((EQ !#RES 'TOOBIG) (RETURN (PRINT2 " ...> ")))
             ((ATOM !#RES) (ERROR !#RES NIL)))
       (RETURN !#E)))

(DE DEPTH!#PRINT (!#E !#DEPTH !#PLENGTH !#DOTFLG)
 (PROG NIL
       (OR (LESSP (SETQ !#PLENGTH (ADD1 !#PLENGTH)) F!:E!#MAX!#PLENGTH)
           (ERROR 'TOOBIG NIL))
       (COND ((ATOM !#E) (PROGN (PRIN1 !#E) (RETURN !#PLENGTH)))
             ((ZEROP !#DEPTH) (PROGN (PRIN2 "&") (RETURN !#PLENGTH))))
       (PRIN2 (COND (!#DOTFLG "... ") (T "(")))
       (SETQ !#DEPTH (SUB1 !#DEPTH))
  LOOP (SETQ !#PLENGTH (DEPTH!#PRINT (CAR !#E) !#DEPTH !#PLENGTH NIL))
       (SETQ !#E (CDR !#E))
       (COND ((NULL !#E) NIL)
             ((ATOM !#E) (PROGN (PRIN2 " . ") (PRIN1 !#E)))
             (T (PROGN (PRIN2 " ") (GO LOOP))))
       (PRIN2 ")")
       (RETURN !#PLENGTH)))

(!* 
"LDIFF( X:list Y:list ):list                         EXPR
    -----
    If X is a tail of Y, returns the list difference of X and Y,
    a list of the elements of Y preceeding X.")

(CDE LDIFF (!#X !#Y)
 (COND ((OR (EQ !#X !#Y) (ATOM !#X)) NIL)
       ((NULL !#Y) !#X)
       (T (PROG (!#V !#Z)
                (SETQ !#Z (SETQ !#V (LIST (CAR !#X))))
           LOOP (SETQ !#X (CDR !#X))
                (COND ((OR (EQ !#X !#Y) (ATOM !#X)) (RETURN !#Z)))
                (SETQ !#V (CDR (RPLACD !#V (LIST (CAR !#X)))))
                (GO LOOP)))))

(!* "FREELIST is an efficiency hack in the DEC interpreter."
"It explicitly returns the cells of a list to the freelist.")

(CDE FREELIST (!#X) NIL)

(!* "EDITRACEFN is an optional debugging routine for the editor.")

(CDE EDITRACEFN (!#X) NIL)

(DE PRINT2 (!#X) (PROGN (PRIN2 !#X) (TERPRI) !#X))

(SETQ F!:E!#LOOKDPTH -1)

(SETQ F!:E!#DEPTH -1)

(SETQ F!:E!#TRACEFLG NIL)

(SETQ F!:E!#LAST!#ID NIL)

(SETQ F!:E!#MAXLEVEL 300)

(SETQ F!:E!#UPFINDFLG T)

(SETQ F!:E!#MAXLOOP 30)

(SETQ F!:E!#EDITCOMSL
 '(S R E I N P F FS F!= ORF BF NTH IF RI RO LI LO BI BO M NX BK ORR MBD XTR
   THRU TO A B !: AFTER BEFORE FOR MV LP LPQ LC LCL !_ BELOW SW BIND COMS 
COMSQ INSERT REPLACE CHANGE DELETE EMBED SURROUND MOVE EXTRACT SECOND THIRD 
NEX REPACK MAKEFN))

(SETQ F!:E!#USERMACROS NIL)

(SETQ F!:E!#MAX!#PLENGTH 1750)

(SETQ F!:E!#MACROS
 '((MAKEFN (EX ARGS N M)
           (IF 'M
               ((BI N M) (LC . N) (BELOW !\))
               ((IF 'N ((BI N) (LC . N) (BELOW !\)))))
           (E (MAPC '(LAMBDA (!#X !#Y) (EDITDSUBST !#X !#Y (EDIT!#!#)))
                    'ARGS
                    (CDR 'EX))
              T)
           (E (PUTD (CAR 'EX) 'EXPR (CONS 'LAMBDA (CONS 'ARGS (EDIT!#!#)))) 
T)         UP
           (1 EX))
   (REPACK !#X (LC . !#X) REPACK)
   (REPACK NIL
           (IF (PAIRP (EDIT!#!#)) (1) NIL)
           (I !: (PRINT (READLIST (EDITE (EXPLODE (EDIT!#!#)) NIL NIL)))))
   (NEX (!#X) (BELOW !#X) NX)
   (NEX NIL (BELOW !_) NX)
   (THIRD !#X (ORR ((LC . !#X) (LC . !#X) (LC . !#X))))
   (SECOND !#X (ORR ((LC . !#X) (LC . !#X))))))

(SETQ F!:E!#OPS
 '((INSERT (BEFORE AFTER FOR) (EDIT!: F!:E!#2 F!:E!#3 F!:E!#1))
   (REPLACE (WITH BY) (EDIT!: !: F!:E!#1 F!:E!#3))
   (CHANGE (TO) (EDIT!: !: F!:E!#1 F!:E!#3))
   (DELETE NIL (EDIT!: !: F!:E!#1 NIL))
   (EMBED (IN WITH) (EDITMBD F!:E!#1 F!:E!#3))
   (SURROUND (WITH IN) (EDITMBD F!:E!#1 F!:E!#3))
   (MOVE (TO) (EDITMV F!:E!#1 (CAR F!:E!#3) (CDR F!:E!#3)))
   (EXTRACT (FROM) (EDITXTR F!:E!#3 F!:E!#1))))

