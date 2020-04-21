(DM !* (!#X) NIL)

(SETQ !*EOLINSTRINGOK T)

(!* 
"Needed for PSL, to avoid error messages while reading strings which
contain carriage returns.")

(!* 
"*( X:any ): NIL                             MACRO
    ===> NIL
    For comments--doesn't evaluate anything.  Returns NIL.
    Note: expressions starting with * which are read by the
    lisp scanner must obey all the normal syntax rules.")

(!* 
" ZBOOT -- Bootstrapping functions and SLISP extensions

ONEP (U)                EXPR  used where?
LIST2 (U V)             EXPR  compiler support fn
LIST3 (U V W)           EXPR  compiler support fn
LIST4 (U V W X)         EXPR  compiler support fn
LIST5 (U V W X Y)       EXPR  compiler support fn
MAPOBL (!*PI!*)         EXPR  UTAH random utility
REVERSIP (U)            EXPR  UTAH support fn
WARNING  (U)            EXPR  UTAH support fn

IMSSS additions: (complement LOSE mechanism)

CDEF (FDSCR TYPE)       EXPR   conditional function definition
CDE (Z)                 FEXPR  conditional expr  definition
CDF (Z)                 FEXPR  conditional fexpr definition
CDM (Z)                 FEXPR  conditional macro definition
CLAP( LAPCODE )         FEXPR  conditional lap   definition
C-SETQ (#ARGS)          FEXPR  conditional setq

These are for compatibility with the IBM interpreter:

ERASE( #FILE: file descriptor ):NIL       EXPR

")

(!* "ARE THESE USED ONLY IN COMPILER PACKAGE?")

(!* (REMFLAG '(LIST2 LIST3 LIST4 LIST5 REVERSIP) 'LOSE))

(!* (GLOBAL '(OBLIST)))

(!* "IMSSS additions: ")

(!* 
"CDEF( FNDSCR: pair, TYPE: {expr,fexpr,macro} ): {id,NIL}    EXPR
    ----
   Conditional function definition.
   #FNDSCR = (NAME ARGS BODY)   #TYPE = {EXPR, FEXPR, or MACRO}
   If the function is already defined, a warning is printed,
   the function is not redefined, and nil is returned.
   Otherwise, the function is defined and the name is returned.
   CDEF is called by CDE, CDM and CDF, analogs to DE, DF and DM.")

(!*
(DE CDEF (!#FDSCR !#TYPE)
 (PROG (!#NAME !#NEWARGS !#NEWBODY !#OLDDEF)
       (COND ((ATOM !#FDSCR) (RETURN (WARNING "Bad arg to CDEF."))))
       (SETQ !#NAME (CAR !#FDSCR))
       (COND ((NOT (EQUAL (LENGTH !#FDSCR) 3))
              (RETURN (WARNING (LIST "Bad args to CDEF for " !#NAME)))))
       (SETQ !#NEWARGS (CADR !#FDSCR))
       (SETQ !#NEWBODY (CADDR !#FDSCR))
       (COND ((NULL (SETQ !#OLDDEF (GETD !#NAME)))
              (RETURN (PUTD !#NAME !#TYPE (LIST 'LAMBDA !#NEWARGS !#NEWBODY))))
             ((PAIRP (CDR !#OLDDEF))
              (WARNING
               (LIST !#NAME
                     " already "
                     (LENGTH (CADDR !#OLDDEF))
                     "-arg "
                     (CAR !#OLDDEF)
                     ", not redefined as "
                     (LENGTH !#NEWARGS)
                     "-arg "
                     !#TYPE)))
             (T (WARNING
                 (LIST !#NAME
                       " is a compiled "
                       (CAR !#OLDDEF)
                       ", not redefined as "
                       (LENGTH !#NEWARGS)
                       "-arg "
                       !#TYPE))))))

(DF CDE (!#Z) (CDEF !#Z 'EXPR))

(DF CDF (!#Z) (CDEF !#Z 'FEXPR))

(DF CDM (!#Z) (CDEF !#Z 'MACRO))

(!* 
"CLAP( LAPCODE ): {id,NIL}                                   EXPR
    ----
   Conditional lap definition.
   If the function already has a compiled definition, warning is given,
   the function is not redefined, and nil is returned.
   Otherwise, LAP is called.")

(DE CLAP (LAP!#CODE)
 (PROG (!#ENTRY !#ID OLD!#DEF)
       (COND ((NULL (SETQ !#ENTRY (ASSOC '!*ENTRY LAP!#CODE)))
              (RETURN (WARNING "CLAP: No *ENTRY in lap code."))))
       (SETQ !#ID (CADR !#ENTRY))
       (SETQ OLD!#DEF (GETD !#ID))
       (COND ((OR (NULL OLD!#DEF) (PAIRP (CDR OLD!#DEF))) (LAP LAP!#CODE))
             (T (WARNING
                 (LIST !#ID
                       " is compiled "
                       (CAR OLD!#DEF)
                       ", not changed to compiled "
                       (CADDR !#ENTRY)
                       "."))))))
)

(DM CDE (!#X) (CONS 'DE (CDR !#X)))

(DM CDF (!#X) (CONS 'DF (CDR !#X)))

(DM CDM (!#X) (CONS 'DM (CDR !#X)))

(!* 
"C-SETQ( ARGS: (id any)): any                FEXPR
    ------
   Conditional SETQ.
   If the cadr of #ARGS is already defined, it is not reset and its old
   value is returned.  Otherwise, it acts like SETQ.  ")

(DF C!-SETQ (!#ARGS)
 (COND ((PAIRP (ERRORSET (CAR !#ARGS) NIL NIL)) (EVAL (CAR !#ARGS)))
       (T (SET (CAR !#ARGS) (EVAL (CADR !#ARGS))))))

(!* "This CDE is best left here to avoid bootstrapping problems.")

(CDE WARNING (!#X!#)
 (PROG (!#CHAN!#)
       (SETQ !#CHAN!# (WRS NIL))
       (TERPRI)
       (PRIN2 "*** ")
       (COND ((ATOM !#X!#) (PRIN2 !#X!#)) (T (MAPC !#X!# (FUNCTION PRIN2))))
       (TERPRI)
       (WRS !#CHAN!#)))

(!*
(CDE ONEP (U) (OR (EQUAL U 1) (EQUAL U 1.0)))

(CDE LIST2 (U V) (CONS U (CONS V NIL)))

(CDE LIST3 (U V W) (CONS U (CONS V (CONS W NIL))))

(CDE LIST4 (U V W X) (CONS U (CONS V (CONS W (CONS X NIL)))))

(CDE LIST5 (U V W X Y) (CONS U (CONS V (CONS W (CONS X (CONS Y NIL))))))
)

(!* 
"This definition of MAPOBL doesn't work in PSL, because the oblist has
a different structure. MAPOBL is defined in the interpreter though.")

(!*(CDE MAPOBL
        (!*PI!*)
        (FOREACH X IN OBLIST DO (FOREACH Y IN X DO (APPLY !*PI!* (LIST Y))))))

(!*
(CDE REVERSIP (U)
 (PROG (X Y)
       (WHILE U (PROGN (SETQ X (CDR U)) (SETQ Y (RPLACD U Y)) (SETQ U X)))
       (RETURN Y)))
)

(!* 
"ERASE( #FILE: file descriptor ):NIL       EXPR
    -----
    This is defined in the IBM interpreter to (irrevocably) delete
    a file from the file system, which is a highly necessary operation
    when you are not allowed versions of files.
    It should be a no-op in the TENEX interpreters until such an
    operation seems necessary.  This assumes the user will delete and
    expunge old versions from the exec.")

(CDE ERASE (!#FILE) NIL)

