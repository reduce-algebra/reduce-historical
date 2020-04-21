(!* 
"ZFILES contains 2 packages --
    (1) YFILES -- useful functions for accessing files.
    (2) YTOPCOM -- useful functions for compiling files. ")

(!* 
" YFILES -- BASIC FILE ACCESSING UTILITIES

FORM-FILE       ( FILE:DSCR ): filename                 EXPR
GRABBER         ( SELECTION FILE:DSCR ): NIL            EXPR
DUMPER          ( FILE:DSCR ): NIL                      EXPR
DUMPFNS-DE      ( SELECTION FILE:DSCR ): NIL            EXPR
DUMP-REMAINING  ( SELECTION:list DUMPED:list ): NIL     EXPR
FCOPY           ( IN:DSCR OUT:DSCR filedscrs ):boolean  EXPR
REFPRINT-FOR-GRAB-CTL( #X: any ):NIL                    EXPR

G:CREFON      Switched on by cross reference program CREF:FILE
G:JUST:FNS    Save only fn names in variable whose name is the first
              field of filename if T, O/W save all exprs in that variable
G:FILES       List of files read into LISP
G:SHOW:TRACE  Turns backtrace in ERRORSET on if T
G:SHOW:ERRORS Prints ERRORSET error messages if T

")

(GLOBAL '(G!:FILES G!:CREFON G!:JUST!:FNS))

(GLOBAL '(G!:SHOW!:ERRORS G!:SHOW!:TRACE))

(FLUID '(F!:FILE!:ID F!:OLD!:FILE PPPRINT))

(FLUID '(DUMP!#ID))

(!* 
"GRAB( <file description> )                  MACRO
    ===> (GRABBER NIL '<file-dscr>)
    Reads in entire file, whose system name is created using
    conventions described in FORM-FILE.  See ZMACROS.")

(!* 
"GRABFNS( <ids> . <file description> )       MACRO
    ===> (GRABBER IDS <file-dscr>)
    Like GRAB, but only reads in specified ids.  See ZMACROS.")

(!* 
"FORM-FILE( FILE:DSCR ): filename              EXPR
    ---------
    Takes a file dscr, possibly NIL, and returns a file name
    corresponding to that dscr and suitable as an argument to OPEN.
    F:OLD:FILE is set to this file name for future reference.
    Meanwhile, F:FILE:ID is set to a lisp identifier, and the file
    name is put on the OPEN:FILE:NAME property of that identifier.
    The identifier can be used to hold info about the file.
    E.g. its value may be a list of objects read from the file.

    NB:  FORM-FILE is at the lowest level of machine-independant code.
    MAKE-OPEN-FILE-NAME is a system dependant routine that creates
    file names specifically tailored to the version of SLISP in use.
")

(DE FORM!-FILE (FILE!#DSCR)
 (PROG (!#TEMP)
       (COND ((IDP FILE!#DSCR) (MAKE FILE!#DSCR NCONS)))
       (!* 
"COND below: case 1--defaults to most recent file referenced
                  case 2--virtual file name: access property list
                  case 3--build usable file name from all or part
                          of FILE:DSCR given")
       (COND ((NULL (CAR FILE!#DSCR))
              (COND (F!:OLD!:FILE
                     (PROGN (TTY " = " F!:FILE!:ID) (RETURN F!:OLD!:FILE)))
                    (T (ERROR 0 "No file specified and no default file."))))
             ((SETQ !#TEMP (GET (CAR FILE!#DSCR) 'OPEN!:FILE!:NAME))
              (PROGN (SETQ F!:FILE!:ID (CAR FILE!#DSCR))
                     (RETURN (SETQ F!:OLD!:FILE !#TEMP))))
             (T (RETURN (MAKE!-OPEN!-FILE!-NAME FILE!#DSCR))))))

(!* 
"GRABBER( SELECTION:id-list FILE:DSCR ):T            EXPR
    -------
    Opens the specified file, applies GRAB-EVAL-CTL to each
    expression on it, and then closes it.  Returns T.
    See GRAB-EVAL-CTL for important side effects.")

(DE GRABBER (!#SELECTION FILE!#DSCR)
 (PROG (!#Y EXPR!#READ !#ICHAN IBASE FILE!#ID FILE!#NAME)
       (SETQ FILE!#NAME (FORM!-FILE FILE!#DSCR))
       (!* SETQ FILE!#NAME (GET FILE!#ID 'FILE!:NAME))
       (SETQ FILE!#ID F!:FILE!:ID)
       (SETQ G!:FILES (NCONC1 G!:FILES FILE!#ID))
       (SET FILE!#ID (LIST NIL))
       (SETQ IBASE (PLUS 5 5))
       (RDS (SETQ !#ICHAN (OPEN FILE!#NAME 'INPUT)))
  LOOP (SETQ EXPR!#READ (ERRORSET '(READ) T G!:SHOW!:TRACE))
       (COND (!#SELECTION (PRINA ".")))
       (COND ((AND (PAIRP EXPR!#READ) (NEQ !$EOF!$ (CAR EXPR!#READ)))
              (PROGN
               (ERRORSET
                (LIST 'GRAB!-EVAL!-CTL
                      (MKQUOTE !#SELECTION)
                      (MKQUOTE (CAR EXPR!#READ))
                      (MKQUOTE FILE!#ID))
                T
                G!:SHOW!:TRACE)
               (COND ((NOT (SUBSET !#SELECTION (CDR (EVAL FILE!#ID))))
                      (GO LOOP))))))
       (RDS NIL)
       (CLOSE !#ICHAN)
       (SET FILE!#ID (DREMOVE NIL (EVAL FILE!#ID)))
       (TERPRI)
       (RETURN T)))

(!* 
"GRAB-EVAL-CTL( #SELECTION EXPR#READ FILE#ID )       EXPR
    -------------
    Examines each expression read from file, and determines whether
    to EVAL that expression.  Also decides whether to append the
    expression, or an id taken from it, or nothing at all, to the
    value of the file id poined at by FILE#ID.
    The file id is stored for use as an argument to DUMP or COMPILE,
    for example.
    Note: G:JUSTFNS suppresses the storage of comments from the file.
          When reading LAP files, no list of fns is made.")

(DE GRAB!-EVAL!-CTL (!#SELECTION EXPR!#READ FILE!#ID)
 (COND ((ATOM EXPR!#READ) NIL)
       ((AND (EQ (CAR EXPR!#READ) 'SETQ) (EQ (CADR EXPR!#READ) FILE!#ID)) 
NIL)   ((AND (OR (NULL !#SELECTION) (MEMBER (CADR EXPR!#READ) !#SELECTION))
             (MEMBER (CAR EXPR!#READ) '(DE DF DM SETQ CDE CDF CDM C!-SETQ)))
        (PROGN (PRINA (CADR EXPR!#READ))
               (EVAL EXPR!#READ)
               (COND ((AND (NEQ (CADR EXPR!#READ) 'IBASE)
                           (NOT (MEMBER (CADR EXPR!#READ) (EVAL FILE!#ID)))
                           (NOT (MEMBER (CAR EXPR!#READ) '(LAP CLAP))))
                      (NCONC1 (EVAL FILE!#ID) (CADR EXPR!#READ))))))
       ((NULL !#SELECTION)
        (PROGN (OR G!:JUST!:FNS (NCONC1 (EVAL FILE!#ID) EXPR!#READ))
               (!* "G:JUST:FNS reduces consumption of string space.")
               (COND (G!:CREFON (REFPRINT!-FOR!-GRAB!-CTL EXPR!#READ)))
               (EVAL EXPR!#READ)
               (PRINA (CCAR EXPR!#READ))))))

(!* 
"DUMPER( FILE:DSCR : file-dscr ): NIL       EXPR
    ------
    Dumps file onto disk.  Filename as in GRABBER.
    Prettyprints the defined functions, set variables, and evaluated
    expressions which are members of the value of the variable filename.
    (For DEC versions:
     If IBASE neq 10, puts (SETQ IBASE current:base) at head of file.)")

(DE DUMPER (!#DSCR)
 (PROG (!#OCHAN OLD!#OCHAN FILE!#ID)
       (!* SETQ FILE!#ID (FORM!-FILE !#DSCR))
       (SETQ !#OCHAN (OPEN (FORM!-FILE !#DSCR) 'OUTPUT))
       (SETQ FILE!#ID F!:FILE!:ID)
       (SETQ OLD!#OCHAN (WRS !#OCHAN))
       (MAPC (EVAL FILE!#ID) (FUNCTION PP1))
       (CLOSE !#OCHAN)
       (WRS OLD!#OCHAN)
       (RETURN T)))

(!* 
"DUMPFNS-DE( FNS FILE:DSCR ): NIL            EXPR
    ----------
    Like DUMPER. Copies old file, putting new definitions for specified
    functions/variables.
    E.g.: (DUMPFNS-DE '(A B) '(FOO)) will first copy verbatim all the
    expressions on FOO.LSP which do not define A or B.
    Then the core definitions of A and B are dumped onto the file.")

(DE DUMPFNS!-DE (!#SELECTION FILE!#DSCR)
 (PROG (FILE!#ID FILE!#NAME IBASE !#OLD !#DUMPED !#ICHAN !#OCHAN OLD!#ICHAN
        OLD!#OCHAN !#ID)
       (SETQ FILE!#NAME (FORM!-FILE FILE!#DSCR))
       (SETQ FILE!#ID F!:FILE!:ID)
       (SETQ IBASE (PLUS 5 5))
       (SETQ OLD!#ICHAN (RDS (SETQ !#ICHAN (OPEN FILE!#NAME 'INPUT))))
       (SETQ OLD!#OCHAN (WRS (SETQ !#OCHAN (OPEN FILE!#NAME 'OUTPUT))))
  LOOP (SETQ !#OLD (ERRORSET '(READ) G!:SHOW!:ERRORS G!:SHOW!:TRACE))
       (COND ((OR (ATOM !#OLD) (EQ (SETQ !#OLD (CAR !#OLD)) !$EOF!$))
              (PROGN (!* "dump remaining selected objects")
                     (DUMP!-REMAINING !#SELECTION !#DUMPED)
                     (CLOSE !#ICHAN)
                     (CLOSE !#OCHAN)
                     (RDS OLD!#ICHAN)
                     (WRS OLD!#OCHAN)
                     (RETURN T))))
       (COND ((AND (PAIRP !#OLD)
                   (MEMBER (CAR !#OLD) '(SETQ DE DF DM CDE CDF CDM))
                   (MEMBER (SETQ !#ID (CADR !#OLD)) !#SELECTION))
              (PROGN
               (SETQ !#DUMPED
                     (CONS (CONS !#ID
                                 (COND ((EQ 'SETQ (CAR !#OLD))
                                        (PROGN (PP!-VAL !#ID) 'VAL))
                                       (T (PROGN (PP!-DEF !#ID) 'DEF))))
                           !#DUMPED))
               (GO LOOP))))
       (COND ((AND (PAIRP !#OLD)
                   (EQ (CAR !#OLD) 'SETQ)
                   (EQ (CADR !#OLD) 'IBASE))
              (ERRORSET !#OLD T G!:SHOW!:TRACE)))
       (TERPRI)
       (APPLY PPPRINT (LIST !#OLD 1))
       (TERPRI)
       (TERPRI)
       (GO LOOP)))

(!* 
"DUMP-REMAINING( SELECTION:list DUMPED:list )         EXPR
    --------------
    Taken out of DUMPFNS for ease of reading.
    Dumps those properties of items in selection which have not
    already been dumped.")

(DE DUMP!-REMAINING (!#SELECTION !#DUMPED)
 (PROG (DUMP!#ID !#IGNORE)
  LOOP (SETQ DUMP!#ID (CAR !#SELECTION))
       (SETQ !#IGNORE
             (MAPCAN !#DUMPED
                     (FUNCTION
                      (LAMBDA (!#PAIR)
                       (COND ((EQ DUMP!#ID (CAR !#PAIR)) (LIST (CDR !#PAIR)))))
                      )))
       (OR (MEMBER 'VAL !#IGNORE) (PP!-VAL DUMP!#ID))
       (OR (MEMBER 'DEF !#IGNORE) (PP!-DEF DUMP!#ID))
       (COND ((SETQ !#SELECTION (CDR !#SELECTION)) (GO LOOP)))))

(!* 
"FCOPY( IN:DSCR filename, OUT:DSCR filename ):boolean  EXPR
    -----
    Reformats file using the prettyprinter.  Useful for removing
    angle brackets or for tightening up function format.
    Returns T on normal exit, NIL if error reading file. ")

(DE FCOPY (IN!#DSCR OUT!#DSCR)
 (PROG (IN!#CHAN OUT!#CHAN !#EXP)
       (SETQ IN!#CHAN (OPEN (FORM!-FILE IN!#DSCR) 'INPUT))
       (SETQ OUT!#CHAN (OPEN (FORM!-FILE OUT!#DSCR) 'OUTPUT))
       (RDS IN!#CHAN)
       (WRS OUT!#CHAN)
       (LINELENGTH 80)
  LOOP (SETQ !#EXP (ERRORSET '(READ) T T))
       (COND ((OR (ATOM !#EXP) (EQ (CAR !#EXP) !$EOF!$))
              (PROGN (CLOSE IN!#CHAN)
                     (RDS NIL)
                     (CLOSE OUT!#CHAN)
                     (WRS NIL)
                     (RETURN (EQ !#EXP !$EOF!$)))))
       (SETQ !#EXP (CAR !#EXP))
       (TTY ".")
       (COND ((ATOM !#EXP) (SPRINT !#EXP 1))
             ((MEMQ (CAR !#EXP) '(DE DF DM CDE CDF CDM))
              (PROGN (PRIN2 "(")
                     (PRIN1 (CAR !#EXP))
                     (PRIN2 " ")
                     (PRIN1 (CADR !#EXP))
                     (PRIN2 " ")
                     (PRIN1 (CADDR !#EXP))
                     (S2PRINT " " (CADDDR !#EXP))
                     (PRIN2 ")")))
             ((EQ (CAR !#EXP) 'SETQ)
              (PROGN (PRIN2 "(")
                     (PRIN1 (CAR !#EXP))
                     (PRIN2 " ")
                     (PRIN1 (CADR !#EXP))
                     (S2PRINT " " (CADDR !#EXP))
                     (PRIN2 ")")))
             (T (SPRINT !#EXP 1)))
       (TERPRI)
       (TERPRI)
       (GO LOOP)))

(!* 
"FCOPY-SQ ( IN:DSCR filename, OUT:DSCR filename ):boolean  EXPR
    -----
    Reformats file using the compacting printer.  Letterizes
    and reports via '<big>' message long strings.
    Returns T on normal exit, NIL if error reading file. ")

(DE FCOPY!-SQ (IN!#DSCR OUT!#DSCR)
 (PROG (IN!#CHAN OUT!#CHAN !#EXP)
       (SETQ IN!#CHAN (OPEN (FORM!-FILE IN!#DSCR) 'INPUT))
       (SETQ OUT!#CHAN (OPEN (FORM!-FILE OUT!#DSCR) 'OUTPUT))
       (RDS IN!#CHAN)
       (WRS OUT!#CHAN)
  LOOP (SETQ !#EXP (ERRORSET '(READ) T T))
       (COND ((ATOM !#EXP)
              (PROGN (CLOSE IN!#CHAN)
                     (RDS NIL)
                     (CLOSE OUT!#CHAN)
                     (WRS NIL)
                     (RETURN (EQ !#EXP !$EOF!$))))
             ((EQ (SETQ !#EXP (CAR !#EXP)) !$EOF!$)
              (PROGN (CLOSE IN!#CHAN) (CLOSE OUT!#CHAN) (RETURN T))))
       (TTY ".")
       (PRIN1SQ !#EXP)
       (TERPRI)
       (TERPRI)
       (GO LOOP)))

(!* "Dummy -- may be replaced by real cref routine.")

(DE REFPRINT!-FOR!-GRAB!-CTL (!#X) NIL)

(!* 
" YTOPCOM -- Compiler Control functions

(DF COMPILE-FILE (FILE:NAME)
(DF COMPILE-IN-CORE (FILE:NAME)

")

(!* 
"Commonly used globals.  Declared in this file so each individual
    file doesn't have to declare them.  ")

(GLOBAL '(!#SELECTQ G!:SHOW!:ERRORS G!:SHOW!:TRACE))

(!* "Other globals/fluids")

(GLOBAL '(!*SAVEDEF))

(FLUID '(F!:FILE!:ID COMPILED!:FNS))

(!* "This flag is checked by COMPILE-FILE.")

(FLAG '(EXPR FEXPR) 'COMPILE)

(!* 
"PPLAP( MODE CODE )                          EXPR
    -----
   Prints the lap code in some appropriate format.
   Currently uses PRIN1SQ (PRIN1, Safe, use apostrophe to Quote
   non-numeric expressions).")

(DE PPLAP (!#MODE !#CODE) (PRIN1SQ (LIST !#MODE (MKQUOTE !#CODE))))

(!* 
"COMPILE-FILE( FILE:DSCR )                   FEXPR
    ------------
    Reads the given file, and creates a corresponding LAP file.
    Each expression on the original file is mapped into an expression
    on the LAP file.
    Comments map into NIL.
    Function definitions map into the corresponding LAP code.
    These definitions are compiled, but NOT evaluated -- hence the
    functions will not be loaded into this core image by this routine.
    All other expressions are evaluated in an errorset then copied verbatim.
    EXCEPTION:  UNFLUID is evalutated, but converted into a comment
        when printed, to avoid confusing loader.
")

(FLUID '(QUIET_FASLOUT!*))

(!* "Controls printing of welcome message in FASLOUT.")

(DF COMPILE!-FILE (FILE!:DSCR)
 (PROG (IN!:SEXPR LSP!:FILE LAP!:FILE OLD!:SAVEDEF LAP!:FN!:NAME LAP!:OUT
	 QUIET_FASLOUT!*
        LAP!:FN LSP!:FILE!:ID OCHAN ICHAN TYPE MODE)
       (!* 
"*SAVEDEF Saves LAP code generated by the compiler on the property
           list of the function under indicator COMPEXP")
(!*       (SETQ OLD!:SAVEDEF !*SAVEDEF)
       (SETQ !*SAVEDEF T))
       (SETQ QUIET_FASLOUT!* T)
       (GCMSG NIL)
       (!* 
"Note: If FILE:DSCR = (AAA BBB) then
            TENEX: from LSP:FILE = '<AAA>BBB.LSP', LSP:FILE:ID = BBB
                     to LAP:FILE = '<AAA>BBB.LAP', LAP:FILE:ID = BBB
              CMS: from LSP:FILE = 'AAA BBB', LSP:FILE:ID = AAA
                     to LAP:FILE = 'AAA LAP', LAP:FILE:ID = AAA
           This is non-ideal, since the first filename gets lost.
           It is not clear, however, what an elegant solution would be.
           Perhaps the file id should have a list of filenames, one for
           each extension... ")
       (SETQ LSP!:FILE (FORM!-FILE FILE!:DSCR))
       (SETQ LSP!:FILE!:ID F!:FILE!:ID)
       (SETQ ICHAN (OPEN LSP!:FILE 'INPUT))
       (!* "Try to create lap file corresponding to LSP file.")
       (SETQ LAP!:FILE (SUBST '!; 'LSP LSP!:FILE))
       (!* "But if that doesn't work out..")
       (COND ((EQUAL LSP!:FILE LAP!:FILE)
              (SETQ LAP!:FILE (FORM!-FILE (CONS LSP!:FILE!:ID '!;)))))
       (!* SETQ LAP!:FILE!:ID F!:FILE!:ID)
       (ERRORSET (LIST 'ERASE (MKQUOTE LAP!:FILE))
                 G!:SHOW!:ERRORS
                 G!:SHOW!:TRACE)
       (!*(SETQ OCHAN (OPEN LAP!:FILE 'OUTPUT)))
       (FASLOUT LAP!:FILE)
       (RDS ICHAN)
       (WHILE
        (AND (PAIRP (SETQ IN!:SEXPR (ERRORSET '(READ) NIL NIL)))
             (NOT (EQ (SETQ IN!:SEXPR (CAR IN!:SEXPR)) !$EOF!$)))
        (!* PROGN (SETQ COMPILED!:FNS NIL)
               (SETQ TYPE
                     (SELECTQ (CAR IN!:SEXPR)
                              ((DE CDE) 'EXPR)
                              ((DF CDF) 'FEXPR)
                              ((DM CDM) 'MACRO)
                              NIL))
               (SETQ MODE
                     (SELECTQ (CAR IN!:SEXPR)
                              ((CDE CDF CDM) 'CLAP)
                              ((DE DF DM) 'LAP)
                              NIL))
               (COND ((FLAGP TYPE 'COMPILE)
                      (PROG NIL
                            (PRINA (SETQ LAP!:FN!:NAME (CADR IN!:SEXPR)))
                            (SETQ LAP!:OUT
                                  (SIMPLIFYLAP
                                   (CONS (LIST '!*ENTRY
                                               LAP!:FN!:NAME
                                               TYPE
                                               (LENGTH (CADDR IN!:SEXPR)))
                                         (!&COMPROC
                                          (CONS 'LAMBDA (CDDR IN!:SEXPR))
                                          LAP!:FN!:NAME))))
                            (WRS OCHAN)
                            (!* LOOP
                               (SETQ LAP!:OUT
                                     (CDR (REMPROP LAP!:FN!:NAME 'COMPEXP))))
                            (PPLAP MODE LAP!:OUT)
                            (TERPRI)
                            (!*(COND ((SETQ COMPILED!:FNS
                                            (DREMOVE LAP!:FN!:NAME
                                             COMPILED!:FNS))
                                      (PROGN
                                       (SETQ LAP!:FN!:NAME
                                             (CCAR COMPILED!:FNS))
                                       (GO LOOP)))))
                            (WRS NIL)
                            (PRINA "ok")))
                     ((MEMQ (CAR IN!:SEXPR) '(!* !*!*)) NIL)
                     ((EQ (CAR IN!:SEXPR) 'UNFLUID) (EVAL IN!:SEXPR))
                     (T (PROGN
                         (ERRORSET (LIST 'EVAL (MKQUOTE IN!:SEXPR)) T NIL)
                         (!* "Be sure errors are printed to terminal")
                         (WRS OCHAN)
                         (SPRINT IN!:SEXPR 1)
                         (TERPRI)
                         (WRS NIL)))))
	    (DFPRINTFASL IN!:SEXPR))
       (SETQ !*SAVEDEF OLD!:SAVEDEF)
       (CLOSE ICHAN)
       (RDS NIL)
   (!* (CLOSE OCHAN))
       (FASLEND)))

(!* 
"COMPILE-IN-CORE( FILE:DSCR ):NIL              FEXPR
    ---------------
   Compiles all EXPRS and FEXPRS on a file and loads compiled code into
   core.  Creates a file FILE:NAME.cpl which is a compilation log
   consisting of the names of functions compiled and the space used in
   their loading.")

(DF COMPILE!-IN!-CORE (FILE!:DSCR)
 (PROG (IN!:SEXPR LAP!:FN!:NAME LAP!:FN LOG!:FILE LOG!:CHAN LSP!:CHAN
        LSP!:FILE!:ID LSP!:FILE)
       (SETQ LSP!:FILE (FORM!-FILE FILE!:DSCR))
       (SETQ LSP!:FILE!:ID F!:FILE!:ID)
       (SETQ LSP!:CHAN (OPEN LSP!:FILE 'INPUT))
       (SETQ LOG!:FILE (FORM!-FILE (CONS LSP!:FILE!:ID 'CPL)))
       (SETQ LOG!:CHAN (OPEN LOG!:FILE 'OUTPUT))
       (RDS LSP!:CHAN)
       (WHILE
        (AND (PAIRP
              (SETQ IN!:SEXPR
                    (ERRORSET '(READ) G!:SHOW!:ERRORS G!:SHOW!:TRACE)))
             (NOT (EQ !$EOF!$ (SETQ IN!:SEXPR (CAR IN!:SEXPR))))
             (PAIRP (ERRORSET IN!:SEXPR G!:SHOW!:ERRORS G!:SHOW!:TRACE)))
        (COND ((MEMQ (CAR IN!:SEXPR) '(DE DF CDE CDF))
               (PROGN (SETQ LAP!:FN!:NAME (CADR IN!:SEXPR))
                      (WRS LOG!:CHAN)
                      (COMPILE (NCONS LAP!:FN!:NAME))
                      (WRS NIL)
                      (PRINA LAP!:FN!:NAME)))))
       (SETQ COMPILED!:FNS NIL)
       (RDS NIL)
       (CLOSE LSP!:CHAN)
       (CLOSE LOG!:CHAN)))

(!* 
"GCMSG( X:boolean ):any              EXPR
    -----
    Pre-defined in both SLISP and new IBM intpreter, so this cde shouln't
    do anything.  GCMSG turns the garbage collection msgs on or off.")

(CDE GCMSG (!#X) NIL)

