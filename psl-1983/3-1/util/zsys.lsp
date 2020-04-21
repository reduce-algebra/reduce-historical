(!* 
"ZSYS -- the system dependent file.
    Currently, the only code in it is MAKE-OPEN-FILE-NAME, which
    uses a semi machine-independant file description to create a
    filename suitable for OPEN in the resident system.

    N.B.: TO SET THIS CODE UP FOR A PARTICULAR INTEPRETER,
          REMOVE THE * FROM BEFORE THE APPROPRIATE SETQ BELOW.
          THAT SHOULD BE ALL YOU NEED TO DO.

")

(COMPILETIME
(GLOBAL '(G!:SYSTEM))

(IF!_SYSTEM TOPS20
(SETQ G!:SYSTEM 'PSL!-TOPS20))

(IF!_SYSTEM UNIX
(SETQ G!:SYSTEM 'PSL!-UNIX))

(!* SETQ G!:SYSTEM 'IMSSS!-TENEX)

(!* SETQ G!:SYSTEM 'UTAH!-TOPS10)

(!* SETQ G!:SYSTEM 'UTAH!-TENEX)

(!* SETQ G!:SYSTEM 'CMS)

(!* SETQ G!:SYSTEM 'ORVYL)

(PROGN (TERPRI)
       (PRIN2 "Filenames will be made for ")
       (PRIN2 G!:SYSTEM)
       (PRIN2 " system.")
       (TERPRI))
)

(FLUID '(F!:FILE!:ID F!:OLD!:FILE))

(COMPILETIME
(!* 
"This macro (and those following) are separated only for readability.
    The appropriate MAKE-xxx-NAME will provide the body of the definition
    for MAKE-OPEN-FILE-NAME.
    Note: (a) #DSCR can be mentioned free in the macros since it is the
              lambda variable for MAKE-OPEN-FILE-NAME.
          (b) ORVYL and CMS differ only in the delimiter they use.
          (c) When compiling, all these macros are REMOB'ed to clear up
              otherwise extraneous code.")

(DM MAKE!-SYS!-FILE!-NAME (!#X)
 (SELECTQ G!:SYSTEM
          (PSL!-TOPS20 '(MAKE!-PSL!-TOPS20!-NAME))
          (PSL!-UNIX '(MAKE!-PSL!-UNIX!-NAME))
          (UTAH!-TENEX '(MAKE!-UTAH!-TENEX!-NAME))
          (UTAH!-TOPS10 '(MAKE!-UTAH!-TOPS10!-NAME))
          (IMSSS!-TENEX '(MAKE!-IMSSS!-TENEX!-NAME))
          (ORVYL '(MAKE!-IBM!-NAME !.))
          (CMS '(MAKE!-IBM!-NAME ! ))
          (ERROR 0
                 (LIST "Don't know how to make file names for system "
                  G!:SYSTEM))))

(DM MAKE!-UTAH!-TENEX!-NAME (!#X)
 '(PROG (!#DIR !#NAM !#EXT)
        (RETURN
         (SETQ F!:OLD!:FILE
               (COND ((NULL (PAIRP !#DSCR))
                      (ERROR 0 (LIST "BAD FILE DSCR: " !#DSCR)))
                     ((NULL (CDR !#DSCR))
                      (LIST (CONS (SETQ F!:FILE!:ID (CAR !#DSCR)) 'LSP)))
                     ((EQ (CDR !#DSCR) '!;)
                      (LIST (SETQ F!:FILE!:ID (CAR !#DSCR))))
                     ((IDP (CDR !#DSCR))
                      (PROGN (SETQ F!:FILE!:ID (CAR !#DSCR)) (LIST !#DSCR)))
                     (T (PROGN (SETQ !#DIR (CAR !#DSCR))
                               (SETQ F!:FILE!:ID (SETQ !#NAM (CADR !#DSCR)))
                               (SETQ !#EXT
                                     (COND ((NULL (CDDR !#DSCR)) 'LSP)
                                           ((IDP (CDDR !#DSCR)) (CDDR !#DSCR))
                                           (T (CADDR !#DSCR))))
                               (LIST 'DIR!: !#DIR (CONS !#NAM !#EXT)))))))))

(!* 
"Use decimal equivalent of PPNs for tops 10.  Maybe the ROCT switch
      in the interpreter will allow octal PPNS??")

(DM MAKE!-UTAH!-TOPS10!-NAME (!#X)
 '(PROG (!#DIR !#NAM !#EXT)
        (RETURN
         (SETQ F!:OLD!:FILE
               (COND ((NULL (PAIRP !#DSCR))
                      (ERROR 0 (LIST "BAD FILE DSCR: " !#DSCR)))
                     ((NULL (CDR !#DSCR))
                      (LIST (CONS (SETQ F!:FILE!:ID (CAR !#DSCR)) 'LSP)))
                     ((EQ (CDR !#DSCR) '!;)
                      (LIST (SETQ F!:FILE!:ID (CAR !#DSCR))))
                     ((IDP (CDR !#DSCR))
                      (PROGN (SETQ F!:FILE!:ID (CAR !#DSCR)) (LIST !#DSCR)))
                     (T (PROGN (SETQ !#DIR (CAR !#DSCR))
                               (COND ((NOT (AND (PAIRP !#DIR)
                                                (NUMBERP (CAR !#DIR))
                                                (NUMBERP (CADR !#DIR))))
                                      (BUG!-STOP
                       "Bad PPN: USE (<n> <n>) w/ decimal equiv of octal PPN.")
                                      ))
                               (SETQ F!:FILE!:ID (SETQ !#NAM (CADR !#DSCR)))
                               (SETQ !#EXT
                                     (COND ((NULL (CDDR !#DSCR)) 'LSP)
                                           ((IDP (CDDR !#DSCR)) (CDDR !#DSCR))
                                           (T (CADDR !#DSCR))))
                               (LIST !#DIR (CONS !#NAM !#EXT)))))))))

(DM MAKE!-IMSSS!-TENEX!-NAME (!#X)
 '(PROG (DIR!#NAM !#EXT)
        (!* "#DSCR is a list")
        (RETURN
         (SETQ F!:OLD!:FILE
               (LIST (COND ((NULL (PAIRP !#DSCR))
                            (ERROR 0 (LIST "BAD FILE DSCR: " !#DSCR)))
                           ((NULL (CDR !#DSCR))
                            (CONS (SETQ F!:FILE!:ID (CAR !#DSCR)) 'LSP))
                           ((EQ (CDR !#DSCR) '!;)
                            (SETQ F!:FILE!:ID (CAR !#DSCR)))
                           ((IDP (CDR !#DSCR))
                            (PROGN (SETQ F!:FILE!:ID (CAR !#DSCR)) !#DSCR))
                           (T (PROGN
                               (SETQ DIR!#NAM
                                     (COMPRESS
                                      (NCONCL (LIST '!! '!<)
                                              (EXPLODE (CAR !#DSCR))
                                              (LIST '!! '!>)
                                              (EXPLODE (CADR !#DSCR)))))
                               (SETQ F!:FILE!:ID (CADR !#DSCR))
                               (SETQ !#EXT
                                     (COND ((NULL (CDDR !#DSCR)) 'LSP)
                                           ((IDP (CDDR !#DSCR)) (CDDR !#DSCR))
                                           (T (CADDR !#DSCR))))
                               (CONS DIR!#NAM !#EXT)))))))))

(DM MAKE!-PSL!-TOPS20!-NAME (!#X)
 '(PROG (DIR!#NAM !#EXT)
        (!* "#DSCR is a list")
	(COND ((STRINGP !#DSCR) (MAKE !#DSCR NCONS)))
        (RETURN
         (SETQ F!:OLD!:FILE
               (COND ((NULL (PAIRP !#DSCR))
                      (ERROR 0 (LIST "BAD FILE DSCR: " !#DSCR)))
                     ((NULL (CDR !#DSCR))
                      (COND ((STRINGP (CAR !#DSCR))
                             (PROGN
                              (SETQ F!:FILE!:ID
                                    (EXTRACT!-FILE!-ID (CAR !#DSCR)))
                              (CAR !#DSCR)))
                            (T (ID!-LIST!-TO!-STRING
                                (LIST (SETQ F!:FILE!:ID (CAR !#DSCR))
                                      '!.
                                      'LSP)))))
                     ((EQ (CDR !#DSCR) '!;)
                      (ID2STRING (SETQ F!:FILE!:ID (CAR !#DSCR))))
                     ((IDP (CDR !#DSCR))
                      (ID!-LIST!-TO!-STRING
                       (LIST (SETQ F!:FILE!:ID (CAR !#DSCR)) '!. (CDR !#DSCR)))
                      )
                     (T (PROGN
                         (SETQ DIR!#NAM
                               (COMPRESS
                                (NCONCL (LIST '!! '!<)
                                        (EXPLODE (CAR !#DSCR))
                                        (LIST '!! '!>)
                                        (EXPLODE (CADR !#DSCR)))))
                         (SETQ F!:FILE!:ID (CADR !#DSCR))
                         (SETQ !#EXT
                               (COND ((NULL (CDDR !#DSCR)) 'LSP)
                                     ((IDP (CDDR !#DSCR)) (CDDR !#DSCR))
                                     (T (CADDR !#DSCR))))
                         (ID!-LIST!-TO!-STRING (LIST DIR!#NAM '!. !#EXT)))))))))


(DM MAKE!-PSL!-UNIX!-NAME (!#X)
 '(PROG (DIR!#NAM !#EXT)
        (!* "#DSCR is a list")
	(COND ((STRINGP !#DSCR) (MAKE !#DSCR NCONS)))
        (RETURN
         (SETQ F!:OLD!:FILE
               (COND ((NULL (PAIRP !#DSCR))
		      (ERROR 0 (LIST "BAD FILE DSCR: " !#DSCR)))
		     ((NULL (CDR !#DSCR))
		      (COND ((STRINGP (CAR !#DSCR))
			     (PROGN (SETQ F!:FILE!:ID
					  (EXTRACT!-FILE!-ID (CAR
							      !#DSCR)))
				    (CAR !#DSCR)))
			    (T (ID!-LIST!-TO!-STRING (LIST (SETQ
							    F!:FILE!:ID
							    (CAR
							     !#DSCR))
							   '!.
							   'LSP)))))
		     ((EQ (CDR !#DSCR) '!;)
		      (ID2STRING (SETQ F!:FILE!:ID (CAR !#DSCR))))
		     ((IDP (CDR !#DSCR))
		      (ID!-LIST!-TO!-STRING (LIST (SETQ F!:FILE!:ID
							(CAR !#DSCR))
						  '!.
						  (CDR !#DSCR))))
		     (T (PROGN (SETQ DIR!#NAM
				     (COMPRESS (NCONCL (EXPLODE (CAR
								 !#DSCR))
						       (LIST '!!
							     '!/)
						       (EXPLODE (CADR
								 !#DSCR)))))
			       (SETQ F!:FILE!:ID (CADR !#DSCR))
			       (SETQ !#EXT
				     (COND ((NULL (CDDR !#DSCR))
					    'LSP)
					   ((IDP (CDDR !#DSCR))
					    (CDDR !#DSCR))
					   (T (CADDR !#DSCR))))
			       (ID!-LIST!-TO!-STRING (LIST DIR!#NAM
							   '!.
							   !#EXT))))))))))

(IF!_SYSTEM TOPS20 (PROGN
(DE EXTRACT!-FILE!-ID (!#X)
 (PROG (!#Y)
       (!* 
"Take a TOPS-20 filename string and try to
      find a root file name in it")
       (SETQ !#Y (DREVERSE (EXPLODE2 !#X)))
       (SETQ !#X !#Y)
  LOOP1(COND ((OR (NULL !#X) (MEMQ (CAR !#X) '(!: !>))) (GO LOOP1END))
             ((EQ (CAR !#X) '!.) (PROGN (SETQ !#Y (CDR !#X)) (GO LOOP1END))))
       (SETQ !#X (CDR !#X))
       (GO LOOP1)
  LOOP1END
       (SETQ !#X !#Y)
  LOOP2(COND ((OR (NULL !#X) (NULL (CDR !#X))) (GO LOOP2END))
             ((MEMQ (CADR !#X) '(!> !:))
              (PROGN (RPLACD !#X NIL) (GO LOOP2END))))
       (SETQ !#X (CDR !#X))
       (GO LOOP2)
  LOOP2END
       (RETURN (ICOMPRESS (DREVERSE !#Y)))))

(DE ID!-LIST!-TO!-STRING (!#X)
 (PROG (!#S)
       (SETQ !#S "")
  LOOP (COND ((NULL !#X) (RETURN !#S)))
       (SETQ !#S (CONCAT !#S (ID2STRING (CAR !#X))))
       (SETQ !#X (CDR !#X))
       (GO LOOP)))))

(IF!_SYSTEM UNIX (PROGN
(DE EXTRACT!-FILE!-ID (!#X)
 (PROG (!#Y)
       (!* 
"Take a UNIX filename string and try to
find a root file name in it")
       (SETQ !#Y (DREVERSE (EXPLODE2 !#X)))
       (SETQ !#X !#Y)
  LOOP1(COND ((OR (NULL !#X) (MEMQ (CAR !#X) '(!: !>))) (GO LOOP1END))
             ((EQ (CAR !#X) '!.) (PROGN (SETQ !#Y (CDR !#X)) (GO LOOP1END))))
       (SETQ !#X (CDR !#X))
       (GO LOOP1)
  LOOP1END
       (SETQ !#X !#Y)
  LOOP2(COND ((OR (NULL !#X) (NULL (CDR !#X))) (GO LOOP2END))
             ((MEMQ (CADR !#X) '(!> !:))
              (PROGN (RPLACD !#X NIL) (GO LOOP2END))))
       (SETQ !#X (CDR !#X))
       (GO LOOP2)
  LOOP2END
       (RETURN (ICOMPRESS (DREVERSE !#Y)))))

(FLUID '(!*LOWER))

(!* "*LOWER when T all output (including EXPLODE) is in lowercase")

(DE ID!-LIST!-TO!-STRING (!#X)
 (PROG (!#S !*LOWER)
       (SETQ !*LOWER T)
       (SETQ !#S "")
  LOOP (COND ((NULL !#X) (RETURN !#S)))
       (SETQ !#S (CONCAT !#S (LIST2STRING (EXPLODE2 (CAR !#X)))))
       (SETQ !#X (CDR !#X))
       (GO LOOP)))))

(!* "IBM code got lost")

(DE MAKE!-OPEN!-FILE!-NAME (!#DSCR) (MAKE!-SYS!-FILE!-NAME))

(!* "Remove excess baggage once macros have been used.")

(!* COND ((CODEP (CDR (GETD 'MAKE!-OPEN!-FILE!-NAME)))
       (PROGN (REMOB 'MAKE!-SYS!-FILE!-NAME)
              (REMOB 'MAKE!-UTAH!-TENEX!-NAME)
              (REMOB 'MAKE!-UTAH!-TOPS10!-NAME)
              (REMOB 'MAKE!-IMSSS!-TENEX!-NAME)
              (REMOB 'MAKE!-IBM!-NAME))))

