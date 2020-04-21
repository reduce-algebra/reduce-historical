%%%%%%%%%%%%  Standard - LISP Verification file. %%%%%%%%%%%%%%%%%%%%%%%
%
% Copyright (C) M. Griss and J. Marti, February 1981
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Flags for SYSLISP based PSL


(SETQ !*ECHO T)
(SETQ FZERO (FLOAT 0))
(SETQ FONE (FLOAT 1))
(SETQ !*RAISE 'T)

%  The following should return T:
T 
(NULL NIL) 
(COND (T T)) 
(COND (NIL NIL) (T T)) 
%  The following should return NIL:
NIL 
(NULL T) 
(COND (T NIL)) 
(COND (NIL T) 
      (T NIL)) 
%  The following should be 0
0 (QUOTE 0) 
%  The following minimum set of functions must work:
%   PUTD, PROG, SET, QUOTE, COND, NULL, RETURN, LIST, CAR, CDR,
%   EVAL, PRINT, PRIN1, TERPRI, PROGN, GO.
%  Check PUTD, GETD, LAMBDA 
(PUTD (QUOTE FOO) (QUOTE EXPR) (QUOTE (LAMBDA (X) 3)))
% Expect (EXPR LAMBDA (X) 3)
(GETD (QUOTE FOO))
%  Should return 3
(FOO 1)
(FOO 2)
%  Test SET :
(SET (QUOTE A) 1)
A
(SET (QUOTE B) 2)
B
%  Test LIST, CAR, CDR 
%  Expect (1 2 3 4) 1 and (2 3 4) 
(SET (QUOTE A) (LIST 1 2 3 4))
(CAR A)
(CDR A)
% Test REDEFINITION in PUTD, PROGN, PRIN1, TERPRI 
(PUTD (QUOTE FOO) (QUOTE EXPR) 
      (QUOTE (LAMBDA (X) (PROGN (PRIN1 X) (TERPRI)))))
%  expect 1 and 2 printed , value NIL
(FOO 1)
(FOO 2)
%  Test simple PROG, GO, RETURN 
(PROG NIL (PRINT 1) (PRINT 2))
(PROG (A) (PRINT A) (PRINT 1))
% Now test GO, RETURN, PROG binding
(SET 'A 'AA) (SET 'B 'BB)
(PROG (A B) (PRINT 'test! binding! of! A!,! B! expect! NIL)
            (PRIN1 A) (PRINT B) 
            (PRINT 'Reset! to! 1!,2)
            (SET 'A 1) (SET 'B 2)
               (PRIN1 A) (PRINT B)
            (PRINT 'test! forward! GO)
               (GO LL)
               (PRINT 'forward! GO! failed)
LL            (PRINT 'Forward! GO! ok)
            (GO L2)
L1        (PRINT '! Should! be! after! BACKWARD! go )
        (PRINT '! now! return! 3)
        (RETURN 3)
L2        (PRINT 'Test! backward! GO)
        (GO L1) )
% Test that A,B correctly rebound, expect AA and BB% 
A B
%  Test simple FEXPR% 
(PUTD (QUOTE FOO) (QUOTE FEXPR) (QUOTE (LAMBDA (X) (PRINT X))))
% Expect (FEXPR LAMBDA (X) (PRINT X))% 
(GETD (QUOTE FOO))
%  Expect (1) (1 2) and (1 2 3)% 
(FOO 1)
(FOO 1 2)
(FOO 1 2 3)
%  Finally, TEST EVAL inside an FEXPR % 
(PUTD (QUOTE FOO) (QUOTE FEXPR)
  (QUOTE (LAMBDA (XX) (PRINT (EVAL (CAR XX))))))
(FOO 1)
(FOO (NULL NIL))
%  PUTD is being used here to define a function !$TEST.%  
(PUTD (QUOTE !$TEST) (QUOTE FEXPR) (QUOTE 
  (LAMBDA (!$X) 
   (PROG (A B) 
     (SET (QUOTE A) (CDR !$X)) 
LOOP (while A (progn
         %  (print (list 'trying (car a)))
           (SET (QUOTE B) (EVAL (CAR A)))
           (COND ( (null (eq b T)) 
            (PROGN (PRIN1 (LIST '!*!*!*!*!*  (CAR A) '! returned B)) 
                   (TERPRI)))) 
     (SET (QUOTE A) (CDR A)) 
     (GO LOOP)))
     (return (LIST (CAR !$X) '! test! complete)) 
           )))) 
% $TEST should be defined.
(GETD (QUOTE !$TEST)) 
%  Global, vector, function-pointer partial test.
(!$TEST 'GLOBAL!,VECTOR (NULL (GLOBAL (QUOTE (!$VECTOR !$CODE TEMP)))) 
     (GLOBALP (QUOTE !$VECTOR)) 
     (GLOBALP (QUOTE !$CODE)) 
     (SET (QUOTE !$VECTOR) (MKVECT 4)) 
     (SET (QUOTE !$CODE) (CDR (GETD (QUOTE CDR)))) ) 
 
 
(!$TEST LIST (EQUAL (LIST 1 (QUOTE A) 'STRING ) 
                    (QUOTE (1 A STRING)))) 

% -----3.1 Elementary Predicates-----%  
% This section tests the elementary predicates of section 3.1 of 
% the Standard LISP Report. In general they will test that the 
% predicate returns non-NIL for the correct case, and NIL for all 
% others.  
 
% CODEP should not return T for numbers as function 
% pointers must not be implemented in this way.  
(!$TEST CODEP (CODEP !$CODE) (NULL (CODEP 1)) 
     (NULL (CODEP T)) (NULL (CODEP NIL)) 
     (NULL (CODEP (QUOTE IDENTIFIER))) 
     (NULL (CODEP 'STRING)) (NULL (CODEP (QUOTE (A . B)))) 
     (NULL (CODEP (QUOTE (A B C)))) 
     (NULL (CODEP !$VECTOR)) ) 
 
% PAIRP must not return T for vectors even if vectors are 
% implemented as lists.  
(!$TEST PAIRP 
     (PAIRP (QUOTE (A . B))) (PAIRP (QUOTE (NIL))) 
     (PAIRP (QUOTE (A B C))) (NULL (PAIRP 0)) 
     (NULL (PAIRP (QUOTE IDENTIFIER))) 
     (NULL (PAIRP 'STRING)) 
     (NULL (PAIRP !$VECTOR)) ) 
 
(!$TEST FIXP (FIXP 1) 
     (NULL (FIXP (QUOTE IDENTIFIER))) 
     (NULL (FIXP (QUOTE 'STRING))) 
     (NULL (FIXP (QUOTE (A . B)))) 
     (NULL (FIXP (QUOTE (A B C)))) 
     (NULL (FIXP !$VECTOR)) 
     (NULL (FIXP !$CODE))  ) 
 
% T and NIL must test as identifiers as must specially 
% escaped character identifiers.  
(!$TEST IDP (IDP (QUOTE IDENTIFIER)) 
     (IDP NIL)  (IDP T) 
     (IDP (QUOTE !1)) (IDP (QUOTE !A)) (IDP (QUOTE !!)) 
     (IDP (QUOTE !()) (IDP (QUOTE !))) (IDP (QUOTE !.)) 
     (IDP (QUOTE !')) (IDP (QUOTE !*)) (IDP (QUOTE !/)) 
     (IDP (QUOTE !+)) (IDP (QUOTE !-)) (IDP (QUOTE !#)) 
     (IDP (QUOTE ! )) (IDP (QUOTE !1!2!3)) (IDP (QUOTE !*!*!*)) 
     (IDP (QUOTE !'ID)) 
     (NULL (IDP 1)) 
     (NULL (IDP 'STRING)) 
     (NULL (IDP (QUOTE (A . B)))) 
     (NULL (IDP (QUOTE (A B C)))) 
     (NULL (IDP !$VECTOR)) 
     (NULL (IDP !$CODE)) ) 
 
% STRINGP should answer T to strings only and not things 
% that might look like strings if the system implements them as 
% identifiers.  
(!$TEST STRINGP (STRINGP 'STRING) 
     (NULL (STRINGP (QUOTE (STRING NOTASTRING)))) 
     (NULL (STRINGP 1)) 
     (NULL (STRINGP (QUOTE A))) 
     (NULL (STRINGP (QUOTE (A . B)))) 
     (NULL (STRINGP (QUOTE (A B C)))) 
     (NULL (STRINGP !$VECTOR)) 
     (NULL (STRINGP !$CODE)) ) 
 
% VECTORP should not answer T to pairs if vectors are 
% implemented as pairs.  
(!$TEST VECTORP (VECTORP !$VECTOR) 
     (NULL (VECTORP 1)) 
     (NULL (VECTORP (QUOTE A))) 
     (NULL (VECTORP 'STRING)) 
     (NULL (VECTORP (QUOTE (A . B)))) 
     (NULL (VECTORP (QUOTE (A B C)))) 
     (NULL (VECTORP !$CODE)) ) 
 
% Vectors are constants in Standard LISP. However T and NIL 
% are special global variables with the values T and NIL.  
(!$TEST CONSTANTP (CONSTANTP 1) 
     (CONSTANTP 'STRING) 
     (CONSTANTP !$VECTOR) 
     (CONSTANTP !$CODE) 
     (NULL (CONSTANTP NIL)) 
     (NULL (CONSTANTP T)) 
     (NULL (CONSTANTP (QUOTE A))) 
     (NULL (CONSTANTP (QUOTE (A . B)))) 
     (NULL (CONSTANTP (QUOTE (A B C)))) ) 
 
% An ATOM is anything that is not a pair, thus vectors are 
% atoms.  
(!$TEST ATOM (ATOM T) (ATOM NIL) (ATOM 1) (ATOM 0) 
     (ATOM 'STRING) (ATOM (QUOTE IDENTIFIER)) 
     (ATOM !$VECTOR) 
     (NULL (ATOM (QUOTE (A . B)))) 
     (NULL (ATOM (QUOTE (A B C)))) ) 
 
 
(!$TEST EQ (EQ NIL NIL) (EQ T T) 
     (EQ !$VECTOR !$VECTOR) 
     (EQ !$CODE !$CODE) 
     (EQ (QUOTE A) (QUOTE A)) 
     (NULL (EQ NIL T)) 
     (NULL (EQ NIL !$VECTOR)) 
     (NULL (EQ (QUOTE (A . B)) (QUOTE (A . B)))) ) 
 
% Function pointers are not numbers, therefore the function 
% pointer $CODE is not EQN to the fixed number 0. Numbers must have 
% the same type to be EQN.  
(!$TEST EQN (EQN 1 1) (EQN 0 0) 
     (EQN FONE FONE)  (EQN FZERO FZERO) 
     (NULL (EQN FONE FZERO)) (NULL (EQN FZERO FONE)) 
     (NULL (EQN 1 FONE)) (NULL (EQN 0 FZERO)) 
     (NULL (EQN 1 0)) (NULL (EQN 0 1)) 
     (NULL (EQN 0 !$CODE)) 
     (NULL (EQN NIL 0)) 
     (EQN NIL NIL)  (EQN T T) (EQN !$VECTOR !$VECTOR) 
     (EQN !$CODE !$CODE) (EQN (QUOTE A) (QUOTE A)) 
     (NULL (EQN (QUOTE (A . B)) (QUOTE (A . B)))) 
     (NULL (EQN (QUOTE (A B C)) (QUOTE (A B C))))  ) 
 
% EQUAL checks for general equality rather than specific, so 
% it must check all elements of general expressions and all elements 
% of vectors for equality. This test assumes that CAR does not have 
% the function pointer value  EQUAL to 0. Further tests of EQUAL 
% are in the vector section 3.9.  
(!$TEST EQUAL (EQUAL NIL NIL) 
     (EQUAL T T) 
     (NULL (EQUAL NIL T)) 
     (EQUAL !$CODE !$CODE) 
     (NULL (EQUAL !$CODE (CDR (GETD (QUOTE CAR))))) 
     (EQUAL (QUOTE IDENTIFIER) (QUOTE IDENTIFIER)) 
     (NULL (EQUAL (QUOTE IDENTIFIER1) (QUOTE IDENTIFIER2))) 
     (EQUAL 'STRING 'STRING) 
     (NULL (EQUAL 'STRING1 'STRING2)) 
     (EQUAL 0 0) 
     (NULL (EQUAL 0 1)) 
     (EQUAL (QUOTE (A . B)) (QUOTE (A . B))) 
     (NULL (EQUAL (QUOTE (A . B)) (QUOTE (A . C)))) 
     (NULL (EQUAL (QUOTE (A . B)) (QUOTE (C . B)))) 
     (EQUAL (QUOTE (A B)) (QUOTE (A B))) 
     (NULL (EQUAL (QUOTE (A B)) (QUOTE (A C)))) 
     (NULL (EQUAL (QUOTE (A B)) (QUOTE (C B)))) 
     (EQUAL !$VECTOR !$VECTOR) 
     (NULL (EQUAL 0 NIL)) 
     (NULL (EQUAL 'T T)) 
     (NULL (EQUAL 'NIL NIL)) ) 
 
% -----3.2 Functions on Dotted-Pairs-----%  
% Test the C....R functions by simply verifying that they select
% correct part of a structure.
(!$TEST CAR (EQ (CAR (QUOTE (A . B))) (QUOTE A)) 
    (EQUAL (CAR (QUOTE ((A) . B))) (QUOTE (A))) ) 
 
(!$TEST CDR (EQ (CDR (QUOTE (A . B))) (QUOTE B)) 
     (EQUAL (CDR (QUOTE (A B))) (QUOTE (B))) ) 
 
(!$TEST CAAR (EQ (CAAR (QUOTE ((A)))) (QUOTE A))) 
(!$TEST CADR (EQ (CADR (QUOTE (A B))) (QUOTE B))) 
(!$TEST CDAR (EQ (CDAR (QUOTE ((A . B)))) (QUOTE B))) 
(!$TEST CDDR (EQ (CDDR (QUOTE (A . (B . C)))) (QUOTE C))) 
 
(!$TEST CAAAR (EQ (CAAAR (QUOTE (((A))))) (QUOTE A))) 
(!$TEST CAADR (EQ (CAADR (QUOTE (A (B)))) (QUOTE B))) 
(!$TEST CADAR (EQ (CADAR (QUOTE ((A B)))) (QUOTE B))) 
(!$TEST CADDR (EQ (CADDR (QUOTE (A B C))) (QUOTE C))) 
(!$TEST CDAAR (EQ (CDAAR (QUOTE (((A . B)) C))) (QUOTE B))) 
(!$TEST CDADR (EQ (CDADR (QUOTE (A (B . C)))) (QUOTE C))) 
(!$TEST CDDAR (EQ (CDDAR (QUOTE ((A . (B . C))))) (QUOTE C))) 
(!$TEST CDDDR (EQ (CDDDR (QUOTE (A . (B . (C . D))))) (QUOTE D))) 
 
(!$TEST CAAAAR (EQ (CAAAAR (QUOTE ((((A)))))) (QUOTE A))) 
(!$TEST CAAADR (EQ (CAAADR (QUOTE (A ((B))))) (QUOTE B))) 
(!$TEST CAADAR (EQ (CAADAR (QUOTE ((A (B))))) (QUOTE B))) 
(!$TEST CAADDR (EQ (CAADDR (QUOTE (A . (B (C))))) (QUOTE C))) 
(!$TEST CADAAR (EQ (CADAAR (QUOTE (((A . (B)))))) (QUOTE B))) 
(!$TEST CADADR (EQ (CADADR (QUOTE (A (B . (C))))) (QUOTE C))) 
(!$TEST CADDAR (EQ (CADDAR (QUOTE ((A . (B . (C)))))) (QUOTE C))) 
(!$TEST CADDDR (EQ (CADDDR (QUOTE (A . (B . (C . (D)))))) (QUOTE D))) 
(!$TEST CDAAAR (EQ (CDAAAR (QUOTE ((((A . B)))))) (QUOTE B))) 
(!$TEST CDAADR (EQ (CDAADR (QUOTE (A ((B . C))))) (QUOTE C))) 
(!$TEST CDADAR (EQ (CDADAR (QUOTE ((A (B . C))))) (QUOTE C))) 
(!$TEST CDADDR (EQ (CDADDR (QUOTE (A . (B . ((C . D)))))) (QUOTE D))) 
(!$TEST CDDAAR (EQ (CDDAAR (QUOTE (((A . (B . C)))))) (QUOTE C))) 
(!$TEST CDDADR (EQ (CDDADR (QUOTE (A . ((B . (C . D)))))) (QUOTE D))) 
(!$TEST CDDDAR (EQ (CDDDAR (QUOTE ((A  . (B . (C . D)))))) (QUOTE D))) 
(!$TEST CDDDDR (EQ (CDDDDR (QUOTE (A . (B . (C . (D . E)))))) (QUOTE E))) 
 
% CONS should return a unique cell when invoked. Also test that
% the left and right parts are set correctly.
(!$TEST CONS (NULL (EQ (CONS (QUOTE A) (QUOTE B)) (QUOTE (A . B)))) 
     (EQ (CAR (CONS (QUOTE A) (QUOTE B))) (QUOTE A)) 
     (EQ (CDR (CONS (QUOTE A) (QUOTE B))) (QUOTE B)) ) 
 
% Veryify that RPLACA doesn't modify the binding of a list, and
% that only the CAR part of the cell is affected.
(!$TEST RPLACA 
  (SET (QUOTE TEMP) (QUOTE (A))) 
  (EQ (RPLACA TEMP 1) TEMP) 
  (EQ (CAR (RPLACA TEMP (QUOTE B))) (QUOTE B))  
  (EQ (CDR TEMP) NIL) )
 
(!$TEST RPLACD 
  (SET (QUOTE TEMP) (QUOTE (A . B))) 
  (EQ (RPLACD TEMP (QUOTE A)) TEMP) 
  (EQ (CDR (RPLACD TEMP (QUOTE C))) (QUOTE C))  
  (EQ (CAR TEMP) (QUOTE A)) )
 
% -----3.3 Identifiers-----%  
% Verify that COMPRESS handles the various types of lexemes
% correctly.
(!$TEST COMPRESS 
  (NULL (EQ (COMPRESS (QUOTE (A B))) (COMPRESS (QUOTE (A B))))) 
  (EQN (COMPRESS (QUOTE (!1 !2))) 12) 
  (EQN (COMPRESS (QUOTE (!+ !1 !2))) 12) 
  (EQN (COMPRESS (QUOTE (!- !1 !2))) -12) 
  (EQUAL (COMPRESS (QUOTE ( S T R I N G ))) 'STRING) 
  (EQ (INTERN (COMPRESS (QUOTE (A B)))) (QUOTE AB))   
  (EQ (INTERN (COMPRESS (QUOTE (!! !$ A)))) (QUOTE !$A)) )
 
% Verify that EXPLODE returns the expected lists and that COMPRESS
% and explode are inverses of each other.
(!$TEST EXPLODE 
  (EQUAL (EXPLODE 12) (QUOTE (!1 !2))) 
  (EQUAL (EXPLODE -12) (QUOTE (!- !1 !2))) 
  (EQUAL (EXPLODE 'STRING) (QUOTE ( S T R I N G ))) 
  (EQUAL (EXPLODE (QUOTE AB)) (QUOTE (A B)) ) 
  (EQUAL (EXPLODE (QUOTE !$AB)) (QUOTE (!! !$ A B)))   
  (EQUAL (COMPRESS (EXPLODE 12)) 12)
  (EQUAL (COMPRESS (EXPLODE -12)) -12)
  (EQUAL (COMPRESS (EXPLODE 'STRING)) 'STRING)
  (EQ (INTERN (COMPRESS (EXPLODE (QUOTE AB)))) (QUOTE AB))
  (EQ (INTERN (COMPRESS (EXPLODE (QUOTE !$AB)))) (QUOTE !$AB)) )
 
% Test that GENSYM returns identifiers and that they are different.
(!$TEST GENSYM 
  (IDP (GENSYM)) 
  (NULL (EQ (GENSYM) (GENSYM))) ) 
 
% Test that INTERN works on strings to produce identifiers the same
% as those read in. Try ID's with special characters in them (more
% will be tested with READ).
(!$TEST INTERN 
  (EQ (INTERN 'A) (QUOTE A)) 
  (EQ (INTERN 'A12) (QUOTE A12))
  (EQ (INTERN 'A!*) (QUOTE A!*))
  (NULL (EQ (INTERN 'A) (INTERN 'B))) ) 
 
% Just test that REMOB returns the ID removed.
(!$TEST REMOB 
  (EQ (REMOB (QUOTE AAAA)) (QUOTE AAAA)) ) 
 
% ----- 3.4 Property List Functions-----%  
% Test that FLAG always returns NIL. More testing is done in FLAGP.
(!$TEST FLAG 
  (NULL (FLAG NIL (QUOTE W))) 
  (NULL (FLAG (QUOTE (U V T NIL)) (QUOTE X))) 
  (NULL (FLAG (QUOTE (U)) NIL)) ) 
 
% Test that FLAG worked only on a list. Test all items in a flagged
% list were flagged and that those that weren't aren't.
(!$TEST FLAGP 
  (NULL (FLAGP NIL (QUOTE W))) 
  (FLAGP (QUOTE U) (QUOTE X)) 
  (FLAGP (QUOTE V) (QUOTE X)) 
  (FLAGP T (QUOTE X)) 
  (FLAGP NIL (QUOTE X)) 
  (FLAGP (QUOTE U) NIL) ) 
 
% Test that REMFLAG always returns NIL and that flags removed are
% gone. Test that unremoved flags are still present.
(!$TEST REMFLAG 
  (NULL (REMFLAG NIL (QUOTE X))) 
  (NULL (REMFLAG (QUOTE (U T NIL)) (QUOTE X))) 
  (NULL (FLAGP (QUOTE U) (QUOTE X))) 
  (FLAGP (QUOTE V) (QUOTE X)) 
  (NULL (FLAGP T (QUOTE X))) 
  (NULL (FLAGP NIL (QUOTE X))) ) 
 
(!$TEST PUT 
  (EQ (PUT (QUOTE U) (QUOTE IND1) (QUOTE PROP)) (QUOTE PROP)) 
  (EQN (PUT (QUOTE U) (QUOTE IND2) 0) 0) 
  (EQ (PUT (QUOTE U) (QUOTE IND3) !$VECTOR) !$VECTOR) 
  (EQ (PUT (QUOTE U) (QUOTE IND4) !$CODE) !$CODE) ) 
 
(!$TEST GET 
  (EQ (GET (QUOTE U) (QUOTE IND1)) (QUOTE PROP)) 
  (EQN (GET (QUOTE U) (QUOTE IND2)) 0) 
  (EQ (GET (QUOTE U) (QUOTE IND3)) !$VECTOR) 
  (EQ (GET (QUOTE U) (QUOTE IND4)) !$CODE) ) 
 
(!$TEST REMPROP 
  (NULL (REMPROP !$CODE !$CODE)) 
  (EQ (REMPROP (QUOTE U) (QUOTE IND1)) (QUOTE PROP)) 
  (NULL (GET (QUOTE U) (QUOTE IND1))) 
  (EQN (REMPROP (QUOTE U) (QUOTE IND2)) (QUOTE 0)) 
  (NULL (GET (QUOTE U) (QUOTE IND2))) 
  (EQ (REMPROP (QUOTE U) (QUOTE IND3)) !$VECTOR) 
  (NULL (GET (QUOTE U) (QUOTE IND3))) 
  (GET (QUOTE U) (QUOTE IND4)) 
  (EQ (REMPROP (QUOTE U) (QUOTE IND4)) !$CODE) 
  (NULL (GET (QUOTE U) (QUOTE IND4)))  ) 
 
 
% -----3.5 Function Definition-----% 
(!$TEST DE 
        (EQ (DE FIE (X) (PLUS2 X 1)) (QUOTE FIE))
        (GETD (QUOTE FIE))
        (EQN (FIE 1) 2)
)
% Expect (FIE 1) to return 2% 
(FIE 1)
% Expect FIE redefined in DF test% 
(!$TEST DF 
        (EQ (DF FIE (X) (PROGN (PRINT X) (CAR X))) (QUOTE FIE))
        (GETD (QUOTE FIE))
        (EQN (FIE 1) 1)
        (EQN (FIE 2 3) 2)
)
% Expect (FIE 1) to return 1, and print (1)% 
(FIE 1)
% Expect (FIE 1 2) to return 1, and print (1 2)% 
(FIE 1 2)
% Expect FIE redefined in DM% 
(!$TEST DM 
        (EQ (DM FIE (X) 
             (LIST (QUOTE LIST) 
                              (LIST (QUOTE QUOTE)  X)
                              (LIST (QUOTE QUOTE)  X) )) 
          (QUOTE FIE))
        (GETD (QUOTE FIE))
        (EQUAL (FIE 1) (QUOTE ((FIE 1) (FIE 1))))
)
% Expect (FIE 1) to return ((FIE 1) (FIE 1))% 
(FIE 1)
(!$TEST GETD 
        (PAIRP (GETD (QUOTE FIE)))
        (NULL (PAIRP (GETD (QUOTE FIEFIEFIE))))
        (EQ (CAR (GETD (QUOTE FIE))) (QUOTE MACRO))
)

(!$TEST PUTD 
        (GLOBALP (QUOTE FIE))
 )
% Should check that a FLUID variable not PUTDable;
(!$TEST REMD 
        (PAIRP (REMD (QUOTE FIE)))
        (NULL (GETD (QUOTE FIE)))
             (NULL (REMD (QUOTE FIE)))
             (NULL (REMD (QUOTE FIEFIEFIE)))
)
% -----3.6 Variables and Bindings------% 
%  Make FLUIDVAR1 and FLUIDVAR2 fluids% 
(FLUID (QUOTE (FLUIDVAR1 FLUIDVAR2)))
% Check that FLUIDVAR1 and FLUIDVAR2 are fluid,expect T, T% 
(FLUIDP (QUOTE FLUIDVAR1))
(FLUIDP (QUOTE FLUIDVAR2))
% Give FLUIDVAR1 and FLUIDVAR2 initial values% 
(SETQ FLUIDVAR1 1)
(SETQ FLUIDVAR2 2)

(!$TEST 'FLUID! and! FLUIDP
        (NULL (FLUID (QUOTE (FLUIDVAR3 FLUIDVAR1 FLUIDVAR2 FLUIDVAR4))))
        (FLUIDP (QUOTE FLUIDVAR3))
        (FLUIDP (QUOTE FLUIDVAR1))
        (FLUIDP (QUOTE FLUIDVAR2))
        (FLUIDP (QUOTE FLUIDVAR4))
        (NULL (GLOBALP (QUOTE FLUIDVAR3)))
        (NULL (GLOBALP (QUOTE FLUIDVAR1)))
        (NULL FLUIDVAR3)
        (EQN FLUIDVAR1 1)
        (NULL (FLUIDP (QUOTE CAR)))
)
(GLOBAL (QUOTE (FLUIDGLOBAL1)))
% Expect ERROR that FLUIDGLOBAL1 already FLUID% 
(FLUID (QUOTE (FLUIDGLOBAL2)))

% Expect ERROR that cant change FLUID% 
(GLOBAL (QUOTE (FLUIDVAR1 FLUIDVAR2 GLOBALVAR1 GLOBALVAR2)))
% Does error cause GLOBALVAR1, GLOBALVAR2 to be declared ;

(!$TEST 'GLOBAL! and! GLOBALP
        (NULL (GLOBAL (QUOTE (GLOBALVAR1 GLOBALVAR2))))
        (GLOBALP (QUOTE GLOBALVAR1))
        (GLOBALP (QUOTE GLOBALVAR2))
        (NULL (GLOBALP (QUOTE FLUIDVAR1)))
        (FLUIDP (QUOTE FLUIDVAR1))
        (NULL (FLUIDP (QUOTE GLOBALVAR1)))
        (NULL (FLUIDP (QUOTE GLOBALVAR2)))
        (GLOBALP (QUOTE CAR))
)

% Set SETVAR1 to have an ID value% 
(SET (QUOTE SETVAR1) (QUOTE SETVAR2))

% Expect SETVAR3 to be declared FLUID% 
(!$TEST SET
        (NULL (FLUIDP (QUOTE SETVAR3)))
        (EQN 3 (SET (QUOTE SETVAR3) 3))
        (EQN 3 SETVAR3)
        (FLUIDP (QUOTE SETVAR3))
        (EQN (SET SETVAR1 4) 4)
        (NULL (EQN SETVAR1 4))
        (EQ SETVAR1 (QUOTE SETVAR2))
        (EQN SETVAR2 4)
)
% Expect ERROR if try to set non ID% 
(SET 1 2)
(SET (QUOTE SETVAR1) 1)
(SET SETVAR1 2)

% Expect ERROR if try to SET T or NIL% 
(SET (QUOTE SAVENIL) NIL)
(SET (QUOTE SAVET) T)
(!$TEST 'Special! SET! value
        (SET (QUOTE NIL) 1)
        (NULL (EQN NIL 1))
        (SET (QUOTE NIL) SAVENIL)
        (SET (QUOTE T) 2)
        (NULL (EQN T 2))
        (SET (QUOTE T) SAVET)
)


% Expect SETVAR3 to be declared FLUID% 
(!$TEST SETQ
        (NULL (FLUIDP (QUOTE SETVAR3)))
        (EQN 3 (SETQ SETVAR3 3))
        (EQN 3 SETVAR3)
        (FLUIDP (QUOTE SETVAR3))
)

% Expect ERROR if try to SETQ T or NIL% 
(SET (QUOTE SAVENIL) NIL)
(SET (QUOTE SAVET) T)
(!$TEST 'Special! SETQ! value
        (SETQ NIL 1)
        (NULL (EQN NIL 1))
        (SETQ NIL SAVENIL)
        (SETQ T 2)
        (NULL (EQN T 2))
        (SETQ T SAVET)
)

(!$TEST UNFLUID
        (GLOBALP (QUOTE GLOBALVAR1))
        (FLUIDP  (QUOTE FLUIDVAR1))
        (NULL (UNFLUID (QUOTE (GLOBALVAR1 FLUIDVAR1))))
        (GLOBALP (QUOTE GLOBALVAR1))
        (NULL (FLUIDP (QUOTE FLUIDVAR1)))
)


% ----- 3.7 Program Feature Functions -----% 

% These have been tested as part of BASIC tests;

% Check exact GO and RETURN scoping rules ;

% ----- 3.8 Error Handling -----% 

(!$TEST EMSG!* (GLOBALP (QUOTE EMSG!*)))

(!$TEST ERRORSET
        (EQUAL (ERRORSET 1 T T) (QUOTE (1)))
        (NULL (PAIRP (ERRORSET (QUOTE (CAR 1)) T T)))
)

% Display ERRORSET range of messages and features% 

% First with primitive (CAR 1) error% 

(SETQ ERRORVAR1 (QUOTE (CAR 1)))

%  Expect MSG and BACKTRACE % 
(ERRORSET ERRORVAR1 T T)
(PRINT (LIST (QUOTE EMSG!*) EMSG!*))
%  Expect MSG, no backtrace % 
(ERRORSET ERRORVAR1 T NIL)
(PRINT (LIST (QUOTE EMSG!*) EMSG!*))
%  Expect no MSG, but BACKTRACE % 
(ERRORSET ERRORVAR1 NIL T)
(PRINT (LIST (QUOTE EMSG!*) EMSG!*))
% Expect neither MSG nor Backtrace% 
(ERRORSET ERRORVAR1 NIL NIL)
(PRINT (LIST (QUOTE EMSG!*) EMSG!*))

% Test with CALL on ERROR, with num=789, (A MESSAGE)% 

(SETQ ERRORVAR2 (QUOTE (ERROR 789 (LIST (QUOTE A) (QUOTE MESSAGE)))))
%  Expect MSG and BACKTRACE % 
(ERRORSET ERRORVAR2 T T)
(PRINT (LIST (QUOTE EMSG!*) EMSG!*))
%  Expect MSG, no backtrace % 
(ERRORSET ERRORVAR2 T NIL)
(PRINT (LIST (QUOTE EMSG!*) EMSG!*))
%  Expect no MSG, but BACKTRACE % 
(ERRORSET ERRORVAR2 NIL T)
(PRINT (LIST (QUOTE EMSG!*) EMSG!*))
% Expect neither MSG nor Backtrace% 
(ERRORSET ERRORVAR2 NIL NIL)
(PRINT (LIST (QUOTE EMSG!*) EMSG!*))

% Test of Rebinding/Unbinding% 

(FLUID (QUOTE (ERRORVAR3 ERRORVAR4)))
(SETQ ERRORVAR3 3)
(SETQ ERRORVAR4 4)

(DE ERRORFN1 (X ERRORVAR3)
  (PROGN (PRINT (LIST (QUOTE ERRORVAR3) ERRORVAR3))
         (SETQ ERRORVAR3 33)
  (PROG (Y ERRORVAR4)
        (PRINT (LIST (QUOTE ERRORVAR3) ERRORVAR3))
        (PRINT (LIST (QUOTE ERRORVAR4) ERRORVAR4))
        (SETQ ERRORVAR3 333)
        (SETQ ERRORVAR4 444)
        (ERROR 555 'Error! Inside! ERRORFN1)
          (RETURN 'Error! Failed))))

% Expect to see 3333 33 44 printed% 
% Followed by ERROR 555 messgae% 
(ERRORSET (QUOTE (ERRORFN1 3333 4444)) T T)
% Expect 3 and 4 as Final values of ERRORVAR3 and ERRORVAR4% 
ERRORVAR3
ERRORVAR4
(!$TEST ERRORVARS
        (EQN ERRORVAR3 3)
        (EQN ERRORVAR4 4)
)
% ----- 3.9 Vectors -----% 
%  Create a few variables that may be vectors % 
(SETQ VECTVAR1 NIL)
(SETQ VECTVAR2 (QUOTE (VECTOR 1 2 3)))
(SETQ VECTVAR3 (QUOTE [1 2 3 4]))

% Expect Type mismatch Error for next 2% 
(GETV VECTVAR1 1)
(GETV VECTVAR2 1)
% Expect 1 2 for next 2% 
(GETV VECTVAR3 0)
(GETV VECTVAR3 1)
% Expect Index error for next 2% 
(GETV VECVAR3 -1)
(GETV VECTVAR3 4)
        

(!$TEST MKVECT
        (VECTORP (SETQ VECTVAR3 (MKVECT 5)))
        (EQN 5 (UPBV VECTVAR3))
        (NULL (GETV VECTVAR3 0))
        (NULL (GETV VECTVAR3 5))
        (EQN 10 (PUTV VECTVAR3 0 10))
        (EQN 10 (GETV VECTVAR3 0))
        (EQN 20 (PUTV VECTVAR3 5 20))
        (EQN 20 (GETV VECTVAR3 5))
)
%  Expect VECTVAR3 to be [ 10 nil nil nil nil 20 ]% 
(PRINT VECTVAR3)

% Expect MKVECT error for index less than 0% 
(MKVECT -1)
% Expect length 1 vector% 
(MKVECT 0)
% Expect type error% 
(MKVECT NIL)
% Expect 2  TYPE  errors% 
(PUTV VECTVAR1 0 1)
(PUTV VECTVAR1 -1 1)

(!$TEST UPBV
        (NULL (UPBV VECTVAR1))
        (EQN (UPBV VECTVAR3 5) 5 )
)
% ----- 3.10 Booleans and Conditionals -----% 
(!$TEST AND
        (EQ T (AND))
        (EQ T (AND T))
        (EQ T (AND T T))
        (EQN 1 (AND T 1))
        (EQ T (AND 1 T))
        (EQ T (AND T T 1 1 T T))
        (NULL (AND NIL))
        (NULL (AND T NIL))
        (NULL (AND NIL T))
        (NULL (AND T T T T NIL T T))
)
% The next should not ERROR, else AND is evaluating all args% 
(AND T T NIL (ERROR 310 'AND! Failed) T)

(!$TEST COND
        (EQN 1 (COND (T 1)))
        (NULL (COND))
        (NULL (COND (NIL 1)))
        (EQN 1 (COND (T 1) (T 2)))
        (EQN 2 (COND (NIL 1) (T 2)))
        (NULL  (COND (NIL 1) (NIL 2)))
)
% Test COND with GO and RETURN% 
(PROG NIL
        (COND (T (GO L1)))
        (ERROR 310 'COND! fell! through)
 L1        (PRINT 'GO! in! cond! worked)
        (COND (T (RETURN (PRINT 'Return! 2))))
        (ERROR 310 'COND! did! not! RETURN)
)
% Certain Extensions to COND might fail% 
%/(COND 1 2)
%/(COND (T))
%/(COND (T 1 2 3))

(!$TEST NOT
        (NULL (NOT T))
        (EQ T (NOT NIL))
)

(!$TEST OR
        (NULL (OR))
        (EQ T (OR T))
        (EQ T (OR T T))
        (EQN T (OR T 1))
        (EQ 1 (OR 1 T))
        (EQ T (OR T T 1 1 T T))
        (NULL (OR NIL))
        (EQ T (OR T NIL))
        (EQ T (OR NIL T))
        (EQ T (OR T T T T NIL T T))
)
% The next should not ERROR, else OR is evaluating all args% 
(OR T NIL NIL (ERROR 310 'OR! Failed) T)

% -----3.11 Arithmetic Functions-----% 
% Setup some ints% 
% Setup some floats% 
(SETQ FZERO (FLOAT 0))
(SETQ FONE (FLOAT 1))
(SETQ FTWO (FLOAT 2))
(SETQ FTHREE (FLOAT 3))
(!$TEST ABS
        (EQN 0 (ABS 0))
        (EQN 1 (ABS 1))
        (EQN 1 (ABS -1))
        (EQN FZERO (ABS FZERO))
        (EQN FONE (ABS FONE))
        (EQN FONE (ABS (MINUS FONE)))
)

(!$TEST ADD1
        (EQN 1 (ADD1 0))
        (EQN 0 (ADD1 -1))
        (EQN 2 (ADD1 1))
        (EQN FONE (ADD1 FZERO))
        (EQN FTWO (ADD1 FONE))
)

(!$TEST DIFFERENCE
        (EQN 0 (DIFFERENCE 1 1))
        (EQN FZERO (DIFFERENCE FONE FONE))
        (EQN FZERO (DIFFERENCE 1 FONE))
        (EQN FZERO (DIFFERENCE FONE 1))
        (EQN 1 (DIFFERENCE 2 1))
        (EQN -1 (DIFFERENCE 1 2))
)

(!$TEST DIVIDE
        (EQUAL (CONS 1 2) (DIVIDE 7 5))
        (EQUAL (CONS -1 -2) (DIVIDE -7 5))
        (EQUAL (CONS -1 2) (DIVIDE 7 -5))
        (EQUAL (CONS 1 -2) (DIVIDE -7 -5))
)
(!$TEST EXPT
        (EQN (EXPT 2 0) 1)
        (EQN (EXPT 2 1) 2)
        (EQN (EXPT 2 2) 4)
        (EQN (EXPT 2 3) 8)
        (EQN (EXPT -2 2) 4)
        (EQN (EXPT -2 3) -8)
)

(!$TEST FIX
        (NUMBERP (FIX FONE))
        (FIXP (FIX FONE))
        (NULL (FLOATP (FIX FONE)))
        (EQN (FIX FONE ) 1)
        (NUMBERP (FIX 1))
        (FIXP (FIX 1))
)

(!$TEST FLOAT
        (NUMBERP (FLOAT 1))
        (FLOATP (FLOAT 1))
        (NULL (FIXP (FLOAT 1)))
        (EQN FONE (FLOAT 1))
)

(!$TEST GREATERP
        (GREATERP 2 1)
        (GREATERP 1 0)
        (GREATERP 0 -1)
        (NULL (GREATERP 2 2))
        (NULL (GREATERP 1 1))
        (NULL (GREATERP 0 0))
        (NULL (GREATERP 0 1))
        (NULL (GREATERP -1 0))
)
(!$TEST LESSP
        (NULL (LESSP 2 1))
        (NULL (LESSP 1 0))
        (NULL (LESSP 0 -1))
        (NULL (LESSP 2 2))
        (NULL (LESSP 1 1))
        (NULL (LESSP 0 0))
        (LESSP 0 1)
        (LESSP -1 0)
)
(!$TEST MAX
        (EQN (MAX 1 2 3) 3)
        (EQN (MAX 3 2 1) 3)
        (EQN 1 (MAX 1 0))
        (EQN 1 (MAX 1))
)
% What is (MAX) ;

(!$TEST MAX2
        (EQN (MAX2 1 2) 2)
        (EQN (MAX2 2 1) 2)
        (EQN 1 (MAX2 1 0))
        (EQN -1 (MAX2 -1 -2))
)
(!$TEST MIN
        (EQN (MIN 1 2 3) 1)
        (EQN (MIN 3 2 1) 1)
        (EQN 0 (MIN 1 0))
        (EQN 1 (MIN 1))
)
% What is (MIN) ;

(!$TEST MIN2
        (EQN (MIN2 1 2) 1)
        (EQN (MIN2 2 1) 1)
        (EQN 0 (MIN2 1 0))
        (EQN 0 (MIN2 0 1))
        (EQN -2 (MIN2 -1 -2))
)
(!$TEST MINUS
        (EQN 0 (MINUS 0))
        (EQN -1 (MINUS 1))
        (MINUSP (MINUS 1))
        (MINUSP -1)
        (LESSP -1 0)
        (EQN 1 (MINUS -1))
)

(!$TEST PLUS
        (EQN 6 (PLUS 1 2 3))
        (EQN 10 (PLUS 1 2 3 4))
        (EQN 0 (PLUS 1 2 3 -6))
        (EQN 3 (PLUS 1 2))
        (EQN 1 (PLUS 1))
)
% What is (PLUS) ;

(!$TEST PLUS2
        (EQN 3 (PLUS2 1 2))
        (EQN 0 (PLUS2 1 -1))
        (EQN 1 (PLUS2 -2 3))
)

(!$TEST QUOTIENT
        (EQN 1 (QUOTIENT 3 3))
        (EQN 1 (QUOTIENT 4 3))
        (EQN 1 (QUOTIENT 5 3))
        (EQN 2 (QUOTIENT 6 3))
        (EQN -1 (QUOTIENT -3 3))
        (EQN -1 (QUOTIENT 3 -3))
        (EQN -1 (QUOTIENT 4 -3))
        (EQN -1 (QUOTIENT -4 3))
)

% Expect 2 ZERO DIVISOR error messages% 
(QUOTIENT 1 0)
(QUOTIENT 0 0)

(!$TEST REMAINDER
        (EQN 0 (REMAINDER 3 3))
        (EQN 1 (REMAINDER 4 3))
        (EQN 2 (REMAINDER 5 3))
        (EQN 0 (REMAINDER 6 3))
        (EQN 0 (REMAINDER -3 3))
        (EQN 0 (REMAINDER 3 -3))
        (EQN -1 (REMAINDER 4 -3))
        (EQN -1 (REMAINDER -4 3))
)

% Expect 2 ZERO DIVISOR  error messages% 
%(REMAINDER 1 0)
%(REMAINDER 0 0)

(!$TEST SUB1
        (EQN 1 (SUB1 2))
        (EQN 0 (SUB1 1))
        (EQN -1 (SUB1 0))
)

(!$TEST TIMES
        (EQN 6 (TIMES 1 2 3))
        (EQN 1 (TIMES 1))
        (EQN 2 (TIMES 1 2))
)
% What is (TIMES) ;

(!$TEST TIMES2
        (EQN 0 (TIMES2 1 0))
        (EQN 0 (TIMES2 0 1))
        (EQN 10 (TIMES2 5 2))
        (EQN -10 (TIMES2 5 -2))
)

% -----3.12 MAP composite functions ------% 

(SETQ LST (QUOTE (1 2 3)))
(DE LISTX (X) (LIST X (QUOTE X)))
(DE PRNTX (X) (PRINT (LISTX X)))

% MAP: Expect 3 lines of output, equivalent to:% 
% ((1 2 3) X)% 
% ((2 3) X)% 
% ((3) X)% 
(!$TEST MAP (NULL (MAP LST (FUNCTION PRNTX))))

% MAPC:          Expect 3 lines of output, equivalent to:% 
% (1 X)% 
% (2 X)% 
% (3 X)% 
(!$TEST MAPC (NULL (MAPC LST (FUNCTION PRNTX))))

% MAPCAN:  Expect 3 lines of output, equivalent to:% 
% (1 X)% 
% (2 X)% 
% (3 X)% 
(!$TEST MAPCAN 
        (EQUAL (MAPCAN LST (FUNCTION PRNTX))
                (QUOTE (1 X 2 X 3 X)))
)

% MAPCAR:  Expect 3 Lines of output, equivalent to:% 
% (1 X)% 
% (2 X)% 
% (3 X)% 
(!$TEST MAPCAR
        (EQUAL        (MAPCAR LST (FUNCTION PRNTX))
                (QUOTE ((1 X) (2 X) (3 X))))
)

% MAPCON:  Expect 3 lines of output, equivalent to:% 
% ((1 2 3) X)% 
% ((2 3) X)% 
% ((3) X)% 
(!$TEST MAPCON
        (EQUAL         (MAPCON LST (FUNCTION PRNTX))
        (QUOTE ((1 2 3) X (2 3) X (3) X)))
)

% MAPLIST: Expect 3 lines of output, equivalent to:% 
% ((1 2 3) X)% 
% ((2 3) X)% 
% ((3) X)% 

(!$TEST MAPLIST
        (EQUAL        (MAPLIST LST (FUNCTION PRNTX))
                (QUOTE (((1 2 3) X) ((2 3) X) ((3) X))))
)

% ----- 3 . 13 Composite Functions -----% 
(SETQ APPVAR1 (QUOTE (1 2 3)))

(!$TEST APPEND
        (NULL (APPEND NIL NIL))
        (EQUAL APPVAR1 (SETQ APPVAR2 (APPEND APPVAR1 NIL)))
        (NULL (EQ APPVAR1 APPVAR2))
        (EQUAL APPVAR1 (SETQ APPVAR2 (APPEND NIL APPVAR1)))
        (EQ APPVAR1 APPVAR2)
        (EQUAL APPVAR1 (APPEND (QUOTE (1)) (QUOTE (2 3))))
        (EQUAL APPVAR1 (APPEND (QUOTE (1 2)) (QUOTE (3))))
)

(SETQ ASSVAR 
   (QUOTE ( ((1 . 1) . ONE) ((2 . 2) . TWO) ((3 . 3) . THREE) ) ) )
(!$TEST ASSOC
        (NULL (ASSOC NIL NIL))
        (NULL (ASSOC 1 NIL))
        (NULL (ASSOC 1 ASSVAR))
        (EQUAL (QUOTE ((1 . 1) . ONE)) (ASSOC (QUOTE (1 . 1)) ASSVAR))
        (EQUAL (QUOTE ((2 . 2) . TWO)) (ASSOC (QUOTE (2 . 2)) ASSVAR))
)
% Expect Error MSG on poor ALIST% 
%(ASSOC (QUOTE (1)) (QUOTE (1 2 3)))

(SETQ DLIST (QUOTE ((AA BB) (EE FF))))

(!$TEST DEFLIST
        (EQUAL (QUOTE (AA EE)) (DEFLIST DLIST (QUOTE DEFLIST)))
        (EQ (QUOTE BB) (GET (QUOTE AA) (QUOTE DEFLIST)))
        (EQ (QUOTE FF) (GET (QUOTE EE) (QUOTE DEFLIST)))
)

(!$TEST DELETE
        (EQUAL (QUOTE ((1 . 1) (2 . 2))) 
               (DELETE (QUOTE (0 . 0)) (QUOTE ((0 . 0) (1 . 1) (2 . 2)))))
        (EQUAL (QUOTE ((0 . 0) (2 . 2))) 
               (DELETE (QUOTE (1 . 1)) (QUOTE ((0 . 0) (1 . 1) (2 . 2)))))
        (EQUAL (QUOTE ((0 . 0) (2 . 2) (1 . 1))) 
               (DELETE (QUOTE (1 . 1)) 
                        (QUOTE ((0 . 0) (1 . 1) (2 . 2) (1 . 1)))))
)

% remove the comments when digit and liter are added.

%(SETQ DIGITLST (QUOTE (!0 !1 !2 !3 !4 !5 !6 !7 !8 !9)))

%(DE TESTEACH (LST FN)
%        (PROG (X)
%         L1     (while t (progn
%            (COND ((NULL (PAIRP LST)) (RETURN T)))
%                (SETQ X (APPLY FN (LIST (CAR LST))))  % Not (FN (CAR LST)) ?
%                (COND ((NULL X) 
%                 (PRINT (LIST '!*!*!*! TESTEACH (CAR LST) 'failed))))
%                (SETQ LST (CDR LST))
%                (GO L1)))))
%
%(!$TEST DIGIT
%        (TESTEACH DIGITLST (FUNCTION DIGIT))
%        (NULL (DIGIT 1))
%        (NULL (DIGIT (QUOTE A)))
%        (NULL (DIGIT '1))
%)

(!$TEST LENGTH
        (EQN 0 (LENGTH (QUOTE A)))
        (EQN 0 (LENGTH 1))
        (EQN 1 (LENGTH (QUOTE (A))))
        (EQN 1 (LENGTH (QUOTE (A . B))))
        (EQN 2 (LENGTH (QUOTE (A B))))
)

%(SETQ UPVAR 
% (QUOTE (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)))
%(SETQ DNVAR
% (QUOTE (a b c d e f g h i j k l m n o p q r s t u v w x y z)))
%
%(!$TEST LITER
%        (TESTEACH UPVAR (FUNCTION LITER))
%        (TESTEACH DNVAR (FUNCTION LITER))
%        (NULL (LITER 'A))
%        (NULL (LITER 1))
%        (NULL (LITER (QUOTE AA)))
%)

(SETQ MEMBVAR (QUOTE ((1 . 1) ( 2 . 2) (3 . 3))))

(!$TEST MEMBER
        (NULL (MEMBER NIL NIL))
        (NULL (MEMBER NIL MEMBVAR))
        (NULL (MEMBER (QUOTE (4 . 4)) MEMBVAR))
        (EQ (CDR MEMBVAR) (MEMBER (QUOTE (2 . 2)) MEMBVAR))
)

(!$TEST MEMQ
        (NULL (MEMQ NIL NIL))
        (EQ MEMBVAR (MEMQ (CAR MEMBVAR) MEMBVAR))
        (NULL (MEMQ (QUOTE (1 . 1)) MEMBVAR))
        (EQ (CDR MEMBVAR) (MEMQ (CADR MEMBVAR) MEMBVAR))
)


(SETQ NCONCVAR1 (LIST 1 2 3))

(!$TEST NCONC
        (EQUAL (QUOTE ( 1 2 3 4 5)) 
         (SETQ NCONCVAR2 (NCONC NCONCVAR1 (QUOTE ( 4 5)))))
        (EQ NCONCVAR1 NCONCVAR2)
        (EQUAL NCONCVAR1 (QUOTE (1 2 3 4 5)))
)

(!$TEST PAIR
        (EQUAL NIL (PAIR NIL NIL))
        (EQUAL (QUOTE ((1 . ONE) (2 . TWO))) 
            (PAIR (QUOTE (1 2)) (QUOTE (ONE TWO))))
)

% expect 2 PAIR mismatch errors% 

%(PAIR (QUOTE (1)) (QUOTE ( ONE TWO)))
%(PAIR (QUOTE (1)) NIL)

(!$TEST REVERSE
        (NULL (REVERSE NIL))
        (EQUAL (QUOTE (1)) (REVERSE (QUOTE (1))))
        (EQUAL (QUOTE (1 2 3)) (REVERSE (QUOTE (3 2 1))))
        (EQUAL (QUOTE ((1 . 2) (2 . 3) (3 4 5)))
           (REVERSE (QUOTE ((3 4 5) (2 . 3) (1 . 2)))))
)

(DE SASSFN NIL
        (PROG2 (PRINT 'Sassfn! Called) 99))

(SETQ SASSVAR (QUOTE ((1 . ONE) (2 . TWO))))

(!$TEST SASSOC
        (EQN 99 (SASSOC NIL NIL (FUNCTION SASSFN)))
        (EQN 99 (SASSOC NIL SASSVAR (FUNCTION SASSFN)))
        (EQUAL (QUOTE (2 . TWO))
                (SASSOC 2 SASSVAR (FUNCTION SASSFN)))
)

% Expect ERROR for poor alist:
%(SASSOC (QUOTE A) (QUOTE (B (A . 1))) (FUNCTION SASSFN))

% Set up SUBLIS values
(SETQ SUBLVAR1 (QUOTE ((X . 1) ((X . X) . 2))))
(SETQ SUBLVAR2 (QUOTE (X X (X . 1) (X . X) ((X . X)))))
(SETQ SUBLVAR3 (QUOTE (1 1 (1 . 1) 2 (2))))

%(!$TEST SUBLIS
%        (NULL (SUBLIS NIL NIL))
%        (EQN 1 (SUBLIS NIL 1))
%        (EQ SUBLVAR2 (SUBLIS NIL SUBLVAR2))
%        (EQUAL SUBLVAR2 (SUBLIS NIL SUBLVAR2))
%        (EQ SUBLVAR2 (SUBLIS (QUOTE ((Y . 3))) SUBLVAR2))
%% Will fail, but nice opt if no action;
%        (EQUAL SUBLVAR2 (SUBLIS (QUOTE ((Y . 3))) SUBLVAR2))
%        (EQUAL SUBLVAR3 (SUBLIS SUBLVAR1 SUBLVAR2))
%)
%
(!$TEST SUBST
        (NULL (SUBST NIL 1 NIL))
        (EQ (QUOTE A) (SUBST NIL 1 (QUOTE A)))
        (EQN 1 (SUBST  1 2 2))
        (EQUAL (CONS 2 2) (SUBST 2 1 (CONS 1 1)))
        (EQUAL (QUOTE (1 1 (1 . 1) (1 . 1) ((1 . 1))))
                (SUBST 1 (QUOTE X) SUBLVAR2))
)
% ----- 3.14 The Interpreter ----% 

% To be done ;

% ----- 3.15 IO -----% 
% ----- 3.16 The Standard LISP Reader ----% 
% To be done ;

% ----- 4.0 Globals ----% 

% To be done ;

% ----- 5.0 Miscellaneous functions -----% 

% to be done ;

(RDS NIL)
