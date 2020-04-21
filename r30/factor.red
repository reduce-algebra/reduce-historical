
% ***********************************************
% ******* The REDUCE Factorization module *******
% ******* A. C. Norman and P. M. A. Moore *******
% ***********************************************;

% This version dated 12 September 1982.  ACN;

% This file should be used with a system dependent file containing
% a setting of the variable LARGEST!-SMALL!-MODULUS.
% If at all possible the integer arithmetic
% operations used here should be mapped onto corresponding ones
% available in the underlying Lisp implementation, and the support
% for modular arithmetic (perhaps based on these integer arithmetic
% operations) should be reviewed. This file provides placeholder
% definitions of functions that are used on some implementations
% to support block compilation, car/cdr access checks and the like.
% The front-end files on the systems that can use these features will
% disable the definitions given here by use of a 'LOSE flag;;


SYMBOLIC;

% MODULE FSUPPORT;  % Support for factorizer;


DEFLIST('((MINUS!-ONE -1)),'NEWNAM);   %so that it EVALs properly;

SYMBOLIC SMACRO PROCEDURE CARCHECK U; NIL;

FLUID '(!*TRFAC FACTOR!-LEVEL FACTOR!-TRACE!-LIST);

SYMBOLIC SMACRO PROCEDURE FACTOR!-TRACE ACTION;
BEGIN SCALAR STREAM;
  IF !*TRFAC AND FACTOR!-LEVEL = 1 THEN
    STREAM := NIL . NIL
  ELSE
    STREAM := ASSOC(FACTOR!-LEVEL,FACTOR!-TRACE!-LIST);
  IF STREAM THEN <<
    STREAM:=WRS CDR STREAM;
    ACTION;
    WRS STREAM >>
 END;

SYMBOLIC SMACRO PROCEDURE GCD(M,N); GCDN(M,N);

SYMBOLIC SMACRO PROCEDURE ILOGAND(M,N); LOGAND2(M,N);

SYMBOLIC SMACRO PROCEDURE ILOGOR(M,N); LOGOR2(M,N);

SYMBOLIC SMACRO PROCEDURE ILOGXOR(M,N); LOGXOR2(M,N);

SYMBOLIC MACRO PROCEDURE LOGAND U; EXPAND(CDR U,'LOGAND2);

SYMBOLIC MACRO PROCEDURE LOGOR U; EXPAND(CDR U,'LOGOR2);

SYMBOLIC MACRO PROCEDURE LOGXOR U; EXPAND(CDR U,'LOGXOR2);

SYMBOLIC SMACRO PROCEDURE IMIN(U,V); MIN(U,V);

SYMBOLIC SMACRO PROCEDURE IRECIP U; 1/U;

SYMBOLIC SMACRO PROCEDURE IRIGHTSHIFT(U,N); LEFTSHIFT(U,-N);

SYMBOLIC SMACRO PROCEDURE ISDOMAIN U; DOMAINP U;

SYMBOLIC SMACRO PROCEDURE MODULE U; NIL;

SYMBOLIC SMACRO PROCEDURE ENDMODULE; NIL;

SYMBOLIC SMACRO PROCEDURE BLKCMP; NIL;

SYMBOLIC SMACRO PROCEDURE EXPORTS U; NIL;

SYMBOLIC SMACRO PROCEDURE IMPORTS U; NIL;

DEFLIST('((MODULE RLIS) (EXPORTS RLIS)
	  (IMPORTS RLIS) (ENDMODULE ENDSTAT)),'STAT);

SYMBOLIC SMACRO PROCEDURE PRINC U; PRIN2 U;

SYMBOLIC SMACRO PROCEDURE PRINTC U; PRIN2T U;

SYMBOLIC SMACRO PROCEDURE READGCTIME; GCTIME();

SYMBOLIC SMACRO PROCEDURE READTIME; TIME()-GCTIME();

SYMBOLIC SMACRO PROCEDURE REVERSEWOC U; REVERSIP U;

SYMBOLIC SMACRO PROCEDURE TTAB N; SPACES(N-POSN());

% Operators for fast arithmetic;

SYMBOLIC MACRO PROCEDURE IPLUS U; EXPAND(CDR U,'PLUS2);

SYMBOLIC MACRO PROCEDURE ITIMES U; EXPAND(CDR U,'TIMES2);

SMACRO PROCEDURE ISUB1 A; A-1;

SMACRO PROCEDURE IADD1 A; A+1;

SMACRO PROCEDURE IMINUS A; -A;

SMACRO PROCEDURE IDIFFERENCE(A,B); A-B;

SMACRO PROCEDURE IQUOTIENT(A,B); A/B;

SMACRO PROCEDURE IREMAINDER(A,B); REMAINDER(A,B);

SMACRO PROCEDURE IGREATERP(A,B); A>B;

SMACRO PROCEDURE ILESSP(A,B); A<B;

SMACRO PROCEDURE IMINUSP A; A<0;

NEWTOK '((!#) HASH);
NEWTOK '((!# !+) IPLUS);
NEWTOK '((!# !-) IDIFFERENCE);
NEWTOK '((!# !*) ITIMES);
NEWTOK '((!# !/) IQUOTIENT);
NEWTOK '((!# !>) IGREATERP);
NEWTOK '((!# !<) ILESSP);

INFIX #+,#-,#*,#/,#>,#<;

PRECEDENCE #+,+;
PRECEDENCE #-,-;
PRECEDENCE #*,*;
PRECEDENCE #/,/;
PRECEDENCE #>,>;
PRECEDENCE #<,<;

FLAG('(IPLUS ITIMES),'NARY);

DEFLIST('((IDIFFERENCE IMINUS)),'UNARY);

DEFLIST('((IMINUS IPLUS)), 'ALT);


SYMBOLIC PROCEDURE MOVED(OLD,NEW);
 << REMD OLD;
    PUTD(OLD,'EXPR,CDR GETD NEW) >>;
    
SMACRO PROCEDURE EVENP A; REMAINDER(A,2)=0;

SMACRO PROCEDURE SUPERPRINT A; PRETTYPRINT A;


%The following number is probably not machine dependent;

GLOBAL '(TWENTYFOURBITS);

TWENTYFOURBITS := 2**24-1;

COMMENT An Exponential Function for Real Numbers;

% The following  definitions  constitute a  simple  floating
% point exponential function.  The argument is normalized to
% the interval -ln  2 to  0, and a  Taylor series  expansion
% used (formula 4.2.45 on page 71 of Abramowitz and  Stegun,
% "Handbook of Mathematical  Functions").  Note that  little
% effort has been expended to minimize truncation errors.

% On many systems it will be appropriate to define a system-
% specific EXP routine that does bother about rounding and that
% understands the precision of the host floating point arithmetic;


SYMBOLIC PROCEDURE CEILING!-FLOAT X;
% Returns the ceiling (fixnum) of its floatnum argument;
  BEGIN SCALAR N;
    N := FIX X;
    RETURN IF X = FLOAT N THEN N ELSE N+1
  END;

GLOBAL '(EXP!-COEFFS NATURAL!-LOG!-2);

EXP!-COEFFS := MKVECT 7;

PUTV(EXP!-COEFFS,0,1.0);
PUTV(EXP!-COEFFS,1,-1.0);
PUTV(EXP!-COEFFS,2,0.49999992);
PUTV(EXP!-COEFFS,3,-0.16666530);
PUTV(EXP!-COEFFS,4,0.41657347E-1);
PUTV(EXP!-COEFFS,5,-0.83013598E-2);
PUTV(EXP!-COEFFS,6,0.13298820E-2);
PUTV(EXP!-COEFFS,7,-0.14131610E-3);

NATURAL!-LOG!-2 := 0.69314718;

SYMBOLIC PROCEDURE EXP X;
% Returns the exponential (ie, e**x) of its floatnum argument as
% a floatnum;
  BEGIN SCALAR N,ANS;
    N := CEILING!-FLOAT(X / NATURAL!-LOG!-2);
    X := N * NATURAL!-LOG!-2 - X;
    ANS := 0.0;
    FOR I := UPBV EXP!-COEFFS STEP -1 UNTIL 0 DO
      ANS := GETV(EXP!-COEFFS,I) + X*ANS;
    RETURN ANS * 2**N
  END;


COMMENT A Random Number Generator;

% The declarations below  constitute a linear,  congruential
% random number generator (see  Knuth, "The Art of  Computer
% Programming: Volume 2: Seminumerical Algorithms", pp9-24).
% With the given  constants it  has a period  of 392931  and
% potency  6.    To   have  deterministic   behaviour,   set
% RANDOM!-SEED.
%
% Constants are:        6  2
%    modulus: 392931 = 3 * 7 * 11
%    multiplier: 232 = 3 * 7 * 11 + 1
%    increment: 65537 is prime;

GLOBAL '(RANDOM!-SEED);

SYMBOLIC PROCEDURE RANDOMIZE();
    RANDOM!-SEED := REMAINDER(TIME(),392931);

RANDOMIZE();


SYMBOLIC PROCEDURE RANDOM;
% Returns a pseudo-random number between 0 and 392931;
    RANDOM!-SEED := REMAINDER(232*RANDOM!-SEED + 65537, 392931);


COMMENT Support for Real Square Roots;

SYMBOLIC PROCEDURE SQRT N;
% return sqrt of n if same is exact, or something non-numeric
% otherwise. Note that only the floating point parts of this
% code get excercised by the factorizer, and that they only
% ever get called with arguments in the range 1 to 10**12;
    IF NOT NUMBERP N THEN 'NONNUMERIC
    ELSE IF N<0 THEN 'NEGATIVE
    ELSE IF FLOATP N THEN SQRT!-FLOAT N
    ELSE IF N<2 THEN N
    ELSE NR(N,(N+1)/2);

SYMBOLIC PROCEDURE NR(N,ROOT);
% root is an overestimate here. nr moves downwards to root.
% In the case of this being called on really big numbers the
% initial approximate used will be bad & the iteration will start
% in effect by halving it until it is reasonable. This could do
% with improvement in any system where big square roots will be
% taken at all often;
  BEGIN
    SCALAR W;
    W:=ROOT*ROOT;
    IF N=W THEN RETURN ROOT;
    W:=(ROOT+N/ROOT)/2;
    IF W>=ROOT THEN RETURN !*P2F MKSP(LIST('SQRT,N),1);
    RETURN NR(N,W)
  END;

GLOBAL '(SQRT!-FLOAT!-TOLERANCE);

SQRT!-FLOAT!-TOLERANCE := 0.00001;

SYMBOLIC PROCEDURE SQRT!-FLOAT N;
% Simple Newton-Raphson floating point square root calculator;
  BEGIN SCALAR SCALE,ANS;
    IF N=0.0 THEN RETURN 0.0
    ELSE IF N<0.0 THEN REDERR "SQRT!-FLOAT GIVEN NEGATIVE ARGUMENT";
    SCALE := 1.0; 
    % Detatch the exponent by doing a sequence of multiplications
    % and divisions by powers of 2 until the remaining number is in
    % the range 1.0 to 4.0. On a binary machine the scaling should
    % not introduce any error at all;
    WHILE N > 256.0 DO <<
      SCALE := SCALE * 16.0;
      N := N/256.0 >>;
    WHILE N < 1.0/256.0 DO <<
      SCALE := SCALE / 16.0;
      N := N*256.0 >>;         % Coarse scaled: now finish off the job;
    WHILE N < 1.0 DO <<
      SCALE := SCALE / 2.0;
      N := N*4.0 >>;
    WHILE N > 4.0 DO <<
      SCALE := SCALE * 2.0;
      N := N/4.0 >>;
    ANS := 2.0;               % 5 iterations get me as good a result
			      % as I can reasonably want & it is cheaper
			      % to do 5 always than to test for stopping
			      % criteria;
    FOR I:=1:5 DO
      ANS := (ANS+N/ANS)/2.0;

    RETURN ANS*SCALE
  END;

COMMENT A Simple Sorting Routine;

SYMBOLIC PROCEDURE SORT(L,FN);
  BEGIN
    SCALAR TREE;
    IF NULL L OR NULL CDR L THEN RETURN L;
    FOR EACH J IN L DO TREE := TREEADD(J,TREE,FN);
    RETURN FLATTREE(TREE,NIL)
  END;

SYMBOLIC PROCEDURE TREEADD(ITEM,TREE,FN);
% add item to a tree, using fn as an order predicate;
    IF NULL TREE THEN ITEM . (NIL . NIL)
    ELSE IF APPLY(FN,LIST(ITEM,CAR TREE)) THEN
        CAR TREE . (TREEADD(ITEM,CADR TREE,FN). CDDR TREE)
    ELSE CAR TREE . (CADR TREE . TREEADD(ITEM,CDDR TREE,FN));

SYMBOLIC PROCEDURE FLATTREE(TREE,L);
    IF NULL TREE THEN L
    ELSE FLATTREE(CADR TREE,CAR TREE . FLATTREE(CDDR TREE,L));


% Modular arithmetic;


FLUID '(CURRENT!-MODULUS MODULUS!/2 
	LARGEST!-SMALL!-MODULUS);

% LARGEST!-SMALL!-MODULUS must be set in the front-end (system
% dependent) file;


SYMBOLIC PROCEDURE SET!-SMALL!-MODULUS P;
  BEGIN
    SCALAR PREVIOUS!-MODULUS;
    IF P>LARGEST!-SMALL!-MODULUS
      THEN ERRORF "Overlarge modulus being used";
    PREVIOUS!-MODULUS:=CURRENT!-MODULUS;
    CURRENT!-MODULUS:=P;
    MODULUS!/2 := P/2;
    RETURN PREVIOUS!-MODULUS
  END;


SMACRO PROCEDURE MODULAR!-PLUS(A,B);
  BEGIN SCALAR RESULT;
     RESULT:=A #+ B;
     IF NOT RESULT #< CURRENT!-MODULUS THEN
	    RESULT:=RESULT #- CURRENT!-MODULUS;
     RETURN RESULT
  END;

SMACRO PROCEDURE MODULAR!-DIFFERENCE(A,B);
  BEGIN SCALAR RESULT;
     RESULT:=A #- B;
     IF IMINUSP RESULT THEN RESULT:=RESULT #+ CURRENT!-MODULUS;
     RETURN RESULT
  END;

SYMBOLIC PROCEDURE MODULAR!-NUMBER A;
  BEGIN
     A:=REMAINDER(A,CURRENT!-MODULUS);
     IF IMINUSP A THEN A:=A #+ CURRENT!-MODULUS;
     RETURN A
  END;

SMACRO PROCEDURE MODULAR!-TIMES(A,B);
    REMAINDER(A*B,CURRENT!-MODULUS);


SMACRO PROCEDURE MODULAR!-RECIPROCAL A;
    RECIPROCAL!-BY!-GCD(CURRENT!-MODULUS,A,0,1);

SYMBOLIC PROCEDURE RECIPROCAL!-BY!-GCD(A,B,X,Y);
%On input A and B should be coprime. This routine then
%finds X and Y such that A*X+B*Y=1, and returns the value Y
%on input A > B;
   IF B=0 THEN ERRORF "INVALID MODULAR DIVISION"
   ELSE IF B=1 THEN IF IMINUSP Y THEN Y #+ CURRENT!-MODULUS ELSE Y
   ELSE BEGIN SCALAR W;
%N.B. Invalid modular division is either:
% a)  attempt to divide by zero directly
% b)  modulus is not prime, and input is not
%     coprime with it;
     W:=IQUOTIENT(A,B); %Truncated integer division;
     RETURN RECIPROCAL!-BY!-GCD(B,A #- B #* W,
			        Y,X #- Y #* W)
   END;


SMACRO PROCEDURE MODULAR!-QUOTIENT(A,B);
    MODULAR!-TIMES(A,MODULAR!-RECIPROCAL B);


SMACRO PROCEDURE MODULAR!-MINUS A;
    IF A=0 THEN A ELSE CURRENT!-MODULUS #- A;




% Comparison functions used with the sort package;

SYMBOLIC PROCEDURE LESSPCAR(A,B);
    CAR A < CAR B;

SYMBOLIC PROCEDURE LESSPCDR(A,B);
    CDR A < CDR B;

SYMBOLIC PROCEDURE LESSPPAIR(A,B);
    IF CAR A=CAR B THEN CDR A < CDR B
    ELSE CAR A < CAR B;

SYMBOLIC PROCEDURE GREATERPCDR(A,B);
    CDR A > CDR B;

SYMBOLIC PROCEDURE LESSPCDADR(A,B);
    CDADR A < CDADR B;

SYMBOLIC PROCEDURE LESSPDEG(A,B);
    IF DOMAINP B THEN NIL
    ELSE IF DOMAINP A THEN T
    ELSE LDEG A < LDEG B;

SYMBOLIC PROCEDURE ORDOPCAR(A,B);
    ORDOP(CAR A,CAR B);

SYMBOLIC PROCEDURE ORDERFACTORS(A,B);
    IF CDR A=CDR B THEN ORDP(CAR A,CAR B)
    ELSE CDR A < CDR B;


% ENDMODULE;


MODULE FLUIDS;

% *******************************************************************
%
%   copyright (c)  university of cambridge, england 1981
%
% *******************************************************************;



SYMBOLIC PROCEDURE ERRORF MSGG;
 BEGIN
    TERPRI();
    PRIN2 "*** ERROR IN FACTORIZATION: ";
    PRIN2 MSGG;
    TERPRI();
    ERROR(0,'ERRORF)
 END;

% macro definitions for functions that create and
% access reduce-type datastructures;

SMACRO PROCEDURE TVAR A;
    CAAR A;


FLUID '(POLYZERO);
POLYZERO:=NIL;

SMACRO PROCEDURE POLYZEROP U; NULL U;
SMACRO PROCEDURE DIDNTGO Q; NULL Q;
SMACRO PROCEDURE DEPENDS!-ON!-VAR(A,V);
  (LAMBDA !#!#A;
    (NOT DOMAINP !#!#A) AND (MVAR !#!#A=V)) A;

SMACRO PROCEDURE L!-NUMERIC!-C(A,VLIST);
  LNC A;

% macro definitions for use in berlekamps algorithm;

% SMACROs used in linear equation package;

SMACRO PROCEDURE GETM2(A,I,J);
% Store by rows, to ease pivoting process;
    GETV(GETV(A,I),J);

SMACRO PROCEDURE PUTM2(A,I,J,V);
    PUTV(GETV(A,I),J,V);




SMACRO PROCEDURE !*D2N A;
% converts domain elt into number;
  (LAMBDA !#A!#;
    IF NULL !#A!# THEN 0 ELSE !#A!#) A;

SMACRO PROCEDURE !*NUM2F N;
% converts number to s.f. ;
  (LAMBDA !#N!#;
    IF !#N!#=0 THEN NIL ELSE !#N!#) N;

SMACRO PROCEDURE !*MOD2F U; U;
SMACRO PROCEDURE !*F2MOD U; U;

SMACRO PROCEDURE COMES!-BEFORE(P1,P2);
% Similar to the REDUCE function ORDPP, but does not cater for
% non-commutative terms and assumes that exponents are small
% integers;
    (CAR P1=CAR P2 AND IGREATERP(CDR P1,CDR P2)) OR
       (NOT CAR P1=CAR P2 AND ORDOP(CAR P1,CAR P2));

SMACRO PROCEDURE ADJOIN!-TERM (P,C,R);
  (LAMBDA !#C!#; % Lambda binding prevents repeated evaluation of C;
    IF NULL !#C!# THEN R ELSE (P .* !#C!#) .+ R) C;


% a load of access smacros for image sets follow:   ;

SMACRO PROCEDURE GET!-IMAGE!-SET S; CAR S;
SMACRO PROCEDURE GET!-CHOSEN!-PRIME S; CADR S;
SMACRO PROCEDURE GET!-IMAGE!-LC S; CADDR S;
SMACRO PROCEDURE GET!-IMAGE!-MOD!-P S; CADR CDDR S;
SMACRO PROCEDURE GET!-IMAGE!-CONTENT S; CADR CDR CDDR S;
SMACRO PROCEDURE GET!-IMAGE!-POLY S; CADR CDDR CDDR S;
SMACRO PROCEDURE GET!-F!-NUMVEC S; CADR CDDR CDDDR S;

SMACRO PROCEDURE PUT!-IMAGE!-POLY!-AND!-CONTENT(S,IMCONT,IMPOL);
  LIST(GET!-IMAGE!-SET S,
       GET!-CHOSEN!-PRIME S,
       GET!-IMAGE!-LC S,
       GET!-IMAGE!-MOD!-P S,
       IMCONT,
       IMPOL,
       GET!-F!-NUMVEC S);


FLUID '(
!*GCD
!*EXP
SAFE!-FLAG
BASE!-TIME
GC!-BASE!-TIME
LAST!-DISPLAYED!-TIME
LAST!-DISPLAYED!-GC!-TIME
INPUT!-POLYNOMIAL
PRIMES
CURRENT!-MODULUS
MODULUS!/2
POLY!-MOD!-P
INPUT!-LEADING!-COEFFICIENT
INPUT!-NORM
INPUT!-MAIN!-VARIABLE
NUMBER!-NEEDED
BEST!-VARIABLE
KNOWN!-FACTORS
X!*!*P
DX!*!*P
WORK!-VECTOR1
DWORK1
WORK!-VECTOR2
DWORK2
POLY!-VECTOR
DPOLY
LINEAR!-FACTORS
NULL!-SPACE!-BASIS
SPLIT!-LIST
FACTOR!-COUNT
BEST!-FACTOR!-COUNT
BEST!-KNOWN!-FACTORS
MODULAR!-SPLITTINGS
BEST!-MODULUS
VALID!-IMAGE!-SETS
FACTORED!-LC
MULTIVARIATE!-INPUT!-POLY
BEST!-SET!-POINTER
IMAGE!-FACTORS
TRUE!-LEADING!-COEFFTS
IRREDUCIBLE
INVERTED
INVERTED!-SIGN
NUMBER!-OF!-FACTORS
M!-IMAGE!-VARIABLE
MODULAR!-VALUES
NO!-OF!-RANDOM!-SETS
NO!-OF!-BEST!-SETS
IMAGE!-SET!-MODULUS
!*ALL!-CONTENTS
FACTOR!-X
SFP!-COUNT
FACTOR!-TRACE!-LIST
FACTOR!-LEVEL
!*OVERVIEW
!*OVERSHOOT
NON!-MONIC
!*NEW!-TIMES!-MOD!-P
POLYNOMIAL!-TO!-FACTOR
FORBIDDEN!-SETS
FORBIDDEN!-PRIMES
VARS!-TO!-KILL
ZERO!-SET!-TRIED
BAD!-CASE
PREVIOUS!-DEGREE!-MAP
TARGET!-FACTOR!-COUNT
MODULAR!-INFO
MULTIVARIATE!-FACTORS
IMAGE!-SET
CHOSEN!-PRIME
IMAGE!-LC
IMAGE!-MOD!-P
IMAGE!-CONTENT
IMAGE!-POLY
F!-NUMVEC
VALID!-PRIMES
UNIVARIATE!-INPUT!-POLY
NO!-OF!-RANDOM!-PRIMES
NO!-OF!-BEST!-PRIMES
UNIVARIATE!-FACTORS
!*FORCE!-PRIME
!*FORCE!-ZERO!-SET
!*LINEAR
!*MULTIVARIATE!-TREATMENT
!*TIMINGS
RECONSTRUCTING!-GCD
FULL!-GCD
PREDICTIONS
PRIME!-BASE
ONE!-COMPLETE!-DEG!-ANALYSIS!-DONE
DEGREE!-BOUNDS
UNKNOWNS!-LIST
UNKNOWN
DEG!-OF!-UNKNOWN
DIVISOR!-FOR!-UNKNOWN
DIFFERENCE!-FOR!-UNKNOWN
BEST!-KNOWN!-FACTOR!-LIST
COEFFT!-VECTORS
REDUCED!-DEGREE!-LCLST
UNLUCKY!-CASE
!*KERNREVERSE
EXACT!-QUOTIENT!-FLAG
NUMBER!-OF!-UNKNOWNS
MAX!-UNKNOWNS
USER!-PRIME
NN
!*LINEAR
FACTORS!-DONE
COEFFTBD
HENSEL!-POLY
ZEROVARSET
ZSET
OTHERVARS
SAVE!-ZSET
REDUCTION!-COUNT
    );
!*TIMINGS:=NIL; % Default not to displaying timings;
!*OVERSHOOT:=NIL; % Default not to show overshoot occurring;
RECONSTRUCTING!-GCD:=NIL;  % This is primarily a factorizer!  ;

FLUID '(HENSEL!-GROWTH!-SIZE ALPHALIST);
FLUID '(
 FACVEC
 FHATVEC
 FACTORVEC
 MODFVEC
 ALPHAVEC
 DELFVEC
 DELTAM
 CURRENT!-FACTOR!-PRODUCT
 );

GLOBAL '(POSN!* SPARE!*);   %used in TTAB*;

SYMBOLIC PROCEDURE TTAB!* N;
<<
  IF N>(LINELENGTH NIL - SPARE!*) THEN N:=0;
  IF POSN!* > N THEN TERPRI!*(NIL);
  WHILE NOT(POSN!*=N) DO PRIN2!* '!  >>;

SMACRO PROCEDURE PRINTSTR L;
<< PRIN2!* L; TERPRI!*(NIL) >>;

SYMBOLIC PROCEDURE FAC!-PRINTSF A;
 << IF A THEN XPRINF(A,NIL,NIL) ELSE PRIN2!* 0;
    TERPRI!* NIL >>;

SMACRO PROCEDURE PRINSF U;
  IF U THEN XPRINF(U,NIL,NIL)
  ELSE PRIN2!* 0;

SMACRO PROCEDURE PRINTVAR V; PRINTSTR V;

SMACRO PROCEDURE PRINVAR V; PRIN2!* V;

SYMBOLIC PROCEDURE PRINTVEC(STR1,N,STR2,V);
<< FOR I:=1:N DO <<
    PRIN2!* STR1;
    PRIN2!* I;
    PRIN2!* STR2;
    FAC!-PRINTSF GETV(V,I) >>;
   TERPRI!*(NIL) >>;

SMACRO PROCEDURE DISPLAY!-TIME(STR,MT);
% Displays the string str followed by time mt (millisecs);
  << PRINC STR; PRINC MT; PRINTC " millisecs." >>;

% trace control package.
%
%;

SMACRO PROCEDURE TRACE!-TIME ACTION;
  IF !*TIMINGS THEN ACTION;

SMACRO PROCEDURE NEW!-LEVEL(N,C);
  (LAMBDA FACTOR!-LEVEL; C) N;

SYMBOLIC PROCEDURE SET!-TRACE!-FACTOR(N,FILE);
    FACTOR!-TRACE!-LIST:=(N . (IF FILE=NIL THEN NIL
			       ELSE OPEN(MKFIL FILE,'OUTPUT))) .
			                        FACTOR!-TRACE!-LIST;

SYMBOLIC PROCEDURE CLEAR!-TRACE!-FACTOR N;
  BEGIN
    SCALAR W;
    W := ASSOC(N,FACTOR!-TRACE!-LIST);
    IF W THEN <<
       IF CDR W THEN CLOSE CDR W;
       FACTOR!-TRACE!-LIST:=DELASC(N,FACTOR!-TRACE!-LIST) >>;
    RETURN NIL
  END; 

SYMBOLIC PROCEDURE CLOSE!-TRACE!-FILES();
 << WHILE FACTOR!-TRACE!-LIST
       DO CLEAR!-TRACE!-FACTOR(CAAR FACTOR!-TRACE!-LIST);
    NIL >>;


FACTOR!-TRACE!-LIST:=NIL;
FACTOR!-LEVEL:=0;  % start with a numeric value;


ENDMODULE;


MODULE ALPHAS;

% *******************************************************************
%
%   copyright (c)  university of cambridge, england 1979
%
% *******************************************************************;




%********************************************************************;
%
% this section contains access and update functions for the alphas;


SYMBOLIC PROCEDURE GET!-ALPHA POLY;
% gets the poly and its associated alpha from the current alphalist
% if poly is not on the alphalist then we force an error;
  BEGIN SCALAR W;
    W:=ASSOC!-ALPHA(POLY,ALPHALIST);
    IF NULL W THEN ERRORF LIST("Alpha not found for ",POLY," in ",
        ALPHALIST);
    RETURN W
  END;

SYMBOLIC PROCEDURE DIVIDE!-ALL!-ALPHAS N;
% multiply the factors by n mod p and alter the alphas accordingly;
  BEGIN SCALAR OM,M;
    OM:=SET!-MODULUS HENSEL!-GROWTH!-SIZE;
    M:=MODULAR!-EXPT(
          MODULAR!-RECIPROCAL MODULAR!-NUMBER N,
          NUMBER!-OF!-FACTORS #- 1);
    ALPHALIST:=FOR EACH A IN ALPHALIST COLLECT
      (TIMES!-MOD!-P(N,CAR A) . TIMES!-MOD!-P(M,CDR A));
    SET!-MODULUS OM
  END;

SYMBOLIC PROCEDURE MULTIPLY!-ALPHAS(N,OLDPOLY,NEWPOLY);
% multiply all the alphas except the one associated with oldpoly
% by n mod p. also replace oldpoly by newpoly in the alphalist;
  BEGIN SCALAR OM,FACA,W;
    OM:=SET!-MODULUS HENSEL!-GROWTH!-SIZE;
    N:=MODULAR!-NUMBER N;
    OLDPOLY:=REDUCE!-MOD!-P OLDPOLY;
    FACA:=GET!-ALPHA OLDPOLY;
    ALPHALIST:=DELETE(FACA,ALPHALIST);
    ALPHALIST:=FOR EACH A IN ALPHALIST COLLECT
      CAR A . TIMES!-MOD!-P(CDR A,N);
    ALPHALIST:=(REDUCE!-MOD!-P NEWPOLY . CDR FACA) . ALPHALIST;
    SET!-MODULUS OM
  END;

SYMBOLIC PROCEDURE MULTIPLY!-ALPHAS!-RECIP(N,OLDPOLY,NEWPOLY);
% multiply all the alphas except the one associated with oldpoly
% by the reciprocal mod p of n. also replace oldpoly by newpoly;
  BEGIN SCALAR OM,W;
    OM:=SET!-MODULUS HENSEL!-GROWTH!-SIZE;
    N:=MODULAR!-RECIPROCAL MODULAR!-NUMBER N;
    W:=MULTIPLY!-ALPHAS(N,OLDPOLY,NEWPOLY);
    SET!-MODULUS OM;
    RETURN W
  END;

ENDMODULE;


MODULE BIGMODP;

% (C) Copyright 1981, University of Cambridge;

% Modular arithmetic where the modulus may be a bignum.

% Currently only called from section UNIHENS;





SYMBOLIC PROCEDURE SET!-GENERAL!-MODULUS P;
  IF NOT NUMBERP P THEN CURRENT!-MODULUS
  ELSE BEGIN
    SCALAR PREVIOUS!-MODULUS;
    PREVIOUS!-MODULUS:=CURRENT!-MODULUS;
    CURRENT!-MODULUS:=P;
    MODULUS!/2 := P/2;
    RETURN PREVIOUS!-MODULUS
  END;

SYMBOLIC PROCEDURE GENERAL!-PLUS!-MOD!-P(A,B);
% form the sum of the two polynomials a and b
% working over the ground domain defined by the routines
% general!-modular!-plus, general!-modular!-times etc. the inputs to
% this routine are assumed to have coefficients already
% in the required domain;
   IF NULL A THEN B
   ELSE IF NULL B THEN A
   ELSE IF ISDOMAIN A THEN
      IF ISDOMAIN B THEN !*NUM2F GENERAL!-MODULAR!-PLUS(A,B)
      ELSE (LT B) .+ GENERAL!-PLUS!-MOD!-P(A,RED B)
   ELSE IF ISDOMAIN B THEN (LT A) .+ GENERAL!-PLUS!-MOD!-P(RED A,B)
   ELSE IF LPOW A = LPOW B THEN
      ADJOIN!-TERM(LPOW A,
	 GENERAL!-PLUS!-MOD!-P(LC A,LC B),
	 GENERAL!-PLUS!-MOD!-P(RED A,RED B))
   ELSE IF COMES!-BEFORE(LPOW A,LPOW B) THEN
         (LT A) .+ GENERAL!-PLUS!-MOD!-P(RED A,B)
   ELSE (LT B) .+ GENERAL!-PLUS!-MOD!-P(A,RED B);



SYMBOLIC PROCEDURE GENERAL!-TIMES!-MOD!-P(A,B);
   IF (NULL A) OR (NULL B) THEN NIL
   ELSE IF ISDOMAIN A THEN GEN!-MULT!-BY!-CONST!-MOD!-P(B,A)
   ELSE IF ISDOMAIN B THEN GEN!-MULT!-BY!-CONST!-MOD!-P(A,B)
   ELSE IF MVAR A=MVAR B THEN GENERAL!-PLUS!-MOD!-P(
     GENERAL!-PLUS!-MOD!-P(GENERAL!-TIMES!-TERM!-MOD!-P(LT A,B),
                  GENERAL!-TIMES!-TERM!-MOD!-P(LT B,RED A)),
     GENERAL!-TIMES!-MOD!-P(RED A,RED B))
   ELSE IF ORDOP(MVAR A,MVAR B) THEN
     ADJOIN!-TERM(LPOW A,GENERAL!-TIMES!-MOD!-P(LC A,B),
       GENERAL!-TIMES!-MOD!-P(RED A,B))
   ELSE ADJOIN!-TERM(LPOW B,
        GENERAL!-TIMES!-MOD!-P(A,LC B),GENERAL!-TIMES!-MOD!-P(A,RED B));


SYMBOLIC PROCEDURE GENERAL!-TIMES!-TERM!-MOD!-P(TERM,B);
%multiply the given polynomial by the given term;
    IF NULL B THEN NIL
    ELSE IF ISDOMAIN B THEN
        ADJOIN!-TERM(TPOW TERM,
            GEN!-MULT!-BY!-CONST!-MOD!-P(TC TERM,B),NIL)
    ELSE IF TVAR TERM=MVAR B THEN
         ADJOIN!-TERM(MKSP(TVAR TERM,IPLUS(TDEG TERM,LDEG B)),
                      GENERAL!-TIMES!-MOD!-P(TC TERM,LC B),
                      GENERAL!-TIMES!-TERM!-MOD!-P(TERM,RED B))
    ELSE IF ORDOP(TVAR TERM,MVAR B) THEN
      ADJOIN!-TERM(TPOW TERM,GENERAL!-TIMES!-MOD!-P(TC TERM,B),NIL)
    ELSE ADJOIN!-TERM(LPOW B,
      GENERAL!-TIMES!-TERM!-MOD!-P(TERM,LC B),
      GENERAL!-TIMES!-TERM!-MOD!-P(TERM,RED B));

SYMBOLIC PROCEDURE GEN!-MULT!-BY!-CONST!-MOD!-P(A,N);
% multiply the polynomial a by the constant n;
   IF NULL A THEN NIL
   ELSE IF N=1 THEN A
   ELSE IF ISDOMAIN A THEN !*NUM2F GENERAL!-MODULAR!-TIMES(A,N)
   ELSE ADJOIN!-TERM(LPOW A,GEN!-MULT!-BY!-CONST!-MOD!-P(LC A,N),
     GEN!-MULT!-BY!-CONST!-MOD!-P(RED A,N));

SYMBOLIC PROCEDURE GENERAL!-DIFFERENCE!-MOD!-P(A,B);
   GENERAL!-PLUS!-MOD!-P(A,GENERAL!-MINUS!-MOD!-P B);

SYMBOLIC PROCEDURE GENERAL!-MINUS!-MOD!-P A;
   IF NULL A THEN NIL
   ELSE IF ISDOMAIN A THEN GENERAL!-MODULAR!-MINUS A
   ELSE (LPOW A .* GENERAL!-MINUS!-MOD!-P LC A) .+
        GENERAL!-MINUS!-MOD!-P RED A;

SYMBOLIC PROCEDURE GENERAL!-REDUCE!-MOD!-P A;
%converts a multivariate poly from normal into modular polynomial;
    IF NULL A THEN NIL
    ELSE IF ISDOMAIN A THEN !*NUM2F GENERAL!-MODULAR!-NUMBER A
    ELSE ADJOIN!-TERM(LPOW A,
                      GENERAL!-REDUCE!-MOD!-P LC A,
                      GENERAL!-REDUCE!-MOD!-P RED A);

SYMBOLIC PROCEDURE GENERAL!-MAKE!-MODULAR!-SYMMETRIC A;
% input is a multivariate MODULAR poly A with nos in the range 0->(p-1).
% This folds it onto the symmetric range (-p/2)->(p/2);
    IF NULL A THEN NIL
    ELSE IF DOMAINP A THEN
      IF A>MODULUS!/2 THEN !*NUM2F(A - CURRENT!-MODULUS)
      ELSE A
    ELSE ADJOIN!-TERM(LPOW A,
                      GENERAL!-MAKE!-MODULAR!-SYMMETRIC LC A,
                      GENERAL!-MAKE!-MODULAR!-SYMMETRIC RED A);

SYMBOLIC PROCEDURE GENERAL!-MODULAR!-PLUS(A,B);
  BEGIN SCALAR RESULT;
     RESULT:=A+B;
     IF RESULT >= CURRENT!-MODULUS THEN RESULT:=RESULT-CURRENT!-MODULUS;
     RETURN RESULT
  END;

SYMBOLIC PROCEDURE GENERAL!-MODULAR!-DIFFERENCE(A,B);
  BEGIN SCALAR RESULT;
     RESULT:=A-B;
     IF RESULT < 0 THEN RESULT:=RESULT+CURRENT!-MODULUS;
     RETURN RESULT
  END;

SYMBOLIC PROCEDURE GENERAL!-MODULAR!-NUMBER A;
  BEGIN
     A:=REMAINDER(A,CURRENT!-MODULUS);
     IF A < 0 THEN A:=A+CURRENT!-MODULUS;
     RETURN A
  END;

SYMBOLIC PROCEDURE GENERAL!-MODULAR!-TIMES(A,B);
  BEGIN SCALAR RESULT;
     RESULT:=REMAINDER(A*B,CURRENT!-MODULUS);
     IF RESULT < 0 THEN RESULT:=RESULT+CURRENT!-MODULUS;
     RETURN RESULT
  END;


SYMBOLIC PROCEDURE GENERAL!-MODULAR!-RECIPROCAL A;
  BEGIN
    RETURN RECIPROCAL!-BY!-GCD(CURRENT!-MODULUS,A,0,1)
  END;

SYMBOLIC PROCEDURE RECIPROCAL!-BY!-GCD(A,B,X,Y);
%On input A and B should be coprime. This routine then
%finds X and Y such that A*X+B*Y=1, and returns the value Y
%on input A > B;
   IF B=0 THEN ERRORF "INVALID MODULAR DIVISION"
   ELSE IF B=1 THEN IF Y < 0 THEN Y+CURRENT!-MODULUS ELSE Y
   ELSE BEGIN SCALAR W;
%N.B. Invalid modular division is either:
% a)  attempt to divide by zero directly
% b)  modulus is not prime, and input is not
%     coprime with it;
     W:=QUOTIENT(A,B); %Truncated integer division;
     RETURN RECIPROCAL!-BY!-GCD(B,A-B*W,Y,X-Y*W)
   END;


SYMBOLIC PROCEDURE GENERAL!-MODULAR!-QUOTIENT(A,B);
    GENERAL!-MODULAR!-TIMES(A,GENERAL!-MODULAR!-RECIPROCAL B);


SYMBOLIC PROCEDURE GENERAL!-MODULAR!-MINUS A;
    IF A=0 THEN A ELSE CURRENT!-MODULUS - A;


ENDMODULE;


MODULE COEFFTS;

% *******************************************************************
%
%   copyright (c)  university of cambridge, england 1979
%
% *******************************************************************;





%**********************************************************************;
%  code for trying to determine more multivariate coefficients
%  by inspection before using multivariate hensel construction.  ;


SYMBOLIC PROCEDURE DETERMINE!-MORE!-COEFFTS();
% ...;
  BEGIN SCALAR UNKNOWNS!-LIST,UV,R,W,BEST!-KNOWN!-FACTOR!-LIST;
    BEST!-KNOWN!-FACTORS:=MKVECT NUMBER!-OF!-FACTORS;
    UV:=MKVECT NUMBER!-OF!-FACTORS;
    FOR I:=NUMBER!-OF!-FACTORS STEP -1 UNTIL 1 DO
      PUTV(UV,I,CONVERT!-FACTOR!-TO!-TERMVECTOR(
        GETV(IMAGE!-FACTORS,I),GETV(TRUE!-LEADING!-COEFFTS,I)));
    R:=RED MULTIVARIATE!-INPUT!-POLY;
            % we know all about the leading coeffts;
    IF NOT DEPENDS!-ON!-VAR(R,M!-IMAGE!-VARIABLE)
      OR NULL(W:=TRY!-FIRST!-COEFFT(
              LDEG R,LC R,UNKNOWNS!-LIST,UV)) THEN <<
      FOR I:=1:NUMBER!-OF!-FACTORS DO
        PUTV(BEST!-KNOWN!-FACTORS,I,FORCE!-LC(
          GETV(IMAGE!-FACTORS,I),GETV(TRUE!-LEADING!-COEFFTS,I)));
      COEFFT!-VECTORS:=UV;
      RETURN NIL >>;
    FACTOR!-TRACE <<
      PRINTSTR
	 "By exploiting any sparsity wrt the main variable in the";
      PRINTSTR "factors, we can try guessing some of the multivariate";
      PRINTSTR "coefficients." >>;
    TRY!-OTHER!-COEFFTS(R,UNKNOWNS!-LIST,UV);
    W:=CONVERT!-AND!-TRIAL!-DIVIDE UV;
    TRACE!-TIME
      IF FULL!-GCD THEN PRINTC "Possible gcd found"
      ELSE PRINTC "Have found some coefficients";
    RETURN SET!-UP!-GLOBALS(UV,W)
  END;

SYMBOLIC PROCEDURE CONVERT!-FACTOR!-TO!-TERMVECTOR(U,TLC);
% ...;
  BEGIN SCALAR TERMLIST,RES,N,SLIST;
    TERMLIST:=(LDEG U . TLC) . LIST!-TERMS!-IN!-FACTOR RED U;
    RES:=MKVECT (N:=LENGTH TERMLIST);
    FOR I:=1:N DO <<
      SLIST:=(CAAR TERMLIST . I) . SLIST;
      PUTV(RES,I,CAR TERMLIST);
      TERMLIST:=CDR TERMLIST >>;
    PUTV(RES,0,(N . (N #- 1)));
    UNKNOWNS!-LIST:=(REVERSEWOC SLIST) . UNKNOWNS!-LIST;
    RETURN RES
  END;

SYMBOLIC PROCEDURE TRY!-FIRST!-COEFFT(N,C,SLIST,UV);
% ...;
  BEGIN SCALAR COMBNS,UNKNOWN,W,L,D,V,M;
    COMBNS:=GET!-TERM(N,SLIST);
    IF (COMBNS='NO) OR NOT NULL CDR COMBNS THEN RETURN NIL;
    L:=CAR COMBNS;
    FOR I:=1:NUMBER!-OF!-FACTORS DO <<
      W:=GETV(GETV(UV,I),CAR L);    % degree . coefft ;
      IF NULL CDR W THEN <<
        UNKNOWN:=(I . CAR L);
        D:=CAR W >>
      ELSE <<
        C:=QUOTF(C,CDR W);
        IF DIDNTGO C THEN RETURN >>;
      L:=CDR L >>;
    IF DIDNTGO C THEN RETURN NIL;
    PUTV(V:=GETV(UV,CAR UNKNOWN),CDR UNKNOWN,(D . C));
    M:=GETV(V,0);
    PUTV(V,0,(CAR M . (CDR M #- 1)));
    IF CDR M = 1 AND FACTORS!-COMPLETE UV THEN RETURN 'COMPLETE;
    RETURN C
  END;

SYMBOLIC PROCEDURE SOLVE!-NEXT!-COEFFT(N,C,SLIST,UV);
% ...;
  BEGIN SCALAR COMBNS,W,UNKNOWN,DEG!-OF!-UNKNOWN,DIVISOR!-FOR!-UNKNOWN,
    DIFFERENCE!-FOR!-UNKNOWN,V;
    DIFFERENCE!-FOR!-UNKNOWN:=POLYZERO;
    DIVISOR!-FOR!-UNKNOWN:=POLYZERO;
    COMBNS:=GET!-TERM(N,SLIST);
    IF COMBNS='NO THEN RETURN 'NOGOOD;
    WHILE COMBNS DO <<
      W:=SPLIT!-TERM!-LIST(CAR COMBNS,UV);
      IF W='NOGOOD THEN RETURN W;
      COMBNS:=CDR COMBNS >>;
    IF W='NOGOOD THEN RETURN W;
    IF NULL UNKNOWN THEN RETURN;
    W:=QUOTF(ADDF(C,NEGF DIFFERENCE!-FOR!-UNKNOWN),
	     DIVISOR!-FOR!-UNKNOWN);
    IF DIDNTGO W THEN RETURN 'NOGOOD;
    PUTV(V:=GETV(UV,CAR UNKNOWN),CDR UNKNOWN,(DEG!-OF!-UNKNOWN . W));
    N:=GETV(V,0);
    PUTV(V,0,(CAR N . (CDR N #- 1)));
    IF CDR N = 1 AND FACTORS!-COMPLETE UV THEN RETURN 'COMPLETE;
    RETURN W
  END;

SYMBOLIC PROCEDURE SPLIT!-TERM!-LIST(TERM!-COMBN,UV);
% ...;
  BEGIN SCALAR A,V,W;
    A:=1;
    FOR I:=1:NUMBER!-OF!-FACTORS DO <<
      W:=GETV(GETV(UV,I),CAR TERM!-COMBN);  % degree . coefft ;
      IF NULL CDR W THEN
        IF V OR (UNKNOWN AND NOT((I.CAR TERM!-COMBN)=UNKNOWN)) THEN
          RETURN V:='NOGOOD
        ELSE <<
          UNKNOWN:=(I . CAR TERM!-COMBN);
          DEG!-OF!-UNKNOWN:=CAR W;
          V:=UNKNOWN >>
      ELSE A:=MULTF(A,CDR W);
      TERM!-COMBN:=CDR TERM!-COMBN >>;
    IF V='NOGOOD THEN RETURN V;
    IF V THEN DIVISOR!-FOR!-UNKNOWN:=ADDF(DIVISOR!-FOR!-UNKNOWN,A)
    ELSE DIFFERENCE!-FOR!-UNKNOWN:=ADDF(DIFFERENCE!-FOR!-UNKNOWN,A);
    RETURN 'OK
  END;

SYMBOLIC PROCEDURE FACTORS!-COMPLETE UV;
% ...;
  BEGIN SCALAR FACTOR!-NOT!-DONE,R;
    R:=T;
    FOR I:=1:NUMBER!-OF!-FACTORS DO
      IF NOT(CDR GETV(GETV(UV,I),0)=0) THEN
        IF FACTOR!-NOT!-DONE THEN RETURN R:=NIL
        ELSE FACTOR!-NOT!-DONE:=T;
    RETURN R
  END;

SYMBOLIC PROCEDURE CONVERT!-AND!-TRIAL!-DIVIDE UV;
% ...;
  BEGIN SCALAR W,R,FDONE!-PRODUCT!-MOD!-P,OM;
    OM:=SET!-MODULUS HENSEL!-GROWTH!-SIZE;
    FDONE!-PRODUCT!-MOD!-P:=1;
    FOR I:=1:NUMBER!-OF!-FACTORS DO <<
      W:=GETV(UV,I);
      W:= IF (CDR GETV(W,0))=0 THEN TERMVECTOR2SF W
        ELSE MERGE!-TERMS(GETV(IMAGE!-FACTORS,I),W);
      R:=QUOTF(MULTIVARIATE!-INPUT!-POLY,W);
      IF DIDNTGO R THEN BEST!-KNOWN!-FACTOR!-LIST:=
        ((I . W) . BEST!-KNOWN!-FACTOR!-LIST)
      ELSE IF RECONSTRUCTING!-GCD AND I=1 THEN RETURN
        FULL!-GCD:=IF NON!-MONIC THEN CAR PRIMITIVE!.PARTS(
          LIST W,M!-IMAGE!-VARIABLE,NIL) ELSE W
      ELSE <<
        MULTIVARIATE!-FACTORS:=W . MULTIVARIATE!-FACTORS;
        FDONE!-PRODUCT!-MOD!-P:=TIMES!-MOD!-P(
          REDUCE!-MOD!-P GETV(IMAGE!-FACTORS,I),
          FDONE!-PRODUCT!-MOD!-P);
        MULTIVARIATE!-INPUT!-POLY:=R >> >>;
    IF FULL!-GCD THEN RETURN;
    IF NULL BEST!-KNOWN!-FACTOR!-LIST THEN MULTIVARIATE!-FACTORS:=
      PRIMITIVE!.PARTS(MULTIVARIATE!-FACTORS,M!-IMAGE!-VARIABLE,NIL)
    ELSE IF NULL CDR BEST!-KNOWN!-FACTOR!-LIST THEN <<
      IF RECONSTRUCTING!-GCD THEN
        IF NOT(CAAR BEST!-KNOWN!-FACTOR!-LIST=1) THEN
          ERRORF("gcd is jiggered in determining other coeffts")
        ELSE FULL!-GCD:=IF NON!-MONIC THEN CAR PRIMITIVE!.PARTS(
          LIST MULTIVARIATE!-INPUT!-POLY,
          M!-IMAGE!-VARIABLE,NIL)
          ELSE MULTIVARIATE!-INPUT!-POLY
      ELSE MULTIVARIATE!-FACTORS:=PRIMITIVE!.PARTS(
        MULTIVARIATE!-INPUT!-POLY . MULTIVARIATE!-FACTORS,
        M!-IMAGE!-VARIABLE,NIL);
      BEST!-KNOWN!-FACTOR!-LIST:=NIL >>;
    FACTOR!-TRACE <<
      IF NULL BEST!-KNOWN!-FACTOR!-LIST THEN
	PRINTSTR
	   "We have completely determined all the factors this way"
      ELSE IF MULTIVARIATE!-FACTORS THEN <<
        PRIN2!* "We have completely determined the following factor";
        PRINTSTR IF (LENGTH MULTIVARIATE!-FACTORS)=1 THEN ":" ELSE "s:";
	FOR EACH WW IN MULTIVARIATE!-FACTORS DO FAC!-PRINTSF WW >> >>;
    SET!-MODULUS OM;
    RETURN FDONE!-PRODUCT!-MOD!-P
  END;

SYMBOLIC PROCEDURE SET!-UP!-GLOBALS(UV,F!-PRODUCT);
  IF NULL BEST!-KNOWN!-FACTOR!-LIST OR FULL!-GCD THEN 'DONE
  ELSE BEGIN SCALAR I,R,N,K,FLIST!-MOD!-P,IMF,OM,SAVEK;
    N:=LENGTH BEST!-KNOWN!-FACTOR!-LIST;
    BEST!-KNOWN!-FACTORS:=MKVECT N;
    COEFFT!-VECTORS:=MKVECT N;
    R:=MKVECT N;
    K:=IF RECONSTRUCTING!-GCD THEN 1 ELSE 0;
    OM:=SET!-MODULUS HENSEL!-GROWTH!-SIZE;
    FOR EACH W IN BEST!-KNOWN!-FACTOR!-LIST DO <<
      I:=CAR W; W:=CDR W;
      IF RECONSTRUCTING!-GCD AND I=1 THEN << SAVEK:=K; K:=1 >>
      ELSE K:=K #+ 1;
            % in case we are reconstructing gcd we had better know
            % which is the gcd and which the cofactor - so don't move
            % move the gcd from elt one;
      PUTV(R,K,IMF:=GETV(IMAGE!-FACTORS,I));
      FLIST!-MOD!-P:=(REDUCE!-MOD!-P IMF) . FLIST!-MOD!-P;
      PUTV(BEST!-KNOWN!-FACTORS,K,W);
      PUTV(COEFFT!-VECTORS,K,GETV(UV,I));
      IF RECONSTRUCTING!-GCD AND K=1 THEN K:=SAVEK;
            % restore k if necessary;
      >>;
    IF NOT(N=NUMBER!-OF!-FACTORS) THEN <<
      ALPHALIST:=FOR EACH MODF IN FLIST!-MOD!-P COLLECT
        (MODF . REMAINDER!-MOD!-P(TIMES!-MOD!-P(F!-PRODUCT,
          CDR GET!-ALPHA MODF),MODF));
      NUMBER!-OF!-FACTORS:=N >>;
    SET!-MODULUS OM;
    IMAGE!-FACTORS:=R;
    RETURN 'NEED! TO! RECONSTRUCT
  END;

SYMBOLIC PROCEDURE GET!-TERM(N,L);
% ...;
  IF N#<0 THEN 'NO
  ELSE IF NULL CDR L THEN GET!-TERM!-N(N,CAR L)
  ELSE BEGIN SCALAR W,RES;
    FOR EACH FTERM IN CAR L DO <<
      W:=GET!-TERM(N#-CAR FTERM,CDR L);
      IF NOT(W='NO) THEN RES:=
        APPEND(FOR EACH V IN W COLLECT (CDR FTERM . V),RES) >>;
    RETURN IF NULL RES THEN 'NO ELSE RES
  END;

SYMBOLIC PROCEDURE GET!-TERM!-N(N,U);
  IF NULL U OR N #> CAAR U THEN 'NO
  ELSE IF CAAR U = N THEN LIST(CDAR U . NIL)
  ELSE GET!-TERM!-N(N,CDR U);



ENDMODULE;


MODULE CPRES;

% part of resultant program;

SYMBOLIC PROCEDURE CPRES(A,B,X);
% calculates res(A,B) wrt X modulo p;
% A and B are multivariate polynomials modulo p with X as main variable;
BEGIN
INTEGER K, MR, MQ, NR, NQ, NUM!-B, LOOP!-COUNT;
SCALAR C, D, NEW!-A, NEW!-B, NEW!-C, Q, V;
IF NOT (MVAR A=X AND MVAR B=X)
THEN ERRORF "VARIABLE IS NOT IN BOTH POLYNOMIALS";
V := DELETE(X,UNION(VARIABLES!-IN!-FORM A,VARIABLES!-IN!-FORM B));
IF (V = NIL) THEN RETURN NATURAL!-PRS!-ALGORITHM(A,B,X); % simple case;
Q := CAR V; % Q is some variable other than X occuring in A or B;
MR := LDEG A;
NR := LDEG B;
MQ := DEGREE!-IN!-VARIABLE(A,Q);
NQ := DEGREE!-IN!-VARIABLE(B,Q);
K := MR*NQ + NR*MQ; COMMENT limit of degree of resultant in Q;
                    COMMENT I think the given value is wrong;
% PRINTC "VALUE OF K IS";
% SUPERPRINT K;
% initialise variables ;
C := 0;
D := 1;
NUM!-B := -1;
NEW!-A := A;
NEW!-B := B;
% main loop starts here;
WHILE (LEADING!-DEGREE D <= K)
DO BEGIN
   LOOP!-COUNT := 0; % ensures going round inner loop >= once;
                     % I'd use a boolean but there aren't any;
   % PRINTC "VALUE OF D IS";
   % SUPERPRINT D;
         WHILE ((DEGREE!-IN!-VARIABLE(NEW!-A,X) < MR)
            OR  (DEGREE!-IN!-VARIABLE(NEW!-B,X) < NR)
            OR  (LOOP!-COUNT = 0))
         DO BEGIN
            LOOP!-COUNT := 1;
            NUM!-B := NUM!-B + 1;
            IF (NUM!-B=SET!-MODULUS 0) THEN ERRORF "PRIME TOO SMALL";
            NEW!-A := EVALUATE!-MOD!-P(A,Q,NUM!-B);
            NEW!-B := EVALUATE!-MOD!-P(B,Q,NUM!-B);
            % PRINTC "NEW!-A AND NEW!-B ARE";
            % SUPERPRINT NEW!-A;
            % SUPERPRINT NEW!-B;
            END;
   % PRINTC "RECURSE HERE";
   NEW!-C := CPRES(NEW!-A,NEW!-B,X); COMMENT recursion applied;
   % PRINTC "VALUE OF NEW!-C AFTER RECURSION IS";
   % SUPERPRINT NEW!-C;
   % PRINTC "VALUE OF NUM!-B IS";
   % SUPERPRINT NUM!-B;
   % PRINTC "INTERPOLATE HERE";
   C := INTERPOLATE (D,NUM!-B,C,NEW!-C,Q);
   % PRINTC "VALUE OF C AFTER INTERPOLATION IS";
   % SUPERPRINT C;
   D := TIMES!-MOD!-P(DIFFERENCE!-MOD!-P
                        (!*K2F Q,!*N2F NUM!-B),D)
   END;
RETURN C
 END;

SYMBOLIC PROCEDURE INTERPOLATE(POLY!-D,NUMBER!-B,POLY!-A,POLY!-C,VAR);
% inputs - D = PI(xr - bi) for 0<=i<=k where the bi are distinct   ;
% elements of GF(p)  -  B is an element of GF(p) distinct from the ;
% bi  -  A(x1 ... xr) is a poly mod p of degree k or less in xr    ;
% -  C(x1 ... xr-1) is a poly mod p                                ;
% outputs H(x1 ... xr) of degree k+1 or less in xr where H         ;
% interpolates A for all points xr=bi and also H = C when xr=B     ;
% VAR = xr                                                         ;

PLUS!-MOD!-P(POLY!-A,
             TIMES!-MOD!-P(QUOTIENT!-MOD!-P(POLY!-D,
                                            EVALUATE!-MOD!-P(POLY!-D,
                                                             VAR,
							   NUMBER!-B)),
                           DIFFERENCE!-MOD!-P(POLY!-C,
                                              EVALUATE!-MOD!-P(POLY!-A,
                                                               VAR,
							 NUMBER!-B))));

SYMBOLIC PROCEDURE MAIN!-VARIABLE A;
% returns mvar a unless a is numeric, in which case returns nil;
IF ISDOMAIN A THEN NIL
ELSE MVAR A;


ENDMODULE;


MODULE DEGSETS;

%**********************************************************************;
%
%   copyright (c)  university of cambridge, england 1979
%
%**********************************************************************;




%**********************************************************************;
%
%    degree set processing
%;





SYMBOLIC PROCEDURE CHECK!-DEGREE!-SETS(N,MULTIVARIATE!-CASE);
% MODULAR!-INFO (vector of size N) contains the
% modular factors now;
  BEGIN SCALAR DEGREE!-SETS,W,X!-IS!-FACTOR,DEGS;
    W:=SPLIT!-LIST;
    FOR I:=1:N DO <<
      IF MULTIVARIATE!-CASE THEN
        X!-IS!-FACTOR:=NOT NUMBERP GET!-IMAGE!-CONTENT
          GETV(VALID!-IMAGE!-SETS,CDAR W);
      DEGS:=FOR EACH V IN GETV(MODULAR!-INFO,CDAR W) COLLECT LDEG V;
      DEGREE!-SETS:=
        (IF X!-IS!-FACTOR THEN 1 . DEGS ELSE DEGS)
              . DEGREE!-SETS;
      W:=CDR W >>;
    CHECK!-DEGREE!-SETS!-1 DEGREE!-SETS;
    BEST!-SET!-POINTER:=CDAR SPLIT!-LIST;
    IF MULTIVARIATE!-CASE AND FACTORED!-LC THEN <<
      WHILE NULL(W:=GET!-F!-NUMVEC
           GETV(VALID!-IMAGE!-SETS,BEST!-SET!-POINTER))
       AND (SPLIT!-LIST:=CDR SPLIT!-LIST) DO
        BEST!-SET!-POINTER:=CDAR SPLIT!-LIST;
      IF NULL W THEN BAD!-CASE:=T >>;
            % make sure the set is ok for distributing the
            % leading coefft where necessary;
  END;

SYMBOLIC PROCEDURE CHECK!-DEGREE!-SETS!-1 L;
% L is a list of degree sets. Try to discover if the entries
% in it are consistent, or if they imply that some of the
% modular splittings were 'false';
  BEGIN
    SCALAR I,DEGREE!-MAP,DEGREE!-MAP1,DPOLY,
        PLAUSIBLE!-SPLIT!-FOUND,TARGET!-COUNT;
    FACTOR!-TRACE <<
       PRINTC "Degree sets are:";
       FOR EACH S IN L DO <<
	  PRINC "     ";
	  FOR EACH N IN S DO <<
	     PRINC " "; PRINC N >>;
          TERPRI() >> >>;
    DPOLY:=SUM!-LIST CAR L;
    TARGET!-COUNT:=LENGTH CAR L;
    FOR EACH S IN CDR L DO TARGET!-COUNT:=IMIN(TARGET!-COUNT,
      LENGTH S);
    IF NULL PREVIOUS!-DEGREE!-MAP THEN <<
      DEGREE!-MAP:=MKVECT DPOLY;
% To begin with all degrees of factors may be possible;
      FOR I:=0:DPOLY DO PUTV(DEGREE!-MAP,I,T) >>
    ELSE <<
      FACTOR!-TRACE "Refine an existing degree map";
      DEGREE!-MAP:=PREVIOUS!-DEGREE!-MAP >>;
    DEGREE!-MAP1:=MKVECT DPOLY;
    FOR EACH S IN L DO <<
% For each degree set S I will collect in DEGREE-MAP1 a
% bitmap showing what degree factors would be consistent
% with that set. By ANDing together all these maps
% (into DEGREE-MAP) I find what degrees for factors are
% consistent with the whole of the information I have;
      FOR I:=0:DPOLY DO PUTV(DEGREE!-MAP1,I,NIL);
      PUTV(DEGREE!-MAP1,0,T);
      PUTV(DEGREE!-MAP1,DPOLY,T);
      FOR EACH D IN S DO FOR I:=DPOLY#-D#-1 STEP -1 UNTIL 0 DO
        IF GETV(DEGREE!-MAP1,I) THEN
           PUTV(DEGREE!-MAP1,I#+D,T);
      FOR I:=0:DPOLY DO
        PUTV(DEGREE!-MAP,I,GETV(DEGREE!-MAP,I) AND
             GETV(DEGREE!-MAP1,I)) >>;
    FACTOR!-TRACE <<
	PRINTC "Possible degrees for factors are: ";
        FOR I:=1:DPOLY#-1 DO
          IF GETV(DEGREE!-MAP,I) THEN << PRINC I; PRINC " " >>;
        TERPRI() >>;
    I:=DPOLY#-1;
    WHILE I#>0 DO IF GETV(DEGREE!-MAP,I) THEN I:=-1
                 ELSE I:=I#-1;
    IF I=0 THEN <<
       FACTOR!-TRACE
	  PRINTC "Degree analysis proves polynomial irreducible";
       RETURN IRREDUCIBLE:=T >>;
    FOR EACH S IN L DO IF LENGTH S=TARGET!-COUNT THEN BEGIN
      % Sets with too many factors are not plausible anyway;
      I:=S;
      WHILE I AND GETV(DEGREE!-MAP,CAR I) DO I:=CDR I;
      % If I drop through with I null it was because the set was
      % consistent, otherwise it represented a false split;
      IF NULL I THEN PLAUSIBLE!-SPLIT!-FOUND:=T END;
    PREVIOUS!-DEGREE!-MAP:=DEGREE!-MAP;
    IF PLAUSIBLE!-SPLIT!-FOUND OR ONE!-COMPLETE!-DEG!-ANALYSIS!-DONE
      THEN RETURN NIL;
%    PRINTC "Going to try getting some more images";
    RETURN BAD!-CASE:=T
  END;

SYMBOLIC PROCEDURE SUM!-LIST L;
   IF NULL CDR L THEN CAR L
   ELSE CAR L #+ SUM!-LIST CDR L;




ENDMODULE;


MODULE EZGCD;

% *******************************************************************
%
%   copyright (c)  university of cambridge, england 1981
%
% *******************************************************************;




% polynomial gcd algorithms;
%
% a. c. norman.  1981.
%
%
%**********************************************************************;

SYMBOLIC PROCEDURE EZGCDF(U,V);
   %entry point for REDUCE call in GCDF;
   BEGIN SCALAR FACTOR!-LEVEL;
      FACTOR!-LEVEL := 0;
      RETURN POLY!-ABS GCDLIST LIST(U,V)
   END;

%SYMBOLIC PROCEDURE SIMPEZGCD U;
% calculate the gcd of the polynomials given as arguments;
%  BEGIN
%    SCALAR FACTOR!-LEVEL,W;
%    FACTOR!-LEVEL:=0;
%    U := FOR EACH P IN U COLLECT <<
%        W := SIMP!* P;
%        IF (DENR W NEQ 1) THEN
%           REDERR "EZGCD requires polynomial arguments";
%        NUMR W >>;
%    RETURN (POLY!-ABS GCDLIST U) ./ 1
%  END;

%PUT('EZGCD,'SIMPFN,'SIMPEZGCD);

SYMBOLIC PROCEDURE SIMPNPRIMITIVE P;
% Remove any simple numeric factors from the expression P;
  BEGIN
    SCALAR NP,DP;
    IF ATOM P OR NOT ATOM CDR P THEN
       REDERR "NPRIMITIVE requires just one argument";
    P := SIMP!* CAR P;
    IF POLYZEROP(NUMR P) THEN RETURN NIL ./ 1;
    NP := QUOTFAIL(NUMR P,NUMERIC!-CONTENT NUMR P);
    DP := QUOTFAIL(DENR P,NUMERIC!-CONTENT DENR P);
    RETURN (NP ./ DP)
  END;

PUT('NPRIMITIVE,'SIMPFN,'SIMPNPRIMITIVE);





SYMBOLIC PROCEDURE POLY!-GCD(U,V);
   %U and V are standard forms.
   %Value is the gcd of U and V;
   BEGIN SCALAR XEXP,Y,Z;
        IF POLYZEROP U THEN RETURN POLY!-ABS V
         ELSE IF POLYZEROP V THEN RETURN POLY!-ABS U
         ELSE IF U=1 OR V=1 THEN RETURN 1;
        XEXP := !*EXP;
        !*EXP := T;
        % The case of one argument exactly dividing the other is
        % detected specially here because it is perhaps a fairly
        % common circumstance;
        IF QUOTF1(U,V) THEN Z := V
        ELSE IF QUOTF1(V,U) THEN Z := U
        ELSE IF !*GCD THEN  Z := GCDLIST LIST(U,V)
        ELSE Z := 1;
        !*EXP := XEXP;
        RETURN POLY!-ABS Z
   END;

MOVED('GCDF,'POLY!-GCD);



SYMBOLIC PROCEDURE EZGCD!-COMFAC P;
  %P is a standard form
  %CAR of result is lowest common power of leading kernel in
  %every term in P (or NIL). CDR is gcd of all coefficients of
  %powers of leading kernel;
  IF DOMAINP P THEN NIL . POLY!-ABS P
  ELSE IF NULL RED P THEN LPOW P . POLY!-ABS LC P
  ELSE BEGIN
    SCALAR POWER,COEFLIST,VAR;
    % POWER will be the first part of the answer returned,
    % COEFLIST will collect a list of all coefs in the polynomial
    % P viewed as a poly in its main variable,
    % VAR is the main variable concerned;
    VAR := MVAR P;
    WHILE MVAR P=VAR AND NOT DOMAINP RED P DO <<
      COEFLIST := LC P . COEFLIST;
      P:=RED P >>;
    IF MVAR P=VAR THEN <<
      COEFLIST := LC P . COEFLIST;
      IF NULL RED P THEN POWER := LPOW P
      ELSE COEFLIST := RED P . COEFLIST >>
    ELSE COEFLIST := P . COEFLIST;
    RETURN POWER . GCDLIST COEFLIST
  END;

GLOBAL '(!*FLOAT);

SYMBOLIC PROCEDURE GCD!-WITH!-NUMBER(N,A);
% n is a number, a is a polynomial - return their gcd, given that
% n is non-zero;
    IF N=1 OR NOT ATOM N OR !*FLOAT THEN 1
    ELSE IF DOMAINP A
     THEN IF A=NIL THEN ABS N
	   ELSE IF NOT ATOM A THEN 1
           ELSE GCD(N,A)
    ELSE GCD!-WITH!-NUMBER(GCD!-WITH!-NUMBER(N,LC A),RED A);

MOVED('GCDFD,'GCD!-WITH!-NUMBER);


SYMBOLIC PROCEDURE CONTENTS!-WITH!-RESPECT!-TO(P,V);
    IF DOMAINP P THEN NIL . POLY!-ABS P
    ELSE IF MVAR P=V THEN EZGCD!-COMFAC P
    ELSE BEGIN
      SCALAR Y,W;
      Y := SETKORDER LIST V;
      P := REORDER P;
      W := EZGCD!-COMFAC P;
      SETKORDER Y;
      P := REORDER P;
      RETURN REORDER W
    END;

SYMBOLIC PROCEDURE NUMERIC!-CONTENT FORM;
% Find numeric content of non-zero polynomial;
   IF DOMAINP FORM THEN ABS FORM
   ELSE IF NULL RED FORM THEN NUMERIC!-CONTENT LC FORM
   ELSE BEGIN
     SCALAR G1;
     G1 := NUMERIC!-CONTENT LC FORM;
     IF NOT (G1=1) THEN G1 := GCD(G1,NUMERIC!-CONTENT RED FORM);
     RETURN G1
   END;


SYMBOLIC PROCEDURE GCDLIST L;
% Return the GCD of all the polynomials in the list L.
%
% First find all variables mentioned in the polynomials in L,
% and remove monomial content from them all. If in the process
% a constant poly is found, take special action. If then there
% is some variable that is mentioned in all the polys in L, and
% which occurs only linearly in one of them establish that as
% main variable and proceed to GCDLIST3 (which will take s
% a special case exit). Otherwise, if there are any variables that
% do not occur in all the polys in L they can not occur in the GCD,
% so take coefficients with respect to them to get a longer list of
% smaller polynomials - restart. Finally we have a set of polys
% all involving exactly the same set of variables;
  IF NULL L THEN NIL
  ELSE IF NULL CDR L THEN POLY!-ABS CAR L
  ELSE IF DOMAINP CAR L THEN GCDLD(CDR L,CAR L)
  ELSE BEGIN
    SCALAR L1,GCONT,X;
    % Copy L to L1, but on the way detect any domain elements
    % and deal with them specially;
    WHILE NOT NULL L DO <<
        IF NULL CAR L THEN L := CDR L
        ELSE IF DOMAINP CAR L THEN <<
          L1 := LIST LIST GCDLD(CDR L,GCDLD(MAPCARCAR L1,CAR L));
          L := NIL >>
        ELSE <<
          L1 := (CAR L . POWERS1 CAR L) . L1;
          L := CDR L >> >>;
    IF NULL L1 THEN RETURN NIL
    ELSE IF NULL CDR L1 THEN RETURN POLY!-ABS CAAR L1;
    % Now L1 is a list where each polynomial is paired with information
    % about the powers of variables in it;
    GCONT := NIL; % Compute monomial content on things in L;
    X := NIL; % First time round flag;
    L := FOR EACH P IN L1 COLLECT BEGIN
        SCALAR GCONT1,GCONT2,W;
	% Set GCONT1 to least power information, and W to power
	% difference;
	W := FOR EACH Y IN CDR P
		COLLECT << GCONT1 := (CAR Y . CDDR Y) . GCONT1;
			   CAR Y . (CADR Y-CDDR Y) >>;
        % Now get the monomial content as a standard form (in GCONT2);
        GCONT2 := NUMERIC!-CONTENT CAR P;
        IF NULL X THEN << GCONT := GCONT1; X := GCONT2 >>
	ELSE << GCONT := VINTERSECTION(GCONT,GCONT1);
		   % Accumulate monomial gcd;
                X := GCD(X,GCONT2) >>;
        FOR EACH Q IN GCONT1 DO IF NOT CDR Q=0 THEN
            GCONT2 := MULTF(GCONT2,!*P2F MKSP(CAR Q,CDR Q));
	RETURN QUOTFAIL1(CAR P,GCONT2,"Term content division failed")
		  . W
        END;
    % Here X is the numeric part of the final GCD;
    FOR EACH Q IN GCONT DO X := MULTF(X,!*P2F MKSP(CAR Q,CDR Q));
    TRACE!-TIME <<
      PRIN2!* "Term gcd = ";
      FAC!-PRINTSF X >>;
    RETURN POLY!-ABS MULTF(X,GCDLIST1 L)
  END;


SYMBOLIC PROCEDURE GCDLIST1 L;
% Items in L are monomial-primitive, and paired with power information.
% Find out what variables are common to all polynomials in L and
% remove all others;
  BEGIN
    SCALAR UNIONV,INTERSECTIONV,VORD,X,L1,REDUCTION!-COUNT;
    UNIONV := INTERSECTIONV := CDAR L;
    FOR EACH P IN CDR L DO <<
       UNIONV := VUNION(UNIONV,CDR P);
       INTERSECTIONV := VINTERSECTION(INTERSECTIONV,CDR P) >>;
    IF NULL INTERSECTIONV THEN RETURN 1;
    FOR EACH V IN INTERSECTIONV DO
       UNIONV := VDELETE(V,UNIONV);
    % Now UNIONV is list of those variables mentioned that
    % are not common to all polynomials;
    INTERSECTIONV := SORT(INTERSECTIONV,FUNCTION LESSPCDR);
    IF CDAR INTERSECTIONV=1 THEN <<
       % I have found something that is linear in one of its variables;
       VORD := MAPCARCAR APPEND(INTERSECTIONV,UNIONV);
       L1 := SETKORDER VORD;
       TRACE!-TIME <<
         PRINC "Selecting "; PRINC CAAR INTERSECTIONV;
         PRINTC " as main because some poly is linear in it" >>;
       X := GCDLIST3(FOR EACH P IN L COLLECT REORDER CAR P,NIL,VORD);
       SETKORDER L1;
       RETURN REORDER X >>
    ELSE IF NULL UNIONV THEN RETURN GCDLIST2(L,INTERSECTIONV);
    TRACE!-TIME <<
      PRINC "The variables "; PRINC UNIONV; PRINTC " can be removed" >>;
    VORD := SETKORDER MAPCARCAR APPEND(UNIONV,INTERSECTIONV);
    L1 := NIL;
    FOR EACH P IN L DO
        L1:=SPLIT!-WRT!-VARIABLES(REORDER CAR P,MAPCARCAR UNIONV,L1);
    SETKORDER VORD;
    RETURN GCDLIST1(FOR EACH P IN L1 COLLECT
      (REORDER P . TOTAL!-DEGREE!-IN!-POWERS(P,NIL)))
  END;


SYMBOLIC PROCEDURE GCDLIST2(L,VARS);
% Here all the variables in VARS are used in every polynomial
% in L. Select a good variable ordering;
  BEGIN
    SCALAR X,X1,GG,LMODP,ONESTEP,VORD,OLDMOD,IMAGE!-SET,GCDPOW,
	   UNLUCKY!-CASE;
% In the univariate case I do not need to think very hard about
% the selection of a main variable!! ;
    IF NULL CDR VARS
      THEN RETURN GCDLIST3(MAPCARCAR L,NIL,LIST CAAR VARS);
    OLDMOD := SET!-MODULUS NIL;
% If some variable appears at most to degree two in some pair
% of the polynomials then that will do as a main variable;
    VARS := MAPCARCAR SORT(VARS,FUNCTION GREATERPCDR);
% Vars is now arranged with the variable that appears to highest
% degree anywhere in L first, and the rest in descending order;
    L := FOR EACH P IN L COLLECT CAR P .
      SORT(CDR P,FUNCTION LESSPCDR);
    L := SORT(L,FUNCTION LESSPCDADR);
% Each list of degree information in L is sorted with lowest degree
% vars first, and the polynomial with the lowest degree variable
% of all will come first;
    X := INTERSECTION(DEG2VARS(CDAR L),DEG2VARS(CDADR L));
    IF NOT NULL X THEN <<
       TRACE!-TIME << PRINC "Two inputs are at worst quadratic in ";
                      PRINTC CAR X >>;
      GO TO X!-TO!-TOP >>;   % Here I have found two polys with a common
                             % variable that they are quadratic in;
% Now generate modular images of the gcd to guess its degree wrt
% all possible variables;

% If either (a) modular gcd=1 or (b) modular gcd can be computed with
% just 1 reduction step, use that information to choose a main variable;
TRY!-AGAIN:  % Modular images may be degenerate;
    SET!-MODULUS RANDOM!-PRIME();
    UNLUCKY!-CASE := NIL;
    IMAGE!-SET := FOR EACH V IN VARS
		     COLLECT (V . MODULAR!-NUMBER RANDOM());
    TRACE!-TIME <<
      PRINC "Select variable ordering using P=";
      PRINC CURRENT!-MODULUS;
      PRINC " and substitutions from ";
      PRINTC IMAGE!-SET >>;
    X1 := VARS;
TRY!-VARS:
    IF NULL X1 THEN GO TO IMAGES!-TRIED;
    LMODP := FOR EACH P IN L COLLECT MAKE!-IMAGE!-MOD!-P(CAR P,CAR X1);
    IF UNLUCKY!-CASE THEN GO TO TRY!-AGAIN;
    LMODP := SORT(LMODP,FUNCTION LESSPDEG);
    GG := GCDLIST!-MOD!-P(CAR LMODP,CDR LMODP);
    IF DOMAINP GG OR (REDUCTION!-COUNT<2 AND (ONESTEP:=T)) THEN <<
           TRACE!-TIME << PRINC "Select "; PRINTC CAR X1 >>;
           X := LIST CAR X1; GO TO X!-TO!-TOP >>;
    GCDPOW := (CAR X1 . LDEG GG) . GCDPOW;
    X1 := CDR X1;
    GO TO TRY!-VARS;
IMAGES!-TRIED:
  % In default of anything better to do, use image variable such that
  % degree of gcd wrt it is as large as possible;
    VORD := MAPCARCAR SORT(GCDPOW,FUNCTION GREATERPCDR);
    TRACE!-TIME << PRINC "Select order by degrees: ";
                   PRINTC GCDPOW >>;
    GO TO ORDER!-CHOSEN;

X!-TO!-TOP:
    FOR EACH V IN X DO VARS := DELETE(V,VARS);
    VORD := APPEND(X,VARS);
ORDER!-CHOSEN:
    TRACE!-TIME << PRINC "Selected Var order = "; PRINTC VORD >>;
    SET!-MODULUS OLDMOD;
    VARS := SETKORDER VORD;
    X := GCDLIST3(FOR EACH P IN L COLLECT REORDER CAR P,ONESTEP,VORD);
    SETKORDER VARS;
    RETURN REORDER X
  END;

SYMBOLIC PROCEDURE GCDLIST!-MOD!-P(GG,L);
   IF NULL L THEN GG
   ELSE IF GG=1 THEN 1
   ELSE GCDLIST!-MOD!-P(GCD!-MOD!-P(GG,CAR L),CDR L);



SYMBOLIC PROCEDURE DEG2VARS L;
    IF NULL L THEN NIL
    ELSE IF CDAR L>2 THEN NIL
    ELSE CAAR L . DEG2VARS CDR L;

SYMBOLIC PROCEDURE VDELETE(A,B);
    IF NULL B THEN NIL
    ELSE IF CAR A=CAAR B THEN CDR B
    ELSE CAR B . VDELETE(A,CDR B);

SYMBOLIC PROCEDURE INTERSECTION(U,V);
    IF NULL U THEN NIL
    ELSE IF MEMBER(CAR U,V) THEN CAR U . INTERSECTION(CDR U,V)
    ELSE INTERSECTION(CDR U,V);


SYMBOLIC PROCEDURE VINTERSECTION(A,B);
  BEGIN
    SCALAR C;
    RETURN IF NULL A THEN NIL
    ELSE IF NULL (C:=ASSOC(CAAR A,B)) THEN VINTERSECTION(CDR A,B)
    ELSE IF CDAR A>CDR C THEN
      IF CDR C=0 THEN VINTERSECTION(CDR A,B)
      ELSE C . VINTERSECTION(CDR A,B)
    ELSE IF CDAR A=0 THEN VINTERSECTION(CDR A,B)
    ELSE CAR A . VINTERSECTION(CDR A,B)
  END;


SYMBOLIC PROCEDURE VUNION(A,B);
  BEGIN
    SCALAR C;
    RETURN IF NULL A THEN B
    ELSE IF NULL (C:=ASSOC(CAAR A,B)) THEN CAR A . VUNION(CDR A,B)
    ELSE IF CDAR A>CDR C THEN CAR A . VUNION(CDR A,DELETE(C,B))
    ELSE C . VUNION(CDR A,DELETE(C,B))
  END;


SYMBOLIC PROCEDURE MAPCARCAR L;
    FOR EACH X IN L COLLECT CAR X;


SYMBOLIC PROCEDURE GCDLD(L,N);
% GCD of the domain element N and all the polys in L;
    IF N=1 OR N=-1 THEN 1
    ELSE IF L=NIL THEN ABS N
    ELSE IF CAR L=NIL THEN GCDLD(CDR L,N)
    ELSE GCDLD(CDR L,GCD!-WITH!-NUMBER(N,CAR L));

SYMBOLIC PROCEDURE SPLIT!-WRT!-VARIABLES(P,VL,L);
% Push all the coeffs in P wrt variables in VL onto the list L
% Stop if 1 is found as a coeff;
    IF P=NIL THEN L
    ELSE IF NOT NULL L AND CAR L=1 THEN L
    ELSE IF DOMAINP P THEN ABS P . L
    ELSE IF MEMBER(MVAR P,VL) THEN
        SPLIT!-WRT!-VARIABLES(RED P,VL,SPLIT!-WRT!-VARIABLES(LC P,VL,L))
    ELSE P . L;


SYMBOLIC PROCEDURE GCDLIST3(L,ONESTEP,VLIST);
% GCD of the nontrivial polys in the list L given that they all
% involve all the variables that any of them mention,
% and they are all monomial-primitive.
% ONESTEP is true if it is predicted that only one PRS step
% will be needed to compute the gcd - if so try that PRS step;
  BEGIN
    SCALAR OLD!-MODULUS,PRIME,UNLUCKY!-CASE,IMAGE!-SET,GG,GCONT,
	  COFACTOR,ZEROS!-LIST,L1,W,LCG,W1,REDUCED!-DEGREE!-LCLST,P1,P2;
    % Make all the polys primitive;
    L1:=FOR EACH P IN L COLLECT P . EZGCD!-COMFAC P;
    L:=FOR EACH C IN L1 COLLECT
        QUOTFAIL1(CAR C,COMFAC!-TO!-POLY CDR C,
                  "Content divison in GCDLIST3 failed");
    % All polys in L are now primitive;
    % Because all polys were monomial-primitive, there should
    % be no power of V to go in the result;
    GCONT:=GCDLIST FOR EACH C IN L1 COLLECT CDDR C;
    IF DOMAINP GCONT THEN IF NOT GCONT=1
      THEN ERRORF "GCONT has numeric part";
    % GCD of contents complete now;
    IF DOMAINP (GG:=CAR (L:=SORT(L,FUNCTION DEGREE!-ORDER))) THEN
      RETURN GCONT;
	 % Primitive part of one poly is a constant (must be +/-1);
    IF LDEG GG=1 THEN <<
    % True gcd is either GG or 1;
       IF DIVISION!-TEST(GG,L) THEN RETURN MULTF(POLY!-ABS GG,GCONT)
       ELSE RETURN GCONT >>;
    % All polys are now primitive and nontrivial. Use a modular
    % method to extract GCD;
    IF ONESTEP THEN <<
       % Try to take gcd in just one pseudoremainder step, because some
       % previous modular test suggests it may be possible;
       P1 := POLY!-ABS CAR L; P2 := POLY!-ABS CADR L;
       IF P1=P2 THEN <<
             IF DIVISION!-TEST(P1,CDDR L) THEN RETURN MULTF(P1,GCONT) >>
       ELSE <<
       TRACE!-TIME PRINTC "Just one pseudoremainder step needed?";
       GG := POLY!-GCD(LC P1,LC P2);
       GG := EZGCD!-PP ADDF(MULTF(RED P1,
           QUOTFAIL1(LC P2,GG,
	"Division failure when just one pseudoremainder step needed")),
	MULTF(RED P2,NEGF QUOTFAIL1(LC P1,GG,
	"Division failure when just one pseudoremainder step needed")));
       TRACE!-TIME FAC!-PRINTSF GG;
       IF DIVISION!-TEST(GG,L) THEN RETURN MULTF(GG,GCONT) >>
       >>;
    OLD!-MODULUS:=SET!-MODULUS NIL; %Remember modulus;
    LCG:=FOR EACH POLY IN L COLLECT LC POLY;
     TRACE!-TIME << PRINTC "L.C.S OF L ARE:";
       FOR EACH LCPOLY IN LCG DO FAC!-PRINTSF LCPOLY >>;
    LCG:=GCDLIST LCG;
     TRACE!-TIME << PRIN2!* "LCG (=GCD OF THESE) = ";
       FAC!-PRINTSF LCG >>;
TRY!-AGAIN:
    UNLUCKY!-CASE:=NIL;
    IMAGE!-SET:=NIL;
    SET!-MODULUS(PRIME:=RANDOM!-PRIME());
    % Produce random univariate modular images of all the
    % polynomials;
    W:=L;
    IF NOT ZEROS!-LIST THEN <<
      IMAGE!-SET:=
	 ZEROS!-LIST:=TRY!-MAX!-ZEROS!-FOR!-IMAGE!-SET(W,VLIST);
      TRACE!-TIME << PRINTC IMAGE!-SET;
        PRINC " Zeros-list = ";
        PRINTC ZEROS!-LIST >> >>;
    TRACE!-TIME PRINTC LIST("IMAGE SET",IMAGE!-SET);
    GG:=MAKE!-IMAGE!-MOD!-P(CAR W,CAR VLIST);
    TRACE!-TIME PRINTC LIST("IMAGE SET",IMAGE!-SET," GG",GG);
    IF UNLUCKY!-CASE THEN <<
      TRACE!-TIME << PRINTC "Unlucky case, try again";
        PRINT IMAGE!-SET >>;
      GO TO TRY!-AGAIN >>;
    L1:=LIST(CAR W . GG);
MAKE!-IMAGES:
    IF NULL (W:=CDR W) THEN GO TO IMAGES!-CREATED!-SUCCESSFULLY;
    L1:=(CAR W . MAKE!-IMAGE!-MOD!-P(CAR W,CAR VLIST)) . L1;
    IF UNLUCKY!-CASE THEN <<
     TRACE!-TIME << PRINTC "UNLUCKY AGAIN...";
       PRINTC L1;
       PRINT IMAGE!-SET >>;
      GO TO TRY!-AGAIN >>;
    GG:=GCD!-MOD!-P(GG,CDAR L1);
    IF DOMAINP GG THEN <<
      SET!-MODULUS OLD!-MODULUS;
      TRACE!-TIME PRINT "Primitive parts are coprime";
      RETURN GCONT >>;
    GO TO MAKE!-IMAGES;
IMAGES!-CREATED!-SUCCESSFULLY:
    L1:=REVERSEWOC L1; % Put back in order with smallest first;
    % If degree of gcd seems to be same as that of smallest item
    % in input list, that item should be the gcd;
    IF LDEG GG=LDEG CAR L THEN <<
        GG:=POLY!-ABS CAR L;
        TRACE!-TIME <<
          PRIN2!* "Probable GCD = ";
	  FAC!-PRINTSF GG >>;
        GO TO RESULT >>
    ELSE IF (LDEG CAR L=ADD1 LDEG GG) AND
            (LDEG CAR L=LDEG CADR L) THEN <<
    % Here it seems that I have just one pseudoremainder step to
    % perform, so I might as well do it;
        TRACE!-TIME <<
           PRINTC "Just one pseudoremainder step needed"
           >>;
        GG := POLY!-GCD(LC CAR L,LC CADR L);
        GG := EZGCD!-PP ADDF(MULTF(RED CAR L,
            QUOTFAIL1(LC CADR L,GG,
	 "Division failure when just one pseudoremainder step needed")),
	 MULTF(RED CADR L,NEGF QUOTFAIL1(LC CAR L,GG,
	 "Divison failure when just one pseudoremainder step needed")));
	TRACE!-TIME FAC!-PRINTSF GG;
        GO TO RESULT >>;
    W:=L1;
FIND!-GOOD!-COFACTOR:
    IF NULL W THEN GO TO SPECIAL!-CASE; % No good cofactor available;
    IF DOMAINP GCD!-MOD!-P(GG,COFACTOR:=QUOTIENT!-MOD!-P(CDAR W,GG))
      THEN GO TO GOOD!-COFACTOR!-FOUND;
    W:=CDR W;
    GO TO FIND!-GOOD!-COFACTOR;
GOOD!-COFACTOR!-FOUND:
    COFACTOR:=MONIC!-MOD!-P COFACTOR;
    TRACE!-TIME PRINTC "*** Good cofactor found";
    W:=CAAR W;
     TRACE!-TIME << PRIN2!* "W= ";
       FAC!-PRINTSF W;
       PRIN2!* "GG= ";
       FAC!-PRINTSF GG;
       PRIN2!* "COFACTOR= ";
       FAC!-PRINTSF COFACTOR >>;
    IMAGE!-SET:=SORT(IMAGE!-SET,FUNCTION ORDOPCAR);
     TRACE!-TIME << PRINC "IMAGE-SET = ";
       PRINTC IMAGE!-SET;
       PRINC "PRIME= ";   PRINTC PRIME;
       PRINTC "L (=POLYLIST) IS:";
       FOR EACH LL IN L DO FAC!-PRINTSF LL >>;
    GG:=RECONSTRUCT!-GCD(W,GG,COFACTOR,L,PRIME,IMAGE!-SET,LCG);
    IF GG='NOGOOD THEN GOTO TRY!-AGAIN;
    GO TO RESULT;
SPECIAL!-CASE: % Here I have to do the first step of a PRS method;
    TRACE!-TIME << PRINTC "*** SPECIAL CASE IN GCD ***";
      PRINTC L;
      PRINTC "----->";
      PRINTC GG >>;
    REDUCED!-DEGREE!-LCLST:=NIL;
TRY!-REDUCED!-DEGREE!-AGAIN:
    TRACE!-TIME << PRINTC "L1 =";
      FOR EACH ELL IN L1 DO PRINT ELL >>;
    W1:=REDUCED!-DEGREE(CAADR L1,CAAR L1);
    W:=CAR W1; W1:=CDR W1;
    TRACE!-TIME << PRINC "REDUCED!-DEGREE = "; FAC!-PRINTSF W;
      PRINC " and its image = "; FAC!-PRINTSF W1 >>;
            % reduce the degree of the 2nd poly using the 1st. Result is
            % a pair : (new poly . image new poly);
    IF DOMAINP W AND NOT NULL W THEN <<
      SET!-MODULUS OLD!-MODULUS; RETURN GCONT >>;
            % we're done as they're coprime;
    IF W AND LDEG W = LDEG GG THEN <<
      GG:=W; GO TO RESULT >>;
            % possible gcd;
    IF NULL W THEN <<
            % the first poly divided the second one;
      L1:=(CAR L1 . CDDR L1);  % discard second poly;
      IF NULL CDR L1 THEN <<
         GG := POLY!-ABS CAAR L1;
         GO TO RESULT >>;
      GO TO TRY!-REDUCED!-DEGREE!-AGAIN >>;
            % haven't made progress yet so repeat with new polys;
    IF LDEG W<=LDEG GG THEN <<
       GG := POLY!-ABS W;
       GO TO RESULT >>
    ELSE IF DOMAINP GCD!-MOD!-P(GG,COFACTOR:=QUOTIENT!-MOD!-P(W1,GG))
     THEN <<
       W := LIST LIST W;
       GO TO GOOD!-COFACTOR!-FOUND >>;
    L1:= IF LDEG W <= LDEG CAAR L1 THEN
      ((W . W1) . (CAR L1 . CDDR L1))
      ELSE (CAR L1 . ((W . W1) . CDDR L1));
            % replace first two polys by the reduced poly and the first
            % poly ordering according to degree;
    GO TO TRY!-REDUCED!-DEGREE!-AGAIN;
            % need to repeat as we still haven't found a good cofactor;
RESULT: % Here GG holds a tentative gcd for the primitive parts of
        % all input polys, and GCONT holds a proper one for the content;
    IF DIVISION!-TEST(GG,L) THEN <<
      SET!-MODULUS OLD!-MODULUS;
      RETURN MULTF(GG,GCONT) >>;
    TRACE!-TIME PRINTC LIST("Trial division by ",GG," failed");
    GO TO TRY!-AGAIN
  END;

GLOBAL '(KORD!*);

SYMBOLIC PROCEDURE MAKE!-A!-LIST!-OF!-VARIABLES L;
  BEGIN SCALAR VLIST;
    FOR EACH LL IN L DO VLIST:=VARIABLES!.IN!.FORM(LL,VLIST);
    RETURN MAKE!-ORDER!-CONSISTENT(VLIST,KORD!*)
  END;

SYMBOLIC PROCEDURE MAKE!-ORDER!-CONSISTENT(L,M);
% L is a subset of M. Make its order consistent with that
% of M;
    IF NULL L THEN NIL
    ELSE IF NULL M THEN ERRORF("Variable missing from KORD*")
    ELSE IF CAR M MEMBER L THEN CAR M .
       MAKE!-ORDER!-CONSISTENT(DELETE(CAR M,L),CDR M)
    ELSE MAKE!-ORDER!-CONSISTENT(L,CDR M);

SYMBOLIC PROCEDURE TRY!-MAX!-ZEROS!-FOR!-IMAGE!-SET(L,VLIST);
  IF NULL VLIST THEN ERROR(0,"VLIST NOT SET IN TRY-MAX-ZEROS-...")
  ELSE BEGIN SCALAR Z;
    Z:=FOR EACH V IN CDR VLIST COLLECT
      IF DOMAINP LC CAR L OR NULL QUOTF(LC CAR L,!*K2F V) THEN
        (V . 0) ELSE (V . MODULAR!-NUMBER RANDOM());
    FOR EACH FF IN CDR L DO
      Z:=FOR EACH W IN Z COLLECT
        IF ZEROP CDR W THEN
          IF DOMAINP LC FF OR NULL QUOTF(LC FF,!*K2F CAR W) THEN W
          ELSE (CAR W . MODULAR!-NUMBER RANDOM())
        ELSE W;
    RETURN Z
  END;

SYMBOLIC PROCEDURE RECONSTRUCT!-GCD(FULL!-POLY,GG,COFACTOR,POLYLIST,
                                    P,IMSET,LCG);
% ... ;
  IF NULL ADDF(FULL!-POLY,NEGF MULTF(GG,COFACTOR)) THEN GG
  ELSE (LAMBDA FACTOR!-LEVEL;
    BEGIN SCALAR NUMBER!-OF!-FACTORS,IMAGE!-FACTORS,
    TRUE!-LEADING!-COEFFTS,MULTIVARIATE!-INPUT!-POLY,
    IRREDUCIBLE,NON!-MONIC,BAD!-CASE,TARGET!-FACTOR!-COUNT,
    MULTIVARIATE!-FACTORS,HENSEL!-GROWTH!-SIZE,ALPHALIST,
    COEFFTS!-VECTORS,BEST!-KNOWN!-FACTORS,PRIME!-BASE,
    M!-IMAGE!-VARIABLE, RECONSTRUCTING!-GCD,FULL!-GCD;
    IF NOT(CURRENT!-MODULUS=P) THEN
      ERRORF("GCDLIST HAS NOT RESTORED THE MODULUS");
            % *WARNING* GCDLIST does not restore the modulus so
              % I had better reset it here!  ;
    IF POLY!-MINUSP LCG THEN ERROR(0,LIST("Negative GCD: ",LCG));
    FULL!-POLY:=POLY!-ABS FULL!-POLY;
    INITIALISE!-HENSEL!-FLUIDS(FULL!-POLY,GG,COFACTOR,P,LCG);
     TRACE!-TIME << PRINTC "TRUE LEADING COEFFTS ARE:";
       FOR I:=1:2 DO <<
	 FAC!-PRINTSF GETV(IMAGE!-FACTORS,I);
         PRIN2!* " WITH L.C.:";
	 FAC!-PRINTSF GETV(TRUE!-LEADING!-COEFFTS,I) >> >>;
    IF DETERMINE!-MORE!-COEFFTS()='DONE THEN
      RETURN FULL!-GCD;
    IF NULL ALPHALIST THEN ALPHALIST:=ALPHAS(2,
      LIST(GETV(IMAGE!-FACTORS,1),GETV(IMAGE!-FACTORS,2)),1);
    IF ALPHALIST='FACTORS! NOT! COPRIME THEN
      ERRORF LIST("image factors not coprime?",IMAGE!-FACTORS);
    IF NOT !*OVERVIEW THEN FACTOR!-TRACE <<
      PRINTSTR
	 "The following modular polynomials are chosen such that:";
      TERPRI();
      PRIN2!* "   a(2)*f(1) + a(1)*f(2) = 1 mod ";
      PRINTSTR HENSEL!-GROWTH!-SIZE;
      TERPRI();
      PRINTSTR "  where degree of a(1) < degree of f(1),";
      PRINTSTR "    and degree of a(2) < degree of f(2),";
      PRINTSTR "    and";
      FOR I:=1:2 DO <<
        PRIN2!* "    a("; PRIN2!* I; PRIN2!* ")=";
	FAC!-PRINTSF CDR GET!-ALPHA GETV(IMAGE!-FACTORS,I);
        PRIN2!* "and f("; PRIN2!* I; PRIN2!* ")=";
	FAC!-PRINTSF GETV(IMAGE!-FACTORS,I);
        TERPRI!* T >>
    >>;
    RECONSTRUCT!-MULTIVARIATE!-FACTORS(
      FOR EACH V IN IMSET COLLECT (CAR V . MODULAR!-NUMBER CDR V));
    IF IRREDUCIBLE OR BAD!-CASE THEN RETURN 'NOGOOD
    ELSE RETURN FULL!-GCD
  END) (FACTOR!-LEVEL+1) ;

SYMBOLIC PROCEDURE INITIALISE!-HENSEL!-FLUIDS(FPOLY,FAC1,FAC2,P,LCF1);
% ... ;
  BEGIN SCALAR LC1!-IMAGE,LC2!-IMAGE;
    RECONSTRUCTING!-GCD:=T;
    MULTIVARIATE!-INPUT!-POLY:=MULTF(FPOLY,LCF1);
    PRIME!-BASE:=HENSEL!-GROWTH!-SIZE:=P;
    NUMBER!-OF!-FACTORS:=2;
    LC1!-IMAGE:=MAKE!-NUMERIC!-IMAGE!-MOD!-P LCF1;
    LC2!-IMAGE:=MAKE!-NUMERIC!-IMAGE!-MOD!-P LC FPOLY;
% Neither of the above leading coefficients will vanish;
    FAC1:=TIMES!-MOD!-P(LC1!-IMAGE,FAC1);
    FAC2:=TIMES!-MOD!-P(LC2!-IMAGE,FAC2);
    IMAGE!-FACTORS:=MKVECT 2;
    TRUE!-LEADING!-COEFFTS:=MKVECT 2;
    PUTV(IMAGE!-FACTORS,1,FAC1);
    PUTV(IMAGE!-FACTORS,2,FAC2);
    PUTV(TRUE!-LEADING!-COEFFTS,1,LCF1);
    PUTV(TRUE!-LEADING!-COEFFTS,2,LC FPOLY);
    % If the GCD is going to be monic, we know the lc
    % of both cofactors exactly;
    NON!-MONIC:=NOT(LCF1=1);
    M!-IMAGE!-VARIABLE:=MVAR FPOLY
  END;

SYMBOLIC PROCEDURE DIVISION!-TEST(GG,L);
% Predicate to test if GG divides all the polynomials in the list L;
    IF NULL L THEN T
    ELSE IF NULL QUOTF(CAR L,GG) THEN NIL
    ELSE DIVISION!-TEST(GG,CDR L);



SYMBOLIC PROCEDURE DEGREE!-ORDER(A,B);
% Order standard forms using their degrees wrt main vars;
    IF DOMAINP A THEN T
    ELSE IF DOMAINP B THEN NIL
    ELSE LDEG A<LDEG B;


SYMBOLIC PROCEDURE MAKE!-IMAGE!-MOD!-P(P,V);
% Form univariate image, set UNLUCKY!-CASE if leading coefficient
% gets destroyed;
  BEGIN
    SCALAR LP;
    LP := DEGREE!-IN!-VARIABLE(P,V);
    P := MAKE!-UNIVARIATE!-IMAGE!-MOD!-P(P,V);
    IF NOT DEGREE!-IN!-VARIABLE(P,V)=LP THEN UNLUCKY!-CASE := T;
    RETURN P
  END;


SYMBOLIC PROCEDURE MAKE!-UNIVARIATE!-IMAGE!-MOD!-P(P,V);
% Make a modular image of P, keeping only the variable V;
  IF DOMAINP P THEN
     IF P=NIL THEN NIL
     ELSE !*N2F MODULAR!-NUMBER P
  ELSE IF MVAR P=V THEN
     ADJOIN!-TERM(LPOW P,
                  MAKE!-UNIVARIATE!-IMAGE!-MOD!-P(LC P,V),
                  MAKE!-UNIVARIATE!-IMAGE!-MOD!-P(RED P,V))
    ELSE PLUS!-MOD!-P(
      TIMES!-MOD!-P(IMAGE!-OF!-POWER(MVAR P,LDEG P),
                    MAKE!-UNIVARIATE!-IMAGE!-MOD!-P(LC P,V)),
      MAKE!-UNIVARIATE!-IMAGE!-MOD!-P(RED P,V));

SYMBOLIC PROCEDURE IMAGE!-OF!-POWER(V,N);
  BEGIN
    SCALAR W;
    W := ASSOC(V,IMAGE!-SET);
    IF NULL W THEN <<
       W := MODULAR!-NUMBER RANDOM();
       IMAGE!-SET := (V . W) . IMAGE!-SET >>
    ELSE W := CDR W;
    RETURN MODULAR!-EXPT(W,N)
  END;

SYMBOLIC PROCEDURE MAKE!-NUMERIC!-IMAGE!-MOD!-P P;
% Make a modular image of P;
  IF DOMAINP P THEN
     IF P=NIL THEN 0
     ELSE MODULAR!-NUMBER P
    ELSE MODULAR!-PLUS(
      MODULAR!-TIMES(IMAGE!-OF!-POWER(MVAR P,LDEG P),
                    MAKE!-NUMERIC!-IMAGE!-MOD!-P LC P),
      MAKE!-NUMERIC!-IMAGE!-MOD!-P RED P);


SYMBOLIC PROCEDURE TOTAL!-DEGREE!-IN!-POWERS(FORM,POWLST);
% Returns a list where each variable mentioned in FORM is paired
% with the maximum degree it has. POWLST collects the list, and should
% normally be NIL on initial entry;
  IF NULL FORM OR DOMAINP FORM THEN POWLST
  ELSE BEGIN SCALAR X;
    IF (X := ATSOC(MVAR FORM,POWLST))
      THEN LDEG FORM>CDR X AND RPLACD(X,LDEG FORM)
    ELSE POWLST := (MVAR FORM . LDEG FORM) . POWLST;
    RETURN TOTAL!-DEGREE!-IN!-POWERS(RED FORM,
      TOTAL!-DEGREE!-IN!-POWERS(LC FORM,POWLST))
  END;


SYMBOLIC PROCEDURE POWERS1 FORM;
% For each variable V in FORM collect (V . (MAX . MIN)) where
% MAX and MIN are limits to the degrees V has in FORM;
  POWERS2(FORM,POWERS3(FORM,NIL),NIL);

SYMBOLIC PROCEDURE POWERS3(FORM,L);
% Start of POWERS1 by collecting power information for
% the leading monomial in FORM;
    IF DOMAINP FORM THEN L
    ELSE POWERS3(LC FORM,(MVAR FORM . (LDEG FORM . LDEG FORM)) . L);

SYMBOLIC PROCEDURE POWERS2(FORM,POWLST,THISMONOMIAL);
    IF DOMAINP FORM THEN
        IF NULL FORM THEN POWLST ELSE POWERS4(THISMONOMIAL,POWLST)
    ELSE POWERS2(LC FORM,
                 POWERS2(RED FORM,POWLST,THISMONOMIAL),
                 LPOW FORM . THISMONOMIAL);

SYMBOLIC PROCEDURE POWERS4(NEW,OLD);
% Merge information from new monomial into old information,
% updating MAX and MIN details;
  IF NULL NEW THEN FOR EACH V IN OLD COLLECT (CAR V . (CADR V . 0))
  ELSE IF NULL OLD THEN FOR EACH V IN NEW COLLECT (CAR V . (CDR V . 0))
  ELSE IF CAAR NEW=CAAR OLD THEN <<
    % variables match - do MAX and MIN on degree information;
    IF CDAR NEW>CADAR OLD THEN RPLACA(CDAR OLD,CDAR NEW);
    IF CDAR NEW<CDDAR OLD THEN RPLACD(CDAR OLD,CDAR NEW);
    RPLACD(OLD,POWERS4(CDR NEW,CDR OLD)) >>
  ELSE IF ORDOP(CAAR NEW,CAAR OLD) THEN <<
    RPLACD(CDAR OLD,0); % Some variable not mentioned in new monomial;
    RPLACD(OLD,POWERS4(NEW,CDR OLD)) >>
  ELSE (CAAR NEW . (CDAR NEW  . 0)) . POWERS4(CDR NEW,OLD);


SYMBOLIC PROCEDURE EZGCD!-PP U; 
   %returns the primitive part of the polynomial U wrt leading var; 
   QUOTF1(U,COMFAC!-TO!-POLY EZGCD!-COMFAC U); 
 
SYMBOLIC PROCEDURE EZGCD!-SQFRF P;
   %P is a primitive standard form;
   %value is a list of square free factors;
  BEGIN
    SCALAR PDASH,P1,D,V;
    PDASH := DIFF(P,V := MVAR P);
    D := POLY!-GCD(P,PDASH); % p2*p3**2*p4**3*... ;
    IF DOMAINP D THEN RETURN LIST P;
    P := QUOTFAIL1(P,D,"GCD division in FACTOR-SQFRF failed");
    P1 := POLY!-GCD(P,
       ADDF(QUOTFAIL1(PDASH,D,"GCD division in FACTOR-SQFRF failed"),
            NEGF DIFF(P,V)));
    RETURN P1 . EZGCD!-SQFRF D
  END;

SYMBOLIC PROCEDURE REDUCED!-DEGREE(U,V);
   %U and V are primitive polynomials in the main variable VAR;
   %result is pair: (reduced poly of U by V . its image) where by
   % reduced I mean using V to kill the leading term of U;
   BEGIN SCALAR VAR,W,X;
    TRACE!-TIME << PRINTC "ARGS FOR REDUCED!-DEGREE ARE:";
     FAC!-PRINTSF U;  FAC!-PRINTSF V >>;
    IF U=V OR QUOTF1(U,V) THEN RETURN (NIL . NIL)
    ELSE IF LDEG V=1 THEN RETURN (1 . 1);
    TRACE!-TIME PRINTC "CASE NON-TRIVIAL SO TAKE A REDUCED!-DEGREE:";
    VAR := MVAR U;
    IF LDEG U=LDEG V THEN X := NEGF LC U
    ELSE X:=(MKSP(VAR,LDEG U - LDEG V) .* NEGF LC U) .+ NIL;
    W:=ADDF(MULTF(LC V,U),MULTF(X,V));
    TRACE!-TIME FAC!-PRINTSF W;
    IF DEGR(W,VAR)=0 THEN RETURN (1 . 1);
    TRACE!-TIME << PRINC "REDUCED!-DEGREE-LCLST = ";
      PRINT REDUCED!-DEGREE!-LCLST >>;
    REDUCED!-DEGREE!-LCLST := ADDLC(V,REDUCED!-DEGREE!-LCLST);
    TRACE!-TIME << PRINC "REDUCED!-DEGREE-LCLST = ";
      PRINT REDUCED!-DEGREE!-LCLST >>;
    IF X := QUOTF1(W,LC W) THEN W := X
    ELSE FOR EACH Y IN REDUCED!-DEGREE!-LCLST DO
      WHILE (X := QUOTF1(W,Y)) DO W := X;
    U := V; V := EZGCD!-PP W;
    TRACE!-TIME << PRINTC "U AND V ARE NOW:";
      FAC!-PRINTSF U; FAC!-PRINTSF V >>;
    IF DEGR(V,VAR)=0 THEN RETURN (1 . 1)
    ELSE RETURN (V . MAKE!-UNIVARIATE!-IMAGE!-MOD!-P(V,VAR))
  END;


MOVED('COMFAC,'EZGCD!-COMFAC);

MOVED('PP,'EZGCD!-PP);



ENDMODULE;


MODULE FACMISC;

% *******************************************************************
%
%   copyright (c)  university of cambridge, england 1979
%
% *******************************************************************;





%**********************************************************************;
%         miscellaneous routines used from several sections            ;
%**********************************************************************;



% (1) investigate variables in polynomial;





SYMBOLIC PROCEDURE MULTIVARIATEP(A,V);
    IF DOMAINP A THEN NIL
    ELSE IF NOT(MVAR A EQ V) THEN T
    ELSE IF MULTIVARIATEP(LC A,V) THEN T
    ELSE MULTIVARIATEP(RED A,V);


SYMBOLIC PROCEDURE VARIABLES!-IN!-FORM A;
% collect variables that occur in the form a;
    VARIABLES!.IN!.FORM(A,NIL);

SYMBOLIC PROCEDURE GET!.COEFFT!.BOUND(POLY,DEGBD);
% calculates a coefft bound for the factors of poly. this simple
% bound is that suggested by paul wang and linda p. rothschild in
% math.comp.vol29 july 75 p.940 due to gel'fond;
% Note that for tiny polynomials the bound is forced up to be
% larger than any prime that will get used in the mod-p splitting;
  MAX(GET!-HEIGHT POLY * FIXEXPFLOAT SUMOF DEGBD,110);

SYMBOLIC PROCEDURE SUMOF DEGBD;
  IF NULL DEGBD THEN 0
  ELSE CDAR DEGBD + SUMOF CDR DEGBD;

SYMBOLIC PROCEDURE FIXEXPFLOAT N;
% Compute exponential function e**n for potentially large N,
% rounding result up somewhat. Note that exp(13)=442413 or so,
% so if the basic floating point exponential function is accurate
% to 6 or so digits we are protected here against roundoff;
  IF N>13 THEN BEGIN
     SCALAR N2;
     N2 := N/2;
     RETURN FIXEXPFLOAT(N2)*FIXEXPFLOAT(N-N2)
  END
  ELSE 2+FIX EXP FLOAT N;


% (2) timer services;


SYMBOLIC PROCEDURE SET!-TIME();
 << LAST!-DISPLAYED!-TIME:=BASE!-TIME:=READTIME();
    LAST!-DISPLAYED!-GC!-TIME:=GC!-BASE!-TIME:=READGCTIME();
    NIL >>;


GLOBAL '(!*TEST);   %not really supported in REDUCE anymore;

SYMBOLIC PROCEDURE PRINT!-TIME M;
% display time used so far, with given message;
  BEGIN SCALAR TOTAL,INCR,GCTOTAL,GCINCR,W;
    IF NOT !*TEST THEN RETURN NIL;
    W:=READTIME();
    TOTAL:=W-BASE!-TIME;
    INCR:=W-LAST!-DISPLAYED!-TIME;
    LAST!-DISPLAYED!-TIME:=W;
    W:=READGCTIME();
    GCTOTAL:=W-GC!-BASE!-TIME;
    GCINCR:=W-LAST!-DISPLAYED!-GC!-TIME;
    LAST!-DISPLAYED!-GC!-TIME:=W;
    IF ATOM M THEN PRINC M ELSE <<
        PRINC CAR M;
        M:=CDR M;
        WHILE NOT ATOM M DO << PRINC '! ; PRINC CAR M; M:=CDR M >>;
        IF NOT NULL M THEN << PRINC '! ; PRINC M >> >>;
    PRINC " after ";
    PRINMILLI INCR;
    PRINC "+";
    PRINMILLI GCINCR;
    PRINC " seconds (total = ";
    PRINMILLI TOTAL;
    PRINC "+";
    PRINMILLI GCTOTAL;
    PRINC ")";
    TERPRI()
  END;


SYMBOLIC PROCEDURE PRINMILLI N;
% print n/1000 as a decimal fraction with 2 decimal places;
  BEGIN
    SCALAR U,D1,D01;
    N:=N+5; %rounding;
    N:=QUOTIENT(N,10); %now centiseconds;
    N:=DIVIDE(N,10);
    D01:=CDR N;
    N:=CAR N;
    N:=DIVIDE(N,10);
    D1:=CDR N;
    U:=CAR N;
    PRINC U;
    PRINC '!.;
    PRINC D1;
    PRINC D01;
    RETURN NIL
  END;




% (3) minor variations on ordinary algebraic operations;

SYMBOLIC PROCEDURE QUOTFAIL(A,B);
% version of quotf that fails if the division does;
  IF POLYZEROP A THEN POLYZERO
  ELSE BEGIN SCALAR W;
    W:=QUOTF(A,B);
    IF DIDNTGO W THEN ERRORF LIST("UNEXPECTED DIVISION FAILURE",A,B)
    ELSE RETURN W
  END;

SYMBOLIC PROCEDURE QUOTFAIL1(A,B,MSG);
% version of quotf that fails if the division does, and gives
% custom message;
  IF POLYZEROP A THEN POLYZERO
  ELSE BEGIN SCALAR W;
    W:=QUOTF(A,B);
    IF DIDNTGO W THEN ERRORF MSG
    ELSE RETURN W
  END;



% (4) pseudo-random prime numbers - small and large;


GLOBAL '(TEENY!-PRIMES);

SYMBOLIC PROCEDURE SET!-TEENY!-PRIMES();
  BEGIN SCALAR I;
    I:=-1;
    TEENY!-PRIMES:=MKVECT 9;
    PUTV(TEENY!-PRIMES,I:=IADD1 I,3);
    PUTV(TEENY!-PRIMES,I:=IADD1 I,5);
    PUTV(TEENY!-PRIMES,I:=IADD1 I,7);
    PUTV(TEENY!-PRIMES,I:=IADD1 I,11);
    PUTV(TEENY!-PRIMES,I:=IADD1 I,13);
    PUTV(TEENY!-PRIMES,I:=IADD1 I,17);
    PUTV(TEENY!-PRIMES,I:=IADD1 I,19);
    PUTV(TEENY!-PRIMES,I:=IADD1 I,23);
    PUTV(TEENY!-PRIMES,I:=IADD1 I,29);
    PUTV(TEENY!-PRIMES,I:=IADD1 I,31)
  END;

SET!-TEENY!-PRIMES();


SYMBOLIC PROCEDURE RANDOM!-SMALL!-PRIME();
  BEGIN
    SCALAR P;
    P:=ILOGOR(1,SMALL!-RANDOM!-NUMBER());
    WHILE NOT PRIMEP P DO
       P:=ILOGOR(1,SMALL!-RANDOM!-NUMBER());
    RETURN P
  END;

SYMBOLIC PROCEDURE SMALL!-RANDOM!-NUMBER();
% Returns a number in the range 3 to 103 with a distribution
% favouring smaller numbers;
  BEGIN
    SCALAR W;
    W:=REMAINDER(RANDOM(),2000);
    W:=TIMES(W,W); % In range 0 to about 4 million;
    RETURN IPLUS(3,W/40000)
  END;

SYMBOLIC PROCEDURE RANDOM!-TEENY!-PRIME L;
% get one of the first 10 primes at random providing it is
% not in the list L or that L says we have tried them all;
  IF L='ALL OR (LENGTH L = 10) THEN NIL
  ELSE BEGIN SCALAR P;
AGAIN:
    P:=GETV(TEENY!-PRIMES,REMAINDER(RANDOM(),10));
    IF MEMBER(P,L) THEN GOTO AGAIN;
    RETURN P
  END;

SYMBOLIC PROCEDURE PRIMEP N;
% Test if prime. Only for use on small integers.
% Does not consider '2' to be a prime;
    IGREATERP(N,2) AND ILOGAND(N,1)=1 AND PRIMETEST(N,3);

SYMBOLIC PROCEDURE PRIMETEST(N,TRIAL);
    IF IGREATERP(ITIMES(TRIAL,TRIAL),N) THEN T
    ELSE IF IREMAINDER(N,TRIAL)=0 THEN NIL
    ELSE PRIMETEST(N,IPLUS(TRIAL,2));

GLOBAL '(BIT1AND23 PSEUDO!-PRIMES);
BIT1AND23:=LOGOR(1,LEFTSHIFT(1,23));

FLAG('(BIT1AND23 TWENTYFOURBITS),'CONSTANT);

% PSEUDO-PRIMES will be a list of all composite numbers which
% do not have a factor less than 68, and which are in the range
% 2**23 to 2**24 for which 2**(n-1)=1 mod n;

PSEUDO!-PRIMES:=MKVECT 121;
BEGIN
  SCALAR I,L;
  I:=0;
  L:= '(           8534233   8650951   8725753   8727391
         8745277   8902741   9006401   9037729   9040013
         9056501   9073513   9131401   9273547   9371251
         9480461   9533701   9564169   9567673   9588151
         9591661   9724177   9729301   9774181   9863461
        10024561  10031653  10084177  10251473  10266001
        10323769  10331141  10386241  10402237  10403641
        10425511  10505701  10545991  10610063  10700761
        10712857  10763653  10802017  10974881  11081459
        11115037  11335501  11367137  11541307  11585293
        11592397  11777599  12032021  12096613  12263131
        12273769  12322133  12327121  12376813  12407011
        12498061  12599233  12659989  12711007  12854437
        12932989  13057787  13073941  13295281  13338371
        13446253  13448593  13500313  13635289  13694761
        13747361  13773061  13838569  13856417  13991647
        13996951  14026897  14154337  14179537  14282143
        14324473  14469841  14589901  14671801  14676481
        14709241  14794081  14796289  14865121  14899751
        14980411  15082901  15101893  15139199  15188557
        15220951  15268501  15479777  15525241  15583153
        15603391  15621409  15700301  15732721  15757741
        15802681  15976747  15978007  16070429  16132321
        16149169  16324001  16349477  16360381  16435747
        16705021  16717061  16773121);
    WHILE L DO <<
       PUTV(PSEUDO!-PRIMES,I,CAR L);
       I:=I+1;
       L:=CDR L >>
  END;

SYMBOLIC PROCEDURE RANDOM!-PRIME();
  BEGIN
    SCALAR P,W,OLDMOD;
    IF TWENTYFOURBITS>LARGEST!-SMALL!-MODULUS THEN <<
	REPEAT
	   P := LOGOR(1,REMAINDER(RANDOM(),LARGEST!-SMALL!-MODULUS - 1))
	      UNTIL P*P>LARGEST!-SMALL!-MODULUS AND PRIMEP P;
        RETURN P >>;
    % W will become 1 when P is prime;
    OLDMOD := CURRENT!-MODULUS;
    WHILE NOT (W=1) DO <<
      % OR in bits 1 and 2**23 to make number odd and large;
      P:=LOGOR(BIT1AND23,LOGAND(TWENTYFOURBITS,RANDOM()));
		 % A random (odd) 24 bit integer;
      IF IREMAINDER(P,3)=0 OR IREMAINDER(P,5)=0 OR
         IREMAINDER(P,7)=0 OR IREMAINDER(P,11)=0 OR
         IREMAINDER(P,13)=0 OR IREMAINDER(P,17)=0 OR
         IREMAINDER(P,19)=0 OR IREMAINDER(P,23)=0 OR
         IREMAINDER(P,29)=0 OR IREMAINDER(P,31)=0 OR
         IREMAINDER(P,37)=0 OR IREMAINDER(P,41)=0 OR
         IREMAINDER(P,43)=0 OR IREMAINDER(P,47)=0 OR
         IREMAINDER(P,53)=0 OR IREMAINDER(P,59)=0 OR
         IREMAINDER(P,61)=0 OR IREMAINDER(P,67)=0 THEN W:=0
      ELSE <<
          SET!-MODULUS P;
          W:=MODULAR!-EXPT(2,ISUB1 P);
          IF W=1 AND PSEUDO!-PRIME!-P P THEN W:=0 >> >>;
    SET!-MODULUS OLDMOD;
    RETURN P
  END;

SYMBOLIC PROCEDURE PSEUDO!-PRIME!-P N;
  BEGIN
    SCALAR LOW,MID,HIGH,V;
    LOW:=0;
    HIGH:=121; % Size of vector of pseudo-primes;
    WHILE NOT (HIGH=LOW) DO << % Binary search in table;
      MID:=IRIGHTSHIFT(IPLUS(IADD1 HIGH,LOW),1);
	 % Mid point of (low,high);
      V:=GETV(PSEUDO!-PRIMES,MID);
      IF IGREATERP(V,N) THEN HIGH:=ISUB1 MID ELSE LOW:=MID >>;
    RETURN (GETV(PSEUDO!-PRIMES,LOW)=N)
  END;


% (5) usefull routines for vectors;


SYMBOLIC PROCEDURE FORM!-SUM!-AND!-PRODUCT!-MOD!-P(AVEC,FVEC,R);
% sum over i (avec(i) * fvec(i));
  BEGIN SCALAR S;
    S:=POLYZERO;
    FOR I:=1:R DO
      S:=PLUS!-MOD!-P(TIMES!-MOD!-P(GETV(AVEC,I),GETV(FVEC,I)),
        S);
    RETURN S
  END;

SYMBOLIC PROCEDURE FORM!-SUM!-AND!-PRODUCT!-MOD!-M(AVEC,FVEC,R);
% Same as above but AVEC holds alphas mod p and want to work
% mod m (m > p) so minor difference to change AVEC to AVEC mod m;
  BEGIN SCALAR S;
    S:=POLYZERO;
    FOR I:=1:R DO
      S:=PLUS!-MOD!-P(TIMES!-MOD!-P(
        !*F2MOD !*MOD2F GETV(AVEC,I),GETV(FVEC,I)),S);
    RETURN S
  END;

SYMBOLIC PROCEDURE REDUCE!-VEC!-BY!-ONE!-VAR!-MOD!-P(V,PT,N);
% substitute for the given variable in all elements creating a
% new vector for the result. (all arithmetic is mod p);
  BEGIN SCALAR NEWV;
    NEWV:=MKVECT N;
    FOR I:=1:N DO
      PUTV(NEWV,I,EVALUATE!-MOD!-P(GETV(V,I),CAR PT,CDR PT));
    RETURN NEWV
  END;

SYMBOLIC PROCEDURE MAKE!-BIVARIATE!-VEC!-MOD!-P(V,IMSET,VAR,N);
  BEGIN SCALAR NEWV;
    NEWV:=MKVECT N;
    FOR I:=1:N DO
      PUTV(NEWV,I,MAKE!-BIVARIATE!-MOD!-P(GETV(V,I),IMSET,VAR));
    RETURN NEWV
  END;

SYMBOLIC PROCEDURE TIMES!-VECTOR!-MOD!-P(V,N);
% product of all the elements in the vector mod p;
  BEGIN SCALAR W;
    W:=1;
    FOR I:=1:N DO W:=TIMES!-MOD!-P(GETV(V,I),W);
    RETURN W
  END;

SYMBOLIC PROCEDURE MAKE!-VEC!-MODULAR!-SYMMETRIC(V,N);
% fold each elt of V which is current a modular poly in the
% range 0->(p-1) onto the symmetric range (-p/2)->(p/2);
  FOR I:=1:N DO PUTV(V,I,MAKE!-MODULAR!-SYMMETRIC GETV(V,I));

% (6) Combinatorial fns used in finding values for the variables;


SYMBOLIC PROCEDURE MAKE!-ZEROVARSET VLIST;
% vlist is a list of pairs (v . tag) where v is a variable name and
% tag is a boolean tag. The procedure splits the list into two
% according to the tags: Zerovarset is set to a list of variables
% whose tag is false and othervars contains the rest;
  FOR EACH W IN VLIST DO
    IF CDR W THEN OTHERVARS:= CAR W . OTHERVARS
    ELSE ZEROVARSET:= CAR W . ZEROVARSET;

SYMBOLIC PROCEDURE MAKE!-ZEROSET!-LIST N;
% Produces a list of lists each of length n with all combinations of
% ones and zeroes;
  BEGIN SCALAR W;
    FOR K:=0:N DO W:=APPEND(W,KCOMBNS(K,N));
    RETURN W
  END;

SYMBOLIC PROCEDURE KCOMBNS(K,M);
% produces a list of all combinations of ones and zeroes with k ones
% in each;
  IF K=0 OR K=M THEN BEGIN SCALAR W;
    IF K=M THEN K:=1;
    FOR I:=1:M DO W:=K.W;
    RETURN LIST W
    END
  ELSE IF K=1 OR K=ISUB1 M THEN <<
    IF K=ISUB1 M THEN K:=0;
    LIST!-WITH!-ONE!-A(K,1 #- K,M) >>
  ELSE APPEND(
    FOR EACH X IN KCOMBNS(ISUB1 K,ISUB1 M) COLLECT (1 . X),
    FOR EACH X IN KCOMBNS(K,ISUB1 M) COLLECT (0 . X) );

SYMBOLIC PROCEDURE LIST!-WITH!-ONE!-A(A,B,M);
% Creates list of all lists with one a and m-1 b's in;
  BEGIN SCALAR W,X,R;
    FOR I:=1:ISUB1 M DO W:=B . W;
    R:=LIST(A . W);
    FOR I:=1:ISUB1 M DO <<
      X:=(CAR W) . X; W:=CDR W;
      R:=APPEND(X,(A . W)) . R >>;
    RETURN R
  END;

SYMBOLIC PROCEDURE MAKE!-NEXT!-ZSET L;
  BEGIN SCALAR K,W;
    IMAGE!-SET!-MODULUS:=IADD1 IMAGE!-SET!-MODULUS;
    SET!-MODULUS IMAGE!-SET!-MODULUS;
    W:=FOR EACH LL IN CDR L COLLECT
      FOR EACH N IN LL COLLECT
        IF N=0 THEN N
        ELSE <<
          K:=MODULAR!-NUMBER RANDOM();
          WHILE (ZEROP K) OR (ONEP K) DO
            K:=MODULAR!-NUMBER RANDOM();
          IF K>MODULUS!/2 THEN K:=K-CURRENT!-MODULUS;
           K >>;
    SAVE!-ZSET:=NIL;
    RETURN W
  END;


ENDMODULE;


MODULE FACMOD;

%**********************************************************************;
%
%   copyright (c)  university of cambridge, england 1979
%
%**********************************************************************;




%**********************************************************************;
%
%    modular factorization section
%;



%**********************************************************************;
%    modular factorization : discover the factor count mod p;




SAFE!-FLAG:=CARCHECK 0; % For speed of array access - important here;


SYMBOLIC PROCEDURE GET!-FACTOR!-COUNT!-MOD!-P
                              (N,POLY!-MOD!-P,P,X!-IS!-FACTOR);
% gets the factor count mod p from the nth image using the
% first half of Berlekamp's method;
  BEGIN SCALAR OLD!-M,F!-COUNT,WTIME;
    OLD!-M:=SET!-MODULUS P;
%    PRINC "prime = ";% PRINTC CURRENT!-MODULUS;
%    PRINC "degree = ";% PRINTC LDEG POLY!-MOD!-P;
    TRACE!-TIME DISPLAY!-TIME("Entered GET-FACTOR-COUNT after ",TIME());
    WTIME:=TIME();
    F!-COUNT:=MODULAR!-FACTOR!-COUNT();
    TRACE!-TIME DISPLAY!-TIME("Factor count obtained in ",TIME()-WTIME);
    SPLIT!-LIST:=
      ((IF X!-IS!-FACTOR THEN CAR F!-COUNT#+1 ELSE CAR F!-COUNT) . N)
        . SPLIT!-LIST;
    PUTV(MODULAR!-INFO,N,CDR F!-COUNT);
    SET!-MODULUS OLD!-M
  END;

SYMBOLIC PROCEDURE MODULAR!-FACTOR!-COUNT();
  BEGIN
    SCALAR POLY!-VECTOR,WVEC1,WVEC2,X!-TO!-P,
      N,WTIME,W,LIN!-F!-COUNT,NULL!-SPACE!-BASIS;
    KNOWN!-FACTORS:=NIL;
    DPOLY:=LDEG POLY!-MOD!-P;
    WVEC1:=MKVECT (2#*DPOLY);
    WVEC2:=MKVECT (2#*DPOLY);
    X!-TO!-P:=MKVECT DPOLY;
    POLY!-VECTOR:=MKVECT DPOLY;
    FOR I:=0:DPOLY DO PUTV(POLY!-VECTOR,I,0);
    POLY!-TO!-VECTOR POLY!-MOD!-P;
    W:=COUNT!-LINEAR!-FACTORS!-MOD!-P(WVEC1,WVEC2,X!-TO!-P);
    LIN!-F!-COUNT:=CAR W;
    IF DPOLY#<4 THEN RETURN
       (IF DPOLY=0 THEN LIN!-F!-COUNT
        ELSE LIN!-F!-COUNT#+1) .
        LIST(LIN!-F!-COUNT . CADR W,
             DPOLY . POLY!-VECTOR,
             NIL);
% When I use Berlekamp I certainly know that the polynomial
% involved has no linear factors;
    WTIME:=TIME();
    NULL!-SPACE!-BASIS:=USE!-BERLEKAMP(X!-TO!-P,CADDR W,WVEC1);
    TRACE!-TIME DISPLAY!-TIME("Berlekamp done in ",TIME()-WTIME);
    N:=LIN!-F!-COUNT #+ LENGTH NULL!-SPACE!-BASIS #+ 1;
            % there is always 1 more factor than the number of
            % null vectors we have picked up;
    RETURN N . LIST(
     LIN!-F!-COUNT . CADR W,
     DPOLY . POLY!-VECTOR,
     NULL!-SPACE!-BASIS)
  END;

%**********************************************************************;
% Extraction of linear factors is done specially;

SYMBOLIC PROCEDURE COUNT!-LINEAR!-FACTORS!-MOD!-P(WVEC1,WVEC2,X!-TO!-P);
% Compute gcd(x**p-x,u). It will be the product of all the
% linear factors of u mod p;
  BEGIN SCALAR DX!-TO!-P,LIN!-F!-COUNT,LINEAR!-FACTORS;
    FOR I:=0:DPOLY DO PUTV(WVEC2,I,GETV(POLY!-VECTOR,I));
    DX!-TO!-P:=MAKE!-X!-TO!-P(CURRENT!-MODULUS,WVEC1,X!-TO!-P);
    FOR I:=0:DX!-TO!-P DO PUTV(WVEC1,I,GETV(X!-TO!-P,I));
    IF DX!-TO!-P#<1 THEN <<
        IF DX!-TO!-P#<0 THEN PUTV(WVEC1,0,0);
        PUTV(WVEC1,1,MODULAR!-MINUS 1);
        DX!-TO!-P:=1 >>
    ELSE <<
      PUTV(WVEC1,1,MODULAR!-DIFFERENCE(GETV(WVEC1,1),1));
      IF DX!-TO!-P=1 AND GETV(WVEC1,1)=0 THEN
         IF GETV(WVEC1,0)=0 THEN DX!-TO!-P:=-1
         ELSE DX!-TO!-P:=0 >>;
    IF DX!-TO!-P#<0 THEN
      LIN!-F!-COUNT:=COPY!-VECTOR(WVEC2,DPOLY,WVEC1)
    ELSE LIN!-F!-COUNT:=GCD!-IN!-VECTOR(WVEC1,DX!-TO!-P,
      WVEC2,DPOLY);
    LINEAR!-FACTORS:=MKVECT LIN!-F!-COUNT;
    FOR I:=0:LIN!-F!-COUNT DO
      PUTV(LINEAR!-FACTORS,I,GETV(WVEC1,I));
    DPOLY:=QUOTFAIL!-IN!-VECTOR(POLY!-VECTOR,DPOLY,
        LINEAR!-FACTORS,LIN!-F!-COUNT);
    RETURN LIST(LIN!-F!-COUNT,LINEAR!-FACTORS,DX!-TO!-P)
  END;

SYMBOLIC PROCEDURE MAKE!-X!-TO!-P(P,WVEC1,X!-TO!-P);
  BEGIN SCALAR DX!-TO!-P,DW1;
    IF P#<DPOLY THEN <<
       FOR I:=0:P#-1 DO PUTV(X!-TO!-P,I,0);
       PUTV(X!-TO!-P,P,1);
       RETURN P >>;
    DX!-TO!-P:=MAKE!-X!-TO!-P(P/2,WVEC1,X!-TO!-P);
    DW1:=TIMES!-IN!-VECTOR(X!-TO!-P,DX!-TO!-P,X!-TO!-P,DX!-TO!-P,WVEC1);
    DW1:=REMAINDER!-IN!-VECTOR(WVEC1,DW1,
        POLY!-VECTOR,DPOLY);
    IF NOT(IREMAINDER(P,2)=0) THEN <<
       FOR I:=DW1 STEP -1 UNTIL 0 DO
          PUTV(WVEC1,I#+1,GETV(WVEC1,I));
       PUTV(WVEC1,0,0);
       DW1:=REMAINDER!-IN!-VECTOR(WVEC1,DW1#+1,
         POLY!-VECTOR,DPOLY) >>;
    FOR I:=0:DW1 DO PUTV(X!-TO!-P,I,GETV(WVEC1,I));
    RETURN DW1
  END;

SYMBOLIC PROCEDURE FIND!-LINEAR!-FACTORS!-MOD!-P(P,N);
% P is a vector representing a polynomial of degree N which has
% only linear factors. Find all the factors and return a list of
% them;
  BEGIN
    SCALAR ROOT,VAR,W,VEC1;
    IF N#<1 THEN RETURN NIL;
    VEC1:=MKVECT 1;
    PUTV(VEC1,1,1);
    ROOT:=0;
    WHILE (N#>1) AND NOT (ROOT #> CURRENT!-MODULUS) DO <<
        W:=EVALUATE!-IN!-VECTOR(P,N,ROOT);
        IF W=0 THEN << %a factor has been found!!;
          IF VAR=NIL THEN
             VAR:=MKSP(M!-IMAGE!-VARIABLE,1) . 1;
          W:=!*F2MOD
            ADJOIN!-TERM(CAR VAR,CDR VAR,!*N2F MODULAR!-MINUS ROOT);
          KNOWN!-FACTORS:=W . KNOWN!-FACTORS;
          PUTV(VEC1,0,MODULAR!-MINUS ROOT);
          N:=QUOTFAIL!-IN!-VECTOR(P,N,VEC1,1) >>;
        ROOT:=ROOT#+1 >>;
    KNOWN!-FACTORS:=
        VECTOR!-TO!-POLY(P,N,M!-IMAGE!-VARIABLE) . KNOWN!-FACTORS
  END;


%**********************************************************************;
% Berlekamp's algorithm part 1: find null space basis giving factor
% count;


SYMBOLIC PROCEDURE USE!-BERLEKAMP(X!-TO!-P,DX!-TO!-P,WVEC1);
% Set up a basis for the set of remaining (nonlinear) factors
% using Berlekamp's algorithm;
  BEGIN
    SCALAR BERL!-M,BERL!-M!-SIZE,W,
           DCURRENT,CURRENT!-POWER,WTIME;
    BERL!-M!-SIZE:=DPOLY#-1;
    BERL!-M:=MKVECT BERL!-M!-SIZE;
    FOR I:=0:BERL!-M!-SIZE DO <<
      W:=MKVECT BERL!-M!-SIZE;
      FOR J:=0:BERL!-M!-SIZE DO PUTV(W,J,0); %initialize to zero;
      PUTV(BERL!-M,I,W) >>;
% Note that column zero of the matrix (as used in the
% standard version of Berlekamp's algorithm) is not in fact
% needed and is not used here;
% I want to set up a matrix that has entries
%  x**p, x**(2*p), ... , x**((n-1)*p)
% as its columns,
% where n is the degree of poly-mod-p
% and all the entries are reduced mod poly-mod-p;
% Since I computed x**p I have taken out some linear factors,
% so reduce it further;
    DX!-TO!-P:=REMAINDER!-IN!-VECTOR(X!-TO!-P,DX!-TO!-P,
      POLY!-VECTOR,DPOLY);
    DCURRENT:=0;
    CURRENT!-POWER:=MKVECT BERL!-M!-SIZE;
    PUTV(CURRENT!-POWER,0,1);
    FOR I:=1:BERL!-M!-SIZE DO <<
       IF CURRENT!-MODULUS#>DPOLY THEN
         DCURRENT:=TIMES!-IN!-VECTOR(
            CURRENT!-POWER,DCURRENT,
            X!-TO!-P,DX!-TO!-P,
            WVEC1)
       ELSE << % Multiply by shifting;
         FOR I:=0:CURRENT!-MODULUS#-1 DO
           PUTV(WVEC1,I,0);
         FOR I:=0:DCURRENT DO
           PUTV(WVEC1,CURRENT!-MODULUS#+I,
             GETV(CURRENT!-POWER,I));
         DCURRENT:=DCURRENT#+CURRENT!-MODULUS >>;
       DCURRENT:=REMAINDER!-IN!-VECTOR(
         WVEC1,DCURRENT,
         POLY!-VECTOR,DPOLY);
       FOR J:=0:DCURRENT DO
          PUTV(GETV(BERL!-M,J),I,PUTV(CURRENT!-POWER,J,
            GETV(WVEC1,J)));
% also I need to subtract 1 from the diagonal of the matrix;
       PUTV(GETV(BERL!-M,I),I,
         MODULAR!-DIFFERENCE(GETV(GETV(BERL!-M,I),I),1)) >>;
    WTIME:=TIME();
%   PRINT!-M("Q matrix",BERL!-M,BERL!-M!-SIZE);
    W := FIND!-NULL!-SPACE(BERL!-M,BERL!-M!-SIZE);
    TRACE!-TIME DISPLAY!-TIME("Null space found in ",TIME()-WTIME);
    RETURN W
  END;


SYMBOLIC PROCEDURE FIND!-NULL!-SPACE(BERL!-M,BERL!-M!-SIZE);
% Diagonalize the matrix to find its rank and hence the number of
% factors the input polynomial had;
  BEGIN SCALAR NULL!-SPACE!-BASIS;
% find a basis for the null-space of the matrix;
    FOR I:=1:BERL!-M!-SIZE DO
      NULL!-SPACE!-BASIS:=
        CLEAR!-COLUMN(I,NULL!-SPACE!-BASIS,BERL!-M,BERL!-M!-SIZE);
%    PRINT!-M("Null vectored",BERL!-M,BERL!-M!-SIZE);
    RETURN
      TIDY!-UP!-NULL!-VECTORS(NULL!-SPACE!-BASIS,BERL!-M,BERL!-M!-SIZE)
  END;

SYMBOLIC PROCEDURE PRINT!-M(M,BERL!-M,BERL!-M!-SIZE);
 << PRINTC M;
    FOR I:=0:BERL!-M!-SIZE DO <<
      FOR J:=0:BERL!-M!-SIZE DO <<
        PRINC GETV(GETV(BERL!-M,I),J);
        TTAB((4#*J)#+4) >>;
      TERPRI() >> >>;



SYMBOLIC PROCEDURE CLEAR!-COLUMN(I,
                    NULL!-SPACE!-BASIS,BERL!-M,BERL!-M!-SIZE);
% Process column I of the matrix so that (if possible) it
% just has a '1' in row I and zeros elsewhere;
  BEGIN
    SCALAR II,W;
% I want to bring a non-zero pivot to the position (i,i)
% and then add multiples of row i to all other rows to make
% all but the i'th element of column i zero. First look for
% a suitable pivot;
    II:=0;
SEARCH!-FOR!-PIVOT:
    IF GETV(GETV(BERL!-M,II),I)=0 OR
       ((II#<I) AND NOT(GETV(GETV(BERL!-M,II),II)=0)) THEN
          IF (II:=II#+1)#>BERL!-M!-SIZE THEN
              RETURN (I . NULL!-SPACE!-BASIS)
          ELSE GO TO SEARCH!-FOR!-PIVOT;
% Here ii references a row containing a suitable pivot element for
% column i. Permute rows in the matrix so as to bring the pivot onto
% the diagonal;
    W:=GETV(BERL!-M,II);
    PUTV(BERL!-M,II,GETV(BERL!-M,I));
    PUTV(BERL!-M,I,W);
            % swop rows ii and i ;
    W:=MODULAR!-MINUS MODULAR!-RECIPROCAL GETV(GETV(BERL!-M,I),I);
% w = -1/pivot, and is used in zeroing out the rest of column i;
    FOR ROW:=0:BERL!-M!-SIZE DO
      IF ROW NEQ I THEN BEGIN
         SCALAR R; %process one row;
         R:=GETV(GETV(BERL!-M,ROW),I);
         IF NOT(R=0) THEN <<
           R:=MODULAR!-TIMES(R,W);
   %that is now the multiple of row i that must be added to row ii;
           FOR COL:=I:BERL!-M!-SIZE DO
             PUTV(GETV(BERL!-M,ROW),COL,
               MODULAR!-PLUS(GETV(GETV(BERL!-M,ROW),COL),
               MODULAR!-TIMES(R,GETV(GETV(BERL!-M,I),COL)))) >>
         END;
    FOR COL:=I:BERL!-M!-SIZE DO
        PUTV(GETV(BERL!-M,I),COL,
           MODULAR!-TIMES(GETV(GETV(BERL!-M,I),COL),W));
    RETURN NULL!-SPACE!-BASIS
  END;


SYMBOLIC PROCEDURE TIDY!-UP!-NULL!-VECTORS(NULL!-SPACE!-BASIS,
                    BERL!-M,BERL!-M!-SIZE);
  BEGIN
    SCALAR ROW!-TO!-USE;
    ROW!-TO!-USE:=BERL!-M!-SIZE#+1;
    NULL!-SPACE!-BASIS:=
      FOR EACH NULL!-VECTOR IN NULL!-SPACE!-BASIS COLLECT
        BUILD!-NULL!-VECTOR(NULL!-VECTOR,
            GETV(BERL!-M,ROW!-TO!-USE:=ROW!-TO!-USE#-1),BERL!-M);
    BERL!-M:=NIL; % Release the store for full matrix;
%    PRINC "Null vectors: ";
%    PRINT NULL!-SPACE!-BASIS;
    RETURN NULL!-SPACE!-BASIS
  END;

SYMBOLIC PROCEDURE BUILD!-NULL!-VECTOR(N,VEC,BERL!-M);
% At the end of the elimination process (the CLEAR-COLUMN loop)
% certain columns, indicated by the entries in NULL-SPACE-BASIS
% will be null vectors, save for the fact that they need a '1'
% inserted on the diagonal of the matrix. This procedure copies
% these null-vectors into some of the vectors that represented
% rows of the Berlekamp matrix;
  BEGIN
%   PUTV(VEC,0,0); % Not used later!!;
    FOR I:=1:N#-1 DO
      PUTV(VEC,I,GETV(GETV(BERL!-M,I),N));
    PUTV(VEC,N,1);
%   FOR I:=N#+1:BERL!-M!-SIZE DO
%     PUTV(VEC,I,0);
    RETURN VEC . N
  END;



%**********************************************************************;
% Berlekamp's algorithm part 2: retrieving the factors mod p;


SYMBOLIC PROCEDURE GET!-FACTORS!-MOD!-P(N,P);
% given the modular info (for the nth image) generated by the
% previous half of Berlekamp's method we can reconstruct the
% actual factors mod p;
  BEGIN SCALAR NTH!-MODULAR!-INFO,OLD!-M,WTIME;
    NTH!-MODULAR!-INFO:=GETV(MODULAR!-INFO,N);
    OLD!-M:=SET!-MODULUS P;
    WTIME:=TIME();
    PUTV(MODULAR!-INFO,N,
      CONVERT!-NULL!-VECTORS!-TO!-FACTORS NTH!-MODULAR!-INFO);
    TRACE!-TIME DISPLAY!-TIME("Factors constructed in ",TIME()-WTIME);
    SET!-MODULUS OLD!-M
  END;

SYMBOLIC PROCEDURE CONVERT!-NULL!-VECTORS!-TO!-FACTORS M!-INFO;
% Using the null space found, complete the job
% of finding modular factors by taking gcd's of the
% modular input polynomial and variants on the
% null space generators;
  BEGIN
    SCALAR NUMBER!-NEEDED,FACTORS,
      WORK!-VECTOR1,DWORK1,WORK!-VECTOR2,DWORK2,WTIME;
    KNOWN!-FACTORS:=NIL;
    WTIME:=TIME();
    FIND!-LINEAR!-FACTORS!-MOD!-P(CDAR M!-INFO,CAAR M!-INFO);
    TRACE!-TIME DISPLAY!-TIME("Linear factors found in ",TIME()-WTIME);
    DPOLY:=CAADR M!-INFO;
    POLY!-VECTOR:=CDADR M!-INFO;
    NULL!-SPACE!-BASIS:=CADDR M!-INFO;
    IF DPOLY=0 THEN RETURN KNOWN!-FACTORS; % All factors were linear;
    IF NULL NULL!-SPACE!-BASIS THEN
      RETURN KNOWN!-FACTORS:=
          VECTOR!-TO!-POLY(POLY!-VECTOR,DPOLY,M!-IMAGE!-VARIABLE) .
            KNOWN!-FACTORS;
    NUMBER!-NEEDED:=LENGTH NULL!-SPACE!-BASIS;
% count showing how many more factors I need to find;
    WORK!-VECTOR1:=MKVECT DPOLY;
    WORK!-VECTOR2:=MKVECT DPOLY;
    FACTORS:=LIST (POLY!-VECTOR . DPOLY);
TRY!-NEXT!-NULL:
    IF NULL!-SPACE!-BASIS=NIL THEN
      ERRORF "RAN OUT OF NULL VECTORS TOO EARLY";
    WTIME:=TIME();
    FACTORS:=TRY!-ALL!-CONSTANTS(FACTORS,
        CAAR NULL!-SPACE!-BASIS,CDAR NULL!-SPACE!-BASIS);
    TRACE!-TIME DISPLAY!-TIME("All constants tried in ",TIME()-WTIME);
    IF NUMBER!-NEEDED=0 THEN
       RETURN KNOWN!-FACTORS:=APPEND!-NEW!-FACTORS(FACTORS,
            KNOWN!-FACTORS);
    NULL!-SPACE!-BASIS:=CDR NULL!-SPACE!-BASIS;
    GO TO TRY!-NEXT!-NULL
  END;


SYMBOLIC PROCEDURE TRY!-ALL!-CONSTANTS(LIST!-OF!-POLYS,V,DV);
% use gcd's of v, v+1, v+2, ... to try to split up the
% polynomials in the given list;
  BEGIN
    SCALAR A,B,AA,S,WTIME;
% aa is a list of factors that can not be improved using this v,
% b is a list that might be;
    AA:=NIL; B:=LIST!-OF!-POLYS;
    S:=0;
TRY!-NEXT!-CONSTANT:
    PUTV(V,0,S); % Fix constant term of V to be S;
%    WTIME:=TIME();
    A:=SPLIT!-FURTHER(B,V,DV);
%    TRACE!-TIME DISPLAY!-TIME("Polys split further in ",TIME()-WTIME);
    B:=CDR A; A:=CAR A;
    AA:=NCONC(A,AA);
% Keep aa up to date as a list of polynomials that this poly
% v can not help further with;
    IF B=NIL THEN RETURN AA; % no more progress possible here;
    IF NUMBER!-NEEDED=0 THEN RETURN NCONC(B,AA);
      % no more progress needed;
    S:=S#+1;
    IF S#<CURRENT!-MODULUS THEN GO TO TRY!-NEXT!-CONSTANT;
% Here I have run out of choices for the constant
% coefficient in v without splitting everything;
    RETURN NCONC(B,AA)
  END;

SYMBOLIC PROCEDURE SPLIT!-FURTHER(LIST!-OF!-POLYS,V,DV);
% list-of-polys is a list of polynomials. try to split
% its members further by taking gcd's with the polynomial
% v. return (a . b) where the polys in a can not possibly
% be split using v+constant, but the polys in b might
% be;
    IF NULL LIST!-OF!-POLYS THEN NIL . NIL
    ELSE BEGIN
      SCALAR A,B,GG,Q;
      A:=SPLIT!-FURTHER(CDR LIST!-OF!-POLYS,V,DV);
      B:=CDR A; A:=CAR A;
      IF NUMBER!-NEEDED=0 THEN GO TO NO!-SPLIT;
      % if all required factors have been found there is no need to
      % search further;
      DWORK1:=COPY!-VECTOR(V,DV,WORK!-VECTOR1);
      DWORK2:=COPY!-VECTOR(CAAR LIST!-OF!-POLYS,CDAR LIST!-OF!-POLYS,
        WORK!-VECTOR2);
      DWORK1:=GCD!-IN!-VECTOR(WORK!-VECTOR1,DWORK1,
         WORK!-VECTOR2,DWORK2);
      IF DWORK1=0 OR DWORK1=CDAR LIST!-OF!-POLYS THEN GO TO NO!-SPLIT;
      DWORK2:=COPY!-VECTOR(CAAR LIST!-OF!-POLYS,CDAR LIST!-OF!-POLYS,
        WORK!-VECTOR2);
      DWORK2:=QUOTFAIL!-IN!-VECTOR(WORK!-VECTOR2,DWORK2,
        WORK!-VECTOR1,DWORK1);
% Here I have a splitting;
      GG:=MKVECT DWORK1;
      COPY!-VECTOR(WORK!-VECTOR1,DWORK1,GG);
      A:=((GG . DWORK1) . A);
      COPY!-VECTOR(WORK!-VECTOR2,DWORK2,Q:=MKVECT DWORK2);
      B:=((Q . DWORK2) . B);
      NUMBER!-NEEDED:=NUMBER!-NEEDED#-1;
      RETURN (A . B);
   NO!-SPLIT:
      RETURN (A . ((CAR LIST!-OF!-POLYS) . B))
    END;

SYMBOLIC PROCEDURE APPEND!-NEW!-FACTORS(A,B);
% Convert to REDUCE (rather than vector) form;
    IF NULL A THEN B
    ELSE
      VECTOR!-TO!-POLY(CAAR A,CDAR A,M!-IMAGE!-VARIABLE) .
        APPEND!-NEW!-FACTORS(CDR A,B);



CARCHECK SAFE!-FLAG; % Restore status quo;

ENDMODULE;


MODULE FACPRIM;

% *******************************************************************
%
%   copyright (c)  university of cambridge, england 1979
%
% *******************************************************************;




%**********************************************************************;
%
%    multivariate polynomial factorization more or less as described
%    by paul wang in:  math. comp. vol.32 no.144 oct 1978 pp. 1215-1231
%       'an improved multivariate polynomial factoring algorithm'
%
%    p. m. a. moore.  1979.
%
%
%**********************************************************************;


%----------------------------------------------------------------------;
%   this code works by using a local database of fluid variables
%   whose meaning is (hopefully) obvious.
%   they are used as follows:
%
%   global name:            set in:               comments:
%
% m!-factored!-leading!    create!.images        only set if non-numeric
%  -coefft
% m!-factored!-images      factorize!.images     vector
% m!-input!-polynomial     factorize!-primitive!
%                           -polynomial
% m!-best!-image!-pointer  choose!.best!.image
% m!-image!-factors        choose!.best!.image   vector
% m!-true!-leading!        choose!.best!.image   vector
%  -coeffts
% m!-prime                 choose!.best!.image
% irreducible              factorize!.images     predicate
% inverted                 create!.images        predicate
% m!-inverted!-sign        create!-images        +1 or -1
% non!-monic               determine!-leading!   predicate
%                           -coeffts
%                          (also reconstruct!-over!
%                           -integers)
% m!-number!-of!-factors   choose!.best!.image
% m!-image!-variable       square!.free!.factorize
%                          or factorize!-form
% m!-image!-sets           create!.images        vector
% this last contains the images of m!-input!-polynomial and the
% numbers associated with the factors of lc m!-input!-polynomial (to be
% used later) the latter existing only when the lc m!-input!-polynomial
% is non-integral. ie.:
%    m!-image!-sets=< ... , (( d . u ), a, d) , ... >   ( a vector)
% where: a = an image set (=association list);
%        d = cont(m!-input!-polynomial image wrt a);
%        u = prim.part.(same) which is non-trivial square-free
%            by choice of image set.;
%        d = vector of numbers associated with factors in lc
%            m!-input!-polynomial (these depend on a as well);
% the number of entries in m!-image!-sets is defined by the fluid
% variable, no.of.random.sets;
%
%
%
%----------------------------------------------------------------------;




%**********************************************************************;
% multivariate factorization part 1. entry point for this code:
%  ** n.b.** the polynomial is assumed to be non-trivial and primitive;


SYMBOLIC PROCEDURE SQUARE!.FREE!.FACTORIZE U;
% u primitive (multivariate) poly but not yet square free.
% result is list of factors consed with their respective multiplicities:
%  ((f1 . m1),(f2 . m2),...) where mi may = mj when i not = j ;
% u is non-trivial - ie. at least linear in some variable;
%***** nb. this does not use best square free method *****;
  BEGIN SCALAR V,W,X,Y,I,NEWU,F!.LIST,SFP!-COUNT;
    SFP!-COUNT:=0;
    FACTOR!-TRACE
      IF NOT U=POLYNOMIAL!-TO!-FACTOR THEN
       << PRIN2!* "Primitive polynomial to factor: ";
	  FAC!-PRINTSF U >>;
    IF NULL M!-IMAGE!-VARIABLE THEN
      ERRORF LIST("M-IMAGE-VARIABLE not set: ",U);
    V:=POLY!-GCD(U,
	  DERIVATIVE!-WRT!-MAIN!-VARIABLE(U,M!-IMAGE!-VARIABLE));
    IF ONEP V THEN <<
      FACTOR!-TRACE PRINTSTR "The polynomial is square-free.";
      RETURN SQUARE!-FREE!-PRIM!-FACTOR(U,1) >>
    ELSE FACTOR!-TRACE <<
      PRINTSTR
	 "We now square-free decompose this to produce a series of ";
      PRINTSTR
	 "(square-free primitive) factors which we treat in turn: ";
      TERPRI(); TERPRI() >>;
    W:=QUOTFAIL(U,V);
    X:=POLY!-GCD(V,W);
    NEWU:=QUOTFAIL(W,X);
    IF NOT ONEP NEWU THEN
    << F!.LIST:=APPEND(F!.LIST,
        SQUARE!-FREE!-PRIM!-FACTOR(NEWU,1))
    >>;
    I:=2;  % power of next factors;
            % from now on we can avoid an extra gcd and any diffn;
    WHILE NOT DOMAINP V DO
    << V:=QUOTFAIL(V,X);
      W:=QUOTFAIL(W,NEWU);
      X:=POLY!-GCD(V,W);
      NEWU:=QUOTFAIL(W,X);
      IF NOT ONEP NEWU THEN
      << F!.LIST:=APPEND(F!.LIST,
          SQUARE!-FREE!-PRIM!-FACTOR(NEWU,I))
      >>;
      I:=IADD1 I
    >>;
    IF NOT V=1 THEN F!.LIST:=(V . 1) . F!.LIST;
    RETURN F!.LIST
  END;

SYMBOLIC PROCEDURE SQUARE!-FREE!-PRIM!-FACTOR(U,I);
% factorize the square-free primitive factor u whose multiplicity
% in the original poly is i. return the factors consed with this
% multiplicity;
  BEGIN SCALAR W;
    SFP!-COUNT:=IADD1 SFP!-COUNT;
    FACTOR!-TRACE <<
      IF NOT(U=POLYNOMIAL!-TO!-FACTOR) THEN <<
        PRIN2!* "("; PRIN2!* SFP!-COUNT;
	PRIN2!* ") Square-free primitive factor: "; FAC!-PRINTSF U;
        PRIN2!* "    with multiplicity "; PRIN2!* I;
        TERPRI!*(NIL) >> >>;
    W:=DISTRIBUTE!.MULTIPLICITY(FACTORIZE!-PRIMITIVE!-POLYNOMIAL U,I);
    FACTOR!-TRACE
      IF NOT U=POLYNOMIAL!-TO!-FACTOR THEN <<
        PRIN2!* "Factors of ("; PRIN2!* SFP!-COUNT;
	PRINTSTR ") are: "; FAC!-PRINTFACTORS(1 . W);
        TERPRI(); TERPRI() >>;
    RETURN W
  END;

SYMBOLIC PROCEDURE DISTRIBUTE!.MULTIPLICITY(FACTORLIST,N);
% factorlist is a simple list of factors of a square free primitive
% multivariate poly and n is their multiplicity in a square free
% decomposition of another polynomial. result is a list of form:
%  ((f1 . n),(f2 . n),...) where fi are the factors.;
  FOR EACH W IN FACTORLIST COLLECT (W . N);

SYMBOLIC PROCEDURE FACTORIZE!-PRIMITIVE!-POLYNOMIAL U;
% u is primitive square free and at least linear in
% m!-image!-variable. m!-image!-variable is the variable preserved in
% the univariate images. this function determines a random set of
% integers and a prime to create a univariate modular image of u,
% factorize it and determine the leading coeffts of the factors in the
% full factorization of u. finally the modular image factors are grown
% up to the full multivariates ones using the hensel construction;
% result is simple list of irreducible factors;
  IF DEGREE!-IN!-VARIABLE(U,M!-IMAGE!-VARIABLE) = 1 THEN LIST U
  ELSE IF UNIVARIATEP U THEN
     UNIVARIATE!-FACTORIZE U
  ELSE BEGIN SCALAR
    VALID!-IMAGE!-SETS,FACTORED!-LC,IMAGE!-FACTORS,PRIME!-BASE,
    ONE!-COMPLETE!-DEG!-ANALYSIS!-DONE,ZSET,ZEROVARSET,OTHERVARS,
    MULTIVARIATE!-INPUT!-POLY,BEST!-SET!-POINTER,REDUCTION!-COUNT,
    TRUE!-LEADING!-COEFFTS,NUMBER!-OF!-FACTORS,
    INVERTED!-SIGN,IRREDUCIBLE,INVERTED,VARS!-TO!-KILL,
    FORBIDDEN!-SETS,ZERO!-SET!-TRIED,NON!-MONIC,
    NO!-OF!-BEST!-SETS,NO!-OF!-RANDOM!-SETS,BAD!-CASE,
    TARGET!-FACTOR!-COUNT,MODULAR!-INFO,MULTIVARIATE!-FACTORS,
    HENSEL!-GROWTH!-SIZE,ALPHALIST,BASE!-TIMER,W!-TIME,
    PREVIOUS!-DEGREE!-MAP,IMAGE!-SET!-MODULUS,COEFFTS!-VECTORS,
    BEST!-KNOWN!-FACTORS,RECONSTRUCTING!-GCD,FULL!-GCD;
    BASE!-TIMER:=TIME();
    TRACE!-TIME DISPLAY!-TIME(
      " Entered multivariate primitive polynomial code after ",
      BASE!-TIMER - BASE!-TIME);
%note that this code works by using a local database of
%fluid variables that are updated by the subroutines directly
%called here. this allows for the relativly complicated
%interaction between flow of data and control that occurs in
%the factorization algorithm;
    FACTOR!-TRACE <<
      PRINTSTR "From now on we shall refer to this polynomial as U.";
      PRINTSTR
	 "We now create an image of U by picking suitable values ";
      PRINTSTR "for all but one of the variables in U.";
      PRIN2!* "The variable preserved in the image is ";
      PRINVAR M!-IMAGE!-VARIABLE; TERPRI!*(NIL) >>;
    INITIALIZE!-FLUIDS U;
            % set up the fluids to start things off;
    W!-TIME:=TIME();
TRYAGAIN:
    GET!-SOME!-RANDOM!-SETS();
    CHOOSE!-THE!-BEST!-SET();
      TRACE!-TIME <<
        DISPLAY!-TIME("Modular factoring and best set chosen in ",
          TIME()-W!-TIME);
        W!-TIME:=TIME() >>;
      IF IRREDUCIBLE THEN
        RETURN LIST U
      ELSE IF BAD!-CASE THEN <<
        IF !*OVERSHOOT THEN PRINTC "Bad image sets - loop";
        BAD!-CASE:=NIL; GOTO TRYAGAIN >>;
    RECONSTRUCT!-IMAGE!-FACTORS!-OVER!-INTEGERS();
      TRACE!-TIME <<
        DISPLAY!-TIME("Image factors reconstructed in ",TIME()-W!-TIME);
        W!-TIME:=TIME() >>;
      IF IRREDUCIBLE THEN
        RETURN LIST U
      ELSE IF BAD!-CASE THEN <<
        IF !*OVERSHOOT THEN PRINTC "Bad image factors - loop";
        BAD!-CASE:=NIL; GOTO TRYAGAIN >>;
    DETERMINE!.LEADING!.COEFFTS();
      TRACE!-TIME <<
        DISPLAY!-TIME("Leading coefficients distributed in ",
          TIME()-W!-TIME);
        W!-TIME:=TIME() >>;
      IF IRREDUCIBLE THEN
        RETURN LIST U
      ELSE IF BAD!-CASE THEN <<
        IF !*OVERSHOOT THEN PRINTC "Bad split shown by LC distribution";
        BAD!-CASE:=NIL; GOTO TRYAGAIN >>;
    IF DETERMINE!-MORE!-COEFFTS()='DONE THEN <<
      TRACE!-TIME <<
        DISPLAY!-TIME("All the coefficients distributed in ",
          TIME()-W!-TIME);
        W!-TIME:=TIME() >>;
      RETURN CHECK!-INVERTED MULTIVARIATE!-FACTORS >>;
    TRACE!-TIME <<
      DISPLAY!-TIME("More coefficients distributed in ",
        TIME()-W!-TIME);
      W!-TIME:=TIME() >>;
    RECONSTRUCT!-MULTIVARIATE!-FACTORS(NIL);
      IF BAD!-CASE AND NOT IRREDUCIBLE THEN <<
        IF !*OVERSHOOT THEN PRINTC "Multivariate overshoot - restart";
         BAD!-CASE:=NIL; GOTO TRYAGAIN >>;
      TRACE!-TIME
        DISPLAY!-TIME("Multivariate factors reconstructed in ",
          TIME()-W!-TIME);
      IF IRREDUCIBLE THEN
        RETURN LIST U;
    RETURN CHECK!-INVERTED MULTIVARIATE!-FACTORS
   END;


SYMBOLIC PROCEDURE INITIALIZE!-FLUIDS U;
% Set up the fluids to be used in factoring primitive poly;
  BEGIN SCALAR W,W1,WTIME;
    IF !*FORCE!-ZERO!-SET THEN <<
      NO!-OF!-RANDOM!-SETS:=1;
      NO!-OF!-BEST!-SETS:=1 >>
    ELSE <<
      NO!-OF!-RANDOM!-SETS:=9;
            % we generate this many and calculate their factor counts;
      NO!-OF!-BEST!-SETS:=5;
            % we find the modular factors of this many;
      >>;
    IMAGE!-SET!-MODULUS:=5;
    VARS!-TO!-KILL:=VARIABLES!-TO!-KILL LC U;
    MULTIVARIATE!-INPUT!-POLY:=U;
    TARGET!-FACTOR!-COUNT:=DEGREE!-IN!-VARIABLE(U,M!-IMAGE!-VARIABLE);
    IF NOT DOMAINP LC MULTIVARIATE!-INPUT!-POLY THEN
      IF DOMAINP (W:=
        TRAILING!.COEFFT(MULTIVARIATE!-INPUT!-POLY,
                         M!-IMAGE!-VARIABLE)) THEN
    << INVERTED:=T;
	% note that we are 'inverting' the poly m!-input!-polynomial;
      W1:=INVERT!.POLY(MULTIVARIATE!-INPUT!-POLY,M!-IMAGE!-VARIABLE);
      MULTIVARIATE!-INPUT!-POLY:=CDR W1;
      INVERTED!-SIGN:=CAR W1;
            % to ease the lc problem, m!-input!-polynomial <- poly
            % produced by taking numerator of (m!-input!-polynomial
            % with 1/m!-image!-variable substituted for
            % m!-image!-variable);
            % m!-inverted!-sign is -1 if we have inverted the sign of
            % the resulting poly to keep it +ve, else +1;
      FACTOR!-TRACE <<
        PRIN2!* "The trailing coefficient of U wrt ";
        PRINVAR M!-IMAGE!-VARIABLE; PRIN2!* "(="; PRIN2!* W;
        PRINTSTR ") is purely numeric so we 'invert' U to give: ";
	PRIN2!* "  U <- "; FAC!-PRINTSF MULTIVARIATE!-INPUT!-POLY;
        PRINTSTR "This simplifies any problems with the leading ";
        PRINTSTR "coefficient of U." >>
    >>
    ELSE <<
      TRACE!-TIME PRINTC "Factoring the leading coefficient:";
      WTIME:=TIME();
      FACTORED!-LC:=
        FACTORIZE!-FORM!-RECURSION LC MULTIVARIATE!-INPUT!-POLY;
      TRACE!-TIME DISPLAY!-TIME("Leading coefficient factored in ",
        TIME()-WTIME);
            % factorize the lc of m!-input!-polynomial completely;
      FACTOR!-TRACE <<
	PRINTSTR
	   "The leading coefficient of U is non-trivial so we must ";
        PRINTSTR "factor it before we can decide how it is distributed";
        PRINTSTR "over the leading coefficients of the factors of U.";
        PRINTSTR "So the factors of this leading coefficient are:";
	FAC!-PRINTFACTORS FACTORED!-LC >>
    >>;
   MAKE!-ZEROVARSET VARS!-TO!-KILL;
            % Sets ZEROVARSET and OTHERVARS;
   IF NULL ZEROVARSET THEN ZERO!-SET!-TRIED:=T
   ELSE <<
    ZSET:=MAKE!-ZEROSET!-LIST LENGTH ZEROVARSET;
    SAVE!-ZSET:=ZSET >>
  END;



SYMBOLIC PROCEDURE VARIABLES!-TO!-KILL LC!-U;
% picks out all the variables in u except var. also checks to see if
% any of these divide lc u: if they do they are dotted with t otherwise
% dotted with nil. result is list of these dotted pairs;
  FOR EACH W IN CDR KORD!* COLLECT
    IF (DOMAINP LC!-U) OR DIDNTGO QUOTF(LC!-U,!*K2F W) THEN
       (W . NIL) ELSE (W . T);


%**********************************************************************;
% multivariate factorization part 2. creating image sets and picking
%  the best one;


FLUID '(USABLE!-SET!-FOUND);

SYMBOLIC PROCEDURE GET!-SOME!-RANDOM!-SETS();
% here we create a number of random sets to make the input
% poly univariate by killing all but 1 of the variables. at
% the same time we pick a random prime to reduce this image
% poly mod p;
  BEGIN SCALAR IMAGE!-SET,CHOSEN!-PRIME,IMAGE!-LC,IMAGE!-MOD!-P,WTIME,
        IMAGE!-CONTENT,IMAGE!-POLY,F!-NUMVEC,FORBIDDEN!-PRIMES,I,J,
        USABLE!-SET!-FOUND;
    VALID!-IMAGE!-SETS:=MKVECT NO!-OF!-RANDOM!-SETS;
    I:=0;
    WHILE I < NO!-OF!-RANDOM!-SETS DO <<
      WTIME:=TIME();
      GENERATE!-AN!-IMAGE!-SET!-WITH!-PRIME(
        IF I<IDIFFERENCE(NO!-OF!-RANDOM!-SETS,3) THEN NIL ELSE T);
      TRACE!-TIME
        DISPLAY!-TIME("  Image set generated in ",TIME()-WTIME);
      I:=IADD1 I;
      PUTV(VALID!-IMAGE!-SETS,I,LIST(
        IMAGE!-SET,CHOSEN!-PRIME,IMAGE!-LC,IMAGE!-MOD!-P,IMAGE!-CONTENT,
        IMAGE!-POLY,F!-NUMVEC));
      FORBIDDEN!-SETS:=IMAGE!-SET . FORBIDDEN!-SETS;
      FORBIDDEN!-PRIMES:=LIST CHOSEN!-PRIME;
      J:=1;
      WHILE (J<3) AND (I<NO!-OF!-RANDOM!-SETS) DO <<
        WTIME:=TIME();
        IMAGE!-MOD!-P:=FIND!-A!-VALID!-PRIME(IMAGE!-LC,IMAGE!-POLY,
          NOT NUMBERP IMAGE!-CONTENT);
        IF NOT(IMAGE!-MOD!-P='NOT!-SQUARE!-FREE) THEN <<
          TRACE!-TIME
            DISPLAY!-TIME("  Prime and image mod p found in ",
              TIME()-WTIME);
          I:=IADD1 I;
          PUTV(VALID!-IMAGE!-SETS,I,LIST(
            IMAGE!-SET,CHOSEN!-PRIME,IMAGE!-LC,IMAGE!-MOD!-P,
            IMAGE!-CONTENT,IMAGE!-POLY,F!-NUMVEC));
          FORBIDDEN!-PRIMES:=CHOSEN!-PRIME . FORBIDDEN!-PRIMES >>;
        J:=IADD1 J
        >>
      >>
  END;

SYMBOLIC PROCEDURE CHOOSE!-THE!-BEST!-SET();
% given several random sets we now choose the best by factoring
% each image mod its chosen prime and taking one with the
% lowest factor count as the best for hensel growth;
  BEGIN SCALAR SPLIT!-LIST,POLY!-MOD!-P,NULL!-SPACE!-BASIS,
               KNOWN!-FACTORS,W,N,FNUM,REMAINING!-SPLIT!-LIST,WTIME;
    MODULAR!-INFO:=MKVECT NO!-OF!-RANDOM!-SETS;
    WTIME:=TIME();
    FOR I:=1:NO!-OF!-RANDOM!-SETS DO <<
      W:=GETV(VALID!-IMAGE!-SETS,I);
      GET!-FACTOR!-COUNT!-MOD!-P(I,GET!-IMAGE!-MOD!-P W,
        GET!-CHOSEN!-PRIME W,NOT NUMBERP GET!-IMAGE!-CONTENT W) >>;
    SPLIT!-LIST:=SORT(SPLIT!-LIST,FUNCTION LESSPPAIR);
            % this now contains a list of pairs (m . n) where
            % m is the no: of factors in image no: n. the list
            % is sorted with best split (smallest m) first;
    TRACE!-TIME
      DISPLAY!-TIME("  Factor counts found in ",TIME()-WTIME);
    IF CAAR SPLIT!-LIST = 1 THEN <<
      IRREDUCIBLE:=T; RETURN NIL >>;
    W:=NIL;
    WTIME:=TIME();
    FOR I:=1:NO!-OF!-BEST!-SETS DO <<
      N:=CDAR SPLIT!-LIST;
      GET!-FACTORS!-MOD!-P(N,
          GET!-CHOSEN!-PRIME GETV(VALID!-IMAGE!-SETS,N));
      W:=(CAR SPLIT!-LIST) . W;
      SPLIT!-LIST:=CDR SPLIT!-LIST >>;
            % pick the best few of these and find out their
            % factors mod p;
    TRACE!-TIME
      DISPLAY!-TIME("  Best factors mod p found in ",TIME()-WTIME);
    REMAINING!-SPLIT!-LIST:=SPLIT!-LIST;
    SPLIT!-LIST:=REVERSEWOC W;
            % keep only those images that are fully factored mod p;
    WTIME:=TIME();
    CHECK!-DEGREE!-SETS(NO!-OF!-BEST!-SETS,T);
            % the best image is pointed at by best!-set!-pointer;
    TRACE!-TIME
      DISPLAY!-TIME("  Degree sets analysed in ",TIME()-WTIME);
            % now if these didn't help try the rest to see
            % if we can avoid finding new image sets altogether:    ;
    IF BAD!-CASE THEN <<
      BAD!-CASE:=NIL;
      WTIME:=TIME();
      WHILE REMAINING!-SPLIT!-LIST DO <<
        N:=CDAR REMAINING!-SPLIT!-LIST;
        GET!-FACTORS!-MOD!-P(N,
            GET!-CHOSEN!-PRIME GETV(VALID!-IMAGE!-SETS,N));
        W:=(CAR REMAINING!-SPLIT!-LIST) . W;
        REMAINING!-SPLIT!-LIST:=CDR REMAINING!-SPLIT!-LIST >>;
      TRACE!-TIME
        DISPLAY!-TIME("  More sets factored mod p in ",TIME()-WTIME);
      SPLIT!-LIST:=REVERSEWOC W;
      WTIME:=TIME();
      CHECK!-DEGREE!-SETS(NO!-OF!-RANDOM!-SETS - NO!-OF!-BEST!-SETS,T);
            % best!-set!-pointer hopefully points at the best image ;
      TRACE!-TIME
        DISPLAY!-TIME("  More degree sets analysed in ",TIME()-WTIME)
    >>;
    ONE!-COMPLETE!-DEG!-ANALYSIS!-DONE:=T;
    FACTOR!-TRACE <<
      W:=GETV(VALID!-IMAGE!-SETS,BEST!-SET!-POINTER);
      PRIN2!* "The chosen image set is:  ";
      FOR EACH X IN GET!-IMAGE!-SET W DO <<
        PRINVAR CAR X; PRIN2!* "="; PRIN2!* CDR X; PRIN2!* "; " >>;
      TERPRI!*(NIL);
      PRIN2!* "and chosen prime is "; PRINTSTR GET!-CHOSEN!-PRIME W;
      PRINTSTR "Image polynomial (made primitive) = ";
      FAC!-PRINTSF GET!-IMAGE!-POLY W;
      IF NOT(GET!-IMAGE!-CONTENT W=1) THEN <<
        PRIN2!* " with (extracted) content of ";
	FAC!-PRINTSF GET!-IMAGE!-CONTENT W >>;
      PRIN2!* "The image polynomial mod "; PRIN2!* GET!-CHOSEN!-PRIME W;
      PRINTSTR ", made monic, is:";
      FAC!-PRINTSF GET!-IMAGE!-MOD!-P W;
      PRINTSTR "and factors of the primitive image mod this prime are:";
      FOR EACH X IN GETV(MODULAR!-INFO,BEST!-SET!-POINTER)
	 DO FAC!-PRINTSF X;
      IF (FNUM:=GET!-F!-NUMVEC W) AND NOT !*OVERVIEW THEN <<
        PRINTSTR "The numeric images of each (square-free) factor of";
        PRINTSTR "the leading coefficient of the polynomial are as";
        PRIN2!* "follows (in order):";
        PRIN2!* "  ";
        FOR I:=1:LENGTH CDR FACTORED!-LC DO <<
          PRIN2!* GETV(FNUM,I); PRIN2!* "; " >>;
        TERPRI!*(NIL) >>
      >>
  END;



%**********************************************************************;
% multivariate factorization part 3. reconstruction of the
% chosen image over the integers;


SYMBOLIC PROCEDURE RECONSTRUCT!-IMAGE!-FACTORS!-OVER!-INTEGERS();
% the hensel construction from modular case to univariate
% over the integers;
  BEGIN SCALAR BEST!-MODULUS,BEST!-FACTOR!-COUNT,INPUT!-POLYNOMIAL,
    INPUT!-LEADING!-COEFFICIENT,BEST!-KNOWN!-FACTORS,S,W,I,
    X!-IS!-FACTOR,X!-FACTOR;
    S:=GETV(VALID!-IMAGE!-SETS,BEST!-SET!-POINTER);
    BEST!-KNOWN!-FACTORS:=GETV(MODULAR!-INFO,BEST!-SET!-POINTER);
    BEST!-MODULUS:=GET!-CHOSEN!-PRIME S;
    BEST!-FACTOR!-COUNT:=LENGTH BEST!-KNOWN!-FACTORS;
    INPUT!-POLYNOMIAL:=GET!-IMAGE!-POLY S;
    IF LDEG INPUT!-POLYNOMIAL=1 THEN
      IF NOT(X!-IS!-FACTOR:=NOT NUMBERP GET!-IMAGE!-CONTENT S) THEN
        ERRORF LIST("Trying to factor a linear image poly: ",
          INPUT!-POLYNOMIAL)
      ELSE BEGIN SCALAR BRECIP,WW,OM,X!-MOD!-P;
        NUMBER!-OF!-FACTORS:=2;
        PRIME!-BASE:=BEST!-MODULUS;
        X!-FACTOR:=!*K2F M!-IMAGE!-VARIABLE;
        PUTV(VALID!-IMAGE!-SETS,BEST!-SET!-POINTER,
          PUT!-IMAGE!-POLY!-AND!-CONTENT(S,LC GET!-IMAGE!-CONTENT S,
            MULTF(X!-FACTOR,GET!-IMAGE!-POLY S)));
        OM:=SET!-MODULUS BEST!-MODULUS;
        BRECIP:=MODULAR!-RECIPROCAL
          RED (WW:=REDUCE!-MOD!-P INPUT!-POLYNOMIAL);
        X!-MOD!-P:=!*F2MOD X!-FACTOR;
        ALPHALIST:=LIST(
          (X!-MOD!-P . BRECIP),
          (WW . MODULAR!-MINUS MODULAR!-TIMES(BRECIP,LC WW)));
        DO!-QUADRATIC!-GROWTH(LIST(X!-FACTOR,INPUT!-POLYNOMIAL),
          LIST(X!-MOD!-P,WW),BEST!-MODULUS);
        W:=LIST INPUT!-POLYNOMIAL; % All factors apart from X-FACTOR;
        SET!-MODULUS OM
      END
    ELSE <<
      INPUT!-LEADING!-COEFFICIENT:=LC INPUT!-POLYNOMIAL;
      FACTOR!-TRACE <<
	PRINTSTR
	   "Next we use the Hensel Construction to grow these modular";
      PRINTSTR "factors into factors over the integers." >>;
      W:=RECONSTRUCT!.OVER!.INTEGERS();
      IF IRREDUCIBLE THEN RETURN T;
      IF (X!-IS!-FACTOR:=NOT NUMBERP GET!-IMAGE!-CONTENT S) THEN <<
        NUMBER!-OF!-FACTORS:=LENGTH W + 1;
        X!-FACTOR:=!*K2F M!-IMAGE!-VARIABLE;
        PUTV(VALID!-IMAGE!-SETS,BEST!-SET!-POINTER,
          PUT!-IMAGE!-POLY!-AND!-CONTENT(S,LC GET!-IMAGE!-CONTENT S,
            MULTF(X!-FACTOR,GET!-IMAGE!-POLY S)));
        FIX!-ALPHAS() >>
      ELSE NUMBER!-OF!-FACTORS:=LENGTH W;
      IF NUMBER!-OF!-FACTORS=1 THEN RETURN IRREDUCIBLE:=T >>;
    IF NUMBER!-OF!-FACTORS>TARGET!-FACTOR!-COUNT THEN
      RETURN BAD!-CASE:=LIST GET!-IMAGE!-SET S;
    IMAGE!-FACTORS:=MKVECT NUMBER!-OF!-FACTORS;
    I:=1;
    FACTOR!-TRACE
      PRINTSTR "The full factors of the image polynomial are:";
    FOR EACH IM!-FACTOR IN W DO <<
      PUTV(IMAGE!-FACTORS,I,IM!-FACTOR);
      FACTOR!-TRACE FAC!-PRINTSF IM!-FACTOR;
      I:=IADD1 I >>;
   IF X!-IS!-FACTOR THEN <<
     PUTV(IMAGE!-FACTORS,I,X!-FACTOR);
     FACTOR!-TRACE <<
       FAC!-PRINTSF X!-FACTOR;
       FAC!-PRINTSF GET!-IMAGE!-CONTENT
         GETV(VALID!-IMAGE!-SETS,BEST!-SET!-POINTER) >> >>
  END;

SYMBOLIC PROCEDURE DO!-QUADRATIC!-GROWTH(FLIST,MODFLIST,P);
  BEGIN SCALAR FHATVEC,ALPHAVEC,FACTORVEC,MODFVEC,FACVEC,
    CURRENT!-FACTOR!-PRODUCT,OM,I,DELTAM,M;
    FHATVEC:=MKVECT NUMBER!-OF!-FACTORS;
    ALPHAVEC:=MKVECT NUMBER!-OF!-FACTORS;
    FACTORVEC:=MKVECT NUMBER!-OF!-FACTORS;
    MODFVEC:=MKVECT NUMBER!-OF!-FACTORS;
    FACVEC:=MKVECT NUMBER!-OF!-FACTORS;
    CURRENT!-FACTOR!-PRODUCT:=1;
    I:=0;
    FOR EACH FF IN FLIST DO <<
      PUTV(FACTORVEC,I:=IADD1 I,FF);
      CURRENT!-FACTOR!-PRODUCT:=MULTF(FF,CURRENT!-FACTOR!-PRODUCT) >>;
    I:=0;
    FOR EACH MODFF IN MODFLIST DO <<
      PUTV(MODFVEC,I:=IADD1 I,MODFF);
      PUTV(ALPHAVEC,I,CDR GET!-ALPHA MODFF) >>;
    DELTAM:=P;
    M:=DELTAM*DELTAM;
    WHILE M<LARGEST!-SMALL!-MODULUS DO <<
      QUADRATIC!-STEP(M,NUMBER!-OF!-FACTORS);
      M:=M*DELTAM >>;
    HENSEL!-GROWTH!-SIZE:=DELTAM;
    ALPHALIST:=NIL;
    FOR J:=1:NUMBER!-OF!-FACTORS DO
      ALPHALIST:=(REDUCE!-MOD!-P GETV(FACTORVEC,J) . GETV(ALPHAVEC,J))
        . ALPHALIST
  END;

SYMBOLIC PROCEDURE FIX!-ALPHAS();
% we extracted a factor x (where x is the image variable)
% before any alphas were calculated, we now need to put
% back this factor and its coresponding alpha which incidently
% will change the other alphas;
  BEGIN SCALAR OM,F1,X!-FACTOR,A,ARECIP,B;
    OM:=SET!-MODULUS HENSEL!-GROWTH!-SIZE;
    F1:=REDUCE!-MOD!-P INPUT!-POLYNOMIAL;
    X!-FACTOR:=!*F2MOD !*K2F M!-IMAGE!-VARIABLE;
    ARECIP:=MODULAR!-RECIPROCAL
      (A:=EVALUATE!-MOD!-P(F1,M!-IMAGE!-VARIABLE,0));
    B:=TIMES!-MOD!-P(MODULAR!-MINUS ARECIP,
      QUOTFAIL!-MOD!-P(DIFFERENCE!-MOD!-P(F1,A),X!-FACTOR));
    ALPHALIST:=(X!-FACTOR . ARECIP) .
      (FOR EACH AA IN ALPHALIST COLLECT
        ((CAR AA) . REMAINDER!-MOD!-P(TIMES!-MOD!-P(B,CDR AA),CAR AA)));
    SET!-MODULUS OM
  END;




%**********************************************************************;
% multivariate factorization part 4. determining the leading
%  coefficients;


SYMBOLIC PROCEDURE DETERMINE!.LEADING!.COEFFTS();
% this function determines the leading coeffts to all but a constant
% factor which is spread over all of the factors before reconstruction;
  BEGIN SCALAR DELTA,C,S;
    S:=GETV(VALID!-IMAGE!-SETS,BEST!-SET!-POINTER);
    DELTA:=GET!-IMAGE!-CONTENT S;
            % cont(the m!-input!-polynomial image);
    IF NOT DOMAINP LC MULTIVARIATE!-INPUT!-POLY THEN
    << TRUE!-LEADING!-COEFFTS:=
      DISTRIBUTE!.LC(NUMBER!-OF!-FACTORS,IMAGE!-FACTORS,S,
        FACTORED!-LC);
       IF BAD!-CASE THEN <<
         BAD!-CASE:=LIST GET!-IMAGE!-SET S;
         TARGET!-FACTOR!-COUNT:=NUMBER!-OF!-FACTORS - 1;
         IF TARGET!-FACTOR!-COUNT=1 THEN IRREDUCIBLE:=T;
         RETURN BAD!-CASE >>;
       DELTA:=CAR TRUE!-LEADING!-COEFFTS;
       TRUE!-LEADING!-COEFFTS:=CDR TRUE!-LEADING!-COEFFTS;
            % if the lc problem exists then use wang's algorithm to
            % distribute it over the factors. ;
       IF NOT !*OVERVIEW THEN FACTOR!-TRACE <<
         PRINTSTR "We now determine the leading coefficients of the ";
         PRINTSTR "factors of U by using the factors of the leading";
         PRINTSTR "coefficient of U and their (square-free) images";
         PRINTSTR "referred to earlier:";
         FOR I:=1:NUMBER!-OF!-FACTORS DO <<
           PRINSF GETV(IMAGE!-FACTORS,I);
	   PRIN2!* " with l.c.: ";
	   FAC!-PRINTSF GETV(TRUE!-LEADING!-COEFFTS,I)
         >> >>;
       IF NOT ONEP DELTA THEN FACTOR!-TRACE <<
         IF !*OVERVIEW THEN
	<< PRINTSTR
	      "In determining the leading coefficients of the factors";
           PRIN2!* "of U, " >>;
         PRIN2!* "We have an integer factor, ";
         PRIN2!* DELTA;
         PRINTSTR ", left over that we ";
         PRINTSTR "cannot yet distribute correctly." >>
      >>
    ELSE <<
      TRUE!-LEADING!-COEFFTS:=MKVECT NUMBER!-OF!-FACTORS;
      FOR I:=1:NUMBER!-OF!-FACTORS DO
        PUTV(TRUE!-LEADING!-COEFFTS,I,LC GETV(IMAGE!-FACTORS,I));
      IF NOT ONEP DELTA THEN
        FACTOR!-TRACE <<
          PRIN2!* "U has a leading coefficient = ";
          PRIN2!* DELTA;
          PRINTSTR " which we cannot ";
          PRINTSTR "yet distribute correctly over the image factors." >>
      >>;
    IF NOT ONEP DELTA THEN
    << FOR I:=1:NUMBER!-OF!-FACTORS DO
       << PUTV(IMAGE!-FACTORS,I,MULTF(DELTA,GETV(IMAGE!-FACTORS,I)));
          PUTV(TRUE!-LEADING!-COEFFTS,I,
            MULTF(DELTA,GETV(TRUE!-LEADING!-COEFFTS,I)))
       >>;
       DIVIDE!-ALL!-ALPHAS DELTA;
       C:=EXPT(DELTA,ISUB1 NUMBER!-OF!-FACTORS);
       MULTIVARIATE!-INPUT!-POLY:=MULTF(C,MULTIVARIATE!-INPUT!-POLY);
       NON!-MONIC:=T;
       FACTOR!-TRACE <<
         PRINTSTR "(a) We multiply each of the image factors by the ";
         PRINTSTR "absolute value of this constant and multiply";
         PRIN2!* "U by ";
         IF NOT(NUMBER!-OF!-FACTORS=2) THEN
           << PRIN2!* DELTA; PRIN2!* "**";
             PRIN2!* ISUB1 NUMBER!-OF!-FACTORS >>
         ELSE PRIN2!* DELTA;
         PRINTSTR " giving new image factors";
         PRINTSTR "as follows: ";
         FOR I:=1:NUMBER!-OF!-FACTORS DO
	   FAC!-PRINTSF GETV(IMAGE!-FACTORS,I)
       >>
    >>;
            % if necessary, fiddle the remaining integer part of the
            % lc of m!-input!-polynomial;
  END;


%**********************************************************************;
% multivariate factorization part 5. reconstruction;


SYMBOLIC PROCEDURE RECONSTRUCT!-MULTIVARIATE!-FACTORS VSET!-MOD!-P;
% Hensel construction for multivariate case
% Full univariate split has already been prepared (if factoring);
% but we only need the modular factors and the true leading coeffts;
  (LAMBDA FACTOR!-LEVEL; BEGIN
    SCALAR S,OM,U0,ALPHAVEC,WTIME,PREDICTIONS,
      BEST!-FACTORS!-MOD!-P,FHATVEC,W1,FVEC!-MOD!-P,D,DEGREE!-BOUNDS,
      LC!-VEC;
    ALPHAVEC:=MKVECT NUMBER!-OF!-FACTORS;
    BEST!-FACTORS!-MOD!-P:=MKVECT NUMBER!-OF!-FACTORS;
    LC!-VEC := MKVECT NUMBER!-OF!-FACTORS;
	% This will preserve the LCs of the factors while we are working
	% mod p since they may contain numbers that are bigger than the
	% modulus.;
    IF NOT(
      (D:=MAX!-DEGREE(MULTIVARIATE!-INPUT!-POLY,0)) < PRIME!-BASE) THEN
      FVEC!-MOD!-P:=CHOOSE!-LARGER!-PRIME D;
    OM:=SET!-MODULUS HENSEL!-GROWTH!-SIZE;
    IF NULL FVEC!-MOD!-P THEN <<
      FVEC!-MOD!-P:=MKVECT NUMBER!-OF!-FACTORS;
      FOR I:=1:NUMBER!-OF!-FACTORS DO
        PUTV(FVEC!-MOD!-P,I,REDUCE!-MOD!-P GETV(IMAGE!-FACTORS,I)) >>;
    FOR I:=1:NUMBER!-OF!-FACTORS DO <<
      PUTV(ALPHAVEC,I,CDR GET!-ALPHA GETV(FVEC!-MOD!-P,I));
      PUTV(BEST!-FACTORS!-MOD!-P,I,
        REDUCE!-MOD!-P GETV(BEST!-KNOWN!-FACTORS,I));
      PUTV(LC!-VEC,I,LC GETV(BEST!-KNOWN!-FACTORS,I)) >>;
	 % Set up the Alphas, input factors mod p and remember to save
	 % the LCs for use after finding the multivariate factors mod p;
    IF NOT RECONSTRUCTING!-GCD THEN <<
      S:=GETV(VALID!-IMAGE!-SETS,BEST!-SET!-POINTER);
      VSET!-MOD!-P:=FOR EACH V IN GET!-IMAGE!-SET S COLLECT
        (CAR V . MODULAR!-NUMBER CDR V) >>;
%    PRINC "KORD* =";% PRINT KORD!*;
%    PRINC "ORDER OF VARIABLE SUBSTITUTION=";% PRINT VSET!-MOD!-P;
    U0:=REDUCE!-MOD!-P MULTIVARIATE!-INPUT!-POLY;
    SET!-DEGREE!-BOUNDS VSET!-MOD!-P;
    WTIME:=TIME();
    FACTOR!-TRACE <<
      PRINTSTR
	 "We use the Hensel Construction to grow univariate modular";
      PRINTSTR
	 "factors into multivariate modular factors, which will in";
      PRINTSTR "turn be used in the later Hensel construction.  The";
      PRINTSTR "starting modular factors are:";
      PRINTVEC(" f(",NUMBER!-OF!-FACTORS,")=",BEST!-FACTORS!-MOD!-P);
      PRIN2!* "The modulus is "; PRINTSTR CURRENT!-MODULUS >>;
    FIND!-MULTIVARIATE!-FACTORS!-MOD!-P(U0,
      BEST!-FACTORS!-MOD!-P,
      VSET!-MOD!-P);
    IF BAD!-CASE THEN <<
      TRACE!-TIME <<
        DISPLAY!-TIME(" Multivariate modular factors failed in ",
          TIME()-WTIME);
        WTIME:=TIME() >>;
      TARGET!-FACTOR!-COUNT:=NUMBER!-OF!-FACTORS - 1;
      IF TARGET!-FACTOR!-COUNT=1 THEN IRREDUCIBLE:=T;
      SET!-MODULUS OM;
      RETURN BAD!-CASE >>;
    TRACE!-TIME <<
      DISPLAY!-TIME(" Multivariate modular factors found in ",
        TIME()-WTIME);
      WTIME:=TIME() >>;
    FHATVEC:=MAKE!-MULTIVARIATE!-HATVEC!-MOD!-P(BEST!-FACTORS!-MOD!-P,
      NUMBER!-OF!-FACTORS);
    FOR I:=1:NUMBER!-OF!-FACTORS DO
      PUTV(FVEC!-MOD!-P,I,GETV(BEST!-FACTORS!-MOD!-P,I));
    MAKE!-VEC!-MODULAR!-SYMMETRIC(BEST!-FACTORS!-MOD!-P,
      NUMBER!-OF!-FACTORS);
    FOR I:=1:NUMBER!-OF!-FACTORS DO <<
%      W1:=GETV(COEFFT!-VECTORS,I);
%      PUTV(BEST!-KNOWN!-FACTORS,I,
%        MERGE!-TERMS(GETV(BEST!-FACTORS!-MOD!-P,I),W1));
      PUTV(BEST!-KNOWN!-FACTORS,I,
        FORCE!-LC(GETV(BEST!-FACTORS!-MOD!-P,I),GETV(LC!-VEC,I)));
	 % Now we put back the LCs before growing the multivariate
	 % factors to be correct over the integers giving the final
	 % result;
      >>;
    WTIME:=TIME();
    W1:=HENSEL!-MOD!-P(
      MULTIVARIATE!-INPUT!-POLY,
      FVEC!-MOD!-P,
      BEST!-KNOWN!-FACTORS,
      GET!.COEFFT!.BOUND(MULTIVARIATE!-INPUT!-POLY,
        TOTAL!-DEGREE!-IN!-POWERS(MULTIVARIATE!-INPUT!-POLY,NIL)),
      VSET!-MOD!-P,
      HENSEL!-GROWTH!-SIZE);
    IF CAR W1='OVERSHOT THEN <<
      TRACE!-TIME <<
        DISPLAY!-TIME(" Full factors failed in ",TIME()-WTIME);
        WTIME:=TIME() >>;
      TARGET!-FACTOR!-COUNT:=NUMBER!-OF!-FACTORS - 1;
      IF TARGET!-FACTOR!-COUNT=1 THEN IRREDUCIBLE:=T;
      SET!-MODULUS OM;
      RETURN BAD!-CASE:=T >>;
    IF NOT(CAR W1='OK) THEN ERRORF W1;
    TRACE!-TIME <<
      DISPLAY!-TIME(" Full factors found in ",TIME()-WTIME);
      WTIME:=TIME() >>;
    IF RECONSTRUCTING!-GCD THEN <<
      FULL!-GCD:=IF NON!-MONIC THEN CAR PRIMITIVE!.PARTS(
          LIST GETV(CDR W1,1),M!-IMAGE!-VARIABLE,NIL)
        ELSE GETV(CDR W1,1);
      SET!-MODULUS OM;
      RETURN FULL!-GCD >>;
    FOR I:=1:GETV(CDR W1,0) DO
      MULTIVARIATE!-FACTORS:=GETV(CDR W1,I) . MULTIVARIATE!-FACTORS;
    IF NON!-MONIC THEN MULTIVARIATE!-FACTORS:=
      PRIMITIVE!.PARTS(MULTIVARIATE!-FACTORS,M!-IMAGE!-VARIABLE,NIL);
    FACTOR!-TRACE <<
      PRINTSTR "The full multivariate factors are:";
      FOR EACH X IN MULTIVARIATE!-FACTORS DO FAC!-PRINTSF X >>;
    SET!-MODULUS OM;
  END) (FACTOR!-LEVEL*100);

SYMBOLIC PROCEDURE CHECK!-INVERTED MULTI!-FACLIST;
  BEGIN SCALAR INV!.SIGN,L;
    IF INVERTED THEN <<
      INV!.SIGN:=1;
      MULTI!-FACLIST:=
        FOR EACH X IN MULTI!-FACLIST COLLECT <<
        L:=INVERT!.POLY(X,M!-IMAGE!-VARIABLE);
        INV!.SIGN:=(CAR L) * INV!.SIGN;
        CDR L >>;
      IF NOT(INV!.SIGN=INVERTED!-SIGN) THEN
        ERRORF LIST("INVERSION HAS LOST A SIGN",INV!.SIGN) >>;
      RETURN MULTIVARIATE!-FACTORS:=MULTI!-FACLIST END;


ENDMODULE;


MODULE FACTOR;

% *******************************************************************
%
%   copyright (c)  university of cambridge, england 1979
%
% *******************************************************************;






% factorization of polynomials
%
% p. m. a. moore  1979.
%
%
%**********************************************************************;



SYMBOLIC PROCEDURE MULTIPLE!-RESULT(Z,W);
% z is a list of items (n . prefix-form), and the largest value
% of n must come first in this list. w is supposed to be an array
% name. the items in the list z are put into the array w;
  BEGIN
    SCALAR X,Y,N;
    N:=(LENGTH Z)-1;
    IF NOT IDP W THEN <<
      LPRIM "ANSWERS WILL BE IN 'ANS'";
      W:='ANS >>;
    IF ATOM W AND (Y := DIMENSION W) AND NULL CDR Y THEN <<
    % one dimensional array found;
      Y := CAR Y-1;
      IF CAAR Z>Y THEN REDERR "ARRAY TOO SMALL";
      WHILE NOT Y<0 DO <<
        IF NULL Z OR Y NEQ CAAR Z THEN SETELV(LIST(W,Y),0)
        ELSE << SETELV(LIST(W,Y),CDAR Z); Z := CDR Z >>;
        Y := Y-1 >>;
      RETURN !*N2F N ./ 1 >>;
    % here w was not the name of a 1-dimensional array, so i
    % will spread the results out into various discrete variables;
    Y := EXPLODE W;
    W := NIL;
    FOR EACH ZZ IN Z DO <<
      W := INTERN COMPRESS APPEND(Y,EXPLODE CAR ZZ) . W;
      SETK1(CAR W,CDR ZZ,T) >>;
    IF LENGTH W=1 THEN LPRIM ACONC(W,"IS NOW NON-ZERO")
        ELSE LPRIM ACONC(W,"ARE NOW NON-ZERO");
    RETURN !*N2F N ./ 1
  END;


%**********************************************************************;

SYMBOLIC PROCEDURE FACTORF U;
% This is the entry to the factorizer that is to be used
% by programmers working at the symbolic level. U is to
% be a standard form. FACTORF hands back a list giving the factors
% of U. The format of said list is described below in the
% comments with FACTORIZE!-FORM.
% Entry to the factorizer at any level other than this is at
% the programmers own risk!! ;
    FACTORF1(U,NIL);

SYMBOLIC PROCEDURE FACTORF1(U,!*FORCE!-PRIME);
% This entry to the factorizer allows one to force
% the code to use some particular prime for its
% modular factorization. It is not for casual
% use;
  BEGIN
    SCALAR FACTOR!-LEVEL,BASE!-TIME,LAST!-DISPLAYED!-TIME,
      GC!-BASE!-TIME,LAST!-DISPLAYED!-GC!-TIME,GCDSAVE,
      CURRENT!-MODULUS,MODULUS!/2,W;
    GCDSAVE := !*GCD;
    !*GCD := T; % This code will not work otherwise! ;
    SET!-TIME();
    FACTOR!-LEVEL := 0;
    W := FACTORIZE!-FORM U;
    !*GCD := GCDSAVE;
    RETURN W
  END;



%**********************************************************************;

SYMBOLIC PROCEDURE FACTORIZE!-FORM P;
% input:
% p is a reduce standard form that is to be factorized
% over the integers
% result:      (nc . l)
%  where nc is numeric (may be just 1)
%  and l is list of the form:
%    ((p1 . x1) (p2 . x2) .. (pn . xn))
% where p<i> are standard forms and x<i> are integers,
% and p= product<i> p<i>**x<i>;
%
% method:
% (a) reorder polynomial to make the variable of lowest maximum
% degree the main one and the rest ordered similarly;
% (b) use contents and primitive parts to split p up as far as possible
% (c) use square-free decomposition to continue the process
% (c.1) detect & perform special processing on cyclotomic polynomials
% (d) use modular-based method to find factors over integers;
  BEGIN SCALAR NEW!-KORDER,OLD!-KORDER;
    NEW!-KORDER:=KERNORD(P,POLYZERO);
    IF !*KERNREVERSE THEN NEW!-KORDER:=REVERSE NEW!-KORDER;
    OLD!-KORDER:=SETKORDER NEW!-KORDER;
    P:=REORDER P; % Make var of lowest degree the main one;
    P:=FACTORIZE!-FORM1(P,NEW!-KORDER);
    SETKORDER OLD!-KORDER;
    P := (CAR P . FOR EACH W IN CDR P COLLECT
           (REORDER CAR W . CDR W));
    IF MINUSP CAR P AND NOT CDR P=NIL THEN
       P := (- CAR P) . (NEGF CAADR P . CDADR P) . CDDR P;
    RETURN P
  END;

SYMBOLIC PROCEDURE FACTORIZE!-FORM1(P,GIVEN!-KORDER);
% input:
% p is a reduce standard form that is to be factorized
% over the integers
% given-korder is a list of kernels in the order of importance
% (ie when finding leading terms etc. we use this list)
% See FACTORIZE-FORM above;
  IF DOMAINP P THEN (P . NIL)
  ELSE BEGIN SCALAR M!-IMAGE!-VARIABLE,VAR!-LIST,
		    POLYNOMIAL!-TO!-FACTOR,N;
    IF !*ALL!-CONTENTS THEN VAR!-LIST:=GIVEN!-KORDER
    ELSE <<
      M!-IMAGE!-VARIABLE:=CAR GIVEN!-KORDER;
      VAR!-LIST:=LIST M!-IMAGE!-VARIABLE >>;
    RETURN (LAMBDA FACTOR!-LEVEL;
     << FACTOR!-TRACE <<
	  PRIN2!* "FACTOR : "; FAC!-PRINTSF P;
          PRIN2!* "Chosen main variable is ";
          PRINTVAR M!-IMAGE!-VARIABLE >>;
        POLYNOMIAL!-TO!-FACTOR:=P;
        N:=NUMERIC!-CONTENT P;
        P:=QUOTF(P,N);
        IF POLY!-MINUSP P THEN <<
          P:=NEGF P;
          N:=-N >>;
        FACTOR!-TRACE <<
          PRIN2!* "Numeric content = ";
	  FAC!-PRINTSF N >>;
        P:=FACTORIZE!-BY!-CONTENTS(P,VAR!-LIST);
        P:=N . SORT!-FACTORS P;
        FACTOR!-TRACE <<
          TERPRI(); TERPRI();
	  PRINTSTR "Final result is:";  FAC!-PRINTFACTORS P >>;
        P >>)
        (FACTOR!-LEVEL+1)
  END;


SYMBOLIC PROCEDURE FACTORIZE!-FORM!-RECURSION P;
% this is essentially the same as FACTORIZE!-FORM except that
% we must be careful of stray minus signs due to a possible
% reordering in the recursive factoring;
  BEGIN SCALAR S,N,X,RES,NEW!-KORDER,OLD!-KORDER;
    NEW!-KORDER:=KERNORD(P,POLYZERO);
    IF !*KERNREVERSE THEN NEW!-KORDER:=REVERSE NEW!-KORDER;
    OLD!-KORDER:=SETKORDER NEW!-KORDER;
    P:=REORDER P; % Make var of lowest degree the main one;
    X:=FACTORIZE!-FORM1(P,NEW!-KORDER);
    SETKORDER OLD!-KORDER;
    N := CAR X;
    X := FOR EACH P IN CDR X COLLECT (REORDER CAR P . CDR P);
    IF MINUSP N THEN << S:=-1; N:=-N >> ELSE S:=1;
    RES:=FOR EACH FF IN X COLLECT
      IF POLY!-MINUSP CAR FF THEN <<
        S:=S*(-1**CDR FF);
        (NEGF CAR FF . CDR FF) >>
      ELSE FF;
    IF MINUSP S THEN ERRORF LIST(
      "Stray minus sign in recursive factorisation:",X);
    RETURN (N . RES)
  END;

SYMBOLIC PROCEDURE SORT!-FACTORS L;
%sort factors as found into some sort of standard order. The order
%used here is more or less random, but will be self-consistent;
    SORT(L,FUNCTION ORDERFACTORS);




%**********************************************************************;
% contents and primitive parts as applied to factorization;



SYMBOLIC PROCEDURE FACTORIZE!-BY!-CONTENTS(P,V);
%use contents wrt variables in list v to split the
%polynomial p. return a list of factors;
% specification is that on entry p *must* be positive;
    IF DOMAINP P THEN
      ERRORF LIST("FACTORIZE-BY-CONTENTS HANDED DOMAIN ELT:",P)
    ELSE IF NULL V THEN SQUARE!.FREE!.FACTORIZE P
    ELSE BEGIN SCALAR C,W,L,WTIME;
        W:=CONTENTS!-WITH!-RESPECT!-TO(P,CAR V);
% contents!-with!-respect!-to returns a pair (g . c) where
% if g=nil the content is just c, otherwise g is a power
% [ x ** n ] and g*c is the content;
        IF NOT NULL CAR W THEN <<
% here a power of v divides p;
            L:=(!*K2F CAAR W . CDAR W) . NIL;
            P:=QUOTFAIL(P,!*P2F CAR W);
            IF P=1 THEN RETURN L
            ELSE IF DOMAINP P THEN
                ERRORF "P SHOULD NOT BE CONSTANT HERE" >>;
        C:=CDR W;
        IF C=1 THEN << %no progress here;
          IF NULL L THEN
            FACTOR!-TRACE << PRIN2!* "Polynomial is primitive wrt ";
              PRINVAR CAR V; TERPRI!*(NIL) >>
          ELSE FACTOR!-TRACE << PRINTSTR "Content is: ";
	      FAC!-PRINTFACTORS(1 . L) >>;
          RETURN IF !*ALL!-CONTENTS THEN
            APPEND(FACTORIZE!-BY!-CONTENTS(P,CDR V),L)
          ELSE APPEND(SQUARE!.FREE!.FACTORIZE P,L) >>;
        P:=QUOTFAIL(P,C); %primitive part;
% p is now primitive, so if it is not a real polynomial it
% must be a unit. since input was +ve it had better be +1 !! ;
        IF P=-1 THEN
          ERRORF "NEGATIVE PRIMITIVE PART IN FACTORIZE-BY-CONTENTS";
        TRACE!-TIME PRINTC "Factoring the content:";
        WTIME:=TIME();
        L:=APPEND(CDR1 FACTORIZE!-FORM!-RECURSION C,L);
        TRACE!-TIME DISPLAY!-TIME("Content factored in ",
          TIME()-WTIME);
        FACTOR!-TRACE <<
          PRIN2!* "Content wrt "; PRINVAR CAR V; PRIN2!* " is: ";
	  FAC!-PRINTSF COMFAC!-TO!-POLY W;
          PRINTSTR "Factors of content are: ";
	  FAC!-PRINTFACTORS(1 . L) >>;
        IF P=1 THEN RETURN L
        ELSE IF !*ALL!-CONTENTS THEN
            RETURN APPEND(FACTORIZE!-BY!-CONTENTS(P,CDR V),L)
        ELSE RETURN APPEND(SQUARE!.FREE!.FACTORIZE P,L)
    END;

SYMBOLIC PROCEDURE CDR1 A;
  IF CAR A=1 THEN CDR A
  ELSE ERRORF LIST("NUMERIC CONTENT NOT EXTRACTED:",CAR A);





ENDMODULE;


MODULE FACUNI;

% *******************************************************************
%
%   copyright (c)  university of cambridge, england 1979
%
% *******************************************************************;




SYMBOLIC PROCEDURE UNIVARIATE!-FACTORIZE POLY;
% input poly a primitive square-free univariate polynomial at least
% quadratic and with +ve lc.  output is a list of the factors of poly
% over the integers ;
  IF TESTX!*!*N!+1 POLY THEN
    FACTORIZEX!*!*N!+1(M!-IMAGE!-VARIABLE,LDEG POLY,1)
  ELSE IF TESTX!*!*N!-1 POLY THEN
    FACTORIZEX!*!*N!-1(M!-IMAGE!-VARIABLE,LDEG POLY,1)
  ELSE UNIVARIATE!-FACTORIZE1 POLY;

SYMBOLIC PROCEDURE UNIVARIATE!-FACTORIZE1 POLY;
  BEGIN SCALAR
    VALID!-PRIMES,UNIVARIATE!-INPUT!-POLY,BEST!-SET!-POINTER,
    NUMBER!-OF!-FACTORS,IRREDUCIBLE,FORBIDDEN!-PRIMES,
    NO!-OF!-BEST!-PRIMES,NO!-OF!-RANDOM!-PRIMES,BAD!-CASE,
    TARGET!-FACTOR!-COUNT,MODULAR!-INFO,UNIVARIATE!-FACTORS,
    HENSEL!-GROWTH!-SIZE,ALPHALIST,PREVIOUS!-DEGREE!-MAP,
    ONE!-COMPLETE!-DEG!-ANALYSIS!-DONE,REDUCTION!-COUNT;
%note that this code works by using a local database of
%fluid variables that are updated by the subroutines directly
%called here. this allows for the relativly complicated
%interaction between flow of data and control that occurs in
%the factorization algorithm;
    FACTOR!-TRACE <<
      PRIN2!* "Univariate polynomial="; FAC!-PRINTSF POLY;
      PRINTSTR
	 "The polynomial is univariate, primitive and square-free";
      PRINTSTR "so we can treat it slightly more specifically. We";
      PRINTSTR "factorise mod several primes,then pick the best one";
      PRINTSTR "to use in the Hensel construction." >>;
    INITIALIZE!-UNIVARIATE!-FLUIDS POLY;
            % set up the fluids to start things off;
TRYAGAIN:
    GET!-SOME!-RANDOM!-PRIMES();
    CHOOSE!-THE!-BEST!-PRIME();
      IF IRREDUCIBLE THEN <<
        UNIVARIATE!-FACTORS:=LIST UNIVARIATE!-INPUT!-POLY;
        GOTO EXIT >>
      ELSE IF BAD!-CASE THEN <<
        BAD!-CASE:=NIL; GOTO TRYAGAIN >>;
    RECONSTRUCT!-FACTORS!-OVER!-INTEGERS();
      IF IRREDUCIBLE THEN <<
        UNIVARIATE!-FACTORS:=LIST UNIVARIATE!-INPUT!-POLY;
        GOTO EXIT >>;
EXIT:
    FACTOR!-TRACE <<
      PRINTSTR "The univariate factors are:";
      FOR EACH FF IN UNIVARIATE!-FACTORS DO FAC!-PRINTSF FF >>;
    RETURN UNIVARIATE!-FACTORS
   END;


%**********************************************************************
% univariate factorization part 1. initialization and setting fluids;


SYMBOLIC PROCEDURE INITIALIZE!-UNIVARIATE!-FLUIDS U;
% Set up the fluids to be used in factoring primitive poly;
  BEGIN SCALAR W,W1;
    IF !*FORCE!-PRIME THEN <<
      NO!-OF!-RANDOM!-PRIMES:=1;
      NO!-OF!-BEST!-PRIMES:=1 >>
    ELSE <<
      NO!-OF!-RANDOM!-PRIMES:=5;
            % we generate this many modular images and calculate
            % their factor counts;
      NO!-OF!-BEST!-PRIMES:=3;
            % we find the modular factors of this many;
      >>;
    UNIVARIATE!-INPUT!-POLY:=U;
    TARGET!-FACTOR!-COUNT:=LDEG U
  END;


%**********************************************************************;
% univariate factorization part 2. creating modular images and picking
%  the best one;


SYMBOLIC PROCEDURE GET!-SOME!-RANDOM!-PRIMES();
% here we create a number of random primes to reduce the input mod p;
  BEGIN SCALAR CHOSEN!-PRIME,POLY!-MOD!-P,I;
    VALID!-PRIMES:=MKVECT NO!-OF!-RANDOM!-PRIMES;
    I:=0;
    WHILE I < NO!-OF!-RANDOM!-PRIMES DO <<
      POLY!-MOD!-P:=
        FIND!-A!-VALID!-PRIME(LC UNIVARIATE!-INPUT!-POLY,
                    UNIVARIATE!-INPUT!-POLY,NIL);
      IF NOT(POLY!-MOD!-P='NOT!-SQUARE!-FREE) THEN <<
        I:=IADD1 I;
        PUTV(VALID!-PRIMES,I,CHOSEN!-PRIME . POLY!-MOD!-P);
        FORBIDDEN!-PRIMES:=CHOSEN!-PRIME . FORBIDDEN!-PRIMES
        >>
      >>
  END;

SYMBOLIC PROCEDURE CHOOSE!-THE!-BEST!-PRIME();
% given several random primes we now choose the best by factoring
% the poly mod its chosen prime and taking one with the
% lowest factor count as the best for hensel growth;
  BEGIN SCALAR SPLIT!-LIST,POLY!-MOD!-P,NULL!-SPACE!-BASIS,
               KNOWN!-FACTORS,W,N;
    MODULAR!-INFO:=MKVECT NO!-OF!-RANDOM!-PRIMES;
    FOR I:=1:NO!-OF!-RANDOM!-PRIMES DO <<
      W:=GETV(VALID!-PRIMES,I);
      GET!-FACTOR!-COUNT!-MOD!-P(I,CDR W,CAR W,NIL) >>;
    SPLIT!-LIST:=SORT(SPLIT!-LIST,FUNCTION LESSPPAIR);
            % this now contains a list of pairs (m . n) where
            % m is the no: of factors in set no: n. the list
            % is sorted with best split (smallest m) first;
    IF CAAR SPLIT!-LIST = 1 THEN <<
      IRREDUCIBLE:=T; RETURN NIL >>;
    W:=SPLIT!-LIST;
    FOR I:=1:NO!-OF!-BEST!-PRIMES DO <<
      N:=CDAR W;
      GET!-FACTORS!-MOD!-P(N,CAR GETV(VALID!-PRIMES,N));
      W:=CDR W >>;
            % pick the best few of these and find out their
            % factors mod p;
    SPLIT!-LIST:=DELETE(W,SPLIT!-LIST);
            % throw away the other sets;
    CHECK!-DEGREE!-SETS(NO!-OF!-BEST!-PRIMES,NIL);
            % the best set is pointed at by best!-set!-pointer;
    ONE!-COMPLETE!-DEG!-ANALYSIS!-DONE:=T;
    FACTOR!-TRACE <<
      W:=GETV(VALID!-PRIMES,BEST!-SET!-POINTER);
      PRIN2!* "The chosen prime is "; PRINTSTR CAR W;
      PRIN2!* "The polynomial mod "; PRIN2!* CAR W;
      PRINTSTR ", made monic, is:";
      FAC!-PRINTSF CDR W;
      PRINTSTR "and the factors of this modular polynomial are:";
      FOR EACH X IN GETV(MODULAR!-INFO,BEST!-SET!-POINTER)
	 DO FAC!-PRINTSF X;
      >>
  END;



%**********************************************************************;
% univariate factorization part 3. reconstruction of the
% chosen image over the integers;


SYMBOLIC PROCEDURE RECONSTRUCT!-FACTORS!-OVER!-INTEGERS();
% the hensel construction from modular case to univariate
% over the integers;
  BEGIN SCALAR BEST!-MODULUS,BEST!-FACTOR!-COUNT,INPUT!-POLYNOMIAL,
    INPUT!-LEADING!-COEFFICIENT,BEST!-KNOWN!-FACTORS,S;
    S:=GETV(VALID!-PRIMES,BEST!-SET!-POINTER);
    BEST!-KNOWN!-FACTORS:=GETV(MODULAR!-INFO,BEST!-SET!-POINTER);
    INPUT!-LEADING!-COEFFICIENT:=LC UNIVARIATE!-INPUT!-POLY;
    BEST!-MODULUS:=CAR S;
    BEST!-FACTOR!-COUNT:=LENGTH BEST!-KNOWN!-FACTORS;
    INPUT!-POLYNOMIAL:=UNIVARIATE!-INPUT!-POLY;
    UNIVARIATE!-FACTORS:=RECONSTRUCT!.OVER!.INTEGERS();
    IF IRREDUCIBLE THEN RETURN T;
    NUMBER!-OF!-FACTORS:=LENGTH UNIVARIATE!-FACTORS;
    IF NUMBER!-OF!-FACTORS=1 THEN RETURN IRREDUCIBLE:=T
  END;


SYMBOLIC PROCEDURE RECONSTRUCT!.OVER!.INTEGERS();
  BEGIN SCALAR W,LCLIST,NON!-MONIC;
    SET!-MODULUS BEST!-MODULUS;
    FOR I:=1:BEST!-FACTOR!-COUNT DO
      LCLIST:=INPUT!-LEADING!-COEFFICIENT . LCLIST;
    IF NOT (INPUT!-LEADING!-COEFFICIENT=1) THEN <<
      BEST!-KNOWN!-FACTORS:=
        FOR EACH FF IN BEST!-KNOWN!-FACTORS COLLECT
          MULTF(INPUT!-LEADING!-COEFFICIENT,!*MOD2F FF);
      NON!-MONIC:=T;
      FACTOR!-TRACE <<
	PRINTSTR
	   "(a) Now the polynomial is not monic so we multiply each";
	PRINTSTR
	   "of the modular factors, f(i), by the absolute value of";
        PRIN2!* "the leading coefficient: ";
        PRIN2!* INPUT!-LEADING!-COEFFICIENT; PRINTSTR '!.;
        PRINTSTR "To bring the polynomial into agreement with this, we";
        PRIN2!* "multiply it by ";
        IF BEST!-FACTOR!-COUNT > 2 THEN
          << PRIN2!* INPUT!-LEADING!-COEFFICIENT; PRIN2!* "**";
            PRINTSTR ISUB1 BEST!-FACTOR!-COUNT >>
        ELSE PRINTSTR INPUT!-LEADING!-COEFFICIENT >> >>;
    W:=UHENSEL!.EXTEND(INPUT!-POLYNOMIAL,
      BEST!-KNOWN!-FACTORS,LCLIST,BEST!-MODULUS);
    IF IRREDUCIBLE THEN RETURN T;
    IF CAR W ='OK THEN RETURN CDR W
    ELSE ERRORF W
  END;


% Now some special treatment for cyclotomic polynomials;

SYMBOLIC PROCEDURE TESTX!*!*N!+1 U;
  NOT DOMAINP U AND (
    LC U=1 AND
    RED U = 1);


SYMBOLIC PROCEDURE TESTX!*!*N!-1 U;
  NOT DOMAINP U AND (
    LC U=1 AND
    RED U = -1);


SYMBOLIC PROCEDURE FACTORIZEX!*!*N!+1(VAR,DEGREE,VORDER);
% Deliver factors of (VAR**VORDER)**DEGREE+1 given that it is
% appropriate to treat VAR**VORDER as a kernel;
  IF EVENP DEGREE THEN FACTORIZEX!*!*N!+1(VAR,DEGREE/2,2*VORDER)
  ELSE BEGIN
    SCALAR W;
    W := FACTORIZEX!*!*N!-1(VAR,DEGREE,VORDER);
    W := NEGF CAR W . CDR W;
    RETURN FOR EACH P IN W COLLECT NEGATE!-VARIABLE(VAR,2*VORDER,P)
  END;

SYMBOLIC PROCEDURE NEGATE!-VARIABLE(VAR,VORDER,P);
% VAR**(VORDER/2) -> -VAR**(VORDER/2) in the polynomial P;
  IF DOMAINP P THEN P
  ELSE IF MVAR P=VAR THEN
    IF REMAINDER(LDEG P,VORDER)=0 THEN
            LT P .+ NEGATE!-VARIABLE(VAR,VORDER,RED P)
    ELSE (LPOW P .* NEGF LC P) .+ NEGATE!-VARIABLE(VAR,VORDER,RED P)
  ELSE (LPOW P .* NEGATE!-VARIABLE(VAR,VORDER,LC P)) .+
        NEGATE!-VARIABLE(VAR,VORDER,RED P);


SYMBOLIC PROCEDURE INTEGER!-FACTORS N;
% Return integer factors of N, with attached multiplicities. Assumes
% that N is fairly small;
  BEGIN
    SCALAR L,Q,M,W;
% L is list of results generated so far, Q is current test divisor,
% and M is associated multiplicity;
    IF N=1 THEN RETURN '((1 . 1));
    Q := 2; M := 0;
TOP:
    W := DIVIDE(N,Q);
    WHILE CDR W=0 DO << N := CAR W; W := DIVIDE(N,Q); M := M+1 >>;
    IF NOT M=0 THEN L := (Q . M) . L;
    IF Q>CAR W THEN <<
      IF NOT N=1 THEN L := (N . 1) . L;
      RETURN REVERSEWOC L >>;
    Q := ILOGOR(1,IADD1 Q); % Test divide by 2,3,5,7,9,11,13,... ;
    M := 0;
    GO TO TOP
  END;


SYMBOLIC PROCEDURE FACTORED!-DIVISORS FL;
% FL is an association list of primes and exponents. Return a list
% of all subsets of this list, i.e. of numbers dividing the
% original integer. Exclude '1' from the list;
  IF NULL FL THEN NIL
  ELSE BEGIN
    SCALAR L,W;
    W := FACTORED!-DIVISORS CDR FL;
    L := W;
    FOR I := 1:CDAR FL DO <<
      L := LIST (CAAR FL . I) . L;
      FOR EACH P IN W DO
        L := ((CAAR FL . I) . P) . L >>;
    RETURN L
  END;

SYMBOLIC PROCEDURE FACTORIZEX!*!*N!-1(VAR,DEGREE,VORDER);
  IF EVENP DEGREE THEN APPEND(FACTORIZEX!*!*N!+1(VAR,DEGREE/2,VORDER),
                              FACTORIZEX!*!*N!-1(VAR,DEGREE/2,VORDER))
  ELSE IF DEGREE=1 THEN LIST((MKSP(VAR,VORDER) .* 1) .+ (-1))
  ELSE BEGIN
    SCALAR FACDEG,L;
    FACDEG := '((1 . 1)) . FACTORED!-DIVISORS INTEGER!-FACTORS DEGREE;
    RETURN FOR EACH FL IN FACDEG
       COLLECT CYCLOTOMIC!-POLYNOMIAL(VAR,FL,VORDER)
  END;

SYMBOLIC PROCEDURE CYCLOTOMIC!-POLYNOMIAL(VAR,FL,VORDER);
% Create Psi<degree>(var**order)
% where degree is given by the association list of primes and
% multiplicities FL;
  IF NOT CDAR FL=1 THEN
    CYCLOTOMIC!-POLYNOMIAL(VAR,(CAAR FL . SUB1 CDAR FL) . CDR FL,
			   VORDER*CAAR FL)
  ELSE IF CDR FL=NIL THEN
     IF CAAR FL=1 THEN (MKSP(VAR,VORDER) .* 1) .+ (-1)
     ELSE QUOTFAIL((MKSP(VAR,VORDER*CAAR FL) .* 1) .+ (-1),
                   (MKSP(VAR,VORDER) .* 1) .+ (-1))
  ELSE QUOTFAIL(CYCLOTOMIC!-POLYNOMIAL(VAR,CDR FL,VORDER*CAAR FL),
                CYCLOTOMIC!-POLYNOMIAL(VAR,CDR FL,VORDER));



ENDMODULE;


MODULE IMAGESET;

% *******************************************************************
%
%   copyright (c)  university of cambridge, england 1979
%
% *******************************************************************;




%*******************************************************************;
%
%      this section deals with the image sets used in
%      factorising multivariate polynomials according
%      to wang's theories.
%       ref:  math. comp. vol.32 no.144 oct 1978 pp 1217-1220
%        'an improved multivariate polynomial factoring algorithm'
%
%*******************************************************************;


%*******************************************************************;
%    first we have routines for generating the sets
%*******************************************************************;


SYMBOLIC PROCEDURE GENERATE!-AN!-IMAGE!-SET!-WITH!-PRIME
		      GOOD!-SET!-NEEDED;
% given a multivariate poly (in a fluid) we generate an image set
% to make it univariate and also a random prime to use in the
% modular factorization. these numbers are random except that
% we will not allow anything in forbidden!-sets or forbidden!-primes;
  BEGIN SCALAR CURRENTLY!-FORBIDDEN!-SETS,U,WTIME;
    U:=MULTIVARIATE!-INPUT!-POLY;
            % a bit of a handful to type otherwise!!!!   ;
    IMAGE!-SET:=NIL;
    CURRENTLY!-FORBIDDEN!-SETS:=FORBIDDEN!-SETS;
TRYANOTHERSET:
    IF IMAGE!-SET THEN
      CURRENTLY!-FORBIDDEN!-SETS:=IMAGE!-SET .
                                CURRENTLY!-FORBIDDEN!-SETS;
    WTIME:=TIME();
    IMAGE!-SET:=GET!-NEW!-SET CURRENTLY!-FORBIDDEN!-SETS;
%           PRINC "Trying imageset= ";
%           PRINTC IMAGE!-SET;
    TRACE!-TIME <<
      DISPLAY!-TIME("    New image set found in ",TIME()-WTIME);
      WTIME:=TIME() >>;
    IMAGE!-LC:=MAKE!-IMAGE!-LC!-LIST(LC U,IMAGE!-SET);
            % list of image lc's wrt different variables in IMAGE-SET;
%    PRINC "Image set to try is:";% PRINTC IMAGE!-SET;
%    PRIN2!* "L.C. of poly is:";% FAC!-PRINTSF LC U;
%    PRINTC "Image l.c.s with variables substituted on order:";
%    FOR EACH IMLC IN IMAGE!-LC DO FAC!-PRINTSF IMLC;
    TRACE!-TIME
      DISPLAY!-TIME("    Image of lc made in ",TIME()-WTIME);
    IF (CAAR IMAGE!-LC)=0 THEN GOTO TRYANOTHERSET;
    WTIME:=TIME();
    IMAGE!-POLY:=MAKE!-IMAGE(U,IMAGE!-SET);
    TRACE!-TIME <<
      DISPLAY!-TIME("    Image poly made in ",TIME()-WTIME);
      WTIME:=TIME() >>;
    IMAGE!-CONTENT:=GET!.CONTENT IMAGE!-POLY;
            % note: the content contains the image variable if it
            % is a factor of the image poly;
    TRACE!-TIME
      DISPLAY!-TIME("    Content found in ",TIME()-WTIME);
    IMAGE!-POLY:=QUOTFAIL(IMAGE!-POLY,IMAGE!-CONTENT);
            % make sure the image polynomial is primitive which includes
	    % making the leading coefft positive (-ve content if
	    % necessary);
    WTIME:=TIME();
    IMAGE!-MOD!-P:=FIND!-A!-VALID!-PRIME(IMAGE!-LC,IMAGE!-POLY,
      NOT NUMBERP IMAGE!-CONTENT);
    IF IMAGE!-MOD!-P='NOT!-SQUARE!-FREE THEN GOTO TRYANOTHERSET;
    TRACE!-TIME <<
      DISPLAY!-TIME("    Prime and image mod p found in ",TIME()-WTIME);
      WTIME:=TIME() >>;
    IF FACTORED!-LC THEN
      IF F!-NUMVEC:=UNIQUE!-F!-NOS(FACTORED!-LC,IMAGE!-CONTENT,
          IMAGE!-SET) THEN <<
        USABLE!-SET!-FOUND:=T;
        TRACE!-TIME
          DISPLAY!-TIME("    Nos for lc found in ",TIME()-WTIME) >>
      ELSE <<
        TRACE!-TIME DISPLAY!-TIME("    Nos for lc failed in ",
            TIME()-WTIME);
        IF (NOT USABLE!-SET!-FOUND) AND GOOD!-SET!-NEEDED THEN
          GOTO TRYANOTHERSET >>
  END;


SYMBOLIC PROCEDURE GET!-NEW!-SET FORBIDDEN!-S;
% associate each variable in vars-to-kill with a random no. mod
% image-set-modulus. If the boolean tagged with a variable is true then
% a value of 1 or 0 is no good and so rejected, however all other
% variables can take these values so they are tried exhaustively before
% using truly random values. sets in forbidden!-s not allowed;
  BEGIN SCALAR OLD!.M,ALIST,N,NEXTZSET,W;
    IF ZERO!-SET!-TRIED THEN <<
      IF !*FORCE!-ZERO!-SET THEN
        ERRORF "Zero set tried - possibly it was invalid";
      IMAGE!-SET!-MODULUS:=IADD1 IMAGE!-SET!-MODULUS;
      OLD!.M:=SET!-MODULUS IMAGE!-SET!-MODULUS;
      ALIST:=FOR EACH V IN VARS!-TO!-KILL COLLECT
      << N:=MODULAR!-NUMBER RANDOM();
         IF N>MODULUS!/2 THEN N:=N-CURRENT!-MODULUS;
         IF CDR V THEN <<
           WHILE N=0
              OR N=1
              OR (N = (ISUB1 CURRENT!-MODULUS)) DO
             N:=MODULAR!-NUMBER RANDOM();
           IF N>MODULUS!/2 THEN N:=N-CURRENT!-MODULUS >>;
         CAR V . N >> >>
    ELSE <<
      OLD!.M:=SET!-MODULUS IMAGE!-SET!-MODULUS;
      NEXTZSET:=CAR ZSET;
      ALIST:=FOR EACH ZV IN ZEROVARSET COLLECT <<
        W:=ZV . CAR NEXTZSET;
        NEXTZSET:=CDR NEXTZSET;
        W >>;
      IF OTHERVARS THEN ALIST:=
        APPEND(ALIST,FOR EACH V IN OTHERVARS COLLECT <<
          N:=MODULAR!-NUMBER RANDOM();
          WHILE N=0
             OR N=1
             OR (N = (ISUB1 CURRENT!-MODULUS)) DO
            N:=MODULAR!-NUMBER RANDOM();
          IF N>MODULUS!/2 THEN N:=N-CURRENT!-MODULUS;
          V . N >>);
      IF NULL(ZSET:=CDR ZSET) THEN
        IF NULL SAVE!-ZSET THEN ZERO!-SET!-TRIED:=T
        ELSE ZSET:=MAKE!-NEXT!-ZSET SAVE!-ZSET;
      ALIST:=FOR EACH V IN CDR KORD!* COLLECT ATSOC(V,ALIST);
            % Puts the variables in alist in the right order;
      >>;
    SET!-MODULUS OLD!.M;
    RETURN IF MEMBER(ALIST,FORBIDDEN!-S) THEN
        GET!-NEW!-SET FORBIDDEN!-S
      ELSE ALIST
  END;


%**********************************************************************
% now given an image/univariate polynomial find a suitable random prime;


SYMBOLIC PROCEDURE FIND!-A!-VALID!-PRIME(LC!-U,U,FACTOR!-X);
% finds a suitable random prime for reducing a poly mod p.
% u is the image/univariate poly. we are not allowed to use
% any of the primes in forbidden!-primes (fluid).
% lc!-u is either numeric or (in the multivariate case) a list of
% images of the lc;
  BEGIN SCALAR CURRENTLY!-FORBIDDEN!-PRIMES,RES,PRIME!-COUNT,V,W;
    IF FACTOR!-X THEN U:=MULTF(U,V:=!*K2F M!-IMAGE!-VARIABLE);
    CHOSEN!-PRIME:=NIL;
    CURRENTLY!-FORBIDDEN!-PRIMES:=FORBIDDEN!-PRIMES;
    PRIME!-COUNT:=1;
TRYANOTHERPRIME:
    IF CHOSEN!-PRIME THEN
      CURRENTLY!-FORBIDDEN!-PRIMES:=CHOSEN!-PRIME .
                                 CURRENTLY!-FORBIDDEN!-PRIMES;
    CHOSEN!-PRIME:=GET!-NEW!-PRIME CURRENTLY!-FORBIDDEN!-PRIMES;
    SET!-MODULUS CHOSEN!-PRIME;
    IF NOT ATOM LC!-U THEN <<
      W:=LC!-U;
      WHILE W AND
           ((DOMAINP CAAR W AND NOT(MODULAR!-NUMBER CAAR W = 0))
        OR NOT (DOMAINP CAAR W OR
                MODULAR!-NUMBER L!-NUMERIC!-C(CAAR W,CDAR W)=0)) DO
        W:=CDR W;
      IF W THEN GOTO TRYANOTHERPRIME >>
    ELSE IF MODULAR!-NUMBER LC!-U=0 THEN GOTO TRYANOTHERPRIME;
    RES:=MONIC!-MOD!-P REDUCE!-MOD!-P U;
    IF NOT SQUARE!-FREE!-MOD!-P RES THEN
      IF MULTIVARIATE!-INPUT!-POLY
        AND (PRIME!-COUNT:=PRIME!-COUNT+1)>5 THEN
        RES:='NOT!-SQUARE!-FREE
      ELSE GOTO TRYANOTHERPRIME;
    IF FACTOR!-X AND NOT(RES='NOT!-SQUARE!-FREE) THEN
      RES:=QUOTFAIL!-MOD!-P(RES,!*F2MOD V);
    RETURN RES
 END;

SYMBOLIC PROCEDURE GET!-NEW!-PRIME FORBIDDEN!-P;
% get a small prime that is not in the list forbidden!-p;
% we pick one of the first 10 primes if we can;
  IF !*FORCE!-PRIME THEN !*FORCE!-PRIME
  ELSE BEGIN SCALAR P,PRIMES!-DONE;
    FOR EACH PP IN FORBIDDEN!-P DO
      IF PP<32 THEN PRIMES!-DONE:=PP.PRIMES!-DONE;
TRYAGAIN:
    IF NULL(P:=RANDOM!-TEENY!-PRIME PRIMES!-DONE) THEN <<
      P:=RANDOM!-SMALL!-PRIME();
      PRIMES!-DONE:='ALL >>
    ELSE PRIMES!-DONE:=P . PRIMES!-DONE;
    IF MEMBER(P,FORBIDDEN!-P) THEN GOTO TRYAGAIN;
    RETURN P
  END;

%***********************************************************************
% find the numbers associated with each factor of the leading
% coefficient of our multivariate polynomial. this will help
% to distribute the leading coefficient later.;



SYMBOLIC PROCEDURE UNIQUE!-F!-NOS(V,CONT!.U0,IM!.SET);
% given an image set (im!.set), this finds the numbers associated with
% each factor in v subject to wang's condition (2) on the image set.
% this is an implementation of his algorithm n. if the condition
% is met the result is a vector containing the images of each factor
% in v, otherwise the result is nil;
  BEGIN SCALAR D,K,Q,R,LC!.IMAGE!.VEC;
            % v's integer factor is at the front:  ;
    K:=LENGTH CDR V;
            % no. of non-trivial factors of v;
    IF NOT NUMBERP CONT!.U0 THEN CONT!.U0:=LC CONT!.U0;
    PUTV(D:=MKVECT K,0,ABS(CONT!.U0 * CAR V));
	    % d will contain the special numbers to be used in the
	    % loop below;
    PUTV(LC!.IMAGE!.VEC:=MKVECT K,0,ABS(CONT!.U0 * CAR V));
            % vector for result with 0th entry filled in;
    V:=CDR V;
            % throw away integer factor of v;
            % k is no. of non-trivial factors (say f(i)) in v;
            % d will contain the nos. associated with each f(i);
            % v is now a list of the f(i) (and their multiplicities);
    FOR I:=1:K DO
    << Q:=ABS MAKE!-IMAGE(CAAR V,IM!.SET);
       PUTV(LC!.IMAGE!.VEC,I,Q);
       V:=CDR V;
       FOR J:=ISUB1 I STEP -1 UNTIL 0 DO
       << R:=GETV(D,J);
          WHILE NOT ONEP R DO
          << R:=GCD(R,Q); Q:=Q/R >>;
          IF ONEP Q THEN RETURN LC!.IMAGE!.VEC:=NIL;
            % if q=1 here then we have failed the condition so exit;
          >>;
      IF NULL LC!.IMAGE!.VEC THEN RETURN LC!.IMAGE!.VEC;
      PUTV(D,I,Q);
            % else q is the ith number we want;
   >>;
    RETURN LC!.IMAGE!.VEC
  END;

SYMBOLIC PROCEDURE GET!.CONTENT U;
% u is a univariate square free poly. gets the content of u (=integer);
% if lc u is negative then the minus sign is pulled out as well;
% nb. the content includes the variable if it is a factor of u;
  BEGIN SCALAR C;
    C:=IF POLY!-MINUSP U THEN -(NUMERIC!-CONTENT U)
       ELSE NUMERIC!-CONTENT U;
    IF NOT DIDNTGO QUOTF(U,!*K2F M!-IMAGE!-VARIABLE) THEN
      C:=ADJOIN!-TERM(MKSP(M!-IMAGE!-VARIABLE,1),C,POLYZERO);
    RETURN C
  END;


%********************************************************************;
%    finally we have the routines that use the numbers generated
%    by unique.f.nos to determine the true leading coeffts in
%    the multivariate factorization we are doing and which image
%    factors will grow up to have which true leading coefft.
%********************************************************************;




SYMBOLIC PROCEDURE DISTRIBUTE!.LC(R,IM!.FACTORS,S,V);
% v is the factored lc of a poly, say u, whose image factors (r of
% them) are in the vector im.factors. s is a list containing the
% image information including the image set, the image poly etc.
%  this uses wang's ideas for distributing the factors in v over
% those in im.factors. result is (delta . vector of the lc's of
% the full factors of u) , where delta is the remaining integer part
% of the lc that we have been unable to distribute.             ;
  (LAMBDA FACTOR!-LEVEL;
  BEGIN SCALAR K,DELTA,DIV!.COUNT,Q,UF,I,D,MAX!.MULT,F,NUMVEC,
               DVEC,WVEC,DTWID,W;
    DELTA:=GET!-IMAGE!-CONTENT S;
            % the content of the u image poly;
    DIST!.LC!.MSG1(DELTA,IM!.FACTORS,R,S,V);
    V:=CDR V;
            % we are not interested in the numeric factors of v;
    K:=LENGTH V;
            % number of things to distribute;
    NUMVEC:=GET!-F!-NUMVEC S;
            % nos. associated with factors in v;
    DVEC:=MKVECT R;
    WVEC:=MKVECT R;
    FOR J:=1:R DO <<
      PUTV(DVEC,J,1);
      PUTV(WVEC,J,DELTA*LC GETV(IM!.FACTORS,J)) >>;
            % result lc's will go into dvec which we initialize to 1's;
            % wvec is a work vector that we use in the division process
            % below;
    V:=REVERSE V;
    FOR J:=K STEP -1 UNTIL 1 DO
    << % (for each factor in v, call it f(j) );
      F:=CAAR V;
            % f(j) itself;
      MAX!.MULT:=CDAR V;
            % multiplicity of f(j) in v (=lc u);
      V:=CDR V;
      D:=GETV(NUMVEC,J);
            % number associated with f(j);
      I:=1; % we trial divide d into lc of each image
            % factor starting with 1st;
      DIV!.COUNT:=0;
            % no. of d's that have been distributed;
      FACTOR!-TRACE <<
	PRIN2!* "f("; PRIN2!* J; PRIN2!* ")= "; FAC!-PRINTSF F;
        PRIN2!* "There are "; PRIN2!* MAX!.MULT;
        PRINTSTR " of these in the leading coefficient.";
        PRIN2!* "The absolute value of the image of f("; PRIN2!* J;
        PRIN2!* ")= "; PRINTSTR D >>;
      WHILE ILESSP(DIV!.COUNT,MAX!.MULT)
        AND NOT IGREATERP(I,R) DO
      << Q:=DIVIDE(GETV(WVEC,I),D);
            % first trial division;
        FACTOR!-TRACE <<
          PRIN2!* "  Trial divide into ";
          PRIN2!* GETV(WVEC,I); PRINTSTR " :" >>;
        WHILE (ZEROP CDR Q) AND ILESSP(DIV!.COUNT,MAX!.MULT) DO
        << PUTV(DVEC,I,MULTF(GETV(DVEC,I),F));
            % f(j) belongs in lc of ith factor;
          FACTOR!-TRACE <<
	    PRIN2!* "    It goes so an f("; PRIN2!* J;
	    PRIN2!* ") belongs in ";
	    FAC!-PRINTSF GETV(IM!.FACTORS,I);
            PRINTSTR "  Try again..." >>;
          DIV!.COUNT:=IADD1 DIV!.COUNT;
            % another d done;
          PUTV(WVEC,I,CAR Q);
            % save the quotient for next factor to distribute;
          Q:=DIVIDE(CAR Q,D);
            % try again;
        >>;
        I:=IADD1 I;
            % as many d's as possible have gone into that
            % factor so now try next factor;
        FACTOR!-TRACE <<
          PRINTSTR "    no good so try another factor ..." >>
      >>;
            % at this point the whole of f(j) should have been
            % distributed by dividing d the maximum no. of times
            % (= max!.mult), otherwise we have an extraneous factor;
      IF ILESSP(DIV!.COUNT,MAX!.MULT) THEN
        RETURN BAD!-CASE:=T
    >>;
    IF BAD!-CASE THEN RETURN;
    FACTOR!-TRACE <<
      PRINTSTR "The leading coefficients are now correct to within an";
      PRINTSTR "integer factor and are as follows:";
      FOR J:=1:R DO <<
        PRINSF GETV(IM!.FACTORS,J);
        PRIN2!* " with l.c. ";
	FAC!-PRINTSF GETV(DVEC,J) >> >>;
    IF ONEP DELTA THEN
    << FOR J:=1:R DO <<
         W:=LC GETV(IM!.FACTORS,J) /
          EVALUATE!-IN!-ORDER(GETV(DVEC,J),GET!-IMAGE!-SET S);
         IF W<0 THEN BEGIN
           SCALAR OLDPOLY;
           DELTA:= -DELTA;
           OLDPOLY:=GETV(IM!.FACTORS,J);
           PUTV(IM!.FACTORS,J,NEGF OLDPOLY);
            % to keep the leading coefficients positive we negate the
            % image factors when necessary;
           MULTIPLY!-ALPHAS(-1,OLDPOLY,GETV(IM!.FACTORS,J));
            % remember to fix the alphas as well;
         END;
         PUTV(DVEC,J,MULTF(ABS W,GETV(DVEC,J))) >>;
      DIST!.LC!.MSG2(DVEC,IM!.FACTORS,R);
      RETURN (DELTA . DVEC)
    >>;
      % if delta=1 then we know the true lc's exactly so put in their
      % integer contents and return with result.
      % otherwise try spreading delta out over the factors:      ;
    FACTOR!-TRACE <<
      PRIN2!* " Here delta is not 1 meaning that we have a content, ";
      PRINTSTR DELTA;
      PRINTSTR "of the image to distribute among the factors somehow.";
      PRINTSTR "For each IM-factor we can divide its leading";
      PRINTSTR "coefficient by the image of its determined leading";
      PRINTSTR "coefficient and see if there is a non-trivial result.";
      PRINTSTR "This will indicate a factor of delta belonging to this";
      PRINTSTR "IM-factor's leading coefficient." >>;
    FOR J:=1:R DO
    << DTWID:=EVALUATE!-IN!-ORDER(GETV(DVEC,J),GET!-IMAGE!-SET S);
       UF:=GETV(IM!.FACTORS,J);
       D:=GCD(LC UF,DTWID);
       PUTV(DVEC,J,MULTF(LC UF/D,GETV(DVEC,J)));
       PUTV(IM!.FACTORS,J,MULTF(DTWID/D,UF));
            % have to fiddle the image factors by an integer multiple;
       MULTIPLY!-ALPHAS!-RECIP(DTWID/D,UF,GETV(IM!.FACTORS,J));
            % fix the alphas;
       DELTA:=DELTA/(DTWID/D)
    >>;
    % now we've done all we can to distribute delta so we return with
    % what's left:                                    ;
    IF DELTA<=0 THEN
      ERRORF LIST("FINAL DELTA IS -VE IN DISTRIBUTE!.LC",DELTA);
    FACTOR!-TRACE <<
      PRINTSTR "     Finally we have:";
      FOR J:=1:R DO <<
        PRINSF GETV(IM!.FACTORS,J);
        PRIN2!* " with l.c. ";
	FAC!-PRINTSF GETV(DVEC,J) >> >>;
    RETURN (DELTA . DVEC)
  END) (FACTOR!-LEVEL * 10);

SYMBOLIC PROCEDURE DIST!.LC!.MSG1(DELTA,IM!.FACTORS,R,S,V);
    FACTOR!-TRACE <<
      TERPRI(); TERPRI();
      PRINTSTR "We have a polynomial whose image factors (call";
      PRINTSTR "them the IM-factors) are:";
      PRIN2!* DELTA; PRINTSTR " (= numeric content, delta)";
      PRINTVEC(" f(",R,")= ",IM!.FACTORS);
      PRIN2!* "  wrt the image set: ";
      FOR EACH X IN GET!-IMAGE!-SET S DO <<
        PRINVAR CAR X; PRIN2!* "="; PRIN2!* CDR X; PRIN2!* ";" >>;
      TERPRI!*(NIL);
      PRINTSTR "We also have its true multivariate leading";
      PRINTSTR "coefficient whose factors (call these the";
      PRINTSTR "LC-factors) are:";
      FAC!-PRINTFACTORS V;
      PRINTSTR "We want to determine how these LC-factors are";
      PRINTSTR "distributed over the leading coefficients of each";
      PRINTSTR "IM-factor.  This enables us to feed the resulting";
      PRINTSTR "image factors into a multivariate Hensel";
      PRINTSTR "construction.";
      PRINTSTR "We distribute each LC-factor in turn by dividing";
      PRINTSTR "its image into delta times the leading coefficient";
      PRINTSTR "of each IM-factor until it finds one that it";
      PRINTSTR "divides exactly. The image set is chosen such that";
      PRINTSTR "this will only happen for the IM-factors to which";
      PRINTSTR "this LC-factor belongs - (there may be more than";
      PRINTSTR "one if the LC-factor occurs several times in the";
      PRINTSTR "leading coefficient of the original polynomial).";
      PRINTSTR "This choice also requires that we distribute the";
      PRINTSTR "LC-factors in a specific order:"
      >>;

SYMBOLIC PROCEDURE DIST!.LC!.MSG2(DVEC,IM!.FACTORS,R);
      FACTOR!-TRACE <<
        PRINTSTR "Since delta=1, we have no non-trivial content of the";
	PRINTSTR
	  "image to deal with so we know the true leading coefficients";
	PRINTSTR
	  "exactly.  We fix the signs of the IM-factors to match those";
        PRINTSTR "of their true leading coefficients:";
        FOR J:=1:R DO <<
          PRINSF GETV(IM!.FACTORS,J);
          PRIN2!* " with l.c. ";
	  FAC!-PRINTSF GETV(DVEC,J) >> >>;

ENDMODULE;


MODULE INTERFAC;

%**********************************************************************;
%
%   copyright (c)  university of cambridge, england 1981
%
%**********************************************************************;




%**********************************************************************;
% Routines that are specific to REDUCE.
%  These are either routines that are not needed in the HASH system
%  (which is the other algebra system that this factorizer
%  can be plugged into) or routines that are specifically
%  redefined in the HASH system. ;




%---------------------------------------------------------------------;
% The following would normally live in section:  ALPHAS
%---------------------------------------------------------------------;

SYMBOLIC PROCEDURE ASSOC!-ALPHA(POLY,ALIST);  ASSOC(POLY,ALIST);



%---------------------------------------------------------------------;
% The following would normally live in section:  COEFFTS
%---------------------------------------------------------------------;


SYMBOLIC PROCEDURE TERMVECTOR2SF V;
  BEGIN SCALAR R,W;
    FOR I:=CAR GETV(V,0) STEP -1 UNTIL 1 DO <<
      W:=GETV(V,I);
            % degree . coefft;
      R:=IF CAR W=0 THEN CDR W ELSE
        (MKSP(M!-IMAGE!-VARIABLE,CAR W) .* CDR W) .+ R
    >>;
    RETURN R
  END;

SYMBOLIC PROCEDURE FORCE!-LC(A,N);
% force polynomial a to have leading coefficient as specified;
    (LPOW A .* N) .+ RED A;

SYMBOLIC PROCEDURE MERGE!-TERMS(U,V);
  MERGE!-TERMS1(1,U,V,CAR GETV(V,0));

SYMBOLIC PROCEDURE MERGE!-TERMS1(I,U,V,N);
  IF I#>N THEN U
  ELSE BEGIN SCALAR A,B;
    A:=GETV(V,I);
    IF DOMAINP U OR NOT(MVAR U=M!-IMAGE!-VARIABLE) THEN
      IF NOT(CAR A=0) THEN ERRORF LIST("MERGING COEFFTS FAILED",U,A)
      ELSE IF CDR A THEN RETURN CDR A
      ELSE RETURN U;
    B:=LT U;
    IF TDEG B=CAR A THEN RETURN
      (IF CDR A THEN TPOW B .* CDR A ELSE B) .+
        MERGE!-TERMS1(I #+ 1,RED U,V,N)
    ELSE IF TDEG B #> CAR A THEN RETURN B .+ MERGE!-TERMS1(I,RED U,V,N)
    ELSE ERRORF LIST("MERGING COEFFTS FAILED ",U,A)
  END;

SYMBOLIC PROCEDURE LIST!-TERMS!-IN!-FACTOR U;
% ...;
  IF DOMAINP U THEN LIST (0 . NIL)
  ELSE (LDEG U . NIL) . LIST!-TERMS!-IN!-FACTOR RED U;

SYMBOLIC PROCEDURE TRY!-OTHER!-COEFFTS(R,UNKNOWNS!-LIST,UV);
  BEGIN SCALAR LDEG!-R,LC!-R,W;
    WHILE NOT DOMAINP R AND (R:=RED R) AND NOT(W='COMPLETE) DO <<
      IF NOT DEPENDS!-ON!-VAR(R,M!-IMAGE!-VARIABLE) THEN
        << LDEG!-R:=0; LC!-R:=R >>
      ELSE << LDEG!-R:=LDEG R; LC!-R:=LC R >>;
      W:=SOLVE!-NEXT!-COEFFT(LDEG!-R,LC!-R,UNKNOWNS!-LIST,UV) >>
  END;


%---------------------------------------------------------------------;
% The following would normally live in section:  FACMISC
%---------------------------------------------------------------------;

SYMBOLIC PROCEDURE DERIVATIVE!-WRT!-MAIN!-VARIABLE(P,VAR);
% partial derivative of the polynomial p with respect to
% its main variable, var;
    IF DOMAINP P OR (MVAR P NEQ VAR) THEN NIL
    ELSE
     BEGIN
      SCALAR DEGREE;
      DEGREE:=LDEG P;
      IF DEGREE=1 THEN RETURN LC P; %degree one term is special;
      RETURN (MKSP(MVAR P,DEGREE-1) .* MULTF(DEGREE,LC P)) .+
        DERIVATIVE!-WRT!-MAIN!-VARIABLE(RED P,VAR)
     END;

SYMBOLIC PROCEDURE UNIVARIATEP U;
% tests to see if u is univariate;
  DOMAINP U OR NOT MULTIVARIATEP(U,MVAR U);

SYMBOLIC PROCEDURE VARIABLES!.IN!.FORM(A,SOFAR);
    IF DOMAINP A THEN SOFAR
    ELSE <<
      IF NOT MEMQ(MVAR A,SOFAR) THEN
        SOFAR:=MVAR A . SOFAR;
      VARIABLES!.IN!.FORM(RED A,
        VARIABLES!.IN!.FORM(LC A,SOFAR)) >>;


SYMBOLIC PROCEDURE DEGREE!-IN!-VARIABLE(P,V);
% returns the degree of the polynomial p in the
% variable v;
    IF DOMAINP P THEN 0
    ELSE IF LC P=0
     THEN ERRORF "Polynomial with a zero coefficient found"
    ELSE IF V=MVAR P THEN LDEG P
    ELSE MAX(DEGREE!-IN!-VARIABLE(LC P,V),
      DEGREE!-IN!-VARIABLE(RED P,V));

SYMBOLIC PROCEDURE GET!-HEIGHT POLY;
% find height (max coefft) of given poly;
  IF NULL POLY THEN 0
  ELSE IF NUMBERP POLY THEN ABS POLY
  ELSE MAX(GET!-HEIGHT LC POLY,GET!-HEIGHT RED POLY);


SYMBOLIC PROCEDURE POLY!-MINUSP A;
    IF A=NIL THEN NIL
    ELSE IF DOMAINP A THEN MINUSP A
    ELSE POLY!-MINUSP LC A;

SYMBOLIC PROCEDURE POLY!-ABS A;
    IF POLY!-MINUSP A THEN NEGF A
    ELSE A;

SYMBOLIC PROCEDURE FAC!-PRINTFACTORS L;
% procedure to print the result of factorize!-form;
% ie. l is of the form: (c . f)
%  where c is the numeric content (may be 1)
%  and f is of the form: ( (f1 . e1) (f2 . e2) ... (fn . en) )
%    where the fi's are s.f.s and ei's are numbers;
<< TERPRI();
  IF NOT (CAR L = 1) THEN FAC!-PRINTSF CAR L;
  FOR EACH ITEM IN CDR L DO
    FAC!-PRINTSF !*P2F MKSP(PREPF CAR ITEM,CDR ITEM) >>;

%---------------------------------------------------------------------;
% The following would normally live in section:  FACPRIM
%---------------------------------------------------------------------;

SYMBOLIC PROCEDURE INVERT!.POLY(U,VAR);
% u is a non-trivial primitive square free multivariate polynomial.
% assuming var is the top-level variable in u, this effectively
% reverses the position of the coeffts: ie
%   a(n)*var**n + a(n-1)*var**(n-1) + ... + a(0)
% becomes:
%   a(0)*var**n + a(1)*var**(n-1) + ... + a(n) .               ;
  BEGIN SCALAR W,INVERT!-SIGN;
    W:=INVERT!.POLY1(RED U,LDEG U,LC U,VAR);
    IF POLY!-MINUSP LC W THEN <<
      W:=NEGF W;
      INVERT!-SIGN:=-1 >>
    ELSE INVERT!-SIGN:=1;
    RETURN INVERT!-SIGN . W
  END;

SYMBOLIC PROCEDURE INVERT!.POLY1(U,D,V,VAR);
% d is the degree of the poly we wish to invert.
% assume d > ldeg u always, and that v is never nil;
  IF (DOMAINP U) OR NOT (MVAR U=VAR) THEN
    (VAR TO D) .* U .+ V
  ELSE INVERT!.POLY1(RED U,D,(VAR TO (D-LDEG U)) .* (LC U) .+ V,VAR);


SYMBOLIC PROCEDURE TRAILING!.COEFFT(U,VAR);
% u is multivariate poly with var as the top-level variable. we find
% the trailing coefft - ie the constant wrt var in u;
  IF DOMAINP U THEN U
  ELSE IF MVAR U=VAR THEN TRAILING!.COEFFT(RED U,VAR)
  ELSE U;


%---------------------------------------------------------------------;
% The following would normally live in section:  FACTOR
%---------------------------------------------------------------------;




SYMBOLIC PROCEDURE SIMPFACTORIZE U;
% factorize the polynomial p, putting the factors into
% the array w, and return the number of factors found.
% w(0) gets set to the (numeric) content of p (which
% may well be just +1). w should be a one-dimensional array. if it
% the name of a variable, not an array, the variables w0, w1,...
% will be set instead;
  BEGIN SCALAR P,W,!*FORCE!-PRIME,X,Y,Z,FACTOR!-COUNT;
    IF ATOM U THEN REDERR "FACTORIZE needs arguments"
    ELSE IF ATOM CDR U THEN U := LIST(CAR U,'FACTOR); 
    P:= !*Q2F SIMP!* CAR U;
    W := CADR U;
    IF NOT ATOM CDDR U AND NUMBERP CADDR U THEN
	!*FORCE!-PRIME := CADDR U;
    X:=FACTORF1(P,!*FORCE!-PRIME);
    Z:= (0 . CAR X) . NIL;
    FACTOR!-COUNT:=0;
    FOR EACH FFF IN CDR X DO
        FOR I:=1:CDR FFF DO
            Z:=((FACTOR!-COUNT:=FACTOR!-COUNT+1) .
                MK!*SQ(CAR FFF ./ 1)) . Z;
    RETURN MULTIPLE!-RESULT(Z,W)
  END;

PUT('FACTORIZE,'SIMPFN,'SIMPFACTORIZE);


%---------------------------------------------------------------------;
% The following would normally live in section:  IMAGESET
%---------------------------------------------------------------------;

SYMBOLIC PROCEDURE MAKE!-IMAGE!-LC!-LIST(U,IMSET);
  REVERSEWOC MAKE!-IMAGE!-LC!-LIST1(U,IMSET,
    FOR EACH X IN IMSET COLLECT CAR X);

SYMBOLIC PROCEDURE MAKE!-IMAGE!-LC!-LIST1(U,IMSET,VARLIST);
% If IMSET=((x1 . a1, x2 . a2, ... , xn . an)) (ordered) where xj is
% the variable and aj its value, then this fn creates n images of U wrt
% sets S(i) where S(i)= ((x1 . a1), ... , (xi . ai)). The result is an
% ordered list of pairs: (u(i) . X(i+1)) where u(i)= U wrt S(i) and
% X(i) = (xi, ... , xn) and X(n+1) = NIL.  VARLIST = X(1).
% (Note. the variables tagged to u(i) should be all those
% appearing in u(i) unless it is degenerate). The returned list is
% ordered with u(1) first and ending with the number u(n);
  IF NULL IMSET THEN NIL
  ELSE IF DOMAINP U THEN LIST(!*D2N U . CDR VARLIST)
  ELSE IF MVAR U=CAAR IMSET THEN
    BEGIN SCALAR W;
      W:=HORNER!-RULE!-FOR!-ONE!-VAR(
        U,CAAR IMSET,CDAR IMSET,POLYZERO,LDEG U) . CDR VARLIST;
      RETURN
        IF POLYZEROP CAR W THEN LIST (0 . CDR W)
        ELSE (W . MAKE!-IMAGE!-LC!-LIST1(CAR W,CDR IMSET,CDR VARLIST))
    END
  ELSE MAKE!-IMAGE!-LC!-LIST1(U,CDR IMSET,CDR VARLIST);

SYMBOLIC PROCEDURE HORNER!-RULE!-FOR!-ONE!-VAR(U,X,VAL,C,DEGG);
  IF DOMAINP U OR NOT(MVAR U=X) THEN ADDF(U,MULTF(C,!*NUM2F(VAL**DEGG)))
  ELSE BEGIN SCALAR NEWDEG;
    NEWDEG:=LDEG U;
    RETURN HORNER!-RULE!-FOR!-ONE!-VAR(RED U,X,VAL,
      ADDF(LC U,MULTF(C,!*NUM2F(VAL**(IDIFFERENCE(DEGG,NEWDEG))))),
			    NEWDEG)
  END;

SYMBOLIC PROCEDURE MAKE!-IMAGE(U,IMSET);
% finds image of u wrt image set, imset, (=association list);
  IF DOMAINP U THEN U
  ELSE IF MVAR U=M!-IMAGE!-VARIABLE THEN
    ADJOIN!-TERM(LPOW U,!*NUM2F EVALUATE!-IN!-ORDER(LC U,IMSET),
                        MAKE!-IMAGE(RED U,IMSET))
  ELSE !*NUM2F EVALUATE!-IN!-ORDER(U,IMSET);

SYMBOLIC PROCEDURE EVALUATE!-IN!-ORDER(U,IMSET);
% makes an image of u wrt imageset, imset, using horner's rule. result
% should be purely numeric;
  IF DOMAINP U THEN !*D2N U
  ELSE IF MVAR U=CAAR IMSET THEN
    HORNER!-RULE(EVALUATE!-IN!-ORDER(LC U,CDR IMSET),
      LDEG U,RED U,IMSET)
  ELSE EVALUATE!-IN!-ORDER(U,CDR IMSET);

SYMBOLIC PROCEDURE HORNER!-RULE(C,DEGG,A,VSET);
% c is running total and a is what is left;
  IF DOMAINP A THEN (!*D2N A)+C*((CDAR VSET)**DEGG)
  ELSE IF NOT(MVAR A=CAAR VSET) THEN
    EVALUATE!-IN!-ORDER(A,CDR VSET)+C*((CDAR VSET)**DEGG)
  ELSE BEGIN SCALAR NEWDEG;
    NEWDEG:=LDEG A;
    RETURN HORNER!-RULE(EVALUATE!-IN!-ORDER(LC A,CDR VSET)
      +C*((CDAR VSET)**(IDIFFERENCE(DEGG,NEWDEG))),NEWDEG,RED A,VSET)
  END;


%---------------------------------------------------------------------;
% The following would normally live in section:  MHENSFNS
%---------------------------------------------------------------------;

SYMBOLIC PROCEDURE MAX!-DEGREE(U,N);
% finds maximum degree of any single variable in U (n is max so far);
  IF DOMAINP U THEN N
  ELSE IF IGREATERP(N,LDEG U) THEN
    MAX!-DEGREE(RED U,MAX!-DEGREE(LC U,N))
  ELSE MAX!-DEGREE(RED U,MAX!-DEGREE(LC U,LDEG U));

SYMBOLIC PROCEDURE DIFF!-OVER!-K!-MOD!-P(U,K,V);
% derivative of u wrt v divided by k (=number);
  IF DOMAINP U THEN NIL
  ELSE IF MVAR U = V THEN
    IF LDEG U = 1 THEN QUOTIENT!-MOD!-P(LC U,MODULAR!-NUMBER K)
    ELSE ADJOIN!-TERM(MKSP(V,ISUB1 LDEG U),
      QUOTIENT!-MOD!-P(
        TIMES!-MOD!-P(MODULAR!-NUMBER LDEG U,LC U),
        MODULAR!-NUMBER K),
      DIFF!-OVER!-K!-MOD!-P(RED U,K,V))
  ELSE ADJOIN!-TERM(LPOW U,
    DIFF!-OVER!-K!-MOD!-P(LC U,K,V),
    DIFF!-OVER!-K!-MOD!-P(RED U,K,V));

SYMBOLIC PROCEDURE DIFF!-K!-TIMES!-MOD!-P(U,K,V);
% differentiates u k times wrt v and divides by (k!) ie. for each term
% a*v**n we get [n k]*a*v**(n-k) if n>=k and nil if n<k where
% [n k] is the binomial coefficient;
  IF DOMAINP U THEN NIL
  ELSE IF MVAR U = V THEN
    IF LDEG U < K THEN NIL
    ELSE IF LDEG U = K THEN LC U
    ELSE ADJOIN!-TERM(MKSP(V,LDEG U - K),
      TIMES!-MOD!-P(BINOMIAL!-COEFFT!-MOD!-P(LDEG U,K),LC U),
      DIFF!-K!-TIMES!-MOD!-P(RED U,K,V))
  ELSE ADJOIN!-TERM(LPOW U,
    DIFF!-K!-TIMES!-MOD!-P(LC U,K,V),
    DIFF!-K!-TIMES!-MOD!-P(RED U,K,V));

SYMBOLIC PROCEDURE SPREADVAR(U,V,SLIST);
% find all the powers of V in U and merge their degrees into SLIST.
% We ignore the constant term wrt V;
  IF DOMAINP U THEN SLIST
  ELSE <<
    IF MVAR U=V AND NOT MEMBER(LDEG U,SLIST) THEN SLIST:=LDEG U . SLIST;
    SPREADVAR(RED U,V,SPREADVAR(LC U,V,SLIST)) >>;


%---------------------------------------------------------------------;
% The following would normally live in section:  UNIHENS
%---------------------------------------------------------------------;

SYMBOLIC PROCEDURE ROOT!-SQUARES(U,SOFAR);
  IF NULL U THEN PMAM!-SQRT SOFAR
  ELSE IF DOMAINP U THEN PMAM!-SQRT(SOFAR+(U*U))
  ELSE ROOT!-SQUARES(RED U,SOFAR+(LC U * LC U));

%---------------------------------------------------------------------;
% The following would normally live in section:  VECPOLY
%---------------------------------------------------------------------;

SYMBOLIC PROCEDURE POLY!-TO!-VECTOR P;
% spread the given univariate polynomial out into POLY-VECTOR;
    IF ISDOMAIN P THEN PUTV(POLY!-VECTOR,0,!*D2N P)
    ELSE <<
      PUTV(POLY!-VECTOR,LDEG P,LC P);
      POLY!-TO!-VECTOR RED P >>;

SYMBOLIC PROCEDURE VECTOR!-TO!-POLY(P,D,V);
% Convert the vector P into a polynomial of degree D in variable V;
  BEGIN
    SCALAR R;
    IF D#<0 THEN RETURN NIL;
    R:=!*N2F GETV(P,0);
    FOR I:=1:D DO
      IF GETV(P,I) NEQ 0 THEN R:=((V TO I) .* GETV(P,I)) .+ R;
    RETURN R
  END;



ENDMODULE;


MODULE LINMODP;

% *******************************************************************
%
%   copyright (c)  university of cambridge, england 1979
%
% *******************************************************************;




%**********************************************************************;
%
%      This section solves linear equations mod p;








SYMBOLIC PROCEDURE LU!-FACTORIZE!-MOD!-P(A,N);
% A is a matrix of size N*N. Overwrite it with its LU factorization;
  BEGIN SCALAR W;
   FOR I:=1:N DO BEGIN
    SCALAR II,PIVOT;
    II:=I;
    WHILE (PIVOT:=GETM2(A,II,I))=0
       OR IREMAINDER(PIVOT,PRIME!-BASE)=0 DO <<
        II:=II+1;
        IF II>N THEN RETURN W:='SINGULAR >>;
    IF W='SINGULAR THEN RETURN W;
    IF NOT II=I THEN BEGIN
        SCALAR TEMP;
        TEMP:=GETV(A,I);
        PUTV(A,I,GETV(A,II));
        PUTV(A,II,TEMP) END;
    PUTM2(A,I,0,II); % Remember pivoting information;
    PIVOT:=MODULAR!-RECIPROCAL PIVOT;
    PUTM2(A,I,I,PIVOT);
    FOR J:=I+1:N DO
      PUTM2(A,I,J,MODULAR!-TIMES(PIVOT,GETM2(A,I,J)));
    FOR II:=I+1:N DO BEGIN
       SCALAR MULTIPLE;
       MULTIPLE:=GETM2(A,II,I);
       FOR J:=I+1:N DO
          PUTM2(A,II,J,MODULAR!-DIFFERENCE(GETM2(A,II,J),
            MODULAR!-TIMES(MULTIPLE,GETM2(A,I,J)))) END END;
    RETURN W
  END;

SYMBOLIC PROCEDURE BACK!-SUBSTITUTE(A,V,N);
% A is an N*N matrix as produced by LU-FACTORIZE-MOD-P, and V is
% a vector of length N. Overwrite V with solution to linear equations;
  BEGIN
    FOR I:=1:N DO BEGIN
        SCALAR II;
        II:=GETM2(A,I,0); % Pivot control;
        IF NOT II=I THEN DO BEGIN
           SCALAR TEMP;
           TEMP:=GETV(V,I); PUTV(V,I,GETV(V,II)); PUTV(V,II,TEMP) END
        END;
    FOR I:=1:N DO BEGIN
        PUTV(V,I,TIMES!-MOD!-P(!*N2F GETM2(A,I,I),GETV(V,I)));
        FOR II:=I+1:N DO
           PUTV(V,II,DIFFERENCE!-MOD!-P(GETV(V,II),
              TIMES!-MOD!-P(GETV(V,I),!*N2F GETM2(A,II,I)))) END;
            % Now do the actual back substitution;
    FOR I:=N-1 STEP -1 UNTIL 1 DO
      FOR J:=I+1:N DO
        PUTV(V,I,DIFFERENCE!-MOD!-P(GETV(V,I),
          TIMES!-MOD!-P(!*N2F GETM2(A,I,J),GETV(V,J))));
    RETURN V
  END;



ENDMODULE;


MODULE MHENSFNS;

% *******************************************************************
%
%   copyright (c)  university of cambridge, england 1979
%
% *******************************************************************;





%**********************************************************************;
%    This section contains some of the functions used in
%    the multivariate hensel growth. (ie they are called from
%    section MULTIHEN or function RECONSTRUCT-MULTIVARIATE-FACTORS). ;



SYMBOLIC PROCEDURE SET!-DEGREE!-BOUNDS V;
  DEGREE!-BOUNDS:=FOR EACH VAR IN V COLLECT
    (CAR VAR . DEGREE!-IN!-VARIABLE(MULTIVARIATE!-INPUT!-POLY,CAR VAR));

SYMBOLIC PROCEDURE GET!-DEGREE!-BOUND V;
  BEGIN SCALAR W;
    W:=ATSOC(V,DEGREE!-BOUNDS);
    IF NULL W THEN ERRORF(LIST("Degree bound not found for ",
        V," in ",DEGREE!-BOUNDS));
    RETURN CDR W
  END;

SYMBOLIC PROCEDURE CHOOSE!-LARGER!-PRIME N;
% our prime base in the multivariate hensel must be greater than n so
% this sets a new prime to be that (previous one was found to be no
% good). We also set up various fluids e.g. the Alphas;
% the primes we can choose are < 2**24 so if n is bigger
% we collapse;
  IF N > 2**24-1 THEN
    ERRORF LIST("CANNOT CHOOSE PRIME > GIVEN NUMBER:",N)
  ELSE BEGIN SCALAR P,FLIST!-MOD!-P,K,FVEC!-MOD!-P,FORBIDDEN!-PRIMES;
TRYNEWPRIME:
    IF P THEN FORBIDDEN!-PRIMES:=P . FORBIDDEN!-PRIMES;
    P:=RANDOM!-PRIME();
            % this chooses a word-size prime (currently 24 bits);
    SET!-MODULUS P;
    IF NOT(P>N) OR MEMBER(P,FORBIDDEN!-PRIMES) OR
      POLYZEROP REDUCE!-MOD!-P LC MULTIVARIATE!-INPUT!-POLY THEN
       GOTO TRYNEWPRIME;
    FOR I:=1:NUMBER!-OF!-FACTORS DO
      FLIST!-MOD!-P:=(REDUCE!-MOD!-P GETV(IMAGE!-FACTORS,I) .
		       FLIST!-MOD!-P);
    ALPHALIST:=ALPHAS(NUMBER!-OF!-FACTORS,FLIST!-MOD!-P,1);
    IF ALPHALIST='FACTORS! NOT! COPRIME THEN GOTO TRYNEWPRIME;
    HENSEL!-GROWTH!-SIZE:=P;
    PRIME!-BASE:=P;
    FACTOR!-TRACE <<
      PRIN2!* "New prime chosen: ";
      PRINTSTR HENSEL!-GROWTH!-SIZE >>;
    K:=NUMBER!-OF!-FACTORS;
    FVEC!-MOD!-P:=MKVECT K;
    FOR EACH W IN FLIST!-MOD!-P DO <<
      PUTV(FVEC!-MOD!-P,K,W); K:=ISUB1 K >>;
    RETURN FVEC!-MOD!-P
  END;

SYMBOLIC PROCEDURE BINOMIAL!-COEFFT!-MOD!-P(N,R);
  IF N<R THEN NIL
  ELSE IF N=R THEN 1
  ELSE IF R=1 THEN !*NUM2F MODULAR!-NUMBER N
  ELSE BEGIN SCALAR N!-C!-R,B,J;
    N!-C!-R:=1;
    B:=MIN(R,N-R);
    N:=MODULAR!-NUMBER N;
    R:=MODULAR!-NUMBER R;
    FOR I:=1:B DO <<
      J:=MODULAR!-NUMBER I;
      N!-C!-R:=MODULAR!-QUOTIENT(
        MODULAR!-TIMES(N!-C!-R,
          MODULAR!-DIFFERENCE(N,MODULAR!-DIFFERENCE(J,1))),
        J) >>;
    RETURN !*NUM2F N!-C!-R
  END;

SYMBOLIC PROCEDURE MAKE!-MULTIVARIATE!-HATVEC!-MOD!-P(BVEC,N);
% makes a vector whose ith elt is product over j [ BVEC(j) ] / BVEC(i);
% NB. we must NOT actually do the division here as we are likely
% to be working mod p**n (some n > 1) and the division can involve
% a division by p.;
  BEGIN SCALAR BHATVEC,R;
    BHATVEC:=MKVECT N;
    FOR I:=1:N DO <<
      R:=1;
      FOR J:=1:N DO IF NOT(J=I) THEN R:=TIMES!-MOD!-P(R,GETV(BVEC,J));
      PUTV(BHATVEC,I,R) >>;
    RETURN BHATVEC
  END;

SYMBOLIC PROCEDURE MAX!-DEGREE!-IN!-VAR(FVEC,V);
  BEGIN SCALAR R,D;
    R:=0;
    FOR I:=1:NUMBER!-OF!-FACTORS DO
      IF R<(D:=DEGREE!-IN!-VARIABLE(GETV(FVEC,I),V)) THEN R:=D;
    RETURN R
  END;

SYMBOLIC PROCEDURE MAKE!-GROWTH!-FACTOR PT;
% pt is of form (v . n) where v is a variable. we make the s.f. v-n;
  IF CDR PT=0 THEN !*F2MOD !*K2F CAR PT
  ELSE PLUS!-MOD!-P(!*F2MOD !*K2F CAR PT,MODULAR!-MINUS CDR PT);

SYMBOLIC PROCEDURE TERMS!-DONE!-MOD!-P(FVEC,DELFVEC,DELFACTOR);
% calculate the terms introduced by the corrections in DELFVEC;
  BEGIN SCALAR FLIST,DELFLIST;
    FOR I:=1:NUMBER!-OF!-FACTORS DO <<
      FLIST:=GETV(FVEC,I) . FLIST;
      DELFLIST:=GETV(DELFVEC,I) . DELFLIST >>;
    RETURN TERMS!-DONE1!-MOD!-P(NUMBER!-OF!-FACTORS,FLIST,DELFLIST,
      NUMBER!-OF!-FACTORS,DELFACTOR)
  END;

SYMBOLIC PROCEDURE TERMS!-DONE1!-MOD!-P(N,FLIST,DELFLIST,R,M);
  IF N=1 THEN (CAR FLIST) . (CAR DELFLIST)
  ELSE BEGIN SCALAR K,I,F1,F2,DELF1,DELF2;
    K:=N/2; I:=1;
    FOR EACH F IN FLIST DO
    << IF I>K THEN F2:=(F . F2)
       ELSE F1:=(F . F1);
       I:=I+1 >>;
    I:=1;
    FOR EACH DELF IN DELFLIST DO
    << IF I>K THEN DELF2:=(DELF . DELF2)
       ELSE DELF1:=(DELF . DELF1);
       I:=I+1 >>;
    F1:=TERMS!-DONE1!-MOD!-P(K,F1,DELF1,R,M);
    DELF1:=CDR F1; F1:=CAR F1;
    F2:=TERMS!-DONE1!-MOD!-P(N-K,F2,DELF2,R,M);
    DELF2:=CDR F2; F2:=CAR F2;
    DELF1:=
      PLUS!-MOD!-P(PLUS!-MOD!-P(
        TIMES!-MOD!-P(F1,DELF2),
        TIMES!-MOD!-P(F2,DELF1)),
        TIMES!-MOD!-P(TIMES!-MOD!-P(DELF1,M),DELF2));
    IF N=R THEN RETURN DELF1;
    RETURN (TIMES!-MOD!-P(F1,F2) . DELF1)
  END;

SYMBOLIC PROCEDURE PRIMITIVE!.PARTS(FLIST,VAR,UNIVARIATE!-INPUTS);
% finds the prim.part of each factor in flist wrt variable var;
% Note that FLIST may contain univariate or multivariate S.F.s
% (according to UNIVARIATE!-INPUTS) - in the former case we correct the
% ALPHALIST if necessary;
  BEGIN SCALAR C,PRIMF;
    IF NULL VAR THEN
      ERRORF "Must take primitive parts wrt some non-null variable";
    IF NON!-MONIC THEN
      FACTOR!-TRACE <<
        PRINTSTR "Because we multiplied the original primitive";
        PRINTSTR "polynomial by a multiple of its leading coefficient";
        PRINTSTR "(see (a) above), the factors we have now are not";
        PRINTSTR "necessarily primitive. However the required factors";
        PRINTSTR "are merely their primitive parts." >>;
    RETURN FOR EACH FW IN FLIST COLLECT
    << IF NOT DEPENDS!-ON!-VAR(FW,VAR) THEN
            ERRORF LIST("WRONG VARIABLE",VAR,FW);
       C:=COMFAC FW;
       IF CAR C THEN ERRORF(LIST(
         "FACTOR DIVISIBLE BY MAIN VARIABLE:",FW,CAR C));
       PRIMF:=QUOTFAIL(FW,CDR C);
       IF NOT(CDR C=1) AND UNIVARIATE!-INPUTS THEN
         MULTIPLY!-ALPHAS(CDR C,FW,PRIMF);
       PRIMF >>
  END;


SYMBOLIC PROCEDURE MAKE!-PREDICTED!-FORMS(PFS,V);
% PFS is a vector of S.F.s which represents the sparsity of
% the associated polynomials wrt V. Here PFS is adjusted to a
% suitable form for handling this sparsity. ie. we record the
% degrees of V in a vector for each poly in PFS. Each
% monomial (in V) represents an unknown (its coefft) in the predicted
% form of the associated poly. We count the maximum no of unknowns for
% each poly and return the maximum of these;
  BEGIN SCALAR L,N,PVEC,J,W;
    MAX!-UNKNOWNS:=0;
    FOR I:=1:NUMBER!-OF!-FACTORS DO <<
      W:=GETV(PFS,I);  % get the ith poly;
      L:=SORT(SPREADVAR(W,V,NIL),FUNCTION LESSP);
            % Pick out the monomials in V from this poly and order
            % them in increasing degree;
      N:=IADD1 LENGTH L; % no of unknowns in predicted poly - we add
                         % one for the constant term;
      NUMBER!-OF!-UNKNOWNS:=(N . I) . NUMBER!-OF!-UNKNOWNS;
      IF MAX!-UNKNOWNS<N THEN MAX!-UNKNOWNS:=N;
      PVEC:=MKVECT ISUB1 N;
            % get space for the info on this poly;
      J:=0;
      PUTV(PVEC,J,ISUB1 N);
            % put in the length of this vector which will vary
            % from poly to poly;
      FOR EACH M IN L DO PUTV(PVEC,J:=IADD1 J,M);
            % put in the monomial info;
      PUTV(PFS,I,PVEC);
            % overwrite the S.F. in PFS with the more compact vector;
      >>;
    NUMBER!-OF!-UNKNOWNS:=SORT(NUMBER!-OF!-UNKNOWNS,FUNCTION LESSPCAR);
    RETURN MAX!-UNKNOWNS
  END;

SYMBOLIC PROCEDURE MAKE!-CORRECTION!-VECTORS(PFS,BFS,N);
% set up space for the vector of vectors to hold the correction
% terms as we generate them by the function SOLVE-FOR-CORRECTIONS.
% Also put in the starting values;
  BEGIN SCALAR CVS,CV;
    CVS:=MKVECT NUMBER!-OF!-FACTORS;
    FOR I:=1:NUMBER!-OF!-FACTORS DO <<
      CV:=MKVECT N;
            % each CV will hold the corrections for the ith factor;
            % the no of corrections we put in here depends on the
            % maximum no of unknowns we have in the predicted
            % forms, giving a set of soluble linear systems (hopefully);
      PUTV(CV,1,GETV(BFS,I));
            % put in the first 'corrections';
      PUTV(CVS,I,CV) >>;
    RETURN CVS
  END;

SYMBOLIC PROCEDURE CONSTRUCT!-SOLN!-MATRICES(PFS,VAL);
% Here we construct the matrices - one for each linear system
% we will have to solve to see if our predicted forms of the
% answer are correct. Each matrix is a vector of row-vectors
% - the ijth elt is in jth slot of ith row-vector (ie zero slots
% are not used here);
  BEGIN SCALAR SOLN!-MATRIX,RESVEC,N,PV;
    RESVEC:=MKVECT NUMBER!-OF!-FACTORS;
    FOR I:=1:NUMBER!-OF!-FACTORS DO <<
      PV:=GETV(PFS,I);
      SOLN!-MATRIX:=MKVECT(N:=IADD1 GETV(PV,0));
      CONSTRUCT!-ITH!-MATRIX(SOLN!-MATRIX,PV,N,VAL);
      PUTV(RESVEC,I,SOLN!-MATRIX) >>;
    RETURN RESVEC
  END;

SYMBOLIC PROCEDURE CONSTRUCT!-ITH!-MATRIX(SM,PV,N,VAL);
  BEGIN SCALAR MV;
    MV:=MKVECT N;  %  this will be the first row;
    PUTV(MV,1,1);  % the first column represents the constant term;
    FOR J:=2:N DO PUTV(MV,J,MODULAR!-EXPT(VAL,GETV(PV,ISUB1 J)));
            % first row is straight substitution;
    PUTV(SM,1,MV);
            % now for the rest of the rows:   ;
    FOR J:=2:N DO <<
      MV:=MKVECT N;
      PUTV(MV,1,0);
      CONSTRUCT!-MATRIX!-ROW(MV,ISUB1 J,PV,N,VAL);
      PUTV(SM,J,MV) >>
  END;

SYMBOLIC PROCEDURE CONSTRUCT!-MATRIX!-ROW(MROW,J,PV,N,VAL);
  BEGIN SCALAR D;
    FOR K:=2:N DO <<
      D:=GETV(PV,ISUB1 K);  % degree representing the monomial;
      IF D<J THEN PUTV(MROW,K,0)
      ELSE <<
        D:=MODULAR!-TIMES(!*D2N BINOMIAL!-COEFFT!-MOD!-P(D,J),
             MODULAR!-EXPT(VAL,IDIFFERENCE(D,J)));
            % differentiate and substitute all at once;
        PUTV(MROW,K,D) >> >>
  END;

SYMBOLIC PROCEDURE PRINT!-LINEAR!-SYSTEMS(SOLN!-M,CORRECTION!-V,
                                              PREDICTED!-F,V);
<<
  FOR I:=1:NUMBER!-OF!-FACTORS DO
    PRINT!-LINEAR!-SYSTEM(I,SOLN!-M,CORRECTION!-V,PREDICTED!-F,V);
  TERPRI!*(NIL) >>;

SYMBOLIC PROCEDURE PRINT!-LINEAR!-SYSTEM(I,SOLN!-M,CORRECTION!-V,
                                              PREDICTED!-F,V);
  BEGIN SCALAR PV,SM,CV,MR,N,TT;
    TERPRI!*(T);
    PRIN2!* " i = "; PRINTSTR I;
    TERPRI!*(NIL);
    SM:=GETV(SOLN!-M,I);
    CV:=GETV(CORRECTION!-V,I);
      PV:=GETV(PREDICTED!-F,I);
      N:=IADD1 GETV(PV,0);
      FOR J:=1:N DO << % for each row in matrix ... ;
        PRIN2!* "(  ";
        TT:=2;
        MR:=GETV(SM,J);  % matrix row;
      FOR K:=1:N DO << % for each elt in row ... ;
          PRIN2!* GETV(MR,K);
          TTAB!* (TT:=TT+10) >>;
        PRIN2!* ")  ( [";
        IF J=1 THEN PRIN2!* 1
        ELSE PRINSF ADJOIN!-TERM(MKSP(V,GETV(PV,ISUB1 J)),1,POLYZERO);
      PRIN2!* "]";
      TTAB!* (TT:=TT+10);
      PRIN2!* " )";
      IF J=(N/2) THEN PRIN2!* "  =  (  " ELSE PRIN2!* "     (  ";
      PRINSF GETV(CV,J);
      TTAB!* (TT:=TT+30); PRINTSTR ")";
      IF NOT(J=N) THEN <<
        TT:=2;
        PRIN2!* "(";
        TTAB!* (TT:=TT+N*10);
        PRIN2!* ")  (";
        TTAB!* (TT:=TT+10);
        PRIN2!* " )     (";
        TTAB!* (TT:=TT+30);
        PRINTSTR ")" >> >>;
    TERPRI!*(T)
  END;

SYMBOLIC PROCEDURE TRY!-PREDICTION(SM,CV,PV,N,I,POLY,V,FF,FFHAT,
                                        LU!-DECOMPN!-DONE);
  BEGIN SCALAR W,FFI,FHATI;
    SM:=GETV(SM,I);
    CV:=GETV(CV,I);
    PV:=GETV(PV,I);
    IF NOT(N=IADD1 GETV(PV,0)) THEN
      ERRORF LIST("Predicted unknowns gone wrong? ",N,IADD1 GETV(PV,0));
    IF NOT LU!-DECOMPN!-DONE THEN <<
      W:=LU!-FACTORIZE!-MOD!-P(SM,N);
      IF W='SINGULAR THEN <<
        FACTOR!-TRACE <<
          PRIN2!* "Prediction for ";
          PRIN2!* IF NULL FF THEN 'f ELSE 'a;
          PRIN2!* "("; PRIN2!* I;
          PRINTSTR ") failed due to singular matrix." >>;
        RETURN (W . I) >> >>;
    BACK!-SUBSTITUTE(SM,CV,N);
    W:=
      IF NULL FF THEN TRY!-FACTOR(POLY,CV,PV,N,V)
      ELSE <<
	FFI := GETV(FF,I);
	FHATI := GETV(FFHAT,I); % The unfolding here is to get round
				% a bug in the PSL compiler 12/9/82. It
				% will be tidied back up as soon as
				% possible;
	TRY!-ALPHA(POLY,CV,PV,N,V,FFI,FHATI) >>;
    IF W='BAD!-PREDICTION THEN <<
      FACTOR!-TRACE <<
        PRIN2!* "Prediction for ";
        PRIN2!* IF NULL FF THEN 'f ELSE 'a;
        PRIN2!* "("; PRIN2!* I;
        PRINTSTR ") was an inadequate guess." >>;
      RETURN (W . I) >>;
    FACTOR!-TRACE <<
      PRIN2!* "Prediction for ";
      PRIN2!* IF NULL FF THEN 'f ELSE 'a;
      PRIN2!* "("; PRIN2!* I; PRIN2!* ") worked: ";
      FAC!-PRINTSF CAR W >>;
    RETURN (I . W)
  END;

SYMBOLIC PROCEDURE TRY!-FACTOR(POLY,TESTV,PREDICTEDF,N,V);
  BEGIN SCALAR R,W;
    R:=GETV(TESTV,1);
    FOR J:=2:N DO <<
      W:=!*F2MOD ADJOIN!-TERM(MKSP(V,GETV(PREDICTEDF,ISUB1 J)),1,
			      POLYZERO);
      R:=PLUS!-MOD!-P(R,TIMES!-MOD!-P(W,GETV(TESTV,J))) >>;
    W:=QUOTIENT!-MOD!-P(POLY,R);
    IF DIDNTGO W OR
      NOT POLYZEROP DIFFERENCE!-MOD!-P(POLY,TIMES!-MOD!-P(W,R)) THEN
      RETURN 'BAD!-PREDICTION
    ELSE RETURN LIST(R,W)
  END;

SYMBOLIC PROCEDURE TRY!-ALPHA(POLY,TESTV,PREDICTEDF,N,V,FI,FHATI);
  BEGIN SCALAR R,W,WR;
    R:=GETV(TESTV,1);
    FOR J:=2:N DO <<
      W:=!*F2MOD ADJOIN!-TERM(MKSP(V,GETV(PREDICTEDF,ISUB1 J)),1,
			      POLYZERO);
      R:=PLUS!-MOD!-P(R,TIMES!-MOD!-P(W,GETV(TESTV,J))) >>;
    IF POLYZEROP
      (WR:=DIFFERENCE!-MOD!-P(POLY,TIMES!-MOD!-P(R,FHATI))) THEN
      RETURN LIST (R,WR);
    W:=QUOTIENT!-MOD!-P(WR,FI);
    IF DIDNTGO W OR
      NOT POLYZEROP DIFFERENCE!-MOD!-P(WR,TIMES!-MOD!-P(W,FI)) THEN
      RETURN 'BAD!-PREDICTION
    ELSE RETURN LIST(R,WR)
  END;



ENDMODULE;


MODULE MODPOLY;

% *******************************************************************
%
%   copyright (c)  university of cambridge, england 1979
%
% *******************************************************************;




%**********************************************************************;
% routines for performing arithmetic on multivariate
% polynomials with coefficients that are modular
% numbers as defined by modular!-plus etc;

% note that the datastructure used is the same as that used in
% REDUCE except that it is assumesd that domain elements are atomic;



SYMBOLIC PROCEDURE PLUS!-MOD!-P(A,B);
% form the sum of the two polynomials a and b
% working over the ground domain defined by the routines
% modular!-plus, modular!-times etc. the inputs to this
% routine are assumed to have coefficients already
% in the required domain;
   IF NULL A THEN B
   ELSE IF NULL B THEN A
   ELSE IF ISDOMAIN A THEN
      IF ISDOMAIN B THEN !*NUM2F MODULAR!-PLUS(A,B)
      ELSE (LT B) .+ PLUS!-MOD!-P(A,RED B)
   ELSE IF ISDOMAIN B THEN (LT A) .+ PLUS!-MOD!-P(RED A,B)
   ELSE IF LPOW A = LPOW B THEN
      ADJOIN!-TERM(LPOW A,
         PLUS!-MOD!-P(LC A,LC B),PLUS!-MOD!-P(RED A,RED B))
   ELSE IF COMES!-BEFORE(LPOW A,LPOW B) THEN
         (LT A) .+ PLUS!-MOD!-P(RED A,B)
   ELSE (LT B) .+ PLUS!-MOD!-P(A,RED B);



SYMBOLIC PROCEDURE TIMES!-MOD!-P(A,B);
   IF (NULL A) OR (NULL B) THEN NIL
   ELSE IF ISDOMAIN A THEN MULTIPLY!-BY!-CONSTANT!-MOD!-P(B,A)
   ELSE IF ISDOMAIN B THEN MULTIPLY!-BY!-CONSTANT!-MOD!-P(A,B)
   ELSE IF MVAR A=MVAR B THEN PLUS!-MOD!-P(
     PLUS!-MOD!-P(TIMES!-TERM!-MOD!-P(LT A,B),
                  TIMES!-TERM!-MOD!-P(LT B,RED A)),
     TIMES!-MOD!-P(RED A,RED B))
   ELSE IF ORDOP(MVAR A,MVAR B) THEN
     ADJOIN!-TERM(LPOW A,TIMES!-MOD!-P(LC A,B),TIMES!-MOD!-P(RED A,B))
   ELSE ADJOIN!-TERM(LPOW B,
        TIMES!-MOD!-P(A,LC B),TIMES!-MOD!-P(A,RED B));


SYMBOLIC PROCEDURE TIMES!-TERM!-MOD!-P(TERM,B);
%multiply the given polynomial by the given term;
    IF NULL B THEN NIL
    ELSE IF ISDOMAIN B THEN
        ADJOIN!-TERM(TPOW TERM,
            MULTIPLY!-BY!-CONSTANT!-MOD!-P(TC TERM,B),NIL)
    ELSE IF TVAR TERM=MVAR B THEN
         ADJOIN!-TERM(MKSP(TVAR TERM,IPLUS(TDEG TERM,LDEG B)),
                      TIMES!-MOD!-P(TC TERM,LC B),
                      TIMES!-TERM!-MOD!-P(TERM,RED B))
    ELSE IF ORDOP(TVAR TERM,MVAR B) THEN
      ADJOIN!-TERM(TPOW TERM,TIMES!-MOD!-P(TC TERM,B),NIL)
    ELSE ADJOIN!-TERM(LPOW B,
      TIMES!-TERM!-MOD!-P(TERM,LC B),
      TIMES!-TERM!-MOD!-P(TERM,RED B));

SYMBOLIC PROCEDURE DIFFERENCE!-MOD!-P(A,B);
   PLUS!-MOD!-P(A,MINUS!-MOD!-P B);

SYMBOLIC PROCEDURE MINUS!-MOD!-P A;
   IF NULL A THEN NIL
   ELSE IF ISDOMAIN A THEN MODULAR!-MINUS A
   ELSE (LPOW A .* MINUS!-MOD!-P LC A) .+ MINUS!-MOD!-P RED A;


SYMBOLIC PROCEDURE REDUCE!-MOD!-P A;
%converts a multivariate poly from normal into modular polynomial;
    IF NULL A THEN NIL
    ELSE IF ISDOMAIN A THEN !*NUM2F MODULAR!-NUMBER A
    ELSE ADJOIN!-TERM(LPOW A,REDUCE!-MOD!-P LC A,REDUCE!-MOD!-P RED A);

SYMBOLIC PROCEDURE MONIC!-MOD!-P A;
% This procedure can only cope with polys that have a numeric
% leading coeff;
   IF A=NIL THEN NIL
   ELSE IF ISDOMAIN A THEN 1
   ELSE IF LC A = 1 THEN A
   ELSE IF NOT DOMAINP LC A THEN
       ERRORF "LC NOT NUMERIC IN MONIC-MOD-P"
   ELSE MULTIPLY!-BY!-CONSTANT!-MOD!-P(A,
     MODULAR!-RECIPROCAL LC A);


SYMBOLIC PROCEDURE QUOTFAIL!-MOD!-P(A,B);
% Form quotient A/B, but complain if the division is
% not exact;
  BEGIN
    SCALAR C;
    EXACT!-QUOTIENT!-FLAG:=T;
    C:=QUOTIENT!-MOD!-P(A,B);
    IF EXACT!-QUOTIENT!-FLAG THEN RETURN C
    ELSE ERRORF "QUOTIENT NOT EXACT (MOD P)"
  END;

SYMBOLIC PROCEDURE QUOTIENT!-MOD!-P(A,B);
% truncated quotient of a by b;
    IF NULL B THEN ERRORF "B=0 IN QUOTIENT-MOD-P"
    ELSE IF ISDOMAIN B THEN MULTIPLY!-BY!-CONSTANT!-MOD!-P(A,
                             MODULAR!-RECIPROCAL B)
    ELSE IF A=NIL THEN NIL
    ELSE IF ISDOMAIN A THEN EXACT!-QUOTIENT!-FLAG:=NIL
    ELSE IF MVAR A=MVAR B THEN XQUOTIENT!-MOD!-P(A,B,MVAR B)
    ELSE IF ORDOP(MVAR A,MVAR B) THEN
       ADJOIN!-TERM(LPOW A,
          QUOTIENT!-MOD!-P(LC A,B),
          QUOTIENT!-MOD!-P(RED A,B))
    ELSE EXACT!-QUOTIENT!-FLAG:=NIL;


SYMBOLIC PROCEDURE XQUOTIENT!-MOD!-P(A,B,V);
% truncated quotient a/b given that b is nontrivial;
    IF A=NIL THEN NIL
    ELSE IF (ISDOMAIN A) OR (NOT MVAR A=V) OR
      ILESSP(LDEG A,LDEG B) THEN EXACT!-QUOTIENT!-FLAG:=NIL
    ELSE IF LDEG A = LDEG B THEN BEGIN SCALAR W;
      W:=QUOTIENT!-MOD!-P(LC A,LC B);
      IF DIFFERENCE!-MOD!-P(A,TIMES!-MOD!-P(W,B)) THEN
        EXACT!-QUOTIENT!-FLAG:=NIL;
      RETURN W
      END
    ELSE BEGIN SCALAR TERM;
      TERM:=MKSP(MVAR A,IDIFFERENCE(LDEG A,LDEG B)) .*
        QUOTIENT!-MOD!-P(LC A,LC B);
%that is the leading term of the quotient. now subtract
%term*b from a;
      A:=PLUS!-MOD!-P(RED A,
		      TIMES!-TERM!-MOD!-P(NEGATE!-TERM TERM,RED B));
% or a:=a-b*term given leading terms must cancel;
      RETURN TERM .+ XQUOTIENT!-MOD!-P(A,B,V)
    END;

SYMBOLIC PROCEDURE NEGATE!-TERM TERM;
% negate a term;
    TPOW TERM .* MINUS!-MOD!-P TC TERM;


SYMBOLIC PROCEDURE REMAINDER!-MOD!-P(A,B);
% remainder when a is divided by b;
    IF NULL B THEN ERRORF "B=0 IN REMAINDER-MOD-P"
    ELSE IF ISDOMAIN B THEN NIL
    ELSE IF ISDOMAIN A THEN A
    ELSE XREMAINDER!-MOD!-P(A,B,MVAR B);


SYMBOLIC PROCEDURE XREMAINDER!-MOD!-P(A,B,V);
% remainder when the modular polynomial a is
% divided by b, given that b is non degenerate;
   IF (ISDOMAIN A) OR (NOT MVAR A=V) OR ILESSP(LDEG A,LDEG B) THEN A
   ELSE BEGIN
    SCALAR Q,W;
    Q:=QUOTIENT!-MOD!-P(MINUS!-MOD!-P LC A,LC B);
% compute -lc of quotient;
    W:=IDIFFERENCE(LDEG A,LDEG B); %ldeg of quotient;
    IF W=0 THEN A:=PLUS!-MOD!-P(RED A,
      MULTIPLY!-BY!-CONSTANT!-MOD!-P(RED B,Q))
    ELSE
      A:=PLUS!-MOD!-P(RED A,TIMES!-TERM!-MOD!-P(
            MKSP(MVAR B,W) .* Q,RED B));
% the above lines of code use red a and red b because
% by construction the leading terms of the required
% answers will cancel out;
     RETURN XREMAINDER!-MOD!-P(A,B,V)
   END;

SYMBOLIC PROCEDURE MULTIPLY!-BY!-CONSTANT!-MOD!-P(A,N);
% multiply the polynomial a by the constant n;
   IF NULL A THEN NIL
   ELSE IF N=1 THEN A
   ELSE IF ISDOMAIN A THEN !*NUM2F MODULAR!-TIMES(A,N)
   ELSE ADJOIN!-TERM(LPOW A,MULTIPLY!-BY!-CONSTANT!-MOD!-P(LC A,N),
     MULTIPLY!-BY!-CONSTANT!-MOD!-P(RED A,N));



SYMBOLIC PROCEDURE GCD!-MOD!-P(A,B);
% return the monic gcd of the two modular univariate
% polynomials a and b. Set REDUCTION-COUNT to the number
% of steps taken in the process;
 << REDUCTION!-COUNT := 0;
    IF NULL A THEN MONIC!-MOD!-P B
    ELSE IF NULL B THEN MONIC!-MOD!-P A
    ELSE IF ISDOMAIN A THEN 1
    ELSE IF ISDOMAIN B THEN 1
    ELSE IF IGREATERP(LDEG A,LDEG B) THEN
      ORDERED!-GCD!-MOD!-P(A,B)
    ELSE ORDERED!-GCD!-MOD!-P(B,A) >>;


SYMBOLIC PROCEDURE ORDERED!-GCD!-MOD!-P(A,B);
% as above, but deg a > deg b;
  BEGIN
    SCALAR STEPS;
    STEPS := 0;
TOP:
    A := REDUCE!-DEGREE!-MOD!-P(A,B);
    IF NULL A THEN RETURN MONIC!-MOD!-P B;
    STEPS := STEPS + 1;
    IF DOMAINP A THEN <<
        REDUCTION!-COUNT := REDUCTION!-COUNT+STEPS;
        RETURN 1 >>
    ELSE IF LDEG A<LDEG B THEN BEGIN
      SCALAR W;
      REDUCTION!-COUNT := REDUCTION!-COUNT + STEPS;
      STEPS := 0;
      W := A; A := B; B := W
      END;
    GO TO TOP
  END;


SYMBOLIC PROCEDURE REDUCE!-DEGREE!-MOD!-P(A,B);
% Compute A-Q*B where Q is a single term chosen so that the result
% has lower degree than A did;
  BEGIN
    SCALAR Q,W;
    Q:=MODULAR!-QUOTIENT(MODULAR!-MINUS LC A,LC B);
% compute -lc of quotient;
    W:=IDIFFERENCE(LDEG A,LDEG B); %ldeg of quotient;
% the next lines of code use red a and red b because
% by construction the leading terms of the required
% answers will cancel out;
    IF W=0 THEN RETURN PLUS!-MOD!-P(RED A,
      MULTIPLY!-BY!-CONSTANT!-MOD!-P(RED B,Q))
    ELSE
      RETURN PLUS!-MOD!-P(RED A,TIMES!-TERM!-MOD!-P(
            MKSP(MVAR B,W) .* Q,RED B))
   END;

SYMBOLIC PROCEDURE DERIVATIVE!-MOD!-P A;
% derivative of a wrt its main variable;
   IF ISDOMAIN A THEN NIL
   ELSE IF LDEG A=1 THEN LC A
   ELSE DERIVATIVE!-MOD!-P!-1(A,MVAR A);

SYMBOLIC PROCEDURE DERIVATIVE!-MOD!-P!-1(A,V);
    IF ISDOMAIN A THEN NIL
    ELSE IF NOT MVAR A=V THEN NIL
    ELSE IF LDEG A=1 THEN LC A
   ELSE ADJOIN!-TERM(MKSP(V,ISUB1 LDEG A),
		 MULTIPLY!-BY!-CONSTANT!-MOD!-P(LC A,
						MODULAR!-NUMBER LDEG A),
                 DERIVATIVE!-MOD!-P!-1(RED A,V));

SYMBOLIC PROCEDURE SQUARE!-FREE!-MOD!-P A;
% predicate that tests if a is square-free as a modular
% univariate polynomial;
    IF ISDOMAIN A THEN T
    ELSE ISDOMAIN GCD!-MOD!-P(A,DERIVATIVE!-MOD!-P A);


SYMBOLIC PROCEDURE EVALUATE!-MOD!-P(A,V,N);
% evaluate polynomial A at the point V=N;
    IF ISDOMAIN A THEN A
    ELSE IF V=NIL THEN ERRORF "Variable=NIL in EVALUATE-MOD-P"
    ELSE IF MVAR A=V THEN HORNER!-RULE!-MOD!-P(LC A,LDEG A,RED A,N,V)
    ELSE ADJOIN!-TERM(LPOW A,
      EVALUATE!-MOD!-P(LC A,V,N),
      EVALUATE!-MOD!-P(RED A,V,N));

SYMBOLIC PROCEDURE HORNER!-RULE!-MOD!-P(V,DEGG,A,N,VAR);
% v is the running total, and it must be multiplied by
% n**deg and added to the value of a at n;
    IF ISDOMAIN A OR NOT MVAR A=VAR THEN <<
      V:=TIMES!-MOD!-P(V,EXPT!-MOD!-P(N,DEGG));
      PLUS!-MOD!-P(A,V) >>
    ELSE BEGIN
      SCALAR NEWDEG;
      NEWDEG:=LDEG A;
      RETURN HORNER!-RULE!-MOD!-P(PLUS!-MOD!-P(LC A,
         TIMES!-MOD!-P(V,EXPT!-MOD!-P(N,IDIFFERENCE(DEGG,NEWDEG)))),
       NEWDEG,RED A,N,VAR)
    END;




SYMBOLIC PROCEDURE EXPT!-MOD!-P(A,N);
% a**n;
    IF N=0 THEN 1
    ELSE IF N=1 THEN A
    ELSE BEGIN
     SCALAR W,X;
     W:=DIVIDE(N,2);
     X:=EXPT!-MOD!-P(A,CAR W);
     X:=TIMES!-MOD!-P(X,X);
     IF NOT (CDR W = 0) THEN X:=TIMES!-MOD!-P(X,A);
     RETURN X
    END;

SYMBOLIC PROCEDURE MAKE!-BIVARIATE!-MOD!-P(U,IMSET,V);
% Substitute into U for all variables in IMSET which should result in
% a bivariate poly. One variable is M-IMAGE-VARIABLE and V is the other
% U is modular multivariate with these two variables at top 2 levels
% - V at 2nd level;
  IF DOMAINP U THEN U
  ELSE IF MVAR U = M!-IMAGE!-VARIABLE THEN
    ADJOIN!-TERM(LPOW U,MAKE!-UNIVARIATE!-MOD!-P(LC U,IMSET,V),
      MAKE!-BIVARIATE!-MOD!-P(RED U,IMSET,V))
  ELSE MAKE!-UNIVARIATE!-MOD!-P(U,IMSET,V);

SYMBOLIC PROCEDURE MAKE!-UNIVARIATE!-MOD!-P(U,IMSET,V);
% Substitute into U for all variables in IMSET giving a univariate
% poly in V. U is modular multivariate with V at top level;
  IF DOMAINP U THEN U
  ELSE IF MVAR U = V THEN
    ADJOIN!-TERM(LPOW U,!*NUM2F EVALUATE!-IN!-ORDER!-MOD!-P(LC U,IMSET),
      MAKE!-UNIVARIATE!-MOD!-P(RED U,IMSET,V))
  ELSE !*NUM2F EVALUATE!-IN!-ORDER!-MOD!-P(U,IMSET);

SYMBOLIC PROCEDURE EVALUATE!-IN!-ORDER!-MOD!-P(U,IMSET);
% makes an image of u wrt imageset, imset, using horner's rule. result
% should be purely numeric (and modular);
  IF DOMAINP U THEN !*D2N U
  ELSE IF MVAR U=CAAR IMSET THEN
    HORNER!-RULE!-IN!-ORDER!-MOD!-P(
      EVALUATE!-IN!-ORDER!-MOD!-P(LC U,CDR IMSET),LDEG U,RED U,IMSET)
  ELSE EVALUATE!-IN!-ORDER!-MOD!-P(U,CDR IMSET);

SYMBOLIC PROCEDURE HORNER!-RULE!-IN!-ORDER!-MOD!-P(C,DEGG,A,VSET);
% c is running total and a is what is left;
  IF DOMAINP A THEN MODULAR!-PLUS(!*D2N A,
    MODULAR!-TIMES(C,MODULAR!-EXPT(CDAR VSET,DEGG)))
  ELSE IF NOT(MVAR A=CAAR VSET) THEN
    MODULAR!-PLUS(
      EVALUATE!-IN!-ORDER!-MOD!-P(A,CDR VSET),
      MODULAR!-TIMES(C,MODULAR!-EXPT(CDAR VSET,DEGG)))
  ELSE BEGIN SCALAR NEWDEG;
    NEWDEG:=LDEG A;
    RETURN HORNER!-RULE!-IN!-ORDER!-MOD!-P(
      MODULAR!-PLUS(
        EVALUATE!-IN!-ORDER!-MOD!-P(LC A,CDR VSET),
        MODULAR!-TIMES(C,
          MODULAR!-EXPT(CDAR VSET,(IDIFFERENCE(DEGG,NEWDEG))))),
      NEWDEG,RED A,VSET)
  END;

SYMBOLIC PROCEDURE MAKE!-MODULAR!-SYMMETRIC A;
% input is a multivariate MODULAR poly A with nos in the range 0->(p-1).
% This folds it onto the symmetric range (-p/2)->(p/2);
    IF NULL A THEN NIL
    ELSE IF DOMAINP A THEN
      IF A>MODULUS!/2 THEN !*NUM2F(A - CURRENT!-MODULUS)
      ELSE A
    ELSE ADJOIN!-TERM(LPOW A,MAKE!-MODULAR!-SYMMETRIC LC A,
      MAKE!-MODULAR!-SYMMETRIC RED A);



ENDMODULE;


MODULE MULTIHEN;

% *******************************************************************
%
%   copyright (c)  university of cambridge, england 1979
%
% *******************************************************************;





%**********************************************************************;
%    hensel construction for the multivariate case
%     (this version is highly recursive);



SYMBOLIC PROCEDURE FIND!-MULTIVARIATE!-FACTORS!-MOD!-P(POLY,
    BEST!-FACTORS,VARIABLE!-SET);
% All arithmetic is done mod p, best-factors is overwritten;
    IF NULL VARIABLE!-SET THEN BEST!-FACTORS
    ELSE (LAMBDA FACTOR!-LEVEL; BEGIN
    SCALAR GROWTH!-FACTOR,B0S,RES,CORRECTION!-FACTOR,SUBSTRES,V,
           B1,BHAT0S,W,K,DEGBD,FIRST!-TIME,REDPOLY,D,
           PREDICTED!-FORMS,NUMBER!-OF!-UNKNOWNS,SOLVE!-COUNT,
           CORRECTION!-VECTORS,SOLN!-MATRICES,MAX!-UNKNOWNS,
           UNKNOWNS!-COUNT!-LIST,TEST!-PREDICTION,POLY!-REMAINING,
           PREDICTION!-RESULTS,ONE!-PREDICTION!-FAILED,KK;
    V:=CAR VARIABLE!-SET;
    DEGBD:=GET!-DEGREE!-BOUND CAR V;
    FIRST!-TIME:=T;
    GROWTH!-FACTOR:=MAKE!-GROWTH!-FACTOR V;
    POLY!-REMAINING:=POLY;
    PREDICTION!-RESULTS:=MKVECT NUMBER!-OF!-FACTORS;
    FACTOR!-TRACE <<
      PRINTSTR "Want f(i) s.t.";
      PRIN2!* "  product over i [ f(i) ] = ";
      PRINSF POLY;
      PRIN2!* " mod ";
      PRINTSTR HENSEL!-GROWTH!-SIZE;
      TERPRI!*(NIL);
      PRINTSTR "We know f(i) as follows:";
      PRINTVEC("  f(",NUMBER!-OF!-FACTORS,") = ",BEST!-FACTORS);
      PRIN2!* " and we shall put in powers of ";
      PRINSF GROWTH!-FACTOR;
      PRINTSTR " to find them fully."
    >>;
    B0S:=REDUCE!-VEC!-BY!-ONE!-VAR!-MOD!-P(BEST!-FACTORS,
                    V,NUMBER!-OF!-FACTORS);
            % The above made a copy of the vector;
    FOR I:=1:NUMBER!-OF!-FACTORS DO
      PUTV(BEST!-FACTORS,I,
        DIFFERENCE!-MOD!-P(GETV(BEST!-FACTORS,I),GETV(B0S,I)));
    REDPOLY:=EVALUATE!-MOD!-P(POLY,CAR V,CDR V);
    FACTOR!-TRACE <<
      PRIN2!*
	 "First solve the problem in one less variable by putting ";
      PRINVAR CAR V; PRIN2!* "="; PRINTSTR CDR V;
      IF CDR VARIABLE!-SET THEN <<
        PRIN2!* "and growing wrt ";
        PRINTVAR CAADR VARIABLE!-SET
        >>;
      TERPRI!*(NIL)
    >>;
    FIND!-MULTIVARIATE!-FACTORS!-MOD!-P(REDPOLY,B0S,CDR VARIABLE!-SET);
            % answers in b0s;
    IF BAD!-CASE THEN RETURN;
    FOR I:=1:NUMBER!-OF!-FACTORS DO
      PUTV(BEST!-FACTORS,I,
        PLUS!-MOD!-P(GETV(B0S,I),GETV(BEST!-FACTORS,I)));
    FACTOR!-TRACE <<
      PRIN2!* "After putting back any knowledge of ";
      PRINVAR CAR V;
      PRINTSTR ", we have the";
      PRINTSTR "factors so far as:";
      PRINTVEC("  f(",NUMBER!-OF!-FACTORS,") = ",BEST!-FACTORS);
      PRINTSTR "Subtracting the product of these from the polynomial";
      PRIN2!* "and differentiating wrt "; PRINVAR CAR V;
      PRINTSTR " gives a residue:"
    >>;
    RES:=DIFF!-OVER!-K!-MOD!-P(
        DIFFERENCE!-MOD!-P(POLY,
          TIMES!-VECTOR!-MOD!-P(BEST!-FACTORS,NUMBER!-OF!-FACTORS)),
        1,CAR V);
            % RES is the residue and must eventually be reduced to zero;
    FACTOR!-TRACE << FAC!-PRINTSF RES; TERPRI!*(NIL) >>;
    IF NOT POLYZEROP RES AND
      CDR VARIABLE!-SET AND NOT ZEROP CDR V THEN <<
      PREDICTED!-FORMS:=MAKE!-BIVARIATE!-VEC!-MOD!-P(BEST!-FACTORS,
        CDR VARIABLE!-SET,CAR V,NUMBER!-OF!-FACTORS);
      FIND!-MULTIVARIATE!-FACTORS!-MOD!-P(
        MAKE!-BIVARIATE!-MOD!-P(POLY,CDR VARIABLE!-SET,CAR V),
        PREDICTED!-FORMS,LIST V);
            % answers in PREDICTED!-FORMS;
      FACTOR!-TRACE <<
        PRINTSTR "To help reduce the number of Hensel steps we try";
        PRIN2!* "predicting how many terms each factor will have wrt ";
        PRINVAR CAR V; PRINTSTR ".";
        PRINTSTR
          "Predictions are based on the bivariate factors :";
        PRINTVEC("     f(",NUMBER!-OF!-FACTORS,") = ",PREDICTED!-FORMS)
        >>;
      MAKE!-PREDICTED!-FORMS(PREDICTED!-FORMS,CAR V);
            % sets max!-unknowns and number!-of!-unknowns;
      FACTOR!-TRACE <<
        TERPRI!*(NIL);
        PRINTSTR "We predict :";
        FOR EACH W IN NUMBER!-OF!-UNKNOWNS DO <<
          PRIN2!* CAR W;
          PRIN2!* " terms in f("; PRIN2!* CDR W; PRINTSTR '!) >>;
        IF (CAAR NUMBER!-OF!-UNKNOWNS)=1 THEN <<
          PRIN2!* "Since we predict only one term for f(";
          PRIN2!* CDAR NUMBER!-OF!-UNKNOWNS;
          PRINTSTR "), we can try";
          PRINTSTR "dividing it out now:" >>
        ELSE <<
          PRIN2!* "So we shall do at least ";
          PRIN2!* ISUB1 CAAR NUMBER!-OF!-UNKNOWNS;
          PRIN2!* " Hensel step";
          IF (CAAR NUMBER!-OF!-UNKNOWNS)=2 THEN PRINTSTR "."
          ELSE PRINTSTR "s." >>;
        TERPRI!*(NIL) >>;
      UNKNOWNS!-COUNT!-LIST:=NUMBER!-OF!-UNKNOWNS;
      WHILE UNKNOWNS!-COUNT!-LIST AND
         (CAR (W:=CAR UNKNOWNS!-COUNT!-LIST))=1 DO
        BEGIN SCALAR I,R;
          UNKNOWNS!-COUNT!-LIST:=CDR UNKNOWNS!-COUNT!-LIST;
          I:=CDR W;
          W:=QUOTIENT!-MOD!-P(POLY!-REMAINING,R:=GETV(BEST!-FACTORS,I));
          IF DIDNTGO W OR
            NOT POLYZEROP DIFFERENCE!-MOD!-P(POLY!-REMAINING,
            TIMES!-MOD!-P(W,R)) THEN
            IF ONE!-PREDICTION!-FAILED THEN <<
              FACTOR!-TRACE PRINTSTR "Predictions are no good";
              MAX!-UNKNOWNS:=NIL >>
            ELSE <<
              FACTOR!-TRACE <<
                PRIN2!* "Guess for f(";
                PRIN2!* I;
                PRINTSTR ") was bad." >>;
              ONE!-PREDICTION!-FAILED:=I >>
          ELSE <<
            PUTV(PREDICTION!-RESULTS,I,R);
            FACTOR!-TRACE <<
	      PRIN2!* "Prediction for f("; PRIN2!* I;
	      PRIN2!* ") worked: ";
	      FAC!-PRINTSF R >>;
            POLY!-REMAINING:=W >>
        END;
      W:=LENGTH UNKNOWNS!-COUNT!-LIST;
      IF W=1 AND NOT ONE!-PREDICTION!-FAILED THEN <<
        PUTV(BEST!-FACTORS,CDAR UNKNOWNS!-COUNT!-LIST,POLY!-REMAINING);
        GOTO EXIT >>
      ELSE IF W=0 AND ONE!-PREDICTION!-FAILED THEN <<
        PUTV(BEST!-FACTORS,ONE!-PREDICTION!-FAILED,POLY!-REMAINING);
        GOTO EXIT >>;
      SOLVE!-COUNT:=1;
      IF MAX!-UNKNOWNS THEN
        CORRECTION!-VECTORS:=MAKE!-CORRECTION!-VECTORS(PREDICTED!-FORMS,
        BEST!-FACTORS,MAX!-UNKNOWNS) >>;
    BHAT0S:=MAKE!-MULTIVARIATE!-HATVEC!-MOD!-P(B0S,NUMBER!-OF!-FACTORS);
    K:=1;
    KK:=0;
    CORRECTION!-FACTOR:=GROWTH!-FACTOR;
            % next power of growth-factor we are
            % adding to the factors;
    B1:=MKVECT NUMBER!-OF!-FACTORS;
TEMPLOOP:
    WHILE NOT POLYZEROP RES AND (NULL MAX!-UNKNOWNS
                  OR NULL TEST!-PREDICTION) DO
      IF K>DEGBD THEN RETURN <<
        FACTOR!-TRACE <<
          PRIN2!* "We have overshot the degree bound for ";
          PRINTVAR CAR V >>;
        IF !*OVERSHOOT THEN
          PRINTC "Multivariate degree bound overshoot -> restart";
        BAD!-CASE:=T >>
      ELSE
	IF POLYZEROP(SUBSTRES:=EVALUATE!-MOD!-P(RES,CAR V,CDR V))
	THEN <<
        K:=IADD1 K;
        RES:=DIFF!-OVER!-K!-MOD!-P(RES,K,CAR V);
        CORRECTION!-FACTOR:=
          TIMES!-MOD!-P(CORRECTION!-FACTOR,GROWTH!-FACTOR) >>
      ELSE <<
        FACTOR!-TRACE <<
          PRIN2!* "Hensel Step "; PRINTSTR (KK:=KK #+ 1);
          PRIN2!* "-------------";
          IF KK>10 THEN PRINTSTR "-" ELSE TERPRI!*(T);
          PRIN2!* "Next corrections are for (";
          PRINSF GROWTH!-FACTOR;
          IF NOT (K=1) THEN <<
            PRIN2!* ") ** ";
            PRIN2!* K >> ELSE PRIN2!* '!);
          PRINTSTR ". To find these we solve:";
          PRIN2!* "     sum over i [ f(i,1)*fhat(i,0) ] = ";
          PRINSF SUBSTRES;
          PRIN2!* " mod ";
          PRIN2!* HENSEL!-GROWTH!-SIZE;
          PRINTSTR " for f(i,1), ";
          IF FIRST!-TIME THEN <<
            FIRST!-TIME:=NIL;
	    PRIN2!*
	       "       where fhat(i,0) = product over j [ f(j,0) ]";
            PRIN2!* " / f(i,0) mod ";
            PRINTSTR HENSEL!-GROWTH!-SIZE >>;
          TERPRI!*(NIL)
        >>;
	SOLVE!-FOR!-CORRECTIONS(SUBSTRES,BHAT0S,B0S,B1,
				CDR VARIABLE!-SET);
            % Answers left in B1;
        IF BAD!-CASE THEN RETURN;
        IF MAX!-UNKNOWNS THEN <<
          SOLVE!-COUNT:=IADD1 SOLVE!-COUNT;
          FOR I:=1:NUMBER!-OF!-FACTORS DO
            PUTV(GETV(CORRECTION!-VECTORS,I),SOLVE!-COUNT,GETV(B1,I));
          IF SOLVE!-COUNT=CAAR UNKNOWNS!-COUNT!-LIST THEN
            TEST!-PREDICTION:=T >>;
        FACTOR!-TRACE <<
          PRINTSTR "   Giving:";
          PRINTVEC("     f(",NUMBER!-OF!-FACTORS,",1) = ",B1) >>;
        D:=TIMES!-MOD!-P(CORRECTION!-FACTOR,
            TERMS!-DONE!-MOD!-P(BEST!-FACTORS,B1,CORRECTION!-FACTOR));
        IF DEGREE!-IN!-VARIABLE(D,CAR V)>DEGBD THEN RETURN <<
          FACTOR!-TRACE <<
            PRIN2!* "We have overshot the degree bound for ";
            PRINTVAR CAR V >>;
          IF !*OVERSHOOT THEN
            PRINTC "Multivariate degree bound overshoot -> restart";
          BAD!-CASE:=T >>;
        D:=DIFF!-K!-TIMES!-MOD!-P(D,K,CAR V);
        FOR I:=1:NUMBER!-OF!-FACTORS DO
          PUTV(BEST!-FACTORS,I,
            PLUS!-MOD!-P(GETV(BEST!-FACTORS,I),
              TIMES!-MOD!-P(GETV(B1,I),CORRECTION!-FACTOR)));
        K:=IADD1 K;
        RES:=DIFF!-OVER!-K!-MOD!-P(DIFFERENCE!-MOD!-P(RES,D),K,CAR V);
        FACTOR!-TRACE <<
          PRINTSTR "   New factors are now:";
          PRINTVEC("     f(",NUMBER!-OF!-FACTORS,") = ",BEST!-FACTORS);
          PRIN2!* "   and residue = ";
	  FAC!-PRINTSF RES;
          PRINTSTR "-------------"
        >>;
        CORRECTION!-FACTOR:=
          TIMES!-MOD!-P(CORRECTION!-FACTOR,GROWTH!-FACTOR) >>;
    IF NOT POLYZEROP RES AND NOT BAD!-CASE THEN <<
      SOLN!-MATRICES:=CONSTRUCT!-SOLN!-MATRICES(PREDICTED!-FORMS,CDR V);
      FACTOR!-TRACE <<
        PRINTSTR "We use the results from the Hensel growth to";
        PRINTSTR "produce a set of linear equations to solve";
        PRINTSTR "for coefficients in the relevent factors:" >>;
      WHILE UNKNOWNS!-COUNT!-LIST AND
        (CAR (W:=CAR UNKNOWNS!-COUNT!-LIST))=SOLVE!-COUNT DO <<
        UNKNOWNS!-COUNT!-LIST:=CDR UNKNOWNS!-COUNT!-LIST;
        FACTOR!-TRACE
          PRINT!-LINEAR!-SYSTEM(CDR W,SOLN!-MATRICES,
            CORRECTION!-VECTORS,PREDICTED!-FORMS,CAR V);
        W:=TRY!-PREDICTION(SOLN!-MATRICES,CORRECTION!-VECTORS,
	     PREDICTED!-FORMS,CAR W,CDR W,POLY!-REMAINING,CAR V,
	     NIL,NIL,NIL);
        IF CAR W='SINGULAR OR CAR W='BAD!-PREDICTION THEN
          IF ONE!-PREDICTION!-FAILED THEN <<
            FACTOR!-TRACE PRINTSTR "Predictions were no help.";
            RETURN MAX!-UNKNOWNS:=NIL >>
          ELSE ONE!-PREDICTION!-FAILED:=CDR W
        ELSE <<
          PUTV(PREDICTION!-RESULTS,CAR W,CADR W);
          POLY!-REMAINING:=CADDR W >> >>;
      IF NULL MAX!-UNKNOWNS THEN GOTO TEMPLOOP;
      W:=LENGTH UNKNOWNS!-COUNT!-LIST;
      IF W>1 OR (W=1 AND ONE!-PREDICTION!-FAILED) THEN <<
        TEST!-PREDICTION:=NIL;
        GOTO TEMPLOOP >>;
      IF W=1 OR ONE!-PREDICTION!-FAILED THEN <<
        W:=IF ONE!-PREDICTION!-FAILED THEN ONE!-PREDICTION!-FAILED
           ELSE CDAR UNKNOWNS!-COUNT!-LIST;
        PUTV(PREDICTION!-RESULTS,W,POLY!-REMAINING) >>;
      FOR I:=1:NUMBER!-OF!-FACTORS DO
        PUTV(BEST!-FACTORS,I,GETV(PREDICTION!-RESULTS,I));
      IF NOT ONE!-PREDICTION!-FAILED THEN
        PREDICTIONS:=
        (CAR V .
          LIST(SOLN!-MATRICES,PREDICTED!-FORMS,MAX!-UNKNOWNS,
            NUMBER!-OF!-UNKNOWNS))
        . PREDICTIONS >>;
EXIT:
    FACTOR!-TRACE <<
      IF NOT BAD!-CASE THEN
        IF FIRST!-TIME THEN
          PRINTSTR "Therefore these factors are already correct."
        ELSE <<
          PRINTSTR "Correct factors are:";
          PRINTVEC("  f(",NUMBER!-OF!-FACTORS,") = ",BEST!-FACTORS)
        >>;
      TERPRI!*(NIL);
      PRINTSTR "******************************************************";
      TERPRI!*(NIL) >>
  END) (FACTOR!-LEVEL+1);


SYMBOLIC PROCEDURE SOLVE!-FOR!-CORRECTIONS(C,FHATVEC,FVEC,RESVEC,VSET);
% ....;
  IF NULL VSET THEN
    FOR I:=1:NUMBER!-OF!-FACTORS DO
      PUTV(RESVEC,I,
        REMAINDER!-MOD!-P(
          TIMES!-MOD!-P(C,GETV(ALPHAVEC,I)),
          GETV(FVEC,I)))
  ELSE (LAMBDA FACTOR!-LEVEL; BEGIN
    SCALAR RESIDUE,GROWTH!-FACTOR,F0S,FHAT0S,V,F1,
      CORRECTION!-FACTOR,SUBSTRES,K,DEGBD,FIRST!-TIME,REDC,D,
      PREDICTED!-FORMS,MAX!-UNKNOWNS,SOLVE!-COUNT,NUMBER!-OF!-UNKNOWNS,
      CORRECTION!-VECTORS,SOLN!-MATRICES,W,PREVIOUS!-PREDICTION!-HOLDS,
      UNKNOWNS!-COUNT!-LIST,TEST!-PREDICTION,POLY!-REMAINING,
      PREDICTION!-RESULTS,ONE!-PREDICTION!-FAILED,KK;
    V:=CAR VSET;
    DEGBD:=GET!-DEGREE!-BOUND CAR V;
    FIRST!-TIME:=T;
    GROWTH!-FACTOR:=MAKE!-GROWTH!-FACTOR V;
    POLY!-REMAINING:=C;
    PREDICTION!-RESULTS:=MKVECT NUMBER!-OF!-FACTORS;
    REDC:=EVALUATE!-MOD!-P(C,CAR V,CDR V);
    FACTOR!-TRACE <<
      PRINTSTR "Want a(i) s.t.";
      PRIN2!* "(*)  sum over i [ a(i)*fhat(i) ] = ";
      PRINSF C;
      PRIN2!* " mod ";
      PRINTSTR HENSEL!-GROWTH!-SIZE;
      PRIN2!* "    where fhat(i) = product over j [ f(j) ]";
      PRIN2!* " / f(i) mod ";
      PRINTSTR HENSEL!-GROWTH!-SIZE;
      PRINTSTR "    and";
      PRINTVEC("      f(",NUMBER!-OF!-FACTORS,") = ",FVEC);
      TERPRI!*(NIL);
      PRIN2!*
	 "First solve the problem in one less variable by putting ";
      PRINVAR CAR V; PRIN2!* '!=; PRINTSTR CDR V;
      TERPRI!*(NIL)
    >>;
    SOLVE!-FOR!-CORRECTIONS(REDC,
      FHAT0S:=REDUCE!-VEC!-BY!-ONE!-VAR!-MOD!-P(
        FHATVEC,V,NUMBER!-OF!-FACTORS),
      F0S:=REDUCE!-VEC!-BY!-ONE!-VAR!-MOD!-P(
        FVEC,V,NUMBER!-OF!-FACTORS),
      RESVEC,
      CDR VSET); % Results left in RESVEC;
    IF BAD!-CASE THEN RETURN;
    FACTOR!-TRACE <<
      PRINTSTR "Giving:";
      PRINTVEC("  a(",NUMBER!-OF!-FACTORS,",0) = ",RESVEC);
      PRINTSTR "Subtracting the contributions these give in (*) from";
      PRIN2!* "the R.H.S. of (*) ";
      PRIN2!* "and differentiating wrt "; PRINVAR CAR V;
      PRINTSTR " gives a residue:"
    >>;
    RESIDUE:=DIFF!-OVER!-K!-MOD!-P(DIFFERENCE!-MOD!-P(C,
          FORM!-SUM!-AND!-PRODUCT!-MOD!-P(RESVEC,FHATVEC,
            NUMBER!-OF!-FACTORS)),1,CAR V);
    FACTOR!-TRACE <<
      FAC!-PRINTSF RESIDUE;
      PRIN2!* " Now we shall put in the powers of ";
      PRINSF GROWTH!-FACTOR;
      PRINTSTR " to find the a's fully."
    >>;
    IF NOT POLYZEROP RESIDUE AND NOT ZEROP CDR V THEN <<
      W:=ATSOC(CAR V,PREDICTIONS);
      IF W THEN <<
        PREVIOUS!-PREDICTION!-HOLDS:=T;
        FACTOR!-TRACE <<
	  PRINTSTR
	     "We shall use the previous prediction for the form of";
          PRIN2!* "polynomials wrt "; PRINTVAR CAR V >>;
        W:=CDR W;
        SOLN!-MATRICES:=CAR W;
        PREDICTED!-FORMS:=CADR W;
        MAX!-UNKNOWNS:=CADDR W;
        NUMBER!-OF!-UNKNOWNS:=CADR CDDR W >>
      ELSE <<
        FACTOR!-TRACE <<
     PRINTSTR
	"We shall use a new prediction for the form of polynomials ";
        PRIN2!* "wrt "; PRINTVAR CAR V >>;
        PREDICTED!-FORMS:=MKVECT NUMBER!-OF!-FACTORS;
        FOR I:=1:NUMBER!-OF!-FACTORS DO
          PUTV(PREDICTED!-FORMS,I,GETV(FVEC,I));
            % make a copy of the factors in a vector that we shall
            % overwrite;
        MAKE!-PREDICTED!-FORMS(PREDICTED!-FORMS,CAR V);
            % sets max!-unknowns and number!-of!-unknowns;
        >>;
      FACTOR!-TRACE <<
        TERPRI!*(NIL);
        PRINTSTR "We predict :";
        FOR EACH W IN NUMBER!-OF!-UNKNOWNS DO <<
          PRIN2!* CAR W;
          PRIN2!* " terms in a("; PRIN2!* CDR W; PRINTSTR '!) >>;
        IF (CAAR NUMBER!-OF!-UNKNOWNS)=1 THEN <<
          PRIN2!* "Since we predict only one term for a(";
          PRIN2!* CDAR NUMBER!-OF!-UNKNOWNS;
          PRINTSTR "), we can test it right away:" >>
        ELSE <<
          PRIN2!* "So we shall do at least ";
          PRIN2!* ISUB1 CAAR NUMBER!-OF!-UNKNOWNS;
          PRIN2!* " Hensel step";
          IF (CAAR NUMBER!-OF!-UNKNOWNS)=2 THEN PRINTSTR "."
          ELSE PRINTSTR "s." >>;
        TERPRI!*(NIL) >>;
      UNKNOWNS!-COUNT!-LIST:=NUMBER!-OF!-UNKNOWNS;
      WHILE UNKNOWNS!-COUNT!-LIST AND
         (CAR (W:=CAR UNKNOWNS!-COUNT!-LIST))=1 DO
        BEGIN SCALAR I,R,WR,FI;
          UNKNOWNS!-COUNT!-LIST:=CDR UNKNOWNS!-COUNT!-LIST;
          I:=CDR W;
          W:=QUOTIENT!-MOD!-P(
            WR:=DIFFERENCE!-MOD!-P(POLY!-REMAINING,
              TIMES!-MOD!-P(R:=GETV(RESVEC,I),GETV(FHATVEC,I))),
            FI:=GETV(FVEC,I));
          IF DIDNTGO W OR NOT POLYZEROP
            DIFFERENCE!-MOD!-P(WR,TIMES!-MOD!-P(W,FI)) THEN
            IF ONE!-PREDICTION!-FAILED THEN <<
              FACTOR!-TRACE PRINTSTR "Predictions are no good.";
              MAX!-UNKNOWNS:=NIL >>
            ELSE <<
              FACTOR!-TRACE <<
                PRIN2!* "Guess for a(";
                PRIN2!* I;
                PRINTSTR ") was bad." >>;
              ONE!-PREDICTION!-FAILED:=I >>
          ELSE <<
            PUTV(PREDICTION!-RESULTS,I,R);
            FACTOR!-TRACE <<
	      PRIN2!* "Prediction for a("; PRIN2!* I;
	      PRIN2!* ") worked: ";
	      FAC!-PRINTSF R >>;
            POLY!-REMAINING:=WR >>
        END;
      W:=LENGTH UNKNOWNS!-COUNT!-LIST;
      IF W=1 AND NOT ONE!-PREDICTION!-FAILED THEN <<
        PUTV(RESVEC,CDAR UNKNOWNS!-COUNT!-LIST,
          QUOTFAIL!-MOD!-P(POLY!-REMAINING,GETV(FHATVEC,
            CDAR UNKNOWNS!-COUNT!-LIST)));
        GOTO EXIT >>
      ELSE IF W=0 AND ONE!-PREDICTION!-FAILED THEN <<
        PUTV(RESVEC,ONE!-PREDICTION!-FAILED,
          QUOTFAIL!-MOD!-P(POLY!-REMAINING,GETV(FHATVEC,
            ONE!-PREDICTION!-FAILED)));
        GOTO EXIT >>;
      SOLVE!-COUNT:=1;
      IF MAX!-UNKNOWNS THEN
        CORRECTION!-VECTORS:=MAKE!-CORRECTION!-VECTORS(PREDICTED!-FORMS,
          RESVEC,MAX!-UNKNOWNS) >>;
    F1:=MKVECT NUMBER!-OF!-FACTORS;
    K:=1;
    KK:=0;
    CORRECTION!-FACTOR:=GROWTH!-FACTOR;
    IF NOT POLYZEROP RESIDUE THEN FIRST!-TIME:=NIL;
TEMPLOOP:
    WHILE NOT POLYZEROP RESIDUE AND (NULL MAX!-UNKNOWNS
                      OR NULL TEST!-PREDICTION) DO
      IF K>DEGBD THEN RETURN <<
        FACTOR!-TRACE <<
          PRIN2!* "We have overshot the degree bound for ";
          PRINTVAR CAR V >>;
        IF !*OVERSHOOT THEN
          PRINTC "Multivariate degree bound overshoot -> restart";
        BAD!-CASE:=T >>
      ELSE
	IF POLYZEROP(SUBSTRES:=EVALUATE!-MOD!-P(RESIDUE,CAR V,CDR V))
	 THEN <<
          K:=IADD1 K;
          RESIDUE:=DIFF!-OVER!-K!-MOD!-P(RESIDUE,K,CAR V);
          CORRECTION!-FACTOR:=
            TIMES!-MOD!-P(CORRECTION!-FACTOR,GROWTH!-FACTOR) >>
      ELSE <<
        FACTOR!-TRACE <<
          PRIN2!* "Hensel Step "; PRINTSTR (KK:=KK #+ 1);
          PRIN2!* "-------------";
          IF KK>10 THEN PRINTSTR "-" ELSE TERPRI!*(T);
          PRIN2!* "Next corrections are for (";
          PRINSF GROWTH!-FACTOR;
          IF NOT (K=1) THEN <<
            PRIN2!* ") ** ";
            PRIN2!* K >> ELSE PRIN2!* '!);
          PRINTSTR ". To find these we solve:";
          PRIN2!* "     sum over i [ a(i,1)*fhat(i,0) ] = ";
          PRINSF SUBSTRES;
          PRIN2!* " mod ";
          PRIN2!* HENSEL!-GROWTH!-SIZE;
          PRINTSTR " for a(i,1). ";
          TERPRI!*(NIL)
        >>;
        SOLVE!-FOR!-CORRECTIONS(SUBSTRES,FHAT0S,F0S,F1,CDR VSET);
            % answers in f1;
        IF BAD!-CASE THEN RETURN;
        IF MAX!-UNKNOWNS THEN <<
          SOLVE!-COUNT:=IADD1 SOLVE!-COUNT;
          FOR I:=1:NUMBER!-OF!-FACTORS DO
            PUTV(GETV(CORRECTION!-VECTORS,I),SOLVE!-COUNT,GETV(F1,I));
          IF SOLVE!-COUNT=CAAR UNKNOWNS!-COUNT!-LIST THEN
            TEST!-PREDICTION:=T >>;
        FOR I:=1:NUMBER!-OF!-FACTORS DO
          PUTV(RESVEC,I,PLUS!-MOD!-P(GETV(RESVEC,I),TIMES!-MOD!-P(
            GETV(F1,I),CORRECTION!-FACTOR)));
        FACTOR!-TRACE <<
          PRINTSTR "   Giving:";
          PRINTVEC("     a(",NUMBER!-OF!-FACTORS,",1) = ",F1);
          PRINTSTR "   New a's are now:";
          PRINTVEC("     a(",NUMBER!-OF!-FACTORS,") = ",RESVEC)
        >>;
         D:=TIMES!-MOD!-P(CORRECTION!-FACTOR,
              FORM!-SUM!-AND!-PRODUCT!-MOD!-P(F1,FHATVEC,
                NUMBER!-OF!-FACTORS));
        IF DEGREE!-IN!-VARIABLE(D,CAR V)>DEGBD THEN RETURN <<
          FACTOR!-TRACE <<
            PRIN2!* "We have overshot the degree bound for ";
            PRINTVAR CAR V >>;
          IF !*OVERSHOOT THEN
            PRINTC "Multivariate degree bound overshoot -> restart";
          BAD!-CASE:=T >>;
        D:=DIFF!-K!-TIMES!-MOD!-P(D,K,CAR V);
        K:=IADD1 K;
        RESIDUE:=DIFF!-OVER!-K!-MOD!-P(
             DIFFERENCE!-MOD!-P(RESIDUE,D),K,CAR V);
        FACTOR!-TRACE <<
          PRIN2!* "   and residue = ";
	  FAC!-PRINTSF RESIDUE;
          PRINTSTR "-------------"
        >>;
        CORRECTION!-FACTOR:=
          TIMES!-MOD!-P(CORRECTION!-FACTOR,GROWTH!-FACTOR) >>;
    IF NOT POLYZEROP RESIDUE AND NOT BAD!-CASE THEN <<
      IF NULL SOLN!-MATRICES THEN
	SOLN!-MATRICES:=
	   CONSTRUCT!-SOLN!-MATRICES(PREDICTED!-FORMS,CDR V);
      FACTOR!-TRACE <<
        PRINTSTR "The Hensel growth so far allows us to test some of";
        PRINTSTR "our predictions:" >>;
      WHILE UNKNOWNS!-COUNT!-LIST AND
        (CAR (W:=CAR UNKNOWNS!-COUNT!-LIST))=SOLVE!-COUNT DO <<
        UNKNOWNS!-COUNT!-LIST:=CDR UNKNOWNS!-COUNT!-LIST;
        FACTOR!-TRACE
          PRINT!-LINEAR!-SYSTEM(CDR W,SOLN!-MATRICES,
            CORRECTION!-VECTORS,PREDICTED!-FORMS,CAR V);
        W:=TRY!-PREDICTION(SOLN!-MATRICES,CORRECTION!-VECTORS,
          PREDICTED!-FORMS,CAR W,CDR W,POLY!-REMAINING,CAR V,FVEC,
          FHATVEC,PREVIOUS!-PREDICTION!-HOLDS);
        IF CAR W='SINGULAR OR CAR W='BAD!-PREDICTION THEN
          IF ONE!-PREDICTION!-FAILED THEN <<
            FACTOR!-TRACE PRINTSTR "Predictions were no help.";
            RETURN MAX!-UNKNOWNS:=NIL >>
          ELSE <<
            IF PREVIOUS!-PREDICTION!-HOLDS THEN <<
              PREDICTIONS:=DELASC(CAR V,PREDICTIONS);
              PREVIOUS!-PREDICTION!-HOLDS:=NIL >>;
            ONE!-PREDICTION!-FAILED:=CDR W >>
        ELSE <<
          PUTV(PREDICTION!-RESULTS,CAR W,CADR W);
          POLY!-REMAINING:=CADDR W >> >>;
      IF NULL MAX!-UNKNOWNS THEN <<
        IF PREVIOUS!-PREDICTION!-HOLDS THEN
          PREDICTIONS:=DELASC(CAR V,PREDICTIONS);
        GOTO TEMPLOOP >>;
      W:=LENGTH UNKNOWNS!-COUNT!-LIST;
      IF W>1 OR (W=1 AND ONE!-PREDICTION!-FAILED) THEN <<
        TEST!-PREDICTION:=NIL;
        GOTO TEMPLOOP >>;
      IF W=1 OR ONE!-PREDICTION!-FAILED THEN <<
        W:=IF ONE!-PREDICTION!-FAILED THEN ONE!-PREDICTION!-FAILED
           ELSE CDAR UNKNOWNS!-COUNT!-LIST;
        PUTV(PREDICTION!-RESULTS,W,QUOTFAIL!-MOD!-P(
          POLY!-REMAINING,GETV(FHATVEC,W))) >>;
      FOR I:=1:NUMBER!-OF!-FACTORS DO
          PUTV(RESVEC,I,GETV(PREDICTION!-RESULTS,I));
      IF NOT PREVIOUS!-PREDICTION!-HOLDS
         AND NOT ONE!-PREDICTION!-FAILED THEN
        PREDICTIONS:=
          (CAR V .
            LIST(SOLN!-MATRICES,PREDICTED!-FORMS,MAX!-UNKNOWNS,
              NUMBER!-OF!-UNKNOWNS))
          . PREDICTIONS >>;
EXIT:
    FACTOR!-TRACE <<
      IF NOT BAD!-CASE THEN
        IF FIRST!-TIME THEN
          PRINTSTR "But these a's are already correct."
        ELSE <<
          PRINTSTR "Correct a's are:";
          PRINTVEC("  a(",NUMBER!-OF!-FACTORS,") = ",RESVEC)
        >>;
      TERPRI!*(NIL);
      PRINTSTR "**************************************************";
      TERPRI!*(NIL) >>
  END) (FACTOR!-LEVEL+1);



ENDMODULE;


MODULE NATURAL;


% part of resultant program;

SYMBOLIC PROCEDURE NATURAL!-PRS!-ALGORITHM(A,B,X);
% A,B are univariate polynomials mod p. The procedure calculates;
% the natural prs and hence res(A,B) mod p.;
% one poly may be a number;
IF NOT (UNIVARIATEP A AND UNIVARIATEP B)
THEN ERRORF "NON UNIVARIATE POLYS INPUT TO NATURAL PRS ALG"
ELSE BEGIN
     INTEGER V, TEMPANS, ANS, LOOP;
     SCALAR T1, T2, T3;
     IF NOT X = CAR UNION(VARIABLES!-IN!-FORM A, VARIABLES!-IN!-FORM B)
     THEN ERRORF "WRONG VARIABLE INPUT TO NATURAL";
     LOOP := 0; % loop is used as a pseudo-boolean;
     V := 0;
     TEMPANS := 1;
     T3 := REMAINDER!-MOD!-P(A,B);
     IF (T3 = A)
     THEN <<
          T1 := B;
          T2 := A;
          T3 := REMAINDER!-MOD!-P(T1,T2)
          >>
     ELSE <<
          T1 := A;
          T2 := B
          >>;
     WHILE (LOOP = 0)
     DO <<
        TEMPANS := MODULAR!-TIMES(TEMPANS,
                     MODULAR!-EXPT(LC T2,
                       LDEG T1 - LEADING!-DEGREE T3));
        V := LOGXOR(V,LOGAND(LDEG T1,LDEG T2,1));
        IF (LEADING!-DEGREE T3 = 0) THEN LOOP := 1
        ELSE BEGIN
             T1 := T2;
             T2 := T3;
             T3 := REMAINDER!-MOD!-P(T1,T2);
             IF NOT (LEADING!-DEGREE T3 < LDEG T2)
             THEN ERRORF "PRS DOES NOT CONVERGE"
             END
        >>;
     ANS := MODULAR!-TIMES(TEMPANS,
              MODULAR!-EXPT(!*D2N T3,LDEG T2));
     RETURN IF V=0 THEN ANS ELSE MODULAR!-MINUS ANS
 END;

ENDMODULE;


MODULE PFACTOR;

% *******************************************************************
%
%   Copyright (C)  University of Cambridge, England 1979
%
% *******************************************************************;





% factorization of polynomials modulo p
%
% a. c. norman.  1978.
%
%
%**********************************************************************;





SYMBOLIC PROCEDURE SIMPPFACTORIZE U;
% q is a prefix form. convert to standard quotient, factorize,
% return the factors in the array w. do all work mod p;
  BEGIN
    SCALAR Q,W,P,FF,NN,GCDSAV,BASE!-TIME,LAST!-DISPLAYED!-TIME,
        GC!-BASE!-TIME,LAST!-DISPLAYED!-GC!-TIME,
        USER!-PRIME,CURRENT!-MODULUS,MODULUS!/2;
    IF ATOM U OR ATOM CDR U OR ATOM CDDR U THEN
       REDERR "PFACTORIZE requires 3 arguments";
    Q := CAR U;
    W := CADR U;
    P := CADDR U;
    SET!-TIME();
    GCDSAV := !*GCD;
    !*GCD:=T;
       %gcd explicitly enabled during the following call to simp!*;
    Q:= SIMP!* Q; %convert to standard quotient;
    NN := !*Q2F Q; %must be a polynomial;
    P:=SIMP!* P; %should be a number;
    IF NOT (DENR P=1) THEN REDERR "P HAS A DENOMINATOR IN PFACTOR";
    P:=NUMR P;
    IF NOT NUMBERP P THEN REDERR "P NOT A NUMBER IN PFACTOR";
    IF NOT PRIMEP P THEN REDERR "P NOT PRIME IN PFACTOR";
    USER!-PRIME:=P;
    SET!-MODULUS P;
    !*GCD:=GCDSAV;
    IF DOMAINP NN OR (REDUCE!-MOD!-P LC NN=NIL) THEN
       PRINTC "*** DEGENERATE CASE IN PFACTOR";
    IF NOT (LENGTH VARIABLES!-IN!-FORM NN=1) THEN
       REDERR "MULTIVARIATE INPUT TO PFACTOR";
    NN:=MONIC!-MOD!-P REDUCE!-MOD!-P NN;
    PRINT!-TIME "About to call FACTOR-FORM-MOD-P";
    NN:=ERRORSET('(FACTOR!-FORM!-MOD!-P NN),T,T);
    PRINT!-TIME "FACTOR-FORM-MOD-P returned";
    IF ERRORP NN THEN GO TO FAILED;
    NN:=CAR NN;
    FF:=0; %factor count;
    P:=LIST (0 . 1);
    FOR EACH FFF IN NN DO
        FOR I:=1:CDR FFF DO P:=
          ((FF:=FF+1) . MK!*SQ(CAR FFF ./ 1)) . P;
    RETURN MULTIPLE!-RESULT(P,W);
FAILED:
    PRINTC "****** FACTORIZATION FAILED******";
    RETURN MULTIPLE!-RESULT(LIST(1 . MK!*SQ Q),W)
  END;

PUT('PFACTORIZE,'SIMPFN,'SIMPPFACTORIZE);


SYMBOLIC PROCEDURE FACTOR!-FORM!-MOD!-P P;
% input:
% p is a reduce standard form that is to be factorized
% mod prime;
% result:
% ((p1 . x1) (p2 . x2) .. (pn . xn))
% where p<i> are standard forms and x<i> are integers,
% and p= product<i> p<i>**x<i>;
    SORT!-FACTORS FACTORIZE!-BY!-SQUARE!-FREE!-MOD!-P P;


SYMBOLIC PROCEDURE FACTORIZE!-BY!-SQUARE!-FREE!-MOD!-P P;
    IF P=1 THEN NIL
    ELSE IF DOMAINP P THEN (P . 1) . NIL
    ELSE
     BEGIN
      SCALAR DP,V;
      V:=(MKSP(MVAR P,1).* 1) .+ NIL;
      DP:=0;
      WHILE EVALUATE!-MOD!-P(P,MVAR V,0)=0 DO <<
        P:=QUOTFAIL!-MOD!-P(P,V);
        DP:=DP+1 >>;
      IF DP>0 THEN RETURN ((V . DP) .
        FACTORIZE!-BY!-SQUARE!-FREE!-MOD!-P P);
      DP:=DERIVATIVE!-MOD!-P P;
      IF DP=NIL THEN <<
%here p is a something to the power current!-modulus;
        P:=DIVIDE!-EXPONENTS!-BY!-P(P,CURRENT!-MODULUS);
        P:=FACTORIZE!-BY!-SQUARE!-FREE!-MOD!-P P;
        RETURN MULTIPLY!-MULTIPLICITIES(P,CURRENT!-MODULUS) >>;
      DP:=GCD!-MOD!-P(P,DP);
      IF DP=1 THEN RETURN FACTORIZE!-PP!-MOD!-P P;
%now p is not square-free;
      P:=QUOTFAIL!-MOD!-P(P,DP);
%factorize p and dp separately;
      P:=FACTORIZE!-PP!-MOD!-P P;
      DP:=FACTORIZE!-BY!-SQUARE!-FREE!-MOD!-P DP;
% i feel that this scheme is slightly clumsy, but
% square-free decomposition mod p is not as straightforward
% as square free decomposition over the integers, and pfactor
% is probably not going to be slowed down too badly by
% this;
      RETURN MERGEFACTORS(P,DP)
   END;




%**********************************************************************;
% code to factorize primitive square-free polynomials mod p;



SYMBOLIC PROCEDURE DIVIDE!-EXPONENTS!-BY!-P(P,N);
    IF ISDOMAIN P THEN P
    ELSE (MKSP(MVAR P,EXACTQUOTIENT(LDEG P,N)) .* LC P) .+
       DIVIDE!-EXPONENTS!-BY!-P(RED P,N);

SYMBOLIC PROCEDURE EXACTQUOTIENT(A,B);
  BEGIN
    SCALAR W;
    W:=DIVIDE(A,B);
    IF CDR W=0 THEN RETURN CAR W;
    ERROR("INEXACT DIVISION",LIST(A,B,W))
  END;


SYMBOLIC PROCEDURE MULTIPLY!-MULTIPLICITIES(L,N);
    IF NULL L THEN NIL
    ELSE (CAAR L . (N*CDAR L)) .
        MULTIPLY!-MULTIPLICITIES(CDR L,N);


SYMBOLIC PROCEDURE MERGEFACTORS(A,B);
% a and b are lists of factors (with multiplicities),
% merge them so that no factor occurs more than once in
% the result;
    IF NULL A THEN B
    ELSE MERGEFACTORS(CDR A,ADDFACTOR(CAR A,B));

SYMBOLIC PROCEDURE ADDFACTOR(A,B);
%add factor a into list b;
    IF NULL B THEN LIST A
    ELSE IF CAR A=CAAR B THEN
      (CAR A . (CDR A + CDAR B)) . CDR B
    ELSE CAR B . ADDFACTOR(A,CDR B);

SYMBOLIC PROCEDURE FACTORIZE!-PP!-MOD!-P P;
%input a primitive square-free polynomial p,
% output a list of irreducible factors of p;
  BEGIN
    SCALAR VARS;
    IF P=1 THEN RETURN NIL
    ELSE IF ISDOMAIN P THEN RETURN (P . 1) . NIL;
% now I am certain that p is not degenerate;
    PRINT!-TIME "primitive square-free case detected";
    VARS:=VARIABLES!-IN!-FORM P;
    IF LENGTH VARS=1 THEN RETURN UNIFAC!-MOD!-P P;
    ERRORF "SHAMBLED IN PFACTOR - MULTIVARIATE CASE RESURFACED"
  END;

SYMBOLIC PROCEDURE UNIFAC!-MOD!-P P;
%input p a primitive square-free univariate polynomial
%output a list of the factors of p over z mod p;
  BEGIN
    SCALAR MODULAR!-INFO,M!-IMAGE!-VARIABLE;
    IF ISDOMAIN P THEN RETURN NIL
    ELSE IF LDEG P=1 THEN RETURN (P . 1) . NIL;
    MODULAR!-INFO:=MKVECT 1;
    M!-IMAGE!-VARIABLE:=MVAR P;
    GET!-FACTOR!-COUNT!-MOD!-P(1,P,USER!-PRIME,NIL);
    PRINT!-TIME "Factor counts obtained";
    GET!-FACTORS!-MOD!-P(1,USER!-PRIME);
    PRINT!-TIME "Actual factors extracted";
    RETURN FOR EACH Z IN GETV(MODULAR!-INFO,1) COLLECT (Z . 1)
  END;

ENDMODULE;


MODULE PRES;

% part of resultant program;

SYMBOLIC PROCEDURE RESULTANTF(A,B,X);
% returns resultant of A,B wrt X;
  BEGIN
    SCALAR C, NEW!-A, NEW!-B, NEW!-C, PRIMES!-USED, LOOP!-COUNT,
	   ORDER!-CHANGE;
    INTEGER M, N, D, E, Q, F, OLD!-MODULUS, NEW!-PRIME;
    IF (NULL A OR NULL B)
      THEN ERRORF "NIL POLYNOMIAL PASSED TO RESULTANTF";
    IF NOT (MEMBER(X,VARIABLES!-IN!-FORM A)
	  AND MEMBER(X,VARIABLES!-IN!-FORM B))
	THEN ERRORF
		"X MUST OCCUR IN BOTH POLYNOMIALS INPUT TO RESULTANTF";
    % X must be in both polynomials if it is to be eliminated
    % between them;

    ORDER!-CHANGE := NIL;
       % pseudo-boolean, indicates whether the order of
       % the variables has been changed;
    % check X is the main variable of A and B, if not make it so;
    IF NOT ((X=MVAR A) AND (X=MVAR B))
    THEN BEGIN
     SCALAR V;
     V := SETKORDER APPEND(CONS(X,NIL),
			   DELETE(X,UNION(VARIABLES!-IN!-FORM A,
					  VARIABLES!-IN!-FORM B)));
     A := REORDER A;
     B := REORDER B;
     ORDER!-CHANGE := LIST V
     END;

    % initialise variables ;

    OLD!-MODULUS := SET!-MODULUS NIL;
    M := LDEG A;
    N := LDEG B;
    D := MAX!-NORM!-COEFFS(A,X);
    E := MAX!-NORM!-COEFFS(B,X);
    Q := 1;
    C := 0;
    PRIMES!-USED := NIL; % list of primes used - dont want repetitions;
    NEW!-A := 0;
    NEW!-B := 0;
    F := 2 * FACTORIAL(M+N) * D**N * E**M;
    % F/2 is the limit of the coefficients of the resultant of A,B ;

    % main loop starts here;
    WHILE NOT (Q > F)
    DO BEGIN
       LOOP!-COUNT := T; % used as a pseudo-boolean;
       WHILE ((DEGREE!-IN!-VARIABLE(NEW!-A,X) < M)
          OR  (DEGREE!-IN!-VARIABLE(NEW!-B,X) < N)
          OR  LOOP!-COUNT )
       DO BEGIN
          LOOP!-COUNT := NIL;
          % set up prime modulus before calling cpres ;
          NEW!-PRIME := RANDOM!-PRIME();
          WHILE MEMBER(NEW!-PRIME,PRIMES!-USED) DO
                NEW!-PRIME := RANDOM!-PRIME();
          PRIMES!-USED := NEW!-PRIME . PRIMES!-USED;
          SET!-MODULUS NEW!-PRIME;
          NEW!-A := REDUCE!-MOD!-P A;
          NEW!-B := REDUCE!-MOD!-P B
          END;
       NEW!-C := CPRES(NEW!-A,NEW!-B,X);
       C := CHINESE!-REMAINDER(C,NEW!-C,Q,NEW!-PRIME);
       Q := Q * NEW!-PRIME;
       IF 2* GET!-HEIGHT C > F THEN ERRORF "COEFFICIENT BOUND EXCEEDED"
       END;
    IF ORDER!-CHANGE
    THEN BEGIN
         SETKORDER CAR ORDER!-CHANGE;
         C := REORDER C
         END;
    SET!-MODULUS OLD!-MODULUS; %return to original state before exiting;
    RETURN C
  END;


SYMBOLIC PROCEDURE MAX!-NORM!-COEFFS(A,VAR);
% var must be the main variable of A;
  IF ISDOMAIN A THEN ABS !*D2N A
  ELSE IF NOT MVAR A = VAR THEN SUM!-OF!-NORMS A
  ELSE MAX(SUM!-OF!-NORMS LC A,MAX!-NORM!-COEFFS(RED A,VAR));


SYMBOLIC PROCEDURE SUM!-OF!-NORMS A;
  IF ISDOMAIN A THEN ABS !*D2N A
  ELSE PLUS(SUM!-OF!-NORMS LC A,SUM!-OF!-NORMS RED A);


SYMBOLIC PROCEDURE CHINESE!-REMAINDER(POLY!-B,POLY!-A,Q,P);
% poly!-b is a poly with !coeffs! < Q/2                             ;
% poly!-a is a poly mod p                                           ;
% returns a poly with !coeffs! < PQ/2                               ;
  IF ISDOMAIN POLY!-A
  THEN IF ISDOMAIN POLY!-B
       THEN GARNERS!-ALG(!*D2N POLY!-B,!*D2N POLY!-A,Q,P)
       ELSE ADJOIN!-TERM(LPOW POLY!-B,
                         CHINESE!-REMAINDER(LC POLY!-B,0,Q,P),
                         CHINESE!-REMAINDER(RED POLY!-B,POLY!-A,Q,P))
  ELSE IF ISDOMAIN POLY!-B
  THEN ADJOIN!-TERM(LPOW POLY!-A,
                    CHINESE!-REMAINDER(0,LC POLY!-A,Q,P),
                    CHINESE!-REMAINDER(POLY!-B,RED POLY!-A,Q,P))
  ELSE IF LPOW POLY!-A = LPOW POLY!-B
  THEN ADJOIN!-TERM(LPOW POLY!-A,
                    CHINESE!-REMAINDER(LC POLY!-B,LC POLY!-A,Q,P),
                    CHINESE!-REMAINDER(RED POLY!-B,RED POLY!-A,Q,P))
  ELSE IF COMES!-BEFORE(LPOW POLY!-A,LPOW POLY!-B)
  THEN ADJOIN!-TERM(LPOW POLY!-A,
                    CHINESE!-REMAINDER(0,LC POLY!-A,Q,P),
                    CHINESE!-REMAINDER(POLY!-B,RED POLY!-A,Q,P))
  ELSE ADJOIN!-TERM(LPOW POLY!-B,
                    CHINESE!-REMAINDER(LC POLY!-B,0,Q,P),
                    CHINESE!-REMAINDER(RED POLY!-B,POLY!-A,Q,P));


SYMBOLIC PROCEDURE GARNERS!-ALG(B,A,Q,P);
% inputs !B! < Q/2, A mod P                                    ;
% returns unique integer c such that c = B mod Q and c = A modP;
% and !c! < PQ/2                                               ;
  BEGIN
    INTEGER L;
    L := MODULAR!-QUOTIENT(MODULAR!-DIFFERENCE(A,MODULAR!-NUMBER B),
                          MODULAR!-NUMBER Q);
    IF L*2 > P THEN L := DIFFERENCE(L,P);
    % PRINTC "L IS";
    % SUPERPRINT L;
    RETURN !*NUM2F PLUS(B,TIMES(L,Q))
  END;




SYMBOLIC PROCEDURE LEADING!-DEGREE A;
% returns 0 if a is numeric, ldeg a otherwise;
  IF ISDOMAIN A THEN 0
  ELSE LDEG A;


SYMBOLIC PROCEDURE FACTORIAL N;
  IF NOT ISDOMAIN N THEN ERRORF "NUMBER EXPECTED IN FACTORIAL"
  ELSE IF N < 0 THEN ERRORF "NEGATIVE NUMBER GIVEN TO FACTORIAL"
  ELSE IF N = 0 THEN 1
  ELSE N * FACTORIAL(N-1);


ENDMODULE;


MODULE RSLTNT;

% (C) Copyright 1979, University of Cambridge;

% RESULTANT CALCULATION;





SYMBOLIC PROCEDURE SIMPRESULTANT U;
% COMPUTE THE RESULTANT OF A AND B WITH RESPECT TO
% THE VARIABLE 'VAR';
  BEGIN
    SCALAR A,B,VAR;
    IF ATOM U OR ATOM CDR U OR ATOM CDDR U THEN
       REDERR "RESULTANT requires 3 arguments";
    A:= !*Q2F SIMP!* CAR U;  %must be polynomials;
    B:= !*Q2F SIMP!* CADR U;
    VAR:= !*Q2K SIMP!* CADDR U;
%   PRINTC "LISP DATASTRUCTURES THAT ARE ARGS FOR RESULTANT";
%   SUPERPRINT A;
%   SUPERPRINT B;
%   SUPERPRINT VAR;
    A := RESULTANTF(A,B,VAR);
    RETURN (A ./ 1);
  END;

PUT('RESULTANT,'SIMPFN,'SIMPRESULTANT);


ENDMODULE;


MODULE UNIHENS;


% *******************************************************************
%
%   copyright (c)  university of cambridge, england 1981
%
% *******************************************************************;






% new hensel construction and related code ;
%     - univariate case with quadratic growth;
%
% p. m. a. moore.  1979.
%
%
%**********************************************************************;




SYMBOLIC PROCEDURE UHENSEL!.EXTEND(POLY,BEST!-FLIST,LCLIST,P);
% extend poly=product(factors in best!-flist) mod p
% even if poly is non-monic. return a list (ok. list of factors) if
% factors can be extended to be correct over the integers,
% otherwise return a list (failed <reason> <reason>);
  BEGIN SCALAR W,K,TIMER,OLD!-MODULUS,ALPHAVEC,MODULAR!-FLIST,FACTORVEC,
        MODFVEC,COEFFTBD,FCOUNT,FHATVEC,DELTAM,MOD!-SYMM!-FLIST,
        CURRENT!-FACTOR!-PRODUCT,FACVEC,FACTORS!-DONE,HENSEL!-POLY;
    PRIME!-BASE:=P;
    OLD!-MODULUS:=SET!-MODULUS P;
    TIMER:=READTIME();
    NUMBER!-OF!-FACTORS:=LENGTH BEST!-FLIST;
    W:=EXPT(LC POLY,NUMBER!-OF!-FACTORS -1);
    IF LC POLY < 0 THEN ERRORF LIST("LC SHOULD NOT BE -VE",POLY);
    COEFFTBD:=MAX(110,LC POLY*GET!-COEFFT!-BOUND(POLY,LDEG POLY));
    POLY:=MULTF(POLY,W);
    MODULAR!-FLIST:=FOR EACH FF IN BEST!-FLIST COLLECT
      REDUCE!-MOD!-P FF;
            % modular factors have been multiplied by a constant to
            % fix the l.c.'s, so they may be out of range - this
            % fixes that;
      IF NOT(W=1) THEN FACTOR!-TRACE <<
	PRIN2!* "Altered univariate polynomial: "; FAC!-PRINTSF POLY >>;
          % make sure the leading coefft will not cause trouble
          % in the hensel construction;
    MOD!-SYMM!-FLIST:=FOR EACH FF IN MODULAR!-FLIST COLLECT
      MAKE!-MODULAR!-SYMMETRIC FF;
    IF NOT !*OVERVIEW THEN FACTOR!-TRACE <<
      PRIN2!* "The factors mod "; PRIN2!* P;
      PRINTSTR " to start from are:";
      FCOUNT:=1;
      FOR EACH FF IN MOD!-SYMM!-FLIST DO <<
        PRIN2!* "   f("; PRIN2!* FCOUNT; PRIN2!* ")=";
	FAC!-PRINTSF FF; FCOUNT:=IADD1 FCOUNT >>;
      TERPRI!*(NIL) >>;
    ALPHALIST:=ALPHAS(NUMBER!-OF!-FACTORS,MODULAR!-FLIST,1);
            % 'magic' polynomials associated with the image factors;
    IF NOT !*OVERVIEW THEN FACTOR!-TRACE <<
      PRINTSTR
	 "The following modular polynomials are chosen such that:";
      TERPRI();
      PRIN2!* "   a(1)*h(1) + ... + a(";
      PRIN2!* NUMBER!-OF!-FACTORS;
      PRIN2!* ")*h("; PRIN2!* NUMBER!-OF!-FACTORS;
      PRIN2!* ") = 1 mod "; PRINTSTR P;
      TERPRI();
      PRINTSTR "  where h(i)=(product of all f(j) [see below])/f(i)";
      PRINTSTR "    and degree of a(i) < degree of f(i).";
      FCOUNT:=1;
      FOR EACH A IN MODULAR!-FLIST DO <<
        PRIN2!* "   a("; PRIN2!* FCOUNT; PRIN2!* ")=";
	FAC!-PRINTSF CDR GET!-ALPHA A;
        PRIN2!* "   f("; PRIN2!* FCOUNT; PRIN2!* ")=";
	FAC!-PRINTSF A;
        FCOUNT:=IADD1 FCOUNT >>
    >>;
    K:=0;
    FACTORVEC:=MKVECT NUMBER!-OF!-FACTORS;
    MODFVEC:=MKVECT NUMBER!-OF!-FACTORS;
    ALPHAVEC:=MKVECT NUMBER!-OF!-FACTORS;
    FOR EACH MODSYMMF IN MOD!-SYMM!-FLIST DO
      << PUTV(FACTORVEC,K:=K+1,FORCE!-LC(MODSYMMF,CAR LCLIST));
         LCLIST:=CDR LCLIST
      >>;
    K:=0;
    FOR EACH MODFACTOR IN MODULAR!-FLIST DO
         << PUTV(MODFVEC,K:=K+1,MODFACTOR);
         PUTV(ALPHAVEC,K,CDR GET!-ALPHA MODFACTOR);
         >>;
            % best!-fvec is now a vector of factors of poly correct
            % mod p with true l.c.s forced in ;
    FHATVEC:=MKVECT NUMBER!-OF!-FACTORS;
    W:=HENSEL!-MOD!-P(POLY,MODFVEC,FACTORVEC,COEFFTBD,NIL,P);
    IF CAR W='OVERSHOT THEN
      BEGIN SCALAR OKLIST,BADLIST,M,R,FF,OM,POL;
        M:=CADR W; % the modulus;
        R:=GETV(FACTORVEC,0); % the no: of factors;
        IF R=2 THEN RETURN (IRREDUCIBLE:=T);
        IF FACTORS!-DONE THEN <<
          POLY:=HENSEL!-POLY;
          FOR EACH WW IN FACTORS!-DONE DO
            POLY:=MULTF(POLY,WW) >>;
        POL:=POLY;
        OM:=SET!-MODULUS HENSEL!-GROWTH!-SIZE;
        ALPHALIST:=NIL;
        FOR I:=R STEP -1 UNTIL 1 DO
	  ALPHALIST:=
	     (REDUCE!-MOD!-P GETV(FACTORVEC,I) . GETV(ALPHAVEC,I))
                      . ALPHALIST;
        SET!-MODULUS OM;
            % bring alphalist up to date;
        FOR I:=1:R DO <<
          FF:=GETV(FACTORVEC,I);
          IF NOT DIDNTGO(W:=QUOTF(POL,FF)) THEN
          << OKLIST:=FF . OKLIST; POL:=W>>
          ELSE BADLIST:=(I . FF) . BADLIST >>;
        IF NULL BADLIST THEN W:='OK . OKLIST
        ELSE <<
          IF NOT !*OVERVIEW THEN FACTOR!-TRACE <<
            PRINTSTR "Overshot factors are:";
            FOR EACH F IN BADLIST DO <<
	      PRIN2!* " f("; PRIN2!* CAR F; PRIN2!* ")=";
	      FAC!-PRINTSF CDR F >>
          >>;
          W:=TRY!.COMBINING(BADLIST,POL,M,NIL);
          IF CAR W='ONE! BAD! FACTOR THEN BEGIN SCALAR X;
            W:=APPEND(OKLIST,CDR W);
            X:=1;
            FOR EACH V IN W DO X:=MULTF(X,V);
            W:='OK . (QUOTFAIL(POL,X) . W)
          END
          ELSE W:='OK . APPEND(OKLIST,W) >>;
        IF (NOT !*LINEAR) AND MULTIVARIATE!-INPUT!-POLY THEN <<
          POLY:=1;
          NUMBER!-OF!-FACTORS:=0;
          FOR EACH FACC IN CDR W DO <<
            POLY:=MULTF(POLY,FACC);
            NUMBER!-OF!-FACTORS:=1 #+ NUMBER!-OF!-FACTORS >>;
            % make sure poly is the product of the factors we have,
            % we recalculate it this way because we may have the wrong
            % lc in old value of poly;
	  RESET!-QUADRATIC!-STEP!-FLUIDS(POLY,CDR W,
					 NUMBER!-OF!-FACTORS);
          IF M=DELTAM THEN ERRORF LIST("Coefft bound < prime ?",
              COEFFTBD,M);
          M:=DELTAM*DELTAM;
          WHILE M<LARGEST!-SMALL!-MODULUS DO <<
            QUADRATIC!-STEP(M,NUMBER!-OF!-FACTORS);
            M:=M*DELTAM >>;
          HENSEL!-GROWTH!-SIZE:=DELTAM;
          OM:=SET!-MODULUS HENSEL!-GROWTH!-SIZE;
          ALPHALIST:=NIL;
          FOR I:=NUMBER!-OF!-FACTORS STEP -1 UNTIL 1 DO
            ALPHALIST:=
              (REDUCE!-MOD!-P GETV(FACTORVEC,I) . GETV(ALPHAVEC,I))
                      . ALPHALIST;
          SET!-MODULUS OM >>
      END
    ELSE BEGIN SCALAR R,FACLIST,OM;
      R:=GETV(FACTORVEC,0); % no of factors;
      OM:=SET!-MODULUS HENSEL!-GROWTH!-SIZE;
      ALPHALIST:=NIL;
      FOR I:=R STEP -1 UNTIL 1 DO
        ALPHALIST:=(REDUCE!-MOD!-P GETV(FACTORVEC,I) . GETV(ALPHAVEC,I))
                    . ALPHALIST;
      SET!-MODULUS OM;
            % bring alphalist up to date;
      FOR I:=R STEP -1 UNTIL 1 DO
        FACLIST:=GETV(FACTORVEC,I) . FACLIST;
      W:=CAR W . FACLIST
    END;
    SET!-MODULUS OLD!-MODULUS;
    FACTOR!-TRACE BEGIN SCALAR K;
      K:=0;
      PRINTSTR "Univariate factors, possibly with adjusted leading";
      PRINTSTR "coefficients, are:";
      FOR EACH WW IN CDR W DO <<
        PRIN2!* " f("; PRIN2!* (K:=K #+ 1);
	PRIN2!* ")="; FAC!-PRINTSF WW >>
    END;
    RETURN IF IRREDUCIBLE THEN T ELSE IF NON!-MONIC THEN
        (CAR W . PRIMITIVE!.PARTS(CDR W,M!-IMAGE!-VARIABLE,T))
      ELSE W
  END;

SYMBOLIC PROCEDURE GET!-COEFFT!-BOUND(POLY,DDEG);
% this uses Mignottes bound which is minimal I believe;
% NB. poly had better be univariate as bound only valid for this;
  BINOMIAL!-COEFFT(DDEG/2,DDEG/4) * ROOT!-SQUARES(POLY,0);

SYMBOLIC PROCEDURE BINOMIAL!-COEFFT(N,R);
  IF N<R THEN NIL
  ELSE IF N=R THEN 1
  ELSE IF R=1 THEN N
  ELSE BEGIN SCALAR N!-C!-R,B;
    N!-C!-R:=1;
    B:=MIN(R,N-R);
    FOR I:=1:B DO
      N!-C!-R:=(N!-C!-R * (N - I + 1)) / I;
    RETURN N!-C!-R
  END;

SYMBOLIC PROCEDURE PMAM!-SQRT N;
% find the square root of n and return integer part + 1;
% n is fixed pt on input as it may be very large ie > largest
% allowed floating pt number so i scale it appropriately;
  BEGIN SCALAR S,TEN!*!*14,TEN!*!*12;
    S:=0;
    TEN!*!*12:=10**12;
    TEN!*!*14:=100*TEN!*!*12;
    WHILE N>TEN!*!*14 DO << S:=IADD1 S; N:=1+N/TEN!*!*12 >>;
    RETURN ((FIX SQRT FLOAT N) + 1) * 10**(6*S)
  END;

SYMBOLIC PROCEDURE FIND!-ALPHAS!-IN!-A!-RING(N,MFLIST,FHATLIST,GAMMA);
% find the alphas (as below) given that the modulus may not be prime
% but is a prime power.;
  BEGIN SCALAR GG,M,PPOW,I,GG!-MOD!-P,MODFLIST,WVEC,ALPHA,ALPHAZEROS,W;
    IF NULL PRIME!-BASE THEN ERRORF
      LIST("Prime base not set for finding alphas",
        CURRENT!-MODULUS,N,MFLIST);
    M:=SET!-MODULUS PRIME!-BASE;
    MODFLIST:= IF M=PRIME!-BASE THEN MFLIST
      ELSE FOR EACH FTHING IN MFLIST COLLECT
        REDUCE!-MOD!-P !*MOD2F FTHING;
    ALPHALIST:=ALPHAS(N,MODFLIST,GAMMA);
    IF M=PRIME!-BASE THEN <<
      SET!-MODULUS M;
      RETURN ALPHALIST >>;
    I:=0;
    ALPHAZEROS:=MKVECT N;
    WVEC:=MKVECT N;
    FOR EACH MODFTHING IN MODFLIST DO <<
      PUTV(MODFVEC,I:=IADD1 I,MODFTHING);
      PUTV(ALPHAVEC,I,!*F2MOD(ALPHA:=CDR GET!-ALPHA MODFTHING));
      PUTV(ALPHAZEROS,I,ALPHA);
      PUTV(WVEC,I,ALPHA);
      PUTV(FHATVEC,I,CAR FHATLIST);
      FHATLIST:=CDR FHATLIST >>;
    GG:=GAMMA;
    PPOW:=PRIME!-BASE;
    WHILE PPOW<M DO <<
      SET!-MODULUS M;
      GG:=!*F2MOD QUOTFAIL(!*MOD2F DIFFERENCE!-MOD!-P(GG,
          FORM!-SUM!-AND!-PRODUCT!-MOD!-M(WVEC,FHATVEC,N)),PRIME!-BASE);
      SET!-MODULUS PRIME!-BASE;
      GG!-MOD!-P:=REDUCE!-MOD!-P !*MOD2F GG;
      FOR K:=1:N DO <<
        PUTV(WVEC,K,W:=REMAINDER!-MOD!-P(
          TIMES!-MOD!-P(GETV(ALPHAZEROS,K),GG!-MOD!-P),
          GETV(MODFVEC,K)));
	PUTV(ALPHAVEC,K,ADDF(GETV(ALPHAVEC,K),MULTF(!*MOD2F W,PPOW)))>>;
      PPOW:=PPOW*PRIME!-BASE >>;
    SET!-MODULUS M;
    I:=0;
    RETURN (FOR EACH FTHING IN MFLIST COLLECT
      (FTHING . !*F2MOD GETV(ALPHAVEC,I:=IADD1 I)))
  END;

SYMBOLIC PROCEDURE ALPHAS(N,FLIST,GAMMA);
% finds alpha,beta,delta,... wrt factors f(i) in flist s.t:
%  alpha*g(1) + beta*g(2) + delta*g(3) + ... = gamma mod p;
% where g(i)=product(all the f(j) except f(i) itself);
% (cf. xgcd!-mod!-p below). n is number of factors in flist;
  IF N=1 THEN LIST(CAR FLIST . GAMMA)
  ELSE BEGIN SCALAR K,W,F1,F2,I,GAMMA1,GAMMA2;
    K:=N/2;
    F1:=1; F2:=1;
    I:=1;
    FOR EACH F IN FLIST DO
    << IF I>K THEN F2:=TIMES!-MOD!-P(F,F2)
       ELSE F1:=TIMES!-MOD!-P(F,F1);
       I:=I+1 >>;
    W:=XGCD!-MOD!-P(F1,F2,1,POLYZERO,POLYZERO,1);
    IF ATOM W THEN
      RETURN 'FACTORS! NOT! COPRIME;
    GAMMA1:=REMAINDER!-MOD!-P(TIMES!-MOD!-P(CDR W,GAMMA),F1);
    GAMMA2:=REMAINDER!-MOD!-P(TIMES!-MOD!-P(CAR W,GAMMA),F2);
    I:=1; F1:=NIL; F2:=NIL;
    FOR EACH F IN FLIST DO
    << IF I>K THEN F2:=F . F2
       ELSE F1:=F . F1;
       I:=I+1 >>;
    RETURN APPEND(
      ALPHAS(K,F1,GAMMA1),
      ALPHAS(N-K,F2,GAMMA2))
  END;

SYMBOLIC PROCEDURE XGCD!-MOD!-P(A,B,X1,Y1,X2,Y2);
% finds alpha and beta s.t. alpha*a+beta*b=1;
% returns alpha . beta or nil if a and b are not coprime;
    IF NULL B THEN NIL
    ELSE IF ISDOMAIN B THEN BEGIN
        B:=MODULAR!-RECIPROCAL B;
        X2:=MULTIPLY!-BY!-CONSTANT!-MOD!-P(X2,B);
        Y2:=MULTIPLY!-BY!-CONSTANT!-MOD!-P(Y2,B);
        RETURN X2 . Y2 END
    ELSE BEGIN SCALAR Q;
        Q:=QUOTIENT!-MOD!-P(A,B); % Truncated quotient here;
        RETURN XGCD!-MOD!-P(B,DIFFERENCE!-MOD!-P(A,TIMES!-MOD!-P(B,Q)),
            X2,Y2,
            DIFFERENCE!-MOD!-P(X1,TIMES!-MOD!-P(X2,Q)),
            DIFFERENCE!-MOD!-P(Y1,TIMES!-MOD!-P(Y2,Q)))
        END;

SYMBOLIC PROCEDURE HENSEL!-MOD!-P(POLY,MVEC,FVEC,CBD,VSET,P);
% hensel construction building up in powers of p;
% given that poly=product(factors in factorvec) mod p, find the full
% factors over the integers. mvec contains the univariate factors mod p
% while fvec contains our best knowledge of the factors to date.
% fvec includes leading coeffts (and in multivariate case possibly other
% coeffts) of the factors. return a list whose first element is a flag
% with one of the following values:
%  ok        construction worked, the cdr of the result is a list of
%            the correct factors.;
%  failed    inputs must have been incorrect
%  overshot  factors are correct mod some power of p (say p**m),
%            but are not correct over the integers.
%            result is (overshot,p**m,list of factors so far);
  BEGIN SCALAR W,U0,DELFVEC,OLD!.MOD,RES,M;
    U0:=INITIALIZE!-HENSEL(NUMBER!-OF!-FACTORS,P,POLY,MVEC,FVEC,CBD);
            % u0 contains the product (over integers) of factors mod p;
    IF NUMBER!-OF!-FACTORS=1 THEN GOTO EXIT;
            % only one factor to grow! but need to go this deep to
            % construct the alphas and set things up for the
            % multivariate growth which may follow;
    FACTOR!-TRACE <<
      PRINTSTR
	 "We are now ready to use the Hensel construction to grow";
      PRIN2!* "in powers of "; PRINTSTR CURRENT!-MODULUS;
      IF NOT !*OVERVIEW THEN <<PRIN2!* "Polynomial to factor (=U): ";
	FAC!-PRINTSF HENSEL!-POLY>>;
      PRIN2!* "Initial factors mod "; PRIN2!* P;
      PRINTSTR " with some correct coefficients:";
      W:=1;
      FOR I:=1:NUMBER!-OF!-FACTORS DO <<
        PRIN2!* " f("; PRIN2!* W; PRIN2!* ")=";
	FAC!-PRINTSF GETV(FACTORVEC,I); W:=IADD1 W >>;
      IF NOT !*OVERVIEW THEN << PRIN2!* "Coefficient bound = ";
        PRIN2!* COEFFTBD;
      TERPRI!*(NIL);
      PRIN2!* "The product of factors over the integers is ";
      FAC!-PRINTSF U0;
      PRINTSTR "In each step below, the residue is U - (product of the";
      PRINTSTR
	 "factors as far as we know them). The correction to each";
      PRINTSTR "factor, f(i), is (a(i)*v) mod f0(i) where f0(i) is";
      PRIN2!* "f(i) mod "; PRIN2!* P;
      PRINTSTR "(ie. the f(i) used in calculating the a(i))"
      >>
    >>;
    OLD!.MOD:=SET!-MODULUS P;
    RES:=ADDF(HENSEL!-POLY,NEGF U0);
            % calculate the residue. from now on this is always
            % kept in res;
    M:=P;
            % measure of how far we have built up factors - at this;
            % stage we know the constant terms mod p in the factors;
    WHILE NOT POLYZEROP RES DO
    <<
      IF (M/2)>COEFFTBD THEN
        RETURN <<
            % we started with a false split of the image so some
            % of the factors we have built up must amalgamate in
            % the complete factorization;
          IF !*OVERSHOOT THEN <<
            PRINC IF NULL VSET THEN "Univariate " ELSE "Multivariate ";
            PRINTC "coefft bound overshoot" >>;
          IF NOT !*OVERVIEW THEN
        FACTOR!-TRACE PRINTSTR "We have overshot the coefficient bound";
          W:='OVERSHOT >>;
      RES:=QUOTFAIL(RES,DELTAM);
            % next term in residue;
      IF NOT !*OVERVIEW THEN FACTOR!-TRACE <<
        PRIN2!* "Residue divided by "; PRIN2!* M; PRIN2!* " is ";
	FAC!-PRINTSF RES >>;
      IF (NOT !*LINEAR) AND NULL VSET
        AND M<=LARGEST!-SMALL!-MODULUS AND M>P THEN
        QUADRATIC!-STEP(M,NUMBER!-OF!-FACTORS);
      W:=REDUCE!-MOD!-P RES;
      IF NOT !*OVERVIEW THEN FACTOR!-TRACE <<
          PRIN2!* "Next term in residue to kill is:";
          PRINSF W; PRIN2!* " which is of size ";
	  FAC!-PRINTSF (DELTAM*M);
          >>;
      SOLVE!-FOR!-CORRECTIONS(W,FHATVEC,MODFVEC,DELFVEC,VSET);
            % delfvec is vector of next correction terms to factors;
      MAKE!-VEC!-MODULAR!-SYMMETRIC(DELFVEC,NUMBER!-OF!-FACTORS);
      IF NOT !*OVERVIEW THEN FACTOR!-TRACE <<
        PRINTSTR "Correction terms are:";
        W:=1;
        FOR I:=1:NUMBER!-OF!-FACTORS DO <<
          PRIN2!* "  To f("; PRIN2!* W; PRIN2!* "): ";
	  FAC!-PRINTSF MULTF(M,GETV(DELFVEC,I));
          W:=IADD1 W >>
      >>;
      W:=TERMS!-DONE(FACTORVEC,DELFVEC,M);
      RES:=ADDF(RES,NEGF W);
            % subtract out the terms generated by these corrections
            % from the residue;
      CURRENT!-FACTOR!-PRODUCT:=
	 ADDF(CURRENT!-FACTOR!-PRODUCT,MULTF(M,W));
            % add in the correction terms to give new factor product;
      FOR I:=1:NUMBER!-OF!-FACTORS DO
        PUTV(FACTORVEC,I,
          ADDF(GETV(FACTORVEC,I),MULTF(GETV(DELFVEC,I),M)));
            % add the corrections into the factors;
      IF NOT !*OVERVIEW THEN FACTOR!-TRACE <<
        PRINTSTR "   giving new factors as:";
        W:=1;
        FOR I:=1:NUMBER!-OF!-FACTORS DO <<
          PRIN2!* " f("; PRIN2!* W; PRIN2!* ")=";
	  FAC!-PRINTSF GETV(FACTORVEC,I); W:=IADD1 W >>
        >>;
      M:=M*DELTAM;
      IF NOT POLYZEROP RES AND NULL VSET AND
        NOT RECONSTRUCTING!-GCD THEN
        BEGIN SCALAR J,U,FAC;
          J:=0;
          WHILE (J:=J #+ 1)<=NUMBER!-OF!-FACTORS DO
%            IF NULL GETV(DELFVEC,J) AND;
            % - Try dividing out every time for now;
            IF NOT DIDNTGO
              (U:=QUOTF(HENSEL!-POLY,FAC:=GETV(FACTORVEC,J))) THEN <<
              HENSEL!-POLY:=U;
              RES:=ADJUST!-GROWTH(FAC,J,M);
              J:=NUMBER!-OF!-FACTORS >>
        END
    >>;
EXIT:
    IF FACTORS!-DONE THEN <<
      IF NOT(W='OVERSHOT) THEN M:=P*P;
      SET!-HENSEL!-FLUIDS!-BACK P >>;
    IF (NOT (W='OVERSHOT)) AND NULL VSET
      AND (NOT !*LINEAR) AND MULTIVARIATE!-INPUT!-POLY THEN
      WHILE M<LARGEST!-SMALL!-MODULUS DO <<
        IF NOT(M=DELTAM) THEN QUADRATIC!-STEP(M,NUMBER!-OF!-FACTORS);
        M:=M*DELTAM >>;
            % set up the alphas etc so that multivariate growth can
            % use a hensel growth size of about word size;
    SET!-MODULUS OLD!.MOD;
            % reset the old modulus;
    HENSEL!-GROWTH!-SIZE:=DELTAM;
    PUTV(FACTORVEC,0,NUMBER!-OF!-FACTORS);
    RETURN
      IF W='OVERSHOT THEN LIST('OVERSHOT,M,FACTORVEC)
      ELSE 'OK . FACTORVEC
  END;

SYMBOLIC PROCEDURE INITIALIZE!-HENSEL(R,P,POLY,MVEC,FVEC,CBD);
% set up the vectors and initialize the fluids;
  BEGIN SCALAR U0,W;
    DELFVEC:=MKVECT R;
    FACVEC:=MKVECT R;
    HENSEL!-POLY:=POLY;
    MODFVEC:=MVEC;
    FACTORVEC:=FVEC;
    COEFFTBD:=CBD;
    FACTORS!-DONE:=NIL;
    DELTAM:=P;
    U0:=1;
    FOR I:=1:R DO U0:=MULTF(GETV(FACTORVEC,I),U0);
    CURRENT!-FACTOR!-PRODUCT:=U0;
    RETURN U0
  END;

% SYMBOLIC PROCEDURE RESET!-QUADRATIC!-STEP!-FLUIDS(POLY,FACLIST,N);
%   BEGIN SCALAR I,OM,MODF;
%     CURRENT!-FACTOR!-PRODUCT:=POLY;
%     OM:=SET!-MODULUS HENSEL!-GROWTH!-SIZE;
%     I:=0;
%     FOR EACH FAC IN FACLIST DO <<
%       PUTV(FACTORVEC,I:=IADD1 I,FAC);
%       PUTV(MODFVEC,I,MODF:=REDUCE!-MOD!-P FAC);
%       PUTV(ALPHAVEC,I,CDR GET!-ALPHA MODF) >>;
%      FOR I:=1:N DO <<
%        PRINC "f("; % PRINC I; % PRINC ") = ";
%        FAC!-PRINTSF GETV(FACTORVEC,I);
%        PRINC "f("; % PRINC I; % PRINC ") mod p = ";
%        FAC!-PRINTSF GETV(MODFVEC,I);
%        PRINC "a("; % PRINC I; % PRINC ") = ";
%        FAC!-PRINTSF GETV(ALPHAVEC,I) >>;
%     SET!-MODULUS OM
%   END;

SYMBOLIC PROCEDURE RESET!-QUADRATIC!-STEP!-FLUIDS(POLY,FACLIST,N);
  BEGIN SCALAR I,OM,FACPAIRLIST,CFP!-MOD!-P,FHATLIST;
    CURRENT!-FACTOR!-PRODUCT:=POLY;
    OM:=SET!-MODULUS HENSEL!-GROWTH!-SIZE;
    CFP!-MOD!-P:=REDUCE!-MOD!-P CURRENT!-FACTOR!-PRODUCT;
    I:=0;
    FACPAIRLIST:=FOR EACH FAC IN FACLIST COLLECT <<
      I:= I #+ 1;
      (FAC . REDUCE!-MOD!-P FAC) >>;
    FHATLIST:=FOR EACH FACC IN FACPAIRLIST COLLECT
      QUOTFAIL!-MOD!-P(CFP!-MOD!-P,CDR FACC);
    IF FACTORS!-DONE THEN ALPHALIST:=
      FIND!-ALPHAS!-IN!-A!-RING(I,
        FOR EACH FACPR IN FACPAIRLIST COLLECT CDR FACPR,
        FHATLIST,1);
	  % a bug has surfaced such that the alphas get out of step
	  % in this case so recalculate them to stop the error for now;
    I:=0;
    FOR EACH FACPAIR IN FACPAIRLIST DO <<
      PUTV(FACTORVEC,I:=IADD1 I,CAR FACPAIR);
      PUTV(MODFVEC,I,CDR FACPAIR);
      PUTV(ALPHAVEC,I,CDR GET!-ALPHA CDR FACPAIR) >>;
%      FOR I:=1:N DO <<
%        PRINC "f("; % PRINC I; % PRINC ") = ";
%        FAC!-PRINTSF GETV(FACTORVEC,I);
%        PRINC "f("; % PRINC I; % PRINC ") mod p = ";
%        FAC!-PRINTSF GETV(MODFVEC,I);
%        PRINC "a("; % PRINC I; % PRINC ") = ";
%        FAC!-PRINTSF GETV(ALPHAVEC,I) >>;
    SET!-MODULUS OM
  END;

SYMBOLIC PROCEDURE QUADRATIC!-STEP(M,R);
% code for adjusting the hensel variables to take quadratic
% steps in the growing process;
  BEGIN SCALAR W,S,CFP!-MOD!-P;
    SET!-MODULUS M;
    CFP!-MOD!-P:=REDUCE!-MOD!-P CURRENT!-FACTOR!-PRODUCT;
    FOR I:=1:R DO PUTV(FACVEC,I,REDUCE!-MOD!-P GETV(FACTORVEC,I));
    FOR I:=1:R DO PUTV(FHATVEC,I,
      QUOTFAIL!-MOD!-P(CFP!-MOD!-P,GETV(FACVEC,I)));
    W:=FORM!-SUM!-AND!-PRODUCT!-MOD!-M(ALPHAVEC,FHATVEC,R);
    W:=!*MOD2F PLUS!-MOD!-P(1,MINUS!-MOD!-P W);
    S:=QUOTFAIL(W,DELTAM);
    SET!-MODULUS DELTAM;
    S:=!*F2MOD S;
            % Boxes S up to look like a poly mod deltam;
    FOR I:=1:R DO <<
      W:=REMAINDER!-MOD!-P(TIMES!-MOD!-P(S,GETV(ALPHAVEC,I)),
        GETV(MODFVEC,I));
      PUTV(ALPHAVEC,I,
        ADDF(!*MOD2F GETV(ALPHAVEC,I),MULTF(!*MOD2F W,DELTAM))) >>;
    S:=MODFVEC;
    MODFVEC:=FACVEC;
    FACVEC:=S;
    DELTAM:=M;
            % this is our new growth rate;
    SET!-MODULUS DELTAM;
    FOR I:=1:R DO <<
      PUTV(FACVEC,I,"RUBBISH");
            % we will want to overwrite facvec next time so we
            % had better point it to the old (no longer needed)
            % modvec. Also mark it as containing rubbish for safety;
      PUTV(ALPHAVEC,I,!*F2MOD GETV(ALPHAVEC,I)) >>;
            % Make sure the alphas are boxed up as being mod new deltam;
    IF NOT !*OVERVIEW THEN FACTOR!-TRACE <<
      PRINTSTR "The new modular polynomials are chosen such that:";
      TERPRI();
      PRIN2!* "   a(1)*h(1) + ... + a(";
      PRIN2!* R;
      PRIN2!* ")*h("; PRIN2!* R;
      PRIN2!* ") = 1 mod "; PRINTSTR M;
      TERPRI();
      PRINTSTR "  where h(i)=(product of all f(j) [see below])/f(i)";
      PRINTSTR "    and degree of a(i) < degree of f(i).";
      FOR I:=1:R DO <<
        PRIN2!* "  a("; PRIN2!* I; PRIN2!* ")=";
	FAC!-PRINTSF GETV(ALPHAVEC,I);
        PRIN2!* "   f("; PRIN2!* I; PRIN2!* ")=";
	FAC!-PRINTSF GETV(MODFVEC,I) >>
    >>
  END;

SYMBOLIC PROCEDURE TERMS!-DONE(FVEC,DELFVEC,M);
  BEGIN SCALAR FLIST,DELFLIST;
    FOR I:=1:NUMBER!-OF!-FACTORS DO <<
      FLIST:=GETV(FVEC,I) . FLIST;
      DELFLIST:=GETV(DELFVEC,I) . DELFLIST >>;
    RETURN TERMS!.DONE(NUMBER!-OF!-FACTORS,FLIST,DELFLIST,
                                 NUMBER!-OF!-FACTORS,M)
  END;

SYMBOLIC PROCEDURE TERMS!.DONE(N,FLIST,DELFLIST,R,M);
  IF N=1 THEN (CAR FLIST) . (CAR DELFLIST)
  ELSE BEGIN SCALAR K,I,F1,F2,DELF1,DELF2;
    K:=N/2; I:=1;
    FOR EACH F IN FLIST DO
    << IF I>K THEN F2:=(F . F2)
       ELSE F1:=(F . F1);
       I:=I+1 >>;
    I:=1;
    FOR EACH DELF IN DELFLIST DO
    << IF I>K THEN DELF2:=(DELF . DELF2)
       ELSE DELF1:=(DELF . DELF1);
       I:=I+1 >>;
    F1:=TERMS!.DONE(K,F1,DELF1,R,M);
    DELF1:=CDR F1; F1:=CAR F1;
    F2:=TERMS!.DONE(N-K,F2,DELF2,R,M);
    DELF2:=CDR F2; F2:=CAR F2;
    DELF1:=
      ADDF(ADDF(
        MULTF(F1,DELF2),
        MULTF(F2,DELF1)),
        MULTF(MULTF(DELF1,M),DELF2));
    IF N=R THEN RETURN DELF1;
    RETURN (MULTF(F1,F2) . DELF1)
  END;

SYMBOLIC PROCEDURE TRY!.COMBINING(L,POLY,M,SOFAR);
% l is a list of factors, f(i), s.t. (product of the f(i) mod m) = poly
% but no f(i) divides poly over the integers. we find the combinations
% of the f(i) that yield the true factors of poly over the integers.
% sofar is a list of these factors found so far. ;
  IF POLY=1 THEN
    IF NULL L THEN SOFAR
    ELSE ERRORF(LIST("TOO MANY BAD FACTORS:",L))
  ELSE BEGIN SCALAR N,RES,FF,V,W,W1,COMBINED!.FACTORS,LL;
    N:=LENGTH L;
    IF N=1 THEN
      IF LDEG CAR L > (LDEG POLY)/2 THEN
        RETURN ('ONE! BAD! FACTOR . SOFAR)
      ELSE ERRORF(LIST("ONE BAD FACTOR DOES NOT FIT:",L));
    IF N=2 OR N=3 THEN <<
      W:=LC CDAR L; % The LC of all the factors is the same;
      WHILE NOT (W=LC POLY) DO POLY:=QUOTFAIL(POLY,W);
            % poly's LC may be a higher power of w than we want
            % and we must return a result with the same
            % LC as each of the combined factors;
      IF NOT !*OVERVIEW THEN FACTOR!-TRACE <<
        PRINTSTR "We combine:";
	 FOR EACH LF IN L DO FAC!-PRINTSF CDR LF;
	 PRIN2!* " mod "; PRIN2!* M;
	 PRINTSTR " to give correct factor:";
	 FAC!-PRINTSF POLY >>;
       COMBINE!.ALPHAS(L,T);
       RETURN (POLY . SOFAR) >>;
    LL:=FOR EACH FF IN L COLLECT (CDR FF . CAR FF);
    FOR K:=2:(N/2) DO <<
      W:=KOUTOF(K,IF 2*K=N THEN CDR L ELSE L,NIL);
      WHILE W AND (V:=FACTOR!-TRIALDIV(POLY,CAR W,M,LL))='DIDNTGO DO
      << W:=CDR W;
        WHILE W AND
            ((CAR W = '!*LAZYADJOIN) OR (CAR W = '!*LAZYKOUTOF)) DO
          IF CAR W= '!*LAZYADJOIN THEN
            W:=LAZY!-ADJOIN(CADR W,CADDR W,CADR CDDR W)
          ELSE W:=KOUTOF(CADR W,CADDR W,CADR CDDR W)
        >>;
      IF NOT(V='DIDNTGO) THEN <<
        FF:=CAR V; V:=CDR V;
        IF NOT !*OVERVIEW THEN FACTOR!-TRACE <<
          PRINTSTR "We combine:";
	   FOR EACH A IN CAR W DO FAC!-PRINTSF A;
	 PRIN2!* " mod "; PRIN2!* M;
	 PRINTSTR " to give correct factor:";
	 FAC!-PRINTSF FF >>;
       FOR EACH A IN CAR W DO <<
         W1:=L;
         WHILE NOT (A = CDAR W1) DO W1:=CDR W1;
         COMBINED!.FACTORS:=CAR W1 . COMBINED!.FACTORS;
         L:=DELETE(CAR W1,L) >>;
       COMBINE!.ALPHAS(COMBINED!.FACTORS,T);
       RETURN RES:=TRY!.COMBINING(L,V,M,FF . SOFAR) >>
    >>;
    IF RES THEN RETURN RES
    ELSE <<
      W:=LC CDAR L; % The LC of all the factors is the same;
      WHILE NOT (W=LC POLY) DO POLY:=QUOTFAIL(POLY,W);
            % poly's LC may be a higher power of w than we want
            % and we must return a result with the same
            % LC as each of the combined factors;
      IF NOT !*OVERVIEW THEN FACTOR!-TRACE <<
        PRINTSTR "We combine:";
	  FOR EACH FF IN L DO FAC!-PRINTSF CDR FF;
	  PRIN2!* " mod "; PRIN2!* M;
	  PRINTSTR " to give correct factor:";
	  FAC!-PRINTSF POLY >>;
      COMBINE!.ALPHAS(L,T);
      RETURN (POLY . SOFAR) >>
  END;

SYMBOLIC PROCEDURE KOUTOF(K,L,SOFAR);
% produces all permutations of length k from list l accumulating them
% in sofar as we go.  we use lazy evaluation in that this results in
% a permutation dotted with:
%   ( '!*lazy . (argument for eval) )
%  except when k=1 when the permutations are explicitly given.;
  IF K=1 THEN APPEND(
    FOR EACH F IN L COLLECT LIST CDR F,SOFAR)
  ELSE IF K>LENGTH L THEN SOFAR
  ELSE <<
    WHILE EQCAR(L,'!*LAZYADJOIN) OR EQCAR(L,'!*LAZYKOUTOF) DO
      IF CAR L='!*LAZYADJOIN THEN
        L := LAZY!-ADJOIN(CADR L,CADDR L,CADR CDDR L)
      ELSE L := KOUTOF(CADR L,CADDR L,CADR CDDR L);
    IF K=LENGTH L THEN
      (FOR EACH LL IN L COLLECT CDR LL ) . SOFAR
    ELSE KOUTOF(K,CDR L,
      LIST('!*LAZYADJOIN,CDAR L,
        LIST('!*LAZYKOUTOF,(K-1),CDR L,NIL),
         SOFAR)) >>;

SYMBOLIC PROCEDURE LAZY!-ADJOIN(ITEM,L,TAIL);
% dots item with each element in l using lazy evaluation on l.
% if l is null tail results;
 << WHILE EQCAR(L,'!*LAZYADJOIN) OR EQCAR(L,'!*LAZYKOUTOF) DO
      IF CAR L ='!*LAZYADJOIN THEN
        L:=LAZY!-ADJOIN(CADR L,CADDR L,CADR CDDR L)
      ELSE L:=KOUTOF(CADR L,CADDR L,CADR CDDR L);
    IF NULL L THEN TAIL
    ELSE (ITEM . CAR L) .
     IF NULL CDR L THEN TAIL
     ELSE LIST('!*LAZYADJOIN,ITEM,CDR L,TAIL) >>;

SYMBOLIC PROCEDURE FACTOR!-TRIALDIV(POLY,FLIST,M,LLIST);
% Combines the factors in FLIST mod M and test divides the result
% into POLY (over integers) to see if it goes. If it doesn't
% then DIDNTGO is returned, else the pair (D . Q) is
% returned where Q is the quotient obtained and D is the product
% of the factors mod M;
  IF POLYZEROP POLY THEN ERRORF "Test dividing into zero?"
  ELSE BEGIN SCALAR D,Q;
    D:=COMBINE(FLIST,M,LLIST);
    IF DIDNTGO(Q:=QUOTF(POLY,CAR D)) THEN <<
      FACTOR!-TRACE PRINTSTR " it didn't go";
      RETURN 'DIDNTGO >>
    ELSE <<
      FACTOR!-TRACE PRINTSTR " it worked !";
      RETURN (CAR D . QUOTF(Q,CDR D)) >>
  END;

SYMBOLIC PROCEDURE COMBINE(FLIST,M,L);
% multiply factors in flist mod m;
% L is a list of the factors for use in FACTOR!-TRACE;
  BEGIN SCALAR OM,RES,W,LCF,LCFINV,LCFPROD;
    FACTOR!-TRACE <<
      PRIN2!* "We combine factors ";
      FOR EACH FF IN FLIST DO <<
        W:=ASSOC(FF,L);
        PRIN2!* "f(";
        PRIN2!* CDR W;
        PRIN2!* "), " >> ;
      PRIN2!* "and try dividing : " >>;
    LCF := LC CAR FLIST; % ALL LEADING COEFFTS SHOULD BE THE SAME;
    LCFPROD := 1;
% This is one of only two places in the entire factorizer where
% it is ever necessary to use a modulus larger than word-size;
    IF M>LARGEST!-SMALL!-MODULUS THEN <<
      OM:=SET!-GENERAL!-MODULUS M;
      LCFINV := GENERAL!-MODULAR!-RECIPROCAL LCF;
      RES:=GENERAL!-REDUCE!-MOD!-P CAR FLIST;
      FOR EACH FF IN CDR FLIST DO <<
        IF NOT LCF=LC FF THEN ERRORF "BAD LC IN FLIST";
        RES:=GENERAL!-TIMES!-MOD!-P(
            GENERAL!-TIMES!-MOD!-P(LCFINV,
                GENERAL!-REDUCE!-MOD!-P FF),RES);
        LCFPROD := LCFPROD*LCF >>;
      RES:=GENERAL!-MAKE!-MODULAR!-SYMMETRIC RES;
      SET!-MODULUS OM;
      RETURN (RES . LCFPROD) >>
    ELSE <<
      OM:=SET!-MODULUS M;
      LCFINV := MODULAR!-RECIPROCAL LCF;
      RES:=REDUCE!-MOD!-P CAR FLIST;
      FOR EACH FF IN CDR FLIST DO <<
        IF NOT LCF=LC FF THEN ERRORF "BAD LC IN FLIST";
        RES:=TIMES!-MOD!-P(TIMES!-MOD!-P(LCFINV,REDUCE!-MOD!-P FF),RES);
        LCFPROD := LCFPROD*LCF >>;
      RES:=MAKE!-MODULAR!-SYMMETRIC RES;
      SET!-MODULUS OM;
      RETURN (RES . LCFPROD) >>
  END;

SYMBOLIC PROCEDURE COMBINE!.ALPHAS(FLIST,FIXLCS);
% combine the alphas associated with each of these factors to
% give the one alpha for their combination;
  BEGIN SCALAR F1,A1,FF,AA,OLDM,W,LCFAC,LCFINV,SAVEFLIST;;
    OLDM:=SET!-MODULUS HENSEL!-GROWTH!-SIZE;
    FLIST:=FOR EACH FAC IN FLIST COLLECT <<
      SAVEFLIST:= (REDUCE!-MOD!-P CDR FAC) . SAVEFLIST;
      (CAR FAC) . CAR SAVEFLIST >>;
    IF FIXLCS THEN <<
        LCFINV:=MODULAR!-RECIPROCAL LC CDAR FLIST;
        LCFAC:=MODULAR!-EXPT(LC CDAR FLIST,SUB1 LENGTH FLIST)
      >>
      ELSE << LCFINV:=1; LCFAC:=1 >>;
            % If FIXLCS is set then we have combined n factors
            % (each with the same l.c.) to give one and we only need one
            % l.c. in the result, we have divided the combination by
            % lc**(n-1) and we must be sure to do the same for the
            % alphas.;
    FF:=CDAR FLIST;
    AA:=CDR GET!-ALPHA FF;
    FLIST:=CDR FLIST;
    WHILE FLIST DO <<
      F1:=CDAR FLIST;
      A1:=CDR GET!-ALPHA F1;
      FLIST:=CDR FLIST;
      AA:=PLUS!-MOD!-P(TIMES!-MOD!-P(AA,F1),TIMES!-MOD!-P(A1,FF));
      FF:=TIMES!-MOD!-P(FF,TIMES!-MOD!-P(LCFINV,F1))
    >>;
    FOR EACH A IN ALPHALIST DO
      IF NOT MEMBER(CAR A,SAVEFLIST) THEN
        FLIST:=(CAR A . IF LCFAC=1 THEN CDR A
            ELSE TIMES!-MOD!-P(CDR A,LCFAC)) . FLIST;
    ALPHALIST:=(FF . AA) . FLIST;
    SET!-MODULUS OLDM
  END;

%*********************************************************************;
% The following code is for dividing out factors in the middle
% of the Hensel construction and adjusting all the associated
% variables that go with it.
%;


SYMBOLIC PROCEDURE ADJUST!-GROWTH(FACDONE,K,M);
% One factor (at least) divides out so we can reconfigure the
% problem for Hensel constrn giving a smaller growth and hopefully
% reducing the coefficient bound considerably;
  BEGIN SCALAR W,U,BOUND!-SCALE,MODFLIST,FACTORLIST,FHATLIST,
        MODFDONE,B;
    FACTORLIST:=VEC2LIST!-WITHOUT!-K(FACTORVEC,K);
    MODFLIST:=VEC2LIST!-WITHOUT!-K(MODFVEC,K);
    FHATLIST:=VEC2LIST!-WITHOUT!-K(FHATVEC,K);
    W:=NUMBER!-OF!-FACTORS;
    MODFDONE:=GETV(MODFVEC,K);
TOP:
    FACTORS!-DONE:=FACDONE . FACTORS!-DONE;
    IF (NUMBER!-OF!-FACTORS:=NUMBER!-OF!-FACTORS #- 1)=1 THEN <<
      FACTORS!-DONE:=HENSEL!-POLY . FACTORS!-DONE;
      NUMBER!-OF!-FACTORS:=0;
      HENSEL!-POLY:=1;
      IF NOT !*OVERVIEW THEN FACTOR!-TRACE <<
        PRINTSTR "    All factors found:";
	FOR EACH FD IN FACTORS!-DONE DO FAC!-PRINTSF FD >>;
      RETURN POLYZERO >>;
    FHATLIST:=FOR EACH FHAT IN FHATLIST COLLECT
      QUOTFAIL!-MOD!-P(IF NULL FHAT THEN POLYZERO ELSE FHAT,MODFDONE);
    U:=COMFAC FACDONE;  % Take contents and prim. parts;
    IF CAR U THEN
      ERRORF(LIST("Factor divisible by main variable: ",FACDONE,CAR U));
    FACDONE:=QUOTFAIL(FACDONE,CDR U);
    BOUND!-SCALE:=CDR U;
    IF NOT((B:=LC FACDONE)=1) THEN BEGIN SCALAR B!-INV,OLD!-M;
      HENSEL!-POLY:=QUOTFAIL(HENSEL!-POLY,B**NUMBER!-OF!-FACTORS);
      B!-INV:=MODULAR!-RECIPROCAL MODULAR!-NUMBER B;
      MODFLIST:=FOR EACH MODF IN MODFLIST COLLECT
        TIMES!-MOD!-P(B!-INV,MODF);
% This is one of only two places in the entire factorizer where
% it is ever necessary to use a modulus larger than word-size;
      IF M>LARGEST!-SMALL!-MODULUS THEN <<
        OLD!-M:=SET!-GENERAL!-MODULUS M;
        FACTORLIST:=FOR EACH FACC IN FACTORLIST COLLECT
          ADJOIN!-TERM(LPOW FACC,QUOTFAIL(LC FACC,B),
            GENERAL!-MAKE!-MODULAR!-SYMMETRIC(
              GENERAL!-TIMES!-MOD!-P(
            GENERAL!-MODULAR!-RECIPROCAL GENERAL!-MODULAR!-NUMBER B,
                            GENERAL!-REDUCE!-MOD!-P RED FACC))) >>
      ELSE <<
        OLD!-M:=SET!-MODULUS M;
        FACTORLIST:=FOR EACH FACC IN FACTORLIST COLLECT
          ADJOIN!-TERM(LPOW FACC,QUOTFAIL(LC FACC,B),
            MAKE!-MODULAR!-SYMMETRIC(
              TIMES!-MOD!-P(MODULAR!-RECIPROCAL MODULAR!-NUMBER B,
                            REDUCE!-MOD!-P RED FACC))) >>;
            % We must be careful not to destroy the information
            % that we have about the leading coefft;
      SET!-MODULUS OLD!-M;
      FHATLIST:=FOR EACH FHAT IN FHATLIST COLLECT
        TIMES!-MOD!-P(
          MODULAR!-EXPT(B!-INV,NUMBER!-OF!-FACTORS #- 1),FHAT)
    END;
TRY!-ANOTHER!-FACTOR:
    IF (W:=W #- 1)>0 THEN
      IF NOT DIDNTGO
        (U:=QUOTF(HENSEL!-POLY,FACDONE:=CAR FACTORLIST)) THEN <<
        HENSEL!-POLY:=U;
        FACTORLIST:=CDR FACTORLIST;
        MODFDONE:=CAR MODFLIST;
        MODFLIST:=CDR MODFLIST;
        FHATLIST:=CDR FHATLIST;
        GOTO TOP >>
      ELSE <<
        FACTORLIST:=APPEND(CDR FACTORLIST,LIST CAR FACTORLIST);
        MODFLIST:=APPEND(CDR MODFLIST,LIST CAR MODFLIST);
        FHATLIST:=APPEND(CDR FHATLIST,LIST CAR FHATLIST);
        GOTO TRY!-ANOTHER!-FACTOR >>;
    SET!-FLUIDS!-FOR!-NEWHENSEL(FACTORLIST,FHATLIST,MODFLIST);
    BOUND!-SCALE:=
      BOUND!-SCALE * GET!-COEFFT!-BOUND(
	QUOTFAIL(HENSEL!-POLY,BOUND!-SCALE**(NUMBER!-OF!-FACTORS #- 1)),
        LDEG HENSEL!-POLY);
    % We expect the new coefficient bound to be smaller, but on
    % dividing out a factor our polynomial's height may have grown
    % more than enough to compensate in the bound formula for
    % the drop in degree. Anyway, the bound we computed last time
    % will still be valid, so let's stick with the smaller;
    IF BOUND!-SCALE < COEFFTBD THEN COEFFTBD := BOUND!-SCALE;
    W:=QUOTFAIL(ADDF(HENSEL!-POLY,NEGF CURRENT!-FACTOR!-PRODUCT),
          M/DELTAM);
    IF NOT !*OVERVIEW THEN FACTOR!-TRACE <<
      PRINTSTR "    Factors found to be correct:";
      FOR EACH FD IN FACTORS!-DONE DO
	FAC!-PRINTSF FD;
      PRINTSTR "Remaining factors are:";
      PRINTVEC("    f(",NUMBER!-OF!-FACTORS,") = ",FACTORVEC);
      PRIN2!* "New coefficient bound is "; PRINTSTR COEFFTBD;
      PRIN2!* " and the residue is now "; FAC!-PRINTSF W >>;
    RETURN W
  END;

SYMBOLIC PROCEDURE VEC2LIST!-WITHOUT!-K(V,K);
% Turn a vector into a list leaving out Kth element;
  BEGIN SCALAR W;
    FOR I:=1:NUMBER!-OF!-FACTORS DO
      IF NOT(I=K) THEN W:=GETV(V,I) . W;
    RETURN W
  END;

SYMBOLIC PROCEDURE SET!-FLUIDS!-FOR!-NEWHENSEL(FLIST,FHATLIST,MODFLIST);
<< CURRENT!-FACTOR!-PRODUCT:=1;
  ALPHALIST:=
    FIND!-ALPHAS!-IN!-A!-RING(NUMBER!-OF!-FACTORS,MODFLIST,FHATLIST,1);
  FOR I:=NUMBER!-OF!-FACTORS STEP -1 UNTIL 1 DO <<
    PUTV(FACTORVEC,I,CAR FLIST);
    PUTV(MODFVEC,I,CAR MODFLIST);
    PUTV(FHATVEC,I,CAR FHATLIST);
    PUTV(ALPHAVEC,I,CDR GET!-ALPHA CAR MODFLIST);
    CURRENT!-FACTOR!-PRODUCT:=MULTF(CAR FLIST,CURRENT!-FACTOR!-PRODUCT);
    FLIST:=CDR FLIST;
    MODFLIST:=CDR MODFLIST;
    FHATLIST:=CDR FHATLIST >>
>>;

SYMBOLIC PROCEDURE SET!-HENSEL!-FLUIDS!-BACK P;
% After the Hensel growth we must be careful to set back any fluids
% that have been changed when we divided out a factor in the middle
% of growing.  Since calculating the alphas involves modular division
% we cannot do it mod DELTAM which is generally a non-trivial power of
% P (prime). So we calculate them mod P and if necessary we can do a
% few quadratic growth steps later. ;
  BEGIN SCALAR N,FD,MODFLIST,FULLF,MODF;
    SET!-MODULUS P;
    DELTAM:=P;
    N:=NUMBER!-OF!-FACTORS #+ LENGTH (FD:=FACTORS!-DONE);
    CURRENT!-FACTOR!-PRODUCT:=HENSEL!-POLY;
    FOR I:=(NUMBER!-OF!-FACTORS #+ 1):N DO <<
      PUTV(FACTORVEC,I,FULLF:=CAR FD);
      PUTV(MODFVEC,I,MODF:=REDUCE!-MOD!-P FULLF);
      CURRENT!-FACTOR!-PRODUCT:=MULTF(FULLF,CURRENT!-FACTOR!-PRODUCT);
      MODFLIST:=MODF . MODFLIST;
      FD:=CDR FD >>;
    FOR I:=1:NUMBER!-OF!-FACTORS DO <<
      MODF:=REDUCE!-MOD!-P !*MOD2F GETV(MODFVEC,I);
            % need to 'unbox' a modpoly before reducing it mod p as we
            % know that the input modpoly is wrt a larger modulus
            % (otherwise this would be a stupid thing to do anyway!)
            % and so we are just pretending it is a full poly;
      MODFLIST:=MODF . MODFLIST;
      PUTV(MODFVEC,I,MODF) >>;
    ALPHALIST:=ALPHAS(N,MODFLIST,1);
    FOR I:=1:N DO PUTV(ALPHAVEC,I,CDR GET!-ALPHA GETV(MODFVEC,I));
    NUMBER!-OF!-FACTORS:=N
  END;

ENDMODULE;


MODULE VECPOLY;

%**********************************************************************;
%
%   copyright (c)  university of cambridge, england 1979
%
%**********************************************************************;




%**********************************************************************;
% Routines for working with modular univariate polynomials
% stored as vectors. Used to avoid unwarranted storage management
% in the mod-p factorization process;


SAFE!-FLAG:=CARCHECK 0;


SYMBOLIC PROCEDURE COPY!-VECTOR(A,DA,B);
% Copy A into B;
 << FOR I:=0:DA DO
      PUTV(B,I,GETV(A,I));
    DA >>;

SYMBOLIC PROCEDURE TIMES!-IN!-VECTOR(A,DA,B,DB,C);
% Put the product of A and B into C and return its degree.
% C must not overlap with either A or B;
  BEGIN
    SCALAR DC,IC,W;
    IF DA#<0 OR DB#<0 THEN RETURN MINUS!-ONE;
    DC:=DA#+DB;
    FOR I:=0:DC DO PUTV(C,I,0);
    FOR IA:=0:DA DO <<
      W:=GETV(A,IA);
      FOR IB:=0:DB DO <<
        IC:=IA#+IB;
        PUTV(C,IC,MODULAR!-PLUS(GETV(C,IC),
          MODULAR!-TIMES(W,GETV(B,IB)))) >> >>;
    RETURN DC
  END;


SYMBOLIC PROCEDURE QUOTFAIL!-IN!-VECTOR(A,DA,B,DB);
% Overwrite A with (A/B) and return degree of result.
% The quotient must be exact;
    IF DA#<0 THEN DA
    ELSE IF DB#<0 THEN ERRORF "Attempt to divide by zero"
    ELSE IF DA#<DB THEN ERRORF "Bad degrees in QUOTFAIL-IN-VECTOR"
    ELSE BEGIN
      SCALAR DC;
      DC:=DA#-DB; % Degree of result;
      FOR I:=DC STEP -1 UNTIL 0 DO BEGIN
        SCALAR Q;
        Q:=MODULAR!-QUOTIENT(GETV(A,DB#+I),GETV(B,DB));
        FOR J:=0:DB#-1 DO
          PUTV(A,I#+J,MODULAR!-DIFFERENCE(GETV(A,I#+J),
            MODULAR!-TIMES(Q,GETV(B,J))));
        PUTV(A,DB#+I,Q)
      END;
      FOR I:=0:DB#-1 DO IF GETV(A,I) NEQ 0 THEN
        ERRORF "Quotient not exact in QUOTFAIL!-IN!-VECTOR";
      FOR I:=0:DC DO
        PUTV(A,I,GETV(A,DB#+I));
      RETURN DC
    END;


SYMBOLIC PROCEDURE REMAINDER!-IN!-VECTOR(A,DA,B,DB);
% Overwrite the vector A with the remainder when A is
% divided by B, and return the degree of the result;
  BEGIN
    SCALAR DELTA,DB!-1,RECIP!-LC!-B,W;
    IF DB=0 THEN RETURN MINUS!-ONE
    ELSE IF DB=MINUS!-ONE THEN ERRORF "ATTEMPT TO DIVIDE BY ZERO";
    RECIP!-LC!-B:=MODULAR!-MINUS MODULAR!-RECIPROCAL GETV(B,DB);
    DB!-1:=DB#-1; % Leading coeff of B treated specially, hence this;
    WHILE NOT((DELTA:=DA#-DB) #< 0) DO <<
      W:=MODULAR!-TIMES(RECIP!-LC!-B,GETV(A,DA));
      FOR I:=0:DB!-1 DO
        PUTV(A,I#+DELTA,MODULAR!-PLUS(GETV(A,I#+DELTA),
          MODULAR!-TIMES(GETV(B,I),W)));
      DA:=DA#-1;
      WHILE NOT(DA#<0) AND GETV(A,DA)=0 DO DA:=DA#-1 >>;
    RETURN DA
  END;

SYMBOLIC PROCEDURE EVALUATE!-IN!-VECTOR(A,DA,N);
% Evaluate A at N;
  BEGIN
    SCALAR R;
    R:=GETV(A,DA);
    FOR I:=DA#-1 STEP -1 UNTIL 0 DO
      R:=MODULAR!-PLUS(GETV(A,I),
        MODULAR!-TIMES(R,N));
    RETURN R
  END;

SYMBOLIC PROCEDURE GCD!-IN!-VECTOR(A,DA,B,DB);
% Overwrite A with the gcd of A and B. On input A and B are
% vectors of coefficients, representing polynomials
% of degrees DA and DB. Return DG, the degree of the gcd;
  BEGIN
    SCALAR W;
    IF DA=0 OR DB=0 THEN << PUTV(A,0,1); RETURN 0 >>
    ELSE IF DA#<0 OR DB#<0 THEN ERRORF "GCD WITH ZERO NOT ALLOWED";
TOP:
% Reduce the degree of A;
    DA:=REMAINDER!-IN!-VECTOR(A,DA,B,DB);
    IF DA=0 THEN << PUTV(A,0,1); RETURN 0 >>
    ELSE IF DA=MINUS!-ONE THEN <<
      W:=MODULAR!-RECIPROCAL GETV(B,DB);
      FOR I:=0:DB DO PUTV(A,I,MODULAR!-TIMES(GETV(B,I),W));
      RETURN DB >>;
% Now reduce degree of B;
    DB:=REMAINDER!-IN!-VECTOR(B,DB,A,DA);
    IF DB=0 THEN << PUTV(A,0,1); RETURN 0 >>
    ELSE IF DB=MINUS!-ONE THEN <<
      W:=MODULAR!-RECIPROCAL GETV(A,DA);
      IF NOT (W=1) THEN
        FOR I:=0:DA DO PUTV(A,I,MODULAR!-TIMES(GETV(A,I),W));
      RETURN DA >>;
    GO TO TOP
  END;



CARCHECK SAFE!-FLAG;


ENDMODULE;


MODULE ZMODP;

% *******************************************************************
%
%   copyright (c)  university of cambridge, england 1979
%
% *******************************************************************;



% modular arithmetic for use in univariate factorization
% routines;


SYMBOLIC PROCEDURE SET!-MODULUS P;
  IF NOT NUMBERP P OR P=0 THEN CURRENT!-MODULUS
  ELSE BEGIN
    SCALAR PREVIOUS!-MODULUS;
    PREVIOUS!-MODULUS:=CURRENT!-MODULUS;
    CURRENT!-MODULUS:=P;
    MODULUS!/2:=P/2;
    SET!-SMALL!-MODULUS P;
    RETURN PREVIOUS!-MODULUS
  END;

SYMBOLIC PROCEDURE MODULAR!-EXPT(A,N);
% a**n;
    IF N=0 THEN 1
    ELSE IF N=1 THEN A
    ELSE BEGIN
     SCALAR X;
     X:=MODULAR!-EXPT(A,IQUOTIENT(N,2));
     X:=MODULAR!-TIMES(X,X);
     IF NOT (IREMAINDER(N,2) = 0) THEN X:=MODULAR!-TIMES(X,A);
     RETURN X
    END;



LISP SET!-MODULUS(1) ; % forces everything into a standard state;



ENDMODULE;


END;
