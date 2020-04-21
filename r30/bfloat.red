COMMENT Module for Arbitrary Precision Real Arithmetic;

SYMBOLIC;

COMMENT *** Tables for Bigfloats ***;

GLOBAL '(DOMAINLIST!*);

DOMAINLIST!* := UNION('(!:BF!:),DOMAINLIST!*);
PUT('BIGFLOAT,'TAG,'!:BF!:);
PUT('!:BF!:,'DNAME,'BIGFLOAT);
FLAG('(!:BF!:),'FIELD);
PUT('!:BF!:,'I2D,'I2BF!:);
PUT('!:FT!:,'!:BF!:,'!*FT2BF);
PUT('!:RN!:,'!:BF!:,'!*RN2BF);
PUT('!:BF!:,'MINUSP,'MINUSP!:);
PUT('!:BF!:,'PLUS,'BFPLUS!:);
PUT('!:BF!:,'TIMES,'TTIMES!:);
PUT('!:BF!:,'DIFFERENCE,'TDIFFERENCE!:);
PUT('!:BF!:,'QUOTIENT,'BFQUOTIENT!:);
PUT('!:BF!:,'ZEROP,'ZEROP!:);
PUT('!:BF!:,'PREPFN,'BFPREP!:);
PUT('!:BF!:,'SPECPRN,'BFPRIN);

COMMENT SMACROS needed;

SYMBOLIC SMACRO PROCEDURE MT!: U; CADR U;

SYMBOLIC SMACRO PROCEDURE EP!: U; CDDR U;

SYMBOLIC PROCEDURE I2BF!: U; '!:BF!: . U . 0;

SYMBOLIC PROCEDURE !*RN2BF U;
   BEGIN SCALAR X;
      X := GET('!:BF!:,'I2D);
      RETURN APPLY(GET('!:BF!:,'QUOTIENT),
	LIST(APPLY(X,LIST CADR U),APPLY(X,LIST CDDR U)))
   END;

SYMBOLIC PROCEDURE !*FT2BF U; CONV!:A2BF CDR U;

GLOBAL '(!:PREC!:);

SYMBOLIC PROCEDURE BFPLUS!:(U,V);
   %value is sum of U and V, or zero (NIL) if outside precision;
   BEGIN SCALAR X,Y;
      X := TPLUS!:(U,V);
      Y := '!:BF!: . ABS MT!: X . (EP!: X+!:PREC!:-1);
      RETURN IF LESSP!:(Y,ABS!: U) AND LESSP!:(Y,ABS!: V) THEN NIL
	      ELSE X
   END;

SYMBOLIC PROCEDURE BFQUOTIENT!:(U,V);
   DIVIDE!:(U,V,!:PREC!:);

SYMBOLIC PROCEDURE BFPREP!: U; U;

SYMBOLIC PROCEDURE BFPRIN NMBR;
   %prints a big-float in a variety of formats. Still needs work
   %for fortran output;
    BEGIN INTEGER J,K;  SCALAR U,V,W;
	NMBR := ROUND!:MT('!:BF!: . NMBR,!:PREC!:-2);
	IF ZEROP!:(NMBR) THEN RETURN PRIN2!* '!0;
	U := EXPLODE ABS(J := MT!: NMBR);
	K := EP!: NMBR;
	IF K>=0 THEN IF K>5 THEN GO TO ETYPE
		ELSE <<V := LIST('!.,'!0);
		       WHILE (K := K-1)>=0 DO V := '!0 . V;
		       U := NCONC(U,V)>>
	 ELSE IF (K := ORDER!:(NMBR)+1)>0 
	  THEN <<V := U;
		 WHILE (K := K-1)>0 DO V := CDR V;
		 RPLACD(V,'!. . CDR V)>>
	 ELSE IF K<-10 THEN GO TO ETYPE
	 ELSE <<WHILE (K := K+1)<=0 DO U := '!0 . U;
		U := '!0 . '!. . U>>;
	BFPRIN1(U,J);
	RETURN NMBR;
   ETYPE:
	IF NULL( CDR(U)) THEN RPLACD(U , LIST('!0));
	U:= CAR U . '!. . CDR U;
	J := BFPRIN1(U,J);
	IF J=0 THEN <<PRIN2!*("E "  ); J:=2>> ELSE
	IF J=1 THEN <<PRIN2!*(" E " ); J:=4>> ELSE
	IF J=2 THEN <<PRIN2!*(" E  "); J:=0>> ELSE
	IF J=3 THEN <<PRIN2!*(" E " ); J:=0>> ELSE
	IF J=4 THEN <<PRIN2!*("  E "); J:=2>>;
	U:=EXPLODE( K:=ORDER!:(NMBR));
	IF K>=0 THEN U:=CONS('!+,U);
	WHILE U DO <<PRIN2!*( CAR(U)); U:=CDR(U); J:=J+1;
		   IF J=5 THEN <<PRIN2!*(" "); J:=0>> >>;
	RETURN NMBR
    END;

SYMBOLIC PROCEDURE BFPRIN1(U,J);
   BEGIN SCALAR V,W;
	IF J<0 THEN U := '!- . U;
	%suppress trailing zeros;
	V := U;
	WHILE NOT(CAR V EQ '!.) DO V := CDR V;
	V := CDR V;
    L:	WHILE CDR V AND NOT(CADR V EQ '!0) DO V := CDR V;
	W := CDR V;
        WHILE W AND CAR W EQ '!0 DO W := CDR W;
	IF NULL W THEN RPLACD(V,NIL) ELSE <<V := W; GO TO L>>;
	%now print the number;
	J := 0;
	FOR EACH CHAR IN U DO <<PRIN2!* CHAR; J := J+1;
				IF J=5 THEN <<IF !*NAT THEN PRIN2!* '! ;
					      J := 0>>>>;
	RETURN J
   END;

SYMBOLIC PROCEDURE BFLERRMSG U;
   %Standard error message for BFLOAT module;
   REDERR LIST("Invalid argument to",U);


COMMENT Simp property for !:BF!: since PREP is identity;

SYMBOLIC PROCEDURE !:BF!:SIMP U; ('!:BF!: . U) ./ 1;

PUT('!:BF!:,'SIMPFN,'!:BF!:SIMP);

!:PREC!: := 12;   %default value;

INITDMODE 'BIGFLOAT;

SYMBOLIC PROCEDURE PRECISION N;
   IF N=0 THEN !:PREC!:-2 ELSE <<!:PREC!: := N+2; N>>;

SYMBOLIC OPERATOR PRECISION;


COMMENT *** Tables for Elementary Function Numerical Values ***;

DEFLIST('((EXP BIGFLOAT) (LOG BIGFLOAT) (SIN BIGFLOAT) (COS BIGFLOAT)
	  (TAN BIGFLOAT) (ASIN BIGFLOAT) (ACOS BIGFLOAT)
	  (ATAN BIGFLOAT) (SQRT BIGFLOAT)),
        'TARGETMODE);

PUT('EXP,'DOMAINFN,'EXP!*);

SYMBOLIC PROCEDURE EXP!* U; EXP!:(U,!:PREC!:);

PUT('LOG,'DOMAINFN,'LOG!*);

SYMBOLIC PROCEDURE LOG!* U; LOG!:(U,!:PREC!:);

PUT('SIN,'DOMAINFN,'SIN!*);

SYMBOLIC PROCEDURE SIN!* U; SIN!:(U,!:PREC!:);

PUT('COS,'DOMAINFN,'COS!*);

SYMBOLIC PROCEDURE COS!* U; COS!:(U,!:PREC!:);

PUT('TAN,'DOMAINFN,'TAN!*);

SYMBOLIC PROCEDURE TAN!* U; TAN!:(U,!:PREC!:);

PUT('ASIN,'DOMAINFN,'ASIN!*);

SYMBOLIC PROCEDURE ASIN!* U; ASIN!:(U,!:PREC!:);

PUT('ACOS,'DOMAINFN,'ACOS!*);

SYMBOLIC PROCEDURE ACOS!* U; ACOS!:(U,!:PREC!:);

PUT('ATAN,'DOMAINFN,'ATAN!*);

SYMBOLIC PROCEDURE ATAN!* U; ATAN!:(U,!:PREC!:);

PUT('SQRT,'DOMAINFN,'SQRT!*);

SYMBOLIC PROCEDURE SQRT!* U; SQRT!:(U,!:PREC!:);


COMMENT *** Tables for constants with numerical values ***;

DEFLIST('((E BIGFLOAT) (PI BIGFLOAT)),'TARGETMODE);

PUT('E,'DOMAINFN,'E!*);

PUT('PI,'DOMAINFN,'PI!*);

SYMBOLIC PROCEDURE PI!*;
   IF !:PREC!:>1000 THEN !:BIGPI !:PREC!: ELSE !:PI !:PREC!:;

SYMBOLIC PROCEDURE E!*; !:E !:PREC!:;


%*************************************************************$
%*************************************************************$
%**                                                         **$
%**       ARBITRARY PRECISION REAL ARITHMETIC SYSTEM        **$
%**               machine-independent version               **$
%**                                                         **$
%**                         made by                         **$
%**                                                         **$
%**                     Tateaki  Sasaki                     **$
%**                                                         **$
%**           The University of Utah,  March 1979           **$
%**                                                         **$
%**=========================================================**$
%**                                                         **$
%**  For design philosophy and characteristics of this      **$
%**      system, see T. Sasaki, "An Arbitrary Precision     **$
%**      Real Arithmetic Package in REDUCE," Proceedings    **$
%**      of EUROSAM '79, Marseille (France), June 1979.     **$
%**                                                         **$
%**  For implementing and using this system, see T. Sasaki, **$
%**      "Manual for Arbitrary Precision Real Arithmetic    **$
%**      System in REDUCE," Operating Report of Utah Sym-   **$
%**      bolic Computation Group.                           **$
%**                                                         **$
%**=========================================================**$
%**                                                         **$
%**  In order to speed up this system, you have only to     **$
%**      rewrite four routines (DECPREC!:, INCPREC!:,       **$
%**      PRECI!:, and ROUND!:LAST) machine-dependently.     **$
%**                                                         **$
%**=========================================================**$
%**                                                         **$
%**                    Table of Contents                    **$
%**                                                         **$
%** 1-1. Initialization.                                    **$
%** 1-2. Constructor, selectors and basic predicate.        **$
%** 1-3. Temporary routines for rational number arithmetic. **$
%** 1-4. Counters.                                          **$
%** 1-5. Routines for converting the numeric type.          **$
%** 1-6. Routines for converting a big-float number.        **$
%** 1-7. Routines for reading/printing numbers.             **$
%** 2-1. Arithmetic manipulation routines.                  **$
%** 2-2. Arithmetic predicates.                             **$
%** 3-1. Elementary constants.                              **$
%** 3-2. Routines for saving constants.                     **$
%** 4-1. Elementary functions.                              **$
%** 5-1. Appendix: routines for defining infix operators.   **$
%**                                                         **$
%*************************************************************$
%*************************************************************$




%*************************************************************$
%*************************************************************$
%**                                                         **$
%** 1-1. Initialization.                                    **$
%**                                                         **$
%*************************************************************$
%*************************************************************$



SYMBOLIC$                 % Mode ====> SYMBOLIC mode $
GLOBAL '(!:PREC!:)$       % For the global precision $
%!:PREC!: := NIL$          % Default value of !:PREC!:$




%*************************************************************$
%*************************************************************$
%**                                                         **$
%** 1-2. CONSTRUCTOR, SELECTORS and basic PREDICATE.        **$
%**                                                         **$
%*************************************************************$
%*************************************************************$



%*************************************************************$
 SYMBOLIC SMACRO PROCEDURE MAKE!:BF(MT,EP); %****************$

   %========================================================$
   % This function constructs an internal representation of $
   %      a number "n" composed of the mantissa MT and the  $
   %      exponent EP with the base 10.  The magnitude of   $
   %      the number thus constructed is hence MT*10**EP.   $
   % **** CAUTION!  MT and EP are integers.  So, EP denotes $
   % ****      the order of the last figure in "n", where   $
   % ****      ORDER(n)=k if 10**k <= ABS(n) < 10**(k+1),   $
   % ****      with the exception ORDER(0)=0.               $
   % The number "n" is said to be of precision "k" if its   $
   %      mantissa is a k-figure number.                    $
   % MT and EP are any integers (positive or negative).  So,$
   %      you can handle any big or small numbers.  In this $
   %      sense, "BF" denotes a BIG-FLOATING-POINT number.  $
   %      Hereafter, an internal representation of a number $
   %      constructed by MAKE!:BF is referred to as a       $
   %      BIG-FLOAT representation.                         $
   %========================================================$

          CONS('!:BF!: , CONS(MT,EP))$



%*************************************************************$
 SYMBOLIC PROCEDURE BFP!:(X); %******************************$

   %==============================================$
   % This function returns T if X is a BIG-FLOAT  $
   %      representation, else it returns NIL.    $
   % X is any LISP entity.                        $
   %==============================================$

          IF ATOM(X) THEN NIL ELSE
          IF CAR(X) EQ '!:BF!: THEN T ELSE NIL$



%*************************************************************$
 SYMBOLIC SMACRO PROCEDURE MT!:(NMBR); %*********************$

   %====================================================$
   % This function selects the mantissa of a number "n".$
   % NMBR is a BIG-FLOAT representation of "n".         $
   %====================================================$

          CADR(NMBR)$



%*************************************************************$
 SYMBOLIC SMACRO PROCEDURE EP!:(NMBR); %*********************$

   %====================================================$
   % This function selects the exponent of a number "n".$
   % NMBR is a BIG-FLOAT representation of "n".         $
   %====================================================$

          CDDR(NMBR)$




%*************************************************************$
%*************************************************************$
%**                                                         **$
%** 1-3. Temporary routines for rational number arithmetic. **$
%**                                                         **$
%*************************************************************$
%*************************************************************$



%*************************************************************$
 SYMBOLIC PROCEDURE MAKE!:RATNUM(NM,DN); %*******************$

   %=====================================================$
   % This function constructs an internal representation $
   %      of a rational number composed of the numerator $
   %      NM and the denominator DN.                     $
   % NM and DN are any integers (positive or negative).  $
   % **** Four routines in this section are temporary.   $
   % ****      That is, if your system has own routines  $
   % ****      for rational number arithmetic, you can   $
   % ****      accommodate our system to yours only by   $
   % ****      redefining these four routines.           $
   %=====================================================$

	  IF DN=0 THEN REDERR
	     ("ZERO DENOMINATOR IN MAKE!:RATNUM") ELSE
          IF DN>0 THEN CONS('!:RATNUM!: , CONS( NM, DN))
          ELSE         CONS('!:RATNUM!: , CONS(-NM,-DN))$



%*************************************************************$
 SYMBOLIC PROCEDURE RATNUMP!:(X); %**************************$

   %===================================================$
   % This function returns T if X is a rational number $
   %      representation, else it returns NIL.         $
   % X is any LISP entity.                             $
   %===================================================$

          IF ATOM(X) THEN NIL ELSE
          IF CAR(X) EQ '!:RATNUM!: THEN T ELSE NIL$



%*************************************************************$
 SYMBOLIC SMACRO PROCEDURE NUMR!:(RNMBR); %******************$

   %===================================================$
   % This function selects the numerator of a rational $
   %      number "n".                                  $
   % RNMBR is a rational number representation of "n". $
   %===================================================$

          CADR(RNMBR)$



%*************************************************************$
 SYMBOLIC SMACRO PROCEDURE DENM!:(RNMBR); %******************$

   %=====================================================$
   % This function selects the denominator of a rational $
   %      number "n".                                    $
   % RNMBR is a rational number representation of "n".   $
   %=====================================================$

          CDDR(RNMBR)$




%*************************************************************$
%*************************************************************$
%**                                                         **$
%** 1-4. COUNTERS.                                          **$
%**                                                         **$
%*************************************************************$
%*************************************************************$



%*************************************************************$
 SYMBOLIC SMACRO PROCEDURE PRECI!:(NMBR); %******************$

   %====================================================$
   % This function counts the precision of a number "n".$
   % NMBR is a BIG-FLOAT representation of "n".         $
   %====================================================$

          LENGTH( EXPLODE( ABS( MT!:(NMBR))))$



%*************************************************************$
 SYMBOLIC PROCEDURE ORDER!:(NMBR); %*************************$

   %================================================$
   % This function counts the order of a number "n".$
   % NMBR is a BIG-FLOAT representation of "n".     $
   % **** ORDER(n)=k if 10**k <= ABS(n) < 10**(k+1) $
   % ****     when n is not 0, and ORDER(0)=0.      $
   %================================================$

          IF MT!:(NMBR)=0 THEN 0
          ELSE PRECI!:(NMBR) + EP!:(NMBR) - 1$




%*************************************************************$
%*************************************************************$
%**                                                         **$
%** 1-5. Routines for converting the numeric type.          **$
%**                                                         **$
%*************************************************************$
%*************************************************************$



%*************************************************************$
 SYMBOLIC PROCEDURE CONV!:A2BF(N); %*************************$

   %======================================================$
   % This function converts a number N or a number-like   $
   %      entity N to a <BIG-FLOAT>, i.e., a BIG-FLOAT    $
   %      representation of N.                            $
   % N is either an integer, a floating-point number,     $
   %      a string representing a number, a rational      $
   %      number, or a <BIG-FLOAT>.                       $
   % **** This function is the most general conversion    $
   % ****      function to get a BIG-FLOAT representation.$
   % ****      In this sense, A means an Arbitrary number.$
   % **** A rational number is converted to a <BIG-FLOAT> $
   % ****      of precision !:PREC!: if !:PREC!: is not   $
   % ****      NIL, else the precision is set 50.         $
   %======================================================$

          IF BFP!:(N)     THEN N             ELSE
          IF FIXP(N)      THEN MAKE!:BF(N,0) ELSE
          IF FLOATP(N)    THEN READ!:NUM(N)  ELSE
          IF STRINGP(N)   THEN READ!:NUM(N)  ELSE
          IF RATNUMP!:(N) THEN CONV!:R2BF(N ,
                        (IF !:PREC!: THEN !:PREC!: ELSE 50) )
	  ELSE BFLERRMSG 'CONV!:A2BF$



%*************************************************************$
 SYMBOLIC PROCEDURE CONV!:F2BF(FNMBR); %*********************$

   %================================================$
   % This function converts a floating-point number $
   %      FNMBR to a <BIG-FLOAT>, i.e., a BIG-FLOAT $
   %      representation.                           $
   % FNMBR is a floating-point number.              $
   % **** CAUSION!. If you input a number, say, 0.1,$
   % ****      some systems do not accept it as 0.1 $
   % ****      but may accept it as 0.09999999.     $
   % ****      In such a case, you had better use   $
   % ****      CONV!:S2BF than to use CONV!:F2BF.   $
   %================================================$

          IF FLOATP(FNMBR) THEN READ!:NUM(FNMBR)
	  ELSE BFLERRMSG 'CONV!:F2BF$



%*************************************************************$
 SYMBOLIC PROCEDURE CONV!:I2BF(INTGR); %*********************$

   %====================================================$
   % This function converts an integer INTGR to a <BIG- $
   %      FLOAT>, i.e., a BIG-FLOAT representation.     $
   % INTGR is an integer.                               $
   %====================================================$

          IF FIXP(INTGR) THEN MAKE!:BF(INTGR,0)
	  ELSE BFLERRMSG 'CONV!:I2BF$



%*************************************************************$
 SYMBOLIC PROCEDURE CONV!:R2BF(RNMBR,K); %*******************$

   %=====================================================$
   % This function converts a rational number RNMBR to a $
   %      <BIG-FLOAT> of precision K, i.e., a BIG-FLOAT  $
   %      representation with a given precision.         $
   % RNMBR is a rational number representation.          $
   % K is a positive integer.                            $
   %=====================================================$

          IF RATNUMP!:(RNMBR) AND FIXP(K) AND K>0 THEN
             DIVIDE!:( MAKE!:BF( NUMR!:(RNMBR),0) ,
                       MAKE!:BF( DENM!:(RNMBR),0) , K)
	  ELSE BFLERRMSG 'CONV!:R2BF$



%*************************************************************$
 SYMBOLIC PROCEDURE CONV!:S2BF(STRNG); %*********************$

   %==============================================$
   % This function converts a string representing $
   %      a number "n" to a <BIG-FLOAT>, i.e.,    $
   %      a BIG-FLOAT representation.             $
   % STRNG is a string representing "n".  "n" may $
   %      be an integer, a floating-point number  $
   %      of any precision, or a rational number. $
   % **** CAUTION!  Some systems may set the      $
   % ****           maximum size of string.       $
   %==============================================$

          IF STRINGP(STRNG) THEN READ!:NUM(STRNG)
	  ELSE BFLERRMSG 'CONV!:S2BF$



%*************************************************************$
 SYMBOLIC PROCEDURE CONV!:BF2F(NMBR); %**********************$

   %=========================================================$
   % This function converts a <BIG-FLOAT>, i.e., a BIG-FLOAT $
   %      representation of "n", to a floating-point number. $
   % NMBR is a BIG-FLOAT representation of the number "n".   $
   %=========================================================$

          IF BFP!:(NMBR) THEN
             TIMES( FLOAT( MT!:(NMBR)) ,
                    FLOAT( EXPT(10 , EP!:(NMBR))) )
	  ELSE BFLERRMSG 'CONV!:BF2F$



%*************************************************************$
 SYMBOLIC PROCEDURE CONV!:BF2I(NMBR); %**********************$

   %=========================================================$
   % This function converts a <BIG-FLOAT>, i.e., a BIG-FLOAT $
   %      representation of "n", to an integer.  The result  $
   %      is the integer part of "n".                        $
   % **** For getting the nearest integer to "n", please use $
   % ****      the combination MT!:( CONV!:EP(NMBR,0)).      $
   % NMBR is a BIG-FLOAT representation of the number "n".   $
   %=========================================================$

          IF BFP!:(NMBR) THEN
             IF EP!:(NMBR:=CUT!:EP(NMBR,0)) = 0 THEN MT!:(NMBR)
             ELSE MT!:(NMBR)*EXPT(10 , EP!:(NMBR))
	  ELSE BFLERRMSG 'CONV!:BF2I$



%*************************************************************$
 SYMBOLIC PROCEDURE CONV!:BF2R(NMBR); %**********************$

   %=========================================================$
   % This function converts a <BIG-FLOAT>, i.e., a BIG-FLOAT $
   %      representation of "n", to a rational number.       $
   % NMBR is a BIG-FLOAT representation of "n".              $
   % **** The numerator and the denominator of the result    $
   % ****      have no common divisor.                       $
   %=========================================================$

          IF BFP!:(NMBR) THEN
    BEGIN INTEGER NN,ND,M,N,Q;
          IF (Q:=EP!:(NMBR)) >= 0 THEN
               <<NN:=MT!:(NMBR)*EXPT(10,Q); ND:=1; M:=1>>
          ELSE <<NN:=MT!:(NMBR); ND:=EXPT(10,-Q);
                 IF ABS(NN) > ABS(ND) THEN <<M:=NN; N:=ND>>
                 ELSE <<M:=ND; N:=NN>>;
                 WHILE NOT(N=0) DO
                       <<Q:=REMAINDER(M,N); M:=N; N:=Q>> >>;
          RETURN MAKE!:RATNUM( NN/M , ND/M);
    END
	  ELSE BFLERRMSG 'CONV!:BF2R$




%*************************************************************$
%*************************************************************$
%**                                                         **$
%** 1-6. Routines for converting a BIG-FLOAT number.        **$
%**                                                         **$
%*************************************************************$
%*************************************************************$



%*************************************************************$
 SYMBOLIC PROCEDURE DECPREC!:(NMBR,K); %*********************$

   %======================================================$
   % This function converts a number "n" to an equivalent $
   %      number the precision of which is decreased by K.$
   % **** CAUTION!  No rounding is made.                  $
   % NMBR is a BIG-FLOAT representation of "n".           $
   % K is a positive integer.                             $
   %======================================================$

          MAKE!:BF( MT!:(NMBR)/EXPT(10,K) , EP!:(NMBR)+K)$



%*************************************************************$
 SYMBOLIC PROCEDURE INCPREC!:(NMBR,K); %*********************$

   %======================================================$
   % This function converts a number "n" to an equivalent $
   %      number the precision of which is increased by K.$
   % **** CAUTION!  No rounding is made.                  $
   % NMBR is a BIG-FLOAT representation of "n".           $
   % K is a positive integer.                             $
   %======================================================$

          MAKE!:BF( MT!:(NMBR)*EXPT(10,K) , EP!:(NMBR)-K)$



%*************************************************************$
 SYMBOLIC PROCEDURE CONV!:MT(NMBR,K); %**********************$

   %===========================================$
   % This function converts a number "n" to an $
   %      equivalent number of precision K by  $
   %      rounding "n" or adding "0"s to "n".  $
   % NMBR is a BIG-FLOAT representation of "n".$
   % K is a positive integer.                  $
   %===========================================$

          IF BFP!:(NMBR) AND FIXP(K) AND K>0 THEN
             IF (K:=PRECI!:(NMBR)-K) = 0 THEN NMBR
             ELSE IF K<0 THEN INCPREC!:(NMBR,-K)
                  ELSE ROUND!:LAST( DECPREC!:(NMBR,K-1))
	  ELSE BFLERRMSG 'CONV!:MT$



%*************************************************************$
 SYMBOLIC PROCEDURE CONV!:EP(NMBR,K); %**********************$

   %==============================================$
   % This function converts a number "n" to an    $
   %      equivalent number having the exponent K $
   %      by rounding "n" or adding "0"s to "n".  $
   % NMBR is a BIG-FLOAT representation of "n".   $ 
   % K is an integer (positive or negative).      $
   %==============================================$

          IF BFP!:(NMBR) AND FIXP(K) THEN
             IF (K:=K-EP!:(NMBR)) = 0 THEN NMBR
             ELSE IF K<0 THEN INCPREC!:(NMBR,-K)
                  ELSE ROUND!:LAST( DECPREC!:(NMBR,K-1))
	  ELSE BFLERRMSG 'CONV!:EP$



%*************************************************************$
 SYMBOLIC PROCEDURE CUT!:MT(NMBR,K); %***********************$

   %======================================================$
   % This function returns a given number "n" unchanged   $
   %      if its precision is not greater than K, else it $
   %      cuts off its mantissa at the (K+1)th place and  $
   %      returns an equivalent number of precision K.    $
   % **** CAUTION!  No rounding is made.                  $
   % NMBR is a BIG-FLOAT representation of "n".           $
   % K is a positive integer.                             $
   %======================================================$

          IF BFP!:(NMBR) AND FIXP(K) AND K>0 THEN
             IF (K:=PRECI!:(NMBR)-K) <= 0 THEN NMBR
             ELSE DECPREC!:(NMBR,K)
	  ELSE BFLERRMSG 'CUT!:MT$



%*************************************************************$
 SYMBOLIC PROCEDURE CUT!:EP(NMBR,K); %***********************$

   %======================================================$
   % This function returns a given number "n" unchanged   $
   %      if its exponent is not less than K, else it     $
   %      cuts off its mantissa and returns an equivalent $
   %      number of exponent K.                           $
   % **** CAUTION!  No rounding is made.                  $
   % NMBR is a BIG-FLOAT representation of "n".           $
   % K is an integer (positive or negative).              $
   %======================================================$

          IF BFP!:(NMBR) AND FIXP(K) THEN
             IF (K:=K-EP!:(NMBR)) <= 0 THEN NMBR
             ELSE DECPREC!:(NMBR,K)
	  ELSE BFLERRMSG 'CUT!:EP$



%*************************************************************$
 SYMBOLIC PROCEDURE MATCH!:(N1,N2); %************************$

   %==========================================================$
   % This function converts either "n1" or "n2" so that they  $
   %      have the same exponent, which is the smaller of     $
   %      the exponents of "n1" and "n2".                     $
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2".$
   % **** CAUTION!  Using this function, one of the previous  $
   % ****           expressions of "n1" and "n2" is lost.     $
   %==========================================================$

          IF BFP!:(N1) AND BFP!:(N2) THEN
    BEGIN INTEGER E1,E2;  SCALAR N;
          IF (E1:=EP!:(N1)) = (E2:=EP!:(N2)) THEN RETURN T;
          IF E1>E2 THEN <<RPLACA(N1 , CAR(N:=CONV!:EP(N1,E2)));
                          RPLACD(N1 , CDR(N)) >>
          ELSE          <<RPLACA(N2 , CAR(N:=CONV!:EP(N2,E1)));
                          RPLACD(N2 , CDR(N)) >>;  RETURN T;
    END
	  ELSE BFLERRMSG 'MATCH!:$



%*************************************************************$
 SYMBOLIC PROCEDURE ROUND!:MT(NMBR,K); %*********************$

   %========================================================$
   % This function rounds a number "n" at the (K+1)th place $
   %      and returns an equivalent number of precision K   $
   %      if the precision of "n" is greater than K, else   $
   %      it returns the given number unchanged.            $
   % NMBR is a BIG-FLOAT representation of "n".             $
   % K is a positive integer.                               $
   %========================================================$

          IF BFP!:(NMBR) AND FIXP(K) AND K>0 THEN
             IF (K:=PRECI!:(NMBR)-K-1) < 0 THEN NMBR
             ELSE IF K=0 THEN ROUND!:LAST(NMBR)
                  ELSE ROUND!:LAST( DECPREC!:(NMBR,K))
	  ELSE BFLERRMSG 'ROUND!:MT$



%*************************************************************$
 SYMBOLIC PROCEDURE ROUND!:EP(NMBR,K); %*********************$

   %==================================================$
   % This function rounds a number "n" and returns an $
   %      equivalent number having the exponent K if  $
   %      the exponent of "n" is less than K, else    $
   %      it returns the given number unchanged.      $
   % NMBR is a BIG-FLOAT representation of "n".       $
   % K is an integer (positive or negative).          $
   %==================================================$

          IF BFP!:(NMBR) AND FIXP(K) THEN
             IF (K:=K-1-EP!:(NMBR)) < 0 THEN NMBR
             ELSE IF K=0 THEN ROUND!:LAST(NMBR)
                  ELSE ROUND!:LAST( DECPREC!:(NMBR,K))
	  ELSE BFLERRMSG 'ROUND!:EP$



%*************************************************************$
 SYMBOLIC PROCEDURE ROUND!:LAST(NMBR); %*********************$

   %=====================================================$
   % This function rounds a number "n" at its last place.$
   % NMBR is a BIG-FLOAT representation of "n".          $
   %=====================================================$

    BEGIN SCALAR N;
	  N := DIVIDE(ABS(MT!:(NMBR)),10);
	  IF CDR N<5 THEN N := CAR N ELSE N := CAR N+1;
          IF MT!:(NMBR) < 0 THEN N := -N;
          RETURN MAKE!:BF(N , EP!:(NMBR)+1);
    END$




%*************************************************************$
%*************************************************************$
%**                                                         **$
%** 1-7. Routines for reading/printing numbers.             **$
%**                                                         **$
%*************************************************************$
%*************************************************************$



%*************************************************************$
 SYMBOLIC PROCEDURE READ!:LNUM(L); %*************************$

   %=======================================================$
   % This function reads a long number "n" represented by  $
   %      a list in a way described below, and constructs  $
   %      a BIG-FLOAT representation of "n".               $
   % **** Using this function, you can input any long      $
   % ****      floating-point numbers without difficulty.  $
   % L is a list of integers, the first element of which   $  
   %      gives the order of "n" and all the next elements $
   %      when concatenated give the mantissa of "n".      $
   % **** ORDER(n)=k if 10**k <= ABS(n) < 10**(k+1).       $
   % **** Except for the first element, all integers in L  $
   % ****      should not begin with "0" because some      $
   % ****      systems suppress leading zeros.             $
   %=======================================================$

	  IF MEMBER(NIL , MAPCAR(L,'FIXP)) THEN BFLERRMSG
	    'READ!:LNUM ELSE

    BEGIN INTEGER MT,EP,K,SIGN;  SCALAR U,V;

          MT:=0;
          EP:=CAR( U:=L)+1;
          IF CADR(L)>0 THEN SIGN:=1 ELSE SIGN:=-1;
          WHILE U:=CDR(U) DO
            <<V:=EXPLODE( ABS( CAR(U))); K:=0;
              WHILE V DO <<K:=K+1; V:=CDR(V) >>;
              MT:=MT*EXPT(10,K)+ABS( CAR(U)); EP:=EP-K>>;
          RETURN MAKE!:BF(SIGN*MT,EP);
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE READ!:NUM(N); %**************************$

   %========================================================$
   % This function reads a number or a number-like entity N $
   %      and constructs a BIG-FLOAT representation of it.  $
   % N is an integer, a floating-point number, or a string  $
   %      representing a number.                            $
   % **** If the system does not accept or may incorrectly  $
   % ****      accept the floating-point numbers, you can   $
   % ****      input them as strings such as "1.234E-56",   $
   % ****      "-78.90 D+12" , "+3456 B -78", or "901/234". $
   % **** A rational number in a string form is converted   $
   % ****      to a <BIG-FLOAT> of precision !:PREC!: if    $
   % ****      !:PREC!: is not NIL, else the precision of   $
   % ****      the result is set 50.                        $
   % **** Some systems set the maximum size of strings.  If $
   % ****      you want to input long numbers exceeding     $
   % ****      such a maximum size, please use READ!:LNUM.  $
   %========================================================$

          IF FIXP(N) THEN MAKE!:BF(N,0) ELSE
	  IF NOT( NUMBERP(N) OR STRINGP(N)) THEN BFLERRMSG
	     'READ!:NUM ELSE

    BEGIN INTEGER J,M,SIGN;  SCALAR CH,U,V,L,APPEAR!.,APPEAR!/;

          J:=M:=0;
          SIGN:=1;
          U:=V:=APPEAR!.:=APPEAR!/:=NIL;
          L:=EXPLODE(N);

    LOOP: CH:=CAR(L);
          IF DIGIT(CH) THEN <<U:=CONS(CH,U); J:=J+1>> ELSE
          IF CH EQ '!. THEN <<APPEAR!.:=T  ; J:=0  >> ELSE
          IF CH EQ '!/ THEN <<APPEAR!/:=T; V:=U; U:=NIL>> ELSE
          IF CH EQ '!- THEN SIGN:=-1 ELSE
	  IF CH EQ 'E OR CH EQ 'D OR CH EQ 'B
	     OR CH EQ '!e OR CH EQ '!d OR CH EQ '!b THEN GO TO JUMP;
    ENDL: IF L:=CDR(L) THEN GOTO LOOP ELSE GOTO MAKE;
    JUMP: WHILE L:=CDR(L) DO
            <<IF DIGIT( CH:=CAR(L)) OR CH EQ '!-
                 THEN V:=CONS(CH,V) >>;
          L:=REVERSE(V);
          IF CAR(L) EQ '!- THEN M:=-COMPRESS( CDR(L))
          ELSE                  M:= COMPRESS(L);

    MAKE: U:=REVERSE(U);
          V:=REVERSE(V);
          IF APPEAR!/ THEN RETURN CONV!:R2BF
             ( MAKE!:RATNUM( SIGN*COMPRESS(V) , COMPRESS(U)) ,
               (IF !:PREC!: THEN !:PREC!: ELSE 50) );
          IF APPEAR!. THEN J:=-J ELSE J:=0;
          IF SIGN=1 THEN U:=COMPRESS(U) ELSE U:=-COMPRESS(U);
          RETURN MAKE!:BF(U,J+M);
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE PRINT!:BF(NMBR,TYPE); %******************$

   %==========================================================$
   % This function prints a number "n" in the print-type TYPE.$
   % NMBR is a BIG-FLOAT representation of "n".               $
   % TYPE is either 'N, 'I, 'E, 'F, 'L, 'R, meaning as:       $
   %      TYPE='N ... the internal representation is printed. $
   %      TYPE='I ... the integer part is printed.            $
   %      TYPE='E ... <mantissa in form *.***>E<exponent>.    $
   %      TYPE='F ... <integer part>.<decimal part>.          $
   %      TYPE='L ... in a list form readable by READ!:LNUM.  $
   %      TYPE='R ... printed as a rational number.           $
   % **** The number is printed by being inserted a blank     $
   % ****      after each five characters.  Therefore, you    $
   % ****      can not use the printed numbers as input data, $
   % ****      except when they are printed in type 'L.       $
   %==========================================================$

          IF NOT( TYPE EQ 'N OR TYPE EQ 'I OR TYPE EQ 'E OR
                  TYPE EQ 'F OR TYPE EQ 'L OR TYPE EQ 'R)
	     OR NOT( BFP!:(NMBR)) THEN BFLERRMSG 'PRINT!:BF ELSE

    BEGIN INTEGER J,K;  SCALAR U,V;

          IF ZEROP!:(NMBR) THEN NMBR:=MAKE!:BF(0,0);
          IF TYPE EQ 'I THEN GOTO ITYPE ELSE
          IF TYPE EQ 'E THEN GOTO ETYPE ELSE
          IF TYPE EQ 'F THEN GOTO FTYPE ELSE
          IF TYPE EQ 'L THEN GOTO LTYPE ELSE
          IF TYPE EQ 'R THEN GOTO RTYPE;

   NTYPE: PRINT(NMBR);
          RETURN T;

   ITYPE: U:=EXPLODE( CONV!:BF2I(NMBR));
          J:=0;
          WHILE U DO <<PRIN2( CAR(U)); U:=CDR(U); J:=J+1;
                       IF J=5 THEN <<PRIN2(" "); J:=0>> >>;
          TERPRI();  RETURN T;

   ETYPE: U:=EXPLODE( ABS( J:=MT!:(NMBR)));
          IF NULL( CDR(U)) THEN RPLACD(U , LIST(0));
          IF J>=0 THEN U:=CONS( CAR(U) , CONS('!. , CDR(U)))
          ELSE U:=CONS('!- , CONS( CAR(U) , CONS('!.,CDR(U))));
          J:=0;
          WHILE U DO <<PRIN2( CAR(U)); U:=CDR(U); J:=J+1;
                       IF J=5 THEN <<PRIN2(" "); J:=0>> >>;
          IF J=0 THEN <<PRIN2("E "  ); J:=2>> ELSE
          IF J=1 THEN <<PRIN2(" E " ); J:=4>> ELSE
          IF J=2 THEN <<PRIN2(" E  "); J:=0>> ELSE
          IF J=3 THEN <<PRIN2(" E " ); J:=0>> ELSE
          IF J=4 THEN <<PRIN2("  E "); J:=2>>;
          U:=EXPLODE( K:=ORDER!:(NMBR));
          IF K>=0 THEN U:=CONS('!+,U);
          WHILE U DO <<PRIN2( CAR(U)); U:=CDR(U); J:=J+1;
                       IF J=5 THEN <<PRIN2(" "); J:=0>> >>;
          TERPRI();  RETURN T;

   FTYPE: U:=EXPLODE( ABS( MT!:(NMBR)));
          IF (J:=EP!:(NMBR)) >= 0 THEN
               <<V:=NIL; WHILE (J:=J-1)>=0 DO V:=CONS(0,V);
                 U:=NCONC(U,V) >>  ELSE
          IF (J:=ORDER!:(NMBR)+1) > 0 THEN
               <<V:=U; WHILE (J:=J-1)>0 DO V:=CDR(V);
                 RPLACD(V , CONS('!.,CDR(V))) >>
          ELSE <<WHILE (J:=J+1)<=0 DO U:=CONS(0,U);
                 U:=CONS(0 , CONS('!.,U)) >>;
          IF MT!:(NMBR) < 0 THEN U:=CONS('!-,U);
          J:=0;
          WHILE U DO <<PRIN2( CAR(U)); U:=CDR(U); J:=J+1;
                       IF J=5 THEN <<PRIN2(" "); J:=0>> >>;
          TERPRI();  RETURN T;

   LTYPE: PRIN2(" '(");
          PRIN2( ORDER!:(NMBR));
          PRIN2("  ");
          U:=EXPLODE( MT!:(NMBR));
          J:=0;
          WHILE U DO <<PRIN2( CAR(U)); U:=CDR(U); J:=J+1;
                       IF J>=5 AND U AND NOT( CAR(U) EQ '!0)
                          THEN <<PRIN2(" "); J:=J-5>> >>;
          PRIN2(")");  TERPRI();  RETURN T;

   RTYPE: PRINT!:RATNUM( CONV!:BF2R(NMBR));
          RETURN T;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE PRINT!:RATNUM(RNMBR); %******************$

   %======================================================$
   % This function prints a rational number "n".          $
   % RNMBR is a rational number representation of "n".    $
   % **** The number is printed by being inserted a blank $
   % ****      after each five characters.  So, you can   $
   % ****      not use the printed numbers as input data. $
   %======================================================$

	  IF NOT( RATNUMP!:(RNMBR)) THEN BFLERRMSG 'PRINT!:RATNUM ELSE

    BEGIN INTEGER J;  SCALAR U,V;

          U:=NUMR!:(RNMBR);
          V:=DENM!:(RNMBR);
          IF V<0 THEN <<U:=-U; V:=-V>>;
          J:=0;
          U:=EXPLODE(U);
          WHILE U DO <<PRIN2( CAR(U)); U:=CDR(U); J:=J+1;
                       IF J=5 THEN <<PRIN2(" "); J:=0>> >>;
          IF J=0 THEN <<PRIN2("/ "  ); J:=2>> ELSE
          IF J=1 THEN <<PRIN2(" / " ); J:=4>> ELSE
          IF J=2 THEN <<PRIN2(" /  "); J:=0>> ELSE
          IF J=3 THEN <<PRIN2(" / " ); J:=0>> ELSE
          IF J=4 THEN <<PRIN2("  / "); J:=2>>;
          V:=EXPLODE(V);
          WHILE V DO <<PRIN2( CAR(V)); V:=CDR(V); J:=J+1;
                       IF J=5 THEN <<PRIN2(" "); J:=0>> >>;
          TERPRI();  RETURN T;
    END$




%*************************************************************$
%*************************************************************$
%**                                                         **$
%** 2-1. Arithmetic manipulation routines.                  **$
%**                                                         **$
%*************************************************************$
%*************************************************************$



%*************************************************************$
 SYMBOLIC PROCEDURE ABS!:(NMBR); %***************************$

   %===============================================$
   % This function makes the absolute value of "n".$
   % N is a BIG-FLOAT representation of "n".       $
   %===============================================$

          IF MT!:(NMBR) > 0 THEN NMBR
          ELSE MAKE!:BF( -MT!:(NMBR) , EP!:(NMBR))$



%*************************************************************$
 SYMBOLIC PROCEDURE MINUS!:(NMBR); %*************************$

   %=============================================$
   % This function makes the minus number of "n".$
   % N is a BIG-FLOAT representation of "n".     $
   %=============================================$

          MAKE!:BF( -MT!:(NMBR) , EP!:(NMBR))$



%*************************************************************$
 SYMBOLIC PROCEDURE PLUS!:(N1,N2); %*************************$

   %==========================================================$
   % This function calculates the sum of "n1" and "n2".       $
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2".$
   %==========================================================$

    BEGIN INTEGER E1,E2;
          IF (E1:=EP!:(N1)) = (E2:=EP!:(N2)) THEN RETURN
             MAKE!:BF( MT!:(N1)+MT!:(N2) , E1)
          ELSE IF E1>E2 THEN RETURN MAKE!:BF
                  ( MT!:( INCPREC!:(N1,E1-E2))+MT!:(N2) , E2)
               ELSE RETURN MAKE!:BF
                  ( MT!:(N1)+MT!:( INCPREC!:(N2,E2-E1)) , E1);
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE DIFFERENCE!:(N1,N2); %*******************$

   %==========================================================$
   % This function calculates the difference of "n1" and "n2".$
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2".$
   %==========================================================$

    BEGIN INTEGER E1,E2;
          IF (E1:=EP!:(N1)) = (E2:=EP!:(N2)) THEN RETURN
             MAKE!:BF( MT!:(N1)-MT!:(N2) , E1)
          ELSE IF E1>E2 THEN RETURN MAKE!:BF
                  ( MT!:( INCPREC!:(N1,E1-E2))-MT!:(N2) , E2)
               ELSE RETURN MAKE!:BF
                  ( MT!:(N1)-MT!:( INCPREC!:(N2,E2-E1)) , E1);
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE TIMES!:(N1,N2); %************************$

   %==========================================================$
   % This function calculates the product of "n1" and "n2".   $
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2".$
   %==========================================================$

          MAKE!:BF( MT!:(N1)*MT!:(N2) , EP!:(N1)+EP!:(N2))$



%*************************************************************$
 SYMBOLIC PROCEDURE DIVIDE!:(N1,N2,K); %*********************$

   %==========================================================$
   % This function calculates the quotient of "n1" and "n2",  $
   %      with the precision K, by rounding the ratio of "n1" $
   %      and "n2" at the (K+1)th place.                      $
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2".$
   % K is any positive integer.                               $
   %==========================================================$

    BEGIN
          N1:=CONV!:MT(N1 , K+PRECI!:(N2)+1);
          N1:=MAKE!:BF( MT!:(N1)/MT!:(N2) , EP!:(N1)-EP!:(N2));
          RETURN ROUND!:MT(N1,K);
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE EXPT!:(NMBR,K); %************************$

   %===============================================$
   % This function calculates the Kth power of "n".$
   %      The result will become a long number if  $
   %      ABS(K) >> 1.                             $
   % NMBR is a BIG-FLOAT representation of "n".    $
   % K is an integer (positive or negative).       $
   % **** For calculating a power X**K, with non-  $ 
   % ****      integer K, please use TEXPT!:ANY.   $
   %===============================================$

          IF K>=0 THEN
             MAKE!:BF( EXPT( MT!:(NMBR) , K) , EP!:(NMBR)*K)
          ELSE DIVIDE!:( MAKE!:BF(1,0) , EXPT!:(NMBR,-K) ,
                                        -PRECI!:(NMBR)*K)$



%*************************************************************$
 SYMBOLIC PROCEDURE TPLUS!:(N1,N2); %************************$

   %==========================================================$
   % This function calculates the sum of "n1" and "n2"        $
   %      up to a precision specified by !:PREC!: or N1 or N2.$
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2",$
   %      otherwise they are converted to <BIG-FLOAT>'s.      $
   %==========================================================$

          IF BFP!:( N1:=CONV!:A2BF(N1)) AND
             BFP!:( N2:=CONV!:A2BF(N2)) THEN ROUND!:MT
             ( PLUS!:(N1,N2) , (IF !:PREC!: THEN !:PREC!:
                    ELSE MAX( PRECI!:(N1) , PRECI!:(N2))) )
	  ELSE BFLERRMSG 'TPLUS!:$



%*************************************************************$
 SYMBOLIC PROCEDURE TDIFFERENCE!:(N1,N2); %******************$

   %==========================================================$
   % This function calculates the difference of "n1" and "n2" $
   %      up to a precision specified by !:PREC!: or N1 or N2.$
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2",$
   %      otherwise they are converted to <BIG-FLOAT>'s.      $
   %==========================================================$

          IF BFP!:( N1:=CONV!:A2BF(N1)) AND
             BFP!:( N2:=CONV!:A2BF(N2)) THEN ROUND!:MT
             ( DIFFERENCE!:(N1,N2) , (IF !:PREC!: THEN !:PREC!:
                        ELSE MAX( PRECI!:(N1) , PRECI!:(N2))) )
	  ELSE BFLERRMSG 'TDIFFERENCE!:$



%*************************************************************$
 SYMBOLIC PROCEDURE TTIMES!:(N1,N2); %***********************$

   %==========================================================$
   % This function calculates the product of "n1" and "n2"    $
   %      up to a precision specified by !:PREC!: or N1 or N2.$
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2",$
   %      otherwise they are converted to <BIG-FLOAT>'s.      $
   %==========================================================$

          IF BFP!:( N1:=CONV!:A2BF(N1)) AND
             BFP!:( N2:=CONV!:A2BF(N2)) THEN ROUND!:MT
             ( TIMES!:(N1,N2) , (IF !:PREC!: THEN !:PREC!:
                     ELSE MAX( PRECI!:(N1) , PRECI!:(N2))) )
	  ELSE BFLERRMSG 'TTIMES!:$



%*************************************************************$
 SYMBOLIC PROCEDURE TDIVIDE!:(N1,N2); %**********************$

   %==========================================================$
   % This function calculates the quotient of "n1" and "n2"   $
   %      up to a precision specified by !:PREC!: or N1 or N2.$
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2",$
   %      otherwise they are converted to <BIG-FLOAT>'s.      $
   %==========================================================$

          IF BFP!:( N1:=CONV!:A2BF(N1)) AND
             BFP!:( N2:=CONV!:A2BF(N2)) THEN
             DIVIDE!:(N1 , N2 , (IF !:PREC!: THEN !:PREC!:
                      ELSE MAX( PRECI!:(N1) , PRECI!:(N2))) )
	  ELSE BFLERRMSG 'TDIVIDE!:$



%*************************************************************$
 SYMBOLIC PROCEDURE TEXPT!:(NMBR,K); %***********************$

   %=====================================================$
   % This function calculates the Kth power of "n" up to $
   %      the precision specified by !:PREC!: or NMBR.   $
   % NMBR is a BIG-FLOAT representation of "n",          $
   %      otherwise it is converted to a <BIG-FLOAT>.    $
   % K is an integer (positive or negative).             $
   % **** For calculating a power X**K, where K is not   $
   % ****      an integer, please use TEXPT!:ANY.        $
   %=====================================================$

          IF BFP!:( NMBR:=CONV!:A2BF(NMBR)) AND FIXP(K) THEN
             IF K=0 THEN MAKE!:BF(1,0) ELSE
             IF K=1 THEN NMBR ELSE
             IF K<0 THEN TDIVIDE!:( MAKE!:BF(1,0) ,
                                    TEXPT!:(NMBR,-K) )
             ELSE TEXPT!:CAL(NMBR , K , (IF !:PREC!: THEN
                              !:PREC!: ELSE PRECI!:(NMBR)) )
	  ELSE BFLERRMSG 'TEXPT!:$

    SYMBOLIC PROCEDURE TEXPT!:CAL(NMBR,K,PREC);
          IF K=1 THEN NMBR ELSE
    BEGIN INTEGER K2;  SCALAR U;
          U:=ROUND!:MT( TIMES!:(NMBR,NMBR) , PREC);
          IF K=2 THEN RETURN U ELSE
          IF (K-2*(K2:=K/2)) = 0 THEN RETURN
               TEXPT!:CAL(U,K2,PREC)
          ELSE RETURN ROUND!:MT
               ( TIMES!:(NMBR , TEXPT!:CAL(U,K2,PREC)) , PREC);
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE QUOTIENT!:(N1,N2); %*********************$

   %==========================================================$
   % This function calculates the integer quotient of "n1"    $
   %      and "n2", just as the "QUOTIENT" for integers does. $
   % **** For calculating the quotient up to a necessary      $
   % ****      precision, please use DIVIDE!:.                $
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2".$
   %==========================================================$

    BEGIN INTEGER E1,E2;
          IF (E1:=EP!:(N1)) = (E2:=EP!:(N2)) THEN RETURN
             MAKE!:BF( MT!:(N1)/MT!:(N2) , 0)
          ELSE IF E1>E2 THEN RETURN
                    QUOTIENT!:( INCPREC!:(N1,E1-E2) , N2)
               ELSE RETURN
                    QUOTIENT!:( N1 , INCPREC!:(N2,E2-E1));
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE REMAINDER!:(N1,N2); %********************$

   %==========================================================$
   % This function calculates the remainder of "n1" and "n2", $
   %      just as the "REMAINDER" for integers does.          $
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2".$
   %==========================================================$

    BEGIN INTEGER E1,E2;
          IF (E1:=EP!:(N1)) = (E2:=EP!:(N2)) THEN RETURN
             MAKE!:BF( REMAINDER( MT!:(N1) , MT!:(N2)) , E2)
          ELSE IF E1>E2 THEN RETURN
                    REMAINDER!:( INCPREC!:(N1,E1-E2) , N2)
               ELSE RETURN
                    REMAINDER!:( N1 , INCPREC!:(N2,E2-E1));
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE TEXPT!:ANY(X,Y); %***********************$

   %====================================================$
   % This function calculates the power x**y, where "x" $
   %      and "y" are any numbers.  The precision of    $
   %      the result is specified by !:PREC!: or X or Y.$
   % **** For a negative "x", this function returns     $
   % ****      -(-x)**y unless "y" is an integer.       $
   % X is a BIG-FLOAT representation of "x", otherwise  $
   %      it is converted to a <BIG-FLOAT>.             $
   % Y is either an integer, a floating-point number,   $
   %      or a BIG-FLOAT number, i.e., a BIG-FLOAT      $
   %      representation of "y".                        $
   %====================================================$

          IF FIXP(Y) THEN TEXPT!:(X,Y) ELSE
          IF INTEGERP!:(Y) THEN TEXPT!:(X , CONV!:BF2I(Y)) ELSE
          IF NOT( BFP!:( X:=CONV!:A2BF(X))) OR
	     NOT( BFP!:( Y:=CONV!:A2BF(Y))) THEN BFLERRMSG
		'TEXPT!:ANY ELSE
          IF MINUSP!:(Y) THEN TDIVIDE!:( MAKE!:BF(1,0) ,
                            TEXPT!:ANY(X , MINUS!:(Y)) ) ELSE

    BEGIN INTEGER N;  SCALAR XP,YP;

          N:=(IF !:PREC!: THEN !:PREC!:
              ELSE MAX( PRECI!:(X) , PRECI!:(Y)) );
          IF MINUSP!:(X) THEN XP:=MINUS!:(X) ELSE XP:=X;

          IF INTEGERP!:( TIMES!:(Y , CONV!:I2BF(2))) THEN
             <<XP:=INCPREC!:(XP,1);
               YP:=TEXPT!:(XP , CONV!:BF2I(Y));
               YP:=TIMES!:(YP , SQRT!:(XP,N+1)); 
               YP:=ROUND!:MT(YP,N) >>
          ELSE
             <<YP:=TTIMES!:(Y , LOG!:(XP,N+1));
               YP:=EXP!:(YP,N) >>;

          RETURN (IF MINUSP!:(X) THEN MINUS!:(YP) ELSE YP);
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE MAX!:(N1,N2); %**************************$

   %==========================================================$
   % This function returns the larger of "n1" and "n2".       $
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2".$
   %==========================================================$

          IF GREATERP!:(N2,N1) THEN N2 ELSE N1$



%*************************************************************$
 SYMBOLIC PROCEDURE MIN!:(N1,N2); %**************************$

   %==========================================================$
   % This function returns the smaller of "n1" and "n2".      $
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2".$
   %==========================================================$

          IF LESSP!:(N2,N1) THEN N2 ELSE N1$




%*************************************************************$
%*************************************************************$
%**                                                         **$
%** 2-2. Arithmetic predicates.                             **$
%**                                                         **$
%*************************************************************$
%*************************************************************$



%*************************************************************$
 SYMBOLIC PROCEDURE GREATERP!:(N1,N2); %*********************$

   %==========================================================$
   % This function returns T if "n1" > "n2" else returns NIL. $
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2".$
   %==========================================================$

    BEGIN INTEGER E1,E2;
          IF (E1:=EP!:(N1)) = (E2:=EP!:(N2)) THEN
             RETURN (IF MT!:(N1) > MT!:(N2) THEN T ELSE NIL)
          ELSE IF E1>E2 THEN
                    IF MT!:( INCPREC!:(N1,E1-E2)) > MT!:(N2)
                       THEN RETURN T ELSE RETURN NIL
               ELSE IF MT!:(N1) > MT!:( INCPREC!:(N2,E2-E1))
                       THEN RETURN T ELSE RETURN NIL;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE GEQ!:(N1,N2); %**************************$

   %==========================================================$
   % This function returns T if "n1" >= "n2" else returns NIL.$
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2".$
   %==========================================================$

          NOT( LESSP!:(N1,N2))$



%*************************************************************$
 SYMBOLIC PROCEDURE EQUAL!:(N1,N2); %************************$

   %==========================================================$
   % This function returns T if "n1" = "n2" else returns NIL. $
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2".$
   %==========================================================$

          IF ZEROP!:( DIFFERENCE!:(N1,N2)) THEN T ELSE NIL$



%*************************************************************$
 SYMBOLIC PROCEDURE LESSP!:(N1,N2); %************************$

   %==========================================================$
   % This function returns T if "n1" < "n2" else returns NIL. $
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2".$
   %==========================================================$

          GREATERP!:(N2,N1)$



%*************************************************************$
 SYMBOLIC PROCEDURE LEQ!:(N1,N2); %**************************$

   %==========================================================$
   % This function returns T if "n1" <= "n2" else returns NIL.$
   % N1 and N2 are BIG-FLOAT representations of "n1" and "n2".$
   %==========================================================$

          NOT( GREATERP!:(N1,N2))$



%*************************************************************$
 SYMBOLIC PROCEDURE INTEGERP!:(X); %*************************$

   %===================================================$
   % This function returns T if X is a BIG-FLOAT       $
   %      representing an integer, else it returns NIL.$
   % X is any LISP entity.                             $
   %===================================================$

          IF BFP!:(X) THEN IF EP!:(X)>=0 OR
               EQUAL!:(X , CONV!:I2BF( CONV!:BF2I(X))) THEN T
                           ELSE NIL
          ELSE NIL$



%*************************************************************$
 SYMBOLIC PROCEDURE MINUSP!:(X); %***************************$

   %===================================================$
   % This function returns T if "x"<0 else returns NIL.$
   % X is any LISP entity.                             $
   %===================================================$

          IF BFP!:(X) AND MT!:(X) < 0 THEN T ELSE NIL$



%*************************************************************$
 SYMBOLIC PROCEDURE ZEROP!:(X); %****************************$

   %===================================================$
   % This function returns T if "x"=0 else returns NIL.$
   % X is any LISP entity.                             $
   %===================================================$

          IF BFP!:(X) AND MT!:(X) = 0 THEN T ELSE NIL$




%*************************************************************$
%*************************************************************$
%**                                                         **$
%** 3-1. Elementary CONSTANTS.                              **$
%**                                                         **$
%*************************************************************$
%*************************************************************$



%*************************************************************$
 SYMBOLIC PROCEDURE !:PI(K); %*******************************$

   %====================================================$
   % This function calculates the value of the circular $
   %      constant "PI", with the precision K, by       $
   %      using Machin's well known identity:           $
   %         PI = 16*atan(1/5) - 4*atan(1/239).         $
   %      Calculation is performed mainly on integers.  $
   % K is a positive integer.                           $
   %====================================================$

	  IF NOT( FIXP(K)) OR K<=0 THEN BFLERRMSG '!:PI ELSE
          IF K<=20 THEN ROUND!:MT
             ( MAKE!:BF( 314159265358979323846 , -20) , K) ELSE

    BEGIN INTEGER K3,S,SS,M,N,X;  SCALAR U;

          U:=GET!:CONST( '!:PI , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;

          SS:=N:=EXPT(10 , K3:=K+3)/5;
          X :=-5**2;
          M:=1;
          WHILE NOT(N=0) DO <<N:=N/X; SS:=SS+N/( M:=M+2) >>;

          S:=N:=EXPT(10,K3)/239;
          X:=-239**2;
          M:=1;
          WHILE NOT(N=0) DO <<N:=N/X; S:=S+N/( M:=M+2) >>;

     ANS: U:=ROUND!:MT( MAKE!:BF( 16*SS-4*S , -K3) , K);
          SAVE!:CONST( '!:PI , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:BIGPI(K); %****************************$

   %====================================================$
   % This function calculates the value of the circular $
   %      constant "PI", with the precision K, by the   $
   %      arithmetic-geometric mean method.  (See,      $
   %      R. Brent, JACM Vol.23, #2, pp.242-251(1976).) $
   % K is a positive integer.                           $
   % **** This function should be used only when you    $
   % ****      need "PI" of precision higher than 1000. $
   %====================================================$

	  IF NOT( FIXP(K)) OR K<=0 THEN BFLERRMSG '!:BIGPI ELSE

    BEGIN INTEGER K2,N;  SCALAR DCUT,HALF,X,Y,U,V;

          U:=GET!:CONST( '!:PI , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;

          K2  :=K+2;
          HALF:=CONV!:S2BF("0.5");
          DCUT:=MAKE!:BF(10,-K2);
          X:=CONV!:I2BF( N:=1);
          Y:=DIVIDE!:(X , !:SQRT2(K2) , K2);
          U:=CONV!:S2BF("0.25");
          
          WHILE GREATERP!:( ABS!:(DIFFERENCE!:(X,Y)) , DCUT) DO
            <<V:=X;
              X:=TIMES!:( PLUS!:(X,Y) , HALF);
              Y:=SQRT!:( CUT!:EP( TIMES!:(Y,V) , -K2) , K2);
              V:=DIFFERENCE!:(X,V);
              V:=TIMES!:( TIMES!:(V,V) , CONV!:I2BF(N));
              U:=DIFFERENCE!:(U , CUT!:EP(V,-K2));
              N:=2*N>>;

          V:=CUT!:MT( EXPT!:( PLUS!:(X,Y) , 2) , K2);
          U:=DIVIDE!:(V , TIMES!:( CONV!:I2BF(4) , U) , K);
          SAVE!:CONST( '!:PI , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:E(K); %********************************$

   %=====================================================$
   % This function calculates the value of "e", the base $
   %      of the natural logarithm, with the precision K,$
   %      by summing the Taylor series for exp(x=1).     $
   %      Calculation is performed mainly on integers.   $
   % K is a positive integer.                            $
   %=====================================================$

	  IF NOT( FIXP(K)) OR K<=0 THEN BFLERRMSG '!:E ELSE
          IF K<=20 THEN ROUND!:MT
             ( MAKE!:BF( 271828182845904523536 , -20) , K) ELSE

    BEGIN INTEGER K2,ANS,M,N;  SCALAR U;

          U:=GET!:CONST( '!:E , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;

          K2:=K+2;
          M :=1;
          N :=EXPT(10,K2);
          ANS:=0;
          WHILE NOT(N=0) DO ANS:=ANS+( N:=N/( M:=M+1));

          ANS:=ANS+2*EXPT(10,K2);
          U:=ROUND!:MT( MAKE!:BF(ANS,-K2) , K);
          SAVE!:CONST( '!:E , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:E01(K); %******************************$

   %=====================================================$
   % This function calculates exp(0.1), the value of the $
   %      exponential function at the point 0.1, with    $
   %      the precision K.                               $
   % K is a positive integer.                            $
   %=====================================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:E01 , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=EXP!:( CONV!:S2BF("0.1") , K);
          SAVE!:CONST( '!:E01 , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:LOG2(K); %*****************************$

   %==============================================$
   % This function calculates log(2), the natural $
   %      logarithm of 2, with the precision K.   $
   % K is a positive integer.                     $
   %==============================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:LOG2 , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=LOG!:( CONV!:I2BF(2) , K);
          SAVE!:CONST( '!:LOG2 , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:LOG3(K); %*****************************$

   %==============================================$
   % This function calculates log(3), the natural $
   %      logarithm of 3, with the precision K.   $
   % K is a positive integer.                     $
   %==============================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:LOG3 , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=LOG!:( CONV!:I2BF(3) , K);
          SAVE!:CONST( '!:LOG3 , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:LOG5(K); %*****************************$

   %==============================================$
   % This function calculates log(5), the natural $
   %      logarithm of 5, with the precision K.   $
   % K is a positive integer.                     $
   %==============================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:LOG5 , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=LOG!:( CONV!:I2BF(5) , K);
          SAVE!:CONST( '!:LOG5 , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:LOG10(K); %****************************$

   %===============================================$
   % This function calculates log(10), the natural $
   %      logarithm of 10, with the precision K.   $
   % K is a positive integer.                      $
   %===============================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:LOG10 , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=LOG!:( CONV!:I2BF(10) , K);
          SAVE!:CONST( '!:LOG10 , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:LOGPI(K); %****************************$

   %===============================================$
   % This function calculates log(PI), the natural $
   %      logarithm of "PI", with the precision K. $
   % K is a positive integer.                      $
   %===============================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:LOGPI , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=LOG!:( !:PI(K+2) , K);
          SAVE!:CONST( '!:LOGPI , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:SQRT2(K); %****************************$

   %===================================================$
   % This function calculates SQRT(2), the square root $
   %      of 2, with the precision K.                  $
   % K is a positive integer.                          $
   %===================================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:SQRT2 , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=SQRT!:( CONV!:I2BF(2) , K);
          SAVE!:CONST( '!:SQRT2 , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:SQRT3(K); %****************************$

   %===================================================$
   % This function calculates SQRT(3), the square root $
   %      of 3, with the precision K.                  $
   % K is a positive integer.                          $
   %===================================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:SQRT3 , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=SQRT!:( CONV!:I2BF(3) , K);
          SAVE!:CONST( '!:SQRT3 , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:SQRT5(K); %****************************$

   %===================================================$
   % This function calculates SQRT(5), the square root $
   %      of 5, with the precision K.                  $
   % K is a positive integer.                          $
   %===================================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:SQRT5 , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=SQRT!:( CONV!:I2BF(5) , K);
          SAVE!:CONST( '!:SQRT5 , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:SQRT10(K); %***************************$

   %====================================================$
   % This function calculates SQRT(10), the square root $
   %      of 10, with the precision K.                  $
   % K is a positive integer.                           $
   %====================================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:SQRT10 , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=SQRT!:( CONV!:I2BF(10) , K);
          SAVE!:CONST( '!:SQRT10 , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:SQRTPI(K); %***************************$

   %====================================================$
   % This function calculates SQRT(PI), the square root $
   %      of "PI", with the precision K.                $
   % K is a positive integer.                           $
   %====================================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:SQRTPI , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=SQRT!:( !:PI(K+2) , K);
          SAVE!:CONST( '!:SQRTPI , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:SQRTE(K); %****************************$

   %===================================================$
   % This function calculates SQRT(e), the square root $
   %      of "e", with the precision K.                $
   % K is a positive integer.                          $
   %===================================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:SQRTE , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=SQRT!:( !:E(K+2) , K);
          SAVE!:CONST( '!:SQRTE , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:CBRT2(K); %****************************$

   %=================================================$
   % This function calculates CBRT(2), the cube root $
   %      of 2, with the precision K.                $
   % K is a positive integer.                        $
   %=================================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:CBRT2 , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=CBRT!:( CONV!:I2BF(2) , K);
          SAVE!:CONST( '!:CBRT2 , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:CBRT3(K); %****************************$

   %=================================================$
   % This function calculates CBRT(3), the cube root $
   %      of 3, with the precision K.                $
   % K is a positive integer.                        $
   %=================================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:CBRT3 , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=CBRT!:( CONV!:I2BF(3) , K);
          SAVE!:CONST( '!:CBRT3 , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:CBRT5(K); %****************************$

   %=================================================$
   % This function calculates CBRT(5), the cube root $
   %      of 5, with the precision K.                $
   % K is a positive integer.                        $
   %=================================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:CBRT5 , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=CBRT!:( CONV!:I2BF(5) , K);
          SAVE!:CONST( '!:CBRT5 , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:CBRT10(K); %***************************$

   %==================================================$
   % This function calculates CBRT(10), the cube root $
   %      of 10, with the precision K.                $
   % K is a positive integer.                         $
   %==================================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:CBRT10 , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=CBRT!:( CONV!:I2BF(10) , K);
          SAVE!:CONST( '!:CBRT10 , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:CBRTPI(K); %***************************$

   %==================================================$
   % This function calculates CBRT(PI), the cube root $
   %      of "PI", with the precision K.              $
   % K is a positive integer.                         $
   %==================================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:CBRTPI , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=CBRT!:( !:PI(K+2) , K);
          SAVE!:CONST( '!:CBRTPI , U);  RETURN U;
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE !:CBRTE(K); %****************************$

   %=================================================$
   % This function calculates CBRT(e), the cube root $
   %      of "e", with the precision K.              $
   % K is a positive integer.                        $
   %=================================================$

    BEGIN SCALAR U;
          U:=GET!:CONST( '!:CBRTE , K);
          IF U = "NOT FOUND" THEN NIL ELSE RETURN U;
          U:=CBRT!:( !:E(K+2) , K);
          SAVE!:CONST( '!:CBRTE , U);  RETURN U;
    END$




%*************************************************************$
%*************************************************************$
%**                                                         **$
%** 3-2. Routines for saving CONSTANTS.                     **$
%**                                                         **$
%*************************************************************$
%*************************************************************$



%*************************************************************$
 SYMBOLIC PROCEDURE GET!:CONST(CNST,K); %********************$

   %==================================================$
   % This function returns the value of constant CNST $
   %      of the precision K, if it was calculated    $
   %      previously with, at least, the precision K, $
   %      else it returns "NOT FOUND".                $
   % CNST is the name of the constant (to be quoted). $
   % K is a positive integer.                         $
   %==================================================$

          IF ATOM(CNST) AND FIXP(K) AND K>0 THEN
    BEGIN SCALAR U;
          U:=GET(CNST , 'SAVE!:C);
          IF NULL(U) OR CAR(U)<K THEN RETURN "NOT FOUND"
          ELSE IF CAR(U)=K THEN RETURN CDR(U)
               ELSE RETURN ROUND!:MT(CDR(U),K);
    END
	  ELSE BFLERRMSG 'GET!:CONST$



%*************************************************************$
 SYMBOLIC PROCEDURE SAVE!:CONST(CNST,NMBR); %****************$

   %=================================================$
   % This function saves the value of constant CNST  $
   %      for the later use.                         $
   % CNST is the name of the constant (to be quoted).$
   % NMBR is a BIG-FLOAT representation of the value.$
   %=================================================$

          IF ATOM(CNST) AND BFP!:(NMBR) THEN
             PUT(CNST , 'SAVE!:C , CONS( PRECI!:(NMBR) , NMBR))
	  ELSE BFLERRMSG 'SAVE!:CONST$



%*************************************************************$
 SYMBOLIC PROCEDURE SET!:CONST(CNST,L); %********************$

   %=================================================$
   % This function sets the value of constant CNST.  $
   % CNST is the name of the constant (to be quoted).$
   % L is a list of integers, which represents the   $
   %      value of the constant in the way described $
   %      in the function READ!:LNUM.                $
   %=================================================$

          SAVE!:CONST(CNST , READ!:LNUM(L))$



%*************************************************************$
 SYMBOLIC$ %SETTING THE CONSTANTS ***************************$

   SET!:CONST( '!:PI    , '( 0   3141 59265 35897 93238 46264
        33832 79502 88419 71693 99375 105820 9749 44592 30781
        64062 86208 99862 80348 25342 11706 79821 48086 51328
        23066 47093 84460 95505 82231 72535 94081 28481 1174
       5028410 2701 93852 11055 59644 62294 89549 30381 96442
        88109 8) )$

   SET!:CONST( '!:E     , '( 0   2718 28182 84590 45235 36028
        74713 52662 49775 72470 93699 95957 49669 67627 72407
        66303 53547 59457 13821 78525 16642 74274 66391 93200
        30599 21817 41359 66290 43572 90033 42952 60595 63073
        81323 28627 943490 7632 33829 88075 31952 510190 1157
        38341 9) )$

   SET!:CONST( '!:E01   , '( 0   1105 17091 80756 47624 81170
        78264 90246 66822 45471 94737 51871 87928 63289 44096
        79667 47654 30298 91433 18970 74865 36329 2) )$

   SET!:CONST( '!:LOG2  , '(-1   6931 47180 55994 53094 17232
        12145 81765 68075 50013 43602 55254 1206 800094 93393
        62196 96947 15605 86332 69964 18687 54200 2) )$

   SET!:CONST( '!:LOG3  , '( 0   1098 61228 866810 9691 39524
        52369 22525 70464 74905 57822 74945 17346 94333 63749
        42932 18608 96687 36157 54813 73208 87879 7) )$

   SET!:CONST( '!:LOG5  , '( 0   1609 43791 2434100 374 60075
        93332 26187 63952 56013 54268 51772 19126 47891 47417
        898770 7657 764630 1338 78093 179610 7999 7) )$

   SET!:CONST( '!:LOG10 , '( 0   2302 58509 29940 456840 1799
        14546 84364 20760 11014 88628 77297 60333 27900 96757
        26096 77352 48023 599720 5089 59829 83419 7) )$

   SET!:CONST( '!:LOGPI , '( 0   1144 72988 5849400 174 14342
        73513 53058 71164 72948 12915 31157 15136 23071 47213
        77698 848260 7978 36232 70275 48970 77020 1) )$

   SET!:CONST( '!:SQRT2 , '( 0   1414 21356 23730 95048 80168
        872420 96980 7856 96718 75376 94807 31766 79737 99073
        24784 621070 38850 3875 34327 64157 27350 1) )$

   SET!:CONST( '!:SQRT3 , '( 0   17320 5080 75688 77293 52744
        634150 5872 36694 28052 53810 38062 805580 6979 45193
        301690 88000 3708 11461 86757 24857 56756 3) )$

   SET!:CONST( '!:SQRT5 , '( 0   22360 6797 74997 89696 40917
        36687 31276 235440 6183 59611 52572 42708 97245 4105
       209256 37804 89941 441440 8378 78227 49695 1) )$

   SET!:CONST( '!:SQRT10, '( 0   3162 277660 1683 79331 99889
        35444 32718 53371 95551 39325 21682 685750 4852 79259
        44386 39238 22134 424810 8379 30029 51873 47))$

   SET!:CONST( '!:SQRTPI, '( 0   1772 453850 9055 16027 29816
        74833 41145 18279 75494 56122 38712 821380 7789 85291
        12845 91032 18137 49506 56738 54466 54162 3) )$

   SET!:CONST( '!:SQRTE , '( 0   1648 721270 7001 28146 8486
       507878 14163 57165 3776100 710 14801 15750 79311 64066
        10211 94215 60863 27765 20056 36664 30028 7) )$

   SET!:CONST( '!:CBRT2 , '( 0   1259 92104 98948 73164 7672
       106072 78228 350570 2514 64701 5079800 819 75112 15529
        96765 13959 48372 93965 62436 25509 41543 1) )$

   SET!:CONST( '!:CBRT3 , '( 0   1442 249570 30740 8382 32163
        83107 80109 58839 18692 53499 35057 75464 16194 54168
        75968 29997 33985 47554 79705 64525 66868 4) )$

   SET!:CONST( '!:CBRT5 , '( 0   1709 97594 66766 96989 35310
        88725 43860 10986 80551 105430 5492 43828 61707 44429
        592050 4173 21625 71870 10020 18900 220450 ) )$

   SET!:CONST( '!:CBRT10, '( 0   2154 4346900 318 83721 75929
        35665 19350 49525 93449 42192 10858 24892 35506 34641
        11066 48340 80018 544150 3543 24327 61012 6) )$

   SET!:CONST( '!:CBRTPI, '( 0   1464 59188 75615 232630 2014
        25272 63790 39173 85968 55627 93717 43572 55937 13839
        36497 98286 26614 56820 67820 353820 89750 ) )$

   SET!:CONST( '!:CBRTE , '( 0   1395 61242 50860 89528 62812
        531960 2586 83759 79065 15199 40698 26175 167060 3173
        90156 45951 84696 97888 17295 83022 41352 1) )$




%*************************************************************$
%*************************************************************$
%**                                                         **$
%** 4-1. Elementary FUNCTIONS.                              **$
%**                                                         **$
%*************************************************************$
%*************************************************************$



%*************************************************************$
 SYMBOLIC PROCEDURE SQRT!:(X,K); %***************************$

   %===================================================$
   % This function calculates SQRT(x), the square root $
   %      of "x", with the precision K, by Newton's    $
   %      iteration method.                            $
   % X is a BIG-FLOAT representation of "x", x >= 0,   $
   %      otherwise it is converted to a <BIG-FLOAT>.  $
   % K is a positive integer.                          $
   %===================================================$

          IF NOT( BFP!:( X:=CONV!:A2BF(X))) OR MINUSP!:(X) OR
	     NOT( FIXP(K)) OR K<=0 THEN BFLERRMSG 'SQRT!: ELSE
          IF ZEROP!:(X) THEN CONV!:I2BF(0) ELSE

    BEGIN INTEGER K2,NCUT,NFIG;  SCALAR DCUT,HALF,DY,Y,Y0,U;

          K2  :=K+2;
          NCUT:=K2-(ORDER!:(X)+1)/2;
          HALF:=CONV!:S2BF("0.5");
          DCUT:=MAKE!:BF(10,-NCUT);
          DY  :=MAKE!:BF(20,-NCUT);

          Y0:=CONV!:MT(X,2);
          IF REMAINDER( EP!:(Y0) , 2) = 0 THEN
               Y0:=MAKE!:BF( 3+2*MT!:(Y0)/25 ,  EP!:(Y0)/2)
          ELSE Y0:=MAKE!:BF( 10+2*MT!:(Y0)/9 , (EP!:(Y0)-1)/2);

          NFIG:=1;
          WHILE NFIG<K2 OR GREATERP!:( ABS!:(DY) , DCUT) DO
            <<IF (NFIG:=2*NFIG) > K2 THEN NFIG:=K2;
              U :=DIVIDE!:(X,Y0,NFIG);
              Y :=TIMES!:( PLUS!:(Y0,U) , HALF);
              DY:=DIFFERENCE!:(Y,Y0);
              Y0:=Y>>;

          RETURN ROUND!:MT(Y,K);
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE CBRT!:(X,K); %***************************$

   %===================================================$
   % This function calculates CBRT(x), the cube root   $
   %      of "x", with the precision K, by Newton's    $
   %      iteration method.                            $
   % X is a BIG-FLOAT representation of any real "x",  $
   %      otherwise it is converted to a <BIG-FLOAT>.  $
   % K is a positive integer.                          $
   %===================================================$

          IF NOT( BFP!:( X:=CONV!:A2BF(X))) OR
	     NOT( FIXP(K)) OR K<=0 THEN BFLERRMSG 'CBRT!: ELSE
          IF ZEROP!:(X) THEN CONV!:I2BF(0) ELSE
          IF MINUSP!:(X) THEN
             MINUS!:( CBRT!:( MINUS!:(X) , K)) ELSE

    BEGIN INTEGER K2,NCUT,NFIG,J;  SCALAR DCUT,THRE,DY,Y,U;

          K2  :=K+2;
          NCUT:=K2-(ORDER!:(X)+2)/3;
          THRE:=CONV!:I2BF(3);
          DCUT:=MAKE!:BF(10,-NCUT);
          DY  :=MAKE!:BF(20,-NCUT);

          Y:=CONV!:MT(X,3);
          IF (J:=REMAINDER( EP!:(Y) , 3)) = 0 THEN
               Y:=MAKE!:BF( 5 + MT!:(Y)/167 ,  EP!:(Y)/3) ELSE
          IF J=1 OR J=-2 THEN
               Y:=MAKE!:BF( 10+  MT!:(Y)/75 , (EP!:(Y)-1)/3)
          ELSE Y:=MAKE!:BF( 22+2*MT!:(Y)/75 , (EP!:(Y)-2)/3);

          NFIG:=1;
          WHILE NFIG<K2 OR GREATERP!:( ABS!:(DY) , DCUT) DO
            <<IF (NFIG:=2*NFIG) > K2 THEN NFIG:=K2;
              U :=CUT!:MT( TIMES!:(Y,Y) , NFIG);
              U :=DIVIDE!:(X , U , NFIG);
              J :=ORDER!:( U:=DIFFERENCE!:(U,Y))+NCUT-K2;
              DY:=DIVIDE!:(U , THRE , MAX(1,NFIG+J));
              Y :=PLUS!:(Y,DY) >>;

          RETURN ROUND!:MT(Y,K);
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE EXP!:(X,K); %****************************$

   %=================================================$
   % This function calculates exp(x), the value of   $
   %      the exponential function at the point "x", $
   %      with the precision K, by summing terms of  $
   %      the Taylor series for exp(z), 0 < z < 1.   $
   % X is a BIG-FLOAT representation of any real "x",$
   %      otherwise it is converted to a <BIG-FLOAT>.$
   % K is a positive integer.                        $
   %=================================================$

          IF NOT( BFP!:( X:=CONV!:A2BF(X))) OR
	     NOT( FIXP(K)) OR K<=0 THEN BFLERRMSG 'EXP!: ELSE
          IF ZEROP!:(X) THEN CONV!:I2BF(1) ELSE

    BEGIN INTEGER K2,M;  SCALAR ONE,Q,R,Y,YQ,YR,SAVE!:P;

          K2 :=K+2;
          ONE:=CONV!:I2BF(1);
          Q:=CONV!:I2BF( M:=CONV!:BF2I( Y:=ABS!:(X)));
          R:=DIFFERENCE!:(Y,Q);
          IF ZEROP!:(Q) THEN YQ:=ONE
          ELSE <<    SAVE!:P:=!:PREC!:; !:PREC!::=K2;
                 YQ:=TEXPT!:( !:E(K2) , M);
                     !:PREC!::=SAVE!:P>>;
          IF ZEROP!:(R) THEN YR:=ONE ELSE

        BEGIN INTEGER J,N;  SCALAR DCUT,FCTRIAL,RI,TM;
 
              DCUT:=MAKE!:BF(10,-K2);
              YR:=RI:=TM:=ONE;
 
              M:=1;
              J:=0;
              WHILE GREATERP!:(TM,DCUT) DO
                <<FCTRIAL:=CONV!:I2BF( M:=M*( J:=J+1));
                  RI:=CUT!:EP( TIMES!:(RI,R) , -K2);
                  N :=MAX(1 , K2-ORDER!:(FCTRIAL)+ORDER!:(RI));
                  TM:=DIVIDE!:(RI,FCTRIAL,N);
                  YR:=PLUS!:(YR,TM);  IF REMAINDER(J,10)=0 THEN
                                      YR:=CUT!:EP(YR,-K2) >>;
        END;

          Y:=CUT!:MT( TIMES!:(YQ,YR) , K+1);
          RETURN (IF MINUSP!:(X) THEN DIVIDE!:(ONE,Y,K)
                  ELSE ROUND!:LAST(Y) );
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE LOG!:(X,K); %****************************$

   %===================================================$
   % This function calculates log(x), the value of the $
   %      logarithmic function at the point "x", with  $
   %      the precision K, by summing terms of the     $
   %      Taylor series for log(1+z), 0 < z < 0.10518. $
   % X is a BIG-FLOAT representation of "x", x > 0,    $
   %      otherwise it is converted to a <BIG-FLOAT>.  $
   % K is a positive integer.                          $
   %===================================================$

          IF NOT( BFP!:( X:=CONV!:A2BF(X))) OR
             MINUSP!:(X) OR ZEROP!:(X) OR
	     NOT( FIXP(K)) OR K<=0 THEN BFLERRMSG 'LOG!: ELSE
          IF EQUAL!:(X , CONV!:I2BF(1)) THEN CONV!:I2BF(0) ELSE

    BEGIN INTEGER K2,M;  SCALAR EE,ES,ONE,SIGN,L,Y,Z,SAVE!:P;

          K2 :=K+2;
          ONE:=CONV!:I2BF(1);
          EE :=!:E(K2);
          ES :=!:E01(K2);

          IF GREATERP!:(X,ONE) THEN <<SIGN:=ONE; Y:=X>>
          ELSE <<SIGN:=MINUS!:(ONE); Y:=DIVIDE!:(ONE,X,K2) >>;

          IF LESSP!:(Y,EE) THEN <<M:=0; Z:=Y>>
          ELSE <<IF (M:=(ORDER!:(Y)*23)/10) = 0 THEN Z:=Y
                 ELSE <<    SAVE!:P:=!:PREC!:; !:PREC!::=K2;
                        Z:=DIVIDE!:(Y , TEXPT!:(EE,M) , K2);
                            !:PREC!::=SAVE!:P>>;
                 WHILE GREATERP!:(Z,EE) DO
                   <<M:=M+1; Z:=DIVIDE!:(Z,EE,K2) >> >>;
          L:=CONV!:I2BF(M);

          Y:=CONV!:S2BF("0.1");
          WHILE GREATERP!:(Z,ES) DO
            <<L:=PLUS!:(L,Y); Z:=DIVIDE!:(Z,ES,K2) >>;
          Z:=DIFFERENCE!:(Z,ONE);

        BEGIN INTEGER N;  SCALAR DCUT,TM,ZI;

              Y:=TM:=ZI:=Z;
              Z:=MINUS!:(Z);
              DCUT:=MAKE!:BF(10,-K2);

              M:=1;
              WHILE GREATERP!:( ABS!:(TM) , DCUT) DO
                <<ZI:=CUT!:EP( TIMES!:(ZI,Z) , -K2);
                  N :=MAX(1 , K2+ORDER!:(ZI));
                  TM:=DIVIDE!:(ZI , CONV!:I2BF( M:=M+1) , N);
                  Y :=PLUS!:(Y,TM);  IF REMAINDER(M,10)=0 THEN
                                     Y:=CUT!:EP(Y,-K2) >>;
        END;

          Y:=PLUS!:(Y,L);
          RETURN ROUND!:MT( TIMES!:(SIGN,Y) , K);
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE LN!:(X,K); %*****************************$

   %=================================================$
   % This function calculates log(x), the value of   $
   %      the logarithmic function at the point "x", $
   %      with the precision K, by solving           $
   %         x = exp(y)  by Newton's method.         $
   % X is a BIG-FLOAT representation of "x", x > 0,  $
   %      otherwise it is converted to a <BIG-FLOAT>.$
   % K is a positive integer.                        $
   %=================================================$

          IF NOT( BFP!:( X:=CONV!:A2BF(X))) OR
             MINUSP!:(X) OR ZEROP!:(X) OR
	     NOT( FIXP(K)) OR K<=0 THEN BFLERRMSG 'LN!: ELSE
          IF EQUAL!:(X , CONV!:I2BF(1)) THEN CONV!:I2BF(0) ELSE

    BEGIN INTEGER K2,M;  SCALAR EE,ONE,SIGN,Y,Z,SAVE!:P;

          K2 :=K+2;
          ONE:=CONV!:I2BF(1);
          EE :=!:E(K2+2);

          IF GREATERP!:(X,ONE) THEN <<SIGN:=ONE; Y:=X>>
          ELSE <<SIGN:=MINUS!:(ONE); Y:=DIVIDE!:(ONE,X,K2) >>;

          IF LESSP!:(Y,EE) THEN <<M:=0; Z:=Y>>
          ELSE <<IF (M:=(ORDER!:(Y)*23)/10) = 0 THEN Z:=Y
                 ELSE <<    SAVE!:P:=!:PREC!:; !:PREC!::=K2;
                        Z:=DIVIDE!:(Y , TEXPT!:(EE,M) , K2);
                            !:PREC!::=SAVE!:P>>;
                 WHILE GREATERP!:(Z,EE) DO
                   <<M:=M+1; Z:=DIVIDE!:(Z,EE,K2) >> >>;

        BEGIN INTEGER NFIG,N;  SCALAR DCUT,DX,DY,X0;
 
              DCUT:=MAKE!:BF(10,-K2);
              DY  :=MAKE!:BF(20,-K2);
              Y:=DIVIDE!:( DIFFERENCE!:(Z,ONE) ,
                           CONV!:S2BF("1.72") , 2);
 
              NFIG:=1;
              WHILE NFIG<K2 OR GREATERP!:( ABS!:(DY) , DCUT) DO
                <<IF (NFIG:=2*NFIG) > K2 THEN NFIG:=K2;
                  X0:=EXP!:(Y,NFIG);
                  DX:=DIFFERENCE!:(Z,X0);
                  N :=MAX(1 , NFIG+ORDER!:(DX));
                  DY:=DIVIDE!:(DX,X0,N);
                  Y :=PLUS!:(Y,DY) >>;
        END;

          Y:=PLUS!:( CONV!:I2BF(M) , Y);
          RETURN ROUND!:MT( TIMES!:(SIGN,Y) , K);
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE SIN!:(X,K); %****************************$

   %=================================================$
   % This function calculates sin(x), the value of   $
   %      the sine function at the point "x", with   $
   %      the precision K, by summing terms of the   $
   %      Taylor series for sin(z), 0 < z < PI/4.    $
   % X is a BIG-FLOAT representation of any rael "x",$
   %      otherwise it is converted to a <BIG-FLOAT>.$
   % K is a positive integer.                        $
   %=================================================$

          IF NOT( BFP!:( X:=CONV!:A2BF(X))) OR
	     NOT( FIXP(K)) OR K<=0 THEN BFLERRMSG 'SIN!: ELSE
          IF ZEROP!:(X) THEN CONV!:I2BF(0) ELSE
          IF MINUSP!:(X) THEN
             MINUS!:( SIN!:( MINUS!:(X) , K)) ELSE

    BEGIN INTEGER K2,M;  SCALAR PI4,SIGN,Q,R,Y;

          K2 :=K+2;
          M  :=PRECI!:(X);
          PI4:=TIMES!:( !:PI(K2+M) , CONV!:S2BF("0.25"));
          IF LESSP!:(X,PI4) THEN <<M:=0; R:=X>>
          ELSE <<M:=CONV!:BF2I( Q:=QUOTIENT!:(X,PI4));
                 R:=DIFFERENCE!:(X , TIMES!:(Q,PI4)) >>;

          SIGN:=CONV!:I2BF(1);
          IF M>=8 THEN M:=REMAINDER(M,8);
          IF M>=4 THEN <<SIGN:=MINUS!:(SIGN); M:=M-4>>;
          IF M=0 THEN GOTO SN ELSE IF M=1 THEN GOTO M1 ELSE
          IF M=2 THEN GOTO M2 ELSE             GOTO M3;

      M1: R:=CUT!:MT( DIFFERENCE!:(PI4,R) , K2);
          RETURN TIMES!:(SIGN , COS!:(R,K));

      M2: R:=CUT!:MT(R,K2);
          RETURN TIMES!:(SIGN , COS!:(R,K));

      M3: R:=CUT!:MT( DIFFERENCE!:(PI4,R) , K2);

    SN: BEGIN INTEGER J,N,NCUT;  SCALAR DCUT,FCTRIAL,RI,TM;
 
              NCUT:=K2-MIN(0 , ORDER!:(R)+1);
              DCUT:=MAKE!:BF(10,-NCUT);
              Y:=RI:=TM:=R;
              R:=MINUS!:( CUT!:EP( TIMES!:(R,R) , -NCUT));
 
              M:=J:=1;
              WHILE GREATERP!:( ABS!:(TM) , DCUT) DO
                <<J:=J+2;
                  FCTRIAL:=CONV!:I2BF( M:=M*J*(J-1));
                  RI:=CUT!:EP( TIMES!:(RI,R) , -NCUT);
                  N :=MAX(1 , K2-ORDER!:(FCTRIAL)+ORDER!:(RI));
                  TM:=DIVIDE!:(RI,FCTRIAL,N);
                  Y :=PLUS!:(Y,TM);  IF REMAINDER(J,20)=0 THEN
                                     Y:=CUT!:EP(Y,-NCUT) >>;
        END;

          RETURN ROUND!:MT( TIMES!:(SIGN,Y) , K);
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE COS!:(X,K); %****************************$

   %=================================================$
   % This function calculates cos(x), the value of   $
   %      the cosine function at the point "x", with $
   %      the precision K, by summing terms of the   $
   %      Taylor series for cos(z), 0 < z < PI/4.    $
   % X is a BIG-FLOAT representation of any real "x",$
   %      otherwise it is converted to a <BIG-FLOAT>.$
   % K is a positive integer.                        $
   %=================================================$

          IF NOT( BFP!:( X:=CONV!:A2BF(X))) OR
	     NOT( FIXP(K)) OR K<=0 THEN BFLERRMSG 'COS!: ELSE
          IF ZEROP!:(X) THEN CONV!:I2BF(1) ELSE
          IF MINUSP!:(X) THEN COS!:( MINUS!:(X) , K) ELSE

    BEGIN INTEGER K2,M;  SCALAR PI4,SIGN,Q,R,Y;

          K2 :=K+2;
          M  :=PRECI!:(X);
          PI4:=TIMES!:( !:PI(K2+M) , CONV!:S2BF("0.25"));
          IF LESSP!:(X,PI4) THEN <<M:=0; R:=X>>
          ELSE <<M:=CONV!:BF2I( Q:=QUOTIENT!:(X,PI4));
                 R:=DIFFERENCE!:(X , TIMES!:(Q,PI4)) >>;

          SIGN:=CONV!:I2BF(1);
          IF M>=8 THEN M:=REMAINDER(M,8);
          IF M>=4 THEN <<SIGN:=MINUS!:(SIGN); M:=M-4>>;
          IF M>=2 THEN SIGN:=MINUS!:(SIGN);
          IF M=0 THEN GOTO CS ELSE IF M=1 THEN GOTO M1 ELSE
          IF M=2 THEN GOTO M2 ELSE             GOTO M3;

      M1: R:=CUT!:MT( DIFFERENCE!:(PI4,R) , K2);
          RETURN TIMES!:(SIGN , SIN!:(R,K));

      M2: R:=CUT!:MT(R,K2);
          RETURN TIMES!:(SIGN , SIN!:(R,K));

      M3: R:=CUT!:MT( DIFFERENCE!:(PI4,R) , K2);

    CS: BEGIN INTEGER J,N;  SCALAR DCUT,FCTRIAL,RI,TM;
 
              DCUT:=MAKE!:BF(10,-K2);
              Y:=RI:=TM:=CONV!:I2BF(1);
              R:=MINUS!:( CUT!:EP( TIMES!:(R,R) , -K2));
 
              M:=1;
              J:=0;
              WHILE GREATERP!:( ABS!:(TM) , DCUT) DO
                <<J:=J+2;
                  FCTRIAL:=CONV!:I2BF( M:=M*J*(J-1));
                  RI:=CUT!:EP( TIMES!:(RI,R) , -K2);
                  N :=MAX(1 , K2-ORDER!:(FCTRIAL)+ORDER!:(RI));
                  TM:=DIVIDE!:(RI,FCTRIAL,N);
                  Y :=PLUS!:(Y,TM);  IF REMAINDER(J,20)=0 THEN
                                     Y:=CUT!:EP(Y,-K2) >>;
        END;

          RETURN ROUND!:MT( TIMES!:(SIGN,Y) , K);
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE TAN!:(X,K); %****************************$

   %=================================================$
   % This function calculates tan(x), the value of   $
   %      the tangent function at the point "x",     $
   %      with the precision K, by calculating       $
   %         sin(x)  or  cos(x) = sin(PI/2-x).       $
   % X is a BIG-FLOAT representation of any real "x",$
   %      otherwise it is converted to a <BIG-FLOAT>.$
   % K is a positive integer.                        $
   %=================================================$

          IF NOT( BFP!:( X:=CONV!:A2BF(X))) OR
	     NOT( FIXP(K)) OR K<=0 THEN BFLERRMSG 'TAN!: ELSE
          IF ZEROP!:(X) THEN CONV!:I2BF(0) ELSE
          IF MINUSP!:(X) THEN
             MINUS!:( TAN!:( MINUS!:(X) , K)) ELSE

    BEGIN INTEGER K2,M;  SCALAR ONE,PI4,SIGN,Q,R;

          K2 :=K+2;
          ONE:=CONV!:I2BF(1);
          M  :=PRECI!:(X);
          PI4:=TIMES!:( !:PI(K2+M) , CONV!:S2BF("0.25"));
          IF LESSP!:(X,PI4) THEN <<M:=0; R:=X>>
          ELSE <<M:=CONV!:BF2I( Q:=QUOTIENT!:(X,PI4));
                 R:=DIFFERENCE!:(X , TIMES!:(Q,PI4)) >>;

          IF M>=4 THEN M:=REMAINDER(M,4);
          IF M>=2 THEN SIGN:=MINUS!:(ONE) ELSE SIGN:=ONE;
          IF M=1 OR M=3 THEN R:=DIFFERENCE!:(PI4,R);
          R:=CUT!:MT(R,K2);
          IF M=0 OR M=3 THEN GOTO M03 ELSE GOTO M12;

     M03: R:=SIN!:(R,K2);
          Q:=DIFFERENCE!:(ONE , TIMES!:(R,R));
          Q:=SQRT!:( CUT!:MT(Q,K2) , K2);
          RETURN TIMES!:(SIGN , DIVIDE!:(R,Q,K));

     M12: R:=SIN!:(R,K2);
          Q:=DIFFERENCE!:(ONE , TIMES!:(R,R));
          Q:=SQRT!:( CUT!:MT(Q,K2) , K2);
          RETURN TIMES!:(SIGN , DIVIDE!:(Q,R,K));

    END$



%*************************************************************$
 SYMBOLIC PROCEDURE ASIN!:(X,K); %***************************$

   %==================================================$
   % This function calculates asin(x), the value of   $
   %      the arcsine function at the point "x",      $
   %      with the precision K, by calculating        $
   %         atan(x/SQRT(1-x**2))  by ATAN!:.         $
   %      The answer is in the range [-PI/2 , PI/2].  $
   % X is a BIG-FLOAT representation of "x", IxI <= 1,$
   %      otherwise it is converted to a <BIG-FLOAT>. $
   % K is a positive integer.                         $
   %==================================================$

          IF NOT( BFP!:( X:=CONV!:A2BF(X))) OR
             GREATERP!:( ABS!:(X) , CONV!:I2BF(1)) OR
	     NOT( FIXP(K)) OR K<=0 THEN BFLERRMSG 'ASIN!: ELSE
          IF MINUSP!:(X) THEN
             MINUS!:( ASIN!:( MINUS!:(X) , K)) ELSE

    BEGIN INTEGER K2;  SCALAR ONE,Y;

          K2 :=K+2;
          ONE:=CONV!:I2BF(1);
          IF LESSP!:( DIFFERENCE!:(ONE,X) , MAKE!:BF(10,-K2))
             THEN RETURN ROUND!:MT
                ( TIMES!:( !:PI(K+1) , CONV!:S2BF("0.5")) , K);

          Y:=CUT!:MT( DIFFERENCE!:(ONE , TIMES!:(X,X)) , K2);
          Y:=DIVIDE!:(X , SQRT!:(Y,K2) , K2);
          RETURN ATAN!:(Y,K);
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE ACOS!:(X,K); %***************************$

   %==================================================$
   % This function calculates acos(x), the value of   $
   %      the arccosine function at the point "x",    $
   %      with the precision K, by calculating        $
   %         atan(SQRT(1-x**2)/x)  if  x > 0  or      $
   %         atan(SQRT(1-x**2)/x) + PI  if  x < 0.    $
   %      The answer is in the range [0 , PI].        $
   % X is a BIG-FLOAT representation of "x", IxI <= 1,$
   %      otherwise it is converted to a <BIG-FLOAT>. $
   % K is a positive integer.                         $
   %==================================================$

          IF NOT( BFP!:( X:=CONV!:A2BF(X))) OR
             GREATERP!:( ABS!:(X) , CONV!:I2BF(1)) OR
	     NOT( FIXP(K)) OR K<=0 THEN BFLERRMSG 'ACOS!: ELSE

    BEGIN INTEGER K2;  SCALAR Y;

          K2:=K+2;
          IF LESSP!:( ABS!:(X) , MAKE!:BF(50,-K2))
             THEN RETURN ROUND!:MT
                ( TIMES!:( !:PI(K+1) , CONV!:S2BF("0.5")) , K);

          Y:=DIFFERENCE!:( CONV!:I2BF(1) , TIMES!:(X,X));
          Y:=CUT!:MT(Y,K2);
          Y:=DIVIDE!:( SQRT!:(Y,K2) , ABS!:(X) , K2);
          RETURN (IF MINUSP!:(X) THEN ROUND!:MT
                  ( DIFFERENCE!:( !:PI(K+1) , ATAN!:(Y,K)) , K)
                  ELSE ATAN!:(Y,K) );
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE ATAN!:(X,K); %***************************$

   %====================================================$
   % This function calculates atan(x), the value of the $
   %      arctangent function at the point "x", with    $
   %      the precision K, by summing terms of the      $
   %      Taylor series for atan(z)  if  0 < z < 0.42.  $
   %      Otherwise the following identities are used:  $
   %         atan(x) = PI/2 - atan(1/x)  if  1 < x  and $
   %         atan(x) = 2*atan(x/(1+SQRT(1+x**2)))       $
   %            if  0.42 <= x <= 1.                     $
   %      The answer is in the range [-PI/2 , PI/2].    $
   % X is a BIG-FLOAT representation of any real "x",   $
   %      otherwise it is converted to a <BIG-FLOAT>.   $
   % K is a positive integer.                           $
   %====================================================$

          IF NOT( BFP!:( X:=CONV!:A2BF(X))) OR
	     NOT( FIXP(K)) OR K<=0 THEN BFLERRMSG 'ATAN!: ELSE
          IF ZEROP!:(X) THEN CONV!:I2BF(0) ELSE
          IF MINUSP!:(X) THEN
             MINUS!:( ATAN!:( MINUS!:(X) , K)) ELSE

    BEGIN INTEGER K2;  SCALAR ONE,PI4,Y,Z;

          K2 :=K+2;
          ONE:=CONV!:I2BF(1);
          PI4:=TIMES!:( !:PI(K2) , CONV!:S2BF("0.25"));
          IF EQUAL!:(X,ONE) THEN RETURN ROUND!:MT(PI4,K);
          IF GREATERP!:(X,ONE) THEN RETURN ROUND!:MT
             ( DIFFERENCE!:( PLUS!:(PI4,PI4) ,
               ATAN!:( DIVIDE!:(ONE,X,K2) , K+1)) , K);

          IF LESSP!:(X , CONV!:S2BF("0.42")) THEN GOTO AT;

          Y:=PLUS!:(ONE , CUT!:MT( TIMES!:(X,X) , K2));
          Y:=PLUS!:(ONE , SQRT!:(Y,K2));
          Y:=ATAN!:( DIVIDE!:(X,Y,K2) , K+1);
          RETURN ROUND!:MT( TIMES!:(Y , CONV!:I2BF(2)) , K);

    AT: BEGIN INTEGER M,N,NCUT;  SCALAR DCUT,TM,ZI;

              NCUT:=K2-MIN(0 , ORDER!:(X)+1);
              Y:=TM:=ZI:=X;
              Z:=MINUS!:( CUT!:EP( TIMES!:(X,X) , -NCUT));
              DCUT:=MAKE!:BF(10,-NCUT);

              M:=1;
              WHILE GREATERP!:( ABS!:(TM) , DCUT) DO
                <<ZI:=CUT!:EP( TIMES!:(ZI,Z) , -NCUT);
                  N :=MAX(1 , K2+ORDER!:(ZI));
                  TM:=DIVIDE!:(ZI , CONV!:I2BF( M:=M+2) , N);
                  Y :=PLUS!:(Y,TM);  IF REMAINDER(M,20)=0 THEN
                                     Y:=CUT!:EP(Y,-NCUT) >>;
        END;

          RETURN ROUND!:MT(Y,K)
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE ARCSIN!:(X,K); %*************************$

   %==================================================$
   % This function calculates arcsin(x), the value of $
   %      the arcsine function at the point "x", with $
   %      the precision K, by solving                 $
   %         x = sin(y)  if  0 < x <= 0.72,  or       $
   %         SQRT(1-x**2) = sin(y)  if  0.72 < x,     $
   %      by Newton's iteration method.               $
   %      The answer is in the range [-PI/2 , PI/2].  $
   % X is a BIG-FLOAT representation of "x", IxI <= 1,$
   %      otherwise it is converted to a <BIG-FLOAT>. $
   % K is a positive integer.                         $
   %==================================================$

          IF NOT( BFP!:( X:=CONV!:A2BF(X))) OR
             GREATERP!:( ABS!:(X) , CONV!:I2BF(1)) OR
	     NOT( FIXP(K)) OR K<=0 THEN BFLERRMSG 'ARCSIN!: ELSE
          IF ZEROP!:(X) THEN CONV!:I2BF(0) ELSE
          IF MINUSP!:(X) THEN
             MINUS!:( ARCSIN!:( MINUS!:(X) , K)) ELSE

    BEGIN INTEGER K2;  SCALAR DCUT,ONE,PI2,Y;

          K2  :=K+2;
          DCUT:=MAKE!:BF(10 , -K2+ORDER!:(X)+1);
          ONE :=CONV!:I2BF(1);
          PI2 :=TIMES!:( !:PI(K2+2) , CONV!:S2BF("0.5"));

          IF LESSP!:( DIFFERENCE!:(ONE,X) , DCUT) THEN
             RETURN ROUND!:MT(PI2,K);
          IF GREATERP!:(X , CONV!:S2BF("0.72")) THEN GOTO AC
          ELSE GOTO AS;

      AC: Y:=CUT!:MT( DIFFERENCE!:(ONE , TIMES!:(X,X)) , K2);
          Y:=ARCSIN!:( SQRT!:(Y,K2) , K);
          RETURN ROUND!:MT( DIFFERENCE!:(PI2,Y) , K);

    AS: BEGIN INTEGER NFIG,N;  SCALAR CX,DX,DY,X0;

              DY:=ONE;
              Y :=X;

              NFIG:=1;
              WHILE NFIG<K2 OR GREATERP!:( ABS!:(DY) , DCUT) DO
                <<IF (NFIG:=2*NFIG) > K2 THEN NFIG:=K2;
                  X0:=SIN!:(Y,NFIG);
                  CX:=DIFFERENCE!:(ONE , TIMES!:(X0,X0));
                  CX:=CUT!:MT(CX,NFIG);
                  CX:=SQRT!:(CX,NFIG);
                  DX:=DIFFERENCE!:(X,X0);
                  N :=MAX(1 , NFIG+ORDER!:(DX));
                  DY:=DIVIDE!:(DX,CX,N);
                  Y :=PLUS!:(Y,DY) >>;
        END;

          RETURN ROUND!:MT(Y,K);
    END$



%*************************************************************$
 SYMBOLIC PROCEDURE ARCCOS!:(X,K); %*************************$

   %====================================================$
   % This function calculates arccos(x), the value of   $
   %      the arccosine function at the point "x", with $
   %      the precision K, by calculating               $
   %         arcsin(SQRT(1-x**2))  if  x > 0.72  and    $
   %         PI/2 - arcsin(x)  otherwise  by ARCSIN!:.  $
   %      The answer is in the range [0 , PI].          $
   % X is a BIG-FLOAT representation of "x", IxI <= 1,  $
   %      otherwise it is converted to a <BIG-FLOAT>.   $
   % K is a positive integer.                           $
   %====================================================$

          IF NOT( BFP!:( X:=CONV!:A2BF(X))) OR
             GREATERP!:( ABS!:(X) , CONV!:I2BF(1)) OR
	     NOT( FIXP(K)) OR K<=0 THEN BFLERRMSG 'ARCCOS!: ELSE

          IF LEQ!:(X , CONV!:S2BF("0.72")) THEN
             ROUND!:MT( DIFFERENCE!:
               ( TIMES!:( !:PI(K+1) , CONV!:S2BF("0.5")) ,
                 ARCSIN!:(X,K) ) , K)
          ELSE ARCSIN!:( SQRT!:( CUT!:MT
               ( DIFFERENCE!:( CONV!:I2BF(1) , TIMES!:(X,X)) ,
                 K+2) , K+2) , K)$



%*************************************************************$
 SYMBOLIC PROCEDURE ARCTAN!:(X,K); %*************************$

   %==================================================$
   % This function calculates arctan(x), the value of $
   %      the arctangent function at the point "x",   $
   %      with the precision K, by calculating        $
   %         arcsin(x/SQRT(1+x**2))  by ARCSIN!:      $
   %      The answer is in the range [-PI/2 , PI/2].  $
   % X is a BIG-FLOAT representation of any real "x", $
   %      otherwise it is converted to a <BIG-FLOAT>. $
   % K is a positive integer.                         $
   %==================================================$

          IF NOT( BFP!:( X:=CONV!:A2BF(X))) OR
	     NOT( FIXP(K)) OR K<=0 THEN BFLERRMSG 'ARCTAN!: ELSE
          IF MINUSP!:(X) THEN 
             MINUS!:( ARCTAN!:( MINUS!:(X) , K))

          ELSE ARCSIN!:( DIVIDE!:(X , SQRT!:( CUT!:MT
               ( PLUS!:( CONV!:I2BF(1) , TIMES!:(X,X)) ,
                   K+2) , K+2) , K+2) , K)$


END;
