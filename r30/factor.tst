COMMENT FACTORIZER TEST FILE;

ARRAY A(20),B(20);
 
FACTORIZE(X**2-1,A);   %To make sure factorizer is loaded;

SYMBOLIC RANDOMIZE();   %To set RANDOM-SEED. This can be set direct if
			%deterministic behavior is required.

ALGEBRAIC PROCEDURE TEST(PROB,NFAC);
  BEGIN
    SCALAR BASETIME;
    P := FOR I:=1:NFAC PRODUCT A(I);
    WRITE "Problem number ",PROB;
    LISP BASETIME := TIME();
    LISP PRIN2T LIST("The random seed is",RANDOM!-SEED);
    M := FACTORIZE(P, B);
    LISP BASETIME := TIME() - BASETIME;
    LISP LPRI LIST("Time =",BASETIME);
    LISP TERPRI();
    Q := FOR I:=0:M PRODUCT B(I);
    IF (M=NFAC) AND (P=Q) THEN RETURN OK;
    WRITE "This example failed";
    FOR I:=0:M DO WRITE B(I);
    RETURN FAILED
  END;
 
 
% Wang test case 1;
 
A(1) := X*Y+Z+10$
A(2) := X*Z+Y+30$
A(3) := X+Y*Z+20$
TEST(1,3);
 
% Wang test case 2;
 
A(1) := X**3*Z+X**3*Y+Z-11$
A(2) := X**2*Z**2+X**2*Y**2+Y+90$
TEST(2,2);
 
 
% Wang test case 3;
 
A(1) := X**3*Y**2+X*Z**4+X+Z$
A(2) := X**3+X*Y*Z+Y**2+Y*Z**3$
TEST(3,2);
 
 
% Wang test case 4;
 
A(1) := X**2*Z+Y**4*Z**2+5$
A(2) := X*Y**3+Z**2$
A(3) := -X**3*Y+Z**2+3$
A(4) := X**3*Y**4+Z**2$
TEST(4,4);
 
 
% Wang test case 5;
 
A(1) := 3*U**2*X**3*Y**4*Z+X*Z**2+Y**2*Z**2+19*Y**2$
A(2) := U**2*Y**4*Z**2+X**2*Z+5$
A(3) := U**2+X**3*Y**4+Z**2$
TEST(5,3);

 
% Wang test case 6;
 
A(1) := W**4*X**5*Y**6-W**4*Z**3+W**2*X**3*Y+X*Y**2*Z**2$
A(2) := W**4*Z**6-W**3*X**3*Y-W**2*X**2*Y**2*Z**2+X**5*Z
	   -X**4*Y**2+Y**2*Z**3$
A(3) := -X**5*Z**3+X**2*Y**3+Y*Z$
TEST(6,3);
 
 
% Wang test case 7;
 
A(1) := X+Y+Z-2$
A(2) := X+Y+Z-2$
A(3) := X+Y+Z-3$
A(4) := X+Y+Z-3$
A(5) := X+Y+Z-3$
TEST(7,5); 
 
 
% Wang test case 8;
 
A(1) := -Z**31-W**12*Z**20+Y**18-Y**14+X**2*Y**2+X**21+W**2$
A(2) := -15*Y**2*Z**16+29*W**4*X**12*Z**3+21*X**3*Z**2+3*W**15*Y**20$
TEST(8,2);
 
 
 
% Wang test case 9;
 
A(1) := 18*U**2*W**3*X*Z**2+10*U**2*W*X*Y**3+15*U*Z**2+6*W**2*Y**3*Z**2$
A(2) := X$
A(3) := 25*U**2*W**3*Y*Z**4+32*U**2*W**4*Y**4*Z**3-
        48*U**2*X**2*Y**3*Z**3-2*U**2*W*X**2*Y**2+44*U*W*X*Y**4*Z**4-
        8*U*W*X**3*Z**4+4*W**2*X+11*W**2*X**3*Y+12*Y**3*Z**2$
A(4) := Z$
A(5) := Z$
A(6) := U$
A(7) := U$
A(8) := U$
A(9) := U$
TEST(9,9);
 
 
 
% Wang test case 10;
 
A(1) := 31*U**2*X*Z+35*W**2*Y**2+40*W*X**2+6*X*Y$
A(2) := 42*U**2*W**2*Y**2+47*U**2*W**2*Z+22*U**2*W**2+9*U**2*W*X**2+21
	*U**2*W*X*Y*Z+37*U**2*Y**2*Z+U**2*W**2*X*Y**2*Z**2+8*U**2*W**2
	*Z**2+24*U**2*W*X*Y**2*Z**2+24*U**2*X**2*Y*Z**2+12*U**2*X*Y**2
	*Z**2+13*U*W**2*X**2*Y**2+27*U*W**2*X**2*Y+39*U*W*X*Z+43*U*
	X**2*Y+44*U*W**2* Z**2+37*W**2*X*Y+29*W**2*Y**2+31*W**2*Y*Z**2
	+12*W*X**2*Y*Z+43*W*X*Y*Z**2+22*X*Y**2+23*X*Y*Z+24*X*Y+41*Y**2
	*Z$
TEST(10,2);
 
 
 
% Wang test case 11;
 
A(1) := -36*U**2*W**3*X*Y*Z**3-31*U**2*W**3*Y**2+20*U**2*W**2*X**2*Y**2
	*Z**2-36*U**2*W*X*Y**3*Z+46*U**2*W*X+9*U**2*Y**2-36*U*W**2*Y**3
	+9*U*W*Y**3-5*U*W*X**2*Y**3+48*U*W*X**3*Y**2*Z+23*U*W*X**3*Y**2
	-43*U*X**3*Y**3*Z**3-46*U*X**3*Y**2+29*W**3*X*Y**3*Z**2-
	14*W**3*X**3*Y**3*Z**2-45*X**3-8*X*Y**2$
A(2) := 13*U**3*W**2*X*Y*Z**3-4*U*X*Y**2-W**3*Z**3-47*X*Y$
A(3) := X$
A(4) := Y$
TEST(11,4);
 
 
 
 
% Wang test case 12; 
A(1) := X+Y+Z-3$
A(2) := X+Y+Z-3$
A(3) := X+Y+Z-3$
TEST(12,3);
 
 
 
 
% Wang test case 13;
 
A(1) := 2*W*Z+45*X**3-9*Y**3-Y**2+3*Z**3$
A(2) := W**2*Z**3-W**2+47*X*Y$
TEST(13,2);
 
 
 
 
% Wang test case 14;
 
A(1) := 18*X**4*Y**5+41*X**4*Y**2-37*X**4+26*X**3*Y**4+38*X**2*Y**4-29*
        X**2*Y**3-22*Y**5$
A(2) := 33*X**5*Y**6-22*X**4+35*X**3*Y+11*Y**2$
TEST(14,2);
 
 
 
 
% Wang test case 15;
 
A(1) := 12*W**2*X*Y*Z**3-W**2*Z**3+W**2-29*X-3*X*Y**2$
A(2) := 14*W**2*Y**2+2*W*Z+18*X**3*Y-8*X*Y**2-Y**2+3*Z**3$
A(3) := Z$
A(4) := Z$
A(5) := Y$
A(6) := Y$
A(7) := Y$
A(8) := X$
A(9) := X$
A(10) := X$
A(11) := X$
A(12) := X$
A(13) := X$
TEST(15,13);
 
 
% Test 16 - the 40th degree polynomial that comes from
% SIGSAM problem number 7;
 
A(1) := 8192*Y**10+20480*Y**9+58368*Y**8-161792*Y**7+198656*Y**6+
        199680*Y**5-414848*Y**4-4160*Y**3+171816*Y**2-48556*Y+469$
A(2) := 8192*Y**10+12288*Y**9+66560*Y**8-22528*Y**7-138240*Y**6+
        572928*Y**5-90496*Y**4-356032*Y**3+113032*Y**2+23420*Y-8179$
A(3) := 4096*Y**10+8192*Y**9+1600*Y**8-20608*Y**7+20032*Y**6+87360*Y**5-
	105904*Y**4+18544*Y**3+11888*Y**2-3416*Y+1$
A(4) := 4096*Y**10+8192*Y**9-3008*Y**8-30848*Y**7+21056*Y**6+146496*
        Y**5-221360*Y**4+1232*Y**3+144464*Y**2-78488*Y+11993$
TEST(16,4);
 
% Test 17 - taken from Erich Kaltofen's thesis. This polynomial
% splits mod all possible primes p;
 
A(1) := X**25-25*X**20-3500*X**15-57500*X**10+21875*X**5-3125$
TEST(17,1);
 
% Test 18 - another 'hard-to-factorize' univariate;
 
A(1) := X**18+9*X**17+45*X**16+126*X**15+189*X**14+27*X**13-
	540*X**12-1215*X**11+1377*X**10+15444*X**9+46899*X**8+
	90153*X**7+133893*X**6+125388*X**5+29160*X**4-
	32076*X**3+26244*X**2-8748*X+2916$
TEST(18,1);
 
% Test 19 - another example chosen to lead to false splits mod p;
 
A(1) := X**16+4*X**12-16*X**11+80*X**9+2*X**8+160*X**7+
	128*X**6-160*X**5+28*X**4-48*X**3+128*X**2-16*X+1$
A(2) := X**16+4*X**12+16*X**11-80*X**9+2*X**8-160*X**7+
	128*X**6+160*X**5+28*X**4+48*X**3+128*X**2+16*X+1$
TEST(19,2);
 
 
% End of all tests;
 
 
END;
