
% {DSK}PERMUTE.PSL;1  5-FEB-83 15:53:01 





(GLISPOBJECTS


(HISTOGRAM (LISTOBJECT (MIN INTEGER)
		       (MAX INTEGER)
		       (TOTAL INTEGER)
		       (COUNTS (LISTOF INTEGER)))
PROP    ((PEAKS HISTO-PEAKS))
MSG     ((CREATE HISTO-CREATE)
	 (+ HISTO-ADD)))


(PERMUTATION (LISTOF INTEGER)
PROP    ((LENGTH LENGTH)
	 (INVERSE PERM-INVERSE RESULT PERMUTATION))
MSG     ((* COMPOSEBITSHUFFLES RESULT PERMUTATION)))

)


(SETQ PERM3S '((7 3 5 1 6 2 4 0)
	       (7 5 3 1 6 4 2 0)
	       (7 3 6 2 5 1 4 0)
	       (7 5 6 4 3 1 2 0)
	       (7 6 3 2 5 4 1 0)))
(SETQ FOLD3S '((3 2 1 0 7 6 5 4)
	       (5 4 7 6 1 0 3 2)
	       (6 7 4 5 2 3 0 1)))
(SETQ PERM4S '((15 7 11 3 13 5 9 1 14 6 10 2 12 4 8 0)
	       (15 11 7 3 13 9 5 1 14 10 6 2 12 8 4 0)
	       (15 7 13 5 11 3 9 1 14 6 12 4 10 2 8 0)
	       (15 11 13 9 7 3 5 1 14 10 12 8 6 2 4 0)
	       (15 13 7 5 11 9 3 1 14 12 6 4 10 8 2 0)
	       (15 13 11 9 7 5 3 1 14 12 10 8 6 4 2 0)
	       (15 7 11 3 14 6 10 2 13 5 9 1 12 4 8 0)
	       (15 11 7 3 14 10 6 2 13 9 5 1 12 8 4 0)
	       (15 7 13 5 14 6 12 4 11 3 9 1 10 2 8 0)
	       (15 11 13 9 14 10 12 8 7 3 5 1 6 2 4 0)
	       (15 13 7 5 14 12 6 4 11 9 3 1 10 8 2 0)
	       (15 13 11 9 14 12 10 8 7 5 3 1 6 4 2 0)
	       (15 7 14 6 11 3 10 2 13 5 12 4 9 1 8 0)
	       (15 11 14 10 7 3 6 2 13 9 12 8 5 1 4 0)
	       (15 7 14 6 13 5 12 4 11 3 10 2 9 1 8 0)
	       (15 11 14 10 13 9 12 8 7 3 6 2 5 1 4 0)
	       (15 13 14 12 7 5 6 4 11 9 10 8 3 1 2 0)
	       (15 13 14 12 11 9 10 8 7 5 6 4 3 1 2 0)
	       (15 14 7 6 11 10 3 2 13 12 5 4 9 8 1 0)
	       (15 14 11 10 7 6 3 2 13 12 9 8 5 4 1 0)
	       (15 14 7 6 13 12 5 4 11 10 3 2 9 8 1 0)
	       (15 14 11 10 13 12 9 8 7 6 3 2 5 4 1 0)
	       (15 14 13 12 7 6 5 4 11 10 9 8 3 2 1 0)))
(SETQ FOLD4S '((7 6 5 4 3 2 1 0 15 14 13 12 11 10 9 8)
	       (11 10 9 8 15 14 13 12 3 2 1 0 7 6 5 4)
	       (13 12 15 14 9 8 11 10 5 4 7 6 1 0 3 2)
	       (14 15 12 13 10 11 8 9 6 7 4 5 2 3 0 1)))

% edited: 27-DEC-82 15:36 
% Generate a list of all permutations of length N. The identity 
%   permutation is always the first member of the list. 
(DG ALLPERMS (N:INTEGER)
(RESULT (LISTOF PERMUTATION))
% (SPECVARS LST) 
(PROG (LST)
      (IF N>5 (ERROR 0 "TOO MANY PERMUTATIONS!"))
      (GENPERMS NIL (IDPERM N))
      (RETURN LST)))


% edited: 28-DEC-82 11:26 
% Convert N to a list of bit values. 
(DG BINLIST (N,NBITS:INTEGER)
(RESULT (LISTOF INTEGER))(PROG (L I BIT)
			       (I_0)
			       (BIT_1)
			       (WHILE I<NBITS DO
				      (L+_ (IF (LOGAND N BIT)
					       =0 THEN 0 ELSE 1))
				      (I_+1)
				      (BIT_+BIT))
			       (RETURN L)))


% edited:  6-MAY-82 16:33 
% Compute a bit-shuffle of the input according to the specification 
%   list LST. LST gives, for each output bit in order, the input bit 
%   from which it comes. 
(DE BITSHUFFLE (INPUT LST)
(PROG (RES)
      (SETQ RES 0)
      (MAPC LST (FUNCTION (LAMBDA (X)
			    (SETQ RES (PLUS (PLUS RES RES)
					    (COND
					      ((NULL X)
						0)
					      ((NOT (NUMBERP X))
						1)
					      ((ZEROP (LOGAND INPUT
							      (BITPICK X)))
						0)
					      (T 1)))))))
      (RETURN RES)))


% edited: 23-JUN-82 15:17 
% Compose two bitshuffles to produce a single bitshuffle which is 
%   equivalent. 
(DE COMPOSEBITSHUFFLES (FIRST SECOND)
(PROG (L)
      (COND ((NOT (EQUAL (SETQ L (LENGTH FIRST))
			 (LENGTH SECOND)))
	     (ERROR 0 NIL)))
      (RETURN (MAPCAR SECOND (FUNCTION (LAMBDA (X)
					 (COND
					   ((FIXP X)
					     (CAR (PNth FIRST
							(DIFFERENCE L X))))
					   (T X))))))))


% edited: 27-DEC-82 15:44 
(DE DOBITSHUFFLE (INT PERM)
(BITSHUFFLE INT PERM))


% edited: 27-DEC-82 15:38 
% Generate all permutations consisting of the list PREV followed by 
%   all permutations of the list L. The permutations which are 
%   generated are added to the global LST. Called by ALLPERMS. 
(DG GENPERMS (PREV,L: (LISTOF INTEGER))
(GLOBAL LST: (LISTOF PERMUTATION))(PROG (I TMP N)
					(IF ~L THEN LST+_PREV (RETURN NIL))
					(N_ (LENGTH L))
					(I_0)
					(WHILE (I_+1)
					       <=N DO
					       (TMP_ (CAR (PNth L I)))
					       (GENPERMS (PREV+TMP)
							 (L - TMP)))))


% edited: 30-DEC-82 13:26 
(DG HISTO-ADD (H:HISTOGRAM N:INTEGER)
(IF N>MAX OR N<MIN THEN (ERROR 0 NIL)
    ELSE TOTAL_+1 (CAR (PNth COUNTS (N - MIN + 1)))
    _+1)H)


% edited:  2-JAN-83 14:14 
(DG HISTO-CREATE (H:HISTOGRAM)
(RESULT HISTOGRAM)% Initialize a histogram. 
(TOTAL_0)(COUNTS_ (LISTOFC 0 (MAX - MIN + 1)))H)


% edited:  2-JAN-83 14:10 
(DG HISTO-PEAKS (H:HISTOGRAM)
(PROG (THRESH L MX N)
      (MX_0)
      (FOR X IN COUNTS (IF X>MX MX_X))
      (THRESH_MX/2)
      (N_MIN)
      (FOR X IN COUNTS DO (IF X>=THRESH L+_N)
	   N_+1)
      (RETURN (REVERSIP L))))


% edited: 28-DEC-82 11:23 
% Produce an identity permutation of length N. 
(DG IDPERM (N:INTEGER)
(RESULT PERMUTATION)(PROG (L I)
			  (SETQ I 0)
			  (WHILE I<N L+_I I_+1)
			  (RETURN L)))


% edited: 28-DEC-82 11:23 
% Make a list of N copies of the constant C. 
(DG LISTOFC (C N:INTEGER)
(RESULT (LISTOF ATOM))(PROG (I L)
			    (I_0)
			    (WHILE (I_+1)
				   <=N DO L+_C)
			    (RETURN L)))


% edited: 28-DEC-82 11:07 
% Log to the base 2 of an integer, rounded up. 
(DG LOG2 (N:INTEGER)
(RESULT INTEGER)(PROG (I M)
		      (SETQ I 0)
		      (SETQ M 1)
		      (WHILE M<N DO I_+1 M_+M)
		      (RETURN I)))


% edited: 28-DEC-82 11:03 
% Compute the permutation to be applied to the output of a boolean 
%   function of N inputs to account for negating the Mth input. 
(DG NEGINPPERM (N,M:INTEGER)
(RESULT PERMUTATION)(PROG (TWON TWOM I L)
			  (SETQ I 0)
			  (TWON_2^N)
			  (TWOM_2^M)
			  (WHILE I<TWON L+_ (IF (LOGAND I TWOM)
						~=0 THEN I - TWOM ELSE I+TWOM)
				 I_+1)
			  (RETURN L)))


% edited: 28-DEC-82 11:02 
% Create the set of permutations of the set of 2^N outputs 
%   corresponding to isomorphisms, i.e., renamings of the N inputs of 
%   a boolean function. The identity isomorphism is omitted. 
(DG OUTPERMS (N:INTEGER)
(RESULT (LISTOF PERMUTATION))(PROG (I TMP RES TWON)
				   (TWON_2^N)
				   (FOR X IN (CDR (ALLPERMS N))
					DO
					(I_0)
					(TMP_NIL)
					(WHILE I<TWON DO
					       (TMP+_ (DOBITSHUFFLE I X))
					       (I_+1))
					(RES+_TMP))
				   (RETURN RES)))


% edited:  2-SEP-82 10:47 
(DG PERM-INVERSE (P:PERMUTATION)
(RESULT PERMUTATION)% edited:  2-SEP-82 10:44 
% Compute the inverse of a permutation. 
(PROG (LST N M I J PP TMP)
      (SETQ I 0)
      (N_P:LENGTH)
      (WHILE I<N DO (J _ N - 1)
	     (PP_P)
	     (WHILE PP DO (IF (CAR PP)
			      =I THEN LST+_J PP_NIL ELSE TMP-_PP J_-1
			      (IF ~PP (ERROR 0 NIL))))
	     (I_+1))
      (RETURN LST)))

 (PUT 'BITSHUFFLE
      'GLRESULTTYPE
      'INTEGER)
 (PUT 'DOBITSHUFFLE
      'GLRESULTTYPE
      'INTEGER)