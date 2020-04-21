        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
        %                                                       % 
        %                                                       % 
        %                         MINI                          % 
        %                 (A SMALL META SYSTEM)                 % 
        %                                                       % 
        %                                                       % 
        %          Copyright (c) Robert R. Kessler 1979         % 
        %          Mods: MLG, Feb 1981
        %                                                       % 
        %          This file is the support routines.           % 
        %          The file MINI.MIN contains the MINI          % 
        %          system self definition and MINI.SL           % 
        %          is the Standard LISP translation             % 
        %          of MINI.MIN.                                 % 
        %                                                       % 
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
 
GLOBAL '(!#KEY!# !#DIP!# !*MDEFN !#STACK!# !#STACK!-ELE!# !#TOK!# 
         !#TOKTYPE!# !#NTOK!# !#LABLIST!# SINGLEOP!* FAILURE!* INDEXLIST!* 
         !#RT!# !#GT!# !#RTNOW!# !#GTNOW!# !#IDTYPE!# !#NUMTYPE!# 
         !#STRTYPE!# !#GENLABLIST!#); 
 
%   Global description: 
%    !#DIP!#            - List of diphthongs for grammar being defined. 
%    FAILURE!*          - Value of failed match in pattern matcher. 
%    !#GENLABLIST!#     - List of generated labels used in push/pop lab.
%    !#GT!#             - List of grammar terminators for invoked grammar. 
%    !#GTNOW!#          - List of grammar terminators for grammar being def. 
%    !#IDTYPE!#         - The value of toktype for id's (0)
%    INDEXLIST!*        - List of number value pairs for pattern matcher. 
%    !#KEY!#            - List of key workds for grammar being defined. 
%    !#LABLIST!#        - The list of gensymed labels ($n). 
%    !*MDEFN            - Flag to MPRINT (ON) or EVAL (OFF) defined rule. 
%    !#NUMTYPE!#        - The value of toktype for numbers (2)
%    !#NTOK!#           - Next token, used for diphthong checking. 
%    !#RT!#             - List of rule terminators for invoked grammar. 
%    !#RTNOW!#          - List of rule terminators for grammar being defined. 
%    SINGLEOP!*         - The operator for any match pattern (&). 
%    !#STACK!#          - The stack list: push +, pop #n , ref ##n 
%    !#STACK!-ELE!#     - Used to pass info between stack operations 
%    !#SPECTYPE!#       - The value of toktype for specials (3)
%    !#STRTYPE!#        - The value of toktype for strings (1)
%    !#TOK!#            - The current token 
%    !#TOKTYPE!#        - The type of the token from rSYMBOLIC Parser
%                         (0-id, 1-str, 2-num, 3-special)
 
%   A grammar is defined by calling the function MINI with argument of 
%    the name of the goal rule.  i.e. MINI 'RUL redefines MINI itself. 
%   Then to invoke a grammar, you use INVOKE goal rule name.(INVOKE 'RUL). 
 
SYMBOLIC PROCEDURE MINI U; 
 << INVOKE 'RUL; 
    RULE!-DEFINE LIST ('PUT, LIST('QUOTE, U), '(QUOTE KEYS), 
       LIST('QUOTE, !#KEY!#)); 
    RULE!-DEFINE LIST ('PUT, LIST('QUOTE, U), '(QUOTE DIPS), 
       LIST('QUOTE, !#DIP!#)); 
    RULE!-DEFINE LIST ('PUT, LIST('QUOTE, U), '(QUOTE RTS), 
       LIST('QUOTE, !#RT!#)); 
    RULE!-DEFINE LIST ('PUT, LIST('QUOTE, U), '(QUOTE GTS), 
       LIST('QUOTE, !#GT!#)); 
    NIL >>; 
 
%   Invoke starts execution of a previously defined grammar. 

SYMBOLIC PROCEDURE INVOKE U; 
 BEGIN SCALAR X; 
    !#IDTYPE!# := 0;
    !#NUMTYPE!# := 2;
    !#STRTYPE!# := 1;
    FLAG (GET (U, 'KEYS), 'KEY); 
    DIPBLD (GET (U, 'DIPS)); 
    !#RTNOW!# := GET (U, 'RTS); 
    !#GTNOW!# := GET (U, 'GTS); 
    !#DIP!# := !#KEY!# := !#RT!# := !#GT!# := !#GENLABLIST!# := NIL; 
 L: !#STACK!# := NIL; 
    NEXT!-TOK(); 
    X := APPLY (U, NIL); 
    IF NULL X THEN 
    << ERROR!-PRINT(); 
       IF SCAN!-TERM() THEN <<PRIN2 ("Resuming scan"); TERPRI(); GOTO L>> >>; 
    REMFLAG (GET (U, 'KEYS), 'KEY) 
 END; 

% The following errs out if its argument is NIL

SYMBOLIC PROCEDURE FAIL!-NOT U;
U OR <<ERROR!-PRINT();
       ERROR(997,"Failure scanning a concatenation.")>>;


%   This procedure is called when a rule is defined.  If ON MDEFN then the 
%    value is MPRINTed, otherwise, it is evaled. 
 
SYMBOLIC PROCEDURE RULE!-DEFINE U; 
 << IF !*MDEFN THEN MPRINT U 
    ELSE EVAL U>>; 
 
%   Mprint is used so it may be redefined if something other than PRINT 
%    is desired when ON MDEFN is used. 
 
SYMBOLIC PROCEDURE MPRINT U; 
 << TERPRI(); PRINT U>>; 
 
%   Error-print is called when the major loop returns a NIL. 
 
SYMBOLIC PROCEDURE ERROR!-PRINT; 
  <<PRIN2 "ERROR in grammar, current token is "; 
    PRIN2 !#TOK!#; PRIN2 " and stack is "; 
    PRIN2 !#STACK!#; TERPRI() >>; 
 
%   Scan for a rule terminator or grammar terminator by fetching tokens. 
%    Returns T if a rule terminator is found and NIL for a grammar term. 
%    The rule terminator causes processing to continue after the terminator. 
%    The grammar terminator ceases processing. 
 
SYMBOLIC PROCEDURE SCAN!-TERM; 
 BEGIN SCALAR X; 
   PRIN2 ("Scanning for rule terminator: "); PRIN2 !#RTNOW!#; 
   PRIN2 (" or grammar terminator: "); PRIN2 !#GTNOW!#; 
   TERPRI(); 
  L: X := NEXT!-TOK(); 
   IF MEMQ (X, !#GTNOW!#) THEN RETURN NIL 
   ELSE IF MEMQ (X, !#RTNOW!#) THEN RETURN T 
   ELSE GOTO L 
 END; 
 
%   Add the argument to the current key list, if not already there. 
 
SYMBOLIC PROCEDURE ADDKEY U; 
  <<IF NOT MEMQ (U, !#KEY!#) THEN !#KEY!# := U . !#KEY!#; T>>; 
 
%   Add the argument to the current grammar terminator list. 
 
SYMBOLIC PROCEDURE ADDGTERM U; 
  <<IF NOT MEMQ (U, !#GT!#) THEN !#GT!# := U . !#GT!#; T>>; 
 
%   Add the argument to the current rule terminator list. 
 
SYMBOLIC PROCEDURE ADDRTERM U; 
  <<IF NOT MEMQ (U, !#RT!#) THEN !#RT!# := U . !#RT!#; T>>; 
 
%   This procedure will take a list of identifiers and flag them as 
%    diphthongs (2 character max). 
 
SYMBOLIC PROCEDURE DIPBLD U; 
 BEGIN SCALAR W, X, Y; 
   FOR EACH X IN U DO 
   << IF NOT MEMQ (X, !#DIP!#) THEN !#DIP!# := X . !#DIP!#; 
      Y := EXPLODE X; 
      Y := STRIP!! Y; % Take out the escapes; 
      W := GET (CAR Y, 'FOLLOW); % Property follow is list of legal dip terms; 
      PUT (CAR Y, 'FOLLOW, (LIST (CADR Y, X)) . W) >>; 
   RETURN T 
 END; 
 
SYMBOLIC PROCEDURE UNDIPBLD U; 
 BEGIN SCALAR W, X, Y; 
   FOR EACH X IN U DO 
   << Y := EXPLODE X; 
      Y := STRIP!! Y; % Take out the escapes; 
      REMPROP(CAR Y, 'FOLLOW) >>; 
   RETURN T 
 END; 
 
%   Following procedure will eliminate the escapes in a list 
 
SYMBOLIC PROCEDURE STRIP!! U; 
  IF PAIRP U THEN 
     IF CAR U EQ '!! THEN CADR U . STRIP!! CDDR U 
     ELSE CAR U . STRIP!! CDR U 
  ELSE NIL; 
 
%   Push something onto the stack; 
 
SYMBOLIC PROCEDURE PUSH U; 
  !#STACK!# := U . !#STACK!#; 
 
%   Reference a stack element 
 
SYMBOLIC PROCEDURE REF U; 
  SCAN!-STACK (U, !#STACK!#); 
 
%   Stack underflow is called then that error happens.  Right now, it errors 
%    out.  Future enhancement is to make it more friendly to the user. 
 
SYMBOLIC PROCEDURE STACK!-UNDERFLOW; 
  ERROR (4000, "Stack underflow"); 
 
%   Like above, a stack error has occured, so quit the game. 
 
SYMBOLIC PROCEDURE STACK!-ERROR; 
  ERROR (4001, "Error in stack access"); 
 
%   Search stack for the element U elements from the top (1 is top). 
 
SYMBOLIC PROCEDURE SCAN!-STACK (U, STK); 
  IF NULL STK THEN STACK!-UNDERFLOW () 
  ELSE IF U = 1 THEN CAR STK 
  ELSE SCAN!-STACK (U-1, CDR STK); 
 
%   Remove the Uth element from the stack (1 is the top). 
 
SYMBOLIC PROCEDURE EXTRACT U; 
  << !#STACK!# := FETCH!-STACK (U, !#STACK!#); 
     !#STACK!-ELE!# >>;  % Return the value found; 
 
%   Recursive routine to remove the Uth element from the stack. 
 
SYMBOLIC PROCEDURE FETCH!-STACK (U, STK); 
 BEGIN SCALAR X; 
  IF NULL STK THEN STACK!-UNDERFLOW () 
  ELSE IF U EQ 1 THEN <<!#STACK!-ELE!# := CAR STK; RETURN CDR STK>> 
  ELSE RETURN CAR STK . FETCH!-STACK (U-1, CDR STK) 
 END; 
 
%   Retrieve the length of the stack.  This is used to build a single 
%    list used in repetition.  It takes the top of the stack down to 
%    the stack length at the beginning to build the list.  Therefore, 
%    STK!-LENGTH must be called prior to calling BUILD!-REPEAT, which 
%    must be passed the value returned by the call to STK!-LENGTH. 
 
SYMBOLIC PROCEDURE STK!-LENGTH; 
   LENGTH !#STACK!#; 
 
%   The procedure to handle repetition by building a list out of the 
%    top n values on the stack. 
 
SYMBOLIC PROCEDURE BUILD!-REPEAT U; 
 BEGIN SCALAR V; 
   V := STK!-LENGTH(); 
   IF U > V THEN STACK!-ERROR() 
   ELSE IF U = V THEN PUSH NIL 
   ELSE IF U < V THEN 
   BEGIN SCALAR L, I;   % Build it for the top V-U elements 
     L := NIL; 
     FOR I := 1:(V-U) DO 
       L := (EXTRACT 1) . L; 
     PUSH L 
   END; 
   RETURN T 
 END; 
 
%   Actually get the next token, if !#NTOK!# has a value then use that, 
%    else call your favorite token routine. 
%   This routine must return an identifier, string or number. 
%   If U is T then don't break up a quoted list right now. 
 
SYMBOLIC PROCEDURE GET!-TOK U; 
 BEGIN SCALAR X;
  IF !#NTOK!# THEN 
  << X := !#NTOK!#;
     !#NTOK!# := NIL;
     RETURN X >>
  ELSE 
  << X := !%SCAN();
           % Scan sets the following codes:
           % 0 - ID, and thus was escapeed
           % 1 - STRING
           % 2 - Integer
           % 3 - Special (;, (, ), etc.)
           % Therefore, it is important to distinguish between
           %  the special and ID for key words.
     IF (X EQ 2) OR (X EQ 1) THEN RETURN (X . SCNVAL)
     ELSE RETURN (0 . INTERN SCNVAL) >> %//Ignore ESCAPE for now
 END;
 
%   Fetch the next token, if a diphthong, turn into an identifier 
 
SYMBOLIC PROCEDURE NEXT!-TOK; 
 BEGIN SCALAR X,Y;
   !#TOK!# := GET!-TOK(NIL); 
   !#TOKTYPE!# := CAR !#TOK!#;
   !#TOK!# := CDR !#TOK!#;
   IF (Y:=GET(!#TOK!#, 'FOLLOW)) THEN
     << !#NTOK!# := 0 . READCH();		% Use READCH since white space 
        IF X := ATSOC(CDR !#NTOK!#, Y) THEN	% within diphthong is illegal
      << !#TOK!# := CADR X;
         !#TOKTYPE!# := !#IDTYPE!# >>
      ELSE UNREADCH CDR !#NTOK!#;	% Push the character back for the
	 !#NTOK!# := NIL  >>;		% scanner if not part of diphthong
   RETURN !#TOK!# 
 END; 
 
SYMBOLIC PROCEDURE T!-NTOK;
 <<NEXT!-TOK(); 'T>>;

SYMBOLIC PROCEDURE EQTOK(X);	% Test Token Value
  EQUAL(!#TOK!#,X);		% maybe use EQ?

SYMBOLIC PROCEDURE EQTOK!-NEXT(X);
   EQTOK(X) AND T!-NTOK();

%   See if current token is an identifier and not a keyword.  If it is, 
%    then push onto the stack and fetch the next token. 
 
SYMBOLIC PROCEDURE ID; 
 IF !#TOKTYPE!# EQ !#IDTYPE!# AND NOT FLAGP(!#TOK!#,'KEY) THEN 
      <<PUSH !#TOK!#; 
        IF NOT (MEMQ (!#TOK!#, !#GTNOW!#)
                 OR MEMQ(!#TOK!#, !#RTNOW!#)) THEN
         NEXT!-TOK(); 
        T>> 
   ELSE NIL;
 
%   See if current token is an id whether or not it is a keyword. 
 
SYMBOLIC PROCEDURE ANYID; 
  IF (!#TOKTYPE!# EQ !#IDTYPE!#) THEN
%      (!#TOKTYPE!# EQ !#SPECTYPE!#) OR FLAGP(!#TOK!#, 'KEY) THEN 
      ANYTOK() ELSE NIL;
 
%   Always succeeds by pushing the current token onto the stack. 
 
SYMBOLIC PROCEDURE ANYTOK; 
 <<PUSH !#TOK!#; NEXT!-TOK(); T>>; 
 
%   Tests to see if the current token is a number, if so it pushes the 
%    number onto the stack and fetches the next token. 
 
SYMBOLIC PROCEDURE NUM; 
  IF (!#TOKTYPE!# EQ !#NUMTYPE!#) THEN ANYTOK() ELSE NIL;
 
%   Same as NUM, except for strings. 
 
SYMBOLIC PROCEDURE STR; 
 IF (!#TOKTYPE!# EQ !#STRTYPE!#) THEN ANYTOK() ELSE NIL;
 
%   Generate a label.  If the label has been previously generated, the 
%    return the old value.  (used by $n). 
 
SYMBOLIC PROCEDURE GENLAB U; 
 BEGIN SCALAR X; 
   IF X:=ASSOC(U, !#LABLIST!#) THEN RETURN CADR X; 
   X:=INTERN GENSYM(); 
   !#LABLIST!# := LIST(U, X) . !#LABLIST!#; 
   RETURN X 
 END; 
 
%   Push the current label lists so we don't get any conflicts.
LISP PROCEDURE PUSH!-LAB;
 << !#GENLABLIST!# := !#LABLIST!# . !#GENLABLIST!#; 
    !#LABLIST!# := NIL;
    T>>;

%   Pop label lists.
LISP PROCEDURE POP!-LAB;
 <<!#LABLIST!# := CAR !#GENLABLIST!#; 
   !#GENLABLIST!# := CDR !#GENLABLIST!#;
   T>>;

GLOBAL '(!*DO!#);
 
ON DO!#;
 
FLUID '(NEWENV!*);
 
%   RBMATCH will accept a list of rules and subject list and
%    search for a match on one of the rules.  Upon finding the
%    match, the body will be executed.
 
SYMBOLIC PROCEDURE RBMATCH (SUBLIST, RULESLIST, INITENV);
 BEGIN SCALAR  TEMP, ENVLIST, RULFOUND, RVAL, TRYAGAIN, SN;
%    IF NUMARGS() EQ 4 THEN TRYAGAIN := T ELSE TRYAGAIN := NIL;
%    IF NUMARGS() > 2 THEN INITENV := ARGUMENT(3) ELSE INITENV:=NIL;
    RVAL := FAILURE!*;
    WHILE RULESLIST DO
    <<
       RULFOUND := CAR RULESLIST;
       RULESLIST := CDR RULESLIST;
       ENVLIST := LIST (LIST (0, SUBLIST));
       IF INITENV THEN ENVLIST := APPEND (ENVLIST, INITENV);
       IF (NEWENV!* := PEVAL (CAR RULFOUND, SUBLIST, ENVLIST)) NEQ FAILURE!*
          THEN
          IF (TEMP := EVAL (LIST (CDR RULFOUND, 'NEWENV!*, NIL, NIL, NIL)))
               NEQ FAILURE!*
             THEN
                IF TEMP EQ 'FAIL THEN <<RVAL := NIL; RETURN NIL>>
                ELSE IF TRYAGAIN THEN
                << PRIN2T ("Success, will try again");
                   RVAL := APPEND (TEMP, RVAL) >>
                ELSE <<RVAL := TEMP;
                       RETURN TEMP >>
    >>;
    RETURN RVAL
 END RBMATCH;
%
%    PEVAL accepts a subjectlist, a pattern and an environment.
%     It then determines if the subjectlist matches the pattern
%     with the particular environment.  The pattern may contain
%     lists or variable expressions.  The variable expressions are
%     of two form:  & "ATOM" which will match a single list or
%     ATOM and & & "ATOM" which will test to see if the match is
%     equal to a previously matched item.
%;
SINGLEOP!* := '&;
 
FAILURE!* := NIL;
 
SYMBOLIC PROCEDURE PEVAL(P, S, ENV);
 IF P EQ S THEN LIST ENV
 ELSE IF EQCAR (S, '!#) AND !*DO!# THEN TST!#(P, S, ENV)
 ELSE IF ATOM P THEN NIL
 ELSE IF CAR P EQ SINGLEOP!* THEN TST!-SINGLE(P, S, ENV)
 ELSE IF ATOM S THEN NIL
 ELSE BEGIN SCALAR ENVL;
   ENVL := PEVAL (CAR P, CAR S, ENV);
   RETURN PEVALL (CDR P, CDR S, ENVL)
 END;
 
SYMBOLIC PROCEDURE PEVALL (P, S, ENVL);
 IF NULL ENVL THEN NIL
 ELSE IF NULL CDR ENVL THEN PEVAL (P, S, CAR ENVL)
 ELSE APPEND (PEVAL(P, S, CAR ENVL), PEVALL(P, S, CDR ENVL));
 
SYMBOLIC PROCEDURE TST!-SINGLE (P, S, ENV);
 BEGIN SCALAR IDX;
  IF LENGTH (IDX := CDR P) NEQ 1 THEN
  << IF CAR IDX EQ SINGLEOP!* THEN
       (IF EQUAL (S, CADR ASSOC (CADR IDX, ENV)) THEN
           RETURN LIST (ENV))
     ELSE IF MEMBER (S, CAR IDX) THEN
        RETURN LIST (LIST(CADR IDX, S) . ENV);
     RETURN FAILURE!* >>;
  RETURN  LIST (LIST (CAR IDX, S) . ENV)
 END;
 
SYMBOLIC PROCEDURE TST!# (P, S, ENV);
 BEGIN SCALAR OLST, N, ENVL, CLST, X;
  OLST := CADR S;
  N := CADDR S;
  ENVL := NIL;
 L: IF NULL OLST THEN RETURN ENVL;
  CLST := CAR OLST;
  X := PEVAL (P, CLST, ENV);
  OLST := CDR OLST;
  FOR EACH Y IN X DO
   ENVL := (LIST (N, CLST) . Y) . ENVL;
  GO TO L
 END;
  
END; 
 
 
 
