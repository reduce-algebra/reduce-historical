(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  PASCAL Based MINI-LISP/ compilation: V1
   Special Schlumberger Demo 
   All RIGHTS RESERVED
   COPYRIGHT (C) - 1981 - M. L. GRISS
      Computer Science Department
           University of Utah
 
 Do Not distribute with out written consent of M. L. Griss
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
(* !(!*ENTRY TCATCH EXPR !2!) *)
(*  EXPR TCATCH *)
procedure PAS31;
label
      102,
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE NIL !(FLUID THROWING!*!)!) *)
      idspace[131].val := nilref;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*LINK CATCH EXPR !1!) *)
     XCATCH;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMPNIL G!0!0!1!0!) *)
      IF R[1] = nilref THEN GOTO 100;
(* !(!*LOAD !1 !(FLUID THROWING!*!)!) *)
      R[1] := idspace[131].val;
(* !(!*JUMPNIL G!0!0!1!0!) *)
      IF R[1] = nilref THEN GOTO 100;
(* !(!*LOAD !1 !(FLUID THROWTAG!*!)!) *)
      R[1] := idspace[156].val;
(* !(!*JUMPN G!0!0!0!9 !0!) *)
      IF R[1] <> stk[st] THEN GOTO 101;
(* !(!*LBL G!0!0!1!0!) *)
100: 
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*JUMP G!0!0!0!4!) *)
      GOTO 102;
(* !(!*LBL G!0!0!0!9!) *)
101: 
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK THROW EXPR !1!) *)
     XTHROW;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LBL G!0!0!0!4!) *)
102: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY TTHROW EXPR !2!) *)
(*  EXPR TTHROW *)
procedure PAS32;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !3 !1!) *)
      R[3] := R[1];
(* !(!*LOAD !1 !(QUOTE T!)!) *)
      R[1] := trueref;
(* !(!*STORE !1 !(FLUID THROWING!*!)!) *)
      idspace[131].val := R[1];
(* !(!*LOAD !1 !3!) *)
      R[1] := R[3];
(* !(!*STORE !1 !(FLUID THROWTAG!*!)!) *)
      idspace[156].val := R[1];
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*LINK THROW EXPR !1!) *)
     XTHROW;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY GETD EXPR !1!) *)
(*  EXPR GETD *)
procedure PAS33;
label
      102,
      101,
      100;
begin
(* !(!*ALLOC !3!) *)
    alloc3;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMPC G!0!0!1!5 !1 IDTAG!) *)
      IF tag_of(R[1]) = IDTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!0!1!3!) *)
      GOTO 102;
(* !(!*LBL G!0!0!1!5!) *)
100: 
(* !(!*LOAD !2 !(QUOTE TYPE!)!) *)
      mkident(157,2);
(* !(!*LINK GET EXPR !2!) *)
     PAS230;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK FUNCELL EXPR !1!) *)
        R[1] := IDSPACE[INFO_OF(R[1])].FUNCELL;
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*JUMPT G!0!0!1!7!) *)
      IF R[1] <> nilref THEN GOTO 101;
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*JUMPNIL G!0!0!1!7!) *)
      IF R[1] = nilref THEN GOTO 101;
(* !(!*LOAD !1 !(QUOTE EXPR!)!) *)
      mkident(158,1);
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*LBL G!0!0!1!7!) *)
101: 
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*JUMPNIL G!0!0!1!3!) *)
      IF R[1] = nilref THEN GOTO 102;
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*LBL G!0!0!1!3!) *)
102: 
(* !(!*DEALLOC !3!) *)
      dealloc3;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY PUTD EXPR !3!) *)
(*  EXPR PUTD *)
procedure PAS34;
label
      105,
      104,
      103,
      102,
      101,
      100;
begin
(* !(!*ALLOC !3!) *)
    alloc3;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*STORE !3 !-!2!) *)
      store(3,2);
(* !(!*LOAD !2 !(QUOTE LOSE!)!) *)
      mkident(159,2);
(* !(!*LINK FLAGP EXPR !2!) *)
     PAS235;
(* !(!*JUMPNIL G!0!0!2!5!) *)
      IF R[1] = nilref THEN GOTO 100;
(* !(!*LOAD !4 !(QUOTE LOSE!)!) *)
      mkident(159,4);
(* !(!*LOAD !3 !(QUOTE FLAGGED!)!) *)
      mkident(160,3);
(* !(!*LOAD !2 !(QUOTE NOT!)!) *)
      mkident(161,2);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK LIST!4 EXPR !4!) *)
     PAS210;
(* !(!*LINK ERRPRT EXPR !1!) *)
     PAS233;
(* !(!*JUMP G!0!0!3!8!) *)
      GOTO 105;
(* !(!*LBL G!0!0!2!5!) *)
100: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK GETD EXPR !1!) *)
     PAS33;
(* !(!*JUMPNIL G!0!0!3!0!) *)
      IF R[1] = nilref THEN GOTO 101;
(* !(!*LOAD !3 !(QUOTE REDEFINED!)!) *)
      mkident(162,3);
(* !(!*LOAD !2 !0!) *)
      load(2,0);
(* !(!*LOAD !1 !(QUOTE FUNCTION!)!) *)
      mkident(163,1);
(* !(!*LINK LIST!3 EXPR !3!) *)
     PAS29;
(* !(!*LINK MSGPRT EXPR !1!) *)
     PAS234;
(* !(!*LBL G!0!0!3!0!) *)
101: 
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*JUMPC G!0!0!3!5 !1 CODETAG!) *)
      IF tag_of(R[1]) = CODETAG THEN GOTO 103;
(* !(!*LOAD !2 !(QUOTE LAMBDA!)!) *)
      mkident(154,2);
(* !(!*LINK EQCAR EXPR !2!) *)
     PAS240;
(* !(!*JUMPNIL G!0!0!3!3!) *)
      IF R[1] = nilref THEN GOTO 102;
(* !(!*LOAD !2 !(QUOTE !(EXPR FEXPR NEXPR MACRO!)!)!) *)
      R[2] := stk[2];
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK MEMQ EXPR !2!) *)
     PAS222;
(* !(!*JUMPT G!0!0!3!5!) *)
      IF R[1] <> nilref THEN GOTO 103;
(* !(!*LBL G!0!0!3!3!) *)
102: 
(* !(!*LOAD !4 !(QUOTE DEFINED!)!) *)
      mkident(164,4);
(* !(!*LOAD !3 !(QUOTE BE!)!) *)
      mkident(165,3);
(* !(!*LOAD !2 !(QUOTE CANT!)!) *)
      mkident(166,2);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK LIST!4 EXPR !4!) *)
     PAS210;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !(QUOTE !9!9!)!) *)
      mkint(99,1);
(* !(!*LINK ERROR EXPR !2!) *)
     PAS215;
(* !(!*LBL G!0!0!3!5!) *)
103: 
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*JUMPN G!0!0!3!7 !(QUOTE EXPR!)!) *)
      mkitem(IDTAG,158,RXX);
      IF R[1] <> RXX THEN GOTO 104;
(* !(!*STORE NIL !-!1!) *)
      storenil(1);
(* !(!*LBL G!0!0!3!7!) *)
104: 
(* !(!*LOAD !3 !-!1!) *)
      load(3,1);
(* !(!*LOAD !2 !(QUOTE TYPE!)!) *)
      mkident(157,2);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK PUT EXPR !3!) *)
     PAS224;
(* !(!*LOAD !2 !-!2!) *)
      load(2,2);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK SETFUNCELL EXPR !2!) *)
        IDSPACE[INFO_OF(R[1])].FUNCELL := R[2];
(* !(!*LBL G!0!0!3!8!) *)
105: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*DEALLOC !3!) *)
      dealloc3;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY REMD EXPR !1!) *)
(*  EXPR REMD *)
procedure PAS35;
label
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LINK GETD EXPR !1!) *)
     PAS33;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*JUMPNIL G!0!0!4!5!) *)
      IF R[1] = nilref THEN GOTO 100;
(* !(!*LOAD !2 !(QUOTE NIL!)!) *)
      R[2] := nilref;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK SETFUNCELL EXPR !2!) *)
        IDSPACE[INFO_OF(R[1])].FUNCELL := R[2];
(* !(!*LOAD !2 !(QUOTE TYPE!)!) *)
      mkident(157,2);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK REMPROP EXPR !2!) *)
     PAS231;
(* !(!*LBL G!0!0!4!5!) *)
100: 
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
procedure PAS36;
forward;
(* !(!*ENTRY PUTL EXPR !3!) *)
(*  EXPR PUTL *)
procedure PAS36;
label
      101,
      100;
begin
(* !(!*ALLOC !3!) *)
    alloc3;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*STORE !3 !-!2!) *)
      store(3,2);
(* !(!*JUMPC G!0!0!4!8 !1 PAIRTAG!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!0!4!9!) *)
      GOTO 101;
(* !(!*LBL G!0!0!4!8!) *)
100: 
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*LINK PUT EXPR !3!) *)
     PAS224;
(* !(!*LOAD !3 !-!2!) *)
      load(3,2);
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*LINK PUTL EXPR !3!) *)
     PAS36;
(* !(!*LBL G!0!0!4!9!) *)
101: 
(* !(!*DEALLOC !3!) *)
      dealloc3;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY DE FEXPR !1!) *)
(*  FEXPR DE *)
procedure PAS37;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !2 !(CDR !1!)!) *)
   ANYcdr(R[1],R[2]);
(* !(!*LOAD !1 !(QUOTE LAMBDA!)!) *)
      mkident(154,1);
(* !(!*LINK CONS EXPR !2!) *)
     XCONS;
(* !(!*LOAD !3 !1!) *)
      R[3] := R[1];
(* !(!*LOAD !2 !(QUOTE EXPR!)!) *)
      mkident(158,2);
(* !(!*LOAD !1 !(CAR !0!)!) *)
   ANYcar(stk[st],R[1]);
(* !(!*LINK PUTD EXPR !3!) *)
     PAS34;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY DF FEXPR !1!) *)
(*  FEXPR DF *)
procedure PAS38;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !2 !(CDR !1!)!) *)
   ANYcdr(R[1],R[2]);
(* !(!*LOAD !1 !(QUOTE LAMBDA!)!) *)
      mkident(154,1);
(* !(!*LINK CONS EXPR !2!) *)
     XCONS;
(* !(!*LOAD !3 !1!) *)
      R[3] := R[1];
(* !(!*LOAD !2 !(QUOTE FEXPR!)!) *)
      mkident(167,2);
(* !(!*LOAD !1 !(CAR !0!)!) *)
   ANYcar(stk[st],R[1]);
(* !(!*LINK PUTD EXPR !3!) *)
     PAS34;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY DN FEXPR !1!) *)
(*  FEXPR DN *)
procedure PAS39;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !2 !(CDR !1!)!) *)
   ANYcdr(R[1],R[2]);
(* !(!*LOAD !1 !(QUOTE LAMBDA!)!) *)
      mkident(154,1);
(* !(!*LINK CONS EXPR !2!) *)
     XCONS;
(* !(!*LOAD !3 !1!) *)
      R[3] := R[1];
(* !(!*LOAD !2 !(QUOTE NEXPR!)!) *)
      mkident(168,2);
(* !(!*LOAD !1 !(CAR !0!)!) *)
   ANYcar(stk[st],R[1]);
(* !(!*LINK PUTD EXPR !3!) *)
     PAS34;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY DM FEXPR !1!) *)
(*  FEXPR DM *)
procedure PAS310;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !2 !(CDR !1!)!) *)
   ANYcdr(R[1],R[2]);
(* !(!*LOAD !1 !(QUOTE LAMBDA!)!) *)
      mkident(154,1);
(* !(!*LINK CONS EXPR !2!) *)
     XCONS;
(* !(!*LOAD !3 !1!) *)
      R[3] := R[1];
(* !(!*LOAD !2 !(QUOTE MACRO!)!) *)
      mkident(169,2);
(* !(!*LOAD !1 !(CAR !0!)!) *)
   ANYcar(stk[st],R[1]);
(* !(!*LINK PUTD EXPR !3!) *)
     PAS34;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
procedure PAS311;
forward;
procedure PAS312;
forward;
(* !(!*ENTRY EVAL EXPR !1!) *)
(*  EXPR EVAL *)
procedure XEVAL;
label
      111,
      110,
      109,
      108,
      107,
      106,
      105,
      104,
      103,
      102,
      101,
      100;
begin
(* !(!*ALLOC !4!) *)
    alloc(4);
(* !(!*STORE NIL !-!1!) *)
      storenil(1);
(* !(!*STORE NIL !-!3!) *)
      storenil(3);
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LBL G!0!0!6!0!) *)
100: 
(* !(!*JUMPNC G!0!0!6!2 !1 IDTAG!) *)
      IF tag_of(R[1]) <> IDTAG THEN GOTO 101;
(* !(!*LINK VALUE EXPR !1!) *)
        R[1] := IDSPACE[INFO_OF(R[1])].VAL;
(* !(!*JUMP G!0!0!5!9!) *)
      GOTO 111;
(* !(!*LBL G!0!0!6!2!) *)
101: 
(* !(!*JUMPNC G!0!0!6!5 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 102;
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*JUMPN G!0!0!6!4 !(QUOTE LAMBDA!)!) *)
      mkitem(IDTAG,154,RXX);
      IF R[1] <> RXX THEN GOTO 103;
(* !(!*LBL G!0!0!6!5!) *)
102: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMP G!0!0!5!9!) *)
      GOTO 111;
(* !(!*LBL G!0!0!6!4!) *)
103: 
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*JUMPN G!0!0!6!7 !(QUOTE QUOTE!)!) *)
      mkitem(IDTAG,138,RXX);
      IF R[1] <> RXX THEN GOTO 104;
(* !(!*LOAD !1 !(CAR !-!2!)!) *)
   ANYcar(stk[st-2],R[1]);
(* !(!*JUMP G!0!0!5!9!) *)
      GOTO 111;
(* !(!*LBL G!0!0!6!7!) *)
104: 
(* !(!*JUMPN G!0!0!6!9 !(QUOTE SETQ!)!) *)
      mkitem(IDTAG,170,RXX);
      IF R[1] <> RXX THEN GOTO 105;
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*LINK CADR EXPR !1!) *)
     PAS12;
(* !(!*LINK EVAL EXPR !1!) *)
     XEVAL;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !(CAR !-!2!)!) *)
   ANYcar(stk[st-2],R[1]);
(* !(!*LINK SET EXPR !2!) *)
     PAS269;
(* !(!*JUMP G!0!0!5!9!) *)
      GOTO 111;
(* !(!*LBL G!0!0!6!9!) *)
105: 
(* !(!*JUMPNC G!0!0!7!4 !1 IDTAG!) *)
      IF tag_of(R[1]) <> IDTAG THEN GOTO 109;
(* !(!*LOAD !2 !(QUOTE TYPE!)!) *)
      mkident(157,2);
(* !(!*LINK GET EXPR !2!) *)
     PAS230;
(* !(!*STORE !1 !-!3!) *)
      store(1,3);
(* !(!*JUMPNIL G!0!0!7!4!) *)
      IF R[1] = nilref THEN GOTO 109;
(* !(!*JUMPN G!0!0!7!7 !(QUOTE FEXPR!)!) *)
      mkitem(IDTAG,167,RXX);
      IF R[1] <> RXX THEN GOTO 106;
(* !(!*LOAD !2 !-!2!) *)
      load(2,2);
(* !(!*JUMP G!0!0!8!8!) *)
      GOTO 107;
(* !(!*LBL G!0!0!7!7!) *)
106: 
(* !(!*JUMPN G!0!0!8!0 !(QUOTE NEXPR!)!) *)
      mkitem(IDTAG,168,RXX);
      IF R[1] <> RXX THEN GOTO 108;
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*LINK EVLIS EXPR !1!) *)
     PAS311;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LBL G!0!0!8!8!) *)
107: 
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK APPLY!1 EXPR !2!) *)
     PAS312;
(* !(!*JUMP G!0!0!5!9!) *)
      GOTO 111;
(* !(!*LBL G!0!0!8!0!) *)
108: 
(* !(!*JUMPN G!0!0!7!4 !(QUOTE MACRO!)!) *)
      mkitem(IDTAG,169,RXX);
      IF R[1] <> RXX THEN GOTO 109;
(* !(!*LOAD !2 !0!) *)
      load(2,0);
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK APPLY!1 EXPR !2!) *)
     PAS312;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMP G!0!0!6!0!) *)
      GOTO 100;
(* !(!*LBL G!0!0!7!4!) *)
109: 
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*LINK EVLIS EXPR !1!) *)
     PAS311;
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*JUMPN G!0!0!8!6 !(QUOTE LIST!)!) *)
      mkitem(IDTAG,171,RXX);
      IF R[1] <> RXX THEN GOTO 110;
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*JUMP G!0!0!5!9!) *)
      GOTO 111;
(* !(!*LBL G!0!0!8!6!) *)
110: 
(* !(!*LOAD !2 !-!2!) *)
      load(2,2);
(* !(!*LINK APPLY EXPR !2!) *)
     PAS217;
(* !(!*LBL G!0!0!5!9!) *)
111: 
(* !(!*DEALLOC !4!) *)
      dealloc(4);
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY APPLY!1 EXPR !2!) *)
(*  EXPR APPLY1 *)
procedure PAS312;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*LINK NCONS EXPR !1!) *)
     XNCONS;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK APPLY EXPR !2!) *)
     PAS217;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY APPLY EXPR !2!) *)
(*  EXPR APPLY *)
procedure PAS217;
label
      101,
      100;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !2 !0!) *)
      store(2,0);
(* !(!*LINK FUNCELL EXPR !1!) *)
        R[1] := IDSPACE[INFO_OF(R[1])].FUNCELL;
(* !(!*LOAD !2 !0!) *)
      load(2,0);
(* !(!*JUMPNC G!0!0!9!4 !1 CODETAG!) *)
      IF tag_of(R[1]) <> CODETAG THEN GOTO 100;
(* !(!*LINK XAPPLY EXPR !2!) *)
     XXAPPLY;
(* !(!*JUMP G!0!0!9!2!) *)
      GOTO 101;
(* !(!*LBL G!0!0!9!4!) *)
100: 
(* !(!*LINK EVLAM EXPR !2!) *)
     PAS279;
(* !(!*LBL G!0!0!9!2!) *)
101: 
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY EVLIS EXPR !1!) *)
(*  EXPR EVLIS *)
procedure PAS311;
label
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMPC G!0!0!9!9 !1 PAIRTAG!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LINK EVAL EXPR !1!) *)
     XEVAL;
(* !(!*JUMP G!0!1!0!0!) *)
      GOTO 101;
(* !(!*LBL G!0!0!9!9!) *)
100: 
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*LINK EVAL EXPR !1!) *)
     XEVAL;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*LINK EVLIS EXPR !1!) *)
     PAS311;
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*LBL G!0!1!0!0!) *)
101: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY PROGN FEXPR !1!) *)
(*  FEXPR PROGN *)
procedure PAS313;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LINK P!.N EXPR !1!) *)
     PAS278;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY PROG!2 EXPR !2!) *)
(*  EXPR PROG2 *)
procedure PAS314;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY P!.N EXPR !1!) *)
(*  EXPR P.N *)
procedure PAS278;
label
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LBL G!0!1!0!9!) *)
100: 
(* !(!*JUMPNC G!0!1!0!8 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 101;
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*LINK EVAL EXPR !1!) *)
     XEVAL;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMP G!0!1!0!9!) *)
      GOTO 100;
(* !(!*LBL G!0!1!0!8!) *)
101: 
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY RETURN FEXPR !1!) *)
(*  FEXPR RETURN *)
procedure PAS315;
begin
(* !(!*ALLOC !0!) *)
(* !(!*STORE NIL !(FLUID P!.P!)!) *)
      idspace[172].val := nilref;
(* !(!*LINK P!.N EXPR !1!) *)
     PAS278;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !(QUOTE !$PROG!$!)!) *)
      mkident(173,1);
(* !(!*LINK TTHROW EXPR !2!) *)
     PAS32;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY GO FEXPR !1!) *)
(*  FEXPR GO *)
procedure PAS316;
label
      102,
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE NIL !-!1!) *)
      storenil(1);
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LBL G!0!1!1!9!) *)
100: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMPC G!0!1!2!0 !1 IDTAG!) *)
      IF tag_of(R[1]) = IDTAG THEN GOTO 101;
(* !(!*LOAD !3 !(QUOTE LABEL!)!) *)
      mkident(174,3);
(* !(!*LOAD !2 !(QUOTE NOT!)!) *)
      mkident(161,2);
(* !(!*LINK LIST!3 EXPR !3!) *)
     PAS29;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !(QUOTE !1!1!0!0!)!) *)
      mkint(1100,1);
(* !(!*LINK ERROR EXPR !2!) *)
     PAS215;
(* !(!*LBL G!0!1!2!0!) *)
101: 
(* !(!*LOAD !2 !(FLUID P!.G!)!) *)
      R[2] := idspace[175].val;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK ATSOC EXPR !2!) *)
     PAS223;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*JUMPNC G!0!1!2!6 !1 ATOM!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 102;
(* !(!*LOAD !4 !(QUOTE LABEL!)!) *)
      mkident(174,4);
(* !(!*LOAD !3 !(QUOTE A!)!) *)
      mkident(66,3);
(* !(!*LOAD !2 !(QUOTE NOT!)!) *)
      mkident(161,2);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK LIST!4 EXPR !4!) *)
     PAS210;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !(QUOTE !1!1!0!1!)!) *)
      mkint(1101,1);
(* !(!*LINK ERROR EXPR !2!) *)
     PAS215;
(* !(!*LBL G!0!1!2!6!) *)
102: 
(* !(!*JUMPNC G!0!1!1!9 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(CDR !-!1!)!) *)
   ANYcdr(stk[st-1],R[1]);
(* !(!*STORE !1 !(FLUID P!.P!)!) *)
      idspace[172].val := R[1];
(* !(!*LOAD !2 !(QUOTE NIL!)!) *)
      R[2] := nilref;
(* !(!*LOAD !1 !(QUOTE !$PROG!$!)!) *)
      mkident(173,1);
(* !(!*LINK TTHROW EXPR !2!) *)
     PAS32;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY PROGG!0!1!3!1 EXPR !1!) *)
(*  EXPR PROGG0131 *)
procedure PAS317;
label
      101,
      100;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*JUMPNC G!0!1!3!4 !1 IDTAG!) *)
      IF tag_of(R[1]) <> IDTAG THEN GOTO 100;
(* !(!*LOAD !2 !(FLUID P!.G!)!) *)
      R[2] := idspace[175].val;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK CONS EXPR !2!) *)
     XCONS;
(* !(!*STORE !1 !(FLUID P!.G!)!) *)
      idspace[175].val := R[1];
(* !(!*JUMP G!0!1!3!3!) *)
      GOTO 101;
(* !(!*LBL G!0!1!3!4!) *)
100: 
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LBL G!0!1!3!3!) *)
101: 
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
procedure PAS318;
forward;
(* !(!*ENTRY PROG FEXPR !1!) *)
(*  FEXPR PROG *)
procedure PAS319;
label
      103,
      102,
      101,
      100;
begin
(* !(!*ALLOC !7!) *)
    alloc(7);
(* !(!*STORE NIL !-!3!) *)
      storenil(3);
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !1 !(FLUID P!.P!)!) *)
      R[1] := idspace[172].val;
(* !(!*STORE !1 !-!5!) *)
      store(1,5);
(* !(!*LOAD !1 !(FLUID P!.G!)!) *)
      R[1] := idspace[175].val;
(* !(!*STORE !1 !-!6!) *)
      store(1,6);
(* !(!*LOAD !1 !(CAR !0!)!) *)
   ANYcar(stk[st],R[1]);
(* !(!*LINK LENGTH EXPR !1!) *)
     PAS232;
(* !(!*STORE !1 !-!4!) *)
      store(1,4);
(* !(!*LOAD !1 !(CAR !0!)!) *)
   ANYcar(stk[st],R[1]);
(* !(!*LINK PBINDN EXPR !1!) *)
     PAS280;
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*STORE !1 !(FLUID P!.P!)!) *)
      idspace[172].val := R[1];
(* !(!*STORE NIL !(FLUID P!.G!)!) *)
      idspace[175].val := nilref;
(* !(!*LOAD !2 !(QUOTE PROGG!0!1!3!1!)!) *)
      mkident(176,2);
(* !(!*LINK MAP EXPR !2!) *)
     PAS318;
(* !(!*STORE NIL !(FLUID THROWING!*!)!) *)
      idspace[131].val := nilref;
(* !(!*LOAD !1 !(QUOTE !$PROG!$!)!) *)
      mkident(173,1);
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*LBL G!0!1!3!8!) *)
100: 
(* !(!*LOAD !1 !(FLUID P!.P!)!) *)
      R[1] := idspace[172].val;
(* !(!*JUMPNIL G!0!1!3!7!) *)
      IF R[1] = nilref THEN GOTO 101;
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*JUMPN G!0!1!3!7 !(QUOTE !$PROG!$!)!) *)
      mkitem(IDTAG,173,RXX);
      IF R[1] <> RXX THEN GOTO 101;
(* !(!*LOAD !1 !(CAR !(FLUID P!.P!)!)!) *)
   ANYcar(idspace[172].val,R[1]);
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*LOAD !1 !(CDR !(FLUID P!.P!)!)!) *)
   ANYcdr(idspace[172].val,R[1]);
(* !(!*STORE !1 !(FLUID P!.P!)!) *)
      idspace[172].val := R[1];
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*JUMPC G!0!1!3!8 !1 IDTAG!) *)
      IF tag_of(R[1]) = IDTAG THEN GOTO 100;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LINK TCATCH EXPR !2!) *)
     PAS31;
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*LOAD !1 !(FLUID THROWING!*!)!) *)
      R[1] := idspace[131].val;
(* !(!*JUMPNIL G!0!1!3!8!) *)
      IF R[1] = nilref THEN GOTO 100;
(* !(!*LOAD !1 !(FLUID THROWTAG!*!)!) *)
      R[1] := idspace[156].val;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*STORE !1 !-!3!) *)
      store(1,3);
(* !(!*JUMP G!0!1!3!8!) *)
      GOTO 100;
(* !(!*LBL G!0!1!3!7!) *)
101: 
(* !(!*LOAD !1 !-!4!) *)
      load(1,4);
(* !(!*LINK UNBINDN EXPR !1!) *)
     PAS276;
(* !(!*LOAD !1 !-!5!) *)
      load(1,5);
(* !(!*STORE !1 !(FLUID P!.P!)!) *)
      idspace[172].val := R[1];
(* !(!*LOAD !1 !-!6!) *)
      load(1,6);
(* !(!*STORE !1 !(FLUID P!.G!)!) *)
      idspace[175].val := R[1];
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*JUMPN G!0!1!4!9 !(QUOTE !$PROG!$!)!) *)
      mkitem(IDTAG,173,RXX);
      IF R[1] <> RXX THEN GOTO 102;
(* !(!*LOAD !1 !-!3!) *)
      load(1,3);
(* !(!*JUMP G!0!1!3!6!) *)
      GOTO 103;
(* !(!*LBL G!0!1!4!9!) *)
102: 
(* !(!*LOAD !2 !-!3!) *)
      load(2,3);
(* !(!*LINK TTHROW EXPR !2!) *)
     PAS32;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LBL G!0!1!3!6!) *)
103: 
(* !(!*DEALLOC !7!) *)
      dealloc(7);
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY WHILE FEXPR !1!) *)
(*  FEXPR WHILE *)
procedure PAS320;
label
      102,
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMPNC G!0!1!5!7 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 101;
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*LBL G!0!1!5!2!) *)
100: 
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK EVAL EXPR !1!) *)
     XEVAL;
(* !(!*JUMPNIL G!0!1!5!1!) *)
      IF R[1] = nilref THEN GOTO 102;
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*LINK P!.N EXPR !1!) *)
     PAS278;
(* !(!*JUMP G!0!1!5!2!) *)
      GOTO 100;
(* !(!*LBL G!0!1!5!7!) *)
101: 
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LBL G!0!1!5!1!) *)
102: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY AND FEXPR !1!) *)
(*  FEXPR AND *)
procedure PAS321;
label
      102,
      101,
      100;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMPC G!0!1!6!2 !1 PAIRTAG!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE T!)!) *)
      R[1] := trueref;
(* !(!*JUMP G!0!1!5!9!) *)
      GOTO 102;
(* !(!*LBL G!0!1!6!2!) *)
100: 
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*JUMPT G!0!1!6!4!) *)
      IF R[1] <> nilref THEN GOTO 101;
(* !(!*LOAD !1 !(CAR !0!)!) *)
   ANYcar(stk[st],R[1]);
(* !(!*LINK EVAL EXPR !1!) *)
     XEVAL;
(* !(!*JUMP G!0!1!5!9!) *)
      GOTO 102;
(* !(!*LBL G!0!1!6!4!) *)
101: 
(* !(!*LOAD !1 !(CAR !0!)!) *)
   ANYcar(stk[st],R[1]);
(* !(!*LINK EVAL EXPR !1!) *)
     XEVAL;
(* !(!*JUMPNIL G!0!1!5!9!) *)
      IF R[1] = nilref THEN GOTO 102;
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMP G!0!1!6!2!) *)
      GOTO 100;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LBL G!0!1!5!9!) *)
102: 
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY COND FEXPR !1!) *)
(*  FEXPR COND *)
procedure PAS322;
label
      105,
      104,
      103,
      102,
      101,
      100;
begin
(* !(!*ALLOC !3!) *)
    alloc3;
(* !(!*STORE NIL !-!2!) *)
      storenil(2);
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LBL G!0!1!7!0!) *)
100: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMPC G!0!1!7!2 !1 PAIRTAG!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 101;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!1!6!9!) *)
      GOTO 105;
(* !(!*LBL G!0!1!7!2!) *)
101: 
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*JUMPNC G!0!1!7!4 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 102;
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*LBL G!0!1!7!4!) *)
102: 
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*LINK EVAL EXPR !1!) *)
     XEVAL;
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*JUMPNIL G!0!1!7!0!) *)
      IF R[1] = nilref THEN GOTO 100;
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*JUMPNC G!0!1!8!0 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 103;
(* !(!*LOAD !1 !(CDR !1!)!) *)
   XCDR;
(* !(!*JUMPT G!0!1!7!9!) *)
      IF R[1] <> nilref THEN GOTO 104;
(* !(!*LBL G!0!1!8!0!) *)
103: 
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*JUMP G!0!1!6!9!) *)
      GOTO 105;
(* !(!*LBL G!0!1!7!9!) *)
104: 
(* !(!*LINK P!.N EXPR !1!) *)
     PAS278;
(* !(!*LBL G!0!1!6!9!) *)
105: 
(* !(!*DEALLOC !3!) *)
      dealloc3;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY OR FEXPR !1!) *)
(*  FEXPR OR *)
procedure PAS323;
label
      102,
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE NIL !-!1!) *)
      storenil(1);
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LBL G!0!1!8!5!) *)
100: 
(* !(!*JUMPNC G!0!1!9!0 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 101;
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*LINK EVAL EXPR !1!) *)
     XEVAL;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*JUMPT G!0!1!8!4!) *)
      IF R[1] <> nilref THEN GOTO 102;
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMP G!0!1!8!5!) *)
      GOTO 100;
(* !(!*LBL G!0!1!9!0!) *)
101: 
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LBL G!0!1!8!4!) *)
102: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY MAP EXPR !2!) *)
(*  EXPR MAP *)
procedure PAS318;
label
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*LBL G!0!1!9!4!) *)
100: 
(* !(!*JUMPNIL G!0!1!9!3!) *)
      IF R[1] = nilref THEN GOTO 101;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK APPLY!1 EXPR !2!) *)
     PAS312;
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMP G!0!1!9!4!) *)
      GOTO 100;
(* !(!*LBL G!0!1!9!3!) *)
101: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY MAPC EXPR !2!) *)
(*  EXPR MAPC *)
procedure PAS324;
label
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*LBL G!0!2!0!0!) *)
100: 
(* !(!*JUMPNIL G!0!1!9!9!) *)
      IF R[1] = nilref THEN GOTO 101;
(* !(!*LOAD !2 !(CAR !1!)!) *)
   ANYcar(R[1],R[2]);
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK APPLY!1 EXPR !2!) *)
     PAS312;
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMP G!0!2!0!0!) *)
      GOTO 100;
(* !(!*LBL G!0!1!9!9!) *)
101: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
procedure PAS325;
forward;
procedure PAS326;
forward;
(* !(!*ENTRY MAPCAN EXPR !2!) *)
(*  EXPR MAPCAN *)
procedure PAS325;
label
      101,
      100;
begin
(* !(!*ALLOC !3!) *)
    alloc3;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*JUMPNC G!0!2!0!5 !1 ATOM!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!2!0!6!) *)
      GOTO 101;
(* !(!*LBL G!0!2!0!5!) *)
100: 
(* !(!*LOAD !2 !(CAR !1!)!) *)
   ANYcar(R[1],R[2]);
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK APPLY!1 EXPR !2!) *)
     PAS312;
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*LINK MAPCAN EXPR !2!) *)
     PAS325;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*LINK NCONC EXPR !2!) *)
     PAS326;
(* !(!*LBL G!0!2!0!6!) *)
101: 
(* !(!*DEALLOC !3!) *)
      dealloc3;
(* !(!*EXIT!) *)
end;
procedure PAS327;
forward;
(* !(!*ENTRY MAPCAR EXPR !2!) *)
(*  EXPR MAPCAR *)
procedure PAS327;
label
      101,
      100;
begin
(* !(!*ALLOC !3!) *)
    alloc3;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*JUMPNC G!0!2!1!1 !1 ATOM!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!2!1!2!) *)
      GOTO 101;
(* !(!*LBL G!0!2!1!1!) *)
100: 
(* !(!*LOAD !2 !(CAR !1!)!) *)
   ANYcar(R[1],R[2]);
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK APPLY!1 EXPR !2!) *)
     PAS312;
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*LINK MAPCAR EXPR !2!) *)
     PAS327;
(* !(!*LOAD !2 !-!2!) *)
      load(2,2);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*LBL G!0!2!1!2!) *)
101: 
(* !(!*DEALLOC !3!) *)
      dealloc3;
(* !(!*EXIT!) *)
end;
procedure PAS328;
forward;
(* !(!*ENTRY MAPCON EXPR !2!) *)
(*  EXPR MAPCON *)
procedure PAS328;
label
      101,
      100;
begin
(* !(!*ALLOC !3!) *)
    alloc3;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*JUMPNC G!0!2!1!7 !1 ATOM!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!2!1!8!) *)
      GOTO 101;
(* !(!*LBL G!0!2!1!7!) *)
100: 
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK APPLY!1 EXPR !2!) *)
     PAS312;
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*LINK MAPCON EXPR !2!) *)
     PAS328;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*LINK NCONC EXPR !2!) *)
     PAS326;
(* !(!*LBL G!0!2!1!8!) *)
101: 
(* !(!*DEALLOC !3!) *)
      dealloc3;
(* !(!*EXIT!) *)
end;
procedure PAS329;
forward;
(* !(!*ENTRY MAPLIST EXPR !2!) *)
(*  EXPR MAPLIST *)
procedure PAS329;
label
      101,
      100;
begin
(* !(!*ALLOC !3!) *)
    alloc3;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*JUMPNC G!0!2!2!3 !1 ATOM!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!2!2!4!) *)
      GOTO 101;
(* !(!*LBL G!0!2!2!3!) *)
100: 
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK APPLY!1 EXPR !2!) *)
     PAS312;
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*LINK MAPLIST EXPR !2!) *)
     PAS329;
(* !(!*LOAD !2 !-!2!) *)
      load(2,2);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*LBL G!0!2!2!4!) *)
101: 
(* !(!*DEALLOC !3!) *)
      dealloc3;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY NCONC EXPR !2!) *)
(*  EXPR NCONC *)
procedure PAS326;
label
      103,
      102,
      101,
      100;
begin
(* !(!*ALLOC !3!) *)
    alloc3;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*JUMPNC G!0!2!3!1 !1 ATOM!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*JUMP G!0!2!2!9!) *)
      GOTO 103;
(* !(!*LBL G!0!2!3!1!) *)
100: 
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*LBL G!0!2!3!3!) *)
101: 
(* !(!*LOAD !1 !(CDR !-!2!)!) *)
   ANYcdr(stk[st-2],R[1]);
(* !(!*JUMPNC G!0!2!3!2 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 102;
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*JUMP G!0!2!3!3!) *)
      GOTO 101;
(* !(!*LBL G!0!2!3!2!) *)
102: 
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*LINK RPLACD EXPR !2!) *)
     XRPLACD;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LBL G!0!2!2!9!) *)
103: 
(* !(!*DEALLOC !3!) *)
      dealloc3;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY PUTC EXPR !3!) *)
(*  EXPR PUTC *)
procedure PAS330;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LINK PUT EXPR !3!) *)
     PAS224;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY FLUID EXPR !1!) *)
(*  EXPR FLUID *)
procedure PAS331;
begin
(* !(!*ALLOC !0!) *)
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
procedure PAS332;
forward;
(* !(!*ENTRY PRIN!2TL EXPR !1!) *)
(*  EXPR PRIN2TL *)
procedure PAS332;
label
      101,
      100;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMPC G!0!2!4!0 !1 PAIRTAG!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LINK TERPRI EXPR !0!) *)
     XTERPRI;
(* !(!*JUMP G!0!2!4!1!) *)
      GOTO 101;
(* !(!*LBL G!0!2!4!0!) *)
100: 
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*LINK PRIN!2 EXPR !1!) *)
     PAS129;
(* !(!*LOAD !1 !(QUOTE ! !)!) *)
      mkident(33,1);
(* !(!*LINK PRIN!2 EXPR !1!) *)
     PAS129;
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*LINK PRIN!2TL EXPR !1!) *)
     PAS332;
(* !(!*LBL G!0!2!4!1!) *)
101: 
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY FLOATP EXPR !1!) *)
(*  EXPR FLOATP *)
procedure PAS333;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY STRINGP EXPR !1!) *)
(*  EXPR STRINGP *)
procedure PAS334;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LINK IDP EXPR !1!) *)
     PAS24;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY VECTORP EXPR !1!) *)
(*  EXPR VECTORP *)
procedure PAS335;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY FLUIDP EXPR !1!) *)
(*  EXPR FLUIDP *)
procedure PAS336;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY INTERN EXPR !1!) *)
(*  EXPR INTERN *)
procedure PAS337;
begin
(* !(!*ALLOC !0!) *)
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY REMOB EXPR !1!) *)
(*  EXPR REMOB *)
procedure PAS338;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY GLOBAL EXPR !1!) *)
(*  EXPR GLOBAL *)
procedure PAS339;
label
      101,
      100;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LBL G!0!2!5!1!) *)
100: 
(* !(!*JUMPNIL G!0!2!5!0!) *)
      IF R[1] = nilref THEN GOTO 101;
(* !(!*LOAD !2 !(QUOTE GLOBAL!)!) *)
      mkident(177,2);
(* !(!*LINK FLAG EXPR !2!) *)
     PAS237;
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMP G!0!2!5!1!) *)
      GOTO 100;
(* !(!*LBL G!0!2!5!0!) *)
101: 
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY GLOBALP EXPR !1!) *)
(*  EXPR GLOBALP *)
procedure PAS340;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !2 !(QUOTE GLOBAL!)!) *)
      mkident(177,2);
(* !(!*LINK FLAGP EXPR !2!) *)
     PAS235;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY UNFLUID EXPR !1!) *)
(*  EXPR UNFLUID *)
procedure PAS341;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY GETV EXPR !2!) *)
(*  EXPR GETV *)
procedure PAS342;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY MKVECT EXPR !1!) *)
(*  EXPR MKVECT *)
procedure PAS343;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY PUTV EXPR !3!) *)
(*  EXPR PUTV *)
procedure PAS344;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY UPBV EXPR !1!) *)
(*  EXPR UPBV *)
procedure PAS345;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY DIGIT EXPR !1!) *)
(*  EXPR DIGIT *)
procedure PAS346;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY LITER EXPR !1!) *)
(*  EXPR LITER *)
procedure PAS347;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY READCH EXPR !1!) *)
(*  EXPR READCH *)
procedure PAS348;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY RDEVPR EXPR !0!) *)
(*  EXPR RDEVPR *)
procedure PAS349;
label
      101,
      100;
begin
(* !(!*ALLOC !0!) *)
(* !(!*JUMP G!0!2!6!8!) *)
      GOTO 100;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!2!6!5!) *)
      GOTO 101;
(* !(!*LBL G!0!2!6!8!) *)
100: 
(* !(!*LINK READ EXPR !0!) *)
     XREAD;
(* !(!*LINK EVAL EXPR !1!) *)
     XEVAL;
(* !(!*LINK PRINT EXPR !1!) *)
     XPRINT;
(* !(!*JUMP G!0!2!6!8!) *)
      GOTO 100;
(* !(!*LBL G!0!2!6!5!) *)
101: 
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY DSKIN EXPR !1!) *)
(*  EXPR DSKIN *)
procedure PAS350;
label
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !2 !(QUOTE INPUT!)!) *)
      mkident(134,2);
(* !(!*LINK OPEN EXPR !2!) *)
     XOPEN;
(* !(!*LINK RDS EXPR !1!) *)
     XRDS;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*LBL G!0!2!7!6!) *)
100: 
(* !(!*LINK READ EXPR !0!) *)
     XREAD;
(* !(!*LINK EVAL EXPR !1!) *)
     XEVAL;
(* !(!*LINK PRINT EXPR !1!) *)
     XPRINT;
(* !(!*LINK EOFP EXPR !1!) *)
     PAS132;
(* !(!*JUMPNIL G!0!2!7!6!) *)
      IF R[1] = nilref THEN GOTO 100;
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK RDS EXPR !1!) *)
     XRDS;
(* !(!*LINK CLOSE EXPR !1!) *)
     XCLOSE;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY !*FIRST!-PROCEDURE EXPR !0!) *)
(*  EXPR *FIRST-PROCEDURE *)
procedure FIRSTP;
label
      105,
      104,
      103,
      102,
      101,
      100;
begin
(* !(!*ALLOC !3!) *)
    alloc3;
(* !(!*STORE NIL !-!2!) *)
      storenil(2);
(* !(!*LOAD !1 !(QUOTE !(PASCAL LISP V!2 !- !1!5 FEB !1!9!8!2!)!)!) *)
      R[1] := stk[3];
(* !(!*LINK PRIN!2TL EXPR !1!) *)
     PAS332;
(* !(!*LOAD !1 !(QUOTE !(COPYRIGHT !(C!) !1!9!8!1 U UTAH!)!)!) *)
      R[1] := stk[4];
(* !(!*LINK PRIN!2TL EXPR !1!) *)
     PAS332;
(* !(!*LOAD !1 !(QUOTE !(ALL RIGHTS RESERVED!)!)!) *)
      R[1] := stk[5];
(* !(!*LINK PRIN!2TL EXPR !1!) *)
     PAS332;
(* !(!*LOAD !1 !(QUOTE !(LIST!)!)!) *)
      R[1] := stk[6];
(* !(!*STORE !1 !(FLUID NEXPRS!)!) *)
      idspace[178].val := R[1];
(* !(!*LOAD !3 !(QUOTE NEXPR!)!) *)
      mkident(168,3);
(* !(!*LOAD !2 !(QUOTE TYPE!)!) *)
      mkident(157,2);
(* !(!*LINK PUTL EXPR !3!) *)
     PAS36;
(* !(!*LOAD !1 !(QUOTE !(EXPR FEXPR NEXPR MACRO!)!)!) *)
      R[1] := stk[2];
(* !(!*STORE !1 !(FLUID PROCS!)!) *)
      idspace[179].val := R[1];
(* !(!*STORE NIL !-!1!) *)
      storenil(1);
(* !(!*LBL G!0!2!8!9!) *)
100: 
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*JUMPT G!0!2!8!8!) *)
      IF R[1] <> nilref THEN GOTO 101;
(* !(!*LINK READ EXPR !0!) *)
     XREAD;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LINK EOFP EXPR !1!) *)
     PAS132;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*JUMPT G!0!2!8!9!) *)
      IF R[1] <> nilref THEN GOTO 100;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK EVAL EXPR !1!) *)
     XEVAL;
(* !(!*JUMP G!0!2!8!9!) *)
      GOTO 100;
(* !(!*LBL G!0!2!8!8!) *)
101: 
(* !(!*LOAD !1 !(QUOTE !2!)!) *)
      mkint(2,1);
(* !(!*LINK RDS EXPR !1!) *)
     XRDS;
(* !(!*STORE NIL !-!1!) *)
      storenil(1);
(* !(!*LBL G!0!2!9!5!) *)
102: 
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*JUMPT G!0!2!9!4!) *)
      IF R[1] <> nilref THEN GOTO 105;
(* !(!*LOAD !1 !(QUOTE !3!)!) *)
      mkint(3,1);
(* !(!*LINK WRS EXPR !1!) *)
     XWRS;
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*LOAD !1 !(QUOTE !>!)!) *)
      mkident(63,1);
(* !(!*LINK PRIN!2 EXPR !1!) *)
     PAS129;
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*LINK WRS EXPR !1!) *)
     XWRS;
(* !(!*LINK READ EXPR !0!) *)
     XREAD;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !2 !(QUOTE QUIT!)!) *)
      mkident(180,2);
(* !(!*LINK EQCAR EXPR !2!) *)
     PAS240;
(* !(!*JUMPNIL G!0!2!9!9!) *)
      IF R[1] = nilref THEN GOTO 103;
(* !(!*LOAD !1 !(QUOTE T!)!) *)
      R[1] := trueref;
(* !(!*JUMP G!0!3!0!5!) *)
      GOTO 104;
(* !(!*LBL G!0!2!9!9!) *)
103: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK EOFP EXPR !1!) *)
     PAS132;
(* !(!*LBL G!0!3!0!5!) *)
104: 
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*JUMPT G!0!2!9!5!) *)
      IF R[1] <> nilref THEN GOTO 102;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK CATCH EXPR !1!) *)
     XCATCH;
(* !(!*LINK PRIN!2T EXPR !1!) *)
     PAS272;
(* !(!*JUMP G!0!2!9!5!) *)
      GOTO 102;
(* !(!*LBL G!0!2!9!4!) *)
105: 
(* !(!*LOAD !3 !(QUOTE LOOP!)!) *)
      mkident(181,3);
(* !(!*LOAD !2 !(QUOTE TOP!)!) *)
      mkident(182,2);
(* !(!*LOAD !1 !(QUOTE EXITING!)!) *)
      mkident(183,1);
(* !(!*LINK LIST!3 EXPR !3!) *)
     PAS29;
(* !(!*LINK PRIN!2T EXPR !1!) *)
     PAS272;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*DEALLOC !3!) *)
      dealloc3;
(* !(!*EXIT!) *)
end;
  