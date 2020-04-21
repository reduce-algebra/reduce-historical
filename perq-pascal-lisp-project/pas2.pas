(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  PASCAL Based MINI-LISP/ compilation: V1
   Special Schlumberger Demo 
   All RIGHTS RESERVED
   COPYRIGHT (C) - 1981 - M. L. GRISS
      Computer Science Department
           University of Utah
 
 Do Not distribute with out written consent of M. L. Griss
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
(* !(!*ENTRY PAIRP EXPR !1!) *)
(*  EXPR PAIRP *)
procedure PAS21;
label
      101,
      100;
begin
(* !(!*ALLOC !0!) *)
(* !(!*JUMPNC G!0!0!0!5 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE T!)!) *)
      R[1] := trueref;
(* !(!*JUMP G!0!0!0!6!) *)
      GOTO 101;
(* !(!*LBL G!0!0!0!5!) *)
100: 
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LBL G!0!0!0!6!) *)
101: 
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
procedure PAS22;
forward;
(* !(!*ENTRY NOT EXPR !1!) *)
(*  EXPR NOT *)
procedure PAS23;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !2 !(QUOTE NIL!)!) *)
      R[2] := nilref;
(* !(!*LINK EQ EXPR !2!) *)
     PAS22;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CODEP EXPR !1!) *)
(*  EXPR CODEP *)
procedure XCODEP;
label
      101,
      100;
begin
(* !(!*ALLOC !0!) *)
(* !(!*JUMPNC G!0!0!1!0 !1 CODETAG!) *)
      IF tag_of(R[1]) <> CODETAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE T!)!) *)
      R[1] := trueref;
(* !(!*JUMP G!0!0!1!1!) *)
      GOTO 101;
(* !(!*LBL G!0!0!1!0!) *)
100: 
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LBL G!0!0!1!1!) *)
101: 
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
procedure PAS24;
forward;
procedure PAS25;
forward;
(* !(!*ENTRY CONSTANTP EXPR !1!) *)
(*  EXPR CONSTANTP *)
procedure PAS26;
label
      100;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LINK PAIRP EXPR !1!) *)
     PAS21;
(* !(!*JUMPT G!0!0!1!3!) *)
      IF R[1] <> nilref THEN GOTO 100;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK IDP EXPR !1!) *)
     PAS24;
(* !(!*LBL G!0!0!1!3!) *)
100: 
(* !(!*LINK NULL EXPR !1!) *)
     PAS25;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY EQN EXPR !2!) *)
(*  EXPR EQN *)
procedure PAS27;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LINK EQ EXPR !2!) *)
     PAS22;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY LIST!2 EXPR !2!) *)
(*  EXPR LIST2 *)
procedure PAS28;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*LINK NCONS EXPR !1!) *)
     XNCONS;
(* !(!*LOAD !2 !0!) *)
      load(2,0);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY LIST!3 EXPR !3!) *)
(*  EXPR LIST3 *)
procedure PAS29;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*LOAD !2 !3!) *)
      R[2] := R[3];
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK LIST!2 EXPR !2!) *)
     PAS28;
(* !(!*LOAD !2 !0!) *)
      load(2,0);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY LIST!4 EXPR !4!) *)
(*  EXPR LIST4 *)
procedure PAS210;
begin
(* !(!*ALLOC !3!) *)
    alloc3;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*STORE !3 !-!2!) *)
      store(3,2);
(* !(!*LOAD !3 !4!) *)
      R[3] := R[4];
(* !(!*LOAD !2 !-!2!) *)
      load(2,2);
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK LIST!3 EXPR !3!) *)
     PAS29;
(* !(!*LOAD !2 !0!) *)
      load(2,0);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*DEALLOC !3!) *)
      dealloc3;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY LIST!5 EXPR !5!) *)
(*  EXPR LIST5 *)
procedure PAS211;
begin
(* !(!*ALLOC !4!) *)
    alloc(4);
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*STORE !3 !-!2!) *)
      store(3,2);
(* !(!*STORE !4 !-!3!) *)
      store(4,3);
(* !(!*LOAD !4 !5!) *)
      R[4] := R[5];
(* !(!*LOAD !3 !-!3!) *)
      load(3,3);
(* !(!*LOAD !2 !-!2!) *)
      load(2,2);
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK LIST!4 EXPR !4!) *)
     PAS210;
(* !(!*LOAD !2 !0!) *)
      load(2,0);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*DEALLOC !4!) *)
      dealloc(4);
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY REVERSE EXPR !1!) *)
(*  EXPR REVERSE *)
procedure PAS212;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LINK REV EXPR !1!) *)
     PAS131;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY APPEND EXPR !2!) *)
(*  EXPR APPEND *)
procedure PAS213;
label
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*LINK REVERSE EXPR !1!) *)
     PAS212;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LBL G!0!0!2!9!) *)
100: 
(* !(!*JUMPNC G!0!0!2!8 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 101;
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*LINK CONS EXPR !2!) *)
     XCONS;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMP G!0!0!2!9!) *)
      GOTO 100;
(* !(!*LBL G!0!0!2!8!) *)
101: 
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
procedure PAS214;
forward;
(* !(!*ENTRY MEMBER EXPR !2!) *)
(*  EXPR MEMBER *)
procedure PAS214;
label
      102,
      101,
      100;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !3 !1!) *)
      R[3] := R[1];
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*JUMPT G!0!0!3!4!) *)
      IF R[1] <> nilref THEN GOTO 100;
(* !(!*LOAD !1 !3!) *)
      R[1] := R[3];
(* !(!*JUMP G!0!0!3!6!) *)
      GOTO 102;
(* !(!*LBL G!0!0!3!4!) *)
100: 
(* !(!*LOAD !1 !3!) *)
      R[1] := R[3];
(* !(!*JUMPN G!0!0!3!5 !(CAR !2!)!) *)
   ANYcar(R[2],RXX);
      IF R[1] <> RXX THEN GOTO 101;
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*JUMP G!0!0!3!6!) *)
      GOTO 102;
(* !(!*LBL G!0!0!3!5!) *)
101: 
(* !(!*LOAD !2 !(CDR !2!)!) *)
   ANYcdr(R[2],R[2]);
(* !(!*LINK MEMBER EXPR !2!) *)
     PAS214;
(* !(!*LBL G!0!0!3!6!) *)
102: 
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
procedure PAS215;
forward;
procedure PAS216;
forward;
(* !(!*ENTRY PAIR EXPR !2!) *)
(*  EXPR PAIR *)
procedure PAS216;
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
(* !(!*JUMPNIL G!0!0!3!9!) *)
      IF R[1] = nilref THEN GOTO 100;
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*JUMPT G!0!0!4!0!) *)
      IF R[1] <> nilref THEN GOTO 102;
(* !(!*LBL G!0!0!3!9!) *)
100: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMPT G!0!0!4!4!) *)
      IF R[1] <> nilref THEN GOTO 101;
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*JUMPNIL G!0!0!4!5!) *)
      IF R[1] = nilref THEN GOTO 103;
(* !(!*LBL G!0!0!4!4!) *)
101: 
(* !(!*LOAD !2 !(QUOTE PAIR!)!) *)
      mkident(139,2);
(* !(!*LOAD !1 !(QUOTE !0!)!) *)
      mkint(0,1);
(* !(!*LINK ERROR EXPR !2!) *)
     PAS215;
(* !(!*LBL G!0!0!4!0!) *)
102: 
(* !(!*LOAD !2 !(CAR !1!)!) *)
   ANYcar(R[1],R[2]);
(* !(!*LOAD !1 !(CAR !0!)!) *)
   ANYcar(stk[st],R[1]);
(* !(!*LINK CONS EXPR !2!) *)
     XCONS;
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*LOAD !2 !(CDR !-!1!)!) *)
   ANYcdr(stk[st-1],R[2]);
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*LINK PAIR EXPR !2!) *)
     PAS216;
(* !(!*LOAD !2 !-!2!) *)
      load(2,2);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*LBL G!0!0!4!5!) *)
103: 
(* !(!*DEALLOC !3!) *)
      dealloc3;
(* !(!*EXIT!) *)
end;
procedure PAS217;
forward;
procedure PAS218;
forward;
(* !(!*ENTRY SASSOC EXPR !3!) *)
(*  EXPR SASSOC *)
procedure PAS218;
label
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
(* !(!*JUMPC G!0!0!4!8 !2 PAIRTAG!) *)
      IF tag_of(R[2]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !2 !(QUOTE !(NIL!)!)!) *)
      R[2] := stk[1];
(* !(!*LOAD !1 !3!) *)
      R[1] := R[3];
(* !(!*LINK APPLY EXPR !2!) *)
     PAS217;
(* !(!*JUMP G!0!0!5!0!) *)
      GOTO 102;
(* !(!*LBL G!0!0!4!8!) *)
100: 
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*LINK CAAR EXPR !1!) *)
     PAS11;
(* !(!*JUMPN G!0!0!4!9 !0!) *)
      IF R[1] <> stk[st] THEN GOTO 101;
(* !(!*LOAD !1 !(CAR !-!1!)!) *)
   ANYcar(stk[st-1],R[1]);
(* !(!*JUMP G!0!0!5!0!) *)
      GOTO 102;
(* !(!*LBL G!0!0!4!9!) *)
101: 
(* !(!*LOAD !3 !-!2!) *)
      load(3,2);
(* !(!*LOAD !2 !(CDR !-!1!)!) *)
   ANYcdr(stk[st-1],R[2]);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK SASSOC EXPR !3!) *)
     PAS218;
(* !(!*LBL G!0!0!5!0!) *)
102: 
(* !(!*DEALLOC !3!) *)
      dealloc3;
(* !(!*EXIT!) *)
end;
procedure PAS219;
forward;
procedure PAS220;
forward;
(* !(!*ENTRY SUBLIS EXPR !2!) *)
(*  EXPR SUBLIS *)
procedure PAS220;
label
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
(* !(!*JUMPC G!0!0!5!3 !1 PAIRTAG!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*JUMP G!0!0!5!4!) *)
      GOTO 102;
(* !(!*LBL G!0!0!5!3!) *)
100: 
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK ASSOC EXPR !2!) *)
     PAS219;
(* !(!*JUMPNIL G!0!0!5!7!) *)
      IF R[1] = nilref THEN GOTO 101;
(* !(!*LOAD !1 !(CDR !1!)!) *)
   XCDR;
(* !(!*JUMP G!0!0!5!4!) *)
      GOTO 102;
(* !(!*LBL G!0!0!5!7!) *)
101: 
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*JUMPC G!0!0!5!4 !1 ATOM!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 102;
(* !(!*LOAD !2 !(CAR !1!)!) *)
   ANYcar(R[1],R[2]);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK SUBLIS EXPR !2!) *)
     PAS220;
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*LOAD !2 !(CDR !-!1!)!) *)
   ANYcdr(stk[st-1],R[2]);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK SUBLIS EXPR !2!) *)
     PAS220;
(* !(!*LOAD !2 !-!2!) *)
      load(2,2);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*LBL G!0!0!5!4!) *)
102: 
(* !(!*DEALLOC !3!) *)
      dealloc3;
(* !(!*EXIT!) *)
end;
procedure PAS221;
forward;
(* !(!*ENTRY SUBST EXPR !3!) *)
(*  EXPR SUBST *)
procedure PAS221;
label
      102,
      101,
      100;
begin
(* !(!*ALLOC !4!) *)
    alloc(4);
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*JUMPNIL G!0!0!6!8!) *)
      IF R[1] = nilref THEN GOTO 102;
(* !(!*JUMPN G!0!0!6!6 !3!) *)
      IF R[1] <> R[3] THEN GOTO 100;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMP G!0!0!6!8!) *)
      GOTO 102;
(* !(!*LBL G!0!0!6!6!) *)
100: 
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*STORE !3 !-!2!) *)
      store(3,2);
(* !(!*JUMPNC G!0!0!6!7 !3 ATOM!) *)
      IF tag_of(R[3]) = PAIRTAG THEN GOTO 101;
(* !(!*LOAD !1 !3!) *)
      R[1] := R[3];
(* !(!*JUMP G!0!0!6!8!) *)
      GOTO 102;
(* !(!*LBL G!0!0!6!7!) *)
101: 
(* !(!*LOAD !3 !(CAR !3!)!) *)
   ANYcar(R[3],R[3]);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK SUBST EXPR !3!) *)
     PAS221;
(* !(!*STORE !1 !-!3!) *)
      store(1,3);
(* !(!*LOAD !3 !(CDR !-!2!)!) *)
   ANYcdr(stk[st-2],R[3]);
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK SUBST EXPR !3!) *)
     PAS221;
(* !(!*LOAD !2 !-!3!) *)
      load(2,3);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*LBL G!0!0!6!8!) *)
102: 
(* !(!*DEALLOC !4!) *)
      dealloc(4);
(* !(!*EXIT!) *)
end;
procedure PAS222;
forward;
(* !(!*ENTRY MEMQ EXPR !2!) *)
(*  EXPR MEMQ *)
procedure PAS222;
label
      102,
      101,
      100;
begin
(* !(!*ALLOC !0!) *)
(* !(!*JUMPNC G!0!0!7!6 !2 PAIRTAG!) *)
      IF tag_of(R[2]) <> PAIRTAG THEN GOTO 100;
(* !(!*JUMPN G!0!0!7!4 !(CAR !2!)!) *)
   ANYcar(R[2],RXX);
      IF R[1] <> RXX THEN GOTO 101;
(* !(!*LBL G!0!0!7!6!) *)
100: 
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*JUMP G!0!0!7!5!) *)
      GOTO 102;
(* !(!*LBL G!0!0!7!4!) *)
101: 
(* !(!*LOAD !2 !(CDR !2!)!) *)
   ANYcdr(R[2],R[2]);
(* !(!*LINK MEMQ EXPR !2!) *)
     PAS222;
(* !(!*LBL G!0!0!7!5!) *)
102: 
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
procedure PAS223;
forward;
(* !(!*ENTRY ATSOC EXPR !2!) *)
(*  EXPR ATSOC *)
procedure PAS223;
label
      103,
      102,
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*JUMPC G!0!0!7!9 !2 PAIRTAG!) *)
      IF tag_of(R[2]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*JUMP G!0!0!8!2!) *)
      GOTO 103;
(* !(!*LBL G!0!0!7!9!) *)
100: 
(* !(!*LOAD !1 !(CAR !2!)!) *)
   ANYcar(R[2],R[1]);
(* !(!*JUMPNC G!0!0!8!1 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 101;
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*LINK CAAR EXPR !1!) *)
     PAS11;
(* !(!*JUMPE G!0!0!8!0 !0!) *)
      IF R[1]=stk[st] THEN GOTO 102;
(* !(!*LBL G!0!0!8!1!) *)
101: 
(* !(!*LOAD !2 !(CDR !-!1!)!) *)
   ANYcdr(stk[st-1],R[2]);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK ATSOC EXPR !2!) *)
     PAS223;
(* !(!*JUMP G!0!0!8!2!) *)
      GOTO 103;
(* !(!*LBL G!0!0!8!0!) *)
102: 
(* !(!*LOAD !1 !(CAR !-!1!)!) *)
   ANYcar(stk[st-1],R[1]);
(* !(!*LBL G!0!0!8!2!) *)
103: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY ASSOC EXPR !2!) *)
(*  EXPR ASSOC *)
procedure PAS219;
label
      103,
      102,
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*JUMPC G!0!0!8!5 !2 PAIRTAG!) *)
      IF tag_of(R[2]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!0!8!9!) *)
      GOTO 103;
(* !(!*LBL G!0!0!8!5!) *)
100: 
(* !(!*LOAD !1 !(CAR !2!)!) *)
   ANYcar(R[2],R[1]);
(* !(!*JUMPNC G!0!0!8!6 !1 ATOM!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 101;
(* !(!*LOAD !2 !(QUOTE ASSOC!)!) *)
      mkident(140,2);
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK LIST!2 EXPR !2!) *)
     PAS28;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !(QUOTE !1!0!0!)!) *)
      mkint(100,1);
(* !(!*LINK ERROR EXPR !2!) *)
     PAS215;
(* !(!*LBL G!0!0!8!6!) *)
101: 
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*LINK CAAR EXPR !1!) *)
     PAS11;
(* !(!*JUMPN G!0!0!8!8 !0!) *)
      IF R[1] <> stk[st] THEN GOTO 102;
(* !(!*LOAD !1 !(CAR !-!1!)!) *)
   ANYcar(stk[st-1],R[1]);
(* !(!*JUMP G!0!0!8!9!) *)
      GOTO 103;
(* !(!*LBL G!0!0!8!8!) *)
102: 
(* !(!*LOAD !2 !(CDR !-!1!)!) *)
   ANYcdr(stk[st-1],R[2]);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK ASSOC EXPR !2!) *)
     PAS219;
(* !(!*LBL G!0!0!8!9!) *)
103: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
procedure PAS224;
forward;
procedure PAS225;
forward;
(* !(!*ENTRY DEFLIST EXPR !2!) *)
(*  EXPR DEFLIST *)
procedure PAS225;
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
(* !(!*JUMPC G!0!0!9!2 !1 PAIRTAG!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!0!9!3!) *)
      GOTO 101;
(* !(!*LBL G!0!0!9!2!) *)
100: 
(* !(!*LINK CAAR EXPR !1!) *)
     PAS11;
(* !(!*LOAD !3 !(CAR !(CDR !(CAR !0!)!)!)!) *)
   ANYcar(stk[st],R[3]);
   ANYcdr(R[3],R[3]);
   ANYcar(R[3],R[3]);
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LINK PUT EXPR !3!) *)
     PAS224;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK CAAR EXPR !1!) *)
     PAS11;
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*LINK DEFLIST EXPR !2!) *)
     PAS225;
(* !(!*LOAD !2 !-!2!) *)
      load(2,2);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*LBL G!0!0!9!3!) *)
101: 
(* !(!*DEALLOC !3!) *)
      dealloc3;
(* !(!*EXIT!) *)
end;
procedure PAS226;
forward;
procedure PAS227;
forward;
(* !(!*ENTRY DELETE EXPR !2!) *)
(*  EXPR DELETE *)
procedure PAS227;
label
      102,
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*JUMPC G!0!0!9!9 !2 PAIRTAG!) *)
      IF tag_of(R[2]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!1!0!1!) *)
      GOTO 102;
(* !(!*LBL G!0!0!9!9!) *)
100: 
(* !(!*LOAD !2 !(CAR !2!)!) *)
   ANYcar(R[2],R[2]);
(* !(!*LINK EQUAL EXPR !2!) *)
     PAS226;
(* !(!*JUMPNIL G!0!1!0!0!) *)
      IF R[1] = nilref THEN GOTO 101;
(* !(!*LOAD !1 !(CDR !-!1!)!) *)
   ANYcdr(stk[st-1],R[1]);
(* !(!*JUMP G!0!1!0!1!) *)
      GOTO 102;
(* !(!*LBL G!0!1!0!0!) *)
101: 
(* !(!*LOAD !2 !(CDR !-!1!)!) *)
   ANYcdr(stk[st-1],R[2]);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK DELETE EXPR !2!) *)
     PAS227;
(* !(!*LOAD !2 !(CAR !-!1!)!) *)
   ANYcar(stk[st-1],R[2]);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*LBL G!0!1!0!1!) *)
102: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
procedure PAS228;
forward;
(* !(!*ENTRY DELQ EXPR !2!) *)
(*  EXPR DELQ *)
procedure PAS228;
label
      102,
      101,
      100;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !2 !0!) *)
      store(2,0);
(* !(!*JUMPC G!0!1!0!5 !2 PAIRTAG!) *)
      IF tag_of(R[2]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*JUMP G!0!1!0!7!) *)
      GOTO 102;
(* !(!*LBL G!0!1!0!5!) *)
100: 
(* !(!*JUMPN G!0!1!0!6 !(CAR !2!)!) *)
   ANYcar(R[2],RXX);
      IF R[1] <> RXX THEN GOTO 101;
(* !(!*LOAD !1 !(CDR !2!)!) *)
   ANYcdr(R[2],R[1]);
(* !(!*JUMP G!0!1!0!7!) *)
      GOTO 102;
(* !(!*LBL G!0!1!0!6!) *)
101: 
(* !(!*LOAD !2 !(CDR !2!)!) *)
   ANYcdr(R[2],R[2]);
(* !(!*LINK DELQ EXPR !2!) *)
     PAS228;
(* !(!*LOAD !2 !(CAR !-!1!)!) *)
   ANYcar(stk[st-1],R[2]);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*LBL G!0!1!0!7!) *)
102: 
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
procedure PAS229;
forward;
(* !(!*ENTRY DELATQ EXPR !2!) *)
(*  EXPR DELATQ *)
procedure PAS229;
label
      103,
      102,
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*JUMPC G!0!1!1!1 !2 PAIRTAG!) *)
      IF tag_of(R[2]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*JUMP G!0!1!1!5!) *)
      GOTO 103;
(* !(!*LBL G!0!1!1!1!) *)
100: 
(* !(!*LOAD !1 !(CAR !2!)!) *)
   ANYcar(R[2],R[1]);
(* !(!*JUMPNC G!0!1!1!3 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 101;
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*LINK CAAR EXPR !1!) *)
     PAS11;
(* !(!*JUMPE G!0!1!1!2 !0!) *)
      IF R[1]=stk[st] THEN GOTO 102;
(* !(!*LBL G!0!1!1!3!) *)
101: 
(* !(!*LOAD !2 !(CDR !-!1!)!) *)
   ANYcdr(stk[st-1],R[2]);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK DELATQ EXPR !2!) *)
     PAS229;
(* !(!*LOAD !2 !(CAR !-!1!)!) *)
   ANYcar(stk[st-1],R[2]);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*JUMP G!0!1!1!5!) *)
      GOTO 103;
(* !(!*LBL G!0!1!1!2!) *)
102: 
(* !(!*LOAD !1 !(CDR !-!1!)!) *)
   ANYcdr(stk[st-1],R[1]);
(* !(!*LBL G!0!1!1!5!) *)
103: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY GET EXPR !2!) *)
(*  EXPR GET *)
procedure PAS230;
label
      101,
      100;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !2 !0!) *)
      store(2,0);
(* !(!*JUMPNC G!0!1!1!9 !1 IDTAG!) *)
      IF tag_of(R[1]) <> IDTAG THEN GOTO 100;
(* !(!*LINK PLIST EXPR !1!) *)
        R[1] := IDSPACE[INFO_OF(R[1])].PLIST;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK ATSOC EXPR !2!) *)
     PAS223;
(* !(!*JUMPNC G!0!1!1!9 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(CDR !1!)!) *)
   XCDR;
(* !(!*JUMP G!0!1!2!1!) *)
      GOTO 101;
(* !(!*LBL G!0!1!1!9!) *)
100: 
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LBL G!0!1!2!1!) *)
101: 
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY PUT EXPR !3!) *)
(*  EXPR PUT *)
procedure PAS224;
label
      103,
      102,
      101,
      100;
begin
(* !(!*ALLOC !4!) *)
    alloc(4);
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*STORE !3 !-!2!) *)
      store(3,2);
(* !(!*JUMPC G!0!1!2!6 !1 IDTAG!) *)
      IF tag_of(R[1]) = IDTAG THEN GOTO 100;
(* !(!*LOAD !1 !3!) *)
      R[1] := R[3];
(* !(!*JUMP G!0!1!2!4!) *)
      GOTO 103;
(* !(!*LBL G!0!1!2!6!) *)
100: 
(* !(!*LINK PLIST EXPR !1!) *)
        R[1] := IDSPACE[INFO_OF(R[1])].PLIST;
(* !(!*STORE !1 !-!3!) *)
      store(1,3);
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK ATSOC EXPR !2!) *)
     PAS223;
(* !(!*JUMPNIL G!0!1!2!8!) *)
      IF R[1] = nilref THEN GOTO 101;
(* !(!*LOAD !2 !-!3!) *)
      load(2,3);
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK DELATQ EXPR !2!) *)
     PAS229;
(* !(!*STORE !1 !-!3!) *)
      store(1,3);
(* !(!*LBL G!0!1!2!8!) *)
101: 
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*JUMPNIL G!0!1!3!0!) *)
      IF R[1] = nilref THEN GOTO 102;
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*LOAD !2 !-!3!) *)
      load(2,3);
(* !(!*LINK CONS EXPR !2!) *)
     XCONS;
(* !(!*STORE !1 !-!3!) *)
      store(1,3);
(* !(!*LBL G!0!1!3!0!) *)
102: 
(* !(!*LOAD !2 !-!3!) *)
      load(2,3);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK SETPLIST EXPR !2!) *)
        IDSPACE[INFO_OF(R[1])].PLIST := R[2];
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*LBL G!0!1!2!4!) *)
103: 
(* !(!*DEALLOC !4!) *)
      dealloc(4);
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY REMPROP EXPR !2!) *)
(*  EXPR REMPROP *)
procedure PAS231;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !3 !(QUOTE NIL!)!) *)
      R[3] := nilref;
(* !(!*LINK PUT EXPR !3!) *)
     PAS224;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
procedure PAS232;
forward;
(* !(!*ENTRY LENGTH EXPR !1!) *)
(*  EXPR LENGTH *)
procedure PAS232;
label
      101,
      100;
begin
(* !(!*ALLOC !0!) *)
(* !(!*JUMPC G!0!1!3!5 !1 PAIRTAG!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE !0!)!) *)
      mkint(0,1);
(* !(!*JUMP G!0!1!3!6!) *)
      GOTO 101;
(* !(!*LBL G!0!1!3!5!) *)
100: 
(* !(!*LOAD !1 !(CDR !1!)!) *)
   XCDR;
(* !(!*LINK LENGTH EXPR !1!) *)
     PAS232;
(* !(!*LINK ADD!1 EXPR !1!) *)
     XADD1;
(* !(!*LBL G!0!1!3!6!) *)
101: 
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY ERRPRT EXPR !1!) *)
(*  EXPR ERRPRT *)
procedure PAS233;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !1 !(QUOTE !*!*!*!*! !)!) *)
      mkident(141,1);
(* !(!*LINK PRIN!2 EXPR !1!) *)
     PAS129;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK PRINT EXPR !1!) *)
     XPRINT;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY MSGPRT EXPR !1!) *)
(*  EXPR MSGPRT *)
procedure PAS234;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !1 !(QUOTE !*!*!*! !)!) *)
      mkident(142,1);
(* !(!*LINK PRIN!2 EXPR !1!) *)
     PAS129;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK PRINT EXPR !1!) *)
     XPRINT;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY FLAGP EXPR !2!) *)
(*  EXPR FLAGP *)
procedure PAS235;
label
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*LINK IDP EXPR !1!) *)
     PAS24;
(* !(!*JUMPNIL G!0!1!4!1!) *)
      IF R[1] = nilref THEN GOTO 100;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK PLIST EXPR !1!) *)
        R[1] := IDSPACE[INFO_OF(R[1])].PLIST;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK MEMQ EXPR !2!) *)
     PAS222;
(* !(!*LBL G!0!1!4!1!) *)
100: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
procedure PAS236;
forward;
procedure PAS237;
forward;
(* !(!*ENTRY FLAG EXPR !2!) *)
(*  EXPR FLAG *)
procedure PAS237;
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
(* !(!*JUMPC G!0!1!4!5 !1 PAIRTAG!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!1!4!6!) *)
      GOTO 101;
(* !(!*LBL G!0!1!4!5!) *)
100: 
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*LINK FLAG!1 EXPR !2!) *)
     PAS236;
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*LINK FLAG EXPR !2!) *)
     PAS237;
(* !(!*LBL G!0!1!4!6!) *)
101: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY FLAG!1 EXPR !2!) *)
(*  EXPR FLAG1 *)
procedure PAS236;
label
      102,
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*JUMPNC G!0!1!5!5 !1 IDTAG!) *)
      IF tag_of(R[1]) <> IDTAG THEN GOTO 100;
(* !(!*LINK PLIST EXPR !1!) *)
        R[1] := IDSPACE[INFO_OF(R[1])].PLIST;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK MEMQ EXPR !2!) *)
     PAS222;
(* !(!*JUMPNIL G!0!1!5!0!) *)
      IF R[1] = nilref THEN GOTO 101;
(* !(!*LBL G!0!1!5!5!) *)
100: 
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!1!5!2!) *)
      GOTO 102;
(* !(!*LBL G!0!1!5!0!) *)
101: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK PLIST EXPR !1!) *)
        R[1] := IDSPACE[INFO_OF(R[1])].PLIST;
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK SETPLIST EXPR !2!) *)
        IDSPACE[INFO_OF(R[1])].PLIST := R[2];
(* !(!*LBL G!0!1!5!2!) *)
102: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
procedure PAS238;
forward;
procedure PAS239;
forward;
(* !(!*ENTRY REMFLAG EXPR !2!) *)
(*  EXPR REMFLAG *)
procedure PAS239;
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
(* !(!*JUMPC G!0!1!5!8 !1 PAIRTAG!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!1!5!9!) *)
      GOTO 101;
(* !(!*LBL G!0!1!5!8!) *)
100: 
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*LINK REMFLAG!1 EXPR !2!) *)
     PAS238;
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*LINK REMFLAG EXPR !2!) *)
     PAS239;
(* !(!*LBL G!0!1!5!9!) *)
101: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY REMFLAG!1 EXPR !2!) *)
(*  EXPR REMFLAG1 *)
procedure PAS238;
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
(* !(!*JUMPC G!0!1!6!2 !1 IDTAG!) *)
      IF tag_of(R[1]) = IDTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!1!6!5!) *)
      GOTO 101;
(* !(!*LBL G!0!1!6!2!) *)
100: 
(* !(!*LINK PLIST EXPR !1!) *)
        R[1] := IDSPACE[INFO_OF(R[1])].PLIST;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK MEMQ EXPR !2!) *)
     PAS222;
(* !(!*JUMPNIL G!0!1!6!5!) *)
      IF R[1] = nilref THEN GOTO 101;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK PLIST EXPR !1!) *)
        R[1] := IDSPACE[INFO_OF(R[1])].PLIST;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK DELQ EXPR !2!) *)
     PAS228;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK SETPLIST EXPR !2!) *)
        IDSPACE[INFO_OF(R[1])].PLIST := R[2];
(* !(!*LBL G!0!1!6!5!) *)
101: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY EQ EXPR !2!) *)
(*  EXPR EQ *)
procedure PAS22;
label
      101,
      100;
begin
(* !(!*ALLOC !0!) *)
(* !(!*JUMPN G!0!1!7!0 !2!) *)
      IF R[1] <> R[2] THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE T!)!) *)
      R[1] := trueref;
(* !(!*JUMP G!0!1!7!1!) *)
      GOTO 101;
(* !(!*LBL G!0!1!7!0!) *)
100: 
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LBL G!0!1!7!1!) *)
101: 
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY EQCAR EXPR !2!) *)
(*  EXPR EQCAR *)
procedure PAS240;
label
      101,
      100;
begin
(* !(!*ALLOC !0!) *)
(* !(!*JUMPNC G!0!1!7!4 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*JUMPN G!0!1!7!4 !2!) *)
      IF R[1] <> R[2] THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE T!)!) *)
      R[1] := trueref;
(* !(!*JUMP G!0!1!7!3!) *)
      GOTO 101;
(* !(!*LBL G!0!1!7!4!) *)
100: 
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LBL G!0!1!7!3!) *)
101: 
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY NULL EXPR !1!) *)
(*  EXPR NULL *)
procedure PAS25;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !2 !(QUOTE NIL!)!) *)
      R[2] := nilref;
(* !(!*LINK EQ EXPR !2!) *)
     PAS22;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY PLIST EXPR !1!) *)
(*  EXPR PLIST *)
procedure PAS241;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LINK PLIST EXPR !1!) *)
        R[1] := IDSPACE[INFO_OF(R[1])].PLIST;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY VALUE EXPR !1!) *)
(*  EXPR VALUE *)
procedure PAS242;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LINK VALUE EXPR !1!) *)
        R[1] := IDSPACE[INFO_OF(R[1])].VAL;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY FUNCELL EXPR !1!) *)
(*  EXPR FUNCELL *)
procedure PAS243;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LINK FUNCELL EXPR !1!) *)
        R[1] := IDSPACE[INFO_OF(R[1])].FUNCELL;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY SETPLIST EXPR !2!) *)
(*  EXPR SETPLIST *)
procedure PAS244;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LINK SETPLIST EXPR !2!) *)
        IDSPACE[INFO_OF(R[1])].PLIST := R[2];
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY SETVALUE EXPR !2!) *)
(*  EXPR SETVALUE *)
procedure PAS245;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LINK SETVALUE EXPR !2!) *)
       IDSPACE[INFO_OF(R[1])].VAL := R[2];
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY SETFUNCELL EXPR !2!) *)
(*  EXPR SETFUNCELL *)
procedure PAS246;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LINK SETFUNCELL EXPR !2!) *)
        IDSPACE[INFO_OF(R[1])].FUNCELL := R[2];
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY ORDERP EXPR !2!) *)
(*  EXPR ORDERP *)
procedure PAS247;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*LINK !*INF EXPR !1!) *)
      mkitem(INTTAG,info_of(R[1]),R[1]);
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK !*INF EXPR !1!) *)
      mkitem(INTTAG,info_of(R[1]),R[1]);
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK GREATERP EXPR !2!) *)
     XGREATERP;
(* !(!*LINK NULL EXPR !1!) *)
     PAS25;
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY TOKEN EXPR !0!) *)
(*  EXPR TOKEN *)
procedure PAS248;
label
      100;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LINK RDTOK EXPR !0!) *)
     XRDTOK;
(* !(!*STORE !1 !(FLUID TOK!*!)!) *)
      idspace[143].val := R[1];
(* !(!*JUMPNC G!0!1!9!1 !1 CHARTAG!) *)
      IF tag_of(R[1]) <> CHARTAG THEN GOTO 100;
(* !(!*LINK CHAR!2ID EXPR !1!) *)
     SET_TAG(R[1], IDTAG);
(* !(!*STORE !1 !(FLUID TOK!*!)!) *)
      idspace[143].val := R[1];
(* !(!*LBL G!0!1!9!1!) *)
100: 
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY EQUAL EXPR !2!) *)
(*  EXPR EQUAL *)
procedure PAS226;
label
      103,
      102,
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*JUMPNC G!0!1!9!6 !1 ATOM!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*JUMPNC G!0!2!0!3 !2 ATOM!) *)
      IF tag_of(R[2]) = PAIRTAG THEN GOTO 101;
(* !(!*LINK EQ EXPR !2!) *)
     PAS22;
(* !(!*JUMP G!0!2!0!1!) *)
      GOTO 103;
(* !(!*LBL G!0!1!9!6!) *)
100: 
(* !(!*JUMPNC G!0!2!0!0 !2 ATOM!) *)
      IF tag_of(R[2]) = PAIRTAG THEN GOTO 102;
(* !(!*LBL G!0!2!0!3!) *)
101: 
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!2!0!1!) *)
      GOTO 103;
(* !(!*LBL G!0!2!0!0!) *)
102: 
(* !(!*LOAD !2 !(CAR !2!)!) *)
   ANYcar(R[2],R[2]);
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*LINK EQUAL EXPR !2!) *)
     PAS226;
(* !(!*JUMPNIL G!0!2!0!1!) *)
      IF R[1] = nilref THEN GOTO 103;
(* !(!*LOAD !2 !(CDR !-!1!)!) *)
   ANYcdr(stk[st-1],R[2]);
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*LINK EQUAL EXPR !2!) *)
     PAS226;
(* !(!*LBL G!0!2!0!1!) *)
103: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY ERROR EXPR !2!) *)
(*  EXPR ERROR *)
procedure PAS215;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*LOAD !3 !2!) *)
      R[3] := R[2];
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !(QUOTE !*!*!*!*! ERROR! !)!) *)
      mkident(144,1);
(* !(!*LINK LIST!3 EXPR !3!) *)
     PAS29;
(* !(!*LINK PRINT EXPR !1!) *)
     XPRINT;
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*STORE !1 !(FLUID EMSG!*!)!) *)
      idspace[145].val := R[1];
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*STORE !1 !(FLUID ENUM!*!)!) *)
      idspace[146].val := R[1];
(* !(!*LINK THROW EXPR !1!) *)
     XTHROW;
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY ERRORSET EXPR !3!) *)
(*  EXPR ERRORSET *)
procedure PAS249;
label
      102,
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE NIL !(FLUID THROWING!*!)!) *)
      idspace[131].val := nilref;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*LINK CATCH EXPR !1!) *)
     XCATCH;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !1 !(FLUID THROWING!*!)!) *)
      R[1] := idspace[131].val;
(* !(!*JUMPT G!0!2!0!9!) *)
      IF R[1] <> nilref THEN GOTO 100;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK NCONS EXPR !1!) *)
     XNCONS;
(* !(!*JUMP G!0!2!0!7!) *)
      GOTO 102;
(* !(!*LBL G!0!2!0!9!) *)
100: 
(* !(!*STORE NIL !(FLUID THROWING!*!)!) *)
      idspace[131].val := nilref;
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*JUMPNIL G!0!2!1!2!) *)
      IF R[1] = nilref THEN GOTO 101;
(* !(!*LOAD !3 !(FLUID EMSG!*!)!) *)
      R[3] := idspace[145].val;
(* !(!*LOAD !2 !(FLUID ENUM!*!)!) *)
      R[2] := idspace[146].val;
(* !(!*LOAD !1 !(QUOTE !*!*!*!*!)!) *)
      mkident(147,1);
(* !(!*LINK LIST!3 EXPR !3!) *)
     PAS29;
(* !(!*LINK PRINT EXPR !1!) *)
     XPRINT;
(* !(!*LBL G!0!2!1!2!) *)
101: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LBL G!0!2!0!7!) *)
102: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
procedure PAS250;
forward;
(* !(!*ENTRY FIXP EXPR !1!) *)
(*  EXPR FIXP *)
procedure PAS251;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LINK NUMBERP EXPR !1!) *)
     PAS250;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
procedure PAS252;
forward;
(* !(!*ENTRY ABS EXPR !1!) *)
(*  EXPR ABS *)
procedure PAS253;
label
      101,
      100;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LINK MINUSP EXPR !1!) *)
     PAS252;
(* !(!*JUMPNIL G!0!2!1!7!) *)
      IF R[1] = nilref THEN GOTO 100;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK MINUS EXPR !1!) *)
     XMINUS;
(* !(!*JUMP G!0!2!1!8!) *)
      GOTO 101;
(* !(!*LBL G!0!2!1!7!) *)
100: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LBL G!0!2!1!8!) *)
101: 
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY SUB!1 EXPR !1!) *)
(*  EXPR SUB1 *)
procedure PAS254;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !2 !(QUOTE !-!1!)!) *)
      mkint(-1,2);
(* !(!*LINK PLUS!2 EXPR !2!) *)
     XPLUS2;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY ZEROP EXPR !1!) *)
(*  EXPR ZEROP *)
procedure PAS255;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !2 !(QUOTE !0!)!) *)
      mkint(0,2);
(* !(!*LINK EQ EXPR !2!) *)
     PAS22;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY ONEP EXPR !1!) *)
(*  EXPR ONEP *)
procedure PAS256;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !2 !(QUOTE !1!)!) *)
      mkint(1,2);
(* !(!*LINK EQ EXPR !2!) *)
     PAS22;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY IDP EXPR !1!) *)
(*  EXPR IDP *)
procedure PAS24;
label
      101,
      100;
begin
(* !(!*ALLOC !0!) *)
(* !(!*JUMPNC G!0!2!2!4 !1 IDTAG!) *)
      IF tag_of(R[1]) <> IDTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE T!)!) *)
      R[1] := trueref;
(* !(!*JUMP G!0!2!2!5!) *)
      GOTO 101;
(* !(!*LBL G!0!2!2!4!) *)
100: 
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LBL G!0!2!2!5!) *)
101: 
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
procedure PAS257;
forward;
(* !(!*ENTRY EXPT EXPR !2!) *)
(*  EXPR EXPT *)
procedure PAS257;
label
      102,
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*JUMPN G!0!2!2!8 !(QUOTE !0!)!) *)
      mkitem(INTTAG,0,RXX);
      IF R[1] <> RXX THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE !1!)!) *)
      mkint(1,1);
(* !(!*JUMP G!0!2!3!0!) *)
      GOTO 102;
(* !(!*LBL G!0!2!2!8!) *)
100: 
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*LINK MINUSP EXPR !1!) *)
     PAS252;
(* !(!*JUMPNIL G!0!2!2!9!) *)
      IF R[1] = nilref THEN GOTO 101;
(* !(!*LOAD !1 !(QUOTE !0!)!) *)
      mkint(0,1);
(* !(!*JUMP G!0!2!3!0!) *)
      GOTO 102;
(* !(!*LBL G!0!2!2!9!) *)
101: 
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK SUB!1 EXPR !1!) *)
     PAS254;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK EXPT EXPR !2!) *)
     PAS257;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK TIMES!2 EXPR !2!) *)
     XTIMES2;
(* !(!*LBL G!0!2!3!0!) *)
102: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY FIX EXPR !1!) *)
(*  EXPR FIX *)
procedure PAS258;
begin
(* !(!*ALLOC !0!) *)
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY FLOAT EXPR !1!) *)
(*  EXPR FLOAT *)
procedure PAS259;
begin
(* !(!*ALLOC !0!) *)
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
procedure PAS260;
forward;
(* !(!*ENTRY MAX MACRO !1!) *)
(*  MACRO MAX *)
procedure PAS261;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !2 !(QUOTE MAX!2!)!) *)
      mkident(148,2);
(* !(!*LOAD !1 !(CDR !1!)!) *)
   XCDR;
(* !(!*LINK EXPAND EXPR !2!) *)
     PAS260;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY MIN MACRO !1!) *)
(*  MACRO MIN *)
procedure PAS262;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !2 !(QUOTE MIN!2!)!) *)
      mkident(149,2);
(* !(!*LOAD !1 !(CDR !1!)!) *)
   XCDR;
(* !(!*LINK EXPAND EXPR !2!) *)
     PAS260;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY PLUS MACRO !1!) *)
(*  MACRO PLUS *)
procedure PAS263;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !2 !(QUOTE PLUS!2!)!) *)
      mkident(150,2);
(* !(!*LOAD !1 !(CDR !1!)!) *)
   XCDR;
(* !(!*LINK EXPAND EXPR !2!) *)
     PAS260;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY TIMES MACRO !1!) *)
(*  MACRO TIMES *)
procedure PAS264;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !2 !(QUOTE TIMES!2!)!) *)
      mkident(151,2);
(* !(!*LOAD !1 !(CDR !1!)!) *)
   XCDR;
(* !(!*LINK EXPAND EXPR !2!) *)
     PAS260;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY MAX!2 EXPR !2!) *)
(*  EXPR MAX2 *)
procedure PAS265;
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
(* !(!*LINK GREATERP EXPR !2!) *)
     XGREATERP;
(* !(!*JUMPNIL G!0!2!4!1!) *)
      IF R[1] = nilref THEN GOTO 100;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMP G!0!2!4!2!) *)
      GOTO 101;
(* !(!*LBL G!0!2!4!1!) *)
100: 
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LBL G!0!2!4!2!) *)
101: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY MIN!2 EXPR !2!) *)
(*  EXPR MIN2 *)
procedure PAS266;
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
(* !(!*LINK LESSP EXPR !2!) *)
     XLESSP;
(* !(!*JUMPNIL G!0!2!4!5!) *)
      IF R[1] = nilref THEN GOTO 100;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMP G!0!2!4!6!) *)
      GOTO 101;
(* !(!*LBL G!0!2!4!5!) *)
100: 
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LBL G!0!2!4!6!) *)
101: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY FUNCTION FEXPR !1!) *)
(*  FEXPR FUNCTION *)
procedure PAS267;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY EXPAND EXPR !2!) *)
(*  EXPR EXPAND *)
procedure PAS260;
label
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !1 !(CDR !1!)!) *)
   XCDR;
(* !(!*JUMPT G!0!2!5!0!) *)
      IF R[1] <> nilref THEN GOTO 100;
(* !(!*LOAD !1 !(CAR !0!)!) *)
   ANYcar(stk[st],R[1]);
(* !(!*JUMP G!0!2!5!1!) *)
      GOTO 101;
(* !(!*LBL G!0!2!5!0!) *)
100: 
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*LINK EXPAND EXPR !2!) *)
     PAS260;
(* !(!*LOAD !3 !1!) *)
      R[3] := R[1];
(* !(!*LOAD !2 !(CAR !0!)!) *)
   ANYcar(stk[st],R[2]);
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK LIST!3 EXPR !3!) *)
     PAS29;
(* !(!*LBL G!0!2!5!1!) *)
101: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY NUMBERP EXPR !1!) *)
(*  EXPR NUMBERP *)
procedure PAS250;
label
      101,
      100;
begin
(* !(!*ALLOC !0!) *)
(* !(!*JUMPNC G!0!2!5!5 !1 NUMTAG!) *)
      IF not((tag_of(R[1]) = INTTAG)
       or (tag_of(R[1]) = FIXTAG)) THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE T!)!) *)
      R[1] := trueref;
(* !(!*JUMP G!0!2!5!6!) *)
      GOTO 101;
(* !(!*LBL G!0!2!5!5!) *)
100: 
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LBL G!0!2!5!6!) *)
101: 
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY ATOM EXPR !1!) *)
(*  EXPR ATOM *)
procedure PAS268;
label
      101,
      100;
begin
(* !(!*ALLOC !0!) *)
(* !(!*JUMPNC G!0!2!5!9 !1 ATOM!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE T!)!) *)
      R[1] := trueref;
(* !(!*JUMP G!0!2!6!0!) *)
      GOTO 101;
(* !(!*LBL G!0!2!5!9!) *)
100: 
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LBL G!0!2!6!0!) *)
101: 
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY MINUSP EXPR !1!) *)
(*  EXPR MINUSP *)
procedure PAS252;
label
      101,
      100;
begin
(* !(!*ALLOC !0!) *)
(* !(!*JUMPNC G!0!2!6!3 !1 NUMTAG!) *)
      IF not((tag_of(R[1]) = INTTAG)
       or (tag_of(R[1]) = FIXTAG)) THEN GOTO 100;
(* !(!*LOAD !2 !(QUOTE !-!1!)!) *)
      mkint(-1,2);
(* !(!*LINK GREATERP EXPR !2!) *)
     XGREATERP;
(* !(!*JUMPT G!0!2!6!3!) *)
      IF R[1] <> nilref THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE T!)!) *)
      R[1] := trueref;
(* !(!*JUMP G!0!2!6!5!) *)
      GOTO 101;
(* !(!*LBL G!0!2!6!3!) *)
100: 
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LBL G!0!2!6!5!) *)
101: 
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY SET EXPR !2!) *)
(*  EXPR SET *)
procedure PAS269;
label
      102,
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*JUMPNC G!0!2!6!9 !1 IDTAG!) *)
      IF tag_of(R[1]) <> IDTAG THEN GOTO 100;
(* !(!*JUMPE G!0!2!6!9 !(QUOTE T!)!) *)
      IF R[1]=trueref THEN GOTO 100;
(* !(!*JUMPN G!0!2!6!8 !(QUOTE NIL!)!) *)
      IF R[1] <> nilref THEN GOTO 101;
(* !(!*LBL G!0!2!6!9!) *)
100: 
(* !(!*LOAD !1 !2!) *)
      R[1] := R[2];
(* !(!*LINK NCONS EXPR !1!) *)
     XNCONS;
(* !(!*LOAD !2 !0!) *)
      load(2,0);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*LOAD !2 !(QUOTE SET!)!) *)
      mkident(152,2);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*JUMP G!0!2!7!2!) *)
      GOTO 102;
(* !(!*LBL G!0!2!6!8!) *)
101: 
(* !(!*LINK SETVALUE EXPR !2!) *)
       IDSPACE[INFO_OF(R[1])].VAL := R[2];
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LBL G!0!2!7!2!) *)
102: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY PRINC EXPR !1!) *)
(*  EXPR PRINC *)
procedure PAS270;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LINK PRIN!2 EXPR !1!) *)
     PAS129;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY PRIN!1 EXPR !1!) *)
(*  EXPR PRIN1 *)
procedure PAS271;
begin
(* !(!*ALLOC !0!) *)
(* !(!*LINK PRIN!2 EXPR !1!) *)
     PAS129;
(* !(!*DEALLOC !0!) *)
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY PRINT EXPR !1!) *)
(*  EXPR PRINT *)
procedure XPRINT;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LINK PRIN!1 EXPR !1!) *)
     PAS271;
(* !(!*LINK TERPRI EXPR !0!) *)
     XTERPRI;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY PRIN!2T EXPR !1!) *)
(*  EXPR PRIN2T *)
procedure PAS272;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LINK PRIN!2 EXPR !1!) *)
     PAS129;
(* !(!*LINK TERPRI EXPR !0!) *)
     XTERPRI;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY LBIND!1 EXPR !2!) *)
(*  EXPR LBIND1 *)
procedure PAS273;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*LINK VALUE EXPR !1!) *)
        R[1] := IDSPACE[INFO_OF(R[1])].VAL;
(* !(!*LOAD !2 !0!) *)
      load(2,0);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*LOAD !2 !(FLUID BSTK!*!)!) *)
      R[2] := idspace[130].val;
(* !(!*LINK CONS EXPR !2!) *)
     XCONS;
(* !(!*STORE !1 !(FLUID BSTK!*!)!) *)
      idspace[130].val := R[1];
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK SETVALUE EXPR !2!) *)
       IDSPACE[INFO_OF(R[1])].VAL := R[2];
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY PBIND!1 EXPR !1!) *)
(*  EXPR PBIND1 *)
procedure PAS274;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LINK VALUE EXPR !1!) *)
        R[1] := IDSPACE[INFO_OF(R[1])].VAL;
(* !(!*LOAD !2 !0!) *)
      load(2,0);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*LOAD !2 !(FLUID BSTK!*!)!) *)
      R[2] := idspace[130].val;
(* !(!*LINK CONS EXPR !2!) *)
     XCONS;
(* !(!*STORE !1 !(FLUID BSTK!*!)!) *)
      idspace[130].val := R[1];
(* !(!*LOAD !2 !(QUOTE NIL!)!) *)
      R[2] := nilref;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK SETVALUE EXPR !2!) *)
       IDSPACE[INFO_OF(R[1])].VAL := R[2];
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY UNBIND!1 EXPR !0!) *)
(*  EXPR UNBIND1 *)
procedure PAS275;
label
      100;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(FLUID BSTK!*!)!) *)
      R[1] := idspace[130].val;
(* !(!*JUMPC G!0!2!8!9 !1 PAIRTAG!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !2 !(QUOTE BNDUNDERFLOW!)!) *)
      mkident(153,2);
(* !(!*LOAD !1 !(QUOTE !9!9!)!) *)
      mkint(99,1);
(* !(!*LINK ERROR EXPR !2!) *)
     PAS215;
(* !(!*LBL G!0!2!8!9!) *)
100: 
(* !(!*LINK CAAR EXPR !1!) *)
     PAS11;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !1 !(FLUID BSTK!*!)!) *)
      R[1] := idspace[130].val;
(* !(!*LINK CDAR EXPR !1!) *)
     PAS13;
(* !(!*LOAD !2 !1!) *)
      R[2] := R[1];
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK SETVALUE EXPR !2!) *)
       IDSPACE[INFO_OF(R[1])].VAL := R[2];
(* !(!*LOAD !1 !(CDR !(FLUID BSTK!*!)!)!) *)
   ANYcdr(idspace[130].val,R[1]);
(* !(!*STORE !1 !(FLUID BSTK!*!)!) *)
      idspace[130].val := R[1];
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY UNBINDN EXPR !1!) *)
(*  EXPR UNBINDN *)
procedure PAS276;
label
      101,
      100;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LBL G!0!2!9!3!) *)
100: 
(* !(!*LOAD !2 !(QUOTE !0!)!) *)
      mkint(0,2);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK GREATERP EXPR !2!) *)
     XGREATERP;
(* !(!*JUMPNIL G!0!2!9!2!) *)
      IF R[1] = nilref THEN GOTO 101;
(* !(!*LINK UNBIND!1 EXPR !0!) *)
     PAS275;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK SUB!1 EXPR !1!) *)
     PAS254;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMP G!0!2!9!3!) *)
      GOTO 100;
(* !(!*LBL G!0!2!9!2!) *)
101: 
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY UNBINDTO EXPR !2!) *)
(*  EXPR UNBINDTO *)
procedure XUNBINDTO;
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
(* !(!*LBL G!0!2!9!9!) *)
100: 
(* !(!*LOAD !1 !(FLUID BSTK!*!)!) *)
      R[1] := idspace[130].val;
(* !(!*JUMPNC G!0!3!0!2 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 101;
(* !(!*JUMPE G!0!3!0!2 !-!1!) *)
      IF R[1]=stk[st-1] THEN GOTO 101;
(* !(!*LINK UNBIND!1 EXPR !0!) *)
     PAS275;
(* !(!*JUMP G!0!2!9!9!) *)
      GOTO 100;
(* !(!*LBL G!0!3!0!2!) *)
101: 
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
procedure PAS277;
forward;
procedure PAS278;
forward;
(* !(!*ENTRY EVLAM EXPR !2!) *)
(*  EXPR EVLAM *)
procedure PAS279;
label
      101,
      100;
begin
(* !(!*ALLOC !3!) *)
    alloc3;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMPNC G!0!3!0!7 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*JUMPE G!0!3!0!6 !(QUOTE LAMBDA!)!) *)
      mkitem(IDTAG,154,RXX);
      IF R[1]=RXX THEN GOTO 101;
(* !(!*LBL G!0!3!0!7!) *)
100: 
(* !(!*LOAD !2 !(QUOTE NOT! DEFINED!)!) *)
      mkident(155,2);
(* !(!*LOAD !1 !(QUOTE !9!9!)!) *)
      mkint(99,1);
(* !(!*LINK ERROR EXPR !2!) *)
     PAS215;
(* !(!*LBL G!0!3!0!6!) *)
101: 
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*STORE !1 !-!2!) *)
      store(1,2);
(* !(!*LINK LBINDN EXPR !2!) *)
     PAS277;
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*LINK P!.N EXPR !1!) *)
     PAS278;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*LINK LENGTH EXPR !1!) *)
     PAS232;
(* !(!*LINK UNBINDN EXPR !1!) *)
     PAS276;
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*DEALLOC !3!) *)
      dealloc3;
(* !(!*EXIT!) *)
end;
procedure PAS280;
forward;
(* !(!*ENTRY LBINDN EXPR !2!) *)
(*  EXPR LBINDN *)
procedure PAS277;
label
      102,
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*STORE !2 !-!1!) *)
      store(2,1);
(* !(!*JUMPC G!0!3!1!2 !1 PAIRTAG!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!3!1!4!) *)
      GOTO 102;
(* !(!*LBL G!0!3!1!2!) *)
100: 
(* !(!*JUMPC G!0!3!1!3 !2 PAIRTAG!) *)
      IF tag_of(R[2]) = PAIRTAG THEN GOTO 101;
(* !(!*LINK PBINDN EXPR !1!) *)
     PAS280;
(* !(!*JUMP G!0!3!1!4!) *)
      GOTO 102;
(* !(!*LBL G!0!3!1!3!) *)
101: 
(* !(!*LOAD !2 !(CAR !2!)!) *)
   ANYcar(R[2],R[2]);
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*LINK LBIND!1 EXPR !2!) *)
     PAS273;
(* !(!*LOAD !2 !(CDR !-!1!)!) *)
   ANYcdr(stk[st-1],R[2]);
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*LINK LBINDN EXPR !2!) *)
     PAS277;
(* !(!*LBL G!0!3!1!4!) *)
102: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY PBINDN EXPR !1!) *)
(*  EXPR PBINDN *)
procedure PAS280;
label
      101,
      100;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMPC G!0!3!1!7 !1 PAIRTAG!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*JUMP G!0!3!1!8!) *)
      GOTO 101;
(* !(!*LBL G!0!3!1!7!) *)
100: 
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*LINK PBIND!1 EXPR !1!) *)
     PAS274;
(* !(!*LOAD !1 !(CDR !0!)!) *)
   ANYcdr(stk[st],R[1]);
(* !(!*LINK PBINDN EXPR !1!) *)
     PAS280;
(* !(!*LBL G!0!3!1!8!) *)
101: 
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
