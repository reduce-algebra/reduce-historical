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
