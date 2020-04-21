(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  PASCAL Based MINI-LISP/ compilation: V1
   Special Schlumberger Demo 
   All RIGHTS RESERVED
   COPYRIGHT (C) - 1981 - M. L. GRISS
      Computer Science Department
           University of Utah
 
 Do Not distribute with out written consent of M. L. Griss
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
(* !(!*ENTRY CAAR EXPR !1!) *)
(*  EXPR CAAR *)
procedure PAS11;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CAR !(CAR !1!)!)!) *)
   XCAR;
   XCAR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CADR EXPR !1!) *)
(*  EXPR CADR *)
procedure PAS12;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CAR !(CDR !1!)!)!) *)
   XCDR;
   XCAR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CDAR EXPR !1!) *)
(*  EXPR CDAR *)
procedure PAS13;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CDR !(CAR !1!)!)!) *)
   XCAR;
   XCDR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CDDR EXPR !1!) *)
(*  EXPR CDDR *)
procedure PAS14;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CDR !(CDR !1!)!)!) *)
   XCDR;
   XCDR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CAAAAR EXPR !1!) *)
(*  EXPR CAAAAR *)
procedure PAS15;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CAR !(CAR !(CAR !(CAR !1!)!)!)!)!) *)
   XCAR;
   XCAR;
   XCAR;
   XCAR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CAAADR EXPR !1!) *)
(*  EXPR CAAADR *)
procedure PAS16;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CAR !(CAR !(CAR !(CDR !1!)!)!)!)!) *)
   XCDR;
   XCAR;
   XCAR;
   XCAR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CAADAR EXPR !1!) *)
(*  EXPR CAADAR *)
procedure PAS17;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CAR !(CAR !(CDR !(CAR !1!)!)!)!)!) *)
   XCAR;
   XCDR;
   XCAR;
   XCAR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CAADDR EXPR !1!) *)
(*  EXPR CAADDR *)
procedure PAS18;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CAR !(CAR !(CDR !(CDR !1!)!)!)!)!) *)
   XCDR;
   XCDR;
   XCAR;
   XCAR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CADAAR EXPR !1!) *)
(*  EXPR CADAAR *)
procedure PAS19;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CAR !(CDR !(CAR !(CAR !1!)!)!)!)!) *)
   XCAR;
   XCAR;
   XCDR;
   XCAR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CADADR EXPR !1!) *)
(*  EXPR CADADR *)
procedure PAS110;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CAR !(CDR !(CAR !(CDR !1!)!)!)!)!) *)
   XCDR;
   XCAR;
   XCDR;
   XCAR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CADDAR EXPR !1!) *)
(*  EXPR CADDAR *)
procedure PAS111;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CAR !(CDR !(CDR !(CAR !1!)!)!)!)!) *)
   XCAR;
   XCDR;
   XCDR;
   XCAR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CADDDR EXPR !1!) *)
(*  EXPR CADDDR *)
procedure PAS112;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CAR !(CDR !(CDR !(CDR !1!)!)!)!)!) *)
   XCDR;
   XCDR;
   XCDR;
   XCAR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CDAAAR EXPR !1!) *)
(*  EXPR CDAAAR *)
procedure PAS113;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CDR !(CAR !(CAR !(CAR !1!)!)!)!)!) *)
   XCAR;
   XCAR;
   XCAR;
   XCDR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CDAADR EXPR !1!) *)
(*  EXPR CDAADR *)
procedure PAS114;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CDR !(CAR !(CAR !(CDR !1!)!)!)!)!) *)
   XCDR;
   XCAR;
   XCAR;
   XCDR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CDADAR EXPR !1!) *)
(*  EXPR CDADAR *)
procedure PAS115;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CDR !(CAR !(CDR !(CAR !1!)!)!)!)!) *)
   XCAR;
   XCDR;
   XCAR;
   XCDR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CDADDR EXPR !1!) *)
(*  EXPR CDADDR *)
procedure PAS116;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CDR !(CAR !(CDR !(CDR !1!)!)!)!)!) *)
   XCDR;
   XCDR;
   XCAR;
   XCDR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CDDAAR EXPR !1!) *)
(*  EXPR CDDAAR *)
procedure PAS117;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CDR !(CDR !(CAR !(CAR !1!)!)!)!)!) *)
   XCAR;
   XCAR;
   XCDR;
   XCDR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CDDADR EXPR !1!) *)
(*  EXPR CDDADR *)
procedure PAS118;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CDR !(CDR !(CAR !(CDR !1!)!)!)!)!) *)
   XCDR;
   XCAR;
   XCDR;
   XCDR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CDDDAR EXPR !1!) *)
(*  EXPR CDDDAR *)
procedure PAS119;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CDR !(CDR !(CDR !(CAR !1!)!)!)!)!) *)
   XCAR;
   XCDR;
   XCDR;
   XCDR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CDDDDR EXPR !1!) *)
(*  EXPR CDDDDR *)
procedure PAS120;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CDR !(CDR !(CDR !(CDR !1!)!)!)!)!) *)
   XCDR;
   XCDR;
   XCDR;
   XCDR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CAAAR EXPR !1!) *)
(*  EXPR CAAAR *)
procedure PAS121;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CAR !(CAR !(CAR !1!)!)!)!) *)
   XCAR;
   XCAR;
   XCAR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CAADR EXPR !1!) *)
(*  EXPR CAADR *)
procedure PAS122;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CAR !(CAR !(CDR !1!)!)!)!) *)
   XCDR;
   XCAR;
   XCAR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CADAR EXPR !1!) *)
(*  EXPR CADAR *)
procedure PAS123;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CAR !(CDR !(CAR !1!)!)!)!) *)
   XCAR;
   XCDR;
   XCAR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CADDR EXPR !1!) *)
(*  EXPR CADDR *)
procedure PAS124;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CAR !(CDR !(CDR !1!)!)!)!) *)
   XCDR;
   XCDR;
   XCAR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CDAAR EXPR !1!) *)
(*  EXPR CDAAR *)
procedure PAS125;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CDR !(CAR !(CAR !1!)!)!)!) *)
   XCAR;
   XCAR;
   XCDR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CDADR EXPR !1!) *)
(*  EXPR CDADR *)
procedure PAS126;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CDR !(CAR !(CDR !1!)!)!)!) *)
   XCDR;
   XCAR;
   XCDR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CDDAR EXPR !1!) *)
(*  EXPR CDDAR *)
procedure PAS127;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CDR !(CDR !(CAR !1!)!)!)!) *)
   XCAR;
   XCDR;
   XCDR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY CDDDR EXPR !1!) *)
(*  EXPR CDDDR *)
procedure PAS128;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !1 !(CDR !(CDR !(CDR !1!)!)!)!) *)
   XCDR;
   XCDR;
   XCDR;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
procedure PAS129;
forward;
(* !(!*ENTRY PRIN!2 EXPR !1!) *)
(*  EXPR PRIN2 *)
procedure PAS129;
label
      103,
      102,
      101,
      100;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMPNC G!0!0!3!5 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 103;
(* !(!*LOAD !1 !(QUOTE !(!)!) *)
      mkident(41,1);
(* !(!*LINK WRTOK EXPR !1!) *)
     XWRTOK;
(* !(!*LBL G!0!0!3!7!) *)
100: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMPNC G!0!0!3!6 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 101;
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*LINK PRIN!2 EXPR !1!) *)
     PAS129;
(* !(!*MOVE !0 !(CDR !0!)!) *)
   ANYcdr(stk[st],stk[st]);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMPE G!0!0!3!7 !(QUOTE NIL!)!) *)
      IF R[1]=nilref THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE ! !)!) *)
      mkident(33,1);
(* !(!*LINK WRTOK EXPR !1!) *)
     XWRTOK;
(* !(!*JUMP G!0!0!3!7!) *)
      GOTO 100;
(* !(!*LBL G!0!0!3!6!) *)
101: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMPE G!0!0!4!4 !(QUOTE NIL!)!) *)
      IF R[1]=nilref THEN GOTO 102;
(* !(!*LOAD !1 !(QUOTE !.! !)!) *)
      mkident(137,1);
(* !(!*LINK WRTOK EXPR !1!) *)
     XWRTOK;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK PRIN!2 EXPR !1!) *)
     PAS129;
(* !(!*LBL G!0!0!4!4!) *)
102: 
(* !(!*LOAD !1 !(QUOTE !)!)!) *)
      mkident(42,1);
(* !(!*LBL G!0!0!3!5!) *)
103: 
(* !(!*LINK WRTOK EXPR !1!) *)
     XWRTOK;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY REVX EXPR !2!) *)
(*  EXPR REVX *)
procedure PAS130;
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
(* !(!*LBL G!0!0!5!1!) *)
100: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMPNC G!0!0!5!0 !1 PAIRTAG!) *)
      IF tag_of(R[1]) <> PAIRTAG THEN GOTO 101;
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LOAD !1 !(CAR !1!)!) *)
   XCAR;
(* !(!*LINK CONS EXPR !2!) *)
     XCONS;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*MOVE !0 !(CDR !0!)!) *)
   ANYcdr(stk[st],stk[st]);
(* !(!*JUMP G!0!0!5!1!) *)
      GOTO 100;
(* !(!*LBL G!0!0!5!0!) *)
101: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMPNIL G!0!0!5!6!) *)
      IF R[1] = nilref THEN GOTO 102;
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LINK CONS EXPR !2!) *)
     XCONS;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*LBL G!0!0!5!6!) *)
102: 
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY REV EXPR !1!) *)
(*  EXPR REV *)
procedure PAS131;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*LOAD !2 !(QUOTE NIL!)!) *)
      R[2] := nilref;
(* !(!*LINK REVX EXPR !2!) *)
     PAS130;
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY EOFP EXPR !1!) *)
(*  EXPR EOFP *)
procedure PAS132;
label
      101,
      100;
begin
(* !(!*ALLOC !1!) *)
    alloc1;
(* !(!*JUMPNC G!0!0!6!0 !1 ATOM!) *)
      IF tag_of(R[1]) = PAIRTAG THEN GOTO 100;
(* !(!*LINK !*INF EXPR !1!) *)
      mkitem(INTTAG,info_of(R[1]),R[1]);
(* !(!*JUMPN G!0!0!6!0 !(QUOTE !2!7!)!) *)
      mkitem(INTTAG,27,RXX);
      IF R[1] <> RXX THEN GOTO 100;
(* !(!*LOAD !1 !(QUOTE T!)!) *)
      R[1] := trueref;
(* !(!*JUMP G!0!0!6!2!) *)
      GOTO 101;
(* !(!*LBL G!0!0!6!0!) *)
100: 
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LBL G!0!0!6!2!) *)
101: 
(* !(!*DEALLOC !1!) *)
      dealloc1;
(* !(!*EXIT!) *)
end;
procedure PAS133;
forward;
(* !(!*ENTRY READ EXPR !0!) *)
(*  EXPR READ *)
procedure XREAD;
label
      104,
      103,
      102,
      101,
      100;
begin
(* !(!*ALLOC !2!) *)
    alloc2;
(* !(!*STORE NIL !-!1!) *)
      storenil(1);
(* !(!*LINK RDTOK EXPR !0!) *)
     XRDTOK;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LOAD !1 !(FLUID TOKTYPE!)!) *)
      R[1] := idspace[129].val;
(* !(!*JUMPN G!0!0!6!7 !(QUOTE !3!)!) *)
      mkitem(INTTAG,3,RXX);
      IF R[1] <> RXX THEN GOTO 100;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK EOFP EXPR !1!) *)
     PAS132;
(* !(!*JUMPNIL G!0!0!6!6!) *)
      IF R[1] = nilref THEN GOTO 101;
(* !(!*LBL G!0!0!6!7!) *)
100: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMP G!0!0!6!4!) *)
      GOTO 104;
(* !(!*LBL G!0!0!6!6!) *)
101: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMPN G!0!0!6!9 !(QUOTE !(!)!) *)
      mkitem(IDTAG,41,RXX);
      IF R[1] <> RXX THEN GOTO 102;
(* !(!*LINK RLIST EXPR !0!) *)
     PAS133;
(* !(!*JUMP G!0!0!6!4!) *)
      GOTO 104;
(* !(!*LBL G!0!0!6!9!) *)
102: 
(* !(!*JUMPN G!0!0!6!4 !(QUOTE !'!)!) *)
      mkitem(IDTAG,40,RXX);
      IF R[1] <> RXX THEN GOTO 104;
(* !(!*LINK READ EXPR !0!) *)
     XREAD;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*LINK EOFP EXPR !1!) *)
     PAS132;
(* !(!*JUMPNIL G!0!0!7!3!) *)
      IF R[1] = nilref THEN GOTO 103;
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*JUMP G!0!0!6!4!) *)
      GOTO 104;
(* !(!*LBL G!0!0!7!3!) *)
103: 
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK NCONS EXPR !1!) *)
     XNCONS;
(* !(!*LOAD !2 !(QUOTE QUOTE!)!) *)
      mkident(138,2);
(* !(!*LINK XCONS EXPR !2!) *)
     XXCONS;
(* !(!*JUMP G!0!0!6!4!) *)
      GOTO 104;
(* !(!*LOAD !1 !(QUOTE NIL!)!) *)
      R[1] := nilref;
(* !(!*LBL G!0!0!6!4!) *)
104: 
(* !(!*DEALLOC !2!) *)
      dealloc2;
(* !(!*EXIT!) *)
end;
(* !(!*ENTRY RLIST EXPR !0!) *)
(*  EXPR RLIST *)
procedure PAS133;
label
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
(* !(!*LINK READ EXPR !0!) *)
     XREAD;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*LINK EOFP EXPR !1!) *)
     PAS132;
(* !(!*JUMPT G!0!0!9!7!) *)
      IF R[1] <> nilref THEN GOTO 104;
(* !(!*STORE NIL !-!2!) *)
      storenil(2);
(* !(!*LBL G!0!0!8!4!) *)
100: 
(* !(!*LOAD !1 !-!2!) *)
      load(1,2);
(* !(!*JUMPT G!0!0!8!3!) *)
      IF R[1] <> nilref THEN GOTO 103;
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMPN G!0!0!8!8 !(QUOTE !)!)!) *)
      mkitem(IDTAG,42,RXX);
      IF R[1] <> RXX THEN GOTO 101;
(* !(!*LOAD !1 !(FLUID TOKTYPE!)!) *)
      R[1] := idspace[129].val;
(* !(!*JUMPN G!0!0!8!8 !(QUOTE !3!)!) *)
      mkitem(INTTAG,3,RXX);
      IF R[1] <> RXX THEN GOTO 101;
(* !(!*MOVE !-!2 !(QUOTE T!)!) *)
     stk[st-2] := trueref;
(* !(!*JUMP G!0!0!8!4!) *)
      GOTO 100;
(* !(!*LBL G!0!0!8!8!) *)
101: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMPN G!0!0!9!0 !(QUOTE !.!)!) *)
      mkitem(IDTAG,47,RXX);
      IF R[1] <> RXX THEN GOTO 102;
(* !(!*LOAD !1 !(FLUID TOKTYPE!)!) *)
      R[1] := idspace[129].val;
(* !(!*JUMPN G!0!0!9!0 !(QUOTE !3!)!) *)
      mkitem(INTTAG,3,RXX);
      IF R[1] <> RXX THEN GOTO 102;
(* !(!*MOVE !-!2 !(QUOTE T!)!) *)
     stk[st-2] := trueref;
(* !(!*LINK RLIST EXPR !0!) *)
     PAS133;
(* !(!*LINK CAR EXPR !1!) *)
     XCAR;
(* !(!*STORE !1 !-!3!) *)
      store(1,3);
(* !(!*JUMP G!0!0!8!4!) *)
      GOTO 100;
(* !(!*LBL G!0!0!9!0!) *)
102: 
(* !(!*LOAD !2 !-!1!) *)
      load(2,1);
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK CONS EXPR !2!) *)
     XCONS;
(* !(!*STORE !1 !-!1!) *)
      store(1,1);
(* !(!*LINK READ EXPR !0!) *)
     XREAD;
(* !(!*STORE !1 !0!) *)
      store10;
(* !(!*JUMP G!0!0!8!4!) *)
      GOTO 100;
(* !(!*LBL G!0!0!8!3!) *)
103: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*LINK EOFP EXPR !1!) *)
     PAS132;
(* !(!*JUMPNIL G!0!0!9!5!) *)
      IF R[1] = nilref THEN GOTO 105;
(* !(!*LBL G!0!0!9!7!) *)
104: 
(* !(!*LOAD !1 !0!) *)
      load10;
(* !(!*JUMP G!0!0!8!0!) *)
      GOTO 106;
(* !(!*LBL G!0!0!9!5!) *)
105: 
(* !(!*LOAD !2 !-!3!) *)
      load(2,3);
(* !(!*LOAD !1 !-!1!) *)
      load(1,1);
(* !(!*LINK REVX EXPR !2!) *)
     PAS130;
(* !(!*LBL G!0!0!8!0!) *)
106: 
(* !(!*DEALLOC !4!) *)
      dealloc(4);
(* !(!*EXIT!) *)
end;
    