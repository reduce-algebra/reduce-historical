%  <PSL.KERNEL>MINI-EDITOR.RED.3, 21-Sep-82 11:14:10, Edit by BENSON
%  Flagged internal functions

%. PSL Structure Editor Module;
%. Adapted By D. Morrison for PSL V1.
%. Based on Nordstroms trimmed InterLISP editor
%. Cleaned Up and commented by M. L. Griss, 
%. 8:57pm  Monday, 2 November 1981

%. See PH:Editor.Hlp for guide

CompileTime flag('(EDIT0 QEDNTH EDCOPY RPLACEALL FINDFIRST XCHANGE XINS),
		 'InternalFunction);

FLUID '(QEDITFNS        %. Keep track of which changed
        !*EXPERT        %. Do not print "help" if NIL
        !*VERBOSE       %. Dont do implicit "P" if NIL
        PROMPTSTRING!*  %. For "nicer" interface
        EditorReader!*  %. Use RLISP etc Syntax, ala Break
        EditorPrinter!*
        CL
);

QEDITFNS:=NIL;
!*Expert := NIL;
!*Verbose := NIL;

lisp procedure EDITF(FN);           %. Edit a Copy of Function Body
Begin scalar BRFL,X,SAVE,TRFL;
                %/ Capture !*BREAK, reset to NIL?
	X := GETD FN;
	If ATOM X OR CODEP CDR X then
	  StdError BldMsg("%r is not an editable function", Fn);
	SAVE:=COPY CDR X;
	EDIT CDR X;
	If YESP "Change Definition?" then GO TO YES;
	RPLACW(CDR X,SAVE); %/ Why not Just PUTD again?
        RETURN NIL;
YES:	If NULL (FN MEMBER QEDITFNS) then
		QEDITFNS:=FN.QEDITFNS; 
       	RETURN FN;
    END;

lisp procedure EDIT S;              %. Edit a Structure, S
begin scalar PROMPTSTRING!*;
  PROMPTSTRING!* := "edit> ";
  TERPRI();
  If NOT !*EXPERT then
    PRIN2T "Type HELP<CR> for a list of commands.";
        %/ Savea  copy for UNDO?
  RETURN EDIT0(S,EDITORREADER!* OR 'READ,EDITORPRINTER!* OR 'PRINT)
END;

lisp procedure EDIT0(S,READER,PRINTER);
	Begin scalar CL,CTLS,CTL,PLEVEL,TOP,TEMP,X,NNN;
	TOP:=LIST  S;
	PLEVEL:=3;
B:	CTL:=TOP; CTLS:=LIST CTL; CL:=CAR TOP;
NEXT:   If !*VERBOSE then APPLY(PRINTER,LIST EDCOPY(CL,PLEVEL));
	X:=APPLY(READER,NIL);
	If ATOM X then GO TO ATOMX else
	If NUMBERP CAR X then 
		If CAR X = 0 then GO TO ILLG else
		If CAR X > 0 then XCHANGE(QEDNTH(CAR X - 1,CL),CTL,CDR X,CAR X)
		else XINS(QEDNTH(-(CAR X + 1),CL),CTL,CDR X,CAR X)    else
	If CAR X = 'R then RPLACEALL(CADR X,CADDR X,CL) else GO TO ILLG;
	GO TO NEXT;
F:	TEMP:=FINDFIRST(APPLY(READER,NIL),CL,CTLS);
	If NULL TEMP 
	  then <<PRIN2T "NOT FOUND"; GO TO NEXT>>;
	 CL:=CAR TEMP;
	 CTLS:=CDR TEMP;
	 CTL:=CAR CTLS;
	 GO TO NEXT;
 ATOMX:  If NUMBERP X then If X = 0 then CL:=CAR CTL else GO TO NUMBX
      else
	 If X = 'P then !*VERBOSE OR APPLY(PRINTER,LIST EDCOPY(CL,PLEVEL)) else
	 If X = 'OK then RETURN CAR TOP else
	 If X = 'UP then GO TO UP else
	 If X = 'B then BREAK() else
	 If X = 'F then GO TO F else
	 If X = 'PL then PLEVEL:=APPLY(READER,NIL) else
	 If X MEMQ '(HELP !?) then EHELP() else
        If X EQ 'E then Apply(PRINTER,LIST EVAL Apply(READER,NIL)) else
	If X = 'T then GO TO B else GO TO ILLG;
	GO TO NEXT;
UP:	If CDR CTLS then GO TO UP1;
	PRIN2T "You are already at the top level";
	GO TO NEXT;
UP1:	CTLS:=CDR CTLS;
	CTL:=CAR CTLS;
	CL:=CAR CTL;
	GO TO NEXT;
NUMBX:	NNN := X;
	X:=QEDNTH(ABS(X),CL);
	If NULL X then <<
	  PRIN2T "List empty";
	  GO TO NEXT >>;
	If NNN > 0 then
	  CL:=CAR X;
	CTL:=X;
	CTLS:=CTL.CTLS;
	GO TO NEXT;
ILLG:	PRIN2T "Illegal command";
	GO TO NEXT   
END;

lisp procedure QEDNTH(N,L); 
 If ATOM L then NIL else If N > 1 then QEDNTH(N-1,CDR L) else L;

lisp procedure EDCOPY(L,N);
If ATOM L then L else If N < 0 then 
  "***" else EDCOPY(CAR L,N-1).EDCOPY(CDR L,N);

lisp procedure RPLACEALL(A,NEW,S);
If ATOM S then NIL else If CAR S = A then 
RPLACEALL(A,NEW,CDR RPLACA(S,NEW)) else
	<<RPLACEALL(A,NEW,CAR S); RPLACEALL(A,NEW,CDR S)>>;

lisp procedure FINDFIRST(A,S,TRC);      %. FIND Occurance of A in S
 Begin scalar RES;
   If ATOM S then RETURN NIL;
   If A MEMBER S then RETURN S. TRC;
   RETURN(FINDFIRST(A,CAR S,S.TRC) or FINDFIRST(A,CDR S,TRC));
 %/ Add a PMAT here
 END;

lisp procedure XCHANGE(S,CTL,NEW,N);
	If ATOM S then <<PRIN2T "List empty"; NIL>> else
	If N = 1 then <<RPLACA(CTL,NCONC(NEW,CDR S)); CL:=CAR CTL>> else
	RPLACD(S,NCONC(NEW,If CDDR S then CDDR S else NIL));

lisp procedure XINS(S,CTL,NEW,N);
	If ATOM S then <<PRIN2T "List empty"; NIL>> else
	If N = 1 then <<RPLACA(CTL,NCONC(NEW,S)); CL:=CAR CTL>> else
	RPLACD(S,NCONC(NEW,CDR S));

UNFLUID '(CL);

lisp procedure EHELP;
<<  EvLoad '(Help);
    DisplayHelpFile 'Editor >>;

PUT('EDIT,	'HelpFunction,	'EHELP);
PUT('EDITF,	'HelpFunction,	'EHELP);
PUT('EDITOR,	'HelpFunction,	'EHELP);

END;
