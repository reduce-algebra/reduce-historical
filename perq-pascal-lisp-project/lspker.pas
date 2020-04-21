(* include following two lines for terak *)
(*  [$s+] *) (* swapping mode to manage this large file *)
(*  [$g+] *) (* goto is legal *)

PROGRAM Paslsp(symin, input, output);

    (************************************************************)
    (* this file contains global data declarations and          *)
    (* function definitions to support a sub-standard lisp      *)
    (* system.  it is used with a compiler which compiles lisp  *)
    (* to pascal source code. this file is divided into the     *)
    (* following sections:                                      *)
    (*     1. constant, type & global variable declarations.    *)
    (*     2. lisp item selectors & constructors - these are    *)
    (*        the functions which know about the internal       *)
    (*        (pascal) representation of lisp data primitives.  *)
    (*        currently these are: integers (-4096..4095),      *)
    (*        characters, dotted pairs, identifiers,            *)
    (*        code pointers, error conditions, large integers & *)
    (*        floating point numbers (most hooks exist).        *)
    (*     3. stack allocation - variables local to a function  *)
    (*        are kept on a stack.                              *)
    (*     4. the garbage collector.                            *)
    (*     5. identifier lookup & entry - symbol table          *)
    (*        management.                                       *)
    (*     6. standard lisp functions - pascal implementations  *)
    (*        taking lisp items as arguments and returning a    *)
    (*        lisp item. more standard lisp functions are found *)
    (*        in lspfns.red.                                    *)
    (*     7. i/o primitives (not callable from lisp functions).*)
    (*     8. a lisp callable token scanner.                    *)
    (*     9. initialization.                                   *)
    (*    10. apply                                             *)
    (************************************************************)
    (* symin is input channel one--used to initialize "symbol   *)
    (* table".  input is input channel two--standard input.     *)
    (* output is output channel one--the standard output.       *)
    (************************************************************)
    (* written by martin l. griss, william f. galway and        *)
    (* ralph ottenheimer.                                       *)
    (* last changed 16 june 1981                                *)
    (************************************************************)

CONST

    (* constants relating to input / output *)
    sp = ' ';
    nul = 0;          (* ascii codes *)
    ht = 9;
    lf = 10;
    cr = 13;

    inchns = 2;       (* number of input channels.  *)
    outchns = 1;      (* number of output channels. *)
    eofcode = 26;     (* magic character code for eof, ascii for *)
    (*                   cntrl-z.   kludge, see note in rdtok.  *)
    choffset = 1;     (* add choffset to ascii code to get address  *)
    (*                   in id space for corresponding identifier.  *)
    eos = nul;        (* terminator character for strings. *)

    (* constants relating to the token scanner *)
    toktype  =  129; (* slot in idspace for toktype. *)
    chartype  =  3;   (* various token types *)
    inttype  =  1;
    idtype  =  2;

    (* constants relating to lisp data types and their representations. *)
    shift_const = 8192; (* tags and info are packed into an integer *)
    (* assumed to be at least 16 bits long.  low order 13 bits  *)
    (* are the info, top 3 are the tag.                         *)
    int_offset = 4096;  (* small integers are stored 0..8191    *)
    (* instead of -4096..4095 because it will pack smaller      *)
    (* under ucsd pascal.                                       *)
    end_flag = -1;  (* marks end of fixnum free list. *)

    (* the various tags - can't use a defined scalar type *)
    (* because of the lack of convertion functions.       *)
    inttag = 0;    (* info is an integer                  *)
    chartag = 1;   (* info is a character code            *)
    pairtag = 2;   (* info points to pair                 *)
    idtag = 3;     (* info points to identifier           *)
    codetag = 4;   (* info is index into a case statement *)
    (*                that calls appropriate function.    *)
    errtag = 5;    (* info is an error code - see below.  *)
    fixtag = 6;    (* info points to a full word (or      *)
    (*                longer) integer.                    *)
    flotag = 7;    (* info points to a float number.      *)

    (* error codes.  corresponding to tag = errtag.  *)
    noprspace = 1;    (* no more "pair space"--can't cons. *)
    notpair = 2;      (* a pair operation attempted on a non-pair. *)
    noidspace = 3;    (* no more free identifiers *)
    undefined = 4;    (* used to mark undefined function cells (etc?) *)

    (* constants relating to data space *)
    maxpair = 2500;   (* max number of pairs allowed. *)
    maxident = 400;   (* max number of identifiers *)
    maxstrsp = 2000;  (* size of string (literal) storage space. *)
    maxintsp = 50;    (* max number of long integers allowed *)
    maxflosp = 2;     (* max number of floating numbers allowed *)
    maxgcstk = 100;   (* size of garbage collection stack.    *)
    stksize = 500;    (* stack size *)

    (* constants relating to the symbol table. *)
    hidmax = 50;      (* number of hash values for identifiers *)
    nillnk = 0;       (* when integers are used as pointers.  *)


TYPE
    onechar = char;

    (* note we allow zero for id_ptr, allowing a "nil" link. *)
    stringp = 1..maxstrsp;        (* pointer into string space. *)
    id_ptr = 0..maxident;         (* pointer into id space. *)

    any = integer;  (* your basic lisp item *)
    itemtype = 0..7;    (* the tags *)


    pair = PACKED RECORD
		      prcar: any;
		      prcdr: any;
		      markflg: boolean;        (* for garbage collection   *)
		  END;


    ascfile = PACKED FILE OF onechar;

    ident = PACKED RECORD           (* identifier *)
		       idname: stringp;
		       val: any;       (* value *)
		       plist: any;     (* property list *)
		       funcell: any;   (* function cell *)
		       idhlink: id_ptr;    (* hash link *)
		   END;
    longint = integer;  (* use integer[n] on terak *)

VAR
    (* global information *)
    xnil, t: any;    (* refers to identifiers "nil", and "t". *)
    junk: any;     (* global to hold uneeded function results *)
    old_binds: any;	(* saved fluid bindings *)

    (* "st" is the stack pointer into "stk".  it counts the number of  *)
    (* items on the stack, so it runs from zero while the stack starts *)
    (* at one.                                                         *)
    st: 0..stksize;
    stk: ARRAY[1..stksize] OF any;

    (* pair space *)
    prspace: PACKED ARRAY[1..maxpair] OF pair; (* all pairs stored here. *)
    freepair: integer;          (* pointer to next free pair in prspace. *)

    (* identifier space *)
    idhead: ARRAY[0..hidmax] OF id_ptr;
    idspace: PACKED ARRAY[1..maxident] OF ident;
    freeident: integer;

    (* string space *)
    strspace: PACKED ARRAY[1..maxstrsp] OF onechar;
    freestr: stringp;

    (* large integer space *)
    intspace: ARRAY[1..maxintsp] OF longint;
    freeint: 1..maxintsp;

    (* floating point number space *)
    flospace: ARRAY[1..maxflosp] OF real;
    freefloat: 1..maxflosp;

    (* i/o channels *)
    symin: ascfile;
    input: ascfile; (* comment out for terak. *)

    inchnl: 1..inchns;      (* current input channel number  *)
    outchnl: 1..outchns;    (* current output channel number *)

    (* "current character" for each input channel.                    *)
    (* may want to include more than one character at some later date *)
    (* (for more lookahead).                                          *)
    ichrbuf: ARRAY[1..inchns] OF onechar;

    (* for collecting statistics. *)
    gccount: integer;           (* counts garbage collections *)
    (* counts from last garbage collection. *)
    consknt: integer;           (* number of times "cons" called *)
    pairknt: integer;           (* number of pairs created *)


    (********************************************************)
    (*                                                      *)
    (*             item selectors & constructors            *)
    (*                                                      *)
    (********************************************************)

FUNCTION Truep(predicate: any): boolean;
    BEGIN (* truep *)
    Truep := predicate <> xnil
    END  (* truep *);

FUNCTION Falsep(predicate: any): boolean;
    BEGIN (* Falsep *)
    Falsep := predicate = xnil
    END (* Falsep *);

FUNCTION Tag_of(item: any): itemtype;
    BEGIN (* tag_of *)
    Tag_of := item DIV shift_const;
    END;
    (* tag_of *)

FUNCTION Info_of(item: any): integer;
    BEGIN (* info_of *)
    IF item DIV shift_const = inttag THEN
	Info_of := item MOD shift_const - int_offset
    ELSE
	Info_of := item MOD shift_const
    END;
    (* info_of *)

FUNCTION Mkitem(tag: itemtype; info: longint): any;
    (* do range checking on info. ints run from -4096 to +4095 *)
    (* everything else runs from 0 to 8191. ints & chars       *)
    (* contain their info, all others points into an           *)
    (* appropriate space.                                      *)

    BEGIN (* mkitem *)
    IF info < 0 THEN        (* this check probably not necessary *)
	Writeln('*****MKITEM: BAD NEG');

    (* pack tag and info into 16-bit item.   *)
    Mkitem := tag * shift_const + info
    END    (* mkitem *);


PROCEDURE Set_info(VAR item: any; newinfo: longint);
    BEGIN (* set_info *)
    item := Mkitem(Tag_of(item), newinfo)
    END;
    (* set_info *)

PROCEDURE Set_tag(VAR item: any; newtag: itemtype);
    BEGIN (* set_tag *)
    item := Mkitem(newtag, Info_of(item))
    END;
    (* set_tag *)

FUNCTION Mkident(id: integer): any;
    BEGIN       (* mkident *)
    Mkident := Mkitem(idtag, id);
    END;
    (* mkident *)

FUNCTION Car(u: any): any; FORWARD;
FUNCTION Cdr(u: any): any; FORWARD;
FUNCTION Pairp(item: any): any; FORWARD;

FUNCTION Mkfixint(fixint: longint): any;
    VAR p: integer;

    PROCEDURE Gc_int;       (* Garbage collect large integer space. *)
	VAR i: integer;
	    mark_flag: PACKED ARRAY[1..maxintsp] OF boolean;

	PROCEDURE Mark(u: any);
	    BEGIN (* mark *)
	    IF Truep(Pairp(u)) THEN
		BEGIN
		Mark(Car(u));
		Mark(Cdr(u))
		END
	    ELSE IF Tag_of(u) = fixtag THEN
		mark_flag[Info_of(u)] := true
	    END; (* mark *)

	BEGIN (* gc_int *)
	FOR i := 1 TO maxintsp DO       (* clear mark flags *)
	    mark_flag[i] := false;

	FOR i := 1 TO st DO             (* mark from the stack *)
	    Mark(stk[i]);

	FOR i := 1 TO maxident DO       (* mark from the symbol table *)
	    BEGIN
	    Mark(idspace[i].val);
	    Mark(idspace[i].plist);
	    Mark(idspace[i].funcell)
	    END;

	(* reconstruct free list *)
	FOR i := 1 TO maxintsp - 1 DO
	    IF NOT mark_flag[i] THEN
		BEGIN
		intspace[i] := freeint;
		freeint := i
		END
	END; (* gc_int *)


    BEGIN (* mkfixint *)
    IF intspace[freeint] = end_flag THEN Gc_int;
    IF intspace[freeint] <> end_flag THEN     (* convert to fixnum *)
	BEGIN
	p := freeint;
	freeint := intspace[freeint];
	Mkfixint := Mkitem(fixtag, p);
	intspace[p] := fixint
	END
    ELSE Writeln('*****FIXNUM SPACE EXHAUSTED')
    END  (* mkfixint *);

FUNCTION Mkint(int: longint): any;
    BEGIN       (* mkint *)
    IF (int < -int_offset) OR (int > int_offset - 1) THEN
	Mkint := Mkfixint(int)
    ELSE
	Mkint := Mkitem(inttag, int + int_offset)
	(* int was in range so add offset *)
    END    (* mkint *);

FUNCTION Mkpair(pr: integer): any;
    BEGIN (* mkpair *)
    Mkpair := Mkitem(pairtag, pr)
    END;
    (* mkpair *)

PROCEDURE Int_val(item: any; VAR number: longint);
    (* returns integer value of item (int or fixnum). *)
    (* must return 'number' in var parameter instead  *)
    (* of function value since long integers are not  *)
    (* a legal function type in ucsd pascal.          *)
    BEGIN (* int_val *)
    IF Tag_of(item) = inttag THEN
	number := Info_of(item)
    ELSE IF Tag_of(item) = fixtag THEN
	number := intspace[Info_of(item)]
    ELSE Writeln('***** ILLEGAL DATA TYPE FOR NUMERIC OPERATION')
    END    (* int_val *);


    (********************************************************)
    (*                                                      *)
    (*                  stack allocation                    *)
    (*                                                      *)
    (********************************************************)

PROCEDURE Alloc(n: integer);
    BEGIN
    IF n + st <= stksize THEN
	st := n+st
    ELSE
	BEGIN
	Writeln('*****LISP STACK OVERFLOW');
	Writeln('     TRIED TO ALLOCATE ',n);
	Writeln('     CURRENT STACK TOP IS ',st);
	END;
    END;

PROCEDURE Dealloc(n: integer);
    BEGIN
    IF st - n >= 0 THEN
	st := st - n
    ELSE
	Writeln('*****LISP STACK UNDERFLOW');
    END;

    (* optimized allocs *)

PROCEDURE Alloc1;
    BEGIN Alloc(1) END;

PROCEDURE Dealloc1;
    BEGIN Dealloc(1) END;

PROCEDURE Alloc2;
    BEGIN Alloc(2) END;

PROCEDURE Dealloc2;
    BEGIN Dealloc(2) END;

PROCEDURE Alloc3;
    BEGIN Alloc(3) END;

PROCEDURE Dealloc3;
    BEGIN Dealloc(3) END;


    (********************************************************)
    (*                                                      *)
    (*              the garbage collector                   *)
    (*                                                      *)
    (********************************************************)

PROCEDURE Faststat;
    (* give quick summary of statistics gathered *)
    BEGIN
    Writeln('CONSES:',consknt);
    Writeln('PAIRS :',pairknt);
    Writeln('CONSES/PAIRS: ',consknt/pairknt);
    Writeln('ST    :',st);
    END;


PROCEDURE Gcollect;
    VAR
	i: integer;
	markedk: integer;   (* counts the number of pairs marked *)
	freedk: integer;    (* counts the number of pairs freed. *)
	gcstkp: 0..maxgcstk; (* note the garbage collection stack   *)
	mxgcstk: 0..maxgcstk;           (* is local to this procedure. *)
	gcstk: ARRAY[1..maxgcstk] OF integer;

    PROCEDURE Pushref(pr: any);
	(* push the address of an unmarked pair, if that's what it is. *)
	BEGIN
	IF Tag_of(pr) = pairtag THEN
	    IF NOT prspace[Info_of(pr)].markflg THEN
		BEGIN
		IF gcstkp < maxgcstk THEN
		    BEGIN
		    gcstkp := gcstkp + 1;
		    gcstk[gcstkp] := Info_of(pr);
		    IF gcstkp > mxgcstk THEN
			mxgcstk := gcstkp;
		    END
		ELSE
		    Writeln('*****GARBAGE STACK OVERFLOW');
		(* fatal error *)
		END;
	END;

    PROCEDURE Mark;
	(* "recursively" mark pairs referred to from gcstk. gcstk is used to *)
	(* simulate recursion.                                               *)
	VAR
	    prloc: integer;
	BEGIN
	WHILE gcstkp > 0 DO
	    BEGIN
	    prloc := gcstk[gcstkp];
	    gcstkp := gcstkp - 1;
	    prspace[prloc].markflg := true;
	    Pushref(prspace[prloc].prcdr);
	    Pushref(prspace[prloc].prcar);  (* trace the car first. *)
	    END;
	END;

    BEGIN       (* gcollect *)
    Writeln('***GARBAGE COLLECTOR CALLED');
    gccount := gccount + 1;          (* count garbage collections. *)
    Faststat;   (* give summary of statistics collected *)
    consknt := 0;       (* clear out the cons/pair counters *)
    pairknt := 0;
    gcstkp := 0;                    (* initialize the garbage stack pointer. *)
    mxgcstk := 0;                   (* keeps track of max stack depth. *)

    (* mark things from the "computation" stack. *)
    FOR i := 1 TO st DO
	BEGIN
	Pushref(stk[i]);
	Mark;
	END;
    (* mark things from identifier space. *)
    FOR i := 1 TO maxident DO
	BEGIN
	Pushref(idspace[i].val);
	Mark;
	Pushref(idspace[i].plist);
	Mark;
	Pushref(idspace[i].funcell);
	Mark;
	END;

    (* reconstruct free list by adding things to the head. *)
    freedk := 0;
    markedk := 0;
    FOR i:= 1 TO maxpair - 1 DO
	BEGIN
	IF prspace[i].markflg THEN
	    BEGIN
	    markedk := markedk + 1;
	    prspace[i].markflg := false
	    END
	ELSE
	    BEGIN
	    prspace[i].prcar := xnil;
	    prspace[i].prcdr := Mkitem(pairtag, freepair);
	    freepair := i;
	    freedk := freedk + 1
	    END
	END (* for *);
    Writeln(freedk,' PAIRS FREED.');
    Writeln(markedk,' PAIRS IN USE.');
    Writeln('MAX GC STACK WAS ',mxgcstk);
    END    (* gcollect *);


    (********************************************************)
    (*                                                      *)
    (*              identifier lookup & entry               *)
    (*                                                      *)
    (********************************************************)

FUNCTION Nmhash(nm: stringp): integer;
    CONST
	hashc = 256;
    VAR
	i,tmp: integer;
    BEGIN
    tmp := 0;
    i := 1;     (* get hash code from first three chars of string. *)
    WHILE (i <= 3) AND (strspace[nm+i] <> Chr(eos)) DO
	BEGIN
	tmp := Ord(strspace[nm+i]) + hashc*tmp;
	i := i + 1;
	END;
    Nmhash := Abs(tmp) MOD hidmax;      (* abs because mod is screwy. *)
    END;

FUNCTION Eqstr(s1,s2: stringp): boolean;
    BEGIN
    WHILE (strspace[s1] = strspace[s2]) AND (strspace[s1] <> Chr(eos)) DO
	BEGIN
	s1 := s1 + 1;
	s2 := s2 + 1;
	END;
    Eqstr := (strspace[s1] = strspace[s2]);
    END;

PROCEDURE Nmlookup(nm: stringp; VAR found: boolean; VAR hash: integer;
		   VAR loc: any);
    (* lookup a name in "identifier space".                                 *)
    (* "hash" returns the hash value for the name.                          *)
    (* "loc" returns the location in the space for the (possibly new)       *)
    (* identifier.                                                          *)
    BEGIN
    hash := Nmhash(nm);
    loc := Mkitem(idtag, idhead[hash]);
    (* default is identifier, but may be "error". *)
    (* start at appropriate hash chain. *)

    found := false;
    WHILE (Info_of(loc) <> nillnk) AND (NOT found) DO
	BEGIN
	found := Eqstr(nm, idspace[Info_of(loc)].idname);
	IF NOT found THEN
	    Set_info(loc, idspace[Info_of(loc)].idhlink);
	(* next id in chain *)
	END;
    IF NOT found THEN               (* find spot for new identifier *)
	BEGIN
	IF freeident=nillnk THEN    (* no more free identifiers. *)
	    loc := Mkitem(errtag, noidspace)
	ELSE
	    BEGIN
	    Set_info(loc, freeident);
	    freeident := idspace[freeident].idhlink;
	    END;
	END;
    END;

PROCEDURE Putnm(nm: stringp; VAR z: any; VAR found: boolean);
    (* put a new name into identifier space, or return old location *)
    (* if it's already there.                                       *)
    VAR
	tmp: ident;
	hash: integer;
    BEGIN
    Nmlookup(nm, found, hash, z);
    IF (NOT found) AND (Tag_of(z) = idtag) THEN
	BEGIN
	tmp.idname := nm;
	tmp.idhlink := idhead[hash];   (* put new ident at head of chain     *)
	tmp.val := xnil;             (* initialize value and property list *)
	tmp.plist := xnil;
	tmp.funcell := xnil;         (* also, the function cell *)
	idhead[hash] := Info_of(z);
	idspace[Info_of(z)] := tmp;
	END;
    END;

    (********************************************************)
    (*                                                      *)
    (*               standard lisp functions                *)
    (*                                                      *)
    (********************************************************)

    (* the following standard lisp functions appear in *)
    (* lspfns.red: reverse, append, memq, atsoc, get,  *)
    (* put, remprop, eq, null, equal, error, errorset, *)
    (* abs, idp, numberp, atom, minusp, eval, apply,   *)
    (* evlis, prin1, print, prin2t, list2 ... list5.   *)

FUNCTION Setq(VAR u: any; v: any): any;
    BEGIN   (* setq *)
    (* should check to make sure u not t or nil. *)
    u := v;
    Setq := v
    END  (* setq *);

FUNCTION Atom(item : any): any;
    BEGIN       (* atom *)
    IF Tag_of(item) <> pairtag THEN Atom := t
    ELSE Atom := xnil
    END (* atom *);

FUNCTION Codep(item: any): any;
    BEGIN       (* codep *)
    IF Tag_of(item) = codetag THEN Codep := t
    ELSE Codep := xnil
    END (* codep *);

FUNCTION Idp(item: any): any;
    BEGIN       (* idp *)
    IF Tag_of(item) = idtag THEN Idp := t
    ELSE Idp := xnil
    END (* idp *);

FUNCTION Pairp(*item: any): any*);
    BEGIN (* pairp *)
    IF Tag_of(item) = pairtag THEN Pairp := t
    ELSE Pairp := xnil
    END (* pairp *);

FUNCTION Constantp(item: any): any;
    BEGIN       (* constantp *)
    IF NOT((Pairp(item) = t) OR (Idp(item) = t)) THEN
	Constantp := t
    ELSE Constantp := xnil
    END (* constantp *);

FUNCTION Eq(u, v: any): any;
    BEGIN       (* eq *)
    IF u = v THEN Eq := t
    ELSE Eq := xnil
    END (* eq *);

FUNCTION Eqn(u, v: any): any;
    VAR i, j: longint;

    BEGIN       (* eqn *)
    Int_val(u, i);
    Int_val(v, j);
    IF i = j THEN Eqn := t
    ELSE Eqn := xnil
    END (* eqn *);

FUNCTION Fixp(item: any): any;
    BEGIN       (* fixp *)
    IF (Tag_of(item) = inttag) OR (Tag_of(item) = fixtag) THEN
	Fixp := t
    ELSE Fixp := xnil
    END (* fixp *);

FUNCTION Floatp(item: any): any;
    BEGIN       (* floatp *)
    IF Tag_of(item) = flotag THEN Floatp := t
    ELSE Floatp := xnil
    END (* floatp *);

FUNCTION Numberp(item: any): any;
    BEGIN       (* numberp *)
    Numberp := Fixp(item)   (* will have to fix for floats *)
    END (* numberp *);

FUNCTION Cons(u, v: any): any;
    VAR p: integer;

    BEGIN   (* cons *)
    (* push args onto stack, in case we need to garbage collect the *)
    (* references will be detected.                                 *)
    Alloc(2);
    stk[st] := u;
    stk[st-1] := v;

    IF prspace[freepair].prcdr = xnil THEN Gcollect;

    p := freepair;
    freepair := Info_of(prspace[p].prcdr);
    prspace[p].prcar := u;
    prspace[p].prcdr := v;
    Cons := Mkpair(p);       (* return new pair. *)

    consknt := consknt + 1;
    Dealloc(2);
    END     (* cons *);

FUNCTION Ncons(u: any): any;
    BEGIN
    Ncons := Cons(u, xnil)
    END;

FUNCTION Xcons(u, v: any): any;
    BEGIN
    Xcons := Cons(v, u)
    END;

FUNCTION Car(*u: any): any*);
    BEGIN
    IF Tag_of(u) = pairtag THEN
	Car := prspace[Info_of(u)].prcar
    ELSE
	Car := Mkitem(errtag, notpair);
    END;

FUNCTION Cdr(*u: any): any*);
    BEGIN
    IF Tag_of(u) = pairtag THEN
	Cdr := prspace[Info_of(u)].prcdr
    ELSE
	Cdr := Mkitem(errtag, notpair);
    END;


    (* fluid binding *)
FUNCTION Push_bind(bind: any): any;
    BEGIN	(* push_bind *)
    old_binds := cons(bind, old_binds);
    push_bind := xnil
    END	(* push_bind *);

FUNCTION Lam_bind(alist: any): any;
    VAR bind: any;

    BEGIN (* lam_bind *)
    WHILE Truep(Pairp(alist)) DO
	BEGIN
	bind := Car(alist);
	alist := Cdr(alist);
	push_bind(bind);
	setvalue(Car(bind), Cdr(bind))
	END;
    Lam_bind := xnil
    END  (* lam_bind *);

FUNCTION Prog_bind(id: any): any;
    BEGIN (* prog_bind *)
    Prog_bind := Lam_bind(cons(id, xnil))
    END (* prog_bind *);

FUNCTION Unbind(id: any): any;
    BEGIN (* unbind *)
    setvalue(id, cdr(atsoc(id, old_binds)))
    Unbind := xnil
    END (* unbind *);

    (* arithmetic functions *)
FUNCTION Add1(i: any): any;
    VAR j: longint;

    BEGIN
    Int_val(i, j);
    Add1 := Mkint(j + 1)
    END;

FUNCTION Difference(i, j: any): any;
    VAR i1, i2: longint;

    BEGIN
    Int_val(i, i1);
    Int_val(j, i2);
    Difference := Mkint(i1 - i2)
    END;

FUNCTION Divide(i, j: any): any;
    (* returns dotted pair (quotient . remainder). *)
    VAR i1, i2: longint;

    BEGIN
    Int_val(i, i1);
    Int_val(j, i2);
    IF i2 = 0 THEN Writeln('***** ATTEMPT TO DIVIDE BY 0 IN DIVIDE');
    Divide := Cons(Mkint(i1 DIV i2), Mkint(i1 MOD i2))
    END;

FUNCTION Greaterp(i, j: any): any;
    VAR i1, i2: longint;

    BEGIN
    Int_val(i, i1);
    Int_val(j, i2);

    IF i1 > i2 THEN
	Greaterp := t
    ELSE
	Greaterp := xnil;
    END;

FUNCTION Lessp(i, j: any): any;
    VAR i1, i2: longint;

    BEGIN
    Int_val(i, i1);
    Int_val(j, i2);

    IF i1 < i2 THEN
	Lessp := t
    ELSE
	Lessp := xnil;
    END;

FUNCTION Minus(i: any): any;
    VAR j: longint;

    BEGIN
    Int_val(i, j);
    Minus := Mkint(-j)
    END;

FUNCTION Plus2(i, j: any): any;
    VAR i1, i2: longint;

    BEGIN
    Int_val(i, i1);
    Int_val(j, i2);
    Plus2 := Mkint(i1 + i2)
    END;

FUNCTION Quotient(i, j: any): any;
    VAR i1, i2: longint;

    BEGIN
    Int_val(i, i1);
    Int_val(j, i2);
    IF i2 = 0 THEN Writeln('***** ATTEMPT TO DIVIDE BY 0 IN QUOTIENT');
    Quotient := Mkint(i1 DIV i2)
    END;

FUNCTION Remainder(i, j: any): any;
    VAR i1, i2: longint;

    BEGIN
    Int_val(i, i1);
    Int_val(j, i2);
    IF i2 = 0 THEN Writeln('***** ATTEMPT TO DIVIDE BY 0 IN REMAINDER');
    Remainder := Mkint(i1 MOD i2)
    END;

FUNCTION Times2(i, j: any): any;
    VAR i1, i2: longint;

    BEGIN
    Int_val(i, i1);
    Int_val(j, i2);
    Times2 := Mkint(i1 * i2)
    END;
    (* times2 *)

    (* symbol table support *)
FUNCTION Value(u: any): any;
    BEGIN   (* value *)
    Value := idspace[Info_of(u)].val
    END     (* value *);

FUNCTION Plist(u: any): any;
    BEGIN   (* plist *)
    Plist := idspace[Info_of(u)].plist
    END     (* plist *);

FUNCTION Funcell(u: any): any;
    BEGIN   (* funcell *)
    Funcell := idspace[Info_of(u)].funcell
    END     (* funcell *);

FUNCTION Setplist(u, v: any): any;
    BEGIN (* setplist *)
    END (* setplist *);

    (* also need setvalue, setfuncell, setplist. *)

FUNCTION Xnot(u: any): any;
    BEGIN (* xnot *)
    Xnot := Eq(u, xnil)
    END (* xnot *);


    (********************************************************)
    (*                                                      *)
    (*                    i/o primitives                    *)
    (*                                                      *)
    (********************************************************)


PROCEDURE Terpri;
    (* need to change for multiple output channels.  *)
    BEGIN
    Writeln(output);
    END;


PROCEDURE Wrtok(u: any);
    (* doesn't expand escaped characters in identifier names *)
    VAR i: integer;
    BEGIN
    IF Tag_of(u) = inttag THEN
	IF Info_of(u) = 0 THEN
	    Write('0')
	ELSE
	    Write(Info_of(u): 2+Trunc(Log(Abs(Info_of(u)))))

    ELSE IF Tag_of(u) = fixtag THEN
	Write(intspace[Info_of(u)])

    ELSE IF Tag_of(u) = flotag THEN
	Write(flospace[Info_of(u)])

    ELSE IF Tag_of(u) = idtag THEN
	BEGIN
	i := idspace[Info_of(u)].idname;
	WHILE (i <= maxstrsp) AND (strspace[i] <> Chr(eos)) DO
	    BEGIN
	    Write(strspace[i]);
	    i:= i + 1;
	    END;
	END

    ELSE IF Tag_of(u) = chartag THEN
	Write(Chr(Info_of(u) - choffset))

    ELSE
	Writeln('WRTOK GIVEN ',Tag_of(u), Info_of(u));
    END;


PROCEDURE Rdchnl(chnlnum: integer; VAR ch: onechar);
    BEGIN
    IF (chnlnum < 1) OR (chnlnum > inchns) THEN
	Writeln('*****BAD INPUT CHANNEL FOR RDCHNL')
    ELSE
	CASE chnlnum OF
	    1:  BEGIN
		ch := symin^;  (* a little strange, but avoids  *)
		Get(symin);              (* initialization problems *)
		ichrbuf[inchnl] := symin^;
		END;

	    2:  BEGIN
		ch := input^;
		Get(input);
		ichrbuf[inchnl] := input^;
		END;
		END;
    (* case *)
    END;
    (* rdchnl *)

FUNCTION Eofchnl(chnlnum: integer): boolean;
    BEGIN
    IF (chnlnum < 1) OR (chnlnum > inchns) THEN
	Writeln('*****BAD INPUT CHANNEL FOR EOFCHNL')
    ELSE
	CASE chnlnum OF
	    1:  Eofchnl := Eof(symin);
	    2:  Eofchnl := Eof(input);
		END;
    END;

    (********************************************************)
    (*                                                      *)
    (*                   token scanner                      *)
    (*                                                      *)
    (********************************************************)

FUNCTION Rdtok: any;
    VAR
	ch: onechar;
	i: integer;
	anint: longint;
	moreid: boolean;
	found: boolean;
	token: any; (* the token read *)

    FUNCTION Digit(ch: onechar): boolean;
	BEGIN
	Digit := ( '0' <= ch ) AND ( ch <= '9')
	END;

    FUNCTION Escalpha(VAR ch: onechar): boolean;
	(* test for alphabetic or escaped character.  *)
	(* note possible side effect.                 *)
	BEGIN   (* escalpha *)
	IF ( 'A' <= ch ) AND ( ch <= 'Z') THEN
	    Escalpha := true
	ELSE IF ( Ord('A')+32 <= Ord(ch)) AND ( Ord(ch) <= Ord('Z')+32) THEN
	    Escalpha := true    (* lower case alphabetics *)
	ELSE IF ch='!' THEN
	    BEGIN
	    Rdchnl(inchnl,ch);
	    Escalpha := true;
	    END
	ELSE
	    Escalpha := false;
	END     (* escalpha *);

    FUNCTION Alphanum(VAR ch: onechar): boolean;
	(* test if escalfa or digit *)
	VAR b: boolean;
	BEGIN
	b := Digit(ch);
	IF NOT b THEN b := Escalpha(ch);
	Alphanum := b;
	END;

    FUNCTION Whitesp(ch: onechar): boolean;
	BEGIN
	(* may want a faster test *)
	Whitesp := (ch = sp) OR (Ord(ch) = cr) OR (Ord(ch) = lf)
	OR (Ord(ch) = ht) OR (Ord(ch) = nul)
	END;


	(* reads fixnums...need to read flonums too *)
    BEGIN       (* rdtok *)
    IF NOT Eofchnl(inchnl) THEN
	REPEAT                          (* skip leading white space. *)
	    Rdchnl(inchnl,ch)
	UNTIL (NOT Whitesp(ch)) OR Eofchnl(inchnl);
    IF Eofchnl(inchnl) THEN
	token := Mkitem(chartag, eofcode + choffset)
	(* should really return !$eof!$  *)

    ELSE
	BEGIN
	token := xnil;        (* init to something *)

	IF Digit(ch) THEN
	    Set_tag(token, inttag)
	ELSE IF Escalpha(ch) THEN
	    Set_tag(token, idtag)
	ELSE
	    Set_tag(token, chartag);

	CASE Tag_of(token) OF
	    chartag:  BEGIN
		Set_tag(token, idtag);
		idspace[toktype].val := Mkitem(inttag, chartype);
		Set_info(token, Ord(ch) + choffset);
		END;
	    inttag:   BEGIN
		idspace[toktype].val := Mkitem(inttag, inttype);
		anint := Ord(ch) - Ord('0');
		WHILE Digit(ichrbuf[inchnl]) DO
		    BEGIN
		    Rdchnl(inchnl,ch);
		    anint := 10 * anint + (Ord(ch) - Ord('0'))
		    END;
		Set_info(token, anint)
		END;

	    idtag:    BEGIN
		idspace[toktype].val := Mkitem(inttag, idtype);
		i := freestr; (* point to possible new string *)
		moreid := true;
		WHILE (i < maxstrsp) AND moreid DO
		    BEGIN
		    strspace[i] := ch;
		    i := i + 1;
		    moreid := Alphanum(ichrbuf[inchnl]);
		    IF moreid THEN
			Rdchnl(inchnl,ch);
		    END;
		strspace[i] := Chr(eos);   (* terminate string *)
		IF (i >= maxstrsp) THEN
		    Writeln('*****STRING SPACE EXHAUSTED')
		ELSE  (* look the name up, return item for it *)
		    BEGIN
		    Putnm(freestr, token, found);
		    IF NOT found THEN
			freestr := i + 1;
		    END;
		END;
		(* of case idtag *)
	END;
	(* of case *)
	END;
    Rdtok := token
    END;
    (* rdtok *)

    (********************************************************)
    (*                                                      *)
    (*                    initialization                    *)
    (*                                                      *)
    (********************************************************)

FUNCTION Read: any;    FORWARD;

PROCEDURE Init;
    (* initialization procedure depends on  *)
    (* ability to load stack with constants *)
    (* from a file.                         *)
    VAR
	strptr: stringp;
	nam: PACKED ARRAY[1..3] OF onechar;
	(* holds 'nil', other strings? *)
	i, n: integer;
	idref: any;
	found: boolean;

	(* init is divided into two parts so it can compile on terak *)
    PROCEDURE Init1;
	BEGIN
	(* initialize top of stack *)
	st := 0;

	freefloat := 1;

	(* define nil - the id, nil, is defined a little later. *)
	freeident := 1;
	xnil := Mkitem(idtag, freeident);

	(* initialize pair space. *)
	FOR i := 1 TO maxpair - 1 DO      (* initialize free list. *)
	    BEGIN
	    prspace[i].markflg := false;        (* redundant? *)
	    prspace[i].prcar := xnil;         (* just for fun *)
	    prspace[i].prcdr := Mkitem(pairtag, i + 1)
	    END;
	prspace[maxpair].prcar := xnil;
	prspace[maxpair].prcdr := xnil;       (* end flag *)
	freepair := 1;                  (* point to first free pair *)


	(* initialize identifier space and string space. *)
	freestr := 1;
	FOR i := 0 TO hidmax - 1 DO
	    idhead[i] := nillnk;
	FOR i := 1 TO maxident DO
	    BEGIN
	    IF i < maxident THEN
		idspace[i].idhlink := i + 1
	    ELSE    (* nil to mark the final identifier in the table. *)
		idspace[i].idhlink := nillnk;
	    (* set function cells to undefined *)
	    idspace[i].funcell := Mkitem(errtag, undefined)
	    END;

	(* nil must be the first identifier in the table--id #1 *)
	(* must fill in fields by hand for nil.*)
	(* putnm can handle any later additions.  *)
	nam := 'NIL';
	strptr := freestr;
	FOR i := 1 TO 3 DO
	    BEGIN
	    strspace[strptr] := nam[i];
	    strptr:= strptr + 1;
	    END;
	strspace[strptr] := Chr(eos);
	Putnm(freestr, xnil, found);
	IF NOT found THEN
	    freestr := strptr + 1;

	(* make the single character ascii identifiers, except nul(=eos). *)
	FOR i := 1 TO 127  DO
	    BEGIN
	    strspace[freestr] := Chr(i);
	    strspace[freestr + 1] := Chr(eos);
	    Putnm(freestr, idref, found);
	    IF NOT found THEN
		freestr := freestr + 2;
	    IF i = Ord('T') THEN
		t := idref;
	    (* returns location for 't. *)
	    END;

	(* init fixnum free list. *)
	FOR i := 1 TO maxintsp - 1 DO
	    intspace[i] := i + 1;
	intspace[maxintsp] := end_flag;
	freeint := 1;


	(* clear the counters *)
	gccount := 0;
	consknt := 0;
	pairknt := 0;
	END     (* init1 *);


    PROCEDURE Init2;
	VAR token: any;

	BEGIN
	(* load "symbol table" with identifiers, constants, and functions.  *)
	inchnl := 1;        (* select symbol input file. *)
	(* reset(symin,'#5:poly.data'); *) (* for terak *)


	token := Rdtok;     (* get count of identifiers. *)
	IF Tag_of(token) <> inttag THEN
	    Writeln('*****BAD SYMBOL TABLE, INTEGER EXPECTED AT START');
	n := Info_of(token);
	FOR i := 1 TO n DO
	    token := Rdtok;
	(* reading token magically loads it into id space. *)
	token := Rdtok;         (* look for zero terminator. *)
	IF (Tag_of(token) <> inttag) OR (Info_of(token) <> 0) THEN
	    Writeln('*****BAD SYMBOL TABLE, ZERO EXPECTED AFTER IDENTIFIERS');

	token := Rdtok;         (* count of constants  *)
	IF Tag_of(token) <> inttag THEN
	    Writeln('*****BAD SYMBOL TABLE, INTEGER EXPECTED BEFORE CONSTANTS');
	n := Info_of(token);
	Alloc(n);       (* space for constants on the stack *)
	FOR i := 1 TO n DO
	    stk[i] := Read;
	token := Rdtok;
	IF (Tag_of(token) <> inttag) OR (Info_of(token) <> 0) THEN
	    Writeln('*****BAD SYMBOL TABLE, ZERO EXPECTED AFTER CONSTANTS');


	token := Rdtok;     (* count of functions. *)
	IF Tag_of(token) <> inttag THEN
	    Writeln('*****BAD SYMBOL TABLE, INTEGER EXPECTED BEFORE FUNCTIONS');
	n := Info_of(token);
	FOR i := 1 TO n DO
	    (* for each function *)
	    (* store associated code *)
	    idspace[Rdtok].funcell := Mkitem(codetag, i);
	token := Rdtok;
	IF (Tag_of(token) <> inttag) OR (Info_of(token) <> 0) THEN
	    Writeln('*****BAD SYMBOL TABLE, ZERO EXPECTED AFTER FUNCTIONS');

	inchnl := 2;        (* select standard input. *)
	END     (* init2 *);

    BEGIN       (* init *)
    Init1;
    Init2;
    END    (* init *);


    (********************************************************)
    (*                                                      *)
    (*                        apply                         *)
    (*                                                      *)
    (********************************************************)

FUNCTION Apply(fn, arglist: any): any;
    VAR arg1, arg2, arg3, arg4, arg5: any;
	numargs: integer;

    BEGIN (* apply *)
    IF Tag_of(fn) <> codetag THEN
	Writeln('*****APPLY: UNDEFINED FUNCTION.')
    ELSE
	BEGIN   (* spread the arguments *)
	numargs := 0;
	WHILE Truep(Pairp(arglist)) DO
	    BEGIN
	    numargs := numargs + 1;
	    CASE numargs OF
		1: arg1 := Car(arglist);
		2: arg2 := Car(arglist);
		3: arg3 := Car(arglist);
		4: arg4 := Car(arglist);
		5: arg5 := Car(arglist);
		6: Writeln('APPLY: TOO MANY ARGS SUPPLIED.')
		END (* case *);
	    arglist := Cdr(arglist)
	    END (* while *)
	END (* if *);

    CASE Info_of(fn) OF
	1: Apply := Atom(arg1);
	END (* case *)
    END (* apply *);
    (*??* Missing closing point at end of program. *??*)
(*??* Missing closing point at end of program. *??*)
