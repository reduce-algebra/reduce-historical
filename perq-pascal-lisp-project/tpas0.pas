(* include following two lines for terak *)
(*  [$s+] *) (* swapping mode to manage this large file *)
(*  [$g+] *) (* goto is legal *)

PROGRAM pas0(symin*,input*,output);

    (************************************************************)
    (* support routines for a "lisp" machine.  uses a register  *)
    (* model with a stack for holding frames.  stack also used  *)
    (* to hold compiler generated constants.                    *)
    (* written by william f. galway and martin l. griss         *)
    (* modified by ralph ottenheimer may 81                     *)
    (* append pas1...pasn at  end                               *)
    (* -------------------------------------------------------- *)
    (* symin is input channel one--used to initialize "symbol   *)
    (* table".  input is input channel two--standard input.     *)
    (* output is output channel one--the standard output.       *)
    (************************************************************)

CONST
    (* for terak  *)
     sp = ' ';  
     ht = 9;           (* ascii codes *)
     lf = 10; 
     cr = 13; 
     nul = 0; 

    eos = nul;      (* terminator character for strings. *)
    (* note: use chr(eos) on terak *)
    inchns = 2;       (* number of input channels.  *)
    outchns = 1;      (* number of output channels. *)

    xtoktype  =  129; (* slot in idspace for toktype. *)
    chartype  =  3;   (* various token types *)
    inttype  =  1;
    idtype  =  2;

    shift_const = 8192; (* tags and info are packed into an integer *)
    (* assumed to be at least 16 bits long.  low order 13 bits  *)
    (* are the info, top 3 are the tag.                         *)
    int_offset = 4096;  (* small integers are stored 0..8191    *)
    (* instead of -4096..4095 because it will pack smaller      *)
    (* under ucsd pascal.                                       *)

    (* the various tags - can't use a defined scalar type *)
    (* because of the lack of convertion functions.       *)
    inttag = 0;    (* info is an integer                  *)
    chartag = 1;   (* info is a character code            *)
    pairtag = 2;   (* info points to pair                 *)
    idtag = 3;     (* info points to identifier           *)
    codetag = 4;   (* info is index into a case statement *)
    (*                that calls appropriate function.    *)
    errtag = 5;    (* info is an error code - see below.  *)
    bigtag = 6;    (* info points to a full word (or      *)
    (*                longer) integer.                    *)
    flotag = 7;    (* info points to a float number.      *)

    (* error codes.  corresponding to tag = errtag.  *)
    noprspace = 1;    (* no more "pair space"--can't cons. *)
    notpair = 2;      (* a pair operation attempted on a non-pair. *)
    noidspace = 3;    (* no more free identifiers *)
    undefined = 4;    (* used to mark undefined function cells (etc?) *)

    maxpair = 2500;   (* max number of pairs allowed. *)
    maxident = 400;   (* max number of identifiers *)
    maxstrsp = 2000;  (* size of string (literal) storage space. *)
    maxintsp = 50;      (* max number of long integers allowed *)
    maxflosp = 50;      (* max number of floating numbers allowed *)

    hidmax = 50;      (* number of hash values for identifiers *)
    maxgcstk = 100;   (* size of garbage collection stack.    *)
    stksize = 500;    (* stack size *)
    maxreg = 15;      (* number of registers in lisp machine. *)

    eofcode = 26;     (* magic character code for eof, ascii for *)
    (*  cntrl-z.  kludge, see note in xrdtok.  *)
    choffset = 1;     (* add choffset to ascii code to get address  *)
    (* in id space for corresponding identifier.  *)
    nillnk = 0;       (* when integers are used as pointers.  *)


TYPE
    (* onechar = ascii; *)
    onechar = char;     (* for terak *)

    (* note we allow zero for id_ptr, allowing a "nil" link. *)
    stringp = 1..maxstrsp;        (* pointer into string space. *)
    id_ptr = 0..maxident;            (* pointer into id space. *)

    itemref = integer;
    itemtype = 0..7;    (* the tags *)


    pair = PACKED RECORD
		      prcar: itemref;
		      prcdr: itemref;
		      markflg: boolean;        (* for garbage collection   *)
		  END;


    ascfile = PACKED FILE OF onechar;

    ident = PACKED RECORD           (* identifier *)
		       idname: stringp;
		       val: itemref;       (* value *)
		       plist: itemref;     (* property list *)
		       funcell: itemref;   (* function cell *)
		       idhlink: id_ptr;   (* hash link *)
		   END;
    longint = integer;    (* use long int on terak *)

VAR
    (* global information *)
    nilref,trueref: itemref;    (* refers to identifiers "nil", and "t". *)

    r: ARRAY[1..maxreg] OF itemref;
    rxx,ryy: itemref;

    (* "st" is the stack pointer into "stk".  it counts the number of  *)
    (* items on the stack, so it runs from zero while the stack starts *)
    (* at one.                                                         *)
    st: 0..stksize;
    stk: ARRAY[1..stksize] OF itemref;

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

    (* i/o channels *)
    symin: ascfile;
(*    input: ascfile; (* comment out for terak. *)

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


FUNCTION tag_of(item: itemref): itemtype;
    BEGIN (* tag_of *)
    tag_of := item DIV shift_const;
    END;
    (* tag_of *)

FUNCTION info_of(item: itemref): integer;
    BEGIN (* info_of *)
    IF item DIV shift_const = inttag THEN
	info_of := item MOD shift_const - int_offset
    ELSE
	info_of := item MOD shift_const
    END;
    (* info_of *)

PROCEDURE mkitem(tag: itemtype; info: longint; VAR item: itemref);
    (* do range checking on info. ints run from -4096 to +4095 *)
    (* everything else runs from 0 to 8191. ints & chars       *)
    (* contain their info, all others points into an           *)
    (* appropriate space.                                      *)

    PROCEDURE mkbigint;
	BEGIN (* mkbigint *)
	IF freeint <= maxintsp THEN     (* convert to bignum *)
	    BEGIN
	    tag := bigtag;
	    intspace[freeint] := info;
	    info := freeint;        (* since we want the pointer *)
	    freeint := freeint + 1;
	    END
	ELSE writeln('*****BIGNUM SPACE EXHAUSTED')     (* should do gc *)
	END;
	(* mkbigint *)


    BEGIN (* mkitem *)
    IF tag = inttag THEN
	IF (info < -int_offset) OR (info > int_offset - 1) THEN mkbigint
	ELSE info := info + int_offset    (* info was in range so add offset *)

    ELSE IF tag = bigtag THEN mkbigint

	 ELSE IF info < 0 THEN
		  BEGIN
		  writeln('*****MKITEM: BAD NEG');
		  break(output); halt
		  END;
    (* nothing special to do for other types *)

    (* pack tag and info into 16-bit item.   *)
    item := tag * shift_const + info
    END;
    (* mkitem *)

PROCEDURE set_info(VAR item: itemref; newinfo: longint);
    BEGIN (* set_info *)
    mkitem(tag_of(item), newinfo, item)
    END;
    (* set_info *)

PROCEDURE set_tag(VAR item: itemref; newtag: itemtype);
    BEGIN (* set_tag *)
    mkitem(newtag, info_of(item), item)
    END;
    (* set_tag *)

PROCEDURE mkident(id: integer; reg: integer);
    (* make identifier "id" in register "reg" *)
    BEGIN       (* mkident *)
    mkitem(idtag, id, r[reg]);
    END;
    (* mkident *)

PROCEDURE mkint(int: longint; reg: integer);
    BEGIN       (* mkint *)
    mkitem(inttag, int, r[reg]);
    END;
    (* mkint *)

PROCEDURE mkpair(pr: integer; reg: integer);
    BEGIN (* mkpair *)
    mkitem(pairtag, pr, r[reg])
    END;
    (* mkpair *)

PROCEDURE int_val(item: itemref; VAR number: longint);
    (* returns integer value of item (int or bignum). *)
    (* must return 'number' in var parameter instead  *)
    (* of function value since long integers are not  *)
    (* a legal function type in ucsd pascal.          *)
    BEGIN (* int_val *)
    IF tag_of(item) = inttag THEN
	number := info_of(item)
    ELSE IF tag_of(item) = bigtag THEN
	     number := intspace[info_of(item)]
	 ELSE writeln('***** ILLEGAL DATA TYPE FOR NUMERIC OPERATION')
    END;
    (* int_val *)


    (********************************************************)
    (*                                                      *)
    (*                  stack allocation                    *)
    (*                                                      *)
    (********************************************************)

PROCEDURE xsetuniq;     (* just here temporarily until i can *)
    BEGIN (* xsetuniq *)(* figure out how to get them out of *)
    END;
    (* execute.                          *)
    (* xsetuniq *)

PROCEDURE xgetuniq;
    BEGIN (* xgetuniq *)
    END;
    (* xgetuniq *)


PROCEDURE alloc(n: integer);
    BEGIN
    IF n + st <= stksize THEN
	st := n+st
    ELSE
	BEGIN
	writeln('*****LISP STACK OVERFLOW');
	writeln('     TRIED TO ALLOCATE ',n);
	writeln('     CURRENT STACK TOP IS ',st);
	END;
    END;

PROCEDURE dealloc(n: integer);
    BEGIN
    IF st - n >= 0 THEN
	st := st - n
    ELSE
	writeln('*****LISP STACK UNDERFLOW');
    END;

    (* optimized allocs *)

PROCEDURE alloc1;
    BEGIN alloc(1) END;

PROCEDURE dealloc1;
    BEGIN dealloc(1) END;

PROCEDURE alloc2;
    BEGIN alloc(2) END;

PROCEDURE dealloc2;
    BEGIN dealloc(2) END;

PROCEDURE alloc3;
    BEGIN alloc(3) END;

PROCEDURE dealloc3;
    BEGIN dealloc(3) END;


    (********************************************************)
    (*                                                      *)
    (*              support for register model              *)
    (*                                                      *)
    (********************************************************)

PROCEDURE load(reg: integer; sloc: integer);
    BEGIN
    IF sloc < 0 THEN r[reg] := r[-sloc]
    ELSE  r[reg] := stk[st-sloc];
    (* will, fix for load (pos,pos) *)
    END;

PROCEDURE store(reg: integer; sloc: integer);
    BEGIN
    stk[st-sloc] := r[reg];
    END;

    (* optimized load/store. *)
PROCEDURE load10;
    BEGIN
    load(1,0);
    END;

PROCEDURE store10;
    BEGIN
    store(1,0);
    END;

PROCEDURE storenil(sloc: integer);
    BEGIN
    stk[st-sloc] := nilref;
    END;


    (********************************************************)
    (*                                                      *)
    (*               standard lisp functions                *)
    (*                                                      *)
    (********************************************************)

    (* the following standard lisp functions appear in *)
    (* eval.red: reverse, append, memq, atsoc, get,    *)
    (* put, remprop, eq, null, equal, error, errorset, *)
    (* abs, idp, numberp, atom, minusp, eval, xapply,  *)
    (* evlis, prin1, print, prin2t, list2 ... list5.   *)

FUNCTION atom(item : itemref): itemref;
    BEGIN       (* atom *)
    IF tag_of(item) <> pairtag THEN atom := trueref
    ELSE atom := nilref
    END (* atom *);

FUNCTION codep(item: itemref): itemref;
    BEGIN       (* codep *)
    IF (tag_of(item) = codetag) AND (info_of(item) <> undefined) THEN
	codep := trueref
    ELSE codep := nilref
    END (* codep *);

FUNCTION idp(item: itemref): itemref;
    BEGIN       (* idp *)
    IF tag_of(item) = idtag THEN idp := trueref
    ELSE idp := nilref
    END (* idp *);

FUNCTION pairp(item: itemref): itemref;
    BEGIN (* pairp *)
    IF tag_of(item) = pairtag THEN pairp := trueref
    ELSE pairp := nilref
    END (* pairp *);

FUNCTION constantp(item: itemref): itemref;
    BEGIN       (* constantp *)
    IF NOT((pairp(item) = trueref) OR (idp(item) = trueref)) THEN
	constantp := trueref
    ELSE constantp := nilref
    END (* constantp *);

FUNCTION eq(u, v: itemref): itemref;
    BEGIN       (* eq *)
    IF u = v THEN eq := trueref
    ELSE eq := nilref
    END (* eq *);

FUNCTION eqn(u, v: itemref): itemref;
    VAR i, j: longint;

    BEGIN       (* eqn *)
    int_val(u, i);
    int_val(v, j);
    IF i = j THEN eqn := trueref
    ELSE eqn := nilref
    END (* eqn *);

FUNCTION fixp(item: itemref): itemref;
    BEGIN       (* fixp *)
    IF (tag_of(item) = inttag) OR (tag_of(item) = bigtag) THEN
	fixp := trueref
    ELSE fixp := nilref
    END (* fixp *);

FUNCTION floatp(item: itemref): itemref;
    BEGIN       (* floatp *)
    IF tag_of(item) = flotag THEN floatp := trueref
    ELSE floatp := nilref
    END (* floatp *);

FUNCTION numberp(item: itemref): itemref;
    BEGIN       (* numberp *)
    numberp := fixp(item)       (* will have to be fixed for floats *)
    END (* numberp *);


    (********************************************************)
    (*                                                      *)
    (*              identifier lookup & entry               *)
    (*                                                      *)
    (********************************************************)

FUNCTION nmhash(nm: stringp): integer;
    CONST
	hashc = 256;
    VAR
	i,tmp: integer;
    BEGIN
    tmp := 0;
    i := 1;     (* get hash code from first three chars of string. *)
    WHILE (i <= 3) AND (strspace[nm+i] <> chr(eos)) DO
	BEGIN
	tmp := ord(strspace[nm+i]) + hashc*tmp;
	i := i + 1;
	END;
    nmhash := abs(tmp) MOD hidmax;      (* abs because mod is screwy. *)
    END;

FUNCTION eqstr(s1,s2: stringp): boolean;
    BEGIN
    WHILE (strspace[s1] = strspace[s2]) AND (strspace[s1] <> chr(eos)) DO
	BEGIN
	s1 := s1 + 1;
	s2 := s2 + 1;
	END;
    eqstr := (strspace[s1] = strspace[s2]);
    END;

PROCEDURE nmlookup(nm: stringp; VAR found: boolean; VAR hash: integer;
		   VAR loc: itemref);
    (* lookup a name in "identifier space".                                 *)
    (* "hash" returns the hash value for the name.                          *)
    (* "loc" returns the location in the space for the (possibly new)       *)
    (* identifier.                                                          *)
    BEGIN
    hash := nmhash(nm);
    mkitem(idtag, idhead[hash], loc);
    (* default is identifier, but may be "error". *)
    (* start at appropriate hash chain. *)

    found := false;
    WHILE (info_of(loc) <> nillnk) AND (NOT found) DO
	BEGIN
	found := eqstr(nm, idspace[info_of(loc)].idname);
	IF NOT found THEN
	    set_info(loc, idspace[info_of(loc)].idhlink);
	(* next id in chain *)
	END;
    IF NOT found THEN               (* find spot for new identifier *)
	BEGIN
	IF freeident=nillnk THEN    (* no more free identifiers. *)
	    mkitem(errtag, noidspace, loc)
	ELSE
	    BEGIN
	    set_info(loc, freeident);
	    freeident := idspace[freeident].idhlink;
	    END;
	END;
    END;

PROCEDURE putnm(nm: stringp; VAR z: itemref; VAR found: boolean);
    (* put a new name into identifier space, or return old location *)
    (* if it's already there.                                       *)
    VAR
	tmp: ident;
	hash: integer;
    BEGIN
    nmlookup(nm, found, hash, z);
    IF (NOT found) AND (tag_of(z) = idtag) THEN
	BEGIN
	tmp.idname := nm;
	tmp.idhlink := idhead[hash];   (* put new ident at head of chain     *)
	tmp.val := nilref;             (* initialize value and property list *)
	tmp.plist := nilref;
	tmp.funcell := nilref;         (* also, the function cell *)
	idhead[hash] := info_of(z);
	idspace[info_of(z)] := tmp;
	END;
    END;

PROCEDURE xfaststat;
    (* give quick summary of statistics gathered *)
    BEGIN
    writeln('CONSES:',consknt);
    writeln('PAIRS :',pairknt);
    writeln('CONSES/PAIRS: ',consknt/pairknt);
    writeln('ST    :',st);
    END;


    (********************************************************)
    (*                                                      *)
    (*              the garbage collector                   *)
    (*                                                      *)
    (********************************************************)

PROCEDURE xgcollect;
    VAR
	i: integer;
	markedk: integer;   (* counts the number of pairs marked *)
	freedk: integer;    (* counts the number of pairs freed. *)
	gcstkp: 0..maxgcstk; (* note the garbage collection stack   *)
	mxgcstk: 0..maxgcstk;           (* is local to this procedure. *)
	gcstk: ARRAY[1..maxgcstk] OF integer;

    PROCEDURE pushref(pr: itemref);
	(* push the address of an unmarked pair, if that's what it is. *)
	BEGIN
	IF tag_of(pr) = pairtag THEN
	    IF NOT prspace[info_of(pr)].markflg THEN
		BEGIN
		IF gcstkp < maxgcstk THEN
		    BEGIN
		    gcstkp := gcstkp + 1;
		    gcstk[gcstkp] := info_of(pr);
		    IF gcstkp > mxgcstk THEN
			mxgcstk := gcstkp;
		    END
		ELSE
		    BEGIN
		    writeln('*****GARBAGE STACK OVERFLOW');
		    halt;       (* fatal error *)
		    END;
		END;
	END;

    PROCEDURE mark;
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
	    pushref(prspace[prloc].prcdr);
	    pushref(prspace[prloc].prcar);  (* trace the car first. *)
	    END;
	END;

    BEGIN       (* xgcollect *)
    writeln('***GARBAGE COLLECTOR CALLED');
    gccount := gccount + 1;          (* count garbage collections. *)
    xfaststat;   (* give summary of statistics collected *)
    consknt := 0;       (* clear out the cons/pair counters *)
    pairknt := 0;
    gcstkp := 0;                    (* initialize the garbage stack pointer. *)
    mxgcstk := 0;                   (* keeps track of max stack depth. *)

    (* mark things from the "computation" stack. *)
    FOR i := 1 TO st DO
	BEGIN
	pushref(stk[i]);
	mark;
	END;
    (* mark things from identifier space. *)
    FOR i := 1 TO maxident DO
	BEGIN
	pushref(idspace[i].val);
	mark;
	pushref(idspace[i].plist);
	mark;
	pushref(idspace[i].funcell);
	mark;
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
	    prspace[i].prcar := nilref;
	    mkitem(pairtag, freepair, prspace[i].prcdr);
	    freepair := i;
	    freedk := freedk + 1
	    END
	END;
    writeln(freedk,' PAIRS FREED.');
    writeln(markedk,' PAIRS IN USE.');
    writeln('MAX GC STACK WAS ',mxgcstk);
    END;
    (* xgcollect *)

    (********************************************************)
    (*                                                      *)
    (*                  lisp primitives                     *)
    (*                                                      *)
    (********************************************************)

    (* return r[1].r[2] in r[1] *)
PROCEDURE xcons;
    VAR p: integer;

    BEGIN
    (* push args onto stack, in case we need to garbage collect the *)
    (* references will be detected.                                 *)
    alloc(2);
    stk[st] := r[1];
    stk[st-1] := r[2];

    IF prspace[freepair].prcdr = nilref THEN xgcollect;

    p := freepair;
    freepair := info_of(prspace[p].prcdr);
    prspace[p].prcar := stk[st];
    prspace[p].prcdr := stk[st - 1];
    mkpair(p, 1);       (* leave r[1] pointing at new pair. *)

    pairknt := pairknt + 1;
    consknt := consknt + 1;
    dealloc(2);
    END;

PROCEDURE xncons;
    BEGIN r[2] := nilref;
    xcons;
    END;

PROCEDURE xxcons;
    BEGIN rxx := r[1];
    r[1] := r[2];
    r[2] := rxx;
    xcons;
    END;

    (* return car of r[1] in r[1] *)
PROCEDURE xcar;
    BEGIN
    IF tag_of(r[1]) = pairtag THEN
	r[1] := prspace[info_of(r[1])].prcar
    ELSE
	mkitem(errtag, notpair, r[1]);
    END;

PROCEDURE xcdr;
    BEGIN
    IF tag_of(r[1]) = pairtag THEN
	r[1] := prspace[info_of(r[1])].prcdr
    ELSE
	mkitem(errtag, notpair, r[1]);
    END;

    (* anyreg car and cdr *)
PROCEDURE anycar(VAR a, b: itemref);
    BEGIN
    IF tag_of(a) = pairtag THEN
	b := prspace[info_of(a)].prcar
    ELSE
	mkitem(errtag, notpair, b);
    END;

PROCEDURE anycdr(VAR a, b: itemref);
    BEGIN
    IF tag_of(a) = pairtag THEN
	b := prspace[info_of(a)].prcdr
    ELSE
	mkitem(errtag, notpair, b);
    END;

    (********************************************************)
    (*                                                      *)
    (*                    i/o primitives                    *)
    (*                                                      *)
    (********************************************************)


PROCEDURE xterpri;
    (* need to change for multiple output channels.  *)
    (* improve choice of break/nobreak.              *)
    BEGIN
    writeln(output);
    END;


PROCEDURE xwrtok;
    (* doesn't expand escaped characters in identifier names *)
    VAR
	i: integer;
    BEGIN
    IF tag_of(r[1]) = inttag THEN
	BEGIN
	IF info_of(r[1]) = 0 THEN
	    write('0')
	ELSE
	    write(info_of(r[1]): 2+trunc(log(abs(info_of(r[1])))));
	END

    ELSE IF tag_of(r[1]) = bigtag THEN
	     write(intspace[info_of(r[1])])

	 ELSE IF tag_of(r[1]) = flotag THEN
		  write(flospace[info_of(r[1])])

	      ELSE IF tag_of(r[1]) = idtag THEN
		       BEGIN
		       i := idspace[info_of(r[1])].idname;
		       WHILE (i <= maxstrsp) AND (strspace[i] <> chr(eos)) DO
			   BEGIN
			   write(strspace[i]);
			   i:= i + 1;
			   END;
		       END

		   ELSE IF tag_of(r[1]) = chartag THEN
			    write(chr(info_of(r[1]) - choffset))

			ELSE
			    writeln('XWRTOK GIVEN ',tag_of(r[1]), info_of(r[1]));
    END;

PROCEDURE rdchnl(chnlnum: integer; VAR ch: onechar);
    BEGIN
    IF (chnlnum < 1) OR (chnlnum > inchns) THEN
	writeln('*****BAD INPUT CHANNEL FOR RDCHNL')
    ELSE
	CASE chnlnum OF
	    1:  BEGIN
	    ch := symin^;  (* a little strange, but avoids  *)
	    get(symin);              (* initialization problems *)
	    ichrbuf[inchnl] := symin^;
	    END;

	    2:  BEGIN
	    ch := input^;
	    get(input);
	    ichrbuf[inchnl] := input^;
	    END;
	    END;
    (* case *)
    END;
    (* rdchnl *)

FUNCTION eofchnl(chnlnum: integer): boolean;
    BEGIN
    IF (chnlnum < 1) OR (chnlnum > inchns) THEN
	writeln('*****BAD INPUT CHANNEL FOR EOFCHNL')
    ELSE
	CASE chnlnum OF
	    1:  eofchnl := eof(symin);
	    2:  eofchnl := eof(input);
	    END;
    END;

    (********************************************************)
    (*                                                      *)
    (*                   token scanner                      *)
    (*                                                      *)
    (********************************************************)

PROCEDURE xrdtok;
    VAR
	ch: onechar;
	i: integer;
	anint: longint;
	moreid: boolean;
	found: boolean;

    FUNCTION digit(ch: onechar): boolean;
	BEGIN
	digit := ( '0' <= ch ) AND ( ch <= '9');
	END;

    FUNCTION escalpha(VAR ch: onechar): boolean;
	(* test for alphabetic or escaped character.                 *)
	(* note possible side effect.                                *)
	BEGIN
	IF ( 'A' <= ch ) AND ( ch <= 'Z') THEN
	    escalpha := true
	ELSE IF ( ord('A')+32 <= ord(ch)) AND ( ord(ch) <= ord('Z')+32) THEN
		 escalpha := true    (* lower case alphabetics *)
	     ELSE IF ch='!' THEN
		      BEGIN
		      rdchnl(inchnl,ch);
		      escalpha := true;
		      END
		  ELSE
		      escalpha := false;
	END;

    FUNCTION alphanum(VAR ch: onechar): boolean;
	(* test if escalfa or digit *)
	VAR b: boolean;
	BEGIN
	b := digit(ch);
	IF NOT b THEN b := escalpha(ch);
	alphanum := b;
	END;

	    function whitesp(ch: onechar): boolean; *)
	    var asccode: integer; *)
	    begin 
	      asccode := ord(ch);       (* ascii character code *)
	        WHITESP := (CH = SP) OR (ASCCODE = CR) OR (ASCCODE = LF)
	 OR (asccode = ht) or (asccode = nul);         (* null?? *)
	    end; 
	(* end of terak version *)

	(* reads bignums...need to read flonums too *)
    BEGIN       (* xrdtok *)
    IF NOT eofchnl(inchnl) THEN
	REPEAT                          (* skip leading white space. *)
	    rdchnl(inchnl,ch)
	UNTIL (NOT whitesp(ch)) OR eofchnl(inchnl);
    IF eofchnl(inchnl) THEN
	mkitem(chartag, eofcode + choffset, r[1])
	(* should really return !$eof!$  *)
    ELSE
	BEGIN
	IF digit(ch) THEN
	    set_tag(r[1], inttag)
	ELSE IF escalpha(ch) THEN
		 set_tag(r[1], idtag)
	     ELSE
		 set_tag(r[1], chartag);

	CASE tag_of(r[1]) OF
	    chartag:  BEGIN
		  set_tag(r[1], idtag);
		  mkitem(inttag, chartype, idspace[xtoktype].val);
		  set_info(r[1], ord(ch) + choffset);
		  END;
	    inttag:   BEGIN
		 mkitem(inttag, inttype, idspace[xtoktype].val);
		 anint := ord(ch) - ord('0');
		 WHILE digit(ichrbuf[inchnl]) DO
		     BEGIN
		     rdchnl(inchnl,ch);
		     anint := 10 * anint + (ord(ch) - ord('0'))
		     END;
		 set_info(r[1], anint)
		 END;
	    idtag:    BEGIN
		mkitem(inttag, idtype, idspace[xtoktype].val);
		i := freestr; (* point to possible new string *)
		moreid := true;
		WHILE (i < maxstrsp) AND moreid DO
		    BEGIN
		    strspace[i] := ch;
		    i:= i + 1;
		    moreid := alphanum(ichrbuf[inchnl]);
		    IF moreid THEN
			rdchnl(inchnl,ch);
		    END;
		strspace[i] := chr(eos);   (* terminate string *)
		IF (i >= maxstrsp) THEN
		    writeln('*****STRING SPACE EXHAUSTED')
		ELSE  (* look the name up, return itemref for it *)
		    BEGIN
		    putnm(freestr, r[1], found);
		    IF NOT found THEN
			freestr := i + 1;
		    END;
		END;
		(* of case idtag *)
	    END;
	(* of case *)
	END;
    END;
    (* xrdtok *)

    (********************************************************)
    (*                                                      *)
    (*                    initialization                    *)
    (*                                                      *)
    (********************************************************)

PROCEDURE xread;
    FORWARD;

PROCEDURE init;
    (* initialization procedure depends on  *)
    (* ability to load stack with constants *)
    (* from a file.                         *)
    VAR
	strptr: stringp;
	nam: PACKED ARRAY[1..3] OF onechar;
	(* holds 'nil', other strings? *)
	i, n: integer;
	idref: itemref;
	found: boolean;

	(* init is divided into two parts so it can compile on terak *)
    PROCEDURE init1;
	BEGIN
	(* initialize top of stack *)
	st := 0;

	freefloat := 1;
	freeint := 1;

	(* define nilref - the id, nil, is defined a little later. *)
	freeident := 1;
	mkitem(idtag, freeident, nilref);

	(* initialize pair space. *)
	FOR i := 1 TO maxpair - 1 DO      (* initialize free list. *)
	    BEGIN
	    prspace[i].markflg := false;        (* redundant? *)
	    prspace[i].prcar := nilref;         (* just for fun *)
	    mkitem(pairtag, i + 1, prspace[i].prcdr);
	    END;
	prspace[maxpair].prcar := nilref;
	prspace[maxpair].prcdr := nilref;       (* end flag *)
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
	    mkitem(errtag, undefined, idspace[i].funcell);
	    END;

	(* nil must be the first identifier in the table--id #1 *)
	(* must fill in fields by hand for nilref.*)
	(* putnm can handle any later additions.  *)
	nam := 'NIL';
	strptr := freestr;
	FOR i := 1 TO 3 DO
	    BEGIN
	    strspace[strptr] := nam[i];
	    strptr:= strptr + 1;
	    END;
	strspace[strptr] := chr(eos);
	putnm(freestr, nilref, found);
	IF NOT found THEN
	    freestr := strptr + 1;

	(* make the single character ascii identifiers, except nul(=eos). *)
	FOR i := 1 TO 127  DO
	    BEGIN
	    strspace[freestr] := chr(i);
	    strspace[freestr + 1] := chr(eos);
	    putnm(freestr, idref, found);
	    IF NOT found THEN
		freestr := freestr + 2;
	    IF i = ord('T') THEN
		trueref := idref;
	    (* returns location for 't. *)
	    END;

	(* clear the counters *)
	gccount := 0;
	consknt := 0;
	pairknt := 0;
	END;
	(* init1 *)

    PROCEDURE init2;
	BEGIN
	(* load "symbol table" with identifiers, constants, and functions.  *)
	inchnl := 1;        (* select symbol input file. *)
	reset(symin,'#5:paslsp.data');  (* for terak *)

	xrdtok;     (* get count of identifiers. *)
	IF tag_of(r[1]) <> inttag THEN
	    writeln('*****BAD SYMBOL TABLE, INTEGER EXPECTED AT START');
	n := info_of(r[1]);
	FOR i := 1 TO n DO
	    xrdtok;
	(* reading token magically loads it into id space. *)
	xrdtok;         (* look for zero terminator. *)
	IF (tag_of(r[1]) <> inttag) OR (info_of(r[1]) <> 0) THEN
	    writeln('*****BAD SYMBOL TABLE, ZERO EXPECTED AFTER IDENTIFIERS');

	xrdtok;         (* count of constants  *)
	IF tag_of(r[1]) <> inttag THEN
	    writeln('*****BAD SYMBOL TABLE, INTEGER EXPECTED BEFORE CONSTANTS');
	n := info_of(r[1]);
	alloc(n);       (* space for constants on the stack *)
	FOR i := 1 TO n DO
	    BEGIN
	    xread;
	    stk[i] := r[1];
	    END;
	xrdtok;
	IF (tag_of(r[1]) <> inttag) OR (info_of(r[1]) <> 0) THEN
	    writeln('*****BAD SYMBOL TABLE, ZERO EXPECTED AFTER CONSTANTS');

	xrdtok;     (* count of functions. *)
	IF tag_of(r[1]) <> inttag THEN
	    writeln('*****BAD SYMBOL TABLE, INTEGER EXPECTED BEFORE FUNCTIONS');
	n := info_of(r[1]);
	FOR i := 1 TO n DO
	    (* for each function *)
	    (* store associated code *)
	    BEGIN
	    xrdtok;
	    mkitem(codetag, i, idspace[info_of(r[1])].funcell);
	    END;
	xrdtok;
	IF (tag_of(r[1]) <> inttag) OR (info_of(r[1]) <> 0) THEN
	    writeln('*****BAD SYMBOL TABLE, ZERO EXPECTED AFTER FUNCTIONS');

	inchnl := 2;        (* select standard input. *)
	END;
	(* init2 *)
    BEGIN       (* init *)
    init1;
    init2;
    END;
    (* init *)

    (********************************************************)
    (*                                                      *)
    (*                 arithmetic functions                 *)
    (*                                                      *)
    (********************************************************)

PROCEDURE xadd1;
    VAR i: longint;

    BEGIN
    int_val(r[1], i);
    mkint(i + 1, 1)
    END;

PROCEDURE xdifference;
    VAR i1, i2: longint;

    BEGIN
    int_val(r[1], i1);
    int_val(r[2], i2);
    mkint(i1 - i2, 1)
    END;

PROCEDURE xdivide;      (* returns dotted pair (quotient . remainder). *)
    VAR quot, rem: integer;
	i1, i2: longint;

    BEGIN
    int_val(r[1], i1);
    int_val(r[2], i2);

    mkint(i1 DIV i2, 1);
    mkint(i1 MOD i2, 2);
    xcons
    END;

PROCEDURE xgreaterp;
    VAR i1, i2: longint;

    BEGIN
    int_val(r[1], i1);
    int_val(r[2], i2);

    IF i1 > i2 THEN
	r[1] := trueref
    ELSE
	r[1] := nilref;
    END;

PROCEDURE xlessp;
    VAR i1, i2: longint;

    BEGIN
    int_val(r[1], i1);
    int_val(r[2], i2);

    IF i1 < i2 THEN
	r[1] := trueref
    ELSE
	r[1] := nilref;
    END;

PROCEDURE xminus;
    VAR i: longint;

    BEGIN
    int_val(r[1], i);
    mkint(-i, 1)
    END;

PROCEDURE xplus2;
    VAR i1, i2: longint;

    BEGIN
    int_val(r[1], i1);
    int_val(r[2], i2);
    mkint(i1 + i2, 1)
    END;

PROCEDURE xquotient;
    VAR i1, i2: longint;

    BEGIN
    int_val(r[1], i1);
    int_val(r[2], i2);
    mkint(i1 DIV i2, 1)
    END;

PROCEDURE xremainder;
    VAR i1, i2: longint;

    BEGIN
    int_val(r[1], i1);
    int_val(r[2], i2);
    mkint(i1 MOD i2, 1)
    END;

PROCEDURE xtimes2;
    VAR i1, i2: longint;

    BEGIN
    int_val(r[1], i1);
    int_val(r[2], i2);
    mkint(i1 * i2, 1)
    END;
    (* xtimes2 *)


    (********************************************************)
    (*                                                      *)
    (*                    support for eval                  *)
    (*                                                      *)
    (********************************************************)


PROCEDURE execute(code: integer);
    FORWARD;

    (* apply(fn,arglist)-- "fn" is an operation code. *)
PROCEDURE xapply;
    VAR
	i: integer;
	code: integer;
	tmp: itemref;
	tmpreg: ARRAY[1..maxreg] OF itemref;
    BEGIN
    code := info_of(r[1]);
    r[1] := r[2];
    i := 1;
    (* spread the arguments  *)
    WHILE (r[1] <> nilref) AND (i <= maxreg) DO
	BEGIN
	tmp := r[1];
	xcar;
	tmpreg[i] := r[1];
	i := i + 1;
	r[1] := tmp;
	xcdr;
	END;
    WHILE i > 1 DO
	BEGIN
	i := i - 1;
	r[i] := tmpreg[i];
	END;
    execute(code);
    END;

    (*  rest of pas1...pasn follow *)

