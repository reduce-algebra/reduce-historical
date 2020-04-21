function caar(x: any): any;
    begin
    caar := car(car(x))
    end;

function cadr(x: any): any;
    begin
    cadr := car(cdr(x))
    end;

function cdar(x: any): any;
    begin
    cdar := car(cdr(x))
    end;

function cddr(x: any): any;
    begin
    cddr := cdr(cdr(x))
    end;

function prin2(x: any): any;
    begin
    end;

function rev(l1: any): any;
    begin
    end;

function notnull(x: any): any;
    begin 
    notnull := x
    end;

function list2(r1, r2: any): any;
    begin
    list2 := cons(r1, ncons(r2))
    end;

function list3(r1, r2, r3: any): any;
    begin
    list3 := cons(r1, list2(r2, r3))
    end;

function list4(r1, r2, r3, r4: any): any;
    begin
    list4 := cons(r1, list3(r2, r3, r4))
    end;

function list5(r1, r2, r3, r4, r5: any): any;
    begin
    list5 := cons(r1, list4(r2, r3, r4, r5))
    end;

function reverse(u: any): any;
    begin
    reverse := rev(u)
    end;

function append(u, v: any): any;
    function append1: any;
        begin
	junk := setq(u, reverse(u));
	while truep(pairp(u)) do
	    begin
	    junk := setq(v, cons(car(u), v));
	    junk := setq(u, cdr(u))	(* a hard case *)
	    end;
	append := v	(* goto also needed? *)
	end;

    begin
    append := append1;
    end;

	(* procedures to support get & put. *)
function memq(u, v: any): any;
    begin
    if truep(xnot(pairp(v))) then memq := v
    else if truep(eq(u, car(v))) then memq := v
    else memq := memq(u, cdr(v))
    end;

function atsoc(u, v: any): any;
    begin
    if truep(xnot(pairp(v))) then atsoc := v
    else if truep(xnot(pairp(v))) or truep(xnot(eq(u, caar(v)))) then
	atsoc := atsoc(u, cdr(v))
    else atsoc := car(v)
    end;

function delq(u, v: any): any;
    begin
    if truep(xnot(pairp(v))) then delq := v
    else if truep(eq(u, car(v))) then delq := cdr(v)
    else delq := cons(car(v), delq(u, cdr(v)))
    end;

function delatq(u, v: any): any;
    begin
    if truep(xnot(pairp(v))) then delatq := v
    else if truep(xnot(pairp(car(v)))) or truep(xnot(eq(u, caar(v)))) then
	delatq := cons(car(v), delatq(u, cdr(v)))
    else delatq := cdr(v)
    end;

function get(u, v:any): any;
    begin
    if truep(xnot(idp(u))) then get := xnil
    else if truep(pairp(setq(u, atsoc(v, plist(u))))) then get := cdr(u)
    else get := xnil
    end;

function put(u, v, ww: any): any;
    function put1: any;
        label 1;
        var l: any;
	begin
	if truep(xnot(idp(u))) then
	    begin
	    put1 := ww;
	    goto 1
	    end;
	junk := setq(l, plist(u));
	if truep(atsoc(v, l)) then junk := delatq(v, l);
	if truep(notnull(ww)) then junk := setq(l, cons(cons(v, ww), l));
	junk := setplist(u, l);
	begin
	put1 := ww;
	goto 1
	end;
	1:
	end;

    begin
    put := put1
    end;

function remprop(u, v: any): any;
    begin
    remprop := put(u, v, xnil)
    end;

function eqcar(u, v: any): any;
    begin
    if truep(pairp(u)) then
	if truep(eq(car(u), v)) then eqcar := t
	else eqcar := xnil
    end;

function null(u: any): any;
    begin
    null := eq(u, xnil)
    end;

function equal(x, y: any): any;
    begin
    if truep(atom(x)) then
	if truep(atom(y)) then
	    equal := eq(x, y)
	else equal := xnil
    else if truep(atom(y)) then equal := xnil
    else if truep(equal(car(x), car(y))) then
	if truep(equal(cdr(x), cdr(y))) then equal := t
	else equal := xnil
    else equal := xnil
    end;

function read;
    begin
    end;

