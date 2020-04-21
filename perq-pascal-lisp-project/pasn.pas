(*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 %
 %               PASCAL BASED MINI-LISP
 %
 % File:         PASN.RED - Trailer File
 % ChangeDate:   12:26pm  Saturday, 18 July 1981
 % By:           M. L. Griss
 %               Change to add Features for Schlumberger Demo
 %               Add Hooks for CATCH/THROW
 %
 %           All RIGHTS RESERVED
 %           COPYRIGHT (C) - 1981 - M. L. GRISS
 %           Computer Science Department
 %           University of Utah
 %
 %           Do Not distribute with out written consent of M. L. Griss
 %
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)


(* pasN.PAS ---- the last file to be appended, close CATCH, do init *)
BEGIN     (*  Body of Catch  *)
IF initphase=0 THEN  (* Kludge to get into scope of CATCH *)
    BEGIN init; initphase := 1; firstp; END
ELSE BEGIN
    idspace[xthrowing].val := nilref;
    catch_stk:=st;                        (* Capture Stack *)
    catch_bstk:=idspace[xbstack].val;     (* Capture Bstack *)
    xeval;
9999:                                 (* Return Point *)
    IF idspace[xthrowing].val <> nilref
    THEN BEGIN
	st:=catch_stk;
	r[2]:=catch_bstk;
	xunbindto; (* return value, old stack *)
	END;
    END
END (* catch *);

BEGIN   (* Top Level *)
    initphase := 0;
    r[1] := 0;
    Xcatch;
    writeln(tty,'halt');break(tty);
end.
