module tok; % Identifier and reserved character reading.

% Author: Anthony C. Hearn.
% Modifications by: Arthur Norman.

% Copyright (c) 1995 RAND.  All rights reserved.

fluid '(!*adjprec !*defn !*eoldelimp !*lower !*minusliter !*quotenewnam
	semic!*);

% Note *raise is global in following for consistency with the SL Report.

global '(!$eof!$
         !$eol!$
         !*micro!-version
         !*raise
	 !*savecomments!*
         comment!*
         crbuf!*
         crbuf1!*
         crchar!*
         curline!*
         cursym!*
         eof!*
         ifl!*
         nxtsym!*
         outl!*
         ttype!*);

flag('(adjprec),'switch);

!*quotenewnam := t;

crchar!* := '! ;

curline!* := 1;

% The function TOKEN defined below is used for reading identifiers
% and reserved characters (such as parentheses and infix operators).
% It is called by the function SCAN, which translates reserved
% characters into their internal name, and sets up the output of the
% input line.  The following definitions of TOKEN and SCAN are quite
% general, but also inefficient.  The reading process can often be
% speeded up considerably if these functions (especially token) are
% written in terms of the explicit LISP used.

symbolic procedure prin2x u;
  outl!* := u . outl!*;

symbolic procedure mkstrng u;
   %converts the uninterned id U into a string;
   %if strings are not constants, this should be replaced by
   %list('string,u);
   u;

symbolic procedure readch1;
   begin scalar x;
      if null terminalp()
        then progn(x := readch(),
                   x eq !$eol!$ and (curline!* := curline!*+1),
                   return x)
       else if crbuf1!*
        then begin x := car crbuf1!*; crbuf1!* := cdr crbuf1!* end
       else x := readch();
      crbuf!* := x . crbuf!*;
      return x
   end;

symbolic procedure tokquote;
   begin
      crchar!* := readch1();
      nxtsym!* := mkquote rread();
      ttype!* := 4;
      return nxtsym!*
   end;

put('!','tokprop,'tokquote);

symbolic procedure token!-number x;
   % Read and return a valid number from input.
   % Adjusted by A.C. Norman to be less sensitive to input case and to
   % support hex numbers.
   begin scalar dotp,power,sign,y,z;
      power := 0;
      ttype!* := 2;
    num1:
      if y or null(x eq '!)) then y := x . y;
      if dotp then power := power - 1;
    num2:
      if (x := readch1()) eq '!.
         then if dotp
                then rerror('rlisp,3,"Syntax error: improper number")
               else progn(dotp := t, go to num2)
       else if digit x then go to num1
       else if y = '(!0) and (x eq '!x or x eq '!X) then go to hexnum
       else if x eq '!\ then progn(readch(), go to num2)
       else if null(x eq '!e or x eq '!E) then go to ret;
      % Case of number with embedded or trailing E.
      dotp := t;
      if (x := readch1()) eq '!- then sign := t
       else if x eq '!+ then nil
       else if null digit x then go to ret
       else z := list x;
   nume1:
      if null digit(x := readch1()) then go to nume2;
      z := x . z;
      go to nume1;
   hexnum:
      y := 0;
   hexnum1:
      if not (z := get(x := readch1(), 'hexdigit)) then go to ret1;
      y := 16*y + z;
      go to hexnum1;
   nume2:
      if null z then rerror('rlisp,4,"Syntax error: improper number");
      z := compress reversip!* z;
      if sign then power := power - z else power := power + z;
   ret:
      y := compress reversip!* y;
   ret1:
      nxtsym!* := if dotp then '!:dn!: . (y . power)
		   else if !*adjprec then '!:int!: . (y . nil)
		   else y;
      crchar!* := x;
      return nxtsym!*
   end;

deflist(
 '((!0 0)  (!1 1)  (!2 2)  (!3 3)  (!4 4) 
   (!5 5)  (!6 6)  (!7 7)  (!8 8)  (!9 9)
   (!a 10) (!b 11) (!c 12) (!d 13) (!e 14) (!f 15)
   (!A 10) (!B 11) (!C 12) (!D 13) (!E 14) (!F 15)), 'hexdigit);

symbolic procedure token1;
   begin scalar x,y;
        x := crchar!*;
    a:  if seprp x and null(x eq !$eol!$ and !*eoldelimp)
          then progn(x := readch1(), go to a)
         else if digit x then return token!-number x
         else if liter x then go to letter
         else if (y := get(x,'tokprop)) then return lispapply(y,nil)
	 else if x eq '!% and null !*savecomments!* then go to coment
         else if x eq '!! and null(!*micro!-version and null !*defn)
          then go to escape
         else if x eq '!" then go to string;
        ttype!* := 3;
        if x eq !$eof!$ then prog2(crchar!* := '! ,filenderr());
        nxtsym!* := x;
        if delcp x then crchar!*:= '!  else crchar!*:= readch1();
	if null(x eq '!- and digit crchar!* and !*minusliter)
          then go to c;
        x := token!-number crchar!*;
	if numberp x then return apply1('minus,x);  % For bootstrapping.
	rplaca(cdr x,apply1('minus,cadr x));        % Also for booting.
        return x;
    escape:
        begin scalar raise,!*lower;
           raise := !*raise;
           !*raise := nil;
           y := x . y;
           x := readch1();
           !*raise := raise
        end;
    letter:
        ttype!* := 0;
    let1:
        y := x . y;
        if digit (x := readch1()) or liter x then go to let1
         else if x eq '!! then go to escape
	 else if x eq '!- and !*minusliter
          then progn(y := '!! . y, go to let1)
         else if x eq '!_ then go to let1;    % Allow _ as letter.
        nxtsym!* := intern compress reversip!* y;
        crchar!* := x;
    c:  return nxtsym!*;
%   minusl:
%       if digit (x := readch1())
%         then progn(crchar!* := x, return(nxtsym!* := 'minus))
%        else progn(y := '!- . '!! . y, go to letter);
    string:
        begin scalar raise,!*lower;
           raise := !*raise;
           !*raise := nil;
       strinx:
           y := x . y;
           if (x := readch1()) eq !$eof!$
             then progn(!*raise := raise,
			crchar!* := '! ,
			lpriw("***** End-of-file in string",nil),
			filenderr())
            else if null(x eq '!") then go to strinx;
           y := x . y;
           % Now check for embedded string character.
           x := readch1();
           if x eq '!" then go to strinx;
           nxtsym!* := mkstrng compress reversip!* y;
           !*raise := raise
         end;
        ttype!* := 1;
        crchar!* := x;
        go to c;
    coment:
        if null(readch1() eq !$eol!$) then go to coment;
        x := readch1();
        go to a
   end;

symbolic procedure tokbquote;
   begin
     crchar!* := readch1();
      nxtsym!* := list('backquote,rread());
      ttype!* := 3;
      return nxtsym!*
   end;

put('!`,'tokprop,'tokbquote);

symbolic procedure token;
   %This provides a hook for a faster TOKEN;
   token1();

symbolic procedure filenderr;
   begin 
      eof!* := eof!*+1;
      if terminalp() then error1()
       else error(99,if ifl!*
                       then list("End-of-file read in file",car ifl!*)
                      else "End-of-file read")
   end;

symbolic procedure ptoken;
   begin scalar x;
        x := token();
        if x eq '!) and eqcar(outl!*,'! ) then outl!*:= cdr outl!*;
           %an explicit reference to OUTL!* used here;
        prin2x x;
        if null ((x eq '!() or (x eq '!))) then prin2x '! ;
        return x
   end;

symbolic procedure rread1;
   % Modified to use QUOTENEWNAM's for ids.
   % Note that handling of reals uses symbolic mode, regardless of
   % actual mode.
   begin scalar x,y;
        x := ptoken();
        if null (ttype!*=3) 
          then return if idp x
                        then if !*quotenewnam
                                and (y := get(x,'quotenewnam))
                               then y
                              else x
                       else if eqcar(x,'!:dn!:)
                        then dnform(x,nil,'symbolic)
                       else x
         else if x eq '!( then return rrdls()
         else if null (x eq '!+ or x eq '!-) then return x;
        y := ptoken();
        if eqcar(y,'!:dn!:) then y := dnform(y,nil,'symbolic);
        if null numberp y
          then progn(nxtsym!* := " ",
                     symerr("Syntax error: improper number",nil))
         else if x eq '!- then y := apply1('minus,y);
           % We need this construct for bootstrapping purposes.
        return y
   end;

symbolic procedure rrdls;
   begin scalar x,y,z;
    a:  x := rread1();
        if null (ttype!*=3) then go to b
         else if x eq '!) then return z
         else if null (x eq '!.) then go to b;
        x := rread1();
        y := ptoken();
        if null (ttype!*=3) or null (y eq '!))
          then progn(nxtsym!* := " ",symerr("Invalid S-expression",nil))
         else return nconc(z,x);
    b: z := nconc(z,list x);
       go to a
   end;

symbolic procedure rread;
   progn(prin2x " '",rread1());

symbolic procedure delcp u;
   % Returns true if U is a semicolon, dollar sign, or other delimiter.
   % This definition replaces the one in the BOOT file.
   flagp(u,'delchar);

flag('(!; !$),'delchar);

symbolic procedure toknump x;
   numberp x or eqcar(x,'!:dn!:) or eqcar(x,'!:int!:);

% The following version of SCAN provides RLISP with a facility for
% conditional compilation.  The protocol is that text is included or
% excluded at the level of tokens.  Control by use of new reserved
% tokens !#if, !#else and !#endif.  These are used in the form:
%    !#if (some Lisp expression for use as a condition)
%    ... RLISP input ...
%    !#else
%    ... alternative RLISP input ...
%    !#endif
%
% The form
%    !#if C1 ... !#elif C2 ... !#elif C3 ... !#else ... !#endif
% is also supported.
%
% Conditional compilation can be nested.  If the Lisp expression used
% to guard a condition causes an error it is taken to be a FALSE
% condition. It is not necessary to have an !#else before !#endif if no
% alternative text is needed.  Although the examples here put !#if etc
% at the start of lines this is not necessary (though it may count as
% good style?).  Since the condition will be read using RLISPs own
% list-reader there could be conditional compilation guarding parts of
% it - the exploitation of that possibility is to be discouraged!

% Making the condition a raw Lisp expression makes sure that parsing it
% is easy. It makes it possible to express arbitrary conditions, but it
% is hoped that most conditions will not be very elaborate - things like
%    !#if (not (member 'csl lispsystem!*))
%         error();
%    !#else
%         magic();
%    !#endif
% or
%    !#if debugging!-mode  % NB if variable is unset that counts as nil
%    print "message";      % so care should be taken to select the most
%    !#endif               % useful default sense for such tests
% should be about as complicated as reasonable people need.
%
% Two further facilities are provided:
%    !#eval (any lisp expression)
% causes that expression to be evaluated at parse time.  Apart from any
% side-effects in the evaluation the text involved is all ignored. It is
% expected that this will only be needed in rather curious cases, for
% instance to set system-specific options for a compiler.

%    !#define symbol value
% where the value should be another symbol, a string or a number,
% causes the first symbol to be mapped onto the second value wherever
% it occurs in subsequent input.  This uses exactly the same mechanism
% as the existing REDUCE "define" statement and so has the same
% limitations.  The use of a hook in SCAN to support this ensures that
% the !#define can be written anywhere in REDUCE source code (eg within
% a procedure definition) and will still apply while the program
% involved is parsed.  No special facility for undoing the effect of a
% !#define is provided, but the general-purpose !#eval could be used to
% remove the 'newnam property that is involved.

symbolic procedure scan;
   begin scalar bool,x,y;
        if null (cursym!* eq '!*semicol!*) then go to b;
    a:  nxtsym!* := token();
    b:  if null atom nxtsym!* and null toknump nxtsym!*
          then go to q1
         else if nxtsym!* eq 'else or cursym!* eq '!*semicol!*
         then outl!* := nil;
        prin2x nxtsym!*;
    c:  if null idp nxtsym!* then go to l
         else if (x:=get(nxtsym!*,'newnam)) and
                        (null (x=nxtsym!*)) then go to new
         else if nxtsym!* eq 'Comment then go to comm
         else if nxtsym!* eq '!#if then go to conditional
         else if nxtsym!* eq '!#else or
                 nxtsym!* eq '!#elif then progn(nxtsym!* := x := nil,
                                                go to skipping)
         else if nxtsym!* eq '!#endif then go to a
         else if nxtsym!* eq '!#eval then progn(
                     errorset(rread(), !*backtrace, nil),
                     go to a)
	 else if nxtsym!* eq '!#define then progn(
		     x := errorset(rread(), !*backtrace, nil),
		     progn(if errorp x then go to a),
		     y := errorset(rread(), !*backtrace, nil),
		     progn(if errorp y then go to a),
		     put(x, 'newnam, y),
		     go to a)
         else if null(ttype!* = 3) then go to l
         else if nxtsym!* eq !$eof!$ then return filenderr()
         else if nxtsym!* eq '!' then rederr "Invalid QUOTE"
         else if !*eoldelimp and nxtsym!* eq !$eol!$ then go to delim
         else if null (x:= get(nxtsym!*,'switch!*)) then go to l
         else if eqcar(cdr x,'!*semicol!*) then go to delim;
        bool := seprp crchar!*;
   sw1: nxtsym!* := token();
        if null(ttype!* = 3) then go to sw2
         else if nxtsym!* eq !$eof!$ then return filenderr()
         else if car x then go to sw3;
   sw2: cursym!*:=cadr x;
        bool := nil;
        if cursym!* eq '!*rpar!* then go to l2 else return cursym!*;
   sw3: if bool or null (y:= atsoc(nxtsym!*,car x)) then go to sw2;
        prin2x nxtsym!*;
        x := cdr y;
        if null car x and cadr x eq '!*Comment!*
          then progn(comment!* := read!-comment(),go to a);
        go to sw1;
  conditional:
% The conditional expression used here must be written in Lisp form
        x := errorset(rread(), !*backtrace, nil);
% errors in evaluation count as NIL
        if null errorp x and car x then go to a;
        x := nil;
  skipping:
% I support nesting of conditional inclusion.
        if nxtsym!* eq '!#endif then
           if null x then go to a else x := cdr x
        else if nxtsym!* eq '!#if then x := nil . x
        else if (nxtsym!* eq '!#else) and null x then go to a
        else if (nxtsym!* eq '!#elif) and null x then go to conditional;
        nxtsym!* := token();
	if (ttype!*=3) and (nxtsym!* eq !$eof!$)
	  then return filenderr()
	 else go to skipping;
  comm: if delcp crchar!* and null(crchar!* eq !$eol!$)
          then progn(crchar!* := '! , condterpri(), go to a);
        crchar!* := readch();
        go to comm;
  delim:
        semic!*:=nxtsym!*;
        return (cursym!*:='!*semicol!*);
  new:  nxtsym!* := x;
        if stringp x then go to l
        else if atom x then go to c
        else go to l;
  q1:   if null (car nxtsym!* eq 'string) then go to l;
        prin2x " ";
        prin2x cadr(nxtsym!* := mkquote cadr nxtsym!*);
  l:    cursym!*:=nxtsym!*;
        nxtsym!* := token();
        if nxtsym!* eq !$eof!$ and ttype!* = 3 then return filenderr();
  l2:   if numberp nxtsym!*
           or (atom nxtsym!* and null get(nxtsym!*,'switch!*))
          then prin2x " ";
        return cursym!*
   end;

endmodule;

end;
