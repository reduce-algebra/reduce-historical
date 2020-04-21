%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                     
% 		PASCAL BASED MINI-LISP
%
% File: 	PAS1.RED - Basic I/O Functions
% ChangeDate: 	10:48pm  Wednesday, 15 July 1981
% By: 		M. L. Griss
%       	Change to add Features for Schlumberger Demo
% 
% 	    All RIGHTS RESERVED
%           COPYRIGHT (C) - 1981 - M. L. GRISS
%           Computer Science Department
%           University of Utah
%
%           Do Not distribute with out written consent of M. L. Griss
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Additional Support procedures for optimized code;

SYMBOLIC PROCEDURE CAAR(X);
 CAR CAR X;

SYMBOLIC PROCEDURE CADR X;
 CAR CDR X;

SYMBOLIC PROCEDURE CDAR X;
 CDR CAR X;

SYMBOLIC PROCEDURE CDDR X;
 CDR CDR X;

% All Friendly CxxxR's

SYMBOLIC PROCEDURE CAAAAR X; CAR CAR CAR CAR X;

SYMBOLIC PROCEDURE CAAADR X; CAR CAR CAR CDR X;

SYMBOLIC PROCEDURE CAADAR X; CAR CAR CDR CAR X;

SYMBOLIC PROCEDURE CAADDR X; CAR CAR CDR CDR X;

SYMBOLIC PROCEDURE CADAAR X; CAR CDR CAR CAR X;

SYMBOLIC PROCEDURE CADADR X; CAR CDR CAR CDR X;

SYMBOLIC PROCEDURE CADDAR X; CAR CDR CDR CAR X;

SYMBOLIC PROCEDURE CADDDR X; CAR CDR CDR CDR X;

SYMBOLIC PROCEDURE CDAAAR X; CDR CAR CAR CAR X;

SYMBOLIC PROCEDURE CDAADR X; CDR CAR CAR CDR X;

SYMBOLIC PROCEDURE CDADAR X; CDR CAR CDR CAR X;

SYMBOLIC PROCEDURE CDADDR X; CDR CAR CDR CDR X;

SYMBOLIC PROCEDURE CDDAAR X; CDR CDR CAR CAR X;

SYMBOLIC PROCEDURE CDDADR X; CDR CDR CAR CDR X;

SYMBOLIC PROCEDURE CDDDAR X; CDR CDR CDR CAR X;

SYMBOLIC PROCEDURE CDDDDR X; CDR CDR CDR CDR X;

SYMBOLIC PROCEDURE CAAAR X; CAR CAR CAR X;

SYMBOLIC PROCEDURE CAADR X; CAR CAR CDR X;

SYMBOLIC PROCEDURE CADAR X; CAR CDR CAR X;

SYMBOLIC PROCEDURE CADDR X; CAR CDR CDR X;

SYMBOLIC PROCEDURE CDAAR X; CDR CAR CAR X;

SYMBOLIC PROCEDURE CDADR X; CDR CAR CDR X;

SYMBOLIC PROCEDURE CDDAR X; CDR CDR CAR X;

SYMBOLIC PROCEDURE CDDDR X; CDR CDR CDR X;

symbolic procedure prin2(x);
    begin
        if pairp(x) then
        <<  wrtok( '!( );
            while pairp(x) do
            <<  prin2 car(x);
                x := cdr x;
		if not eq(x,NIL) then wrtok('! ); % A space.
            >>;
            if not eq(x,NIL) then
            <<  wrtok( '!.!  ); %Period followed by space.
                prin2(x);
            >>;
            wrtok( '!) );
        >>
        else
            wrtok(x);
    end;

symbolic procedure revx(l1,l2);
   % Non-destructive reverser, adds reverse of l1 to front of l2.
    begin
        while pairp(l1) do
        <<  l2 := (car l1).l2;
            l1 := cdr l1;
        >>;
        if not null (l1) then l2 := l1 . l2;
        return l2;
    end;

symbolic procedure rev(l1);
     revx(l1,NIL);

% EOF code is Ascii Z plus an offset of 1,  much too obscure!.
symbolic procedure eofp(x);
     if atom(x) and (!*inf(x) eq 27) then 'T else 'NIL;

symbolic procedure read();
    begin scalar itm,ii;
        itm := rdtok(); 
	if not(toktype eq 3) or eofp(itm)  then return(itm); % Over cautious;
        if (itm eq '!( ) 
            then return rlist()
        else if (itm eq '!' )         % Treat quote mark as QUOTE.
	    then return <<ii := read();
			  if eofp(ii) then ii
			   else ('QUOTE . ii . NIL)>>
        else return itm;
    end;

symbolic procedure rlist();
% Non destructive READ of S-expr, including ".".
    begin scalar itm,lst,done,last;
        itm := read();
        if eofp(itm) then return itm;
	done := NIL;
        while not done do
	    if itm eq '!) and toktype eq 3 
                  then done :='T
              else if itm = '!. and toktype eq 3 
	          then <<done:='T; last:= car rlist()>>  %CAR cures bug? WFG
              else
	          <<lst := itm.lst; itm := read()>>;
% ???   if pairp last then last:=car last>>;
     	if eofp(itm) then return itm;
        return revx(lst,last);
    end;

END$
