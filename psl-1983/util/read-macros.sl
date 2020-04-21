% READ-MACROS.SL - some specilized reader macros
%
% Author:      Don Morrison
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        Wednesday, 12 May 1982
% Copyright (c) 1981 University of Utah

% Edit by Cris Perdue,  1 Feb 1983 1400-PST
% Dochar moved into "nonkernel", "C" for "CONTROL", etc. commented out.
% Many miscellaneous symbolic names for characters removed.

((lambda (o-table)
   (setq LispScanTable* (TotalCopy o-table)) % in case it's in pure space
   (cond ((eq CurrentScanTable* o-table)
	   (setq CurrentScanTable* LispScanTable*))))
  LispScanTable*)

% plug backquote and friends into the lisp reader via read macros
% ` for backquote, , for unquote, ,@ for unquotel, and ,. for unquoted

(de backquote-read-macro (channel qt)
  (list 'backquote (ChannelReadTokenWithHooks channel)))

(de unquote-read-macro (channel qt)
  (list 'unquote (ChannelReadTokenWithHooks channel)))

(de unquotel-read-macro (channel qt)
  (list 'unquotel (ChannelReadTokenWithHooks channel)))

(de unquoted-read-macro (channel qt)
  (list 'unquoted (ChannelReadTokenWithHooks channel)))

(putv LispScanTable* (char !`) 11)

(putv LispScanTable* (char !,) 13)

(put '!, (getv LispScanTable* 128) '((!@ . !,!@)(!. . !,!.)))

(deflist
  '((!` backquote-read-macro)
    (!, unquote-read-macro)
    (!,!@ unquotel-read-macro)
    (!,!. unquoted-read-macro))
  'LispReadMacro)

% A couple of MACLISP style sharp sign read macros...

(putv LispScanTable* (char !#) 13)

(put '!# (getv LispScanTable* 128) '((!. . !#!.)
				     (!/ . !#!/)
				     (!' . !#!')
				     (!+ . !#!+)
				     (!- . !#!-)
				     (!\ . !#!\)))

(deflist
  `((!#!' ,(function function-read-macro))
    (!#!. ,(function eval-read-macro))
    (!#!\ ,(function char-read-macro))
    (!#!+ ,(function if-system-read-macro))
    (!#!- ,(function if-not-system-read-macro))
    (!#!/ ,(function single-char-read-macro)))
  'LispReadMacro)

(de function-read-macro (channel qt)
  `(function ,(ChannelReadTokenWithHooks channel)))

(de eval-read-macro (channel qt)
  (eval (ChannelReadTokenWithHooks channel)))

% (imports '(if-system)) % actually doesn't use the code, just the convention

(fluid '(system_list*))

(de if-system-read-macro (channel qt)
  ((lambda (system)
	   ((lambda (when_true)
		    (cond ((memq system system_list*) when_true)
			  (t (ChannelReadTokenWithHooks channel))))
	    (ChannelReadTokenWithHooks channel)))
   (ChannelReadTokenWithHooks channel)))

(de if-not-system-read-macro (channel qt)
  ((lambda (system)
	   ((lambda (when_false)
		    (cond ((not (memq system system_list*)) when_false)
			  (t (ChannelReadTokenWithHooks channel))))
	    (ChannelReadTokenWithHooks channel)))
   (ChannelReadTokenWithHooks channel)))

%(de when-read-macro (channel qt)
%  (let ((a (ChannelReadTokenWithHooks channel)))
%    (let ((b (ChannelReadTokenWithHooks channel))
%          (fn (and (idp a) (get a 'when-macro))))
%      (if fn
%	(apply fn (list b))
%	(StdError (BldMsg "Can't evaluate %r at %r time" b a))))))

% CompileTime and friends have to be made to work from LISP before these
% will be of much use.

%(foreach u in '(compile c CompileTime compile-time comp) do
%  (put u 'when-macro #'(lambda(x) `(CompileTime ,x))))

%(foreach u in '(load l LoadTime load-time) do
%  (put u 'when-macro #'(lambda(x) `(LoadTime ,x))))

%(foreach u in '(both b BothTimes both-times BothTime both-time) do
%  (put u 'when-macro #'(lambda(x) `(BothTimes ,x))))

%(foreach u in '(read r ReadTime read-time) do
%  (put u 'when-macro #'eval))

(de single-char-read-macro (channel qt)
  (ChannelReadChar channel))
% % Frightfully kludgey.  Anybody know how to just read the one character?
%   ((lambda (*raise)
%      ((lambda (ch)
%         ((lambda (n)
%    	   (if (lessp n 128)
% 	     n
% 	     (StdError (BldMsg "%r is illegal after #/" ch))))
% 	  (dochar ch)))
%         (ChannelReadTokenWithHooks channel)))
%    nil))

(de char-read-macro (channel qt)
  (dochar (ChannelReadTokenWithHooks channel)))

% Definition of dochar moved to char-macro.sl in the kernel /csp
% Alternative modifiers (below) removed, hope they aren't needed (yuk) /csp

% (put 'c 'char-prefix-function (get 'control 'char-prefix-function))
% (put '!^ 'char-prefix-function (get 'control 'char-prefix-function))
% (put 'm 'char-prefix-function (get 'meta 'char-prefix-function))

(commentoutcode
(deflist
% let char know all about the "standard" two and three letter names for
% non-printing ASCII characters.
  '((NUL 0)
    (SOH 1)
    (STX 2)
    (ETX 3)
    (EOT 4)
    (ENQ 5)
    (ACK 6)
    (BEL 7)
    (BS 8)
    (HT 9)
    (NL 10)
    (VT 11)
    (NP 12)
    (CR 13)
    (SO 14)
    (SI 15)
    (DLE 16)
    (DC1 17)
    (DC2 18)
    (DC3 19)
    (DC4 20)
    (NAK 21)
    (SYN 22)
    (ETB 23)
    (CAN 24)
    (EM 25)
    (SUB 26)
    (ESC 27)
    (FS 28)
    (GS 29)
    (RS 30)
    (US 31)
    (SP 32)
    (DEL 127))
  'charconst)
)

(commentoutcode
(deflist
  '((!^!@ 0) % "creeping featurism" here for sure...
    (!^A 1)
    (!^B 2)
    (!^C 3)
    (!^D 4)
    (!^E 5)
    (!^F 6)
    (!^G 7)
    (!^H 8)
    (!^I 9)
    (!^J 10)
    (!^K 11)
    (!^L 12)
    (!^M 13)
    (!^N 14)
    (!^O 15)
    (!^P 16)
    (!^Q 17)
    (!^R 18)
    (!^S 19)
    (!^T 20)
    (!^U 21)
    (!^V 22)
    (!^W 23)
    (!^X 24)
    (!^Y 25)
    (!^Z 26)
    (!^![ 8#33)
    (!^!\ 8#34)
    (!^!] 8#35)
    (!^!^ 8#36)
    (!^!~ 8#36)	% for telerays...
    (!^!_ 8#37)
    (!^!/ 8#37)	% for telerays...
    (!^!? 8#177))
  'charconst)
)

(commentoutcode
% It has been suggested that nice names for printing characters would be good,
% too, so here are some.  I don't really see that they're all that much use,
% but I guess they don't do any harm.  I doubt I'll ever use them, though.
% If this isn't "creeping featurism" I don't know what is....
(foreach u in 
  '((BANG !!)
    (EXCLAMATION !!)
    (AT !@)
    (ATSIGN !@)
    (SHARP !#)
    (POUND !#)
    (NUMBER !#)
    (NUMBER-SIGN !#)
    (HASH !#)
    (NOT-EQUAL !#) % For Algol 60 fans...
    (DOLLAR !$)
    (PERCENT !%)
    (CARET !^)
    (UPARROW !^)
    (AND !&)
    (AMPERSAND !&)
    (STAR !*)
    (TIMES !*)
    (LPAREN !( )
    (LEFT-PARENTHESIS !( )
    (LEFT-PAREN !( )
    (LPAR !( )
    (OPEN !( )
    (RPAREN !) )
    (RIGHT-PARENTHESIS !) )
    (RIGHT-PAREN !) )
    (RPAR !) )
    (CLOSE !) )
    (MINUS !-)
    (DASH !-)
    (UNDERSCORE !_)
    (UNDERLINE !_)
    (BACKARROW !_)
    (PLUS !+)
    (EQUAL !=)
    (EQUALS !=)
    (TILDE !~)
    (BACKQUOTE !`)
    (LBRACE !{)
    (LEFT-BRACE !{)
    (RBRACE !})
    (RIGHT-BRACE !})
    (LBRACKET ![)
    (LEFT-BRACKET ![)
    (LBRA ![)
    (RBRACKET !])
    (RIGHT-BRACKET !])
    (RBRA !])
    (APOSTROPHE !')
    (SINGLE-QUOTE !')
    (QUOTE-MARK !')
    (DOUBLE-QUOTE !")
    (STRING-MARK !")
%   (QUOTE should this be ' or "  -- I'll play it safe and not use either
    (COLON !:)
    (SEMI !;)
    (SEMICOL !;)
    (SEMICOLON !;)
    (QUESTION !?)
    (QUESTION-MARK !?)
    (QUESTIONMARK !?)
    (LESS !<)
    (LESS-THAN !<)
    (LANGLE !<)
    (LEFT-ANGLE !<)
    (LEFT-ANGLE-BRACKET !<)
    (GREATER !>)
    (GREATER-THAN !>)
    (GRTR !>)
    (RANGLE !>)
    (RIGHT-ANGLE !>)
    (RIGHT-ANGLE-BRACKET !>)
    (COMMA !,)
    (DOT !.)
    (PERIOD !.)
    (FULL-STOP !.) % For the English among us...
    (SLASH !/)
    (SOLIDUS !/)
    (DIVIDE !/)
    (BACKSLASH !\)
    (BAR !|)
    (VERTICAL !|)
    (VETICAL-BAR !|)
    (ZERO !0)
    (NAUGHT !0) % For the English among us...
    (ONE !1)
    (TWO !2)
    (THREE !3)
    (FOUR !4)
    (FIVE !5)
    (SIX !6)
    (SEVEN !7)
    (EIGHT !8)
    (NINE !9))
  do (put (car u) 'charconst (dochar (cadr u))))
)
