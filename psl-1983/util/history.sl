;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File containing functions to create a history mechanism.
;;	(exploited what is there with (inp n) (ans n) and historylist*).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  This file depends upon : init.lisp (basic lisp functions and syntax).
;;			(in <lanam.dhl>).
;;
;;  This file written by Douglas H. Lanam. September 1982.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; How to use the history mechanism implemented in this file:
;;
;;  This file allows you to take any previous input or output and substitute
;;	it in place of what you typed.  Thus you can either print or redo
;;	any input you have previously done.  You can also print or
;;	execute any result you have previously received.
;;	The system will work identify commands by either their history number,
;;	or by a subword in the input command.
;;
;;	This file also allows you to take any previously expression and do
;;	global substitutions on subwords inside words or numbers inside
;;	expressions(Thus allowing spelling corrections, and other word
;;	changes easily.)
;;
;;	This file has a set of read macros that insert the previous history
;;	text asked for inplace of them selves.  Thus they can be put inside
;;	any lisp expression typed by the user.  The system will evaluate
;;	the resulting expression the same as if the user had retyped everything
;;	in himself.
;;
;;	^^ : means insert last input command inplace of ^^.
;;		As an input command by itself,
;;			^^ by itself means redo last command.
;;
;;	^n : where n is a number replaces itself with the result of
;;		(inp n). ^n by itself means (redo n).
;;	^+n : same as ^n.
;;	^-n : is replaced by the nth back command. 
;;		replaced with the result of
;;		(inp (- current-history-number n)).
;;		by itself means (redo (- current-history-number n))
;;
;;	^word : where word starts with 'a'-'z' or 'A'-'Z', means
;;		take the last input command that has word as a subword
;;		or pattern of what was typed (after readmacros were
;;		executed.), and replace that ^word with that entire input
;;		command.
;;		If you want a word that doesn't begin with 'a'-'z', or 'A'-'Z',
;;		use ^?word where word can be any lisp atom.
;;		(say 23, *, |"ab|, word).
;;		ex.:  1 lisp> (plus 2 3)
;;			5
;;		      2 lisp> (* 4 5)
;;			20
;;		      3 lisp> ^us
;;			(PLUS 2 3)
;;			5
;;		      4 lisp> (* 3 ^lu)
;;			(PLUS 2 3)
;;			15
;;
;;		Case is ignored in word.  Word is read by the command read,
;;		And thus should be a normal lisp atom.  Use the escape
;;		character as needed.
;;
;;	If the first ^ in any of the above commands is replaced with
;;	^@, then instead of (inp n) , the read macro is replaced with
;;	(ans n).  Words are still matched against the input, not the
;;	answer.  (Probably something should be added to allow matching
;;	of subwords against the answer also.)
;;
;;	Thus:(if typed as commands by themselves):
;;	
;;	^@^ = (eval (ans (last-command)))
;;	^@3 = (eval (ans 3))
;;
;;	^@plus = (eval (ans (last-command which has plus as a subword in
;;				its input))).
;;
;;
;; Once the ^ readmacro is replaced with its history expression, you are
;;	allowed to do some editing of the command.  The way to do this
;;	is to type a colon immediately after the ^ command as described
;;	above before any space or other delimiting character.
;;	ex.: ^plus:p 
;;		^2:s/ab/cd/
;;		^^:p
;;		^@^:p
;;
;;	Currently there are two types of editing commands allowed.
;;
;;	:p means print only, do not insert in expression, whole 
;;		read macro returns only nil.
;;
;;	:s/word1/word2/ means take each atom in the expression found,
;;		and if word1 is a subword of that atom, replace the
;;		subword word1 with word2.  Read is used to read word1
;;		and word2, thus the system expects an atom and will
;;		ignore anything after what read sees before the /.
;;		Use escape characters as necessary.
;;
;;	:n where n is a positive unsigned number, means take the nth 
;;		element of the command(must be a list) and return it.
;;	
;;      ^string1^string2^ is equivalent to ^string1:s/string1/string2/
;;	ex.: ^plus^times^  is equivalent to ^plus:s/plus/times/ .
;;
;;	After a :s, ^ or :<n> command you may have another :s command, ^
;;	or a :p
;;	command.  :p command may not be followed by any other command.
;;
;;	The expression as modified by the :s commands is what is
;;	returned in place of the ^ readmacro.
;;	You need a closing / as seen in the :s command above.
;;	After the command you should type a delimiting character if
;;	you wish the next expression to begin with a :, since a :
;;	will be interpreted as another editing command.
;;
;;	On substitution, case is ignored when matching the subword,
;;	and the replacement subword
;;	is capitalized(unless you use an escape character before 
;;	typing a lowercase letter).
;;
;;	Examples:
;;	1 lisp> (plus 23 34)
;;	57
;;	2 lisp> ^^:s/plus/times/
;;	(TIMES 23 34)
;;	782
;;	3 lisp> ^plus:s/3/5/
;;	(PLUS 25 54)
;;	79
;;	4 lisp>
;;
;;
(defmacro unreadch (x) `(unreadchar (id2int ,x)))
(defmacro last-command () `(caadr historylist*))
(defmacro last-answer () `(cdadr historylist*))
(defun nth-command (n part) (cond ((eq part 'input) (inp n))
				  (t (ans n))))

(defun my-nthcdr (l n)
  (cond ((<= n 0) l)
	((null l) nil)
	((my-nthcdr (cdr l) (- n 1)))))

(defvar *print-history-command-expansion t)

(de skip-if (stop-char)
    (let ((x (readch)))
      (or (eq x stop-char) (unreadch x))))

(defun return-command (command)
  (and *print-history-command-expansion
       command
       ($prpr command) (terpri))
  command)

(defun do-history-command-and-return-command (string1 c)
  (let ((command (do-history-command string1 c)))
    (and *print-history-command-expansion command
	 ($prpr command) (terpri))
    command))

(defun nth-back-command (n)
  (do ((i n (+ 1 i))
       (command-list historylist*
		     (cdr command-list)))
      ((eq i 0) (caar command-list))))

(defvar *flink (*makhunk 80))

(defun kmp-flowchart-construction (p m)
  (rplacx 0 *flink -1)
  (do ((i 1 (+ 1 i)))
      ((> i m))
    (do ((j (cxr (- i 1) *flink) (cxr j *flink)))
	((or (= j -1) (= (cxr j p) (cxr (- i 1) p)))
	 (rplacx i *flink (+ j 1))))))

(defun kmp-scan (p m s)
  (and s
       (prog (j)
	 (setq j 0)
	loop (cond ((and (<> j -1) (<> (uppercassify (cxr j p))
				       (uppercassify (car s))))
		    (setq j (cxr j *flink)) (go loop)))
	 (and (= j m) (return t))
	 (or (setq j (+ 1 j) s (cdr s)) (return nil))
	 (go loop))))

(defun match-list-beginnings (starting-list list)
  (do ((x starting-list (cdr x))
       (y list (cdr y)))
      ((null x) t)
    (or (eq (car x) (car y))
	(return nil))))

(defun uppercassify (y)
  (cond ((and (>= y '|a|) (<= y '|z|))
	 (+ y (- '|A| '|a|)))
	(t y)))

(defun read-till-and-raise (stop-char)
  (let ((s (my-syntax stop-char)) (d))
    (my-set-syntax stop-char 17)
    (setq d (read)) (skip-if stop-char)
    (my-set-syntax stop-char s)
    d))

(defun do-history-command (string1 command)
  (let ((b))
       ;; colon after word indicates history command.
       ;; 
       (cond ((eq (setq b (readch)) '|:|)
	      ;; read key command
	      (selectq (setq b (readch))
		       (p
			;; only print result - dont execute
			;; return nil so that a quoted version doesn't confuse the
			;; history mechanism later.  ( i would like to change this
							 ;; to enter command in the history list but not execute).
			($prpr command) (terpri)
			(rplaca (car historylist*) command)
			(*throw '$error$ nil))
		       (s ; change all subwords of string1 with string2.
			  (do-history-command string1
					      (let ((delimiter (readch)))
						   (match-and-substitute
						    (read-till-and-raise delimiter) command
						    (read-till-and-raise delimiter)))))
		       ;;
		       ;; number indicates get that element of the command out of
		       ;; the list.
		       ;;
		       ((|0| |1| |2| |3| |4| |5| |6| |7| |8| |9|)
			(unreadch b)
			(let ((s (my-syntax '|:|))
			      (s1 (my-syntax '|^|))
			      (n))
			     (my-set-syntax '|:| 17)
			     (my-set-syntax '|^| 17)
			     (setq n (read))
			     (my-set-syntax '|:| s)
			     (my-set-syntax '|^| s1)
			     (cond ((null (dtpr command))
				    (princ "Error: not a list : ") ($prpr command)
				    (terpri) nil)
				   ((null (numberp n))
				    (princ "Error: expected number.  ")
				    (princ n)
				    (princ " is not a number.")
				    (terpri) nil)
				   ((> n (length command))
				    (princ "Error: ") (princ n)
				    (princ " is out of range for ") ($prpr command)
				    (terpri) nil)
				   (t (do-history-command string1 (nth command n))))))
		       (t
			(princ "Error: unknown command key : \|") 
			(princ b) (princ "|") 
			(terpri)
			;; return original command
			command)))
	     ((eq b '|^|)	
	      ;; equivalent to :s/string1/string2/
	      ;; is ^string1^string2^
	      (cond (string1 (match-and-substitute
			      string1 command
			      (read-till-and-raise '|^|)))
		    (t (terpri)
		       (princ "illegal option to history command.")
		       (terpri)
		       nil)))
	     (t (unreadch b)
		;; return original command
		command))))

(defun match-back-command (partial-match /&optional (part-to-return 'input))
  (let ((p (list2vector (explode partial-match))))
    (let ((m (upbv p)))
      (kmp-flowchart-construction p m)
      (do ((x (cdr historylist*) (cdr x)))
	  ((null x) nil)
	(and (kmp-scan p m (explode (caar x)))
	     (cond ((eq part-to-return 'input)
		    (return (caar x)))
		   (t (return (cdar x)))))))))

(defun match-and-substitute (partial-match command replacement)
  (let ((p (list2vector (explode partial-match))))
    (let ((m (upbv p)))
      (kmp-flowchart-construction p m)
      (let ((l (flatsize partial-match)))
	(match-and-substitute1 p m (explode partial-match)
			       command (explode replacement) l)))))

(defun match-and-substitute1 (p m s command replacement l)
  (cond ((or (atom command) (numberp command))
	 (kmp-scan-and-replace p m (explode command)
			       replacement l command))
	(t (cons
	    (match-and-substitute1 p m s (car command) replacement l)
	    (match-and-substitute1 p m s (cdr command) replacement l)))))

(defun kmp-scan-and-replace (p m s replacement l command)
  (and s (prog (j k flag)
	   (setq flag (stringp command))
	   (setq j 0) (setq k nil)
	  loop
	   (cond ((and (<> j -1)
		       (<> (uppercassify (cxr j p))
			   (uppercassify (car s))))
		  (setq j (cxr j *flink)) (go loop)))
	   (setq k (cons (car s) k))
	   (and (= j m)
		(return (cond ((stringp command)
			       (list2string
				(cdr (append
				      (append (nreverse (my-nthcdr k l))
					      replacement)
				      (cdr (nreverse
					    (cdr (nreverse s))))))))
			      (t (let ((x (append
					   (append
					    (nreverse (my-nthcdr k l))
					    replacement)
					   (cdr s))))
				   (and (= (my-syntax (car x)) 14)
					(<= (my-syntax (cadr x)) 10)
					(setq x (cdr x)))
				   (let ((y (implode x)))
				     (cond ((eq (flatsize y) (length x)) y)
					   (t (intern (list2string x))))))))))
	   (or (setq j (+ 1 j) s (cdr s)) (return command))
	   (go loop))))

(defun read-sub-word ()
  (let ((c (my-syntax '|:|))
	(d))
    ;; dont read : since it is the special command character.
    (my-set-syntax '|:| 17)
    (setq d (read))
    (my-set-syntax '|:| c)
    d))

(defun re-execute-command (/&optional (part 'input))
  (let ((y (readch)))
    (cond ((eq y '\^) (do-history-command-and-return-command 
		       nil (last-command)))
	  ((eq y '\*) (do-history-command-and-return-command 
		       nil (last-answer)))
	  ((eq y '\@) (re-execute-command 'answer))
	  ((eq y '\?) 
	   (let ((yy (read-sub-word)))
		(do-history-command-and-return-command yy
		 (match-back-command yy part))))
	  ((or (digit y) (memq y '(|+| |-|)))
	   (unreadch y)
	   (let ((y (read-sub-word)))
	     (cond ((numberp y)
		    (cond ((> y 0) (do-history-command-and-return-command nil
				    (nth-command y part)))
			  ((< y 0) (do-history-command-and-return-command nil
				    (nth-back-command y))))))))
	  ((liter y)
	   (unreadch y)
	   (let ((yy (read-sub-word)))
		(do-history-command-and-return-command  
		 yy
		 (match-back-command yy))))
	  )))

(my-set-readmacro '\^ (function re-execute-command))
