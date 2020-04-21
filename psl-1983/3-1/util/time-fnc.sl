;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Time-fnc.sl : code to time function calls.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Written by Douglas Lanam. (November 1982).
;;
;; To be compiled inside `pfrl' using the command:
;;	(compile-file time-fnc).
;;
;; The object created is usuable in any psl on machine it is compiled for.
;;
;;  Usage:
;;
;;	do 
;;	(timef function-name-1 function-name-2 ...)
;;
;;	Timef is a fexpr.
;;	It will redefine the functions named so that timing information is
;;	kept on these functions.  
;;	This information is kept on the property list of the function name.
;;	The properties used are `time' and `number-of-calls'.
;;
;;	(get function-name 'time) gives you the total time in the function.
;;	(not counting gc time).
;;	Note, this is the time from entrance to exit.
;;	The timef function redefines the function with an
;;	unwind-protect, so calls that are interrupted
;;	by *throws are counted.
;;
;;	(get function-name 'number-of-calls) gives you the number of times
;;	the function is called.
;;
;;	To stop timing do : 
;;	(untimef function-name1 ..)
;;	or do (untimef) for all functions.
;;	(untimef) is a fexpr.
;;
;;	To print timing information do 
;;	(print-time-info function-name-1 function-name-2 ..)
;;
;;	or do (print-time-info) for timing information on all function names.
;;
;;	special variables used: 
;;	*timed-functions* : list of all functions currently being timed.
;;	*all-timed-functions* : list of all functions ever timed in the
;;		current session.
;;
;;	Comment: if tr is called on a called on a function that is already
;;	being timed, and then untimef is called on the function, the
;;	function will no longer be traced.
;;
(defvar *timed-functions* nil)
(defvar *all-timed-functions* nil)

(defun timef fexpr (names)
  (cond ((null names) *timed-functions*)
	((f-mapc
	  '(lambda (x)
		   (or (memq x *timed-functions*)
		       (let ((a (getd x)))
			    (cond (a (put x 'orig-function-def a)
				     (setq *timed-functions*
					   (cons x *timed-functions*))
				     (or (memq x *all-timed-functions*)
					 (setq *all-timed-functions*
					       (cons x *all-timed-functions*)))
				     (set-up-time-function
				      (car a) x (cdr a)))
				  (t (princ x) 
				     (princ " is not a defined function.")
				     (terpri))))))
	  names))))

(defun set-up-time-function (type x old-func)
  (let ((y (cond ((codep old-func)
		  (code-number-of-arguments old-func))
		 (t (length (cadr old-func)))))
	(args) (function) (result-var (gensym)) (gc-time-var (gensym))
	(time-var (gensym)))
       (do ((i y (difference i 1)))
	   ((= i 0))
	   (setq args (cons (gensym) args)))
       (putd x type
	     `(lambda ,args
		      (time-function ',x ',old-func 
				     (list (time) . ,args))))
       x))

(defvar |* timing time *| 0)

#+dec20
(defvar *call-overhead-time* 0.147)

#+vax
(defvar *call-overhead-time* 0.1)

#+dec20
(defvar *time-overhead-time* 0.437)

#+vax
(defvar *time-overhead-time* 1.3)

(defvar |* number of sub time calls *| 0)

(defun time-function (name function-pointer arguments)
  (let ((itime-var (car arguments)) (result) (n)
	(endt) (total-fnc-time) (time-var) (gc-time-var))
       (unwind-protect
	(let ((|* timing time *| 0)
	      (|* number of sub time calls *| 0))
	     (unwind-protect
	      (let () (setq gc-time-var gctime* time-var (time)
			    result (apply function-pointer (cdr arguments))
			    endt (time))
		   result)
	      (cond
	       (time-var
		(or endt (setq endt (time)))
		(Setq n |* number of sub time calls *|)
		(put name 'number-of-sub-time-calls
		     (+ n (or (get name 'number-of-sub-time-calls) 0)))
		(setq total-fnc-time (- (- endt time-var) |* timing time *|))
		(put name 'time
		     (+ (or (get name 'time) 0)
			(- total-fnc-time (- gctime* gc-time-var))))
		(put name 'number-of-calls
		     (1+ (or (get name 'number-of-calls) 0)))))))
	(prog ()
	      (setq |* timing time *|
		    (- (- |* timing time *| itime-var) total-fnc-time)))
	      (setq |* number of sub time calls *| 
		    (1+ |* number of sub time calls *|))
	      (setq |* timing time *| (+ |* timing time *| (time)))))))

(defun untimef fexpr (names)
  (f-mapc '(lambda (x)
		   (cond ((memq x *timed-functions*)
			  (let ((a (get x 'orig-function-def)))
			       (cond (a (putd x (car a) (cdr a)))))
			  (setq *timed-functions*
				(delq x *timed-functions*)))))
	  (or names *timed-functions*)))

(defun print-time-info fexpr (names)
  (f-mapc '(lambda (x)
		   (let ((n (get x 'number-of-calls))
			 (ns (get x 'number-of-sub-time-calls))
			 (time) (t1 (get x 'time)))
			(princ x) (princ " ")
			(tab 20)
			(princ (or n 0)) (princ " calls")
			(cond (n 
			       (setq time
				     (max 0 
					  (difference
					   (difference
					    (or t1 0)
					    (times *call-overhead-time*
						   (or n 0)))
					   (times *time-overhead-time*
						  (or ns 0)))))
			       (tab 31) (princ time) (princ " ms")
			       (tab 48) 
			       (princ (quotient (float time) (float n)))
			       (princ " ms\/call")))
			(terpri)))
	  (or names *all-timed-functions*))
  (terpri))
