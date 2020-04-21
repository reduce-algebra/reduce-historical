;;;
;;; STEP.LSP - Single-step evaluator
;;; 
;;; Author:      Eric Benson
;;;	         Symbolic Computation Group
;;;              Computer Science Dept.
;;;              University of Utah
;;; Date:        30 March 1982
;;; Copyright (c) 1982 University of Utah
;;;

#+Tops20
(eval-when (compile eval)	; Needed for PBIN in STEP-GET-CHAR
  (load monsym))

(imports '(evalhook))		; Tell the loader that evalhook is needed

(defvar step-level 0 "Level of recursion while stepping")

(defvar step-form () "Current form being evaluated")

(defvar step-pending-forms () "Buffer of forms being evaluated")

(defvar abort-step () "Flag to indicate exiting step")

(defvar step-dispatch (make-vector 127 t ())
		      "Dispatch table for character commands")

(defvar step-channel () "I/O Channel used for printing truncated forms.")

(eval-when (compile eval)

;;;; DEF-STEP-COMMAND - define a character command routine
(defmacro def-step-command (char . form)
  `(vset step-dispatch ,char (function (lambda () ,@form))))
)

;;;; STEP - user entry point
(defun step (form)
  (let ((step-level 0)
	(step-pending-forms ())
	(abort-step ()))
    (prog1 (step-eval form)
	   (terpri))))

;;;; STEP-EVAL - main routine
(defun step-eval (step-form)
  (if abort-step
      (eval step-form)
      (let ((step-pending-forms (cons step-form step-pending-forms)))
	   (step-print-form step-form "-> ")
	   (let ((macro-call (macro-p (first step-form))))
		(when macro-call
		      (setq step-form (funcall macro-call step-form))
		      (step-print-form step-form "<->")))
	   (let ((step-value (let ((step-level (add1 step-level)))
				  (step-command))))
		(unless (and abort-step (not (eql abort-step step-level)))
			(setq abort-step ())
			;; Print the non macro-expanded form
			(step-print-value (first step-pending-forms)
					  step-value))
		step-value))))

;;;; Control-N - Continue stepping each time
(def-step-command #\
  (evalhookfn step-form #'step-eval))

;;;; Space - do not step lower levels
(def-step-command #\blank
  (eval step-form))

;;;; Control-U - go up to next higher evaluation level
(def-step-command #\
  (setq abort-step (- step-level 2))
  (eval step-form))

;;;; Control-X - abort stepping entirely
(def-step-command #\
  (setq abort-step -1)
  (eval step-form))

;;;; Control-G - grind the current form
(def-step-command #\bell
  (terpri)
  (prettyprint (first step-pending-forms))
  (step-command))

;;;; Control-P is the same as Control-G
(vset step-dispatch #\ (vref step-dispatch #\bell))

;;;; Control-R grinds the form in Rlisp syntax
(def-step-command #\
  (terpri)
  (rprint (first step-pending-forms))			; This will only
  (step-command))					; work in Rlisp


;;;; Control-E - edit the current form
(def-step-command #\
  (setq step-form (edit step-form))
  (step-command))

;;;; Control-B - go into a break loop
(def-step-command #\
  (step-break)
  (step-command))

;;;; Control-L redisplay the last 10 pending forms
(def-step-command #\ff
  (display-last-10)
  (step-command))

;;;; ? - help
(def-step-command #\?
  (load help)
  (displayhelpfile 'step)
  (step-command))

(defun display-last-10 ()
  (display-aux step-pending-forms 10))

(defun display-aux (b n)
  (let ((step-level (sub1 step-level)))
       (unless (or (null b) (eql n 0))
	       (display-aux (rest b) (sub1 n))
	       (step-print-form (first b) "-> "))))

;;;; STEP-COMMAND - read a character and dispatch on it
(defun step-command ()
  (let ((c (vref step-dispatch (step-get-char))))
    (if c (funcall c)
          (ouch #\bell) (step-command))))

;;;; STEP-PRINT-FORM - print incoming form with indentation
(defun step-print-form (form herald)
  (terpri)
  (tab (min step-level 15))
  (princ herald)
  (channelprin1 step-channel form))

;;;; STEP-PRINT-VALUE - print form and result of evaluation
(defun step-print-value (form value)
  (terpri)
  (tab (min step-level 15))
  (princ "<- ")
  (channelprin1 step-channel form)
  (terpri)
  (tab (+ (min step-level 15) 3))
  (prin1 value))

;;;; STEP-BREAK - errset-protected break loop
(defun step-break ()
  (errset (break) ()))

;;;; STEP-GET-CHAR - read a single character
#+Tops20
(lap '((*entry step-get-char expr 0)
       (*move #\? (reg 1))
       (pbout)
       (pbin)
       (*exit 0)))

#-Tops20
(defun step-get-char ()
  (let ((promptstring* "?"))
    (do ((ch (channelreadchar stdin*) (channelreadchar stdin*)))
        ((not (eql ch #\eol)) ch))))

;;;; STEP-PUT-CHAR - prints on current channel, truncates to one line
(defun step-put-char (channel ch)
  (if (not (eql ch #\eol))
      (unless (> (posn) 75) (writechar ch))))

(eval-when (load eval)			; Open a special channel
(let ((specialwritefunction* #'step-put-char)
      (specialreadfunction* #'writeonlychannel)
      (specialclosefunction* #'illegalstandardchannelclose))
     (setq step-channel (open "" 'special)))
)
