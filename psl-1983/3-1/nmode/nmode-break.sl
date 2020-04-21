%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% NMODE-BREAK.SL - NMODE Break Handler
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        26 August 1982
%
% Adapted from Will Galway's EMODE
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects))
(fluid '(*NMODE-RUNNING
	 *nmode-init-running
	 *OutWindow
	 nmode-terminal
	 nmode-command-argument
	 nmode-buffer-channel))

(fluid '(BreakLevel* *QuitBreak BreakEval* BreakName* ERROUT* ErrorForm*))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% We redefine BREAK (the break handler) and YESP.
% Grab the original versions (if we can find them!).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(if (FUnboundP 'pre-nmode-break)
    (CopyD 'pre-nmode-break
	   (if (FUnboundP 'pre_rawio_break)
		'break
		'pre_rawio_break
		)))

(if (FUnboundP 'pre-nmode-yesp)
    (CopyD 'pre-nmode-yesp 'yesp))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialization:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de enable-nmode-break ()
  (let ((*usermode NIL)
	(*redefmsg NIL)
	)
    (CopyD 'break 'nmode-break)
    (CopyD 'yesp 'nmode-yesp)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Break handler:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-break ()
  (cond (*NMODE-RUNNING (nmode-break-handler))
	(t
	 (let ((old-raw-mode (=> nmode-terminal raw-mode)))
	   (leave-raw-mode)
	   (prog1
	    (pre-nmode-break)
	    (if old-raw-mode (enter-raw-mode))
	    )))))

(de nmode-break-handler ()
  (let* ((BreakLevel* (+ BreakLevel* 1))
	 (*QuitBreak T)
	 (BreakEval* 'Eval)
	 (BreakName* "NMODE Break")
	 (OldIN* IN*)
	 (OldOUT* OUT*)
	 (nmode-error? (eq in* 0))
	 (nmode-channel? (eq in* nmode-buffer-channel))
	 (init-error? *nmode-init-running)
	 (old-raw-mode (=> nmode-terminal raw-mode))
	 (*OutWindow T) % always pop up on a break
	 (*nmode-init-running NIL) % ditto
	 (*NMODE-RUNNING (not nmode-error?))
	 )
    (cond (nmode-error?
	   (leave-raw-mode)
	   (RDS 0)
	   (WRS 1)
	   )
	  (t
	   (RDS nmode-buffer-channel)
	   (WRS nmode-buffer-channel)
	   (enter-raw-mode)
	   ))
    (when init-error?
      (Printf "Error occurred while executing your NMODE INIT file!%n")
      (Ding)
      )
    (unwind-protect
      (Catch '$Break$
	(TopLoop 'Read 'Print 'BreakEval BreakName* "NMODE Break loop")
	)
      (RDS OldIN*)
      (WRS OldOUT*)
      (if old-raw-mode (enter-raw-mode))
      )
    (if *QuitBreak
	(let ((*Break NIL)
	      (*EmsgP NIL)
	      )
	  (StdError "Exit to ErrorSet")))
    )
  (Eval ErrorForm*)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Break command functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de lisp-quit-command ()
  (cond ((ensure-in-break)
	 (setf *QuitBreak T)
	 (throw '$Break$ NIL)
	 )))

(de lisp-retry-command ()
  (cond ((ensure-in-break)
	 (cond (*ContinuableError
		 (setf *QuitBreak NIL)
		 (throw '$Break$ NIL)
		 )
	       (t
		(write-prompt "Cannot retry: error is not continuable.")
		(Ding)))
	 )))

(de lisp-continue-command ()
  (cond ((ensure-in-break)
	 (cond (*ContinuableError
		 (setf ErrorForm* (MkQuote BreakValue*))
		 (setf *QuitBreak NIL)
		 (throw '$Break$ NIL)
		 )
	       (t
		(write-prompt "Cannot continue: error is not continuable.")
		(Ding)))
	 )))

(de lisp-abort-command ()
  (cond ((ensure-in-break)
	 (reset))))

(de lisp-backtrace-command ()
  (cond ((ensure-in-break)
	 (nmode-select-buffer-channel)
	 (cond ((>= nmode-command-argument 16) (VerboseBackTrace))
	       ((>= nmode-command-argument 4) (InterpBackTrace))
	       (t (BackTrace)))
	 (nmode-select-old-channels)
	 )))

(de lisp-help-command ()
  (write-message
   (if (> BreakLevel* 0)
    "Lisp break commands: Q-quit;A-abort;R-retry;C-continue;B-backtrace"
    "Lisp commands: E-execute form;Y-yank last output;L-invoke Lisp Listener"
    )))

(de ensure-in-break ()
  (if (> BreakLevel* 0)
      T
      (write-prompt "Not in a break loop!")
      (Ding)
      NIL
      ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Query functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-yesp (message)
  (cond ((and *NMODE-RUNNING (=> nmode-terminal raw-mode))
	 (nmode-yes-or-no? message))
	(t (pre-nmode-yesp message))
	))

(de nmode-yes-or-no? (message)
  (let ((response (prompt-for-string message NIL)))
    (while T
      (cond ((string-equal response "Yes") (exit T))
	    ((string-equal response "No") (exit NIL))
	    (t (Ding)
	       (write-prompt "Please answer YES or NO.")
	       (sleep-until-timeout-or-input 60)
	       (setf response (prompt-for-string message NIL))
	       )))))

(de nmode-y-or-n? (message)
  (write-message message)
  (nmode-set-immediate-prompt "Y or N: ")
  (let ((answer
	 (while T
	   (let ((ch (char-upcase (input-direct-terminal-character))))
	     (when (= ch #/Y) (nmode-complete-prompt "Y") (exit T))
	     (when (= ch #/N) (nmode-complete-prompt "N") (exit NIL))
	     (when (= ch #\BELL) (exit 'ABORT))
	     (Ding)
	     ))))
    (set-prompt "")
    (write-message "")
    (if (eq answer 'ABORT) (throw 'ABORT NIL))
    answer
    ))
