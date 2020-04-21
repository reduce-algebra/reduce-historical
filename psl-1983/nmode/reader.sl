%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Reader.SL - NMODE Command Reader
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        23 August 1982
% Revised:     16 February 1983
%
% 16-Feb-83 Alan Snyder
%  Declare -> Declare-Flavor.
% 3-Dec-82 Alan Snyder
%  GC calls cleanup-buffers before reclaiming.
% 21-Dec-82 Alan Snyder
%  Use generic arithmetic on processor times (overflowed on 9836).
%  Add declaration for NMODE-TIMER-OUTPUT-STREAM.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects extended-char fast-int pathnames))

% External variables used here:

(fluid '(nmode-allow-refresh-breakout))

% Global variables defined here:

(fluid '(
	 nmode-command-argument		% Numeric C-U argument (default: 1)
	 nmode-command-argument-given	% T if C-U used for this command
	 nmode-command-number-given	% T if an explicit number given
	 nmode-previous-command-killed	% T if previous command KILLED text
	 nmode-current-command		% Current command (char or list)
	 nmode-previous-command		% Previous command (char or list)
	 nmode-current-command-function	% Function for current command
	 nmode-previous-command-function% Function for previous command
	 nmode-autoarg-mode		% T => digits start command argument
	 nmode-temporary-autoarg	% T while reading command argument
	 nmode-command-killed		% Commands set this if they KILL text
	 nmode-command-set-argument	% Commands like C-U set this
	 nmode-reader-exit-flag		% Internal flag: causes reader to exit
	 nmode-gc-check-level		% number of free words causing GC
	 nmode-timing?			% T => time command execution
	 nmode-display-times?		% T => display times after each command
	 nmode-timer-output-stream	% optional stream to write times to

	 % The following variables are set when timing is on:

	 nmode-timed-step-count		% number of reader steps timed
	 nmode-refresh-time		% time used for last refresh
	 nmode-read-time		% time used for last read command
	 nmode-command-execution-time	% time to execute last command
	 nmode-total-refresh-time	% sum of nmode-refresh-time
	 nmode-total-read-time		% sum of nmode-read-time
	 nmode-total-command-execution-time% sum of nmode-command-execution-time
	 nmode-gc-start-count		% GCKnt when timing starts
	 nmode-gc-reported-count	% GCKnt when last reported
	 nmode-total-cons-count		% total words allocated (except GC)
	 ))

(setf nmode-timing? NIL)
(setf nmode-display-times? NIL)

(declare-flavor output-stream nmode-timer-output-stream)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(nmode-exit-on-abort))
(de nmode-reader (nmode-exit-on-abort)

  % Execute refresh/read/dispatch loop.  The loop can terminate in the following
  % ways: (1) A command can cause the reader to exit by either calling
  % EXIT-NMODE-READER or by throwing 'EXIT-NMODE.  In this case, the reader
  % terminates and returns NIL.  (2) A command can throw 'ABORT.  If
  % NMODE-EXIT-ON-ABORT is non-NIL, then the reader will terminate and return
  % 'ABORT; otherwise, it will ring the bell and continue.  (3) A command can
  % throw '$BREAK$ or 'RESET; this throw is relayed.  Other errors and throws
  % within a command are caught, messages are printed, and execution resumes.

  (let* ((nmode-reader-exit-flag NIL)		% FLUID variable
	 (nmode-previous-command-killed NIL)   	% FLUID variable
	 (nmode-command-killed NIL)		% FLUID variable
	 (nmode-command-argument 1)		% FLUID variable
	 (nmode-command-argument-given NIL)	% FLUID variable
	 (nmode-command-number-given NIL)	% FLUID variable
	 (nmode-current-command NIL)		% FLUID variable
	 (nmode-previous-command NIL)		% FLUID variable
	 (nmode-current-command-function NIL)	% FLUID variable
	 (nmode-previous-command-function NIL)	% FLUID variable
	 (nmode-command-set-argument NIL)	% FLUID variable 
	 (nmode-timing? NIL)			% FLUID variable
	 (*MsgP T)				% FLUID variable
	 (*BackTrace T)				% FLUID variable
	 )

    (while (not nmode-reader-exit-flag)
      (catch-all
        #'(lambda (tag result)
	    (cond
	     ((eq tag 'abort)
	      (if nmode-exit-on-abort (exit 'abort) (Ding)))
	     ((or (eq tag '$Break$) (eq tag 'RESET))
	      (nmode-select-buffer-channel)
	      (throw tag NIL))
	     ((eq tag '$error$) (Ding))
	     ((eq tag 'exit-nmode) (exit NIL))
	     (t (Printf "*****Unhandled THROW of %p" tag) (Ding))
	     ))
	(nmode-reader-step)
	))))

(de nmode-reader-step ()
  (cond ((not nmode-timing?)
	 (nmode-refresh)
	 (nmode-gc-check)
	 (nmode-read-command)
	 (nmode-execute-current-command)
	 )
	(t (nmode-timed-reader-step))
	))

(de nmode-read-command ()
  % Read one command and set the appropriate global variables.

  (when (not nmode-command-set-argument) % starting a new command
    (setf nmode-previous-command-killed nmode-command-killed)
    (setf nmode-previous-command nmode-current-command)
    (setf nmode-previous-command-function nmode-current-command-function)
    (setf nmode-command-argument 1)
    (setf nmode-command-argument-given NIL)
    (setf nmode-command-number-given NIL)
    (setf nmode-command-killed NIL)
    (setf nmode-temporary-autoarg NIL)
    (nmode-set-delayed-prompt "")
    )
  (setf nmode-current-command (input-command))
  (setf nmode-current-command-function
    (dispatch-table-lookup nmode-current-command))
  )

(de nmode-execute-current-command ()
  (setf nmode-command-set-argument NIL)
  (if nmode-current-command-function
    (apply nmode-current-command-function NIL)
    (nmode-undefined-command nmode-current-command)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Timing Support
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de start-timing-command ()
  (let ((fn (prompt-for-file-name
	     "Timing output to file:"
	     (namestring (make-pathname 'name "timing" 'type "txt"))
	     )))
    (cond ((not (setf nmode-timer-output-stream (attempt-to-open-output fn)))
	   (write-prompt "Unable to open file.")
	   (Ding)
	   )
	  (t
	   (reclaim)
	   (nmode-start-timing))
	  )))

(de stop-timing-command ()
  (cond (nmode-timing?
	 (nmode-stop-timing)
	 (if nmode-timer-output-stream (=> nmode-timer-output-stream close))
	 (setf nmode-timer-output-stream nil)
	 )))

(de nmode-start-timing ()
  (setf nmode-timing? T)
  (setf nmode-total-refresh-time 0)
  (setf nmode-total-read-time 0)
  (setf nmode-total-command-execution-time 0)
  (setf nmode-timed-step-count 0)
  (setf nmode-gc-start-count GCknt*)
  (setf nmode-gc-reported-count nmode-gc-start-count)
  (setf nmode-total-cons-count 0)
  )

(de nmode-stop-timing ()
  (cond (nmode-timing?
	 (setf nmode-timing? NIL)
	 (nmode-timing-message
	  (BldMsg "Total times: Refresh=%d Read=%d Execute=%d Cons=%d #GC=%d"
		  nmode-total-refresh-time
		  nmode-total-read-time
		  nmode-total-command-execution-time
		  nmode-total-cons-count
		  (- GCknt* nmode-gc-start-count)
		  ))
	 (nmode-timing-message
	  (BldMsg "Number of reader steps: %d" nmode-timed-step-count))
	 (if (> nmode-timed-step-count 0)
	   (nmode-timing-message
	    (BldMsg "Averages: Refresh=%d Read=%d Execute=%d Cons=%d"
		    (/ nmode-total-refresh-time nmode-timed-step-count)
		    (/ nmode-total-read-time nmode-timed-step-count)
		    (/ nmode-total-command-execution-time nmode-timed-step-count)
		    (/ nmode-total-cons-count nmode-timed-step-count)
		    ))))))

(de nmode-timed-reader-step ()
  (let ((heapx (GtHeap NIL))
	gc-happened
	)
    (nmode-timed-refresh)
    (nmode-gc-check)
    (nmode-timed-read-command)
    (nmode-timed-execute-current-command)
    (setf heapx (- heapx (GtHeap NIL)))
    (setf gc-happened (> GCknt* nmode-gc-reported-count))
    (setf nmode-gc-reported-count GCknt*)

    (cond ((not gc-happened)
	   (setf nmode-timed-step-count (+ nmode-timed-step-count 1))
	   (setf nmode-total-refresh-time
	     (+ nmode-total-refresh-time nmode-refresh-time))
	   (setf nmode-total-read-time
	     (+ nmode-total-read-time nmode-read-time))
	   (setf nmode-total-command-execution-time
	     (+ nmode-total-command-execution-time
		nmode-command-execution-time))
	   (setf nmode-total-cons-count
	     (+ nmode-total-cons-count heapx))
	   ))

    (nmode-timing-message
     (BldMsg "%w Refresh=%d Read=%d Execute=%d %w"
	     (string-pad-left (command-name nmode-current-command) 20)
	     nmode-refresh-time
	     nmode-read-time
	     nmode-command-execution-time
	     (if gc-happened
	       (BldMsg "#GC=%d" nmode-gc-reported-count)
	       (BldMsg "Cons=%d" heapx)
	       )
	     ))))

(de nmode-timed-refresh ()
  (let ((ptime (processor-time)))
    (nmode-refresh)
    (setf nmode-refresh-time (difference (processor-time) ptime))
    ))

(de nmode-timed-read-command ()
  (let ((ptime (processor-time)))
    (nmode-read-command)
    (setf nmode-read-time (difference (processor-time) ptime))
    ))

(de nmode-timed-execute-current-command ()
  (let ((ptime (processor-time)))
    (nmode-execute-current-command)
    (setf nmode-command-execution-time (difference (processor-time) ptime))
    ))

(de nmode-timing-message (s)
  (cond (nmode-display-times? (write-message s))
	(nmode-timer-output-stream
	 (=> nmode-timer-output-stream putl s))
	))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Garbage Collection
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-gc-check ()
  % Check to see if a garbage collection is needed (because we are low on
  % space).  If so, display a message and invoke the garbage collector.  (If a
  % garbage collection happens "by itself", no message will be displayed.)

  (if (not nmode-gc-check-level) (setf nmode-gc-check-level 1000))
  (when (< (GtHeap NIL) nmode-gc-check-level)
    (nmode-gc)
    ))

(de nmode-gc ()
  % Perform garbage collection while displaying a message.
  (let ((nmode-allow-refresh-breakout NIL)) % FLUID variable
    (write-prompt "Garbage Collecting!")
    (cleanup-buffers)
    (reclaim)
    (write-prompt
     (BldMsg "Garbage Collection Done: Free Space = %d words" (GtHeap NIL)))
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Miscellaneous Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de exit-nmode-reader ()
  % Set flag to cause exit from NMODE reader loop.
  (setf nmode-reader-exit-flag T)
  )

(de nmode-undefined-command (command)
  (nmode-error (BldMsg "Undefined command: %w" (command-name command)))
  )

(de nmode-error (s)
  (let ((nmode-allow-refresh-breakout NIL)) % FLUID variable
    (write-prompt s)
    (Ding)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Numeric Argument Command Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de argument-digit ()
  % This procedure must be attached only to extended characters whose base
  % characters are digits.
  (let* ((command nmode-current-command)
	 (base-ch (if (FixP command) (X-base command)))
	 (n (if (and base-ch (digitp base-ch)) (char-digit base-ch)))
	 )
    (if (null n)
      (Ding)
      (argument-digit-number n)
      )))

(de negative-argument ()
  (if (not nmode-command-number-given)
    % make "C-U -" do the right thing
    (cond ((> nmode-command-argument 0) (setf nmode-command-argument 1))
	  ((< nmode-command-argument 0) (setf nmode-command-argument -1))
	  ))
  (setf nmode-command-argument (- nmode-command-argument))
  (setf nmode-command-argument-given T)
  (setf nmode-command-set-argument T)
  (nmode-set-delayed-prompt
   (cond
    ((= nmode-command-argument 1) "C-U ")
    ((= nmode-command-argument -1) "C-U -")
    (t (BldMsg "C-U %d" nmode-command-argument))
    )))

(de universal-argument ()
  (setf nmode-command-argument (* nmode-command-argument 4))
  (setf nmode-command-argument-given T)
  (setf nmode-command-set-argument T)
  (setf nmode-temporary-autoarg T)
  (cond
   (nmode-command-number-given
    (nmode-set-delayed-prompt (BldMsg "C-U %d" nmode-command-argument))
    )
   (t (nmode-append-separated-prompt "C-U"))
   ))

(de argument-or-insert-command ()
  % This command interprets digits and leading hyphens as argument
  % prefix characters if NMODE-AUTOARG-MODE or NMODE-TEMPORARY-AUTOARG
  % is non-NIL; otherwise, it self-inserts.

  (let ((base-ch
	 (if (FixP nmode-current-command) (X-base nmode-current-command)))
	)
    (cond
     ((and (digitp base-ch) (or nmode-temporary-autoarg nmode-autoarg-mode))
      (argument-digit (char-digit base-ch)))
     ((and (= base-ch #/-)
	   (or nmode-temporary-autoarg nmode-autoarg-mode)
	   (not nmode-command-number-given))
      (negative-argument))
     (t (insert-self-command))
     )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Numeric Argument Support Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de argument-digit-number (n)
  (cond
   (nmode-command-number-given % this is not the first digit
    (setf nmode-command-argument
      (+ (* nmode-command-argument 10)
	 (if (>= nmode-command-argument 0) n (- n))))
    )
   (t % this is the first digit
    (if (> nmode-command-argument 0)
      (setf nmode-command-argument n)
      (setf nmode-command-argument (- n))
      )))
  (nmode-set-delayed-prompt (BldMsg "C-U %d" nmode-command-argument))
  (setf nmode-command-argument-given T)
  (setf nmode-command-number-given T)
  (setf nmode-command-set-argument T)
  )

% Convert from character code to digit.
(de char-digit (c)
  (cond ((digitp c) (difference (char-int c) (char-int #/0)))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(undeclare-flavor nmode-timer-output-stream)
