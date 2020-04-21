%
% PROCESS.SL
%    Routines to support  generalized inferior processes in TOPS20 PSL.
%    Much of the code is based on PHOTO.FAI
%
%  Mark R. Swanson
%  University of Utah
%  June 17, 1983
%

(load objects monsym jsys)
(fluid '(current-process
	  process-list
	  nmode-selectable-processes))
		
(setf current-process nil)
(setf process-list nil)
(setf nmode-selectable-processes ())

(de create-process-stream (name b)
  (let ((process (make-instance 'process-stream
				'exe-file-name name
				'out-buf b)))
    process))

(defflavor process-stream (
   (sys-proc-id 0)
   ttyjfn
   ptyjfn
   (exe-jfn -1)
   exe-file-name
   out-buf
   output-end
   mode-word
   string-in
   status
   )
  ()
  (initable-instance-variables exe-file-name out-buf)
  (gettable-instance-variables ttyjfn 
			       out-buf mode-word
			       status exe-file-name
			       sys-proc-id)
)

(defmethod (process-stream init) ()
        (=> self getjfn)        % get jfn for executable
	(=> self getpty)	% get a jfn for pty
        (=> self efork)		% create an inferior fork and attach it to PTY
	(=> self setpty)	% set up pty parameters, links, etc.
	(=> self runfrk)        % start up the fork
	)

(defmethod (process-stream write-to-process) (string)
  % Send the given string to the inferior process thru the PTY,  but do not
  %  block if buffer is full (for whatever reason).  Also, only dole out the
  %  string in bite-size pieces.
  %  91 seems to be a magic number, as far as tty buffers go.

  (let ((str-len (add1 (size string)))
	(i 0)
	cur-sout-len)
  (while (and (timeout-wait 'accepting-output? (list ttyjfn) 60)
              (> str-len 0))
    (setf cur-sout-len (min 92 str-len))
    (jsys0 ptyjfn 
           (sub string i (sub1 cur-sout-len))
           cur-sout-len
           0 (const jsSOUT))
    (setf i (+ i cur-sout-len))
    (setf str-len (- str-len cur-sout-len)))
  (if (~= str-len 0)
    (write-message "Current process not accepting input"))
  ))

(de user-typed-input? ()
  % Return T if our user has typed something, NIL if not
  (~= (xsibe 8#100) 0))

(de accepting-output? (jfn)
  % See if PTY buffer is already filled to capacity
  (<= (xsibe jfn) 92))   % 8#91 is assumed not to exceed buffer capacity
                         %  of a PTY, but be enough to force process wakeup

% The following are provided to avoid unwanted error handling on the +1 return

(lap '((!*entry xsibe expr 1)
      (jsys (const jssibe))
      (jfcl)
      (!*move (reg 2) (reg 1))
      (!*exit 0)))

(lap '((!*entry xsobe expr 1)
      (jsys (const jssobe))
      (jfcl)
      (!*move (reg 2) (reg 1))
      (!*exit 0)))

(defmethod (process-stream read-into-buffer) ()
  % Reads output of inferior process into associated buffer, if any output
  %  is to be had;  waits only a *small* finite amount of time for input to
  %  appear.

  (let ((chars-read nil)
	(input-recvd nil))
    (=> out-buf move-to-buffer-end)    % New output should appear at buffer end
    (while  (and                       % Keep reading until no more output from
	     (not (user-typed-input?)) %  process  or user typein.
	     (setf chars-read (=> self read-from-process)))
      (setf input-recvd t)             % So we will know to refresh window.
      (let ((string string-in)
	    (i 0)
	    char)
	(while
	  (< i chars-read)
	  (if (~= (setf char (string-fetch string i)) #\cr) % ignore CR's
	    (=> out-buf insert-character char))
	  (setf i (+ i 1))
	  )))
    (setf output-end (=> out-buf position))
    (if input-recvd (=> self window-refresh)) % refresh window when all done
    ))
  
(defmethod (process-stream read-from-process) ()
  % READ-FROM-PROCESS reads as many chars as are waiting to be read into
  %  string-in and returns number read, or NIL if there were none.  Will
  %  not block if no output is available,  though it will wait a short 
  %  time for some to arrive.

  (let ((chars-to-read (timeout-wait 'output-waiting? (list ttyjfn) 20))
	)
    (if (null chars-to-read) (exit nil))
    (setf string-in (mkstring (- chars-to-read 1) 0))
    (- chars-to-read (jsys3 ptyjfn string-in chars-to-read 0 (const jsSIN)))
    ))

(de output-waiting? (jfn)
  % OUTPUT-WAITING? checks inferior process' tty output buffer to see if it's
  %  empty.  Returns NIL if it is empty, else the count of characters in buffer.

  (let ((n (xsobe jfn)))
    (if (= n 0) nil n)))

(defmethod (process-stream getjfn) ()
  % GETJFN -- get a jfn for executable file specified by exe-file-name
  (setf exe-jfn (jsys1 (bits 2 17) exe-file-name 0 0 (const jsGTJFN)))
  )

(defmethod (process-stream efork) ()
  % EFORK -- create an inferior fork and get a copy of the desired file into it
  (setf sys-proc-id (jsys1 (bits 1) 0 0 0 (const jsCFORK))) % create fork
  (jsys0 sys-proc-id 0 0 0 (const jsFFORK)) % freeze it
  (jsys0 (xword sys-proc-id exe-jfn) 0 0 0 (const jsGET)) % get the executable into it
  (jsys0 sys-proc-id                        % don't allow LOGOff or CTRL-C trap
	 (xword 8#200001 
		(lowhalfword (jsys2 sys-proc-id 0 0 0 (const jsRPCAP))))
	 0 0 (const jsEPCAP))
  (jsys0 sys-proc-id (xword ttyjfn ttyjfn) 0 0 (const jsSPJFN))
  )

(defmethod (process-stream runfrk) ()
  % RUNFRK -- run something in an inferior fork
  % returns with ERRFLG T if the fork terminated abnormally
  (jsys0 sys-proc-id 0 0 0 (const jsSFRKV))
  (jsys0 sys-proc-id 0 0 0 (const jsRFORK))
  (setf status (jsys1 sys-proc-id 0 0 0 (const jsRFSTS)))
  %  (setf error-flag (not (eqn 2 (land (loworderhalf status) 2))))
  )

% (defmethod (process-stream proc-sts) ()
% (setf status (jsys1 sys-proc-id 0 0 0 (const jsRFSTS)))
%  (setf mode-word (jsys2 ttyjfn 0 0 0 (const jsRFMOD)))
%  )

%(defmethod (process-stream running) ()
%  (not (eqn (land (highhalfword status) 8#400000) 8#400000)))

%(defmethod (process-stream io-wait) ()
%  (eqn (land (highhalfword status) 8#377777) 1))

(defmethod (process-stream getpty) ()
  % GETPTY - get a jfn on a pty and also its TTY number
  (let ((curpty (get-1-pty)))
    (cond ((eqn curpty -1)
	   (ErrorPrintF
	    "There are too many people using PTY's now; try again later.")))
    (setf ptyjfn (openpty (ptynum curpty)))
    (setf ttyjfn (openpty (ttynum curpty)))
    ))

(defmethod (process-stream intrpt-process) ()
  % essentially the same as ^C to the inferior
  (jsys0  sys-proc-id (bits 1) 0 0 (const jsIIC))
  )

(defmethod (process-stream close-pty) ()
  (jsys0 ptyjfn 0 0 0 (const jsCLOSF))
  (jsys0 ttyjfn 0 0 0 (const jsCLOSF))
  (setf ptyjfn 0)
  (setf ttyjfn 0)
  )

(defmethod (process-stream kill) ()
  % kil the fork, close its PTY's, reset fork handle
  (jsys0 sys-proc-id 0 0 0 (const jsKfork))
  (setf sys-proc-id 0)
  (=> self close-pty)
  )

(de get-1-pty ()
  % find an available PTY; note that TOPS20 will tell us that a PTY is available
  %  to us if we have it in use already--ensure that we get a new one.

  (for* (with dev-characteristics pty-owning-job
	      (numpty (HighHalfWord (jsys1 26 0 0 0 (const JsGETAB))))
	      (my-job-num (jsys3 -1 (xword -1 3) 0 0 (const jsGETJI))))
	(from curpty 0 numpty 1)
	(finally (return -1))	% in case none is found
	(do
	 (setf dev-characteristics 
	   (jsys2 (xword 8#600013 curpty) 0 0 0 (const JsDVCHR)))
	 (setf pty-owning-job 
	   (highhalfword (jsys3 (xword 8#600013 curpty) 0 0 0 (const JsDVCHR))))
	 (cond
	  ((and
	    (eqn 8#010000	   % is it available?
		 (land (highhalfword dev-characteristics) 8#010000)) % dv%av 
	    (not (eqn my-job-num    % does it already belong to us?
		      pty-owning-job)))
	   (return curpty))
	  )
	 )))

(de openpty (ptynum)
  %
  (let ((devnam (Mkstring 10))
	ptyjfn)
    (jsys0 devnam                  % turn Device descriptor into a name-string
	   (jsys1 ptynum 0 0 0 (const JsDVCHR))
	          0 0 (const JsDEVST))
    (setf devnam (recopystringtonull devnam)) % truncate it at NULL
    (setf ptyjfn                              % make it into a TOPS-20 dev name
      (jsys1 (Xword 8#001 0) (concat devnam ":") 0 0 (const JsGTJFN))) % gj%sht!gj%acc
    (jsys0 ptyjfn (Xword 8#70000 8#300000) 0 0 (const JsOPENF)) % 7 bit byte,in-out
    ptyjfn))

(de ttynum (ptynum)
% TTYNUM--given a PTY number, turn it into the device designator of the
%  associated TTY
  (plus ptynum 
	(LowHalfWord (jsys1 22 0 0 0 (const JsGETAB))) % 26 is the index of the PTY table
	8#400000)) % .ttdes

(de ptynum (ptynum)
  % PTYNUM--given a PTY number, turn it into a PTY device designator
  (xword 8#600013 ptynum))

(defmethod (process-stream setpty) ()
  % SETPTY-- set up PTY mode

  (jsys0 ttyjfn 8#525252525252 8#525252525252 0 (const jsSFCOC))
  (setf mode-word (jsys2 ttyjfn 0 0 0 (const jsRFMOD)))
  (jsys0 ttyjfn (land mode-word 8#777777774000) 0 0 (const jsSFMOD))
  (jsys0 ttyjfn (land mode-word 8#777777774000) 0 0 (const jsSTPAR))
  )

(defmethod (process-stream window-refresh) ()
  (when out-buf
    (if (and *OutWindow
	  (not (buffer-is-displayed? out-buf)))
      (nmode-expose-output-buffer out-buf))
    (let ((window-list (find-buffer-in-exposed-windows out-buf)))
      (when window-list
	(nmode-adjust-output-window (car window-list))
	))))
    
(defmethod (process-stream name) ()
  (=> out-buf name))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	
(de feed-process-from-buffer (terminate-flag)
  % Causes NMODE to send input to an inferior process from the current buffer.
  % Output will go to end of the output buffer.  Supply a free EOL if the last
  % line is unterminated.

  (if (null current-process) (write-message "No process")
    (if (=> nmode-current-buffer modified?) (make-buffer-terminated))
    (let* ((process-output-buffer (=> current-process out-buf))
	   (old-pos (=> process-output-buffer position))
	   (input-line (=> nmode-current-buffer current-line))
	   )
      (=> process-output-buffer set-mark-from-point)

      % Set things up to read from and write to NMODE buffers.
      (=> current-process write-to-process input-line)
      (if terminate-flag
	(=> current-process write-to-process (mkstring 0 #\lf)))
      (=> nmode-current-buffer move-to-next-line)
      (=> current-process read-into-buffer)
      )))

(de create-process-command ()
  (let* ((fn (prompt-for-file-name "Executable file: " "SYSTEM:EXEC.EXE"))
	  (nmode-default-mode process-mode)
	  (b (buffer-create-default
	       (buffer-make-unique-name
		 (filename-to-buffername fn))))
	  (process  (create-process-stream fn b)))
    (setf nmode-selectable-processes (cons process nmode-selectable-processes))
    (setf current-process process)
    ))
	
(de execute-region-command ()
  % Send region to inferior process; one line at a time.
  % NOT YET FULLY IMPLEMENTED
  (set-mark-from-point) % in case the user wants to come back
  (move-to-start-of-line)
  (feed-process-from-buffer t)
  )

(de execute-line-command ()
  % Send current line to inferior process; start at the beginning of the line.

  (set-mark-from-point) % in case the user wants to come back
  (move-to-start-of-line)
  (feed-process-from-buffer t)
  )

(de execute-unterminated-line-command ()
  % Execute starting at the beginning of the current line, do not send an EOL.

  (set-mark-from-point) % in case the user wants to come back
  (move-to-start-of-line)
  (feed-process-from-buffer nil)
  )

(de intrpt-process-command ()
  (if (null current-process)
    (write-message "No process")
    (=> current-process intrpt-process)))

(de kill-process-command ()
  (if current-process
    (progn
     (=> current-process kill)
     (setf current-process (cadr nmode-selectable-processes))
     (setf nmode-selectable-processes (cdr nmode-selectable-processes)))
    (write-message "No process")))

(de send-char-immediate-command ()
  % Send the next character as is, without waiting for a line terminator
  %  Useful for sending control characters, and for talking to programs (such
  %  as DDT, that break on single, non-control characters such as "/"

  (if current-process
    (let ((ch (input-direct-terminal-character)))
      (=> current-process write-to-process (mkstring 0 ch))
      (=> current-process read-into-buffer))
    (write-message "No process")))

(de execute-from-input-window ()
  (if (null current-process) 
    (write-message "No process")
    %else
    (let* ((buf (=> current-process out-buf))
	   (prompt-string (progn
			   (=> buf move-to-buffer-end)
			   (=> buf current-line))))
     (=> current-process write-to-process (prompt-for-process-string 
					   prompt-string NIL))
     (=> current-process write-to-process (mkstring 0 #\lf))
     (=> current-process read-into-buffer))
    ))

(de cut-line-command ()
  (let ((cur-char-pos (current-char-pos))
	(cur-line (current-line)))
    (update-kill-buffer
     (cons 1 (vector (sub cur-line cur-char-pos
			  (- (size cur-line) cur-char-pos))))
     )))
				      
% A replacement for NMODE-READER-STEP (found in PN:READER.SL);  the only
%  change is to check for output from inferior process(es)

(de nmode-reader-step ()
  (cond ((not nmode-timing?)
	 (nmode-refresh)
	 (nmode-gc-check)
	 (nmode-process-output-check)
	 (nmode-read-command)
	 (nmode-execute-current-command)
	 )
	(t (nmode-timed-reader-step))
	))

(de nmode-process-output-check()
  % Check for output from the current (if there is one) process; read it if
  % there is any; the read should not block waiting for further output
  (cond ((and
	  current-process
	  (output-waiting?  (=> current-process ttyjfn)))
	 (=> current-process read-into-buffer)))
  T
  )

(de prompt-for-process-string (prompt-string restore-inserts?)
  % This function is similar to PROMPT-FOR-STRING.
  (setf nmode-input-special-command-list nil)
  (if restore-inserts?
    (self-inserting-command))
  (if (> nmode-input-level 0)
    (throw '$error$ NIL)
    % else
    (let ((old-msg nmode-message-string)
	  (old-window nmode-current-window)
	  (nmode-input-level (+ nmode-input-level 1)) % FLUID
	  )
      (=> (=> nmode-input-window buffer) reset)
      (nmode-select-window nmode-input-window)
      (set-message prompt-string)
      (set-prompt "") % avoid old prompt popping back up when we're done

      % Edit the buffer until an "exit" character is typed or the user aborts.

      (cond ((eq (NMODE-reader T) 'abort)
	     (=> nmode-input-window deexpose)
	     (nmode-select-window old-window)
	     (set-message old-msg)
	     (throw 'abort NIL)
	     ))

      % Show the user that his input has been accepted.
      (move-to-start-of-line)
      (nmode-refresh-one-window nmode-input-window)

      % Pick up the string that was typed. 
      (let ((return-string (current-line)))

	% Switch back to old window, etc.
	(=> nmode-input-window deexpose)
	(nmode-select-window old-window)

	% Restore original "message window".
	(set-message old-msg)
	return-string
	))))

(de Process-prefix ()
  (nmode-append-separated-prompt "Process-")
  (let ((ch (input-terminal-character)))
    (nmode-complete-prompt (x-char-name ch))
    (list (x-char C-!\) ch)
    ))

(define-command-prefix 'Process-prefix "Process-")

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Most of what follows really should gpo into MODE-DEFS.SL, if processes become
%  an accepted part of NMODE

(CompileTime (load extended-char))

(fluid '(Process-Mode
	 ))

(fluid '(Process-Command-List
	 Process-Mode-Command-List
	 ))

(setf Text-Mode
  (nmode-define-mode
   "Text"
   '((nmode-define-commands Text-Command-List)
     (nmode-define-commands Modifying-Terminal-Command-List)
     (nmode-define-commands Process-Command-List)
     (nmode-establish-mode Read-Only-Text-Mode)
     (nmode-define-normal-self-inserts)
     )))

(setf Process-Mode
  (nmode-define-mode
   "Process"
   '((nmode-define-commands Process-Command-List)
     (nmode-define-commands Process-Mode-Command-List)
     (nmode-establish-mode Read-Only-Text-Mode)
     )))

(setf Lisp-Interface-Mode
  (nmode-define-mode
   "Lisp"
   '((nmode-define-commands Rlisp-Command-List)
     (establish-lisp-parser)
     (nmode-define-commands Lisp-Command-List)
     (nmode-define-commands Process-Command-List)
     (nmode-establish-mode Text-Mode)
     )))

(de process-mode-command ()
  (buffer-set-mode nmode-current-buffer Process-Mode)
  )

% Process-Mode-Command-List - commands related to the Process interface

(setf Process-Mode-Command-List
  (list
   (cons (x-char C-k)                   'cut-line-command)
   (cons (x-char RETURN)		'execute-line-command)
   ))

% Process-Command-List - commands related to the Process interface

(setf Process-Command-List
  (list
   (cons (x-char C-!\)			'Process-prefix)
   (cons (x-chars C-!\ C)		'intrpt-process-command)
   (cons (x-chars C-!\ E)		'execute-line-command)
   (cons (x-chars C-!\ I)		'execute-from-input-window)
   (cons (x-chars C-!\ K)		'kill-process-command)
   (cons (x-chars C-!\ Q)               'send-char-immediate-command)
   (cons (x-chars C-!\ P)               'process-browser-command)
   (cons (x-chars C-!\ U)		'execute-unterminated-line-command)
   ))

(setf Basic-Command-List
  (NConc Basic-Command-List
	 (list (cons (m-x "Create Process") 'create-process-command))))

(setf Text-Command-List
  (NConc Text-Command-List
	 (list (cons (m-x "Process Mode") 'Process-mode-command))))
