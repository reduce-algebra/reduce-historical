%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% LISP-Interface.SL - NMODE Lisp Text Execution Interface
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        23 August 1982
% Revised:     28 February 1983
%
% Adapted from Will Galway's EMODE
%
% 28-Feb-83 Alan Snyder
%  Change nmode-main to initially call leave-raw-mode.  This is to make NMODE
%  refresh the display automatically when it is restarted.
% 14-Feb-83 Alan Snyder
%  Added statement to flush output buffer cache.
% 2-Feb-83 Alan Snyder
%  Added Execute-Defun-Command.  Change to supply the free EOL at the end of
%  the input buffer whenever the buffer-modified flag is set, instead of only
%  when currently at the end of the buffer.
% 25-Jan-83 Alan Snyder
%  Check terminal type after resuming.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects))

(fluid '(nmode-current-buffer
	 nmode-output-buffer
	 nmode-terminal
	 nmode-initialized
	 *NMODE-RUNNING
	 *GC
	 LispBanner*
	 *RAWIO
	 *nmode-init-running
	 *nmode-init-has-run
	 nmode-terminal-input-buffer
	 nmode-default-init-file-name
	 nmode-auto-start
	 nmode-first-start
	 ))

(setf *NMODE-RUNNING NIL)
(setf *nmode-init-running NIL)
(setf *nmode-init-has-run NIL)
(setf nmode-default-init-file-name "PSL:NMODE.INIT")
(setf nmode-auto-start NIL)
(setf nmode-first-start T)

(fluid '(
	 nmode-buffer-channel	% Channel used for NMODE I/O.
	 nmode-output-start-position  % Where most recent "output" started in buffer.
	 nmode-output-end-position  % Where most recent "output" ended in buffer.
	 OldStdIn
	 OldStdOut
	 OldErrOut
	 ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de yank-last-output-command ()
  % Insert "last output" typed in the OUTPUT buffer.  Output is demarked by
  % NMODE-OUTPUT-START-POSITION and NMODE-OUTPUT-END-POSITION.

  (if (not nmode-output-start-position)
    (Ding)
    % Otherwise
    (let ((text (=> nmode-output-buffer
		    extract-region
		    NIL
		    nmode-output-start-position
		    (or nmode-output-end-position
			(buffer-position-create (=> nmode-output-buffer size) 0)
			)
		    )))
      (=> nmode-current-buffer insert-text (cdr text))
      )))

(de execute-form-command ()
  % Execute starting at the beginning of the current line.

  (set-mark-from-point) % in case the user wants to come back
  (move-to-start-of-line)
  (execute-from-buffer)
  )

(de execute-defun-command ()
  % Execute starting at the beginning of the current defun (if the current
  % position is within a defun) or from the current position (otherwise).

  (set-mark-from-point) % in case the user wants to come back
  (move-to-start-of-current-defun)
  (execute-from-buffer)
  )

(de make-buffer-terminated ()
  % If the current buffer ends with an "unterminated" line, add an EOL to
  % terminate it.

  (let ((old-pos (buffer-get-position)))
    (move-to-buffer-end)
    (when (not (current-line-empty?)) (insert-eol))
    (buffer-set-position old-pos)
    ))

(de execute-from-buffer ()
  % Causes NMODE to return to the procedure that called it (via
  % nmode-channel-editor) with input redirected to come from the (now) current
  % buffer.  We arrange for output to go to the end of the output buffer.

  (if (=> nmode-current-buffer modified?) (make-buffer-terminated))
  (buffer-channel-set-input-buffer nmode-buffer-channel nmode-current-buffer)

  % Output will go to end of the output buffer.  Supply a free EOL if the last
  % line is unterminated.  Record the current end-of-buffer for later use by
  % Lisp-Y.

  (let ((old-pos (=> nmode-output-buffer position)))
    (=> nmode-output-buffer move-to-buffer-end)
    (if (not (=> nmode-output-buffer current-line-empty?))
      (=> nmode-output-buffer insert-eol))
    (setf nmode-output-start-position (=> nmode-output-buffer position))
    (=> nmode-output-buffer set-position old-pos)
    )

  % Set things up to read from and write to NMODE buffers.
  (nmode-select-buffer-channel)
  (exit-nmode-reader)
  )

(de nmode-exit-to-superior ()
  (if (not *NMODE-RUNNING)
    (original-quit)
    % else
    (leave-raw-mode)		% Turn echoing back on.  Next refresh is FULL.
    (original-quit)
    (enter-raw-mode)		% Turn echoing off.
    (nmode-set-terminal)	% Ensure proper terminal driver is loaded.
    ))

% Redefine QUIT so that it restores the terminal to echoing before exiting.
(when (FUnboundP 'original!-quit)
  (CopyD 'original!-quit 'quit)
  (CopyD 'quit 'nmode-exit-to-superior)
  )

(de emode () (nmode)) % for user convenience

(de nmode ()

  % Rebind the PSL input channel to the NMODE buffer channel and return.  This
  % will cause the next READ to invoke Nmode-Channel-Editor and start running
  % NMODE.  Use the function "exit-nmode" to switch back to original channels.

  (nmode-initialize) % does nothing if already initialized
  (when (neq STDIN* nmode-buffer-channel)
    (setf OldStdIn STDIN*)
    (setf OldStdOut STDOUT*)
    (setf OldErrOut ErrOut*)
    )
  (nmode-select-buffer-input-channel)
  )

(de nmode-run-init-file ()
  (setf *nmode-init-has-run T)
  (let ((fn (namestring (init-file-pathname "NMODE"))))
    (cond ((FileP fn)
	   (nmode-execute-init-file fn))
	  ((FileP (setf fn nmode-default-init-file-name))
	   (nmode-execute-init-file fn))
	  )))

(de nmode-execute-init-file (fn)
  (let ((*nmode-init-running T))
    (nmode-read-and-evaluate-file fn)
    ))

(de nmode-read-and-evaluate-file (fn)
  (let ((chn (open fn 'INPUT))
	exp
	)
    (while (not (eq (setf exp (ChannelRead chn)) $Eof$))
      (eval exp)
      )
    (close chn)
    )
  )

(de exit-nmode ()
  % Leave NMODE, return to normal listen loop.
  (nmode-select-old-channels)
  (=> nmode-terminal move-cursor (=> nmode-terminal maxrow) 0)
  (leave-raw-mode)
  (setf *NMODE-RUNNING NIL)
  (setf *GC T)
  (exit-nmode-reader) % Set flag to cause NMODE to exit.
  )

% The following function is not currently used.
(de nmode-invoke-lisp-listener ()
  % Invoke a normal listen loop.
  (let* ((*NMODE-RUNNING NIL)
	 (OldIN* IN*)
	 (OldOUT* OUT*)
	 (ERROUT* 1)
	 (StdIn* 0)
	 (StdOut* 1)
	 (old-raw-mode (=> nmode-terminal raw-mode))
	 )
    (leave-raw-mode)
    (RDS 0)
    (WRS 1)
    (unwind-protect
     (TopLoop 'Read 'Print 'Eval "Lisp" "Return to NMODE with ^Z")
     (RDS OldIN*)
     (WRS OldOUT*)
     (if old-raw-mode (enter-raw-mode))
     )))
% (de emode () (throw '$read$ $eof$)) % use with above function
% (de nmode () (throw '$read$ $eof$)) % use with above function

(de nmode-select-old-channels ()
  % Select channels that were in effect when "Lisp Interface" was started up.
  % (But don't turn echoing on.)  NOTE that the "old channels" are normally
  % selected while NMODE is actually running (this is somewhat counter
  % intuitive).  This is so that any error messages created by bugs in NMODE
  % will not be printed into NMODE buffers.  (If they were, it might break
  % things recursively!)

  (setf STDIN* OldStdIn)
  (setf STDOUT* OldStdOut)
  (setf ErrOut* OldErrOut)
  (RDS STDIN*)    % Select the channels.
  (WRS STDOUT*)
  )

(de nmode-select-buffer-channel ()
  % Select channels that read from and write to NMODE buffers.
  (nmode-select-buffer-input-channel)
  (setf STDOUT* nmode-buffer-channel)
  (setf ErrOut* nmode-buffer-channel)
  (WRS STDOUT*)
  )

(de nmode-select-buffer-input-channel ()
  % Select channel that reads from NMODE buffer.  "NMODE-Channel-Editor" is
  % called when read routines invoke the "editor routine" for the newly selected
  % channel.

  (if (null nmode-buffer-channel)
    (setf nmode-buffer-channel
      (OpenBufferChannel NIL nmode-output-buffer 'nmode-channel-editor)))
  (setf STDIN* nmode-buffer-channel)
  (RDS STDIN*)
  )

(de nmode-channel-editor (chn)

  % This procedure is called every time that input is requested from an NMODE
  % buffer.  It starts up NMODE (if not already running) and resumes NMODE
  % execution.  When the user has decided on what input to give to the channel
  % (by performing Lisp-E), the NMODE-reader will return with I/O bound to the
  % "buffer channel".  The reader will also return if the user performs Lisp-L,
  % in which case I/O will remain bound to the "standard" channels.

  % Select "old" channels, so if an error occurs we don't get a bad recursive
  % situation where printing into a buffer causes more trouble!

  (nmode-select-old-channels)
  (cond ((not *NMODE-RUNNING)
	 (setf *NMODE-RUNNING T)
	 (setf *GC NIL)
	 (if (not *nmode-init-has-run)
	   (nmode-run-init-file)
	   )
	 )
	(t
	 (buffer-channel-flush nmode-buffer-channel)
	 (setf nmode-output-end-position (=> nmode-output-buffer position))
	 % compensate for moving to line start on next Lisp-E:
	 (if (not (at-line-start?))
	   (move-to-next-line))
         )
	)
  (enter-raw-mode)
  (nmode-select-major-window) % just in case
  (NMODE-reader NIL) % NIL => don't exit when a command aborts
  )

(de nmode-main ()
  (setf CurrentReadMacroIndicator* 'LispReadMacro) % Crock!
  (setf CurrentScanTable* LispScanTable*)
  (when (not toploopread*)
    (setf toploopread* 'read)
    (setf toploopprint* 'print)
    (setf toploopeval* 'eval)
    (setf toploopname* "NMODE Lisp")
    )
  (nmode-initialize) % does nothing if already initialized
  (nmode-set-terminal) % ensure proper terminal driver is loaded

  % Note: RESET may cause echoing to be turned on without clearing *RawIO.
  (when *RawIO
    (setf *RawIO NIL)
    (EchoOff)
    )
  (leave-raw-mode)

  (when nmode-first-start
    (setf nmode-first-start NIL) % never again
    (cond (nmode-auto-start
	   (setf *NMODE-RUNNING T) % see below
           (let ((was-modified? (=> nmode-output-buffer modified?)))
	     (=> nmode-output-buffer insert-line LispBanner*)
	     (if (not was-modified?)
	       (=> nmode-output-buffer set-modified? NIL)
	       )))
	  (t
	   (printf "%w%n" LispBanner*)
	   ))
    )

  (while T
    (setf nmode-terminal-input-buffer NIL) % flush execution from buffers
    (cond (*NMODE-RUNNING
	   (setf *NMODE-RUNNING NIL) % force full start-up
	   (nmode) % cause next READ to start up NMODE
	   )
	  (t
	   (RDS 0)
	   (WRS 1)
	   ))
    (nmode-top-loop)
    ))

(copyd 'main 'nmode-main)

(de nmode-top-loop ()
  (TopLoop toploopread* toploopprint* toploopeval* toploopname* "")
  (Printf "End of File read!")
  )
