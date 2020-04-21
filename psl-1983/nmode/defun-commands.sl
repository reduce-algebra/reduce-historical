%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defun-Commands.SL - NMODE DEFUN commands and functions
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        12 November 1982
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects fast-int))

(fluid '(nmode-command-argument
	 nmode-command-argument-given
	 nmode-current-command
	 ))

% Global variables:

(fluid '(nmode-defun-predicate
	 nmode-defun-scanner
	 ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Defun Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de reposition-window-command ()
  % Adjust the current window so that the beginning of the
  % current DEFUN is on the top line of the screen.  If this change
  % would push the current line off the screen, do nothing but ring
  % the bell.

  (let ((old-pos (buffer-get-position)))
    (when (move-to-start-of-current-defun) % if search for defun succeeds
      (let ((old-line (buffer-position-line old-pos))
	    (defun-line (current-line-pos))
	    )
	(if (or (< old-line defun-line) % Impossible?
		(>= old-line (+ defun-line (current-window-height)))
		)
	  (Ding) % Old Line wouldn't show on the screen
	  % otherwise
	  (current-window-set-top-line defun-line)
	  ))
      (buffer-set-position old-pos)
      )))

(de end-of-defun-command ()
  % This command has a very strange definition in EMACS.  I don't even
  % want to try to explain it!  It is probably a kludge in EMACS since
  % it generates very strange error messages!

  (if (< nmode-command-argument 0)
    (move-backward))

  % First, we must get positioned up at the beginning of the proper defun.
  % If we are within a defun, we want to start at the beginning of that
  % defun.  If we are between defuns, then we want to start at the beginning
  % of the next defun.

  (if (not (move-to-start-of-current-defun))
    (move-forward-defun))

  % Next, we move to the requested defun, and complain if we can't find it.
  (unless
   (cond
    ((> nmode-command-argument 1)
     (move-over-defuns (- nmode-command-argument 1)))
    ((< nmode-command-argument 0)
     (move-over-defuns nmode-command-argument))
    (t t)
    )
   (Ding)
   )

  % Finally, we move to the end of whatever defun we wound up at.
  (if (not (move-to-end-of-current-defun)) (Ding))
  )

(de mark-defun-command ()
  (cond ((or (move-to-end-of-current-defun)
	     (and (move-forward-defun) (move-to-end-of-current-defun))
	     )
	 (set-mark-from-point)
	 (move-backward-defun)
	 (when (not (current-line-is-first?))
	   (move-to-previous-line)
	   (if (not (current-line-blank?))
	     (move-to-next-line))
	   ))
	(t (Ding))
	))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Defun Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de move-backward-defun ()
  % Move backward at least one character to the previous beginning of a
  % "defun".  If no defun is found, return NIL and leave point unchanged.

  (when (move-backward-character)
    (or (beginning-of-defun)
	(progn (move-forward-character) NIL) % return NIL
	)))

(de beginning-of-defun ()
  % Move backward, if necessary, to the beginning of a
  % "defun".  If no defun is found, return NIL and leave point unchanged.

  (let ((old-pos (buffer-get-position)))
    (move-to-start-of-line)
    (while T
      (when (current-line-is-defun?) (exit T))
      (when (current-line-is-first?) (buffer-set-position old-pos) (exit NIL))
      (move-to-previous-line)
      )))

(de move-forward-defun ()
  % Move forward at least one character to the next beginning of a
  % "defun".  If no defun is found, return NIL and leave point unchanged.

  (let ((old-pos (buffer-get-position)))
    (while T
      (when (current-line-is-last?) (buffer-set-position old-pos) (exit NIL))
      (move-to-next-line)
      (when (current-line-is-defun?) (exit T))
      )))

(de move-to-start-of-current-defun ()
  % If point lies within the text of a (possibly incomplete) defun, or on
  % the last line of a complete defun, then move to the beginning of the
  % defun.  Otherwise, return NIL and leave point unchanged.

  (let ((old-pos (buffer-get-position))) % save original position
    (if (beginning-of-defun) % find previous defun start
      (let ((start-pos (buffer-get-position))) % save defun starting position
	% We succeed if the current defun has no end, or if the end is
	% beyond the old position in the buffer.
	(if (or (not (scan-past-defun))
		(<= (buffer-position-line old-pos) (current-line-pos))
		)
	  (progn (buffer-set-position start-pos) T)
	  (progn (buffer-set-position old-pos) NIL)
	  )))))

(de move-to-end-of-current-defun ()
  % If point lies within the text of a complete defun, or on the last line
  % of the defun, then move to the next line following the end of the defun.
  % Otherwise, return NIL and leave point unchanged.

  (let ((old-pos (buffer-get-position))) % save original position
    (if (and (beginning-of-defun) % find previous defun start
	     (scan-past-defun) % find end of that defun
	     (<= (buffer-position-line old-pos) (current-line-pos))
	     )
      (progn (move-to-next-line) T)
      (progn (buffer-set-position old-pos) NIL)
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basic Defun Scanning Primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de current-line-is-defun? ()
  (if nmode-defun-predicate
    (apply nmode-defun-predicate ())
    ))

(de scan-past-defun ()
  % This function should be called with point at the start of a defun.
  % It will scan past the end of the defun (not to the beginning of the
  % next line, however).  If the end of the defun is not found, it returns
  % NIL and leaves point unchanged.

  (if nmode-defun-scanner
    (apply nmode-defun-scanner ())
    ))
