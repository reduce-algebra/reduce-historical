%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% MODES.SL - NMODE Mode Manipulation Functions
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        14 September 1982
% Revised:     4 March 1983
%
% 4-Mar-83 Alan Snyder
%  Revise pathname-default-mode to handle invalid pathname.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects extended-char))

% Global variables:

(fluid '(nmode-default-mode
         nmode-minor-modes % list of active minor modes (don't modify inplace!)
	 ))

% Internal static variables:

(fluid '(nmode-defined-modes
	 nmode-file-modes
	 ))

(setf nmode-default-mode NIL)
(setf nmode-defined-modes ())
(setf nmode-file-modes ())
(setf nmode-minor-modes ())

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Mode Definition:
%
% The following function is used to define a mode (either major or minor):
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-define-mode (name establish-expressions)
  (let* ((mode (make-instance 'mode
			      'name name
			      'establish-expressions establish-expressions
			      ))
	 (pair (Ass
		(function string-equal)
		name
		nmode-defined-modes
		)))
    (if pair
      (rplacd pair mode)
      (setf nmode-defined-modes
	(cons (cons name mode) nmode-defined-modes)
	))
    mode
    ))

(defflavor mode (
		name
  		establish-expressions
		)
  ()
  gettable-instance-variables
  initable-instance-variables
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File Modes
%
% The following functions associate a default mode with certain filename
% extensions.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-declare-file-mode (file-type mode)
  (let ((pair (Ass
		(function string-equal)
		file-type
		nmode-file-modes
		)))
    (if pair
      (rplacd pair mode)
      (setf nmode-file-modes
	(cons (cons file-type mode) nmode-file-modes)
	))
    ))

(de pathname-default-mode (fn)
  (let ((pn (maybe-pathname fn)))
    (if pn
      (let ((pair (Ass
		   (function string-equal)
		   (pathname-type pn)
		   nmode-file-modes
		   )))
	(if pair (cdr pair) nmode-default-mode)
	)
      nmode-default-mode
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Minor Modes
%
% A minor mode is a mode that can be turned on or off independently of the
% current buffer or the current major mode.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de minor-mode-active? (m)
  % M is a mode object.  Return T if it is an active minor mode.
  (memq m nmode-minor-modes)
  )

(de activate-minor-mode (m)
  % M is a mode object.  Make it active (if it isn't already).
  (when (not (minor-mode-active? m))
    (setf nmode-minor-modes (cons m nmode-minor-modes))
    (nmode-establish-current-mode)
    ))

(de deactivate-minor-mode (m)
  % M is a mode object.  If it is active, deactivate it.
  (when (minor-mode-active? m)
    (setf nmode-minor-modes (delq m nmode-minor-modes))
    (nmode-establish-current-mode)
    ))

(de toggle-minor-mode (m)
  % M is a mode object.  If it is active, deactivate it and return T;
  % otherwise, activate it and return NIL.

  (let ((is-active? (minor-mode-active? m)))
    (if is-active?
      (deactivate-minor-mode m)
      (activate-minor-mode m)
      )
    is-active?
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Manipulating mode lists:
%
% The following functions are provided for use in user init files.  They are
% not used in NMODE.  See the file -CUSTOMIZING.TXT for information on how to
% customize NMODE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de add-to-command-list (listname command func)
  (let* ((old-list (eval listname))
	 (old-binding (assoc command old-list))
	 (binding (cons command func))
	 )
    (cond
        % If the binding isn't already in the a-list.
        ((null old-binding)
          % Add the new binding
	  (set listname (aconc old-list binding)))
        % Otherwise, replace the old operation in the binding.
        (T
          (setf (cdr old-binding) func)))
    NIL
    ))

(de remove-from-command-list (listname command)
  (let* ((old-list (eval listname))
	 (old-binding (assoc command old-list))
	 )
    (cond (old-binding
	   (set listname (DelQ old-binding old-list))
	   NIL
	   ))))

(de set-text-command (command func)

  % This function is a shorthand for modifying text mode.  The arguments are as
  % for ADD-TO-COMMAND-LIST.  The change takes effect immediately.

  (add-to-command-list 'Text-Command-List command func)
  (nmode-establish-current-mode))
