%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Buffers.SL - Buffer Collection Manipulation Functions
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        23 August 1982
% Revised:     25 January 1983
%
% This file contains functions that manipulate the set of existing buffers.
%
% 25-Jan-83 Alan Snyder
%  Fix bug in buffer name completion: now accepts the name of an existing buffer
%  even when the name is a prefix of the name of some other buffer.
% 29-Dec-82 Alan Snyder
%  Revise prompt-for-buffer code to use new prompted input.
%  PROMPT-FOR-EXISTING-BUFFER now completes on CR and LF, as well as SPACE.
% 3-Dec-82 Alan Snyder
%  Added CLEANUP-BUFFERS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects extended-char fast-strings))
(load stringx)

(fluid '(nmode-current-buffer nmode-current-window nmode-main-buffer
	 nmode-output-buffer nmode-default-mode nmode-input-default
	 ))

(fluid '(nmode-selectable-buffers))
(if (not (boundp 'nmode-selectable-buffers))
  (setf nmode-selectable-buffers NIL))

% Internals:

(fluid '(prompt-for-buffer-command-list
	 prompt-for-existing-buffer-command-list))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creating buffers:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de buffer-create-default (buffer-name)

  % Create a new buffer with the default mode.  The name of the new buffer will
  % be the specified name if no buffer already exists with that name.
  % Otherwise, a similar name will be chosen.  The buffer becomes selectable,
  % but is not selected.

  (buffer-create buffer-name nmode-default-mode))

(de buffer-create (buffer-name initial-mode)

  % Create a new buffer.  The name of the new buffer will be the specified name
  % if no buffer already exists with that name.  Otherwise, a similar name will
  % be chosen.  The buffer becomes selectable, but is not selected.

  (setf buffer-name (buffer-make-unique-name buffer-name))
  (let ((b (buffer-create-unselectable buffer-name initial-mode)))
    (setq nmode-selectable-buffers (cons b nmode-selectable-buffers))
    b))

(de buffer-create-unselectable (buffer-name initial-mode)

  % Create a new buffer.  The name of the new buffer will be the specified
  % name.  The buffer will not be selectable.

  (let ((b (create-text-buffer buffer-name)))
    (=> b set-mode initial-mode)
    (=> b set-previous-buffer nmode-current-buffer)
    b))

(de buffer-make-unique-name (buffer-name)
  % Return a buffer name not equal to the name of any existing buffer.

  (setf buffer-name (string-upcase buffer-name))
  (for*
    (with (root-name (string-concat buffer-name "-")))
    (for count 0 (+ count 1))
    (for name buffer-name (string-concat root-name (BldMsg "%d" count)))
    (do (if (not (buffer-exists? name)) (exit name)))
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Finding buffers:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de buffer-find (buffer-name)
  % If a selectable buffer exists with the specified name (case does
  % not matter), then return it.  Otherwise, return NIL.

  (for (in b nmode-selectable-buffers)
       (do (if (string-equal buffer-name (=> b name))
	       (exit b)))
       (returns nil)
       ))

(de buffer-find-or-create (buffer-name)
  % Return the specified buffer, if it exists and is selectable.
  % Otherwise, create a buffer of that name and return it.

  (or (buffer-find buffer-name)
      (buffer-create-default buffer-name)
      ))

(de buffer-exists? (buffer-name)
  % Return T if a selectable buffer exists with the specified name
  % (case does not matter), NIL otherwise.

  (if (buffer-find buffer-name) T NIL))

(de nmode-user-buffers ()
  % Return a list of those selectable buffers whose names do not begin
  % with a '+'.

  (for (in b nmode-selectable-buffers)
       (when (~= (string-fetch (=> b name) 0) #/+))
       (collect b)
       ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Manipulating buffers:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de buffer-is-selectable? (b)
  % Return T if the specified buffer is selectable.
  (MemQ b nmode-selectable-buffers))

(de buffer-set-mode (b mode)
  % Set the "mode" of the buffer B.  If B is the current buffer, then the
  % mode is "established".

  (=> b set-mode mode)
  (when (eq b nmode-current-buffer)
	(nmode-establish-current-mode)
	(set-message "")
	))

(de cleanup-buffers ()
  % Ask each buffer to "clean up" any unneeded storage.
  (for (in b nmode-selectable-buffers)
       (do (=> b cleanup))
       ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Selecting Buffers:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de buffer-select (b)
  % If B is not NIL and B is a selectable buffer, then make it the current
  % buffer, attach it to the current window, and return it.  Otherwise, do
  % nothing and return NIL.

  (window-select-buffer nmode-current-window b))

(de buffer-select-previous (b)
  % Select the previous buffer of B, if it exists and is selectable.
  % Otherwise, select the MAIN buffer.

  (if (not (buffer-select (=> b previous-buffer)))
      (buffer-select nmode-main-buffer))
  )

(de buffer-select-by-name (buffer-name)
  % If the specified buffer exists and is selectable, select it and return it.
  % Otherwise, return NIL.

  (buffer-select (buffer-find buffer-name)))

(de buffer-select-or-create (buffer-name)
  % Select the specified buffer, if it exists and is selectable.
  % Otherwise, create a buffer of that name and select it.

  (or (buffer-select-by-name buffer-name)
      (buffer-select (buffer-create-default buffer-name))
      ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prompting for buffer names:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(setf prompt-for-buffer-command-list
  (list
   (cons (x-char SPACE) 'complete-input-buffer-name)
   (cons (x-char CR) 'check-input-buffer-name)
   (cons (x-char LF) 'check-input-buffer-name)
   ))

(setf prompt-for-existing-buffer-command-list
  (list
   (cons (x-char SPACE) 'complete-input-buffer-name)
   (cons (x-char CR) 'complete-input-existing-buffer-name)
   (cons (x-char LF) 'complete-input-existing-buffer-name)
   ))

(de prompt-for-buffer (prompt default-b)
  % Ask the user for the name of a buffer.  If the user gives a name that does
  % not name an existing buffer, a new buffer with that name will be created
  % (but NOT selected), and the prompt "(New Buffer)" will be displayed.
  % Return the buffer.  DEFAULT-B is the buffer to return as default (it may
  % be NIL).  A valid buffer will always be returned (the user may ABORT).

  (let* ((default-name (and default-b (=> default-b name)))
	 (name (prompt-for-string-special
		prompt
		default-name
		prompt-for-buffer-command-list
		))
	 )
    (or (buffer-find name)
	(prog1
	 (buffer-create-default (string-upcase name))
	 (write-prompt "(New Buffer)")
	 ))))

(de prompt-for-existing-buffer (prompt default-b)
  % Ask the user for the name of an existing buffer.  Return the buffer.
  % DEFAULT-B is the buffer to return as default (it may be NIL).  A valid
  % buffer will always be returned, unless the user aborts (throw 'ABORT).

  (let* ((default-name (and default-b (=> default-b name)))
	 (name (prompt-for-string-special
		prompt
		default-name
		prompt-for-existing-buffer-command-list
		))
	 )
    (buffer-find name)
    ))

% Internal functions:

(de complete-input-buffer-name ()
  % Extend the string in the input buffer as far as possible to match the set of
  % existing buffers.  Return T if the resulting string names an existing
  % buffer; otherwise Beep and return NIL.

  (let* ((name (nmode-get-input-string))
	 (names (buffer-names-that-match name))
	 )
    (when (not (null names))
      (setf name (strings-largest-common-prefix names))
      (nmode-replace-input-string name)
      )
    (if (member name names)
      T
      (progn (Ding) NIL)
      )))

(de check-input-buffer-name ()
  % Check the string in the input buffer to ensure that it is non-empty, or if
  % it is empty, that the default string exists and is not empty.  Beep if this
  % condition fails, otherwise terminate the input.

  (if (or (not (string-empty? (nmode-get-input-string)))
	  (and nmode-input-default
	       (not (string-empty? nmode-input-default))))
    (nmode-terminate-input)
    (Ding)
    ))

(de complete-input-existing-buffer-name ()
  % If the input buffer is empty and there is a default string, substitute the
  % default string.  Then, extend the string in the input buffer as far as
  % possible to match the set of existing buffers.  If the resulting string
  % names an existing buffer, refresh and terminate input.  Otherwise, beep.

  (nmode-substitute-default-input)
  (when (complete-input-buffer-name)
    (nmode-refresh)
    (nmode-terminate-input)
    ))

(de buffer-names-that-match (name)
  (for (in b nmode-selectable-buffers)
       (when (buffer-name-matches b name))
       (collect (=> b name))))

(de buffer-name-matches (b name2)
  (let* ((len2 (string-length name2))
	 (name1 (=> b name))
	 (len1 (string-length name1))
	 )
    (and
      (>= len1 len2)
      (string-equal (substring name1 0 len2) name2)
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Attaching buffers to windows
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de window-select-buffer (w b)
  % If B is not NIL and B is a selectable buffer, then attach B to the window
  % W and return B.  Otherwise, do nothing and return NIL.

  (cond ((and b (buffer-is-selectable? b))
	 (=> w set-buffer b)
	 (nmode-adjust-window w)
	 (cond ((eq w nmode-current-window)
		(setf nmode-current-buffer b)
		(nmode-establish-current-mode)
		(reset-message)
		))
	 b
	 )))

(de window-select-previous-buffer (w)
  % Replace window W's current buffer with that buffer's previous
  % buffer, if it exists and is selectable.  Otherwise, replace
  % it with the MAIN buffer.

  (if (not (window-select-buffer w (=> (=> w buffer) previous-buffer)))
      (window-select-buffer w nmode-main-buffer)))

(de window-copy-buffer (w-source w-dest)
  % Attach to window W-DEST the buffer belonging to window W-SOURCE.
  % Duplicate the window's BUFFER-TOP and BUFFER-LEFT as well.
  % If W is the current window, then the buffer becomes the current buffer.

  (let ((b (=> w-source buffer)))
    (=> w-dest set-buffer b)
    (=> w-dest set-buffer-top (=> w-source buffer-top))
    (=> w-dest set-buffer-left (=> w-source buffer-left))
    (cond ((eq w-dest nmode-current-window)
	   (setf nmode-current-buffer b)
	   (nmode-establish-current-mode)
	   (reset-message)
	   ))
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Killing Buffers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de window-kill-buffer ()
  % This function kills the buffer associated with the current window and
  % detaches it from that window or any other window (replacing it with
  % another buffer, preferrably the buffer's "previous buffer").
  % Do not kill the MAIN or OUTPUT buffer.

  (buffer-kill-and-detach (=> nmode-current-window buffer)))

(de buffer-kill-and-detach (b)
  % Kill the specified buffer and detach it from any existing windows
  % (replacing with another buffer, preferrably the buffer's previous buffer).
  % Do not kill the MAIN or OUTPUT buffer.

  (if (buffer-kill b)
    (for (in w (find-buffer-in-windows b))
	 (do (window-select-previous-buffer w)))))

(de buffer-killable? (b)
  (not (or (eq b nmode-main-buffer)
	   (eq b nmode-output-buffer)
	   )))

% Internal function:

(de buffer-kill (b)
  % Remove the specified buffer from the list of selectable buffers and return
  % T, unless the buffer is the MAIN or OUTPUT buffer, in which case do
  % nothing and return NIL.

  (let ((kill? (buffer-killable? b)))
    (if kill?
      (setf nmode-selectable-buffers (DelQ b nmode-selectable-buffers))
      )
    kill?
    ))
