%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Command-Input.SL - NMODE Command Input Routines
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        27 October 1982
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load extended-char fast-int))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Command Prefix Character Functions:
%
% A command prefix character function must be tagged with the property
% 'COMMAND-PREFIX.  It should also define the property 'COMMAND-PREFIX-NAME
% to be a string that will be used to print the command name of commands
% that include a prefix character that is mapped to that function.  (The
% function DEFINE-COMMAND-PREFIX is used to set these properties.)  The
% function itself should return a command (see dispatch.sl for a description).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de define-command-prefix (function-name name-string)
  (put function-name 'command-prefix T)
  (put function-name 'command-prefix-name name-string)
  )

(de prefix-name (ch)
  % Return the string to be used in printing a command with this prefix char.
  (let ((func (dispatch-table-lookup ch)))
    (or (and func (get func 'command-prefix-name))
	(string-concat (x-char-name ch) " ")
	)))

% Here we define some prefix command functions:
(define-command-prefix 'c-x-prefix "C-X ")
(define-command-prefix 'Esc-prefix "Esc-")
(define-command-prefix 'Lisp-prefix "Lisp-")
(define-command-prefix 'm-x-prefix "M-X ")

(de c-x-prefix ()
  (nmode-append-separated-prompt "C-X ")
  (let ((ch (input-terminal-character)))
    (nmode-complete-prompt (x-char-name ch))
    (list (x-char C-X) ch)
    ))

(de Esc-prefix ()
  (nmode-append-separated-prompt "Esc-")
  (let ((ch (input-extended-character)))
    (nmode-complete-prompt (x-char-name ch))
    (list (x-char ESC) ch)
    ))

(de Lisp-prefix ()
  (nmode-append-separated-prompt "Lisp-")
  (let ((ch (input-terminal-character)))
    (nmode-complete-prompt (x-char-name ch))
    (list (x-char C-!]) ch)
    ))

(de m-x-prefix ()
  (list (x-char M-X) (prompt-for-extended-command "Extended Command:")))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Command Input Functions:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de input-base-character ()
  (X-Base (input-terminal-character))
  )

(de input-command ()
  % Return either a single (extended) character or a list containing a valid
  % prefix character plus its argument (character or string).

  (let* ((ch (input-extended-character))
	 (func (dispatch-table-lookup ch))
	 )
    (if (and func (get func 'command-prefix))
	(apply func ())
	ch
	)))
