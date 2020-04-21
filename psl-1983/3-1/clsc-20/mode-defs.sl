%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% MODE-DEFS.SL - NMODE Command Table and Mode Definitions
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        14 September 1982
% Revised:     15 March 1983
%
% 15-Mar-83 Alan Snyder
%  Add M-X List Browsers, M-X Print Buffer, C-X C-P.  Define modes at load
%  time.  Rename write-screen-photo-command to write-screen-command; change to
%  M-X Write Screen (instead of C-X P).
% 18-Feb-83 Alan Snyder
%  Rename down-list and insert-parens.  Add M-) command.
% 9-Feb-83 Alan Snyder
%  Add Esc-_ (Help), temporarily attached to M-X Apropos.
%  Move some M-X commands into text-command-list.
% 2-Feb-83 Alan Snyder
%  Add Lisp-D.
% 26-Jan-83 Alan Snyder
%  Add Esc-/.
% 25-Jan-83 Alan Snyder
%  Created Window-Command-List to allow scrolling in Recurse mode.
%  Removed modifying text commands from Recurse mode.
% 24-Jan-83 Jeffrey Soreff
%  Added definition of Recurse-Mode
%  Defined M-X commands: Delete Matching Lines, Flush Lines,
%  Delete Non-Matching Lines, Keep Lines, How Many, Count Occurrences,
%  Set Key, Set Visited Filename, Rename Buffer, Kill Some Buffers,
%  Insert Date, Revert File
% 5-Jan-83 Alan Snyder
%  Revised definition of input mode, C-S, and C-R.
% 3-Dec-82 Alan Snyder
%  New definitions for ) and ] in Lisp mode.
%  New definitions for C-M-(, C-M-), C-M-U, C-M-N, and C-M-P.
%  New definitions for C-M-A, C-M-[, and C-M-R.
%  Define C-M-\ (Indent Region) in Lisp mode and Text mode.
%  Define C-? same as M-?, C-( same as C-M-(, C-) same as C-M-).
%  Lisp Mode establishes Lisp Parser.
%  Define C-M-C.
%  Define the text commands: C-=, C-X =, M-A, M-E, M-K, C-X Rubout, M-Z, M-Q,
%  M-G, M-H, M-], M-[, M-S.
%  Fix definitions of digits and hyphen: inserting definition goes on
%  text-command-list (where insertion commands go).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (CompileTime (load objects))
(CompileTime (load extended-char))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% External variables:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(nmode-default-mode
	 nmode-current-buffer
	 nmode-input-special-command-list
	 ))

% Mode definitions:

(fluid '(Lisp-Interface-Mode
	 Text-Mode
	 Basic-Mode
	 Read-Only-Text-Mode
	 Input-Mode
	 Recurse-Mode
	 ))

% Command lists:

(fluid '(Input-Command-List
	 Read-Only-Text-Command-List
	 Text-Command-List
	 Rlisp-Command-List
	 Lisp-Command-List
	 Read-Only-Terminal-Command-List
	 Modifying-Terminal-Command-List
	 Window-Command-List
	 Basic-Command-List
	 Essential-Command-List
	 Recurse-Command-List
	 ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Mode Definitions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(setf Basic-Mode
  (nmode-define-mode
   "Basic"
   '((nmode-define-commands Basic-Command-List)
     (nmode-define-commands Read-Only-Terminal-Command-List)
     (nmode-define-commands Window-Command-List)
     (nmode-define-commands Essential-Command-List)
     )))

(setf Read-Only-Text-Mode
  (nmode-define-mode
   "Read-Only-Text"
   '((nmode-define-commands Read-Only-Text-Command-List)
     (nmode-establish-mode Basic-Mode)
     )))

(setf Text-Mode
  (nmode-define-mode
   "Text"
   '((nmode-define-commands Text-Command-List)
     (nmode-define-commands Modifying-Terminal-Command-List)
     (nmode-establish-mode Read-Only-Text-Mode)
     (nmode-define-normal-self-inserts)
     )))

(setf Lisp-Interface-Mode
  (nmode-define-mode
   "Lisp"
   '((nmode-define-commands Rlisp-Command-List)
     (establish-lisp-parser)
     (nmode-define-commands Lisp-Command-List)
     (nmode-establish-mode Text-Mode)
     )))

(setf Input-Mode
  (nmode-define-mode
   "Input"
   '((nmode-define-commands nmode-input-special-command-list)
     (nmode-define-command (x-char CR) 'nmode-terminate-input)
     (nmode-define-command (x-char LF) 'nmode-terminate-input)
     (nmode-define-commands Input-Command-List)
     (nmode-define-commands Text-Command-List)
     (nmode-define-commands Read-Only-Text-Command-List)
     (nmode-define-commands Read-Only-Terminal-Command-List)
     (nmode-define-commands Essential-Command-List)
     (nmode-define-normal-self-inserts)
     )))

(setf Recurse-Mode
  (nmode-define-mode
   "Recurse"
   '((nmode-define-commands Read-Only-Text-Command-List)
     (nmode-define-commands Read-Only-Terminal-Command-List)
     (nmode-define-commands Window-Command-List)
     (nmode-define-commands Essential-Command-List)
     (nmode-define-commands Recurse-Command-List)
     )))

(setf nmode-default-mode Text-Mode)

(de nmode-initialize-modes ()
  % Define initial set of file modes.
  (nmode-declare-file-mode "txt"   Text-Mode)
  (nmode-declare-file-mode "red"   Lisp-Interface-Mode)
  (nmode-declare-file-mode "sl"    Lisp-Interface-Mode)
  (nmode-declare-file-mode "lsp"   Lisp-Interface-Mode)
  (nmode-declare-file-mode "lap"   Lisp-Interface-Mode)
  (nmode-declare-file-mode "build" Lisp-Interface-Mode)
  )

(de lisp-mode-command ()
  (buffer-set-mode nmode-current-buffer Lisp-Interface-Mode)
  )

(de text-mode-command ()
  (buffer-set-mode nmode-current-buffer Text-Mode)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Command Lists:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Rlisp-Command-List - commands related to the LISP interface

(setf Rlisp-Command-List
  (list
   (cons (x-char C-!])			'Lisp-prefix)
   (cons (x-chars C-!] !?)		'lisp-help-command)
   (cons (x-chars C-!] A)		'lisp-abort-command)
   (cons (x-chars C-!] B)		'lisp-backtrace-command)
   (cons (x-chars C-!] C)		'lisp-continue-command)
   (cons (x-chars C-!] D)		'execute-defun-command)
   (cons (x-chars C-!] E)		'execute-form-command)
   (cons (x-chars C-!] L)		'exit-nmode)
   (cons (x-chars C-!] Q)		'lisp-quit-command)
   (cons (x-chars C-!] R)		'lisp-retry-command)
   (cons (x-chars C-!] Y)		'yank-last-output-command)
   ))

% Lisp-Command-List - commands related to editing LISP text

(setf Lisp-Command-List
  (list
   (cons (x-char !))			'insert-closing-bracket)
   (cons (x-char !])			'insert-closing-bracket)
   (cons (x-char C-!()			'backward-up-list-command)
   (cons (x-char C-!))			'forward-up-list-command)
   (cons (x-char C-M-!()		'backward-up-list-command)
   (cons (x-char C-M-!))		'forward-up-list-command)
   (cons (x-char C-M-![)		'move-backward-defun-command)
   (cons (x-char C-M-!])		'end-of-defun-command)
   (cons (x-char C-M-!\)		'lisp-indent-region-command)
   (cons (x-char C-M-@)			'mark-form-command)
   (cons (x-char C-M-A)			'move-backward-defun-command)
   (cons (x-char C-M-B)			'move-backward-form-command)
   (cons (x-char C-M-BACKSPACE)		'mark-defun-command)
   (cons (x-char C-M-D)			'down-list-command)
   (cons (x-char C-M-E)			'end-of-defun-command)
   (cons (x-char C-M-F)			'move-forward-form-command)
   (cons (x-char C-M-H)			'mark-defun-command)
   (cons (x-char C-M-I)			'lisp-tab-command)
   (cons (x-char C-M-K)			'kill-forward-form-command)
   (cons (x-char C-M-N)			'move-forward-list-command)
   (cons (x-char C-M-P)			'move-backward-list-command)
   (cons (x-char C-M-Q)			'lisp-indent-sexpr)
   (cons (x-char C-M-R)			'reposition-window-command)
   (cons (x-char C-M-RUBOUT)		'kill-backward-form-command)
   (cons (x-char C-M-T)			'transpose-forms)
   (cons (x-char C-M-TAB)		'lisp-tab-command)
   (cons (x-char C-M-U)			'backward-up-list-command)
   (cons (x-char M-!;)			'insert-comment-command)
   (cons (x-char M-BACKSPACE)		'mark-defun-command)
   (cons (x-char M-!()			'make-parens-command)
   (cons (x-char M-!))			'move-over-paren-command)
   (cons (x-char RUBOUT)		'delete-backward-hacking-tabs-command)
   (cons (x-char TAB)			'lisp-tab-command)
   ))

% Essential-Command-List: the most essential commands

(setf Essential-Command-List
  (list
   (cons (x-char C-X)			'c-x-prefix)
   (cons (x-char ESC)			'Esc-prefix)
   (cons (x-char M-X)			'm-x-prefix)
   (cons (x-char C-M-X)			'm-x-prefix)
   (cons (x-char C-G)			'nmode-abort-command)
   (cons (x-char C-L)			'nmode-refresh-command)
   (cons (x-char C-U)			'universal-argument)
   (cons (x-char 0)			'argument-digit)
   (cons (x-char 1)			'argument-digit)
   (cons (x-char 2)			'argument-digit)
   (cons (x-char 3)			'argument-digit)
   (cons (x-char 4)			'argument-digit)
   (cons (x-char 5)			'argument-digit)
   (cons (x-char 6)			'argument-digit)
   (cons (x-char 7)			'argument-digit)
   (cons (x-char 8)			'argument-digit)
   (cons (x-char 9)			'argument-digit)
   (cons (x-char -)			'negative-argument)
   (cons (x-char C-0)			'argument-digit)
   (cons (x-char C-1)			'argument-digit)
   (cons (x-char C-2)			'argument-digit)
   (cons (x-char C-3)			'argument-digit)
   (cons (x-char C-4)			'argument-digit)
   (cons (x-char C-5)			'argument-digit)
   (cons (x-char C-6)			'argument-digit)
   (cons (x-char C-7)			'argument-digit)
   (cons (x-char C-8)			'argument-digit)
   (cons (x-char C-9)			'argument-digit)
   (cons (x-char C--)			'negative-argument)
   (cons (x-char M-0)			'argument-digit)
   (cons (x-char M-1)			'argument-digit)
   (cons (x-char M-2)			'argument-digit)
   (cons (x-char M-3)			'argument-digit)
   (cons (x-char M-4)			'argument-digit)
   (cons (x-char M-5)			'argument-digit)
   (cons (x-char M-6)			'argument-digit)
   (cons (x-char M-7)			'argument-digit)
   (cons (x-char M-8)			'argument-digit)
   (cons (x-char M-9)			'argument-digit)
   (cons (x-char M--)			'negative-argument)
   (cons (x-char C-M-0)			'argument-digit)
   (cons (x-char C-M-1)			'argument-digit)
   (cons (x-char C-M-2)			'argument-digit)
   (cons (x-char C-M-3)			'argument-digit)
   (cons (x-char C-M-4)			'argument-digit)
   (cons (x-char C-M-5)			'argument-digit)
   (cons (x-char C-M-6)			'argument-digit)
   (cons (x-char C-M-7)			'argument-digit)
   (cons (x-char C-M-8)			'argument-digit)
   (cons (x-char C-M-9)			'argument-digit)
   (cons (x-char C-M--)			'negative-argument)
   (cons (x-chars C-X C-Z)		'nmode-exit-to-superior)
   (cons (x-chars C-X V)		'nmode-invert-video)
   (cons (x-chars Esc !/)		'execute-softkey-command)
   ))

% Window-Command-List: commands for scrolling, etc.
% These commands do not allow selecting a new window, buffer, mode, etc.

(setf Window-Command-List
  (list
   (cons (x-char C-M-V)			'scroll-other-window-command)
   (cons (x-char C-V)			'next-screen-command)
   (cons (x-char M-R)			'move-to-screen-edge-command)
   (cons (x-char M-V)			'previous-screen-command)
   (cons (x-chars C-X <)		'scroll-window-left-command)
   (cons (x-chars C-X >)		'scroll-window-right-command)
   (cons (x-chars C-X ^)		'grow-window-command)
   (cons (m-x "Write Screen")		'write-screen-command)
   ))

% Basic-Command-List: contains commands desirable in almost any mode.

(setf Basic-Command-List
  (list
   (cons (x-char C-!?)			'help-dispatch)
   (cons (x-char C-M-L)			'select-previous-buffer-command)
   (cons (x-char M-!/)			'help-dispatch)
   (cons (x-char M-!?)			'help-dispatch)
   (cons (x-char M-!~)			'buffer-not-modified-command)
   (cons (x-chars C-X !.)		'set-fill-prefix-command)
   (cons (x-chars C-X 1)		'one-window-command)
   (cons (x-chars C-X 2)		'two-windows-command)
   (cons (x-chars C-X 3)		'view-two-windows-command)
   (cons (x-chars C-X 4)		'visit-in-other-window-command)
   (cons (x-chars C-X B)		'select-buffer-command)
   (cons (x-chars C-X C-B)		'buffer-browser-command)
   (cons (x-chars C-X C-F)		'find-file-command)
   (cons (x-chars C-X C-P)		'print-buffer-command)
   (cons (x-chars C-X C-S)		'save-file-command)
   (cons (x-chars C-X C-W)		'write-file-command) % here???
   (cons (x-chars C-X D)		'dired-command)
   (cons (x-chars C-X E)		'exchange-windows-command)
   (cons (x-chars C-X F)		'set-fill-column-command)
   (cons (x-chars C-X K)		'kill-buffer-command)
   (cons (x-chars C-X O)		'other-window-command)
   (cons (x-chars Esc _)		'apropos-command)
   (cons (m-x "Append to File")		'append-to-file-command)
   (cons (m-x "Apropos")		'apropos-command)
   (cons (m-x "Auto Fill Mode")		'auto-fill-mode-command)
   (cons (m-x "Count Occurrences")      'Count-Occurrences-command)
   (cons (m-x "Delete and Expunge File") 'delete-and-expunge-file-command)
   (cons (m-x "Delete File")		'delete-file-command)
   (cons (m-x "DIRED")			'edit-directory-command)
   (cons (m-x "Edit Directory")		'edit-directory-command)
   (cons (m-x "Execute Buffer")		'execute-buffer-command)
   (cons (m-x "Execute File")		'execute-file-command)
   (cons (m-x "Find File")		'find-file-command)
   (cons (m-x "How Many")               'Count-Occurrences-command)
   (cons (m-x "Kill Buffer")		'kill-buffer-command)
   (cons (m-x "Kill File")		'delete-file-command)
   (cons (m-x "Kill Some Buffers")      'kill-some-buffers-command)
   (cons (m-x "List Browsers")		'browser-browser-command)
   (cons (m-x "List Buffers")		'buffer-browser-command)
   (cons (m-x "Make Space")		'nmode-gc)
   (cons (m-x "Prepend to File")	'prepend-to-file-command)
   (cons (m-x "Print Buffer")		'print-buffer-command)
   (cons (m-x "Rename Buffer")          'rename-buffer-command)
   (cons (m-x "Save All Files")		'save-all-files-command)
   (cons (m-x "Select Buffer")		'select-buffer-command)
   (cons (m-x "Set Key")                'set-key-command)
   (cons (m-x "Set Visited Filename")   'set-visited-filename-command)
   (cons (m-x "Start Scripting")	'start-scripting-command)
   (cons (m-x "Start Timing NMODE")	'start-timing-command)
   (cons (m-x "Stop Scripting")		'stop-scripting-command)
   (cons (m-x "Stop Timing NMODE")	'stop-timing-command)
   (cons (m-x "Undelete File")		'undelete-file-command)
   (cons (m-x "Write File")		'write-file-command) % here???
   (cons (m-x "Write Region")		'write-region-command)
   ))

% Read-Only-Text-Command-List: Commands for editing text buffers that
% do not modify the buffer.

(setf Read-Only-Text-Command-List
  (list
   % These commands are read-only commands for text mode.
   (cons (x-char BACKSPACE)		'move-backward-character-command)
   (cons (x-char C-<)			'mark-beginning-command)
   (cons (x-char C->)			'mark-end-command)
   (cons (x-char C-=)			'what-cursor-position-command)
   (cons (x-char C-@)			'set-mark-command)
   (cons (x-char C-A)			'move-to-start-of-line-command)
   (cons (x-char C-B)			'move-backward-character-command)
   (cons (x-char C-E)			'move-to-end-of-line-command)
   (cons (x-char C-F)			'move-forward-character-command)
   (cons (x-char C-M-M)			'back-to-indentation-command)
   (cons (x-char C-M-RETURN)		'back-to-indentation-command)
   (cons (x-char C-M-W)			'append-next-kill-command)
   (cons (x-char C-N)			'move-down-command)
   (cons (x-char C-P)			'move-up-command)
   (cons (x-char C-R)			'reverse-search-command)
   (cons (x-char C-S)			'incremental-search-command)
   (cons (x-char C-SPACE)		'set-mark-command)
   (cons (x-char M-<)			'move-to-buffer-start-command)
   (cons (x-char M->)			'move-to-buffer-end-command)
   (cons (x-char M-![)			'backward-paragraph-command)
   (cons (x-char M-!])			'forward-paragraph-command)
   (cons (x-char M-@)			'mark-word-command)
   (cons (x-char M-A)			'backward-sentence-command)
   (cons (x-char M-B)			'move-backward-word-command)
   (cons (x-char M-E)			'forward-sentence-command)
   (cons (x-char M-F)			'move-forward-word-command)
   (cons (x-char M-H)			'mark-paragraph-command)
   (cons (x-char M-M)			'back-to-indentation-command)
   (cons (x-char M-RETURN)		'back-to-indentation-command)
   (cons (x-char M-W)			'copy-region)
   (cons (x-chars C-X A)		'append-to-buffer-command)
   (cons (x-chars C-X C-N)		'set-goal-column-command)
   (cons (x-chars C-X C-X)		'exchange-point-and-mark)
   (cons (x-chars C-X H)		'mark-whole-buffer-command)
   (cons (x-chars C-X =)		'what-cursor-position-command)
   ))

% Text-Command-List: Commands for editing text buffers that might modify
% the buffer.  Note: put read-only commands on
% Read-Only-Text-Command-List (above).

(setf Text-Command-List
  (list
   (cons (x-char 0)			'argument-or-insert-command)
   (cons (x-char 1)			'argument-or-insert-command)
   (cons (x-char 2)			'argument-or-insert-command)
   (cons (x-char 3)			'argument-or-insert-command)
   (cons (x-char 4)			'argument-or-insert-command)
   (cons (x-char 5)			'argument-or-insert-command)
   (cons (x-char 6)			'argument-or-insert-command)
   (cons (x-char 7)			'argument-or-insert-command)
   (cons (x-char 8)			'argument-or-insert-command)
   (cons (x-char 9)			'argument-or-insert-command)
   (cons (x-char -)			'argument-or-insert-command)
   (cons (x-char C-!%)			'replace-string-command)
   (cons (x-char C-D)			'delete-forward-character-command)
   (cons (x-char C-K)			'kill-line)
   (cons (x-char C-M-C)			'insert-self-command)
   (cons (x-char C-M-O)			'split-line-command)
   (cons (x-char C-M-!\)		'indent-region-command)
   (cons (x-char C-N)			'move-down-extending-command)
   (cons (x-char C-O)			'open-line-command)
   (cons (x-char C-Q)			'insert-next-character-command)
   (cons (x-char C-RUBOUT)		'delete-backward-hacking-tabs-command)
   (cons (x-char C-T)			'transpose-characters-command)
   (cons (x-char C-W)			'kill-region)
   (cons (x-char C-Y)			'insert-kill-buffer)
   (cons (x-char LF)			'indent-new-line-command)
   (cons (x-char M-!')			'upcase-digit-command)
   (cons (x-char M-!%)			'query-replace-command)
   (cons (x-char M-!\)			'delete-horizontal-space-command)
   (cons (x-char M-C)			'uppercase-initial-command)
   (cons (x-char M-D)			'kill-forward-word-command)
   (cons (x-char M-G)			'fill-region-command)
   (cons (x-char M-I)			'tab-to-tab-stop-command)
   (cons (x-char M-K)			'kill-sentence-command)
   (cons (x-char M-L)			'lowercase-word-command)
   (cons (x-char M-Q)			'fill-paragraph-command)
   (cons (x-char M-RUBOUT)		'kill-backward-word-command)
   (cons (x-char M-S)			'center-line-command)
   (cons (x-char M-T)			'transpose-words)
   (cons (x-char M-TAB)			'tab-to-tab-stop-command)
   (cons (x-char M-U)			'uppercase-word-command)
   (cons (x-char M-Y)			'unkill-previous)
   (cons (x-char M-Z)			'fill-comment-command)
   (cons (x-char M-^)			'delete-indentation-command)
   (cons (x-char RETURN)		'return-command)
   (cons (x-char RUBOUT)		'delete-backward-character-command)
   (cons (x-char TAB)			'tab-to-tab-stop-command)
   (cons (x-chars C-X C-L)		'lowercase-region-command)
   (cons (x-chars C-X C-O)		'delete-blank-lines-command)
   (cons (x-chars C-X C-T)		'transpose-lines)
   (cons (x-chars C-X C-U)		'uppercase-region-command)
   (cons (x-chars C-X C-V)		'visit-file-command)
   (cons (x-chars C-X G)		'get-register-command)
   (cons (x-chars C-X Rubout)		'backward-kill-sentence-command)
   (cons (x-chars C-X T)		'transpose-regions)
   (cons (x-chars C-X X)		'put-register-command)
   (cons (m-x "Delete Matching Lines")  'delete-matching-lines-command)
   (cons (m-x "Delete Non-Matching Lines") 'delete-non-matching-lines-command)
   (cons (m-x "Flush Lines")            'delete-matching-lines-command)
   (cons (m-x "Insert Buffer")		'insert-buffer-command)
   (cons (m-x "Insert Date")            'insert-date-command)
   (cons (m-x "Insert File")		'insert-file-command)
   (cons (m-x "Keep Lines")             'delete-non-matching-lines-command)
   (cons (m-x "Lisp Mode")		'lisp-mode-command)
   (cons (m-x "Replace String")		'replace-string-command)
   (cons (m-x "Query Replace")		'query-replace-command)
   (cons (m-x "Revert File")            'revert-file-command)
   (cons (m-x "Text Mode")		'text-mode-command)
   (cons (m-x "Visit File")		'visit-file-command)
   ))

(setf Read-Only-Terminal-Command-List
  (list
   (cons (x-chars ESC !h)		'move-to-buffer-start-command)
   (cons (x-chars ESC 4)		'move-backward-word-command)
   (cons (x-chars ESC 5)		'move-forward-word-command)
   (cons (x-chars ESC A)		'move-up-command)
   (cons (x-chars ESC B)		'move-down-command)
   (cons (x-chars ESC C)		'move-forward-character-command)
   (cons (x-chars ESC D)		'move-backward-character-command)
   (cons (x-chars ESC F)		'move-to-buffer-end-command)
   (cons (x-chars ESC J)		'nmode-full-refresh)
   (cons (x-chars ESC S)		'scroll-window-up-line-command)
   (cons (x-chars ESC T)		'scroll-window-down-line-command)
   (cons (x-chars ESC U)		'scroll-window-up-page-command)
   (cons (x-chars ESC V)		'scroll-window-down-page-command)
   ))

(setf Modifying-Terminal-Command-List
  (list
   (cons (x-chars ESC L)		'open-line-command)
   (cons (x-chars ESC M)		'kill-line)
   (cons (x-chars ESC P)		'delete-forward-character-command)
   ))

(setf Input-Command-List
  (list
   (cons (x-char C-R)			'nmode-yank-default-input)
   ))

(setf Recurse-Command-List
  (list
   (cons (x-char y)                     'affirmative-exit)
   (cons (x-char n)                     'negative-exit)
   ))
