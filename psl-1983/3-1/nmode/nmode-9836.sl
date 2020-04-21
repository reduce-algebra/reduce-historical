%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% NMODE-9836.SL - HP9836 Nmode Stuff (intended only for HP9836 version)
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        20 January 1983
% Revised:     5 April 1983
%
% 5-Apr-83 Alan Snyder
%  Changes relating to keeping NMODE source and binary files in separate
%  directories.  Add NMODE-SET-TERMINAL from old set-terminal file.
%  Remove set-terminal from list of source files.
% 24-Mar-83 Alan Snyder
%  External function renamed: System-Date -> Date-and-Time.
% 15-Mar-83 Alan Snyder
%  Add browser-browser.  Implement current-date-time.
% 4-Mar-83 Alan Snyder
%  Load pathnamex.  Load nmode-aids (instead of lapin).
% 15-Feb-83 Alan Snyder
%  No longer sets NMODE-AUTO-START (inconsistent with other systems).  Add new
%  online documentation stuff.
% 7-Feb-83 Alan Snyder
%  Load browser.
% 31-Jan-83 Alan Snyder
%  Add softkey stuff, keyboard mapping stuff, load window-label.  Redefine
%  PasFiler and PasEditor to refresh the screen upon exit, if NMODE was
%  running.
% 25-Jan-83 Alan Snyder
%  Added dummy version of current-date-time function; load M-XCMD and REC.
% 21-Jan-83 Alan Snyder
%  Load more stuff.  Change INIT to return NIL.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(compiletime (load fast-strings fast-int extended-char))
(bothtimes (load strings common))

(fluid '(alpha-terminal
	 color-terminal
	 nmode-file-list
	 nmode-source-prefix
	 nmode-binary-prefix
	 *quiet_faslout
	 *usermode
	 *redefmsg
	 installkeys-address
	 uninstallkeys-address
	 nmode-softkey-label-screen-height
	 nmode-softkey-label-screen-width
	 doc-text-file
	 reference-text-file
	 ))

(if (or (unboundp 'nmode-source-prefix) (null nmode-source-prefix))
  (setf nmode-source-prefix "pn:"))

(if (or (unboundp 'nmode-binary-prefix) (null nmode-binary-prefix))
  (setf nmode-binary-prefix "pnb:"))

(if (funboundp 'pre-nmode-main)
  (copyd 'pre-nmode-main 'main))

(if (funboundp 'pre-nmode-pasfiler)
  (copyd 'pre-nmode-pasfiler 'pasfiler))

(if (funboundp 'pre-nmode-paseditor)
  (copyd 'pre-nmode-paseditor 'paseditor))

(setf installkeys-address (system-address "NMODEKEYS_INSTALL_KEYMAP"))
(setf uninstallkeys-address (system-address "NMODEKEYS_UNINSTALL_KEYMAP"))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 9836 Customization:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-9836-init ()
  % This function modifies "standard" NMODE for use on the 9836.
  (let ((*usermode nil) (*redefmsg nil))
    (copyd 'nmode-initialize 'original-nmode-initialize)
    (copyd 'actualize-file-name '9836-actualize-file-name)
    )
  (original-nmode-initialize)
  (add-to-command-list 'basic-command-list (x-chars C-X C-Z) 'exit-nmode)
  (nmode-establish-current-mode)
  (setf alpha-terminal nmode-terminal)
  (setf color-terminal (make-instance '9836-color))
  nil % for looks
  )

(de nmode-set-terminal ()
  (or nmode-terminal (ensure-terminal-type '9836-alpha))
  (or nmode-other-terminal (ensure-other-terminal-type '9836-color))
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Useful Functions for Compiling:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de load-nmode ()
  % Load NMODE.
  % Any system-dependent customization is done here so that it can
  % be overrided by the user before nmode is initialized.
  (nmode-load-required-modules)
  (nmode-load-all)
  (setf nmode-softkey-label-screen-height 2) % two rows
  (setf nmode-softkey-label-screen-width 5) % of five keys each
  (setf doc-text-file "psl:nmode.frames")
  (setf reference-text-file "psl:nmode.xref")
  (let ((*usermode nil) (*redefmsg nil))
    (if (funboundp 'original-nmode-initialize)
      (copyd 'original-nmode-initialize 'nmode-initialize))
    (copyd 'nmode-initialize 'nmode-9836-init)
    ))

(de compile-lisp-file (source-name object-name)
  (let ((*quiet_faslout T))
    (if (not (filep source-name))
      (printf "Unable to open source file: %w%n" source-name)
      % else
      (printf "%n----- Compiling %w to %w%n"
	      source-name (string-concat object-name ".b"))
      (faslout object-name)
      (unwind-protect
       (dskin source-name)
       (faslend)
       )
      (printf "%n----------------------------------------------------------%n")
      )))

(de file-compile (s)
  (let ((object-name s)
	(source-name (string-concat s ".sl"))
	)
    (compile-lisp-file source-name object-name)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% System-Dependent Stuff:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de current-date-time () (date-and-time))

(de 9836-actualize-file-name (fn) fn)

(de nmode-use-color ()
  % Use the COLOR screen (only).
  (setf nmode-terminal color-terminal)
  (nmode-new-terminal)
  )

(de nmode-use-alpha ()
  % Use the ALPHA screen as the primary screen.
  (setf nmode-terminal alpha-terminal)
  (nmode-new-terminal)
  )

(de install-nmode-keymap ()
  (setf nmode-meta-bit-prefix-character (x-char ^!\))
  (lpcall0 installkeys-address)
  )

(de uninstall-nmode-keymap ()
  (setf nmode-meta-bit-prefix-character (x-char ^![))
  (lpcall0 uninstallkeys-address)
  )

(de pasfiler ()
  (pre-nmode-pasfiler)
  (if *NMODE-RUNNING (nmode-full-refresh))
  )

(de paseditor ()
  (pre-nmode-paseditor)
  (if *NMODE-RUNNING (nmode-full-refresh))
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Stuff for Building NMODE:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-load-required-modules ()
  (load addr2id)
  (load objects)
  (load common)
  (load useful)
  (load strings)
  (load pathnames)
  (load pathnamex)
  (load ring-buffer)
  (load extended-char)
  (load directory)
  (load input-stream)
  (load output-stream)
  (load processor-time)
  (load wait)
  (load vector-fix)
  (load nmode-parsing)
  (load windows)
  (load nmode-aids)
  )

(de nmode-fixup-name (s) s)

(de nmode-load-all ()
  (for (in s nmode-file-list)
       (do (nmode-load s))
       ))

(de nmode-load (s)
  (nmode-faslin nmode-binary-prefix s)
  )

(de nmode-faslin (directory-name module-name)
  (setf module-name (nmode-fixup-name module-name))
  (setf module-name (string-concat module-name ".b"))
  (let ((object-name (string-concat directory-name module-name)))
    (if (filep object-name)
      (faslin object-name)
      (continuableerror 99
       (bldmsg "Unable to FASLIN %w" object-name)
       (list 'faslin object-name)
       ))))

(setf nmode-file-list
  (list
   "browser"
   "browser-support"
   "buffer"
   "buffer-io"
   "buffer-position"
   "buffer-window"
   "buffers"
   "case-commands"
   "command-input"
   "commands"
   "defun-commands"
   "dispatch"
   "extended-input"
   "fileio"
   "incr"
   "indent-commands"
   "kill-commands"
   "lisp-commands"
   "lisp-indenting"
   "lisp-interface"
   "lisp-parser"
   "m-x"
   "m-xcmd"
   "modes"
   "mode-defs"
   "move-commands"
   "nmode-break"
   "nmode-init"
   "prompting"
   "query-replace"
   "reader"
   "rec"
   "screen-layout"
   "search"
   "softkeys"
   "structure-functions"
   "terminal-input"
   "text-buffer"
   "text-commands"
   "window"
   "window-label"

   % These must be last:

   "autofill"
   "browser-browser"
   "buffer-browser"
   "dired"
   "doc"
   ))

