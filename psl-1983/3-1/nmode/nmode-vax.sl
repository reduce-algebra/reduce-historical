%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% NMODE-VAX.SL  Vax-Unix specific loading and modifications for NMODE.
% 
% Author:      William F. Galway
%              University of Utah
% Date:        28 March 1983
% Revised:     5 April 1983
%
% 7-Apr-83 Nancy Kendzierski
%  Added knowledge about hp and 2641 terminal types to table.
% 5-Apr-83 Alan Snyder
%  Revised to be more like the 9836 code: add load-nmode stuff and set-terminal
%  stuff.
%
% This file contains functions to load NMODE and make some final changes to
% customize things for Vax-Unix.  Some modules for NMODE are unimplemented on
% the Vax, thus not loaded for now; these are commented out with a "%*".
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(compiletime
  (load useful common fast-strings))


% External variables used here:

(fluid '(nmode-file-list
	 nmode-source-prefix
	 nmode-binary-prefix
	 *usermode
	 *redefmsg
	 doc-text-file
	 reference-text-file
	 nmode-terminal
	 ))

% Global variables defined here:

(fluid
  '(
    % Association list of (Unix-TERM-name . NMODE-terminal-name).  The
    % Unix-TERM-name is a string, the NMODE-terminal-name is an identifier.
    term-name-table
    ))

(setf term-name-table
  '(
%    ("t10" . teleray)
%    ("aaa" . ambassador)
    ("hp" . hp2648a)
    ("2621" . hp2648a)
    ("vt52" . vt52x)))

(if (or (unboundp 'nmode-source-prefix) (null nmode-source-prefix))
  (setf nmode-source-prefix "$pn/"))

(if (or (unboundp 'nmode-binary-prefix) (null nmode-binary-prefix))
  (setf nmode-binary-prefix "$pnb/"))

(if (funboundp 'pre-nmode-main)
  (copyd 'pre-nmode-main 'main))

(de load-nmode ()
  % Load NMODE.
  % Any system-dependent customization is done here so that it can
  % be overrided by the user before nmode is initialized.
  (nmode-load-required-modules)
  (nmode-load-all)
  % Set up "pointers" to online documentation.
  (setf doc-text-file "$pn/ONLINE-DOCS/frames.lpt")
  (setf reference-text-file "$pn/ONLINE-DOCS/costly.sl")
  (let ((*usermode nil) (*redefmsg nil))
    (copyd 'actualize-file-name 'vax-actualize-file-name)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Terminal Selection Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-set-terminal ()
  % Needs better error handling?
  (let* (
	 % Get terminal name from the system.
 	 (system-term-type (GetEnv "TERM"))
	 % Map to NMODE name.
	 (table-entry
	   (assoc system-term-type term-name-table))
	 (terminal-type
	   (cond
	     (table-entry
	       (cdr table-entry))
	     (T
	       (StdError
		 (BldMsg "%r is unsupported terminal type" system-term-type))
	       ))))

  (ensure-terminal-type terminal-type)))


% These functions defined for compatibility:

(de hp2648a () (ensure-terminal-type 'hp2648a))
(de vt52x () (ensure-terminal-type 'vt52x))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% System-Dependent Stuff:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de vax-actualize-file-name (file-name)
  (cond
    % If the file-name starts with a character that's "special" to
    % Unix, just pass it on through.
    ((MemQ (string-fetch file-name 0) '(#// #/~ #/$))
      file-name)
    (T
      % Otherwise, tack the current working directory onto the front
      % of the name.
      (string-concat (pwd) file-name))))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Stuff for Building NMODE:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-load-required-modules ()
  (load objects)
  (load common)
  (load useful)
  (load strings)
  (load pathnames)
  (load pathnamex)
  (load ring-buffer)
  (load extended-char)
  %* (load directory)
  (load input-stream)
  (load output-stream)
  %* (load processor-time)
  (load wait)
  (load vector-fix)
  (load nmode-parsing)
  (load windows)
  (load rawio)
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
   %* "dired"
   "doc"
   ))
