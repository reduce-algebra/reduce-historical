%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% NMODE-20.SL - DEC-20 NMODE Stuff (intended for DEC-20 Version Only)
%
% Author:	Jeffrey Soreff
%		Hewlett-Packard/CRC
% Date:		24 January 1983
% Revised:      5 April 1983
%
% 5-Apr-83 Alan Snyder
%  Add load-nmode and set-terminal stuff to make it more like other systems.
% 15-Mar-83 Alan Snyder
%  Add nmode-print-device.
% 25-Jan-83 Alan Snyder
%  Add version of actualize-file-name that ensures that transiently-created
%  file has delete access.
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
	 nmode-print-device
	 nmode-terminal
	 ))

% Global variables defined here:

(fluid '(terminal-type))


(if (or (unboundp 'nmode-source-prefix) (null nmode-source-prefix))
  (setf nmode-source-prefix "pn:"))

(if (or (unboundp 'nmode-binary-prefix) (null nmode-binary-prefix))
  (setf nmode-binary-prefix "pnb:"))

(de load-nmode ()
  % Load NMODE.
  % Any system-dependent customization is done here so that it can
  % be overridden by the user before NMODE is initialized.

  (nmode-load-required-modules)
  (nmode-load-all)
  (setf nmode-print-device "LPT:")
  % Set up "pointers" to online documentation.
  (setf doc-text-file "SS:<PSL.NMODE-DOC>FRAMES.LPT")
  (setf reference-text-file "SS:<PSL.NMODE-DOC>COSTLY.SL")
  (let ((*usermode nil) (*redefmsg nil))
    (copyd 'actualize-file-name 'dec20-actualize-file-name)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Terminal Selection Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de nmode-set-terminal ()
  (setf terminal-type (jsys2 65 0 0 0 (const jsgttyp)))
  (selectq terminal-type
    (21 % HP2621
     (ensure-terminal-type 'hp2648a)
     )
    (6 % HP264X
     (ensure-terminal-type 'hp2648a)
     )
    (15 % VT52
     (ensure-terminal-type 'vt52x)
     )
    (t
     (or nmode-terminal (ensure-terminal-type 'hp2648a))
     )
    ))


% These functions defined for compatibility:

(de hp2648a () (ensure-terminal-type 'hp2648a))
(de vt52x () (ensure-terminal-type 'vt52x))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% System-Dependent Stuff:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de current-date-time () % Stolen directly from Nancy Kendzierski
  % Date/time in appropriate format for the network mail header
  (let ((date-time (MkString 80)))
    (jsys1 date-time -1 #.(bits 5 7 10 12 13) 0 (const jsODTIM))
    (recopystringtonull date-time)))

(de dec20-actualize-file-name (file-name)
  % If the specified file exists, return its "true" (and complete) name.
  % Otherwise, return the "true" name of the file that would be created if one
  % were to do so.  (Unfortunately, we have no way to do this except by actually
  % creating the file and then deleting it!)  Return NIL if the file cannot be
  % read or created.

  (let ((s (attempt-to-open-input file-name)))
    (cond ((not s)
	   (setf s (attempt-to-open-output
		    (string-concat file-name ";P777777") % so we can delete it!
		    ))
	   (when s
	     (setf file-name (=> s file-name))
	     (=> s close)
	     (file-delete-and-expunge file-name)
	     file-name
	     )
	   )
	  (t
	   (setf file-name (=> s file-name))
	   (=> s close)
	   file-name
	   ))))

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
  (load directory)
  (load input-stream)
  (load output-stream)
  (load processor-time)
  (load wait)
  (load vector-fix)
  (load nmode-parsing)
  (load rawio)
  (load windows)
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
