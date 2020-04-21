@; This file constructs a new RLISPCOMP.
@;
@; It creates a new executable file S:RLISPCOMP.EXE, first deleting any previous
@; versions and expunging.  When approved, this file should be renamed to
@; PSL:RLISPCOMP.EXE.
@;
@delete s:rlispcomp.exe
@expunge s:
@s:bare-psl random-argument-to-get-a-new-fork
* (load rlisp rlispcomp init-file if-system monsym)
* % The following things are loaded because their definitions are useful
* % when users compile things:
* (load objects common strings pathnames fast-vector nstruct)
* (savesystem "Extended-20 RLISP Compiler 3.1"
*	      "s:rlispcomp.exe"
*	      '((read-init-file "rlispcomp")(rlispcomp)))
* (quit)
@reset .
