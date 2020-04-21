@; This file constructs a new PSLCOMP.
@;
@; It creates a new executable file S:PSLCOMP.EXE, first deleting any previous
@; versions and expunging.  When approved, this file should be renamed to
@; PSL:PSLCOMP.EXE.
@;
@delete s:pslcomp.exe
@expunge s:
@psl:bare-psl random-argument-to-get-a-new-fork
* (load pslcomp-main init-file)
* % The following things are loaded because their definitions are useful
* % when users compile things:
* (load objects common strings pathnames fast-vector nstruct)
* (savesystem "UTAH-PSL Compiler 3.1"
*	      "s:pslcomp.exe"
*	      '((read-init-file "pslcomp")))
* (quit)
@reset .
