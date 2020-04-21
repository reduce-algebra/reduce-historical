@; This file constructs a new PSL containing many useful things, including:
@;
@;	The NMODE (EMACS-like) editor and Lisp interface.
@;	The Lisp Machine Defstruct Facility.
@;	A set of "useful" things described in the manual.
@;
@; It creates a new executable file S:PSL.EXE, first deleting any previous
@; versions and expunging.  When approved, this file should be renamed to
@; PSL:PSL.EXE.
@;
@delete s:psl.exe
@expunge s:
@s:bare-psl random-argument-to-get-a-new-fork
*(load useful nstruct debug find nmode init-file)
*(nmode-initialize)
*(nmode-switch-windows) % Switch to "OUTPUT" window
*(set-message 
*"C-] E executes Lisp form on current line; C-] L gets normal PSL interface")
*(savesystem "Extended-20 PSL 3.1" "s:psl.exe" '((read-init-file "psl")))
*(quit)
@reset .
@set file autokeep s:psl.exe
