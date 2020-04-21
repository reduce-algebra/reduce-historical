@; This file constructs a version of NMODE, including
@;
@;	The NMODE (EMACS-like) editor and Lisp interface.
@;	A set of "useful" things described in the manual.
@;
@; It creates a new executable file S:NMODE.EXE, first deleting any previous
@; versions and expunging.  When approved, this file should be renamed to
@; PSL:NMODE.EXE.
@;
@delete s:nmode.exe,
@expunge
@
@psl:bare-psl random-argument-to-get-a-new-fork
*(load useful nmode init-file)
*(nmode-initialize)
*(setq nmode-auto-start t)
*(savesystem "NMODE PSL 3.1" "s:nmode.exe" nil) %((read-init-file "nmode")))
*(quit)
@reset .
