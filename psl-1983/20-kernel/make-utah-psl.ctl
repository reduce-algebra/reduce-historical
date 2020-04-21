@; This file constructs a new PSL containing many useful things, including:
@; It creates a new executable file S:PSL.EXE, first deleting any previous
@; versions and expunging.  When approved, this file should be renamed to
@;
@s:bare-psl random-argument-to-get-a-new-fork
*(load init-file homedir)
*(savesystem "PSL 3.1" "s:psl.exe" '((read-init-file "psl")))
*(quit)
@reset .
@set file autokeep s:psl.exe
