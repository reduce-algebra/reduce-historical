; This file creates a new S:EX-NMODE.EXE, replacing the old one.
;  NOTE: the compiler is also loaded, as most users will need it.
@delete s:nmode.exe,
@exp
@
@s:bare-psl random-argument-to-get-a-new-fork
*(load nmode)
*(load compiler)
*(nmode-initialize)
*(setf nmode-auto-start T)
*(setf prinlevel 2)
*(savesystem "Extended 20-PSL 3.1 NMODE" "S:NMODE.EXE" ())
*(quit)
@reset .
