@define dsk: dsk:,p20:
@S:BPSL.EXE
*(lapin "psl.init")
*(setq loaddirectories* '("" "pl:"))
*(load char-macro))
*(de gc-trap () nil)
*(setq heap-warning-level 1000)
*(setq options* nil)
*(setq bug-mail-to "PSL")
*(de versionname() "Extended-20 Bare PSL 3.1")
*(savesystem (versionname) "s:bare-psl.exe" ())
*(quit)
;@rename S:BARE-PSL.EXE PSL:BARE-PSL.EXE
;@set file autokeep PSL:BARE-PSL.EXE
