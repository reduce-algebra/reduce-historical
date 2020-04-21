@S:BARE-PSL.EXE random-argument-to-get-a-new-fork
*((lambda (loaddirectories!*)
          (load compiler rlisp init-file))
	'("" "pl:"))
*(SaveSystem "Extended 20-PSL 3.1 RLisp" "S:RLISP.EXE" '((read-init-file "rlisp")))
*(quit)
@reset .
