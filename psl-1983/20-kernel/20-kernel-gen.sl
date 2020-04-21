%
% 20-KERNEL-GEN.SL - Generate scripts for building Dec-20 PSL kernel
% 
% Author:      Eric Benson
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        26 May 1982
% Copyright (c) 1982 University of Utah
%

% <PSL.20-INTERP>20-KERNEL-GEN.SL.15,  7-Jun-82 12:48:19, Edit by BENSON
% Converted kernel-file-name* to all-kernel-script...
% <PSL.20-INTERP>20-KERNEL-GEN.SL.14,  6-Jun-82 05:29:21, Edit by GRISS
% Add kernel-file-name*


(compiletime (load kernel))
(compiletime (setq *EOLInStringOK T))
(loadtime (imports '(kernel)))

(setq command-file-name* "%w.ctl")

(setq command-file-format*
"define DSK: DSK:, P20:, PI:
S:DEC20-CROSS.EXE
ASMOut ""%w"";
in ""%w.build"";
ASMEnd;
quit;
compile %w.mac, d%w.mac
delete %w.mac, d%w.mac
")

(setq init-file-name* "psl.init")

(setq init-file-format* "(lapin ""%w.init"")
")

(setq all-kernel-script-name* "all-kernel.ctl")

(setq all-kernel-script-format* "submit %w.ctl
")

(setq code-object-file-name* "%w.rel")

(setq data-object-file-name* "d%w.rel")

(setq link-script-name* "psl-link.ctl")

(setq link-script-format*
"cd S:
define DSK:, DSK:, P20:
LINK
/nosymbol
nil.rel
/set:.low.:202
%e
/save s:bpsl.exe
/go
")

(setq script-file-name-separator* "
")

(kernel '(types randm alloc arith debg error eval extra fasl io macro prop
	  symbl sysio tloop main heap))
