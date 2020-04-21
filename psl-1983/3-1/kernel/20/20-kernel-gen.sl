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

% 21-May-83  Modified to produce Extended-20 version.
%   Took out delete of .MAC files, as some hand patching is (unfortunately)
%   still necessary.
% 01-Mar-83  Nancy Kendzierski
%   Changed script files to use PathIn, instead of In and DSK:.
%   Changed link file to explicitly use .REL files on P20:.
% <PSL.20-INTERP>20-KERNEL-GEN.SL.15,  7-Jun-82 12:48:19, Edit by BENSON
% Converted kernel-file-name* to all-kernel-script...
% <PSL.20-INTERP>20-KERNEL-GEN.SL.14,  6-Jun-82 05:29:21, Edit by GRISS
% Add kernel-file-name*


(compiletime (load kernel))
(compiletime (setq *EOLInStringOK T))
(loadtime (imports '(kernel)))

(setq command-file-name* "%w.ctl")

(setq command-file-format*
";Modifications to this file may disappear, as this file is generated
;automatically using information in P20:20-KERNEL-GEN.SL.
def dsk: dsk:,p20,pk:
S:DEC20-CROSS.EXE
ASMOut ""%w"";
PathIn ""%w.build"";
ASMEnd;
quit;
compile %w.mac, d%w.mac
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
";Modifications to this file may disappear, as this file is generated
;automatically using information in P20E:20-KERNEL-GEN.SL.
cd S:
LINK
/map
p20:nil.rel
/set:.low.:202
p20:%e
/save s:pbpsl.exe
/go
@get s:pbpsl.exe/u 1
@save s:bpsl.exe
")

(setq script-file-name-separator* "
p20:")

(kernel '(types randm alloc arith debg error eval extra fasl io macro prop
	  symbl sysio tloop main heap))
