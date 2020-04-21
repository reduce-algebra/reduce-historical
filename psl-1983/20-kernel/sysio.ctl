define DSK: DSK:, P20:, PI:
S:DEC20-CROSS.EXE
ASMOut "sysio";
in "sysio.build";
ASMEnd;
quit;
compile sysio.mac, dsysio.mac
delete sysio.mac, dsysio.mac
