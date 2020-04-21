define DSK: DSK:, P20:, PI:
S:DEC20-CROSS.EXE
ASMOut "symbl";
in "symbl.build";
ASMEnd;
quit;
compile symbl.mac, dsymbl.mac
delete symbl.mac, dsymbl.mac
