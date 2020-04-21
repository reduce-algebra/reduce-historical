define DSK: DSK:, P20:, PI:
S:DEC20-CROSS.EXE
ASMOut "fasl";
in "fasl.build";
ASMEnd;
quit;
compile fasl.mac, dfasl.mac
delete fasl.mac, dfasl.mac
