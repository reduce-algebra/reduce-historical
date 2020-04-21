;Modifications to this file may disappear, as this file is generated
;automatically using information in P20:20-KERNEL-GEN.SL.
def dsk: dsk:,p20:,pk:
S:DEC20-CROSS.EXE
ASMOut "sysio";
PathIn "sysio.build";
ASMEnd;
quit;
compile sysio.mac, dsysio.mac
