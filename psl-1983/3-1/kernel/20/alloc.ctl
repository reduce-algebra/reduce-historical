;Modifications to this file may disappear, as this file is generated
;automatically using information in P20:20-KERNEL-GEN.SL.
def dsk: dsk:,p20:,pk:
S:DEC20-CROSS.EXE
ASMOut "alloc";
PathIn "alloc.build";
ASMEnd;
quit;
compile alloc.mac, dalloc.mac
