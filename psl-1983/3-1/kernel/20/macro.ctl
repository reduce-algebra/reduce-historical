;Modifications to this file may disappear, as this file is generated
;automatically using information in P20:20-KERNEL-GEN.SL.
@def dsk: dsk:,p20:,pk:
@S:DEC20-CROSS.EXE
*!*symwrite := T;
*!*symsave := nil;
*ASMOut "macro";
*PathIn "macro.build";
*ASMEnd;
*quit;
@reset .
@S:DEC20-CROSS.EXE
*!*symread := T;
*readsymfile();
*!*symread := nil;
*writesavefile();
*quit;
@compile macro.mac, dmacro.mac
