; Run this after BUILD-20-CROSS.CTL
S:DEC20-CROSS
load Tenex!-Asm, Non!-KL!-Comp;
system_list!* := Delete('Tops20, system_list!*);
system_list!* := Delete('KL10, system_list!*);
system_list!* := Adjoin('Tenex, system_list!*);
DumpLisp "S:TENEX-CROSS.EXE";
