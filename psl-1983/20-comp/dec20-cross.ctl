
@get PSL:RLISP
@st
*Options!*:=NIL; % Force reload of ALL
*LoadDirectories!*:='("pl:"); % Only look at <psl.lap>
*load(zboot, syslisp, if!-system, lap!-to!-asm);
*load(dec20!-comp,dec20!-cmac,dec20!-asm);
*  %/ old:? remflag('(extrareg),'terminaloperand);
*  %/ to fix HRRZI for ExtraReg... why was it here
*off usermode;
*Date!* := "Dec 20 cross compiler";
*Dumplisp "S:DEC20-CROSS.EXE";
*Quit;
@reset .