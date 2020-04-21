; Rebuild the ASM module
@term page 0
@get psl:rlisp
@st
*loaddirectories!*:='("pl:");
*load build;
*build "DEC20-ASM";
*quit;
@reset .
@term page 24