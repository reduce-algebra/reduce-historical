; Rebuild the LAP-TO-ASM module
@def dsk: dsk:,p20ec:,pc:
@def pl: mple:,ple:
@term page 0
@get psl:ex-rlisp
@st
*load build;
*build "LAP-TO-ASM";
*quit;
@reset .
@term page 24