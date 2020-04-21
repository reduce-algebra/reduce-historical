; Rebuild the ASM module
@def dsk: dsk:,p20ec:,p20c:,pc:
@def pl: ple:
@term page 0
@get psl:ex-rlisp
@st
*load build;
*build "DEC20-ASM";
*quit;
@reset .
@term page 24