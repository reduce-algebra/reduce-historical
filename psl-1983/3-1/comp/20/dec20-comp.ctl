; Rebuild the COMP module
@term page 0
@def dsk: dsk:,p20ec:,p20c:
@def pl: ple:
@get psl:ex-rlisp
@st
*load build;
*build "DEC20-COMP";
*quit;
@reset .
@term page 24