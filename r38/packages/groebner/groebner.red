module groebner;
 
% Author: Herbert Melenk
% in cooperation with Winfried Neun, H. Michael Moeller.

% module structure:       
%     GROEBNER  package + GROEBNR2 package
%  polynomial arithmetic:
%     VDP2DIP package included in DIPOLY package

fluid '(asymplis!* basecount!* bcount!* bczerodivl!* b4count!* current!-modulus
 currentvdpmodule!* denominators!* depl!* dipsortmode!* dipvars!* dmode!*
 factorlevel!* factortime!* factorlvevel!* fcount!* fourthvalue!* glexdomain!*
 global!-dipvars!* gmodule gmodule!* groebabort!* groebactualg!*
 groebactualg99!* groebdomain!* groebfabort!* groebmodular!* groebrestriction!*
 groebroots!* groecontcount!* groefeedback!* groesfactors!* groesoldb!*
 groesoldmode!* groesolvelevel!* groetags!* groetime!* fourthvalue!*
 hcount!* hzerocount!* intvdpvars!* mcount!*
 pcount!* pairsdone!* powlis!* probcount!* secondvalue!* thirdvalue!*
 variables!* vars!* vbccurrentmode!* vdplastvar!* vdpone!* vdpsortmode!*
 vdpvars!* vbcmodule!* vdpsortmode!* vdpsortextension!* vdpvars!*
 !*arbvars !*complex !*compxroots !*convert !*divisor !*exp !*ezgcd
 !*factor !*fullreduction !*gcd !*gltbasis !*greduce !*gsugar !*grmod!*
 !*groebcomplex !*groebdivide !*groebfac !*groebfullreduction !*groebheufact
 !*groebidqbasis !*groebnumval !*groebopt !*groebprot !*groebprereduce
 !*groebreduce !*groebsubs !*groebprot
 !*groebrm !*groebstat !*groebweak !*groelterms !*groesolgarbage !*groesolrecurs
 !*groebrm !*groebstat !*gsugar !*gtraverso!-sloppy !*msg !*precise !*trgroeb
 !*trgroebr !*trgroebr1 !*trgroebs !*trgroebsi !*trgroeb1 !*varopt !*vdpinteger
 !*vdpmodular
);

global '(assumptions gltb glterms groebmonfac groebprotfile groebrestriction
 groebresmax gvarslast largest!-small!-modulus requirements !*match !*trgroesolv);

currentvdpmodule!*:='vdp2dip;

create!-package('(groebner grinterf grinter2 buchbg groebcri groesolv groebopt
 groebsea groebsor groebspa groebfac groebidq kredelw traverso hille),
 '(contrib groebner));

put('groebner,'version,3.1);

% Other packages needed.

load!-package ' dipoly;

if(null v or v < 4.1)where v=get('dipoly,'version)
  then rederr {"wrong dipoly module",
                "(get and compile dipoly, before you compile groebner)"};

smacro procedure tt(s1,s2);
  % Lcm of leading terms of s1 and s2 .
vevlcm(vdpevlmon s1,vdpevlmon s2);

smacro procedure vdpnumber f;vdpgetprop(f,'number);

imports a2vdp,a2vbc,dependsl,domainp,eqexpr,f2vdp,fctrf,korder,lc,lpow,
 multroot0,
 makearbcomplex,mvar,numr,precision,prepcadr,prepf,prepsq,
 reorder,rerror,reval,
 setkorder, simp,solveeval,torder,
 vdp2a,vdp2f,vdpfmon,vdpappendmon,vdpappendvdp,vdplbc,vdpred,vdplastmon,
 vdpzero!?,vdpredzero!?,vdpone!?,vevzero!?, vbcplus!?,vbcone!?,vbcnumberp!?,vevdivides!?,
 vdpequal,vdpmember,vdpsum,vdpdif,vdpprod,vdpdivmon,vdpcancelvev,
 vdplcomb1,vdpcontent, vbcsum,vbcdif,vbcneg,vbcprod,vbcquot,vbcinv,vbcgcd,vbcabs,vbcone!?,
 vdpputprop,vdpgetprop,vdplsort,vdplsortin,vdpprint,
 vdpprin3t,vdpcondense,vdplcm,vdprectoint,vdpsimpcont,vdpvbcprod,vdpcancelmvev,vdpprin2,
 vdplength,vdpilcomb1,vdpinit,vdpinit2,vdpcleanup,
 vevcompless!?,vevdif,vevequal,vevsum,vevnth,vevtdeg,vevweightedcomp2,vevzero,
 writepri,
 !*eqn2a;

exports groebnereval,groesolveeval,groepostsolveeval,idquotienteval,
 gdimensioneval,glexconvert,greduce,preduce,preduceeval,groebnert,dd_groebner,
 hilbertpolynomial,gsort,gsplit,gspoly,gzerodim!?;

endmodule;;end;
