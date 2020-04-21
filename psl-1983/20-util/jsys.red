%
% JSYS.RED - Simple XJSYS function
% 
% Author:      Martin L. Griss 
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        8 March 1981
% Copyright (c) 1981 University of Utah
%

%  <PSL.UTIL>JSYS.RED.9, 18-May-82 13:24:36, Edit by BENSON
%  Made XJSYSn OpenCode'ed
%/ Changed FILNAM->FileName, due to GLOBAL conflict
%/ Changed JSYS calls, so LIST(..) rather than '(..) used
%/ Changed for V3:JSYS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  <PSL.UTIL>JSYS.RED.2, 18-Mar-82 21:49:32, Edit by GRISS
%  Converted to V3
%. M. Griss 3:32pm  Saturday, 7 November 1981
%. MLG: Fixed GetErrorString and BITS macro, 8:57am  Friday, 25 December 1981
on syslisp;

% Modeled after the IDapply to avoid CONS, register reloads
% could easily be done Opencoded
% SYSLSP calls, expect W value, return appropriate register

%. syslsp procedure XJsys0(Jr1,Jr2,Jr3,Jr4,Jnum)
%. syslsp procedure XJsys1(Jr1,Jr2,Jr3,Jr4,Jnum)
%. syslsp procedure XJsys2(Jr1,Jr2,Jr3,Jr4,Jnum)
%. syslsp procedure XJsys3(Jr1,Jr2,Jr3,Jr4,Jnum)
%. syslsp procedure XJsys4(Jr1,Jr2,Jr3,Jr4,Jnum)

lap '((!*entry xjsys0 expr 5)
      (jsys (indirect (reg 5)))
      (erjmp (entry xjsyserror))
      (!*move (wconst 0) (reg 1))
      (!*exit 0))$

BothTimes put('xjsys0, 'OpenCode, '((jsys (indexed (reg 5) 0))
				    (jump 8#16 (entry xjsyserror))
				    (setzm (reg 1))));

lap '((!*entry xjsys1 expr 5)
      (jsys (indirect (reg 5)))
      (erjmp (entry xjsyserror))
      (!*exit 0))$

BothTimes put('xjsys1, 'OpenCode, '((jsys (indexed (reg 5) 0))
				    (jump 8#16 (entry xjsyserror))));

lap '((!*entry xjsys2 expr 5)
      (jsys (indirect (reg 5)))
      (erjmp (entry xjsyserror))
      (!*move (reg 2) (reg 1))
      (!*exit 0))$

BothTimes put('xjsys2, 'OpenCode, '((jsys (indexed (reg 5) 0))
				    (jump 8#16 (entry xjsyserror))
				    (move (reg 1) (reg 2))));

lap '((!*entry xjsys3 expr 5)
      (jsys (indirect (reg 5)))
      (erjmp (entry xjsyserror))
      (!*move (reg 3) (reg 1))
      (!*exit 0))$

BothTimes put('xjsys3, 'OpenCode, '((jsys (indexed (reg 5) 0))
				    (jump 8#16 (entry xjsyserror))
				    (move (reg 1) (reg 3))));

lap '((!*entry xjsys4 expr 5)
      (jsys (indirect (reg 5)))
      (erjmp (entry xjsyserror))
      (!*move (reg 4) (reg 1))
      (!*exit 0))$


BothTimes put('xjsys4, 'OpenCode, '((jsys (indexed (reg 5) 0))
				    (jump 8#16 (entry xjsyserror))
				    (move (reg 1) (reg 4))));

lap '((!*entry geterrorstring expr 1)
      (!*move (wconst -1) (reg 2))       % most recent error
      (hrli  (reg 2) 8#400000) % self process
      (!*move (wconst 0) (reg 3))        % all string
      (erstr)           % get the error string to a1 buffer
      (jfcl)
      (jfcl)
      (!*exit 0))$

syslsp procedure xjsyserror$	 %/ should load up errstr
 begin scalar s;
    s:=gtstr 200;
    geterrorstring lor(lsh(8#10700,18), s)$
    return stderror recopystringtonull s;
 end;

% --- conversions for lisp level calls

syslsp procedure str2int s; 
 sys2int strinf s;

syslsp procedure int2str i;
  mkstr int2sys i;

syslsp procedure jconv j;	%. handle untagging
 if fixp j then int2sys j
  else if stringp j 
     then lor(lsh(8#10700,18),strinf(j))  % Bug in LONG const
  else stderror list(j,'" not known in jconv");

% lisp calls. untag args, then tag result as integer
%             user has to convert result from xword, stringbase, etc

syslsp procedure jsys0(jr1,jr2,jr3,jr4,jnum);
 sys2int xjsys0(jconv jr1,jconv jr2,jconv jr3,jconv jr4,int2sys jnum)$

syslsp procedure jsys1(jr1,jr2,jr3,jr4,jnum);
 sys2int xjsys1(jconv jr1,jconv jr2,jconv jr3,jconv jr4,int2sys jnum)$

syslsp procedure jsys2(jr1,jr2,jr3,jr4,jnum);
 sys2int xjsys2(jconv jr1,jconv jr2,jconv jr3,jconv jr4,int2sys jnum)$

syslsp procedure jsys3(jr1,jr2,jr3,jr4,jnum);
 sys2int xjsys3(jconv jr1,jconv jr2,jconv jr3,jconv jr4,int2sys jnum)$

syslsp procedure jsys4(jr1,jr2,jr3,jr4,jnum);
 sys2int xjsys4(jconv jr1,jconv jr2,jconv jr3,jconv jr4,int2sys jnum)$

syslsp procedure checknum(x,y);
 if intp x then intinf x else nonintegererror(x,y);

CommentOutCode<<
syslsp procedure insertstringsize s;
 begin scalar l,s1;			% this must not be done to a string
	l:=0; s1:=strinf(s);		% in the heap!
	while not (strbyt(s1,l)= char null) do l:=l+1;
	@s1:=mkitem(hstr,l-1);
 return s;
 end;
>>;

syslsp procedure recopystringtonull s;
 begin scalar l,s1,s2,ch;
	l:=0; s1:=strinf(s);
	while not (strbyt(s1,l)= char null) do l:=l+1;
	s2:=gtstr(l-1);
	l:=0;
	while not ((ch:=strbyt(s1,l))= char null) 
	  do <<strbyt(s2,l):= ch; l:=l+1>>;
	return mkstr s2;
  end;

% ------------ useful bit, byte and word utilities

syslsp procedure swap(x);		%. swap half words
 xword(lowhalfword x,highhalfword x);

syslsp procedure lowhalfword n;
  sys2int land(int2sys n,8#777777);

compiletime <<
syslsp smacro procedure rsh(x,y);
  lsh(x,-y);
>>;

syslsp procedure highhalfword n;
  sys2int land(rsh(int2sys n,18),8#777777);

syslsp procedure xword(x,y);   %. build word from half-words
%  sys2int lor(lsh(lowhalfword(int2sys x),18),
%                  lowhalfword int2sys y);	%/Compiler error
begin scalar Tmp;
  Tmp := lowhalfword int2sys x;
  Tmp := lsh(Tmp, 18);
  Tmp := lor(Tmp, lowhalfword int2sys y);
  return sys2int Tmp;
end;

syslsp procedure jbits l;            %. convert bit and byte fields
% l is list of bitpos or (fieldvalue . rightbitpos)
% msb is #0, lsb is #35 on dec-20
 begin scalar wd,x,fldpos,fldval;
	wd:=0;
   lb:	if not pairp l then return sys2int wd;
	x:=car l; l := cdr l;
        if pairp x then <<fldpos:=cdr x; fldval:=car x>>
         else <<fldpos:=x; fldval:=1>>;
        if not (fixp fldval and fixp fldpos) then goto lb;
	if fldpos <0 or fldpos > 35 then goto lb;
	wd := lor(wd,lsh(fldval,35-fldpos));
	goto lb;
 end;

macro procedure bits l;
 list('jbits, 'list . cdr l);


%. load jSYS Names

procedure MakeJsys(Name, Number);
    EvDefConst(Name, Number);

off syslisp;

MakeJsys( 'jsJSYS , 8#0)$
MakeJsys( 'jsLOGIN , 8#1)$
MakeJsys( 'jsCRJOB , 8#2)$
MakeJsys( 'jsLGOUT , 8#3)$
MakeJsys( 'jsCACCT , 8#4)$
MakeJsys( 'jsEFACT , 8#5)$
MakeJsys( 'jsSMON , 8#6)$
MakeJsys( 'jsTMON , 8#7)$
MakeJsys( 'jsGETAB , 8#10)$
MakeJsys( 'jsERSTR , 8#11)$
MakeJsys( 'jsGETER , 8#12)$
MakeJsys( 'jsGJINF , 8#13)$
MakeJsys( 'jsTIME , 8#14)$
MakeJsys( 'jsRUNTM , 8#15)$
MakeJsys( 'jsSYSGT , 8#16)$
MakeJsys( 'jsGNJFN , 8#17)$
MakeJsys( 'jsGTJFN , 8#20)$
MakeJsys( 'jsOPENF , 8#21)$
MakeJsys( 'jsCLOSF , 8#22)$
MakeJsys( 'jsRLJFN , 8#23)$
MakeJsys( 'jsGTSTS , 8#24)$
MakeJsys( 'jsSTSTS , 8#25)$
MakeJsys( 'jsDELF , 8#26)$
MakeJsys( 'jsSFPTR , 8#27)$
MakeJsys( 'jsJFNS , 8#30)$
MakeJsys( 'jsFFFFP , 8#31)$
MakeJsys( 'jsRDDIR , 8#32)$
MakeJsys( 'jsCPRTF , 8#33)$
MakeJsys( 'jsCLZFF , 8#34)$
MakeJsys( 'jsRNAMF , 8#35)$
MakeJsys( 'jsSIZEF , 8#36)$
MakeJsys( 'jsGACTF , 8#37)$
MakeJsys( 'jsSTDIR , 8#40)$
MakeJsys( 'jsDIRST , 8#41)$
MakeJsys( 'jsBKJFN , 8#42)$
MakeJsys( 'jsRFPTR , 8#43)$
MakeJsys( 'jsCNDIR , 8#44)$
MakeJsys( 'jsRFBSZ , 8#45)$
MakeJsys( 'jsSFBSZ , 8#46)$
MakeJsys( 'jsSWJFN , 8#47)$
MakeJsys( 'jsBIN , 8#50)$
MakeJsys( 'jsBOUT , 8#51)$
MakeJsys( 'jsSIN , 8#52)$
MakeJsys( 'jsSOUT , 8#53)$
MakeJsys( 'jsRIN , 8#54)$
MakeJsys( 'jsROUT , 8#55)$
MakeJsys( 'jsPMAP , 8#56)$
MakeJsys( 'jsRPACS , 8#57)$
MakeJsys( 'jsSPACS , 8#60)$
MakeJsys( 'jsRMAP , 8#61)$
MakeJsys( 'jsSACTF , 8#62)$
MakeJsys( 'jsGTFDB , 8#63)$
MakeJsys( 'jsCHFDB , 8#64)$
MakeJsys( 'jsDUMPI , 8#65)$
MakeJsys( 'jsDUMPO , 8#66)$
MakeJsys( 'jsDELDF , 8#67)$
MakeJsys( 'jsASND , 8#70)$
MakeJsys( 'jsRELD , 8#71)$
MakeJsys( 'jsCSYNO , 8#72)$
MakeJsys( 'jsPBIN , 8#73)$
MakeJsys( 'jsPBOUT , 8#74)$
MakeJsys( 'jsPSIN , 8#75)$
MakeJsys( 'jsPSOUT , 8#76)$
MakeJsys( 'jsMTOPR , 8#77)$
MakeJsys( 'jsCFIBF , 8#100)$
MakeJsys( 'jsCFOBF , 8#101)$
MakeJsys( 'jsSIBE , 8#102)$
MakeJsys( 'jsSOBE , 8#103)$
MakeJsys( 'jsDOBE , 8#104)$
MakeJsys( 'jsGTABS , 8#105)$
MakeJsys( 'jsSTABS , 8#106)$
MakeJsys( 'jsRFMOD , 8#107)$
MakeJsys( 'jsSFMOD , 8#110)$
MakeJsys( 'jsRFPOS , 8#111)$
MakeJsys( 'jsRFCOC , 8#112)$
MakeJsys( 'jsSFCOC , 8#113)$
MakeJsys( 'jsSTI , 8#114)$
MakeJsys( 'jsDTACH , 8#115)$
MakeJsys( 'jsATACH , 8#116)$
MakeJsys( 'jsDVCHR , 8#117)$
MakeJsys( 'jsSTDEV , 8#120)$
MakeJsys( 'jsDEVST , 8#121)$
MakeJsys( 'jsMOUNT , 8#122)$
MakeJsys( 'jsDSMNT , 8#123)$
MakeJsys( 'jsINIDR , 8#124)$
MakeJsys( 'jsSIR , 8#125)$
MakeJsys( 'jsEIR , 8#126)$
MakeJsys( 'jsSKPIR , 8#127)$
MakeJsys( 'jsDIR , 8#130)$
MakeJsys( 'jsAIC , 8#131)$
MakeJsys( 'jsIIC , 8#132)$
MakeJsys( 'jsDIC , 8#133)$
MakeJsys( 'jsRCM , 8#134)$
MakeJsys( 'jsRWM , 8#135)$
MakeJsys( 'jsDEBRK , 8#136)$
MakeJsys( 'jsATI , 8#137)$
MakeJsys( 'jsDTI , 8#140)$
MakeJsys( 'jsCIS , 8#141)$
MakeJsys( 'jsSIRCM , 8#142)$
MakeJsys( 'jsRIRCM , 8#143)$
MakeJsys( 'jsRIR , 8#144)$
MakeJsys( 'jsGDSTS , 8#145)$
MakeJsys( 'jsSDSTS , 8#146)$
MakeJsys( 'jsRESET , 8#147)$
MakeJsys( 'jsRPCAP , 8#150)$
MakeJsys( 'jsEPCAP , 8#151)$
MakeJsys( 'jsCFORK , 8#152)$
MakeJsys( 'jsKFORK , 8#153)$
MakeJsys( 'jsFFORK , 8#154)$
MakeJsys( 'jsRFORK , 8#155)$
MakeJsys( 'jsRFSTS , 8#156)$
MakeJsys( 'jsSFORK , 8#157)$
MakeJsys( 'jsSFACS , 8#160)$
MakeJsys( 'jsRFACS , 8#161)$
MakeJsys( 'jsHFORK , 8#162)$
MakeJsys( 'jsWFORK , 8#163)$
MakeJsys( 'jsGFRKH , 8#164)$
MakeJsys( 'jsRFRKH , 8#165)$
MakeJsys( 'jsGFRKS , 8#166)$
MakeJsys( 'jsDISMS , 8#167)$
MakeJsys( 'jsHALTF , 8#170)$
MakeJsys( 'jsGTRPW , 8#171)$
MakeJsys( 'jsGTRPI , 8#172)$
MakeJsys( 'jsRTIW , 8#173)$
MakeJsys( 'jsSTIW , 8#174)$
MakeJsys( 'jsSOBF , 8#175)$
MakeJsys( 'jsRWSET , 8#176)$
MakeJsys( 'jsGETNM , 8#177)$
MakeJsys( 'jsGET , 8#200)$
MakeJsys( 'jsSFRKV , 8#201)$
MakeJsys( 'jsSAVE , 8#202)$
MakeJsys( 'jsSSAVE , 8#203)$
MakeJsys( 'jsSEVEC , 8#204)$
MakeJsys( 'jsGEVEC , 8#205)$
MakeJsys( 'jsGPJFN , 8#206)$
MakeJsys( 'jsSPJFN , 8#207)$
MakeJsys( 'jsSETNM , 8#210)$
MakeJsys( 'jsFFUFP , 8#211)$
MakeJsys( 'jsDIBE , 8#212)$
MakeJsys( 'jsFDFRE , 8#213)$
MakeJsys( 'jsGDSKC , 8#214)$
MakeJsys( 'jsLITES , 8#215)$
MakeJsys( 'jsTLINK , 8#216)$
MakeJsys( 'jsSTPAR , 8#217)$
MakeJsys( 'jsODTIM , 8#220)$
MakeJsys( 'jsIDTIM , 8#221)$
MakeJsys( 'jsODCNV , 8#222)$
MakeJsys( 'jsIDCNV , 8#223)$
MakeJsys( 'jsNOUT , 8#224)$
MakeJsys( 'jsNIN , 8#225)$
MakeJsys( 'jsSTAD , 8#226)$
MakeJsys( 'jsGTAD , 8#227)$
MakeJsys( 'jsODTNC , 8#230)$
MakeJsys( 'jsIDTNC , 8#231)$
MakeJsys( 'jsFLIN , 8#232)$
MakeJsys( 'jsFLOUT , 8#233)$
MakeJsys( 'jsDFIN , 8#234)$
MakeJsys( 'jsDFOUT , 8#235)$
MakeJsys( 'jsCRDIR , 8#240)$
MakeJsys( 'jsGTDIR , 8#241)$
MakeJsys( 'jsDSKOP , 8#242)$
MakeJsys( 'jsSPRIW , 8#243)$
MakeJsys( 'jsDSKAS , 8#244)$
MakeJsys( 'jsSJPRI , 8#245)$
MakeJsys( 'jsSTO , 8#246)$
MakeJsys( 'jsBBNIIT , 8#247)$
MakeJsys( 'jsARCF , 8#247)$
MakeJsys( 'jsASNDP , 8#260)$
MakeJsys( 'jsRELDP , 8#261)$
MakeJsys( 'jsASNDC , 8#262)$
MakeJsys( 'jsRELDC , 8#263)$
MakeJsys( 'jsSTRDP , 8#264)$
MakeJsys( 'jsSTPDP , 8#265)$
MakeJsys( 'jsSTSDP , 8#266)$
MakeJsys( 'jsRDSDP , 8#267)$
MakeJsys( 'jsWATDP , 8#270)$
MakeJsys( 'jsATNVT , 8#274)$
MakeJsys( 'jsCVSKT , 8#275)$
MakeJsys( 'jsCVHST , 8#276)$
MakeJsys( 'jsFLHST , 8#277)$
MakeJsys( 'jsGCVEC , 8#300)$
MakeJsys( 'jsSCVEC , 8#301)$
MakeJsys( 'jsSTTYP , 8#302)$
MakeJsys( 'jsGTTYP , 8#303)$
MakeJsys( 'jsBPT , 8#304)$
MakeJsys( 'jsGTDAL , 8#305)$
MakeJsys( 'jsWAIT , 8#306)$
MakeJsys( 'jsHSYS , 8#307)$
MakeJsys( 'jsUSRIO , 8#310)$
MakeJsys( 'jsPEEK , 8#311)$
MakeJsys( 'jsMSFRK , 8#312)$
MakeJsys( 'jsESOUT , 8#313)$
MakeJsys( 'jsSPLFK , 8#314)$
MakeJsys( 'jsADVIS , 8#315)$
MakeJsys( 'jsJOBTM , 8#316)$
MakeJsys( 'jsDELNF , 8#317)$
MakeJsys( 'jsSWTCH , 8#320)$
MakeJsys( 'jsOPRFN , 8#326)$
MakeJsys( 'jsCGRP , 8#327)$
MakeJsys( 'jsVACCT , 8#330)$
MakeJsys( 'jsGDACC , 8#331)$
MakeJsys( 'jsATGRP , 8#332)$
MakeJsys( 'jsGACTJ , 8#333)$
MakeJsys( 'jsGPSGN , 8#334)$
MakeJsys( 'jsRSCAN , 8#500)$
MakeJsys( 'jsHPTIM , 8#501)$
MakeJsys( 'jsCRLNM , 8#502)$
MakeJsys( 'jsINLNM , 8#503)$
MakeJsys( 'jsLNMST , 8#504)$
MakeJsys( 'jsRDTXT , 8#505)$
MakeJsys( 'jsSETSN , 8#506)$
MakeJsys( 'jsGETJI , 8#507)$
MakeJsys( 'jsMSEND , 8#510)$
MakeJsys( 'jsMRECV , 8#511)$
MakeJsys( 'jsMUTIL , 8#512)$
MakeJsys( 'jsENQ , 8#513)$
MakeJsys( 'jsDEQ , 8#514)$
MakeJsys( 'jsENQC , 8#515)$
MakeJsys( 'jsSNOOP , 8#516)$
MakeJsys( 'jsSPOOL , 8#517)$
MakeJsys( 'jsALLOC , 8#520)$
MakeJsys( 'jsCHKAC , 8#521)$
MakeJsys( 'jsTIMER , 8#522)$
MakeJsys( 'jsRDTTY , 8#523)$
MakeJsys( 'jsTEXTI , 8#524)$
MakeJsys( 'jsUFPGS , 8#525)$
MakeJsys( 'jsSFPOS , 8#526)$
MakeJsys( 'jsSYERR , 8#527)$
MakeJsys( 'jsDIAG , 8#530)$
MakeJsys( 'jsSINR , 8#531)$
MakeJsys( 'jsSOUTR , 8#532)$
MakeJsys( 'jsRFTAD , 8#533)$
MakeJsys( 'jsSFTAD , 8#534)$
MakeJsys( 'jsTBDEL , 8#535)$
MakeJsys( 'jsTBADD , 8#536)$
MakeJsys( 'jsTBLUK , 8#537)$
MakeJsys( 'jsSTCMP , 8#540)$
MakeJsys( 'jsSETJB , 8#541)$
MakeJsys( 'jsGDVEC , 8#542)$
MakeJsys( 'jsSDVEC , 8#543)$
MakeJsys( 'jsCOMND , 8#544)$
MakeJsys( 'jsPRARG , 8#545)$
MakeJsys( 'jsGACCT , 8#546)$
MakeJsys( 'jsLPINI , 8#547)$
MakeJsys( 'jsGFUST , 8#550)$
MakeJsys( 'jsSFUST , 8#551)$
MakeJsys( 'jsACCES , 8#552)$
MakeJsys( 'jsRCDIR , 8#553)$
MakeJsys( 'jsRCUSR , 8#554)$
MakeJsys( 'jsSNDIM , 8#750)$
MakeJsys( 'jsRCVIM , 8#751)$
MakeJsys( 'jsASNSQ , 8#752)$
MakeJsys( 'jsRELSQ , 8#753)$
MakeJsys( 'jsTHIBR , 8#770)$
MakeJsys( 'jsTWAKE , 8#771)$
MakeJsys( 'jsMRPAC , 8#772)$
MakeJsys( 'jsSETPV , 8#773)$
MakeJsys( 'jsMTALN , 8#774)$
MakeJsys( 'jsTTMSG , 8#775)$

End$
