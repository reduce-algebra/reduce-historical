(compiletime
(dm DEFINEOPCODERANGEFROM (U)
  (prog (start args)
    (setq start (sub1 (second U)))
    (setq args (second (third U)))
    (return (cons 'progn
	      (foreach X in args collect (list 'put
					    (mkquote X)
					    ''opcodevalue
					    (setq start (add1 start))))))))
)
(DEFINEOPCODERANGEFROM 68 (QUOTE (JSYS ADJSP)))
(DEFINEOPCODERANGEFROM 91 (QUOTE (ADJBP)))
(DEFINEOPCODERANGEFROM 72 (QUOTE (DFAD DFSB DFMP DFDV)))
(DEFINEOPCODERANGEFROM 80 (QUOTE (DMOVE DMOVN FIX)))
(DEFINEOPCODERANGEFROM 84 (QUOTE (DMOVEM DMOVNM FIXR FLTR UFA DFN FSC IBP 
ILDB LDB IDPB DPB FAD FADL FADM FADB FADR FADRI FADRM FADRB FSB FSBL FSBM 
FSBB FSBR FSBRI FSBRM FSBRB FMP FMPL FMPM FMPB FMPR FMPRI FMPRM FMPRB FDV 
FDVL FDVM FDVB FDVR FDVRI FDVRM FDVRB MOVE MOVEI MOVEM MOVES MOVS MOVSI 
MOVSM MOVSS MOVN MOVNI MOVNM MOVNS MOVM MOVMI MOVMM MOVMS IMUL IMULI IMULM 
IMULB MUL MULI MULM MULB IDIV IDIVI IDIVM IDIVB DIV DIVI DIVM DIVB ASH ROT 
LSH JFFO ASHC ROTC LSHC)))
(DEFINEOPCODERANGEFROM 168 (QUOTE (EXCH BLT AOBJP AOBJN JRST JFCL XCT MAP 
PUSHJ PUSH POP POPJ JSR JSP JSA JRA ADD ADDI ADDM ADDB SUB SUBI SUBM SUBB 
CAI CAIL CAIE CAILE CAIA CAIGE CAIN CAIG CAM CAML CAME CAMLE CAMA CAMGE CAMN 
CAMG)))
(DEFINEOPCODERANGEFROM 208 (QUOTE (JUMP JUMPL JUMPE JUMPLE JUMPA JUMPGE 
JUMPN JUMPG SKIP SKIPL SKIPE SKIPLE SKIPA SKIPGE SKIPN SKIPG AOJ AOJL AOJE 
AOJLE AOJA AOJGE AOJN AOJG AOS AOSL AOSE AOSLE AOSA AOSGE AOSN AOSG SOJ SOJL 
SOJE SOJLE SOJA SOJGE SOJN SOJG SOS SOSL SOSE SOSLE SOSA SOSGE SOSN SOSG)))
(DEFINEOPCODERANGEFROM 256 (QUOTE (SETZ SETZI SETZM SETZB AND ANDI ANDM ANDB 
ANDCA ANDCAI ANDCAM ANDCAB SETM SETMI SETMM SETMB ANDCM ANDCMI ANDCMM ANDCMB)))
(DEFINEOPCODERANGEFROM 276 (QUOTE (SETA SETAI SETAM SETAB XOR XORI XORM XORB 
IOR IORI IORM IORB ANDCB ANDCBI ANDCBM ANDCBB EQV EQVI EQVM EQVB SETCA 
SETCAI SETCAM SETCAB ORCA ORCAI ORCAM ORCAB SETCM SETCMI SETCMM SETCMB ORCM 
ORCMI ORCMM ORCMB ORCB ORCBI ORCBM ORCBB SETO SETOI SETOM SETOB)))
(DEFINEOPCODERANGEFROM 320 (QUOTE (HLL HLLI HLLM HLLS HRL HRLI HRLM HRLS 
HLLZ HLLZI HLLZM HLLZS HRLZ HRLZI HRLZM HRLZS HLLO HLLOI HLLOM HLLOS HRLO 
HRLOI HRLOM HRLOS HLLE HLLEI HLLEM HLLES HRLE HRLEI HRLEM HRLES HRR HRRI 
HRRM HRRS HLR HLRI HLRM HLRS HRRZ HRRZI HRRZM HRRZS HLRZ HLRZI HLRZM HLRZS 
HRRO HRROI HRROM HRROS HLRO HLROI HLROM HLROS HRRE HRREI HRREM HRRES HLRE 
HLREI HLREM HLRES)))
(DEFINEOPCODERANGEFROM 384 (QUOTE (TRN TLN TRNE TLNE TRNA TLNA TRNN TLNN TDN 
TSN TDNE TSNE TDNA TSNA TDNN TSNN TRZ TLZ TRZE TLZE TRZA TLZA TRZN TLZN TDZ 
TSZ TDZE TSZE TDZA TSZA TDZN TSZN TRC TLC TRCE TLCE TRCA TLCA TRCN TLCN TDC 
TSC TDCE TSCE TDCA TSCA TDCN TSCN TRO TLO TROE TLOE TROA TLOA TRON TLON TDO 
TSO TDOE TSOE TDOA TSOA TDON TSON)))
(DEFINEOPCODERANGEFROM 269 (QUOTE (XMOVEI)))