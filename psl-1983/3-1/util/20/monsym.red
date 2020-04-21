%
% MONSYM.RED - Support for Dec-20 system LAP code
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        19 March 1982
% Copyright (c) 1982 University of Utah
%

CompileTime <<

macro procedure DefineJSYSRangeFrom X;
begin scalar Start, L;
    Start := Sub1 second X;
    L := third X;
    return ('progn
	     . for each Name in second L collect
		list('progn, list('put, MkQuote Name,'(quote JSYSValue),
					Start := Add1 Start),
			     list('put,MkQuote Name,
                               '(quote InstructionDepositFunction),
                                  '(quote JSYSDeposit))));
end;

>>;

lisp procedure JSYSDeposit X;
<<  if !*WritingFaslFile then UpdateBitTable(1, 0);
    DepositAllFields(8#104, 0, get(car X, 'JSYSValue)) >>;

flag('(ERJMP ERCAL), 'MC);

lisp procedure ERJMP Address;
    list list('jump, 8#16, Address);

lisp procedure ERCAL Address;
    list list('jump, 8#17, Address);

DefineJSYSRangeFrom(1, '(
	LOGIN
	CRJOB
	LGOUT
	CACCT
	EFACT
	SMON
	TMON
	GETAB
	ERSTR
	GETER
	GJINF
	TIME
	RUNTM
	SYSGT
	GNJFN
	GTJFN
	OPENF
	CLOSF
	RLJFN
	GTSTS
	STSTS
	DELF
	SFPTR
	JFNS
	FFFFP
	RDDIR
	CPRTF
	CLZFF
	RNAMF
	SIZEF
	GACTF
	STDIR
	DIRST
	BKJFN
	RFPTR
	CNDIR
	RFBSZ
	SFBSZ
	SWJFN
	BIN
	BOUT
	SIN
	SOUT
	RIN
	ROUT
	PMAP
	RPACS
	SPACS
	RMAP
	SACTF
	GTFDB
	CHFDB
	DUMPI
	DUMPO
	DELDF
	ASND
	RELD
	CSYNO
	PBIN
	PBOUT
	PSIN
	PSOUT
	MTOPR
	CFIBF
	CFOBF
	SIBE
	SOBE
	DOBE
	GTABS
	STABS
	RFMOD
	SFMOD
	RFPOS
	RFCOC
	SFCOC
	STI
	DTACH
	ATACH
	DVCHR
	STDEV
	DEVST
	MOUNT
	DSMNT
	INIDR
	SIR
	EIR
	SKPIR
	DIR
	AIC
	IIC
	DIC
	RCM
	RWM
	DEBRK
	ATI
	DTI
	CIS
	SIRCM
	RIRCM
	RIR
	GDSTS
	SDSTS
	RESET
	RPCAP
	EPCAP
	CFORK
	KFORK
	FFORK
	RFORK
	RFSTS
	SFORK
	SFACS
	RFACS
	HFORK
	WFORK
	GFRKH
	RFRKH
	GFRKS
	DISMS
	HALTF
	GTRPW
	GTRPI
	RTIW
	STIW
	SOBF
	RWSET
	GETNM
	GET
	SFRKV
	SAVE
	SSAVE
	SEVEC
	GEVEC
	GPJFN
	SPJFN
	SETNM
	FFUFP
	DIBE
	FDFRE
	GDSKC
	LITES
	TLINK
	STPAR
	ODTIM
	IDTIM
	ODCNV
	IDCNV
	NOUT
	NIN
	STAD
	GTAD
	ODTNC
	IDTNC
	FLIN
	FLOUT
	DFIN
	DFOUT
));

DefineJSYSRangeFrom(160, '(
	CRDIR
	GTDIR
	DSKOP
	SPRIW
	DSKAS
	SJPRI
	STO
	ARCF
));

%define(jsASNDP,8%260)			# NOT IMPLEMENTED
%define(jsRELDP,8%261)			# NOT IMPLEMENTED
%define(jsASNDC,8%262)			# NOT IMPLEMENTED
%define(jsRELDC,8%263)			# NOT IMPLEMENTED
%define(jsSTRDP,8%264)			# NOT IMPLEMENTED
%define(jsSTPDP,8%265)			# NOT IMPLEMENTED
%define(jsSTSDP,8%266)			# NOT IMPLEMENTED
%define(jsRDSDP,8%267)			# NOT IMPLEMENTED
%define(jsWATDP,8%270)			# NOT IMPLEMENTED

DefineJSYSRangeFrom(188, '(
	ATNVT
	CVSKT
	CVHST
	FLHST
	GCVEC
	SCVEC
	STTYP
	GTTYP
	BPT
	GTDAL
	WAIT
	HSYS
	USRIO
	PEEK
	MSFRK
	ESOUT
	SPLFK
	ADVIS
	JOBTM
	DELNF
	SWTCH
	TFORK
	RTFRK
	UTFRK
));

DefineJSYSRangeFrom(214, '(
	OPRFN
	CGRP
	VACCT
	GDACC
	ATGRP
	GACTJ
	GPSGN
));

DefineJSYSRangeFrom(320, '(
	RSCAN
	HPTIM
	CRLNM
	INLNM
	LNMST
	RDTXT
	SETSN
	GETJI
	MSEND
	MRECV
	MUTIL
	ENQ
	DEQ
	ENQC
	SNOOP
	SPOOL
	ALLOC
	CHKAC
	TIMER
	RDTTY
	TEXTI
	UFPGS
	SFPOS
	SYERR
	DIAG
	SINR
	SOUTR
	RFTAD
	SFTAD
	TBDEL
	TBADD
	TBLUK
	STCMP
	SETJB
	GDVEC
	SDVEC
	COMND
	PRARG
	GACCT
	LPINI
	GFUST
	SFUST
	ACCES
	RCDIR
	RCUSR
));

DefineJSYSRangeFrom(488, '(
	SNDIM
	RCVIM
	ASNSQ
	RELSQ
));

DefineJSYSRangeFrom(504, '(
	THIBR
	TWAKE
	MRPAC
	SETPV
	MTALN
	TTMSG
));

END;
