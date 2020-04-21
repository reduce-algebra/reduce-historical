;      psl:	ss:<psl>		! System-wide definition

define psys:	ss:<psl.subsys>		! Directory of executable files
define psl:	ss:<psl>,ss:<psl.subsys>

;OBJECT CODE FILES

define pl:	ss:<psl.lap>		! All PSL .B files live here
define plap:	ss:<psl.lap>

;SOURCE CODE, COMMAND FILES, (also .rel files)

define pu:	ss:<psl.util>		! Machine-independent loadable modules
define p20u:	ss:<psl.util-20>	! Dec-20 utility program sources
define pn:	ss:<psl.nmode>		! NMODE sources
define pnb:	ss:<psl.nmode-binary>	! NMODE binaries
define pw:	ss:<psl.windows>	! WINDOW PACKAGE sources
define pwb:	ss:<psl.windows-binary>	! WINDOW PACKAGE binaries

;DOCUMENTATION FILES

define plpt:	ss:<psl.lpt>		! Printable version of ref. manual
define pman:	ss:<psl.manual>		! Manual sources and working files
define pndoc:	ss:<psl.nmode-doc>	! Documentation for NMODE
define ph:	ss:<psl.help>		! xxx.HLP => help,
					! xxx.DOC => documentation of PU: file
define p20h:	ss:<psl.help-20>	! For the DEC-20
define pd:	ss:<psl.doc>		! Should be source and output files for
					!  formal documents (except the manual)
define p20d:	ss:<psl.doc-20>		! For the DEC-20

;MAINTAINER-ORIENTED ARCANA AND ESOTERICA (no erotica)

define pnew:	ss:<psl.new>		! Pre-release loadable files

take
