take psl:psl-names	! Defines names commented out here

;      psl:	ss:<psl>		! System-wide definition

;define psys:	ss:<psl.subsys>		! Directory of executable files
;define psl:	ss:<psl>,ss:<psl.subsys>

;OBJECT CODE FILES

;define pl:	ss:<psl.lap>		! All PSL .B files live here
;define plap:	ss:<psl.lap>
		! Loadable files (untouched by search path games)

;SOURCE CODE, COMMAND FILES, (also .rel files)

define pk:	ss:<psl.kernel>		! Machine-independent kernel sources
define pi:	pk:			! Old logical name for kernel stuff
define pcr:	ss:<psl.kernel-cray>	! cray kernel sources
define p20:	ss:<psl.kernel-20>	! Dec-20 kernel sources
define pv:	ss:<psl.kernel-vax>	! Vax kernel sources
define php:     ss:<psl.kernel-hp9836>  ! hp9836 kernel
define phpp:	ss:<psl.kernel-hp9836-pascal> ! Pascal sources for HP9836
define p68:	ss:<psl.kernel-68>	! 68000 kernel sources
define p10x:	ss:<psl.kernel-tenex>	! Tenex and KI specific kernel sources

define pnk:	ss:<psl.nonkernel>	! Machine-independent non-kernel
define p20nk:	ss:<psl.nonkernel-20>	! Dec-20 non-kernel
define pvnk:	ss:<psl.nonkernel-vax>	! Vax non-kernel

define pc:	ss:<psl.comp>		! Machine-independent compiler sources
define pcrc:	ss:<psl.comp-cray>	! CRAY compiler sources
define p20c:	ss:<psl.comp-20>	! Dec-20 compiler sources
define pvc:	ss:<psl.comp-vax>	! Vax compiler sources
define p68c:	ss:<psl.comp-68>	! 68000 compiler sources
define phpc:    ss:<psl.comp-hp9836>    ! Hp9836 compiler sources - fix name

;define pu:	ss:<psl.util>		! Machine-independent loadable modules
;define p20u:	ss:<psl.util-20>	! Dec-20 utility program sources
define pvu:	ss:<psl.util-vax>	! Vax utility program sources
define phpu:	ss:<psl.util-hp9836>	! Hp9836 utility program sources

;define pn:	ss:<psl.nmode>		! NMODE sources and binaries
define pe:	ss:<psl.emode>		! EMODE sources
;define pw:	ss:<psl.windows>	! WINDOW PACKAGE sources and binaries
define pg:	ss:<glisp>		! GLISP, not a subdirectory at HP . . .

;DOCUMENTATION FILES

;define plpt:	ss:<psl.lpt>		! Printable version of ref. manual
;define pman:	ss:<psl.manual>		! Manual sources and working files
;define pndoc:	ss:<psl.nmode-doc>	! Documentation for NMODE

;define ph:	ss:<psl.help>		! xxx.HLP => help,
					! xxx.DOC => documentation of PU: file
;define p20h:	ss:<psl.help-20>	! For the DEC-20
define pvh:	ss:<psl.help-vax>	! For the VAX
define phph:	ss:<psl.help-hp9836>	! For the HP9836

define p20dist:	ss:<psl.dist-20>	! Dec-20 distribution docs and tools
define pvdist:	ss:<psl.dist-vax>	! Vax distribution docs and tools
define phpdist:	ss:<psl.dist-hp9836>	! HP9836 distribution docs and tools
define padist:	ss:<psl.dist-apollo>	! Apollo distribution docs and tools

;define pd:	ss:<psl.doc>		! Should be source and output files for
					!  formal documents (except the manual)
;define p20d:	ss:<psl.doc-20>		! For the DEC-20
define pvd:	ss:<psl.doc-vax>	! For VAX
define phpd:	ss:<psl.doc-hp9836>	! For HP9836
define pad:	ss:<psl.doc-apollo>	! For Apollo

;MAINTAINER-ORIENTED ARCANA AND ESOTERICA (no erotica)

! Files for pl: not generated, e.g. from .sl, .red files
define p20l:	ss:<psl.lap-20>
define pvl:	ss:<psl.lap-vax>
define phpl:	ss:<psl.lap-hp9836>

! Files that belong on "psl:" on the "target" machine, but not
!  necessarily on "psl:" on the central file repository machine.
define p20psl:	ss:<psl.psl-20>
define pvpsl:	ss:<psl.psl-vax>
define phppsl:	ss:<psl.psl-hp9836>

define psup:	ss:<psl.support>	! PSL support stuff
define p20sup:	ss:<psl.support-20>	! PSL support stuff, 20 specific
define pvsup:	ss:<psl.support-vax>	! PSL support stuff, Vax spcific
define phpsup:	ss:<psl.support-hp9836>	! PSL support stuff, Hp9836
define pasup:	ss:<psl.support-apollo>	! For Apollo

;define pnew:	ss:<psl.new>		! Pre-release loadable files
define s:	ss:<psl.scratch>	! Scratch directory

define pt:      ss:<psl.tests>          ! Test directory
define p20t:    ss:<psl.tests-20>       ! 20 sub-case
define phpt:    ss:<psl.tests-hp9836>   ! hp9836 sub-case
take
