! Unused names with unknown purpose are commented out with a ";?".
! [WFG, U. of U.]
define psl:	ps:<psl>                ! System-wide definition
define pb:      ps:<psl.betty>		! Betty sources
define pc:	ps:<psl.comp>		! Machine-independent compiler sources
define p20c:	ps:<psl.comp.20>	! Dec-20 compiler sources
define p20ec:	ps:<psl.comp.20.ext>	! Extended Dec-20 compiler sources
define p68c:	ps:<psl.comp.68>	! 68000 compiler sources
define capollo: ps:<psl.comp.68.apollo> ! Apollo compiler sources
define pac:     ps:<psl.comp.68.apollo> ! Apollo compiler sources
define phpc:    ps:<psl.comp.68.hp>     ! Hp9836 compiler sources - fix name
define cwicat:  ps:<psl.comp.68.wicat>  ! wicat compiler sources
define pwc:     ps:<psl.comp.68.wicat>  ! wicat compiler sources
define pcrc:	ps:<psl.comp.cray>	! CRAY compiler sources
define pvc:	ps:<psl.comp.vax>	! Vax compiler sources
define pdist:   ps:<psl.dist>		! Distribution main directory
define p20dist:	ps:<psl.dist.20>	! Dec-20 distribution documents
define p68dist:	ps:<psl.dist.68>	! 68K distribution documents
define pcrdist:	ps:<psl.dist.cray>	! Cray distribution documents
define phpdist:	ps:<psl.dist.hp>	! HP distribution documents
define pvdist:	ps:<psl.dist.vax>	! Vax distribution
define pd:	ps:<psl.doc>		! Other documentation
define p20d:	ps:<psl.doc.20>		! Dec-20 Documentation 
define p68d:	ps:<psl.doc.68>		! 68000 Documentation
define pad:	ps:<psl.doc.68.apollo> 	! Apollo Documentation
define phpd:	ps:<psl.doc.68.hp>   	! hp9836 Documentation
define pwd:	ps:<psl.doc.68.wicat> 	! Wicat Documentation
define pcrd:	ps:<psl.doc.cray>	! CRAY Documentation
define pndoc:   ps:<psl.doc.nmode>	! NMODE Documentation
define pvd:	ps:<psl.doc.vax>	! Vax Documentation
define pe:	ps:<psl.emode>		! Emode sources and support
define pg:      ps:<psl.glisp>		! GLISP sources
define ph:	ps:<psl.help>		! Help files
define pk:	ps:<psl.kernel>         ! Machine-independent kernel sources
define p20:	ps:<psl.kernel.20>	! Dec-20 kernel sources
define p20e:	ps:<psl.kernel.20.ext>	! Extended Dec-20 kernel sources
define p68:	ps:<psl.kernel.68>	! 68000 kernel sources
define kapollo: ps:<psl.kernel.68.apollo> ! Apollo kernel sources
define pa:      ps:<psl.kernel.68.apollo> ! Apollo kernel sources
define php:     ps:<psl.kernel.68.hp>   ! hp9836 kernel (fix name)
define khp:     ps:<psl.kernel.68.hp>   ! Hp9836 kernel   sources
define kwicat:  ps:<psl.kernel.68.wicat> !wicat kernel sources
define pcr:	ps:<psl.kernel.cray>	! CRAY kernel sources
define p10x:	ps:<psl.kernel.tenex>	! Tenex and KI specific kernel sources
define pv:	ps:<psl.kernel.vax>	! Vax kernel sources
define pl:	ps:<psl.lap>		! Loadable files
define ple:	ps:<psl.lap.ext>	! Loadable files for extended 20
define plap:	ps:<psl.lap>		! Loadable files (untouched by search
					!                 path games)
define plpt:	ps:<psl.lpt>		! Printable version of documentation
define pm:      ps:<psl.manual>         ! The Psl Manual sources
define pnew:    ps:<psl.new> 		! New versions of anything
define pn:	ps:<psl.nmode>		! NMODE sources
define pne:	ps:<psl.nmode.ext>      ! Extended 20 NMODE binaries
define pnb:	ps:<psl.nmode.binary>   ! NMODE Binaries
define pnk:	ps:<psl.nonkernel>	! Machine-independent non-kernel
define p20nk:	ps:<psl.nonkernel.20>	! Dec-20 non-kernel
define pvnk:	ps:<psl.nonkernel.vax>	! Vax non-kernel
define pr:      ps:<psl.reduce>         ! Reduce files for PSL
define pred:    ps:<psl.reduce>         ! Reduce files for PSL
define psc:     ps:<psl.scratch>        ! Scratch area
define psup:	ps:<psl.support>	! Local PSL support stuff
define p20sup:	ps:<psl.support.20>	! Local PSL support stuff, 20 specific
define pasup:   ps:<psl.support.apollo>	! Local PSL support Apollo
define phpsup:  ps:<psl.support.hp>	! Local PSL support HP
define pvsup:	ps:<psl.support.vax>	! Local PSL support stuff, Vax spcific
define pt:      ps:<psl.tests>          ! Test directory
define p20t:    ps:<psl.tests.20>       ! 20 sub-case
define phpt:	ps:<psl.tests.hp>	! hp sub-case
define pvt:	ps:<psl.test.vax>	! vax sub-case
define ptr:     ps:<psl.trash>		! Trash to be backed up and discarded.
define putah:   ps:<psl.utah>		! Utah specific files.
define pu:	ps:<psl.util>		! Machine-independent utility programs
define p20u:	ps:<psl.util.20>	! Dec-20 utility program sources
define p20eu:	ps:<psl.util.20.ext>	! Extended Dec-20 utility program srcs
define phpu:	ps:<psl.util.hp>	! HP utility program sources
define pvu:	ps:<psl.util.vax>	! Vax utility program sources
define pw:	ps:<psl.windows>	! WINDOW PACKAGE sources
define pwb:	ps:<psl.windows.binary>	! WINDOW PACKAGE binaries
; A few others to make things nice
define pi:	pk:
take
