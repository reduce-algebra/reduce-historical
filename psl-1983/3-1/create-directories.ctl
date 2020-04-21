; Please edit this, and replace all <psl with <yourpslname
@build <psl>
@@perm 6400           	! choose appropriate size
@@work 6400		! nnnn+extra
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@max 30
@@
; 5230 pages for following.  PSL: needs about 1100.
; Single directory, partial restore needs about 1300 below and 1100 above.
@build <psl.comp>
@@perm 180           	! choose appropriate size
@@work 180		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@max 1
@@
@build <psl.comp.20>
@@perm 55           	! choose appropriate size
@@work 55		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@
@build <psl.dist>
@@perm 25           	! choose appropriate size
@@work 25		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@
@build <psl.doc>
@@perm 725           	! choose appropriate size
@@work 725		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@max 2
@@
@build <psl.doc.20>
@@perm 25           	! choose appropriate size
@@work 25		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@
@build <psl.doc.nmode>
@@perm 590           	! choose appropriate size
@@work 590		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@
@build <psl.glisp>
@@perm 330           	! choose appropriate size
@@work 330		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@
@build <psl.help>
@@perm 100           	! choose appropriate size
@@work 100		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@
@build <psl.kernel>
@@perm 785           	! choose appropriate size
@@work 785		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@max 1
@@
@build <psl.kernel.20>
@@perm 560          	! choose appropriate size
@@work 560		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@
@build <psl.lap>
@@perm 500           	! choose appropriate size
@@work 500		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@
@build <psl.lpt>
@@perm 430          	! choose appropriate size
@@work 430		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@
@build <psl.nmode>
@@perm 510           	! choose appropriate size
@@work 510		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@max 1
@@
@build <psl.nmode.binary>
@@perm 230           	! choose appropriate size
@@work 230		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@
@build <psl.nonkernel>
@@perm 5           	! choose appropriate size
@@work 5		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@
@build <psl.tests>
@@perm 715           	! choose appropriate size
@@work 715		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@max 1
@@
@build <psl.tests.20>
@@perm 500          	! choose appropriate size
@@work 500		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@
@build <psl.util>
@@perm 635           	! choose appropriate size
@@work 635		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@max 1
@@
@build <psl.util.20>
@@perm 60           	! choose appropriate size
@@work 60		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@
@build <psl.windows>
@@perm 105           	! choose appropriate size
@@work 105		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@max 1
@@
@build <psl.windows.binary>
@@perm 30           	! choose appropriate size
@@work 30		! increase this as needed
@@files-only		! Cant login
@@gen 2			! Retain 1 previous version
@@protection 777700   	! Give group access
@@default    777700     ! Give group access
@@
