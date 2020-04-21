; Used to retrieve subset of ssnames for MINIMAL PSL system
; First edit MINIMAL-LOGICAL-NAMES.CMD to reflect <name>
; then TAKE to install names
; then BUILD sub-directories or single directory
; then mount TAPE, def X:
@DUMPER
*tape X:
*density 1600
*files
*account system-default

*; --- Skip over the logical names etc to do the restore.
*skip 1
*restore dsk*:<*>*.*.* PSL:*.*.* 
; --- not needed --- *restore dsk*:<*>*.*.* PC:*.*.*
*skip 1
; --- not needed --- *restore dsk*:<*>*.*.* P20C:*.*.*  
*skip 1
; --- not needed --- *restore dsk*:<*>*.*.* PDIST:*.*.*
*skip 1
; --- not needed --- *restore dsk*:<*>*.*.* PD:*.*.*
*skip 1
; --- not needed --- *restore dsk*:<*>*.*.* P20D:*.*.*
*skip 1
; --- not needed --- *restore dsk*:<*>*.*.* PNDOC:*.*.*
*skip 1
; --- not distributed anymore --- *restore dsk*:<*>*.*.* pe:*.*.*
; --- not needed --- *restore dsk*:<*>*.*.* pg:*.*.* 
*skip 1
*restore dsk*:<*>*.*.* ph:*.*.*
; --- not needed --- *restore dsk*:<*>*.*.* pk:*.*.*
*skip 1
; --- not needed --- *restore dsk*:<*>*.*.* p20:*.*.*
*skip 1
*restore dsk*:<*>*.*.* pl:*.*.*
; --- not needed --- *restore dsk*:<*>*.*.* plpt:*.*.*
*skip 1
; --- not needed --- *restore dsk*:<*>*.*.* pn:*.*.*
*skip 1
*restore dsk*:<*>*.*.* pnb:*.*.*
; --- not needed --- *restore dsk*:<*>*.*.* pnk:*.*.*
*skip 1
; --- not needed --- *restore dsk*:<*>*.*.* pT:*.*.*
*skip 1
; --- not needed --- *restore dsk*:<*>*.*.* p20T:*.*.*
*skip 1
; --- not needed --- *restore dsk*:<*>*.*.* pu:*.*.*
*skip 1
; --- not needed --- *restore dsk*:<*>*.*.* p20u:*.*.*
*skip 1
; --- not needed --- *restore dsk*:<*>*.*.* pw:*.*.*
*skip 1
*restore dsk*:<*>*.*.* pwb:*.*.*
 
