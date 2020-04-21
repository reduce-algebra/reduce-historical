; Used to retrieve ALL ssnames for FULL PSL system
; First edit FULL-LOGICAL-NAMES.CMD to reflect <name>
; then TAKE to install names
; then BUILD sub-directories
; then mount TAPE, def X:
@TERM PAGE 0
@DUMPER
*tape X:
*density 1600
*files
*account system-default

*; --- Skip over the logical names etc to do the restore.
*skip 1
*restore dsk*:<*>*.*.* PSL:*.*.* 
*restore dsk*:<*>*.*.* PC:*.*.*
*restore dsk*:<*>*.*.* P20C:*.*.*  
*restore dsk*:<*>*.*.* PDIST:*.*.*
*restore dsk*:<*>*.*.* PD:*.*.*
*restore dsk*:<*>*.*.* P20D:*.*.*
*restore dsk*:<*>*.*.* PNDOC:*.*.*
; not distributed anymore *restore dsk*:<*>*.*.* PE:*.*.*
*restore dsk*:<*>*.*.* PG:*.*.* 
*restore dsk*:<*>*.*.* ph:*.*.*
*restore dsk*:<*>*.*.* pk:*.*.*
*restore dsk*:<*>*.*.* p20:*.*.*
*restore dsk*:<*>*.*.* pl:*.*.*
*restore dsk*:<*>*.*.* plpt:*.*.*
*restore dsk*:<*>*.*.* pn:*.*.*
*restore dsk*:<*>*.*.* pnb:*.*.*
*restore dsk*:<*>*.*.* pnk:*.*.*
*restore dsk*:<*>*.*.* pT:*.*.*
*restore dsk*:<*>*.*.* p20T:*.*.*
*restore dsk*:<*>*.*.* pu:*.*.*
*restore dsk*:<*>*.*.* p20u:*.*.*
*restore dsk*:<*>*.*.* pw:*.*.*
*restore dsk*:<*>*.*.* pwb:*.*.*
