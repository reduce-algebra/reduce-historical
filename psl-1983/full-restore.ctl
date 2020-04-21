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

*restore <*>*.*.* PSL:*.*.* 
*restore <*>*.*.* PC:*.*.*
*restore <*>*.*.* P20C:*.*.*  
*restore <*>*.*.* PD:*.*.*
*restore <*>*.*.* PND:*.*.*
*restore <*>*.*.* PE:*.*.*
*restore <*>*.*.* PG:*.*.* 
*restore <*>*.*.* ph:*.*.*
*restore <*>*.*.* pk:*.*.*
*restore <*>*.*.* p20K:*.*.*
*restore <*>*.*.* pl:*.*.*
*restore <*>*.*.* plpt:*.*.*
*restore <*>*.*.* pn:*.*.*
*restore <*>*.*.* pnk:*.*.*
*restore <*>*.*.* pT:*.*.*
*restore <*>*.*.* p20T:*.*.*
*restore <*>*.*.* pu:*.*.*
*restore <*>*.*.* p20u:*.*.*
*restore <*>*.*.* pw:*.*.*
 
*rewind
*unload
*exit
