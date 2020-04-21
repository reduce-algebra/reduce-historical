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

*restore <*>*.* PSL:*.*  
*skip 4
*restore <*>*.* PE:*.*
*skip 1
*restore <*>*.* PH:*.*  
*skip 2
*restore <*>*.* PL:*.*  
*skip 1
*restore <*>*.* PN:*.*
*skip 3
*restore <*>*.* PU:*.*  
*skip 1
*restore <*>*.* PW:*.*
 
*rewind
*unload
*exit
