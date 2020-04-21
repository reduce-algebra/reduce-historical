;;; File of commands to transfer PSL support from HULK to THOR
;;; Cris Perdue 3-2-83

;;; The user this job runs under must have a CFTP.CMD file that
;;; logs in as guest and gives the guest password when connected to THOR.

cftp thor
take p20sup:cftp-thor.cmd

; The blank line after each wildcard send tells CFTP that its
; default destination is OK.

; Using "delete" makes this file liable to fail because if the
; deletion can't be done, a "?" message is put out, stopping the
; batch job.  There is enough extra space to make it unnecessary
; right now.

; Delete the .EXE files so there is room in the directory.
; delete psl.exe
; delete bare-psl.exe

send p20sup:thor-names.cmd
logical-names.cmd
expunge

send plap:*.b

expunge

send plap:*.lap

expunge

send ph:help.tbl
help.tbl
send ph:*.hlp

expunge

send pnb:*.b

expunge

send pwb:*.b

expunge

send psl:psl.exe
psl.exe
expunge

send psl:bare-psl.exe
bare-psl.exe
expunge

exit

reset .

submit p20sup:thor-xfer.ctl /after:+168:00 /restartable:yes
mail perdue, kendzierski
THOR file transfer
The weekly PSL file transfer to Thor has completed and next
week's job has been submitted.


