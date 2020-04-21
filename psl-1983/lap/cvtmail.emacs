!~Filename~:! !For dealing with PSL bug reports.!
CVTMAIL

!Cut Header:! !C Removes unwanted fields from a mail header.
One must already be positioned at the start of a mail header.
Cursor is left at the beginning of the next mail header.!
[1 [2
k
.u1
-l .,.+9:fb-------		    !* Kill preceding mail trailer, if any!
"L -l ki
'"# q1j'
MM&_Fix_Mail-From
l				    !* Skip initial date line!
!loop!				    !* Kill uninteresting header lines!
.u1 l .-q1-2"E Odone'
q1j
.,.+6:fbFrom:_ "LOmatch'
.,.+9:fbSubject:_ "LOmatch'
.,.+7:fbClass:_ "LOmatch'
k Oloop
!match!
l Oloop
!done!
MM^R_Set/Pop_Mark
<MM&_Header?			    !* Find a mail header line!
 q0"E l'"# 1;'			    !* Exit loop if found!
>
-l
2MM^R_Indent_Rigidly		    !* Indent the body of the message!
l


!& Header?:! !C -1 if current line is header line else 0.!
.u0 0l
z-.-24 :"G Onomatch'
3a-- "N Onomatch'
7a-- "N Onomatch'
13a-: "N Onomatch'
16a-: "N Onomatch'
19a-- "N Onomatch'
23a-, "N Onomatch'
q0j
-1u0

!nomatch!
q0j
0u0


!& Fix Mail-From:! !C Fixes up any initial "Mail-from:" line.
Some "date" lines actually begin with "Mail-from" and contain
additional information not wanted here.  Cursor is left at the
beginning of the same line it started on.!
.,.+10:FBMail-from: :"L Oend'
0l
iDate:
1MM^R_Kill_Word
1MM^R_Kill_Word
1MM^R_Kill_Word
1MM^R_Kill_Word
!end!
0l


!Reverse Mail List:! !C Reverses a bufferful of mail messages.
The idea is to move forward through the file putting messages
found later in front of all found sooner.!
[0 [1 [2 [3
.u2				    !* q2 has loc of last header found!
<
 .-z "E '			    !* Stop reversing if at end of buffer!

 <				    !* Find "end of message"!
  l				    !* Go to next line!
  .-z @;			    !* Exit if at end of buffer!
  MM&_Header?
  q0 :@;			    !* Exit if header line (q0 nonzero)!
 >
				    !* End of message now found!
 q2u1				    !* Now q1 has prev. header!
 .u2				    !* q2 has next header loc!
 q1,q2x3			    !* Save message in q3!
 q1,q2k				    !* Kill message!
 bj g3				    !* Put at front of buffer!
 q2j				    !* Go to where left off!
>

