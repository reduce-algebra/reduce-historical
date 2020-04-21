! Master PSL Tape Copy
! 12:31 pm  Friday, 22 April 1983
@enable ! so operators can read the files
@set account small
@assign mta0:
@assign mta1:
@MTU
Tape mta0:
Copy mta1:
rew
tape mta1:
unload
exit
@deas mta0:
@deas mta1:
