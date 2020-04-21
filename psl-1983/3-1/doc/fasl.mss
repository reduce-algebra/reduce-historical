@make(article)
@section(How in the hell does faslout work???)
This section is a guide to the internal workings of faslout and then
faslin.

The user begins the faslout procedure by calling the procedure faslout with
a string that does not have the extension (because it will add the
appropriate binary extension for you).  However, when fasling in, the file
name requires the binary extension [Change this inconsistency].  

Inside the procedure faslout, the file name is assigned to the fluid
variable ModuleName!*.  Depending upon the setting of the flag
!*Quiet_Faslout, the system will either print out a greeting message or
not.  Next, an output binary file is opened using the argument file name.
It will return the channel number to a fluid variable CodeOut!*.
CodeFileHeader is called to put in a header in the output file.  

CodeFileHeader writes out a word consisting of the Fasl Magic Number
(currently set to 99).  This magic word is used to check consistency
between old and current fasl format files (an error is given upon fasling
in the file if there is not a 99 as the first word).  Therefore, the system
must consistently modify that number when a new fasl format is produced.
To continue, we need to understand the allocation that takes place within
the Binary Program Space (BPS).  The BPS is a large, non-collected space
that contains compiled code, warrays, the string assocaited with interned
ID's, constant data in fasl files, etc.  Space is allocated from both
ends of the space.  Compiled code is allocated from the bottom (using
NextBPS as a pointer) and warrays are allocated from the top (using LastBPS
as the pointer).  When an allocation is attempted, the desired size is
checked to see if it will cause LastBPS and NextBPS to cross; if it will,
an error message will be printed.  The next step is to allocate 2/3 or the
remaining BPS from the top.
@begin(verbatim)

         .------------------------------------.
         |                                    |
         |     WArrays                        |
         |                                    |
         |                                    |
Last_BPS>|------------------------------------| <-FaslBlockEnd!* ---.
         |      Code                          |                     |  
         |                                    |                     |
         |                                    |                     |
         |                                    |                    2/3
         |====================================| <-CodeBase!*        |
         |      Bit Table                     |                     |
         |====================================| <-BitTableBase!* ---'
         |                                    |
         |                                    |
Next_BPS>|------------------------------------|
         |                                    |
         |                                    |
         |                                    |
         `------------------------------------'

               Binary Program Space

@end(verbatim)
The procedure AllocateFaslSpaces will setup the following fluid variables.
FaslBlockEnd!* will be the address to the top of the available space for
this particular allocation.

BitTableBase!* points to the beginning of the BitTable.

CurrentOffset!* keeps a pointer into the codespace of this allocation to
the next available point to add more code.

BitTableOffset!* is a running pointer to the current location in the
BitTable where the next entry will go. 

CodeBase!* is the base pointer to the beginning of the code segment for
this allocation.

MaxFaslOffset!* is the max size of the codespace allowed for this
implementation.

OrderedIDList!* keeps record of the ID's as they are added.

NextIDNumber!* is a base number used just in fasl files to indicate which
IDs are local and which are global. It is assumed that there will never be
more than 2048 pre-allocated ID's, currently there are 129. The first 128
preallocated IDs are ASCII codes(0-127) and the last one is NIL(128).

Everything is now setup to begin fasling PSL code out to the file.
The remainder of the faslout procedure sets up three more fluid variables.

!*DEFN is set to T which indicates that you are not going to do normal
evaluation from the top loop and from files such as using the functions IN
and DSKIN.

DFPRINT!* signals that DFPRINT!* is now used as the printing function.
The procedure used will be DFPRINTFasl!*.

!*WritingFaslFile is set to T to let the system know that fasling out is
goping on as opposed to compiling code directly into memory inside the PSL
system.


@section(What happens to code being fasled out to a file)

