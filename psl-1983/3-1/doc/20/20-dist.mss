@make(article)
@Case(Draft, 1 <@device(Omnitech)>,
             else <@device(LPT)>
      )
@Style(WidowAction=warn)
@Style(Hyphenation Off) @comment(on)
@Style(DoubleSided no) @comment(yes)
@style(Spacing 1)
@use(Bibliography "<griss.docs>mtlisp.bib")
@modify(enumerate,numbered=<@a. @,@i. >, spread 1)
@modify(itemize,spread 1)
@pageheading(Left  "Utah Symbolic Computation Group",
             Right "June 1983",
             Line "Operating Note No. xx"
            )
@set(page=1)
@newpage()
@Begin(TitlePAge)
@begin(TitleBox)
@center[Release Notes

@b(Extended DEC-20 V3.1 PSL System)


M. L. Griss and R. R. Kessler

Utah Symbolic Computation Group
Computer Science Department
University of Utah
Salt Lake City, Utah 84112
(801)-581-5017

@value(date)]
@end(TitleBox)
@begin(abstract)
This note describes how to install the extended DEC-20 version of PSL.
@end(abstract)
@begin(ResearchCredit)
Work supported in part by the National Science Foundation
under Grants MCS80-07034 and MCS81-21750, and by development 
grants from Boeing, Patil Systems,
Lucas Film, Wicat and Hewlett Packard.
@end(ResearchCredit)
@end(TitlePage)
@pageheading(Left  "DEC-20 PSL Release",
             Right "Page @Value(Page)"
            )
@newpage()
@section(INTRODUCTION)

     The attached DUMPER format tape contains most of the files needed to
use and maintain the DEC-20 PSL system. At UTAH we have a <PSL> main
directory, with a number of sub-directories, each containing a separate
class of file, such as common interpreter and compiler sources, DEC-20
sources, VAX sources, 68000 sources, help files, etc.  This multi-directory
structure enables us to manage the sources for all machines in a reasonable
way. Most people running PSL on the DEC-20 will not be interested in all of
the files, and certainly will not want to have them all on line.

     We have therefore created the tape to enable either a multi-directory
or single directory model; a set of logical device definitions will be
TAKEn by the user (usually inserted in the LOGIN.CMD file). Each separate
distribution directory is a separate SAVESET on the attached dumper format
tape, and so may be individually restored into a common (<PSL> at Utah)
directory, or into appropriate sub-directories (<PSL.*> at Utah).

@section(DISCLAIMER)

     Please be aware that this is a PRELIMINARY release, and some of the
files and documentation are not quite complete; we may also have forgotten
some files, or sent incorrect versions. We are releasing this preliminary
version to you at this time to enhance our collaborative research, and we
expect the files to continue to change quite rapidly as the system and
distribution is tested.

     For these reasons please:
@begin(enumerate)
Make a note of ANY problems, concerns, suggestions you have, and
send this information to us to aid in improving the system and this
distribution mechanism.

Please do not REDISTRIBUTE any of these files, listings or machine
readable form to anyone, and try to restrict access to a small group
of users.
@end(enumerate)
@section(CONTENTS OF THE TAPE)
     Attached to this note is a copy of the DUMPER run that created the
tape, indicating the savesets, the file names, and sizes needed to restore
each saveset.

The following lists each of the savesets, their logical names, sizes and
whether or not it is included in the saveset:
@begin(Description, spread 1)
SSname@ @ Pages@ Min@ <Utah@ File@ Name>@ Logical@ Name 

RESTORE-PSL@ 10@ NO@ @ @ ----@ @ @ @ @ @ @ @ @ @ @ @ ----
@\Files necessary to restore the PSL system.

PSL@ @ @ @ @ 1100@ @ YES@ @ <psl>@ @ @ @ @ @ @ @ @ @ @ @ psl: 
@\The executable files (PSL.EXE and RLISP.EXE), 
this 20-DIST.DOC file,
.CMD files to define appropriate logical names and a
sample message to announce PSL availability.  Also, included are a number
of news files announcing new features and changes, some files associated
with the NMODE editor and a version of psl (PSLCOMP.EXE) that will compile
the argument on the execution line.

COMP@ @ @ @ @ 125@ @ NO@ @ @ <psl.comp>@ @ @ @ @ @ @ pc:
@\Common compiler, LAP, FASL sources.

20COMP@ @ @ @ 55@ @ NO@ @ @ <psl.comp.20>@ @ @ @ p20c:
@\DEC-20 specific compiler, LAP and FASL sources.

DIST@ @ @ @ @ @ 25@ @ NO@ @ @ <psl.dist>@ @ @ @ @ @ @ pdist:
@\Files as an aid to the installer.

DOC@ @ @ @ @ @ 110@ @ NO@ @ @ <psl.doc>@ @ @ @ @ @ @ @ pdoc:
@\Miscellaneous documentation files, including random notes on new
features.

20DOC@ @ @ @ @ 25@ @ NO@ @ @ <psl.doc.20>@ @ @ @ @ p20d:
@\Documentation files that are 20 specific.

DOCNMODE@ 590@ @ NO@ @ @ <psl.doc.nmode>@ @ pndoc:
@\NMODE documentation files.

GLISP@ @ @ @ 330@ @ NO@ @ @ <psl.glisp>@ @ @ @ @ @ pg:
@\An object oriented LISP.

HELP@ @ @ @ @ 100@ @ YES@ @ <psl.help>@ @ @ @ @ @ @ ph:
@\A set of *.HLP files, describing major modules.

KERNEL@ @ @ 225@ @ NO@ @ @ <psl.kernel>@ @ @ @ @ pk:
@\Machine Independent kernel sources.

P20@ @ @ @ @ @ 560@ @ NO@ @ @ <psl.kernel.20>@ @ p20:
@\DecSystem 20 dependent kernel sources.

LAP@ @ @ @ @ @ 500@ @ YES@ @ <psl.lap>@ @ @ @ @ @ @ @ pl:
@\Mostly binary FASL (*.B) files, with some
LISP files (*.LAP) for
loading multiple .B files of loadable (optional) modules.

LPT@ @ @ @ @ @ 430@ @ NO@ @ @ <psl.lpt>@ @ @ @ @ @ @ @ plpt:
@\The PSL manual in printable form (has overprinting and underlining), 
as SCRIBE .LPT files.

NMODE@ @ @ @ 270@ @ NO@ @ @ <psl.nmode>@ @ @ @ @ @ pn:
@\The NMODE text editor sources, which is
a newer version of EMODE developed at HP Research Laboratories.

NMODEBIN@ 230@ @ YES@ @ <psl.nmode.binary>@ pnb:
@\The binary files associated with NMODE.

NONKERNEL@ @ 5@ @ NO@ @ @ <psl.nonkernel>@ @ pnk:
@\The sources that are not in the kernel, 
but are kernel related. 

PT@ @ @ @ @ @ @ 215@ @ NO@ @ @ <psl.tests>@ @ @ @ @ @ pt:
@\A set of timing and test files.

P20T@ @ @ @ @ 500@ @ NO@ @ @ <psl.tests.20>@ @ @ p20t:
@\DecSystem 20 specific test files.

UTIL@ @ @ @ @ 575@ @ NO@ @ @ <psl.util>@ @ @ @ @ @ @ pu:
@\Sources for most utilities, useful as examples of
PSL and RLISP code, and for customization.

P20U@ @ @ @ @ @ 60@ @ NO@ @ @ <psl.util.20>@ @ @ @ p20u:
@\DecSystem 20 specific utilities.

WINDOWS@ @ @ 75@ @ NO@ @ @ <psl.windows>@ @ @ @ pw:
@\The window support functions used by NMODE.

WINBIN@ @ @ @ 30@ @ YES@ @ <psl.windows.binary>@ pwb:
@\The binaries associated with the window support.
@end(description)
@section(INSTALLING PSL)

When installing the PSL system, you have two options for the directory
structure.  You may utilize a single directory for all of the file, or you
may create a directory tree using subdirectories.  The Utah group utilizes a
directory tree structure and recommends its use when installing a "full" system
(that includes all of the sources and the capability of rebuilding any part
of the system).  However, if only a minimal system is desired, it can be
accomplished using a single directory.

@subsection(Retrieve Control Files)

Whether building a single directory system or multiple directory system,
logical name definition files and file restore control files must be first
retrieved.  Therefore, first mount the dumper tape, at 1600 BPI (verify
that there is no write ring in the tape).  Then, define X: as the
appropriate tape device, MTAn:, or use MOUNT if running a labeled tape
system:
@verbatim[
@@DEFINE X: MTAn:             or    @@MOUNT TAPE X:
@@ASSIGN X:
]

Restore from the first saveset (PSL) the .cmd and .ctl files
@begin(verbatim)
   @@DUMPER
   *tape X:
   *density 1600
   *files
   *account system-default
   *restore <*>*.*.* *.*.*
@end(verbatim)
These files will be restored to your connected directory, and should be
copied to your main PSL directory after their creation.

@subsection(Create a single subdirectory)
Create a directory, call it <name> and define a logical device PSL:
(a size of about 2400 should be sufficient).
  
Any <name> will do, since the logical device name PSL: will be used.
@begin(verbatim)
   @@DEF PSL: <name>
@end(verbatim)

Copy the minimal-* restored files to PSL
@begin(verbatim)
   @@COPY minimal-*.* PSL:*.*
@end(verbatim)

Now edit the file PSL:minimal-logical-names.cmd to reflect the your choice
of <name>.

Also put @@TAKE <name>minimal-logical-names.cmd in your LOGIN.CMD.

Finally, restore the minimal system by DOing the minimal-restore.ctl file:
@begin(verbatim)
   @@DO MINIMAL-RESTORE
   @@DEASSIGN X:          or             @@DISMOUNT  X:
@end(verbatim)

@subsection(A MULTIPLE SUB-DIRECTORY SYSTEM)
If you plan to do much source modification, or a significant number of
rebuilds, or maintain a compatible multiple-machine version of PSL, or
attempt retargeting of PSL, a multiple-directory structure such as that at
UTAH should be built. 

The file FULL-LOGICAL-NAMES.CMD, retrieved above should be used as a guide
to building the sub-directories. We currently use 18 sub-directories for the
Common Sources and DEC-20 specific sources, and have at least an extra three
for each new machine. Consult the 20-DIST.LOG file supplied with the PSL
tape as a guide for the amount of space required for each sub-directory.
The current set of directories for DEC-20 PSL, the logical names that we
use, and rough space estimate follows.  Build the sub-directories with a
somewhat larger working space allocation.

Now edit the file PSL:full-logical-names.cmd to reflect the your choice of
<name> along with the create-directories.ctl file.

Also put @@TAKE <name>full-logical-names.cmd in your LOGIN.CMD.

@subsection(Build Sub-Directories)
Then use the system command, BUILD, to build each sub-directory with the name
Pxxx:, as follows. Assistance from the system manager may be required to permit
the creation of sub-directories, and the appropriate choice of sub-directory
parameters:
@begin(ProgramExample)
@@BUILD Pxxx:
@@@@PERM nnnn           ! choose appropriate size
@@@@WORK wwww           ! nnnn+extra
@@@@FILES-ONLY		! Can't login
@@@@GEN 2		! Retain 1 previous version
@@@@PROTECTION 777700   ! Give group access
@@@@DEFAULT    777700   
@@                      ! that are permitted access
@end(ProgramExample)

To make this process easier, we have created a control file:
CREATE-DIRECTORIES.CTL that will build all of the subdirectories with sizes
such that restoration of the files will succeed.  Therefore, after editing
the full-logical-names.cmd file above to reflect the correct logical names,
simply DO the CTL file (some systems use MIC instead of DO, so that may be
substituted in the following examples) : 
@begin(verbatim)
    @@DO CREATE-DIRECTORIES.CTL
@end(verbatim)

This will create all of the necessary directories.

Finally, restore the full system by DOing the full-restore.ctl file:
@begin(verbatim)
   @@DO FULL-RESTORE
   @@DEASSIGN X:          or             @@DISMOUNT  X:
@end(verbatim)

@subsection(Announce the System)
Send out a Message to all those interested in using PSL.
The file BBOARD.MSG is a suggested start. 

Edit as you see fit, but please REMIND people not to re-distribute
the PSL system and sources. 

You may also want to set the directory protection to 775200
and limit access only to those that you feel should have access at
this time.

@subsection(Summary of Restoration Process)
In summary, first retrieve the cmd and ctl files from the first saveset on
the DUMPER tape.  Then choose a single or multiple directory system and
edit the appropriate logical name file to reflect the directory name(s).
If creating a multiple directory system use the create-directories.ctl
control file to build each directory.  Then run the appropriate file
retrieval control file.  Finally, announce the system to any interested users.

@section(REBUILDING LOADABLE MODULES)
Most of the utilities, and many of the more experimental parts of the
system are kept as binary FASL files (with extensions .b) on the PL:
directory.  NMODE is currently the only major sub-system that
has its own set of sub-directories. In some cases (usually large
sub-systems, or sub-systems that share modules) there are a number of .B
files, and a .LAP file that loads each .B file in turn. The PSL LOAD
function will look first for a .B file, then a .LAP file first on the user
directory, then on PL: (both this "search" path and the order of extensions
can be changed).

In order to ease the task of rebuilding and modifying the .B files, we have
a small utility, BUILD.  To use BUILD for a module you call xxxx, prepare a
file called xxxx.BUILD, which has RLISP syntax commands for loading the
appropriate source files.  The file can also have various CompileTime
options, including the loading of various .B files to set up the correct
compilation environment.

Then run PSL:RLISP, LOAD BUILD; and finally enter BUILD 'xxxx; this will
do a FASLOUT to "PL:xxxx", input the xxxx.BUILD file, and finally close the
FASL file. 

The target file "PL:xxxx" is constructed using the variable
"BuildFileFormat!*", initialized in the file PU:Build.Red .

For example, consider the contents of PU:Gsort.Build:

@ProgramExample[
CompileTime load Syslisp;
in "gsort.red"$]

Note that the SYSLISP module is required, since some of the fast sorting
functions in GSORT are written in SYSLISP mode.

GSORT is then rebuilt by the sequence:

@ProgramExample[
PSL:RLISP
LOAD BUILD;
BUILD 'GSORT;
QUIT;]

This is such a common sequence that a MIC file (MIC is a parameterized DO
facility) PU:BUILD.MIC is provided, and is used by passing the
module name to MIC, after connecting to PU:
@ProgramExample[
@@mic BUILD GSORT
]

is all that is required.

@Section(REBUILDING THE INTERPRETER)
A running `rlisp' is required to rebuild the basic interpreter, since the
entire system is written in itself.  The kernel modules, rather than being
compiled to FASL files, are compiled to assembly code (@i(MACRO)) and
linked using the system loader @i(LINK).  The command file
@i{P20C:DEC20-cross.CTL} is executed to produce the cross compiler,
@i{S:DEC20-cross} (S: should be set to an appropriate scratch directory).
The modules in the kernel are represented by the files
@I{P20:*.build}.  There is a program @I{PU:kernel.sl or PL:kernel.b} which
generates command files for building the kernel when parameterized for
Tops-20 by @I{P20:20-kernel-gen.sl}.  The specific modules which are in the
kernel are only listed in this file, in the call to the function
@I{kernel}.  This generates a file @I{xxxx.CTL} for each @I{xxxx.build}.

@subsection(Complete Kernel Rebuild)
A complete rebuild is accomplished by the following steps. At Utah we
use a <scratch> directory for some intermediate files. Define S:
to be this directory or some other appropriate location that can be
deleted when done. Below we use @@SUBMIT xxxx.CTL to run batch jobs;
on some systems, @@DO xxxx.CTL can be used instead, or on others, @@MIC
xxxx.CTL may be used.

Begin by defining S: as  <scratch> or other scratch directory:

@verbatim[	@@DEFINE S: <scratch>]

Now connect to <psl.20-comp> and rebuild DEC20-CROSS.EXE:

@verbatim[	@@CONN P20C:]
@verbatim[	@@SUBMIT DEC20-CROSS.CTL]

Copy the <psl.comp>BARE-PSL.SYM to 20.SYM, and regenerate the
appropriate .CTL files. This saves the old 20.SYM as 
PREVIOUS-20.SYM:

@verbatim[	@@CONN P20:]
@verbatim[	@@SUBMIT P20:FRESH-KERNEL.CTL]

Rebuild each module (xxxx) in turn, using its xxxx.CTL. This creates xxxx.MAC
and Dxxxx.MAC files, and assembles each to make xxxx.REL and Dxxxx.REL.
The entire set is submitted with the file ALL-KERNEL.CTL, which submits
each file in turn.  (Note that these must be done sequentially, not
simultaneously.  If you have more than one batch stream, make sure that
these are run one at a time):

@verbatim[       @@SUBMIT ALL-KERNEL.CTL]

Build the main module, which converts the accumulated 
20.SYM into heap and symbol-table initialization:

@verbatim[	@@SUBMIT P20:MAIN.CTL]

Finally LINK the xxxx.REL and Dxxxx.REL files to produce S:BARE-PSL.EXE:

@verbatim[	@@SUBMIT P20:PSL-LINK.CTL]

Execute and save as PSL.EXE, reading appropriate xxxx.INIT files (note,
each site usually customizes the PSL environment to suit their needs,
therefore we recommend that you create your own version of Make-psl.ctl to
perform this task).
	
@verbatim[	@@SUBMIT PDIST:MAKE-PSL.CTL]

Finally, run MAKE-RLISP.CTL as needed:

@verbatim[	@@SUBMIT PDIST:MAKE-RLISP.CTL]

Rlisp.exe and Psl.exe will be saved on the <PSL> directory.
You now may want to delete any xxx.log files that where created.

You may also remake, RLISPCOMP, PSLCOMP and NMODE, in a similar manner.

@Verbatim[
	@@DEL P20:*.LOG
	@@DEL P20C:*.LOG]


@subsection(Partial or Incremental Kernel Rebuild)
Often, only a single kernel file needs to be changed, and a complete
rebuild is not needed. The PSL kernel building process permits a
(semi-)independent rebuilding of modules, by maintaining the 20.SYM file to
record Identifier Numbers, etc.  The 20.SYM file from the recent
full-rebuild, and xxxx.INIT files are required, as are the "xxxx.REL" and
"Dxxxx.REL". The partial rebuild will replace the "mmmm.REL", "Dmmmm.REL"
and "mmmm.INIT" files, modify "20.SYM", and then rebuild the MAIN module.
Assuming that a recent full rebuild has been done, a partial rebuild of
module "mmmm", is accomplished by the following steps.

As above, S: is required for "Scratch" space.

Define S: as  <scratch> or other scratch directory:

@verbatim[	@@DEFINE S: <scratch> ]

Rebuild DEC20-CROSS.EXE, if needed:

@verbatim[	@@SUBMIT P20C:DEC20-CROSS.CTL]

Rebuild the module (mmmm), using its mmmm.CTL. This creates mmmm.MAC
and Dmmmm.MAC files, and assembled each to make mmmm.REL and Dmmmm.REL.
See the file ALL-KERNEL.CTL for current modules.

@verbatim[	@@SUBMIT P20:mmmm.CTL
        Other modules can be done after this]

Rebuild the main module, which converts the accumulated 
20.SYM into heap and symbol-table initialization: (This step can be omitted
if 20.SYM has not been changed by the incremental recompilation.)

@verbatim[	@@SUBMIT P20:MAIN.CTL]

Finally LINK the xxxx.REL and Dxxxx.REL files to produce S:BARE-PSL.EXE:

@verbatim[	@@SUBMIT P20:PSL-LINK.CTL]

Execute and save as PSL.EXE, reading appropriate xxxx.INIT files:
	
@verbatim[	@@SUBMIT PDIST:MAKE-PSL.CTL]

Finally, run MAKE-RLISP as needed:

@verbatim[	@@SUBMIT PDIST:MAKE-RLISP.CTL]

You may also remake, RLISPCOMP, PSLCOMP and NMODE, in a similar manner.

Note that 20.SYM may be changed slightly to reflect any new symbols
encountered, and certain generated symbols. Occasionally, repeated building
of certain modules can cause 20.SYM to grow, and then a full rebuild may be
required.

@subsection(Rebuilding RLISP.EXE from PSL.EXE)
The PSL executable file, PSL.EXE, is a fairly bare system, and is usually
extended by loading appropriate utilities, and then saving this as a new
executable. We have provided RLISP.EXE, which includes the compiler, and
the RLISP parser. RLISP.EXE is built from PSL.EXE by the following
commands:
@begin(verbatim)
   @@TAKE PSL:minimal-logical-names.cmd
   @@PSL:PSL.EXE
   (LOAD COMPILER RLISP INIT-FILE)
	    % Also LOAD any other modules that
	    % should be in your "standard" system
   (SAVESYSTEM "PSL 3.1 Rlisp" "PSL:rlisp.exe" '((Read-init-file
       "rlisp")))
            % The string is the Welcome Message, the save file
	    % name and the startup expression to read rlisp.init.
   (QUIT)
@end(verbatim)

We have provided a command file, PDIST:MAKE-RLISP.CTL for this purpose.
Edit it to reflect any modules that local usage desires in the
basic system (PRLISP, USEFUL, etc. are common choices).

In a similar fashion, a customized PSL.EXE could be maintained instead of
the "bare" version we provide. In order to avoid destroying PSL entirely,
we suggest that you maintain a copy of the supplied PSL.EXE as
BARE-PSL.EXE, and customize your PSL.EXE from it.

@section(RELATIONSHIP TO PSL 3.0)
Even though this is the first version of PSL for the DecSystem-20 that
utilizes extended addressing, it is identical to the PSL V3.1 for the
non-extended 20.  As a new PSL version 3.1, it is a complete release, and
totally replaces the previous PSL 3.0 that underwent limited distribution.
The files @i(pd:bug-fix.log) and @i(pd:bugs.txt) record many of the changes
and bug fixes that occurred since version 3.0.

@section(FUTURE UPDATES)
It is currently envisioned that future updates will still be complete
releases.  It is therefore suggested that you

@begin(enumerate)
Retain this distribution tape in case you may have to compare files.

Do not make any changes on these distributed directories. If you must make
your own bug fixes, it is suggested that you put the changed files on some
other directories, such as @i(pnew:).  They can then be compared with any
new files sent out in subsequent releases.
@end
