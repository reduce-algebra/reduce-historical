@make(Article)
@comment[
 9-Dec-82 20:46:50-MST,16664;000000000001
Mail-from: ARPANET site RAND-RELAY rcvd at 9-Dec-82 2044-MST
Date:  9 Dec 1982 0544-PST
From: GRISS.HP-HULK at Rand-Relay
Subject: Draft of more BUILD
To: jw-peterson at Utah-20
Via:  HP-Labs; 9 Dec 82 19:36-PDT

Here is a portion of manual about the next steps. Not complete,
incorp@orates some of what youve seen:
]

[For the moment, this note will use 68000 building as example, using
DEC-20 as HOST]

@section(Building the Cross Compiler)
Connect to P68c: .

 Make sure that you have the following .b files on p68c:, or rebuild
as below:

   a. m68k-cmac.b
   b. m68k-comp.b
   c. m68k-asm.b

@subsection(How to make the .B files using the  .MIC files)
To rebuild a missing .B file, run the SYSBUILD .MIC file
on the appropriate module:
 
  @@MIC PU:sysbuild M68k-xxx

where "xxx" represents CMAC, COMP or ASM, as appropriate.

@subsection(How to make the .EXE file)
Now build the cross compiler onto the scratch directory, S: by
running the .CTL file (using DO or SUBMIT):
   
@@do p68c:new-m68k-cross.ctl

[In the future, this should actually be changed to "do
new-apollo-cross" to avoid confusion between the various 68000 based
machines].


@section(Running the Cross Compiler)
Now connect to p68:
@subsection(Independent Compilation and the .SYM file)
To build a fresh BARE-PSL or a fresh FULL-PSL you will need a fresh
symbol file. The current symbol file has the name of "m68k.sym" 
[which  should be changed to "apollo.sym" or something related in the future]

First generate a fresh m68k.sym file:

@@MIC fresh-kernel.ctl

This will keep your last m68k.sym file as p68:previous-m68k.sym.  The
fresh m68k.sym file will be on S:. Make sure it is there.

@subsection(Generating the Module .CTL Files)
Currently fourteen modules are required to build the first phase of
either the
BARE-PSL build or the FULL-PSL.  You will need xxx.CTL files on kapollo:
for each of these. The kernel module names (xxx) are currently:

<<< how's about compiler modules?  have same faclity make .ctl files for
    those?? >>>

TYPES RANDM ALLOC ARITH DEBG ERROR EVAL EXTRA FASL
IO MACRO PROP
SYMBL SYSIO TLOOP HEAP
and
MAIN 

[Note, order is different from older P68: version <<<how??>>>]

[Note, change to generate also for BIG-KERNEL?] 

<<< there needs to be some clear consensous on the terminolgy, i.e., what's the
differances between big/bare/full-comp/kernel/psl??? >>>

Take a look to see if they are there, if they are not
you will have to re-generate them. The easiest way will be to do this
is via the "kernel-gen" program:

@begin(verbatim)
@@PSL:PSL
*(dskin "apollo-kernel-gen.sl")
*(quit)
@end(verbatim)

  This will create the xxx.CTL files you need on kapollo:.
 

@subsection(Building the Modules)
<<<again, terminology.  need some clear definitions as to what all
encompases (in terms of functionality, not "contains xx, yy & zz") >>>
Now connect to kapollo:

Now you must execute all the CTL files for the first 14 modules. Do this with
the following command:

@@MIC kapollo:All-kernel.ctl



This command will SUBMIT all these CTL files to batch. 

[Alternatively, single modules my be run by submitting

@@SUBMIT xxxx.CTL

for module xxx]  <<< any order or presatance to be followed?  hows about .sym
                     file??>>>

Each batch job processed will create an xxx.log file on kapollo: which
you can look at to evaluate errors.  Initially before running a fresh
build you might want to delete all these log files just for the sake
of space.

@subsection(Processing the MAIN file)
<<<re: "is built last"  where (timewise) does the compiler fit in?>>>

Note that the MAIN module is built last, and that it takes the
contents of the .SYM file and builds the run-time symbol table
initialization.

@@submit MAIN.CTL

[Why is this not in ALL-KERNEL.CTL?]<<<because all-kernel refers to building
the Individual pieces.  Main crunches specificly on the main-start file and
builds a resulting dmain.  it is separate from all-kernel (specifcly, if i
remember) simply so it Can be run last>>>.

@subsection(Linking the files and executing)
<<<huh?>>>
@section(Details on the Test series)


[Absorb details from TEST GUIDE here] 

<<< NO!  we're talking about building *re*building sources that are assumed to
be complete (i.e., a new version), not developing a port to a new machine.
the port process, including the use of the small tests, deserves to be in
a separate document; as it works quite differntly from building the whole
thing. >>>

@subsection(Command Files, and Kernel Generator?)
[Describe kernel generator earlier?] <<< yes, please.  and while you're at
it, a functional description of "a kernel", and what it must contain, would
help.>>>

@subsection(Basic Test Strategy)
Each test will use some modules tested in previous test,
and add others, mostly extracted from the full PSL sources.
Occassionally some stub-files have to be added, to be replaced
by more complete sets extracetd frm sources later. Early tests
simply try to print informative messages about what is happening,
and whether each test succeeded or not. As more of a complete
LISP is built up, the tests will require a variety of manual
inputs. Finally a complete MINI-PSL will result.

<<< again, i'd like to see the porting manual separate from the system-rebuild
description; not doing so risks confusion, and perhaps a 'missing piece 
syndrome'.  the idea is pick up vol. one "how to design and test psl cmacros"
once you think your cmacros work, you pick up vol. two "how to build a
complete running psl."  theoreticly, the only thing in common between the
two should be one(?) i/o module and the key compiler files xxx-cmac, xxx-asm,
etc. (they guys who live in .../comp) >>>

@subsection(Test1)


@subsection(Testn)


@subsection(Testing Mini-EVAL)


@subsection(Testing Character and File I/O)


@subsection(Switch Over to INIT files)
<<< what switch?  where?  magic? >>>
At this point, can flip a switch in the build process, and
have INITCODE be smaller, and instead have .INIT files produced,
which will be read in by LAPIN or DSKIN.

@subsection(Testing Binary I/O)
[Write a small BINDUMP routine]  

<<<again, the vol.1 "how to test"/vol 2."how to build" concept.  perhaps
set up a testn+1  to test bin i/o?? >>>

@section(Building the BARE-PSL kernel)

At this point, enough basic tests have been done, and now the standard
BARE-PSL should be built. This requires a few more files, 
<<<this is where things can get murky between "test phase" and "build phase".>>
and a more
stable BUILD sequence. This will result in a complete 3.1 version of
BARE-PSL.

<<<what about comp/faslout?  build it on the resident bare-psl via the
interpreter?  maybe go whole-hog first time?  we thought we could get away
bare-psl on the apollo mainly because we thought the 3.0 could handle
generating new binaries.  it couldn't, so we had start from square 0.  and
you can't (at least if i interpeted all of chip & steve's swearing & cursing
right) build the comp stuff interpetivly because you start tripping
over syslisp.  is that now fixed?  if so, how? needs 

the concept as presented here needs details, and looks like it may not
be fully correct...take a hard look.>>>

@subsection(Use and Customization of Kernel Generator)

[Should kernel-gen be used with test series?]  

<<<no, see above dissertation on vol1/vol2>>>


@subsection(Common Files, Machine Specific Files and Dummy Files)

@subsection(Init Files)
<<<short section.  I could use the info, what -are- they used for?
when do you need to replace them?>>>

@subsection(Testing BARE-PSL)

@section(Bootstrapping the LAP, FASL and COMPILER)
Currently, we bootstrap complete system by adding additional modules
to BARE-PSL to make BIG-PSL.<<<terminology again>>> 
These are LAP, FASLOUT and COMPILER
modules, and also RLISP parser. BIG-PSL <<<don't you mean bare?>>>
is used as a bootstrap step to
the production of COMPILER.B, FASLOUT.B, LAP.B etc., since once these
are built, they can be loaded into the BARE-PSL when needed.
Having core-save working by this time is important, since
the kernel is quite large, and loading RLISP and COMPILER and INIT
files takes quite a while.  <<<though somewhat of a moot point on the apollo,
since copying the entire image also takes plenty of time>>>.

[In future, should convert critical files to .SL, avoid RLISP in
kernel at ALL] <<<or how's about the host generating a .sl rlisp 
automaticlly?  I would Much rather read .red then .sl >>>

[In future, will do alternative model, with just LAP to start, test
with LAP files from cross-compiled files.  Then test FASLOUT and
FASLIN.  Should be able to load many things as .LAP files.  Then
finally load compiler. It should work without much problem since its
essentially all common code, and mostly tested even for this target in
CROSS mode.]  <<<yeah.  reminds me, this doc doesn't say much about lap.
generation of the lap system is quite arcane, no?>>>

@subsection(Building the FULL-PSL)
Essentially same procedure as BARE-PSL, just have 2 more modules,
RLISP and COMP, and rebuild MAIN.  <<<but if you're going the cross compile
route, watch out for booby traps (i.e., fasl in bare-psl stepping on fasl
in comp>>>

@subsection(Extra Files)
For the RLISP module, need PU:RLISP.BUILD which accesses
PU:RLISP-PARSER.RED and PU:RLISP-SUPPORT.RED.

[We should change sources so that dont need RLISP for 
for BIG-BUILD].

For the COMP module, we need to access a large number of
files right now:  <<<huh?  this is mislocated>>>

@subsection(Building both BARE-PSL and FULL-PSL)
Its worth building both BARE-PSL and FULL-PSL at the same time during this
phase. Build up to the MAIN module of BARE-PSL. Then copy the .SYM
file for use in incremental rebuilding of BARE-PSL modules and
BARE-MAIN. Then continue to build the RLISP, COMP and FULL-MAIN
modules. These 2 different .SYM files are then used for rebuilding
modules in the BARE-PSL series or the FULL-PSL series, as appropriate.
Most of the time, errors will be only in the COMP module, but occasionally
errors will be found that require a full build of the BARE-PSL and FULL-PSL,
or incremental rebuild of some earlier modules.  <<<hmmm, what about .sym
file?  and cleaning it out and restoring it?  and how do the .init files
fit into this process.  i don't like the idea of several lisps lying around
(e.g., bare, big, full, etc).   

would be MUCH simpler just to deal with one resulting system, rather than try
and keep track of several.  particularly if they start getting into fights
and stepping on each other.  cost in dealing with one larger system may be made
up in avoiding screwups caused by multpile ones.  think about this!>>>

To build a FULL-PSL you must submit two additional .CTL files to be
cross compiled, they are COMP.CTL and MAIN.CTL. To build just BARE-PSL
you submit only MAIN.CTL.  Both of these CTL files should be on
kapollo:, if not you will have to create them by hand.

Here is COMP.CTL:

@begin(verbatim)
@@define DSK: DSK:, kapollo:, PI:  <<<search lists are too much a form of
					magic.  would prefer that it be
					dictated as to which dir the .ctl is
					run from, and logicals (or on unix,
					relative paths) be used to specify
					where things belong.  besides, they
					Only work this way on the 20.>>>
@@S:m68k-CROSS.EXE
*ASMOut "comp";
*in "comp.build";
*ASMEnd;
*quit;

The COMP.BUILD file should look like this:

macro procedure !* u;nil;
on eolinstringok;
put('bitsperword,'wconst,32);
compiletime flag('(taggedlabel inump !*jumpeq !*jumpnoteq
		   !*jumpwgreaterp !*jumpwlessp !*jumpwgeq
		   !*link !*linke
		   onep
		   !*jumpwleq), 'lose);
in "pc:anyreg-cmacro.sl"$
in "pc:common-cmacros.sl"$
in "pc:common-predicates.sl"$
in "pc:pass-1-lap.sl"$
in "pc:compiler.red"$
in "pc:comp-decls.red"$
in "pc:tags.red"$
compiletime remflag('(taggedlabel inump !*jumpeq !*jumpnoteq
		   !*jumpwgreaterp !*jumpwlessp !*jumpwgeq
		   !*link !*linke
		   !*jumpwleq), 'lose);
compiletime flag('(tagnumber), 'lose);
in "kapollo:m68k-cmac.sl"$
in "kapollo:m68k-comp.red"$
in "kapollo:m68k-lap.red"$
in "p68:nsystem-faslout.red"$ <<<are these duplicated in the bare-kernel?>>>
in "pc:faslout.red"$  <<<again, problems with multilpe version, maybe not
			a good idea>>>

The MAIN.CTL file will look like this:

define DSK: DSK:, PHP:, PI:
S:HP-CROSS.EXE
ASMOut "main";
in "main.build";
ASMEnd;
quit;
@end(verbatim)

So send one or both of these files to batch like this

"submit comp.ctl"
"submit main.ctl"

   Each ctl file sent to batch will produce three files on the scratch
   directory, an xxx.ASM, an Dxxx.ASM, and a xxx.INIT file.
   Some of the init files are of length zero, this is ok.

@subsection(Append INIT files)
Connect to the scratch directory, S:.

The init files can all be appended together to cut down shipping and the
time it takes to startup the APOLLO PSL.
Append all the init files together to create an all.init. 

 If you also are building the BIG-PSL then you will have to append
COMP.INIT to all.init by hand or ship it to the apollo seperately and
edit the file on the Apollo to include the comp.init.

@@DO P68:all-init.ctl

@subsection(Removing Tabs)
[I believe 3.1 CROSS compiler fixed to only put in 1 space (or 2 for CRAY),
so tabs dont need to be stripped. EXPAND is unsafe program]

The Apollo Assembler does not like tabs so the .ASM files will need to
have the tabs expanded into spaces. One way to do this is to do the
following.
    
@@DO p68:allexpand.ctl  <<<unix has much better facilities for doing this>>>

If you are building a BIG-PSL then you will have to expand the two comp 
by hand by doing:

@@unix:expand <comp.asm >comp.asm
@@unix:expand <dcomp.asm >dcomp.asm

I suggest you copy everything to rs: to keep it  around. Thats all
the .asm's, the .inits, and the m68k.sym. 

[Why not change the .CTL files to insert RS: instead of S:]
<<<perhaps because disk space is guarenteed on scratch, i.e., an extra
set of versions won't kill you.  would be nice tohave them back the next
day though....>>>

@subsection(Ship via the VAX)
You are now ready to ship the code to the Apollo.  Login on the VAX
and run

regexp.csh, 

a copy is on lowder's directory.  This will move all the files off
scratch except for the two comp files. So do:

[Add BIGregexp.csh]  <<<what on earth does regexp stand for?>>>

<<<important:  you should also give the following vax commands to avoid
getting screwed over by mail, system, and autologout msgs:

biff n	#shut off mail notifyier
mesg n  # sys msgs
set autlogout=2000  #so it won't die while waiting for asm
>>>

get20 scratch comp.asm dcomp.asm

@subsection(Fetch from Apollo)

Get logged in on the Apollo and conect to the VAX by running ST.
>From the Apollo shell type:

   "apollo.csh"

This will ship and assemble everything from the VAX except files related
to comp. If you are using them you will have to type this to the apollo:
 
[Add BIGAPOLLO.csh]

   "vfv1 comp.asm
    asmnl comp
    vfv1 dcomp.asm
    asmnl dcomp"

@subsection(Bind the Modules)
Now link with shell script:

PSLBIND.SH PSL

[Here again you CURRENTLY have to edit pslbind.sh to add the names of
COMP.BIN and DCOMP.BIN if you are going to build a BIG-PSL.  Suggest
doing this once, create a BIGBIND.SH]  <<<again, look at the special
casy-ness of having big vs. bare [vs. full], etc.  worth avoiding?
time savings in the long run?>>>

@subsection(Notes)
There are a number of ways to vary this entire prcocess to customize
it to your needs. If you started by building a BARE-PSL you can go
back and build just the comp module by copying the m68k.sym from rs:
onto the scratch directory and submitting the comp.ctl and the the
main.ctl as previously described. Also you can choose to link or not
the comp module in the apollo.
<<<important:  you need to spell out booby traps you can run into while
doing this>>>

@subsection(Testing LAP)
Once most of LAP has been run on the host machine (interpretively or
compiled), the next step is to run it as a "resident" PSL assembler on
the target machine to ensure that it correctly assembles small
procedures written in TLM ("target" LAP) form. Then procedures are
input in ALM (cmacro form). Usually this next step will work quite
well, since the CMACRO's will have been well tested while building the
TEST-SERIES and BARE-PSL.

Note that until RESIDENT mode of assembly seems stable (basically
checking assembler and cmacro tables), there is no point in trying
to do much with faslout. 

Here are some simple procedures to try; others can be generated
by looking at the output of the cross-compiler:

<<<comments!  what are these guys trying to do?  what should i look for
to see that they work right?  >>>

@begin(verbatim)
(LAP '((!*ENTRY FOO EXPR 1) % can we define ANY procedure
       (!*ALLOC 0)	
       (!*EXIT 0)))         % or (RTS) on 68000
	                    % when called, should return argument

(LAP '((!*ENTRY FOO EXPR 0)
       (!*ALLOC 0)	
       (!*MOVE (QUOTE 1) (REG 1))
       (!*EXIT 0)))

(LAP '((!*ENTRY FOO EXPR 1) % adds 2 to argument, prints and returns
       (!*ALLOC 0)	
       (!*MOVE (QUOTE 2) (REG 2))
       (!*LINK PLUS2 EXPR 2)
       (!*LINK PRINT EXPR 1)
       (!*EXIT 0)))
@end(verbatim)

Common problems encountered at this phase are:
@begin(description)
LAP Table Errors@\Most implementations of lap have procedures
for common formats, and tables of numbers for the opcodes.
Often the numbers are mistyped, or the instructions misclassified
or missing.

Trace@\If it blows up with illegal addressing, try tracing certain passes
to see which is at fault; then as a quick patch, redefine these
passes to be NO-OPS:
@begin(verbatim)
(de OptimizeBranches (U) U)

or

(de PASS1LAP (u) U)

etc.
@end(verbatim)

@end(description)
<<<what does alm mean?>>>
[Prepare file of sample procedures, and corresponding ALM form
to test important things. E.g., HALFWORD tables for LAMBIND, etc.]

<<< why did chip & steve use interpretiv put/gethalfword functions?
tricks worth knowing about???>>>

[In future, hope to be able to run LAP interpretively on BARE-PSL,
rather than having to build into kernel.]

@subsection(Testing FASLOUT and FASLIN)
Now that resident LAP seems to work, try some simple FASLOUT and
FASLIN. Binary I/O should have been tested, so main thing is checking
that RELOC stuff works, and that bytes and words are correctly
assembled into the incore array for FASL, passed out to the file
and correctly re-written.   <<<examples of what this looks like?>>>

FASLOUT and the FASLIN a few small files 
<<<how's about some pre-built tests?>>> to check accuracy. These
files should be self-contained, and not intially contain
SYSLISP code, since the SYSLISP.B module has not been built.
<<< easier said than done- syslisp has had a tendenacy to creep into
nearly everything for "effeciency" sake...>>>

For example, try the PU:POLY.RED. An important one
is PU:RLISP-PARSER.RED and PU:RLISP-SUPPORT.RED.

[It is worth while to use a small BINARY-DUMP
routine that reads a binary file and prints it as OCTAL or HEX numbers.
This can be compared with the known FASL format<<<which is ____>>>,
for a test file that
has been fasled on a similar machine].

Common problems encountered at this phase are:
@begin(description)
 ???? <<<amen>>>


@end(description)

@subsection(FASLOUT the critical files)
In order to build most of the .B files that are needed, one needs to
create the IF-SYSTEM, BUILD, RLISP, COMPILER, FASLOUT and LAP modules.
First "hand-build" the IF-SYSTEM and SYSLISP and BUILD modules:

@begin(verbatim)
FASLOUT "IF-SYSTEM";
IN "IF-SYSTEM.RED"$
FASLEND;
@End(verbatim)

Building SYSLISP is tricker since it needs
a version of SYSLISP to build from. First edit the PC:SYSLISP.BUILD file,
to make sure that the IF_SYSTEM clauses mention your machine
(as set up in the SYSTEM_LIST!* list before). Then  read in the
SYSLISP support interpretively, and then FASLOUT :
@begin(verbatim)
<<<where are we?  is this with the cross compiler?>>>
LOAD IF!-SYSTEM;       % Needs IF-SYSTEM
IN "SYSLISP.BUILD";    % To get interpreted SYSLISP in
		       % since it needs SYSLISP to build
OPTIONS!* := 'SYSLISP . OPTIONS!*;
			% To prevent PSL from attempting to load Syslisp;
FASLOUT "SYSLISP";     
IN "SYSLISP.BUILD"$    % may have to use PATHIN off PC:
FASLEND;
@end(verbatim)

Finally, faslout the BUILD.B module, for future module building:
@begin(verbatim)
FASLOUT "BUILD";
IN "BUILD.BUILD"$
@end(verbatim)

Now use BUILD on the other modules that are needed to produce
the base system:

@BEGIN(verbatim)
BUILD 'RLISP;
BUILD 'COMP!-DECLS;
BUILD 'PASS!-1!-LAP;
BUILD 'xxx!-LAP;
BUILD 'xxx!-CMAC;
BUILD 'xxx!-COMP;
BUILD 'FASLOUT;
BUILD 'COMPILER;
@end(verbatim)

@subsection(Test FASL'd RLISP and COMPILER)
LOAD the RLISP  modules into the BARE-PSL
system, check that RLISP works on a number of files.

Now LOAD the COMPILER, try some in-core compilation of simple
procedures (ON COMP).

Finally use this system to FASLOUT or BUILD a variety of modules.
Ultimately try rebuilding RLISP and COMPILER and SYSLISP.
<<<what are problems here?  what's the roles of the resident system and the
cross compiler at this point?>>>

@subsection(BUILD rest of library)

Now go through the PU: directory, running BUILD on each of the BUILD
files. Check each build-file to see which additional modules are needed.
Important shared modules are:
@begin(verbatim)
<<<gee, if you squint this looks like a unix makefile...>>>

INUM		Needs SYSLISP
FAST-VECTOR     Needs SYSLISP, IF-SYSTEM
VECTOR-FIX      Needs SYSLISP
GSORT           Needs SYSLISP
BIGBIG          Needs SYSLISP, FAST-VECTOR,VECTOR-FIX,ARITH 
BIGFACE         Needs SYSLISP, FAST-VECTOR,VECTOR-FIX,ARITH 
		      INUM, BIGBIG,IF-SYSTEM
@end(verbatim)
-------


