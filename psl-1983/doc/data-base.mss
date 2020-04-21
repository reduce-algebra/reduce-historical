25-Nov-82 06:12:44-PST,5564;000000000001
Date: 25 Nov 1982 0557-MST
From: Martin Griss <Griss@Utah-20>
Subject: Database
To: griss.hplabs at UDel-Relay, hplabs!griss.UTAH-CS at Utah-20
cc: griss at Utah-20
Via:  Utah-20; 25 Nov 82 8:07-EST
Via:  Udel-Relay; 25 Nov 82 5:24-PDT
Via:  UDel; 25 Nov 82 6:11-PDT

@pageheading[left "Database Project Proposal",  right "@value[page]"]

@begin[center]
Project Proposal for CS638, Databases
William F. Galway
@value[date]
@end[center]

This paper proposes the development of tools for the maintenance of the PSL
programming environment.  Although PSL is the specific target of the tools,
many of the concepts (and perhaps some of the code) could be applied to
other programming environments.  These tools are similar to the Source Code
Control System (SCCS) of Programmer's Workbench (under Unix), and the
MasterScope utility of INTERLISP.

These tools are meant to solve the following problems:
@begin[enumerate]
Keeping a history of PSL development.

Maintaining consistency of the system across multiple sites.

Maintaining consistency between a function, functions which call it, and
documentation which refers to it.

Locating the source code and documentation for functions.
@end[enumerate]

To implement these tools, I intend to provide an interface to utilities
already present on our Vax-unix operating systems, and to extend some
utilities currently present in PSL.

@Comment[Interface to RCS.]
@Comment{files vs functions?}
@heading[Keeping a history of development]
The @i[Revision Control System] (RCS, similar to SCCS) allows the user to
keep multiple versions of text files.  It does this "efficiently" by only
storing differences between files, while sharing their common parts.  It
also stores information about authorship of files and the reasons for
changes to them.  This information will be used by other tools in the
proposed project.

@begin[Comment]
Maintenance on different machines.
Need a "database" indicating our idea of foreign site's state.
Periodically we mail changes, in the form of (last-mailed-version,
current-version).  last-mailed-version corresponds to "root" for "join"
operation of RCS.  Can easily check for any possible problems caused by
foreign site, even if they don't maintain their own tree.  (If they do, we
could avoid mailing the last-mailed-version, but send a pointer to the
last-mailed-version instead.)  (Note that sites sending changes out must
work harder than recieving sites?)
@end[Comment]
@heading[Maintaining consistency between sites]
PSL is under devlopment at two sites, the University of Utah and Hewlett
Packard Research labs in Palo Alto.  Obviously, problems occur when changes
are made to corresponding files at both sites.

To deal with this problem, each site needs to "mail" changes to the other
site(s).  I assume that each such mailing re-establishes consistency
between those two sites.  I propose that each "devlopment" site keep a
record of when mailings were sent.  Each new mailing will involve the
following:
@begin[itemize]
Finding all files which have changed since the last mailing.  (This
information can be retrieved from RCS.)

The transmission (via network or mag-tape, say) of the new files.  (Or
of their incremental changes from the previously mailed files.)

At the recieving site the recieved files (or their "last modified dates")
must be compared with the most recent local version.  Any local versions
which have not been changed since the last receipt of mail can be
superseded.  Any files which have been changed locally must be "merged"
with the received file.  (RCS provides tools for automating this job, to
some degree.)
@end[itemize]
(Unfortunately, this doesn't deal with the renaming of files--an area for
more research!)

@begin[Comment]
Cross reference (tracing effects of changes).  Must include .MSS support.
Might implement .MSS by just giving a new reader, like READ vs XREAD
(roughly speaking).  Whenever it hits a function documentation line it just
build a dummy function definition, which is manipulated by standard tools
after that?  (Might fit in well with comments as first class citizens, both
the MSS reader and the other readers would return documentary commentary.)
@end[Comment]

@heading[Consistency between interrelated parts]
PSL currently provides a cross-reference utility to find interrelationships
between functions.  Also, the ".MSS" sources for the PSL manual clearly mark
definitions of and references to functions.  I propose to use this
information in the following ways:
@begin[itemize]
Given a list of files changed since a given date, to locate other files
referring to them.  (Or, perhaps it will be possible to work in units of
functions rather than files.)

Given a list of functions, to check that other functions and documentation
referring to them agree on number of arguments, "type" of function (e.g.
"macro" or "expr"), and any other information which can be easily extracted
and compared.
@end[itemize]

@Heading[Locating things]
PSL's cross-reference utility (or the EMACS tags utility, or PSL's
"Inspect" utility) finds the location of function definitions (at least to
the file level).  A similar utility needs to be provided for ".MSS" files
(also to be used for the consistency checking described above).  I propose
to write tools that will use this information to look up and print (or
read into a screen editor running under PSL) source code and documentation
for functions.
-------

