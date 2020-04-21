@make(article)
@Case(Draft, 1 <@device(Omnitech)>,
             else <@device(LPT)>
      )
@style(Spacing 1,spread 0)
@modify(description, spread 0, 
     above 0, below 0, indent -2 inches, leftmargin +2.5inches)
@case(Device, LPT <@modify(HDX,below 0, above 0)
                   @modify(HD2,below 0, Above 1, Use B)
                   @modify(HD3,above 0, below 0,indent 3 char)
                  >
     )
@MajorHeading(PSL projects for SUMMER 1982)
@Heading(M. L. Griss)
@begin(center)
Last Update: @value(Date)
@end(center)

        This document gives a list of the projects to be done regarding PSL
during this summer.  Those individual associated with each aspect of the
project are listed with their activities.  Missing are a list of priorities
associated with each of these project, or in some cases a PERT (or
whatever) chart would be appropriate as there is some precidence ordering.
As the Package system probably should have a high priority than the
BIGFLOAT stuff (as we will soon have major problems with names due to users
wanting to add their own packages of routines and compatability packages
etc. which will cause many name conflicts).

        The section at the end of the document is to be used to keep track
of who knows what is going on about a given topic and who is working on it.
There us a section for each of the people connected with PSL and what they
are @dq[going to be doing]!


@Section[DEC-20 and VAX]
@begin(description)
Polish BIGNUM@\

Implement BIGFLOAT@\

Packages and FASL@\Benson

Resurrect ALTBIND@\

Polish REDUCE@\Griss, Hearn

Franz-LISP and MACLISP Compatibility@\@Comment{Lanam (sp) at HP ??? for Franz}

Extended-DEC-20@\Benson
@end(description)

@section[APOLLO]
@begin(description)
I/O, Floats, 32 bits@\Lowder

LAP and FASL@\Maguire, Lowder

Core Save/Restore@\Peterson->Lowder and Maguire

SYSCALL@\Maguire, Lowder
@end(description)

@section[Other 68000s]
@subsection[WICAT]
@begin(description)
Transfer PSL@\Lowder, Snelgrove
@end(description)

@subsection[HP9836]
@begin(description)
Test I/O, and build@\ ??
@end(description)

@section[CRAY]
@begin(description)
LAP-to-ASM@\Griss, Kessler

CMACROs@\

I/O and other LAP@\

Basic testing Model@\
@end(description)

@section[Documentation]
@subsection[MANUAL and HELP]
@begin(description)
Update Manual@\

New Help Files@\

Automate HELP files, Dirs@\

Add DESCRIBE@\
@end(description)

@subsection[SYSTEM Documentation]
@begin(description)
Implementation@\

BUILD Guide@\

CMACRO Guide@\

LAP Guide@\

Testing Model@\
@end(description)

@section[EMODE]
@begin(Description)
DOCUMENT@\Galway

Optimize@\

POP-UP windows and Menus@\

Augment with Structure@\

EMODE and Graphics@\Stay, Fish

EMODE and Apollo@\Move to Apollo PSL, see if Aegis window handler can be
used at all, or if have to"borrow" display and do one-self (based on ST
like emulator).  

EMODE and Algebra@\Need special structure editor, "boxes", etc. Get stuff from
Don.
@end(description)

@section[Miscellaneous Modules]
@begin(description)
File Package/MasterScope@\

Improve  or Replace RCREF@\

Improve PictureRLISP@\

Improve MINI, add error handler@\

Continue BETTY mode system@\

@end(description)

@section[Applications]
@begin(description)
Algebra, Graphics and CAGD@\Griss, Knapp, Stay

GPL@\Maguire, Robinson [, Lowder, Kessler]. Conversion of LISP 1.6 "engine" to
PSL.

CAI@\
@end(description)


@Section(Activities by Individual)
@Subsection(Benson)
@Begin(Format)
Packages and FASL
Extended-DEC-20
@End(Format)

@SubSection(Galway)
@Begin(Format)
EMODE DOCUMENT
@End(Format)

@Subsection(Griss)
@Begin(Format)
Polish REDUCE
LAP-to-ASM
Algebra, Graphics and CAGD
@End(Format)

@Subsection(Hearn)
@Begin(Format)
Polish REDUCE
@End(Format)

@SubSection(Kessler)
@Begin(Format)
LAP-to-ASM
GPL
@End(Format)

@Subsection(Knapp)
@Begin(Format)
Algebra, Graphics and CAGD
@End(Format)

@SubSection(Lowder)
@Begin(Format)
I/O, Floats, 32 bits
LAP and FASL
Core Save/Restore
SYSCALL
WICAT Transfer PSL (With Snelgrove of WICAT)
GPL
@End(Format)

@SubSection(Maguire)
@Begin(Format)
GPL (with Robison [, Kessler, Lowder])
LAP and FASL
Core Save/Restore
SYSCALL
@End(Format)

@Subsection(Stay)
@Begin(Format)
Algebra, Graphics and CAGD
EMODE and Graphics (With Fish)
@End(Format)
