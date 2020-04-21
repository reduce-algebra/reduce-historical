%
% LOAD.RED - New version of LOAD function, with search path
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        2 April 1982
% Copyright (c) 1982 University of Utah
%

%  <PSL.KERNEL>LOAD.RED.17, 23-Mar-83 11:44:39, Edit by KESSLER
%  Change Apollo Load directory
% Edit by Cris Perdue, 21 Mar 1983 1440-PST
% Put "" back in loaddirectories*.  Fun, huh?
% Edit by Cris Perdue,  7 Mar 1983 1527-PST
% Removed ".sl" from loadextensions* and "" from loaddirectories*.
% Edit by MLG, 6 March 1983. 
%  Corrected bug in fix to Imports -- "else" was matched with incorrect "then".
% Edit by Cris Perdue, 17 Feb 1983 1201-PST
% Corrected use of *verboseload in top of load1
%  MLG, 15 Feb 1983
%   Added !*VERBOSELOAD and !*PRINTLOADNAMES
%  M. Griss, 9 Feb 1983
%   Changed LoadDirectories!* for the VAX to refer to "$pl/"
%  <PSL.NEW>-SOURCE-CHANGES.LOG.15, 15-Dec-82 15:45:55, Edit by PERDUE
%  LOAD will now handle ".sl" extension
%  <PSL.KERNEL>LOAD.RED.7,  1-Dec-82 16:07:38, Edit by BENSON
%  Added if_system(HP9836, ...)
% EDIT by GRISS 28 Oct 1982: Added EvLoad to Imports
%  <PSL.KERNEL>LOAD.RED.4,  4-Oct-82 09:46:54, Edit by BENSON
%  Moved addition of U to Options!* to avoid double load
%  <PSL.KERNEL>LOAD.RED.3, 30-Sep-82 11:57:03, Edit by BENSON
%  Removed "FOO already loaded" message
%  <PSL.KERNEL>LOAD.RED.2, 22-Sep-82 15:38:48, Edit by BENSON
%  Added ReLoad, changed VAX search path

fluid '(LoadDirectories!*		% list of strings to append to front
	LoadExtensions!*		% a-list of (str . fn) to append to end
					% and apply
	PendingLoads!*			% created by Imports, aux loads
	!*Lower				% print IDs in lowercase, for building
					% filename for Unix
	!*RedefMSG			% controls printing of redefined
					% function message
	!*UserMode			% Controls query of user for redefining
					% system functions
	!*InsideLoad			% Controls "already loaded" message
	!*VerboseLoad			% Print REDEFs and LOAD file names
	!*PrintLoadNames		% Print Names of files loading
	Options!*);			% list of modules already loaded

if_system(Apollo,
	  LoadDirectories!* := '("" "~p/l/"));
if_system(Tops20,
	  LoadDirectories!* := '("" "pl:"));
if_system(Unix,
	  LoadDirectories!* := '("" "$pll/" "$pl/"));
if_system(HP9836,
	  LoadDirectories!* := '("" "pl:"));
if_system(Wicat,
	  LoadDirectories!* := '("" "PSL.LAP/"));

LoadExtensions!* := '((".b" . FaslIN) (".lap" . LapIN));
!*VerboseLoad :=NIL;
!*PrintLoadNames := NIL;

macro procedure Load U;
    list('EvLoad, MkQuote cdr U);

lisp procedure EvLoad U;
    for each X in U do Load1 X;

macro procedure ReLoad U;
    list('EvReLoad, MkQuote cdr U);

lisp procedure EvReLoad U;
<<  for each X in U do Options!* := Delete(X, Options!*);
    EvLoad U >>;

lisp procedure Load1 U;
begin scalar !*RedefMSG, !*UserMode, LD, LE, F, Found;
    If !*VerBoseLoad then !*RedefMSG := T;	
    return if U memq Options!* then
	if !*VerboseLoad then
	    ErrorPrintF("*** %w already loaded", U)
	else NIL
    else
(lambda(!*InsideLoad);
<<  LD := LoadDirectories!*;
    (lambda (!*Lower);
    while not null LD and not Found do
    <<  LE := LoadExtensions!*;
	while not null LE and not Found do
	<<  if FileP(F := BldMsg("%w%w%w", first LD, U, car first LE)) then
		Found := cdr first LE;	% Found is function to apply
	    LE := rest LE >>;
	LD := rest LD >>)(T);
    if not Found then
	StdError BldMsg("%r load module not found", U)
    else
    <<  Options!* := U . Options!*;
	If !*VerboseLoad or !*PrintLoadNames
	   then ErrorPrintf("*** loading %w%n",F);
	Apply(Found, list F);
	while not null PendingLoads!* do
	<<  Found := car PendingLoads!*;
	    PendingLoads!* := cdr PendingLoads!*;
	    Load1 Found >> >> >>)(T);
end;

lisp procedure Imports L;
    if !*InsideLoad then
	<<for each X in L do
	    if not (X memq Options!* or X memq PendingLoads!*) then
		PendingLoads!* := Append(PendingLoads!*, list X)>>
     else EvLoad L;

END;
