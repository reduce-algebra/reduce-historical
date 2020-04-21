%
% HELP.RED - User assistance and documentation
% 
% Author:      Eric Benson and Martin Griss
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        23 October 1981
% Copyright (c) 1981 University of Utah
%
% 30 Dec, 1982, MLG
%   Move IF_SYSTEM to the Build file
%  <PSL.UTIL.NEWVERSIONS>HELP.RED, 30-Nov-82 16:31, Edit by GALWAY
%   Changed "FLAG" to "SWITCH" to avoid confusion with flags on property
%   lists and to bring terminology in line with PSL manual.
%  <PSL.UTIL>HELP.RED.3,  1-Dec-82 16:16:39, Edit by BENSON
%  Added if_system(HP9836, ... )
%  <PSL.UTIL>HELP.RED.4, 10-Aug-82 00:54:26, Edit by BENSON
%  Changed ReadCh to ReadChar in DisplayHelpFile
%  <PSL.INTERP>HELP.RED.5, 31-May-82 11:50:48, Edit by GRISS
%  Make it LAPIN Help.Tbl
% Changed: to use PH:

% Display help texts, invoke interactive HELPs or print default values

% Place a HELP function on topic name under 'HelpFunction
% Or HELP file on topic name under 'HelpFile
% Or even a short string under 'HelpString (this may be removed)

fluid '(TopLoopRead!*
	TopLoopPrint!*
	TopLoopEval!*
	TopLoopName!*
	HelpFileFormat!*
        Options!*
	!*Echo
	HelpIn!*
	HelpOut!*
	!*Lower
	!*ReloadHelpTable
	HelpTable!*
);

!*ReloadHelpTable := T;

lisp procedure ReloadHelpTable();
% Set !*ReloadHelpTable to T to cause a fresh help table to be loaded
    if !*ReloadHelpTable then
    <<  LapIn HelpTable!*;
	!*ReloadHelpTable := NIL >>;

lisp procedure DisplayHelpFile F;	
% Type help file about 'F'
begin scalar NewIn, C, !*Echo;
    (lambda(!*Lower);
	F := BldMsg(HelpFileFormat!*, F))(T);
    NewIn := ErrorSet(list('Open, MkQuote F, '(quote Input)), NIL, NIL);
    if not PairP NewIn then
	ErrorPrintF("*** Couldn't find help file %r", F)
    else
    <<  NewIn := car NewIn;
	while not ((C := ChannelReadChar NewIn) = char EOF) do WriteChar C;
	Close NewIn >>;
end;

fexpr procedure Help U;			
% Look for Help on topics U
begin scalar OldOut;
    OldOut := WRS HelpOut!*;
    ReloadHelpTable();			% Conditional Reload
    HelpTopicList U;
    WRS OldOut;
end;

lisp procedure HelpTopicList U;
% Auxilliary function to prind help for each topic in list U
    if null U then HelpHelp()
    else for each X in U do
    begin scalar F;
	if F := get(X, 'HelpFunction) then Apply(F, NIL)
	else if F := get(X, 'HelpFile) then DisplayHelpFile F
	else if F := get(X, 'HelpString) then Prin2T F
	else DisplayHelpFile X; % Perhaps a File Exists.
    end;

lisp procedure HelpHelp();
% HELPFUNCTION: for help itself
<<  DisplayHelpFile 'Help;
    FindHelpTopics();
    PrintF("%nOptional modules now loaded:%n%l%n",Options!*);
 >>;

lisp procedure FindHelpTopics();
% Scan the ID HAST TABLE for loaded HELP info
<<  PrintF("Help is available on the following topics:%n");
    MapObl Function TestHelpTopic;
    TerPri();
    PrintF("The files in the help directory can be read using Help.%n") >>;

lisp procedure TestHelpTopic X;         
% auxilliary function applied to each ID to see if
% some help info exists
    if get(X, 'HelpFunction) or get(X, 'HelpFile) or get(X, 'HelpString) then
    <<  Prin2 '! ; 
	Prin1 X >>;

lisp procedure HelpTopLoop();
% HELPFUNCTION: for TopLoop, show READER/WRITERS
<<  DisplayHelpFile 'Top!-Loop;
    if TopLoopName!* then
    <<  PrintF("%nCurrently inside %w top loop%n", TopLoopName!*);
	PrintF("Reader: %p, Evaluator: %p, Printer: %p%n",
		TopLoopRead!*, TopLoopEval!*, TopLoopPrint!*) >>
    else PrintF("%nNot currently inside top loop%n") >>;

% Switch and global help - record and display all switches and globals.

lisp procedure DefineSwitch(Name, Info); 	
% Define important switch
% Name does Not have the !*, Info should be a string.
%
<<  put(Name, 'SwitchInfo, Info);
    Name >>;

lisp procedure Show1Switch(Name);		
% Display a single switch
begin scalar X;
    Prin1 Name; 
    Tab 15; 
    Prin1 Eval Intern Concat("*", ID2String Name);
    If (X := Get(Name, 'SwitchInfo)) then
    <<  Tab 25;
	Prin2 X >>;
    TerPri();
end;

lisp procedure ShowSwitches L;		
% Display all switches in a list
<<  if not PairP L then MapObl function TestShowSwitch;
    for each X in L do Show1Switch X >>;

lisp procedure TestShowSwitch X;
% Support function for 1 switch display
  if get(X, 'SwitchInfo) then Show1Switch X;

lisp procedure DefineGlobal(Name, Info);
% Define important global
% Name is an ID, Info should be a string.
%
<<  put(Name, 'GlobalInfo, Info);
    Name >>;

lisp procedure Show1Global Name;	
% Display a Single Global
begin scalar X;
    Prin1 Name; 
    Tab 15; 
    Prin1 Eval Name;
    If (X := get(Name, 'GlobalInfo)) then
    <<  Tab 25;
	Prin2 X >>;
    TerPri();
end;

lisp procedure TestShowGlobal X;
% Support for GLOBAL info
    if get(X, 'GlobalInfo) then Show1Global X;

lisp procedure Show1State Name;
% Display a single switch or global
<<  if get(Name, 'GlobalInfo) then Show1Global Name;
    if get(Name, 'SwitchInfo) then Show1Switch Name >>;

lisp procedure ShowGlobals L;		
% Display all globals in a list
<<  if not PairP L then MapObl Function TestShowGlobal;
    for each X in L do Show1Global X >>;

lisp procedure ShowState L;		
% Display all globals in a list
<<  if not PairP L then MapObl function TestShowState;
    for each X in L do Show1State X >>;

lisp procedure TestShowState X;
% Support for a Global
    if get(X, 'SwitchInfo) or get(X, 'GlobalInfo) then Show1State X;

END;
