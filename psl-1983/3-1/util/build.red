%
% BUILD.RED - Compile a module from .BUILD or .RED file
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        23 March 1982
% Copyright (c) 1982 University of Utah
%
% Edit by MLG, 9 April 1983
%  added MakeBuildFilename, and ERRSET, so Build more  robust
%  and more like  Compile-file. Also turned off break,
%  and do closing FASLEND in case of error.
% Edit by Cris Perdue, 23 Mar 1983 0856-PST
%  Added BuildFileFormat for Apollo as requested by Kessler
% 07-Mar-83  Nancy Kendzierski
%  Added load if-system, since many .build files use the if_system macro.
% 09-Feb-83  MLG
%  Changed Buildformat to use $pl/
% <PSL.UTIL>BUILD.RED.3,  1-Dec-82 16:12:33, Edit by BENSON
%  Added if_system(HP9836, ... )

Compiletime load if!-system;
Imports '(If!-system);		        % useful for most "built" systems

fluid '(!*quiet_faslout			% turns off welcome message in faslout
	!*Lower				% lowercase ids on output
	!*UserMode			% query on redefinition
	BuildFileFormat!*
);

if_system(Tops20,
	  BuildFileFormat!* := "pl:%w");
if_system(Unix,
	  BuildFileFormat!* := "$pl/%w");
if_system(HP9836,
	  BuildFileFormat!* := "pl:%w");
if_system(Apollo,
          BuildFileFormat!* := "~p/l/%w");

Lisp Procedure MakeBuildFileName(ModuleName,ExtList);
% Try to construct Filename form Modulename
 Begin scalar y;
  If Null ExtList then return StdError
	 BldMsg("Cant find a complete filename for %r",ModuleName);
  If FileP(y:=BldMsg("%w.%w",ModuleName,car Extlist)) then
	return <<ErrorPrintF("--- Building %w%n",Y); Y>>;
  Return MakeBuildFileName(ModuleName,Cdr ExtList);
 End;

lisp procedure Build X;
 Begin scalar result;
	result:=Errset(BuildAux X, T);
	if fixp Result then 
	    <<if !*WritingFaslFile then faslend;
	      Errorprintf("***** Error during build of %w%n",X)>>;
 End;

Lisp Procedure BuildAux X;
begin scalar !*UserMode, !*quiet_faslout,y,!*break,result;
    !*quiet_faslout := T;
    (lambda (!*Lower);
    <<  y:=MakeBuildFileName(X,'(build red sl));
        faslout BldMsg(BuildFileFormat!*, X) >>)(T);
    EvIn list y;   % Examines .RED, .SL
    FaslEnd;
end;

END;




