%
% BUILD.RED - Compile a load module
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        23 March 1982
% Copyright (c) 1982 University of Utah
%
% Edit by MLG, 9 Feb, chchanged Buildformat to use $pl/
%  <PSL.UTIL>BUILD.RED.3,  1-Dec-82 16:12:33, Edit by BENSON
%  Added if_system(HP9836, ... )

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

lisp procedure Build X;
begin scalar !*UserMode, !*quiet_faslout;
    !*quiet_faslout := T;
    (lambda (!*Lower);
    <<  FaslOut BldMsg(BuildFileFormat!*, X);
	X := BldMsg("%w.build", X) >>)(T);
    EvIn list X;
    FaslEnd;
end;

END;
