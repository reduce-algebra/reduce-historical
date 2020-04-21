#! /bin/csh -f
# Build a compiled version of EMODE for Vax Unix.
#
# This builds a "COMPLETE SYSTEM"--modifying Rlisp to use the "Rlisp
# interface".

rlisp << 'EOF'              # Portable Standard Lisp version of RLISP
load Useful$    % Don Morrison's utilities.
load Nstruct$   % Routines for structures.
load common$
load SysLisp$
load If!-System$ % Routines for condition exectution based on machine.

OFF USERMODE$   % So we can redefine things.

% Cause constants and structures to be defined at both compile and runtime.
flag( '(DefStruct DefConst), ' EVAL);

% Build EMODE in two parts, due to size problems with FASL
% builder.  (May be unnecessary these days.)
% emode-b-1.b and emode-b-2.b are to be loaded with emode.lap.
faslout "emode-b-1"$
in "emode-files-1.r";
faslend;

faslout "emode-b-2"$
in "emode-files-2.r";
!*GC := NIL$           % Turn off garbage collection messages after
                       % EMODE is loaded, since printing messages
                       % causes consing.
faslend;
quit;
'EOF'
