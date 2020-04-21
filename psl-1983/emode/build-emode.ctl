! Build a compiled version of EMODE for the DEC-20.
!
! Use DO or SUBMIT to "run" this file.
!
! Make sure you define the necessary logical names in your BATCH.CMD file.
! The best way is to include a line something like the following:
!   @take <PSL>LOGICAL-NAMES.CMD
!

@define DSK:  DSK:, PE:
@PSL:RLISP              ! Portable Standard Lisp version of RLISP
*load Useful$    % Don Morrison's utilities
*load NSTRUCT$   % Routines for structures
*load common$    % Common-Lisp compatibility package
*load SysLisp$
*load If!-System$ % Allow conditional compilation based on machine type.
*load monsym$    % Define JSYS stuff
*load jsys$      % Still more JSYS stuff
*OFF USERMODE$   % So we can redefine things.
*
* % Cause constants and structures to be defined at both compile and
* % runtime????
* FLAG( '(DefStruct DefConst), ' EVAL); % Space after ' in case of MIC
*
* % Build EMODE in two parts, due to size problems with FASL
* % builder.  (May be unnecessary these days.)
* % EMODE-B-1 and EMODE-B-2 are to be loaded with EMODE.LAP.
*FASLOUT "EMODE-B-1"$
* IN "EMODE-FILES-1.RED";
*FASLEND;
*
*FASLOUT "EMODE-B-2"$
* IN "EMODE-FILES-2.RED";
* !*GC := NIL$           % Turn off garbage collection messages after
*                        % EMODE is loaded, since printing messages
*                        % causes consing.
*FASLEND;
*
*QUIT$
