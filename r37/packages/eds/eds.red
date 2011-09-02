module eds;
%
%				EDS V2.1
%
%
% Author: 	David Hartley		                           
% 		GMD - German National Research Center 
% 	              for Information Technology 
% 		D-53754 St Augustin 
% 		Germany             
% 
% 		email: 	David.Hartley@gmd.de
%
%
% Description: 	EDS is a REDUCE package for symbolic analysis of partial
% 		differential equations using the geometrical approach of
% 		exterior differential systems. The package implements
% 		much of exterior differential systems theory, including
% 		prolongation and involution analysis, and has been
% 		optimised for large, non-linear problems.
%
%
% Requires:	REDUCE 3.6 patched to 25 Apr 96 or later
%
% Created:	23/6/90  V0	as es.red (with Robin W Tucker)
%
% Modified:     8/8/90   V0.1   Added quasi-linear solving and consist-
%                               ency conditions for simultaneous eqns
%		11/9/90	 V0.2	Added resimp in front of all subf, subsq
%				etc as temporary fix for subf bug.
%               14/5/91  V0.3   Switched off factor (and on exp) in
%                               various routines to make simplifications
%                               work.
%		22/5/91	 V0.4	Added subs2 in front of all resimp as
%				temporary fix for unseen power LET rule
%				bug.
%               26/11/91 V0.5   Altered algorithm in regchn so that
%                               alpha coefficients are not chosen until
%                               entire chain has been constructed.
%		30/6/92  V1	Renamed exsys.red.
%				Radically altered exsolve to use modulo
%				rather than contraction. Eliminated need
%                               for frame vectors. Added extra switches
%                               to allow given, random or (as before)
%                               generic combinations of the independence
%                               1-forms to be used to construct a
%                               regular chain.
%				Removed many utilities to tools.red.
%		23/7/92	 V1.1	Added module `complete'
%		14/3/94  V1.2	Renamed eds.red.
%                               Modified for independent compilation,
%                               and for compatibility with new xideal.
%               25/4/96  V2.0   Total rewrite using parts of earlier
%                               versions.
%				Added types for EDS and coframing.
%		??	 ??	Made cross a bundle product when arguments
%				share submanifold


% Other packages which must be loaded at run-time.

load_package solve,excalc,xideal;
 
create!-package('(
 
     eds	% Header module
     edseval	% Definition and manipulation of eds structure for exterior systems 
     edscfrm	% Coframing structure for EDS
     systems	% Operations on exterior differential systems
     tableaux	% Definition and manipulation of tableaux using tab structure
     contact	% Contact systems on jet bundles and Grassmann bundles
     invol	% Cartan characters, reduced characters, involution test
     prolong	% Prolonged systems, tableaux
     pullback	% Pullback transformations
     restrict	% Restrict to a subset of a coframing
     transfrm	% Cobasis transformations
     edspde	% PDE interface to EDS
     edsequiv	% Check if EDS structures are equivalent
     edsuser	% Miscellaneous user functions
     edsnorml	% Converting exterior systems to internal form
     edssolve	% Specialised solvers for EDS
     disjoin	% Convert a variety to a disjoint union of sub-coframings
     element	% Generate a random integral element
     edsaux	% Miscellaneous support functions
     edsexptl	% Experimental (algebraic mode) operators
     edspatch	% Various patches for other parts of Reduce.
 
		),'(contrib eds));
 
% Switches

fluid '(!*edsverbose !*edsdebug !*edssloppy !*edsdisjoint !*genpos
	!*ranpos);

switch edsverbose;	% prints calculation traces when on
switch edsdebug;	% prints debugging information when on
switch edsdisjoint;	% allows automatic variety decomposition when on
switch edssloppy;	% treat quasilinear systems as semilinear
switch genpos;          % Calculate characters with system in general
			% position
switch ranpos;          % Calculate characters with system in random
			% position

put('genpos,'simpfg,'((t (setq !*ranpos nil))));
put('ranpos,'simpfg,'((t (setq !*genpos nil))));


% Global variables

fluid '(cfrmcob!* cfrmcrd!* cfrmdrv!* cfrmrsx!* pullback_maps
	dependencies);

cfrmcob!* := nil;     % cobasis for background coframing
cfrmcrd!* := nil;     % coordinates for background coframing
cfrmdrv!* := nil;     % structure equations for background coframing
cfrmrsx!* := nil;     % restrictions for background coframing as pf
pullback_maps:= makelist {}; % list of maps used by last call to prolong
dependencies := makelist {}; % dependencies removed by pde2eds

flag('(pullback_maps dependencies),'share);
 
% Macros used throughout


symbolic smacro procedure eds_sys s;
   cadr s;

symbolic smacro procedure eds_ind s;
   caddr s;

symbolic smacro procedure eds_cfrm s;
   cadddr s;

symbolic smacro procedure eds_props s;
   car cddddr s;

symbolic smacro procedure cfrm_cob m;
   cadr m;

symbolic smacro procedure cfrm_crd m;
   caddr m;

symbolic smacro procedure cfrm_drv m;
   cadddr m;

symbolic smacro procedure cfrm_rsx m;
   nth(m,5);


% Macro for edscall


symbolic macro procedure edscall u;
   % evaluate form cadr u within edsprotect
   function edsprotect .
      foreach x in cdr u collect
         function list . mkquote car x . cdr x;


%%%% Form function for edscall
%%%
%%%
%%%put('edscall,'formfn,'formedscall);
%%%
%%%symbolic procedure formedscall(u,v,mode);
%%%   % evaluate form cadr u within edsprotect
%%%   function edsprotect .
%%%      foreach x in formlis(cdr u,v,mode) collect
%%%         function list . mkquote car x . cdr x;


% Macros from excalc for compilation


smacro procedure !*k2pf u;
   u .* (1 ./ 1) .+ nil;

smacro procedure negpf u;
   multpfsq(u,(-1) ./ 1);

smacro procedure lowerind u;
   list('minus,u);
 
endmodule;
end;
