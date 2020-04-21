%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fast-EVectors.sl -- Fast compiled EVector operations
%%% Author: Cris Perdue
%%% Date:  8 Apr 1983
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% This is a facility so the user can generate code to access
%%% evectors that runs fast.  To use this facility, LOAD (don't
%%% IMPORT) it at compiletime.  It does an (on fast-evectors),
%%% turning on the generation of faster code.  The feature may be
%%% turned off and on by the user.  The affected evector
%%% functions are EGetV, EPutV, and ESizeV.

(compiletime (load if-system data-machine))

(put 'fast-evectors 'simpfg '((t (enable-fast-evectors))
			      (nil (disable-fast-evectors))))

(if_system VAX
(de enable-fast-evectors ()
  (DefList '((EGetV (lambda (V I) (EVecItm (EVecInf V) I)))
	     (EPutV (lambda (V I X) (PutEVecItm (EVecInf V) I X)))
	     (ESizeV (lambda (V) (EVecLen (EVecInf V))))) 'CMacro)))

(if_system PDP10		% tags don't need to be stripped on the PDP10
(de enable-fast-evectors ()
  (DefList '((EGetV (lambda (V I) (EVecItm V I)))
	     (EPutV (lambda (V I X) (PutEVecItm V I X)))
	     (ESizeV (lambda (V) (EVecLen V)))) 'CMacro)))

(if_system MC68000		% tags don't need to be stripped on the 68000
(de enable-fast-evectors ()
  (DefList '((EGetV (lambda (V I) (EVecItm V I)))
	     (EPutV (lambda (V I X) (PutEVecItm V I X)))
	     (ESizeV (lambda (V) (EVecLen V)))) 'CMacro)))

(de disable-fast-evectors ()
  (remprop 'egetv 'cmacro)
  (remprop 'eputv 'cmacro)
  (remprop 'esizev 'cmacro))

(loadtime (on fast-evectors))
