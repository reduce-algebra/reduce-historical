module taylor;

%****************************************************************
%
%                      THE TAYLOR PACKAGE
%                      ==================
%
%****************************************************************
%
%  Copyright (C) 1989,1990,1991,1992,1993 by Rainer M. Schoepf,
%   all rights reserved.
%
%  Copying of this file is authorized only if either
%  (1) you make absolutely no changes to your copy, including name, or
%  (2) if you do make changes, you name it something other than
%      taylor.red.
%
%  Error reports please to: Rainer Schoepf
%                           Konrad-Zuse-Zentrum
%                           fuer Informationstechnik Berlin
%                           Heilbronner Str. 10
%                           D-10711 Berlin
%                           Federal Republic of Germany
%                   Email:  <Schoepf@sc.ZIB-Berlin.De>
%
%
%  This package implements a new data structure:
%
%            >>> the TAYLOR KERNEL <<<
%
%  and the functions necessary to operate on it.
%
%  A TAYLOR KERNEL is a kernel of the following form:
%
%   (Taylor!* TayCoeffList TayTemplate TayOrig TayFlags)
%
%   where:
%    Taylor!*        is a symbol identifying the kernel.
%    TayCoeffList    is a list of TayCoeff's.
%                    A TayCoeff is a pair
%                     (TayPowerList . StandardQuotient).
%                    A TayPowerList is a list of TayDegreeList's,
%                    each of which is a list of integers that
%                    denote the exponents of the Taylor variables.
%    TayTemplate     is a list of lists, each containing the three
%                    elements  TayVars, TayPoint, TayOrder,
%                    TayNextOrder, (being the list of variables, the
%                    expansion point the power of the variable in the
%                    last coefficient computed, and the power of the
%                    next coefficient, respectively)
%                    It is used as a template for the car of the
%                    pairs in a TayCoeffList.
%    TayOrig         is the original expression to be Taylor expanded,
%                    as a standard quotient. It is held mainly for
%                    the use by possible future extensions.
%                    This part is nil if the switch taylorkeeporiginal
%                    was off during expansion.
%    TayFlags        is currently unused and reserved for future
%                    extensions.
%
%
%
%*****************************************************************
%
%       Revision history
%
%*****************************************************************
%
%
% 23-Sep-93    1.8c
%   Changed Taylor!-generate!-big!-O to produce a capital O even if
%    !*lower is t.
%
% 03-Sep-93    1.8b
%   Changed multintocoefflist to bind !*sub2 and first call subs2
%    and then resimp so that power substitutions are carried out
%    properly.
%
% 02-Sep-93    1.8a
%   Corrected stupid error that caused car/cdr of an atom.
%   Corrected code for expansion by differentiation and for caching
%    of intermediate results to use the template of the computed kernel
%    rather than the template given to the expansion functions.
%   Corrected truncate!-Taylor!* to correctly handle the case that
%    the kernel to be truncated has already fewer terms than expected.
%
% 17-Aug-93    1.8
%   Improvement in expttayrat if zero is raised to negative power;
%    this might be a case for expansion to higher order.
%   Rewritten code for expansion by differentiation to handle
%    occurrence of a kernel in its own derivative.
%
%
% 12-Aug-93    1.7e
%   Rewritten part of the expansion by differentiation code to try
%    to differentiate once and expand the result.
%   Slight improvement in trace printing.
%
% 13-Jul-93    1.7d
%   Corrected taysimpasin and taysimpasin!* to handle branch points
%    correctly.
%   simpTaylor!* can now handle free variables as part of Taylor!*
%    kernel.
%
% 28-Jun-93    1.7c
%   Corrected error introduced by last change to mult!.comp!.tp!.
%    and inv!.tp!.
%
% 21-Jun-93    1.7b
%   Changed taylor2f to not collect zero coefficients into coefflist.
%   Changed subs2coefflist to leave out zero coefficients.
%   Set default of Taylorautocombine switch to on.
%   Replaced calls to subs2!* be calls to subs2 with explicit binding
%    of !*sub2.
%
% 17-Jun-93    1.7a
%   Added get!-min!-degreelist to find minimum degree list of a
%    coefflist; corrected mult!.comp!.tp!. and inv!.comp!.tp!. to
%    use it.
%   Corrected error in quottaylor that could wrongly calculate first   
%    coefficient of result.
%   Shortened lines in tayfns.red to 72 characters.
%   Improved taysimpexp and expttayrat!* to yield an exponential only
%    if there are no analytical terms.
%   Added code to expand inverse trigonometric and hyperbolic functions
%    around their logarithmic singularities.
%   Corrected taysimpsin, improved taysimpsinh.
%
% 15-Jun-93    1.7
%   Added a lot of utility functions to test for zero, constant,
%    and nonzero constant Taylor series.
%   Rewritten the Taylor simplification functions in modules TaySimp
%    and TayFns to handle non-analytical terms as well.
%   Split expttayrat into two procedures.
%
%
% 14-Jun-93    1.6
%   Added Taylor!*!-zerop and TayCoeffList!-zerop.
%   Introduced addtaylor!-as!-sq, multtaylor!-as!-sq, and
%    quottaylor!-as!-sq.
%   Changed taymincoeff into an expr procedure, added tayminpowerlist.
%   simptaylor returns now the input if it cannot expand.
%   Added trtaylor switch and some procedures for tracing.
%   Added Taylor!-error!*, currently used only in taysimplog.
%   Added TayExpnd module, removed taylorexpand from tayinterf module.
%   Moved taymincoeff and tayminpowerlist into TayUtils module.
%   Corrected procedures in tayfns.red to not crash for Taylor kernels
%    with empty TayCoefflist.
%
%
% 10-Jun-93    1.5b
%   Introduced get!-degreelist.
%   Rewritten exceeds!-order to be tail-recursive.
%   Changed protocol for makecoeffs call, introduced makecoeffs0.
%   Introduced !*TayExp2q to convert a power into a standard quotient.
%
% 09-Jun-93    1.5a
%   Introduced has!-TayVars predicate, replaced calls to smemberlp
%    and smemqlp by it.
%   Split addtaylor, multtaylor and quottaylor into two procedures.
%   Replaced taysimpp by expttayi in difftaylor.
%   Introduced taysimpsq!* (taysimpsq with taylorautoexpand off). 
%
% 08-Jun-93    1.5
%   Introduction of special operations for exponents in powerlists:
%    TayExp!-<op>. The macro Taylor!: is used to replace normal
%    operations by these.
%    Changed makecoeffpairs and addcoeffs to avoid consing pairs and
%     taking them apart again.
%    Changed makecoeffpairshom to allow steps different from 1.
%
%
% 07-Jun-93    1.4l
%   Introduced TayNextOrder and made corresponding changes to the
%    sources.
%   Added addto!-all!-TayTpElOrders function.
%   Corrected problem in expttayrat that would result in extra (wrong)
%    terms if there was no constant term in the Taylor series to be
%    raised to a power.
%   Removed some calls to !*n2f.
%   Replaced call to subs2 by subs2!* in taysimptan.
%   Corrected generation of new template in inttaylorwrttayvar
%
% 03-Jun-93    1.4k
%   Changed expttayi to handle negative integer powers as well,
%    changed taysimpp correspondingly.
%
% 27-May-93    1.4j
%   Corrected some oversights in taysimpexpt.
%   Added smacro TayTpVars.
%   Added code to taysimpp to handle s.f. as main variable.
%   Added function subtr!-degrees to tayutils.red.
%   Corrected error in var!-is!-nth in tayintro.red.
%   Corrected error in taydegree!-lessp in tayutils.red that caused
%    incorrect ordering of coefficients.
%   Corrected error in quottaylor1.
%
% 06-May-93    1.4i
%   Changed printing routines for better interface to fancy printing.
%
% 21-Apr-93    1.4h
%   Corrected cosec --> csc, cosech --> csch in tayfns.red.
%   Corrected taysimpsin to calculate coth with correct overall sign.
%   Improved handling of poles in taysimptan.
%   Rewritten taysimpsin to use exponential rather than tangent.
%   Added slight optimization in taylorsamevar if desired matches
%    actual expansion order.
%
% 01-Apr-93    1.4g
%   Changed multtaylor1 so that in multiplication of Taylor kernels
%    products would be simplified as well.
%   Changed taysimptan to allow expansion point to be a pole of tan,
%    cot, tanh, or coth.
%   Changed constant!-sq!-p and taysimpexpt to recognize constant
%    expression in first argument to expt.
%   Changed printing so that only non-vanishing terms are counted.
%   Introduced taysimpsinh for simplifcation of expressions involving
%   sinh, coth, sech, csch.
%   Added subs2coefflist and resimpcoefflist smacros.
%   Modified addtaylor1, multtaylor1, quottaylor1 and invtaylor1 to call
%    subs2coefflist.
%   Protected klist property of atom Taylor!* against being filled
%    unnecessarily with intermediate results.
%   Changed expttayrat to print a more meaningful error message when it
%    detects a branch point.
%   Improved taysimpexpt to handle more cases.
%
% 08-Mar-93    1.4f
%   Changed printing of Taylor kernels to include `(nn terms)' instead
%    of `...'.
%
% 25-Feb-93    1.4e
%   Made expttayi more efficient by replacing straightforward 
%    multiplication by a scheme that computes powers of 2.
%   Corrected error in taysimpexp.
%
% 24-Feb-93    1.4d
%   Corrected error in taydiffp.
%   Made especially multtaylor1, but also addtaylor1 and expttayrat
%    more efficient.
%
% 01-Feb-93    1.4c
%   Corrected error in tayrevert1: constant term was missing.
%   Changed frlis!* from global to fluid.
%   Corrected error in taylor2f that caused certain expressions, like
%    i^2, not to be simplified correctly (discovered by Stan Kameny).
%
% 16-Apr-92    1.4b
%   Corrected errors in taysimpexpt, mult!.comp!.tp!., and expttayrat.
%   The error corrected in version 1.4a was also present in invtaylor.
%   Corrected error in taylor2e/taylor2f.
%   Improved printing of negative coefficients.
%   Corrected error in subsubtaylor.
%   Corrected error in taysimpexp.
%   Added simp0fn property to Taylor!* for proper handling of
%    substitutions.
%   Added make!-cst!-coefficient smacro.
%   Added partial suppression of printing of coefficients and
%    TAYLORPRINTTERMS variable.
%
% 11-Feb-92    1.4a
%   Corrected error in quottaylor1: If numerator or denominator had
%    a zero constant term, the result had the wrong number of terms.
%
% 09-Jan-92    1.4
%   Implemented Taylor reversion.
%
%
% 07-Jan-92    1.3e
%   Corrected bug in taysimpsin: wrong type of return value.
%
% 27-Nov-91    1.3d
%   Corrected glitch in quottaylor1: Taylor kernel representing 0
%    as numerator gave an error.
%
% 06-Nov-91    1.3c
%   Improved support for integration of expressions involving Taylor
%    kernels.
%
% 31-Oct-91    1.3b
%   Added (limited) support for the integration of Taylor kernels.
%
% 19-Jul-91    1.3a
%   Introduced taysimpmainvar for main variables that are standard
%    forms, as in factored expressions. Changed taysimpp accordingsly.
%   Introduced new smacros !*tay2f and !*tay2q that make Taylor kernels
%    unique.
%
% 03-Jun-91    1.3
%   Started version for REDUCE 3.4.
%   Updated diffp according to changes for REDUCE 3.4.
%   Replaced freeof by has!-Taylor!* in taysimpf and taysimpt, and by
%    depends in difftaylor and taylor2e.
%   Changed module names to conform to file names.
%   Moved (nearly) all smacros into header (taylor) module,
%    made cst!-Taylor!* an smacro, moved remaining functions from
%    taymacros into tayutils, deleted taymacros module.
%   Made makecoeffpairs (in module taybasic) from smacro to expr.
%   Changed taylorsamevar to use TayOrig part of Taylor kernel if
%    present.
%   Changed Taylor printing functions since it is now possible to
%    pass information of operator precedence (via pprifn property).
%   Fixed bug in subsubtaylor (found by H. Melenk): did not substitute
%    in expansion point.
%   Changed error handling to call new function rerror instead of
%    rederr.
%   Changed for use of new hooks prepfn2, subfunc, and dfform
%    instead of redefining sqchk, subsublis, and diffp.
%
% 20-Oct-90    1.2j
%   Added check in subsubtaylor for variable dependency.
%   Fixed stupid bug in taylorsamevar.
%   Corrected taysimpexpt to handle rational exponents with ON RATIONAL.
%   Corrected expttayrat: looks now at first NON-ZERO coefficient to
%    determine whether root can safely be computed.
%   Fixed bug in mult!.comp!.tp..
%   Added error check in invtaylor1 and quottaylor1.
%   Fixed bug in quottaylor1 that produced wrong results for
%    multidimensional Taylor series, introduced taydegree!-min2 and
%    taydegree!-strict!-less!-or!-equal!-p.
%
% 05-Oct-90    1.2i
%   Replaced variable name nn by nm in taysimpsq to avoid name conflicts
%    with the SPDE package by Fritz Schwartz.
%   Replaced call to apply by apply1 in taysimpkernel.
%   Minor changes in expttayrat, taysimplog, taysimpexp, and taysimptan:
%    inserted explicit calls to invsq to allow negative numbers in
%    denominator.
%   Fixed bugs in difftaylorwrttayvar, inttaylorwrttayvar and
%    subsubtaylor: treatment of a Taylor kernel expanded about infinity
%    would give a wrong result.  Found by John Stewart.
%
% 11-Aug-90    1.2h
%   Replaced call to get!* by get in diffp since get!* will no longer
%    be available in REDUCE 3.4.
%   Fixed bug in multintocoefflist that was introduced by replacing
%    car by TayCfPl.
%   Moved setting of TayOrig part from taylor1 to taylorexpand.  This
%    avoids Taylor kernels in the TayOrig part of multidimensional
%    Taylor expansions.  It does not fully solve the problem since
%    they can still be generated by applying the Taylor operator to
%    expressions that do not contain fully Taylor-combined Taylor
%    kernels.
%   Reversed list of expansions in call to taylorexpand in simptaylor.
%    Modified taylor1 accordingly.  Previously this could trigger a
%    `This can't happen' error message (due to incorrect ordering of
%    the Taylor variables).
%   Removed procedures delete!-coeff and replace!-coeff since they are
%    no longer used.
%   Moved call to subs2 out of differentiation loop in taylor2f,
%    improves timing significantly; deleted superfluous declared
%    integer variable.
%   Fixed bug in taylorsamevar.
%   Added extra checks and double evaluation of lists in simptaylor.
%   Replaced a number of ./ by !*f2q, introduced some !*n2f conversion
%    functions.
%   Development frozen, version shipped out.
%
% 06-May-90    1.2g
%   Fixed bug in taylor2e that caused order of kernels in homogenous
%    expansions to be reverted.  Discovered by Karin Gatermann.
%   Removed binomial!-coeff since no longer needed (in expttayrat).
%   Replaced some forgotten car/cdr by TayCfPl/TayCfSq.
%   Reordered import declarations.
%   Replaced many semicolons by dollar signs.  Decreases amount of
%    printing during input of this file.
%   Minor bug fix in taysimpsin.
%   Minor change in taysimpasin and taysimpatan.
%   Split inttaylorwrttayvar into two procedures, added check for
%    logarithmic term in integration procedure inttaylorwrttayvar1.
%   Replaced combination of addsq and negsq by subtrsq in quottaylor1,
%    subsubtaylor and taysimplog.
%   Renamed taygetcoeff to TayGetCoeff (doesn't make any difference
%    on case-insensitive systems).
%   Minor changes in taymultcoeffs, multintocoefflist, resimptaylor,
%    taylor1sq, taylor2f, negtaylor1, quottaylor1, invtaylor1,
%    expttayrat, subsubtaylor, difftaylor, taysimpasin, taysimpatan,
%    taysimplog, taysimpexp, taysimptan, difftaylorwrttayvar,
%    inttaylorwrttayvar1, addtaylor1 (cons -> TayMakeCoeff).
%   Similar change in taysimpp (cons -> .**, i.e. to).
%
% 29-Mar-90    1.2f
%   Fixed bug in taysimpf (addition of Taylor kernels) that could cause
%    "This can't happen" message.
%   Fixed bug in difftaylorwrttayvar: arguments to make!-cst!-coefflis
%    were interchanged.
%   Fixed similar bug in expttayrat (this procedure was never used!)
%   Added forgotten call to list in taylor2f.
%   Changed representation of big-O notation: print O(x^3,y^3) instead
%    of O(x^3*y^3) if expansion was done up to x^2*y^2.
%   Introduced new version of expttayrat (algorithm as quoted by Knuth)
%    which is faster by about a factor of two.
%   Fixed Taylor!-gen!-big!-O so that expansion point at infinity
%    is treated correctly for homogeneously expanded series.
%
% 27-Feb-90    1.2e
%   Removed procedures addlogcoeffs, addexpcoeffs and addtancoeffs,
%    inserted code directly into taysimplog, taysimpexp, and
%    taysimptan.
%   taylorvars renamed to TayVars.
%   find!-non!-zero moved into Taylor!:macros module.
%
% 26-Feb-90    1.2d
%   Added following selector, constructor, and modification smacros:
%    TayCfPl, TayCfSq, TayMakeCoeff, set!-TayCfPl, set!-TayCfSq,
%    TayTpElVars, TayTpElPoint, TayTpElOrder.
%   Some minor changes in several procedures to improve readability.
%
% 19-Jan-90    1.2c
%   Removed first argument of addtaylor since never used.
%
% 14-Nov-89    1.2b
%   Added taysimpsin.
%   Split tayinpoly1 off from tayinpoly, modified expttayrat
%    accordingly.
%   Changed global declarations to fluid.  No reason to prevent
%    binding.
%
% 11-Nov-89    1.2a
%   Minor changes in the procedures changed yesterday (cleanup).
%   Added procedure taysimptan.
%   Replaced taylor1sq by taylorexpand in taysimpf1.
%   taysimpsq partly rewritten (will these bugs never die out?)
%   taysimpf1 renamed to taysimpf, taylor!*!-sf!-p to
%    Taylor!-kernel!-sf!-p.
%   Replaced a few of these by Taylor!-kernel!-sq!-p.
%
% 10-Nov-89    1.2
%   Introduced taylorexpand to be the heart of simptaylor and to
%    replace simptaylor in taysimpt and multpowerintotaylor.
%   Added new versions of procedures taysimplog and taysimpexp
%    (Knuth's algorithm).
%   Taylor!:basic module moved up (so that some smacros are
%    defined earlier).
%
%
% 09-Nov-89    1.1b
%   Fixed bug in taylor2e: quottaylor1 got the wrong template so
%    that it would truncate the resulting coeff list.
%   Added call to subs2 after call to diffsq in taylor2f so that
%    expressions containing radicals are simplified already at
%    this point.
%
% 21-Aug-89    1.1a
%   Fixed bug in taysimpp: it could return a s.q. instead of a s.f.
%   Added a few forgotten import declarations.
%
% 31-Jul-89    1.1
%   Slight changes in calls to confusion, minor change in taysimpp.
%   Introduced big-O notation, added taylorprintorder switch.
%   taysimpasin and taysimpatan now also calculate the inverse
%    hyperbolic functions.
%   New smacro Taylor!-kernel!-sq!-p replaces combinations of
%    kernp and Taylor!*p.
%
%
% 24-Jul-89    1.0a
%   Bug fix in constant!-sq!-p: mvar applied to s.q., not s.f.
%   Added safety check in taysimpt.
%
% 27-May-89    1.0
%   Decided to call this version 1.0, it seems to be sufficiently (?)
%    bug free to do so.
%   Minor bug fix in expttayrat (forgotten variable declaration).
%
%
% 25-May-89    0.9l
%   Bug fixed in taylor2e
%    (thanx to Rich Winkel from UMC for pointing out this one).
%   Cleaned up the code for truncating when combining Taylor kernels
%    of different order.
%   Introduced taysimpasin for computing the asin, acos, etc.
%    of a Taylor kernel.
%   Changed some internal procedures to call addtaylor1, etc.
%    instead of addtaylor, etc. if both arguments have the same
%    template.
%   Changed representation of the coefflist: expansion with respect
%    to one variable is a special case of homogeneous expansion.
%    This is now reflected in the internal representation.  These
%    changes make the code shorter since all expansions are
%    done the same way (fewer checks necessary).
%
% 23-Mar-89    0.9k
%   Numerous bug fixes.
%   Changed subsubtaylor to allow error checking in var!-is!-nth.
%   Rewrote taydegree!-lessp to iterate over its arguments rather
%    than call itself recursively.
%   Introduced exceeds!-order instead of taydegree!-lessp
%    (in truncate!-coefflist and multtaylor1).
%   Minor changes in Taylor!*!-sf!-p, taysimpexp, var!-is!-nth,
%    taysimpexpt, and inttaylorwrttayvar.
%   Changed simptaylor!* to apply resimp to all coefficients and
%    to the tayorig part.
%   Changed subsubtaylor to allow substitution of a kernel into a
%    homogeneously expanded expression.
%   Changed difftaylorwrttayvar to allow differentiation of
%    homogeneously expanded expressions.
%   Changed subsubtaylor so that substitution of a kernel is possible
%    (not only a variable).
%   New constructor smacros make!-Taylor!* and TayFlagsCombine replace
%    explicit list building.
%   New procedures: get!-degree and truncate!-coefflist induced
%    changes in addtaylor/multtaylor/quottaylor/invtaylor.
%    This fixes the other problem pointed out by H. Melenk.
%   Split addtaylor/multtaylor the same way as quottaylor/invtaylor.
%   Introduced taylorautocombine switch and interface to simp!*
%    (via mul!* list).
%   Added symbolic; statement in taylor!-intro module; necessary
%    until module/endmodule are fixed to work together with faslout.
%   Changed subsubtaylor to return a conventional prefix form
%    if all Taylor variables are substituted by constants.
%   Changed difftaylorwrttayvar to ensure that the coefflist of
%    the Taylor kernel it returns is not empty.
%   Changed subsubtaylor to avoid 0**n for n<=0 when substituting
%    a Taylor variable (to signal an error instead);  changed
%    taylor!-error accordingly.
%   Added taylortemplate operator, removed smacro
%    taylor!-map!-over!-coefficients.
%   Added code for expansion about infinity.
%   Split quottaylor into two parts: the driver (quottaylor) and
%    the routine doing the work (quottaylor1).  Same for invtaylor.
%   Rewrote the expansion procedures taylor1, taylor2,...
%   Change in taylor2e: added flg parameter, introduced
%    delete!-superfluous!-coeffs.
%   Added set!-tayorig and multintocoefflist.
%   Replaced simp by simp!* for simplication of tayorig part in
%    taysimplog and taysimpexpt.
%   Removed taysimpsq in taylorseriesp: it now returns t iff its
%    argument is a Taylor kernel disguised as a standard quotient.
%   Added taylororiginal operator.
%   Added a number of tests if tayorig of a Taylor kernel is non-nil
%    if !*taylorkeeporiginal is set.
%   Replaced calls to simpdiff in taylor2e and simpexpt by a call
%    to simp.
%   Minor change in taylor!*print!*1.
%   H. Melenk discovered that the code did not behave as documented:
%    addition of Taylor kernels differing only in their order did not
%    work, and Taylor expansion of a Taylor kernel w.r.t. one of its
%    variables would fail.
%    Corrected the latter problem by changing the substitution code
%    to allow a Taylor variable to be replaced by a constant.
%   taylorseriesp is now a boolean operator and therefore only
%    usable in if statements.
%   Replaced calls to subsq1/subf1 by subsq/subf,
%    definitions of subsq1 and taymaxdegree deleted.
%   Minor changes in taylor2hom and taylor2e.
%
% 28-Nov-88    0.9j
%   Changed printing of `. . . ' to `...'.
%   Changed simptrig to simp in taysimpatan.
%   Changed simptaylor to simplify all its arguments, not only
%    the first one.
%   Added !*verboseload switch to conditionalize printing of
%    loading info.
%   Changed taylor2 to call taylor!-error instead of rederr.
%    Modified taylor!-error accordingly.
%
% 16-Nov-88    0.9i
%   Fixed a Typo in quottaylor.
%   Inserted module/endmodule declarations.
%   Added errorset in taylor2 to catch zero denominators, etc.,
%    caused by expansion about essential singularities.
%
% 10-Nov-88    0.9h
%   Fixed bugs that caused taking car of an atom (found by A.C.Hearn).
%    taysimpt used multf instead of multpf.
%    I also discovered a car/cdr of nil in
%    makecoeffpairs1/makecoeffpairshom1.
%    Reason: (nil . nil) == (nil), but what I want is
%    ((nil . nil)) == ((nil)).  Stupid, eh?
%
% 23-Jul-88    0.9g
%   Added dmode!* to list of fluid variables,
%    removed taylor!-map!-over!-coefficients!-and!-orig.
%
% 26-May-88    0.9f
%   Minor bug fix in taydegree!-lessp.
%
% 25-May-88    0.9e
%   Fixed a number of smaller bugs.
%   Finally implemented multiplication and division for
%    homogeneously expanded Taylor series.
%   Today I realized that the procedure diffp in ALG2 had
%    been changed for REDUCE 3.3.
%
% 21-May-88    0.9d
%   Fixed bug in invtaylor.
%   Rewrote quottaylor to do direct division rather use invtaylor.
%
% 14-May-88    0.9c
%   Fixed substitution for expansion variable.
%
% 11-May-88    0.9b
%   Fixed user interface functions taylorseriesp and taylortostandard.
%
% 10-May-88    0.9a
%   Small changes in subsubtaylor and difftaylor to make the code
%    compilable, plus minor bug fixes.
%
% 08-May-88    0.9
%    invtaylor changed to allow inversion of multidimensional
%     Taylor kernels (but still not for homogeneous expansion).
%
%
% 06-May-88    0.8i
%   `conc' changed to `join' (for mnemonic purposes).
%
% 29-Apr-88    0.8h
%   Minor bug fix in invtaylor (missing quote).
%
% 21-Mar-88    0.8g
%   Minor change in TayDegreeSum.
%
% 17-Jan-88    0.8f
%   Started implementation of homogeneous expansion
%    (required change in conversion to prefix form).
%
% 16-Jan-88    0.8e
%   Minor change in the definition of confusion.
%
% 15-Jan-88    0.8d
%   Changed to conform to REDUCE 3.3
%    (SWITCH statement, minor changes in parsing).
%
% 03-Jan-88    0.8c
%   First version that is supposed to return always a correct result
%    (but not all possible cases are handled).
%
%
%*****************************************************************
%
%       Things to do:
%
%*****************************************************************
%
%     a) Finish implementation of homogeneous expansion (hard).
%     b) Find a method to handle singularities (very hard).
%     c) Perhaps I should change the definition of ORDP to order
%         Taylor kernels in some special way?
%     d) An interface for the PART operator is desirable.  Currently
%        Taylor kernels are converted into prefix form.
%        Alas, with REDUCE 3.x this requires redefinition of some
%         functions. (More hooks, please!)
%        Same applies to COEFF and COEFFN.
%     e) Rewrite the expansion code to recursively descend a standard
%         form.  This allows recognition of certain special functions,
%         e.g., roots and logarithms.  (Much work, requires rewriting
%         of a large part of the code.)
%     f) With e) it is easy to implement a DEFTAYLOR operator so that
%         the user may define the Taylor expansion of an unknown
%         function.
%     g) This would also allow the use of Taylor for power series
%         solutions of ODEs.
%     h) Implement a sort of lazy evaluation scheme, similar to the
%         one used in the TPS package written by Alan Barnes and
%         Julian Padget.  This would allow the calculation of more
%         terms of a series when they are needed.
%     i) Replace all non-id kernels that are independent of the Taylor
%         variables by gensyms.  This would reduce the size of the
%         expressions.
%
%

create!-package('(taylor tayintro tayutils tayinterf tayexpnd taybasic
                  taysimp      taysubst  taydiff  tayconv     tayprint
                  tayfrontend tayfns tayrevert), '(contrib taylor));


%*****************************************************************
%
%       Non-local variables used in this package
%
%*****************************************************************

fluid  '(taylor!:version        % version number
         taylor!:date!*         % release date
         taylorprintterms       % Number of terms to be printed
         !*tayexpanding!*
         !*tayrestart!*
         !*taylorkeeporiginal   % \
         !*taylorautoexpand     %  \
         !*taylorautocombine    %   >  see below
         !*taylorprintorder     %  /
         !*trtaylor             % /
         convert!-taylor!*
         !*sub2
         !*verboseload);

share taylorprintterms;

Comment This package has six switches:
        `TAYLORKEEPORIGINAL' causes the expression for which the
        expansion is performed to be kept.
        `TAYLORAUTOEXPAND' makes Taylor expressions ``contagious''
        in the sense that all other terms are automatically
        Taylor expanded and combined.
        `TAYLORAUTOCOMBINE' causes taysimpsq to be applied to all
        expressions containing Taylor kernels.  This is equivalent
        to applying `TAYLORCOMBINE' to all those expressions.
        If `TAYLORPRINTORDER' is set to ON Taylor kernels are
        printed in big-O notation instead of just printing three dots.
        `TRTAYLOR', if on, prints some information about the expansion
        process.
        `VERBOSELOAD' is a variable used by Portable Standard Lisp
        and causes a loading info to be printed;

switch taylorautocombine,
       taylorautoexpand,
       taylorkeeporiginal,
       taylorprintorder,
       trtaylor,
       verboseload;

convert!-taylor!* := nil;      % flag indicating that Taylor kernels
                               % should be converted to prefix forms
taylorprintterms := 5;         % Only this nubmer of non-zero terms 
                               % will normally be printed.
!*taylorkeeporiginal := nil;   % used to indicate if the original
                               % expressions (before the expansion)
                               % are to be kept.
!*taylorautoexpand := nil;     % set if non-taylor expressions are to
                               % be expanded automatically on
                               % combination.
!*taylorautocombine := t;      % set if taysimpsq should be added to
                               % the MUL!* list.
!*taylorprintorder := t;       % set if Taylor kernels should be printed
                               % with big-O notation, now on by default.
%!*verboseload := nil;         % set if loading info should be printed
!*tayexpanding!* := nil;       % set by taylorexpand to indicate that
                               % expansion is in progress.
!*tayrestart!* := nil;         % set by Taylor!-error!* if expansion is
                               % in progress to indicate that the error
                               % might disappear if the order is
                               % increased.
taylor!:version := "1.8c";     % version number of the package
taylor!:date!* := "23-Sep-93"; % release date

if !*verboseload then
  << terpri ();
     prin2 "TAYLOR PACKAGE, version ";
     prin2 taylor!:version;
     prin2 ", as of ";
     prin2 taylor!:date!*;
     prin2t " for REDUCE 3.5 being loaded...";
     terpri () >> ;



exports !*tay2f, !*tay2q, !*tayexp2q, copy!-list, cst!-taylor!*,
        get!-degree, get!-degreelist, has!-taylor!*, has!-tayvars,
        make!-cst!-coefficient, make!-cst!-coefflis,
        make!-cst!-powerlist, make!-taylor!*, multintocoefflist,
        nzerolist, resimpcoefflist, resimptaylor, set!-taycfpl,
        set!-taycfsq, set!-taycoefflist, set!-tayflags, set!-tayorig,
        set!-taytemplate, subs2coefflist, taycfpl, taycfsq,
        taycoefflist, taydegreesum, tayexp!-difference,
        tayexp!-greaterp, tayexp!-lessp, tayexp!-min2, tayexp!-minus,
        tayexp!-minusp, tayexp!-plus, tayexp!-plus2, tayexp!-times,
        tayexp!-times2, tayflags, tayflagscombine, taygetcoeff,
        taylor!*p, taylor!-kernel!-sf!-p, taylor!-kernel!-sq!-p,
        taylor!:, taymakecoeff, taymincoeff, taymultcoeffs, tayorig,
        taytemplate, taytpelnext, taytpelorder, taytpelpoint,
        taytpelvars, tayvars, tpdegreelist;

imports

% from REDUCE kernel:
        !*f2q, !*n2f, !*p2f, !*p2q, domainp, eqcar, kernp, lastpair,
        lc, ldeg, lpriw, mathprint, mksp, multsq, mvar, nlist, numr,
        over, prin2t, red, resimp, smember, subs2,

% from module Tayintro:
        smemberlp,

% from module Tayutils:
        add!-degrees;

%*****************************************************************
%
%      General utility smacros
%
%*****************************************************************


symbolic smacro procedure nzerolist n;
  %
  % generates a list of n zeros
  %
  nlist (0, n);

symbolic smacro procedure copy!-list l;
  %
  % produces a copy of list l.
  %
  append (l, nil);



%*****************************************************************
%
%      Selector and constructor smacros for Taylor kernels
%
%*****************************************************************


symbolic smacro procedure make!-taylor!* (cflis, tp, orig, flgs);
  %
  % Builds a new Taylor kernel structure out of its parts.
  %
  {'taylor!*, cflis, tp, orig, flgs};

symbolic smacro procedure taymakecoeff (u, v);
  %
  % Builds a coefficient from degreelist and s.q.
  %
  u . v;


Comment Selector smacros for the parts of a Taylor kernel;

symbolic smacro procedure taycoefflist u; cadr u;

symbolic smacro procedure taytemplate u; caddr u;

symbolic smacro procedure tayorig u; cadddr u;

symbolic smacro procedure tayflags u; car cddddr u;

symbolic smacro procedure taycfpl u; car u;

symbolic smacro procedure taycfsq u; cdr u;

symbolic smacro procedure taytpvars tp;
  for each x in tp join copy!-list car x;

symbolic smacro procedure tayvars u;
  taytpvars taytemplate u;

symbolic smacro procedure taygetcoeff (degrlis, coefflis);
  (if null cc then nil ./ 1 else taycfsq cc)
    where cc := assoc (degrlis, coefflis);

symbolic smacro procedure taytpelvars u; car u;

symbolic smacro procedure taytpelpoint u; cadr u;

symbolic smacro procedure taytpelorder u; caddr u;

symbolic smacro procedure taytpelnext u; cadddr u;

symbolic smacro procedure tpdegreelist tp;
  for each x in tp collect taytpelorder x;

%symbolic smacro procedure TayDegreeList u;
%  TpDegreeList TayTemplate u;

symbolic smacro procedure taydegreesum u;
  for each x in taytemplate u sum taytpelorder x;


Comment Modification smacros;

symbolic smacro procedure set!-taycoefflist (u, v);
  %
  % Sets TayCoeffList part of Taylor kernel u to v
  %
  rplaca (cdr u, v);

symbolic smacro procedure set!-taytemplate (u, v);
  %
  % Sets TayTemplate part of Taylor kernel u to v
  %
  rplaca (cddr u, v);

symbolic smacro procedure set!-tayorig (u, v);
  %
  % Sets TayOrig part of Taylor kernel u to v
  %
  rplaca (cdddr u, v);

symbolic smacro procedure set!-tayflags (u, v);
  %
  % Sets TayFlags part of Taylor kernel u to v
  %
  rplaca (cddddr u, v);

symbolic smacro procedure set!-taycfpl (u, v);
  rplaca (u, v);

symbolic smacro procedure set!-taycfsq (u, v);
  rplacd (u, v);


Comment Smacro that implement arithmetic operations on
        exponents in powerlist;

symbolic smacro procedure !*tayexp2f u; !*n2f u;

symbolic procedure !*tayexp2q u; !*f2q !*tayexp2f u;

symbolic macro procedure tayexp!-plus x;
   if null cdr x then 0
    else if null cddr x then cadr x
    else expand(cdr x,'tayexp!-plus2);

symbolic procedure tayexp!-plus2(u,v); u+v;

symbolic procedure tayexp!-difference(u,v); u-v;

symbolic procedure tayexp!-minus u; -u;

symbolic macro procedure tayexp!-times x;
   if null cdr x then 1
    else if null cddr x then cadr x
    else expand(cdr x,'tayexp!-times2);

symbolic procedure tayexp!-times2(u,v); u*v;

symbolic procedure tayexp!-minusp u; minusp u;

symbolic procedure tayexp!-greaterp(u,v); u>v;

symbolic procedure tayexp!-lessp(u,v); u<v;

symbolic procedure tayexp!-min2(u,v); min2(u,v);

symbolic macro procedure taylor!: u;
   sublis('((plus . tayexp!-plus)
            (plus2 . tayexp!-plus2)
            (difference . tayexp!-difference)
            (minus . tayexp!-minus)
            (times . tayexp!-times)
            (times2 . tayexp!-times2)
            (minusp . tayexp!-minusp)
            (greaterp . tayexp!-greaterp)
            (lessp . tayexp!-lessp)
            (min2 . tayexp!-min2)),
          cadr u);



Comment Smacros and procedures that are commonly used ;

symbolic smacro procedure tayflagscombine (u, v);
  %
  % Not much for now
  %
  nil;

symbolic smacro procedure get!-degree dg;
  %
  % Procedure to handle degree parts of Taylor kernels.
  %
  taylor!: for each n in dg sum n;

symbolic smacro procedure get!-degreelist dgl;
   for each dg in dgl collect get!-degree dg;

symbolic smacro procedure taymultcoeffs (c1, c2);
  %
  % (TayCoeff, TayCoeff) -> TayCoeff
  %
  % multiplies the two coefficients c1,c2.
  % both are of the form (TayPowerList . s.q.)
  % so generate an appropriate degreelist by adding the degrees.
  %
  taymakecoeff (add!-degrees (taycfpl c1, taycfpl c2),
                multsq (taycfsq c1, taycfsq c2));

symbolic smacro procedure multintocoefflist(coefflis,sq);
  %
  % (TayCoeffList, s.q.) -> TayCoeffList
  %
  % Multiplies each coefficient in coefflis by the s.q. sq.
  %
  for each p in coefflis collect
    taymakecoeff(taycfpl p,
                 (resimp subs2 multsq(taycfsq p,sq)
                   where !*sub2 := !*sub2));

symbolic smacro procedure subs2coefflist clist;
  for each pp in clist join
    ((if not null numr sq then {taymakecoeff(taycfpl pp,sq)})
     where sq := (subs2 taycfsq pp where !*sub2 := !*sub2));

symbolic smacro procedure resimpcoefflist clist;
  for each cc in clist collect
    taymakecoeff(taycfpl cc,subs2 resimp taycfsq cc);

symbolic smacro procedure resimptaylor u;
  %
  % (TaylorKernel) -> TaylorKernel
  %
  % u is a Taylor kernel, value is the Taylor kernel
  % with coefficients and TayOrig part resimplified
  %
  make!-taylor!* (
         resimpcoefflist taycoefflist u,
         taytemplate u,
         if !*taylorkeeporiginal and tayorig u
           then resimp tayorig u else nil,
         tayflags u);

symbolic smacro procedure make!-cst!-powerlist tp;
  %
  % (TayTemplate) -> TayPowerList
  %
  % Generates a powerlist for the constant coefficient
  % according to template tp
  %
  for each el in tp collect nzerolist length taytpelvars el;

symbolic smacro procedure make!-cst!-coefficient (cst, tp);
  %
  % (s.q., TayTemplate) -> TayCoefficient
  %
  % Generates the constant coefficient cst
  % according to Taylor template tp
  %
  taymakecoeff (make!-cst!-powerlist tp, cst);

symbolic smacro procedure make!-cst!-coefflis (cst, tp);
  %
  % (s.q., TayTemplate) -> TayCoeffList
  %
  % Generates a TayCoeffList with only the constant coefficient cst
  % according to Taylor template tp
  %
  {make!-cst!-coefficient (cst, tp)};

symbolic smacro procedure cst!-taylor!* (cst, tp);
  %
  % (s.q., TayTemplate) -> TaylorKernel
  %
  % generates a Taylor kernel with template tp for the constant cst.
  %
  make!-taylor!* (
     make!-cst!-coefflis (cst, tp), tp, cst, nil);


Comment Predicates;

symbolic smacro procedure has!-taylor!* u;
  %
  % (Any) -> Boolean
  %
  % checks if an expression u contains a Taylor kernel
  %
  smember ('taylor!*, u);

symbolic smacro procedure taylor!*p u;
  %
  % (Kernel) -> Boolean
  %
  % checks if kernel u is a Taylor kernel
  %
  eqcar (u, 'taylor!*);

symbolic smacro procedure taylor!-kernel!-sf!-p u;
  %
  % (s.f.) -> Boolean
  %
  % checks if s.f. u is a Taylor kernel
  %
  not domainp u and null red u and lc u = 1
     and ldeg u = 1 and taylor!*p mvar u;

symbolic smacro procedure taylor!-kernel!-sq!-p u;
  %
  % u is a standard quotient,
  % returns t if it is simply a Taylor kernel
  %
  kernp u and taylor!*p mvar numr u;

symbolic smacro procedure has!-tayvars(tay,ex);
   %
   % Checks whether ex contains any of the Taylor variables
   %  of Taylor kernel tay.
   %
   smemberlp(tayvars tay,ex);

symbolic procedure taylor!*!-zerop tay;
   taycoefflist!-zerop taycoefflist tay;

symbolic procedure taycoefflist!-zerop tcl;
   null tcl
     or null numr taycfsq car tcl and taycoefflist!-zerop cdr tcl;


Comment smacros for the generation of unique Taylor kernels;

symbolic smacro procedure !*tay2f u;
  !*p2f mksp (u, 1);

symbolic smacro procedure !*tay2q u;
  !*p2q mksp (u, 1);


Comment some procedures for tracing;

symbolic smacro procedure taylor!-trace u;
   if !*trtaylor then lpriw ("Taylor: ",u);

symbolic smacro procedure taylor!-trace!-mprint u;
   if !*trtaylor then mathprint u;

endmodule;


module tayintro;

%*****************************************************************
%
%          General utility functions
%
%*****************************************************************


exports
        confusion, constant!-sq!-p, delete!-nth, delete!-nth!-nth,
        replace!-nth, replace!-nth!-nth, smemberlp, taylor!-error,
        var!-is!-nth;

imports

% from REDUCE kernel
        constant_exprp, denr, domainp, error1, kernp, mvar, neq, numr,
        prepsq, prin2t, rerror,

% from the header module
        taytpelvars;

fluid '(!*tayexpanding!* !*tayrestart!* taylor!:date!* taylor!:version);

symbolic procedure var!-is!-nth(tp,var);
  %
  % Determines in which part of template tp the kernel var occurs.
  % Returns a pair (n . m) of positive integers which means
  %  that var is the mth subkernel in nth element of template tp
  % This would look a lot better if the loop statements allowed
  %  the use of the return statement.
  %
  begin scalar el,found; integer n,m;
    repeat <<
      n := n + 1;
      el := taytpelvars car tp;
      m := 1;
      while el do <<
        if var neq car el then <<el := cdr el; m := m + 1>>
         else <<el := nil; found := t>>>>;
      tp := cdr tp>>
    until null tp or found;
    if not found then confusion 'var!-is!-nth
     else return (n . m)
  end;

symbolic procedure delete!-nth (l, n);
  %
  % builds a new list with nth element of list l removed
  %
  if n = 1 then cdr l else car l . delete!-nth (cdr l, n - 1);

symbolic procedure delete!-nth!-nth (l, n, m);
  %
  % builds a new list with mth element of nth sublist of list l
  % removed
  %
  if n = 1 then delete!-nth (car l, m) . cdr l
   else car l . delete!-nth!-nth (cdr l, n - 1, m);

symbolic procedure replace!-nth (l, n, v);
  %
  % builds a new list with the nth element of list l replaced by v
  %
  if n = 1 then v . cdr l else car l . replace!-nth (cdr l, n - 1, v);

symbolic procedure replace!-nth!-nth (l, n, m, v);
  %
  % builds a new list with the mth element of nth sublist of list l
  % replaced by v
  %
  if n = 1 then replace!-nth (car l, m, v) . cdr l
   else car l . replace!-nth!-nth (cdr l, n - 1, m, v);

symbolic procedure constant!-sq!-p u;
  %
  % returns t if s.q. u represents a constant
  %
  numberp denr u and domainp numr u
      or kernp u and atom mvar u and flagp (mvar u, 'constant)
      or constant_exprp prepsq u;

symbolic procedure smemberlp (u, v);
  %
  % true if any member of list u is contained at any level in v
  %
  if null v then nil
   else if atom v then v member u
   else smemberlp (u, car v) or smemberlp (u, cdr v);

symbolic procedure confusion msg;
  %
  % called if an internal error occurs.
  % (I borrowed the name from Prof. Donald E. Knuth's TeX program)
  %
  << terpri ();
     prin2 "TAYLOR PACKAGE (version ";
     prin2 taylor!:version;
     prin2 ", as of ";
     prin2 taylor!:date!*;
     prin2t "):";
     prin2 "This can't happen (";
     prin2 msg;
     prin2t ") !";
     rerror (taylor, 1,
             "Please send input and output to Rainer M. Schoepf!") >>;

symbolic procedure taylor!-error (type, info);
  %
  % called if a normal error occurs.
  % type is the type of error, info the error info.
  %
  begin scalar msg; integer errno;
    msg := if type eq 'not!-a!-unit then "Not a unit in argument to"
            else if type eq 'wrong!-no!-args
             then "Wrong number of arguments to"
            else if type eq 'expansion
             then "Error during expansion"
            else if type eq 'wrong!-type!-arg
             then "Wrong argument type"
            else if type eq 'no!-original
             then "Taylor kernel doesn't have an original part in"
            else if type eq 'zero!-denom
             then "Zero divisor in"
            else if type eq 'essential!-singularity
             then "Essential singularity in"
            else if type eq 'branch!-point
             then "Branch point detected in"
            else if type eq 'branch!-cut
             then "Expansion point lies on branch cut in"
            else if type eq 'inttaylorwrttayvar
             then
              "Integration of Taylor kernel yields non-analytical term"
            else if type eq 'dependent!-subst
             then "Substitution of dependent variables"
            else if type eq 'tayrevert
             then "Reversion of Taylor series not possible:"
            else if type eq 'not!-implemented
             then "Not implemented yet"
            else confusion 'taylor!-error;
%    rerror (taylor, errno,
    rerror (taylor, 2,
            if null info then msg
             else if atom info then {msg, info}
             else msg . info);
  end;

symbolic procedure taylor!-error!*(type,info);
   %
   % Like Taylor!-error, but calls sets !*tayrestart!* and calls
   %  error1 if !*tayexpanding!* indicates that expansion is going
   %  on and more terms might be necessary.
   %
   if !*tayexpanding!* then <<!*tayrestart!* := t; error1()>>
    else taylor!-error(type,info);

endmodule;


module tayutils;

%*****************************************************************
%
%       Utility functions that operate on Taylor kernels
%
%*****************************************************************


exports add!-degrees, add!.comp!.tp!., check!-for!-cst!-taylor,
        comp!.tp!.!-p, delete!-superfluous!-coeffs, enter!-sorted,
        exceeds!-order, find!-non!-zero, get!-cst!-coeff, inv!.tp!.,
        is!-neg!-pl, mult!.comp!.tp!., subtr!-degrees,
        subtr!-tp!-order, taydegree!-lessp,
        taydegree!-strict!-less!-or!-equal!-p, taymincoeff,
        tayminpowerlist, taylor!*!-constantp, taylor!*!-nzconstantp,
        taylor!*!-onep, taylor!*!-zerop, taytpmin2, tp!-greaterp,
        truncate!-coefflist, truncate!-taylor!*;

imports

% from the REDUCE kernel:
        ./, lastpair, neq, nth, numr, reversip,

% from the header module:
        !*tay2q, get!-degree, get!-degreelist, make!-cst!-powerlist,
        nzerolist, taycfpl, taycfsq, taycoefflist, taygetcoeff,
        taylor!:, taytemplate, taytpelnext, taytpelorder,
        taytpelpoint, taytpelvars,

% from module Tayintro:
        confusion,

% from module TayUtils:
        taycoefflist!-zerop;



symbolic procedure add!-degrees (dl1, dl2);
  %
  % calculates the element-wise sum of two degree lists dl1, dl2.
  %
  taylor!:
    begin scalar dl,u,v,w;
      while dl1 do <<
        u := car dl1;
        v := car dl2;
        w := nil;
        while u do <<
          w := (car u + car v) . w;
          u := cdr u;
          v := cdr v>>;
        dl := reversip w . dl;
        dl1 := cdr dl1;
        dl2 := cdr dl2>>;
      return reversip dl
    end;

symbolic procedure subtr!-degrees(dl1,dl2);
  %
  % calculates the element-wisedifference of two degree lists dl1, dl2.
  %
  taylor!:
    begin scalar dl,u,v,w;
      while dl1 do <<
        u := car dl1;
        v := car dl2;
        w := nil;
        while u do <<
          w := (car u - car v) . w;
          u := cdr u;
          v := cdr v>>;
      dl := reversip w . dl;
      dl1 := cdr dl1;
      dl2 := cdr dl2>>;
    return reversip dl
  end;

symbolic procedure find!-non!-zero pl;
  %
  % pl is a power list.  Returns pair (n . m) of position of first
  % non zero degree.
  %
  begin scalar u; integer n, m;
    n := 1;
   loop:
    m := 1;
    u := car pl;
   loop2:
    if not (car u = 0) then return (n . m);
    u := cdr u;
    m := m + 1;
    if not null u then goto loop2;
    pl := cdr pl;
    n := n + 1;
    if null pl then confusion 'find!-non!-zero;
    goto loop
  end$

symbolic procedure comp!.tp!.!-p (u, v);
  %
  % Checks templates of Taylor kernels u and v for compatibility,
  % i.e. whether variables and expansion points match.
  % Returns t if possible.
  %
  begin;
    u := taytemplate u; v := taytemplate v;
    if length u neq length v then return nil;
   loop:
    if not (taytpelvars car u = taytpelvars car v
            and taytpelpoint car u = taytpelpoint car v)
      then return nil;
    u := cdr u; v := cdr v;
    if null u then return t;
    goto loop
  end$

symbolic procedure add!.comp!.tp!.(u,v);
  %
  % Checks templates of Taylor kernels u and v for compatibility
  % when adding them, i.e. whether variables and expansion points
  % match.
  % Returns either new Taylor template whose degrees are the minimum
  % of the corresponding degrees of u and v, or nil if variables or
  % expansion point(s) do not match.
  %
  taylor!:
  begin scalar w;
    u := taytemplate u; v := taytemplate v;
    if length u neq length v then return nil;
   loop:
    if not (taytpelvars car u = taytpelvars car v
            and taytpelpoint car u = taytpelpoint car v)
      then return nil
     else w := {taytpelvars car u,
                taytpelpoint car u,
                min2(taytpelorder car u,taytpelorder car v),
                min2(taytpelnext car u,taytpelnext car v)}
                . w;
    u := cdr u; v := cdr v;
    if null u then return reversip w;
    goto loop
  end;

symbolic procedure taymindegreel(pl,dl);
   taylor!:
     if null pl then nil
      else min2(get!-degree car pl,car dl)
             . taymindegreel(cdr pl,cdr dl);

symbolic procedure get!-min!-degreelist cfl;
  taylor!:
    if null cfl then confusion 'get!-min!-degreelist
     else if null cdr cfl then get!-degreelist taycfpl car cfl
     else taymindegreel(taycfpl car cfl,
                        get!-min!-degreelist cdr cfl);

symbolic procedure mult!.comp!.tp!.(u,v,div!?);
  %
  % Checks templates of Taylor kernels u and v for compatibility
  % when multiplying or dividing them, i.e., whether variables and
  % expansion points match.  The difference to addition is that
  % in addition to the new template it returns two degreelists
  % to be used by truncate!-coefflist which
  % are made up so that the kernels have the same number of terms.
  %
  taylor!:
  begin scalar w,w1,w2,cf1,cf2,
        !#terms!-1,!#terms!-next,dl1,dl2,mindg;
    cf1 := taycoefflist u;
    while cf1 and null numr taycfsq car cf1 do cf1 := cdr cf1;
    if null cf1 then dl1 := nzerolist length taytemplate u
     else dl1 := get!-min!-degreelist cf1;
    cf2 := taycoefflist v;
    while cf2 and null numr taycfsq car cf2 do cf2 := cdr cf2;
    if null cf2 then dl2 := nzerolist length taytemplate v
     else dl2 := get!-min!-degreelist cf2;
    u := taytemplate u; v := taytemplate v;
    if length u neq length v then return nil;
   loop:
    if not (taytpelvars car u = taytpelvars car v
            and taytpelpoint car u = taytpelpoint car v)
      then return nil;
    mindg := if div!? then car dl1 - car dl2 else car dl1 + car dl2;
    !#terms!-1 := min2(taytpelorder car u - car dl1,
                       taytpelorder car v - car dl2);
    !#terms!-next := min2(taytpelnext car u - car dl1,
                          taytpelnext car v - car dl2);
    w1 :=  (!#terms!-1 + car dl1) . w1;
    w2 :=  (!#terms!-1 + car dl2) . w2;
    w := {taytpelvars car u,taytpelpoint car u,
          mindg + !#terms!-1,mindg + !#terms!-next}
          . w;
    u := cdr u; v := cdr v; dl1 := cdr dl1; dl2 := cdr dl2;
    if null u then return {reversip w,reversip w1,reversip w2};
    goto loop
  end;

symbolic procedure inv!.tp!. u;
  %
  % Checks template of Taylor kernel u for inversion. It returns a
  % degreelist to be used by truncate!-coefflist which is made up
  % are made up so that the resulting kernel has the correct number
  % of terms.
  %
  taylor!:
  begin scalar w,w2,cf,!#terms!-1,!#terms!-next,dl,mindg;
    cf := taycoefflist u;
    while cf and null numr taycfsq car cf do cf := cdr cf;
    if null cf then dl := nzerolist length taytemplate u
     else dl := get!-degreelist taycfpl car cf;
    u := taytemplate u;
   loop:
    mindg := - car dl;
    !#terms!-1 := taytpelorder car u - car dl;
    !#terms!-next := taytpelnext car u - car dl;
    w2 := (!#terms!-1 + car dl) . w2;
    w := {taytpelvars car u,taytpelpoint car u,mindg + !#terms!-1,
          mindg + !#terms!-next}
          . w;
    u := cdr u; dl := cdr dl;
    if null u then return {reversip w, reversip w2};
    goto loop
  end;

symbolic smacro procedure taycoeff!-before (cc1, cc2);
  %
  % (TayCoeff, TayCoeff) -> Boolean
  %
  % returns t if coeff cc1 is ordered before cc2
  % both are of the form (degreelist . sq)
  %
  taydegree!-lessp (taycfpl cc1, taycfpl cc2)$

symbolic procedure taydegree!-lessp(u,v);
  %
  % (TayPowerList, TayPowerList) -> Boolean
  %
  % returns t if coefflist u is ordered before v
  %
  taylor!:
  begin scalar u1,v1;
   loop:
    u1 := car u;
    v1 := car v;
   loop2:
     if car u1 > car v1 then return nil
      else if car u1 < car v1 then return t;
     u1 := cdr u1;
     v1 := cdr v1;
     if not null u1 then go to loop2;
    u := cdr u;
    v := cdr v;
    if not null u then go to loop
  end;

symbolic procedure taydegree!-strict!-less!-or!-equal!-p(u,v);
  %
  % (TayPowerList, TayPowerList) -> Boolean
  %
  % returns t if every component coefflist u is less or equal than
  %  respective component of v
  %
  taylor!:
  begin scalar u1,v1;
   loop:
    u1 := car u;
    v1 := car v;
   loop2:
     if car u1 > car v1 then return nil;
     u1 := cdr u1;
     v1 := cdr v1;
     if not null u1 then go to loop2;
    u := cdr u;
    v := cdr v;
    if not null u then go to loop;
    return t
  end;

symbolic procedure exceeds!-order(ordlis,cf);
  %
  % (List of Integers, TayPowerlist) -> Boolean
  %
  % Returns t if the degrees in coefficient cf exceed those
  % in the degreelist ordlis
  %
  if null ordlis then nil
   else taylor!:(get!-degree car cf > car ordlis)
          or exceeds!-order(cdr ordlis,cdr cf);

symbolic procedure enter!-sorted (u, alist);
  %
  % (TayCoeff, TayCoeffList) -> TayCoeffList
  %
  % enters u into the alist alist according to the standard
  % ordering for the car part
  %
  if null alist then {u}
   else if taycoeff!-before (u, car alist) then u . alist
   else car alist . enter!-sorted (u, cdr alist)$

symbolic procedure delete!-superfluous!-coeffs(cflis,pos,n);
  %
  % (TayCoeffList, Integer, Integer) -> TayCoeffList
  %
  % This procedure deletes all coefficients of a TayCoeffList cflis
  % whose degree in position pos exceeds n.
  %
  taylor!:
  for each cc in cflis join
     (if get!-degree nth(taycfpl cc,pos) > n then nil else {cc});

symbolic procedure truncate!-coefflist (cflis, dl);
  %
  % (TayCoeffList, List of Integers) -> TayCoeffList
  %
  % Deletes all coeffs from coefflist cflis that have at least
  %  one degree greater than the corresponding degree in the
  %  degreelist dl.
  %
  begin scalar l;
    for each cf in cflis do
      if not exceeds!-order (dl, taycfpl cf) then l := cf . l;
    return reversip l
  end;

symbolic procedure taytp!-min2(tp1,tp2);
  %
  % finds minimum (w.r.t. Order and Next parts) of compatible
  %  templates tp1 and tp2
  %
  taylor!:
    if null tp1 then nil
     else if not (taytpelvars car tp1 = taytpelvars car tp2 and
                  taytpelpoint car tp1 = taytpelpoint car tp2)
      then confusion 'taytpmin2
     else {taytpelvars car tp1,taytpelpoint car tp2,
           min2(taytpelorder car tp1,taytpelorder car tp2),
           min2(taytpelnext car tp1,taytpelnext car tp2)}
          . taytp!-min2(cdr tp1,cdr tp2);


symbolic procedure truncate!-taylor!*(tay,tp);
   make!-taylor!*(truncate!-coefflist(taycoefflist tay,tpdegreelist tp),
                  taytp!-min2(taytemplate tay,tp),nil,nil);


symbolic procedure tp!-greaterp(tp1,tp2);
   %
   % Given two templates tp1 and tp2 with matching variables and
   %  expansion points this function returns t if the expansion
   %  order wrt at least one variable is greater in tp1 than in tp2.
   %
   if null tp1 then nil
    else taylor!: (taytpelorder car tp1 > taytpelorder car tp2)
           or tp!-greaterp(cdr tp1,cdr tp2);

symbolic procedure subtr!-tp!-order(tp1,tp2);
   %
   % Given two templates tp1 and tp2 with matching variables and
   %  expansion points this function returns the difference in their
   %  orders.
   %
   taylor!:
    if null tp1 then nil
     else (taytpelorder car tp1 - taytpelorder car tp2)
            . subtr!-tp!-order(cdr tp1,cdr tp2);


Comment Procedures to non-destructively modify Taylor templates;

symbolic procedure addto!-all!-taytpelorders(tp,nl);
  taylor!:
   if null tp then nil
    else {taytpelvars car tp,
          taytpelpoint car tp,
          taytpelorder car tp + car nl,
          taytpelnext car tp + car nl} .
         addto!-all!-taytpelorders(cdr tp,cdr nl);

symbolic procedure taymincoeff cflis;
  %
  % Returns degree of first non-zero coefficient
  % or 0 if there isn't any.
  %
  if null cflis then 0
   else if not null numr taycfsq car cflis
    then get!-degree car taycfpl car cflis
   else taymincoeff cdr cflis;

symbolic procedure tayminpowerlist cflis;
  %
  % Returns degreelist of first non-zero coefficient of TayCoeffList
  % cflis or a list of zeroes if there isn't any.
  %
  if null cflis then confusion 'tayminpowerlist
   else tayminpowerlist1(cflis,length taycfpl car cflis);

symbolic procedure tayminpowerlist1(cflis,l);
   if null cflis then nzerolist l
    else if null numr taycfsq car cflis
     then tayminpowerlist1(cdr cflis,l)
    else get!-degreelist taycfpl car cflis;

symbolic procedure get!-cst!-coeff tay;
   taygetcoeff(make!-cst!-powerlist taytemplate tay,taycoefflist tay);

symbolic procedure taylor!*!-constantp tay;
   taylor!*!-constantp1(make!-cst!-powerlist taytemplate tay,
                        taycoefflist tay);

symbolic procedure taylor!*!-constantp1(pl,tcf);
   if null tcf then t
    else if taycfpl car tcf = pl
     then taycoefflist!-zerop cdr tcf
    else if not null numr taycfsq car tcf then nil
    else taylor!*!-constantp1(pl,cdr tcf);

symbolic procedure check!-for!-cst!-taylor tay;
   begin scalar flg,pl,tc;
      pl := make!-cst!-powerlist taytemplate tay;
      tc := taycoefflist tay;
      return if taylor!*!-constantp1(pl,tc) then taygetcoeff(pl,tc)
              else !*tay2q tay
   end;

symbolic procedure taylor!*!-nzconstantp tay;
   taylor!*!-nzconstantp1(make!-cst!-powerlist taytemplate tay,
                          taycoefflist tay);

symbolic procedure taylor!*!-nzconstantp1(pl,tcf);
   if null tcf then nil
    else if taycfpl car tcf = pl
     then if null numr taycfsq car tcf then nil
           else taycoefflist!-zerop cdr tcf
    else if taycfpl car tcf neq pl and
            not null numr taycfsq car tcf
     then nil
    else taylor!*!-nzconstantp1(pl,cdr tcf);

symbolic procedure taylor!*!-onep tay;
   taylor!-onep1(make!-cst!-powerlist taytemplate tay,taycoefflist tay);

symbolic procedure taylor!-onep1(pl,tcf);
   if null tcf then nil
    else if taycfpl car tcf = pl
     then if taycfsq car tcf = (1 ./ 1)
            then taycoefflist!-zerop cdr tcf
           else nil
    else if null numr taycfsq car tcf
     then taylor!*!-nzconstantp1(pl,cdr tcf)
    else nil;

symbolic procedure is!-neg!-pl pl;
  %
  % Returns t if any of the exponents in pl is negative.
  %
  taylor!:
    if null pl then nil
     else if get!-degree car pl < 0 then t
     else is!-neg!-pl cdr pl;

endmodule;


module tayinterf;

%*****************************************************************
%
%      The interface to the REDUCE simplificator
%
%*****************************************************************


exports simptaylor, simptaylor!*, taylorexpand$

imports

% from the REDUCE kernel:
        !*f2q, aconc!*, denr, depends, diffsq, eqcar, kernp, lastpair,
        leq, lprim, mkquote, mksq, multsq, mvar, neq, nth, numr, over,
        prepsq, revlis, reversip, simp, simp!*, subs2, subsq, typerr,

% from the header module:
        !*tay2q, get!-degree, has!-taylor!*, has!-tayvars,
        make!-taylor!*, multintocoefflist, resimptaylor, set!-tayorig,
        taycfpl, taycfsq, taycoefflist, tayflags, taymakecoeff,
        tayorig, taytemplate, taytpelorder, taytpelpoint, tayvars,
        taylor!-kernel!-sq!-p, taymincoeff,

% from module Tayintro:
        replace!-nth, taylor!-error, var!-is!-nth,

% from module TayExpnd:
        taylorexpand,

% from module Tayutils:
        delete!-superfluous!-coeffs,

% from module taybasic:
        invtaylor1, quottaylor1,

% from module Tayconv:
        preptaylor!*$


fluid '(!*backtrace !*taylorkeeporiginal !*taylorautocombine)$

global '(mul!* subfg!*)$

Comment The following statement forces all expressions to be
        re-simplified if the switch `taylorautocombine' is set to on,
        unfortunately, this is not always sufficient;

put ('taylorautocombine, 'simpfg, '((t (rmsubs))))$


symbolic procedure simptaylor u;
  %
  % (PrefixForm) -> s.q.
  %
  % This procedure is called directly by the simplifier.
  % Its argument list must be of the form
  %     (exp, [var, var0, deg, ...]),
  % where [...] indicate one or more occurences.
  % This means that exp is to be expanded w.r.t var about var0
  % up to degree deg.
  % var may also be a list of variables, which means that the
  % the expansion takes place in a homogeneous way.
  % If var0 is the special atom infinity var is replaced by 1/var
  % and the result expanded about 0.
  %
  % This procedure returns the input if it cannot expand the expression.
  %
  if remainder(length u,3) neq 1
    then taylor!-error('wrong!-no!-args,'taylor)
   else if null subfg!* then mksq('taylor . u,1)
   else begin scalar arglist,degree,f,ll,result,var,var0;
     %
     % Allow automatic combination of Taylor kernels.
     %
     if !*taylorautocombine and not ('taysimpsq memq mul!*)
       then mul!* := aconc!*(mul!*,'taysimpsq);
     %
     % First of all, call the simplifier on exp (i.e. car u),
     %
     f := simp!* car u;
     u := revlis cdr u; % reval instead of simp!* to handle lists
     arglist := u;
     %
     % then scan the rest of the argument list.
     %
     while not null arglist do
       << var := car arglist;
          var := if eqcar(var,'list) then cdr var else {var};
          % In principle one should use !*a2k
          % but typerr (maprin) does not correctly print the atom nil
          for each el in var collect begin
            el := simp!* el;
            if kernp el then return mvar numr el
             else typerr(prepsq el,'kernel)
            end;
          var0 := cadr arglist;
          degree := caddr arglist;
          if not fixp degree
            then typerr(degree,"order of Taylor expansion");
          arglist := cdddr arglist;
          ll := {var,var0,degree,degree + 1} . ll>>;
     %
     % Now ll is a Taylor template, i.e. of the form
     %  ((var_1 var0_1 deg1 next_1) (var_2 var0_2 deg_2 next_2) ...)
     %
     result := taylorexpand(f,reversip ll);
     %
     % If the result does not contain a Taylor kernel, return the input.
     %
     return if has!-taylor!* result then result
             else mksq('taylor . prepsq f . u,1)
   end;

put('taylor,'simpfn,'simptaylor)$


%symbolic procedure taylorexpand (f, ll);
%  %
%  % f is a s.q. that is expanded according to the list ll
%  %  which has the form
%  %  ((var_1 var0_1 deg1) (var_2 var0_2 deg_2) ...)
%  %
%  begin scalar result;
%    result := f;
%    for each el in ll do
%      %
%      % taylor1 is the function that does the real work
%      %
%      result := !*tay2q taylor1 (result, car el, cadr el, caddr el);
%      if !*taylorkeeporiginal then set!-TayOrig (mvar numr result, f);
%      return result
%  end$


symbolic procedure taylor1 (f, varlis, var0, n);
  %
  % Taylor expands s.q. f w.r.t. varlis about var0 up to degree n.
  % var is a list of kernels, which means that the expansion
  % takes place in a homogeneous way if there is more than one
  % kernel.
  % If var0 is the special atom infinity all kernels in varlis are
  % replaced by 1/kernel.  The result is then expanded about 0.
  %
  taylor1sq (if var0 eq 'infinity
               then subsq (f,
                           for each krnl in varlis collect
                             (krnl . list ('quotient, 1, krnl)))
              else f,
             varlis, var0, n)$

symbolic procedure taylor1sq (f, varlis, var0, n);
  %
  % f is a standard quotient, value is a Taylor kernel.
  %
  % First check if it is a Taylor kernel
  %
  if taylor!-kernel!-sq!-p f
    then if has!-tayvars(mvar numr f,varlis)
           %
           % special case: f has already been expanded w.r.t. var.
           %
           then taylorsamevar (mvar numr f, varlis, var0, n)
          else begin scalar y, z;
            f := mvar numr f;
            %
            % taylor2 returns a list of the form
            %  ((deg1 . coeff1) (deg2 . coeff2) ... )
            % apply this to every coefficient in f.
            % car cc is the degree list of this coefficient,
            % cdr cc the coefficient itself.
            % Finally collect the new pairs
            %  (degreelist . coefficient)
            %
            z :=
              for each cc in taycoefflist f join
                for each cc2 in taylor2 (taycfsq cc, varlis, var0, n)
                  collect
                    taymakecoeff (append (taycfpl cc, taycfpl cc2),
                                  taycfsq cc2);
            %
            % Append the new list to the Taylor template and return.
            %
            y := append(taytemplate f,list {varlis,var0,n,n+1});
            return make!-taylor!* (z, y, tayorig f, tayflags f)
            end
   %
   % Last possible case: f is not a Taylor expression.
   % Expand it.
   %
   else make!-taylor!* (
                 taylor2 (f, varlis, var0, n),
                 list {varlis,var0,n,n+1},
                 if !*taylorkeeporiginal then f else nil,
                 nil)$

symbolic procedure taylor2 (f, varlis, var0, n);
  begin scalar result,oldklist;
    oldklist := get('taylor!*,'klist);
    result := errorset (list ('taylor2e,
                               mkquote f,
                               mkquote varlis,
                               mkquote var0,
                               mkquote n),
                        nil, !*backtrace);
    put('taylor!*,'klist,oldklist);
    if atom result
      then taylor!-error ('expansion, "(possible singularity!)")
     else return car result
  end$

symbolic procedure taylor2e (f, varlis, var0, n);
  %
  % taylor2e expands s.q. f w.r.t. varlis about var0 up to degree n.
  % var is a list of kernels, which means that the expansion takes
  % place in a homogeneous way if there is more than one kernel.
  % The case that var0 is the special atom infinity has to be taken
  % care of by the calling function(s).
  % Expansion is carried out separately for numerator and
  % denominator.  This approach has the advantage of not producing
  % complicated s.q.'s which usually appear if an s.q. is
  % differentiated.  The procedure is (roughly) as follows:
  %  if the denominator of f is free of var
  %   then expand the numerator and divide,
  %  else if the numerator is free of var expand the denominator,
  %   take the reciprocal of the Taylor series and multiply,
  %  else expand both and divide the two series.
  % This fails if there are nontrivial dependencies, e.g.,
  %  if a variable is declared to depend on a kernel in varlis.
  % It is of course necessary that the denominator yields a unit
  %  in the ring of Taylor series. If not, an error will be
  %  signalled by invtaylor or quottaylor, resp.
  %
  if cdr varlis then taylor2hom (f, varlis, var0, n)
   else if denr f = 1 then taylor2f (numr f, car varlis, var0, n, t)
   else if not depends (denr f, car varlis)
    then multintocoefflist (taylor2f (numr f, car varlis, var0, n, t),
                            1 ./ denr f)
   else if numr f = 1
    then delete!-superfluous!-coeffs
           (invtaylor1 (list list (varlis, var0, n),
                        taylor2f (denr f, car varlis, var0, n, nil)),
            1, n)
   else if not depends (numr f, car varlis)
    then multintocoefflist
           (invtaylor1 (list list (varlis, var0, n),
                        taylor2f (denr f, car varlis, var0, n, nil)),
            !*f2q numr f)
   else begin scalar denom; integer n1;
     denom := taylor2f (denr f, car varlis, var0, n, nil);
     n1 := n + taymincoeff denom;
     return
       delete!-superfluous!-coeffs
         (quottaylor1 (list list (varlis, var0, n1),
                       taylor2f (numr f, car varlis, var0, n1, t),
                       denom),
          1, n)
  end$

symbolic procedure taylor2f (f, var, var0, n, flg);
  %
  % taylor2f is the procedure that does the actual expansion
  % of the s.f. f.
  % It returns a list of the form
  %  ((deglis1 . coeff1) (deglis2 . coeff2) ... )
  % For the use of the parameter `flg' see below.
  %
  begin scalar x, y, z; integer k;
    %
    % Calculate once what is needed several times.
    % var0 eq 'infinity is a special case that has already been taken
    % care of by the calling functions by replacing var by 1/var.
    %
    if var0 eq 'infinity then var0 := 0;
    x := list (var . var0);
    y := simp list ('difference, var, var0);
    %
    % The following is a first attempt to treat expressions
    % that have poles at the expansion point.
    % Currently nothing more than factorizable poles, i.e.
    % factors in the denominator are handled.
    % We use the following trick to calculate enough terms: If the
    % first l coefficients of the Taylor series are zero we replace n
    % by n + 2l.  This is necessary since we separately expand
    % numerator and denominator of an expression.  If the expansion of
    % both parts starts with, say, the quadratic term we have to
    % expand them up to order (n+2) to get the correct result up to
    % order n. However, if the numerator starts with a constant term
    % instead, we have to expand up to order (n+4).  It is important,
    % however, to drop the additional coefficients as soon as they are
    % no longer needed.  The parameter `flg' is used here to control
    % this behaviour.  The calling function must pass the value `t' if
    % it wants to inhibit the calculation of additional coefficients.
    % This is currently the case if the s.q. f has a denominator that
    % may contain the expansion variable.  Otherwise `flg' is used to
    % remember if we already encountered a non-zero coefficient.
    %
    f := !*f2q f;
    z := subs2 subsq (f, x);
    if null numr z and not flg then n := n + 1 else flg := t;
    y := list taymakecoeff ((list list 0), z);
    k := 1;
    while k <= n and not null numr f do
      << f := multsq (diffsq (f, var), 1 ./ k);
                                             % k is always > 0!
         % subs2 added to simplify expressions involving roots
         z := subs2 subsq (f, x);
         if null numr z and not flg then n := n + 2 else flg := t;
         if not null numr z then y := taymakecoeff(list list k, z) . y;
         k := k + 1 >>;
    return reversip y
  end;

symbolic procedure taylor2hom (f, varlis, var0, n);
  %
  % Homogeneous expansion of s.q. f wrt the variables in varlis,
  % i.e. the sum of the degrees of the kernels is varlis is <= n
  %
  if null cdr varlis then taylor2e (f, list car varlis, var0, n)
   else for each u in taylor2e (f, list car varlis, var0, n) join
     for each v in taylor2hom (cdr u,
                               cdr varlis,
                               var0,
                               n - get!-degree taycfpl car u)
           collect list (car taycfpl car u . taycfpl car v) . cdr v$

symbolic procedure taylorsamevar (u, varlis, var0, n);
  begin scalar tp; integer mdeg, pos;
    if cdr varlis
      then taylor!-error ('not!-implemented,
                          "(homogeneous expansion in TAYLORSAMEVAR)");
    tp := taytemplate u;
    pos := car var!-is!-nth (tp, car varlis);
    tp := nth (tp, pos);
    if taytpelpoint tp neq var0
      then return taylor1 (if not null tayorig u then tayorig u
                            else simp!* preptaylor!* u,
                           varlis, var0, n);
    mdeg := taytpelorder tp;
    if n=mdeg then return u
     else if n > mdeg
      %
      % further expansion required
      %
      then << lprim "Cannot expand further... truncated.";
              return u >> ;
    return make!-taylor!* (
        for each cc in taycoefflist u join
          if nth (nth (taycfpl cc, pos), 1) > n
            then nil
           else list cc,
        replace!-nth(taytemplate u,pos,
                      {varlis,taytpelpoint tp,n,n+1}),
        tayorig u, tayflags u)
  end$


Comment The `FULL' flag causes the whole term (including the
        Taylor!* symbol) to be passed to SIMPTAYLOR!* ;

symbolic procedure simptaylor!* u;
  if taycoefflist u memq frlis!* then !*tay2q u
   else <<
     %
     % Allow automatic combination of Taylor kernels.
     %
     if !*taylorautocombine and not ('taysimpsq memq mul!*)
       then mul!* := aconc!* (mul!*, 'taysimpsq);
     !*tay2q resimptaylor u >>$

flag ('(taylor!*), 'full)$

put ('taylor!*, 'simpfn, 'simptaylor!*)$

Comment The following is necessary to properly process Taylor kernels
        in substitutions;

flag ('(taylor!*), 'simp0fn);

endmodule;


module tayexpnd;

%*****************************************************************
%
%          The Taylor expansion machinery
%
%*****************************************************************


exports taylorexpand;

imports

% from the REDUCE kernel:
        !*k2q, !*p2q, .*, .+, ./, addsq, apply1, denr, dependsl,
        domainp, error1, errorp, errorset!*, exptsq, invsq, lastpair,
        lc, lpow, lprim, mk!*sq, mkquote, mksq, multsq, mvar, neq,
        nlist, nth, numr, prepsq, quotsq, red, rederr, sfp, simp!*,

% from the header module:
        !*tay2q, cst!-taylor!*, has!-taylor!*, make!-cst!-coefficient,
        make!-taylor!*, set!-tayorig, taycfpl, taycfsq, taycoefflist,
        tayflags, taylor!*!-zerop, taylor!*p, taylor!-kernel!-sq!-p,
        taylor!-trace, taylor!-trace!-mprint, taylor!:, taymakecoeff,
        tayorig, taytemplate, taytpelorder, taytpelpoint, taytpelvars,
        taytpvars, tayvars, tpdegreelist,

% from module TayBasic:
        addtaylor, invtaylor, multtaylor, multtaylorsq,
        quottaylor!-as!-sq,

% from module TayConv:
        preptaylor!*,

% from module TayInterf:
        taylor1,

% from module TayIntro:
        nzerolist, replace!-nth, smemberlp, taylor!-error,
        taylor!-error!*, var!-is!-nth,

% from module TaySimp:
        expttayrat, taysimpsq, taysimpsq!*,

% from module TayUtils:
        addto!-all!-taytpelorders, subtr!-tp!-order, tayminpowerlist,
        tp!-greaterp, truncate!-coefflist, truncate!-taylor!*;


fluid '(!*taylorkeeporiginal !*tayexpanding!* !*tayrestart!*
        !*trtaylor);

global '(!*sqvar!* erfg!*);

symbolic smacro procedure !*tay2q!* u;
   ((u . 1) .* 1 .+ nil) ./ 1;

fluid '(!*taylor!-assoc!-list!* !*taylornocache);

switch taylornocache;

symbolic procedure init!-taylor!-cache;
   !*taylor!-assoc!-list!* := nil . !*sqvar!*;

put('taylornocache,'simpfg,'((t (init!-taylor!-cache))));

symbolic init!-taylor!-cache();

symbolic procedure taylor!-add!-to!-cache(krnl,tp,result);
   if null !*taylornocache
     then car !*taylor!-assoc!-list!* :=
                        ({krnl,sublis({nil . nil},tp)} . result) .
                           car !*taylor!-assoc!-list!*;

fluid '(!*taylor!-max!-precision!-cycles!*);

symbolic(!*taylor!-max!-precision!-cycles!* := 20);

symbolic procedure taylorexpand(sq,tp);
   begin scalar result,oldklist,!*tayexpanding!*,!*tayrestart!*,ll;
         integer cycles;
      ll := tp;
      oldklist := get('taylor!*,'klist);
      !*tayexpanding!* := t;
    restart:
      !*tayrestart!* := nil;
      result := errorset!*({'taylorexpand1,mkquote sq,mkquote ll,'t},
                           !*trtaylor);
      put('taylor!*,'klist,oldklist);
      if null errorp result
        then <<result := car result;
%               if cycles>0 and Taylor!-kernel!-sq!-p result
%                 then result := !*tay2q
%                                  truncate!-Taylor!*(
%                                   mvar numr result,tp);
               return result>>;
      if null !*tayrestart!* then error1();
      erfg!* := nil;
      taylor!-trace {"Failed with template",ll};
      cycles := cycles + 1;
      if cycles > !*taylor!-max!-precision!-cycles!*
         then rederr "cannot expand!!!!!!!!!!!";
      ll := addto!-all!-taytpelorders(ll,nlist(2,length ll));
      taylor!-trace {"Restarting with template",ll};
      goto restart
   end;


symbolic procedure taylorexpand1(sq,ll,flg);
   %
   % sq is a s.q. that is expanded according to the list ll
   %  which has the form
   %  ((var_1 var0_1 deg1) (var_2 var0_2 deg_2) ...)
   % flg if true indicates that the expansion order should be
   %  automatically increased if the result has insufficient order.
   %
   begin scalar result,lll,lmin,dorestart,nl;
     lll := ll;
     if null cadr !*taylor!-assoc!-list!*
       then !*taylor!-assoc!-list!* := nil . !*sqvar!*;
    restart:
     if denr sq = 1
       then result := taysimpsq!* taylorexpand!-sf(numr sq,lll,t)
%      else if not dependsl(denr sq,TayTpVars lll) then begin scalar nn;
%         nn := taylorexpand!-sf(numr sq,lll,t);
%         if Taylor!-kernel!-sq!-p nn
%           then result := !*tay2q multtaylorsq(
%                             truncate!-Taylor!*(mvar numr nn,lll),
%                             1 ./ denr sq)
%          else result := taysimpsq!* quotsq(nn,1 ./ denr sq)
%        end
%      else if numr sq = 1 then begin scalar dd;
%         dd := taylorexpand!-sf(denr sq,lll,nil);
%         if null numr dd
%           then Taylor!-error!*('zero!-denom,'taylorexpand)
%          else if Taylor!-kernel!-sq!-p dd
%           then if Taylor!*!-zerop mvar numr dd
%                  then <<!*tayrestart!* := t;
%                         Taylor!-error('zero!-denom,
%                                       'taylorexpand)>>
%                 else result := !*tay2q invtaylor mvar numr dd
%          else result := taysimpsq!* invsq dd
%        end
%      else if not dependsl(numr sq,TayTpVars lll) then begin scalar dd;
%         dd := taylorexpand!-sf(denr sq,lll,nil);
%         if null numr dd
%           then Taylor!-error!*('zero!-denom,'taylorexpand)
%          else if Taylor!-kernel!-sq!-p dd
%           then if Taylor!*!-zerop mvar numr dd
%                  then <<!*tayrestart!* := t;
%                         Taylor!-error('zero!-denom,
%                                       'taylorexpand)>>
%          else result := !*tay2q multtaylorsq(
%                                     truncate!-Taylor!*(
%                                       invtaylor mvar numr dd,
%                                       lll),
%                                   numr sq ./ 1)
%          else result := taysimpsq!* quotsq(numr sq ./ 1,dd)
%        end
      else begin scalar nn,dd;
         dd := taylorexpand!-sf(denr sq,lll,nil);
         if null numr dd
           then taylor!-error!*('zero!-denom,'taylorexpand)
          else if not taylor!-kernel!-sq!-p dd
           then return
              result := taysimpsq!*
                          quotsq(taylorexpand!-sf(numr sq,lll,t),dd);
         lmin := taycoefflist mvar numr dd;
         while lmin and null numr taycfsq car lmin do lmin := cdr lmin;
         if null lmin then taylor!-error!*('zero!-denom,'taylorexpand);
         lmin := tayminpowerlist lmin;
         nn := taylorexpand!-sf(
                 numr sq,
                 addto!-all!-taytpelorders(lll,lmin),t);
         if not (taylor!-kernel!-sq!-p nn and taylor!-kernel!-sq!-p dd)
           then result := taysimpsq!* quotsq(nn,dd)
          else result := quottaylor!-as!-sq(nn,dd);
       end;
     if not taylor!-kernel!-sq!-p result
       then return if not smemberlp(taytpvars ll,result)
                     then !*tay2q cst!-taylor!*(result,ll)
                    else result;
     result := mvar numr result;
     dorestart := nil;
      taylor!:
        begin scalar ll1;
          ll1 := taytemplate result;
          for i := (length ll1 - length ll) step -1 until 1 do
            ll := nth(ll1,i) . ll;
          if flg then <<nl := subtr!-tp!-order(ll,ll1);
                        for each o in nl do if o>0 then dorestart := t>>
         end;
     if dorestart
%     if tp!-greaterp(ll,TayTemplate result)
       then <<lll := addto!-all!-taytpelorders(lll,nl);
              taylor!-trace "restarting (prec loss)...";
              goto restart>>;
     result := truncate!-taylor!*(result,ll);
     if !*taylorkeeporiginal then set!-tayorig(result,sq);
     return !*tay2q result
  end;

symbolic procedure taylorexpand!-sf(sf,ll,flg);
   begin scalar lcof,lp,next,rest,x,tp,dorestart,lll,xcl,nl;
         integer l,n,count;
     lll := ll;
    restart:
     count := count + 1;
     x := nil ./ 1;
     rest := sf;
     while not null rest do <<
       if domainp rest
         then <<next := !*tay2q!* cst!-taylor!*(rest ./ 1,lll);
                rest := nil>>
        else <<
          lp := taylorexpand!-sp(lpow rest,lll,flg);
          if lc rest=1 then next := lp
           else <<
          lcof := taylorexpand!-sf(lc rest,lll,flg);
          if taylor!-kernel!-sq!-p lcof and taylor!-kernel!-sq!-p lp
            then next := !*tay2q!*
                           multtaylor(mvar numr lcof,mvar numr lp)
           else next := taysimpsq!* multsq(lcof,lp);
         >>;
          rest := red rest>>;
       if null numr x then x := next
        else if taylor!-kernel!-sq!-p x and taylor!-kernel!-sq!-p next
         then x := !*tay2q!* addtaylor(mvar numr x,mvar numr next)
        else x := taysimpsq!* addsq(x,next)>>;
     if not taylor!-kernel!-sq!-p x then return x;
     xcl := taycoefflist mvar numr x;
     if null flg then <<
       while xcl and null numr taycfsq car xcl do xcl := cdr xcl;
       if null xcl then <<
         lll := addto!-all!-taytpelorders(lll,nlist(2,length lll));
         taylor!-trace {"restarting (no coeffs)...(",count,")"};
         goto restart >>
        else taylor!:
             <<l := tayminpowerlist xcl;
               dorestart := nil;
               nl := for i := 1:length lll collect
                       if nth(l,i)>0
                         then <<dorestart := t;
                                2*nth(l,i)>>
                        else 0;
               if dorestart
                 then <<flg :=t;
                        lll := addto!-all!-taytpelorders(lll,nl);
                        taylor!-trace
                          {"restarting (no cst trm)...(",count,")"};
                        goto restart>>>>>>;
     return x
  end;


symbolic procedure taylorexpand!-sp(sp,ll,flg);
  taylor!:
  begin scalar fn,krnl,pow,sk,vars;
    taylor!-trace "expanding s.p.";
    taylor!-trace!-mprint mk!*sq !*p2q sp;
    vars := taytpvars ll;
    krnl := car sp;
    pow := cdr sp;
    sk := if sfp krnl then taylorexpand!-sf(krnl,ll,flg)
           else if (sk := assoc({krnl,ll},
                                car !*taylor!-assoc!-list!*))
            then cdr sk
           else if idp krnl
            then if krnl memq vars
                   then !*tay2q!* make!-var!-taylor!*(krnl,ll)
                  else if dependsl(krnl,vars)
                   then taylorexpand!-diff(krnl,ll,flg)
                  else !*tay2q!* cst!-taylor!*(simp!* krnl,ll)
           else if taylor!*p krnl
                  then if smemberlp(vars,tayvars krnl)
                         then taylorexpand!-samevar(krnl,ll,flg)
                 else taylorexpand!-taylor(krnl,ll,flg)
           else if not idp car krnl
            then taylorexpand!-diff(krnl,ll,flg)
%           else if (fn := get(car krnl,'taylordef))
%            then 
           else if null(fn := get(car krnl,'taylorsimpfn))
            then taylorexpand!-diff(krnl,ll,flg)
           else begin scalar res,args,flg,!*taylorautoexpand;
                  args := for each el in cdr krnl collect
                            if not dependsl(el,vars) then el
                             else <<flg := t;
                                    prepsq taylorexpand(simp!* el,ll)>>;
                  if has!-taylor!* args
                    then res := apply1(fn,car krnl . args)
                   else if null flg
                    then res := !*tay2q!* cst!-taylor!*(mksq(krnl,1),ll)
                   else res := mksq(krnl,1);
                  return res
                end;
    if taylor!-kernel!-sq!-p sk
      then taylor!-add!-to!-cache(krnl,taytemplate mvar numr sk,sk);
    sp := if pow = 1 then sk
           else if not taylor!-kernel!-sq!-p sk
            then taysimpsq exptsq(sk,pow)
           else <<sk := mvar numr sk;
                  !*tay2q!* if pow=2 then multtaylor(sk,sk)
                             else expttayrat(sk,pow ./ 1)>>;
    taylor!-trace "result of expanding s.p. is";
    taylor!-trace!-mprint mk!*sq sp;
    return sp
  end;

symbolic procedure make!-var!-taylor!*(krnl,ll);
  taylor!:
   begin scalar pos,var0,cfl,ordr; integer pos1;
     pos := var!-is!-nth(ll,krnl);
     pos1 := cdr pos;
     pos := car pos;
     var0 := taytpelpoint nth(ll,pos);
     ordr := taytpelorder nth(ll,pos);
%     if ordr < 1
%       then ll := replace!-TayTpEl(ll,pos,
%                    replace!-TayTpElOrder(nth(ll,pos),1));
     if ordr < 1 then return
        make!-taylor!*(nil,replace!-nth(ll,pos,
                                        ({taytpelvars tpel,
                                         taytpelpoint tpel,
                                         taytpelorder tpel,
                                         1}
                                         where tpel := nth(ll,pos))),
                       nil,nil)
     else return make!-taylor!*(
       if var0 = 0
         then {taymakecoeff(make!-var!-powerlist(ll,pos,pos1,nil),
                            1 ./ 1)}
        else if var0 eq 'infinity
         then {taymakecoeff(make!-var!-powerlist(ll,pos,pos1,t),1 ./ 1)}
        else {make!-cst!-coefficient(simp!* var0,ll),
              taymakecoeff(make!-var!-powerlist(ll,pos,pos1,nil),
                           1 ./ 1)},
       ll,nil,nil)
   end;


symbolic procedure make!-var!-powerlist(tp,pos,pos1,flg);
   if null tp then nil
    else ((if pos=1
             then for j := 1:l collect
                    if j neq pos1 then 0
                     else if flg then -1
                     else 1
            else nzerolist l)
          where l := length taytpelvars car tp)
          . make!-var!-powerlist(cdr tp,pos - 1,pos1,flg);


symbolic procedure taylorexpand!-taylor(tkrnl,ll,flg);
   begin scalar tay,notay,x;
     notay := nil ./ 1;
     for each cc in taycoefflist tkrnl do <<
       x := taylorexpand1(taycfsq cc,ll,t);
       if taylor!-kernel!-sq!-p x
         then tay := nconc(tay,
                           for each cc2 in taycoefflist mvar numr x
                             collect taymakecoeff(
                                       append(taycfpl cc,taycfpl cc2),
                                       taycfsq cc2))
        else taylor!-error('expansion,"(possbile singularity)")>>;
              %notay := aconc!*(notay,cc)>>;
     return
       if null tay then nil ./ 1
        else !*tay2q!* make!-taylor!*(tay,
                                      append(taytemplate tkrnl,ll),
                                      nil,nil);
  end;


Comment The cache maintained in !*!*taylorexpand!-diff!-cache!*!* is
        the key to handle the case of a kernel whose derivative
        involves the kernel itself. It is guaranteed that in every
        recursive step the expansion order is smaller than in the
        previous one;

fluid '(!*!*taylorexpand!-diff!-cache!*!*);

symbolic procedure taylorexpand!-diff(krnl,ll,flg);
  begin scalar result;
    %
    % We use a very simple strategy: if we know a partial derivative
    %  of the kernel, we pass the problem to taylorexpand!-diff1 which
    %  will try to differentiate the kernel, expand the result and
    %  integrate again.
    %
    if null atom krnl and get(car krnl,'dfn)
      then (result := errorset!*({'taylorexpand!-diff1,
                                 mkquote krnl,mkquote ll,mkquote flg},
                                !*backtrace)
           where !*!*taylorexpand!-diff!-cache!*!* :=
                   !*!*taylorexpand!-diff!-cache!*!*);
    %
    % If this fails we fall back to simple differentiation and
    %  substitution at the expansion point.
    %
    if result and not errorp result
      then result := car result
     else if !*tayrestart!* then error1() % propagate
     else <<result := !*k2q krnl;
            for each el in ll do
              %
              % taylor1 is the function that does the real work
              %
              result := !*tay2q!* taylor1(result,
                                          taytpelvars el,
                                          taytpelpoint el,
                                          taytpelorder el)>>;
    if !*taylorkeeporiginal
      then set!-tayorig(mvar numr result,!*k2q krnl);
    return result
  end;

symbolic procedure taylorexpand!-diff1(krnl,ll,flg);
   <<if y and taytpvars ll = taytpvars (y := cdr y)
          and not tp!-greaterp(y,ll)
       then ll := for each el in y collect
                    {taytpelvars el,taytpelpoint el,
                     taytpelorder el - 1,taytpelnext el - 1};
     !*!*taylorexpand!-diff!-cache!*!* :=
                (krnl . ll) . !*!*taylorexpand!-diff!-cache!*!*;
     for each el in ll do result := taylorexpand!-diff2(result,el,nil);
     result>>
       where result := !*k2q krnl,
             y := assoc(krnl,!*!*taylorexpand!-diff!-cache!*!*);

symbolic procedure taylorexpand!-diff2(sq,el,flg);
   begin scalar l,v,singlist,c0,tay,l0,tp,tcl;
     singlist := nil ./ 1;
     tp := {el};
     if taytpelorder el > 0 then <<
       l := for each var in taytpelvars el collect begin scalar r;
            r := taylorexpand1(diffsq(sq,var),
                               {{taytpelvars el,
                                 taytpelpoint el,
                                 taytpelorder el - 1,
                                 taytpelnext el - 1}},flg);
            if taylor!-kernel!-sq!-p r
              then return inttaylorwrttayvar(mvar numr r,var)
             else taylor!-error('expansion,"(possible singularity)");
          end;
       v := taycoefflist!-union
              for each pp in l collect taycoefflist cdr pp;
       for each pp in l do
         if car pp then singlist := addsq(singlist,car pp);
       if not null numr singlist
         then taylor!-error('expansion,"(possible singularity)")>>;
     c0 := subsq(sq,for each var in taytpelvars el collect
                      (var . taytpelpoint el));
     l0 := {nzerolist length taytpelvars el};

     if not taylor!-kernel!-sq!-p c0
       then tcl := taymakecoeff(l0,c0) . v
      else <<c0 := mvar numr c0;
             tp := nconc!*(taytemplate c0,tp);
             tcl := nconc(for each pp in taycoefflist c0 collect
                            taymakecoeff(append(taycfpl pp,l0),
                                         taycfsq pp),
                          v)>>;

     if not null l
       then for each pp in l do
              tp := taytp!-min2(tp,taytemplate cdr pp);

     tay := !*tay2q!* make!-taylor!*(tcl,tp,nil,nil);

     if null numr singlist then return tay
      else return addsq(singlist,tay)
  end;

symbolic procedure taylorexpand!-samevar(u,ll,flg);
  taylor!:
  begin scalar tp,varlis; integer mdeg,pos,n;
    if length ll > 1 or length taytpelvars (ll := car ll) > 1
      then taylor!-error('not!-implemented,
                          "(homogeneous expansion in TAYLORSAMEVAR)");
    tp := taytemplate u;
    varlis := taytpelvars ll;
    n := taytpelorder ll;
    pos := car var!-is!-nth(tp,car varlis);
    tp := nth(tp,pos);
    if taytpelpoint tp neq taytpelpoint ll
      then return !*tay2q!* taylor1(if not null tayorig u then tayorig u
                            else simp!* preptaylor!* u,
                           varlis,taytpelpoint ll,n);
    mdeg := taytpelorder tp;
    if n=mdeg then return !*tay2q!* u
     else if n > mdeg
      %
      % further expansion required
      %
      then << lprim "Cannot expand further... truncated.";
              return !*tay2q!* u>>;
    return !*tay2q!* make!-taylor!*(
        for each cc in taycoefflist u join
          if nth(nth(taycfpl cc,pos),1) > n
            then nil
           else list cc,
        replace!-nth(taytemplate u,pos,
                      {varlis,taytpelpoint tp,n,n+1}),
        tayorig u,tayflags u)
  end;

endmodule;


module taybasic;

%*****************************************************************
%
%      Functions that implement the basic operations
%       on Taylor kernels
%
%*****************************************************************


exports addtaylor, addtaylor1, invtaylor, invtaylor1, makecoeffpairs,
        multtaylor, multtaylor1, multtaylorsq, negtaylor, negtaylor1,
        quottaylor, quottaylor1$

imports

% from the REDUCE kernel:
        addsq, invsq, lastpair, mvar, multsq, negsq, nth, numr, over,
        quotsq, reversip, subtrsq, union,

% from the header module:
        !*tay2q, get!-degree, make!-cst!-coefflis,
        make!-cst!-powerlist, make!-taylor!*, multintocoefflist,
        subs2coefflist, taycfpl, taycfsq, taycoefflist, tayflags,
        tayflagscombine, taygetcoeff, taylor!-kernel!-sq!-p, taylor!:,
        taymakecoeff, tayorig, taytemplate, taytpelvars, tpdegreelist,

% from module Tayintro:
        confusion, taylor!-error,

% from module Tayutils:
        add!.comp!.tp!., add!-degrees, enter!-sorted, exceeds!-order,
        inv!.tp!., mult!.comp!.tp!.,
        taydegree!-strict!-less!-or!-equal!-p, truncate!-coefflist$


fluid '(!*taylorkeeporiginal)$

symbolic procedure multtaylorsq (tay, sq);
  %
  % multiplies the s.q. sq into the Taylor kernel tay.
  % value is a Taylor kernel.
  % no check for variable dependency is done!
  %
  if null tay or null numr sq then nil
   else make!-taylor!* (
              multintocoefflist (taycoefflist tay, sq),
              taytemplate tay,
              if !*taylorkeeporiginal and tayorig tay
                then multsq (sq, tayorig tay)
               else nil,
              tayflags tay)$


symbolic smacro procedure degree!-union (u, v);
  union (u, v)$ % works for the moment;

symbolic procedure addtaylor(u,v);
  %
  % u and v are two Taylor kernels
  % value is their sum, as a Taylor kernel
  %
  (if null tp then confusion 'addtaylor
    else addtaylor!*(u,v,tp))
    where tp := add!.comp!.tp!.(u,v);


symbolic procedure addtaylor!-as!-sq(u,v);
   begin scalar tp;
     return
       if taylor!-kernel!-sq!-p u and taylor!-kernel!-sq!-p v and
          (tp := add!.comp!.tp!.(mvar numr u,mvar numr v))
         then !*tay2q addtaylor!*(mvar numr u,mvar numr v,tp)
        else addsq(u,v)
   end;

symbolic procedure addtaylor!*(u,v,tp);
   begin scalar dl;
     dl := tpdegreelist tp;
     return
       make!-taylor!*
           (addtaylor1(truncate!-coefflist(taycoefflist u,dl),
                       truncate!-coefflist(taycoefflist v,dl)),
            tp,
            if !*taylorkeeporiginal and tayorig u and tayorig v
              then addsq(tayorig u,tayorig v)
             else nil,
            tayflagscombine(u,v))
   end;

symbolic procedure addtaylor1 (l1, l2);
  %
  % Returns the coefflist that is the sum of coefflists l1, l2.
  %
  begin scalar degreelist, cff, clist;
    %
    % The following is necessary to ensure that the rplacd below
    %  doesn't do any harm to the list in l1.
    %
    clist := for each cc in l1 collect
               taymakecoeff(taycfpl cc,taycfsq cc);
    for each cc in l2 do <<
      cff := assoc(taycfpl cc,clist);
      if null cff then clist := enter!-sorted(cc,clist)
       else rplacd(cff,addsq(taycfsq cff,taycfsq cc))>>;
    return subs2coefflist clist
  end;


symbolic procedure negtaylor u;
  make!-taylor!* (
         negtaylor1 taycoefflist u,
         taytemplate u,
         if !*taylorkeeporiginal and tayorig u
           then negsq tayorig u else nil,
         tayflags u)$

symbolic procedure negtaylor1 tcl;
  for each cc in tcl collect
    taymakecoeff (taycfpl cc, negsq taycfsq cc)$

symbolic procedure multtaylor(u,v);
  %
  % u and v are two Taylor kernels,
  % result is their product, as a Taylor kernel.
  %
  (if null tps then confusion 'multtaylor
    else multtaylor!*(u,v,tps))
   where tps := mult!.comp!.tp!.(u,v,nil);

symbolic procedure multtaylor!-as!-sq(u,v);
   begin scalar tps;
     return
       if taylor!-kernel!-sq!-p u and taylor!-kernel!-sq!-p v and
          (tps := mult!.comp!.tp!.(mvar numr u,mvar numr v,nil))
         then !*tay2q multtaylor!*(mvar numr u,mvar numr v,tps)
        else multsq(u,v)
   end;

symbolic procedure multtaylor!*(u,v,tps);
   make!-taylor!*
        (multtaylor1(car tps,
                     truncate!-coefflist(taycoefflist u,cadr tps),
                     truncate!-coefflist(taycoefflist v,caddr tps)),
         car tps,
         if !*taylorkeeporiginal and tayorig u and tayorig v
           then multsq (tayorig u, tayorig v)
          else nil,
         tayflagscombine(u,v));

symbolic procedure multtaylor1 (tp, l1, l2);
  %
  % Returns the coefflist that is the product of coefflists l1, l2,
  %  with respect to Taylor template tp.
  %
  begin scalar clist, cff, pl, rlist, sq;
    tp := tpdegreelist tp;
    for each cf1 in l1 do
      for each cf2 in l2 do <<
        pl := add!-degrees(taycfpl cf1,taycfpl cf2);
        if not exceeds!-order(tp,pl) then <<
          sq := multsq(taycfsq cf1,taycfsq cf2);
          cff := assoc(pl,rlist);
          if null cff
            then rlist := enter!-sorted (taymakecoeff(pl,sq), rlist)
           else rplacd (cff, addsq (taycfsq cff, sq))>>>>;
    return subs2coefflist rlist
  end;

Comment Implementation of Taylor division.
        We use the following algorithm:
        Suppose the numerator and denominator are of the form

                -----                    -----
                \          k             \          l
        f(x) =   >     a  x   ,  g(x) =   >     b  x   ,
                /       k                /       l
                -----                    -----
                k>=k0                    l>=l0

        respectively.  The quotient is supposed to be

                -----
                \          m
        h(x) =   >     c  x   .
                /       m
                -----
                m>=m0

        Clearly: m0 = k0 - l0.  This follows immediately from
        f(x) = g(x) * h(x) by comparing lowest order terms.
        This equation can now be written:

         -----            -----                 -----
         \          k     \             l+m     \              n
          >     a  x   =   >     b  c  x     =   >    b    c  x  .
         /       k        /       l  m          /      n-m  m
         -----            -----                 -----
         k>=k0            l>=l0              m0<=m<=n-l0
                          m>=m0                n>=l0+m0

        Comparison of orders leads immediately to

                  -----
                  \
          a    =   >    b    c   ,  n>=l0+m0 .
           n      /      n-m  m
                  -----
               m0<=m<=n-l0

        We write the last term of the series separately:

                  -----
                  \
          a    =   >    b    c   + b   c     ,  n>=l0+m0 ,
           n      /      n-m  m     l0  n-l0
                  -----
               m0<=m<n-l0

        which leads immediately to the recursion formula

                       /       -----           \
                   1   |       \               |
         c     = ----- | a  -   >     b    c   | .
          n-l0    b    |  n    /       n-m  m  |
                   l0  \       -----           /
                            m0<=m<n-l0

        Finally we shift n by l0 and obtain

                       /          -----              \
                   1   |          \                  |
         c     = ----- | a     -   >     b       c   | .  (*)
          n       b    |  n+l0    /       n-m+l0  m  |
                   l0  \          -----              /
                                 m0<=m<n

        For multidimensional Taylor series we can use the same
        expression if we interpret all indices as appropriate
        multiindices.

        For computing the reciprocal of a Taylor series we use
        the formula (*) above with f(x) = 1, i.e. lowest order
        coefficient = 1, all others = 0;


symbolic procedure quottaylor(u,v);
  %
  % Divides Taylor series u by Taylor series v.
  % Like invtaylor, it depends on ordering of the coefficients
  %  according to the degree of the expansion variables (lowest first).
  %
  (if null tps then confusion 'quottaylor
    else quottaylor!*(u,v,tps))
   where tps := mult!.comp!.tp!.(u,v,t);

symbolic procedure quottaylor!-as!-sq(u,v);
   begin scalar tps;
     return
       if taylor!-kernel!-sq!-p u and taylor!-kernel!-sq!-p v and
          (tps := mult!.comp!.tp!.(mvar numr u,mvar numr v,t))
         then !*tay2q quottaylor!*(mvar numr u,mvar numr v,tps)
        else quotsq(u,v)
   end;

symbolic procedure quottaylor!*(u,v,tps);
   make!-taylor!*(
      quottaylor1(car tps,
                  truncate!-coefflist(taycoefflist u,cadr tps),
                  truncate!-coefflist(taycoefflist v,caddr tps)),
      car tps,
      if !*taylorkeeporiginal and tayorig u and tayorig v
        then quotsq (tayorig u, tayorig v)
       else nil,
      tayflagscombine(u,v));

symbolic procedure quottaylor1 (tay, lu, lv);
  %
  % Does the real work, called also by the expansion procedures.
  % Returns the coefflist.
  %
  taylor!:
  begin scalar clist, lminu, lminv, aminu, aminv, ccmin, coefflis;
    while lu and null numr taycfsq car lu do lu := cdr lu;
    if null lu then return make!-cst!-coefflis(nil ./ 1,tay);
    lminu := taycfpl car lu;
    for each el in cdr lu do
      if not null numr taycfsq el then
        lminu := taydegree!-min2(lminu,taycfpl el);
    aminu := if lminu neq taycfpl car lu then taygetcoeff(lminu,lu)
              else taycfsq car lu;
    while lv and null numr taycfsq car lv do lv := cdr lv;
    if null lv then taylor!-error('not!-a!-unit,'quottaylor);
    if null lu then
      return {taymakecoeff(make!-cst!-powerlist tay,nil ./ 1)};
    aminv := taycfsq car lv;   % first element is that of lowest degree
    lminv := taycfpl car lv;
    for each cf in cdr lv do
      if not taydegree!-strict!-less!-or!-equal!-p(lminv,taycfpl cf)
        then taylor!-error('not!-a!-unit,'quottaylor);
    ccmin := subtr!-degrees(lminu,lminv);
    clist := {taymakecoeff(ccmin,quotsq(aminu,aminv))};
    coefflis := makecoeffs(ccmin,tpdegreelist tay);
    for each cc in cdr coefflis do
      clist := taymakecoeff(cc,
        quotsq(subtrsq(taygetcoeff(add!-degrees(cc,lminv),lu),
                       addcoeffs(clist,lv,ccmin,cc)),
               aminv))
                 . clist;
    return subs2coefflist reversip clist
  end;

symbolic procedure taydegree!-min2(u,v);
  %
  % (TayPowerList, TayPowerList) -> TayPowerList
  %
  % returns minimum of both powerlists
  %
  for i := 1 : length u collect begin scalar l1,l2;
    l1 := nth(u,i);
    l2 := nth(v,i);
    return
      for j := 1 : length l1 collect
        taylor!: min2(nth(l1,j),nth(l2,j))
  end;

symbolic procedure makecoeffshom (cmin, nterms);
  if null cmin then '(nil)
   else taylor!:
     for i := 0 : nterms join
       for each l in makecoeffshom (cdr cmin, nterms - i) collect
             (car cmin + i) . l$

symbolic procedure makecoeffshom0(nvars,nterms);
  %
  % Like makecoeffshom, but the smallest powerlist is a list
  % of nvars zeroes.
  %
  if nvars=0 then '(nil)
   else taylor!:
     for i := 0 : nterms join
       for each l in makecoeffshom0(nvars - 1,nterms - i) collect i . l;

%symbolic procedure makecoeffs!-old pl;
%  %
%  % pl is a list of pairs, the car of each being the smallest,
%  % the cdr number of terms.
%  % It returns an ordered list of all index lists matching this
%  % requirement.
%  %
%  if null pl then '(nil)
%   else Taylor!:
%      for each l1 in makecoeffs!-old cdr pl join
%        for each l2 in makecoeffshom(caar pl,cdar pl)
%                 collect (l2 . l1);

symbolic procedure makecoeffs(plmin,dgl);
  %
  % plmin is a list of the smallest term, dgl the degreelist of
  % the largest term.
  % number of terms.
  % It returns an ordered list of all index lists matching this
  % requirement.
  %
  if null plmin then '(nil)
   else for each l1 in makecoeffs(cdr plmin,cdr dgl) join
          for each l2 in makecoeffshom(
                           car plmin,
                           car dgl - get!-degree car plmin)
                 collect (l2 . l1);

symbolic procedure makecoeffs0(tp,dgl);
  %
  % Like makecoeffs, but plmin is implied to consist of all zeroes.
  %
  if null tp then '(nil)
   else for each l1 in makecoeffs0(cdr tp,cdr dgl) join
        for each l2 in makecoeffshom0(length taytpelvars car tp,car dgl)
                 collect (l2 . l1);

symbolic procedure makecoeffpairs1(pllow,plhigh,lmin);
  if null pllow then '((nil))
   else for each l1 in makecoeffpairs1(cdr pllow,
                                       cdr plhigh,
                                       cdr lmin) join
     for each l2 in makecoeffpairshom(car pllow,car plhigh,car lmin,-1)
           collect (car l2 . car l1) . (cdr l2 . cdr l1);

symbolic procedure makecoeffpairshom(clow,chigh,clmin,inc);
  if null clmin then '((nil))
   else taylor!:
    for i := car chigh step inc until car clow join
      for each l in makecoeffpairshom(cdr clow,cdr chigh,cdr clmin,inc)
          collect (i . car l) . ((car chigh + car clmin - i) . cdr l);

symbolic procedure makecoeffpairs(pllow,plhigh,lmin);
  reversip cdr makecoeffpairs1(pllow,plhigh,lmin);

symbolic procedure addcoeffs(cl1,cl2,pllow,plhigh);
  begin scalar s;
    s := nil ./ 1;
    for each p in makecoeffpairs(pllow,plhigh,caar cl2) do
        s := addsq(s,multsq(taygetcoeff(car p,cl1),
                            taygetcoeff(cdr p,cl2)));
    return s
  end;

symbolic procedure invtaylor u;
  %
  % Inverts a Taylor series expansion,
  % depends on ordering of the coefficients according to the
  %  degree of the expansion variables (lowest first)
  %
  if null u then confusion 'invtaylor
   else begin scalar tps;
     tps := inv!.tp!. u;
     return make!-taylor!* (
               invtaylor1 (car tps,
                           truncate!-coefflist (taycoefflist u,
                                                cadr tps)),
               car tps,
               if !*taylorkeeporiginal and tayorig u
                then invsq tayorig u
                 else nil,
               tayflags u);
   end;

symbolic procedure invtaylor1 (tay, l);
  %
  % Does the real work, called also by the expansion procedures.
  % Returns the coefflist.
  %
  taylor!:
  begin scalar clist, amin, ccmin, coefflis;
    while l and null numr taycfsq car l do l := cdr l;
    if null l then taylor!-error ('not!-a!-unit, 'invtaylor);
    amin := taycfsq car l;  % first element must have lowest degree
    ccmin := taycfpl car l;
    for each cf in cdr l do
      if not taydegree!-strict!-less!-or!-equal!-p (ccmin, taycfpl cf)
        then taylor!-error ('not!-a!-unit, 'invtaylor);
    ccmin := for each m in ccmin collect
               for each m1 in m collect -m1;
    clist := list taymakecoeff(ccmin, invsq amin);
    coefflis := makecoeffs(ccmin,tpdegreelist tay);
    for each cc in cdr coefflis do
      clist := taymakecoeff(cc,
                 negsq quotsq (addcoeffs(clist,l,ccmin,cc),
                               amin))
                   . clist;
    return subs2coefflist reversip clist
  end;

endmodule;


module taysimp;

%*****************************************************************
%
%     The special Taylor simplification functions
%
%*****************************************************************


exports taysimpp, taysimpsq, taysimpsq!*, tayinpoly, expttayrat,
        expttayrat1;

imports

% from the REDUCE kernel:
        !*f2q, !*k2q, !*p2f, !*p2q, !*t2q, addsq, apply1, denr,
        domainp, evenp, exptsq, invsq, kernp, mk!*sq, multpq, multsq,
        mvar, neq, nth, numr, over, pdeg, prepsq, quotsq, reversip,
        sfp, simp, simp!*, tc, to, tpow,

% from the header module:
        !*tay2q, !*tayexp2q, comp!.tp!.!-p, cst!-taylor!*,
        has!-taylor!*, find!-non!-zero, get!-degreelist, has!-tayvars,
        make!-cst!-coefflis, make!-cst!-powerlist, make!-taylor!*,
        resimptaylor, taycfpl, taycfsq, taycoefflist, tayflags,
        taygetcoeff, taylor!-kernel!-sq!-p, taylor!*p, taymakecoeff,
        taymultcoeffs, tayorig, taytemplate, tpdegreelist,

% from module Tayintro:
        confusion, taylor!-error,

% from module Tayutils:
        addto!-all!-taytpelorders, get!-cst!-coeff,
        taylor!*!-nzconstantp, taylor!*!-zerop,

% from module Tayinterf:
        taylorexpand, taylorexpand!-sf,

% from module Taybasic:
        addtaylor, addtaylor!-as!-sq, addtaylor1, invtaylor,
        makecoeffpairs, makecoeffs0, multtaylor, multtaylor!-as!-sq,
        multtaylor1, multtaylorsq, quottaylor, quottaylor!-as!-sq;


fluid '(!*taylorautoexpand !*taylorkeeporiginal)$

Comment The procedures in this module provide the higher taylor
        manipulation machinery.  Given any s.q. (s.f.,...) they
        return the equivalent Taylor kernel (disguised as a s.q.)
        if the argument contains a Taylor kernel and everything
        else may be Taylor expanded.
        Otherwise the Taylor kernels in the argument are only
        partially combined (but as far as possible);


symbolic procedure taysimpsq u;
  %
  % The argument u is any standard quotient.
  % numerator and denominator are first simplified independently,
  % then the quotient is built.
  % We have four possible cases here, as both expressions
  %  may or may not be Taylor kernels.
  %
  (lambda(nm,dd);
    if null numr dd then taylor!-error('zero!-denom,'taysimpsq)
     else if taylor!-kernel!-sq!-p dd
      then if taylor!-kernel!-sq!-p nm
        then if comp!.tp!.!-p(mvar numr nm,mvar numr dd)
          then !*tay2q resimptaylor
                         quottaylor(mvar numr nm,mvar numr dd)
         else quotsq(nm,dd)
       else if not has!-tayvars(mvar numr dd,nm)
        then !*tay2q resimptaylor
                    multtaylorsq(invtaylor mvar numr dd,nm)
       else if null !*taylorautoexpand or has!-taylor!* nm
        then quotsq(nm,dd)
       else !*tay2q resimptaylor quottaylor(
                mvar numr taylorexpand(nm,taytemplate mvar numr dd),
                mvar numr dd)
     else if taylor!-kernel!-sq!-p nm
      then if not has!-tayvars(mvar numr nm,dd)
             then !*tay2q resimptaylor
                            multtaylorsq(mvar numr nm,invsq dd)
            else if null !*taylorautoexpand or has!-taylor!* dd
             then quotsq(nm,dd)
            else taysimpsq!*
                   quottaylor!-as!-sq(
                     nm,
                     taylorexpand(dd,taytemplate mvar numr nm))
     else quotsq(nm,dd))
   (taysimpf numr u,taysimpf denr u)$


symbolic procedure taysimpsq!* u;
   %
   % Variant of taysimpsq that does not automatically expand
   %  non-Taylor expressions
   %
   taysimpsq u where !*taylorautoexpand := nil;


symbolic procedure taysimpf u;
  %
  % u is a standard form which may contain Taylor subexpressions;
  % value is a standard form
  %
  begin scalar tay,notay,x,flg;
    %
    % Remember the definition of a s.f.:
    %  it is either a domain element,
    %  or it's car is a standard term and it's cdr is a s.f.
    %
    notay := nil ./ 1;
    while u do
      %
      % Split the constituents of the s.f. into non-Taylor and
      %  Taylor parts.  Taylor s.t.'s are simplified accordingly.
      % A domain element can never be a Taylor kernel.
      %
      <<if domainp u then notay := addsq(!*f2q u,notay)
         else if not has!-taylor!* car u
          then notay := addsq(notay,!*t2q car u)
         else <<x := taysimpt car u;
                if taylor!-kernel!-sq!-p x
                  then if null tay then tay := mvar numr x
                    else if comp!.tp!.!-p(tay,mvar numr x)
                           then tay := addtaylor(tay,mvar numr x)
                          else <<flg := t;
                                 %
                                 % flg indicates that there are
                                 %  Taylor kernels whose templates
                                 %  are not compatible
                                 %
                                 notay := addsq(notay,x)>>
                 else notay := addsq(notay,x)>>;
         u := if domainp u then nil else cdr u>>;
    %
    % tay is now a Taylor kernel or nil.
    % If it is nil, return the non-taylor parts.
    %
    if null numr notay and not null tay then return !*tay2q tay
     else if null tay or taylor!*!-zerop tay then return notay;
    %
    % Otherwise the non-taylor parts (if the are non-nil)
    % must be expanded if !*taylorautoexpand is non-nil.
    % The only exception are terms that do not contain
    % any of the Taylor variables: these are always expanded.
    %
    if null !*taylorautoexpand and has!-tayvars(tay,notay)
      then return addsq(!*tay2q tay,notay);
    if flg then return addsq(!*tay2q tay,notay)
     else <<
       notay := taylorexpand(notay,taytemplate tay);
       return taysimpsq!* addtaylor!-as!-sq(notay,!*tay2q tay)>>
  end;

symbolic procedure taysimpt u;
  %
  % u is a standard term containing one or more Taylor kernels,
  % value is the simplified Taylor expression (also as a s.f.).
  %
  begin scalar rest,pow;
    %
    % Since the coefficient of a term is a s.f.
    % we call taysimpf on it.
    %
    rest := taysimpf tc u;
    if null numr rest then return rest;
    pow := tpow u;
    %
    % Then we have to distinguish three cases:
    %   the case where no Taylor kernel appears was already caught
    %   by taysimpf before taysimpt was called.
    %
    % If combination of different Taylor kernels is impossible
    %   return them separately
    %
    % Remark: the call to SMEMQLP checks if rest contains one of
    %         the Taylor variables if it is not a Taylor kernel.
    %
    return if not has!-taylor!* pow
      then if taylor!-kernel!-sq!-p rest
             then multpowerintotaylor(pow,mvar numr rest)
            else multpq(pow,rest)
     else <<pow := taysimpp pow;
            if not has!-taylor!* rest and taylor!-kernel!-sq!-p pow
              then if has!-tayvars(mvar numr pow,rest)
                 then if !*taylorautoexpand
                   then taysimpsq!* multtaylor!-as!-sq(pow,
%                          taylorexpand(rest,TayTemplate mvar numr pow))
%
% the above is not entirely correct. the expansion should be done
% in a way so that the result of the multiplication has same order 
% as pow
%
                          taylorexpand!-sf(tc u,
                                           taytemplate mvar numr pow,
                                           nil))
                  else multsq(pow,rest)
                else !*tay2q multtaylorsq(mvar numr pow,rest)
              else multtaylor!-as!-sq(pow,rest)>>
  end;

symbolic procedure multpowerintotaylor (p, tk);
  %
  % p is a standard power, tk a Taylor kernel
  % value is p expanded to a Taylor kernel multiplied by tk
  % this requires Taylor expansion of p if it contains
  % at least one of the expansion variables
  %
  % Remark: the call to SMEMQLP checks if p contains one of
  %         the Taylor variables.
  %
  if not has!-tayvars(tk,p)
    then !*tay2q multtaylorsq(tk,!*p2q p)
   else if !*taylorautoexpand
%    then taysimpsq!*
%           multtaylor!-as!-sq(!*tay2q tk,
%                     taylorexpand(!*p2q p,TayTemplate tk))
%
% here the same comment as above applies
%
    then taysimpsq!*
           multtaylor!-as!-sq(!*tay2q tk,
                     taylorexpand!-sf(!*p2f p,taytemplate tk,nil))
   else if taylor!*!-nzconstantp tk
    then multpq(p,get!-cst!-coeff tk)
   else multpq(p,!*tay2q tk);


symbolic procedure taysimpp u;
  %
  % u is a standard power containing a Taylor expression,
  % value is the simplified Taylor expression, as a s.f.
  %
  % We begin by isolating base and exponent.
  % First we simplify them separately.
  % Remember that the exponent is always an integer,
  % base is a kernel.
  %
  % If the main variable of the power is a kernel made of one
  %  of the functions known to the Taylor simplifier, call
  %  the appropriate simplification function.
  %  (There is a user hook here!)
  %
  if null car u or null pdeg u then confusion 'taysimpp
   else if sfp car u then taysimpsq exptsq(taysimpf car u,cdr u)
   else if not taylor!*p car u
    then ((if kernp x
             then if (x := mvar numr x) = car u then !*p2q u
                   else if cdr u=1 then !*k2q x
                   else taysimpp(x .** cdr u)
            else if cdr u=1 then x
            else taysimpsq exptsq(x,cdr u))
          where x := (taysimpmainvar car u))
   %
   % We know how to raise a Taylor series to a rational power:
   %  positive integer --> multiply
   %  negative integer --> multiply and invert
   % Zero exponent should not appear: should be already simplified
   %  to 1 by the standard simplifier
   %
   else if not fixp pdeg u or pdeg u = 0 then confusion 'taysimpp
   else !*tay2q
     if pdeg u = 1 then car u else expttayi(car u,cdr u)$


symbolic procedure taysimpmainvar u;
  if not sfp u then taysimpkernel u
   else !*f2q taysimpf u;


symbolic procedure taysimpkernel u;
  begin scalar fn, x;
    u := simp!* u;
    if not kernp u then return u
     else << x := mvar numr u;
             if atom x or taylor!*p x then return u;
             fn := get (car x, 'taylorsimpfn);
             return if null fn then u
                     else apply1 (fn, x) >>
  end$


symbolic procedure expttayi(u,i);
  %
  % raise Taylor kernel u to integer power i
  % algorithm is a scheme that computes powers of two.
  %
  begin scalar v,flg;
    if i<0 then <<i := -i; flg := t>>;
    v := if evenp i then cst!-taylor!*(1 ./ 1,taytemplate u)
          else <<i := i - 1; u>>;
    while (i:=i/2)>0 do <<if not evenp i then v := multtaylor(v,u);
                   u := multtaylor(u,u)>>;
    return if flg then invtaylor u else u
  end;


Comment non-integer powers of Taylor kernels;


symbolic procedure tayinpoly (polylist, tay);
  %
  % polylist is a list of the coefficients of a polynomial,
  % with the i-th element corresponding to the power var**(i-1)
  % result is the Taylor kernel tay substituted for the polynomial's
  %  variable
  %
  make!-taylor!* (
    tayinpoly1 (polylist, taytemplate tay, taycoefflist tay),
    taytemplate tay,
    nil,
    nil)$


symbolic procedure tayinpoly1 (polylist, tp, cflis);
  %
  % Does the work for tayinpoly.  Returns coeff list.
  % Uses Horner's scheme!
  % We use addtaylor1/multtaylor1 instead of addtaylor/multtaylor
  % since we are sure that the templates match.
  %
  begin scalar x;
    polylist := reversip polylist;
    x := make!-cst!-coefflis (car polylist, tp);
    polylist := cdr polylist;
    while polylist do
      << x := multtaylor1 (tp, x, cflis);
         if not null numr car polylist
           then x := addtaylor1 (x,
                               make!-cst!-coefflis (car polylist, tp));
         polylist := cdr polylist >>;
    return x
  end$


Comment The implementation of expttayrat follows the algorithm
        quoted by Knuth in the second volume of `The Art of
        Computer Programming', extended to the case of series in
        more than one variable.

        Assume you want to compute the series W(x) where

            W(x) = V(x)**alpha

        Differentiation of this equation gives

            W'(x) = alpha * V(x)**alpha * V'(x) .

        You make now the ansatz

                    -----
                    \           n
            W(x) =   >      W  x  ,
                    /        n
                    -----

        substitute this into the above equation and compare
        powers of x.  This yields the recursion formula

                       n-1
                      -----
                  1   \                  m      m
           W  = -----  >    (alpha (1 - ---) - --- ) W  V     .
            n    V    /                  n      n     m  n-m
                  0   -----
                       m=0

        The first coefficient must be calculated directly, it is

           W   = V  ** alpha .
            0     0

        To use this for series in more than one variable you have to
        calculate all partial derivatives: n and m refer then to the
        corresponding component of the multi index.  Looking closely
        one finds that there is an ambiguity: the same coefficient
        can be calculated using any of the partial derivatives.  The
        only restriction is that the corresponding component of the
        multi index must not be zero, since we have to divide by it.

        We resolve this ambiguity by simply taking the first nonzero
        component.

        We use it here only for the case that alpha is a rational
        number;


symbolic procedure expttayrat(tay,rat);
  %
  % tay is a Taylor kernel, rat is a s.q. of two integers
  % value is tay ** rat
  % algorithm as quoted by Knuth
  %
  begin scalar clist,tc,tp;
    %
    % First of all we have to find out if we can raise the leading
    %  term to the power rat.
    % If so we calculate the reciprocal of this leading coefficient
    %  and multiply all other terms with it.
    % This guarantees that the resulting Taylor kernel starts with
    %  coefficient 1.
    %
    if not taylor!*p tay then return simp!* {'expt,tay,mk!*sq rat};
    tc := taycoefflist tay;
    tp := taytemplate tay;
    %
    % Find first non-zero coefficient.
    %
    while not null tc and null numr taycfsq car tc do tc := cdr tc;
    if null tc
      then if minusp car rat
             then taylor!-error!*('not!-a!-unit,'expttayrat)
            else clist := make!-cst!-coefflis(nil ./ 1,tp)
     else begin scalar c0,coefflis,l,l0,l1;
       c0 := car tc;
       l1 := for each ll in taycfpl c0 collect
          for each p in ll collect begin scalar x;
            x := divide(p * car rat,cdr rat);
            if cdr x neq 0
              then taylor!-error('branch!-point,"Taylor exponentation");
            return car x;
           end;
       l := for each ll in taycfpl c0 collect
              for each p in ll collect -p;
       tp := addto!-all!-taytpelorders(tp,get!-degreelist l);
       l := taymakecoeff(l,invsq taycfsq c0);
       %
       % We divide the rest of the kernel (without the leading term)
       %  by the leading term.
       %
       l := for each el in cdr tc collect taymultcoeffs(el,l);
       clist := expttayrat1(tp,l,rat);
       %
       % Next we multiply the resulting Taylor kernel by the leading
       %  coefficient raised to the power rat.
       %
       c0 := taymakecoeff(l1,simp!* {'expt,mk!*sq taycfsq c0,
                                     {'quotient,car rat,cdr rat}});
       clist := for each el in clist collect taymultcoeffs(el,c0);
       tp := addto!-all!-taytpelorders(tp,get!-degreelist l1);
    end;
    %
    % Finally we fill in the original expression
    %
    return make!-taylor!*(
             clist,
             tp,
             if !*taylorkeeporiginal and tayorig tay
               then simp {'expt,prepsq tayorig tay,
                          {'quotient,car rat,cdr rat}}
              else nil,
             tayflags tay)
  end;

symbolic procedure expttayrat1(tp,tcl,rat);
   begin scalar clist,coefflis,l0,rat1;
     rat1 := addsq(rat,1 ./ 1);
     %
     % Now we compute the coefficients
     %
     l0 := make!-cst!-powerlist tp;
     clist := {taymakecoeff(l0,1 ./ 1)};
     tcl := taymakecoeff(l0,1 ./ 1) . tcl;
     coefflis := makecoeffs0(tp,tpdegreelist tp);
     for each cc in cdr coefflis do
       begin scalar s,pos,pp,q,n,n1;
         s := nil ./ 1;
         pos := find!-non!-zero cc;
         n := nth(nth(cc,car pos),cdr pos);
         pp := makecoeffpairs(l0,cc,l0);
         for each p in pp do begin scalar v,w;
           v := taygetcoeff(cdr p,tcl);
           w := taygetcoeff(car p,clist);
           %
           % The following line is a short cut for efficiency.
           %
           if null numr v or null numr w then return;
           w := multsq(w,v);
           n1 := nth(nth(car p,car pos),cdr pos);
           q := quotsq(!*tayexp2q(-n1),!*tayexp2q n);
           s := addsq(s,multsq(addsq(rat,multsq(q,rat1)),w))
          end;
         if not null numr s then clist := taymakecoeff(cc,s) . clist
       end;
     return reversip clist
   end;

endmodule;


module taysubst;

%*****************************************************************
%
%      Interface to the substitution functions
%
%*****************************************************************


exports subsubtaylor$

imports

% from the REDUCE kernel:
        addsq, depends, exptsq, invsq, kernp, multsq, nth, numr,
        prepsq, reval, reversip, simp!*, subeval1, subsq, subtrsq,
        typerr,

% from the header module:
        make!-taylor!*, set!-taycfsq, taycfpl, taycfsq, taycoefflist,
        tayflags, taylor!-error, tayvars, taymakecoeff, tayorig,
        taytemplate, taytpelnext, taytpelorder, taytpelpoint,
        taytpelvars,

% from module Tayintro:
        constant!-sq!-p, delete!-nth, delete!-nth!-nth, replace!-nth,
        var!-is!-nth,

% from module Tayutils:
       enter!-sorted;


fluid '(!*taylorkeeporiginal);

put ('taylor!*, 'subfunc, 'subsubtaylor);

symbolic procedure subsubtaylor(l,v);
  begin scalar x,clist,tp;
    clist := for each u in taycoefflist v collect
               taymakecoeff(taycfpl u,subsq(taycfsq u,l));
    tp := taytemplate v;
    %
    % Substitute in expansion point
    %
    tp := for each quartet in tp collect
            {taytpelvars quartet,
             reval subeval1(l,taytpelpoint quartet),
             taytpelorder quartet,
             taytpelnext quartet};
    %
    % Make x the list of substitutions of Taylor variables.
    %
    for each p in l do
      if car p member tayvars v
        %
        % The replacement of a Taylor variable must again be
        % a kernel.  If it is a constant, we have to delete it
        % from the list of Taylor variables.  Actually the main
        % problem is to distinguish kernels that are constant
        % expressions (e.g. sin (acos (4))) from others.
        %
        then begin scalar temp;
         temp := simp!* cdr p;
         if constant!-sq!-p temp
          then begin scalar about,ll,w,y,z; integer pos,pos1;
            %
            % Determine the position of the variable
            %
            w := var!-is!-nth(tp,car p);
            pos := car w;
            pos1 := cdr w;
            %
            % Calculate the difference (new_variable - expansion_point)
            %
            about := taytpelpoint nth(tp,pos);
            if about eq 'infinity
              then if null numr temp
                then taylor!-error('zero!-denom,"Taylor Substitution")
               else temp := invsq temp
             else temp := subtrsq(temp,simp!* about);
            %
            % Substitute in every coefficient
            %
            for each cc in clist do begin scalar exponent;
              w := nth(taycfpl cc,pos);
              w := if null cdr w then delete!-nth(taycfpl cc,pos)
                    else delete!-nth!-nth(taycfpl cc,pos,pos1);
              exponent := nth(nth(taycfpl cc,pos),pos1);
              z := if exponent = 0 then taycfsq cc
                     else if exponent < 0 and null numr temp
                      then taylor!-error('zero!-denom,
                                         "Taylor Substitution")
                     else multsq(taycfsq cc,exptsq(temp,exponent));
              y := assoc(w,ll);
              if y then set!-taycfsq(y,addsq(taycfsq y,z))
               else ll := taymakecoeff(w,z) . ll
             end;
            %
            % Delete zero coefficients
            %
            clist := nil;
            while ll do <<
              if not null numr taycfsq car ll
                then clist := enter!-sorted(car ll,clist);
              ll := cdr ll>>;
            <<u := delete(car p,taytpelvars u) . cdr u;
              tp := if null car u
                      then delete!-nth(tp,pos)
                     else replace!-nth(tp,pos,u)>>
              where u := nth(tp,pos)
          end
         else if not kernp temp
          then typerr (cdr p, "Taylor variable")
         else <<
           for each el in tayvars v do
             if depends(temp,el)
               then taylor!-error('dependent!-subst,{cdr p,el});
           x := p . x>>
        end;
    x := reversip x;
    return if null tp
             then if null clist then 0 else prepsq taycfsq car clist
            else make!-taylor!*(clist,sublis(x,tp),
                        if !*taylorkeeporiginal and tayorig v
                          then subsq(tayorig v,l)
                         else nil,
                        tayflags v)
  end;

endmodule;


module taydiff;

%*****************************************************************
%
%        Differentiation of Taylor kernels
%
%*****************************************************************


exports difftaylorwrttayvar;

imports

% from the REDUCE kernel:
        !*f2q, !*n2f, diffsq, lastpair, ldepends, multsq, nth, over,

% from the header module:
        !*tay2q, make!-cst!-coefflis, make!-taylor!*, taycfpl,
        taycfsq, taycoefflist, tayflags, taylor!:, taymakecoeff,
        tayorig, taytemplate, taytpelnext, taytpelorder, taytpelpoint,
        taytpelvars, tayvars,

% from module Tayintro:
        replace!-nth, replace!-nth!-nth, var!-is!-nth,

% from module Taybasic:
        multtaylor, multtaylorsq,

% from module Taysimp:
        expttayi;


fluid '(!*taylorkeeporiginal);

put ('taylor!*, 'dfform, 'taydiffp);

symbolic procedure taydiffp(u,v,n);
  %
  % differentiates u**n w.r.t. v, u is a Taylor kernel
  % value is a standard quotient
  %
  !*tay2q
    ((if n=1 then uv
       else multtaylor(multtaylorsq(expttayi(u,n - 1),!*n2f n ./ 1),uv))
      where uv := difftaylor(u,v));

symbolic procedure difftaylor (u, kernel);
  if not ldepends(tayvars u, kernel)
    then make!-taylor!* (
                 for each cc in taycoefflist u collect
                   taymakecoeff (taycfpl cc,
                                 diffsq (taycfsq cc, kernel)),
                 taytemplate u,
                 if !*taylorkeeporiginal and tayorig u
                   then diffsq (tayorig u, kernel)
                  else nil,
                 tayflags u)
   else difftaylorwrttayvar (u, kernel)$

symbolic procedure difftaylorwrttayvar(u,kernel);
  %
  % kernel is one of the Taylor variables
  % differentiates Taylor kernel u wrt kernel
  %
  taylor!:
  begin scalar v,w,w1; integer n,m;
    v := taytemplate u;
    w := var!-is!-nth(v,kernel);
    n := car w;
    m := cdr w;
    w := for each x in taycoefflist u join <<
           w := nth(taycfpl x,n);
           w1 := nth(w,m);
           if w1 = 0 then nil
            else list
              if taytpelpoint nth(v,n) eq 'infinity
                then taymakecoeff(
                        replace!-nth!-nth(taycfpl x,n,m,w1 + 1),
                        multsq(taycfsq x,!*f2q (- w1)))
               else taymakecoeff(
                        replace!-nth!-nth(taycfpl x,n,m,w1 - 1),
                        multsq (taycfsq x,!*f2q w1))>>;
    return
      make!-taylor!*(
            if null w
              then make!-cst!-coefflis(nil ./ 1,v)
             else w,
            replace!-nth (v,n,
                          ({taytpelvars w1,
                            taytpelpoint w1,
                            if taytpelpoint w1 eq 'infinity
                              then taytpelorder w1 + 1
                             else taytpelorder w1 - 1,
                            if taytpelpoint w1 eq 'infinity
                              then taytpelnext w1 + 1
                             else taytpelnext w1 - 1}
                              where w1 := nth(v,n))),
            if !*taylorkeeporiginal and tayorig u
              then diffsq(tayorig u,kernel)
             else nil,
            tayflags u)
  end;

endmodule;


module tayconv;

%*****************************************************************
%
%     Functions converting Taylor kernels to prefix forms
%
%*****************************************************************


exports preptaylor!*!*, preptaylor!*, preptaylor!*1,
        taylor!-gen!-big!-o;

imports

% from the REDUCE kernel:
        eqcar, lastpair, prepsq!*, replus, retimes, reval,

% from the header module:
        taycfpl, taycfsq, taycoefflist, taytemplate, taytpelnext,
        taytpelpoint, taytpelvars;


fluid '(convert!-taylor!*
        taylorprintterms
        taylor!-truncation!-flag);


symbolic procedure preptaylor!*1 (coefflist, template, no!-of!-terms);
  replus for each cc in coefflist join
    begin scalar x; integer count;
      if taylor!-truncation!-flag then return nil;
      x := preptaylor!*2 (cc, template);
      if null x or null no!-of!-terms then return x;
      no!-of!-terms := no!-of!-terms - 1;
      if no!-of!-terms < 0
        then << taylor!-truncation!-flag := t;
                return nil >>;
      return x
    end;

symbolic procedure preptaylor!*2 (coeff, template);
  (lambda (pc);
    if pc = 0 then nil
     else {retimes (
            (if eqcar (pc, 'quotient) and eqcar (cadr pc, 'minus)
               then {'minus, {'quotient, cadr cadr pc, caddr pc}}
              else pc) . preptaycoeff (taycfpl coeff, template))})
    (prepsq!* taycfsq coeff);


symbolic procedure checkdifference (var, var0);
  if var0 = 0 then var else {'difference, var, var0};

symbolic procedure checkexp (bas, exp);
  if exp = 0 then 1
   else if exp = 1 then bas
   else {'expt, bas, exp};

symbolic smacro procedure checkpower (var, var0, n);
  if var0 eq 'infinity
    then if n = 0 then 1
          else {'quotient, 1, checkexp (var, n)}
   else checkexp (checkdifference (var, reval var0), n);

symbolic procedure preptaycoeff (cc, template);
  begin scalar result;
    while not null template do begin scalar ccl;
      ccl := car cc;
      for each var in taytpelvars car template do <<
        result := checkpower (var, taytpelpoint car template, car ccl)
                    . result;
        ccl := cdr ccl >>;
      cc := cdr cc;
      template := cdr template
    end;
    return result
  end;

put ('taylor!*, 'prepfn2, 'preptaylor!*!*);

symbolic procedure preptaylor!*!* u;
   if null convert!-taylor!* then u else preptaylor!* u;

symbolic procedure preptaylor!* u;
   preptaylor!*1 (taycoefflist u, taytemplate u, nil);

symbolic procedure taylor!-gen!-big!-o tp;
  %
  % Generates a big-O notation for the Taylor template tp
  %
  "O" . for each el in tp collect
          if null cdr taytpelvars el
            then checkpower(car taytpelvars el,taytpelpoint el,
                            taytpelnext el)
           else begin scalar var0;
             var0 := reval taytpelpoint el;
             return
               if var0 eq 'infinity
                 then {'quotient,1,
                       checkexp('list . taytpelvars el,taytpelnext el)}
                else checkexp(
                 'list .
                   for each krnl in taytpelvars el collect
                     checkdifference(krnl,var0),
                 taytpelnext el)
           end;

endmodule;


module tayprint$

%*****************************************************************
%
%     Functions for printing Taylor kernels
%
%*****************************************************************


exports taylor!*print$

imports

% from the REDUCE kernel:
        denr, eqcar, fmprint, kernp, lastpair, maprint, mvar, numr,
        prepsq, simp!*, typerr,

% from the header module:
        taycfsq, taycoefflist, tayorig, taytemplate, taytpelorder,
        taytpelpoint, taytpelvars,

% from module Tayconv:
        preptaylor!*, preptaylor!*1, taylor!-gen!-big!-o;


fluid '(!*fort !*nat !*taylorprintorder taylor!-truncation!-flag
        taylorprintterms);

symbolic procedure check!-print!-terms u;
  begin scalar x;
    x := simp!* u;
    if kernp x and mvar numr x eq 'all then return nil
     else if denr x = 1 and fixp numr x then return numr x
     else typerr (x, "value of TaylorPrintTerms")
  end;


symbolic procedure taylor!*print1 u;
  begin scalar taylor!-truncation!-flag, prepexpr, rest, nterms;
    nterms := if !*taylorprintorder
                then check!-print!-terms taylorprintterms
               else nil;
    prepexpr := preptaylor!*1 (
                  taycoefflist u,
                  taytemplate u,
                  nterms);
    if !*taylorprintorder then <<
      rest := {taylor!-gen!-big!-o taytemplate u};
      if taylor!-truncation!-flag then begin integer notprinted;
           notprinted := -nterms;
           for each pp in taycoefflist u do
             if not null numr taycfsq pp then
               notprinted := notprinted + 1;
           if notprinted=1 then rest := "(1 term)" . rest
            else rest := compress append('(!" !(),
                           nconc(explode notprinted,
                                 '(!  !t !e !r !m !s !) !"))) . rest
        end>>
     else rest := {'!.!.!.};
    return if not eqcar (prepexpr, 'plus)
             then 'plus . (prepexpr or 0) . rest
            else nconc (prepexpr, rest)
  end;


symbolic procedure taylor!*print(u,p);
  if !*fort then fmprint(preptaylor!* u,0)
   else if null !*nat then maprint(
                     'taylor .
                        (if tayorig u
                           then prepsq tayorig u
                          else preptaylor!* u) .
                        for each el in taytemplate u join
                          {if null cdr taytpelvars el
                             then car taytpelvars el
                            else 'list . taytpelvars el,
                           taytpelpoint el,
                           taytpelorder el},
                     p)
   else maprint(taylor!*print1 u,p);

put ('taylor!*, 'pprifn, 'taylor!*print);


Comment We need another printing function for use with the 
        TeX-REDUCE interface; %not yet done;


endmodule;


module tayfrontend;

%*****************************************************************
%
%          User interface
%
%*****************************************************************


exports taylorcombine, taylororiginal, taylorprintorder,
        taylorseriesp, taylortemplate, taylortostandard;

imports

% from the REDUCE kernel:
        eqcar, mk!*sq, mvar, numr, prepsq, simp!*, typerr,

% from the header module:
        taylor!-kernel!-sq!-p, tayorig, taytemplate, taytpelorder,
        taytpelpoint, taytpelvars,

% from module Tayintro:
        taylor!-error,

% from module Taysimp:
        taysimpsq;


symbolic procedure taylorseriesp u;
  (taylor!-kernel!-sq!-p sq)
      where sq := simp!* u;

symbolic procedure taylorcombine u;
  mk!*sq taysimpsq simp!* u;

symbolic procedure taylortostandard u;
  (prepsq if not eqcar (u, '!*sq) then simp!* u else cadr u)
          where convert!-taylor!* := t;

symbolic procedure taylororiginal u;
  (if not taylor!-kernel!-sq!-p sq
     then typerr (u, "Taylor kernel")
    else (if tayorig tay then mk!*sq tayorig tay
        else taylor!-error ('no!-original, 'taylororiginal))
           where tay := mvar numr sq)
     where sq := simp!* u;

symbolic procedure taylortemplate u;
  (if not taylor!-kernel!-sq!-p sq
     then typerr (u, "Taylor kernel")
    else 'list . for each quartet in taytemplate mvar numr sq collect
              {'list,
               if null cdr taytpelvars quartet
                 then car taytpelvars quartet
                else 'list . taytpelvars quartet,
               taytpelpoint quartet,
               taytpelorder quartet})
     where sq := simp!* u;

flag ('(taylorseriesp taylorcombine taylortostandard taylororiginal
        taylortemplate),
      'opfn);

flag ('(taylorseriesp), 'boolean);

endmodule;


module tayfns;

%*****************************************************************
%
%       Simplification functions for special functions
%
%*****************************************************************


exports taysimpexpt, taysimpatan, taysimplog, taysimpexp,
        taysimptan, taysimpsin, taysimpsinh, taysimpasin;

imports

% from the REDUCE kernel: 
        !*f2q, addsq, aeval, denr, domainp, eqcar, evenp, freeof,
        invsq, lastpair, lprim, kernp, mk!*sq, mksq, multsq, mvar,
        negsq, neq, nth, numr, over, prepd, prepsq, quotsq, retimes,
        reval, reversip, simp, simp!*, simplogi, subs2, subsq,
        subtrsq,

% from the header module:
        !*tay2q, !*tayexp2q, constant!-sq!-p, cst!-taylor!*,
        find!-non!-zero, get!-degree, get!-degreelist, has!-tayvars,
        make!-cst!-powerlist, make!-cst!-coefflis, make!-taylor!*,
        multintocoefflist, set!-taycoefflist, set!-tayflags,
        set!-tayorig, taycfpl, taycfsq, taycoefflist, tayflags,
        taygetcoeff, taylor!*p, taylor!-kernel!-sq!-p, taylor!:,
        taymakecoeff, taymultcoeffs, tayorig, taytemplate,
        taytpelnext, taytpelorder, taytpelpoint, taytpelvars,
        taytpvars, tayvars, tpdegreelist,

% from the module Tayintro:
        confusion, delete!-nth, delete!-nth!-nth, replace!-nth,
        replace!-nth!-nth, taylor!-error, taylor!-error!*,
        var!-is!-nth,

% from the module Tayutils:
        addto!-all!-taytpelorders, get!-cst!-coeff, is!-neg!-pl,
        subtr!-degrees, taylor!*!-constantp, taylor!*!-zerop,

% from the module Taybasic:
        addtaylor, addtaylor1, invtaylor, invtaylor1, makecoeffs0,
        makecoeffpairs, makecoeffpairs1, multtaylor,
        multtaylor!-as!-sq, multtaylorsq, negtaylor, quottaylor,

% from the module TayExpnd:
        taylorexpand,

% from the module Taysimp:
        expttayrat, expttayrat1, taysimpsq, taysimpsq!*,

% from the module Taydiff:
        difftaylorwrttayvar,

% from the module TayConv:
        preptaycoeff, preptaylor!*,

% from the module Tayfrontend:
        taylorcombine, taylortostandard;


fluid '(!*taylorkeeporiginal !*!*taylor!-epsilon!*!* frlis!*);

symbolic procedure taysimpexpt u;
  %
  % Argument is of the form ('expt base exponent)
  % where both base and exponent (but a least one of them)
  % may contain Taylor kernels given as prefix forms.
  % Value is the equivalent Taylor kernel.
  %
  if not (car u eq 'expt) or cdddr u then confusion 'taysimpexpt
   else if cadr u eq 'e then taysimpexp {'exp, caddr u}
   else begin scalar bas, expn;
     bas := taysimpsq simp!* cadr u;
     expn := taysimpsq simp!* caddr u;
     if constant!-sq!-p bas then return taysimpexp
         {'exp,mk!*sq multsq(simp!*{'log,mk!*sq bas},expn)};
     if null kernp bas
       then if not denr bas = 1
              then return mksq({'expt,prepsq bas,prepsq expn},1)
             else if domainp numr bas
              then return taysimpexp {'exp,
                  prepsq multsq(simp!* {'log,prepd numr bas},expn)}
             else return mksq({'expt,prepsq bas,prepsq expn},1);
     if fixp numr expn and fixp denr expn
       then return expttayrat!*(mvar numr bas,expn);
     if denr expn = 1 and eqcar(numr expn,'!:rn!:)
       then return expttayrat!*(mvar numr bas,cdr numr expn);
     bas := mvar numr bas;
     return
       if taylor!*p bas
         then if taylor!-kernel!-sq!-p expn
                then if taytemplate bas = taytemplate mvar numr expn
                       then taysimpexp {'exp,
                                        mk!*sq taysimpsq
                                          multtaylor!-as!-sq(
                                              expn,
                                              taysimplog {'log,bas})}
                      else mksq({'expt,bas,mvar numr expn},1)
               else if not has!-tayvars(bas,expn)
                then begin scalar logterm;
                  logterm := taysimplog{'log,bas};
                  return
                    if taylor!-kernel!-sq!-p logterm
                      then taysimpexp{'exp,
                                      multtaylorsq(mvar numr logterm,
                                                   expn)}
                     else taysimpsq
                            simp!* {'exp,mk!*sq multsq(logterm,expn)}
                 end
               else mksq({'expt,bas,mk!*sq expn},1)
        else if taylor!-kernel!-sq!-p expn
               then if not has!-tayvars(mvar numr expn,bas)
                      then taysimpexp{'exp,
                                      multtaylorsq(mvar numr expn,
                                                   simp!*{'log,bas})}
                     else if taylor!*!-constantp mvar numr expn
                      then taylorexpand(
                             simp!* {'expt,bas,
                                     preptaylor!* mvar numr expn},
                             taytemplate mvar numr expn)
                     else mksq({'expt,bas,mk!*sq expn},1)
        else mksq({'expt,bas,mk!*sq expn},1);
  end;

put('expt,'taylorsimpfn,'taysimpexpt);

symbolic procedure expttayrat!*(tay,rat);
  %
  % tay is a Taylor kernel, rat is a s.q. of two integers
  % value is tay ** rat
  % this function separates non-analytical terms and proceeds
  % to compute the analytical part like expttayrat does.
  %
  begin scalar tc,tp,x,csing;
    %
    % First of all we have to find out if we can raise the leading
    %  term to the power rat.
    % If so we calculate the reciprocal of this leading coefficient
    %  and multiply all other terms with it.
    % This guarantees that the resulting Taylor kernel starts with
    %  coefficient 1.
    %
    if not taylor!*p tay then return simp!* {'expt,tay,mk!*sq rat};
    tc := taycoefflist tay;
    tp := taytemplate tay;
    %
    % Find first non-zero coefficient.
    %
    while not null tc and null numr taycfsq car tc do tc := cdr tc;
    if null tc
      then if minusp car rat
             then taylor!-error('not!-a!-unit,'expttayrat)
            else x := make!-cst!-coefflis(nil ./ 1,tp)
     else begin scalar c0,clist,coefflis,flg,l,l0,l1,l2;
       c0 := car tc;
       l1 := for each ll in taycfpl c0 collect begin scalar tmp,l3;
          tmp := for each p in ll collect <<
            x := divide(p * car rat,cdr rat);
            if cdr x neq 0 then flg := t;
            l3 := cdr x . l3;
            car x>>;
          l2 := reversip l3 . l2;
          return tmp
          end;
       l2 := reversip l2;
       if flg
         then <<tc := for each pp in tc collect
                        taymakecoeff(subtr!-degrees(taycfpl pp,l2),
                                     taycfsq pp);
                tp := addto!-all!-taytpelorders(
                        tp,
                        for each nl in l2 collect - get!-degree nl);
                csing := simp!* {'expt,
                                 preptaylor!*
                                   make!-taylor!*(
                                     {taymakecoeff(l2,1 ./ 1)},
                                     tp,nil,nil),
                                 mk!*sq rat}>>;
       c0 := car tc;
       l := for each ll in taycfpl c0 collect
              for each p in ll collect -p;
       tp := addto!-all!-taytpelorders(tp,get!-degreelist l);
       l := taymakecoeff(l,invsq taycfsq c0);
       %
       % We divide the rest of the kernel (without the leading term)
       %  by the leading term.
       %
       l := for each el in cdr tc collect taymultcoeffs(el,l);
       clist := expttayrat1(tp,l,rat);
       %
       % Next we multiply the resulting Taylor kernel by the leading
       %  coefficient raised to the power rat.
       %
       c0 := taymakecoeff(l1,simp!* {'expt,mk!*sq taycfsq c0,
                                     {'quotient,car rat,cdr rat}});
       x := for each el in clist collect taymultcoeffs(el,c0);
       tp := addto!-all!-taytpelorders(tp,get!-degreelist l1);
    end;
    %
    % Finally we fill in the original expression
    %
    tay := make!-taylor!*(
             x,
             tp,
             if !*taylorkeeporiginal and tayorig tay
               then simp {'expt,prepsq tayorig tay,
                          {'quotient,car rat,cdr rat}}
              else nil,
             tayflags tay);
    if null csing then return !*tay2q tay
     else if taylor!*!-onep tay then return csing;
    if !*taylorkeeporiginal and tayorig tay
      then set!-tayorig(tay,quotsq(tayorig tay,csing));
    return multsq(!*tay2q tay,csing)
  end;


symbolic procedure taycoefflist!-union u;
  if null cdr u then car u
   else taycoefflist!-union2 (car u, taycoefflist!-union cdr u)$

symbolic procedure taycoefflist!-union2 (x, y);
  %
  % returns union of TayCoeffLists x and y
  %
  << for each w in y do
       if null (assoc (car w, x)) then x := w . x;
     x >>$

symbolic procedure inttaylorwrttayvar(tay,var);
  %
  % integrates Taylor kernel tay wrt variable var
  %
  inttaylorwrttayvar1(taycoefflist tay,taytemplate tay,var)$

symbolic procedure inttaylorwrttayvar1(tcl,tp,var);
  %
  % integrates Taylor kernel with TayCoeffList tcl and template tp
  %  wrt variable var
  %
  taylor!:
  begin scalar tt,u,w,singlist,csing; integer n,n1,m;
    u := var!-is!-nth(tp,var);
    n := car u;
    n1 := cdr u;
    tt := nth(tp,n);
    u := for each cc in tcl join <<
           m := nth(nth(taycfpl cc,n),n1);
           if taytpelpoint nth(tp,n) eq 'infinity
             then <<
               if m=1 then <<singlist :=
                               taymakecoeff(
                                 delete!-nth!-nth(taycfpl cc,n,n1),
                                 taycfsq cc) . singlist;nil>>
                else {taymakecoeff(
                        replace!-nth!-nth(taycfpl cc,n,n1,m-1),
                        multsq(taycfsq cc,invsq !*tayexp2q(-m + 1)))}>>
            else <<
               if m=-1 then <<singlist :=
                                taymakecoeff(
                                  delete!-nth!-nth(taycfpl cc,n,n1),
                                  taycfsq cc) . singlist;nil>>
               else {taymakecoeff(
                       replace!-nth!-nth(taycfpl cc,n,n1,m+1),
                       multsq(taycfsq cc,invsq !*tayexp2q(m + 1)))}>>>>;
    w := {taytpelvars tt,taytpelpoint tt,
          if var member taytpelvars tt
            then if taytpelpoint tt eq 'infinity
                   then taytpelorder tt - 1
                  else taytpelorder tt + 1
           else taytpelorder tt,
          if var member taytpelvars tt
            then if taytpelpoint tt eq 'infinity
                   then taytpelnext tt - 1
                  else taytpelnext tt + 1
           else taytpelorder tt};
    if singlist then begin scalar tpel;
        tpel := nth(tp,n);
        singlist := reversip singlist;
        if taycfpl car singlist = '(nil) % no Taylor left
          then csing := taycfsq car singlist
         else csing := !*tay2q
                         make!-taylor!*(
                           singlist,
                           replace!-nth(
                             tp,n,
                             {delete!-nth(taytpelvars tpel,n1),
                              taytpelpoint tpel,
                              taytpelorder tpel,
                              taytpelnext tpel}),
                           nil,nil);
        csing := multsq(csing,simp!* {'log,nth(taytpelvars tpel,n1)})
       end;

    return (csing . make!-taylor!*(u,replace!-nth(tp,n,w),nil,nil))
%
% The following is not needed yet
%
%     return make!-Taylor!*(
%              u,
%              replace!-nth(TayTemplate tay,n,w),
%              if !*taylorkeeporiginal and TayOrig tay
%                then simp {'int,mk!*sq TayOrig tay,var}
%               else nil,
%              TayFlags u)
  end;


Comment The inverse trigonometric and inverse hyperbolic functions
        of a Taylor kernel are calculated by first computing the
        derivative(s) with respect to the Taylor variable(s) and
        integrating the result.  The derivatives can easily be
        calculated by the manipulation functions defined above.

        The method is best illustrated with an example.  Let T(x)
        be a Taylor kernel depending on one variable x.  To compute
        the inverse tangent T1(x) = atan(T(x)) we calculate the
        derivative

                                T'(x)
                    T1'(x) = ----------- .
                                      2
                              1 + T(x)

        (If T and T1 depend on more than one variable replace
        the derivatives by gradients.)
        This is integrated again with the integration constant
        T1(x0) = atan(T(x0)) yielding the desired result.
        If there is more than variable we have to find the
        potential function T1(x1,...,xn) corresponding to
        the vector grad T1(x1,...,xn) which is always possible
        by construction.

        The prescriptions for the eight functions asin, acos, asec,
        acsc, asinh, acosh, asech, and acsch can be put together
        in one procedure since the expressions for their derivatives
        differ only in certain signs.  The same remark applies to
        the four functions atan, acot, atanh, and acoth.

        These expressions are:

         d                 1
         -- asin x = ------------- ,
         dx           sqrt(1-x^2)

         d                -1
         -- acos x = ------------- ,
         dx           sqrt(1-x^2)

         d                 1
         -- asinh x = ------------- ,
         dx            sqrt(1+x^2)

         d                 1
         -- acosh x = ------------- ,
         dx            sqrt(x^2-1)

         d               1
         -- atan x = --------- ,
         dx           1 + x^2

         d               -1
         -- acot x = --------- ,
         dx           1 + x^2

         d                1
         -- atanh x = --------- ,
         dx            1 - x^2

         d                1
         -- acoth x = --------- ,
         dx            1 - x^2

        together with the relations

                       1
         asec x = acos - ,
                       x

                       1
         acsc x = asin - ,
                       x

                         1
         asech x = acosh - ,
                         x

                         1
         acsch x = asinh -
                         x .


        This method has one drawback: it is applicable only when T(x0)
        is a regular point of the function in question. E.g., if T(x0)
        = 0, then asech(T(x)) cannot be calculated by this method, as
        asech has a logarithmic singularity at 0. This means that the
        constant term of the series cannot be determined by computing
        asech(T(x0)).  In that case, we use the following relations
        instead:


         asin z = -i log(i z + sqrt(1 - z^2)),


         acos z = -i log(z + sqrt(z^2 - 1)),

                    1        1 + i z
         atan z = ----- log ---------,
                   2 i       1 - i z

                   -1        i z + 1
         acot z = ----- log ---------,
                   2 i       i z - 1


         asinh z = log(z + sqrt(1 + z^2)),


         acosh z = log(z + sqrt(z^2 - 1)),

                    1       1 + z
         atanh z = --- log -------,
                    2       1 - z

                    1       z + 1
         acoth z = --- log -------.
                    2       z - 1


         These formulas are applied at the following points:

           infinity for all functions,

           +i/-i for atan and acot,

           +1/-1 for atanh and acoth.


         There are still some branch points, where the calculation is
         not always possible:

           +1/-1 for asin and acos, and consequently for asec and acsc,

           +i/-i for asinh, acosh, asech and acsch.

         In these cases, the above formulas are applied as well, but
         simplification of the square roots and the logarithm may lead
         to a rather complicated result;



symbolic procedure taysimpasin u;
  if not (car u memq '(asin acos acsc asec asinh acosh acsch asech))
     or cddr u
    then confusion 'taysimpasin
   else taylor!:
     begin scalar l,l0,c0,v,tay,tay2,tp,singlist;
     tay := taysimpsq simp!* cadr u;
     if not taylor!-kernel!-sq!-p tay
       then return mksq({car u,mk!*sq tay},1);
     tay := mvar numr tay; % asin's argument
     if car u memq '(asec acsc asech acsch) then tay := invtaylor tay;
     tp := taytemplate tay;
     l0 := make!-cst!-powerlist tp;
     l := taycoefflist tay;
     while not null l and null numr taycfsq car l do l := cdr l;
     if null l then return !*tay2q cst!-taylor!*(simp!* {car u,0},tp);
     if is!-neg!-pl taycfpl car l then return taysimpasin!*(car u,tay);
     tay2 := multtaylor(tay,tay);
     if car u memq '(asin acos acsc asec)
       then tay2 := negtaylor tay2;
     tay2 := addtaylor(
               cst!-taylor!*(
                 !*f2q(if car u memq '(acosh asech) then -1 else 1),
                 tp),
               tay2);
     if taylor!*!-zerop tay2
       then taylor!-error!*('essential!-singularity,car u)
      else if null numr taygetcoeff(l0,taycoefflist tay2)
       then return taysimpasin!*(car u,tay);

     tay2 := invtaylor expttayrat(tay2,1 ./ 2);
     if car u eq '(acos asec) then tay2 := negtaylor tay2;
     l := for each krnl in tayvars tay collect
            inttaylorwrttayvar(
              multtaylor(difftaylorwrttayvar(tay,krnl),tay2),
              krnl);
     v := taycoefflist!-union
            for each pp in l collect taycoefflist cdr pp;
     singlist := nil ./ 1;
     for each pp in l do
       if car pp then singlist := addsq(singlist,car pp);
     %
     % special treatment for zeroth coefficient
     %
     c0 := taygetcoeff(l0,taycoefflist tay);
     c0 := simp {car u,mk!*sq c0};
     v := taymakecoeff(l0,c0) . v;
     tay := make!-taylor!*(
              v,
              tp,
              if !*taylorkeeporiginal and tayorig tay
                then simp {car u,mk!*sq tayorig tay}
               else nil,
              tayflags tay);
     if null numr singlist then return !*tay2q tay;
     if !*taylorkeeporiginal and tayorig tay
       then set!-tayorig(tay,subtrsq(tayorig tay,singlist));
     return addsq(singlist,!*tay2q tay)
  end;

symbolic procedure taysimpasin!*(fn,tay);
   begin scalar result,tay1;
     if fn memq '(asin acsc)
       then tay := multtaylorsq(tay,simp 'i);
     tay1 := cst!-taylor!*(
               (if fn memq '(asin asinh acsc acsch)
                  then 1
                 else -1) ./ 1,
                taytemplate tay);
     result := taysimplog {'log,
                           mk!*sq addtaylor!-as!-sq(
                             expttayrat!*(addtaylor(multtaylor(tay,tay),
                                                  tay1),
                                          1 ./ 2),
                             !*tay2q tay)};
     if fn memq '(asin acos asec acsc)
       then result := quotsq(result,simp 'i);
     return taysimpsq!* result
   end;


put('asin,'taylorsimpfn,'taysimpasin);
put('acos,'taylorsimpfn,'taysimpasin);
put('asec,'taylorsimpfn,'taysimpasin);
put('acsc,'taylorsimpfn,'taysimpasin);
put('asinh,'taylorsimpfn,'taysimpasin);
put('acosh,'taylorsimpfn,'taysimpasin);
put('asech,'taylorsimpfn,'taysimpasin);
put('acsch,'taylorsimpfn,'taysimpasin);


symbolic procedure taysimpatan u;
  if not (car u memq '(atan acot atanh acoth)) or cddr u
    then confusion 'taysimpatan
   else begin scalar l,l0,c0,v,tay,tay2,tp,singlist;
     tay := taysimpsq simp!* cadr u;
     if not taylor!-kernel!-sq!-p tay
       then return mksq({car u,mk!*sq tay},1);
     tay := mvar numr tay; % atan's argument
     tp := taytemplate tay;
     l0 := make!-cst!-powerlist tp;
     l := taycoefflist tay;
     while not null l and null numr taycfsq car l do l := cdr l;
     if null l then return !*tay2q cst!-taylor!*(simp!* {car u,0},tp);
     if is!-neg!-pl taycfpl car l then return taysimpatan!*(car u,tay);
     c0 := get!-cst!-coeff tay;
     if car u memq '(atan acot)
       then c0 := subs2 multsq(c0,simp 'i) where !*sub2 := !*sub2;
     if c0 = (1 ./ 1) or c0 = (-1 ./ 1)
       then return taysimpatan!*(car u,tay);
     tay2 := multtaylor(tay,tay);
     if car u memq '(atanh acoth) then tay2 := negtaylor tay2;
     tay2 := invtaylor addtaylor(cst!-taylor!*(1 ./ 1,tp),tay2);
     if car u eq 'acot then tay2 := negtaylor tay2;
     l := for each krnl in tayvars tay collect
            inttaylorwrttayvar(
              multtaylor(difftaylorwrttayvar(tay,krnl),tay2),
              krnl);
     v := taycoefflist!-union
            for each pp in l collect taycoefflist cdr pp;
     singlist := nil ./ 1;
     for each pp in l do
       if car pp then singlist := addsq(singlist,car pp);
     %
     % special treatment for zeroth coefficient
     %
     c0 := simp {car u,
                 mk!*sq taygetcoeff(l0,taycoefflist tay)};
     v := taymakecoeff (l0,c0) . v;
     tay := make!-taylor!*(
              v,
              tp,
              if !*taylorkeeporiginal and tayorig tay
                then simp {car u,mk!*sq tayorig tay}
               else nil,
              tayflags tay);
     if null numr singlist then return !*tay2q tay;
     if !*taylorkeeporiginal and tayorig tay
       then set!-tayorig(tay,subtrsq(tayorig tay,singlist));
     return addsq(singlist,!*tay2q tay)
  end;

symbolic procedure taysimpatan!*(fn,tay);
   begin scalar result,tay1;
     if fn memq '(atan acot)
       then tay := multtaylorsq(tay,simp 'i);
     tay1 := cst!-taylor!*(1 ./ 1,taytemplate tay);
     tay := quottaylor(addtaylor(tay1,tay),
                       addtaylor(tay1,negtaylor tay));
     result := multsq(taysimplog {'log,tay},1 ./ 2);
     if fn eq 'atan then result := quotsq(result,simp 'i)
      else if fn eq 'acot then result := multsq(result,simp 'i);
     return taysimpsq!* result
   end;



put('atan,'taylorsimpfn,'taysimpatan);
put('acot,'taylorsimpfn,'taysimpatan);
put('atanh,'taylorsimpfn,'taysimpatan);
put('acoth,'taylorsimpfn,'taysimpatan);


Comment For the logarithm and exponential we use the extension of
        an algorithm quoted by Knuth who shows how to do this for
        series in one expansion variable.

        We extended this to the case of several variables which is
        straightforward except for one point, see below.
        Knuth's algorithm works as follows: Assume you want to compute
        the series W(x) where

            W(x) = log V(x)

        Differentiation of this equation gives

                    V'(x)
            W'(x) = ----- ,   or V'(x) = W'(x)V(x) .
                     V(x)

        You make now the ansatz

                    -----
                    \           n
            W(x) =   >      W  x  ,
                    /        n
                    -----

        substitute this into the above equation and compare
        powers of x.  This yields the recursion formula

                               n-1
                 V            -----
                  n       1   \
           W  = ---- - ------  >    m W  V     .
            n    V      n V   /        m  n-m
                  0        0  -----
                               m=0

        The first coefficient must be calculated directly, it is

           W   = log V  .
            0         0

        To use this for series in more than one variable you have to
        calculate all partial derivatives: n and m refer then to the
        corresponding component of the multi index.  Looking closely
        one finds that there is an ambiguity: the same coefficient
        can be calculated using any of the partial derivatives.  The
        only restriction is that the corresponding component of the
        multi index must not be zero, since we have to divide by it.

        We resolve this ambiguity by simply taking the first nonzero
        component.

        The case of the exponential is nearly the same: differentiation
        gives

            W'(x) = V'(x) W(x) ,

        from which we derive the recursion formula

                   n-1
                  -----
                  \     n-m
            W  =   >    --- W  V     , W  = exp V  .
             n    /      m   m  n-m     0        0
                  -----
                   m=0

        ;


symbolic procedure taysimplog u;
  %
  % Special Taylor expansion function for logarithms
  %
  if not (car u eq 'log) or cddr u then confusion 'taysimplog
   else taylor!:
    begin scalar a0,clist,coefflis,l0,l,tay,tp,csing,singterm;
    u := simplogi cadr u;
    if not kernp u then return taysimpsq u;
    u := mvar numr u;
    if not (car u eq 'log) then confusion 'taysimplog;
    u := taysimpsq simp!* cadr u;
    if not taylor!-kernel!-sq!-p u then return mksq({'log,mk!*sq u},1);
    tay := mvar numr u;
    tp := taytemplate tay;
    l0 := make!-cst!-powerlist tp;
    %
    % The following relies on the standard ordering of the
    % TayCoeffList.
    %
    l := taycoefflist tay;
    while not null l and null numr taycfsq car l do l := cdr l;
    if null l then taylor!-error!*('not!-a!-unit,'taysimplog);
    %
    % The assumption at this point is that the first term is the one
    % with lowest degree, i.e. dividing by this term yields a series
    % which starts with a constant term.
    %
    if taycfpl car l neq l0 then
      <<csing := taycfpl car l;
        l := for each pp in l collect
               taymakecoeff(subtr!-degrees(taycfpl pp,csing),
                            taycfsq pp);
        tp := addto!-all!-taytpelorders(
                tp,
                for each nl in csing collect
                  - get!-degree nl)>>;
    a0 := taygetcoeff(l0,l);
    clist := {taymakecoeff(l0,simplogi mk!*sq a0)};
    coefflis := makecoeffs0(tp,tpdegreelist tp);
    for each cc in cdr coefflis do
      begin scalar s,pos,pp,n,n1;
        s := nil ./ 1;
        pos := find!-non!-zero cc;
        n := nth(nth(cc,car pos),cdr pos);
        pp := makecoeffpairs(l0,cc,taycfpl car l);
        for each p in pp do <<
          n1 := nth(nth(car p,car pos),cdr pos);
          s := addsq(s,
                     multsq(!*tayexp2q n1,
                            multsq(taygetcoeff(car p,clist),
                                   taygetcoeff(cdr p,l))))>>;
%        for each p in pp addsq
%          multsq(!*TayExp2q nth(nth(car p,car pos),cdr pos),
%                 multsq(TayGetCoeff(car p,clist),
%                        TayGetCoeff(cdr p,l)));
        s := subtrsq(taygetcoeff(cc,l),quotsq(s,!*tayexp2q n));
        if not null numr s
          then clist := taymakecoeff(cc,quotsq(s,a0)) . clist;
      end;
    tay := make!-taylor!*(
             reversip clist,
             tp,
             if !*taylorkeeporiginal and tayorig tay
               then simplogi mk!*sq tayorig tay
              else nil,
             tayflags tay);
    if null csing then return !*tay2q tay;
    singterm := simplogi retimes preptaycoeff(csing,tp);
    if taylor!*!-zerop tay then return singterm;
    if !*taylorkeeporiginal and tayorig tay
      then set!-tayorig(tay,subtrsq(tayorig tay,singterm));
    return addsq(singterm,!*tay2q tay)
  end;

put('log,'taylorsimpfn,'taysimplog);


symbolic procedure taysimpexp u;
  %
  % Special Taylor expansion function for exponentials
  %
  if not (car u eq 'exp) or cddr u then confusion 'taysimpexp
   else taylor!:
    begin scalar a0,clist,coefflis,l0,l,lm,lp,tay,tp;
    u := taysimpsq simp!* cadr u;
    if not taylor!-kernel!-sq!-p u
      then return mksq ({'exp, mk!*sq u},1);
    tay := mvar numr u;
    tp := taytemplate tay;
    l0 := make!-cst!-powerlist tp;
    %
    % The following relies on the standard ordering of the
    % TayCoeffList.
    %
    l := taycoefflist tay;
    while l and null numr taycfsq car l do l := cdr l;
    if null l then return !*tay2q cst!-taylor!*(1 ./ 1,tp);
    for each pp in l do
      if is!-neg!-pl taycfpl pp then lm := pp . lm
       else if not null numr taycfsq pp then lp := pp . lp;
    lm := reversip lm;
    l := reversip lp;

    if lm
      then lm := simp!* {'exp,
                         preptaylor!* make!-taylor!*(lm,tp,nil,nil)};

    if null l then return lm;

    a0 := taygetcoeff(l0,l);
    clist := {taymakecoeff(l0,simp!* {'exp,mk!*sq a0})};
    coefflis := makecoeffs0(tp,tpdegreelist tp);
    for each cc in cdr coefflis do
      begin scalar s,pos,pp,n,n1;
        s := nil ./ 1;
        pos := find!-non!-zero cc;
        n := nth(nth(cc,car pos),cdr pos);
        pp := makecoeffpairs(l0,cc,l0);
        for each p in pp do <<
          n1 := nth(nth(car p,car pos),cdr pos);
          s := addsq(s,
                     multsq(!*tayexp2q(n - n1),
                            multsq(taygetcoeff(car p,clist),
                                   taygetcoeff(cdr p,l))))>>;
        s := subs2 quotsq(s,!*tayexp2q n) where !*sub2 := !*sub2;
        if not null numr s then clist := taymakecoeff(cc,s) . clist
      end;

    clist := reversip clist;
%    clist := truncate!-Taylor1(clist,tp,tp);
%    tp := cdr clist; clist := car clist;

    u := !*tay2q
           make!-taylor!*(
             clist,
             tp,
             if !*taylorkeeporiginal and tayorig tay
               then simp {'exp,mk!*sq tayorig tay}
              else nil,
             tayflags tay);
    return if null lm then u else multsq(u,lm)
  end;

put('exp,'taylorsimpfn,'taysimpexp);


Comment The algorithm for the trigonometric functions is also
        derived from their differential equation.
        The simplest case is that of tangent whose equation is

                            2
           tan'(x) = 1 + tan (x) .          (*)

        For the others we have

                               2
           cot'(x) = - (1 + cot (x)),

                              2
           tanh'(x) = 1 - tanh (x),

                              2
           coth'(x) = 1 - coth (x) .



        Let T(x) be a Taylor series,

                  -----
                  \         N
           T(x) =  >    a  x
                  /      N
                  -----
                   N=0

        Now, let

                              -----
                              \         N
           T1(x) = tan T(x) =  >    b  x
                              /      N
                              -----
                               N=0

        from which we immediately deduce that b  = tan a .
                                               0        0

        From (*) we get
                              2
           T1'(x) = (1 + T1(x) ) T'(x) ,

        or, written in terms of the series:

        Inserting this into (*) we get

          -----              /     /  -----       \ 2 \  -----
          \           N-1    |     |  \         N |   |  \           L
           >    N b  x    =  | 1 + |   >    b  x  |   |   >    L a  x
          /        N         |     |  /      N    |   |  /        L
          -----              \     \  -----       /   /  -----
           N=1                         N=0                L=1

        We perform the square on the r.h.s. using Cauchy's rule
        and obtain:


           -----
           \           N-1
            >    N b  x    =
           /        N
           -----
            N=1

                              N
               /     -----  -----            \  -----
               |     \      \              N |  \           L
               | 1 +  >      >    b    b  x  |   >    L a  x  .
               |     /      /      N-M  M    |  /        L
               \     -----  -----            /  -----
                      N=0    M=0                 L=1

        Expanding this once again with Cauchy's product rule we get

           -----
           \           N-1
            >    N b  x    =
           /        N
           -----
            N=1

                                L-1     N
           -----      /        -----  -----                    \
           \      L-1 |        \      \                        |
            >    x    | L a  +  >      >    b    b  (L-N) a    | .
           /          |    L   /      /      N-M  M        L-N |
           -----      \        -----  -----                    /
            L=1                 N=0    M=0

        From this we immediately deduce the recursion relation

                      L-1                 N
                     -----              -----
                     \       L-N        \
           b  = a  +  >     ----- a      >    b    b  .  (**)
            L    L   /        L    L-N  /      N-M  M
                     -----              -----
                      N=0                M=0

        This relation is easily generalized to the case of a
        series in more than one variable, where the same comments
        apply as in the case of log and exp above.

        For the hyperbolic tangent the relation is nearly the same.
        Since its differential equation has a `-' where that of
        tangent has a `+' we simply have to do the same substitution
        in the relation (**).  For the cotangent we get an additional
        overall minus sign.

        There is one additional problem to be handled: if the constant
        term of T(x), i.e. T(x0) is a pole of tangent. This can be
        solved quite easily: for a small quantity TAYEPS we calculate

             Te(x) = T(x) + TAYEPS .

        and perform the above calculation for Te(x). Then we recover
        the desired result via the relation

            tan T(x)  = tan (Te(x) - TAYEPS)

                           tan Te(x) - tan TAYEPS
                      = ---------------------------- .
                         1 + tan Te(x) * tan TAYEPS

        For the other functions we have similar relations:

            tanh T(x) = tanh (Te(x) - TAYEPS)

                           tanh Te(x) - tanh TAYEPS
                      = ------------------------------ ,
                         1 - tanh Te(x) * tanh TAYEPS

            cot T(x)  = cot (Te(x) - TAYEPS)

                         1 + cot Te(x) * cot TAYEPS
                      = ---------------------------- ,
                           cot TAYEPS - cot Te(x)

            coth T(x) = coth (Te(x) - TAYEPS)

                         1 - coth Te(x) * coth TAYEPS
                      = ------------------------------ .
                           coth Te(x) - coth TAYEPS

        We know that this result is independent of TAYEPS, and we can
        thus evaluate it for any value of TAYEPS.

        Since we further know that T(x0) is a pole of the function in
        question, we can express tan (T(x0) + TAYEPS) as

                                1
                        - ------------ ,
                           tan TAYEPS

        and similarly all the other possible expressions involving
        TAYEPS can be written in terms of tan TAYEPS and tanh TAYEPS,
        respectively. This makes it possible to just substitute any
        occurrence of tan TAYEPS or tanh TAYEPS by zero, which greatly
        simplifies the final calculation

        ;


!*!*taylor!-epsilon!*!* := compress '(t a y e p s);

symbolic procedure taysimptan u;
  %
  % Special Taylor expansion function for circular and hyperbolic
  %  tangent and cotangent
  %
  if not (car u memq '(tan cot tanh coth)) or cddr u
    then confusion 'taysimptan
   else taylor!:
    begin scalar a,a0,clist,coefflis,l0,l,poleflag,tay,tp;
    tay := taysimpsq simp!* cadr u;
    if not taylor!-kernel!-sq!-p tay
      then return mksq({car u,mk!*sq tay},1);
    tay := mvar numr tay;
    tp := taytemplate tay;
    l0 := make!-cst!-powerlist tp;
    %
    % First we get rid of possible zero coefficients.
    %
    l := taycoefflist tay;
    while l and null numr taycfsq car l do l := cdr l;
    if null l then return !*tay2q cst!-taylor!*(simp!* {car u,0},tp);
    %
    % The following relies on the standard ordering of the
    % TayCoeffList.
    %
    if is!-neg!-pl taycfpl car l
      then taylor!-error('essential!-singularity,car u);
    a0 := taygetcoeff(l0,l);
    %
    %%% handle poles of function
    %
    a := quotsq(a0,simp 'pi);
    if car u memq '(tanh coth)
      then a := subs2 multsq(a,simp 'i) where !*sub2 := !*sub2;
    if car u memq '(tan tanh) and
       denr(a := multsq(a,simp '2)) = 1 and
       fixp (a := numr a) and not evenp a
       or car u memq '(cot coth) and denr a = 1 and
          (null (a := numr a) or fixp a)
      then <<
        %
        % OK, now we are at a pole, so we add a small quantity, compute
        %  the series and use the addition formulas to get the real
        %  result.
        %
        poleflag := t;
        a0 := if car u eq 'tan
                then negsq invsq simp!* {'tan,!*!*taylor!-epsilon!*!*}
               else if car u eq 'tanh
                then invsq simp!* {'tanh,!*!*taylor!-epsilon!*!*}
               else if car u eq 'cot
                then invsq simp!* {'tan,!*!*taylor!-epsilon!*!*}
               else invsq simp!* {'tanh,!*!*taylor!-epsilon!*!*};
        clist := {taymakecoeff(l0,a0)};
        >>
     else clist := {taymakecoeff(l0,simp!* {car u,mk!*sq a0})};
    %
    coefflis := makecoeffs0(tp,tpdegreelist tp);
    for each cc in cdr coefflis do
      begin scalar cf,s,pos,pp,x,y,n,n1;
        s := nil ./ 1;
        pos := find!-non!-zero cc;
        n := nth(nth(cc,car pos),cdr pos);
        pp := makecoeffpairs(l0,cc,l0);
        for each p in pp do <<
          x := reversip makecoeffpairs1(l0,car p,l0);
          y := nil ./ 1;
          for each z in x do
            y := addsq(y,multsq(taygetcoeff(car z,clist),
                                taygetcoeff(cdr z,clist)));
          n1 := nth(nth(car p,car pos),cdr pos);
          s := addsq(s,
                     multsq(!*tayexp2q(n - n1),
                            multsq(y,taygetcoeff(cdr p,l))))>>;
        cf := quotsq(s,!*tayexp2q n);
        if car u memq '(tanh coth) then cf := negsq cf;
        cf := addsq(taygetcoeff(cc,l),cf);
        if numr cf then <<if car u eq 'cot then cf := negsq cf;
                          clist := taymakecoeff(cc,cf) . clist>>;
      end;
    clist := reversip clist;
    a := make!-taylor!*(clist,tp,nil,nil);
    %
    % Construct ``real'' series in case of pole
    %
    if poleflag then begin scalar x;
       x := if car u eq 'cot
              then cst!-taylor!*(
                     invsq simp {'tan,!*!*taylor!-epsilon!*!*},tp)
             else if car u eq 'coth
              then cst!-taylor!*(
                     invsq simp {'tanh,!*!*taylor!-epsilon!*!*},tp)
             else cst!-taylor!*(
                     simp {car u,!*!*taylor!-epsilon!*!*},tp);

       if car u eq 'tan then
         a := quottaylor(addtaylor(a,negtaylor x),
                         addtaylor(cst!-taylor!*(1 ./ 1,tp),
                                   multtaylor(a,x)))
        else if car u eq 'cot then
         a := quottaylor(addtaylor(multtaylor(a,x),
                                   cst!-taylor!*(1 ./ 1,tp)),
                         addtaylor(x,negtaylor a))
        else if car u eq 'tanh then
         a := quottaylor(addtaylor(a,negtaylor x),
                         addtaylor(cst!-taylor!*(1 ./ 1,tp),
                                   negtaylor multtaylor(a,x)))
        else if car u eq 'coth then
         a := quottaylor(addtaylor(multtaylor(a,x),
                                   cst!-taylor!*(-1 ./ 1,tp)),
                         addtaylor(x,negtaylor a));

        if not (a freeof !*!*taylor!-epsilon!*!*)
          then set!-taycoefflist(a,
                  for each pp in taycoefflist a collect
                    taymakecoeff(taycfpl pp,
                                 subsq(taycfsq pp,
                                       {!*!*taylor!-epsilon!*!* . 0})));
      end;
    %
    if !*taylorkeeporiginal and tayorig tay
      then set!-tayorig(a,simp {car u,mk!*sq tayorig tay});
    set!-tayflags(a,tayflags tay);
    return !*tay2q a
  end;

put('tan,'taylorsimpfn,'taysimptan);
put('cot,'taylorsimpfn,'taysimptan);
put('tanh,'taylorsimpfn,'taysimptan);
put('coth,'taylorsimpfn,'taysimptan);



Comment For the circular sine and cosine and their reciprocals
        we calculate the exponential and use it via the formulas


                     exp(+I*x) - exp(-I*x)
            sin x = ----------------------- ,
                               2

                     exp(+I*x) + exp(-I*x)
            cos x = ----------------------- ,
                               2

        etc. To avoid clumsy expressions we separate the constant term
        of the argument,

               T(x) = a0 + T1(x),

        and use the addition theorems which give

                       1
            sin T(x) = - exp(+I*T1(x)) * (sin a0 - I*cos a0) +
                       2

                       1
                       - exp(-I*T1(x)) * (sin a0 + I*cos a0) ,
                       2
       
                       1
            cos T(x) = - exp(+I*T1(x)) * (cos a0 + I*sin a0) +
                       2

                       1
                       - exp(-I*T1(x)) * (cos a0 - I*sin a0) .
                       2

        ;


symbolic procedure taysimpsin u;
  %
  % Special Taylor expansion function for circular sine, cosine,
  %  and their reciprocals
  %
  if not (car u memq '(sin cos sec csc)) or cddr u
    then confusion 'taysimpsin
   else taylor!:
    begin scalar l,l1,tay,result,tp,i1,l0,a0,a1,a2;
    tay := taysimpsq simp!* cadr u;
    if not taylor!-kernel!-sq!-p tay
      then return mksq({car u,mk!*sq tay},1);
    tay := mvar numr tay;
    tp := taytemplate tay;
    l0 := make!-cst!-powerlist tp;
    l := taycoefflist tay;
    while l and null numr taycfsq car l do l := cdr l;
    if null l then return !*tay2q cst!-taylor!*(simp!* {car u,0},tp);
%    if is!-neg!-pl TayCfPl car l
%      then Taylor!-error('essential!-singularity,car u);
    a0 := taygetcoeff(l0,l);
    %
    % make constant term to 0
    %
    i1 := simp 'i;
    if not null numr a0
      then tay := addtaylor(tay,cst!-taylor!*(negsq a0,tp));
    result := taysimpexp{'exp,multtaylor(tay,cst!-taylor!*(i1,tp))};
    a1 := simp!* {'sin,mk!*sq a0} . simp!* {'cos,mk!*sq a0};
    if car u memq '(sin csc) then <<
      a2 := addsq(car a1,multsq(i1,cdr a1));
      a1 := addsq(car a1,negsq multsq(i1,cdr a1));
      >>
     else <<
      a2 := addsq(cdr a1,negsq multsq(i1,car a1));
      a1 := addsq(cdr a1,multsq(i1,car a1));
     >>;
    result := multsq(addsq(multsq(result,a1),
                           multsq(taysimpsq!* invsq result,a2)),
                     1 ./ 2);
    if car u memq '(sec csc) then result := invsq result;
    return taysimpsq!* result
  end;

put('sin,'taylorsimpfn,'taysimpsin);
put('cos,'taylorsimpfn,'taysimpsin);
put('sec,'taylorsimpfn,'taysimpsin);
put('csc,'taylorsimpfn,'taysimpsin);



Comment For the hyperbolic sine and cosine and their reciprocals
        we calculate the exponential and use it via the formulas


                     exp(+x) - exp(-x)
           sinh x = ------------------- ,
                             2

                     exp(+x) + exp(-x)
           cosh x = ------------------- ,
                             2

        etc.

        ;


symbolic procedure taysimpsinh u;
  %
  % Special Taylor expansion function for hyperbolic sine, cosine,
  %  and their reciprocals
  %
  if not (car u memq '(sinh cosh sech csch)) or cddr u
    then confusion 'taysimpsinh
   else taylor!:
    begin scalar l,l1,lm,lp,tay, tay2, tp;
    tay := taysimpexp {'exp,cadr u};
    tay2 := taysimpsq!* invsq tay;
    if car u memq '(sinh csch) then tay2 := negsq tay2;
    tay2 := multsq(addsq(tay,tay2),1 ./ 2);
    return
      taysimpsq!*
        if car u memq '(sech csch) then invsq tay2 else tay2;
  end;


put('sinh, 'taylorsimpfn, 'taysimpsinh);
put('cosh, 'taylorsimpfn, 'taysimpsinh);
put('sech, 'taylorsimpfn, 'taysimpsinh);
put('csch, 'taylorsimpfn, 'taysimpsinh);


Comment Support for the integration of Taylor kernels.
        Unfortunately, with the current interface, only Taylor kernels
        on toplevel can be treated successfully.

        The way it is down means stretching certain interfaces beyond
        what they were designed for...but it works!

        First we add a rule that replaces a call to INT with a Taylor
        kernel as argument by a call to TAYLORINT, then we define
        REVALTAYLORINT as simplification function for that;


put ('int, 'opmtch,
     '(((taylor!* !=x !=y !=z !=w) !=u) (nil . t)
       (taylorint !=x !=y !=z !=w !=u) nil) .
      '((!=x !=y) (nil . (smember 'taylor!* (aeval '!=x)))
        (taylorint1 !=x !=y) nil) .
      get ('int, 'opmtch));

for each x in '(!=x !=y !=z !=w !=u) do
  if not (x memq frlis!*) then frlis!* := x . frlis!*;

put('taylorint1, 'psopfn, 'revaltaylorint1);

symbolic procedure revaltaylorint1 x;
  begin scalar u, v;
    u := car x;
    v := cadr x;
    if taylor!*p u then return revaltaylorint append (cdr u, list v);
    u := reval taylorcombine u;
    if taylor!*p u then return revaltaylorint append (cdr u, list v);
    lprim "Converting Taylor kernels to standard representation";
    return aeval {'int, taylortostandard car x, v}
  end;

put('taylorint, 'psopfn, 'revaltaylorint);

symbolic procedure revaltaylorint u;
  begin scalar taycfl, taytp, tayorg, tayflgs, var;
    taycfl := car u;
    taytp := cadr u;
    tayorg := caddr u;
    tayflgs := cadddr u;
    var := car cddddr u;
    if null (var member taytpvars taytp)
      then return mk!*sq !*tay2q
            make!-taylor!*(
              for each pp in taycfl collect
                taycfpl pp . simp!* {'int,mk!*sq taycfsq pp,var},
              taytp,
              if not null tayorg
                then reval {'int,mk!*sq tayorg,var}
               else nil,
            nil)
     else return mk!*sq
            ((if null car result then !*tay2q cdr result
               else addsq(car result,!*tay2q cdr result))
             where result := inttaylorwrttayvar1(taycfl,taytp,var))
   end;

endmodule;


module tayrevert;

%*****************************************************************
%
%       Functions for reversion of Taylor kernels
%
%*****************************************************************


exports taylorrevert;

imports

% from the REDUCE kernel:
        !*a2k, !*f2q, !*n2f, !*q2a, lastpair, mvar, neq, nth, numr,
        over, simp!*, typerr,

% from the header module:
        make!-cst!-coefficient, make!-cst!-coefflis, make!-taylor!*,
        multintocoefflist, set!-taytemplate, taycfpl, taycfsq,
        taycoefflist, taymakecoeff, taylor!-kernel!-sq!-p,
        taytemplate, taytpelnext, taytpelorder, taytpelpoint,
        taytpelvars,

% from module Tayintro:
        confusion, delete!-nth, taylor!-error,

% from module Taybasic:
        addtaylor1, invtaylor1, multtaylor1, negtaylor1,

% from module Tayutils:
        enter!-sorted;


symbolic procedure tayrevert (tay, okrnl, krnl);
  %
  % Reverts Taylor kernel tay that has okrnl as variable
  %  into a Taylor kernel that has krnl as variable.
  %
  % This is the driver procedure; it check whether okrnl
  %  is valid for this operation and calls tayrevert1 to do the work.
  %
  begin scalar tp, cfl, x; integer i;
    cfl := taycoefflist tay;
    tp := taytemplate tay;
    x := tp;
    i := 1;
    %
    % Loop through the template and make sure that the kernel
    %  okrnl is present and not part of a homogeneous template.
    %
   loop:
    if okrnl member taytpelvars car x then <<
      if not null cdr taytpelvars car x then <<
        taylor!-error ('tayrevert,
                       {"Kernel", okrnl,
                        "appears in homogenous template", car x});
        return
        >>
       else goto found;
      >>
     else <<
       x := cdr x;
       i := i + 1;
       if not null x then goto loop
       >>;
    taylor!-error
      ('tayrevert, {"Kernel", okrnl, "not found in template"});
    return;
   found:
    return tayrevert1 (tp, cfl, car x, i, okrnl, krnl)
  end;


symbolic procedure tayrevertreorder (cf, i);
  %
  % reorders coefflist cf so that
  %  a) part i of template is put first
  %  b) the resulting coefflist is again ordered properly
  %
  begin scalar cf1, pl, sq;
    for each pp in cf do <<
      pl := taycfpl pp;
      sq := taycfsq pp;
      pl := nth (pl, i) . delete!-nth (pl, i);
      cf1 := enter!-sorted (taymakecoeff (pl, sq), cf1)
      >>;
    return cf1
  end;

symbolic procedure tayrevertfirstdegreecoeffs (cf, n);
  %
  % Returns a coefflist that corresponds to the coefficient
  %  of (the first kernel in the template) ** n.
  %
  for each el in cf join
    if car car taycfpl el = n and not null numr taycfsq el
     then {taymakecoeff (cdr taycfpl el, taycfsq el)} else nil;


symbolic procedure tayrevert1 (tp, cf, el, i, okrnl, krnl);
  %
  % This is the procedure that does the real work.
  %  tp is the old template,
  %  cf the old coefflist,
  %  el the element of the template that contains the "old" kernel,
  %  i its position in the template,
  %  okrnl the old kernel,
  %  krnl the new kernel.
  %
  begin scalar el, newtp, newcf, newpoint, newel, u, u!-k, v, w, x, x1;
        integer n;
    %
    % First step: reorder the coefflist as if the okrnl appears
    %              at the beginning of the template and construct a
    %              new template by first deleting this element from it.
    %
    newcf := tayrevertreorder (cf, i);
    newtp := delete!-nth (tp, i);
    %
    % Remove zero coefficients
    %
    while null numr taycfsq car newcf do newcf := cdr newcf;
    %
    % Check that the lowest degree of okrnl is -1, 0, or +1.
    %  For -1, we have a first order pole.
    %  For 0, reversion is possible only if the coefficient
    %   of okrnl is a constant w.r.t. the other Taylor variables.
    %  For +1, we use the algorithm quoted by Knuth,
    %   in: The Art of Computer Programming, vol2. p. 508.
    %
    n := car car taycfpl car newcf;
    if n = -1
      then tayrevert1pole (tp, cf, el, i, okrnl, krnl, newcf, newtp);
    if n = 0
      then if not null newtp
             then begin scalar xx;
               xx := tayrevertfirstdegreecoeffs (newcf, 0);
               if length xx > 1
                 then taylor!-error
                       ('tayrevert,
                        "Term with power 0 is a Taylor series");
               xx := car xx;
               for each el in taycfpl xx do
                 for each el2 in el do
                   if not (el2 = 0)
                     then taylor!-error
                           ('tayrevert,
                            "Term with power 0 is a Taylor series");
               newpoint := !*q2a taycfsq xx;
              end
            else << newpoint := !*q2a taycfsq car newcf;
                    newcf := cdr newcf >>
     else if n = 1
      then newpoint := 0
     else return taylor!-error
                   ('tayrevert, "Lowest order term is not linear");
    x := tayrevertfirstdegreecoeffs (newcf, 1);
    x1 := x := invtaylor1 (newtp, x);
    w := for each pp in x1 collect
           taymakecoeff ({1} . taycfpl pp, taycfsq pp);
    v := for j := 2 : taytpelorder el collect
           (j . multtaylor1 (newtp,
                             tayrevertfirstdegreecoeffs (newcf, j), x));
%
    u := (0 . make!-cst!-coefflis (1 ./ 1, newtp)) . nil;
    for j := 2 : taytpelorder el do <<
      for k := 1 : j - 2 do begin scalar xx;
        xx := assoc (k, u);
        if null xx then confusion "revert" else u!-k := cdr xx;
        for l := k - 1 step -1 until 0 do
          u!-k := addtaylor1
                   (u!-k,
                    negtaylor1 multtaylor1 (newtp,
                                            cdr assoc (l, u),
                                            cdr assoc (k - l + 1, v)));
        rplacd (xx, u!-k);
        end;
      u!-k := multintocoefflist (cdr assoc (j, v), !*f2q !*n2f j);
      for k := 1 : j - 2 do
        u!-k := addtaylor1
                 (multintocoefflist (multtaylor1 (newtp,
                                                  cdr assoc (k, u),
                                                  cdr assoc (j - k, v)),
                                     !*f2q !*n2f (j - k)),
                  u!-k);
      u!-k := negtaylor1 u!-k;
      u := ((j - 1) . u!-k) . u;
%
      x1 := multtaylor1 (newtp, x1, x);            % x1 is now x ** j
      for each pp in
             multtaylor1 (newtp,
                          multintocoefflist
                           (cdr assoc (j - 1, u), 1 ./ !*n2f j), x1) do
        w := enter!-sorted (taymakecoeff ({j} . taycfpl pp, taycfsq pp),
                            w);
      >>;
%
      newtp := {{krnl},newpoint,taytpelorder el,taytpelnext el} . newtp;
      w := enter!-sorted (
             make!-cst!-coefficient (simp!* taytpelpoint el, newtp),
             w);

      return make!-taylor!* (w, newtp, nil, nil)
%
  end;

Comment The mechanism for a first order pole is very simple:
        This corresponds to a first order zero at infinity,
        so we invert the original kernel and revert the result;

symbolic procedure tayrevert1pole (tp, cf, el, i, okrnl, krnl,
                                   newcf, newtp);
  begin scalar x, y, z;
    cf := invtaylor1 (tp, cf);
    x := tayrevert1 (tp, cf, el, i, okrnl, krnl);
    y := taytemplate x;
    if taytpelpoint car y neq 0
      then taylor!-error ('not!-implemented,
                          "(Taylor series reversion)")
     else <<
       set!-taytemplate (x, {{krnl}, 'infinity, taytpelorder car y}
                              . cdr y);
       return x >>
  end;

Comment The driver routine;

symbolic procedure taylorrevert (u, okrnl, nkrnl);
  (if not taylor!-kernel!-sq!-p sq
     then typerr (u, "Taylor kernel")
    else tayrevert (mvar numr sq, !*a2k okrnl, !*a2k nkrnl))
     where sq := simp!* u$

flag ('(taylorrevert), 'opfn);

endmodule;


end;