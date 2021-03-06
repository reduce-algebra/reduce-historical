% "build38.lsp"
%
% Build a CSL REDUCE.
%
% Depending on how this file is used it will EITHER create a bootstrap
% version of REDUCE or a full and optimised one.
%
% The behaviour is determined by whether the version of CSL used to
% run it has a full complement of functions in the modules u01.c to u12.c.
%
%
%           slowr38 -z build38.lsp -D@srcdir=<DIR> -- log/boot38.log
%
% Builds a system "slowr38.img" that does not depend on any
% custom C code. The main use of this slow system is for profiling
% REDUCE and then compiling the hot-spots into C. Once that has been
% done this image is logically unnecessary.
%
%
%           r38 -z build38.lsp -D@srcdir=<DIR> -l log/full38.log
%
% Here the files u01.c to u12.c and u01.lsp to u12.lsp must already
% have been created, and that the r38 executable has them compiled in.
% The REDUCE source files that are compiled *MUST* be the same as those used
% to create this C code.

% Author: Anthony C. Hearn, Stanley L. Kameny and Arthur Norman

(verbos 3)

(window!-heading "basic CSL")

(setq !*savedef (lessp (cdr (assoc 'c!-code lispsystem!*)) 20))
(make!-special '!*native_code)
(setq !*native_code nil)

(cond ((null !*savedef) (progn

   (de c!:install (name env c!-version !&optional c1)
      (cond
        (c1 (check!-c!-code name env c!-version c1))
        (t (progn
              (put name 'c!-version c!-version)
              (cond (env (prog (v n)
                 (setq v (mkvect (sub1 (length env))))
                 (setq n 0)
            top  (cond
                    ((null env) (progn
                     (put name 'funarg v)
                     (return (symbol!-set!-env name v)))))
                 (putv v n (car env))
                 (setq n (add1 n))
                 (setq env (cdr env))
                 (go top))))
              name))))

   (rdf "$srcdir/../csl-c/u01.lsp")
   (rdf "$srcdir/../csl-c/u02.lsp")
   (rdf "$srcdir/../csl-c/u03.lsp")
   (rdf "$srcdir/../csl-c/u04.lsp")
   (rdf "$srcdir/../csl-c/u05.lsp")
   (rdf "$srcdir/../csl-c/u06.lsp")
   (rdf "$srcdir/../csl-c/u07.lsp")
   (rdf "$srcdir/../csl-c/u08.lsp")
   (rdf "$srcdir/../csl-c/u09.lsp")
   (rdf "$srcdir/../csl-c/u10.lsp")
   (rdf "$srcdir/../csl-c/u11.lsp")
   (rdf "$srcdir/../csl-c/u12.lsp")    )))

(rdf "$srcdir/../util/fastgets.lsp")
(rdf "$srcdir/../cslbase/compat.lsp")
(rdf "$srcdir/../cslbase/extras.lsp")
(rdf "$srcdir/../cslbase/compiler.lsp")
% (rdf "$srcdir/../cslbase/ccomp.lsp")

(compile!-all)

(setq !*comp t)               % It's faster if we compile the boot file.

% Tidy up be deleting any modules that are left over in this image

(dolist (a (library!-members)) (delete!-module a))

% Build fasl files for the compatibility code and the two
% versions of the compiler.

(faslout 'cslcompat)
(rdf "$srcdir/../util/fastgets.lsp")
(rdf "$srcdir/../cslbase/compat.lsp")
(rdf "$srcdir/../cslbase/extras.lsp")
(faslend)

(faslout 'compiler)
(rdf "$srcdir/../cslbase/compiler.lsp")
(faslend)

%(faslout 'ccomp)
%(rdf "$srcdir/../cslbase/ccomp.lsp")
%(faslend)

(setq !*comp t)

(de concat (u v)
    (compress (cons '!" (append (explode2 u)
			 (nconc (explode2 v) (list '!"))))))

(global '(oldchan!*))

(setq prolog_file 'cslprolo)

(setq rend_file 'cslrend)

(setq !*argnochk t)

(setq !*int nil)                    % Prevents input buffer being saved.

(setq !*msg nil)

(window!-heading "bootstrap RLISP")

(rdf "$srcdir/../../../packages/support/boot.sl")

(begin2)

!@reduce := concat(!@srcdir, "/../../..");

rds(xxx := open("$reduce/packages/support/build.red",'input));

(close xxx)

(load!-package!-sources prolog_file 'support)

(load!-package!-sources 'rlisp 'rlisp)

(load!-package!-sources rend_file 'support)

(load!-package!-sources 'poly 'poly)

(load!-package!-sources 'alg 'alg)

(load!-package!-sources 'arith 'arith)  %  Needed by roots, specfn*, (psl).

(load!-package!-sources 'entry 'support)

(load!-package!-sources 'remake 'support)

(setq !*comp nil)



(begin)

symbolic;

!#if (not !*savedef)

faslout 'user;

%
% The "user" module is only useful when building a full system, since
% in the bootstrap the files u01.lsp to u12.lsp will probably not exist
% and it is CERTAIN that they are not useful.
%

symbolic procedure c!:install(name, env, c!-version, !&optional, c1);
  begin
    scalar v, n;
    if c1 then return check!-c!-code(name, env, c!-version, c1);
    put(name, 'c!-version, c!-version);
    if null env then return name;
    v := mkvect sub1 length env;
    n := 0;
    while env do <<
      putv(v, n, car env);
      n := n + 1;
      env := cdr env >>;
% I only instate the environment if there is nothing useful there at
% present. Actually this is even stronger. When a built-in function is
% set up it gets NIL in its environment cell by default. Things that are
% not defined at all have themselves there.
    if symbol!-env name = nil then symbol!-set!-env(name, v);
    put(name, 'funarg, v);
    return name;
  end;


rdf "$srcdir/../csl-c/u01.lsp"$
rdf "$srcdir/../csl-c/u02.lsp"$
rdf "$srcdir/../csl-c/u03.lsp"$
rdf "$srcdir/../csl-c/u04.lsp"$
rdf "$srcdir/../csl-c/u05.lsp"$
rdf "$srcdir/../csl-c/u06.lsp"$
rdf "$srcdir/../csl-c/u07.lsp"$
rdf "$srcdir/../csl-c/u08.lsp"$
rdf "$srcdir/../csl-c/u09.lsp"$
rdf "$srcdir/../csl-c/u10.lsp"$
rdf "$srcdir/../csl-c/u11.lsp"$
rdf "$srcdir/../csl-c/u12.lsp"$


faslend;
!#endif

faslout 'remake;

!#if (not !*savedef)

load!-module "user";

!#endif

!@reduce := concat(!@srcdir, "/../../..");

in "$reduce/packages/support/remake.red"$

global '(r38_base_modules r38_extra_modules r38_test_cases);
 
% Master configuration data is stored in two DOS batch files. 
% This function extracts the information and puts it where I want it!

symbolic procedure make_conf_file();
  begin
    scalar a, p1, p2, p3, w;
% upackage.bat may look like
%   set upackages=(support rlisp alg poly polydiv arith ...)
%   set upackages2=(odesolve pf cvit noncom2 physop crack liepde ...)
%   set upackages3=(specfn2 specfaux specbess sfgamma tps limits ...)
% where the first list is of CORE packages needed for bootstrapping and all
% subsequent ones are independent of each other (at least ideally).
%
% Well as I understand it at present in the development tree this file is
% called vpackage.bat...
    if filep "$srcdir/../../../getred.pl" then
        a := open("$srcdir/../../../vpackage.bat", 'input)
    else a := open("$srcdir/../../../upackage.bat", 'input);
    p1 := nil;
    while atom p1 and p1 neq !$eof!$ do <<
       a := rds a; p1 := read(); a := rds a >>;

    p2 := w := nil;
    while w neq !$eof!$ do <<
       w := nil;
       while atom w and w neq !$eof!$ do <<
          a := rds a; w := read(); a := rds a >>;
       if not atom w then p2 := append(p2, w) >>;
    close a;
% xpackage.bat must be a list of test scripts, similar to this:
%   set xpackages=(alg poly polydiv arith factor int matrix solve ...)
%   set xpackages2=(odesolve pf cvit physop crack liepde applysym ...)
%   set xpackages3=(tps limits defint fps trigint ratint mathml ...)
    a := open("$srcdir/../../../xpackage.bat", 'input);
    p3 := w := nil;
    while w neq !$eof!$ do <<
       w := nil;
       while atom w and w neq !$eof!$ do <<
          a := rds a; w := read(); a := rds a >>;
       if not atom w then p3 := append(p3, w) >>;
    close a;
    if filep "$srcdir/../../../getred.pl" then
       a := "$srcdir/../util/devconfig.lsp"
    else a := "$srcdir/../util/config.lsp";
    a := open(a, 'output);
    << a := wrs a; linelength 72;
       terpri();
       printc "% These are packages that get built into the base system that";
       printc "% is used to compile what follows...";
       terpri();
       prettyprint p1; terpri();
       terpri();
       printc "% The next set of modules are all built using a system that";
       printc "% has the above set available. The key issue here is that the";
       printc "% packages in this list of ""extensions"" can all be built";
       printc "% independently of each other.";
       terpri();
% The v3tools module depends on the groebner code already being built, and so
% it must appear late in the list. As I introduce this script the package.bat
% file may not reflect that so FUDGE it here.
       if memq('v3tools, p2) then
          p2 := append(delete('v3tools, p2), '(v3tools));
% rltools must occur before redlog. This is also a pretty shameless FUDGE
% here.
       if memq('rltools, p2) then
          p2 := 'rltools . delete('rltools, p2);
       prettyprint p2; terpri();
       terpri();
       printc "% Finally we have a list of all the test scripts that REDUCE";
       printc "% is shipped with.";
% As a special HACK I will remove gnuplot (if present) since at least as
% of the time of working on this its level of interactivity causes the
% profiling job to fail.
       r3 := delete('gnuplot, p3);
       terpri();
       prettyprint p3; terpri();
       terpri();
       printc "% End of configuration file";
       close wrs a >>
  end;


symbolic procedure get_configuration_data full_version;
% Read data from a configuration file that lists the modules that must
% be processed.  NOTE that this and the next few funtions will ONLY
% work properly if REDUCE had been started up with the correct
% working directory. This is (just about) acceptable because these are
% system maintainance functions rather than generally flexible things
% for arbitrary use.
  begin
    scalar i, j;
% Configuration information is held in a file called something like
% "config.lsp", but the exact details differ in a number of cases. For
% the Development Version (not for general public use) this file will
% be $srcdir/../util/devconfig.lsp. For a personal version it will be
% util/config.lsp and in the case of a "professional" package it is
% $srcdir/../util/config.lsp. The code here tries to detect which case
% applies. If a file "$srcdir/../../../getred.pl" exists it will believe
% it is the development tree!
    if filep "$srcdir/../../../getred.pl" then
    <<  i := "$srcdir/../util/devconfig.lsp";
        j := "$srcdir/../../../vpackage.bat" >>
    else if full_version then <<
        i := "$srcdir/../util/config.lsp";
        j := "$srcdir/../../../vpackage.bat" >>
    else i := "util/config.lsp";
% I now try to ensure that the configuration file exists and is up to date.
% Note that this is only done on a full build of Reduce. The [dev]config.lsp
% is updated if either upackage.bat or xpackage.bat is newer than it...
    if not filep i or
       (j and filep j and
        datelessp(filedate i, filedate j)) or
       (filep "$srcdir/../../../xpackage.bat" and
        datelessp(filedate i, filedate "$srcdir/../../../xpackage.bat")) then
       make_conf_file(); 
    i := open(i, 'input);
    i := rds i;
    r38_base_modules := read();
    r38_extra_modules := read();
    r38_test_cases := read();
    i := rds i;
    close i
  end;

symbolic procedure build_reduce_modules names;
  begin
    scalar w;
!#if !*savedef
    !*savedef := t;
!#else
    !*savedef := nil;
!#endif
    make!-special '!*native_code;
    !*native_code := nil;
    load!-latest!-patches();
    !@reduce := concat(!@srcdir, "/../../..");
    in "$reduce/package.red"$
    window!-heading list!-to!-string explodec car names;
!#if !*savedef
% When building the bootstrap version I want to record what switches
% get declared...
    if not getd 'original!-switch then <<
       w := getd 'switch;
       putd('original!-switch, car w, cdr w);
       putd('switch, 'expr, 
          '(lambda (x) 
              (dolist (y x) (princ "+++ Declaring a switch: ") (print y)) 
              (original!-switch x))) >>;
!#endif
    package!-remake car names;
    if null (names := cdr names) then printc "Recompilation complete";
!#if !*savedef
    if null names then restart!-csl 'begin
!#else
    if null names then restart!-csl '(user begin)
!#endif
    else restart!-csl('(remake build_reduce_modules), names)
  end;

symbolic procedure test_a_package names;
  begin
    scalar packge, logname, logtmp, logfile, start_time, start_gctime, gt;
    scalar redef, quitfn, oll;
    princ "TESTING: "; print car names;
    window!-heading list!-to!-string explodec car names;
    !*backtrace := nil;
    !*errcont := t;
    !*extraecho := t;    % Ensure standard environment for the test...
    !*int := nil;        % ... so that results are predictable.
    packge := car names;
    verbos nil;
    load!-latest!-patches();
    if boundp '!@log and stringp symbol!-value '!@log then
        logname := symbol!-value '!@log
    else logname := "log";
    logname := concat("$srcdir/../../../", concat(logname, "/"));
    logtmp  := concat(logname, concat(car names, ".tmp"));
    logname := concat(logname, concat(car names,".rlg"));
    logfile := open(logtmp, 'output);
    in_list1("$srcdir/../../../package.red", nil);
    begin
       scalar !*terminal!-io!*, !*standard!-output!*, !*error!-output!*,
              !*trace!-output!*, !*debug!-io!*, !*query!-io!*, !*errcont,
              outputhandler!*;
       !*terminal!-io!* := !*standard!-output!* := !*error!-output!* := logfile;
       !*trace!-output!* := !*debug!-io!* := !*query!-io!* := logfile;
       oll := linelength 80;
       princ date(); princ " run on "; printc cdr assoc('name, lispsystem!*);
       load!-package packge;
       if get(packge,'folder) then packge := get(packge,'folder);
       packge := concat("$srcdir/../../../packages/",
                   concat(packge,
                     concat("/",
                       concat(car names,".tst"))));
       redef := !*redefmsg;
       !*redefmsg := nil;
       quitfn := getd 'quit;
% At least at one stage at least one test file ends in "quit;" rather than
% "end;" and the normal effect would be that this leads it to cancel
% all execution instantly. To avoid that I will undefine the function
% "quit", but restore it after the test. I reset !*redefmsg to avoid getting
% messages about this. I redefined quit to something (specifically "posn")
% that does not need an argument and that is "harmless".
       remd 'quit;
       putd('quit, 'expr, 'posn);
       start_time := time();
       start_gctime := gctime();
       !*mode := 'algebraic;
       !*extraecho := t;    % Ensure standard environment for the test...
       !*int := nil;        % ... so that results are predictable.
       !*errcont := t;
       in_list1(packge, t);
       erfg!* := nil;
       terpri();
       putd('quit, car quitfn, cdr quitfn);
       !*redefmsg := redef;
       terpri();
       prin2 "Time for test: ";
       gt := time() - start_time;
% I ensure that the reported time is at least 1 millisecond.
       if gt = 0 then gt := 1;
       prin2 gt;
       prin2 " ms";
       if (gt := gctime() - start_gctime) > 0 then <<
           prin2 ", plus GC time: ";
           prin2 gt;
           prin2 " ms" >>;
       terpri();
       linelength oll
    end;
    close logfile;
    delete!-file logname;
    rename!-file(logtmp, logname);
    names := cdr names;
    if null names then restart!-csl t
    else restart!-csl('(remake test_a_package), names)
  end;

symbolic procedure personal_test_a_package names;
% This version is only used with the personal version of REDUCE and uses
% different paths to access files.
  begin
    scalar packge, logname, logtmp, logfile, start_time, start_gctime, gt;
    scalar redef, quitfn, oll;
    princ "TESTING: "; print car names;
    window!-heading list!-to!-string explodec car names;
    !*backtrace := nil;
    !*errcont := t;
    !*extraecho := t;    % Ensure standard environment for the test...
    !*int := nil;        % ... so that results are predictable.
    packge := car names;
    verbos nil;
    load!-latest!-patches();
    if boundp '!@log and stringp symbol!-value '!@log then
        logname := symbol!-value '!@log
    else logname := "log";
    logname := concat(logname, "/");
    logtmp  := concat(logname, concat(car names, ".tmp"));
    logname := concat(logname, concat(car names,".rlg"));
    logfile := open(logtmp, 'output);
    begin
       scalar !*terminal!-io!*, !*standard!-output!*, !*error!-output!*,
              !*trace!-output!*, !*debug!-io!*, !*query!-io!*, !*errcont,
              outputhandler!*;
       !*terminal!-io!* := !*standard!-output!* := !*error!-output!* := logfile;
       !*trace!-output!* := !*debug!-io!* := !*query!-io!* := logfile;
       oll := linelength 80;
       princ date(); princ " run on "; printc cdr assoc('name, lispsystem!*);
       load!-package packge;
       packge := concat("tests/", concat(car names,".tst"));
       redef := !*redefmsg;
       !*redefmsg := nil;
       quitfn := getd 'quit;
% At least at one stage at least one test file ends in "quit;" rather than
% "end;" and the normal effect would be that this leads it to cancel
% all execution instantly. To avoid that I will undefine the function
% "quit", but restore it after the test. I reset !*redefmsg to avoid getting
% messages about this. I redefined quit to something (specifically "posn")
% that does not need an argument and that is "harmless".
       remd 'quit;
       putd('quit, 'expr, 'posn);
       start_time := time();
       start_gctime := gctime();
       !*mode := 'algebraic;
       !*extraecho := t;    % Ensure standard environment for the test...
       !*int := nil;        % ... so that results are predictable.
       !*errcont := t;
       in_list1(packge, t);
       erfg!* := nil;
       terpri();
       putd('quit, car quitfn, cdr quitfn);
       !*redefmsg := redef;
       terpri();
       prin2 "Time for test: ";
       gt := time() - start_time;
% I ensure that the reported time is at least 1 millisecond.
       if gt = 0 then gt := 1;
       prin2 gt;
       prin2 " ms";
       if (gt := gctime() - start_gctime) > 0 then <<
           prin2 ", plus GC time: ";
           prin2 gt;
           prin2 " ms" >>;
       terpri();
       linelength oll
    end;
    close logfile;
    delete!-file logname;
    rename!-file(logtmp, logname);
    names := cdr names;
    if null names then restart!-csl t
    else restart!-csl('(remake personal_test_a_package), names)
  end;

symbolic procedure report_incomplete_tests names;
  begin
% Displays information about what "complete_tests" would do
    scalar packge, tfile, logname;
    scalar date1, date2, date3;
    in_list1("$srcdir/../../../package.red", nil);
    for each packge in names do <<
       tfile := packge;
       if get(packge,'folder) then tfile := get(packge,'folder);
       tfile := concat("$srcdir/../../../packages/",
                   concat(tfile,
                      concat("/",
                        concat(packge,".tst"))));
       if boundp '!@log and stringp symbol!-value '!@log then
           logname := symbol!-value '!@log
       else logname := "log";
       logname := concat("$srcdir/../../../", concat(logname, "/"));
       logname := concat(logname, concat(packge,".rlg"));
       date1 := filedate "r38.img";
       date2 := filedate tfile;
       date3 := filedate logname;
       if null date1 then date1 := date();
       if null date2 then date2 := date();
       if null date3 or
          datelessp(date3, date1) or datelessp(date3, date2) then <<
             princ "NEED TO TEST: "; print packge >> >>
  end;

symbolic procedure complete_tests names;
  begin
% Just like the previous testing code except that logs that are already up
% to date are not re-generated.
    scalar packge, tfile, logname, logfile, logtmp,
           start_time, start_gctime, gt;
    scalar date1, date2, date3, oll;
    !*backtrace := nil;
    !*errcont := t;
    !*extraecho := t;    % Ensure standard environment for the test...
    !*int := nil;        % ... so that results are predictable.
    verbos nil;
    load!-latest!-patches();
    in_list1("$srcdir/../../../package.red", nil);
top:
    tfile := packge := car names;
    if get(tfile,'folder) then tfile := get(tfile,'folder);
    tfile := concat("$srcdir/../../../packages/",
                concat(tfile,
                  concat("/",
                    concat(packge,".tst"))));
    if boundp '!@log and stringp symbol!-value '!@log then
        logname := symbol!-value '!@log
    else logname := "log";
    logname := concat("$srcdir/../../../", concat(logname, "/"));
    logtmp  := concat(logname, concat(packge, ".tmp"));
    logname := concat(logname, concat(packge, ".rlg"));
    date1 := filedate "r38.img";
    date2 := filedate tfile;
    date3 := filedate logname;
    if null date1 then date1 := date();
    if null date2 then date2 := date();
    if null date3 or
       datelessp(date3, date1) or datelessp(date3, date2) then <<
       princ "TESTING: "; print packge;
       window!-heading list!-to!-string explodec packge;
       logfile := open(logtmp, 'output);
       start_time := time();
       start_gctime := gctime();
       begin
          scalar !*terminal!-io!*, !*standard!-output!*, !*error!-output!*,
                 !*trace!-output!*, !*debug!-io!*, !*query!-io!*, !*errcont,
                 outputhandler!*, redef, quitfn;
          !*terminal!-io!* := !*standard!-output!* := !*error!-output!* := logfile;
          !*trace!-output!* := !*debug!-io!* := !*query!-io!* := logfile;
          oll := linelength 80;
          princ date(); princ " run on ";
          printc cdr assoc('name, lispsystem!*);
          load!-package packge;
          !*mode := 'algebraic;
          !*extraecho := t;    % Ensure standard environment for the test...
          !*int := nil;        % ... so that results are predictable.
          redef := !*redefmsg;
          !*redefmsg := nil;
          quitfn := getd 'quit;
          remd 'quit;
          putd('quit, 'expr, 'posn);
          !*errcont := t;
          in_list1(tfile, t);
          erfg!* := nil;
          terpri();
          putd('quit, car quitfn, cdr quitfn);
          !*redefmsg := redef;
          terpri();
          prin2 "Time for test: ";
          gt := time() - start_time;
          if gt = 0 then gt := 1;
          prin2 gt;
          prin2 " ms";
          if (gt := gctime() - start_gctime) > 0 then <<
              prin2 ", plus GC time: ";
              prin2 gt;
              prin2 " ms" >>;
          terpri();
          linelength oll
       end;
       close logfile;
       delete!-file logname;
       rename!-file(logtmp, logname) >>
    else if cdr names then <<
       names := cdr names;
       go to top >>;
    names := cdr names;
    if null names then restart!-csl t
    else restart!-csl('(remake complete_tests), names)
  end;

symbolic procedure profile_compare_fn(p, q);
   (float caddr p/float cadr p) < (float caddr q/float cadr q);

%
% This function runs a test file and sorts out what the top 350
% functions in it. It appends their names to "profile.dat".
%

% I need to talk a little about the interaction between profiling and
% patching.  Well firstly I arrange that whenever I run a profiling job
% I rebuild REDUCE with the latest paches. This may involve re-compiling
% the patches.red source.  Thus when a test is run the current patches
% will be in place. Patched functions are first defined with funny names
% (including a hash based on their definition) and then copied into place
% when a package is loaded. However MAPSTORE and the CSL instrumentation
% attributes their cost to the hash-extended name even though the
% functions may have been called via the simple one. Thus in the face
% of patches one can expect the profile data to refer to some names that
% are long and curious looking. Throughout all this I assume that there will
% never be embarassing collisions in my hash functions.

symbolic procedure profile_a_package names;
  begin
    scalar packge, oll, w, w1, w2, quitfn, !*errcont;
    princ "PROFILING: "; print car names;
    !*backtrace := nil;
    !*errcont := t;
    !*int := nil;
    packge := car names;
    verbos nil;
    load!-latest!-patches();
    load!-package packge;
    in_list1("$srcdir/../../../package.red", nil);
    if get(packge,'folder) then packge := get(packge,'folder);
    packge := concat("$srcdir/../../../packages/",
                concat(packge,
                  concat("/",
                    concat(car names,".tst"))));
    oll := linelength 80;
    !*mode := 'algebraic;
    window!-heading list!-to!-string explodec car names;
    quitfn := getd 'quit;
    remd 'quit;
    putd('quit, 'expr, 'posn);
    mapstore 4;  % reset counts;
    !*errcont := t;
% I try hard to arrange that even if the test fails I can continue and that
% input & output file selection is not messed up for me.
    w := wrs nil;   w1 := rds nil;
    wrs w;          rds w1;
    errorset(list('in_list1, mkquote packge, t), nil, nil);
    wrs w;          rds w1;
    erfg!* := nil;
    terpri();
    putd('quit, car quitfn, cdr quitfn);
    w := sort(mapstore 2, function profile_compare_fn);
    w1 := nil;
    while w do <<
        w2 := get(caar w, '!*savedef);
%       if eqcar(w2, 'lambda) then <<
%           princ "md60: "; print (caar w . cdr w2);
%           princ "= "; print md60 (caar w . cdr w2) >>;
        if eqcar(w2, 'lambda) then w1 := (caar w . md60 (caar w . cdr w2) .
                                          cadar w . caddar w) . w1;
        w := cdr w >>;
    w := w1;
    % I collect the top 350 functions as used by each test, not because all
    % that many will be wanted but because I might as well record plenty
    % of information here and discard unwanted parts later on.
    for i := 1:349 do if w1 then w1 := cdr w1;
    if w1 then rplacd(w1, nil);
    % princ "MODULE "; prin car names; princ " suggests ";
    % print for each z in w collect car z;
    w1 := open("profile.dat", 'append);
    w1 := wrs w1;
    linelength 80;
    princ "("; prin car names; terpri();
    for each n in w do <<
        princ "  ("; prin car n; princ " ";
        if posn() > 30 then << terpri(); ttab 30 >>;
        prin cadr n;
        % I also display the counts just to help me debug & for interest.
        princ " "; prin caddr n; princ " "; princ cdddr n;
        printc ")" >>;
    printc "  )";
    terpri();
    close wrs w1;
    linelength oll;
    names := cdr names;
    if null names then restart!-csl t
    else restart!-csl('(remake profile_a_package), names)
  end;

symbolic procedure trim_prefix(a, b);
  begin
    while a and b and car a = car b do <<
      a := cdr a;
      b := cdr b >>;
    if null a then return b
    else return nil
  end;

fluid '(time_info);

symbolic procedure read_file f1;
  begin
% I take the view that I can afford to read the whole of a file into
% memory at the start of processing. This makes life easier for me
% and the REDUCE log files are small compared with current main memory sizes.
    scalar r, w, w1, n, x;
    scalar p1, p2, p3, p4, p5, p6, p7;
% To make comparisons between my CSL logs and some of the Hearn "reference
% logs", which are created using a different script, I will discard
% lines that match certain patterns!  Note that if the reference logs change
% the particular tests I perform here could become out of date! Also if any
% legitimate test output happened to match one of the following strings
% I would lose out slightly.
    p1 := explodec "REDUCE 3.8,";
    p2 := explodec "1: 1:";
    p3 := explodec "2: 2: 2:";
    p4 := explodec "3: 3: ";    % a prefix to first real line of output.
    p5 := explodec "4: 4: 4:";
    p6 := explodec "5: 5:";
    p7 := explodec "Quittin";   % nb left so that the "g" remains!
                                % this is so that the match is detected.
    r := nil;
    n := 0;
    while not ((w := readline f1) = !$eof!$) do <<
       w1 := explodec w;
       if x := trim_prefix(p4, w1) then
           r := ((n := n + 1) . list!-to!-string x) . r
       else if trim_prefix(p1, w1) or
               trim_prefix(p2, w1) or
               trim_prefix(p3, w1) or
               trim_prefix(p5, w1) or
               trim_prefix(p6, w1) or
               trim_prefix(p7, w1) then nil
       else r := ((n := n + 1) . w) . r >>;
    w := r;
% The text scanned for here is expected to match that generated by the
% test script. I locate the last match in a file, extract the numbers
% and eventually write them to log/times.log
    n := explodec "Time for test:";
    while w and null (x := trim_prefix(n, explodec cdar w)) do w := cdr w;
    if null w then <<
       time_info := nil;
       return reversip r >>;
    while eqcar(x, '! ) do x := cdr x;
    w := n := nil;
    while digit car x do << w := car x . w; x := cdr x >>;
    while eqcar(x, '! ) do x := cdr x;
    if x := trim_prefix(explodec "ms, plus GC time:", x) then <<
        while eqcar(x, '! ) do x := cdr x;
        while digit car x do << n := car x . n; x := cdr x >> >>;
    if null w then w := '(!0);
    if null n then n := '(!0);
    time_info := compress reverse w . compress reverse n;
    return reversip r;
  end;

symbolic procedure roughly_equal(a, b);
  begin
% a and b are strings repesenting lines of text. I want to test if they
% match subject to some floating point slop.
    scalar wa, wb, adot, bdot;
    if a = b then return t;
    a := explodec a;
    b := explodec b;
top:
% First deal with end of line matters.
    if null a and null b then return t
    else if null a or null b then return nil;
% next split off any bits of a and b up to a digit
    wa := wb := nil;
    while a and not digit car a do <<
       wa := car a . wa;
       a := cdr a >>;
    while b and not digit car b do <<
       wb := car b . wb;
       b := cdr b >>;
    if not (wa = wb) then return nil;
% now both a and b start with digits. I will seek a chunk of the
% form nnn.mmmE+xxx where E<sign>xxx is optional...
% Note that any leading sign on the float has been checked already!
    wa := wb := nil;
    adot := bdot := nil;
    while a and digit car a do <<
       wa := car a . wa;
       a := cdr a >>;
    if eqcar(a, '!.) then <<
       adot := t;
       wa := car a . wa;
       a := cdr a >>;
    while a and digit car a do <<
       wa := car a . wa;
       a := cdr a >>;
    if eqcar(a, '!e) or eqcar(a, '!E) then <<
       adot := t;
       wa := car a . wa;
       a := cdr a;
       if eqcar(a, '!+) or eqcar(a, '!-) then <<
          wa := car a . wa;
          a := cdr a >>;
       while a and digit car a do <<
          wa := car a . wa;
          a := cdr a >> >>;
% Now all the same to grab a float from b
    while b and digit car b do <<
       wb := car b . wb;
       b := cdr b >>;
    if eqcar(b, '!.) then <<
       bdot := t;
       wb := car b . wb;
       b := cdr b >>;
    while b and digit car b do <<
       wb := car b . wb;
       b := cdr b >>;
    if eqcar(b, '!e) or eqcar(b, '!E) then <<
       bdot := t;
       wb := car b . wb;
       b := cdr b;
       if eqcar(b, '!+) or eqcar(b, '!-) then <<
          wb := car b . wb;
          b := cdr b >>;
       while b and digit car b do <<
          wb := car b . wb;
          b := cdr b >> >>;
% Now one possibility is that I had an integer not a float,
% and in that case I want an exact match
    if not adot or not bdot then <<
       if wa = wb then goto top
       else return nil >>;
    if wa = wb then goto top;   % textual match on floating point values
    wa := compress reversip wa;
    wb := compress reversip wb;
    if fixp wa then wa := float wa;
    if fixp wb then wb := float wb;
    if not (floatp wa and floatp wb) then return nil; % messed up somehow!
    if wa = wb then goto top;
% now the crucial approximate floating point test - note that both numbers
% are positive, but that they may be extreme in range.
% As a cop-out I am going to insist that if values are either very very big
% or very very small that they match as text.
    if wa > 1.0e100 or wb > 1.0e100 then return nil;
    if wa < 1.0e-100 or wb < 1.0e-100 then return nil;
    wa := (wa - wb)/(wa + wb);
    if wa < 0 then wa := -wa;
    if wa > 0.0001 then return nil; % pretty crude!
    goto top
  end;

symbolic procedure in_sync(d1, n1, d2, n2);
  begin
    for i := 1:n1 do if d1 then <<  % skip n1 lines from d1
       d1 := cdr d1 >>;
    for i := 1:n2 do if d2 then <<  % skip n2 lines from d2
       d2 := cdr d2 >>;
% If one is ended but the other is not then we do not have a match. If
% both are ended we do have one.
    if null d1 then return null d2
    else if null d2 then return nil;
% Here I insist on 3 lines that agree before I count a match as
% having been re-established.
    if not roughly_equal(cdar d1, cdar d2) then return nil;
    d1 := cdr d1; d2 := cdr d2;
    if null d1 then return null d2
    else if null d2 then return nil;
    if not roughly_equal(cdar d1, cdar d2) then return nil;
    d1 := cdr d1; d2 := cdr d2;
    if null d1 then return null d2
    else if null d2 then return nil;
    if not roughly_equal(cdar d1, cdar d2) then return nil;
    d1 := cdr d1; d2 := cdr d2;
    if null d1 then return null d2
    else if null d2 then return nil
    else return t
  end;

fluid '(time_data time_ratio gc_time_ratio log_count);

symbolic procedure file_compare(f1, f2, name);
  begin
    scalar i, j, d1, d2, t1, gt1, t2, gt2, time_info;
    d1 := read_file f1;
    if null time_info then t1 := gt1 := 0
    else << t1 := car time_info; gt1 := cdr time_info >>;
    d2 := read_file f2;
    if null time_info then t2 := gt2 := 0
    else << t2 := car time_info; gt2 := cdr time_info >>;
    i := wrs time_data;
    j := set!-print!-precision 3;
    prin name;
    ttab 20;
    if zerop t1 then princ "---"
    else << prin t1; ttab 30; prin gt1 >>;
    ttab 40;
    if zerop t2 then princ "---"
    else << prin t2; ttab 50; prin gt2 >>;
    ttab 60;
    if zerop t1 or zerop t2 then princ "***       ***"
    else begin
       scalar r1, gr1;
       r1 := float t1 / float t2;
       gr1 := float (t1+gt1)/float (t2+gt2);
% I will only use tests where the time taken was over 200ms in my eventual
% composite summary of timings, since measurement accuracy can leave the
% really short tests pretty meaningless.
       if t1 > 200 and t2 > 200 then <<
          time_ratio := time_ratio * r1;
          gc_time_ratio := gc_time_ratio * gr1;
          log_count := log_count + 1 >>;
       prin r1;
       ttab 70;
       prin gr1 end;
    terpri();
    set!-print!-precision j;
    wrs i;
% The next segment of code is a version of "diff" to report ways in which
% reference and recent log files match or diverge.
% I can not see a neat way to get a "structured" control structure
% here easily.  Ah well, drop back to GOTO statements!
top:
    if null d1 then <<      % end of one file
       if d2 then terpri();
       i := 0;
       while d2 and i < 20 do <<
          princ "REF "; princ caar d2; princ ":"; ttab 10; printc cdar d2;
          d2 := cdr d2;
          i := i + 1 >>;
       if d2 then printc "...";
       return >>;
    if null d2 then <<      % end of other file
       i := 0;
       while d1 and i < 20 do <<
          princ "NEW "; princ caar d1; princ ":"; ttab 10; printc cdar d1;
          d1 := cdr d1;
          i := i + 1 >>;
       if d1 then printc "...";
       return >>;
% The test "roughly_equal" compares allowing some tolerance on floating
% point values. This is because REDUCE uses platform libraries for
% floating point elementary functions and printing, so small differences
% are expected. This is perhaps uncomfortable, but is part of reality, and
% the test here makes comparison output much more useful in that the
% differences shown up are better limited towards "real" ones.
    if roughly_equal(cdar d1, cdar d2) then <<
       d1 := cdr d1;
       d2 := cdr d2;
       go to top >>;
% I will first see if there are just a few blank lines inserted into
% one or other file. This special case is addressed here because it
% appears more common a possibility than I had expected.
    if cdar d1 = "" and cdr d1 and roughly_equal(cdadr d1, cdar d2) then <<
       princ "NEW "; princ caar d1; princ ":"; ttab 10; printc cdar d1;
       d1 := cdr d1;
       go to top >>
    else if cdar d1 = "" and cdr d1 and cdadr d1 = "" and cddr d1 and
       roughly_equal(cdaddr d1, cdar d2) then <<
       princ "NEW "; princ caar d1; princ ":"; ttab 10; printc cdar d1;
       d1 := cdr d1;
       princ "NEW "; princ caar d1; princ ":"; ttab 10; printc cdar d1;
       d1 := cdr d1;
       go to top >>
    else if cdar d2 = "" and cdr d2 and
       roughly_equal(cdadr d2, cdar d1) then <<
       princ "REF "; princ caar d2; princ ":"; ttab 10; printc cdar d2;
       d2 := cdr d2;
       go to top >>
    else if cdar d2 = "" and cdr d2 and cdadr d2 = "" and cddr d2 and
       roughly_equal(cdaddr d2, cdar d1) then <<
       princ "REF "; princ caar d2; princ ":"; ttab 10; printc cdar d2;
       d2 := cdr d2;
       princ "REF "; princ caar d2; princ ":"; ttab 10; printc cdar d2;
       d2 := cdr d2;
       go to top >>;
    i := 1;
seek_rematch:
    j := 0;
inner:
    if in_sync(d1, i, d2, j) then <<
       terpri();
       for k := 1:i do <<
          princ "NEW "; princ caar d1; princ ":"; ttab 10; printc cdar d1;
          d1 := cdr d1 >>;
       for k := 1:j do <<
          princ "REF "; princ caar d2; princ ":"; ttab 10; printc cdar d2;
          d2 := cdr d2 >>;
% Should be in step again here.
       if null d1 then return
       else go to top >>;
    j := j + 1;
    i := i - 1;
    if i >= 0 then go to inner;
    i := j;
% I am prepared to seek 80 lines ahead on each side before I give up.
% The number 80 is pretty much arbitrary.
    if i < 80 then goto seek_rematch;
    terpri();
    i := 0;
    while d2 and i < 20 do <<
       princ "REF "; princ caar d2; princ ":"; ttab 10; printc cdar d2;
       d2 := cdr d2;
       i := i+1 >>;
    if d2 then printc "...";
    i := 0;
    while d1 and i < 20 do <<
       princ "NEW "; princ caar d1; princ ":"; ttab 10; printc cdar d1;
       d1 := cdr d1;
       i := i+1 >>;
    if d1 then printc "...";
    printc "Comparison failed."
  end;

fluid '(which_module);

symbolic procedure check_a_package;
  begin
    scalar oll, names, p1, logname, mylogname, mylog, reflogname, reflog,
           time_data, time_ratio, gc_time_ratio, log_count;
    get_configuration_data t;
    if boundp 'which_module and symbol!-value 'which_module and
       not (symbol!-value 'which_module = "") then <<
       names := compress explodec symbol!-value 'which_module;
       if member(names, r38_test_cases) then names := list names
       else error(0, list("unknown module to check", which_module)) >>
    else names := r38_test_cases;
    in_list1("$srcdir/../../../package.red", nil);
% I write a summary of timing information into log/times.log
    time_data := open("log/times.log", 'output);
    p1 := wrs time_data;
    princ "MODULE";
    ttab 20; princ "Local";     ttab 30; princ "(GC)";
    ttab 40; princ "Reference"; ttab 50; princ "(GC)";
    ttab 60; princ "Ratio";     ttab 70; printc "inc GC";
    wrs p1;
    terpri();
    oll := linelength 100;
    printc "=== Comparison results ===";
    time_ratio := gc_time_ratio := 1.0; log_count := 0;
    for each packge in names do <<
       terpri();
       princ "CHECKING: "; print packge;
       if boundp '!@log and stringp symbol!-value '!@log then
           logname := symbol!-value '!@log
       else logname := "log";
       logname := concat("$srcdir/../../../", concat(logname, "/"));
       mylogname := concat(logname, concat(packge, ".rlg"));
       if get(packge,'folder) then p1 := get(packge,'folder)
       else p1 := packge;
       reflogname := concat("$srcdir/../../../packages/",
                       concat(p1,
                         concat("/",
                           concat(packge,".rlg"))));
       mylog := errorset(list('open, mkquote mylogname, ''input), nil, nil);
       reflog := errorset(list('open, mkquote reflogname, ''input), nil, nil);
       if errorp mylog then <<
          if not errorp reflog then close car reflog;
          princ "No current log in "; print mylogname >>
       else if errorp reflog then <<
          close car mylog;
          princ "No reference log in "; print reflogname >>
       else <<
          princ "LOGS: "; princ mylogname; princ " "; printc reflogname;
          mylog := car mylog; reflog := car reflog;
          file_compare(mylog, reflog, packge);
          close mylog;
          close reflog >> >>;
     time_data := wrs time_data;
     if not zerop log_count then <<
        time_ratio := expt(time_ratio, 1.0/log_count);
        gc_time_ratio := expt(gc_time_ratio, 1.0/log_count);
        terpri();
        p1 := set!-print!-precision 3;
        princ "Over "; prin log_count; princ " tests the speed ratio was ";
        print time_ratio;
        princ "    (or ";
        prin gc_time_ratio;
        printc " is garbage collection costs are included)";
        set!-print!-precision p1 >>;
     close wrs time_data;
     linelength oll;
  end;


symbolic procedure personal_check_a_package;
  begin
    scalar oll, names, p1, logname, mylogname, mylog, reflogname, reflog,
           time_data, time_ratio, gc_time_ratio, log_count;
    get_configuration_data nil;
    if boundp 'which_module and symbol!-value 'which_module and
       not (symbol!-value 'which_module = "") then <<
       names := compress explodec symbol!-value 'which_module;
       if member(names, r38_test_cases) then names := list names
       else error(0, list("unknown module to check", which_module)) >>
    else names := r38_test_cases;
% I write a summary of timing information into log/times.log
    time_data := open("log/times.log", 'output);
    p1 := wrs time_data;
    princ "MODULE";
    ttab 20; princ "Local";     ttab 30; princ "(GC)";
    ttab 40; princ "Reference"; ttab 50; princ "(GC)";
    ttab 60; princ "Ratio";     ttab 70; printc "inc GC";
    wrs p1;
    terpri();
    oll := linelength 100;
    printc "=== Comparison results ===";
    time_ratio := gc_time_ratio := 1.0; log_count := 0;
    for each packge in names do <<
       terpri();
       princ "CHECKING: "; print packge;
       mylogname := concat("log/", concat(packge, ".rlg"));
       reflogname := concat("tests/", concat(packge,".rlg"));
       mylog := errorset(list('open, mkquote mylogname, ''input), nil, nil);
       reflog := errorset(list('open, mkquote reflogname, ''input), nil, nil);
       if errorp mylog then <<
          if not errorp reflog then close car reflog;
          princ "No current log in "; print mylogname >>
       else if errorp reflog then <<
          close car mylog;
          princ "No reference log in "; print reflogname >>
       else <<
          princ "LOGS: "; princ mylogname; princ " "; printc reflogname;
          mylog := car mylog; reflog := car reflog;
          file_compare(mylog, reflog, packge);
          close mylog;
          close reflog >> >>;
     time_data := wrs time_data;
     if not zerop log_count then <<
        time_ratio := expt(time_ratio, 1.0/log_count);
        gc_time_ratio := expt(gc_time_ratio, 1.0/log_count);
        terpri();
        p1 := set!-print!-precision 3;
        princ "Over "; prin log_count; princ " tests the speed ratio was ";
        print time_ratio;
        princ "    (or ";
        prin gc_time_ratio;
        printc " is garbage collection costs are included)";
        set!-print!-precision p1 >>;
     close wrs time_data;
     linelength oll;
  end;


faslend;

% faslout 'cslhelp;
% 
% module cslhelp;
% 
% global '(!*force);
% 
% flag('(force),'switch);
% flag('(on),'eval);
% 
% on force;
% 
% symbolic procedure formhelp(u,vars,mode);
%    list('help, 'list . for each x in cdr u collect mkquote x);
% 
% if member('help, lispsystem!*) then <<
%    put('help, 'stat, 'rlis);
%    flag('(help), 'go);
%    put('help, 'formfn, 'formhelp) >>;
% 
% off force;
% remflag('(on),'eval);
% 
% endmodule;
% 
% faslend;

load!-module 'remake;

<< initreduce();
   date!* := "Bootstrap version";
   !@reduce := symbol!-value gensym();
   checkpoint('begin, "REDUCE 3.8") >>;

!#if (not !*savedef)
load!-module 'user;
!#endif

!@reduce := concat(!@srcdir, "/../../..");
in "$reduce/package.red"$

package!-remake2(prolog_file,'support);

package!-remake2(rend_file,'support);

package!-remake2('entry,'support);

package!-remake2('remake,'support);


% The next lines have LOTS of hidden depth!  They restart CSL repeatedly
% so that each of the modules that has to be processed gets dealt with in
% a fresh uncluttered environment. The list of modules is fetched from
% a configuration file which must have 3 s-expressions in it. The first
% is a list of basic modules that must be built to get a core version of
% REDUCE. The second list identifies modules that can be built one the core
% is ready for use, while the last list indicates which modules have
% associated test scripts.
%
% when the modules have been rebuild the system does a restart that
% kicks it back into REDUCE by calling begin(). This then continues
% reading from the stream that had been the standard input when this
% job started. Thus this script MUST be invoked as
%           ./csl -oslowr38.img -z build38.lsp -l log/build38.log
% with the file build38.lsp specified on the command line in the call. It
% will not work if you start csl manually and then do a (rdf ..) [say]
% on build38.lsp.  I told you that it was a little delicate.

!#if !*savedef
% Some switches may be in the utter core and not introduced via the
% "switch" declaration...
for each y in oblist() do
  if flagp(y, 'switch) then <<
     princ "+++ Declaring a switch: ";
     print y >>; 
!#endif

get_configuration_data t;

build_reduce_modules r38_base_modules;

% Now I want to do a cold-start so that I can create a sensible
% image for use in the subsequent build steps. This image should not
% contain ANYTHING extraneous.

symbolic restart!-csl nil;

(setq !*savedef (lessp (cdr (assoc 'c!-code lispsystem!*)) 20))
(make!-special '!*native_code)
(setq !*native_code nil)

(setq !*backtrace t)

(cond ((null !*savedef) (load!-module 'user)))

(load!-module 'cslcompat)

(setq !*comp nil)

(load!-module 'module)            % Definition of load_package, etc.

(load!-module 'cslprolo)          % CSL specific code.

(setq loaded!-packages!* '(cslcompat user cslprolo))

% NB I will re-load the "patches" module when REDUCE is started
% if there is a version newer than the one I load up here. Note that
% if there had not been a "patches.red" file I will not have a module to load
% here.

(cond
   ((modulep 'patches) (load!-module 'patches)))

(load!-package 'rlisp)

(load!-package 'cslrend)

(load!-package 'poly)

(load!-package 'arith)

(load!-package 'alg)

(load!-package 'mathpr)

(cond 
   ((modulep 'tmprint) (load!-package 'tmprint)))

(load!-package 'entry)

% (write!-help!-module "$srcdir/../util/r38.inf" nil)
%
% (load!-module 'cslhelp)

(setq version!* "REDUCE 3.8")

(setq !*backtrace nil)

(initreduce)

(setq date!* "15-Apr-04")

(setq no_init_file nil)

(setq !@csl (setq !@reduce (symbol!-value (gensym))))

% If the user compiles a new FASL module then I will let it
% generate native code by default. I build the bulk of REDUCE
% without that since I have statically-selected hot-spot compilation
% that gives me what I believe to be a better speed/space tradeoff.

(fluid '(!*native_code))
(setq !*native_code t)

(checkpoint 'begin (bldmsg "%w, %w ..." version!* date!*))

(setq no_init_file t)

(begin)

%
% See the fairly length comments given a bit earlier about the
% delicacy of the next few lines!
%

symbolic;

load!-module 'remake;

get_configuration_data t;

build_reduce_modules r38_extra_modules;

% At this stage I have a complete workable REDUCE. If built using a
% basic CSL (I call it "slowr38" here)  nothing has been compiled into C
% (everything is bytecoded), and it is big because it has retained all
% Lisp source code in the image file. If however I built using a version
% of CSL ("r38") that did have things compiled into C then these will
% be exploited and the original Lisp source will be omitted from the
% image, leaving a production version.

bye;


