%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PSLCOMP-MAIN.SL
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        27 September 1982
% Revised:     8 December 1982
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This file redefines the start-up routine for PSLCOMP to read and interpret
% the program command string as a list of source files to be compiled.

% Edit by Cris Perdue,  8 Apr 1983 1401-PST
% Compile-files now does exitlisp rather than quit.
%  EvIn is only given a definition if not already defined.
%  Syntax is assumed to be LISP if given a crazy file extension.
% Edit by Cris Perdue,  5 Apr 1983 1421-PST
% Changed to use get-command-args rather than get-command-string
%  and parse-command-string.
%  Uses EVIN to read the file, thus compiles any type of file.
%  If no extension specified, tries "sl", "build", and "red" extensions.
%  Defines EVIN to load RLISP if needed.  This also gets around the
%  problem of starting up in the RLISP top level with RLISP
%  loaded.
%  Now uses ErrSet rather than ErrorSet.
% 8-Dec-82 Alan Snyder
%   Changed use of DSKIN (now an EXPR).

(CompileTime (load common pathnames))
(imports '(pathnamex get-command-args compiler))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(*usermode *redefmsg CurrentReadMacroIndicator* CurrentScanTable*))
(fluid '(*quiet_faslout *WritingFASLFile))

(cond ((funboundp 'original-main)
       (copyd 'original-main 'main)))

(de main ()
  (let ((CurrentReadMacroIndicator* 'LispReadMacro) % Crock!
	(CurrentScanTable* LispScanTable*)
	(c-list (get-command-args))
	(*usermode nil)
	(*redefmsg nil))
       (compile-files c-list)
       (copyd 'main 'original-main)
       )
  (original-main)
  )

(de pslcomp ()	% Not in use. /csp
  (let ((*usermode nil)
	(*redefmsg nil))
    (compile-files (get-command-args))))

(if (funboundp 'evin)
  (de evin (x)
    (load rlisp)
    (eval (list 'in x))))	% Hack. /csp

(de compile-files (c-list)
  (cond ((null c-list)
	 (PrintF "Portable Standard Lisp Compiler%n")
	 (PrintF "Usage: PSLCOMP source-file ...%n")
	 )
	(t
	 (for (in fn c-list)
	      (do (attempt-to-compile-file fn))
	      )
         (exitlisp)
	 )))

(de attempt-to-compile-file (fn)
  (let* ((*break NIL)
	 (result (ErrSet (compile-file fn) T))
	 )
    (cond ((FixP result)
	   (if *WritingFASLFile (faslend))
	   (printf "%n ***** Error during compilation of %w.%n" fn)
	   ))
    ))

(de compile-file (fn)
  (let* ((pathname (pathname fn))
	 (source-names
	  (cond ((pathname-type pathname)
		 (list (namestring pathname)))
		(t (for (in ext '("build" "sl" "red"))
			(collect
			 (namestring (pathname-set-default-type 
				      pathname
				      ext)))))))
	 (binary-fn (namestring (pathname-set-type fn "b")))
	 (*quiet_faslout T)
	 (type NIL)
	 )
    (for (in source-fn source-names)
	 (do
	  (cond
	   ((FileP source-fn)
	    (printf "%n----- Compiling %w%n" source-fn)
	    (faslout (namestring (pathname-without-type binary-fn)))
	    (setq type (pathname-type (pathname source-fn)))
	    (funcall (cond ((string-equal type "sl") 'dskin)
			   ((string-equal type "build") 'evin)
			   ((string-equal type "red") 'evin)
			   (t 'dskin))
		     source-fn)
	    (faslend)
	    (printf "%nDone compiling %w%n%n" source-fn)
	    (return t)
	    )))
	 (finally
	    (printf "Unable to find source file for: %w%n" fn)))))
