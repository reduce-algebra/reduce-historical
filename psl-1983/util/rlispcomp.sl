%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% RLISPCOMP.SL
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        27 September 1982
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This program reads and interprets
% the program command string as a list of source files to be compiled.

(CompileTime (load common pathnames))
(load pathnamex parse-command-string get-command-string compiler)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(*usermode *redefmsg CurrentReadMacroIndicator* CurrentScanTable*))
(fluid '(*quiet_faslout *WritingFASLFile))

(de rlispcomp ()
  (let ((c-list (parse-command-string (get-command-string)))
	(*usermode nil)
	(*redefmsg nil))
       (compile-files c-list)
       )
  )

(de compile-files (c-list)
  (cond ((null c-list)
	 (PrintF "RLisp Compiler%n")
	 (PrintF "Usage: RLISPCOMP source-file ...%n")
	 )
	(t
	 (for (in fn c-list)
	      (do (attempt-to-compile-file fn))
	      )
         (quit)
	 )))

(de attempt-to-compile-file (fn)
  (let* ((form (list 'COMPILE-FILE fn))
	 (*break NIL)
	 (result (ErrorSet form T NIL))
	 )
    (cond ((FixP result)
	   (if *WritingFASLFile (faslend))
	   (printf "%n ***** Error during compilation of %w.%n" fn)
	   ))
    ))

(de compile-file (fn)
  (let ((source-fn (namestring (pathname-set-default-type fn "RED")))
	(binary-fn (namestring (pathname-set-type fn "B")))
	(*quiet_faslout T)
	)
       (if (not (FileP source-fn))
	   (printf "Unable to open source file: %w%n" source-fn)
	   % else
	   (printf "%n----- Compiling %w%n" source-fn binary-fn)
	   (faslout (namestring (pathname-without-type binary-fn)))
	   (eval (list 'in source-fn)) % Damn FEXPRs
	   (faslend)
	   (printf "%nDone compiling %w%n%n" source-fn)
	   )))
