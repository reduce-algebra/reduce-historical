%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Program-Command-Interpreter.SL - Perform Program Command
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        10 August 1982
% Revised:     8 December 1982
%
% 8-Dec-82 Alan Snyder
%   Changed use of DSKIN (now an EXPR).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This file redefines the start-up routine for PSL (Lisp Reader) to first read
% and interpret the program command string.  If the command string contains a
% recognized command name, then the corresponding function is immediately
% executed and the program QUITs.  Otherwise, the normal top-level function
% definition is restored and invoked as normal.  Commands are defined using the
% property PROGRAM-COMMAND (see below).  This file defines only one command,
% COMPILE, which is used to compile Lisp files (not RLisp files).

(BothTimes (load common))
(load parse-command-string get-command-string compiler)

(fluid '(*usermode *redefmsg CurrentReadMacroIndicator* CurrentScanTable*))

(cond ((funboundp 'original-main)
       (copyd 'original-main 'main)))

(de main ()
  (let ((CurrentReadMacroIndicator* 'LispReadMacro) % Crock!
	(CurrentScanTable* LispScanTable*)
	(c-list (parse-command-string (get-command-string)))
	(*usermode nil)
	(*redefmsg nil))
       (perform-program-command c-list)
       (copyd 'main 'original-main)
       )
  (original-main)
  )

(de perform-program-command (c-list)
  (if (not (Null c-list))
      (let ((command (car c-list)))
	   (if (StringP command)
	       (let* ((command-id (intern (string-upcase command)))
		      (func (get command-id 'PROGRAM-COMMAND)))
		     (if func (apply func (list c-list))))))))

(put 'COMPILE 'PROGRAM-COMMAND 'compile-program-command)

(fluid '(*quiet_faslout *WritingFASLFile))

(de compile-program-command (c-list)
  (setq c-list (cdr c-list))
  (for (in file-name-root c-list)
       (do (let* ((form (list 'COMPILE-FILE file-name-root))
		  (*break NIL)
		  (result (ErrorSet form T NIL))
		  )
	     (if (FixP result)
	         (progn
		   (if *WritingFASLFile (faslend))
	           (printf "%n ***** Error during compilation of %w.%n"
		           file-name-root)
	           ))
	     )))
  (quit))

(de compile-file (file-name-root)
  (let ((source-fn (string-concat file-name-root ".SL"))
	(binary-fn (string-concat file-name-root ".B"))
	(*quiet_faslout T)
	)
       (if (not (FileP source-fn))
	   (printf "Unable to open source file: %w%n" source-fn)
	   % else
	   (printf "%n----- Compiling %w%n" source-fn binary-fn)
	   (faslout file-name-root)
	   (dskin source-fn)
	   (faslend)
	   (printf "%nDone compiling %w%n%n" source-fn)
	   )))
