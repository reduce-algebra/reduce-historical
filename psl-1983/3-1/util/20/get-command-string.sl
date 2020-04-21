%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Get-Command-String.SL (TOPS-20 Version) - Get Program Command String
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        4 August 1982
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load common jsys))
(load strings)

% The function GET-COMMAND-STRING returns the string argument given
% to the program when it was invoked.

(de char-blank? (ch)
  (or (= ch (char space)) (= ch (char tab))))

(fluid '(command-string*))

(de get-command-string ()
  (or command-string* (setq command-string* (dec20-get-command-string))))

(de dec20-get-command-string ()

  % Read the process command string.  This function should only be invoked once
  % in a given fork, and should be invoked as soon as possible.  The process
  % command string is massaged to remove the program name and any trailing
  % CRLF.

  (prog (s high i j)
	(setq s (dec20-read-process-arg))
	(setq high (size s))
	(if (< high 0) (return ""))
	(setq i 0)
	(while (and (<= i high) (char-blank? (igets s i)))
	       (setq i (+ i 1)))
	(setq j i)
	(while (and (<= j high) (not (char-blank? (igets s j))))
	       (setq j (+ j 1)))
	(if (string-equal (substring s i j) "run") (return ""))
	(while (and (<= j high) (char-blank? (igets s j)))
	       (setq j (+ j 1)))
	(while (and (> high j) (not (graphicp (igets s high))))
	       (setq high (- high 1)))
	(return (substring s j (+ high 1)))
	))

(CompileTime (put 'prarg 'OpenCode '((jsys 357) (move (reg 1) (reg 3)))))
(CompileTime (put 'rscan 'OpenCode '((jsys 320) (move (reg 1) (reg 1)))))
(CompileTime (put 'sin 'OpenCode '((jsys 42) (move (reg 1) (reg 3)))))

(de dec20-read-process-arg ()

  % On TOPS-20, the command argument can be passed to an inferior fork in two
  % ways.  The first (and better) way is to pass a string in the process
  % argument block.  The second (and more popular) way is to pass a string in
  % the RESCAN buffer (what a crock!).  We will use the process argument block,
  % if it is nonempty, otherwise we will read from the RESCAN buffer.

  (prog (arg-len str)
    (setq arg-len (prarg #.(int2sys (xword 1 8#400000)) 4 0))
    (cond ((> arg-len 0)
	   (setq str (MkString arg-len))
	   (prarg #.(int2sys (xword 1 8#400000)) (jconv str) arg-len)
	   (return (recopystringtonull str))
	   ))
    (setq arg-len (rscan 0))
    (if (= arg-len 0) (return "")) % no input string
    (setq str (MkString arg-len))
    (sin 8#777777 (jconv str) (- arg-len))
    (return str)
    ))
