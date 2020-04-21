%
% HOMEDIR.SL - USER-HOMEDIR-STRING function for Tops-20
% 
% Author:      Eric Benson
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        21 September 1982
% Copyright (c) 1982 University of Utah
%

(compiletime (progn
 (load monsym syslisp)
 (put 'get-user-number 'opencode '((gjinf)))
 (flag '(user-homedir-string-aux get-dir-string)
       'internalfunction)))

% Returns a string which is the init file for program-name.
% Optional HOST is not supported.
(de init-file-string (program-name)
  (concat (user-homedir-string) (concat program-name ".INIT")))

% Returns a string which is the users home directory name.
% Optional HOST is not supported.
(lap '((*entry user-homedir-string expr 0)
       (movei (reg 1) (indexed (reg st) 1))	% Pointer into the stack
       (*alloc 20)				% allocate space
       (*call user-homedir-string-aux)	% call the real function
       (*exit 20)))				% deallocate and return

(de user-homedir-string-aux (p)
  (concat "PS:<" (mkstr (get-dir-string p (get-user-number)))))

(lap '((*entry get-dir-string expr 2)
       (*move (reg 1) (reg 5))			% save original addr in ac5
       (hrli (reg 1) 8#10700)			% make a byte pointer
       (*move (reg 1) (reg 3))			% save it in ac3
       (dirst)
         (erjmp cant-get-dir)
       (movei (reg 4) 62)			% put a closing > on it
       (idpb (reg 4) (reg 1))
       (setz (reg 4) 0)				% put a null char on the end
       (idpb (reg 4) (reg 1))
       (seto (reg 4) 0)				% initialize length to -1
string-length-loop
       (ildb (reg 2) (reg 3))
       (jumpe (reg 2) done-computing-length)
       (aoja (reg 4) string-length-loop)
done-computing-length
       (movem (reg 4) (indexed (reg 5) 0))	% put len in string header
       (*move (reg 5) (reg 1))			% return original pointer
       (*exit 0)
cant-get-dir
       (*move (reg 1) '"UNKNOWN>")
       (*exit 0)))
