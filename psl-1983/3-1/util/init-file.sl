%
% INIT-FILE.SL - Function which reads an init file
% 
% Author:      Eric Benson
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        21 September 1982
% Copyright (c) 1982 University of Utah
%

(if_system Tops20 (imports '(homedir)))

(de read-init-file (program-name)
  ((lambda (f)
     (cond ((filep f) (lapin f))))
   (init-file-string program-name)))
