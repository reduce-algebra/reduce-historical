%
% ADDR2ID.RED - Attempt to find out what function an address is in
% 
% Author:      Eric Benson
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        18 May 1982
% Copyright (c) 1982 University of Utah
%

(compiletime (load syslisp useful))

(compiletime (fluid '(code-address* closest-address* closest-symbol*)))

(de code-address-to-symbol (code-address*)
  (let ((closest-symbol* ()) (closest-address* 0))
       (mapobl #'(lambda (symbol)
		         (when (fcodep symbol)
			       (let ((address (inf (getfcodepointer symbol))))
				    (when (and (ileq address
						     code-address*)
					       (igreaterp address
							  closest-address*))
					  (setq closest-address*
						address)
					  (setq closest-symbol* symbol))))))
       closest-symbol*))
